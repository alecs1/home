#include <stdio.h>

#define __STDC_FORMAT_MACROS
#include <inttypes.h>

#include <string.h>
#include <stdlib.h>

#include "test_api.h"
#include "structures.h"
#include "basic_ops.h"

#define CHILDREN_ADDRESS_BUFFER 4096


//returns how many it has written
uint32_t get_child_addresses(S_metadata* parent, uint64_t* buffer, const uint32_t capacity, const uint32_t index) {
    if (parent->type != TYPE_DIR) {
        printf("%s - error, not a directory\n", __func__);
        return 0;
    }
    S_dir_metadata* dir_md = (S_dir_metadata*)parent->specific;
    
        
    if (index >= dir_md->child_count) {
        printf("%s - warning not children from index %" PRIu32 "\n", __func__, index);
        return 0;
    }

    //two cases
    //a)easy and optimal-few children, all of their addresses stored in memory
    //b)harder - need to actually read from disk

    uint32_t write_count = 0;
    if (dir_md->in_mem_addresses_count == dir_md->child_count) {

        for (uint32_t i = index; i < index + capacity && i < dir_md->child_count; i++) {
            buffer[i - index] = dir_md->in_mem_addresses[index];
            write_count += 1;
        }
    }
    else {
        //read at most buffer_size addresses from disk
        if (dir_md->child_list_address == 0) {
            //read from the rest of the structure
            uint64_t address = parent->address + CHILD_LIST_OFFSET + (index * ADDRESS_SIZE);
            uint64_t read_size = ADDRESS_SIZE * (dir_md->child_count - index);
            if (capacity*ADDRESS_SIZE < read_size)
                read_size = capacity*ADDRESS_SIZE;
            uint64_t read_bytes = d_read(parent->part_id, address, buffer, read_size);
            write_count = read_bytes / ADDRESS_SIZE;
        }
        else {
            //read from the address
            uint64_t address = dir_md->child_list_address + (index * ADDRESS_SIZE);
            uint64_t read_size = ADDRESS_SIZE * (dir_md->child_count - index);
            if (capacity*ADDRESS_SIZE < read_size)
                read_size = capacity*ADDRESS_SIZE;
            uint64_t read_bytes = d_read(parent->part_id, address, buffer, read_size);
            write_count = read_bytes / ADDRESS_SIZE;
        }
    }

    return write_count;
}

//internal to our API
//return the address of the metadata found
uint64_t find_child(S_metadata* parent, char* name, uint8_t type) {
    uint64_t ret_val = 0;
    
    if (parent->type != TYPE_DIR) {
        printf("%s - error, not a directory: %s\n", __func__, parent->name);
        return 0;
    }

    if (parent->specific == NULL) {
        printf("%s - function call needs struct parent completely constructed\n", __func__);
        return 0;
    }
        
    S_dir_metadata* dir_md = (S_dir_metadata*)parent->specific;
    


    //first search in the metadata area allocated for the files names that start with the name[0]
    //the metadata batch is already in memory somewhere, now we need it
    S_metadata_batch* parent_mdb = get_metadata_batch(parent->part_id, parent->batch_address);
    

    uint64_t expected_min_index = parent_mdb->index_table[(uint8_t)name[0]];
    uint64_t expected_max_index = 0;
    if (name[0] < ALPHABET_LAST_BYTE)
        expected_max_index = parent_mdb->index_table[(uint8_t)(name[0]+1)];
    else
        expected_max_index =
            parent_mdb->address + METADATA_BATCH_HEADER_SIZE +
            parent_mdb->file_capacity * METADATA_SIZE - ADDRESS_SIZE;

    //we should generally be in the happy case where the child is inside the metadata at the expected address; only if it's not, start an exhaustive search (also assume these are sorted)
    uint64_t children[CHILDREN_ADDRESS_BUFFER];
    uint32_t index = 0;

    do {
        uint32_t count = get_child_addresses(parent, children, CHILDREN_ADDRESS_BUFFER, index);
        printf("%s - parent=%s, count=%" PRIu32 "\n", __func__, parent->name, count);
        index += count;
        for(uint32_t i = 0; i < count; i++) {
            uint64_t crt_address = children[i];
            if ( (crt_address >= expected_min_index) && (crt_address <= expected_max_index) ) {
                //read metadata of this child from disk
                S_metadata *child = read_metadata(parent->part_id, crt_address, READ_BASIC_MD);
                
                printf("%s - child %u, addr=%" PRIu64 ", name=%s\n",
                       __func__, i, crt_address, child->name);
                if (strncmp(name, (char*)child->name, NAME_SIZE) == 0) {
                    //found the match
                    ret_val = child->address;
                    free_metadata_struct(child);
                    break;
                }
                else
                    free_metadata_struct(child);
            }
        }
    }
    while ( (ret_val == 0) && (index < dir_md->child_count) );

    if (ret_val == 0) {
        //unhappy case, we have to search everywhere
    }

    if (ret_val == 0) {
        printf("%s - error, could not find child %s, type%" PRIu8 " in ", __func__, name, type);
    }

    return ret_val;
}

S_metadata* create(uint16_t id, char* path, uint8_t type) {
    printf("%s - id=%" PRIu16 ", path=%s, type=%" PRIu8 "\n", __func__, id, path, type);
    //TODO - how about some sanitisation on path?
    
    S_metadata *md = NULL;

    
    //split the path and search for the parent
    char *aux_path = (char*)malloc((strlen(path)+1) * sizeof(char));
    char *saveptr, *next_name, *crt_name;
    strcpy(aux_path, path);

    S_metadata* parent_dir = fs_defs[id].root_metadata;

    printf("%s - parent_dir:", __func__); print_metadata(parent_dir);

    crt_name = NULL;
    next_name = strtok_r(aux_path, "/", &saveptr);

    //printf("%s - crt_name=%s, next_name=%s\n", __func__, crt_name, next_name);

    while (next_name != NULL) {
        crt_name = next_name;
        next_name = strtok_r(NULL, "/", &saveptr);
        //printf("%s - crt_name=%s, next_name=%s\n", __func__, crt_name, next_name);
        if (next_name != NULL) {
            //next_name is the next child
            //crt_dir is the directory we're going to find now in parent_dir
            uint64_t crt_dir_addr = find_child(parent_dir, crt_name, TYPE_DIR);
            //printf("%s - crt_name=%s, crt_dir_addr=%" PRIu64 "\n", __func__, crt_name, crt_dir_addr);
            if (crt_dir_addr != 0) {
                if (parent_dir != fs_defs[id].root_metadata)
                    free_metadata_struct(parent_dir);
                parent_dir = read_metadata(id, crt_dir_addr, READ_FULL_MD);
            }
            else {
                printf("%s - error, could not correctly read dir %s\n", __func__, crt_name);
            }
        }
        else
            break;
    }

    printf("%s - parent_dir for %s: ", __func__, path); print_metadata(parent_dir);


    if (parent_dir != NULL) {
        next_name = crt_name;
        md = create_child(parent_dir, next_name, type);
    }
    else {
        printf("%s - error, parent_dir = NULL\n", __func__);
    }


    
    
    //tok = NULL, prev_tok = our name, parent_dir

    free(aux_path);
    return md;
}

//update the address of one child
int update_child_address(S_metadata* parent, uint64_t addr, uint64_t new_addr) {
    printf("%s - parent=%s, parent addr=%" PRIu64 ", old child addr=%" PRIu64 ", new addr=%" PRIu64 "\n",
           __func__, parent->name, parent->address, addr, new_addr);

    int ret_val = -1;
    S_dir_metadata* dir_md = (S_dir_metadata*)parent->specific;

    if (dir_md->in_mem_addresses_count == dir_md->child_count) {
        for(uint32_t i = 0; i < dir_md->in_mem_addresses_count; i++) {
            printf("%s - child %u, addr=%" PRIu64 "\n", __func__, i, new_addr);

            if (dir_md->in_mem_addresses[i] == addr) {
                dir_md->in_mem_addresses[i] = new_addr;
                ret_val = 0;
                break;
            }
        }
    }
    else {
        printf("%s - updating for addresses not already loaded not implemented\n", __func__);
        ret_val = -2;
    }

    if (ret_val == 0) {
        write_metadata(parent);
    }
    else {
        printf("%s - child addr %" PRIu64 " not found for parent %s addr %" PRIu64 "\n",
               __func__, addr, parent->name, parent->address);
    }

    return ret_val;
}

//update the parent address of all children
int update_parent_address(S_metadata* parent, uint64_t new_addr) {
    int ret_val = -1;
    S_dir_metadata* dir_md = (S_dir_metadata*)parent->specific;

    if (dir_md->in_mem_addresses_count == dir_md->child_count) {
        for(uint32_t i = 0; i < dir_md-> in_mem_addresses_count; i++) {
            S_metadata* child_md = read_metadata(parent->part_id,
                                             dir_md->in_mem_addresses[i], READ_FULL_MD);
            child_md->parent_address = new_addr;
            write_metadata(child_md);
            free_metadata_struct(child_md);
        }
        ret_val = 0;
    }
    else {
        printf("%s - updating for addresses not already loaded not implemented\n", __func__);
        ret_val = -2;
    }

    return ret_val;
}

//migrate the metadata of all file with names starting with first_char
//new_mdb may be null, in case we don't move to another mdb
//TODO - this is a complicated operation which whould be hard to journal
int migrate_md_index(uint16_t part_id, S_metadata_batch* old_mdb, S_metadata_batch* new_mdb,
                     uint8_t first_char, uint64_t new_addr, uint64_t new_size) {

    printf("%s - part_id=%" PRIu16 ", old_mdb=%" PRIu64 ", new_mdb=%p, first_char=%c, new_addr=%"
           PRIu64 ", new_size=%" PRIu64 "\n",
           __func__, part_id, old_mdb->address, new_mdb, first_char, new_addr, new_size);


    if (new_mdb != NULL) {
        printf("%s - migrating to new mdb no implemented\n", __func__);
        return -1;
    }
    uint64_t read_addr = old_mdb->index_table[first_char];
    uint64_t write_addr = new_addr;
    for(uint64_t i = 0; i < old_mdb->file_count_for_index[first_char]; i++) {
        S_metadata* crt_md = read_metadata(part_id, read_addr, READ_FULL_MD);
        crt_md->address = write_addr;
        S_metadata* parent_md = read_metadata(part_id, crt_md->parent_address, READ_FULL_MD);
        //find the current child and update it
        update_child_address(parent_md, read_addr, write_addr);
        free_metadata_struct(parent_md);

        if (crt_md->type == TYPE_DIR) {
            update_parent_address(crt_md, write_addr);
        }

        uint32_t  fill_bytes = (uint32_t)MIGRATED_METADATA_FILLER;
        d_write_repeat(part_id, read_addr, &fill_bytes, sizeof(uint32_t),
                     METADATA_SIZE/sizeof(uint32_t));

        write_metadata(crt_md);
        read_addr += METADATA_SIZE;
        write_addr += METADATA_SIZE;
    }

    old_mdb->index_table[first_char] = new_addr;
    old_mdb->file_capacity_for_index[first_char] = new_size/METADATA_SIZE;
    write_metadata_batch(old_mdb);


    uint64_t root_addr = fs_defs[part_id].root_metadata->address;
    free_metadata_struct(fs_defs[part_id].root_metadata);
    fs_defs[part_id].root_metadata = read_metadata(part_id, root_addr, READ_FULL_MD);

    return 0;
}

S_metadata* create_child(S_metadata* parent_md, char* name, uint8_t type) {

    //printf("%s - parent_md: ", __func__); print_metadata(parent_md);
    //printf("%s - name=%s\n", __func__, name);

    if (check_metadata_struct(parent_md) != 0) {
        printf("%s - invalid parent_md\n", __func__);
        return NULL;
    }

    int error = 0;
    S_metadata* md = NULL;

    S_metadata_batch *mdb = get_metadata_batch(parent_md->part_id, parent_md->batch_address);

    uint8_t first_char = (uint8_t)name[0];
    uint64_t md_address = 0;

    if (mdb->file_capacity_for_index[first_char] - mdb->file_count_for_index[first_char] > 0) {
        md_address = mdb->index_table[first_char] + 
            mdb->file_count_for_index[first_char] * METADATA_SIZE;

        //TODO - some checks
    }
    else {
        //check if we have space inside this md batch
        //check at the end, then check if there is any index which has too much space compared to its necesities
        uint64_t GROWTH_FACTOR = 2;
        uint64_t count_needed = (mdb->file_capacity_for_index[first_char] + 1) * GROWTH_FACTOR;

        int last_index = ALPHABET_FIRST_BYTE;
        for(int i = ALPHABET_FIRST_BYTE; i < ALPHABET_LAST_BYTE; i++)
            if ( (mdb->index_table[i] != ADDRESS_FFFF) && 
                 (mdb->index_table[i] > mdb->index_table[last_index]) )
                last_index = i;
        
        uint64_t md_end = mdb->address + mdb->size;

        printf("%s - need to make space, md_end=%" PRIu64 ", start of unallocated space=%" PRIu64
               ", free space=%" PRIu64 ", count_needed=%" PRIu64 ", space needed=%" PRIu64 "\n",
               __func__, md_end,
               mdb->index_table[last_index] + mdb->file_capacity_for_index[last_index] * METADATA_SIZE,
               md_end -
               mdb->index_table[last_index] + mdb->file_capacity_for_index[last_index] * METADATA_SIZE,
               count_needed, count_needed * METADATA_SIZE);

        if (md_end - 
            (mdb->index_table[last_index] + mdb->file_capacity_for_index[last_index] * METADATA_SIZE) >=
            count_needed * METADATA_SIZE ) {
            //migrate metadata index to address
            //also modify all children and parents!

            uint64_t new_addr = mdb->index_table[last_index] +
                mdb->file_capacity_for_index[last_index] * METADATA_SIZE;

            migrate_md_index(parent_md->part_id, mdb, NULL, first_char, new_addr, count_needed*METADATA_SIZE);

            md_address = new_addr + mdb->file_count_for_index[first_char] * METADATA_SIZE;
        }
        else {
            printf("%s - need to migrate to another mdb, not implemented\n", __func__);
            return NULL;
        }
    }

    if (md_address != 0) {
        if (md_address % METADATA_SIZE != 0) {
            printf("%s - error, computed address %" PRIu64 " not multiple of %u\n",
                   __func__, md_address, METADATA_SIZE);
            error = 1;
            goto cleanup;
        }
        
        md = init_metadata_struct(type);
        md->part_id = parent_md->part_id;
        md->address = md_address;
        memcpy(md->name, name, NAME_SIZE);
        md->parent_address = parent_md->address;
        md->batch_address = parent_md->batch_address;
        mdb->file_count_for_index[first_char] += 1;
    }

        
        
cleanup:
    if (error == 0) {
        write_metadata(md);
        //the parent also needs to be updated!!!
        if (parent_md->specific == NULL) {
            printf("%s - error, please use fully populated S_dir_metadata structures, parent_md=%s, name=%s\n",
                   __func__, parent_md->name, name);
        }
        S_dir_metadata* parent_dir_md = (S_dir_metadata*) parent_md->specific;

        parent_dir_md->child_count+=1;
        parent_dir_md->in_mem_addresses_count += 1;

        if (parent_dir_md->in_mem_addresses == NULL)
            parent_dir_md->in_mem_addresses =
                malloc(parent_dir_md->in_mem_addresses_count*sizeof(uint64_t));
        else 
            parent_dir_md->in_mem_addresses =
                (uint64_t*)realloc(parent_dir_md->in_mem_addresses,
                               (parent_dir_md->in_mem_addresses_count)*sizeof(uint64_t));
        parent_dir_md->in_mem_addresses[parent_dir_md->in_mem_addresses_count-1] = md->address;
        write_metadata(parent_md);

        printf("%s - parent_dir %s at %" PRIu64 ", in_mem_addresses_count=%" PRIu32 ", in_mem_addresses=%p, newly created child addr=%" PRIu64 "\n",
               __func__, parent_md->name, parent_md->address,
               parent_dir_md->in_mem_addresses_count,
               parent_dir_md->in_mem_addresses,
               parent_dir_md->in_mem_addresses[parent_dir_md->in_mem_addresses_count-1]);
        

        return md;
    }
    else
        return NULL;
}

//write all bytes at time, file already exists and contents will be overwritten
int sfs_write(uint16_t id, char* path, void* contents, uint64_t size) {
    return -1;
}

int sfs_delete(uint16_t id, char* path) {
    return -1;
}

int sfs_read(uint16_t id, char* path, void* buffer, uint64_t b_size) {
    return -1;
}
