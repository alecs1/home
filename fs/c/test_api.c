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
        index += count;
        for(uint32_t i = 0; i < count; i++) {
            uint64_t crt_address = children[i];
            if ( (crt_address >= expected_min_index) && (crt_address <= expected_max_index) ) {
                //read metadata of this child from disk
                S_metadata *child = read_metadata(parent->part_id, crt_address, READ_BASIC_MD);
                //char* child_name = (char*) md->name;
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

    return ret_val;
}

S_metadata* create(uint16_t id, char* path, uint8_t type) {
    printf("%s - id=%" PRIu16 ", path=%s, type=%" PRIu8 "\n", __func__, id, path, type);
    //TODO - how about some sanitisation on path?
    
    S_metadata *md = NULL;

    /*
    if (type == TYPE_FILE) {
        md = init_file_struct();
    }
    else if (type == TYPE_DIR) {
        md = init_dir_struct();
    }
    md->part_id = id;
    */

    //split the path and search for the parent
    char *aux_path = (char*)malloc((strlen(path)+1) * sizeof(char));
    char *saveptr, *next_name, *crt_name;
    strcpy(aux_path, path);

    S_metadata* parent_dir = fs_defs[id].root_metadata;

    printf("%s - parent_dir:", __func__); print_metadata(parent_dir);

    crt_name = NULL;
    next_name = strtok_r(aux_path, "/", &saveptr);

    printf("%s - crt_name=%s, next_name=%s\n", __func__, crt_name, next_name);

    while (next_name != NULL) {
        crt_name = next_name;
        next_name = strtok_r(NULL, "/", &saveptr);
        printf("%s - crt_name=%s, next_name=%s\n", __func__, crt_name, next_name);
        if (next_name != NULL) {
            //next_name is the next child
            //crt_dir is the directory we're going to find now in parent_dir
            uint64_t crt_dir_addr = find_child(parent_dir, crt_name, TYPE_DIR);
            if (crt_dir_addr != 0) {
                if (parent_dir != fs_defs[id].root_metadata)
                    free_metadata_struct(parent_dir);
                parent_dir = read_metadata(id, crt_dir_addr, READ_FULL_MD);
            }
        }
        else
            break;
    }


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

S_metadata* create_child(S_metadata* parent_md, char* name, uint8_t type) {

    printf("%s - parent_md: ", __func__); print_metadata(parent_md);
    printf("%s - name=%s\n", __func__, name);

    int error = 0;
    //steps: establish where in the metadata it's created, write down
    S_metadata* md = NULL;
    //S_dir_metadata *dir_md = (S_dir_metadata*)parent_md->specific;

    S_metadata_batch *mdb = get_metadata_batch(parent_md->part_id, parent_md->batch_address);

    uint8_t first_char = (uint8_t)name[0];
    uint64_t md_address = 0;
    uint64_t start_address_for_index = mdb->index_table[first_char];
    uint64_t end_address_for_index = 0;
    if (name[0] < ALPHABET_LAST_BYTE)
        end_address_for_index = mdb->index_table[first_char+1];
    else {
        end_address_for_index = mdb->address + mdb->size;
    }

    printf("%s - start_address_for_index=%" PRIu64 ", end_address_for_index=%" PRIu64 "\n",
           __func__, start_address_for_index, end_address_for_index);

    if ( (start_address_for_index + mdb->file_count_for_index[first_char]*METADATA_SIZE) < end_address_for_index) {
        printf("%s - writing directly\n", __func__);
        md_address = start_address_for_index + mdb->file_count_for_index[first_char]*METADATA_SIZE;
        
        //some more checks:
        if (md_address % METADATA_SIZE != 0) {
            printf("%s - error, computed address %" PRIu64 " not multiple of %u\n",
                   __func__, md_address, METADATA_SIZE);
            error = 1;
            goto cleanup;
        }
        
        if (end_address_for_index - md_address < METADATA_SIZE) {
            printf("%s - error, computed address %" PRIu64 " does not have enough space before next filled element at %" PRIu64 "\n",
                   __func__, md_address, end_address_for_index);
            error = 1;
            goto cleanup;
        }

        //now we can proceed to write it
        md = init_metadata_struct(type);
        md->part_id = parent_md->part_id;
        md->address = md_address;
        memcpy(md->name, name, NAME_SIZE);
        md->parent_address = parent_md->address;
        md->batch_address = parent_md->batch_address;
      
    }
    else if ( (mdb->index_table[ALPHABET_LAST_BYTE] +
               mdb->file_count_for_index[ALPHABET_LAST_BYTE]*METADATA_SIZE) <
               mdb->address + mdb->size ){
        //there is free space in this metadata batch, just need to make it available by moving things around
        printf("%s - need to make space inside the metadata batch\n", __func__);
    }
    else {
        //need to move to another metadata batch (possibly allocating it), or maybe resize this one
        printf("%s - need to write this metadata to another batch or extend this one\n", __func__);
    }
        
        
cleanup:
    if (error == 0) {
        write_metadata(md);
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
