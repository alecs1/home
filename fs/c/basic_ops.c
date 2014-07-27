#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>

#include "basic_ops.h"
#include "utils.h"

S_fs_definition fs_defs[MAX_PARTITIONS_COUNT];
uint16_t part_count = 0;
int verbose;

//TODO - to simulate some constraints, also split their work into blocks of some limited size
uint64_t d_write(uint16_t id, uint64_t address, void* bytes, uint64_t size) {
    //UNUSED(id);
    int fd = fs_defs[id].fd;
    lseek(fd, address, SEEK_SET);
    //    lseek(static_fd, address, SEEK_SET);
    uint64_t b_wrote = 0;
    b_wrote = write(fd, bytes, size);

    verbose = 0;
    if (verbose) {
        printf("%s, wrote %" PRIu64 " at %" PRIu64 ".\n", __func__, b_wrote, address);
        print_fd_info(fd);
    }
    return b_wrote;
}



uint64_t d_read(uint16_t id, uint64_t address, void* bytes, uint64_t size) {
    int fd = fs_defs[id].fd;

    off_t off = lseek(fd, address, SEEK_SET);
    if (off == -1) {
        printf("%s - error, lseek failed, error:%s\n", __func__, strerror(errno));
    }
    else if (off != address) {
        printf("%s - error, lseek to unexpected address: %d", __func__, (int)off);
    }
    

    uint64_t b_read = 0;
    ssize_t read_result = read(fd, bytes, size);

    if (read_result > 0) {
        b_read = read_result;
    }
    else if (read_result == 0) {
        printf("%s - read 0 bytes from %" PRIu64 "\n", __func__, address);
        //return 0;
    }
    else {
        printf("%s - error reading from %" PRIu64 ", error: %s\n",
               __func__, address, strerror(errno));
        //return 0;
    }

    verbose = 0;
    if (verbose) {
        printf("%s, id=%" PRIu16 ", fd=%d, read %" PRIu64 " from %" PRIu64 ".\n",
               __func__, id, fd, b_read, address);
        print_fd_info(fd);
    }
    return b_read;
}


//use it for example to write zeros
uint64_t d_write_repeat(uint16_t id, uint64_t address, void* bytes, uint64_t size, uint64_t repeat_count) {
    if (repeat_count > 5)
        verbose = 0;

    uint64_t b_wrote = 0;
    for(uint64_t i = 0; i < repeat_count; i++) {
        b_wrote += d_write(id, address, bytes, size);
        address += size;
    }

    verbose = 1;
    return b_wrote;
}

uint8_t mark_global_block_map(uint16_t id, uint64_t first, uint64_t size, uint8_t allocated) {
    uint64_t a_first = ROUND_TO_MULTIPLE_DOWN(first, DISK_BLOCK_BYTES);

    uint64_t a_last = first + size;
    a_last = ROUND_TO_MULTIPLE_UP(a_last, DISK_BLOCK_BYTES);

    printf("%s - id=%" PRIu16 ", fist=%" PRIu64 ", last=%" PRIu64 ", allocated=%" PRIu8 "\n",
           __func__, id, first, a_last, allocated);
    
    if ((first != a_first) || (first+size != a_last)) {
        printf("%s - error: first or last not aligned: %" PRIu64 "->%" PRIu64 ", %" PRIu64 "->%" PRIu64 "\n",
               __func__, first, a_first, first+size, a_last);
    }

    //optimise later :D
    for (uint64_t block_addr = a_first; block_addr < a_last; block_addr += DISK_BLOCK_BYTES) {
        uint64_t map_byte = block_addr / PARTITION_BYTE_MULTIPLE;
        uint8_t map_bit = (block_addr % PARTITION_BYTE_MULTIPLE) / DISK_BLOCK_BYTES;
        if (allocated)
            fs_defs[id].global_block_map[map_byte] |= (1 << (7 - map_bit)); //fill bytes from "left" to "right"
        else
            fs_defs[id].global_block_map[map_byte] &= (0 << (7 - map_bit));

        /*
        printf("byte=%" PRIu64 ", bit=%" PRIu8 ", val=%" PRIu8 ", block at %" PRIu64 ", map=",
               map_byte, map_bit, allocated, block_addr);
        print_bits(fs_defs[id].global_block_map[map_byte]); printf("\n");
        */
    }

    d_write(id, fs_defs[id].block_map_address, fs_defs[id].global_block_map, fs_defs[id].block_map_size);
    return 0;
}


/*
int mark_md_block_map(uint16_t id, uint64_t bm_address, uint64_t first, uint64_t size, uint8_t allocated){
    int ret_val = 0;
    if (first%METADATA_SIZE != 0) {
        printf("%s - error - first = %" PRIu64 " not aligned to %d\n",
               __func__, first, METADATA_SIZE);
        return -1;
    }

    if (size%METADATA_SIZE != 0) {
        printf("%s - error - size =%" PRIu64 " not aligned to %d\n",
               __func__, size, METADATA_SIZE);
        return -1;
    }
}
*/


S_metadata* read_metadata(uint16_t id, uint64_t address, uint8_t full) {
    //some basic checks: address aligned at 1024, address inside an allocated batch area
    int success = 1;
    S_metadata *md = init_metadata_struct(TYPE_ANY);


    if (address % METADATA_SIZE != 0) {
        success = -1;
        printf("%s - error, address %" PRIu64 " not aligned at %u\n",
               __func__, address, METADATA_SIZE);
        goto finish;
    }

    uint8_t bytes[METADATA_SIZE];
    d_read(id, address, bytes, METADATA_SIZE);

    //printf("%s - bytes at %" PRIu64 ":\n", __func__, address);
    //print_bytes(bytes, METADATA_SIZE); printf("\n");
    
    uint64_t pos = 0;
    memcpy(md->name, bytes+pos, NAME_SIZE);   pos += NAME_SIZE;
    memcpy(&md->parent_address, bytes+pos, PARENT_ADDRESS_SIZE); pos += PARENT_ADDRESS_SIZE;
    memcpy(&md->batch_address, bytes+pos, BATCH_ADDRESS_SIZE);   pos += BATCH_ADDRESS_SIZE;
    memcpy(&md->hl_count, bytes+pos, HARDLINK_COUNT_SIZE);       pos += HARDLINK_COUNT_SIZE;
    memcpy(&md->type, bytes+pos, TYPE_SIZE);  pos += TYPE_SIZE;
    pos += POSIX_METADATA_SIZE;
    pos = ROUND_TO_MULTIPLE_UP(pos, ADDRESS_SIZE);

    //printf("%s - md until now: ", __func__); print_metadata(md); 
    
    md->part_id = id;
    md->address = address;
    
    //now that we filled data in, we can do further checks
    if (check_metadata_batch_address(id, md->batch_address) != 0) {
        printf("%s - error, metadata batch not found at %" PRIu64 ", address %" PRIu64 " probably is not a valid metadata location\n",
               __func__, md->batch_address, address);
        success = -1;
        goto finish;
    }

    //we now already have enough data to do useful stuff with this item
    if (full == READ_FULL_MD) {
        if (md->type == TYPE_FILE) {

            init_file_struct(md);
            S_file_metadata* f_md = (S_file_metadata*)md->specific;
            memcpy(&f_md->size, bytes+pos, SIZE_SIZE); pos += SIZE_SIZE;
            memcpy(&f_md->fragments_count, bytes+pos, CONTENT_FRAGMENTS_COUNT_SIZE);
            pos += CONTENT_FRAGMENTS_COUNT_SIZE;
            pos += FILE_4_BYTES_PLACEHOLDER_1;
            memcpy(&f_md->first_fragment_address, bytes+pos, FIRST_FRAGMENT_ADDRESS_SIZE);
            pos += FIRST_FRAGMENT_ADDRESS_SIZE;
            //printf("%s - read file=%s, size=%" PRIu64 "\n", __func__, md->name, f_md->size);

        }
        else if (md->type == TYPE_DIR) {

            init_dir_struct(md);
            S_dir_metadata *d_md = (S_dir_metadata*)md->specific;
            memcpy(&d_md->child_count, bytes+pos, CHILD_COUNT_SIZE); pos += CHILD_COUNT_SIZE;
            pos += DIR_4_BYTES_PLACEHOLDER_1;
            memcpy(&d_md->child_list_address, bytes+pos, CHILD_LIST_ADDRESS_SIZE);
            pos += CHILD_LIST_ADDRESS_SIZE;
            
            pos = ROUND_TO_MULTIPLE_UP(pos, ADDRESS_SIZE);
            if (pos != CHILD_LIST_OFFSET) {
                printf("%s - error, pos=%" PRIu64 ", CHILD_LIST_OFFSET: %" PRIu64 "\n",
                       __func__, pos, (uint64_t)CHILD_LIST_OFFSET);
            }
            
            if (d_md->child_list_address == 0) {
                //will fill in all addresses, since they are a few
                d_md->in_mem_addresses = (uint64_t*)malloc(d_md->child_count * sizeof(uint64_t));
                memcpy(d_md->in_mem_addresses, bytes+pos, d_md->child_count * sizeof(uint64_t));
                d_md->in_mem_addresses_count = d_md->child_count;
            }
            else {
                printf("%s - error, reading the list of many children not implemented\n", __func__);
            }
            
        }
        else {
            printf("%s - error, wrong fs object type %" PRIu8 ", at address %" PRIu64 "\n",
               __func__, md->type, address);
            goto finish;
        }
        printf("%s - metadata: ", __func__); print_metadata(md);
    }
    
finish:
    if (success == 1)
        return md;
    else {
        free_metadata_struct(md);
        return NULL;
    }

}

uint64_t write_metadata(S_metadata* md) {
    uint64_t pos = 0;

    if (md->specific == NULL) {
        printf("%s - error, cannot write down metadata not fully populated\n", __func__);
        return 0;
    }

    if (check_metadata_struct(md) != 0) {
        printf("%s - error, invalid metadata structure\n", __func__);
        return 0;
    }

    void *buffer = malloc(METADATA_SIZE);
    memset(buffer, 0, METADATA_SIZE);

    memcpy(buffer+pos, md->name, NAME_SIZE);       pos += NAME_SIZE;
    
#ifdef DEBUG
    memset(buffer+strlen((char*)md->name)+1, (int)NAME_FILLER, NAME_SIZE-(strlen((char*)md->name)+1) );
#endif
    

    memcpy(buffer+pos, &md->parent_address, PARENT_ADDRESS_SIZE);  pos += PARENT_ADDRESS_SIZE;
    memcpy(buffer+pos, &md->batch_address, BATCH_ADDRESS_SIZE);    pos += BATCH_ADDRESS_SIZE;
    memcpy(buffer+pos, &md->hl_count, HARDLINK_COUNT_SIZE);        pos += HARDLINK_COUNT_SIZE;
    memcpy(buffer+pos, &md->type, TYPE_SIZE);      pos += TYPE_SIZE;
    pos = ROUND_TO_MULTIPLE_UP(pos, ADDRESS_SIZE);
    
    if (md->type == TYPE_FILE) {
        //pos += d_write(id, pos, 
        S_file_metadata *f_md = (S_file_metadata*)md->specific;

        memcpy(buffer+pos, &f_md->size, SIZE_SIZE); pos += SIZE_SIZE;
        memcpy(buffer+pos, &f_md->fragments_count, CONTENT_FRAGMENTS_COUNT_SIZE);
        pos += CONTENT_FRAGMENTS_COUNT_SIZE;
        memset(buffer+pos, PLACEHOLDER_4B_FILLER, FILE_4_BYTES_PLACEHOLDER_1);
        pos += FILE_4_BYTES_PLACEHOLDER_1;
        memcpy(buffer+pos, &f_md->first_fragment_address, FIRST_FRAGMENT_ADDRESS_SIZE);
        pos += FIRST_FRAGMENT_ADDRESS_SIZE;

        memset(buffer+pos, (int)FILE_CONTENTS_FILLER, METADATA_SIZE-pos);
        
        if (f_md->fragments_count > 0)
            printf("%s - error, file contents writing not yet implemented\n", __func__);
    }
    else if (md->type == TYPE_DIR) {
        S_dir_metadata *d_md = (S_dir_metadata*)md->specific;
        memcpy(buffer+pos, &d_md->child_count, CHILD_COUNT_SIZE); pos += CHILD_COUNT_SIZE;
        memset(buffer+pos, PLACEHOLDER_4B_FILLER, DIR_4_BYTES_PLACEHOLDER_1);
        pos += DIR_4_BYTES_PLACEHOLDER_1;
        memcpy(buffer+pos, &d_md->child_list_address, CHILD_LIST_ADDRESS_SIZE);
        pos += CHILD_LIST_ADDRESS_SIZE;

        if (d_md->child_count > 0) {

            if (d_md->child_count <= LOCAL_CHILDREN_CAPACITY) {
                if (d_md->child_list_address != 0) {
                    printf("%s - error - TODO - implement deleting of space allocated for list of children metadata\n",
                           __func__);
                }
                else {
                    memcpy(buffer+pos, d_md->in_mem_addresses, d_md->child_count*sizeof(uint64_t));
                    //for(uint32_t i = 0; i < d_md->child_count; i++) {
                    //memcpy(buffer+pos, &d_md->in_mem_addresses[i], ADDRESS_SIZE);
                    //}
                    pos += d_md->child_count*sizeof(uint64_t);
                    memset(buffer+pos, (int)CHILDREN_ADDRESSES_FILLER, METADATA_SIZE-pos);
                }
            }
            else {
                if (d_md->child_list_address != 0) {
                    printf("%s -error - TODO - implement writing outside the local metadata space\n",
                           __func__);
                }
                else {
                    printf("%s -error - TODO - allocated and write outside the local metadata space\n",
                           __func__);
                }
            }
        }
    }
    
    uint64_t b_wrote = d_write(md->part_id, md->address, buffer, METADATA_SIZE);
    free(buffer);
    return b_wrote;
}

uint64_t write_metadata_batch(S_metadata_batch* mdb) {
    uint16_t part_id = mdb->part_id;
    uint64_t pos = 0;

    void* buffer = malloc(METADATA_BATCH_HEADER_SIZE);
    memcpy(buffer+pos, &mdb->size, METADATA_BATCH_SIZE);
    pos += METADATA_BATCH_SIZE;
    memcpy(buffer+pos, &mdb->file_capacity, FILE_CAPACITY_SIZE);
    pos += FILE_CAPACITY_SIZE;
    memcpy(buffer+pos, &mdb->file_count, FILE_COUNT_SIZE);
    pos += FILE_CAPACITY_SIZE;

    memcpy(buffer+pos, mdb->index_table,
                  (1+ALLOWED_BYTES_IN_NAME_COUNT)*ADDRESS_SIZE);
    pos += (1+ALLOWED_BYTES_IN_NAME_COUNT)*ADDRESS_SIZE;

    memcpy(buffer+pos, mdb->file_capacity_for_index,
                  (1+ALLOWED_BYTES_IN_NAME_COUNT)*FILE_CAPACITY_SIZE);
    pos += (1+ALLOWED_BYTES_IN_NAME_COUNT)*FILE_CAPACITY_SIZE;

    memcpy(buffer+pos, mdb->file_count_for_index,
                  (1+ALLOWED_BYTES_IN_NAME_COUNT)*FILE_COUNT_SIZE);
    pos += (1+ALLOWED_BYTES_IN_NAME_COUNT)*FILE_COUNT_SIZE;

    

    if (ROUND_TO_MULTIPLE_UP(pos, DISK_BLOCK_BYTES) != METADATA_BATCH_HEADER_SIZE) {
        printf("%s - error pos=%" PRIu64 " different from expected size=%" PRIu64 "\n",
               __func__, pos, (uint64_t)METADATA_BATCH_HEADER_SIZE);
        pos = 0;
        goto cleanup;
    }
    
    d_write(part_id, mdb->address, buffer, METADATA_BATCH_HEADER_SIZE);

    
 cleanup:
    free(buffer);
    return pos;
}

S_metadata_batch* get_metadata_batch(uint16_t id, uint64_t address) {
    S_metadata_batch* mdb = NULL;

    for(uint64_t i = 0; i < fs_defs[id].metadata_batch_count; i++)
        if (address == fs_defs[id].metadata_batch_addresses[i]) {
            mdb = &fs_defs[id].metadata_batches[i];
            break;
        }

    return mdb;
}


int check_metadata_batch_address(uint16_t id, uint64_t address) {
    
    for(uint64_t i = 0; i < fs_defs[id].metadata_batch_count; i++)
        if (fs_defs[id].metadata_batch_addresses[i] == address)
            return 0;
    return -1;
}

int check_metadata_struct(S_metadata* md) {
    int ret = 0;

    if (md->part_id >= part_count) {
        ret+=1;
        printf("%s - wrong part_id %" PRIu16 "\n", __func__, md->part_id);
    }
    
    if (md->address % METADATA_SIZE != 0) {
        ret += 1;
        printf("%s - wrong alignment: %" PRIu64 "\n", __func__, md->address);
    }

    uint64_t len = strlen((char*)md->name);
    if (len >= NAME_SIZE) {
        ret+=1;
        printf("%s - invalid name %s of len %" PRIu64 "\n", __func__, md->name, len);
    }
    else if ( (len == 0) && (md->address != fs_defs[md->part_id].root_metadata->address) ) {
        ret+=1;
        printf("%s - name is empty without being root dir\n", __func__);
    }

    if (md->parent_address % METADATA_SIZE != 0) {
        ret+=1;
        printf("%s - parent_address wrong alignment: %" PRIu64 "\n", __func__, md->parent_address);
    }

    if (check_metadata_batch_address(md->part_id, md->batch_address) != 0) {
        ret+=1;
        printf("%s - metadata_batch_address is wrong: %" PRIu64 "\n", __func__, md->batch_address);
    }

    if ( (md->type < TYPE_FILE) || (md->type > TYPE_DIR) ) {
        ret+=1;
        printf("%s - wrong file type: %" PRIu8 "\n", __func__, md->type);
    }

    if (md->specific == NULL) {
        ret+=1;
        printf("%s - specific is not populated\n", __func__);
    }
    else {
        if (md->type == TYPE_FILE) {
            //just trigger a read from the addresses for now
            S_file_metadata* file_md = (S_file_metadata*)md->specific;
            uint64_t size = file_md->size;
            UNUSED(size);            
        }
        else if (md->type == TYPE_DIR) {
            S_dir_metadata* dir_md = (S_dir_metadata*)md->specific;
            uint32_t child_count = dir_md->child_count;
            UNUSED(child_count);
        }
    }

    if (ret != 0)
        printf("%s - %d errors in structure\n", __func__, ret);

    return ret;
}
