#ifndef _STRUCTURES_H_
#define _STRUCTURES_H_

#include <stdint.h>
#include "definitions.h"

typedef struct file_metadata {
    uint64_t size;
    uint32_t fragments_count;
    uint64_t first_fragment_address; //this is interpreted as a fragment or as an fragment definition structure, based on fragments_count value
    uint64_t *fragment_addresses;
} S_file_metadata;

typedef struct dir_metadata {
    uint32_t child_count;
    uint64_t child_list_address; //non-zero if the list of children addresses does not fit in the remaining space
    //TODO - in this structure we could also temporarily store some of the children addresses
    uint32_t in_mem_addresses_count;
    uint64_t* in_mem_addresses; //fill this in at initialisation, if they are not too many
} S_dir_metadata;

typedef struct metadata {
    uint16_t part_id;
    uint64_t address;
    uint64_t batch_address;
    uint8_t type;
    uint8_t name[NAME_SIZE];
    uint64_t parent_address;
    uint32_t hl_count;
    void* specific; //specific metadata, pointer to a file_metadata or dir_metadata
} S_metadata;

S_metadata* init_metadata_struct();
S_metadata* init_dir_struct(S_metadata* md);
S_metadata* init_file_struct(S_metadata* md);
int free_metadata_struct(S_metadata* md);
int free_dir_struct(S_dir_metadata* dir_md);
int free_file_struct(S_file_metadata* file_md);


typedef struct metadata_batch {
    uint64_t address;
    uint64_t size;
    uint64_t metadata_start;
    uint64_t file_capacity;
    uint64_t file_count;
    uint64_t index_table[ALLOWED_BYTES_IN_NAME_COUNT+1]; //first is 0x0, which is not used in file names, but present here for ease of addressing
    uint64_t file_count_for_index[ALLOWED_BYTES_IN_NAME_COUNT+1];
} S_metadata_batch;

typedef struct fs_definition {
    //    uint16_t id;
    uint64_t size;
    
    uint64_t block_map_address;
    uint64_t block_map_size; //computed as size/CONTENTS_TO_MAP_RATIO
    uint8_t *global_block_map; //this will get large, work on it in chunks later
    uint64_t free_blocks;

    //TODO - at some point we should no longer store all these batches in memory, should they be too many?
    uint64_t metadata_batch_count;
    uint64_t *metadata_batch_addresses;

    S_metadata_batch *metadata_batches;

    S_metadata* root_metadata;

    int fd; //real file backing our partition :D
} S_fs_definition;




#endif
