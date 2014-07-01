#ifndef _STRUCTURES_H_
#define _STRUCTURES_H_

#include <stdint.h>

typedef struct metadata_batch {
    uint64_t address;
    uint64_t size;
    uint64_t metadata_start;
    uint64_t file_capacity;
    uint64_t file_count;
    uint64_t index_table[255];
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

    int fd; //real file backing our partition :D
} S_fs_definition;




#endif
