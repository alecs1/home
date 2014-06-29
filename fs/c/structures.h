#ifndef _STRUCTURES_H_
#define _STRUCTURES_H_

#include <stdint.h>


typedef struct fs_definition {
    uint16_t id;
    uint64_t size;
    uint8_t *global_block_map; //this will get large, work on it in chunks later
    int fd_real_file;
} S_fs_definition;


#endif
