#ifndef _BASIC_OPS_H_
#define _BASIC_OPS_H_

#include <inttypes.h>
#include <unistd.h>

#include "global.h"
#include "structures.h"



uint64_t d_write(uint16_t id, uint64_t address, void* bytes, uint64_t size);

uint64_t d_read(uint16_t id, uint64_t address, void* bytes, uint64_t size);

uint64_t d_write_repeat(uint16_t id, uint64_t address, void* bytes, uint64_t size, uint64_t repeat_count);

//works on DISK_BLOCK_BYTES
uint8_t mark_global_block_map(uint16_t id, uint64_t first, uint64_t size, uint8_t allocated);

/*
Will try not to use this
//works on METADATA_SIZE
//uint8_t mark_md_block_map(uint16_t id, uint64_t bm_address, uint64_t first, uint8_t allocated);
*/

S_metadata* read_metadata(uint16_t id, uint64_t address, uint8_t full);
uint64_t write_metadata(S_metadata* md);

S_metadata_batch* get_metadata_batch(uint16_t id, uint64_t address);


//checks
int check_metadata_batch_address(uint16_t id, uint64_t address);

#ifdef DEBUG
#define CHECK_metadata_batch_address(x, y) check_metadata_batch_address(x, y)
#else
#define CHECK_metadata_batch_address(x, y) 0
#endif


int check_free(uint16_t id, uint64_t address);
int check_allocated(uint16_t id, uint64_t address);
int check_bounds(uint16_t id, uint64_t address);
int check_is_metadata(uint16_t id, uint64_t address);
int check_metadata_struct(S_metadata* md);


#endif
