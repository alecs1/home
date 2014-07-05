#ifndef _TEST_API_H_
#define _TEST_API_H_

#include "structures.h"
//a couple of simple test functions, bases on which we'll test functionality and build a real API

//
S_metadata* create(uint16_t id, char* path, uint8_t type);
S_metadata* create_child(S_metadata* parent_md, char* name, uint8_t type);

//write all bytes at time, file already exists and contents will be overwritten
int sfs_write(uint16_t id, char* path, void* contents, uint64_t size);

int sfs_delete(uint16_t id, char* path);

int sfs_read(uint16_t id, char* path, void* buffer, uint64_t b_size);

#endif
