#ifndef _TEST_API_H_
#define _TEST_API_H_

//a couple of simple test functions, bases on which we'll test functionality and build a real API

//
int create(uint16_t id, char* path, uint8_t type);

//write all bytes at time, file already exists and contents will be overwritten
int write(uint16_t id, char* path, void* contents, uint64_t size);

int delete(uint16_t id, char* path);

int read(uint16_t id, char* path, void* buffer, uint64_t b_size);

#endif
