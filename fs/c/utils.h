#ifndef _UTILS_H_
#define _UTILS_H_

#include <inttypes.h>

int print_fd_info(int fd);

int print_units(uint64_t bytes_count);

int print_bits(uint8_t byte);

int print_bytes(uint8_t* bytes, uint64_t size);

#endif

