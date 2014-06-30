#ifndef _UTILS_H_
#define _UTILS_H_

#include <inttypes.h>

int printf_fd_info(int fd);

int printf_units(uint64_t bytes_count);

int printf_bits(uint8_t byte);

#endif

