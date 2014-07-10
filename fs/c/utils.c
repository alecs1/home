#include "utils.h"

#include <sys/stat.h>

#include <stdio.h>

int print_fd_info(int fd) {
    struct stat fd_info;
    fstat(fd, &fd_info);
    printf("fd=%d, st_ino=%" PRIu64 ", st_nlink=%d, st_size=%" PRIu64 ", st_blksize=%" PRIu64 "\n",
           fd, fd_info.st_ino, fd_info.st_nlink, fd_info.st_size, fd_info.st_blksize);

    return 0;
}

int print_units(uint64_t bytes_count) {
    uint64_t KiB = bytes_count / 1024;
    double MiB = (double)bytes_count / (1024 * 1024);
    double GiB = (double)bytes_count / (1024 * 1024 * 1024);
    printf("KiB=%" PRIu64 ", MiB=%g, GiB=%g", KiB, MiB, GiB);
    return 0;
}

int print_bits(uint8_t byte) {
    for(int i = 7; i >= 0; i--) {
        printf("%d", (byte & (1 << i)) ? 1 : 0);
    }
    return 0;
}

int print_bytes(uint8_t* bytes, uint64_t size) {
    for(uint64_t i = 0; i < size; i++) {
        printf("%c", (char)bytes[i]);
    }
    return 0;
}
