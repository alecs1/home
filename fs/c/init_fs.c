#include "definitions.h"
#include "structures.h"
#include "init_fs.h"

#include <stdint.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>

//these are for the printf warnings
#define __STDC_FORMAT_MACROS
#include <inttypes.h>


//probably such headers should be missing from low level implementations
#include <stdio.h>

int static_fd; //this is a copy of the fd, remove it when we implement multiple partitions

//some functions which will pretend to write to an actuall disk, but actually call to seek and write :D
//move them to another file with primitives

int printf_fd_info(int fd) {
    struct stat fd_info;
    fstat(fd, &fd_info);
    printf("fd=%d, st_ino=%" PRIu64 ", st_nlink=%d, st_size=%" PRIu64 ", st_blksize=%" PRIu64 "\n",
           fd, fd_info.st_ino, fd_info.st_nlink, fd_info.st_size, fd_info.st_blksize);
}

//TODO - to simulate some constraints, also split their work into blocks of some limited size
uint64_t d_write(uint16_t id, uint64_t address, void* bytes, uint64_t size) {
    UNUSED(id);
    lseek(static_fd, address, SEEK_SET);
    uint64_t b_wrote = 0;
    b_wrote = write(static_fd, bytes, size);

    printf("%s, wrote %" PRIu64 " at %" PRIu64 "\n", __func__, b_wrote, address);
    printf_fd_info(static_fd);
    return b_wrote;
}



uint64_t d_read(uint16_t id, uint64_t address, void* bytes, uint64_t size) {
}
//use it for example to write zeros
uint64_t d_write_repeat(uint16_t id, uint64_t address, void* bytes, uint64_t size, uint64_t repeat_count) {
}

//return some partition id
//TODO - it looks like usually return code are "int", do this too
int16_t allocate_partition(uint64_t size, int* ret_code) {
    //this will become global
    uint16_t id = 0;
    char file_name[100];
    sprintf(file_name, "/home/alex/partition.bin.%d", id);
    printf("init partition in file %s, of size %" PRIu64 " :D\n", file_name, size);

    int fd = creat(file_name, S_IRWXU);
    static_fd = fd;
    off_t offset = lseek(fd, size-1, SEEK_END);
    int zero = 0;
    write(fd, &zero, 1);
    printf("%s, lseek off_t = %" PRIu64 "\n", __func__, offset);
    //close(fd);

    struct stat fd_info;
    *ret_code = fstat(fd, &fd_info);
    //these printfs might very well be wrong, bla bla
    printf("create file: st_ino=%" PRIu64 ", st_nlink=%d, st_size=%" PRIu64 ", st_blksize=%" PRIu64 "\n",
           fd_info.st_ino, fd_info.st_nlink, fd_info.st_size, fd_info.st_blksize);


    ret_code = 0;
    return id;
}

//TODO - there's absolutely no way to make searches fast


//TODO - think about how some space can be preallocated
//assumes space is free and overwrites everything
uint64_t init_metadata_batch(uint16_t id, uint64_t pos, uint64_t size) {

    int file_count = 0;
    return file_count;
}

uint8_t mark_global_block_map(uint16_t id, uint64_t first, uint64_t last, uint8_t allocated) {

}


int printf_units(uint64_t bytes_count) {
    uint64_t KiB = bytes_count / 1024;
    double MiB = (double)bytes_count / (1024 * 1024);
    double GiB = (double)bytes_count / (1024 * 1024 * 1024);
    printf("KiB=%" PRIu64 ", MiB=%g, GiB=%g", KiB, MiB, GiB);
    return 0;
}


//TODO - endianess
uint64_t create_fs(uint64_t size) {

    int success_code = 0;

    size = size - (size % MAP_TO_CONTENTS_RATIO);
    
    int ret_code;
    uint16_t id = allocate_partition(size, &ret_code);
    if (ret_code != 0) {
        printf("id < 0\n");
    }
    printf("allocate_partition, size: "); printf_units(size); printf("\n");


    uint8_t version = VERSION;
    uint64_t b_wrote = 0;
    uint64_t pos = 0;

    //TODO:
    // 1 - error checking
    // 2 - shouldn't rely a pointer to compute where we should write
    b_wrote = d_write(id, 0, &version, VERSION_SIZE);
    pos += b_wrote;

    //skip id generation

    printf("Writing start header\n");
    uint8_t zero = 0;
    b_wrote = d_write_repeat(id, pos, &zero, 1, ID_SIZE);
    pos += b_wrote;
    b_wrote = d_write(id, pos, &size, VOLUME_SIZE_SIZE);
    pos += b_wrote;

    uint16_t metadata_blocks_count = 0;
    b_wrote = d_write(id, pos, &metadata_blocks_count, METADATA_BLOCK_COUNT_SIZE);
    pos += b_wrote;
    
    b_wrote = d_write_repeat(id, pos, &zero, 1, METADATA_ADDRESSES_SIZE);
    pos += b_wrote;
    
    
    uint64_t free_space = size - pos;
    if (free_space < 0) {
        printf("free_space < 0\n");
    }
    b_wrote = d_write(id, pos, &free_space, FREE_SPACE_SIZE);
    pos += b_wrote;

    
    if (pos != START_BLOCK_SIZE) {
printf("Error, pos=%" PRIu64 ", START_BLOCK_SIZE=%" PRIu64 "\n", pos, (uint64_t)START_BLOCK_SIZE);
    }


    printf("Writing block map at address %" PRIu64 ", ", pos); printf_units(pos); printf("\n");
    uint64_t map_size = size / MAP_TO_CONTENTS_RATIO;
    pos += d_write_repeat(id, pos, &zero, 1, map_size);
    mark_global_block_map(id, 0,  pos-1, 1);
    printf("Block map size: "); printf_units(map_size); printf("\n");
    printf("Block map ends at %" PRIu64 ", ", pos); printf_units(pos); printf("\n");



    //we can now start writing the first block of metadata
    //say 1% of the whole filesystem size
    uint64_t block_size = size / 100;
    printf("Init first metadata block at %" PRIu64 ", ", pos); printf_units(pos); printf("\n");
    printf("Metadata block_size=%" PRIu64 ", ", block_size); printf_units(block_size); printf("\n");

    uint64_t file_count = init_metadata_batch(id, pos, block_size);

    printf("allocated for the first metadata batch, file_count=%" PRIu64 ", ends at %" PRIu64 ", ", file_count, pos);
    printf_units(block_size); printf("\n");


    //now write / as the first directory


    if (success_code == 0)
        return size;
    else
        return success_code;
}


