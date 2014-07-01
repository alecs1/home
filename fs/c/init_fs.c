#include <stdint.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>

//these are for the printf warnings
#define __STDC_FORMAT_MACROS
#include <inttypes.h>


#include <stdlib.h>
#include <string.h>

//probably such headers should be missing from low level implementations
#include <stdio.h>


#include "definitions.h"
#include "structures.h"
#include "init_fs.h"
#include "global.h"
#include "utils.h"

const uint8_t  u8_zero = 0;
const uint64_t u64_zero = 0;

int verbose; //disable printing in some situations

S_fs_definition fs_defs[MAX_PARTITIONS_COUNT];
uint16_t part_count = 0;


//some functions which will pretend to write to an actuall disk, but actually call to seek and write :D
//move them to another file with primitives



//TODO - to simulate some constraints, also split their work into blocks of some limited size
uint64_t d_write(uint16_t id, uint64_t address, void* bytes, uint64_t size) {
    //UNUSED(id);
    int fd = fs_defs[id].fd;
    lseek(fd, address, SEEK_SET);
    //    lseek(static_fd, address, SEEK_SET);
    uint64_t b_wrote = 0;
    b_wrote = write(fd, bytes, size);

    if (verbose) {
        printf("%s, wrote %" PRIu64 " at %" PRIu64 ". ", __func__, b_wrote, address);
        printf_fd_info(fd);
    }
    return b_wrote;
}



uint64_t d_read(uint16_t id, uint64_t address, void* bytes, uint64_t size) {
}


//use it for example to write zeros
uint64_t d_write_repeat(uint16_t id, uint64_t address, void* bytes, uint64_t size, uint64_t repeat_count) {
    if (repeat_count > 10)
        verbose = 0;

    uint64_t b_wrote = 0;
    for(uint64_t i = 0; i < repeat_count; i++) {
        b_wrote += d_write(id, address, bytes, size);
        address += size;
    }

    verbose = 1;
    return b_wrote;
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
    off_t offset = lseek(fd, size-1, SEEK_END);
    write(fd, &u8_zero, 1);
    printf("%s, lseek off_t = %" PRIu64 "\n", __func__, offset);
    //close(fd);

    struct stat fd_info;
    *ret_code = fstat(fd, &fd_info);
    //these printfs might very well be wrong, bla bla
    printf("create file: st_ino=%" PRIu64 ", st_nlink=%d, st_size=%" PRIu64 ", st_blksize=%" PRIu64 "\n",
           fd_info.st_ino, fd_info.st_nlink, fd_info.st_size, fd_info.st_blksize);

    id = part_count;
    part_count += 1;

    fs_defs[id].size = size;
    fs_defs[id].fd = fd;
    
    ret_code = 0;
    return id;
}

//TODO - there's absolutely no way to make searches fast


//assumes space is free and overwrites everything
//TODO - think how to allocate some space for very small files, right after the metadata block
uint64_t init_metadata_batch(uint16_t id, uint64_t pos, uint64_t size) {
    if (size % DISK_BLOCK_BYTES != 0) {
        printf("%s - warning, metada block not multiple of block size, size=%" PRIu64 "\n",
               __func__, size);
        size = ROUND_TO_MULTIPLE_DOWN(size, DISK_BLOCK_BYTES);
        printf("%s - rounded to size=%" PRIu64 "\n", __func__, size);
    }

    uint64_t header_size = METADATA_HEADER_SIZE;
    uint64_t metadata_size = size - header_size;
    //metadata_size = ROUND_TO_MULTIPLE_DOWN(metadata_size, DISK_BLOCK_BYTES);

    uint64_t file_capacity = metadata_size / METADATA_SIZE;
    uint64_t batch_index = fs_defs[id].metadata_batch_count;

    fs_defs[id].metadata_batch_count += 1;
    fs_defs[id].metadata_batches =
        (S_metadata_batch*)malloc(fs_defs[id].metadata_batch_count * sizeof(S_metadata_batch));
    fs_defs[id].metadata_batch_addresses =
        (uint64_t*) realloc(fs_defs[id].metadata_batch_addresses,
                              fs_defs[id].metadata_batch_count * sizeof(uint64_t));
    
    fs_defs[id].metadata_batch_addresses[batch_index] = pos;
    fs_defs[id].metadata_batches[batch_index].address = pos;

    fs_defs[id].metadata_batches[batch_index].size = size;
    pos += d_write(id, pos, &size, METADATA_BATCH_SIZE);

    fs_defs[id].metadata_batches[batch_index].file_capacity = file_capacity;
    pos += d_write(id, pos, &file_capacity, FILE_CAPACITY_SIZE);

    fs_defs[id].metadata_batches[batch_index].file_count = 0;
    pos += d_write (id, pos, &fs_defs[id].metadata_batches[batch_index].file_count,
                    FILE_COUNT_SIZE);

    for(int i = 0; i <= ALLOWED_BYTES_IN_NAME_COUNT; i++) {
        pos += d_write (id, pos, (void*)&u64_zero, 8);
        fs_defs[id].metadata_batches[batch_index].index_table[i] = 0;
    }

    printf("%s - pos=%" PRIu64 ", wrote metadata header of size %" PRIu64 " at %" PRIu64 ", unused bytes: %" PRIu64 "\n",
           __func__,
           pos,
           pos - fs_defs[id].metadata_batch_addresses[batch_index],
           fs_defs[id].metadata_batch_addresses[batch_index],
           METADATA_BATCH_HEADER_SIZE - (pos - fs_defs[id].metadata_batch_addresses[batch_index]));
    
    
    pos = ROUND_TO_MULTIPLE_UP(pos, DISK_BLOCK_BYTES);
    
    if (pos != fs_defs[id].metadata_batch_addresses[batch_index] + METADATA_BATCH_HEADER_SIZE)
        printf("%s - error, pos=%" PRIu64 ", should be %" PRIu64 "\n",
               __func__,
               pos,
               fs_defs[id].metadata_batch_addresses[batch_index] + METADATA_BATCH_HEADER_SIZE);

    printf("%s, header size=%" PRIu64 ", at %" PRIu64 ", metadata size=%" PRIu64 ", at %" PRIu64 ", file_capacity=%" PRIu64 "\n",
           __func__,
           header_size,
           fs_defs[id].metadata_batch_addresses[batch_index],
           metadata_size,
           pos,
           file_capacity);

    size = header_size + metadata_size;

    return size;
}

uint8_t mark_global_block_map(uint16_t id, uint64_t first, uint64_t last, uint8_t allocated) {
    uint64_t a_first = first - first % DISK_BLOCK_BYTES;
    uint64_t a_last = last + last % DISK_BLOCK_BYTES;

    printf("%s - id=%" PRIu16 ", fist=%" PRIu64 ", last=%" PRIu64 ", allocated=%" PRIu8 "\n",
           __func__, id, first, last, allocated);
    
    if ((first != a_first) || (last != a_last)) {
        printf("%s - error: first or last not aligned: %" PRIu64 "->%" PRIu64 ", %" PRIu64 "->%" PRIu64 "\n",
               __func__, first, a_first, last, a_last);
    }

    //optimise later :D
    for (uint64_t block_addr = a_first; block_addr < a_last; block_addr += DISK_BLOCK_BYTES) {
        uint64_t map_byte = block_addr / PARTITION_BYTE_MULTIPLE;
        uint8_t map_bit = (block_addr % PARTITION_BYTE_MULTIPLE) / DISK_BLOCK_BYTES;
        if (allocated)
            fs_defs[id].global_block_map[map_byte] |= (1 << (7 - map_bit)); //fill bytes from "left" to "right"
        else
            fs_defs[id].global_block_map[map_byte] &= (0 << (7 - map_bit));

        printf("byte=%" PRIu64 ", bit=%" PRIu8 ", val=%" PRIu8 ", block at %" PRIu64 ", map=",
               map_byte, map_bit, allocated, block_addr);
        printf_bits(fs_defs[id].global_block_map[map_byte]); printf("\n");
    }

    d_write(id, fs_defs[id].block_map_address, fs_defs[id].global_block_map, fs_defs[id].block_map_size);
    return 0;
}





//TODO - endianess
uint64_t create_fs(uint64_t size) {

    int success_code = 0;

    size = size - (size % PARTITION_BYTE_MULTIPLE);
    
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

    b_wrote = d_write_repeat(id, pos, (void*)&u8_zero, 1, ID_SIZE);
    pos += b_wrote;
    b_wrote = d_write(id, pos, &size, VOLUME_SIZE_SIZE);
    pos += b_wrote;

    uint16_t metadata_blocks_count = 0;
    b_wrote = d_write(id, pos, &metadata_blocks_count, METADATA_BLOCK_COUNT_SIZE);
    pos += b_wrote;
    
    b_wrote = d_write_repeat(id, pos,(void*)&u8_zero, 1, METADATA_ADDRESSES_SIZE);
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


    pos += pos % DISK_BLOCK_BYTES;
    
    printf("Writing block map at address %" PRIu64 ", ", pos); printf_units(pos); printf("\n");

    fs_defs[id].block_map_address = pos;
    fs_defs[id].block_map_size = size/CONTENTS_TO_MAP_RATIO;
    fs_defs[id].global_block_map = (uint8_t*)malloc(fs_defs[id].block_map_size);
    memset(fs_defs[id].global_block_map, 0, fs_defs[id].block_map_size);
    printf("Allocated %" PRIu64 " bytes for the global_block_map\n", fs_defs[id].block_map_size);

    //pos += d_write_repeat(id, pos, &zero, 1, map_size);
    mark_global_block_map(id, 0,  pos-1, 1);
    printf("Block map size: "); printf_units(fs_defs[id].block_map_size); printf("\n");
    printf("Block map ends at %" PRIu64 ", ", pos); printf_units(pos); printf("\n");



    //we can now start writing the first block of metadata
    //say 1% of the whole filesystem size
    uint64_t block_size = size / 100;
    printf("Init first metadata block at %" PRIu64 ", ", pos); printf_units(pos); printf("\n");
    printf("Metadata block_size=%" PRIu64 ", ", block_size); printf_units(block_size); printf("\n");

    fs_defs[id].metadata_batch_count = 0;
    fs_defs[id].metadata_batch_addresses = NULL;
    pos = ROUND_TO_MULTIPLE_UP(pos, DISK_BLOCK_BYTES);
    block_size = init_metadata_batch(id, pos, block_size);
    //fs_defs[id].metadata_batch_count = 1;
    //fs_defs[id].metadata_batch_addresses = (uint64_t*) malloc(block_size);
    //fs_defs[id].metadata_batch_addresses[0] = pos;

    pos += block_size;

    printf("allocated for the first metadata batch, file_capacity=%" PRIu64 ", ends at %" PRIu64 ", ",
           fs_defs[id].metadata_batches[0].file_capacity, pos);
    printf_units(block_size); printf("\n");


    //now write / as the first directory


    if (success_code == 0)
        return size;
    else
        return success_code;
}


