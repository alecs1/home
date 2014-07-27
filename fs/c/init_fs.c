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
#include "basic_ops.h"
#include "global.h"
#include "utils.h"

const uint8_t  u8_zero = 0;
const uint64_t u64_zero = 0;
const uint64_t u64_ffff = 0xFFFFFFFFFFFFFFFF;


int generate_id(uint8_t* buffer) {
    for(int i = 0; i < ID_SIZE; i++) {
        switch (i%6) {
        case 0:
            buffer[i] = 0xAA;
            break;
        case 1:
            buffer[i] = 0xBB;
            break;
        case 2:
            buffer[i] = 0xCC;
            break;
        case 3:
            buffer[i] = 0xDD;
            break;
        case 4:
            buffer[i] = 0xEE;
            break;
        case 5:
            buffer[i] = 0xFF;
            break;
        default:
            break;
        }
    }
    return 0;
}



//return some partition id
//TODO - it looks like usually return code are "int", do this too
int16_t allocate_partition(uint64_t size, int* ret_code) {
    //this will become global
    uint16_t id = part_count;
    part_count += 1;
    char file_name[100];
    sprintf(file_name, "/home/alex/partition.bin.%d", id);
    printf("init partition in file %s, of size %" PRIu64 " :D\n", file_name, size);

    int fd = open(file_name, O_CREAT|O_RDWR|O_TRUNC, S_IRWXU);
    off_t offset = lseek(fd, size-1, SEEK_END);
    write(fd, &u8_zero, 1);
    printf("%s, lseek off_t = %" PRIu64 "\n", __func__, offset);
    //close(fd);

    struct stat fd_info;
    *ret_code = fstat(fd, &fd_info);
    //these printfs might very well be wrong, bla bla
    printf("%s - create file: fd=%d, st_ino=%" PRIu64 ", st_nlink=%d, st_size=%" PRIu64 ", st_blksize=%" PRIu64 "\n",
           __func__, fd, fd_info.st_ino, fd_info.st_nlink, fd_info.st_size, fd_info.st_blksize);

    fs_defs[id].size = size;
    fs_defs[id].fd = fd;
    
    ret_code = 0;
    return id;
}

//TODO - there's absolutely no way to make searches fast


//assumes space is free and overwrites everything
uint64_t init_metadata_batch(uint16_t id, uint64_t pos, uint64_t size) {
    //printf("%s - start\n", __func__);

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
    fs_defs[id].metadata_batches[batch_index].metadata_start =
        fs_defs[id].metadata_batches[batch_index].address + METADATA_BATCH_HEADER_SIZE;


    fs_defs[id].metadata_batches[batch_index].size = size;

    fs_defs[id].metadata_batches[batch_index].file_capacity = file_capacity;

    fs_defs[id].metadata_batches[batch_index].file_count = 0;

    
    fs_defs[id].metadata_batches[batch_index].index_table[0] = 0;

    //INDEX_TABLE
    //space for one name in each area

    uint64_t preallocated_count = 1;
    uint64_t crt_index_address = fs_defs[id].metadata_batches[batch_index].metadata_start;
    for(int i = ALPHABET_FIRST_BYTE; i <= ALPHABET_LAST_BYTE; i++) {
        fs_defs[id].metadata_batches[batch_index].index_table[i] = crt_index_address;
        crt_index_address += preallocated_count*METADATA_SIZE;
    }

    //CAPACITY_FOR_INDEX
    pos += d_write(id, pos, (void*)&u64_zero, FILE_COUNT_SIZE);
    for(int i = ALPHABET_FIRST_BYTE; i < ALPHABET_LAST_BYTE; i++) {
        fs_defs[id].metadata_batches[batch_index].file_capacity_for_index[i] = preallocated_count;
    }
    
    //FILE_COUNT_FOR_INDEX
    pos += d_write(id, pos, (void*)&u64_zero, FILE_COUNT_SIZE);
    for(int i = 0; i <= ALLOWED_BYTES_IN_NAME_COUNT; i++) {
        fs_defs[id].metadata_batches[batch_index].file_count_for_index[i] = 0;
    }

    uint64_t wrote = write_metadata_batch(&(fs_defs[id].metadata_batches[batch_index]));
    pos += wrote;

    printf("%s - pos=%" PRIu64 ", wrote metadata header of size %" PRIu64 " at %" PRIu64 ", unused bytes: %" PRIu64 "\n",
           __func__,
           pos,
           pos - fs_defs[id].metadata_batch_addresses[batch_index],
           fs_defs[id].metadata_batch_addresses[batch_index],
           METADATA_BATCH_HEADER_SIZE - (pos - fs_defs[id].metadata_batch_addresses[batch_index]));
    
    
    pos = ROUND_TO_MULTIPLE_UP(pos, DISK_BLOCK_BYTES);

    //checks    
    if (pos != fs_defs[id].metadata_batches[batch_index].metadata_start)
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
    

    mark_global_block_map(id, fs_defs[id].metadata_batch_addresses[batch_index], size, 1);

    //printf("%s - end\n", __func__);
    return size;
}


//name is empty, first entry in the first batch of metadata
uint8_t init_root_dir(uint16_t id, S_metadata_batch* parent) {
    S_metadata* root_metadata = init_dir_struct(NULL);
    memset(root_metadata->name, '/', 1);
    memset(root_metadata->name+1, 0x7F, NAME_SIZE-2);
    root_metadata->address = fs_defs[id].metadata_batches[0].metadata_start;
    root_metadata->batch_address = parent->address;
    root_metadata->part_id = id;
    fs_defs[id].root_metadata = root_metadata;

    //write down
    write_metadata(root_metadata);
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
    printf("allocate_partition, size: "); print_units(size); printf("\n");


    uint16_t version = VERSION;
    uint64_t b_wrote = 0;
    uint64_t pos = 0;

    //TODO:
    // 1 - error checking
    // 2 - shouldn't rely on a pointer to compute where we should write
    b_wrote = d_write(id, 0, &version, VERSION_SIZE);
    pos += b_wrote;

    uint8_t id_buffer[ID_SIZE];
    generate_id(id_buffer);
    b_wrote = d_write(id, pos, id_buffer, ID_SIZE);
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
        printf("%s - error, pos=%" PRIu64 ", START_BLOCK_SIZE=%" PRIu64 "\n",
               __func__, pos, (uint64_t)START_BLOCK_SIZE);
    }


    pos += pos % DISK_BLOCK_BYTES;
    
    printf("%s - writing block map at address %" PRIu64 ", ", __func__, pos);
    print_units(pos); printf("\n");

    fs_defs[id].block_map_address = pos;
    fs_defs[id].block_map_size = size/CONTENTS_TO_MAP_RATIO;
    fs_defs[id].global_block_map = (uint8_t*)malloc(fs_defs[id].block_map_size);
    memset(fs_defs[id].global_block_map, 0, fs_defs[id].block_map_size);
    
    printf("%s - allocated %" PRIu64 " bytes for the global_block_map\n",
           __func__, fs_defs[id].block_map_size);

    //pos += d_write_repeat(id, pos, &zero, 1, map_size);
    mark_global_block_map(id, 0,  pos-1, 1);
    printf("%s - block map size: ", __func__); print_units(fs_defs[id].block_map_size); printf("\n");
    printf("%s - block map ends at %" PRIu64 ", ", __func__, pos); print_units(pos); printf("\n");



    //we can now start writing the first block of metadata
    //say 2% of the whole filesystem size
    uint64_t block_size = size / 50;
    block_size = ROUND_TO_MULTIPLE_UP(block_size, DISK_BLOCK_BYTES);
    printf("%s - init first metadata block at %" PRIu64 ", ", __func__, pos); print_units(pos); printf("\n");
    printf("%s - metadata block_size=%" PRIu64 ", ", __func__, block_size); print_units(block_size); printf("\n");

    fs_defs[id].metadata_batch_count = 0;
    fs_defs[id].metadata_batch_addresses = NULL;
    pos = ROUND_TO_MULTIPLE_UP(pos, DISK_BLOCK_BYTES);
    block_size = init_metadata_batch(id, pos, block_size);
    //fs_defs[id].metadata_batch_count = 1;
    //fs_defs[id].metadata_batch_addresses = (uint64_t*) malloc(block_size);
    //fs_defs[id].metadata_batch_addresses[0] = pos;

    pos += block_size;

    printf("%s - allocated for the first metadata batch, file_capacity=%" PRIu64 ", ends at %" PRIu64 ", ",
           __func__, fs_defs[id].metadata_batches[0].file_capacity, pos);
    print_units(block_size); printf("\n");


    //now write / as the first directory
    init_root_dir(id, &(fs_defs[id].metadata_batches[0]));


    if (success_code == 0)
        return size;
    else
        return success_code;
}


