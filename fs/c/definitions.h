#ifndef _DEFINITIONS_H_
#define _DEFINITIONS_H_


#define DEBUG
//TODO TODO TODO - always align to DISK_BLOCK_BYTES addresses


//other generic definitions which should stay in their own files:
#define ROUND_TO_MULTIPLE_UP(x, m)   ( (x)%(m) ? ((x)/(m)+1)*(m) : (x) )
#define ROUND_TO_MULTIPLE_DOWN(x, m) ( (x)%(m) ? ((x)/(m)-1)*(m) : (x) )
#define UNUSED(x) (void)(x)

#define VERSION 1

#define ALPHABET_FIRST_BYTE 1
#define ALPHABET_LAST_BYTE 255
#define ALLOWED_BYTES_IN_NAME_COUNT 255 //any byte except NULL


#define ADDRESS_SIZE 8
#define ADDRESS_FFFF ((uint64_t)0xFFFFFFFFFFFFFFFF)
#define ADDRESS_ZERO ((uint64_t)0x0)

//various fillers
#define NAME_FILLER ((uint64_t)0xAAAAAAAAAAAAAAAA)
#define CHILDREN_ADDRESSES_FILLER ((uint64_t)0xADADADADADADADAD)
#define EMPTY_CHILDREN_ADDRESSES_FILLER ((uint64_t)0x0D0D0D0D0D0D0D0D)
#define FILE_CONTENTS_FILLER ((uint64_t)0xAFAFAFAFAFAFAFAF)
#define MIGRATED_METADATA_FILLER ((uint64_t)0x89ABCDEF89ABCDEF)
#define PLACEHOLDER_1B_FILLER ((uint8_t)0x11)
#define PLACEHOLDER_2B_FILLER ((uint16_t)0x2222)
#define PLACEHOLDER_3B_FILLER ((uint32_t)0x3333)
#define PLACEHOLDER_4B_FILLER ((uint32_t)0x44444444)
#define PLACEHOLDER_8B_FILLER ((uint64_t)0x8888888888888888)


#define DISK_BLOCK_BYTES 4096 //try to align most address requests at this value
#define SMALL_FILE_SIZE (2 * DISK_BLOCK_SIZE) //these files should be aligned right after a block of metadata, with a separate free space bitfield; for now all files with take a DISK_BLOCK_BYTES number of bytes

// start block
// sizes for data in the start block of the partition, in bytes
#define VERSION_SIZE 2
#define ID_SIZE 16
#define VOLUME_SIZE_SIZE 8 //2**64 bytes max volume size
#define METADATA_BLOCK_COUNT_SIZE 2 //2**16 max metadata blocks
#define METADATA_ADDRESSES_SIZE 8 * 65536 //8 bytes per address times 2**16 possible block addresses - for a total of 512 KiB of space
#define FREE_SPACE_SIZE 8 //how often to we recalculate this value?

//TODO - which parts of the data and metadata should have some error correction?

#define START_BLOCK_SIZE (VERSION_SIZE+ID_SIZE+VOLUME_SIZE_SIZE+METADATA_BLOCK_COUNT_SIZE+METADATA_ADDRESSES_SIZE+FREE_SPACE_SIZE)


//free space map, such that we don't have to cross the whole FS to find out if an area is written or not
//1 bit for each 4 KiB of disk, ratio map -> storage is 1 -> 2^15
//1 byte for each 32 KiB of disk, PARTITION_BYTE_MULTIPLE
//TODO - also align this to a 4096 address
#define BIT_TABLE_ADDRESS START_BLOCK_SIZE
#define CONTENTS_TO_MAP_RATIO (DISK_BLOCK_BYTES * 8)
#define PARTITION_BYTE_MULTIPLE (DISK_BLOCK_BYTES * 8) // choose partition size such that each byte in the contents map is entirely covered with existing partition space
//#define MAP_SIZE_SIZE 8 //we don't write this down anymore, it's calculated
#define BYTE_SIZE 8 //8 bits




//***Batch of metadata***
//header
#define METADATA_BATCH_SIZE 8 //metadata follows right away after the header and bit table, use this number to know where it ends!
#define FILE_CAPACITY_SIZE 8
#define FILE_COUNT_SIZE 8 //
#define INDEX_TABLE_SIZE ((1+ALLOWED_BYTES_IN_NAME_COUNT) * ADDRESS_SIZE) //256 indexes, first not used, one for each byte that may represent the first symbol in the name of a file, such that a fast jump can be done in the sorted names
#define CAPACITY_FOR_INDEX_SIZE ((1+ALLOWED_BYTES_IN_NAME_COUNT) * FILE_COUNT_SIZE)
#define FILE_COUNT_FOR_INDEX_SIZE ((1+ALLOWED_BYTES_IN_NAME_COUNT) * FILE_COUNT_SIZE)
#define METADATA_BATCH_HEADER_SIZE \
    ROUND_TO_MULTIPLE_UP( \
    (METADATA_BATCH_SIZE + FILE_CAPACITY_SIZE + FILE_COUNT_SIZE + INDEX_TABLE_SIZE + \
     FILE_COUNT_FOR_INDEX_SIZE + CAPACITY_FOR_INDEX_SIZE), DISK_BLOCK_BYTES)


//metadata of a single file/directory (should we split to two different definitions?)
//if it starts with zero this is not allocated
#define NAME_SIZE 256 //TODO - this is wasteful, for now assume everything ends with NULL to make it easy
#define PARENT_ADDRESS_SIZE 8 //refers to metadata of parent, of course :)
#define BATCH_ADDRESS_SIZE 8 //address of the batch inside which our structure is
#define HARDLINK_COUNT_SIZE 4 //we don't do hard links yet and also don't know how to implement them, but that doesn't stop us
#define TYPE_SIZE 1 //could be packed together with something else; 0 - file, 1 - dir
//POSIX stuff
#define POSIX_METADATA_SIZE 0
#define METADATA_HEADER_SIZE ROUND_TO_MULTIPLE_UP( \
    (NAME_SIZE + PARENT_ADDRESS_SIZE + BATCH_ADDRESS_SIZE +             \
     HARDLINK_COUNT_SIZE + TYPE_SIZE + POSIX_METADATA_SIZE), ADDRESS_SIZE)

#define METADATA_SIZE 1024 //1 KiB for metadata
#define REMAINING_SIZE (METADATA_SIZE - MEDATA_HEADER_SIZE) //this data will be available to store children info, in case of a directory, or a part of the contents in case of a file

#define TYPE_FILE 0
#define TYPE_DIR 1
#define TYPE_ANY 2
#define TYPE_NONE 3

//***Directory***
//we're going to assume that the list of files in a directory does not need to be split into multiple blocks
#define CHILD_COUNT_SIZE 4 //2**32 are more than enough files in a directory; lots of tools will start to fail if there exist milions of files in a directory
#define DIR_4_BYTES_PLACEHOLDER_1 4
#define CHILD_LIST_ADDRESS_SIZE 8 //address of the block of children addresses (that is, metadata addresses), 0x0 if all the chidlren fit inside here; at the moment we don't support having the list of children addresses split
//these children addresses could also point to another batch of metadata, but some migration should be considered
//rest is used 8 by 8 to store file addresses
#define CHILD_LIST_OFFSET (ROUND_TO_MULTIPLE_UP( \
    (METADATA_HEADER_SIZE + CHILD_COUNT_SIZE \
     + DIR_4_BYTES_PLACEHOLDER_1 + CHILD_LIST_ADDRESS_SIZE),    \
    ADDRESS_SIZE))
#define LOCAL_CHILDREN_CAPACITY ( (METADATA_SIZE - CHILD_LIST_OFFSET)/ADDRESS_SIZE )



//***File***
//0 - file fits entirely here
//1 - what's here plus the block at FIRST_BLOCK_ADDRESS_SIZE contains all the file
//2 - the file is split into many blocks, their addresses and sizes are defined in the block contained ata FIRST_BLOCK_ADDRESS_SIZE (pairs of 8 bytes address, 8 bytes size)
#define SIZE_SIZE 8
#define CONTENT_FRAGMENTS_COUNT_SIZE 4 //for now we'll assume this to be 0 or 1
#define FILE_4_BYTES_PLACEHOLDER_1 4
#define FIRST_FRAGMENT_ADDRESS_SIZE ADDRESS_SIZE //address of the file contents or to the split definition, if the file is split into more), 0x0 if it fits entirely here


#define READ_BASIC_MD 0
#define READ_FULL_MD 1



#endif










