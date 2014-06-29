#ifndef _DEFINITIONS_H_
#define _DEFINITIONS_H_

//TODO TODO TODO - always align to DISK_BLOCK_BYTES addresses

#define VERSION 1

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

#define START_BLOCK_SIZE (ID_SIZE+VERSION_SIZE+VOLUME_SIZE_SIZE+METADATA_BLOCK_COUNT_SIZE+METADATA_ADDRESSES_SIZE)


//free space map, such that we don't have to cross the whole FS to find out which if an area is written or not
//1 bit for each 4 KiB of disk, ratio map -> storage is 1 -> 2^15
//TODO - also align this to a 4096 address
#define BIT_TABLE_ADDRESS START_BLOCK_SIZE
#define MAP_TO_CONTENTS_RATIO (DISK_BLOCK_BYTES * 8)
#define MAP_SIZE_SIZE 8



//***Batch of metadata***
//header
#define ALLOCATED_COUNT_SIZE 8 //metadata follows right away after the header, use this number to know where it ends!
#define FILES_COUNT_SIZE 8 //
#define INDEX_TABLE_SIZE 255 * 8 //255 indexes, one for each byte that may represent the first symbol in the name of a file, such that a fast jump can be done in the sorted names
#define METADATA_BATCH_HEADER_SIZE (FILES_COUNT_SIZE+INDEX_TABLE_SIZE)


//metadata of a single file/directory (should we split to two different definitions?)
//if it starts with zero this is not allocated
#define NAME_SIZE 256 //TODO - this is wastefull
#define SIZE_SIZE 8
#define PARENTS_ADDRESS_SIZE 8 //refers to metadata of parent, of course :)
#define HARDLINK_COUNT_SIZE 4 //of course, we don't do hard links yet and also don't know how to implement them, but that doesn't stop us
#define TYPE_SIZE 1 //TODO - this is wastefull, could be packed together with something else
//POSIX stuff
#define POSIX_METADATA_SIZE 0
#define METADATA_HEADER_SIZE (NAME_SIZE + SIZE_SIZE + TYPE_SIZE + POSIX_METADATA_SIZE)

#define METADATA_SIZE 1024 //1 KiB for metadata - how does this correlate with 512/4096 bytes per sector?
#define REMAINING_SIZE (METADATA_SIZE - MEDATA_HEADER_SIZE) //this data will be available to store children info, in case of a directory, or a part of the contents in case of a file



//***Directory***
//we're going to assume that the list of files in a directory does not need to be split into multiple blocks
#define CHILDREN_COUNT 4 //2**32 are more than enough files in a directory; lots of tools will start to fail if there exist milions of files in a directory
#define CHILDREN_LIST_BLOCK_ADDRESS_SIZE 8 //address of the block of children addresses (that is, metadata addresses), 0x0 if all the chidlren fit inside here
//the metadata could also stay in another batch of metadata, but some migration should be considered
//rest is used 8 by 8 to store file addresses



//***File***
//0 - file fits entirely here
//1 - what's here plus the block at FIRST_BLOCK_ADDRESS_SIZE contains all the file
//2 - the file is split into many blocks, their addresses and sizes are defined in the block contained ata FIRST_BLOCK_ADDRESS_SIZE (pairs of 8 bytes address, 8 bytes size)
#define CONTENT_BLOCKS_COUNT_SIZE 4
#define FIRST_BLOCK_ADDRESS_SIZE 8 //address of the file contents or to the split definition, if the file is split into more), 0x0 if it fits entirely here




//other generic definitions which should stay in their own files:
#define UNUSED(x) (void)(x)

#endif









