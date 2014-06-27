#define VERSION 1

// start block
// sizes for data in the start block of the partition, in bytes
#define ID_SIZE 16
#define VERSION_SIZE 2
#define VOLUME_SIZE_SIZE 8 //2**64 bytes max volume size
#define METADATA_BLOCK_COUNT_SIZE 2 //2**16 max metadata blocks
#define METADATA_ADDRESSES_SIZE 8 * 65536 //8 bytes pe address times 2**16 possible block addresses
#define FREE_SPACE_SIZE 8 //how often to we recalculate this value?

//TODO - which parts of the data and metadata should have some error correction?

#define START_BLOCK_SIZE (ID_SIZE+VERSION_SIZE+VOLUME_SIZE_SIZE+METADATA_BLOCK_COUNTSIZE+METADATA_ADDRESSES_SIZE)


//batch of metadata
//header
#define FILES_COUNT_SIZE 8 //metadata follows right away after the header, use this number to know where it ends!
#define INDEX_TABLE_SIZE 255 * 8 //255 indexes, one for each byte that may represent the first symbol in the name of a file, such that a fast jump can be done in the sorted names
#define METADATA_BATCH_HEADER_SIZE (FILES_COUNT_SIZE+INDEX_TABLE_SIZE)


//metadata of a single file/directory (should we split to two different definitions?)
#define NAME_SIZE 256 //TODO - this is wastefull
#define SIZE_SIZE 8
#define TYPE_SIZE 1 //TODO - this is wastefull, could be packed together with something else
//POSIX stuff
#define POSIX_METADATA_SIZE 0
#define METADATA_HEADER_SIZE (NAME_SIZE + SIZE_SIZE + TYPE_SIZE + POSIX_METADATA_SIZE)

#define METADATA_SIZE 1024 //1 KiB for metadata - how does this correlate with 512/4096 bytes per sector?
#define REMAINING_SIZE (METADATA_SIZE - MEDATA_HEADER_SIZE) //this data will be available to store children info, in case of a directory, of a part of the contents in case of a file

//if this is a a directory
#define CHILDREN_COUNT 4 //2**32 are more than enough files in a directory; lots of tools will start to fail if there exist milions of files in a directory
#define CHILDREN_LIST_BLOCK_ADDRESS_SIZE 8 //address of the block of children addresses, 0x0 if all the chidlren fit inside here //the metadata could also stay in another batch of metadata
//rest is used 8 by eight to store file addresses

//if this is a file
#define IS_SPLIT_SIZE 1 //0 if the the file is entirely in the metadata, 1 if the file only has one block, 2 if the file is split into more
#define FIRST_BLOCK_ADDRESS_SIZE 8 //address of the file contents or to the split definition, if the file is split into more), 0x0 if it fits entirely here


#define CONTENT_BLOCKS_COUNT_SIZE 4 //if the contents are split into more blocks
//rest is used to store the file


#define 









