#ifndef _COMMON_H_
#define _COMMON_H_

#define PORT_CHAR "1666"

//size of a request: request type, request value, client_id
#define IN_BUFFER_SIZE (sizeof(int) + sizeof(double) + sizeof(int))
//size of a reply: reply value, worker thread id
#define OUT_BUFFER_SIZE (sizeof(double) + sizeof(int))

//size of on item put into shared memory:
//success code, client, worker thread, request type, request value, reply value, open socket timestamp, close socket timestamp
#define MMAP_ITEM_SIZE (sizeof(int) + sizeof(int) + sizeof(int) + sizeof(int) + sizeof(double) + sizeof(double) + 2*sizeof(struct timespec))

//header of the shared memory, contents:
//no of currently running processes with access to this memory
#define MMAP_HEADER_SIZE (sizeof(int))

//0 - sqrt(x), 1 - pow(x, 2), 2 - pow(x, 3), 3 - sin(x), 4 - cos(x), 5 - hang-up
#define OP_SQRT 0
#define OP_POW2 1
#define OP_POW3 2
#define OP_SIN  3
#define OP_COS  4
#define OP_HUP  5
#define OP_TYPE_COUNT 6

#endif
