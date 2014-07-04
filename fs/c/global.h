#ifndef _GLOBAL_H_
#define _GLOBAL_H_

#include "structures.h"

#define MAX_PARTITIONS_COUNT 10

extern int verbose; //disable printing in some situations
extern S_fs_definition fs_defs[MAX_PARTITIONS_COUNT];
extern uint16_t part_count;

#endif
