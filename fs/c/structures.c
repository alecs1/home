#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#define __STDC_FORMAT_MACROS
#include <inttypes.h>

#include "structures.h"
#include "definitions.h"
#include "basic_ops.h"


S_metadata* init_metadata_struct(uint8_t type) {
    S_metadata* md = (S_metadata*)malloc(sizeof(S_metadata));
    md->part_id = 0;
    md->address = 0;
    md->type = 0;
    memset(md->name, 0, NAME_SIZE);
    md->parent_address = 0;
    md->hl_count = 1;
    md->specific = NULL;
    if (type == TYPE_FILE)
        init_file_struct(md);
    else if (type == TYPE_DIR)
        init_dir_struct(md);
    return md;
}

S_metadata* init_dir_struct(S_metadata *md) {
    if (md == NULL) {
        md = init_metadata_struct(TYPE_NONE);
        md->type = 1;
    }
    
    S_dir_metadata *dir_md = (S_dir_metadata*)malloc(sizeof(S_dir_metadata));
    dir_md->child_count = 0;
    dir_md->child_list_address = 0;
    dir_md->in_mem_addresses_count = 0;
    dir_md->in_mem_addresses = NULL;
    
    md->specific = dir_md;

    return md;
}

S_metadata* init_file_struct(S_metadata *md) {
    if (md == NULL) {
        md = init_metadata_struct(TYPE_NONE);
        md->type = 0;
    }

    S_file_metadata *file_md = (S_file_metadata*)malloc(sizeof(S_dir_metadata));
    file_md->size = 0;
    file_md->fragments_count = 0;
    file_md->first_fragment_address = 0;
    file_md->fragment_addresses = NULL;

    md->specific = file_md;

    return md;
}

int free_metadata_struct(S_metadata* md) {
    return -1;
}
int free_dir_struct(S_dir_metadata* dir_md) {
    return -1;
}
int free_file_struct(S_file_metadata* file_md) {
    return -1;
}


int print_metadata(S_metadata* md) {
    if (md != NULL) {
        printf("S_metadata: part_id=%" PRIu16
               ", address=%" PRIu64
               ", name=%s"
               ", parent_address=%" PRIu64
               ", batch_address=%" PRIu64
               ", type=%" PRIu8
               ", hl_count=%" PRIu32 "\n",
               md->part_id, md->address, md->name,
               md->parent_address, md->batch_address, md->type, md->hl_count);
    }
    else {
        printf("%s - error, can't print NULL struct\n", __func__);
        return -1;
    }
    return 0;
}

int print_path(S_metadata* md) {
    char *path[1000];
    int path_count = 1;
    path[0] = (char*)malloc(NAME_SIZE);
    memcpy(path[0], md->name, NAME_SIZE);
    
    for(int i = 1; i < 1000; i++) {
        path[0] = NULL;
    }
    
    S_metadata* aux_md = read_metadata(md->part_id, md->address, READ_FULL_MD);
        
    while(aux_md->parent_address != fs_defs[aux_md->part_id].root_metadata->address) {
        S_metadata* parent_md = read_metadata(aux_md->part_id, aux_md->parent_address, READ_FULL_MD);
        path[path_count]= (char*)malloc(NAME_SIZE);
        memcpy(path[path_count], parent_md->name, NAME_SIZE);
        path_count += 1;
        free(aux_md);
        aux_md = parent_md;

        if (path_count >= 1000) {
            printf("%s - error, path_count=%d, either this is very deeply nested or this is an error\n",
                   __func__, path_count);
        }
    }

    free(aux_md);

    char* print_string = (char*)malloc(path_count*NAME_SIZE);
    char* pos = print_string;
    for(int i = path_count-1; i >= 0; i--) {
        pos += sprintf(pos, "/%s", path[i]);
    }
    printf("%s - full path:%s\n", __func__, print_string);
    free(print_string);
    return pos - print_string;
}
