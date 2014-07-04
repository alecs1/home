#include <stdlib.h>
#include <string.h>

#include "structures.h"

#include "definitions.h"

S_metadata* init_metadata_struct() {
    S_metadata* md = (S_metadata*)malloc(sizeof(S_metadata));
    md->part_id = 0;
    md->address = 0;
    md->type = 0;
    memset(md->name, 0, NAME_SIZE);
    md->parent_address = 0;
    md->hl_count = 1;
    md->specific = NULL;
    return md;
}

S_metadata* init_dir_struct(S_metadata *md) {
    if (md == NULL) {
        md = init_metadata_struct();
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
        md = init_metadata_struct();
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
