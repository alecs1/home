#include "structures.h"

S_metadata* init_metadata_struct() {
    S_metadata* root_metadata = (S_metadata*)malloc(sizeof(S_metadata));
    metadata->address = 0;
    metadata->type = 0;
    memset(metadata->name, 0, NAME_SIZE);
    metadata->parent_address = 0;
    metadata->hl_count = 1;
    return metadata;
}

S_metadata* init_dir_struct() {
    S_metadata *md = init_metadata_struct();
    md->type = 1;
    
    S_dir_metadata *dir_md = (S_dir_metadata*)malloc(sizeof(S_dir_metadata));
    dir_md->child_count = 0;
    dir_md->child_list_address = 0;
    
    md->specific = dir_md;
}

S_metadata* init_file_struct() {
    S_metadata *md = init_metadata_struct();
    md->type = 0;

    S_file_metadata *file_md = (S_file_metadata*)malloc(sizeof(S_dir_metadata));
    file_md->size = 0;
    file_md->fragments_count = 0;
    file_md->first_fragment_address = 0;
    file_md->fragment_addresses = NULL;
}
