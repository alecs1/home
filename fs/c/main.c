#include "init_fs.h"
#include "test_api.h"
#include "global.h"



#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>

int main(int argc, char* argv[]) {

    //let's say we init 50 MiB
    printf("%s - part_count=%d\n", __func__, part_count);
    create_fs(50 * 1024 * 1024);
    printf("%s - part_count=%d\n", __func__, part_count);

    
    printf("/usr\n");
    S_metadata *usr = create(part_count-1, "/usr", TYPE_DIR);
    print_path(usr); print_metadata(usr);
    printf("\n\n\n");

    S_metadata *aaaa = create(part_count-1, "/aaaaaaaaaaaaaaaaaa", TYPE_DIR);
    print_path(aaaa); print_metadata(aaaa);
    printf("\n\n\n");

    S_metadata *usr_bin = create(part_count-1, "/usr/bin", TYPE_DIR);
    print_path(usr_bin); print_metadata(usr_bin);
    printf("\n\n\n");

    S_metadata *bin = create(part_count-1, "/bin", TYPE_DIR);
    if (bin != NULL) {
        print_path(bin); print_metadata(bin);
        printf("\n\n\n");
    }

    S_metadata *initrd_img = create(part_count-1, "/initrd.img", TYPE_FILE);
    print_path(initrd_img); print_metadata(initrd_img);
    printf("\n\n\n");

    S_metadata *bin_bash = create(part_count-1, "/bin/bash", TYPE_FILE);
    if (bin_bash != NULL) {
        print_path(bin_bash); print_metadata(bin_bash);
        printf("\n\n\n");
    }

    S_metadata *usr_bin_gcc = create(part_count-1, "/usr/bin/gcc", TYPE_FILE);
    if (usr_bin_gcc != NULL) {
        print_path(usr_bin_gcc); print_metadata(usr_bin_gcc);
        printf("\n\n\n");
    }

///printf("Rest of pointers: %p %p %p %p %p\n", usr_bin, bin, initrd_img, bin_bash, usr_bin_gcc);

    printf("End\n");
}
