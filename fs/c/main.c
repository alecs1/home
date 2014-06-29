#include "init_fs.h"

#include <stdint.h>
#include <stdio.h>

int main(int argc, char* argv[]) {

    //let's say we init 50 MiB

    create_fs(50 * 1024 * 1024);

    printf("End\n");
}
