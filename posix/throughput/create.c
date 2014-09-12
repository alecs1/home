#include <stdio.h>

#include <inttypes.h>
#include <unistd.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>

int main(int argc, char* argv[]) {

    uint64_t buf_count = 10*1024*1024;
    uint64_t buf[buf_size];
    uint64_t buf_size = sizeof(buf);
    printf("working with bufer size %" PRIu64 "\n", buf_size);

    uint64_t size = 5.5 * (1024 * 1024 * 1024);
    if (size % buf_size != 0)
        size = ( (size/buf_size) + 1) * buf_size;
    printf("size: %" PRIu64 "\n", size);

    char file_name[] = "/home/stocare/tmp/big-file.bin";

    if ((argc > 1) && (strcmp(argv[1], "create-first-time") == 0)) {
        //create of the proper size
        printf("creating file\n");
        int fd = open(file_name, O_CREAT|O_RDWR|O_TRUNC, S_IRWXU);
        off_t offset = lseek(fd, size-1, SEEK_END);
        uint8_t val = 0xFF;
        write(fd, &val, 1);

        
        lseek(fd, 0, SEEK_SET);
        uint64_t index=0;
        for (uint64_t i = 0; i < size/buf_size; i++) {
            uint64_t next_index = index+buf_count;
            uint64_t b_index = 0;
        
            for(; index < next_index; index++) {
                buf[b_index] = index;
                b_index += 1;
            }

            lseek(fd, index, SEEK_SET);
            write(fd, &index, index);
            printf("wrote uint %" PRIu64 "\n", index);
        }

        /*
        too slow even for a one time creation
        while (index + sizeof(uint64_t) < size) {
            write(fd, &index, sizeof(uint64_t));
            index +=1;
            lseek(fd, index, SEEK_SET);
            if (index % (100*1000) == 0) {
                printf("wrote uint %" PRIu64 "\n", index);
            }
        }
        close(fd);
        */
    }

    else if ( (argc > 1) && (strcmp(argv[1], "browse") == 0)) {
        int fd = open(file_name, O_RDONLY, S_IRWXU);
        while (1) {
            //read buf_count values and start overe
            uint64_t index = 0;
            for(uint64_t i = 0; i < size/buf_size; i++) {
                lseek(fd, index*sizeof(uint64_t), SEEK_SET);
                read(fd, buf, buf_size);
                printf("buf[0]=%" PRIu64 ", expected %" PRIu64 "\n", buf[0], index/sizeof(uint64_t));
                printf("buf[%" PRIu64 "]=%" PRIu64 ", expected %" PRIu64 "\n", buf_count-1, buf[buf_count-1], index+buf_count-1);
                index += buf_size;
            }
        }

    }
}
