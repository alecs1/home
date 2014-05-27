#include <unistd.h>
#include <sys/wait.h>
#include <stdio.h>
#include <sys/socket.h>
#include <sys/types.h>
//#include <sys/stat.h>
#include <time.h>
#include <sys/epoll.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <netdb.h>
#include <string.h>
#include <semaphore.h>
#include <errno.h>

#include <sys/mman.h>


#include "common.h"

#define NO_OF_REQUESTS 5000
#define ERRSTR_LEN 500
#define REPLY_TIMEOUT 1 //30 seconds

//worker processes write in a shared memory area, without access control (each writes in his own cell)
//they also write over a file provided by the parent, with access controlled via a semaphore
int doSocketOperations(int client_id, void* mapped_mem, char* semaphore_name, char* out_file_name) {

    int ret_val = 0;
    char err_str[ERRSTR_LEN];
    //prepare buffer for writing request
    int urandom = open("/dev/urandom", O_RDONLY);
    int op;
    double value;
    read(urandom, &op, sizeof(int));
    read(urandom, &value, sizeof(double));
    if (op < 0)
        op = -op;
    if (op > OP_TYPE_COUNT) {
        op = op % OP_TYPE_COUNT;
    }
    
    if (value < 0)
        value = -value;
    value += 1;
    while (value > 1000000)
        value /= 19;

    char request_buf[IN_BUFFER_SIZE];
    int buffer_index = 0;
    memcpy(request_buf, &op, sizeof(op)); buffer_index += sizeof(op);
    memcpy(request_buf+buffer_index, &value, sizeof(value)); buffer_index += sizeof(value);
    memcpy(request_buf+buffer_index, &client_id, sizeof(client_id)); buffer_index += sizeof(client_id);

    printf("doSocketOperations - client_id=%d, read from /dev/urandom: op=%d, value=%g\n", client_id, op, value);
    
    //end buffer preparation


    struct addrinfo hints;
    struct addrinfo *result;
    memset(&hints, 0, sizeof(struct addrinfo));
    hints.ai_family = AF_INET6;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_protocol = 0;

    int res = getaddrinfo(NULL, PORT_CHAR, &hints, &result);
    if (res != 0) {
        snprintf(err_str, ERRSTR_LEN, "doSocketOperation - client_id=%d, getaddrinfo failed.", client_id);
        ret_val = -1;
        goto print_results;
    }

    int socket_fd;
    struct addrinfo *crt_addr_i;
    for(crt_addr_i = result; crt_addr_i != NULL; crt_addr_i = crt_addr_i->ai_next) {
        socket_fd = socket(crt_addr_i->ai_family, crt_addr_i->ai_socktype, crt_addr_i->ai_protocol);

        if (socket_fd == -1) {
            printf("doSocketOperations - client_id=%d, socket() failed\n", client_id);
            continue;
        }
        if (connect(socket_fd, crt_addr_i->ai_addr, crt_addr_i->ai_addrlen) == 0) {
            printf("doSocketOperations - client_id=%d, successfully connected\n", client_id);
            break;
        }
        else {
            snprintf(err_str, ERRSTR_LEN, "doSocketOperations - client_id=%d, connect failed.", client_id);
            ret_val = -1;
            goto print_results; //not much cleanup, the process will exist anyway
        }
        close(socket_fd);
    }

    if (crt_addr_i == NULL) {
        printf("doSocketOperations - client_id=%d failed to connect\n", client_id);
        snprintf(err_str, ERRSTR_LEN, "doSocketOperations - client_id=%d failed to connect.", client_id);
        ret_val = -1;
        goto print_results;
    }

    freeaddrinfo(result);

    //printf("doSocketOperations - client_id=%d, write()\n", client_id);
    int written_bytes = write(socket_fd, request_buf, IN_BUFFER_SIZE);
    //printf("doSocketOperations - client_id=%d, wrote %d bytes\n", client_id, written_bytes);

    //now wait at most two minutes for a reply
    struct epoll_event event_descriptor, events[10];
    
    int epoll_fd = epoll_create1(0);
    if (epoll_fd == -1) {
        snprintf(err_str, ERRSTR_LEN, "doSocketOperation - client_id=%d, epoll_create1 failed.", client_id);
        ret_val = -1;
        goto print_results;
    }
    
    event_descriptor.events = EPOLLIN;
    event_descriptor.data.fd = socket_fd;
    if (epoll_ctl(epoll_fd, EPOLL_CTL_ADD, socket_fd, &event_descriptor) == 1) {
        snprintf(err_str, ERRSTR_LEN, "doSocketOperations - client_id=%d, epoll_ctl-EPOLL_CTL_ADD failed.", client_id);
        ret_val = -2;
        goto print_results;
    }

    int read_bytes = 0;
    char reply_buf[OUT_BUFFER_SIZE];
    time_t start_time = time(NULL);
    time_t elapsed_time = 0;
    int spin_count = -1;
    while ((elapsed_time <= REPLY_TIMEOUT) && (read_bytes < OUT_BUFFER_SIZE)) {
        spin_count += 1;
        int nfds = epoll_wait(epoll_fd, events, 10, REPLY_TIMEOUT);
        if (nfds == -1) {
            snprintf(err_str, ERRSTR_LEN, "doSocketOperations - client_id=%d, epoll_wait failed.", client_id);
            ret_val = -3;
            goto print_results;
        }
        for (int i = 0; i < nfds; i++) {
            if (events[i].data.fd == socket_fd) {
                int len;
                ioctl(socket_fd, FIONREAD, &len);
                if (len < 0)
                    continue;
                int max_read_bytes = OUT_BUFFER_SIZE - read_bytes;
                int res = read(socket_fd, reply_buf + read_bytes, max_read_bytes);
                if (res >= 0)
                    read_bytes += res;
                else {
                    snprintf(err_str, ERRSTR_LEN, "doSocketOperations - client_id=%d, read returned negative value %d", client_id, res);
                    ret_val = -1;
                    goto print_results;
                }
                //printf("doSocketOperations - read %d bytes, remaining %d\n", read_bytes, IN_BUFFER_SIZE - read_bytes);
            }
            else {
                printf("doSocketOperations - client_id=%d, epoll_wait woke up on unknown fd\n", client_id);
            }
        }
        time_t crt_time = time(NULL);
        elapsed_time = crt_time - start_time;        
    }
    //if (read_bytes == 0)
        //printf("doSocketOperations - client_id=%d, nothing received on socket, spin_count=%d\n", client_id, spin_count);
    if (read_bytes < OUT_BUFFER_SIZE) {
        snprintf(err_str, ERRSTR_LEN, "doSocketOperations - client_id=%d, read only %d bytes, spin_count=%d.", client_id, read_bytes, spin_count);
        ret_val = -4;
        goto print_results;
    }
    else {
        printf("doSocketOperations - client_id=%d, read %d bytes, spin_count=%d, proceding to process reply\n", client_id, read_bytes, spin_count);
    }

 print_results:
    if (ret_val != 0) {
        printf("doSocketOperations - client_id=%d failed with the following error:%s\n", client_id, err_str);
    }

    //if everything worked, we can now interpret the buffer
    double reply_val = -1;
    int reply_thread = -1;
    int buf_index = 0;

    if (ret_val == 0) {
        memcpy(&reply_val, reply_buf+buf_index, sizeof(reply_val)); buf_index += sizeof(reply_val);
        memcpy(&reply_thread, reply_buf+buf_index, sizeof(reply_thread)); buf_index += sizeof(reply_thread);
    }

    printf("doSocketOperations - client_id=%d, value=%g, op=%d, reply_val=%g, reply_thread=%d\n",
           client_id, value, op, reply_val, reply_thread);


    int write_offset = MMAP_HEADER_SIZE + client_id * MMAP_ITEM_SIZE;
    void *write_address = mapped_mem + write_offset;
    write_offset = 0;
    memcpy(write_address + write_offset, &ret_val, sizeof(ret_val)); write_offset += sizeof(ret_val);
    memcpy(write_address + write_offset, &client_id, sizeof(client_id)); write_offset += sizeof(client_id);
    memcpy(write_address + write_offset, &reply_thread, sizeof(reply_thread)); write_offset += sizeof(reply_thread);
    memcpy(write_address + write_offset, &op, sizeof(op)); write_offset += sizeof(op);
    memcpy(write_address + write_offset, &value, sizeof(value)); write_offset += sizeof(value);
    memcpy(write_address + write_offset, &reply_val, sizeof(reply_val)); write_offset += sizeof(reply_val);
    memset(write_address + write_offset, 0, 2 * sizeof(struct timespec)); write_offset += 2*sizeof(struct timespec);


    printf("doSocketOperations - client_id=%d, waiting for out file semaphore\n", client_id);
    sem_t* file_semaphore = sem_open(semaphore_name, O_CREAT, O_RDWR, 1);
    if (file_semaphore == SEM_FAILED) {
        int error_val = errno;
        printf("doSocketOperations - client_id=%d, sem_open failed, errno=%d - %s\n", client_id, error_val, strerror(error_val));
        return -1;
    }
    sem_wait(file_semaphore);
    int out_file_fd = open(out_file_name, O_RDWR, S_IRWXU);
    //printf("doSocketOperations - client_id=%d, took the file\n", client_id);
    int seek_place = lseek(out_file_fd, 0, SEEK_END);
    if (seek_place == -1) {
        int errno_val = errno;
        printf("doSocketOperations - client_id=%d, lseek failed, errno=%d - %s\n", client_id, errno_val, strerror(errno_val));
        ret_val = -1;
        goto print_cleanup;
    }
    if (ret_val == 0) {
        dprintf(out_file_fd, "%d\t %d\t %g\t %g\n", client_id, op, value, reply_val);
    }
    else {
        dprintf(out_file_fd, "%d\t %d\t %g\t - XXXXXXXXXXXXXXXX - failed with the following error: %s\n", client_id, op, value, err_str);
    }
    //seek_place = lseek(out_file_fd, 0, SEEK_END);
    //printf("doSocketOperations - client_id=%d, lseek=%d\n", client_id, seek_place);
 print_cleanup:
    close(out_file_fd);
    sem_post(file_semaphore);
    printf("doSocketOperations - client_id=%d, released the file\n", client_id);

    printf("doSocketOperations exiting, wrote result at %p\n", write_address);
    return ret_val;
}


int main(int argc, char* argv[]) {
    int pids[NO_OF_REQUESTS];

    void* mapped_mem = mmap(NULL, MMAP_HEADER_SIZE + NO_OF_REQUESTS * MMAP_ITEM_SIZE, PROT_READ|PROT_WRITE, MAP_SHARED|MAP_ANONYMOUS, -1, 0);

    time_t start_time = time(NULL);
    char char_time[50];
    snprintf(char_time, 50, "%lld", (long long)start_time);


    char semaphore_name[50];
    snprintf(semaphore_name, 50, "/client_sem_%d", getpid());

    sem_t* file_semaphore = sem_open(semaphore_name, O_CREAT|O_EXCL|O_RDWR, S_IRWXU, 1);
    if (file_semaphore == SEM_FAILED) {
        printf("client main: could not init semaphore %s, quiting\n", semaphore_name);
        return -1;
    }
    printf("client main, created semaphore name=%s\n", semaphore_name);

    char file_name[50];
    snprintf(file_name, 50, "client_out-%s-%d.txt", char_time, getpid());
    int out_file_fd = open(file_name, O_RDWR|O_CREAT|O_EXCL, S_IRWXU);
    dprintf(out_file_fd, "client_id\t op\t value\t reply\n");
    close(out_file_fd);

    for (int i = 0; i < NO_OF_REQUESTS; i++) {
        //spawn a new child
        int pid = fork();
        if (pid > 0) {
            pids[i] = pid;
        }
        else {
            return doSocketOperations(i, mapped_mem, semaphore_name, file_name);
            //open socket, write to it and read from it, then close; write to shared memory that we have finished
        }
        
    }

    int status;
    int stopped_children = 0;
    int successfull_children = 0;
    while (stopped_children < NO_OF_REQUESTS) {
        int ret = waitpid(-1, &status, 0);
        stopped_children += 1;
        if (status == 0)
            successfull_children += 1;
        printf("Child pid:%d, status=%d, successfull=%d, total finished=%d\n", ret, status, successfull_children, stopped_children);
    }
    
    sem_unlink(semaphore_name);

    printf("client main - appending contents of the shared memory to file %s\n", file_name);
    out_file_fd = open(file_name, O_RDWR, S_IRWXU);

    lseek(out_file_fd, 0, SEEK_END);
    dprintf(out_file_fd, "\n\n\n\n\n*****************************************************************************************\n");
    dprintf(out_file_fd, "client_id\tret_val\treply_thread\top\tvalue\treply\n");
    
    //int read_offset = MMAP_HEADER_SIZE + client_id * MMAP_ITEM_SIZE;
    int ret_val = -1;
    int client_id = -1;
    int reply_thread = -1;
    int op = -1;
    double value = -1;
    double reply_val = -1;
    struct timespec start, end;
    void *read_address = mapped_mem + MMAP_HEADER_SIZE;
    
    while (read_address <  mapped_mem + MMAP_HEADER_SIZE + NO_OF_REQUESTS * MMAP_ITEM_SIZE) {
        memcpy(&ret_val, read_address, sizeof(ret_val));           read_address += sizeof(ret_val);
        memcpy(&client_id, read_address, sizeof(client_id));       read_address += sizeof(client_id);
        memcpy(&reply_thread, read_address, sizeof(reply_thread)); read_address += sizeof(reply_thread);
        memcpy(&op, read_address, sizeof(op));                     read_address += sizeof(op);
        memcpy(&value, read_address, sizeof(value));               read_address += sizeof(value);
        memcpy(&reply_val, read_address, sizeof(reply_val));       read_address += sizeof(reply_val);
        memcpy(&start, read_address, sizeof(start));               read_address += sizeof(start);
        memcpy(&end, read_address, sizeof(end));                   read_address += sizeof(end);

        dprintf(out_file_fd, "%d\t\t %d\t %d\t %d\t %g\t %g\t\n", client_id, ret_val, reply_thread, op, value, reply_val);
    }
    
    close(out_file_fd);

    printf("Program exiting\n");
    return 0;
}
