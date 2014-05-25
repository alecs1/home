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

#include <sys/mman.h>


#include "common.h"

#define NO_OF_REQUESTS 5
#define REPLY_TIMEOUT 1 //30 seconds

//worker processes write in a shared memory area, without access control (each writes in his own cell)
//they also write over a file provided by the parent, with access controlled via a semaphore
int doSocketOperations(int client_id, void* mapped_mem, char* semaphore_name, int out_file_fd) {


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

    printf("doSocketOperations, read from /dev/urandom: op=%d, value=%g\n", op, value);
    
    //end buffer preparation


    struct addrinfo hints;
    struct addrinfo *result;
    memset(&hints, 0, sizeof(struct addrinfo));
    hints.ai_family = AF_INET6;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_protocol = 0;

    int res = getaddrinfo(NULL, PORT_CHAR, &hints, &result);
    if (res != 0) {
        printf("doSocketOperation - getaddrinfo failed\n");
        return -1;
    }

    int socket_fd;
    struct addrinfo *crt_addr_i;
    for(crt_addr_i = result; crt_addr_i != NULL; crt_addr_i = crt_addr_i->ai_next) {
        socket_fd = socket(crt_addr_i->ai_family, crt_addr_i->ai_socktype, crt_addr_i->ai_protocol);

        if (socket_fd == -1) {
            printf("doSocketOperations - socket() failed\n");
            continue;
        }
        if (connect(socket_fd, crt_addr_i->ai_addr, crt_addr_i->ai_addrlen) == 0) {
            printf("doSocketOperations - successfully connected client_id %d\n", client_id);
            break;
        }
        else {
            printf("doSocketOperations - client_id=%d, connect failed\n", client_id);
            return -1;
        }
        close(socket_fd);
    }

    if (crt_addr_i == NULL) {
        printf("doSocketOperations - client_id=%d failed to connect\n", client_id);
        freeaddrinfo(result);
        return -1;
    }
    freeaddrinfo(result);

    printf("doSocketOperations, client_id=%d, write()\n", client_id);
    int written_bytes = write(socket_fd, request_buf, IN_BUFFER_SIZE);
    printf("doSocketOperations, client_id=%d, wrote %d bytes\n", client_id, written_bytes);

    //now wait at most two minutes for a reply
    struct epoll_event event_descriptor, events[10];
    
    int epoll_fd = epoll_create1(0);
    if (epoll_fd == -1) {
        printf("doSocketOperation - epoll_create1 failed\n");
        return -1;
    }
    
    event_descriptor.events = EPOLLIN;
    event_descriptor.data.fd = socket_fd;
    if (epoll_ctl(epoll_fd, EPOLL_CTL_ADD, socket_fd, &event_descriptor) == 1) {
        printf("doSocketOperations - epoll_ctl-EPOLL_CTL_ADD failed\n");
        return -2;
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
            printf("doSocketOperations - epoll_wait failed\n");
            return -3;
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
                    printf("doSocketOperations - read returned %d\n", res);
                    return -1;
                }
                //printf("doSocketOperations - read %d bytes, remaining %d\n", read_bytes, IN_BUFFER_SIZE - read_bytes);
            }
            else {
                printf("doSocketOperations - epoll_wait woke up on unknown fd\n");
            }
        }
        time_t crt_time = time(NULL);
        elapsed_time = crt_time - start_time;        
    }
    if (read_bytes == 0)
        printf("doSocketOperations - client_id=%d, nothing received on socket, spin_count=%d\n", client_id, spin_count);
    if (read_bytes < OUT_BUFFER_SIZE) {
        printf("doSocketOperations - client_id=%d, read only %d bytes, spin_count=%d - exiting\n", client_id, read_bytes, spin_count);
        return -4;
    }
    else {
        printf("doSocketOperation - client_id=%d, read %d bytes, spin_count=%d, proceding to process reply\n", client_id, read_bytes, spin_count);
    }

    //if everything worked, we can now interpret the buffer
    double reply_val;
    int reply_thread;
    int buf_index = 0;
    memcpy(&reply_val, reply_buf+buf_index, sizeof(reply_val)); buf_index += sizeof(reply_val);
    memcpy(&reply_thread, reply_buf+buf_index, sizeof(reply_thread)); buf_index += sizeof(reply_thread);

    printf("doSocketOperations - client_id=%d, value=%g, op=%d, reply_val=%g, reply_thread=%d\n",
           client_id, value, op, reply_val, reply_thread);

    
    int write_offset = MMAP_HEADER_SIZE + client_id * MMAP_ITEM_SIZE;
    void *write_address = mapped_mem + write_offset;
    write_offset = 0;
    memset(write_address + write_offset, 0, sizeof(int)); write_offset += sizeof(int);
    memcpy(write_address + write_offset, &client_id, sizeof(client_id)); write_offset += sizeof(client_id);
    memcpy(write_address + write_offset, &reply_thread, sizeof(reply_thread)); write_offset += sizeof(reply_thread);
    memcpy(write_address + write_offset, &op, sizeof(op)); write_offset += sizeof(op);
    memcpy(write_address + write_offset, &value, sizeof(value)); write_offset += sizeof(value);
    memcpy(write_address + write_offset, &reply_val, sizeof(reply_val)); write_offset += sizeof(reply_val);
    memset(write_address + write_offset, 0, 2 * sizeof(struct timespec)); write_offset += 2*sizeof(struct timespec);

    printf("doSocketOperations - client_id=%d, waiting for out file semaphore\n", client_id);
    sem_t* file_semaphore = sem_open(semaphore_name, O_CREAT, O_RDWR, 1);
    sem_wait(file_semaphore);
    printf("doSocketOperations - client_id=%d, took the file\n", client_id);
    if (lseek(out_file_fd, SEEK_END, 0) == -1)
        printf("doSocketOperations - client_id=%d, lseek failed\n");
    //client_id, 
    dprintf(out_file_fd, "%d\t %d\t %g\t %g\t\n", client_id, op, value, reply_val)
    sem_post(file_semaphore);

    printf("doSocketOperations exiting, wrote result at %p\n", write_address);
    return 0;
}


int main(int argc, char* argv[]) {
    int pids[NO_OF_REQUESTS];

    void* mapped_mem = mmap(NULL, MMAP_HEADER_SIZE + NO_OF_REQUESTS * MMAP_ITEM_SIZE, PROT_READ|PROT_WRITE, MAP_SHARED|MAP_ANONYMOUS, -1, 0);

    time_t start_time = time(NULL);
    char char_time[50];
    snprintf(char_time, 50, "%lld", (long long)start_time);


    char semaphore_name[50];
    snprintf(semaphore_name, 50, "/client_sem_%d", getpid());

    sem_t* file_semaphore = sem_open(semaphore_name, O_CREAT, O_RDWR, 1);
    if (file_semaphore == SEM_FAILED) {
        printf("client main: could not init semaphore %s, quiting\n", semaphore_name);
        return -1;
    }
    printf("client main, created semaphore name=%s\n", semaphore_name);

    char* file_name[50];
    snprintf(file_name, 50, "client_out-%s-%d", char_time, getpid());
    int out_file_fd = open(file_name, O_RDWR, O_CREAT);

    for (int i = 0; i < NO_OF_REQUESTS; i++) {
        //spawn a new child
        int pid = fork();
        if (pid > 0) {
            pids[i] = pid;
        }
        else {
            return doSocketOperations(i, mapped_mem, semaphore_name, out_file_fd);
            //open socket, write to it and read from it, then close; write to shared memory that we have finished
        }
        
    }

    int status;
    int stopped_children = 0;
    while (stopped_children < NO_OF_REQUESTS) {
        int ret = waitpid(-1, &status, 0);
        stopped_children += 1;
        printf("Child pid:%d, status=%d, total finished=%d\n", ret, status, stopped_children);
    }
    
    

    printf("Program exiting\n");
    return 0;
}
