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



#include "common.h"

#define NO_OF_REQUESTS 1
#define REPLY_TIMEOUT 1 //30 seconds

int doSocketOperations(int client_id) {


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
    while ((elapsed_time <= REPLY_TIMEOUT) && (read_bytes < IN_BUFFER_SIZE)) {
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
                printf("doSocketOperations - read %d bytes, remaining %d\n", read_bytes, IN_BUFFER_SIZE - read_bytes);
            }
            else {
                printf("doSocketOperations - epoll_wait woke up on unknown fd\n");
            }
        }
        time_t crt_time = time(NULL);
        elapsed_time = crt_time - start_time;        
    }
    if (read_bytes == 0)
        printf("doSocketOperations - client_id=%d, nothing received on socket\n", client_id);
    if (read_bytes < OUT_BUFFER_SIZE) {
        printf("doSocketOperations - client_id=%d, read only %d bytes\n", client_id, read_bytes);
        printf("doSocketOperations - client_id=%d exiting\n", client_id);
        return -4;
    }

    //if everything worked, we can now interpret the buffer



    return -1;
}


int main(int argc, char* argv[]) {
    int pids[NO_OF_REQUESTS];


    for (int i = 0; i < NO_OF_REQUESTS; i++) {
        //spawn a new child
        int pid = fork();
        if (pid > 0) {
            pids[i] = pid;
        }
        else {
            doSocketOperations(i);
            //open socket, write to it and read from it, then close; write to shared memory that we have finished
            return -1;
        }
        
    }

    int status;
    for (int i = 0; i < NO_OF_REQUESTS; i++) {
        int ret = waitpid(pids[i], &status, WUNTRACED|WEXITED|WCONTINUED);
        printf("Child:%d, pid:%d, ret=%d\n", i, pids[i], ret);
    }
    printf("Program exiting");
    return 0;
}
