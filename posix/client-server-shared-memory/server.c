#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <sys/epoll.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <sys/types.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <errno.h>
#include <string.h>

#define MAX_QUEUE_SIZE 1000
#define THREAD_COUNT 100
#define PORT_CHAR "1666"


//two queues, one of requests, one of replies. 100 worker threads browse like crazy and fill in replies
//one read thread fills in requests, one write thread writes them
//inefficient, but triggers all needed functionality

struct connection {
    int socket_fd;
    struct sockaddr peer_info;
};

struct queue_element {
    int request_type; //0 - sqrt(x), 1 - pow(x, 2), 2 - pow(x, 3), 3 - sin(x), 4 - cos(x), 5 - hang-up
    double request;
    double result;
    int fd;
    struct sockaddr peer_info; //is this needed?
    //something about the connection
};

struct thread_args {
    struct queue_element* new_conn_queue;
    pthread_mutex_t* new_conn_mutex;
    int *s_new_conn_queue;
    struct queue_element* requests_queue;
    pthread_mutex_t* requests_mutex;
    struct queue_element* replies_queue;
    pthread_mutex_t* replies_mutex;
    int queue_size;
    int signals_fd; //main program might signal us to stop
    int new_conn_fd;
    int thread_id;
};

void* workerThread(void* start_args) {
    struct thread_args* args = (struct thread_args*)start_args;
    printf("Started thread %d\n", args->thread_id);
    fflush (stdout);
    free(start_args);
    return 0;
}

void* readerThread(void* start_args) {
    //also listen on a fd on which connectionThread might write
    
    return 0;
}

void* connectionThread(void* start_args) {
    struct thread_args* args = (struct thread_args*)start_args;

    int listen_socket;
    //get local addres
    struct addrinfo hints;
    struct addrinfo *result;
    memset(&hints, 0, sizeof(struct addrinfo));
    hints.ai_family = AF_INET6;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_flags = AI_PASSIVE;
    hints.ai_protocol = 0;

    int ret_val = getaddrinfo(NULL, PORT_CHAR, &hints, &result);

    if (ret_val != 0) {
        printf("getaddrinfo failed: %s\n", gai_strerror(ret_val));
        return NULL;
    }

    struct addrinfo* crt_addr_i;
    for(crt_addr_i = result; crt_addr_i != NULL; crt_addr_i = crt_addr_i->ai_next) {

        //try to print
        char printable_addr[INET6_ADDRSTRLEN];
        struct sockaddr *sockaddr_generic = crt_addr_i->ai_addr;
        struct sockaddr_in6 *sock_addr_in6 = (struct sockaddr_in6*)sockaddr_generic;
        struct in6_addr inet6_addr = sock_addr_in6->sin6_addr;
        printf("sock_addr_in6: sin6_family:%d, sin6_port:%d, sin6_flowinfo:%d, sin6_scope_id:%d\n", sock_addr_in6->sin6_family, sock_addr_in6->sin6_port, sock_addr_in6->sin6_flowinfo, sock_addr_in6->sin6_scope_id);
        printf("crt_addr_i, ai_family:%d, ai_socktype:%d, ai_protocol:%d, ai_canonname=%s\n", crt_addr_i->ai_family, crt_addr_i->ai_socktype, crt_addr_i->ai_protocol, crt_addr_i->ai_canonname);
        inet_ntop(crt_addr_i->ai_family, &inet6_addr, printable_addr, INET6_ADDRSTRLEN);
        printf("trying to bind to %s\n", printable_addr);
        //end printing


        listen_socket = socket(crt_addr_i->ai_family, crt_addr_i->ai_socktype, crt_addr_i->ai_protocol);
        if (listen_socket == -1) {
            printf("Could not create socket\n");
            continue;
        }
        else
            printf("Created socket\n");

        if (bind(listen_socket, crt_addr_i->ai_addr, crt_addr_i->ai_addrlen) == 0) {
            printf("Successfully bound to %s\n", printable_addr);
            break;
        }
        else
            printf("Failed to bind to %s\n", printable_addr);
        
        close(listen_socket);
    }
    freeaddrinfo(result);

    listen(listen_socket, 500);


    
    int epoll_fd;
    struct epoll_event fd_event_descriptor, fd_event_descriptor2, events[100];
    fd_event_descriptor.events = EPOLLIN;
    fd_event_descriptor.data.fd = listen_socket;
    fd_event_descriptor2.events = EPOLLIN;
    fd_event_descriptor2.data.fd = args->signals_fd;
    epoll_fd = epoll_create1(0);
    if (epoll_fd == 1) {
        int error_val = errno;
        printf("Failed epoll_create, errno=%d\n", error_val);
        return 0;
    }
    epoll_ctl(epoll_fd, EPOLL_CTL_ADD, listen_socket, &fd_event_descriptor);
    epoll_ctl(epoll_fd, EPOLL_CTL_ADD, args->signals_fd, &fd_event_descriptor2);
    

    for(;;) {

        int nfds = epoll_wait(epoll_fd, events, 100, -1);
        if (nfds == -1) {
            printf("Error on epoll_wait()\n");
            return NULL;
        }

        for (int i=0; i < nfds; i++) {
            if (events[i].data.fd == listen_socket) {
                struct sockaddr peer_addr;
                unsigned int peer_addr_len = sizeof(struct sockaddr);
                int data_socket = accept(listen_socket, &peer_addr, &peer_addr_len);
                if (data_socket == -1) {
                    printf("Error at connecting to peer\n");
                }
                if (*(args->s_new_conn_queue) < MAX_QUEUE_SIZE) {
                    pthread_mutex_lock(args->new_conn_mutex);
                    args->new_conn_queue[*(args->s_new_conn_queue)].fd = data_socket;
                    args->new_conn_queue[*(args->s_new_conn_queue)].peer_info = peer_addr;
                    *(args->s_new_conn_queue) += 1;
                    pthread_mutex_unlock(args->new_conn_mutex);
                    write(args->new_conn_fd, "1", 1);
                }
                //do socket stuff
            }
            else if (events[i].data.fd == args->signals_fd) {
                printf("Exiting cleanly on a socket signal\n");
                return NULL;
            }
            else {
                printf("Error, received an event on a descriptor unknown to us:%d\n", events[i].data.fd);
            }
        }

    }

    free(start_args);

    close(listen_socket);


    return 0;
}

void* writerThread(void* start_arg) {

    return 0;
}

int main(int argc, char* argv[]) {
    struct queue_element new_conn_queue[MAX_QUEUE_SIZE];
    pthread_mutex_t new_conn_mutex;
    int s_new_conn_queue = 0; //size of queue

    struct queue_element requests_queue[MAX_QUEUE_SIZE];
    pthread_mutex_t requests_mutex;

    struct queue_element replies_queue[MAX_QUEUE_SIZE];
    pthread_mutex_t replies_mutex;
    
    pthread_t threads[THREAD_COUNT];

    struct thread_args start_args = {
        .new_conn_queue = new_conn_queue,
        .new_conn_mutex = &new_conn_mutex,
        .s_new_conn_queue = &s_new_conn_queue,
        .requests_queue = requests_queue,
        .requests_mutex = &requests_mutex,
        .replies_queue = replies_queue,
        .replies_mutex = &replies_mutex,
        .queue_size = MAX_QUEUE_SIZE,
        .thread_id = 0
    };
    pthread_attr_t attr;
    pthread_attr_init(&attr);
    for(int i = 0; i < THREAD_COUNT; i++) {
        struct thread_args *actual_start_args = malloc(sizeof(struct thread_args));
        *actual_start_args = start_args;
        actual_start_args->thread_id = i;
        printf("pthread_create for thread %d\n", actual_start_args->thread_id);
        pthread_create(threads + i, &attr, workerThread, actual_start_args);
    }

    pthread_t connection_thread;
    struct thread_args *connection_thread_args = malloc(sizeof(struct thread_args));
    *connection_thread_args = start_args;
    pthread_create(&connection_thread, &attr, connectionThread, connection_thread_args);

    pthread_t reader_thread;
    struct thread_args *reader_thread_args = malloc(sizeof(struct thread_args));
    *reader_thread_args = start_args;
    pthread_create(&reader_thread, &attr, readerThread, reader_thread_args);

    for(int i = 0; i < THREAD_COUNT; i++) {
        pthread_join(threads[i], NULL);
    }
    pthread_join(connection_thread, NULL);
    pthread_join(reader_thread, NULL);
    
    printf("Program will exit now\n");
    return 0;
}



