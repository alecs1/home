//#define _POSIX_C_SOURCE >= 199309L
#define _GNU_SOURCE

#include <sys/ioctl.h>
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

#include <math.h>

#include <unistd.h>
#include <fcntl.h>
#include <time.h>

#include "common.h"

#define MAX_QUEUE_SIZE 1000
#define THREAD_COUNT 5
#define EPOLL_TIMEOUT 10000
#define WORKER_TIMEOUT 5
#define WORKER_EXIT_CODE 666


/*
  Test usage of many POSIX/Linux basic functionality: pipes, file descriptors, epoll, sockets, threads.
  To test in the next program: shared memory, further thread functionality (wake, conditions etc), epoll performance.
*/

/*
  Three queues, one of new connections, one of requests, one of replies. 100 worker threads browse like crazy and fill in replies
  One read thread fills in requests, one write thread writes them
  Inefficient, but triggers all needed functionality
*/

struct connection {
    int socket_fd;
    struct sockaddr peer_info;
};

struct queue_element {
    int request_type; //0 - sqrt(x), 1 - pow(x, 2), 2 - pow(x, 3), 3 - sin(x), 4 - cos(x), 5 - hang-up
    char buffer[IN_BUFFER_SIZE];
    char reply_buffer[OUT_BUFFER_SIZE];
    double request;
    double result;
    int client_id;
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
    int *s_requests_queue;
    struct queue_element* replies_queue;
    pthread_mutex_t* replies_mutex;
    int *s_replies_queue;
    int signals_fd; //main program might signal us to stop
    int new_conn_fd[2];
    int new_reply_fd[2];
    pthread_cond_t *worker_cond;
    pthread_mutex_t *worker_cond_mutex;
    int *worker_thread_stop; //tell workers to stop
    int thread_id;
};

int processRequest(struct queue_element *request, int worker_id) {
    int buffer_index = 0;
    memcpy(&request->request_type, request->buffer + buffer_index, sizeof(int)); buffer_index += sizeof(int);
    memcpy(&request->request, request->buffer + buffer_index, sizeof(double)); buffer_index += sizeof(double);
    memcpy(&request->client_id, request->buffer + buffer_index, sizeof(int)); buffer_index += sizeof(int);

    switch(request->request_type) {
        case OP_SQRT:
            request->result = sqrt(request->request);
            break;
        case OP_POW2:
            request->result = pow(request->request, 2);
            break;
        case OP_POW3:
            request->result = pow(request->request, 2);
            break;
        case OP_SIN:
            request->result = sin(request->request);
            break;
        case OP_COS:
            request->result = cos(request->request);
            break;
        case OP_HUP:
            //nothing to do in this case
            printf("processRequest, client %d called us just to hang up\n", request->client_id);
            break;
        default:
            printf("processRequest, client %d sent uknown request type %d\n", request->client_id, request->request_type);
            break;
    }

   printf("Worker=%d, op=%d, request=%g, result=%g\n", worker_id, request->request_type, request->request, request->result);


    buffer_index = 0;
    memcpy(request->reply_buffer + buffer_index, &request->result, sizeof(double)); buffer_index += sizeof(double);
    memcpy(request->reply_buffer + buffer_index, &worker_id, sizeof(int)); buffer_index += sizeof(int);

    return 0;
}

int delQueueElement(struct queue_element* queue, int index, int *size) {
    if (*size <= 0) {
        printf("delQueueElement - size <= 0\n");
        return -1;
    }
    if (index >= *size) {
        printf("qelQueueElement - index >= size\n");
        return -1;
    }
        
    for(int i = index; i < *size; i++)
        queue[i] = queue[i+1];
    *size -= 1;
    return 0;
}

void* workerThread(void* start_args) {
    struct thread_args* args = (struct thread_args*)start_args;
    printf("workerThread - %d, started thread\n", args->thread_id);
    
    struct timespec ts;

    while(1) {
        if (*args->s_requests_queue == 0) {
            clock_gettime(CLOCK_REALTIME, &ts);
            ts.tv_sec += 5;
            //printf("workerThread, thread_id=%d, lock worker_cond_mutex\n", args->thread_id);
            pthread_mutex_lock(args->worker_cond_mutex);
            //printf("workerThread, thread_id=%d, locked worker_cond_mutex\n", args->thread_id);
            pthread_cond_timedwait(args->worker_cond, args->worker_cond_mutex, &ts);
            pthread_mutex_unlock(args->worker_cond_mutex);
        }


        printf("workerThread - %d, wake-up with s_requests_queue~%d, s_replies_queue~%d\n", args->thread_id, *args->s_requests_queue, *args->s_replies_queue);

        if (*args->worker_thread_stop == WORKER_EXIT_CODE) {
            goto cleanup;
        }

        if (*args->s_requests_queue > 0) {
            printf("workerThread - %d, s_request_queue=%d, s_replies_queue~%d\n", args->thread_id, *args->s_requests_queue, *args->s_replies_queue);
            pthread_mutex_lock(args->requests_mutex);
            if (*args->s_requests_queue <= 0) {
                pthread_mutex_unlock(args->requests_mutex);
                continue;
            }
            struct queue_element request = args->requests_queue[0];
            delQueueElement(args->requests_queue, 0, args->s_requests_queue);
            pthread_mutex_unlock(args->requests_mutex);
            
            processRequest(&request, args->thread_id);

            pthread_mutex_lock(args->replies_mutex);
            if (*args->s_replies_queue < MAX_QUEUE_SIZE) {
                args->replies_queue[*args->s_replies_queue] = request;
                *args->s_replies_queue += 1;
                printf("workerThread - %d, s_requests_queue~%d, s_replies_queue=%d\n", args->thread_id, *args->s_requests_queue, *args->s_replies_queue);
                write(args->new_reply_fd[1], "1", 1);
            }
            else {
                printf("workerThread - %d, replies queue is full, loosing reply and hanging up\n", args->thread_id);
            }
            pthread_mutex_unlock(args->replies_mutex);
        }
        
    }


 cleanup:
    printf("workerThread - %d exiting\n", args->thread_id);
    fflush (stdout);
    free(start_args);
    return 0;
 }


void* readerThread(void* start_args) {
    //also listen on a fd on which connectionThread might write
    struct thread_args *args = (struct thread_args*)start_args;
    
    int epoll_fd;
    struct epoll_event events[10], new_conn_event_descriptor, signals_event_descriptor;
    new_conn_event_descriptor.events = EPOLLIN;
    new_conn_event_descriptor.data.fd = args->new_conn_fd[0];
    signals_event_descriptor.events = EPOLLIN;
    signals_event_descriptor.data.fd = args->signals_fd;

    epoll_fd = epoll_create1(0);
    if (epoll_fd == -1) {
        printf("readerThread, error at epoll_create1\n");
        return NULL;
    }

    epoll_ctl(epoll_fd, EPOLL_CTL_ADD, args->new_conn_fd[0], &new_conn_event_descriptor);
    epoll_ctl(epoll_fd, EPOLL_CTL_ADD, args->signals_fd, &signals_event_descriptor);

    char null_buf[1];

    while(1) {
        int nfds = epoll_wait(epoll_fd, events, 100, EPOLL_TIMEOUT);
        if (nfds == -1) {
            printf("readerThread - error on epoll_wait\n");
            return NULL;
        }
        else if (nfds == 0)
            printf("readerThread - wake on timeout\n");
        else
            printf("readerThread - wake with %d fds\n", nfds);
        
        for (int i = 0; i < nfds; i++) {
            if (events[i].data.fd == args->new_conn_fd[0]) {
                printf("readerThread - epoll wake if=new_conn_fd[0]=%d\n", events[i].data.fd);
                read(events[i].data.fd, null_buf, 1);
                pthread_mutex_lock(args->new_conn_mutex);
                //do stuff here
                while (*args->s_new_conn_queue > 0) {
                    printf("readerThread, s_new_conn_queue=%d\n", *args->s_new_conn_queue);
                    int len = 0;
                    char buffer[IN_BUFFER_SIZE];
                    //spin one second, maybe the write will come
                    struct timespec ts_start;
                    clock_gettime(CLOCK_REALTIME, &ts_start);
                    struct timespec ts_current = ts_start;
                    int spin_count = 0;
                    while (ts_current.tv_sec - ts_start.tv_sec < 2) {
                        ioctl(args->new_conn_queue[0].fd, FIONREAD, &len);
                        if (len == IN_BUFFER_SIZE)
                            break;
                        clock_gettime(CLOCK_REALTIME, &ts_current);
                        spin_count += 1;
                    }
                    printf("readerThread - socket read, %d bytes on socket, spin_count=%d\n", len, spin_count);

                    if(len == IN_BUFFER_SIZE) {
                        len = read(args->new_conn_queue[0].fd, buffer, IN_BUFFER_SIZE);
                        printf("readerThread - read %d bytes from socket\n", len);
                        pthread_mutex_lock(args->requests_mutex);
                        if (*args->s_requests_queue < MAX_QUEUE_SIZE) {
                            args->requests_queue[*args->s_requests_queue].fd = args->new_conn_queue[0].fd;
                            args->requests_queue[*args->s_requests_queue].peer_info =
                                args->new_conn_queue[0].peer_info;
                            memcpy(args->requests_queue[*args->s_requests_queue].buffer, buffer, IN_BUFFER_SIZE);
                            *args->s_requests_queue += 1;

                            pthread_cond_signal(args->worker_cond);
                            
                            printf("readerThread - read request, s_requests_queue=%d\n", *args->s_requests_queue);
                            epoll_ctl(epoll_fd, EPOLL_CTL_DEL, args->new_conn_queue[0].fd, NULL);
                            delQueueElement(args->new_conn_queue, 0, args->s_new_conn_queue);
                        }
                        else {
                            printf("readerThread - requests queue full, dropping new connection\n");
                            epoll_ctl(epoll_fd, EPOLL_CTL_DEL, args->new_conn_queue[0].fd, NULL);
                            close(args->new_conn_queue[0].fd);
                            delQueueElement(args->new_conn_queue, 0, args->s_new_conn_queue);
                        }
                        pthread_mutex_unlock(args->requests_mutex);
                    }
                    else {
                        printf("readerThread - peer only wrote %d bytes in time, hanging up\n", len);
                        epoll_ctl(epoll_fd, EPOLL_CTL_DEL, args->new_conn_queue[0].fd, NULL);
                        close(args->new_conn_queue[0].fd);
                        delQueueElement(args->new_conn_queue, 0, args->s_new_conn_queue);
                    }
                }
                pthread_mutex_unlock(args->new_conn_mutex);
            }
            else if (events[i].data.fd == args->signals_fd) {
                printf("readerThread - exiting cleanly on signals_fd\n");
                return NULL;
            }
            else {
                printf("readerThread - epoll_wait wake on unknown fd %d\n", events[i].data.fd);
            }
        }
        
    }

    
    
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

        //apparently, without calling bind() you would listen on some random port number (which could be found later with getsockname(2)
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
    if (epoll_fd == -1) {
        int error_val = errno;
        printf("Failed epoll_create, errno=%d\n", error_val);
        return 0;
    }
    epoll_ctl(epoll_fd, EPOLL_CTL_ADD, listen_socket, &fd_event_descriptor);
    epoll_ctl(epoll_fd, EPOLL_CTL_ADD, args->signals_fd, &fd_event_descriptor2);
    

    for(;;) {

        int nfds = epoll_wait(epoll_fd, events, 100, EPOLL_TIMEOUT);
        if (nfds == -1) {
            printf("connectionThread error on epoll_wait() - quiting\n");
            return NULL;
        }
        else if (nfds == 0)
            printf("connectionThread - wake on timeout\n");
        else
            printf("connectionThread - wake with %d fds\n", nfds);

        for (int i=0; i < nfds; i++) {
            if (events[i].data.fd == listen_socket) {
                struct sockaddr peer_addr;
                unsigned int peer_addr_len = sizeof(struct sockaddr);
                int data_socket = accept(listen_socket, &peer_addr, &peer_addr_len);
                if (data_socket == -1) {
                    printf("Error at connecting to peer\n");
                }
                if (*(args->s_new_conn_queue) < MAX_QUEUE_SIZE) {
                    printf("connectionThread - adding 1 to new_conn_queue, count=%d\n", *args->s_new_conn_queue);
                    pthread_mutex_lock(args->new_conn_mutex);
                    args->new_conn_queue[*(args->s_new_conn_queue)].fd = data_socket;
                    args->new_conn_queue[*(args->s_new_conn_queue)].peer_info = peer_addr;
                    *(args->s_new_conn_queue) += 1;
                    pthread_mutex_unlock(args->new_conn_mutex);
                    write(args->new_conn_fd[1], "1", 1);
                }
                else {
                    printf("connectionThread - new_conn_queue full, dropping\n");
                }
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

void* writerThread(void* start_args) {
    struct thread_args *args = (struct thread_args *) start_args;

    struct epoll_event events[10], new_reply_event_desc, signals_event_desc;

    new_reply_event_desc.data.fd = args->new_reply_fd[0];
    new_reply_event_desc.events = EPOLLIN;
    signals_event_desc.data.fd = args->signals_fd;
    signals_event_desc.events = EPOLLIN;

    int epoll_fd = epoll_create1(0);
    if (epoll_fd < 0) {
        printf("writerThread - epoll_create1 error\n");
    }

    int res = epoll_ctl(epoll_fd, EPOLL_CTL_ADD, args->new_reply_fd[0], &new_reply_event_desc);
    if (res != 0) {
        printf("writerThread - error on epoll_ctl\n");
        return NULL;
    }

    res = epoll_ctl(epoll_fd, EPOLL_CTL_ADD, args->signals_fd, &signals_event_desc);
    if (res != 0) {
        printf("writerThread - error on epoll_ctl, quitting\n");
        return NULL;
    }

    char null_buf[1];

    while(1) {
        //printf("writerThread - loop\n");
        int nfds = epoll_wait(epoll_fd, events, 10, EPOLL_TIMEOUT);
        if (nfds == -1) {
            printf("writerThread - error on epoll_wait, quitting\n");
            return NULL;
        }
        for (int i = 0; i < nfds; i++) {
            if (events[i].data.fd == args->new_reply_fd[0]) {
                read(args->new_reply_fd[0], null_buf, 1);
                printf("writerThread - new reply, s_requests_queue~%d, s_replies_queue~%d\n", *args->s_requests_queue, *args->s_replies_queue);
                if (*args->s_replies_queue > 0) {
                    pthread_mutex_lock(args->replies_mutex);
                    while (*args->s_replies_queue > 0) {
                        write(args->replies_queue[0].fd, args->replies_queue[0].reply_buffer, OUT_BUFFER_SIZE);
                        close(args->replies_queue[0].fd);
                        delQueueElement(args->replies_queue, 0, args->s_replies_queue);
                    }
                    pthread_mutex_unlock(args->replies_mutex);
                }
            }
            else if (events[i].data.fd == args->signals_fd) {
                printf("writerThread - event on signals_fd, assuming it's time to exit\n");
                return NULL;
            }
            else {
                printf("writerThread - event on fd unknown to us! \n");
            }
        }

    }
    return NULL;
}

int main(int argc, char* argv[]) {
    struct queue_element new_conn_queue[MAX_QUEUE_SIZE];
    pthread_mutex_t new_conn_mutex;
    pthread_mutex_init(&new_conn_mutex, NULL);
    int s_new_conn_queue = 0; //size of queue

    struct queue_element requests_queue[MAX_QUEUE_SIZE];
    pthread_mutex_t requests_mutex;
    pthread_mutex_init(&requests_mutex, NULL);
    int s_requests_queue = 0;

    struct queue_element replies_queue[MAX_QUEUE_SIZE];
    pthread_mutex_t replies_mutex;
    pthread_mutex_init(&replies_mutex, NULL);
    int s_replies_queue;
    
    pthread_t threads[THREAD_COUNT];

    int new_conn_pipe[2];
    int ret = pipe(new_conn_pipe);
    ret += 0; //avoid compile warning

    int new_reply_pipe[2];
    ret = pipe(new_reply_pipe);
    
    pthread_cond_t worker_cond;
    pthread_cond_init(&worker_cond, NULL);
    pthread_mutex_t worker_cond_mutex;
    pthread_mutex_init(&worker_cond_mutex, NULL);

    int worker_thread_stop = 0;
    struct thread_args start_args = {
        .new_conn_queue = new_conn_queue,
        .new_conn_mutex = &new_conn_mutex,
        .s_new_conn_queue = &s_new_conn_queue,
        .requests_queue = requests_queue,
        .requests_mutex = &requests_mutex,
        .s_requests_queue = &s_requests_queue,
        .replies_queue = replies_queue,
        .replies_mutex = &replies_mutex,
        .s_replies_queue = &s_replies_queue,
        .new_conn_fd[0] = new_conn_pipe[0],
        .new_conn_fd[1] = new_conn_pipe[1],
        .new_reply_fd[0] = new_reply_pipe[0],
        .new_reply_fd[1] = new_reply_pipe[1],
        .worker_cond = &worker_cond,
        .worker_cond_mutex = &worker_cond_mutex,
        .worker_thread_stop = &worker_thread_stop,
        .thread_id = 0,
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

    pthread_t writer_thread;
    struct thread_args *writer_thread_args = malloc(sizeof(struct thread_args));
    *writer_thread_args = start_args;
    pthread_create(&writer_thread, &attr, writerThread, writer_thread_args);

    for(int i = 0; i < THREAD_COUNT; i++) {
        pthread_join(threads[i], NULL);
    }
    pthread_join(connection_thread, NULL);
    pthread_join(reader_thread, NULL);
    
    printf("Program will exit now\n");
    return 0;
}



