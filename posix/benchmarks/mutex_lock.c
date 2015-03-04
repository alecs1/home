#include <pthread.h>
#include <stdio.h>
#include <stdint.h>

void* lockOften(void* startArgs);
void* lockRarely(void* startArgs);

struct SStartArgs {
    int lockCount;
    int stop;
    pthread_mutex_t* mutex;
};

int main() {
    printf("starting\n");

    pthread_mutex_t mutex;
    pthread_mutex_init(&mutex, NULL);

    int task = 2;
    if (task==1) {
        //test lock unlock from a single thread
        //single-threaded program, around 300 million lock+unlock pairs per second.
        int lockCount = 300000000;
        int x = 0;
        for (int i = 0; i < lockCount; i++) {
            pthread_mutex_lock(&mutex);
            x += 1;
            pthread_mutex_unlock(&mutex);
        }
    }
    else if (task==2) {
        //test lock unlock from 2 threads, which lock rarely
        //multi-threaded program, around 500000-1000000 lock+unlock pairs per second
        pthread_t t1;
        pthread_t t2;
        pthread_t t3;
        pthread_t t4;
        pthread_attr_t attr;
        pthread_attr_init(&attr);

        struct SStartArgs args;
        args.mutex = &mutex;
        args.lockCount = 0;
        args.stop = 0;

        pthread_create(&t1, &attr, lockRarely, &args);
        pthread_create(&t2, &attr, lockOften, &args);
        pthread_create(&t3, &attr, lockOften, &args);
        pthread_create(&t4, &attr, lockOften, &args);
        //pthread_create(&t4, &attr, lockOften, &args);

        pthread_join(t1, NULL);
        pthread_join(t2, NULL);
        pthread_join(t3, NULL);
        printf("done, lockCount=%d\n", args.lockCount);
    }
    return 0;
}

void* lockRarely(void* startArgs) {
    struct SStartArgs* args = (struct SStartArgs*)startArgs;
    //int count = 1000000000;
    int period = 1000000;
    uint64_t i = 0;
    while (args->stop == 0) {
        if (i % period == 0) {
            pthread_mutex_lock(args->mutex);
            args->lockCount += 1;
            pthread_mutex_unlock(args->mutex);
        }
        i += 1;
    }
    printf("%s - done, count=%d\n", __func__, args->lockCount);
    return NULL;
}

void* lockOften(void* startArgs) {
    struct SStartArgs* args = (struct SStartArgs*)startArgs;
    uint64_t count = 10000000000;
    int period = 1000;
    for (uint64_t i = 0; i < count; i++) {
        if (i % period == 0) {
            pthread_mutex_lock(args->mutex);
            args->lockCount += 1;
            pthread_mutex_unlock(args->mutex);
        }
    }
    args->stop = 1;
    printf("%s - done, count=%d\n", __func__, args->lockCount);
    return NULL;
}



