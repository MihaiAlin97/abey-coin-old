#include <stdio.h>
#include <string.h>
#include <pthread.h>
#include <io.h>
#define PORT 6069

#include "blockchainSock.h"

#pragma comment(lib,"ws2_32.lib")
#define NUM_THREADS     1



int main(int argc, char const *argv[])
{
    //compute_prime_struct *args = malloc(sizeof (*args));
    ThreadArgs *args = malloc(sizeof (*args));

    args->port = 6069;

    printf("arg%d\n",argv[1]);
    args->event = SubscriptionRequest(100,"After","Transaction",8800,"null",20);

    startConnection(args);
}