//
// Created by Admin on 2/9/2021.
//

#ifndef BLOCKCHAINOBSERVER_BLOCKCHAINSOCK_H
#define BLOCKCHAINOBSERVER_BLOCKCHAINSOCK_H

#ifdef _WIN32
    #ifndef _WIN32_WINNT
        #define _WIN32_WINNT 0x0501  /* Windows XP. */
    #endif

#include <winsock2.h>
#include <Ws2tcpip.h>
#else
/* Assume that any non-Windows platform uses POSIX-style sockets instead. */
  #include <sys/socket.h>
  #include <arpa/inet.h>
  #include <netdb.h>  /* Needed for getaddrinfo() and freeaddrinfo() */
  #include <unistd.h> /* Needed for close() */
#endif

#include <stdio.h>
#include <string.h>
#include <pthread.h>
#include <io.h>

#include "jsonOut.h"
#include "ABEYJson.h"

#ifdef _WIN32
#include <windows.h>
#else
#include "unistd.h"
#endif


#define SOCKET_CONNECTION_SUCCESS 0
#define SOCKET_CONNECTION_FAILED -1
#define SOCKET_ADDRESS_INVALID -2
#define SOCKET_CREATION_ERROR -3

typedef struct {
    //pthread_mutex_t lock;
    long threadId;
    int pingBool;
    char *event;
    u_short port;
}ThreadArgs;


pthread_t eventThread;

int sockInit(void);
int sockQuit(void);
int sockClose(SOCKET sock);
int startConnection(void *args);
void *handleEvent(void *args);


#endif //BLOCKCHAINOBSERVER_BLOCKCHAINSOCK_H
