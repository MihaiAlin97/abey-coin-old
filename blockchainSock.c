//
// Created by Admin on 2/9/2021.
//


#include "jsonOut.h"


#define PUBLISHER_PORT 6069
#define NEW_PORT 6071

#include "blockchainSock.h"

#pragma comment(lib,"ws2_32.lib")
#define NUM_THREADS     1

void sleepMS(u_int miliseconds){

#ifdef _WIN32
    printf("windows\n");
    Sleep(miliseconds);
#else
    printf('unix\n');

    sleep(miliseconds/1000);
#endif

}

int sockInit(void)
{
#ifdef _WIN32
    WSADATA wsa_data;
    return WSAStartup(MAKEWORD(1,1), &wsa_data);
#else
    return 0;
#endif
}



int sockConnect(u_short PORT,char *IP,struct sockaddr_in sockInfo,int *sock)
{

    if ((*sock = socket(AF_INET, SOCK_STREAM, 0)) < 0)
    {
        printf("\n Socket creation error \n");
        return SOCKET_CREATION_ERROR;
    }

    sockInfo.sin_family = AF_INET;
    sockInfo.sin_port = htons(PORT);

    // Convert IPv4 and IPv6 addresses from text to binary form
    if(inet_pton(AF_INET,IP, &sockInfo.sin_addr)<=0)
    {
        printf("\nInvalid address/ Address not supported \n");
        return SOCKET_ADDRESS_INVALID;
    }

    if (connect(*sock, (struct sockaddr *)&sockInfo, sizeof(sockInfo)) < 0)
    {
        printf("\nConnection Failed \n");
        return SOCKET_CONNECTION_FAILED;
    }

    return SOCKET_CONNECTION_SUCCESS;

}

void tryConnectSocket(u_short PORT,char *IP,struct sockaddr_in sockInfo,int *sock)
{
    sockInit();
    while ( sockConnect(PORT,IP,sockInfo,sock) != SOCKET_CONNECTION_SUCCESS ) {
        printf("Failed connection \n");
        sockClose(*sock);
        sockQuit();
        sleepMS(1000);
        sockInit();
    }
    printf ("Connection established \n");
}

int sockQuit(void)
{
#ifdef _WIN32
    return WSACleanup();
#else
    return 0;
#endif
}

int sockClose(SOCKET sock)
{

    int status = 0;

#ifdef _WIN32
    status = shutdown(sock, SD_BOTH);
    if (status == 0) { status = closesocket(sock); }
#else
    status = shutdown(sock, SHUT_RDWR);
    if (status == 0) { status = close(sock); }
#endif

    return status;

}


void *handleEvent(void *args)
{


    ThreadArgs *actualArgs = args;



    int sock = 0, valread;
    u_short port = actualArgs->port;
    char *IP = "127.0.0.1";

    struct sockaddr_in subscriberAddress;

    char *RegistrationMessage,*SubscriptionMessage;
    char subscriberReply[2000];
    char messageBuffer[1024] = {0};
    int replySize;



    boolean RegistrationComplete;
    RegistrationComplete = FALSE;


    time_t secondsBegin,secondEnd;


    printf("sock before Registration : %d\n",sock);
    tryConnectSocket(port,IP,subscriberAddress,&sock);

    printf("sock after Registration : %d\r\n",sock);

    while(RegistrationComplete == FALSE){

        RegistrationMessage = RegistrationRequest(23,"Partial");



        printf("size of hello message %d\n",strlen(RegistrationMessage));
        send(sock , RegistrationMessage , strlen(RegistrationMessage), 0 );
        printf("Hello message sent\n");

        sleepMS(1000);


        if((replySize = recv(sock , subscriberReply , 2000 , 0)) == SOCKET_ERROR)
        {
            puts("recv failed");
        }
        else {


            //make sure you add NULL after replySize bytes in subscriberReply
            subscriberReply[replySize] = '\0';
            puts(subscriberReply);

            printf("ipStr: %s\n",RegistrationResponseGetIp(subscriberReply));
            printf("portStr: %s\n",RegistrationResponseGetPort(subscriberReply));


            if(checkRegistrationResponse(RegistrationResponseGetResult(subscriberReply)) == REGISTRATION_ACCEPTED){

                IP = RegistrationResponseGetIp(subscriberReply);

                port = atoi(RegistrationResponseGetPort(subscriberReply));

                printf("ip: %s\n",IP);
                printf("port: %d\n",port);
                printf( "port: %hu\n",port);

                RegistrationComplete = TRUE;
            }


        }
        sleepMS(1000);
    }

    sleepMS(3000);

    sockClose(sock);
    sockQuit();

    sock = 0;
    //registration phase
    boolean EventFulfilled = FALSE;
    boolean EventRegistered = FALSE;


    sleepMS(2000);

    printf("sock before Subscription : %d\n",sock);

    tryConnectSocket(port,IP,subscriberAddress,&sock);

    printf("sock after Subscription : %d\n",sock);


    while(EventFulfilled == FALSE){


        if ( EventRegistered == FALSE){

            printf("Event not registered\n");

            SubscriptionMessage = actualArgs->event;


            send(sock , SubscriptionMessage , strlen(SubscriptionMessage), 0 );

            if((replySize = recv(sock , subscriberReply , 2000 , 0)) == SOCKET_ERROR)
            {
                puts("recv failed");
            }
            else {



                printf("size of subscriptionReply message %d\n",strlen(subscriberReply));

                //make sure you add NULL after replySize bytes in subscriberReply
                subscriberReply[replySize] = '\0';


                puts(subscriberReply);
                EventRegistered = TRUE;
            }



        } else {

            printf("Event registered\n");

            char * WaitingMessage = WaitMes;

            printf("new size of subscription message %d\n",strlen(WaitingMessage));

            send(sock , WaitingMessage , strlen(WaitingMessage), 0 );

            printf("pasdsed\n");
            if((replySize = recv(sock , subscriberReply , 2000 , 0)) == SOCKET_ERROR)
            {
                puts("recv failed");
            }
            else {


                if(strcmp(WaitingMessage,"Done")==0)
                {
                    printf("Event fired!\r\n");
                    EventFulfilled = TRUE;
                }


                printf("size of subscriptionReply message %d\n",strlen(subscriberReply));

                //make sure you add NULL after replySize bytes in subscriberReply
                subscriberReply[replySize] = '\0';


                puts(subscriberReply);
            }

        }

        printf("Done one loop\n");

        sleepMS(1000);

    }

    sockClose(sock);
    sockQuit();





    printf("Exit\n");

    pthread_exit(NULL);
}




int startConnection(void *args){

    //pthread_mutex_init(args.lock, NULL);

    pthread_create(&eventThread, NULL, handleEvent, args);

    pthread_exit(NULL);
}
