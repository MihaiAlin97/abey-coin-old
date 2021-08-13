//
// Created by Admin on 5/24/2021.
//

#ifndef BLOCKCHAINOBSERVER_ABEYJSON_H
#define BLOCKCHAINOBSERVER_ABEYJSON_H

#include "parson.h"


#define INVALID_JSON_STRING -1
#define INVALID_REGISTRATION_RESPONSE -2
#define VALID_REGISTRATION_RESPONSE -3

#define REGISTRATION_REJECTED -4
#define REGISTRATION_ACCEPTED -5

#define INVALID_REGISTRATION_RESULT -6

int validateRegistrationResponse(char * regResp);
int validateSubscriptionResponse(char * subResp);
int validateNotificationRequest(char * notResp);

char* RegistrationResponseGetResult(char * regResp);
char* RegistrationResponseGetPort(char * regResp);
char* RegistrationResponseGetIp(char * regResp);

int checkRegistrationResponse(char  *resultStr);



#endif //BLOCKCHAINOBSERVER_ABEYJSON_H
