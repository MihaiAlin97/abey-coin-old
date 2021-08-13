//
// Created by Admin on 2/9/2021.
//

#ifndef BLOCKCHAINOBSERVER_JSONIN_H
#define BLOCKCHAINOBSERVER_JSONIN_H

#define JSMN_HEADER
#include "json.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#define INVALID_JSON_STRING -1
#define INVALID_REGISTRATION_RESPONSE -2
#define VALID_REGISTRATION_RESPONSE -3

#define REGISTRATION_REJECTED -4
#define REGISTRATION_ACCEPTED -5

#define INVALID_REGISTRATION_RESULT -6




/*
 * "_comment": "JSON for negative response to registration",
  "registrationNegativeResponse" : {
    "action": "Registration",
    "actionType" : "Response",
    "actionInfo": {
      "result" : "Rejected",
      "port" : "null",
      "ip" : "null",
    },
    "id" : 1
 */

typedef struct{
    char * action;
    char * actionInfo;
    char * result;
    char * port;
    char * ip;
    int id;
} RegistrationResponse;

typedef struct{
    char * action;
    char * actionInfo;
    char * result;
    int id;
} SubscriptionResponse;

typedef struct{
    char * action;
    char * actionType;
    char * result;
    char * timingType;
    char * eventType;
    int  sender;
    int  receiver;
    int  amount;
    int id;
}NotificationRequest;



static int jsoneq(const char *json, jsmntok_t *tok, const char *s);

int checkRegistrationResponse(RegistrationResponse *responseObj);
int getRegistrationResponse(char *responseStr,RegistrationResponse *responseObj);





#endif //BLOCKCHAINOBSERVER_JSONIN_H
