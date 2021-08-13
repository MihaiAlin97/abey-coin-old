//
// Created by Admin on 5/24/2021.
//

#include "ABEYJson.h"
#include "jsonOut.h"
#include <stdio.h>
#include <string.h>

int validateRegistrationResponse(char * regResp){

    JSON_Value *regRespSchema = json_parse_string(NotificationResponse(3,""));
    JSON_Value *regResp1 = json_parse_string("{\n"
                                              "   \"action\":\"Registration\",\n"
                                              "   \"actionType\":\"Response\",\n"
                                              "   \"actionInfo\":{\n"
                                              "      \"result\":\"Rejected\",\n"
                                              "      \"port\":\"null\",\n"
                                              "      \"ip\":\"null\"\n"
                                              "   }\n"
                                              "}");
    if ( json_validate(regRespSchema,regResp1) == JSONSuccess) return VALID_REGISTRATION_RESPONSE;

    else return INVALID_REGISTRATION_RESPONSE;
}

int validateSubscriptionResponse(char * subResp){

    return VALID_REGISTRATION_RESPONSE;
}

int validateNotificationRequest(char * notResp){



    return VALID_REGISTRATION_RESPONSE;
}


char *RegistrationResponseGetPort(char *regResp) {
    JSON_Value *regRespValue = json_parse_string(regResp);

    char *port = json_object_get_string(json_object_get_object(json_object(regRespValue),"actionInfo"),"port");

    printf("port %s\n",port);
    return port;
}

char *RegistrationResponseGetIp(char *regResp) {
    JSON_Value *regRespValue = json_parse_string(regResp);

    char *ip = json_object_get_string(json_object_get_object(json_object(regRespValue),"actionInfo"),"ip");
    printf("ip %s\n",ip);
    return ip;

}

char *RegistrationResponseGetResult(char *regResp) {
    JSON_Value *regRespValue = json_parse_string(regResp);

    char *result = json_object_get_string(json_object_get_object(json_object(regRespValue),"actionInfo"),"result");
    printf("result %s\n",result);
    return result;


}


int checkRegistrationResponse(char *resultStr){



    if ( strcmp( resultStr,"Accepted" ) == 0 )  {
        printf("Registration accepted\n");
        return REGISTRATION_ACCEPTED;
    }
    else if ( strcmp(resultStr,"Rejected" ) == 0 ) {
        printf("Registration rejected\n");
        return REGISTRATION_REJECTED;
    }
    else {
        printf("Registration result invalid\n");
        return INVALID_REGISTRATION_RESULT;
    }
}
