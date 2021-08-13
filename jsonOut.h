//
// Created by Admin on 2/9/2021.
//
#ifndef BLOCKCHAINOBSERVER_JSONOUT_H
#define BLOCKCHAINOBSERVER_JSONOUT_H

#define WaitMes "waiting\r\n"


#define RegistrationRequest(id,type) ""\
                               "{                                       \n"\
                               "    \"action\": \"Registration\",       \n"\
                               "    \"actionType\" : \"Request\"        \n"\
                               "    \"actionInfo\"  : {                 \n"\
                               "        \"type\"  : \""#type"\",        \n"\
                               "     },                                 \n"\
                               "    \"id\" :  " #id "                   \n"\
                               "}\r\n"

/*
"action": "Registration",
"actionType" : "Request",
"actionInfo": {
"type": "Partial"
},
"id": 1
*/


#define SubscriptionRequest(id,timingType,eventType,sender,receiver,amount) ""\
                               "{                                               \n"\
                               "    \"action\" : \"Subscription\",              \n"\
                               "    \"actionType\" : \"Subscription\",          \n"\
                               "    \"actionInfo\" : {                          \n"\
                               "        \"timingType\" : " #timingType",     \n"\
                               "        \"eventType\"  : "#eventType",      \n"\
                               "        \"eventArguments\"  :  {                \n"\
                               "            \"sender\"   : "#sender",       \n"\
                               "            \"receiver\" : "#receiver",     \n"\
                               "            \"amount\"   : "#amount"       \n"\
                               "         }                                     \n"\
                               "     },                                         \n"\
                               "    \"id\" : "#id"                              \n"\
                               "}\r\n"

/*
 "action": "Subscription",
    "actionType" : "Request",
    "actionInfo": {
      "timingType": "Before",
      "eventType": "Transaction",
      "eventArguments": {
        "sender" : "null",
        "receiver" : "85",
        "amount" : "2500",
      }
    },
 */

#define NotificationResponse(id,result)""\
                               "{                                       \n"\
                               "    \"action\": \"Subscription\",       \n"\
                               "    \"actionType\" : \"Response\"       \n"\
                               "    \"actionInfo\"  : {                 \n"\
                               "        \"result\"  : \""#result"\",    \n"\
                               "     },                                 \n"\
                               "    \"id\" :  " #id "                   \n"\
                               "}\r\n"

/*
 "_comment": "JSON for responding to notification - Done by subscriber",
  "subscriptionPositiveResponse" : {
    "action": "Subscription",
    "actionType" : "Response",
    "actionInfo": {
      "result" : "Accepted"
    },
    "id" : 1
  }
 */



#endif //BLOCKCHAINOBSERVER_JSONOUT_H
