//
// Created by Admin on 2/9/2021.
//

#include "jsonIn.h"

static const char *JSON_STRING =
        "{\"user\": \"johndoe\", \"admin\": false, \"uid\": 1000,\n  "
        "\"groups\": [\"users\", \"wheel\", \"audio\", \"video\"]}";

static int jsoneq(const char *json, jsmntok_t *tok, const char *s) {
    if (tok->type == JSMN_STRING && (int)strlen(s) == tok->end - tok->start &&
        strncmp(json + tok->start, s, tok->end - tok->start) == 0) {
        return 0;
    }
    return -1;
}

int checkRegistrationResponse(RegistrationResponse *responseObj){



    if ( strcmp( (*responseObj).result,"Accepted" ) == 0 )  {
        printf("Registration accepted\n");
        return REGISTRATION_ACCEPTED;
    }
    else if ( strcmp( (*responseObj).result,"Rejected" ) == 0 ) {
        printf("Registration rejected\n");
        return REGISTRATION_REJECTED;
    }
    else {
        printf("Registration result invalid\n");
        return INVALID_REGISTRATION_RESULT;
    }
}

int getRegistrationResponse(char *responseStr,RegistrationResponse *responseObj){

    int i;
    int r;
    jsmn_parser p;
    jsmntok_t t[128]; /* We expect no more than 128 tokens */

    jsmn_init(&p);
    r = jsmn_parse(&p, responseStr, strlen(responseStr), t,
                   sizeof(t) / sizeof(t[0]));
    if (r < 0) {
        printf("Failed to parse JSON: %d\n", r);
        return INVALID_JSON_STRING;
    }

    /* Loop over all keys of the root object */
    for (i = 1; i < r; i++) {
        printf("index I is %d\n",i);
        if (jsoneq(responseStr, &t[i], "action") == 0) {
            /* We may use strndup() to fetch string value */
            printf("- action: %.*s\n", t[i + 1].end - t[i + 1].start,
                   responseStr + t[i + 1].start);
            i++;
        } else if (jsoneq(responseStr, &t[i], "actionType") == 0) {
            /* We may additionally check if the value is either "true" or "false" */
            int j;
            printf("- actionInfo:\n");
            printf("- actionInfo size : % d\n",t[i + 1].size);

            // next token contains the number of keys(not keys and values)
            for (j = 0; j < t[i + 1].size * 2 ; j+=2) {

                printf ("subIndex is %d \n",i + j + 2);
                jsmntok_t *key = &t[i + j + 2];
                jsmntok_t *value = &t[i + j + 3];

                if (jsoneq(responseStr, key, "actionType") == 0) {
                    printf("- actionType: %.*s\n", value->end - value->start, responseStr + value->start);

                }

                else if (jsoneq(responseStr, key, "result") == 0) {
                    printf("- result: %.*s\n", value->end - value->start, responseStr + value->start);
                    (*responseObj).result = malloc(value->end - value->start);
                    strncpy((*responseObj).result,responseStr + value->start,value->end - value->start );
                }
                else if (jsoneq(responseStr, key, "port") == 0) {


                    printf("- port: %.*s\n", value->end - value->start, responseStr + value->start);

                    //allocate memory
                    (*responseObj).port = malloc(value->end - value->start);

                    //copy the string into port field
                    strncpy((*responseObj).port,responseStr + value->start,value->end - value->start);

                }

                else if (jsoneq(responseStr, key, "ip") == 0) {
                    printf("- ip: %.*s\n", value->end - value->start, responseStr + value->start);

                    (*responseObj).ip = malloc(value->end - value->start);

                    strncpy((*responseObj).ip,responseStr + value->start,value->end - value->start);
                }
                else {
                    printf("Unexpected key: %.*s\n", value->end - value->start, responseStr + value->start);
                    return INVALID_REGISTRATION_RESPONSE;
                }


            }
            i += t[i + 1].size * 2 + 1;
        } else if (jsoneq(responseStr, &t[i], "id") == 0) {
            printf("- id: %.*s\n", t[i + 1].end - t[i + 1].start,
                   responseStr + t[i + 1].start);
            i++;
        } else {
            printf("Unexpected key: %.*s\n", t[i].end - t[i].start,
                   responseStr + t[i].start);
        }

    }
    return VALID_REGISTRATION_RESPONSE;
}

int JSON_getKey(char * key, char * jsonString){

    int i;
    int r;
    jsmn_parser p;
    jsmntok_t t[128]; /* We expect no more than 128 tokens */

    jsmn_init(&p);
    r = jsmn_parse(&p, jsonString, strlen(jsonString), t,
                   sizeof(t) / sizeof(t[0]));
    if (r < 0) {
        printf("Failed to parse JSON: %d\n", r);
        return INVALID_JSON_STRING;
    }

    //JSON_paseDown(char * key,)
}


int getOperations() {
    int i;
    int r;
    jsmn_parser p;
    jsmntok_t t[128]; /* We expect no more than 128 tokens */

    jsmn_init(&p);
    r = jsmn_parse(&p, JSON_STRING, strlen(JSON_STRING), t,
                   sizeof(t) / sizeof(t[0]));
    if (r < 0) {
        printf("Failed to parse JSON: %d\n", r);
        return 1;
    }



    /* Assume the top-level element is an object */
    if (r < 1 || t[0].type != JSMN_OBJECT) {
        printf("Object expected\n");
        return 1;
    }

    /* Loop over all keys of the root object */
    for (i = 1; i < r; i++) {
        if (jsoneq(JSON_STRING, &t[i], "user") == 0) {
            /* We may use strndup() to fetch string value */
            printf("- User: %.*s\n", t[i + 1].end - t[i + 1].start,
                   JSON_STRING + t[i + 1].start);
            i++;
        } else if (jsoneq(JSON_STRING, &t[i], "admin") == 0) {
            /* We may additionally check if the value is either "true" or "false" */
            printf("- Admin: %.*s\n", t[i + 1].end - t[i + 1].start,
                   JSON_STRING + t[i + 1].start);
            i++;
        } else if (jsoneq(JSON_STRING, &t[i], "uid") == 0) {
            /* We may want to do strtol() here to get numeric value */
            printf("- UID: %.*s\n", t[i + 1].end - t[i + 1].start,
                   JSON_STRING + t[i + 1].start);
            i++;
        } else if (jsoneq(JSON_STRING, &t[i], "groups") == 0) {
            int j;
            printf("- Groups:\n");
            if (t[i + 1].type != JSMN_ARRAY) {
                continue; /* We expect groups to be an array of strings */
            }
            for (j = 0; j < t[i + 1].size; j++) {
                jsmntok_t *g = &t[i + j + 2];
                printf("  * %.*s\n", g->end - g->start, JSON_STRING + g->start);
            }
            i += t[i + 1].size + 1;
        } else {
            printf("Unexpected key: %.*s\n", t[i].end - t[i].start,
                   JSON_STRING + t[i].start);
        }
    }
    return EXIT_SUCCESS;
}