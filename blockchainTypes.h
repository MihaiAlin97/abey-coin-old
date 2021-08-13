//
// Created by Admin on 2/17/2021.
//

#ifndef BLOCKCHAINOBSERVER_BLOCKCHAINTYPES_H
#define BLOCKCHAINOBSERVER_BLOCKCHAINTYPES_H

#include "stdint.h"
#include "stdbool.h"

struct AccountObject {
   uint32_t account;
   char enc_pubkey [272] ;
   double balance;
   uint32_t n_operation;
   uint32_t updated_b;
   enum states { normal , listed } state;
   uint32_t locked_until_block;
   double price;
   uint32_t seller_account ;
   bool private_sale;
   char new_enc_pubkey [272] ;
   char* name;
   uint16_t type;

};


struct BlockObject {
    uint32_t block;
    char enc_pubkey [272] ;
    double reward;
    double fee;
    uint16_t ver;
    uint16_t ver_a;
    uint32_t timestamp;
    uint32_t target;
    uint32_t nonce;
    char payload [255] ;
    char sbh [32] ;
    char oph [32] ;
    char pow [32];
    uint32_t operations;
    uint32_t hashratekhs;
    uint32_t maturation;
};


struct OperationObject {
    bool valid;
    char * errors;
    uint32_t block;
    uint32_t time;
    uint32_t opblock;
    uint32_t maturation;
    enum optypes {
        BlockchainReward,
        Transaction ,
        ChangeKey ,
        RecoverFunds ,
        ListAccount ,
        DelistAccount,
        BuyAccount,
        ChangeKeySignedByAnotherAccount,
        ChangeAccountInfo,
        Multioperation,
        AccountForSale,
        AccountPurchased,
        AccountNameChanged
    } optype;
    uint32_t account;
    char * optxt;
    float amount;
    double balance;
    uint32_t sender_account;
    uint32_t dest_account;
    char enc_pubkey [ 272 ] ;
    char ophash[32] ;
    char old_ophash [32] ;
    char subtype[32] ;
    char signer_account [32] ;
};



#endif //BLOCKCHAINOBSERVER_BLOCKCHAINTYPES_H
