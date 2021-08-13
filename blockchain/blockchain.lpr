library blockchain;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpjson, jsonparser, fphttpclient, fpopenssl, openssl,
  ctypes;
  { you can add units after this }
Const
  CT_RPC_URL  = 'http://localhost:12261';
  CT_RPC_CALL_GETACCOUNT = '{"jsonrpc":"2.0","method":"getaccount","params":{"account":RACCOUNT},"id":123}' ;
  CT_RPC_CALL_GETWALLETACCOUNTS = '{"jsonrpc":"2.0","method":"getwalletaccounts","id":123}' { - GET AVAILABLE WALLET ACCOUNTS INFORMATION (ALL OR FILTERED BY PUBLIC KEY) } ;
  {CT_RPC_CALL_GETWALLETACCOUNTSCOUNT = { - GET NUMBER OF AVAILABLE WALLET ACCOUNTS (TOTAL OR FILTERED BY PUBLIC KEY)} ;
  CT_RPC_CALL_GETWALLETPUBKEYS = { - GET WALLET PUBLIC KEYS} ;
  CT_RPC_CALL_GETWALLETPUBKEY = { - SEARCH FOR A PUBLIC KEY IN THE WALLET } ;
  CT_RPC_CALL_GETWALLETCOINS = { - GET WALLET COINS TOTAL BALANCE (TOTAL OR FILTERED BY PUBLIC KEY)} ;
  CT_RPC_CALL_GETBLOCK = { - GET BLOCK INFORMATION        } ;
  CT_RPC_CALL_GETBLOCKS = { - GET A LIST OF BLOCKS (LAST N BLOCKS, OR FROM START TO END)} ;
  CT_RPC_CALL_GETBLOCKCOUNT = { - GET BLOCKCHAIN HIGH IN THIS NODE      } ;
  CT_RPC_CALL_GETBLOCKOPERATION = { - GET AN OPERATION OF THE BLOCK INFORMATION  } ;
  CT_RPC_CALL_GETBLOCKOPERATIONS = { - GET ALL OPERATIONS OF SPECIFIED BLOCK    } ;
  CT_RPC_CALL_GETACCOUNTOPERATIONS = { - GET OPERATIONS MADE TO AN ACCOUNT        } ;
  CT_RPC_CALL_GETPENDINGS = { - GET PENDINGS OPERATIONS TO BE INCLUDED IN THE BLOCKCHAIN   } ;
  CT_RPC_CALL_GETPENDINGSCOUNT = { - RETURNS NODE PENDING BUFFER COUNT ( NEW ON BUILD 3.0 )} ;
  CT_RPC_CALL_FINDOPERATION = { - FINDS AN OPERATION BY "OPHASH" } ;
  CT_RPC_CALL_FINDACCOUNTS = { - FIND ACCOUNTS BY NAME/TYPE    } ;
  CT_RPC_CALL_SENDTO = { - EXECUTES A TRANSACTION        } ;
  CT_RPC_CALL_CHANGEKEY = { - EXECUTES A CHANGE KEY OVER AN ACCOUNT    } ;
  CT_RPC_CALL_CHANGEKEYS = { - EXECUTES A CHANGE KEY OVER MULTIPLE ACCOUNTS   } ;
  CT_RPC_CALL_LISTACCOUNTFORSALE = { - LISTS AN ACCOUNT FOR SALE (PUBLIC OR PRIVATE)    } ;
  CT_RPC_CALL_DELISTACCOUNTFORSALE = { - DELIST AN ACCOUNT FOR SALE            } ;
  CT_RPC_CALL_BUYACCOUNT = { - BUY AN ACCOUNT PREVIOUSLY LISTED FOR SALE (PUBLIC OR PRIVATE)} ;
  CT_RPC_CALL_CHANGEACCOUNTINFO = { - CHANGES AN ACCOUNT PUBLIC KEY, OR NAME, OR TYPE VALUE (AT LEAST 1 ON 3)    } ;
  CT_RPC_CALL_SIGNSENDTO = { - CREATES AND SIGNS A TRANSACTION, BUT NO TRANSFERS IT TO NETWORK             } ;
  CT_RPC_CALL_SIGNCHANGEKEY  = {- CREATES AND SIGNS A CHANGE KEY OVER AN ACCOUNT, BUT NO TRANSFERS IT TO NETWORK  } ;
  CT_RPC_CALL_SIGNLISTACCOUNTFORSALE = { - SIGNS A LIST AN ACCOUNT FOR SALE (PUBLIC OR PRIVATE) FOR COLD WALLETS  } ;
  CT_RPC_CALL_SIGNDELISTACCOUNTFORSALE = { - SIGNS A LIST AN ACCOUNT FOR SALE (PUBLIC OR PRIVATE) FOR COLD WALLETS  } ;
  CT_RPC_CALL_SIGNBUYACCOUNT = { - SIGNS A BUY OPERATION FOR COLD WALLETS                                           } ;
  CT_RPC_CALL_SIGNCHANGEACCOUNTINFO = { - SIGNS A CHANGE ACCOUNT INFO FOR COLD COLD WALLETS} ;
  CT_RPC_CALL_OPERATIONSINFO = { - GETS INFORMATION ABOUT A SIGNED OPERATION WITHOUT TRANSFERING IT TO NETWORK } ;
  CT_RPC_CALL_EXECUTEOPERATIONS = { - EXECUTES A SIGNED OPERATION AND TRANSFERS IT TO THE NETWORK          } ;
  CT_RPC_CALL_NODESTATUS  = {- RETURNS NODE STATUS        } ;
  CT_RPC_CALL_ENCODEPUBKEY  = {- ENCODES A PUBLIC KEY    } ;
  CT_RPC_CALL_DECODEPUBKEY  = {- DECODES A PUBLIC KEY     } ;
  CT_RPC_CALL_PAYLOADENCRYPT = { - ENCRYPTS A TEXT         } ;
  CT_RPC_CALL_PAYLOADDECRYPT = { - DECRYPTS A TEXT          } ;
  CT_RPC_CALL_GETCONNECTIONS = { - LISTS ALL ACTIVE CONNECTIONS OF THIS NODE    } ;
  CT_RPC_CALL_ADDNEWKEY = { - ADDS A NEW KEY TO THE WALLET               } ;
  CT_RPC_CALL_LOCK = { - LOCKS THE WALLET        } ;
  CT_RPC_CALL_UNLOCK = { - UNLOCKS THE WALLET   } ;
  CT_RPC_CALL_SETWALLETPASSWORD = { - CHANGES WALLET PASSWORD   } ;
  CT_RPC_CALL_STOPNODE = { - STOPS THE NODE       } ;
  RPC_CALL_STARTNODE = { - STARTS THE NODE       } ;
  CT_RPC_CALL_SIGNMESSAGE = { - SIGNS A DIGEST MESSAGE USING A PUBLIC KEY ( NEW ON BUILD 3.0 )} ;
  CT_RPC_CALL_VERIFYSIGN = { - VERIFY IF A DIGEST MESSAGE IS SIGNED BY A PUBLIC KEY ( NEW ON BUILD 3.0 )} ;
  CT_RPC_CALL_MULTIOPERATIONADDOPERATION  = { - ADDS OPERATIONS TO A MULTIOPERATION (OR CREATES A NEW MULTIOPERATION AND ADDS NEW OPERATIONS) ( NEW ON BUILD 3.0 )   } ;
  CT_RPC_CALL_MULTIOPERATIONSIGNOFFLINE = {   - THIS METHOD WILL SIGN A MULTIOPERATION FOUND IN A "RAWOPERATIONS", MUST PROVIDE ALL N_OPERATION INFO OF EACH SIGNER BECAUSE CAN WORK IN COLD WALLETS ( NEW ON BUILD 3.0 )} ;
  CT_RPC_CALL_MULTIOPERATIONSIGNONLINE = { - THIS METHOD WILL SIGN A MULTIOPERATION FOUND IN A "RAWOPERATIONS" BASED ON CURRENT SAFEBOX STATE PUBLIC KEYS ( NEW ON BUILD 3.0 )    } ;
  CT_RPC_CALL_OPERATIONSDELETE}

  CT_RPC_CALL_GETBLOCKCOUNT='{"jsonrpc":"2.0","method":"getblockcount","id":123}';
  CT_RPC_CALL_GETBLOCK='{"jsonrpc":"2.0","method":"getblock","params":{"block":RBLOCK},"id":123}';
  CT_RPC_CALL_GETBLOCKS='{"jsonrpc":"2.0","method":"getblocks","params":{"start":RSTART,"end":REND},"id":123}';
  CT_RPC_CALL_GETLASTNBLOCKS='{"jsonrpc":"2.0","method":"getblocks","params":{"last":RLAST},"id":123}';
  CT_RPC_CALL_GETBLOCKOPERATIONS='{"jsonrpc":"2.0","method":"getblockoperations","params":{"block":RBLOCK,"start":0,"end":100},"id":123}' ;



  function executeJsonCall(json : String): String ; cdecl;
  var
    responseData: String;
    postJson: TJSONData;
  Begin
    postJson:=getJson(json);
    With TFPHttpClient.Create(Nil) do
    try
      addHeader('Content-Type', 'application/json');
      requestBody := TStringStream.Create(PostJson.AsJSON);
      responseData := Post(CT_RPC_URL);
    finally
     free;
    end;
    result :=responseData;
    {WriteLn(responseData);}
  End;

function addNode(nodes:pcchar):pcchar ; cdecl;
var
  jsonBody: string;
  temp: string;
  begin
       jsonBody := '{"jsonrpc":"2.0","method":"addnode","params":{"nodes":"'+ nodes^.ToString() +'"},"id":123}';
       result := PCChar(ExecuteJSONCall(jsonBody));
  end;

function getAccount(account:cuint64):pcchar ; cdecl;
var jsonBody:String;
begin
  jsonBody:='{"jsonrpc":"2.0","method":"getaccount","params":{"account":RACCOUNT},"id":123}' ;
  jsonBody:= stringReplace(CT_RPC_Call_GetBlocks,'RACCOUNT',intToStr(account),[rfReplaceAll, rfIgnoreCase]);
  result := pcchar(executeJsonCall(jsonBody));
end;


function getWalletAccounts(enc_pubkey:pcchar;b58_pubkey:pcchar;start:cint64;max:cint64):pcchar ; cdecl;
var
  jsonBody:String;
  jsonParams:TJSONArray;
  jsonObject:TJSONObject ;
begin

  if(enc_pubkey <> Nil) then begin
    jsonObject.Add('enc_pubkey',enc_pubkey^.ToString());
    jsonParams.Add(jsonObject);
    jsonObject.Clear;
  end;

  if(b58_pubkey <> Nil) then begin
    jsonObject.Add('b58_pubkey',b58_pubkey^.ToString());
    jsonParams.Add(jsonObject);
    jsonObject.Clear;
  end;

  if(start <> -1) then begin
    jsonObject.Add('start',start.ToString());
    jsonParams.Add(jsonObject);
    jsonObject.Clear;
  end;

  if(max <> -1) then begin
    jsonObject.Add('max',max.ToString());
    jsonParams.Add(jsonObject);
    jsonObject.Clear;
  end;

  jsonBody:='{"jsonrpc":"2.0","method":"getwalletaccounts",'+jsonParams.ToString+'"id":123}' ;
  result := pcchar(ExecuteJSONCall(jsonBody));
end;

function getWalletAccountsCount(enc_pubkey:pcchar;b58_pubkey:pcchar;start:cint64;max:cint64): pcchar; cdecl;
var
  jsonBody:string;
  jsonParams:tJsonArray;
  jsonObject:tJsonObject ;

  begin
  if(enc_pubkey <> Nil) then begin
    jsonObject.Add('enc_pubkey',enc_pubkey^.ToString());
    jsonParams.Add(jsonObject);
    jsonObject.Clear;
  end;

  if(b58_pubkey <> Nil) then begin
    jsonObject.Add('b58_pubkey',b58_pubkey^.ToString());
    jsonParams.Add(jsonObject);
    jsonObject.Clear;
  end;

  if(start <> -1) then begin
    jsonObject.Add('start',start.ToString());
    jsonParams.Add(jsonObject);
    jsonObject.Clear;
  end;

  if(max <> -1) then begin
    jsonObject.Add('max',max.ToString());
    jsonParams.Add(jsonObject);
    jsonObject.Clear;
  end;

  jsonBody:='{"jsonrpc":"2.0","method":"getwalletaccountscount",'+jsonParams.ToString+'"id":123}' ;
  result := pcchar(ExecuteJSONCall(jsonBody));

  end;

function getBlockCount: pcchar ; cdecl;
Begin
  result := pcchar(executeJsonCall(CT_RPC_CALL_GETBLOCKCOUNT));
End;


function getBlock(block:cuint64): pcchar ; cdecl;
var jsonBody:String;
Begin
  jsonBody:= StringReplace(CT_RPC_Call_GetBlock,'RBLOCK',IntToStr(block),[rfReplaceAll, rfIgnoreCase]);
  result := PCChar(ExecuteJSONCall(CT_RPC_Call_GetBlock));
End;

function getBlocks(startBlock:cuint64;endBlock:cuint64): pcchar ; cdecl;
var JsonBody:String;
Begin
  JsonBody:= StringReplace(CT_RPC_Call_GetBlocks,'RSTART',IntToStr(startBlock),[rfReplaceAll, rfIgnoreCase]);
  JsonBody:= StringReplace(      JsonBody    ,'REND',IntToStr(startBlock),[rfReplaceAll, rfIgnoreCase]);

  result := pcchar(ExecuteJSONCall(CT_RPC_Call_GetBlocks));
End;




{function GetBlocks(StartBlock:Integer; EndBlock: Integer): TJSONData;
var
  json: String;
  Block:TJSONData;
  Blocks: TJSONArray;
  Index:Integer;
  ResultedJSON:TJSONData;
  CheckBlock:TJSONData;
Begin
  Blocks:=TJSONArray.Create;

  json := StringReplace(RPC_Call_GetBlocks,'RSTART',IntToStr(StartBlock),[rfReplaceAll, rfIgnoreCase]);
  json := StringReplace(json,'REND',IntToStr(EndBlock),[rfReplaceAll, rfIgnoreCase]);

  ResultedJSON := ExecuteJSONCall(json);
  CheckBlock:=ResultedJson.FindPath('result');

  if CheckBlock.Count <> 0 then
  begin
       for Index:=0 to CheckBlock.Count-1 do
       begin
         {WriteLn(CheckBlock.Items[Index].AsJson);}
         Block:=CheckBlock.Items[Index];
         Blocks.Add(Block);
       end;
  end;

  result:=Blocks;
End;

function GetLastNBlocks(Last:Integer): TJSONData;
var
  json: String;
Begin
    json := StringReplace(RPC_Call_GetLastNBlocks,'RLAST',IntToStr(Last),[rfReplaceAll, rfIgnoreCase]);
    result := ExecuteJSONCall(json);
    {WriteLn(result.AsJson);}
End;

function GetOperations(StartBlock:Integer;EndBlock:Integer):TJSONData;
var
  Block: Integer;
  Index:Integer;
  Operation:TJSONData;
  Operations:TJSONArray;
  ResultedJSON:TJSONData;
  CheckOp:TJSONData;
begin
    Operations:=TJSONArray.Create;

    for Block := StartBlock to EndBlock do
    begin
      ResultedJSON:=GetBlockOperations(Block);
      CheckOp:=ResultedJSON.FindPath('result');
      {WriteLn(IntToStr(Block));  }
      if CheckOp.Count <> 0 then
      begin

           for Index:=0 to CheckOp.Count-1 do
           begin
               Operation:=CheckOp.Items[Index];
               Operations.Add(Operation);
           end;
      end;
    end;
    {WriteLn(Operations.ASJson);}
    result:=Operations;
end;

function GetBlockOperations(BlockNumber:Integer): TJSONData;cdecl;
var
  json:String;
begin
    json := StringReplace(RPC_Call_GetBlockOperations,'RBLOCK',IntToStr(BlockNumber),[rfReplaceAll, rfIgnoreCase]);
    result := ExecuteJSONCall(json);
    {WriteLn(result.AsJson); }
end;

function ExecuteJSONCall(Json : String): TJSONData;
const url = 'http://localhost:5003';
var
  responseData: String;
  PostJson: TJSONData;
Begin
  PostJson:=GetJSON(Json);
  With TFPHttpClient.Create(Nil) do
  try
    AddHeader('Content-Type', 'application/json');
    RequestBody := TStringStream.Create(PostJson.AsJSON);
    responseData := Post(url);
  finally
   Free;
  end;
  result := GetJSON(responseData);
  {WriteLn(responseData);}
End;

function GetLastNBlocks(Last:Integer): TJSONData;
var
  json: String;
Begin
    json := StringReplace(RPC_Call_GetLastNBlocks,'RLAST',IntToStr(Last),[rfReplaceAll, rfIgnoreCase]);
    result := ExecuteJSONCall(json);
    {WriteLn(result.AsJson);}
End;

function GetOperations(StartBlock:Integer;EndBlock:Integer):TJSONData;
var
  Block: Integer;
  Index:Integer;
  Operation:TJSONData;
  Operations:TJSONArray;
  ResultedJSON:TJSONData;
  CheckOp:TJSONData;
begin
    Operations:=TJSONArray.Create;

    for Block := StartBlock to EndBlock do
    begin
      ResultedJSON:=GetBlockOperations(Block);
      CheckOp:=ResultedJSON.FindPath('result');
      {WriteLn(IntToStr(Block));  }
      if CheckOp.Count <> 0 then
      begin

           for Index:=0 to CheckOp.Count-1 do
           begin
               Operation:=CheckOp.Items[Index];
               Operations.Add(Operation);
           end;
      end;
    end;
    {WriteLn(Operations.ASJson);}
    result:=Operations;
end;

function GetBlockOperations(BlockNumber:Integer): TJSONData;
var
  json:String;
begin
    json := StringReplace(RPC_Call_GetBlockOperations,'RBLOCK',IntToStr(BlockNumber),[rfReplaceAll, rfIgnoreCase]);
    result := ExecuteJSONCall(json);
    {WriteLn(result.AsJson); }
end; }

exports
  addNode,
  getAccount,
  getWalletAccountsCount,
  getWalletAccounts,
  getBlockCount,
  getBlock,
  getBlocks;
end.



