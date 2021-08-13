unit URPC;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

{$I config.inc}

Uses UThread, ULog, UConst, UNode, UAccounts, UCrypto, UBlockChain,
  UNetProtocol, UOpTransaction, UWallet, UTime, UPCEncryption, UTxMultiOperation,
  UJSONFunctions, classes, blcksock, synsock,
  IniFiles, Variants, math, UBaseTypes, UFolderHelper, LazUTF8, Process, DBStorage, UVirtualMachine,
  {$IFDEF Use_OpenSSL}
  UOpenSSL,
  {$ENDIF}
  UPCOrderedLists, UPCDataTypes,
  {$IFNDEF FPC}System.Generics.Collections{$ELSE}Generics.Collections{$ENDIF},
  UNetProtection,lazlogger;

Const
  CT_RPC_ErrNum_InternalError = 100;
  CT_RPC_ErrNum_NotImplemented = 101;

  CT_RPC_ErrNum_MethodNotFound = 1001;
  CT_RPC_ErrNum_InvalidAccount = 1002;
  CT_RPC_ErrNum_InvalidBlock = 1003;
  CT_RPC_ErrNum_InvalidOperation = 1004;
  CT_RPC_ErrNum_InvalidPubKey = 1005;
  CT_RPC_ErrNum_InvalidAccountName = 1006;
  CT_RPC_ErrNum_NotFound = 1010;
  CT_RPC_ErrNum_WalletPasswordProtected = 1015;
  CT_RPC_ErrNum_InvalidData = 1016;
  CT_RPC_ErrNum_InvalidSignature = 1020;
  CT_RPC_ErrNum_NotAllowedCall = 1021;
  CT_RPC_ErrNum_FlagForStartingBlock = 1031;
  CT_RPC_ErrNum_ActiveBlocks = 1032;
  CT_RPC_ErrNum_PassiveBlocks = 1033;
  CT_RPC_ErrNum_MaxInstructionCount = 1035;
  CT_RPC_ErrNum_MaxMemoryUsage = 1036;
  CT_RPC_ErrNum_MaxRuntimeSeconds = 1037;
  CT_RPC_ErrNum_MaxIoUsage = 1038;
  CT_RPC_ErrNum_MaxDataSegmentSize = 1039;
  CT_RPC_ErrNum_ExportTableSize = 1040;
  CT_RPC_ErrNum_BytecodeSize = 1041;
  CT_RPC_ErrNum_ExportTableFilePath = 1042;
  CT_RPC_ErrNum_PayloadFilePath = 1043;
  CT_RPC_ErrNum_InvalidContractID = 1060;
  CT_RPC_ErrNum_InvalidFunctionName = 1061;
  CT_RPC_ErrNum_InvalidParameterTypeForContract = 1062;
  CT_RPC_ErrNum_InvalidParameterValueForType = 1063;
  CT_RPC_ErrNum_ErrorDuringFirstExecutionOfContract = 1070;


Type

  { TRPCServer }

  { TABEYJSONComp }

  TABEYJSONComp = Class
  private
    class function OperationsHashTreeToHexaString(Const OperationsHashTree : TOperationsHashTree) : String;
  public
    class function ToJSONCurrency(pascalCoins : Int64) : Currency;
    class procedure FillAccountObject(Const account : TAccount; jsonObj : TABEYJSONObject);
    class procedure FillBlockObject(nBlock : Cardinal; ANode : TNode; jsonObject: TABEYJSONObject);
    class procedure FillOperationObject(Const OPR : TOperationResume; currentNodeBlocksCount : Cardinal; jsonObject : TABEYJSONObject);
    class procedure FillOperationsHashTreeObject(Const OperationsHashTree : TOperationsHashTree; jsonObject : TABEYJSONObject);
    class procedure FillMultiOperationObject(current_protocol : Word; Const multiOperation : TOpMultiOperation; jsonObject : TABEYJSONObject);
    class procedure FillPublicKeyObject(const PubKey : TAccountKey; jsonObj : TABEYJSONObject);
  end;

  TRPCServerThread = Class;
  TRPCServer = Class
  private
    FRPCServerThread : TRPCServerThread;
    FActive: Boolean;
    FWalletKeys: TWalletKeysExt;
    FPort: Word;
    FJSON20Strict: Boolean;
    FIniFileName: String;
    FIniFile : TIniFile;
    FRPCLog : TLog;
    FCallsCounter : Int64;
    FValidIPs: String;
    FAllowUsePrivateKeys: Boolean;
    procedure SetActive(AValue: Boolean);
    procedure SetIniFileName(const Value: String);
    procedure SetLogFileName(const Value: String);
    Function GetLogFileName : String;
    procedure SetValidIPs(const Value: String);  protected
    Function IsValidClientIP(Const clientIp : String; clientPort : Word) : Boolean;
    Procedure AddRPCLog(Const Sender : String; Const Message : String);
    Function GetNewCallCounter : Int64;
  public
    Constructor Create;
    Destructor Destroy; override;
    Property Port : Word read FPort Write FPort;
    Property Active : Boolean read FActive write SetActive;
    Property WalletKeys : TWalletKeysExt read FWalletKeys write FWalletKeys;
    //
    Property JSON20Strict : Boolean read FJSON20Strict write FJSON20Strict;
    Property IniFileName : String read FIniFileName write SetIniFileName;
    Property LogFileName : String read GetLogFileName write SetLogFileName;
    Property ValidIPs : String read FValidIPs write SetValidIPs;
    Property AllowUsePrivateKeys : Boolean read FAllowUsePrivateKeys write FAllowUsePrivateKeys; // New v4 protection for free access server
  end;

  { TRPCServerThread }

  TRPCServerThread = Class(TABEYThread)
    FServerSocket:TTCPBlockSocket;
    FPort : Word;
  protected
    procedure BCExecute; override;
  public
    Constructor Create(Port : Word);
    Destructor Destroy; Override;
  End;

  { TRPCProcess }

  TRPCProcess = class(TABEYThread)
  private
    FSock:TTCPBlockSocket;
    FNode : TNode;
  public
    Constructor Create (hsock:tSocket);
    Destructor Destroy; override;
    procedure BCExecute; override;
    function ProcessMethod(Const method : String; params : TABEYJSONObject; jsonresponse : TABEYJSONObject; Var ErrorNum : Integer; Var ErrorDesc : String) : Boolean;
  end;


implementation

Uses  {$IFNDEF FPC}windows,{$ENDIF}
  SysUtils, Synautil;

var _RPCServer : TRPCServer = Nil;

{ TABEYJSONComp }

class function TABEYJSONComp.ToJSONCurrency(pascalCoins: Int64): Currency;
Begin
  Result := RoundTo( pascalCoins / 10000 , -4);
end;

class procedure TABEYJSONComp.FillBlockObject(nBlock : Cardinal; ANode : TNode; jsonObject: TABEYJSONObject);
var pcops : TABEYOperationsComp;
  ob : TOperationBlock;
begin
  pcops := TABEYOperationsComp.Create(Nil);
  try
    If ANode.Bank.BlocksCount<=nBlock then begin
      Exit;
    end;
    ob := ANode.Bank.Vault.Block(nBlock).blockchainInfo;

    jsonObject.GetAsVariant('block').Value:=ob.block;
    jsonObject.GetAsVariant('enc_pubkey').Value := TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(ob.account_key));
    jsonObject.GetAsVariant('reward').Value:=ToJSONCurrency(ob.reward);
    jsonObject.GetAsVariant('fee').Value:=ToJSONCurrency(ob.fee);
    jsonObject.GetAsVariant('ver').Value:=ob.protocol_version;
    jsonObject.GetAsVariant('ver_a').Value:=ob.protocol_available;
    jsonObject.GetAsVariant('timestamp').Value:=Int64(ob.timestamp);
    jsonObject.GetAsVariant('target').Value:=Int64(ob.compact_target);
    jsonObject.GetAsVariant('nonce').Value:=Int64(ob.nonce);
    jsonObject.GetAsVariant('payload').Value:=ob.block_payload.ToString;
    jsonObject.GetAsVariant('sbh').Value:=TCrypto.ToHexaString(ob.initial_safe_box_hash);
    jsonObject.GetAsVariant('oph').Value:=TCrypto.ToHexaString(ob.operations_hash);
    jsonObject.GetAsVariant('pow').Value:=TCrypto.ToHexaString(ob.proof_of_work);
    jsonObject.GetAsVariant('hashratekhs').Value := ANode.Bank.Vault.CalcBlockHashRateInKhs(ob.Block,50);
    jsonObject.GetAsVariant('maturation').Value := ANode.Bank.BlocksCount - ob.block - 1;
    If ANode.Bank.LoadOperations(pcops,nBlock) then begin
      jsonObject.GetAsVariant('operations').Value:=pcops.Count;
    end;
  finally
    pcops.Free;
  end;
end;

class procedure TABEYJSONComp.FillOperationObject(const OPR: TOperationResume; currentNodeBlocksCount : Cardinal; jsonObject: TABEYJSONObject);
Var i : Integer;
  jsonArr : TABEYJSONArray;
  auxObj : TABEYJSONObject;
Begin
  if Not OPR.valid then begin
    jsonObject.GetAsVariant('valid').Value := OPR.valid;
  end;
  if (OPR.errors<>'') And (Not OPR.valid) then begin
    jsonObject.GetAsVariant('errors').Value := OPR.errors;
  end;
  if OPR.valid then begin
    jsonObject.GetAsVariant('block').Value:=OPR.Block;
    jsonObject.GetAsVariant('time').Value:=OPR.time;
    jsonObject.GetAsVariant('opblock').Value:=OPR.NOpInsideBlock;
    if (OPR.Block>0) And (OPR.Block<currentNodeBlocksCount) then
      jsonObject.GetAsVariant('maturation').Value := currentNodeBlocksCount - OPR.Block - 1
    else jsonObject.GetAsVariant('maturation').Value := null;
  end;
  jsonObject.GetAsVariant('optype').Value:=OPR.OpType;
  jsonObject.GetAsVariant('subtype').Value:=OPR.OpSubtype;
  If (Not OPR.isMultiOperation) then Begin
    jsonObject.GetAsVariant('account').Value:=OPR.AffectedAccount;
    jsonObject.GetAsVariant('signer_account').Value:=OPR.SignerAccount;
    if (OPR.n_operation>0) then jsonObject.GetAsVariant('n_operation').Value:=OPR.n_operation;
  end;
  // New V3: Will include senders[], receivers[] and changers[]
    jsonArr := jsonObject.GetAsArray('senders');
    for i:=Low(OPR.senders) to High(OPR.Senders) do begin
      auxObj := jsonArr.GetAsObject(jsonArr.Count);
      auxObj.GetAsVariant('account').Value := OPR.Senders[i].Account;
      if (OPR.Senders[i].N_Operation>0) then auxObj.GetAsVariant('n_operation').Value := OPR.Senders[i].N_Operation;
      auxObj.GetAsVariant('amount').Value := ToJSONCurrency(OPR.Senders[i].Amount * (-1));
      auxObj.GetAsVariant('payload').Value := TCrypto.ToHexaString(OPR.Senders[i].Payload);
    end;
    //
    jsonArr := jsonObject.GetAsArray('receivers');
    for i:=Low(OPR.Receivers) to High(OPR.Receivers) do begin
      auxObj := jsonArr.GetAsObject(jsonArr.Count);
      auxObj.GetAsVariant('account').Value := OPR.Receivers[i].Account;
      auxObj.GetAsVariant('amount').Value := ToJSONCurrency(OPR.Receivers[i].Amount);
      auxObj.GetAsVariant('payload').Value := TCrypto.ToHexaString(OPR.Receivers[i].Payload);
    end;
    jsonArr := jsonObject.GetAsArray('changers');
    for i:=Low(OPR.Changers) to High(OPR.Changers) do begin
      auxObj := jsonArr.GetAsObject(jsonArr.Count);
      auxObj.GetAsVariant('account').Value := OPR.Changers[i].Account;
      if (OPR.Changers[i].N_Operation>0) then auxObj.GetAsVariant('n_operation').Value := OPR.Changers[i].N_Operation;
      If (public_key in OPR.Changers[i].Changes_type) then begin
        auxObj.GetAsVariant('new_enc_pubkey').Value := TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(OPR.Changers[i].New_Accountkey));
      end;
      If account_name in OPR.Changers[i].Changes_type then begin
        auxObj.GetAsVariant('new_name').Value := OPR.Changers[i].New_Name.ToPrintable;
      end;
      If account_type in OPR.Changers[i].Changes_type then begin
        auxObj.GetAsVariant('new_type').Value := OPR.Changers[i].New_Type;
      end;
      if (list_for_public_sale in OPR.Changers[i].Changes_type)
        Or (list_for_private_sale in OPR.Changers[i].Changes_type) then begin
        auxObj.GetAsVariant('seller_account').Value := OPR.Changers[i].Seller_Account;
        auxObj.GetAsVariant('account_price').Value := ToJSONCurrency(OPR.Changers[i].Account_Price);
      end;
      if (list_for_private_sale in OPR.Changers[i].Changes_type) then begin
        auxObj.GetAsVariant('locked_until_block').Value := OPR.Changers[i].Locked_Until_Block;
        auxObj.GetAsVariant('new_enc_pubkey').Value := TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(OPR.Changers[i].New_Accountkey));
      end;
      if (OPR.Changers[i].Fee<>0) then begin
        auxObj.GetAsVariant('fee').Value := ToJSONCurrency(OPR.Changers[i].Fee * (-1));
      end;
    end;
  jsonObject.GetAsVariant('optxt').Value:=OPR.OperationTxt;
  jsonObject.GetAsVariant('fee').Value:=ToJSONCurrency(OPR.Fee);
  jsonObject.GetAsVariant('amount').Value:=ToJSONCurrency(OPR.Amount);
  if (Not OPR.isMultiOperation) then begin
    jsonObject.GetAsVariant('payload').Value:=TCrypto.ToHexaString(OPR.OriginalPayload);
  end;
  if (OPR.Balance>=0) And (OPR.valid) then jsonObject.GetAsVariant('balance').Value:=ToJSONCurrency(OPR.Balance);
  If (OPR.OpType = CT_Op_Transaction) then begin
    If OPR.SignerAccount>=0 then begin
      jsonObject.GetAsVariant('sender_account').Value:=OPR.SignerAccount;
    end;
    If OPR.DestAccount>=0 then begin
      jsonObject.GetAsVariant('dest_account').Value:=OPR.DestAccount;
    end;
  end;
  If OPR.newKey.EC_OpenSSL_NID>0 then begin
    jsonObject.GetAsVariant('enc_pubkey').Value := TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(OPR.newKey));
  end;
  if (OPR.valid) And (Length(OPR.OperationHash)>0) then begin
    jsonObject.GetAsVariant('ophash').Value := TCrypto.ToHexaString(OPR.OperationHash);
    if (OPR.Block<CT_Protocol_Upgrade_v2_MinBlock) then begin
      jsonObject.GetAsVariant('old_ophash').Value := TCrypto.ToHexaString(OPR.OperationHash_OLD);
    end;
  end;
end;

class procedure TABEYJSONComp.FillAccountObject(const account: TAccount; jsonObj: TABEYJSONObject);
Begin
  jsonObj.GetAsVariant('account').Value:=account.account;
  jsonObj.GetAsVariant('enc_pubkey').Value := TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(account.accountInfo.accountKey));
  jsonObj.GetAsVariant('balance').Value:=ToJSONCurrency(account.balance);
  jsonObj.GetAsVariant('n_operation').Value:=account.n_operation;
  jsonObj.GetAsVariant('updated_b').Value:=account.updated_block;
  case account.accountInfo.state of
    as_Normal : jsonObj.GetAsVariant('state').Value:='normal';
    as_ForSale : begin
      jsonObj.GetAsVariant('state').Value:='listed';
      jsonObj.GetAsVariant('locked_until_block').Value:=account.accountInfo.locked_until_block;
      jsonObj.GetAsVariant('price').Value:=ToJSONCurrency(account.accountInfo.price);
      jsonObj.GetAsVariant('seller_account').Value:=account.accountInfo.account_to_pay;
      jsonObj.GetAsVariant('private_sale').Value:= (account.accountInfo.new_publicKey.EC_OpenSSL_NID<>0);
      if not (account.accountInfo.new_publicKey.EC_OpenSSL_NID<>0) then begin
        jsonObj.GetAsVariant('new_enc_pubkey').Value := TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(account.accountInfo.new_publicKey));
      end else raise Exception.Create('ERROR DEV 20170425-1');
    end;
  end;
  jsonObj.GetAsVariant('name').Value := account.name.ToPrintable;
  jsonObj.GetAsVariant('type').Value := account.account_type;

end;

class procedure TABEYJSONComp.FillOperationsHashTreeObject(const OperationsHashTree: TOperationsHashTree; jsonObject: TABEYJSONObject);
begin
  jsonObject.GetAsVariant('operations').Value:=OperationsHashTree.OperationsCount;
  jsonObject.GetAsVariant('amount').Value:=ToJSONCurrency(OperationsHashTree.TotalAmount);
  jsonObject.GetAsVariant('fee').Value:=ToJSONCurrency(OperationsHashTree.TotalFee);
  jsonObject.GetAsVariant('rawoperations').Value:=OperationsHashTreeToHexaString(OperationsHashTree);
end;

class procedure TABEYJSONComp.FillMultiOperationObject(current_protocol : Word; const multiOperation: TOpMultiOperation; jsonObject: TABEYJSONObject);
Var i, nSigned, nNotSigned : Integer;
  opht : TOperationsHashTree;
  jsonArr : TABEYJSONArray;
  auxObj : TABEYJSONObject;
begin
  opht := TOperationsHashTree.Create;
  Try
    opht.AddOperationToHashTree(multiOperation);
    jsonObject.GetAsVariant('rawoperations').Value:=OperationsHashTreeToHexaString(opht);
  finally
    opht.Free;
  end;
  nSigned := 0; nNotSigned := 0;
  for i:=0 to High(multiOperation.Data.txSenders) do begin
    If (Length(multiOperation.Data.txSenders[i].Signature.r)>0) And  (Length(multiOperation.Data.txSenders[i].Signature.s)>0) then inc(nSigned)
    else inc(nNotSigned);
  end;
  for i:=0 to High(multiOperation.Data.changesInfo) do begin
    If (Length(multiOperation.Data.changesInfo[i].Signature.r)>0) And  (Length(multiOperation.Data.changesInfo[i].Signature.s)>0) then inc(nSigned)
    else inc(nNotSigned);
  end;
  //
  jsonArr := jsonObject.GetAsArray('senders');
  for i:=Low(multiOperation.Data.txSenders) to High(multiOperation.Data.txSenders) do begin
    auxObj := jsonArr.GetAsObject(jsonArr.Count);
    auxObj.GetAsVariant('account').Value := multiOperation.Data.txSenders[i].Account;
    auxObj.GetAsVariant('n_operation').Value := multiOperation.Data.txSenders[i].N_Operation;
    auxObj.GetAsVariant('amount').Value := ToJSONCurrency(multiOperation.Data.txSenders[i].Amount * (-1));
    auxObj.GetAsVariant('payload').Value := TCrypto.ToHexaString(multiOperation.Data.txSenders[i].Payload);
  end;
  //
  jsonArr := jsonObject.GetAsArray('receivers');
  for i:=Low(multiOperation.Data.txReceivers) to High(multiOperation.Data.txReceivers) do begin
    auxObj := jsonArr.GetAsObject(jsonArr.Count);
    auxObj.GetAsVariant('account').Value := multiOperation.Data.txReceivers[i].Account;
    auxObj.GetAsVariant('amount').Value := ToJSONCurrency(multiOperation.Data.txReceivers[i].Amount);
    auxObj.GetAsVariant('payload').Value := TCrypto.ToHexaString(multiOperation.Data.txReceivers[i].Payload);
  end;
  jsonArr := jsonObject.GetAsArray('changers');
  for i:=Low(multiOperation.Data.changesInfo) to High(multiOperation.Data.changesInfo) do begin
    auxObj := jsonArr.GetAsObject(jsonArr.Count);
    auxObj.GetAsVariant('account').Value := multiOperation.Data.changesInfo[i].Account;
    auxObj.GetAsVariant('n_operation').Value := multiOperation.Data.changesInfo[i].N_Operation;
    If public_key in multiOperation.Data.changesInfo[i].Changes_type then begin
      auxObj.GetAsVariant('new_enc_pubkey').Value := TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(multiOperation.Data.changesInfo[i].New_Accountkey));
    end;
    If account_name in multiOperation.Data.changesInfo[i].Changes_type then begin
      auxObj.GetAsVariant('new_name').Value := multiOperation.Data.changesInfo[i].New_Name.ToPrintable;
    end;
    If account_type in multiOperation.Data.changesInfo[i].Changes_type then begin
      auxObj.GetAsVariant('new_type').Value := multiOperation.Data.changesInfo[i].New_Type;
    end;
  end;
  jsonObject.GetAsVariant('amount').Value:=ToJSONCurrency( multiOperation.OperationAmount );
  jsonObject.GetAsVariant('fee').Value:=ToJSONCurrency( multiOperation.OperationFee );
  // New params for third party signing: (3.0.2)
  if (current_protocol>CT_PROTOCOL_3) then begin
    jsonObject.GetAsVariant('digest').Value:=TCrypto.ToHexaString(multiOperation.GetDigestToSign(current_protocol));
  end;

  jsonObject.GetAsVariant('senders_count').Value:=Length(multiOperation.Data.txSenders);
  jsonObject.GetAsVariant('receivers_count').Value:=Length(multiOperation.Data.txReceivers);
  jsonObject.GetAsVariant('changesinfo_count').Value:=Length(multiOperation.Data.changesInfo);
  jsonObject.GetAsVariant('signed_count').Value:=nSigned;
  jsonObject.GetAsVariant('not_signed_count').Value:=nNotSigned;
  //
  jsonObject.GetAsVariant('signed_can_execute').Value:=(nSigned>0) And (nNotSigned=0);
end;

class procedure TABEYJSONComp.FillPublicKeyObject(const PubKey: TAccountKey; jsonObj: TABEYJSONObject);
begin
  jsonObj.GetAsVariant('ec_nid').Value := PubKey.EC_OpenSSL_NID;
  jsonObj.GetAsVariant('x').Value := TCrypto.ToHexaString(PubKey.x);
  jsonObj.GetAsVariant('y').Value := TCrypto.ToHexaString(PubKey.y);
  jsonObj.GetAsVariant('enc_pubkey').Value := TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(PubKey));
  jsonObj.GetAsVariant('b58_pubkey').Value := TAccountComp.AccountPublicKeyExport(PubKey);
end;

class function TABEYJSONComp.OperationsHashTreeToHexaString(const OperationsHashTree: TOperationsHashTree): String;
var ms : TMemoryStream;
  raw : TRawBytes;
Begin
  ms := TMemoryStream.Create;
  Try
    OperationsHashTree.SaveOperationsHashTreeToStream(ms,false);
    ms.Position := 0;
    SetLength(raw,ms.Size);
    ms.ReadBuffer(raw[Low(raw)],ms.Size);
    Result := TCrypto.ToHexaString(raw);
  Finally
    ms.Free;
  End;
end;

{ TRPCServer }

Procedure TRPCServer.AddRPCLog(Const Sender : String; Const Message : String);
Begin
  If Not Assigned(FRPCLog) then exit;
  FRPCLog.NotifyNewLog(ltinfo,Sender+' '+Inttostr(FCallsCounter),Message);
end;

Function TRPCServer.GetLogFileName : String;
Begin
  If Assigned(FRPCLog) then
    Result := FRPCLog.FileName
  else Result := '';
end;

function TRPCServer.GetNewCallCounter: Int64;
begin
  inc(FCallsCounter);
  Result := FCallsCounter;
end;

procedure TRPCServer.SetActive(AValue: Boolean);
begin
  if FActive=AValue then Exit;
  FActive:=AValue;
  if (FActive) then begin
    FRPCServerThread := TRPCServerThread.Create(FPort);
  end else begin
    FRPCServerThread.Terminate;
    FRPCServerThread.WaitFor;
    FreeAndNil(FRPCServerThread);
  end;
  TLog.NewLog(ltupdate,Classname,'Updated RPC Server to Active='+CT_TRUE_FALSE[FActive]);
end;

procedure TRPCServer.SetIniFileName(const Value: String);
begin
  if FIniFileName=Value then exit;
  FreeAndNil(FIniFile);
  FIniFileName := Value;
  if (FIniFileName<>'') And (FileExists(FIniFileName)) then begin
    FIniFile := TIniFile.Create(FIniFileName);
  end;
  if Assigned(FIniFile) then begin
    FJSON20Strict := FIniFile.ReadBool('general','json20strict',true)
  end;
end;

procedure TRPCServer.SetLogFileName(const Value: String);
begin
  If (Not Assigned(FRPCLog)) And (Trim(Value)<>'') then begin
    FRPCLog := TLog.Create(Nil);
    FRPCLog.ProcessGlobalLogs:=false;
    FRPCLog.SaveTypes:=CT_TLogTypes_ALL;
  end;
  If (trim(Value)<>'') then begin
    FRPCLog.FileName:=Value;
  end else FreeAndNil(FRPCLog);
end;

procedure TRPCServer.SetValidIPs(const Value: String);
begin
  if FValidIPs=Value then exit;
  FValidIPs := Value;
  if FValidIPs='' then begin
    TLog.NewLog(ltupdate,Classname,'Updated RPC Server valid IPs to ALL');
    // New Build 3.0.2
    FAllowUsePrivateKeys := False; // By default, when opening RPC server to all IP's, use of private keys is forbidden to protect server
  end else TLog.NewLog(ltupdate,Classname,'Updated RPC Server valid IPs to: '+FValidIPs)
end;

function TRPCServer.IsValidClientIP(const clientIp: String; clientPort: Word): Boolean;
begin
  if FValidIPs='' then Result := true
  else begin
    Result := pos(clientIP,FValidIPs) > 0;
  end;
end;

constructor TRPCServer.Create;
begin
  FActive := false;
  FRPCLog := Nil;
  FIniFile := Nil;
  FIniFileName := '';
  FJSON20Strict := true;
  FWalletKeys := Nil;
  FRPCServerThread := Nil;
  FPort := CT_JSONRPC_Port;
  FCallsCounter := 0;
  FValidIPs := '127.0.0.1;localhost'; // New Build 1.5 - By default, only localhost can access to RPC
  FAllowUsePrivateKeys := True;       // New Build 3.0.2 - By default RPC allows to use private keys functions
  If Not assigned(_RPCServer) then _RPCServer := Self;
end;

destructor TRPCServer.Destroy;
begin
  FreeAndNil(FRPCLog);
  active := false;
  if _RPCServer=Self then _RPCServer:=Nil;
  inherited Destroy;
end;

{ TRPCProcess }

constructor TRPCProcess.Create(hsock: tSocket);
begin
  FSock:=TTCPBlockSocket.create;
  FSock.socket:=HSock;
  FreeOnTerminate:=true;
  FNode := TNode.Node;
  //Priority:=tpNormal;
  inherited create(True);
  FreeOnTerminate:=true;
  Suspended := False;
end;

destructor TRPCProcess.Destroy;
begin
  FSock.free;
  inherited Destroy;
end;

procedure TRPCProcess.BCExecute;
var
  timeout: integer;
  s: string;
  method, uri, protocol: string;
  size: integer;
  x, n: integer;
  resultcode: integer;
  inputdata : TRawBytes;
  js,jsresult : TABEYJSONData;
  jsonobj,jsonresponse : TABEYJSONObject;
  errNum : Integer; errDesc : String;
  jsonrequesttxt,
  jsonresponsetxt, methodName, paramsTxt : String;
  valid : Boolean;
  i : Integer;
  Headers : TStringList;
  tc : TTickCount;
  callcounter : Int64;
  Buffer: PChar;
begin
  callcounter := _RPCServer.GetNewCallCounter;
  tc := TPlatform.GetTickCount;
  methodName := '';
  paramsTxt := '';
  // IP Protection
  If (Not _RPCServer.IsValidClientIP(FSock.GetRemoteSinIP,FSock.GetRemoteSinPort)) then begin
    TLog.NewLog(lterror,Classname,FSock.GetRemoteSinIP+':'+inttostr(FSock.GetRemoteSinPort)+' INVALID IP');
    _RPCServer.AddRPCLog(FSock.GetRemoteSinIP+':'+InttoStr(FSock.GetRemoteSinPort),' INVALID IP');
    exit;
  end;
  errNum := CT_RPC_ErrNum_InternalError;
  errDesc := 'No data';
  valid := false;
  SetLength(inputdata,0);
  Headers := TStringList.Create;
  jsonresponse := TABEYJSONObject.Create;
  try
    timeout := 5000;
    resultcode:= 200;
    repeat
      //read request line
      s := Fsock.RecvString(timeout);
      if Fsock.lasterror <> 0 then Exit;
      if s = '' then Exit;
      method := Fetch(s, ' ');
      if (s = '') or (method = '') then  Exit;
      uri := Fetch(s, ' '); if uri = '' then  Exit;
      protocol := Fetch(s, ' ');
      headers.Clear;
      size := -1;
      //read request headers
      if protocol <> '' then begin
        if protocol.IndexOf('HTTP/1.1')<>0 then begin
          errDesc := 'Invalid protocol '+protocol;
          Exit;
        end;
        repeat
          s := Fsock.RecvString(Timeout);
          if Fsock.lasterror <> 0 then
            Exit;
          s := UpperCase(s);
          if s.IndexOf('CONTENT-LENGTH:')=0 then
            Size := StrToIntDef(SeparateRight(s, ' '), -1);
        until s = '';
      end;
      //recv document...
      if size >= 0 then begin
        setLength(inputdata,size);
        x := FSock.RecvBufferEx(InputData, Size, Timeout);
        if Fsock.lasterror <> 0 then
          Exit;
        if (x<>size) And (x>0) then
          setLength(inputdata,x);
      end else setlength(inputdata,0);
      jsonrequesttxt := inputdata.ToString;
      // Convert InputData to JSON object
      try
        js := TABEYJSONData.ParseJSONValue(jsonrequesttxt);
      except
        On E:Exception do begin
          errDesc:='Error decoding JSON: '+E.Message;
          TLog.NewLog(lterror,Classname,FSock.GetRemoteSinIP+':'+inttostr(FSock.GetRemoteSinPort)+' Error decoding JSON: '+E.Message);
          exit;
        end;
      end;
      If Assigned(js) then begin
        try
          If (js is TABEYJSONObject) then begin
            jsonobj := TABEYJSONObject(js);
            errNum := 0;
            errDesc := '';
            try
              methodName := jsonobj.AsString('method','');
              paramsTxt := jsonobj.GetAsObject('params').ToJSON(false);
              {$IFDEF HIGHLOG}TLog.NewLog(ltinfo,Classname,FSock.GetRemoteSinIP+':'+inttostr(FSock.GetRemoteSinPort)+' Processing method '+methodName+' params '+paramsTxt);{$ENDIF}
              Valid := ProcessMethod(methodName,jsonobj.GetAsObject('params'),jsonresponse,errNum,errDesc);
              if not Valid then begin
                if (errNum<>0) or (errDesc<>'') then begin
                  jsonresponse.GetAsObject('error').GetAsVariant('code').Value:=errNum;
                  jsonresponse.GetAsObject('error').GetAsVariant('message').Value:=errDesc;
                end else begin
                  jsonresponse.GetAsObject('error').GetAsVariant('code').Value:=CT_RPC_ErrNum_InternalError;
                  jsonresponse.GetAsObject('error').GetAsVariant('message').Value:='Unknown error processing method';
                end;
              end;
            Except
              on E:Exception do begin
                TLog.NewLog(lterror,Classname,'Exception processing method'+methodName+' ('+E.ClassName+'): '+E.Message );
                jsonresponse.GetAsObject('error').GetAsVariant('code').Value:=CT_RPC_ErrNum_InternalError;
                jsonresponse.GetAsObject('error').GetAsVariant('message').Value:=E.Message;


                ShowException(ExceptObject,ExceptAddr);
                valid:=False;
              end;
            end;
            jsonresponse.GetAsVariant('id').Value:= jsonobj.GetAsVariant('id').Value;
            jsonresponse.GetAsVariant('jsonrpc').Value:= '2.0';
          end;
        finally
          js.free;
        end;
      end else begin
        TLog.NewLog(lterror,ClassName,FSock.GetRemoteSinIP+':'+inttostr(FSock.GetRemoteSinPort)+' Received data is not a JSON: '+jsonrequesttxt+' (length '+inttostr(length(jsonrequesttxt))+' bytes)');
      end;
    until (FSock.LastError <> 0) Or (protocol<>'');
  Finally
    try
      // Send result:
      if Fsock.lasterror = 0 then begin
        // Save JSON response:
        If (Not Valid) then begin
          if Not assigned(jsonresponse.FindName('error')) then begin
            jsonresponse.GetAsObject('error').GetAsVariant('code').Value:=errNum;
            jsonresponse.GetAsObject('error').GetAsVariant('message').Value:=errDesc;
          end;
        end;
        jsonresponsetxt := jsonresponse.ToJSON(false);
        Fsock.SendString(protocol + ' ' + IntTostr(ResultCode) + CRLF);
        if (protocol <> '') then begin
          headers.Add('Server: ABEY HTTP JSON-RPC Server');
          headers.Add('Content-Type: application/json;charset=utf-8');
          headers.Add('Content-length: ' + IntTostr(length(jsonresponsetxt)));
          headers.Add('Connection: close');
          headers.Add('Access-Control-Allow-Origin: *');
          headers.Add('Date: ' + Rfc822DateTime(now));
          headers.Add('');
          for n := 0 to headers.count - 1 do
            Fsock.sendstring(headers[n] + CRLF);
        end;
        if Fsock.lasterror = 0 then begin
          FSock.SendString(jsonresponsetxt);
        end;
      end;
      _RPCServer.AddRPCLog(FSock.GetRemoteSinIP+':'+InttoStr(FSock.GetRemoteSinPort),'Method:'+methodName+' Params:'+paramsTxt+' '+Inttostr(errNum)+':'+errDesc+' Time:'+FormatFloat('0.000',(TPlatform.GetElapsedMilliseconds(tc)/1000)));
    finally
      jsonresponse.free;
      Headers.Free;
    end;
  end;
end;

function TRPCProcess.ProcessMethod(const method: String; params: TABEYJSONObject;
  jsonresponse: TABEYJSONObject; var ErrorNum: Integer; var ErrorDesc: String): Boolean;
  var _ro : TABEYJSONObject;
      _ra : TABEYJSONArray;
      errors:String;
  Function GetResultObject : TABEYJSONObject;
  begin
    if not assigned(_ro) then begin
      _ro := jsonresponse.GetAsObject('result');
      _ra := Nil;
    end;
    Result := _ro;
  end;

  Function GetResultArray : TABEYJSONArray;
  begin
    if not assigned(_ra) then begin
      _ra := jsonresponse.GetAsArray('result');
      _ro := Nil;
    end;
    Result := _ra;
  end;

  Function ToJSONCurrency(pascalCoins : Int64) : Real;
  Begin
    Result := RoundTo( pascalCoins / 10000 , -4);
  End;

  Function ToABEYs(jsonCurr : Real) : Int64;
  Begin
    Result := Round(jsonCurr * 10000);
  End;

  Function HexaStringToOperationsHashTree(Const HexaStringOperationsHashTree : String; out OperationsHashTree : TOperationsHashTree; var errors : String) : Boolean;
  var raw : TRawBytes;
    ms : TMemoryStream;
  Begin
    Result := False;
    raw := TCrypto.HexaToRaw(HexaStringOperationsHashTree);
    if (HexaStringOperationsHashTree<>'') And (Length(raw)=0) then begin
      errors := 'Invalid HexaString as operations';
      exit;
    end;
    ms := TMemoryStream.Create;
    Try
      ms.WriteBuffer(raw[Low(raw)],Length(raw));
      ms.Position := 0;
      OperationsHashTree := TOperationsHashTree.Create;
      if (Length(raw)>0) then begin
        If not OperationsHashTree.LoadOperationsHashTreeFromStream(ms,false,CT_PROTOCOL_1,Nil,errors) then begin
          FreeAndNil(OperationsHashTree);
          exit;
        end;
      end;
      Result := true;
    Finally
      ms.Free;
    End;
  End;

  Function HexaStringToOperationsHashTreeAndGetMultioperation(Const HexaStringOperationsHashTree : String; canCreateNewOne : Boolean; out OperationsHashTree : TOperationsHashTree; out multiOperation : TOpMultiOperation; var errors : String) : Boolean;
    { This function will return true only if HexaString contains only 1 operation and is a multioperation.
      Also, if "canCreateNewOne" is true and has no operations, then will create new one and return True
      }
  var op : TABEYOperation;
  Begin
    multiOperation := Nil;
    Result := HexaStringToOperationsHashTree(HexaStringOperationsHashTree,OperationsHashTree,errors);
    If (Result) then begin
      Try
        If (OperationsHashTree.OperationsCount=0) And (canCreateNewOne) then begin
          multiOperation := TOpMultiOperation.Create;
          OperationsHashTree.AddOperationToHashTree(multiOperation);
          multiOperation.Free;
          multiOperation := OperationsHashTree.GetOperation(0) as TOpMultiOperation;
        end else if (OperationsHashTree.OperationsCount=1) then begin
          op := OperationsHashTree.GetOperation(0);
          if (op is TOpMultiOperation) then multiOperation := op as TOpMultiOperation
          else errors := 'operation is not a multioperation';
        end else errors := 'No multioperation found';
      finally
        If (Not Assigned(multiOperation)) then begin
          FreeAndNil(OperationsHashTree);
          Result := false;
        end;
      end;
    end;
  End;

  Function GetBlock(nBlock : Cardinal; jsonObject : TABEYJSONObject) : Boolean;
  begin
    If FNode.Bank.BlocksCount<=nBlock then begin
      ErrorNum := CT_RPC_ErrNum_InvalidBlock;
      ErrorDesc := 'Cannot load Block: '+IntToStr(nBlock);
      Result := False;
      Exit;
    end;
    TABEYJSONComp.FillBlockObject(nBlock,FNode,jsonObject);
    Result := True;
  end;

  Procedure FillOperationResumeToJSONObject(Const OPR : TOperationResume; jsonObject : TABEYJSONObject);
  Begin
    TABEYJSONComp.FillOperationObject(OPR,FNode.Bank.BlocksCount,jsonObject);
  end;

  Function GetAccountOperations(accountNumber : Cardinal; jsonArray : TABEYJSONArray; maxBlocksDepth, startReg, maxReg: Integer; forceStartBlock : Cardinal) : Boolean;
  var list : TList<Cardinal>;
    Op : TABEYOperation;
    OPR : TOperationResume;
    Obj : TABEYJSONObject;
    OperationsResume : TOperationsResumeList;
    i, nCounter : Integer;
    LLockedMempool : TABEYOperationsComp;
  Begin
    Result := false;
    if (startReg<-1) or (maxReg<=0) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Invalid start or max value';
      Exit;
    end;
    nCounter := 0;
    OperationsResume := TOperationsResumeList.Create;
    try
      if ((startReg=-1) And (forceStartBlock=0)) then begin
        // 1.5.5 change: If start=-1 then will include PENDING OPERATIONS, otherwise not.
        // Only will return pending operations if start=0, otherwise
        list := TList<Cardinal>.Create;
        Try
          LLockedMempool := FNode.LockMempoolRead;
          try
            LLockedMempool.OperationsHashTree.GetOperationsAffectingAccount(accountNumber,list);
            for i := list.Count - 1 downto 0 do begin
              Op := LLockedMempool.OperationsHashTree.GetOperation(list[i]);
              If TABEYOperation.OperationToOperationResume(0,Op,False,accountNumber,OPR) then begin
                OPR.NOpInsideBlock := i;
                OPR.Block := LLockedMempool.OperationBlock.block;
                OPR.Balance := LLockedMempool.VaultTransaction.Account(accountNumber).balance;
                if (nCounter>=startReg) And (nCounter<maxReg) then begin
                  OperationsResume.Add(OPR);
                end;
                inc(nCounter);
              end;
            end;
          finally
            FNode.UnlockMempoolRead;
          end;
        Finally
          list.Free;
        End;
      end;
      if (nCounter<maxReg) then begin
        if (startReg<0) then startReg := 0; // Prevent -1 value
        FNode.GetStoredOperationsFromAccount(OperationsResume,accountNumber,maxBlocksDepth,startReg,startReg+maxReg-1,forceStartBlock);
      end;
      for i:=0 to OperationsResume.Count-1 do begin
        Obj := jsonArray.GetAsObject(jsonArray.Count);
        OPR := OperationsResume[i];
        FillOperationResumeToJSONObject(OPR,Obj);
      end;
      Result := True;
    finally
      OperationsResume.Free;
    end;
  end;

  Procedure GetConnections;
  var i : Integer;
    l : TList<TNetConnection>;
    nc : TNetConnection;
    obj: TABEYJSONObject;
  Begin
    l := TNetData.NetData.NetConnections.LockList;
    try
      for i:=0 to l.Count-1 do begin
        nc := TNetData.NetData.Connection(i);
        obj := jsonresponse.GetAsArray('result').GetAsObject(i);
        obj.GetAsVariant('server').Value := Not (nc is TNetServerClient);
        obj.GetAsVariant('ip').Value:=nc.Client.RemoteHost;
        obj.GetAsVariant('port').Value:=nc.Client.RemotePort;
        obj.GetAsVariant('secs').Value:=UnivDateTimeToUnix(now) - UnivDateTimeToUnix(nc.CreatedTime);
        obj.GetAsVariant('sent').Value:=nc.Client.BytesSent;
        obj.GetAsVariant('recv').Value:=nc.Client.BytesReceived;
        obj.GetAsVariant('appver').Value:=nc.ClientAppVersion;
        obj.GetAsVariant('netver').Value:=nc.NetProtocolVersion.protocol_version;
        obj.GetAsVariant('netver_a').Value:=nc.NetProtocolVersion.protocol_available;
        obj.GetAsVariant('timediff').Value:=nc.TimestampDiff;
      end;
    finally
      TNetData.NetData.NetConnections.UnlockList;
    end;
  end;

  Procedure Get_node_ip_stats;
  var aip : String;
    i : Integer;
    json, newJson : TABEYJSONObject;
    ipInfo : TIpInfo;
    aDisconnectedOnly : Boolean;
    LShowDetailedStats : Boolean;
  begin
    if params.AsBoolean('clean',False) then begin
      GetResultObject.GetAsVariant('cleaned').Value := TNetData.NetData.IpInfos.CleanLastStats;
      Exit;
    end;
    if params.AsBoolean('clear',False) then begin
      GetResultObject.GetAsVariant('cleared').Value := TNetData.NetData.IpInfos.Count;
      TNetData.NetData.IpInfos.Clear;
      Exit;
    end;
    LShowDetailedStats := params.AsBoolean('detailed-stats',False);
    aip := Trim(params.AsString('ip',''));
    if aip<>'' then begin
      json := TNetData.NetData.IpInfos.Lock(aip,False);
      Try
        newJson := TABEYJSONObject.Create;
        newJson.GetAsVariant('ip').Value := aip;
        if LShowDetailedStats then begin
          newJson.GetAsObject('values').Assign(json);
        end;
        GetResultArray.Insert(GetResultArray.Count,newJson);
      Finally
        TNetData.NetData.IpInfos.Unlock;
      End;
    end else begin
      aDisconnectedOnly := params.AsBoolean('only-disconnected',False);
      for i :=0 to TNetData.NetData.IpInfos.Count-1 do begin
        ipInfo := TNetData.NetData.IpInfos.Lock(i);
        Try
          if (Not aDisconnectedOnly) Or (Assigned(ipInfo.json.FindName('disconnect'))) then begin
            newJson := TABEYJSONObject.Create;
            newJson.GetAsVariant('ip').Value := ipInfo.ip;
            if LShowDetailedStats then begin
              newJson.GetAsObject('values').Assign(ipInfo.json);
            end;
            GetResultArray.Insert(GetResultArray.Count,newJson);
          end;
        Finally
          TNetData.NetData.IpInfos.Unlock;
        End;
      end;
    end;
  end;

  Function CheckAndGetPrivateKeyInWallet(const publicKey : TAccountKey; var privateKey : TECPrivateKey) : Boolean;
  Var i : Integer;
    f_raw : TRawBytes;
  Begin
    Result := False;
    privateKey := Nil;
    i := _RPCServer.FWalletKeys.IndexOfAccountKey(publicKey);
    if i<0 then begin
      ErrorDesc:='Sender Public Key not found in wallet: '+TAccountComp.AccountPublicKeyExport(publicKey);
      ErrorNum:=CT_RPC_ErrNum_InvalidPubKey;
      Exit;
    end;
    privateKey := _RPCServer.FWalletKeys.Key[i].PrivateKey;
    if (Not assigned(privateKey)) then begin
      if Length(_RPCServer.FWalletKeys.Key[i].CryptedKey)>0 then begin
        // Wallet is password protected
        ErrorDesc := 'Wallet is password protected';
        ErrorNum := CT_RPC_ErrNum_WalletPasswordProtected;
      end else begin
        ErrorDesc := 'Wallet private key not found in Wallet';
        ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
      end;
    end else Result := True;
  End;

  Function CheckAndGetEncodedRAWPayload(Const RawPayload : TRawBytes; Const Payload_method, EncodePwdForAES : String; const senderAccounKey, targetAccountKey : TAccountKey; var EncodedRAWPayload : TRawBytes) : Boolean;
  begin
    Result := False;
    if (length(RawPayload)>0) then begin
      if (Payload_method='none') then EncodedRAWPayload:=RawPayload
      else if (Payload_method='dest') then begin
        TABEYEncryption.DoABEYECIESEncrypt(targetAccountKey,RawPayload,EncodedRAWPayload);
      end else if (Payload_method='sender') then begin
        TABEYEncryption.DoABEYECIESEncrypt(senderAccounKey,RawPayload,EncodedRAWPayload);
      end else if (Payload_method='aes') then begin
        EncodedRAWPayload := TABEYEncryption.DoABEYAESEncrypt(RawPayload,TEncoding.ANSI.GetBytes(EncodePwdForAES));
      end else begin
        ErrorNum:=CT_RPC_ErrNum_InvalidOperation;
        ErrorDesc:='Invalid encode payload method: '+Payload_method;
        exit;
      end;
    end else EncodedRAWPayload := Nil;
    Result := True;
  end;

  // This function creates a TOpTransaction without looking for balance/private key of sender account
  // It assumes that sender,target,sender_last_n_operation,senderAccountKey and targetAccountKey are correct
  Function CreateOperationTransaction(current_protocol : Word; sender, target, sender_last_n_operation : Cardinal; amount, fee : UInt64;
    Const senderAccounKey, targetAccountKey : TAccountKey; Const RawPayload : TRawBytes;
    Const Payload_method, EncodePwd : String) : TOpTransaction;
  // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
  Var f_raw : TRawBytes;
    privateKey : TECPrivateKey;
  Begin
    Result := Nil;
    if Not CheckAndGetPrivateKeyInWallet(senderAccounKey,privateKey) then Exit(Nil);
    if Not CheckAndGetEncodedRAWPayload(RawPayload,Payload_method,EncodePwd,senderAccounKey,targetAccountKey,f_raw) then Exit(Nil);
    Result := TOpTransaction.CreateTransaction(current_protocol, sender,sender_last_n_operation+1,target,privateKey,amount,fee,f_raw);
    if Not Result.HasValidSignature then begin
      FreeAndNil(Result);
      ErrorNum:=CT_RPC_ErrNum_InternalError;
      ErrorDesc:='Invalid signature';
      exit;
    end;
  End;

  Function OpSendTo(sender, target : Cardinal; amount, fee : UInt64; Const RawPayload : TRawBytes; Const Payload_method, EncodePwd : String) : Boolean;
  // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
  Var opt : TOpTransaction;
    sacc,tacc : TAccount;
    errors : String;
    opr : TOperationResume;
  begin
    FNode.OperationSequenceLock.Acquire;  // Use lock to prevent N_Operation race-condition on concurrent sends
    try
      Result := false;
      if (sender<0) or (sender>=FNode.Bank.AccountsCount) then begin
        If (sender=CT_MaxAccount) then ErrorDesc := 'Need sender'
        else ErrorDesc:='Invalid sender account '+Inttostr(sender);
        ErrorNum:=CT_RPC_ErrNum_InvalidAccount;
        Exit;
      end;
      if (target<0) or (target>=FNode.Bank.AccountsCount) then begin
        If (target=CT_MaxAccount) then ErrorDesc := 'Need target'
        else ErrorDesc:='Invalid target account '+Inttostr(target);
        ErrorNum:=CT_RPC_ErrNum_InvalidAccount;
        Exit;
      end;
      sacc := FNode.GetMempoolAccount(sender);
      tacc := FNode.GetMempoolAccount(target);

      opt := CreateOperationTransaction(FNode.Bank.Vault.CurrentProtocol,sender,target,sacc.n_operation,amount,fee,sacc.accountInfo.accountKey,tacc.accountInfo.accountKey,RawPayload,Payload_method,EncodePwd);
      if opt=nil then exit;
      try
        If not FNode.AddOperation(Nil,opt,errors) then begin
          ErrorDesc := 'Error adding operation: '+errors;
          ErrorNum := CT_RPC_ErrNum_InvalidOperation;
          Exit;
        end;
        TABEYOperation.OperationToOperationResume(0,opt,False,sender,opr);
        FillOperationResumeToJSONObject(opr,GetResultObject);
        Result := true;
      finally
        opt.free;
      end;
    finally
      FNode.OperationSequenceLock.Release;
    end;
  end;

  Function SignOpSendTo(Const HexaStringOperationsHashTree : String; current_protocol : Word;
    sender, target : Cardinal;
    Const senderAccounKey, targetAccountKey : TAccountKey;
    last_sender_n_operation : Cardinal;
    amount, fee : UInt64; Const RawPayload : TRawBytes; Const Payload_method, EncodePwd : String) : Boolean;
  // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
  var OperationsHashTree : TOperationsHashTree;
    errors : String;
    opt : TOpTransaction;
  begin
    Result := false;
    if Not HexaStringToOperationsHashTree(HexaStringOperationsHashTree,OperationsHashTree,errors) then begin
      ErrorNum:=CT_RPC_ErrNum_InvalidData;
      ErrorDesc:= 'Error decoding param "rawoperations": '+errors;
      Exit;
    end;
    Try
      opt := CreateOperationTransaction(current_protocol, sender,target,last_sender_n_operation,amount,fee,senderAccounKey,targetAccountKey,RawPayload,Payload_method,EncodePwd);
      if opt=nil then exit;
      try
        OperationsHashTree.AddOperationToHashTree(opt);
        TABEYJSONComp.FillOperationsHashTreeObject(OperationsHashTree,GetResultObject);
        Result := true;
      finally
        opt.Free;
      end;
    Finally
      OperationsHashTree.Free;
    End;
  end;

  // This function creates a TOpChangeKey without looking for private key of account
  // It assumes that account_signer,account_last_n_operation, account_target and account_pubkey are correct
  Function CreateOperationChangeKey(current_protocol : Word; account_signer, account_last_n_operation, account_target : Cardinal; const account_pubkey, new_pubkey : TAccountKey; fee : UInt64; RawPayload : TRawBytes; Const Payload_method, EncodePwd : String) : TOpChangeKey;
  // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
  var
    errors : String;
    f_raw : TRawBytes;
    privateKey : TECPrivateKey;
  Begin
    Result := Nil;
    if Not CheckAndGetPrivateKeyInWallet(account_pubkey,privateKey) then Exit(Nil);
    if Not CheckAndGetEncodedRAWPayload(RawPayload,Payload_method,EncodePwd,account_pubkey,new_pubkey,f_raw) then Exit(Nil);
    If account_signer=account_target then begin
      Result := TOpChangeKey.Create(current_protocol,account_signer,account_last_n_operation+1,account_target,privateKey,new_pubkey,fee,f_raw);
    end else begin
      Result := TOpChangeKeySigned.Create(current_protocol,account_signer,account_last_n_operation+1,account_target,privateKey,new_pubkey,fee,f_raw);
    end;
    if Not Result.HasValidSignature then begin
      FreeAndNil(Result);
      ErrorNum:=CT_RPC_ErrNum_InternalError;
      ErrorDesc:='Invalid signature';
      exit;
    end;
  End;

  Function ChangeAccountKey(account_signer, account_target : Cardinal; const new_pub_key : TAccountKey; fee : UInt64; const RawPayload : TRawBytes; Const Payload_method, EncodePwd : String) : Boolean;
  // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
  Var opck : TOpChangeKey;
    acc_signer : TAccount;
    errors : String;
    opr : TOperationResume;
  begin
    FNode.OperationSequenceLock.Acquire;  // Use lock to prevent N_Operation race-condition on concurrent invocations
    try
      Result := false;
      if (account_signer<0) or (account_signer>=FNode.Bank.AccountsCount) then begin
        ErrorDesc:='Invalid account '+Inttostr(account_signer);
        ErrorNum:=CT_RPC_ErrNum_InvalidAccount;
        Exit;
      end;
      acc_signer := FNode.GetMempoolAccount(account_signer);

      opck := CreateOperationChangeKey(FNode.Bank.Vault.CurrentProtocol,account_signer,acc_signer.n_operation,account_target,acc_signer.accountInfo.accountKey,new_pub_key,fee,RawPayload,Payload_method,EncodePwd);
      if not assigned(opck) then exit;
      try
        If not FNode.AddOperation(Nil,opck,errors) then begin
          ErrorDesc := 'Error adding operation: '+errors;
          ErrorNum := CT_RPC_ErrNum_InvalidOperation;
          Exit;
        end;
        TABEYOperation.OperationToOperationResume(0,opck,False,account_signer,opr);
        FillOperationResumeToJSONObject(opr,GetResultObject);
        Result := true;
      finally
        opck.free;
      end;
    finally
      FNode.OperationSequenceLock.Release;
    end;
  end;

  //helper functions for mask in save contract
  procedure ClearBit(var Value: Cardinal; Index: Byte);
  begin
    Value := Value and ((Cardinal(1) shl Index) xor High(Cardinal));
  end;

  procedure SetBit(var Value: Cardinal; Index: Byte);
  begin
    Value:=  Value or (Cardinal(1) shl Index);
  end;

  procedure PutBit(var Value: Cardinal; Index: Byte; State: Boolean);
  begin
    Value := (Value and ((Cardinal(1) shl Index) xor High(Cardinal))) or (Cardinal(State) shl Index);
  end;

  function GetBit(Value: Cardinal; Index: Byte): Boolean;
  begin
    Result := ((Value shr Index) and 1) = 1;
  end;

  Function CreateOperationSaveContract(current_protocol : Word; account_signer, account_last_n_operation, account_target : Cardinal; const account_pubkey : TAccountKey; fee : UInt64;
                                        startBlockFlag: Boolean; startBlocks, activeBlocks, passiveBlocks, instructionCount, memoryUsage: Cardinal;
                                        runtimeSeconds, ioUsage, dataSegSize, exportTableSize, bytecodeSize : Cardinal; exportTableFilePath, rawPayloadFilePath : String;
                                        Const Payload_method, EncodePwd : String) : TOpSaveContract;   // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
  var
    errors : String;
    f_raw : TRawBytes;
    export_table_raw : TRawBytes;
    privateKey : TECPrivateKey;
    paramsTableAndBytecode : TRawBytes;
    i : integer;
    flags : Cardinal;
    rawFlags : TRawBytes absolute flags;
    rawStartBlocks: TRawBytes absolute startBlocks;
    rawActiveBlocks: TRawBytes absolute activeBlocks;
    rawPassiveBlocks: TRawBytes absolute passiveBlocks;
    rawInstructionCount: TRawBytes absolute instructionCount;
    rawMemoryUsage: TRawBytes absolute memoryUsage;
    rawRuntimeSeconds: TRawBytes absolute runtimeSeconds;
    rawIoUsage: TRawBytes absolute ioUsage;
    rawDataSegSize: TRawBytes absolute dataSegSize;
    rawExportTableSize: TRawBytes absolute exportTableSize;
    rawBytecodeSize: TRawBytes absolute bytecodeSize;
    memoryStreamForFile, memoryStreamForSecondFile, memoryStreamForThirdFile : TMemoryStream;
    bufferForBytes, anotherBufferForBytes, thirdBuffer : TRawBytes;
    exportTableFileSize, bytecodeFileSize, numberOfBytesRead, datasegmentFileSize : UInt64;
    PathToDataSegment, PathToQ3VM : String;
    Q3VMProcess : TProcess;
    OutputStream : TStream;
    BytesRead    : longint;
    Buffer       : array[1..2048] of byte;
  Begin
    ReturnValue := 0;
    DebugLn('URPC.pas CreateOperationSaveContract started');
    Result := Nil;
    if Not CheckAndGetPrivateKeyInWallet(account_pubkey,privateKey) then Exit(Nil);

    memoryStreamForFile := TMemoryStream.Create;
    //try to get data from files and add to payload
    try
      memoryStreamForFile.LoadFromFile(exportTableFilePath);
      exportTableFileSize := memoryStreamForFile.size;
      DebugLn('#################################');
      DebugLn(Format('Export table file size = %d ', [exportTableFileSize]));
      DebugLn('#################################');
      memoryStreamForFile.seek(0, soFromBeginning);
      SetLength(bufferForBytes, exportTableFileSize);
      numberOfBytesRead := memoryStreamForFile.Read(Pointer(bufferForBytes)^, exportTableFileSize);
      DebugLn('#################################');
      DebugLn(Format('Actual number of bytes from export table file = %d ', [numberOfBytesRead]));
      DebugLn('#################################');
    except
    on E:Exception do
      TLog.NewLog(lterror,'TRPCProcess','Error loading export table from file, exception message: ' + E.message);
    end;

    memoryStreamForSecondFile := TMemoryStream.Create;
    try
       memoryStreamForSecondFile.LoadFromFile(rawPayloadFilePath);
       bytecodeFileSize := memoryStreamForSecondFile.size;
       DebugLn('#################################');
       DebugLn(Format('Bytecode file size = %d ', [bytecodeFileSize]));
       DebugLn('#################################');
       SetLength(anotherBufferForBytes, bytecodeFileSize);
       memoryStreamForSecondFile.seek(0, sofromBeginning);
       numberOfBytesRead := memoryStreamForSecondFile.Read(Pointer(anotherBufferForBytes)^, bytecodeFileSize);
       DebugLn('#################################');
       DebugLn(Format('Actual number of bytes from bytecode file = %d ', [numberOfBytesRead]));
       DebugLn('#################################');
    except
    on E:Exception do
      TLog.NewLog(lterror,'TRPCProcess','Error loading bytecode from file, exception message: '+ E.message);
    end;

    //Debugln(Format('Only export table, length = %d',[Length(bufferForBytes)]));
    //for i := Low(bufferForBytes) to High(bufferForBytes) do begin
    //  Debugln('%d ',[bufferForBytes[i]]);
    //end;
    //
    //Debugln(Format('Only payload, aka just Bytecode = %d', [Length(anotherBufferForBytes)]));
    //for i := Low(anotherBufferForBytes) to High(anotherBufferForBytes) do begin
    //  Debugln('%d ',[anotherBufferForBytes[i]]);
    //end;
    //
    //DebugLn(' CreateOperationSaveContract Active blocks = %d', [activeBlocks]);
    //DebugLn('CreateOperationSaveContract export table size = %d', [exportTableSize]);
    //DebugLn('CreateOperationSaveContract bytecode length = %d', [Length(RawPayload)]);
    //DebugLn('CreateOperationSaveContract length of export table = %d', [Length(rawExportTable)]);
    //DebugLn('CreateOperationSaveContract startBlocks = %d', [startBlocks]);
    //DebugLn('CreateOperationSaveContract length of payload/bytecode = %d', [Length(f_raw)]);
    //44 bytes for parameters  4 bytes for each
    //flag
    //start block number
    //active blocks
    //passive blocks
    //max instruction count
    //max memory usage
    //max runtime seconds
    //max io usage
    //max data segment size
    //exporttable size
    //bytecode size
    // actual raw exporttable
    // actual raw bytecode
    // actual raw datasegment
    SetLength(paramsTableAndBytecode, 44);
    flags := Cardinal(0);
    if(startBlockFlag)then
      SetBit(flags, 31);
    move(rawFlags, paramsTableAndByteCode[Low(paramsTableAndByteCode)],4);
    move(rawStartBlocks, paramsTableAndByteCode[Low(paramsTableAndByteCode)+4],4);
    move(rawActiveBlocks, paramsTableAndByteCode[Low(paramsTableAndByteCode)+8],4);
    move(rawPassiveBlocks, paramsTableAndByteCode[Low(paramsTableAndByteCode)+12],4);
    move(rawInstructionCount, paramsTableAndByteCode[Low(paramsTableAndByteCode)+16],4);
    move(rawMemoryUsage, paramsTableAndByteCode[Low(paramsTableAndByteCode)+20],4);
    move(rawRuntimeSeconds, paramsTableAndByteCode[Low(paramsTableAndByteCode)+24],4);
    move(rawIoUsage, paramsTableAndByteCode[Low(paramsTableAndByteCode)+28],4);
    move(rawDataSegSize, paramsTableAndByteCode[Low(paramsTableAndByteCode)+32],4);
    move(rawExportTableSize, paramsTableAndByteCode[Low(paramsTableAndByteCode)+36],4);
    move(rawBytecodeSize, paramsTableAndByteCode[Low(paramsTableAndByteCode)+40],4);
    Debugln('before adding export table data');
    paramsTableAndBytecode.Add(bufferForBytes);
    Debugln('before adding bytecode, but after added export table data');
    paramsTableAndBytecode.Add(anotherBufferForBytes);
    Debugln('after adding bytecode and exportabe data');

    PathToDataSegment := TFolderHelper.GetABEYDataFolder() + PathDelim + 'out' + PathDelim + 'data_segment.bin';


    //Execute contract with initVM to get real data_segment
    //IFDEF to be moved to UConst
    {$IFDEF WINDOWS}
    PathToQ3VM := GetCurrentDir + PathDelim + 'q3vm.exe';
    {$ELSE}  //Linux + Mac
    PathToQ3VM := GetCurrentDir + PathDelim + 'q3vm';
    //do error checking, lacking to q3vm
    {$ENDIF WINDOWS}

    Debugln('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!');
    Debugln('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!');
    Debugln('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!');
    DebugLn('q3vm path = ' + PathToQ3VM);
    DebugLn('raw bytecode path = ' + rawPayloadFilePath);
    DebugLn('Path to data segment = ' + PathToDataSegment);
    DebugLn('Path to export table = ' + exportTableFilePath);
    DebugLn('instruction count = ' + IntToStr(instructionCount));
    //set up process
    Q3VMProcess := TProcess.Create(nil);
    Q3VMProcess.Executable := PathToQ3VM;
    Q3VMProcess.Parameters.Add('--bytecodePath');
    Q3VMProcess.Parameters.Add(rawPayloadFilePath);
    Q3VMProcess.Parameters.Add('--snapshotPath');
    Q3VMProcess.Parameters.Add(PathToDataSegment);
    Q3VMProcess.Parameters.Add('--exportTablePath');
    Q3VMProcess.Parameters.Add(ExportTableFilePath);
    Q3VMProcess.Parameters.Add('--instructionCountLimit');
    Q3VMProcess.Parameters.Add(IntToStr(instructionCount));
    Q3VMProcess.Parameters.Add('--functionName');
    Q3VMProcess.Parameters.Add('initVM');

    Q3VMProcess.Options := Q3VMProcess.Options + [poUsePipes] + [poStderrToOutPut];

    DebugLn('Start contract execution');
    Q3VMProcess.Execute;
    DebugLn('End of contract execution');

    OutputStream := TMemoryStream.Create;
    repeat
      BytesRead := Q3vmProcess.Output.Read(Buffer, 2048);
      OutputStream.Write(Buffer, BytesRead);
    until Not(Q3VMProcess.Running);

    if (Q3VMProcess.ExitStatus <> 0) then
    begin
      //error happened
      DebugLn('Test1');
      ErrorDesc := 'Error during execution of contract : ';
      with TStringList.Create do
      begin
        OutputStream.Position := 0;
        LoadFromStream(OutputStream);
        ErrorDesc := ErrorDesc + ' ' + Text;
      end;

      ErrorNum:= CT_RPC_ErrNum_ErrorDuringFirstExecutionOfContract;
      DebugLn('Test2');
      Exit;
    end
    else
    begin
      DebugLn('Output of contract:');
      with TStringList.Create do
      begin
        OutputStream.Position := 0;
        LoadFromStream(OutputStream);
        DebugLn(Text);
      end;
    end;

    memoryStreamForThirdFile := TMemoryStream.Create;
    memoryStreamForThirdFile.LoadFromFile(PathToDataSegment);
    datasegmentFileSize := memoryStreamForThirdFile.size;
    DebugLn('#################################');
    DebugLn(Format('DataSegment file size = %d ', [datasegmentFileSize]));
    DebugLn('#################################');
    SetLength(thirdBuffer, datasegmentFileSize);
    memoryStreamForThirdFile.seek(0, sofromBeginning);
    numberOfBytesRead := memoryStreamForThirdFile.Read(Pointer(thirdBuffer)^, datasegmentFileSize);
    DebugLn('#################################');
    DebugLn(Format('Actual number of bytes from data_segment file = %d ', [numberOfBytesRead]));
    DebugLn('#################################');

    if(numberOfBytesRead <> datasegmentFileSize) then
    begin
      ErrorDesc := 'Error getting data segment after execution of contract';
      ErrorNum := CT_RPC_ErrNum_ErrorDuringFirstExecutionOfContract;
    end;

    paramsTableAndBytecode.Add(thirdBuffer);
    Debugln('after adding bytecode, exportable and datasegment data');


    Debugln('length of tmp after + = %d', [Length(paramsTableAndBytecode)]);
    Debugln('Manual calculation of length = %d', [44 + exportTableSize + bytecodeSize + length(thirdBuffer)]);

    if Not(DeleteFile(PathToDataSegment)) then begin
        ErrorDesc := 'Error while deleting data segment after initVM';
        ErrorNum := CT_RPC_ErrNum_InternalError;
        Exit;
    end;

    Result := TOpSaveContract.CreateSaveContract(current_protocol,account_signer,account_last_n_operation+1,account_target,privateKey,fee, startBlockFlag, startBlocks, activeBlocks, passiveBlocks,
                                                 instructionCount, memoryUsage, runtimeSeconds, ioUsage, dataSegSize, exportTableSize, bytecodeSize, bufferForBytes, paramsTableAndBytecode, thirdBuffer);

   if Not Result.HasValidSignature then begin
      FreeAndNil(Result);
      ErrorNum:=CT_RPC_ErrNum_InternalError;
      ErrorDesc:='Invalid signature';
      exit;
    end;
   DebugLn('URPC.pas CreateOperationSaveContract done');
  End;

  Function SaveContract(account_signer, account_target, fee : UInt64; startBlockFlag: Boolean; startBlocks, activeBlocks, passiveBlocks, instructionCount, memoryUsage: Cardinal;
                        runtimeSeconds, ioUsage, dataSegSize, exportTableSize, bytecodeSize : Cardinal;
                        const exportTableFilePath, PayloadFilePath : String; Const Payload_method, EncodePwd : String) : Boolean;
  // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
  Var opck : TOpSaveContract;
    acc_signer : TAccount;
    errors : String;
    opr : TOperationResume;
  begin
   DebugLn('URPC.pas SaveContract started');
    FNode.OperationSequenceLock.Acquire;  // Use lock to prevent N_Operation race-condition on concurrent invocations
    try
      Result := false;
      if (account_signer<0) or (account_signer>=FNode.Bank.AccountsCount) then begin
        ErrorDesc:='Invalid account '+Inttostr(account_signer);
        ErrorNum:=CT_RPC_ErrNum_InvalidAccount;
        Exit;
      end;
      acc_signer := FNode.GetMempoolAccount(account_signer);

      //DebugLn(Format('SaveContract Number of active blocks = %d',[activeBlocks]));
      //DebugLn(Format('SaveContract export table size = %d',[exportTableSize]));
      //DebugLn(Format('SaveContract bytecode size = %d',[bytecodeSize]));
      //DebugLn(Format('SaveContract export table length = %d',[Length(exportTable)]));
      //DebugLn(Format('SaveContract bytecode legth = %d',[Length(RawPayload)]));


      opck := CreateOperationSaveContract(FNode.Bank.Vault.CurrentProtocol,account_signer, acc_signer.n_operation,account_target,acc_signer.accountInfo.accountKey,fee, startBlockFlag, startBlocks, activeBlocks,
                                          passiveBlocks, instructionCount, memoryUsage, runtimeSeconds, ioUsage, dataSegSize, exportTableSize, bytecodeSize, exportTableFilePath, PayloadFilePath,Payload_method ,EncodePwd);

      //TODO: here you get operation id
      //acc_signer := FNode.GetStoredOperationsFromAccount();
      if not assigned(opck) then exit;
      try
        If not FNode.AddOperation(Nil,opck,errors) then begin
          ErrorDesc := 'Error adding operation: '+errors;
          ErrorNum := CT_RPC_ErrNum_InvalidOperation;
          Exit;
        end;
        TABEYOperation.OperationToOperationResume(0,opck,False,account_signer,opr);
        FillOperationResumeToJSONObject(opr,GetResultObject);
        Result := true;
      finally
        opck.free;
      end;
    finally
      FNode.OperationSequenceLock.Release;
    end;
    DebugLn('URPC.pas SaveContract done');
  end;


  Function CreateOperationData(current_protocol : Word; sender, target, sender_last_n_operation : Cardinal; amount, fee : UInt64;
    Const senderAccounKey, targetAccountKey : TAccountKey; Const RawPayload : TRawBytes;
    Const Payload_method, EncodePwd : String) : TOpData;
  // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
  Var f_raw : TRawBytes;
    privateKey : TECPrivateKey;
  Begin
    Result := Nil;
    if Not CheckAndGetPrivateKeyInWallet(senderAccounKey,privateKey) then Exit(Nil);
    if Not CheckAndGetEncodedRAWPayload(RawPayload,Payload_method,EncodePwd,senderAccounKey,targetAccountKey,f_raw) then Exit(Nil);
    Result := TOpData.CreateOpData(sender,sender,target,privateKey,sender_last_n_operation+1,5,5,amount,fee,f_raw);

    if Not Result.HasValidSignature then begin
      FreeAndNil(Result);
      ErrorNum:=CT_RPC_ErrNum_InternalError;
      ErrorDesc:='Invalid signature';
      exit;
    end;
  End;

  Function SendData(sender, target : Cardinal; amount, fee : UInt64; Const RawPayload : TRawBytes; Const Payload_method, EncodePwd : String) : Boolean;
  // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
  Var opt : TOpData;
    sacc,tacc : TAccount;
    errors : String;
    opr : TOperationResume;
  begin
    FNode.OperationSequenceLock.Acquire;  // Use lock to prevent N_Operation race-condition on concurrent sends
    try
      Result := false;
      if (sender<0) or (sender>=FNode.Bank.AccountsCount) then begin
        If (sender=CT_MaxAccount) then ErrorDesc := 'Need sender'
        else ErrorDesc:='Invalid sender account '+Inttostr(sender);
        ErrorNum:=CT_RPC_ErrNum_InvalidAccount;
        Exit;
      end;
      if (target<0) or (target>=FNode.Bank.AccountsCount) then begin
        If (target=CT_MaxAccount) then ErrorDesc := 'Need target'
        else ErrorDesc:='Invalid target account '+Inttostr(target);
        ErrorNum:=CT_RPC_ErrNum_InvalidAccount;
        Exit;
      end;
      sacc := FNode.GetMempoolAccount(sender);
      tacc := FNode.GetMempoolAccount(target);

      opt := CreateOperationData(FNode.Bank.Vault.CurrentProtocol,sender,target,sacc.n_operation,amount,fee,sacc.accountInfo.accountKey,tacc.accountInfo.accountKey,RawPayload,Payload_method,EncodePwd);
      if opt=nil then exit;
      try
        If not FNode.AddOperation(Nil,opt,errors) then begin
          ErrorDesc := 'Error adding operation: '+errors;
          ErrorNum := CT_RPC_ErrNum_InvalidOperation;
          Exit;
        end;
        TABEYOperation.OperationToOperationResume(0,opt,False,sender,opr);
        FillOperationResumeToJSONObject(opr,GetResultObject);
        Result := true;
      finally
        opt.free;
      end;
    finally
      FNode.OperationSequenceLock.Release;
    end;
  end;

  Function SaveContractRefactored(params:TABEYJsonObject) : Boolean;
  // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
  Var opck : TOpSaveContractRefactored;
    acc_signer : TAccount;
    account_signer : Cardinal;
    errors : String;
    opr : TOperationResume;
    privateKey : TECPrivateKey;
  begin
   DebugLn('URPC.pas SaveContract started');
   Result := false;
    FNode.OperationSequenceLock.Acquire;  // Use lock to prevent N_Operation race-condition on concurrent invocations
    try

      if params.IndexOfName('contractSignerID')<0 then begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'Need "contractSignerID" param';
          Errors := ErrorDesc;
          exit;
      end;

      account_signer :=  params.AsCardinal('contractSignerID',CT_MaxAccount) ;

      if (account_signer<0) or (account_signer>=FNode.Bank.AccountsCount) then begin
        ErrorDesc:='Invalid account '+Inttostr(account_signer);
        ErrorNum:=CT_RPC_ErrNum_InvalidAccount;
        Exit;
      end;

      acc_signer := FNode.GetMempoolAccount(account_signer);


      //get private key
      if CheckAndGetPrivateKeyInWallet(acc_signer.accountInfo.accountKey,privateKey) = False then Exit;


      DebugLn('Passed prelimins');

      DebugLn('n_operation' + IntTOStr(acc_signer.n_operation));


      if TSmartContractHelper.CreateOperationSaveContract (
                 FNode.Bank.Vault.CurrentProtocol,
                 privateKey,
                 (acc_signer.n_operation + 1),
                 FNode.Bank.AccountsCount,FNode.Bank.BlocksCount,
                 params,
                 TObject(opck),
                 errors,
                 ErrorNum,
                 ErrorDesc)  = False
      then begin
        DebugLn(ErrorDesc);
        DebugLn(errors);
        Exit;
      end;

      DebugLn('Succ operation');

      if not assigned(opck) then exit;
      try
        DebugLn('assigned opck');
        DebugLn(IntTOStr(Length(opck.Data.payload)));

        TLog.NewLog(ltInfo,ClassName,'Reached critical moment');
        If not FNode.AddOperation(Nil,TOpSaveContractRefactored(opck),errors) then begin
          DebugLn('Badd');
          ErrorDesc := 'Error adding operation: ' + errors;
          ErrorNum := CT_RPC_ErrNum_InvalidOperation;
          Exit;
        end;
        DebugLn('almost');
        TABEYOperation.OperationToOperationResume(0,opck,False,account_signer,opr);
        FillOperationResumeToJSONObject(opr,GetResultObject);
        DebugLn('almost2');
        Result := true;
      finally
        opck.free;
      end;
    finally
      FNode.OperationSequenceLock.Release;
    end;
    DebugLn('URPC.pas SaveContract done');
  end;

  Function ModifySnapshot(Path:String;errors:String;ErrorDesc:String;ErrorNum:Integer):Boolean;

  var    Types:Array of String;
         Values:Array of String;
    begin
     Result := TSmartContractHelper.ModifyDataSegment('nextPrime','/home/mma/ABEY/Contracts/Temp/bytecode7991','/home/mma/ABEY/Contracts/Temp/exportTable7991',Path,Types,Values,errors,ErrorNum,ErrorDesc);
    end;

  Function ExecuteContractRefactored(params:TABEYJsonObject) : Boolean;
  // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
  Var opck : TOpExecuteContract;
    acc_signer : TAccount;
    account_signer : Cardinal;
    errors : String;
    opr : TOperationResume;
    privateKey : TECPrivateKey;
    RawPayload:TRawBytes;
    Payload_method:String;
    EncodePwd:String;
  begin
   DebugLn('URPC.pas SaveContract started');
   Result := false;
    FNode.OperationSequenceLock.Acquire;  // Use lock to prevent N_Operation race-condition on concurrent invocations
    try

      if params.IndexOfName('callerID')<0 then begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'Need "callerID" param';
          Errors := ErrorDesc;
          exit;
      end;

      account_signer :=  params.AsCardinal('callerID',CT_MaxAccount) ;

      if (account_signer<0) or (account_signer>=FNode.Bank.AccountsCount) then begin
        ErrorDesc:='Invalid account '+Inttostr(account_signer);
        ErrorNum:=CT_RPC_ErrNum_InvalidAccount;
        Exit;
      end;

      acc_signer := FNode.GetMempoolAccount(account_signer);


      //get private key
      if CheckAndGetPrivateKeyInWallet(acc_signer.accountInfo.accountKey,privateKey) = False then Exit;

      if Not CheckAndGetEncodedRAWPayload(RawPayload,Payload_method,EncodePwd,acc_signer.accountInfo.accountKey,acc_signer.accountInfo.accountKey,RawPayload) then Exit;

      DebugLn('Passed prelimins');

      DebugLn('n_operation' + IntTOStr(acc_signer.n_operation));

      if TSmartContractHelper.CreateOperationExecuteContract (
                 FNode.Bank.Vault.CurrentProtocol,
                 privateKey,
                 (acc_signer.n_operation + 1),
                 FNode.Bank.AccountsCount,FNode.Bank.BlocksCount,
                 params,
                 TObject(opck),
                 RawPayload,
                 errors,
                 ErrorNum,
                 ErrorDesc)  = False
      then begin
        DebugLn(ErrorDesc);
        DebugLn(errors);
        Exit;
      end;

      DebugLn('Succ operation');



      if not assigned(opck) then exit;
      try
        DebugLn('assigned opck');
        DebugLn(IntTOStr(Length(opck.Data.payload)));


        If not FNode.AddOperation(Nil,TOpExecuteContractRefactored(opck),errors) then begin
          DebugLn('Badd');
          ErrorDesc := 'Error adding operation: ' + errors;
          ErrorNum := CT_RPC_ErrNum_InvalidOperation;
          Exit;
        end;
        DebugLn('almost');
        TABEYOperation.OperationToOperationResume(0,opck,False,account_signer,opr);
        FillOperationResumeToJSONObject(opr,GetResultObject);
        DebugLn('almost');
        Result := true;
      finally
        opck.free;
      end;
    finally
      FNode.OperationSequenceLock.Release;
    end;
    DebugLn('URPC.pas SaveContract done');
  end;

  Function CreateOperationExecuteContract(current_protocol : Word; account_signer, account_last_n_operation, contractID : Cardinal; const account_pubkey : TAccountKey; fee : UInt64;
                                        functionName : string; parametersTypes, parametersValues : array of String;
                                        Const Payload_method, EncodePwd : String) : TOpExecuteContract;   // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
  var
    errors : String;
    f_raw : TRawBytes;
    privateKey : TECPrivateKey;
    i : integer;
  Begin

    DebugLn('URPC.pas Create Operation Execute Contract started');
    Result := Nil;
    if Not CheckAndGetPrivateKeyInWallet(account_pubkey,privateKey) then Exit(Nil);

    //empty payload for now, we can add it later. Missing only on frontend for JSONRPC
    SetLength(f_raw, 0);
    Result := TOpExecuteContract.CreateExecuteContract(current_protocol,account_signer,account_last_n_operation+1,contractID, privateKey,fee,
                                                       functionName, parametersTypes, parametersValues, f_raw);

   if Not Result.HasValidSignature then begin
      FreeAndNil(Result);
      ErrorNum:=CT_RPC_ErrNum_InternalError;
      ErrorDesc:='Invalid signature';
      exit;
    end;
   DebugLn('URPC.pas Create Operation Execute Contract done');
  End;

  Function ExecuteContract(account_signer, contractID, fee : UInt64; functionName : string; parametersType, parametersValue : array of String;
                           Const Payload_method, EncodePwd : String) : Boolean;
  // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
  Var opck : TOpExecuteContract;
    acc_signer : TAccount;
    errors : String;
    opr : TOperationResume;
  begin
   DebugLn('URPC.pas Execute Contract started');
    FNode.OperationSequenceLock.Acquire;  // Use lock to prevent N_Operation race-condition on concurrent invocations
    try
      Result := false;
      if (account_signer<0) or (account_signer>=FNode.Bank.AccountsCount) then begin
        ErrorDesc:='Invalid account '+Inttostr(account_signer);
        ErrorNum:=CT_RPC_ErrNum_InvalidAccount;
        Exit;
      end;
      acc_signer := FNode.GetMempoolAccount(account_signer);

      opck := CreateOperationExecuteContract(FNode.Bank.Vault.CurrentProtocol,account_signer, acc_signer.n_operation,contractID,acc_signer.accountInfo.accountKey,fee, functionName,
                                          parametersType, parametersValue,Payload_method ,EncodePwd);

      //TODO: here you get operation id
      //acc_signer := FNode.GetStoredOperationsFromAccount();

      if not assigned(opck) then exit;
      try
        If not FNode.AddOperation(Nil,opck,errors) then begin
          ErrorDesc := 'Error adding operation: '+errors;
          ErrorNum := CT_RPC_ErrNum_InvalidOperation;
          Exit;
        end;
        TABEYOperation.OperationToOperationResume(0,opck,False,account_signer,opr);
        FillOperationResumeToJSONObject(opr,GetResultObject);
        Result := true;
      finally
        opck.free;
      end;
    finally
      FNode.OperationSequenceLock.Release;
    end;
    DebugLn('URPC.pas Execute Contract done');
  end;


  // This function creates a TOpListAccountForSale without looking for actual state (cold wallet)
  // It assumes that account_number,account_last_n_operation and account_pubkey are correct
  Function CreateOperationListAccountForSale(current_protocol : Word; account_signer, account_last_n_operation, account_listed : Cardinal; const account_signer_pubkey: TAccountKey;
    account_price : UInt64; locked_until_block : Cardinal; account_to_pay : Cardinal; Const new_account_pubkey : TAccountKey;
    fee : UInt64; RawPayload : TRawBytes; Const Payload_method, EncodePwd : String) : TOpListAccountForSale;
  // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
  var privateKey : TECPrivateKey;
    errors : String;
    f_raw : TRawBytes;
    aux_target_pubkey : TAccountKey;
  Begin
    Result := Nil;
    if Not CheckAndGetPrivateKeyInWallet(account_signer_pubkey,privateKey) then Exit(Nil);
    if (Payload_method='dest') And (new_account_pubkey.EC_OpenSSL_NID<>CT_TECDSA_Public_Nul.EC_OpenSSL_NID) then begin
        // If using 'dest', only will apply if there is a fixed new public key, otherwise will use current public key of account
       aux_target_pubkey := new_account_pubkey;
    end else aux_target_pubkey := account_signer_pubkey;
    if Not CheckAndGetEncodedRAWPayload(RawPayload,Payload_method,EncodePwd,account_signer_pubkey,aux_target_pubkey,f_raw) then Exit(Nil);
    Result := TOpListAccountForSale.CreateListAccountForSale(current_protocol, account_signer,account_last_n_operation+1,account_listed,account_price,fee,account_to_pay,new_account_pubkey,locked_until_block,
      privateKey,f_raw);
    if Not Result.HasValidSignature then begin
      FreeAndNil(Result);
      ErrorNum:=CT_RPC_ErrNum_InternalError;
      ErrorDesc:='Invalid signature';
      exit;
    end;
  End;

  // This function creates a TOpDelistAccountForSale without looking for actual state (cold wallet)
  // It assumes that account_number,account_last_n_operation are correct
  Function CreateOperationDelistAccountForSale(current_protocol : Word; account_signer, account_last_n_operation, account_delisted : Cardinal; const account_signer_pubkey: TAccountKey;
    fee : UInt64; RawPayload : TRawBytes; Const Payload_method, EncodePwd : String) : TOpDelistAccountForSale;
  // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
  var privateKey : TECPrivateKey;
    f_raw : TRawBytes;
  Begin
    Result := Nil;
    if Not CheckAndGetPrivateKeyInWallet(account_signer_pubkey,privateKey) then Exit(Nil);
    if Not CheckAndGetEncodedRAWPayload(RawPayload,Payload_method,EncodePwd,account_signer_pubkey,account_signer_pubkey,f_raw) then Exit(Nil);
    Result := TOpDelistAccountForSale.CreateDelistAccountForSale(current_protocol,account_signer,account_last_n_operation+1,account_delisted,fee,privateKey,f_raw);
    if Not Result.HasValidSignature then begin
      FreeAndNil(Result);
      ErrorNum:=CT_RPC_ErrNum_InternalError;
      ErrorDesc:='Invalid signature';
      exit;
    end;
  End;

  // This function creates a TOpBuyAccount without looking for actual state (cold wallet)
  // It assumes that account_number,account_last_n_operation and account_pubkey are correct
  // Also asumes that amount is >= price and other needed conditions
  Function CreateOperationBuyAccount(current_protocol : Word; account_number, account_last_n_operation : Cardinal; const account_pubkey: TAccountKey;
    account_to_buy : Cardinal; account_price, amount : UInt64; account_to_pay : Cardinal; Const new_account_pubkey : TAccountKey;
    fee : UInt64; RawPayload : TRawBytes; Const Payload_method, EncodePwd : String) : TOpBuyAccount;
  // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
  var privateKey : TECPrivateKey;
    errors : String;
    f_raw : TRawBytes;
  Begin
    Result := Nil;
    if Not CheckAndGetPrivateKeyInWallet(account_pubkey,privateKey) then Exit(Nil);
    if Not CheckAndGetEncodedRAWPayload(RawPayload,Payload_method,EncodePwd,account_pubkey,new_account_pubkey,f_raw) then Exit(Nil);
    Result := TOpBuyAccount.CreateBuy(current_protocol,account_number,account_last_n_operation+1,account_to_buy,account_to_pay,account_price,amount,fee,new_account_pubkey,privateKey,f_raw);
    if Not Result.HasValidSignature then begin
      FreeAndNil(Result);
      ErrorNum:=CT_RPC_ErrNum_InternalError;
      ErrorDesc:='Invalid signature';
      exit;
    end;
  End;

  Function GetCardinalsValues(ordinals_coma_separated : String; cardinals : TOrderedCardinalList; var errors : String) : Boolean;
  Var i,istart : Integer;
    ctxt : String;
    an : Cardinal;
  begin
    result := false;
    cardinals.Clear;
    errors := '';
    ctxt := '';
    istart := 1;
    for i := Low(ordinals_coma_separated) to High(ordinals_coma_separated) do begin
      case ordinals_coma_separated[i] of
        '0'..'9','-' : ctxt := ctxt + ordinals_coma_separated[i];
        ',',';' : begin
          if trim(ctxt)<>'' then begin
            if Not TAccountComp.AccountTxtNumberToAccountNumber(trim(ctxt),an) then begin
              errors := 'Invalid account number at pos '+IntToStr(istart)+': '+ctxt;
              exit;
            end;
            cardinals.Add(an);
          end;
          ctxt := '';
          istart := i+1;
        end;
        ' ' : ; // Continue...
      else
        errors := 'Invalid char at pos '+inttostr(i)+': "'+ordinals_coma_separated[i]+'"';
        exit;
      end;
    end;
    //
    if (trim(ctxt)<>'') then begin
      if Not TAccountComp.AccountTxtNumberToAccountNumber(trim(ctxt),an) then begin
        errors := 'Invalid account number at pos '+IntToStr(istart)+': '+ctxt;
        exit;
      end;
      cardinals.Add(an);
    end;
    if cardinals.Count=0 then begin
      errors := 'No valid value';
      exit;
    end;
    Result := true;
  end;

  Function ChangeAccountsKey(accounts_txt : String; const new_pub_key : TAccountKey; fee : UInt64; const RawPayload : TRawBytes; Const Payload_method, EncodePwd : String) : Boolean;
  // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
  Var opck : TOpChangeKey;
    acc : TAccount;
    i, ian : Integer;
    errors : String;
    opr : TOperationResume;
    accountsnumber : TOrderedCardinalList;
    operationsht : TOperationsHashTree;
    OperationsResumeList : TOperationsResumeList;
  begin
    FNode.OperationSequenceLock.Acquire;  // Use lock to prevent N_Operation race-condition on concurrent invocations
    try
      Result := false;
      accountsnumber := TOrderedCardinalList.Create;
      try
        if not GetCardinalsValues(accounts_txt,accountsnumber,errors) then begin
          ErrorDesc := 'Error in accounts: '+errors;
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          Exit;
        end;
        operationsht := TOperationsHashTree.Create;
        try
          for ian := 0 to accountsnumber.Count - 1 do begin

            if (accountsnumber.Get(ian)<0) or (accountsnumber.Get(ian)>=FNode.Bank.AccountsCount) then begin
              ErrorDesc:='Invalid account '+Inttostr(accountsnumber.Get(ian));
              ErrorNum:=CT_RPC_ErrNum_InvalidAccount;
              Exit;
            end;
            acc := FNode.GetMempoolAccount(accountsnumber.Get(ian));
            opck := CreateOperationChangeKey(FNode.Bank.Vault.CurrentProtocol,acc.account,acc.n_operation,acc.account,acc.accountInfo.accountKey,new_pub_key,fee,RawPayload,Payload_method,EncodePwd);
            if not assigned(opck) then exit;
            try
              operationsht.AddOperationToHashTree(opck);
            finally
              opck.free;
            end;
          end; // For
          // Ready to execute...
          OperationsResumeList := TOperationsResumeList.Create;
          Try
            i := FNode.AddOperations(Nil,operationsht,OperationsResumeList, errors);
            if (i<0) then begin
              ErrorNum:=CT_RPC_ErrNum_InternalError;
              ErrorDesc:=errors;
              exit;
            end;
            GetResultArray.Clear; // Inits an array
            for i := 0 to OperationsResumeList.Count - 1 do begin
              FillOperationResumeToJSONObject(OperationsResumeList[i],GetResultArray.GetAsObject(i));
            end;
          Finally
            OperationsResumeList.Free;
          End;
          Result := true;
        finally
          operationsht.Free;
        end;
      finally
        accountsnumber.Free;
      end;
    finally
      FNode.OperationSequenceLock.Release;
    end;
  end;

  Function SignOpChangeKey(Const HexaStringOperationsHashTree : String; current_protocol : Word; account_signer, account_target : Cardinal;
    Const actualAccounKey, newAccountKey : TAccountKey;
    last_n_operation : Cardinal;
    fee : UInt64; Const RawPayload : TRawBytes; Const Payload_method, EncodePwd : String) : Boolean;
  // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
  var OperationsHashTree : TOperationsHashTree;
    errors : String;
    opck : TOpChangeKey;
  begin
    Result := false;
    if Not HexaStringToOperationsHashTree(HexaStringOperationsHashTree,OperationsHashTree,errors) then begin
      ErrorNum:=CT_RPC_ErrNum_InvalidData;
      ErrorDesc:= 'Error decoding param "rawoperations": '+errors;
      Exit;
    end;
    Try
      opck := CreateOperationChangeKey(current_protocol,account_signer,last_n_operation,account_target,actualAccounKey,newAccountKey,fee,RawPayload,Payload_method,EncodePwd);
      if opck=nil then exit;
      try
        OperationsHashTree.AddOperationToHashTree(opck);
        TABEYJSONComp.FillOperationsHashTreeObject(OperationsHashTree,GetResultObject);
        Result := true;
      finally
        opck.Free;
      end;
    Finally
      OperationsHashTree.Free;
    End;
  end;

  Function OperationsInfo(Const HexaStringOperationsHashTree : String; jsonArray : TABEYJSONArray) : Boolean;
  var OperationsHashTree : TOperationsHashTree;
    errors : String;
    OPR : TOperationResume;
    Obj : TABEYJSONObject;
    Op : TABEYOperation;
    i : Integer;
  Begin
    Result := False;
    if Not HexaStringToOperationsHashTree(HexaStringOperationsHashTree,OperationsHashTree,errors) then begin
      ErrorNum:=CT_RPC_ErrNum_InvalidData;
      ErrorDesc:= 'Error decoding param "rawoperations": '+errors;
      Exit;
    end;
    Try
      jsonArray.Clear;
      for i := 0 to OperationsHashTree.OperationsCount - 1 do begin
        Op := OperationsHashTree.GetOperation(i);
        Obj := jsonArray.GetAsObject(i);
        If TABEYOperation.OperationToOperationResume(0,Op,True,Op.SignerAccount,OPR) then begin
          OPR.NOpInsideBlock := i;
          OPR.Balance := -1;
        end else OPR := CT_TOperationResume_NUL;
        FillOperationResumeToJSONObject(OPR,Obj);
      end;
      Result := true;
    Finally
      OperationsHashTree.Free;
    End;
  End;

  Function ExecuteOperations(Const HexaStringOperationsHashTree : String) : Boolean;
  var OperationsHashTree : TOperationsHashTree;
    errors : String;
    i : Integer;
    OperationsResumeList : TOperationsResumeList;
  Begin
    Result := False;
    if Not HexaStringToOperationsHashTree(HexaStringOperationsHashTree,OperationsHashTree,errors) then begin
      ErrorNum:=CT_RPC_ErrNum_InvalidData;
      ErrorDesc:= 'Error decoding param "rawoperations": '+errors;
      Exit;
    end;
    Try
      errors := '';
      OperationsResumeList := TOperationsResumeList.Create;
      Try
        i := FNode.AddOperations(Nil,OperationsHashTree,OperationsResumeList,errors);
        if (i<0) then begin
          ErrorNum:=CT_RPC_ErrNum_InternalError;
          ErrorDesc:=errors;
          exit;
        end;
        GetResultArray.Clear; // Inits an array
        for i := 0 to OperationsResumeList.Count - 1 do begin
          FillOperationResumeToJSONObject(OperationsResumeList[i],GetResultArray.GetAsObject(i));
        end;
      Finally
        OperationsResumeList.Free;
      End;
      Result := true;
    Finally
      OperationsHashTree.Free;
    End;
  End;

  Function DoEncrypt(RawPayload : TRawBytes; pub_key : TAccountKey; Const Payload_method, EncodePwdForAES : String) : Boolean;
  Var f_raw : TRawBytes;
  begin
    Result := false;
    if (length(RawPayload)>0) then begin
      if (Payload_method='none') then f_raw:=RawPayload
      else if (Payload_method='pubkey') then begin
        TABEYEncryption.DoABEYECIESEncrypt(pub_key,RawPayload,f_raw);
      end else if (Payload_method='aes') then begin
        f_raw := TABEYEncryption.DoABEYAESEncrypt(RawPayload,TEncoding.ANSI.GetBytes(EncodePwdForAES));
      end else begin
        ErrorNum:=CT_RPC_ErrNum_InvalidOperation;
        ErrorDesc:='Invalid encode payload method: '+Payload_method;
        exit;
      end;
    end else f_raw := Nil;
    jsonresponse.GetAsVariant('result').Value := TCrypto.ToHexaString(f_raw);
    Result := true;
  end;

  Function DoDecrypt(RawEncryptedPayload : TRawBytes; jsonArrayPwds : TABEYJSONArray) : Boolean;
  var i : Integer;
    pkey : TECPrivateKey;
    decrypted_payload : TRawBytes;
  Begin
    Result := false;
    if Length(RawEncryptedPayload)=0 then begin
      GetResultObject.GetAsVariant('result').Value:= False;
      GetResultObject.GetAsVariant('enc_payload').Value:= '';
      Result := true;
      exit;
    end;
    for i := 0 to _RPCServer.WalletKeys.Count - 1 do begin
      pkey := _RPCServer.WalletKeys.Key[i].PrivateKey;
      if (assigned(pkey)) then begin
        if TABEYEncryption.DoABEYECIESDecrypt(pkey.PrivateKey,RawEncryptedPayload,decrypted_payload) then begin
          GetResultObject.GetAsVariant('result').Value:= true;
          GetResultObject.GetAsVariant('enc_payload').Value:= TCrypto.ToHexaString(RawEncryptedPayload);
          GetResultObject.GetAsVariant('unenc_payload').Value:= decrypted_payload.ToPrintable;
          GetResultObject.GetAsVariant('unenc_hexpayload').Value:= TCrypto.ToHexaString(decrypted_payload);
          GetResultObject.GetAsVariant('payload_method').Value:= 'key';
          GetResultObject.GetAsVariant('enc_pubkey').Value:= TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(pkey.PublicKey));
          Result := true;
          Exit;
        end;
      end;
    end;
    for i := 0 to jsonArrayPwds.Count - 1 do begin
      if TABEYEncryption.DoABEYAESDecrypt(RawEncryptedPayload,TEncoding.ANSI.GetBytes(jsonArrayPwds.GetAsVariant(i).AsString('')),decrypted_payload) then begin
        GetResultObject.GetAsVariant('result').Value:= true;
        GetResultObject.GetAsVariant('enc_payload').Value:= TCrypto.ToHexaString(RawEncryptedPayload);
        GetResultObject.GetAsVariant('unenc_payload').Value:= decrypted_payload.ToPrintable;
        GetResultObject.GetAsVariant('unenc_hexpayload').Value:= TCrypto.ToHexaString(decrypted_payload);
        GetResultObject.GetAsVariant('payload_method').Value:= 'pwd';
        GetResultObject.GetAsVariant('pwd').Value:= jsonArrayPwds.GetAsVariant(i).AsString('');
        Result := true;
        exit;
      end;
    end;
    // Not found
    GetResultObject.GetAsVariant('result').Value:= False;
    GetResultObject.GetAsVariant('enc_payload').Value:= TCrypto.ToHexaString(RawEncryptedPayload);
    Result := true;
  End;

  Function CapturePubKeyExt(const jsonObjParams : TABEYJSONObject; const prefix : String; var pubkey : TAccountKey; var errortxt : String) : Boolean;
  var errors_aux : String;
    auxpubkey : TAccountKey;
  begin
    pubkey := CT_Account_NUL.accountInfo.accountKey;
    errortxt := '';
    Result := false;
    if (jsonObjparams.IndexOfName(prefix+'b58_pubkey')>=0) then begin
      If Not TAccountComp.AccountPublicKeyImport(jsonObjparams.AsString(prefix+'b58_pubkey',''),pubkey,errors_aux) then begin
        errortxt:= 'Invalid value of param "'+prefix+'b58_pubkey": '+errors_aux;
        exit;
      end;
      if (jsonObjparams.IndexOfName(prefix+'enc_pubkey')>=0) then begin
        auxpubkey := TAccountComp.RawString2Accountkey(TCrypto.HexaToRaw(jsonObjparams.AsString(prefix+'enc_pubkey','')));
        if (Not TAccountComp.EqualAccountKeys(auxpubkey,pubkey)) then begin
          errortxt := 'Params "'+prefix+'b58_pubkey" and "'+prefix+'enc_pubkey" public keys are not the same public key';
          exit;
        end;
      end;
    end else begin
      if (jsonObjparams.IndexOfName(prefix+'enc_pubkey')<0) then begin
        errortxt := 'Need param "'+prefix+'enc_pubkey" or "'+prefix+'b58_pubkey"';
        exit;
      end;
      pubkey := TAccountComp.RawString2Accountkey(TCrypto.HexaToRaw(jsonObjparams.AsString(prefix+'enc_pubkey','')));
    end;
    If Not TAccountComp.IsValidAccountKey(pubkey,errors_aux) then begin
      errortxt := 'Invalid public key: '+errors_aux;
    end else Result := true;
  end;

  Function CapturePubKey(const prefix : String; var pubkey : TAccountKey; var errortxt : String) : Boolean;
  begin
    Result := CapturePubKeyExt(params,prefix,pubkey,errortxt);
  end;

  function SignListAccountForSaleEx(params : TABEYJSONObject; OperationsHashTree : TOperationsHashTree; current_protocol : Word; const actualAccounKey : TAccountKey; last_n_operation : Cardinal) : boolean;
    // params:
    // "account_signer" is the account that signs operations and pays the fee
    // "account_target" is the account being listed
    // "locked_until_block" is until which block will be locked this account (Note: A locked account cannot change it's state until sold or finished lock)
    // "price" is the price
    // "seller_account" is the account to pay (seller account)
    // "new_b58_pubkey" or "new_enc_pubkey" is the future public key for this sale (private sale), otherwise is open and everybody can buy
  var
    opSale: TOpListAccountForSale;
    account_signer, account_target, seller_account : Cardinal;
    locked_until_block : Cardinal;
    price,fee : Int64;
    new_pubkey : TAccountKey;
  begin
    Result := false;
    account_signer := params.AsInteger('account_signer',MaxInt);
    if (account_signer>=MaxInt) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      ErrorDesc := 'Invalid "account_signer" account';
      Exit;
    end;
    account_target := params.AsInteger('account_target',MaxInt);
    if (account_target>=MaxInt) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      ErrorDesc := 'Invalid "account_target" account';
      Exit;
    end;
    seller_account := params.AsInteger('seller_account',MaxInt);
    if (seller_account>=MaxInt) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      ErrorDesc := 'Invalid "seller_account" to pay';
      Exit;
    end;
    locked_until_block := params.AsInteger('locked_until_block',MaxInt);
    if (locked_until_block>=MaxInt) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Invalid "locked_until_block" value';
      Exit;
    end;
    price := ToABEYs(params.AsDouble('price',0));
    if (price=0) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Invalid price value';
      Exit;
    end;
    fee := ToABEYs(params.AsDouble('fee',0));
    if (fee<0) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Invalid fee value';
      Exit;
    end;
    if (params.IndexOfName('new_b58_pubkey')>=0) or (params.IndexOfName('new_enc_pubkey')>=0) then begin
      If Not CapturePubKey('new_',new_pubkey,ErrorDesc) then begin
        ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
        Exit;
      end;
    end else new_pubkey := CT_TECDSA_Public_Nul;
    opSale := CreateOperationListAccountForSale(current_protocol, account_signer,last_n_operation,account_target,actualAccounKey,price,locked_until_block,
      seller_account, new_pubkey,fee,
      TCrypto.HexaToRaw(params.AsString('payload','')),
      params.AsString('payload_method','dest'),params.AsString('pwd',''));
    if opSale=nil then exit;
    try
      OperationsHashTree.AddOperationToHashTree(opSale);
      Result := true;
    finally
      opSale.Free;
    end;
  end;

  function SignListAccountForSaleColdWallet(Const HexaStringOperationsHashTree : String; params : TABEYJSONObject) : boolean;
  var errors : String;
    OperationsHashTree : TOperationsHashTree;
    accountpubkey : TAccountKey;
    last_n_operation : Cardinal;
    current_protocol : Word;
  begin
    Result := false;
    if Not HexaStringToOperationsHashTree(HexaStringOperationsHashTree,OperationsHashTree,errors) then begin
      ErrorNum:=CT_RPC_ErrNum_InvalidData;
      ErrorDesc:= 'Error decoding param previous operations hash tree raw value: '+errors;
      Exit;
    end;
    try
      If Not CapturePubKey('signer_',accountpubkey,ErrorDesc) then begin
        ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
        ErrorDesc := 'Params "signer_b58_pubkey" or "signer_enc_pubkey" not found';
        exit;
      end;
      last_n_operation := params.AsCardinal('last_n_operation',0);
      current_protocol := params.AsCardinal('protocol',CT_BUILD_PROTOCOL);
      If not SignListAccountForSaleEx(params,OperationsHashTree,current_protocol, accountpubkey,last_n_operation) then Exit
      else Result := True;
      TABEYJSONComp.FillOperationsHashTreeObject(OperationsHashTree,GetResultObject);
    finally
      OperationsHashTree.Free;
    end;
  end;

  function SignDelistAccountForSaleEx(params : TABEYJSONObject; OperationsHashTree : TOperationsHashTree; current_protocol : Word; const actualAccountKey : TAccountKey; last_n_operation : Cardinal) : boolean;
    // params:
    // "account_signer" is the account that signs operations and pays the fee
    // "account_target" is the delisted account
    // "locked_until_block" is until which block will be locked this account (Note: A locked account cannot change it's state until sold or finished lock)
    // "price" is the price
    // "seller_account" is the account to pay
    // "new_b58_pubkey" or "new_enc_pubkey" is the future public key for this sale (private sale), otherwise is open and everybody can buy
  var
    opDelist: TOpDelistAccountForSale;
    account_signer, account_target : Cardinal;
    fee : Int64;
  begin
    Result := false;
    account_signer := params.AsInteger('account_signer',MaxInt);
    if (account_signer>=MaxInt) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      ErrorDesc := 'Invalid "account_signer" account';
      Exit;
    end;
    account_target := params.AsInteger('account_target',MaxInt);
    if (account_target>=MaxInt) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      ErrorDesc := 'Invalid "account_target" account';
      Exit;
    end;
    fee := ToABEYs(params.AsDouble('fee',0));
    if (fee<0) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Invalid fee value';
      Exit;
    end;
    opDelist := CreateOperationDelistAccountForSale(current_protocol,account_signer,last_n_operation,account_target,actualAccountKey,fee,TCrypto.HexaToRaw(params.AsString('payload','')),
      params.AsString('payload_method','dest'),params.AsString('pwd',''));
    if opDelist=nil then exit;
    try
      OperationsHashTree.AddOperationToHashTree(opDelist);
      Result := true;
    finally
      opDelist.Free;
    end;
  end;

  // This function creates a TOpChangeAccountInfo without looking for actual state (cold wallet)
  // It assumes that account_number,account_last_n_operation and account_pubkey are correct
  Function CreateOperationChangeAccountInfo(current_protocol : Word; account_signer, account_last_n_operation, account_target: Cardinal; const account_signer_pubkey: TAccountKey;
    changePubKey : Boolean; Const new_account_pubkey : TAccountKey;
    changeName: Boolean; Const new_name : TRawBytes;
    changeType: Boolean; new_type : Word;
    fee : UInt64; RawPayload : TRawBytes; Const Payload_method, EncodePwd : String) : TOpChangeAccountInfo;
  // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
  var privateKey : TECPrivateKey;
    errors : String;
    f_raw : TRawBytes;
    aux_target_pubkey : TAccountKey;
  Begin
    Result := Nil;
    if Not CheckAndGetPrivateKeyInWallet(account_signer_pubkey,privateKey) then Exit(Nil);
    if (Payload_method='dest') And (new_account_pubkey.EC_OpenSSL_NID<>CT_TECDSA_Public_Nul.EC_OpenSSL_NID) then begin
        // If using 'dest', only will apply if there is a fixed new public key, otherwise will use current public key of account
       aux_target_pubkey := new_account_pubkey;
    end else aux_target_pubkey := account_signer_pubkey;
    if Not CheckAndGetEncodedRAWPayload(RawPayload,Payload_method,EncodePwd,account_signer_pubkey,aux_target_pubkey,f_raw) then Exit(Nil);
    Result := TOpChangeAccountInfo.CreateChangeAccountInfo(current_protocol,
      account_signer,account_last_n_operation+1,account_target,
      privateKey,
      changePubKey,new_account_pubkey,changeName,new_name,changeType,new_type,
      fee,f_raw);
    if Not Result.HasValidSignature then begin
      FreeAndNil(Result);
      ErrorNum:=CT_RPC_ErrNum_InternalError;
      ErrorDesc:='Invalid signature';
      exit;
    end;
  End;

  function SignChangeAccountInfoEx(params : TABEYJSONObject; OperationsHashTree : TOperationsHashTree; current_protocol : Word; const actualAccountKey : TAccountKey; last_n_operation : Cardinal) : boolean;
    // params:
    // "account_signer" is the account that signs operations and pays the fee
    // "account_target" is the target to change info
    // "new_b58_pubkey" or "new_enc_pubkey" is the new public key for this account
    // "new_name" is the new account name
    // "new_type" is the new account type
  var
    opChangeInfo: TOpChangeAccountInfo;
    account_signer, account_target : Cardinal;
    fee : Int64;
    changeKey,changeName,changeType : Boolean;
    new_name : TRawBytes;
    new_type : Word;
    new_typeI : Integer;
    new_pubkey : TAccountKey;
  begin
    Result := false;
    account_signer := params.AsInteger('account_signer',MaxInt);
    if (account_signer>=MaxInt) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      ErrorDesc := 'Invalid "account_signer" account';
      Exit;
    end;
    account_target := params.AsInteger('account_target',MaxInt);
    if (account_target>=MaxInt) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      ErrorDesc := 'Invalid "account_target" account';
      Exit;
    end;
    fee := ToABEYs(params.AsDouble('fee',0));
    if (fee<0) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Invalid fee value';
      Exit;
    end;
    if (params.IndexOfName('new_b58_pubkey')>=0) or (params.IndexOfName('new_enc_pubkey')>=0) then begin
      changeKey:=True;
      If Not CapturePubKey('new_',new_pubkey,ErrorDesc) then begin
        ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
        Exit;
      end;
    end else begin
      new_pubkey := CT_TECDSA_Public_Nul;
      changeKey:=False;
    end;
    if (params.IndexOfName('new_name')>=0) then begin
      changeName:=True;
      new_name.FromString(params.AsString('new_name',''));
    end else begin
      new_name := Nil;
      changeName:=False;
    end;
    if (params.IndexOfName('new_type')>=0) then begin
      changeType:=True;
      new_typeI := params.AsInteger('new_type',-1);
      if (new_typeI<0) Or (new_typeI>65536) then begin
        ErrorNum := CT_RPC_ErrNum_InvalidData;
        ErrorDesc := 'Invalid new type value '+IntToStr(new_typeI);
        Exit;
      end;
      new_type := new_typeI;
    end else begin
      new_type := 0;
      changeType:=False;
    end;

    opChangeInfo := CreateOperationChangeAccountInfo(current_protocol,account_signer,last_n_operation,account_target,actualAccountKey,
      changeKey,new_pubkey,
      changeName,new_name,
      changeType,new_type,
      fee,TCrypto.HexaToRaw(params.AsString('payload','')),
      params.AsString('payload_method','dest'),params.AsString('pwd',''));
    if opChangeInfo=nil then exit;
    try
      OperationsHashTree.AddOperationToHashTree(opChangeInfo);
      Result := true;
    finally
      opChangeInfo.Free;
    end;
  end;

  function SignChangeAccountInfoColdWallet(Const HexaStringOperationsHashTree : String; params : TABEYJSONObject) : boolean;
  var errors : String;
    OperationsHashTree : TOperationsHashTree;
    accountpubkey : TAccountKey;
    last_n_operation : Cardinal;
    current_protocol : Word;
  begin
    Result := false;
    if Not HexaStringToOperationsHashTree(HexaStringOperationsHashTree,OperationsHashTree,errors) then begin
      ErrorNum:=CT_RPC_ErrNum_InvalidData;
      ErrorDesc:= 'Error decoding param previous operations hash tree raw value: '+errors;
      Exit;
    end;
    try
      If Not CapturePubKey('signer_',accountpubkey,ErrorDesc) then begin
        ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
        ErrorDesc := 'Params "signer_b58_pubkey" or "signer_enc_pubkey" not found';
        exit;
      end;
      last_n_operation := params.AsCardinal('last_n_operation',0);
      current_protocol := params.AsCardinal('protocol',CT_BUILD_PROTOCOL);
      If not SignChangeAccountInfoEx(params,OperationsHashTree,current_protocol,accountpubkey,last_n_operation) then Exit
      else Result := True;
      TABEYJSONComp.FillOperationsHashTreeObject(OperationsHashTree,GetResultObject);
    finally
      OperationsHashTree.Free;
    end;
  end;

  function SignDelistAccountForSaleColdWallet(Const HexaStringOperationsHashTree : String; params : TABEYJSONObject) : boolean;
  var errors : String;
    OperationsHashTree : TOperationsHashTree;
    accountpubkey : TAccountKey;
    last_n_operation : Cardinal;
    current_protocol : Word;
  begin
    Result := false;
    if Not HexaStringToOperationsHashTree(HexaStringOperationsHashTree,OperationsHashTree,errors) then begin
      ErrorNum:=CT_RPC_ErrNum_InvalidData;
      ErrorDesc:= 'Error decoding param previous operations hash tree raw value: '+errors;
      Exit;
    end;
    try
      If Not CapturePubKey('signer_',accountpubkey,ErrorDesc) then begin
        ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
        ErrorDesc := 'Params "signer_b58_pubkey" or "signer_enc_pubkey" not found';
        exit;
      end;
      last_n_operation := params.AsCardinal('last_n_operation',0);
      current_protocol := params.AsCardinal('protocol',CT_BUILD_PROTOCOL);
      If not SignDelistAccountForSaleEx(params,OperationsHashTree,current_protocol,accountpubkey,last_n_operation) then Exit
      else Result := True;
      TABEYJSONComp.FillOperationsHashTreeObject(OperationsHashTree,GetResultObject);
    finally
      OperationsHashTree.Free;
    end;
  end;

  function SignBuyAccountEx(params : TABEYJSONObject; OperationsHashTree : TOperationsHashTree; current_protocol : Word; const buyerAccountKey : TAccountKey; last_n_operation : Cardinal) : boolean;
    // params:
    // "buyer_account" is the buyer account
    // "account_to_purchase" is the account to purchase
    // "price" is the price
    // "seller_account" is the account to pay
    // "new_b58_pubkey" or "new_enc_pubkey" is the future public key for this sale (private sale), otherwise is open and everybody can buy
    // "amount" is the transferred amount to pay (can exceed price)
  var
    opBuy: TOpBuyAccount;
    buyer_account, account_to_purchase, seller_account : Cardinal;
    price,amount,fee : Int64;
    new_pubkey : TAccountKey;
  begin
    Result := false;
    buyer_account := params.AsInteger('buyer_account',MaxInt);
    if (buyer_account>=MaxInt) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      ErrorDesc := 'Invalid buyer account';
      Exit;
    end;
    account_to_purchase := params.AsInteger('account_to_purchase',MaxInt);
    if (account_to_purchase>=MaxInt) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      ErrorDesc := 'Invalid account to purchase';
      Exit;
    end;
    seller_account := params.AsInteger('seller_account',MaxInt);
    if (seller_account>=MaxInt) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      ErrorDesc := 'Invalid seller account';
      Exit;
    end;
    price := ToABEYs(params.AsDouble('price',0));
    if (price<0) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Invalid price value';
      Exit;
    end;
    amount := ToABEYs(params.AsDouble('amount',0));
    if (amount<0) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Invalid amount value';
      Exit;
    end;
    fee := ToABEYs(params.AsDouble('fee',0));
    if (fee<0) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Invalid fee value';
      Exit;
    end;
    if (params.IndexOfName('new_b58_pubkey')>=0) or (params.IndexOfName('new_enc_pubkey')>=0) then begin
      If Not CapturePubKey('new_',new_pubkey,ErrorDesc) then begin
        ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
        Exit;
      end;
    end else new_pubkey := CT_TECDSA_Public_Nul;
    opBuy := CreateOperationBuyAccount(current_protocol,buyer_account,last_n_operation,buyerAccountKey,account_to_purchase,price,amount,seller_account,new_pubkey,fee,
      TCrypto.HexaToRaw(params.AsString('payload','')),
      params.AsString('payload_method','dest'),params.AsString('pwd',''));
    if opBuy=nil then exit;
    try
      OperationsHashTree.AddOperationToHashTree(opBuy);
      Result := true;
    finally
      opBuy.Free;
    end;
  end;

  function SignBuyAccountColdWallet(Const HexaStringOperationsHashTree : String; params : TABEYJSONObject) : boolean;
  var errors : String;
    OperationsHashTree : TOperationsHashTree;
    accountpubkey : TAccountKey;
    last_n_operation : Cardinal;
    current_protocol : Word;
  begin
    Result := false;
    if Not HexaStringToOperationsHashTree(HexaStringOperationsHashTree,OperationsHashTree,errors) then begin
      ErrorNum:=CT_RPC_ErrNum_InvalidData;
      ErrorDesc:= 'Error decoding param previous operations hash tree raw value: '+errors;
      Exit;
    end;
    try
      If Not CapturePubKey('signer_',accountpubkey,ErrorDesc) then begin
        ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
        ErrorDesc := 'Params "signer_b58_pubkey" or "signer_enc_pubkey" not found';
        exit;
      end;
      last_n_operation := params.AsCardinal('last_n_operation',0);
      current_protocol := params.AsCardinal('protocol',CT_BUILD_PROTOCOL);
      If not SignBuyAccountEx(params,OperationsHashTree,current_protocol,accountpubkey,last_n_operation) then Exit
      else Result := True;
      TABEYJSONComp.FillOperationsHashTreeObject(OperationsHashTree,GetResultObject);
    finally
      OperationsHashTree.Free;
    end;
  end;

  function ListAccountForSale(params : TABEYJSONObject) : boolean;
  Var OperationsHashTree : TOperationsHashTree;
    account_signer, account_target : TAccount;
    opt : TABEYOperation;
    opr : TOperationResume;
    errors : String;
    c_account : Cardinal;
  Begin
    FNode.OperationSequenceLock.Acquire;  // Use lock to prevent N_Operation race-condition on concurrent invocations
    try
      Result := False;
      OperationsHashTree := TOperationsHashTree.Create;
      try
        if (params.IndexOfName('account_signer')<0) then begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'Need account_signer param';
          Exit;
        end;
        c_account := params.AsCardinal('account_signer',0);
        if (c_account<0) or (c_account>=FNode.Bank.AccountsCount) then begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'Invalid account_signer '+params.AsString('account_signer','');
          Exit;
        end;
        account_signer := FNode.GetMempoolAccount(c_account);
        if (params.IndexOfName('account_target')<0) then begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'Need account_target param';
          Exit;
        end;
        c_account := params.AsCardinal('account_target',0);
        if (c_account<0) or (c_account>=FNode.Bank.AccountsCount) then begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'Invalid account_target '+params.AsString('account_target','');
          Exit;
        end;
        account_target := FNode.GetMempoolAccount(c_account);
        if (Not TAccountComp.EqualAccountKeys(account_signer.accountInfo.accountKey,account_target.accountInfo.accountKey)) then begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'account_signer and account_target have distinct keys. Cannot sign';
          Exit;
        end;
        If not SignListAccountForSaleEx(params,OperationsHashTree,FNode.Bank.Vault.CurrentProtocol, account_signer.accountInfo.accountKey,account_signer.n_operation) then Exit;
        opt := OperationsHashTree.GetOperation(0);
        If not FNode.AddOperation(Nil,opt,errors) then begin
          ErrorNum := CT_RPC_ErrNum_InternalError;
          ErrorDesc := errors;
          Exit;
        end else Result := True;
        TABEYOperation.OperationToOperationResume(0,opt,False,c_account,opr);
        FillOperationResumeToJSONObject(opr,GetResultObject);
      finally
        OperationsHashTree.Free;
      end;
    finally
      FNode.OperationSequenceLock.Release;
    end;
  End;

  function DelistAccountForSale(params : TABEYJSONObject) : boolean;
  Var OperationsHashTree : TOperationsHashTree;
    account_signer, account_target : TAccount;
    opt : TABEYOperation;
    opr : TOperationResume;
    errors : String;
    c_account : Cardinal;
  Begin
    FNode.OperationSequenceLock.Acquire;  // Use lock to prevent N_Operation race-condition on concurrent invocations
    try
      Result := False;
      OperationsHashTree := TOperationsHashTree.Create;
      try
        if (params.IndexOfName('account_signer')<0) then begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'Need account_signer param';
          Exit;
        end;
        c_account := params.AsCardinal('account_signer',0);
        if (c_account<0) or (c_account>=FNode.Bank.AccountsCount) then begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'Invalid account_signer '+params.AsString('account_signer','');
          Exit;
        end;
        account_signer := FNode.GetMempoolAccount(c_account);
        if (params.IndexOfName('account_target')<0) then begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'Need account_target param';
          Exit;
        end;
        c_account := params.AsCardinal('account_target',0);
        if (c_account<0) or (c_account>=FNode.Bank.AccountsCount) then begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'Invalid account_target '+params.AsString('account_target','');
          Exit;
        end;
        account_target := FNode.GetMempoolAccount(c_account);
        if (Not TAccountComp.EqualAccountKeys(account_signer.accountInfo.accountKey,account_target.accountInfo.accountKey)) then begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'account_signer and account_target have distinct keys. Cannot sign';
          Exit;
        end;
        If not SignDelistAccountForSaleEx(params,OperationsHashTree,FNode.Bank.Vault.CurrentProtocol,account_signer.accountInfo.accountKey,account_signer.n_operation) then Exit;
        opt := OperationsHashTree.GetOperation(0);
        If not FNode.AddOperation(Nil,opt,errors) then begin
          ErrorNum := CT_RPC_ErrNum_InternalError;
          ErrorDesc := errors;
          Exit;
        end else Result := True;
        TABEYOperation.OperationToOperationResume(0,opt,False,c_account,opr);
        FillOperationResumeToJSONObject(opr,GetResultObject);
      finally
        OperationsHashTree.Free;
      end;
    finally
      FNode.OperationSequenceLock.Acquire;
    end;
  End;

  function BuyAccount(params : TABEYJSONObject) : boolean;
  Var OperationsHashTree : TOperationsHashTree;
    buyer_account, account_to_purchase : TAccount;
    opt : TABEYOperation;
    opr : TOperationResume;
    errors : String;
    c_account : Cardinal;
  Begin
    FNode.OperationSequenceLock.Acquire;  // Use lock to prevent N_Operation race-condition on concurrent invocations
    try
      Result := False;
      OperationsHashTree := TOperationsHashTree.Create;
      try
        if (params.IndexOfName('buyer_account')<0) then begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'Need buyer_account param';
          Exit;
        end;
        c_account := params.AsCardinal('buyer_account',0);
        if (c_account<0) or (c_account>=FNode.Bank.AccountsCount) then begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'Invalid account '+params.AsString('buyer_account','');
          Exit;
        end;
        buyer_account := FNode.GetMempoolAccount(c_account);
        // Check params
        c_account := params.AsInteger('account_to_purchase',MaxInt);
        if (c_account<0) or (c_account>=FNode.Bank.AccountsCount) then begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'Invalid account to purchase '+params.AsString('account_to_purchase','');
          Exit;
        end;
        account_to_purchase := FNode.GetMempoolAccount(c_account);
        if Not TAccountComp.IsAccountForSale(account_to_purchase.accountInfo) then begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'Account is not for sale: '+params.AsString('account_to_purchase','');
          Exit;
        end;
        // Fill automatic params
        if (params.IndexOfName('price')<0) then
          params.GetAsVariant('price').Value := ToJSONCurrency( account_to_purchase.accountInfo.price );
        if (params.IndexOfName('seller_account')<0) then
          params.GetAsVariant('seller_account').Value := account_to_purchase.accountInfo.account_to_pay;
        If not SignBuyAccountEx(params,OperationsHashTree,FNode.Bank.Vault.CurrentProtocol,buyer_account.accountInfo.accountKey,buyer_account.n_operation) then Exit;
        opt := OperationsHashTree.GetOperation(0);
        If not FNode.AddOperation(Nil,opt,errors) then begin
          ErrorNum := CT_RPC_ErrNum_InternalError;
          ErrorDesc := errors;
          Exit;
        end else Result := True;
        TABEYOperation.OperationToOperationResume(0,opt,False,c_account,opr);
        FillOperationResumeToJSONObject(opr,GetResultObject);
      finally
        OperationsHashTree.Free;
      end;
    finally
      FNode.OperationSequenceLock.Release;
    end;
  End;

  function ChangeAccountInfo(params : TABEYJSONObject) : boolean;
  Var OperationsHashTree : TOperationsHashTree;
    account_signer, account_target : TAccount;
    opt : TABEYOperation;
    opr : TOperationResume;
    errors : String;
    c_account : Cardinal;
  Begin
    FNode.OperationSequenceLock.Acquire;  // Use lock to prevent N_Operation race-condition on concurrent invocations
    try
      Result := False;
      OperationsHashTree := TOperationsHashTree.Create;
      try
        if (params.IndexOfName('account_signer')<0) then begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'Need account_signer param';
          Exit;
        end;
        c_account := params.AsCardinal('account_signer',0);
        if (c_account<0) or (c_account>=FNode.Bank.AccountsCount) then begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'Invalid account_signer '+params.AsString('account_signer','');
          Exit;
        end;
        account_signer := FNode.GetMempoolAccount(c_account);
        if (params.IndexOfName('account_target')<0) then begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'Need account_target param';
          Exit;
        end;
        c_account := params.AsCardinal('account_target',0);
        if (c_account<0) or (c_account>=FNode.Bank.AccountsCount) then begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'Invalid account_target '+params.AsString('account_target','');
          Exit;
        end;
        account_target := FNode.GetMempoolAccount(c_account);
        if (Not TAccountComp.EqualAccountKeys(account_signer.accountInfo.accountKey,account_target.accountInfo.accountKey)) then begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'account_signer and account_target have distinct keys. Cannot sign';
          Exit;
        end;
        If not SignChangeAccountInfoEx(params,OperationsHashTree,FNode.Bank.Vault.CurrentProtocol,account_signer.accountInfo.accountKey,account_signer.n_operation) then Exit;
        opt := OperationsHashTree.GetOperation(0);
        If not FNode.AddOperation(Nil,opt,errors) then begin
          ErrorNum := CT_RPC_ErrNum_InternalError;
          ErrorDesc := errors;
          Exit;
        end else Result := True;
        TABEYOperation.OperationToOperationResume(0,opt,False,c_account,opr);
        FillOperationResumeToJSONObject(opr,GetResultObject);
      finally
        OperationsHashTree.Free;
      end;
    finally
      FNode.OperationSequenceLock.Release;
    end;
  End;


  function SaveLog(params : TABEYJSONObject) : boolean;
  var
    ContractId:Cardinal;
    ContractLogsDB: TABEYBlockchainDBStorage;
    PathToDb:String;
    LogData:String; // param 'data' of saveLog is stored here
    LogDataStream:TStream; // then LogData is written to stream and stored in ABEYdb

  begin

    Result := False;

    // Param "contractID"
    // Saves a contract log inside Logs folder with key ContractID
    if params.IndexOfName('data')<0 then begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Need "data" param';
      exit;
    end;

    LogData := params.GetAsVariant('data').AsString('');

    if LogData = '' then begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Invalid "data"';
      exit;
    end;

    ContractId := params.GetAsVariant('contractId').AsCardinal(CT_MaxAccount);
    //we treat contractId as an account since it is bassically the same thing

    if ( ContractId >= 0 ) And ( ContractId < FNode.Bank.AccountsCount ) then begin

      PathToDb := TFolderHelper.GetContractStorageFolder;
      ContractLogsDB := TABEYBlockchainDBStorage.GetInstance('Logs', (PathToDB), ErrorNum);

      if ErrorNum <> CT_SUCCESS then               // check to see if instance exists

      begin

          ErrorDesc := 'Error getting logging database for contracts, Error Number = ' + IntToStr(ErrorNum);

          Exit;

      end;

      ErrorNum := ContractLogsDB.ItemExists(IntToStr(ContractId));

      if (ErrorNum <> CT_ITEM_EXISTS_IN_TREE) then     //check if the item already exists or it is first time when storing contract logs
      begin
        //just insert

        LogData := '[' + DateTimeToStr(Now) + ']' + '[' + IntToStr(FNode.Bank.BlocksCount) +  ']' + LogData + ' ' ;   // [DateTime][BlockNumber]contractOutput
        LogDataStream := TMemoryStream.Create;
        LogDataStream.WriteAnsiString(AnsiString(LogData));

        ErrorNum := ContractLogsDB.InsertItem(IntToStr(ContractId),LogDataStream); // if 0 then 1 is the key,if 200 then 2001 is the key and so on
        if ErrorNum <> (CT_SUCCESS) then
        begin
          ErrorDesc := 'Error saving log of contract during execution, Error Number = ' + IntToStr(ErrorNum);
          Exit;
        end;

        LogDataStream.Free;

      end

      else begin

        //if log with contractId already exists, just replace

        LogData := '[' + DateTimeToStr(Now) + ']' + '[' + IntToStr(FNode.Bank.BlocksCount) +  ']' + LogData + ' ' ;   // [DateTime][BlockNumber]contractOutput
        LogDataStream := TMemoryStream.Create;
        LogDataStream.WriteAnsiString(AnsiString(LogData));

        ErrorNum := ContractLogsDB.InsertItem(IntToStr(ContractId),LogDataStream); // if 0 then 1 is the key,if 200 then 2001 is the key and so on
        if ErrorNum <> (CT_SUCCESS) then
        begin
          ErrorDesc := 'Error saving log of contract during execution, Error Number = ' + IntToStr(ErrorNum);
          Exit;
        end;

        LogDataStream.Free;

      end;
    jsonresponse.GetAsVariant('result').Value := True;
    Result:=True;
   end else begin
       ErrorNum := CT_RPC_ErrNum_InvalidAccount;
       ErrorDesc := 'Invalid contractId';
       exit;
   end;
  end;

  function AppendLog(params : TABEYJSONObject) : boolean;
  var
    ContractId:Cardinal;
    ContractLogsDB: TABEYBlockchainDBStorage;
    PathToDb:String;
    LogData:String; // param 'data' of appendLog is stored here
    LogDataStream:TStream; // then LogData is written to stream and stored in ABEYdb

  begin

    Result := False;

    // Param "contractID"
    // Appends to contract log inside Logs folder with key ContractID
    if params.IndexOfName('data')<0 then begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Need "data" param';
      exit;
    end;

    LogData := params.GetAsVariant('data').AsString('');

    if LogData = '' then begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Invalid "data"';
      exit;
    end;

    ContractId := params.GetAsVariant('contractId').AsCardinal(CT_MaxAccount);
    //we treat contractId as an account since it is bassically the same thing

    if ( ContractId >= 0 ) And ( ContractId < FNode.Bank.AccountsCount ) then begin

      PathToDb := TFolderHelper.GetContractStorageFolder;
      ContractLogsDB := TABEYBlockchainDBStorage.GetInstance('Logs', (PathToDB), ErrorNum);

      if ErrorNum <> CT_SUCCESS then               // check to see if instance exists

      begin

          ErrorDesc := 'Error getting logging database for contracts, Error Number = ' + IntToStr(ErrorNum);

          Exit;

      end;

      ErrorNum := ContractLogsDB.ItemExists(IntToStr(ContractId));

      if (ErrorNum <> CT_ITEM_EXISTS_IN_TREE) then     //check if the item already exists or it is first time when storing contract logs
      begin

       //just insert

        LogData := '[' + DateTimeToStr(Now) + ']' + '[' + IntToStr(FNode.Bank.BlocksCount) +  ']' + LogData + ' ' ;   // [DateTime][BlockNumber]contractOutput
        LogDataStream := TMemoryStream.Create;
        LogDataStream.WriteAnsiString(AnsiString(LogData));

        ErrorNum := ContractLogsDB.InsertItem(IntToStr(ContractId),LogDataStream); // if 0 then 1 is the key,if 200 then 2001 is the key and so on
        if ErrorNum <> (CT_SUCCESS) then
        begin
          ErrorDesc := 'Error saving log of contract during execution, Error Number = ' + IntToStr(ErrorNum);
          Exit;
        end;

        LogDataStream.Free;

      end

      else begin
        //get the data already available for contract logs and append to its content


        LogData := '[' + DateTimeToStr(Now) + ']' + '[' + IntToStr(FNode.Bank.BlocksCount) +  ']' + LogData + ' ' ;
        LogDataStream := TMemoryStream.Create;


        ErrorNum := ContractLogsDB.FindItem ( IntToStr( ContractId ) , LogDataStream );

        if ErrorNum <> CT_SUCCESS then begin

          ErrorNum := CT_RPC_ErrNum_InvalidData;
          ErrorDesc := 'Error getting Logs for ContractId '+ IntToStr(ContractId) +' , Error Number = ' + IntToStr(ErrorNum);
          Exit;
        end;


        //set stream at position 0 from beggining
        LogDataStream.Seek(0,soBeginning);

        //read the content and add Carriage return and Line feed
        LogData := LogDataStream.ReadAnsiString + #13#10 + LogData;

        //reset position bc after read  is soBeginning + Sizeof(byte) * number of bytes read
        LogDataStream.Seek(0,soBeginning);

        //put it back into stream
        LogDataStream.WriteAnsiString(AnsiString(LogData));

        ErrorNum := ContractLogsDB.InsertItem(IntToStr(ContractId),LogDataStream,True);

        if ErrorNum <> (CT_SUCCESS) then
        begin
          ErrorDesc := 'Error saving log of contract during execution, Error Number = ' + IntToStr(ErrorNum);
          Exit;
        end;

        LogDataStream.Free;

      end;
    jsonresponse.GetAsVariant('result').Value := True;
    Result:=True;
   end else begin
       ErrorNum := CT_RPC_ErrNum_InvalidAccount;
       ErrorDesc := 'Invalid contractId';
       exit;
   end;
  end;

  function RetrieveLog(params : TABEYJSONObject;var output: TABEYJSONObject) : boolean;
    var
      ContractId:Cardinal;
      PathToDb :String;
      LogData:String;
      LogDataStream:TStream;
      ContractLogsDB : TABEYBlockchainDBStorage;
    begin

      Result := False;

      //check contractId param exists
      if params.IndexOfName( 'contractId' ) < 0 then begin
        ErrorNum := CT_RPC_ErrNum_InvalidData;
        ErrorDesc := 'Need "contractId" param';
        exit;
      end;

      ContractId := params.GetAsVariant('contractId').AsCardinal(CT_MaxAccount);

      //check contracId is between 0 and total number of accounts
      if ( ContractId < 0 ) Or ( ContractId >= FNode.Bank.AccountsCount ) then begin

        ErrorNum := CT_RPC_ErrNum_InvalidAccount;

        if (ContractId=CT_MaxAccount) then
           ErrorDesc := 'Need "contract" param'
        else ErrorDesc := 'contract not found: '+IntToStr(ContractId);

        Exit;

      end;

     //get database for logging contracts
     PathToDb := TFolderHelper.GetContractStorageFolder;
     ContractLogsDB := TABEYBlockchainDBStorage.GetInstance('Logs', (PathToDB), ErrorNum);
     LogDataStream := TMemoryStream.Create;


     //check item exists in database
    ErrorNum := ContractLogsDB.ItemExists(IntToStr(ContractId));
    if (ErrorNum <> CT_ITEM_EXISTS_IN_TREE) then
    begin
        ErrorDesc := 'Error getting Logs for ContractId '+ IntToStr(ContractId) +' , Error Number = ' + IntToStr(ErrorNum);
        Exit;
    end;

    //try to retrieve the item
    ErrorNum:=ContractLogsDB.FindItem ( IntToStr(ContractId) , LogDataStream );
    if ErrorNum <> CT_SUCCESS then
     begin
         ErrorDesc := 'Error getting Logs for ContractId '+ IntToStr(ContractId) +' , Error Number = ' + IntToStr(ErrorNum);
         Exit;
     end;

    //set stream head at position 0 relative to the begginging of the stream
    LogDataStream.Seek(0,soBeginning);

    Output := jsonresponse.GetAsObject('result');

    Output.GetAsVariant('data').Value:= LogDataStream.ReadAnsiString; //put the data inside 'data' param of stream

    LogDataStream.Free;

    Result := True;
  end;


function RetrieveLogs(params : TABEYJSONObject; var output : TABEYJSONArray) : boolean;
  var  f,l,s,e:Integer; // first , last , start , end ; end is reserver in Pascal so we use only first letters
                        // first and last should be used only separated -> you can get first N logs or last N logs and
                        // start and end can be used together or separated
       contractID:Cardinal;
       Index:Integer;
       LogData:String;
       LogDataStream:TStream;
       PathToDb : String;
       ContractLogsDB : TABEYBlockchainDBStorage;
       JSONLogArr:TABEYJSONArray;
       JSONLogObj:TABEYJSONObject;
       LogBytes:TBytes;
  begin


       Result := False;

       if params.IndexOfName('contractId')<0 then begin
          ErrorNum := CT_RPC_ErrNum_InvalidData;
          ErrorDesc := 'Need "contractId" param';
          exit;
       end;

       ContractId := params.GetAsVariant('contractId').AsCardinal(CT_MaxAccount);

       //check contracId is between 0 and total number of accounts
       if ( ContractId < 0) Or (ContractId >= FNode.Bank.AccountsCount ) then begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          if (ContractId=CT_MaxAccount) then
             ErrorDesc := 'Need "contract" param'
          else ErrorDesc := 'contract not found: '+IntToStr(ContractId);
          Exit;
       end;


       //get logs database
       PathToDb := TFolderHelper.GetContractStorageFolder;
       ContractLogsDB := TABEYBlockchainDBStorage.GetInstance('Logs'+IntToStr(ContractId), (PathToDB), ErrorNum);
       LogDataStream := TMemoryStream.Create;

       if ErrorNum <> CT_SUCCESS then
       begin
           ErrorDesc := 'Error getting Logs for ContractId '+ IntToStr(ContractId) +' in TOpExecuteContranct, Error Number = ' + IntToStr(ErrorNum);
           Exit;
       end;

       //case when 'first' param is used independently
       if ( params.IndexOfName('first') >= 0 ) and ( params.IndexOfName('last')<0 ) and ( params.IndexOfName('start')<0 )  and ( params.IndexOfName('end')<0 ) then begin

         f := params.GetAsVariant('first').AsCardinal(CT_MaxAccount);

         //check if start parameter is between 0 and total number of accounts
         if ( f < 0) Or ( f >= FNode.Bank.AccountsCount ) then begin
            ErrorNum := CT_RPC_ErrNum_InvalidAccount;
            if (f = CT_MaxAccount) then
               ErrorDesc := 'Need "first" param'
            else ErrorDesc := 'contract not found: '+IntToStr(f);
            Exit;
         end;

         JSONLogArr := TABEYJSONArray.Create;
         JSONLogArr := jsonresponse.GetAsArray('result');
         JSONLogObj := TABEYJSONObject.Create;


         LogDataStream := TMemoryStream.Create;

          ErrorNum := ContractLogsDB.ItemExists(IntToStr(1));

          if (ErrorNum <> CT_ITEM_EXISTS_IN_TREE) then
          begin
              ErrorDesc := 'Error getting bytecode and exporttable from contract, Error Number = ' + IntToStr(ErrorNum);
              Exit;
          end;
         ContractLogsDB.FindItem ( IntToStr(1) , LogDataStream );


         LogDataStream.Seek(0,soBeginning);


         JSONLogObj.GetAsVariant(IntToStr(1)).Value:= LogDataStream.ReadAnsiString;


         output := jsonresponse.GetAsArray('result');
         output.Insert( 0 , JSONLogObj );


         LogDataStream.Free;

       Result := True;


       end;

       //case when 'last' param is used independently
       if (params.IndexOfName('last')>=0) and (params.IndexOfName('first')<0) and (params.IndexOfName('start')<0) and (params.IndexOfName('end')<0 ) then begin



       end;

       //case where 'start' param is used independently
       if (params.IndexOfName('start')>=0) and (params.IndexOfName('first')<0) and (params.IndexOfName('last')<0) then begin

       end;

       //case where 'end' param is used independently
       if (params.IndexOfName('end')>=0 ) and (params.IndexOfName('first')<0) and (params.IndexOfName('last')<0) then begin

       end;





       ContractId := params.GetAsVariant('contractId').AsCardinal(CT_MaxAccount);



      LogData := '[' + DateTimeToStr(Now) + ']' + '[' + IntToStr(FNode.Bank.BlocksCount) +  ']' + LogData  ;
      LogDataStream := TMemoryStream.Create;
      LogDataStream.WriteAnsiString(LogData);

      PathToDb := TFolderHelper.GetContractStorageFolder;
      ContractLogsDB := TABEYBlockchainDBStorage.GetInstance('Logs'+IntToStr(ContractId), (PathToDB), ErrorNum);

      if ErrorNum <> CT_SUCCESS then               // check to see if
      begin
           ErrorDesc := 'Error accesing logs for contractId '+ IntToStr(ContractId) +', Error Number = ' + IntToStr(ErrorNum);
           Exit;
      end;

      ErrorNum := ContractLogsDB.InsertItem(IntToStr(ContractLogsDB.GetNumberOfItems() + 1 ),LogDataStream); // if 0 then 1 is the key,if 200 then 2001 is the key and so on
      if ErrorNum <> (CT_SUCCESS) then
      begin
        ErrorDesc := 'Error saving log of contract during execution, Error Number = ' + IntToStr(ErrorNum);
        Exit;
      end;
  end;


  function RetrieveContract(params : TABEYJSONObject; var output : TABEYJSONObject) : boolean;
  var
    ContractId : Cardinal ;
    PathToDb : String ;

    ContractInitializationDatabase : TABEYBlockchainDBStorage;
    ContractExecutionDatabase : TABEYBlockchainDBStorage ;

    ContractInitializationData: TStream;
    ContractExecutionData: TStream;

    Bytecode:TRawBytes;
    ExportTable:TRawBytes;
    DataSegment:TRawBytes;

    flags : Cardinal;
    startBlocks: Cardinal ;
    activeBlocks: Cardinal ;
    passiveBlocks: Cardinal ;
    instructionCount: Cardinal ;
    memoryUsage: Cardinal;
    runtimeSeconds: Cardinal ;
    ioUsage: Cardinal;
    dataSegSize: Cardinal;
    exportTableSize: Cardinal ;
    bytecodeSize: Cardinal ;



  begin
    Result:= False;


    //check contractId param exists
    if params.IndexOfName( 'contractId' ) < 0 then begin

      ErrorNum := CT_RPC_ErrNum_InvalidData;

      ErrorDesc := 'Need "contractId" param';

      exit;

    end;

    ContractId := params.GetAsVariant( 'contractId' ).AsCardinal( CT_MaxAccount );

    //check contracId is between 0 and total number of accounts
    if ( ContractId < 0 ) Or ( ContractId >= FNode.Bank.AccountsCount ) then begin

      ErrorNum := CT_RPC_ErrNum_InvalidAccount;

      if (ContractId=CT_MaxAccount) then
         ErrorDesc := 'Need "contract" param'
      else ErrorDesc := 'contract not found: '+IntToStr(ContractId);

      Exit;

    end;

   //get database for saving contracts
   PathToDb := TFolderHelper.GetContractStorageFolder;

   ContractInitializationDatabase := TABEYBlockchainDBStorage.GetInstance('Initialization', (PathToDB), ErrorNum);

   // check to see if instance exists

   if ErrorNum <> CT_SUCCESS then

      begin

          ErrorDesc := 'Error getting initialization database for contracts, Error Number = ' + IntToStr(ErrorNum);

          Exit;

      end;


   ErrorNum := ContractInitializationDatabase.ItemExists( IntToStr( ContractId ) );

   if ( ErrorNum <> CT_ITEM_EXISTS_IN_TREE ) then

   begin

       ErrorDesc := 'Error getting contract data for ContractId '+ IntToStr(ContractId) +' , Error Number = ' + IntToStr(ErrorNum);

       Exit;

   end;

   ContractInitializationData := TMemoryStream.Create;

   ErrorNum := ContractInitializationDatabase.FindItem( IntToStr( ContractId ) , ContractInitializationData);

   if ErrorNum <> CT_SUCCESS then begin

      ErrorNum := CT_RPC_ErrNum_InvalidData;

      ErrorDesc := 'Error getting contract data for ContractId '+ IntToStr(ContractId) +' , Error Number = ' + IntToStr(ErrorNum);

      Exit;

   end;

   ContractInitializationData.Seek(0,soBeginning);

   flags              := ContractInitializationData.ReadDWord;
   startBlocks        := ContractInitializationData.ReadDWord;
   activeBlocks       := ContractInitializationData.ReadDWord;
   passiveBlocks      := ContractInitializationData.ReadDWord;
   instructionCount   := ContractInitializationData.ReadDWord;
   memoryUsage        := ContractInitializationData.ReadDWord;
   runtimeSeconds     := ContractInitializationData.ReadDWord;
   ioUsage            := ContractInitializationData.ReadDWord;
   bytecodeSize       := ContractInitializationData.ReadDWord;
   exportTableSize    := ContractInitializationData.ReadDWord;
   dataSegSize        := ContractInitializationData.ReadDWord;



   SetLength(Bytecode,bytecodeSize);
   ContractInitializationData.ReadBuffer(Bytecode[0],bytecodeSize);

   SetLength(ExportTable,exportTableSize);
   ContractInitializationData.ReadBuffer(ExportTable[0],exportTableSize);







   Output := jsonresponse.GetAsObject('result');

   Output.GetAsVariant('flags').Value := flags ;

   Output.GetAsVariant('startBlocks').Value := startBlocks ;

   Output.GetAsVariant('activeBlocks').Value := activeBlocks ;

   Output.GetAsVariant('passiveBlocks').Value := passiveBlocks ;

   Output.GetAsVariant('instructionCount').Value := instructionCount ;

   Output.GetAsVariant('memoryUsage').Value := memoryUsage ;
   
   Output.GetAsVariant('runtimeSeconds').Value := runtimeSeconds ;

   Output.GetAsVariant('ioUsage').Value := ioUsage ;

   Output.GetAsVariant('dataSegSize').Value := dataSegSize ;

   Output.GetAsVariant('exportTableSize').Value := exportTableSize ;

   Output.GetAsVariant('bytecodeSize').Value := bytecodeSize ;

   Output.GetAsVariant('bytecode').Value := TCrypto.ToHexaString(Bytecode);
   Output.GetAsVariant('exportTable').Value := TCrypto.ToHexaString(ExportTable);
    DebugLn('gere');

   //get database for storing contract states ( during execution )

   PathToDb := TFolderHelper.GetContractStorageFolder;

   ContractExecutionDatabase := TABEYBlockchainDBStorage.GetInstance('Execution', (PathToDB), ErrorNum);

   DebugLn('gere');
   // check to see if instance exists

   if ErrorNum <> CT_SUCCESS then

      begin

          ErrorDesc := 'Error getting execution database for contracts, Error Number = ' + IntToStr(ErrorNum);

          Exit;

      end;

   ErrorNum := ContractExecutionDatabase.ItemExists( IntToStr( ContractId ) );

   if ( ErrorNum = CT_ITEM_EXISTS_IN_TREE ) then

   begin

     // if data segment is available in db, put it inside "dataSegment" param; else do nothing

       ContractExecutionData := TMemoryStream.Create;

       ErrorNum := ContractExecutionDatabase.FindItem( IntToStr( ContractId ) , ContractExecutionData);

       if ErrorNum <> CT_SUCCESS then begin

          ErrorNum := CT_RPC_ErrNum_InvalidData;

          ErrorDesc := 'Error getting contract data for ContractId '+ IntToStr(ContractId) +' , Error Number = ' + IntToStr(ErrorNum);

          Exit;

       end;

       ContractExecutionData.Seek(0,soBeginning);
       SetLength(DataSegment,dataSegSize) ;

       ContractExecutionData.ReadBuffer(DataSegment[0],ContractExecutionData.Size);        // dataSegment size is stored in ContractInitializationData
       Output.GetAsVariant('dataSegment').Value := TCrypto.ToHexaString(DataSegment);   // convert to hex string




   end;

   ContractInitializationData.Free;

   Result := True ;

  end;

  function FindAccounts(params : TABEYJSONObject; var output: TABEYJSONArray) : boolean;
  var
    accountName : TRawBytes;
    accountType : Integer;
    accountNumber : Integer;
    accountBalanceMin : Int64;
    accountBalanceMax : Int64;
    accountForSale, searchByPubkey : Boolean;
    exactMatch : Boolean;
    start, max, iPubKey : Integer;
    account : TAccount;
    i : Cardinal;
    errors : String;
    auxErrors : String;
    addToResult : Boolean;
    accPubKey : TAccountKey;
  begin
    // Get Parameters
    Result := False;
    accountName.FromString({LowerCase(}params.AsString('name', '')); // Convert to lowercase...
    accountType := params.AsInteger('type', -1);
    start := params.AsInteger('start', 0);
    max := params.AsInteger('max', 100);
    accountForSale := params.AsBoolean('listed',false);
    exactMatch := params.AsBoolean('exact',true);
    accountBalanceMin := ToABEYs(params.AsDouble('min_balance',-1));
    accountBalanceMax := ToABEYs(params.AsDouble('max_balance',-1));
    // Validate Parameters
    if (Length(accountName)>0) And (exactMatch) then begin
      if not FNode.Bank.Vault.ValidAccountName(accountName, errors) then begin
        ErrorNum := CT_RPC_ErrNum_InvalidAccountName;
        ErrorDesc := errors;
        exit;
      end;
    end;

    if start < 0 then begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := '"start" param must be >=0';
      exit;
    end;
    if max <= 0 then begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := '"max" param must be greater than zero';
      exit;
    end;

    // Declare return result (empty by default)
    output := jsonresponse.GetAsArray('result');

    // Search by accPubKey (if provided)
    If CapturePubKey('',accPubKey,auxErrors) then begin
      // Must match accPubKey
      if (Not Assigned(FNode.Bank.Vault.OrderedAccountKeysList)) then begin
        ErrorNum := CT_RPC_ErrNum_NotImplemented;
        ErrorDesc := 'Not allowed search by public key';
        Exit;
      end;
      searchByPubkey := True;
      iPubKey := FNode.Bank.Vault.OrderedAccountKeysList.IndexOfAccountKey(accPubKey);
      if (iPubKey<0) then begin
        // No account available with this pubkey
        Exit;
      end;
    end else searchByPubkey := False;
    // Search by name
    if ((Length(accountName)>0) AND (exactMatch=true)) then begin
       accountNumber := FNode.Bank.Vault.FindAccountByName(accountName);
       if accountNumber >= 0 then begin
          account := FNode.GetMempoolAccount(accountNumber);
          if ((accountType = -1) OR (Integer(account.account_type) = accountType))
             AND
             ((Not searchByPubkey) OR (TAccountComp.EqualAccountKeys(accPubKey,account.accountInfo.accountKey))) then
             TABEYJSONComp.FillAccountObject(account,output.GetAsObject(output.Count));
       end;
    end else begin
      // Search by type-forSale-balance
      for i := start to FNode.Bank.AccountsCount - 1 do begin
        if (searchByPubkey) then begin
          if (i>=FNode.Bank.Vault.OrderedAccountKeysList.AccountKeyList[iPubKey].Count) then Break;
          account := FNode.GetMempoolAccount( FNode.Bank.Vault.OrderedAccountKeysList.AccountKeyList[iPubKey].Get(i) );
        end else begin
          account := FNode.GetMempoolAccount(i);
        end;
        if (accountType <> -1) AND (Integer(account.account_type) <> accountType) then
        begin
           Continue;
        end;

        if ((Length(accountName)>0) AND (TBaseType.StartsWith(accountName,account.name))) then
        begin
          Continue
        end;
        if ((accountForSale = true) AND (account.accountInfo.state <> as_ForSale)) then
        begin
           Continue;
        end;
        if ((accountBalanceMin>0) AND (accountBalanceMax<0) AND (account.balance<accountBalanceMin)) then
        begin
           Continue;
        end;
        if (accountBalanceMin>0) AND (accountBalanceMax>0) AND ((account.balance<accountBalanceMin) OR (account.balance>accountBalanceMax)) then
        begin
            Continue;
        end;
        if ((accountBalanceMin<0) AND (accountBalanceMax>0) AND (account.balance>accountBalanceMax)) then
        begin
            Continue;
        end;
        TABEYJSONComp.FillAccountObject(account,output.GetAsObject(output.Count));
        if output.Count>=max then break;
      end;
    end;
    Result := True;
  end;

  function FindNOperations : Boolean;
  Var oprl : TOperationsResumeList;
    start_block, account, n_operation_min, n_operation_max : Cardinal;
    sor : TSearchOperationResult;
    jsonarr : TABEYJSONArray;
    i : Integer;
  begin
    Result := False;
    oprl := TOperationsResumeList.Create;
    try
      account := params.AsCardinal('account',MaxInt);
      If (params.IndexOfName('n_operation_min')<0) Or (params.IndexOfName('n_operation_max')<0) then begin
        ErrorNum:=CT_RPC_ErrNum_NotFound;
        ErrorDesc:='Need n_operation_min and n_operation_max params';
        exit;
      end;
      n_operation_min := params.AsCardinal('n_operation_min',0);
      n_operation_max := params.AsCardinal('n_operation_max',0);
      start_block := params.AsCardinal('start_block',0); // Optional: 0 = Search all
      sor := FNode.FindNOperations(account,start_block,true,n_operation_min,n_operation_max,oprl);
      Case sor of
        found : Result := True;
        invalid_params : begin
            ErrorNum:=CT_RPC_ErrNum_NotFound;
            ErrorDesc:='Not found using block/account/n_operation';
            exit;
          end;
        blockchain_block_not_found : begin
            ErrorNum := CT_RPC_ErrNum_InvalidBlock;
            ErrorDesc:='Blockchain file does not contain all blocks to find';
            exit;
          end;
      else Raise Exception.Create('ERROR DEV 20171120-7');
      end;
      jsonarr := jsonresponse.GetAsArray('result');
      if oprl.Count>0 then begin;
        for i:=0 to oprl.Count-1 do begin
          FillOperationResumeToJSONObject(oprl.OperationResume[i],jsonarr.GetAsObject(jsonarr.Count));
        end;
      end;
    finally
      oprl.Free;
    end;
  end;

  function MultiOperationAddOperation(Const HexaStringOperationsHashTree : String; params : TABEYJSONObject) : boolean;
    Function Capture_Current_Account(const nAccount : Int64) : TAccount;
    Begin
      Result := CT_Account_NUL;
      if (nAccount<0) Or (nAccount>=FNode.Bank.AccountsCount) then Exit;
      Result := FNode.GetMempoolAccount( nAccount );
    end;

  var errors : String;
    OperationsHashTree : TOperationsHashTree;
    jsonArr : TABEYJSONArray;
    i,j : Integer;
    sender : TMultiOpSender;
    receiver : TMultiOpReceiver;
    changeinfo : TMultiOpChangeInfo;
    mop : TOpMultiOperation;
  begin
    { This will ADD or UPDATE a MultiOperation with NEW field/s
      - UPDATE: If LAST operation in HexaStringOperationsHashTree RAW value contains a MultiOperation
      - ADD: Otherwise

      NOTE: This function will not check if provided info is valid (enough balance, valid n_operation...), and can work for COLD STORAGE
      - "senders" : ARRAY of OBJECT
        - "account" :
        - "n_operation" : New n_operation value for account (remember, current + 1)
        - "amount" : PASCURRENCY
        - "payload" : HEXASTRING (optional)
      - "receivers" : ARRAY of OBJECT
        - "account"
        - "amount" : PASCURRENCY
        - "payload" : HEXASTRING (optional)
      - "changesinfo" : ARRAY of OBJECT
        - "account"
        - "n_operation" : New n_operation value for account (remember, current + 1)
        - "new_b58_pubkey" or "new_enc_pubkey" : (optional) The new public key for this account
        - "new_name" : (optional) The new account name
        - "new_type" : (optional) The new account type
        }
    Result := false;
    if Not HexaStringToOperationsHashTreeAndGetMultioperation(HexaStringOperationsHashTree,True,OperationsHashTree,mop,errors) then begin
      ErrorNum:=CT_RPC_ErrNum_InvalidData;
      ErrorDesc:= 'Error decoding param previous operations hash tree raw value: '+errors;
      Exit;
    end;
    Try
      // "senders"
      jsonArr := params.GetAsArray('senders');
      for i:=0 to jsonArr.Count-1 do begin
        sender := CT_TMultiOpSender_NUL;
        j := jsonArr.GetAsObject(i).AsInteger('account',-1);
        if j<0 then begin
          ErrorNum := CT_RPC_ErrNum_InvalidData;
          ErrorDesc := 'Field "account" for "senders" array not found';
          Exit;
        end;
        sender.Account := j;
        sender.Amount:= ToABEYs(jsonArr.GetAsObject(i).AsDouble('amount',0));
        sender.N_Operation:=jsonArr.GetAsObject(i).AsInteger('n_operation',0);
        // Update N_Operation with valid info
        if (sender.N_Operation<=0) then sender.N_Operation:=Capture_Current_Account(sender.Account).n_operation+1;

        sender.Payload:=TCrypto.HexaToRaw(jsonArr.GetAsObject(i).AsString('payload',''));
        if Not mop.AddTxSender(sender) then begin
          ErrorNum := CT_RPC_ErrNum_InvalidData;
          ErrorDesc := 'Cannot add sender '+inttostr(sender.Account)+' duplicated or invalid data';
          Exit;
        end;
      end;
      // "receivers"
      jsonArr := params.GetAsArray('receivers');
      for i:=0 to jsonArr.Count-1 do begin
        receiver := CT_TMultiOpReceiver_NUL;
        j := jsonArr.GetAsObject(i).AsInteger('account',-1);
        if j<0 then begin
          ErrorNum := CT_RPC_ErrNum_InvalidData;
          ErrorDesc := 'Field "account" for "receivers" array not found';
          Exit;
        end;
        receiver.Account := j;
        receiver.Amount:= ToABEYs(jsonArr.GetAsObject(i).AsDouble('amount',0));
        receiver.Payload:=TCrypto.HexaToRaw(jsonArr.GetAsObject(i).AsString('payload',''));
        if Not mop.AddTxReceiver(receiver) then begin
          ErrorNum := CT_RPC_ErrNum_InvalidData;
          ErrorDesc := 'Cannot add receiver '+inttostr(receiver.Account)+' invalid data';
          Exit;
        end;
      end;
      // "changesinfo"
      jsonArr := params.GetAsArray('changesinfo');
      for i:=0 to jsonArr.Count-1 do begin
        changeinfo := CT_TMultiOpChangeInfo_NUL;
        j := jsonArr.GetAsObject(i).AsInteger('account',-1);
        if j<0 then begin
          ErrorNum := CT_RPC_ErrNum_InvalidData;
          ErrorDesc := 'Field "account" for "changesinfo" array not found';
          Exit;
        end;
        changeinfo.Account := j;
        changeinfo.N_Operation:=jsonArr.GetAsObject(i).AsInteger('n_operation',0);
        // Update N_Operation with valid info
        if (changeinfo.N_Operation<=0) then changeinfo.N_Operation:=Capture_Current_Account(changeinfo.Account).n_operation+1;

        if (jsonArr.GetAsObject(i).IndexOfName('new_b58_pubkey')>=0) or (jsonArr.GetAsObject(i).IndexOfName('new_enc_pubkey')>=0) then begin
          changeinfo.Changes_type:=changeinfo.Changes_type + [public_key];
          If Not CapturePubKeyExt(jsonArr.GetAsObject(i),'new_',changeinfo.New_Accountkey,ErrorDesc) then begin
            ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
            Exit;
          end;
        end;
        if (jsonArr.GetAsObject(i).IndexOfName('new_name')>=0) then begin
          changeinfo.Changes_type:=changeinfo.Changes_type + [account_name];
          changeinfo.New_Name.FromString(jsonArr.GetAsObject(i).AsString('new_name',''));
        end;
        if (jsonArr.GetAsObject(i).IndexOfName('new_type')>=0) then begin
          changeinfo.Changes_type:=changeinfo.Changes_type + [account_type];
          changeinfo.New_Type:=jsonArr.GetAsObject(i).AsInteger('new_type',0);
        end;
        if (changeinfo.Changes_type = []) then begin
          ErrorNum:=CT_RPC_ErrNum_InvalidData;
          ErrorDesc:='Need change something for account '+inttostr(changeinfo.Account);
          Exit;
        end;
        if Not mop.AddChangeInfo(changeinfo) then begin
          ErrorNum := CT_RPC_ErrNum_InvalidData;
          ErrorDesc := 'Cannot add receiver '+inttostr(receiver.Account)+' duplicated or invalid data';
          Exit;
        end;
      end;
      // Return multioperation object:
      TABEYJSONComp.FillMultiOperationObject(FNode.Bank.Vault.CurrentProtocol,mop,GetResultObject);
    finally
      OperationsHashTree.Free;
    end;
    Result := True;
  end;

  function DoSignOrVerifyMessage(params : TABEYJSONObject) : boolean;
    { Will sign data or verify a signature
      In params:
        - "digest" : HEXASTRING containing data to sign
        - "b58_pubkey" or "enc_pubkey" : The public key that must sign "digest" data
        - "signature" : (optional) HEXASTRING If provided, will check if "digest" data is signed by "_pubkey" provided
      Out object:
        - "digest" : HEXASTRING containing data
        - "b58_pubkey" or "enc_pubkey" : The public key that have signed
        - "signature" : HEXASTRING with
      If validation is incorrect or errors, will return an error object }
  var digest : TRawBytes;
    pubKey : TAccountKey;
    signature : TECDSA_SIG;
    iKey : Integer;
  begin
    Result := False;
    if Not TCrypto.HexaToRaw( params.AsString('digest',''),digest ) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc:= 'Param digest with invalid hexadecimal data';
      Exit;
    end;
    If Not CapturePubKeyExt(params,'',pubKey,ErrorDesc) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
      Exit;
    end;
    if (params.IndexOfName('signature')>=0) then begin
      // Verify
      If Not TCrypto.DecodeSignature( TCrypto.HexaToRaw(params.AsString('signature','')),signature) then begin
        ErrorNum := CT_RPC_ErrNum_InvalidData;
        ErrorDesc:= 'Param signature with invalid data';
        Exit;
      end;
      //
      If TCrypto.ECDSAVerify(pubKey,digest,signature) then begin
        GetResultObject.GetAsVariant('digest').Value:=TCrypto.ToHexaString(digest);
        GetResultObject.GetAsVariant('enc_pubkey').Value:=TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(pubKey));
        GetResultObject.GetAsVariant('signature').Value:=TCrypto.ToHexaString(TCrypto.EncodeSignature(signature));
        Result := True;
      end else begin
        // Invalid signature
        ErrorNum := CT_RPC_ErrNum_InvalidSignature;
        ErrorDesc := 'Signature does not match';
        Exit;
      end;
    end else begin
      // Sign process
      if (Not (_RPCServer.FWalletKeys.IsValidPassword)) then begin
        // Wallet is password protected
        ErrorDesc := 'Wallet is password protected';
        ErrorNum := CT_RPC_ErrNum_WalletPasswordProtected;
        Exit;
      end;
      iKey := _RPCServer.FWalletKeys.IndexOfAccountKey(pubKey);
      if (iKey<0) then begin
        ErrorDesc:= 'Public Key not found in wallet: '+TAccountComp.AccountPublicKeyExport(pubKey);
        ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
        Exit;
      end;
      if (Not Assigned(_RPCServer.FWalletKeys.Key[iKey].PrivateKey)) then begin
        ErrorDesc:= 'Private key from public Key not found in wallet: '+TAccountComp.AccountPublicKeyExport(pubKey);
        ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
        Exit;
      end;
      signature := TCrypto.ECDSASign( _RPCServer.FWalletKeys.Key[iKey].PrivateKey.PrivateKey ,digest );
      //
      GetResultObject.GetAsVariant('digest').Value:=TCrypto.ToHexaString(digest);
      GetResultObject.GetAsVariant('enc_pubkey').Value:=TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(pubKey));
      GetResultObject.GetAsVariant('signature').Value:=TCrypto.ToHexaString(TCrypto.EncodeSignature(signature));
      Result := True;
    end;
  end;

  procedure InternalMultiOperationSignCold(multiOperation : TOpMultiOperation; current_protocol : Word; accounts_and_keys : TABEYJSONArray; var signedAccounts : Integer);
    { Signs a multioperation in a Cold storage, so cannot check if current signatures are valid because public keys of accounts are unknown
      accounts_and_keys is a JSON ARRAY with Objects:
      - "account"
      - "b58_pubkey" or "enc_pubkey" : The public key of the "account"
      - "protocol"
    }
  var i,iKey : Integer;
    pubKey : TAccountKey;
    nAccount : Cardinal;
    _error_desc : String;
  begin
    signedAccounts := 0;
    if (Not (_RPCServer.FWalletKeys.IsValidPassword)) then Exit;
    for i := 0 to accounts_and_keys.Count-1 do begin
      nAccount := accounts_and_keys.GetAsObject(i).AsCardinal('account',CT_MaxAccount);
      If CapturePubKeyExt(accounts_and_keys.GetAsObject(i),'',pubKey,_error_desc) then begin
        iKey := _RPCServer.FWalletKeys.IndexOfAccountKey(pubKey);
        if (iKey>=0) then begin
          if (Assigned(_RPCServer.FWalletKeys.Key[iKey].PrivateKey)) then begin
            inc(signedAccounts,multiOperation.DoSignMultiOperationSigner(current_protocol, nAccount,_RPCServer.FWalletKeys.Key[iKey].PrivateKey));
          end;
        end;
      end;
    end;
  end;

  function MultiOperationSignCold(Const HexaStringOperationsHashTree : String; params : TABEYJSONObject) : boolean;
  var errors : String;
    senderOperationsHashTree : TOperationsHashTree;
    mop : TOpMultiOperation;
    i,j : Integer;
    protocol : Word;
  begin
    { This will SIGN a MultiOperation on a HexaStringOperationsHashTree in COLD mode (without knowledge of current public keys)
      Must provide param "accounts_and_keys"
      - "accounts_and_keys" is a JSON ARRAY with Objects:
        - "account"
        - "b58_pubkey" or "enc_pubkey" : The public key of the "account"
      Must provide "protocol" version, by default will use current build protocol
      Will Return an OperationsHashTree Object
    }
    Result := false;
    if (Not (_RPCServer.FWalletKeys.IsValidPassword)) then begin
      // Wallet is password protected
      ErrorDesc := 'Wallet is password protected';
      ErrorNum := CT_RPC_ErrNum_WalletPasswordProtected;
      Exit;
    end;
    protocol := params.GetAsVariant('protocol').AsCardinal(CT_BUILD_PROTOCOL);
    if Not HexaStringToOperationsHashTreeAndGetMultioperation(HexaStringOperationsHashTree,False,senderOperationsHashTree,mop,errors) then begin
      ErrorNum:=CT_RPC_ErrNum_InvalidData;
      ErrorDesc:= 'Error decoding param previous operations hash tree raw value: '+errors;
      Exit;
    end;
    Try
      InternalMultiOperationSignCold(mop,protocol,params.GetAsArray('accounts_and_keys'),j);
      // Return multioperation object:
      TABEYJSONComp.FillMultiOperationObject(protocol,mop,GetResultObject);
      Result := True;
    finally
      senderOperationsHashTree.Free;
    end;
  end;
  function MultiOperationSignOnline(Const HexaStringOperationsHashTree : String) : boolean;
  var errors : String;
    senderOperationsHashTree : TOperationsHashTree;
    j,iKey,nSignedAccounts : Integer;
    mop : TOpMultiOperation;
    lSigners : TList<Cardinal>;
    nAccount : Integer;
    pubKey : TAccountKey;
  begin
    Result := false;
    if (Not (_RPCServer.FWalletKeys.IsValidPassword)) then begin
      // Wallet is password protected
      ErrorDesc := 'Wallet is password protected';
      ErrorNum := CT_RPC_ErrNum_WalletPasswordProtected;
      Exit;
    end;
    if Not HexaStringToOperationsHashTreeAndGetMultioperation(HexaStringOperationsHashTree,False,senderOperationsHashTree,mop,errors) then begin
      ErrorNum:=CT_RPC_ErrNum_InvalidData;
      ErrorDesc:= 'Error decoding param previous operations hash tree raw value: '+errors;
      Exit;
    end;
    Try
      nSignedAccounts := 0;
      lSigners := TList<Cardinal>.Create;
      Try
        mop.SignerAccounts(lSigners);
        for j:=0 to lSigners.Count-1 do begin
          nAccount := PtrInt(lSigners[j]);
          if (nAccount>=0) And (nAccount<FNode.Bank.AccountsCount) then begin
            // Try to
            pubKey := FNode.GetMempoolAccount(nAccount).accountInfo.accountKey;
            // Is mine?
            iKey := _RPCServer.FWalletKeys.IndexOfAccountKey(pubKey);
            if (iKey>=0) then begin
              if (assigned(_RPCServer.FWalletKeys.Key[iKey].PrivateKey)) then begin
                // Can sign
                inc(nSignedAccounts, mop.DoSignMultiOperationSigner(FNode.Bank.Vault.CurrentProtocol,nAccount,_RPCServer.FWalletKeys.Key[iKey].PrivateKey) );
              end;
            end;
          end;
        end;
      finally
        lSigners.Free;
      end;
      // Return multioperation object:
      TABEYJSONComp.FillMultiOperationObject(FNode.Bank.Vault.CurrentProtocol,mop,GetResultObject);
      Result := True;
    finally
      senderOperationsHashTree.Free;
    end;
  end;

  function RawOperations_Delete(Const HexaStringOperationsHashTree : String; index : Integer) : boolean;
  var senderOperationsHashTree : TOperationsHashTree;
    errors : String;
  begin
    Result := False;
    if Not HexaStringToOperationsHashTree(HexaStringOperationsHashTree,senderOperationsHashTree,errors) then begin
      ErrorNum:=CT_RPC_ErrNum_InvalidData;
      ErrorDesc:= 'Error decoding param previous operations hash tree raw value: '+errors;
      Exit;
    end;
    Try
      // Obtain mop from last OperationsHashTree operation, otherwise create a new one
      if (index>=0) And (index<senderOperationsHashTree.OperationsCount) then begin
        senderOperationsHashTree.Delete(index);
      end else begin
        ErrorNum := CT_RPC_ErrNum_InvalidData;
        ErrorDesc:='Cannot delete index '+IntToStr(index)+' from Raw operations length '+IntToStr(senderOperationsHashTree.OperationsCount);
        Exit;
      end;
      // Return objects:
      TABEYJSONComp.FillOperationsHashTreeObject(senderOperationsHashTree,GetResultObject);
      Result := True;
    finally
      senderOperationsHashTree.Free;
    end;
  end;

Var c,c2,c3 : Cardinal;
  starting_block_number_flag : Boolean;
  starting_block_number, active_blocks, passive_blocks, max_instruction_count, max_memory_usage_kb, max_runtime_seconds : Cardinal;
  max_io_usage, max_data_segment_size_kb, export_table_size, bytecode_size : Cardinal;
  i,j,k,l : Integer;
  account : TAccount;
  senderpubkey,destpubkey : TAccountKey;
  ansistr, functionType, functionValueForType, functionName : String;
  nsaarr : TNodeServerAddressArray;
  pcops : TABEYOperationsComp;
  ecpkey : TECPrivateKey;
  opr : TOperationResume;
  r1,r2, exportTable: TRawBytes;
  ocl : TOrderedCardinalList;
  jsonarr, jsonarrtmp : TABEYJSONArray;
  jso : TABEYJSONObject;
  pathToExportTable, pathToBytecode : String;
  typesOfFunctionParameters : array of string;
  valuesOfFunctionParameters :  array of string;

begin
  _ro := Nil;
  _ra := Nil;
  ErrorNum:=0;
  ErrorDesc:='';
  pathToExportTable := '';
  pathToBytecode := '';
  Result := false;
  {$IFDEF HIGHLOG}TLog.NewLog(ltdebug,ClassName,'Processing RPC-JSON method '+method);{$ENDIF}

  if (method='addnode') then begin
    // Param "nodes" contains ip's and ports in format "ip1:port1;ip2:port2 ...". If port is not specified, use default
    // Returns quantity of nodes added
    if (Not _RPCServer.AllowUsePrivateKeys) then begin
      // Protection when server is locked to avoid external calls
      ErrorNum := CT_RPC_ErrNum_NotAllowedCall;
      Exit;
    end;
    TNode.DecodeIpStringToNodeServerAddressArray(params.AsString('nodes',''),nsaarr);
    for i:=low(nsaarr) to high(nsaarr) do begin
      TNetData.NetData.AddServer(nsaarr[i]);
    end;
    jsonresponse.GetAsVariant('result').Value:=length(nsaarr);
    Result := true;

  end else if (method='executecontract') then begin

    if params.IndexOfName('callerAccount') < 0 then begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      ErrorDesc := 'Need caller account number param';
    end;
    if params.IndexOfName('contractID') < 0 then begin
      ErrorNum := CT_RPC_ErrNum_InvalidContractID;
      ErrorDesc := 'Need "contractID" param';
      exit;
    end;

    if params.IndexOfName('functionName') < 0 then begin
      ErrorNum := CT_RPC_ErrNum_InvalidFunctionName;
      ErrorDesc := 'Need "functionName" param';
      exit;
    end;

    jsonarr := params.GetAsArray('parameters');
    SetLength(typesOfFunctionParameters, jsonArr.Count);
    SetLength(valuesOfFunctionParameters, jsonArr.Count);
    for i:=0 to jsonArr.Count-1 do begin
        functionType := jsonArr.GetAsObject(i).AsString('type','');
        if functionType.Equals('char') then begin
          DebugLn('Execute Contract with "char" paramter type');
          typesOfFunctionParameters[i] := 'char';
          functionValueForType := jsonArr.GetAsObject(i).AsString('value','');
          if functionValueForType.Equals('') then begin
            ErrorNum := CT_RPC_ErrNum_InvalidParameterValueForType;
            ErrorDesc := 'Missing value for "char" type paramter';
          end else begin
            valuesOfFunctionParameters[i] := functionValueForType;
            DebugLn('Execute Contract with "char" paramter type and value = ', functionValueForType);
          end;
        end else if functionType.equals('int') then begin
          DebugLn('Execute Contract with "int" paramter type');
          typesOfFunctionParameters[i] := 'int';
          functionValueForType := jsonArr.GetAsObject(i).AsString('value','');
          if functionValueForType.Equals('') then begin
            ErrorNum := CT_RPC_ErrNum_InvalidParameterValueForType;
            ErrorDesc := 'Missing value for "int" type paramter';
          end else begin
            valuesOfFunctionParameters[i] := functionValueForType;
            DebugLn('Execute Contract with "int" paramter type and value = ', functionValueForType);
          end;
        end else if functionType.equals('float') then begin
          DebugLn('Execute Contract with "float" paramter type');
          typesOfFunctionParameters[i] := 'float';
          functionValueForType := jsonArr.GetAsObject(i).AsString('value','');
          if functionValueForType.Equals('') then begin
            ErrorNum := CT_RPC_ErrNum_InvalidParameterValueForType;
            ErrorDesc := 'Missing value for "float" type paramter';
          end else begin
            valuesOfFunctionParameters[i] := functionValueForType;
            DebugLn('Execute Contract with "float" paramter type and value = ', functionValueForType);
          end;
        end else if functionType.equals('double') then begin
          DebugLn('Execute Contract with "double" paramter type');
          typesOfFunctionParameters[i] := 'double';
          functionValueForType := jsonArr.GetAsObject(i).AsString('value','');
          if functionValueForType.Equals('') then begin
            ErrorNum := CT_RPC_ErrNum_InvalidParameterValueForType;
            ErrorDesc := 'Missing value for "double" type paramter';
          end else begin
            valuesOfFunctionParameters[i] := functionValueForType;
            DebugLn('Execute Contract with "double" paramter type and value = ', functionValueForType);
          end;
        //end else if functionType.equals('char1024') then begin
        //  DebugLn('execute Contract with "char1024" parameter type');
        //  typesOfFunctionParameters[i] := 'char1024';
        //  must add arrays
        //  jsonarrtmp := params.GetAsArray('char1024');
        //  for i:=0 to jsonarrtmp.Count-1 do begin
        //
        //  end;
        end else begin
          ErrorNum := CT_RPC_ErrNum_InvalidParameterTypeForContract;
          ErrorDesc := 'Invalid Parameter Type for execute contract';
        end;

      end;

      c := params.AsCardinal('callerAccount', CT_MaxAccount);
      c2 := params.AsCardinal('contractID',-1);
      functionName := params.AsString('functionName','');
      DebugLn('callerAccount = %d', [c]);
      DebugLn('contractID = %d', [c2]);
      DebugLn('functionName = %s', [functionName]);
      for functionType in typesOfFunctionParameters do
          DebugLn('type = %s', [functionType]);
      for functionValueForType in valuesOfFunctionParameters do
          DebugLn('value = %s', [functionValueForType]);

      Result := ExecuteContract(c, c2, ToABEYs(params.AsDouble('fee',0)), functionName, typesOfFunctionParameters, valuesOfFunctionParameters, params.AsString('payload_method','dest'),params.AsString('pwd',''));
  end else if (method='savecontract') then begin

    DebugLn('RPC method savecontract started');

    if (Not _RPCServer.AllowUsePrivateKeys) then begin
      // Protection when server is locked to avoid private keys call
      ErrorNum := CT_RPC_ErrNum_NotAllowedCall;
      Exit;
    end;
    If Not _RPCServer.WalletKeys.IsValidPassword then begin
      ErrorNum := CT_RPC_ErrNum_WalletPasswordProtected;
      ErrorDesc := 'Wallet is password protected. Unlock first';
      exit;
    end;
    if params.IndexOfName('account_signer')<0 then begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      ErrorDesc := 'Need "account_signer" param';
      exit;
    end;

    if params.IndexOfName('account_target')<0 then begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      ErrorDesc := 'Need "account_target" param';
      exit;
    end;

    if params.IndexOfName('starting_block_number_flag')<0 then begin
      ErrorNum :=  CT_RPC_ErrNum_FlagForStartingBlock;
      ErrorDesc := 'Need "starting_block_number_flag" param';
    end;

    if params.IndexOfName('starting_block_number')<0 then begin
      ErrorNum :=  CT_RPC_ErrNum_ActiveBlocks;
      ErrorDesc := 'Need "starting_block_number" param';
    end;

    if params.IndexOfName('active_blocks')<0 then begin
      ErrorNum :=  CT_RPC_ErrNum_ActiveBlocks;
      ErrorDesc := 'Need "active_blocks" param';
    end;

    if params.IndexOfName('passive_blocks')<0 then begin
      ErrorNum :=  CT_RPC_ErrNum_PassiveBlocks;
      ErrorDesc := 'Need "passive_blocks" param';
    end;

    if params.IndexOfName('max_instruction_count')<0 then begin
      ErrorNum :=  CT_RPC_ErrNum_MaxInstructionCount;
      ErrorDesc := 'Need "max_instruction_count" param';
    end;

    if params.IndexOfName('max_memory_usage_kb')<0 then begin
      ErrorNum :=  CT_RPC_ErrNum_MaxMemoryUsage;
      ErrorDesc := 'Need "max_memory_usage_kb" param';
    end;

    if params.IndexOfName('max_runtime_seconds')<0 then begin
      ErrorNum :=  CT_RPC_ErrNum_MaxRuntimeSeconds;
      ErrorDesc := 'Need "max_runtime_seconds" param';
    end;

    if params.IndexOfName('max_io_usage')<0 then begin
      ErrorNum :=  CT_RPC_ErrNum_MaxIoUsage;
      ErrorDesc := 'Need "max_io_usage" param';
    end;

    if params.IndexOfName('max_data_segment_size_kb')<0 then begin
      ErrorNum :=  CT_RPC_ErrNum_MaxDataSegmentSize;
      ErrorDesc := 'Need "max_data_segment_size_kb" param';
    end;

    if params.IndexOfName('export_table_size')<0 then begin
      ErrorNum :=  CT_RPC_ErrNum_ExportTableSize;
      ErrorDesc := 'Need "export_table_size" param';
    end;

    if params.IndexOfName('bytecode_size')<0 then begin
      ErrorNum :=  CT_RPC_ErrNum_BytecodeSize;
      ErrorDesc := 'Need "bytecode_size" param';
    end;

    if params.IndexOfName('export_table')<0 then begin
      ErrorNum := CT_RPC_ErrNum_ExportTableFilePath;
      ErrorDesc := 'Need "export_table" file path param';
    end;

    if params.IndexOfName('bytecode')<0 then begin
      ErrorNum := CT_RPC_ErrNum_PayloadFilePath;
      ErrorDesc := 'Need "byte" file path param';
    end;

    c := params.AsCardinal('account_target',CT_MaxAccount);
    c2 := params.AsCardinal('account_signer',CT_MaxAccount);
    starting_block_number_flag := params.AsBoolean('starting_block_number_flag', false);
    starting_block_number := params.AsCardinal('starting_block_number', CT_MaxCardinalValue);
    active_blocks := params.AsCardinal('active_blocks', 0);
    passive_blocks := params.AsCardinal('passive_blocks', 0);
    max_instruction_count := params.AsCardinal('max_instruction_count', CT_MaxCardinalValue);
    max_memory_usage_kb := params.AsCardinal('max_memory_usage_kb', CT_MaxCardinalValue);
    max_runtime_seconds := params.AsCardinal('max_runtime_seconds', CT_MaxCardinalValue);
    max_io_usage := params.AsCardinal('max_io_usage', CT_MaxCardinalValue);
    max_data_segment_size_kb := params.AsCardinal('max_data_segment_size_kb', CT_MaxCardinalValue);
    export_table_size := params.AsCardinal('export_table_size', CT_MaxCardinalValue);
    bytecode_size := params.AsCardinal('bytecode_size', CT_MaxCardinalValue);
    pathToExportTable := params.AsString('export_table','');
    pathToBytecode := params.AsString('payload','');

    //check file paths here as we can prevent the error here
    if (Not(FileExists(pathToExportTable))) then begin
       ErrorNum := CT_RPC_ErrNum_ExportTableFilePath;
       ErrorDesc := 'Invalid path to export table file';
    end;

    if (Not(FileExists(pathToBytecode))) then begin
      ErrorNum := CT_RPC_ErrNum_PayloadFilePath;
      ErrorDesc := 'Invalid path to bytecode file';
    end;


    //DebugLn('Payload is HEX string:',BoolToStr(TCrypto.IsHexString(params.AsString('payload',''))));
    //DebugLn(Format('Process method Active blocks = %d',[active_blocks]));
    //DebugLn(Format('URPC.pas starting_block_number = %d', [starting_block_number]));

    Result := SaveContract(c2,c,ToABEYs(params.AsDouble('fee',0)),starting_block_number_flag,starting_block_number,active_blocks, passive_blocks, max_instruction_count, max_memory_usage_kb, max_runtime_seconds,
                           max_io_usage, max_data_segment_size_kb,export_table_size, bytecode_size, pathToExportTable, pathToBytecode,params.AsString('payload_method','dest'),params.AsString('pwd',''));

    DebugLn('RPC method savecontract done');

  end else if (method='saveContract') then begin

    Result := SaveContractRefactored(params);

  end else if (method='sendData') then begin
    // dummy operation for blockchain RollBack
    if (Not _RPCServer.AllowUsePrivateKeys) then begin
      // Protection when server is locked to avoid private keys call
      ErrorNum := CT_RPC_ErrNum_NotAllowedCall;
      Exit;
    end;
    If Not _RPCServer.WalletKeys.IsValidPassword then begin
      ErrorNum := CT_RPC_ErrNum_WalletPasswordProtected;
      ErrorDesc := 'Wallet is password protected. Unlock first';
      exit;
    end;
    Result := SendData(params.AsCardinal('sender',CT_MaxAccount),params.AsCardinal('target',CT_MaxAccount),
       ToABEYs(params.AsDouble('amount',0)),
       ToABEYs(params.AsDouble('fee',0)),
       TCrypto.HexaToRaw(params.AsString('payload','')),
       params.AsString('payload_method','dest'),params.AsString('pwd',''));
  end else if (method='executeContract') then begin

    Result := ExecuteContractRefactored(params);

  end else if (method='modifySnapshot') then begin

    Result := ModifySnapshot(params.AsString('snapshotPath',''),errors,ErrorDesc,ErrorNum);


  end else if (method='saveLog') then begin

    Result := SaveLog(params);

  end else if (method='appendLog') then begin

    Result := AppendLog(params);

  end else if (method='retrieveLogs') then begin

    Result :=  RetrieveLogs(params,jsonarr);

  end else if (method='retrieveLog') then begin

    Result :=  RetrieveLog(params,jso);

  end else if (method='retrieveContract') then begin

    Result := RetrieveContract(params,jso);

  end else if (method='getaccount') then begin
    // Param "account" contains account number
    // Returns JSON Object with account information based on BlockChain + Pending operations
    c := params.GetAsVariant('account').AsCardinal(CT_MaxAccount);
    if (c>=0) And (c<FNode.Bank.AccountsCount) then begin
      account := FNode.GetMempoolAccount(c);
      TABEYJSONComp.FillAccountObject(account,GetResultObject);
      Result := True;
    end else begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      if (c=CT_MaxAccount) then ErrorDesc := 'Need "account" param'
      else ErrorDesc := 'Account not found: '+IntToStr(c);
    end;
  end else if (method='findaccounts') then begin
    Result := FindAccounts(params, jsonarr);
  end else if (method='getwalletaccounts') then begin
    if (Not _RPCServer.AllowUsePrivateKeys) then begin
      // Protection when server is locked to avoid private keys call
      ErrorNum := CT_RPC_ErrNum_NotAllowedCall;
      Exit;
    end;

    // Returns JSON array with accounts in Wallet
    jsonarr := jsonresponse.GetAsArray('result');
    if (params.IndexOfName('enc_pubkey')>=0) Or (params.IndexOfName('b58_pubkey')>=0) then begin
      if Not (CapturePubKey('',opr.newKey,ErrorDesc)) then begin
        ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
        exit;
      end;
      i := _RPCServer.WalletKeys.AccountsKeyList.IndexOfAccountKey(opr.newKey);
      if (i<0) then begin
        ErrorNum := CT_RPC_ErrNum_NotFound;
        ErrorDesc := 'Public key not found in wallet';
        exit;
      end;
      ocl := _RPCServer.WalletKeys.AccountsKeyList.AccountKeyList[i];
      k := params.AsInteger('max',100);
      l := params.AsInteger('start',0);
      for j := 0 to ocl.Count - 1 do begin
        if (j>=l) then begin
          account := FNode.GetMempoolAccount(ocl.Get(j));
          TABEYJSONComp.FillAccountObject(account,jsonarr.GetAsObject(jsonarr.Count));
        end;
        if (k>0) And ((j+1)>=(k+l)) then break;
      end;
      Result := true;
    end else begin
      k := params.AsInteger('max',100);
      l := params.AsInteger('start',0);
      c := 0;
      for i:=0 to _RPCServer.WalletKeys.AccountsKeyList.Count-1 do begin
        ocl := _RPCServer.WalletKeys.AccountsKeyList.AccountKeyList[i];
        for j := 0 to ocl.Count - 1 do begin
          if (c>=l) then begin
            account := FNode.GetMempoolAccount(ocl.Get(j));
            TABEYJSONComp.FillAccountObject(account,jsonarr.GetAsObject(jsonarr.Count));
          end;
          inc(c);
          if (k>0) And (c>=(k+l)) then break;
        end;
        if (k>0) And (c>=(k+l)) then break;
      end;
      Result := true;
    end;
  end else if (method='getwalletaccountscount') then begin
    if (Not _RPCServer.AllowUsePrivateKeys) then begin
      // Protection when server is locked to avoid private keys call
      ErrorNum := CT_RPC_ErrNum_NotAllowedCall;
      Exit;
    end;
    // New Build 1.1.1
    // Returns a number with count value
    if (params.IndexOfName('enc_pubkey')>=0) Or (params.IndexOfName('b58_pubkey')>=0) then begin
      if Not (CapturePubKey('',opr.newKey,ErrorDesc)) then begin
        ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
        exit;
      end;
      i := _RPCServer.WalletKeys.AccountsKeyList.IndexOfAccountKey(opr.newKey);
      if (i<0) then begin
        ErrorNum := CT_RPC_ErrNum_NotFound;
        ErrorDesc := 'Public key not found in wallet';
        exit;
      end;
      ocl := _RPCServer.WalletKeys.AccountsKeyList.AccountKeyList[i];
      jsonresponse.GetAsVariant('result').value := ocl.count;
      Result := true;
    end else begin
      ErrorDesc := '';
      c :=0;
      for i:=0 to _RPCServer.WalletKeys.AccountsKeyList.Count-1 do begin
        ocl := _RPCServer.WalletKeys.AccountsKeyList.AccountKeyList[i];
        inc(c,ocl.count);
      end;
      jsonresponse.GetAsVariant('result').value := c;
      Result := true;
    end;
  end else if (method='getwalletcoins') then begin
    if (Not _RPCServer.AllowUsePrivateKeys) then begin
      // Protection when server is locked to avoid private keys call
      ErrorNum := CT_RPC_ErrNum_NotAllowedCall;
      Exit;
    end;
    if (params.IndexOfName('enc_pubkey')>=0) Or (params.IndexOfName('b58_pubkey')>=0) then begin
      if Not (CapturePubKey('',opr.newKey,ErrorDesc)) then begin
        ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
        exit;
      end;
      i := _RPCServer.WalletKeys.AccountsKeyList.IndexOfAccountKey(opr.newKey);
      if (i<0) then begin
        ErrorNum := CT_RPC_ErrNum_NotFound;
        ErrorDesc := 'Public key not found in wallet';
        exit;
      end;
      ocl := _RPCServer.WalletKeys.AccountsKeyList.AccountKeyList[i];
      account.balance := 0;
      for j := 0 to ocl.Count - 1 do begin
        inc(account.balance, FNode.GetMempoolAccount(ocl.Get(j)).balance );
      end;
      jsonresponse.GetAsVariant('result').value := ToJSONCurrency(account.balance);
      Result := true;
    end else begin
      ErrorDesc := '';
      c :=0;
      account.balance := 0;
      for i:=0 to _RPCServer.WalletKeys.AccountsKeyList.Count-1 do begin
        ocl := _RPCServer.WalletKeys.AccountsKeyList.AccountKeyList[i];
        for j := 0 to ocl.Count - 1 do begin
          inc(account.balance, FNode.GetMempoolAccount(ocl.Get(j)).balance );
        end;
      end;
      jsonresponse.GetAsVariant('result').value := ToJSONCurrency(account.balance);
      Result := true;
    end;
  end else if (method='getwalletpubkeys') then begin
    if (Not _RPCServer.AllowUsePrivateKeys) then begin
      // Protection when server is locked to avoid private keys call
      ErrorNum := CT_RPC_ErrNum_NotAllowedCall;
      Exit;
    end;
    // Returns JSON array with pubkeys in wallet
    k := params.AsInteger('max',100);
    j := params.AsInteger('start',0);
    jsonarr := jsonresponse.GetAsArray('result');
    for i:=0 to _RPCServer.WalletKeys.Count-1 do begin
      if (i>=j) then begin
        jso := jsonarr.GetAsObject(jsonarr.count);
        jso.GetAsVariant('name').Value := _RPCServer.WalletKeys.Key[i].Name;
        jso.GetAsVariant('can_use').Value := Length(_RPCServer.WalletKeys.Key[i].CryptedKey)>0;
        TABEYJSONComp.FillPublicKeyObject(_RPCServer.WalletKeys.Key[i].AccountKey,jso);
      end;
      if (k>0) And ((i+1)>=(j+k)) then break;
    end;
    Result := true;
  end else if (method='getwalletpubkey') then begin
    if (Not _RPCServer.AllowUsePrivateKeys) then begin
      // Protection when server is locked to avoid private keys call
      ErrorNum := CT_RPC_ErrNum_NotAllowedCall;
      Exit;
    end;
    if Not (CapturePubKey('',opr.newKey,ErrorDesc)) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
      exit;
    end;
    i := _RPCServer.WalletKeys.AccountsKeyList.IndexOfAccountKey(opr.newKey);
    if (i<0) then begin
      ErrorNum := CT_RPC_ErrNum_NotFound;
      ErrorDesc := 'Public key not found in wallet';
      exit;
    end;
    TABEYJSONComp.FillPublicKeyObject(_RPCServer.WalletKeys.AccountsKeyList.AccountKey[i],GetResultObject);
    Result := true;
  end else if (method='importpubkey') then begin
       ansistr:= params.AsString('name','');

       if ((params.IndexOfName('b58_pubkey')>=0) AND (ansistr<>'')) then begin
         if Not (CapturePubKey('',destpubkey,ErrorDesc)) then begin
            ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
            exit;
         end;
         _RPCServer.WalletKeys.AddPublicKey(ansistr,destpubkey);
       end else begin
         ErrorNum := CT_RPC_ErrNum_InvalidData;
         ErrorDesc := 'Needed both parameters b58_pubkey and name';
         exit;
    end;
    TABEYJSONComp.FillPublicKeyObject(destpubkey,GetResultObject);
    Result := true;
  end else if (method='getblock') then begin
    // Param "block" contains block number (0..getblockcount-1)
    // Returns JSON object with block information
    c := params.GetAsVariant('block').AsCardinal(CT_MaxBlock);
    if (c>=0) And (c<FNode.Bank.BlocksCount) then begin
      Result := GetBlock(c,GetResultObject);
    end else begin
      ErrorNum := CT_RPC_ErrNum_InvalidBlock;
      if (c=CT_MaxBlock) then ErrorDesc := 'Need block param'
      else ErrorDesc := 'Block not found: '+IntToStr(c);
    end;
  end else if (method='getblocks') then begin
    // Param "start" "end" contains blocks number (0..getblockcount-1)
    // Returns JSON Array with blocks information (limited to 1000 blocks)
    // Sorted by DESCENDING blocknumber
    i := params.AsCardinal('last',0);
    if (i>0) then begin
      if (i>1000) then i := 1000;
      c2 := FNode.Bank.BlocksCount-1;
      if (FNode.Bank.BlocksCount>=i) then
        c := (FNode.Bank.BlocksCount) - i
      else c := 0;
    end else begin
      c := params.GetAsVariant('start').AsCardinal(CT_MaxBlock);
      c2 := params.GetAsVariant('end').AsCardinal(CT_MaxBlock);
      i := params.AsInteger('max',0);
      if (c<FNode.Bank.BlocksCount) And (i>0) And (i<=1000) then begin
        if (c+i<FNode.Bank.BlocksCount) then c2 := c+i
        else c2 := FNode.Bank.BlocksCount-1;
      end;
    end;
    if ((c>=0) And (c<FNode.Bank.BlocksCount)) And (c2>=c) And (c2<FNode.Bank.BlocksCount) then begin
      i := 0; Result := true;
      while (c<=c2) And (Result) And (i<1000) do begin
        Result := GetBlock(c2,jsonresponse.GetAsArray('result').GetAsObject(i));
        dec(c2); inc(i);
      end;
    end else begin
      ErrorNum := CT_RPC_ErrNum_InvalidBlock;
      if (c>c2) then ErrorDesc := 'Block start > block end'
      else if (c=CT_MaxBlock) Or (c2=CT_MaxBlock) then ErrorDesc:='Need param "last" or "start" and "end"/"max"'
      else if (c2>=FNode.Bank.BlocksCount) then ErrorDesc := 'Block higher or equal to getblockccount: '+IntToStr(c2)
      else  ErrorDesc := 'Block not found: '+IntToStr(c);
    end;
  end else if (method='getblockcount') then begin
    // Returns a number with Node blocks count
    jsonresponse.GetAsVariant('result').Value:=FNode.Bank.BlocksCount;
    Result := True;
  end else if (method='getblockoperation') then begin
    // Param "block" contains block. Null = Pending operation
    // Param "opblock" contains operation inside a block: (0..getblock.operations-1)
    // Returns a JSON object with operation values as "Operation resume format"
    c := params.GetAsVariant('block').AsCardinal(CT_MaxBlock);
    if (c>=0) And (c<FNode.Bank.BlocksCount) then begin
      pcops := TABEYOperationsComp.Create(Nil);
      try
        If Not FNode.Bank.LoadOperations(pcops,c) then begin
          ErrorNum := CT_RPC_ErrNum_InternalError;
          ErrorDesc := 'Cannot load Block: '+IntToStr(c);
          Exit;
        end;
        i := params.GetAsVariant('opblock').AsInteger(0);
        if (i<0) Or (i>=pcops.Count) then begin
          ErrorNum := CT_RPC_ErrNum_InvalidOperation;
          ErrorDesc := 'Block/Operation not found: '+IntToStr(c)+'/'+IntToStr(i)+' BlockOperations:'+IntToStr(pcops.Count);
          Exit;
        end;
        If TABEYOperation.OperationToOperationResume(c,pcops.Operation[i],True,pcops.Operation[i].SignerAccount,opr) then begin
          opr.NOpInsideBlock:=i;
          opr.time:=pcops.OperationBlock.timestamp;
          opr.Balance := -1;
          FillOperationResumeToJSONObject(opr,GetResultObject);
        end;
        Result := True;
      finally
        pcops.Free;
      end;
    end else begin
      If (c=CT_MaxBlock) then ErrorDesc := 'Need block param'
      else ErrorDesc := 'Block not found: '+IntToStr(c);
      ErrorNum := CT_RPC_ErrNum_InvalidBlock;
    end;
  end else if (method='getblockoperations') then begin
    // Param "block" contains block
    // Returns a JSON array with items as "Operation resume format"
    c := params.GetAsVariant('block').AsCardinal(CT_MaxBlock);
    if (c>=0) And (c<FNode.Bank.BlocksCount) then begin
      pcops := TABEYOperationsComp.Create(Nil);
      try
        If Not FNode.Bank.LoadOperations(pcops,c) then begin
          ErrorNum := CT_RPC_ErrNum_InternalError;
          ErrorDesc := 'Cannot load Block: '+IntToStr(c);
          Exit;
        end;
        jsonarr := GetResultArray;
        k := params.AsInteger('max',100);
        j := params.AsInteger('start',0);
        for i := 0 to pcops.Count - 1 do begin
          if (i>=j) then begin
            If TABEYOperation.OperationToOperationResume(c,pcops.Operation[i],True,pcops.Operation[i].SignerAccount,opr) then begin
              opr.NOpInsideBlock:=i;
              opr.time:=pcops.OperationBlock.timestamp;
              opr.Balance := -1; // Don't include!
              FillOperationResumeToJSONObject(opr,jsonarr.GetAsObject(jsonarr.Count));
            end;
          end;
          if (k>0) And ((i+1)>=(j+k)) then break;
        end;
        Result := True;
      finally
        pcops.Free;
      end;
    end else begin
      If (c=CT_MaxBlock) then ErrorDesc := 'Need block param'
      else ErrorDesc := 'Block not found: '+IntToStr(c);
      ErrorNum := CT_RPC_ErrNum_InvalidBlock;
    end;
  end else if (method='getaccountoperations') then begin
    // Returns all the operations affecting an account in "Operation resume format" as an array
    // Param "account" contains account number
    // Param "depht" (optional or "deep") contains max blocks deep to search (Default: 100)
    // Param "start" and "max" contains starting index and max operations respectively
    // Param "startblock" forces to start searching backwards on a fixed block, will not give balance for each operation due it's unknown
    c := params.GetAsVariant('account').AsCardinal(CT_MaxAccount);
    if ((c>=0) And (c<FNode.Bank.AccountsCount)) then begin
      if (params.IndexOfName('depth')>=0) then i := params.AsInteger('depth',100) else i:=params.AsInteger('deep',100);
      If params.IndexOfName('startblock')>=0 then c2 := params.AsCardinal('startblock',0)
      else c2 := 0;
      Result := GetAccountOperations(c,GetResultArray,i,params.AsInteger('start',0),params.AsInteger('max',100),c2);
    end else begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      If (c=CT_MaxAccount) then ErrorDesc := 'Need account param'
      else ErrorDesc := 'Account not found: '+IntToStr(c);
    end;
  end else if (method='getpendings') then begin
    // Returns all the operations pending to be included in a block in "Operation resume format" as an array
    // Create result
    k := params.AsInteger('max',100);
    j := params.AsInteger('start',0);
    If FNode.TryLockNode(5000) then begin
      Try
        jsonarr := GetResultArray;
        pcops := FNode.LockMempoolRead;
        try
          for i := j to pcops.Count-1 do begin
            If TABEYOperation.OperationToOperationResume(0,pcops.Operation[i],True,pcops.Operation[i].SignerAccount,opr) then begin
              opr.NOpInsideBlock:=i;
              opr.Balance := -1; // Don't include!
              FillOperationResumeToJSONObject(opr,jsonarr.GetAsObject(jsonarr.Count));
            end;
            if (k>0) And (jsonarr.Count>=k) then break;
          end;
        finally
          FNode.UnlockMempoolRead;
        end;
      finally
        FNode.UnlockNode;
      end;
      Result := true;
    end else begin
      ErrorNum := CT_RPC_ErrNum_InternalError;
      ErrorDesc := 'Node is busy';
    end;
  end else if (method='getpendingscount') then begin
    jsonresponse.GetAsVariant('result').Value := FNode.MempoolOperationsCount;
    Result := true;
  end else if (method='decodeophash') then begin
    // Search for an operation based on "ophash"
    r1 := TCrypto.HexaToRaw(params.AsString('ophash',''));
    if (Length(r1)=0) then begin
      ErrorNum:=CT_RPC_ErrNum_NotFound;
      ErrorDesc:='param ophash not found or invalid hexadecimal value "'+params.AsString('ophash','')+'"';
      exit;
    end;
    If not TABEYOperation.DecodeOperationHash(r1,c,c2,c3,r2) then begin
      ErrorNum:=CT_RPC_ErrNum_NotFound;
      ErrorDesc:='invalid ophash param value';
      exit;
    end;
    GetResultObject.GetAsVariant('block').Value:=c;
    GetResultObject.GetAsVariant('account').Value:=c2;
    GetResultObject.GetAsVariant('n_operation').Value:=c3;
    GetResultObject.GetAsVariant('md160hash').Value:=TCrypto.ToHexaString(r2);
    Result := true;
  end else if (method='findoperation') then begin
    // Search for an operation based on "ophash"
    r1 := TCrypto.HexaToRaw(params.AsString('ophash',''));
    if (Length(r1)=0) then begin
      ErrorNum:=CT_RPC_ErrNum_NotFound;
      ErrorDesc:='param ophash not found or invalid hexadecimal value "'+params.AsString('ophash','')+'"';
      exit;
    end;
    pcops := TABEYOperationsComp.Create(Nil);
    try
      Case FNode.FindOperationExt(pcops,r1,c,i) of
        found : ;
        invalid_params : begin
            ErrorNum:=CT_RPC_ErrNum_NotFound;
            ErrorDesc:='ophash not found: "'+params.AsString('ophash','')+'"';
            exit;
          end;
        blockchain_block_not_found : begin
            ErrorNum := CT_RPC_ErrNum_InternalError;
            ErrorDesc:='Blockchain block '+IntToStr(c)+' not found to search ophash: "'+params.AsString('ophash','')+'"';
            exit;
          end;
      else Raise Exception.Create('ERROR DEV 20171120-4');
      end;
      If not TABEYOperation.OperationToOperationResume(c,pcops.Operation[i],True,pcops.Operation[i].SignerAccount,opr) then begin
        ErrorNum := CT_RPC_ErrNum_InternalError;
        ErrorDesc := 'Error 20161026-1';
      end;
      opr.NOpInsideBlock:=i;
      opr.time:=pcops.OperationBlock.timestamp;
      opr.Balance := -1; // don't include
      FillOperationResumeToJSONObject(opr,GetResultObject);
      Result := True;
    finally
      pcops.Free;
    end;
  end else if (method='findnoperation') then begin
    // Search for an operation signed by "account" and with "n_operation", start searching "block" (0=all)
    // "block" = 0 search in all blocks, pending operations included
    Case FNode.FindNOperation(params.AsCardinal('block',0),params.AsCardinal('account',MaxInt),params.AsCardinal('n_operation',0),opr) of
      found : ;
      invalid_params : begin
          ErrorNum:=CT_RPC_ErrNum_NotFound;
          ErrorDesc:='Not found using block/account/n_operation';
          exit;
        end;
      blockchain_block_not_found : begin
          ErrorNum := CT_RPC_ErrNum_InvalidBlock;
          ErrorDesc:='Blockchain file does not contain all blocks to find';
          exit;
        end;
    else Raise Exception.Create('ERROR DEV 20171120-5');
    end;
    FillOperationResumeToJSONObject(opr,GetResultObject);
    Result := True;
  end else if (method='findnoperations') then begin
    // Search for all operations signed by "account" and n_operation value between "n_operation_min" and "n_operation_max", start searching at "block" (0=all)
    // "block" = 0 search in all blocks, pending operations included
    Result := findNOperations;
  end else if (method='sendto') then begin
    // Sends "amount" coins from "sender" to "target" with "fee"
    // If "payload" is present, it will be encoded using "payload_method"
    // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
    // Returns a JSON "Operation Resume format" object when successfull
    // Note: "ophash" will contain block "0" = "pending block"
    if (Not _RPCServer.AllowUsePrivateKeys) then begin
      // Protection when server is locked to avoid private keys call
      ErrorNum := CT_RPC_ErrNum_NotAllowedCall;
      Exit;
    end;
    If Not _RPCServer.WalletKeys.IsValidPassword then begin
      ErrorNum := CT_RPC_ErrNum_WalletPasswordProtected;
      ErrorDesc := 'Wallet is password protected. Unlock first';
      exit;
    end;
    Result := OpSendTo(params.AsCardinal('sender',CT_MaxAccount),params.AsCardinal('target',CT_MaxAccount),
       ToABEYs(params.AsDouble('amount',0)),
       ToABEYs(params.AsDouble('fee',0)),
       TCrypto.HexaToRaw(params.AsString('payload','')),
       params.AsString('payload_method','dest'),params.AsString('pwd',''));
  end else if (method='signsendto') then begin
    // Create a Transaction operation and adds it into a "rawoperations" (that can include
    // previous operations). This RPC method is usefull ffor cold storage, because doesn't
    // need to check or verify accounts status/public key, assuming that passed values
    // are ok.
    // Signs a transaction of "amount" coins from "sender" to "target" with "fee", using "sender_enc_pubkey" or "sender_b58_pubkey"
    // and "last_n_operation" of sender. Also, needs "target_enc_pubkey" or "target_b58_pubkey"
    // If "payload" is present, it will be encoded using "payload_method"
    // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
    // Returns a JSON "Operations info" containing old "rawoperations" plus new Transaction
    if (Not _RPCServer.AllowUsePrivateKeys) then begin
      // Protection when server is locked to avoid private keys call
      ErrorNum := CT_RPC_ErrNum_NotAllowedCall;
      Exit;
    end;
    If Not _RPCServer.WalletKeys.IsValidPassword then begin
      ErrorNum := CT_RPC_ErrNum_WalletPasswordProtected;
      ErrorDesc := 'Wallet is password protected. Unlock first';
      exit;
    end;
    If Not CapturePubKey('sender_',senderpubkey,ErrorDesc) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
      exit;
    end;
    If Not CapturePubKey('target_',destpubkey,ErrorDesc) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
      exit;
    end;
    Result := SignOpSendTo(
       params.AsString('rawoperations',''),
       params.AsCardinal('protocol',CT_BUILD_PROTOCOL),
       params.AsCardinal('sender',CT_MaxAccount),params.AsCardinal('target',CT_MaxAccount),
       senderpubkey,destpubkey,
       params.AsCardinal('last_n_operation',0),
       ToABEYs(params.AsDouble('amount',0)),
       ToABEYs(params.AsDouble('fee',0)),
       TCrypto.HexaToRaw(params.AsString('payload','')),
       params.AsString('payload_method','dest'),params.AsString('pwd',''));
  end else if (method='changekey') then begin
    // Change key of "account" to "new_enc_pubkey" or "new_b58_pubkey" (encoded public key format) with "fee"
    // If "payload" is present, it will be encoded using "payload_method"
    // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
    // Returns a JSON "Operation Resume format" object when successfull
    // Note: "ophash" will contain block "0" = "pending block"
    if (Not _RPCServer.AllowUsePrivateKeys) then begin
      // Protection when server is locked to avoid private keys call
      ErrorNum := CT_RPC_ErrNum_NotAllowedCall;
      Exit;
    end;
    If Not _RPCServer.WalletKeys.IsValidPassword then begin
      ErrorNum := CT_RPC_ErrNum_WalletPasswordProtected;
      ErrorDesc := 'Wallet is password protected. Unlock first';
      exit;
    end;
    if params.IndexOfName('account')<0 then begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      ErrorDesc := 'Need "account" param';
      exit;
    end;
    c := params.AsCardinal('account',CT_MaxAccount);
    if params.IndexOfName('account_signer')>=0 then begin
      c2 := params.AsCardinal('account_signer',CT_MaxAccount);
    end else c2 := c;
    If Not CapturePubKey('new_',account.accountInfo.accountKey,ErrorDesc) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
      exit;
    end;
    Result := ChangeAccountKey(c2,c,
       account.accountInfo.accountKey,
       ToABEYs(params.AsDouble('fee',0)),
       TCrypto.HexaToRaw(params.AsString('payload','')),
       params.AsString('payload_method','dest'),params.AsString('pwd',''));
  end else if (method='changekeys') then begin
    // Allows a massive change key operation
    // Change key of "accounts" to "new_enc_pubkey" or "new_b58_pubkey" (encoded public key format) with "fee"
    // If "payload" is present, it will be encoded using "payload_method"
    // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
    // Returns a JSON object with result information
    if (Not _RPCServer.AllowUsePrivateKeys) then begin
      // Protection when server is locked to avoid private keys call
      ErrorNum := CT_RPC_ErrNum_NotAllowedCall;
      Exit;
    end;
    If Not _RPCServer.WalletKeys.IsValidPassword then begin
      ErrorNum := CT_RPC_ErrNum_WalletPasswordProtected;
      ErrorDesc := 'Wallet is password protected. Unlock first';
      exit;
    end;
    if params.IndexOfName('accounts')<0 then begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      ErrorDesc := 'Need "accounts" param';
      exit;
    end;
    If Not CapturePubKey('new_',account.accountInfo.accountKey,ErrorDesc) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
      exit;
    end;
    Result := ChangeAccountsKey(params.AsString('accounts',''),
       account.accountInfo.accountKey,
       ToABEYs(params.AsDouble('fee',0)),
       TCrypto.HexaToRaw(params.AsString('payload','')),
       params.AsString('payload_method','dest'),params.AsString('pwd',''));
  end else if (method='signchangekey') then begin
    // Create a Change Key operation and adds it into a "rawoperations" (that can include
    // previous operations). This RPC method is usefull for cold storage, because doesn't
    // need to check or verify accounts status/public key, assuming that passed values
    // are ok.
    // Signs a change key of "account" to "new_enc_pubkey" or "new_b58_pubkey" (encoded public key format) with "fee"
    // needs "old_enc_pubkey" or "old_b58_pubkey" that will be used to find private key in wallet to sign
    // and "last_n_operation" of account.
    // If "payload" is present, it will be encoded using "payload_method"
    // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
    // Returns a JSON "Operations info" containing old "rawoperations" plus new Transaction
    if (Not _RPCServer.AllowUsePrivateKeys) then begin
      // Protection when server is locked to avoid private keys call
      ErrorNum := CT_RPC_ErrNum_NotAllowedCall;
      Exit;
    end;
    If Not _RPCServer.WalletKeys.IsValidPassword then begin
      ErrorNum := CT_RPC_ErrNum_WalletPasswordProtected;
      ErrorDesc := 'Wallet is password protected. Unlock first';
      exit;
    end;
    if params.IndexOfName('account')<0 then begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      ErrorDesc := 'Need "account" param';
      exit;
    end;
    c := params.AsCardinal('account',CT_MaxAccount);
    if params.IndexOfName('account_signer')>=0 then begin
      c2 := params.AsCardinal('account_signer',CT_MaxAccount);
    end else c2 := c;
    If Not CapturePubKey('old_',senderpubkey,ErrorDesc) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
      exit;
    end;
    If Not CapturePubKey('new_',destpubkey,ErrorDesc) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
      exit;
    end;
    Result := SignOpChangeKey(params.AsString('rawoperations',''),
       params.AsCardinal('protocol',CT_BUILD_PROTOCOL),
       c2,c,
       senderpubkey,destpubkey,
       params.AsCardinal('last_n_operation',0),
       ToABEYs(params.AsDouble('fee',0)),
       TCrypto.HexaToRaw(params.AsString('payload','')),
       params.AsString('payload_method','dest'),params.AsString('pwd',''));
  end else if (method='listaccountforsale') then begin
    // Will put a single account in "for sale" state
    if (Not _RPCServer.AllowUsePrivateKeys) then begin
      // Protection when server is locked to avoid private keys call
      ErrorNum := CT_RPC_ErrNum_NotAllowedCall;
      Exit;
    end;
    Result := ListAccountForSale(params);
  end else if (method='signlistaccountforsale') then begin
    if (Not _RPCServer.AllowUsePrivateKeys) then begin
      // Protection when server is locked to avoid private keys call
      ErrorNum := CT_RPC_ErrNum_NotAllowedCall;
      Exit;
    end;
    Result := SignListAccountForSaleColdWallet(params.AsString('rawoperations',''),params);
  end else if (method='delistaccountforsale') then begin
    if (Not _RPCServer.AllowUsePrivateKeys) then begin
      // Protection when server is locked to avoid private keys call
      ErrorNum := CT_RPC_ErrNum_NotAllowedCall;
      Exit;
    end;
    Result := DelistAccountForSale(params);
  end else if (method='signdelistaccountforsale') then begin
    if (Not _RPCServer.AllowUsePrivateKeys) then begin
      // Protection when server is locked to avoid private keys call
      ErrorNum := CT_RPC_ErrNum_NotAllowedCall;
      Exit;
    end;
    Result := SignDelistAccountForSaleColdWallet(params.AsString('rawoperations',''),params);
  end else if (method='buyaccount') then begin
    if (Not _RPCServer.AllowUsePrivateKeys) then begin
      // Protection when server is locked to avoid private keys call
      ErrorNum := CT_RPC_ErrNum_NotAllowedCall;
      Exit;
    end;
    Result := BuyAccount(params);
  end else if (method='signbuyaccount') then begin
    if (Not _RPCServer.AllowUsePrivateKeys) then begin
      // Protection when server is locked to avoid private keys call
      ErrorNum := CT_RPC_ErrNum_NotAllowedCall;
      Exit;
    end;
    Result := SignBuyAccountColdWallet(params.AsString('rawoperations',''),params);
  end else if (method='changeaccountinfo') then begin
    if (Not _RPCServer.AllowUsePrivateKeys) then begin
      // Protection when server is locked to avoid private keys call
      ErrorNum := CT_RPC_ErrNum_NotAllowedCall;
      Exit;
    end;
    Result := ChangeAccountInfo(params);
  end else if (method='signchangeaccountinfo') then begin
    if (Not _RPCServer.AllowUsePrivateKeys) then begin
      // Protection when server is locked to avoid private keys call
      ErrorNum := CT_RPC_ErrNum_NotAllowedCall;
      Exit;
    end;
    Result := SignChangeAccountInfoColdWallet(params.AsString('rawoperations',''),params);
  // V3 new calls
  end else if (method='signmessage') then begin
    if (Not _RPCServer.AllowUsePrivateKeys) then begin
      // Protection when server is locked to avoid private keys call
      ErrorNum := CT_RPC_ErrNum_NotAllowedCall;
      Exit;
    end;
    params.DeleteName('signature');
    Result := DoSignOrVerifyMessage(params);
  end else if (method='verifysign') then begin
    if (params.IndexOfName('signature')<0) then params.GetAsVariant('signature').Value:=''; // Init signature value to force verify
    Result := DoSignOrVerifyMessage(params);
  end else if (method='operationsdelete') then begin
    Result := RawOperations_Delete(params.AsString('rawoperations',''),params.AsInteger('index',-1));
  // V3 Multioperation
  end else if (method='multioperationaddoperation') then begin
    Result := MultiOperationAddOperation(params.AsString('rawoperations',''),params);
  end else if (method='multioperationsignoffline') then begin
    if (Not _RPCServer.AllowUsePrivateKeys) then begin
      // Protection when server is locked to avoid private keys call
      ErrorNum := CT_RPC_ErrNum_NotAllowedCall;
      Exit;
    end;
    Result := MultiOperationSignCold(params.AsString('rawoperations',''),params);
  end else if (method='multioperationsignonline') then begin
    if (Not _RPCServer.AllowUsePrivateKeys) then begin
      // Protection when server is locked to avoid private keys call
      ErrorNum := CT_RPC_ErrNum_NotAllowedCall;
      Exit;
    end;
    Result := MultiOperationSignOnline(params.AsString('rawoperations',''));
  //
  end else if (method='operationsinfo') then begin
    Result := OperationsInfo(params.AsString('rawoperations',''),GetResultArray);
  end else if (method='executeoperations') then begin
    Result := ExecuteOperations(params.AsString('rawoperations',''));

  //
  end else if (method='nodestatus') then begin
    if (Not _RPCServer.AllowUsePrivateKeys) then begin
      // Protection when server is locked to avoid external calls
      ErrorNum := CT_RPC_ErrNum_NotAllowedCall;
      Exit;
    end;
    // Returns a JSON Object with Node status
    GetResultObject.GetAsVariant('ready').Value := False;
    If FNode.IsReady(ansistr) then begin
      GetResultObject.GetAsVariant('ready_s').Value := ansistr;
      if TNetData.NetData.NetStatistics.ActiveConnections>0 then begin
        GetResultObject.GetAsVariant('ready').Value := True;
        if TNetData.NetData.IsDiscoveringServers then begin
          GetResultObject.GetAsVariant('status_s').Value := 'Discovering servers';
        end else if TNetData.NetData.IsGettingNewBlockChainFromClient(ansistr) then begin
          GetResultObject.GetAsVariant('status_s').Value := ansistr;
        end else begin
          GetResultObject.GetAsVariant('status_s').Value := 'Running';
        end;
      end else begin
        GetResultObject.GetAsVariant('ready_s').Value := 'Alone in the world...';
      end;
    end else begin
      GetResultObject.GetAsVariant('ready_s').Value := ansistr;
    end;
    GetResultObject.GetAsVariant('port').Value:=FNode.NetServer.Port;
    GetResultObject.GetAsVariant('locked').Value:=Not _RPCServer.WalletKeys.IsValidPassword;
    GetResultObject.GetAsVariant('timestamp').Value:=UnivDateTimeToUnix(DateTime2UnivDateTime(now));
    GetResultObject.GetAsVariant('version').Value:=CT_ClientAppVersion;
    GetResultObject.GetAsObject('netprotocol').GetAsVariant('ver').Value := CT_NetProtocol_Version;
    GetResultObject.GetAsObject('netprotocol').GetAsVariant('ver_a').Value := CT_NetProtocol_Available;
    GetResultObject.GetAsVariant('blocks').Value:=FNode.Bank.BlocksCount;
    GetResultObject.GetAsVariant('sbh').Value:=TCrypto.ToHexaString(FNode.Bank.LastOperationBlock.initial_safe_box_hash);
    GetResultObject.GetAsVariant('pow').Value:=TCrypto.ToHexaString(FNode.Bank.LastOperationBlock.proof_of_work);
    GetResultObject.GetAsObject('netstats').GetAsVariant('active').Value:=TNetData.NetData.NetStatistics.ActiveConnections;
    GetResultObject.GetAsObject('netstats').GetAsVariant('clients').Value:=TNetData.NetData.NetStatistics.ClientsConnections;
    GetResultObject.GetAsObject('netstats').GetAsVariant('servers').Value:=TNetData.NetData.NetStatistics.ServersConnectionsWithResponse;
    GetResultObject.GetAsObject('netstats').GetAsVariant('servers_t').Value:=TNetData.NetData.NetStatistics.ServersConnections;
    GetResultObject.GetAsObject('netstats').GetAsVariant('total').Value:=TNetData.NetData.NetStatistics.TotalConnections;
    GetResultObject.GetAsObject('netstats').GetAsVariant('tclients').Value:=TNetData.NetData.NetStatistics.TotalClientsConnections;
    GetResultObject.GetAsObject('netstats').GetAsVariant('tservers').Value:=TNetData.NetData.NetStatistics.TotalServersConnections;
    GetResultObject.GetAsObject('netstats').GetAsVariant('breceived').Value:=TNetData.NetData.NetStatistics.BytesReceived;
    GetResultObject.GetAsObject('netstats').GetAsVariant('bsend').Value:=TNetData.NetData.NetStatistics.BytesSend;
    {$IFDEF Use_OpenSSL}
    GetResultObject.GetAsVariant('openssl').Value := IntToHex(OpenSSLVersion,8);
    {$ENDIF}
    nsaarr := TNetData.NetData.NodeServersAddresses.GetValidNodeServers(true,20);
    for i := low(nsaarr) to High(nsaarr) do begin
      jso := GetResultObject.GetAsArray('nodeservers').GetAsObject(i);
      jso.GetAsVariant('ip').Value := nsaarr[i].ip;
      jso.GetAsVariant('port').Value := nsaarr[i].port;
      jso.GetAsVariant('lastcon').Value := nsaarr[i].last_connection;
      jso.GetAsVariant('attempts').Value := nsaarr[i].total_failed_attemps_to_connect;
    end;
    Result := True;
  end else if (method='encodepubkey') then begin
    // Creates a encoded public key based on params
    // Param "ec_nid" can be 714=secp256k1 715=secp384r1 729=secp283k1 716=secp521r1
    // Param "x","y" are x and y ec public keys values in hexadecimal based on ec_nid
    // Returns a hexadecimal value containing encoded public key
    account.accountInfo.accountKey.EC_OpenSSL_NID:=params.AsInteger('ec_nid',0);
    account.accountInfo.accountKey.x:=TCrypto.HexaToRaw(params.AsString('x',''));
    account.accountInfo.accountKey.y:=TCrypto.HexaToRaw(params.AsString('y',''));
    if (account.accountInfo.accountKey.EC_OpenSSL_NID=0) Or (Length(account.accountInfo.accountKey.x)=0) Or (Length(account.accountInfo.accountKey.y)=0) then begin
      ErrorDesc:= 'Need params "ec_nid","x","y" to encodepubkey';
      ErrorNum:= CT_RPC_ErrNum_InvalidPubKey;
      exit;
    end;
    if TAccountComp.IsValidAccountKey(account.accountInfo.accountKey,ansistr) then begin
      jsonresponse.GetAsVariant('result').Value:=TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(account.accountInfo.accountKey));
      Result := True;
    end else begin
      ErrorDesc:= ansistr;
      ErrorNum:= CT_RPC_ErrNum_InvalidPubKey;
    end;
  end else if (method='decodepubkey') then begin
    // Returns "ec_nid", "x" and "y" of an encoded public key (x and y in hexadecimal)
    // Must provide:
    // - Param "enc_pubkey" is an hexadecimal encoded public key (see 'encodepubkey')
    // or
    // - Param "b58_pubkey" is a Base58 encoded public key
    If Not CapturePubKey('',account.accountInfo.accountKey,ErrorDesc) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
      exit;
    end;
    if (TAccountComp.IsValidAccountKey(account.accountInfo.accountKey,ansistr)) then begin
      TABEYJSONComp.FillPublicKeyObject(account.accountInfo.accountKey,GetResultObject);
      Result := True;
    end else begin
      ErrorDesc:= ansistr;
      ErrorNum:= CT_RPC_ErrNum_InvalidPubKey;
    end;
  end else if (method='payloadencrypt') then begin
    // Encrypts a "payload" using "payload_method"
    // "payload_method" types: "none","pubkey"(must provide "enc_pubkey" or "b58_pubkey"),"aes"(must provide "pwd" param)
    // If payload is "pubkey"
    // Returns an hexa string with encrypted payload
    if (params.AsString('payload','')='') then begin
      ErrorNum:= CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Need param "payload"';
      exit;
    end;
    opr.newKey := CT_TWalletKey_NUL.AccountKey;
    if (params.IndexOfName('enc_pubkey')>=0) Or (params.IndexOfName('b58_pubkey')>=0) then begin
      if Not (CapturePubKey('',opr.newKey,ErrorDesc)) then begin
        ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
        exit;
      end;
    end;
    Result := DoEncrypt(TCrypto.HexaToRaw(params.AsString('payload','')),
       opr.newKey,
       params.AsString('payload_method',''),params.AsString('pwd',''));
  end else if (method='payloaddecrypt') then begin
    if (Not _RPCServer.AllowUsePrivateKeys) then begin
      // Protection when server is locked to avoid private keys call
      ErrorNum := CT_RPC_ErrNum_NotAllowedCall;
      Exit;
    end;
    // Decrypts a "payload" searching for wallet private keys and for array of strings in "pwds" param
    // Returns an JSON Object with "result" (Boolean) and
    if (params.AsString('payload','')='') then begin
      ErrorNum:= CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Need param "payload"';
      exit;
    end;
    If Not _RPCServer.WalletKeys.IsValidPassword then begin
      ErrorNum := CT_RPC_ErrNum_WalletPasswordProtected;
      ErrorDesc := 'Wallet is password protected. Unlock first';
      exit;
    end;
    Result := DoDecrypt(TCrypto.HexaToRaw(params.AsString('payload','')),params.GetAsArray('pwds'));
  end else if (method='getconnections') then begin
    if (Not _RPCServer.AllowUsePrivateKeys) then begin
      // Protection when server is locked to avoid private keys call
      ErrorNum := CT_RPC_ErrNum_NotAllowedCall;
      Exit;
    end;
    // Returns an array of connections objects with info about state
    GetConnections;
    Result := true;
  end else if (method='addnewkey') then begin
    // Creates a new private key and stores it on the wallet, returning Public key JSON object
    // Param "ec_nid" can be 714=secp256k1 715=secp384r1 729=secp283k1 716=secp521r1. (Default = CT_Default_EC_OpenSSL_NID)
    // Param "name" is name for this address
    if (Not _RPCServer.AllowUsePrivateKeys) then begin
      // Protection when server is locked to avoid private keys call
      ErrorNum := CT_RPC_ErrNum_NotAllowedCall;
      Exit;
    end;
    If Not _RPCServer.WalletKeys.IsValidPassword then begin
      ErrorNum := CT_RPC_ErrNum_WalletPasswordProtected;
      ErrorDesc := 'Wallet is password protected. Unlock first';
      exit;
    end;
    ecpkey := TECPrivateKey.Create;
    try
      ecpkey.GenerateRandomPrivateKey(params.AsInteger('ec_nid',CT_Default_EC_OpenSSL_NID));
      _RPCServer.FWalletKeys.AddPrivateKey(params.AsString('name',DateTimeToStr(now)),ecpkey);
      TABEYJSONComp.FillPublicKeyObject(ecpkey.PublicKey,GetResultObject);
      Result := true;
    finally
      ecpkey.Free;
    end;
  end else if (method='lock') then begin
    if (Not _RPCServer.AllowUsePrivateKeys) then begin
      // Protection when server is locked to avoid private keys call
      ErrorNum := CT_RPC_ErrNum_NotAllowedCall;
      Exit;
    end;
    jsonresponse.GetAsVariant('result').Value := _RPCServer.WalletKeys.LockWallet;
    Result := true;
  end else if (method='unlock') then begin
    // Unlocks the Wallet with "pwd" password
    // Returns Boolean if wallet is unlocked
    if (Not _RPCServer.AllowUsePrivateKeys) then begin
      // Protection when server is locked to avoid private keys call
      ErrorNum := CT_RPC_ErrNum_NotAllowedCall;
      Exit;
    end;
    if (params.IndexOfName('pwd')<0) then begin
      ErrorNum:= CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Need param "pwd"';
      exit;
    end;
    If Not _RPCServer.WalletKeys.IsValidPassword then begin
      _RPCServer.WalletKeys.WalletPassword:=params.AsString('pwd','');
    end;
    jsonresponse.GetAsVariant('result').Value := _RPCServer.WalletKeys.IsValidPassword;
    Result := true;
  end else if (method='setwalletpassword') then begin
    // Changes the Wallet password with "pwd" param
    // Must be unlocked first
    // Returns Boolean if wallet password changed
    if (Not _RPCServer.AllowUsePrivateKeys) then begin
      // Protection when server is locked to avoid private keys call
      ErrorNum := CT_RPC_ErrNum_NotAllowedCall;
      Exit;
    end;
    If Not _RPCServer.WalletKeys.IsValidPassword then begin
      ErrorNum := CT_RPC_ErrNum_WalletPasswordProtected;
      ErrorDesc := 'Wallet is password protected. Unlock first';
      exit;
    end;
    //
    if (params.IndexOfName('pwd')<0) then begin
      ErrorNum:= CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Need param "pwd"';
      exit;
    end;
    _RPCServer.WalletKeys.WalletPassword:=params.AsString('pwd','');
    jsonresponse.GetAsVariant('result').Value := _RPCServer.WalletKeys.IsValidPassword;
    Result := true;
  end else if (method='stopnode') then begin
    // Stops communications to other nodes
    if (Not _RPCServer.AllowUsePrivateKeys) then begin
      // Protection when server is locked to avoid private keys call
      ErrorNum := CT_RPC_ErrNum_NotAllowedCall;
      Exit;
    end;
    FNode.NetServer.Active := false;
    TNetData.NetData.NetConnectionsActive:=false;
    jsonresponse.GetAsVariant('result').Value := true;
    Result := true;
  end else if (method='startnode') then begin
    // Stops communications to other nodes
    if (Not _RPCServer.AllowUsePrivateKeys) then begin
      // Protection when server is locked to avoid private keys call
      ErrorNum := CT_RPC_ErrNum_NotAllowedCall;
      Exit;
    end;
    FNode.NetServer.Active := true;
    TNetData.NetData.NetConnectionsActive:=true;
    jsonresponse.GetAsVariant('result').Value := true;
    Result := true;
  end else if (method='cleanblacklist') then begin
    if (Not _RPCServer.AllowUsePrivateKeys) then begin
      // Protection when server is locked to avoid private keys call
      ErrorNum := CT_RPC_ErrNum_NotAllowedCall;
      Exit;
    end;
    jsonresponse.GetAsVariant('result').Value := TNetData.NetData.NodeServersAddresses.CleanBlackList(True);
    Result := True;
  end else if (method='node_ip_stats') then begin
    if (Not _RPCServer.AllowUsePrivateKeys) then begin
      // Protection when server is locked to avoid private keys call
      ErrorNum := CT_RPC_ErrNum_NotAllowedCall;
      Exit;
    end;
    Get_node_ip_stats;
    Result := True;
  end else begin
    ErrorNum := CT_RPC_ErrNum_MethodNotFound;
    ErrorDesc := 'Method not found: "'+method+'"';
  end;
end;

{ TRPCServerThread }

procedure TRPCServerThread.BCExecute;
var
  ClientSock:TSocket;
begin
  with FServerSocket do begin
    CreateSocket;
    setLinger(true,10000);
    bind('0.0.0.0',Inttostr(FPort));
    listen;
    repeat
      if terminated then break;
      Try
        if canread(1000) then begin
          ClientSock:=accept;
          if lastError=0 then begin
            TRPCProcess.create(ClientSock);
          end;
        end;
      Except
        On E:Exception do begin
          TLog.NewLog(ltError,Classname,'Error '+E.ClassName+':'+E.Message);
        end;
      End;
      sleep(1);
    until false;
  end;
end;

constructor TRPCServerThread.Create(Port: Word);
begin
  TLog.NewLog(ltInfo,ClassName,'Activating RPC-JSON Server on port '+inttostr(Port));
  FServerSocket:=TTCPBlockSocket.create;
  FPort := Port;
  inherited create(false);
end;

destructor TRPCServerThread.Destroy;
begin
  TLog.NewLog(ltInfo,ClassName,'Stoping RPC-JSON Server');
  FreeAndNil(FServerSocket);
  inherited Destroy;
end;

end.
