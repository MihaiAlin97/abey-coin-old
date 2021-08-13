unit UVirtualMachine;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils,UJSONFunctions,UAccounts,UbaseTypes,UPCDataTypes,UCrypto;

type

  TOpSaveContractData= Record
        account_signer,
        account_target: Cardinal;
        locked_until_block : Cardinal;
        n_operation : Cardinal;
        fee: UInt64;
        starting_block_number_flag: Boolean;
        starting_block_number: Cardinal;
        active_blocks: Cardinal;
        passive_blocks: Cardinal;
        max_instruction_count: Cardinal;
        max_memory_usage_kb: Cardinal;
        max_runtime_seconds: Cardinal;
        max_io_usage: Cardinal;
        bytecode_size: Cardinal;
        export_table_size: Cardinal;
        data_segment_size: Cardinal;
        bytecode: TRawBytes;
        export_table: TRawBytes;
        data_segment : TRawBytes;
        payload: TRawBytes;
        public_key: TAccountKey;
        sign: TECDSA_SIG;
      End;



  { TOpExecuteContractData}
  TOpExecuteContractData = Record
    account_signer, contractID : Cardinal;
    n_operation : Cardinal;        //number of ops. executed by signer
    fee: UInt64;
    function_name : String;
    parameter_types : array of String;
    parameter_values : array of String;
    payload: TRawBytes;
    public_key: TAccountKey;
    sign: TECDSA_SIG;
  end;

  { TODO : This SmartContractData idea could serve as a template for the other operations; Whenever new fields are added to FData of an operation or the class functions change, we can verify these protocol changes through this class. }
  TSmartContractData = Record     //this contains only static metrics and fields needed by contract operations
        contract_signer: Cardinal;
        contract_target : Cardinal;
        contract_caller : Cardinal;

        {TOpSaveContractData}

        starting_block_number_flag: Boolean;    //TOpSaveContractData
        starting_block_number: Cardinal;        //TOpSaveContractData
        active_blocks: Cardinal;                //TOpSaveContractData
        passive_blocks: Cardinal;               //TOpSaveContractData
        max_instruction_count: Cardinal;        //TOpSaveContractData
        max_memory_usage_kb: Cardinal;          //TOpSaveContractData
        max_runtime_seconds: Cardinal;          //TOpSaveContractData
        max_io_usage: Cardinal;                 //TOpSaveContractData
        max_data_segment_size_kb: Cardinal;     //TOpSaveContractData
        export_table_size: Cardinal;            //TOpSaveContractData
        bytecode_size: Cardinal;                //TOpSaveContractData
        export_table: TRawBytes;                //TOpSaveContractData

        {agnostic}
        initial_data_segment : TRawBytes;       //this is data_segment from TOpSaveContractData
        data_segment : TRawBytes;               //this is the data_segment from TOpExecuteContractData
        bytecode: TRawBytes;                    //this is not stored in TOpSaveContractData or TOpExecuteContractData
        bytecodePath:String;
        exportTablePath:String;
        dataSegmentPath:String;

        {TOpExecuteContract}
        function_name : String;                 //TOpExecuteContractData
        parameter_types : array of String;      //TOpExecuteContractData
        parameter_values : array of String;     //TOpExecuteContractData

  end;

  {

  this is a bit of a hack

       TOpSaveCOntract,TOpExecuteContract and their FData fields are needed in here.

       TMetrics,TSmartContractHelper , TSmartContract and TVirtualMachine are needed in URPC.pas and UOpTransaction.pas

       The way to keep smart contract helper classes and operations on smart contracts separarated and at the same time avoid a circular dependency is to:

       - use the functional paradigm

       - keep arguments of types contained in URPC.pas and UOpTransaction.pas as TObject and typecast as needed in the function body or if they are records,redeclare here;

       - add "uses" clause in the implementation section and include units you need(URPC.pas and UOpTransaction.pas)


  }

  { metrics will be of three types -> static: stored in blockchain;
                                   -> static: stored in db or other environment;
                                   -> dynamic: obtained from contract metadata;
                                   -> dynamic: obtained from other blockchain endpoints(JSON-RPC,Vault hashrate,etc)}
  TMetrics = Class
    private
  end;




  // this class is made for making it easier to work with TOpSaveContract, TOpExecuteContract, TSmartContract, TMetrics and TVirtualMachine
  // as there are a lot of dependencies between those classes , I/O operations (loading exportTable, bytecode.qvm,generating the snapshot at saving)

  TSmartContractHelper = Class
    //TODO: add description for each function
    public
      class function SaveContractCheckParameters (var params:TABEYJSONObject;var errors:String;var ErrorNum:Integer; var ErrorDesc: String) : Boolean;     // verifies params inside JSON-RPC call are ok
      class function SaveContractTryGetFDataFromParams ( var params:TABEYJSONObject; var Data: TOpSaveContractData; AccountsCount: Cardinal ; BlocksCount: Cardinal; var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean;
      class function SaveContractFillInitData (BytecodePath:String;ExportTablePath:String;var Data: TOpSaveContractData;var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean;
      class function SaveContractWriteInitDataToByteArray(var InitData:TRawBytes;var Data: TOpSaveContractData;var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean;
      class function CreateOperationSaveContract(current_protocol:Word;privateKey:TECPrivateKey;n_operation:Cardinal;AccountsCount:Cardinal;BlocksCount:Cardinal;var params:TABEYJSONObject;var SaveContract : TObject;var errors:String;var ErrorNum:Integer; var ErrorDesc: String) : Boolean;


      class function ExecuteContractCheckParameters ( var params:TABEYJSONObject;var errors:String;var ErrorNum:Integer; var ErrorDesc: String) : Boolean ;     // verifies params inside JSON-RPC call are ok
      class function ExecuteContractGetContractArguments ( var params:TABEYJSONObject; var Types : Array of String; var Values : Array of String;var errors:String;var ErrorNum:Integer; var ErrorDesc: String) : Boolean ;
      class function ExecuteContractTryGetFDataFromParams ( var params:TABEYJSONObject; var Data: TOpExecuteContractData; AccountsCount: Cardinal ; BlocksCount: Cardinal; var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean;
      class function CreateOperationExecuteContract(current_protocol:Word;privateKey:TECPrivateKey;n_operation:Cardinal;AccountsCount:Cardinal;BlocksCount:Cardinal;var params:TABEYJSONObject;var ExecuteContract : TObject; var payload: TRawBytes;var errors:String;var ErrorNum:Integer; var ErrorDesc: String) : Boolean;

      {class function ExecuteContractTryGetFDataFromParams ( var params:TABEYJSONObject; var Data: TOpSaveContractData; AccountsCount: Cardinal ; BlocksCount: Cardinal; var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean;
      class function ExecuteContractCheckFee(var Data: TObject ;Fee:Double;var errors:String;var ErrorNum : Integer; var ErrorDesc : String) : Boolean ;               // verifies fee is allright
      class function CreateExecuteContractOperation(var params:TABEYJSONObject;var ExecuteContract:TObject;var errors:String;var ErrorNum:Integer; var ErrorDesc: String) : Boolean ;}

      class function LoadBytecodeFromFile(BytecodePath:String ; var Content : TMemoryStream ; var errors:String;var ErrorNum:Integer; var ErrorDesc: String) : Boolean ; overload;
      class function LoadBytecodeFromFile(BytecodePath:String ; var Content : TRawBytes ; var errors:String;var ErrorNum:Integer; var ErrorDesc: String) : Boolean ; overload;
      //class function LoadBytecodeFromDatabase(ContractId:Cardinal ; var Content : TStream ; var errors:String;var ErrorNum:Integer; var ErrorDesc: String) : Boolean ;
      //class function LoadBytecodeFromDatabase(ContractId:Cardinal ; var Content : TRawBytes ; var errors:String;var ErrorNum:Integer; var ErrorDesc: String) : Boolean ;

      class function LoadExportTableFromFile(ExportTablePath:String; var Content : TMemoryStream ; var errors:String;var ErrorNum:Integer; var ErrorDesc: String) : Boolean ; overload;
      class function LoadExportTableFromFile(ExportTablePath:String; var Content : TRawBytes ; var errors:String;var ErrorNum:Integer; var ErrorDesc: String) : Boolean ; overload;
      //class function LoadExportTableFromDatabase(ContractId:Cardinal ; var Content : TStream ; var errors:String;var ErrorNum:Integer; var ErrorDesc: String) : Boolean ;
      //class function LoadExportTableFromDatabase(ContractId:Cardinal ; var Content : TRawBytes ; var errors:String;var ErrorNum:Integer; var ErrorDesc: String) : Boolean ;

      class function LoadDataSegmentFromFile(DataSegmentPath:String;var Content : TmemoryStream ; var errors:String;var ErrorNum:Integer; var ErrorDesc: String) : Boolean ; overload;
      class function LoadDataSegmentFromFile(DataSegmentPath:String;var Content : TRawBytes ; var errors:String;var ErrorNum:Integer; var ErrorDesc: String) : Boolean ; overload;
      //class function LoadDataSegmentFromDatabase(ContractId:Cardinal ; var Content : TStream ; var errors:String;var ErrorNum:Integer; var ErrorDesc: String) : Boolean ;
      //class function LoadDataSegmentFromDatabase(ContractId:Cardinal ; var Content : TRawBytes ; var errors:String;var ErrorNum:Integer; var ErrorDesc: String) : Boolean ;


      class function SaveBytecodeToFile ( BytecodePath : String ; var Content : TMemoryStream ; var errors : String ; var ErrorNum : Integer ; var ErrorDesc : String ) : Boolean ; overload;
      class function SaveBytecodeToFile ( BytecodePath : String ; var Content : TRawBytes ; var errors : String ; var ErrorNum : Integer ; var ErrorDesc : String ) : Boolean ; overload;
      class function SaveExportTableToFile ( ExportTablePath :String ; var Content : TMemoryStream ; var errors : String ; var ErrorNum : Integer ; var ErrorDesc : String ) : Boolean ; overload;
      class function SaveExportTableToFile ( ExportTablePath :String ; var Content : TRawBytes ; var errors : String ; var ErrorNum : Integer ; var ErrorDesc : String ) : Boolean ; overload;
      class function SaveDataSegmentToFile ( DataSegmentPath :String ; var Content : TmemoryStream ; var errors : String ; var ErrorNum : Integer ; var ErrorDesc : String ) : Boolean ; overload;
      class function SaveDataSegmentToFile ( DataSegmentPath :String ; var Content : TRawBytes ; var errors : String ; var ErrorNum : Integer ; var ErrorDesc : String ) : Boolean ; overload;
      class function SaveDataToFile ( FilePath : String ; var Content : TRawBytes; var errors : String ; var ErrorNum : Integer ; var ErrorDesc : String ) : Boolean;


      class function InitDataSegment(BytecodePath:String ; ExportTablePath:String;DataSegmentPath:String;var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean;
      class function RemoveDataSegment(DataSegmentPath:String;var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean;

      class function ModifyDataSegment(FunctionName:String;BytecodePath:String ; ExportTablePath:String;DataSegmentPath:String;var Types : Array of String; var Values : Array of String;var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean ;

      class procedure PrintSaveContractOperationData(Data : UVIRTUALMACHINE.TOpSaveContractData);
      class procedure PrintExecuteContractOperationData(Data : UVIRTUALMACHINE.TOpExecuteContractData);




      {}
      {class function SaveContractInitializationDataToDatabase(ContractId:Cardinal;var ContractInitializationData:TMemoryStream;var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean;  overload;
      class function SaveContractExecutionDataToDatabase(ContractId:Cardinal;var ContractInitializationData:TMemoryStream;var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean;  overload; }

      class function SaveContractInitializationDataToDatabase(ContractId:Cardinal;var ContractInitializationData:TRawBytes;var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean;  overload;
      class function SaveContractInitializationDataToDatabase(ContractId:Cardinal;var ContractInitializationData:TMemoryStream;var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean;  overload;
      class function SaveContractExecutionDataToDatabase(ContractId:Cardinal;var ContractExecutionData:TRawBytes;var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean;  overload;
      class function SaveContractExecutionDataToDatabase(ContractId:Cardinal;var ContractExecutionData:TMemoryStream;var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean;  overload;

      class function LoadContractInitializationDataFromDatabase(ContractId:Cardinal;var ContractInitializationData:TRawBytes;var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean;  overload;
      class function LoadContractInitializationDataFromDatabase(ContractId:Cardinal;var ContractInitializationData:TMemoryStream;var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean;  overload;
      class function LoadContractExecutionDataFromDatabase(ContractId:Cardinal;var ContractExecutionData:TMemoryStream;var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean;  overload;
      class function LoadContractExecutionDataFromDatabase(ContractId:Cardinal;var ContractExecutionData:TRawBytes;var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean;  overload;


      class function GetTempPath(FileName:String):String;
      class function GetTempFolderPath:String;


      class function ExecuteContract(ContractId:Cardinal;FunctionName:String;var Types : Array of String; var Values : Array of String;var TempDataSegmentPath : String;var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean;

      //class function LoadBytecodeFromDatabase(ContractId:Cardinal ; var Content : TStream ; var errors:String;var ErrorNum:Integer; var ErrorDesc: String) : Boolean ;
      //class function LoadBytecodeFromDatabase(ContractId:Cardinal ; var Content : TRawBytes ; var errors:String;var ErrorNum:Integer; var ErrorDesc: String) : Boolean ;


      
     { class function ExecuteContract(ContractData:TSmartContractData;var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean; overload;
      class function ExecuteContract(BytecodePath:String ; ExportTablePath:String;DataSegmentPath:String;var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean;
      }



      {class function SaveContractCheckFee(var Data: TObject;Fee:Double;var errors:String;var ErrorNum : Integer; var ErrorDesc : String):Boolean;               // verifies fee is allright
      class function CreateSaveContractOperation(var params:TABEYJSONObject;var SaveContract:TObject;var errors:String;var ErrorNum:Integer; var ErrorDesc: String) : Boolean;


      class function ExecuteContractCheckParameters ( var params:TABEYJSONObject;var errors:String;var ErrorNum:Integer; var ErrorDesc: String) : Boolean ;     // verifies params inside JSON-RPC call are ok
      class function ExecuteContractTryGetFDataFromParams ( var params:TABEYJSONObject; var Data: TObject; AccountPreviousUpdatedBlock : TAccountPreviousBlockInfo; AccountTransaction : TABEYVaultTransaction; var errors:String;var ErrorNum:Integer; var ErrorDesc: String): Boolean ;
      class function ExecuteContractCheckFee(var Data: TObject ;Fee:Double;var errors:String;var ErrorNum : Integer; var ErrorDesc : String) : Boolean ;               // verifies fee is allright
      class function CreateExecuteContractOperation(var params:TABEYJSONObject;var ExecuteContract:TObject;var errors:String;var ErrorNum:Integer; var ErrorDesc: String) : Boolean ;}







      {class function WriteSaveContractDataToStream ( var  ) ;                                                             // this is meant to be used from inside TOpSaveContract
      class function LoadSaveContractOperationToStream (  ) ;
      class function LoadExecuteContractOperationToStream (  ) ;
      class function LoadFromStream(FData:record,);

      class function LoadContractDataFromFile();   }

  end;

  TSmartContract = class
    private
      SaveData:TOpSaveContractData;
      ExecuteData:TOpExecuteContractData;
      SmartContractData:TSmartContractData;

    private

      //function CreateFromParameters(Data:Pointer;OpType);      // creates record of SaveData
      //function OperationPerformed:OpType;

    public

      {function


      function LoadSaveDataFromStream();

      function ReadFromFile();

      function GetParameters();

      function LoadSaveDataToStream

      function LoadSaveDataFromRecord

      function LoadSaveDataTo

      function LoadExecuteDataFromStream

      function LoadExecuteDataToStream

      function InitializeSmartContract( var errors:String;var ErrorNum:Integer;var ErrorDesc): Integer ;    //Creates a TOpSaveFiles operation

      function ExecuteSmartContract(): Integer;        // Creates a TOpExecuteContractOperation      }



  end;

  {TVirtualMachine = Class
    private
      //SmartContract : TSmartContract;
    public
      SmartContractData;
  end;    }


 Const
     CT_TOpSaveContractData_NUL : TOpSaveContractData = (account_signer:0;account_target:0;locked_until_block:0;n_operation:0;fee:0;starting_block_number_flag:true;starting_block_number:0;active_blocks:0;passive_blocks:0;
     max_instruction_count:0;max_memory_usage_kb:0;max_runtime_seconds:0;max_io_usage:0;bytecode_size:0;export_table_size:0;data_segment_size:0;
     bytecode:Nil;export_table:Nil;data_segment:Nil;payload:Nil;public_key:(EC_OpenSSL_NID:0;x:Nil;y:Nil);sign:(r:Nil;s:Nil));

     CT_TOpExecuteContractData_NUL : TOpExecuteContractData = (account_signer:0; contractID:0; n_operation:0;fee:0; function_name:'' ; parameter_types:Nil;parameter_values:Nil;payload:Nil;public_key:(EC_OpenSSL_NID:0;x:Nil;y:Nil);sign:(r:Nil;s:Nil));

implementation

uses
   URPC,UOpTransaction,Uconst,UFolderHelper,Process,lazlogger,ULog,DBstorage;


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

class function TSmartContractHelper.SaveContractCheckParameters (var params:TABEYJSONObject;var errors:String;var ErrorNum:Integer; var ErrorDesc: String) : Boolean;
begin

  TLog.NewLog(ltInfo,ClassName,'SaveContractCheckParameters' );

    Result := False;

    if params.IndexOfName('contractSignerID')<0 then begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      ErrorDesc := 'Need "contractSignerID" param';
      Errors := ErrorDesc;
      exit;
    end;

    if params.IndexOfName('contractTargetID')<0 then begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      ErrorDesc := 'Need "contractTargetID" param';
      Errors := ErrorDesc;
      exit;
    end;

    if params.IndexOfName('startingBlockIDFlag')<0 then begin
      ErrorNum :=  CT_RPC_ErrNum_FlagForStartingBlock;
      ErrorDesc := 'Need "startingBlockIDFlag" param';
      Errors := ErrorDesc;
      exit;
    end;

    if params.IndexOfName('startingBlockID')<0 then begin
      ErrorNum :=  CT_RPC_ErrNum_ActiveBlocks;
      ErrorDesc := 'Need "startingBlockID" param';
      Errors := ErrorDesc;
     exit;
    end;

    if params.IndexOfName('activeBlocks')<0 then begin
      ErrorNum :=  CT_RPC_ErrNum_ActiveBlocks;
      ErrorDesc := 'Need "activeBlocks" param';
      Errors := ErrorDesc;
     exit;
    end;

    if params.IndexOfName('passiveBlocks')<0 then begin
      ErrorNum :=  CT_RPC_ErrNum_PassiveBlocks;
      ErrorDesc := 'Need "passiveBlocks" param';
      Errors := ErrorDesc;
      exit;
    end;

    if params.IndexOfName('maxInstructionCount')<0 then begin
      ErrorNum :=  CT_RPC_ErrNum_MaxInstructionCount;
      ErrorDesc := 'Need "maxInstructionCount" param';
      Errors := ErrorDesc;
      exit;
    end;

    if params.IndexOfName('maxMemoryUsageKB')<0 then begin
      ErrorNum :=  CT_RPC_ErrNum_MaxMemoryUsage;
      ErrorDesc := 'Need "maxMemoryUsageKB" param';
      Errors := ErrorDesc;
      exit;
    end;

    if params.IndexOfName('maxRuntimeSeconds')<0 then begin
      ErrorNum :=  CT_RPC_ErrNum_MaxRuntimeSeconds;
      ErrorDesc := 'Need "maxRuntimeSeconds" param';
      Errors := ErrorDesc;
      exit;
    end;

    if params.IndexOfName('maxIOUsage')<0 then begin
      ErrorNum :=  CT_RPC_ErrNum_MaxIoUsage;
      ErrorDesc := 'Need "maxIOUsage" param';
      Errors := ErrorDesc;
      exit;
    end;

    if params.IndexOfName('exportTablePath')<0 then begin
      ErrorNum := CT_RPC_ErrNum_ExportTableFilePath;
      ErrorDesc := 'Need "exportTablePath" file path param';
      Errors := ErrorDesc;
      exit;
    end;

    if params.IndexOfName('bytecodePath')<0 then begin
      ErrorNum := CT_RPC_ErrNum_PayloadFilePath;
      ErrorDesc := 'Need "bytecodePath" file path param';
      Errors := ErrorDesc;
    end;

    Result:= True;
end;

Function ToABEYs(jsonCurr : Real) : UInt64;
  Begin
    Result := Round(jsonCurr * 10000);
  End;

class function TSmartContractHelper.SaveContractTryGetFDataFromParams ( var params:TABEYJSONObject; var Data: TOpSaveContractData; AccountsCount: Cardinal ; BlocksCount: Cardinal;  var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean;
var ExportTablePath:String;
    BytecodePath:String;
begin
    TLog.NewLog(ltInfo,ClassName,'SaveContractTryGetFDataFromParams' );
    Result := False;

    Data := CT_TOpSaveContractData_NUL ;

    Data.fee := ToABEYs(params.AsDouble('fee',0));

    if (Data.fee<0) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Invalid fee value';
      Exit;
    end;

    Data.account_signer := params.AsCardinal('contractSignerID',CT_MaxAccount) ;

    if (Data.account_signer<0) or (Data.account_signer >= AccountsCount ) then begin
        ErrorDesc:='Invalid account '+Inttostr(Data.account_signer);
        ErrorNum:= CT_RPC_ErrNum_InvalidAccount;
        Errors := ErrorDesc;
        Exit;
    end;

    Data.account_target :=  params.AsCardinal('contractTargetID',CT_MaxAccount);

    if (Data.account_target<0) or (Data.account_target >= AccountsCount ) then begin
        ErrorDesc:='Invalid account '+Inttostr(Data.account_target);
        ErrorNum:= CT_RPC_ErrNum_InvalidAccount;
        Errors := ErrorDesc;
        Exit;
    end;


    Data.starting_block_number_flag := params.AsBoolean('startingBlockIDFlag', false);

    Data.starting_block_number := params.AsCardinal('startingBlockID', CT_MaxBlock );

    if (Data.starting_block_number < 0) or (Data.starting_block_number >= BlocksCount) then begin
        ErrorDesc:='Invalid startingBlockNumber ' + Inttostr(Data.starting_block_number);
        ErrorNum:= CT_RPC_ErrNum_InvalidAccount;
        Errors := ErrorDesc;
        Exit;
    end;

    Data.active_blocks := params.AsCardinal('activeBlocks', 0);

    if (Data.active_blocks < 0) or (Data.active_blocks >= BlocksCount) then begin
        ErrorDesc:='Invalid activeBlocks ' + Inttostr(Data.active_blocks);
        ErrorNum:= CT_RPC_ErrNum_InvalidAccount;
        Errors := ErrorDesc;
        Exit;
    end;

    Data.passive_blocks := params.AsCardinal('passiveBlocks', 0);

    if (Data.passive_blocks < 0) or (Data.passive_blocks >= BlocksCount) then begin
        ErrorDesc:='Invalid passiveBlocks ' + Inttostr(Data.passive_blocks);
        ErrorNum:= CT_RPC_ErrNum_InvalidAccount;
        Errors := ErrorDesc;
        Exit;
    end;


    Data.max_instruction_count := params.AsCardinal('maxInstructionCount', CT_MaxCardinalValue);

    if (Data.max_instruction_count < 0) or (Data.max_instruction_count >= CT_MaxCardinalValue) then begin
        ErrorDesc:='Invalid maxInstructionCount ' + Inttostr(Data.max_instruction_count);
        ErrorNum:= CT_RPC_ErrNum_InvalidAccount;
        Errors := ErrorDesc;
        Exit;
    end;


    Data.max_memory_usage_kb := params.AsCardinal('maxMemoryUsageKB', CT_MaxCardinalValue);

    if (Data.max_memory_usage_kb < 0) or (Data.max_memory_usage_kb >= CT_MaxCardinalValue) then begin
        ErrorDesc:='Invalid maxMemoryUsageKB ' + Inttostr(Data.max_memory_usage_kb);
        ErrorNum:= CT_RPC_ErrNum_InvalidAccount;
        Errors := ErrorDesc;
        Exit;
    end;


    Data.max_runtime_seconds := params.AsCardinal('maxRuntimeSeconds', CT_MaxCardinalValue);

    if (Data.max_runtime_seconds < 0) or (Data.max_runtime_seconds >= CT_MaxCardinalValue) then begin
        ErrorDesc:='Invalid maxRuntimeSeconds ' + Inttostr(Data.max_runtime_seconds);
        ErrorNum:= CT_RPC_ErrNum_InvalidAccount;
        Errors := ErrorDesc;
        Exit;
    end;

    Data.max_io_usage := params.AsCardinal('maxIOUsage', CT_MaxCardinalValue);

    if (Data.max_io_usage < 0) or (Data.max_io_usage >= CT_MaxCardinalValue) then begin
        ErrorDesc:='Invalid maxIOUsage ' + Inttostr(Data.max_io_usage);
        ErrorNum:= CT_RPC_ErrNum_InvalidAccount;
        Errors := ErrorDesc;
        Exit;
    end;

    ExportTablePath := params.AsString('exportTablePath','');

    BytecodePath := params.AsString('bytecodePath','');
    //check file paths here as we can prevent the error here
    if (Not(FileExists(ExportTablePath))) then begin
       ErrorNum := CT_RPC_ErrNum_ExportTableFilePath;
       ErrorDesc := 'Invalid path to export table file'+ExportTablePath;
       Errors := ErrorDesc;
       Exit;
    end;

    if (Not(FileExists(BytecodePath))) then begin
      ErrorNum := CT_RPC_ErrNum_PayloadFilePath;
      ErrorDesc := 'Invalid path to bytecode file'+BytecodePath;
      Errors := ErrorDesc;
      Exit;
    end;

    //get Data.bytecode,Data.export_table and Data.data_segment);
    if SaveContractFillInitData(BytecodePath,ExportTablePath,Data,errors,ErrorNum,ErrorDesc) = False then Exit;

    Result:=True;

end;

class function TSmartContractHelper.SaveContractFillInitData(BytecodePath:String;ExportTablePath:String;var Data: TOpSaveContractData;var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean;
var MemStream:TMemoryStream;
    DataSegmentPath:String;
    Bytecode:TRawBytes;
    ExportTable:TrawBytes;
    DataSegment:TRawBytes;
    flags:Cardinal = Cardinal(0);
begin

     TLog.NewLog(ltInfo,ClassName,'SaveContractFillInitData');

     Result := False;

     DataSegmentPath := TSmartContractHelper.GetTempPath('dataSegmentInit');

     if InitDataSegment(BytecodePath,ExportTablePath,DataSegmentPath,errors,ErrorNum,ErrorDesc) = False then Exit; // generate data segment by executing initVM function

     if (LoadBytecodeFromFile(BytecodePath,Bytecode,errors,ErrorNum,ErrorDesc) = False ) then begin
      Exit;
     end;


     if (LoadExportTableFromFile(ExportTablePath,ExportTable,errors,ErrorNum,ErrorDesc) = False ) then begin
      Exit;
     end;

     if (LoadDataSegmentFromFile(DataSegmentPath,DataSegment,errors,ErrorNum,ErrorDesc) = False ) then begin
      //RemoveDataSegment(DataSegmentPath,errors,ErrorNum,ErrorDesc);
      Exit;
     end;

     //if RemoveDataSegment(DataSegmentPath,errors,ErrorNum,ErrorDesc) = False then Exit;

     Data.bytecode.Add(Bytecode);
     Data.export_table.Add(ExportTable);
     Data.data_segment.Add(DataSegment);

     Data.bytecode_size := Length(Data.bytecode);
     Data.export_table_size := Length(Data.export_table);
     Data.data_segment_size := Length(Data.data_segment);

     Result := True;
end;


class function TSmartContractHelper.SaveContractWriteInitDataToByteArray(var InitData:TRawBytes;var Data: TOpSaveContractData;var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean;
var MemStream:TMemoryStream;
    flags:Cardinal = Cardinal(0);
begin
     //TODO: Handle errors here;

     TLog.NewLog(ltInfo,ClassName,'SaveContractWriteInitDataToByteArray');

     Result := False;

     MemStream := TMemoryStream.Create;

     MemStream.Position := 0;

     if(Data.starting_block_number_flag) then SetBit(flags, 31);  // if a flag is true , set a bit inside 4 bytes cardinal value "flags"

     // write these metrics to a stream

     MemStream.WriteDWord(flags);
     MemStream.WriteDWord(Data.starting_block_number);
     MemStream.WriteDWord(Data.active_blocks);
     MemStream.WriteDWord(Data.passive_blocks);
     MemStream.WriteDWord(Data.max_instruction_count);
     MemStream.WriteDWord(Data.max_memory_usage_kb);
     MemStream.WriteDWord(Data.max_runtime_seconds);
     MemStream.WriteDWord(Data.max_io_usage);

     MemStream.WriteDWord(Data.bytecode_size);
     MemStream.WriteDWord(Data.export_table_size);
     MemStream.WriteDWord(Data.data_segment_size);

     MemStream.WriteBuffer(Data.bytecode[0],Data.bytecode_size);
     MemStream.WriteBuffer(Data.export_table[0],Data.export_table_size);

     MemStream.Position := 0;

     //move memorystream content to payload field

     // we store only the metrics at this point
     SetLength(InitData, sizeof(Cardinal) * 11 + Data.bytecode_size + Data.export_table_size );

     DebugLn('==================InitData length',IntTostr(Length(InitData)));
     DebugLn('==================Bytecode length',IntTostr(Length(Data.bytecode)));
     DebugLn('==================export_table length',IntTostr(Length(Data.export_table)));

     MemStream.ReadBuffer(InitData[0],Length(InitData));

     Result := True;
end;
{This function does}
class function TSmartContractHelper.CreateOperationSaveContract(current_protocol:Word;privateKey:TECPrivateKey;n_operation:Cardinal;AccountsCount:Cardinal;BlocksCount:Cardinal;var params:TABEYJSONObject;var SaveContract : TObject;var errors:String;var ErrorNum:Integer; var ErrorDesc: String) : Boolean;
      var Data:TOpSaveContractData;
begin
     TLog.NewLog(ltInfo,ClassName,'CreateOperationSaveContract');
     Result := False;

     //validate JSON params
     if SaveContractCheckParameters(params,errors,ErrorNum,ErrorDesc) = False then Exit;

     // Fill Data with params ; except data_segment and payload
     if SaveContractTryGetFDataFromParams(params,Data,AccountsCount,BlocksCount,errors,ErrorNum,ErrorDesc) = False then Exit;


     TOpSaveContractRefactored(SaveContract) := TOpSaveContractRefactored.CreateSaveContract(
        current_protocol,
        Data.account_signer,
        n_operation,
        Data.account_target,
        privateKey,
        Data.fee,
        Data.starting_block_number_flag,
        Data.starting_block_number,
        Data.active_blocks,
        Data.passive_blocks,
        Data.max_instruction_count,
        Data.max_memory_usage_kb,
        Data.max_runtime_seconds,
        Data.max_io_usage,
        Data.bytecode_size,
        Data.export_table_size,
        Data.data_segment_size,
        Data.bytecode,
        Data.export_table,
        Data.data_segment,
        Data.payload);

     Result := True;

end;


class function TSMartContractHelper.ExecuteContractCheckParameters ( var params:TABEYJSONObject;var errors:String;var ErrorNum:Integer; var ErrorDesc: String) : Boolean ;     // verifies params inside JSON-RPC call are ok
begin
    TLog.NewLog(ltInfo,ClassName,'ExecuteContractCheckParameters');

    Result := False;

    if params.IndexOfName('callerID') < 0 then begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      ErrorDesc := 'Need caller "callerID" param';
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

   { if params.IndexOfName('functionParameters') < 0 then begin
      ErrorNum := CT_RPC_ErrNum_InvalidFunctionName;
      ErrorDesc := 'Need "functionParameters" param';
      exit;
    end;           }



    Result:= True;
end;

class function TSmartContractHelper.ExecuteContractGetContractArguments ( var params:TABEYJSONObject; var Types : Array of String; var Values : Array of String;var errors:String;var ErrorNum:Integer; var ErrorDesc: String) : Boolean ;     // verifies params inside JSON-RPC call are ok
var ArgumentsArr:TABEYJSONArray;
    FuncValue : String;
    FuncType : String;
    i: Integer;
begin
    TLog.NewLog(ltInfo,ClassName,'ExecuteContractGetContractArguments');

    Result := False;


    if params.IndexOfName('functionParameters') < 0 then begin
      Result := True;
      exit;
    end;

    ArgumentsArr := params.GetAsArray('functionParameters');

    for I:= 0 to ArgumentsArr.Count do begin

    FuncType := ArgumentsArr.GetAsObject(i).AsString('type', '');

    if FuncType.Equals('char') then
    begin
      Types[i] := 'char';

      FuncValue := ArgumentsArr.GetAsObject(i).AsString('value', '');
      if FuncValue.Equals('') then
          begin
            ErrorNum := CT_RPC_ErrNum_InvalidParameterValueForType;
            ErrorDesc := 'Missing value for "char" type paramter';
          end
          else
          begin
            Values[i] := FuncValue;
          end;
    end
    else if FuncType.equals('int') then
    begin

      Types[i] := 'int';
      FuncValue := ArgumentsArr.GetAsObject(i).AsString('value', '');
      if FuncValue.Equals('') then
          begin
            ErrorNum := CT_RPC_ErrNum_InvalidParameterValueForType;
            ErrorDesc := 'Missing value for "int" type paramter';
          end
          else
          begin
            Values[i] := FuncValue;

          end;
    end
    else if FuncType.equals('float') then
    begin
      Types[i] := 'float';
      FuncValue := ArgumentsArr.GetAsObject(i).AsString('value', '');
      if FuncValue.Equals('') then
          begin
            ErrorNum := CT_RPC_ErrNum_InvalidParameterValueForType;
            ErrorDesc := 'Missing value for "float" type paramter';
          end
          else
          begin
            Values[i] := FuncValue;
          end;
    end
    else if FuncType.equals('double') then
    begin
      Types[i] := 'double';
      FuncValue := ArgumentsArr.GetAsObject(i).AsString('value', '');
      if FuncValue.Equals('') then
          begin
            ErrorNum := CT_RPC_ErrNum_InvalidParameterValueForType;
            ErrorDesc := 'Missing value for "double" type paramter';
          end
          else
          begin
            Values[i] := FuncValue;
          end;
    end
    else
    begin
      ErrorNum := CT_RPC_ErrNum_InvalidParameterTypeForContract;
      ErrorDesc := 'Invalid Parameter Type for execute contract v2';
    end;

  end;

    Result:= True;
end;

class function TSmartContractHelper.ExecuteContractTryGetFDataFromParams ( var params:TABEYJSONObject; var Data: TOpExecuteContractData; AccountsCount: Cardinal ; BlocksCount: Cardinal; var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean;
var CallerAccount : Cardinal;
    Types: Array of String;
    Values: Array of String;
begin
    TLog.NewLog(ltInfo,ClassName,'ExecuteContractTryGetFDataFromParams');

    Result := False;

    Data := CT_TOpExecuteContractData_NUL ;

    Data.fee := ToABEYs(params.AsDouble('fee',0));

    if (Data.fee<0) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Invalid fee value';
      Exit;
    end;

    Data.account_signer := params.AsCardinal('callerID',CT_MaxAccount) ;

    if (Data.account_signer<0) or (Data.account_signer >= AccountsCount ) then begin
        ErrorDesc:='Invalid account '+Inttostr(Data.account_signer);
        ErrorNum:= CT_RPC_ErrNum_InvalidAccount;
        Errors := ErrorDesc;
        Exit;
    end;

    Data.contractId :=  params.AsCardinal('contractID',CT_MaxAccount);

    if (Data.contractId<0) or (Data.contractId >= AccountsCount ) then begin
        ErrorDesc:='Invalid contractID: '+Inttostr(Data.contractId);
        ErrorNum:= CT_RPC_ErrNum_InvalidAccount;
        Errors := ErrorDesc;
        Exit;
    end;

    Data.function_name := params.AsString('functionName','');

    if Data.function_name = '' then begin
        ErrorDesc:='functionName param empty ';
        ErrorNum:= CT_RPC_ErrNum_InvalidData;
        Errors := ErrorDesc;
        Exit;
    end;

    TLog.NewLog(ltInfo,ClassName,'ArgumentsSize'+IntTOStr(params.GetAsArray('functionParameters').Count));

    SetLength(Data.parameter_types, params.GetAsArray('functionParameters').Count);
    SetLength(Data.parameter_values, params.GetAsArray('functionParameters').Count);

    if ExecuteContractGetContractArguments(params,Data.parameter_types,Data.parameter_values,errors,ErrorNum,ErrorDesc) = False then Exit;

    Result := True

end;


class function TSmartContractHelper.CreateOperationExecuteContract(current_protocol:Word;privateKey:TECPrivateKey;n_operation:Cardinal;AccountsCount:Cardinal;BlocksCount:Cardinal;var params:TABEYJSONObject;var ExecuteContract : TObject; var payload: TRawBytes;var errors:String;var ErrorNum:Integer; var ErrorDesc: String) : Boolean;
var Data : TOPExecuteContractData;
begin
    TLog.NewLog(ltInfo,ClassName,'CreateOperationExecuteContract');

    Result := False;



     //validate JSON params
     if ExecuteContractCheckParameters(params,errors,ErrorNum,ErrorDesc) = False then Exit;



     // Fill Data with params ; except data_segment and payload
     if ExecuteContractTryGetFDataFromParams(params,Data,AccountsCount,BlocksCount,errors,ErrorNum,ErrorDesc) = False then Exit;



     TOpExecuteContractRefactored( ExecuteContract ) := TOpExecuteContractRefactored.CreateExecuteContract(
     current_protocol,
     Data.account_signer,
     n_operation,
     Data.contractID,
     privateKey,
     Data.fee,
     Data.function_name,
     Data.parameter_types,
     Data.parameter_values,
     payload);


     Result := True;


end;
class function TSmartContractHelper.LoadBytecodeFromFile(BytecodePath:String ; var Content : TMemoryStream ; var errors:String;var ErrorNum:Integer; var ErrorDesc: String) : Boolean ; overload;
begin
    TLog.NewLog(ltInfo,ClassName,'LoadBytecodeFromFile');

    Result := False ;
    Content := TMemoryStream.Create;

    try
      Content.LoadFromFile(BytecodePath);
    except
    on E:Exception do
       begin
          ErrorNum := CT_RPC_ErrNum_PayloadFilePath;
          ErrorDesc := 'Error loading bytecode from file'+E.Message;
          Errors := ErrorDesc;
          Exit;
       end;
    end;

    Result := True;

end;

class function TSmartContractHelper.LoadBytecodeFromFile(BytecodePath:String ; var Content : TRawBytes ; var errors:String;var ErrorNum:Integer; var ErrorDesc: String) : Boolean ;  overload;
var Ms:TMemoryStream;
begin
    TLog.NewLog(ltInfo,ClassName,'LoadBytecodeFromFile');

    Result := False;

    //be sure content is empty
    SetLength(Content,0);

    if LoadBytecodeFromFile(BytecodePath,Ms,errors,ErrorNum,ErrorDesc) = True then
    begin
      ms.Position:=0;
      SetLength(Content,Ms.Size);
      Ms.ReadBuffer(Content[Low(Content)],Ms.Size);
      Result := True;
    end;

end;

class function TSmartContractHelper.LoadExportTableFromFile(ExportTablePath:String ; var Content : TMemoryStream ; var errors:String;var ErrorNum:Integer; var ErrorDesc: String) : Boolean ; overload;
begin
    TLog.NewLog(ltInfo,ClassName,'LoadExportTableFromFile');

    Result := False ;
    Content := TMemoryStream.Create;

    try
      Content.LoadFromFile(ExportTablePath);
    except
    on E:Exception do
       begin
          ErrorNum := CT_RPC_ErrNum_PayloadFilePath;
          ErrorDesc := 'Error loading export table from file'+E.Message;;
          Errors := ErrorDesc;
          Exit;
       end;
    end;

    Result := True;

end;

class function TSmartContractHelper.LoadExportTableFromFile(ExportTablePath:String ; var Content : TRawBytes ; var errors:String;var ErrorNum:Integer; var ErrorDesc: String) : Boolean ; overload;
var Ms:TMemoryStream;
begin
    TLog.NewLog(ltInfo,ClassName,'LoadExportTableFromFile');
    Result := False;

    //be sure content is empty
    SetLength(Content,0);

    if LoadExportTableFromFile(ExportTablePath,Ms,errors,ErrorNum,ErrorDesc) = True then
    begin
      ms.Position:=0;
      SetLength(Content,Ms.Size);
      Ms.ReadBuffer(Content[Low(Content)],Ms.Size);
      Result := True;
    end;

end;


class function TSmartContractHelper.LoadDataSegmentFromFile(DataSegmentPath:String;var Content : TmemoryStream ; var errors:String;var ErrorNum:Integer; var ErrorDesc: String) : Boolean ; overload;
begin
    TLog.NewLog(ltInfo,ClassName,'LoadDataSegmentFromFile');
    Result := False ;
    Content := TMemoryStream.Create;

    try
      Content.LoadFromFile(DataSegmentpath);
    except
    on E:Exception do
       begin
          ErrorNum := CT_RPC_ErrNum_PayloadFilePath;
          ErrorDesc := 'Error loading data segment from file:'+E.Message;;
          Errors := ErrorDesc;
          Exit;
       end;
    end;

    Result := True;

end;

class function TSmartContractHelper.LoadDataSegmentFromFile(DataSegmentpath:String ; var Content : TRawBytes ; var errors:String;var ErrorNum:Integer; var ErrorDesc: String) : Boolean ; overload;
var Ms:TMemoryStream;
begin
    TLog.NewLog(ltInfo,ClassName,'LoadDataSegmentFromFile');
    Result := False;

    //be sure content is empty
    SetLength(Content,0);

    if LoadDataSegmentFromFile(DataSegmentpath,Ms,errors,ErrorNum,ErrorDesc) = True then
    begin
      ms.Position:=0;
      SetLength(Content,Ms.Size);
      Ms.ReadBuffer(Content[Low(Content)],Ms.Size);
      Result := True;
    end;

end;


class function TSmartContractHelper.SaveBytecodeToFile(BytecodePath:String ; var Content : TMemoryStream ; var errors:String;var ErrorNum:Integer; var ErrorDesc: String) : Boolean ; overload;
var ContentBytes:TRawBytes;
begin
    TLog.NewLog(ltInfo,ClassName,'SaveBytecodeToFile');
    Result := False;

    Content.Position:=0;

    SetLength(ContentBytes,Content.Size);

    Content.ReadBuffer(ContentBytes[0],Content.Size);

    if SaveBytecodeToFile(BytecodePath,ContentBytes,errors,ErrorNum,ErrorDesc) = False then Exit;

    Result := True;


end;

class function TSmartContractHelper.SaveBytecodeToFile(BytecodePath:String ; var Content : TRawBytes ; var errors:String;var ErrorNum:Integer; var ErrorDesc: String) : Boolean ; overload;
Var Count:Integer;
    BytecodeFile:TFileStream;
begin
  TLog.NewLog(ltInfo,ClassName,'SaveBytecodeToFile');
  Result := False;


  if SaveDataToFile(BytecodePath,Content,errors,ErrorNum,ErrorDesc) = False then begin
    ErrorDesc := 'Bytecode: ' + ErrorDesc;
    errors := ErrorDesc;
    Exit;
  end;

  Result := True;
end;

class function TSmartContractHelper.SaveExportTableToFile(ExportTablePath:String; var Content : TMemoryStream ; var errors:String;var ErrorNum:Integer; var ErrorDesc: String) : Boolean ; overload;
var ContentBytes:TRawBytes;
begin
  TLog.NewLog(ltInfo,ClassName,'SaveExportTableToFile');
    Result := False;

    Content.Position:=0;

    SetLength(ContentBytes,Content.Size);

    Content.ReadBuffer(ContentBytes[0],Content.Size);

    if SaveExportTableToFile(ExportTablePath,ContentBytes,errors,ErrorNum,ErrorDesc) = False then Exit;

    Result := True;


end;

class function TSmartContractHelper.SaveExportTableToFile(ExportTablePath:String; var Content : TRawBytes ; var errors:String;var ErrorNum:Integer; var ErrorDesc: String) : Boolean ; overload;
Var Count:Integer;
    ExporTableFile:TFileStream;
begin

  TLog.NewLog(ltInfo,ClassName,'SaveExportTableToFile');
  Result := False;

  if SaveDataToFile(ExportTablePath,Content,errors,ErrorNum,ErrorDesc) = False then begin
    ErrorDesc := 'ExportTable: ' + ErrorDesc;
    errors := ErrorDesc;
    Exit;
  end;

  Result := True;

end;

class function TSmartContractHelper.SaveDataSegmentToFile(DataSegmentPath:String;var Content : TmemoryStream ; var errors:String;var ErrorNum:Integer; var ErrorDesc: String) : Boolean ; overload;
var ContentBytes:TRawBytes;
begin
  TLog.NewLog(ltInfo,ClassName,'SaveDataSegmentToFile');
  Result := False;

  Content.Position:=0;

  SetLength(ContentBytes,Content.Size);

  Content.ReadBuffer(ContentBytes[0],Content.Size);

  if SaveDataSegmentToFile(DataSegmentPath,ContentBytes,errors,ErrorNum,ErrorDesc) = False then Exit;

  Result := True;


end;

class function TSmartContractHelper.SaveDataSegmentToFile(DataSegmentPath:String;var Content : TRawBytes ; var errors:String;var ErrorNum:Integer; var ErrorDesc: String) : Boolean ; overload;
Var Count:Integer;
    DataSegmentFile:TFileStream;
begin
  TLog.NewLog(ltInfo,ClassName,'SaveDataSegmentToFile');

  Result := False;

  if SaveDataToFile(DataSegmentPath,Content,errors,ErrorNum,ErrorDesc) = False then begin
    ErrorDesc := 'DataSegment: ' + ErrorDesc;
    errors := ErrorDesc;
    Exit;
  end;

  Result := True;

end;

class function TSmartContractHelper.SaveDataToFile ( FilePath : String ; var Content : TRawBytes; var errors : String ; var ErrorNum : Integer ; var ErrorDesc : String ) : Boolean;
Var Count:Integer;
    FileStream:TFileStream;
begin
  TLog.NewLog(ltInfo,ClassName,'SaveDataToFile');
 { Result := False;
  try
    try
      FileStream := TFileStream.Create(FilePath, fmCreate);
      Count := FileStream.Write(Content[0],Length(Content));
    finally
      FileStream.Free;

      if Count <> Length(Content) then begin
        ErrorDesc := 'Incomplete writing of file';
        ErrorNum  :=  CT_RPC_ErrNum_InvalidData;
        Exit;
      end;
    end;
  except
    on E: EInOutError do begin
      ErrorDesc := 'Error saving data to file:' + E.Message;
      ErrorNum  := E.ErrorCode;
      errors := ErrorDesc;
      Exit;
    end;
  end;

  Result := True;}

  {$IOCHECKS ON}
  Result := False;
  try
    FileStream := TFileStream.Create(FilePath, fmCreate);
    Count := FileStream.Write(Content[0],Length(Content));
    FileStream.Free;

    if Count <> Length(Content) then begin
      ErrorDesc := 'Incomplete writing of file';
      ErrorNum  :=  CT_RPC_ErrNum_InvalidData;
      Exit;
    end;
  except
    on E: EInOutError do begin
      ErrorDesc := 'Error saving data to file:' + E.Message;
      ErrorNum  := E.ErrorCode;
      errors := ErrorDesc;
      Exit;
    end;
  end;

  Result := True;

end;

class function TSmartContractHelper.InitDataSegment(BytecodePath:String ; ExportTablePath:String;DataSegmentPath:String;var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean;
var
    //TODO: redo this function
    //TODO: Check TProcess.Free and why it gives an AccessViolation when trying to do VirtualMachine.Free
      VirtualMachine : TProcess;
      PathToVm:String;
      InitVmLim:Integer=10000;
      VmOutput:String;
      BytesRead    : longint;
      Buffer       : array[1..2048] of byte;
      OutputStream : TStream;
begin
  TLog.NewLog(ltInfo,ClassName,'InitDataSegment');
  Result := False;
  {$IFDEF WINDOWS}
  PathToVm := GetCurrentDir + PathDelim + 'abey-vm.exe';
  {$ELSE}  //Linux + Mac
  PathToVm := GetCurrentDir + PathDelim + 'abey-vm';
  //do error checking, lacking to q3vm
  {$ENDIF WINDOWS}

  VirtualMachine := TProcess.Create(nil);
  VirtualMachine.Executable := PathToVm;



  VirtualMachine.Parameters.Add('--bytecodePath');
  VirtualMachine.Parameters.Add(BytecodePath);

  VirtualMachine.Parameters.Add('--snapshotPath');
  VirtualMachine.Parameters.Add(DataSegmentPath);

  VirtualMachine.Parameters.Add('--exportTablePath');
  VirtualMachine.Parameters.Add(ExportTablePath);

  VirtualMachine.Parameters.Add('--instructionCountLimit');
  VirtualMachine.Parameters.Add(IntToStr(InitVmLim));

  VirtualMachine.Parameters.Add('--functionName');
  VirtualMachine.Parameters.Add('initVM');




  VirtualMachine.Options := VirtualMachine.Options + [poUsePipes] + [poStderrToOutPut];
  VirtualMachine.Execute;

  OutputStream := TMemoryStream.Create;
  repeat
    BytesRead := VirtualMachine.Output.Read(Buffer, 2048);
    OutputStream.Write(Buffer, BytesRead);
  until Not(VirtualMachine.Running);

  with TStringList.Create do
  begin
    OutputStream.Position := 0;
    LoadFromStream(OutputStream);
    VmOutput := ErrorDesc + ' ' + Text;
  end;

  if (VirtualMachine.ExitStatus <> 0) then  begin
    ErrorDesc := 'Error during execution of contract : ' + VmOutput;
    ErrorNum:= CT_RPC_ErrNum_ErrorDuringFirstExecutionOfContract;
    //VirtualMachine.Free;
    Exit;
  end;
  ///VirtualMachine.Free;
  Result := True;
end;

class function TSmartContractHelper.ModifyDataSegment(FunctionName:String;BytecodePath:String ; ExportTablePath:String;DataSegmentPath:String;var Types : Array of String; var Values : Array of String;var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean;
var
    //TODO: redo this function
    //TODO: Check TProcess.Free and why it gives an AccessViolation when trying to do VirtualMachine.Free
      VirtualMachine : TProcess;
      PathToVm:String;
      InitVmLim:Integer=10000000;
      VmOutput:String;
      BytesRead    : longint;
      Buffer       : array[1..2048] of byte;
      OutputStream : TStream;
      I:Integer;
begin
    TLog.NewLog(ltInfo,ClassName,'ModifyDataSegment' );

    Result := False;
    {$IFDEF WINDOWS}
    PathToVm := GetCurrentDir + PathDelim + 'abey-vm.exe';
    {$ELSE}  //Linux + Mac
    PathToVm := GetCurrentDir + PathDelim + 'abey-vm';
    //do error checking;
    {$ENDIF WINDOWS}

    VirtualMachine := TProcess.Create(nil);
    VirtualMachine.Executable := PathToVm;



    VirtualMachine.Parameters.Add('--bytecodePath');
    VirtualMachine.Parameters.Add(BytecodePath);

    VirtualMachine.Parameters.Add('--snapshotPath');
    VirtualMachine.Parameters.Add(DataSegmentPath);

    VirtualMachine.Parameters.Add('--exportTablePath');
    VirtualMachine.Parameters.Add(ExportTablePath);

    VirtualMachine.Parameters.Add('--instructionCountLimit');
    VirtualMachine.Parameters.Add(IntToStr(InitVmLim));

    VirtualMachine.Parameters.Add('--functionName');
    VirtualMachine.Parameters.Add(FunctionName);

    for I := Low(Types) to High(Types) do begin

        TLog.NewLog(ltInfo,ClassName,'ModifyDataSegment functionArguments: ' + 'Value: ' + Values[I] + '; Type: ' + Types[I] );

        if Types[I] = 'int' then begin

           VirtualMachine.Parameters.Add('--arg');
           VirtualMachine.Parameters.Add(Values[I]);

           VirtualMachine.Parameters.Add('--size');
           VirtualMachine.Parameters.Add(IntToStr(sizeof(Integer)));

        end;

        if Types[I] = 'char' then begin

           VirtualMachine.Parameters.Add('--arg');
           VirtualMachine.Parameters.Add(Values[I]);

           VirtualMachine.Parameters.Add('--size');
           VirtualMachine.Parameters.Add(IntToStr(sizeof(Byte)));

        end;

        if Types[I] = 'string' then begin

           VirtualMachine.Parameters.Add('--arg');
           VirtualMachine.Parameters.Add( Values[I] );

           VirtualMachine.Parameters.Add('--size');
           VirtualMachine.Parameters.Add( IntToStr( Length(Values[I] ) ) );

        end;



    end;




    VirtualMachine.Options := VirtualMachine.Options + [poUsePipes] + [poStderrToOutPut];
    VirtualMachine.Execute;

    OutputStream := TMemoryStream.Create;
    repeat
      BytesRead := VirtualMachine.Output.Read(Buffer, 2048);
      OutputStream.Write(Buffer, BytesRead);
    until Not(VirtualMachine.Running);

    with TStringList.Create do
    begin
      OutputStream.Position := 0;
      LoadFromStream(OutputStream);
      VmOutput := ErrorDesc + ' ' + Text;
    end;

    if (VirtualMachine.ExitStatus <> 0) then  begin
      ErrorDesc := 'Error during execution of contract : ' + VmOutput;
      ErrorNum:= CT_RPC_ErrNum_ErrorDuringFirstExecutionOfContract;
      //VirtualMachine.Free;
      Exit;
    end;

    TLog.NewLog(ltInfo,ClassName,'ModifyDataSegment VmOutput: '+VmOutput);
    ///VirtualMachine.Free;
    Result := True;
end;

class function TSmartContractHelper.RemoveDataSegment(DataSegmentPath:String;var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean;
begin
  TLog.NewLog(ltInfo,ClassName,'RemoveDataSegment' );
    if DeleteFile(DataSegmentPath) then Result := True
    else begin
       ErrorDesc := 'Cannot remove data segment after initialization of contract: ' + IntTOStr(10);
       ErrorNum:= CT_RPC_ErrNum_ErrorDuringFirstExecutionOfContract;
       Errors := ErrorDesc;
    end;
end;

class procedure TSmartContractHelper.PrintSaveContractOperationData(Data : TOpSaveContractData);
var FunctionName:String;
begin
  FunctionName:='PrintSaveContractOperationData';

  TLog.NewLog(ltInfo,ClassName,'PrintSaveContractOperationData');

  TLog.NewLog(ltInfo,Classname,FunctionName + ' account_signer ' + IntToStr(Data.account_signer));
  TLog.NewLog(ltInfo,Classname,FunctionName + ' account_target ' + IntToStr(Data.account_target));
  TLog.NewLog(ltInfo,Classname,FunctionName + ' locked_until_block ' + IntToStr(Data.locked_until_block));
  TLog.NewLog(ltInfo,Classname,FunctionName + ' n_operation ' + IntToStr(Data.n_operation));
  TLog.NewLog(ltInfo,Classname,FunctionName + ' fee ' + IntToStr(Data.fee));
  TLog.NewLog(ltInfo,Classname,FunctionName + ' starting_block_number_flag ' + BoolToStr(Data.starting_block_number_flag));
  TLog.NewLog(ltInfo,Classname,FunctionName + ' starting_block_number ' + IntToStr(Data.starting_block_number));
  TLog.NewLog(ltInfo,Classname,FunctionName + ' active_blocks ' + IntToStr(Data.active_blocks));
  TLog.NewLog(ltInfo,Classname,FunctionName + ' passive_blocks ' + IntToStr(Data.passive_blocks));
  TLog.NewLog(ltInfo,Classname,FunctionName + ' max_instruction_count ' + IntToStr(Data.max_instruction_count));
  TLog.NewLog(ltInfo,Classname,FunctionName + ' max_memory_usage_kb ' + IntToStr(Data.max_memory_usage_kb));
  TLog.NewLog(ltInfo,Classname,FunctionName + ' max_runtime_seconds ' + IntToStr(Data.max_runtime_seconds));
  TLog.NewLog(ltInfo,Classname,FunctionName + ' max_io_usage ' + IntToStr(Data.max_io_usage));
  TLog.NewLog(ltInfo,Classname,FunctionName + ' bytecode_size ' + IntToStr(Data.bytecode_size));
  TLog.NewLog(ltInfo,Classname,FunctionName + ' export_table_size ' + IntToStr(Data.export_table_size));
  TLog.NewLog(ltInfo,Classname,FunctionName + ' data_segment_size ' + IntToStr(Data.data_segment_size));
  TLog.NewLog(ltInfo,Classname,FunctionName + ' bytecode ' + TCrypto.ToHexaString(Data.bytecode));
  TLog.NewLog(ltInfo,Classname,FunctionName + ' export_table ' + TCrypto.ToHexaString(Data.export_table));
  TLog.NewLog(ltInfo,Classname,FunctionName + ' data_segment ' + TCrypto.ToHexaString(Data.data_segment));
  TLog.NewLog(ltInfo,Classname,FunctionName + ' payload ' + TCrypto.ToHexaString(Data.payload));
  TLog.NewLog(ltInfo,Classname,FunctionName + ' PK-x ' + TCrypto.ToHexaString(Data.public_key.x));
  TLog.NewLog(ltInfo,Classname,FunctionName + ' PK-y ' + TCrypto.ToHexaString(Data.public_key.y));
  TLog.NewLog(ltInfo,Classname,FunctionName + ' Sign-x ' + TCrypto.ToHexaString(Data.sign.r));
  TLog.NewLog(ltInfo,Classname,FunctionName + ' Sign-y ' + TCrypto.ToHexaString(Data.sign.s));


end;

class procedure TSmartContractHelper.PrintExecuteContractOperationData(Data : UVIRTUALMACHINE.TOpExecuteContractData);
var FunctionName:String;
begin
   FunctionName:='PrintExecuteContractOperationData';

   TLog.NewLog(ltInfo,ClassName,FunctionName + 'PrintExecuteContractOperationData');
   TLog.NewLog(ltInfo,Classname,FunctionName + 'account_signer ' + IntToStr(Data.account_signer));
   TLog.NewLog(ltInfo,Classname,FunctionName + 'n_operation ' + IntToStr(Data.n_operation));
   TLog.NewLog(ltInfo,Classname,FunctionName + 'fee ' + IntToStr(Data.fee));
   TLog.NewLog(ltInfo,Classname,FunctionName + 'function_name ' + Data.function_name);

end;


class function TSmartContractHelper.SaveContractInitializationDataToDatabase(ContractId:Cardinal;var ContractInitializationData:TRawBytes;var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean;  overload;
var
      PathToDb:String;
      ContractInitializationDatabase : TABEYBlockchainDBStorage;
begin
  TLog.NewLog(ltInfo,ClassName,'SaveContractInitializationDataToDatabase');
  Result := False;

   //get database for saving contracts
   PathToDb := TFolderHelper.GetContractStorageFolder;

   ContractInitializationDatabase := TABEYBlockchainDBStorage.GetInstance('Initialization', (PathToDB), ErrorNum);

   // check to see if instance exists

   if ErrorNum <> CT_SUCCESS then
   begin
      ErrorDesc := 'Error getting initialization database for contracts, Error Number = ' + IntToStr(ErrorNum);
      errors := ErrorDesc;
      Exit;
   end;

   Debugln('nna'+intToStr( ContractId ));
   ErrorNum := ContractInitializationDatabase.InsertItem( IntToStr( ContractId ),ContractInitializationData) ;
   if ErrorNum <> CT_SUCCESS then
   begin
       ErrorDesc := 'Error saving initialization data for ContractId '+ IntToStr(ContractId) +' , Error Number = ' + IntToStr(ErrorNum);
       errors := ErrorDesc;
       Exit;
   end;

   Result :=True;
end;

class function TSmartContractHelper.SaveContractInitializationDataToDatabase(ContractId:Cardinal;var ContractInitializationData:TMemoryStream;var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean;  overload;
var
      PathToDb:String;
      ContractInitializationDatabase : TABEYBlockchainDBStorage;
begin
  TLog.NewLog(ltInfo,ClassName,'SaveContractInitializationDataToDatabase');
  Result := False;

   //get database for saving contracts
   PathToDb := TFolderHelper.GetContractStorageFolder;

   ContractInitializationDatabase := TABEYBlockchainDBStorage.GetInstance('Initialization', (PathToDB), ErrorNum);

   // check to see if instance exists

   if ErrorNum <> CT_SUCCESS then
   begin
      ErrorDesc := 'Error getting initialization database for contracts, Error Number = ' + IntToStr(ErrorNum);
      errors := ErrorDesc;
      Exit;
   end;

   //be sure position in stream is set in the beginning
   ContractInitializationData.Seek(0,soBeginning);

   ErrorNum := ContractInitializationDatabase.InsertItem( IntToStr( ContractId ),ContractInitializationData ) ;
   if ErrorNum <> CT_SUCCESS then
   begin
       ErrorDesc := 'Error saving initialization data for ContractId '+ IntToStr(ContractId) +' , Error Number = ' + IntToStr(ErrorNum);
       errors := ErrorDesc;
       Exit;
   end;

   ContractInitializationData.Seek(0,soBeginning);

   Result :=True;
end;

class function TSmartContractHelper.SaveContractExecutionDataToDatabase(ContractId:Cardinal;var ContractExecutionData:TMemoryStream;var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean;  overload;
var
      PathToDb:String;
      ContractExecutionDatabase : TABEYBlockchainDBStorage;
begin
   TLog.NewLog(ltInfo,ClassName,'SaveContractExecutionDataToDatabase');
   Result := False;

   // get database for saving contracts
   PathToDb := TFolderHelper.GetContractStorageFolder;

   ContractExecutionDatabase := TABEYBlockchainDBStorage.GetInstance('Execution', (PathToDB), ErrorNum);

   // check to see if instance exists

   if ErrorNum <> CT_SUCCESS then
   begin
      ErrorDesc := 'Error getting execution database for contracts, Error Number = ' + IntToStr(ErrorNum);
      errors := ErrorDesc;
      Exit;
   end;

   // be sure position in stream is set in the beginning
   ContractExecutionData.Seek(0,soBeginning);

   ErrorNum := ContractExecutionDatabase.InsertItem( IntToStr( ContractId ),ContractExecutionData ) ;
   if ErrorNum <> CT_SUCCESS then
   begin
       ErrorDesc := 'Error saving execution data for ContractId '+ IntToStr(ContractId) +' , Error Number = ' + IntToStr(ErrorNum);
       errors := ErrorDesc;
       Exit;
   end;

   ContractExecutionData.Seek(0,soBeginning);

   Result :=True;
end;





class function TSmartContractHelper.SaveContractExecutionDataToDatabase(ContractId:Cardinal;var ContractExecutionData:TRawBytes;var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean;  overload;
var
      PathToDb:String;
      ContractExecutionDatabase : TABEYBlockchainDBStorage;
begin
   TLog.NewLog(ltInfo,ClassName,'SaveContractExecutionDataToDatabase');
   Result := False;

   //get database for saving contracts
   PathToDb := TFolderHelper.GetContractStorageFolder;

   ContractExecutionDatabase := TABEYBlockchainDBStorage.GetInstance('Execution', (PathToDB), ErrorNum);

   // check to see if instance exists
   if ErrorNum <> CT_SUCCESS then
   begin
      ErrorDesc := 'Error getting execution database for contracts, Error Number = ' + IntToStr(ErrorNum);
      errors := ErrorDesc;
      Exit;
   end;

   ErrorNum := ContractExecutionDatabase.InsertItem( IntToStr( ContractId ),ContractExecutionData ) ;
   if ErrorNum <> CT_SUCCESS then
   begin
       ErrorDesc := 'Error saving execution data for ContractId '+ IntToStr(ContractId) +' , Error Number = ' + IntToStr(ErrorNum);
       errors := ErrorDesc;
       Exit;
   end;

   Result :=True;
end;


class function TSmartContractHelper.LoadContractInitializationDataFromDatabase(ContractId:Cardinal;var ContractInitializationData:TRawBytes;var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean;  overload;
var PathToDb:String;
    MemStream:TMemoryStream;
begin
  TLog.NewLog(ltInfo,ClassName,'LoadContractInitializationDataFromDatabase');

  Result := False;

  MemStream := TMemoryStream.Create;
  MemStream.Position := 0;

  if LoadContractInitializationDataFromDatabase(ContractId,MemStream,errors,ErrorNum,ErrorDesc) = False then Exit;


  SetLength(ContractInitializationData,MemStream.Size);

  MemStream.ReadBuffer(ContractInitializationData[0],Length(ContractInitializationData));

  Result:=True;

end;

class function TSmartContractHelper.LoadContractInitializationDataFromDatabase(ContractId:Cardinal;var ContractInitializationData:TMemoryStream;var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean;  overload;
var
      PathToDb:String;
      ContractInitializationDatabase : TABEYBlockchainDBStorage;
begin
  TLog.NewLog(ltInfo,ClassName,'LoadContractInitializationDataFromDatabase');
  Result := False;

   //get database for saving contracts
   PathToDb := TFolderHelper.GetContractStorageFolder;

   ContractInitializationDatabase := TABEYBlockchainDBStorage.GetInstance('Initialization', (PathToDB), ErrorNum);

   // check to see if instance exists

   if ErrorNum <> CT_SUCCESS then
   begin
      ErrorDesc := 'Error getting initialization database for contracts, Error Number = ' + IntToStr(ErrorNum);
      errors := ErrorDesc;
      Exit;
   end;

   ErrorNum := ContractInitializationDatabase.ItemExists( IntToStr( ContractId ) );
   if ( ErrorNum <> CT_ITEM_EXISTS_IN_TREE ) then
   begin
       ErrorDesc := 'Error getting contract data for ContractId '+ IntToStr(ContractId) +' , Error Number = ' + IntToStr(ErrorNum);
       errors := ErrorDesc;
       Exit;
   end;

   ContractInitializationData := TMemoryStream.Create;

   ErrorNum := ContractInitializationDatabase.FindItem( IntToStr( ContractId ) , ContractInitializationData);

   if ErrorNum <> CT_SUCCESS then begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Error getting contract data for ContractId '+ IntToStr(ContractId) +' , Error Number = ' + IntToStr(ErrorNum);
      errors := ErrorDesc;
      Exit;
   end;

   ContractInitializationData.Seek(0,soBeginning);

   Result :=True;
end;


class function TSmartContractHelper.LoadContractExecutionDataFromDatabase(ContractId:Cardinal;var ContractExecutionData:TRawBytes;var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean;  overload;
var PathToDb:String;
    MemStream:TMemoryStream;
begin
  TLog.NewLog(ltInfo,ClassName,'LoadContractExecutionDataFromDatabase');

  Result := False;

  MemStream := TMemoryStream.Create;
  MemStream.Position := 0;

  if LoadContractExecutionDataFromDatabase(ContractId,MemStream,errors,ErrorNum,ErrorDesc) = False then Exit;


  SetLength(ContractExecutionData,MemStream.Size);

  MemStream.ReadBuffer(ContractExecutionData[0],Length(ContractExecutionData));

  Result:=True;

end;

class function TSmartContractHelper.LoadContractExecutionDataFromDatabase(ContractId:Cardinal;var ContractExecutionData:TMemoryStream;var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean;  overload;
var
      PathToDb:String;
      ContractExecutionDatabase : TABEYBlockchainDBStorage;
begin
   TLog.NewLog(ltInfo,ClassName,'LoadContractExecutionDataFromDatabase');
   Result := False;

  //get database for saving contracts
   PathToDb := TFolderHelper.GetContractStorageFolder;

   ContractExecutionDatabase := TABEYBlockchainDBStorage.GetInstance('Execution', (PathToDB), ErrorNum);

   // check to see if instance exists

   if ErrorNum <> CT_SUCCESS then
   begin
      ErrorDesc := 'Error getting execution database for contracts, Error Number = ' + IntToStr(ErrorNum);
      errors := ErrorDesc;
      Exit;
   end;

   ErrorNum := ContractExecutionDatabase.ItemExists( IntToStr( ContractId ) );
   if ( ErrorNum <> CT_ITEM_EXISTS_IN_TREE ) then
   begin
       ErrorDesc := 'Error getting contract data for ContractId '+ IntToStr(ContractId) +' , Error Number = ' + IntToStr(ErrorNum);
       errors := ErrorDesc;
       Exit;
   end;

   ContractExecutionData := TMemoryStream.Create;

   ErrorNum := ContractExecutionDatabase.FindItem( IntToStr( ContractId ) , ContractExecutionData);

   if ErrorNum <> CT_SUCCESS then begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Error getting contract data for ContractId '+ IntToStr(ContractId) +' , Error Number = ' + IntToStr(ErrorNum);
      errors := ErrorDesc;
      Exit;
   end;

   ContractExecutionData.Seek(0,soBeginning);

   Result :=True;
end;


class function TSmartContractHelper.GetTempPath( FileName:String):String;
var
      Path:String;
      PathVariant: String;
begin
  TLog.NewLog(ltInfo,ClassName,'GetTempPath');
  Randomize;

  Path := TFolderHelper.GetContractTempFolder + PathDelim + FileName ;
  PathVariant  := Path + IntToStr(Random(10000));

  while FileExists(PathVariant) do // check to see if file exists on disk
  begin

    PathVariant  := Path + IntToStr(Random(10000));
  end;
  Result := PathVariant;
end;

class function TSmartContractHelper.GetTempFolderPath:String;
var
      Path:String;
      PathVariant: String;
begin
  TLog.NewLog(ltInfo,ClassName,'GetTempFolderPath');
  Randomize;

  Path := TFolderHelper.GetContractTempFolder + PathDelim + 'Temp' ;
  PathVariant  := Path + IntToStr(Random(10000));

  while DirectoryExists(PathVariant) do // check to see if folder exists on disk
  begin

  PathVariant  := Path + IntToStr(Random(10000));

  end;

  //crete dir with temp path
  if Not CreateDir(PathVariant) then
     begin
       TLog.NewLog(lterror,Classname, 'Error creating ' + PathVariant + ' folder');
     end;

  //return temporary folder path
  Result := PathVariant;
end;

class function TSmartContractHelper.ExecuteContract(ContractId:Cardinal;FunctionName:String;var Types : Array of String; var Values : Array of String;var TempDataSegmentPath : String;var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean;
var

      ContractInitializationData:TMemoryStream;
      ContractExecutionData:TMemoryStream;
      ExportTable,Bytecode,DataSegment:TRawBytes;

      ExportTableSize:Cardinal;
      BytecodeSize:Cardinal;
      DataSegmentSize:Cardinal;

      FolderPath:String;
      BytecodePath:String;
      ExportTablePath:String;
      DataSegmentPath:String;
begin
  TLog.NewLog(ltInfo,ClassName,'ExecuteContract');

  Result := False;

  if LoadContractInitializationDataFromDatabase(ContractId,ContractInitializationData,errors,ErrorNum,ErrorDesc) = False then Exit;

  if LoadContractExecutionDataFromDatabase(ContractId,ContractExecutionData,errors,ErrorNum,ErrorDesc) = False then Exit;




  ContractInitializationData.Seek( 8 * sizeof(Cardinal) , soBeginning); // jump to the 8'th position where the 8th metric of SaveContract is stored; -> dataSegSize

  BytecodeSize       := ContractInitializationData.ReadDWord;
  ExportTableSize    := ContractInitializationData.ReadDWord;
  DataSegmentSize    := ContractInitializationData.ReadDWord;



   //get exportTable + bytecode from InitializationDb -> where metrics + exportTable + bytecode are stored
  SetLength(Bytecode,bytecodeSize);
  ContractInitializationData.ReadBuffer(Bytecode[0],bytecodeSize);

  SetLength(ExportTable,exportTableSize);
  ContractInitializationData.ReadBuffer(ExportTable[0],exportTableSize);

  TLog.NewLog(ltInfo,ClassName,'BytecodeSize ' + IntToStr(BytecodeSize));
  TLog.NewLog(ltInfo,ClassName,'ExportTableSize ' + IntToStr(ExportTableSize));
  TLog.NewLog(ltInfo,ClassName,'Bytecode: ' + TCrypto.ToHexaString(Bytecode));
  TLog.NewLog(ltInfo,ClassName,'ExportTable: ' + TCrypto.ToHexaString(ExportTable));


  //get dataSegment from ExecutionDb -> where data segment(snapshot) is stored
  SetLength(DataSegment,DataSegmentSize);
  ContractExecutionData.ReadBuffer(DataSegment[0],DataSegmentSize);

  FolderPath := GetTempFolderPath;
  ExportTablePath := FolderPath + PathDelim + 'exportTable';
  BytecodePath := FolderPath + PathDelim + 'bytecode';
  DataSegmentPath :=  FolderPath + PathDelim + 'snapshot';




  //save to temporary files -> inside ABEY/Contracts/Temp
  if SaveBytecodeToFile(BytecodePath,Bytecode,errors,ErrorNum,ErrorDesc) = False then Exit;
  if SaveExportTableToFile(ExportTablePath,ExportTable,errors,ErrorNum,ErrorDesc) = False then Exit;
  if SaveDataSegmentToFile(DataSegmentPath,DataSegment,errors,ErrorNum,ErrorDesc) = False then Exit;



   if ModifyDataSegment(FunctionName,BytecodePath,ExportTablePath,DataSegmentPath,Types,Values,errors,ErrorNum,ErrorDesc) = False then Exit;


   TempDataSegmentPath := DataSegmentPath;

  Result := True;
end;

end.

