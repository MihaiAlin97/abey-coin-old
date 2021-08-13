unit UVirtualMachine;

{$mode delphi}

interface

uses
  Classes, SysUtils,UJSONFunctions,UAccounts,UbaseTypes,UPCDataTypes,UCrypto;

type

  TOpSaveContractData = Record
        account_signer,                  //account that signs the operation of saving
        account_target: Cardinal;        //account that gets associated with contract
        locked_until_block : Cardinal;  //
        n_operation : Cardinal;        //number of ops. executed by
        fee: UInt64;
        starting_block_number_flag: Boolean; //Boolean flag for (if 1 its true, otherwise any non-zero value is false)
        starting_block_number: Cardinal; //the block when the contract is going to be active     (0 when block is mined, +N blocks or at block M) its N or M given flag
        active_blocks: Cardinal;       //
        passive_blocks: Cardinal;
        max_instruction_count: Cardinal;
        max_memory_usage_kb: Cardinal;
        max_runtime_seconds: Cardinal;
        max_io_usage: Cardinal;               //max number of IO operations interaction with DB, Blockchain or may files
        max_data_segment_size_kb: Cardinal;   //not max size, actual size of global and static variables in bytes
        export_table_size: Cardinal;
        bytecode_size: Cardinal;
        export_table: TRawBytes;
        payload: TRawBytes;
        data_segment : TRawBytes;
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
        initial_data_segment : TRawBytes;       //this is data_segment from TOpSaveContractData
        data_segment : TRawBytes;               //this is the data_segment from TOpExecuteContractData
        bytecode: TRawBytes;                    //this is not stored in TOpSaveContractData or TOpExecuteContractData
        function_name : String;                 //TOpExecuteContractData
        parameter_types : array of String;      //TOpExecuteContractData
        parameter_values : array of String;     //TOpExecuteContractData
        bytecodePath:String;
        exportTablePath:String;
        dataSegmentPath:String;
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
    public
      class function SaveContractCheckParameters (var params:TABEYJSONObject;var errors:String;var ErrorNum:Integer; var ErrorDesc: String) : Boolean;     // verifies params inside JSON-RPC call are ok
      class function SaveContractTryGetFDataFromParams ( var params:TABEYJSONObject; var Data: TOpSaveContractData; AccountsCount: Cardinal ; BlocksCount: Cardinal; var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean;
      class function SaveContractFillPayload (BytecodePath:String;ExportTablePath:String;var Data: TOpSaveContractData;var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean;
      class function CreateOperationSaveContract(current_protocol:Word;privateKey:TECPrivateKey;n_operation:Cardinal;AccountsCount:Cardinal;BlocksCount:Cardinal;var params:TABEYJSONObject;var SaveContract : TObject;var errors:String;var ErrorNum:Integer; var ErrorDesc: String) : Boolean;


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


      class function InitDataSegment(BytecodePath:String ; ExportTablePath:String;DataSegmentPath:String;var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean;
      //class function RemoveDataSegment(BytecodePath:String):Boolean;

      class procedure PrintSaveContractOperationData(Data : UVIRTUALMACHINE.TOpSaveContractData);
      class procedure PrintExecuteContractOperationData(Data : UVIRTUALMACHINE.TOpExecuteContractData);



      {class function LoadContractDataFromDatabase(ContractId:Cardinal;var ContractData:TSmartContractData;var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean;  overload;
      class function TOpExecuteContractDataFromTSmartContractData
      class function TSmartContractDataTo   }
      class function LoadContractInitializationDataFromDatabase(ContractId:Cardinal;var ContractInitializationData:TMemoryStream;var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean;  overload;
      class function LoadContractExecutionDataFromDatabase(ContractId:Cardinal;var ContractExecutionData:TMemoryStream;var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean;  overload;
      //class function LoadContractInitializationDataFromDatabase(ContractId:Cardinal;var ContractData:TRawBytes;var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean;  overload;

      
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
     max_instruction_count:0;max_memory_usage_kb:0;max_runtime_seconds:0;max_io_usage:0;max_data_segment_size_kb:0;export_table_size:0;
     bytecode_size:0;export_table:Nil;payload:Nil;data_segment:Nil;public_key:(EC_OpenSSL_NID:0;x:Nil;y:Nil);sign:(r:Nil;s:Nil));

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
    Result := False;

    if params.IndexOfName('account_signer')<0 then begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      ErrorDesc := 'Need "account_signer" param';
      Errors := ErrorDesc;
      exit;
    end;

    if params.IndexOfName('account_target')<0 then begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      ErrorDesc := 'Need "account_target" param';
      Errors := ErrorDesc;
      exit;
    end;

    if params.IndexOfName('starting_block_number_flag')<0 then begin
      ErrorNum :=  CT_RPC_ErrNum_FlagForStartingBlock;
      ErrorDesc := 'Need "starting_block_number_flag" param';
      Errors := ErrorDesc;
      exit;
    end;

    if params.IndexOfName('starting_block_number')<0 then begin
      ErrorNum :=  CT_RPC_ErrNum_ActiveBlocks;
      ErrorDesc := 'Need "starting_block_number" param';
      Errors := ErrorDesc;
     exit;
    end;

    if params.IndexOfName('active_blocks')<0 then begin
      ErrorNum :=  CT_RPC_ErrNum_ActiveBlocks;
      ErrorDesc := 'Need "active_blocks" param';
      Errors := ErrorDesc;
     exit;
    end;

    if params.IndexOfName('passive_blocks')<0 then begin
      ErrorNum :=  CT_RPC_ErrNum_PassiveBlocks;
      ErrorDesc := 'Need "passive_blocks" param';
      Errors := ErrorDesc;
      exit;
    end;

    if params.IndexOfName('max_instruction_count')<0 then begin
      ErrorNum :=  CT_RPC_ErrNum_MaxInstructionCount;
      ErrorDesc := 'Need "max_instruction_count" param';
      Errors := ErrorDesc;
      exit;
    end;

    if params.IndexOfName('max_memory_usage_kb')<0 then begin
      ErrorNum :=  CT_RPC_ErrNum_MaxMemoryUsage;
      ErrorDesc := 'Need "max_memory_usage_kb" param';
      Errors := ErrorDesc;
      exit;
    end;

    if params.IndexOfName('max_runtime_seconds')<0 then begin
      ErrorNum :=  CT_RPC_ErrNum_MaxRuntimeSeconds;
      ErrorDesc := 'Need "max_runtime_seconds" param';
      Errors := ErrorDesc;
      exit;
    end;

    if params.IndexOfName('max_io_usage')<0 then begin
      ErrorNum :=  CT_RPC_ErrNum_MaxIoUsage;
      ErrorDesc := 'Need "max_io_usage" param';
      Errors := ErrorDesc;
      exit;
    end;

    if params.IndexOfName('max_data_segment_size_kb')<0 then begin
      ErrorNum :=  CT_RPC_ErrNum_MaxDataSegmentSize;
      ErrorDesc := 'Need "max_data_segment_size_kb" param';
      Errors := ErrorDesc;
      exit;
    end;

    if params.IndexOfName('export_table_size')<0 then begin
      ErrorNum :=  CT_RPC_ErrNum_ExportTableSize;
      ErrorDesc := 'Need "export_table_size" param';
      Errors := ErrorDesc;
      exit;
    end;

    if params.IndexOfName('bytecode_size')<0 then begin
      ErrorNum :=  CT_RPC_ErrNum_BytecodeSize;
      ErrorDesc := 'Need "bytecode_size" param';
      Errors := ErrorDesc;
      exit;
    end;

    if params.IndexOfName('export_table_path')<0 then begin
      ErrorNum := CT_RPC_ErrNum_ExportTableFilePath;
      ErrorDesc := 'Need "export_table" file path param';
      Errors := ErrorDesc;
      exit;
    end;

    if params.IndexOfName('bytecode_path')<0 then begin
      ErrorNum := CT_RPC_ErrNum_PayloadFilePath;
      ErrorDesc := 'Need "bytecode_path" file path param';
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

    Result := False;

    Data := CT_TOpSaveContractData_NUL ;

    Data.fee := ToABEYs(params.AsDouble('fee',0));

    if (Data.fee<0) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Invalid fee value';
      Exit;
    end;

    Data.account_signer := params.AsCardinal('account_signer',CT_MaxAccount) ;

    if (Data.account_signer<0) or (Data.account_signer >= AccountsCount ) then begin
        ErrorDesc:='Invalid account '+Inttostr(Data.account_signer);
        ErrorNum:= CT_RPC_ErrNum_InvalidAccount;
        Errors := ErrorDesc;
        Exit;
    end;

    Data.account_target :=  params.AsCardinal('account_target',CT_MaxAccount);

    if (Data.account_target<0) or (Data.account_target >= AccountsCount ) then begin
        ErrorDesc:='Invalid account '+Inttostr(Data.account_target);
        ErrorNum:= CT_RPC_ErrNum_InvalidAccount;
        Errors := ErrorDesc;
        Exit;
    end;


    Data.starting_block_number_flag := params.AsBoolean('starting_block_number_flag', false);

    Data.starting_block_number := params.AsCardinal('starting_block_number', CT_MaxBlock );

    if (Data.starting_block_number < 0) or (Data.starting_block_number >= BlocksCount) then begin
        ErrorDesc:='Invalid starting_block_number ' + Inttostr(Data.starting_block_number);
        ErrorNum:= CT_RPC_ErrNum_InvalidAccount;
        Errors := ErrorDesc;
        Exit;
    end;

    Data.active_blocks := params.AsCardinal('active_blocks', 0);

    if (Data.active_blocks < 0) or (Data.active_blocks >= BlocksCount) then begin
        ErrorDesc:='Invalid active_blocks ' + Inttostr(Data.active_blocks);
        ErrorNum:= CT_RPC_ErrNum_InvalidAccount;
        Errors := ErrorDesc;
        Exit;
    end;

    Data.passive_blocks := params.AsCardinal('passive_blocks', 0);

    if (Data.passive_blocks < 0) or (Data.passive_blocks >= BlocksCount) then begin
        ErrorDesc:='Invalid passive_blocks ' + Inttostr(Data.passive_blocks);
        ErrorNum:= CT_RPC_ErrNum_InvalidAccount;
        Errors := ErrorDesc;
        Exit;
    end;


    Data.max_instruction_count := params.AsCardinal('max_instruction_count', CT_MaxCardinalValue);

    if (Data.max_instruction_count < 0) or (Data.max_instruction_count >= CT_MaxCardinalValue) then begin
        ErrorDesc:='Invalid max_instruction_count ' + Inttostr(Data.max_instruction_count);
        ErrorNum:= CT_RPC_ErrNum_InvalidAccount;
        Errors := ErrorDesc;
        Exit;
    end;


    Data.max_memory_usage_kb := params.AsCardinal('max_memory_usage_kb', CT_MaxCardinalValue);

    if (Data.max_memory_usage_kb < 0) or (Data.max_memory_usage_kb >= CT_MaxCardinalValue) then begin
        ErrorDesc:='Invalid max_memory_usage_kb ' + Inttostr(Data.max_memory_usage_kb);
        ErrorNum:= CT_RPC_ErrNum_InvalidAccount;
        Errors := ErrorDesc;
        Exit;
    end;


    Data.max_runtime_seconds := params.AsCardinal('max_runtime_seconds', CT_MaxCardinalValue);

    if (Data.max_runtime_seconds < 0) or (Data.max_runtime_seconds >= CT_MaxCardinalValue) then begin
        ErrorDesc:='Invalid max_runtime_seconds ' + Inttostr(Data.max_runtime_seconds);
        ErrorNum:= CT_RPC_ErrNum_InvalidAccount;
        Errors := ErrorDesc;
        Exit;
    end;

    Data.max_io_usage := params.AsCardinal('max_io_usage', CT_MaxCardinalValue);

    if (Data.max_io_usage < 0) or (Data.max_io_usage >= CT_MaxCardinalValue) then begin
        ErrorDesc:='Invalid max_io_usage ' + Inttostr(Data.max_io_usage);
        ErrorNum:= CT_RPC_ErrNum_InvalidAccount;
        Errors := ErrorDesc;
        Exit;
    end;

    Data.max_data_segment_size_kb := params.AsCardinal('max_data_segment_size_kb', CT_MaxCardinalValue);

    if (Data.max_data_segment_size_kb < 0) or (Data.max_data_segment_size_kb >= CT_MaxCardinalValue) then begin
        ErrorDesc:='Invalid max_data_segment_size_kb ' + Inttostr(Data.max_data_segment_size_kb);
        ErrorNum:= CT_RPC_ErrNum_InvalidAccount;
        Errors := ErrorDesc;
        Exit;
    end;

    Data.export_table_size := params.AsCardinal('export_table_size', CT_MaxCardinalValue);
    if (Data.export_table_size < 0) or (Data.export_table_size >= CT_MaxCardinalValue) then begin
        ErrorDesc:='Invalid export_table_size ' + Inttostr(Data.export_table_size);
        ErrorNum:= CT_RPC_ErrNum_InvalidAccount;
        Errors := ErrorDesc;
        Exit;
    end;

    Data.bytecode_size := params.AsCardinal('bytecode_size', CT_MaxCardinalValue);
    if (Data.bytecode_size < 0) or (Data.bytecode_size >= CT_MaxCardinalValue) then begin
        ErrorDesc:='Invalid bytecode_size ' + Inttostr(Data.bytecode_size);
        ErrorNum:= CT_RPC_ErrNum_InvalidAccount;
        Errors := ErrorDesc;
        Exit;
    end;


    ExportTablePath := params.AsString('export_table_path','');

    BytecodePath := params.AsString('bytecode_path','');
    //check file paths here as we can prevent the error here
    if (Not(FileExists(ExportTablePath))) then begin
       ErrorNum := CT_RPC_ErrNum_ExportTableFilePath;
       ErrorDesc := 'Invalid path to export table file';
       Errors := ErrorDesc;
       Exit;
    end;

    if (Not(FileExists(BytecodePath))) then begin
      ErrorNum := CT_RPC_ErrNum_PayloadFilePath;
      ErrorDesc := 'Invalid path to bytecode file';
      Errors := ErrorDesc;
      Exit;
    end;

    DebugLn('Entering FillPayload');

    // fill payload
    if SaveContractFillPayload(BytecodePath,ExportTablePath,Data,errors,ErrorNum,ErrorDesc) = False then Exit;

    Result:=True;

end;

class function TSmartContractHelper.SaveContractFillPayload (BytecodePath:String;ExportTablePath:String;var Data: TOpSaveContractData;var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean;
var MemStream:TMemoryStream;
    DataSegmentPath:String;
    Bytecode:TRawBytes;
    ExportTable:TrawBytes;
    DataSegment:TRawBytes;
    flags:Cardinal = Cardinal(0);
begin
     Result := False;



     DataSegmentPath := TFolderHelper.GetABEYDataFolder() + PathDelim + 'out' + PathDelim + 'data_segment.bin';

     DebugLn('Initiating Data Segment');

     if InitDataSegment(BytecodePath,ExportTablePath,DataSegmentPath,errors,ErrorNum,ErrorDesc) = False then Exit; // generate data segment by executing initVM function

     DebugLn('Loading bytecode from file');

     if (LoadBytecodeFromFile(BytecodePath,Bytecode,errors,ErrorNum,ErrorDesc) = False ) then begin
      Exit;
     end;


     DebugLn('Loading exportTable from file');

     if (LoadExportTableFromFile(ExportTablePath,ExportTable,errors,ErrorNum,ErrorDesc) = False ) then begin
      Exit;
     end;

     DebugLn('Loading data segment from file');

     if (LoadDataSegmentFromFile(DataSegmentPath,DataSegment,errors,ErrorNum,ErrorDesc) = False ) then begin
      DeleteFile(DataSegmentPath);
      Exit;
     end;


     DeleteFile(DataSegmentPath);

     MemStream := TMemoryStream.Create;

     MemStream.Position := 0;

     // we store only the params and exportTable + bytecode inside payload; data segment is set in FData.data_segment
     SetLength(Data.payload, sizeof(Cardinal) * 11 );

     DebugLn('Setting payload length');

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
     MemStream.WriteDWord(Data.max_data_segment_size_kb);
     MemStream.WriteDWord(Data.export_table_size);
     MemStream.WriteDWord(Data.bytecode_size);

     MemStream.Position := 0;

     DebugLn('Pos ' + IntToStr(MemStream.Position));
     //move memorystream content to payload field


     MemStream.ReadBuffer(Data.payload[0],Length(Data.payload));

     DebugLn('Wrote payload');

     Data.export_table.Add(ExportTable);
     Data.data_segment.Add(DataSegment);



     Data.payload.Add(ExportTable);
     Data.payload.Add(Bytecode);
     Data.payload.Add(DataSegment);

     DebugLn('Wrote payload + exportTable and Bytecode');

     Result := True;
end;


class function TSmartContractHelper.CreateOperationSaveContract(current_protocol:Word;privateKey:TECPrivateKey;n_operation:Cardinal;AccountsCount:Cardinal;BlocksCount:Cardinal;var params:TABEYJSONObject;var SaveContract : TObject;var errors:String;var ErrorNum:Integer; var ErrorDesc: String) : Boolean;
      var Data:TOpSaveContractData;
begin

     Result := False;

     DebugLn('Entered Helper class');

     //validate JSON params
     if SaveContractCheckParameters(params,errors,ErrorNum,ErrorDesc) = False then Exit;

     DebugLn('Passed Check Parameters');

     // Fill Data with params ; except data_segment and payload
     if SaveContractTryGetFDataFromParams(params,Data,AccountsCount,BlocksCount,errors,ErrorNum,ErrorDesc) = False then Exit;

     DebugLn('Passed TryGetFData');



     TOPSaveContract(SaveContract) := TOpSaveContract.CreateSaveContract(
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
        Data.max_data_segment_size_kb,
        Data.export_table_size,
        Data.bytecode_size,
        Data.export_table,
        Data.payload,
        Data.data_segment);

     Result := True;

end;

class function TSmartContractHelper.LoadBytecodeFromFile(BytecodePath:String ; var Content : TMemoryStream ; var errors:String;var ErrorNum:Integer; var ErrorDesc: String) : Boolean ; overload;
begin
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
    Result := False;

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
    Result := False;

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
    Result := False ;
    Content := TMemoryStream.Create;

    try
      Content.LoadFromFile(DataSegmentpath);
    except
    on E:Exception do
       begin
          ErrorNum := CT_RPC_ErrNum_PayloadFilePath;
          ErrorDesc := 'Error loading data segment from file'+E.Message;;
          Errors := ErrorDesc;
          Exit;
       end;
    end;

    Result := True;

end;

class function TSmartContractHelper.LoadDataSegmentFromFile(DataSegmentpath:String ; var Content : TRawBytes ; var errors:String;var ErrorNum:Integer; var ErrorDesc: String) : Boolean ; overload;
var Ms:TMemoryStream;
begin
    Result := False;

    if LoadDataSegmentFromFile(DataSegmentpath,Ms,errors,ErrorNum,ErrorDesc) = True then
    begin
      ms.Position:=0;
      SetLength(Content,Ms.Size);
      Ms.ReadBuffer(Content[Low(Content)],Ms.Size);
      Result := True;
    end;

end;

class function TSmartContractHelper.InitDataSegment(BytecodePath:String ; ExportTablePath:String;DataSegmentPath:String;var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean;
var
      VirtualMachine : TProcess;
      PathToVm:String;
      InitVmLim:Integer=10000;
      VmOutput:String;
      BytesRead    : longint;
      Buffer       : array[1..2048] of byte;
      OutputStream : TStream;
begin
    Result := False;
    {$IFDEF WINDOWS}
    PathToVm := GetCurrentDir + PathDelim + 'q3vm.exe';
    {$ELSE}  //Linux + Mac
    PathToVm := GetCurrentDir + PathDelim + 'q3vm';
    //do error checking, lacking to q3vm
    {$ENDIF WINDOWS}

    DebugLn('vm path ' + PathToVm) ;
    DebugLn('DataSeg path' + DataSegmentPath);
    VirtualMachine := TProcess.Create(nil);
    VirtualMachine.Executable := PathToVm;



    VirtualMachine.Parameters.Add('--bytecodePath ');
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


class procedure TSmartContractHelper.PrintSaveContractOperationData(Data : TOpSaveContractData);
begin

  TLog.NewLog(ltInfo,Classname,'account_signer ' + IntToStr(Data.account_signer));
  TLog.NewLog(ltInfo,Classname,'account_target ' + IntToStr(Data.account_target));
  TLog.NewLog(ltInfo,Classname,'locked_until_block ' + IntToStr(Data.locked_until_block));
  TLog.NewLog(ltInfo,Classname,'n_operation ' + IntToStr(Data.n_operation));
  TLog.NewLog(ltInfo,Classname,'fee ' + IntToStr(Data.fee));
  TLog.NewLog(ltInfo,Classname,'starting_block_number_flag ' + BoolToStr(Data.starting_block_number_flag));
  TLog.NewLog(ltInfo,Classname,'starting_block_number ' + IntToStr(Data.starting_block_number));
  TLog.NewLog(ltInfo,Classname,'active_blocks ' + IntToStr(Data.active_blocks));
  TLog.NewLog(ltInfo,Classname,'passive_blocks ' + IntToStr(Data.passive_blocks));
  TLog.NewLog(ltInfo,Classname,'max_instruction_count ' + IntToStr(Data.max_instruction_count));
  TLog.NewLog(ltInfo,Classname,'max_memory_usage_kb ' + IntToStr(Data.max_memory_usage_kb));
  TLog.NewLog(ltInfo,Classname,'max_runtime_seconds ' + IntToStr(Data.max_runtime_seconds));
  TLog.NewLog(ltInfo,Classname,'max_io_usage ' + IntToStr(Data.max_io_usage));
  TLog.NewLog(ltInfo,Classname,'max_data_segment_size_kb ' + IntToStr(Data.max_data_segment_size_kb));
  TLog.NewLog(ltInfo,Classname,'export_table_size ' + IntToStr(Data.export_table_size));
  TLog.NewLog(ltInfo,Classname,'bytecode_size ' + IntToStr(Data.bytecode_size));
  TLog.NewLog(ltInfo,Classname,'export_table ' + TCrypto.ToHexaString(Data.export_table));
  TLog.NewLog(ltInfo,Classname,'payload ' + TCrypto.ToHexaString(Data.payload));
  TLog.NewLog(ltInfo,Classname,'data_segment ' + TCrypto.ToHexaString(Data.data_segment));
  TLog.NewLog(ltInfo,Classname,'data_segment ' + TCrypto.ToHexaString(Data.data_segment));

end;

class procedure TSmartContractHelper.PrintExecuteContractOperationData(Data : UVIRTUALMACHINE.TOpExecuteContractData);
begin
   TLog.NewLog(ltInfo,Classname,'account_signer ' + IntToStr(Data.account_signer));
   TLog.NewLog(ltInfo,Classname,'n_operation ' + IntToStr(Data.n_operation));
   TLog.NewLog(ltInfo,Classname,'fee ' + IntToStr(Data.fee));
   TLog.NewLog(ltInfo,Classname,'function_name ' + Data.function_name);
end;

class function TSmartContractHelper.LoadContractInitializationDataFromDatabase(ContractId:Cardinal;var ContractInitializationData:TMemoryStream;var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean;  overload;
var
      PathToDb:String;
      ContractInitializationDatabase : TABEYBlockchainDBStorage;
begin
  Result := False;

  //get database for saving contracts
   PathToDb := TFolderHelper.GetABEYDataFolder() + PathDelim + 'StorageDB';

   ContractInitializationDatabase := TABEYBlockchainDBStorage.GetInstance('contracts', (PathToDB), ErrorNum);

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

   ContractInitializationData.Seek(0,soFromBeginning);

   Result :=True;
end;

class function TSmartContractHelper.LoadContractExecutionDataFromDatabase(ContractId:Cardinal;var ContractExecutionData:TMemoryStream;var errors:String;var ErrorNum:Integer; var ErrorDesc: String):Boolean;  overload;
var
      PathToDb:String;
      ContractExecutionDatabase : TABEYBlockchainDBStorage;
begin
  Result := False;

  //get database for saving contracts
   PathToDb := TFolderHelper.GetABEYDataFolder() + PathDelim + 'StorageDBExecuteContract';

   ContractExecutionDatabase := TABEYBlockchainDBStorage.GetInstance('DataSegment', (PathToDB), ErrorNum);

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

   ContractExecutionData.Seek(0,soFromBeginning);

   Result :=True;
end;

end.

