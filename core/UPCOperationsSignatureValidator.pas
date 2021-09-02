unit UPCOperationsSignatureValidator;

{
  This unit adds a TABEYOperationsSignatureValidator class that will check
  signature of operations in a multithread mode

  NOTE: This object is only a helper for fast processing speed when
  multithreading can help. There is no warranty that will validate all

  }


interface

{$I config.inc}

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

Uses Classes, UThread, UAccounts, UBlockChain,
  {$IFNDEF FPC}System.Generics.Collections{$ELSE}Generics.Collections{$ENDIF};

type
  TABEYOperationsSignatureValidator = Class;

  TABEYOperationsSignatureValidatorThread = Class(TABEYThread)
  private
    FValidator : TABEYOperationsSignatureValidator;
    //
  protected
    procedure BCExecute; override;
  public
    Constructor Create(AValidator : TABEYOperationsSignatureValidator);
  End;

  TABEYOperationsSignatureValidator = Class
  private
    FLock : TABEYCriticalSection;
    //
    FOperationsList : TList<TABEYOperation>;
    FLastIndexOperations : Integer;
    //
    FVaultTransaction : TABEYVaultTransaction;
    FValidatedOkCount : Integer;
    FValidatedErrorCount : Integer;
    FProgressNotify : TProgressNotify;
  protected
    function GetNextOperation(AValidatorThread : TABEYOperationsSignatureValidatorThread) : TABEYOperation;
    procedure SetOperationCheckResult(AValidatorThread : TABEYOperationsSignatureValidatorThread; APCOperation : TABEYOperation; AValidated : Boolean);
  public
    Constructor Create(AVaultTransaction : TABEYVaultTransaction; AProgressNotify : TProgressNotify);
    destructor Destroy; override;
    function Validate(AOperationsList : TList<TABEYOperation>) : Integer;
    class procedure MultiThreadPreValidateSignatures(AVaultTransaction : TABEYVaultTransaction; AOperationsHashTree : TOperationsHashTree; AProgressNotify : TProgressNotify); overload;
    class procedure MultiThreadPreValidateSignatures(AVaultTransaction : TABEYVaultTransaction; APCOperationsList : TList<TABEYOperation>; AProgressNotify : TProgressNotify); overload;
    class procedure MultiThreadPreValidateSignatures(AVaultTransaction : TABEYVaultTransaction; APCOperationsCompList: TList<TABEYOperationsComp>; AProgressNotify : TProgressNotify); overload;
  End;

implementation

Uses
  SysUtils,
  ULog, UBaseTypes,
  UCommon;

var _Cpus : Integer = 0;

{ TABEYOperationsSignatureValidator }

constructor TABEYOperationsSignatureValidator.Create(AVaultTransaction: TABEYVaultTransaction; AProgressNotify : TProgressNotify);
begin
  FVaultTransaction := AVaultTransaction;
  FLastIndexOperations := -1;
  FLock := TABEYCriticalSection.Create('');
  FValidatedOkCount := 0;
  FValidatedErrorCount := 0;
  FProgressNotify := AProgressNotify;
end;

destructor TABEYOperationsSignatureValidator.Destroy;
begin
  FreeAndNil(FLock);
  inherited;
end;

function TABEYOperationsSignatureValidator.GetNextOperation(AValidatorThread : TABEYOperationsSignatureValidatorThread) : TABEYOperation;
var LIndex : Integer;
begin
  FLock.Acquire;
  try
    // Search new
    LIndex := FLastIndexOperations + 1; // Move to next
    if (LIndex<FOperationsList.Count) then begin
      Result := FOperationsList[LIndex];
      FLastIndexOperations := Lindex;
    end else Result := Nil;
  finally
    FLock.Release;
  end;
end;

class procedure TABEYOperationsSignatureValidator.MultiThreadPreValidateSignatures(AVaultTransaction: TABEYVaultTransaction;
  APCOperationsList: TList<TABEYOperation>; AProgressNotify: TProgressNotify);
var
  i : Integer;
  LMultiThreadValidator : TABEYOperationsSignatureValidator;
  LValidatedOk, LValidatedError, LValidatedTotal : Integer;
  LTC : TTickCount;
begin
  if _Cpus<=0 then begin
    _Cpus := TLogicalCPUCount.GetLogicalCPUCount;
  end;
  if _Cpus<=1 then Exit;

    LTC := TPlatform.GetTickCount;
    LMultiThreadValidator := TABEYOperationsSignatureValidator.Create(AVaultTransaction,AProgressNotify);
    try
      LValidatedTotal := LMultiThreadValidator.Validate(APCOperationsList);
      LValidatedOk := LMultiThreadValidator.FValidatedOkCount;
      LValidatedError := LMultiThreadValidator.FValidatedErrorCount;
      LTC := TPlatform.GetElapsedMilliseconds(LTC);
      if (LValidatedTotal>0) and (LTC>0) and ((LValidatedOk>0) or (LValidatedError>0))  then begin
        TLog.NewLog(ltdebug,ClassName,Format('Validated %d operations with %d signatures Ok and %d signatures Error in %d miliseconds avg %.2f op/sec',[LValidatedTotal,LValidatedOk,LValidatedError,LTC,LValidatedTotal*1000/LTC]));
      end;
    finally
      LMultiThreadValidator.Free;
    end;

end;

class procedure TABEYOperationsSignatureValidator.MultiThreadPreValidateSignatures(
  AVaultTransaction: TABEYVaultTransaction; APCOperationsCompList: TList<TABEYOperationsComp>; AProgressNotify : TProgressNotify);
var LList : TList<TABEYOperation>;
  i : Integer;
  LMultiThreadValidator : TABEYOperationsSignatureValidator;
  LValidatedOk, LValidatedError, LValidatedTotal : Integer;
  LTC : TTickCount;
begin
  if _Cpus<=0 then begin
    _Cpus := TLogicalCPUCount.GetLogicalCPUCount;
  end;
  if _Cpus<=1 then Exit;

  LList := TList<TABEYOperation>.Create;
  Try
    for i := 0 to APCOperationsCompList.Count-1 do begin
      APCOperationsCompList[i].OperationsHashTree.GetOperationsList(LList,True);
    end;
    LTC := TPlatform.GetTickCount;
    LMultiThreadValidator := TABEYOperationsSignatureValidator.Create(AVaultTransaction,AProgressNotify);
    try
      LValidatedTotal := LMultiThreadValidator.Validate(LList);
      LValidatedOk := LMultiThreadValidator.FValidatedOkCount;
      LValidatedError := LMultiThreadValidator.FValidatedErrorCount;
      LTC := TPlatform.GetElapsedMilliseconds(LTC);
      if (LValidatedTotal>0) and (LTC>0) and ((LValidatedOk>0) or (LValidatedError>0))  then begin
        TLog.NewLog(ltdebug,ClassName,Format('Validated %d operations from %d Blocks with %d signatures Ok and %d signatures Error in %d miliseconds avg %.2f op/sec',[LValidatedTotal,APCOperationsCompList.Count,LValidatedOk,LValidatedError,LTC,LValidatedTotal*1000/LTC]));
      end;
    finally
      LMultiThreadValidator.Free;
    end;

  Finally
    LList.Free;
  End;

end;


class procedure TABEYOperationsSignatureValidator.MultiThreadPreValidateSignatures(
  AVaultTransaction: TABEYVaultTransaction; AOperationsHashTree: TOperationsHashTree; AProgressNotify : TProgressNotify);
var LMultiThreadValidator : TABEYOperationsSignatureValidator;
  LValidatedOk, LValidatedError, LValidatedTotal : Integer;
  LTC : TTickCount;
  LList : TList<TABEYOperation>;
begin
  if _Cpus<=0 then begin
    _Cpus := TLogicalCPUCount.GetLogicalCPUCount;
  end;
  if _Cpus<=1 then Exit;
  if AOperationsHashTree.OperationsCount<_Cpus then Exit;   // If less than cpus, no need for multithreading...

  LTC := TPlatform.GetTickCount;
  LMultiThreadValidator := TABEYOperationsSignatureValidator.Create(AVaultTransaction,AProgressNotify);
  try
    LList := TList<TABEYOperation>.Create;
    Try
      AOperationsHashTree.GetOperationsList(Llist,True);
      if LList.Count<_Cpus then Exit; // No need for multithreading...

      LValidatedTotal := LMultiThreadValidator.Validate(LList);
      LValidatedOk := LMultiThreadValidator.FValidatedOkCount;
      LValidatedError := LMultiThreadValidator.FValidatedErrorCount;
      LTC := TPlatform.GetElapsedMilliseconds(LTC);
      if (LValidatedTotal>0) and (LTC>0) and ((LValidatedOk>0) or (LValidatedError>0))  then begin
        TLog.NewLog(ltdebug,ClassName,Format('Validated %d operations with %d signatures Ok and %d signatures Error in %d miliseconds avg %.2f op/sec',[LValidatedTotal,LValidatedOk,LValidatedError,LTC,LValidatedTotal*1000/LTC]));
      end;
    Finally
      LList.Free;
    End;
  finally
    LMultiThreadValidator.Free;
  end;
end;

procedure TABEYOperationsSignatureValidator.SetOperationCheckResult(
  AValidatorThread: TABEYOperationsSignatureValidatorThread;
  APCOperation: TABEYOperation; AValidated: Boolean);
begin
  FLock.Acquire;
  try
    if AValidated then inc(FValidatedOkCount)
    else inc(FValidatedErrorCount);
  finally
    FLock.Release;
  end;
end;

function TABEYOperationsSignatureValidator.Validate(AOperationsList : TList<TABEYOperation>) : Integer;
var LLastTC : TTickCount;
  procedure DoNotify;
  var LMsg : String;
    LCurPos,LTotal : Int64;
  begin
    if (Assigned(FProgressNotify)) and (TPlatform.GetElapsedMilliseconds(LLastTC)>200) then begin
      LLastTC := TPlatform.GetTickCount;
      FLock.Acquire;
      try
        LMsg := Format('Validating signatures %d/%d',[FLastIndexOperations,FOperationsList.Count]);
        LCurPos := FLastIndexOperations;
        LTotal := FOperationsList.Count;
      finally
        FLock.Release;
      end;
      //
      FProgressNotify(Self,LMsg,LCurPos,LTotal);
    end;
  end;
var LMaxThreads : Integer;
  LThreads : TList<TABEYOperationsSignatureValidatorThread>;
  i,LTerminatedThreads : Integer;
begin
  FValidatedOkCount := 0;
  FValidatedErrorCount := 0;
  if AOperationsList.Count<=0 then Exit(0);

  FLastIndexOperations := -1;

  if _Cpus<=0 then begin
    _Cpus := TLogicalCPUCount.GetLogicalCPUCount;
  end;
  if (_Cpus>2) then LMaxThreads := _Cpus-1
  else LMaxThreads := _Cpus;
  if (LMaxThreads<=0) then LMaxThreads := 1;
  if (LMaxThreads>7) then LMaxThreads := 7;

  LThreads := TList<TABEYOperationsSignatureValidatorThread>.Create;
  Try
    // Init values
    FLastIndexOperations := -1;
    FOperationsList := AOperationsList;

    // Step 1: Create the threads:
    for i := 1 to LMaxThreads do begin
      LThreads.Add( TABEYOperationsSignatureValidatorThread.Create(Self) );
    end;
    // Step 2: Start the threads
    for i := 0 to LThreads.Count-1 do begin
      LThreads[i].Suspended := False;
    end;
    // Step 3: Wait until error of finalized
    LLastTC := TPlatform.GetTickCount;
    repeat
      DoNotify;
      LTerminatedThreads := 0;
      for i := 0 to LThreads.Count-1 do begin
        if LThreads[i].Terminated then inc(LTerminatedThreads);
      end;
      Sleep(1);
    until (LTerminatedThreads>=LThreads.Count);
  Finally
    for i := 0 to LThreads.Count-1 do begin
      LThreads[i].Terminate;
      LThreads[i].WaitFor;
      LThreads[i].Free;
    end;
    LThreads.Free;
  End;
  Result := FOperationsList.Count;
end;

{ TABEYOperationsSignatureValidatorThread }

procedure TABEYOperationsSignatureValidatorThread.BCExecute;
var LOperation : TABEYOperation;
    LIsValid : Boolean;
begin
  repeat
    LOperation := FValidator.GetNextOperation(Self);
    if Assigned(LOperation) then begin
      if Not LOperation.HasValidSignature then begin
        // Only will validate if HasValidSignature is False (Not validated before)
        try
          LIsValid := LOperation.IsValidSignatureBasedOnCurrentVaultState(FValidator.FVaultTransaction);
        except
          LIsValid := False;
        end;
        FValidator.SetOperationCheckResult(Self,LOperation, LIsValid);
      end;
    end;
  until (Not Assigned(LOperation)) or (Terminated);
end;

constructor TABEYOperationsSignatureValidatorThread.Create(AValidator: TABEYOperationsSignatureValidator);
begin
  FValidator := AValidator;
  inherited Create(True);
  FreeOnTerminate := False;
end;

end.
