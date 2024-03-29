unit UServerApp;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$IFDEF MSWINDOWS}
  {$DEFINE OS_MSWIN}
{$ENDIF}

interface

uses
  {$IFDEF LCL}
  interfaces,
  {$ENDIF}
  {$IFDEF OS_MSWIN}
  Windows,
  Messages,
  {$ENDIF}
  SyncObjs,
  UOpenSSL, UCrypto, UNode, UFileStorage, UFolderHelper, UWalletKeys, UConst, ULog, UNetProtocol,
  URPC;

type
  TABEYServerLogType = (sltDebug, sltInfo, sltError, sltWarning);

  TABEYServerAppLogEvent = procedure (LogType: TABEYServerLogType;
      Msg: String; Level: Integer) of object;

  { TABEYServerApp }

  TABEYServerApp = class
  private
    FLock : TCriticalSection;
    FOnLog: TABEYServerAppLogEvent;
    FTerminated : Boolean;
    {$IFDEF OS_MSWIN}
    hStdIn : THandle;
    {$ENDIF}
    FNode : TNode;
    FWalletKeys : TWalletKeysExt;
    FRPC : TRPCServer;
    FLog : TLog;

    procedure Lock;
    procedure Unlock;

    procedure Log(const LogType: TABEYServerLogType; const Msg: String;
              const Level: Integer = 0); overload;
    procedure Log(const LogType: TABEYServerLogType; const Msg: String;
              const Params: array of const; const Level: Integer = 0); overload;
    procedure OnABEYLog(logtype : TLogType; Time : TDateTime; ThreadID : Cardinal; Const sender, logtext : AnsiString);

    function  GetTerminated: Boolean;
    procedure SetTerminated;

    function  ProcessOSMessage(out Terminate: Boolean): Boolean;
    function  QuitKeyPressed: Boolean;
    function  ProcessApplication: Boolean;
    function  Process: Boolean;
    procedure ProcessOrWait;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Init;
    procedure Run;
    procedure Stop;

    property  Terminated: Boolean read GetTerminated;
    property  OnLog: TABEYServerAppLogEvent read FOnLog write FOnLog;
  end;

var
  ServerApp : TABEYServerApp = nil;

implementation

uses
  SysUtils, Classes;

{ TABEYServerApp }

constructor TABEYServerApp.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FLog := TLog.Create(Nil);
  FLog.OnInThreadNewLog:=OnABEYLog;
  {$IFDEF OS_MSWIN}
  // get the console input handle
  hStdIn := GetStdHandle(STD_INPUT_HANDLE);
  {$ENDIF}
  FWalletKeys := TWalletKeysExt.Create(Nil);
end;

destructor TABEYServerApp.Destroy;
begin
  FreeAndNil(FWalletKeys);
  FLog.OnNewLog:=Nil;
  FreeAndNil(FLog);
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TABEYServerApp.Lock;
begin
  FLock.Acquire;
end;

procedure TABEYServerApp.Unlock;
begin
  FLock.Release;
end;

procedure TABEYServerApp.Log(const LogType: TABEYServerLogType;
  const Msg: String; const Level: Integer);
begin
  if Assigned(FOnLog) then
    FOnLog(LogType, Msg, Level);
end;

procedure TABEYServerApp.Log(const LogType: TABEYServerLogType;
  const Msg: String; const Params: array of const; const Level: Integer);
begin
  Log(LogType, Format(Msg, Params), Level);
end;

procedure TABEYServerApp.OnABEYLog(logtype: TLogType;
  Time: TDateTime; ThreadID: Cardinal; const sender, logtext: AnsiString);
Var s : AnsiString;
begin
  if (logtype=ltdebug)  then exit;
  if ThreadID=MainThreadID then s := ' MAIN:' else s:=' TID:';
  Log(sltInfo,s+IntToHex(ThreadID,8)+' ['+CT_LogType[Logtype]+'] <'+sender+'> '+logtext);
end;

function TABEYServerApp.GetTerminated: Boolean;
begin
  Lock;
  try
    Result := FTerminated;
  finally
    Unlock;
  end;
end;

procedure TABEYServerApp.SetTerminated;
begin
  Lock;
  try
    FTerminated := True;
  finally
    Unlock;
  end;
end;

// Returns True if OS message processed
// Terminate is returned True if application terminated
function TABEYServerApp.ProcessOSMessage(out Terminate: Boolean): Boolean;
{$IFDEF OS_MSWIN}
var
  Msg: TMsg;
{$ENDIF}
begin
  Terminate := False;
  {$IFDEF OS_MSWIN}
  Result := PeekMessageA(Msg, 0, 0, 0, PM_REMOVE);
  if Result then
    if Msg.Message = WM_QUIT then
      Terminate := True
    else
      begin
        TranslateMessage(Msg);
        DispatchMessageA(Msg);
      end
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TABEYServerApp.QuitKeyPressed: Boolean;
{$IFDEF OS_MSWIN}
const
  MaxConsoleEvents = 64;
var
  NumberOfEvents     : DWORD;
  ConsoleEvents      : array[0..MaxConsoleEvents - 1] of TInputRecord;
  EvtP               : PInputRecord;
  NumberOfEventsRead : DWORD;
  I                  : Integer;
  QuitKeyDown        : Boolean;
begin
  QuitKeyDown := False;
  // get the number of events
  NumberOfEvents := 0;
  GetNumberOfConsoleInputEvents(hStdIn, NumberOfEvents);
  if NumberOfEvents <> 0 then
    begin
      // retrieve the event
      NumberOfEventsRead := 0;
      PeekConsoleInput(hStdIn, ConsoleEvents[0], MaxConsoleEvents, NumberOfEventsRead);
      for I := 0 to NumberOfEventsRead - 1 do
        begin
          EvtP := @ConsoleEvents[I];
          if EvtP^.EventType = KEY_EVENT then
            if EvtP^.Event.KeyEvent.bKeyDown and
               ( (EvtP^.Event.KeyEvent.UnicodeChar = 'q') or
                 (EvtP^.Event.KeyEvent.UnicodeChar = 'Q') ) then
              begin
                QuitKeyDown := True;
                break;
              end;
        end;
      // flush the buffer
      FlushConsoleInputBuffer(hStdIn);
    end;
  Result := QuitKeyDown;
end;
{$ELSE}
var
  C : Char;
begin
  Result := False;
  Read(C);
  if (C = 'q') or (C = 'Q') then
    Result := True;
end;
{$ENDIF}

function TABEYServerApp.ProcessApplication: Boolean;
begin
  Result := False;
end;

// Returns True if state processed
// Returns False if idle
function TABEYServerApp.Process: Boolean;
var
  Busy : Boolean;
  DoTerminate : Boolean;
begin
  Busy := True;
  DoTerminate := False;
  if QuitKeyPressed then
    DoTerminate := True
  else
    if not ProcessOSMessage(DoTerminate) then
      if not ProcessApplication then
        Busy := False;
  if DoTerminate then
    SetTerminated;
  Result := Busy;
end;

procedure TABEYServerApp.ProcessOrWait;
begin
  if not Process then
    Sleep(1);
end;

procedure TABEYServerApp.Init;
begin
  TLog.NewLog(ltinfo,Classname,'ABEY Server');
  // Load Node
  // Check OpenSSL dll
  if Not LoadSSLCrypt then raise Exception.Create('Cannot load '+SSL_C_LIB+#10+'To use this software make sure this file is available on you system or reinstall the application');
  TCrypto.InitCrypto;
  FWalletKeys.WalletFileName := TFolderHelper.GetABEYDataFolder+PathDelim+'ABEY.keys';
  // Creating Node:
  FNode := TNode.Node;
  // RPC Server
  Log(sltInfo,'Activating RPC server');
  FRPC := TRPCServer.Create;
  FRPC.WalletKeys := FWalletKeys;
  FRPC.Active:=true;
  // Check Database
  FNode.Bank.StorageClass := TFileStorage;
  TFileStorage(FNode.Bank.Storage).DatabaseFolder := TFolderHelper.GetABEYDataFolder+PathDelim+'Data';
  // Reading database
  Log(sltInfo,'Reading database and constructing blockchain accounts');
  FNode.Node.Bank.DiskRestoreFromOperations(CT_MaxBlock);
  FWalletKeys.Vault := FNode.Node.Bank.Vault;
  Log(sltInfo,'Start discovering nodes');
  FNode.Node.AutoDiscoverNodes(CT_Discover_IPs);
  FNode.Node.NetServer.Active := true;
end;

procedure TABEYServerApp.Run;
begin
  Log(sltinfo,'Start');
  Log(sltinfo,'Running (press Q to stop)');
  while not GetTerminated do
    ProcessOrWait;
end;

procedure TABEYServerApp.Stop;
begin
  Log(sltinfo,'Stop');
  FreeAndNil(FRPC);
  FNode.NetServer.Active := false;
  TNetData.NetData.Free;
  FreeAndNil(FNode);
  Log(sltinfo,'Finalized');
end;

end.
