unit UUpdateVersionThread;

{$mode objfpc}{$H+}

interface

uses

{$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
{$ENDIF}{$ENDIF}

{$IFDEF Unix}
  cthreads, baseUnix,

{$ENDIF Unix}

  Classes, SysUtils, fpjson, jsonparser, fphttpclient, OpenSSL, Process, UConst;

type
  TUpdateInstalledEvent = procedure(Update: string) of Object;

  TUpdateThread = class(TThread)
  private
    fUpdateText: String;
    fServiceType: String; // w = wallet, m = miner, d = daemon
    FOnUpdateDone: TUpdateInstalledEvent;
    procedure UpdateDone;
  protected
    procedure Execute; override;
  public
    fParent: string; // for knowing where this thread is called;
    constructor Create(CreateSuspended: boolean; Caller: string);
    property OnUpdateDone: TUpdateInstalledEvent read FOnUpdateDone write FOnUpdateDone;
    procedure KillProcess;
  end;

implementation

constructor TUpdateThread.Create(CreateSuspended: boolean; Caller: string);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := true;
  fParent := Caller;
end;

procedure TUpdateThread.UpdateDone;
// this method is executed by the mainthread and can therefore access all GUI elements.
begin
  if Assigned(FOnUpdateDone) then
    FOnUpdateDone(fUpdateText);
end;

procedure TUpdateThread.KillProcess;
var
  Process: TProcess;
  I: integer;
begin
  Process := TProcess.Create(nil);
  try
    Process.InheritHandles := false;
    Process.Options := [];
    Process.ShowWindow := swoShow;

    for I := 1 to GetEnvironmentVariableCount do
      Process.Environment.Add(GetEnvironmentString(I));

    Process.Parameters.Add('-p');
    Process.Parameters.Add(ParamStr(0));
    Process.Execute;
  finally
    Process.Free;
  end;
end;

procedure TUpdateThread.Execute;
var
  newUpdate: string;
  json: string;
  PostJson: TJSONData;
  responseData: string;
  JsonResponse: TJSONData;
  urlToDownloadFrom: string;
  isCriticalUpdate: integer;
  s: string;

  UpdateType: string;

  LastVersion: string;
  Major: string;
  Minor: string;
  Build: string;

  Process: TProcess;
  I: integer;
  Client: TFPHttpClient;
begin
  InitSSLInterface;
  while (not Terminated) do
  begin
    try
      if (fParent = 'daemon') then
        fServiceType := 'd'
      else
        if (fParent = 'wallet') then
          fServiceType := 'w'
        else
          if (fParent = 'miner') then
            fServiceType := 'm';
      Client := TFPHttpClient.Create(nil);
      responseData := Client.Get('https://updates.abey.com/version.php?t='+fServiceType);
      JsonResponse := GetJSON(responseData);
      Client.Free;

      {$IFDEF Windows}
      urlToDownloadFrom := JsonResponse.FindPath('url_windows').AsString;
      {$ENDIF Windows}

      {$IFDEF Linux}
      urlToDownloadFrom := JsonResponse.FindPath('url_linux').AsString;
      {$ENDIF LinUx}

      {$IFDEF Mac}
      urlToDownloadFrom := JsonResponse.FindPath('url_mac').AsString;
      {$ENDIF Mac}

      Major := JsonResponse.FindPath('major').AsString;
      Minor := JsonResponse.FindPath('minor').AsString;
      Build := JsonResponse.FindPath('build').AsString;
      isCriticalUpdate := JsonResponse.FindPath('critical').AsInteger;
      LastVersion := Major + Minor + Build;

      if (fParent = 'daemon') then
      begin
        if ((VERSION_DAEMON_MAJOR < StrToIntDef(Major, 0)) or
           ((VERSION_DAEMON_MAJOR = StrToIntDef(Major, 0)) and (VERSION_DAEMON_MINOR < StrToIntDef(Minor, 0))) or
           ((VERSION_DAEMON_MAJOR = StrToIntDef(Major, 0)) and (VERSION_DAEMON_MINOR = StrToIntDef(Minor, 0)) and (VERSION_DAEMON_BUILD < StrToIntDef(Build, 0)))) then // update is available
        if (isCriticalUpdate = 1) then // we force a restart of the daemon
        begin
          fUpdateText := IntToStr(isCriticalUpdate)+urlToDownloadFrom;
          Synchronize(@UpdateDone);
        end;
      end
      else
      if (fParent = 'miner') then
      begin
        if ((VERSION_MINER_MAJOR < StrToIntDef(Major, 0)) or
           ((VERSION_MINER_MAJOR = StrToIntDef(Major, 0)) and (VERSION_MINER_MINOR < StrToIntDef(Minor, 0))) or
           ((VERSION_MINER_MAJOR = StrToIntDef(Major, 0)) and (VERSION_MINER_MINOR = StrToIntDef(Minor, 0)) and (VERSION_MINER_BUILD < StrToIntDef(Build, 0)))) then // update is available
        if (isCriticalUpdate = 1) then // we force a restart of the miner
        begin
          fUpdateText := IntToStr(isCriticalUpdate)+urlToDownloadFrom;
          Synchronize(@UpdateDone);
        end;
      end
      else
      if (fParent = 'wallet') then
      begin
        if ((VERSION_WALLET_MAJOR < StrToIntDef(Major, 0)) or
           ((VERSION_WALLET_MAJOR = StrToIntDef(Major, 0)) and (VERSION_WALLET_MINOR < StrToIntDef(Minor, 0))) or
           ((VERSION_WALLET_MAJOR = StrToIntDef(Major, 0)) and (VERSION_WALLET_MINOR = StrToIntDef(Minor, 0)) and (VERSION_WALLET_BUILD < StrToIntDef(Build, 0)))) then // update is available}
        begin
          // notify wallet that an update is available
          fUpdateText := IntToStr(isCriticalUpdate)+urlToDownloadFrom;
          Synchronize(@UpdateDone);
        end;
      end;

    except
{      on E: EHtTABEYlient do
        writeln('Update ERROR: '+e.message);}
      on E: Exception do
      begin
        fUpdateText := E.Message;
        Synchronize(@UpdateDone);
      end;
    end;

    Sleep(UPDATE_INTERVAL); // every 15 minutes
  end;
end;

end.
