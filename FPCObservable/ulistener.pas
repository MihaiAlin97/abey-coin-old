unit UListener;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,Classes,SysUtils,blcksock,synsock,lazlogger;

type
  TClientListenerThread = class(TThread)
    private
      FServerSocket : TTCPBlockSocket;
      FClientSocket : TTCPBlockSocket;
      FPort: Word;
      FStatusText : string;
      FSocketHandle : TSocket;
      FOnNewConnection: TNewConnectionEvent;
      procedure HandleNewConnection;  // this is an high level method that signals that a TNewConnectionEvent occurred
    protected
      procedure Execute; override;
    public
      constructor Create(CreateSuspended : Boolean ; DestroyAfterExecution: Boolean);
      property OnNewConnection: TNewConnectionEvent read FOnNewConnection write FOnNewConnection;
    end;

implementation

{ TClientListenerThread }

  constructor TClientListenerThread.Create(CreateSuspended : Boolean ; DestroyAfterExecution: Boolean);
  begin
    inherited Create(CreateSuspended);
    FreeOnTerminate := True;
    fServerSocket := TTCPBlockSocket.Create;
  end;


  procedure TClientListenerThread.HandleNewConnection;
  // this method is executed by the mainthread and can therefore access all GUI elements.
  begin
    if Assigned(FOnNewConnection) then
    begin
      FOnNewConnection(fSocketHandle);
    end;
  end;



  procedure TClientListenerThread.Execute;
  var
    newStatus : string;
    ClientSock:TSocket;
    Message: String;
  begin
    DebugLn(BoolToStr(Terminated));
    fStatusText := 'TClientListenerThread Starting...';
    fStatusText := 'TClientListenerThread Running...';
    while (not Terminated) do
      begin
        DebugLn(NewStatus);
        with FServerSocket do begin
          CreateSocket;
          setLinger(true,10000);
          // the amount of time socket should stay open after a close call was scheduled(this is used to allow queued data to be sent)
          bind('0.0.0.0',Inttostr(6069));
          listen;
          repeat
            if terminated then break;
            Try
              DebugLn('Listening...');
              if canread(1000) then begin
                DebugLn('Can read...');
                ClientSock:=accept;
                if lastError=0 then begin
                  DebugLn('Connecting...');
                  fSocketHandle := ClientSock;
                  //Synchronize(@HandleNewConnection);
                  Queue(@HandleNewConnection);   // this is non-blocking -> schedules calls to functions in a queue,then all calls are executed in the order they've been received
                end;
              end;
            Except
              On E:Exception do begin
                DebugLn('Error '+E.ClassName+':'+E.Message);
              end;
            End;
            sleep(1);
          until false;
        end;
      end;
  end;

end.

