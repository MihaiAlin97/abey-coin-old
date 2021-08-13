unit UPublisher;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef unix}cthreads,cmem,{$endif}
  Classes,SysUtils,blcksock,synsock,lazlogger;
Type

    SubscribeDetails = TList

    Connection = (IPAddress, Port, Latency);

    TNewConnectionEvent = procedure(SocketHandle:TSocket) of Object;

    TNewRegistration = procedure (SocketHandle)

    TConnectionClosed = procedure (SocketHandle:TSocket) of Object;  //
    TConnectionOpened = procedure (SocketHandle:TSocket) of Object;  //


    TEventManager = class (TObject)
    public
          procedure OpenConnection
          procedure GetData
          procedure CloseConnection
          procedure CheckRegistration() of Object;
    end;

    TThreadPool = class // this is an opaque pool; they should be asyncronous and perform programed actions in an async way : such as: open and close a connection; transmit data;
      public
    end;


    {TSettingsManagerThread = class(TThread)
      private
      protected
      public

    end;  }

 // this thread makes changes on every element of the pattern : Listener thread, communication threads(pool), Publisher -> this should block other threads


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

    TClientCommunicationThread = class(TThread)
      private
        SocketHandle:TSocket;
        fSock : TTCPBlockSocket;
      protected
        procedure Execute; override;
        //
        //procedure CloseConnection;
      public
        procedure PrintSettings;
        constructor Create(CreateSuspended : boolean; DestroyAfterExit:Boolean; SH : TSocket );
    end;

    TPublisher = class
      private
        Listener : TClientListenerThread;
      public
        ThreadPool: array of TClientCommunicationThread;
        Subscribers: TStringList;
        SubscriberSockets: array of TSocket; // this is shared between Publisher , TClientListenerThread

        //function RegisterClient(Port:String;Address:String):Integer;
        procedure HandleNewConnection(SockHandle:TSocket);
        Constructor Create;
     end;

    procedure ShowStatus(Status:String);


implementation



  procedure ShowStatus(Status:String);
  begin
    DebugLn(Status);
  end;


  constructor TPublisher.Create;
  var I:Integer;
  begin
    inherited Create;
    //create listener
    Listener := TClientListenerThread.Create(True,True);

    //make OnNewConnection property point to HandleNewConnection method inside TPublisher
    Listener.OnNewConnection:= @HandleNewConnection;


    {SetLength(Connections,15);

    for I:=Low(Connections) to High(Connections) do begin
      Connections[I] := TClientCommunicationThread.Create(True,False,);
    end;          }

    Listener.Start;
  end;

  procedure TPublisher.HandleNewConnection(SockHandle:TSocket);
  var
    ClientHandler:TClientCommunicationThread;
    Message:String;
  begin
    ClientHandler := TClientCommunicationThread.Create(True,True,SockHandle);
    ClientHandler.Start;
  end;



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


  { TClientCommunicationThread }

constructor TClientCommunicationThread.Create(CreateSuspended : boolean; DestroyAfterExit:Boolean; SH : TSocket );
  begin
    inherited Create(CreateSuspended);
    SocketHandle := SH;
    FreeOnTerminate := DestroyAfterExit;
  end;

procedure TClientCommunicationThread.PrintSettings;
var
  SocksType : TSocksType;
begin
  DebugLn('Uses SOCKS: '       + BoolToStr ( fsock.UsingSocks )             + '; ');
  DebugLn('SOCKS type :'       + IntToStr  ( Ord ( fSock.SocksType ) )      + '; ');
  DebugLn('SOCKS Username :'   + fSock.SocksUsername                        + '; ');
  DebugLn('Socks IP :'         + fSock.SocksIP                              + '; ');
  DebugLn('Uses IPv6 mode  :'  + BoolToStr ( fSock.IP6used )                + '; ');
  DebugLn('Socket protocol :'  + IntToStr  ( fSock.GetSocketProtocol )      + '; ');
  DebugLn('Socket type :'      + IntToStr  ( fSock.GetSocketType    )       + '; ');
  DebugLn('Local Sin IP :'     + fSock.GetLocalSinIP                        + '; ');
  DebugLn('Local Sin Port :'   + IntToStr ( fSock.GetLocalSinPort  )        + '; ');
  DebugLn('Remote Sin IP :'    + fSock.GetRemoteSinIP                       + '; ');
  DebugLn('Remote Sin Port:'   + IntToStr ( fSock.GetRemoteSinPort )        + '; ');

end;

 { procedure TClientCommunicationThread.HandleConnection;
  // this method is executed by the mainthread and can therefore access all GUI elements.
  begin
   // DebugLn ('here');
    if Assigned(FOnNewConnection) then
    begin
      FOnNewConnection(fStatusText);
    end;
  end;    }

procedure TClientCommunicationThread.Execute;
var
  Message: String;
begin
  fSock:=TTCPBlockSocket.create;

  fSock.socket:=SocketHandle;
  PrintSettings;
  while (not Terminated) do
    begin


      Message:= fSock.RecvString(2000);
      DebugLn(Message);
      fsock.SendString('responded'#13#10);
      sleep(1000);
    end;
  fSock.Free;
  inherited Destroy;
end;




end.

