unit UPublisher;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef unix}cthreads,cmem,{$endif}
  Classes,SysUtils,blcksock,synsock,lazlogger,contnrs,typinfo,syncobjs,UJSONCommunication,fgl,ULog,sockets;

Type

    TPortDict = specialize TFPGMap <String, Integer>;

    TConnection = class;
    PConnection = ^TConnection;

    TWorkerTHread = class;
    PWorkerThread = ^TWorkerThread;

    {Types a ThreadWorker can be in}
    ThreadWorkerStatus = (TaskAssigned,TaskRunning,TaskCompleted);
    {TaskType}
    WorkerTaskType = (RegisterConnection,EstablishConnection,OpenConnection,CloseConnection,SendData,RemoveConnection);


    TNewConnectionEvent = procedure(SocketHandle:TSocket;Port:String;IP:String) of Object;


    PBlockchainEvent = ^BlockchainEvent;
    TNewSubscriptionEvent = procedure(Event:PBlockchainEvent) of Object;



    TEstablishConnectionEvent = procedure (AConnection:PConnection) of Object;
    TRegisterConnectionEvent = procedure (AConnection:PConnection) of Object;

    TOpenConnectionEvent = procedure (AConnection:PConnection) of Object;
    TCloseConnectionEvent = procedure (AConnection:PConnection) of Object;

    TThreadAvailableEvent = procedure (AThread : PWorkerThread) of Object;

    {$INTERFACES CORBA}
    TConnection = class      //(TObject,IFPObserved)
      private
        FSocket : TTCPBlockSocket;
        FSocketHandle : TSocket;
        //FSocket : TTCPBlockSocket;
        FRemoteIP : String;
        FRemotePort : String;
        FLocalPort : String;
        FLinger : Integer;
        FTimeout : Integer;

        FStatus : String;
        FLastError : Integer;
        FReference : Pointer;

        FEncoding : String;
        FEventList : String;

        //events
        FOnEstablishConnection : TEstablishConnectionEvent;
        FOnRegisterConnection : TRegisterConnectionEvent;

      class var Connections: TList;


      public
        TaskType : WorkerTaskType;

        property DoEstablishConnection : TEstablishConnectionEvent read FOnEstablishConnection write FOnEstablishConnection;
        procedure EstablishConnection(AConnection:PConnection);

        property DoRegisterConnection :  TRegisterConnectionEvent read FOnRegisterCOnnection write FOnRegisterCOnnection;
        procedure RegisterConnection(AConnection:PConnection);

        procedure SetSocket ( ASocket : TTCPBlockSocket);
        function GetSocket : TTCPBlockSocket;
        property Socket : TTCPBlockSocket read GetSocket write SetSocket;



        procedure SetSocketHandle ( ASocketHandle : TSocket );
        function GetSocketHandle : TSocket ;
        property SocketHandle : TSocket read GetSocketHandle write SetSocketHandle;

        procedure SetRemoteIP ( AIP : String );
        function GetRemoteIP : String ;
        property RemoteIP : String read GetRemoteIP write SetRemoteIP;

        procedure SetRemotePort ( APort : String );
        function GetRemotePort : String ;
        property RemotePort : String read GetRemotePort write SetRemotePort;

        procedure SetLocalPort ( APort : String );
        function GetLocalPort : String ;
        property LocalPort : String read GetLocalPort write SetLocalPort;

        procedure SetLinger( ALinger : Integer );
        function GetLinger : Integer ;
        property Linger : Integer read GetLinger write SetLinger;

        procedure SetTimeout( ATimeout : Integer );
        function GetTimeout : Integer ;
        property Timeout : Integer read GetTimeout write SetTimeout;

        constructor Create( ASocketHandle: TSocket)  ; overload;
        constructor Create( ASocketHandle: TSocket; ARemoteIP:String ; ARemotePort : String; ALocalPort : String ; ALinger : Integer ; ATimeout : Integer) ; overload;
    end;


    TConnectionPool = class (TList)
    private
          FCoupledComponents : TList;
    public
          ConnectionStack: array of TConnection ;
          function  AddNewConnection( ASocketHandle : TSocket; AIP:String;APort:String):PConnection;
          function AddNew( AConnection : PConnection) : PConnection;

          procedure Couple( AComponent : TObject);
          procedure DecoupleComponent ( AComponent : TObject);

          {Setters & Getters}

          procedure SetCoupledComponents( AComponents : TList );
          function  GetCoupledComponents : TList ;
          property CoupledComponents : TList read GetCoupledComponents write SetCoupledComponents;

          constructor Create;
    end;

 // this thread makes changes on every element of the pattern : Listener thread, communication threads(pool), Publisher -> this should block other threads

    {TClientListenerThread}
    TClientListenerThread = class(TThread)
    private
      FServerSocket : TTCPBlockSocket;
      FClientSocket : TTCPBlockSocket;
      FPort: Word;
      FStatusText : string;
      FSocketHandle : TSocket;
      fLastPort : String;
      fLastIP : String;
      FOnNewConnection: TNewConnectionEvent;
      procedure HandleNewConnection;  // this is an high level method that signals that a TNewConnectionEvent occurred
    protected
      procedure Execute; override;
    public
      constructor Create(CreateSuspended : Boolean ; DestroyAfterExecution: Boolean);
      property OnNewConnection: TNewConnectionEvent read FOnNewConnection write FOnNewConnection;
    end;


    {TWorkerThread }
    TWorkerThread = class(TThread)
      private

        FStatus : ThreadWorkerStatus;
        FTask : WorkerTaskType;
        FConnection : PConnection;


        CritSec : TCriticalSection;

        FOnNewSubscription : TNewSubscriptionEvent;

      protected
        procedure Execute; override;
      public
        FEvent : PBlockchainEvent;

        ID : Integer;
        {Getters & Setters}
        Name: String;

        procedure SetName ( AName : String);
        function GetName : String;
        property SecureName : String read GetName write SetName;

        {Task}
        procedure SetTask ( ATask : WorkerTaskType);
        function GetTask : WorkerTaskType;
        property Task : WorkerTaskType read GetTask write SetTask;

        {Status}
        procedure SetStatus ( AStatus : ThreadWorkerStatus);
        function GetStatus : ThreadWorkerStatus;
        property Status : ThreadWorkerStatus read GetStatus write SetStatus;

        {Connection}
        procedure SetConnection ( AConnection : PConnection);
        function GetCOnnection : PConnection;
        property Connection : PConnection read GetCOnnection write SetConnection;


        procedure EstablishConnection(AConnection : PConnection);
        procedure OpenConnection(AConnection : PConnection);
        procedure RegisterConnection(AConnection : PConnection);
        procedure CloseConnection(AConnection : PConnection);
        procedure RemoveConnection (AConnection : PConnection);
        procedure SendData( AConnection : PConnection);
        procedure PrintSettings(AConnection : PConnection);


        procedure ExecuteTask(ATask : WorkerTaskType);




        constructor Create(CreateSuspended : boolean; DestroyAfterExit:Boolean);
        property OnNewSubscriptionEvent : TNewSubscriptionEvent read FOnNewSubscription write FOnNewSubscription;
        procedure HandleNewSubscription;

    end;



    {TWorkerPool}
    TWorkerPool = class(TList)
    private
          FComponent : TObject;
          FAvailableThread : PWorkerThread;

    public
          FOnThreadAvailable : TThreadAvailableEvent ;
          WorkerStack: array of TWorkerThread ;
          ThreadsCount : Integer;
          constructor Create(CreateSuspended : Boolean = True ; DestroyAfterExit : Boolean = True; NumberOfThreads : Cardinal = 6 );
          procedure Init(CreateSuspended : Boolean ; DestroyAfterExit : Boolean);

          procedure CoupleComponent( AComponent : TObject);
          procedure DecoupleComponent ( AComponent : TObject);

          function AddNew( AThread : PWorkerThread) : PWorkerThread;
          function AddNewThread:PWorkerThread;
          function GetItem (Index:Integer): PWorkerThread;

           procedure ThreadAvailable;

           property DoOnThreadAvailable : TThreadAvailableEvent read FOnThreadAvailable write FOnThreadAvailable;


    end;


    {TSyncQueue}
    TSyncQueue = class (TQueue)
    private
      FList: TList;
    protected
        procedure PushItem(AItem: Pointer); override;
        function PopItem: Pointer; override;
        function PeekItem: Pointer; override;
        property List: TList read FList;
    public
       Lock : TCriticalSection;
       function AtLeast(ACount: Integer): Boolean;
       function Count: Integer;
       constructor Create;
       destructor Destroy; override;
       function Pop: Pointer;
       function Push(AItem: Pointer): Pointer;

    end;

    {TTaskManager}
    TTaskManager = class (TThread)
      private
          {FConnectionPool : TConnectionPool;
          FWorkerPool : TWorkerPool;
          FConnections : TSyncQueue;
          FThreads : TSyncQueue; }

      protected
          procedure Execute; override;
      public
          FConnectionPool : TConnectionPool;
          FWorkerPool : TWorkerPool;
          FConnections : TSyncQueue;
          FThreads : TSyncQueue;

          ConnectionsLock:TCriticalSection;
          ThreadsLock:TCriticalSection;
          FID: Integer;
          procedure NotifyThreadAvailable(AThread : PWorkerThread);
          procedure NotifyMethodToBeExecuted(AConnection : PConnection);
          procedure AssignTask( AThread : PWorkerThread ; AConnection : PConnection; ATask : WorkerTaskType);
          procedure ScheduleTask( AThread : PWorkerThread ; AConnection : PConnection; ATask : WorkerTaskType);
          constructor Create(CreateSuspended : Boolean = True ; DestroyAfterExecution: Boolean = True);
          procedure Start;

          {Getters & Setters}

          {Connections}
          procedure SetConnections ( SConnections : TSyncQueue);
          function GetConnections : TSyncQueue;
          property Connections : TSyncQueue read GetConnections write SetConnections;

          {Threads}
          procedure SetThreads ( SThreads : TSyncQueue);
          function GetThreads : TSyncQueue;
          property Threads : TSyncQueue read GetThreads write SetThreads;
    end;

    {TPublisher}
    TPublisher = class
      private
        Listener : TClientListenerThread;
      public

        BlockCache : TObject;

        EventList : array of BlockchainEvent;
        FTaskManager : TTaskManager;

        //function RegisterClient(Port:String;Address:String):Integer;
        procedure HandleNewSubscription(Event:PBLockchainEvent);
        procedure HandleNewConnection(SockHandle:TSocket;Port:String;IP:String);

        Constructor Create;


        end;

    procedure ShowStatus(Status:String);
var
  GlobalCanReadCritSection : TCriticalSection;
  GlobalEvent : BlockchainEvent;
  GlobalEventDone:Boolean = False;

implementation

uses UBlockchain;


  {HelperFunctions}
  function GetRandomPortBetween(a, b: Integer): Integer;
  var Difference : Integer;
  begin
    Difference := b - a;
    Result := a + Random (Difference);
  end;
  {HelperFunction}

  procedure TConnection.EstablishConnection(AConnection:PConnection);
  begin
    if Assigned(FOnEstablishConnection) then
    begin
      DebugLn('EstablishConnection set up');
      FOnEstablishConnection(AConnection);
    end;
  end;

  procedure TConnection.RegisterConnection(AConnection:PConnection);
  begin
    if Assigned(FOnRegisterConnection) then
    begin
      FOnRegisterConnection(AConnection);
    end;
  end;

  procedure TConnection.SetSocket ( ASocket : TTCPBlockSocket);
  begin
    FSocket := ASocket;
  end;

  function TConnection.GetSocket : TTCPBlockSocket;
  begin
    Result := FSocket;
  end;


  procedure TConnection.SetSocketHandle ( ASocketHandle : TSocket );
  begin
    FSocketHandle := ASocketHandle;
  end;

  function TConnection.GetSocketHandle : TSocket ;
  begin
    Result := FSocketHandle;
  end;

  procedure TConnection.SetRemoteIP ( AIP : String );
  begin
    FRemoteIP := AIP;
  end;

  function TConnection.GetRemoteIP : String ;
  begin
    Result := FRemoteIp;
  end;

  procedure TConnection.SetRemotePort ( APort : String );
  begin
    RemotePort := APort;
  end;

  function TConnection.GetRemotePort : String ;
  begin
    Result := FRemotePort;
  end;


  procedure TConnection.SetLocalPort ( APort : String );
  begin
    FLocalPort := APort;
  end;

  function TConnection.GetLocalPort : String ;
  begin
    Result := FLocalPort;
  end;

  procedure TConnection.SetLinger( ALinger : Integer );
  begin
    FLinger := ALinger;
  end;

  function TConnection.GetLinger : Integer ;
  begin
    Result := FLinger;
  end;

  procedure TConnection.SetTimeout( ATimeout : Integer );
  begin
    FTimeout := ATimeout;
  end;

  function TConnection.GetTimeout : Integer ;
  begin
    Result := FTimeout;
  end;

  constructor TConnection.Create( ASocketHandle: TSocket)  ; overload;
  begin
    //inherited;
    FSocketHandle := ASocketHandle;
  end;


  constructor TConnection.Create( ASocketHandle: TSocket; ARemoteIP:String ; ARemotePort : String; ALocalPort : String ; ALinger : Integer ; ATimeout : Integer)  ; overload;
  begin
    //inherited;
    FSocketHandle := ASocketHandle;
    FRemoteIP := ARemoteIP;
    FRemotePort := ARemotePort;
    FLocalPort := ALocalPort;
    FLinger := ALinger;
    FTimeout := ATimeout;
  end;

   { TConnectionPool }
  function TConnectionPool.AddNewConnection( ASocketHandle : TSocket; AIP:String;APort:String):PConnection;
  var Connection : TConnection;
  begin
    //Randomize;
    DebugLn('Count1:' + IntToStr(Count));
    // resize the worker stack; this is where the objects are created and later referenced inside the TWorkerPool's inherited list of pointers for quick access
    SetLength(ConnectionStack,( Count + 1 ));

    ConnectionStack [Count] := TConnection.Create(ASocketHandle,AIP,APort,AIp,10,10);
    DebugLn(ConnectionStack[Count].RemoteIP);
    DebugLn(IntToStr(ConnectionStack[Count].Linger));
    //DebugLn('Creating new PWorkerThread with threadID');

    Result := AddNew(@ConnectionStack [Count]);
    DebugLn('Count2:' + IntToStr(Count));


    {Connection := TConnection.Create(ASocketHandle,AIP,APort,AIp,10,10);
    DebugLn('Creating new PConnection');
    Result := AddNew(@Connection); }
  end;

  function TConnectionPool.AddNew( AConnection : PConnection) : PConnection;
  var cpldCOmp : ^TTaskManager;
    res:Integer;
  begin
    //TODO : Error checking for Add;
    DebugLn('Adding PConnection to PConnectionPool');
    Res := Add(AConnection);
    Result := Items[Count-1];

  end;

  procedure TConnectionPool.Couple( AComponent : TObject);
  var I,J : Integer;
      tst : ^TTaskManager;
      res : Integer;
  begin
    //FPOAttachObserver(AComponent);
    res := CoupledComponents.Add(@AComponent);
    tst := CoupledComponents.First;
    DebugLn('Coupled component with FID' + IntToStr(tst^.FID));
    for I := 1 to Count do begin
       DebugLn('Coupling connection ' + IntToStr (I) + ' to TaskManager' );
       PConnection(Items[I])^.DoEstablishConnection := @TTaskManager(AComponent).NotifyMethodToBeExecuted;
       PConnection(Items[I])^.DoRegisterConnection := @TTaskManager(AComponent).NotifyMethodToBeExecuted;
     end;
    //all methods of connections are set to the same callback NotifyMethodToBeExecuted



  end;

  procedure TConnectionPool.DecoupleComponent ( AComponent : TObject);
  begin
    //FPODetachObserver(AComponent);

  end;

  procedure TConnectionPool.SetCoupledComponents( AComponents : TList );
  begin

    FCoupledComponents := AComponents;

  end;

  function  TConnectionPool.GetCoupledComponents : TList ;
  begin
    Result := FCoupledComponents;
  end;

  constructor TConnectionPool.Create;
  begin
    inherited Create;
    FCoupledComponents := TList.Create;
    DebugLn('Created TConnectionPool');
  end;

  procedure ShowStatus(Status:String);
  begin
    DebugLn(Status);
  end;


  constructor TPublisher.Create;
  var I:Integer;
      tempTh : TWOrkerThread;
      PWT : PWorkerThread;
      Layka : String;
  begin
    inherited Create;
    //create listener

    GlobalCanReadCritSection := TCriticalSection.Create;

    Listener := TClientListenerThread.Create(True,True);

    //make OnNewConnection property point to HandleNewConnection method inside TPublisher

    TLog.NewLog(lterror,ClassName,'Created Publisher ');
    //Start task manager
    FTaskManager := TTaskManager.Create(True,True);

    DebugLn('ListenerStarted');
    DebugLn('Assigned OnNewConnection');
    Listener.OnNewConnection:= @HandleNewConnection;

    //Start listener
    Listener.Start;

    TLog.NewLog(lterror,ClassName,'Publisher violation here');
    for I := 1 to FTaskManager.FWorkerPool.Count - 1 do begin

       PWorkerThread(FTaskManager.FWorkerPool.Items[I])^.OnNewSubscriptionEvent:=@HandleNewSubscription;

    end;

   //PWT := FTaskManager.FWorkerPool.Last;
   //DebugLn('Last thread added ' + TWT.Name);
   //DebugLn('Last thread added ' + IntTOStr((PWT^.ID)));

    //FTaskManager.FWorkerPool.CoupleComponent(FTaskManager);


    FTaskManager.Start;


  end;

  procedure TPublisher.HandleNewConnection(SockHandle:TSocket;Port:String;IP:String);
  var
    ClientHandler:TWorkerTHread;
    Message:String;
    tempPConn:PConnection;
  begin
    TLog.NewLog(lterror,ClassName,'Arrived to HandleSubscription: ');

    //This creates a TConnection in the back-end
    tempPConn := FTaskManager.FConnectionPool.AddNewConnection(SockHandle,IP,Port);  // a TConnection was created

    DebugLn('Conn remote portL ' + tempPConn^.RemotePort);
    //set task type to be at EstablishConnection
    tempPConn^.TaskType:= EstablishConnection;
    //tempPconn^.RemotePort:=Port;
    //tempPconn^.RemoteIP:=Ip;

    tempPConn^.DoEstablishConnection := @FTaskManager.NotifyMethodToBeExecuted ;
    tempPConn^.EstablishConnection(tempPConn);

  end;


  procedure TPublisher.HandleNewSubscription(Event:PBLockchainEvent);

  begin

    GlobalCanReadCritSection.Acquire;

    SetLength(EventList,Length(EventList) + 1 );
    EventList[Length(EventList) - 1 ] := GlobalEvent;


    TLog.NewLog(lterror,ClassName,'Arrived to HandleSubscription: ');
    TLog.NewLog(lterror,ClassName,'EventType: ' + IntToStr(EventList[0].EvType));
    TLog.NewLog(lterror,ClassName,'EventTiming: ' + IntToStr(EventList[0].EvTiming));
    TLog.NewLog(lterror,ClassName,'EventSender: ' + IntToStr(EventList[0].Sender));
    TLog.NewLog(lterror,ClassName,'EventReceiver: ' + IntToStr(EventList[0].Receiver));
    TLog.NewLog(lterror,ClassName,'EventAmount: ' + IntToStr(EventList[0].Amount));

    GlobalCanReadCritSection.Release;
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
      FOnNewConnection(fSocketHandle,fLastPort,fLastIP);
    end;
  end;



  procedure TClientListenerThread.Execute;
  var
    newStatus : string;
    ClientSock:TSocket;
    Message: String;
    JSONHelper : TJSONHelper;
    AvailablePorts : TPortDict;
    tempPort : String;
    Done : Boolean;
  begin
    JSONHelper := TJSONHelper.Create;
    AvailablePorts := TPortDict.Create;
    Randomize;

    DebugLn(BoolToStr(Terminated));
    fStatusText := 'TClientListenerThread Starting...';
    fStatusText := 'TClientListenerThread Running...';

    Done := False;
    while (not Terminated) do
      begin
        DebugLn(NewStatus);
        FServerSocket := TTCPBlockSocket.Create;


        with FServerSocket do begin
          //CreateSocket;
          setLinger(true,100);
          // the amount of time socket should stay open after a close call was scheduled(this is used to allow queued data to be sent)
          TLog.NewLog(lterror,ClassName,'Listening ');

          TLog.NewLog(lterror,ClassName,'Last error ' + IntTOStr(LastError));
          TLog.NewLog(lterror,ClassName,'Last error desc' + LastErrorDesc);

          bind('0.0.0.0','6069');

          TLog.NewLog(lterror,ClassName,'Last error ' + IntTOStr(LastError));
          TLog.NewLog(lterror,ClassName,'Last error desc' + LastErrorDesc);
          TLog.NewLog(lterror,ClassName,'Socket ' + IntTOStr(Socket));
          listen;


          Done := False;

          TLog.NewLog(lterror,ClassName,'Last error ' + IntTOStr(LastError));
          TLog.NewLog(lterror,ClassName,'Last error desc' + LastErrorDesc);
          repeat
            if terminated then break;
            Try
              //DebugLn('Listening...');
              TLog.NewLog(lterror,ClassName,'Entered once ');
              TLog.NewLog(lterror,ClassName,'Can read...' + BoolToStr(canread(2500)));

              if canread(2500) then begin
              TLog.NewLog(lterror,ClassName,'Socket after Accept ' + IntTOStr(Socket));
                Socket:= Accept;

                if lastError=0 then begin
                  //DebugLn('Connecting...');


                  Message:= RecvString(2000);
                  DebugLn('AccesVIolationPassed');

                  if Message <> '' then begin
                     //DebugLn('IsValidJSON' + BoolToStr(JSONHelper.StringIsValidJSON(Message)));
                     DebugLn('Message : '+ Message);
                     //DebugLn('IsRegistrationAction' + BoolToStr(JSONHelper.IsRegistrationAction(Message)));

                     tempPort := IntToStr(GetRandomPortBetween(5000,6000));

                     while AvailablePorts.IndexOf(tempPort) <> -1 do begin
                        tempPort := IntToStr(GetRandomPortBetween(5000,6000));
                     end;
                     AvailablePorts.Add(tempPort);

                     SendString(JSONHelper.RegistrationResponse('127.0.0.1',tempPort,1));

                     fSocketHandle := Socket;
                     fLastPort := tempPort;
                     fLastIP := '0.0.0.0';
                     Synchronize(@HandleNewConnection);   // this is non-blocking -> schedules calls to functions in a queue,then all calls are executed in the order they've been received
                     Done := True;

                  end;

                  sleep(100);
                  //Queue(@HandleNewConnection);   // this is non-blocking -> schedules calls to functions in a queue,then all calls are executed in the order they've been received
                end;
              end;

            Except
              On E:Exception do begin
                TLog.NewLog(lterror,ClassName,'Error '+E.ClassName+':'+E.Message);
              end;
            End;
            sleep(10);
          until Done= True;
          TLog.NewLog(lterror,ClassName,'Socket closed');




        end;


        FServerSocket.Destroy;
        TLog.NewLog(lterror,ClassName,'Socket closing result' + IntTOStr(FPShutdown(FServerSocket.Socket, SHUT_RDWR)));
        TLog.NewLog(lterror,ClassName,'Socket closing result' + IntTOStr(SocketError));

        TLog.NewLog(lterror,ClassName,'Socket after closing ' + IntTOStr(FServerSocket.Socket));

        FServerSocket.StopFlag := True;

        FServerSocket.CloseSocket;

      end;
    DebugLn('Listener done');
  end;


  { TWorkerThreadThread }

constructor TWorkerThread.Create(CreateSuspended : boolean; DestroyAfterExit:Boolean);
  begin
    inherited Create(CreateSuspended);

    //SocketHandle := SH;
    FreeOnTerminate := DestroyAfterExit;
    Name := 'None';

    Randomize;
    sleep(100);
    ID := Random(100);
    Randomize;

    DebugLn('Creating thread with ID ' + IntTOStr(ID));
    Name := Name + IntTOStr(ID);
    CritSec := TCriticalSection.Create;

  end;

procedure TWorkerThread.HandleNewSubscription;

 begin
   if Assigned(FOnNewSubscription) then
   begin
     FOnNewSubscription(@FEvent);
   end;
 end;

procedure TWorkerThread.SetName ( AName : String);
begin
  CritSec.Acquire;
  Name := AName;
  CritSec.Release;
end;

function TWorkerThread.GetName : String;
begin
  CritSec.Acquire;
  result := Name;
  CritSec.Release;
end;

procedure TWorkerThread.PrintSettings(AConnection : PConnection);
begin
  DebugLn('Uses SOCKS: '       + BoolToStr ( Aconnection^.Socket.UsingSocks )             + '; ');
  DebugLn('SOCKS type :'       + IntToStr  ( Ord ( Aconnection^.Socket.SocksType ) )      + '; ');
  DebugLn('SOCKS Username :'   + Aconnection^.Socket.SocksUsername                        + '; ');
  DebugLn('Socks IP :'         + Aconnection^.Socket.SocksIP                              + '; ');
  DebugLn('Uses IPv6 mode  :'  + BoolToStr ( Aconnection^.Socket.IP6used )                + '; ');
  DebugLn('Socket protocol :'  + IntToStr  ( Aconnection^.Socket.GetSocketProtocol )      + '; ');
  DebugLn('Socket type :'      + IntToStr  ( Aconnection^.Socket.GetSocketType    )       + '; ');
  DebugLn('Local Sin IP :'     + Aconnection^.Socket.GetLocalSinIP                        + '; ');
  DebugLn('Local Sin Port :'   + IntToStr ( Aconnection^.Socket.GetLocalSinPort  )        + '; ');
  DebugLn('Remote Sin IP :'    + Aconnection^.Socket.GetRemoteSinIP                       + '; ');
  DebugLn('Remote Sin Port:'   + IntToStr ( Aconnection^.Socket.GetRemoteSinPort )        + '; ');

end;

procedure TWorkerThread.SetTask ( ATask : WorkerTaskType);
begin
   FTask := ATask;
end;

function TWorkerThread.GetTask : WorkerTaskType;
begin
   Result := FTask;
end;

procedure TWorkerThread.SetStatus ( AStatus : ThreadWorkerStatus);
begin
     FStatus := AStatus;
end;

function TWorkerThread.GetStatus : ThreadWorkerStatus;
begin
    Result := FStatus;
end;

procedure TWorkerThread.SetConnection ( AConnection : PConnection);
begin
  FConnection := @AConnection^;
end;

function TWorkerThread.GetConnection : PConnection;
begin
  Result := @FConnection^;
end;

procedure TWorkerThread.EstablishConnection(AConnection : PConnection);
var fsock : TTCPBlockSocket;
  Message : String;
  Done:Boolean;
  Port : String;
  CLientSock : TSocket;
  JSONHelper : TJSONHelper;
  tempEvent : BlockchainEvent;
  SubsMessage:String;
  Count : Integer = 0 ;
begin
  FStatus := TaskRunning;
  DebugLn('Entered EstablishConnection Task in thread ' + IntTOstr(ID));

  Done := False;
  fSock:=TTCPBlockSocket.create;

  Message:='init';
  JSONHelper := TJSONHelper.Create;

  Port := Aconnection^.RemotePort;
  while (Done=False) do
      begin
        with fSock do begin
          CreateSocket;
          setLinger(true,10000);
          DebugLn('OnSeparate Port: ' + Aconnection^.RemotePort);
          DebugLn('OnSeparate Port: ' + Port);
          // the amount of time socket should stay open after a close call was scheduled(this is used to allow queued data to be sent)
          bind('0.0.0.0',Port);
          listen;
          //TODO:MAKE sure accept is called only once;and outside repeat
          fsock.Socket:=accept;
          repeat
            if terminated then break;
            Try
              //DebugLn('ListeningOnSeparate...');

              //DebugLn('LastErrorDesc' + LastErrorDesc);
              //DebugLn('LasError' + IntTOStr(LastError));


              {DebugLn('Local Sin IP :'     + GetLocalSinIP                        + '; ');
              DebugLn('Local Sin Port :'   + IntToStr ( GetLocalSinPort  )        + '; ');
              DebugLn('Remote Sin IP :'    + GetRemoteSinIP                       + '; ');
              DebugLn('Remote Sin Port:'   + IntToStr ( GetRemoteSinPort )        + '; ');
              DebugLn('Usign socks:'   + BoolToStr ( UsingSocks )        + '; ');  }

              DebugLn('OnSeparate canread ' + BoolToStr(canread(2500)));


              if canread(2500) then begin





                if lastError = 0 then begin
                  TLog.NewLog(lterror,ClassName,'Count: '+ IntToStr(Count));

                  TLog.NewLog(lterror,ClassName,'Event done :'+BoolToStr(GlobalEventDone));


                  Message:= RecvString(2000);
                  SubsMessage := Message;
                  TLog.NewLog(lterror,ClassName,'Message: '+Message);



                  //check event is done
                  GlobalCanReadCritSection.Acquire;
                    if GlobalEventDone = True then begin

                      TLog.NewLog(lterror,ClassName,'Event done YADAA:'+BoolToStr(GlobalEventDone));
                      SendString('Done'#13#10);
                    end;
                  GlobalCanReadCritSection.Release;

                  //get message

                  if Message <> '' then begin

                    TLog.NewLog(lterror,ClassName,'Is subscribing action: '+ BoolToStr(JSONHelper.IsSubscribingAction(Message)));

                    if  JSONHelper.IsSubscribingAction(Message) = True then begin
                        Count := Count + 1;

                         TLog.NewLog(lterror,ClassName,'Count: '+ IntToStr(Count));


                        tempEvent := JSONHelper.GetEventFromSubscription(SubsMessage);




                        SendString(JSONHelper.SubscriptionResponse(1));

                        TLog.NewLog(lterror,ClassName,'Count: '+ IntToStr(Count));

                        GlobalCanReadCritSection.Acquire;

                        TLog.NewLog(lterror,ClassName,'EventType: ' + IntToStr(GlobalEvent.EvType));
                        TLog.NewLog(lterror,ClassName,'EventTiming: ' + IntToStr(GlobalEvent.EvTiming));
                        TLog.NewLog(lterror,ClassName,'EventSender: ' + IntToStr(GlobalEvent.Sender));
                        TLog.NewLog(lterror,ClassName,'EventReceiver: ' + IntToStr(GlobalEvent.Receiver));
                        TLog.NewLog(lterror,ClassName,'EventAmount: ' + IntToStr(GlobalEvent.Amount));

                        GlobalCanReadCritSection.Release;


                        Queue(@HandleNewSubscription);



                        TLog.NewLog(lterror,ClassName,'Continued EstablishConnection here: '+SubsMessage);

                    end

                    else SendString('Waiting'#13#10);

                  end else SendString('Waiting'#13#10);

                  //SubsMessage := RecvString(2000);
                  //TLog.NewLog(lterror,ClassName,'Exited: ' + SubsMessage);





                  sleep(100);

                end;
              end;

            Except
              On E:Exception do begin
                DebugLn('Error '+E.ClassName+':'+E.Message);
              end;
            End;
            sleep(10);
          until Done=True;
        end;
      end;
  fSock.Free;

  FStatus := TaskCompleted;
end;

procedure TWorkerThread.RegisterConnection(AConnection : PConnection);
begin
  FStatus := TaskRunning;
  //DebugLn('Entered RegisterConnection Task in thread' + IntToStr(ID));
  FStatus := TaskCompleted;
end;


procedure TWorkerThread.OpenConnection(AConnection : PConnection);
begin


end;

procedure TWorkerThread.SendData( AConnection : PConnection);
begin

end;

procedure TWorkerThread.CloseConnection(AConnection : PConnection);
begin


end;


procedure TWorkerThread.RemoveConnection(AConnection : PConnection);
begin

end;


procedure TWorkerPool.ThreadAvailable;
begin
  if Assigned(FOnThreadAvailable) then
    begin
      //DebugLn('EstablishConnection set up');
      FOnThreadAvailable(FAvailableThread);
    end;
end;

procedure TWorkerThread.ExecuteTask(ATask : WorkerTaskType);
begin
  //DebugLn('Task of conn is ' + GetEnumName(TypeInfo(WorkerTaskType),Ord(ATask)   )  + 'to thread' + IntTOStr(ID));

  if ATask = WorkerTaskType.EstablishConnection then begin
    DebugLn('TAskkk');
     EstablishConnection(Connection);
  end
  else if ATask = WorkerTaskType.RegisterConnection then begin
     RegisterConnection(Connection);
  end
  else if ATask = WorkerTaskType.OpenConnection then begin
     OpenConnection(Connection);
  end
  else if ATask = WorkerTaskType.CloseConnection then begin
     CloseConnection(Connection);
  end
  else if ATask = WorkerTaskType.SendData then begin
     SendData(Connection);
  end
  else if ATask = WorkerTaskType.RemoveConnection then begin
     RemoveConnection(Connection);
  end;
end;

procedure TWorkerThread.Execute;
var
  Message: String;
begin
  DebugLn('TWorkerThread ' + IntTOStr(ID) + ' is alive!');
  while (not Terminated) do
    begin
      if FStatus = TaskAssigned then
        begin
          ExecuteTask(FTask);
        end
      else if FStatus = TaskCompleted then
        begin
          ExecuteTask(FTask);
        end;


      sleep(50);
    end;
  DebugLn('TWorkerThread termination; should not happen');
  {fSock:=TTCPBlockSocket.create;

  fSock.socket:=SocketHandle;
  PrintSettings;
  while (not Terminated) do
    begin


      Message:= fSock.RecvString(2000);
      DebugLn(Message);
      fsock.SendString('responded'#13#10);
      sleep(1000);
    end;
  fSock.Free;    }
  inherited Destroy;
end;

{procedure TWorkerThread.ExecuteContract;
var
  Message: String;
begin
  while (not Terminated) do
    begin

      ExecuteMethod(@);

    end;

  inherited Destroy;
end;    }



constructor TWorkerPool.Create(CreateSuspended : Boolean = True ; DestroyAfterExit : Boolean = True; NumberOfThreads : Cardinal = 6 );
var I : Integer;
    tempThread : TWorkerThread;
begin
   inherited Create;

   DebugLn('Creating TWorkerPool');
   ThreadsCount:= NumberOfThreads;

   for I := 0 to ThreadsCount - 1  do begin
      //Randomize;
      tempThread := AddNewThread^;
      DebugLn('Added thread with ID' + (tempThread.SecureName));
      DebugLn('Added thread with ID' + IntTOStr((tempThread.ID)));
    end;

end;

procedure TWorkerPool.Init(CreateSuspended : Boolean ; DestroyAfterExit : Boolean);
var I : Integer;
  tempP : PWorkerThread;
  res : Integer;
  TWT : TWorkerThread ;
  PWT : PWorkerThread;
  toBeAdded : TWorkerThread;
  PtoBeAdded : PWorkerThread;
begin

   {Done Creating threads}

   //Randomize;
   DebugLn('Init TWorkerPool');

   PWT := Last;
   //DebugLn('Last thread added ' + TWT.Name);
   DebugLn('Checking first' + IntTOStr((PWT^.ID)));
   DebugLn('Checking first ' + PWT^.Name);

   for I := 1 to Count - 1 do begin
     PWorkerThread(Items[I])^.Start ;

     DebugLn('Starting ThreadWorker no ' + IntToStr(I) + 'ThreadId' + IntToStr(PWorkerThread(Items[I])^.ID));

     tempP := @Items[I]^;
     DebugLn('Starting ThreadWorker no ' + IntToStr(I) + 'ThreadId' + tempP^.Name);
     //tempP^.ThreadAvailable;
     FAvailableThread := @Items[I]^;
     ThreadAvailable;
   end;


end;

procedure TWorkerPool.CoupleComponent( AComponent : TObject);
var I: Integer;
  tempThread: TWorkerThread;
begin
  if Assigned(FComponent) then begin
    DebugLn('WorkerPool already coupled');
    Exit;
  end;
  FComponent := AComponent;

  for I := 1 to Count - 1 do begin

      DebugLn('trhead' + IntToStr(I) );

      tempThread := GetItem(I)^;

      DebugLn('ThreadIDd  ' + IntToStr(tempThread.ID));

      DoOnThreadAvailable := @TTaskManager(AComponent).NotifyThreadAvailable;
   end;
end;

function TWorkerPool.AddNewThread:PWorkerThread;
  var WorkerThread : TWorkerThread;
  begin
    //Randomize;
    DebugLn('Count1:' + IntToStr(Count));
    // resize the worker stack; this is where the objects are created and later referenced inside the TWorkerPool's inherited list of pointers for quick access
    SetLength(WorkerStack,( Count + 1 ));

    WorkerStack [Count] := TWorkerThread.Create(True,True);
    DebugLn(WorkerStack[Count].Name);
    DebugLn(IntToStr(WorkerStack[Count].ID));
    //DebugLn('Creating new PWorkerThread with threadID');

    Result := AddNew(@WorkerStack [Count]);
    DebugLn('Count2:' + IntToStr(Count));
  end;

function TWorkerPool.AddNew( AThread : PWorkerThread) : PWorkerThread;
  var Res : Integer;
    PAthread:pointer;
    PItems:pointer;
    PResult:pointer;
  begin
    //TODO : Error checking for Add;
    //DebugLn('Adding PWorkerThread to PWorkerPool');
    Res := Add(AThread);
    DebugLn('Result of adding:' + IntTOStr(Res));
    Result := Items[Count-1];

    pathread := @ATHread;
    pitems:= Items[Count-1];
    presult := @Result;

    DebugLn( Format('Address of funcVar: %p  Addres of Items[]: %p  Addres of Result : %p', [pathread, pitems,presult ]) );
    DebugLn( Format('Address StoredIN funcVar: %p  Addres StoredIN Items[]: %p  Addres StoredIN Result : %p', [AThread, Items[Count-1], Result ]) );


  end;

function TWorkerPool.GetItem (Index:Integer): PWorkerThread;
var PH : PWorkerThread;
begin

   PH := Items[Index];

   DebugLn('Index' + IntToStr(Index));
   Result := PH;
   DebugLn( Format('Address SI funcVar: %p  Addres SI Items[]: %p  Addres SI Result : %p', [PH, @Items[Index]^,Result ]) );
end;

procedure TWorkerPool.DecoupleComponent ( AComponent : TObject);
var I : Integer;
begin
  if Assigned(FComponent) = False then DebugLn('WorkerPool not coupled');

   for I := 1 to Count - 1 do begin
     //PWorkerThread(Items[I])^.DoOnThreadAvailable := Nil;
   end;
end;

procedure TTaskManager.NotifyThreadAvailable(AThread : PWorkerThread);
var tt1:PWorkerTHread;
    tt2:Pointer;
begin
   DebugLn('Notified');
   tt1 := FThreads.Push(AThread);
   //tt2 := FThreads.Pop;

   DebugLn('added Worker thread with id: ' + IntTOStr(PWorkerThread(tt1)^.ID));
  //DebugLn('added Worker thread with id: ' + IntTOStr(PWorkerThread(tt2)^.ID));
end;

procedure TTaskManager.NotifyMethodToBeExecuted(AConnection : PConnection);
var tt:Pointer;
  tempConnection : PConnection;
begin
  //check the thread queue not empty -> there is at least one available thread
  DebugLn('NotifyMethodToBeExecuted 2');
  DebugLn('Can print FID ' + INtToStr(FID));
  tt := FConnections.Push(AConnection);
end;

procedure TTAskManager.AssignTask( AThread : PWorkerThread ; AConnection : PConnection; ATask : WorkerTaskType);
begin
  DebugLn('Assigned task ' + GetEnumName(TypeInfo(WorkerTaskType),Ord(ATask)   )  + 'to thread' + IntTOStr(AThread^.ID));
  AThread^.SetTask(ATask);
  AThread^.SetStatus(TaskAssigned);
  AThread^.SetConnection(AConnection);
end;

procedure TTAskManager.ScheduleTask( AThread : PWorkerThread ; AConnection : PConnection; ATask : WorkerTaskType);
begin
  //Obsolete
  DebugLn('Scheduled task ' + GetEnumName(TypeInfo(WorkerTaskType),Ord(ATask)   )  + 'to thread' + IntToStr(AThread^.ID));
  FConnections.Push(AConnection);
  FThreads.Push(AThread);

end;

constructor TTaskManager.Create(CreateSuspended : Boolean = True; DestroyAfterExecution: Boolean = True);
var I:Integer;
  tempTH: PWorkerTHread;
begin
  //create the thread
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;

  // create locks for threads and connections(queues)
  ConnectionsLock := TCriticalSection.Create;
  ThreadsLock := TCriticalSection.Create;

  //Randomize;
  FID := Random(50);

  FThreads := TSyncQueue.Create;
  FConnections := TSyncQueue.Create;

  Debugln('TTaskManager created with FID ' + IntToStr(FID));

  //this will also create all the threads
  FWorkerPool := TWorkerPool.Create(True,True,6);

  FWorkerPool.CoupleComponent(Self);


  FconnectionPool := TConnectionPool.Create;
  FConnectionPool.Couple(Self); // couple connection pool with task manager

end;

procedure TTaskManager.Start;
var tt : Pointer;
  pt : PWorkerThread;
begin
  inherited Start;
  FWorkerPool.Init(True,True);

  tt := FWorkerPool.WorkerStack[1];
  tt := @(FWorkerPool.Items[1])^;
  pt := FThreads.Pop;

  DebugLn('Testing FTHreads w thread count  ' + IntTOstr(FThreads.Count));
  //tt := Fthreads.Pop;
  DebugLn('Testing FTHreads no ' + IntTOstr(PWorkerTHread(tt)^.ID));
  DebugLn('Testing FTHreads no ' + IntTOstr( PWorkerTHread(FWorkerPool.Items[1])^.ID ));

  DebugLn('Testing FTHreads name ' + PWorkerTHread(FWorkerPool.Items[1])^.SecureName);
  DebugLn('Testing FTHreads name ' + PWorkerTHread(pt)^.SecureName);
  DebugLn('Testing FTHreads name ' + PWorkerTHread(tt)^.Name);
  DebugLn('Testing FTHreads w thread count  ' + IntTOstr(FThreads.Count));

end;

procedure TTaskManager.Execute;
var
  Message: String;
  TempCon : PConnection;
  TempThread : PWorkerThread;
  tt:PWorkerThread;
begin

  while (not Terminated) do
    begin
      //DebugLn('Contains : ' + IntToStr(Connections.Count) + 'elements');
      if FConnections.AtLeast(1) then
      begin
        DebugLn('At least one Connection!!!');

        if FThreads.AtLeast(1) then
        begin
           DebugLn('At least one available thread!!!');
           TempCon := FConnections.Pop;

           tt := FThreads.Pop;
           DebugLn('PoppedThreadID' + IntTOstR(tt^.ID));
           DebugLn('Connection' + tempCOn^.RemoteIP);
           AssignTask(PworkerThread(tt),TempCon,TempCon^.TaskType);
        end;
      end;
      sleep(100);
    end;
  DebugLn('TTaskManager termination; should not happen');

  inherited Destroy;
end;

procedure TTaskManager.SetConnections ( SConnections : TSyncQueue);
begin
   ConnectionsLock.Acquire;
   FConnections := SConnections;
   ConnectionsLock.Release;
end;

function TTaskManager.GetConnections : TSyncQueue;
begin
   ConnectionsLock.Acquire;
   Result := FConnections;
   ConnectionsLock.Release;
end;

procedure TTaskManager.SetThreads ( SThreads : TSyncQueue);
begin
   THreadsLock.Acquire;
   FThreads := Sthreads;
   THreadsLock.Release;
end;

function TTaskManager.GetThreads : TSyncQueue;
begin
  ThreadsLock.Acquire;
  Result := FThreads;
  THreadsLock.Release;
end;


{TSyncQueue}

Function TSyncQueue.AtLeast(ACount: Integer): Boolean;
begin
  //DebugLn('Called');
  Lock.Acquire;
  Result:=(FList.Count>=Acount);
  Lock.Release;
end;

Function TSyncQueue.Count: Integer;
begin
  Lock.Acquire;
  Result:=FList.Count;
  Lock.Release;
end;

constructor TSyncQueue.Create;
begin
  FList:=Tlist.Create;
  Lock := TCriticalSection.Create;
end;

destructor TSyncQueue.Destroy;
begin
  Lock.Acquire;
  FList.Free;
  Lock.Release;
  Lock.Destroy;
end;

Function TSyncQueue.Pop: Pointer;
begin
  If Atleast(1) then
  begin
    Lock.Acquire;
    Result:=PopItem;
    Lock.Release;
  end
  else
    Result:=nil;
end;


Function TSyncQueue.Push(AItem: Pointer): Pointer;
begin
  DebugLn('Added to queue');
  Lock.Acquire;
  PushItem(AItem);
  Lock.Release;
  Result:=AItem;
end;


Procedure TSyncQueue.PushItem(AItem: Pointer);
begin
  Lock.Acquire;
  FList.Add(AItem);
  Lock.Release;
end;

Function TSyncQueue.PeekItem: Pointer;
begin
  Lock.Acquire;
  with Flist do
    Result:=Items[Count-1] ;

  Lock.Release;
end;


Function TSyncQueue.PopItem: Pointer;
begin
 // Lock.Acquire;
  with FList do
    if Count>0 then
      begin
      Result:=Items[Count-1];
      Delete(Count-1);
      end
    else
      Result:=nil;
  //Lock.Release;
end;

end.


