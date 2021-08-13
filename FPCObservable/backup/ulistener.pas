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

end.

