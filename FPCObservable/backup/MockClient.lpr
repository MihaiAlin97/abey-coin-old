program MockClient;

uses
    blcksock,lazlogger,SysUtils;

var
    sock: TTCPBlockSocket;

procedure Main();
var
    buffer: String = '';
begin
    sock := TTCPBlockSocket.Create;

    sock.Connect('127.0.0.1', '6069');
    // Was there an error?
    if sock.LastError <> 0 then
    begin
        writeLn('Could not connect to server.');
        halt(1);
    end;
    // Send a HTTP request
    sock.SendString('nenea'+ CRLF);

    // Keep looping...
     buffer := sock.RecvString(2000);
    DebugLn('Response' + Buffer);
end;


begin
    Main();
end.
