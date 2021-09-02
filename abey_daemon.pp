program abey_daemon;

{$mode objfpc}{$H+}
{$define usecthreads}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  sysutils,
  Classes, daemonapp,
  UCrypto, upcdaemon;

begin
  Application.Title:='ABEY Service';
  RegisterDaemonClass(TABEYService);
  RegisterDaemonMapper(TABEYDaemonMapper);
  TCrypto.InitCrypto;
  Application.Run;
end.
