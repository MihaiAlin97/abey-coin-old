program ObservableServer;
{$mode objfpc}{$H+}

uses
  {$ifdef unix}cthreads,{$endif}
  UPublisher,lazlogger,SysUtils,Classes,SyncObjs ;//UListener;

var CM:TPublisher;


begin

  GlobalCanReadCritSection :=

  Randomize;
  CM := TPublisher.Create;
  while True do begin
    sleep(1000);
    CheckSynchronize;
  end;



end.
