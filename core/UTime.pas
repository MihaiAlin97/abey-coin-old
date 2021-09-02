unit UTime;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

Uses
{$IFnDEF FPC}
  System.DateUtils,
{$ELSE}
  {LCLIntf, LCLType, LMessages,}
{$ENDIF}
  SysUtils;

Function DateTime2UnivDateTime(d:TDateTime):TDateTime;
Function UnivDateTime2LocalDateTime(d:TDateTime):TDateTime;

function UnivDateTimeToUnix(dtDate: TDateTime): Longint;
function UnixToUnivDateTime(USec: Longint): TDateTime;

function UnixTimeToLocalElapsedTime(USec : Longint) : String;
Function DateTimeElapsedTime(dtDate : TDateTime) : String;

Function UnixTimeToLocalStr(UnixTime : Longint) : String;

implementation

{$IFDEF FPC}
Uses DateUtils;
{$ENDIF}

const
    UnixStartDate: TDateTime = 25569.0; // 01/01/1970

function UnixTimeToLocalElapsedTime(USec : Longint) : String;
Var diff, positivediff : Longint;
Begin
  diff := UnivDateTimeToUnix(DateTime2UnivDateTime(now)) - Usec;
  if diff<0 then positivediff := diff * (-1)
  else positivediff := diff;
  
  diff := positivediff;

  if positivediff<60 then Result := inttostr(diff)+' seconds ago'
  else if positivediff<(60*2) then Result := '1 minute ago'
  else if positivediff<(60*60) then Result := inttostr(diff DIV 60)+' minutes ago'
  else if positivediff<(60*60*2) then Result := '1 hour ago'
  else if positivediff<(60*60*24) then Result := inttostr(diff DIV (60*60))+' hours ago'
  else Result := inttostr(diff DIV (60*60*24))+' days ago';
End;

Function DateTimeElapsedTime(dtDate : TDateTime) : String;
Begin
  Result := UnixTimeToLocalElapsedTime( UnivDateTimeToUnix(DateTime2UnivDateTime(dtDate)) );
End;

Function DateTime2UnivDateTime(d:TDateTime):TDateTime;
{$IFDEF FPC}
begin
  Result := LocalTimeToUniversal(d,-GetLocalTimeOffset);
end;
{$ELSE}
begin
  Result := TTimeZone.Local.ToUniversalTime(d);
end;
{$ENDIF}

Function UnivDateTime2LocalDateTime(d:TDateTime):TDateTime;
{$IFDEF FPC}
begin
  Result := UniversalTimeToLocal(d,-GetLocalTimeOffset);
end;
{$ELSE}
begin
  Result := TTimeZone.Local.ToLocalTime(d);
end;
{$ENDIF}

function UnivDateTimeToUnix(dtDate: TDateTime): Longint;
begin
  Result := Round((dtDate - UnixStartDate) * 86400);
end;

function UnixToUnivDateTime(USec: Longint): TDateTime;
begin
  Result := (Usec / 86400) + UnixStartDate;
end;

Function UnixTimeToLocalStr(UnixTime : Longint) : String;
begin
  Result := DateTimeToStr(UnivDateTime2LocalDateTime(UnixToUnivDateTime(UnixTime)))
end;

end.
