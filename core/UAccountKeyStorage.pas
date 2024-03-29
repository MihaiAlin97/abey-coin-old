unit UAccountKeyStorage;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, UAccounts, UThread, UBaseTypes,
  {$IFNDEF FPC}System.Generics.Collections{$ELSE}Generics.Collections{$ENDIF};

type
  TAccountKeyStorageData = record
    ptrAccountKey : PAccountKey;
    counter : Integer;
  end;
  PAccountKeyStorageData = ^TAccountKeyStorageData;

  { TAccountKeyStorage }

  // This class reduces memory because allows to reuse account keys
  // Based on tests, allows a 10-20% memory reduction when multiple accounts use the same Account key
  TAccountKeyStorage = Class
  private
    FAccountKeys : TABEYThreadList<Pointer>;
    Function Find(list : TList<Pointer>; const accountKey: TAccountKey; out Index: Integer): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function AddAccountKey(Const accountKey: TAccountKey) : PAccountKey;
    function AddAccountKeyExt(Const accountKey: TAccountKey) : PAccountKeyStorageData;
    procedure RemoveAccountKey(Const accountKey: TAccountKey);
    class function KS : TAccountKeyStorage;
    function LockList : TList<Pointer>;
    procedure UnlockList;
  end;

implementation

uses
  ULog;

var _aks : TAccountKeyStorage = Nil;

{ TAccountKeyStorage }

function TAccountKeyStorage.Find(list : TList<Pointer>; const accountKey: TAccountKey; out Index: Integer): Boolean;
var L, H, I: Integer;
  C : Integer;
begin
  Result := False;
  L := 0;
  H := list.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := Integer(PAccountKeyStorageData(list[i])^.ptrAccountKey^.EC_OpenSSL_NID) - Integer(accountKey.EC_OpenSSL_NID);
    if C=0 then begin
      C := TBaseType.BinStrComp(PAccountKeyStorageData(list[i])^.ptrAccountKey^.x,accountKey.x);
      if C=0 then begin
        C := TBaseType.BinStrComp(PAccountKeyStorageData(list[i])^.ptrAccountKey^.y,accountKey.y);
      end;
    end;
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  Index := L;
end;

constructor TAccountKeyStorage.Create;
begin
  FAccountKeys := TABEYThreadList<Pointer>.Create('TAccountKeyStorage');
end;

destructor TAccountKeyStorage.Destroy;
Var l : TList<Pointer>;
  i : Integer;
  P1 : PAccountKeyStorageData;
  P2 : PAccountKey;
begin
  l := FAccountKeys.LockList;
  try
    For i:=0 to l.Count-1 do begin
      P1 := l[i];
      P2 := P1^.ptrAccountKey;
      Dispose(P1);
      Dispose(P2);
    end;
    l.Clear;
  finally
    FAccountKeys.UnlockList;
  end;
  FreeAndNil(FAccountKeys);
  inherited Destroy;
end;

function TAccountKeyStorage.AddAccountKey(const accountKey: TAccountKey): PAccountKey;
var l : TList<Pointer>;
  i : Integer;
  P : PAccountKeyStorageData;
begin
  Result := Nil;
  l := FAccountKeys.LockList;
  try
    If Find(l,accountKey,i) then begin
      Result := PAccountKeyStorageData(l[i]).ptrAccountKey;
      inc( PAccountKeyStorageData(l[i]).counter );
    end else begin
      New(P);
      New(P^.ptrAccountKey);
      P^.counter:=1;
      P^.ptrAccountKey^:=accountKey;
      Result := P^.ptrAccountKey;
      l.Insert(i,P);
    end;
  finally
    FAccountKeys.UnlockList;
  end;
end;

function TAccountKeyStorage.AddAccountKeyExt(const accountKey: TAccountKey): PAccountKeyStorageData;
  // This function will return allocated pointer and will increase counter like AddAccountKey
var l : TList<Pointer>;
  i : Integer;
begin
  Result := Nil;
  l := FAccountKeys.LockList;
  try
    If Find(l,accountKey,i) then begin
      Result := PAccountKeyStorageData(l[i]);
      inc(Result^.counter);
    end else begin
      New(Result);
      New(Result^.ptrAccountKey);
      Result^.counter:=1;
      Result^.ptrAccountKey^:=accountKey;
      l.Insert(i,Result);
    end;
  finally
    FAccountKeys.UnlockList;
  end;
end;

procedure TAccountKeyStorage.RemoveAccountKey(const accountKey: TAccountKey);
var l : TList<Pointer>;
  i : Integer;
  P : PAccountKeyStorageData;
begin
  l := FAccountKeys.LockList;
  try
    If Find(l,accountKey,i) then begin
      P := PAccountKeyStorageData(l[i]);
      dec( P^.counter );
      If P^.counter<0 then begin
        TLog.NewLog(lterror,Self.ClassName,'ERROR DEV 20171110-2');
      end;
    end else begin
      TLog.NewLog(lterror,Self.ClassName,'ERROR DEV 20171110-1');
    end;
  finally
    FAccountKeys.UnlockList;
  end;
end;

class function TAccountKeyStorage.KS: TAccountKeyStorage;
begin
  if Not Assigned(_aks) then begin
    _aks := TAccountKeyStorage.Create;
  end;
  Result := _aks;
end;

function TAccountKeyStorage.LockList: TList<Pointer>;
begin
  Result := FAccountKeys.LockList;
end;

procedure TAccountKeyStorage.UnlockList;
begin
  FAccountKeys.UnlockList;
end;

initialization
  _aks := Nil;
finalization
  FreeAndNil(_aks);
end.

