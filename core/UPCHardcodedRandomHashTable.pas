unit UPCHardcodedRandomHashTable;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I config.inc}

interface

uses
  SysUtils, Classes,
  UCrypto, UBaseTypes, UPCDataTypes, UConst,
  {$IFNDEF FPC}System.Generics.Collections{$ELSE}Generics.Collections{$ENDIF};


type
  TABEYHardcodedRandomHashTable = Class
    {
     This object will store an ordered list of pairs <Digest><RandomHash>
     ordered by <Digest> for quick searching.
     Is usefull to store Hardcoded values of RandomHash

     For security reasons, this object data will be stored in a secured Stream
     (For example a file) where last value of the stream is a SHA256 hash
     of the previous content.

     The "LoadFromStream" function will return TRUE only if the SHA256 hash
     stored in the Stream matches the loaded data, preventing corruption

     Also, an external App can hardcode this SHA256 value (obtained
     in a call to GetHardcodedSha256) for securize this usage

     }
    Type
    TRow = Record
      RandomHashValue : T32Bytes;
      DigestValue : TRawBytes;
    End;
  private
    FList : TList<TRow>;
    FHardcodedSha256: TRawBytes;
    function Find(const ADigest : TRawBytes; var AIndex : Integer) : Boolean;
    function GetRandomHash(AIndex : Integer) : TRawBytes;
    function GetRow(AIndex : Integer) : TRow;
    procedure Clear;
    procedure SaveToStreamWithoutHardcodedSha256(AStream : TStream);
  public
    constructor Create;
    destructor Destroy; override;
    function LoadFromStream(AStream : TStream; var AHardcodedSha256 : TRawBytes) : Boolean;
    procedure SaveToStream(AStream : TStream);
    procedure AddRandomHash(const ARandomHash, ADigest : TRawBytes; ACheckIntegrity : Boolean);
    function Count : Integer;
    function FindRandomHashByDigest(const ADigest : TRawBytes; var ARandomHash : TRawBytes) : Boolean;
    function GetHardcodedSha256 : TRawBytes;  // Will return a SHA256 of the object content for hardcode external apps
    procedure CopyFrom(ASource : TABEYHardcodedRandomHashTable);
  End;

implementation

uses UAccounts;

const
  CT_MagicValue_Stream_Header = 'TABEYHardcodedRandomHashTable';
  CT_MagicValue_Stream_Version = 1;

{ TABEYHardcodedRandomHashTable }

procedure TABEYHardcodedRandomHashTable.AddRandomHash(const ARandomHash, ADigest: TRawBytes; ACheckIntegrity: Boolean);
var i,j, LMemPos_0, LMemPos_1 : Integer;
  LRaw : TRawBytes;
  LRow : TRow;
  LRowPtr : Pointer;
  LRowPosition : Int64;
begin
  if ACheckIntegrity then begin
    LRaw := TCrypto.DoRandomHash(ADigest);
    if TBaseType.BinStrComp(LRaw,ARandomHash)<>0 then raise Exception.Create('RandomHash(Digest) & Provided Hash value does not match');
  end;
  if Length(ARandomHash)<>32 then raise Exception.Create('Invalid RandomHash length<>32');

  if Find(ADigest,i) then begin
    if TBaseType.BinStrComp(GetRandomHash(i),ARandomHash)<>0 then raise Exception.Create('Digest found with another RandomHash value');
  end else begin
    FHardcodedSha256 := Nil;

    LRow.RandomHashValue := TBaseType.To32Bytes(ARandomHash);
    LRow.DigestValue := ADigest;
    FList.Insert(i,LRow);
  end;
end;

procedure TABEYHardcodedRandomHashTable.Clear;
begin
  FList.Clear;
  FHardcodedSha256 := Nil;
end;

procedure TABEYHardcodedRandomHashTable.CopyFrom(ASource: TABEYHardcodedRandomHashTable);
var i, iIndex : Integer;
begin
  if ASource=Self then Exit;
  for i := 0 to ASource.Count-1 do begin
    if Not Find(ASource.FList[i].DigestValue,iIndex) then begin
      FList.Insert(iIndex,ASource.FList[i]);
      FHardcodedSha256 := Nil;
    end;
  end;
end;

function TABEYHardcodedRandomHashTable.Count: Integer;
begin
  Result := FList.Count;
end;

constructor TABEYHardcodedRandomHashTable.Create;
begin
  FList := TList<TRow>.Create;
  FHardcodedSha256 := Nil;
end;

destructor TABEYHardcodedRandomHashTable.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

function TABEYHardcodedRandomHashTable.Find(const ADigest : TRawBytes; var AIndex : Integer) : Boolean;
var L, H, I: Integer;
  C : Int64;
  LPtr : PByte;
  LRowDigest : TRawBytes;
begin
  Result := False;
  L := 0;
  H := Count-1;
  while L <= H do
  begin
    I := (L + H) shr 1;

    LRowDigest := GetRow(I).DigestValue;

    C := TBaseType.BinStrComp(LRowDigest,ADigest);
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
  AIndex := L;
end;

function TABEYHardcodedRandomHashTable.FindRandomHashByDigest(const ADigest: TRawBytes; var ARandomHash: TRawBytes): Boolean;
var i : Integer;
begin
  if Find(ADigest,i) then begin
    ARandomHash := TBaseType.T32BytesToRawBytes( FList.Items[i].RandomHashValue );
    Result := True;
  end else begin
    Result := False;
    SetLength(ARandomHash,0);
  end;
end;

function TABEYHardcodedRandomHashTable.GetHardcodedSha256: TRawBytes;
var LMemStream : TMemoryStream;
begin
  if Length(FHardcodedSha256)<>32 then begin
    LMemStream := TMemoryStream.Create;
    try
      SaveToStreamWithoutHardcodedSha256(LMemStream);
      FHardcodedSha256 := TCrypto.DoSha256( PAnsiChar(LMemStream.Memory), LMemStream.Size );
    finally
      LMemStream.Free;
    end;
  end;
  Result := FHardcodedSha256;
end;

function TABEYHardcodedRandomHashTable.GetRandomHash(AIndex: Integer): TRawBytes;
begin
  Result := TBaseType.ToRawBytes( GetRow(AIndex).RandomHashValue );
end;

function TABEYHardcodedRandomHashTable.GetRow(AIndex: Integer): TRow;
begin
  Result := FList.Items[AIndex];
end;

function TABEYHardcodedRandomHashTable.LoadFromStream(AStream: TStream; var AHardcodedSha256 : TRawBytes): Boolean;
var i : Integer;
  LRaw, LRaw2 : TRawBytes;
  LTotalRows : UInt32;
  LVersion : Word;
begin
  Clear;
  TStreamOp.ReadAnsiString(AStream,LRaw);
  LRaw2.FromString(CT_MagicValue_Stream_Header);
  Result := False;
  if (Not TBaseType.Equals(LRaw,LRaw2)) then begin
    Exit(False);
  end;
  AStream.Read(LVersion,2);
  if Not LVersion=CT_MagicValue_Stream_Version then Exit(False);

  AStream.Read(LTotalRows,4);
  for i := 1 to LTotalRows do begin
    if TStreamOp.ReadAnsiString(AStream,LRaw)<0 then Exit(False); // RandomHash value
    if TStreamOp.ReadAnsiString(AStream,LRaw2)<0 then Exit(False);// Digest value
    AddRandomHash(LRaw,LRaw2,False);
  end;

  // Last value must be a
  if TStreamOp.ReadAnsiString(AStream,AHardcodedSha256)<0 then Exit(False);
  if TBaseType.BinStrComp(AHardcodedSha256,GetHardcodedSha256)<>0 then Exit(False);

  Result := True;
end;

procedure TABEYHardcodedRandomHashTable.SaveToStream(AStream: TStream);
begin
  SaveToStreamWithoutHardcodedSha256(AStream);

  // Last value to the Stream must be
  TStreamOp.WriteAnsiString(AStream,GetHardcodedSha256);
end;

procedure TABEYHardcodedRandomHashTable.SaveToStreamWithoutHardcodedSha256(AStream: TStream);
var LVersion : Word;
  LRaw : TRawBytes;
  LTotalRows : UInt32;
  i : Integer;
begin
  LRaw.FromString(CT_MagicValue_Stream_Header);
  TStreamOp.WriteAnsiString(AStream,LRaw);
  LVersion := CT_MagicValue_Stream_Version;
  AStream.Write(LVersion,2);
  //
  LTotalRows := Count;
  AStream.Write(LTotalRows,4);
  for i := 0 to Count-1 do begin
    LRaw := TBaseType.ToRawBytes(FList[i].RandomHashValue);
    TStreamOp.WriteAnsiString(AStream,LRaw);
    TStreamOp.WriteAnsiString(AStream,FList[i].DigestValue);
  end;
end;

end.
