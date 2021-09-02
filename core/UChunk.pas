unit UChunk;

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  {$IFDEF FPC}
    // NOTE:
    // Due to FreePascal 3.0.4 (and earlier) bug, will not use internal "paszlib" package, use modified instead
    {$IFDEF VER3_2}
      zStream, // <- Not used in current FreePascal v3.0.4 caused by a bug: https://bugs.freepascal.org/view.php?id=34422
    {$ELSE}
      paszlib_zStream,
    {$ENDIF}
  {$ELSE}
  zlib,
  {$ENDIF}
  UAccounts, ULog, UConst, UCrypto, UBaseTypes;

type

  { TABEYChunk }

  TABEYChunk = Class
  private
  public
    class function SaveVaultChunkFromVault(VaultStream, DestStream : TStream; fromBlock, toBlock : Cardinal; var errors : String) : Boolean;
    class function LoadVaultFromChunk(Chunk, DestStream : TStream; var VaultHeader : TABEYVaultHeader; var errors : String) : Boolean;
  end;

implementation

{ TABEYChunk }

class function TABEYChunk.SaveVaultChunkFromVault(VaultStream, DestStream : TStream; fromBlock, toBlock: Cardinal; var errors : String) : Boolean;
Var
  c: Cardinal;
  cs : Tcompressionstream;
  auxStream : TStream;
  iPosSize, iAux : Int64;
  initialSbPos : Int64;
  sbHeader : TABEYVaultHeader;
begin
  Result := false; errors := '';
  // Chunk struct:
  // - Header:
  //   - Magic value  (fixed AnsiString)
  //   - Vault version (2 bytes)
  //   - Uncompressed size (4 bytes)
  //   - Compressed size (4 bytes)
  // - Data:
  //   - Compressed data using ZLib
  initialSbPos :=VaultStream.Position;
  Try
    If Not TABEYVault.LoadVaultStreamHeader(VaultStream,sbHeader) then begin
      errors := 'VaultStream is not a valid Vault!';
      exit;
    end;
    If (sbHeader.startBlock>fromBlock) Or (sbHeader.endBlock<ToBlock) Or (fromBlock>toBlock) then begin
      errors := Format('Cannot save a chunk from %d to %d on a stream with %d to %d!',[fromBlock,toBlock,sbHeader.startBlock,sbHeader.endBlock]);
      exit;
    end;
    TLog.NewLog(ltDebug,ClassName,Format('Saving Vault chunk from %d to %d (current blockscount: %d)',[FromBlock,ToBlock,sbHeader.blocksCount]));

    // Header:
    TStreamOp.WriteAnsiString(DestStream,TEncoding.ASCII.GetBytes(CT_VaultChunkIdentificator));
    DestStream.Write(CT_VaultBankVersion,SizeOf(CT_VaultBankVersion));
    //
    auxStream := TMemoryStream.Create;
    try
      VaultStream.Position:=initialSbPos;
      If Not TABEYVault.CopyVaultStream(VaultStream,auxStream,fromBlock,toBlock,errors) then exit;
      auxStream.Position:=0;
      // Save uncompressed size
      c := auxStream.Size;
      DestStream.Write(c,SizeOf(c));
      // Save compressed size ... later
      iPosSize := DestStream.Position;
      c := $FFFFFFFF;
      DestStream.Write(c,SizeOf(c)); // Save 4 random bytes, latter will be changed
      //
      // Zip it and add to Stream
      cs := Tcompressionstream.create(clFastest,DestStream);
        // Note: Previously was using clDefault, but found a bug for FreePascal 3.0.4
        // https://bugs.freepascal.org/view.php?id=34422
        // On 2018-10-15 changed clDefault to clFastest
      try
        cs.CopyFrom(auxStream,auxStream.Size); // compressing
      finally
        cs.Free;
      end;
    finally
      auxStream.Free;
    end;
    //
    iAux := DestStream.Position;
    c := DestStream.Position - iPosSize - 4; // Save data size
    DestStream.Position:=iPosSize;
    DestStream.Write(c,SizeOf(c));
    DestStream.Position := iAux; // Back to last position
    Result := True; errors := '';
  finally
    VaultStream.Position:=initialSbPos;
  end;
end;

class function TABEYChunk.LoadVaultFromChunk(Chunk, DestStream: TStream; var VaultHeader : TABEYVaultHeader; var errors: String): Boolean;
var raw : TRawBytes;
  w : Word;
  cUncompressed, cCompressed : Cardinal;
  ds : Tdecompressionstream;
  dbuff : Array[1..2048] of byte;
  r : Integer;
  destInitialPos, auxPos : Int64;
begin
  Result := false;
  VaultHeader := CT_PCVaultHeader_NUL;
  // Header:
  errors := 'Invalid stream header';
  TStreamOp.ReadAnsiString(Chunk,raw);
  If (Not TBaseType.Equals(raw,TEncoding.ASCII.GetBytes(CT_VaultChunkIdentificator))) then begin
    exit;
  end;
  Chunk.Read(w,sizeof(w));
  if (w<>CT_VaultBankVersion) then begin
    errors := errors + ' Invalid version '+IntToStr(w);
    exit;
  end;
  // Size
  Chunk.Read(cUncompressed,SizeOf(cUncompressed)); // Uncompressed size
  Chunk.Read(cCompressed,SizeOf(cCompressed)); // Compressed size
  if (Chunk.Size - Chunk.Position < cCompressed) then begin
    errors := Format('Not enough LZip bytes Stream.size:%d Stream.position:%d (avail %d) LZipSize:%d',[Chunk.Size,Chunk.Position,Chunk.Size - Chunk.Position,cCompressed]);
    exit;
  end;
  //
  destInitialPos:=DestStream.Position;
  ds := Tdecompressionstream.create(Chunk);
  try
    repeat
      r := ds.read(dbuff,SizeOf(dbuff));
      if (r>0) then begin
        DestStream.Write(dbuff,r);
      end;
    until r < SizeOf(dbuff);
    //auxStream.CopyFrom(Stream,cCompressed);
  finally
    ds.Free;
  end;
  If (DestStream.Size-destInitialPos)<>cUncompressed then begin
    errors := Format('Uncompressed size:%d <> saved:%d',[(DestStream.Size-destInitialPos),cUncompressed]);
    exit;
  end;

  auxPos := DestStream.Position;
  DestStream.Position:=destInitialPos;
  If Not TABEYVault.LoadVaultStreamHeader(DestStream,VaultHeader) then begin
    errors:= 'Invalid extracted stream!';
    exit;
  end;
  DestStream.Position:=auxPos;
  Result := true;
end;

end.

