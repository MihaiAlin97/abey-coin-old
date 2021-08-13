{
** Thread-safe ABEY binary database storage system **
Copyright (c) 2020 Dipl. Eng. Ciprian Pungila, MSc., PhD. All rights reserved.
Stores data in a hashmap organized as an AVL tree using Key/Value pairs

*****
IMPORTANT: Designed to be used as a SINGLETON class through tight-coupling in-app.
To change this, instead of critical regions use mutex (binary semaphores) which are
system-wide, e.g. through the CreateMutex() call in Windows. According to
https://forum.lazarus.freepascal.org/index.php?topic=48032.0 the system-wide mutex
implementations are not cross-platform yet in FPC.
****
}
unit DBStorage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AvgLvlTree, DCPsha256, DCPripemd160, StrHashMap;

const MAX_KEY_LENGTH = 128; // max key length
      FILENAME_LENGTH = 64; // SHA256 length (in hex!)
      MAX_DB_NAME_LENGTH = 64; // the maximum DB name length
      MAX_BUFFER_LENGTH = 1 * 1048576;// div 16;

      BUCKET_SIZE = (1 shl 17) - 1; // used in red-black trees for key mutexes (1M - 1)
      BUCKET_SIZE_INSTANCES = (1 shl 10) - 1; // used in list of DB instances

      CT_ERROR_INVALID_DB_NAME = 1; // name is invalid

      CT_ERROR_CANNOT_READ_FILE = 2; // cannot read file from disk
      CT_ERROR_INVALID_BINARY_DATA = 3; // binary DB data seems to be corrupt
      CT_ERROR_CANNOT_WRITE_FILE = 4; // cannot read file from disk
      CT_ITEM_ALREADY_EXISTS = 5; // item already exists in the hash tree
      CT_ERROR_WRITING_BINARY_DATA = 6; // error writing binary data to disk
      CT_ERROR_INCOMPLETE_WRITING_BINARY_DATA = 7; // incomplete writing of binary file; possibly out of disk space
      CT_ITEM_NOT_FOUND = 8; // item not found in hash tree
      CT_SUCCESS_BUT_COULD_NOT_DELETE_FILE = 9; // was able to remove item from hash tree, but could not delete file on disk
      CT_ITEM_EXISTS_IN_TREE = 10; // key exists in the hash tree
      CT_ERROR_BINARY_DATA_FILE_NOT_FOUND = 11; // trying to read from hash tree, but file not on disk?
      CT_ERROR_READING_BINARY_DATA_FILE = 12; // error when reading binary data file from disk
      CT_ITEM_EXISTS_BUT_NO_DATA = 13; // item exists in hash tree, but the binary data file is missing

      CT_SINGLETON_EXCEPTION = 14; // exception thrown in case initialization of a singleton for a specific DB did not work out

      CT_SUCCESS = 0; // successfull call

{$IFDEF WINDOWS}
      PATH_SEPARATOR = '\';
{$ELSE}
      PATH_SEPARATOR = '/';
{$ENDIF WINDOWS}


type
    { Class to be called when initializing a DB }
    PRTLCriticalSection = ^TRTLCriticalSection;
    PABEYBlockchainDBStorage = ^TABEYBlockchainDBStorage;
    TABEYBlockchainDBStorage = class
      private
        fDatabaseName:  String;
        fStorageFolder: String;
        fTotalItems:    Integer;
        fHashTree:      TStringToStringTree;
        fMutex:         TRTLCriticalSection;
        fKeyMutex:      TStringHashMap; // array of mutexes for each key in the system

        function        LoadFromDisk(): Integer;

        function        IterateInstances(AUserData: Pointer; const AStr: string; var APtr: Pointer): Boolean;
      public
        constructor     Create(DatabaseName, StorageFolder: String; var Error: Integer); // initializes a DB with the given name
        destructor      Done(); // destroys the instance

        function        InsertItem(Key: String; BinaryData: TStream; ReplaceItemIfExists: Boolean = True): Integer; // inserts an item into the storage system
        function        InsertItem(Key: String; Bytes: TBytes; ReplaceItemIfExists: Boolean = True): Integer; // inserts an item into the storage system
        function        RemoveItem(Key: String): Integer; // removes an item from the storage system
        function        ItemExists(Key: String): Integer; // determines if an item exists in the DB storage system
        function        FindItem(Key: String; BinaryData: TStream): Integer; // locates the binary data associated to the key "Key" and puts it in the "BinaryData" stream; returns NULL if non-existent
        function        GetAllKeys(): TStringList; // returns a list of all the keys in the database
        function        GetNumberOfItems():Integer; //returns the numbers of items in the database
        function        SaveToDisk(): Integer;
        function        ComputeSHA256(Source: String): String;

        class function  GetInstance(DatabaseName, StorageFolder: String; var Error: Integer): TABEYBlockchainDBStorage; static; // to be used with the singleton pattern
        class procedure FreeUpAllInstances(); static; // frees up all existing singleton instances; to be called if GetInstance() is used
    end;

var
   fInstanceMutex: TRTLCriticalSection;
   fInstances:     TStringHashMap; // map of instances for the various DBs existing now

implementation

{ Computes the SHA256 of a given string }
function TABEYBlockchainDBStorage.ComputeSHA256(Source: String): String;
var
    Hash: TDCP_sha256;
    Digest: array[0..31] of byte;  // sha256 produces a 256bit digest (32bytes)
    i: integer;
    str1: string;
begin
    Hash:= TDCP_sha256.Create(nil);  // create the hash
    Hash.Init;                        // initialize it
    Hash.UpdateStr(Source);
    Hash.Final(Digest);               // produce the digest
    str1:= '';
    for i:= 0 to 31 do
      str1:= str1 + IntToHex(Digest[i],2);
    Result := str1;
end;

{ Returns CT_SUCCESS is name is valid, otherwise CT_ERROR_INVALID_DB_NAME }
function IsDBNameValid(S: String): Integer;
var I: Integer;
begin
  if Length(S) > MAX_DB_NAME_LENGTH then
  begin
    Result := CT_ERROR_INVALID_DB_NAME;
    Exit;
  end;
  for I:=1 to Length(S) do
    if not ( ((S[I] >= 'A') and (S[I] <= 'Z')) or ((S[I] >= 'a') and (S[I] <= 'z'))
       or ((S[I] >= '0') and (S[I] <= '9'))) then
       begin
         Result := CT_ERROR_INVALID_DB_NAME;
         Exit;
       end;
  Result := CT_SUCCESS;
end;

{ Initializes a DB with the given name}
constructor TABEYBlockchainDBStorage.Create(DatabaseName, StorageFolder: String; var Error: Integer);
var Res: Integer;
begin
  if IsDBNameValid(DatabaseName) <> CT_SUCCESS then
     begin
       Error := CT_ERROR_INVALID_DB_NAME;
       Exit;
     end;
  fDatabaseName := DatabaseName;
  fStorageFolder := StorageFolder;
  fTotalItems := 0;
  if not DirectoryExists(fStorageFolder) then // attempt to create storage folder if it doesn't exist
     CreateDir(fStorageFolder);
  if not DirectoryExists(fStorageFolder+PATH_SEPARATOR+fDatabaseName) then // attempt to create database folder if it doesn't exist
     CreateDir(fStorageFolder+PATH_SEPARATOR+fDatabaseName);

  fHashTree := TStringToStringTree.Create(false); // false = Names are compared case insensitive via ''AnsiCompareText''
  fKeyMutex := TStringHashMap.Create(BUCKET_SIZE); // create our hashmap (red-black trees)

  InitCriticalSection(fMutex); // create DB lock/mutex to use
  Res := LoadFromDisk(); // attempt to load DB from disk
  if Res <> CT_SUCCESS then // an error happened?
     begin
       Error := Res;
       Exit;
     end;

  Error := CT_SUCCESS;
  //inherited Create; // call ancestor constructor
end;

{ Frees up resources }
destructor TABEYBlockchainDBStorage.Done();
var
  Item: PStringToStringItem;
begin
  SaveToDisk(); // save DB to disk...
  DoneCriticalSection(fMutex);
  for Item in fHashTree do
    begin
      DoneCriticalSection(PRTLCriticalSection(fKeyMutex[Item^.Name])^); // we're done with this mutex, free it up
      Dispose(PRTLCriticalSection(fKeyMutex[Item^.Name])); // free up memory for this mutex
    end;
  fKeyMutex.Free;
  fHashTree.Free;
  inherited Destroy; // call ancestor
end;


{ Loads the entire DB structure from the disk, into the hash tree }
function TABEYBlockchainDBStorage.LoadFromDisk(): Integer;
var Stream: TFileStream;
    Key, HashKey: ShortString;
    N: Byte;
    P: PRTLCriticalSection;
begin
  Result := CT_SUCCESS;
  if not FileExists(fStorageFolder+PATH_SEPARATOR+fDatabaseName+'.db') then // we need to create this file
    Stream := TFileStream.Create(fStorageFolder+PATH_SEPARATOR+fDatabaseName+'.db', fmCreate)
  else
    Stream := TFileStream.Create(fStorageFolder+PATH_SEPARATOR+fDatabaseName+'.db', fmOpenRead or fmShareDenyWrite);
  try
      while (Stream.Position < Stream.Size) do
      begin
        Stream.Read(N, SizeOf(Byte));
        SetLength(Key, N);
        Stream.Read(N, SizeOf(Byte));
        SetLength(HashKey, N);
        Stream.Read(Key[1], MAX_KEY_LENGTH);
        Stream.Read(HashKey[1], FILENAME_LENGTH);
        fHashTree[Key] := HashKey; // key --> filename

        // create a new mutex for this key
        New(P);
        fKeyMutex[Key] := P;
        InitCriticalSection(P^); // create mutexes to use

        Inc(fTotalItems); // increment total number of items in DB
      end;
  except
    Result := CT_ERROR_CANNOT_READ_FILE;
  end;
  Stream.Free;
end;

{ Saves the entire DB structure from the hash tree, onto disk }
function TABEYBlockchainDBStorage.SaveToDisk(): Integer;
var Stream: TFileStream;
    Item: PStringToStringItem;
    N: Byte;
    Key, HashKey: ShortString;
begin
  EnterCriticalSection(fMutex); // lock DB
  try
    Result := CT_SUCCESS;
    Stream := TFileStream.Create(fStorageFolder+PATH_SEPARATOR+fDatabaseName+'.db', fmCreate);
    try
      for Item in fHashTree do
      begin
        Key := Item^.Name;
        HashKey := Item^.Value;

        N := Length(Key);
        Stream.Write(N, SizeOf(Byte));
        N := Length(HashKey);
        Stream.Write(N, SizeOf(Byte));

        Stream.Write(Key[1], MAX_KEY_LENGTH);
        Stream.Write(HashKey[1], FILENAME_LENGTH);
      end;
    except
      Result := CT_ERROR_CANNOT_WRITE_FILE;
    end;
    Stream.Free;
  finally
    LeaveCriticalSection(fMutex); // unlock DB
  end;
end;

{ Removes an item from the storage system }
function TABEYBlockchainDBStorage.RemoveItem(Key: String): Integer;
var P: PRTLCriticalSection;
begin
  Result := CT_SUCCESS;
  EnterCriticalSection(fMutex); // lock DB
  try
    if not fHashTree.Contains(Key) then // items is not in hash tree
      begin
        Result := CT_ITEM_NOT_FOUND; // nothing to remove
        LeaveCriticalSection(fMutex); // unlock DB
        Exit;
      end;

    // item is ready to be removed...

    // delete the file
    P := fKeyMutex[Key];  // also remove mutex key
    EnterCriticalSection(P^); // lock key
    DeleteFile(fStorageFolder+PATH_SEPARATOR+fDatabaseName+PATH_SEPARATOR+fHashTree[Key]);
    LeaveCriticalSection(P^); // unlock key
    DoneCriticalSection(P^);

    // check if file was removed
    if FileExists(fStorageFolder+PATH_SEPARATOR+fDatabaseName+PATH_SEPARATOR+fHashTree[Key]) then
      Result := CT_SUCCESS_BUT_COULD_NOT_DELETE_FILE;
    fHashTree.Remove(Key); // remove this key altogether
    Dispose(P);
    fKeyMutex.Remove(Key);
  finally
    Dec(fTotalItems);
    LeaveCriticalSection(fMutex); // unlock DB
  end;
end;

{ Determines if an item exists in the DB storage system }
function TABEYBlockchainDBStorage.ItemExists(Key: String): Integer;
begin
  Result := CT_ITEM_NOT_FOUND; // item does not exist
  EnterCriticalSection(fMutex); // lock DB
  try
    if fHashTree.Contains(Key) then
    begin
      Result := CT_ITEM_EXISTS_IN_TREE;
      if not FileExists(fStorageFolder+PATH_SEPARATOR+fDatabaseName+PATH_SEPARATOR+fHashTree[Key]) then // binary data not found!
        Result := CT_ITEM_EXISTS_BUT_NO_DATA;
    end;
  finally
    LeaveCriticalSection(fMutex); // unlock DB
  end;
end;

{ Locates the binary data associated to the key "Key" and puts it in the "BinaryData" stream; returns NULL if key doesn't exist }
function TABEYBlockchainDBStorage.FindItem(Key: String; BinaryData: TStream): Integer;
var Stream: TFileStream;
    BytesRead: Integer;
    //Buffer: TBytes; --> requires SetLength(), so too slow
    P: PRTLCriticalSection;
    Value: String;
    BufferSize: Integer;
    Buffer: array[0..MAX_BUFFER_LENGTH-1] of Char; // much faster than SetLength()
begin
  Result := CT_SUCCESS;
  EnterCriticalSection(fMutex); // lock DB
  try
    Value := fHashTree[Key];
    if not FileExists(fStorageFolder+PATH_SEPARATOR+fDatabaseName+PATH_SEPARATOR+Value) then // check to make sure file exists
      begin
        Result := CT_ERROR_BINARY_DATA_FILE_NOT_FOUND;
        LeaveCriticalSection(fMutex); // unlock DB
        Exit;
      end;
    P := PRTLCriticalSection(fKeyMutex[Key]);
    LeaveCriticalSection(fMutex); // unlock DB

    // SetLength(Buffer, MAX_BUFFER_LENGTH); ---> very slow, obsolete
    EnterCriticalSection(P^); // lock key
    Stream := TFileStream.Create(fStorageFolder+PATH_SEPARATOR+fDatabaseName+PATH_SEPARATOR+Value, fmOpenRead or fmShareDenyWrite); // try to open in exclusive mode
    try
      while Stream.Position < Stream.Size do
      begin
        BytesRead := Stream.Read(Buffer[0], MAX_BUFFER_LENGTH);
        BinaryData.Write(Buffer[0], BytesRead);
      end;
      Stream.Free;
    except
      Result := CT_ERROR_READING_BINARY_DATA_FILE; // error when reading binary data file
      Stream.Free;
      LeaveCriticalSection(P^); // unlock key
      Exit;
    end;
  finally
    LeaveCriticalSection(P^); // unlock key
  end;
end;

{ Variant 1 (TStream): Inserts an item into the storage system }
{ Could also use read-write-lock as per https://en.wikipedia.org/wiki/Readers%E2%80%93writer_lock }
function TABEYBlockchainDBStorage.InsertItem(Key: String; BinaryData: TStream; ReplaceItemIfExists: Boolean = True): Integer;
var
  Tmp, BytesRead, BytesWritten: Integer;
  HashKey: String;
  Stream: TFileStream;
  //Buffer: TBytes;
  KeyMutex: PRTLCriticalSection; // mutex to use for this key
  P: PRTLCriticalSection;
  Buffer: array[0..MAX_BUFFER_LENGTH-1] of Char; // much faster than SetLength()
begin
  Result := CT_SUCCESS;
  //SetLength(Buffer, MAX_BUFFER_LENGTH);
  EnterCriticalSection(fMutex); // lock DB

  if (not ReplaceItemIfExists) then // if we cannot replace this item in case it exists...
       // check if the key exists
       if fHashTree.Contains(Key) then
          begin
            Result := CT_ITEM_ALREADY_EXISTS;
            LeaveCriticalSection(fMutex); // unlock DB
            Exit;
          end;

    // ---
    // if key already exists, simply replace file in question
    // ---
    if fHashTree.Contains(Key) then
      begin
        HashKey := fHashTree[Key];
        KeyMutex := PRTLCriticalSection(fKeyMutex[Key]); // the mutex for this key
        EnterCriticalSection(KeyMutex^); // lock key
        LeaveCriticalSection(fMutex); // unlock DB, so others can read from it also

        // write data here
        Stream := TFileStream.Create(fStorageFolder+PATH_SEPARATOR+fDatabaseName+PATH_SEPARATOR+HashKey, fmCreate);
        try
          BinaryData.Seek(0, soBeginning); // start from offset 0
          while BinaryData.Position < BinaryData.Size do
          begin
            BytesRead := BinaryData.Read(Buffer[0], MAX_BUFFER_LENGTH);
            BytesWritten := Stream.Write(Buffer[0], BytesRead);
            if (BytesWritten <> BytesRead) then // we did not write as many bytes as we had read - out of disk space?
              begin
                Result := CT_ERROR_INCOMPLETE_WRITING_BINARY_DATA;
                Stream.Free;
                LeaveCriticalSection(KeyMutex^); // unlock key
                Exit;
              end;
          end;
          Stream.Free;
        // end writing data
        except
          Result := CT_ERROR_WRITING_BINARY_DATA;
          LeaveCriticalSection(KeyMutex^); // unlock key
          Exit;
        end;
        LeaveCriticalSection(KeyMutex^); // unlock key
        Exit;
      end
    else
    // ---
    // key does not exist, can be added without setting a key lock of its own, because the DB lock protects access anyway
    // ---
    begin
      // step 1. compute the hash
      HashKey := ComputeSHA256(Key);
      Tmp := 0;
      while FileExists(fStorageFolder+PATH_SEPARATOR+fDatabaseName+PATH_SEPARATOR+HashKey) do // check to see if file exists on disk
      begin
        // looks like a SHA256 collision was found!
        HashKey := ComputeSHA256(IntToStr(Tmp)+Key);
        Inc(Tmp);
      end;
      fHashTree[Key] := HashKey; // key --> filename bv
      Inc(fTotalItems);

      // we need to create a lock for this key, to use from this point forward
      New(P);
      InitCriticalSection(P^);
      fKeyMutex[Key] := P;

      // step 2. now we have our file, we have our binary data, time to write data to file
      Stream := TFileStream.Create(fStorageFolder+PATH_SEPARATOR+fDatabaseName+PATH_SEPARATOR+HashKey, fmCreate);
      try
        BinaryData.Seek(0, soBeginning); // start from offset 0
        while BinaryData.Position < BinaryData.Size do
        begin
          BytesRead := BinaryData.Read(Buffer[0], MAX_BUFFER_LENGTH);
          BytesWritten := Stream.Write(Buffer[0], BytesRead);
          if (BytesWritten <> BytesRead) then // we did not write as many bytes as we had read - out of disk space?
            begin
              Result := CT_ERROR_INCOMPLETE_WRITING_BINARY_DATA;
              Stream.Free;
              LeaveCriticalSection(fMutex); // unlock DB
              Exit;
            end;
        end;
        Stream.Free;
        LeaveCriticalSection(fMutex); // unlock DB
      except
        Result := CT_ERROR_WRITING_BINARY_DATA;
        LeaveCriticalSection(fMutex); // unlock DB
        Exit;
      end;
    end;
end;

{ Variant 2 (TBytes): Inserts an item into the storage system }
function TABEYBlockchainDBStorage.InsertItem(Key: String; Bytes: TBytes; ReplaceItemIfExists: Boolean = True): Integer;
var
  Tmp, BytesWritten: Integer;
  HashKey: String;
  Stream: TFileStream;
  KeyMutex: PRTLCriticalSection; // mutex to use for this key
  P: PRTLCriticalSection;
begin
  Result := CT_SUCCESS;
  EnterCriticalSection(fMutex); // lock DB

  if (not ReplaceItemIfExists) then // if we cannot replace this item in case it exists...
       // check if the key exists
       if fHashTree.Contains(Key) then
          begin
            Result := CT_ITEM_ALREADY_EXISTS;
            LeaveCriticalSection(fMutex); // unlock DB
            Exit;
          end;

    // ---
    // if key already exists, simply replace file in question
    // ---
    if fHashTree.Contains(Key) then
      begin
        HashKey := fHashTree[Key];
        KeyMutex := PRTLCriticalSection(fKeyMutex[Key]); // the mutex for this key
        EnterCriticalSection(KeyMutex^); // lock key
        LeaveCriticalSection(fMutex); // unlock DB, so others can read from it also

        // write data here
        Stream := TFileStream.Create(fStorageFolder+PATH_SEPARATOR+fDatabaseName+PATH_SEPARATOR+HashKey, fmCreate);
        try
          BytesWritten := Stream.Write(Bytes[0], Length(Bytes));
          if (BytesWritten <> Length(Bytes)) then // we did not write as many bytes as we had read - out of disk space?
            begin
              Result := CT_ERROR_INCOMPLETE_WRITING_BINARY_DATA;
              Stream.Free;
              LeaveCriticalSection(KeyMutex^); // unlock key
              Exit;
            end;
          Stream.Free;
        // end writing data
        except
          Result := CT_ERROR_WRITING_BINARY_DATA;
          LeaveCriticalSection(KeyMutex^); // unlock key
          Exit;
        end;
        LeaveCriticalSection(KeyMutex^); // unlock key
        Exit;
      end
    else
    // ---
    // key does not exist, can be added without setting a key lock of its own, because the DB lock protects access anyway
    // ---
    begin
      // step 1. compute the hash
      HashKey := ComputeSHA256(Key);
      Tmp := 0;
      while FileExists(fStorageFolder+PATH_SEPARATOR+fDatabaseName+PATH_SEPARATOR+HashKey) do // check to see if file exists on disk
      begin
        // looks like a SHA256 collision was found!
        HashKey := ComputeSHA256(IntToStr(Tmp)+Key);
        Inc(Tmp);
      end;
      fHashTree[Key] := HashKey; // key --> filename
      Inc(fTotalItems);

      // we need to create a lock for this key, to use from this point forward
      New(P);
      InitCriticalSection(P^);
      fKeyMutex[Key] := P;

      // step 2. now we have our file, we have our binary data, time to write data to file
      Stream := TFileStream.Create(fStorageFolder+PATH_SEPARATOR+fDatabaseName+PATH_SEPARATOR+HashKey, fmCreate);
      try
        BytesWritten := Stream.Write(Bytes[0], Length(Bytes));
        if (BytesWritten <> Length(Bytes)) then // we did not write as many bytes as we had read - out of disk space?
          begin
            Result := CT_ERROR_INCOMPLETE_WRITING_BINARY_DATA;
            Stream.Free;
            LeaveCriticalSection(fMutex); // unlock DB
            Exit;
          end;
        Stream.Free;
        LeaveCriticalSection(fMutex); // unlock DB
      except
        Result := CT_ERROR_WRITING_BINARY_DATA;
        LeaveCriticalSection(fMutex); // unlock DB
        Exit;
      end;
    end;
end;

{ Returns a list (as TStringList) of all the keys in the current database }
function TABEYBlockchainDBStorage.GetAllKeys(): TStringList;
var
  Item: PStringToStringItem;
begin
  EnterCriticalSection(fMutex); // lock DB
  try
    Result := TStringList.Create;
    for Item in fHashTree do
      Result.Add(Item^.Name);
  finally
    LeaveCriticalSection(fMutex); // unlock DB
  end;
end;


function  TABEYBlockchainDBStorage.GetNumberOfItems():Integer;
begin
  EnterCriticalSection(fMutex); // lock DB
  try
    Result:=fTotalItems;
  finally
    LeaveCriticalSection(fMutex); // unlock DB
  end

end;

{ For singleton-pattern usage for a specific database. }
class function TABEYBlockchainDBStorage.GetInstance(DatabaseName, StorageFolder: String; var Error: Integer): TABEYBlockchainDBStorage; // to be used with the singleton pattern
begin
  Result := nil; // default to no result
  Error := CT_SUCCESS;
  EnterCriticalSection(fInstanceMutex); // lock instance list
  try
    try
      if (not fInstances.Contains(DatabaseName)) then
        begin
          fInstances[DatabaseName] := TABEYBlockchainDBStorage.Create(DatabaseName, StorageFolder, Error);
          if (Error <> CT_SUCCESS) then
            begin
              fInstances.Remove(DatabaseName); // set to empty
              LeaveCriticalSection(fInstanceMutex); // unlock instance list
              Exit;
            end;
        end;
      Result := TABEYBlockchainDBStorage(fInstances[DatabaseName]);
    except
      Error := CT_SINGLETON_EXCEPTION;
    end;
  finally
    LeaveCriticalSection(fInstanceMutex); // unlock instance list
  end;
end;

function TABEYBlockchainDBStorage.IterateInstances(AUserData: Pointer; const AStr: string; var APtr: Pointer): Boolean;
begin
  if APtr <> nil then
    TABEYBlockchainDBStorage(APtr).Done(); // terminate
  Result := True;                       // Will continue iterating.
end;

{ Frees up all singleton instances stored in fInstances; to be used in conjuction with GetInstance(), at the end of execution to free up memory & flush index data to disk. }
class procedure TABEYBlockchainDBStorage.FreeUpAllInstances();
begin
  fInstances.IterateMethod(nil, @IterateInstances); // because of {$mode objfpc} (see https://wiki.freepascal.org/FPC_message:_Wrong_number_of_parameters_specified and https://wiki.freepascal.org/Mode_ObjFPC)
end;

initialization
  fInstances := TStringHashMap.Create(BUCKET_SIZE_INSTANCES); // create our hashmap (red-black trees)
  InitCriticalSection(fInstanceMutex); // create instances mutex

finalization
  DoneCriticalSection(fInstanceMutex); // remov mutex for instances
  TABEYBlockchainDBStorage.FreeUpAllInstances(); // frees up all instances and flushed index data to disk
  fInstances.Free;

end.

