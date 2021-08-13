unit BinarySearch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,LazLogger,FileUtil,DCPsha256;

type
  Tree = ^TreeNode;

  TreeIndex = ^Tree;

  TreeNode = record

      LeftChild  : Tree;     // just for when binary tree not serialized
      RightChild : Tree;     // just for when binary tree not serialized


      { first part - serialized tree information }

      Value           : Uint32;  //  - SHA256(filename) as integer
      FirstOffset     : Uint32;  // - Offset of Node in essentials(array)
      SecondOffset    : Uint32;  // - Offset of Node in file contents

      { second part - other tree members serialized }

      NameSize           : UInt32 ;          // - 4 bytes
      Name               : String;           // - (Name length) bytes
      ContentSize        : UInt32 ;          // - 4 bytes : size of file content
      Content            : array of byte;    // - (ContentSize) bytes
  end;

  {

  *** Flattening the tree ***

    - the nodes of a binary search tree nodes are put into an array representing the preorder traversal of the tree
    - relevant functions: Flatten,Unflatten,Construct,Reconstruct

  *** Serialization ***

    - we split each node in the array into two parts which will be written to the stream at different positions
    - relevant functions:Serialize,Unserialize

   ** First part **

      - { Value - 4bytes } { FirstOffset - 4bytes } { SecondOffset - 4 bytes }

   ** Second part

      - { NameSize - 4bytes } { Name - NameSize bytes} { ContentSize - 4 bytes } { Content = ContentSize bytes }


   - FirstOffset points to the first byte in the Value field of the left child
   - SecondOffset points to the first byte of the NameSize field of the current Node

   Example:

   Let us assume we have the following files and their content(ASCII encoding for both names and content):

   Name:                 Content:
       firstFile.txt         abcdef
       secondFile.txt        abcdefg
       thirdFile.txt         abcdefgh
       fourthFile.txt        abcdefghi
       fifthFile.txt         abcdefghij

   We proceed constructing the nodes and storing them inside an array:

               Value:             FirstOffset:      SecondOffset:      NameSize:      Name:                 ContentSize:      Content:
   Index: 0      1771275350         0                 0                  13             firstFile.txt         6                 abcdef
          1      193738685          0                 0                  14             secondFile.txt        7                 abcdefg
          2      5247790            0                 0                  13             thirdFile.txt         8                 abcdefgh
          3      1159916483         0                 0                  14             fourthFile.txt        9                 abcdefghi
          4      40369949504        0                 0                  13             fifthFile.txt         10                abcdefghij


   We sort the array and obtain the following:

               Value:             FirstOffset:      SecondOffset:      NameSize:      Name:                 ContentSize:      Content:
   Index: 0      5247790            0                 0                  13             thirdFile.txt         8                 abcdefgh
          1      193738685          0                 0                  14             secondFile.txt        7                 abcdefg
          2      1159916483         0                 0                  14             fourthFile.txt        9                 abcdefghi
          3      1771275350         0                 0                  13             firstFile.txt         6                 abcdef
          4      40369949504        0                 0                  13             fifthFile.txt         10                abcdefghij


   We construct the following binary search tree from the sorted array:

                                1159916483
                           /                \
                  5247790                      1771275350
                           \                              \
                             193738685                      40369949504



  We flatten the tree and obtain pre-order traversal array ( First Offset parameter represents the index in array at this point ):

               Value:             FirstOffset:      SecondOffset:      NameSize:      Name:                 ContentSize:      Content:
   Index: 0      1159916483         1                 0                  14             fourthFile.txt        9                 abcdefghi
          1      5247790            3                 0                  13             thirdFile.txt         8                 abcdefgh
          2      193738685          4                 0                  14             secondFile.txt        7                 abcdefg
          3      1771275350         4                 0                  13             firstFile.txt         6                 abcdef
          4      40369949504        5                 0                  13             fifthFile.txt         10                abcdefghij


  Computing the offsets in stream:

  FirstOffset := Sizeof( NumberOfNodes ) + FirstOffset * (  Sizeof( Value ) + SizeOf ( FirstOffset  ) + SizeOf ( SecondOffset  )    )

  Starting position of Second Part := Sizeof( NumberOfNodes ) + Sizeof( NumberOfNodes ) * ( (  Sizeof( Value ) + SizeOf ( FirstOffset  ) + SizeOf ( SecondOffset  ) )

  We go through the elements of the array and write their bytes in the stream ( array of bytes or TMemoryStream ) :

  Index: 0

         { First Part ... }.....................................{ Second Part ... }

         5   1159916483  1 * 12 + 4 64                              14 fourthFile.txt 9 abcdefghi
             |                                                      |

             CurrentPosition = 0                                    CurrentPosition = 4 + 5 * 12 ;
                                                                                 = 64

  Index: 1

         { First Part ... }.....................................{ Second Part ... }

         5   1159916483  (1 * 16) 64                                14 fourthFile.txt 9 abcdefghi
                                                                                                |
                                    |
                                    CurrentPosition =  4 + 4 + 4 + 4                            CurrentPosition = 64 + 4 + 14 + 4 + 9 ;
                                                    = 16                                                        = 95

  }

  TNodeArray = array of TreeNode;
  PNodeArray = ^TNodeArray;
  PInteger = ^Integer;

  Functor = procedure (Node : Tree);
  Decorator = procedure (Node: Tree;Stack:PNodeArray;CurrentPosition:PInteger;TopOfStack:PInteger;OldPosition:PInteger);

  { TreeUtil }

  TreeUtil = class(TObject)
    public

      {Tree}
      function  CreateTree() : Tree;
      function  Search(Value : Integer; var _tree : Tree) : TreeIndex;
      function  SearchForFile( FileName : String; var _tree : Tree) : TreeIndex;

      procedure Add(FileNode : TreeNode; var _tree : Tree);
      procedure AddFromArray(_array : array of TreeNode; var _tree : Tree);
      procedure Remove(Index : TreeIndex);
      procedure Remove(Value : Integer; var _tree : Tree);


      procedure Swap(var _Node1:TreeNode;var _Node2:TreeNode);
      function  Partition (var arr:TNodeArray;low:Integer;high:Integer): Integer;
      procedure QuickSort (var _arr:TNodeArray; low:Integer;high:integer);
      procedure SortedArrayToBST(var _tree:Tree;var _arr:TNodeArray;Low,High:Integer);


      procedure InOrderTraversal(Action : Functor; _tree : Tree);
      procedure PostOrderTraversal(Action : Functor; _tree : Tree);
      procedure PreOrderTraversal(Action : Functor; _tree : Tree);

      procedure PrintTreeInOrder(_tree : Tree);
      procedure PrintTreePostOrder(_tree : Tree);
      procedure PrintTreePreOrder(_tree : Tree);

      procedure EraseTree(var _tree : Tree);

      { File Specific }

      function CreateFileNode( FileName : String; FilePath : String; var  TempNode : TreeNode; var ErrorMsg : String; var ErrorCode : Integer ):Boolean;
      function CreateFileNode( FilePath : String; var  TempNode : TreeNode ) : Boolean;
      function CreateFileNode( FilePath : String) : TreeNode;
      function SaveFile( FileName : String; var _tree : Tree):Boolean;
      function SaveFile( FileName:String; FilePath : String; var _tree : Tree; var ErrorMsg : String; var ErrorCode : Integer ) : Boolean;

      { BitMappedStructure }

      function  Serialize(var _stack:PNodeArray;var _ms:TMemoryStream):Boolean;
      function  Serialize(var _stack:PNodeArray;var byteArr:TBytes):Boolean;
      function  UnSerialize(var _stack:PNodeArray;var _ms:TMemoryStream):Boolean;
      function  UnSerialize(var _stack:PNodeArray;var byteArr:TBytes ):Boolean;

      procedure Flatten(var _tree:Tree;var _stack:TNodeArray);
      function  UnFlatten(var _stack:TNodeArray; size:Integer):Tree;

      procedure Construct(_node: Tree;var _stack:TNodeArray; _currentPosition,_topOfStack:PInteger);
      function  Reconstruct(var _stack:TNodeArray;var PreIndex:Pinteger;Key:Tree;min,max,size:Uint32):Tree;

      function ComputeSHA256(S: String): Uint32;

      constructor Create();

  end;

implementation

const MAXINT =  9223372036854775807;     // these should be used as integersf >= uint32
const MININT = -9223372036854775808;

constructor TreeUtil.Create();
begin
    inherited Create;
end;

function TreeUtil.CreateTree() : Tree;
begin
    createTree := Nil;
end;

function TreeUtil.Search(Value : Integer; var _tree : Tree) : TreeIndex;
begin
    Search := @_tree;
    while (search^ <> Nil) and (search^^.Value <> Value) do
        if Value < Search^^.Value then
        begin
            Search := @Search^^.LeftChild  ;
        end
        else
        begin
            Search := @Search^^.RightChild;
        end;
end;


function TreeUtil.SearchForFile( FileName : String; var _tree : Tree) : TreeIndex;
var Value:Uint32;
begin
    Value := ComputeSha256(FileName);
    SearchForFile := @_tree;
    while (SearchForFile^ <> Nil) and (SearchForFile^^.Value <> Value) do
        if Value < SearchForFile^^.Value then
        begin
            SearchForFile := @SearchForFile^^.LeftChild  ;
        end
        else
        begin
            SearchForFile := @SearchForFile^^.RightChild;
        end;
end;


function TreeUtil.SaveFile(FileName:String;var _tree : Tree):Boolean;
var MS:TMemoryStream;
    FileNode:Tree;
begin
    FileNode:= SearchForFile(FileName,_tree)^;
    ms := TMemoryStream.Create;

    if ( FileNode <>  Nil )  then
       begin
         ms.Write(FileNode^.Content[0],FileNode^.ContentSize);
         ms.SaveToFile(ExtractFilePath(ParamStr(0)) + FileName);
       end;

    Ms.Free;
    Result:=True;
end;


function TreeUtil.SaveFile(FileName:String;FilePath:String;var _tree : Tree; var ErrorMsg:String; var ErrorCode:Integer):Boolean;
var MS:TMemoryStream;
    FileNode:Tree;
begin
    Result := False;

    FileNode:= SearchForFile(FileName,_tree)^;

    if DirectoryExists(FilePath) <> True then begin
      ErrorCode := 2003;  // CT_RPC_ErrNum_PathNotFound
      ErrorMsg := 'Path not found or invalid';
      Exit;
      Result := False;
    end;


    if FileNode <>  Nil then begin
        try
          try
            ms := TMemoryStream.Create;
            ms.Write(FileNode^.Content[0],FileNode^.ContentSize);
            ms.SaveToFile(FilePath + PathDelim + FileName);
            Result := True;
          finally
            Ms.Free;
          end;
        except
          on E : EInOutError do
          begin
             ErrorCode := E.ErrorCode + 2000;  // this is set to be able to add new error codes to ABEY blockhain rpc API; remove the "+2000 " for usage in other projects
             ErrorMsg  := E.Message + ' - ' + Filename;
             Result    := False;
          end;
        end;
      end else begin
        ErrorCode := 2000;  // CT_RPC_ErrNum_FileNotFoundInOperation
        ErrorMsg  := 'File not found in operation - ' + Filename;
        Result := False;
      end;

end;

procedure TreeUtil.Add(FileNode : TreeNode; var _tree : Tree);
var
    Index : TreeIndex;
    I:Integer;
begin
    Index := search(FileNode.Value, _tree);
    if Index^ = Nil then
    begin
        new(Index^);
        Index^^.Value         := ComputeSha256(FileNode.Name);
        Index^^.LeftChild     := Nil;
        Index^^.RightChild    := Nil;


        Index^^.NameSize      := FileNode.NameSize;
        Index^^.Name          := FileNode.Name;

        Index^^.ContentSize   := FileNode.ContentSize;

        SetLength(Index^^.Content,Length(FileNode.Content))  ;
        for I:=Low(FileNode.Content) to High(FileNode.Content) do begin
           Index^^.Content[I] := FileNode.Content[I];
        end;
    end;
end;

procedure TreeUtil.AddFromArray(_array : array of TreeNode; var _tree : Tree);
var
    I : Integer;
begin
    for I := low(_array) to high(_array) do
        add(_array[I], _tree);
end;

procedure TreeUtil.Remove(Index : TreeIndex);
var
    ToRemove : Tree;
    Cursor   : TreeIndex;
begin
    ToRemove := Index^;
    if ToRemove = Nil then
        exit;
    if (ToRemove^.LeftChild = Nil) or (ToRemove^.RightChild = Nil) then
    begin
        if ToRemove^.LeftChild <> Nil then
            Index^ := ToRemove^.LeftChild
        else
            Index^ := ToRemove^.RightChild;
        Dispose(ToRemove);
    end
    else
    begin
        Cursor := @ToRemove^.RightChild;
        while Cursor^^.LeftChild <> Nil do
            Cursor := @cursor^^.LeftChild;

        ToRemove^.Value := Cursor^^.Value;
        Remove(Cursor);
    end;
end;

procedure TreeUtil.Remove(Value : Integer; var _tree : Tree);
begin
    Remove(search(Value, _tree));
end;



procedure Print(Node : Tree);
begin
   // ShowMessage(IntTOStr(Node^.Value)+ ' ');

end;

procedure TreeUtil.PrintTreeInOrder(_tree : Tree);
begin
    Write('Tree( ');
    InOrderTraversal(@Print, _tree);
    //WriteLn(')');
end;

procedure TreeUtil.PrintTreePreOrder(_tree : Tree);
begin
    //Write('Tree( ');
    PreOrderTraversal(@Print, _tree);
    ////WriteLn(')');
end;


procedure TreeUtil.PrintTreePostOrder(_tree : Tree);
begin
    Write('Tree( ');
    PostOrderTraversal(@Print, _tree);
    //WriteLn(')');
end;


procedure DoDispose(Node : Tree);
begin

    //Finalize(Node^);
    {FillChar( Node^ ,
             SizeOf(Node^.FirstOffset)  +
             SizeOf(Node^.SecondOffset) +
             SizeOf(Node^.LeftChild)    +
             SizeOf(Node^.RightChild)   +
             SizeOf(Node^.NameSize)     +
             SizeOf(Node^.ContentSize)  +
             Length(Node^.Name)         +
             Length(Node^.Content)      ,
             0);   }



    //Dispose(Node);

end;

procedure TreeUtil.EraseTree(var _tree : Tree);
begin
    InOrderTraversal(@DoDispose, _tree);
    _tree := Nil;
end;


procedure TreeUtil.InorderTraversal(Action : Functor; _tree : Tree);
var
    RightChild : Tree;
begin
    if _tree <> Nil then
    begin
        InorderTraversal(Action, _tree^.LeftChild);
        RightChild := _tree^.RightChild;
        Action(_tree);
        InorderTraversal(Action, RightChild);
    end;
end;

procedure TreeUtil.PostOrderTraversal(Action : Functor; _tree : Tree);
var
    RightChild : Tree;
begin
    if _tree <> Nil then
    begin
        PostOrderTraversal(Action, _tree^.LeftChild);
        RightChild := _tree^.RightChild;
        PostOrderTraversal(Action, RightChild);
        Action(_tree);
    end;
end;


procedure TreeUtil.PreOrderTraversal(Action : Functor; _tree : Tree);
var
    LeftChild,RightChild : Tree;
begin
    if _tree <> Nil then
    begin
        RightChild := _tree^.RightChild;
        LeftChild  := _tree^.LeftChild;
        Action(_tree);
        PreOrderTraversal(Action, LeftChild);
        PreOrderTraversal(Action, RightChild);
    end;
end;

procedure TreeUtil.Flatten(var _tree:Tree;var _stack:TNodeArray);
var CurrentPosition,TopOfStack:integer;
begin
      TopOfStack := 1;
      CurrentPosition := 0;
      Construct(_tree,_stack,@CurrentPosition,@TopOfStack);
end;

procedure TreeUtil.Construct(_node: Tree;var _stack:TNodeArray; _currentPosition,_topOfStack:PInteger);
var OldPosition , I : Integer;
    MessageStr:String;
begin

    if _node = Nil then exit;

    SetLength( _stack , _currentPosition^ + 1 );

    _stack [ _currentPosition^ ] := _node^;

    _stack [ _currentPosition^ ].FirstOffset := _topOfStack^;

    OldPosition := _currentPosition^;

    if  ( _node^.LeftChild = Nil ) And ( _node^.RightChild = Nil )
         then  begin
             exit;
         end

    else if ( _node^.LeftChild = Nil ) And ( _node^.RightChild <> Nil )
         then  begin
             _topOfStack^ := _topOfStack^ + 1 ;

             _currentPosition^ := _currentPosition^ + 1;

             Construct(_node^.RightChild , _stack , _currentPosition,_topOfStack)
         end

    else if ( _node^.RightChild = Nil ) And ( _node^.LeftChild <> Nil )
         then begin
             _topOfStack^ := _topOfStack^ + 1 ;

             _currentPosition^ := _currentPosition^ + 1;

             Construct(_node^.LeftChild , _stack , _currentPosition,_topOfStack) ;
         end

    else if ( _node^.LeftChild <> Nil ) And ( _node^.RightChild <> Nil )

         then begin

             _topOfStack^ := _topOfStack^ + 2 ;

             _currentPosition^ := _currentPosition^ + 1;
             Construct(_node^.LeftChild , _stack ,  _currentPosition , _topOfStack) ;

             _currentPosition^ := _currentPosition^ + 1;
             Construct(_node^.RightChild, _stack ,  _currentPosition     , _topOfStack)
         end;



    {  Pseudo-code for flattening the tree
      1: topOfNodeStack ← 1 (top of node stack)
      2: currentP osition ← 0 (position in the stack)
      3: topOfOffsetStack ← 0 (top of the stack of offsets)
      4: node (the currently processed node)

      1: nodeStack[CurrentPosition] ← node
      2: nodeStack[currentP osition].offset ← topOfNodeStack
      3: add in hash (key ← node, value ← currentP osition)
      4: pc ← popCount(node.bitmap)
      5: old ← currentP osition
      6: topOfNodeStack ← topOfNodeStack + pc
      7: for i ← 0 to pc − 1 do
      8: addNode(node.child[i], old + i)
      9: end for
    }

end;



function TreeUtil.Reconstruct(var _stack:TNodeArray;var PreIndex:Pinteger;Key:Tree;min,max,size:Uint32):Tree;
var root:Tree;
begin


  if (PreIndex^ >= Size) then Result:= Nil;

  if ( Key^.Value > Min) AND ( Key^.Value < Max ) then
  begin

      root:= Key;

      root^.Value := Key^.Value;

      PreIndex^ := PreIndex^ + 1;

      if PreIndex^ < Size then
      begin

          root^.LeftChild:=Reconstruct(_stack,PreIndex,@_stack[PreIndex^],min,Key^.Value,Size);
      end;

      if PreIndex^ < Size then
      begin

          root^.RightChild:=Reconstruct(_stack,PreIndex,@_stack[PreIndex^],Key^.Value,Max,Size);
      end;

  end;

  Result := root;

end;


function TreeUtil.UnFlatten(var _stack:TNodeArray; size:Integer):Tree;
var IPreIndex:Integer;
    PPreIndex:PInteger;
begin
  IPreIndex:=0;
  PPreIndex:= @(IPreIndex);
  Result:=Reconstruct(_stack,PPreIndex,@_stack[0],MININT,MAXINT,High(_stack)+1);
end;

function TreeUtil.CreateFileNode(FilePath:String;var  TempNode: TreeNode):Boolean;
var TempFile:File;
    FileName: String;
begin

    {$I+}
    FileName := UTF8ToAnsi(ExtractFileName(FilePath));

    Result := True;

    if FileExists(FilePath) then begin
        try
          AssignFile(TempFile, FilePath);
          try
            Reset(TempFile, 1);
            TempNode.Value       := ComputeSHA256(Filename);
            TempNode.Name        := Filename;
            TempNode.NameSize    := Length(Filename);
            TempNode.ContentSize := Uint32(FileSize(TempFile));
            SetLength(TempNode.Content,TempNode.ContentSize);
            BlockRead(TempFile,TempNode.Content[0],TempNode.ContentSize);
          finally
            CloseFile(TempFile);
          end;
        except
          on E : EInOutError do
             Result:=False;
        end;
      end else Result := False;
end;

function TreeUtil.CreateFileNode(FileName:String;FilePath:String;var  TempNode: TreeNode; var ErrorMsg:String; var ErrorCode:Integer):Boolean;
var TempFile:File;
begin
    {$I+} // file handling errors will be treated as exceptions of type EInOutError

    Result := True;

    if FileExists(FilePath) then begin
        try
          AssignFile(TempFile, FilePath);
          try
            Reset(TempFile, 1);
            TempNode.Value       := ComputeSHA256(Filename);
            TempNode.Name        := Filename;
            TempNode.NameSize    := Length(Filename);
            TempNode.ContentSize := Uint32(FileSize(TempFile));
            SetLength(TempNode.Content,TempNode.ContentSize);
            BlockRead(TempFile,TempNode.Content[0],TempNode.ContentSize);
          finally
            CloseFile(TempFile);
          end;
        except
          on E : EInOutError do
          begin
             ErrorCode := E.ErrorCode + 2000;  // this is set to be able to add new error codes to ABEY blockhain rpc API; remove the "+2000 " for usage in other projects
             ErrorMsg := E.Message + ' - ' + FilePath;
             Result:=False;
          end;
        end;
      end else begin
        ErrorCode := 2002;  // CT_RPC_ErrNum_FileNotFound
        ErrorMsg := 'File not found - ' + FilePath;
        Result := False;
      end;
end;


function TreeUtil.CreateFileNode(FilePath:String):TreeNode;
var TempFile:File;
    FileName: String;
    TempTreeNode : TreeNode;
begin
    FileName := UTF8ToAnsi(ExtractFileName(FilePath));

    if FileExists(FilePath) then
    begin
      AssignFile(TempFile, FilePath);
      try
        Reset(TempFile, 1);
        TempTreeNode.Value       := ComputeSha256(Filename);
        TempTreeNode.Name        := Filename;
        TempTreeNode.NameSize    := Length(Filename);
        TempTreeNode.ContentSize := Uint32(FileSize(TempFile));
        SetLength(TempTreeNode.Content,TempTreeNode.ContentSize);
        BlockRead(TempFile,TempTreeNode.Content[0],TempTreeNode.ContentSize);
      finally
        CloseFile(TempFile);
      end;
    end;
end;

function  TreeUtil.Serialize(var _stack:PNodeArray;var _ms:TMemoryStream):Boolean;
var TreeStream:TMemoryStream;
    ContentStream:TMemoryStream;
    I:Integer;
    NodeSize:Uint32;   // represents the 4 integer fields of TreeNode stored in TreeStream: Value,FirstOffset , SecondOffset , ChildCount
    NodesCount:Uint32;
begin

    TreeStream    := TMemoryStream.Create;
    ContentStream := TMemoryStream.Create;
    _ms := TMemoryStream.Create;

    try
      NodeSize := SizeOf(TreeNode.Value) + SizeOf(TreeNode.FirstOffset) + SizeOf(TreeNode.SecondOffset) ; //+ SizeOf(TreeNode.ChildCount);
      NodesCount := High(_stack^) + 1;     // number of nodes

      TreeStream.Seek(0,soFromCurrent);
      TreeStream.Write( NodesCount, SizeOf(NodesCount) );   //write number of nodes at the beginning

      for I := Low(_stack^) to High(_stack^) do begin
         TreeStream.Seek(0,soFromCurrent);
                                                        // number of elements
         _stack^[I].FirstOffset := SizeOf(Uint32) + _stack^[I].FirstOffset * ( NodeSize );

         TreeStream.Write(_stack^[I].Value,SizeOf(_stack^[I].Value));               // write value field of TreeNode - 4 bytes

         TreeStream.Write(_stack^[I].FirstOffset,SizeOf(_stack^[I].FirstOffset));         // write offset of Node in array - also 4 bytes

         ContentStream.Seek(0,soFromCurrent);                                       // set content stream to 0 relative to what's already written


         _stack^[I].SecondOffset:= Uint32(ContentStream.Position);                  // get current position in Content stream and save it in SecondOffset field of TreeNode

         _stack^[I].SecondOffset:= _stack^[I].SecondOffset + SizeOf(Uint32) + (NodesCount)*NodeSize;     // pad with all bytes in TreeStream


         TreeStream.Write(_stack^[I].SecondOffset,SizeOf(_stack^[I].SecondOffset));        //Save SecondOffset to TreeStream - that ties the node to it's other fields in ContentStream

         ContentStream.Write(_stack^[I].NameSize,SizeOf(_stack^[I].NameSize));
         ContentStream.Write(_stack^[I].Name[1] , Length(_stack^[I].Name) * SizeOf(Char));

         ContentStream.Write(_stack^[I].ContentSize, SizeOf(_stack^[I].ContentSize));
         ContentStream.Write(_stack^[I].Content[0], _stack^[I].ContentSize);

       end;



       _ms.Seek(0,soBeginning);
       TreeStream.SaveToStream(_ms);
       _ms.Seek(0,soFromCurrent);
       ContentStream.SaveToStream(_ms);

       { //For debug
       TreeStream.SaveToFile('D:\bitMappedNodes\TreeStream.txt');
       ContentStream.SaveToFile('D:\bitMappedNodes\ContentStream.txt');
       _ms.SaveToFile('D:\bitMappedNodes\FullStream.txt');
       }
    finally
      TreeStream.Free;
      ContentStream.Free;
      Result:=True;
    end;

  Result:=false;
end;


function  TreeUtil.Serialize(var _stack:PNodeArray;var byteArr:TBytes):Boolean;
var TreeStream:TMemoryStream;
    ContentStream:TMemoryStream;
    _ms:TMemoryStream;
    I:Integer;
    NodeSize:Uint32;   // represents the 4 integer fields of TreeNode stored in TreeStream: Value,FirstOffset , SecondOffset , ChildCount
    NodesCount:Uint32;
    BytesToRead: Uint32;
begin

    TreeStream    := TMemoryStream.Create;
    ContentStream := TMemoryStream.Create;
    _ms := TMemoryStream.Create;


    Result:=false;

    try
      NodeSize := SizeOf(TreeNode.Value) + SizeOf(TreeNode.FirstOffset) + SizeOf(TreeNode.SecondOffset) ; //+ SizeOf(TreeNode.ChildCount);
      NodesCount := High(_stack^) + 1;     // number of nodes

      TreeStream.Seek(0,soFromCurrent);
      TreeStream.Write( NodesCount, SizeOf(NodesCount) );   //write number of nodes at the beginning

      for I := Low(_stack^) to High(_stack^) do begin
         TreeStream.Seek(0,soFromCurrent);
                                                           // number of elements
         _stack^[I].FirstOffset := SizeOf(Uint32) + _stack^[I].FirstOffset * ( NodeSize );

         TreeStream.Write(_stack^[I].Value,SizeOf(_stack^[I].Value));               // write value field of TreeNode - 4 bytes

         TreeStream.Write(_stack^[I].FirstOffset,SizeOf(_stack^[I].FirstOffset));         // write offset of Node in array - also 4 bytes

         ContentStream.Seek(0,soFromCurrent);                                       // set content stream to 0 relative to what's already written


         _stack^[I].SecondOffset:= Uint32(ContentStream.Position);                  // get current position in Content stream and save it in SecondOffset field of TreeNode

         _stack^[I].SecondOffset:= _stack^[I].SecondOffset + SizeOf(Uint32) + (NodesCount)*NodeSize;     // pad with bytes


         TreeStream.Write(_stack^[I].SecondOffset,SizeOf(_stack^[I].SecondOffset));        //Save SecondOffset to TreeStream - that ties the node to it's other fields in ContentStream

         ContentStream.Write(_stack^[I].NameSize,SizeOf(_stack^[I].NameSize));
         ContentStream.Write(_stack^[I].Name[1] , Length(_stack^[I].Name) * SizeOf(Char));

         ContentStream.Write(_stack^[I].ContentSize, SizeOf(_stack^[I].ContentSize));
         ContentStream.Write(_stack^[I].Content[0], _stack^[I].ContentSize);

       end;

       _ms.Seek(0,soBeginning);
       TreeStream.SaveToStream(_ms);

       _ms.Seek(0,soFromCurrent);
       ContentStream.SaveToStream(_ms);


       BytesToRead := _ms.Size;

       SetLength(byteArr,BytesToRead);
       _ms.Seek(0,soBeginning);
       _ms.Read(byteArr[0], BytesToRead );

       {//For debug
        TreeStream.SaveToFile('D:\bitMappedNodes\TreeStream.txt');
        ContentStream.SaveToFile('D:\bitMappedNodes\ContentStream.txt');
        _ms.SaveToFile('D:\bitMappedNodes\FullStreamBytes.txt'); }

    finally
      TreeStream.Free;
      ContentStream.Free;
      _ms.Free;
      Result:=True;
    end;
end;



function  TreeUtil.UnSerialize(var _stack:PNodeArray;var _ms:TMemoryStream):Boolean;
var TreeStream:TMemoryStream;
    ContentStream:TMemoryStream;
    I:Integer;
    NodeSize:Uint32;   // represents the 4 integer fields of TreeNode stored in TreeStream: Value,FirstOffset , SecondOffset , ChildCount
    NodesCount:Uint32;
    ByteIndex:Integer;
begin
    try

      _ms.Seek(0,soFromBeginning);

      NodeSize := SizeOf(TreeNode.Value) + SizeOf(TreeNode.FirstOffset) + SizeOf(TreeNode.SecondOffset) ;//+ SizeOf(TreeNode.ChildCount);
      NodesCount:= Uint32((_ms.Memory)^);  // number of nodes

      SetLength(_stack^,NodesCount);
      ByteIndex := 4;

      for I := Low(_stack^) to High(_stack^) do begin


         _stack^[I].Value:= Uint32((_ms.Memory  + ByteIndex )^);
         ByteIndex +=4;

         _stack^[I].FirstOffset := Uint32((_ms.Memory  + ByteIndex )^);
         ByteIndex +=4;

         _stack^[I].SecondOffset := Uint32((_ms.Memory  + ByteIndex )^);
         ByteIndex +=4;


         _stack^[I].NameSize := Uint32((_ms.Memory  + _stack^[I].SecondOffset )^);


         _stack^[I].ContentSize := Uint32((_ms.Memory  + _stack^[I].SecondOffset + sizeof(_stack^[I].NameSize)+_stack^[I].NameSize)^);

         _ms.Seek(_stack^[I].SecondOffset + sizeof(_stack^[I].NameSize) ,soFromBeginning);

         SetLength(_stack^[I].Name,_stack^[I].NameSize);

         _ms.Read(_stack^[I].Name[1] , _stack^[I].NameSize );
                                             //4 bytes NameSize       + NameSize                  + 4 bytes ContentSize          + ContentSize
         _ms.Seek(_stack^[I].SecondOffset + _stack^[I].NameSize + sizeof(_stack^[I].NameSize) + sizeof(_stack^[I].ContentSize),soFromBeginning);


         SetLength(_stack^[I].Content,_stack^[I].ContentSize);


         _ms.Read(_stack^[I].Content[0] , _stack^[I].ContentSize );

       end;
    finally

      Result:=True;
    end;

  Result:=false;
end;


function  TreeUtil.UnSerialize(var _stack:PNodeArray;var byteArr:TBytes ):Boolean;
var TreeStream:TMemoryStream;
    ContentStream:TMemoryStream;
    _ms :TMemoryStream;
    I:Integer;
    NodeSize:Uint32;   // represents the 4 integer fields of TreeNode stored in TreeStream: Value,FirstOffset , SecondOffset , ChildCount
    NodesCount:Uint32;
    ByteIndex:Integer;
begin
    try
      _ms := TMemoryStream.Create;

      _ms.Write(byteArr[0],Length(byteArr));
      _ms.Seek(0,soFromBeginning);

      NodeSize := SizeOf(TreeNode.Value) + SizeOf(TreeNode.FirstOffset) + SizeOf(TreeNode.SecondOffset) ;//+ SizeOf(TreeNode.ChildCount);
      NodesCount:= Uint32((_ms.Memory)^);  // number of nodes


      DebugLn('Count'+ IntTOstr(NodesCount));

      SetLength(_stack^,NodesCount);
      ByteIndex := 4;

      for I := Low(_stack^) to High(_stack^) do begin


         _stack^[I].Value:= Uint32((_ms.Memory  + ByteIndex )^);
         ByteIndex +=4;

         _stack^[I].FirstOffset := Uint32((_ms.Memory  + ByteIndex )^);
         ByteIndex +=4;

         _stack^[I].SecondOffset := Uint32((_ms.Memory  + ByteIndex )^);
         ByteIndex +=4;


         _stack^[I].NameSize := Uint32((_ms.Memory  + _stack^[I].SecondOffset )^);


         _stack^[I].ContentSize := Uint32((_ms.Memory  + _stack^[I].SecondOffset + sizeof(_stack^[I].NameSize)+_stack^[I].NameSize)^);

         _ms.Seek(_stack^[I].SecondOffset + sizeof(_stack^[I].NameSize) ,soFromBeginning);

         SetLength(_stack^[I].Name,_stack^[I].NameSize);

         _ms.Read(_stack^[I].Name[1] , _stack^[I].NameSize );

                                             //4 bytes NameSize       + NameSize                  + 4 bytes ContentSize          + ContentSize
         _ms.Seek(_stack^[I].SecondOffset + _stack^[I].NameSize + sizeof(_stack^[I].NameSize) + sizeof(_stack^[I].ContentSize),soFromBeginning);


         SetLength(_stack^[I].Content,_stack^[I].ContentSize);


         _ms.Read(_stack^[I].Content[0] , _stack^[I].ContentSize );

       end;
    finally

      Result:=True;
    end;

  Result:=false;
end;

procedure TreeUtil.Swap(var _Node1: TreeNode; var _Node2: TreeNode);
var aux:TreeNode;
begin
          aux    := _node1;
          _node1 := _node2;
          _node2 := aux;
end;

function TreeUtil.Partition (var arr:TNodeArray;low:Integer;high:Integer): Integer;
var pivot : TreeNode;
    I,J : Integer;
begin

  pivot.Value := arr[ high ].Value;

  I := ( low - 1 );

  for J:= Low to ( High - 1) do

  begin

    if arr[J].Value <= pivot.Value then
    begin

       I:=I+1;
       swap ( arr[I], arr[J] );

    end;

  end;

  swap( arr[I + 1], arr[High] );

  Result := I + 1;

end;

procedure TreeUtil.QuickSort(var _arr:TNodeArray; low:Integer;high:integer);
var PartitionIndex : Integer;
begin

  while(Low < High) do
        begin

          PartitionIndex := Partition(_arr,Low,High);


          if(PartitionIndex - Low < High - PartitionIndex ) then
          begin
              QuickSort(_arr,Low,PartitionIndex - 1);
              Low := PartitionIndex
          end

          else begin
             QuickSort(_arr, PartitionIndex + 1, High);
             High := PartitionIndex - 1;
          end;

        end;

end;


procedure TreeUtil.SortedArrayToBST(var _tree:Tree;var _arr:TNodeArray;Low,High:Integer);
var Middle:Integer;
begin

  if Low > High then Exit;

  Middle := (Low + High ) DIV 2;

  //set the root at middle position in array
  _tree := @_arr[Middle];

  //contstruct left subtree
  SortedArrayToBST(_tree^.LeftChild,_arr,Low,Middle-1);


  //construct right subtree
  SortedArrayToBST(_tree^.RightChild,_arr,Middle + 1,High);

end;


function TreeUtil.ComputeSHA256(S: String): Uint32;
var
    Hash: TDCP_sha256;
    Digest: array[0..31] of byte;  // sha256 produces a 256bit digest (32bytes)

    Source: string;
    i: integer;
    str1: string;
    Temp: Uint32 absolute Digest;     // check for endianness
  begin
    Source:= S;  // here your string for get sha256

    if Source <> '' then
    begin
      Hash:= TDCP_sha256.Create(nil);  // create the hash
      Hash.Init;                        // initialize it
      Hash.UpdateStr(Source);
      Hash.Final(Digest);               // produce the digest
      str1:= '';
      for i:= 0 to 31 do
        str1:= str1 + IntToHex(Digest[i],2);

      Result:= Temp;
    end;
  end;


end.

