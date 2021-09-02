unit UJSONFunctions;

{ Copyright (c) 2016 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the ABEY Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://www.pascalcoin.org
  Source: https://github.com/ABEY/ABEY

  If you like it, consider a donation using Bitcoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

Uses
{$IFNDEF VER210}
{$DEFINE DELPHIXE}
{$ENDIF}

  {$IFDEF FPC}
  fpjson, jsonparser,
  {$ELSE}
  {$IFDEF DELPHIXE}
  System.JSON,
  {$ENDIF}
  DBXJSON,
  {$ENDIF}
  SysUtils, DateUtils, Variants, Classes, ULog,
  {$IFNDEF FPC}System.Generics.Collections{$ELSE}Generics.Collections{$ENDIF};

Type
  {$IFDEF FPC}
  TJSONValue = TJSONData;
  {$ENDIF}

  TABEYJSONData = Class
  private
    FParent : TABEYJSONData;
  protected
    Function ToJSONFormatted(pretty:Boolean;Const prefix : String) : String; virtual; abstract;
  public
    Constructor Create; virtual;
    Destructor Destroy; override;
    Class Function ParseJSONValue(Const JSONObject : String) : TABEYJSONData; overload;
    Class Function ParseJSONValue(Const JSONObject : TBytes) : TABEYJSONData; overload;
    Class Function _GetCount : Integer;
    Function ToJSON(pretty : Boolean) : String;
    Procedure SaveToStream(Stream : TStream);
    Procedure Assign(PCJSONData : TABEYJSONData);
  End;

  TABEYJSONDataClass = Class of TABEYJSONData;

  { TABEYJSONVariantValue }

  TABEYJSONVariantValue = Class(TABEYJSONData)
  private
    FOldValue : Variant;
    FWritable : Boolean;
    FValue: Variant;
    procedure SetValue(const Value: Variant);
  protected
    Function ToJSONFormatted(pretty:Boolean;const prefix : String) : String; override;
  public
    Constructor Create; override;
    Constructor CreateFromJSONValue(JSONValue : TJSONValue);
    Property Value : Variant read FValue write SetValue;
    Function AsString(DefValue : String) : String;
    Function AsInteger(DefValue : Integer) : Integer;
    Function AsInt64(DefValue : Int64) : Int64;
    Function AsDouble(DefValue : Double) : Double;
    Function AsBoolean(DefValue : Boolean) : Boolean;
    Function AsDateTime(DefValue : TDateTime) : TDateTime;
    Function AsCurrency(DefValue : Currency) : Currency;
    Function AsCardinal(DefValue : Cardinal) : Cardinal;
    Function IsNull : Boolean;
  End;

  TABEYJSONNameValue = Class(TABEYJSONData)
  private
    FName: String;
    FValue: TABEYJSONData;
    FFreeValue : Boolean;
    procedure SetValue(const Value: TABEYJSONData);
  protected
    Function ToJSONFormatted(pretty:Boolean;const prefix : String) : String; override;
  public
    Constructor Create(AName : String);
    Destructor Destroy; override;
    Property Name : String read FName;
    Property Value : TABEYJSONData read FValue write SetValue;
  End;

  TABEYJSONArray = class;
  TABEYJSONObject = Class;

  TABEYJSONList = Class(TABEYJSONData)
  private
    FList : TList<TABEYJSONData>;
    function GetItems(Index: Integer): TABEYJSONData;
    procedure SetItems(Index: Integer; const Value: TABEYJSONData);
  protected
    Function GetIndexAsVariant(Index : Integer) : TABEYJSONVariantValue;
    Function GetIndexAsArray(Index : Integer) : TABEYJSONArray;
    Function GetIndexAsObject(Index : Integer) : TABEYJSONObject;
    Procedure CheckCanInsert(Index:Integer; PCJSONData:TABEYJSONData); virtual;
  public
    Constructor Create; override;
    Destructor Destroy; override;
    Property Items[Index:Integer] : TABEYJSONData read GetItems write SetItems;
    Procedure Insert(Index:Integer; PCJSONData:TABEYJSONData);
    Procedure Delete(index : Integer);
    function Count : Integer;
    Procedure Clear;
  End;

  TABEYJSONArray = class(TABEYJSONList)
  private
    Procedure GrowToIndex(index : Integer);
    function GetItemOfType(Index: Integer; DataClass:TABEYJSONDataClass): TABEYJSONData;
  protected
    Function ToJSONFormatted(pretty:Boolean;const prefix : String) : String; override;
  public
    Constructor Create; override;
    Constructor CreateFromJSONArray(JSONArray : TJSONArray);
    Destructor Destroy; override;
    Function GetAsVariant(index : Integer) : TABEYJSONVariantValue;
    Function GetAsObject(index : Integer) : TABEYJSONObject;
    Function GetAsArray(index : Integer) : TABEYJSONArray;
  end;

  { TABEYJSONObject }

  TABEYJSONObject = Class(TABEYJSONList)
  private
    Function GetIndexOrCreateName(Name : String) : Integer;
    Function GetByName(Name : String) : TABEYJSONNameValue;
  protected
    Function ToJSONFormatted(pretty:Boolean;const prefix : String) : String; override;
    Procedure CheckCanInsert(Index:Integer; PCJSONData:TABEYJSONData); override;
    Procedure CheckValidName(Name : String);
  public
    Constructor Create; override;
    Constructor CreateFromJSONObject(JSONObject : TJSONObject);
    Destructor Destroy; override;
    Function FindName(Name : String) : TABEYJSONNameValue;
    Function IndexOfName(Name : String) : Integer;
    Procedure DeleteName(Name : String);
    Function GetAsVariant(Name : String) : TABEYJSONVariantValue;
    Function GetAsObject(Name : String) : TABEYJSONObject;
    Function GetAsArray(Name : String) : TABEYJSONArray;
    Function AsString(ParamName : String; DefValue : String) : String;
    Function AsInteger(ParamName : String; DefValue : Integer) : Integer;
    Function AsCardinal(ParamName : String; DefValue : Cardinal) : Cardinal;
    Function AsInt64(ParamName : String; DefValue : Int64) : Int64;
    Function AsDouble(ParamName : String; DefValue : Double) : Double;
    Function AsBoolean(ParamName : String; DefValue : Boolean) : Boolean;
    Function AsDateTime(ParamName : String; DefValue : TDateTime) : TDateTime;
    Function AsCurrency(ParamName : String; DefValue : Currency) : Currency;
    Function SaveAsStream(ParamName : String; Stream : TStream) : Integer;
    Function LoadAsStream(ParamName : String; Stream : TStream) : Integer;
    Function GetNameValue(index : Integer) : TABEYJSONNameValue;
    Function IsNull(ParamName : String) : Boolean;
    Procedure SetAs(Name : String; Value : TABEYJSONData);
  End;

  EPCParametresError = Class(Exception);

implementation

Function UTF8JSONEncode(plainTxt : String; includeSeparator : Boolean) : String;
Var ws : String;
  i : Integer;
Begin
   ws := UTF8Encode(plainTxt);
   {ALERT:
    UTF8Encode function deletes last char if equal to #0, so we put it manually
    }
   if plainTxt.Substring(Length(plainTxt)-1,1)=#0 then  ws := ws + #0;
        i := 0;
        result := '"';
        while i < Length(ws) do
          begin
            case ws.Chars[i] of
              '/', '\', '"': result := result + '\' + ws.Chars[i];
              #8: result := result + '\b';
              #9: result := result + '\t';
              #10: result := result + '\n';
              #13: result := result + '\r';
              #12: result := result + '\f';
            else
              if (ord(ws.Chars[i]) < 32) Or (ord(ws.Chars[i])>122) then
                result := result + '\u' + inttohex(ord(ws.Chars[i]), 4)
              else
                result := result + ws.Chars[i];
            end;
            inc(i);
          end;
        result := result + '"';
End;

{ TABEYJSONArray }

constructor TABEYJSONArray.Create;
begin
  inherited;

end;

constructor TABEYJSONArray.CreateFromJSONArray(JSONArray: TJSONArray);
Var i : Integer;
begin
  Create;
{$IFDEF FPC}
  for i := 0 to JSONArray.Count - 1 do begin
    if (JSONArray.Items[i] is TJSONArray) then begin
      Insert(i,TABEYJSONArray.CreateFromJSONArray(TJSONArray(JSONArray.Items[i])));
    end else if (JSONArray.Items[i] is TJSONObject) then begin
      Insert(i,TABEYJSONObject.CreateFromJSONObject(TJSONObject(JSONArray.Items[i])));
    end else if (JSONArray.Items[i] is TJSONValue) then begin
      Insert(i,TABEYJSONVariantValue.CreateFromJSONValue(TJSONValue(JSONArray.Items[i])));
    end else raise EPCParametresError.Create('Invalid TJSON Data: '+JSONArray.Items[i].ClassName);
  end;
{$ELSE}
  for i := 0 to JSONArray.Size - 1 do begin
    if (JSONArray.Get(i) is TJSONArray) then begin
      Insert(i,TABEYJSONArray.CreateFromJSONArray(TJSONArray(JSONArray.Get(i))));
    end else if (JSONArray.Get(i) is TJSONObject) then begin
      Insert(i,TABEYJSONObject.CreateFromJSONObject(TJSONObject(JSONArray.Get(i))));
    end else if (JSONArray.Get(i) is TJSONValue) then begin
      Insert(i,TABEYJSONVariantValue.CreateFromJSONValue(TJSONValue(JSONArray.Get(i))));
    end else raise EPCParametresError.Create('Invalid TJSON Data: '+JSONArray.Get(i).ClassName);
  end;
{$ENDIF}
end;


destructor TABEYJSONArray.Destroy;
begin
  inherited;
end;

function TABEYJSONArray.GetAsArray(index: Integer): TABEYJSONArray;
begin
  Result := GetItemOfType(index,TABEYJSONArray) as TABEYJSONArray;
end;

function TABEYJSONArray.GetAsObject(index: Integer): TABEYJSONObject;
begin
  Result := GetItemOfType(index,TABEYJSONObject) as TABEYJSONObject;
end;

function TABEYJSONArray.GetAsVariant(index: Integer): TABEYJSONVariantValue;
begin
  Result := GetItemOfType(index,TABEYJSONVariantValue) as TABEYJSONVariantValue;
end;

function TABEYJSONArray.GetItemOfType(Index: Integer;
  DataClass: TABEYJSONDataClass): TABEYJSONData;
Var V,New : TABEYJSONData;
begin
  GrowToIndex(Index);
  V := GetItems(index);
  if Not (V is DataClass) then begin
    New := DataClass.Create;
    Items[index] := New;
    V := New;
  end;
  Result := V as DataClass;
end;

procedure TABEYJSONArray.GrowToIndex(index: Integer);
begin
  While (index>=Count) do Insert(Count,TABEYJSONVariantValue.Create);
end;

function TABEYJSONArray.ToJSONFormatted(pretty: Boolean; const prefix: String): String;
Var i : Integer;
begin
  If pretty then Result := prefix+'['
  else Result := '[';
  for i := 0 to Count - 1 do begin
    if (i>0) then begin
      Result := Result+',';
      If pretty then Result :=Result +#10+prefix;
    end;
    Result := Result + Items[i].ToJSONFormatted(pretty,prefix+'   ');
  end;
  Result := Result+']';
end;

{ TABEYJSONList }

procedure TABEYJSONList.CheckCanInsert(Index: Integer; PCJSONData: TABEYJSONData);
begin
  if (Index<0) Or (Index>Count) then raise Exception.Create('Invalid insert at index '+Inttostr(Index)+' (Count:'+Inttostr(Count)+')');
end;

procedure TABEYJSONList.Clear;
begin
  while (FList.Count>0) do Delete(FList.Count-1);
end;

function TABEYJSONList.Count: Integer;
begin
  Result := FList.Count;
end;

constructor TABEYJSONList.Create;
begin
  inherited;
  FParent := Nil;
  FList := TList<TABEYJSONData>.Create;
end;

procedure TABEYJSONList.Delete(index: Integer);
Var M : TABEYJSONData;
begin
  M := GetItems(index);
  FList.Delete(index);
  M.Free;
end;

destructor TABEYJSONList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TABEYJSONList.GetIndexAsArray(Index: Integer): TABEYJSONArray;
Var D : TABEYJSONData;
begin
  D := GetItems(Index);
  if (Not (D is TABEYJSONArray)) then begin
    Result := TABEYJSONArray.Create;
    SetItems(Index,Result);
    D.Free;
  end else Result := TABEYJSONArray(D);
end;

function TABEYJSONList.GetIndexAsObject(Index: Integer): TABEYJSONObject;
Var D : TABEYJSONData;
begin
  D := GetItems(Index);
  if (Not (D is TABEYJSONObject)) then begin
    Result := TABEYJSONObject.Create;
    SetItems(Index,Result);
    D.Free;
  end else Result := TABEYJSONObject(D);
end;

function TABEYJSONList.GetIndexAsVariant(Index: Integer): TABEYJSONVariantValue;
Var D : TABEYJSONData;
begin
  D := GetItems(Index);
  if (Not (D is TABEYJSONVariantValue)) then begin
    Result := TABEYJSONVariantValue.Create;
    SetItems(Index,Result);
    D.Free;
  end else Result := TABEYJSONVariantValue(D);
end;

function TABEYJSONList.GetItems(Index: Integer): TABEYJSONData;
begin
  Result := FList.Items[Index];
end;

procedure TABEYJSONList.Insert(Index: Integer; PCJSONData: TABEYJSONData);
begin
  CheckCanInsert(Index,PCJSONData);
  FList.Insert(Index,PCJSONData);
end;

procedure TABEYJSONList.SetItems(Index: Integer; const Value: TABEYJSONData);
Var OldP : TABEYJSONData;
begin
  OldP := FList.Items[Index];
  Try
    FList.Items[Index] := Value;
  Finally
    OldP.Free;
  End;
end;

{ TABEYJSONVariantValue }

Function VariantToDouble(Value : Variant) : Double;
Var s : String;
Begin
  Result := 0;
  Case varType(Value) of
    varSmallint, varInteger, varSingle, varDouble,
      varCurrency : Result := Value;
  Else
    Begin
      s := VarToStr(Value);
      If s='' Then Abort
      Else Result := StrToFloat(s);
    End;
  End;
End;

function TABEYJSONVariantValue.AsBoolean(DefValue: Boolean): Boolean;
begin
  try
    Result := VarAsType(Value,varBoolean);
  except
    Result := DefValue;
  end;
end;

function TABEYJSONVariantValue.AsCurrency(DefValue: Currency): Currency;
begin
  try
    Result := VariantToDouble(Value);
  except
    Result := DefValue;
  end;
end;

function TABEYJSONVariantValue.AsCardinal(DefValue: Cardinal): Cardinal;
begin
  Result := Cardinal( StrToIntDef(VarToStrDef(Value,''),DefValue) );
end;

function TABEYJSONVariantValue.AsDateTime(DefValue: TDateTime): TDateTime;
begin
  try
    Result := VarAsType(Value,varDate);
  except
    Result := DefValue;
  end;
end;

function TABEYJSONVariantValue.AsDouble(DefValue: Double): Double;
begin
  try
    Result := VariantToDouble(Value);
  except
    Result := DefValue;
  end;
end;

function TABEYJSONVariantValue.AsInt64(DefValue: Int64): Int64;
begin
  Result := StrToInt64Def(VarToStrDef(Value,''),DefValue);
end;

function TABEYJSONVariantValue.AsInteger(DefValue: Integer): Integer;
begin
  Result := StrToIntDef(VarToStrDef(Value,''),DefValue);
end;

function TABEYJSONVariantValue.AsString(DefValue: String): String;
begin
  try
    Case VarType(Value) of
      varNull : Result := '';
      varSmallint, varInteger :
        Begin
          Result := inttostr(Value);
        End;
      varSingle, varDouble,varCurrency :
        Begin
          Result := FloatToStr(VariantToDouble(Value));
        End;
      varDate : Result := DateTimeToStr(Value);
    Else Result := VarToStr(Value);
    End;
  except
    Result := DefValue;
  end;
end;

constructor TABEYJSONVariantValue.Create;
begin
  inherited;
  FValue := Null;
  FOldValue := Unassigned;
  FWritable := False;
end;

constructor TABEYJSONVariantValue.CreateFromJSONValue(JSONValue: TJSONValue);
{$IFnDEF FPC}
Var d : Double;
    i64 : Integer;
  ds,ts : Char;
{$ENDIF}
begin
  Create;
  {$IFDEF FPC}
  Value := JSONValue.Value;
  {$ELSE}
  if JSONValue is TJSONNumber then begin
    d := TJSONNumber(JSONValue).AsDouble;
    if JSONValue.ToString.IndexOf('.')>=0 then i64 := 0
    else i64 := TJSONNumber(JSONValue).AsInt;
    ds := {$IFDEF DELPHIXE}FormatSettings.{$ENDIF}DecimalSeparator;
    ts := {$IFDEF DELPHIXE}FormatSettings.{$ENDIF}ThousandSeparator;
    {$IFDEF DELPHIXE}FormatSettings.{$ENDIF}DecimalSeparator := '.';
    {$IFDEF DELPHIXE}FormatSettings.{$ENDIF}ThousandSeparator := ',';
    Try
      if FormatFloat('0.##########',d)=inttostr(i64) then
        Value := i64
      else Value := d;
    Finally
      {$IFDEF DELPHIXE}FormatSettings.{$ENDIF}DecimalSeparator := ds;
      {$IFDEF DELPHIXE}FormatSettings.{$ENDIF}ThousandSeparator := ts;
    End;
  end else if JSONValue is TJSONTrue then Value := true
  else if JSONValue is TJSONFalse then Value := false
  else if JSONValue is TJSONNull then Value := Null
  else Value := JSONValue.Value;
  {$ENDIF}
end;

function TABEYJSONVariantValue.IsNull: Boolean;
begin
  Result := VarIsNull(FValue) or VarIsEmpty(FValue);
end;

procedure TABEYJSONVariantValue.SetValue(const Value: Variant);
begin
  FOldValue := FValue;
  FValue := Value;
end;

function TABEYJSONVariantValue.ToJSONFormatted(pretty: Boolean; const prefix: String): String;
Var   ds,ts : Char;
begin
  Case VarType(Value) of
    varSmallint,varInteger,varByte,varWord,
    varLongWord,varInt64 : Result := VarToStr(Value);
    varBoolean : if (Value) then Result := 'true' else Result:='false';
    varNull : Result := 'null';
    varDate,varDouble,varcurrency : begin
      ds := {$IFDEF DELPHIXE}FormatSettings.{$ENDIF}DecimalSeparator;
      ts := {$IFDEF DELPHIXE}FormatSettings.{$ENDIF}ThousandSeparator;
      {$IFDEF DELPHIXE}FormatSettings.{$ENDIF}DecimalSeparator := '.';
      {$IFDEF DELPHIXE}FormatSettings.{$ENDIF}ThousandSeparator := ',';
      try
        if VarType(Value)=varcurrency then Result := FormatFloat('0.0000',Value)
        else Result := FormatFloat('0.##########',Value);
      finally
        {$IFDEF DELPHIXE}FormatSettings.{$ENDIF}DecimalSeparator := ds;
        {$IFDEF DELPHIXE}FormatSettings.{$ENDIF}ThousandSeparator := ts;
      end;
    end
  else
    Result := UTF8JSONEncode(VarToStr(Value),true);
  end;
end;

{ TABEYJSONObject }

function TABEYJSONObject.AsBoolean(ParamName: String; DefValue: Boolean): Boolean;
Var v : Variant;
  VV : TABEYJSONVariantValue;
begin
  VV := GetAsVariant(ParamName);
  if (VarType(VV.Value)=varNull) AND (VarType( VV.FOldValue ) = varEmpty) then begin
    Result := DefValue;
    Exit;
  end;
  v := GetAsVariant(ParamName).Value;
  try
    if VarIsNull(v) then Result := DefValue
    else Result := VarAsType(v,varBoolean);
  except
    Result := DefValue;
  end;
end;

function TABEYJSONObject.AsCardinal(ParamName: String; DefValue: Cardinal): Cardinal;
begin
  Result := Cardinal(AsInt64(ParamName,DefValue));
end;

function TABEYJSONObject.AsCurrency(ParamName: String; DefValue: Currency): Currency;
Var v : Variant;
  VV : TABEYJSONVariantValue;
begin
  VV := GetAsVariant(ParamName);
  if (VarType(VV.Value)=varNull) AND (VarType( VV.FOldValue ) = varEmpty) then begin
    Result := DefValue;
    Exit;
  end;
  v := GetAsVariant(ParamName).Value;
  try
    if VarIsNull(v) then Result := DefValue
    else Result := VariantToDouble(v);
  except
    Result := DefValue;
  end;
end;

function TABEYJSONObject.AsDateTime(ParamName: String;
  DefValue: TDateTime): TDateTime;
Var v : Variant;
  VV : TABEYJSONVariantValue;
begin
  VV := GetAsVariant(ParamName);
  if (VarType(VV.Value)=varNull) AND (VarType( VV.FOldValue ) = varEmpty) then begin
    Result := DefValue;
    Exit;
  end;
  v := GetAsVariant(ParamName).Value;
  try
    if VarIsNull(v) then Result := DefValue
    else Result := VarAsType(v,varDate);
  except
    Result := DefValue;
  end;
end;

function TABEYJSONObject.AsDouble(ParamName: String; DefValue: Double): Double;
Var v : Variant;
  VV : TABEYJSONVariantValue;
begin
  VV := GetAsVariant(ParamName);
  if (VarType(VV.Value)=varNull) AND (VarType( VV.FOldValue ) = varEmpty) then begin
    Result := DefValue;
    Exit;
  end;
  v := GetAsVariant(ParamName).Value;
  try
    if VarIsNull(v) then Result := DefValue
    else Result := VariantToDouble(v);
  except
    Result := DefValue;
  end;
end;

function TABEYJSONObject.AsInt64(ParamName: String; DefValue: Int64): Int64;
Var v : Variant;
  VV : TABEYJSONVariantValue;
begin
  VV := GetAsVariant(ParamName);
  if (VarType(VV.Value)=varNull) AND (VarType( VV.FOldValue ) = varEmpty) then begin
    Result := DefValue;
    Exit;
  end;
  v := GetAsVariant(ParamName).Value;
  try
    if VarIsNull(v) then Result := DefValue
    else Result := StrToInt64Def(VarToStrDef(v,''),DefValue);
  except
    Result := DefValue;
  end;
end;

function TABEYJSONObject.AsInteger(ParamName: String; DefValue: Integer): Integer;
Var v : Variant;
  VV : TABEYJSONVariantValue;
begin
  VV := GetAsVariant(ParamName);
  if (VarType(VV.Value)=varNull) AND (VarType( VV.FOldValue ) = varEmpty) then begin
    Result := DefValue;
    Exit;
  end;
  v := GetAsVariant(ParamName).Value;
  try
    if VarIsNull(v) then Result := DefValue
    else Result := StrToIntDef(VarToStrDef(v,''),DefValue);
  except
    Result := DefValue;
  end;
end;

function TABEYJSONObject.AsString(ParamName: String; DefValue: String): String;
Var v : Variant;
  VV : TABEYJSONVariantValue;
begin
  VV := GetAsVariant(ParamName);
  if (VarType(VV.Value)=varNull) AND (VarType( VV.FOldValue ) = varEmpty) then begin
    Result := DefValue;
    Exit;
  end;
  v := GetAsVariant(ParamName).Value;
  try
    Case VarType(V) of
      varNull : Result := '';
      varSmallint, varInteger :
        Begin
          Result := inttostr(v);
        End;
      varSingle, varDouble,varCurrency :
        Begin
          Result := FloatToStr(VariantToDouble(v));
        End;
      varDate : Result := DateTimeToStr(v);
    Else Result := VarToStr(v);
    End;
  except
    Result := DefValue;
  end;
end;


procedure TABEYJSONObject.CheckCanInsert(Index: Integer; PCJSONData: TABEYJSONData);
begin
  inherited;
  if Not Assigned(PCJSONData) then raise Exception.Create('Object is nil');
  if Not (PCJSONData is TABEYJSONNameValue) then raise Exception.Create('Object inside a '+TABEYJSONData.ClassName+' must be a '+TABEYJSONNameValue.ClassName+' (currently '+PCJSONData.ClassName+')');
end;

procedure TABEYJSONObject.CheckValidName(Name: String);
Var i : Integer;
begin
  for i := 0 to Length(Name)-1 do begin
    if i=0 then begin
      if Not (Name.Chars[i] in ['a'..'z','A'..'Z','0'..'9','_','.']) then raise Exception.Create(Format('Invalid char %s at pos %d/%d',[Name.Chars[i],i+1,length(Name)]));
    end else begin
      if Not (Name.Chars[i] in ['a'..'z','A'..'Z','0'..'9','_','-','.']) then raise Exception.Create(Format('Invalid char %s at pos %d/%d',[Name.Chars[i],i+1,length(Name)]));
    end;
  end;
end;

constructor TABEYJSONObject.Create;
begin
  inherited;
end;

constructor TABEYJSONObject.CreateFromJSONObject(JSONObject: TJSONObject);
var i,i2 : Integer;
  {$IFDEF FPC}
  aname : TJSONStringType;
  {$ENDIF}
begin
  Create;
  {$IFDEF FPC}
  for i := 0 to JSONObject.Count - 1 do begin
    aname := JSONObject.Names[i];
    i2 := GetIndexOrCreateName(JSONObject.Names[i]);
    if (JSONObject.Types[ aname ] = jtArray) then begin
      (Items[i2] as TABEYJSONNameValue).Value := TABEYJSONArray.CreateFromJSONArray(JSONObject.Arrays[aname]);
    end else if (JSONObject.Types[ aname ] = jtObject) then begin
      (Items[i2] as TABEYJSONNameValue).Value := TABEYJSONObject.CreateFromJSONObject(JSONObject.Objects[aname]);
    end else if (JSONObject.Types[ aname ] in [jtBoolean,jtNull,jtNumber,jtString]) then begin
      (Items[i2] as TABEYJSONNameValue).Value := TABEYJSONVariantValue.CreateFromJSONValue(JSONObject.Items[i]);
    end else raise EPCParametresError.Create('Invalid TJSON Data in JSONObject.'+aname+': '+JSONObject.Items[i].ClassName);
  end;
  {$ELSE}
  for i := 0 to JSONObject.Size - 1 do begin
    i2 := GetIndexOrCreateName(JSONObject.Get(i).JsonString.Value);
    if (JSONObject.Get(i).JsonValue is TJSONArray) then begin
      (Items[i2] as TABEYJSONNameValue).Value := TABEYJSONArray.CreateFromJSONArray(TJSONArray(JSONObject.Get(i).JsonValue));
    end else if (JSONObject.Get(i).JsonValue is TJSONObject) then begin
      (Items[i2] as TABEYJSONNameValue).Value := TABEYJSONObject.CreateFromJSONObject(TJSONObject(JSONObject.Get(i).JsonValue));
    end else if (JSONObject.Get(i).JsonValue is TJSONValue) then begin
      (Items[i2] as TABEYJSONNameValue).Value := TABEYJSONVariantValue.CreateFromJSONValue(TJSONValue(JSONObject.Get(i).JsonValue));
    end else raise EPCParametresError.Create('Invalid TJSON Data in JSONObject.'+JSONObject.Get(i).JsonString.Value+': '+JSONObject.Get(i).ClassName);
  end;
  {$ENDIF}
end;


procedure TABEYJSONObject.DeleteName(Name: String);
Var i : Integer;
begin
  i := IndexOfName(Name);
  if (i>=0) then begin
    Delete(i);
  end;
end;

destructor TABEYJSONObject.Destroy;
begin

  inherited;
end;

function TABEYJSONObject.FindName(Name: String): TABEYJSONNameValue;
Var i : Integer;
begin
  i := IndexOfName(Name);
  Result := Nil;
  if (i>=0) then Result := Items[i] as TABEYJSONNameValue;
end;

function TABEYJSONObject.GetAsArray(Name: String): TABEYJSONArray;
Var NV : TABEYJSONNameValue;
  V : TABEYJSONData;
begin
  NV := GetByName(Name);
  if Not (NV.Value is TABEYJSONArray) then begin
    NV.Value := TABEYJSONArray.Create;
  end;
  Result := NV.Value as TABEYJSONArray;
end;

function TABEYJSONObject.GetAsObject(Name: String): TABEYJSONObject;
Var NV : TABEYJSONNameValue;
  V : TABEYJSONData;
begin
  NV := GetByName(Name);
  if Not (NV.Value is TABEYJSONObject) then begin
    NV.Value := TABEYJSONObject.Create;
  end;
  Result := NV.Value as TABEYJSONObject;
end;

function TABEYJSONObject.GetAsVariant(Name: String): TABEYJSONVariantValue;
Var NV : TABEYJSONNameValue;
  V : TABEYJSONData;
begin
  NV := GetByName(Name);
  if Not (NV.Value is TABEYJSONVariantValue) then begin
    NV.Value := TABEYJSONVariantValue.Create;
  end;
  Result := NV.Value as TABEYJSONVariantValue;
end;

function TABEYJSONObject.GetByName(Name: String): TABEYJSONNameValue;
Var i : Integer;
begin
  i := GetIndexOrCreateName(Name);
  Result := Items[i] as TABEYJSONNameValue;
end;

function TABEYJSONObject.GetIndexOrCreateName(Name: String): Integer;
Var
  NV : TABEYJSONNameValue;
Begin
  Result := IndexOfName(Name);
  if (Result<0) then begin
    CheckValidName(Name);
    NV := TABEYJSONNameValue.Create(Name);
    Result := FList.Add(NV);
  end;
end;

function TABEYJSONObject.GetNameValue(index: Integer): TABEYJSONNameValue;
begin
  Result := Items[index] as TABEYJSONNameValue;
end;

function TABEYJSONObject.IsNull(ParamName: String): Boolean;
Var i : Integer;
  NV : TABEYJSONNameValue;
begin
  i := IndexOfName(ParamName);
  if i<0 then result := true
  else begin
    Result := false;
    NV := TABEYJSONNameValue( FList.Items[i] );
    If (Assigned(NV.Value)) AND (NV.Value is TABEYJSONVariantValue) then begin
      Result := TABEYJSONVariantValue(NV.Value).IsNull;
    end;
  end;
end;

function TABEYJSONObject.IndexOfName(Name: String): Integer;
begin
  for Result := 0 to FList.Count - 1 do begin
    if (Assigned(FList.Items[Result])) And (TObject(FList.Items[Result]) is TABEYJSONNameValue) then begin
      If TABEYJSONNameValue( FList.Items[Result] ).Name = Name then begin
        exit;
      end;
    end;
  end;
  Result := -1;
end;

function TABEYJSONObject.LoadAsStream(ParamName: String; Stream: TStream): Integer;
Var s : RawByteString;
begin
  s := AsString(ParamName,'');
  if (s<>'') then begin
    Stream.Write(s[Low(s)],length(s));
  end;
  Result := Length(s);
end;

function TABEYJSONObject.SaveAsStream(ParamName: String; Stream: TStream): Integer;
Var s : RawByteString;
begin
  Stream.Position := 0;
  SetLength(s,Stream.Size);
  Stream.Read(s[Low(s)],Stream.Size);
  GetAsVariant(ParamName).Value := s;
end;

procedure TABEYJSONObject.SetAs(Name: String; Value: TABEYJSONData);
 // When assigning a object with SetAs this will not be freed automatically
Var NV : TABEYJSONNameValue;
  V : TABEYJSONData;
  i : Integer;
begin
  i := GetIndexOrCreateName(Name);
  NV := Items[i] as TABEYJSONNameValue;
  NV.Value := Value;
  NV.FFreeValue := false;
end;

function TABEYJSONObject.ToJSONFormatted(pretty: Boolean; const prefix: String): String;
Var i : Integer;
begin
  if pretty then Result := prefix+'{'
  else Result := '{';
  for i := 0 to Count - 1 do begin
    if (i>0) then Begin
      Result := Result+',';
      If pretty then Result :=Result +#10+prefix;
    End;
    Result := Result + Items[i].ToJSONFormatted(pretty,prefix+'   ');
  end;
  Result := Result+'}';
end;

{ TABEYJSONNameValue }

constructor TABEYJSONNameValue.Create(AName: String);
begin
  inherited Create;
  FName := AName;
  FValue := TABEYJSONData.Create;
  FFreeValue := True;
end;

destructor TABEYJSONNameValue.Destroy;
begin
  if FFreeValue then FValue.Free;
  inherited;
end;

procedure TABEYJSONNameValue.SetValue(const Value: TABEYJSONData);
Var old : TABEYJSONData;
begin
  if FValue=Value then exit;
  old := FValue;
  FValue := Value;
  if FFreeValue then old.Free;
  FFreeValue := true;
end;

function TABEYJSONNameValue.ToJSONFormatted(pretty: Boolean; const prefix: String): String;
begin
  if pretty then Result := prefix else Result := '';
  Result := Result + UTF8JSONEncode(name,true)+':'+Value.ToJSONFormatted(pretty,prefix+'   ');
end;

{ TABEYJSONData }

Var _objectsCount : Integer;

procedure TABEYJSONData.Assign(PCJSONData: TABEYJSONData);
Var i : Integer;
  NV : TABEYJSONNameValue;
  JSOND : TABEYJSONData;
  s : String;
begin
  if Not Assigned(PCJSONData) then Abort;
  if (PCJSONData is TABEYJSONObject) AND (Self is TABEYJSONObject) then begin
    for i := 0 to TABEYJSONObject(PCJSONData).Count - 1 do begin
      NV := TABEYJSONObject(PCJSONData).Items[i] as TABEYJSONNameValue;
      if NV.Value is TABEYJSONObject then begin
        TABEYJSONObject(Self).GetAsObject(NV.Name).Assign(NV.Value);
      end else if NV.Value is TABEYJSONArray then begin
        TABEYJSONObject(Self).GetAsArray(NV.Name).Assign(NV.Value);
      end else if NV.Value is TABEYJSONVariantValue then begin
        TABEYJSONObject(Self).GetAsVariant(NV.Name).Assign(NV.Value);
      end else raise Exception.Create('Error in TABEYJSONData.Assign decoding '+NV.Name+' ('+NV.Value.ClassName+')');
    end;
  end else if (PCJSONData is TABEYJSONArray) AND (Self is TABEYJSONArray) then begin
    for i := 0 to TABEYJSONArray(PCJSONData).Count - 1 do begin
      JSOND := TABEYJSONArray(PCJSONData).Items[i];
      s := JSOND.ToJSON(false);
      TABEYJSONArray(Self).Insert(TABEYJSONArray(Self).Count,TABEYJSONData.ParseJSONValue(s));
    end;
  end else if (PCJSONData is TABEYJSONVariantValue) AND (Self is TABEYJSONVariantValue) then begin
    TABEYJSONVariantValue(Self).Value := TABEYJSONVariantValue(PCJSONData).Value;
  end else begin
    raise Exception.Create('Error in TABEYJSONData.Assign assigning a '+PCJSONData.ClassName+' to a '+ClassName);
  end;

end;

constructor TABEYJSONData.Create;
begin
   inc(_objectsCount);
end;

destructor TABEYJSONData.Destroy;
begin
  dec(_objectsCount);
  inherited;
end;

class function TABEYJSONData.ParseJSONValue(Const JSONObject: TBytes): TABEYJSONData;
Var JS : TJSONValue;
  {$IFDEF FPC}
  jss : TJSONStringType;
  i : Integer;
  {$ENDIF}
begin
  Result := Nil;
  JS := Nil;
  {$IFDEF FPC}
  SetLength(jss,length(JSONObject));
  for i:=0 to High(JSONObject) do jss[i+1] := AnsiChar( JSONObject[i] );
  Try
    JS := GetJSON(jss);
  Except
    On E:Exception do begin
      TLog.NewLog(ltDebug,ClassName,'Error processing JSON: '+E.Message);
    end;
  end;
  {$ELSE}
  Try
    JS := TJSONObject.ParseJSONValue(JSONObject,0);
  Except
    On E:Exception do begin
      TLog.NewLog(ltDebug,ClassName,'Error processing JSON: '+E.Message);
    end;
  End;
  {$ENDIF}
  if Not Assigned(JS) then exit;
  Try
    if JS is TJSONObject then begin
      Result := TABEYJSONObject.CreateFromJSONObject(TJSONObject(JS));
    end else if JS is TJSONArray then begin
      Result := TABEYJSONArray.CreateFromJSONArray(TJSONArray(JS));
    end else if JS is TJSONValue then begin
      Result := TABEYJSONVariantValue.CreateFromJSONValue(TJSONValue(JS));
    end else raise EPCParametresError.Create('Invalid TJSON Data type '+JS.ClassName);
  Finally
    JS.Free;
  End;
end;

procedure TABEYJSONData.SaveToStream(Stream: TStream);
Var s : RawByteString;
begin
  s := ToJSON(false);
  Stream.Write(s[Low(s)],Length(s));
end;

class function TABEYJSONData.ParseJSONValue(Const JSONObject: String): TABEYJSONData;
begin
  Result := ParseJSONValue( TEncoding.ASCII.GetBytes(JSONObject) );
end;

function TABEYJSONData.ToJSON(pretty: Boolean): String;
begin
  Result := ToJSONFormatted(pretty,'');
end;

class function TABEYJSONData._GetCount: Integer;
begin
  Result := _objectsCount;
end;

initialization
  _objectsCount := 0;
end.
