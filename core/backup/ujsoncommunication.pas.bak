unit UJsonCommunication;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils , fpJSON, JSONParser,ULOG;

type
  BlockchainEvent = record
      EvType : Byte;
      EvTiming : Integer;
      Sender : Integer ;
      Receiver : Integer;
      Amount : Integer;
    end;

  TJSONHelper = class
    private
      FData:AnsiString;
    public
      function StringIsValidJSON (AJSONString:String):Boolean;

      function IsRegistrationAction(AJSONString:String):Boolean;
      function IsSubscribingAction(AJSONString:String):Boolean;
      function IsResponseAction(AJSONString:String):Boolean;

      function GetAppropriateResponse(AJSONString:String):String;
      function RegistrationResponse(AIP : String ; APort : String; ID : Integer) : String;
      function SubscriptionResponse( ID : Integer ) : String;

      function GetEventFromSubscription( AJSONString : String):BlockchainEvent;


  end;
const
  CT_BlockchainEvent_NULL: BlockchainEvent = (EvType:0;EvTiming:0;Sender:0;Receiver:0;Amount:0);
implementation
uses UPublisher;

function TJSONHelper.StringIsValidJSON (AJSONString:string):boolean;
var
  Data: TJSONData;
begin
   Try
   Data := GetJSON(AJSONString);
   except
   On E : EJSONParser do
      Begin
        Result := False;
        Exit;
      end;
   end;
   Result := True;
end;

function TJSONHelper.IsRegistrationAction(AJSONString:String):Boolean;
var
  Data: TJSONData;
begin

   Result := True;

   //check String is JSON-encoded
   if StringIsValidJSON(AJSONString) = False then
   begin
     Result := False;
     Exit;
   end;

   //get JSON from string
   Data := GetJSON( AJSONString) ;

   // check if keys exit
   if Data.FindPath('action') = Nil then begin
     Result := False;
     Exit;
   end;

   if Data.FindPath('data') = Nil then begin
     Result := False;
     Exit;
   end;

   if Data.FindPath('data.type') = Nil then begin
     Result := False;
     Exit;
   end;

   if Data.FindPath('id') = Nil then begin
     Result := False;
     Exit;
   end;

   //check values of keys
   if Data.FindPath('action').AsString <> 'Registration' then begin
     Result := False;
     Exit;
   end;

   if ( Data.FindPath('data.type').AsString <> 'Full' ) AND ( Data.FindPath('data.type').AsString <> 'Local' ) then begin
     Result := False;
     Exit;
   end;

end;

function TJSONHelper.IsSubscribingAction(AJSONString:String):Boolean;
var Data: TJSONData;
begin

   {
    "action" : "Subscription",
    "actionType" : "Subscription",
    "actionInfo" : {
        "timingType" : ""After"",
        "eventType"  : ""Transaction"",
        "eventArguments"  :  {
            "sender"   : ""23"",
            "receiver" : ""null"",
            "amount"   : ""20"",
         },
     },
    "id" : 100
   }

   Result := True;

   //check String is JSON-encoded
   if StringIsValidJSON(AJSONString) = False then
   begin
     TLog.NewLog(lterror,ClassName,'String is invalid JSON: ');
     Result := False;
     Exit;
   end;

   //get JSON from string
   Data := GetJSON( AJSONString) ;

   // check if keys exit
   if Data.FindPath('action') = Nil then begin
     Result := False;
     Exit;
   end;

   if Data.FindPath('actionType') = Nil then begin
     Result := False;
     Exit;
   end;

   if Data.FindPath('actionInfo') = Nil then begin
     Result := False;
     Exit;
   end;

   if Data.FindPath('actionInfo.timingType') = Nil then begin
     Result := False;
     Exit;
   end;

   if Data.FindPath('actionInfo.eventType') = Nil then begin
     Result := False;
     Exit;
   end;

   if Data.FindPath('actionInfo.eventArguments') = Nil then begin
     Result := False;
     Exit;
   end;

   if Data.FindPath('actionInfo.eventArguments.sender') = Nil then begin
     Result := False;
     Exit;
   end;

   if Data.FindPath('actionInfo.eventArguments.receiver') = Nil then begin
     Result := False;
     Exit;
   end;

   if Data.FindPath('actionInfo.eventArguments.amount') = Nil then begin
     Result := False;
     Exit;
   end;

   //check values of keys
   if Data.FindPath('action').AsString <> 'Subscription' then begin
     Result := False;
     Exit;
   end;
end;

function TJSONHelper.GetEventFromSubscription( AJSONString : String):BlockchainEvent;
var Data :TJSONData;
  Event : BlockchainEvent;
  TempStr : String;
  TempInt : Integer;
begin

   if  IsSubscribingAction(AJSONString) = False then
   begin
     Result := CT_BlockchainEvent_NULL;
     Exit;
   end;

   with GlobalEvent do begin
     EvType := $01;
     EvTiming := 3;
     Sender := 3;
     Sender := 3;
     Receiver := 3;
     Amount := 3;
   end;

   //get JSON from string
   Data := GetJSON( AJSONString) ;




   if Data.FindPath('actionInfo.timingType').AsString = 'After' then GlobalEvent.EvTiming:=1
   else Event.EvTiming:=0;

   if Data.FindPath('actionInfo.eventType').AsString  = 'Transaction' then GlobalEvent.EvType:=$01
   else if Data.FindPath('actionInfo.eventType').AsString  = 'ChangeKey' then GlobalEvent.EvType:=$02
   else if Data.FindPath('actionInfo.eventType').AsString  = 'Recover' then GlobalEvent.EvType:=$03
   else if Data.FindPath('actionInfo.eventType').AsString  = 'ListAccountForSale' then GlobalEvent.EvType:=$04
   else if Data.FindPath('actionInfo.eventType').AsString  = 'DelistAccount' then GlobalEvent.EvType:=$05
   else if Data.FindPath('actionInfo.eventType').AsString  = 'BuyAccount' then GlobalEvent.EvType:=$06
   else if Data.FindPath('actionInfo.eventType').AsString  = 'ExecuteContract' then GlobalEvent.EvType:=$0E;

   if Data.FindPath('actionInfo.eventArguments.sender').AsString = 'null' then GlobalEvent.Sender := -1
   else GlobalEvent.Sender:= StrTOInt(Data.FindPath('actionInfo.eventArguments.sender').AsString);

   if Data.FindPath('actionInfo.eventArguments.receiver').AsString = 'null' then GlobalEvent.Receiver := -1
   else GlobalEvent.Receiver:= StrTOInt(Data.FindPath('actionInfo.eventArguments.receiver').AsString);

   if Data.FindPath('actionInfo.eventArguments.amount').AsString = 'null' then GlobalEvent.Amount := -1
   else GlobalEvent.Amount:= StrTOInt(Data.FindPath('actionInfo.eventArguments.amount').AsString) * 10000;


    TLog.NewLog(lterror,ClassName,'EventType: ' + IntToStr(GlobalEvent.EvType));
    TLog.NewLog(lterror,ClassName,'EventTiming: ' + IntToStr(GlobalEvent.EvTiming));
    TLog.NewLog(lterror,ClassName,'EventSender: ' + IntToStr(GlobalEvent.Sender));
    TLog.NewLog(lterror,ClassName,'EventReceiver: ' + IntToStr(GlobalEvent.Receiver));
    TLog.NewLog(lterror,ClassName,'EventAmount: ' + IntToStr(GlobalEvent.Amount));


end;

function TJSONHelper.IsResponseAction(AJSONString:String):Boolean;
begin

end;

function TJSONHelper.GetAppropriateResponse(AJSONString:String):String;
begin


   if ( IsRegistrationAction(AJSONString) = True ) then begin

   end;

end;


function TJSONHelper.RegistrationResponse(AIP : String ; APort : String; ID : Integer) : String;
var TempJSON : TJSONData;
begin
   TempJSON := TJSONObject.Create([
             'action','Registration',
             'actionType','Partial',
             'actionInfo', TJSONObject.Create([
                     'result','Accepted',
                     'port',APort,
                     'ip',AIP]),
             'id',ID
             ]);


   Result := TempJSON.FormatJSON;
end;

function TJSONHelper.SubscriptionResponse( ID : Integer ) : String;
var TempJSON : TJSONData;
begin
   TempJSON := TJSONObject.Create([
             'action','Subscription',
             'actionType','Partial',
             'actionInfo', TJSONObject.Create([
                     'result','Accepted']),
             'id',ID
             ]);


   Result := TempJSON.FormatJSON;
end;
end.

