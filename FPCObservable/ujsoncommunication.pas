unit UJsonCommunication;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils , fpJSON, JSONParser;

type
  BlockchainEvent = record
      EvType : Integer;
      EvTiming : Integer;
      Sender : Integer ;
      Receiver : Integer;
      Amount : Cardinal;
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

   Event := CT_BlockchainEvent_NULL;

   //get JSON from string
   Data := GetJSON( AJSONString) ;


   if Data.FindPath('actionInfo.timingType').AsString = 'Before' then Event.EvTiming:=0
   else Event.EvTiming:=1;

   if Data.FindPath('actionInfo.eventType').AsString  = 'Transaction' then Event.EvTiming:=1
   else if Data.FindPath('actionInfo.eventType').AsString  = 'ChangeKey' then Event.EvTiming:=2
   else if Data.FindPath('actionInfo.eventType').AsString  = 'SaveContract' then Event.EvTiming:=3
   else if Data.FindPath('actionInfo.eventType').AsString  = 'ExecuteContract' then Event.EvTiming:=4 ;

   if Data.FindPath('actionInfo.eventArguments.sender').AsString = 'null' then Event.Sender := -1
   else Event.Sender:= Data.FindPath('actionInfo.eventArguments.sender').AsInt64;

   if Data.FindPath('actionInfo.eventArguments.receiver').AsString = 'null' then Event.Sender := -1
   else Event.Sender:= Data.FindPath('actionInfo.eventArguments.receiver').AsInt64;

   if Data.FindPath('actionInfo.eventArguments.amount').AsString = 'null' then Event.Sender := -1
   else Event.Sender:= Data.FindPath('actionInfo.eventArguments.amount').AsInt64;

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

