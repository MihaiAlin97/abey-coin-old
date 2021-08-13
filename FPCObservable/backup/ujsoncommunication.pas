unit UJsonCommunication;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils , fpJSON, JSONParser;

type
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
      function SubscriptionResponse : String;


  end;

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
begin



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

