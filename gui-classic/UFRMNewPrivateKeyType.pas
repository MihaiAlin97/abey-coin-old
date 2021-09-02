unit UFRMNewPrivateKeyType;


{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, UWallet, UCrypto,
  {$IFNDEF FPC}System.Generics.Collections{$ELSE}Generics.Collections{$ENDIF};

type

  { TFRMNewPrivateKeyType }

  TFRMNewPrivateKeyType = class(TForm)
    bbOk: TButton;
    Button2: TButton;
    Label1: TLabel;
    ebName: TEdit;
    Label17: TLabel;
    Label2: TLabel;
    rgKeyType: TRadioGroup;
    Shape12: TShape;
    Shape13: TShape;
    Shape14: TShape;
    Shape15: TShape;
    procedure FormCreate(Sender: TObject);
    procedure bbOkClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FWalletKeys: TWalletKeys;
    FGeneratedPrivateKey: TECPrivateKey;
    procedure SetWalletKeys(const Value: TWalletKeys);
    { Private declarations }
  public
    { Public declarations }
    Property WalletKeys : TWalletKeys read FWalletKeys write SetWalletKeys;
    Property GeneratedPrivateKey : TECPrivateKey read FGeneratedPrivateKey write FGeneratedPrivateKey;
  end;


implementation

uses
  UAccounts, UConst ;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TFRMNewPrivateKeyType.bbOkClick(Sender: TObject);
begin
  if Not Assigned(WalletKeys) then exit;
  if rgKeyType.ItemIndex<0 then raise Exception.Create('In order to be able to generate the key, please first choose the type of the key');

  if Assigned(FGeneratedPrivateKey) then FGeneratedPrivateKey.Free;

  FGeneratedPrivateKey := TECPrivateKey.Create;
  FGeneratedPrivateKey.GenerateRandomPrivateKey( PtrInt(rgKeyType.Items.Objects[rgKeyType.ItemIndex]) );
  WalletKeys.AddPrivateKey(ebName.Text,FGeneratedPrivateKey);
  ModalResult := MrOk;
end;

procedure TFRMNewPrivateKeyType.FormCreate(Sender: TObject);
Var l : TList<Word>;
  i : Integer;
begin
  FGeneratedPrivateKey := Nil;
  FWalletKeys := Nil;
  ebName.Text := DateTimeToStr(now);
  rgKeyType.Items.Clear;
  l := TList<Word>.Create;
  Try
    TAccountComp.ValidsEC_OpenSSL_NID(l);
    for i := 0 to l.Count - 1 do begin
      rgKeyType.Items.AddObject(TAccountComp.GetECInfoTxt(l[i]),TObject(l[i]));
    end;
  Finally
    l.free;
  End;
end;

procedure TFRMNewPrivateKeyType.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FGeneratedPrivateKey);
end;

procedure TFRMNewPrivateKeyType.SetWalletKeys(const Value: TWalletKeys);
begin
  FWalletKeys := Value;
end;

end.
