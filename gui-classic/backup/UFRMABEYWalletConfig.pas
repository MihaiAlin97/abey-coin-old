unit UFRMABEYWalletConfig;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Windows,
  ShellApi,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls, ExtCtrls, UAppParams, UWallet;

type

  TMinerPrivateKey = (mpk_NewEachTime, mpk_Random, mpk_Selected);

  { TFRMABEYWalletConfig }

  TFRMABEYWalletConfig = class(TForm)
    btnKeys1: TShape;
    btnKeys10: TShape;
    btnKeys11: TShape;
    btnKeys2: TShape;
    btnKeys3: TShape;
    btnKeys4: TShape;
    btnKeys5: TShape;
    btnKeys6: TShape;
    btnKeys7: TShape;
    btnKeys8: TShape;
    btnKeys9: TShape;
    cbDownloadNewCheckpoint: TCheckBox;
    cbJSONRPCMinerServerActive: TCheckBox;
    cbJSONRPCPortEnabled: TCheckBox;
    ebDefaultFee: TEdit;
    ebInternetServerPort: TEdit;
    ebJSONRPCAllowedIPs: TEdit;
    ebJSONRPCMinerServerPort: TEdit;
    ebMinFutureBlocksToDownloadNewVault: TEdit;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Label1: TLabel;
    cbSaveLogFiles: TCheckBox;
    cbShowLogs: TCheckBox;
    bbOk: TBitBtn;
    bbCancel: TBitBtn;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lblDefaultInternetServerPort: TLabel;
    lblDefaultJSONRPCMinerServerPort: TLabel;
    lbl_btnKeys: TLabel;
    lbl_btnKeys1: TLabel;
    lbl_btnKeys2: TLabel;
    Panel1: TPanel;
    Shape1: TShape;
    Shape10: TShape;
    Shape11: TShape;
    Shape12: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    Shape5: TShape;
    Shape6: TShape;
    Shape7: TShape;
    Shape8: TShape;
    Shape9: TShape;
    bbUpdatePassword: TBitBtn;
    Label3: TLabel;
    ebMinerName: TEdit;
    cbShowModalMessages: TCheckBox;
    udInternetServerPort: TUpDown;
    gbMinerPrivateKey: TGroupBox;
    rbGenerateANewPrivateKeyEachBlock: TRadioButton;
    rbUseARandomKey: TRadioButton;
    rbMineAllwaysWithThisKey: TRadioButton;
    cbPrivateKeyToMine: TComboBox;
    cbSaveDebugLogs: TCheckBox;
    bbOpenDataFolder: TBitBtn;
    udJSONRPCMinerServerPort: TUpDown;
    procedure cbDownloadNewCheckpointClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure bbOkClick(Sender: TObject);
    procedure bbUpdatePasswordClick(Sender: TObject);
    procedure cbSaveLogFilesClick(Sender: TObject);
    procedure bbOpenDataFolderClick(Sender: TObject);
    procedure cbJSONRPCPortEnabledClick(Sender: TObject);
    procedure Shape3ChangeBounds(Sender: TObject);
    procedure Shape4ChangeBounds(Sender: TObject);
  private
    FAppParams: TAppParams;
    FWalletKeys: TWalletKeys;
    procedure SetAppParams(const Value: TAppParams);
    procedure SetWalletKeys(const Value: TWalletKeys);
    Procedure UpdateWalletConfig;
    { Private declarations }
  public
    { Public declarations }
    Property AppParams : TAppParams read FAppParams write SetAppParams;
    Property WalletKeys : TWalletKeys read FWalletKeys write SetWalletKeys;
  end;

implementation

uses UConst, UAccounts, ULog, UCrypto, UFolderHelper, USettings, UGUIUtils, UNetProtocol;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TFRMABEYWalletConfig.bbOkClick(Sender: TObject);
Var df : Int64;
  mpk : TMinerPrivateKey;
  i : Integer;
begin
  if udInternetServerPort.Position = udJSONRPCMinerServerPort.Position then raise Exception.Create('Server port and JSON-RPC Server miner port are equal!');

  if TAccountComp.TxtToMoney(ebDefaultFee.Text,df) then begin
    AppParams.ParamByName[CT_PARAM_DefaultFee].SetAsInt64(df);
  end else begin
    ebDefaultFee.Text := TAccountComp.FormatMoney(AppParams.ParamByName[CT_PARAM_DefaultFee].GetAsInteger(0));
    raise Exception.Create('Please choose a correct value for the default fee to use!');
  end;
  AppParams.ParamByName[CT_PARAM_InternetServerPort].SetAsInteger(udInternetServerPort.Position );
  if rbGenerateANewPrivateKeyEachBlock.Checked then mpk := mpk_NewEachTime
  else if rbUseARandomKey.Checked then mpk := mpk_Random
  else if rbMineAllwaysWithThisKey.Checked then begin
    mpk := mpk_Selected;
    if cbPrivateKeyToMine.ItemIndex<0 then raise Exception.Create('Please select a private ke!');
    i := PtrInt(cbPrivateKeyToMine.Items.Objects[cbPrivateKeyToMine.ItemIndex]);
    if (i<0) Or (i>=FWalletKeys.Count) then raise Exception.Create('Invalid private key!');
    AppParams.ParamByName[CT_PARAM_MinerPrivateKeySelectedPublicKey].SetAsTBytes( TAccountComp.AccountKey2RawString( FWalletKeys.Key[i].AccountKey ) );
  end else mpk := mpk_Random;

  AppParams.ParamByName[CT_PARAM_MinerPrivateKeyType].SetAsInteger(integer(mpk));
  AppParams.ParamByName[CT_PARAM_JSONRPCMinerServerActive].SetAsBoolean(cbJSONRPCMinerServerActive.Checked );
  AppParams.ParamByName[CT_PARAM_SaveLogFiles].SetAsBoolean(cbSaveLogFiles.Checked );
  AppParams.ParamByName[CT_PARAM_ShowLogs].SetAsBoolean(cbShowLogs.Checked );
  AppParams.ParamByName[CT_PARAM_SaveDebugLogs].SetAsBoolean(cbSaveDebugLogs.Checked);
  AppParams.ParamByName[CT_PARAM_MinerName].SetAsString(ebMinerName.Text);
  AppParams.ParamByName[CT_PARAM_ShowModalMessages].SetAsBoolean(cbShowModalMessages.Checked);
  AppParams.ParamByName[CT_PARAM_JSONRPCMinerServerPort].SetAsInteger(udJSONRPCMinerServerPort.Position);
  AppParams.ParamByName[CT_PARAM_JSONRPCEnabled].SetAsBoolean(cbJSONRPCPortEnabled.Checked);
  AppParams.ParamByName[CT_PARAM_JSONRPCAllowedIPs].SetAsString(ebJSONRPCAllowedIPs.Text);
  if cbDownloadNewCheckpoint.Checked then begin
    i := StrToIntDef(ebMinFutureBlocksToDownloadNewVault.Text,50);
    AppParams.ParamByName[CT_PARAM_MinFutureBlocksToDownloadNewVault].SetAsInteger(i);
    AppParams.ParamByName[CT_PARAM_AllowDownloadNewCheckpointIfOlderThan].SetAsBoolean(i>200);
  end else AppParams.ParamByName[CT_PARAM_AllowDownloadNewCheckpointIfOlderThan].SetAsBoolean(False);

  ModalResult := MrOk;
end;

procedure TFRMABEYWalletConfig.bbOpenDataFolderClick(Sender: TObject);
begin
  {$IFDEF FPC}
  OpenDocument(pchar(TFolderHelper.GetABEYDataFolder))
  {$ELSE}
  shellexecute(0, 'open', pchar(TFolderHelper.GetABEYDataFolder), nil, nil, SW_SHOW)
  {$ENDIF}
end;

procedure TFRMABEYWalletConfig.bbUpdatePasswordClick(Sender: TObject);
Var s,s2 : String;
begin
  if Not Assigned(FWalletKeys) then exit;
  if Not FWalletKeys.IsValidPassword then begin
    s := '';
    Repeat
      if Not InputQueryPassword('Enter Your Wallet''s Password', 'Please enter your wallet''s password below:',s) then exit;
      FWalletKeys.WalletPassword := s;
      if Not FWalletKeys.IsValidPassword then Application.MessageBox(PChar('Invalid password entered!'),PChar(Application.Title),MB_ICONERROR+MB_OK);
    Until FWalletKeys.IsValidPassword;
  end;
  if FWalletKeys.IsValidPassword then begin
    s := ''; s2 := '';
    if Not InputQueryPassword('Change Your Wallet''s Password','Please enter the new password to use for securing your wallet:',s) then exit;
    if trim(s)<>s then raise Exception.Create('Password cannot start or end with a space character!');
    if Not InputQueryPassword('Change password','Please enter again the password to use for securing your wallet:',s2) then exit;
    if s<>s2 then raise Exception.Create('The two passwords entered are different! Operation aborted.');

    FWalletKeys.WalletPassword := s;
    Application.MessageBox(PChar('Your wallet''s password has been changed!'+#10+#10+
      'Please note that your new password is now: "'+s+'".'+#10+#10+
      '(WARNING: Losing this password means a complete loss of your wallet!)'),
      PChar(Application.Title),MB_ICONWARNING+MB_OK);
  end;
  UpdateWalletConfig;
end;

procedure TFRMABEYWalletConfig.cbJSONRPCPortEnabledClick(Sender: TObject);
begin
  ebJSONRPCAllowedIPs.Enabled := cbJSONRPCPortEnabled.Checked;
end;

procedure TFRMABEYWalletConfig.Shape3ChangeBounds(Sender: TObject);
begin

end;

procedure TFRMABEYWalletConfig.Shape4ChangeBounds(Sender: TObject);
begin

end;

procedure TFRMABEYWalletConfig.cbSaveLogFilesClick(Sender: TObject);
begin
  cbSaveDebugLogs.Enabled := cbSaveLogFiles.Checked;
end;

procedure TFRMABEYWalletConfig.FormCreate(Sender: TObject);
begin
  lblDefaultInternetServerPort.Caption := Format('(Default: %d)',[CT_NetServer_Port]);
  udInternetServerPort.Position := CT_NetServer_Port;
  ebDefaultFee.Text := TAccountComp.FormatMoney(0);
  ebMinerName.Text := '';
  bbUpdatePassword.Enabled := false;
  UpdateWalletConfig;
  lblDefaultJSONRPCMinerServerPort.Caption := Format('(Default: %d)',[CT_JSONRPCMinerServer_Port]);
end;

procedure TFRMABEYWalletConfig.cbDownloadNewCheckpointClick(
  Sender: TObject);
begin
  UpdateWalletConfig;
end;

procedure TFRMABEYWalletConfig.SetAppParams(const Value: TAppParams);
Var i : Integer;
begin
  FAppParams := Value;
  if Not Assigned(Value) then exit;
  Try
    udInternetServerPort.Position := AppParams.ParamByName[CT_PARAM_InternetServerPort].GetAsInteger(CT_NetServer_Port);
    ebDefaultFee.Text := TAccountComp.FormatMoney(AppParams.ParamByName[CT_PARAM_DefaultFee].GetAsInt64(0));
    cbJSONRPCMinerServerActive.Checked := AppParams.ParamByName[CT_PARAM_JSONRPCMinerServerActive].GetAsBoolean(true);
    case TMinerPrivateKey(AppParams.ParamByName[CT_PARAM_MinerPrivateKeyType].GetAsInteger(Integer(mpk_Random))) of
      mpk_NewEachTime : rbGenerateANewPrivateKeyEachBlock.Checked := true;
      mpk_Random : rbUseARandomKey.Checked := true;
      mpk_Selected : rbMineAllwaysWithThisKey.Checked := true;
    else rbUseARandomKey.Checked := true;
    end;
    UpdateWalletConfig;
    cbSaveLogFiles.Checked := AppParams.ParamByName[CT_PARAM_SaveLogFiles].GetAsBoolean(true);
    cbShowLogs.Checked := AppParams.ParamByName[CT_PARAM_ShowLogs].GetAsBoolean(false);
    cbSaveDebugLogs.Checked := AppParams.ParamByName[CT_PARAM_SaveDebugLogs].GetAsBoolean(true);
    ebMinerName.Text := AppParams.ParamByName[CT_PARAM_MinerName].GetAsString('');
    cbShowModalMessages.Checked := AppParams.ParamByName[CT_PARAM_ShowModalMessages].GetAsBoolean(false);
    udJSONRPCMinerServerPort.Position := AppParams.ParamByName[CT_PARAM_JSONRPCMinerServerPort].GetAsInteger(CT_JSONRPCMinerServer_Port);
    cbJSONRPCPortEnabled.Checked := AppParams.ParamByName[CT_PARAM_JSONRPCEnabled].GetAsBoolean(true);
    ebJSONRPCAllowedIPs.Text := AppParams.ParamByName[CT_PARAM_JSONRPCAllowedIPs].GetAsString('127.0.0.1;');
    ebMinFutureBlocksToDownloadNewVault.Text := IntToStr(AppParams.ParamByName[CT_PARAM_MinFutureBlocksToDownloadNewVault].GetAsInteger(TNetData.NetData.MinFutureBlocksToDownloadNewVault));
    cbDownloadNewCheckpoint.Checked:= AppParams.ParamByName[CT_PARAM_AllowDownloadNewCheckpointIfOlderThan].GetAsBoolean(TNetData.NetData.MinFutureBlocksToDownloadNewVault>200);
  Except
    On E:Exception do begin
      TLog.NewLog(lterror,ClassName,'Exception at SetAppParams: '+E.Message);
    end;
  End;
  cbSaveLogFilesClick(nil);
  cbJSONRPCPortEnabledClick(nil);
  UpdateWalletConfig;
end;

procedure TFRMABEYWalletConfig.SetWalletKeys(const Value: TWalletKeys);
begin
  FWalletKeys := Value;
  UpdateWalletConfig;
end;


procedure TFRMABEYWalletConfig.UpdateWalletConfig;
Var i, iselected : Integer;
  raw : TBytes;
  wk : TWalletKey;
  auxs : String;
begin
  if Assigned(FWalletKeys) then begin
    if FWalletKeys.IsValidPassword then begin
      if FWalletKeys.WalletPassword='' then begin
        bbUpdatePassword.Caption := 'Secure Wallet Now';
      end else begin
        bbUpdatePassword.Caption := 'Change Password';
      end;
    end else begin
        bbUpdatePassword.Caption := 'Change Password';
    end;
    cbPrivateKeyToMine.Items.Clear;
    for i := 0 to FWalletKeys.Count - 1 do begin
      wk := FWalletKeys.Key[i];
      if (wk.Name='') then begin
        auxs := TCrypto.ToHexaString( TAccountComp.AccountKey2RawString(wk.AccountKey));
      end else begin
        auxs := wk.Name;
      end;
      if (Length(wk.CryptedKey)>0) then begin
        cbPrivateKeyToMine.Items.AddObject(auxs,TObject(i));
      end;
    end;
    cbPrivateKeyToMine.Sorted := true;
    if Assigned(FAppParams) then begin
      raw := FAppParams.ParamByName[CT_PARAM_MinerPrivateKeySelectedPublicKey].GetAsTBytes(Nil);
      iselected := FWalletKeys.IndexOfAccountKey(TAccountComp.RawString2Accountkey(raw));
      if iselected>=0 then begin
        iselected :=  cbPrivateKeyToMine.Items.IndexOfObject(TObject(iselected));
        cbPrivateKeyToMine.ItemIndex := iselected;
      end;

    end;

  end else bbUpdatePassword.Caption := 'Enter Wallet Password';
  bbUpdatePassword.Enabled := Assigned(FWAlletKeys);
  ebMinFutureBlocksToDownloadNewVault.Enabled:=cbDownloadNewCheckpoint.Checked;
end;

end.
