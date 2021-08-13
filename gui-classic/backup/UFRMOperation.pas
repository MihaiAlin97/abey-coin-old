unit UFRMOperation;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Windows,
  System.Actions,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, UNode, UWallet, UCrypto, Buttons, UBlockChain, UAccounts,
  UFRMAccountSelect, ActnList, ComCtrls, ExtCtrls, Types, UFRMMemoText,
  UPCEncryption, UBaseTypes, UPCOrderedLists,binarysearch,lazlogger,StrHashMap;

Const
  CM_PC_WalletKeysChanged = WM_USER + 1;
  BTN_PRESSED_COLOR = $00CEE7FF;
  BTN_HIGHLIGHT_COLOR = $00F9FDFF;

  PAGE_ACCOUNTNAME = 0;
  PAGE_SENDTOKENS = 1;
  PAGE_CHANGEKEY = 2;
  PAGE_BUYACCOUNT = 3;
  PAGE_LISTACCOUNT = 4;
  PAGE_DELISTACCOUNT = 5;
  PAGE_SAVEFILES = 6;



type

  TFileWrap = class
    class var
        FileNames:TStringHashMap;
        FileTree:Tree;
        NodeArray:TNodeArray;
        FlatNodeArray:TNodeArray;
        PFlatNodeArray:PNodeArray;
        ByteNodeArr:TBytes;
        FileCount:Integer;
  end;

  { TFRMOperation }

  TFRMOperation = class(TForm)

    FileContents:TMemoryStream;
    bbBuyNewKey: TBitBtn;
    bbChangePrivateKeyKeys: TBitBtn;
    btnAccountName: TShape;
    btnSaveFiles: TShape;
    btnSendTokens: TShape;
    btnKeys12: TShape;
    btnAccountKey: TShape;
    btnBuyAccount: TShape;
    btnListAccount: TShape;
    btnDelistAccount: TShape;
    btnKeys17: TShape;
    btnKeys18: TShape;
    btnKeys19: TShape;
    cbBuyNewKey: TComboBox;
    cbNewPrivateKey: TComboBox;
    comboUserTypes: TComboBox;
    ebAccountToBuy: TEdit;
    ebAmount: TEdit;
    ebBuyAmount: TEdit;
    ebChangeName: TEdit;
    ebChangeType: TEdit;
    ebDestAccount: TEdit;
    ebEncryptPassword: TEdit;
    ebFee: TEdit;
    ebNewPublicKey: TEdit;
    ebSaleLockedUntilBlock: TEdit;
    ebSaleNewOwnerPublicKey: TEdit;
    ebSalePrice: TEdit;
    ebSellerAccount: TEdit;
    ebSenderAccount: TEdit;
    ebSignerAccount: TEdit;
    gbChangeKey: TGroupBox;
    gbPayload: TGroupBox;
    gbSaleType: TGroupBox;
    Image1: TImage;
    imgSaveFiles: TImage;
    imgBuyAccount: TImage;
    imgAccountName: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Image6: TImage;
    Image7: TImage;
    Image8: TImage;
    Image9: TImage;
    imgListAccount: TImage;
    imgDelistAccount: TImage;
    imgSendTokens: TImage;
    imgAccountKey: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    lbl_btnSaveFiles: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label7: TLabel;
    lblAmount: TLabel;
    lblBuyAccountErrors: TLabel;
    lblBuyAmount: TLabel;
    lblBuyNewKey: TLabel;
    lblChangeKeyErrors: TLabel;
    lblDelistErrors: TLabel;
    lblDestAccount: TLabel;
    lblListAccountErrors: TLabel;
    lblNewOwnerErrors: TLabel;
    lblNewOwnerPublicKey: TLabel;
    lblNewPrivateKey: TLabel;
    lblSaleLockedUntilBlock: TLabel;
    lblSaleNewOwnerPublicKey: TLabel;
    lblTransactionErrors: TLabel;
    lblSaveFilesErrors: TLabel;
    lbl_AccountID: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lblChangeName: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lblChangeInfoErrors: TLabel;
    lblEncryptionErrors: TLabel;
    lblFee: TLabel;
    lblPayloadLength: TLabel;
    lblAccountCaption: TLabel;
    bbExecute: TBitBtn;
    bbCancel: TBitBtn;
    lblAccountBalance: TLabel;
    lblBalanceCaption: TLabel;
    lblSignerAccount: TLabel;
    lbl_btnAccountName: TLabel;
    lbl_btnSendTokens: TLabel;
    lbl_btnKeys2: TLabel;
    lbl_btnAccountKey: TLabel;
    lbl_btnBuyAccount: TLabel;
    lbl_btnListAccount: TLabel;
    lbl_btnDelistAccount: TLabel;
    memoPayload: TMemo;
    Notebook: TNotebook;
    ChangeAccountName: TPage;
    ChangeAccountKey: TPage;
    BuyAccount: TPage;
    ListAccount: TPage;
    DelistAccount: TPage;
    OpenDialogSaveFiles: TOpenDialog;
    SaveFiles: TPage;
    PageControlOpType: TPageControl;
    rbChangeKeyTransferAccountToNewOwner: TRadioButton;
    rbChangeKeyWithAnother: TRadioButton;
    rbListAccountForPrivateSale: TRadioButton;
    rbListAccountForPublicSale: TRadioButton;
    sbSearchBuyAccount: TSpeedButton;
    sbSearchDestinationAccount: TSpeedButton;
    sbSearchListerSellerAccount: TSpeedButton;
    SendTokens: TPage;
    PageControlLocked: TPageControl;
    rbEncrptedWithPassword: TRadioButton;
    rbEncryptedWithEC: TRadioButton;
    rbEncryptedWithOldEC: TRadioButton;
    rbNotEncrypted: TRadioButton;
    sbSearchSignerAccount: TSpeedButton;
    Shape1: TShape;
    Shape12: TShape;
    Shape15: TShape;
    Shape16: TShape;
    Shape25: TShape;
    shapeBuyAccount2: TShape;
    shapeSaveFiles: TShape;
    shapeSaveFiles2: TShape;
    shapeListAccount2: TShape;
    shapeDelistAccount2: TShape;
    shapeSendTokens2: TShape;
    shapeChangeAccount: TShape;
    shapeSendTokens: TShape;
    Shape19: TShape;
    Shape2: TShape;
    shapeChangeKey: TShape;
    shapeBuyAccount: TShape;
    Shape22: TShape;
    shapeListAccount: TShape;
    Shape24: TShape;
    shapeDelistAccount: TShape;
    Shape26: TShape;
    shapeAccountName2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    Shape5: TShape;
    Shape6: TShape;
    Shape7: TShape;
    Shape8: TShape;
    Shape9: TShape;
    shapeChangeKey2: TShape;
    tsBuyAccount: TTabSheet;
    tsChangeInfo: TTabSheet;
    tsChangePrivateKey: TTabSheet;
    tsDelist: TTabSheet;
    tsListForSale: TTabSheet;
    tsOperation: TTabSheet;
    ActionList: TActionList;
    actExecute: TAction;
    tsGlobalError: TTabSheet;
    lblGlobalErrors: TLabel;
    bbPassword: TBitBtn;
    memoAccounts: TMemo;
    lblAccountsCount: TLabel;
    tsTransaction: TTabSheet;
    UploadEdit: TLabeledEdit;
    UploadFilesButton: TButton;
    procedure bbCancelClick(Sender: TObject);
    procedure btnBuyAccountChangeBounds(Sender: TObject);
    procedure btnKeys17ChangeBounds(Sender: TObject);
    procedure btnKeys18ChangeBounds(Sender: TObject);
    procedure btnKeys19ChangeBounds(Sender: TObject);
    procedure btnListAccountChangeBounds(Sender: TObject);
    procedure btnSaveFilesChangeBounds(Sender: TObject);
    procedure comboUserTypesChange(Sender: TObject);
    procedure ebAmountChange(Sender: TObject);
    procedure ebAccountChange(Sender: TObject);
    procedure ebDestAccountChange(Sender: TObject);
    procedure ebFeeChange(Sender: TObject);
    procedure ebNewPublicKeyExit(Sender: TObject);
    procedure ebSenderAccountChange(Sender: TObject);
    procedure ebSignerAccountChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure imgSaveFilesClick(Sender: TObject);
    procedure imgAccountKeyClick(Sender: TObject);
    procedure imgAccountNameClick(Sender: TObject);
    procedure imgBuyAccountClick(Sender: TObject);
    procedure imgDelistAccountClick(Sender: TObject);
    procedure imgListAccountClick(Sender: TObject);
    procedure imgSendTokensClick(Sender: TObject);
    procedure lblAccountBalanceClick(Sender: TObject);
    procedure lblAccountClick(Sender: TObject);
    procedure lblAccountsCountClick(Sender: TObject);
    procedure lblSaveFilesErrorsClick(Sender: TObject);
    procedure lbl_AccountIDClick(Sender: TObject);
    procedure lbl_btnAccountNameClick(Sender: TObject);
    procedure lbl_btnBuyAccountClick(Sender: TObject);
    procedure lbl_btnDelistAccountClick(Sender: TObject);
    procedure lbl_btnSaveFilesClick(Sender: TObject);
    procedure memoAccountsChange(Sender: TObject);
    procedure memoPayloadClick(Sender: TObject);
    procedure ebEncryptPasswordChange(Sender: TObject);
    procedure bbChangePrivateKeyKeysClick(Sender: TObject);
    procedure actExecuteExecute(Sender: TObject);
    procedure ebSenderAccountExit(Sender: TObject);
    procedure ebSenderAccountKeyPress(Sender: TObject; var Key: Char);
    procedure bbPasswordClick(Sender: TObject);
    procedure PageControlLockedChange(Sender: TObject);
    procedure PageControlOpTypeChange(Sender: TObject);
    procedure SaveFilesBeforeShow(ASender: TObject; ANewPage: TPage;
      ANewIndex: Integer);
    procedure sbSearchBuyAccountClick(Sender: TObject);
    procedure sbSearchDestinationAccountClick(Sender: TObject);
    procedure sbSearchListerSellerAccountClick(Sender: TObject);
    procedure sbSearchSignerAccountClick(Sender: TObject);
    procedure SendTokensBeforeShow(ASender: TObject; ANewPage: TPage;
      ANewIndex: Integer);
    procedure Shape16ChangeBounds(Sender: TObject);
    procedure Shape26ChangeBounds(Sender: TObject);
    procedure Shape3ChangeBounds(Sender: TObject);
    procedure Shape9ChangeBounds(Sender: TObject);
    procedure shapeAccountName2ChangeBounds(Sender: TObject);
    procedure shapeChangeAccountChangeBounds(Sender: TObject);
    procedure shapeDelistAccount3ChangeBounds(Sender: TObject);
    procedure shapeSaveFilesChangeBounds(Sender: TObject);
    procedure shapeSaveFiles2ChangeBounds(Sender: TObject);
    procedure shapeListAccount2ChangeBounds(Sender: TObject);
    procedure shapeListAccountChangeBounds(Sender: TObject);
    procedure shapeSendTokens2ChangeBounds(Sender: TObject);
    procedure shapeSendTokensChangeBounds(Sender: TObject);
    procedure StaticText1Click(Sender: TObject);
    procedure updateInfoClick(Sender: TObject);
    procedure bbBuyNewKeyClick(Sender: TObject);
    procedure ebAccountNumberExit(Sender: TObject);
    procedure ebCurrencyExit(Sender: TObject);
    procedure UploadFilesButtonClick(Sender: TObject);
  private
    FNode : TNode;
    FWalletKeys: TWalletKeys;
    FDefaultFee: Int64;
    FEncodedPayload : TRawBytes;
    FDisabled : Boolean;
    FSenderAccounts: TOrderedCardinalList; // TODO: TOrderedCardinalList should be replaced with a "TCardinalList" since signer account should be processed last
    procedure SetWalletKeys(const Value: TWalletKeys);
    Procedure UpdateWalletKeys;
    { Private declarations }
    Procedure UpdateAccountsInfo;
    Function UpdateFee(var Fee : Int64; errors : String) : Boolean;
    Function UpdateOperationOptions(var errors : String) : Boolean;
    Function UpdatePayload(Const SenderAccount : TAccount; var errors : String) : Boolean;
    Function UpdatePayloadSaveFiles(const SenderAccount: TAccount):Boolean;
    Function UpdateOpTransaction(Const SenderAccount : TAccount; var DestAccount : TAccount; var amount : Int64; var errors : String) : Boolean;
    Function UpdateOpChangeKey(Const TargetAccount : TAccount; var SignerAccount : TAccount; var NewPublicKey : TAccountKey; var errors : String) : Boolean;
    Function UpdateOpListForSale(Const TargetAccount : TAccount; var SalePrice : Int64; var SellerAccount,SignerAccount : TAccount; var NewOwnerPublicKey : TAccountKey; var LockedUntilBlock : Cardinal; var errors : String) : Boolean;
    Function UpdateOpDelist(Const TargetAccount : TAccount; var SignerAccount : TAccount; var errors : String) : Boolean;
    Function UpdateOpBuyAccount(Const SenderAccount : TAccount; var AccountToBuy : TAccount; var amount : Int64; var NewPublicKey : TAccountKey; var errors : String) : Boolean;
    Function UpdateOpChangeInfo(Const TargetAccount : TAccount; var SignerAccount : TAccount; var changeName : Boolean; var newName : TRawBytes; var changeType : Boolean; var newType : Word; var errors : String) : Boolean;
    Function UpdateOpSaveFiles(Const SenderAccount : TAccount;var errors: String):Boolean;
    procedure SetDefaultFee(const Value: Int64);
    Procedure OnSenderAccountsChanged(Sender : TObject);
    procedure OnWalletKeysChanged(Sender : TObject);
    procedure CM_WalletChanged(var Msg: TMessage); message CM_PC_WalletKeysChanged;
    Function GetDefaultSenderAccount : TAccount;
    procedure ebAccountKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected
    procedure searchAccount(editBox : TCustomEdit);
  public
    { Public declarations }
    procedure AllButtonsUp;
    Property SenderAccounts : TOrderedCardinalList read FSenderAccounts;
    Property WalletKeys : TWalletKeys read FWalletKeys write SetWalletKeys;
    Property DefaultFee : Int64 read FDefaultFee write SetDefaultFee;
  end;

implementation

uses
  UConst, UOpTransaction, UFRMNewPrivateKeyType, UFRMWalletKeys,
  UCommon, UGUIUtils, UPCDataTypes, ULog;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

Type
  { Created by Herman Schoenfeld as TArrayTool in v2.0
    Moved here from UCommon.pas and renamed in order to be Delphi specific (Delphi will no longer use UCommon.pas) }
  TArrayTool_internal<T> = class
    public
      class procedure Swap(var Values : array of T; Item1Index, Item2Index : Integer);
    end;

var btnAccountName_Pressed, btnAccountKey_Pressed, btnSendTokens_Pressed, btnBuyAccount_Pressed, btnListAccount_Pressed,
    btnDelistAccount_Pressed, btnSaveFiles_Pressed: Boolean;
    Helper:TFileWrap;
    TreeFunc:TreeUtil;
    testNode1,testNode2,testNode3,testNode4,testNode5,testNode6:TreeNode;

{ TArrayTool_internal }

class procedure TArrayTool_internal<T>.Swap(var Values : array of T; Item1Index, Item2Index : Integer);
var temp : T; len, recSize : Integer; itemSize : Integer;
begin
  len := Length(Values);
  recSize := SizeOf(T);
  if (Item1Index < 0) OR (Item1Index > len) then Raise Exception.Create('Invalid Parameter: Item1Index out of bounds');
  if (Item2Index < 0) OR (Item2Index > len) then Raise Exception.Create('Invalid Parameter: Item2Index out of bounds');
  temp := Values[Item1Index];
  Values[Item1Index] := Values[Item2Index];
  Values[Item2Index] := temp;
end;

{ TFRMOperation }

procedure TFRMOperation.AllButtonsUp;
const CT_MAX_FILES  = (1 shl 10) - 1;
begin
  shapeChangeAccount.Visible := False;
  shapeSendTokens.Visible := False;
  shapeChangeKey.Visible := False;
  shapeBuyAccount.Visible := False;
  shapeListAccount.Visible := False;
  shapeDelistAccount.Visible := False;
  shapeSaveFiles.Visible := False;

  shapeAccountName2.Visible := False;
  shapeSendTokens2.Visible := False;
  shapeChangeKey2.Visible := False;
  shapeBuyAccount2.Visible := False;
  shapeListAccount2.Visible := False;
  shapeDelistAccount2.Visible := False;
  shapeSaveFiles2.Visible := False;

  btnAccountName.Brush.Color := BTN_HIGHLIGHT_COLOR;
  btnAccountName.Pen.Width := 1;
  btnAccountName_Pressed := False;
  lbl_btnAccountName.Font.Style := [];

  btnSendTokens.Brush.Color := BTN_HIGHLIGHT_COLOR;
  btnSendTokens.Pen.Width := 1;
  btnSendTokens_Pressed := False;
  lbl_btnSendTokens.Font.Style := [];

  btnAccountKey.Brush.Color := BTN_HIGHLIGHT_COLOR;
  btnAccountKey.Pen.Width := 1;
  btnAccountKey_Pressed := False;
  lbl_btnAccountKey.Font.Style := [];

  btnBuyAccount.Brush.Color := BTN_HIGHLIGHT_COLOR;
  btnBuyAccount.Pen.Width := 1;
  btnBuyAccount_Pressed := False;
  lbl_btnBuyAccount.Font.Style := [];

  btnListAccount.Brush.Color := BTN_HIGHLIGHT_COLOR;
  btnListAccount.Pen.Width := 1;
  btnListAccount_Pressed := False;
  lbl_btnListAccount.Font.Style := [];

  btnDelistAccount.Brush.Color := BTN_HIGHLIGHT_COLOR;
  btnDelistAccount.Pen.Width := 1;
  btnDelistAccount_Pressed := False;
  lbl_btnDelistAccount.Font.Style := [];

  btnSaveFiles.Brush.Color := BTN_HIGHLIGHT_COLOR;
  btnSaveFiles.Pen.Width := 1;
  btnSaveFiles_Pressed := False;
  lbl_btnSaveFiles.Font.Style := [];


  // efficient file storage (serialized binary tree) helpers initialization
  TreeFunc:= TreeUtil.Create();
  Helper.FileTree := TreeFunc.CreateTree();
  Helper.FileCount := 0 ;
  Helper.FileNames := TStringHashMap.Create(CT_MAX_FILES);
  UploadEdit.Text := '' ;
end;

procedure TFRMOperation.actExecuteExecute(Sender: TObject);
Var errors : String;
  P : PAccount;
  i,iAcc : Integer;
  wk : TWalletKey;
  ops : TOperationsHashTree;
  op : TABEYOperation;
  account,signerAccount,destAccount,accountToBuy : TAccount;
  operation_to_string, operationstxt, auxs : String;
  _amount,_fee, _totalamount, _totalfee, _totalSignerFee, _salePrice : Int64;
  _lockedUntil, _signer_n_ops : Cardinal;
  dooperation : Boolean;
  _newOwnerPublicKey : TECDSA_Public;
  _newName : TRawBytes;
  _newType : Word;
  _changeName, _changeType, _V2, _executeSigner  : Boolean;
  _senderAccounts : TCardinalsArray;
  c:Cardinal;
label loop_start;
begin
  if Not Assigned(WalletKeys) then raise Exception.Create('No wallet keys');
  If Not UpdateOperationOptions(errors) then raise Exception.Create(errors);
  ops := TOperationsHashTree.Create;
  Try
    _V2 := FNode.Bank.Vault.CurrentProtocol >= CT_PROTOCOL_2;
    _totalamount := 0;
    _totalfee := 0;
    _totalSignerFee := 0;
    _signer_n_ops := 0;
    operationstxt := '';
    operation_to_string := '';

    // Compile FSenderAccounts into a reorderable array
    _senderAccounts := FSenderAccounts.ToArray;

    // Loop through each sender account
    for iAcc := 0 to Length(_senderAccounts) - 1 do begin
loop_start:
      op := Nil;
      account := FNode.GetMempoolAccount(_senderAccounts[iAcc]);
      If Not UpdatePayload(account, errors) then
        raise Exception.Create('Error encoding payload of sender account '+TAccountComp.AccountNumberToAccountTxtNumber(account.account)+': '+errors);
      i := WalletKeys.IndexOfAccountKey(account.accountInfo.accountKey);
      if i<0 then begin
        Raise Exception.Create('Sender account private key not found in Wallet');
      end;

      wk := WalletKeys.Key[i];
      dooperation := true;
      // Default fee
      if account.balance > uint64(DefaultFee) then _fee := DefaultFee else _fee := account.balance;
      // Determine which operation type it is
      if {PageControlOpType.ActivePage = tsTransaction} Notebook.PageIndex = PAGE_SENDTOKENS then begin
        {%region Operation: Transaction}
        if Not UpdateOpTransaction(account,destAccount,_amount,errors) then raise Exception.Create(errors);
        if Length(_senderAccounts) > 1 then begin
          if account.balance>0 then begin
            if account.balance>DefaultFee then begin
              _amount := account.balance - DefaultFee;
              _fee := DefaultFee;
            end else begin
              _amount := account.balance;
              _fee := 0;
            end;
          end else dooperation := false;
        end else begin
        end;
        if dooperation then begin
          op := TOpTransaction.CreateTransaction(FNode.Bank.Vault.CurrentProtocol,account.account,account.n_operation+1,destAccount.account,wk.PrivateKey,_amount,_fee,FEncodedPayload);
          inc(_totalamount,_amount);
          inc(_totalfee,_fee);
        end;
        operationstxt := 'Transaction to '+TAccountComp.AccountNumberToAccountTxtNumber(destAccount.account);
        {%endregion}
      end
      else if {PageControlOpType.ActivePage = tsTransaction} Notebook.PageIndex = PAGE_SAVEFILES then begin
        {%region Operation: Transaction}
        if Not UpdateOpSaveFiles(account,errors) then raise Exception.Create(errors);


        {if Length(_senderAccounts) > 1 then begin
          if account.balance>0 then begin
            if account.balance>DefaultFee then begin
              _amount := account.balance - DefaultFee;
              _fee := DefaultFee;
            end else begin
              _amount := account.balance;
              _fee := 0;
            end;
          end else dooperation := false;
        end else begin


        end;    }
        //TAccountComp.AccountTxtNumberToAccountNumber(ebAccount.Text,c) ;
        //account := FNode.GetMempoolAccount(c) ;
        if dooperation then begin
          UpdatePayloadSaveFiles(account);
          ShowMessage('n_operation: '+IntToStr(account.n_operation));
          //op:= TOpSaveFiles.CreateSaveFiles(FNode.Bank.Vault.CurrentProtocol,account.account,account.n_operation+1,account.account,wk.PrivateKey,_fee,FEncodedPayload);
        end;
        operationstxt := 'Transaction to '+TAccountComp.AccountNumberToAccountTxtNumber(destAccount.account);
        {%endregion}
      end else if {(PageControlOpType.ActivePage = tsChangePrivateKey)} Notebook.PageIndex = PAGE_CHANGEKEY then begin
        {%region Operation: Change Private Key}
        if Not UpdateOpChangeKey(account,signerAccount,_newOwnerPublicKey,errors) then raise Exception.Create(errors);
        if _V2 then begin
          // must ensure is Signer account last if included in sender accounts (not necessarily ordered enumeration)
          if (iAcc < Length(_senderAccounts) - 1) AND (account.account = signerAccount.account) then begin
            TArrayTool_internal<Cardinal>.Swap(_senderAccounts, iAcc, Length(_senderAccounts) - 1); // ensure signer account processed last
            goto loop_start; // TODO: remove ugly hack with refactoring!
          end;

          // Maintain correct signer fee distribution
          if uint64(_totalSignerFee) >= signerAccount.balance then _fee := 0
          else if signerAccount.balance - uint64(_totalSignerFee) > uint64(DefaultFee) then _fee := DefaultFee
          else _fee := signerAccount.balance - uint64(_totalSignerFee);
          op := TOpChangeKeySigned.Create(FNode.Bank.Vault.CurrentProtocol,signerAccount.account,signerAccount.n_operation+_signer_n_ops+1,account.account,wk.PrivateKey,_newOwnerPublicKey,_fee,FEncodedPayload);
          inc(_signer_n_ops);
          inc(_totalSignerFee, _fee);
        end else begin
          op := TOpChangeKey.Create(FNode.Bank.Vault.CurrentProtocol,account.account,account.n_operation+1,account.account,wk.PrivateKey,_newOwnerPublicKey,_fee,FEncodedPayload);
        end;
        inc(_totalfee,_fee);
        operationstxt := 'Change private key to '+TAccountComp.GetECInfoTxt(_newOwnerPublicKey.EC_OpenSSL_NID);
        {%endregion}
      end else if {(PageControlOpType.ActivePage = tsListForSale)}Notebook.PageIndex = PAGE_LISTACCOUNT then begin
        {%region Operation: List For Sale}
        If Not UpdateOpListForSale(account,_salePrice,destAccount,signerAccount,_newOwnerPublicKey,_lockedUntil,errors) then raise Exception.Create(errors);
        // Special fee account:
        if signerAccount.balance>DefaultFee then _fee := DefaultFee
        else _fee := signerAccount.balance;
        if (rbListAccountForPublicSale.Checked) then begin
          op := TOpListAccountForSale.CreateListAccountForSale(FNode.Bank.Vault.CurrentProtocol,signerAccount.account,signerAccount.n_operation+1+iAcc, account.account,_salePrice,_fee,destAccount.account,CT_TECDSA_Public_Nul,0,wk.PrivateKey,FEncodedPayload);
        end else if (rbListAccountForPrivateSale.Checked) then begin
          op := TOpListAccountForSale.CreateListAccountForSale(FNode.Bank.Vault.CurrentProtocol,signerAccount.account,signerAccount.n_operation+1+iAcc, account.account,_salePrice,_fee,destAccount.account,_newOwnerPublicKey,_lockedUntil,wk.PrivateKey,FEncodedPayload);
        end else raise Exception.Create('Select Sale type');
        {%endregion}
      end else if {(PageControlOpType.ActivePage = tsDelist)}Notebook.PageIndex = PAGE_DELISTACCOUNT then begin
        {%region Operation: Delist For Sale}
        if Not UpdateOpDelist(account,signerAccount,errors) then raise Exception.Create(errors);
        // Special fee account:
        if signerAccount.balance>DefaultFee then _fee := DefaultFee
        else _fee := signerAccount.balance;
        op := TOpDelistAccountForSale.CreateDelistAccountForSale(FNode.Bank.Vault.CurrentProtocol,signerAccount.account,signerAccount.n_operation+1+iAcc,account.account,_fee,wk.PrivateKey,FEncodedPayload);
        {%endregion}
      end else if {(PageControlOpType.ActivePage = tsBuyAccount)}Notebook.PageIndex = PAGE_BUYACCOUNT then begin
        {%region Operation: Buy Account}
        if Not UpdateOpBuyAccount(account,accountToBuy,_amount,_newOwnerPublicKey,errors) then raise Exception.Create(errors);
        op := TOpBuyAccount.CreateBuy(FNode.Bank.Vault.CurrentProtocol,account.account,account.n_operation+1,accountToBuy.account,accountToBuy.accountInfo.account_to_pay,
          accountToBuy.accountInfo.price,_amount,_fee,_newOwnerPublicKey,wk.PrivateKey,FEncodedPayload);
        {%endregion}
      end else if {(PageControlOpType.ActivePage = tsChangeInfo)}Notebook.PageIndex = PAGE_ACCOUNTNAME then begin
        {%region Operation: Change Info}
        if not UpdateOpChangeInfo(account,signerAccount,_changeName,_newName,_changeType,_newType,errors) then begin
          If Length(_senderAccounts)=1 then raise Exception.Create(errors);
        end else begin
          if signerAccount.balance>DefaultFee then _fee := DefaultFee
          else _fee := signerAccount.balance;
          op := TOpChangeAccountInfo.CreateChangeAccountInfo(FNode.Bank.Vault.CurrentProtocol,signerAccount.account,signerAccount.n_operation+1,account.account,wk.PrivateKey,false,CT_TECDSA_Public_Nul,
             _changeName,_newName,_changeType,_newType,_fee,FEncodedPayload);
        end;
        {%endregion}
      end else begin
        raise Exception.Create('No operation selected!');
      end;
      if Assigned(op) And (dooperation) then begin
        ops.AddOperationToHashTree(op);
        if operation_to_string<>'' then operation_to_string := operation_to_string + #10;
        operation_to_string := operation_to_string + op.ToString;
      end;
      FreeAndNil(op);
    end;

    if (ops.OperationsCount=0) then raise Exception.Create('No valid transaction to queue!');

    if (Length(_senderAccounts)>1) then begin
      if {PageControlOpType.ActivePage = tsTransaction}Notebook.PageIndex = PAGE_SENDTOKENS then auxs := 'Total amount that recipient will receive: '+TAccountComp.FormatMoney(_totalamount)+#10
      else auxs:='';
      if Application.MessageBox(PChar('Are you sure you want to initiate '+Inttostr(Length(_senderAccounts))+' transactions?'+#10+
        'Transactions: '+operationstxt+#10+
        auxs+
        'Total fee: '+TAccountComp.FormatMoney(_totalfee)+#10+#10+'Please note, these transactions will be transmitted (queued) in the blockchain!'),
        PChar(Application.Title),MB_YESNO+MB_ICONINFORMATION+MB_DEFBUTTON2)<>IdYes then exit;
    end else begin
      if Application.MessageBox(PChar('Are you sure you want to queue this transaction:'+#10+#10+operation_to_string+'?'+#10+#10+'Please note, this transaction will be transmitted (queue) in the blockchain!'),
        PChar(Application.Title),MB_YESNO+MB_ICONINFORMATION+MB_DEFBUTTON2)<>IdYes then exit;
    end;
    i := FNode.AddOperations(nil,ops,Nil,errors);
    if (i=ops.OperationsCount) then begin
      operationstxt := 'You have queued '+inttostr(i)+' transactions!'+#10+#10+operation_to_string;
      If i>1 then begin
        With TFRMMemoText.Create(Self) do
        Try
          InitData(Application.Title,operationstxt);
          ShowModal;
        finally
          Free;
        end;
      end else begin
        Application.MessageBox(PChar('You have queued '+inttostr(i)+' transactions!'+#10+#10+operation_to_string),PChar(Application.Title),MB_OK+MB_ICONINFORMATION);
      end;
      ModalResult := MrOk;
    end else if (i>0) then begin
      operationstxt := 'One or more of your transactions has not been queued:'+#10+
        'Errors:'+#10+
        errors+#10+#10+
        'Total successfully queued transactions: '+inttostr(i);
      With TFRMMemoText.Create(Self) do
      Try
        InitData(Application.Title,operationstxt);
        ShowModal;
      finally
        Free;
      end;
      ModalResult := MrOk;
    end else begin
      raise Exception.Create(errors);
    end;
  Finally
    ops.Free;
  End;
end;

procedure TFRMOperation.bbBuyNewKeyClick(Sender: TObject);
Var FRM : TFRMWalletKeys;
begin
  FRM := TFRMWalletKeys.Create(Self);
  Try
    FRM.WalletKeys := WalletKeys;
    FRM.ShowModal;
    cbBuyNewKey.SetFocus;
    UpdateWalletKeys;
  Finally
    FRM.Free;
  End;
end;

procedure TFRMOperation.bbChangePrivateKeyKeysClick(Sender: TObject);
Var FRM : TFRMWalletKeys;
begin
  FRM := TFRMWalletKeys.Create(Self);
  Try
    FRM.WalletKeys := WalletKeys;
    FRM.ShowModal;
    rbChangeKeyWithAnother.Checked := true;
    cbNewPrivateKey.SetFocus;
    UpdateWalletKeys;
  Finally
    FRM.Free;
  End;
end;

procedure TFRMOperation.bbPasswordClick(Sender: TObject);
Var s : String;
  errors : String;
begin
  if FWalletKeys.IsValidPassword then begin
  end else begin
    s := '';
    Repeat
      if Not InputQueryPassword('Password Required','Enter your wallet''s password:',s) then exit;
      FWalletKeys.WalletPassword := s;
    Until FWalletKeys.IsValidPassword;
    SetWalletKeys(WalletKeys);
    UpdateOperationOptions(errors);
  end;
end;

procedure TFRMOperation.PageControlLockedChange(Sender: TObject);
begin

end;

procedure TFRMOperation.CM_WalletChanged(var Msg: TMessage);
begin
   UpdateWalletKeys;
end;

procedure TFRMOperation.ebAccountNumberExit(Sender: TObject);
Var an : Cardinal;
  eb : TEdit;
begin
  if (Not assigned(Sender)) then exit;
  if (Not (Sender is TEdit)) then exit;
  eb := TEdit(Sender);
  If TAccountComp.AccountTxtNumberToAccountNumber(eb.Text,an) then begin
    eb.Text := TAccountComp.AccountNumberToAccountTxtNumber(an);
  end else begin
    eb.Text := '';
  end;
  updateInfoClick(Nil);
end;

procedure TFRMOperation.ebCurrencyExit(Sender: TObject);
Var m : Int64;
  eb : TEdit;
begin
  if (Not assigned(Sender)) then exit;
  if (Not (Sender is TEdit)) then exit;
  eb := TEdit(Sender);
  If Not (eb.ReadOnly) then begin
    if Not TAccountComp.TxtToMoney(eb.Text,m) then m:=0;
    eb.Text := TAccountComp.FormatMoney(m);
    updateInfoClick(Nil);
  end;
end;

procedure TFRMOperation.UploadFilesButtonClick(Sender: TObject);
var
  FileName,FilePath: string;
  tempNode: TreeNode;
  I:Integer;
  ErrorCode:Integer;
  ErrorMessage:String;

begin
  //no files
  //index of each file
if OpenDialogSaveFiles.Execute then
   begin

      for I := 0 to OpenDialogSaveFiles.Files.Count - 1 do begin
         //ShowMessage(OpenDialogSaveFiles.Files[I]);

         FilePath := OpenDialogSaveFiles.Files[I];
         FileName := UTF8ToAnsi(ExtractFileName(FilePath));

         //ShowMessage('Filename' + Filename);
         //ShowMessage('FilePath' + FilePath);

         if Helper.FileNames.Has(FileName) = True then begin
          raise Exception.Create('Cannot add ' + FileName +' twice.Duplicates are ignored!');
          Continue;
         end;

         Helper.FileNames[FileName] := @FilePath;
         UploadEdit.Text:= UploadEdit.Text + ' ' +FileName;
         Helper.FileCount +=1;
         SetLength(Helper.NodeArray,Helper.FileCount);

         if TreeFunc.CreateFileNode(FileName,FilePath ,Helper.NodeArray[ High( Helper.NodeArray ) ],ErrorMessage,ErrorCode) <> True then begin
            Helper.FileCount -=1;         //substract 1
            SetLength(Helper.NodeArray,Helper.FileCount);  // delete last element created
            raise Exception.Create(ErrorMessage);
         end;

      end;

     end;
end;

procedure TFRMOperation.ebEncryptPasswordChange(Sender: TObject);
begin
  if FDisabled then exit;
  rbEncrptedWithPassword.Checked := true;
  memoPayloadClick(Nil);
end;

procedure TFRMOperation.ebSenderAccountExit(Sender: TObject);
Var an : Cardinal;
begin
  If TAccountComp.AccountTxtNumberToAccountNumber(ebSenderAccount.Text,an) then begin
    SenderAccounts.Disable;
    try
      SenderAccounts.Clear;
      SenderAccounts.Add(an);
    finally
      SenderAccounts.Enable;
    end;
    ebSenderAccount.Text := TAccountComp.AccountNumberToAccountTxtNumber(an);
    lbl_AccountID.Caption := TAccountComp.AccountNumberToAccountTxtNumber(an);
  end else begin
    if SenderAccounts.Count=1 then begin
      ebSenderAccount.Text := TAccountComp.AccountNumberToAccountTxtNumber(SenderAccounts.Get(0));
      lbl_AccountID.Caption := TAccountComp.AccountNumberToAccountTxtNumber(SenderAccounts.Get(0));
    end else begin
      ebSenderAccount.Text := '';
      lbl_AccountID.Caption := 'N/A';
    end;
  end;
end;

procedure TFRMOperation.ebSenderAccountKeyPress(Sender: TObject; var Key: Char);
begin
  if Key=#13 then ebSenderAccountExit(Nil);
end;

procedure TFRMOperation.FormCreate(Sender: TObject);
begin
  AllButtonsUp;
  imgAccountNameClick(nil);

  FDisabled := false;
  FWalletKeys := Nil;
  FSenderAccounts := TOrderedCardinalList.Create;
  FSenderAccounts.OnListChanged := OnSenderAccountsChanged;
  FDisabled := true;
  FNode := TNode.Node;
  ebSenderAccount.OnKeyDown:=ebAccountKeyDown;
  ebSenderAccount.Tag:=CT_AS_MyAccounts;
  ebSignerAccount.Text:='';
  ebSignerAccount.OnChange := updateInfoClick;
  ebSignerAccount.OnExit := ebAccountNumberExit;
  ebSignerAccount.OnKeyDown := ebAccountKeyDown;
  ebSignerAccount.tag := CT_AS_MyAccounts;
  sbSearchSignerAccount.OnClick := sbSearchSignerAccountClick;

  //
  lblTransactionErrors.Caption := '';
  ebDestAccount.Text := '';
  ebDestAccount.OnChange := updateInfoClick;
  ebDestAccount.OnExit := ebAccountNumberExit;
  ebDestAccount.OnKeyDown := ebAccountKeyDown;
  ebAmount.Text := TAccountComp.FormatMoney(0);
  ebAmount.OnChange := updateInfoClick;
  ebAmount.OnExit := ebCurrencyExit;
  //
  lblChangeKeyErrors.Caption := '';
  lblNewOwnerErrors.Caption := '';
  rbChangeKeyWithAnother.OnClick := updateInfoClick;
  rbChangeKeyTransferAccountToNewOwner.OnClick := updateInfoClick;
  cbNewPrivateKey.OnChange := updateInfoClick;
  //
  lblListAccountErrors.Caption := '';
  rbListAccountForPublicSale.OnClick := updateInfoClick;
  rbListAccountForPrivateSale.OnClick := updateInfoClick;
  ebSalePrice.Text := TAccountComp.FormatMoney(0);
  ebSalePrice.OnChange := updateInfoClick;
  ebSalePrice.OnExit := ebCurrencyExit;

  ebSellerAccount.Text := '';
  ebSellerAccount.OnChange := updateInfoClick;
  ebSellerAccount.OnExit := ebAccountNumberExit;
  ebSellerAccount.OnKeyDown := ebAccountKeyDown;
  ebSellerAccount.tag := CT_AS_MyAccounts;
  ebSaleNewOwnerPublicKey.Text := '';
  ebSaleNewOwnerPublicKey.OnChange := updateInfoClick;
  ebSaleLockedUntilBlock.Text := '';
  ebSaleLockedUntilBlock.OnChange := updateInfoClick;

  //
  lblDelistErrors.Caption := '';
  //
  lblBuyAccountErrors.Caption := '';
  ebAccountToBuy.Text := '';
  ebAccountToBuy.OnChange :=  updateInfoClick;
  ebAccountToBuy.OnExit := ebAccountNumberExit;
  ebAccountToBuy.OnKeyDown := ebAccountKeyDown;
  ebAccountToBuy.tag := CT_AS_OnlyForSale;
  ebBuyAmount.Text := TAccountComp.FormatMoney(0);
  ebBuyAmount.OnChange :=  updateInfoClick;
  ebBuyAmount.OnExit := ebCurrencyExit;
  //
  ebChangeName.OnChange:=updateInfoClick;
  ebChangeType.OnChange:=updateInfoClick;
  //
  sbSearchDestinationAccount.OnClick := sbSearchDestinationAccountClick;
  sbSearchListerSellerAccount.OnClick := sbSearchListerSellerAccountClick;
  sbSearchBuyAccount.OnClick := sbSearchBuyAccountClick;
  //
  ebFee.Text := TAccountComp.FormatMoney(0);
  ebFee.OnExit:= ebCurrencyExit;
  memoAccounts.Lines.Clear;
  PageControlOpType.ActivePage := tsTransaction;
  imgAccountNameClick(nil);
end;

procedure TFRMOperation.ebNewPublicKeyExit(Sender: TObject);
var errors : String;
begin
  UpdateOperationOptions(errors);
end;

procedure TFRMOperation.ebSenderAccountChange(Sender: TObject);
begin

end;

procedure TFRMOperation.ebSignerAccountChange(Sender: TObject);
begin

end;

procedure TFRMOperation.comboUserTypesChange(Sender: TObject);
begin
  ebChangeType.Text := IntToStr(comboUserTypes.ItemIndex);
end;

procedure TFRMOperation.ebAmountChange(Sender: TObject);
begin

end;

procedure TFRMOperation.ebAccountChange(Sender: TObject);
begin

end;

procedure TFRMOperation.ebDestAccountChange(Sender: TObject);
begin

end;

procedure TFRMOperation.ebFeeChange(Sender: TObject);
begin

end;

procedure TFRMOperation.btnSaveFilesChangeBounds(Sender: TObject);
begin

end;

procedure TFRMOperation.btnListAccountChangeBounds(Sender: TObject);
begin

end;

procedure TFRMOperation.btnBuyAccountChangeBounds(Sender: TObject);
begin

end;

procedure TFRMOperation.bbCancelClick(Sender: TObject);
begin

end;

procedure TFRMOperation.btnKeys17ChangeBounds(Sender: TObject);
begin

end;

procedure TFRMOperation.btnKeys18ChangeBounds(Sender: TObject);
begin

end;

procedure TFRMOperation.btnKeys19ChangeBounds(Sender: TObject);
begin

end;

procedure TFRMOperation.FormDestroy(Sender: TObject);
begin
  if Assigned(FWalletKeys) then FWalletKeys.OnChanged.Remove(OnWalletKeysChanged);
  FreeAndNil(FSenderAccounts);
end;

procedure TFRMOperation.imgSaveFilesClick(Sender: TObject);
begin
  AllButtonsUp;

  shapeSaveFiles.Visible := True;
  shapeSaveFiles2.Visible := True;
  btnSaveFiles_Pressed := True;
  btnSaveFiles.Brush.Color := BTN_PRESSED_COLOR;
  lbl_btnSaveFiles.Font.Style := [fsBold];
  Notebook.PageIndex :=  PAGE_SAVEFILES;

  //UpdateOperationOptions(errors);
end;

procedure TFRMOperation.imgAccountKeyClick(Sender: TObject);
var errors : String;
begin
  AllButtonsUp;

  shapeChangeKey.Visible := True;
  shapeChangeKey2.Visible := True;
  btnAccountKey_Pressed := True;
  btnAccountKey.Brush.Color := BTN_PRESSED_COLOR;
  lbl_btnAccountKey.Font.Style := [fsBold];
  Notebook.PageIndex :=  PAGE_CHANGEKEY;

  UpdateOperationOptions(errors);
end;

procedure TFRMOperation.imgAccountNameClick(Sender: TObject);
var errors : String;
begin
  AllButtonsUp;

  shapeChangeAccount.Visible := True;
  shapeAccountName2.Visible := True;
  btnAccountName_Pressed := True;
  btnAccountName.Brush.Color := BTN_PRESSED_COLOR;
  lbl_btnAccountName.Font.Style := [fsBold];
  Notebook.PageIndex :=  PAGE_ACCOUNTNAME;

  UpdateOperationOptions(errors);
end;

procedure TFRMOperation.imgBuyAccountClick(Sender: TObject);
var errors : String;
begin
  AllButtonsUp;

  shapeBuyAccount.Visible := True;
  shapeBuyAccount2.Visible := True;
  btnBuyAccount_Pressed := True;
  btnBuyAccount.Brush.Color := BTN_PRESSED_COLOR;
  lbl_btnBuyAccount.Font.Style := [fsBold];
  Notebook.PageIndex :=  PAGE_BUYACCOUNT;

  UpdateOperationOptions(errors);
end;

procedure TFRMOperation.imgDelistAccountClick(Sender: TObject);
var errors : String;
begin
  AllButtonsUp;

  shapeDelistAccount.Visible := True;
  shapeDelistAccount2.Visible := True;
  btnDelistAccount_Pressed := True;
  btnDelistAccount.Brush.Color := BTN_PRESSED_COLOR;
  lbl_btnDelistAccount.Font.Style := [fsBold];
  Notebook.PageIndex :=  PAGE_DELISTACCOUNT;

  UpdateOperationOptions(errors);
end;

procedure TFRMOperation.imgListAccountClick(Sender: TObject);
var errors : String;
begin
  AllButtonsUp;

  shapeListAccount.Visible := True;
  shapeListAccount2.Visible := True;
  btnListAccount_Pressed := True;
  btnListAccount.Brush.Color := BTN_PRESSED_COLOR;
  lbl_btnListAccount.Font.Style := [fsBold];
  Notebook.PageIndex :=  PAGE_LISTACCOUNT;

  UpdateOperationOptions(errors);
end;

procedure TFRMOperation.imgSendTokensClick(Sender: TObject);
var errors : String;
begin
  AllButtonsUp;

  shapeSendTokens.Visible := True;
  shapeSendTokens2.Visible := True;
  btnSendTokens_Pressed := True;
  btnSendTokens.Brush.Color := BTN_PRESSED_COLOR;
  lbl_btnSendTokens.Font.Style := [fsBold];
  Notebook.PageIndex :=  PAGE_SENDTOKENS;

  UpdateOperationOptions(errors);
end;

procedure TFRMOperation.lblAccountBalanceClick(Sender: TObject);
begin

end;

procedure TFRMOperation.lblAccountClick(Sender: TObject);
begin

end;

procedure TFRMOperation.lblAccountsCountClick(Sender: TObject);
begin

end;

procedure TFRMOperation.lblSaveFilesErrorsClick(Sender: TObject);
begin

end;

procedure TFRMOperation.lbl_AccountIDClick(Sender: TObject);
begin

end;

procedure TFRMOperation.lbl_btnAccountNameClick(Sender: TObject);
begin

end;

procedure TFRMOperation.lbl_btnBuyAccountClick(Sender: TObject);
begin

end;

procedure TFRMOperation.lbl_btnDelistAccountClick(Sender: TObject);
begin

end;

procedure TFRMOperation.lbl_btnSaveFilesClick(Sender: TObject);
begin

end;

procedure TFRMOperation.memoAccountsChange(Sender: TObject);
begin

end;

function TFRMOperation.GetDefaultSenderAccount: TAccount;
begin
  if FSenderAccounts.Count>=1 then Result := FNode.GetMempoolAccount( FSenderAccounts.Get(0) )
  else Result := CT_Account_NUL;
end;

procedure TFRMOperation.ebAccountKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
Var eb : TCustomEdit;
begin
  If (key <> VK_F2) then exit;
  If Not Assigned(Sender) then exit;
  if Not (Sender is TCustomEdit) then exit;
  eb := TCustomEdit(Sender);
  searchAccount(eb);
end;

procedure TFRMOperation.searchAccount(editBox: TCustomEdit);
Var F : TFRMAccountSelect;
  c : Cardinal;
begin
  F := TFRMAccountSelect.Create(Self);
  try
    F.Node := FNode;
    F.WalletKeys := FWalletKeys;
    F.Filters:=editBox.Tag;
    If TAccountComp.AccountTxtNumberToAccountNumber(editBox.Text,c) then F.DefaultAccount := c;
    F.AllowSelect:=True;
    If F.ShowModal=MrOk then begin
      editBox.Text := TAccountComp.AccountNumberToAccountTxtNumber(F.GetSelected);
    end;
  finally
    F.Free;
  end;
end;

procedure TFRMOperation.memoPayloadClick(Sender: TObject);
Var errors : String;
begin
  if SenderAccounts.Count>0 then begin
    UpdatePayload(TNode.Node.Bank.Vault.Account(SenderAccounts.Get(0)),errors);
  end;
end;

procedure TFRMOperation.OnSenderAccountsChanged(Sender: TObject);
Var errors : String;
begin
  if SenderAccounts.Count>1 then begin
    ebAmount.Text := 'ALL BALANCE';
    ebAmount.font.Style := [fsBold];
    ebAmount.ReadOnly := true;
  end else begin
    ebAmount.Text := TAccountComp.FormatMoney(0);
    ebAmount.ReadOnly := false;
    ebAmount.Enabled := true;
  end;
  If SenderAccounts.Count>=1 then begin
    ebSignerAccount.text := TAccountComp.AccountNumberToAccountTxtNumber(SenderAccounts.Get(0));
    ebChangeName.Text := FNode.GetMempoolAccount(SenderAccounts.Get(0)).name.ToPrintable;
    ebChangeType.Text := IntToStr(FNode.GetMempoolAccount(SenderAccounts.Get(0)).account_type);
  end else begin
    ebSignerAccount.text := '';
    ebChangeName.Text := '';
    ebChangeType.Text := '';
  end;
  UpdateAccountsInfo;
  UpdateOperationOptions(errors);
end;

procedure TFRMOperation.OnWalletKeysChanged(Sender: TObject);
begin
  PostMessage(Self.Handle,CM_PC_WalletKeysChanged,0,0);
end;

procedure TFRMOperation.PageControlOpTypeChange(Sender: TObject);
var errors : String;
begin
  UpdateOperationOptions(errors);
end;

procedure TFRMOperation.SaveFilesBeforeShow(ASender: TObject; ANewPage: TPage;
  ANewIndex: Integer);
begin

end;

procedure TFRMOperation.sbSearchBuyAccountClick(Sender: TObject);
begin
  searchAccount(ebAccountToBuy);
end;

procedure TFRMOperation.sbSearchDestinationAccountClick(Sender: TObject);
begin
  searchAccount(ebDestAccount);
end;

procedure TFRMOperation.sbSearchListerSellerAccountClick(Sender: TObject);
begin
  searchAccount(ebSellerAccount);
end;

procedure TFRMOperation.sbSearchSignerAccountClick(Sender: TObject);
begin
  searchAccount(ebSignerAccount);
end;

procedure TFRMOperation.SendTokensBeforeShow(ASender: TObject; ANewPage: TPage;
  ANewIndex: Integer);
begin

end;

procedure TFRMOperation.Shape16ChangeBounds(Sender: TObject);
begin

end;

procedure TFRMOperation.Shape26ChangeBounds(Sender: TObject);
begin

end;

procedure TFRMOperation.Shape3ChangeBounds(Sender: TObject);
begin

end;

procedure TFRMOperation.Shape9ChangeBounds(Sender: TObject);
begin

end;

procedure TFRMOperation.shapeAccountName2ChangeBounds(Sender: TObject);
begin

end;

procedure TFRMOperation.shapeChangeAccountChangeBounds(Sender: TObject);
begin

end;

procedure TFRMOperation.shapeDelistAccount3ChangeBounds(Sender: TObject);
begin
  end;

procedure TFRMOperation.shapeSaveFilesChangeBounds(Sender: TObject);
begin

end;

procedure TFRMOperation.shapeSaveFiles2ChangeBounds(Sender: TObject);
begin

end;

procedure TFRMOperation.shapeListAccount2ChangeBounds(Sender: TObject);
begin


end;

procedure TFRMOperation.shapeListAccountChangeBounds(Sender: TObject);
begin

end;

procedure TFRMOperation.shapeSendTokens2ChangeBounds(Sender: TObject);
begin

end;

procedure TFRMOperation.shapeSendTokensChangeBounds(Sender: TObject);
begin

end;

procedure TFRMOperation.StaticText1Click(Sender: TObject);
begin

end;

procedure TFRMOperation.SetDefaultFee(const Value: Int64);
var wd : Boolean;
begin
  if FDefaultFee = Value then exit;
  wd := FDisabled;
  try
    FDisabled := true;
    FDefaultFee := Value;
    ebFee.Text := TAccountComp.FormatMoney(value);
  finally
    FDisabled := wd;
  end;
end;

procedure TFRMOperation.SetWalletKeys(const Value: TWalletKeys);
begin
  Try
    if FWalletKeys=Value then exit;
    if Assigned(FWalletKeys) then FWalletKeys.OnChanged.Remove(OnWalletKeysChanged);
    FWalletKeys := Value;
    if Assigned(FWalletKeys) then begin
      FWalletKeys.OnChanged.Add(OnWalletKeysChanged);
    end;
  Finally
    UpdateWalletKeys;
  End;
end;

procedure TFRMOperation.UpdateAccountsInfo;
Var ld : Boolean;
  i : Integer;
  balance : int64;
  acc : TAccount;
  accountstext : String;
begin
  ld := FDisabled;
  FDisabled := true;
  Try
    lblAccountCaption.Caption := 'Account';
    lblAccountsCount.Visible := false;
    lblAccountsCount.caption := inttostr(senderAccounts.Count)+' accounts';
    balance := 0;
    if SenderAccounts.Count<=0 then begin
      ebSenderAccount.Text := '';
      //memoAccounts.Visible := false;
      //ebSenderAccount.Visible := true;
    end else if SenderAccounts.Count=1 then begin
      ebSenderAccount.Text := TAccountComp.AccountNumberToAccountTxtNumber(SenderAccounts.Get(0));
      //memoAccounts.Visible := false;
      //ebSenderAccount.Visible := true;
      balance := TNode.Node.GetMempoolAccount(SenderAccounts.Get(0)).balance;
    end else begin
      // Multiple sender accounts
      lblAccountCaption.Caption := 'Accounts';
      //lblAccountsCount.Visible := true;
      //ebSenderAccount.Visible := false;
      accountstext := '';
      for i := 0 to SenderAccounts.Count - 1 do begin
         acc := TNode.Node.GetMempoolAccount(SenderAccounts.Get(i));
         balance := balance + acc.balance;
         if (accountstext<>'') then accountstext:=accountstext+'; ';
         accountstext := accountstext+TAccountComp.AccountNumberToAccountTxtNumber(acc.account)+' ('+TAccountComp.FormatMoney(acc.balance)+')';
      end;
      memoAccounts.Lines.Text := accountstext;
      memoAccounts.Visible := true;
    end;
    ebSenderAccount.Enabled := ebSenderAccount.Visible;
    lblAccountBalance.Caption := TAccountComp.FormatMoney(balance);
  Finally
    FDisabled := ld;
  End;
end;

function TFRMOperation.UpdateFee(var Fee: Int64; errors: String): Boolean;
begin
  errors := '';
  if trim(ebFee.Text)<>'' then begin
    Result := TAccountComp.TxtToMoney(Trim(ebFee.Text),Fee);
    if not Result then errors := 'Invalid fee value "'+ebFee.Text+'"';
  end else begin
    Fee := 0;
    Result := true;
  end;
end;

procedure TFRMOperation.updateInfoClick(Sender: TObject);
Var errors : String;
begin
  UpdateOperationOptions(errors);
end;

function TFRMOperation.UpdateOpBuyAccount(const SenderAccount: TAccount; var AccountToBuy: TAccount; var amount: Int64; var NewPublicKey: TAccountKey; var errors: String): Boolean;
var c : Cardinal;
  i : Integer;
begin
  //
  lblBuyAccountErrors.Caption := ''; c:=0;
  errors := '';
  Try
    if SenderAccounts.Count<>1 then begin
      errors := 'Cannot buy accounts with multioperations. Use only 1 account';
      exit;
    end;
    If (Not TAccountComp.AccountTxtNumberToAccountNumber(ebAccountToBuy.Text,c)) then begin
      errors := 'Invalid account to buy '+ebAccountToBuy.Text;
      exit;
    end;
    If (c<0) Or (c>=FNode.Bank.AccountsCount) Or (c=SenderAccount.account) then begin
      errors := 'Invalid account number';
      exit;
    end;
    AccountToBuy := FNode.GetMempoolAccount(c);
    If not TAccountComp.IsAccountForSale(AccountToBuy.accountInfo) then begin
      errors := 'Account '+TAccountComp.AccountNumberToAccountTxtNumber(c)+' is not for sale';
      exit;
    end;
    If Not TAccountComp.TxtToMoney(ebBuyAmount.Text,amount) then begin
      errors := 'Invalid amount value';
      exit;
    end;
    if (AccountToBuy.accountInfo.price>amount) then begin
      errors := 'Account price '+TAccountComp.FormatMoney(AccountToBuy.accountInfo.price);
      exit;
    end;
    if (amount+DefaultFee > SenderAccount.balance) then begin
      errors := 'Insufficient funds';
      exit;
    end;
    if cbBuyNewKey.ItemIndex<0 then begin
      errors := 'Must select a new private key';
      exit;
    end;
    i := PtrInt(cbBuyNewKey.Items.Objects[cbBuyNewKey.ItemIndex]);
    if (i<0) Or (i>=WalletKeys.Count) then raise Exception.Create('Invalid selected key');
    NewPublicKey := WalletKeys.Key[i].AccountKey;
    If (FNode.Bank.Vault.CurrentProtocol=CT_PROTOCOL_1) then begin
      errors := 'This operation needs PROTOCOL 2 active';
      exit;
    end;
  Finally
    Result := errors = '';
    lblBuyAccountErrors.Caption := errors;
  End;
end;

function TFRMOperation.UpdateOpChangeInfo(const TargetAccount: TAccount; var SignerAccount : TAccount;
   var changeName : Boolean; var newName: TRawBytes; var changeType : Boolean; var newType: Word; var errors: String): Boolean;
var auxC : Cardinal;
  i : Integer;
  errCode : Integer;
begin
  Result := false;
  errors := '';
  lblChangeInfoErrors.Caption:='';
  if not {(PageControlOpType.ActivePage=tsChangeInfo)} Notebook.PageIndex = PAGE_ACCOUNTNAME then exit;
  try
    if (TAccountComp.IsAccountLocked(TargetAccount.accountInfo,FNode.Bank.BlocksCount)) then begin
      errors := 'Account '+TAccountComp.AccountNumberToAccountTxtNumber(TargetAccount.account)+' is locked until block '+IntToStr(TargetAccount.accountInfo.locked_until_block);
      exit;
    end;
    // Signer:
    if SenderAccounts.Count=1 then begin
      If Not TAccountComp.AccountTxtNumberToAccountNumber(ebSignerAccount.Text,auxC) then begin
        errors := 'Invalid signer account';
        exit;
      end;
    end else begin
       auxC := TargetAccount.account;
    end;
    if (auxC<0) Or (auxC >= FNode.Bank.AccountsCount) then begin
      errors := 'Signer account does not exists '+TAccountComp.AccountNumberToAccountTxtNumber(auxC);
      exit;
    end;
    SignerAccount := FNode.GetMempoolAccount(auxC);
    if (TAccountComp.IsAccountLocked(SignerAccount.accountInfo,FNode.Bank.BlocksCount)) then begin
      errors := 'Signer account '+TAccountComp.AccountNumberToAccountTxtNumber(SignerAccount.account)+' is locked until block '+IntToStr(SignerAccount.accountInfo.locked_until_block);
      exit;
    end;
    if (Not TAccountComp.EqualAccountKeys(SignerAccount.accountInfo.accountKey,TargetAccount.accountInfo.accountKey)) then begin
      errors := 'Signer account '+TAccountComp.AccountNumberToAccountTxtNumber(SignerAccount.account)+' is not ower of account '+TAccountComp.AccountNumberToAccountTxtNumber(TargetAccount.account);
      exit;
    end;
    If (FNode.Bank.Vault.CurrentProtocol=CT_PROTOCOL_1) then begin
      errors := 'This operation needs PROTOCOL 2 active';
      exit;
    end;
    // New name and type (only when single operation)
    If (SenderAccounts.Count=1) then begin
      newName.FromString({LowerCase( }Trim(ebChangeName.Text) );
      If Not TBaseType.Equals(newName,TargetAccount.name) then begin
        changeName:=True;
        If Length(newName)>0 then begin
          if (Not TABEYVault.ValidAccountName(newName,errors)) then begin
            errors := '"'+newName.ToPrintable+'" is not a valid name: '+errors;
            Exit;
          end;
          i := (FNode.Bank.Vault.FindAccountByName(newName));
          if (i>=0) then begin
            errors := 'Name "'+newName.ToPrintable+'" is used by account '+TAccountComp.AccountNumberToAccountTxtNumber(i);
            Exit;
          end;
        end;
      end else changeName := False;
    end else changeName := False;
    val(ebChangeType.Text,newType,errCode);
    if (errCode>0) then begin
      errors := 'Invalid type '+ebChangeType.text;
      Exit;
    end;
    changeType := TargetAccount.account_type<>newType;
    //
    If (SenderAccounts.Count=1) And (newName=TargetAccount.name) And (newType=TargetAccount.account_type) then begin
      errors := 'Account name and type are the same. Not changed';
      Exit;
    end;
  finally
    Result := errors = '';
    if Not Result then begin
      lblChangeInfoErrors.Font.Color := clRed;
      lblChangeInfoErrors.Caption := errors;
    end else begin
      lblChangeInfoErrors.Font.Color := clGreen;
      If (SenderAccounts.Count=1) then
        lblChangeInfoErrors.Caption := TAccountComp.AccountNumberToAccountTxtNumber(TargetAccount.account)+' can be updated'
      else lblChangeInfoErrors.Caption := IntToStr(SenderAccounts.Count)+' accounts can be updated'
    end;
  end;
end;

function TFRMOperation.UpdateOpChangeKey(Const TargetAccount : TAccount; var SignerAccount : TAccount; var NewPublicKey: TAccountKey; var errors: String): Boolean;
var i : Integer;
  auxC : Cardinal;
begin
  Result := false;
  errors := '';
  lblChangeKeyErrors.Caption := '';
  lblNewOwnerErrors.Caption := '';
  if not {(PageControlOpType.ActivePage=tsChangePrivateKey)}Notebook.PageIndex = PAGE_CHANGEKEY then exit;
  try
    if rbChangeKeyWithAnother.Checked then begin
      if cbNewPrivateKey.ItemIndex<0 then begin
        errors := 'Must select a new private key';
        lblChangeKeyErrors.Caption := errors;
        exit;
      end;
      i := PtrInt(cbNewPrivateKey.Items.Objects[cbNewPrivateKey.ItemIndex]);
      if (i<0) Or (i>=WalletKeys.Count) then raise Exception.Create('Invalid selected key');
      NewPublicKey := WalletKeys.Key[i].AccountKey;
    end else if rbChangeKeyTransferAccountToNewOwner.Checked then begin
      If Not TAccountComp.AccountKeyFromImport(ebNewPublicKey.Text,NewPublicKey,errors) then begin
        lblNewOwnerErrors.Caption := errors;
        lblNewOwnerErrors.Font.Color := clRed;
        exit;
      end else begin
        lblNewOwnerErrors.Caption := 'New key type: '+TAccountComp.GetECInfoTxt(NewPublicKey.EC_OpenSSL_NID);
        lblNewOwnerErrors.Font.Color := clGreen;
      end;
    end else begin
      errors := 'Select a change type';
      lblChangeKeyErrors.Caption := errors;
      exit;
    end;
    If FNode.Bank.Vault.CurrentProtocol>=1 then begin
      // Signer:
      If Not TAccountComp.AccountTxtNumberToAccountNumber(ebSignerAccount.Text,auxC) then begin
        errors := 'Invalid signer account';
        exit;
      end;
      if (auxC<0) Or (auxC >= FNode.Bank.AccountsCount) then begin
        errors := 'Signer account does not exists '+TAccountComp.AccountNumberToAccountTxtNumber(auxC);
        exit;
      end;
      SignerAccount := FNode.GetMempoolAccount(auxC);
      if (TAccountComp.IsAccountLocked(SignerAccount.accountInfo,FNode.Bank.BlocksCount)) then begin
        errors := 'Signer account '+TAccountComp.AccountNumberToAccountTxtNumber(SignerAccount.account)+' is locked until block '+IntToStr(SignerAccount.accountInfo.locked_until_block);
        exit;
      end;
      if (Not TAccountComp.EqualAccountKeys(SignerAccount.accountInfo.accountKey,TargetAccount.accountInfo.accountKey)) then begin
        errors := 'Signer account '+TAccountComp.AccountNumberToAccountTxtNumber(SignerAccount.account)+' is not ower of account '+TAccountComp.AccountNumberToAccountTxtNumber(TargetAccount.account);
        exit;
      end;
    end else SignerAccount := TargetAccount;
    if (TAccountComp.EqualAccountKeys(TargetAccount.accountInfo.accountKey,NewPublicKey)) then begin
      errors := 'New public key is the same public key';
      lblChangeKeyErrors.Caption := errors;
      lblNewOwnerErrors.Caption := errors;
      exit;
    end;
  finally
    Result := errors = '';
  end;
end;

function TFRMOperation.UpdateOpDelist(const TargetAccount : TAccount; var SignerAccount : TAccount; var errors: String): Boolean;
Var auxC : Cardinal;
begin
  lblDelistErrors.Caption := '';
  errors := '';
  Result := false;
  if not {(PageControlOpType.ActivePage=tsDelist)}Notebook.PageIndex = PAGE_DELISTACCOUNT then exit;
  try
    if Not TAccountComp.IsAccountForSale(TargetAccount.accountInfo) then begin
      errors := 'Account '+TAccountComp.AccountNumberToAccountTxtNumber(TargetAccount.account)+' is not for sale';
      exit;
    end;
    if (TAccountComp.IsAccountLocked(TargetAccount.accountInfo,FNode.Bank.BlocksCount)) then begin
      errors := 'Account '+TAccountComp.AccountNumberToAccountTxtNumber(TargetAccount.account)+' is locked until block '+IntToStr(TargetAccount.accountInfo.locked_until_block);
      exit;
    end;
    // Signer:
    If Not TAccountComp.AccountTxtNumberToAccountNumber(ebSignerAccount.Text,auxC) then begin
      errors := 'Invalid signer account';
      exit;
    end;
    if (auxC<0) Or (auxC >= FNode.Bank.AccountsCount) then begin
      errors := 'Signer account does not exists '+TAccountComp.AccountNumberToAccountTxtNumber(auxC);
      exit;
    end;
    SignerAccount := FNode.GetMempoolAccount(auxC);
    if (TAccountComp.IsAccountLocked(SignerAccount.accountInfo,FNode.Bank.BlocksCount)) then begin
      errors := 'Signer account '+TAccountComp.AccountNumberToAccountTxtNumber(SignerAccount.account)+' is locked until block '+IntToStr(SignerAccount.accountInfo.locked_until_block);
      exit;
    end;
    if (Not TAccountComp.EqualAccountKeys(SignerAccount.accountInfo.accountKey,TargetAccount.accountInfo.accountKey)) then begin
      errors := 'Signer account '+TAccountComp.AccountNumberToAccountTxtNumber(SignerAccount.account)+' is not ower of delisted account '+TAccountComp.AccountNumberToAccountTxtNumber(TargetAccount.account);
      exit;
    end;
    If (FNode.Bank.Vault.CurrentProtocol=CT_PROTOCOL_1) then begin
      errors := 'This operation needs PROTOCOL 2 active';
      exit;
    end;
  finally
    Result := errors = '';
    if Not Result then begin
      lblDelistErrors.Font.Color := clRed;
      lblDelistErrors.Caption := errors;
    end else begin
      lblDelistErrors.Font.Color := clGreen;
      lblDelistErrors.Caption := TAccountComp.AccountNumberToAccountTxtNumber(TargetAccount.account)+' can be delisted';
    end;
  end;
end;

function TFRMOperation.UpdateOperationOptions(var errors : String) : Boolean;
Var
  iWallet,iAcc : Integer;
  wk : TWalletKey;
  e : String;
  sender_account,dest_account,seller_account, account_to_buy, signer_account : TAccount;
  publicKey : TAccountKey;
  salePrice, amount : Int64;
  auxC : Cardinal;
  changeName,changeType : Boolean;
  newName : TRawBytes;
  newType : Word;
begin
  Result := false;
  sender_account := CT_Account_NUL;
  errors := '';

  DebugLn('here');
  if Not UpdateFee(FDefaultFee,errors) then exit;
  try
    bbPassword.Visible := false;
    bbPassword.Enabled := false;
    if Not Assigned(WalletKeys) then begin
      errors := 'No wallet keys';
      lblGlobalErrors.Caption := errors;
      exit;
    end;
    if SenderAccounts.Count=0 then begin
      errors := 'No sender account';
      lblGlobalErrors.Caption := errors;
      exit;
    end else begin
      for iAcc := 0 to SenderAccounts.Count - 1 do begin
        sender_account := TNode.Node.Bank.Vault.Account(SenderAccounts.Get(iAcc));
        iWallet := WalletKeys.IndexOfAccountKey(sender_account.accountInfo.accountKey);
        if (iWallet<0) then begin
          errors := 'Private key of account '+TAccountComp.AccountNumberToAccountTxtNumber(sender_account.account)+' not found in wallet';
          lblGlobalErrors.Caption := errors;
          exit;
        end;
        wk := WalletKeys.Key[iWallet];
        if not assigned(wk.PrivateKey) then begin
          if Length(wk.CryptedKey)>0 then begin
            errors := 'Wallet is password protected. Need password';
            bbPassword.Visible := true;
            bbPassword.Enabled := true;
          end else begin
            errors := 'Only public key of account '+TAccountComp.AccountNumberToAccountTxtNumber(sender_account.account)+' found in wallet. You cannot operate with this account';
          end;
          lblGlobalErrors.Caption := errors;
          exit;
        end;
      end;
    end;
    lblGlobalErrors.Caption := '';
  Finally
    if lblGlobalErrors.Caption<>'' then begin
      tsGlobalError.visible := true;
      tsGlobalError.tabvisible := {$IFDEF LINUX}true{$ELSE}false{$ENDIF};
      tsOperation.TabVisible := false;
      PageControlLocked.ActivePage := tsGlobalError;
      if bbPassword.CanFocus then begin
        ActiveControl := bbPassword;
      end;
    end else begin
      tsOperation.visible := true;
      tsOperation.tabvisible := {$IFDEF LINUX}true{$ELSE}false{$ENDIF};
      tsGlobalError.TabVisible := false;
      PageControlLocked.ActivePage := tsOperation;
    end;
  End;
  if {(PageControlOpType.ActivePage = tsTransaction)}Notebook.PageIndex = PAGE_SENDTOKENS then begin
    Result := UpdateOpTransaction(GetDefaultSenderAccount,dest_account,amount,errors);
  end else if {(PageControlOpType.ActivePage = tsChangePrivateKey)}Notebook.PageIndex = PAGE_CHANGEKEY then begin
    Result := UpdateOpChangeKey(GetDefaultSenderAccount,signer_account,publicKey,errors);
  end else if {(PageControlOpType.ActivePage = tsListForSale)}Notebook.PageIndex = PAGE_LISTACCOUNT then begin
    Result := UpdateOpListForSale(GetDefaultSenderAccount,salePrice,seller_account,signer_account,publicKey,auxC,errors);
  end else if {(PageControlOpType.ActivePage = tsDelist)}Notebook.PageIndex = PAGE_DELISTACCOUNT then begin
    Result := UpdateOpDelist(GetDefaultSenderAccount,signer_account,errors);
  end else if {(PageControlOpType.ActivePage = tsBuyAccount)}Notebook.PageIndex = PAGE_BUYACCOUNT then begin
    Result := UpdateOpBuyAccount(GetDefaultSenderAccount,account_to_buy,amount,publicKey,errors);
  end else if {(PageControlOpType.ActivePage = tsChangeInfo)}Notebook.PageIndex = PAGE_ACCOUNTNAME then begin
    Result := UpdateOpChangeInfo(GetDefaultSenderAccount,signer_account,changeName,newName,changeType,newType,errors);
  end else if Notebook.PageIndex = PAGE_SAVEFILES then begin
    Result := UpdateOpSaveFiles(GetDefaultSenderAccount,errors);
  end else
  begin
    errors := 'Must select an operation';
  end;
  if {(PageControlOpType.ActivePage=tsTransaction)}Notebook.PageIndex = PAGE_SENDTOKENS then begin
    rbEncryptedWithOldEC.Caption := 'Encrypted with sender public key';
    rbEncryptedWithEC.Caption := 'Encrypted with destination public key';
  end else if {(PageControlOpType.ActivePage=tsChangePrivateKey)}Notebook.PageIndex = PAGE_CHANGEKEY then begin
    rbEncryptedWithOldEC.Caption := 'Encrypted with old public key';
    rbEncryptedWithEC.Caption := 'Encrypted with new public key';
  end else if ({(PageControlOpType.ActivePage=tsListForSale) Or (PageControlOpType.ActivePage=tsDelist)}(Notebook.PageIndex = PAGE_LISTACCOUNT) or
  (Notebook.PageIndex = PAGE_DELISTACCOUNT)) then begin
    rbEncryptedWithOldEC.Caption := 'Encrypted with target public key';
    rbEncryptedWithEC.Caption := 'Encrypted with signer public key';
  end else if {(PageControlOpType.ActivePage=tsBuyAccount)}Notebook.PageIndex = PAGE_BUYACCOUNT then begin
    rbEncryptedWithOldEC.Caption := 'Encrypted with buyer public key';
    rbEncryptedWithEC.Caption := 'Encrypted with target public key';
  end;
  ebSignerAccount.Enabled:= ({(PageControlOpType.ActivePage=tsChangePrivateKey)}(Notebook.PageIndex = PAGE_CHANGEKEY) And (FNode.Bank.Vault.CurrentProtocol>=CT_PROTOCOL_2))
    Or ({(PageControlOpType.ActivePage=tsChangeInfo)}(Notebook.PageIndex = PAGE_ACCOUNTNAME) And (SenderAccounts.Count=1))
    Or ({PageControlOpType.ActivePage=tsListForSale}Notebook.PageIndex = PAGE_LISTACCOUNT)
    Or ({PageControlOpType.ActivePage=tsDelist}Notebook.PageIndex = PAGE_DELISTACCOUNT);
  sbSearchSignerAccount.Enabled:=ebSignerAccount.Enabled;
  lblSignerAccount.Enabled := ebSignerAccount.Enabled;
  lblChangeName.Enabled:= {(PageControlOpType.ActivePage=tsChangeInfo)}(Notebook.PageIndex = PAGE_ACCOUNTNAME) And (SenderAccounts.Count=1);
  ebChangeName.Enabled:= lblChangeName.Enabled;
  //
  UpdatePayload(sender_account, e);
end;

function TFRMOperation.UpdateOpListForSale(const TargetAccount: TAccount;
  var SalePrice: Int64; var SellerAccount, SignerAccount: TAccount;
  var NewOwnerPublicKey: TAccountKey; var LockedUntilBlock: Cardinal;
  var errors: String): Boolean;
var auxC : Cardinal;
begin
  Result := False;
  SalePrice := 0; SellerAccount := CT_Account_NUL;
  NewOwnerPublicKey := CT_TECDSA_Public_Nul;
  LockedUntilBlock := 0; errors := '';
  if {(PageControlOpType.ActivePage <> tsListForSale)}Notebook.PageIndex <> PAGE_LISTACCOUNT then exit;
  lblListAccountErrors.Caption := '';
  Try
    if (rbListAccountForPublicSale.Checked) Or (rbListAccountForPrivateSale.Checked) then begin
      if rbListAccountForPublicSale.Checked then begin
        lblSaleNewOwnerPublicKey.Enabled := false;
        ebSaleNewOwnerPublicKey.Enabled := false;
        ebSaleLockedUntilBlock.Enabled := false;
        lblSaleLockedUntilBlock.Enabled := false;
      end else if rbListAccountForPrivateSale.Checked then begin
        lblSaleNewOwnerPublicKey.Enabled := true;
        ebSaleNewOwnerPublicKey.Enabled := true;
        ebSaleLockedUntilBlock.Enabled := true;
        lblSaleLockedUntilBlock.Enabled := true;
      end;
      if not TAccountComp.TxtToMoney(ebSalePrice.Text,salePrice) then begin
        errors := 'Invalid price';
        exit;
      end;
      // Signer:
      If Not TAccountComp.AccountTxtNumberToAccountNumber(ebSignerAccount.Text,auxC) then begin
        errors := 'Invalid signer account';
        exit;
      end;
      if (auxC<0) Or (auxC >= FNode.Bank.AccountsCount) then begin
        errors := 'Signer account does not exists '+TAccountComp.AccountNumberToAccountTxtNumber(auxC);
        exit;
      end;
      SignerAccount := FNode.GetMempoolAccount(auxC);
      // Seller
      If Not TAccountComp.AccountTxtNumberToAccountNumber(ebSellerAccount.Text,auxC) then begin
        errors := 'Invalid seller account';
        exit;
      end;
      if (auxC<0) Or (auxC >= FNode.Bank.AccountsCount) then begin
        errors := 'Seller account does not exists '+TAccountComp.AccountNumberToAccountTxtNumber(auxC);
        exit;
      end;
      if (auxC=TargetAccount.account) then begin
        errors := 'Seller account cannot be same account';
        exit;
      end;

      SellerAccount := FNode.GetMempoolAccount(auxC);
      if rbListAccountForPrivateSale.Checked then begin
        lblSaleNewOwnerPublicKey.Enabled := true;
        ebSaleNewOwnerPublicKey.Enabled := true;
        ebSaleLockedUntilBlock.Enabled := true;
        lblSaleLockedUntilBlock.Enabled := true;
        If Not TAccountComp.AccountKeyFromImport(ebSaleNewOwnerPublicKey.Text,NewOwnerPublicKey,errors) then begin
          errors := 'Public key: '+errors;
          exit;
        end else begin
          lblListAccountErrors.Font.Color := clGreen;
          lblListAccountErrors.Caption := 'New key type: '+TAccountComp.GetECInfoTxt(NewOwnerPublicKey.EC_OpenSSL_NID);
        end;
        if TAccountComp.EqualAccountKeys(NewOwnerPublicKey,TargetAccount.accountInfo.accountKey) then begin
          errors := 'New public key for private sale is the same public key';
          Exit;
        end;
        LockedUntilBlock := StrToIntDef(ebSaleLockedUntilBlock.Text,0);
        if LockedUntilBlock=0 then begin
          errors := 'Insert locking block';
          exit;
        end;
      end;
      If (FNode.Bank.Vault.CurrentProtocol=CT_PROTOCOL_1) then begin
        errors := 'This operation needs PROTOCOL 2 active';
        exit;
      end;
    end else begin
      lblSaleNewOwnerPublicKey.Enabled := false;
      ebSaleNewOwnerPublicKey.Enabled := false;
      ebSaleLockedUntilBlock.Enabled := false;
      lblSaleLockedUntilBlock.Enabled := false;
      errors := 'Select a sale type';
      exit;
    end;
  Finally
    Result := errors='';
    if errors<>'' then begin
      lblListAccountErrors.Caption := errors;
      lblListAccountErrors.Font.Color := clRed;
    end;
  End;
end;

Function TFRMOperation.UpdateOpSaveFiles(const SenderAccount:TAccount;var errors: String):Boolean;
Var c : Cardinal;
    Account: TAccount;
begin
  Result := False;
  errors := '';

  lblSaveFilesErrors.Caption := '';
  if {PageControlOpType.ActivePage<>tsTransaction}Notebook.PageIndex <> PAGE_SAVEFILES then exit;
  if not (TAccountComp.AccountTxtNumberToAccountNumber(IntToStr(SenderAccount.account),c)) then begin
      errors := 'Invalid account! ('+ebDestAccount.Text+')';
      lblSaveFilesErrors.Caption := errors;
      exit;
  end;
  if (c<0) Or (c>=TNode.Node.Bank.AccountsCount) then begin
      errors := 'Invalid account ('+TAccountComp.AccountNumberToAccountTxtNumber(c)+')';
      lblSaveFilesErrors.Caption := errors;
      exit;
  end;
  Account := TNode.Node.GetMempoolAccount(c);

  if (Account.balance<(FDefaultFee)) then begin
       errors := 'Insufficient balance!';
       lblSaveFilesErrors.Caption := errors;
       exit;
  end;

  Result := True;
end;

function TFRMOperation.UpdateOpTransaction(const SenderAccount: TAccount;  var DestAccount: TAccount; var amount: Int64;  var errors: String): Boolean;
Var c : Cardinal;
begin
  Result := False;
  errors := '';

  lblTransactionErrors.Caption := '';
  if {PageControlOpType.ActivePage<>tsTransaction}Notebook.PageIndex <> PAGE_SENDTOKENS then exit;
  if not (TAccountComp.AccountTxtNumberToAccountNumber(ebDestAccount.Text,c)) then begin
    errors := 'Invalid recipient account! ('+ebDestAccount.Text+')';
    lblTransactionErrors.Caption := errors;
    exit;
  end;
  if (c<0) Or (c>=TNode.Node.Bank.AccountsCount) then begin
    errors := 'Invalid recipient account ('+TAccountComp.AccountNumberToAccountTxtNumber(c)+')';
    lblTransactionErrors.Caption := errors;
    exit;
  end;
  DestAccount := TNode.Node.GetMempoolAccount(c);
  if SenderAccounts.Count=1 then begin
    if not TAccountComp.TxtToMoney(ebAmount.Text,amount) then begin
      errors := 'Invalid amount! ('+ebAmount.Text+')';
      lblTransactionErrors.Caption := errors;
      exit;
    end;
  end else amount := 0; // ALL BALANCE
  if DestAccount.account=SenderAccount.account then begin
    errors := 'Sender and recipient accounts are the same!';
    lblTransactionErrors.Caption := errors;
    exit;
  end;
  if (SenderAccounts.Count=1) then begin
    if (SenderAccount.balance<(amount+FDefaultFee)) then begin
       errors := 'Insufficient balance!';
       lblTransactionErrors.Caption := errors;
       exit;
    end;
  end;
  Result := True;
end;

function TFRMOperation.UpdatePayload(const SenderAccount: TAccount;
  var errors: String): Boolean;
Var payload_u : AnsiString;
  payload_encrypted : TRawBytes;
  account : TAccount;
  public_key : TAccountKey;
  dest_account_number : Cardinal;
  i : Integer;
  valid : Boolean;
  wk : TWalletKey;
begin
  valid := false;
  payload_encrypted := Nil;
  FEncodedPayload := Nil;
  errors := 'Unknown error';
  payload_u := memoPayload.Lines.Text;
  try
    if (payload_u='') then begin
      valid := true;
      exit;
    end;
    if (rbEncryptedWithOldEC.Checked) then begin
      // Use sender
      errors := 'Error encrypting';
      account := FNode.GetMempoolAccount(SenderAccount.account);
      TABEYEncryption.DoABEYECIESEncrypt(account.accountInfo.accountKey,TEncoding.ANSI.GetBytes(payload_u),payload_encrypted);
      valid := Length(payload_encrypted)>0;
    end else if (rbEncryptedWithEC.Checked) then begin
      errors := 'Error encrypting';
      if ({PageControlOpType.ActivePage=tsTransaction}Notebook.PageIndex = PAGE_SENDTOKENS) or ({PageControlOpType.ActivePage=tsListForSale}Notebook.PageIndex = PAGE_LISTACCOUNT) or ({PageControlOpType.ActivePage=tsDelist}Notebook.PageIndex = PAGE_DELISTACCOUNT)
        or ({PageControlOpType.ActivePage=tsBuyAccount}Notebook.PageIndex = PAGE_BUYACCOUNT) then begin
        // With dest public key
        If ({PageControlOpType.ActivePage=tsTransaction}Notebook.PageIndex = PAGE_SENDTOKENS) then begin
          If Not TAccountComp.AccountTxtNumberToAccountNumber(ebDestAccount.Text,dest_account_number) then begin
            errors := 'Invalid dest account number';
            exit;
          end;
        end else if ({PageControlOpType.ActivePage=tsListForSale}Notebook.PageIndex = PAGE_LISTACCOUNT) then begin
          If Not TAccountComp.AccountTxtNumberToAccountNumber(ebSignerAccount.Text,dest_account_number) then begin
            errors := 'Invalid signer account number';
            exit;
          end;
        end else if ({PageControlOpType.ActivePage=tsDelist}Notebook.PageIndex = PAGE_DELISTACCOUNT) then begin
          If Not TAccountComp.AccountTxtNumberToAccountNumber(ebSignerAccount.Text,dest_account_number) then begin
            errors := 'Invalid signer account number';
            exit;
          end;
        end else if ({PageControlOpType.ActivePage=tsBuyAccount}Notebook.PageIndex = PAGE_BUYACCOUNT) then begin
          If Not TAccountComp.AccountTxtNumberToAccountNumber(ebAccountToBuy.Text,dest_account_number) then begin
            errors := 'Invalid account to buy number';
            exit;
          end;
        end else begin
          errors := 'ERROR DEV 20170512-1';
          exit;
        end;
        if (dest_account_number<0) or (dest_account_number>=FNode.Bank.AccountsCount) then begin
          errors := 'Invalid payload encrypted account number: '+TAccountComp.AccountNumberToAccountTxtNumber(dest_account_number);
          exit;
        end;
        account := FNode.GetMempoolAccount(dest_account_number);
        TABEYEncryption.DoABEYECIESEncrypt(account.accountInfo.accountKey,TEncoding.ANSI.GetBytes(payload_u),payload_encrypted);
        valid := Length(payload_encrypted)>0;
      end else if ({PageControlOpType.ActivePage=tsChangePrivateKey}Notebook.PageIndex = PAGE_CHANGEKEY) then begin
        if (rbChangeKeyWithAnother.Checked) then begin
          // With new key generated
          if (cbNewPrivateKey.ItemIndex>=0) then begin
            i := PtrInt(cbNewPrivateKey.Items.Objects[cbNewPrivateKey.ItemIndex]);
            if (i>=0) then public_key := WalletKeys.Key[i].AccountKey;
          end else begin
            errors := 'Must select a private key';
            exit;
          end;
        end else if (rbChangeKeyTransferAccountToNewOwner.Checked) then begin
          If Not TAccountComp.AccountKeyFromImport(ebNewPublicKey.Text,public_key,errors) then begin
            errors := 'Public key: '+errors;
            exit;
          end;
        end else begin
          errors := 'Must select change type';
          exit;
        end;
        if public_key.EC_OpenSSL_NID<>CT_Account_NUL.accountInfo.accountKey.EC_OpenSSL_NID then begin
          TABEYEncryption.DoABEYECIESEncrypt(public_key,TEncoding.ANSI.GetBytes(payload_u),payload_encrypted);
          valid := Length(payload_encrypted)>0;
        end else begin
          valid := false;
          errors := 'Selected private key is not valid to encode';
          exit;
        end;
      end else begin
        errors := 'This operation does not allow this kind of payload';
      end;
    end else if (rbEncrptedWithPassword.Checked) then begin
      payload_encrypted := TABEYEncryption.DoABEYAESEncrypt(TEncoding.ANSI.GetBytes(payload_u),TEncoding.ANSI.GetBytes(ebEncryptPassword.Text));
      valid := Length(payload_encrypted)>0;
    end else if (rbNotEncrypted.Checked) then begin
      payload_encrypted := TEncoding.ANSI.GetBytes(payload_u);
      valid := true;
    end else begin
      errors := 'Must select an encryption option for payload';
    end;
  finally
    if valid then begin
      if length(payload_encrypted)>CT_MaxPayloadSize then begin
        valid := false;
        errors := 'Payload size is bigger than '+inttostr(CT_MaxPayloadSize)+' ('+Inttostr(length(payload_encrypted))+')';
      end;
    end;
    if valid then begin
      lblEncryptionErrors.Caption := '';
      lblPayloadLength.Caption := Format('(%db -> %db)',[length(payload_u),length(payload_encrypted)]);
    end else begin
      lblEncryptionErrors.Caption := errors;
      lblPayloadLength.Caption := Format('(%db -> ?)',[length(payload_u)]);
    end;
    FEncodedPayload := payload_encrypted;
    Result := valid;
  end;
end;


function TFRMOperation.UpdatePayloadSaveFiles(const SenderAccount: TAccount):boolean;

Var payload_u : AnsiString;
  payload_encrypted : TRawBytes;
  account : TAccount;
  public_key : TAccountKey;
  dest_account_number : Cardinal;
  i : Integer;
  valid : Boolean;
  wk : TWalletKey;
  Tempbytes:array of byte;
  Index:Integer;
  MessageStr:String;
  feeEstimate:Double;
const CT_FILE_UNIT = 10000;
      CT_UNIT_PRICE = 0.01 ;
      CT_MAX_FILES  = (1 shl 10) - 1;
begin
  valid := false;
  payload_encrypted := Nil;
  FEncodedPayload := Nil;

      TreeFunc.QuickSort(Helper.NodeArray,Low(Helper.NodeArray),High(Helper.NodeArray));    //sort array of nodes

      Helper.FileTree := TreeFunc.CreateTree();   // create Nil root

      TreeFunc.SortedArrayToBST(Helper.FileTree,Helper.NodeArray,Low(Helper.NodeArray),High(Helper.NodeArray));   // construct BST and store root in FileTree

      TreeFunc.Flatten(Helper.FileTree,Helper.FlatNodeArray);         // convert tree to preorder array

      Helper.PFlatNodeArray := @Helper.FlatNodeArray;                 // get a pointer to preorder array

      TreeFunc.Serialize(Helper.PFlatNodeArray,Helper.ByteNodeArr);   //serialize

      feeEstimate := (Length(Helper.ByteNodeArr)/CT_FILE_UNIT )* CT_UNIT_PRICE;

      if feeEstimate > StrToFloat(ebFee.Text) then begin
        raise Exception.Create('Minimum fee required ( '+ FloatToStr(feeEstimate) +' ) not paid');

        //SetLength(Helper.NodeArray,0);             // free all memory used by arrays in Helper
        SetLength(Helper.FlatNodeArray,0);
        SetLength(Helper.ByteNodeArr,0);

        Helper.FileCount := 0;
        Helper.FileNames.Destroy;
        Helper.FileNames := TStringHashMap.Create(CT_MAX_FILES);

        UploadEdit.Text := '' ;

        Exit;
      end;


      FEncodedPayload := Helper.ByteNodeArr;             // put serialzied info in the payload of the operation


      SetLength(Helper.NodeArray,0);             // free all memory used by arrays in Helper
      SetLength(Helper.FlatNodeArray,0);
      SetLength(Helper.ByteNodeArr,0);

      Helper.FileCount := 0;
      Helper.FileNames.Destroy;
      Helper.FileNames := TStringHashMap.Create(CT_MAX_FILES);

      UploadEdit.Text := '' ;


  Result := valid;

end;

procedure TFRMOperation.UpdateWalletKeys;
Var i : Integer;
  wk : TWalletKey;
  s : String;
begin
  cbNewPrivateKey.items.BeginUpdate;
  cbBuyNewKey.Items.BeginUpdate;
  Try
    cbNewPrivateKey.Items.Clear;
    cbBuyNewKey.Items.Clear;
    if Not Assigned(FWalletKeys) then exit;
    For i:=0 to FWalletKeys.Count-1 do begin
      wk := FWalletKeys.Key[i];
      if (wk.Name='') then begin
        s := TCrypto.ToHexaString( TAccountComp.AccountKey2RawString(wk.AccountKey));
      end else begin
        s := wk.Name;
      end;
      if Not Assigned(wk.PrivateKey) then s := s + '(*)';
      cbNewPrivateKey.Items.AddObject(s,TObject(i));
      cbBuyNewKey.Items.AddObject(s,TObject(i));
    end;
    cbNewPrivateKey.Sorted := true;
    cbBuyNewKey.Sorted := true;
  Finally
    cbNewPrivateKey.Items.EndUpdate;
    cbBuyNewKey.Items.EndUpdate;
  End;
  updateInfoClick(Nil);
  memoPayloadClick(Nil);
end;

end.
