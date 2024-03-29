unit UFRMWallet;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

{$I ../config.inc}

uses
{$IFDEF UNIX}
BaseUnix,
{$ENDIF UNIX}
{$IFDEF Windows}
Windows,
{$ENDIF Windows}

{$IFnDEF FPC}
  pngimage, Windows, AppEvnts, ShlObj,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, UWallet, StdCtrls, ULog, Grids, UAppParams, UBlockChain,
  UNode, UGridUtils, UJSONFunctions, UAccounts, Menus, ImgList, UNetProtocol,
  UCrypto, Buttons, UPoolMining, URPC, UFRMAccountSelect, UConst,
  UAccountKeyStorage, UBaseTypes, UPCDataTypes,
  UFRMRPCCalls, UTxMultiOperation,
  {$IFNDEF FPC}System.Generics.Collections{$ELSE}Generics.Collections{$ENDIF},

  frmStatistics, frmNotifications, UFRMWalletKeys, UFRMABEYWalletConfig,
  UFRMoperation, UUpdateVersionThread, Process, AnimatedGIF, MemBitmap,IniFiles;
  //UniqueInstance;

Const
  CM_PC_WalletKeysChanged = WM_USER + 1;
  CM_PC_NetConnectionUpdated = WM_USER + 2;
  MAX_REFRESH_POINTS = 50;
  MAX_REFRESH_POINTS_BANDWIDTH = 50;
  REFRESH_CHARTS = True;

  BTN_PRESSED_COLOR =  $00D9E6FF; //$0000A8FF;//
  BTN_HIGHLIGHT_COLOR = $00F9FBFF;

  PAGE_BLOCKCHAIN_EXPLORER = 0;
  PAGE_KEYS = 1;
  PAGE_OPERATIONS = 2;
  PAGE_CONFIG = 3;


type
  TMinerPrivateKey = (mpk_NewEachTime, mpk_Random, mpk_Selected);

  { TFRMWallet }

  TFRMWallet = class(TForm)
    bbChangeKeyName: TBitBtn;
    BlockchainExplorer: TPage;
    connectConfig: TShape;
    connectExplorer: TShape;
    btnOperations: TShape;
    btnExplorer: TShape;
    btnOperations1: TShape;
    cbExploreMyAccounts: TCheckBox;
    cbFilterAccounts: TCheckBox;
    cbHashRateUnits: TComboBox;
    cbMyPrivateKeys: TComboBox;
    Configuration: TPage;
    connectExplorer1: TShape;
    connectKeys: TShape;
    connectKeys1: TShape;
    connectKeys2: TShape;
    connectKeys3: TShape;
    connectOperations: TShape;
    dgAccountOperations: TDrawGrid;
    dgAccounts: TDrawGrid;
    dgPendingOperations: TDrawGrid;
    ebFilterAccountByBalanceMax: TEdit;
    ebFilterAccountByBalanceMin: TEdit;
    ebFindAccountNumber: TEdit;
    ebHashRateBackBlocks: TEdit;
    ConfigFrame: TFrame;
    backImg: TImage;
    imgContract: TImage;
    imgChain: TImage;
    Label4: TLabel;
    OperationsFrame: TFrame;
    KeysFrame: TFrame;
    Image8: TImage;
    imgConfig: TImage;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Image6: TImage;
    Image7: TImage;
    imgMerchant: TImage;
    imgMiner: TImage;
    imgMiningPool: TImage;
    imgAcademia: TImage;
    imgRefreshAccounts: TImage;
    imgUserUnknown: TImage;
    imgUser: TImage;
    imgExplorer: TImage;
    imgKeys: TImage;
    imgOperations: TImage;
    imgNonProfit: TImage;
    Keys: TPage;
    lblVersion: TLabel;
    Label16: TLabel;
    Label18: TLabel;
    Label21: TLabel;
    Label23: TLabel;
    lblAccountsBalance1: TLabel;
    lblAccountsCount: TLabel;
    lbl_btnKeys: TLabel;
    lblHashRateBackBlocks: TLabel;
    lblHashRateBackBlocks1: TLabel;
    lbl_btnOperations: TLabel;
    lbl_btnExplorer: TLabel;
    lbl_btnConfig: TLabel;
    MiRPCCalls: TMenuItem;
    Notebook: TNotebook;
    Operations: TPage;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    panelTransactions: TPanel;
    panelQueue: TPanel;
    sbSearchAccount: TSpeedButton;
    Shape1: TShape;
    Shape10: TShape;
    Shape11: TShape;
    Shape12: TShape;
    Shape13: TShape;
    Shape16: TShape;
    Shape17: TShape;
    Shape18: TShape;
    Shape19: TShape;
    Shape2: TShape;
    Shape20: TShape;
    Shape21: TShape;
    Shape22: TShape;
    Shape23: TShape;
    Shape24: TShape;
    shapeTX: TShape;
    shapeQ: TShape;
    Shape3: TShape;
    Shape4: TShape;
    PageControl: TPageControl;
    btnKeys: TShape;
    Shape5: TShape;
    Shape6: TShape;
    Shape7: TShape;
    Shape8: TShape;
    Shape9: TShape;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    IdleTimer: TTimer;
    UpdateTimer: TTimer;
    tsMyAccounts: TTabSheet;
    tsOperations: TTabSheet;
    TimerUpdateStatus: TTimer;
    tsLogs: TTabSheet;
    pnlTopLogs: TPanel;
    cbShowDebugLogs: TCheckBox;
    memoLogs: TMemo;
    pnlMyAccountsTop: TPanel;
    MainMenu: TMainMenu;
    miProject: TMenuItem;
    miOptions: TMenuItem;
    miPrivatekeys: TMenuItem;
    miN1: TMenuItem;
    miNewOperation: TMenuItem;
    Panel1: TPanel;
    Label2: TLabel;
    ebFilterOperationsStartBlock: TEdit;
    ebFilterOperationsEndBlock: TEdit;
    tsNodeStats: TTabSheet;
    memoNetConnections: TMemo;
    memoNetServers: TMemo;
    memoNetBlackLists: TMemo;
    Label3: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    tsBlockChain: TTabSheet;
    Panel2: TPanel;
    Label9: TLabel;
    ebBlockChainBlockStart: TEdit;
    ebBlockChainBlockEnd: TEdit;
    tsPendingOperations: TTabSheet;
    pnlPendingOperations: TPanel;
    Label10: TLabel;
    N1: TMenuItem;
    MiClose: TMenuItem;
    MiDecodePayload: TMenuItem;
    tsMessages: TTabSheet;
    lbNetConnections: TListBox;
    bbSendAMessage: TButton;
    Label11: TLabel;
    memoMessages: TMemo;
    memoMessageToSend: TMemo;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    IPnodes1: TMenuItem;
    pcAccountsOptions: TPageControl;
    tsAccountOperations: TTabSheet;
    tsMultiSelectAccounts: TTabSheet;
    dgSelectedAccounts: TDrawGrid;
    pnlSelectedAccountsTop: TPanel;
    pnlSelectedAccountsBottom: TPanel;
    pnlSelectedAccountsLeft: TPanel;
    sbSelectedAccountsAdd: TSpeedButton;
    sbSelectedAccountsAddAll: TSpeedButton;
    sbSelectedAccountsDel: TSpeedButton;
    sbSelectedAccountsDelAll: TSpeedButton;
    Label20: TLabel;
    lblSelectedAccountsCount: TLabel;
    Label22: TLabel;
    lblSelectedAccountsBalance: TLabel;
    bbSelectedAccountsOperation: TBitBtn;
    Label15: TLabel;
    MiOperations: TMenuItem;
    MiAddaccounttoSelected: TMenuItem;
    MiRemoveaccountfromselected: TMenuItem;
    N2: TMenuItem;
    MiMultiaccountoperation: TMenuItem;
    N3: TMenuItem;
    MiFindnextaccountwithhighbalance: TMenuItem;
    MiFindpreviousaccountwithhighbalance: TMenuItem;
    MiFindaccount: TMenuItem;
    dgBlockChainExplorer: TDrawGrid;
    dgOperationsExplorer: TDrawGrid;
    MiFindOperationbyOpHash: TMenuItem;
    MiAccountInformation: TMenuItem;
    MiOperationsExplorer: TMenuItem;

    UpdateVersionThread : TUpdateThread;
    procedure backImgClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IdleTimerTimer(Sender: TObject);
    procedure imgAcademiaClick(Sender: TObject);
    procedure UpdateDone(Update:String); // update mechanism

    procedure cbHashRateUnitsClick(Sender: TObject);
    procedure dgAccountsHeaderSized(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    procedure ebHashRateBackBlocksExit(Sender: TObject);
    procedure ebHashRateBackBlocksKeyPress(Sender: TObject; var Key: char);
    procedure FormChangeBounds(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure imgConfigClick(Sender: TObject);
    procedure imgConfigMouseEnter(Sender: TObject);
    procedure imgConfigMouseLeave(Sender: TObject);
    procedure imgExplorerClick(Sender: TObject);
    procedure imgExplorerMouseEnter(Sender: TObject);
    procedure imgExplorerMouseLeave(Sender: TObject);
    procedure imgKeysClick(Sender: TObject);
    procedure imgKeysMouseEnter(Sender: TObject);
    procedure imgKeysMouseLeave(Sender: TObject);
    procedure imgOperationsClick(Sender: TObject);
    procedure imgOperationsMouseEnter(Sender: TObject);
    procedure imgOperationsMouseLeave(Sender: TObject);
    procedure KeysBeforeShow(ASender: TObject; ANewPage: TPage;
      ANewIndex: Integer);
    procedure Label21Click(Sender: TObject);
    procedure lblAccountsBalance1Click(Sender: TObject);
    procedure MiOperationsExplorerClick(Sender: TObject);
    procedure MiRPCCallsClick(Sender: TObject);
    procedure sbSearchAccountClick(Sender: TObject);
    procedure Shape2ChangeBounds(Sender: TObject);
    procedure Splitter2ChangeBounds(Sender: TObject);
    procedure Splitter2Moved(Sender: TObject);
    procedure Splitter3ChangeBounds(Sender: TObject);
    procedure Splitter3Moved(Sender: TObject);
    procedure TimerUpdateStatusTimer(Sender: TObject);
    procedure cbMyPrivateKeysChange(Sender: TObject);
    procedure dgAccountsClick(Sender: TObject);
    procedure miOptionsClick(Sender: TObject);
    procedure miNewOperationClick(Sender: TObject);
    procedure miPrivatekeysClick(Sender: TObject);
    procedure dgAccountsColumnMoved(Sender: TObject; FromIndex,
      ToIndex: Integer);
    procedure dgAccountsFixedCellClick(Sender: TObject; ACol, ARow: Integer);
    procedure PageControlChange(Sender: TObject);
    procedure ebFilterOperationsAccountExit(Sender: TObject);
    procedure ebFilterOperationsAccountKeyPress(Sender: TObject; var Key: Char);
    procedure ebBlockChainBlockStartExit(Sender: TObject);
    procedure ebBlockChainBlockStartKeyPress(Sender: TObject; var Key: Char);
    procedure cbExploreMyAccountsClick(Sender: TObject);
    procedure MiCloseClick(Sender: TObject);
    procedure MiDecodePayloadClick(Sender: TObject);
    procedure bbSendAMessageClick(Sender: TObject);
    procedure lblReceivedMessagesClick(Sender: TObject);
    procedure ebFindAccountNumberChange(Sender: TObject);
    procedure ebFindAccountNumberExit(Sender: TObject);
    procedure IPnodes1Click(Sender: TObject);
    procedure bbChangeKeyNameClick(Sender: TObject);
    procedure sbSelectedAccountsAddClick(Sender: TObject);
    procedure sbSelectedAccountsAddAllClick(Sender: TObject);
    procedure sbSelectedAccountsDelClick(Sender: TObject);
    procedure sbSelectedAccountsDelAllClick(Sender: TObject);
    procedure bbSelectedAccountsOperationClick(Sender: TObject);
    procedure MiAddaccounttoSelectedClick(Sender: TObject);
    procedure MiRemoveaccountfromselectedClick(Sender: TObject);
    procedure MiMultiaccountoperationClick(Sender: TObject);
    procedure MiFindnextaccountwithhighbalanceClick(Sender: TObject);
    procedure MiFindpreviousaccountwithhighbalanceClick(Sender: TObject);
    procedure MiFindaccountClick(Sender: TObject);
    procedure bbAccountsRefreshClick(Sender: TObject);
    procedure ebFilterAccountByBalanceMinExit(Sender: TObject);
    procedure ebFilterAccountByBalanceMinKeyPress(Sender: TObject;
      var Key: Char);
    procedure cbFilterAccountsClick(Sender: TObject);
    procedure MiFindOperationbyOpHashClick(Sender: TObject);
    procedure MiAccountInformationClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure UpdateTimerTimer(Sender: TObject);
  private
    FLastNodesCacheUpdatedTS : TDateTime;
    FBackgroundPanel : TPanel;
    FBackgroundLabel : TLabel;
    FMinersBlocksFound: Integer;
    image: TAnimatedGif;
    r: TRect;
    {$IFDEF TESTNET}
    FLastAskForAccountTC : TTickCount;
    {$ENDIF}
    procedure SetMinersBlocksFound(const Value: Integer);
    Procedure CheckIsReady;
    Procedure FinishedLoadingApp;
    Procedure FillAccountInformation(Const Strings : TStrings; Const AccountNumber : Cardinal);
    Procedure FillOperationInformation(Const Strings : TStrings; Const OperationResume : TOperationResume);
    procedure BackgroundPanelOnPaint(Sender: TObject);
    {$IFDEF TESTNET}
    Procedure InitMenuForTesting;
    Procedure Test_RandomOperations(Sender: TObject);
    Procedure Test_AskForFreeAccount(Sender: TObject);
    {$IFDEF TESTING_NO_POW_CHECK}
    Procedure Test_CreateABlock(Sender: TObject);
    {$ENDIF}
    {$ENDIF}
    Procedure Test_ShowPublicKeys(Sender: TObject);
    procedure OnAccountsGridUpdatedData(Sender : TObject);

    // GUI
    procedure AllButtonsUp;
  protected
    { Private declarations }
    FNode : TNode;
    FIsActivated : Boolean;
    FWalletKeys : TWalletKeysExt;
    FLog : TLog;
    FAppParams : TAppParams;
    FNodeNotifyEvents : TNodeNotifyEvents;
    FAccountsGrid : TAccountsGrid;
    FSelectedAccountsGrid : TAccountsGrid;
    FOperationsAccountGrid : TOperationsGrid;
    FPendingOperationsGrid : TOperationsGrid;
    FOperationsExplorerGrid : TOperationsGrid;
    FBlockChainGrid : TBlockChainGrid;
    FMinerPrivateKeyType : TMinerPrivateKey;
    FUpdating : Boolean;
    FMessagesUnreadCount : Integer;
    FMinAccountBalance : Int64;
    FMaxAccountBalance : Int64;
    FPoolMiningServer : TPoolMiningServer;
    FRPCServer : TRPCServer;
    FMustProcessWalletChanged : Boolean;
    FMustProcessNetConnectionUpdated : Boolean;
    FThreadActivate : TObject;
    FLastAccountsGridInvalidateTC : TTickCount;
    Procedure OnNewAccount(Sender : TObject);
    Procedure OnReceivedHelloMessage(Sender : TObject);
    Procedure OnNetStatisticsChanged(Sender : TObject);
    procedure OnNewLog(logtype : TLogType; Time : TDateTime; ThreadID : TThreadID; Const sender, logtext : String);
    procedure OnWalletChanged(Sender : TObject);
    procedure OnNetConnectionsUpdated(Sender : TObject);
    procedure OnNetNodeServersUpdated(Sender : TObject);
    procedure OnNetBlackListUpdated(Sender : TObject);
    Procedure OnNodeMessageEvent(NetConnection : TNetConnection; MessageData : String);
    Procedure OnNodeKeysActivity(Sender : TObject);
    Procedure OnSelectedAccountsGridUpdated(Sender : TObject);
    Procedure OnMiningServerNewBlockFound(Sender : TObject);
    Procedure UpdateConnectionStatus;
    Procedure UpdateAccounts(RefreshData : Boolean);
    Procedure UpdateBlockChainState;
    Procedure UpdatePrivateKeys;
    Procedure UpdateOperations;
    Procedure LoadAppParams;
    Procedure SaveAppParams;
    Procedure UpdateConfigChanged;
    Procedure UpdateNodeStatus;
    Procedure UpdateAvailableConnections;
    procedure Activate; override;
    Function ForceMining : Boolean; virtual;
    Function GetAccountKeyForMiner : TAccountKey;
    Procedure DoUpdateAccounts;
    Function DoUpdateAccountsFilter : Boolean;
    procedure CM_WalletChanged(var Msg: TMessage); message CM_PC_WalletKeysChanged;
    procedure CM_NetConnectionUpdated(var Msg: TMessage); message CM_PC_NetConnectionUpdated;
  public
    { Public declarations }
    frmStatistics: TStatisticsForm;
    frmNotifications: TNotificationsForm;
    frmWalletKeys : TFRMWalletKeys;
    frmWalletConfig: TFRMABEYWalletConfig;
    frmWalletOperations: TFRMOperation;
    Property WalletKeys : TWalletKeysExt read FWalletKeys;
    Property MinersBlocksFound : Integer read FMinersBlocksFound write SetMinersBlocksFound;
    procedure ResizeColumns;
    procedure RealignShapesUI;
  end;

var
  FRMWallet: TFRMWallet;
  lastSent, lastReceived: Real;
  btnOperations_Pressed, btnKeys_Pressed, btnExplorer_Pressed, btnConfig_Pressed: Boolean;
  Timeout: Integer;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

Uses UFolderHelper,
{$IFDEF Use_OpenSSL}
  UOpenSSL,
{$ENDIF}
  UTime, UFileStorage,
  UThread, UOpTransaction,
  UFRMOperationsExplorer,
  {$IFDEF TESTNET}
  UFRMRandomOperations,
  UPCTNetDataExtraMessages,
  {$ENDIF}
  UFRMPayloadDecoder, UFRMNodesIp, UFRMMemoText,
  USettings, UCommon, UPCOrderedLists;

Type

  { TThreadActivate }

  TThreadActivate = Class(TABEYThread)
  private
    FLastTC : TTickCount;
    FLastMsg : String;
    procedure OnProgressNotify(sender : TObject; const mesage : String; curPos, totalCount : Int64);
    procedure ThreadSafeNotify;
  protected
    procedure BCExecute; override;
  End;

{$IFDEF WINDOWS}
function TerminateProcessByID(ProcessID: Integer): Boolean;
var
  hProcess : THandle;
begin
  Result := False;
  hProcess := OpenProcess(PROCESS_TERMINATE,False,ProcessID);
  if hProcess > 0 then
  try
    Result := Win32Check(Windows.TerminateProcess(hProcess,0));
  finally
    CloseHandle(hProcess);
  end;
end;
{$ENDIF WINDOWS}

{ TThreadActivate }

procedure TThreadActivate.OnProgressNotify(sender: TObject; const mesage: String; curPos, totalCount: Int64);
var pct : String;
begin
  If TPlatform.GetElapsedMilliseconds(FLastTC)>500 then begin
    FLastTC := TPlatform.GetTickCount;
    if (curPos > totalCount) then // do not go overboard
      curPos := totalCount;
    if (totalCount>0) then pct := Format('%.2f',[curPos*100/totalCount])+'%'
    else pct := '';
    FLastMsg:='['+pct+'] '+mesage;
    Synchronize(ThreadSafeNotify);
  end;
end;

procedure TThreadActivate.ThreadSafeNotify;
begin
  If (FLastMsg<>'') then begin
    if Assigned(FRMWallet.FBackgroundLabel) then begin
      FRMWallet.FBackgroundLabel.Caption:=FLastMsg;
    end;
  end else FRMWallet.UpdateNodeStatus;
end;

procedure TThreadActivate.BCExecute;
Var currentProcess : String;
  LRaw : TRawBytes;
begin
  FLastTC := 0;
  FLastMsg := '';
  //
{
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
OnProgressNotify(Self,'Reading Hardcoded RandomHash file',0,0);
  LRaw := TCrypto.HexaToRaw(CT_Hardcoded_RandomHash_Table_HASH);
  TABEYProtocol.AllowUseHardcodedRandomHashTable(
    ExtractFileDir(Application.ExeName)+PathDelim+CT_Hardcoded_RandomHash_Table_Filename,
    LRaw );
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
}
  // Read Operations saved from disk
  TNode.Node.InitVaultAndOperations($FFFFFFFF,OnProgressNotify); // New Build 2.1.4 to load pending operations buffer
  TNode.Node.AutoDiscoverNodes(CT_Discover_IPs);
  TNode.Node.NetServer.Active := true;
  FLastTC := 0;
  FLastMsg := '';
  if (TNode.Node.Bank.BlocksCount<=1) then begin
    while (Not Terminated) And (Not TNode.Node.IsReady(currentProcess) Or (TNode.Node.Bank.BlocksCount<=1)) do begin
      Synchronize(ThreadSafeNotify);
      Sleep(200);
    end;
  end;
  if Not Terminated then begin
    Synchronize( FRMWallet.DoUpdateAccounts );
    Synchronize( FRMWallet.FinishedLoadingApp );
  end;
  FRMWallet.FThreadActivate := Nil;
end;

{ TFRMWallet }

procedure TFRMWallet.Activate;
Var ips : AnsiString;
  nsarr : TNodeServerAddressArray;
  i : Integer;
  FIniFile : TIniFile;
const
  CT_INI_SECTION_GLOBAL = 'GLOBAL';
  CT_INI_IDENT_FULLARCHIVEMODEALLOWED = 'FULLARCHIVEMODEALLOWED';
begin
  inherited;
  if FIsActivated then exit;
  FIsActivated := true;
  try
    // Check OpenSSL dll
{$IFDEF Use_OpenSSL}
    if Not LoadSSLCrypt then raise Exception.Create('Cannot load '+SSL_C_LIB+#10+'To use this software make sure this file is available on you system or reinstall the application');
{$ENDIF}
    TCrypto.InitCrypto;
    // Read Wallet
    Try
      FWalletKeys.WalletFileName := TFolderHelper.GetABEYDataFolder+PathDelim+'ABEY.keys';
    Except
      On E:Exception do begin
        E.Message := 'Cannot open your wallet... Perhaps another instance of ABEY is active!'+#10+#10+E.Message;
        Raise;
      end;
    End;
    FIniFile := TIniFile.Create(ExtractFileDir(Application.ExeName)+PathDelim+'ABEY.ini');
    //TNetData.FullArchiveModeAllowed:=FIniFile.ReadBool(CT_INI_SECTION_GLOBAL,CT_INI_IDENT_FULLARCHIVEMODEALLOWED,False);
    FreeAndNil(FIniFile);
    ips := FAppParams.ParamByName[CT_PARAM_TryToConnectOnlyWithThisFixedServers].GetAsString('');
    TNode.DecodeIpStringToNodeServerAddressArray(ips,nsarr);
    TNetData.NetData.DiscoverFixedServersOnly(nsarr);
    setlength(nsarr,0);
    // Creating Node:
    FNode := TNode.Node;
    FNode.NetServer.Port := FAppParams.ParamByName[CT_PARAM_InternetServerPort].GetAsInteger(CT_NetServer_Port);
    FNode.PeerCache := FAppParams.ParamByName[CT_PARAM_PeerCache].GetAsString('')+';'+CT_Discover_IPs;
    // Create RPC server
    FRPCServer := TRPCServer.Create;
    FRPCServer.WalletKeys := WalletKeys;
    FRPCServer.Active := FAppParams.ParamByName[CT_PARAM_JSONRPCEnabled].GetAsBoolean(true);
    FRPCServer.ValidIPs := FAppParams.ParamByName[CT_PARAM_JSONRPCAllowedIPs].GetAsString('127.0.0.1');
    WalletKeys.Vault := FNode.Bank.Vault;
    // Check Database
    FNode.Bank.StorageClass := TFileStorage;
    TFileStorage(FNode.Bank.Storage).DatabaseFolder := TFolderHelper.GetABEYDataFolder+PathDelim+'Data';
    TFileStorage(FNode.Bank.Storage).Initialize;
    // Init Grid
    FSelectedAccountsGrid.Node := FNode;
    FWalletKeys.OnChanged.Add( OnWalletChanged );
    FAccountsGrid.Node := FNode;
    FPendingOperationsGrid.Node := FNode; { 02.05.2020 }
    FOperationsAccountGrid.Node := FNode;
    FBlockChainGrid.HashRateAverageBlocksCount := FAppParams.ParamByName[CT_PARAM_HashRateAvgBlocksCount].GetAsInteger(50);
    i := FAppParams.ParamByName[CT_PARAM_ShowHashRateAs].GetAsInteger(Integer({$IFDEF TESTNET}hr_Mega{$ELSE}hr_Tera{$ENDIF}));
    if (i<Integer(Low(TShowHashRateAs))) Or (i>Integer(High(TShowHashRateAs))) then i := Integer({$IFDEF TESTNET}hr_Mega{$ELSE}hr_Tera{$ENDIF});
    FBlockChainGrid.HashRateAs := TShowHashRateAs(i);
    // Reading database
    FThreadActivate := TThreadActivate.Create(true);
    TThreadActivate(FThreadActivate).FreeOnTerminate := true;
    TThreadActivate(FThreadActivate).Suspended := False;
    UpdateConfigChanged;
    UpdateNodeStatus;
    {$IFDEF TESTNET}
    TABEYTNetDataExtraMessages.InitNetDataExtraMessages(FNode,TNetData.NetData,FWalletKeys);
    {$ENDIF}
  Except
    On E:Exception do begin
      E.Message := 'An error occurred during initialization. Application cannot continue:'+#10+#10+E.Message+#10+#10+'Application will close...';
      Application.MessageBox(PChar(E.Message),PChar(Application.Title),MB_ICONERROR+MB_OK);
      Halt;
    end;
  end;

  UpdatePrivateKeys;
  UpdateAccounts(false);
  if FAppParams.ParamByName[CT_PARAM_FirstTime].GetAsBoolean(true) then begin
    FAppParams.ParamByName[CT_PARAM_FirstTime].SetAsBoolean(false);
  end;

end;

procedure TFRMWallet.bbAccountsRefreshClick(Sender: TObject);
begin
  UpdateAccounts(true);
end;

procedure TFRMWallet.bbChangeKeyNameClick(Sender: TObject);
var i : Integer;
  name : String;
begin
  if (cbMyPrivateKeys.ItemIndex<0) then  exit;
  i := PtrInt(cbMyPrivateKeys.Items.Objects[cbMyPrivateKeys.ItemIndex]);
  if (i<0) Or (i>=FWalletKeys.Count) then raise Exception.Create('Must select a Key');
  name := FWalletKeys.Key[i].Name;
  if InputQuery('Change Key name','Input new name',name) then begin
    FWalletKeys.SetName(i,name);
  end;
  UpdatePrivateKeys;
end;

procedure TFRMWallet.bbSelectedAccountsOperationClick(Sender: TObject);
var l : TOrderedCardinalList;
begin
  CheckIsReady;
  if FSelectedAccountsGrid.AccountsCount<=0 then raise Exception.Create('Must select at least 1 account');
  With TFRMOperation.Create(Self) do
  Try
    l := FSelectedAccountsGrid.LockAccountsList;
    try
      SenderAccounts.CopyFrom(l);
    finally
      FSelectedAccountsGrid.UnlockAccountsList;
    end;
    DefaultFee := FAppParams.ParamByName[CT_PARAM_DefaultFee].GetAsInt64(0);
    WalletKeys := FWalletKeys;
    ShowModal;
  Finally
    Free;
  End;
end;

procedure TFRMWallet.bbSendAMessageClick(Sender: TObject);
Var basem,m : String;
  them, errors : String;
  i,n : Integer;
  nc : TNetConnection;
begin
  CheckIsReady;
  if (lbNetConnections.SelCount<=0) Or (lbNetConnections.ItemIndex<0) then raise Exception.Create('Select at least one connection');
  if lbNetConnections.SelCount<=0 then n := 1
  else n := lbNetConnections.SelCount;

  basem := memoMessageToSend.Lines.Text;
  m := '';
  // Clear non valid characters:
  for i := Low(basem) to High(basem) do begin
    if basem[i] in [#32..#127] then m := m + basem[i]
    else m:=m+'.';
  end;

  if trim(m)='' then raise Exception.Create('No message');

  if Application.MessageBox(PChaR('Send this message to '+inttostr(n)+' nodes?'+#10+
    'NOTE: Sending unauthorized messages will be considered spam and you will be banned'+#10+
    #10+
    'Message: '+#10+
    m),PChar(Application.Title),MB_ICONQUESTION+MB_YESNO+MB_DEFBUTTON1)<>IdYes then exit;
  them := m;
  if n>1 then begin
    for i := 0 to lbNetConnections.Items.Count - 1 do begin
      if lbNetConnections.Selected[i] then begin
        nc := TNetConnection(lbNetconnections.Items.Objects[i]);
        if TNetData.NetData.ConnectionExistsAndActive(nc) then begin
          FNode.SendNodeMessage(nc,m,errors);
          memoMessages.Lines.Add(DateTimeToStr(now)+' Sent to '+nc.ClientRemoteAddr+' > '+m);
        end;
      end;
    end;
  end else begin
    nc := TNetConnection(lbNetconnections.Items.Objects[lbNetconnections.ItemIndex]);
    if TNetData.NetData.ConnectionExistsAndActive(nc) then begin
      FNode.SendNodeMessage(nc,m,errors);
      memoMessages.Lines.Add(DateTimeToStr(now)+' Sent to '+nc.ClientRemoteAddr+' > '+m);
    end;
  end;

  Application.MessageBox(PChaR('Message sent to '+inttostr(n)+' nodes'+#10+
    'Message: '+#10+m),PChar(Application.Title),MB_ICONINFORMATION+MB_OK);
end;

procedure TFRMWallet.cbExploreMyAccountsClick(Sender: TObject);
begin
  cbMyPrivateKeys.Enabled := cbExploreMyAccounts.Checked;
  UpdateAccounts(true);
  UpdateOperations;
end;

procedure TFRMWallet.cbFilterAccountsClick(Sender: TObject);
begin
  If not DoUpdateAccountsFilter then UpdateAccounts(true);
end;

procedure TFRMWallet.cbMyPrivateKeysChange(Sender: TObject);
begin
  UpdateAccounts(true);
end;

procedure TFRMWallet.CheckIsReady;
Var isready : String;
begin
  if Not Assigned(FNode) then Abort;

  if Not FNode.IsReady(isready) then begin
    Raise Exception.Create('You cannot do this operation now:'+#10+#10+isready);
  end;
end;

procedure TFRMWallet.CM_NetConnectionUpdated(var Msg: TMessage);
Const CT_BooleanToString : Array[Boolean] of String = ('False','True');
Var i : integer;
 NC : TNetConnection;
 l : TList<TNetConnection>;
 sClientApp, sLastConnTime : String;
 strings, sNSC, sRS, sDisc : TStrings;
 hh,nn,ss,ms : Word;
 {$IFDEF TESTNET}
 LFoundAccounts : Integer;
 {$ENDIF}
begin
  Try
    if Not TNetData.NetData.NetConnections.TryLockList(100,l) then exit;
    try
      strings := memoNetConnections.Lines;
      sNSC := TStringList.Create;
      sRS := TStringList.Create;
      sDisc := TStringList.Create;
      strings.BeginUpdate;
      Try
        for i := 0 to l.Count - 1 do begin
          NC := l[i];
          If NC.Client.BytesReceived>0 then begin
            sClientApp := '['+IntToStr(NC.NetProtocolVersion.protocol_version)+'-'+IntToStr(NC.NetProtocolVersion.protocol_available)+'] '+NC.ClientAppVersion;
          end else begin
            sClientApp := '(no data)';
          end;

          if NC.Connected then begin
            if NC.Client.LastCommunicationTime>1000 then begin
              DecodeTime(now - NC.Client.LastCommunicationTime,hh,nn,ss,ms);
              if (hh=0) and (nn=0) And (ss<10) then begin
                sLastConnTime := ' - Last comunication <10 sec.';
              end else begin
                sLastConnTime := Format(' - Last comunication %.2dm%.2ds',[(hh*60)+nn,ss]);
              end;
            end else begin
              sLastConnTime := '';
            end;
            if NC is TNetServerClient then begin
              sNSC.Add(Format('Client: IP:%s Block:%d Sent/Received:%d/%d Bytes - %s - Time offset %d - Active since %s %s',
                [NC.ClientRemoteAddr,NC.RemoteOperationBlock.block,NC.Client.BytesSent,NC.Client.BytesReceived,sClientApp,NC.TimestampDiff,DateTimeElapsedTime(NC.CreatedTime),sLastConnTime]));
            end else begin
              if NC.IsMyselfServer then sNSC.Add(Format('MySelf IP:%s Sent/Received:%d/%d Bytes - %s - Time offset %d - Active since %s %s',
                [NC.ClientRemoteAddr,NC.Client.BytesSent,NC.Client.BytesReceived,sClientApp,NC.TimestampDiff,DateTimeElapsedTime(NC.CreatedTime),sLastConnTime]))
              else begin
                sRS.Add(Format('Remote Server: IP:%s Block:%d Sent/Received:%d/%d Bytes - %s - Time offset %d - Active since %s %s',
                [NC.ClientRemoteAddr,NC.RemoteOperationBlock.block,NC.Client.BytesSent,NC.Client.BytesReceived,sClientApp,NC.TimestampDiff,DateTimeElapsedTime(NC.CreatedTime),sLastConnTime]));
              end;
            end;
          end else begin
            if NC is TNetServerClient then begin
              sDisc.Add(Format('Disconnected client: IP:%s - %s',[NC.ClientRemoteAddr,sClientApp]));
            end else if NC.IsMyselfServer then begin
              sDisc.Add(Format('Disconnected MySelf IP:%s - %s',[NC.ClientRemoteAddr,sClientApp]));
            end else begin
              sDisc.Add(Format('Disconnected Remote Server: IP:%s %s - %s',[NC.ClientRemoteAddr,CT_BooleanToString[NC.Connected],sClientApp]));
            end;
          end;
        end;
        strings.Clear;
        strings.Add(Format('Connections Updated %s Clients:%d Servers:%d (valid servers:%d)',[DateTimeToStr(now),sNSC.Count,sRS.Count,TNetData.NetData.NetStatistics.ServersConnectionsWithResponse]));
        strings.AddStrings(sRS);
        strings.AddStrings(sNSC);
        if sDisc.Count>0 then begin
          strings.Add('');
          strings.Add('Disconnected connections: '+Inttostr(sDisc.Count));
          strings.AddStrings(sDisc);
        end;
      Finally
        strings.EndUpdate;
        sNSC.Free;
        sRS.Free;
        sDisc.Free;
      End;
      //CheckMining;
    finally
      TNetData.NetData.NetConnections.UnlockList;
    end;
  Finally
    FMustProcessNetConnectionUpdated := false;
  End;
  {$IFDEF TESTNET}
  // TESTNET ONLY: Will automatic ask for get an account to other nodes if I have not enough
  if TPlatform.GetElapsedMilliseconds(FLastAskForAccountTC)<60000 then Exit; // 1 per minute
  FLastAskForAccountTC := TPlatform.GetTickCount;
  LFoundAccounts := 0;
  FNode.Bank.Vault.StartThreadSafe;
  try
    for i := 0 to FWalletKeys.AccountsKeyList.Count-1 do begin
      inc(LFoundAccounts,FWalletKeys.AccountsKeyList.AccountKeyList[i].Count);
      if LFoundAccounts>5 then Break;
    end;
  finally
    FNode.Bank.Vault.EndThreadSave;
  end;
  if LFoundAccounts<5 then begin
    // Will only ask if I have less than 5 accounts
    TABEYTNetDataExtraMessages.AskForFreeAccount(GetAccountKeyForMiner);
  end;
  {$ENDIF}
end;

procedure TFRMWallet.CM_WalletChanged(var Msg: TMessage);
begin
  UpdatePrivateKeys;
  FMustProcessWalletChanged := false;
end;

procedure TFRMWallet.dgAccountsClick(Sender: TObject);
begin
  UpdateOperations;
end;

procedure TFRMWallet.dgAccountsColumnMoved(Sender: TObject; FromIndex, ToIndex: Integer);
begin
  SaveAppParams;
end;

procedure TFRMWallet.dgAccountsFixedCellClick(Sender: TObject; ACol,
  ARow: Integer);
begin
  SaveAppParams;
end;

procedure TFRMWallet.DoUpdateAccounts;
begin
  UpdateAccounts(true);
end;

function TFRMWallet.DoUpdateAccountsFilter: Boolean;
Var m,bmin,bmax:Int64;
  doupd : Boolean;
begin
  if FUpdating then exit;
  FUpdating := true;
  Try
    If Not TAccountComp.TxtToMoney(ebFilterAccountByBalanceMin.Text,bmin) then bmin := 0;
    If not TAccountComp.TxtToMoney(ebFilterAccountByBalanceMax.Text,bmax) then bmax := CT_MaxWalletAmount;
    if (bmax<bmin) or (bmax=0) then bmax := CT_MaxWalletAmount;
    if bmin>bmax then bmin := 0;
    doupd := (bmin<>FMinAccountBalance) Or (bmax<>FMaxAccountBalance);
    if bmin>0 then
      ebFilterAccountByBalanceMin.Text:=TAccountComp.FormatMoney(bmin)
    else ebFilterAccountByBalanceMin.Text := '';
    if bmax<CT_MaxWalletAmount then
      ebFilterAccountByBalanceMax.Text := TAccountComp.FormatMoney(bmax)
    else ebFilterAccountByBalanceMax.Text := '';
    if cbFilterAccounts.Checked then begin
      FMinAccountBalance := bmin;
      FMaxAccountBalance := bmax;
      ebFilterAccountByBalanceMin.ParentFont := true;
      ebFilterAccountByBalanceMax.ParentFont := true;
    end else begin
      FMinAccountBalance := 0;
      FMaxAccountBalance := CT_MaxWalletAmount;
      ebFilterAccountByBalanceMin.font.Color := clDkGray;
      ebFilterAccountByBalanceMax.font.Color := clDkGray;
    end;
  Finally
    FUpdating := false;
  End;
  if doupd then UpdateAccounts(true);
  Result := doupd;
end;

procedure TFRMWallet.ebBlockChainBlockStartExit(Sender: TObject);
var bstart,bend : Int64;
begin
  If FUpdating then exit;
  FUpdating := True;
  Try
    bstart := StrToInt64Def(ebBlockChainBlockStart.Text,-1);
    bend := StrToInt64Def(ebBlockChainBlockEnd.Text,-1);
    FBlockChainGrid.SetBlocks(bstart,bend);
    if FBlockChainGrid.BlockStart>=0 then ebBlockChainBlockStart.Text := Inttostr(FBlockChainGrid.BlockStart) else ebBlockChainBlockStart.Text := '';
    if FBlockChainGrid.BlockEnd>=0 then ebBlockChainBlockEnd.Text := Inttostr(FBlockChainGrid.BlockEnd) else ebBlockChainBlockEnd.Text := '';
  Finally
    FUpdating := false;
  End;
end;

procedure TFRMWallet.ebBlockChainBlockStartKeyPress(Sender: TObject;
  var Key: Char);
begin
  if key=#13 then  ebBlockChainBlockStartExit(Nil);
end;

procedure TFRMWallet.ebFilterAccountByBalanceMinExit(Sender: TObject);
begin
  DoUpdateAccountsFilter;
end;

procedure TFRMWallet.ebFilterAccountByBalanceMinKeyPress(Sender: TObject;
  var Key: Char);
begin
  if key=#13 then DoUpdateAccountsFilter;
end;

procedure TFRMWallet.ebFilterOperationsAccountExit(Sender: TObject);
Var bstart,bend : Int64;
begin
  If FUpdating then exit;
  FUpdating := True;
  Try
    bstart := StrToInt64Def(ebFilterOperationsStartBlock.Text,-1);
    if bstart>=0 then ebFilterOperationsStartBlock.Text := Inttostr(bstart) else ebFilterOperationsStartBlock.Text := '';
    bend := StrToInt64Def(ebFilterOperationsEndBlock.Text,-1);
    if bend>=0 then ebFilterOperationsEndBlock.Text := Inttostr(bend) else ebFilterOperationsEndBlock.Text := '';
    FOperationsExplorerGrid.SetBlocks(bstart,bend);
  Finally
    FUpdating := false;
  End;
end;

procedure TFRMWallet.ebFilterOperationsAccountKeyPress(Sender: TObject;
  var Key: Char);
begin
  if key=#13 then  ebFilterOperationsAccountExit(Nil);
end;

procedure TFRMWallet.ebFindAccountNumberChange(Sender: TObject);
Var an : Cardinal;
begin
  if Trim(ebFindAccountNumber.Text)='' then begin
    ebFindAccountNumber.Color := clWindow;
    ebFindAccountNumber.Font.Color := clDkGray;
  end else if TAccountComp.AccountTxtNumberToAccountNumber(ebFindAccountNumber.Text,an) then begin
    ebFindAccountNumber.Color := clWindow;
    if FAccountsGrid.MoveRowToAccount(an) then begin
      ebFindAccountNumber.Font.Color := clWindowText;
    end else begin
      ebFindAccountNumber.Font.Color := clRed;
    end;
  end else begin
    // Invalid value
    ebFindAccountNumber.Color := clRed;
    ebFindAccountNumber.Font.Color := clWindowText;
  end;
end;

procedure TFRMWallet.ebFindAccountNumberExit(Sender: TObject);
begin
  ebFindAccountNumber.Text := '';
end;

procedure TFRMWallet.FinishedLoadingApp;
var LLockedMempool : TABEYOperationsComp;
begin
  FNodeNotifyEvents.Node := FNode;
  // Init
  TNetData.NetData.OnReceivedHelloMessage := OnReceivedHelloMessage;
  TNetData.NetData.OnStatisticsChanged := OnNetStatisticsChanged;
  TNetData.NetData.OnNetConnectionsUpdated := onNetConnectionsUpdated;
  TNetData.NetData.OnNodeServersUpdated := OnNetNodeServersUpdated;
  TNetData.NetData.OnBlackListUpdated := OnNetBlackListUpdated;
  //
  TimerUpdateStatus.Interval := 1000;
  TimerUpdateStatus.Enabled := true;
  //
  FPoolMiningServer := TPoolMiningServer.Create;
  FPoolMiningServer.Port := FAppParams.ParamByName[CT_PARAM_JSONRPCMinerServerPort].GetAsInteger(CT_JSONRPCMinerServer_Port);
  FPoolMiningServer.MinerAccountKey := GetAccountKeyForMiner;
  FPoolMiningServer.MinerPayload := TEncoding.ANSI.GetBytes( FAppParams.ParamByName[CT_PARAM_MinerName].GetAsString('') );
  LLockedMempool := FNode.LockMempoolWrite;
  try
    LLockedMempool.AccountKey := GetAccountKeyForMiner;
  finally
    FNode.UnlockMempoolWrite;
  end;
  FPoolMiningServer.Active := FAppParams.ParamByName[CT_PARAM_JSONRPCMinerServerActive].GetAsBoolean(true);
  FPoolMiningServer.OnMiningServerNewBlockFound := OnMiningServerNewBlockFound;

  Image.Free; // free resources for our GIF
  IdleTimer.Enabled := False; // no more timer
  imgChain.Visible := False;
  FreeAndNil(FBackgroundLabel);
  FreeAndNil(FBackgroundPanel);

  //PageControl.Visible:=True;
  //PageControl.Enabled:=True;
  UpdatePrivateKeys;
  backImg.Visible := False;
end;

procedure TFRMWallet.FillAccountInformation(const Strings: TStrings;
  const AccountNumber: Cardinal);
Var account : TAccount;
  s : String;
begin
  if AccountNumber<0 then exit;
  account := FNode.GetMempoolAccount(AccountNumber);
  if Length(account.name)>0 then s:='Name: '+TEncoding.ANSI.GetString(account.name)
  else s:='';
  Strings.Add(Format('Account: %s %s Type:%d',[TAccountComp.AccountNumberToAccountTxtNumber(AccountNumber),s,account.account_type]));
  Strings.Add('');
  Strings.Add(Format('Current balance: %s',[TAccountComp.FormatMoney(account.balance)]));
  Strings.Add('');
  Strings.Add(Format('Updated on block: %d  (%d blocks ago)',[account.updated_block,FNode.Bank.BlocksCount-account.updated_block]));
  Strings.Add(Format('Public key type: %s',[TAccountComp.GetECInfoTxt(account.accountInfo.accountKey.EC_OpenSSL_NID)]));
  Strings.Add(Format('Base58 Public key: %s',[TAccountComp.AccountPublicKeyExport(account.accountInfo.accountKey)]));
  if TAccountComp.IsAccountForSale(account.accountInfo) then begin
    Strings.Add('');
    Strings.Add('** Account is for sale: **');
    Strings.Add(Format('Price: %s',[TAccountComp.FormatMoney(account.accountInfo.price)]));
    Strings.Add(Format('Seller account (where to pay): %s',[TAccountComp.AccountNumberToAccountTxtNumber(account.accountInfo.account_to_pay)]));
    if TAccountComp.IsAccountForSaleAcceptingTransactions(account.accountInfo) then begin
      Strings.Add('');
      Strings.Add('** Private sale **');
      Strings.Add(Format('New Base58 Public key: %s',[TAccountComp.AccountPublicKeyExport(account.accountInfo.new_publicKey)]));
      Strings.Add('');
      if TAccountComp.IsAccountLocked(account.accountInfo,FNode.Bank.BlocksCount) then begin
        Strings.Add(Format('PURCHASE IS SECURE UNTIL BLOCK %d (current %d, remains %d)',
          [account.accountInfo.locked_until_block,FNode.Bank.BlocksCount,account.accountInfo.locked_until_block-FNode.Bank.BlocksCount]));
      end else begin
        Strings.Add(Format('PURCHASE IS NOT SECURE (Expired on block %d, current %d)',
          [account.accountInfo.locked_until_block,FNode.Bank.BlocksCount]));
      end;
    end;
  end;
end;

procedure TFRMWallet.FillOperationInformation(const Strings: TStrings;
  const OperationResume: TOperationResume);
var i : Integer;
  jsonObj : TABEYJSONObject;
begin
  If (not OperationResume.valid) then exit;
  If OperationResume.Block<FNode.Bank.BlocksCount then
    if (OperationResume.NOpInsideBlock>=0) then begin
      Strings.Add(Format('Block: %d/%d',[OperationResume.Block,OperationResume.NOpInsideBlock]))
    end else begin
      Strings.Add(Format('Block: %d',[OperationResume.Block]))
    end
  else Strings.Add('** Pending operation not included on blockchain **');
  Strings.Add(Format('%s',[OperationResume.OperationTxt]));
  If (OperationResume.isMultiOperation) then begin
    Strings.Add('Multioperation:');
    For i := 0 to High(OperationResume.Senders) do begin
      Strings.Add(Format('  Sender (%d/%d): %s %s ABEY Payload:%s',[i+1,length(OperationResume.Senders),TAccountComp.AccountNumberToAccountTxtNumber(OperationResume.Senders[i].Account),TAccountComp.FormatMoney(OperationResume.Senders[i].Amount),TCrypto.ToHexaString(OperationResume.Senders[i].Payload)]));
    end;
    For i := 0 to High(OperationResume.Receivers) do begin
      Strings.Add(Format('  Receiver (%d/%d): %s %s ABEY Payload:%s',[i+1,length(OperationResume.Receivers),TAccountComp.AccountNumberToAccountTxtNumber(OperationResume.Receivers[i].Account),TAccountComp.FormatMoney(OperationResume.Receivers[i].Amount),TCrypto.ToHexaString(OperationResume.Receivers[i].Payload)]));
    end;
    For i := 0 to High(OperationResume.Changers) do begin
      Strings.Add(Format('  Change info (%d/%d): %s [%s]',[i+1,length(OperationResume.Changers),TAccountComp.AccountNumberToAccountTxtNumber(OperationResume.Changers[i].Account),TOpMultiOperation.OpChangeAccountInfoTypesToText(OperationResume.Changers[i].Changes_type)]));
    end;

  end;
  Strings.Add(Format('OpType:%d Subtype:%d',[OperationResume.OpType,OperationResume.OpSubtype]));
  Strings.Add(Format('Operation Hash (ophash): %s',[TCrypto.ToHexaString(OperationResume.OperationHash)]));
  If (Length(OperationResume.OperationHash_OLD)>0) then begin
    Strings.Add(Format('Old Operation Hash (old_ophash): %s',[TCrypto.ToHexaString(OperationResume.OperationHash_OLD)]));
  end;
  if (Length(OperationResume.OriginalPayload)>0) then begin
    Strings.Add(Format('Payload length:%d',[length(OperationResume.OriginalPayload)]));
    If OperationResume.PrintablePayload<>'' then begin
      Strings.Add(Format('Payload (human): %s',[OperationResume.PrintablePayload]));
    end;
    Strings.Add(Format('Payload (Hexadecimal): %s',[TCrypto.ToHexaString(OperationResume.OriginalPayload)]));
  end;
  If OperationResume.Balance>=0 then begin
    Strings.Add(Format('Final balance: %s',[TAccountComp.FormatMoney(OperationResume.Balance)]));
  end;
  jsonObj := TABEYJSONObject.Create;
  Try
    TABEYJSONComp.FillOperationObject(OperationResume,FNode.Bank.BlocksCount,jsonObj);
    Strings.Add('JSON:');
    Strings.Add(jsonObj.ToJSON(False));
  Finally
    jsonObj.Free;
  end;
end;

{$IFDEF TESTNET}
procedure TFRMWallet.InitMenuForTesting;
var mi : TMenuItem;
begin
  mi := TMenuItem.Create(MainMenu);
  mi.Caption:='-';
  miAbout.Add(mi);
  {$IFDEF TESTING_NO_POW_CHECK}
  mi := TMenuItem.Create(MainMenu);
  mi.Caption:='Create a block';
  mi.OnClick:=Test_CreateABlock;
  miAbout.Add(mi);
  {$ENDIF}
  mi := TMenuItem.Create(MainMenu);
  mi.Caption:='Show public keys state';
  mi.OnClick:=Test_ShowPublicKeys;
  miAbout.Add(mi);
  mi := TMenuItem.Create(MainMenu);
  mi.Caption:='Create Random operations';
  mi.OnClick:=Test_RandomOperations;
  miAbout.Add(mi);
  mi := TMenuItem.Create(MainMenu);
  mi.Caption:='Ask for Free Account';
  mi.OnClick:=Test_AskForFreeAccount;
  miAbout.Add(mi);
end;

{$IFDEF TESTING_NO_POW_CHECK}
procedure TFRMWallet.Test_CreateABlock(Sender: TObject);
var ops : TABEYOperationsComp;
  nba : TBlockAccount;
  errors : AnsiString;
begin
  {$IFDEF TESTNET}
  ops := TABEYOperationsComp.Create(Nil);
  Try
    ops.bank := FNode.Bank;
    ops.CopyFrom(FNode.Operations);
    ops.BlockPayload:=IntToStr(FNode.Bank.BlocksCount);
    ops.nonce := FNode.Bank.BlocksCount;
    ops.UpdateTimestamp;
    FNode.AddNewBlockChain(Nil,ops,nba,errors);
  finally
    ops.Free;
  end;
  {$ELSE}
  Raise Exception.Create('NOT ALLOWED!');
  {$ENDIF}
end;
{$ENDIF}

procedure TFRMWallet.Test_RandomOperations(Sender: TObject);
Var FRM : TFRMRandomOperations;
begin
  FRM := TFRMRandomOperations.Create(Self);
  Try
    FRM.SourceNode := FNode;
    FRM.SourceWalletKeys := FWalletKeys;
    FRM.ShowModal;
  finally
    FRM.Free;
  end;
end;

Procedure TFRMWallet.Test_AskForFreeAccount(Sender: TObject);
var i : Integer;
begin
  i := TABEYTNetDataExtraMessages.AskForFreeAccount(GetAccountKeyForMiner);
  if i>0 then ShowMessage(Format('Asked for a free account to %d nodes...',[i]))
  else ShowMessage('Sorry. No nodes available to ask for a free account');
end;

{$ENDIF}

procedure TFRMWallet.Test_ShowPublicKeys(Sender: TObject);
var F : TFRMMemoText;
  i : Integer;
  sl : TStrings;
  ak : TAccountKey;
  nmin,nmax : Integer;
  l : TList<Pointer>;
  Pacsd : PAccountKeyStorageData;
  acc : TAccount;
begin
  sl := TStringList.Create;
  try
    for i:=0 to FNode.Bank.Vault.AccountsCount-1 do begin
      acc := FNode.Bank.Vault.Account(i);
      if acc.accountInfo.new_publicKey.EC_OpenSSL_NID<>0 then begin
        sl.Add(Format('Account %d new public key %d %s',[acc.account,
          acc.accountInfo.new_publicKey.EC_OpenSSL_NID,
          TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(acc.accountInfo.new_publicKey))]));
      end;
    end;
    l := TAccountKeyStorage.KS.LockList;
    try
      sl.Add(Format('%d public keys in TAccountKeyStorage data',[l.count]));
      for i:=0 to l.count-1 do begin
        Pacsd := l[i];
        if (Pacsd^.counter<=0) then begin
          sl.Add(Format('%d/%d public keys counter %d',[i+1,l.count,Pacsd^.counter]));
        end;
        if FNode.Bank.Vault.OrderedAccountKeysList.IndexOfAccountKey(Pacsd^.ptrAccountKey^)<0 then begin
          sl.Add(Format('%d/%d public keys counter %d Type %d NOT FOUND %s',[i+1,l.count,Pacsd^.counter,
          Pacsd^.ptrAccountKey^.EC_OpenSSL_NID,
          TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(Pacsd^.ptrAccountKey^))]));
        end;
      end;
    finally
      TAccountKeyStorage.KS.UnlockList;
    end;
    sl.Add(Format('%d public keys in %d accounts',[FNode.Bank.Vault.OrderedAccountKeysList.Count,FNode.Bank.Vault.AccountsCount]));
    for i:=0 to FNode.Bank.Vault.OrderedAccountKeysList.Count-1 do begin
      ak := FNode.Bank.Vault.OrderedAccountKeysList.AccountKey[i];
      if ( FNode.Bank.Vault.OrderedAccountKeysList.AccountKeyList[i].Count > 0) then begin
        nmin := FNode.Bank.Vault.OrderedAccountKeysList.AccountKeyList[i].Get(0);
        nmax := FNode.Bank.Vault.OrderedAccountKeysList.AccountKeyList[i].Get( FNode.Bank.Vault.OrderedAccountKeysList.AccountKeyList[i].Count-1 );
      end else begin
        nmin := -1; nmax := -1;
      end;
      sl.Add(Format('%d/%d %d accounts (%d to %d) for key type %d %s',[
        i+1,FNode.Bank.Vault.OrderedAccountKeysList.Count,
        FNode.Bank.Vault.OrderedAccountKeysList.AccountKeyList[i].Count,
        nmin,nmax,
        ak.EC_OpenSSL_NID,
        TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(ak)) ]));
    end;
    F := TFRMMemoText.Create(Self);
    try
      F.InitData('Keys in Vault',sl.Text);
      F.ShowModal;
    finally
      F.Free;
    end;
  finally
    sl.Free;
  end;
end;

function TFRMWallet.ForceMining: Boolean;
begin
  Result := false;
end;

procedure TFRMWallet.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Hide;
  frmNotifications.Hide;
  frmStatistics.Hide;

  // event was not assigned, but code was there // 30.04.2020
  {if Assigned(FThreadActivate) then begin
    TThreadActivate(FThreadActivate).Terminate;
    FThreadActivate := Nil;
  end;}
end;

procedure TFRMWallet.UpdateTimerTimer(Sender: TObject);
var PID:Integer;
begin
  if (Timeout >= TIMEOUT_TO_DIE) then
  begin
    Timeout := TIMEOUT_TO_DIE;

    PID:=GetProcessID;
    {$IFDEF UNIX}
    FpKill(PID, 9);
    {$ENDIF UNIX}

    {$IFDEF Windows}
    TerminateProcessByID(PID);
    {$ENDIF Windows}
  end;
  Inc(Timeout);
  Caption := 'ABEY Wallet (Terminating in '+IntToStr(TIMEOUT_TO_DIE-Timeout)+' seconds...)';
end;

procedure TFRMWallet.FormCreate(Sender: TObject);
Var i : Integer;
begin
  lblVersion.Caption := 'Version '+IntToStr(VERSION_WALLET_MAJOR)+'.'+IntToStr(VERSION_WALLET_MINOR)+'.'+IntToStr(VERSION_WALLET_BUILD);

  frmStatistics := TStatisticsForm.Create(Self);
  frmStatistics.MainForm := Self;
  //frmStatistics.ResizeToFit();
  frmNotifications := TNotificationsForm.Create(Self);
  frmNotifications.MainForm := Self;

  // Update when operations are completed
  UpdateVersionThread := TUpdateThread.Create(True, 'wallet');
  UpdateVersionThread.OnUpdateDone := UpdateDone;
  //UpdateVersionThread.Start;

  {$IFNDEF FPC}
  {$IFDEF TESTNET}
  System.ReportMemoryLeaksOnShutdown := True; // Delphi memory leaks testing
  {$ENDIF}
  {$ENDIF}
  {$IFDEF TESTNET}
  FLastAskForAccountTC := 0;
  {$ENDIF}
  FLastAccountsGridInvalidateTC := TPlatform.GetTickCount;
  FLastNodesCacheUpdatedTS := Now;
  FBackgroundPanel := Nil;
  FBackgroundLabel := Nil;
  FThreadActivate := Nil;
  FMustProcessWalletChanged := false;
  FMustProcessNetConnectionUpdated := false;
  FRPCServer := Nil;
  FNode := Nil;
  FPoolMiningServer := Nil;
  FMinAccountBalance := 0;
  FMaxAccountBalance := CT_MaxWalletAmount;
  FMessagesUnreadCount := 0;
  frmNotifications.lblMessages.Caption := 'You have no new messages.';
  memoNetConnections.Lines.Clear;
  memoNetServers.Lines.Clear;
  memoNetBlackLists.Lines.Clear;
  memoMessages.Lines.Clear;
  memoMessageToSend.Lines.Clear;
  FUpdating := false;
  TimerUpdateStatus.Enabled := false;
  FIsActivated := false;
  FWalletKeys := TWalletKeysExt.Create(Self);
  {for i := 0 to StatusBar.Panels.Count - 1 do begin
    StatusBar.Panels[i].Text := '';
  end;}
  FLog := TLog.Create(Self);
  FLog.OnNewLog := OnNewLog;
  FLog.SaveTypes := [];
  If Not ForceDirectories(TFolderHelper.GetABEYDataFolder) then raise Exception.Create('Cannot create dir: '+TFolderHelper.GetABEYDataFolder);
  FAppParams := TAppParams.Create(self);
  FAppParams.FileName := TFolderHelper.GetABEYDataFolder+PathDelim+'ABEY.config';
  FNodeNotifyEvents := TNodeNotifyEvents.Create(Self);
  FNodeNotifyEvents.OnBlocksChanged := OnNewAccount;
  FNodeNotifyEvents.OnNodeMessageEvent := OnNodeMessageEvent;
  FNodeNotifyEvents.OnKeyActivity := OnNodeKeysActivity;
  FAccountsGrid := TAccountsGrid.Create(Self);
  FAccountsGrid.DrawGrid := dgAccounts;
  FAccountsGrid.AllowMultiSelect := True;
  FAccountsGrid.OnAccountsGridUpdatedData := OnAccountsGridUpdatedData;
  FAccountsGrid.AccountsGridDatasource := acds_Node;
  FSelectedAccountsGrid := TAccountsGrid.Create(Self);
  FSelectedAccountsGrid.AccountsGridDatasource := acds_InternalList;
  FSelectedAccountsGrid.DrawGrid := dgSelectedAccounts;
  FSelectedAccountsGrid.OnUpdated := OnSelectedAccountsGridUpdated;
  FOperationsAccountGrid := TOperationsGrid.Create(Self);
  FOperationsAccountGrid.DrawGrid := dgAccountOperations;
  FOperationsAccountGrid.MustShowAlwaysAnAccount := true;
  FPendingOperationsGrid := TOperationsGrid.Create(Self);
  FPendingOperationsGrid.DrawGrid := dgPendingOperations;
  FPendingOperationsGrid.AccountNumber := -1; // all
  FPendingOperationsGrid.PendingOperations := true;
  FOperationsExplorerGrid := TOperationsGrid.Create(Self);
  FOperationsExplorerGrid.DrawGrid := dgOperationsExplorer;
  FOperationsExplorerGrid.AccountNumber := -1;
  FOperationsExplorerGrid.PendingOperations := False;
  FBlockChainGrid := TBlockChainGrid.Create(Self);
  FBlockChainGrid.DrawGrid := dgBlockChainExplorer;
  // FWalletKeys.OnChanged.Add( OnWalletChanged );
  LoadAppParams;
  UpdatePrivateKeys;
  UpdateBlockChainState;
  UpdateConnectionStatus;
  PageControl.ActivePage := tsMyAccounts;
  pcAccountsOptions.ActivePage := tsAccountOperations;
  ebFilterOperationsStartBlock.Text := '';
  ebFilterOperationsEndBlock.Text := '';
  cbExploreMyAccounts.Checked:=True; // By default
  cbExploreMyAccountsClick(nil);

  MinersBlocksFound := 0;
  {$IFDEF TESTNET}
  Image1.visible := false;
  {$ENDIF}
  PageControl.Enabled := False;
  PageControl.Visible := False;

  AllButtonsUp;
  btnExplorer_Pressed := True;
  btnExplorer.Brush.Color := BTN_PRESSED_COLOR;
  connectExplorer.Visible := True;

  FBackgroundPanel := TPanel.Create(Self);
  FBackgroundPanel.OnPaint := BackgroundPanelOnPaint;
  FBackgroundPanel.Parent := Self;
  FBackgroundPanel.Align := alClient;
  FBackgroundPanel.Font.Size:=15;
  FBackgroundPanel.BevelWidth := 0;
  FBackgroundLabel := TLabel.Create(Self);
  FBackgroundLabel.Parent := FBackgroundPanel;
  FBackgroundLabel.Align := alNone;
  FBackgroundLabel.Left := 0;
  FBackgroundLabel.Height := 50;
  FBackgroundLabel.Width := FBackgroundPanel.Width;
  FBackgroundLabel.Anchors := [];
  FBackgroundLabel.Layout := tlCenter;
  FBackgroundLabel.Font.Size := 18;
  FBackgroundLabel.Alignment := taCenter;
  FBackgroundLabel.WordWrap := True;
  imgChain.Parent := FBackgroundPanel;
  imgChain.Anchors := [];
  imgChain.AutoSize := True;
  imgChain.Left := (FBackgroundPanel.Width - imgChain.Width) div 2;
  imgChain.Visible := True;

  // add GIF image
  image := TAnimatedGif.Create('images/PleaseWait.gif');
  image.EraseColor := self.Color;
  image.BackgroundMode := TGifBackgroundMode(3); // gbmEraseBackground
  BackgroundPanelOnPaint(Self);
  IdleTimer.Enabled := True;
  FBackgroundLabel.Top := 14;
  imgChain.Top := 0 + 15;
  //FBackgroundLabel.Top := 100;//r.bottom + 5;

  cbHashRateUnits.Items.Clear;
  cbHashRateUnits.Items.Add('h/s');
  cbHashRateUnits.Items.Add('Kh/s');
  cbHashRateUnits.Items.Add('Mh/s');
  cbHashRateUnits.Items.Add('Gh/s');
  cbHashRateUnits.Items.Add('Th/s');
  cbHashRateUnits.Items.Add('Ph/s');
  cbHashRateUnits.Items.Add('Eh/s');
  {$IFDEF TESTNET}
  // Things for testing purposes only
  InitMenuForTesting;
  {$ENDIF}
end;

procedure TFRMWallet.BackgroundPanelOnPaint(Sender: TObject);
var i: integer;
begin
  //UpdateBackground;
  //background.Draw(panel1.Canvas,ClientRect);
  fBackgroundPanel.Invalidate;
  r.left := (fBackgroundPanel.width - image.width) div 2;
  r.top := ((fBackgroundPanel.height- image.height) div 2);// + (image.height div 2);
  r.right := r.left + image.width;
  r.bottom := r.top + image.height;

  fBackgroundPanel.Canvas.StretchDraw(r,image);
  //IdleTimer1.Enabled:= true;
end;


procedure TFRMWallet.ebHashRateBackBlocksKeyPress(Sender: TObject; var Key: char);
begin
  if Key=#13 then ebHashRateBackBlocksExit(Nil);
end;

procedure TFRMWallet.ResizeColumns;
begin
  // resize column
  dgAccounts.ColWidths[2] := dgAccounts.ClientWidth - dgAccounts.ColWidths[0] -
                                   dgAccounts.ColWidths[1] - dgAccounts.ColWidths[3] - dgAccounts.ColWidths[4] - dgAccounts.ColWidths[5] - dgAccounts.ColWidths[6];
  dgAccountOperations.ColWidths[2] := dgAccountOperations.ClientWidth - dgAccountOperations.ColWidths[0] -
                                   dgAccountOperations.ColWidths[1] - dgAccountOperations.ColWidths[3] - dgAccountOperations.ColWidths[4] -
                                   dgAccountOperations.ColWidths[5] - dgAccountOperations.ColWidths[6] -
                                   dgAccountOperations.ColWidths[7] - dgAccountOperations.ColWidths[8];
  dgPendingOperations.ColWidths[2] := dgPendingOperations.ClientWidth - dgPendingOperations.ColWidths[0] -
                                   dgPendingOperations.ColWidths[1] - dgPendingOperations.ColWidths[3] - dgPendingOperations.ColWidths[4] -
                                   dgPendingOperations.ColWidths[5] - dgPendingOperations.ColWidths[6] -
                                   dgPendingOperations.ColWidths[7] - dgPendingOperations.ColWidths[8];
  dgAccounts.Invalidate;
  dgAccountOperations.Invalidate;
  dgPendingOperations.Invalidate;

end;

procedure TFRMWallet.FormChangeBounds(Sender: TObject);
begin
  frmStatistics.ResizeToFit;
  frmNotifications.ResizeToFit;

  ResizeColumns;
end;

procedure TFRMWallet.ebHashRateBackBlocksExit(Sender: TObject);
var i : Integer;
begin
  If FUpdating then exit;
  FUpdating := True;
  Try
    i := StrToIntDef(ebHashRateBackBlocks.Text,-1);
    FBlockChainGrid.HashRateAverageBlocksCount:=i;
    FAppParams.ParamByName[CT_PARAM_HashRateAvgBlocksCount].Value := FBlockChainGrid.HashRateAverageBlocksCount;
  Finally
    ebHashRateBackBlocks.Text := IntToStr(FBlockChainGrid.HashRateAverageBlocksCount);
    FUpdating := false;
  End;
end;

procedure TFRMWallet.cbHashRateUnitsClick(Sender: TObject);
begin
  If FUpdating then Exit;
  FUpdating := True;
  Try
    case cbHashRateUnits.ItemIndex of
      0 : FBlockChainGrid.HashRateAs := hr_Unit;
      1 : FBlockChainGrid.HashRateAs := hr_Kilo;
      2 : FBlockChainGrid.HashRateAs := hr_Mega;
      3 : FBlockChainGrid.HashRateAs := hr_Giga;
      4 : FBlockChainGrid.HashRateAs := hr_Tera;
      5 : FBlockChainGrid.HashRateAs := hr_Peta;
      6 : FBlockChainGrid.HashRateAs := hr_Exa;
    else FBlockChainGrid.HashRateAs := hr_Mega;
    end;
    FAppParams.ParamByName[CT_PARAM_ShowHashRateAs].Value := Integer(FBlockChainGrid.HashRateAs);
  Finally
    FUpdating := false;
  End;
end;

procedure TFRMWallet.dgAccountsHeaderSized(Sender: TObject; IsColumn: Boolean;
  Index: Integer);
begin
  ResizeColumns;
end;

procedure TFRMWallet.FormDestroy(Sender: TObject);
Var i : Integer;
  step : String;
begin
  TLog.NewLog(ltinfo,Classname,'Destroying form - START');
  Try
    if Assigned(FThreadActivate) then begin
      TThreadActivate(FThreadActivate).Terminate;
      FThreadActivate := Nil;
    end;

  FreeAndNil(FRPCServer);
  FreeAndNil(FPoolMiningServer);
  step := 'Saving params';
  SaveAppParams;
  FreeAndNil(FAppParams);
  //
  step := 'Assigning nil events';
  FLog.OnNewLog :=Nil;
  FNodeNotifyEvents.Node := Nil;
  FOperationsAccountGrid.Node := Nil;
  FOperationsExplorerGrid.Node := Nil;
  FPendingOperationsGrid.Node := Nil;
  FAccountsGrid.Node := Nil;
  FSelectedAccountsGrid.Node := Nil;
  TNetData.NetData.OnReceivedHelloMessage := Nil;
  TNetData.NetData.OnStatisticsChanged := Nil;
  TNetData.NetData.OnNetConnectionsUpdated := Nil;
  TNetData.NetData.OnNodeServersUpdated := Nil;
  TNetData.NetData.OnBlackListUpdated := Nil;
  //

  step := 'Destroying NodeNotifyEvents';
  FreeAndNil(FNodeNotifyEvents);
  //
  step := 'Assigning Nil to TNetData';
  TNetData.NetData.OnReceivedHelloMessage := Nil;
  TNetData.NetData.OnStatisticsChanged := Nil;

  step := 'Destroying grids operators';
  FreeAndNil(FOperationsAccountGrid);
  FreeAndNil(FOperationsExplorerGrid);
  FreeAndNil(FBlockChainGrid);

  step := 'Desactivating Node';
  TNode.Node.NetServer.Active := false;
  FNode := Nil;

  TNetData.NetData.Free;

  step := 'Processing messages 1';
  Application.ProcessMessages;

  step := 'Destroying Node';
  TNode.Node.Free;

  step := 'Destroying Wallet';
  FreeAndNil(FWalletKeys);
  step := 'Processing messages 2';
  Application.ProcessMessages;
  step := 'Destroying stringslist';
  Except
    On E:Exception do begin
      TLog.NewLog(lterror,Classname,'Error destroying Form step: '+step+' Errors ('+E.ClassName+'): ' +E.Message);
    end;
  End;
  TLog.NewLog(ltinfo,Classname,'Destroying form - END');
  FreeAndNil(FLog);
  Sleep(100);
end;

procedure TFRMWallet.FormResize(Sender: TObject);
begin
  frmStatistics.ResizeToFit;
  frmNotifications.ResizeToFit;
  ResizeColumns;
end;

procedure TFRMWallet.FormWindowStateChange(Sender: TObject);
begin
  if WindowState = wsMinimized then // hide the statistics window
  begin
    frmStatistics.Hide;
    frmNotifications.Hide;
  end
  else
  begin
    frmStatistics.Show;
    frmNotifications.Show;
  end;
end;

procedure TFRMWallet.Image1Click(Sender: TObject);
begin

end;

procedure TFRMWallet.imgConfigClick(Sender: TObject);
begin
  AllButtonsUp;
  btnConfig_Pressed := True;
  btnOperations1.Brush.Color := BTN_PRESSED_COLOR;
  Notebook.PageIndex :=  PAGE_CONFIG;
  connectConfig.Visible := True;

  // open the keys page in this frame
  frmWalletConfig := TFRMABEYWalletConfig.Create(ConfigFrame);
  Try
    frmWalletConfig.Parent := ConfigFrame;
    frmWalletConfig.Align := alClient;
    frmWalletConfig.BorderIcons := [];
    frmWalletConfig.BorderStyle := bsNone;
    //frmWalletConfig.ParentBackground := True;

    // mandatory
    frmWalletConfig.AppParams := Self.FAppParams;
    frmWalletConfig.WalletKeys := Self.FWalletKeys;

    frmWalletConfig.Show;
  Finally
  End;

end;

procedure TFRMWallet.imgConfigMouseEnter(Sender: TObject);
begin
  btnOperations1.Brush.Color := BTN_HIGHLIGHT_COLOR;
end;

procedure TFRMWallet.imgConfigMouseLeave(Sender: TObject);
begin
  if btnConfig_Pressed then
    btnOperations1.Brush.Color := BTN_PRESSED_COLOR
  else
    btnOperations1.Brush.Color := clWhite;
end;

procedure TFRMWallet.AllButtonsUp;
begin
  // if wallet keys form acive, update keys (e.g. save them before clearing form)
  if Assigned(frmWalletKeys) then
    UpdatePrivateKeys;

  // if config form is active, update config
  if Assigned(frmWalletConfig) then
  begin
    SaveAppParams;
    UpdateConfigChanged;
  end;


  btnExplorer.Brush.Color := clWhite;
  btnKeys.Brush.Color := clWhite;
  btnOperations.Brush.Color := clWhite;
  btnOperations1.Brush.Color := clWhite;
  btnExplorer_Pressed := False;
  btnKeys_Pressed := False;
  btnOperations_Pressed := False;
  btnConfig_Pressed := False;

  connectExplorer.Visible := False;
  connectKeys.Visible := False;
  connectOperations.Visible := False;
  connectConfig.Visible := False;

  // if wallet keys form active, free it
  if Assigned(frmWalletKeys) then
    FreeAndNil(frmWalletKeys);
  // if operations form is active, destroy it
  if Assigned(frmWalletOperations) then
  FreeAndNil(frmWalletOperations);

  // if confirm form active, free it
  if Assigned(frmWalletConfig) then
    FreeAndNil(frmWalletConfig);
end;

procedure TFRMWallet.imgExplorerClick(Sender: TObject);
begin
  AllButtonsUp;
  btnExplorer_Pressed := True;
  btnExplorer.Brush.Color := BTN_PRESSED_COLOR;
  Notebook.PageIndex :=  PAGE_BLOCKCHAIN_EXPLORER;
  connectExplorer.Visible := True;
end;

procedure TFRMWallet.imgExplorerMouseEnter(Sender: TObject);
begin
  btnExplorer.Brush.Color := BTN_HIGHLIGHT_COLOR;
end;

procedure TFRMWallet.imgExplorerMouseLeave(Sender: TObject);
begin
  if btnExplorer_Pressed then
    btnExplorer.Brush.Color := BTN_PRESSED_COLOR
  else
    btnExplorer.Brush.Color := clWhite;
end;

procedure TFRMWallet.imgKeysClick(Sender: TObject);
begin
  AllButtonsUp;
  btnKeys_Pressed := True;
  btnKeys.Brush.Color := BTN_PRESSED_COLOR;
  Notebook.PageIndex :=  PAGE_KEYS;
  connectKeys.Visible := True;

  // open the keys page in this frame
  frmWalletKeys := TFRMWalletKeys.Create(KeysFrame);
  Try
    frmWalletKeys.Parent := KeysFrame;
    frmWalletKeys.Align := alClient;
    frmWalletKeys.BorderIcons := [];
    frmWalletKeys.BorderStyle := bsNone;
    //frmWalletKeys.ParentBackground := True;

    frmWalletKeys.WalletKeys := FWalletKeys;
    frmWalletKeys.Show;
    //UpdatePrivateKeys;
  Finally
    //frmWalletKeys.Free;
  End;
end;

procedure TFRMWallet.imgKeysMouseEnter(Sender: TObject);
begin
  btnKeys.Brush.Color := BTN_HIGHLIGHT_COLOR;
end;

procedure TFRMWallet.imgKeysMouseLeave(Sender: TObject);
begin
  if btnKeys_Pressed then
    btnKeys.Brush.Color := BTN_PRESSED_COLOR
  else
    btnKeys.Brush.Color := clWhite;
end;

procedure TFRMWallet.imgOperationsClick(Sender: TObject);
var l : TOrderedCardinalList;
    isready : String;
    Proceed: Boolean;
begin
  // open the operations page
  Proceed := True;
  if (Not Assigned(FNode)) or (Not FNode.IsReady(isready)) then // not ready
  begin
    Raise Exception.Create('You cannot do this operation now:'+#10+#10+isready);
    Exit;
  end;
  l := TOrderedCardinalList.Create;
  If (not Assigned(FAccountsGrid)) or (FAccountsGrid.SelectedAccounts(l)<1) then
    Proceed := False;
//  If (FAccountsGrid.SelectedAccounts(l)<1) then Proceed := False;
  if not Proceed then
    raise Exception.Create('Please first select an account to perform operations upon!');

  AllButtonsUp;
  btnOperations_Pressed := True;
  btnOperations.Brush.Color := BTN_PRESSED_COLOR;
  Notebook.PageIndex :=  PAGE_OPERATIONS;
  connectOperations.Visible := True;

  frmWalletOperations := TFRMOperation.Create(OperationsFrame);
  l := TOrderedCardinalList.Create;
  frmWalletOperations.Parent := OperationsFrame;
  frmWalletOperations.Align := alClient;
  frmWalletOperations.BorderIcons := [];
  frmWalletOperations.BorderStyle := bsNone;
  //frmWalletOperations.ParentBackground := True;

  try
    If FAccountsGrid.SelectedAccounts(l)<1 then raise Exception.Create('Please first select an account to perform operations upon!');
    frmWalletOperations.SenderAccounts.CopyFrom(l);
  finally
    l.Free;
   end;
   frmWalletOperations.DefaultFee := FAppParams.ParamByName[CT_PARAM_DefaultFee].GetAsInt64(0);
   frmWalletOperations.WalletKeys := FWalletKeys;

   frmWalletOperations.lbl_AccountID.Caption := FloatToStrF(FAccountsGrid.AccountNumber(dgAccounts.Row), ffNumber, 18, 0);
//   frmWalletOperations.lbl_AccountName.Caption := FAccountsGrid.AccountNumber(dgAccounts.Row);

   frmWalletOperations.Show;
end;

procedure TFRMWallet.imgOperationsMouseEnter(Sender: TObject);
begin
  btnOperations.Brush.Color := BTN_HIGHLIGHT_COLOR;
end;

procedure TFRMWallet.imgOperationsMouseLeave(Sender: TObject);
begin
  if btnOperations_Pressed then
    btnOperations.Brush.Color := BTN_PRESSED_COLOR
  else
    btnOperations.Brush.Color := clWhite;
end;

procedure TFRMWallet.KeysBeforeShow(ASender: TObject; ANewPage: TPage;
  ANewIndex: Integer);
begin

end;

procedure TFRMWallet.Label21Click(Sender: TObject);
begin

end;

procedure TFRMWallet.lblAccountsBalance1Click(Sender: TObject);
begin

end;

procedure TFRMWallet.MiOperationsExplorerClick(Sender: TObject);
begin
  With TFRMOperationsExplorer.Create(Self) do
  try
    SourceNode := FNode;
    SourceWalletKeys := FWalletKeys;
    ShowModal;
  finally
    Free;
  end;
end;

procedure TFRMWallet.MiRPCCallsClick(Sender: TObject);
Var FRM : TFRMRPCCalls;
begin
  FRM := TFRMRPCCalls.Create(Self);
  Try
    FRM.ServerURL:='127.0.0.1:'+IntToStr(FRPCServer.Port);
    FRM.ShowModal;
  finally
    FRM.Free;
  end;
end;

procedure TFRMWallet.sbSearchAccountClick(Sender: TObject);
Var F : TFRMAccountSelect;
begin
  F := TFRMAccountSelect.Create(Self);
  try
    F.Node := FNode;
    F.WalletKeys := FWalletKeys;
    F.ShowModal;
  finally
    F.Free;
  end;
end;

procedure TFRMWallet.Shape2ChangeBounds(Sender: TObject);
begin

end;

procedure TFRMWallet.Splitter2ChangeBounds(Sender: TObject);
begin
  RealignShapesUI;
end;

procedure TFRMWallet.Splitter2Moved(Sender: TObject);
begin
end;

procedure TFRMWallet.Splitter3ChangeBounds(Sender: TObject);
begin
  RealignShapesUI;
end;

procedure TFRMWallet.Splitter3Moved(Sender: TObject);
begin
end;

procedure TFRMWallet.RealignShapesUI;
begin
  shapeTX.Top := panelTransactions.Top + Label21.Top + 8;
  shapeQ.Top := panelTransactions.Top + panelQueue.Top + Label23.Top + 8;
  Shape21.Height := shapeQ.Top+2;
end;

function TFRMWallet.GetAccountKeyForMiner: TAccountKey;
Var PK : TECPrivateKey;
  i : Integer;
  PublicK : TECDSA_Public;
begin
  Result := CT_TECDSA_Public_Nul;
  if Not Assigned(FWalletKeys) then exit;
  if Not Assigned(FAppParams) then exit;
  case FMinerPrivateKeyType of
    mpk_NewEachTime: PublicK := CT_TECDSA_Public_Nul;
    mpk_Selected: begin
      PublicK := TAccountComp.RawString2Accountkey(FAppParams.ParamByName[CT_PARAM_MinerPrivateKeySelectedPublicKey].GetAsTBytes(Nil));
    end;
  else
    // Random
    PublicK := CT_TECDSA_Public_Nul;
    if FWalletKeys.Count>0 then begin
      i := Random(FWalletKeys.Count);
      if (Length(FWalletKeys.Key[i].CryptedKey)=0) then begin
        // Not valid, search for first valid:
        i:=0;
        while (i<FWalletKeys.Count) And (Length(FWalletKeys.Key[i].CryptedKey)=0) do inc(i);
        if i<FWalletKeys.Count then PublicK := FWalletKeys.Key[i].AccountKey;
      end else PublicK := FWalletKeys.Key[i].AccountKey;
    end;
  end;
  i := FWalletKeys.IndexOfAccountKey(PublicK);
  if i>=0 then begin
    if (Length(FWalletKeys.Key[i].CryptedKey)=0) then i:=-1;
  end;
  if i<0 then begin
    PK := TECPrivateKey.Create;
    try
      PK.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
      FWalletKeys.AddPrivateKey('Self-generated key at '+DateTimeToStr(Now), PK);
      PublicK := PK.PublicKey;
      // Set to AppParams if not mpk_NewEachTime
      if (FMinerPrivateKeyType<>mpk_NewEachTime) then begin
        FAppParams.ParamByName[CT_PARAM_MinerPrivateKeySelectedPublicKey].SetAsTBytes(TAccountComp.AccountKey2RawString(PublicK));
        FMinerPrivateKeyType:=mpk_Selected;
        FAppParams.ParamByName[CT_PARAM_MinerPrivateKeyType].Value := Integer(mpk_Selected);
      end;
    finally
      PK.Free;
    end;
  end;
  Result := PublicK;
end;

procedure TFRMWallet.IPnodes1Click(Sender: TObject);
Var FRM : TFRMNodesIp;
begin
  FRM := TFRMNodesIp.Create(Self);
  Try
    FRM.AppParams := FAppParams;
    FRM.ShowModal;
  Finally
    FRM.Free;
  End;
end;

procedure TFRMWallet.lblReceivedMessagesClick(Sender: TObject);
begin
  PageControl.ActivePage := tsMessages;
end;

procedure TFRMWallet.LoadAppParams;
Var ms : TMemoryStream;
  s : AnsiString;
begin
  ms := TMemoryStream.Create;
  Try
    s := FAppParams.ParamByName[CT_PARAM_GridAccountsStream].GetAsString('');
    ms.WriteBuffer(s[1],length(s));
    ms.Position := 0;
    // Disabled on V2: FAccountsGrid.LoadFromStream(ms);
  Finally
    ms.Free;
  End;
  If FAppParams.FindParam(CT_PARAM_MinerName)=Nil then begin
    // New configuration... assigning a new random value
    FAppParams.ParamByName[CT_PARAM_MinerName].SetAsString('New Node '+DateTimeToStr(Now)+' - '+
      CT_ClientAppVersion);
  end;
  FBlockChainGrid.ShowTimeAverageColumns:={$IFDEF SHOW_AVERAGE_TIME_STATS}True;{$ELSE}False;{$ENDIF}
  UpdateConfigChanged;
end;

procedure TFRMWallet.MiAccountInformationClick(Sender: TObject);
Var F : TFRMMemoText;
  accn : Int64;
  s,title : String;
  account : TAccount;
  strings : TStrings;
  i : Integer;
  opr : TOperationResume;
begin
  accn := -1;
  title := '';
  strings := TStringList.Create;
  try
    opr := CT_TOperationResume_NUL;
    if PageControl.ActivePage=tsOperations then begin
      i := FOperationsExplorerGrid.DrawGrid.Row;
      if (i>0) and (i<=FOperationsExplorerGrid.OperationsResume.Count) then begin
        opr := FOperationsExplorerGrid.OperationsResume.OperationResume[i-1];
      end;
    end else if PageControl.ActivePage=tsPendingOperations then begin
      i := FPendingOperationsGrid.DrawGrid.Row;
      if (i>0) and (i<=FPendingOperationsGrid.OperationsResume.Count) then begin
        opr := FPendingOperationsGrid.OperationsResume.OperationResume[i-1];
      end;
    end else if PageControl.ActivePage=tsMyAccounts then begin
      accn := FAccountsGrid.AccountNumber(dgAccounts.Row);
      if accn<0 then raise Exception.Create('Select an account');
      FillAccountInformation(strings,accn);
      title := 'Account '+TAccountComp.AccountNumberToAccountTxtNumber(accn)+' info';
      i := FOperationsAccountGrid.DrawGrid.Row;
      if (i>0) and (i<=FOperationsAccountGrid.OperationsResume.Count) then begin
        opr := FOperationsAccountGrid.OperationsResume.OperationResume[i-1];
      end;
    end;
    If (opr.valid) then begin
      if accn>=0 then strings.Add('')
      else title := 'Operation info';
      strings.Add('Operation info:');
      FillOperationInformation(strings,opr);
    end else if accn<0 then Raise Exception.Create('No info available');
    F := TFRMMemoText.Create(Self);
    Try
      F.Caption := title;
      F.Memo.Lines.Assign(strings);
      F.ShowModal;
    Finally
      F.Free;
    End;
  finally
    strings.free;
  end;
end;

procedure TFRMWallet.MiAddaccounttoSelectedClick(Sender: TObject);
begin
  PageControl.ActivePage := tsMyAccounts;
  PageControlChange(Nil);
  pcAccountsOptions.ActivePage := tsMultiSelectAccounts;
  sbSelectedAccountsAddClick(Sender);
end;

procedure TFRMWallet.MiCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFRMWallet.MiDecodePayloadClick(Sender: TObject);
begin
  // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  Exit;
  // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
(*  if {PageControl.ActivePage=tsOperations} dgAccounts.Focused then begin
    FOperationsExplorerGrid.ShowModalDecoder(FWalletKeys,FAppParams);
  end else *) if (Sender is TDrawGrid) and ((Sender as TDrawGrid).Name = 'dgPendingOperations') then begin
    FPendingOperationsGrid.ShowModalDecoder(FWalletKeys,FAppParams);
  end else if (Sender is TDrawGrid) and ((Sender as TDrawGrid).Name = 'dgAccountOperations') then begin
    FOperationsAccountGrid.ShowModalDecoder(FWalletKeys,FAppParams);
  end;

end;

procedure TFRMWallet.MiFindaccountClick(Sender: TObject);
begin
  PageControl.ActivePage := tsMyAccounts;
  PageControlChange(Nil);
  ebFindAccountNumber.SetFocus;
end;

procedure TFRMWallet.MiFindnextaccountwithhighbalanceClick(Sender: TObject);
Var an  : Cardinal;
  an64 : Int64;
  start : TAccount;
begin
  PageControl.ActivePage := tsMyAccounts;
  PageControlChange(Nil);
  an64 := FAccountsGrid.AccountNumber(dgAccounts.Row);
  if an64<0 then an := 0
  else an := an64;
  If an>=FNode.Bank.Vault.AccountsCount then exit;
  start := FNode.Bank.Vault.Account(an);
  while (an<FNode.Bank.Vault.AccountsCount)  do begin
    if FNode.Bank.Vault.Account(an).balance>start.balance then break
    else inc(an);
  end;
  if (an<FNode.Bank.Vault.AccountsCount) then FAccountsGrid.MoveRowToAccount(an)
  else raise Exception.Create('Not found any account higher than '+TAccountComp.AccountNumberToAccountTxtNumber(start.account)+' with balance higher than '+
    TAccountComp.FormatMoney(start.balance));
end;

procedure TFRMWallet.MiFindOperationbyOpHashClick(Sender: TObject);
Var FRM : TFRMPayloadDecoder;
  oph : String;
begin
  oph := '';
  if Not InputQuery('Search operation by OpHash','Insert Operation Hash value (OpHash)',oph) then exit;
  //
  FRM := TFRMPayloadDecoder.Create(Self);
  try
    FRM.Init(CT_TOperationResume_NUL,WalletKeys,FAppParams);
    FRM.DoFind(oph);
    FRM.ShowModal;
  finally
    FRM.Free;
  end;
end;

procedure TFRMWallet.MiFindpreviousaccountwithhighbalanceClick(Sender: TObject);
Var an  : Cardinal;
  an64 : Int64;
  start : TAccount;
begin
  PageControl.ActivePage := tsMyAccounts;
  PageControlChange(Nil);
  an64 := FAccountsGrid.AccountNumber(dgAccounts.Row);
  if an64<0 then an := FNode.Bank.Vault.AccountsCount-1
  else an := an64;
  If an>=FNode.Bank.Vault.AccountsCount then exit;
  start := FNode.Bank.Vault.Account(an);
  while (an>0)  do begin
    if FNode.Bank.Vault.Account(an).balance>start.balance then break
    else dec(an);
  end;
  if (FNode.Bank.Vault.Account(an).balance>start.balance) then FAccountsGrid.MoveRowToAccount(an)
  else raise Exception.Create('Not found any account lower than '+TAccountComp.AccountNumberToAccountTxtNumber(start.account)+' with balance higher than '+
    TAccountComp.FormatMoney(start.balance));
end;

procedure TFRMWallet.MiMultiaccountoperationClick(Sender: TObject);
begin
  PageControl.ActivePage := tsMyAccounts;
  pcAccountsOptions.ActivePage := tsMultiSelectAccounts;
  bbSelectedAccountsOperationClick(Sender);
end;

procedure TFRMWallet.miNewOperationClick(Sender: TObject);
var l : TOrderedCardinalList;
begin
  CheckIsReady;
  With TFRMOperation.Create(Self) do
  Try
    l := TOrderedCardinalList.Create;
    try
      If FAccountsGrid.SelectedAccounts(l)<1 then raise Exception.Create('No row selected');
      SenderAccounts.CopyFrom(l);
    finally
      l.Free;
    end;
    DefaultFee := FAppParams.ParamByName[CT_PARAM_DefaultFee].GetAsInt64(0);
    WalletKeys := FWalletKeys;
    ShowModal;
  Finally
    Free;
  End;
end;

procedure TFRMWallet.miOptionsClick(Sender: TObject);
begin
  With TFRMABEYWalletConfig.Create(Self) do
  try
    AppParams := Self.FAppParams;
    WalletKeys := Self.FWalletKeys;
    if ShowModal=MrOk then begin
      SaveAppParams;
      UpdateConfigChanged;
    end;
  finally
    free;
  end;
end;

procedure TFRMWallet.miPrivatekeysClick(Sender: TObject);
Var FRM : TFRMWalletKeys;
begin
  FRM := TFRMWalletKeys.Create(Self);
  Try
    FRM.WalletKeys := FWalletKeys;
    FRM.ShowModal;
    UpdatePrivateKeys;
  Finally
    FRM.Free;
  End;
end;

procedure TFRMWallet.MiRemoveaccountfromselectedClick(Sender: TObject);
begin
  PageControl.ActivePage := tsMyAccounts;
  PageControlChange(Nil);
  pcAccountsOptions.ActivePage := tsMultiSelectAccounts;
  sbSelectedAccountsDelClick(Sender);
end;

procedure TFRMWallet.OnAccountsGridUpdatedData(Sender: TObject);
begin
  if FAccountsGrid.IsUpdatingData then begin
    lblAccountsCount.Caption := 'Determinining accounts, please wait...';
    lblAccountsBalance1.Caption := 'Determinining balance, please wait...';
  end else begin
    lblAccountsCount.Caption := FloatToStrF(FAccountsGrid.AccountsCount, ffNumber, 18, 0)+' accounts';
    lblAccountsBalance1.Caption := TAccountComp.FormatMoney(FAccountsGrid.AccountsBalance);
  end;
end;

procedure TFRMWallet.OnMiningServerNewBlockFound(Sender: TObject);
begin
  FPoolMiningServer.MinerAccountKey := GetAccountKeyForMiner;
end;

procedure TFRMWallet.OnNetBlackListUpdated(Sender: TObject);
Const CT_TRUE_FALSE : Array[Boolean] Of AnsiString = ('FALSE','TRUE');
Var i,j,n : integer;
 P : PNodeServerAddress;
 l : TList<Pointer>;
 strings : TStrings;
begin
  l := TNetData.NetData.NodeServersAddresses.LockList;
  try
    strings := memoNetBlackLists.Lines;
    strings.BeginUpdate;
    Try
      strings.Clear;
      strings.Add('BlackList Updated: '+DateTimeToStr(now)+' by TID:'+IntToHex(PtrInt(TThread.CurrentThread.ThreadID),8));
      j := 0; n:=0;
      for i := 0 to l.Count - 1 do begin
        P := l[i];
        if (P^.is_blacklisted) then begin
          inc(n);
          if Not P^.its_myself then begin
            inc(j);
            strings.Add(Format('Blacklist IP:%s:%d LastConnection:%s Reason: %s',
              [
               P^.ip,P^.port,
               DateTimeToStr(UnivDateTime2LocalDateTime( UnixToUnivDateTime(P^.last_connection))),P^.BlackListText]));
          end;
        end;
      end;
      Strings.Add(Format('Total Blacklisted IPs: %d (Total %d)',[j,n]));
    Finally
      strings.EndUpdate;
    End;
  finally
    TNetData.NetData.NodeServersAddresses.UnlockList;
  end;
end;

procedure TFRMWallet.OnNetConnectionsUpdated(Sender: TObject);
begin
  if FMustProcessNetConnectionUpdated then exit;
  FMustProcessNetConnectionUpdated := true;
  PostMessage(Self.Handle,CM_PC_NetConnectionUpdated,0,0);
end;

procedure TFRMWallet.OnNetNodeServersUpdated(Sender: TObject);
Var i : integer;
 P : PNodeServerAddress;
 l : TList<Pointer>;
 strings : TStrings;
 s : String;
begin
  l := TNetData.NetData.NodeServersAddresses.LockList;
  try
    strings := memoNetServers.Lines;
    strings.BeginUpdate;
    Try
      strings.Clear;
      strings.Add('NodeServers Updated: '+DateTimeToStr(now) +' Count: '+inttostr(l.Count));
      for i := 0 to l.Count - 1 do begin
        P := l[i];
        if Not (P^.is_blacklisted) then begin
          s := Format('Server IP:%s:%d',[P^.ip,P^.port]);
          if (P^.last_connection_by_me>0) then begin
            s := s + ' [Server] ';
          end;

          if Assigned(P.netConnection) then begin
            If P.last_connection>0 then  s := s+ ' ** ACTIVE **'
            else s := s+' ** TRYING TO CONNECT **';
          end;
          if P.its_myself then begin
            s := s+' ** NOT VALID ** '+P.BlackListText;
          end;
          if P.last_connection>0 then begin
            s := s + ' Last connection: '+DateTimeToStr(UnivDateTime2LocalDateTime( UnixToUnivDateTime(P^.last_connection)));
          end;
          if P.last_connection_by_server>0 then begin
            s := s + ' Last server connection: '+DateTimeToStr(UnivDateTime2LocalDateTime( UnixToUnivDateTime(P^.last_connection_by_server)));
          end;
          if (P.last_attempt_to_connect>0) then begin
            s := s + ' Last attempt to connect: '+DateTimeToStr(P^.last_attempt_to_connect);
          end;
          if (P.total_failed_attemps_to_connect>0) then begin
            s := s + ' (Attempts: '+inttostr(P^.total_failed_attemps_to_connect)+')';
          end;

          strings.Add(s);
        end;
      end;
    Finally
      strings.EndUpdate;
    End;
  finally
    TNetData.NetData.NodeServersAddresses.UnlockList;
  end;
end;

procedure TFRMWallet.OnNetStatisticsChanged(Sender: TObject);
Var NS : TNetStatistics;
begin
  //CheckMining;
  if Assigned(FNode) then begin
    If FNode.NetServer.Active then begin
      //StatusBar.Panels[0].Text := 'Active (Port '+Inttostr(FNode.NetServer.Port)+')';
      frmNotifications.imgNetworkOn.Visible := True;
      frmNotifications.imgNetworkOff.Visible := False;
    end else
    begin
      //StatusBar.Panels[0].Text := 'Server stopped';
      frmNotifications.imgNetworkOn.Visible := False;
      frmNotifications.imgNetworkOff.Visible := True;
    end;
    NS := TNetData.NetData.NetStatistics;
    {StatusBar.Panels[1].Text := Format('Connections:%d Clients:%d Servers:%d - Rcvd:%d Kb Send:%d Kb',
      [NS.ActiveConnections,NS.ClientsConnections,NS.ServersConnections,NS.BytesReceived DIV 1024,NS.BytesSend DIV 1024]);}
    frmNotifications.lblClients.Caption := FloatToStrF(NS.ClientsConnections, ffNumber, 18, 0);
    frmNotifications.lblServers.Caption := FloatToStrF(NS.ServersConnections, ffNumber, 18, 0);
    frmNotifications.lblReceived.Caption := FloatToStrF(NS.BytesReceived / 1048576, ffNumber, 18, 2)+' MB';
    frmNotifications.lblSent.Caption := FloatToStrF(NS.BytesSend / 1048576, ffNumber, 18, 2)+' MB';
    // update network chart
    frmNotifications.ReceivedLineSeries.Add(Abs((NS.BytesReceived / 1024) - lastReceived));
    frmNotifications.SentLineSeries.Add(Abs((NS.BytesSend / 1024) - lastSent));
    if REFRESH_CHARTS and (frmNotifications.ReceivedLineSeries.Count >= MAX_REFRESH_POINTS_BANDWIDTH) then
    begin
       frmNotifications.ReceivedLineSeries.Delete(0);
       frmNotifications.SentLineSeries.Delete(0);
    end;

    lastSent := NS.BytesSend / 1024;
    lastReceived := NS.BytesReceived / 1024;
  end else begin
    frmNotifications.imgNetworkOn.Visible := False;
    frmNotifications.imgNetworkOff.Visible := True;
    //StatusBar.Panels[0].Text := '';
    //StatusBar.Panels[1].Text := '';
  end;
end;

procedure TFRMWallet.OnNewAccount(Sender: TObject);
begin
  Try
    UpdateAccounts(false);
    UpdateBlockChainState;
  Except
    On E:Exception do begin
      E.Message := 'Exception at OnNewAccount '+E.ClassName+': '+E.Message;
      TLog.NewLog(lterror,ClassName,E.Message);
      Raise;
    end;
  end;
end;

procedure TFRMWallet.OnNewLog(logtype: TLogType; Time : TDateTime; ThreadID : TThreadID; const sender,logtext: String);
Var s : AnsiString;
begin
  if (logtype=ltdebug) And (Not cbShowDebugLogs.Checked) then exit;
  if ThreadID=MainThreadID then s := ' MAIN:' else s:=' TID:';
  if MemoLogs.Lines.Count>300 then begin
    // Limit max lines in logs...
    memoLogs.Lines.BeginUpdate;
    try
      while memoLogs.Lines.Count>250 do memoLogs.Lines.Delete(0);
    finally
      memoLogs.Lines.EndUpdate;
    end;
  end;
  memoLogs.Lines.Add(formatDateTime('dd/mm/yyyy hh:nn:ss.zzz',Time)+s+IntToHex(PtrInt(ThreadID),8)+' ['+CT_LogType[Logtype]+'] <'+sender+'> '+logtext);
  //
end;

procedure TFRMWallet.OnNodeMessageEvent(NetConnection: TNetConnection; MessageData: String);
Var s : String;
begin
  inc(FMessagesUnreadCount);
  if Assigned(NetConnection) then begin
    s := DateTimeToStr(now)+' Message received from '+NetConnection.ClientRemoteAddr;
    memoMessages.Lines.Add(DateTimeToStr(now)+' Message received from '+NetConnection.ClientRemoteAddr+' Length '+inttostr(Length(MessageData))+' bytes');
    memoMessages.Lines.Add('RECEIVED> '+MessageData);
    if FAppParams.ParamByName[CT_PARAM_ShowModalMessages].GetAsBoolean(false) then begin
      s := DateTimeToStr(now)+' Message from '+NetConnection.ClientRemoteAddr+#10+
         'Length '+inttostr(length(MessageData))+' bytes'+#10+#10;
      if TCrypto.IsHumanReadable(TEncoding.ANSI.GetBytes(MessageData)) then begin
         s := s + MessageData;
      end else begin
         s := s +'Value in hexadecimal:'+#10+
              TCrypto.ToHexaString(TEncoding.ANSI.GetBytes(MessageData));
      end;
      Application.MessageBox(PChar(s),PChar(Application.Title),MB_ICONINFORMATION+MB_OK);
    end;
  end else begin
    memoMessages.Lines.Add(DateTimeToStr(now)+' Internal message: '+MessageData);
  end;
  if FMessagesUnreadCount>1 then frmNotifications.lblMessages.Caption := Format('You have %d new messages.',[FMessagesUnreadCount])
  else frmNotifications.lblMessages.Caption := 'You have 1 new message.';
end;

procedure TFRMWallet.OnNodeKeysActivity(Sender: TObject);
begin
  DoUpdateAccounts;
end;

procedure TFRMWallet.OnReceivedHelloMessage(Sender: TObject);
Var nsarr : TNodeServerAddressArray;
  i : Integer;
  s : AnsiString;
begin
  If (FLastNodesCacheUpdatedTS + EncodeTime(0,5,0,0) > Now) then exit; // Prevent continuous saving
  FLastNodesCacheUpdatedTS := Now;
  // Update node servers Peer Cache
  nsarr := TNetData.NetData.NodeServersAddresses.GetValidNodeServers(true,0);
  s := '';
  for i := low(nsarr) to High(nsarr) do begin
    if (s<>'') then s := s+';';
    s := s + nsarr[i].ip+':'+IntToStr( nsarr[i].port );
  end;
  FAppParams.ParamByName[CT_PARAM_PeerCache].SetAsString(s);
  TNode.Node.PeerCache := s;
end;

procedure TFRMWallet.OnSelectedAccountsGridUpdated(Sender: TObject);
begin
  lblSelectedAccountsCount.Caption := Inttostr(FSelectedAccountsGrid.AccountsCount);
  lblSelectedAccountsBalance.Caption := TAccountComp.FormatMoney( FSelectedAccountsGrid.AccountsBalance );
end;

procedure TFRMWallet.OnWalletChanged(Sender: TObject);
begin
  if FMustProcessWalletChanged then exit;
  FMustProcessWalletChanged := true;
  PostMessage(Self.Handle,CM_PC_WalletKeysChanged,0,0);
end;

procedure TFRMWallet.PageControlChange(Sender: TObject);
begin
  MiDecodePayload.Enabled := false;

  { Accounts/Blockchain Explorer }
  FAccountsGrid.Node := FNode;
  MiDecodePayload.Enabled := true;
  FSelectedAccountsGrid.Node := FNode;

  { Queue }
  FPendingOperationsGrid.Node := FNode;
  MiDecodePayload.Enabled := true;

  { Messages }
  UpdateAvailableConnections;
  FMessagesUnreadCount := 0;
  frmNotifications.lblMessages.Caption := 'You have no new messages.';

{  if PageControl.ActivePage=tsMyAccounts then begin
  end else begin
    FAccountsGrid.Node := Nil;
    FSelectedAccountsGrid.Node := Nil;
  end;}

  // updated 01.05.2020
  {if PageControl.ActivePage=tsPendingOperations then begin
    FPendingOperationsGrid.Node := FNode;
    MiDecodePayload.Enabled := true;
  end else FPendingOperationsGrid.Node := Nil;}

  if PageControl.ActivePage=tsBlockChain then FBlockChainGrid.Node := FNode
  else FBlockChainGrid.Node := Nil;
  if PageControl.ActivePage=tsOperations then begin
    FOperationsExplorerGrid.Node := FNode;
    MiDecodePayload.Enabled := true;
  end else FOperationsExplorerGrid.Node := Nil;
{  if PageControl.ActivePage=tsMessages then begin
    UpdateAvailableConnections;
    FMessagesUnreadCount := 0;
    frmNotifications.lblMessages.Caption := 'You have no new messages.';
  end;}
end;

procedure TFRMWallet.SaveAppParams;
Var ms : TMemoryStream;
  s : AnsiString;
begin
  ms := TMemoryStream.Create;
  Try
    FAccountsGrid.SaveToStream(ms);
    ms.Position := 0;
    setlength(s,ms.Size);
    ms.ReadBuffer(s[1],ms.Size);
    FAppParams.ParamByName[CT_PARAM_GridAccountsStream].SetAsString(s);
  Finally
    ms.Free;
  End;
end;

procedure TFRMWallet.sbSelectedAccountsAddAllClick(Sender: TObject);
Var lsource,ltarget : TOrderedCardinalList;
  i : Integer;
begin
  lsource := FAccountsGrid.LockAccountsList;
  Try
    ltarget := FSelectedAccountsGrid.LockAccountsList;
    Try
      for i := 0 to lsource.Count-1 do begin
        if FWalletKeys.IndexOfAccountKey(FNode.Bank.Vault.Account(lsource.Get(i)).accountInfo.accountKey)<0 then raise Exception.Create(Format('You cannot operate with account %d because private key not found in your wallet',[lsource.Get(i)]));
        ltarget.Add(lsource.Get(i));
      end;
    Finally
      FSelectedAccountsGrid.UnlockAccountsList;
    End;
  Finally
    FAccountsGrid.UnlockAccountsList;
  End;
end;

procedure TFRMWallet.sbSelectedAccountsAddClick(Sender: TObject);
Var l, selected : TOrderedCardinalList;
  an : Int64;
  i : Integer;
begin
  an := FAccountsGrid.AccountNumber(dgAccounts.Row);
  if (an<0) then raise Exception.Create('No account selected');
  if FWalletKeys.IndexOfAccountKey(FNode.Bank.Vault.Account(an).accountInfo.accountkey)<0 then
    raise Exception.Create(Format('You cannot add %s account because private key not found in your wallet.'#10+#10+'You''re not the owner!',
      [TAccountComp.AccountNumberToAccountTxtNumber(an)]));
  // Add
  l := FSelectedAccountsGrid.LockAccountsList;
  selected := TOrderedCardinalList.Create;
  Try
    FAccountsGrid.SelectedAccounts(selected);
    for i := 0 to selected.Count-1 do begin
      l.Add(selected.Get(i));
    end;
  Finally
    selected.Free;
    FSelectedAccountsGrid.UnlockAccountsList;
  End;
end;

procedure TFRMWallet.sbSelectedAccountsDelAllClick(Sender: TObject);
Var l : TOrderedCardinalList;
begin
  l := FSelectedAccountsGrid.LockAccountsList;
  try
    l.Clear;
  finally
    FSelectedAccountsGrid.UnlockAccountsList;
  end;
end;

procedure TFRMWallet.sbSelectedAccountsDelClick(Sender: TObject);
Var an : Int64;
  l : TOrderedCardinalList;
begin
  l := FSelectedAccountsGrid.LockAccountsList;
  try
    an := FSelectedAccountsGrid.AccountNumber(dgSelectedAccounts.Row);
    if an>=0 then l.Remove(an);
  finally
    FSelectedAccountsGrid.UnlockAccountsList;
  end;
end;

procedure TFRMWallet.SetMinersBlocksFound(const Value: Integer);
begin
  FMinersBlocksFound := Value;
  //lblBlocksFound.Caption := Inttostr(Value);
  frmStatistics.lblBlocksFound.Caption := FloatToStrF(Value, ffNumber, 18, 0);
  if Value>0 then frmStatistics.lblBlocksFound.Font.Color := clGreen
  else frmStatistics.lblBlocksFound.Font.Color := clDkGray;
end;

procedure TFRMWallet.TimerUpdateStatusTimer(Sender: TObject);
begin
  Try
    { Added 02.05.2020 }
    FPendingOperationsGrid.Node := nil;
    FPendingOperationsGrid.Node := FNode;
    // ...this above is to ensure the queue is constantly updated; a more elegant solution would be a callback (notify event) after operation
    // is removed from the queue
    //FAccountsGrid.Node := nil;
    //FAccountsGrid.Node := FNode;
    // update the operations of this account, this event may have been processed already
    FOperationsAccountGrid.Node := nil;
    FOperationsAccountGrid.Node := FNode;

    // Resize columns
    ResizeColumns;

    UpdateConnectionStatus;
    UpdateBlockChainState;
    UpdateNodeStatus;
  Except
    On E:Exception do begin
      E.Message := 'Exception at TimerUpdate '+E.ClassName+': '+E.Message;
      TLog.NewLog(lterror,ClassName,E.Message);
    end;
  End;
end;

procedure TFRMWallet.UpdateAccounts(RefreshData : Boolean);
Var accl : TOrderedCardinalList;
  l : TOrderedCardinalList;
  i,j,k : Integer;
  c  : Cardinal;
  LApplyfilter : Boolean;
  acc : TAccount;
  LFilters : TAccountsGridFilter;
begin
  If Not Assigned(FWalletKeys) Then exit;
  if Not Assigned(FNode) then Exit;

  if Not RefreshData then begin
    if TPlatform.GetElapsedMilliseconds(FLastAccountsGridInvalidateTC)>1000 then begin
      FLastAccountsGridInvalidateTC := TPlatform.GetTickCount;
      dgAccounts.Invalidate;
    end;
    exit;
  end;
  LApplyfilter := (cbFilterAccounts.Checked) and ((FMinAccountBalance>0) Or ((FMaxAccountBalance<CT_MaxWalletAmount) and (FMaxAccountBalance>=0)));
  if (Not cbExploreMyAccounts.Checked) And (not LApplyfilter) then begin
    FAccountsGrid.AccountsGridDatasource := acds_Node;
    FAccountsGrid.UpdateData;
  end else begin
    LFilters := FAccountsGrid.AccountsGridFilter;
    LFilters.MinBalance := FMinAccountBalance;
    LFilters.MaxBalance := FMaxAccountBalance;
    if cbExploreMyAccounts.Checked then begin
      FNode.Bank.Vault.StartThreadSafe;
      try
        LFilters.OrderedAccountsKeyList := FWalletKeys.AccountsKeyList;
        if cbMyPrivateKeys.ItemIndex>0 then begin
          i := PtrInt(cbMyPrivateKeys.Items.Objects[cbMyPrivateKeys.ItemIndex]);
          if (i>=0) And (i<FWalletKeys.Count) then begin
            LFilters.indexAccountsKeyList := FWalletKeys.AccountsKeyList.IndexOfAccountKey(FWalletKeys[i].AccountKey);
          end;
        end else LFilters.indexAccountsKeyList := -1;
      finally
        FNode.Bank.Vault.EndThreadSave;
      end;
    end else begin
      LFilters.OrderedAccountsKeyList := Nil;
      LFilters.indexAccountsKeyList := -1;
    end;
    FAccountsGrid.AccountsGridFilter := LFilters;
    FAccountsGrid.AccountsGridDatasource := acds_NodeFiltered;
  end;

  bbChangeKeyName.Enabled := cbExploreMyAccounts.Checked;
  OnAccountsGridUpdatedData(Nil);
  UpdateOperations;
end;

procedure TFRMWallet.UpdateAvailableConnections;
Var i : integer;
 NC : TNetConnection;
 l : TList<TNetConnection>;
begin
  if Not TNetData.NetData.NetConnections.TryLockList(100,l) then exit;
  try
    lbNetConnections.Items.BeginUpdate;
    Try
      lbNetConnections.Items.Clear;
      for i := 0 to l.Count - 1 do begin
        NC := l[i];
        if NC.Connected then begin
          if NC is TNetServerClient then begin
            if Not NC.IsMyselfServer then begin
              lbNetConnections.Items.AddObject(Format('Client: IP:%s',[NC.ClientRemoteAddr]),NC);
            end;
          end else begin
            if Not NC.IsMyselfServer then begin
              lbNetConnections.Items.AddObject(Format('Server: IP:%s',[NC.ClientRemoteAddr]),NC);
            end;
          end;
        end;
      end;
    Finally
      lbNetConnections.Items.EndUpdate;
    End;
  finally
    TNetData.NetData.NetConnections.UnlockList;
  end;
end;

procedure TFRMWallet.UpdateBlockChainState;
Var isMining : boolean;
  i,mc : Integer;
  s : String;
  f, favg : real;
  LLockedMempool : TABEYOperationsComp;
begin
  UpdateNodeStatus;
  mc := 0;
  if Assigned(FNode) then
  begin
    if FNode.Bank.BlocksCount>0 then
    begin
      //lblCurrentBlock.Caption :=  Inttostr(FNode.Bank.BlocksCount)+' (0..'+Inttostr(FNode.Bank.BlocksCount-1)+')'; ;
      frmStatistics.lblBlocks.Caption := FloatToStrF(FNode.Bank.BlocksCount, ffNumber, 18, 0);
    end
    else
       //lblCurrentBlock.Caption :=  '(none)';
       frmStatistics.lblBlocks.Caption := 'N/A';

    //lblCurrentAccounts.Caption := Inttostr(FNode.Bank.AccountsCount);
    frmStatistics.lblAccounts.Caption := FloatToStrF(FNode.Bank.AccountsCount, ffNumber, 18, 0);

    //lblCurrentBlockTime.Caption := UnixTimeToLocalElapsedTime(FNode.Bank.LastOperationBlock.timestamp);
    frmStatistics.lblBlockTime.Caption := UnixTimeToLocalElapsedTime(FNode.Bank.LastOperationBlock.timestamp);

    LLockedMempool := FNode.LockMempoolRead;
    try
      ///**//lblOperationsPending.Caption := Inttostr(LLockedMempool.Count);
      frmStatistics.lblPendingOperations.Caption := FloatToStrF(LLockedMempool.Count, ffNumber, 18, 0);
      ///**//lblCurrentDifficulty.Caption := InttoHex(LLockedMempool.OperationBlock.compact_target,8);
      frmStatistics.lblMiningTarget.Caption := InttoHex(LLockedMempool.OperationBlock.compact_target,8);
    finally
      FNode.UnlockMempoolRead;
    end;
    favg := FNode.Bank.GetActualTargetSecondsAverage(CT_CalcNewTargetBlocksAverage);
    f := (CT_NewLineSecondsAvg - favg) / CT_NewLineSecondsAvg;
    //lblTimeAverage.Caption := 'Last '+Inttostr(CT_CalcNewTargetBlocksAverage)+': '+FormatFloat('0.0',favg)+' sec. (Optimal '+Inttostr(CT_NewLineSecondsAvg)+'s) Deviation '+FormatFloat('0.00%',f*100);
    frmStatistics.BlockLineSeries.Add(favg);
    if REFRESH_CHARTS and (frmStatistics.BlockLineSeries.Count >= MAX_REFRESH_POINTS) then
       frmStatistics.BlockLineSeries.Delete(0);

    {frmStatistics.LineSeries.Add(f*100);
    // if we need to refresh the charts...
    if REFRESH_CHARTS and (frmStatistics.LineSeries.Count >= MAX_REFRESH_POINTS) then
      //frmStatistics.LineSeries.Clear;
       frmStatistics.LineSeries.Delete(0);}

//    if (frmStatistics.LineSeries.Count > 50) then
  //      frmStatistics.LineSeries.Delete(0);
  {if favg>=CT_NewLineSecondsAvg then begin
    lblTimeAverage.Font.Color := clNavy;
  end else begin
    lblTimeAverage.Font.Color := clOlive;
  end;
  lblTimeAverageAux.Caption := Format('Last %d: %s sec. - %d: %s sec. - %d: %s sec. - %d: %s sec. - %d: %s sec.',[
      CT_CalcNewTargetBlocksAverage * 2 ,FormatFloat('0.0',FNode.Bank.GetActualTargetSecondsAverage(CT_CalcNewTargetBlocksAverage * 2)),
      ((CT_CalcNewTargetBlocksAverage * 3) DIV 2) ,FormatFloat('0.0',FNode.Bank.GetActualTargetSecondsAverage((CT_CalcNewTargetBlocksAverage * 3) DIV 2)),
      ((CT_CalcNewTargetBlocksAverage DIV 4)*3),FormatFloat('0.0',FNode.Bank.GetActualTargetSecondsAverage(((CT_CalcNewTargetBlocksAverage DIV 4)*3))),
      CT_CalcNewTargetBlocksAverage DIV 2,FormatFloat('0.0',FNode.Bank.GetActualTargetSecondsAverage(CT_CalcNewTargetBlocksAverage DIV 2)),
      CT_CalcNewTargetBlocksAverage DIV 4,FormatFloat('0.0',FNode.Bank.GetActualTargetSecondsAverage(CT_CalcNewTargetBlocksAverage DIV 4))]);}
end else begin
    isMining := false;
    frmStatistics.lblBlocks.Caption := '';
    frmStatistics.lblAccounts.Caption := '';
    frmStatistics.lblBlockTime.Caption := '';
    frmStatistics.lblPendingOperations.Caption := '';
    frmStatistics.lblMiningTarget.Caption := '';
    {{lblTimeAverage.Caption := '';
    lblTimeAverageAux.Caption := '';}
  end;
  if (Assigned(FPoolMiningServer)) And (FPoolMiningServer.Active) then begin
    If FPoolMiningServer.ClientsCount>0 then begin
      //lblMinersClients.Caption := IntToStr(FPoolMiningServer.ClientsCount)+' connected JSON-RPC clients';
      //lblMinersClients.Font.Color := clNavy;
      frmStatistics.lblMiners.Caption := FloatToStrF(FPoolMiningServer.ClientsCount, ffNumber, 18, 0);
    end else begin
      frmStatistics.lblMiners.Caption := '0';
      //lblMinersClients.Caption := 'No JSON-RPC clients';
      //lblMinersClients.Font.Color := clDkGray;
    end;
    MinersBlocksFound := FPoolMiningServer.ClientsWins;
  end else begin
    MinersBlocksFound := 0;
    //lblMinersClients.Caption := 'JSON-RPC server not active';
    //lblMinersClients.Font.Color := clRed;
    frmStatistics.lblMiners.Caption := 'N/A';
  end;
end;

procedure TFRMWallet.UpdateConfigChanged;
Var wa : Boolean;
  i : Integer;
  LLockedMempool : TABEYOperationsComp;
begin
  tsLogs.TabVisible := FAppParams.ParamByName[CT_PARAM_ShowLogs].GetAsBoolean(false);
  if (Not tsLogs.TabVisible) then begin
    FLog.OnNewLog := Nil;
    if PageControl.ActivePage = tsLogs then PageControl.ActivePage := tsMyAccounts;
  end else FLog.OnNewLog := OnNewLog;
  {!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
   FLog.SaveTypes := CT_TLogTypes_ALL;
   FLog.FileName := TFolderHelper.GetABEYDataFolder+PathDelim+'ABEY-wallet.log';


  {if FAppParams.ParamByName[CT_PARAM_SaveLogFiles].GetAsBoolean(false) then begin
    if FAppParams.ParamByName[CT_PARAM_SaveDebugLogs].GetAsBoolean(false) then FLog.SaveTypes := CT_TLogTypes_ALL
    else FLog.SaveTypes := CT_TLogTypes_DEFAULT;
    FLog.FileName := TFolderHelper.GetABEYDataFolder+PathDelim+'ABEY-wallet.log';
  end else begin
    FLog.SaveTypes := [];
    FLog.FileName := '';
  end;          }
  {!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
  if Assigned(FNode) then begin
    wa := FNode.NetServer.Active;
    FNode.NetServer.Port := FAppParams.ParamByName[CT_PARAM_InternetServerPort].GetAsInteger(CT_NetServer_Port);
    FNode.NetServer.Active := wa;

    LLockedMempool := FNode.LockMempoolWrite;
    try
      LLockedMempool.BlockPayload := TEncoding.ANSI.GetBytes(FAppParams.ParamByName[CT_PARAM_MinerName].GetAsString(''));
    finally
      FNode.UnlockMempoolWrite;
    end;
    FNode.NodeLogFilename := TFolderHelper.GetABEYDataFolder+PathDelim+'ABEY.log';
  end;
  if Assigned(FPoolMiningServer) then begin
    if FPoolMiningServer.Port<>FAppParams.ParamByName[CT_PARAM_JSONRPCMinerServerPort].GetAsInteger(CT_JSONRPCMinerServer_Port) then begin
      FPoolMiningServer.Active := false;
      FPoolMiningServer.Port := FAppParams.ParamByName[CT_PARAM_JSONRPCMinerServerPort].GetAsInteger(CT_JSONRPCMinerServer_Port);
    end;
    FPoolMiningServer.Active :=FAppParams.ParamByName[CT_PARAM_JSONRPCMinerServerActive].GetAsBoolean(true);
    FPoolMiningServer.UpdateAccountAndPayload(GetAccountKeyForMiner,TEncoding.ANSI.GetBytes(FAppParams.ParamByName[CT_PARAM_MinerName].GetAsString('')));
  end;
  if Assigned(FRPCServer) then begin
    FRPCServer.Active := FAppParams.ParamByName[CT_PARAM_JSONRPCEnabled].GetAsBoolean(true);
    FRPCServer.ValidIPs := FAppParams.ParamByName[CT_PARAM_JSONRPCAllowedIPs].GetAsString('127.0.0.1');
  end;
  i := FAppParams.ParamByName[CT_PARAM_MinerPrivateKeyType].GetAsInteger(Integer(mpk_Random));
  if (i>=Integer(Low(TMinerPrivatekey))) And (i<=Integer(High(TMinerPrivatekey))) then FMinerPrivateKeyType := TMinerPrivateKey(i)
  else FMinerPrivateKeyType := mpk_Random;
  ebHashRateBackBlocks.Text := IntToStr(FBlockChainGrid.HashRateAverageBlocksCount);
  Case FBlockChainGrid.HashRateAs of
    hr_Unit : cbHashRateUnits.ItemIndex:=0;
    hr_Kilo : cbHashRateUnits.ItemIndex:=1;
    hr_Mega : cbHashRateUnits.ItemIndex:=2;
    hr_Giga : cbHashRateUnits.ItemIndex:=3;
    hr_Tera : cbHashRateUnits.ItemIndex:=4;
    hr_Peta : cbHashRateUnits.ItemIndex:=5;
    hr_Exa : cbHashRateUnits.ItemIndex:=6;
  else cbHashRateUnits.ItemIndex:=-1;
  end;
  if TNetData.NetDataExists then begin
    if FAppParams.ParamByName[CT_PARAM_AllowDownloadNewCheckpointIfOlderThan].GetAsBoolean(TNetData.NetData.MinFutureBlocksToDownloadNewVault>200) then begin
      TNetData.NetData.MinFutureBlocksToDownloadNewVault:=FAppParams.ParamByName[CT_PARAM_MinFutureBlocksToDownloadNewVault].GetAsInteger(TNetData.NetData.MinFutureBlocksToDownloadNewVault);
    end else TNetData.NetData.MinFutureBlocksToDownloadNewVault:=0;
  end;
end;

procedure TFRMWallet.UpdateConnectionStatus;
var errors : String;
begin
  UpdateNodeStatus;
  OnNetStatisticsChanged(Nil);
  if Assigned(FNode) then begin
    if FNode.IsBlockChainValid(errors) then begin
      {StatusBar.Panels[2].Text := Format('Last account time:%s',
       [FormatDateTime('dd/mm/yyyy hh:nn:ss',UnivDateTime2LocalDateTime(UnixToUnivDateTime( FNode.Bank.LastOperationBlock.timestamp )))]);}
    end else begin
      //StatusBar.Panels[2].Text := 'NO BLOCKCHAIN: '+errors;
        frmNotifications.lblMessages.Caption := 'Blockchain on hold. '+errors;
    end;
  end else begin
    //StatusBar.Panels[2].Text := '';
  end;
end;

procedure TFRMWallet.UpdateNodeStatus;
Var status : String;
begin
  If Not Assigned(FNode) then begin
    //lblNodeStatus.Font.Color := clRed;
    //lblNodeStatus.Caption := 'Initializing...';
    frmStatistics.lblStatus.Font.Color := $000080FF;
    frmStatistics.lblStatus.Caption := 'Starting up...';

  end else begin
    If FNode.IsReady(status) then begin
      if TNetData.NetData.NetStatistics.ActiveConnections>0 then begin
        frmStatistics.lblStatus.Font.Color := $000080FF;
        //lblNodeStatus.Font.Color := clGreen;
        if TNetData.NetData.IsDiscoveringServers then begin
          //lblNodeStatus.Caption := 'Discovering servers';
          frmStatistics.lblStatus.Caption := 'Searching for peers...';
        end else if TNetData.NetData.IsGettingNewBlockChainFromClient(status) then begin
          //lblNodeStatus.Caption := 'Obtaining new blockchain '+status;
          frmStatistics.lblStatus.Font.Color := $000080FF;
          frmStatistics.lblStatus.Caption := 'Initializing new blockchain '+status;
        end else begin
          //lblNodeStatus.Caption := 'Running';
          frmStatistics.lblStatus.Font.Color := $000080FF;
          frmStatistics.lblStatus.Caption := 'Up and executing normally...';
        end;
      end else begin
        //lblNodeStatus.Font.Color := clRed;
//        lblNodeStatus.Caption := 'Alone in the world...';
        frmStatistics.lblStatus.Font.Color := clRed;
        frmStatistics.lblStatus.Caption := 'No peers found!';
      end;
    end else begin
      //lblNodeStatus.Font.Color := clRed;
      //lblNodeStatus.Caption := status;
      frmStatistics.lblStatus.Font.Color := clRed;
      frmStatistics.lblStatus.Caption := Status;
    end;
  end;
  If Assigned(FBackgroundLabel) then begin
    FBackgroundLabel.Font.Color:=frmStatistics.lblStatus.Font.Color;//lblNodeStatus.Font.Color;
    FBackgroundLabel.Caption:='Please wait...';//+frmStatistics.lblStatus.Caption;//lblNodeStatus.Caption;
  end;
end;

procedure TFRMWallet.UpdateOperations;
Var accn : Int64;
begin
  accn := FAccountsGrid.AccountNumber(dgAccounts.Row);
  FOperationsAccountGrid.AccountNumber := accn;
end;

procedure TFRMWallet.UpdatePrivateKeys;
Var i,last_i : Integer;
  wk : TWalletKey;
  s : AnsiString;
begin
  FNodeNotifyEvents.WatchKeys := FWalletKeys.AccountsKeyList;
  if (cbMyPrivateKeys.ItemIndex>=0) then last_i := PtrInt(cbMyPrivateKeys.Items.Objects[cbMyPrivateKeys.ItemIndex])
  else last_i := -1;
  cbMyPrivateKeys.items.BeginUpdate;
  Try
    cbMyPrivateKeys.Items.Clear;
    For i:=0 to FWalletKeys.Count-1 do begin
      wk := FWalletKeys.Key[i];
      if (wk.Name='') then begin
        s := 'Sha256='+TCrypto.ToHexaString( TCrypto.DoSha256( TAccountComp.AccountKey2RawString(wk.AccountKey) ) );
      end else begin
        s := wk.Name;
      end;
      if Not Assigned(wk.PrivateKey) then begin
        if Length(wk.CryptedKey)>0 then s:=s+' (**NEED PASSWORD**)'
        else s:=s+' (**PUBLIC KEY ONLY**)';
      end;
      cbMyPrivateKeys.Items.AddObject(s,TObject(i));
    end;
    cbMyPrivateKeys.Sorted := true;
    cbMyPrivateKeys.Sorted := false;
    cbMyPrivateKeys.Items.InsertObject(0,'(All my private keys)',TObject(-1));
  Finally
    cbMyPrivateKeys.Items.EndUpdate;
  End;
  last_i := cbMyPrivateKeys.Items.IndexOfObject(TObject(last_i));
  if last_i<0 then last_i := 0;
  if cbMyPrivateKeys.Items.Count>last_i then cbMyPrivateKeys.ItemIndex := last_i
  else if cbMyPrivateKeys.Items.Count>=0 then cbMyPrivateKeys.ItemIndex := 0;
end;

procedure TFRMWallet.UpdateDone(Update:String);
var
  URL:String;
begin
  if (Length(Update) < 1) then // error-checking
    Exit;

  //showmessage(update);
  if (Update[1] = '0') then // non-critical update
  begin
    ShowMessage('An update is available for your ABEY wallet!'+#13#10#13#10+'We recommend that you upgrade to the latest version for the latest features.'+#13#10#13#10+'Please visit http://abey.com to download and install the latest version.');
    OpenUrl('http://abey.com/downloads/');
  end
  else
  if (Update[1] = '1') then // critical update
  begin // critical update
    URL := Copy(Update,2,Update.Length-1);
    UpdateTimer.Enabled := True;

    ShowMessage('A critical update has been found for your ABEY wallet!'+#13#10#13#10+'A download will be initiated on your behalf automatically after you close this message.'+#13#10#13#10+'Your wallet will close in 30 seconds, please update to the latest version.');
    OpenUrl('http://abey.com/downloads/');
    OpenUrl(URL);
  end;
end;

procedure TFRMWallet.IdleTimerTimer(Sender: TObject);
begin
  IdleTimer.Enabled := False;
  //BackgroundPanelOnPaint(Self);
  image.Update(fBackgroundPanel.Canvas,r);
  IdleTimer.Enabled := True;
end;

procedure TFRMWallet.imgAcademiaClick(Sender: TObject);
begin

end;

procedure TFRMWallet.FormShow(Sender: TObject);
begin
  frmNotifications.Visible := True;
  frmStatistics.Visible := True;
end;

procedure TFRMWallet.backImgClick(Sender: TObject);
begin

end;

initialization
  FRMWallet := Nil;
end.
