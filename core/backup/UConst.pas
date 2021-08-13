unit UConst;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

{$OVERFLOWCHECKS ON}  //same as $Q+
{$RANGECHECKS ON}

{$I config.inc}

{$IFNDEF FPC}
  // See http://wiki.freepascal.org/Code_Conversion_Guide
type
  PtrInt = NativeInt;
  PtrUInt = NativeUInt;
  TThreadID = NativeUInt;
{$ENDIF}

Const
  CT_Genesis_Magic_String_For_Old_Block_Hash :
    String =
    'ABEY';

  { Account types }
  ACCOUNT_USER = 0;
  ACCOUNT_MERCHANT = 1;
  ACCOUNT_NONPROFIT = 2;
  ACCOUNT_MINER = 3;
  ACCOUNT_MININGPOOL = 4;
  ACCOUNT_ACADEMIA = 5;
  ACCOUNT_EXCHANGE = 6;

  { Update mechanism }
  VERSION_DAEMON_MAJOR = 1;
  VERSION_DAEMON_MINOR = 0;
  VERSION_DAEMON_BUILD = 1;

  VERSION_WALLET_MAJOR = 1; // major release
  VERSION_WALLET_MINOR = 2020; // year
  VERSION_WALLET_BUILD = 10622; // 1 MM DD

  VERSION_MINER_MAJOR = 1;
  VERSION_MINER_MINOR = 0;
  VERSION_MINER_BUILD = 1;

  UPDATE_INTERVAL = 15*60*1000;
  TIMEOUT_TO_DIE = 30;

  { }
  CT_Zero_Block_Proof_of_work_in_Hexa =
    {$IFDEF PRODUCTION}''{$ELSE}{$IFDEF TESTNET}''{$ELSE}{$ENDIF}{$ENDIF};


  CT_NetServer_Port = {$IFDEF PRODUCTION}12259{$ELSE}{$IFDEF TESTNET}5004{$ELSE}{$ENDIF}{$ENDIF};
  CT_JSONRPCMinerServer_Port = {$IFDEF PRODUCTION}12260{$ELSE}{$IFDEF TESTNET}5009{$ELSE}{$ENDIF}{$ENDIF};
  CT_JSONRPC_Port = {$IFDEF PRODUCTION}12261{$ELSE}{$IFDEF TESTNET}5003{$ELSE}{$ENDIF}{$ENDIF};
  CT_AccountsPerBlock = 25;
  CT_NewLineSecondsAvg: Cardinal = {$IFDEF PRODUCTION}120{$ELSE}{$IFDEF TESTNET}30{$ELSE}{$ENDIF}{$ENDIF};
  // 60*2=120seconds -> 2 minutes avg instead of 10
  //   -> 1 day = 86400 seconds -> 1 year = 31536000 seconds (aprox)
  //   Each year = 262,800 new blocks (aprox)
  //   -> *25 accounts per block = 6,570,000 new accounts each year (aprox)
  CT_RandomHelloRefactoring: Cardinal = 120;
  CT_TimestampGenesis : Cardinal = 1540857600;

  CT_NumberOfKeys : Cardinal = 150;
  CT_FirstReward: UInt64 = 1000*10000; // 4 decimals... First reward = 1.000,0000
  CT_Milestone1BlockNumber: UInt64 = 100; // 131,040 blocks (182 days)
  CT_Milestone1Reward: UInt64 = 3600*10000; // 4 decimals... reward = 3.600,0000
  CT_Milestone2TimestampBlockNumber : UInt64 = 150; // 427,680 blocks
  CT_MinReward: UInt64 = 10000; // 4 decimals... Min reward = 1,0000
  CT_NewLineRewardDecrease: Cardinal = {$IFDEF PRODUCTION}262800{$ELSE}{$IFDEF TESTNET}20000{$ENDIF}{$ENDIF};

  CT_WaitNewBlocksBeforeTransaction = 100;

  CT_RecoverFoundsWaitInactiveCount = 420480;  // After 4 years... if an account has no operations, money will be a reward for a miner!
  CT_MaxFutureBlocksLockedAccount = 105120; // Maximum future blocks an account can be locked

  CT_MaxTransactionAmount = 1000000000000; // ... can be deleted
  CT_MaxTransactionFee = 100000000;
  CT_MaxWalletAmount = 10000000000000; // ... can be deleted
  //
  CT_MinCompactTarget_v1: Cardinal = {$IFDEF PRODUCTION}$11000000{$ELSE}{$IFDEF TESTNET}$08000000{$ELSE}{$ENDIF}{$ENDIF}; // First compact target of block 0
  CT_MinCompactTarget_v4: Cardinal = // Minimum compact target of block if using Protocol 4 or higher
    {$IFDEF ACTIVATE_RANDOMHASH_V4}
    {$IFDEF PRODUCTION}$11000000{$ELSE}{$IFDEF TESTNET}$08000000{$ELSE}{$ENDIF}{$ENDIF}
    {$ELSE}CT_MinCompactTarget_v1{$ENDIF};

  {$IFDEF ACTIVATE_RANDOMHASH_V4}
  CT_CompactTarget_Reset_v4: Cardinal = // First compact target of block if using Protocol 4 and RandomHash is Active
    {$IFDEF PRODUCTION}$1100000{$ELSE}$11000000{$ENDIF};
  {$ENDIF}
  CT_GenesisTarget = ('1229AD5D1224ED2C122020B2121B47D0121662651211705112007173120765AA1'+
                      '2024CD511FE936811FBF9BE11F9595B11F6B22D11F4042311F14F2B11EE933311'+
                      'EBD02911E905FA11E6349511E35BE611E07BDB11E0946111DAA56411D7AED111D'+
                      '4B09511D1AA9B110E9CD011CB871F11C86974110543BA11C215DD11BEDFC711BB'+
                      'A16411B85A9D11B50B5D11B1B38F11AE531C11AAE9ED11A777EC11A3FD0311A07'+
                      '91A119CEC1A119955E81195867611920DA3118E5859118A9F801186DA0011830A'+
                      'BF117F31A411784E961177617B11736A39116F68B5116B5CD51167467F1166404'+
                      '11165395B116431CC11632994116220B31161172811600CF2115F0212115DF686'+
                      '115CEA4F115BDD6C115ACFDC1159C19F115882851157A31D115692D7115581E21'+
                      '154703E11535DEA11524AE611513732115022CD114F0DB7114DF7EF114CE17511'+
                      '4BCA48114AB268114999D41148808C1147669011464BDF114530781144145C114'+
                      '2F7891141DA001140BBBF113F9CC7113E7D17113D5CAE113C3B8C113B19B01139'+
                      'F71A1138D3CA1137AFBF11368AF91135657711343F391133183E1131F08611300'+
                      '810112F9EDC112E74E9112D4A37112C1EC5112AF2931129C5A0112897EC112769'+
                      '7711263A4011250A461123D9891122A809112175C5112042BC111F0EEE111DDA5'+
                      'B111CA502111B6EE2111A37FB1119004D1117C7D711168E9811155490111419BF'+
                      '1112DE241111A1BE1110648D110F2691110DE7C9110CA834110B67D2110A26A31'+
                      '108E4A51107A1D911065E3E110519D31103D49811028E8C110147AF');
  CT_GenesisCount = 145;
  CT_CalcNewTargetBlocksAverage: Cardinal = 100;
  CT_CalcNewTargetLimitChange_SPLIT = 10;

  CT_MaxAccount : Cardinal = $FFFFFFFF;
  CT_MaxBlock : Cardinal = $FFFFFFFF;

  CT_MaxPayloadSize = 255; // Max payload size in bytes
  CT_MaxFilePayloadSize = 18446744073709551615;   // Max file payload in bytes

  CT_MaxFutureBlockTimestampOffset = 15;
  CT_MinNodesToCalcNAT = 4;

  CT_MinServersConnected = 2;
  CT_MaxServersConnected = 5;

  CT_MaxResendMemPoolOperations = 50000;

  CT_MaxClientsConnected = {$IFDEF FPC}140{$ELSE}80{$ENDIF};

  //50 blocks Vault size as per ABEY Whitepaper
  CT_BankToDiskEveryNBlocks = {$IFDEF PRODUCTION}50{$ELSE}50{$ENDIF}; // Vaults number of blocks;

  CT_NID_secp256k1 = 714;
  CT_NID_secp384r1 = 715;
  CT_NID_sect283k1 = 729;
  CT_NID_secp521r1 = 716;

  CT_Default_EC_OpenSSL_NID = CT_NID_secp256k1;

  CT_AccountInfo_ForSale = 1000;

  CT_PROTOCOL_1 = 1;
  CT_PROTOCOL_2 = 2;
  CT_PROTOCOL_3 = 3;
  CT_PROTOCOL_4 = 4;
  CT_BUILD_PROTOCOL = CT_PROTOCOL_4;

  CT_BlockChain_Protocol_Available: Word = 4; // Protocol 4 flag
  CT_Protocol_Upgrade_v2_MinBlock = {$IFDEF PRODUCTION}10{$ELSE}3{$ENDIF};
  CT_Protocol_Upgrade_v3_MinBlock = {$IFDEF PRODUCTION}20{$ELSE}6{$ENDIF};
  CT_Protocol_Upgrade_v4_MinBlock = {$IFDEF PRODUCTION}30{$ELSE}9{$ENDIF};


  CT_MagicNetIdentification = {$IFDEF PRODUCTION}$5BD79F00{$ELSE}$04000000{$ENDIF}; // Unix timestamp 1540857600 ...

  CT_NetProtocol_Version: Word = $0009; // Version 4.0.2 Will allow only net protocol 9
  // IMPORTANT NOTE!!!
  // NetProtocol_Available MUST BE always >= NetProtocol_version
  CT_NetProtocol_Available: Word = {$IFDEF PRODUCTION}$0009{$ELSE}$0009{$ENDIF};  // Version 4.0.0 will start accepting protocol 8 but 4.0.1 will accept 9 due to 4.0.0 bug

  CT_MaxAccountOperationsPerBlockWithoutFee = 1;

  CT_VaultBankVersion : Word = 3; // Protocol 2 upgraded Vault version from 2 to 3

  CT_MagicIdentificator: String = {$IFDEF PRODUCTION}'ABEY'{$ELSE}'ABEYTestnet'{$ENDIF}; //

  CT_PseudoOp_Reward = $0;
  // Value of Operations type in Protocol 1
  CT_Op_Transaction = $01;
  CT_Op_Changekey = $02;
  CT_Op_Recover = $03;
  // Protocol 2 new operations
  CT_Op_ListAccountForSale = $04;
  CT_Op_DelistAccount = $05;
  CT_Op_BuyAccount = $06;
  CT_Op_ChangeKeySigned = $07;
  CT_Op_ChangeAccountInfo = $08;
  // Protocol 3 new operations
  CT_Op_MultiOperation = $09;  // PIP-0017
  // Protocol 4 new operations
  CT_Op_Data = $0A;            // PIP-0016
  CT_Op_SaveFiles = $0C;

  //changed from 20% to 0%, for Premined Wallet Balances
  CT_Protocol_v3_PIP11_Percent = 20; // PIP-0011 20% Percent proposed and voted by PIP-0011

  CT_Hardcoded_RandomHash_Table_Filename = 'HardcodedRH_75800.randomhash';
  CT_Hardcoded_RandomHash_Table_HASH = '0A56291E8368AC855227B67A2F6CBEEFD2DB4CAE3CB8A473A7F6663C63368D0E';

  CT_PseudoOpSubtype_Miner                = 1;
  CT_PseudoOpSubtype_Developer            = 2;

  CT_OpSubtype_TransactionSender          = 11;
  CT_OpSubtype_TransactionReceiver        = 12;
  CT_OpSubtype_BuyTransactionBuyer        = 13;
  CT_OpSubtype_BuyTransactionTarget       = 14;
  CT_OpSubtype_BuyTransactionSeller       = 15;
  CT_OpSubtype_ChangeKey                  = 21;
  CT_OpSubtype_Recover                    = 31;
  CT_OpSubtype_ListAccountForPublicSale   = 41;
  CT_OpSubtype_ListAccountForPrivateSale  = 42;
  CT_OpSubtype_DelistAccount              = 51;
  CT_OpSubtype_BuyAccountBuyer            = 61;
  CT_OpSubtype_BuyAccountTarget           = 62;
  CT_OpSubtype_BuyAccountSeller           = 63;
  CT_OpSubtype_ChangeKeySigned            = 71;
  CT_OpSubtype_ChangeAccountInfo          = 81;
  CT_OpSubtype_MultiOperation_Global      = 91;
  CT_OpSubtype_MultiOperation_AccountInfo = 92;
  CT_OpSubtype_Data_GlobalInfo            = 101;
  CT_OpSubtype_Data_Sender                = 102;
  CT_OpSubtype_Data_Signer                = 103;
  CT_OpSubtype_Data_Receiver              = 104;

  CT_ClientAppVersion : String = {$IFDEF PRODUCTION}'ABEY 1.2020.0428'{$ELSE}{$IFDEF TESTNET}'TESTNET 4.1'{$ELSE}{$ENDIF}{$ENDIF};

  CT_Discover_IPs = {$IFDEF PRODUCTION} '10.0.2.15' //} 'n1.eu.abey.org,n2.us.abey.org'
                    {$ELSE}'10.0.2.15:5104'{$ENDIF};

  CT_TRUE_FALSE : Array[Boolean] Of String = ('FALSE','TRUE');

  CT_MAX_0_fee_operations_per_block_by_miner = {$IFDEF PRODUCTION}10000{$ELSE}{$IFDEF TESTNET}3000{$ELSE}{$ENDIF}{$ENDIF};
  CT_MAX_Operations_per_block_by_miner =  {$IFDEF PRODUCTION}100000{$ELSE}{$IFDEF TESTNET}50000{$ELSE}{$ENDIF}{$ENDIF};

  CT_MAX_MultiOperation_Senders = 100;
  CT_MAX_MultiOperation_Receivers = 1000;
  CT_MAX_MultiOperation_Changers = 100;

  CT_DEFAULT_MaxVaultSnapshots = 10;

  CT_MOLINA  = 1;
  CT_MOLINA_DECIMAL = {$IFDEF FPC}Real(CT_MOLINA/1000.0);{$ELSE}0.0001;{$ENDIF}

  CT_ACTIVATE_RANDOMHASH_V4 = {$IFDEF ACTIVATE_RANDOMHASH_V4}False{$ELSE}False{$ENDIF};

  // App Params
  CT_PARAM_GridAccountsStream = 'GridAccountsStreamV2';
  CT_PARAM_GridAccountsPos = 'GridAccountsPos';
  CT_PARAM_DefaultFee = 'DefaultFee';
  CT_PARAM_InternetServerPort = 'InternetServerPort';
  {$IFDEF TESTNET}CT_PARAM_AutomaticMineWhenConnectedToNodes = 'AutomaticMineWhenConnectedToNodes';{$ENDIF}
  CT_PARAM_MinerPrivateKeyType = 'MinerPrivateKeyType';
  CT_PARAM_MinerPrivateKeySelectedPublicKey = 'MinerPrivateKeySelectedPublicKey';
  CT_PARAM_SaveLogFiles = 'SaveLogFiles';
  CT_PARAM_SaveDebugLogs = 'SaveDebugLogs';
  CT_PARAM_ShowLogs = 'ShowLogs';
  CT_PARAM_MinerName = 'MinerName';
  CT_PARAM_FirstTime = 'FirstTime';
  CT_PARAM_ShowModalMessages = 'ShowModalMessages';
  {$IFDEF TESTNET}CT_PARAM_MaxCPUs = 'MaxCPUs'; {$ENDIF} //deprecated
  CT_PARAM_PeerCache = 'PeerCache';
  CT_PARAM_TryToConnectOnlyWithThisFixedServers = 'TryToConnectOnlyWithFixedServers';
  CT_PARAM_JSONRPCMinerServerPort = 'JSONRPCMinerServerPort';
  CT_PARAM_JSONRPCMinerServerActive = 'JSONRPCMinerServerActive';
  CT_PARAM_JSONRPCEnabled = 'JSONRPCEnabled';
  CT_PARAM_JSONRPCAllowedIPs = 'JSONRPCAllowedIPs';



implementation

end.
