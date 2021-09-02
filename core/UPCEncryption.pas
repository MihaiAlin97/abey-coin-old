unit UPCEncryption;

{
  This unit is used to call Encryption/Decryption routines used by the blockchain
  - ECDSA encryption
  - AES encryption

  It will use OpenSSL library or native CryptoLib4Pascal based on config.inc file
}

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I config.inc}

{$IF (not Defined(Use_OpenSSL)) and (not Defined(Use_CryptoLib4Pascal))}
  {$Message Fatal 'ERROR: Use_OpenSSL or Use_CryptoLib4Pascal are not defined, you need to at least define one!'}
  ERROR: Use_OpenSSL or Use_CryptoLib4Pascal are not defined, you need to at least define one!
{$ENDIF}

Uses SysUtils, UBaseTypes,
  {$IFDEF Use_OpenSSL}
  UAES, UECIES,
  {$ENDIF}
  {$IFDEF Use_CryptoLib4Pascal}
  UPCCryptoLib4Pascal,
  {$ENDIF}
  UPCDataTypes;

type
  TABEYEncryption = Class
  public
    class function DoABEYECIESEncrypt(const APublicKey : TECDSA_Public; const AMessage : TBytes; var AEncryptedMessage : TBytes) : Boolean;
    class function DoABEYECIESDecrypt(const APrivateKeyInfo : TECPrivateKeyInfo; const AEncryptedMessage : TBytes; var ADecryptedMessage : TBytes) : Boolean;
    class function DoABEYAESEncrypt(const AMessage, APassword: TBytes): TBytes; static;
    class function DoABEYAESDecrypt(const AEncryptedMessage, APassword: TBytes; out ADecryptedMessage: TBytes): boolean; static;
  End;

implementation


{ TABEYEncryption }

class function TABEYEncryption.DoABEYAESDecrypt(const AEncryptedMessage, APassword: TBytes; out ADecryptedMessage: TBytes): boolean;
begin
  {$IFDEF Use_OpenSSL}
  Result := TAESComp.EVP_Decrypt_AES256(AEncryptedMessage,APassword,ADecryptedMessage);
  {$ELSE}
  Result := TABEYCryptoLib4Pascal.DoABEYAESDecrypt(AEncryptedMessage,APassword,ADecryptedMessage);
  {$ENDIF}
end;

class function TABEYEncryption.DoABEYAESEncrypt(const AMessage, APassword: TBytes): TBytes;
begin
  {$IFDEF Use_OpenSSL}
  Result := TAESComp.EVP_Encrypt_AES256(AMessage,APassword);
  {$ELSE}
  Result := TABEYCryptoLib4Pascal.DoABEYAESEncrypt(AMessage,APassword);
  {$ENDIF}
end;

class function TABEYEncryption.DoABEYECIESDecrypt(const APrivateKeyInfo : TECPrivateKeyInfo; const AEncryptedMessage: TBytes; var ADecryptedMessage: TBytes): Boolean;
begin
  {$IFDEF Use_OpenSSL}
  Result := ECIESDecrypt(APrivateKeyInfo.EC_OpenSSL_NID,APrivateKeyInfo.EC_KEY_Ptr,False,AEncryptedMessage,ADecryptedMessage);
  {$ELSE}
  Result := TABEYCryptoLib4Pascal.DoABEYECIESDecrypt(APrivateKeyInfo.EC_OpenSSL_NID,APrivateKeyInfo.RAW_PrivKey,AEncryptedMessage,ADecryptedMessage);
  {$ENDIF}
end;

class function TABEYEncryption.DoABEYECIESEncrypt(const APublicKey: TECDSA_Public; const AMessage: TBytes; var AEncryptedMessage: TBytes): Boolean;
begin
  {$IFDEF Use_OpenSSL}
  AEncryptedMessage := ECIESEncrypt(APublicKey,AMessage);
  Result := True;
  {$ELSE}
  Result := TABEYCryptoLib4Pascal.DoABEYECIESEncrypt(APublicKey,AMessage,AEncryptedMessage);
  {$ENDIF}
end;

end.
