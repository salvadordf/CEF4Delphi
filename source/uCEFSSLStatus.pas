unit uCEFSSLStatus;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes,
  {$ELSE}
  Classes,
  {$ENDIF}
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefSSLStatusRef = class(TCefBaseRefCountedRef, ICefSSLStatus)
    protected
      function IsSecureConnection: boolean;
      function GetCertStatus: TCefCertStatus;
      function GetSSLVersion: TCefSSLVersion;
      function GetContentStatus: TCefSSLContentStatus;
      function GetX509Certificate: ICefX509Certificate;

    public
      class function UnWrap(data: Pointer): ICefSSLStatus;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBinaryValue, uCEFX509Certificate;

function TCefSSLStatusRef.IsSecureConnection: boolean;
begin
  Result := (PCefSSLStatus(FData)^.is_secure_connection(PCefSSLStatus(FData)) <> 0);
end;

function TCefSSLStatusRef.GetCertStatus: TCefCertStatus;
begin
  Result := PCefSSLStatus(FData)^.get_cert_status(PCefSSLStatus(FData));
end;

function TCefSSLStatusRef.GetSSLVersion: TCefSSLVersion;
begin
  Result := PCefSSLStatus(FData)^.get_sslversion(PCefSSLStatus(FData));
end;

function TCefSSLStatusRef.GetContentStatus: TCefSSLContentStatus;
begin
  Result := PCefSSLStatus(FData)^.get_content_status(PCefSSLStatus(FData));
end;

function TCefSSLStatusRef.GetX509Certificate: ICefX509Certificate;
begin
  Result := TCEFX509CertificateRef.UnWrap(PCefSSLStatus(FData)^.get_x509certificate(PCefSSLStatus(FData)));
end;

class function TCefSSLStatusRef.UnWrap(data: Pointer): ICefSSLStatus;
begin
  if (data <> nil) then
    Result := Create(data) as ICefSSLStatus
   else
    Result := nil;
end;

end.
