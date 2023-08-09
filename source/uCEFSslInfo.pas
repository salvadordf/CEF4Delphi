unit uCEFSslInfo;

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
  TCefSslInfoRef = class(TCefBaseRefCountedRef, ICefSslInfo)
    protected
      function GetCertStatus: TCefCertStatus;
      function GetX509Certificate: ICefX509Certificate;

    public
      class function UnWrap(data: Pointer): ICefSslInfo;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBinaryValue, uCEFX509Certificate;


function TCefSslInfoRef.GetCertStatus: TCefCertStatus;
begin
  Result := PCefSslInfo(FData)^.get_cert_status(PCefSslInfo(FData));
end;

function TCefSslInfoRef.GetX509Certificate: ICefX509Certificate;
begin
  Result := TCEFX509CertificateRef.UnWrap(PCefSslInfo(FData)^.get_x509certificate(PCefSslInfo(FData)));
end;

class function TCefSslInfoRef.UnWrap(data: Pointer): ICefSslInfo;
begin
  if (data <> nil) then
    Result := Create(data) as ICefSslInfo
   else
    Result := nil;
end;

end.
