unit uCEFX509Certificate;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
    {$IFDEF MSWINDOWS}WinApi.Windows,{$ENDIF} System.Classes, System.SysUtils,
  {$ELSE}
    {$IFDEF MSWINDOWS}Windows,{$ENDIF} Classes, SysUtils,
  {$ENDIF}
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCEFX509CertificateRef = class(TCefBaseRefCountedRef, ICefX509Certificate)
    protected
      function  GetSubject: ICefX509CertPrincipal;
      function  GetIssuer: ICefX509CertPrincipal;
      function  GetSerialNumber: ICefBinaryValue;
      function  GetValidStart: TCefBaseTime;
      function  GetValidExpiry: TCefBaseTime;
      function  GetValidStartAsDateTime: TDateTime;
      function  GetValidExpiryAsDateTime: TDateTime;
      function  GetDerEncoded: ICefBinaryValue;
      function  GetPemEncoded: ICefBinaryValue;
      function  GetIssuerChainSize: NativeUInt;
      procedure GetDEREncodedIssuerChain(chainCount: NativeUInt; var chain : TCefBinaryValueArray);
      procedure GetPEMEncodedIssuerChain(chainCount: NativeUInt; var chain : TCefBinaryValueArray);

    public
      class function UnWrap(data: Pointer): ICefX509Certificate;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBinaryValue, uCEFX509CertPrincipal;

function TCEFX509CertificateRef.GetSubject: ICefX509CertPrincipal;
begin
  Result := TCefX509CertPrincipalRef.UnWrap(PCefX509Certificate(FData)^.get_subject(PCefX509Certificate(FData)));
end;

function TCEFX509CertificateRef.GetIssuer: ICefX509CertPrincipal;
begin
  Result := TCefX509CertPrincipalRef.UnWrap(PCefX509Certificate(FData)^.get_issuer(PCefX509Certificate(FData)));
end;

function TCEFX509CertificateRef.GetSerialNumber: ICefBinaryValue;
begin
  Result := TCefBinaryValueRef.UnWrap(PCefX509Certificate(FData)^.get_serial_number(PCefX509Certificate(FData)));
end;

function TCEFX509CertificateRef.GetValidStart: TCefBaseTime;
begin
  Result := PCefX509Certificate(FData)^.get_valid_start(PCefX509Certificate(FData));
end;

function TCEFX509CertificateRef.GetValidExpiry: TCefBaseTime;
begin
  Result := PCefX509Certificate(FData)^.get_valid_expiry(PCefX509Certificate(FData));
end;

function TCEFX509CertificateRef.GetValidStartAsDateTime: TDateTime;
begin
  Result := CefBaseTimeToDateTime(GetValidStart);
end;

function TCEFX509CertificateRef.GetValidExpiryAsDateTime: TDateTime;
begin
  Result := CefBaseTimeToDateTime(GetValidExpiry);
end;

function TCEFX509CertificateRef.GetDerEncoded: ICefBinaryValue;
begin
  Result := TCefBinaryValueRef.UnWrap(PCefX509Certificate(FData)^.get_derencoded(PCefX509Certificate(FData)));
end;

function TCEFX509CertificateRef.GetPemEncoded: ICefBinaryValue;
begin
  Result := TCefBinaryValueRef.UnWrap(PCefX509Certificate(FData)^.get_pemencoded(PCefX509Certificate(FData)));
end;

function TCEFX509CertificateRef.GetIssuerChainSize: NativeUInt;
begin
  Result := PCefX509Certificate(FData)^.get_issuer_chain_size(PCefX509Certificate(FData));
end;

procedure TCEFX509CertificateRef.GetDEREncodedIssuerChain(chainCount: NativeUInt; var chain : TCefBinaryValueArray);
var
  TempArray : array of PCefBinaryValue;
  i : NativeUInt;
begin
  TempArray := nil;

  try
    try
      if (chainCount > 0) then
        begin
          SetLength(TempArray, chainCount);

          i := 0;
          while (i < chainCount) do
            begin
              TempArray[i] := nil;
              inc(i);
            end;

          PCefX509Certificate(FData)^.get_derencoded_issuer_chain(PCefX509Certificate(FData), chainCount, TempArray[0]);

          if (chainCount > 0) then
            begin
              SetLength(chain, chainCount);

              i := 0;
              while (i < chainCount) do
                begin
                  chain[i] := TCefBinaryValueRef.UnWrap(TempArray[i]);
                  inc(i);
                end;
            end;
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TCEFX509CertificateRef.GetDEREncodedIssuerChain', e) then raise;
    end;
  finally
    if (TempArray <> nil) then
      begin
        Finalize(TempArray);
        TempArray := nil;
      end;
  end;
end;

procedure TCEFX509CertificateRef.GetPEMEncodedIssuerChain(chainCount: NativeUInt; var chain : TCefBinaryValueArray);
var
  TempArray : array of PCefBinaryValue;
  i : NativeUInt;
begin
  TempArray := nil;

  try
    try
      if (chainCount > 0) then
        begin
          SetLength(TempArray, chainCount);

          i := 0;
          while (i < chainCount) do
            begin
              TempArray[i] := nil;
              inc(i);
            end;

          PCefX509Certificate(FData)^.get_pemencoded_issuer_chain(PCefX509Certificate(FData), chainCount, TempArray[0]);

          if (chainCount > 0) then
            begin
              SetLength(chain, chainCount);

              i := 0;
              while (i < chainCount) do
                begin
                  chain[i] := TCefBinaryValueRef.UnWrap(TempArray[i]);
                  inc(i);
                end;
            end;
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TCEFX509CertificateRef.GetPEMEncodedIssuerChain', e) then raise;
    end;
  finally
    if (TempArray <> nil) then
      begin
        Finalize(TempArray);
        TempArray := nil;
      end;
  end;
end;

class function TCEFX509CertificateRef.UnWrap(data: Pointer): ICefX509Certificate;
begin
  if (data <> nil) then
    Result := Create(data) as ICefX509Certificate
   else
    Result := nil;
end;

end.
