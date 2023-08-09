unit uCEFSelectClientCertificateCallback;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefSelectClientCertificateCallbackRef = class(TCefBaseRefCountedRef, ICefSelectClientCertificateCallback)
    protected
      procedure Select(const cert: ICefX509Certificate);

    public
      class function UnWrap(data: Pointer): ICefSelectClientCertificateCallback;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFX509Certificate;

// TCefSelectClientCertificateCallbackRef

procedure TCefSelectClientCertificateCallbackRef.Select(const cert: ICefX509Certificate);
begin
  PCefSelectClientCertificateCallback(FData)^.select(PCefSelectClientCertificateCallback(FData), CefGetData(cert));
end;

class function TCefSelectClientCertificateCallbackRef.UnWrap(data: Pointer): ICefSelectClientCertificateCallback;
begin
  if (data <> nil) then
    Result := Create(data) as ICefSelectClientCertificateCallback
   else
    Result := nil;
end;

end.
