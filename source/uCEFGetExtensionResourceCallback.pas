unit uCEFGetExtensionResourceCallback;

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
  TCefGetExtensionResourceCallbackRef = class(TCefBaseRefCountedRef, ICefGetExtensionResourceCallback)
    protected
      procedure Cont(const stream: ICefStreamReader);
      procedure Cancel;

    public
      class function UnWrap(data: Pointer): ICefGetExtensionResourceCallback;
  end;

implementation

uses
  uCEFMiscFunctions;

procedure TCefGetExtensionResourceCallbackRef.Cont(const stream: ICefStreamReader);
begin
  PCefGetExtensionResourceCallback(FData)^.cont(FData, CefGetData(stream));
end;

procedure TCefGetExtensionResourceCallbackRef.Cancel;
begin
  PCefGetExtensionResourceCallback(FData)^.cancel(FData);
end;

class function TCefGetExtensionResourceCallbackRef.UnWrap(data: Pointer): ICefGetExtensionResourceCallback;
begin
  if (data <> nil) then
    Result := Create(data) as ICefGetExtensionResourceCallback
   else
    Result := nil;
end;

end.
