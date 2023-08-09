unit uCEFPrintDialogCallback;

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
  TCefPrintDialogCallbackRef = class(TCefBaseRefCountedRef, ICefPrintDialogCallback)
    protected
      procedure cont(const settings: ICefPrintSettings);
      procedure cancel;

    public
      class function UnWrap(data: Pointer): ICefPrintDialogCallback;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;

procedure TCefPrintDialogCallbackRef.cancel;
begin
  PCefPrintDialogCallback(FData)^.cancel(PCefPrintDialogCallback(FData));
end;

procedure TCefPrintDialogCallbackRef.cont(const settings: ICefPrintSettings);
begin
  PCefPrintDialogCallback(FData)^.cont(PCefPrintDialogCallback(FData), CefGetData(settings));
end;

class function TCefPrintDialogCallbackRef.UnWrap(data: Pointer): ICefPrintDialogCallback;
begin
  if (data <> nil) then
    Result := Create(data) as ICefPrintDialogCallback
   else
    Result := nil;
end;

end.
