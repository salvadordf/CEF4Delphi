unit uCEFJsDialogCallback;

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
  TCefJsDialogCallbackRef = class(TCefBaseRefCountedRef, ICefJsDialogCallback)
    protected
      procedure Cont(success: Boolean; const userInput: ustring);
    public
      class function UnWrap(data: Pointer): ICefJsDialogCallback;
  end;


implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;

procedure TCefJsDialogCallbackRef.Cont(success: Boolean; const userInput: ustring);
var
  TempInput : TCefString;
begin
  TempInput := CefString(userInput);
  PCefJsDialogCallback(FData)^.cont(PCefJsDialogCallback(FData), Ord(success), @TempInput);
end;

class function TCefJsDialogCallbackRef.UnWrap(data: Pointer): ICefJsDialogCallback;
begin
  if (data <> nil) then
    Result := Create(data) as ICefJsDialogCallback
   else
    Result := nil;
end;

end.
