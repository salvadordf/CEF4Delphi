unit uCEFAuthCallback;

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
  TCefAuthCallbackRef = class(TCefBaseRefCountedRef, ICefAuthCallback)
  protected
    procedure Cont(const username, password: ustring);
    procedure Cancel;

  public
    class function UnWrap(data: Pointer): ICefAuthCallback;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;

procedure TCefAuthCallbackRef.Cancel;
begin
  PCefAuthCallback(FData)^.cancel(PCefAuthCallback(FData));
end;

procedure TCefAuthCallbackRef.Cont(const username, password: ustring);
var
  TempUsername, TempPassword : TCefString;
begin
  TempUsername := CefString(username);
  TempPassword := CefString(password);
  PCefAuthCallback(FData)^.cont(PCefAuthCallback(FData), @TempUsername, @TempPassword);
end;

class function TCefAuthCallbackRef.UnWrap(data: Pointer): ICefAuthCallback;
begin
  if (data <> nil) then
    Result := Create(data) as ICefAuthCallback
   else
    Result := nil;
end;

end.
