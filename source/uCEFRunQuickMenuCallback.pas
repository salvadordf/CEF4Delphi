unit uCEFRunQuickMenuCallback;

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
  TCefRunQuickMenuCallbackRef = class(TCefBaseRefCountedRef, ICefRunQuickMenuCallback)
  protected
    procedure Cont(command_id: Integer; event_flags: TCefEventFlags);
    procedure Cancel;
  public
    class function UnWrap(data: Pointer): ICefRunQuickMenuCallback;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;

procedure TCefRunQuickMenuCallbackRef.Cont(command_id: Integer; event_flags: TCefEventFlags);
begin
  PCefRunQuickMenuCallback(FData)^.cont(PCefRunQuickMenuCallback(FData), command_id, event_flags);
end;

procedure TCefRunQuickMenuCallbackRef.Cancel;
begin
  PCefRunQuickMenuCallback(FData)^.cancel(PCefRunQuickMenuCallback(FData));
end;

class function TCefRunQuickMenuCallbackRef.UnWrap(data: Pointer): ICefRunQuickMenuCallback;
begin
  if (data <> nil) then
    Result := Create(data) as ICefRunQuickMenuCallback
   else
    Result := nil;
end;

end.
