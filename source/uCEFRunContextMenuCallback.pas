unit uCEFRunContextMenuCallback;

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
  TCefRunContextMenuCallbackRef = class(TCefBaseRefCountedRef, ICefRunContextMenuCallback)
  protected
    procedure Cont(commandId: Integer; eventFlags: TCefEventFlags);
    procedure Cancel;
  public
    class function UnWrap(data: Pointer): ICefRunContextMenuCallback;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;

procedure TCefRunContextMenuCallbackRef.Cancel;
begin
  PCefRunContextMenuCallback(FData)^.cancel(PCefRunContextMenuCallback(FData));
end;

procedure TCefRunContextMenuCallbackRef.Cont(commandId: Integer; eventFlags: TCefEventFlags);
begin
  PCefRunContextMenuCallback(FData)^.cont(PCefRunContextMenuCallback(FData), commandId, eventFlags);
end;

class function TCefRunContextMenuCallbackRef.UnWrap(data: Pointer): ICefRunContextMenuCallback;
begin
  if (data <> nil) then
    Result := Create(data) as ICefRunContextMenuCallback
   else
    Result := nil;
end;

end.
