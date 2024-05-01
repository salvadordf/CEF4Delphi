unit uCEFUnresponsiveProcessCallback;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes, System.SysUtils,
  {$ELSE}
  Classes, SysUtils,
  {$ENDIF}
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefUnresponsiveProcessCallbackRef = class(TCefBaseRefCountedRef, ICefUnresponsiveProcessCallback)
  protected
    procedure Wait;
    procedure Terminate;
  public
    class function UnWrap(data: Pointer): ICefUnresponsiveProcessCallback;
  end;

implementation

procedure TCefUnresponsiveProcessCallbackRef.Wait;
begin
  PCefUnresponsiveProcessCallback(FData)^.wait(PCefUnresponsiveProcessCallback(FData));
end;

procedure TCefUnresponsiveProcessCallbackRef.Terminate;
begin
  PCefUnresponsiveProcessCallback(FData)^.terminate(PCefUnresponsiveProcessCallback(FData));
end;

class function TCefUnresponsiveProcessCallbackRef.UnWrap(data: Pointer): ICefUnresponsiveProcessCallback;
begin
  if (data <> nil) then
    Result := Create(data) as ICefUnresponsiveProcessCallback
   else
    Result := nil;
end;

end.
