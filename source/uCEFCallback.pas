unit uCEFCallback;

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
  TCefCallbackRef = class(TCefBaseRefCountedRef, ICefCallback)
    protected
      procedure Cont;
      procedure Cancel;

    public
      class function UnWrap(data: Pointer): ICefCallback;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;

procedure TCefCallbackRef.Cancel;
begin
  PCefCallback(FData)^.cancel(PCefCallback(FData));
end;

procedure TCefCallbackRef.Cont;
begin
  PCefCallback(FData)^.cont(PCefCallback(FData));
end;

class function TCefCallbackRef.UnWrap(data: Pointer): ICefCallback;
begin
  if (data <> nil) then
    Result := Create(data) as ICefCallback
   else
    Result := nil;
end;

end.
