unit uCEFPrintJobCallback;

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
  TCefPrintJobCallbackRef = class(TCefBaseRefCountedRef, ICefPrintJobCallback)
    protected
      procedure cont;

    public
      class function UnWrap(data: Pointer): ICefPrintJobCallback;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;

procedure TCefPrintJobCallbackRef.cont;
begin
  PCefPrintJobCallback(FData)^.cont(PCefPrintJobCallback(FData));
end;

class function TCefPrintJobCallbackRef.UnWrap(data: Pointer): ICefPrintJobCallback;
begin
  if (data <> nil) then
    Result := Create(data) as ICefPrintJobCallback
   else
    Result := nil;
end;

end.
