unit uCEFResourceReadCallback;

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
  TCefResourceReadCallbackRef = class(TCefBaseRefCountedRef, ICefResourceReadCallback)
    protected
      procedure Cont(bytes_read: int64);

    public
      class function UnWrap(data: Pointer): ICefResourceReadCallback;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;

procedure TCefResourceReadCallbackRef.Cont(bytes_read: int64);
begin
  PCefResourceReadCallback(FData)^.cont(PCefResourceReadCallback(FData), bytes_read);
end;

class function TCefResourceReadCallbackRef.UnWrap(data: Pointer): ICefResourceReadCallback;
begin
  if (data <> nil) then
    Result := Create(data) as ICefResourceReadCallback
   else
    Result := nil;
end;

end.
