unit uCEFResourceSkipCallback;

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
  TCefResourceSkipCallbackRef = class(TCefBaseRefCountedRef, ICefResourceSkipCallback)
    protected
      procedure Cont(bytes_skipped: int64);

    public
      class function UnWrap(data: Pointer): ICefResourceSkipCallback;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;

procedure TCefResourceSkipCallbackRef.Cont(bytes_skipped: int64);
begin
  PCefResourceSkipCallback(FData)^.cont(PCefResourceSkipCallback(FData), bytes_skipped);
end;

class function TCefResourceSkipCallbackRef.UnWrap(data: Pointer): ICefResourceSkipCallback;
begin
  if (data <> nil) then
    Result := Create(data) as ICefResourceSkipCallback
   else
    Result := nil;
end;

end.
