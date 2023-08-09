unit uCEFDownloadItemCallback;

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
  TCefDownloadItemCallbackRef = class(TCefBaseRefCountedRef, ICefDownloadItemCallback)
  protected
    procedure Cancel;
    procedure Pause;
    procedure Resume;
  public
    class function UnWrap(data: Pointer): ICefDownloadItemCallback;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;

procedure TCefDownloadItemCallbackRef.cancel;
begin
  PCefDownloadItemCallback(FData)^.cancel(PCefDownloadItemCallback(FData));
end;

procedure TCefDownloadItemCallbackRef.Pause;
begin
  PCefDownloadItemCallback(FData)^.pause(PCefDownloadItemCallback(FData));
end;

procedure TCefDownloadItemCallbackRef.Resume;
begin
  PCefDownloadItemCallback(FData)^.resume(PCefDownloadItemCallback(FData));
end;

class function TCefDownloadItemCallbackRef.UnWrap(data: Pointer): ICefDownloadItemCallback;
begin
  if (data <> nil) then
    Result := Create(data) as ICefDownloadItemCallback
   else
    Result := nil;
end;

end.
