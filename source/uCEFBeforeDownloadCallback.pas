unit uCEFBeforeDownloadCallback;

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
  TCefBeforeDownloadCallbackRef = class(TCefBaseRefCountedRef, ICefBeforeDownloadCallback)
    protected
      procedure Cont(const downloadPath: ustring; showDialog: Boolean);

    public
      class function UnWrap(data: Pointer): ICefBeforeDownloadCallback;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;

procedure TCefBeforeDownloadCallbackRef.Cont(const downloadPath: ustring; showDialog: Boolean);
var
  TempPath : TCefString;
begin
  TempPath := CefString(downloadPath);
  PCefBeforeDownloadCallback(FData)^.cont(PCefBeforeDownloadCallback(FData), @TempPath, Ord(showDialog));
end;

class function TCefBeforeDownloadCallbackRef.UnWrap(data: Pointer): ICefBeforeDownloadCallback;
begin
  if (data <> nil) then
    Result := Create(data) as ICefBeforeDownloadCallback
   else
    Result := nil;
end;

end.
