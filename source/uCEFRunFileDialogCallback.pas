unit uCEFRunFileDialogCallback;

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
  uCEFBaseRefCounted, uCEFInterfaces;

type
  TCefRunFileDialogCallbackOwn = class(TCefBaseRefCountedOwn, ICefRunFileDialogCallback)
    protected
      procedure OnFileDialogDismissed(const filePaths: TStrings); virtual;

    public
      constructor Create;
  end;

  TCefFastRunFileDialogCallback = class(TCefRunFileDialogCallbackOwn)
    protected
      FCallback: TCefRunFileDialogCallbackProc;

      procedure OnFileDialogDismissed(const filePaths: TStrings); override;

    public
      constructor Create(callback: TCefRunFileDialogCallbackProc); reintroduce; virtual;
  end;

implementation

uses
  uCEFTypes, uCEFMiscFunctions, uCEFLibFunctions, uCEFStringList;

procedure cef_run_file_dialog_callback_on_file_dialog_dismissed(self       : PCefRunFileDialogCallback;
                                                                file_paths : TCefStringList); stdcall;
var
  TempSL     : TStringList;
  TempCefSL  : ICefStringList;
  TempObject : TObject;
begin
  TempSL     := nil;
  TempObject := CefGetObject(self);

  try
    try
      if (TempObject <> nil) and (TempObject is TCefRunFileDialogCallbackOwn) then
        begin
          TempSL    := TStringList.Create;
          TempCefSL := TCefStringListRef.Create(file_paths);
          TempCefSL.CopyToStrings(TempSL);

          TCefRunFileDialogCallbackOwn(TempObject).OnFileDialogDismissed(TempSL);
        end;
    except
      on e : exception do
        if CustomExceptionHandler('cef_run_file_dialog_callback_on_file_dialog_dismissed', e) then raise;
    end;
  finally
    if (TempSL <> nil) then FreeAndNil(TempSL);
  end;
end;

// TCefRunFileDialogCallbackOwn

constructor TCefRunFileDialogCallbackOwn.Create;
begin
  inherited CreateData(SizeOf(TCefRunFileDialogCallback));

  PCefRunFileDialogCallback(FData)^.on_file_dialog_dismissed := {$IFDEF FPC}@{$ENDIF}cef_run_file_dialog_callback_on_file_dialog_dismissed;
end;

procedure TCefRunFileDialogCallbackOwn.OnFileDialogDismissed(const filePaths: TStrings);
begin
  //
end;

// TCefFastRunFileDialogCallback

procedure TCefFastRunFileDialogCallback.OnFileDialogDismissed(const filePaths: TStrings);
begin
  FCallback(filePaths);
end;

constructor TCefFastRunFileDialogCallback.Create(callback: TCefRunFileDialogCallbackProc);
begin
  inherited Create;

  FCallback := callback;
end;

end.
