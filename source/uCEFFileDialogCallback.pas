unit uCEFFileDialogCallback;

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
  TCefFileDialogCallbackRef = class(TCefBaseRefCountedRef, ICefFileDialogCallback)
  protected
    procedure Cont(const filePaths: TStrings);
    procedure Cancel;
  public
    class function UnWrap(data: Pointer): ICefFileDialogCallback;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFStringList;

procedure TCefFileDialogCallbackRef.Cancel;
begin
  PCefFileDialogCallback(FData)^.cancel(PCefFileDialogCallback(FData));
end;

procedure TCefFileDialogCallbackRef.Cont(const filePaths: TStrings);
var
  TempSL : ICefStringList;
begin
  try
    TempSL := TCefStringListOwn.Create;
    TempSL.AddStrings(filePaths);

    PCefFileDialogCallback(FData)^.cont(PCefFileDialogCallback(FData),
                                        TempSL.Handle);
  finally
    TempSL := nil;
  end;
end;

class function TCefFileDialogCallbackRef.UnWrap(data: Pointer): ICefFileDialogCallback;
begin
  if (data <> nil) then
    Result := Create(data) as ICefFileDialogCallback
   else
    Result := nil;
end;

end.
