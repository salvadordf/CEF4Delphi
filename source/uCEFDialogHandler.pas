unit uCEFDialogHandler;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes,
  {$ELSE}
  Classes,
  {$ENDIF}
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefDialogHandlerOwn = class(TCefBaseRefCountedOwn, ICefDialogHandler)
    protected
      function  OnFileDialog(const browser: ICefBrowser; mode: TCefFileDialogMode; const title, defaultFilePath: ustring; const acceptFilters, accept_extensions, accept_descriptions: TStrings; const callback: ICefFileDialogCallback): Boolean; virtual;

      procedure RemoveReferences; virtual;

    public
      constructor Create; virtual;
  end;

  TCustomDialogHandler = class(TCefDialogHandlerOwn)
    protected
      FEvents : Pointer;

      function  OnFileDialog(const browser: ICefBrowser; mode: TCefFileDialogMode; const title: ustring; const defaultFilePath: ustring; const acceptFilters, accept_extensions, accept_descriptions: TStrings; const callback: ICefFileDialogCallback): Boolean; override;

      procedure RemoveReferences; override;

    public
      constructor Create(const events : IChromiumEvents); reintroduce; virtual;
      destructor  Destroy; override;
  end;

implementation

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBrowser, uCEFFileDialogCallback, uCEFStringList;

function cef_dialog_handler_on_file_dialog(self                    : PCefDialogHandler;
                                           browser                 : PCefBrowser;
                                           mode                    : TCefFileDialogMode;
                                           const title             : PCefString;
                                           const default_file_path : PCefString;
                                           accept_filters          : TCefStringList;
                                           accept_extensions       : TCefStringList;
                                           accept_descriptions     : TCefStringList;
                                           callback                : PCefFileDialogCallback): Integer; stdcall;
var
  TempFilters          : TStringList;
  TempExtensions       : TStringList;
  TempDescriptions     : TStringList;
  TempCefFilters       : ICefStringList;
  TempCefExtensions    : ICefStringList;
  TempCefDescriptions  : ICefStringList;
  TempObject           : TObject;
begin
  TempFilters      := nil;
  TempExtensions   := nil;
  TempDescriptions := nil;
  Result           := Ord(False);

  try
    try
      TempObject := CefGetObject(self);

      if (TempObject <> nil) and (TempObject is TCefDialogHandlerOwn) then
        begin
          TempFilters    := TStringList.Create;
          TempCefFilters := TCefStringListRef.Create(accept_filters);
          TempCefFilters.CopyToStrings(TempFilters);

          TempExtensions    := TStringList.Create;
          TempCefExtensions := TCefStringListRef.Create(accept_extensions);
          TempCefExtensions.CopyToStrings(TempExtensions);

          TempDescriptions    := TStringList.Create;
          TempCefDescriptions := TCefStringListRef.Create(accept_descriptions);
          TempCefDescriptions.CopyToStrings(TempDescriptions);

          Result := Ord(TCefDialogHandlerOwn(TempObject).OnFileDialog(TCefBrowserRef.UnWrap(browser),
                                                                      mode,
                                                                      CefString(title),
                                                                      CefString(default_file_path),
                                                                      TempFilters,
                                                                      TempExtensions,
                                                                      TempDescriptions,
                                                                      TCefFileDialogCallbackRef.UnWrap(callback)));
        end;
    except
      on e : exception do
        if CustomExceptionHandler('cef_dialog_handler_on_file_dialog', e) then raise;
    end;
  finally
    if (TempFilters      <> nil) then FreeAndNil(TempFilters);
    if (TempExtensions   <> nil) then FreeAndNil(TempExtensions);
    if (TempDescriptions <> nil) then FreeAndNil(TempDescriptions);
  end;
end;

constructor TCefDialogHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefDialogHandler));

  PCefDialogHandler(FData)^.on_file_dialog := {$IFDEF FPC}@{$ENDIF}cef_dialog_handler_on_file_dialog;
end;

function TCefDialogHandlerOwn.OnFileDialog(const browser                : ICefBrowser;
                                                 mode                   : TCefFileDialogMode;
                                           const title                  : ustring;
                                           const defaultFilePath        : ustring;
                                           const acceptFilters          : TStrings;
                                           const accept_extensions      : TStrings;
                                           const accept_descriptions    : TStrings;
                                           const callback               : ICefFileDialogCallback): Boolean;
begin
  Result := False;
end;

procedure TCefDialogHandlerOwn.RemoveReferences;
begin
  //
end;

// TCustomDialogHandler

constructor TCustomDialogHandler.Create(const events : IChromiumEvents);
begin
  inherited Create;

  FEvents := Pointer(events);
end;

destructor TCustomDialogHandler.Destroy;
begin
  RemoveReferences;

  inherited Destroy;
end;

procedure TCustomDialogHandler.RemoveReferences;
begin
  FEvents := nil;
end;

function TCustomDialogHandler.OnFileDialog(const browser              : ICefBrowser;
                                                 mode                 : TCefFileDialogMode;
                                           const title                : ustring;
                                           const defaultFilePath      : ustring;
                                           const acceptFilters        : TStrings;
                                           const accept_extensions    : TStrings;
                                           const accept_descriptions  : TStrings;
                                           const callback             : ICefFileDialogCallback): Boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnFileDialog(browser, mode, title, defaultFilePath,
                                                      acceptFilters, accept_extensions, accept_descriptions, callback)
   else
    Result := inherited OnFileDialog(browser, mode, title, defaultFilePath,
                                     acceptFilters, accept_extensions, accept_descriptions, callback);
end;

end.

