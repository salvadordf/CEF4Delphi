// ************************************************************************
// ***************************** CEF4Delphi *******************************
// ************************************************************************
//
// CEF4Delphi is based on DCEF3 which uses CEF to embed a chromium-based
// browser in Delphi applications.
//
// The original license of DCEF3 still applies to CEF4Delphi.
//
// For more information about CEF4Delphi visit :
//         https://www.briskbard.com/index.php?lang=en&pageid=cef
//
//        Copyright © 2021 Salvador Diaz Fau. All rights reserved.
//
// ************************************************************************
// ************ vvvv Original license and comments below vvvv *************
// ************************************************************************
(*
 *                       Delphi Chromium Embedded 3
 *
 * Usage allowed under the restrictions of the Lesser GNU General Public License
 * or alternatively the restrictions of the Mozilla Public License 1.1
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * Unit owner : Henri Gourvest <hgourvest@gmail.com>
 * Web site   : http://www.progdigy.com
 * Repository : http://code.google.com/p/delphichromiumembedded/
 * Group      : http://groups.google.com/group/delphichromiumembedded
 *
 * Embarcadero Technologies, Inc is not permitted to use or redistribute
 * this source code without explicit permission.
 *
 *)

unit uCEFPrintHandler;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes, uCEFApplicationCore;

type
  TCefPrintHandlerOwn = class(TCefBaseRefCountedOwn, ICefPrintHandler)
    protected
      procedure OnPrintStart(const browser: ICefBrowser); virtual; abstract;
      procedure OnPrintSettings(const browser: ICefBrowser; const settings: ICefPrintSettings; getDefaults: boolean); virtual; abstract;
      procedure OnPrintDialog(const browser: ICefBrowser; hasSelection: boolean; const callback: ICefPrintDialogCallback; var aResult: boolean); virtual;
      procedure OnPrintJob(const browser: ICefBrowser; const documentName, PDFFilePath: ustring; const callback: ICefPrintJobCallback; var aResult: boolean); virtual;
      procedure OnPrintReset(const browser: ICefBrowser); virtual; abstract;
      procedure GetPDFPaperSize(deviceUnitsPerInch: Integer; var aResult: TCefSize); virtual;

      procedure RemoveReferences; virtual; abstract;

    public
      constructor Create; virtual;
  end;

  TCustomPrintHandler = class(TCefPrintHandlerOwn)
    protected
      FCefApp : TCefApplicationCore;

      procedure OnPrintStart(const browser: ICefBrowser); override;
      procedure OnPrintSettings(const browser: ICefBrowser; const settings: ICefPrintSettings; getDefaults: boolean); override;
      procedure OnPrintDialog(const browser: ICefBrowser; hasSelection: boolean; const callback: ICefPrintDialogCallback; var aResult: boolean); override;
      procedure OnPrintJob(const browser: ICefBrowser; const documentName, PDFFilePath: ustring; const callback: ICefPrintJobCallback; var aResult: boolean); override;
      procedure OnPrintReset(const browser: ICefBrowser); override;
      procedure GetPDFPaperSize(deviceUnitsPerInch: Integer; var aResult: TCefSize); override;

      procedure RemoveReferences; override;

    public
      constructor Create(const aCefApp : TCefApplicationCore); reintroduce; virtual;
      destructor  Destroy; override;
  end;

implementation

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBrowser, uCEFPrintSettings,
  uCEFPrintDialogCallback, uCEFPrintJobCallback;


// TCefPrintHandlerOwn

procedure cef_print_handler_on_print_start(self: PCefPrintHandler; browser: PCefBrowser); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefPrintHandlerOwn) then
    TCefPrintHandlerOwn(TempObject).OnPrintStart(TCefBrowserRef.UnWrap(browser));
end;

procedure cef_print_handler_on_print_settings(self         : PCefPrintHandler;
                                              browser      : PCefBrowser;
                                              settings     : PCefPrintSettings;
                                              get_defaults : Integer); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefPrintHandlerOwn) then
    TCefPrintHandlerOwn(TempObject).OnPrintSettings(TCefBrowserRef.UnWrap(browser),
                                                    TCefPrintSettingsRef.UnWrap(settings),
                                                    get_defaults <> 0);
end;

function cef_print_handler_on_print_dialog(self          : PCefPrintHandler;
                                           browser       : PCefBrowser;
                                           has_selection : Integer;
                                           callback      : PCefPrintDialogCallback): Integer; stdcall;
var
  TempObject : TObject;
  TempResult : boolean;
begin
  TempResult := False;
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefPrintHandlerOwn) then
    TCefPrintHandlerOwn(TempObject).OnPrintDialog(TCefBrowserRef.UnWrap(browser),
                                                  has_selection <> 0,
                                                  TCefPrintDialogCallbackRef.UnWrap(callback),
                                                  TempResult);

  Result := ord(TempResult);
end;

function cef_print_handler_on_print_job(      self          : PCefPrintHandler;
                                              browser       : PCefBrowser;
                                        const document_name : PCefString;
                                        const pdf_file_path : PCefString;
                                              callback      : PCefPrintJobCallback): Integer; stdcall;
var
  TempObject : TObject;
  TempResult : boolean;
begin
  TempResult := False;
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefPrintHandlerOwn) then
    TCefPrintHandlerOwn(TempObject).OnPrintJob(TCefBrowserRef.UnWrap(browser),
                                               CefString(document_name),
                                               CefString(pdf_file_path),
                                               TCefPrintJobCallbackRef.UnWrap(callback),
                                               TempResult);

  Result := ord(TempResult);
end;

procedure cef_print_handler_on_print_reset(self    : PCefPrintHandler;
                                           browser : PCefBrowser); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefPrintHandlerOwn) then
    TCefPrintHandlerOwn(TempObject).OnPrintReset(TCefBrowserRef.UnWrap(browser));
end;

function cef_print_handler_get_pdf_paper_size(self                  : PCefPrintHandler;
                                              device_units_per_inch : Integer): TCefSize; stdcall;
var
  TempObject : TObject;
  TempSize   : TCefSize;
begin
  TempObject      := CefGetObject(self);
  TempSize.Width  := 0;
  TempSize.Height := 0;

  if (TempObject <> nil) and (TempObject is TCefPrintHandlerOwn) then
    TCefPrintHandlerOwn(TempObject).GetPDFPaperSize(device_units_per_inch, TempSize);

  Result := TempSize;
end;

constructor TCefPrintHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefPrintHandler));

  with PCefPrintHandler(FData)^ do
    begin
      on_print_start      := {$IFDEF FPC}@{$ENDIF}cef_print_handler_on_print_start;
      on_print_settings   := {$IFDEF FPC}@{$ENDIF}cef_print_handler_on_print_settings;
      on_print_dialog     := {$IFDEF FPC}@{$ENDIF}cef_print_handler_on_print_dialog;
      on_print_job        := {$IFDEF FPC}@{$ENDIF}cef_print_handler_on_print_job;
      on_print_reset      := {$IFDEF FPC}@{$ENDIF}cef_print_handler_on_print_reset;
      get_pdf_paper_size  := {$IFDEF FPC}@{$ENDIF}cef_print_handler_get_pdf_paper_size;
    end;
end;

procedure TCefPrintHandlerOwn.OnPrintDialog(const browser: ICefBrowser; hasSelection: boolean; const callback: ICefPrintDialogCallback; var aResult: boolean);
begin
  aResult := False;
end;

procedure TCefPrintHandlerOwn.OnPrintJob(const browser: ICefBrowser; const documentName, PDFFilePath: ustring; const callback: ICefPrintJobCallback; var aResult: boolean);
begin
  aResult := False;
end;

procedure TCefPrintHandlerOwn.GetPDFPaperSize(deviceUnitsPerInch: Integer; var aResult: TCefSize);
begin
  aResult.Width  := 0;
  aResult.Height := 0;
end;


// TCustomPrintHandler

constructor TCustomPrintHandler.Create(const aCefApp : TCefApplicationCore);
begin
  inherited Create;

  FCefApp := aCefApp;
end;

destructor TCustomPrintHandler.Destroy;
begin
  RemoveReferences;

  inherited Destroy;
end;

procedure TCustomPrintHandler.RemoveReferences;
begin
  FCefApp := nil;
end;

procedure TCustomPrintHandler.OnPrintStart(const browser : ICefBrowser);
begin
  try
    if (FCefApp <> nil) then FCefApp.Internal_OnPrintStart(browser);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomPrintHandler.OnPrintStart', e) then raise;
  end;
end;

procedure TCustomPrintHandler.OnPrintSettings(const browser     : ICefBrowser;
                                              const settings    : ICefPrintSettings;
                                                    getDefaults : boolean);
begin
  try
    if (FCefApp <> nil) then FCefApp.Internal_OnPrintSettings(browser, settings, getDefaults);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomPrintHandler.OnPrintSettings', e) then raise;
  end;
end;

procedure TCustomPrintHandler.OnPrintDialog(const browser      : ICefBrowser;
                                                  hasSelection : boolean;
                                            const callback     : ICefPrintDialogCallback;
                                            var   aResult      : boolean);
begin
  try
    if (FCefApp <> nil) then FCefApp.Internal_OnPrintDialog(browser, hasSelection, callback, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomPrintHandler.OnPrintDialog', e) then raise;
  end;
end;

procedure TCustomPrintHandler.OnPrintJob(const browser      : ICefBrowser;
                                         const documentName : ustring;
                                         const PDFFilePath  : ustring;
                                         const callback     : ICefPrintJobCallback;
                                         var   aResult      : boolean);
begin
  try
    if (FCefApp <> nil) then FCefApp.Internal_OnPrintJob(browser, documentName, PDFFilePath, callback, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomPrintHandler.OnPrintJob', e) then raise;
  end;
end;

procedure TCustomPrintHandler.OnPrintReset(const browser : ICefBrowser);
begin
  try
    if (FCefApp <> nil) then FCefApp.Internal_OnPrintReset(browser);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomPrintHandler.OnPrintReset', e) then raise;
  end;
end;

procedure TCustomPrintHandler.GetPDFPaperSize(    deviceUnitsPerInch : Integer;
                                              var aResult            : TCefSize);
begin
  try
    if (FCefApp <> nil) then FCefApp.Internal_OnGetPDFPaperSize(deviceUnitsPerInch, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomPrintHandler.GetPDFPaperSize', e) then raise;
  end;
end;

end.
