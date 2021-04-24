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

unit uCEFJsDialogHandler;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefJsDialogHandlerOwn = class(TCefBaseRefCountedOwn, ICefJsDialogHandler)
    protected
      function  OnJsdialog(const browser: ICefBrowser; const originUrl: ustring; dialogType: TCefJsDialogType; const messageText, defaultPromptText: ustring; const callback: ICefJsDialogCallback; out suppressMessage: Boolean): Boolean; virtual;
      function  OnBeforeUnloadDialog(const browser: ICefBrowser; const messageText: ustring; isReload: Boolean; const callback: ICefJsDialogCallback): Boolean; virtual;
      procedure OnResetDialogState(const browser: ICefBrowser); virtual;
      procedure OnDialogClosed(const browser: ICefBrowser); virtual;

      procedure RemoveReferences; virtual;

    public
      constructor Create; virtual;
  end;

  TCustomJsDialogHandler = class(TCefJsDialogHandlerOwn)
    protected
      FEvents : Pointer;

      function  OnJsdialog(const browser: ICefBrowser; const originUrl: ustring; dialogType: TCefJsDialogType; const messageText, defaultPromptText: ustring; const callback: ICefJsDialogCallback; out suppressMessage: Boolean): Boolean; override;
      function  OnBeforeUnloadDialog(const browser: ICefBrowser; const messageText: ustring; isReload: Boolean; const callback: ICefJsDialogCallback): Boolean; override;
      procedure OnResetDialogState(const browser: ICefBrowser); override;
      procedure OnDialogClosed(const browser: ICefBrowser); override;

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
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBrowser, uCEFJsDialogCallback;

function cef_jsdialog_handler_on_jsdialog(      self                : PCefJsDialogHandler;
                                                browser             : PCefBrowser;
                                          const origin_url          : PCefString;
                                                dialog_type         : TCefJsDialogType;
                                          const message_text        : PCefString;
                                          const default_prompt_text : PCefString;
                                                callback            : PCefJsDialogCallback;
                                                suppress_message    : PInteger): Integer; stdcall;
var
  TempSuppress : Boolean;
  TempObject   : TObject;
begin
  Result       := Ord(False);
  TempSuppress := suppress_message^ <> 0;
  TempObject   := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefJsDialogHandlerOwn) then
    Result := Ord(TCefJsDialogHandlerOwn(TempObject).OnJsdialog(TCefBrowserRef.UnWrap(browser),
                                                                CefString(origin_url),
                                                                dialog_type,
                                                                CefString(message_text),
                                                                CefString(default_prompt_text),
                                                                TCefJsDialogCallbackRef.UnWrap(callback),
                                                                TempSuppress));

  suppress_message^ := Ord(TempSuppress);
end;

function cef_jsdialog_handler_on_before_unload_dialog(      self         : PCefJsDialogHandler;
                                                            browser      : PCefBrowser;
                                                      const message_text : PCefString;
                                                            is_reload    : Integer;
                                                            callback     : PCefJsDialogCallback): Integer; stdcall;
var
  TempObject : TObject;
begin
  Result     := Ord(False);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefJsDialogHandlerOwn) then
    Result := Ord(TCefJsDialogHandlerOwn(TempObject).OnBeforeUnloadDialog(TCefBrowserRef.UnWrap(browser),
                                                                          CefString(message_text),
                                                                          is_reload <> 0,
                                                                          TCefJsDialogCallbackRef.UnWrap(callback)));
end;

procedure cef_jsdialog_handler_on_reset_dialog_state(self    : PCefJsDialogHandler;
                                                     browser : PCefBrowser); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefJsDialogHandlerOwn) then
    TCefJsDialogHandlerOwn(TempObject).OnResetDialogState(TCefBrowserRef.UnWrap(browser));
end;

procedure cef_jsdialog_handler_on_dialog_closed(self    : PCefJsDialogHandler;
                                                browser : PCefBrowser); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefJsDialogHandlerOwn) then
    TCefJsDialogHandlerOwn(TempObject).OnDialogClosed(TCefBrowserRef.UnWrap(browser));
end;

constructor TCefJsDialogHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefJsDialogHandler));

  with PCefJsDialogHandler(FData)^ do
    begin
      on_jsdialog             := {$IFDEF FPC}@{$ENDIF}cef_jsdialog_handler_on_jsdialog;
      on_before_unload_dialog := {$IFDEF FPC}@{$ENDIF}cef_jsdialog_handler_on_before_unload_dialog;
      on_reset_dialog_state   := {$IFDEF FPC}@{$ENDIF}cef_jsdialog_handler_on_reset_dialog_state;
      on_dialog_closed        := {$IFDEF FPC}@{$ENDIF}cef_jsdialog_handler_on_dialog_closed;
    end;
end;

function TCefJsDialogHandlerOwn.OnJsdialog(const browser           : ICefBrowser;
                                           const originUrl         : ustring;
                                                 dialogType        : TCefJsDialogType;
                                           const messageText       : ustring;
                                           const defaultPromptText : ustring;
                                           const callback          : ICefJsDialogCallback;
                                           out   suppressMessage   : Boolean): Boolean;
begin
  Result          := False;
  suppressMessage := False;
end;

function TCefJsDialogHandlerOwn.OnBeforeUnloadDialog(const browser     : ICefBrowser;
                                                     const messageText : ustring;
                                                           isReload    : Boolean;
                                                     const callback    : ICefJsDialogCallback): Boolean;
begin
  Result := False;
end;

procedure TCefJsDialogHandlerOwn.OnDialogClosed(const browser: ICefBrowser);
begin
  //
end;

procedure TCefJsDialogHandlerOwn.OnResetDialogState(const browser: ICefBrowser);
begin
  //
end;

procedure TCefJsDialogHandlerOwn.RemoveReferences;
begin
  //
end;

// TCustomJsDialogHandler

constructor TCustomJsDialogHandler.Create(const events : IChromiumEvents);
begin
  inherited Create;

  FEvents := Pointer(events);
end;

destructor TCustomJsDialogHandler.Destroy;
begin
  RemoveReferences;

  inherited Destroy;
end;

procedure TCustomJsDialogHandler.RemoveReferences;
begin
  FEvents := nil;
end;

function TCustomJsDialogHandler.OnBeforeUnloadDialog(const browser     : ICefBrowser;
                                                     const messageText : ustring;
                                                           isReload    : Boolean;
                                                     const callback    : ICefJsDialogCallback): Boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnBeforeUnloadDialog(browser, messageText, isReload, callback)
   else
    Result := inherited OnBeforeUnloadDialog(browser, messageText, isReload, callback);
end;

procedure TCustomJsDialogHandler.OnDialogClosed(const browser: ICefBrowser);
begin
  if (FEvents <> nil) then IChromiumEvents(FEvents).doOnDialogClosed(browser);
end;

function TCustomJsDialogHandler.OnJsdialog(const browser           : ICefBrowser;
                                           const originUrl         : ustring;
                                                 dialogType        : TCefJsDialogType;
                                           const messageText       : ustring;
                                           const defaultPromptText : ustring;
                                           const callback          : ICefJsDialogCallback;
                                           out   suppressMessage   : Boolean): Boolean;
begin
  suppressMessage := False;

  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnJsdialog(browser, originUrl, dialogType, messageText, defaultPromptText, callback, suppressMessage)
   else
    Result := inherited OnJsdialog(browser, originUrl, dialogType, messageText, defaultPromptText, callback, suppressMessage);
end;

procedure TCustomJsDialogHandler.OnResetDialogState(const browser: ICefBrowser);
begin
  if (FEvents <> nil) then IChromiumEvents(FEvents).doOnResetDialogState(browser);
end;

end.
