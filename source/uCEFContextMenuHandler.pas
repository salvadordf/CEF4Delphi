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

unit uCEFContextMenuHandler;

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
  TCefContextMenuHandlerOwn = class(TCefBaseRefCountedOwn, ICefContextMenuHandler)
    protected
      procedure OnBeforeContextMenu(const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; const model: ICefMenuModel); virtual;
      function  RunContextMenu(const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; const model: ICefMenuModel; const callback: ICefRunContextMenuCallback): Boolean; virtual;
      function  OnContextMenuCommand(const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; commandId: Integer; eventFlags: TCefEventFlags): Boolean; virtual;
      procedure OnContextMenuDismissed(const browser: ICefBrowser; const frame: ICefFrame); virtual;

      procedure RemoveReferences; virtual;

    public
      constructor Create; virtual;
  end;

  TCustomContextMenuHandler = class(TCefContextMenuHandlerOwn)
    protected
      FEvents : Pointer;

      procedure OnBeforeContextMenu(const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; const model: ICefMenuModel); override;
      function  RunContextMenu(const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; const model: ICefMenuModel; const callback: ICefRunContextMenuCallback): Boolean; override;
      function  OnContextMenuCommand(const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; commandId: Integer; eventFlags: TCefEventFlags): Boolean; override;
      procedure OnContextMenuDismissed(const browser: ICefBrowser; const frame: ICefFrame); override;

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
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBrowser, uCEFFrame, uCEFContextMenuParams,
  uCEFMenuModel, uCEFRunContextMenuCallback;

procedure cef_context_menu_handler_on_before_context_menu(self    : PCefContextMenuHandler;
                                                          browser : PCefBrowser;
                                                          frame   : PCefFrame;
                                                          params  : PCefContextMenuParams;
                                                          model   : PCefMenuModel); stdcall;
var
  TempObject  : TObject;
begin
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefContextMenuHandlerOwn) then
    TCefContextMenuHandlerOwn(TempObject).OnBeforeContextMenu(TCefBrowserRef.UnWrap(browser),
                                                              TCefFrameRef.UnWrap(frame),
                                                              TCefContextMenuParamsRef.UnWrap(params),
                                                              TCefMenuModelRef.UnWrap(model));
end;

function cef_context_menu_handler_run_context_menu(self     : PCefContextMenuHandler;
                                                   browser  : PCefBrowser;
                                                   frame    : PCefFrame;
                                                   params   : PCefContextMenuParams;
                                                   model    : PCefMenuModel;
                                                   callback : PCefRunContextMenuCallback): Integer; stdcall;
var
  TempObject  : TObject;
begin
  Result      := Ord(False);
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefContextMenuHandlerOwn) then
    Result := Ord(TCefContextMenuHandlerOwn(TempObject).RunContextMenu(TCefBrowserRef.UnWrap(browser),
                                                                       TCefFrameRef.UnWrap(frame),
                                                                       TCefContextMenuParamsRef.UnWrap(params),
                                                                       TCefMenuModelRef.UnWrap(model),
                                                                       TCefRunContextMenuCallbackRef.UnWrap(callback)));
end;

function cef_context_menu_handler_on_context_menu_command(self        : PCefContextMenuHandler;
                                                          browser     : PCefBrowser;
                                                          frame       : PCefFrame;
                                                          params      : PCefContextMenuParams;
                                                          command_id  : Integer;
                                                          event_flags : TCefEventFlags): Integer; stdcall;
var
  TempObject  : TObject;
begin
  Result      := Ord(False);
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefContextMenuHandlerOwn) then
    Result := Ord(TCefContextMenuHandlerOwn(TempObject).OnContextMenuCommand(TCefBrowserRef.UnWrap(browser),
                                                                             TCefFrameRef.UnWrap(frame),
                                                                             TCefContextMenuParamsRef.UnWrap(params),
                                                                             command_id,
                                                                             event_flags));
end;

procedure cef_context_menu_handler_on_context_menu_dismissed(self    : PCefContextMenuHandler;
                                                             browser : PCefBrowser;
                                                             frame   : PCefFrame); stdcall;
var
  TempObject  : TObject;
begin
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefContextMenuHandlerOwn) then
    TCefContextMenuHandlerOwn(TempObject).OnContextMenuDismissed(TCefBrowserRef.UnWrap(browser),
                                                                 TCefFrameRef.UnWrap(frame));
end;

constructor TCefContextMenuHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefContextMenuHandler));

  with PCefContextMenuHandler(FData)^ do
    begin
      on_before_context_menu    := {$IFDEF FPC}@{$ENDIF}cef_context_menu_handler_on_before_context_menu;
      run_context_menu          := {$IFDEF FPC}@{$ENDIF}cef_context_menu_handler_run_context_menu;
      on_context_menu_command   := {$IFDEF FPC}@{$ENDIF}cef_context_menu_handler_on_context_menu_command;
      on_context_menu_dismissed := {$IFDEF FPC}@{$ENDIF}cef_context_menu_handler_on_context_menu_dismissed;
    end;
end;

procedure TCefContextMenuHandlerOwn.OnBeforeContextMenu(const browser : ICefBrowser;
                                                        const frame   : ICefFrame;
                                                        const params  : ICefContextMenuParams;
                                                        const model   : ICefMenuModel);
begin
  //
end;

function TCefContextMenuHandlerOwn.OnContextMenuCommand(const browser    : ICefBrowser;
                                                        const frame      : ICefFrame;
                                                        const params     : ICefContextMenuParams;
                                                              commandId  : Integer;
                                                              eventFlags : TCefEventFlags): Boolean;
begin
  Result := False;
end;

procedure TCefContextMenuHandlerOwn.OnContextMenuDismissed(const browser : ICefBrowser;
                                                           const frame   : ICefFrame);
begin
  //
end;

function TCefContextMenuHandlerOwn.RunContextMenu(const browser  : ICefBrowser;
                                                  const frame    : ICefFrame;
                                                  const params   : ICefContextMenuParams;
                                                  const model    : ICefMenuModel;
                                                  const callback : ICefRunContextMenuCallback): Boolean;
begin
  Result := False;
end;

procedure TCefContextMenuHandlerOwn.RemoveReferences;
begin
  //
end;

// TCustomContextMenuHandler

constructor TCustomContextMenuHandler.Create(const events : IChromiumEvents);
begin
  inherited Create;

  FEvents := Pointer(events);
end;

destructor TCustomContextMenuHandler.Destroy;
begin
  RemoveReferences;

  inherited Destroy;
end;

procedure TCustomContextMenuHandler.RemoveReferences;
begin
  FEvents := nil;
end;

procedure TCustomContextMenuHandler.OnBeforeContextMenu(const browser : ICefBrowser;
                                                        const frame   : ICefFrame;
                                                        const params  : ICefContextMenuParams;
                                                        const model   : ICefMenuModel);
begin
  if (FEvents <> nil) then IChromiumEvents(FEvents).doOnBeforeContextMenu(browser, frame, params, model);
end;

function TCustomContextMenuHandler.RunContextMenu(const browser  : ICefBrowser;
                                                  const frame    : ICefFrame;
                                                  const params   : ICefContextMenuParams;
                                                  const model    : ICefMenuModel;
                                                  const callback : ICefRunContextMenuCallback): Boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doRunContextMenu(browser, frame, params, model, callback)
   else
    Result := inherited RunContextMenu(browser, frame, params, model, callback);
end;

function TCustomContextMenuHandler.OnContextMenuCommand(const browser    : ICefBrowser;
                                                        const frame      : ICefFrame;
                                                        const params     : ICefContextMenuParams;
                                                              commandId  : Integer;
                                                              eventFlags : TCefEventFlags): Boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnContextMenuCommand(browser, frame, params, commandId, eventFlags)
   else
    Result := inherited OnContextMenuCommand(browser, frame, params, commandId, eventFlags);
end;

procedure TCustomContextMenuHandler.OnContextMenuDismissed(const browser : ICefBrowser;
                                                           const frame   : ICefFrame);
begin
  if (FEvents <> nil) then IChromiumEvents(FEvents).doOnContextMenuDismissed(browser, frame);
end;

end.
