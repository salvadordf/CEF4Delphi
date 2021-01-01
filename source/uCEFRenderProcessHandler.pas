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

unit uCEFRenderProcessHandler;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes,
  {$ELSE}
  Classes,
  {$ENDIF}
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes, uCEFListValue, uCEFBrowser, uCEFFrame, uCEFRequest,
  uCEFv8Context, uCEFv8Exception, uCEFv8StackTrace, uCEFDomNode, uCEFProcessMessage, uCEFApplicationCore;

type
  TCefRenderProcessHandlerOwn = class(TCefBaseRefCountedOwn, ICefRenderProcessHandler)
    protected
      procedure OnWebKitInitialized; virtual; abstract;
      procedure OnBrowserCreated(const browser: ICefBrowser; const extra_info: ICefDictionaryValue); virtual; abstract;
      procedure OnBrowserDestroyed(const browser: ICefBrowser); virtual; abstract;
      function  GetLoadHandler: ICefLoadHandler; virtual;
      procedure OnContextCreated(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context); virtual; abstract;
      procedure OnContextReleased(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context); virtual; abstract;
      procedure OnUncaughtException(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context; const V8Exception: ICefV8Exception; const stackTrace: ICefV8StackTrace); virtual; abstract;
      procedure OnFocusedNodeChanged(const browser: ICefBrowser; const frame: ICefFrame; const node: ICefDomNode); virtual; abstract;
      function  OnProcessMessageReceived(const browser: ICefBrowser; const frame: ICefFrame; sourceProcess: TCefProcessId; const aMessage: ICefProcessMessage): Boolean; virtual;

      procedure RemoveReferences; virtual; abstract;

    public
      constructor Create; virtual;
  end;

  TCefCustomRenderProcessHandler = class(TCefRenderProcessHandlerOwn)
    protected
      FCefApp      : TCefApplicationCore;
      FLoadHandler : ICefLoadHandler;

      procedure OnWebKitInitialized; override;
      procedure OnBrowserCreated(const browser: ICefBrowser; const extra_info: ICefDictionaryValue); override;
      procedure OnBrowserDestroyed(const browser: ICefBrowser); override;
      function  GetLoadHandler: ICefLoadHandler; override;
      procedure OnContextCreated(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context); override;
      procedure OnContextReleased(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context); override;
      procedure OnUncaughtException(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context; const V8Exception: ICefV8Exception; const stackTrace: ICefV8StackTrace); override;
      procedure OnFocusedNodeChanged(const browser: ICefBrowser; const frame: ICefFrame; const node: ICefDomNode); override;
      function  OnProcessMessageReceived(const browser: ICefBrowser; const frame: ICefFrame; sourceProcess: TCefProcessId; const aMessage : ICefProcessMessage): Boolean; override;

      procedure RemoveReferences; override;

    public
      constructor Create(const aCefApp : TCefApplicationCore); reintroduce;
      destructor  Destroy; override;
  end;

implementation

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  uCEFMiscFunctions, uCEFLibFunctions, uCEFConstants, uCEFLoadHandler, uCEFDictionaryValue;

procedure cef_render_process_handler_on_web_kit_initialized(self: PCefRenderProcessHandler); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and
     (TempObject is TCefRenderProcessHandlerOwn) then
    TCefRenderProcessHandlerOwn(TempObject).OnWebKitInitialized;
end;

procedure cef_render_process_handler_on_browser_created(self       : PCefRenderProcessHandler;
                                                        browser    : PCefBrowser;
                                                        extra_info : PCefDictionaryValue); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and
     (TempObject is TCefRenderProcessHandlerOwn) then
    TCefRenderProcessHandlerOwn(TempObject).OnBrowserCreated(TCefBrowserRef.UnWrap(browser),
                                                             TCefDictionaryValueRef.UnWrap(extra_info));
end;

procedure cef_render_process_handler_on_browser_destroyed(self    : PCefRenderProcessHandler;
                                                          browser : PCefBrowser); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and
     (TempObject is TCefRenderProcessHandlerOwn) then
    TCefRenderProcessHandlerOwn(TempObject).OnBrowserDestroyed(TCefBrowserRef.UnWrap(browser));
end;

function cef_render_process_handler_get_load_handler(self: PCefRenderProcessHandler): PCefLoadHandler; stdcall;
var
  TempObject : TObject;
begin
  Result     := nil;
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and
     (TempObject is TCefRenderProcessHandlerOwn) then
    Result := CefGetData(TCefRenderProcessHandlerOwn(TempObject).GetLoadHandler);
end;

procedure cef_render_process_handler_on_context_created(self    : PCefRenderProcessHandler;
                                                        browser : PCefBrowser;
                                                        frame   : PCefFrame;
                                                        context : PCefv8Context); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and
     (TempObject is TCefRenderProcessHandlerOwn) then
    TCefRenderProcessHandlerOwn(TempObject).OnContextCreated(TCefBrowserRef.UnWrap(browser),
                                                             TCefFrameRef.UnWrap(frame),
                                                             TCefv8ContextRef.UnWrap(context));
end;

procedure cef_render_process_handler_on_context_released(self    : PCefRenderProcessHandler;
                                                         browser : PCefBrowser;
                                                         frame   : PCefFrame;
                                                         context : PCefv8Context); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and
     (TempObject is TCefRenderProcessHandlerOwn) then
    TCefRenderProcessHandlerOwn(TempObject).OnContextReleased(TCefBrowserRef.UnWrap(browser),
                                                              TCefFrameRef.UnWrap(frame),
                                                              TCefv8ContextRef.UnWrap(context));
end;

procedure cef_render_process_handler_on_uncaught_exception(self       : PCefRenderProcessHandler;
                                                           browser    : PCefBrowser;
                                                           frame      : PCefFrame;
                                                           context    : PCefv8Context;
                                                           exception  : PCefV8Exception;
                                                           stackTrace : PCefV8StackTrace); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and
     (TempObject is TCefRenderProcessHandlerOwn) then
    TCefRenderProcessHandlerOwn(TempObject).OnUncaughtException(TCefBrowserRef.UnWrap(browser),
                                                                TCefFrameRef.UnWrap(frame),
                                                                TCefv8ContextRef.UnWrap(context),
                                                                TCefV8ExceptionRef.UnWrap(exception),
                                                                TCefV8StackTraceRef.UnWrap(stackTrace));
end;

procedure cef_render_process_handler_on_focused_node_changed(self    : PCefRenderProcessHandler;
                                                             browser : PCefBrowser;
                                                             frame   : PCefFrame;
                                                             node    : PCefDomNode); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and
     (TempObject is TCefRenderProcessHandlerOwn) then
    TCefRenderProcessHandlerOwn(TempObject).OnFocusedNodeChanged(TCefBrowserRef.UnWrap(browser),
                                                                 TCefFrameRef.UnWrap(frame),
                                                                 TCefDomNodeRef.UnWrap(node));
end;

function cef_render_process_handler_on_process_message_received(self           : PCefRenderProcessHandler;
                                                                browser        : PCefBrowser;
                                                                frame          : PCefFrame;
                                                                source_process : TCefProcessId;
                                                                message_       : PCefProcessMessage): Integer; stdcall;
var
  TempObject : TObject;
begin
  Result     := Ord(False);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and
     (TempObject is TCefRenderProcessHandlerOwn) then
    Result := Ord(TCefRenderProcessHandlerOwn(TempObject).OnProcessMessageReceived(TCefBrowserRef.UnWrap(browser),
                                                                                   TCefFrameRef.UnWrap(frame),
                                                                                   source_process,
                                                                                   TCefProcessMessageRef.UnWrap(message_)));
end;


// TCefRenderProcessHandlerOwn


constructor TCefRenderProcessHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefRenderProcessHandler));

  with PCefRenderProcessHandler(FData)^ do
    begin
      on_web_kit_initialized      := {$IFDEF FPC}@{$ENDIF}cef_render_process_handler_on_web_kit_initialized;
      on_browser_created          := {$IFDEF FPC}@{$ENDIF}cef_render_process_handler_on_browser_created;
      on_browser_destroyed        := {$IFDEF FPC}@{$ENDIF}cef_render_process_handler_on_browser_destroyed;
      get_load_handler            := {$IFDEF FPC}@{$ENDIF}cef_render_process_handler_get_load_handler;
      on_context_created          := {$IFDEF FPC}@{$ENDIF}cef_render_process_handler_on_context_created;
      on_context_released         := {$IFDEF FPC}@{$ENDIF}cef_render_process_handler_on_context_released;
      on_uncaught_exception       := {$IFDEF FPC}@{$ENDIF}cef_render_process_handler_on_uncaught_exception;
      on_focused_node_changed     := {$IFDEF FPC}@{$ENDIF}cef_render_process_handler_on_focused_node_changed;
      on_process_message_received := {$IFDEF FPC}@{$ENDIF}cef_render_process_handler_on_process_message_received;
    end;
end;

function TCefRenderProcessHandlerOwn.GetLoadHandler: ICefLoadHandler;
begin
  Result := nil;
end;

function TCefRenderProcessHandlerOwn.OnProcessMessageReceived(const browser       : ICefBrowser;
                                                              const frame         : ICefFrame;
                                                                    sourceProcess : TCefProcessId;
                                                              const aMessage      : ICefProcessMessage): Boolean;
begin
  Result := False;
end;


// TCefCustomRenderProcessHandler


constructor TCefCustomRenderProcessHandler.Create(const aCefApp : TCefApplicationCore);
begin
  inherited Create;

  FCefApp := aCefApp;

  if (FCefApp <> nil) and FCefApp.MustCreateLoadHandler then
    FLoadHandler := TCustomRenderLoadHandler.Create(FCefApp)
   else
    FLoadHandler := nil;
end;

destructor TCefCustomRenderProcessHandler.Destroy;
begin
  RemoveReferences;

  inherited Destroy;
end;

procedure TCefCustomRenderProcessHandler.RemoveReferences;
begin
  if (FLoadHandler <> nil) then
    FLoadHandler.RemoveReferences;

  FCefApp      := nil;
  FLoadHandler := nil;
end;

procedure TCefCustomRenderProcessHandler.OnWebKitInitialized;
begin
  try
    if (FCefApp <> nil) then FCefApp.Internal_OnWebKitInitialized;
  except
    on e : exception do
      if CustomExceptionHandler('TCefCustomRenderProcessHandler.OnWebKitInitialized', e) then raise;
  end;
end;

procedure TCefCustomRenderProcessHandler.OnBrowserCreated(const browser: ICefBrowser; const extra_info: ICefDictionaryValue);
begin
  try
    if (FCefApp <> nil) then FCefApp.Internal_OnBrowserCreated(browser, extra_info);
  except
    on e : exception do
      if CustomExceptionHandler('TCefCustomRenderProcessHandler.OnBrowserCreated', e) then raise;
  end;
end;

procedure TCefCustomRenderProcessHandler.OnBrowserDestroyed(const browser: ICefBrowser);
begin
  try
    if (FCefApp <> nil) then FCefApp.Internal_OnBrowserDestroyed(browser);
  except
    on e : exception do
      if CustomExceptionHandler('TCefCustomRenderProcessHandler.OnBrowserDestroyed', e) then raise;
  end;
end;

function TCefCustomRenderProcessHandler.GetLoadHandler: ICefLoadHandler;
begin
  if (FLoadHandler <> nil) then
    Result := FLoadHandler
   else
    Result := inherited GetLoadHandler;
end;

procedure TCefCustomRenderProcessHandler.OnContextCreated(const browser : ICefBrowser;
                                                          const frame   : ICefFrame;
                                                          const context : ICefv8Context);
begin
  try
    if (FCefApp <> nil) then FCefApp.Internal_OnContextCreated(browser, frame, context);
  except
    on e : exception do
      if CustomExceptionHandler('TCefCustomRenderProcessHandler.OnContextCreated', e) then raise;
  end;
end;

procedure TCefCustomRenderProcessHandler.OnContextReleased(const browser : ICefBrowser;
                                                           const frame   : ICefFrame;
                                                           const context : ICefv8Context);
begin
  try
    if (FCefApp <> nil) then FCefApp.Internal_OnContextReleased(browser, frame, context);
  except
    on e : exception do
      if CustomExceptionHandler('TCefCustomRenderProcessHandler.OnContextReleased', e) then raise;
  end;
end;

procedure TCefCustomRenderProcessHandler.OnUncaughtException(const browser     : ICefBrowser;
                                                             const frame       : ICefFrame;
                                                             const context     : ICefv8Context;
                                                             const V8Exception : ICefV8Exception;
                                                             const stackTrace  : ICefV8StackTrace);
begin
  try
    if (FCefApp <> nil) then FCefApp.Internal_OnUncaughtException(browser, frame, context, V8Exception, stackTrace);
  except
    on e : exception do
      if CustomExceptionHandler('TCefCustomRenderProcessHandler.OnUncaughtException', e) then raise;
  end;
end;

procedure TCefCustomRenderProcessHandler.OnFocusedNodeChanged(const browser : ICefBrowser;
                                                              const frame   : ICefFrame;
                                                              const node    : ICefDomNode);
begin
  try
   if (FCefApp <> nil) then FCefApp.Internal_OnFocusedNodeChanged(browser, frame, node);
  except
    on e : exception do
      if CustomExceptionHandler('TCefCustomRenderProcessHandler.OnFocusedNodeChanged', e) then raise;
  end;
end;

function TCefCustomRenderProcessHandler.OnProcessMessageReceived(const browser       : ICefBrowser;
                                                                 const frame         : ICefFrame;
                                                                       sourceProcess : TCefProcessId;
                                                                 const aMessage      : ICefProcessMessage): Boolean;
begin
  Result := inherited OnProcessMessageReceived(browser, frame, sourceProcess, aMessage);

  try
    if (FCefApp <> nil) then FCefApp.Internal_OnProcessMessageReceived(browser, frame, sourceProcess, aMessage, Result);
  except
    on e : exception do
      if CustomExceptionHandler('TCefCustomRenderProcessHandler.OnProcessMessageReceived', e) then raise;
  end;
end;

end.
