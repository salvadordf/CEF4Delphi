// ************************************************************************
// ***************************** CEF4Delphi *******************************
// ************************************************************************
//
// CEF4Delphi is based on DCEF3 which uses CEF3 to embed a chromium-based
// browser in Delphi applications.
//
// The original license of DCEF3 still applies to CEF4Delphi.
//
// For more information about CEF4Delphi visit :
//         https://www.briskbard.com/index.php?lang=en&pageid=cef
//
//        Copyright © 2017 Salvador Díaz Fau. All rights reserved.
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

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes,
  {$ELSE}
  Classes,
  {$ENDIF}
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes, uCEFListValue, uCEFBrowser, uCEFFrame, uCEFRequest,
  uCEFv8Context, uCEFv8Exception, uCEFv8StackTrace, uCEFDomNode, uCEFProcessMessage, uCEFApplication;

type
  TCefRenderProcessHandlerOwn = class(TCefBaseRefCountedOwn, ICefRenderProcessHandler)
    protected
      procedure OnRenderThreadCreated(const extraInfo: ICefListValue); virtual; abstract;
      procedure OnWebKitInitialized; virtual; abstract;
      procedure OnBrowserCreated(const browser: ICefBrowser); virtual; abstract;
      procedure OnBrowserDestroyed(const browser: ICefBrowser); virtual; abstract;
      function  GetLoadHandler: ICefLoadHandler; virtual;
      function  OnBeforeNavigation(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; navigationType: TCefNavigationType; isRedirect: Boolean): Boolean; virtual;
      procedure OnContextCreated(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context); virtual; abstract;
      procedure OnContextReleased(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context); virtual; abstract;
      procedure OnUncaughtException(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context; const exception: ICefV8Exception; const stackTrace: ICefV8StackTrace); virtual; abstract;
      procedure OnFocusedNodeChanged(const browser: ICefBrowser; const frame: ICefFrame; const node: ICefDomNode); virtual; abstract;
      function  OnProcessMessageReceived(const browser: ICefBrowser; sourceProcess: TCefProcessId; const aMessage: ICefProcessMessage): Boolean; virtual;
    public
      constructor Create; virtual;
  end;

  TCefCustomRenderProcessHandler = class(TCefRenderProcessHandlerOwn)
    protected
      FCefApp : TCefApplication;

      procedure OnRenderThreadCreated(const extraInfo: ICefListValue); override;
      procedure OnWebKitInitialized; override;
      procedure OnBrowserCreated(const browser: ICefBrowser); override;
      procedure OnBrowserDestroyed(const browser: ICefBrowser); override;
      function  OnBeforeNavigation(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; navigationType: TCefNavigationType; isRedirect: Boolean): Boolean; override;
      procedure OnContextCreated(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context); override;
      procedure OnContextReleased(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context); override;
      procedure OnUncaughtException(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context; const exception: ICefV8Exception; const stackTrace: ICefV8StackTrace); override;
      procedure OnFocusedNodeChanged(const browser: ICefBrowser; const frame: ICefFrame; const node: ICefDomNode); override;
      function  OnProcessMessageReceived(const browser: ICefBrowser; sourceProcess: TCefProcessId; const aMessage : ICefProcessMessage): Boolean; override;

    public
      constructor Create(const aCefApp : TCefApplication); reintroduce;
      destructor  Destroy; override;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFConstants;

procedure cef_render_process_handler_on_render_thread_created(self       : PCefRenderProcessHandler;
                                                              extra_info : PCefListValue); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefRenderProcessHandlerOwn) then
    TCefRenderProcessHandlerOwn(TempObject).OnRenderThreadCreated(TCefListValueRef.UnWrap(extra_info));
end;

procedure cef_render_process_handler_on_web_kit_initialized(self: PCefRenderProcessHandler); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefRenderProcessHandlerOwn) then
    TCefRenderProcessHandlerOwn(TempObject).OnWebKitInitialized;
end;

procedure cef_render_process_handler_on_browser_created(self    : PCefRenderProcessHandler;
                                                        browser : PCefBrowser); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefRenderProcessHandlerOwn) then
    TCefRenderProcessHandlerOwn(TempObject).OnBrowserCreated(TCefBrowserRef.UnWrap(browser));
end;

procedure cef_render_process_handler_on_browser_destroyed(self    : PCefRenderProcessHandler;
                                                          browser : PCefBrowser); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefRenderProcessHandlerOwn) then
    TCefRenderProcessHandlerOwn(TempObject).OnBrowserDestroyed(TCefBrowserRef.UnWrap(browser));
end;

function cef_render_process_handler_get_load_handler(self: PCefRenderProcessHandler): PCefLoadHandler; stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefRenderProcessHandlerOwn) then
    Result := CefGetData(TCefRenderProcessHandlerOwn(TempObject).GetLoadHandler)
   else
    Result := nil;
end;

function cef_render_process_handler_on_before_navigation(self            : PCefRenderProcessHandler;
                                                         browser         : PCefBrowser;
                                                         frame           : PCefFrame;
                                                         request         : PCefRequest;
                                                         navigation_type : TCefNavigationType;
                                                         is_redirect     : Integer): Integer; stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefRenderProcessHandlerOwn) then
    Result := Ord(TCefRenderProcessHandlerOwn(TempObject).OnBeforeNavigation(TCefBrowserRef.UnWrap(browser),
                                                                             TCefFrameRef.UnWrap(frame),
                                                                             TCefRequestRef.UnWrap(request),
                                                                             navigation_type,
                                                                             is_redirect <> 0))
   else
    Result := 0;
end;

procedure cef_render_process_handler_on_context_created(self    : PCefRenderProcessHandler;
                                                        browser : PCefBrowser;
                                                        frame   : PCefFrame;
                                                        context : PCefv8Context); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefRenderProcessHandlerOwn) then
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

  if (TempObject <> nil) and (TempObject is TCefRenderProcessHandlerOwn) then
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

  if (TempObject <> nil) and (TempObject is TCefRenderProcessHandlerOwn) then
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

  if (TempObject <> nil) and (TempObject is TCefRenderProcessHandlerOwn) then
    TCefRenderProcessHandlerOwn(TempObject).OnFocusedNodeChanged(TCefBrowserRef.UnWrap(browser),
                                                                 TCefFrameRef.UnWrap(frame),
                                                                 TCefDomNodeRef.UnWrap(node));
end;

function cef_render_process_handler_on_process_message_received(self           : PCefRenderProcessHandler;
                                                                browser        : PCefBrowser;
                                                                source_process : TCefProcessId;
                                                                message_       : PCefProcessMessage): Integer; stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefRenderProcessHandlerOwn) then
    Result := Ord(TCefRenderProcessHandlerOwn(TempObject).OnProcessMessageReceived(TCefBrowserRef.UnWrap(browser),
                                                                                   source_process,
                                                                                   TCefProcessMessageRef.UnWrap(message_)))
   else
    Result := 0;
end;


// TCefRenderProcessHandlerOwn


constructor TCefRenderProcessHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefRenderProcessHandler));

  with PCefRenderProcessHandler(FData)^ do
    begin
      on_render_thread_created    := cef_render_process_handler_on_render_thread_created;
      on_web_kit_initialized      := cef_render_process_handler_on_web_kit_initialized;
      on_browser_created          := cef_render_process_handler_on_browser_created;
      on_browser_destroyed        := cef_render_process_handler_on_browser_destroyed;
      get_load_handler            := cef_render_process_handler_get_load_handler;
      on_before_navigation        := cef_render_process_handler_on_before_navigation;
      on_context_created          := cef_render_process_handler_on_context_created;
      on_context_released         := cef_render_process_handler_on_context_released;
      on_uncaught_exception       := cef_render_process_handler_on_uncaught_exception;
      on_focused_node_changed     := cef_render_process_handler_on_focused_node_changed;
      on_process_message_received := cef_render_process_handler_on_process_message_received;
    end;
end;

function TCefRenderProcessHandlerOwn.GetLoadHandler: ICefLoadHandler;
begin
  Result := nil;
end;

function TCefRenderProcessHandlerOwn.OnBeforeNavigation(const browser        : ICefBrowser;
                                                        const frame          : ICefFrame;
                                                        const request        : ICefRequest;
                                                              navigationType : TCefNavigationType;
                                                              isRedirect     : Boolean): Boolean;
begin
  Result := False;
end;

function TCefRenderProcessHandlerOwn.OnProcessMessageReceived(const browser       : ICefBrowser;
                                                                    sourceProcess : TCefProcessId;
                                                              const aMessage      : ICefProcessMessage): Boolean;
begin
  Result := False;
end;


// TCefCustomRenderProcessHandler


constructor TCefCustomRenderProcessHandler.Create(const aCefApp : TCefApplication);
begin
  inherited Create;

  FCefApp := aCefApp;
end;

destructor TCefCustomRenderProcessHandler.Destroy;
begin
  FCefApp := nil;

  inherited Destroy;
end;

procedure TCefCustomRenderProcessHandler.OnRenderThreadCreated(const extraInfo: ICefListValue);
begin
  if (FCefApp <> nil) then FCefApp.Internal_OnRenderThreadCreated(extraInfo);
end;

procedure TCefCustomRenderProcessHandler.OnWebKitInitialized;
begin
  if (FCefApp <> nil) then FCefApp.Internal_OnWebKitInitialized;
end;

procedure TCefCustomRenderProcessHandler.OnBrowserCreated(const browser: ICefBrowser);
begin
  if (FCefApp <> nil) then FCefApp.Internal_OnBrowserCreated(browser);
end;

procedure TCefCustomRenderProcessHandler.OnBrowserDestroyed(const browser: ICefBrowser);
begin
  if (FCefApp <> nil) then FCefApp.Internal_OnBrowserDestroyed(browser);
end;

function  TCefCustomRenderProcessHandler.OnBeforeNavigation(const browser        : ICefBrowser;
                                                            const frame          : ICefFrame;
                                                            const request        : ICefRequest;
                                                                  navigationType : TCefNavigationType;
                                                                  isRedirect     : Boolean): Boolean;
begin
  Result := inherited OnBeforeNavigation(browser, frame, request, navigationType, isRedirect);

  if (FCefApp <> nil) then FCefApp.Internal_OnBeforeNavigation(browser, frame, request, navigationType, isRedirect, Result);
end;

procedure TCefCustomRenderProcessHandler.OnContextCreated(const browser : ICefBrowser;
                                                          const frame   : ICefFrame;
                                                          const context : ICefv8Context);
begin
  if (FCefApp <> nil) then FCefApp.Internal_OnContextCreated(browser, frame, context);
end;

procedure TCefCustomRenderProcessHandler.OnContextReleased(const browser : ICefBrowser;
                                                           const frame   : ICefFrame;
                                                           const context : ICefv8Context);
begin
  if (FCefApp <> nil) then FCefApp.Internal_OnContextReleased(browser, frame, context);
end;

procedure TCefCustomRenderProcessHandler.OnUncaughtException(const browser    : ICefBrowser;
                                                             const frame      : ICefFrame;
                                                             const context    : ICefv8Context;
                                                             const exception  : ICefV8Exception;
                                                             const stackTrace : ICefV8StackTrace);
begin
  if (FCefApp <> nil) then FCefApp.Internal_OnUncaughtException(browser, frame, context, exception, stackTrace);
end;

procedure TCefCustomRenderProcessHandler.OnFocusedNodeChanged(const browser : ICefBrowser;
                                                              const frame   : ICefFrame;
                                                              const node    : ICefDomNode);
begin
  if (FCefApp <> nil) then FCefApp.Internal_OnFocusedNodeChanged(browser, frame, node);
end;

function  TCefCustomRenderProcessHandler.OnProcessMessageReceived(const browser       : ICefBrowser;
                                                                        sourceProcess : TCefProcessId;
                                                                  const aMessage      : ICefProcessMessage): Boolean;
begin
  Result := inherited OnProcessMessageReceived(browser, sourceProcess, aMessage);

  if (FCefApp <> nil) then FCefApp.Internal_OnProcessMessageReceived(browser, sourceProcess, aMessage, Result);
end;

end.
