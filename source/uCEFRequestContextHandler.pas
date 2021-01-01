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

unit uCEFRequestContextHandler;

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
  TCefRequestContextHandlerRef = class(TCefBaseRefCountedRef, ICefRequestContextHandler)
    protected
      procedure OnRequestContextInitialized(const request_context: ICefRequestContext);
      function  OnBeforePluginLoad(const mimeType, pluginUrl: ustring; isMainFrame : boolean; const topOriginUrl: ustring; const pluginInfo: ICefWebPluginInfo; var pluginPolicy: TCefPluginPolicy): Boolean;
      procedure GetResourceRequestHandler(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; is_navigation, is_download: boolean; const request_initiator: ustring; var disable_default_handling: boolean; var aResourceRequestHandler : ICefResourceRequestHandler);

      procedure RemoveReferences; virtual;

    public
      class function UnWrap(data: Pointer): ICefRequestContextHandler;
  end;

  TCefRequestContextHandlerOwn = class(TCefBaseRefCountedOwn, ICefRequestContextHandler)
    protected
      procedure OnRequestContextInitialized(const request_context: ICefRequestContext); virtual;
      function  OnBeforePluginLoad(const mimeType, pluginUrl: ustring; isMainFrame : boolean; const topOriginUrl: ustring; const pluginInfo: ICefWebPluginInfo; var pluginPolicy: TCefPluginPolicy): Boolean; virtual;
      procedure GetResourceRequestHandler(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; is_navigation, is_download: boolean; const request_initiator: ustring; var disable_default_handling: boolean; var aResourceRequestHandler : ICefResourceRequestHandler); virtual;

      procedure RemoveReferences; virtual;

    public
      constructor Create; virtual;
  end;

  TCustomRequestContextHandler = class(TCefRequestContextHandlerOwn)
    protected
      FEvents : Pointer;

      procedure OnRequestContextInitialized(const request_context: ICefRequestContext); override;
      function  OnBeforePluginLoad(const mimeType, pluginUrl: ustring; isMainFrame : boolean; const topOriginUrl: ustring; const pluginInfo: ICefWebPluginInfo; var pluginPolicy: TCefPluginPolicy): Boolean; override;
      procedure GetResourceRequestHandler(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; is_navigation, is_download: boolean; const request_initiator: ustring; var disable_default_handling: boolean; var aResourceRequestHandler : ICefResourceRequestHandler); override;

    public
      constructor Create(const events : IChromiumEvents); reintroduce; virtual;
      procedure   BeforeDestruction; override;
      procedure   RemoveReferences; override;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBrowser, uCEFFrame, uCEFRequest,
  uCEFWebPluginInfo, uCEFRequestContext, uCEFResourceRequestHandler;

// TCefRequestContextHandlerOwn

procedure cef_request_context_handler_on_request_context_initialized(self            : PCefRequestContextHandler;
                                                                     request_context : PCefRequestContext); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefRequestContextHandlerOwn) then
    TCefRequestContextHandlerOwn(TempObject).OnRequestContextInitialized(TCefRequestContextRef.UnWrap(request_context));
end;

function cef_request_context_handler_on_before_plugin_load(      self           : PCefRequestContextHandler;
                                                           const mime_type      : PCefString;
                                                           const plugin_url     : PCefString;
                                                                 is_main_frame  : integer;
                                                           const top_origin_url : PCefString;
                                                                 plugin_info    : PCefWebPluginInfo;
                                                                 plugin_policy  : PCefPluginPolicy): Integer; stdcall;
var
  TempObject : TObject;
  TempPolicy : TCefPluginPolicy;
begin
  Result     := Ord(False);
  TempObject := CefGetObject(self);
  TempPolicy := plugin_policy^;

  if (TempObject <> nil) and (TempObject is TCefRequestContextHandlerOwn) then
    Result := Ord(TCefRequestContextHandlerOwn(TempObject).OnBeforePluginLoad(CefString(mime_type),
                                                                              CefString(plugin_url),
                                                                              (is_main_frame <> 0),
                                                                              CefString(top_origin_url),
                                                                              TCefWebPluginInfoRef.UnWrap(plugin_info),
                                                                              TempPolicy));
  plugin_policy^ := TempPolicy;
end;

function cef_request_context_handler_get_resource_request_handler(      self                     : PCefRequestContextHandler;
                                                                        browser                  : PCefBrowser;
                                                                        frame                    : PCefFrame;
                                                                        request                  : PCefRequest;
                                                                        is_navigation            : Integer;
                                                                        is_download              : Integer;
                                                                  const request_initiator        : PCefString;
                                                                        disable_default_handling : PInteger): PCefResourceRequestHandler; stdcall;
var
  TempObject : TObject;
  TempDisableDefHandling : Boolean;
  TempResourceRequestHandler : ICefResourceRequestHandler;
begin
  Result                     := nil;
  TempResourceRequestHandler := nil;
  TempObject                 := CefGetObject(self);
  TempDisableDefHandling     := disable_default_handling^ <> 0;

  if (TempObject <> nil) and (TempObject is TCefRequestContextHandlerOwn) then
    try
      TCefRequestContextHandlerOwn(TempObject).GetResourceRequestHandler(TCefBrowserRef.UnWrap(browser),
                                                                         TCefFrameRef.UnWrap(frame),
                                                                         TCefRequestRef.UnWrap(request),
                                                                         is_navigation <> 0,
                                                                         is_download <> 0,
                                                                         CefString(request_initiator),
                                                                         TempDisableDefHandling,
                                                                         TempResourceRequestHandler);

      Result                     := CefGetData(TempResourceRequestHandler);
      disable_default_handling^  := Ord(TempDisableDefHandling);
    finally
      TempResourceRequestHandler := nil;
    end;
end;

constructor TCefRequestContextHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefRequestContextHandler));

  with PCefRequestContextHandler(FData)^ do
    begin
      on_request_context_initialized := {$IFDEF FPC}@{$ENDIF}cef_request_context_handler_on_request_context_initialized;
      on_before_plugin_load          := {$IFDEF FPC}@{$ENDIF}cef_request_context_handler_on_before_plugin_load;
      get_resource_request_handler   := {$IFDEF FPC}@{$ENDIF}cef_request_context_handler_get_resource_request_handler;
    end;
end;

procedure TCefRequestContextHandlerOwn.OnRequestContextInitialized(const request_context: ICefRequestContext);
begin
  //
end;

function TCefRequestContextHandlerOwn.OnBeforePluginLoad(const mimeType     : ustring;
                                                         const pluginUrl    : ustring;
                                                               isMainFrame  : boolean;
                                                         const topOriginUrl : ustring;
                                                         const pluginInfo   : ICefWebPluginInfo;
                                                         var   pluginPolicy : TCefPluginPolicy): Boolean;
begin
  Result := False;
end;

procedure TCefRequestContextHandlerOwn.GetResourceRequestHandler(const browser                  : ICefBrowser;
                                                                 const frame                    : ICefFrame;
                                                                 const request                  : ICefRequest;
                                                                       is_navigation            : boolean;
                                                                       is_download              : boolean;
                                                                 const request_initiator        : ustring;
                                                                 var   disable_default_handling : boolean;
                                                                 var   aResourceRequestHandler  : ICefResourceRequestHandler);
begin
  aResourceRequestHandler := nil;
end;

procedure TCefRequestContextHandlerOwn.RemoveReferences;
begin
  //
end;


// TCefRequestContextHandlerRef

procedure TCefRequestContextHandlerRef.OnRequestContextInitialized(const request_context: ICefRequestContext);
begin
  PCefRequestContextHandler(FData)^.on_request_context_initialized(PCefRequestContextHandler(FData), CefGetData(request_context));
end;

function TCefRequestContextHandlerRef.OnBeforePluginLoad(const mimeType     : ustring;
                                                         const pluginUrl    : ustring;
                                                               isMainFrame  : boolean;
                                                         const topOriginUrl : ustring;
                                                         const pluginInfo   : ICefWebPluginInfo;
                                                         var   pluginPolicy : TCefPluginPolicy): Boolean;
var
  TempType, TempPluginURL, TempOriginURL : TCefString;
begin
  TempType      := CefString(mimeType);
  TempPluginURL := CefString(pluginUrl);
  TempOriginURL := CefString(topOriginUrl);

  Result := PCefRequestContextHandler(FData)^.on_before_plugin_load(PCefRequestContextHandler(FData),
                                                                    @TempType,
                                                                    @TempPluginURL,
                                                                    ord(isMainFrame),
                                                                    @TempOriginURL,
                                                                    CefGetData(pluginInfo),
                                                                    @pluginPolicy) <> 0;
end;


procedure TCefRequestContextHandlerRef.GetResourceRequestHandler(const browser                  : ICefBrowser;
                                                                 const frame                    : ICefFrame;
                                                                 const request                  : ICefRequest;
                                                                       is_navigation            : boolean;
                                                                       is_download              : boolean;
                                                                 const request_initiator        : ustring;
                                                                 var   disable_default_handling : boolean;
                                                                 var   aResourceRequestHandler  : ICefResourceRequestHandler);
var
  TempRequestInitiator       : TCefString;
  TempDisableDefaultHandling : integer;
  TempResourceRequestHandler : PCefResourceRequestHandler;
begin
  TempRequestInitiator       := CefString(request_initiator);
  TempDisableDefaultHandling := ord(disable_default_handling);
  TempResourceRequestHandler := PCefRequestContextHandler(FData)^.get_resource_request_handler(PCefRequestContextHandler(FData),
                                                                                               CefGetData(browser),
                                                                                               CefGetData(frame),
                                                                                               CefGetData(request),
                                                                                               ord(is_navigation),
                                                                                               ord(is_download),
                                                                                               @TempRequestInitiator,
                                                                                               @TempDisableDefaultHandling);

  disable_default_handling := TempDisableDefaultHandling <> 0;

  if (TempResourceRequestHandler <> nil) then
    aResourceRequestHandler := TCefResourceRequestHandlerRef.UnWrap(TempResourceRequestHandler)
   else
    aResourceRequestHandler := nil;
end;

procedure TCefRequestContextHandlerRef.RemoveReferences;
begin
  //
end;

class function TCefRequestContextHandlerRef.UnWrap(data: Pointer): ICefRequestContextHandler;
begin
  if (data <> nil) then
    Result := Create(data) as ICefRequestContextHandler
   else
    Result := nil;
end;


// TCustomRequestContextHandler

constructor TCustomRequestContextHandler.Create(const events : IChromiumEvents);
begin
  inherited Create;

  FEvents := Pointer(events);
end;

procedure TCustomRequestContextHandler.BeforeDestruction;
begin
  FEvents := nil;

  inherited BeforeDestruction;
end;

procedure TCustomRequestContextHandler.RemoveReferences;
begin
  FEvents := nil;
end;

procedure TCustomRequestContextHandler.OnRequestContextInitialized(const request_context: ICefRequestContext);
begin
  if (FEvents <> nil) then
    IChromiumEvents(FEvents).doOnRequestContextInitialized(request_context)
   else
    inherited OnRequestContextInitialized(request_context);
end;

function TCustomRequestContextHandler.OnBeforePluginLoad(const mimeType     : ustring;
                                                         const pluginUrl    : ustring;
                                                               isMainFrame  : boolean;
                                                         const topOriginUrl : ustring;
                                                         const pluginInfo   : ICefWebPluginInfo;
                                                         var   pluginPolicy : TCefPluginPolicy): Boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnBeforePluginLoad(mimeType,
                                                            pluginUrl,
                                                            isMainFrame,
                                                            topOriginUrl,
                                                            pluginInfo,
                                                            pluginPolicy)
   else
    Result := inherited OnBeforePluginLoad(mimeType,
                                           pluginUrl,
                                           isMainFrame,
                                           topOriginUrl,
                                           pluginInfo,
                                           pluginPolicy);
end;

procedure TCustomRequestContextHandler.GetResourceRequestHandler(const browser                  : ICefBrowser;
                                                                 const frame                    : ICefFrame;
                                                                 const request                  : ICefRequest;
                                                                       is_navigation            : boolean;
                                                                       is_download              : boolean;
                                                                 const request_initiator        : ustring;
                                                                 var   disable_default_handling : boolean;
                                                                 var   aResourceRequestHandler  : ICefResourceRequestHandler);
begin
  if (FEvents <> nil) then
    IChromiumEvents(FEvents).doGetResourceRequestHandler_ReqCtxHdlr(browser,
                                                                    frame,
                                                                    request,
                                                                    is_navigation,
                                                                    is_download,
                                                                    request_initiator,
                                                                    disable_default_handling,
                                                                    aResourceRequestHandler)
   else
    inherited GetResourceRequestHandler(browser,
                                        frame,
                                        request,
                                        is_navigation,
                                        is_download,
                                        request_initiator,
                                        disable_default_handling,
                                        aResourceRequestHandler);
end;


end.
