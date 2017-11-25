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

unit uCEFRequestContextHandler;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefRequestContextHandlerProc = {$IFDEF DELPHI12_UP}reference to{$ENDIF} function: ICefCookieManager;

  TCefRequestContextHandlerRef = class(TCefBaseRefCountedRef, ICefRequestContextHandler)
    protected
      procedure OnRequestContextInitialized(const request_context: ICefRequestContext);
      function  GetCookieManager: ICefCookieManager;
      function  OnBeforePluginLoad(const mimeType, pluginUrl: ustring; isMainFrame : boolean; const topOriginUrl: ustring; const pluginInfo: ICefWebPluginInfo; pluginPolicy: PCefPluginPolicy): Boolean;

    public
      class function UnWrap(data: Pointer): ICefRequestContextHandler;
  end;

  TCefRequestContextHandlerOwn = class(TCefBaseRefCountedOwn, ICefRequestContextHandler)
    protected
      procedure OnRequestContextInitialized(const request_context: ICefRequestContext);
      function  GetCookieManager: ICefCookieManager; virtual;
      function  OnBeforePluginLoad(const mimeType, pluginUrl: ustring; isMainFrame : boolean; const topOriginUrl: ustring; const pluginInfo: ICefWebPluginInfo; pluginPolicy: PCefPluginPolicy): Boolean; virtual;

    public
      constructor Create; virtual;
  end;

  TCefFastRequestContextHandler = class(TCefRequestContextHandlerOwn)
    protected
      FProc: TCefRequestContextHandlerProc;

      function GetCookieManager: ICefCookieManager; override;

    public
      constructor Create(const proc: TCefRequestContextHandlerProc); reintroduce;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFCookieManager, uCEFWebPluginInfo, uCEFRequestContext;

// TCefRequestContextHandlerOwn

procedure cef_request_context_handler_on_request_context_initialized(self: PCefRequestContextHandler; request_context: PCefRequestContext); stdcall;
begin
  TCefRequestContextHandlerOwn(CefGetObject(self)).OnRequestContextInitialized(TCefRequestContextRef.UnWrap(request_context));
end;

function cef_request_context_handler_get_cookie_manager(self: PCefRequestContextHandler): PCefCookieManager; stdcall;
begin
  Result := CefGetData(TCefRequestContextHandlerOwn(CefGetObject(self)).GetCookieManager());
end;

function cef_request_context_handler_on_before_plugin_load(self: PCefRequestContextHandler;
                                                           const mime_type, plugin_url : PCefString;
                                                                 is_main_frame : integer;
                                                           const top_origin_url: PCefString;
                                                                 plugin_info: PCefWebPluginInfo;
                                                                 plugin_policy: PCefPluginPolicy): Integer; stdcall;
begin
  with TCefRequestContextHandlerOwn(CefGetObject(self)) do
    Result := Ord(OnBeforePluginLoad(CefString(mime_type),
                                     CefString(plugin_url),
                                     (is_main_frame <> 0),
                                     CefString(top_origin_url),
                                     TCefWebPluginInfoRef.UnWrap(plugin_info),
                                     plugin_policy));
end;

constructor TCefRequestContextHandlerOwn.Create;
begin
  CreateData(SizeOf(TCefRequestContextHandler));

  with PCefRequestContextHandler(FData)^ do
    begin
      on_request_context_initialized := cef_request_context_handler_on_request_context_initialized;
      get_cookie_manager             := cef_request_context_handler_get_cookie_manager;
      on_before_plugin_load          := cef_request_context_handler_on_before_plugin_load;
    end;
end;

procedure TCefRequestContextHandlerOwn.OnRequestContextInitialized(const request_context: ICefRequestContext);
begin

end;

function TCefRequestContextHandlerOwn.GetCookieManager: ICefCookieManager;
begin
  Result:= nil;
end;

function TCefRequestContextHandlerOwn.OnBeforePluginLoad(const mimeType, pluginUrl : ustring;
                                                               isMainFrame : boolean;
                                                         const topOriginUrl: ustring;
                                                         const pluginInfo: ICefWebPluginInfo;
                                                               pluginPolicy: PCefPluginPolicy): Boolean;
begin
  Result := False;
end;

// TCefRequestContextHandlerRef

procedure TCefRequestContextHandlerRef.OnRequestContextInitialized(const request_context: ICefRequestContext);
begin
  PCefRequestContextHandler(FData).on_request_context_initialized(FData, CefGetData(request_context));
end;

function TCefRequestContextHandlerRef.GetCookieManager: ICefCookieManager;
begin
  Result := TCefCookieManagerRef.UnWrap(PCefRequestContextHandler(FData).get_cookie_manager(FData));
end;

function TCefRequestContextHandlerRef.OnBeforePluginLoad(const mimeType, pluginUrl : ustring;
                                                               isMainFrame : boolean;
                                                         const topOriginUrl: ustring;
                                                         const pluginInfo: ICefWebPluginInfo;
                                                               pluginPolicy: PCefPluginPolicy): Boolean;
var
  mt, pu, ou: TCefString;
begin
  mt := CefString(mimeType);
  pu := CefString(pluginUrl);
  ou := CefString(topOriginUrl);

  Result := PCefRequestContextHandler(FData).on_before_plugin_load(FData, @mt, @pu, ord(isMainFrame), @ou, CefGetData(pluginInfo), pluginPolicy) <> 0;
end;

class function TCefRequestContextHandlerRef.UnWrap(data: Pointer): ICefRequestContextHandler;
begin
  if (data <> nil) then
    Result := Create(data) as ICefRequestContextHandler
   else
    Result := nil;
end;

// TCefFastRequestContextHandler

constructor TCefFastRequestContextHandler.Create(const proc: TCefRequestContextHandlerProc);
begin
  FProc := proc;
  inherited Create;
end;

function TCefFastRequestContextHandler.GetCookieManager: ICefCookieManager;
begin
  Result := FProc();
end;

end.
