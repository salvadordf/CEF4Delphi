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

unit uCEFRequestHandler;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefRequestHandlerOwn = class(TCefBaseRefCountedOwn, ICefRequestHandler)
    protected
      function  OnBeforeBrowse(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; isRedirect: Boolean): Boolean; virtual;
      function  OnOpenUrlFromTab(const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean): Boolean; virtual;
      function  OnBeforeResourceLoad(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const callback: ICefRequestCallback): TCefReturnValue; virtual;
      function  GetResourceHandler(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest): ICefResourceHandler; virtual;
      procedure OnResourceRedirect(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; var newUrl: ustring); virtual;
      function  OnResourceResponse(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse): Boolean; virtual;
      function  GetResourceResponseFilter(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse): ICefResponseFilter; virtual;
      procedure OnResourceLoadComplete(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; status: TCefUrlRequestStatus; receivedContentLength: Int64); virtual;
      function  GetAuthCredentials(const browser: ICefBrowser; const frame: ICefFrame; isProxy: Boolean; const host: ustring; port: Integer; const realm, scheme: ustring; const callback: ICefAuthCallback): Boolean; virtual;
      function  OnQuotaRequest(const browser: ICefBrowser; const originUrl: ustring; newSize: Int64; const callback: ICefRequestCallback): Boolean; virtual;
      function  GetCookieManager(const browser: ICefBrowser; const mainUrl: ustring): ICefCookieManager; virtual;
      procedure OnProtocolExecution(const browser: ICefBrowser; const url: ustring; out allowOsExecution: Boolean); virtual;
      function  OnCertificateError(const browser: ICefBrowser; certError: TCefErrorcode; const requestUrl: ustring; const sslInfo: ICefSslInfo; const callback: ICefRequestCallback): Boolean; virtual;
      function  OnSelectClientCertificate(const browser: ICefBrowser; isProxy: boolean; const host: ustring; port: integer; certificatesCount: NativeUInt; const certificates: TCefX509CertificateArray; const callback: ICefSelectClientCertificateCallback): boolean; virtual;
      procedure OnPluginCrashed(const browser: ICefBrowser; const pluginPath: ustring); virtual;
      procedure OnRenderViewReady(const browser: ICefBrowser); virtual;
      procedure OnRenderProcessTerminated(const browser: ICefBrowser; status: TCefTerminationStatus); virtual;

    public
      constructor Create; virtual;
  end;

  TCustomRequestHandler = class(TCefRequestHandlerOwn)
    protected
      FEvent: IChromiumEvents;

      function  OnBeforeBrowse(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; isRedirect: Boolean): Boolean; override;
      function  OnOpenUrlFromTab(const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean): Boolean; override;
      function  OnBeforeResourceLoad(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const callback: ICefRequestCallback): TCefReturnValue; override;
      function  GetResourceHandler(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest): ICefResourceHandler; override;
      procedure OnResourceRedirect(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; var newUrl: ustring); override;
      function  OnResourceResponse(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse): Boolean; override;
      function  GetResourceResponseFilter(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse): ICefResponseFilter; override;
      procedure OnResourceLoadComplete(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; status: TCefUrlRequestStatus; receivedContentLength: Int64); override;
      function  GetAuthCredentials(const browser: ICefBrowser; const frame: ICefFrame; isProxy: Boolean; const host: ustring; port: Integer; const realm, scheme: ustring; const callback: ICefAuthCallback): Boolean; override;
      function  OnQuotaRequest(const browser: ICefBrowser; const originUrl: ustring; newSize: Int64; const callback: ICefRequestCallback): Boolean; override;
      procedure OnProtocolExecution(const browser: ICefBrowser; const url: ustring; out allowOsExecution: Boolean); override;
      function  OnCertificateError(const browser: ICefBrowser; certError: TCefErrorcode; const requestUrl: ustring; const sslInfo: ICefSslInfo; const callback: ICefRequestCallback): Boolean; override;
      function  OnSelectClientCertificate(const browser: ICefBrowser; isProxy: boolean; const host: ustring; port: integer; certificatesCount: NativeUInt; const certificates: TCefX509CertificateArray; const callback: ICefSelectClientCertificateCallback): boolean; override;
      procedure OnPluginCrashed(const browser: ICefBrowser; const pluginPath: ustring); override;
      procedure OnRenderViewReady(const browser: ICefBrowser); override;
      procedure OnRenderProcessTerminated(const browser: ICefBrowser; status: TCefTerminationStatus); override;

    public
      constructor Create(const events: IChromiumEvents); reintroduce; virtual;
      destructor  Destroy; override;
  end;

implementation

uses
  {$IFDEF DELPHI16_UP}
  WinApi.Windows, System.SysUtils,
  {$ELSE}
  Windows, SysUtils,
  {$ENDIF}
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBrowser, uCEFFrame, uCEFRequest, uCEFRequestCallback,
  uCEFResponse, uCEFAuthCallback, uCEFSslInfo, uCEFSelectClientCertificateCallback, uCEFX509Certificate,
  uCEFApplication;

function cef_request_handler_on_before_browse(self: PCefRequestHandler; browser: PCefBrowser;
  frame: PCefFrame; request: PCefRequest; isRedirect: Integer): Integer; stdcall;
begin
  with TCefRequestHandlerOwn(CefGetObject(self)) do
    Result := Ord(OnBeforeBrowse(TCefBrowserRef.UnWrap(browser), TCefFrameRef.UnWrap(frame),
      TCefRequestRef.UnWrap(request), isRedirect <> 0));
end;

function cef_request_handler_on_open_urlfrom_tab(self: PCefRequestHandler; browser: PCefBrowser;
  frame: PCefFrame; const target_url: PCefString; target_disposition: TCefWindowOpenDisposition;
  user_gesture: Integer): Integer; stdcall;
begin
  with TCefRequestHandlerOwn(CefGetObject(self)) do
    Result := Ord(OnOpenUrlFromTab(TCefBrowserRef.UnWrap(browser), TCefFrameRef.UnWrap(frame),
      CefString(target_url), target_disposition, user_gesture <> 0));
end;

function cef_request_handler_on_before_resource_load(self: PCefRequestHandler;
  browser: PCefBrowser; frame: PCefFrame; request: PCefRequest;
  callback: PCefRequestCallback): TCefReturnValue; stdcall;
begin
  with TCefRequestHandlerOwn(CefGetObject(self)) do
    Result := OnBeforeResourceLoad(
      TCefBrowserRef.UnWrap(browser),
      TCefFrameRef.UnWrap(frame),
      TCefRequestRef.UnWrap(request),
      TcefRequestCallbackRef.UnWrap(callback));
end;

function cef_request_handler_get_resource_handler(self: PCefRequestHandler;
  browser: PCefBrowser; frame: PCefFrame; request: PCefRequest): PCefResourceHandler; stdcall;
begin
  with TCefRequestHandlerOwn(CefGetObject(self)) do
    Result := CefGetData(GetResourceHandler(TCefBrowserRef.UnWrap(browser),
      TCefFrameRef.UnWrap(frame), TCefRequestRef.UnWrap(request)));
end;

procedure cef_request_handler_on_resource_redirect(self: PCefRequestHandler;
  browser: PCefBrowser; frame: PCefFrame; const request: PCefRequest; response: PCefResponse; new_url: PCefString); stdcall;
var
  url: ustring;
begin
  url := CefString(new_url);
  with TCefRequestHandlerOwn(CefGetObject(self)) do
    OnResourceRedirect(TCefBrowserRef.UnWrap(browser), TCefFrameRef.UnWrap(frame),
      TCefRequestRef.UnWrap(request), TCefResponseRef.UnWrap(response), url);
  if url <> '' then
    CefStringSet(new_url, url);
end;

function cef_request_handler_on_resource_response(self: PCefRequestHandler;
  browser: PCefBrowser; frame: PCefFrame; request: PCefRequest;
  response: PCefResponse): Integer; stdcall;
begin
  with TCefRequestHandlerOwn(CefGetObject(self)) do
    Result := Ord(OnResourceResponse(TCefBrowserRef.UnWrap(browser), TCefFrameRef.UnWrap(frame),
      TCefRequestRef.UnWrap(request), TCefResponseRef.UnWrap(response)));
end;

function cef_request_handler_get_resource_response_filter(self: PCefRequestHandler; browser: PCefBrowser;
  frame: PCefFrame; request: PCefRequest; response: PCefResponse): PCefResponseFilter; stdcall;
begin
   with TCefRequestHandlerOwn(CefGetObject(self)) do
     Result := CefGetData(GetResourceResponseFilter(TCefBrowserRef.UnWrap(browser),
      TCefFrameRef.UnWrap(frame), TCefRequestRef.UnWrap(request),
      TCefResponseRef.UnWrap(response)));
end;

procedure cef_request_handler_on_resource_load_complete(self: PCefRequestHandler; browser: PCefBrowser;
  frame: PCefFrame; request: PCefRequest; response: PCefResponse;
  status: TCefUrlRequestStatus; received_content_length: Int64); stdcall;
begin
   with TCefRequestHandlerOwn(CefGetObject(self)) do
     OnResourceLoadComplete(TCefBrowserRef.UnWrap(browser),
      TCefFrameRef.UnWrap(frame), TCefRequestRef.UnWrap(request),
      TCefResponseRef.UnWrap(response), status, received_content_length);
end;

function cef_request_handler_get_auth_credentials(self: PCefRequestHandler;
  browser: PCefBrowser; frame: PCefFrame; isProxy: Integer; const host: PCefString;
  port: Integer; const realm, scheme: PCefString; callback: PCefAuthCallback): Integer; stdcall;
begin
  with TCefRequestHandlerOwn(CefGetObject(self)) do
    Result := Ord(GetAuthCredentials(
      TCefBrowserRef.UnWrap(browser), TCefFrameRef.UnWrap(frame), isProxy <> 0,
      CefString(host), port, CefString(realm), CefString(scheme), TCefAuthCallbackRef.UnWrap(callback)));
end;

function cef_request_handler_on_quota_request(self: PCefRequestHandler; browser: PCefBrowser;
  const origin_url: PCefString; new_size: Int64; callback: PCefRequestCallback): Integer; stdcall;
begin
  with TCefRequestHandlerOwn(CefGetObject(self)) do
    Result := Ord(OnQuotaRequest(TCefBrowserRef.UnWrap(browser),
      CefString(origin_url), new_size, TCefRequestCallbackRef.UnWrap(callback)));
end;

procedure cef_request_handler_on_protocol_execution(self: PCefRequestHandler;
  browser: PCefBrowser; const url: PCefString; allow_os_execution: PInteger); stdcall;
var
  allow: Boolean;
begin
  allow := allow_os_execution^ <> 0;
  with TCefRequestHandlerOwn(CefGetObject(self)) do
    OnProtocolExecution(
      TCefBrowserRef.UnWrap(browser),
      CefString(url), allow);
  allow_os_execution^ := Ord(allow);
end;

function cef_request_handler_on_certificate_error(self: PCefRequestHandler;
  browser: PCefBrowser; cert_error: TCefErrorcode; const request_url: PCefString;
  ssl_info: PCefSslInfo; callback: PCefRequestCallback): Integer; stdcall;
begin
  with TCefRequestHandlerOwn(CefGetObject(self)) do
    Result := Ord(OnCertificateError(TCefBrowserRef.UnWrap(browser), cert_error,
      CefString(request_url), TCefSslInfoRef.UnWrap(ssl_info),
      TCefRequestCallbackRef.UnWrap(callback)));
end;

procedure cef_request_handler_on_plugin_crashed(self: PCefRequestHandler;
  browser: PCefBrowser; const plugin_path: PCefString); stdcall;
begin
  with TCefRequestHandlerOwn(CefGetObject(self)) do
    OnPluginCrashed(TCefBrowserRef.UnWrap(browser), CefString(plugin_path));
end;

procedure cef_request_handler_on_render_view_ready(self: PCefRequestHandler;
  browser: PCefBrowser); stdcall;
begin
  with TCefRequestHandlerOwn(CefGetObject(self)) do
    OnRenderViewReady(TCefBrowserRef.UnWrap(browser));
end;

procedure cef_request_handler_on_render_process_terminated(self: PCefRequestHandler;
  browser: PCefBrowser; status: TCefTerminationStatus); stdcall;
begin
  with TCefRequestHandlerOwn(CefGetObject(self)) do
    OnRenderProcessTerminated(TCefBrowserRef.UnWrap(browser), status);
end;

function cef_request_handler_on_select_client_certificate(self: PCefRequestHandler;
                                                          browser: PCefBrowser;
                                                          isProxy: integer;
                                                          const host: PCefString;
                                                          port: integer;
                                                          certificatesCount: NativeUInt;
                                                          const certificates: PPCefX509Certificate;
                                                          callback: PCefSelectClientCertificateCallback): integer; stdcall;
var
  TempCertArray : TCefX509CertificateArray;
  i : NativeUInt;
begin
  TempCertArray := nil;
  Result        := 0;

  try
    try
      if (certificatesCount > 0) and (certificates <> nil) then
        begin
          SetLength(TempCertArray, certificatesCount);

          i := 0;
          while (i < certificatesCount) do
            begin
              TempCertArray[i] := TCEFX509CertificateRef.UnWrap(PPointerArray(certificates)[i]);
              inc(i);
            end;

          with TCefRequestHandlerOwn(CefGetObject(self)) do
            Result := Ord(OnSelectClientCertificate(TCefBrowserRef.UnWrap(browser),
                                                    (isProxy <> 0),
                                                    CefString(host),
                                                    port,
                                                    certificatesCount,
                                                    TempCertArray,
                                                    TCefSelectClientCertificateCallbackRef.UnWrap(callback)));

          i := 0;
          while (i < certificatesCount) do
            begin
              TempCertArray[i] := nil;
              inc(i);
            end;
        end;
    except
      on e : exception do
        if CustomExceptionHandler('uCEFRequestHandler.cef_request_handler_on_select_client_certificate', e) then raise;
    end;
  finally
    if (TempCertArray <> nil) then
      begin
        Finalize(TempCertArray);
        TempCertArray := nil;
      end;
  end;
end;

constructor TCefRequestHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefRequestHandler));
  with PCefRequestHandler(FData)^ do
  begin
    on_before_browse := cef_request_handler_on_before_browse;
    on_open_urlfrom_tab := cef_request_handler_on_open_urlfrom_tab;
    on_before_resource_load := cef_request_handler_on_before_resource_load;
    get_resource_handler := cef_request_handler_get_resource_handler;
    on_resource_redirect := cef_request_handler_on_resource_redirect;
    on_resource_response := cef_request_handler_on_resource_response;
    get_resource_response_filter := cef_request_handler_get_resource_response_filter;
    on_resource_load_complete := cef_request_handler_on_resource_load_complete;
    get_auth_credentials := cef_request_handler_get_auth_credentials;
    on_quota_request := cef_request_handler_on_quota_request;
    on_protocol_execution := cef_request_handler_on_protocol_execution;
    on_certificate_error := cef_request_handler_on_certificate_error;
    on_select_client_certificate := cef_request_handler_on_select_client_certificate;
    on_plugin_crashed := cef_request_handler_on_plugin_crashed;
    on_render_view_ready := cef_request_handler_on_render_view_ready;
    on_render_process_terminated := cef_request_handler_on_render_process_terminated;
  end;
end;

function TCefRequestHandlerOwn.GetAuthCredentials(const browser: ICefBrowser; const frame: ICefFrame;
  isProxy: Boolean; const host: ustring; port: Integer; const realm, scheme: ustring;
  const callback: ICefAuthCallback): Boolean;
begin
  Result := False;
end;

function TCefRequestHandlerOwn.GetCookieManager(const browser: ICefBrowser;
  const mainUrl: ustring): ICefCookieManager;
begin
  Result := nil;
end;

function TCefRequestHandlerOwn.OnBeforeBrowse(const browser: ICefBrowser;
  const frame: ICefFrame; const request: ICefRequest;
  isRedirect: Boolean): Boolean;
begin
  Result := False;
end;

function TCefRequestHandlerOwn.OnBeforeResourceLoad(const browser: ICefBrowser;
  const frame: ICefFrame; const request: ICefRequest;
  const callback: ICefRequestCallback): TCefReturnValue;
begin
  Result := RV_CONTINUE;
end;

function TCefRequestHandlerOwn.OnCertificateError(const browser: ICefBrowser;
  certError: TCefErrorcode; const requestUrl: ustring; const sslInfo: ICefSslInfo;
  const callback: ICefRequestCallback): Boolean;
begin
  Result := False;
end;

function TCefRequestHandlerOwn.OnSelectClientCertificate(const browser           : ICefBrowser;
                                                               isProxy           : boolean;
                                                         const host              : ustring;
                                                               port              : integer;
                                                               certificatesCount : NativeUInt;
                                                         const certificates      : TCefX509CertificateArray;
                                                         const callback          : ICefSelectClientCertificateCallback): boolean;
begin
  Result := False;
end;

function TCefRequestHandlerOwn.OnOpenUrlFromTab(const browser: ICefBrowser;
  const frame: ICefFrame; const targetUrl: ustring;
  targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean): Boolean;
begin
  Result := False;
end;

function TCefRequestHandlerOwn.GetResourceHandler(const browser: ICefBrowser;
  const frame: ICefFrame; const request: ICefRequest): ICefResourceHandler;
begin
  Result := nil;
end;

procedure TCefRequestHandlerOwn.OnPluginCrashed(const browser: ICefBrowser;
  const pluginPath: ustring);
begin

end;

procedure TCefRequestHandlerOwn.OnProtocolExecution(const browser: ICefBrowser;
  const url: ustring; out allowOsExecution: Boolean);
begin

end;

function TCefRequestHandlerOwn.OnQuotaRequest(const browser: ICefBrowser;
  const originUrl: ustring; newSize: Int64;
  const callback: ICefRequestCallback): Boolean;
begin
  Result := False;
end;

procedure TCefRequestHandlerOwn.OnRenderProcessTerminated(
  const browser: ICefBrowser; status: TCefTerminationStatus);
begin

end;

procedure TCefRequestHandlerOwn.OnRenderViewReady(const browser: ICefBrowser);
begin

end;

procedure TCefRequestHandlerOwn.OnResourceRedirect(const browser: ICefBrowser;
  const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; var newUrl: ustring);
begin

end;

function TCefRequestHandlerOwn.OnResourceResponse(const browser: ICefBrowser;
  const frame: ICefFrame; const request: ICefRequest;
  const response: ICefResponse): Boolean;
begin
  Result := False;
end;

function TCefRequestHandlerOwn.GetResourceResponseFilter(
  const browser: ICefBrowser; const frame: ICefFrame;
  const request: ICefRequest; const response: ICefResponse): ICefResponseFilter;
begin
  Result := nil;
end;

procedure TCefRequestHandlerOwn.OnResourceLoadComplete(
  const browser: ICefBrowser; const frame: ICefFrame;
  const request: ICefRequest; const response: ICefResponse;
  status: TCefUrlRequestStatus; receivedContentLength: Int64);
begin

end;

// TCustomRequestHandler

constructor TCustomRequestHandler.Create(const events: IChromiumEvents);
begin
  inherited Create;

  FEvent := events;
end;

destructor TCustomRequestHandler.Destroy;
begin
  FEvent := nil;

  inherited Destroy;
end;

function TCustomRequestHandler.GetAuthCredentials(const browser  : ICefBrowser;
                                                  const frame    : ICefFrame;
                                                        isProxy  : Boolean;
                                                  const host     : ustring;
                                                        port     : Integer;
                                                  const realm    : ustring;
                                                  const scheme   : ustring;
                                                  const callback : ICefAuthCallback): Boolean;
begin
  if (FEvent <> nil) then
    Result := FEvent.doOnGetAuthCredentials(browser, frame, isProxy, host, port, realm, scheme, callback)
   else
    Result := inherited GetAuthCredentials(browser, frame, isProxy, host, port, realm, scheme, callback);
end;

function TCustomRequestHandler.GetResourceHandler(const browser : ICefBrowser;
                                                  const frame   : ICefFrame;
                                                  const request : ICefRequest): ICefResourceHandler;
begin
  if (FEvent <> nil) then
    Result := FEvent.doOnGetResourceHandler(browser, frame, request)
   else
    Result := inherited GetResourceHandler(browser, frame, request);
end;

function TCustomRequestHandler.OnBeforeBrowse(const browser    : ICefBrowser;
                                              const frame      : ICefFrame;
                                              const request    : ICefRequest;
                                                    isRedirect : Boolean): Boolean;
begin
  Result := FEvent.doOnBeforeBrowse(browser, frame, request, isRedirect);
end;

function TCustomRequestHandler.OnBeforeResourceLoad(const browser  : ICefBrowser;
                                                    const frame    : ICefFrame;
                                                    const request  : ICefRequest;
                                                    const callback : ICefRequestCallback): TCefReturnValue;
begin
  if (FEvent <> nil) then
    Result := FEvent.doOnBeforeResourceLoad(browser, frame, request, callback)
   else
    Result := inherited OnBeforeResourceLoad(browser, frame, request, callback);
end;

function TCustomRequestHandler.OnCertificateError(const browser    : ICefBrowser;
                                                        certError  : TCefErrorcode;
                                                  const requestUrl : ustring;
                                                  const sslInfo    : ICefSslInfo;
                                                  const callback   : ICefRequestCallback): Boolean;
begin
  if (FEvent <> nil) then
    Result := FEvent.doOnCertificateError(browser, certError, requestUrl, sslInfo, callback)
   else
    Result := inherited OnCertificateError(browser, certError, requestUrl, sslInfo, callback);
end;

function TCustomRequestHandler.OnOpenUrlFromTab(const browser           : ICefBrowser;
                                                const frame             : ICefFrame;
                                                const targetUrl         : ustring;
                                                      targetDisposition : TCefWindowOpenDisposition;
                                                      userGesture       : Boolean): Boolean;
begin
  if (FEvent <> nil) then
    Result := FEvent.doOnOpenUrlFromTab(browser, frame, targetUrl, targetDisposition, userGesture)
   else
    Result := inherited OnOpenUrlFromTab(browser, frame, targetUrl, targetDisposition, userGesture);
end;

function TCustomRequestHandler.OnSelectClientCertificate(const browser           : ICefBrowser;
                                                               isProxy           : boolean;
                                                         const host              : ustring;
                                                               port              : integer;
                                                               certificatesCount : NativeUInt;
                                                         const certificates      : TCefX509CertificateArray;
                                                         const callback          : ICefSelectClientCertificateCallback): boolean;
begin
  if (FEvent <> nil) then
    Result := FEvent.doOnSelectClientCertificate(browser, isProxy, host, port, certificatesCount, certificates, callback)
   else
    Result := inherited OnSelectClientCertificate(browser, isProxy, host, port, certificatesCount, certificates, callback);
end;

procedure TCustomRequestHandler.OnPluginCrashed(const browser: ICefBrowser; const pluginPath: ustring);
begin
  if (FEvent <> nil) then FEvent.doOnPluginCrashed(browser, pluginPath);
end;

procedure TCustomRequestHandler.OnProtocolExecution(const browser          : ICefBrowser;
                                                    const url              : ustring;
                                                      out allowOsExecution : Boolean);
begin
  if (FEvent <> nil) then FEvent.doOnProtocolExecution(browser, url, allowOsExecution);
end;

function TCustomRequestHandler.OnQuotaRequest(const browser   : ICefBrowser;
                                              const originUrl : ustring;
                                                    newSize   : Int64;
                                              const callback  : ICefRequestCallback): Boolean;
begin
  if (FEvent <> nil) then
    Result := FEvent.doOnQuotaRequest(browser, originUrl, newSize, callback)
   else
    Result := inherited OnQuotaRequest(browser, originUrl, newSize, callback);
end;

procedure TCustomRequestHandler.OnRenderProcessTerminated(const browser: ICefBrowser; status: TCefTerminationStatus);
begin
  if (FEvent <> nil) then FEvent.doOnRenderProcessTerminated(browser, status);
end;

procedure TCustomRequestHandler.OnRenderViewReady(const browser: ICefBrowser);
begin
  if (FEvent <> nil) then FEvent.doOnRenderViewReady(browser);
end;

procedure TCustomRequestHandler.OnResourceRedirect(const browser  : ICefBrowser;
                                                   const frame    : ICefFrame;
                                                   const request  : ICefRequest;
                                                   const response : ICefResponse;
                                                   var   newUrl   : ustring);
begin
  if (FEvent <> nil) then FEvent.doOnResourceRedirect(browser, frame, request, response, newUrl);
end;

function TCustomRequestHandler.OnResourceResponse(const browser  : ICefBrowser;
                                                  const frame    : ICefFrame;
                                                  const request  : ICefRequest;
                                                  const response : ICefResponse): Boolean;
begin
  if (FEvent <> nil) then
    Result := FEvent.doOnResourceResponse(browser, frame, request, response)
   else
    Result := inherited OnResourceResponse(browser, frame, request, response);
end;

function TCustomRequestHandler.GetResourceResponseFilter(const browser: ICefBrowser;
                                                         const frame: ICefFrame;
                                                         const request: ICefRequest;
                                                         const response: ICefResponse): ICefResponseFilter;
begin
  if (FEvent <> nil) then
    Result := FEvent.doOnGetResourceResponseFilter(browser, frame, request, response)
   else
    Result := inherited GetResourceResponseFilter(browser, frame, request, response);
end;

procedure TCustomRequestHandler.OnResourceLoadComplete(const browser               : ICefBrowser;
                                                       const frame                 : ICefFrame;
                                                       const request               : ICefRequest;
                                                       const response              : ICefResponse;
                                                             status                : TCefUrlRequestStatus;
                                                             receivedContentLength : Int64);
begin
  if (FEvent <> nil) then
    FEvent.doOnResourceLoadComplete(browser, frame, request, response, status, receivedContentLength);
end;

end.
