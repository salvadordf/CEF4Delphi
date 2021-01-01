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

unit uCEFRequestHandler;

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
  TCefRequestHandlerOwn = class(TCefBaseRefCountedOwn, ICefRequestHandler)
    protected
      function  OnBeforeBrowse(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; user_gesture, isRedirect: Boolean): Boolean; virtual;
      function  OnOpenUrlFromTab(const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean): Boolean; virtual;
      procedure GetResourceRequestHandler(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; is_navigation, is_download: boolean; const request_initiator: ustring; var disable_default_handling: boolean; var aResourceRequestHandler : ICefResourceRequestHandler); virtual;
      function  GetAuthCredentials(const browser: ICefBrowser; const originUrl: ustring; isProxy: Boolean; const host: ustring; port: Integer; const realm, scheme: ustring; const callback: ICefAuthCallback): Boolean; virtual;
      function  OnQuotaRequest(const browser: ICefBrowser; const originUrl: ustring; newSize: Int64; const callback: ICefRequestCallback): Boolean; virtual;
      function  OnCertificateError(const browser: ICefBrowser; certError: TCefErrorcode; const requestUrl: ustring; const sslInfo: ICefSslInfo; const callback: ICefRequestCallback): Boolean; virtual;
      function  OnSelectClientCertificate(const browser: ICefBrowser; isProxy: boolean; const host: ustring; port: integer; certificatesCount: NativeUInt; const certificates: TCefX509CertificateArray; const callback: ICefSelectClientCertificateCallback): boolean; virtual;
      procedure OnPluginCrashed(const browser: ICefBrowser; const pluginPath: ustring); virtual;
      procedure OnRenderViewReady(const browser: ICefBrowser); virtual;
      procedure OnRenderProcessTerminated(const browser: ICefBrowser; status: TCefTerminationStatus); virtual;
      procedure OnDocumentAvailableInMainFrame(const browser: ICefBrowser); virtual;

      procedure RemoveReferences; virtual;

    public
      constructor Create; virtual;
  end;

  TCustomRequestHandler = class(TCefRequestHandlerOwn)
    protected
      FEvents : Pointer;

      function  OnBeforeBrowse(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; user_gesture, isRedirect: Boolean): Boolean; override;
      function  OnOpenUrlFromTab(const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean): Boolean; override;
      procedure GetResourceRequestHandler(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; is_navigation, is_download: boolean; const request_initiator: ustring; var disable_default_handling: boolean; var aResourceRequestHandler : ICefResourceRequestHandler); override;
      function  GetAuthCredentials(const browser: ICefBrowser; const originUrl: ustring; isProxy: Boolean; const host: ustring; port: Integer; const realm, scheme: ustring; const callback: ICefAuthCallback): Boolean; override;
      function  OnQuotaRequest(const browser: ICefBrowser; const originUrl: ustring; newSize: Int64; const callback: ICefRequestCallback): Boolean; override;
      function  OnCertificateError(const browser: ICefBrowser; certError: TCefErrorcode; const requestUrl: ustring; const sslInfo: ICefSslInfo; const callback: ICefRequestCallback): Boolean; override;
      function  OnSelectClientCertificate(const browser: ICefBrowser; isProxy: boolean; const host: ustring; port: integer; certificatesCount: NativeUInt; const certificates: TCefX509CertificateArray; const callback: ICefSelectClientCertificateCallback): boolean; override;
      procedure OnPluginCrashed(const browser: ICefBrowser; const pluginPath: ustring); override;
      procedure OnRenderViewReady(const browser: ICefBrowser); override;
      procedure OnRenderProcessTerminated(const browser: ICefBrowser; status: TCefTerminationStatus); override;
      procedure OnDocumentAvailableInMainFrame(const browser: ICefBrowser); override;

    public
      constructor Create(const events : IChromiumEvents); reintroduce; virtual;
      procedure   BeforeDestruction; override;
      procedure   RemoveReferences; override;
  end;

implementation

uses
  {$IFDEF DELPHI16_UP}
    {$IFDEF MSWINDOWS}WinApi.Windows,{$ENDIF} System.SysUtils,
  {$ELSE}
    {$IFDEF MSWINDOWS}Windows,{$ENDIF} SysUtils,
  {$ENDIF}
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBrowser, uCEFFrame, uCEFRequest, uCEFRequestCallback,
  uCEFResponse, uCEFAuthCallback, uCEFSslInfo, uCEFSelectClientCertificateCallback, uCEFX509Certificate,
  uCEFApplicationCore;

function cef_request_handler_on_before_browse(self         : PCefRequestHandler;
                                              browser      : PCefBrowser;
                                              frame        : PCefFrame;
                                              request      : PCefRequest;
                                              user_gesture : Integer;
                                              isRedirect   : Integer): Integer; stdcall;
var
  TempObject : TObject;
begin
  Result     := Ord(False);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefRequestHandlerOwn) then
    Result := Ord(TCefRequestHandlerOwn(TempObject).OnBeforeBrowse(TCefBrowserRef.UnWrap(browser),
                                                                   TCefFrameRef.UnWrap(frame),
                                                                   TCefRequestRef.UnWrap(request),
                                                                   user_gesture <> 0,
                                                                   isRedirect <> 0));
end;

function cef_request_handler_on_open_urlfrom_tab(      self               : PCefRequestHandler;
                                                       browser            : PCefBrowser;
                                                       frame              : PCefFrame;
                                                 const target_url         : PCefString;
                                                       target_disposition : TCefWindowOpenDisposition;
                                                       user_gesture       : Integer): Integer; stdcall;
var
  TempObject : TObject;
begin
  Result     := Ord(False);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefRequestHandlerOwn) then
    Result := Ord(TCefRequestHandlerOwn(TempObject).OnOpenUrlFromTab(TCefBrowserRef.UnWrap(browser),
                                                                     TCefFrameRef.UnWrap(frame),
                                                                     CefString(target_url),
                                                                     target_disposition,
                                                                     user_gesture <> 0));
end;

function cef_request_handler_get_resource_request_handler(      self                     : PCefRequestHandler;
                                                                browser                  : PCefBrowser;
                                                                frame                    : PCefFrame;
                                                                request                  : PCefRequest;
                                                                is_navigation            : Integer;
                                                                is_download              : Integer;
                                                          const request_initiator        : PCefString;
                                                                disable_default_handling : PInteger): PCefResourceRequestHandler; stdcall;
var
  TempObject : TObject;
  TempDisableDefHandling : boolean;
  TempResourceRequestHandler : ICefResourceRequestHandler;
begin
  Result                     := nil;
  TempResourceRequestHandler := nil;
  TempObject                 := CefGetObject(self);
  TempDisableDefHandling     := disable_default_handling^ <> 0;

  if (TempObject <> nil) and (TempObject is TCefRequestHandlerOwn) then
    try
      TCefRequestHandlerOwn(TempObject).GetResourceRequestHandler(TCefBrowserRef.UnWrap(browser),
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

function cef_request_handler_get_auth_credentials(      self       : PCefRequestHandler;
                                                        browser    : PCefBrowser;
                                                  const origin_url : PCefString;
                                                        isProxy    : Integer;
                                                  const host       : PCefString;
                                                        port       : Integer;
                                                  const realm      : PCefString;
                                                  const scheme     : PCefString;
                                                        callback   : PCefAuthCallback): Integer; stdcall;
var
  TempObject : TObject;
begin
  Result     := Ord(False);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefRequestHandlerOwn) then
    Result := Ord(TCefRequestHandlerOwn(TempObject).GetAuthCredentials(TCefBrowserRef.UnWrap(browser),
                                                                       CefString(origin_url),
                                                                       isProxy <> 0,
                                                                       CefString(host),
                                                                       port,
                                                                       CefString(realm),
                                                                       CefString(scheme),
                                                                       TCefAuthCallbackRef.UnWrap(callback)));
end;

function cef_request_handler_on_quota_request(      self       : PCefRequestHandler;
                                                    browser    : PCefBrowser;
                                              const origin_url : PCefString;
                                                    new_size   : Int64;
                                                    callback   : PCefRequestCallback): Integer; stdcall;
var
  TempObject : TObject;
begin
  Result     := Ord(False);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefRequestHandlerOwn) then
    Result := Ord(TCefRequestHandlerOwn(TempObject).OnQuotaRequest(TCefBrowserRef.UnWrap(browser),
                                                                   CefString(origin_url),
                                                                   new_size,
                                                                   TCefRequestCallbackRef.UnWrap(callback)));
end;

function cef_request_handler_on_certificate_error(      self        : PCefRequestHandler;
                                                        browser     : PCefBrowser;
                                                        cert_error  : TCefErrorcode;
                                                  const request_url : PCefString;
                                                        ssl_info    : PCefSslInfo;
                                                        callback    : PCefRequestCallback): Integer; stdcall;
var
  TempObject : TObject;
begin
  Result     := Ord(False);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefRequestHandlerOwn) then
    Result := Ord(TCefRequestHandlerOwn(TempObject).OnCertificateError(TCefBrowserRef.UnWrap(browser),
                                                                       cert_error,
                                                                       CefString(request_url),
                                                                       TCefSslInfoRef.UnWrap(ssl_info),
                                                                       TCefRequestCallbackRef.UnWrap(callback)));
end;

procedure cef_request_handler_on_plugin_crashed(      self        : PCefRequestHandler;
                                                      browser     : PCefBrowser;
                                                const plugin_path : PCefString); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefRequestHandlerOwn) then
    TCefRequestHandlerOwn(TempObject).OnPluginCrashed(TCefBrowserRef.UnWrap(browser),
                                                      CefString(plugin_path));
end;

procedure cef_request_handler_on_render_view_ready(self    : PCefRequestHandler;
                                                   browser : PCefBrowser); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefRequestHandlerOwn) then
    TCefRequestHandlerOwn(TempObject).OnRenderViewReady(TCefBrowserRef.UnWrap(browser));
end;

procedure cef_request_handler_on_render_process_terminated(self    : PCefRequestHandler;
                                                           browser : PCefBrowser;
                                                           status  : TCefTerminationStatus); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefRequestHandlerOwn) then
    TCefRequestHandlerOwn(TempObject).OnRenderProcessTerminated(TCefBrowserRef.UnWrap(browser),
                                                                status);
end;

procedure cef_request_handler_on_document_available_in_main_frame(self    : PCefRequestHandler;
                                                                  browser : PCefBrowser); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefRequestHandlerOwn) then
    TCefRequestHandlerOwn(TempObject).OnDocumentAvailableInMainFrame(TCefBrowserRef.UnWrap(browser));
end;

function cef_request_handler_on_select_client_certificate(      self              : PCefRequestHandler;
                                                                browser           : PCefBrowser;
                                                                isProxy           : integer;
                                                          const host              : PCefString;
                                                                port              : integer;
                                                                certificatesCount : NativeUInt;
                                                          const certificates      : PPCefX509Certificate;
                                                                callback          : PCefSelectClientCertificateCallback): integer; stdcall;
var
  TempCertArray : TCefX509CertificateArray;
  i : NativeUInt;
  TempObject : TObject;
begin
  TempCertArray := nil;
  Result        := Ord(False);

  try
    try
      if (certificatesCount > 0) and (certificates <> nil) then
        begin
          SetLength(TempCertArray, certificatesCount);

          i := 0;
          while (i < certificatesCount) do
            begin
              TempCertArray[i] := TCEFX509CertificateRef.UnWrap(PPointerArray(certificates)^[i]);
              inc(i);
            end;

          TempObject := CefGetObject(self);

          if (TempObject <> nil) and (TempObject is TCefRequestHandlerOwn) then
            Result := Ord(TCefRequestHandlerOwn(TempObject).OnSelectClientCertificate(TCefBrowserRef.UnWrap(browser),
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
      on_before_browse                    := {$IFDEF FPC}@{$ENDIF}cef_request_handler_on_before_browse;
      on_open_urlfrom_tab                 := {$IFDEF FPC}@{$ENDIF}cef_request_handler_on_open_urlfrom_tab;
      get_resource_request_handler        := {$IFDEF FPC}@{$ENDIF}cef_request_handler_get_resource_request_handler;
      get_auth_credentials                := {$IFDEF FPC}@{$ENDIF}cef_request_handler_get_auth_credentials;
      on_quota_request                    := {$IFDEF FPC}@{$ENDIF}cef_request_handler_on_quota_request;
      on_certificate_error                := {$IFDEF FPC}@{$ENDIF}cef_request_handler_on_certificate_error;
      on_select_client_certificate        := {$IFDEF FPC}@{$ENDIF}cef_request_handler_on_select_client_certificate;
      on_plugin_crashed                   := {$IFDEF FPC}@{$ENDIF}cef_request_handler_on_plugin_crashed;
      on_render_view_ready                := {$IFDEF FPC}@{$ENDIF}cef_request_handler_on_render_view_ready;
      on_render_process_terminated        := {$IFDEF FPC}@{$ENDIF}cef_request_handler_on_render_process_terminated;
      on_document_available_in_main_frame := {$IFDEF FPC}@{$ENDIF}cef_request_handler_on_document_available_in_main_frame;
    end;
end;

function TCefRequestHandlerOwn.GetAuthCredentials(const browser   : ICefBrowser;
                                                  const originUrl : ustring;
                                                        isProxy   : Boolean;
                                                  const host      : ustring;
                                                        port      : Integer;
                                                  const realm     : ustring;
                                                  const scheme    : ustring;
                                                  const callback  : ICefAuthCallback): Boolean;
begin
  Result := False;
end;

function TCefRequestHandlerOwn.OnBeforeBrowse(const browser      : ICefBrowser;
                                              const frame        : ICefFrame;
                                              const request      : ICefRequest;
                                                    user_gesture : Boolean;
                                                    isRedirect   : Boolean): Boolean;
begin
  Result := False;
end;

function TCefRequestHandlerOwn.OnCertificateError(const browser    : ICefBrowser;
                                                        certError  : TCefErrorcode;
                                                  const requestUrl : ustring;
                                                  const sslInfo    : ICefSslInfo;
                                                  const callback   : ICefRequestCallback): Boolean;
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

function TCefRequestHandlerOwn.OnOpenUrlFromTab(const browser           : ICefBrowser;
                                                const frame             : ICefFrame;
                                                const targetUrl         : ustring;
                                                      targetDisposition : TCefWindowOpenDisposition;
                                                      userGesture       : Boolean): Boolean;
begin
  Result := False;
end;

procedure TCefRequestHandlerOwn.GetResourceRequestHandler(const browser                  : ICefBrowser;
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

procedure TCefRequestHandlerOwn.OnPluginCrashed(const browser    : ICefBrowser;
                                                const pluginPath : ustring);
begin
  //
end;

function TCefRequestHandlerOwn.OnQuotaRequest(const browser   : ICefBrowser;
                                              const originUrl : ustring;
                                                    newSize   : Int64;
                                              const callback  : ICefRequestCallback): Boolean;
begin
  Result := False;
end;

procedure TCefRequestHandlerOwn.OnRenderProcessTerminated(const browser : ICefBrowser;
                                                                status  : TCefTerminationStatus);
begin
  //
end;

procedure TCefRequestHandlerOwn.OnDocumentAvailableInMainFrame(const browser: ICefBrowser);
begin
  //
end;

procedure TCefRequestHandlerOwn.OnRenderViewReady(const browser: ICefBrowser);
begin
  //
end;

procedure TCefRequestHandlerOwn.RemoveReferences;
begin
  //
end;

// TCustomRequestHandler

constructor TCustomRequestHandler.Create(const events : IChromiumEvents);
begin
  inherited Create;

  FEvents := Pointer(events);
end;

procedure TCustomRequestHandler.BeforeDestruction;
begin
  FEvents := nil;

  inherited BeforeDestruction;
end;

procedure TCustomRequestHandler.RemoveReferences;
begin
  FEvents := nil;
end;

function TCustomRequestHandler.GetAuthCredentials(const browser   : ICefBrowser;
                                                  const originUrl : ustring;
                                                        isProxy   : Boolean;
                                                  const host      : ustring;
                                                        port      : Integer;
                                                  const realm     : ustring;
                                                  const scheme    : ustring;
                                                  const callback  : ICefAuthCallback): Boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnGetAuthCredentials(browser, originUrl, isProxy, host, port, realm, scheme, callback)
   else
    Result := inherited GetAuthCredentials(browser, originUrl, isProxy, host, port, realm, scheme, callback);
end;

function TCustomRequestHandler.OnBeforeBrowse(const browser      : ICefBrowser;
                                              const frame        : ICefFrame;
                                              const request      : ICefRequest;
                                                    user_gesture : Boolean;
                                                    isRedirect   : Boolean): Boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnBeforeBrowse(browser, frame, request, user_gesture, isRedirect)
   else
    Result := inherited OnBeforeBrowse(browser, frame, request, user_gesture, isRedirect);
end;

function TCustomRequestHandler.OnCertificateError(const browser    : ICefBrowser;
                                                        certError  : TCefErrorcode;
                                                  const requestUrl : ustring;
                                                  const sslInfo    : ICefSslInfo;
                                                  const callback   : ICefRequestCallback): Boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnCertificateError(browser, certError, requestUrl, sslInfo, callback)
   else
    Result := inherited OnCertificateError(browser, certError, requestUrl, sslInfo, callback);
end;

function TCustomRequestHandler.OnOpenUrlFromTab(const browser           : ICefBrowser;
                                                const frame             : ICefFrame;
                                                const targetUrl         : ustring;
                                                      targetDisposition : TCefWindowOpenDisposition;
                                                      userGesture       : Boolean): Boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnOpenUrlFromTab(browser, frame, targetUrl, targetDisposition, userGesture)
   else
    Result := inherited OnOpenUrlFromTab(browser, frame, targetUrl, targetDisposition, userGesture);
end;

procedure TCustomRequestHandler.GetResourceRequestHandler(const browser                  : ICefBrowser;
                                                          const frame                    : ICefFrame;
                                                          const request                  : ICefRequest;
                                                                is_navigation            : boolean;
                                                                is_download              : boolean;
                                                          const request_initiator        : ustring;
                                                          var   disable_default_handling : boolean;
                                                          var   aResourceRequestHandler  : ICefResourceRequestHandler);
begin
  if (FEvents <> nil) then
    IChromiumEvents(FEvents).doGetResourceRequestHandler_ReqHdlr(browser,
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

function TCustomRequestHandler.OnSelectClientCertificate(const browser           : ICefBrowser;
                                                               isProxy           : boolean;
                                                         const host              : ustring;
                                                               port              : integer;
                                                               certificatesCount : NativeUInt;
                                                         const certificates      : TCefX509CertificateArray;
                                                         const callback          : ICefSelectClientCertificateCallback): boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnSelectClientCertificate(browser, isProxy, host, port, certificatesCount, certificates, callback)
   else
    Result := inherited OnSelectClientCertificate(browser, isProxy, host, port, certificatesCount, certificates, callback);
end;

procedure TCustomRequestHandler.OnPluginCrashed(const browser: ICefBrowser; const pluginPath: ustring);
begin
  if (FEvents <> nil) then IChromiumEvents(FEvents).doOnPluginCrashed(browser, pluginPath);
end;

function TCustomRequestHandler.OnQuotaRequest(const browser   : ICefBrowser;
                                              const originUrl : ustring;
                                                    newSize   : Int64;
                                              const callback  : ICefRequestCallback): Boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnQuotaRequest(browser, originUrl, newSize, callback)
   else
    Result := inherited OnQuotaRequest(browser, originUrl, newSize, callback);
end;

procedure TCustomRequestHandler.OnRenderProcessTerminated(const browser: ICefBrowser; status: TCefTerminationStatus);
begin
  if (FEvents <> nil) then
    IChromiumEvents(FEvents).doOnRenderProcessTerminated(browser, status);
end;

procedure TCustomRequestHandler.OnDocumentAvailableInMainFrame(const browser: ICefBrowser);
begin
  if (FEvents <> nil) then
    IChromiumEvents(FEvents).doOnDocumentAvailableInMainFrame(browser);
end;

procedure TCustomRequestHandler.OnRenderViewReady(const browser: ICefBrowser);
begin
  if (FEvents <> nil) then
    IChromiumEvents(FEvents).doOnRenderViewReady(browser);
end;

end.
