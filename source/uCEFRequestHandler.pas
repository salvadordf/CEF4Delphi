unit uCEFRequestHandler;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

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
      function  OnCertificateError(const browser: ICefBrowser; certError: TCefErrorcode; const requestUrl: ustring; const sslInfo: ICefSslInfo; const callback: ICefCallback): Boolean; virtual;
      function  OnSelectClientCertificate(const browser: ICefBrowser; isProxy: boolean; const host: ustring; port: integer; certificatesCount: NativeUInt; const certificates: TCefX509CertificateArray; const callback: ICefSelectClientCertificateCallback): boolean; virtual;
      procedure OnRenderViewReady(const browser: ICefBrowser); virtual;
      function  OnRenderProcessUnresponsive(const browser: ICefBrowser; const callback: ICefUnresponsiveProcessCallback): boolean; virtual;
      procedure OnRenderProcessResponsive(const browser: ICefBrowser); virtual;
      procedure OnRenderProcessTerminated(const browser: ICefBrowser; status: TCefTerminationStatus; error_code: integer; const error_string: ustring); virtual;
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
      function  OnCertificateError(const browser: ICefBrowser; certError: TCefErrorcode; const requestUrl: ustring; const sslInfo: ICefSslInfo; const callback: ICefCallback): Boolean; override;
      function  OnSelectClientCertificate(const browser: ICefBrowser; isProxy: boolean; const host: ustring; port: integer; certificatesCount: NativeUInt; const certificates: TCefX509CertificateArray; const callback: ICefSelectClientCertificateCallback): boolean; override;
      procedure OnRenderViewReady(const browser: ICefBrowser); override;
      function  OnRenderProcessUnresponsive(const browser: ICefBrowser; const callback: ICefUnresponsiveProcessCallback): boolean; override;
      procedure OnRenderProcessResponsive(const browser: ICefBrowser); override;
      procedure OnRenderProcessTerminated(const browser: ICefBrowser; status: TCefTerminationStatus; error_code: integer; const error_string: ustring); override;
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
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBrowser, uCEFFrame, uCEFRequest, uCEFCallback,
  uCEFResponse, uCEFAuthCallback, uCEFSslInfo, uCEFSelectClientCertificateCallback, uCEFX509Certificate,
  uCEFApplicationCore, uCEFUnresponsiveProcessCallback;

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

function cef_request_handler_on_certificate_error(      self        : PCefRequestHandler;
                                                        browser     : PCefBrowser;
                                                        cert_error  : TCefErrorcode;
                                                  const request_url : PCefString;
                                                        ssl_info    : PCefSslInfo;
                                                        callback    : PCefCallback): Integer; stdcall;
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
                                                                       TCefCallbackRef.UnWrap(callback)));
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

function cef_request_handler_on_render_process_unresponsive(self     : PCefRequestHandler;
                                                            browser  : PCefBrowser;
                                                            callback : PCefUnresponsiveProcessCallback): integer; stdcall;
var
  TempObject : TObject;
begin
  Result     := Ord(False);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefRequestHandlerOwn) then
    Result := Ord(TCefRequestHandlerOwn(TempObject).OnRenderProcessUnresponsive(TCefBrowserRef.UnWrap(browser),
                                                                                TCefUnresponsiveProcessCallbackRef.UnWrap(callback)));
end;

procedure cef_request_handler_on_render_process_responsive(self: PCefRequestHandler; browser: PCefBrowser); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefRequestHandlerOwn) then
    TCefRequestHandlerOwn(TempObject).OnRenderProcessResponsive(TCefBrowserRef.UnWrap(browser));
end;

procedure cef_request_handler_on_render_process_terminated(      self         : PCefRequestHandler;
                                                                 browser      : PCefBrowser;
                                                                 status       : TCefTerminationStatus;
                                                                 error_code   : integer;
                                                           const error_string : PCefString); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefRequestHandlerOwn) then
    TCefRequestHandlerOwn(TempObject).OnRenderProcessTerminated(TCefBrowserRef.UnWrap(browser),
                                                                status,
                                                                error_code,
                                                                CefString(error_string));
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
      on_certificate_error                := {$IFDEF FPC}@{$ENDIF}cef_request_handler_on_certificate_error;
      on_select_client_certificate        := {$IFDEF FPC}@{$ENDIF}cef_request_handler_on_select_client_certificate;
      on_render_view_ready                := {$IFDEF FPC}@{$ENDIF}cef_request_handler_on_render_view_ready;
      on_render_process_unresponsive      := {$IFDEF FPC}@{$ENDIF}cef_request_handler_on_render_process_unresponsive;
      on_render_process_responsive        := {$IFDEF FPC}@{$ENDIF}cef_request_handler_on_render_process_responsive;
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
                                                  const callback   : ICefCallback): Boolean;
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

procedure TCefRequestHandlerOwn.OnRenderProcessTerminated(const browser      : ICefBrowser;
                                                                status       : TCefTerminationStatus;
                                                                error_code   : integer;
                                                          const error_string : ustring);
begin
  //
end;

procedure TCefRequestHandlerOwn.OnDocumentAvailableInMainFrame(const browser: ICefBrowser);
begin
  //
end;

function TCefRequestHandlerOwn.OnRenderProcessUnresponsive(const browser: ICefBrowser; const callback: ICefUnresponsiveProcessCallback): boolean;
begin
  Result := False;
end;

procedure TCefRequestHandlerOwn.OnRenderProcessResponsive(const browser: ICefBrowser);
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
                                                  const callback   : ICefCallback): Boolean;
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

procedure TCustomRequestHandler.OnRenderViewReady(const browser: ICefBrowser);
begin
  if (FEvents <> nil) then
    IChromiumEvents(FEvents).doOnRenderViewReady(browser);
end;

function TCustomRequestHandler.OnRenderProcessUnresponsive(const browser: ICefBrowser; const callback: ICefUnresponsiveProcessCallback): boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnRenderProcessUnresponsive(browser, callback)
   else
    Result := inherited OnRenderProcessUnresponsive(browser, callback);
end;

procedure TCustomRequestHandler.OnRenderProcessResponsive(const browser: ICefBrowser);
begin
  if (FEvents <> nil) then
    IChromiumEvents(FEvents).doOnRenderProcessResponsive(browser);
end;

procedure TCustomRequestHandler.OnRenderProcessTerminated(const browser      : ICefBrowser;
                                                                status       : TCefTerminationStatus;
                                                                error_code   : integer;
                                                          const error_string : ustring);
begin
  if (FEvents <> nil) then
    IChromiumEvents(FEvents).doOnRenderProcessTerminated(browser, status, error_code, error_string);
end;

procedure TCustomRequestHandler.OnDocumentAvailableInMainFrame(const browser: ICefBrowser);
begin
  if (FEvents <> nil) then
    IChromiumEvents(FEvents).doOnDocumentAvailableInMainFrame(browser);
end;

end.
