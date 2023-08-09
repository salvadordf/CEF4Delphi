unit uCEFUrlrequestClient;

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
  TCefUrlrequestClientOwn = class(TCefBaseRefCountedOwn, ICefUrlrequestClient)
    protected
      procedure OnRequestComplete(const request: ICefUrlRequest); virtual;
      procedure OnUploadProgress(const request: ICefUrlRequest; current, total: Int64); virtual;
      procedure OnDownloadProgress(const request: ICefUrlRequest; current, total: Int64); virtual;
      procedure OnDownloadData(const request: ICefUrlRequest; data: Pointer; dataLength: NativeUInt); virtual;
      function  OnGetAuthCredentials(isProxy: Boolean; const host: ustring; port: Integer; const realm, scheme: ustring; const callback: ICefAuthCallback): Boolean; virtual;

      procedure RemoveReferences; virtual;

    public
      constructor Create; virtual;
  end;

  TCustomCefUrlrequestClient = class(TCefUrlrequestClientOwn)
    protected
      FEvents : Pointer;

      procedure OnRequestComplete(const request: ICefUrlRequest); override;
      procedure OnUploadProgress(const request: ICefUrlRequest; current, total: Int64); override;
      procedure OnDownloadProgress(const request: ICefUrlRequest; current, total: Int64); override;
      procedure OnDownloadData(const request: ICefUrlRequest; data: Pointer; dataLength: NativeUInt); override;
      function  OnGetAuthCredentials(isProxy: Boolean; const host: ustring; port: Integer; const realm, scheme: ustring; const callback: ICefAuthCallback): Boolean; override;

      procedure RemoveReferences; override;

    public
      constructor Create(const events: ICEFUrlRequestClientEvents); reintroduce;
      destructor  Destroy; override;
  end;

  TCefUrlrequestClientRef = class(TCefBaseRefCountedRef, ICefUrlrequestClient)
    protected
      procedure OnRequestComplete(const request: ICefUrlRequest);
      procedure OnUploadProgress(const request: ICefUrlRequest; current, total: Int64);
      procedure OnDownloadProgress(const request: ICefUrlRequest; current, total: Int64);
      procedure OnDownloadData(const request: ICefUrlRequest; data: Pointer; dataLength: NativeUInt);
      function  OnGetAuthCredentials(isProxy: Boolean; const host: ustring; port: Integer; const realm, scheme: ustring; const callback: ICefAuthCallback): Boolean;
      procedure RemoveReferences;
    public
      class function UnWrap(data: Pointer): ICefUrlrequestClient;
  end;

implementation

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  uCEFMiscFunctions, uCEFLibFunctions, uCEFUrlRequest, uCEFAuthCallback;


// TCefUrlrequestClientOwn

procedure cef_url_request_client_on_request_complete(self    : PCefUrlRequestClient;
                                                     request : PCefUrlRequest); stdcall;
var
  TempObject  : TObject;
begin
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefUrlrequestClientOwn) then
    TCefUrlrequestClientOwn(TempObject).OnRequestComplete(TCefUrlRequestRef.UnWrap(request));
end;

procedure cef_url_request_client_on_upload_progress(self    : PCefUrlRequestClient;
                                                    request : PCefUrlRequest;
                                                    current : Int64;
                                                    total   : Int64); stdcall;
var
  TempObject  : TObject;
begin
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefUrlrequestClientOwn) then
    TCefUrlrequestClientOwn(TempObject).OnUploadProgress(TCefUrlRequestRef.UnWrap(request),
                                                         current,
                                                         total);
end;

procedure cef_url_request_client_on_download_progress(self    : PCefUrlRequestClient;
                                                      request : PCefUrlRequest;
                                                      current : Int64;
                                                      total   : Int64); stdcall;
var
  TempObject  : TObject;
begin
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefUrlrequestClientOwn) then
    TCefUrlrequestClientOwn(TempObject).OnDownloadProgress(TCefUrlRequestRef.UnWrap(request),
                                                           current,
                                                           total);
end;

procedure cef_url_request_client_on_download_data(      self        : PCefUrlRequestClient;
                                                        request     : PCefUrlRequest;
                                                  const data        : Pointer;
                                                        data_length : NativeUInt); stdcall;
var
  TempObject  : TObject;
begin
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefUrlrequestClientOwn) then
    TCefUrlrequestClientOwn(TempObject).OnDownloadData(TCefUrlRequestRef.UnWrap(request),
                                                       data,
                                                       data_length);
end;

function cef_url_request_client_get_auth_credentials(      self     : PCefUrlRequestClient;
                                                           isProxy  : Integer;
                                                     const host     : PCefString;
                                                           port     : Integer;
                                                     const realm    : PCefString;
                                                     const scheme   : PCefString;
                                                           callback : PCefAuthCallback): Integer; stdcall;
var
  TempObject  : TObject;
begin
  Result      := Ord(False);
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefUrlrequestClientOwn) then
    Result := Ord(TCefUrlrequestClientOwn(TempObject).OnGetAuthCredentials(isProxy <> 0,
                                                                           CefString(host),
                                                                           port,
                                                                           CefString(realm),
                                                                           CefString(scheme),
                                                                           TCefAuthCallbackRef.UnWrap(callback)));
end;


constructor TCefUrlrequestClientOwn.Create;
begin
  inherited CreateData(SizeOf(TCefUrlrequestClient));

  with PCefUrlrequestClient(FData)^ do
    begin
      on_request_complete  := {$IFDEF FPC}@{$ENDIF}cef_url_request_client_on_request_complete;
      on_upload_progress   := {$IFDEF FPC}@{$ENDIF}cef_url_request_client_on_upload_progress;
      on_download_progress := {$IFDEF FPC}@{$ENDIF}cef_url_request_client_on_download_progress;
      on_download_data     := {$IFDEF FPC}@{$ENDIF}cef_url_request_client_on_download_data;
      get_auth_credentials := {$IFDEF FPC}@{$ENDIF}cef_url_request_client_get_auth_credentials;
    end;
end;

procedure TCefUrlrequestClientOwn.OnDownloadData(const request: ICefUrlRequest; data: Pointer; dataLength: NativeUInt);
begin
  //
end;

procedure TCefUrlrequestClientOwn.OnDownloadProgress(const request: ICefUrlRequest; current, total: Int64);
begin
  //
end;

function TCefUrlrequestClientOwn.OnGetAuthCredentials(isProxy: Boolean; const host: ustring; port: Integer; const realm, scheme: ustring; const callback: ICefAuthCallback): Boolean;
begin
  Result := False;
end;

procedure TCefUrlrequestClientOwn.RemoveReferences;
begin
  //
end;

procedure TCefUrlrequestClientOwn.OnRequestComplete(const request: ICefUrlRequest);
begin
  //
end;

procedure TCefUrlrequestClientOwn.OnUploadProgress(const request: ICefUrlRequest; current, total: Int64);
begin
  //
end;


// TCustomCefUrlrequestClient

constructor TCustomCefUrlrequestClient.Create(const events: ICEFUrlRequestClientEvents);
begin
  inherited Create;

  FEvents := Pointer(events);
end;

destructor TCustomCefUrlrequestClient.Destroy;
begin
  RemoveReferences;

  inherited Destroy;
end;

procedure TCustomCefUrlrequestClient.OnRequestComplete(const request: ICefUrlRequest);
begin
  try
    if (FEvents <> nil) then
      ICEFUrlRequestClientEvents(FEvents).doOnRequestComplete(request);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomCefUrlrequestClient.OnRequestComplete', e) then raise;
  end;
end;

procedure TCustomCefUrlrequestClient.OnUploadProgress(const request: ICefUrlRequest; current, total: Int64);
begin
  try
    if (FEvents <> nil) then
      ICEFUrlRequestClientEvents(FEvents).doOnUploadProgress(request, current, total);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomCefUrlrequestClient.OnUploadProgress', e) then raise;
  end;
end;

procedure TCustomCefUrlrequestClient.OnDownloadProgress(const request: ICefUrlRequest; current, total: Int64);
begin
  try
    if (FEvents <> nil) then
      ICEFUrlRequestClientEvents(FEvents).doOnDownloadProgress(request, current, total);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomCefUrlrequestClient.OnDownloadProgress', e) then raise;
  end;
end;

procedure TCustomCefUrlrequestClient.OnDownloadData(const request: ICefUrlRequest; data: Pointer; dataLength: NativeUInt);
begin
  try
    if (FEvents <> nil) then
      ICEFUrlRequestClientEvents(FEvents).doOnDownloadData(request, data, dataLength);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomCefUrlrequestClient.OnDownloadData', e) then raise;
  end;
end;

function TCustomCefUrlrequestClient.OnGetAuthCredentials(isProxy: Boolean; const host: ustring; port: Integer; const realm, scheme: ustring; const callback: ICefAuthCallback): Boolean;
begin
  Result := False;
  try
    if (FEvents <> nil) then
      Result := ICEFUrlRequestClientEvents(FEvents).doOnGetAuthCredentials(isProxy, host, port, realm, scheme, callback);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomCefUrlrequestClient.OnGetAuthCredentials', e) then raise;
  end;
end;

procedure TCustomCefUrlrequestClient.RemoveReferences;
begin
  FEvents := nil;
end;


// TCefUrlrequestClientRef

procedure TCefUrlrequestClientRef.OnRequestComplete(const request: ICefUrlRequest);
begin
  PCefUrlRequestClient(FData)^.on_request_complete(PCefUrlRequestClient(FData), CefGetData(request));
end;

procedure TCefUrlrequestClientRef.OnUploadProgress(const request: ICefUrlRequest; current, total: Int64);
begin
  PCefUrlRequestClient(FData)^.on_upload_progress(PCefUrlRequestClient(FData), CefGetData(request), current, total);
end;

procedure TCefUrlrequestClientRef.OnDownloadProgress(const request: ICefUrlRequest; current, total: Int64);
begin
  PCefUrlRequestClient(FData)^.on_download_progress(PCefUrlRequestClient(FData), CefGetData(request), current, total);
end;

procedure TCefUrlrequestClientRef.OnDownloadData(const request: ICefUrlRequest; data: Pointer; dataLength: NativeUInt);
begin
  PCefUrlRequestClient(FData)^.on_download_data(PCefUrlRequestClient(FData), CefGetData(request), data, dataLength);
end;

function TCefUrlrequestClientRef.OnGetAuthCredentials(isProxy: Boolean; const host: ustring; port: Integer; const realm, scheme: ustring; const callback: ICefAuthCallback): Boolean;
var
  TempHost, TempRealm, TempScheme : TCefString;
begin
  TempHost   := CefString(host);
  TempRealm  := CefString(realm);
  TempScheme := CefString(scheme);
  Result     := PCefUrlRequestClient(FData)^.get_auth_credentials(PCefUrlRequestClient(FData), ord(isProxy), @TempHost, port, @TempRealm, @TempScheme, CefGetData(callback)) <> 0;
end;

procedure TCefUrlrequestClientRef.RemoveReferences;
begin
  //
end;

class function TCefUrlrequestClientRef.UnWrap(data: Pointer): ICEFUrlRequestClient;
begin
  if (data <> nil) then
    Result := Create(data) as ICEFUrlRequestClient
   else
    Result := nil;
end;


end.
