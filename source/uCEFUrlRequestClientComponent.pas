unit uCEFUrlRequestClientComponent;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
    {$IFDEF MSWINDOWS}WinApi.Windows, WinApi.Messages, WinApi.ActiveX,{$ENDIF}
    System.Classes, System.Math,
  {$ELSE}
    {$IFDEF MSWINDOWS}Windows, ActiveX,{$ENDIF} Classes, Math,
    {$IFDEF FPC}
    LCLProc, LCLType, LCLIntf, LResources, LMessages, InterfaceBase,
    {$ELSE}
    Messages,
    {$ENDIF}
  {$ENDIF}
  uCEFTypes, uCEFInterfaces, uCEFConstants, uCEFUrlRequestClientEvents, uCEFUrlrequestClient, uCEFUrlRequest;

type
  {$IFNDEF FPC}{$IFDEF DELPHI16_UP}[ComponentPlatformsAttribute(pfidWindows or pfidOSX or pfidLinux)]{$ENDIF}{$ENDIF}
  /// <summary>
  /// The TCEFUrlRequestClientComponent class puts together all CEF URL request procedures, functions, properties and events in one place.
  /// </summary>
  TCEFUrlRequestClientComponent = class(TComponent, ICEFUrlRequestClientEvents)
    protected
      FClient               : ICefUrlrequestClient;
      FThreadID             : TCefThreadId;
      FComponentID          : integer;

      FOnRequestComplete    : TOnRequestComplete;
      FOnUploadProgress     : TOnUploadProgress;
      FOnDownloadProgress   : TOnDownloadProgress;
      FOnDownloadData       : TOnDownloadData;
      FOnGetAuthCredentials : TOnGetAuthCredentials;
      FOnCreateURLRequest   : TNotifyEvent;

      function  GetComponentID : integer;

      // ICefUrlrequestClient
      procedure doOnRequestComplete(const request: ICefUrlRequest);
      procedure doOnUploadProgress(const request: ICefUrlRequest; current, total: Int64);
      procedure doOnDownloadProgress(const request: ICefUrlRequest; current, total: Int64);
      procedure doOnDownloadData(const request: ICefUrlRequest; data: Pointer; dataLength: NativeUInt);
      function  doOnGetAuthCredentials(isProxy: Boolean; const host: ustring; port: Integer; const realm, scheme: ustring; const callback: ICefAuthCallback): Boolean;

      // Custom
      procedure doOnCreateURLRequest;

      procedure DestroyRequestClient;

    public
      constructor Create(AOwner: TComponent); override;
      procedure   AfterConstruction; override;
      procedure   BeforeDestruction; override;
      /// <summary>
      /// Create the URLRequest in the context of TCEFUrlRequestClientComponent.ThreadId, which is the CEF UI thread by default.
      /// </summary>
      procedure   AddURLRequest;
      /// <summary>
      /// Returns the client.
      /// </summary>
      property Client               : ICefUrlrequestClient   read FClient;
      /// <summary>
      /// CEF thread used to create the URLRequest. Most of the client events will be executed on the same thread.
      /// </summary>
      property ThreadID             : TCefThreadId           read FThreadID              write FThreadID;

    published
      /// <summary>
      /// Notifies the client that the request has completed. Use the
      /// ICefUrlRequest.GetRequestStatus function to determine if the request
      /// was successful or not.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the TCEFUrlRequestClientComponent.ThreadId thread, which is the CEF UI thread by default.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_urlrequest_capi.h">CEF source file: /include/capi/cef_urlrequest_capi.h (cef_urlrequest_client_t)</see></para>
      /// </remarks>
      property OnRequestComplete    : TOnRequestComplete     read FOnRequestComplete     write FOnRequestComplete;
      /// <summary>
      /// Notifies the client of upload progress. |current| denotes the number of
      /// bytes sent so far and |total| is the total size of uploading data (or -1
      /// if chunked upload is enabled). This function will only be called if the
      /// UR_FLAG_REPORT_UPLOAD_PROGRESS flag is set on the request.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the TCEFUrlRequestClientComponent.ThreadId thread, which is the CEF UI thread by default.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_urlrequest_capi.h">CEF source file: /include/capi/cef_urlrequest_capi.h (cef_urlrequest_client_t)</see></para>
      /// </remarks>
      property OnUploadProgress     : TOnUploadProgress      read FOnUploadProgress      write FOnUploadProgress;
      /// <summary>
      /// Notifies the client of download progress. |current| denotes the number of
      /// bytes received up to the call and |total| is the expected total size of
      /// the response (or -1 if not determined).
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the TCEFUrlRequestClientComponent.ThreadId thread, which is the CEF UI thread by default.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_urlrequest_capi.h">CEF source file: /include/capi/cef_urlrequest_capi.h (cef_urlrequest_client_t)</see></para>
      /// </remarks>
      property OnDownloadProgress   : TOnDownloadProgress    read FOnDownloadProgress    write FOnDownloadProgress;
      /// <summary>
      /// Called when some part of the response is read. |data| contains the current
      /// bytes received since the last call. This function will not be called if
      /// the UR_FLAG_NO_DOWNLOAD_DATA flag is set on the request.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the TCEFUrlRequestClientComponent.ThreadId thread, which is the CEF UI thread by default.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_urlrequest_capi.h">CEF source file: /include/capi/cef_urlrequest_capi.h (cef_urlrequest_client_t)</see></para>
      /// </remarks>
      property OnDownloadData       : TOnDownloadData        read FOnDownloadData        write FOnDownloadData;
      /// <summary>
      /// Called on the IO thread when the browser needs credentials from the user.
      /// |isProxy| indicates whether the host is a proxy server. |host| contains
      /// the hostname and |port| contains the port number. Return true (1) to
      /// continue the request and call ICefAuthCallback.cont() when the
      /// authentication information is available. If the request has an associated
      /// browser/frame then returning false (0) will result in a call to
      /// GetAuthCredentials on the ICefRequestHandler associated with that
      /// browser, if any. Otherwise, returning false (0) will cancel the request
      /// immediately. This function will only be called for requests initiated from
      /// the browser process.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF IO thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_urlrequest_capi.h">CEF source file: /include/capi/cef_urlrequest_capi.h (cef_urlrequest_client_t)</see></para>
      /// </remarks>
      property OnGetAuthCredentials : TOnGetAuthCredentials  read FOnGetAuthCredentials  write FOnGetAuthCredentials;
      /// <summary>
      /// Event triggered when the URLRequest has been created.
      /// </summary>
      property OnCreateURLRequest   : TNotifyEvent           read FOnCreateURLRequest    write FOnCreateURLRequest;
  end;

{$IFDEF FPC}
procedure Register;
{$ENDIF}

// *********************************************************
// ********************** ATTENTION ! **********************
// *********************************************************
// **                                                     **
// **  MANY OF THE EVENTS IN CEF4DELPHI COMPONENTS LIKE   **
// **  TCHROMIUM, TFMXCHROMIUM OR TCEFAPPLICATION ARE     **
// **  EXECUTED IN A CEF THREAD BY DEFAULT.               **
// **                                                     **
// **  WINDOWS CONTROLS MUST BE CREATED AND DESTROYED IN  **
// **  THE SAME THREAD TO AVOID ERRORS.                   **
// **  SOME OF THEM RECREATE THE HANDLERS IF THEY ARE     **
// **  MODIFIED AND CAN CAUSE THE SAME ERRORS.            **
// **                                                     **
// **  DON'T CREATE, MODIFY OR DESTROY WINDOWS CONTROLS   **
// **  INSIDE THE CEF4DELPHI EVENTS AND USE               **
// **  SYNCHRONIZATION OBJECTS TO PROTECT VARIABLES AND   **
// **  FIELDS IF THEY ARE ALSO USED IN THE MAIN THREAD.   **
// **                                                     **
// **  READ THIS FOR MORE INFORMATION :                   **
// **  https://www.briskbard.com/index.php?pageid=cef     **
// **                                                     **
// **  USE OUR FORUMS FOR MORE QUESTIONS :                **
// **  https://www.briskbard.com/forum/                   **
// **                                                     **
// *********************************************************
// *********************************************************

implementation

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  uCEFRequest, uCEFTask, uCEFMiscFunctions, uCEFApplicationCore;


constructor TCEFUrlRequestClientComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FClient               := nil;
  FThreadID             := TID_UI;
  FOnRequestComplete    := nil;
  FOnUploadProgress     := nil;
  FOnDownloadProgress   := nil;
  FOnDownloadData       := nil;
  FOnGetAuthCredentials := nil;
  FOnCreateURLRequest   := nil;
  FComponentID          := 0;
end;

procedure TCEFUrlRequestClientComponent.AfterConstruction;
begin
  inherited AfterConstruction;

  if not(csDesigning in ComponentState) then
    FClient := TCustomCefUrlrequestClient.Create(self);

  if assigned(GlobalCEFApp) then
    FComponentID := GlobalCEFApp.NextComponentID;
end;

procedure TCEFUrlRequestClientComponent.BeforeDestruction;
begin
  if assigned(GlobalCEFApp) then
    GlobalCEFApp.RemoveComponentID(FComponentID);

  DestroyRequestClient;

  inherited BeforeDestruction;
end;

function TCEFUrlRequestClientComponent.GetComponentID : integer;
begin
  Result := FComponentID;
end;

procedure TCEFUrlRequestClientComponent.DestroyRequestClient;
begin
  try
    if (FClient <> nil) then
      begin
        FClient.RemoveReferences;
        FClient := nil;
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TCEFUrlRequestClientComponent.DestroyRequestClient', e) then raise;
  end;
end;

procedure TCEFUrlRequestClientComponent.doOnRequestComplete(const request: ICefUrlRequest);
begin
  if assigned(FOnRequestComplete) then FOnRequestComplete(self, request);
end;

procedure TCEFUrlRequestClientComponent.doOnUploadProgress(const request: ICefUrlRequest; current, total: Int64);
begin
  if assigned(FOnUploadProgress) then FOnUploadProgress(self, request, current, total);
end;

procedure TCEFUrlRequestClientComponent.doOnDownloadProgress(const request: ICefUrlRequest; current, total: Int64);
begin
  if assigned(FOnDownloadProgress) then FOnDownloadProgress(self, request, current, total);
end;

procedure TCEFUrlRequestClientComponent.doOnDownloadData(const request: ICefUrlRequest; data: Pointer; dataLength: NativeUInt);
begin
  if assigned(FOnDownloadData) then FOnDownloadData(self, request, data, datalength);
end;

function TCEFUrlRequestClientComponent.doOnGetAuthCredentials(isProxy: Boolean; const host: ustring; port: Integer; const realm, scheme: ustring; const callback: ICefAuthCallback): Boolean;
begin
  Result := False;

  if assigned(FOnGetAuthCredentials) then FOnGetAuthCredentials(self, isProxy, host, port, realm, scheme, callback, Result);
end;

procedure TCEFUrlRequestClientComponent.doOnCreateURLRequest;
begin
  if assigned(FOnCreateURLRequest) then FOnCreateURLRequest(self);
end;

procedure TCEFUrlRequestClientComponent.AddURLRequest;
var
  TempTask : ICefTask;
begin
  TempTask := TCefURLRequestTask.Create(self);
  CefPostTask(FThreadID, TempTask);
end;

{$IFDEF FPC}
procedure Register;
begin
  {$I res/tcefurlrequestclientcomponent.lrs}
  RegisterComponents('Chromium', [TCEFUrlRequestClientComponent]);
end;
{$ENDIF}

end.
