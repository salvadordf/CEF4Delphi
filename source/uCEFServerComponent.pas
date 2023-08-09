unit uCEFServerComponent;

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
  uCEFTypes, uCEFInterfaces, uCEFConstants, uCEFServer, uCEFServerEvents, uCEFServerHandler;

const
  DEFAULT_CEFSERVER_ADDRESS  = '127.0.0.1';
  DEFAULT_CEFSERVER_PORT     = 8099;
  DEFAULT_CEFSERVER_BACKLOG  = 10;

type
  {$IFNDEF FPC}{$IFDEF DELPHI16_UP}[ComponentPlatformsAttribute(pfidWindows or pfidOSX or pfidLinux)]{$ENDIF}{$ENDIF}
  /// <summary>
  /// The TCEFServerComponent class puts together all CEF server procedures, functions, properties and events in one place.
  /// </summary>
  TCEFServerComponent = class(TComponent, IServerEvents)
    protected
      FHandler                : ICefServerHandler;
      FServer                 : ICefServer;
      FInitialized            : boolean;

      // IServerEvents
      FOnServerCreated        : TOnServerCreated;
      FOnServerDestroyed      : TOnServerDestroyed;
      FOnClientConnected      : TOnClientConnected;
      FOnClientDisconnected   : TOnClientDisconnected;
      FOnHttpRequest          : TOnHttpRequest;
      FOnWebSocketRequest     : TOnWebSocketRequest;
      FOnWebSocketConnected   : TOnWebSocketConnected;
      FOnWebSocketMessage     : TOnWebSocketMessage;

      function  GetInitialized : boolean;
      function  GetIsRunning : boolean;
      function  GetAddress : ustring;
      function  GetHasConnection : boolean;

      // IServerEvents
      procedure doOnServerCreated(const server: ICefServer); virtual;
      procedure doOnServerDestroyed(const server: ICefServer); virtual;
      procedure doOnClientConnected(const server: ICefServer; connection_id: Integer); virtual;
      procedure doOnClientDisconnected(const server: ICefServer; connection_id: Integer); virtual;
      procedure doOnHttpRequest(const server: ICefServer; connection_id: Integer; const client_address: ustring; const request: ICefRequest); virtual;
      procedure doOnWebSocketRequest(const server: ICefServer; connection_id: Integer; const client_address: ustring; const request: ICefRequest; const callback: ICefCallback); virtual;
      procedure doOnWebSocketConnected(const server: ICefServer; connection_id: Integer); virtual;
      procedure doOnWebSocketMessage(const server: ICefServer; connection_id: Integer; const data: Pointer; data_size: NativeUInt); virtual;

      procedure InitializeEvents;

    public
      constructor Create(AOwner: TComponent); override;
      destructor  Destroy; override;
      /// <summary>
      /// Create a new server that binds to |address| and |port|. |address| must be a
      /// valid IPv4 or IPv6 address (e.g. 127.0.0.1 or ::1) and |port| must be a port
      /// number outside of the reserved range (e.g. between 1025 and 65535 on most
      /// platforms). |backlog| is the maximum number of pending connections. A new
      /// thread will be created for each CreateServer call (the "dedicated server
      /// thread"). It is therefore recommended to use a different
      /// ICefServerHandler instance for each CreateServer call to avoid thread
      /// safety issues in the ICefServerHandler implementation. The
      /// ICefServerHandler.OnServerCreated function will be called on the
      /// dedicated server thread to report success or failure. See
      /// ICefServerHandler.OnServerCreated documentation for a description of
      /// server lifespan.
      /// </summary>
      procedure   CreateServer(const address : ustring = DEFAULT_CEFSERVER_ADDRESS; port : uint16 = DEFAULT_CEFSERVER_PORT; backlog : Integer = DEFAULT_CEFSERVER_BACKLOG);
      /// <summary>
      /// Stop the server and shut down the dedicated server thread. See
      /// ICefServerHandler.OnServerCreated documentation for a description of
      /// server lifespan.
      /// </summary>
      procedure   Shutdown;
      /// <summary>
      /// Returns true (1) if |connection_id| represents a valid connection. This
      /// function must be called on the dedicated server thread.
      /// </summary>
      function    IsValidConnection(connection_id: Integer) : boolean;
      /// <summary>
      /// Send an HTTP 200 "OK" response to the connection identified by
      /// |connection_id|. |content_type| is the response content type (e.g.
      /// "text/html"), |data| is the response content, and |data_size| is the size
      /// of |data| in bytes. The contents of |data| will be copied. The connection
      /// will be closed automatically after the response is sent.
      /// </summary>
      procedure   SendHttp200response(connection_id: Integer; const content_type: ustring; const data: Pointer; data_size: NativeUInt);
      /// <summary>
      /// Send an HTTP 404 "Not Found" response to the connection identified by
      /// |connection_id|. The connection will be closed automatically after the
      /// response is sent.
      /// </summary>
      procedure   SendHttp404response(connection_id: Integer);
      /// <summary>
      /// Send an HTTP 500 "Internal Server Error" response to the connection
      /// identified by |connection_id|. |error_message| is the associated error
      /// message. The connection will be closed automatically after the response is
      /// sent.
      /// </summary>
      procedure   SendHttp500response(connection_id: Integer; const error_message: ustring);
      /// <summary>
      /// Send a custom HTTP response to the connection identified by
      /// |connection_id|. |response_code| is the HTTP response code sent in the
      /// status line (e.g. 200), |content_type| is the response content type sent
      /// as the "Content-Type" header (e.g. "text/html"), |content_length| is the
      /// expected content length, and |extra_headers| is the map of extra response
      /// headers. If |content_length| is >= 0 then the "Content-Length" header will
      /// be sent. If |content_length| is 0 then no content is expected and the
      /// connection will be closed automatically after the response is sent. If
      /// |content_length| is < 0 then no "Content-Length" header will be sent and
      /// the client will continue reading until the connection is closed. Use the
      /// SendRawData function to send the content, if applicable, and call
      /// CloseConnection after all content has been sent.
      /// </summary>
      procedure   SendHttpResponse(connection_id, response_code: Integer; const content_type: ustring; content_length: int64; const extra_headers: ICefStringMultimap);
      /// <summary>
      /// Send raw data directly to the connection identified by |connection_id|.
      /// |data| is the raw data and |data_size| is the size of |data| in bytes. The
      /// contents of |data| will be copied. No validation of |data| is performed
      /// internally so the client should be careful to send the amount indicated by
      /// the "Content-Length" header, if specified. See SendHttpResponse
      /// documentation for intended usage.
      /// </summary>
      procedure   SendRawData(connection_id: Integer; const data: Pointer; data_size: NativeUInt);
      /// <summary>
      /// Close the connection identified by |connection_id|. See SendHttpResponse
      /// documentation for intended usage.
      /// </summary>
      procedure   CloseConnection(connection_id: Integer);
      /// <summary>
      /// Send a WebSocket message to the connection identified by |connection_id|.
      /// |data| is the response content and |data_size| is the size of |data| in
      /// bytes. The contents of |data| will be copied. See
      /// ICefServerHandler.OnWebSocketRequest documentation for intended usage.
      /// </summary>
      procedure   SendWebSocketMessage(connection_id: Integer; const data: Pointer; data_size: NativeUInt);
      /// <summary>
      /// Returns true when the server and the handler are initialized.
      /// </summary>
      property Initialized            : boolean                 read GetInitialized;
      /// <summary>
      /// Returns true (1) if the server is currently running and accepting incoming
      /// connections. See ICefServerHandler.OnServerCreated documentation for a
      /// description of server lifespan. This function must be called on the
      /// dedicated server thread.
      /// </summary>
      property IsRunning              : boolean                 read GetIsRunning;
      /// <summary>
      /// Returns the server address including the port number.
      /// </summary>
      property Address                : ustring                 read GetAddress;
      /// <summary>
      /// Returns true (1) if the server currently has a connection. This function
      /// must be called on the dedicated server thread.
      /// </summary>
      property HasConnection          : boolean                 read GetHasConnection;

    published
      /// <summary>
      /// Called when |server| is created. If the server was started successfully
      /// then ICefServer.IsRunning will return true (1). The server will
      /// continue running until ICefServerShutdown is called, after which time
      /// OnServerDestroyed will be called. If the server failed to start then
      /// OnServerDestroyed will be called immediately after this function returns.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the CEF server thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_server_capi.h">CEF source file: /include/capi/cef_server_capi.h (cef_server_handler_t)</see></para>
      /// </remarks>
      property OnServerCreated        : TOnServerCreated        read FOnServerCreated       write FOnServerCreated;
      /// <summary>
      /// Called when |server| is destroyed. The server thread will be stopped after
      /// this function returns. The client should release any references to
      /// |server| when this function is called. See OnServerCreated documentation
      /// for a description of server lifespan.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the CEF server thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_server_capi.h">CEF source file: /include/capi/cef_server_capi.h (cef_server_handler_t)</see></para>
      /// </remarks>
      property OnServerDestroyed      : TOnServerDestroyed      read FOnServerDestroyed     write FOnServerDestroyed;
      /// <summary>
      /// Called when a client connects to |server|. |connection_id| uniquely
      /// identifies the connection. Each call to this function will have a matching
      /// call to OnClientDisconnected.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the CEF server thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_server_capi.h">CEF source file: /include/capi/cef_server_capi.h (cef_server_handler_t)</see></para>
      /// </remarks>
      property OnClientConnected      : TOnClientConnected      read FOnClientConnected     write FOnClientConnected;
      /// <summary>
      /// Called when a client disconnects from |server|. |connection_id| uniquely
      /// identifies the connection. The client should release any data associated
      /// with |connection_id| when this function is called and |connection_id|
      /// should no longer be passed to ICefServer functions. Disconnects can
      /// originate from either the client or the server. For example, the server
      /// will disconnect automatically after a ICefServer.SendHttpXXXResponse
      /// function is called.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the CEF server thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_server_capi.h">CEF source file: /include/capi/cef_server_capi.h (cef_server_handler_t)</see></para>
      /// </remarks>
      property OnClientDisconnected   : TOnClientDisconnected   read FOnClientDisconnected  write FOnClientDisconnected;
      /// <summary>
      /// Called when |server| receives an HTTP request. |connection_id| uniquely
      /// identifies the connection, |client_address| is the requesting IPv4 or IPv6
      /// client address including port number, and |request| contains the request
      /// contents (URL, function, headers and optional POST data). Call
      /// ICefServer functions either synchronously or asynchronusly to send a
      /// response.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the CEF server thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_server_capi.h">CEF source file: /include/capi/cef_server_capi.h (cef_server_handler_t)</see></para>
      /// </remarks>
      property OnHttpRequest          : TOnHttpRequest          read FOnHttpRequest         write FOnHttpRequest;
      /// <summary>
      /// Called when |server| receives a WebSocket request. |connection_id|
      /// uniquely identifies the connection, |client_address| is the requesting
      /// IPv4 or IPv6 client address including port number, and |request| contains
      /// the request contents (URL, function, headers and optional POST data).
      /// Execute |callback| either synchronously or asynchronously to accept or
      /// decline the WebSocket connection. If the request is accepted then
      /// OnWebSocketConnected will be called after the WebSocket has connected and
      /// incoming messages will be delivered to the OnWebSocketMessage callback. If
      /// the request is declined then the client will be disconnected and
      /// OnClientDisconnected will be called. Call the
      /// ICefServer.SendWebSocketMessage function after receiving the
      /// OnWebSocketConnected callback to respond with WebSocket messages.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the CEF server thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_server_capi.h">CEF source file: /include/capi/cef_server_capi.h (cef_server_handler_t)</see></para>
      /// </remarks>
      property OnWebSocketRequest     : TOnWebSocketRequest     read FOnWebSocketRequest    write FOnWebSocketRequest;
      /// <summary>
      /// Called after the client has accepted the WebSocket connection for |server|
      /// and |connection_id| via the OnWebSocketRequest callback. See
      /// OnWebSocketRequest documentation for intended usage.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the CEF server thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_server_capi.h">CEF source file: /include/capi/cef_server_capi.h (cef_server_handler_t)</see></para>
      /// </remarks>
      property OnWebSocketConnected   : TOnWebSocketConnected   read FOnWebSocketConnected  write FOnWebSocketConnected;
      /// <summary>
      /// Called when |server| receives an WebSocket message. |connection_id|
      /// uniquely identifies the connection, |data| is the message content and
      /// |data_size| is the size of |data| in bytes. Do not keep a reference to
      /// |data| outside of this function. See OnWebSocketRequest documentation for
      /// intended usage.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the CEF server thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_server_capi.h">CEF source file: /include/capi/cef_server_capi.h (cef_server_handler_t)</see></para>
      /// </remarks>
      property OnWebSocketMessage     : TOnWebSocketMessage     read FOnWebSocketMessage    write FOnWebSocketMessage;
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
  uCEFLibFunctions, uCEFApplicationCore, uCEFMiscFunctions;

// For more information about the TCEFServerComponent properties and functions
// read the code comments in the CEF source file /include/capi/cef_server_cap.h

constructor TCEFServerComponent.Create(AOwner: TComponent);
begin
  inherited Create(aOwner);

  FHandler     := nil;
  FServer      := nil;
  FInitialized := False;

  InitializeEvents;
end;

destructor TCEFServerComponent.Destroy;
begin
  FServer      := nil;
  FHandler     := nil;

  inherited Destroy;
end;

procedure TCEFServerComponent.InitializeEvents;
begin
  FOnServerCreated        := nil;
  FOnServerDestroyed      := nil;
  FOnClientConnected      := nil;
  FOnClientDisconnected   := nil;
  FOnHttpRequest          := nil;
  FOnWebSocketRequest     := nil;
  FOnWebSocketConnected   := nil;
  FOnWebSocketMessage     := nil;
end;

function TCEFServerComponent.GetInitialized : boolean;
begin
  Result := FInitialized and (FHandler <> nil) and (FServer <> nil);
end;

function TCEFServerComponent.GetIsRunning : boolean;
begin
  Result := Initialized and FServer.IsRunning;
end;

function TCEFServerComponent.GetAddress : ustring;
begin
  if Initialized then
    Result := FServer.GetAddress
   else
    Result := '';
end;

function TCEFServerComponent.GetHasConnection : boolean;
begin
  Result := Initialized and FServer.HasConnection;
end;

procedure TCEFServerComponent.doOnServerCreated(const server: ICefServer);
begin
  if (FServer = nil) and
     (server <> nil) and
     server.IsRunning and
     not(server.HasConnection) then
    begin
      FServer      := server;
      FInitialized := True;
    end;

  if assigned(FOnServerCreated) then FOnServerCreated(self, server);
end;

procedure TCEFServerComponent.doOnServerDestroyed(const server: ICefServer);
begin
  if assigned(FOnServerDestroyed) then FOnServerDestroyed(self, server);

  FServer      := nil;
  FInitialized := False;
end;

procedure TCEFServerComponent.doOnClientConnected(const server: ICefServer; connection_id: Integer);
begin
  if assigned(FOnClientConnected) then FOnClientConnected(self, server, connection_id);
end;

procedure TCEFServerComponent.doOnClientDisconnected(const server: ICefServer; connection_id: Integer);
begin
  if assigned(FOnClientDisconnected) then FOnClientDisconnected(self, server, connection_id);
end;

procedure TCEFServerComponent.doOnHttpRequest(const server         : ICefServer;
                                                    connection_id  : Integer;
                                              const client_address : ustring;
                                              const request        : ICefRequest);
begin
  if assigned(FOnHttpRequest) then FOnHttpRequest(self, server, connection_id, client_address, request);
end;

procedure TCEFServerComponent.doOnWebSocketRequest(const server         : ICefServer;
                                                         connection_id  : Integer;
                                                   const client_address : ustring;
                                                   const request        : ICefRequest;
                                                   const callback       : ICefCallback);
begin
  if assigned(FOnWebSocketRequest) then FOnWebSocketRequest(self, server, connection_id, client_address, request, callback);
end;

procedure TCEFServerComponent.doOnWebSocketConnected(const server: ICefServer; connection_id: Integer);
begin
  if assigned(FOnWebSocketConnected) then FOnWebSocketConnected(self, server, connection_id);
end;

procedure TCEFServerComponent.doOnWebSocketMessage(const server        : ICefServer;
                                                         connection_id : Integer;
                                                   const data          : Pointer;
                                                         data_size     : NativeUInt);
begin
  if assigned(FOnWebSocketMessage) then FOnWebSocketMessage(self, server, connection_id, data, data_size);
end;

procedure TCEFServerComponent.CreateServer(const address : ustring; port : uint16; backlog : Integer);
const
  CEFSERVER_MIN_PORT = 1025;
  CEFSERVER_MAX_PORT = 65535;
var
  TempAddress : TCefString;
  TempPort    : integer;
begin
  if (GlobalCEFApp <> nil) and
     (GlobalCEFApp.Status = asInitialized) and
     not(Initialized) and
     (length(address) > 0) then
    begin
      if (FHandler = nil) then FHandler := TCustomServerHandler.Create(self);

      TempPort    := max(CEFSERVER_MIN_PORT, min(CEFSERVER_MAX_PORT, port));
      TempAddress := CefString(address);

      cef_server_create(@TempAddress, TempPort, backlog, FHandler.Wrap);
    end;
end;

procedure TCEFServerComponent.Shutdown;
begin
  if Initialized then FServer.shutdown;
end;

function TCEFServerComponent.IsValidConnection(connection_id: Integer) : boolean;
begin
  Result := Initialized and FServer.IsValidConnection(connection_id);
end;

procedure TCEFServerComponent.SendHttp200response(      connection_id : Integer;
                                                  const content_type  : ustring;
                                                  const data          : Pointer;
                                                        data_size     : NativeUInt);
begin
  if Initialized then FServer.SendHttp200response(connection_id, content_type, data, data_size);
end;

procedure TCEFServerComponent.SendHttp404response(connection_id: Integer);
begin
  if Initialized then FServer.SendHttp404response(connection_id);
end;

procedure TCEFServerComponent.SendHttp500response(connection_id: Integer; const error_message: ustring);
begin
  if Initialized then FServer.SendHttp500response(connection_id, error_message);
end;

procedure TCEFServerComponent.SendHttpResponse(      connection_id  : Integer;
                                                     response_code  : Integer;
                                               const content_type   : ustring;
                                                     content_length : int64;
                                               const extra_headers  : ICefStringMultimap);
begin
  if Initialized then FServer.SendHttpResponse(connection_id, response_code, content_type, content_length, extra_headers);
end;

procedure TCEFServerComponent.SendRawData(connection_id: Integer; const data: Pointer; data_size: NativeUInt);
begin
  if Initialized then FServer.SendRawData(connection_id, data, data_size);
end;

procedure TCEFServerComponent.CloseConnection(connection_id: Integer);
begin
  if Initialized then FServer.CloseConnection(connection_id);
end;

procedure TCEFServerComponent.SendWebSocketMessage(connection_id: Integer; const data: Pointer; data_size: NativeUInt);
begin
  if Initialized then FServer.SendWebSocketMessage(connection_id, data, data_size);
end;

{$IFDEF FPC}
procedure Register;
begin
  {$I res/tcefservercomponent.lrs}
  RegisterComponents('Chromium', [TCEFServerComponent]);
end;
{$ENDIF}

end.
