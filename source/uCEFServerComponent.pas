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

unit uCEFServerComponent;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

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
  uCEFTypes, uCEFInterfaces, uCEFServer, uCEFServerEvents, uCEFServerHandler;

const
  DEFAULT_CEFSERVER_ADDRESS  = '127.0.0.1';
  DEFAULT_CEFSERVER_PORT     = 8099;
  DEFAULT_CEFSERVER_BACKLOG  = 10;

type
  {$IFNDEF FPC}{$IFDEF DELPHI16_UP}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}{$ENDIF}
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

      procedure   CreateServer(const address : ustring = DEFAULT_CEFSERVER_ADDRESS; port : uint16 = DEFAULT_CEFSERVER_PORT; backlog : Integer = DEFAULT_CEFSERVER_BACKLOG);
      procedure   Shutdown;
      function    IsValidConnection(connection_id: Integer) : boolean;
      procedure   SendHttp200response(connection_id: Integer; const content_type: ustring; const data: Pointer; data_size: NativeUInt);
      procedure   SendHttp404response(connection_id: Integer);
      procedure   SendHttp500response(connection_id: Integer; const error_message: ustring);
      procedure   SendHttpResponse(connection_id, response_code: Integer; const content_type: ustring; content_length: int64; const extra_headers: ICefStringMultimap);
      procedure   SendRawData(connection_id: Integer; const data: Pointer; data_size: NativeUInt);
      procedure   CloseConnection(connection_id: Integer);
      procedure   SendWebSocketMessage(connection_id: Integer; const data: Pointer; data_size: NativeUInt);

      property Initialized            : boolean                 read GetInitialized;
      property IsRunning              : boolean                 read GetIsRunning;
      property Address                : ustring                 read GetAddress;
      property HasConnection          : boolean                 read GetHasConnection;

    published
      property OnServerCreated        : TOnServerCreated        read FOnServerCreated       write FOnServerCreated;
      property OnServerDestroyed      : TOnServerDestroyed      read FOnServerDestroyed     write FOnServerDestroyed;
      property OnClientConnected      : TOnClientConnected      read FOnClientConnected     write FOnClientConnected;
      property OnClientDisconnected   : TOnClientDisconnected   read FOnClientDisconnected  write FOnClientDisconnected;
      property OnHttpRequest          : TOnHttpRequest          read FOnHttpRequest         write FOnHttpRequest;
      property OnWebSocketRequest     : TOnWebSocketRequest     read FOnWebSocketRequest    write FOnWebSocketRequest;
      property OnWebSocketConnected   : TOnWebSocketConnected   read FOnWebSocketConnected  write FOnWebSocketConnected;
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
