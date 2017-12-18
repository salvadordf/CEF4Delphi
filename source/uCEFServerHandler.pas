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

unit uCEFServerHandler;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCEFServerHandlerOwn = class(TCefBaseRefCountedOwn, ICefServerHandler)
    protected
      procedure OnServerCreated(const server: ICefServer); virtual;
      procedure OnServerDestroyed(const server: ICefServer); virtual;
      procedure OnClientConnected(const server: ICefServer; connection_id: Integer); virtual;
      procedure OnClientDisconnected(const server: ICefServer; connection_id: Integer); virtual;
      procedure OnHttpRequest(const server: ICefServer; connection_id: Integer; const client_address: ustring; const request: ICefRequest); virtual;
      procedure OnWebSocketRequest(const server: ICefServer; connection_id: Integer; const client_address: ustring; const request: ICefRequest; const callback: ICefCallback); virtual;
      procedure OnWebSocketConnected(const server: ICefServer; connection_id: Integer); virtual;
      procedure OnWebSocketMessage(const server: ICefServer; connection_id: Integer; const data: Pointer; data_size: NativeUInt); virtual;

    public
      constructor Create; virtual;
  end;

  TCustomServerHandler = class(TCEFServerHandlerOwn)
    protected
      FEvents : IServerEvents;

      procedure OnServerCreated(const server: ICefServer); override;
      procedure OnServerDestroyed(const server: ICefServer); override;
      procedure OnClientConnected(const server: ICefServer; connection_id: Integer); override;
      procedure OnClientDisconnected(const server: ICefServer; connection_id: Integer); override;
      procedure OnHttpRequest(const server: ICefServer; connection_id: Integer; const client_address: ustring; const request: ICefRequest); override;
      procedure OnWebSocketRequest(const server: ICefServer; connection_id: Integer; const client_address: ustring; const request: ICefRequest; const callback: ICefCallback); override;
      procedure OnWebSocketConnected(const server: ICefServer; connection_id: Integer); override;
      procedure OnWebSocketMessage(const server: ICefServer; connection_id: Integer; const data: Pointer; data_size: NativeUInt); override;

    public
      constructor Create(const events: IServerEvents); reintroduce;
      destructor  Destroy; override;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFServer, uCEFRequest, uCEFCallback;

// **************************************************************
// ******************** TCEFServerHandlerOwn ********************
// **************************************************************

procedure cef_load_handler_on_server_created(self   : PCefServerHandler;
                                             server : PCefServer); stdcall;
begin
  with TCEFServerHandlerOwn(CefGetObject(self)) do
    OnServerCreated(TCEFServerRef.UnWrap(server));
end;

procedure cef_load_handler_on_server_destroyed(self   : PCefServerHandler;
                                               server : PCefServer); stdcall;
begin
  with TCEFServerHandlerOwn(CefGetObject(self)) do
    OnServerDestroyed(TCEFServerRef.UnWrap(server));
end;

procedure cef_load_handler_on_client_connected(self          : PCefServerHandler;
                                               server        : PCefServer;
                                               connection_id : Integer); stdcall;
begin
  with TCEFServerHandlerOwn(CefGetObject(self)) do
    OnClientConnected(TCEFServerRef.UnWrap(server), connection_id);
end;

procedure cef_load_handler_on_client_disconnected(self          : PCefServerHandler;
                                                  server        : PCefServer;
                                                  connection_id : Integer); stdcall;
begin
  with TCEFServerHandlerOwn(CefGetObject(self)) do
    OnClientDisconnected(TCEFServerRef.UnWrap(server), connection_id);
end;

procedure cef_load_handler_on_http_request(self : PCefServerHandler;
                                                 server         : PCefServer;
                                                 connection_id  : Integer;
                                           const client_address : PCefString;
                                                 request        : PCefRequest); stdcall;
begin
  with TCEFServerHandlerOwn(CefGetObject(self)) do
    OnHttpRequest(TCEFServerRef.UnWrap(server),
                  connection_id,
                  CefString(client_address),
                  TCefRequestRef.UnWrap(request));
end;

procedure cef_load_handler_on_web_socket_request(self : PCefServerHandler;
                                                       server         : PCefServer;
                                                       connection_id  : Integer;
                                                 const client_address : PCefString;
                                                       request        : PCefRequest;
                                                       callback       : PCefCallback); stdcall;
begin
  with TCEFServerHandlerOwn(CefGetObject(self)) do
    OnWebSocketRequest(TCEFServerRef.UnWrap(server),
                       connection_id,
                       CefString(client_address),
                       TCefRequestRef.UnWrap(request),
                       TCefCallbackRef.UnWrap(callback));
end;

procedure cef_load_handler_on_web_socket_connected(self          : PCefServerHandler;
                                                   server        : PCefServer;
                                                   connection_id : Integer); stdcall;
begin
  with TCEFServerHandlerOwn(CefGetObject(self)) do
    OnWebSocketConnected(TCEFServerRef.UnWrap(server), connection_id);
end;

procedure cef_load_handler_on_web_socket_message(self : PCefServerHandler;
                                                       server        : PCefServer;
                                                       connection_id : Integer;
                                                 const data          : Pointer;
                                                       data_size     : NativeUInt); stdcall;
begin
  with TCEFServerHandlerOwn(CefGetObject(self)) do
    OnWebSocketMessage(TCEFServerRef.UnWrap(server),
                       connection_id,
                       data,
                       data_size);
end;

constructor TCEFServerHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCEFServerHandler));

  with PCEFServerHandler(FData)^ do
    begin
      on_server_created       := cef_load_handler_on_server_created;
      on_server_destroyed     := cef_load_handler_on_server_destroyed;
      on_client_connected     := cef_load_handler_on_client_connected;
      on_client_disconnected  := cef_load_handler_on_client_disconnected;
      on_http_request         := cef_load_handler_on_http_request;
      on_web_socket_request   := cef_load_handler_on_web_socket_request;
      on_web_socket_connected := cef_load_handler_on_web_socket_connected;
      on_web_socket_message   := cef_load_handler_on_web_socket_message;
    end;
end;

procedure TCEFServerHandlerOwn.OnServerCreated(const server: ICefServer);
begin
  //
end;

procedure TCEFServerHandlerOwn.OnServerDestroyed(const server: ICefServer);
begin
  //
end;

procedure TCEFServerHandlerOwn.OnClientConnected(const server: ICefServer; connection_id: Integer);
begin
  //
end;

procedure TCEFServerHandlerOwn.OnClientDisconnected(const server: ICefServer; connection_id: Integer);
begin
  //
end;

procedure TCEFServerHandlerOwn.OnHttpRequest(const server: ICefServer; connection_id: Integer; const client_address: ustring; const request: ICefRequest);
begin
  //
end;

procedure TCEFServerHandlerOwn.OnWebSocketRequest(const server: ICefServer; connection_id: Integer; const client_address: ustring; const request: ICefRequest; const callback: ICefCallback);
begin
  //
end;

procedure TCEFServerHandlerOwn.OnWebSocketConnected(const server: ICefServer; connection_id: Integer);
begin
  //
end;

procedure TCEFServerHandlerOwn.OnWebSocketMessage(const server: ICefServer; connection_id: Integer; const data: Pointer; data_size: NativeUInt);
begin
  //
end;

// ************************************************************************
// ************************* TCustomServerHandler *************************
// ************************************************************************

constructor TCustomServerHandler.Create(const events: IServerEvents);
begin
  inherited Create;

  FEvents := events;
end;

destructor TCustomServerHandler.Destroy;
begin
  FEvents := nil;

  inherited Destroy;
end;

procedure TCustomServerHandler.OnServerCreated(const server: ICefServer);
begin
  if (FEvents <> nil) then FEvents.doOnServerCreated(server);
end;

procedure TCustomServerHandler.OnServerDestroyed(const server: ICefServer);
begin
  if (FEvents <> nil) then FEvents.doOnServerDestroyed(server);
end;

procedure TCustomServerHandler.OnClientConnected(const server: ICefServer; connection_id: Integer);
begin
  if (FEvents <> nil) then FEvents.doOnClientConnected(server, connection_id);
end;

procedure TCustomServerHandler.OnClientDisconnected(const server: ICefServer; connection_id: Integer);
begin
  if (FEvents <> nil) then FEvents.doOnClientDisconnected(server, connection_id);
end;

procedure TCustomServerHandler.OnHttpRequest(const server: ICefServer; connection_id: Integer; const client_address: ustring; const request: ICefRequest);
begin
  if (FEvents <> nil) then FEvents.doOnHttpRequest(server, connection_id, client_address, request);
end;

procedure TCustomServerHandler.OnWebSocketRequest(const server: ICefServer; connection_id: Integer; const client_address: ustring; const request: ICefRequest; const callback: ICefCallback);
begin
  if (FEvents <> nil) then FEvents.doOnWebSocketRequest(server, connection_id, client_address, request, callback);
end;

procedure TCustomServerHandler.OnWebSocketConnected(const server: ICefServer; connection_id: Integer);
begin
  if (FEvents <> nil) then FEvents.doOnWebSocketConnected(server, connection_id);
end;

procedure TCustomServerHandler.OnWebSocketMessage(const server: ICefServer; connection_id: Integer; const data: Pointer; data_size: NativeUInt);
begin
  if (FEvents <> nil) then FEvents.doOnWebSocketMessage(server, connection_id, data, data_size);
end;

end.
