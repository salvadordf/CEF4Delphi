unit uCEFServerEvents;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes,
  {$ELSE}
  Classes,
  {$ENDIF}
  uCEFTypes, uCEFInterfaces;

type
  TOnServerCreated      = procedure(Sender: TObject; const server: ICefServer) of object;
  TOnServerDestroyed    = procedure(Sender: TObject; const server: ICefServer) of object;
  TOnClientConnected    = procedure(Sender: TObject; const server: ICefServer; connection_id: Integer) of object;
  TOnClientDisconnected = procedure(Sender: TObject; const server: ICefServer; connection_id: Integer) of object;
  TOnHttpRequest        = procedure(Sender: TObject; const server: ICefServer; connection_id: Integer; const client_address: ustring; const request: ICefRequest) of object;
  TOnWebSocketRequest   = procedure(Sender: TObject; const server: ICefServer; connection_id: Integer; const client_address: ustring; const request: ICefRequest; const callback: ICefCallback) of object;
  TOnWebSocketConnected = procedure(Sender: TObject; const server: ICefServer; connection_id: Integer) of object;
  TOnWebSocketMessage   = procedure(Sender: TObject; const server: ICefServer; connection_id: Integer; const data: Pointer; data_size: NativeUInt) of object;

implementation

end.
