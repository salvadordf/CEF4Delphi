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
//        Copyright © 2020 Salvador Diaz Fau. All rights reserved.
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

unit uCEFMediaObserver;

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
  TCefMediaObserverOwn = class(TCefBaseRefCountedOwn, ICefMediaObserver)
    protected
      procedure OnSinks(const sinks: TCefMediaSinkArray); virtual;
      procedure OnRoutes(const routes: TCefMediaRouteArray); virtual;
      procedure OnRouteStateChanged(const route: ICefMediaRoute; state: TCefMediaRouteConnectionState); virtual;
      procedure OnRouteMessageReceived(const route: ICefMediaRoute; const message_: Pointer; message_size: NativeUInt); virtual;

    public
      constructor Create; virtual;
  end;

  TCustomMediaObserver = class(TCefMediaObserverOwn)
    protected
      FEvents : Pointer;

      procedure OnSinks(const sinks: TCefMediaSinkArray); override;
      procedure OnRoutes(const routes: TCefMediaRouteArray); override;
      procedure OnRouteStateChanged(const route: ICefMediaRoute; state: TCefMediaRouteConnectionState); override;
      procedure OnRouteMessageReceived(const route: ICefMediaRoute; const message_: Pointer; message_size: NativeUInt); override;

    public
      constructor Create(const events: ICefMediaObserverEvents); reintroduce;
      destructor  Destroy; override;
  end;

implementation

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  uCEFMiscFunctions, uCEFLibFunctions, uCEFMediaSource, uCEFMediaSink,
  uCEFMediaRoute;

// **************************************************
// ************** TCefMediaObserverOwn **************
// **************************************************

procedure cef_media_observer_on_sinks(      self       : PCefMediaObserver;
                                            sinksCount : NativeUInt;
                                      const sinks      : PPCefMediaSink); stdcall;
var
  TempObject : TObject;
  TempArray  : TCefMediaSinkArray;
  i          : NativeUInt;
  TempItem   : PCefMediaSink;
begin
  TempArray  := nil;
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefMediaObserverOwn) then
    try
      SetLength(TempArray, sinksCount);
      TempItem := PCefMediaSink(sinks^);

      i := 0;
      while (i < sinksCount) do
        begin
          TempArray[i] := TCefMediaSinkRef.UnWrap(TempItem);
          inc(TempItem);
          inc(i);
        end;

      TCefMediaObserverOwn(TempObject).OnSinks(TempArray);
    finally
      if (TempArray <> nil) then
        begin
          i := 0;
          while (i < sinksCount) do
            begin
              TempArray[i] := nil;
              inc(i);
            end;

          Finalize(TempArray);
          TempArray := nil;
        end;
    end;
end;

procedure cef_media_observer_on_routes(      self        : PCefMediaObserver;
                                             routesCount : NativeUInt;
                                       const routes      : PPCefMediaRoute); stdcall;
var
  TempObject : TObject;
  TempArray  : TCefMediaRouteArray;
  i          : NativeUInt;
  TempItem   : PCefMediaRoute;
begin
  TempArray  := nil;
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefMediaObserverOwn) then
    try
      SetLength(TempArray, routesCount);
      TempItem := PCefMediaRoute(routes^);

      i := 0;
      while (i < routesCount) do
        begin
          TempArray[i] := TCefMediaRouteRef.UnWrap(TempItem);
          inc(TempItem);
          inc(i);
        end;

      TCefMediaObserverOwn(TempObject).OnRoutes(TempArray);
    finally
      if (TempArray <> nil) then
        begin
          i := 0;
          while (i < routesCount) do
            begin
              TempArray[i] := nil;
              inc(i);
            end;

          Finalize(TempArray);
          TempArray := nil;
        end;
    end;
end;

procedure cef_media_observer_on_route_state_changed(self  : PCefMediaObserver;
                                                    route : PCefMediaRoute;
                                                    state : TCefMediaRouteConnectionState); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefMediaObserverOwn) then
    TCefMediaObserverOwn(TempObject).OnRouteStateChanged(TCefMediaRouteRef.UnWrap(route), state);
end;

procedure cef_media_observer_on_route_message_received(      self         : PCefMediaObserver;
                                                             route        : PCefMediaRoute;
                                                       const message_     : Pointer;
                                                             message_size : NativeUInt); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefMediaObserverOwn) then
    TCefMediaObserverOwn(TempObject).OnRouteMessageReceived(TCefMediaRouteRef.UnWrap(route),
                                                            message_,
                                                            message_size);
end;

constructor TCefMediaObserverOwn.Create;
begin
  inherited CreateData(SizeOf(TCefMediaObserver));

  with PCefMediaObserver(FData)^ do
    begin
      on_sinks                  := {$IFDEF FPC}@{$ENDIF}cef_media_observer_on_sinks;
      on_routes                 := {$IFDEF FPC}@{$ENDIF}cef_media_observer_on_routes;
      on_route_state_changed    := {$IFDEF FPC}@{$ENDIF}cef_media_observer_on_route_state_changed;
      on_route_message_received := {$IFDEF FPC}@{$ENDIF}cef_media_observer_on_route_message_received;
    end;
end;

procedure TCefMediaObserverOwn.OnSinks(const sinks: TCefMediaSinkArray);
begin
  //
end;

procedure TCefMediaObserverOwn.OnRoutes(const routes: TCefMediaRouteArray);
begin
  //
end;

procedure TCefMediaObserverOwn.OnRouteStateChanged(const route: ICefMediaRoute; state: TCefMediaRouteConnectionState);
begin
  //
end;

procedure TCefMediaObserverOwn.OnRouteMessageReceived(const route: ICefMediaRoute; const message_: Pointer; message_size: NativeUInt);
begin
  //
end;

// **************************************************
// ************** TCustomMediaObserver **************
// **************************************************

constructor TCustomMediaObserver.Create(const events: ICefMediaObserverEvents);
begin
  inherited Create;

  FEvents := Pointer(events);
end;

destructor TCustomMediaObserver.Destroy;
begin
  FEvents := nil;

  inherited Destroy;
end;

procedure TCustomMediaObserver.OnSinks(const sinks: TCefMediaSinkArray);
begin
  try
    if (FEvents <> nil) then
      ICefMediaObserverEvents(FEvents).doOnSinks(sinks);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomMediaObserver.OnSinks', e) then raise;
  end;
end;

procedure TCustomMediaObserver.OnRoutes(const routes: TCefMediaRouteArray);
begin
  try
    if (FEvents <> nil) then
      ICefMediaObserverEvents(FEvents).doOnRoutes(routes);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomMediaObserver.OnRoutes', e) then raise;
  end;
end;

procedure TCustomMediaObserver.OnRouteStateChanged(const route: ICefMediaRoute; state: TCefMediaRouteConnectionState);
begin
  try
    if (FEvents <> nil) then
      ICefMediaObserverEvents(FEvents).doOnRouteStateChanged(route, state);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomMediaObserver.OnRouteStateChanged', e) then raise;
  end;
end;

procedure TCustomMediaObserver.OnRouteMessageReceived(const route: ICefMediaRoute; const message_: Pointer; message_size: NativeUInt);
begin
  try
    if (FEvents <> nil) then
      ICefMediaObserverEvents(FEvents).doOnRouteMessageReceived(route, message_, message_size);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomMediaObserver.OnRouteMessageReceived', e) then raise;
  end;
end;


end.
