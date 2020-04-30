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

unit uCEFWindowDelegate;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes, System.SysUtils,
  {$ELSE}
  Classes, SysUtils,
  {$ENDIF}
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes, uCEFPanelDelegate;

type
  TCefWindowDelegateRef = class(TCefPanelDelegateRef, ICefWindowDelegate)
    protected
      procedure OnWindowCreated(const window: ICefWindow);
      procedure OnWindowDestroyed(const window: ICefWindow);
      procedure OnGetParentWindow(const window: ICefWindow; var is_menu, can_activate_menu: boolean; var aResult : ICefWindow);
      procedure OnIsFrameless(const window: ICefWindow; var aResult : boolean);
      procedure OnCanResize(const window: ICefWindow; var aResult : boolean);
      procedure OnCanMaximize(const window: ICefWindow; var aResult : boolean);
      procedure OnCanMinimize(const window: ICefWindow; var aResult : boolean);
      procedure OnCanClose(const window: ICefWindow; var aResult : boolean);
      procedure OnAccelerator(const window: ICefWindow; command_id: Integer; var aResult : boolean);
      procedure OnKeyEvent(const window: ICefWindow; const event: TCefKeyEvent; var aResult : boolean);

    public
      class function UnWrap(data: Pointer): ICefWindowDelegate;
  end;

  TCefWindowDelegateOwn = class(TCefPanelDelegateOwn, ICefWindowDelegate)
    protected
      procedure OnWindowCreated(const window: ICefWindow); virtual;
      procedure OnWindowDestroyed(const window: ICefWindow); virtual;
      procedure OnGetParentWindow(const window: ICefWindow; var is_menu, can_activate_menu: boolean; var aResult : ICefWindow); virtual;
      procedure OnIsFrameless(const window: ICefWindow; var aResult : boolean); virtual;
      procedure OnCanResize(const window: ICefWindow; var aResult : boolean); virtual;
      procedure OnCanMaximize(const window: ICefWindow; var aResult : boolean); virtual;
      procedure OnCanMinimize(const window: ICefWindow; var aResult : boolean); virtual;
      procedure OnCanClose(const window: ICefWindow; var aResult : boolean); virtual;
      procedure OnAccelerator(const window: ICefWindow; command_id: Integer; var aResult : boolean); virtual;
      procedure OnKeyEvent(const window: ICefWindow; const event: TCefKeyEvent; var aResult : boolean); virtual;

      procedure InitializeCEFMethods; override;
    public
      constructor Create; override;
  end;

implementation

uses
  uCEFLibFunctions, uCEFMiscFunctions, uCEFWindow;


// **************************************************************
// ******************* TCefWindowDelegateRef ********************
// **************************************************************

procedure TCefWindowDelegateRef.OnWindowCreated(const window: ICefWindow);
begin
  PCefWindowDelegate(FData)^.on_window_created(PCefWindowDelegate(FData), CefGetData(window));
end;

procedure TCefWindowDelegateRef.OnWindowDestroyed(const window: ICefWindow);
begin
  PCefWindowDelegate(FData)^.on_window_destroyed(PCefWindowDelegate(FData), CefGetData(window));
end;

procedure TCefWindowDelegateRef.OnGetParentWindow(const window            : ICefWindow;
                                                  var   is_menu           : boolean;
                                                  var   can_activate_menu : boolean;
                                                  var   aResult           : ICefWindow);
var
  TempIsMenu, TempCanActivateMenu : integer;
begin
  TempIsMenu          := ord(is_menu);
  TempCanActivateMenu := ord(can_activate_menu);
  aResult             := TCefWindowRef.UnWrap(PCefWindowDelegate(FData)^.get_parent_window(PCefWindowDelegate(FData),
                                                                                           CefGetData(window),
                                                                                           @TempIsMenu,
                                                                                           @TempCanActivateMenu));
  is_menu           := TempIsMenu <> 0;
  can_activate_menu := TempCanActivateMenu <> 0;
end;

procedure TCefWindowDelegateRef.OnIsFrameless(const window: ICefWindow; var aResult : boolean);
begin
  aResult := (PCefWindowDelegate(FData)^.is_frameless(PCefWindowDelegate(FData), CefGetData(window)) <> 0);
end;

procedure TCefWindowDelegateRef.OnCanResize(const window: ICefWindow; var aResult : boolean);
begin
  aResult := (PCefWindowDelegate(FData)^.can_resize(PCefWindowDelegate(FData), CefGetData(window)) <> 0);
end;

procedure TCefWindowDelegateRef.OnCanMaximize(const window: ICefWindow; var aResult : boolean);
begin
  aResult := (PCefWindowDelegate(FData)^.can_maximize(PCefWindowDelegate(FData), CefGetData(window)) <> 0);
end;

procedure TCefWindowDelegateRef.OnCanMinimize(const window: ICefWindow; var aResult : boolean);
begin
  aResult := (PCefWindowDelegate(FData)^.can_minimize(PCefWindowDelegate(FData), CefGetData(window)) <> 0);
end;

procedure TCefWindowDelegateRef.OnCanClose(const window: ICefWindow; var aResult : boolean);
begin
  aResult := (PCefWindowDelegate(FData)^.can_close(PCefWindowDelegate(FData), CefGetData(window)) <> 0);
end;

procedure TCefWindowDelegateRef.OnAccelerator(const window: ICefWindow; command_id: Integer; var aResult : boolean);
begin
  aResult := (PCefWindowDelegate(FData)^.on_accelerator(PCefWindowDelegate(FData), CefGetData(window), command_id) <> 0);
end;

procedure TCefWindowDelegateRef.OnKeyEvent(const window: ICefWindow; const event: TCefKeyEvent; var aResult : boolean);
begin
  aResult := (PCefWindowDelegate(FData)^.on_key_event(PCefWindowDelegate(FData), CefGetData(window), @event) <> 0);
end;

class function TCefWindowDelegateRef.UnWrap(data: Pointer): ICefWindowDelegate;
begin
  if (data <> nil) then
    Result := Create(data) as ICefWindowDelegate
   else
    Result := nil;
end;


// **************************************************************
// ******************* TCefWindowDelegateOwn ********************
// **************************************************************

procedure cef_window_delegate_on_window_created(self: PCefWindowDelegate; window: PCefWindow); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefWindowDelegateOwn) then
    TCefWindowDelegateOwn(TempObject).OnWindowCreated(TCefWindowRef.UnWrap(window));
end;

procedure cef_window_delegate_on_window_destroyed(self: PCefWindowDelegate; window: PCefWindow); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefWindowDelegateOwn) then
    TCefWindowDelegateOwn(TempObject).OnWindowDestroyed(TCefWindowRef.UnWrap(window));
end;

function cef_window_delegate_get_parent_window(self              : PCefWindowDelegate;
                                               window            : PCefWindow;
                                               is_menu           : PInteger;
                                               can_activate_menu : PInteger): PCefWindow; stdcall;
var
  TempObject : TObject;
  TempWindow : ICefWindow;
  TempIsMenu, TempCanActivateMenu : boolean;
begin
  TempObject := CefGetObject(self);
  TempWindow := nil;


  if (TempObject <> nil) and (TempObject is TCefWindowDelegateOwn) and (is_menu <> nil) and (can_activate_menu <> nil) then
    begin
      TempIsMenu          := (is_menu^           <> 0);
      TempCanActivateMenu := (can_activate_menu^ <> 0);

      TCefWindowDelegateOwn(TempObject).OnGetParentWindow(TCefWindowRef.UnWrap(window),
                                                          TempIsMenu,
                                                          TempCanActivateMenu,
                                                          TempWindow);
      is_menu^           := ord(TempIsMenu);
      can_activate_menu^ := ord(TempCanActivateMenu);
    end;

  Result := CefGetData(TempWindow);
end;

function cef_window_delegate_is_frameless(self: PCefWindowDelegate; window: PCefWindow): Integer; stdcall;
var
  TempObject      : TObject;
  TempIsFrameless : boolean;
begin
  TempObject      := CefGetObject(self);
  TempIsFrameless := False;

  if (TempObject <> nil) and (TempObject is TCefWindowDelegateOwn) then
    TCefWindowDelegateOwn(TempObject).OnIsFrameless(TCefWindowRef.UnWrap(window), TempIsFrameless);

  Result := ord(TempIsFrameless);
end;

function cef_window_delegate_can_resize(self: PCefWindowDelegate; window: PCefWindow): Integer; stdcall;
var
  TempObject    : TObject;
  TempCanResize : boolean;
begin
  TempObject    := CefGetObject(self);
  TempCanResize := True;

  if (TempObject <> nil) and (TempObject is TCefWindowDelegateOwn) then
    TCefWindowDelegateOwn(TempObject).OnCanResize(TCefWindowRef.UnWrap(window), TempCanResize);

  Result := ord(TempCanResize);
end;

function cef_window_delegate_can_maximize(self: PCefWindowDelegate; window: PCefWindow): Integer; stdcall;
var
  TempObject      : TObject;
  TempCanMaximize : boolean;
begin
  TempObject      := CefGetObject(self);
  TempCanMaximize := True;

  if (TempObject <> nil) and (TempObject is TCefWindowDelegateOwn) then
    TCefWindowDelegateOwn(TempObject).OnCanMaximize(TCefWindowRef.UnWrap(window), TempCanMaximize);

  Result := ord(TempCanMaximize);
end;

function cef_window_delegate_can_minimize(self: PCefWindowDelegate; window: PCefWindow): Integer; stdcall;
var
  TempObject      : TObject;
  TempCanMinimize : boolean;
begin
  TempObject      := CefGetObject(self);
  TempCanMinimize := True;

  if (TempObject <> nil) and (TempObject is TCefWindowDelegateOwn) then
    TCefWindowDelegateOwn(TempObject).OnCanMinimize(TCefWindowRef.UnWrap(window), TempCanMinimize);

  Result := ord(TempCanMinimize);
end;

function cef_window_delegate_can_close(self: PCefWindowDelegate; window: PCefWindow): Integer; stdcall;
var
  TempObject   : TObject;
  TempCanClose : boolean;
begin
  TempObject   := CefGetObject(self);
  TempCanClose := True;

  if (TempObject <> nil) and (TempObject is TCefWindowDelegateOwn) then
    TCefWindowDelegateOwn(TempObject).OnCanClose(TCefWindowRef.UnWrap(window), TempCanClose);

  Result := ord(TempCanClose);
end;

function cef_window_delegate_on_accelerator(self       : PCefWindowDelegate;
                                            window     : PCefWindow;
                                            command_id : Integer): Integer; stdcall;
var
  TempObject : TObject;
  TempResult : boolean;
begin
  TempObject := CefGetObject(self);
  TempResult := False;

  if (TempObject <> nil) and (TempObject is TCefWindowDelegateOwn) then
    TCefWindowDelegateOwn(TempObject).OnAccelerator(TCefWindowRef.UnWrap(window), command_id, TempResult);

  Result := ord(TempResult);
end;

function cef_window_delegate_on_key_event(      self   : PCefWindowDelegate;
                                                window : PCefWindow;
                                          const event  : PCefKeyEvent): Integer; stdcall;
var
  TempObject : TObject;
  TempResult : boolean;
begin
  TempObject := CefGetObject(self);
  TempResult := False;

  if (TempObject <> nil) and (TempObject is TCefWindowDelegateOwn) then
    TCefWindowDelegateOwn(TempObject).OnKeyEvent(TCefWindowRef.UnWrap(window), event^, TempResult);

  Result := ord(TempResult);
end;

constructor TCefWindowDelegateOwn.Create;
begin
  inherited CreateData(SizeOf(TCefWindowDelegate));

  InitializeCEFMethods;
end;

procedure TCefWindowDelegateOwn.InitializeCEFMethods;
begin
  inherited InitializeCEFMethods;

  with PCefWindowDelegate(FData)^ do
    begin
      on_window_created       := {$IFDEF FPC}@{$ENDIF}cef_window_delegate_on_window_created;
      on_window_destroyed     := {$IFDEF FPC}@{$ENDIF}cef_window_delegate_on_window_destroyed;
      get_parent_window       := {$IFDEF FPC}@{$ENDIF}cef_window_delegate_get_parent_window;
      is_frameless            := {$IFDEF FPC}@{$ENDIF}cef_window_delegate_is_frameless;
      can_resize              := {$IFDEF FPC}@{$ENDIF}cef_window_delegate_can_resize;
      can_maximize            := {$IFDEF FPC}@{$ENDIF}cef_window_delegate_can_maximize;
      can_minimize            := {$IFDEF FPC}@{$ENDIF}cef_window_delegate_can_minimize;
      can_close               := {$IFDEF FPC}@{$ENDIF}cef_window_delegate_can_close;
      on_accelerator          := {$IFDEF FPC}@{$ENDIF}cef_window_delegate_on_accelerator;
      on_key_event            := {$IFDEF FPC}@{$ENDIF}cef_window_delegate_on_key_event;
    end;
end;

procedure TCefWindowDelegateOwn.OnWindowCreated(const window: ICefWindow);
begin
  //
end;

procedure TCefWindowDelegateOwn.OnWindowDestroyed(const window: ICefWindow);
begin
  //
end;

procedure TCefWindowDelegateOwn.OnGetParentWindow(const window: ICefWindow; var is_menu, can_activate_menu: boolean; var aResult : ICefWindow);
begin
  //
end;

procedure TCefWindowDelegateOwn.OnIsFrameless(const window: ICefWindow; var aResult : boolean);
begin
  //
end;

procedure TCefWindowDelegateOwn.OnCanResize(const window: ICefWindow; var aResult : boolean);
begin
  //
end;

procedure TCefWindowDelegateOwn.OnCanMaximize(const window: ICefWindow; var aResult : boolean);
begin
  //
end;

procedure TCefWindowDelegateOwn.OnCanMinimize(const window: ICefWindow; var aResult : boolean);
begin
  //
end;

procedure TCefWindowDelegateOwn.OnCanClose(const window: ICefWindow; var aResult : boolean);
begin
  //
end;

procedure TCefWindowDelegateOwn.OnAccelerator(const window: ICefWindow; command_id: Integer; var aResult : boolean);
begin
  //
end;

procedure TCefWindowDelegateOwn.OnKeyEvent(const window: ICefWindow; const event: TCefKeyEvent; var aResult : boolean);
begin
  //
end;

end.

