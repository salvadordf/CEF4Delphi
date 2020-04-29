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
      function  GetParentWindow(const window: ICefWindow; is_menu, can_activate_menu: boolean): ICefWindow;
      function  IsFrameless(const window: ICefWindow): boolean;
      function  CanResize(const window: ICefWindow): boolean;
      function  CanMaximize(const window: ICefWindow): boolean;
      function  CanMinimize(const window: ICefWindow): boolean;
      function  CanClose(const window: ICefWindow): boolean;
      function  OnAccelerator(const window: ICefWindow; command_id: Integer): boolean;
      function  OnKeyEvent(const window: ICefWindow; const event: TCefKeyEvent): boolean;

    public
      class function UnWrap(data: Pointer): ICefWindowDelegate;
  end;

implementation

uses
  uCEFLibFunctions, uCEFMiscFunctions, uCEFWindow;

procedure TCefWindowDelegateRef.OnWindowCreated(const window: ICefWindow);
begin
  PCefWindowDelegate(FData)^.on_window_created(PCefWindowDelegate(FData), CefGetData(window));
end;

procedure TCefWindowDelegateRef.OnWindowDestroyed(const window: ICefWindow);
begin
  PCefWindowDelegate(FData)^.on_window_destroyed(PCefWindowDelegate(FData), CefGetData(window));
end;

function TCefWindowDelegateRef.GetParentWindow(const window            : ICefWindow;
                                                     is_menu           : boolean;
                                                     can_activate_menu : boolean): ICefWindow;
var
  TempIsMenu, TempCanActivateMenu : integer;
begin
  TempIsMenu          := ord(is_menu);
  TempCanActivateMenu := ord(can_activate_menu);
  Result              := TCefWindowRef.UnWrap(PCefWindowDelegate(FData)^.get_parent_window(PCefWindowDelegate(FData),
                                                                                           CefGetData(window),
                                                                                           @TempIsMenu,
                                                                                           @TempCanActivateMenu));
end;

function TCefWindowDelegateRef.IsFrameless(const window: ICefWindow): boolean;
begin
  Result := (PCefWindowDelegate(FData)^.is_frameless(PCefWindowDelegate(FData), CefGetData(window)) <> 0);
end;

function TCefWindowDelegateRef.CanResize(const window: ICefWindow): boolean;
begin
  Result := (PCefWindowDelegate(FData)^.can_resize(PCefWindowDelegate(FData), CefGetData(window)) <> 0);
end;

function TCefWindowDelegateRef.CanMaximize(const window: ICefWindow): boolean;
begin
  Result := (PCefWindowDelegate(FData)^.can_maximize(PCefWindowDelegate(FData), CefGetData(window)) <> 0);
end;

function TCefWindowDelegateRef.CanMinimize(const window: ICefWindow): boolean;
begin
  Result := (PCefWindowDelegate(FData)^.can_minimize(PCefWindowDelegate(FData), CefGetData(window)) <> 0);
end;

function TCefWindowDelegateRef.CanClose(const window: ICefWindow): boolean;
begin
  Result := (PCefWindowDelegate(FData)^.can_close(PCefWindowDelegate(FData), CefGetData(window)) <> 0);
end;

function TCefWindowDelegateRef.OnAccelerator(const window: ICefWindow; command_id: Integer): boolean;
begin
  Result := (PCefWindowDelegate(FData)^.on_accelerator(PCefWindowDelegate(FData), CefGetData(window), command_id) <> 0);
end;

function TCefWindowDelegateRef.OnKeyEvent(const window: ICefWindow; const event: TCefKeyEvent): boolean;
begin
  Result := (PCefWindowDelegate(FData)^.on_key_event(PCefWindowDelegate(FData), CefGetData(window), @event) <> 0);
end;

class function TCefWindowDelegateRef.UnWrap(data: Pointer): ICefWindowDelegate;
begin
  if (data <> nil) then
    Result := Create(data) as ICefWindowDelegate
   else
    Result := nil;
end;

end.

