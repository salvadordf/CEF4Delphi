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

unit uCEFWindow;

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
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes, uCEFPanel;

type
  TCefWindowRef = class(TCefPanelRef, ICefWindow)
    protected
      procedure Show;
      procedure Hide;
      procedure CenterWindow(const size_: TCefSize);
      procedure Close;
      function  IsClosed : boolean;
      procedure Activate;
      procedure Deactivate;
      function  IsActive : boolean;
      procedure BringToTop;
      procedure SetAlwaysOnTop(on_top: boolean);
      function  IsAlwaysOnTop : boolean;
      procedure Maximize;
      procedure Minimize;
      procedure Restore;
      procedure SetFullscreen(fullscreen: boolean);
      function  IsMaximized : boolean;
      function  IsMinimized : boolean;
      function  IsFullscreen : boolean;
      procedure SetTitle(const title_: ustring);
      function  GetTitle : ustring;
      procedure SetWindowIcon(const image: ICefImage);
      function  GetWindowIcon : ICefImage;
      procedure SetWindowAppIcon(const image: ICefImage);
      function  GetWindowAppIcon : ICefImage;
      procedure ShowMenu(const menu_model: ICefMenuModel; const screen_point: TCefPoint; anchor_position : TCefMenuAnchorPosition);
      procedure CancelMenu;
      function  GetDisplay : ICefDisplay;
      function  GetClientAreaBoundsInScreen : TCefRect;
      procedure SetDraggableRegions(regionsCount: NativeUInt; const regions: PCefDraggableRegionArray);
      function  GetWindowHandle : TCefWindowHandle;
      procedure SendKeyPress(key_code: Integer; event_flags: cardinal);
      procedure SendMouseMove(screen_x, screen_y: Integer);
      procedure SendMouseEvents(button: TCefMouseButtonType; mouse_down, mouse_up: boolean);
      procedure SetAccelerator(command_id, key_code : Integer; shift_pressed, ctrl_pressed, alt_pressed: boolean);
      procedure RemoveAccelerator(command_id: Integer);
      procedure RemoveAllAccelerators;

    public
      class function UnWrap(data: Pointer): ICefWindow;
      class function CreateTopLevel(const delegate: ICefWindowDelegate): ICefWindow;
  end;

implementation

uses
  uCEFLibFunctions, uCEFMiscFunctions, uCEFImage, uCEFDisplay;

procedure TCefWindowRef.Show;
begin
  PCefWindow(FData)^.show(PCefWindow(FData));
end;

procedure TCefWindowRef.Hide;
begin
  PCefWindow(FData)^.hide(PCefWindow(FData));
end;

procedure TCefWindowRef.CenterWindow(const size_: TCefSize);
begin
  PCefWindow(FData)^.center_window(PCefWindow(FData), @size_);
end;

procedure TCefWindowRef.Close;
begin
  PCefWindow(FData)^.close(PCefWindow(FData));
end;

function TCefWindowRef.IsClosed : boolean;
begin
  Result := (PCefWindow(FData)^.is_closed(PCefWindow(FData)) <> 0);
end;

procedure TCefWindowRef.Activate;
begin
  PCefWindow(FData)^.activate(PCefWindow(FData));
end;

procedure TCefWindowRef.Deactivate;
begin
  PCefWindow(FData)^.deactivate(PCefWindow(FData));
end;

function TCefWindowRef.IsActive : boolean;
begin
  Result := (PCefWindow(FData)^.is_active(PCefWindow(FData)) <> 0);
end;

procedure TCefWindowRef.BringToTop;
begin
  PCefWindow(FData)^.bring_to_top(PCefWindow(FData));
end;

procedure TCefWindowRef.SetAlwaysOnTop(on_top: boolean);
begin
  PCefWindow(FData)^.set_always_on_top(PCefWindow(FData), ord(on_top));
end;

function TCefWindowRef.IsAlwaysOnTop : boolean;
begin
  Result := (PCefWindow(FData)^.is_always_on_top(PCefWindow(FData)) <> 0);
end;

procedure TCefWindowRef.Maximize;
begin
  PCefWindow(FData)^.maximize(PCefWindow(FData));
end;

procedure TCefWindowRef.Minimize;
begin
  PCefWindow(FData)^.minimize(PCefWindow(FData));
end;

procedure TCefWindowRef.Restore;
begin
  PCefWindow(FData)^.restore(PCefWindow(FData));
end;

procedure TCefWindowRef.SetFullscreen(fullscreen: boolean);
begin
  PCefWindow(FData)^.set_fullscreen(PCefWindow(FData), ord(fullscreen));
end;

function TCefWindowRef.IsMaximized : boolean;
begin
  Result := (PCefWindow(FData)^.is_maximized(PCefWindow(FData)) <> 0);
end;

function TCefWindowRef.IsMinimized : boolean;
begin
  Result := (PCefWindow(FData)^.is_minimized(PCefWindow(FData)) <> 0);
end;

function TCefWindowRef.IsFullscreen : boolean;
begin
  Result := (PCefWindow(FData)^.is_fullscreen(PCefWindow(FData)) <> 0);
end;

procedure TCefWindowRef.SetTitle(const title_: ustring);
var
  TempTitle : TCefString;
begin
  TempTitle := CefString(title_);
  PCefWindow(FData)^.set_title(PCefWindow(FData), @TempTitle);
end;

function TCefWindowRef.GetTitle : ustring;
begin
  Result := CefStringFreeAndGet(PCefWindow(FData)^.get_title(PCefWindow(FData)));
end;

procedure TCefWindowRef.SetWindowIcon(const image: ICefImage);
begin
  PCefWindow(FData)^.set_window_icon(PCefWindow(FData), CefGetData(image));
end;

function TCefWindowRef.GetWindowIcon : ICefImage;
begin
  Result := TCefImageRef.UnWrap(PCefWindow(FData)^.get_window_icon(PCefWindow(FData)));
end;

procedure TCefWindowRef.SetWindowAppIcon(const image: ICefImage);
begin
  PCefWindow(FData)^.set_window_app_icon(PCefWindow(FData), CefGetData(image));
end;

function TCefWindowRef.GetWindowAppIcon : ICefImage;
begin
  Result := TCefImageRef.UnWrap(PCefWindow(FData)^.get_window_app_icon(PCefWindow(FData)));
end;

procedure TCefWindowRef.ShowMenu(const menu_model      : ICefMenuModel;
                                 const screen_point    : TCefPoint;
                                       anchor_position : TCefMenuAnchorPosition);
begin
  PCefWindow(FData)^.show_menu(PCefWindow(FData),
                               CefGetData(menu_model),
                               @screen_point,
                               anchor_position);
end;

procedure TCefWindowRef.CancelMenu;
begin
  PCefWindow(FData)^.cancel_menu(PCefWindow(FData));
end;

function TCefWindowRef.GetDisplay : ICefDisplay;
begin
  Result := TCefDisplayRef.UnWrap(PCefWindow(FData)^.get_display(PCefWindow(FData)));
end;

function TCefWindowRef.GetClientAreaBoundsInScreen : TCefRect;
begin
  Result := PCefWindow(FData)^.get_client_area_bounds_in_screen(PCefWindow(FData));
end;

procedure TCefWindowRef.SetDraggableRegions(regionsCount: NativeUInt; const regions: PCefDraggableRegionArray);
begin
  PCefWindow(FData)^.set_draggable_regions(PCefWindow(FData), regionsCount, regions);
end;

function TCefWindowRef.GetWindowHandle : TCefWindowHandle;
begin
  Result := PCefWindow(FData)^.get_window_handle(PCefWindow(FData));
end;

procedure TCefWindowRef.SendKeyPress(key_code: Integer; event_flags: cardinal);
begin
  PCefWindow(FData)^.send_key_press(PCefWindow(FData), key_code, event_flags);
end;

procedure TCefWindowRef.SendMouseMove(screen_x, screen_y: Integer);
begin
  PCefWindow(FData)^.send_mouse_move(PCefWindow(FData), screen_x, screen_y);
end;

procedure TCefWindowRef.SendMouseEvents(button     : TCefMouseButtonType;
                                        mouse_down : boolean;
                                        mouse_up   : boolean);
begin
  PCefWindow(FData)^.send_mouse_events(PCefWindow(FData),
                                       button,
                                       ord(mouse_down),
                                       ord(mouse_up));
end;

procedure TCefWindowRef.SetAccelerator(command_id    : Integer;
                                       key_code      : Integer;
                                       shift_pressed : boolean;
                                       ctrl_pressed  : boolean;
                                       alt_pressed   : boolean);
begin
  PCefWindow(FData)^.set_accelerator(PCefWindow(FData),
                                     command_id,
                                     key_code,
                                     ord(shift_pressed),
                                     ord(ctrl_pressed),
                                     ord(alt_pressed));
end;

procedure TCefWindowRef.RemoveAccelerator(command_id: Integer);
begin
  PCefWindow(FData)^.remove_accelerator(PCefWindow(FData), command_id);
end;

procedure TCefWindowRef.RemoveAllAccelerators;
begin
  PCefWindow(FData)^.remove_all_accelerators(PCefWindow(FData));
end;

class function TCefWindowRef.UnWrap(data: Pointer): ICefWindow;
begin
  if (data <> nil) then
    Result := Create(data) as ICefWindow
   else
    Result := nil;
end;

class function TCefWindowRef.CreateTopLevel(const delegate: ICefWindowDelegate): ICefWindow;
var
  TempWindow : PCefWindow;
begin
  Result := nil;

  if (delegate <> nil) then
    begin
      TempWindow := cef_window_create_top_level(CefGetData(delegate));

      if (TempWindow <> nil) then
        Result := Create(TempWindow) as ICefWindow;
    end;
end;

end.

