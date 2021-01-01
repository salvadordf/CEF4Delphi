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

unit uCEFView;

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
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefViewRef = class(TCefBaseRefCountedRef, ICefView)
    protected
      function  AsBrowserView : ICefBrowserView;
      function  AsButton : ICefButton;
      function  AsPanel : ICefPanel;
      function  AsScrollView : ICefScrollView;
      function  AsTextfield : ICefTextfield;
      function  GetTypeString : ustring;
      function  ToStringEx(include_children: boolean): ustring;
      function  IsValid : boolean;
      function  IsAttached : boolean;
      function  IsSame(const that: ICefView): boolean;
      function  GetDelegate : ICefViewDelegate;
      function  GetWindow : ICefWindow;
      function  GetID : Integer;
      procedure SetID(id_: Integer);
      function  GetGroupID : Integer;
      procedure SetGroupID(group_id: Integer);
      function  GetParentView : ICefView;
      function  GetViewForID(id_: Integer): ICefView;
      procedure SetBounds(const bounds_: TCefRect);
      function  GetBounds : TCefRect;
      function  GetBoundsInScreen : TCefRect;
      procedure SetSize(const size_: TCefSize);
      function  GetSize : TCefSize;
      procedure SetPosition(const position_: TCefPoint);
      function  GetPosition : TCefPoint;
      function  GetPreferredSize : TCefSize;
      procedure SizeToPreferredSize;
      function  GetMinimumSize : TCefSize;
      function  GetMaximumSize : TCefSize;
      function  GetHeightForWidth(width: Integer): Integer;
      procedure InvalidateLayout;
      procedure SetVisible(visible_: boolean);
      function  IsVisible : boolean;
      function  IsDrawn : boolean;
      procedure SetEnabled(enabled_: boolean);
      function  IsEnabled : boolean;
      procedure SetFocusable(focusable_: boolean);
      function  IsFocusable : boolean;
      function  IsAccessibilityFocusable : boolean;
      procedure RequestFocus;
      procedure SetBackgroundColor(color: TCefColor);
      function  GetBackgroundColor : TCefColor;
      function  ConvertPointToScreen(var point: TCefPoint): boolean;
      function  ConvertPointFromScreen(var point: TCefPoint): boolean;
      function  ConvertPointToWindow(var point: TCefPoint): boolean;
      function  ConvertPointFromWindow(var point: TCefPoint): boolean;
      function  ConvertPointToView(const view : ICefView; var point: TCefPoint): boolean;
      function  ConvertPointFromView(const view : ICefView; var point: TCefPoint): boolean;

    public
      class function UnWrap(data: Pointer): ICefView;
  end;

implementation

uses
  uCEFLibFunctions, uCEFMiscFunctions, uCEFBrowserView, uCEFButton, uCEFPanel,
  uCEFScrollView, uCEFTextfield, uCEFViewDelegate, uCEFWindow;

function TCefViewRef.AsBrowserView : ICefBrowserView;
begin
  Result := TCefBrowserViewRef.UnWrap(PCefView(FData)^.as_browser_view(PCefView(FData)));
end;

function TCefViewRef.AsButton : ICefButton;
begin
  Result := TCefButtonRef.UnWrap(PCefView(FData)^.as_button(PCefView(FData)));
end;

function TCefViewRef.AsPanel : ICefPanel;
begin
  Result := TCefPanelRef.UnWrap(PCefView(FData)^.as_panel(PCefView(FData)));
end;

function TCefViewRef.AsScrollView : ICefScrollView;
begin
  Result := TCefScrollViewRef.UnWrap(PCefView(FData)^.as_scroll_view(PCefView(FData)));
end;

function TCefViewRef.AsTextfield : ICefTextfield;
begin
  Result := TCefTextfieldRef.UnWrap(PCefView(FData)^.as_textfield(PCefView(FData)));
end;

function TCefViewRef.GetTypeString : ustring;
begin
  Result := CefStringFreeAndGet(PCefView(FData)^.get_type_string(PCefView(FData)));
end;

function TCefViewRef.ToStringEx(include_children: boolean): ustring;
begin
  Result := CefStringFreeAndGet(PCefView(FData)^.to_string(PCefView(FData), ord(include_children)));
end;

function TCefViewRef.IsValid : boolean;
begin
  Result := (PCefView(FData)^.is_valid(PCefView(FData)) <> 0);
end;

function TCefViewRef.IsAttached : boolean;
begin
  Result := (PCefView(FData)^.is_attached(PCefView(FData)) <> 0);
end;

function TCefViewRef.IsSame(const that: ICefView): boolean;
begin
  Result := PCefView(FData)^.is_same(PCefView(FData), CefGetData(that)) <> 0;
end;

function TCefViewRef.GetDelegate : ICefViewDelegate;
begin
  Result := TCefViewDelegateRef.UnWrap(PCefView(FData)^.get_delegate(PCefView(FData)));
end;

function TCefViewRef.GetWindow : ICefWindow;
begin
  Result := TCefWindowRef.UnWrap(PCefView(FData)^.get_window(PCefView(FData)));
end;

function TCefViewRef.GetID : Integer;
begin
  Result := PCefView(FData)^.get_id(PCefView(FData));
end;

procedure TCefViewRef.SetID(id_: Integer);
begin
  PCefView(FData)^.set_id(PCefView(FData), id_);
end;

function TCefViewRef.GetGroupID : Integer;
begin
  Result := PCefView(FData)^.get_group_id(PCefView(FData));
end;

procedure TCefViewRef.SetGroupID(group_id: Integer);
begin
  PCefView(FData)^.set_group_id(PCefView(FData), group_id);
end;

function TCefViewRef.GetParentView : ICefView;
begin
  Result := UnWrap(PCefView(FData)^.get_parent_view(PCefView(FData)));
end;

function TCefViewRef.GetViewForID(id_: Integer): ICefView;
begin
  Result := UnWrap(PCefView(FData)^.get_view_for_id(PCefView(FData), id_));
end;

procedure TCefViewRef.SetBounds(const bounds_: TCefRect);
begin
  PCefView(FData)^.set_bounds(PCefView(FData), @bounds_);
end;

function TCefViewRef.GetBounds : TCefRect;
begin
  Result := PCefView(FData)^.get_bounds(PCefView(FData));
end;

function TCefViewRef.GetBoundsInScreen : TCefRect;
begin
  Result := PCefView(FData)^.get_bounds_in_screen(PCefView(FData));
end;

procedure TCefViewRef.SetSize(const size_: TCefSize);
begin
  PCefView(FData)^.set_size(PCefView(FData), @size_);
end;

function TCefViewRef.GetSize : TCefSize;
begin
  Result := PCefView(FData)^.get_size(PCefView(FData));
end;

procedure TCefViewRef.SetPosition(const position_: TCefPoint);
begin
  PCefView(FData)^.set_position(PCefView(FData), @position_);
end;

function TCefViewRef.GetPosition : TCefPoint;
begin
  Result := PCefView(FData)^.get_position(PCefView(FData));
end;

function TCefViewRef.GetPreferredSize : TCefSize;
begin
  Result := PCefView(FData)^.get_preferred_size(PCefView(FData));
end;

procedure TCefViewRef.SizeToPreferredSize;
begin
  PCefView(FData)^.size_to_preferred_size(PCefView(FData));
end;

function TCefViewRef.GetMinimumSize : TCefSize;
begin
  Result := PCefView(FData)^.get_minimum_size(PCefView(FData));
end;

function TCefViewRef.GetMaximumSize : TCefSize;
begin
  Result := PCefView(FData)^.get_maximum_size(PCefView(FData));
end;

function TCefViewRef.GetHeightForWidth(width: Integer): Integer;
begin
  Result := PCefView(FData)^.get_height_for_width(PCefView(FData), width);
end;

procedure TCefViewRef.InvalidateLayout;
begin
  PCefView(FData)^.invalidate_layout(PCefView(FData));
end;

procedure TCefViewRef.SetVisible(visible_: boolean);
begin
  PCefView(FData)^.set_visible(PCefView(FData), ord(visible_));
end;

function TCefViewRef.IsVisible : boolean;
begin
  Result := (PCefView(FData)^.is_visible(PCefView(FData)) <> 0);
end;

function TCefViewRef.IsDrawn : boolean;
begin
  Result := (PCefView(FData)^.is_drawn(PCefView(FData)) <> 0);
end;

procedure TCefViewRef.SetEnabled(enabled_: boolean);
begin
  PCefView(FData)^.set_enabled(PCefView(FData), ord(enabled_));
end;

function TCefViewRef.IsEnabled : boolean;
begin
  Result := (PCefView(FData)^.is_enabled(PCefView(FData)) <> 0);
end;

procedure TCefViewRef.SetFocusable(focusable_: boolean);
begin
  PCefView(FData)^.set_focusable(PCefView(FData), ord(focusable_));
end;

function TCefViewRef.IsFocusable : boolean;
begin
  Result := (PCefView(FData)^.is_focusable(PCefView(FData)) <> 0);
end;

function TCefViewRef.IsAccessibilityFocusable : boolean;
begin
  Result := (PCefView(FData)^.is_accessibility_focusable(PCefView(FData)) <> 0);
end;

procedure TCefViewRef.RequestFocus;
begin
  PCefView(FData)^.request_focus(PCefView(FData));
end;

procedure TCefViewRef.SetBackgroundColor(color: TCefColor);
begin
  PCefView(FData)^.set_background_color(PCefView(FData), color);
end;

function TCefViewRef.GetBackgroundColor : TCefColor;
begin
  Result := PCefView(FData)^.get_background_color(PCefView(FData));
end;

function TCefViewRef.ConvertPointToScreen(var point: TCefPoint): boolean;
begin
  Result := (PCefView(FData)^.convert_point_to_screen(PCefView(FData), @point) <> 0);
end;

function TCefViewRef.ConvertPointFromScreen(var point: TCefPoint): boolean;
begin
  Result := (PCefView(FData)^.convert_point_from_screen(PCefView(FData), @point) <> 0);
end;

function TCefViewRef.ConvertPointToWindow(var point: TCefPoint): boolean;
begin
  Result := (PCefView(FData)^.convert_point_to_window(PCefView(FData), @point) <> 0);
end;

function TCefViewRef.ConvertPointFromWindow(var point: TCefPoint): boolean;
begin
  Result := (PCefView(FData)^.convert_point_from_window(PCefView(FData), @point) <> 0);
end;

function TCefViewRef.ConvertPointToView(const view : ICefView; var point: TCefPoint): boolean;
begin
  Result := (PCefView(FData)^.convert_point_to_view(PCefView(FData), CefGetData(view), @point) <> 0);
end;

function TCefViewRef.ConvertPointFromView(const view : ICefView; var point: TCefPoint): boolean;
begin
  Result := (PCefView(FData)^.convert_point_from_view(PCefView(FData), CefGetData(view), @point) <> 0);
end;

class function TCefViewRef.UnWrap(data: Pointer): ICefView;
begin
  if (data <> nil) then
    Result := Create(data) as ICefView
   else
    Result := nil;
end;

end.

