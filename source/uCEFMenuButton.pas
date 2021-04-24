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

unit uCEFMenuButton;

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
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes, uCEFLabelButton;

type
  TCefMenuButtonRef = class(TCefLabelButtonRef, ICefMenuButton)
    protected
      procedure ShowMenu(const menu_model: ICefMenuModel; const screen_point: TCefPoint; anchor_position: TCefMenuAnchorPosition);
      procedure TriggerMenu;

    public
      class function UnWrap(data: Pointer): ICefMenuButton;
      class function CreateMenuButton(const delegate: ICefMenuButtonDelegate; const text: ustring): ICefMenuButton;
  end;

implementation

uses
  uCEFLibFunctions, uCEFMiscFunctions;

procedure TCefMenuButtonRef.ShowMenu(const menu_model      : ICefMenuModel;
                                     const screen_point    : TCefPoint;
                                           anchor_position : TCefMenuAnchorPosition);
begin
  PCefMenuButton(FData)^.show_menu(PCefMenuButton(FData),
                                   CefGetData(menu_model),
                                   @screen_point,
                                   anchor_position);
end;

procedure TCefMenuButtonRef.TriggerMenu;
begin
  PCefMenuButton(FData)^.trigger_menu(PCefMenuButton(FData));
end;

class function TCefMenuButtonRef.UnWrap(data: Pointer): ICefMenuButton;
begin
  if (data <> nil) then
    Result := Create(data) as ICefMenuButton
   else
    Result := nil;
end;

class function TCefMenuButtonRef.CreateMenuButton(const delegate : ICefMenuButtonDelegate;
                                                  const text     : ustring): ICefMenuButton;
var
  TempText   : TCefString;
  TempButton : PCefMenuButton;
begin
  Result := nil;

  if (delegate <> nil) then
    begin
      TempText   := CefString(text);
      TempButton := cef_menu_button_create(CefGetData(delegate), @TempText);

      if (TempButton <> nil) then
        Result := Create(TempButton) as ICefMenuButton;
    end;
end;

end.

