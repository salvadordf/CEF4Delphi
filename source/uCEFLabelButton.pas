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

unit uCEFLabelButton;

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
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes, uCEFButton;

type
  TCefLabelButtonRef = class(TCefButtonRef, ICefLabelButton)
    protected
      function  AsMenuButton : ICefMenuButton;
      procedure SetText(const text_: ustring);
      function  GetText : ustring;
      procedure SetImage(button_state: TCefButtonState; const image: ICefImage);
      function  GetImage(button_state: TCefButtonState): ICefImage;
      procedure SetTextColor(for_state: TCefButtonState; color: TCefColor);
      procedure SetEnabledTextColors(color: TCefColor);
      procedure SetFontList(const font_list: ustring);
      procedure SetHorizontalAlignment(alignment: TCefHorizontalAlignment);
      procedure SetMinimumSize(const size_: TCefSize);
      procedure SetMaximumSize(const size_: TCefSize);

    public
      class function UnWrap(data: Pointer): ICefLabelButton;
      class function CreateLabelButton(const delegate: ICefButtonDelegate; const text: ustring): ICefLabelButton;
  end;

implementation

uses
  uCEFLibFunctions, uCEFMiscFunctions, uCEFMenuButton, uCEFImage;

function TCefLabelButtonRef.AsMenuButton : ICefMenuButton;
begin
  Result := TCefMenuButtonRef.UnWrap(PCefLabelButton(FData)^.as_menu_button(PCefLabelButton(FData)));
end;

procedure TCefLabelButtonRef.SetText(const text_: ustring);
var
  TempText : TCefString;
begin
  TempText := CefString(text_);
  PCefLabelButton(FData)^.set_text(PCefLabelButton(FData), @TempText);
end;

function TCefLabelButtonRef.GetText : ustring;
begin
  Result := CefStringFreeAndGet(PCefLabelButton(FData)^.get_text(PCefLabelButton(FData)));
end;

procedure TCefLabelButtonRef.SetImage(button_state: TCefButtonState; const image: ICefImage);
begin
  PCefLabelButton(FData)^.set_image(PCefLabelButton(FData), button_state, CefGetData(image));
end;

function TCefLabelButtonRef.GetImage(button_state: TCefButtonState): ICefImage;
begin
  Result := TCefImageRef.UnWrap(PCefLabelButton(FData)^.get_image(PCefLabelButton(FData), button_state));
end;

procedure TCefLabelButtonRef.SetTextColor(for_state: TCefButtonState; color: TCefColor);
begin
  PCefLabelButton(FData)^.set_text_color(PCefLabelButton(FData), for_state, color);
end;

procedure TCefLabelButtonRef.SetEnabledTextColors(color: TCefColor);
begin
  PCefLabelButton(FData)^.set_enabled_text_colors(PCefLabelButton(FData), color);
end;

procedure TCefLabelButtonRef.SetFontList(const font_list: ustring);
var
  TempFontList : TCefString;
begin
  TempFontList := CefString(font_list);
  PCefLabelButton(FData)^.set_font_list(PCefLabelButton(FData), @TempFontList);
end;

procedure TCefLabelButtonRef.SetHorizontalAlignment(alignment: TCefHorizontalAlignment);
begin
  PCefLabelButton(FData)^.set_horizontal_alignment(PCefLabelButton(FData), alignment);
end;

procedure TCefLabelButtonRef.SetMinimumSize(const size_: TCefSize);
begin
  PCefLabelButton(FData)^.set_minimum_size(PCefLabelButton(FData), @size_);
end;

procedure TCefLabelButtonRef.SetMaximumSize(const size_: TCefSize);
begin
  PCefLabelButton(FData)^.set_maximum_size(PCefLabelButton(FData), @size_);
end;

class function TCefLabelButtonRef.UnWrap(data: Pointer): ICefLabelButton;
begin
  if (data <> nil) then
    Result := Create(data) as ICefLabelButton
   else
    Result := nil;
end;

class function TCefLabelButtonRef.CreateLabelButton(const delegate : ICefButtonDelegate;
                                                    const text     : ustring): ICefLabelButton;
var
  TempText   : TCefString;
  TempButton : PCefLabelButton;
begin
  Result := nil;

  if (delegate <> nil) then
    begin
      TempText   := CefString(text);
      TempButton := cef_label_button_create(CefGetData(delegate), @TempText);

      if (TempButton <> nil) then
        Result := Create(TempButton) as ICefLabelButton;
    end;
end;

end.

