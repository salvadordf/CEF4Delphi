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

unit uCEFButton;

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
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes, uCEFView;

type
  TCefButtonRef = class(TCefViewRef, ICefButton)
    protected
      function  AsLabelButton : ICefLabelButton;
      procedure SetState(state_: TCefButtonState);
      function  GetState : TCefButtonState;
      procedure SetInkDropEnabled(enabled_: boolean);
      procedure SetTooltipText(const tooltip_text: ustring);
      procedure SetAccessibleName(const name: ustring);

    public
      class function UnWrap(data: Pointer): ICefButton;
  end;

implementation

uses
  uCEFLibFunctions, uCEFMiscFunctions, uCEFLabelButton;

function TCefButtonRef.AsLabelButton : ICefLabelButton;
begin
  Result := TCefLabelButtonRef.UnWrap(PCefButton(FData)^.as_label_button(PCefButton(FData)));
end;

procedure TCefButtonRef.SetState(state_: TCefButtonState);
begin
  PCefButton(FData)^.set_state(PCefButton(FData), state_);
end;

function TCefButtonRef.GetState : TCefButtonState;
begin
  Result := PCefButton(FData)^.get_state(PCefButton(FData));
end;

procedure TCefButtonRef.SetInkDropEnabled(enabled_: boolean);
begin
  PCefButton(FData)^.set_ink_drop_enabled(PCefButton(FData), ord(enabled_));
end;

procedure TCefButtonRef.SetTooltipText(const tooltip_text: ustring);
var
  TempText : TCefString;
begin
  TempText := CefString(tooltip_text);
  PCefButton(FData)^.set_tooltip_text(PCefButton(FData), @TempText);
end;

procedure TCefButtonRef.SetAccessibleName(const name: ustring);
var
  TempName : TCefString;
begin
  TempName := CefString(name);
  PCefButton(FData)^.set_accessible_name(PCefButton(FData), @TempName);
end;

class function TCefButtonRef.UnWrap(data: Pointer): ICefButton;
begin
  if (data <> nil) then
    Result := Create(data) as ICefButton
   else
    Result := nil;
end;

end.

