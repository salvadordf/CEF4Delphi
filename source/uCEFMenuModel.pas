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

unit uCEFMenuModel;

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
  TCefMenuModelRef = class(TCefBaseRefCountedRef, ICefMenuModel)
  protected
    function IsSubMenu: Boolean;
    function Clear: Boolean;
    function GetCount: Integer;
    function AddSeparator: Boolean;
    function AddItem(commandId: Integer; const text: ustring): Boolean;
    function AddCheckItem(commandId: Integer; const text: ustring): Boolean;
    function AddRadioItem(commandId: Integer; const text: ustring; groupId: Integer): Boolean;
    function AddSubMenu(commandId: Integer; const text: ustring): ICefMenuModel;
    function InsertSeparatorAt(index: Integer): Boolean;
    function InsertItemAt(index, commandId: Integer; const text: ustring): Boolean;
    function InsertCheckItemAt(index, commandId: Integer; const text: ustring): Boolean;
    function InsertRadioItemAt(index, commandId: Integer; const text: ustring; groupId: Integer): Boolean;
    function InsertSubMenuAt(index, commandId: Integer; const text: ustring): ICefMenuModel;
    function Remove(commandId: Integer): Boolean;
    function RemoveAt(index: Integer): Boolean;
    function GetIndexOf(commandId: Integer): Integer;
    function GetCommandIdAt(index: Integer): Integer;
    function SetCommandIdAt(index, commandId: Integer): Boolean;
    function GetLabel(commandId: Integer): ustring;
    function GetLabelAt(index: Integer): ustring;
    function SetLabel(commandId: Integer; const text: ustring): Boolean;
    function SetLabelAt(index: Integer; const text: ustring): Boolean;
    function GetType(commandId: Integer): TCefMenuItemType;
    function GetTypeAt(index: Integer): TCefMenuItemType;
    function GetGroupId(commandId: Integer): Integer;
    function GetGroupIdAt(index: Integer): Integer;
    function SetGroupId(commandId, groupId: Integer): Boolean;
    function SetGroupIdAt(index, groupId: Integer): Boolean;
    function GetSubMenu(commandId: Integer): ICefMenuModel;
    function GetSubMenuAt(index: Integer): ICefMenuModel;
    function IsVisible(commandId: Integer): Boolean;
    function isVisibleAt(index: Integer): Boolean;
    function SetVisible(commandId: Integer; visible: Boolean): Boolean;
    function SetVisibleAt(index: Integer; visible: Boolean): Boolean;
    function IsEnabled(commandId: Integer): Boolean;
    function IsEnabledAt(index: Integer): Boolean;
    function SetEnabled(commandId: Integer; enabled: Boolean): Boolean;
    function SetEnabledAt(index: Integer; enabled: Boolean): Boolean;
    function IsChecked(commandId: Integer): Boolean;
    function IsCheckedAt(index: Integer): Boolean;
    function setChecked(commandId: Integer; checked: Boolean): Boolean;
    function setCheckedAt(index: Integer; checked: Boolean): Boolean;
    function HasAccelerator(commandId: Integer): Boolean;
    function HasAcceleratorAt(index: Integer): Boolean;
    function SetAccelerator(commandId, keyCode: Integer; shiftPressed, ctrlPressed, altPressed: Boolean): Boolean;
    function SetAcceleratorAt(index, keyCode: Integer; shiftPressed, ctrlPressed, altPressed: Boolean): Boolean;
    function RemoveAccelerator(commandId: Integer): Boolean;
    function RemoveAcceleratorAt(index: Integer): Boolean;
    function GetAccelerator(commandId: Integer; out keyCode: Integer; out shiftPressed, ctrlPressed, altPressed: Boolean): Boolean;
    function GetAcceleratorAt(index: Integer; out keyCode: Integer; out shiftPressed, ctrlPressed, altPressed: Boolean): Boolean;
    function SetColor(commandId: Integer; colorType: TCefMenuColorType; color: TCefColor): Boolean;
    function SetColorAt(index: Integer; colorType: TCefMenuColorType; color: TCefColor): Boolean;
    function GetColor(commandId: Integer; colorType: TCefMenuColorType; out color: TCefColor): Boolean;
    function GetColorAt(index: Integer; colorType: TCefMenuColorType; out color: TCefColor): Boolean;
    function SetFontList(commandId: Integer; const fontList: ustring): Boolean;
    function SetFontListAt(index: Integer; const fontList: ustring): Boolean;
  public
    class function UnWrap(data: Pointer): ICefMenuModel;
    class function New(const delegate: ICefMenuModelDelegate): ICefMenuModel;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;


function TCefMenuModelRef.AddCheckItem(commandId: Integer; const text: ustring): Boolean;
var
  TempText : TCefString;
begin
  TempText := CefString(text);
  Result   := PCefMenuModel(FData)^.add_check_item(PCefMenuModel(FData), commandId, @TempText) <> 0;
end;

function TCefMenuModelRef.AddItem(commandId: Integer; const text: ustring): Boolean;
var
  TempText : TCefString;
begin
  TempText := CefString(text);
  Result   := PCefMenuModel(FData)^.add_item(PCefMenuModel(FData), commandId, @TempText) <> 0;
end;

function TCefMenuModelRef.AddRadioItem(commandId: Integer; const text: ustring; groupId: Integer): Boolean;
var
  TempText : TCefString;
begin
  TempText := CefString(text);
  Result   := PCefMenuModel(FData)^.add_radio_item(PCefMenuModel(FData), commandId, @TempText, groupId) <> 0;
end;

function TCefMenuModelRef.AddSeparator: Boolean;
begin
  Result := PCefMenuModel(FData)^.add_separator(PCefMenuModel(FData)) <> 0;
end;

function TCefMenuModelRef.AddSubMenu(commandId: Integer; const text: ustring): ICefMenuModel;
var
  TempText : TCefString;
begin
  TempText := CefString(text);
  Result   := TCefMenuModelRef.UnWrap(PCefMenuModel(FData)^.add_sub_menu(PCefMenuModel(FData), commandId, @TempText));
end;

function TCefMenuModelRef.IsSubMenu: Boolean;
begin
  Result := PCefMenuModel(FData)^.is_sub_menu(PCefMenuModel(FData)) <> 0;
end;

function TCefMenuModelRef.Clear: Boolean;
begin
  Result := PCefMenuModel(FData)^.clear(PCefMenuModel(FData)) <> 0;
end;

function TCefMenuModelRef.GetAccelerator(commandId: Integer; out keyCode: Integer; out shiftPressed, ctrlPressed, altPressed: Boolean): Boolean;
var
  TempShift, TempCtrl, TempAlt : Integer;
begin
  Result       := PCefMenuModel(FData)^.get_accelerator(PCefMenuModel(FData), commandId, @keyCode, @TempShift, @TempCtrl, @TempAlt) <> 0;
  shiftPressed := TempShift <> 0;
  ctrlPressed  := TempCtrl  <> 0;
  altPressed   := TempAlt   <> 0;
end;

function TCefMenuModelRef.GetAcceleratorAt(index: Integer; out keyCode: Integer; out shiftPressed, ctrlPressed, altPressed: Boolean): Boolean;
var
  TempShift, TempCtrl, TempAlt : Integer;
begin
  Result       := PCefMenuModel(FData)^.get_accelerator_at(PCefMenuModel(FData), index, @keyCode, @TempShift, @TempCtrl, @TempAlt) <> 0;
  shiftPressed := TempShift <> 0;
  ctrlPressed  := TempCtrl  <> 0;
  altPressed   := TempAlt   <> 0;
end;

function TCefMenuModelRef.SetColor(commandId: Integer; colorType: TCefMenuColorType; color: TCefColor): Boolean;
begin
  Result := PCefMenuModel(FData)^.set_color(PCefMenuModel(FData), commandId, colorType, color) <> 0;
end;

function TCefMenuModelRef.SetColorAt(index: Integer; colorType: TCefMenuColorType; color: TCefColor): Boolean;
begin
  Result := PCefMenuModel(FData)^.set_color_at(PCefMenuModel(FData), index, colorType, color) <> 0;
end;

function TCefMenuModelRef.GetColor(commandId: Integer; colorType: TCefMenuColorType; out color: TCefColor): Boolean;
begin
  Result := PCefMenuModel(FData)^.get_color(PCefMenuModel(FData), commandId, colorType, @color) <> 0;
end;

function TCefMenuModelRef.GetColorAt(index: Integer; colorType: TCefMenuColorType; out color: TCefColor): Boolean;
begin
  Result := PCefMenuModel(FData)^.get_color_at(PCefMenuModel(FData), index, colorType, @color) <> 0;
end;

function TCefMenuModelRef.SetFontList(commandId: Integer; const fontList: ustring): Boolean;
var
  TempList : TCefString;
begin
  TempList := CefString(fontList);
  Result   := PCefMenuModel(FData)^.set_font_list(PCefMenuModel(FData), commandId, @TempList) <> 0;
end;

function TCefMenuModelRef.SetFontListAt(index: Integer; const fontList: ustring): Boolean;
var
  TempList : TCefString;
begin
  TempList := CefString(fontList);
  Result   := PCefMenuModel(FData)^.set_font_list_at(PCefMenuModel(FData), index, @TempList) <> 0;
end;

function TCefMenuModelRef.GetCommandIdAt(index: Integer): Integer;
begin
  Result := PCefMenuModel(FData)^.get_command_id_at(PCefMenuModel(FData), index);
end;

function TCefMenuModelRef.GetCount: Integer;
begin
  Result := PCefMenuModel(FData)^.get_count(PCefMenuModel(FData));
end;

function TCefMenuModelRef.GetGroupId(commandId: Integer): Integer;
begin
  Result := PCefMenuModel(FData)^.get_group_id(PCefMenuModel(FData), commandId);
end;

function TCefMenuModelRef.GetGroupIdAt(index: Integer): Integer;
begin
  Result := PCefMenuModel(FData)^.get_group_id(PCefMenuModel(FData), index);
end;

function TCefMenuModelRef.GetIndexOf(commandId: Integer): Integer;
begin
  Result := PCefMenuModel(FData)^.get_index_of(PCefMenuModel(FData), commandId);
end;

function TCefMenuModelRef.GetLabel(commandId: Integer): ustring;
begin
  Result := CefStringFreeAndGet(PCefMenuModel(FData)^.get_label(PCefMenuModel(FData), commandId));
end;

function TCefMenuModelRef.GetLabelAt(index: Integer): ustring;
begin
  Result := CefStringFreeAndGet(PCefMenuModel(FData)^.get_label_at(PCefMenuModel(FData), index));
end;

function TCefMenuModelRef.GetSubMenu(commandId: Integer): ICefMenuModel;
begin
  Result := TCefMenuModelRef.UnWrap(PCefMenuModel(FData)^.get_sub_menu(PCefMenuModel(FData), commandId));
end;

function TCefMenuModelRef.GetSubMenuAt(index: Integer): ICefMenuModel;
begin
  Result := TCefMenuModelRef.UnWrap(PCefMenuModel(FData)^.get_sub_menu_at(PCefMenuModel(FData), index));
end;

function TCefMenuModelRef.GetType(commandId: Integer): TCefMenuItemType;
begin
  Result := PCefMenuModel(FData)^.get_type(PCefMenuModel(FData), commandId);
end;

function TCefMenuModelRef.GetTypeAt(index: Integer): TCefMenuItemType;
begin
  Result := PCefMenuModel(FData)^.get_type_at(PCefMenuModel(FData), index);
end;

function TCefMenuModelRef.HasAccelerator(commandId: Integer): Boolean;
begin
  Result := PCefMenuModel(FData)^.has_accelerator(PCefMenuModel(FData), commandId) <> 0;
end;

function TCefMenuModelRef.HasAcceleratorAt(index: Integer): Boolean;
begin
  Result := PCefMenuModel(FData)^.has_accelerator_at(PCefMenuModel(FData), index) <> 0;
end;

function TCefMenuModelRef.InsertCheckItemAt(index, commandId: Integer; const text: ustring): Boolean;
var
  TempText : TCefString;
begin
  TempText := CefString(text);
  Result   := PCefMenuModel(FData)^.insert_check_item_at(PCefMenuModel(FData), index, commandId, @TempText) <> 0;
end;

function TCefMenuModelRef.InsertItemAt(index, commandId: Integer; const text: ustring): Boolean;
var
  TempText : TCefString;
begin
  TempText := CefString(text);
  Result   := PCefMenuModel(FData)^.insert_item_at(PCefMenuModel(FData), index, commandId, @TempText) <> 0;
end;

function TCefMenuModelRef.InsertRadioItemAt(index, commandId: Integer; const text: ustring; groupId: Integer): Boolean;
var
  TempText : TCefString;
begin
  TempText := CefString(text);
  Result   := PCefMenuModel(FData)^.insert_radio_item_at(PCefMenuModel(FData), index, commandId, @TempText, groupId) <> 0;
end;

function TCefMenuModelRef.InsertSeparatorAt(index: Integer): Boolean;
begin
  Result := PCefMenuModel(FData)^.insert_separator_at(PCefMenuModel(FData), index) <> 0;
end;

function TCefMenuModelRef.InsertSubMenuAt(index, commandId: Integer; const text: ustring): ICefMenuModel;
var
  TempText : TCefString;
begin
  TempText := CefString(text);
  Result   := TCefMenuModelRef.UnWrap(PCefMenuModel(FData)^.insert_sub_menu_at( PCefMenuModel(FData), index, commandId, @TempText));
end;

function TCefMenuModelRef.IsChecked(commandId: Integer): Boolean;
begin
  Result := PCefMenuModel(FData)^.is_checked(PCefMenuModel(FData), commandId) <> 0;
end;

function TCefMenuModelRef.IsCheckedAt(index: Integer): Boolean;
begin
  Result := PCefMenuModel(FData)^.is_checked_at(PCefMenuModel(FData), index) <> 0;
end;

function TCefMenuModelRef.IsEnabled(commandId: Integer): Boolean;
begin
  Result := PCefMenuModel(FData)^.is_enabled(PCefMenuModel(FData), commandId) <> 0;
end;

function TCefMenuModelRef.IsEnabledAt(index: Integer): Boolean;
begin
  Result := PCefMenuModel(FData)^.is_enabled_at(PCefMenuModel(FData), index) <> 0;
end;

function TCefMenuModelRef.IsVisible(commandId: Integer): Boolean;
begin
  Result := PCefMenuModel(FData)^.is_visible(PCefMenuModel(FData), commandId) <> 0;
end;

function TCefMenuModelRef.isVisibleAt(index: Integer): Boolean;
begin
  Result := PCefMenuModel(FData)^.is_visible_at(PCefMenuModel(FData), index) <> 0;
end;

class function TCefMenuModelRef.New(const delegate: ICefMenuModelDelegate): ICefMenuModel;
begin
  Result := UnWrap(cef_menu_model_create(CefGetData(delegate)));
end;

function TCefMenuModelRef.Remove(commandId: Integer): Boolean;
begin
  Result := PCefMenuModel(FData)^.remove(PCefMenuModel(FData), commandId) <> 0;
end;

function TCefMenuModelRef.RemoveAccelerator(commandId: Integer): Boolean;
begin
  Result := PCefMenuModel(FData)^.remove_accelerator(PCefMenuModel(FData), commandId) <> 0;
end;

function TCefMenuModelRef.RemoveAcceleratorAt(index: Integer): Boolean;
begin
  Result := PCefMenuModel(FData)^.remove_accelerator_at(PCefMenuModel(FData), index) <> 0;
end;

function TCefMenuModelRef.RemoveAt(index: Integer): Boolean;
begin
  Result := PCefMenuModel(FData)^.remove_at(PCefMenuModel(FData), index) <> 0;
end;

function TCefMenuModelRef.SetAccelerator(commandId, keyCode: Integer; shiftPressed, ctrlPressed, altPressed: Boolean): Boolean;
begin
  Result := PCefMenuModel(FData)^.set_accelerator(PCefMenuModel(FData), commandId, keyCode,
                                                  Ord(shiftPressed), Ord(ctrlPressed), Ord(altPressed)) <> 0;
end;

function TCefMenuModelRef.SetAcceleratorAt(index, keyCode: Integer; shiftPressed, ctrlPressed, altPressed: Boolean): Boolean;
begin
  Result := PCefMenuModel(FData)^.set_accelerator_at(PCefMenuModel(FData), index, keyCode,
                                                     Ord(shiftPressed), Ord(ctrlPressed), Ord(altPressed)) <> 0;
end;

function TCefMenuModelRef.setChecked(commandId: Integer; checked: Boolean): Boolean;
begin
  Result := PCefMenuModel(FData)^.set_checked(PCefMenuModel(FData), commandId, Ord(checked)) <> 0;
end;

function TCefMenuModelRef.setCheckedAt(index: Integer; checked: Boolean): Boolean;
begin
  Result := PCefMenuModel(FData)^.set_checked_at(PCefMenuModel(FData), index, Ord(checked)) <> 0;
end;

function TCefMenuModelRef.SetCommandIdAt(index, commandId: Integer): Boolean;
begin
  Result := PCefMenuModel(FData)^.set_command_id_at(PCefMenuModel(FData), index, commandId) <> 0;
end;

function TCefMenuModelRef.SetEnabled(commandId: Integer; enabled: Boolean): Boolean;
begin
  Result := PCefMenuModel(FData)^.set_enabled(PCefMenuModel(FData), commandId, Ord(enabled)) <> 0;
end;

function TCefMenuModelRef.SetEnabledAt(index: Integer; enabled: Boolean): Boolean;
begin
  Result := PCefMenuModel(FData)^.set_enabled_at(PCefMenuModel(FData), index, Ord(enabled)) <> 0;
end;

function TCefMenuModelRef.SetGroupId(commandId, groupId: Integer): Boolean;
begin
  Result := PCefMenuModel(FData)^.set_group_id(PCefMenuModel(FData), commandId, groupId) <> 0;
end;

function TCefMenuModelRef.SetGroupIdAt(index, groupId: Integer): Boolean;
begin
  Result := PCefMenuModel(FData)^.set_group_id_at(PCefMenuModel(FData), index, groupId) <> 0;
end;

function TCefMenuModelRef.SetLabel(commandId: Integer; const text: ustring): Boolean;
var
  TempText : TCefString;
begin
  TempText := CefString(text);
  Result   := PCefMenuModel(FData)^.set_label(PCefMenuModel(FData), commandId, @TempText) <> 0;
end;

function TCefMenuModelRef.SetLabelAt(index: Integer; const text: ustring): Boolean;
var
  TempText : TCefString;
begin
  TempText := CefString(text);
  Result   := PCefMenuModel(FData)^.set_label_at(PCefMenuModel(FData), index, @TempText) <> 0;
end;

function TCefMenuModelRef.SetVisible(commandId: Integer; visible: Boolean): Boolean;
begin
  Result := PCefMenuModel(FData)^.set_visible(PCefMenuModel(FData), commandId, Ord(visible)) <> 0;
end;

function TCefMenuModelRef.SetVisibleAt(index: Integer; visible: Boolean): Boolean;
begin
  Result := PCefMenuModel(FData)^.set_visible_at(PCefMenuModel(FData), index, Ord(visible)) <> 0;
end;

class function TCefMenuModelRef.UnWrap(data: Pointer): ICefMenuModel;
begin
  if (data <> nil) then
    Result := Create(data) as ICefMenuModel
   else
    Result := nil;
end;


end.
