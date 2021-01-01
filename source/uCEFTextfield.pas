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

unit uCEFTextfield;

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
  TCefTextfieldRef = class(TCefViewRef, ICefTextfield)
    protected
      procedure SetPasswordInput(password_input: boolean);
      function  IsPasswordInput : boolean;
      procedure SetReadOnly(read_only: boolean);
      function  IsReadOnly : boolean;
      function  GetText : ustring;
      procedure SetText(const text_: ustring);
      procedure AppendText(const text_: ustring);
      procedure InsertOrReplaceText(const text_: ustring);
      function  HasSelection : boolean;
      function  GetSelectedText : ustring;
      procedure SelectAll(reversed: boolean);
      procedure ClearSelection;
      function  GetSelectedRange : TCefRange;
      procedure SelectRange(const range: TCefRange);
      function  GetCursorPosition : NativeUInt;
      procedure SetTextColor(color: TCefColor);
      function  GetTextColor : TCefColor;
      procedure SetSelectionTextColor(color: TCefColor);
      function  GetSelectionTextColor : TCefColor;
      procedure SetSelectionBackgroundColor(color: TCefColor);
      function  GetSelectionBackgroundColor : TCefColor;
      procedure SetFontList(const font_list: ustring);
      procedure ApplyTextColor(color: TCefColor; const range: TCefRange);
      procedure ApplyTextStyle(style: TCefTextStyle; add: boolean; const range: TCefRange);
      function  IsCommandEnabled(command_id: TCefTextFieldCommands): boolean;
      procedure ExecuteCommand(command_id: TCefTextFieldCommands);
      procedure ClearEditHistory;
      procedure SetPlaceholderText(const text_: ustring);
      function  GetPlaceholderText : ustring;
      procedure SetPlaceholderTextColor(color: TCefColor);
      procedure SetAccessibleName(const name: ustring);

    public
      class function UnWrap(data: Pointer): ICefTextfield;
      class function CreateTextField(const delegate: ICefTextfieldDelegate): ICefTextfield;
  end;

implementation

uses
  uCEFLibFunctions, uCEFMiscFunctions;

procedure TCefTextfieldRef.SetPasswordInput(password_input: boolean);
begin
  PCefTextfield(FData)^.set_password_input(PCefTextfield(FData),
                                           ord(password_input));
end;

function TCefTextfieldRef.IsPasswordInput : boolean;
begin
  Result := (PCefTextfield(FData)^.is_password_input(PCefTextfield(FData)) <> 0);
end;

procedure TCefTextfieldRef.SetReadOnly(read_only: boolean);
begin
  PCefTextfield(FData)^.set_read_only(PCefTextfield(FData),
                                      ord(read_only));
end;

function TCefTextfieldRef.IsReadOnly : boolean;
begin
  Result := (PCefTextfield(FData)^.is_read_only(PCefTextfield(FData)) <> 0);
end;

function TCefTextfieldRef.GetText : ustring;
begin
  Result := CefStringFreeAndGet(PCefTextfield(FData)^.get_text(PCefTextfield(FData)));
end;

procedure TCefTextfieldRef.SetText(const text_: ustring);
var
  TempText : TCefString;
begin
  TempText := CefString(text_);
  PCefTextfield(FData)^.set_text(PCefTextfield(FData), @TempText);
end;

procedure TCefTextfieldRef.AppendText(const text_: ustring);
var
  TempText : TCefString;
begin
  TempText := CefString(text_);
  PCefTextfield(FData)^.append_text(PCefTextfield(FData), @TempText);
end;

procedure TCefTextfieldRef.InsertOrReplaceText(const text_: ustring);
var
  TempText : TCefString;
begin
  TempText := CefString(text_);
  PCefTextfield(FData)^.insert_or_replace_text(PCefTextfield(FData), @TempText);
end;

function TCefTextfieldRef.HasSelection : boolean;
begin
  Result := (PCefTextfield(FData)^.has_selection(PCefTextfield(FData)) <> 0);
end;

function TCefTextfieldRef.GetSelectedText : ustring;
begin
  Result := CefStringFreeAndGet(PCefTextfield(FData)^.get_selected_text(PCefTextfield(FData)));
end;

procedure TCefTextfieldRef.SelectAll(reversed: boolean);
begin
  PCefTextfield(FData)^.select_all(PCefTextfield(FData), ord(reversed));
end;

procedure TCefTextfieldRef.ClearSelection;
begin
  PCefTextfield(FData)^.clear_selection(PCefTextfield(FData));
end;

function TCefTextfieldRef.GetSelectedRange : TCefRange;
begin
  Result := PCefTextfield(FData)^.get_selected_range(PCefTextfield(FData));
end;

procedure TCefTextfieldRef.SelectRange(const range: TCefRange);
begin
  PCefTextfield(FData)^.select_range(PCefTextfield(FData), @range);
end;

function TCefTextfieldRef.GetCursorPosition : NativeUInt;
begin
  Result := PCefTextfield(FData)^.get_cursor_position(PCefTextfield(FData));
end;

procedure TCefTextfieldRef.SetTextColor(color: TCefColor);
begin
  PCefTextfield(FData)^.set_text_color(PCefTextfield(FData), color);
end;

function TCefTextfieldRef.GetTextColor : TCefColor;
begin
  Result := PCefTextfield(FData)^.get_text_color(PCefTextfield(FData));
end;

procedure TCefTextfieldRef.SetSelectionTextColor(color: TCefColor);
begin
  PCefTextfield(FData)^.set_selection_text_color(PCefTextfield(FData), color);
end;

function TCefTextfieldRef.GetSelectionTextColor : TCefColor;
begin
  Result := PCefTextfield(FData)^.get_selection_text_color(PCefTextfield(FData));
end;

procedure TCefTextfieldRef.SetSelectionBackgroundColor(color: TCefColor);
begin
  PCefTextfield(FData)^.set_selection_background_color(PCefTextfield(FData), color);
end;

function TCefTextfieldRef.GetSelectionBackgroundColor : TCefColor;
begin
  Result := PCefTextfield(FData)^.get_selection_background_color(PCefTextfield(FData));
end;

procedure TCefTextfieldRef.SetFontList(const font_list: ustring);
var
  TempFontList : TCefString;
begin
  TempFontList := CefString(font_list);
  PCefTextfield(FData)^.set_font_list(PCefTextfield(FData), @TempFontList);
end;

procedure TCefTextfieldRef.ApplyTextColor(color: TCefColor; const range: TCefRange);
begin
  PCefTextfield(FData)^.apply_text_color(PCefTextfield(FData), color, @range);
end;

procedure TCefTextfieldRef.ApplyTextStyle(style: TCefTextStyle; add: boolean; const range: TCefRange);
begin
  PCefTextfield(FData)^.apply_text_style(PCefTextfield(FData), style, ord(add), @range);
end;

function TCefTextfieldRef.IsCommandEnabled(command_id: TCefTextFieldCommands): boolean;
begin
  Result := (PCefTextfield(FData)^.is_command_enabled(PCefTextfield(FData), command_id) <> 0);
end;

procedure TCefTextfieldRef.ExecuteCommand(command_id: TCefTextFieldCommands);
begin
  PCefTextfield(FData)^.execute_command(PCefTextfield(FData), command_id);
end;

procedure TCefTextfieldRef.ClearEditHistory;
begin
  PCefTextfield(FData)^.clear_edit_history(PCefTextfield(FData));
end;

procedure TCefTextfieldRef.SetPlaceholderText(const text_: ustring);
var
  TempText : TCefString;
begin
  TempText := CefString(text_);
  PCefTextfield(FData)^.set_placeholder_text(PCefTextfield(FData), @TempText);
end;

function TCefTextfieldRef.GetPlaceholderText : ustring;
begin
  Result := CefStringFreeAndGet(PCefTextfield(FData)^.get_placeholder_text(PCefTextfield(FData)));
end;

procedure TCefTextfieldRef.SetPlaceholderTextColor(color: TCefColor);
begin
  PCefTextfield(FData)^.set_placeholder_text_color(PCefTextfield(FData), color);
end;

procedure TCefTextfieldRef.SetAccessibleName(const name: ustring);
var
  TempName : TCefString;
begin
  TempName := CefString(name);
  PCefTextfield(FData)^.set_accessible_name(PCefTextfield(FData), @TempName);
end;

class function TCefTextfieldRef.UnWrap(data: Pointer): ICefTextfield;
begin
  if (data <> nil) then
    Result := Create(data) as ICefTextfield
   else
    Result := nil;
end;

class function TCefTextfieldRef.CreateTextField(const delegate: ICefTextfieldDelegate): ICefTextfield;
var
  TempTextfield : PCefTextfield;
begin
  Result := nil;

  if (delegate <> nil) then
    begin
      TempTextfield := cef_textfield_create(CefGetData(delegate));

      if (TempTextfield <> nil) then
        Result := Create(TempTextfield) as ICefTextfield;
    end;
end;

end.

