unit uCEFTextfield;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes, System.SysUtils,
  {$ELSE}
  Classes, SysUtils,
  {$ENDIF}
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes, uCEFView;

type
  /// <summary>
  /// A Textfield supports editing of text. This control is custom rendered with
  /// no platform-specific code. Methods must be called on the browser process UI
  /// thread unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_textfield_capi.h">CEF source file: /include/capi/views/cef_textfield_capi.h (cef_textfield_t)</see></para>
  /// </remarks>
  TCefTextfieldRef = class(TCefViewRef, ICefTextfield)
    protected
      /// <summary>
      /// Sets whether the text will be displayed as asterisks.
      /// </summary>
      procedure SetPasswordInput(password_input: boolean);
      /// <summary>
      /// Returns true (1) if the text will be displayed as asterisks.
      /// </summary>
      function  IsPasswordInput : boolean;
      /// <summary>
      /// Sets whether the text will read-only.
      /// </summary>
      procedure SetReadOnly(read_only: boolean);
      /// <summary>
      /// Returns true (1) if the text is read-only.
      /// </summary>
      function  IsReadOnly : boolean;
      /// <summary>
      /// Returns the currently displayed text.
      /// </summary>
      function  GetText : ustring;
      /// <summary>
      /// Sets the contents to |text|. The cursor will be moved to end of the text
      /// if the current position is outside of the text range.
      /// </summary>
      procedure SetText(const text_: ustring);
      /// <summary>
      /// Appends |text| to the previously-existing text.
      /// </summary>
      procedure AppendText(const text_: ustring);
      /// <summary>
      /// Inserts |text| at the current cursor position replacing any selected text.
      /// </summary>
      procedure InsertOrReplaceText(const text_: ustring);
      /// <summary>
      /// Returns true (1) if there is any selected text.
      /// </summary>
      function  HasSelection : boolean;
      /// <summary>
      /// Returns the currently selected text.
      /// </summary>
      function  GetSelectedText : ustring;
      /// <summary>
      /// Selects all text. If |reversed| is true (1) the range will end at the
      /// logical beginning of the text; this generally shows the leading portion of
      /// text that overflows its display area.
      /// </summary>
      procedure SelectAll(reversed: boolean);
      /// <summary>
      /// Clears the text selection and sets the caret to the end.
      /// </summary>
      procedure ClearSelection;
      /// <summary>
      /// Returns the selected logical text range.
      /// </summary>
      function  GetSelectedRange : TCefRange;
      /// <summary>
      /// Selects the specified logical text range.
      /// </summary>
      procedure SelectRange(const range: TCefRange);
      /// <summary>
      /// Returns the current cursor position.
      /// </summary>
      function  GetCursorPosition : NativeUInt;
      /// <summary>
      /// Sets the text color.
      /// </summary>
      procedure SetTextColor(color: TCefColor);
      /// <summary>
      /// Returns the text color.
      /// </summary>
      function  GetTextColor : TCefColor;
      /// <summary>
      /// Sets the selection text color.
      /// </summary>
      procedure SetSelectionTextColor(color: TCefColor);
      /// <summary>
      /// Returns the selection text color.
      /// </summary>
      function  GetSelectionTextColor : TCefColor;
      /// <summary>
      /// Sets the selection background color.
      /// </summary>
      procedure SetSelectionBackgroundColor(color: TCefColor);
      /// <summary>
      /// Returns the selection background color.
      /// </summary>
      function  GetSelectionBackgroundColor : TCefColor;
      /// <summary>
      /// Sets the font list. The format is "<FONT_FAMILY_LIST>,[STYLES] <SIZE>",
      /// where:
      /// - FONT_FAMILY_LIST is a comma-separated list of font family names,
      /// - STYLES is an optional space-separated list of style names (case-sensitive
      ///   "Bold" and "Italic" are supported), and
      /// - SIZE is an integer font size in pixels with the suffix "px".
      ///
      /// Here are examples of valid font description strings:
      /// - "Arial, Helvetica, Bold Italic 14px"
      /// - "Arial, 14px"
      /// </summary>
      procedure SetFontList(const font_list: ustring);
      /// <summary>
      /// Applies |color| to the specified |range| without changing the default
      /// color. If |range| is NULL the color will be set on the complete text
      /// contents.
      /// </summary>
      procedure ApplyTextColor(color: TCefColor; const range: TCefRange);
      /// <summary>
      /// Applies |style| to the specified |range| without changing the default
      /// style. If |add| is true (1) the style will be added, otherwise the style
      /// will be removed. If |range| is NULL the style will be set on the complete
      /// text contents.
      /// </summary>
      procedure ApplyTextStyle(style: TCefTextStyle; add: boolean; const range: TCefRange);
      /// <summary>
      /// Returns true (1) if the action associated with the specified command id is
      /// enabled. See additional comments on execute_command().
      /// </summary>
      function  IsCommandEnabled(command_id: TCefTextFieldCommands): boolean;
      /// <summary>
      /// Performs the action associated with the specified command id.
      /// </summary>
      procedure ExecuteCommand(command_id: TCefTextFieldCommands);
      /// <summary>
      /// Clears Edit history.
      /// </summary>
      procedure ClearEditHistory;
      /// <summary>
      /// Sets the placeholder text that will be displayed when the Textfield is
      /// NULL.
      /// </summary>
      procedure SetPlaceholderText(const text_: ustring);
      /// <summary>
      /// Returns the placeholder text that will be displayed when the Textfield is
      /// NULL.
      /// </summary>
      function  GetPlaceholderText : ustring;
      /// <summary>
      /// Sets the placeholder text color.
      /// </summary>
      procedure SetPlaceholderTextColor(color: TCefColor);
      /// <summary>
      /// Set the accessible name that will be exposed to assistive technology (AT).
      /// </summary>
      procedure SetAccessibleName(const name: ustring);

    public
      /// <summary>
      /// Returns a ICefTextfield instance using a PCefTextfield data pointer.
      /// </summary>
      class function UnWrap(data: Pointer): ICefTextfield;
      /// <summary>
      /// Create a new Textfield.
      /// </summary>
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

