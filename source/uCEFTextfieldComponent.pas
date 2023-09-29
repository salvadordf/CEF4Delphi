unit uCEFTextfieldComponent;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
    {$IFDEF MSWINDOWS}WinApi.Windows,{$ENDIF} System.Classes,
  {$ELSE}
    {$IFDEF MSWINDOWS}Windows,{$ENDIF} Classes,
    {$IFDEF FPC}
    LCLProc, LCLType, LCLIntf, LResources, InterfaceBase,
    {$ENDIF}
  {$ENDIF}
  uCEFTypes, uCEFInterfaces, uCEFConstants, uCEFViewsFrameworkEvents, uCEFViewComponent;

type
  {$IFNDEF FPC}{$IFDEF DELPHI16_UP}[ComponentPlatformsAttribute(pfidWindows or pfidOSX or pfidLinux)]{$ENDIF}{$ENDIF}
  TCEFTextfieldComponent = class(TCEFViewComponent, ICefTextfieldDelegateEvents)
    protected
      FTextfield                  : ICefTextfield;

      // ICefTextfieldDelegateEvents
      FOnTextfieldKeyEvent        : TOnTextfieldKeyEventEvent;
      FOnAfterUserAction          : TOnAfterUserActionEvent;

      procedure DestroyView; override;
      procedure Initialize; override;

      function  GetInitialized : boolean; override;
      function  GetAsView : ICefView; override;
      function  GetAsTextfield : ICefTextfield; override;
      function  GetIsPasswordInput : boolean;
      function  GetIsReadOnly : boolean;
      function  GetText : ustring;
      function  GetHasSelection : boolean;
      function  GetSelectedText : ustring;
      function  GetSelectedRange : TCefRange;
      function  GetCursorPosition : NativeUInt;
      function  GetTextColor : TCefColor;
      function  GetSelectionTextColor : TCefColor;
      function  GetSelectionBackgroundColor : TCefColor;
      function  GetPlaceholderText : ustring;

      procedure SetPasswordInput(password_input: boolean);
      procedure SetReadOnly(read_only: boolean);
      procedure SetText(const text_: ustring);
      procedure SetTextColor(color: TCefColor);
      procedure SetSelectionTextColor(color: TCefColor);
      procedure SetSelectionBackgroundColor(color: TCefColor);
      procedure SetPlaceholderText(const text_: ustring);
      procedure SetSelectedRange(const range: TCefRange);

      // ICefTextfieldDelegateEvents
      procedure doOnKeyEvent(const textfield: ICefTextfield; const event: TCefKeyEvent; var aResult : boolean);
      procedure doOnAfterUserAction(const textfield: ICefTextfield);

      // ICefViewDelegateEvents
      procedure doCreateCustomView; override;

    public
      /// <summary>
      /// Create a new Textfield.
      /// </summary>
      procedure CreateTextField;
      /// <summary>
      /// Appends |text| to the previously-existing text.
      /// </summary>
      procedure AppendText(const text_: ustring);
      /// <summary>
      /// Inserts |text| at the current cursor position replacing any selected text.
      /// </summary>
      procedure InsertOrReplaceText(const text_: ustring);
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
      /// <para>Sets the font list. The format is "<FONT_FAMILY_LIST>,[STYLES] <SIZE>",
      /// where:</para>
      /// <code>
      /// - FONT_FAMILY_LIST is a comma-separated list of font family names,
      /// - STYLES is an optional space-separated list of style names (case-sensitive
      ///   "Bold" and "Italic" are supported), and
      /// - SIZE is an integer font size in pixels with the suffix "px".
      /// </code>
      /// <para>Here are examples of valid font description strings:</para>
      /// <code>
      /// - "Arial, Helvetica, Bold Italic 14px"
      /// - "Arial, 14px"
      /// </code>
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
      /// Set the accessible name that will be exposed to assistive technology (AT).
      /// </summary>
      procedure SetAccessibleName(const name_: ustring);
      /// <summary>
      /// Sets the placeholder text color.
      /// </summary>
      procedure SetPlaceholderTextColor(color: TCefColor);
      /// <summary>
      /// Returns true (1) if the text will be displayed as asterisks.
      /// </summary>
      property  PasswordInput            : boolean       read GetIsPasswordInput            write SetPasswordInput;
      /// <summary>
      /// Returns true (1) if the text is read-only.
      /// </summary>
      property  ReadOnly                 : boolean       read GetIsReadOnly                 write SetReadOnly;
      /// <summary>
      /// Returns the currently displayed text.
      /// </summary>
      property  Text                     : ustring       read GetText                       write SetText;
      /// <summary>
      /// Returns the currently selected text.
      /// </summary>
      property  SelectedText             : ustring       read GetSelectedText;
      /// <summary>
      /// Returns the selected logical text range.
      /// </summary>
      property  SelectedRange            : TCefRange     read GetSelectedRange              write SetSelectedRange;
      /// <summary>
      /// Returns the current cursor position.
      /// </summary>
      property  CursorPosition           : NativeUInt    read GetCursorPosition;
      /// <summary>
      /// Returns the text color.
      /// </summary>
      property  TextColor                : TCefColor     read GetTextColor                  write SetTextColor;
      /// <summary>
      /// Returns the selection text color.
      /// </summary>
      property  SelectionTextColor       : TCefColor     read GetSelectionTextColor         write SetSelectionTextColor;
      /// <summary>
      /// Returns the selection background color.
      /// </summary>
      property  SelectionBackgroundColor : TCefColor     read GetSelectionBackgroundColor   write SetSelectionBackgroundColor;
      /// <summary>
      /// Returns the placeholder text that will be displayed when the Textfield is
      /// NULL.
      /// </summary>
      property  PlaceholderText          : ustring       read GetPlaceholderText            write SetPlaceholderText;
      /// <summary>
      /// Returns true (1) if there is any selected text.
      /// </summary>
      property  HasSelection             : boolean       read GetHasSelection;

    published
      /// <summary>
      /// Called when |textfield| recieves a keyboard event. |event| contains
      /// information about the keyboard event. Return true (1) if the keyboard
      /// event was handled or false (0) otherwise for default handling.
      /// </summary>
      property  OnTextfieldKeyEvent      : TOnTextfieldKeyEventEvent   read FOnTextfieldKeyEvent   write FOnTextfieldKeyEvent;
      /// <summary>
      /// Called after performing a user action that may change |textfield|.
      /// </summary>
      property  OnAfterUserAction        : TOnAfterUserActionEvent     read FOnAfterUserAction     write FOnAfterUserAction;
  end;

{$IFDEF FPC}
procedure Register;
{$ENDIF}

// *********************************************************
// ********************** ATTENTION ! **********************
// *********************************************************
// **                                                     **
// **  MANY OF THE EVENTS IN CEF4DELPHI COMPONENTS LIKE   **
// **  TCHROMIUM, TFMXCHROMIUM OR TCEFAPPLICATION ARE     **
// **  EXECUTED IN A CEF THREAD BY DEFAULT.               **
// **                                                     **
// **  WINDOWS CONTROLS MUST BE CREATED AND DESTROYED IN  **
// **  THE SAME THREAD TO AVOID ERRORS.                   **
// **  SOME OF THEM RECREATE THE HANDLERS IF THEY ARE     **
// **  MODIFIED AND CAN CAUSE THE SAME ERRORS.            **
// **                                                     **
// **  DON'T CREATE, MODIFY OR DESTROY WINDOWS CONTROLS   **
// **  INSIDE THE CEF4DELPHI EVENTS AND USE               **
// **  SYNCHRONIZATION OBJECTS TO PROTECT VARIABLES AND   **
// **  FIELDS IF THEY ARE ALSO USED IN THE MAIN THREAD.   **
// **                                                     **
// **  READ THIS FOR MORE INFORMATION :                   **
// **  https://www.briskbard.com/index.php?pageid=cef     **
// **                                                     **
// **  USE OUR FORUMS FOR MORE QUESTIONS :                **
// **  https://www.briskbard.com/forum/                   **
// **                                                     **
// *********************************************************
// *********************************************************

implementation

uses
  uCEFTextfieldDelegate, uCEFMiscFunctions, uCEFTask, uCEFTextfield;

procedure TCEFTextfieldComponent.Initialize;
begin
  inherited Initialize;

  FTextfield           := nil;
  FOnTextfieldKeyEvent := nil;
  FOnAfterUserAction   := nil;
end;

procedure TCEFTextfieldComponent.DestroyView;
begin
  FTextfield := nil;
end;

procedure TCEFTextfieldComponent.CreateTextField;
begin
  CreateView;
end;

procedure TCEFTextfieldComponent.doOnKeyEvent(const textfield : ICefTextfield;
                                              const event     : TCefKeyEvent;
                                              var   aResult   : boolean);
begin
  if assigned(FOnTextfieldKeyEvent) then
    FOnTextfieldKeyEvent(self, textfield, event, aResult);
end;

procedure TCEFTextfieldComponent.doOnAfterUserAction(const textfield: ICefTextfield);
begin
  if assigned(FOnAfterUserAction) then
    FOnAfterUserAction(self, textfield);
end;

procedure TCEFTextfieldComponent.doCreateCustomView;
var
  TempDelegate : ICefTextfieldDelegate;
begin
  if (FTextfield = nil) then
    try
      TempDelegate := TCustomTextfieldDelegate.Create(self);
      FTextfield   := TCefTextfieldRef.CreateTextField(TempDelegate);
    finally
      TempDelegate := nil;
    end;
end;

function TCEFTextfieldComponent.GetInitialized : boolean;
begin
  Result := (FTextfield <> nil);
end;

function TCEFTextfieldComponent.GetAsView : ICefView;
begin
  Result := FTextfield as ICefView;
end;

function TCEFTextfieldComponent.GetAsTextfield : ICefTextfield;
begin
  Result := FTextfield;
end;

function TCEFTextfieldComponent.GetIsPasswordInput : boolean;
begin
  Result := Initialized and FTextfield.IsPasswordInput;
end;

function TCEFTextfieldComponent.GetIsReadOnly : boolean;
begin
  Result := Initialized and FTextfield.IsReadOnly;
end;

function TCEFTextfieldComponent.GetText : ustring;
begin
  if Initialized then
    Result := FTextfield.GetText
   else
    Result := '';
end;

function TCEFTextfieldComponent.GetHasSelection : boolean;
begin
  Result := Initialized and FTextfield.HasSelection;
end;

function TCEFTextfieldComponent.GetSelectedText : ustring;
begin
  if Initialized then
    Result := FTextfield.SelectedText
   else
    Result := '';
end;

function TCEFTextfieldComponent.GetSelectedRange : TCefRange;
var
  TempRange : TCefRange;
begin
  if Initialized then
    TempRange := FTextfield.GetSelectedRange
   else
    begin
      TempRange.from := 0;
      TempRange.to_  := 0;
    end;

  Result := TempRange;
end;

function TCEFTextfieldComponent.GetCursorPosition : NativeUInt;
begin
  if Initialized then
    Result := FTextfield.GetCursorPosition
   else
    Result := 0;
end;

function TCEFTextfieldComponent.GetTextColor : TCefColor;
begin
  if Initialized then
    Result := FTextfield.GetTextColor
   else
    Result := 0;
end;

function TCEFTextfieldComponent.GetSelectionTextColor : TCefColor;
begin
  if Initialized then
    Result := FTextfield.GetSelectionTextColor
   else
    Result := 0;
end;

function TCEFTextfieldComponent.GetSelectionBackgroundColor : TCefColor;
begin
  if Initialized then
    Result := FTextfield.GetSelectionBackgroundColor
   else
    Result := 0;
end;

function TCEFTextfieldComponent.GetPlaceholderText : ustring;
begin
  if Initialized then
    Result := FTextfield.GetPlaceholderText
   else
    Result := '';
end;

procedure TCEFTextfieldComponent.SetPasswordInput(password_input: boolean);
begin
  if Initialized then
    FTextfield.SetPasswordInput(password_input);
end;

procedure TCEFTextfieldComponent.SetReadOnly(read_only: boolean);
begin
  if Initialized then
    FTextfield.SetReadOnly(read_only);
end;

procedure TCEFTextfieldComponent.SetText(const text_: ustring);
begin
  if Initialized then
    FTextfield.SetText(text_);
end;

procedure TCEFTextfieldComponent.SetTextColor(color: TCefColor);
begin
  if Initialized then
    FTextfield.SetTextColor(color);
end;

procedure TCEFTextfieldComponent.SetSelectionTextColor(color: TCefColor);
begin
  if Initialized then
    FTextfield.SetSelectionTextColor(color);
end;

procedure TCEFTextfieldComponent.SetSelectionBackgroundColor(color: TCefColor);
begin
  if Initialized then
    FTextfield.SetSelectionBackgroundColor(color);
end;

procedure TCEFTextfieldComponent.SetPlaceholderText(const text_: ustring);
begin
  if Initialized then
    FTextfield.SetPlaceholderText(text_);
end;

procedure TCEFTextfieldComponent.AppendText(const text_: ustring);
begin
  if Initialized then
    FTextfield.AppendText(text_);
end;

procedure TCEFTextfieldComponent.InsertOrReplaceText(const text_: ustring);
begin
  if Initialized then
    FTextfield.InsertOrReplaceText(text_);
end;

procedure TCEFTextfieldComponent.SelectAll(reversed: boolean);
begin
  if Initialized then
    FTextfield.SelectAll(reversed);
end;

procedure TCEFTextfieldComponent.ClearSelection;
begin
  if Initialized then
    FTextfield.ClearSelection;
end;

procedure TCEFTextfieldComponent.SetSelectedRange(const range: TCefRange);
begin
  if Initialized then
    FTextfield.SelectRange(range);
end;

procedure TCEFTextfieldComponent.SetFontList(const font_list: ustring);
begin
  if Initialized then
    FTextfield.SetFontList(font_list);
end;

procedure TCEFTextfieldComponent.ApplyTextColor(color: TCefColor; const range: TCefRange);
begin
  if Initialized then
    FTextfield.ApplyTextColor(color, range);
end;

procedure TCEFTextfieldComponent.ApplyTextStyle(style: TCefTextStyle; add: boolean; const range: TCefRange);
begin
  if Initialized then
    FTextfield.ApplyTextStyle(style, add, range);
end;

function TCEFTextfieldComponent.IsCommandEnabled(command_id: TCefTextFieldCommands): boolean;
begin
  Result := Initialized and FTextfield.IsCommandEnabled(command_id);
end;

procedure TCEFTextfieldComponent.ExecuteCommand(command_id: TCefTextFieldCommands);
begin
  if Initialized then
    FTextfield.ExecuteCommand(command_id);
end;

procedure TCEFTextfieldComponent.ClearEditHistory;
begin
  if Initialized then
    FTextfield.ClearEditHistory;
end;

procedure TCEFTextfieldComponent.SetAccessibleName(const name_: ustring);
begin
  if Initialized then
    FTextfield.SetAccessibleName(name_);
end;

procedure TCEFTextfieldComponent.SetPlaceholderTextColor(color: TCefColor);
begin
  if Initialized then
    FTextfield.SetPlaceholderTextColor(color);
end;

{$IFDEF FPC}
procedure Register;
begin
  {$I res/tceftextfieldcomponent.lrs}
  RegisterComponents('Chromium Views Framework', [TCEFTextfieldComponent]);
end;
{$ENDIF}


end.
