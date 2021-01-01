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

unit uCEFTextfieldComponent;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

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
  uCEFTypes, uCEFInterfaces, uCEFViewsFrameworkEvents, uCEFViewComponent;

type
  {$IFNDEF FPC}{$IFDEF DELPHI16_UP}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}{$ENDIF}
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

      // ICefTextfieldDelegateEvents
      procedure doOnKeyEvent(const textfield: ICefTextfield; const event: TCefKeyEvent; var aResult : boolean);
      procedure doOnAfterUserAction(const textfield: ICefTextfield);

      // ICefViewDelegateEvents
      procedure doCreateCustomView; override;

    public
      procedure CreateTextField;
      procedure AppendText(const text_: ustring);
      procedure InsertOrReplaceText(const text_: ustring);
      procedure SelectAll(reversed: boolean);
      procedure ClearSelection;
      procedure SelectRange(const range: TCefRange);
      procedure SetFontList(const font_list: ustring);
      procedure ApplyTextColor(color: TCefColor; const range: TCefRange);
      procedure ApplyTextStyle(style: TCefTextStyle; add: boolean; const range: TCefRange);
      function  IsCommandEnabled(command_id: TCefTextFieldCommands): boolean;
      procedure ExecuteCommand(command_id: TCefTextFieldCommands);
      procedure ClearEditHistory;
      procedure SetAccessibleName(const name_: ustring);
      procedure SetPlaceholderTextColor(color: TCefColor);

      property  PasswordInput            : boolean       read GetIsPasswordInput            write SetPasswordInput;
      property  ReadOnly                 : boolean       read GetIsReadOnly                 write SetReadOnly;
      property  Text                     : ustring       read GetText                       write SetText;
      property  SelectedText             : ustring       read GetSelectedText;
      property  TextColor                : TCefColor     read GetTextColor                  write SetTextColor;
      property  SelectionTextColor       : TCefColor     read GetSelectionTextColor         write SetSelectionTextColor;
      property  SelectionBackgroundColor : TCefColor     read GetSelectionBackgroundColor   write SetSelectionBackgroundColor;
      property  PlaceholderText          : ustring       read GetPlaceholderText            write SetPlaceholderText;
      property  HasSelection             : boolean       read GetHasSelection;

    published
      property  OnTextfieldKeyEvent      : TOnTextfieldKeyEventEvent   read FOnTextfieldKeyEvent   write FOnTextfieldKeyEvent;
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

procedure TCEFTextfieldComponent.SelectRange(const range: TCefRange);
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
