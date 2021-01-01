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

unit uCEFLabelButtonComponent;

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
  uCEFTypes, uCEFInterfaces, uCEFViewsFrameworkEvents, uCEFButtonComponent;

type
  {$IFNDEF FPC}{$IFDEF DELPHI16_UP}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}{$ENDIF}
  TCEFLabelButtonComponent = class(TCEFButtonComponent)
    protected
      FLabelButton     : ICefLabelButton;
      FText            : ustring;

      procedure DestroyView; override;
      procedure Initialize; override;

      function  GetInitialized : boolean; override;
      function  GetAsView : ICefView; override;
      function  GetAsButton : ICefButton; override;
      function  GetAsLabelButton : ICefLabelButton; override;
      function  GetAsMenuButton : ICefMenuButton; virtual;
      function  GetText : ustring;
      function  GetImage(button_state: TCefButtonState): ICefImage;

      procedure SetText(const text_: ustring);
      procedure SetImage(button_state: TCefButtonState; const image: ICefImage);

      // ICefViewDelegateEvents
      procedure doCreateCustomView; override;

    public
      procedure CreateLabelButton(const aText : ustring);
      procedure SetTextColor(for_state: TCefButtonState; color: TCefColor);
      procedure SetEnabledTextColors(color: TCefColor);
      procedure SetFontList(const font_list: ustring);
      procedure SetHorizontalAlignment(alignment: TCefHorizontalAlignment);
      procedure SetMinimumSize(const size_: TCefSize);
      procedure SetMaximumSize(const size_: TCefSize);

      property  Text                                  : ustring         read GetText          write SetText;
      property  Image[button_state : TCefButtonState] : ICefImage       read GetImage         write SetImage;
      property  AsMenuButton                          : ICefMenuButton  read GetAsMenuButton;
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
  uCEFLabelButton, uCEFMiscFunctions, uCEFButtonDelegate;

procedure TCEFLabelButtonComponent.CreateLabelButton(const aText : ustring);
begin
  FText := aText;
  CreateView;
end;

procedure TCEFLabelButtonComponent.doCreateCustomView;
var
  TempDelegate : ICefButtonDelegate;
begin
  if (FLabelButton = nil) then
    try
      TempDelegate := TCustomButtonDelegate.Create(self);
      FLabelButton := TCefLabelButtonRef.CreateLabelButton(TempDelegate, FText);
    finally
      TempDelegate := nil;
    end;
end;

procedure TCEFLabelButtonComponent.DestroyView;
begin
  inherited DestroyView;

  FLabelButton := nil;
end;

procedure TCEFLabelButtonComponent.Initialize;
begin
  inherited Initialize;

  FLabelButton := nil;
end;

function TCEFLabelButtonComponent.GetInitialized : boolean;
begin
  Result := (FLabelButton <> nil);
end;

function TCEFLabelButtonComponent.GetAsView : ICefView;
begin
  if Initialized then
    Result := FLabelButton as ICefView
   else
    Result := nil;
end;

function TCEFLabelButtonComponent.GetAsButton : ICefButton;
begin
  if Initialized then
    Result := FLabelButton as ICefButton
   else
    Result := nil;
end;

function TCEFLabelButtonComponent.GetAsLabelButton : ICefLabelButton;
begin
  Result := FLabelButton;
end;

function TCEFLabelButtonComponent.GetAsMenuButton : ICefMenuButton;
begin
  Result := nil;
end;

procedure TCEFLabelButtonComponent.SetText(const text_: ustring);
begin
  FText := text_;

  if Initialized then
    AsLabelButton.SetText(FText);
end;

function TCEFLabelButtonComponent.GetText : ustring;
begin
  if Initialized then
    FText := AsLabelButton.GetText;

  Result := FText;
end;

procedure TCEFLabelButtonComponent.SetImage(button_state: TCefButtonState; const image: ICefImage);
begin
  if Initialized then AsLabelButton.SetImage(button_state, image);
end;

function TCEFLabelButtonComponent.GetImage(button_state: TCefButtonState): ICefImage;
begin
  if Initialized then
    Result := AsLabelButton.GetImage(button_state)
   else
    Result := nil;
end;

procedure TCEFLabelButtonComponent.SetTextColor(for_state: TCefButtonState; color: TCefColor);
begin
  if Initialized then AsLabelButton.SetTextColor(for_state, color);
end;

procedure TCEFLabelButtonComponent.SetEnabledTextColors(color: TCefColor);
begin
  if Initialized then AsLabelButton.SetEnabledTextColors(color);
end;

procedure TCEFLabelButtonComponent.SetFontList(const font_list: ustring);
begin
  if Initialized then AsLabelButton.SetFontList(font_list);
end;

procedure TCEFLabelButtonComponent.SetHorizontalAlignment(alignment: TCefHorizontalAlignment);
begin
  if Initialized then AsLabelButton.SetHorizontalAlignment(alignment);
end;

procedure TCEFLabelButtonComponent.SetMinimumSize(const size_: TCefSize);
begin
  if Initialized then AsLabelButton.SetMinimumSize(size_);
end;

procedure TCEFLabelButtonComponent.SetMaximumSize(const size_: TCefSize);
begin
  if Initialized then AsLabelButton.SetMaximumSize(size_);
end;

{$IFDEF FPC}
procedure Register;
begin
  {$I res/tceflabelbuttoncomponent.lrs}
  RegisterComponents('Chromium Views Framework', [TCEFLabelButtonComponent]);
end;
{$ENDIF}

end.
