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

unit uCEFMenuButtonComponent;

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
  uCEFTypes, uCEFInterfaces, uCEFViewsFrameworkEvents, uCEFLabelButtonComponent;

type
  {$IFNDEF FPC}{$IFDEF DELPHI16_UP}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}{$ENDIF}
  TCEFMenuButtonComponent = class(TCEFLabelButtonComponent, ICefMenuButtonDelegateEvents)
    protected
      FMenuButton                : ICefMenuButton;

      // ICefMenuButtonDelegateEvents
      FOnMenuButtonPressed       : TOnMenuButtonPressedEvent;

      procedure DestroyView; override;
      procedure Initialize; override;

      function  GetInitialized : boolean; override;
      function  GetAsView : ICefView; override;
      function  GetAsButton : ICefButton; override;
      function  GetAsLabelButton : ICefLabelButton; override;
      function  GetAsMenuButton : ICefMenuButton; override;

      // ICefMenuButtonDelegateEvents
      procedure doOnMenuButtonPressed(const menu_button: ICefMenuButton; const screen_point: TCefPoint; const button_pressed_lock: ICefMenuButtonPressedLock);

      // ICefViewDelegateEvents
      procedure doCreateCustomView; override;

    public
      procedure CreateMenuButton(const aText : ustring);
      procedure ShowMenu(const menu_model: ICefMenuModel; const screen_point: TCefPoint; anchor_position: TCefMenuAnchorPosition);
      procedure TriggerMenu;

    published
      property OnMenuButtonPressed   : TOnMenuButtonPressedEvent     read FOnMenuButtonPressed     write FOnMenuButtonPressed;
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
  uCEFMenuButtonDelegate, uCEFMenuButton;

procedure TCEFMenuButtonComponent.CreateMenuButton(const aText : ustring);
begin
  CreateLabelButton(aText);
end;

procedure TCEFMenuButtonComponent.doCreateCustomView;
var
  TempDelegate : ICefMenuButtonDelegate;
begin
  if (FMenuButton = nil) then
    try
      TempDelegate := TCustomMenuButtonDelegate.Create(self);
      FMenuButton  := TCefMenuButtonRef.CreateMenuButton(TempDelegate, FText);
    finally
      TempDelegate := nil;
    end;
end;

procedure TCEFMenuButtonComponent.Initialize;
begin
  inherited Initialize;

  FMenuButton          := nil;
  FOnMenuButtonPressed := nil;
end;

procedure TCEFMenuButtonComponent.DestroyView;
begin
  FMenuButton := nil;
end;

function TCEFMenuButtonComponent.GetInitialized : boolean;
begin
  Result := (FMenuButton <> nil);
end;

function TCEFMenuButtonComponent.GetAsView : ICefView;
begin
  Result := FMenuButton as ICefView;
end;

function TCEFMenuButtonComponent.GetAsButton : ICefButton;
begin
  if Initialized then
    Result := FMenuButton as ICefButton
   else
    Result := nil;
end;

function TCEFMenuButtonComponent.GetAsLabelButton : ICefLabelButton;
begin
  if Initialized then
    Result := FMenuButton as ICefLabelButton
   else
    Result := nil;
end;

function TCEFMenuButtonComponent.GetAsMenuButton : ICefMenuButton;
begin
  Result := FMenuButton;
end;

procedure TCEFMenuButtonComponent.doOnMenuButtonPressed(const menu_button         : ICefMenuButton;
                                                        const screen_point        : TCefPoint;
                                                        const button_pressed_lock : ICefMenuButtonPressedLock);
begin
  if assigned(FOnMenuButtonPressed) then
    FOnMenuButtonPressed(self, menu_button, screen_point, button_pressed_lock);
end;

procedure TCEFMenuButtonComponent.ShowMenu(const menu_model      : ICefMenuModel;
                                           const screen_point    : TCefPoint;
                                                 anchor_position : TCefMenuAnchorPosition);
begin
  if Initialized then
    FMenuButton.ShowMenu(menu_model, screen_point, anchor_position);
end;

procedure TCEFMenuButtonComponent.TriggerMenu;
begin
  if Initialized then
    FMenuButton.TriggerMenu;
end;

{$IFDEF FPC}
procedure Register;
begin
  {$I res/tcefmenubuttoncomponent.lrs}
  RegisterComponents('Chromium Views Framework', [TCEFMenuButtonComponent]);
end;
{$ENDIF}

end.
