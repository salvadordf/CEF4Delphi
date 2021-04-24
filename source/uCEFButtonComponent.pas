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

unit uCEFButtonComponent;

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
  TCEFButtonComponent = class(TCEFViewComponent, ICefButtonDelegateEvents)
    protected
      FButton                     : ICefButton;

      // ICefButtonDelegateEvents
      FOnButtonPressed            : TOnButtonPressedEvent;
      FOnButtonStateChanged       : TOnButtonStateChangedEvent;

      procedure DestroyView; override;
      procedure Initialize; override;

      function  GetInitialized : boolean; override;
      function  GetAsView : ICefView; override;
      function  GetState : TCefButtonState;
      function  GetAsButton : ICefButton; override;
      function  GetAsLabelButton : ICefLabelButton; virtual;

      procedure SetState(state_: TCefButtonState);

      // ICefButtonDelegateEvents
      procedure doOnButtonPressed(const button: ICefButton);
      procedure doOnButtonStateChanged(const button: ICefButton);

    public
      procedure SetInkDropEnabled(enabled_: boolean);
      procedure SetTooltipText(const tooltip_text: ustring);
      procedure SetAccessibleName(const name_: ustring);

      property AsLabelButton          : ICefLabelButton             read GetAsLabelButton;
      property State                  : TCefButtonState             read GetState                  write SetState;

    published
      property OnButtonPressed        : TOnButtonPressedEvent       read FOnButtonPressed          write FOnButtonPressed;
      property OnButtonStateChanged   : TOnButtonStateChangedEvent  read FOnButtonStateChanged     write FOnButtonStateChanged;
  end;

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
  uCEFButtonDelegate;

procedure TCEFButtonComponent.Initialize;
begin
  inherited Initialize;

  FButton                     := nil;
  FOnButtonPressed            := nil;
  FOnButtonStateChanged       := nil;
end;

procedure TCEFButtonComponent.DestroyView;
begin
  FButton := nil;
end;

function TCEFButtonComponent.GetInitialized : boolean;
begin
  Result := (FButton <> nil);
end;

function TCEFButtonComponent.GetAsView : ICefView;
begin
  Result := FButton as ICefView;
end;

function TCEFButtonComponent.GetAsButton : ICefButton;
begin
  Result := FButton;
end;

function TCEFButtonComponent.GetAsLabelButton : ICefLabelButton;
begin
  Result := nil;
end;

procedure TCEFButtonComponent.doOnButtonPressed(const button: ICefButton);
begin
  if assigned(FOnButtonPressed) then
    FOnButtonPressed(self, button);
end;

procedure TCEFButtonComponent.doOnButtonStateChanged(const button: ICefButton);
begin
  if assigned(FOnButtonStateChanged) then
    FOnButtonStateChanged(self, button);
end;

procedure TCEFButtonComponent.SetState(state_: TCefButtonState);
begin
  if Initialized then AsButton.SetState(state_);
end;

function TCEFButtonComponent.GetState : TCefButtonState;
begin
  if Initialized then
    Result := AsButton.GetState
   else
    Result := CEF_BUTTON_STATE_NORMAL;
end;

procedure TCEFButtonComponent.SetInkDropEnabled(enabled_: boolean);
begin
  if Initialized then AsButton.SetInkDropEnabled(enabled_);
end;

procedure TCEFButtonComponent.SetTooltipText(const tooltip_text: ustring);
begin
  if Initialized then AsButton.SetTooltipText(tooltip_text);
end;

procedure TCEFButtonComponent.SetAccessibleName(const name_: ustring);
begin
  if Initialized then AsButton.SetAccessibleName(name_);
end;


end.
