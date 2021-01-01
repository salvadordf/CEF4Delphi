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

unit uCEFViewsFrameworkEvents;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes,
  {$ELSE}
  Classes,
  {$ENDIF}
  uCEFTypes, uCEFInterfaces;

type
  // ICefViewDelegate
  TOnGetPreferredSizeEvent  = procedure(const Sender: TObject; const view: ICefView; var aResult : TCefSize) of object;
  TOnGetMinimumSizeEvent    = procedure(const Sender: TObject; const view: ICefView; var aResult : TCefSize) of object;
  TOnGetMaximumSizeEvent    = procedure(const Sender: TObject; const view: ICefView; var aResult : TCefSize) of object;
  TOnGetHeightForWidthEvent = procedure(const Sender: TObject; const view: ICefView; width: Integer; var aResult: Integer) of object;
  TOnParentViewChangedEvent = procedure(const Sender: TObject; const view: ICefView; added: boolean; const parent: ICefView) of object;
  TOnChildViewChangedEvent  = procedure(const Sender: TObject; const view: ICefView; added: boolean; const child: ICefView) of object;
  TOnFocusEvent             = procedure(const Sender: TObject; const view: ICefView) of object;
  TOnBlurEvent              = procedure(const Sender: TObject; const view: ICefView) of object;

  // ICefTextfieldDelegate
  TOnTextfieldKeyEventEvent = procedure(const Sender: TObject; const textfield: ICefTextfield; const event: TCefKeyEvent; var aResult : boolean) of object;
  TOnAfterUserActionEvent   = procedure(const Sender: TObject; const textfield: ICefTextfield) of object;

  // ICefPanelDelegate

  // ICefBrowserViewDelegate
  TOnBrowserCreatedEvent                 = procedure(const Sender: TObject; const browser_view: ICefBrowserView; const browser: ICefBrowser) of object;
  TOnBrowserDestroyedEvent               = procedure(const Sender: TObject; const browser_view: ICefBrowserView; const browser: ICefBrowser) of object;
  TOnGetDelegateForPopupBrowserViewEvent = procedure(const Sender: TObject; const browser_view: ICefBrowserView; const settings: TCefBrowserSettings; const client: ICefClient; is_devtools: boolean; var aResult : ICefBrowserViewDelegate) of object;
  TOnPopupBrowserViewCreatedEvent        = procedure(const Sender: TObject; const browser_view, popup_browser_view: ICefBrowserView; is_devtools: boolean; var aResult : boolean) of object;

  // ICefButtonDelegate
  TOnButtonPressedEvent      = procedure(const Sender: TObject; const button: ICefButton) of object;
  TOnButtonStateChangedEvent = procedure(const Sender: TObject; const button: ICefButton) of object;

  // ICefMenuButtonDelegate
  TOnMenuButtonPressedEvent = procedure(const Sender: TObject; const menu_button: ICefMenuButton; const screen_point: TCefPoint; const button_pressed_lock: ICefMenuButtonPressedLock) of object;

  // ICefWindowDelegate
  TOnWindowCreatedEvent    = procedure(const Sender: TObject; const window: ICefWindow) of object;
  TOnWindowDestroyedEvent  = procedure(const Sender: TObject; const window: ICefWindow) of object;
  TOnGetParentWindowEvent  = procedure(const Sender: TObject; const window: ICefWindow; var is_menu, can_activate_menu: boolean; var aResult : ICefWindow) of object;
  TOnGetInitialBoundsEvent = procedure(const Sender: TObject; const window: ICefWindow; var aResult : TCefRect) of object;
  TOnIsFramelessEvent      = procedure(const Sender: TObject; const window: ICefWindow; var aResult : boolean) of object;
  TOnCanResizeEvent        = procedure(const Sender: TObject; const window: ICefWindow; var aResult : boolean) of object;
  TOnCanMaximizeEvent      = procedure(const Sender: TObject; const window: ICefWindow; var aResult : boolean) of object;
  TOnCanMinimizeEvent      = procedure(const Sender: TObject; const window: ICefWindow; var aResult : boolean) of object;
  TOnCanCloseEvent         = procedure(const Sender: TObject; const window: ICefWindow; var aResult : boolean) of object;
  TOnAcceleratorEvent      = procedure(const Sender: TObject; const window: ICefWindow; command_id: Integer; var aResult : boolean) of object;
  TOnWindowKeyEventEvent   = procedure(const Sender: TObject; const window: ICefWindow; const event: TCefKeyEvent; var aResult : boolean) of object;

implementation

end.
