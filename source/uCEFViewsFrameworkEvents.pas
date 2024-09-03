unit uCEFViewsFrameworkEvents;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

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
  TOnWindowChangedEvent     = procedure(const Sender: TObject; const view: ICefView; added: boolean) of object;
  TOnLayoutChangedEvent     = procedure(const Sender: TObject; const view: ICefView; new_bounds: TCefRect) of object;
  TOnFocusEvent             = procedure(const Sender: TObject; const view: ICefView) of object;
  TOnBlurEvent              = procedure(const Sender: TObject; const view: ICefView) of object;
  TOnThemeChangedEvent      = procedure(const Sender: TObject; const view: ICefView) of object;

  // ICefTextfieldDelegate
  TOnTextfieldKeyEventEvent = procedure(const Sender: TObject; const textfield: ICefTextfield; const event: TCefKeyEvent; var aResult : boolean) of object;
  TOnAfterUserActionEvent   = procedure(const Sender: TObject; const textfield: ICefTextfield) of object;

  // ICefPanelDelegate

  // ICefBrowserViewDelegate
  TOnBrowserCreatedEvent                   = procedure(const Sender: TObject; const browser_view: ICefBrowserView; const browser: ICefBrowser) of object;
  TOnBrowserDestroyedEvent                 = procedure(const Sender: TObject; const browser_view: ICefBrowserView; const browser: ICefBrowser) of object;
  TOnGetDelegateForPopupBrowserViewEvent   = procedure(const Sender: TObject; const browser_view: ICefBrowserView; const settings: TCefBrowserSettings; const client: ICefClient; is_devtools: boolean; var aResult : ICefBrowserViewDelegate) of object;
  TOnPopupBrowserViewCreatedEvent          = procedure(const Sender: TObject; const browser_view, popup_browser_view: ICefBrowserView; is_devtools: boolean; var aResult : boolean) of object;
  TOnGetChromeToolbarTypeEvent             = procedure(const Sender: TObject; const browser_view: ICefBrowserView; var aChromeToolbarType: TCefChromeToolbarType) of object;
  TOnUseFramelessWindowForPictureInPicture = procedure(const Sender: TObject; const browser_view: ICefBrowserView; var aResult : boolean) of object;
  TOnGestureCommandEvent                   = procedure(const Sender: TObject; const browser_view: ICefBrowserView; gesture_command: TCefGestureCommand; var aResult : boolean) of object;
  TOnGetBrowserRuntimeStyleEvent           = procedure(const Sender: TObject; var aResult : TCefRuntimeStyle) of object;

  // ICefButtonDelegate
  TOnButtonPressedEvent      = procedure(const Sender: TObject; const button: ICefButton) of object;
  TOnButtonStateChangedEvent = procedure(const Sender: TObject; const button: ICefButton) of object;

  // ICefMenuButtonDelegate
  TOnMenuButtonPressedEvent = procedure(const Sender: TObject; const menu_button: ICefMenuButton; const screen_point: TCefPoint; const button_pressed_lock: ICefMenuButtonPressedLock) of object;

  // ICefWindowDelegate
  TOnWindowCreatedEvent              = procedure(const Sender: TObject; const window_: ICefWindow) of object;
  TOnWindowClosingEvent              = procedure(const Sender: TObject; const window_: ICefWindow) of object;
  TOnWindowDestroyedEvent            = procedure(const Sender: TObject; const window_: ICefWindow) of object;
  TOnWindowActivationChangedEvent    = procedure(const Sender: TObject; const window_: ICefWindow; active: boolean) of object;
  TOnWindowBoundsChangedEvent        = procedure(const Sender: TObject; const window_: ICefWindow; const new_bounds: TCefRect) of object;
  TOnWindowFullscreenTransitionEvent = procedure(const Sender: TObject; const window_: ICefWindow; is_completed: boolean) of object;
  TOnGetParentWindowEvent            = procedure(const Sender: TObject; const window_: ICefWindow; var is_menu, can_activate_menu: boolean; var aResult : ICefWindow) of object;
  TOnIsWindowModalDialogEvent        = procedure(const Sender: TObject; const window_: ICefWindow; var aResult : boolean) of object;
  TOnGetInitialBoundsEvent           = procedure(const Sender: TObject; const window_: ICefWindow; var aResult : TCefRect) of object;
  TOnGetInitialShowStateEvent        = procedure(const Sender: TObject; const window_: ICefWindow; var aResult : TCefShowState) of object;
  TOnIsFramelessEvent                = procedure(const Sender: TObject; const window_: ICefWindow; var aResult : boolean) of object;
  TOnWithStandardWindowButtonsEvent  = procedure(const Sender: TObject; const window_: ICefWindow; var aResult : boolean) of object;
  TOnGetTitlebarHeightEvent          = procedure(const Sender: TObject; const window_: ICefWindow; var titlebar_height: Single; var aResult : boolean) of object;
  TOnAcceptsFirstMouseEvent          = procedure(const Sender: TObject; const window_: ICefWindow; var aResult : TCefState) of object;
  TOnCanResizeEvent                  = procedure(const Sender: TObject; const window_: ICefWindow; var aResult : boolean) of object;
  TOnCanMaximizeEvent                = procedure(const Sender: TObject; const window_: ICefWindow; var aResult : boolean) of object;
  TOnCanMinimizeEvent                = procedure(const Sender: TObject; const window_: ICefWindow; var aResult : boolean) of object;
  TOnCanCloseEvent                   = procedure(const Sender: TObject; const window_: ICefWindow; var aResult : boolean) of object;
  TOnAcceleratorEvent                = procedure(const Sender: TObject; const window_: ICefWindow; command_id: Integer; var aResult : boolean) of object;
  TOnWindowKeyEventEvent             = procedure(const Sender: TObject; const window_: ICefWindow; const event: TCefKeyEvent; var aResult : boolean) of object;
  TOnThemeColorsChangedEvent         = procedure(const Sender: TObject; const window_: ICefWindow; chrome_theme: Integer) of object;
  TOnGetWindowRuntimeStyleEvent      = procedure(const Sender: TObject; var aResult : TCefRuntimeStyle) of object;
  TOnGetLinuxWindowPropertiesEvent   = procedure(const Sender: TObject; const window_: ICefWindow; var properties: TLinuxWindowProperties; var aResult: boolean) of object;

implementation

end.
