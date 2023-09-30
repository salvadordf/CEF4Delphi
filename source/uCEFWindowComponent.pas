unit uCEFWindowComponent;

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
  uCEFTypes, uCEFInterfaces, uCEFConstants, uCEFViewsFrameworkEvents, uCEFPanelComponent;

type
  {$IFNDEF FPC}{$IFDEF DELPHI16_UP}[ComponentPlatformsAttribute(pfidWindows or pfidOSX or pfidLinux)]{$ENDIF}{$ENDIF}
  TCEFWindowComponent = class(TCEFPanelComponent, ICefWindowDelegateEvents)
    protected
      FWindow                       : ICefWindow;
      FWindowDlg                    : ICefWindowDelegate;

      // ICefWindowDelegateEvents
      FOnWindowCreated              : TOnWindowCreatedEvent;
      FOnWindowClosing              : TOnWindowClosingEvent;
      FOnWindowDestroyed            : TOnWindowDestroyedEvent;
      FOnWindowActivationChanged    : TOnWindowActivationChangedEvent;
      FOnWindowBoundsChanged        : TOnWindowBoundsChangedEvent;
      FOnGetParentWindow            : TOnGetParentWindowEvent;
      FOnIsWindowModalDialog        : TOnIsWindowModalDialogEvent;
      FOnGetInitialBounds           : TOnGetInitialBoundsEvent;
      FOnGetInitialShowState        : TOnGetInitialShowStateEvent;
      FOnIsFrameless                : TOnIsFramelessEvent;
      FOnWithStandardWindowButtons  : TOnWithStandardWindowButtonsEvent;
      FOnGetTitlebarHeight          : TOnGetTitlebarHeightEvent;
      FOnCanResize                  : TOnCanResizeEvent;
      FOnCanMaximize                : TOnCanMaximizeEvent;
      FOnCanMinimize                : TOnCanMinimizeEvent;
      FOnCanClose                   : TOnCanCloseEvent;
      FOnAccelerator                : TOnAcceleratorEvent;
      FOnKeyEvent                   : TOnWindowKeyEventEvent;
      FOnWindowFullscreenTransition : TOnWindowFullscreenTransitionEvent;

      procedure DestroyView; override;
      procedure Initialize; override;

      function  GetInitialized : boolean; override;
      function  GetAsView : ICefView; override;
      function  GetAsPanel : ICefPanel; override;
      function  GetAsWindow : ICefWindow; override;
      function  GetIsClosed : boolean;
      function  GetIsActive : boolean;
      function  GetIsAlwaysOnTop : boolean;
      function  GetIsMaximized : boolean;
      function  GetIsMinimized : boolean;
      function  GetIsFullscreen : boolean;
      function  GetTitle : ustring;
      function  GetWindowIcon : ICefImage;
      function  GetWindowAppIcon : ICefImage;
      function  GetDisplay : ICefDisplay;
      function  GetClientAreaBoundsInScreen : TCefRect;
      function  GetWindowHandle : TCefWindowHandle;

      procedure SetAlwaysOnTop(on_top: boolean);
      procedure SetFullscreen(fullscreen: boolean);
      procedure SetTitle(const title_: ustring);
      procedure SetWindowIcon(const image: ICefImage);
      procedure SetWindowAppIcon(const image: ICefImage);

      // ICefWindowDelegateEvents
      procedure doOnWindowCreated(const window_: ICefWindow);
      procedure doOnWindowClosing(const window_: ICefWindow);
      procedure doOnWindowDestroyed(const window_: ICefWindow);
      procedure doOnWindowActivationChanged(const window_: ICefWindow; active: boolean);
      procedure doOnWindowBoundsChanged(const window_: ICefWindow; const new_bounds: TCefRect);
      procedure doOnGetParentWindow(const window_: ICefWindow; var is_menu, can_activate_menu: boolean; var aResult : ICefWindow);
      procedure doOnIsWindowModalDialog(const window_: ICefWindow; var aResult : boolean);
      procedure doOnGetInitialBounds(const window_: ICefWindow; var aResult : TCefRect);
      procedure doOnGetInitialShowState(const window_: ICefWindow; var aResult : TCefShowState);
      procedure doOnIsFrameless(const window_: ICefWindow; var aResult : boolean);
      procedure doOnWithStandardWindowButtons(const window_: ICefWindow; var aResult : boolean);
      procedure doOnGetTitlebarHeight(const window_: ICefWindow; var titlebar_height: Single; var aResult : boolean);
      procedure doOnCanResize(const window_: ICefWindow; var aResult : boolean);
      procedure doOnCanMaximize(const window_: ICefWindow; var aResult : boolean);
      procedure doOnCanMinimize(const window_: ICefWindow; var aResult : boolean);
      procedure doOnCanClose(const window_: ICefWindow; var aResult : boolean);
      procedure doOnAccelerator(const window_: ICefWindow; command_id: Integer; var aResult : boolean);
      procedure doOnKeyEvent(const window_: ICefWindow; const event: TCefKeyEvent; var aResult : boolean);
      procedure doOnWindowFullscreenTransition(const window_: ICefWindow; is_completed: boolean);

      // ICefViewDelegateEvents
      procedure doCreateCustomView; override;

    public
      /// <summary>
      /// Create a new Window.
      /// </summary>
      procedure CreateTopLevelWindow;
      /// <summary>
      /// Show the Window.
      /// </summary>
      procedure Show;
      /// <summary>
      /// Show the Window as a browser modal dialog relative to |browser_view|. A
      /// parent Window must be returned via
      /// cef_window_delegate_t::get_parent_window() and |browser_view| must belong
      /// to that parent Window. While this Window is visible, |browser_view| will
      /// be disabled while other controls in the parent Window remain enabled.
      /// Navigating or destroying the |browser_view| will close this Window
      /// automatically. Alternately, use show() and return true (1) from
      /// cef_window_delegate_t::is_window_modal_dialog() for a window modal dialog
      /// where all controls in the parent Window are disabled.
      /// </summary>
      procedure ShowAsBrowserModalDialog(const browser_view: ICefBrowserView);
      /// <summary>
      /// Hide the Window.
      /// </summary>
      procedure Hide;
      /// <summary>
      /// Sizes the Window to |size| and centers it in the current display.
      /// </summary>
      procedure CenterWindow(const size_: TCefSize);
      /// <summary>
      /// Close the Window.
      /// </summary>
      procedure Close;
      /// <summary>
      /// Activate the Window, assuming it already exists and is visible.
      /// </summary>
      procedure Activate;
      /// <summary>
      /// Deactivate the Window, making the next Window in the Z order the active
      /// Window.
      /// </summary>
      procedure Deactivate;
      /// <summary>
      /// Bring this Window to the top of other Windows in the Windowing system.
      /// </summary>
      procedure BringToTop;
      /// <summary>
      /// Maximize the Window.
      /// </summary>
      procedure Maximize;
      /// <summary>
      /// Minimize the Window.
      /// </summary>
      procedure Minimize;
      /// <summary>
      /// Restore the Window.
      /// </summary>
      procedure Restore;
      /// <summary>
      /// <para>Add a View that will be overlayed on the Window contents with absolute
      /// positioning and high z-order. Positioning is controlled by |docking_mode|
      /// as described below. The returned cef_overlay_controller_t object is used
      /// to control the overlay. Overlays are hidden by default.</para>
      /// <para>With CEF_DOCKING_MODE_CUSTOM:</para>
      /// <code>
      ///   1. The overlay is initially hidden, sized to |view|'s preferred size,
      ///      and positioned in the top-left corner.
      ///   2. Optionally change the overlay position and/or size by calling
      ///      CefOverlayController methods.
      ///   3. Call CefOverlayController::SetVisible(true) to show the overlay.
      ///   4. The overlay will be automatically re-sized if |view|'s layout
      ///      changes. Optionally change the overlay position and/or size when
      ///      OnLayoutChanged is called on the Window's delegate to indicate a
      ///      change in Window bounds.</code>
      /// <para>With other docking modes:</para>
      /// <code>
      ///   1. The overlay is initially hidden, sized to |view|'s preferred size,
      ///      and positioned based on |docking_mode|.
      ///   2. Call CefOverlayController::SetVisible(true) to show the overlay.
      ///   3. The overlay will be automatically re-sized if |view|'s layout changes
      ///      and re-positioned as appropriate when the Window resizes.</code>
      /// <para>Overlays created by this function will receive a higher z-order then any
      /// child Views added previously. It is therefore recommended to call this
      /// function last after all other child Views have been added so that the
      /// overlay displays as the top-most child of the Window.</para>
      /// </summary>
      function  AddOverlayView(const view: ICefView; docking_mode: TCefDockingMode): ICefOverlayController;
      /// <summary>
      /// Show a menu with contents |menu_model|. |screen_point| specifies the menu
      /// position in screen coordinates. |anchor_position| specifies how the menu
      /// will be anchored relative to |screen_point|.
      /// </summary>
      procedure ShowMenu(const menu_model: ICefMenuModel; const screen_point: TCefPoint; anchor_position : TCefMenuAnchorPosition);
      /// <summary>
      /// Cancel the menu that is currently showing, if any.
      /// </summary>
      procedure CancelMenu;
      /// <summary>
      /// Set the regions where mouse events will be intercepted by this Window to
      /// support drag operations. Call this function with an NULL vector to clear
      /// the draggable regions. The draggable region bounds should be in window
      /// coordinates.
      /// </summary>
      procedure SetDraggableRegions(regionsCount: NativeUInt; const regions: PCefDraggableRegionArray);
      /// <summary>
      /// Simulate a key press. |key_code| is the VKEY_* value from Chromium's
      /// ui/events/keycodes/keyboard_codes.h header (VK_* values on Windows).
      /// |event_flags| is some combination of EVENTFLAG_SHIFT_DOWN,
      /// EVENTFLAG_CONTROL_DOWN and/or EVENTFLAG_ALT_DOWN. This function is exposed
      /// primarily for testing purposes.
      /// </summary>
      procedure SendKeyPress(key_code: Integer; event_flags: cardinal);
      /// <summary>
      /// Simulate a mouse move. The mouse cursor will be moved to the specified
      /// (screen_x, screen_y) position. This function is exposed primarily for
      /// testing purposes.
      /// </summary>
      procedure SendMouseMove(screen_x, screen_y: Integer);
      /// <summary>
      /// Simulate mouse down and/or mouse up events. |button| is the mouse button
      /// type. If |mouse_down| is true (1) a mouse down event will be sent. If
      /// |mouse_up| is true (1) a mouse up event will be sent. If both are true (1)
      /// a mouse down event will be sent followed by a mouse up event (equivalent
      /// to clicking the mouse button). The events will be sent using the current
      /// cursor position so make sure to call send_mouse_move() first to position
      /// the mouse. This function is exposed primarily for testing purposes.
      /// </summary>
      procedure SendMouseEvents(button: TCefMouseButtonType; mouse_down, mouse_up: boolean);
      /// <summary>
      /// Set the keyboard accelerator for the specified |command_id|. |key_code|
      /// can be any virtual key or character value.
      /// cef_window_delegate_t::OnAccelerator will be called if the keyboard
      /// combination is triggered while this window has focus.
      /// </summary>
      procedure SetAccelerator(command_id, key_code : Integer; shift_pressed, ctrl_pressed, alt_pressed: boolean);
      /// <summary>
      /// Remove the keyboard accelerator for the specified |command_id|.
      /// </summary>
      procedure RemoveAccelerator(command_id: Integer);
      /// <summary>
      /// Remove all keyboard accelerators.
      /// </summary>
      procedure RemoveAllAccelerators;
      /// <summary>
      /// Get the Window title.
      /// </summary>
      property Title                    : ustring            read GetTitle                     write SetTitle;
      /// <summary>
      /// Get the Window icon.
      /// </summary>
      property WindowIcon               : ICefImage          read GetWindowIcon                write SetWindowIcon;
      /// <summary>
      /// Get or set the Window App icon. This should be a larger icon for use in the host
      /// environment app switching UI. On Windows, this is the ICON_BIG used in
      /// Alt-Tab list and Windows taskbar. The Window icon will be used by default
      /// if no Window App icon is specified.
      /// </summary>
      property WindowAppIcon            : ICefImage          read GetWindowAppIcon             write SetWindowAppIcon;
      /// <summary>
      /// Returns the Display that most closely intersects the bounds of this
      /// Window. May return NULL if this Window is not currently displayed.
      /// </summary>
      property Display                  : ICefDisplay        read GetDisplay;
      /// <summary>
      /// Returns the bounds (size and position) of this Window's client area.
      /// Position is in screen coordinates.
      /// </summary>
      property ClientAreaBoundsInScreen : TCefRect           read GetClientAreaBoundsInScreen;
      /// <summary>
      /// Retrieve the platform window handle for this Window.
      /// </summary>
      property WindowHandle             : TCefWindowHandle   read GetWindowHandle;
      /// <summary>
      /// Returns true (1) if the Window has been closed.
      /// </summary>
      property IsClosed                 : boolean            read GetIsClosed;
      /// <summary>
      /// Returns whether the Window is the currently active Window.
      /// </summary>
      property IsActive                 : boolean            read GetIsActive;
      /// <summary>
      /// Returns whether the Window has been set to be on top of other Windows in
      /// the Windowing system.
      /// </summary>
      property IsAlwaysOnTop            : boolean            read GetIsAlwaysOnTop             write SetAlwaysOnTop;
      /// <summary>
      /// Returns true (1) if the Window is fullscreen.
      /// </summary>
      property IsFullscreen             : boolean            read GetIsFullscreen              write SetFullscreen;
      /// <summary>
      /// Returns true (1) if the Window is maximized.
      /// </summary>
      property IsMaximized              : boolean            read GetIsMaximized;
      /// <summary>
      /// Returns true (1) if the Window is minimized.
      /// </summary>
      property IsMinimized              : boolean            read GetIsMinimized;

    published
      /// <summary>
      /// Called when |window| is created.
      /// </summary>
      property OnWindowCreated              : TOnWindowCreatedEvent              read FOnWindowCreated              write FOnWindowCreated;
      /// <summary>
      /// Called when |window| is closing.
      /// </summary>
      property OnWindowClosing              : TOnWindowClosingEvent              read FOnWindowClosing              write FOnWindowClosing;
      /// <summary>
      /// Called when |window| is destroyed. Release all references to |window| and
      /// do not attempt to execute any functions on |window| after this callback
      /// returns.
      /// </summary>
      property OnWindowDestroyed            : TOnWindowDestroyedEvent            read FOnWindowDestroyed            write FOnWindowDestroyed;
      /// <summary>
      /// Called when |window| is activated or deactivated.
      /// </summary>
      property OnWindowActivationChanged    : TOnWindowActivationChangedEvent    read FOnWindowActivationChanged    write FOnWindowActivationChanged;
      /// <summary>
      /// Called when |window| bounds have changed. |new_bounds| will be in DIP
      /// screen coordinates.
      /// </summary>
      property OnWindowBoundsChanged        : TOnWindowBoundsChangedEvent        read FOnWindowBoundsChanged        write FOnWindowBoundsChanged;
      /// <summary>
      /// Return the parent for |window| or NULL if the |window| does not have a
      /// parent. Windows with parents will not get a taskbar button. Set |is_menu|
      /// to true (1) if |window| will be displayed as a menu, in which case it will
      /// not be clipped to the parent window bounds. Set |can_activate_menu| to
      /// false (0) if |is_menu| is true (1) and |window| should not be activated
      /// (given keyboard focus) when displayed.
      /// </summary>
      property OnGetParentWindow            : TOnGetParentWindowEvent            read FOnGetParentWindow            write FOnGetParentWindow;
      /// <summary>
      /// Return true (1) if |window| should be created as a window modal dialog.
      /// Only called when a Window is returned via get_parent_window() with
      /// |is_menu| set to false (0). All controls in the parent Window will be
      /// disabled while |window| is visible. This functionality is not supported by
      /// all Linux window managers. Alternately, use
      /// ICefWindow.ShowAsBrowserModalDialog() for a browser modal dialog
      /// that works on all platforms.
      /// </summary>
      property OnIsWindowModalDialog        : TOnIsWindowModalDialogEvent        read FOnIsWindowModalDialog        write FOnIsWindowModalDialog;
      /// <summary>
      /// Return the initial bounds for |window| in density independent pixel (DIP)
      /// coordinates. If this function returns an NULL CefRect then
      /// GetPreferredSize() will be called to retrieve the size, and the window
      /// will be placed on the screen with origin (0,0). This function can be used
      /// in combination with ICefView.GetBoundsInScreen() to restore the
      /// previous window bounds.
      /// </summary>
      property OnGetInitialBounds           : TOnGetInitialBoundsEvent           read FOnGetInitialBounds           write FOnGetInitialBounds;
      /// <summary>
      /// Return the initial show state for |window|.
      /// </summary>
      property OnGetInitialShowState        : TOnGetInitialShowStateEvent        read FOnGetInitialShowState        write FOnGetInitialShowState;
      /// <summary>
      /// Return true (1) if |window| should be created without a frame or title
      /// bar. The window will be resizable if can_resize() returns true (1). Use
      /// ICefWindow.SetDraggableRegions() to specify draggable regions.
      /// </summary>
      property OnIsFrameless                : TOnIsFramelessEvent                read FOnIsFrameless                write FOnIsFrameless;
      /// <summary>
      /// Return true (1) if |window| should be created with standard window buttons
      /// like close, minimize and zoom. This function is only supported on macOS.
      /// </summary>
      property OnWithStandardWindowButtons  : TOnWithStandardWindowButtonsEvent  read FOnWithStandardWindowButtons  write FOnWithStandardWindowButtons;
      /// <summary>
      /// Return whether the titlebar height should be overridden, and sets the
      /// height of the titlebar in |titlebar_height|. On macOS, it can also be used
      /// to adjust the vertical position of the traffic light buttons in frameless
      /// windows. The buttons will be positioned halfway down the titlebar at a
      /// height of |titlebar_height| / 2.
      /// </summary>
      property OnGetTitlebarHeight          : TOnGetTitlebarHeightEvent          read FOnGetTitlebarHeight          write FOnGetTitlebarHeight;
      /// <summary>
      /// Return true (1) if |window| can be resized.
      /// </summary>
      property OnCanResize                  : TOnCanResizeEvent                  read FOnCanResize                  write FOnCanResize;
      /// <summary>
      /// Return true (1) if |window| can be maximized.
      /// </summary>
      property OnCanMaximize                : TOnCanMaximizeEvent                read FOnCanMaximize                write FOnCanMaximize;
      /// <summary>
      /// Return true (1) if |window| can be minimized.
      /// </summary>
      property OnCanMinimize                : TOnCanMinimizeEvent                read FOnCanMinimize                write FOnCanMinimize;
      /// <summary>
      /// Return true (1) if |window| can be closed. This will be called for user-
      /// initiated window close actions and when ICefWindow.close() is called.
      /// </summary>
      property OnCanClose                   : TOnCanCloseEvent                   read FOnCanClose                   write FOnCanClose;
      /// <summary>
      /// Called when a keyboard accelerator registered with
      /// ICefWindow.SetAccelerator is triggered. Return true (1) if the
      /// accelerator was handled or false (0) otherwise.
      /// </summary>
      property OnAccelerator                : TOnAcceleratorEvent                read FOnAccelerator                write FOnAccelerator;
      /// <summary>
      /// Called after all other controls in the window have had a chance to handle
      /// the event. |event| contains information about the keyboard event. Return
      /// true (1) if the keyboard event was handled or false (0) otherwise.
      /// </summary>
      property OnKeyEvent                   : TOnWindowKeyEventEvent             read FOnKeyEvent                   write FOnKeyEvent;
      /// <summary>
      /// Called when |window| is transitioning to or from fullscreen mode. On MacOS
      /// the transition occurs asynchronously with |is_competed| set to false (0)
      /// when the transition starts and true (1) after the transition completes. On
      /// other platforms the transition occurs synchronously with |is_completed|
      /// set to true (1) after the transition completes. With the Alloy runtime you
      /// must also implement ICefDisplayHandler.OnFullscreenModeChange to
      /// handle fullscreen transitions initiated by browser content.
      /// </summary>
      property OnWindowFullscreenTransition : TOnWindowFullscreenTransitionEvent read FOnWindowFullscreenTransition write FOnWindowFullscreenTransition;
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
  uCEFMiscFunctions, uCEFWindowDelegate, uCEFWindow, uCEFTask;

procedure TCEFWindowComponent.Initialize;
begin
  inherited Initialize;

  FWindow                       := nil;
  FWindowDlg                    := nil;
  FOnWindowCreated              := nil;
  FOnWindowClosing              := nil;
  FOnWindowDestroyed            := nil;
  FOnWindowActivationChanged    := nil;
  FOnWindowBoundsChanged        := nil;
  FOnGetParentWindow            := nil;
  FOnIsWindowModalDialog        := nil;
  FOnGetInitialBounds           := nil;
  FOnGetInitialShowState        := nil;
  FOnIsFrameless                := nil;
  FOnWithStandardWindowButtons  := nil;
  FOnGetTitlebarHeight          := nil;
  FOnCanResize                  := nil;
  FOnCanMaximize                := nil;
  FOnCanMinimize                := nil;
  FOnCanClose                   := nil;
  FOnAccelerator                := nil;
  FOnKeyEvent                   := nil;
  FOnWindowFullscreenTransition := nil;
end;

procedure TCEFWindowComponent.CreateTopLevelWindow;
begin
  CreateView;
end;

procedure TCEFWindowComponent.doCreateCustomView;
var
  TempWindow   : ICefWindow;
begin
  if (FWindow = nil) then
    begin
      if (FWindowDlg = nil) then
        FWindowDlg := TCustomWindowDelegate.Create(self);

      TempWindow := TCefWindowRef.CreateTopLevel(FWindowDlg);

      if (FWindow = nil) then FWindow := TempWindow;
    end;
end;

procedure TCEFWindowComponent.DestroyView;
begin
  if (FWindowDlg <> nil) then
    begin
      FWindowDlg.DestroyOtherRefs;
      FWindowDlg := nil;
    end;

  FWindow := nil;
end;

function TCEFWindowComponent.GetInitialized : boolean;
begin
  Result := (FWindow <> nil);
end;

function TCEFWindowComponent.GetAsView : ICefView;
begin
  Result := FWindow as ICefView;
end;

function TCEFWindowComponent.GetAsPanel : ICefPanel;
begin
  if Initialized then
    Result := FWindow as ICefPanel
   else
    Result := nil;
end;

function TCEFWindowComponent.GetAsWindow : ICefWindow;
begin
  Result := FWindow;
end;

procedure TCEFWindowComponent.doOnWindowCreated(const window_: ICefWindow);
begin
  if (FWindow = nil) then FWindow := window_;

  if assigned(FOnWindowCreated) then
    FOnWindowCreated(self, window_);
end;

procedure TCEFWindowComponent.doOnWindowClosing(const window_: ICefWindow);
begin
  if assigned(FOnWindowClosing) then
    FOnWindowClosing(self, window_);
end;

procedure TCEFWindowComponent.doOnWindowDestroyed(const window_: ICefWindow);
begin
  if assigned(FOnWindowDestroyed) then
    FOnWindowDestroyed(self, window_);

  FWindow := nil;
end;

procedure TCEFWindowComponent.doOnWindowActivationChanged(const window_: ICefWindow; active: boolean);
begin
  if assigned(FOnWindowActivationChanged) then
    FOnWindowActivationChanged(self, window_, active);
end;

procedure TCEFWindowComponent.doOnWindowBoundsChanged(const window_: ICefWindow; const new_bounds: TCefRect);
begin
  if assigned(FOnWindowBoundsChanged) then
    FOnWindowBoundsChanged(self, window_, new_bounds);
end;

procedure TCEFWindowComponent.doOnGetParentWindow(const window_: ICefWindow; var is_menu, can_activate_menu: boolean; var aResult : ICefWindow);
begin
  if assigned(FOnGetParentWindow) then
    FOnGetParentWindow(self, window_, is_menu, can_activate_menu, aResult);
end;

procedure TCEFWindowComponent.doOnIsWindowModalDialog(const window_: ICefWindow; var aResult : boolean);
begin
  if assigned(FOnIsWindowModalDialog) then
    FOnIsWindowModalDialog(self, window_, aResult);
end;

procedure TCEFWindowComponent.doOnGetInitialBounds(const window_: ICefWindow; var aResult : TCefRect);
begin
  if assigned(FOnGetInitialBounds) then
    FOnGetInitialBounds(self, window_, aResult);
end;

procedure TCEFWindowComponent.doOnGetInitialShowState(const window_: ICefWindow; var aResult : TCefShowState);
begin
  if assigned(FOnGetInitialShowState) then
    FOnGetInitialShowState(self, window_, aResult);
end;

procedure TCEFWindowComponent.doOnIsFrameless(const window_: ICefWindow; var aResult : boolean);
begin
  if assigned(FOnIsFrameless) then
    FOnIsFrameless(self, window_, aResult);
end;

procedure TCEFWindowComponent.doOnWithStandardWindowButtons(const window_: ICefWindow; var aResult : boolean);
begin
  if assigned(FOnWithStandardWindowButtons) then
    FOnWithStandardWindowButtons(self, window_, aResult);
end;

procedure TCEFWindowComponent.doOnGetTitlebarHeight(const window_: ICefWindow; var titlebar_height: Single; var aResult : boolean);
begin
  if assigned(FOnGetTitlebarHeight) then
    FOnGetTitlebarHeight(self, window_, titlebar_height, aResult);
end;

procedure TCEFWindowComponent.doOnCanResize(const window_: ICefWindow; var aResult : boolean);
begin
  if assigned(FOnCanResize) then
    FOnCanResize(self, window_, aResult);
end;

procedure TCEFWindowComponent.doOnCanMaximize(const window_: ICefWindow; var aResult : boolean);
begin
  if assigned(FOnCanMaximize) then
    FOnCanMaximize(self, window_, aResult);
end;

procedure TCEFWindowComponent.doOnCanMinimize(const window_: ICefWindow; var aResult : boolean);
begin
  if assigned(FOnCanMinimize) then
    FOnCanMinimize(self, window_, aResult);
end;

procedure TCEFWindowComponent.doOnCanClose(const window_: ICefWindow; var aResult : boolean);
begin
  if assigned(FOnCanClose) then
    FOnCanClose(self, window_, aResult);
end;

procedure TCEFWindowComponent.doOnAccelerator(const window_: ICefWindow; command_id: Integer; var aResult : boolean);
begin
  if assigned(FOnAccelerator) then
    FOnAccelerator(self, window_, command_id, aResult);
end;

procedure TCEFWindowComponent.doOnKeyEvent(const window_: ICefWindow; const event: TCefKeyEvent; var aResult : boolean);
begin
  if assigned(FOnKeyEvent) then
    FOnKeyEvent(self, window_, event, aResult);
end;

procedure TCEFWindowComponent.doOnWindowFullscreenTransition(const window_: ICefWindow; is_completed: boolean);
begin
  if assigned(FOnWindowFullscreenTransition) then
    FOnWindowFullscreenTransition(self, window_, is_completed);
end;

procedure TCEFWindowComponent.Show;
begin
  if Initialized then FWindow.Show;
end;

procedure TCEFWindowComponent.ShowAsBrowserModalDialog(const browser_view: ICefBrowserView);
begin
  if Initialized then FWindow.ShowAsBrowserModalDialog(browser_view);
end;

procedure TCEFWindowComponent.Hide;
begin
  if Initialized then FWindow.Hide;
end;

procedure TCEFWindowComponent.CenterWindow(const size_: TCefSize);
begin
  if Initialized then FWindow.CenterWindow(size_);
end;

procedure TCEFWindowComponent.Close;
begin
  if Initialized then FWindow.Close;
end;

function TCEFWindowComponent.GetIsClosed : boolean;
begin
  Result := Initialized and FWindow.IsClosed;
end;

procedure TCEFWindowComponent.Activate;
begin
  if Initialized then FWindow.Activate;
end;

procedure TCEFWindowComponent.Deactivate;
begin
  if Initialized then FWindow.Deactivate;
end;

function TCEFWindowComponent.GetIsActive : boolean;
begin
  Result := Initialized and FWindow.IsActive;
end;

procedure TCEFWindowComponent.BringToTop;
begin
  if Initialized then FWindow.BringToTop;
end;

procedure TCEFWindowComponent.SetAlwaysOnTop(on_top: boolean);
begin
  if Initialized then FWindow.SetAlwaysOnTop(on_top);
end;

function TCEFWindowComponent.GetIsAlwaysOnTop : boolean;
begin
  Result := Initialized and FWindow.IsAlwaysOnTop;
end;

procedure TCEFWindowComponent.Maximize;
begin
  if Initialized then FWindow.Maximize;
end;

procedure TCEFWindowComponent.Minimize;
begin
  if Initialized then FWindow.Minimize;
end;

procedure TCEFWindowComponent.Restore;
begin
  if Initialized then FWindow.Restore;
end;

procedure TCEFWindowComponent.SetFullscreen(fullscreen: boolean);
begin
  if Initialized then FWindow.SetFullscreen(fullscreen);
end;

function TCEFWindowComponent.GetIsMaximized : boolean;
begin
  Result := Initialized and FWindow.IsMaximized;
end;

function TCEFWindowComponent.GetIsMinimized : boolean;
begin
  Result := Initialized and FWindow.IsMinimized;
end;

function TCEFWindowComponent.GetIsFullscreen : boolean;
begin
  Result := Initialized and FWindow.IsFullscreen;
end;

procedure TCEFWindowComponent.SetTitle(const title_: ustring);
begin
  if Initialized then FWindow.SetTitle(title_);
end;

function TCEFWindowComponent.GetTitle : ustring;
begin
  if Initialized then
    Result := FWindow.GetTitle
   else
    Result := '';
end;

procedure TCEFWindowComponent.SetWindowIcon(const image: ICefImage);
begin
  if Initialized then FWindow.SetWindowIcon(image);
end;

function TCEFWindowComponent.GetWindowIcon : ICefImage;
begin
  if Initialized then
    Result := FWindow.GetWindowIcon
   else
    Result := nil;
end;

procedure TCEFWindowComponent.SetWindowAppIcon(const image: ICefImage);
begin
  if Initialized then FWindow.SetWindowAppIcon(image);
end;

function TCEFWindowComponent.GetWindowAppIcon : ICefImage;
begin
  if Initialized then
    Result := FWindow.GetWindowAppIcon
   else
    Result := nil;
end;

function TCEFWindowComponent.AddOverlayView(const view: ICefView; docking_mode: TCefDockingMode): ICefOverlayController;
begin
  if Initialized then
    Result := FWindow.AddOverlayView(view, docking_mode)
   else
    Result := nil;
end;

procedure TCEFWindowComponent.ShowMenu(const menu_model: ICefMenuModel; const screen_point: TCefPoint; anchor_position : TCefMenuAnchorPosition);
begin
  if Initialized then FWindow.ShowMenu(menu_model, screen_point, anchor_position);
end;

procedure TCEFWindowComponent.CancelMenu;
begin
  if Initialized then FWindow.CancelMenu;
end;

function TCEFWindowComponent.GetDisplay : ICefDisplay;
begin
  if Initialized then
    Result := FWindow.GetDisplay
   else
    Result := nil;
end;

function TCEFWindowComponent.GetClientAreaBoundsInScreen : TCefRect;
var
  TempRect : TCefRect;
begin
  if Initialized then
    TempRect := FWindow.GetClientAreaBoundsInScreen
   else
    begin
      TempRect.x      := 0;
      TempRect.y      := 0;
      TempRect.width  := 0;
      TempRect.height := 0;
    end;

  Result := TempRect;
end;

procedure TCEFWindowComponent.SetDraggableRegions(regionsCount: NativeUInt; const regions: PCefDraggableRegionArray);
begin
  if Initialized then FWindow.SetDraggableRegions(regionsCount, regions);
end;

function TCEFWindowComponent.GetWindowHandle : TCefWindowHandle;
var
  TempHandle : TCefWindowHandle;
begin
  InitializeWindowHandle(TempHandle);
  if Initialized then TempHandle := FWindow.GetWindowHandle;
  Result := TempHandle;
end;

procedure TCEFWindowComponent.SendKeyPress(key_code: Integer; event_flags: cardinal);
begin
  if Initialized then FWindow.SendKeyPress(key_code, event_flags);
end;

procedure TCEFWindowComponent.SendMouseMove(screen_x, screen_y: Integer);
begin
  if Initialized then FWindow.SendMouseMove(screen_x, screen_y);
end;

procedure TCEFWindowComponent.SendMouseEvents(button: TCefMouseButtonType; mouse_down, mouse_up: boolean);
begin
  if Initialized then FWindow.SendMouseEvents(button, mouse_down, mouse_up);
end;

procedure TCEFWindowComponent.SetAccelerator(command_id, key_code : Integer; shift_pressed, ctrl_pressed, alt_pressed: boolean);
begin
  if Initialized then FWindow.SetAccelerator(command_id, key_code, shift_pressed, ctrl_pressed, alt_pressed);
end;

procedure TCEFWindowComponent.RemoveAccelerator(command_id: Integer);
begin
  if Initialized then FWindow.RemoveAccelerator(command_id);
end;

procedure TCEFWindowComponent.RemoveAllAccelerators;
begin
  if Initialized then FWindow.RemoveAllAccelerators;
end;

{$IFDEF FPC}
procedure Register;
begin
  {$I res/tcefwindowcomponent.lrs}
  RegisterComponents('Chromium Views Framework', [TCEFWindowComponent]);
end;
{$ENDIF}

end.
