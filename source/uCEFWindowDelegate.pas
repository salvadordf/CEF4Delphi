unit uCEFWindowDelegate;

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
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes, uCEFPanelDelegate;

type
  TCefWindowDelegateRef = class(TCefPanelDelegateRef, ICefWindowDelegate)
    protected
      procedure OnWindowCreated(const window_: ICefWindow);
      procedure OnWindowClosing(const window_: ICefWindow);
      procedure OnWindowDestroyed(const window_: ICefWindow);
      procedure OnWindowActivationChanged(const window_: ICefWindow; active: boolean);
      procedure OnWindowBoundsChanged(const window_: ICefWindow; const new_bounds: TCefRect);
      procedure OnWindowFullscreenTransition(const window_: ICefWindow; is_completed: boolean);
      procedure OnGetParentWindow(const window_: ICefWindow; var is_menu, can_activate_menu: boolean; var aResult : ICefWindow);
      procedure OnIsWindowModalDialog(const window_: ICefWindow; var aResult: boolean);
      procedure OnGetInitialBounds(const window_: ICefWindow; var aResult : TCefRect);
      procedure OnGetInitialShowState(const window_: ICefWindow; var aResult : TCefShowState);
      procedure OnIsFrameless(const window_: ICefWindow; var aResult : boolean);
      procedure OnWithStandardWindowButtons(const window_: ICefWindow; var aResult : boolean);
      procedure OnGetTitlebarHeight(const window_: ICefWindow; var titlebar_height: Single; var aResult : boolean);
      procedure OnAcceptsFirstMouse(const window_: ICefWindow; var aResult: TCefState);
      procedure OnCanResize(const window_: ICefWindow; var aResult : boolean);
      procedure OnCanMaximize(const window_: ICefWindow; var aResult : boolean);
      procedure OnCanMinimize(const window_: ICefWindow; var aResult : boolean);
      procedure OnCanClose(const window_: ICefWindow; var aResult : boolean);
      procedure OnAccelerator(const window_: ICefWindow; command_id: Integer; var aResult : boolean);
      procedure OnKeyEvent(const window_: ICefWindow; const event: TCefKeyEvent; var aResult : boolean);
      procedure OnThemeColorsChanged(const window_: ICefWindow; chrome_theme: Integer);
      procedure OnGetWindowRuntimeStyle(var aResult: TCefRuntimeStyle);
      procedure OnGetLinuxWindowProperties(const window_: ICefWindow; var properties: TLinuxWindowProperties; var aResult: boolean);

    public
      /// <summary>
      /// Returns a ICefWindowDelegate instance using a PCefWindowDelegate data pointer.
      /// </summary>
      class function UnWrap(data: Pointer): ICefWindowDelegate;
  end;

  /// <summary>
  /// Implement this interface to handle window events. The functions of this
  /// interface will be called on the browser process UI thread unless otherwise
  /// indicated.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_window_delegate_capi.h">CEF source file: /include/capi/views/cef_window_delegate_capi.h (cef_window_delegate_t)</see></para>
  /// </remarks>
  TCefWindowDelegateOwn = class(TCefPanelDelegateOwn, ICefWindowDelegate)
    protected
      /// <summary>
      /// Called when |window| is created.
      /// </summary>
      procedure OnWindowCreated(const window_: ICefWindow); virtual;
      /// <summary>
      /// Called when |window| is closing.
      /// </summary>
      procedure OnWindowClosing(const window_: ICefWindow); virtual;
      /// <summary>
      /// Called when |window| is destroyed. Release all references to |window| and
      /// do not attempt to execute any functions on |window| after this callback
      /// returns.
      /// </summary>
      procedure OnWindowDestroyed(const window_: ICefWindow); virtual;
      /// <summary>
      /// Called when |window| is activated or deactivated.
      /// </summary>
      procedure OnWindowActivationChanged(const window_: ICefWindow; active: boolean); virtual;
      /// <summary>
      /// Called when |window| bounds have changed. |new_bounds| will be in DIP
      /// screen coordinates.
      /// </summary>
      procedure OnWindowBoundsChanged(const window_: ICefWindow; const new_bounds: TCefRect); virtual;
      /// <summary>
      /// Called when |window| is transitioning to or from fullscreen mode. On MacOS
      /// the transition occurs asynchronously with |is_competed| set to false (0)
      /// when the transition starts and true (1) after the transition completes. On
      /// other platforms the transition occurs synchronously with |is_completed|
      /// set to true (1) after the transition completes. With Alloy style you must
      /// also implement ICefDisplayHandler.OnFullscreenModeChange to handle
      /// fullscreen transitions initiated by browser content.
      /// </summary>
      procedure OnWindowFullscreenTransition(const window_: ICefWindow; is_completed: boolean); virtual;
      /// <summary>
      /// Return the parent for |window| or NULL if the |window| does not have a
      /// parent. Windows with parents will not get a taskbar button. Set |is_menu|
      /// to true (1) if |window| will be displayed as a menu, in which case it will
      /// not be clipped to the parent window bounds. Set |can_activate_menu| to
      /// false (0) if |is_menu| is true (1) and |window| should not be activated
      /// (given keyboard focus) when displayed.
      /// </summary>
      procedure OnGetParentWindow(const window_: ICefWindow; var is_menu, can_activate_menu: boolean; var aResult : ICefWindow); virtual;
      /// <summary>
      /// Return true (1) if |window| should be created as a window modal dialog.
      /// Only called when a Window is returned via get_parent_window() with
      /// |is_menu| set to false (0). All controls in the parent Window will be
      /// disabled while |window| is visible. This functionality is not supported by
      /// all Linux window managers. Alternately, use
      /// ICefWindow.ShowAsBrowserModalDialog() for a browser modal dialog
      /// that works on all platforms.
      /// </summary>
      procedure OnIsWindowModalDialog(const window_: ICefWindow; var aResult: boolean); virtual;
      /// <summary>
      /// Return the initial bounds for |window| in density independent pixel (DIP)
      /// coordinates. If this function returns an NULL CefRect then
      /// GetPreferredSize() will be called to retrieve the size, and the window
      /// will be placed on the screen with origin (0,0). This function can be used
      /// in combination with ICefView.GetBoundsInScreen() to restore the
      /// previous window bounds.
      /// </summary>
      procedure OnGetInitialBounds(const window_: ICefWindow; var aResult : TCefRect); virtual;
      /// <summary>
      /// Return the initial show state for |window|.
      /// </summary>
      procedure OnGetInitialShowState(const window_: ICefWindow; var aResult : TCefShowState); virtual;
      /// <summary>
      /// Return true (1) if |window| should be created without a frame or title
      /// bar. The window will be resizable if can_resize() returns true (1). Use
      /// ICefWindow.SetDraggableRegions() to specify draggable regions.
      /// </summary>
      procedure OnIsFrameless(const window_: ICefWindow; var aResult : boolean); virtual;
      /// <summary>
      /// Return true (1) if |window| should be created with standard window buttons
      /// like close, minimize and zoom. This function is only supported on macOS.
      /// </summary>
      procedure OnWithStandardWindowButtons(const window_: ICefWindow; var aResult : boolean); virtual;
      /// <summary>
      /// Return whether the titlebar height should be overridden, and sets the
      /// height of the titlebar in |titlebar_height|. On macOS, it can also be used
      /// to adjust the vertical position of the traffic light buttons in frameless
      /// windows. The buttons will be positioned halfway down the titlebar at a
      /// height of |titlebar_height| / 2.
      /// </summary>
      procedure OnGetTitlebarHeight(const window_: ICefWindow; var titlebar_height: Single; var aResult : boolean); virtual;
      /// <summary>
      /// <para>Return whether the view should accept the initial mouse-down event,
      /// allowing it to respond to click-through behavior. If STATE_ENABLED is
      /// returned, the view will be sent a mouseDown: message for an initial mouse-
      /// down event, activating the view with one click, instead of clicking first
      /// to make the window active and then clicking the view.</para>
      /// <para>This function is only supported on macOS. For more details, refer to the
      /// documentation of acceptsFirstMouse.</para>
      /// </summary>
      procedure OnAcceptsFirstMouse(const window_: ICefWindow; var aResult: TCefState); virtual;
      /// <summary>
      /// Return true (1) if |window| can be resized.
      /// </summary>
      procedure OnCanResize(const window_: ICefWindow; var aResult : boolean); virtual;
      /// <summary>
      /// Return true (1) if |window| can be maximized.
      /// </summary>
      procedure OnCanMaximize(const window_: ICefWindow; var aResult : boolean); virtual;
      /// <summary>
      /// Return true (1) if |window| can be minimized.
      /// </summary>
      procedure OnCanMinimize(const window_: ICefWindow; var aResult : boolean); virtual;
      /// <summary>
      /// Return true (1) if |window| can be closed. This will be called for user-
      /// initiated window close actions and when ICefWindow.close() is called.
      /// </summary>
      procedure OnCanClose(const window_: ICefWindow; var aResult : boolean); virtual;
      /// <summary>
      /// Called when a keyboard accelerator registered with
      /// ICefWindow.SetAccelerator is triggered. Return true (1) if the
      /// accelerator was handled or false (0) otherwise.
      /// </summary>
      procedure OnAccelerator(const window_: ICefWindow; command_id: Integer; var aResult : boolean); virtual;
      /// <summary>
      /// Called after all other controls in the window have had a chance to handle
      /// the event. |event| contains information about the keyboard event. Return
      /// true (1) if the keyboard event was handled or false (0) otherwise.
      /// </summary>
      procedure OnKeyEvent(const window_: ICefWindow; const event: TCefKeyEvent; var aResult : boolean); virtual;
      /// <summary>
      /// <para>Called after the native/OS or Chrome theme for |window| has changed.
      /// |chrome_theme| will be true (1) if the notification is for a Chrome theme.</para>
      /// <para>Native/OS theme colors are configured globally and do not need to be
      /// customized for each Window individually. An example of a native/OS theme
      /// change that triggers this callback is when the user switches between dark
      /// and light mode during application lifespan. Native/OS theme changes can be
      /// disabled by passing the `--force-dark-mode` or `--force-light-mode`
      /// command-line flag.</para>
      /// <para>Chrome theme colors will be applied and this callback will be triggered
      /// if/when a BrowserView is added to the Window's component hierarchy. Chrome
      /// theme colors can be configured on a per-RequestContext basis using
      /// ICefRequestContext.SetChromeColorScheme or (Chrome style only) by
      /// visiting chrome://settings/manageProfile. Any theme changes using those
      /// mechanisms will also trigger this callback. Chrome theme colors will be
      /// persisted and restored from disk cache.</para>
      /// <para>This callback is not triggered on Window creation so clients that wish to
      /// customize the initial native/OS theme must call
      /// ICefWindow.SetThemeColor and ICefWindow.ThemeChanged before showing
      /// the first Window.</para>
      /// <para>Theme colors will be reset to standard values before this callback is
      /// called for the first affected Window. Call ICefWindow.SetThemeColor
      /// from inside this callback to override a standard color or add a custom
      /// color. ICefViewDelegate.OnThemeChanged will be called after this
      /// callback for the complete |window| component hierarchy.</para>
      /// </summary>
      procedure OnThemeColorsChanged(const window_: ICefWindow; chrome_theme: Integer); virtual;
      /// <summary>
      /// Optionally change the runtime style for this Window. See
      /// TCefRuntimeStyle documentation for details.
      /// </summary>
      procedure OnGetWindowRuntimeStyle(var aResult: TCefRuntimeStyle); virtual;
      /// <summary>
      /// Return Linux-specific window properties for correctly handling by window
      /// managers.
      /// </summary>
      procedure OnGetLinuxWindowProperties(const window_: ICefWindow; var properties: TLinuxWindowProperties; var aResult: boolean); virtual;
      /// <summary>
      /// Links the methods in the internal CEF record data pointer with the methods in this class.
      /// </summary>
      procedure InitializeCEFMethods; override;

    public
      constructor Create; override;
  end;

  /// <summary>
  /// This class handles all the TCustomWindowDelegate methods which call the ICefWindowDelegateEvents methods.
  /// ICefWindowDelegateEvents will be implemented by the control receiving the TCustomWindowDelegate events.
  /// </summary>
  TCustomWindowDelegate = class(TCefWindowDelegateOwn)
    protected
      FEvents : Pointer;

      // ICefViewDelegate
      procedure OnGetPreferredSize(const view: ICefView; var aResult : TCefSize); override;
      procedure OnGetMinimumSize(const view: ICefView; var aResult : TCefSize); override;
      procedure OnGetMaximumSize(const view: ICefView; var aResult : TCefSize); override;
      procedure OnGetHeightForWidth(const view: ICefView; width: Integer; var aResult: Integer); override;
      procedure OnParentViewChanged(const view: ICefView; added: boolean; const parent: ICefView); override;
      procedure OnChildViewChanged(const view: ICefView; added: boolean; const child: ICefView); override;
      procedure OnWindowChanged(const view: ICefView; added: boolean); override;
      procedure OnLayoutChanged(const view: ICefView; new_bounds: TCefRect); override;
      procedure OnFocus(const view: ICefView); override;
      procedure OnBlur(const view: ICefView); override;
      procedure OnThemeChanged(const view: ICefView); override;

      // ICefWindowDelegate
      procedure OnWindowCreated(const window_: ICefWindow); override;
      procedure OnWindowClosing(const window_: ICefWindow); override;
      procedure OnWindowDestroyed(const window_: ICefWindow); override;
      procedure OnWindowActivationChanged(const window_: ICefWindow; active: boolean); override;
      procedure OnWindowBoundsChanged(const window_: ICefWindow; const new_bounds: TCefRect); override;
      procedure OnWindowFullscreenTransition(const window_: ICefWindow; is_completed: boolean); override;
      procedure OnGetParentWindow(const window_: ICefWindow; var is_menu, can_activate_menu: boolean; var aResult : ICefWindow); override;
      procedure OnIsWindowModalDialog(const window_: ICefWindow; var aResult: boolean); override;
      procedure OnGetInitialBounds(const window_: ICefWindow; var aResult : TCefRect); override;
      procedure OnGetInitialShowState(const window_: ICefWindow; var aResult : TCefShowState); override;
      procedure OnIsFrameless(const window_: ICefWindow; var aResult : boolean); override;
      procedure OnWithStandardWindowButtons(const window_: ICefWindow; var aResult : boolean); override;
      procedure OnGetTitlebarHeight(const window_: ICefWindow; var titlebar_height: Single; var aResult : boolean); override;
      procedure OnAcceptsFirstMouse(const window_: ICefWindow; var aResult: TCefState); override;
      procedure OnCanResize(const window_: ICefWindow; var aResult : boolean); override;
      procedure OnCanMaximize(const window_: ICefWindow; var aResult : boolean); override;
      procedure OnCanMinimize(const window_: ICefWindow; var aResult : boolean); override;
      procedure OnCanClose(const window_: ICefWindow; var aResult : boolean); override;
      procedure OnAccelerator(const window_: ICefWindow; command_id: Integer; var aResult : boolean); override;
      procedure OnKeyEvent(const window_: ICefWindow; const event: TCefKeyEvent; var aResult : boolean); override;
      procedure OnThemeColorsChanged(const window_: ICefWindow; chrome_theme: Integer); override;
      procedure OnGetWindowRuntimeStyle(var aResult: TCefRuntimeStyle); override;
      procedure OnGetLinuxWindowProperties(const window_: ICefWindow; var properties: TLinuxWindowProperties; var aResult: boolean); override;

    public
      /// <summary>
      /// Creates an instance of this class liked to an interface that's implemented by a control receiving the events.
      /// </summary>
      constructor Create(const events: ICefWindowDelegateEvents); reintroduce;
  end;

implementation

uses
  uCEFLibFunctions, uCEFMiscFunctions, uCEFWindow, uCEFConstants;


// **************************************************************
// ******************* TCefWindowDelegateRef ********************
// **************************************************************

procedure TCefWindowDelegateRef.OnWindowCreated(const window_: ICefWindow);
begin
  PCefWindowDelegate(FData)^.on_window_created(PCefWindowDelegate(FData), CefGetData(window_));
end;

procedure TCefWindowDelegateRef.OnWindowClosing(const window_: ICefWindow);
begin
  PCefWindowDelegate(FData)^.on_window_closing(PCefWindowDelegate(FData), CefGetData(window_));
end;

procedure TCefWindowDelegateRef.OnWindowDestroyed(const window_: ICefWindow);
begin
  PCefWindowDelegate(FData)^.on_window_destroyed(PCefWindowDelegate(FData), CefGetData(window_));
end;

procedure TCefWindowDelegateRef.OnWindowActivationChanged(const window_: ICefWindow; active: boolean);
begin
  PCefWindowDelegate(FData)^.on_window_activation_changed(PCefWindowDelegate(FData), CefGetData(window_), ord(active));
end;

procedure TCefWindowDelegateRef.OnWindowBoundsChanged(const window_: ICefWindow; const new_bounds: TCefRect);
begin
  PCefWindowDelegate(FData)^.on_window_bounds_changed(PCefWindowDelegate(FData), CefGetData(window_), @new_bounds);
end;

procedure TCefWindowDelegateRef.OnWindowFullscreenTransition(const window_: ICefWindow; is_completed: boolean);
begin
  PCefWindowDelegate(FData)^.on_window_fullscreen_transition(PCefWindowDelegate(FData), CefGetData(window_), ord(is_completed));
end;

procedure TCefWindowDelegateRef.OnGetParentWindow(const window_            : ICefWindow;
                                                  var   is_menu           : boolean;
                                                  var   can_activate_menu : boolean;
                                                  var   aResult           : ICefWindow);
var
  TempIsMenu, TempCanActivateMenu : integer;
begin
  TempIsMenu          := ord(is_menu);
  TempCanActivateMenu := ord(can_activate_menu);
  aResult             := TCefWindowRef.UnWrap(PCefWindowDelegate(FData)^.get_parent_window(PCefWindowDelegate(FData),
                                                                                           CefGetData(window_),
                                                                                           @TempIsMenu,
                                                                                           @TempCanActivateMenu));
  is_menu           := TempIsMenu <> 0;
  can_activate_menu := TempCanActivateMenu <> 0;
end;

procedure TCefWindowDelegateRef.OnIsWindowModalDialog(const window_: ICefWindow; var aResult: boolean);
begin
  aResult := (PCefWindowDelegate(FData)^.is_window_modal_dialog(PCefWindowDelegate(FData), CefGetData(window_)) <> 0);
end;

procedure TCefWindowDelegateRef.OnGetInitialBounds(const window_: ICefWindow; var aResult : TCefRect);
begin
  aResult := PCefWindowDelegate(FData)^.get_initial_bounds(PCefWindowDelegate(FData), CefGetData(window_));
end;

procedure TCefWindowDelegateRef.OnGetInitialShowState(const window_: ICefWindow; var aResult : TCefShowState);
begin
  aResult := PCefWindowDelegate(FData)^.get_initial_show_state(PCefWindowDelegate(FData), CefGetData(window_));
end;

procedure TCefWindowDelegateRef.OnIsFrameless(const window_: ICefWindow; var aResult : boolean);
begin
  aResult := (PCefWindowDelegate(FData)^.is_frameless(PCefWindowDelegate(FData), CefGetData(window_)) <> 0);
end;

procedure TCefWindowDelegateRef.OnWithStandardWindowButtons(const window_: ICefWindow; var aResult : boolean);
begin
  aResult := (PCefWindowDelegate(FData)^.with_standard_window_buttons(PCefWindowDelegate(FData), CefGetData(window_)) <> 0);
end;

procedure TCefWindowDelegateRef.OnGetTitlebarHeight(const window_: ICefWindow; var titlebar_height: Single; var aResult : boolean);
begin
  aResult := (PCefWindowDelegate(FData)^.get_titlebar_height(PCefWindowDelegate(FData), CefGetData(window_), @titlebar_height) <> 0);
end;

procedure TCefWindowDelegateRef.OnAcceptsFirstMouse(const window_: ICefWindow; var aResult: TCefState);
begin
  aResult := PCefWindowDelegate(FData)^.accepts_first_mouse(PCefWindowDelegate(FData), CefGetData(window_));
end;

procedure TCefWindowDelegateRef.OnCanResize(const window_: ICefWindow; var aResult : boolean);
begin
  aResult := (PCefWindowDelegate(FData)^.can_resize(PCefWindowDelegate(FData), CefGetData(window_)) <> 0);
end;

procedure TCefWindowDelegateRef.OnCanMaximize(const window_: ICefWindow; var aResult : boolean);
begin
  aResult := (PCefWindowDelegate(FData)^.can_maximize(PCefWindowDelegate(FData), CefGetData(window_)) <> 0);
end;

procedure TCefWindowDelegateRef.OnCanMinimize(const window_: ICefWindow; var aResult : boolean);
begin
  aResult := (PCefWindowDelegate(FData)^.can_minimize(PCefWindowDelegate(FData), CefGetData(window_)) <> 0);
end;

procedure TCefWindowDelegateRef.OnCanClose(const window_: ICefWindow; var aResult : boolean);
begin
  aResult := (PCefWindowDelegate(FData)^.can_close(PCefWindowDelegate(FData), CefGetData(window_)) <> 0);
end;

procedure TCefWindowDelegateRef.OnAccelerator(const window_: ICefWindow; command_id: Integer; var aResult : boolean);
begin
  aResult := (PCefWindowDelegate(FData)^.on_accelerator(PCefWindowDelegate(FData), CefGetData(window_), command_id) <> 0);
end;

procedure TCefWindowDelegateRef.OnKeyEvent(const window_: ICefWindow; const event: TCefKeyEvent; var aResult : boolean);
begin
  aResult := (PCefWindowDelegate(FData)^.on_key_event(PCefWindowDelegate(FData), CefGetData(window_), @event) <> 0);
end;

procedure TCefWindowDelegateRef.OnThemeColorsChanged(const window_: ICefWindow; chrome_theme: Integer);
begin
  PCefWindowDelegate(FData)^.on_theme_colors_changed(PCefWindowDelegate(FData), CefGetData(window_), chrome_theme);
end;

procedure TCefWindowDelegateRef.OnGetWindowRuntimeStyle(var aResult: TCefRuntimeStyle);
begin
  aResult := PCefWindowDelegate(FData)^.get_window_runtime_style(PCefWindowDelegate(FData));
end;

procedure TCefWindowDelegateRef.OnGetLinuxWindowProperties(const window_: ICefWindow; var properties: TLinuxWindowProperties; var aResult: boolean);
var
  TempProperties : TCefLinuxWindowProperties;
begin
  aResult := False;

  CefStringInitialize(@TempProperties.wayland_app_id);
  CefStringInitialize(@TempProperties.wm_class_class);
  CefStringInitialize(@TempProperties.wm_class_name);
  CefStringInitialize(@TempProperties.wm_role_name);

  if (PCefWindowDelegate(FData)^.get_linux_window_properties(PCefWindowDelegate(FData), CefGetData(window_), @TempProperties) <> 0) then
    begin
      aResult := True;

      properties.wayland_app_id := CefString(@TempProperties.wayland_app_id);
      properties.wm_class_class := CefString(@TempProperties.wm_class_class);
      properties.wm_class_name  := CefString(@TempProperties.wm_class_name);
      properties.wm_role_name   := CefString(@TempProperties.wm_role_name);
    end;
end;

class function TCefWindowDelegateRef.UnWrap(data: Pointer): ICefWindowDelegate;
begin
  if (data <> nil) then
    Result := Create(data) as ICefWindowDelegate
   else
    Result := nil;
end;


// **************************************************************
// ******************* TCefWindowDelegateOwn ********************
// **************************************************************

procedure cef_window_delegate_on_window_created(self: PCefWindowDelegate; window_: PCefWindow); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefWindowDelegateOwn) then
    TCefWindowDelegateOwn(TempObject).OnWindowCreated(TCefWindowRef.UnWrap(window_));
end;

procedure cef_window_delegate_on_window_closing(self: PCefWindowDelegate; window_: PCefWindow); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefWindowDelegateOwn) then
    TCefWindowDelegateOwn(TempObject).OnWindowClosing(TCefWindowRef.UnWrap(window_));
end;

procedure cef_window_delegate_on_window_destroyed(self: PCefWindowDelegate; window_: PCefWindow); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefWindowDelegateOwn) then
    TCefWindowDelegateOwn(TempObject).OnWindowDestroyed(TCefWindowRef.UnWrap(window_));
end;

procedure cef_window_delegate_on_window_activation_changed(self: PCefWindowDelegate; window_: PCefWindow; active: integer); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefWindowDelegateOwn) then
    TCefWindowDelegateOwn(TempObject).OnWindowActivationChanged(TCefWindowRef.UnWrap(window_),
                                                                active <> 0);
end;

procedure cef_window_delegate_on_window_bounds_changed(self: PCefWindowDelegate; window_: PCefWindow; const new_bounds: PCefRect); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefWindowDelegateOwn) then
    TCefWindowDelegateOwn(TempObject).OnWindowBoundsChanged(TCefWindowRef.UnWrap(window_),
                                                            new_bounds^);
end;

procedure cef_window_delegate_on_window_fullscreen_transition(self         : PCefWindowDelegate;
                                                              window_      : PCefWindow;
                                                              is_completed : integer); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefWindowDelegateOwn) then
    TCefWindowDelegateOwn(TempObject).OnWindowFullscreenTransition(TCefWindowRef.UnWrap(window_),
                                                                   is_completed <> 0);
end;

function cef_window_delegate_get_parent_window(self              : PCefWindowDelegate;
                                               window_            : PCefWindow;
                                               is_menu           : PInteger;
                                               can_activate_menu : PInteger): PCefWindow; stdcall;
var
  TempObject : TObject;
  TempWindow : ICefWindow;
  TempIsMenu, TempCanActivateMenu : boolean;
begin
  TempObject := CefGetObject(self);
  TempWindow := nil;

  if (TempObject <> nil) and (TempObject is TCefWindowDelegateOwn) and (is_menu <> nil) and (can_activate_menu <> nil) then
    begin
      TempIsMenu          := (is_menu^           <> 0);
      TempCanActivateMenu := (can_activate_menu^ <> 0);

      TCefWindowDelegateOwn(TempObject).OnGetParentWindow(TCefWindowRef.UnWrap(window_),
                                                          TempIsMenu,
                                                          TempCanActivateMenu,
                                                          TempWindow);
      is_menu^           := ord(TempIsMenu);
      can_activate_menu^ := ord(TempCanActivateMenu);
    end;

  Result := CefGetData(TempWindow);
end;

function cef_window_delegate_is_window_modal_dialog(self: PCefWindowDelegate; window_: PCefWindow): Integer; stdcall;
var
  TempObject : TObject;
  TempResult : boolean;
begin
  TempObject := CefGetObject(self);
  TempResult := False;

  if (TempObject <> nil) and (TempObject is TCefWindowDelegateOwn) then
    TCefWindowDelegateOwn(TempObject).OnIsWindowModalDialog(TCefWindowRef.UnWrap(window_), TempResult);

  Result := ord(TempResult);
end;

function cef_window_delegate_get_initial_bounds(self: PCefWindowDelegate; window_: PCefWindow): TCefRect; stdcall;
var
  TempObject : TObject;
  TempRect   : TCefRect;
begin
  TempObject      := CefGetObject(self);
  TempRect.x      := 0;
  TempRect.y      := 0;
  TempRect.width  := 0;
  TempRect.height := 0;

  if (TempObject <> nil) and (TempObject is TCefWindowDelegateOwn) then
    TCefWindowDelegateOwn(TempObject).OnGetInitialBounds(TCefWindowRef.UnWrap(window_),
                                                         TempRect);

  Result.x      := TempRect.x;
  Result.y      := TempRect.y;
  Result.width  := TempRect.width;
  Result.height := TempRect.height;
end;

function cef_window_delegate_get_initial_show_state(self: PCefWindowDelegate; window_: PCefWindow): TCefShowState; stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);
  Result     := CEF_SHOW_STATE_NORMAL;

  if (TempObject <> nil) and (TempObject is TCefWindowDelegateOwn) then
    TCefWindowDelegateOwn(TempObject).OnGetInitialShowState(TCefWindowRef.UnWrap(window_),
                                                            Result);
end;

function cef_window_delegate_is_frameless(self: PCefWindowDelegate; window_: PCefWindow): Integer; stdcall;
var
  TempObject : TObject;
  TempResult : boolean;
begin
  TempObject      := CefGetObject(self);
  TempResult := False;

  if (TempObject <> nil) and (TempObject is TCefWindowDelegateOwn) then
    TCefWindowDelegateOwn(TempObject).OnIsFrameless(TCefWindowRef.UnWrap(window_),
                                                    TempResult);

  Result := ord(TempResult);
end;

function cef_window_delegate_with_standard_window_buttons(self: PCefWindowDelegate; window_: PCefWindow): Integer; stdcall;
var
  TempObject : TObject;
  TempResult : boolean;
begin
  TempObject := CefGetObject(self);
  TempResult := True;

  if (TempObject <> nil) and (TempObject is TCefWindowDelegateOwn) then
    TCefWindowDelegateOwn(TempObject).OnWithStandardWindowButtons(TCefWindowRef.UnWrap(window_), TempResult);

  Result := ord(TempResult);
end;

function cef_window_delegate_get_titlebar_height(self: PCefWindowDelegate; window_: PCefWindow; titlebar_height: PSingle): Integer; stdcall;
var
  TempObject : TObject;
  TempResult : boolean;
begin
  TempObject := CefGetObject(self);
  TempResult := False;

  if (TempObject <> nil) and (TempObject is TCefWindowDelegateOwn) then
    TCefWindowDelegateOwn(TempObject).OnGetTitlebarHeight(TCefWindowRef.UnWrap(window_),
                                                          titlebar_height^,
                                                          TempResult);

  Result := ord(TempResult);
end;

function cef_window_delegate_accepts_first_mouse(self: PCefWindowDelegate; window_: PCefWindow): TCefState; stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);
  Result     := STATE_DEFAULT;

  if (TempObject <> nil) and (TempObject is TCefWindowDelegateOwn) then
    TCefWindowDelegateOwn(TempObject).OnAcceptsFirstMouse(TCefWindowRef.UnWrap(window_),
                                                          Result);
end;

function cef_window_delegate_can_resize(self: PCefWindowDelegate; window_: PCefWindow): Integer; stdcall;
var
  TempObject    : TObject;
  TempCanResize : boolean;
begin
  TempObject    := CefGetObject(self);
  TempCanResize := True;

  if (TempObject <> nil) and (TempObject is TCefWindowDelegateOwn) then
    TCefWindowDelegateOwn(TempObject).OnCanResize(TCefWindowRef.UnWrap(window_), TempCanResize);

  Result := ord(TempCanResize);
end;

function cef_window_delegate_can_maximize(self: PCefWindowDelegate; window_: PCefWindow): Integer; stdcall;
var
  TempObject      : TObject;
  TempCanMaximize : boolean;
begin
  TempObject      := CefGetObject(self);
  TempCanMaximize := True;

  if (TempObject <> nil) and (TempObject is TCefWindowDelegateOwn) then
    TCefWindowDelegateOwn(TempObject).OnCanMaximize(TCefWindowRef.UnWrap(window_), TempCanMaximize);

  Result := ord(TempCanMaximize);
end;

function cef_window_delegate_can_minimize(self: PCefWindowDelegate; window_: PCefWindow): Integer; stdcall;
var
  TempObject      : TObject;
  TempCanMinimize : boolean;
begin
  TempObject      := CefGetObject(self);
  TempCanMinimize := True;

  if (TempObject <> nil) and (TempObject is TCefWindowDelegateOwn) then
    TCefWindowDelegateOwn(TempObject).OnCanMinimize(TCefWindowRef.UnWrap(window_), TempCanMinimize);

  Result := ord(TempCanMinimize);
end;

function cef_window_delegate_can_close(self: PCefWindowDelegate; window_: PCefWindow): Integer; stdcall;
var
  TempObject   : TObject;
  TempCanClose : boolean;
begin
  TempObject   := CefGetObject(self);
  TempCanClose := True;

  if (TempObject <> nil) and (TempObject is TCefWindowDelegateOwn) then
    TCefWindowDelegateOwn(TempObject).OnCanClose(TCefWindowRef.UnWrap(window_), TempCanClose);

  Result := ord(TempCanClose);
end;

function cef_window_delegate_on_accelerator(self       : PCefWindowDelegate;
                                            window_     : PCefWindow;
                                            command_id : Integer): Integer; stdcall;
var
  TempObject : TObject;
  TempResult : boolean;
begin
  TempObject := CefGetObject(self);
  TempResult := False;

  if (TempObject <> nil) and (TempObject is TCefWindowDelegateOwn) then
    TCefWindowDelegateOwn(TempObject).OnAccelerator(TCefWindowRef.UnWrap(window_), command_id, TempResult);

  Result := ord(TempResult);
end;

function cef_window_delegate_on_key_event(      self   : PCefWindowDelegate;
                                                window_ : PCefWindow;
                                          const event  : PCefKeyEvent): Integer; stdcall;
var
  TempObject : TObject;
  TempResult : boolean;
begin
  TempObject := CefGetObject(self);
  TempResult := False;

  if (TempObject <> nil) and (TempObject is TCefWindowDelegateOwn) then
    TCefWindowDelegateOwn(TempObject).OnKeyEvent(TCefWindowRef.UnWrap(window_), event^, TempResult);

  Result := ord(TempResult);
end;

procedure cef_window_delegate_on_theme_colors_changed(self: PCefWindowDelegate; window_: PCefWindow; chrome_theme: Integer); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefWindowDelegateOwn) then
    TCefWindowDelegateOwn(TempObject).OnThemeColorsChanged(TCefWindowRef.UnWrap(window_),
                                                           chrome_theme);
end;

function cef_window_delegate_get_window_runtime_style(self: PCefWindowDelegate): TCefRuntimeStyle; stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);
  Result     := CEF_RUNTIME_STYLE_DEFAULT;

  if (TempObject <> nil) and (TempObject is TCefWindowDelegateOwn) then
    TCefWindowDelegateOwn(TempObject).OnGetWindowRuntimeStyle(Result);
end;

function cef_window_delegate_get_linux_window_properties(self: PCefWindowDelegate; window_: PCefWindow; properties: PCefLinuxWindowProperties): Integer; stdcall;
var
  TempProperties : TLinuxWindowProperties;
  TempObject : TObject;
  TempResult : boolean;
begin
  TempObject := CefGetObject(self);
  TempResult := False;

  if (TempObject <> nil) and (TempObject is TCefWindowDelegateOwn) then
    begin
      TCefWindowDelegateOwn(TempObject).OnGetLinuxWindowProperties(TCefWindowRef.UnWrap(window_),
                                                                   TempProperties,
                                                                   TempResult);
      if TempResult then
        begin
          CefStringSet(@properties^.wayland_app_id, TempProperties.wayland_app_id);
          CefStringSet(@properties^.wm_class_class, TempProperties.wm_class_class);
          CefStringSet(@properties^.wm_class_name,  TempProperties.wm_class_name);
          CefStringSet(@properties^.wm_role_name,   TempProperties.wm_role_name);
        end;
    end;

  Result := ord(TempResult);
end;

constructor TCefWindowDelegateOwn.Create;
begin
  inherited CreateData(SizeOf(TCefWindowDelegate));

  InitializeCEFMethods;
end;

procedure TCefWindowDelegateOwn.InitializeCEFMethods;
begin
  inherited InitializeCEFMethods;

  with PCefWindowDelegate(FData)^ do
    begin
      on_window_created                := {$IFDEF FPC}@{$ENDIF}cef_window_delegate_on_window_created;
      on_window_closing                := {$IFDEF FPC}@{$ENDIF}cef_window_delegate_on_window_closing;
      on_window_destroyed              := {$IFDEF FPC}@{$ENDIF}cef_window_delegate_on_window_destroyed;
      on_window_activation_changed     := {$IFDEF FPC}@{$ENDIF}cef_window_delegate_on_window_activation_changed;
      on_window_bounds_changed         := {$IFDEF FPC}@{$ENDIF}cef_window_delegate_on_window_bounds_changed;
      on_window_fullscreen_transition  := {$IFDEF FPC}@{$ENDIF}cef_window_delegate_on_window_fullscreen_transition;
      get_parent_window                := {$IFDEF FPC}@{$ENDIF}cef_window_delegate_get_parent_window;
      is_window_modal_dialog           := {$IFDEF FPC}@{$ENDIF}cef_window_delegate_is_window_modal_dialog;
      get_initial_bounds               := {$IFDEF FPC}@{$ENDIF}cef_window_delegate_get_initial_bounds;
      get_initial_show_state           := {$IFDEF FPC}@{$ENDIF}cef_window_delegate_get_initial_show_state;
      is_frameless                     := {$IFDEF FPC}@{$ENDIF}cef_window_delegate_is_frameless;
      with_standard_window_buttons     := {$IFDEF FPC}@{$ENDIF}cef_window_delegate_with_standard_window_buttons;
      get_titlebar_height              := {$IFDEF FPC}@{$ENDIF}cef_window_delegate_get_titlebar_height;
      accepts_first_mouse              := {$IFDEF FPC}@{$ENDIF}cef_window_delegate_accepts_first_mouse;
      can_resize                       := {$IFDEF FPC}@{$ENDIF}cef_window_delegate_can_resize;
      can_maximize                     := {$IFDEF FPC}@{$ENDIF}cef_window_delegate_can_maximize;
      can_minimize                     := {$IFDEF FPC}@{$ENDIF}cef_window_delegate_can_minimize;
      can_close                        := {$IFDEF FPC}@{$ENDIF}cef_window_delegate_can_close;
      on_accelerator                   := {$IFDEF FPC}@{$ENDIF}cef_window_delegate_on_accelerator;
      on_key_event                     := {$IFDEF FPC}@{$ENDIF}cef_window_delegate_on_key_event;
      on_theme_colors_changed          := {$IFDEF FPC}@{$ENDIF}cef_window_delegate_on_theme_colors_changed;
      get_window_runtime_style         := {$IFDEF FPC}@{$ENDIF}cef_window_delegate_get_window_runtime_style;
      get_linux_window_properties      := {$IFDEF FPC}@{$ENDIF}cef_window_delegate_get_linux_window_properties;
    end;
end;

procedure TCefWindowDelegateOwn.OnWindowCreated(const window_: ICefWindow);
begin
  //
end;

procedure TCefWindowDelegateOwn.OnWindowClosing(const window_: ICefWindow);
begin
  //
end;

procedure TCefWindowDelegateOwn.OnWindowDestroyed(const window_: ICefWindow);
begin
  //
end;

procedure TCefWindowDelegateOwn.OnWindowActivationChanged(const window_: ICefWindow; active: boolean);
begin
  //
end;

procedure TCefWindowDelegateOwn.OnWindowBoundsChanged(const window_: ICefWindow; const new_bounds: TCefRect);
begin
  //
end;

procedure TCefWindowDelegateOwn.OnWindowFullscreenTransition(const window_: ICefWindow; is_completed: boolean);
begin
  //
end;

procedure TCefWindowDelegateOwn.OnGetParentWindow(const window_: ICefWindow; var is_menu, can_activate_menu: boolean; var aResult : ICefWindow);
begin
  //
end;

procedure TCefWindowDelegateOwn.OnIsWindowModalDialog(const window_: ICefWindow; var aResult: boolean);
begin
  //
end;

procedure TCefWindowDelegateOwn.OnGetInitialBounds(const window_: ICefWindow; var aResult : TCefRect);
begin
  //
end;

procedure TCefWindowDelegateOwn.OnGetInitialShowState(const window_: ICefWindow; var aResult : TCefShowState);
begin
  //
end;

procedure TCefWindowDelegateOwn.OnIsFrameless(const window_: ICefWindow; var aResult : boolean);
begin
  //
end;

procedure TCefWindowDelegateOwn.OnWithStandardWindowButtons(const window_: ICefWindow; var aResult : boolean);
begin
  //
end;

procedure TCefWindowDelegateOwn.OnGetTitlebarHeight(const window_: ICefWindow; var titlebar_height: Single; var aResult : boolean);
begin
  //
end;

procedure TCefWindowDelegateOwn.OnAcceptsFirstMouse(const window_: ICefWindow; var aResult: TCefState);
begin
  //
end;

procedure TCefWindowDelegateOwn.OnCanResize(const window_: ICefWindow; var aResult : boolean);
begin
  //
end;

procedure TCefWindowDelegateOwn.OnCanMaximize(const window_: ICefWindow; var aResult : boolean);
begin
  //
end;

procedure TCefWindowDelegateOwn.OnCanMinimize(const window_: ICefWindow; var aResult : boolean);
begin
  //
end;

procedure TCefWindowDelegateOwn.OnCanClose(const window_: ICefWindow; var aResult : boolean);
begin
  //
end;

procedure TCefWindowDelegateOwn.OnAccelerator(const window_: ICefWindow; command_id: Integer; var aResult : boolean);
begin
  //
end;

procedure TCefWindowDelegateOwn.OnKeyEvent(const window_: ICefWindow; const event: TCefKeyEvent; var aResult : boolean);
begin
  //
end;

procedure TCefWindowDelegateOwn.OnThemeColorsChanged(const window_: ICefWindow; chrome_theme: Integer);
begin
  //
end;

procedure TCefWindowDelegateOwn.OnGetWindowRuntimeStyle(var aResult: TCefRuntimeStyle);
begin
  aResult := CEF_RUNTIME_STYLE_DEFAULT;
end;

procedure TCefWindowDelegateOwn.OnGetLinuxWindowProperties(const window_: ICefWindow; var properties: TLinuxWindowProperties; var aResult: boolean);
begin
  aResult := False;
end;

// **************************************************************
// ******************* TCustomWindowDelegate ********************
// **************************************************************

constructor TCustomWindowDelegate.Create(const events: ICefWindowDelegateEvents);
begin
  inherited Create;

  FEvents := Pointer(events);
end;

procedure TCustomWindowDelegate.OnGetPreferredSize(const view: ICefView; var aResult : TCefSize);
begin
  try
    if (FEvents <> nil) then
      ICefWindowDelegateEvents(FEvents).doOnGetPreferredSize(view, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomWindowDelegate.OnGetPreferredSize', e) then raise;
  end;
end;

procedure TCustomWindowDelegate.OnGetMinimumSize(const view: ICefView; var aResult : TCefSize);
begin
  try
    if (FEvents <> nil) then
      ICefWindowDelegateEvents(FEvents).doOnGetMinimumSize(view, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomWindowDelegate.OnGetMinimumSize', e) then raise;
  end;
end;

procedure TCustomWindowDelegate.OnGetMaximumSize(const view: ICefView; var aResult : TCefSize);
begin
  try
    if (FEvents <> nil) then
      ICefWindowDelegateEvents(FEvents).doOnGetMaximumSize(view, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomWindowDelegate.OnGetMaximumSize', e) then raise;
  end;
end;

procedure TCustomWindowDelegate.OnGetHeightForWidth(const view: ICefView; width: Integer; var aResult: Integer);
begin
  try
    if (FEvents <> nil) then
      ICefWindowDelegateEvents(FEvents).doOnGetHeightForWidth(view, width, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomWindowDelegate.OnGetHeightForWidth', e) then raise;
  end;
end;

procedure TCustomWindowDelegate.OnParentViewChanged(const view: ICefView; added: boolean; const parent: ICefView);
begin
  try
    if (FEvents <> nil) then
      ICefWindowDelegateEvents(FEvents).doOnParentViewChanged(view, added, parent);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomWindowDelegate.OnParentViewChanged', e) then raise;
  end;
end;

procedure TCustomWindowDelegate.OnChildViewChanged(const view: ICefView; added: boolean; const child: ICefView);
begin
  try
    if (FEvents <> nil) then
      ICefWindowDelegateEvents(FEvents).doOnChildViewChanged(view, added, child);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomWindowDelegate.OnChildViewChanged', e) then raise;
  end;
end;

procedure TCustomWindowDelegate.OnWindowChanged(const view: ICefView; added: boolean);
begin
  try
    if (FEvents <> nil) then
      ICefWindowDelegateEvents(FEvents).doOnWindowChanged(view, added);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomWindowDelegate.OnWindowChanged', e) then raise;
  end;
end;

procedure TCustomWindowDelegate.OnLayoutChanged(const view: ICefView; new_bounds: TCefRect);
begin
  try
    if (FEvents <> nil) then
      ICefWindowDelegateEvents(FEvents).doOnLayoutChanged(view, new_bounds);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomWindowDelegate.OnLayoutChanged', e) then raise;
  end;
end;

procedure TCustomWindowDelegate.OnFocus(const view: ICefView);
begin
  try
    if (FEvents <> nil) then
      ICefWindowDelegateEvents(FEvents).doOnFocus(view);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomWindowDelegate.OnFocus', e) then raise;
  end;
end;

procedure TCustomWindowDelegate.OnBlur(const view: ICefView);
begin
  try
    if (FEvents <> nil) then
      ICefWindowDelegateEvents(FEvents).doOnBlur(view);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomWindowDelegate.OnBlur', e) then raise;
  end;
end;

procedure TCustomWindowDelegate.OnThemeChanged(const view: ICefView);
begin
  try
    if (FEvents <> nil) then
      ICefWindowDelegateEvents(FEvents).doOnThemeChanged(view);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomWindowDelegate.OnThemeChanged', e) then raise;
  end;
end;

procedure TCustomWindowDelegate.OnWindowCreated(const window_: ICefWindow);
begin
  try
    if (FEvents <> nil) then
      ICefWindowDelegateEvents(FEvents).doOnWindowCreated(window_);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomWindowDelegate.OnWindowCreated', e) then raise;
  end;
end;

procedure TCustomWindowDelegate.OnWindowClosing(const window_: ICefWindow);
begin
  try
    if (FEvents <> nil) then
      ICefWindowDelegateEvents(FEvents).doOnWindowClosing(window_);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomWindowDelegate.OnWindowClosing', e) then raise;
  end;
end;

procedure TCustomWindowDelegate.OnWindowDestroyed(const window_: ICefWindow);
begin
  try
    if (FEvents <> nil) then
      ICefWindowDelegateEvents(FEvents).doOnWindowDestroyed(window_);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomWindowDelegate.OnWindowDestroyed', e) then raise;
  end;
end;

procedure TCustomWindowDelegate.OnWindowActivationChanged(const window_: ICefWindow; active: boolean);
begin
  try
    if (FEvents <> nil) then
      ICefWindowDelegateEvents(FEvents).doOnWindowActivationChanged(window_, active);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomWindowDelegate.OnWindowActivationChanged', e) then raise;
  end;
end;

procedure TCustomWindowDelegate.OnWindowBoundsChanged(const window_: ICefWindow; const new_bounds: TCefRect);
begin
  try
    if (FEvents <> nil) then
      ICefWindowDelegateEvents(FEvents).doOnWindowBoundsChanged(window_, new_bounds);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomWindowDelegate.OnWindowBoundsChanged', e) then raise;
  end;
end;

procedure TCustomWindowDelegate.OnWindowFullscreenTransition(const window_: ICefWindow; is_completed: boolean);
begin
  try
    if (FEvents <> nil) then
      ICefWindowDelegateEvents(FEvents).doOnWindowFullscreenTransition(window_, is_completed);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomWindowDelegate.OnWindowFullscreenTransition', e) then raise;
  end;
end;

procedure TCustomWindowDelegate.OnGetParentWindow(const window_: ICefWindow; var is_menu, can_activate_menu: boolean; var aResult : ICefWindow);
begin
  try
    if (FEvents <> nil) then
      ICefWindowDelegateEvents(FEvents).doOnGetParentWindow(window_, is_menu, can_activate_menu, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomWindowDelegate.OnGetParentWindow', e) then raise;
  end;
end;

procedure TCustomWindowDelegate.OnIsWindowModalDialog(const window_: ICefWindow; var aResult: boolean);
begin
  try
    if (FEvents <> nil) then
      ICefWindowDelegateEvents(FEvents).doOnIsWindowModalDialog(window_, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomWindowDelegate.OnIsWindowModalDialog', e) then raise;
  end;
end;

procedure TCustomWindowDelegate.OnGetInitialBounds(const window_: ICefWindow; var aResult : TCefRect);
begin
  try
    if (FEvents <> nil) then
      ICefWindowDelegateEvents(FEvents).doOnGetInitialBounds(window_, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomWindowDelegate.OnGetInitialBounds', e) then raise;
  end;
end;

procedure TCustomWindowDelegate.OnGetInitialShowState(const window_: ICefWindow; var aResult : TCefShowState);
begin
  try
    if (FEvents <> nil) then
      ICefWindowDelegateEvents(FEvents).doOnGetInitialShowState(window_, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomWindowDelegate.OnGetInitialShowState', e) then raise;
  end;
end;

procedure TCustomWindowDelegate.OnIsFrameless(const window_: ICefWindow; var aResult : boolean);
begin
  try
    if (FEvents <> nil) then
      ICefWindowDelegateEvents(FEvents).doOnIsFrameless(window_, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomWindowDelegate.OnIsFrameless', e) then raise;
  end;
end;

procedure TCustomWindowDelegate.OnWithStandardWindowButtons(const window_: ICefWindow; var aResult : boolean);
begin
  try
    if (FEvents <> nil) then
      ICefWindowDelegateEvents(FEvents).doOnWithStandardWindowButtons(window_, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomWindowDelegate.OnWithStandardWindowButtons', e) then raise;
  end;
end;

procedure TCustomWindowDelegate.OnGetTitlebarHeight(const window_: ICefWindow; var titlebar_height: Single; var aResult : boolean);
begin
  try
    if (FEvents <> nil) then
      ICefWindowDelegateEvents(FEvents).doOnGetTitlebarHeight(window_, titlebar_height, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomWindowDelegate.OnGetTitlebarHeight', e) then raise;
  end;
end;

procedure TCustomWindowDelegate.OnAcceptsFirstMouse(const window_: ICefWindow; var aResult: TCefState);
begin
  try
    if (FEvents <> nil) then
      ICefWindowDelegateEvents(FEvents).doOnAcceptsFirstMouse(window_, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomWindowDelegate.OnAcceptsFirstMouse', e) then raise;
  end;
end;

procedure TCustomWindowDelegate.OnCanResize(const window_: ICefWindow; var aResult : boolean);
begin
  try
    if (FEvents <> nil) then
      ICefWindowDelegateEvents(FEvents).doOnCanResize(window_, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomWindowDelegate.OnCanResize', e) then raise;
  end;
end;

procedure TCustomWindowDelegate.OnCanMaximize(const window_: ICefWindow; var aResult : boolean);
begin
  try
    if (FEvents <> nil) then
      ICefWindowDelegateEvents(FEvents).doOnCanMaximize(window_, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomWindowDelegate.OnCanMaximize', e) then raise;
  end;
end;

procedure TCustomWindowDelegate.OnCanMinimize(const window_: ICefWindow; var aResult : boolean);
begin
  try
    if (FEvents <> nil) then
      ICefWindowDelegateEvents(FEvents).doOnCanMinimize(window_, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomWindowDelegate.OnCanMinimize', e) then raise;
  end;
end;

procedure TCustomWindowDelegate.OnCanClose(const window_: ICefWindow; var aResult : boolean);
begin
  try
    if (FEvents <> nil) then
      ICefWindowDelegateEvents(FEvents).doOnCanClose(window_, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomWindowDelegate.OnCanClose', e) then raise;
  end;
end;

procedure TCustomWindowDelegate.OnAccelerator(const window_: ICefWindow; command_id: Integer; var aResult : boolean);
begin
  try
    if (FEvents <> nil) then
      ICefWindowDelegateEvents(FEvents).doOnAccelerator(window_, command_id, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomWindowDelegate.OnAccelerator', e) then raise;
  end;
end;

procedure TCustomWindowDelegate.OnKeyEvent(const window_: ICefWindow; const event: TCefKeyEvent; var aResult : boolean);
begin
  try
    if (FEvents <> nil) then
      ICefWindowDelegateEvents(FEvents).doOnKeyEvent(window_, event, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomWindowDelegate.OnKeyEvent', e) then raise;
  end;
end;

procedure TCustomWindowDelegate.OnThemeColorsChanged(const window_: ICefWindow; chrome_theme: Integer);
begin
  try
    if (FEvents <> nil) then
      ICefWindowDelegateEvents(FEvents).doOnThemeColorsChanged(window_, chrome_theme);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomWindowDelegate.OnThemeColorsChanged', e) then raise;
  end;
end;

procedure TCustomWindowDelegate.OnGetWindowRuntimeStyle(var aResult: TCefRuntimeStyle);
begin
  aResult := CEF_RUNTIME_STYLE_DEFAULT;

  try
    if (FEvents <> nil) then
      ICefWindowDelegateEvents(FEvents).doOnGetWindowRuntimeStyle(aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomWindowDelegate.OnGetWindowRuntimeStyle', e) then raise;
  end;
end;

procedure TCustomWindowDelegate.OnGetLinuxWindowProperties(const window_: ICefWindow; var properties: TLinuxWindowProperties; var aResult: boolean);
begin
  aResult := False;

  try
    if (FEvents <> nil) then
      ICefWindowDelegateEvents(FEvents).doOnGetLinuxWindowProperties(window_, properties, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomWindowDelegate.OnGetLinuxWindowProperties', e) then raise;
  end;
end;

end.
