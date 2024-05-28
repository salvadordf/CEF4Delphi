unit uCEFWindow;

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
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes, uCEFPanel;

type
  /// <summary>
  /// A Window is a top-level Window/widget in the Views hierarchy. By default it
  /// will have a non-client area with title bar, icon and buttons that supports
  /// moving and resizing. All size and position values are in density independent
  /// pixels (DIP) unless otherwise indicated. Methods must be called on the
  /// browser process UI thread unless otherwise indicated.
  /// </summary>
  TCefWindowRef = class(TCefPanelRef, ICefWindow)
    protected
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
      /// Returns true (1) if the Window has been closed.
      /// </summary>
      function  IsClosed : boolean;

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
      /// Returns whether the Window is the currently active Window.
      /// </summary>
      function  IsActive : boolean;

      /// <summary>
      /// Bring this Window to the top of other Windows in the Windowing system.
      /// </summary>
      procedure BringToTop;

      /// <summary>
      /// Set the Window to be on top of other Windows in the Windowing system.
      /// </summary>
      procedure SetAlwaysOnTop(on_top: boolean);

      /// <summary>
      /// Returns whether the Window has been set to be on top of other Windows in
      /// the Windowing system.
      /// </summary>
      function  IsAlwaysOnTop : boolean;

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
      /// Set fullscreen Window state. The
      /// ICefWindowDelegate.OnWindowFullscreenTransition function will be
      /// called during the fullscreen transition for notification purposes.
      /// </summary>
      procedure SetFullscreen(fullscreen: boolean);

      /// <summary>
      /// Returns true (1) if the Window is maximized.
      /// </summary>
      function  IsMaximized : boolean;

      /// <summary>
      /// Returns true (1) if the Window is minimized.
      /// </summary>
      function  IsMinimized : boolean;

      /// <summary>
      /// Returns true (1) if the Window is fullscreen.
      /// </summary>
      function  IsFullscreen : boolean;

      /// <summary>
      /// Set the Window title.
      /// </summary>
      procedure SetTitle(const title_: ustring);

      /// <summary>
      /// Get the Window title.
      /// </summary>
      function  GetTitle : ustring;

      /// <summary>
      /// Set the Window icon. This should be a 16x16 icon suitable for use in the
      /// Windows's title bar.
      /// </summary>
      procedure SetWindowIcon(const image: ICefImage);

      /// <summary>
      /// Get the Window icon.
      /// </summary>
      function  GetWindowIcon : ICefImage;

      /// <summary>
      /// Set the Window App icon. This should be a larger icon for use in the host
      /// environment app switching UI. On Windows, this is the ICON_BIG used in
      /// Alt-Tab list and Windows taskbar. The Window icon will be used by default
      /// if no Window App icon is specified.
      /// </summary>
      procedure SetWindowAppIcon(const image: ICefImage);

      /// <summary>
      /// Get the Window App icon.
      /// </summary>
      function  GetWindowAppIcon : ICefImage;

      /// <summary>
      /// <para>Add a View that will be overlayed on the Window contents with absolute
      /// positioning and high z-order. Positioning is controlled by |docking_mode|
      /// as described below. Setting |can_activate| to true (1) will allow the
      /// overlay view to receive input focus. The returned cef_overlay_controller_t
      /// object is used to control the overlay. Overlays are hidden by default.</para>
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
      ///      change in Window bounds.
      /// </code>
      /// <para>With other docking modes:</para>
      /// <code>
      ///   1. The overlay is initially hidden, sized to |view|'s preferred size,
      ///      and positioned based on |docking_mode|.
      ///   2. Call CefOverlayController::SetVisible(true) to show the overlay.
      ///   3. The overlay will be automatically re-sized if |view|'s layout changes
      ///      and re-positioned as appropriate when the Window resizes.
      /// </code>
      /// <para>Overlays created by this function will receive a higher z-order then any
      /// child Views added previously. It is therefore recommended to call this
      /// function last after all other child Views have been added so that the
      /// overlay displays as the top-most child of the Window.</para>
      /// </summary>
      function  AddOverlayView(const view: ICefView; docking_mode: TCefDockingMode; can_activate: boolean): ICefOverlayController;

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
      /// Returns the Display that most closely intersects the bounds of this
      /// Window. May return NULL if this Window is not currently displayed.
      /// </summary>
      function  GetDisplay : ICefDisplay;

      /// <summary>
      /// Returns the bounds (size and position) of this Window's client area.
      /// Position is in screen coordinates.
      /// </summary>
      function  GetClientAreaBoundsInScreen : TCefRect;

      /// <summary>
      /// Set the regions where mouse events will be intercepted by this Window to
      /// support drag operations. Call this function with an NULL vector to clear
      /// the draggable regions. The draggable region bounds should be in window
      /// coordinates.
      /// </summary>
      procedure SetDraggableRegions(regionsCount: NativeUInt; const regions: PCefDraggableRegionArray);

      /// <summary>
      /// Retrieve the platform window handle for this Window.
      /// </summary>
      function  GetWindowHandle : TCefWindowHandle;

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
      /// <para>Set the keyboard accelerator for the specified |command_id|. |key_code|
      /// can be any virtual key or character value. Required modifier keys are
      /// specified by |shift_pressed|, |ctrl_pressed| and/or |alt_pressed|.
      /// ICefWindowDelegate.OnAccelerator will be called if the keyboard
      /// combination is triggered while this window has focus.</para>
      /// <para>The |high_priority| value will be considered if a child ICefBrowserView
      /// has focus when the keyboard combination is triggered. If |high_priority|
      /// is true (1) then the key event will not be forwarded to the web content
      /// (`keydown` event handler) or ICefKeyboardHandler first. If
      /// |high_priority| is false (0) then the behavior will depend on the
      /// ICefBrowserView.SetPreferAccelerators configuration.</para>
      /// </summary>
      procedure SetAccelerator(command_id, key_code : Integer; shift_pressed, ctrl_pressed, alt_pressed, high_priority: boolean);

      /// <summary>
      /// Remove the keyboard accelerator for the specified |command_id|.
      /// </summary>
      procedure RemoveAccelerator(command_id: Integer);

      /// <summary>
      /// Remove all keyboard accelerators.
      /// </summary>
      procedure RemoveAllAccelerators;

      /// <summary>
      /// <para>Override a standard theme color or add a custom color associated with
      /// |color_id|. See cef_color_ids.h for standard ID values. Recommended usage
      /// is as follows:</para>
      /// <code>
      /// 1. Customize the default native/OS theme by calling SetThemeColor before
      ///    showing the first Window. When done setting colors call
      ///    ICefWindow.ThemeChanged to trigger ICefViewDelegate.OnThemeChanged
      ///    notifications.
      /// 2. Customize the current native/OS or Chrome theme after it changes by
      ///    calling SetThemeColor from the ICefWindowDelegate.OnThemeColorsChanged
      ///    callback. ICefViewDelegate.OnThemeChanged notifications will then be
      ///    triggered automatically.
      /// </code>
      /// <para>The configured color will be available immediately via
      /// ICefView.GetThemeColor and will be applied to each View in this
      /// Window's component hierarchy when ICefViewDelegate.OnThemeChanged is
      /// called. See OnThemeColorsChanged documentation for additional details.</para>
      /// <para>Clients wishing to add custom colors should use |color_id| values >=
      /// CEF_ChromeColorsEnd.</para>
      /// </summary>
      procedure SetThemeColor(color_id: integer; color: TCefColor);

      /// <summary>
      /// <para>Trigger ICefViewDelegate.OnThemeChanged callbacks for each View in
      /// this Window's component hierarchy. Unlike a native/OS or Chrome theme
      /// change this function does not reset theme colors to standard values and
      /// does not result in a call to ICefWindowDelegate.OnThemeColorsChanged.</para>
      /// <para>Do not call this function from ICefViewDelegate.OnThemeColorsChanged
      /// or ICefViewDelegate.OnThemeChanged.</para>
      /// </summary>
      procedure ThemeChanged;

      /// <summary>
      /// Returns the runtime style for this Window (ALLOY or CHROME). See
      /// TCefRuntimeStyle documentation for details.
      /// </summary>
      function  GetRuntimeStyle: TCefRuntimeStyle;

    public
      /// <summary>
      /// Returns a ICefWindow instance using a PCefWindow data pointer.
      /// </summary>
      class function UnWrap(data: Pointer): ICefWindow;
      /// <summary>
      /// Create a new Window.
      /// </summary>
      class function CreateTopLevel(const delegate: ICefWindowDelegate): ICefWindow;
  end;

implementation

uses
  uCEFLibFunctions, uCEFMiscFunctions, uCEFImage, uCEFDisplay, uCEFOverlayController;

procedure TCefWindowRef.Show;
begin
  PCefWindow(FData)^.show(PCefWindow(FData));
end;

procedure TCefWindowRef.ShowAsBrowserModalDialog(const browser_view: ICefBrowserView);
begin
  PCefWindow(FData)^.show_as_browser_modal_dialog(PCefWindow(FData), CefGetData(browser_view));
end;

procedure TCefWindowRef.Hide;
begin
  PCefWindow(FData)^.hide(PCefWindow(FData));
end;

procedure TCefWindowRef.CenterWindow(const size_: TCefSize);
begin
  PCefWindow(FData)^.center_window(PCefWindow(FData), @size_);
end;

procedure TCefWindowRef.Close;
begin
  PCefWindow(FData)^.close(PCefWindow(FData));
end;

function TCefWindowRef.IsClosed : boolean;
begin
  Result := (PCefWindow(FData)^.is_closed(PCefWindow(FData)) <> 0);
end;

procedure TCefWindowRef.Activate;
begin
  PCefWindow(FData)^.activate(PCefWindow(FData));
end;

procedure TCefWindowRef.Deactivate;
begin
  PCefWindow(FData)^.deactivate(PCefWindow(FData));
end;

function TCefWindowRef.IsActive : boolean;
begin
  Result := (PCefWindow(FData)^.is_active(PCefWindow(FData)) <> 0);
end;

procedure TCefWindowRef.BringToTop;
begin
  PCefWindow(FData)^.bring_to_top(PCefWindow(FData));
end;

procedure TCefWindowRef.SetAlwaysOnTop(on_top: boolean);
begin
  PCefWindow(FData)^.set_always_on_top(PCefWindow(FData), ord(on_top));
end;

function TCefWindowRef.IsAlwaysOnTop : boolean;
begin
  Result := (PCefWindow(FData)^.is_always_on_top(PCefWindow(FData)) <> 0);
end;

procedure TCefWindowRef.Maximize;
begin
  PCefWindow(FData)^.maximize(PCefWindow(FData));
end;

procedure TCefWindowRef.Minimize;
begin
  PCefWindow(FData)^.minimize(PCefWindow(FData));
end;

procedure TCefWindowRef.Restore;
begin
  PCefWindow(FData)^.restore(PCefWindow(FData));
end;

procedure TCefWindowRef.SetFullscreen(fullscreen: boolean);
begin
  PCefWindow(FData)^.set_fullscreen(PCefWindow(FData), ord(fullscreen));
end;

function TCefWindowRef.IsMaximized : boolean;
begin
  Result := (PCefWindow(FData)^.is_maximized(PCefWindow(FData)) <> 0);
end;

function TCefWindowRef.IsMinimized : boolean;
begin
  Result := (PCefWindow(FData)^.is_minimized(PCefWindow(FData)) <> 0);
end;

function TCefWindowRef.IsFullscreen : boolean;
begin
  Result := (PCefWindow(FData)^.is_fullscreen(PCefWindow(FData)) <> 0);
end;

procedure TCefWindowRef.SetTitle(const title_: ustring);
var
  TempTitle : TCefString;
begin
  TempTitle := CefString(title_);
  PCefWindow(FData)^.set_title(PCefWindow(FData), @TempTitle);
end;

function TCefWindowRef.GetTitle : ustring;
begin
  Result := CefStringFreeAndGet(PCefWindow(FData)^.get_title(PCefWindow(FData)));
end;

procedure TCefWindowRef.SetWindowIcon(const image: ICefImage);
begin
  PCefWindow(FData)^.set_window_icon(PCefWindow(FData), CefGetData(image));
end;

function TCefWindowRef.GetWindowIcon : ICefImage;
begin
  Result := TCefImageRef.UnWrap(PCefWindow(FData)^.get_window_icon(PCefWindow(FData)));
end;

procedure TCefWindowRef.SetWindowAppIcon(const image: ICefImage);
begin
  PCefWindow(FData)^.set_window_app_icon(PCefWindow(FData), CefGetData(image));
end;

function TCefWindowRef.GetWindowAppIcon : ICefImage;
begin
  Result := TCefImageRef.UnWrap(PCefWindow(FData)^.get_window_app_icon(PCefWindow(FData)));
end;

function TCefWindowRef.AddOverlayView(const view: ICefView; docking_mode: TCefDockingMode; can_activate: boolean): ICefOverlayController;
begin
  Result := TCefOverlayControllerRef.UnWrap(PCefWindow(FData)^.add_overlay_view(PCefWindow(FData),
                                                                                CefGetData(view),
                                                                                docking_mode,
                                                                                ord(can_activate)));
end;

procedure TCefWindowRef.ShowMenu(const menu_model      : ICefMenuModel;
                                 const screen_point    : TCefPoint;
                                       anchor_position : TCefMenuAnchorPosition);
begin
  PCefWindow(FData)^.show_menu(PCefWindow(FData),
                               CefGetData(menu_model),
                               @screen_point,
                               anchor_position);
end;

procedure TCefWindowRef.CancelMenu;
begin
  PCefWindow(FData)^.cancel_menu(PCefWindow(FData));
end;

function TCefWindowRef.GetDisplay : ICefDisplay;
begin
  Result := TCefDisplayRef.UnWrap(PCefWindow(FData)^.get_display(PCefWindow(FData)));
end;

function TCefWindowRef.GetClientAreaBoundsInScreen : TCefRect;
begin
  Result := PCefWindow(FData)^.get_client_area_bounds_in_screen(PCefWindow(FData));
end;

procedure TCefWindowRef.SetDraggableRegions(regionsCount: NativeUInt; const regions: PCefDraggableRegionArray);
begin
  PCefWindow(FData)^.set_draggable_regions(PCefWindow(FData), regionsCount, regions);
end;

function TCefWindowRef.GetWindowHandle : TCefWindowHandle;
begin
  Result := PCefWindow(FData)^.get_window_handle(PCefWindow(FData));
end;

procedure TCefWindowRef.SendKeyPress(key_code: Integer; event_flags: cardinal);
begin
  PCefWindow(FData)^.send_key_press(PCefWindow(FData), key_code, event_flags);
end;

procedure TCefWindowRef.SendMouseMove(screen_x, screen_y: Integer);
begin
  PCefWindow(FData)^.send_mouse_move(PCefWindow(FData), screen_x, screen_y);
end;

procedure TCefWindowRef.SendMouseEvents(button     : TCefMouseButtonType;
                                        mouse_down : boolean;
                                        mouse_up   : boolean);
begin
  PCefWindow(FData)^.send_mouse_events(PCefWindow(FData),
                                       button,
                                       ord(mouse_down),
                                       ord(mouse_up));
end;

procedure TCefWindowRef.SetAccelerator(command_id    : Integer;
                                       key_code      : Integer;
                                       shift_pressed : boolean;
                                       ctrl_pressed  : boolean;
                                       alt_pressed   : boolean;
                                       high_priority : boolean);
begin
  PCefWindow(FData)^.set_accelerator(PCefWindow(FData),
                                     command_id,
                                     key_code,
                                     ord(shift_pressed),
                                     ord(ctrl_pressed),
                                     ord(alt_pressed),
                                     ord(high_priority));
end;

procedure TCefWindowRef.RemoveAccelerator(command_id: Integer);
begin
  PCefWindow(FData)^.remove_accelerator(PCefWindow(FData), command_id);
end;

procedure TCefWindowRef.RemoveAllAccelerators;
begin
  PCefWindow(FData)^.remove_all_accelerators(PCefWindow(FData));
end;

procedure TCefWindowRef.SetThemeColor(color_id: integer; color: TCefColor);
begin
  PCefWindow(FData)^.set_theme_color(PCefWindow(FData), color_id, color);
end;

procedure TCefWindowRef.ThemeChanged;
begin
  PCefWindow(FData)^.theme_changed(PCefWindow(FData));
end;

function TCefWindowRef.GetRuntimeStyle: TCefRuntimeStyle;
begin
  Result := PCefWindow(FData)^.get_runtime_style(PCefWindow(FData));
end;

class function TCefWindowRef.UnWrap(data: Pointer): ICefWindow;
begin
  if (data <> nil) then
    Result := Create(data) as ICefWindow
   else
    Result := nil;
end;

class function TCefWindowRef.CreateTopLevel(const delegate: ICefWindowDelegate): ICefWindow;
var
  TempWindow : PCefWindow;
begin
  Result := nil;

  if (delegate <> nil) then
    begin
      TempWindow := cef_window_create_top_level(CefGetData(delegate));

      if (TempWindow <> nil) then
        Result := Create(TempWindow) as ICefWindow;
    end;
end;

end.

