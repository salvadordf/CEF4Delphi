unit uCEFView;

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
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  /// <summary>
  /// A View is a rectangle within the views View hierarchy. It is the base
  /// interface for all Views. All size and position values are in density
  /// independent pixels (DIP) unless otherwise indicated. Methods must be called
  /// on the browser process UI thread unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_view_capi.h">CEF source file: /include/capi/views/cef_view_capi.h (cef_view_t)</see></para>
  /// </remarks>
  TCefViewRef = class(TCefBaseRefCountedRef, ICefView)
    protected
      /// <summary>
      /// Returns this View as a BrowserView or NULL if this is not a BrowserView.
      /// </summary>
      function  AsBrowserView : ICefBrowserView;
      /// <summary>
      /// Returns this View as a Button or NULL if this is not a Button.
      /// </summary>
      function  AsButton : ICefButton;
      /// <summary>
      /// Returns this View as a Panel or NULL if this is not a Panel.
      /// </summary>
      function  AsPanel : ICefPanel;
      /// <summary>
      /// Returns this View as a ScrollView or NULL if this is not a ScrollView.
      /// </summary>
      function  AsScrollView : ICefScrollView;
      /// <summary>
      /// Returns this View as a Textfield or NULL if this is not a Textfield.
      /// </summary>
      function  AsTextfield : ICefTextfield;
      /// <summary>
      /// Returns the type of this View as a string. Used primarily for testing
      /// purposes.
      /// </summary>
      function  GetTypeString : ustring;
      /// <summary>
      /// Returns a string representation of this View which includes the type and
      /// various type-specific identifying attributes. If |include_children| is
      /// true (1) any child Views will also be included. Used primarily for testing
      /// purposes.
      /// </summary>
      function  ToStringEx(include_children: boolean): ustring;
      /// <summary>
      /// Returns true (1) if this View is valid.
      /// </summary>
      function  IsValid : boolean;
      /// <summary>
      /// Returns true (1) if this View is currently attached to another View. A
      /// View can only be attached to one View at a time.
      /// </summary>
      function  IsAttached : boolean;
      /// <summary>
      /// Returns true (1) if this View is the same as |that| View.
      /// </summary>
      function  IsSame(const that: ICefView): boolean;
      /// <summary>
      /// Returns the delegate associated with this View, if any.
      /// </summary>
      function  GetDelegate : ICefViewDelegate;
      /// <summary>
      /// Returns the top-level Window hosting this View, if any.
      /// </summary>
      function  GetWindow : ICefWindow;
      /// <summary>
      /// Returns the ID for this View.
      /// </summary>
      function  GetID : Integer;
      /// <summary>
      /// Sets the ID for this View. ID should be unique within the subtree that you
      /// intend to search for it. 0 is the default ID for views.
      /// </summary>
      procedure SetID(id_: Integer);
      /// <summary>
      /// Returns the group id of this View, or -1 if not set.
      /// </summary>
      function  GetGroupID : Integer;
      /// <summary>
      /// A group id is used to tag Views which are part of the same logical group.
      /// Focus can be moved between views with the same group using the arrow keys.
      /// The group id is immutable once it's set.
      /// </summary>
      procedure SetGroupID(group_id: Integer);
      /// <summary>
      /// Returns the View that contains this View, if any.
      /// </summary>
      function  GetParentView : ICefView;
      /// <summary>
      /// Recursively descends the view tree starting at this View, and returns the
      /// first child that it encounters with the given ID. Returns NULL if no
      /// matching child view is found.
      /// </summary>
      function  GetViewForID(id_: Integer): ICefView;
      /// <summary>
      /// Sets the bounds (size and position) of this View. |bounds| is in parent
      /// coordinates, or DIP screen coordinates if there is no parent.
      /// </summary>
      procedure SetBounds(const bounds_: TCefRect);
      /// <summary>
      /// Returns the bounds (size and position) of this View in parent coordinates,
      /// or DIP screen coordinates if there is no parent.
      /// </summary>
      function  GetBounds : TCefRect;
      /// <summary>
      /// Returns the bounds (size and position) of this View in DIP screen
      /// coordinates.
      /// </summary>
      function  GetBoundsInScreen : TCefRect;
      /// <summary>
      /// Sets the size of this View without changing the position. |size| in parent
      /// coordinates, or DIP screen coordinates if there is no parent.
      /// </summary>
      procedure SetSize(const size_: TCefSize);
      /// <summary>
      /// Returns the size of this View in parent coordinates, or DIP screen
      /// coordinates if there is no parent.
      /// </summary>
      function  GetSize : TCefSize;
      /// <summary>
      /// Sets the position of this View without changing the size. |position| is in
      /// parent coordinates, or DIP screen coordinates if there is no parent.
      /// </summary>
      procedure SetPosition(const position_: TCefPoint);
      /// <summary>
      /// Returns the position of this View. Position is in parent coordinates, or
      /// DIP screen coordinates if there is no parent.
      /// </summary>
      function  GetPosition : TCefPoint;
      /// <summary>
      /// Sets the insets for this View. |insets| is in parent coordinates, or DIP
      /// screen coordinates if there is no parent.
      /// </summary>
      procedure SetInsets(const insets: TCefInsets);
      /// <summary>
      /// Returns the insets for this View in parent coordinates, or DIP screen
      /// coordinates if there is no parent.
      /// </summary>
      function  GetInsets: TCefInsets;
      /// <summary>
      /// Returns the size this View would like to be if enough space is available.
      /// Size is in parent coordinates, or DIP screen coordinates if there is no
      /// parent.
      /// </summary>
      function  GetPreferredSize : TCefSize;
      /// <summary>
      /// Size this View to its preferred size. Size is in parent coordinates, or
      /// DIP screen coordinates if there is no parent.
      /// </summary>
      procedure SizeToPreferredSize;
      /// <summary>
      /// Returns the minimum size for this View. Size is in parent coordinates, or
      /// DIP screen coordinates if there is no parent.
      /// </summary>
      function  GetMinimumSize : TCefSize;
      /// <summary>
      /// Returns the maximum size for this View. Size is in parent coordinates, or
      /// DIP screen coordinates if there is no parent.
      /// </summary>
      function  GetMaximumSize : TCefSize;
      /// <summary>
      /// Returns the height necessary to display this View with the provided width.
      /// </summary>
      function  GetHeightForWidth(width: Integer): Integer;
      /// <summary>
      /// Indicate that this View and all parent Views require a re-layout. This
      /// ensures the next call to layout() will propagate to this View even if the
      /// bounds of parent Views do not change.
      /// </summary>
      procedure InvalidateLayout;
      /// <summary>
      /// Sets whether this View is visible. Windows are hidden by default and other
      /// views are visible by default. This View and any parent views must be set
      /// as visible for this View to be drawn in a Window. If this View is set as
      /// hidden then it and any child views will not be drawn and, if any of those
      /// views currently have focus, then focus will also be cleared. Painting is
      /// scheduled as needed. If this View is a Window then calling this function
      /// is equivalent to calling the Window show() and hide() functions.
      /// </summary>
      procedure SetVisible(visible_: boolean);
      /// <summary>
      /// Returns whether this View is visible. A view may be visible but still not
      /// drawn in a Window if any parent views are hidden. If this View is a Window
      /// then a return value of true (1) indicates that this Window is currently
      /// visible to the user on-screen. If this View is not a Window then call
      /// is_drawn() to determine whether this View and all parent views are visible
      /// and will be drawn.
      /// </summary>
      function  IsVisible : boolean;
      /// <summary>
      /// Returns whether this View is visible and drawn in a Window. A view is
      /// drawn if it and all parent views are visible. If this View is a Window
      /// then calling this function is equivalent to calling is_visible().
      /// Otherwise, to determine if the containing Window is visible to the user
      /// on-screen call is_visible() on the Window.
      /// </summary>
      function  IsDrawn : boolean;
      /// <summary>
      /// Set whether this View is enabled. A disabled View does not receive
      /// keyboard or mouse inputs. If |enabled| differs from the current value the
      /// View will be repainted. Also, clears focus if the focused View is
      /// disabled.
      /// </summary>
      procedure SetEnabled(enabled_: boolean);
      /// <summary>
      /// Returns whether this View is enabled.
      /// </summary>
      function  IsEnabled : boolean;
      /// <summary>
      /// Sets whether this View is capable of taking focus. It will clear focus if
      /// the focused View is set to be non-focusable. This is false (0) by default
      /// so that a View used as a container does not get the focus.
      /// </summary>
      procedure SetFocusable(focusable_: boolean);
      /// <summary>
      /// Returns true (1) if this View is focusable, enabled and drawn.
      /// </summary>
      function  IsFocusable : boolean;
      /// <summary>
      /// Return whether this View is focusable when the user requires full keyboard
      /// access, even though it may not be normally focusable.
      /// </summary>
      function  IsAccessibilityFocusable : boolean;
      /// <summary>
      /// Request keyboard focus. If this View is focusable it will become the
      /// focused View.
      /// </summary>
      procedure RequestFocus;
      /// <summary>
      /// Sets the background color for this View. The background color will be
      /// automatically reset when ICefViewDelegate.OnThemeChanged is called.
      /// </summary>
      procedure SetBackgroundColor(color: TCefColor);
      /// <summary>
      /// Returns the background color for this View. If the background color is
      /// unset then the current `GetThemeColor(CEF_ColorPrimaryBackground)` value
      /// will be returned. If this View belongs to an overlay (created with
      /// ICefWindow.AddOverlayView), and the background color is unset, then a
      /// value of transparent (0) will be returned.
      /// </summary>
      function  GetBackgroundColor : TCefColor;
      /// <summary>
      /// Returns the current theme color associated with |color_id|, or the
      /// placeholder color (red) if unset. See cef_color_ids.h for standard ID
      /// values. Standard colors can be overridden and custom colors can be added
      /// using ICefWindow.SetThemeColor.
      /// </summary>
      function  GetThemeColor(color_id: integer): TCefColor;
      /// <summary>
      /// Convert |point| from this View's coordinate system to DIP screen
      /// coordinates. This View must belong to a Window when calling this function.
      /// Returns true (1) if the conversion is successful or false (0) otherwise.
      /// Use ICefDisplay.ConvertPointToPixels() after calling this function
      /// if further conversion to display-specific pixel coordinates is desired.
      /// </summary>
      function  ConvertPointToScreen(var point: TCefPoint): boolean;
      /// <summary>
      /// Convert |point| to this View's coordinate system from DIP screen
      /// coordinates. This View must belong to a Window when calling this function.
      /// Returns true (1) if the conversion is successful or false (0) otherwise.
      /// Use ICefDisplay.ConvertPointFromPixels() before calling this
      /// function if conversion from display-specific pixel coordinates is
      /// necessary.
      /// </summary>
      function  ConvertPointFromScreen(var point: TCefPoint): boolean;
      /// <summary>
      /// Convert |point| from this View's coordinate system to that of the Window.
      /// This View must belong to a Window when calling this function. Returns true
      /// (1) if the conversion is successful or false (0) otherwise.
      /// </summary>
      function  ConvertPointToWindow(var point: TCefPoint): boolean;
      /// <summary>
      /// Convert |point| to this View's coordinate system from that of the Window.
      /// This View must belong to a Window when calling this function. Returns true
      /// (1) if the conversion is successful or false (0) otherwise.
      /// </summary>
      function  ConvertPointFromWindow(var point: TCefPoint): boolean;
      /// <summary>
      /// Convert |point| from this View's coordinate system to that of |view|.
      /// |view| needs to be in the same Window but not necessarily the same view
      /// hierarchy. Returns true (1) if the conversion is successful or false (0)
      /// otherwise.
      /// </summary>
      function  ConvertPointToView(const view : ICefView; var point: TCefPoint): boolean;
      /// <summary>
      /// Convert |point| to this View's coordinate system from that |view|. |view|
      /// needs to be in the same Window but not necessarily the same view
      /// hierarchy. Returns true (1) if the conversion is successful or false (0)
      /// otherwise.
      /// </summary>
      function  ConvertPointFromView(const view : ICefView; var point: TCefPoint): boolean;

    public
      /// <summary>
      /// Returns a ICefView instance using a PCefView data pointer.
      /// </summary>
      class function UnWrap(data: Pointer): ICefView;
  end;

implementation

uses
  uCEFLibFunctions, uCEFMiscFunctions, uCEFBrowserView, uCEFButton, uCEFPanel,
  uCEFScrollView, uCEFTextfield, uCEFViewDelegate, uCEFWindow;

function TCefViewRef.AsBrowserView : ICefBrowserView;
begin
  Result := TCefBrowserViewRef.UnWrap(PCefView(FData)^.as_browser_view(PCefView(FData)));
end;

function TCefViewRef.AsButton : ICefButton;
begin
  Result := TCefButtonRef.UnWrap(PCefView(FData)^.as_button(PCefView(FData)));
end;

function TCefViewRef.AsPanel : ICefPanel;
begin
  Result := TCefPanelRef.UnWrap(PCefView(FData)^.as_panel(PCefView(FData)));
end;

function TCefViewRef.AsScrollView : ICefScrollView;
begin
  Result := TCefScrollViewRef.UnWrap(PCefView(FData)^.as_scroll_view(PCefView(FData)));
end;

function TCefViewRef.AsTextfield : ICefTextfield;
begin
  Result := TCefTextfieldRef.UnWrap(PCefView(FData)^.as_textfield(PCefView(FData)));
end;

function TCefViewRef.GetTypeString : ustring;
begin
  Result := CefStringFreeAndGet(PCefView(FData)^.get_type_string(PCefView(FData)));
end;

function TCefViewRef.ToStringEx(include_children: boolean): ustring;
begin
  Result := CefStringFreeAndGet(PCefView(FData)^.to_string(PCefView(FData), ord(include_children)));
end;

function TCefViewRef.IsValid : boolean;
begin
  Result := (PCefView(FData)^.is_valid(PCefView(FData)) <> 0);
end;

function TCefViewRef.IsAttached : boolean;
begin
  Result := (PCefView(FData)^.is_attached(PCefView(FData)) <> 0);
end;

function TCefViewRef.IsSame(const that: ICefView): boolean;
begin
  Result := PCefView(FData)^.is_same(PCefView(FData), CefGetData(that)) <> 0;
end;

function TCefViewRef.GetDelegate : ICefViewDelegate;
begin
  Result := TCefViewDelegateRef.UnWrap(PCefView(FData)^.get_delegate(PCefView(FData)));
end;

function TCefViewRef.GetWindow : ICefWindow;
begin
  Result := TCefWindowRef.UnWrap(PCefView(FData)^.get_window(PCefView(FData)));
end;

function TCefViewRef.GetID : Integer;
begin
  Result := PCefView(FData)^.get_id(PCefView(FData));
end;

procedure TCefViewRef.SetID(id_: Integer);
begin
  PCefView(FData)^.set_id(PCefView(FData), id_);
end;

function TCefViewRef.GetGroupID : Integer;
begin
  Result := PCefView(FData)^.get_group_id(PCefView(FData));
end;

procedure TCefViewRef.SetGroupID(group_id: Integer);
begin
  PCefView(FData)^.set_group_id(PCefView(FData), group_id);
end;

function TCefViewRef.GetParentView : ICefView;
begin
  Result := UnWrap(PCefView(FData)^.get_parent_view(PCefView(FData)));
end;

function TCefViewRef.GetViewForID(id_: Integer): ICefView;
begin
  Result := UnWrap(PCefView(FData)^.get_view_for_id(PCefView(FData), id_));
end;

procedure TCefViewRef.SetBounds(const bounds_: TCefRect);
begin
  PCefView(FData)^.set_bounds(PCefView(FData), @bounds_);
end;

function TCefViewRef.GetBounds : TCefRect;
begin
  Result := PCefView(FData)^.get_bounds(PCefView(FData));
end;

function TCefViewRef.GetBoundsInScreen : TCefRect;
begin
  Result := PCefView(FData)^.get_bounds_in_screen(PCefView(FData));
end;

procedure TCefViewRef.SetSize(const size_: TCefSize);
begin
  PCefView(FData)^.set_size(PCefView(FData), @size_);
end;

function TCefViewRef.GetSize : TCefSize;
begin
  Result := PCefView(FData)^.get_size(PCefView(FData));
end;

procedure TCefViewRef.SetPosition(const position_: TCefPoint);
begin
  PCefView(FData)^.set_position(PCefView(FData), @position_);
end;

function TCefViewRef.GetPosition : TCefPoint;
begin
  Result := PCefView(FData)^.get_position(PCefView(FData));
end;

procedure TCefViewRef.SetInsets(const insets: TCefInsets);
begin
  PCefView(FData)^.set_insets(PCefView(FData), @insets);
end;

function TCefViewRef.GetInsets: TCefInsets;
begin
  Result := PCefView(FData)^.get_insets(PCefView(FData));
end;

function TCefViewRef.GetPreferredSize : TCefSize;
begin
  Result := PCefView(FData)^.get_preferred_size(PCefView(FData));
end;

procedure TCefViewRef.SizeToPreferredSize;
begin
  PCefView(FData)^.size_to_preferred_size(PCefView(FData));
end;

function TCefViewRef.GetMinimumSize : TCefSize;
begin
  Result := PCefView(FData)^.get_minimum_size(PCefView(FData));
end;

function TCefViewRef.GetMaximumSize : TCefSize;
begin
  Result := PCefView(FData)^.get_maximum_size(PCefView(FData));
end;

function TCefViewRef.GetHeightForWidth(width: Integer): Integer;
begin
  Result := PCefView(FData)^.get_height_for_width(PCefView(FData), width);
end;

procedure TCefViewRef.InvalidateLayout;
begin
  PCefView(FData)^.invalidate_layout(PCefView(FData));
end;

procedure TCefViewRef.SetVisible(visible_: boolean);
begin
  PCefView(FData)^.set_visible(PCefView(FData), ord(visible_));
end;

function TCefViewRef.IsVisible : boolean;
begin
  Result := (PCefView(FData)^.is_visible(PCefView(FData)) <> 0);
end;

function TCefViewRef.IsDrawn : boolean;
begin
  Result := (PCefView(FData)^.is_drawn(PCefView(FData)) <> 0);
end;

procedure TCefViewRef.SetEnabled(enabled_: boolean);
begin
  PCefView(FData)^.set_enabled(PCefView(FData), ord(enabled_));
end;

function TCefViewRef.IsEnabled : boolean;
begin
  Result := (PCefView(FData)^.is_enabled(PCefView(FData)) <> 0);
end;

procedure TCefViewRef.SetFocusable(focusable_: boolean);
begin
  PCefView(FData)^.set_focusable(PCefView(FData), ord(focusable_));
end;

function TCefViewRef.IsFocusable : boolean;
begin
  Result := (PCefView(FData)^.is_focusable(PCefView(FData)) <> 0);
end;

function TCefViewRef.IsAccessibilityFocusable : boolean;
begin
  Result := (PCefView(FData)^.is_accessibility_focusable(PCefView(FData)) <> 0);
end;

procedure TCefViewRef.RequestFocus;
begin
  PCefView(FData)^.request_focus(PCefView(FData));
end;

procedure TCefViewRef.SetBackgroundColor(color: TCefColor);
begin
  PCefView(FData)^.set_background_color(PCefView(FData), color);
end;

function TCefViewRef.GetBackgroundColor : TCefColor;
begin
  Result := PCefView(FData)^.get_background_color(PCefView(FData));
end;

function TCefViewRef.GetThemeColor(color_id: integer): TCefColor;
begin
  Result := PCefView(FData)^.get_theme_color(PCefView(FData), color_id);
end;

function TCefViewRef.ConvertPointToScreen(var point: TCefPoint): boolean;
begin
  Result := (PCefView(FData)^.convert_point_to_screen(PCefView(FData), @point) <> 0);
end;

function TCefViewRef.ConvertPointFromScreen(var point: TCefPoint): boolean;
begin
  Result := (PCefView(FData)^.convert_point_from_screen(PCefView(FData), @point) <> 0);
end;

function TCefViewRef.ConvertPointToWindow(var point: TCefPoint): boolean;
begin
  Result := (PCefView(FData)^.convert_point_to_window(PCefView(FData), @point) <> 0);
end;

function TCefViewRef.ConvertPointFromWindow(var point: TCefPoint): boolean;
begin
  Result := (PCefView(FData)^.convert_point_from_window(PCefView(FData), @point) <> 0);
end;

function TCefViewRef.ConvertPointToView(const view : ICefView; var point: TCefPoint): boolean;
begin
  Result := (PCefView(FData)^.convert_point_to_view(PCefView(FData), CefGetData(view), @point) <> 0);
end;

function TCefViewRef.ConvertPointFromView(const view : ICefView; var point: TCefPoint): boolean;
begin
  Result := (PCefView(FData)^.convert_point_from_view(PCefView(FData), CefGetData(view), @point) <> 0);
end;

class function TCefViewRef.UnWrap(data: Pointer): ICefView;
begin
  if (data <> nil) then
    Result := Create(data) as ICefView
   else
    Result := nil;
end;

end.

