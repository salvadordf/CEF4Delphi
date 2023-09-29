unit uCEFOverlayController;

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
  /// Controller for an overlay that contains a contents View added via
  /// ICefWindow.AddOverlayView. Methods exposed by this controller should be
  /// called in preference to functions of the same name exposed by the contents
  /// View unless otherwise indicated. Methods must be called on the browser
  /// process UI thread unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_overlay_controller_capi.h">CEF source file: /include/capi/views/cef_overlay_controller_capi.h (cef_overlay_controller_t)</see></para>
  /// </remarks>
  TCefOverlayControllerRef = class(TCefBaseRefCountedRef, ICefOverlayController)
    public
      /// <summary>
      /// Returns true (1) if this object is valid.
      /// </summary>
      function  IsValid: boolean;
      /// <summary>
      /// Returns true (1) if this object is the same as |that| object.
      /// </summary>
      function  IsSame(const that: ICefOverlayController): boolean;
      /// <summary>
      /// Returns the contents View for this overlay.
      /// </summary>
      function  GetContentsView: ICefView;
      /// <summary>
      /// Returns the top-level Window hosting this overlay. Use this function
      /// instead of calling get_window() on the contents View.
      /// </summary>
      function  GetWindow: ICefWindow;
      /// <summary>
      /// Returns the docking mode for this overlay.
      /// </summary>
      function  GetDockingMode: TCefDockingMode;
      /// <summary>
      /// Destroy this overlay.
      /// </summary>
      procedure DestroyOverlay;
      /// <summary>
      /// Sets the bounds (size and position) of this overlay. This will set the
      /// bounds of the contents View to match and trigger a re-layout if necessary.
      /// |bounds| is in parent coordinates and any insets configured on this
      /// overlay will be ignored. Use this function only for overlays created with
      /// a docking mode value of CEF_DOCKING_MODE_CUSTOM. With other docking modes
      /// modify the insets of this overlay and/or layout of the contents View and
      /// call size_to_preferred_size() instead to calculate the new size and re-
      /// position the overlay if necessary.
      /// </summary>
      procedure SetBounds(const bounds: TCefRect);
      /// <summary>
      /// Returns the bounds (size and position) of this overlay in parent
      /// coordinates.
      /// </summary>
      function  GetBounds: TCefRect;
      /// <summary>
      /// Returns the bounds (size and position) of this overlay in DIP screen
      /// coordinates.
      /// </summary>
      function  GetBoundsInScreen: TCefRect;
      /// <summary>
      /// Sets the size of this overlay without changing the position. This will set
      /// the size of the contents View to match and trigger a re-layout if
      /// necessary. |size| is in parent coordinates and any insets configured on
      /// this overlay will be ignored. Use this function only for overlays created
      /// with a docking mode value of CEF_DOCKING_MODE_CUSTOM. With other docking
      /// modes modify the insets of this overlay and/or layout of the contents View
      /// and call size_to_preferred_size() instead to calculate the new size and
      /// re-position the overlay if necessary.
      /// </summary>
      procedure SetSize(const size: TCefSize);
      /// <summary>
      /// Returns the size of this overlay in parent coordinates.
      /// </summary>
      function  GetSize: TCefSize;
      /// <summary>
      /// Sets the position of this overlay without changing the size. |position| is
      /// in parent coordinates and any insets configured on this overlay will be
      /// ignored. Use this function only for overlays created with a docking mode
      /// value of CEF_DOCKING_MODE_CUSTOM. With other docking modes modify the
      /// insets of this overlay and/or layout of the contents View and call
      /// size_to_preferred_size() instead to calculate the new size and re-position
      /// the overlay if necessary.
      /// </summary>
      procedure SetPosition(const position: TCefPoint);
      /// <summary>
      /// Returns the position of this overlay in parent coordinates.
      /// </summary>
      function  GetPosition: TCefPoint;
      /// <summary>
      /// Sets the insets for this overlay. |insets| is in parent coordinates. Use
      /// this function only for overlays created with a docking mode value other
      /// than CEF_DOCKING_MODE_CUSTOM.
      /// </summary>
      procedure SetInsets(const insets: TCefInsets);
      /// <summary>
      /// Returns the insets for this overlay in parent coordinates.
      /// </summary>
      function  GetInsets: TCefInsets;
      /// <summary>
      /// Size this overlay to its preferred size and trigger a re-layout if
      /// necessary. The position of overlays created with a docking mode value of
      /// CEF_DOCKING_MODE_CUSTOM will not be modified by calling this function.
      /// With other docking modes this function may re-position the overlay if
      /// necessary to accommodate the new size and any insets configured on the
      /// contents View.
      /// </summary>
      procedure SizeToPreferredSize;
      /// <summary>
      /// Sets whether this overlay is visible. Overlays are hidden by default. If
      /// this overlay is hidden then it and any child Views will not be drawn and,
      /// if any of those Views currently have focus, then focus will also be
      /// cleared. Painting is scheduled as needed.
      /// </summary>
      procedure SetVisible(visible: boolean);
      /// <summary>
      /// Returns whether this overlay is visible. A View may be visible but still
      /// not drawn in a Window if any parent Views are hidden. Call is_drawn() to
      /// determine whether this overlay and all parent Views are visible and will
      /// be drawn.
      /// </summary>
      function  IsVisible: boolean;
      /// <summary>
      /// Returns whether this overlay is visible and drawn in a Window. A View is
      /// drawn if it and all parent Views are visible. To determine if the
      /// containing Window is visible to the user on-screen call is_visible() on
      /// the Window.
      /// </summary>
      function  IsDrawn: boolean;

      /// <summary>
      /// Returns a ICefOverlayController instance using a PCefOverlayController data pointer.
      /// </summary>
      class function UnWrap(data: Pointer): ICefOverlayController;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFView, uCEFWindow;

function TCefOverlayControllerRef.IsValid: boolean;
begin
  Result := PCefOverlayController(FData)^.is_valid(PCefOverlayController(FData)) <> 0;
end;

function TCefOverlayControllerRef.IsSame(const that: ICefOverlayController): boolean;
begin
  Result := PCefOverlayController(FData)^.is_same(PCefOverlayController(FData), CefGetData(that)) <> 0;
end;

function TCefOverlayControllerRef.GetContentsView: ICefView;
begin
  Result := TCefViewRef.UnWrap(PCefOverlayController(FData)^.get_contents_view(PCefOverlayController(FData)));
end;

function TCefOverlayControllerRef.GetWindow: ICefWindow;
begin
  Result := TCefWindowRef.UnWrap(PCefOverlayController(FData)^.get_window(PCefOverlayController(FData)));
end;

function TCefOverlayControllerRef.GetDockingMode: TCefDockingMode;
begin
  Result := PCefOverlayController(FData)^.get_docking_mode(PCefOverlayController(FData));
end;

procedure TCefOverlayControllerRef.DestroyOverlay;
begin
  PCefOverlayController(FData)^.destroy(PCefOverlayController(FData));
end;

procedure TCefOverlayControllerRef.SetBounds(const bounds: TCefRect);
begin
  PCefOverlayController(FData)^.set_bounds(PCefOverlayController(FData), @bounds);
end;

function TCefOverlayControllerRef.GetBounds: TCefRect;
begin
  Result := PCefOverlayController(FData)^.get_bounds(PCefOverlayController(FData));
end;

function TCefOverlayControllerRef.GetBoundsInScreen: TCefRect;
begin
  Result := PCefOverlayController(FData)^.get_bounds_in_screen(PCefOverlayController(FData));
end;

procedure TCefOverlayControllerRef.SetSize(const size: TCefSize);
begin
  PCefOverlayController(FData)^.set_size(PCefOverlayController(FData), @size);
end;

function TCefOverlayControllerRef.GetSize: TCefSize;
begin
  Result := PCefOverlayController(FData)^.get_size(PCefOverlayController(FData));
end;

procedure TCefOverlayControllerRef.SetPosition(const position: TCefPoint);
begin
  PCefOverlayController(FData)^.set_position(PCefOverlayController(FData), @position);
end;

function TCefOverlayControllerRef.GetPosition: TCefPoint;
begin
  Result := PCefOverlayController(FData)^.get_position(PCefOverlayController(FData));
end;

procedure TCefOverlayControllerRef.SetInsets(const insets: TCefInsets);
begin
  PCefOverlayController(FData)^.set_insets(PCefOverlayController(FData), @insets);
end;

function TCefOverlayControllerRef.GetInsets: TCefInsets;
begin
  Result := PCefOverlayController(FData)^.get_insets(PCefOverlayController(FData));
end;

procedure TCefOverlayControllerRef.SizeToPreferredSize;
begin
  PCefOverlayController(FData)^.size_to_preferred_size(PCefOverlayController(FData));
end;

procedure TCefOverlayControllerRef.SetVisible(visible: boolean);
begin
  PCefOverlayController(FData)^.set_visible(PCefOverlayController(FData), ord(visible));
end;

function TCefOverlayControllerRef.IsVisible: boolean;
begin
  Result := PCefOverlayController(FData)^.is_visible(PCefOverlayController(FData)) <> 0;
end;

function TCefOverlayControllerRef.IsDrawn: boolean;
begin
  Result := PCefOverlayController(FData)^.is_drawn(PCefOverlayController(FData)) <> 0;
end;

class function TCefOverlayControllerRef.UnWrap(data: Pointer): ICefOverlayController;
begin
  if (data <> nil) then
    Result := Create(data) as ICefOverlayController
   else
    Result := nil;
end;

end.
