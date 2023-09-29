unit uCEFDisplay;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes, System.SysUtils, System.Types,
  {$ELSE}
  Classes, SysUtils, Types,
  {$ENDIF}
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  /// <summary>
  /// This class typically, but not always, corresponds to a physical display
  /// connected to the system. A fake Display may exist on a headless system, or a
  /// Display may correspond to a remote, virtual display. All size and position
  /// values are in density independent pixel (DIP) coordinates unless otherwise
  /// indicated. Methods must be called on the browser process UI thread unless
  /// otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_display_capi.h">CEF source file: /include/capi/views/cef_display_capi.h (cef_display_t)</see></para>
  /// </remarks>
  TCefDisplayRef = class(TCefBaseRefCountedRef, ICefDisplay)
    protected
      /// <summary>
      /// Returns the unique identifier for this Display.
      /// </summary>
      function  GetID : int64;
      /// <summary>
      /// Returns this Display's device pixel scale factor. This specifies how much
      /// the UI should be scaled when the actual output has more pixels than
      /// standard displays (which is around 100~120dpi). The potential return
      /// values differ by platform.
      /// </summary>
      function  GetDeviceScaleFactor : Single;
      /// <summary>
      /// Convert |point| from DIP coordinates to pixel coordinates using this
      /// Display's device scale factor.
      /// </summary>
      procedure ConvertPointToPixels(var point: TCefPoint);
      /// <summary>
      /// Convert |point| from pixel coordinates to DIP coordinates using this
      /// Display's device scale factor.
      /// </summary>
      procedure ConvertPointFromPixels(var point: TCefPoint);
      /// <summary>
      /// Returns this Display's bounds in DIP screen coordinates. This is the full
      /// size of the display.
      /// </summary>
      function  GetBounds : TCefRect;
      /// <summary>
      /// Returns this Display's work area in DIP screen coordinates. This excludes
      /// areas of the display that are occupied with window manager toolbars, etc.
      /// </summary>
      function  GetWorkArea : TCefRect;
      /// <summary>
      /// Returns this Display's rotation in degrees.
      /// </summary>
      function  GetRotation : Integer;

    public
      /// <summary>
      /// Returns a ICefDisplay instance using a PCefDisplay data pointer.
      /// </summary>
      class function UnWrap(data: Pointer): ICefDisplay;
      /// <summary>
      /// Returns the primary Display.
      /// </summary>
      class function Primary: ICefDisplay;
      /// <summary>
      /// Returns the Display nearest |point|. Set |input_pixel_coords| to true (1) if
      /// |point| is in pixel screen coordinates instead of DIP screen coordinates.
      /// </summary>
      class function NearestPoint(const point: TCefPoint; input_pixel_coords: boolean): ICefDisplay;
      /// <summary>
      /// Returns the Display that most closely intersects |bounds|.  Set
      /// |input_pixel_coords| to true (1) if |bounds| is in pixel screen coordinates
      /// instead of DIP screen coordinates.
      /// </summary>
      class function MatchingBounds(const bounds: TCefRect; input_pixel_coords: boolean): ICefDisplay;
      /// <summary>
      /// Returns the total number of Displays. Mirrored displays are excluded; this
      /// function is intended to return the number of distinct, usable displays.
      /// </summary>
      class function GetCount: NativeUInt;
      /// <summary>
      /// Returns all Displays. Mirrored displays are excluded; this function is
      /// intended to return distinct, usable displays.
      /// </summary>
      class function GetAlls(var aDisplayArray : TCefDisplayArray) : boolean;
      /// <summary>
      /// Convert |point| from DIP screen coordinates to pixel screen coordinates.
      /// This function is only used on Windows.
      /// </summary>
      class function ScreenPointToPixels(const aScreenPoint : TPoint) : TPoint;
      /// <summary>
      /// Convert |point| from pixel screen coordinates to DIP screen coordinates.
      /// This function is only used on Windows.
      /// </summary>
      class function ScreenPointFromPixels(const aPixelsPoint : TPoint) : TPoint;
      /// <summary>
      /// Convert |rect| from DIP screen coordinates to pixel screen coordinates. This
      /// function is only used on Windows.
      /// </summary>
      class function ScreenRectToPixels(const aScreenRect : TRect) : TRect;
      /// <summary>
      /// Convert |rect| from pixel screen coordinates to DIP screen coordinates. This
      /// function is only used on Windows.
      /// </summary>
      class function ScreenRectFromPixels(const aPixelsRect : TRect) : TRect;
  end;

implementation

uses
  uCEFLibFunctions, uCEFApplicationCore;

function TCefDisplayRef.GetID : int64;
begin
  Result := PCefDisplay(FData)^.get_id(PCefDisplay(FData));
end;

function TCefDisplayRef.GetDeviceScaleFactor : Single;
begin
  Result := PCefDisplay(FData)^.get_device_scale_factor(PCefDisplay(FData));
end;

procedure TCefDisplayRef.ConvertPointToPixels(var point: TCefPoint);
begin
  PCefDisplay(FData)^.convert_point_to_pixels(PCefDisplay(FData), @point);
end;

procedure TCefDisplayRef.ConvertPointFromPixels(var point: TCefPoint);
begin
  PCefDisplay(FData)^.convert_point_from_pixels(PCefDisplay(FData), @point);
end;

function TCefDisplayRef.GetBounds : TCefRect;
begin
  Result := PCefDisplay(FData)^.get_bounds(PCefDisplay(FData));
end;

function TCefDisplayRef.GetWorkArea : TCefRect;
begin
  Result := PCefDisplay(FData)^.get_work_area(PCefDisplay(FData));
end;

function TCefDisplayRef.GetRotation : Integer;
begin
  Result := PCefDisplay(FData)^.get_rotation(PCefDisplay(FData));
end;

class function TCefDisplayRef.UnWrap(data: Pointer): ICefDisplay;
begin
  if (data <> nil) then
    Result := Create(data) as ICefDisplay
   else
    Result := nil;
end;

class function TCefDisplayRef.Primary: ICefDisplay;
begin
  if assigned(GlobalCEFApp) and GlobalCEFApp.LibLoaded then
    Result := UnWrap(cef_display_get_primary())
   else
    Result := nil;
end;

class function TCefDisplayRef.NearestPoint(const point: TCefPoint; input_pixel_coords: boolean): ICefDisplay;
begin
  if assigned(GlobalCEFApp) and GlobalCEFApp.LibLoaded then
    Result := UnWrap(cef_display_get_nearest_point(@point, ord(input_pixel_coords)))
   else
    Result := nil;
end;

class function TCefDisplayRef.MatchingBounds(const bounds: TCefRect; input_pixel_coords: boolean): ICefDisplay;
begin
  if assigned(GlobalCEFApp) and GlobalCEFApp.LibLoaded then
    Result := UnWrap(cef_display_get_matching_bounds(@bounds, ord(input_pixel_coords)))
   else
    Result := nil;
end;

class function TCefDisplayRef.GetCount: NativeUInt;
begin
  if assigned(GlobalCEFApp) and GlobalCEFApp.LibLoaded then
    Result := cef_display_get_count()
   else
    Result := 0;
end;

class function TCefDisplayRef.GetAlls(var aDisplayArray : TCefDisplayArray) : boolean;
type
  TDisplayArray = array of PCefDisplay;
var
  i, displaysCount: NativeUInt;
  displays: PPCefDisplay;
  TempSize : integer;
begin
  Result := False;
  if (GlobalCEFApp = nil) or not(GlobalCEFApp.LibLoaded) then
    exit;

  displaysCount := GetCount;

  if (displaysCount > 0) then
    try
      TempSize := SizeOf(TCefDisplay) * displaysCount;
      GetMem(displays, TempSize);
      FillChar(displays, TempSize, 0);

      cef_display_get_alls(@displaysCount, displays);

      SetLength(aDisplayArray, displaysCount);

      i := 0;
      while (i < displaysCount) do
        begin
          aDisplayArray[i] := TCefDisplayRef.UnWrap(TDisplayArray(displays)[i]);
          inc(i);
        end;

      Result := True;
    finally
      FreeMem(displays);
    end;
end;

class function TCefDisplayRef.ScreenPointToPixels(const aScreenPoint : TPoint) : TPoint;
var
  TempScreenPt, TempPixelsPt : TCefPoint;
begin
  if assigned(GlobalCEFApp) and GlobalCEFApp.LibLoaded then
    begin
      TempScreenPt.x := aScreenPoint.X;
      TempScreenPt.y := aScreenPoint.Y;
      TempPixelsPt   := cef_display_convert_screen_point_to_pixels(@TempScreenPt);
      Result.X       := TempPixelsPt.x;
      Result.Y       := TempPixelsPt.y;
    end
   else
    Result := aScreenPoint;
end;

class function TCefDisplayRef.ScreenPointFromPixels(const aPixelsPoint : TPoint) : TPoint;
var
  TempScreenPt, TempPixelsPt : TCefPoint;
begin
  if assigned(GlobalCEFApp) and GlobalCEFApp.LibLoaded then
    begin
      TempPixelsPt.x := aPixelsPoint.X;
      TempPixelsPt.y := aPixelsPoint.Y;
      TempScreenPt   := cef_display_convert_screen_point_from_pixels(@TempPixelsPt);
      Result.X       := TempScreenPt.x;
      Result.Y       := TempScreenPt.y;
    end
   else
    Result := aPixelsPoint;
end;

class function TCefDisplayRef.ScreenRectToPixels(const aScreenRect : TRect) : TRect;
var
  TempScreenRc, TempPixelsRc : TCefRect;
begin
  if assigned(GlobalCEFApp) and GlobalCEFApp.LibLoaded then
    begin
      TempScreenRc.x := aScreenRect.Left;
      TempScreenRc.y := aScreenRect.Top;
      TempPixelsRc   := cef_display_convert_screen_rect_to_pixels(@TempScreenRc);
      Result.Left    := TempPixelsRc.x;
      Result.Top     := TempPixelsRc.y;
      Result.Right   := TempPixelsRc.x + TempPixelsRc.Width - 1;
      Result.Bottom  := TempPixelsRc.y + TempPixelsRc.Height - 1;
    end
   else
    Result := aScreenRect;
end;

class function TCefDisplayRef.ScreenRectFromPixels(const aPixelsRect : TRect) : TRect;
var
  TempScreenRc, TempPixelsRc : TCefRect;
begin
  if assigned(GlobalCEFApp) and GlobalCEFApp.LibLoaded then
    begin
      TempPixelsRc.x := aPixelsRect.Left;
      TempPixelsRc.y := aPixelsRect.Top;
      TempScreenRc   := cef_display_convert_screen_rect_from_pixels(@TempPixelsRc);
      Result.Left    := TempScreenRc.x;
      Result.Top     := TempScreenRc.y;
      Result.Right   := TempScreenRc.x + TempScreenRc.Width - 1;
      Result.Bottom  := TempScreenRc.y + TempScreenRc.Height - 1;
    end
   else
    Result := aPixelsRect;
end;

end.
