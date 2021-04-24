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

unit uCEFDisplay;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes, System.SysUtils,
  {$ELSE}
  Classes, SysUtils,
  {$ENDIF}
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefDisplayRef = class(TCefBaseRefCountedRef, ICefDisplay)
    protected
      function  GetID : int64;
      function  GetDeviceScaleFactor : Single;
      procedure ConvertPointToPixels(var point: TCefPoint);
      procedure ConvertPointFromPixels(var point: TCefPoint);
      function  GetBounds : TCefRect;
      function  GetWorkArea : TCefRect;
      function  GetRotation : Integer;

    public
      class function UnWrap(data: Pointer): ICefDisplay;
      class function Primary: ICefDisplay;
      class function NearestPoint(const point: TCefPoint; input_pixel_coords: boolean): ICefDisplay;
      class function MatchingBounds(const bounds: TCefRect; input_pixel_coords: boolean): ICefDisplay;
      class function GetCount: NativeUInt;
      class function GetAlls(var aDisplayArray : TCefDisplayArray) : boolean;
  end;

implementation

uses
  uCEFLibFunctions;

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
  Result := UnWrap(cef_display_get_primary());
end;

class function TCefDisplayRef.NearestPoint(const point: TCefPoint; input_pixel_coords: boolean): ICefDisplay;
begin
  Result := UnWrap(cef_display_get_nearest_point(@point, ord(input_pixel_coords)));
end;

class function TCefDisplayRef.MatchingBounds(const bounds: TCefRect; input_pixel_coords: boolean): ICefDisplay;
begin
  Result := UnWrap(cef_display_get_matching_bounds(@bounds, ord(input_pixel_coords)));
end;

class function TCefDisplayRef.GetCount: NativeUInt;
begin
  Result := cef_display_get_count();
end;

class function TCefDisplayRef.GetAlls(var aDisplayArray : TCefDisplayArray) : boolean;
type
  TDisplayArray = array of PCefDisplay;
var
  i, displaysCount: NativeUInt;
  displays: PPCefDisplay;
  TempSize : integer;
begin
  Result        := False;
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

end.
