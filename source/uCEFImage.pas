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

unit uCEFImage;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  {$IFDEF MSWINDOWS}WinApi.Windows,{$ENDIF}
  {$ELSE}
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  {$ENDIF}
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefImageRef = class(TCefBaseRefCountedRef, ICefImage)
  protected
    function IsEmpty: Boolean;
    function IsSame(const that: ICefImage): Boolean;
    function AddBitmap(scaleFactor: Single; pixelWidth, pixelHeight: Integer; colorType: TCefColorType; alphaType: TCefAlphaType; const pixelData: Pointer; pixelDataSize: NativeUInt): Boolean;
    function AddPng(scaleFactor: Single; const pngData: Pointer; pngDataSize: NativeUInt): Boolean;
    function AddJpeg(scaleFactor: Single; const jpegData: Pointer; jpegDataSize: NativeUInt): Boolean;
    function GetWidth: NativeUInt;
    function GetHeight: NativeUInt;
    function HasRepresentation(scaleFactor: Single): Boolean;
    function RemoveRepresentation(scaleFactor: Single): Boolean;
    function GetRepresentationInfo(scaleFactor: Single; var actualScaleFactor: Single; var pixelWidth, pixelHeight: Integer): Boolean;
    function GetAsBitmap(scaleFactor: Single; colorType: TCefColorType; alphaType: TCefAlphaType; var pixelWidth, pixelHeight: Integer): ICefBinaryValue;
    function GetAsPng(scaleFactor: Single; withTransparency: Boolean; var pixelWidth, pixelHeight: Integer): ICefBinaryValue;
    function GetAsJpeg(scaleFactor: Single; quality: Integer; var pixelWidth, pixelHeight: Integer): ICefBinaryValue;
  public
    class function UnWrap(data: Pointer): ICefImage;
    class function New: ICefImage;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBinaryValue;

function TCefImageRef.AddBitmap(scaleFactor     : Single;
                                pixelWidth      : Integer;
                                pixelHeight     : Integer;
                                colorType       : TCefColorType;
                                alphaType       : TCefAlphaType;
                                const pixelData : Pointer;
                                pixelDataSize   : NativeUInt): Boolean;
begin
  Result := PCefImage(FData)^.add_bitmap(FData, scaleFactor, pixelWidth, pixelHeight, colorType, alphaType, pixelData, pixelDataSize) <> 0;
end;

function TCefImageRef.AddJpeg(scaleFactor: Single; const jpegData: Pointer; jpegDataSize: NativeUInt): Boolean;
begin
  Result := PCefImage(FData)^.add_jpeg(FData, scaleFactor, jpegData, jpegDataSize) <> 0;
end;

function TCefImageRef.AddPng(scaleFactor: Single; const pngData: Pointer; pngDataSize: NativeUInt): Boolean;
begin
  Result := PCefImage(FData)^.add_png(FData, scaleFactor, pngData, pngDataSize) <> 0;
end;

function TCefImageRef.GetAsBitmap(scaleFactor: Single; colorType: TCefColorType; alphaType: TCefAlphaType; var pixelWidth, pixelHeight: Integer): ICefBinaryValue;
begin
  Result := TCefBinaryValueRef.UnWrap(PCefImage(FData)^.get_as_bitmap(FData, scaleFactor, colorType, alphaType, @pixelWidth, @pixelHeight));
end;

function TCefImageRef.GetAsJpeg(scaleFactor: Single; quality: Integer; var pixelWidth, pixelHeight: Integer): ICefBinaryValue;
begin
  Result := TCefBinaryValueRef.UnWrap(PCefImage(FData)^.get_as_jpeg(FData, scaleFactor, quality, @pixelWidth, @pixelHeight));
end;

function TCefImageRef.GetAsPng(scaleFactor: Single; withTransparency: Boolean; var pixelWidth, pixelHeight: Integer): ICefBinaryValue;
begin
  Result := TCefBinaryValueRef.UnWrap(PCefImage(FData)^.get_as_png(FData, scaleFactor, Ord(withTransparency), @pixelWidth, @pixelHeight));
end;

function TCefImageRef.GetHeight: NativeUInt;
begin
  Result := PCefImage(FData)^.get_height(FData);
end;

function TCefImageRef.GetRepresentationInfo(scaleFactor: Single; var actualScaleFactor: Single; var pixelWidth, pixelHeight: Integer): Boolean;
begin
  Result := PCefImage(FData)^.get_representation_info(FData, scaleFactor, @actualScaleFactor, @pixelWidth, @pixelHeight) <> 0;
end;

function TCefImageRef.GetWidth: NativeUInt;
begin
  Result := PCefImage(FData)^.get_width(FData);
end;

function TCefImageRef.HasRepresentation(scaleFactor: Single): Boolean;
begin
  Result := PCefImage(FData)^.has_representation(FData, scaleFactor) <> 0;
end;

function TCefImageRef.IsEmpty: Boolean;
begin
  Result := PCefImage(FData)^.is_empty(FData) <> 0;
end;

function TCefImageRef.IsSame(const that: ICefImage): Boolean;
begin
  Result := PCefImage(FData)^.is_same(FData, CefGetData(that)) <> 0;
end;

class function TCefImageRef.New: ICefImage;
begin
  Result := UnWrap(cef_image_create());
end;

function TCefImageRef.RemoveRepresentation(scaleFactor: Single): Boolean;
begin
  Result := PCefImage(FData)^.remove_representation(FData, scaleFactor) <> 0;
end;

class function TCefImageRef.UnWrap(data: Pointer): ICefImage;
begin
  if (data <> nil) then
    Result := Create(data) as ICefImage
   else
    Result := nil;
end;

end.
