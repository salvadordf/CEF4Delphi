unit uCEFImage;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

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
