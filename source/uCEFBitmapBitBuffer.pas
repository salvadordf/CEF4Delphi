unit uCEFBitmapBitBuffer;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes, System.SysUtils;
  {$ELSE}
  Classes, SysUtils;
  {$ENDIF}

type
  /// <summary>
  /// Class that stores a copy of the raw bitmap buffer sent by CEF in the TChromiumCore.OnPaint event.
  /// </summary>
  TCEFBitmapBitBuffer = class
    protected
      FBuffer          : pointer;
      FImageWidth      : integer;
      FImageHeight     : integer;
      FBufferWidth     : integer;
      FBufferHeight    : integer;

      function  GetBufferScanlineSize : integer;
      function  GetScanlineSize : integer;
      function  GetBufferLength : integer;
      function  GetEmpty : boolean;
      function  GetScanline(y : integer) : PByte;

      procedure CreateBuffer;
      procedure DestroyBuffer;

    public
      constructor Create(aWidth, aHeight : integer);
      destructor  Destroy; override;
      /// <summary>
      /// Updates the image size.
      /// </summary>
      procedure   UpdateSize(aWidth, aHeight : integer);
      /// <summary>
      /// Image width.
      /// </summary>
      property    Width                  : integer   read FImageWidth;
      /// <summary>
      /// Image height.
      /// </summary>
      property    Height                 : integer   read FImageHeight;
      /// <summary>
      /// Buffer length.
      /// </summary>
      property    BufferLength           : integer   read GetBufferLength;
      /// <summary>
      /// Returns true if the buffer is empty.
      /// </summary>
      property    Empty                  : boolean   read GetEmpty;
      /// <summary>
      /// Returns a pointer to the first byte in of the Y scnaline.
      /// </summary>
      property    Scanline[y : integer]  : PByte     read GetScanline;
      /// <summary>
      /// Returns the scanline size.
      /// </summary>
      property    ScanlineSize           : integer   read GetScanlineSize;
      /// <summary>
      /// Returns the real buffer scanline size.
      /// </summary>
      property    BufferScanlineSize     : integer   read GetBufferScanlineSize;
      /// <summary>
      /// Returns a pointer to the buffer that stores the image.
      /// </summary>
      property    BufferBits             : pointer   read FBuffer;
  end;

implementation

const
  RGBQUAD_SIZE = 4;
  BUFFER_MULTIPLIER = 1.1;

constructor TCEFBitmapBitBuffer.Create(aWidth, aHeight : integer);
begin
  inherited Create;

  FBuffer       := nil;
  FImageWidth   := 0;
  FImageHeight  := 0;
  FBufferWidth  := 0;
  FBufferHeight := 0;

  UpdateSize(aWidth, aHeight);
end;

destructor TCEFBitmapBitBuffer.Destroy;
begin
  DestroyBuffer;

  inherited Destroy;
end;

procedure TCEFBitmapBitBuffer.DestroyBuffer;
begin
  if (FBuffer <> nil) then
    begin
      FreeMem(FBuffer);
      FBuffer := nil;
    end;
end;

function TCEFBitmapBitBuffer.GetScanlineSize : integer;
begin
  Result := FImageWidth * RGBQUAD_SIZE;
end;

function TCEFBitmapBitBuffer.GetBufferScanlineSize : integer;
begin
  Result := FBufferWidth * RGBQUAD_SIZE;
end;

function TCEFBitmapBitBuffer.GetBufferLength : integer;
begin
  Result := FBufferHeight * BufferScanlineSize;
end;

function TCEFBitmapBitBuffer.GetEmpty : boolean;
begin
  Result := (BufferLength = 0) or (FBuffer = nil);
end;

function TCEFBitmapBitBuffer.GetScanline(y : integer) : PByte;
begin
  if (FBuffer = nil) or (y >= FImageHeight) then
    Result := nil
   else
    begin
      Result := PByte(FBuffer);
      if (y > 0) then inc(Result, y * BufferScanlineSize);
    end;
end;

procedure TCEFBitmapBitBuffer.CreateBuffer;
var
  TempLen : integer;
begin
  TempLen := BufferLength;

  if (TempLen > 0) then
    begin
      GetMem(FBuffer, TempLen);
      FillChar(FBuffer^, TempLen, $FF);
    end;
end;

procedure TCEFBitmapBitBuffer.UpdateSize(aWidth, aHeight : integer);
begin
  if (aWidth > 0) and (aHeight > 0) then
    begin
      FImageWidth  := aWidth;
      FImageHeight := aHeight;

      if (FImageWidth  > FBufferWidth)  or
         (FImageHeight > FBufferHeight) then
        begin
          FBufferWidth  := round(FImageWidth  * BUFFER_MULTIPLIER);
          FBufferHeight := round(FImageHeight * BUFFER_MULTIPLIER);

          DestroyBuffer;
          CreateBuffer;
        end;
    end
   else
    begin
      FImageWidth   := 0;
      FImageHeight  := 0;
      FBufferWidth  := 0;
      FBufferHeight := 0;

      DestroyBuffer;
    end;
end;

end.
