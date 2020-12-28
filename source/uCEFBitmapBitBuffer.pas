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
//        Copyright © 2020 Salvador Diaz Fau. All rights reserved.
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

unit uCEFBitmapBitBuffer;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes, System.SysUtils;
  {$ELSE}
  Classes, SysUtils;
  {$ENDIF}

type
  TCEFBitmapBitBuffer = class
    protected
      FBuffer : pointer;
      FWidth  : integer;
      FHeight : integer;

      function  GetScanlineSize : integer;
      function  GetBufferLength : integer;
      function  GetEmpty : boolean;
      function  GetScanline(y : integer) : PByte;

      procedure CreateBuffer;
      procedure DestroyBuffer;

    public
      constructor Create(aWidth, aHeight : integer);
      destructor  Destroy; override;

      property    Width                  : integer   read FWidth;
      property    Height                 : integer   read FHeight;
      property    BufferLength           : integer   read GetBufferLength;
      property    Empty                  : boolean   read GetEmpty;
      property    Scanline[y : integer]  : PByte     read GetScanline;
      property    ScanlineSize           : integer   read GetScanlineSize;
      property    BufferBits             : pointer   read FBuffer;
  end;

implementation

const
  RGBQUAD_SIZE = 4;

constructor TCEFBitmapBitBuffer.Create(aWidth, aHeight : integer);
begin
  inherited Create;

  if (aWidth > 0) and (aHeight > 0) then
    begin
      FWidth  := aWidth;
      FHeight := aHeight;

      CreateBuffer;
    end
   else
    begin
      FWidth  := 0;
      FHeight := 0;
      FBuffer := nil;
    end;
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
  Result := FWidth * RGBQUAD_SIZE;
end;

function TCEFBitmapBitBuffer.GetBufferLength : integer;
begin
  Result := FHeight * ScanlineSize;
end;

function TCEFBitmapBitBuffer.GetEmpty : boolean;
begin
  Result := (BufferLength = 0) or (FBuffer = nil);
end;

function TCEFBitmapBitBuffer.GetScanline(y : integer) : PByte;
begin
  if (FBuffer = nil) or (y >= FHeight) then
    Result := nil
   else
    begin
      Result := PByte(FBuffer);
      if (y > 0) then inc(Result, y * ScanlineSize);
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

end.
