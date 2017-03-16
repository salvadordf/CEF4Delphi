// ************************************************************************
// ***************************** CEF4Delphi *******************************
// ************************************************************************
//
// CEF4Delphi is based on DCEF3 which uses CEF3 to embed a chromium-based
// browser in Delphi applications.
//
// The original license of DCEF3 still applies to CEF4Delphi.
//
// For more information about CEF4Delphi visit :
//         https://www.briskbard.com/index.php?lang=en&pageid=cef
//
//        Copyright © 2017 Salvador Díaz Fau. All rights reserved.
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

unit uCEFWriteHandler;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefWriteHandlerOwn = class(TCefBaseRefCountedOwn, ICefWriteHandler)
  protected
    function Write(const ptr: Pointer; size, n: NativeUInt): NativeUInt; virtual;
    function Seek(offset: Int64; whence: Integer): Integer; virtual;
    function Tell: Int64; virtual;
    function Flush: Integer; virtual;
    function MayBlock: Boolean; virtual;
  public
    constructor Create; virtual;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;


function cef_write_handler_write(self: PCefWriteHandler; const ptr: Pointer;
  size, n: NativeUInt): NativeUInt; stdcall;
begin
  with TCefWriteHandlerOwn(CefGetObject(self)) do
    Result:= Write(ptr, size, n);
end;

function cef_write_handler_seek(self: PCefWriteHandler; offset: Int64;
  whence: Integer): Integer; stdcall;
begin
  with TCefWriteHandlerOwn(CefGetObject(self)) do
    Result := Seek(offset, whence);
end;

function cef_write_handler_tell(self: PCefWriteHandler): Int64; stdcall;
begin
  with TCefWriteHandlerOwn(CefGetObject(self)) do
    Result := Tell();
end;

function cef_write_handler_flush(self: PCefWriteHandler): Integer; stdcall;
begin
  with TCefWriteHandlerOwn(CefGetObject(self)) do
    Result := Flush();
end;

function cef_write_handler_may_block(self: PCefWriteHandler): Integer; stdcall;
begin
  with TCefWriteHandlerOwn(CefGetObject(self)) do
    Result := Ord(MayBlock);
end;

constructor TCefWriteHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefWriteHandler));
  with PCefWriteHandler(FData)^ do
  begin
    write := cef_write_handler_write;
    seek := cef_write_handler_seek;
    tell := cef_write_handler_tell;
    flush := cef_write_handler_flush;
    may_block := cef_write_handler_may_block;
  end;
end;

function TCefWriteHandlerOwn.Flush: Integer;
begin
  Result := 0;
end;

function TCefWriteHandlerOwn.MayBlock: Boolean;
begin
  Result := False;
end;

function TCefWriteHandlerOwn.Seek(offset: Int64; whence: Integer): Integer;
begin
  Result := 0;
end;

function TCefWriteHandlerOwn.Tell: Int64;
begin
  Result := 0;
end;

function TCefWriteHandlerOwn.Write(const ptr: Pointer; size,
  n: NativeUInt): NativeUInt;
begin
  Result := 0;
end;

end.
