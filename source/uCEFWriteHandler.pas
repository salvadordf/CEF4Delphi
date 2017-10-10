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
  {$IFDEF DELPHI16_UP}
  WinApi.Windows,
  {$ELSE}
  Windows,
  {$ENDIF}
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

  TCefBytesWriteHandler = class(TCefWriteHandlerOwn)
    protected
      FCriticalSection : TRTLCriticalSection;

      FGrow     : NativeUInt;
      FData     : Pointer;
      FDataSize : int64;
      FOffset   : int64;

      function Grow(size : NativeUInt) : NativeUInt;

    public
      constructor Create(aGrow : NativeUInt); reintroduce;
      destructor  Destroy; override;

      function Write(const ptr: Pointer; size, n: NativeUInt): NativeUInt; override;
      function Seek(offset: Int64; whence: Integer): Integer; override;
      function Tell: Int64; override;
      function Flush: Integer; override;
      function MayBlock: Boolean; override;

      function GetData : pointer;
      function GetDataSize : int64;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;


// *******************************************
// *********** TCefWriteHandlerOwn ***********
// *******************************************

function cef_write_handler_write(self: PCefWriteHandler; const ptr: Pointer; size, n: NativeUInt): NativeUInt; stdcall;
begin
  with TCefWriteHandlerOwn(CefGetObject(self)) do
    Result:= Write(ptr, size, n);
end;

function cef_write_handler_seek(self: PCefWriteHandler; offset: Int64; whence: Integer): Integer; stdcall;
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
      write     := cef_write_handler_write;
      seek      := cef_write_handler_seek;
      tell      := cef_write_handler_tell;
      flush     := cef_write_handler_flush;
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

function TCefWriteHandlerOwn.Write(const ptr: Pointer; size, n: NativeUInt): NativeUInt;
begin
  Result := 0;
end;


// *******************************************
// ********** TCefBytesWriteHandler **********
// *******************************************

constructor TCefBytesWriteHandler.Create(aGrow : NativeUInt);
begin
  inherited Create;

  InitializeCriticalSection(FCriticalSection);

  FGrow     := aGrow;
  FDataSize := aGrow;
  FOffset   := 0;

  GetMem(FData, aGrow);
end;

destructor TCefBytesWriteHandler.Destroy;
begin
  if (FData <> nil) then FreeMem(FData);

  DeleteCriticalSection(FCriticalSection);

  FCriticalSection.DebugInfo      := nil;
  FCriticalSection.LockCount      := 0;
  FCriticalSection.RecursionCount := 0;
  FCriticalSection.OwningThread   := 0;
  FCriticalSection.LockSemaphore  := 0;
  FCriticalSection.Reserved       := 0;

  inherited Destroy;
end;

function TCefBytesWriteHandler.Write(const ptr: Pointer; size, n: NativeUInt): NativeUInt;
var
  TempPointer : pointer;
begin
  EnterCriticalSection(FCriticalSection);

  if ((FOffset + (size * n)) >= FDataSize) and (Grow(size * n) = 0) then
    Result := 0
   else
    begin
      TempPointer := Pointer(cardinal(FData) + FOffset);

      CopyMemory(TempPointer, ptr, size * n);

      FOffset := FOffset + (size * n);
      Result  := n;
    end;

  LeaveCriticalSection(FCriticalSection);
end;

function TCefBytesWriteHandler.Seek(offset: Int64; whence: Integer): Integer;
const
  SEEK_SET = 0;
  SEEK_CUR = 1;
  SEEK_END = 2;
var
  TempAbsOffset : int64;
begin
  EnterCriticalSection(FCriticalSection);

  Result := -1;

  case whence of
    SEEK_CUR :
      if not((FOffset + offset > FDataSize) or (FOffset + offset < 0)) then
        begin
          FOffset := FOffset + offset;
          Result  := 0;
        end;

    SEEK_END:
      begin
        TempAbsOffset := abs(offset);

        if not(TempAbsOffset > FDataSize) then
          begin
            FOffset := FDataSize - TempAbsOffset;
            Result  := 0;
          end;
      end;

    SEEK_SET:
      if not((offset > FDataSize) or (offset < 0)) then
        begin
          FOffset := offset;
          Result  := 0;
        end;
  end;

  LeaveCriticalSection(FCriticalSection);
end;

function TCefBytesWriteHandler.Tell: Int64;
begin
  EnterCriticalSection(FCriticalSection);

  Result := FOffset;

  LeaveCriticalSection(FCriticalSection);
end;

function TCefBytesWriteHandler.Flush: Integer;
begin
  Result := 0;
end;

function TCefBytesWriteHandler.MayBlock: Boolean;
begin
  Result := False;
end;

function TCefBytesWriteHandler.GetData : pointer;
begin
  Result := FData;
end;

function TCefBytesWriteHandler.GetDataSize : int64;
begin
  Result := FDataSize;
end;

function TCefBytesWriteHandler.Grow(size : NativeUInt) : NativeUInt;
var
  s : NativeUInt;
begin
  EnterCriticalSection(FCriticalSection);

  if (size > FGrow) then
    s := size
   else
    s := FGrow;

  ReallocMem(FData, FDataSize + s);

  FDataSize := FDataSize + s;
  Result    := FDataSize;

  LeaveCriticalSection(FCriticalSection);
end;

end.
