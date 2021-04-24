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

unit uCEFWriteHandler;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  {$IFDEF MSWINDOWS}
    {$IFDEF DELPHI16_UP}
    WinApi.Windows,
    {$ELSE}
    Windows,
    {$ENDIF}
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

      FGrow       : NativeUInt;
      FBuffer     : Pointer;
      FBufferSize : int64;
      FOffset     : int64;

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
  {$IFDEF DELPHI16_UP}
  System.Math,
  {$ELSE}
  Math,
  {$ENDIF}
  uCEFMiscFunctions, uCEFLibFunctions;


// *******************************************
// *********** TCefWriteHandlerOwn ***********
// *******************************************

function cef_write_handler_write(      self : PCefWriteHandler;
                                 const ptr  : Pointer;
                                       size : NativeUInt;
                                       n    : NativeUInt): NativeUInt; stdcall;
var
  TempObject : TObject;
begin
  Result     := 0;
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefWriteHandlerOwn) then
    Result:= TCefWriteHandlerOwn(TempObject).Write(ptr,
                                                   size,
                                                   n);
end;

function cef_write_handler_seek(self   : PCefWriteHandler;
                                offset : Int64;
                                whence : Integer): Integer; stdcall;
var
  TempObject : TObject;
begin
  Result     := 0;
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefWriteHandlerOwn) then
    Result:= TCefWriteHandlerOwn(TempObject).Seek(offset,
                                                  whence);
end;

function cef_write_handler_tell(self: PCefWriteHandler): Int64; stdcall;
var
  TempObject : TObject;
begin
  Result     := 0;
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefWriteHandlerOwn) then
    Result:= TCefWriteHandlerOwn(TempObject).Tell;
end;

function cef_write_handler_flush(self: PCefWriteHandler): Integer; stdcall;
var
  TempObject : TObject;
begin
  Result     := 0;
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefWriteHandlerOwn) then
    Result:= TCefWriteHandlerOwn(TempObject).Flush;
end;

function cef_write_handler_may_block(self: PCefWriteHandler): Integer; stdcall;
var
  TempObject : TObject;
begin
  Result     := Ord(False);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefWriteHandlerOwn) then
    Result := Ord(TCefWriteHandlerOwn(TempObject).MayBlock);
end;

constructor TCefWriteHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefWriteHandler));

  with PCefWriteHandler(FData)^ do
    begin
      write     := {$IFDEF FPC}@{$ENDIF}cef_write_handler_write;
      seek      := {$IFDEF FPC}@{$ENDIF}cef_write_handler_seek;
      tell      := {$IFDEF FPC}@{$ENDIF}cef_write_handler_tell;
      flush     := {$IFDEF FPC}@{$ENDIF}cef_write_handler_flush;
      may_block := {$IFDEF FPC}@{$ENDIF}cef_write_handler_may_block;
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
  {$IFDEF MSWINDOWS}
  InitializeCriticalSection(FCriticalSection);
  {$ELSE}
  InitCriticalSection(FCriticalSection);
  {$ENDIF}
  FGrow       := aGrow;
  FBufferSize := aGrow;
  FOffset     := 0;

  GetMem(FBuffer, aGrow);
end;

destructor TCefBytesWriteHandler.Destroy;
begin
  if (FBuffer <> nil) then FreeMem(FBuffer);

  {$IFDEF MSWINDOWS}
    DeleteCriticalSection(FCriticalSection);
    FillChar(FCriticalSection, SizeOf(FCriticalSection), 0);
  {$ELSE}
    DoneCriticalSection(FCriticalSection);
  {$ENDIF}

  inherited Destroy;
end;

function TCefBytesWriteHandler.Write(const ptr: Pointer; size, n: NativeUInt): NativeUInt;
var
  TempPointer : pointer;
  TempSize    : int64;
begin
  EnterCriticalSection(FCriticalSection);
  try
    TempSize := size * n;

    if ((FOffset + TempSize) >= FBufferSize) and (Grow(TempSize) = 0) then
      Result := 0
     else
      begin
        TempPointer := Pointer(cardinal(FBuffer) + FOffset);

        Move(ptr^, TempPointer^, TempSize);

        FOffset := FOffset + TempSize;
        Result  := n;
      end;

  finally
    LeaveCriticalSection(FCriticalSection);
  end;
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
      if not((FOffset + offset > FBufferSize) or (FOffset + offset < 0)) then
        begin
          FOffset := FOffset + offset;
          Result  := 0;
        end;

    SEEK_END:
      begin
        TempAbsOffset := abs(offset);

        if not(TempAbsOffset > FBufferSize) then
          begin
            FOffset := FBufferSize - TempAbsOffset;
            Result  := 0;
          end;
      end;

    SEEK_SET:
      if not((offset > FBufferSize) or (offset < 0)) then
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
  Result := FBuffer;
end;

function TCefBytesWriteHandler.GetDataSize : int64;
begin
  Result := FBufferSize;
end;

function TCefBytesWriteHandler.Grow(size : NativeUInt) : NativeUInt;
var
  TempTotal : int64;
begin
  EnterCriticalSection(FCriticalSection);
  try

    if (size < FGrow) then
      TempTotal := FGrow
     else
      TempTotal := size;

    inc(TempTotal, FBufferSize);

    ReallocMem(FBuffer, TempTotal);

    FBufferSize := TempTotal;
    Result      := FBufferSize;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

end.
