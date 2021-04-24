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

unit uCEFBinaryValue;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefBinaryValueRef = class(TCefBaseRefCountedRef, ICefBinaryValue)
    protected
      function IsValid: Boolean;
      function IsOwned: Boolean;
      function IsSame(const that: ICefBinaryValue): Boolean;
      function IsEqual(const that: ICefBinaryValue): Boolean;
      function Copy: ICefBinaryValue;
      function GetSize: NativeUInt;
      function GetData(buffer: Pointer; bufferSize, dataOffset: NativeUInt): NativeUInt;

    public
      class function UnWrap(data: Pointer): ICefBinaryValue;
      class function New(const data: Pointer; dataSize: NativeUInt): ICefBinaryValue;
  end;

  TCefBinaryValueOwn = class(TCefBaseRefCountedOwn, ICefBinaryValue)
    protected
      function IsValid: Boolean;
      function IsOwned: Boolean;
      function IsSame(const that: ICefBinaryValue): Boolean;
      function IsEqual(const that: ICefBinaryValue): Boolean;
      function Copy: ICefBinaryValue;
      function GetSize: NativeUInt;
      function GetData(buffer: Pointer; bufferSize, dataOffset: NativeUInt): NativeUInt;

    public
      constructor Create;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;


// **********************************************
// **********  TCefBinaryValueRef  **************
// **********************************************


function TCefBinaryValueRef.Copy: ICefBinaryValue;
begin
  Result := UnWrap(PCefBinaryValue(FData)^.copy(PCefBinaryValue(FData)));
end;

function TCefBinaryValueRef.GetData(buffer: Pointer; bufferSize, dataOffset: NativeUInt): NativeUInt;
begin
  Result := PCefBinaryValue(FData)^.get_data(PCefBinaryValue(FData), buffer, bufferSize, dataOffset);
end;

function TCefBinaryValueRef.GetSize: NativeUInt;
begin
  Result := PCefBinaryValue(FData)^.get_size(PCefBinaryValue(FData));
end;

function TCefBinaryValueRef.IsEqual(const that: ICefBinaryValue): Boolean;
begin
  Result := PCefBinaryValue(FData)^.is_equal(PCefBinaryValue(FData), CefGetData(that)) <> 0;
end;

function TCefBinaryValueRef.IsOwned: Boolean;
begin
  Result := PCefBinaryValue(FData)^.is_owned(PCefBinaryValue(FData)) <> 0;
end;

function TCefBinaryValueRef.IsSame(const that: ICefBinaryValue): Boolean;
begin
  Result := PCefBinaryValue(FData)^.is_same(PCefBinaryValue(FData), CefGetData(that)) <> 0;
end;

function TCefBinaryValueRef.IsValid: Boolean;
begin
  Result := PCefBinaryValue(FData)^.is_valid(PCefBinaryValue(FData)) <> 0;
end;

class function TCefBinaryValueRef.New(const data: Pointer; dataSize: NativeUInt): ICefBinaryValue;
begin
  Result := UnWrap(cef_binary_value_create(data, dataSize));
end;

class function TCefBinaryValueRef.UnWrap(data: Pointer): ICefBinaryValue;
begin
  if (data <> nil) then
    Result := Create(data) as ICefBinaryValue
   else
    Result := nil;
end;


// **********************************************
// **********  TCefBinaryValueOwn  **************
// **********************************************

function cef_binary_value_is_valid(self: PCefBinaryValue): Integer; stdcall;
var
  TempObject  : TObject;
begin
  Result      := Ord(False);
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefBinaryValueOwn) then
    Result := Ord(TCefBinaryValueOwn(TempObject).IsValid);
end;

function cef_binary_value_is_owned(self: PCefBinaryValue): Integer; stdcall;
var
  TempObject  : TObject;
begin
  Result      := Ord(False);
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefBinaryValueOwn) then
    Result := Ord(TCefBinaryValueOwn(TempObject).IsOwned);
end;

function cef_binary_value_is_same(self, that: PCefBinaryValue):Integer; stdcall;
var
  TempObject  : TObject;
begin
  Result      := Ord(False);
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefBinaryValueOwn) then
    Result := Ord(TCefBinaryValueOwn(TempObject).IsSame(TCefBinaryValueRef.UnWrap(that)));
end;

function cef_binary_value_is_equal(self, that: PCefBinaryValue): Integer; stdcall;
var
  TempObject  : TObject;
begin
  Result      := Ord(False);
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefBinaryValueOwn) then
    Result := Ord(TCefBinaryValueOwn(TempObject).IsEqual(TCefBinaryValueRef.UnWrap(that)));
end;

function cef_binary_value_copy(self: PCefBinaryValue): PCefBinaryValue; stdcall;
var
  TempObject  : TObject;
begin
  Result      := nil;
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefBinaryValueOwn) then
    Result := CefGetData(TCefBinaryValueOwn(TempObject).Copy);
end;

function cef_binary_value_get_size(self: PCefBinaryValue): NativeUInt; stdcall;
var
  TempObject  : TObject;
begin
  Result      := 0;
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefBinaryValueOwn) then
    Result := TCefBinaryValueOwn(TempObject).GetSize;
end;

function cef_binary_value_get_data(self: PCefBinaryValue; buffer: Pointer; buffer_size, data_offset: NativeUInt): NativeUInt; stdcall;
var
  TempObject  : TObject;
begin
  Result      := 0;
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefBinaryValueOwn) then
    Result := TCefBinaryValueOwn(TempObject).GetData(buffer, buffer_size, data_offset);
end;

constructor TCefBinaryValueOwn.Create;
begin
  inherited CreateData(SizeOf(TCefBinaryValue));

  with PCefBinaryValue(FData)^ do
    begin
      is_valid := {$IFDEF FPC}@{$ENDIF}cef_binary_value_is_valid;
      is_owned := {$IFDEF FPC}@{$ENDIF}cef_binary_value_is_owned;
      is_same  := {$IFDEF FPC}@{$ENDIF}cef_binary_value_is_same;
      is_equal := {$IFDEF FPC}@{$ENDIF}cef_binary_value_is_equal;
      copy     := {$IFDEF FPC}@{$ENDIF}cef_binary_value_copy;
      get_size := {$IFDEF FPC}@{$ENDIF}cef_binary_value_get_size;
      get_data := {$IFDEF FPC}@{$ENDIF}cef_binary_value_get_data;
    end;
end;

function TCefBinaryValueOwn.IsValid: Boolean;
begin
  Result := False;
end;

function TCefBinaryValueOwn.IsOwned: Boolean;
begin
  Result := False;
end;

function TCefBinaryValueOwn.IsSame(const that: ICefBinaryValue): Boolean;
begin
  Result := False;
end;

function TCefBinaryValueOwn.IsEqual(const that: ICefBinaryValue): Boolean;
begin
  Result := False;
end;

function TCefBinaryValueOwn.Copy: ICefBinaryValue;
begin
  Result := nil;
end;

function TCefBinaryValueOwn.GetSize: NativeUInt;
begin
  Result := 0;
end;

function TCefBinaryValueOwn.GetData(buffer: Pointer; bufferSize, dataOffset: NativeUInt): NativeUInt;
begin
  Result := 0;
end;


end.
