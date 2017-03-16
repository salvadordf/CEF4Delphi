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

unit uCEFPostDataElement;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefPostDataElementRef = class(TCefBaseRefCountedRef, ICefPostDataElement)
    protected
      function  IsReadOnly: Boolean;
      procedure SetToEmpty;
      procedure SetToFile(const fileName: ustring);
      procedure SetToBytes(size: NativeUInt; bytes: Pointer);
      function  GetType: TCefPostDataElementType;
      function  GetFile: ustring;
      function  GetBytesCount: NativeUInt;
      function  GetBytes(size: NativeUInt; bytes: Pointer): NativeUInt;

    public
      class function UnWrap(data: Pointer): ICefPostDataElement;
      class function New: ICefPostDataElement;
  end;

  TCefPostDataElementOwn = class(TCefBaseRefCountedOwn, ICefPostDataElement)
    protected
      FDataType: TCefPostDataElementType;
      FValueByte: Pointer;
      FValueStr: TCefString;
      FSize: NativeUInt;
      FReadOnly: Boolean;

      procedure Clear;
      function  IsReadOnly: Boolean; virtual;
      procedure SetToEmpty; virtual;
      procedure SetToFile(const fileName: ustring); virtual;
      procedure SetToBytes(size: NativeUInt; bytes: Pointer); virtual;
      function  GetType: TCefPostDataElementType; virtual;
      function  GetFile: ustring; virtual;
      function  GetBytesCount: NativeUInt; virtual;
      function  GetBytes(size: NativeUInt; bytes: Pointer): NativeUInt; virtual;

    public
      constructor Create(readonly: Boolean); virtual;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;


function cef_post_data_element_is_read_only(self: PCefPostDataElement): Integer; stdcall;
begin
  with TCefPostDataElementOwn(CefGetObject(self)) do
    Result := Ord(IsReadOnly)
end;

procedure cef_post_data_element_set_to_empty(self: PCefPostDataElement); stdcall;
begin
  with TCefPostDataElementOwn(CefGetObject(self)) do
    SetToEmpty;
end;

procedure cef_post_data_element_set_to_file(self: PCefPostDataElement; const fileName: PCefString); stdcall;
begin
  with TCefPostDataElementOwn(CefGetObject(self)) do
    SetToFile(CefString(fileName));
end;

procedure cef_post_data_element_set_to_bytes(self: PCefPostDataElement; size: NativeUInt; const bytes: Pointer); stdcall;
begin
  with TCefPostDataElementOwn(CefGetObject(self)) do
    SetToBytes(size, bytes);
end;

function cef_post_data_element_get_type(self: PCefPostDataElement): TCefPostDataElementType; stdcall;
begin
  with TCefPostDataElementOwn(CefGetObject(self)) do
    Result := GetType;
end;

function cef_post_data_element_get_file(self: PCefPostDataElement): PCefStringUserFree; stdcall;
begin
  with TCefPostDataElementOwn(CefGetObject(self)) do
    Result := CefUserFreeString(GetFile);
end;

function cef_post_data_element_get_bytes_count(self: PCefPostDataElement): NativeUInt; stdcall;
begin
  with TCefPostDataElementOwn(CefGetObject(self)) do
    Result := GetBytesCount;
end;

function cef_post_data_element_get_bytes(self: PCefPostDataElement; size: NativeUInt; bytes: Pointer): NativeUInt; stdcall;
begin
  with TCefPostDataElementOwn(CefGetObject(self)) do
    Result := GetBytes(size, bytes)
end;

function TCefPostDataElementRef.IsReadOnly: Boolean;
begin
  Result := PCefPostDataElement(FData)^.is_read_only(PCefPostDataElement(FData)) <> 0;
end;

function TCefPostDataElementRef.GetBytes(size: NativeUInt;
  bytes: Pointer): NativeUInt;
begin
  Result := PCefPostDataElement(FData)^.get_bytes(PCefPostDataElement(FData), size, bytes);
end;

function TCefPostDataElementRef.GetBytesCount: NativeUInt;
begin
  Result := PCefPostDataElement(FData)^.get_bytes_count(PCefPostDataElement(FData));
end;

function TCefPostDataElementRef.GetFile: ustring;
begin
  Result := CefStringFreeAndGet(PCefPostDataElement(FData)^.get_file(PCefPostDataElement(FData)));
end;

function TCefPostDataElementRef.GetType: TCefPostDataElementType;
begin
  Result := PCefPostDataElement(FData)^.get_type(PCefPostDataElement(FData));
end;

class function TCefPostDataElementRef.New: ICefPostDataElement;
begin
  Result := UnWrap(cef_post_data_element_create);
end;

procedure TCefPostDataElementRef.SetToBytes(size: NativeUInt; bytes: Pointer);
begin
  PCefPostDataElement(FData)^.set_to_bytes(PCefPostDataElement(FData), size, bytes);
end;

procedure TCefPostDataElementRef.SetToEmpty;
begin
  PCefPostDataElement(FData)^.set_to_empty(PCefPostDataElement(FData));
end;

procedure TCefPostDataElementRef.SetToFile(const fileName: ustring);
var
  f: TCefString;
begin
  f := CefString(fileName);
  PCefPostDataElement(FData)^.set_to_file(PCefPostDataElement(FData), @f);
end;

class function TCefPostDataElementRef.UnWrap(data: Pointer): ICefPostDataElement;
begin
  if data <> nil then
    Result := Create(data) as ICefPostDataElement else
    Result := nil;
end;

// TCefPostDataElementOwn

procedure TCefPostDataElementOwn.Clear;
begin
  case FDataType of
    PDE_TYPE_BYTES:
      if (FValueByte <> nil) then
      begin
        FreeMem(FValueByte);
        FValueByte := nil;
      end;
    PDE_TYPE_FILE:
      CefStringFree(@FValueStr)
  end;
  FDataType := PDE_TYPE_EMPTY;
  FSize := 0;
end;

constructor TCefPostDataElementOwn.Create(readonly: Boolean);
begin
  inherited CreateData(SizeOf(TCefPostDataElement));
  FReadOnly := readonly;
  FDataType := PDE_TYPE_EMPTY;
  FValueByte := nil;
  FillChar(FValueStr, SizeOf(FValueStr), 0);
  FSize := 0;
  with PCefPostDataElement(FData)^ do
  begin
    is_read_only := cef_post_data_element_is_read_only;
    set_to_empty := cef_post_data_element_set_to_empty;
    set_to_file := cef_post_data_element_set_to_file;
    set_to_bytes := cef_post_data_element_set_to_bytes;
    get_type := cef_post_data_element_get_type;
    get_file := cef_post_data_element_get_file;
    get_bytes_count := cef_post_data_element_get_bytes_count;
    get_bytes := cef_post_data_element_get_bytes;
  end;
end;

function TCefPostDataElementOwn.GetBytes(size: NativeUInt;
  bytes: Pointer): NativeUInt;
begin
  if (FDataType = PDE_TYPE_BYTES) and (FValueByte <> nil) then
  begin
    if size > FSize then
      Result := FSize else
      Result := size;
    Move(FValueByte^, bytes^, Result);
  end else
    Result := 0;
end;

function TCefPostDataElementOwn.GetBytesCount: NativeUInt;
begin
  if (FDataType = PDE_TYPE_BYTES) then
    Result := FSize else
    Result := 0;
end;

function TCefPostDataElementOwn.GetFile: ustring;
begin
  if (FDataType = PDE_TYPE_FILE) then
    Result := CefString(@FValueStr) else
    Result := '';
end;

function TCefPostDataElementOwn.GetType: TCefPostDataElementType;
begin
  Result := FDataType;
end;

function TCefPostDataElementOwn.IsReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

procedure TCefPostDataElementOwn.SetToBytes(size: NativeUInt; bytes: Pointer);
begin
  Clear;
  if (size > 0) and (bytes <> nil) then
  begin
    GetMem(FValueByte, size);
    Move(bytes^, FValueByte, size);
    FSize := size;
  end else
  begin
    FValueByte := nil;
    FSize := 0;
  end;
  FDataType := PDE_TYPE_BYTES;
end;

procedure TCefPostDataElementOwn.SetToEmpty;
begin
  Clear;
end;

procedure TCefPostDataElementOwn.SetToFile(const fileName: ustring);
begin
  Clear;
  FSize := 0;
  FValueStr := CefStringAlloc(fileName);
  FDataType := PDE_TYPE_FILE;
end;

end.
