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

unit uCEFBinaryValue;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

interface

uses
  uCEFBase, uCEFInterfaces;

type
  TCefBinaryValueRef = class(TCefBaseRef, ICefBinaryValue)
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

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFTypes;

function TCefBinaryValueRef.Copy: ICefBinaryValue;
begin
  Result := UnWrap(PCefBinaryValue(FData).copy(PCefBinaryValue(FData)));
end;

function TCefBinaryValueRef.GetData(buffer: Pointer; bufferSize,
  dataOffset: NativeUInt): NativeUInt;
begin
  Result := PCefBinaryValue(FData).get_data(PCefBinaryValue(FData), buffer, bufferSize, dataOffset);
end;

function TCefBinaryValueRef.GetSize: NativeUInt;
begin
  Result := PCefBinaryValue(FData).get_size(PCefBinaryValue(FData));
end;

function TCefBinaryValueRef.IsEqual(const that: ICefBinaryValue): Boolean;
begin
  Result := PCefBinaryValue(FData).is_equal(PCefBinaryValue(FData), CefGetData(that)) <> 0;
end;

function TCefBinaryValueRef.IsOwned: Boolean;
begin
  Result := PCefBinaryValue(FData).is_owned(PCefBinaryValue(FData)) <> 0;
end;

function TCefBinaryValueRef.IsSame(const that: ICefBinaryValue): Boolean;
begin
  Result := PCefBinaryValue(FData).is_same(PCefBinaryValue(FData), CefGetData(that)) <> 0;
end;

function TCefBinaryValueRef.IsValid: Boolean;
begin
  Result := PCefBinaryValue(FData).is_valid(PCefBinaryValue(FData)) <> 0;
end;

class function TCefBinaryValueRef.New(const data: Pointer; dataSize: NativeUInt): ICefBinaryValue;
begin
  Result := UnWrap(cef_binary_value_create(data, dataSize));
end;

class function TCefBinaryValueRef.UnWrap(data: Pointer): ICefBinaryValue;
begin
  if data <> nil then
    Result := Create(data) as ICefBinaryValue else
    Result := nil;
end;

end.
