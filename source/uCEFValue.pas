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

unit uCEFValue;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefValueRef = class(TCefBaseRefCountedRef, ICefValue)
    protected
      function IsValid: Boolean;
      function IsOwned: Boolean;
      function IsReadOnly: Boolean;
      function IsSame(const that: ICefValue): Boolean;
      function IsEqual(const that: ICefValue): Boolean;
      function Copy: ICefValue;
      function GetType: TCefValueType;
      function GetBool: Boolean;
      function GetInt: Integer;
      function GetDouble: Double;
      function GetString: ustring;
      function GetBinary: ICefBinaryValue;
      function GetDictionary: ICefDictionaryValue;
      function GetList: ICefListValue;
      function SetNull: Boolean;
      function SetBool(value: Integer): Boolean;
      function SetInt(value: Integer): Boolean;
      function SetDouble(value: Double): Boolean;
      function SetString(const value: ustring): Boolean;
      function SetBinary(const value: ICefBinaryValue): Boolean;
      function SetDictionary(const value: ICefDictionaryValue): Boolean;
      function SetList(const value: ICefListValue): Boolean;

    public
      class function UnWrap(data: Pointer): ICefValue;
      class function New: ICefValue;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBinaryValue, uCEFListValue, uCEFDictionaryValue;

function TCefValueRef.Copy: ICefValue;
begin
  Result := UnWrap(PCefValue(FData).copy(FData));
end;

function TCefValueRef.GetBinary: ICefBinaryValue;
begin
  Result := TCefBinaryValueRef.UnWrap(PCefValue(FData).get_binary(FData));
end;

function TCefValueRef.GetBool: Boolean;
begin
  Result := PCefValue(FData).get_bool(FData) <> 0;
end;

function TCefValueRef.GetDictionary: ICefDictionaryValue;
begin
  Result := TCefDictionaryValueRef.UnWrap(PCefValue(FData).get_dictionary(FData));
end;

function TCefValueRef.GetDouble: Double;
begin
  Result := PCefValue(FData).get_double(FData);
end;

function TCefValueRef.GetInt: Integer;
begin
  Result := PCefValue(FData).get_int(FData);
end;

function TCefValueRef.GetList: ICefListValue;
begin
  Result := TCefListValueRef.UnWrap(PCefValue(FData).get_list(FData));
end;

function TCefValueRef.GetString: ustring;
begin
  Result := CefStringFreeAndGet(PCefValue(FData).get_string(FData));
end;

function TCefValueRef.GetType: TCefValueType;
begin
  Result := PCefValue(FData).get_type(FData);
end;

function TCefValueRef.IsEqual(const that: ICefValue): Boolean;
begin
  Result := PCefValue(FData).is_equal(FData, CefGetData(that)) <> 0;
end;

function TCefValueRef.IsOwned: Boolean;
begin
  Result := PCefValue(FData).is_owned(FData) <> 0;
end;

function TCefValueRef.IsReadOnly: Boolean;
begin
  Result := PCefValue(FData).is_read_only(FData) <> 0;
end;

function TCefValueRef.IsSame(const that: ICefValue): Boolean;
begin
  Result := PCefValue(FData).is_same(FData, CefGetData(that)) <> 0;
end;

function TCefValueRef.IsValid: Boolean;
begin
  Result := PCefValue(FData).is_valid(FData) <> 0;
end;

class function TCefValueRef.New: ICefValue;
begin
  Result := UnWrap(cef_value_create());
end;

function TCefValueRef.SetBinary(const value: ICefBinaryValue): Boolean;
begin
  Result := PCefValue(FData).set_binary(FData, CefGetData(value)) <> 0;
end;

function TCefValueRef.SetBool(value: Integer): Boolean;
begin
  Result := PCefValue(FData).set_bool(FData, value) <> 0;
end;

function TCefValueRef.SetDictionary(const value: ICefDictionaryValue): Boolean;
begin
  Result := PCefValue(FData).set_dictionary(FData, CefGetData(value)) <> 0;
end;

function TCefValueRef.SetDouble(value: Double): Boolean;
begin
  Result := PCefValue(FData).set_double(FData, value) <> 0;
end;

function TCefValueRef.SetInt(value: Integer): Boolean;
begin
  Result := PCefValue(FData).set_int(FData, value) <> 0;
end;

function TCefValueRef.SetList(const value: ICefListValue): Boolean;
begin
  Result := PCefValue(FData).set_list(FData, CefGetData(value)) <> 0;
end;

function TCefValueRef.SetNull: Boolean;
begin
  Result := PCefValue(FData).set_null(FData) <> 0;
end;

function TCefValueRef.SetString(const value: ustring): Boolean;
var
 s: TCefString;
begin
  s := CefString(value);
  Result := PCefValue(FData).set_string(FData, @s) <> 0;
end;

class function TCefValueRef.UnWrap(data: Pointer): ICefValue;
begin
  if data <> nil then
    Result := Create(data) as ICefValue else
    Result := nil;
end;

end.
