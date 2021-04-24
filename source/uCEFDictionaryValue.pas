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

unit uCEFDictionaryValue;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes, System.SysUtils,
  {$ELSE}
  Classes, SysUtils,
  {$ENDIF}
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefDictionaryValueRef = class(TCefBaseRefCountedRef, ICefDictionaryValue)
    protected
      function IsValid: Boolean;
      function isOwned: Boolean;
      function IsReadOnly: Boolean;
      function IsSame(const that: ICefDictionaryValue): Boolean;
      function IsEqual(const that: ICefDictionaryValue): Boolean;
      function Copy(excludeEmptyChildren: Boolean): ICefDictionaryValue;
      function GetSize: NativeUInt;
      function Clear: Boolean;
      function HasKey(const key: ustring): Boolean;
      function GetKeys(const keys: TStrings): Boolean;
      function Remove(const key: ustring): Boolean;
      function GetType(const key: ustring): TCefValueType;
      function GetValue(const key: ustring): ICefValue;
      function GetBool(const key: ustring): Boolean;
      function GetInt(const key: ustring): Integer;
      function GetDouble(const key: ustring): Double;
      function GetString(const key: ustring): ustring;
      function GetBinary(const key: ustring): ICefBinaryValue;
      function GetDictionary(const key: ustring): ICefDictionaryValue;
      function GetList(const key: ustring): ICefListValue;
      function SetValue(const key: ustring; const value: ICefValue): Boolean;
      function SetNull(const key: ustring): Boolean;
      function SetBool(const key: ustring; value: Boolean): Boolean;
      function SetInt(const key: ustring; value: Integer): Boolean;
      function SetDouble(const key: ustring; value: Double): Boolean;
      function SetString(const key, value: ustring): Boolean;
      function SetBinary(const key: ustring; const value: ICefBinaryValue): Boolean;
      function SetDictionary(const key: ustring; const value: ICefDictionaryValue): Boolean;
      function SetList(const key: ustring; const value: ICefListValue): Boolean;

    public
      class function UnWrap(data: Pointer): ICefDictionaryValue;
      class function New: ICefDictionaryValue;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBinaryValue, uCEFListValue, uCEFValue, uCEFStringList;

function TCefDictionaryValueRef.Clear: Boolean;
begin
  Result := PCefDictionaryValue(FData)^.clear(PCefDictionaryValue(FData)) <> 0;
end;

function TCefDictionaryValueRef.Copy(excludeEmptyChildren: Boolean): ICefDictionaryValue;
begin
  Result := UnWrap(PCefDictionaryValue(FData)^.copy(PCefDictionaryValue(FData), Ord(excludeEmptyChildren)));
end;

function TCefDictionaryValueRef.GetBinary(const key: ustring): ICefBinaryValue;
var
  TempKey : TCefString;
begin
  TempKey := CefString(key);
  Result  := TCefBinaryValueRef.UnWrap(PCefDictionaryValue(FData)^.get_binary(PCefDictionaryValue(FData), @TempKey));
end;

function TCefDictionaryValueRef.GetBool(const key: ustring): Boolean;
var
  TempKey : TCefString;
begin
  TempKey := CefString(key);
  Result  := PCefDictionaryValue(FData)^.get_bool(PCefDictionaryValue(FData), @TempKey) <> 0;
end;

function TCefDictionaryValueRef.GetDictionary(const key: ustring): ICefDictionaryValue;
var
  TempKey : TCefString;
begin
  TempKey := CefString(key);
  Result  := UnWrap(PCefDictionaryValue(FData)^.get_dictionary(PCefDictionaryValue(FData), @TempKey));
end;

function TCefDictionaryValueRef.GetDouble(const key: ustring): Double;
var
  TempKey : TCefString;
begin
  TempKey := CefString(key);
  Result  := PCefDictionaryValue(FData)^.get_double(PCefDictionaryValue(FData), @TempKey);
end;

function TCefDictionaryValueRef.GetInt(const key: ustring): Integer;
var
  TempKey : TCefString;
begin
  TempKey := CefString(key);
  Result  := PCefDictionaryValue(FData)^.get_int(PCefDictionaryValue(FData), @TempKey);
end;

function TCefDictionaryValueRef.GetKeys(const keys : TStrings): Boolean;
var
  TempSL : ICefStringList;
begin
  Result := False;

  if (keys <> nil) then
    begin
      TempSL := TCefStringListOwn.Create;

      if (PCefDictionaryValue(FData)^.get_keys(PCefDictionaryValue(FData), TempSL.Handle) <> 0) then
        begin
          TempSL.CopyToStrings(keys);
          Result := True;
        end;
    end;
end;

function TCefDictionaryValueRef.GetList(const key: ustring): ICefListValue;
var
  TempKey : TCefString;
begin
  TempKey := CefString(key);
  Result  := TCefListValueRef.UnWrap(PCefDictionaryValue(FData)^.get_list(PCefDictionaryValue(FData), @TempKey));
end;

function TCefDictionaryValueRef.GetSize: NativeUInt;
begin
  Result := PCefDictionaryValue(FData)^.get_size(PCefDictionaryValue(FData));
end;

function TCefDictionaryValueRef.GetString(const key: ustring): ustring;
var
  TempKey : TCefString;
begin
  TempKey := CefString(key);
  Result  := CefStringFreeAndGet(PCefDictionaryValue(FData)^.get_string(PCefDictionaryValue(FData), @TempKey));
end;

function TCefDictionaryValueRef.GetType(const key: ustring): TCefValueType;
var
  TempKey : TCefString;
begin
  TempKey := CefString(key);
  Result  := PCefDictionaryValue(FData)^.get_type(PCefDictionaryValue(FData), @TempKey);
end;

function TCefDictionaryValueRef.GetValue(const key: ustring): ICefValue;
var
  TempKey : TCefString;
begin
  TempKey := CefString(key);
  Result  := TCefValueRef.UnWrap(PCefDictionaryValue(FData)^.get_value(PCefDictionaryValue(FData), @TempKey));
end;

function TCefDictionaryValueRef.HasKey(const key: ustring): Boolean;
var
  TempKey : TCefString;
begin
  TempKey := CefString(key);
  Result  := PCefDictionaryValue(FData)^.has_key(PCefDictionaryValue(FData), @TempKey) <> 0;
end;

function TCefDictionaryValueRef.IsEqual(const that: ICefDictionaryValue): Boolean;
begin
  Result := PCefDictionaryValue(FData)^.is_equal(PCefDictionaryValue(FData), CefGetData(that)) <> 0;
end;

function TCefDictionaryValueRef.isOwned: Boolean;
begin
  Result := PCefDictionaryValue(FData)^.is_owned(PCefDictionaryValue(FData)) <> 0;
end;

function TCefDictionaryValueRef.IsReadOnly: Boolean;
begin
  Result := PCefDictionaryValue(FData)^.is_read_only(PCefDictionaryValue(FData)) <> 0;
end;

function TCefDictionaryValueRef.IsSame(const that: ICefDictionaryValue): Boolean;
begin
  Result := PCefDictionaryValue(FData)^.is_same(PCefDictionaryValue(FData), CefGetData(that)) <> 0;
end;

function TCefDictionaryValueRef.IsValid: Boolean;
begin
  Result := PCefDictionaryValue(FData)^.is_valid(PCefDictionaryValue(FData)) <> 0;
end;

class function TCefDictionaryValueRef.New: ICefDictionaryValue;
begin
  Result := UnWrap(cef_dictionary_value_create());
end;

function TCefDictionaryValueRef.Remove(const key: ustring): Boolean;
var
  TempKey : TCefString;
begin
  TempKey := CefString(key);
  Result  := PCefDictionaryValue(FData)^.remove(PCefDictionaryValue(FData), @TempKey) <> 0;
end;

function TCefDictionaryValueRef.SetBinary(const key: ustring; const value: ICefBinaryValue): Boolean;
var
  TempKey : TCefString;
begin
  TempKey := CefString(key);
  Result  := PCefDictionaryValue(FData)^.set_binary(PCefDictionaryValue(FData), @TempKey, CefGetData(value)) <> 0;
end;

function TCefDictionaryValueRef.SetBool(const key: ustring; value: Boolean): Boolean;
var
  TempKey : TCefString;
begin
  TempKey := CefString(key);
  Result  := PCefDictionaryValue(FData)^.set_bool(PCefDictionaryValue(FData), @TempKey, Ord(value)) <> 0;
end;

function TCefDictionaryValueRef.SetDictionary(const key: ustring; const value: ICefDictionaryValue): Boolean;
var
  TempKey : TCefString;
begin
  TempKey := CefString(key);
  Result  := PCefDictionaryValue(FData)^.set_dictionary(PCefDictionaryValue(FData), @TempKey, CefGetData(value)) <> 0;
end;

function TCefDictionaryValueRef.SetDouble(const key: ustring; value: Double): Boolean;
var
  TempKey : TCefString;
begin
  TempKey := CefString(key);
  Result  := PCefDictionaryValue(FData)^.set_double(PCefDictionaryValue(FData), @TempKey, value) <> 0;
end;

function TCefDictionaryValueRef.SetInt(const key: ustring; value: Integer): Boolean;
var
  TempKey : TCefString;
begin
  TempKey := CefString(key);
  Result  := PCefDictionaryValue(FData)^.set_int(PCefDictionaryValue(FData), @TempKey, value) <> 0;
end;

function TCefDictionaryValueRef.SetList(const key: ustring; const value: ICefListValue): Boolean;
var
  TempKey : TCefString;
begin
  TempKey := CefString(key);
  Result  := PCefDictionaryValue(FData)^.set_list(PCefDictionaryValue(FData), @TempKey, CefGetData(value)) <> 0;
end;

function TCefDictionaryValueRef.SetNull(const key: ustring): Boolean;
var
  TempKey : TCefString;
begin
  TempKey := CefString(key);
  Result  := PCefDictionaryValue(FData)^.set_null(PCefDictionaryValue(FData), @TempKey) <> 0;
end;

function TCefDictionaryValueRef.SetString(const key, value: ustring): Boolean;
var
  TempKey, TempValue : TCefString;
begin
  TempKey   := CefString(key);
  TempValue := CefString(value);
  Result    := PCefDictionaryValue(FData)^.set_string(PCefDictionaryValue(FData), @TempKey, @TempValue) <> 0;
end;

function TCefDictionaryValueRef.SetValue(const key: ustring; const value: ICefValue): Boolean;
var
  TempKey : TCefString;
begin
  TempKey := CefString(key);
  Result  := PCefDictionaryValue(FData)^.set_value(PCefDictionaryValue(FData), @TempKey, CefGetData(value)) <> 0;
end;

class function TCefDictionaryValueRef.UnWrap(data: Pointer): ICefDictionaryValue;
begin
  if (data <> nil) then
    Result := Create(data) as ICefDictionaryValue
   else
    Result := nil;
end;

end.
