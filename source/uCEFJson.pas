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

unit uCEFJson;

interface

uses
  uCEFInterfaces;

type
  TCEFJson = class
    public
      class function ReadValue(const aDictionary : ICefDictionaryValue; const aKey : string; var aValue : ICefValue) : boolean;
      class function ReadBoolean(const aDictionary : ICefDictionaryValue; const aKey : string; var aValue : boolean) : boolean;
      class function ReadInteger(const aDictionary : ICefDictionaryValue; const aKey : string; var aValue : integer) : boolean;
      class function ReadDouble(const aDictionary : ICefDictionaryValue; const aKey : string; var aValue : double) : boolean;
      class function ReadString(const aDictionary : ICefDictionaryValue; const aKey : string; var aValue : string) : boolean;
      class function ReadBinary(const aDictionary : ICefDictionaryValue; const aKey : string; var aValue : ICefBinaryValue) : boolean;
      class function ReadDictionary(const aDictionary : ICefDictionaryValue; const aKey : string; var aValue : ICefDictionaryValue) : boolean;
      class function ReadList(const aDictionary : ICefDictionaryValue; const aKey : string; var aValue : ICefListValue) : boolean;
  end;

implementation

uses
  uCEFTypes;

class function TCEFJson.ReadValue(const aDictionary : ICefDictionaryValue; const aKey : string; var aValue : ICefValue) : boolean;
begin
  Result := False;
  aValue := nil;

  if (aDictionary <> nil) then
    begin
      aValue := aDictionary.GetValue(aKey);
      Result := (aValue <> nil);
    end;
end;

class function TCEFJson.ReadBoolean(const aDictionary : ICefDictionaryValue; const aKey : string; var aValue : boolean) : boolean;
var
  TempValue : ICefValue;
begin
  Result := False;
  aValue := False;

  if ReadValue(aDictionary, aKey, TempValue) and
     (TempValue.GetType = VTYPE_BOOL) then
    begin
      aValue := TempValue.GetBool;
      Result := True;
    end;
end;

class function TCEFJson.ReadInteger(const aDictionary : ICefDictionaryValue; const aKey : string; var aValue : integer) : boolean;
var
  TempValue : ICefValue;
begin
  Result := False;
  aValue := 0;

  if ReadValue(aDictionary, aKey, TempValue) and
     (TempValue.GetType = VTYPE_INT) then
    begin
      aValue := TempValue.GetInt;
      Result := True;
    end;
end;

class function TCEFJson.ReadDouble(const aDictionary : ICefDictionaryValue; const aKey : string; var aValue : double) : boolean;
var
  TempValue : ICefValue;
begin
  Result := False;
  aValue := 0;

  if ReadValue(aDictionary, aKey, TempValue) and
     (TempValue.GetType = VTYPE_DOUBLE) then
    begin
      aValue := TempValue.GetDouble;
      Result := True;
    end;
end;

class function TCEFJson.ReadString(const aDictionary : ICefDictionaryValue; const aKey : string; var aValue : string) : boolean;
var
  TempValue : ICefValue;
begin
  Result := False;
  aValue := '';

  if ReadValue(aDictionary, aKey, TempValue) and
     (TempValue.GetType = VTYPE_STRING) then
    begin
      aValue := TempValue.GetString;
      Result := True;
    end;
end;

class function TCEFJson.ReadBinary(const aDictionary : ICefDictionaryValue; const aKey : string; var aValue : ICefBinaryValue) : boolean;
var
  TempValue : ICefValue;
begin
  Result := False;
  aValue := nil;

  if ReadValue(aDictionary, aKey, TempValue) and
     (TempValue.GetType = VTYPE_BINARY) then
    begin
      aValue := TempValue.GetBinary;
      Result := True;
    end;
end;

class function TCEFJson.ReadDictionary(const aDictionary : ICefDictionaryValue; const aKey : string; var aValue : ICefDictionaryValue) : boolean;
var
  TempValue : ICefValue;
begin
  Result := False;
  aValue := nil;

  if ReadValue(aDictionary, aKey, TempValue) and
     (TempValue.GetType = VTYPE_DICTIONARY) then
    begin
      aValue := TempValue.GetDictionary;
      Result := True;
    end;
end;

class function TCEFJson.ReadList(const aDictionary : ICefDictionaryValue; const aKey : string; var aValue : ICefListValue) : boolean;
var
  TempValue : ICefValue;
begin
  Result := False;
  aValue := nil;

  if ReadValue(aDictionary, aKey, TempValue) and
     (TempValue.GetType = VTYPE_LIST) then
    begin
      aValue := TempValue.GetList;
      Result := True;
    end;
end;

end.
