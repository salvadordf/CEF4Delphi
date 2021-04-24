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

unit uCEFJson;

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
  uCEFInterfaces, uCEFTypes, uCEFConstants;

type
  TCEFJson = class
    public
      class function ReadValue(const aDictionary : ICefDictionaryValue; const aKey : string; var aValue : ICefValue) : boolean;
      class function ReadBoolean(const aDictionary : ICefDictionaryValue; const aKey : string; var aValue : boolean) : boolean;
      class function ReadInteger(const aDictionary : ICefDictionaryValue; const aKey : string; var aValue : integer) : boolean;
      class function ReadDouble(const aDictionary : ICefDictionaryValue; const aKey : string; var aValue : double) : boolean;
      class function ReadString(const aDictionary : ICefDictionaryValue; const aKey : string; var aValue : ustring) : boolean;
      class function ReadBinary(const aDictionary : ICefDictionaryValue; const aKey : string; var aValue : ICefBinaryValue) : boolean;
      class function ReadDictionary(const aDictionary : ICefDictionaryValue; const aKey : string; var aValue : ICefDictionaryValue) : boolean;
      class function ReadList(const aDictionary : ICefDictionaryValue; const aKey : string; var aValue : ICefListValue) : boolean;

      class function Parse(const jsonString: ustring; options: TCefJsonParserOptions = JSON_PARSER_RFC): ICefValue; overload;
      class function Parse(const json: Pointer; json_size: NativeUInt; options: TCefJsonParserOptions = JSON_PARSER_RFC): ICefValue; overload;
      class function ParseAndReturnError(const jsonString: ustring; options: TCefJsonParserOptions; out errorMsgOut: ustring): ICefValue;
      class function Write(const node: ICefValue; options: TCefJsonWriterOptions = JSON_WRITER_DEFAULT): ustring; overload;
      class function Write(const node: ICefDictionaryValue; options: TCefJsonWriterOptions = JSON_WRITER_DEFAULT): ustring; overload;
      class function Write(const node: ICefValue; var aRsltStrings: TStringList): boolean; overload;
      class function Write(const node: ICefDictionaryValue; var aRsltStrings: TStringList): boolean; overload;
      class function SaveToFile(const node: ICefValue; const aFileName: ustring): boolean; overload;
      class function SaveToFile(const node: ICefDictionaryValue; const aFileName: ustring): boolean; overload;
      class function LoadFromFile(const aFileName: ustring; var aRsltNode: ICefValue; {$IFDEF DELPHI12_UP}encoding: TEncoding = nil;{$ENDIF} options: TCefJsonParserOptions = JSON_PARSER_RFC): boolean;
  end;

implementation

uses
  uCEFLibFunctions, uCEFApplicationCore, uCEFMiscFunctions, uCEFValue;

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

class function TCEFJson.ReadString(const aDictionary : ICefDictionaryValue; const aKey : string; var aValue : ustring) : boolean;
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

class function TCEFJson.Parse(const jsonString: ustring; options: TCefJsonParserOptions): ICefValue;
var
  TempJSON : TCefString;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      TempJSON := CefString(jsonString);
      Result   := TCefValueRef.UnWrap(cef_parse_json(@TempJSON, options));
    end
   else
    Result := nil;
end;

// json must be a pointer to a UTF8 string
class function TCEFJson.Parse(const json: Pointer; json_size: NativeUInt; options: TCefJsonParserOptions): ICefValue;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded and (json <> nil) and (json_size > 0) then
    Result := TCefValueRef.UnWrap(cef_parse_json_buffer(json, json_size, options))
   else
    Result := nil;
end;

class function TCEFJson.ParseAndReturnError(const jsonString   : ustring;
                                                  options      : TCefJsonParserOptions;
                                            out   errorMsgOut  : ustring): ICefValue;
var
  TempJSON, TempError : TCefString;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded then
    begin
      CefStringInitialize(@TempError);
      TempJSON    := CefString(jsonString);
      Result      := TCefValueRef.UnWrap(cef_parse_jsonand_return_error(@TempJSON, options, @TempError));
      errorMsgOut := CefStringClearAndGet(@TempError);
    end
   else
    begin
      Result       := nil;
      errorMsgOut  := '';
    end;
end;

class function TCEFJson.Write(const node: ICefValue; options: TCefJsonWriterOptions): ustring;
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.LibLoaded and (node <> nil) then
    Result := CefStringFreeAndGet(cef_write_json(CefGetData(node), options))
   else
    Result := '';
end;

class function TCEFJson.Write(const node: ICefDictionaryValue; options: TCefJsonWriterOptions): ustring;
var
  TempValue : ICefValue;
begin
  Result := '';

  if (node = nil) then exit;

  try
    TempValue := TCefValueRef.New;
    TempValue.SetDictionary(node);
    Result := Write(TempValue, options);
  finally
    TempValue := nil;
  end;
end;

class function TCEFJson.Write(const node: ICefValue; var aRsltStrings: TStringList): boolean;
var
  TempJSON : ustring;
begin
  Result := False;

  if (aRsltStrings <> nil) then
    begin
      TempJSON := Write(node, JSON_WRITER_PRETTY_PRINT);

      if (length(TempJSON) > 0) then
        begin
          aRsltStrings.SetText(@TempJSON[1]);
          Result := True;
        end;
    end;
end;

class function TCEFJson.Write(const node: ICefDictionaryValue; var aRsltStrings: TStringList): boolean;
var
  TempJSON : ustring;
begin
  Result := False;

  if (aRsltStrings <> nil) then
    begin
      TempJSON := Write(node, JSON_WRITER_PRETTY_PRINT);

      if (length(TempJSON) > 0) then
        begin
          aRsltStrings.SetText(@TempJSON[1]);
          Result := True;
        end;
    end;
end;

class function TCEFJson.SaveToFile(const node: ICefValue; const aFileName: ustring): boolean;
var
  TempJSON : TStringList;
begin
  Result   := False;
  TempJSON := nil;

  try
    try
      TempJSON := TStringList.Create;

      if Write(node, TempJSON) then
        begin
          TempJSON.SaveToFile(aFileName);
          Result := True;
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TCEFJson.SaveToFile', e) then raise;
    end;
  finally
    if (TempJSON <> nil) then FreeAndNil(TempJSON);
  end;
end;

class function TCEFJson.SaveToFile(const node: ICefDictionaryValue; const aFileName: ustring): boolean;
var
  TempJSON : TStringList;
begin
  Result   := False;
  TempJSON := nil;

  try
    try
      TempJSON := TStringList.Create;

      if Write(node, TempJSON) then
        begin
          TempJSON.SaveToFile(aFileName);
          Result := True;
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TCEFJson.SaveToFile', e) then raise;
    end;
  finally
    if (TempJSON <> nil) then FreeAndNil(TempJSON);
  end;
end;

class function TCEFJson.LoadFromFile(const aFileName: ustring; var aRsltNode: ICefValue; {$IFDEF DELPHI12_UP}encoding: TEncoding;{$ENDIF} options: TCefJsonParserOptions): boolean;
var
  TempJSON : TStringList;
begin
  Result   := False;
  TempJSON := nil;

  try
    try
      if (length(aFileName) > 0) and FileExists(aFileName) then
        begin
          TempJSON  := TStringList.Create;
          TempJSON.LoadFromFile(aFileName{$IFDEF DELPHI12_UP}, encoding{$ENDIF});
          aRsltNode := Parse(TempJSON.Text, options);
          Result    := True;
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TCEFJson.LoadFromFile', e) then raise;
    end;
  finally
    if (TempJSON <> nil) then FreeAndNil(TempJSON);
  end;
end;

end.
