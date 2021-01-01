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

unit uCEFStringMap;

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
  TCefCustomStringMap = class(TInterfacedObject, ICefStringMap)
    protected
      FHandle : TCefStringMap;

      function  GetHandle: TCefStringMap; virtual;
      function  GetSize: NativeUInt; virtual;
      function  Find(const key: ustring): ustring; virtual;
      function  GetKey(index: NativeUInt): ustring; virtual;
      function  GetValue(index: NativeUInt): ustring; virtual;
      function  Append(const key, value: ustring) : boolean; virtual;
      procedure Clear; virtual;

    public
      constructor Create; virtual;
  end;

  TCefStringMapOwn = class(TCefCustomStringMap)
    public
      constructor Create; override;
      destructor  Destroy; override;
  end;

  TCefStringMapRef = class(TCefCustomStringMap)
    public
      constructor Create(aHandle : TCefStringMap); reintroduce;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;


// ****************************************
// ********* TCefCustomStringMap **********
// ****************************************


constructor TCefCustomStringMap.Create;
begin
  inherited Create;

  FHandle := nil;
end;

function TCefCustomStringMap.Append(const key, value: ustring) : boolean;
var
  TempKey, TempValue : TCefString;
begin
  if (FHandle <> nil) then
    begin
      TempKey   := CefString(key);
      TempValue := CefString(value);
      Result    := cef_string_map_append(FHandle, @TempKey, @TempValue) <> 0;
    end
   else
    Result := False;
end;

procedure TCefCustomStringMap.Clear;
begin
  if (FHandle <> nil) then cef_string_map_clear(FHandle);
end;

function TCefCustomStringMap.Find(const key: ustring): ustring;
var
  TempKey, TempValue : TCefString;
begin
  Result := '';

  if (FHandle <> nil) then
    begin
      CefStringInitialize(@TempValue);

      TempKey := CefString(key);

      if (cef_string_map_find(FHandle, @TempKey, @TempValue) <> 0) then
        Result := CefStringClearAndGet(@TempValue);
    end;
end;

function TCefCustomStringMap.GetHandle: TCefStringMap;
begin
  Result := FHandle;
end;

function TCefCustomStringMap.GetKey(index: NativeUInt): ustring;
var
  TempKey : TCefString;
begin
  Result := '';

  if (FHandle <> nil) then
    begin
      CefStringInitialize(@TempKey);

      if (cef_string_map_key(FHandle, index, @TempKey) <> 0) then
        Result := CefStringClearAndGet(@TempKey);
    end;
end;

function TCefCustomStringMap.GetSize: NativeUInt;
begin
  if (FHandle <> nil) then
    Result := cef_string_map_size(FHandle)
   else
    Result := 0;
end;

function TCefCustomStringMap.GetValue(index: NativeUInt): ustring;
var
  TempValue : TCefString;
begin
  Result := '';

  if (FHandle <> nil) then
    begin
      CefStringInitialize(@TempValue);

      if (cef_string_map_value(FHandle, index, @TempValue) <> 0) then
        Result := CefStringClearAndGet(@TempValue);
    end;
end;


// **************************************
// ********* TCefStringMapOwn ***********
// **************************************


constructor TCefStringMapOwn.Create;
begin
  inherited Create;

  FHandle := cef_string_map_alloc();
end;

destructor TCefStringMapOwn.Destroy;
begin
  if (FHandle <> nil) then cef_string_map_free(FHandle);

  inherited Destroy;
end;


// **************************************
// ********* TCefStringMapRef ***********
// **************************************


constructor TCefStringMapRef.Create(aHandle : TCefStringMap);
begin
  inherited Create;

  FHandle := aHandle;
end;

end.
