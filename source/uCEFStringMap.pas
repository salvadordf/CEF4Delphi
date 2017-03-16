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

unit uCEFStringMap;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefStringMapOwn = class(TInterfacedObject, ICefStringMap)
    protected
      FStringMap: TCefStringMap;

      function GetHandle: TCefStringMap; virtual;
      function GetSize: Integer; virtual;
      function Find(const key: ustring): ustring; virtual;
      function GetKey(index: Integer): ustring; virtual;
      function GetValue(index: Integer): ustring; virtual;
      procedure Append(const key, value: ustring); virtual;
      procedure Clear; virtual;

    public
      constructor Create; virtual;
      destructor Destroy; override;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;

procedure TCefStringMapOwn.Append(const key, value: ustring);
var
  k, v: TCefString;
begin
  k := CefString(key);
  v := CefString(value);
  cef_string_map_append(FStringMap, @k, @v);
end;

procedure TCefStringMapOwn.Clear;
begin
  cef_string_map_clear(FStringMap);
end;

constructor TCefStringMapOwn.Create;
begin
  FStringMap := cef_string_map_alloc;
end;

destructor TCefStringMapOwn.Destroy;
begin
  cef_string_map_free(FStringMap);
end;

function TCefStringMapOwn.Find(const key: ustring): ustring;
var
  str, k: TCefString;
begin
  FillChar(str, SizeOf(str), 0);
  k := CefString(key);
  cef_string_map_find(FStringMap, @k, str);
  Result := CefString(@str);
end;

function TCefStringMapOwn.GetHandle: TCefStringMap;
begin
  Result := FStringMap;
end;

function TCefStringMapOwn.GetKey(index: Integer): ustring;
var
  str: TCefString;
begin
  FillChar(str, SizeOf(str), 0);
  cef_string_map_key(FStringMap, index, str);
  Result := CefString(@str);
end;

function TCefStringMapOwn.GetSize: Integer;
begin
  Result := cef_string_map_size(FStringMap);
end;

function TCefStringMapOwn.GetValue(index: Integer): ustring;
var
  str: TCefString;
begin
  FillChar(str, SizeOf(str), 0);
  cef_string_map_value(FStringMap, index, str);
  Result := CefString(@str);
end;

end.
