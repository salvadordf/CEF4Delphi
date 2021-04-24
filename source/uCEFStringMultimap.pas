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

unit uCEFStringMultimap;

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
  TCefCustomStringMultimap = class(TInterfacedObject, ICefStringMultimap)
    protected
      FHandle : TCefStringMultimap;

      function  GetHandle: TCefStringMultimap; virtual;
      function  GetSize: NativeUInt; virtual;
      function  FindCount(const Key: ustring): NativeUInt; virtual;
      function  GetEnumerate(const Key: ustring; ValueIndex: NativeUInt): ustring; virtual;
      function  GetKey(Index: NativeUInt): ustring; virtual;
      function  GetValue(Index: NativeUInt): ustring; virtual;
      function  Append(const Key, Value: ustring) : boolean; virtual;
      procedure Clear; virtual;

    public
      constructor Create; virtual;
  end;

  TCefStringMultimapOwn = class(TCefCustomStringMultimap)
    public
      constructor Create; override;
      destructor  Destroy; override;
  end;

  TCefStringMultimapRef = class(TCefCustomStringMultimap)
    public
      constructor Create(aHandle : TCefStringMultimap); reintroduce;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;


// *********************************************
// ********* TCefCustomStringMultimap **********
// *********************************************


constructor TCefCustomStringMultimap.Create;
begin
  inherited Create;

  FHandle := nil;
end;

function TCefCustomStringMultimap.Append(const Key, Value: ustring) : boolean;
var
  TempKey, TempValue : TCefString;
begin
  if (FHandle <> nil) then
    begin
      TempKey   := CefString(key);
      TempValue := CefString(value);
      Result    := (cef_string_multimap_append(FHandle, @TempKey, @TempValue) <> 0);
    end
   else
    Result := False;
end;

procedure TCefCustomStringMultimap.Clear;
begin
  if (FHandle <> nil) then cef_string_multimap_clear(FHandle);
end;

function TCefCustomStringMultimap.FindCount(const Key: ustring): NativeUInt;
var
  TempKey : TCefString;
begin
  if (FHandle <> nil) then
    begin
      TempKey := CefString(Key);
      Result  := cef_string_multimap_find_count(FHandle, @TempKey);
    end
   else
    Result := 0;
end;

function TCefCustomStringMultimap.GetEnumerate(const Key: ustring; ValueIndex: NativeUInt): ustring;
var
  TempKey, TempValue : TCefString;
begin
  Result := '';

  if (FHandle <> nil) then
    begin
      CefStringInitialize(@TempValue);

      TempKey := CefString(Key);

      if (cef_string_multimap_enumerate(FHandle, @TempKey, ValueIndex, @TempValue) <> 0) then
        Result := CefStringClearAndGet(@TempValue);
    end;
end;

function TCefCustomStringMultimap.GetHandle: TCefStringMultimap;
begin
  Result := FHandle;
end;

function TCefCustomStringMultimap.GetKey(Index: NativeUInt): ustring;
var
  TempKey : TCefString;
begin
  Result := '';

  if (FHandle <> nil) then
    begin
      CefStringInitialize(@TempKey);

      if (cef_string_multimap_key(FHandle, index, @TempKey) <> 0) then
        Result := CefStringClearAndGet(@TempKey);
    end;
end;

function TCefCustomStringMultimap.GetSize: NativeUInt;
begin
  if (FHandle <> nil) then
    Result := cef_string_multimap_size(FHandle)
   else
    Result := 0;
end;

function TCefCustomStringMultimap.GetValue(Index: NativeUInt): ustring;
var
  TempValue : TCefString;
begin
  Result := '';

  if (FHandle <> nil) then
    begin
      CefStringInitialize(@TempValue);

      if (cef_string_multimap_value(FHandle, index, @TempValue) <> 0) then
        Result := CefStringClearAndGet(@TempValue);
    end;
end;


// ******************************************
// ********* TCefStringMultimapOwn **********
// ******************************************


constructor TCefStringMultimapOwn.Create;
begin
  inherited Create;

  FHandle := cef_string_multimap_alloc();
end;

destructor TCefStringMultimapOwn.Destroy;
begin
  if (FHandle <> nil) then cef_string_multimap_free(FHandle);

  inherited Destroy;
end;


// ******************************************
// ********* TCefStringMultimapRef **********
// ******************************************


constructor TCefStringMultimapRef.Create(aHandle : TCefStringMultimap);
begin
  inherited Create;

  FHandle := aHandle;
end;

end.
