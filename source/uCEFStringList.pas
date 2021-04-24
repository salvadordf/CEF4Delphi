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

unit uCEFStringList;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes,
  {$ELSE}
  Classes,
  {$ENDIF}
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefCustomStringList = class(TInterfacedObject, ICefStringList)
    protected
      FHandle : TCefStringList;

      function  GetHandle: TCefStringMap; virtual;
      function  GetSize: NativeUInt; virtual;
      function  GetValue(index: NativeUInt): ustring; virtual;
      procedure Append(const value: ustring); virtual;
      procedure Clear; virtual;
      function  Copy : TCefStringList; virtual;
      procedure CopyToStrings(const aStrings : TStrings); virtual;
      procedure AddStrings(const aStrings : TStrings); virtual;

    public
      constructor Create; virtual;
  end;

  TCefStringListOwn = class(TCefCustomStringList)
    public
      constructor Create; override;
      destructor  Destroy; override;
  end;

  TCefStringListRef = class(TCefCustomStringList)
    public
      constructor Create(aHandle : TCefStringList); reintroduce;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;


// *****************************************
// ********* TCefCustomStringList **********
// *****************************************


constructor TCefCustomStringList.Create;
begin
  inherited Create;

  FHandle := nil;
end;

procedure TCefCustomStringList.Append(const value: ustring);
var
  TempValue : TCefString;
begin
  if (FHandle <> nil) then
    begin
      TempValue := CefString(value);
      cef_string_list_append(FHandle, @TempValue);
    end;
end;

procedure TCefCustomStringList.Clear;
begin
  if (FHandle <> nil) then cef_string_list_clear(FHandle);
end;

function TCefCustomStringList.GetHandle: TCefStringMap;
begin
  Result := FHandle;
end;

function TCefCustomStringList.GetSize: NativeUInt;
begin
  if (FHandle <> nil) then
    Result := cef_string_list_size(FHandle)
   else
    Result := 0;
end;

function TCefCustomStringList.Copy : TCefStringList;
begin
  if (FHandle <> nil) then
    Result := cef_string_list_copy(FHandle)
   else
    Result := nil;
end;

function TCefCustomStringList.GetValue(index: NativeUInt): ustring;
var
  TempValue : TCefString;
begin
  Result := '';

  if (FHandle <> nil) then
    begin
      CefStringInitialize(@TempValue);

      if (cef_string_list_value(FHandle, index, @TempValue) <> 0) then
        Result := CefStringClearAndGet(@TempValue);
    end;
end;

procedure TCefCustomStringList.CopyToStrings(const aStrings : TStrings);
var
  i, j : NativeUInt;
  TempValue : TCefString;
begin
  if (aStrings <> nil) and (FHandle <> nil) then
    begin
      i := 0;
      j := GetSize;

      while (i < j) do
        begin
          CefStringInitialize(@TempValue);

          if (cef_string_list_value(FHandle, i, @TempValue) <> 0) then
            aStrings.Add(CefStringClearAndGet(@TempValue));

          inc(i);
        end;
    end;
end;

procedure TCefCustomStringList.AddStrings(const aStrings : TStrings);
var
  i : integer;
begin
  if (FHandle <> nil) and (aStrings <> nil) and (aStrings.Count > 0) then
    for i := 0 to aStrings.Count - 1 do Append(aStrings[i]);
end;



// *****************************************
// *********** TCefStringListOwn ***********
// *****************************************


constructor TCefStringListOwn.Create;
begin
  inherited Create;

  FHandle := cef_string_list_alloc();
end;

destructor TCefStringListOwn.Destroy;
begin
  if (FHandle <> nil) then cef_string_list_free(FHandle);

  inherited Destroy;
end;


// *****************************************
// *********** TCefStringListRef ***********
// *****************************************


constructor TCefStringListRef.Create(aHandle : TCefStringList);
begin
  inherited Create;

  FHandle := aHandle;
end;

end.
