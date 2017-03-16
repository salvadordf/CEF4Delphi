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

unit uCEFv8Interceptor;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes, uCEFv8Types;

type
  TCefV8InterceptorOwn = class(TCefBaseRefCountedOwn, ICefV8Interceptor)
    protected
      function GetByName(const name: ustring; const obj: ICefv8Value; out retval: ICefv8Value; const exception: ustring): boolean; virtual;
      function GetByIndex(index: integer; const obj: ICefv8Value; out retval: ICefv8Value; const exception: ustring): boolean; virtual;
      function SetByName(const name: ustring; const obj, value: ICefv8Value; const exception: ustring): boolean; virtual;
      function SetByIndex(index: integer; const obj, value: ICefv8Value; const exception: ustring): boolean; virtual;

    public
      constructor Create; virtual;
  end;

  TCefFastV8Interceptor = class(TCefV8InterceptorOwn)
    protected
      FGetterByName  : TCefV8InterceptorGetterByNameProc;
      FSetterByName  : TCefV8InterceptorSetterByNameProc;
      FGetterByIndex : TCefV8InterceptorGetterByIndexProc;
      FSetterByIndex : TCefV8InterceptorSetterByIndexProc;

      function GetByName(const name: ustring; const obj: ICefv8Value; out retval: ICefv8Value; const exception: ustring): boolean; override;
      function GetByIndex(index: integer; const obj: ICefv8Value; out retval: ICefv8Value; const exception: ustring): boolean; override;
      function SetByName(const name: ustring; const obj, value: ICefv8Value; const exception: ustring): boolean; override;
      function SetByIndex(index: integer; const obj, value: ICefv8Value; const exception: ustring): boolean; override;

    public
      constructor Create(const getterbyname  : TCefV8InterceptorGetterByNameProc;
                         const setterbyname  : TCefV8InterceptorSetterByNameProc;
                         const getterbyindex : TCefV8InterceptorGetterByIndexProc;
                         const setterbyindex : TCefV8InterceptorSetterByIndexProc); reintroduce;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFv8Value;

function cef_v8_interceptor_get_byname(self: PCefV8Interceptor; const name: PCefString; const obj: PCefV8Value; out retval: PCefv8Value; exception: PCefString): Integer; stdcall;
var
  ret: ICefv8Value;
begin
  Result := Ord(TCefV8InterceptorOwn(CefGetObject(self)).GetByName(CefString(name), TCefv8ValueRef.UnWrap(obj), ret, CefString(exception)));
  retval := CefGetData(ret);
end;

function cef_v8_interceptor_get_byindex(self: PCefV8Interceptor; index: integer; const obj: PCefV8Value; out retval: PCefv8Value; exception: PCefString): integer; stdcall;
var
  ret: ICefv8Value;
begin
  Result := Ord(TCefV8InterceptorOwn(CefGetObject(self)).GetByIndex(index, TCefv8ValueRef.UnWrap(obj), ret, CefString(exception)));
  retval := CefGetData(ret);
end;

function cef_v8_interceptor_set_byname(self: PCefV8Interceptor; const name: PCefString; const obj: PCefV8Value; value: PCefv8Value; exception: PCefString): integer; stdcall;
begin
  Result := Ord(TCefV8InterceptorOwn(CefGetObject(self)).SetByName(CefString(name), TCefv8ValueRef.UnWrap(obj), TCefv8ValueRef.UnWrap(value), CefString(exception)));
end;

function cef_v8_interceptor_set_byindex(self: PCefV8Interceptor; index: integer; const obj: PCefV8Value; value: PCefv8Value; exception: PCefString): integer; stdcall;
begin
  Result := Ord(TCefV8InterceptorOwn(CefGetObject(self)).SetByIndex(index, TCefv8ValueRef.UnWrap(obj), TCefv8ValueRef.UnWrap(value), CefString(exception)));
end;

// TCefV8InterceptorOwn

constructor TCefV8InterceptorOwn.Create;
begin
  inherited CreateData(SizeOf(TCefV8InterceptorOwn));

  with PCefV8Interceptor(FData)^ do
    begin
      get_byname  := cef_v8_interceptor_get_byname;
      get_byindex := cef_v8_interceptor_get_byindex;
      set_byname  := cef_v8_interceptor_set_byname;
      set_byindex := cef_v8_interceptor_set_byindex;
    end;
end;

function TCefV8InterceptorOwn.GetByName(const name: ustring; const obj: ICefv8Value; out retval: ICefv8Value; const exception: ustring): boolean;
begin
  Result := False;
end;

function TCefV8InterceptorOwn.GetByIndex(index: integer; const obj: ICefv8Value; out retval: ICefv8Value; const exception: ustring): boolean;
begin
  Result := False;
end;

function TCefV8InterceptorOwn.SetByName(const name: ustring; const obj, value: ICefv8Value; const exception: ustring): boolean;
begin
  Result := False;
end;

function TCefV8InterceptorOwn.SetByIndex(index: integer; const obj, value: ICefv8Value; const exception: ustring): boolean;
begin
  Result := False;
end;

// TCefFastV8Interceptor

constructor TCefFastV8Interceptor.Create(const getterbyname  : TCefV8InterceptorGetterByNameProc;
                                         const setterbyname  : TCefV8InterceptorSetterByNameProc;
                                         const getterbyindex : TCefV8InterceptorGetterByIndexProc;
                                         const setterbyindex : TCefV8InterceptorSetterByIndexProc);
begin
  FGetterByName  := getterbyname;
  FSetterByName  := setterbyname;
  FGetterByIndex := getterbyindex;
  FSetterByIndex := setterbyindex;
end;

function TCefFastV8Interceptor.GetByName(const name: ustring; const obj: ICefv8Value; out retval: ICefv8Value; const exception: ustring): boolean;
begin
  if assigned(FGetterByName) then
    Result := FGetterByName(name, obj, retval, exception)
   else
    Result := False;
end;

function TCefFastV8Interceptor.GetByIndex(index: integer; const obj: ICefv8Value; out retval: ICefv8Value; const exception: ustring): boolean;
begin
  if assigned(FGetterByIndex) then
    Result := FGetterByIndex(index, obj, retval, exception)
   else
    Result := False;
end;

function TCefFastV8Interceptor.SetByName(const name: ustring; const obj, value: ICefv8Value; const exception: ustring): boolean;
begin
  if assigned(FSetterByName) then
    Result := FSetterByName(name, obj, value, exception)
   else
    Result := False;
end;

function TCefFastV8Interceptor.SetByIndex(index: integer; const obj, value: ICefv8Value; const exception: ustring): boolean;
begin
  if assigned(FSetterByIndex) then
    Result := FSetterByIndex(index, obj, value, exception)
   else
    Result := False;
end;

end.
