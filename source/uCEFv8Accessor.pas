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
//        Copyright © 2019 Salvador Diaz Fau. All rights reserved.
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

unit uCEFv8Accessor;

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
  TCefV8AccessorOwn = class(TCefBaseRefCountedOwn, ICefV8Accessor)
    protected
      function Get(const name: ustring; const obj: ICefv8Value; out retval: ICefv8Value; var exception: ustring): Boolean; virtual;
      function Put(const name: ustring; const obj, value: ICefv8Value; var exception: ustring): Boolean; virtual;

    public
      constructor Create; virtual;
  end;

  TCefFastV8Accessor = class(TCefV8AccessorOwn)
    protected
      FGetter: TCefV8AccessorGetterProc;
      FSetter: TCefV8AccessorSetterProc;

      function Get(const name: ustring; const obj: ICefv8Value; out retval: ICefv8Value; var exception: ustring): Boolean; override;
      function Put(const name: ustring; const obj, value: ICefv8Value; var exception: ustring): Boolean; override;

    public
      constructor Create(const getter: TCefV8AccessorGetterProc; const setter: TCefV8AccessorSetterProc); reintroduce;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFv8Value;

function cef_v8_accessor_get(      self      : PCefV8Accessor;
                             const name      : PCefString;
                                   obj       : PCefv8Value;
                             out   retval    : PCefv8Value;
                                   exception : PCefString): Integer; stdcall;
var
  ret : ICefv8Value;
  TempExcept : ustring;
  TempObject : TObject;
begin
  Result     := Ord(False);
  TempExcept := CefString(exception);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefV8AccessorOwn) then
    Result := Ord(TCefV8AccessorOwn(TempObject).Get(CefString(name),
                                                    TCefv8ValueRef.UnWrap(obj),
                                                    ret,
                                                    TempExcept));

  retval     := CefGetData(ret);
  exception^ := CefString(TempExcept);
end;

function cef_v8_accessor_put(      self      : PCefV8Accessor;
                             const name      : PCefString;
                                   obj       : PCefv8Value;
                                   value     : PCefv8Value;
                                   exception : PCefString): Integer; stdcall;
var
  TempExcept : ustring;
  TempObject : TObject;
begin
  Result     := Ord(False);
  TempExcept := CefString(exception);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefV8AccessorOwn) then
    Result := Ord(TCefV8AccessorOwn(TempObject).Put(CefString(name),
                                                    TCefv8ValueRef.UnWrap(obj),
                                                    TCefv8ValueRef.UnWrap(value),
                                                    TempExcept));

  exception^ := CefString(TempExcept);
end;

// TCefV8AccessorOwn

constructor TCefV8AccessorOwn.Create;
begin
  inherited CreateData(SizeOf(TCefV8Accessor));

  with PCefV8Accessor(FData)^ do
    begin
      get := {$IFDEF FPC}@{$ENDIF}cef_v8_accessor_get;
      put := {$IFDEF FPC}@{$ENDIF}cef_v8_accessor_put;
    end;
end;

function TCefV8AccessorOwn.Get(const name: ustring; const obj: ICefv8Value; out retval: ICefv8Value; var exception: ustring): Boolean;
begin
  Result := False;
end;

function TCefV8AccessorOwn.Put(const name: ustring; const obj, value: ICefv8Value; var exception: ustring): Boolean;
begin
  Result := False;
end;

// TCefFastV8Accessor

constructor TCefFastV8Accessor.Create(const getter: TCefV8AccessorGetterProc; const setter: TCefV8AccessorSetterProc);
begin
  FGetter := getter;
  FSetter := setter;
end;

function TCefFastV8Accessor.Get(const name: ustring; const obj: ICefv8Value; out retval: ICefv8Value; var exception: ustring): Boolean;
begin
  if Assigned(FGetter) then
    Result := FGetter(name, obj, retval, exception)
   else
    Result := False;
end;

function TCefFastV8Accessor.Put(const name: ustring; const obj, value: ICefv8Value; var exception: ustring): Boolean;
begin
  if Assigned(FSetter) then
    Result := FSetter(name, obj, value, exception)
   else
    Result := False;
end;

end.
