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

unit uCEFResourceBundleHandler;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  uCEFBase, uCEFInterfaces, uCEFTypes;

type
  TCefResourceBundleHandlerOwn = class(TCefBaseOwn, ICefResourceBundleHandler)
    protected
      function GetDataResource(stringId: Integer; out data: Pointer; out dataSize: NativeUInt): Boolean; virtual; abstract;
      function GetLocalizedString(messageId: Integer; out stringVal: ustring): Boolean; virtual; abstract;
      function GetDataResourceForScale(resourceId: Integer; scaleFactor: TCefScaleFactor; out data: Pointer; dataSize: NativeUInt): Boolean; virtual; abstract;

    public
      constructor Create; virtual;
  end;

  TGetDataResource         = {$IFDEF DELPHI12_UP}reference to{$ENDIF} function(resourceId: Integer; out data: Pointer; out dataSize: NativeUInt): Boolean;
  TGetLocalizedString      = {$IFDEF DELPHI12_UP}reference to{$ENDIF} function(stringId: Integer; out stringVal: ustring): Boolean;
  TGetDataResourceForScale = {$IFDEF DELPHI12_UP}reference to{$ENDIF} function(resourceId: Integer; scaleFactor: TCefScaleFactor; out data: Pointer; out dataSize: NativeUInt): Boolean;

  TCefFastResourceBundle = class(TCefResourceBundleHandlerOwn)
    protected
      FGetDataResource: TGetDataResource;
      FGetLocalizedString: TGetLocalizedString;
      FGetDataResourceForScale: TGetDataResourceForScale;

      function GetDataResource(resourceId: Integer; out data: Pointer; out dataSize: NativeUInt): Boolean; override;
      function GetLocalizedString(stringId: Integer; out stringVal: ustring): Boolean; override;
      function GetDataResourceForScale(resourceId: Integer; scaleFactor: TCefScaleFactor; out data: Pointer; dataSize: NativeUInt): Boolean; override;

    public
      constructor Create(AGetDataResource: TGetDataResource; AGetLocalizedString: TGetLocalizedString; AGetDataResourceForScale: TGetDataResourceForScale); reintroduce;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;

function cef_resource_bundle_handler_get_localized_string(self: PCefResourceBundleHandler;
  string_id: Integer; string_val: PCefString): Integer; stdcall;
var
  str: ustring;
begin
  Result := Ord(TCefResourceBundleHandlerOwn(CefGetObject(self)).
    GetLocalizedString(string_id, str));
  if Result <> 0 then
    string_val^ := CefString(str);
end;

function cef_resource_bundle_handler_get_data_resource(self: PCefResourceBundleHandler;
  resource_id: Integer; var data: Pointer; var data_size: NativeUInt): Integer; stdcall;
begin
  Result := Ord(TCefResourceBundleHandlerOwn(CefGetObject(self)).
    GetDataResource(resource_id, data, data_size));
end;

function cef_resource_bundle_handler_get_data_resource_for_scale(
  self: PCefResourceBundleHandler; resource_id: Integer; scale_factor: TCefScaleFactor;
  out data: Pointer; data_size: NativeUInt): Integer; stdcall;
begin
  Result := Ord(TCefResourceBundleHandlerOwn(CefGetObject(self)).
    GetDataResourceForScale(resource_id, scale_factor, data, data_size));
end;

constructor TCefResourceBundleHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefResourceBundleHandler));
  with PCefResourceBundleHandler(FData)^ do
  begin
    get_localized_string := cef_resource_bundle_handler_get_localized_string;
    get_data_resource := cef_resource_bundle_handler_get_data_resource;
    get_data_resource_for_scale := cef_resource_bundle_handler_get_data_resource_for_scale;
  end;
end;

// TCefFastResourceBundle

constructor TCefFastResourceBundle.Create(AGetDataResource: TGetDataResource;
  AGetLocalizedString: TGetLocalizedString; AGetDataResourceForScale: TGetDataResourceForScale);
begin
  inherited Create;
  FGetDataResource := AGetDataResource;
  FGetLocalizedString := AGetLocalizedString;
  FGetDataResourceForScale := AGetDataResourceForScale;
end;

function TCefFastResourceBundle.GetDataResource(resourceId: Integer;
  out data: Pointer; out dataSize: NativeUInt): Boolean;
begin
  if Assigned(FGetDataResource) then
    Result := FGetDataResource(resourceId, data, dataSize) else
    Result := False;
end;

function TCefFastResourceBundle.GetDataResourceForScale(resourceId: Integer;
  scaleFactor: TCefScaleFactor; out data: Pointer;
  dataSize: NativeUInt): Boolean;
begin
  if Assigned(FGetDataResourceForScale) then
    Result := FGetDataResourceForScale(resourceId, scaleFactor, data, dataSize) else
    Result := False;
end;

function TCefFastResourceBundle.GetLocalizedString(stringId: Integer;
  out stringVal: ustring): Boolean;
begin
  if Assigned(FGetLocalizedString) then
    Result := FGetLocalizedString(stringId, stringVal) else
    Result := False;
end;

end.
