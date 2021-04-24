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

unit uCEFResourceBundleHandler;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes, uCEFApplicationCore;

type
  TCefResourceBundleHandlerOwn = class(TCefBaseRefCountedOwn, ICefResourceBundleHandler)
    protected
      function GetLocalizedString(stringid: Integer; var stringVal: ustring): Boolean; virtual; abstract;
      function GetDataResource(resourceId: Integer; var data: Pointer; var dataSize: NativeUInt): Boolean; virtual; abstract;
      function GetDataResourceForScale(resourceId: Integer; scaleFactor: TCefScaleFactor; var data: Pointer; var dataSize: NativeUInt): Boolean; virtual; abstract;

      procedure RemoveReferences; virtual; abstract;

    public
      constructor Create; virtual;
  end;

  TCefCustomResourceBundleHandler = class(TCefResourceBundleHandlerOwn)
    protected
      FCefApp : TCefApplicationCore;

      function GetLocalizedString(stringid: Integer; var stringVal: ustring): Boolean; override;
      function GetDataResource(resourceId: Integer; var data: Pointer; var dataSize: NativeUInt): Boolean; override;
      function GetDataResourceForScale(resourceId: Integer; scaleFactor: TCefScaleFactor; var data: Pointer; var dataSize: NativeUInt): Boolean; override;

      procedure RemoveReferences; override;

    public
      constructor Create(const aCefApp : TCefApplicationCore); reintroduce;
      destructor  Destroy; override;
  end;

implementation

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  uCEFMiscFunctions, uCEFLibFunctions, uCEFConstants;

function cef_resource_bundle_handler_get_localized_string(self       : PCefResourceBundleHandler;
                                                          string_id  : Integer;
                                                          string_val : PCefString): Integer; stdcall;
var
  TempString : ustring;
  TempObject : TObject;
begin
  Result     := Ord(False);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and
     (TempObject is TCefResourceBundleHandlerOwn) then
    begin
      TempString := '';
      Result     := Ord(TCefResourceBundleHandlerOwn(TempObject).GetLocalizedString(string_id, TempString));

      if (string_val <> nil) then
        begin
          CefStringFree(string_val);
          string_val^ := CefStringAlloc(TempString);
        end;
    end;
end;

function cef_resource_bundle_handler_get_data_resource(    self        : PCefResourceBundleHandler;
                                                           resource_id : Integer;
                                                       var data        : Pointer;
                                                       var data_size   : NativeUInt): Integer; stdcall;
var
  TempObject : TObject;
begin
  Result     := Ord(False);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and
     (TempObject is TCefResourceBundleHandlerOwn) then
    Result := Ord(TCefResourceBundleHandlerOwn(TempObject).GetDataResource(resource_id, data, data_size));
end;

function cef_resource_bundle_handler_get_data_resource_for_scale(    self         : PCefResourceBundleHandler;
                                                                     resource_id  : Integer;
                                                                     scale_factor : TCefScaleFactor;
                                                                 var data         : Pointer;
                                                                 var data_size    : NativeUInt): Integer; stdcall;
var
  TempObject : TObject;
begin
  Result     := Ord(False);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and
     (TempObject is TCefResourceBundleHandlerOwn) then
    Result := Ord(TCefResourceBundleHandlerOwn(TempObject).GetDataResourceForScale(resource_id, scale_factor, data, data_size));
end;

constructor TCefResourceBundleHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefResourceBundleHandler));

  with PCefResourceBundleHandler(FData)^ do
    begin
      get_localized_string        := {$IFDEF FPC}@{$ENDIF}cef_resource_bundle_handler_get_localized_string;
      get_data_resource           := {$IFDEF FPC}@{$ENDIF}cef_resource_bundle_handler_get_data_resource;
      get_data_resource_for_scale := {$IFDEF FPC}@{$ENDIF}cef_resource_bundle_handler_get_data_resource_for_scale;
    end;
end;


// TCefCustomResourceBundleHandler


constructor TCefCustomResourceBundleHandler.Create(const aCefApp : TCefApplicationCore);
begin
  inherited Create;

  FCefApp := aCefApp;
end;

destructor TCefCustomResourceBundleHandler.Destroy;
begin
  RemoveReferences;

  inherited Destroy;
end;

procedure TCefCustomResourceBundleHandler.RemoveReferences;
begin
  FCefApp := nil;
end;

function TCefCustomResourceBundleHandler.GetLocalizedString(    stringid  : Integer;
                                                            var stringVal : ustring): Boolean;
begin
  Result := False;

  try
    Result := (FCefApp <> nil) and FCefApp.Internal_GetLocalizedString(stringid, stringVal);
  except
    on e : exception do
      if CustomExceptionHandler('TCefCustomResourceBundleHandler.GetLocalizedString', e) then raise;
  end;
end;

function TCefCustomResourceBundleHandler.GetDataResource(    resourceId : Integer;
                                                         var data       : Pointer;
                                                         var dataSize   : NativeUInt): Boolean;
begin
  Result := False;

  try
    Result := (FCefApp <> nil) and FCefApp.Internal_GetDataResource(resourceId, data, dataSize);
  except
    on e : exception do
      if CustomExceptionHandler('TCefCustomResourceBundleHandler.GetDataResource', e) then raise;
  end;
end;

function TCefCustomResourceBundleHandler.GetDataResourceForScale(    resourceId  : Integer;
                                                                     scaleFactor : TCefScaleFactor;
                                                                 var data        : Pointer;
                                                                 var dataSize    : NativeUInt): Boolean;
begin
  Result := False;

  try
    Result := (FCefApp <> nil) and FCefApp.Internal_GetDataResourceForScale(resourceId, scaleFactor, data, dataSize);
  except
    on e : exception do
      if CustomExceptionHandler('TCefCustomResourceBundleHandler.GetDataResourceForScale', e) then raise;
  end;
end;

end.
