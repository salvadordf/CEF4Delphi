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

unit uCEFExtension;

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
  TCefExtensionRef = class(TCefBaseRefCountedRef, ICefExtension)
    protected
      function  GetIdentifier : ustring;
      function  GetPath : ustring;
      function  GetManifest : ICefDictionaryValue;
      function  IsSame(const that : ICefExtension) : boolean;
      function  GetHandler : ICefExtensionHandler;
      function  GetLoaderContext: ICefRequestContext;
      function  IsLoaded : boolean;
      procedure unload;

    public
      class function UnWrap(data: Pointer): ICefExtension;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFDictionaryValue, uCEFRequestContext, uCEFExtensionHandler;

function TCefExtensionRef.GetIdentifier : ustring;
begin
  Result := CefStringFreeAndGet(PCefExtension(FData)^.get_identifier(PCefExtension(FData)));
end;

function TCefExtensionRef.GetPath : ustring;
begin
  Result := CefStringFreeAndGet(PCefExtension(FData)^.get_path(PCefExtension(FData)));
end;

function TCefExtensionRef.GetManifest : ICefDictionaryValue;
begin
  Result := TCefDictionaryValueRef.UnWrap(PCefExtension(FData)^.get_manifest(PCefExtension(FData)));
end;

function TCefExtensionRef.IsSame(const that : ICefExtension) : boolean;
begin
  Result:= PCefExtension(FData)^.is_same(PCefExtension(FData), PCefExtension(CefGetData(that))) <> 0;
end;

function TCefExtensionRef.GetHandler : ICefExtensionHandler;
begin
  Result := TCefExtensionHandlerRef.UnWrap(PCefExtension(FData)^.get_handler(PCefExtension(FData)));
end;

function TCefExtensionRef.GetLoaderContext: ICefRequestContext;
begin
  Result := TCefRequestContextRef.UnWrap(PCefExtension(FData)^.get_loader_context(PCefExtension(FData)));
end;

function TCefExtensionRef.IsLoaded : boolean;
begin
  Result := PCefExtension(FData)^.is_loaded(PCefExtension(FData)) <> 0;
end;

procedure TCefExtensionRef.unload;
begin
  PCefExtension(FData)^.unload(PCefExtension(FData));
end;

class function TCefExtensionRef.UnWrap(data: Pointer): ICefExtension;
begin
  if (data <> nil) then
    Result := Create(data) as ICefExtension
   else
    Result := nil;
end;

end.
