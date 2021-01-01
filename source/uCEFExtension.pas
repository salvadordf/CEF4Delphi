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
      function  GetPopup(const aParent : ustring) : ustring;
      function  GetIcon(const aParent : ustring) : ustring;
      function  GetBrowserActionPopup : ustring;
      function  GetBrowserActionIcon : ustring;
      function  GetPageActionPopup : ustring;
      function  GetPageActionIcon : ustring;
      function  GetOptionsPage : ustring;
      function  GetOptionsUIPage : ustring;
      function  GetBackgroundPage : ustring;
      function  GetURL : ustring;

    public
      class function UnWrap(data: Pointer): ICefExtension;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFDictionaryValue, uCEFRequestContext,
  uCEFExtensionHandler, uCEFJson;

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

function TCefExtensionRef.GetPopup(const aParent : ustring) : ustring;
var
  TempManifest, TempBrowserAction : ICefDictionaryValue;
begin
  Result       := '';
  TempManifest := GetManifest;

  if (TempManifest <> nil) and
     TCEFJson.ReadDictionary(TempManifest, aParent, TempBrowserAction) then
    TCEFJson.ReadString(TempBrowserAction, 'default_popup', Result);
end;

function TCefExtensionRef.GetIcon(const aParent : ustring) : ustring;
var
  TempManifest, TempBrowserAction, TempDefIcon : ICefDictionaryValue;
  TempResult : ustring;
begin
  Result       := '';
  TempManifest := GetManifest;

  if (TempManifest <> nil) and
     TCEFJson.ReadDictionary(TempManifest, aParent, TempBrowserAction) then
    begin
      if TCEFJson.ReadString(TempBrowserAction, 'default_icon', TempResult) then
        Result := TempResult
       else
        if TCEFJson.ReadDictionary(TempManifest, 'default_icon', TempDefIcon) and
           (TCEFJson.ReadString(TempDefIcon, '128', TempResult) or
            TCEFJson.ReadString(TempDefIcon, '48', TempResult) or
            TCEFJson.ReadString(TempDefIcon, '32', TempResult) or
            TCEFJson.ReadString(TempDefIcon, '16', TempResult)) then
          Result := TempResult
    end;
end;

function TCefExtensionRef.GetBrowserActionPopup : ustring;
begin
  Result := GetPopup('browser_action');
end;

function TCefExtensionRef.GetBrowserActionIcon : ustring;
begin
  Result := GetIcon('browser_action');
end;

function TCefExtensionRef.GetPageActionPopup : ustring;
begin
  Result := GetPopup('page_action');
end;

function TCefExtensionRef.GetPageActionIcon : ustring;
begin
  Result := GetIcon('page_action');
end;

function TCefExtensionRef.GetOptionsPage : ustring;
var
  TempManifest : ICefDictionaryValue;
begin
  Result       := '';
  TempManifest := GetManifest;

  if (TempManifest <> nil) then
    TCEFJson.ReadString(TempManifest, 'options_page', Result);
end;

function TCefExtensionRef.GetOptionsUIPage : ustring;
var
  TempManifest, TempOptions : ICefDictionaryValue;
begin
  Result       := '';
  TempManifest := GetManifest;

  if (TempManifest <> nil) and
     TCEFJson.ReadDictionary(TempManifest, 'options_ui', TempOptions) then
    TCEFJson.ReadString(TempOptions, 'page', Result);
end;

function TCefExtensionRef.GetBackgroundPage : ustring;
var
  TempManifest, TempBackground : ICefDictionaryValue;
begin
  Result       := '';
  TempManifest := GetManifest;

  if (TempManifest <> nil) and
     TCEFJson.ReadDictionary(TempManifest, 'background', TempBackground) then
    TCEFJson.ReadString(TempBackground, 'page', Result);
end;

function TCefExtensionRef.GetURL : ustring;
begin
  Result := 'chrome-extension://' + GetIdentifier + '/';
end;

class function TCefExtensionRef.UnWrap(data: Pointer): ICefExtension;
begin
  if (data <> nil) then
    Result := Create(data) as ICefExtension
   else
    Result := nil;
end;

end.
