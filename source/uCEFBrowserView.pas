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

unit uCEFBrowserView;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes, System.SysUtils,
  {$ELSE}
  Classes, SysUtils,
  {$ENDIF}
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes, uCEFView;

type
  TCefBrowserViewRef = class(TCefViewRef, ICefBrowserView)
    protected
      function  GetBrowser : ICefBrowser;
      procedure SetPreferAccelerators(prefer_accelerators: boolean);

    public
      class function UnWrap(data: Pointer): ICefBrowserView;
      class function CreateBrowserView(const client: ICefClient; const url: ustring; const settings: TCefBrowserSettings; const extra_info: ICefDictionaryValue; const request_context: ICefRequestContext; const delegate: ICefBrowserViewDelegate): ICefBrowserView;
      class function GetForBrowser(const browser: ICefBrowser): ICefBrowserView;
  end;

implementation

uses
  uCEFLibFunctions, uCEFMiscFunctions, uCEFBrowser;

function TCefBrowserViewRef.GetBrowser : ICefBrowser;
begin
  Result := TCefBrowserRef.UnWrap(PCefBrowserView(FData)^.get_browser(PCefBrowserView(FData)));
end;

procedure TCefBrowserViewRef.SetPreferAccelerators(prefer_accelerators: boolean);
begin
  PCefBrowserView(FData)^.set_prefer_accelerators(PCefBrowserView(FData),
                                                  ord(prefer_accelerators));
end;

class function TCefBrowserViewRef.UnWrap(data: Pointer): ICefBrowserView;
begin
  if (data <> nil) then
    Result := Create(data) as ICefBrowserView
   else
    Result := nil;
end;

class function TCefBrowserViewRef.CreateBrowserView(const client          : ICefClient;
                                                    const url             : ustring;
                                                    const settings        : TCefBrowserSettings;
                                                    const extra_info      : ICefDictionaryValue;
                                                    const request_context : ICefRequestContext;
                                                    const delegate        : ICefBrowserViewDelegate): ICefBrowserView;

var
  TempURL         : TCefString;
  TempBrowserView : PCefBrowserView;
begin
  Result := nil;

  if (client <> nil) and (delegate <> nil) then
    begin
      TempURL         := CefString(url);
      TempBrowserView := cef_browser_view_create(CefGetData(client),
                                                 @TempURL,
                                                 @settings,
                                                 CefGetData(extra_info),
                                                 CefGetData(request_context),
                                                 CefGetData(delegate));

      if (TempBrowserView <> nil) then
        Result := Create(TempBrowserView) as ICefBrowserView;
    end;
end;

class function TCefBrowserViewRef.GetForBrowser(const browser: ICefBrowser): ICefBrowserView;
var
  TempBrowserView : PCefBrowserView;
begin
  Result := nil;

  if (browser <> nil) then
    begin
      TempBrowserView := cef_browser_view_get_for_browser(CefGetData(browser));

      if (TempBrowserView <> nil) then
        Result := Create(TempBrowserView) as ICefBrowserView;
    end;
end;

end.

