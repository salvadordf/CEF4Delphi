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

unit uCEFBrowserViewDelegate;

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
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes, uCEFViewDelegate;

type
  TCefBrowserViewDelegateRef = class(TCefViewDelegateRef, ICefBrowserViewDelegate)
    protected
      procedure OnBrowserCreated(const browser_view: ICefBrowserView; const browser: ICefBrowser);
      procedure OnBrowserDestroyed(const browser_view: ICefBrowserView; const browser: ICefBrowser);
      function  GetDelegateForPopupBrowserView(const browser_view: ICefBrowserView; const settings: TCefBrowserSettings; const client: ICefClient; is_devtools: boolean): ICefBrowserViewDelegate;
      function  OnPopupBrowserViewCreated(const browser_view, popup_browser_view: ICefBrowserView; is_devtools: boolean): boolean;

    public
      class function UnWrap(data: Pointer): ICefBrowserViewDelegate;
  end;

implementation

uses
  uCEFLibFunctions, uCEFMiscFunctions;

procedure TCefBrowserViewDelegateRef.OnBrowserCreated(const browser_view : ICefBrowserView;
                                                      const browser      : ICefBrowser);
begin
  PCefBrowserViewDelegate(FData)^.on_browser_created(PCefBrowserViewDelegate(FData),
                                                     CefGetData(browser_view),
                                                     CefGetData(browser));
end;

procedure TCefBrowserViewDelegateRef.OnBrowserDestroyed(const browser_view : ICefBrowserView;
                                                        const browser      : ICefBrowser);
begin
  PCefBrowserViewDelegate(FData)^.on_browser_destroyed(PCefBrowserViewDelegate(FData),
                                                       CefGetData(browser_view),
                                                       CefGetData(browser));
end;

function TCefBrowserViewDelegateRef.GetDelegateForPopupBrowserView(const browser_view : ICefBrowserView;
                                                                   const settings     : TCefBrowserSettings;
                                                                   const client       : ICefClient;
                                                                         is_devtools  : boolean): ICefBrowserViewDelegate;
begin
  Result := UnWrap((PCefBrowserViewDelegate(FData)^.get_delegate_for_popup_browser_view(PCefBrowserViewDelegate(FData),
                                                                                        CefGetData(browser_view),
                                                                                        @settings,
                                                                                        CefGetData(client),
                                                                                        ord(is_devtools))));
end;

function TCefBrowserViewDelegateRef.OnPopupBrowserViewCreated(const browser_view       : ICefBrowserView;
                                                              const popup_browser_view : ICefBrowserView;
                                                                    is_devtools        : boolean): boolean;
begin
  Result := (PCefBrowserViewDelegate(FData)^.on_popup_browser_view_created(PCefBrowserViewDelegate(FData),
                                                                           CefGetData(browser_view),
                                                                           CefGetData(popup_browser_view),
                                                                           ord(is_devtools)) <> 0);
end;

class function TCefBrowserViewDelegateRef.UnWrap(data: Pointer): ICefBrowserViewDelegate;
begin
  if (data <> nil) then
    Result := Create(data) as ICefBrowserViewDelegate
   else
    Result := nil;
end;

end.

