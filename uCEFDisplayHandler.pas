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

unit uCEFDisplayHandler;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

interface

uses
  System.Classes,
  uCEFBase, uCEFInterfaces, uCEFTypes;

type
  TCefDisplayHandlerOwn = class(TCefBaseOwn, ICefDisplayHandler)
    protected
      procedure OnAddressChange(const browser: ICefBrowser; const frame: ICefFrame; const url: ustring); virtual;
      procedure OnTitleChange(const browser: ICefBrowser; const title: ustring); virtual;
      procedure OnFaviconUrlChange(const browser: ICefBrowser; iconUrls: TStrings); virtual;
      procedure OnFullScreenModeChange(const browser: ICefBrowser; fullscreen: Boolean); virtual;
      function  OnTooltip(const browser: ICefBrowser; var text: ustring): Boolean; virtual;
      procedure OnStatusMessage(const browser: ICefBrowser; const value: ustring); virtual;
      function  OnConsoleMessage(const browser: ICefBrowser; const message, source: ustring; line: Integer): Boolean; virtual;
    public
      constructor Create; virtual;
  end;

  TCustomDisplayHandler = class(TCefDisplayHandlerOwn)
    protected
      FEvent: IChromiumEvents;

      procedure OnAddressChange(const browser: ICefBrowser; const frame: ICefFrame; const url: ustring); override;
      procedure OnTitleChange(const browser: ICefBrowser; const title: ustring); override;
      procedure OnFaviconUrlChange(const browser: ICefBrowser; iconUrls: TStrings); override;
      procedure OnFullScreenModeChange(const browser: ICefBrowser; fullscreen: Boolean); override;
      function  OnTooltip(const browser: ICefBrowser; var text: ustring): Boolean; override;
      procedure OnStatusMessage(const browser: ICefBrowser; const value: ustring); override;
      function  OnConsoleMessage(const browser: ICefBrowser; const message, source: ustring; line: Integer): Boolean; override;

    public
      constructor Create(const events: IChromiumEvents); reintroduce; virtual;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBrowser, uCEFFrame;


procedure cef_display_handler_on_address_change(self: PCefDisplayHandler;
  browser: PCefBrowser; frame: PCefFrame; const url: PCefString); stdcall;
begin
  with TCefDisplayHandlerOwn(CefGetObject(self)) do
    OnAddressChange(
      TCefBrowserRef.UnWrap(browser),
      TCefFrameRef.UnWrap(frame),
      cefstring(url))
end;

procedure cef_display_handler_on_title_change(self: PCefDisplayHandler;
  browser: PCefBrowser; const title: PCefString); stdcall;
begin
  with TCefDisplayHandlerOwn(CefGetObject(self)) do
    OnTitleChange(TCefBrowserRef.UnWrap(browser), CefString(title));
end;

procedure cef_display_handler_on_favicon_urlchange(self: PCefDisplayHandler;
  browser: PCefBrowser; icon_urls: TCefStringList); stdcall;
var
  list: TStringList;
  i: Integer;
  str: TCefString;
begin
  list := TStringList.Create;
  try
    for i := 0 to cef_string_list_size(icon_urls) - 1 do
    begin
      FillChar(str, SizeOf(str), 0);
      cef_string_list_value(icon_urls, i, @str);
      list.Add(CefStringClearAndGet(str));
    end;
    with TCefDisplayHandlerOwn(CefGetObject(self)) do
      OnFaviconUrlChange(TCefBrowserRef.UnWrap(browser), list);
  finally
    list.Free;
  end;
end;

procedure cef_display_handler_on_fullscreen_mode_change(self: PCefDisplayHandler;
  browser: PCefBrowser; fullscreen: Integer); stdcall;
begin
  with TCefDisplayHandlerOwn(CefGetObject(self)) do
    OnFullScreenModeChange(TCefBrowserRef.UnWrap(browser), fullscreen <> 0);
end;

function cef_display_handler_on_tooltip(self: PCefDisplayHandler;
  browser: PCefBrowser; text: PCefString): Integer; stdcall;
var
  t: ustring;
begin
  t := CefStringClearAndGet(text^);
  with TCefDisplayHandlerOwn(CefGetObject(self)) do
    Result := Ord(OnTooltip(
      TCefBrowserRef.UnWrap(browser), t));
  text^ := CefStringAlloc(t);
end;

procedure cef_display_handler_on_status_message(self: PCefDisplayHandler;
  browser: PCefBrowser; const value: PCefString); stdcall;
begin
  with TCefDisplayHandlerOwn(CefGetObject(self)) do
    OnStatusMessage(TCefBrowserRef.UnWrap(browser), CefString(value));
end;

function cef_display_handler_on_console_message(self: PCefDisplayHandler;
    browser: PCefBrowser; const message: PCefString;
    const source: PCefString; line: Integer): Integer; stdcall;
begin
  with TCefDisplayHandlerOwn(CefGetObject(self)) do
    Result := Ord(OnConsoleMessage(TCefBrowserRef.UnWrap(browser),
    CefString(message), CefString(source), line));
end;


constructor TCefDisplayHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefDisplayHandler));
  with PCefDisplayHandler(FData)^ do
  begin
    on_address_change := cef_display_handler_on_address_change;
    on_title_change := cef_display_handler_on_title_change;
    on_favicon_urlchange := cef_display_handler_on_favicon_urlchange;
    on_fullscreen_mode_change := cef_display_handler_on_fullscreen_mode_change;
    on_tooltip := cef_display_handler_on_tooltip;
    on_status_message := cef_display_handler_on_status_message;
    on_console_message := cef_display_handler_on_console_message;
  end;
end;

procedure TCefDisplayHandlerOwn.OnAddressChange(const browser: ICefBrowser;
  const frame: ICefFrame; const url: ustring);
begin

end;

function TCefDisplayHandlerOwn.OnConsoleMessage(const browser: ICefBrowser;
  const message, source: ustring; line: Integer): Boolean;
begin
  Result := False;
end;

procedure TCefDisplayHandlerOwn.OnFaviconUrlChange(const browser: ICefBrowser;
  iconUrls: TStrings);
begin

end;

procedure TCefDisplayHandlerOwn.OnFullScreenModeChange(
  const browser: ICefBrowser; fullscreen: Boolean);
begin

end;

procedure TCefDisplayHandlerOwn.OnStatusMessage(const browser: ICefBrowser;
  const value: ustring);
begin

end;

procedure TCefDisplayHandlerOwn.OnTitleChange(const browser: ICefBrowser;
  const title: ustring);
begin

end;

function TCefDisplayHandlerOwn.OnTooltip(const browser: ICefBrowser;
  var text: ustring): Boolean;
begin
  Result := False;
end;

// TCustomDisplayHandler

constructor TCustomDisplayHandler.Create(const events: IChromiumEvents);
begin
  inherited Create;
  FEvent := events;
end;

procedure TCustomDisplayHandler.OnAddressChange(const browser: ICefBrowser;
  const frame: ICefFrame; const url: ustring);
begin
  FEvent.doOnAddressChange(browser, frame, url);
end;

function TCustomDisplayHandler.OnConsoleMessage(const browser: ICefBrowser;
  const message, source: ustring; line: Integer): Boolean;
begin
  Result := FEvent.doOnConsoleMessage(browser, message, source, line);
end;

procedure TCustomDisplayHandler.OnFaviconUrlChange(const browser: ICefBrowser;
  iconUrls: TStrings);
begin
  FEvent.doOnFaviconUrlChange(browser, iconUrls);
end;

procedure TCustomDisplayHandler.OnFullScreenModeChange(
  const browser: ICefBrowser; fullscreen: Boolean);
begin
  FEvent.doOnFullScreenModeChange(browser, fullscreen);
end;

procedure TCustomDisplayHandler.OnStatusMessage(const browser: ICefBrowser;
  const value: ustring);
begin
  FEvent.doOnStatusMessage(browser, value);
end;

procedure TCustomDisplayHandler.OnTitleChange(const browser: ICefBrowser;
  const title: ustring);
begin
  FEvent.doOnTitleChange(browser, title);
end;

function TCustomDisplayHandler.OnTooltip(const browser: ICefBrowser;
  var text: ustring): Boolean;
begin
  Result := FEvent.doOnTooltip(browser, text);
end;

end.
