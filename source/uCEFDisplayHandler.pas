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
//        Copyright © 2018 Salvador Díaz Fau. All rights reserved.
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
  TCefDisplayHandlerOwn = class(TCefBaseRefCountedOwn, ICefDisplayHandler)
    protected
      procedure OnAddressChange(const browser: ICefBrowser; const frame: ICefFrame; const url: ustring); virtual;
      procedure OnTitleChange(const browser: ICefBrowser; const title: ustring); virtual;
      procedure OnFaviconUrlChange(const browser: ICefBrowser; iconUrls: TStrings); virtual;
      procedure OnFullScreenModeChange(const browser: ICefBrowser; fullscreen: Boolean); virtual;
      function  OnTooltip(const browser: ICefBrowser; var text: ustring): Boolean; virtual;
      procedure OnStatusMessage(const browser: ICefBrowser; const value: ustring); virtual;
      function  OnConsoleMessage(const browser: ICefBrowser; level: TCefLogSeverity; const message, source: ustring; line: Integer): Boolean; virtual;
      function  OnAutoResize(const browser: ICefBrowser; const new_size: PCefSize): Boolean; virtual;

      procedure RemoveReferences; virtual;

    public
      constructor Create; virtual;
  end;

  TCustomDisplayHandler = class(TCefDisplayHandlerOwn)
    protected
      FEvents : Pointer;

      procedure OnAddressChange(const browser: ICefBrowser; const frame: ICefFrame; const url: ustring); override;
      procedure OnTitleChange(const browser: ICefBrowser; const title: ustring); override;
      procedure OnFaviconUrlChange(const browser: ICefBrowser; iconUrls: TStrings); override;
      procedure OnFullScreenModeChange(const browser: ICefBrowser; fullscreen: Boolean); override;
      function  OnTooltip(const browser: ICefBrowser; var text: ustring): Boolean; override;
      procedure OnStatusMessage(const browser: ICefBrowser; const value: ustring); override;
      function  OnConsoleMessage(const browser: ICefBrowser; level: TCefLogSeverity; const message, source: ustring; line: Integer): Boolean; override;
      function  OnAutoResize(const browser: ICefBrowser; const new_size: PCefSize): Boolean; override;

      procedure RemoveReferences; override;

    public
      constructor Create(const events: Pointer); reintroduce; virtual;
      destructor  Destroy; override;
  end;

implementation

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBrowser, uCEFFrame;


procedure cef_display_handler_on_address_change(self: PCefDisplayHandler;
                                                browser: PCefBrowser;
                                                frame: PCefFrame;
                                                const url: PCefString); stdcall;
begin
  with TCefDisplayHandlerOwn(CefGetObject(self)) do
    OnAddressChange(
      TCefBrowserRef.UnWrap(browser),
      TCefFrameRef.UnWrap(frame),
      cefstring(url))
end;

procedure cef_display_handler_on_title_change(self: PCefDisplayHandler;
                                              browser: PCefBrowser;
                                              const title: PCefString); stdcall;
begin
  with TCefDisplayHandlerOwn(CefGetObject(self)) do
    OnTitleChange(TCefBrowserRef.UnWrap(browser), CefString(title));
end;

procedure cef_display_handler_on_favicon_urlchange(self      : PCefDisplayHandler;
                                                   browser   : PCefBrowser;
                                                   icon_urls : TCefStringList); stdcall;
var
  TempSL : TStringList;
  i, j : NativeUInt;
  TempString : TCefString;
begin
  TempSL := nil;

  try
    try
      TempSL := TStringList.Create;
      i      := 0;
      j      := cef_string_list_size(icon_urls);

      while (i < j) do
        begin
          FillChar(TempString, SizeOf(TempString), 0);
          cef_string_list_value(icon_urls, i, @TempString);
          TempSL.Add(CefStringClearAndGet(TempString));
          inc(i);
        end;

      TCefDisplayHandlerOwn(CefGetObject(self)).OnFaviconUrlChange(TCefBrowserRef.UnWrap(browser), TempSL);
    except
      on e : exception do
        if CustomExceptionHandler('cef_display_handler_on_favicon_urlchange', e) then raise;
    end;
  finally
    if (TempSL <> nil) then FreeAndNil(TempSL);
  end;
end;

procedure cef_display_handler_on_fullscreen_mode_change(self: PCefDisplayHandler;
                                                        browser: PCefBrowser;
                                                        fullscreen: Integer); stdcall;
begin
  with TCefDisplayHandlerOwn(CefGetObject(self)) do
    OnFullScreenModeChange(TCefBrowserRef.UnWrap(browser), fullscreen <> 0);
end;

function cef_display_handler_on_tooltip(self: PCefDisplayHandler;
                                        browser: PCefBrowser;
                                        text: PCefString): Integer; stdcall;
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
                                                browser: PCefBrowser;
                                                const value: PCefString); stdcall;
begin
  with TCefDisplayHandlerOwn(CefGetObject(self)) do
    OnStatusMessage(TCefBrowserRef.UnWrap(browser), CefString(value));
end;

function cef_display_handler_on_console_message(self: PCefDisplayHandler;
                                                browser: PCefBrowser;
                                                level: TCefLogSeverity;
                                                const message: PCefString;
                                                const source: PCefString;
                                                line: Integer): Integer; stdcall;
begin
  with TCefDisplayHandlerOwn(CefGetObject(self)) do
    Result := Ord(OnConsoleMessage(TCefBrowserRef.UnWrap(browser), level, CefString(message), CefString(source), line));
end;

function cef_display_handler_on_auto_resize(self: PCefDisplayHandler;
                                            browser: PCefBrowser;
                                            const new_size: PCefSize): Integer; stdcall;
begin
  Result := Ord(TCefDisplayHandlerOwn(CefGetObject(self)).OnAutoResize(TCefBrowserRef.UnWrap(browser), new_size));
end;


constructor TCefDisplayHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefDisplayHandler));

  with PCefDisplayHandler(FData)^ do
    begin
      on_address_change         := cef_display_handler_on_address_change;
      on_title_change           := cef_display_handler_on_title_change;
      on_favicon_urlchange      := cef_display_handler_on_favicon_urlchange;
      on_fullscreen_mode_change := cef_display_handler_on_fullscreen_mode_change;
      on_tooltip                := cef_display_handler_on_tooltip;
      on_status_message         := cef_display_handler_on_status_message;
      on_console_message        := cef_display_handler_on_console_message;
      on_auto_resize            := cef_display_handler_on_auto_resize;
    end;
end;

procedure TCefDisplayHandlerOwn.OnAddressChange(const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
begin

end;

function TCefDisplayHandlerOwn.OnConsoleMessage(const browser: ICefBrowser; level: TCefLogSeverity; const message, source: ustring; line: Integer): Boolean;
begin
  Result := False;
end;

function TCefDisplayHandlerOwn.OnAutoResize(const browser: ICefBrowser; const new_size: PCefSize): Boolean;
begin
  Result := False;
end;

procedure TCefDisplayHandlerOwn.OnFaviconUrlChange(const browser: ICefBrowser;
  iconUrls: TStrings);
begin

end;

procedure TCefDisplayHandlerOwn.OnFullScreenModeChange(const browser: ICefBrowser; fullscreen: Boolean);
begin

end;

procedure TCefDisplayHandlerOwn.OnStatusMessage(const browser: ICefBrowser; const value: ustring);
begin

end;

procedure TCefDisplayHandlerOwn.OnTitleChange(const browser: ICefBrowser; const title: ustring);
begin

end;

function TCefDisplayHandlerOwn.OnTooltip(const browser: ICefBrowser; var text: ustring): Boolean;
begin
  Result := False;
end;

procedure TCefDisplayHandlerOwn.RemoveReferences;
begin
  //
end;

// TCustomDisplayHandler

constructor TCustomDisplayHandler.Create(const events: Pointer);
begin
  inherited Create;

  FEvents := events;
end;

destructor TCustomDisplayHandler.Destroy;
begin
  RemoveReferences;

  inherited Destroy;
end;

procedure TCustomDisplayHandler.RemoveReferences;
begin
  FEvents := nil;
end;

procedure TCustomDisplayHandler.OnAddressChange(const browser : ICefBrowser;
                                                const frame   : ICefFrame;
                                                const url     : ustring);
begin
  if (FEvents <> nil) then IChromiumEvents(FEvents).doOnAddressChange(browser, frame, url);
end;

function TCustomDisplayHandler.OnConsoleMessage(const browser : ICefBrowser;
                                                      level   : TCefLogSeverity;
                                                const message : ustring;
                                                const source  : ustring;
                                                      line    : Integer): Boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnConsoleMessage(browser, level, message, source, line)
   else
    Result := inherited OnConsoleMessage(browser, level, message, source, line);
end;

function TCustomDisplayHandler.OnAutoResize(const browser: ICefBrowser; const new_size: PCefSize): Boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnAutoResize(browser, new_size)
   else
    Result := inherited OnAutoResize(browser, new_size);
end;

procedure TCustomDisplayHandler.OnFaviconUrlChange(const browser: ICefBrowser; iconUrls: TStrings);
begin
  if (FEvents <> nil) then IChromiumEvents(FEvents).doOnFaviconUrlChange(browser, iconUrls);
end;

procedure TCustomDisplayHandler.OnFullScreenModeChange(const browser: ICefBrowser; fullscreen: Boolean);
begin
  if (FEvents <> nil) then IChromiumEvents(FEvents).doOnFullScreenModeChange(browser, fullscreen);
end;

procedure TCustomDisplayHandler.OnStatusMessage(const browser: ICefBrowser; const value: ustring);
begin
  if (FEvents <> nil) then IChromiumEvents(FEvents).doOnStatusMessage(browser, value);
end;

procedure TCustomDisplayHandler.OnTitleChange(const browser: ICefBrowser; const title: ustring);
begin
  if (FEvents <> nil) then IChromiumEvents(FEvents).doOnTitleChange(browser, title);
end;

function TCustomDisplayHandler.OnTooltip(const browser: ICefBrowser; var text: ustring): Boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnTooltip(browser, text)
   else
    Result := inherited OnTooltip(browser, text);
end;

end.
