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

unit uCEFFocusHandler;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefFocusHandlerOwn = class(TCefBaseRefCountedOwn, ICefFocusHandler)
    protected
      procedure OnTakeFocus(const browser: ICefBrowser; next: Boolean); virtual;
      function OnSetFocus(const browser: ICefBrowser; source: TCefFocusSource): Boolean; virtual;
      procedure OnGotFocus(const browser: ICefBrowser); virtual;

    public
      constructor Create; virtual;
  end;

  TCustomFocusHandler = class(TCefFocusHandlerOwn)
    protected
      FEvent: IChromiumEvents;

      procedure OnTakeFocus(const browser: ICefBrowser; next: Boolean); override;
      function OnSetFocus(const browser: ICefBrowser; source: TCefFocusSource): Boolean; override;
      procedure OnGotFocus(const browser: ICefBrowser); override;

    public
      constructor Create(const events: IChromiumEvents); reintroduce; virtual;
      destructor  Destroy; override;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBrowser;

procedure cef_focus_handler_on_take_focus(self: PCefFocusHandler; browser: PCefBrowser; next: Integer); stdcall;
begin
  with TCefFocusHandlerOwn(CefGetObject(self)) do
    OnTakeFocus(TCefBrowserRef.UnWrap(browser), next <> 0);
end;

function cef_focus_handler_on_set_focus(self: PCefFocusHandler; browser: PCefBrowser; source: TCefFocusSource): Integer; stdcall;
begin
  with TCefFocusHandlerOwn(CefGetObject(self)) do
    Result := Ord(OnSetFocus(TCefBrowserRef.UnWrap(browser), source))
end;

procedure cef_focus_handler_on_got_focus(self: PCefFocusHandler; browser: PCefBrowser); stdcall;
begin
  with TCefFocusHandlerOwn(CefGetObject(self)) do
    OnGotFocus(TCefBrowserRef.UnWrap(browser));
end;

constructor TCefFocusHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefFocusHandler));
  with PCefFocusHandler(FData)^ do
  begin
    on_take_focus := cef_focus_handler_on_take_focus;
    on_set_focus := cef_focus_handler_on_set_focus;
    on_got_focus := cef_focus_handler_on_got_focus;
  end;
end;

function TCefFocusHandlerOwn.OnSetFocus(const browser: ICefBrowser; source: TCefFocusSource): Boolean;
begin
  Result := False;
end;

procedure TCefFocusHandlerOwn.OnGotFocus(const browser: ICefBrowser);
begin
  //
end;

procedure TCefFocusHandlerOwn.OnTakeFocus(const browser: ICefBrowser; next: Boolean);
begin
  //
end;

// TCustomFocusHandler

constructor TCustomFocusHandler.Create(const events: IChromiumEvents);
begin
  inherited Create;

  FEvent := events;
end;

destructor TCustomFocusHandler.Destroy;
begin
  FEvent := nil;

  inherited Destroy;
end;

procedure TCustomFocusHandler.OnGotFocus(const browser: ICefBrowser);
begin
  if (FEvent <> nil) then FEvent.doOnGotFocus(browser);
end;

function TCustomFocusHandler.OnSetFocus(const browser: ICefBrowser; source: TCefFocusSource): Boolean;
begin
  if (FEvent <> nil) then
    Result := FEvent.doOnSetFocus(browser, source)
   else
    Result := inherited OnSetFocus(browser, source);
end;

procedure TCustomFocusHandler.OnTakeFocus(const browser: ICefBrowser; next: Boolean);
begin
  if (FEvent <> nil) then FEvent.doOnTakeFocus(browser, next);
end;



end.

