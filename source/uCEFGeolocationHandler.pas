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

unit uCEFGeolocationHandler;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefGeolocationHandlerOwn = class(TCefBaseRefCountedOwn, ICefGeolocationHandler)
    protected
      function OnRequestGeolocationPermission(const browser: ICefBrowser; const requestingUrl: ustring; requestId: Integer; const callback: ICefGeolocationCallback): Boolean; virtual;
      procedure OnCancelGeolocationPermission(const browser: ICefBrowser; requestId: Integer); virtual;

    public
      constructor Create; virtual;
  end;

  TCustomGeolocationHandler = class(TCefGeolocationHandlerOwn)
    protected
      FEvent: IChromiumEvents;

      function  OnRequestGeolocationPermission(const browser: ICefBrowser; const requestingUrl: ustring; requestId: Integer; const callback: ICefGeolocationCallback): Boolean; override;
      procedure OnCancelGeolocationPermission(const browser: ICefBrowser; requestId: Integer); override;

    public
      constructor Create(const events: IChromiumEvents); reintroduce; virtual;
      destructor  Destroy; override;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBrowser, uCEFGeolocationCallback;

function cef_geolocation_handler_on_request_geolocation_permission(self: PCefGeolocationHandler;
  browser: PCefBrowser; const requesting_url: PCefString; request_id: Integer;
  callback: PCefGeolocationCallback): Integer; stdcall;
begin
  with TCefGeolocationHandlerOwn(CefGetObject(self)) do
    Result := Ord(OnRequestGeolocationPermission(TCefBrowserRef.UnWrap(browser), CefString(requesting_url),
      request_id, TCefGeolocationCallbackRef.UnWrap(callback)));
end;

procedure cef_geolocation_handler_on_cancel_geolocation_permission(self: PCefGeolocationHandler;
  browser: PCefBrowser; request_id: Integer); stdcall;
begin
  with TCefGeolocationHandlerOwn(CefGetObject(self)) do
    OnCancelGeolocationPermission(TCefBrowserRef.UnWrap(browser), request_id);
end;

// TCefGeolocationHandlerOwn

constructor TCefGeolocationHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefGeolocationHandler));

  with PCefGeolocationHandler(FData)^ do
    begin
      on_request_geolocation_permission := cef_geolocation_handler_on_request_geolocation_permission;
      on_cancel_geolocation_permission  := cef_geolocation_handler_on_cancel_geolocation_permission;
    end;
end;


function TCefGeolocationHandlerOwn.OnRequestGeolocationPermission(
  const browser: ICefBrowser; const requestingUrl: ustring; requestId: Integer;
  const callback: ICefGeolocationCallback): Boolean;
begin
  Result := False;
end;

procedure TCefGeolocationHandlerOwn.OnCancelGeolocationPermission(const browser: ICefBrowser; requestId: Integer);
begin

end;

// TCustomGeolocationHandler

constructor TCustomGeolocationHandler.Create(const events: IChromiumEvents);
begin
  inherited Create;

  FEvent := events;
end;

destructor TCustomGeolocationHandler.Destroy;
begin
  FEvent := nil;

  inherited Destroy;
end;

procedure TCustomGeolocationHandler.OnCancelGeolocationPermission(const browser: ICefBrowser; requestId: Integer);
begin
  if (FEvent <> nil) then
    FEvent.doOnCancelGeolocationPermission(browser, requestId);
end;

function TCustomGeolocationHandler.OnRequestGeolocationPermission(const browser       : ICefBrowser;
                                                                  const requestingUrl : ustring;
                                                                        requestId     : Integer;
                                                                  const callback      : ICefGeolocationCallback): Boolean;
begin
  if (FEvent <> nil) then
    Result := FEvent.doOnRequestGeolocationPermission(browser, requestingUrl, requestId, callback)
   else
    Result := inherited OnRequestGeolocationPermission(browser, requestingUrl, requestId, callback);
end;

end.
