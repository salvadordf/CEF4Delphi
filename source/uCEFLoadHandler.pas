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

unit uCEFLoadHandler;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefLoadHandlerOwn = class(TCefBaseRefCountedOwn, ICefLoadHandler)
    protected
      procedure OnLoadingStateChange(const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean); virtual;
      procedure OnLoadStart(const browser: ICefBrowser; const frame: ICefFrame; transitionType: TCefTransitionType); virtual;
      procedure OnLoadEnd(const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: Integer); virtual;
      procedure OnLoadError(const browser: ICefBrowser; const frame: ICefFrame; errorCode: Integer; const errorText, failedUrl: ustring); virtual;

    public
      constructor Create; virtual;
  end;

  TCustomLoadHandler = class(TCefLoadHandlerOwn)
    protected
      FEvent: IChromiumEvents;

      procedure OnLoadingStateChange(const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean); override;
      procedure OnLoadStart(const browser: ICefBrowser; const frame: ICefFrame; transitionType: TCefTransitionType); override;
      procedure OnLoadEnd(const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: Integer); override;
      procedure OnLoadError(const browser: ICefBrowser; const frame: ICefFrame; errorCode: Integer; const errorText, failedUrl: ustring); override;

    public
      constructor Create(const events: IChromiumEvents); reintroduce; virtual;
      destructor  Destroy; override;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBrowser, uCEFFrame;

procedure cef_load_handler_on_loading_state_change(self: PCefLoadHandler; browser: PCefBrowser; isLoading, canGoBack, canGoForward: Integer); stdcall;
begin
  with TCefLoadHandlerOwn(CefGetObject(self)) do
    OnLoadingStateChange(TCefBrowserRef.UnWrap(browser), isLoading <> 0, canGoBack <> 0, canGoForward <> 0);
end;

procedure cef_load_handler_on_load_start(self: PCefLoadHandler; browser: PCefBrowser; frame: PCefFrame; transition_type: TCefTransitionType); stdcall;
begin
  with TCefLoadHandlerOwn(CefGetObject(self)) do
    OnLoadStart(TCefBrowserRef.UnWrap(browser), TCefFrameRef.UnWrap(frame), transition_type);
end;

procedure cef_load_handler_on_load_end(self: PCefLoadHandler; browser: PCefBrowser; frame: PCefFrame; httpStatusCode: Integer); stdcall;
begin
  with TCefLoadHandlerOwn(CefGetObject(self)) do
    OnLoadEnd(TCefBrowserRef.UnWrap(browser), TCefFrameRef.UnWrap(frame), httpStatusCode);
end;

procedure cef_load_handler_on_load_error(self: PCefLoadHandler; browser: PCefBrowser; frame: PCefFrame; errorCode: Integer; const errorText, failedUrl: PCefString); stdcall;
begin
  with TCefLoadHandlerOwn(CefGetObject(self)) do
    OnLoadError(TCefBrowserRef.UnWrap(browser), TCefFrameRef.UnWrap(frame), errorCode, CefString(errorText), CefString(failedUrl));
end;

constructor TCefLoadHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefLoadHandler));

  with PCefLoadHandler(FData)^ do
    begin
      on_loading_state_change := cef_load_handler_on_loading_state_change;
      on_load_start           := cef_load_handler_on_load_start;
      on_load_end             := cef_load_handler_on_load_end;
      on_load_error           := cef_load_handler_on_load_error;
    end;
end;

procedure TCefLoadHandlerOwn.OnLoadEnd(const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: Integer);
begin
  //
end;

procedure TCefLoadHandlerOwn.OnLoadError(const browser: ICefBrowser; const frame: ICefFrame; errorCode: Integer; const errorText, failedUrl: ustring);
begin
  //
end;

procedure TCefLoadHandlerOwn.OnLoadingStateChange(const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
begin
  //
end;

procedure TCefLoadHandlerOwn.OnLoadStart(const browser: ICefBrowser; const frame: ICefFrame; transitionType: TCefTransitionType);
begin
  //
end;

// TCustomLoadHandler

constructor TCustomLoadHandler.Create(const events: IChromiumEvents);
begin
  inherited Create;

  FEvent := events;
end;

destructor TCustomLoadHandler.Destroy;
begin
  FEvent := nil;

  inherited Destroy;
end;

procedure TCustomLoadHandler.OnLoadEnd(const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: Integer);
begin
  if (FEvent <> nil) then FEvent.doOnLoadEnd(browser, frame, httpStatusCode);
end;

procedure TCustomLoadHandler.OnLoadError(const browser: ICefBrowser; const frame: ICefFrame; errorCode: Integer; const errorText, failedUrl: ustring);
begin
  if (FEvent <> nil) then FEvent.doOnLoadError(browser, frame, errorCode, errorText, failedUrl);
end;

procedure TCustomLoadHandler.OnLoadingStateChange(const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
begin
  if (FEvent <> nil) then FEvent.doOnLoadingStateChange(browser, isLoading, canGoBack, canGoForward);
end;

procedure TCustomLoadHandler.OnLoadStart(const browser: ICefBrowser; const frame: ICefFrame; transitionType: TCefTransitionType);
begin
  if (FEvent <> nil) then FEvent.doOnLoadStart(browser, frame, transitionType);
end;

end.
