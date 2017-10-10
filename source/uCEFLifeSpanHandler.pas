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

unit uCEFLifeSpanHandler;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefLifeSpanHandlerOwn = class(TCefBaseRefCountedOwn, ICefLifeSpanHandler)
    protected
      function  OnBeforePopup(const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var noJavascriptAccess: Boolean): Boolean; virtual;
      procedure OnAfterCreated(const browser: ICefBrowser); virtual;
      procedure OnBeforeClose(const browser: ICefBrowser); virtual;
      function  DoClose(const browser: ICefBrowser): Boolean; virtual;

    public
      constructor Create; virtual;
  end;

  TCustomLifeSpanHandler = class(TCefLifeSpanHandlerOwn)
    protected
      FEvent: IChromiumEvents;

      function  OnBeforePopup(const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var noJavascriptAccess: Boolean): Boolean; override;
      procedure OnAfterCreated(const browser: ICefBrowser); override;
      procedure OnBeforeClose(const browser: ICefBrowser); override;
      function  DoClose(const browser: ICefBrowser): Boolean; override;

    public
      constructor Create(const events: IChromiumEvents); reintroduce; virtual;
      destructor  Destroy; override;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFClient, uCEFBrowser, uCEFFrame;

function cef_life_span_handler_on_before_popup(      self                 : PCefLifeSpanHandler;
                                                     browser              : PCefBrowser;
                                                     frame                : PCefFrame;
                                               const target_url           : PCefString;
                                               const target_frame_name    : PCefString;
                                                     target_disposition   : TCefWindowOpenDisposition;
                                                     user_gesture         : Integer;
                                               const popupFeatures        : PCefPopupFeatures;
                                                     windowInfo           : PCefWindowInfo;
                                                 var client               : PCefClient;
                                                     settings             : PCefBrowserSettings;
                                                     no_javascript_access : PInteger): Integer; stdcall;
var
  TempURL    : ustring;
  TempFrame  : ustring;
  TempClient : ICefClient;
  TempOldCli : pointer;
  TempNoJS   : Boolean;
begin
  TempURL    := CefString(target_url);
  TempFrame  := CefString(target_frame_name);
  TempNoJS   := (no_javascript_access^ <> 0);
  TempClient := TCefClientRef.UnWrap(client);
  TempOldCli := pointer(TempClient);

  with TCefLifeSpanHandlerOwn(CefGetObject(self)) do
    Result := Ord(OnBeforePopup(TCefBrowserRef.UnWrap(browser),
                                TCefFrameRef.UnWrap(frame),
                                TempURL,
                                TempFrame,
                                target_disposition,
                                user_gesture <> 0,
                                popupFeatures^,
                                windowInfo^,
                                TempClient,
                                settings^,
                                TempNoJS));

  CefStringSet(target_url,        TempURL);
  CefStringSet(target_frame_name, TempFrame);

  no_javascript_access^ := Ord(TempNoJS);

  if (TempClient = nil) then
    client := nil
   else
    if (TempOldCli <> pointer(TempClient)) then
      client := CefGetData(TempClient);
end;

procedure cef_life_span_handler_on_after_created(self: PCefLifeSpanHandler; browser: PCefBrowser); stdcall;
begin
  with TCefLifeSpanHandlerOwn(CefGetObject(self)) do
    OnAfterCreated(TCefBrowserRef.UnWrap(browser));
end;

procedure cef_life_span_handler_on_before_close(self: PCefLifeSpanHandler; browser: PCefBrowser); stdcall;
begin
  with TCefLifeSpanHandlerOwn(CefGetObject(self)) do
    OnBeforeClose(TCefBrowserRef.UnWrap(browser));
end;

function cef_life_span_handler_do_close(self: PCefLifeSpanHandler; browser: PCefBrowser): Integer; stdcall;
begin
  with TCefLifeSpanHandlerOwn(CefGetObject(self)) do
    Result := Ord(DoClose(TCefBrowserRef.UnWrap(browser)));
end;

constructor TCefLifeSpanHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefLifeSpanHandler));

  with PCefLifeSpanHandler(FData)^ do
    begin
      on_before_popup  := cef_life_span_handler_on_before_popup;
      on_after_created := cef_life_span_handler_on_after_created;
      on_before_close  := cef_life_span_handler_on_before_close;
      do_close         := cef_life_span_handler_do_close;
    end;
end;

procedure TCefLifeSpanHandlerOwn.OnAfterCreated(const browser: ICefBrowser);
begin

end;

procedure TCefLifeSpanHandlerOwn.OnBeforeClose(const browser: ICefBrowser);
begin

end;

function TCefLifeSpanHandlerOwn.OnBeforePopup(const browser: ICefBrowser;
  const frame: ICefFrame; const targetUrl, targetFrameName: ustring;
  targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean;
  var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
  var client: ICefClient; var settings: TCefBrowserSettings;
  var noJavascriptAccess: Boolean): Boolean;
begin
  Result := False;
end;

function TCefLifeSpanHandlerOwn.DoClose(const browser: ICefBrowser): Boolean;
begin
  Result := False;
end;

// TCustomLifeSpanHandler

constructor TCustomLifeSpanHandler.Create(const events: IChromiumEvents);
begin
  inherited Create;

  FEvent := events;
end;

destructor TCustomLifeSpanHandler.Destroy;
begin
  FEvent := nil;

  inherited Destroy;
end;

function TCustomLifeSpanHandler.DoClose(const browser: ICefBrowser): Boolean;
begin
  if (FEvent <> nil) then
    Result := FEvent.doOnClose(browser)
   else
    Result := inherited DoClose(browser);
end;

procedure TCustomLifeSpanHandler.OnAfterCreated(const browser: ICefBrowser);
begin
  if (FEvent <> nil) then FEvent.doOnAfterCreated(browser);
end;

procedure TCustomLifeSpanHandler.OnBeforeClose(const browser: ICefBrowser);
begin
  if (FEvent <> nil) then FEvent.doOnBeforeClose(browser);
end;

function TCustomLifeSpanHandler.OnBeforePopup(const browser            : ICefBrowser;
                                              const frame              : ICefFrame;
                                              const targetUrl          : ustring;
                                              const targetFrameName    : ustring;
                                                    targetDisposition  : TCefWindowOpenDisposition;
                                                    userGesture        : Boolean;
                                              var   popupFeatures      : TCefPopupFeatures;
                                              var   windowInfo         : TCefWindowInfo;
                                              var   client             : ICefClient;
                                              var   settings           : TCefBrowserSettings;
                                              var   noJavascriptAccess : Boolean): Boolean;
begin
  if (FEvent <> nil) then
    Result := FEvent.doOnBeforePopup(browser, frame, targetUrl, targetFrameName,
                                     targetDisposition, userGesture, popupFeatures,
                                     windowInfo, client, settings, noJavascriptAccess)
   else
    Result := inherited OnBeforePopup(browser, frame, targetUrl, targetFrameName,
                                      targetDisposition, userGesture, popupFeatures,
                                      windowInfo, client, settings, noJavascriptAccess);
end;

end.
