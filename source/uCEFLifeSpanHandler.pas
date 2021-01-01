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

unit uCEFLifeSpanHandler;

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
  TCefLifeSpanHandlerOwn = class(TCefBaseRefCountedOwn, ICefLifeSpanHandler)
    protected
      function  OnBeforePopup(const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean): Boolean; virtual;
      procedure OnAfterCreated(const browser: ICefBrowser); virtual;
      function  DoClose(const browser: ICefBrowser): Boolean; virtual;
      procedure OnBeforeClose(const browser: ICefBrowser); virtual;

      procedure RemoveReferences; virtual;

    public
      constructor Create; virtual;
  end;

  TCustomLifeSpanHandler = class(TCefLifeSpanHandlerOwn)
    protected
      FEvents : Pointer;

      function  OnBeforePopup(const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean): Boolean; override;
      procedure OnAfterCreated(const browser: ICefBrowser); override;
      function  DoClose(const browser: ICefBrowser): Boolean; override;
      procedure OnBeforeClose(const browser: ICefBrowser); override;

      procedure RemoveReferences; override;

    public
      constructor Create(const events : IChromiumEvents); reintroduce; virtual;
      destructor  Destroy; override;
  end;

implementation

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  uCEFMiscFunctions, uCEFLibFunctions, uCEFClient, uCEFBrowser, uCEFFrame, uCEFDictionaryValue;

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
                                                 var extra_info           : PCefDictionaryValue;
                                                     no_javascript_access : PInteger): Integer; stdcall;
var
  TempClient    : ICefClient;
  TempExtraInfo : ICefDictionaryValue;
  TempNoJS      : boolean;
  TempObject    : TObject;
begin
  try
    Result     := Ord(False);
    TempObject := CefGetObject(self);

    if (TempObject <> nil) and (TempObject is TCefLifeSpanHandlerOwn) then
      begin
        TempNoJS      := (no_javascript_access^ <> 0);
        TempClient    := TCefClientRef.UnWrap(client);
        TempExtraInfo := TCefDictionaryValueRef.UnWrap(extra_info);

        Result := Ord(TCefLifeSpanHandlerOwn(TempObject).OnBeforePopup(TCefBrowserRef.UnWrap(browser),
                                                                       TCefFrameRef.UnWrap(frame),
                                                                       CefString(target_url),
                                                                       CefString(target_frame_name),
                                                                       target_disposition,
                                                                       user_gesture <> 0,
                                                                       popupFeatures^,
                                                                       windowInfo^,
                                                                       TempClient,
                                                                       settings^,
                                                                       TempExtraInfo,
                                                                       TempNoJS));

        no_javascript_access^ := Ord(TempNoJS);

        if (TempClient = nil) then
          client := nil
         else
          if not(TempClient.SameAs(client)) then
            client := TempClient.Wrap;

        if (TempExtraInfo = nil) then
          extra_info := nil
         else
          if not(TempExtraInfo.SameAs(extra_info)) then
            extra_info := TempExtraInfo.Wrap;
      end;
  finally
    TempClient    := nil;
    TempExtraInfo := nil;
  end;
end;

procedure cef_life_span_handler_on_after_created(self    : PCefLifeSpanHandler;
                                                 browser : PCefBrowser); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefLifeSpanHandlerOwn) then
    TCefLifeSpanHandlerOwn(TempObject).OnAfterCreated(TCefBrowserRef.UnWrap(browser));
end;

procedure cef_life_span_handler_on_before_close(self    : PCefLifeSpanHandler;
                                                browser : PCefBrowser); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefLifeSpanHandlerOwn) then
    TCefLifeSpanHandlerOwn(TempObject).OnBeforeClose(TCefBrowserRef.UnWrap(browser));
end;

function cef_life_span_handler_do_close(self    : PCefLifeSpanHandler;
                                        browser : PCefBrowser): Integer; stdcall;
var
  TempObject : TObject;
begin
  Result     := Ord(False);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefLifeSpanHandlerOwn) then
    Result := Ord(TCefLifeSpanHandlerOwn(TempObject).DoClose(TCefBrowserRef.UnWrap(browser)));
end;

constructor TCefLifeSpanHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefLifeSpanHandler));

  with PCefLifeSpanHandler(FData)^ do
    begin
      on_before_popup  := {$IFDEF FPC}@{$ENDIF}cef_life_span_handler_on_before_popup;
      on_after_created := {$IFDEF FPC}@{$ENDIF}cef_life_span_handler_on_after_created;
      do_close         := {$IFDEF FPC}@{$ENDIF}cef_life_span_handler_do_close;
      on_before_close  := {$IFDEF FPC}@{$ENDIF}cef_life_span_handler_on_before_close;
    end;
end;

procedure TCefLifeSpanHandlerOwn.OnAfterCreated(const browser: ICefBrowser);
begin
  //
end;

procedure TCefLifeSpanHandlerOwn.OnBeforeClose(const browser: ICefBrowser);
begin
  //
end;

function TCefLifeSpanHandlerOwn.OnBeforePopup(const browser            : ICefBrowser;
                                              const frame              : ICefFrame;
                                              const targetUrl          : ustring;
                                              const targetFrameName    : ustring;
                                                    targetDisposition  : TCefWindowOpenDisposition;
                                                    userGesture        : Boolean;
                                              const popupFeatures      : TCefPopupFeatures;
                                              var   windowInfo         : TCefWindowInfo;
                                              var   client             : ICefClient;
                                              var   settings           : TCefBrowserSettings;
                                              var   extra_info         : ICefDictionaryValue;
                                              var   noJavascriptAccess : Boolean): Boolean;
begin
  Result := False;
end;

function TCefLifeSpanHandlerOwn.DoClose(const browser: ICefBrowser): Boolean;
begin
  Result := False;
end;

procedure TCefLifeSpanHandlerOwn.RemoveReferences;
begin
  //
end;

// TCustomLifeSpanHandler

constructor TCustomLifeSpanHandler.Create(const events : IChromiumEvents);
begin
  inherited Create;

  FEvents := Pointer(events);
end;

destructor TCustomLifeSpanHandler.Destroy;
begin
  RemoveReferences;

  inherited Destroy;
end;

procedure TCustomLifeSpanHandler.RemoveReferences;
begin
  FEvents := nil;
end;

function TCustomLifeSpanHandler.DoClose(const browser: ICefBrowser): Boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnClose(browser)
   else
    Result := inherited DoClose(browser);
end;

procedure TCustomLifeSpanHandler.OnAfterCreated(const browser: ICefBrowser);
begin
  if (FEvents <> nil) then IChromiumEvents(FEvents).doOnAfterCreated(browser);
end;

procedure TCustomLifeSpanHandler.OnBeforeClose(const browser: ICefBrowser);
begin
  if (FEvents <> nil) then IChromiumEvents(FEvents).doOnBeforeClose(browser);
end;

function TCustomLifeSpanHandler.OnBeforePopup(const browser            : ICefBrowser;
                                              const frame              : ICefFrame;
                                              const targetUrl          : ustring;
                                              const targetFrameName    : ustring;
                                                    targetDisposition  : TCefWindowOpenDisposition;
                                                    userGesture        : Boolean;
                                              const popupFeatures      : TCefPopupFeatures;
                                              var   windowInfo         : TCefWindowInfo;
                                              var   client             : ICefClient;
                                              var   settings           : TCefBrowserSettings;
                                              var   extra_info         : ICefDictionaryValue;
                                              var   noJavascriptAccess : Boolean): Boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnBeforePopup(browser, frame, targetUrl, targetFrameName,
                                                       targetDisposition, userGesture, popupFeatures,
                                                       windowInfo, client, settings, extra_info, noJavascriptAccess)
   else
    Result := inherited OnBeforePopup(browser, frame, targetUrl, targetFrameName,
                                      targetDisposition, userGesture, popupFeatures,
                                      windowInfo, client, settings, extra_info, noJavascriptAccess);
end;

end.
