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

unit uCEFCookieAccessFilter;

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
  TCefCookieAccessFilterRef = class(TCefBaseRefCountedRef, ICefCookieAccessFilter)
    protected
      function  CanSendCookie(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const cookie: PCefCookie): boolean;
      function  CanSaveCookie(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; const cookie: PCefCookie): boolean;

      procedure RemoveReferences;

    public
      class function UnWrap(data: Pointer): ICefCookieAccessFilter;
  end;

  TCefCookieAccessFilterOwn = class(TCefBaseRefCountedOwn, ICefCookieAccessFilter)
    protected
      function  CanSendCookie(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const cookie: PCefCookie): boolean; virtual;
      function  CanSaveCookie(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; const cookie: PCefCookie): boolean; virtual;

      procedure RemoveReferences; virtual;

    public
      constructor Create; virtual;
  end;

  TCustomCookieAccessFilter = class(TCefCookieAccessFilterOwn)
    protected
      FEvents : Pointer;

      function CanSendCookie(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const cookie: PCefCookie): boolean; override;
      function CanSaveCookie(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; const cookie: PCefCookie): boolean; override;

    public
      constructor Create(const events : IChromiumEvents); reintroduce;
      procedure   BeforeDestruction; override;
      procedure   RemoveReferences; override;
  end;

implementation

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBrowser, uCEFFrame, uCEFRequest, uCefResponse, uCEFRequestCallback;

// TCefCookieAccessFilterOwn

function cef_cookie_access_filter_can_send_cookie(      self    : PCefCookieAccessFilter;
                                                        browser : PCefBrowser;
                                                        frame   : PCefFrame;
                                                        request : PCefRequest;
                                                  const cookie  : PCefCookie): Integer; stdcall;
var
  TempObject : TObject;
begin
  Result     := Ord(True);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefCookieAccessFilterOwn) then
    Result := Ord(TCefCookieAccessFilterOwn(TempObject).CanSendCookie(TCefBrowserRef.UnWrap(browser),
                                                                      TCefFrameRef.UnWrap(frame),
                                                                      TCefRequestRef.UnWrap(request),
                                                                      cookie));
end;

function cef_cookie_access_filter_can_save_cookie(      self     : PCefCookieAccessFilter;
                                                        browser  : PCefBrowser;
                                                        frame    : PCefFrame;
                                                        request  : PCefRequest;
                                                        response : PCefResponse;
                                                  const cookie   : PCefCookie): Integer; stdcall;
var
  TempObject : TObject;
begin
  Result     := Ord(True);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefCookieAccessFilterOwn) then
    Result := Ord(TCefCookieAccessFilterOwn(TempObject).CanSaveCookie(TCefBrowserRef.UnWrap(browser),
                                                                      TCefFrameRef.UnWrap(frame),
                                                                      TCefRequestRef.UnWrap(request),
                                                                      TCefResponseRef.UnWrap(response),
                                                                      cookie));
end;

constructor TCefCookieAccessFilterOwn.Create;
begin
  inherited CreateData(SizeOf(TCefCookieAccessFilter));

  with PCefCookieAccessFilter(FData)^ do
    begin
      can_send_cookie := {$IFDEF FPC}@{$ENDIF}cef_cookie_access_filter_can_send_cookie;
      can_save_cookie := {$IFDEF FPC}@{$ENDIF}cef_cookie_access_filter_can_save_cookie;
    end;
end;

function TCefCookieAccessFilterOwn.CanSendCookie(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const cookie: PCefCookie): boolean;
begin
  Result := True;
end;

function TCefCookieAccessFilterOwn.CanSaveCookie(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; const cookie: PCefCookie): boolean;
begin
  Result := True;
end;

procedure TCefCookieAccessFilterOwn.RemoveReferences;
begin
  //
end;


// TCefCookieAccessFilterRef

class function TCefCookieAccessFilterRef.UnWrap(data: Pointer): ICefCookieAccessFilter;
begin
  if (data <> nil) then
    Result := Create(data) as ICefCookieAccessFilter
   else
    Result := nil;
end;

function TCefCookieAccessFilterRef.CanSendCookie(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const cookie: PCefCookie): boolean;
begin
  Result := (PCefCookieAccessFilter(FData)^.can_send_cookie(PCefCookieAccessFilter(FData),
                                                            CefGetData(browser),
                                                            CefGetData(frame),
                                                            CefGetData(request),
                                                            cookie) <> 0);
end;

function TCefCookieAccessFilterRef.CanSaveCookie(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; const cookie: PCefCookie): boolean;
begin
  Result := (PCefCookieAccessFilter(FData)^.can_save_cookie(PCefCookieAccessFilter(FData),
                                                            CefGetData(browser),
                                                            CefGetData(frame),
                                                            CefGetData(request),
                                                            CefGetData(response),
                                                            cookie) <> 0);
end;

procedure TCefCookieAccessFilterRef.RemoveReferences;
begin
  //
end;


// TCustomCookieAccessFilter


constructor TCustomCookieAccessFilter.Create(const events : IChromiumEvents);
begin
  inherited Create;

  FEvents := Pointer(events);
end;

procedure TCustomCookieAccessFilter.BeforeDestruction;
begin
  FEvents := nil;

  inherited BeforeDestruction;
end;

procedure TCustomCookieAccessFilter.RemoveReferences;
begin
  FEvents := nil;
end;

function TCustomCookieAccessFilter.CanSendCookie(const browser : ICefBrowser;
                                                 const frame   : ICefFrame;
                                                 const request : ICefRequest;
                                                 const cookie  : PCefCookie): boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doCanSendCookie(browser, frame, request, cookie)
   else
    Result := inherited CanSendCookie(browser, frame, request, cookie);
end;

function TCustomCookieAccessFilter.CanSaveCookie(const browser  : ICefBrowser;
                                                 const frame    : ICefFrame;
                                                 const request  : ICefRequest;
                                                 const response : ICefResponse;
                                                 const cookie   : PCefCookie): boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doCanSaveCookie(browser, frame, request, response, cookie)
   else
    Result := inherited CanSaveCookie(browser, frame, request, response, cookie);
end;

end.
