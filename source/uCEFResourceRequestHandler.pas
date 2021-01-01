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

unit uCEFResourceRequestHandler;

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
  TCefResourceRequestHandlerRef = class(TCefBaseRefCountedRef, ICefResourceRequestHandler)
    protected
      procedure GetCookieAccessFilter(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; var aFilter: ICefCookieAccessFilter);
      function  OnBeforeResourceLoad(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const callback: ICefRequestCallback): TCefReturnValue;
      procedure GetResourceHandler(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; var aResourceHandler : ICefResourceHandler);
      procedure OnResourceRedirect(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; var newUrl: ustring);
      function  OnResourceResponse(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse): Boolean;
      procedure GetResourceResponseFilter(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; var aResponseFilter: ICefResponseFilter);
      procedure OnResourceLoadComplete(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; status: TCefUrlRequestStatus; receivedContentLength: Int64);
      procedure OnProtocolExecution(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; var allowOsExecution: Boolean);

      procedure RemoveReferences;
    public
      class function UnWrap(data: Pointer): ICefResourceRequestHandler;
  end;

  TCefResourceRequestHandlerOwn = class(TCefBaseRefCountedOwn, ICefResourceRequestHandler)
    protected
      procedure GetCookieAccessFilter(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; var aFilter: ICefCookieAccessFilter); virtual;
      function  OnBeforeResourceLoad(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const callback: ICefRequestCallback): TCefReturnValue; virtual;
      procedure GetResourceHandler(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; var aResourceHandler : ICefResourceHandler); virtual;
      procedure OnResourceRedirect(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; var newUrl: ustring); virtual;
      function  OnResourceResponse(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse): Boolean; virtual;
      procedure GetResourceResponseFilter(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; var aResponseFilter: ICefResponseFilter); virtual;
      procedure OnResourceLoadComplete(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; status: TCefUrlRequestStatus; receivedContentLength: Int64); virtual;
      procedure OnProtocolExecution(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; var allowOsExecution: Boolean); virtual;

      procedure RemoveReferences; virtual;

    public
      constructor Create; virtual;
  end;

  TCustomResourceRequestHandler = class(TCefResourceRequestHandlerOwn)
    protected
      FEvents             : Pointer;
      FCookieAccessFilter : ICefCookieAccessFilter;

      procedure GetCookieAccessFilter(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; var aFilter: ICefCookieAccessFilter); override;
      function  OnBeforeResourceLoad(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const callback: ICefRequestCallback): TCefReturnValue; override;
      procedure GetResourceHandler(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; var aResourceHandler : ICefResourceHandler);  override;
      procedure OnResourceRedirect(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; var newUrl: ustring); override;
      function  OnResourceResponse(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse): Boolean; override;
      procedure GetResourceResponseFilter(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; var aResponseFilter: ICefResponseFilter); override;
      procedure OnResourceLoadComplete(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; status: TCefUrlRequestStatus; receivedContentLength: Int64); override;
      procedure OnProtocolExecution(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; var allowOsExecution: Boolean); override;

      procedure InitializeVars;

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
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBrowser, uCEFFrame, uCEFRequest, uCEFRequestCallback,
  uCEFResponse, uCEFResponseFilter, uCEFCookieAccessFilter, uCEFResourceHandler;


// TCefResourceRequestHandlerOwn

function cef_resource_request_handler_get_cookie_access_filter(self    : PCefResourceRequestHandler;
                                                               browser : PCefBrowser;
                                                               frame   : PCefFrame;
                                                               request : PCefRequest): PCefCookieAccessFilter; stdcall;
var
  TempObject             : TObject;
  TempCookieAccessFilter : ICefCookieAccessFilter;
begin
  Result                 := nil;
  TempCookieAccessFilter := nil;
  TempObject             := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefResourceRequestHandlerOwn) then
    try
      TCefResourceRequestHandlerOwn(TempObject).GetCookieAccessFilter(TCefBrowserRef.UnWrap(browser),
                                                                      TCefFrameRef.UnWrap(frame),
                                                                      TCefRequestRef.UnWrap(request),
                                                                      TempCookieAccessFilter);
      Result := CefGetData(TempCookieAccessFilter);
    finally
      TempCookieAccessFilter := nil;
    end;
end;

function cef_resource_request_handler_on_before_resource_load(self     : PCefResourceRequestHandler;
                                                              browser  : PCefBrowser;
                                                              frame    : PCefFrame;
                                                              request  : PCefRequest;
                                                              callback : PCefRequestCallback): TCefReturnValue; stdcall;
var
  TempObject : TObject;
begin
  Result     := RV_CONTINUE;
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefResourceRequestHandlerOwn) then
    Result := TCefResourceRequestHandlerOwn(TempObject).OnBeforeResourceLoad(TCefBrowserRef.UnWrap(browser),
                                                                             TCefFrameRef.UnWrap(frame),
                                                                             TCefRequestRef.UnWrap(request),
                                                                             TcefRequestCallbackRef.UnWrap(callback));
end;

function cef_resource_request_handler_get_resource_handler(self    : PCefResourceRequestHandler;
                                                           browser : PCefBrowser;
                                                           frame   : PCefFrame;
                                                           request : PCefRequest): PCefResourceHandler; stdcall;
var
  TempObject          : TObject;
  TempResourceHandler : ICefResourceHandler;
begin
  Result              := nil;
  TempResourceHandler := nil;
  TempObject          := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefResourceRequestHandlerOwn) then
    try
      TCefResourceRequestHandlerOwn(TempObject).GetResourceHandler(TCefBrowserRef.UnWrap(browser),
                                                                   TCefFrameRef.UnWrap(frame),
                                                                   TCefRequestRef.UnWrap(request),
                                                                   TempResourceHandler);
      Result := CefGetData(TempResourceHandler);
    finally
      TempResourceHandler := nil;
    end;
end;

procedure cef_resource_request_handler_on_resource_redirect(self     : PCefResourceRequestHandler;
                                                            browser  : PCefBrowser;
                                                            frame    : PCefFrame;
                                                            request  : PCefRequest;
                                                            response : PCefResponse;
                                                            new_url  : PCefString); stdcall;
var
  TempURL    : ustring;
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefResourceRequestHandlerOwn) then
    begin
      TempURL := CefStringClearAndGet(new_url);
      TCefResourceRequestHandlerOwn(TempObject).OnResourceRedirect(TCefBrowserRef.UnWrap(browser),
                                                                   TCefFrameRef.UnWrap(frame),
                                                                   TCefRequestRef.UnWrap(request),
                                                                   TCefResponseRef.UnWrap(response),
                                                                   TempURL);
      if (new_url <> nil) then new_url^ := CefStringAlloc(TempURL);
    end;
end;

function cef_resource_request_handler_on_resource_response(self     : PCefResourceRequestHandler;
                                                           browser  : PCefBrowser;
                                                           frame    : PCefFrame;
                                                           request  : PCefRequest;
                                                           response : PCefResponse): Integer; stdcall;
var
  TempObject : TObject;
begin
  Result     := Ord(False);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefResourceRequestHandlerOwn) then
    Result := Ord(TCefResourceRequestHandlerOwn(TempObject).OnResourceResponse(TCefBrowserRef.UnWrap(browser),
                                                                               TCefFrameRef.UnWrap(frame),
                                                                               TCefRequestRef.UnWrap(request),
                                                                               TCefResponseRef.UnWrap(response)));
end;

function cef_resource_request_handler_get_resource_response_filter(self     : PCefResourceRequestHandler;
                                                                   browser  : PCefBrowser;
                                                                   frame    : PCefFrame;
                                                                   request  : PCefRequest;
                                                                   response : PCefResponse): PCefResponseFilter; stdcall;
var
  TempObject         : TObject;
  TempResponseFilter : ICefResponseFilter;
begin
  Result             := nil;
  TempResponseFilter := nil;
  TempObject         := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefResourceRequestHandlerOwn) then
    try
      TCefResourceRequestHandlerOwn(TempObject).GetResourceResponseFilter(TCefBrowserRef.UnWrap(browser),
                                                                          TCefFrameRef.UnWrap(frame),
                                                                          TCefRequestRef.UnWrap(request),
                                                                          TCefResponseRef.UnWrap(response),
                                                                          TempResponseFilter);
      Result := CefGetData(TempResponseFilter);
    finally
      TempResponseFilter := nil;
    end;
end;

procedure cef_resource_request_handler_on_resource_load_complete(self                    : PCefResourceRequestHandler;
                                                                 browser                 : PCefBrowser;
                                                                 frame                   : PCefFrame;
                                                                 request                 : PCefRequest;
                                                                 response                : PCefResponse;
                                                                 status                  : TCefUrlRequestStatus;
                                                                 received_content_length : Int64); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefResourceRequestHandlerOwn) then
    TCefResourceRequestHandlerOwn(TempObject).OnResourceLoadComplete(TCefBrowserRef.UnWrap(browser),
                                                                     TCefFrameRef.UnWrap(frame),
                                                                     TCefRequestRef.UnWrap(request),
                                                                     TCefResponseRef.UnWrap(response),
                                                                     status,
                                                                     received_content_length);
end;

procedure cef_resource_request_handler_on_protocol_execution(self               : PCefResourceRequestHandler;
                                                             browser            : PCefBrowser;
                                                             frame              : PCefFrame;
                                                             request            : PCefRequest;
                                                             allow_os_execution : PInteger); stdcall;
var
  TempAllow  : Boolean;
  TempObject : TObject;
begin
  TempAllow  := allow_os_execution^ <> 0;
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefResourceRequestHandlerOwn) then
    TCefResourceRequestHandlerOwn(TempObject).OnProtocolExecution(TCefBrowserRef.UnWrap(browser),
                                                                  TCefFrameRef.UnWrap(frame),
                                                                  TCefRequestRef.UnWrap(request),
                                                                  TempAllow);

  allow_os_execution^ := Ord(TempAllow);
end;

constructor TCefResourceRequestHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefResourceRequestHandler));

  with PCefResourceRequestHandler(FData)^ do
    begin
      get_cookie_access_filter      := {$IFDEF FPC}@{$ENDIF}cef_resource_request_handler_get_cookie_access_filter;
      on_before_resource_load       := {$IFDEF FPC}@{$ENDIF}cef_resource_request_handler_on_before_resource_load;
      get_resource_handler          := {$IFDEF FPC}@{$ENDIF}cef_resource_request_handler_get_resource_handler;
      on_resource_redirect          := {$IFDEF FPC}@{$ENDIF}cef_resource_request_handler_on_resource_redirect;
      on_resource_response          := {$IFDEF FPC}@{$ENDIF}cef_resource_request_handler_on_resource_response;
      get_resource_response_filter  := {$IFDEF FPC}@{$ENDIF}cef_resource_request_handler_get_resource_response_filter;
      on_resource_load_complete     := {$IFDEF FPC}@{$ENDIF}cef_resource_request_handler_on_resource_load_complete;
      on_protocol_execution         := {$IFDEF FPC}@{$ENDIF}cef_resource_request_handler_on_protocol_execution;
    end;
end;

procedure TCefResourceRequestHandlerOwn.GetCookieAccessFilter(const browser : ICefBrowser;
                                                              const frame   : ICefFrame;
                                                              const request : ICefRequest;
                                                              var   aFilter : ICefCookieAccessFilter);
begin
  aFilter := nil;
end;

function TCefResourceRequestHandlerOwn.OnBeforeResourceLoad(const browser  : ICefBrowser;
                                                            const frame    : ICefFrame;
                                                            const request  : ICefRequest;
                                                            const callback : ICefRequestCallback): TCefReturnValue;
begin
  Result := RV_CONTINUE;
end;

procedure TCefResourceRequestHandlerOwn.GetResourceHandler(const browser          : ICefBrowser;
                                                           const frame            : ICefFrame;
                                                           const request          : ICefRequest;
                                                           var   aResourceHandler : ICefResourceHandler);
begin
  aResourceHandler := nil;
end;

procedure TCefResourceRequestHandlerOwn.OnResourceRedirect(const browser  : ICefBrowser;
                                                           const frame    : ICefFrame;
                                                           const request  : ICefRequest;
                                                           const response : ICefResponse;
                                                           var   newUrl   : ustring);
begin
  //
end;

function TCefResourceRequestHandlerOwn.OnResourceResponse(const browser  : ICefBrowser;
                                                          const frame    : ICefFrame;
                                                          const request  : ICefRequest;
                                                          const response : ICefResponse): Boolean;
begin
  Result := False;
end;

procedure TCefResourceRequestHandlerOwn.GetResourceResponseFilter(const browser         : ICefBrowser;
                                                                  const frame           : ICefFrame;
                                                                  const request         : ICefRequest;
                                                                  const response        : ICefResponse;
                                                                  var   aResponseFilter : ICefResponseFilter);
begin
  aResponseFilter := nil;
end;

procedure TCefResourceRequestHandlerOwn.OnResourceLoadComplete(const browser               : ICefBrowser;
                                                               const frame                 : ICefFrame;
                                                               const request               : ICefRequest;
                                                               const response              : ICefResponse;
                                                                     status                : TCefUrlRequestStatus;
                                                                     receivedContentLength : Int64);
begin
  //
end;

procedure TCefResourceRequestHandlerOwn.OnProtocolExecution(const browser          : ICefBrowser;
                                                            const frame            : ICefFrame;
                                                            const request          : ICefRequest;
                                                            var   allowOsExecution : Boolean);
begin
  //
end;

procedure TCefResourceRequestHandlerOwn.RemoveReferences;
begin
  //
end;


// TCefResourceRequestHandlerRef

class function TCefResourceRequestHandlerRef.UnWrap(data: Pointer): ICefResourceRequestHandler;
begin
  if (data <> nil) then
    Result := Create(data) as ICefResourceRequestHandler
   else
    Result := nil;
end;

procedure TCefResourceRequestHandlerRef.GetCookieAccessFilter(const browser : ICefBrowser;
                                                              const frame   : ICefFrame;
                                                              const request : ICefRequest;
                                                              var   aFilter : ICefCookieAccessFilter);
var
  TempCookieAccessFilter : PCefCookieAccessFilter;
begin
  TempCookieAccessFilter := PCefResourceRequestHandler(FData)^.get_cookie_access_filter(PCefResourceRequestHandler(FData),
                                                                                        CefGetData(browser),
                                                                                        CefGetData(frame),
                                                                                        CefGetData(request));

  if (TempCookieAccessFilter <> nil) then
    aFilter := TCefCookieAccessFilterRef.UnWrap(TempCookieAccessFilter)
   else
    aFilter := nil;
end;

function TCefResourceRequestHandlerRef.OnBeforeResourceLoad(const browser  : ICefBrowser;
                                                            const frame    : ICefFrame;
                                                            const request  : ICefRequest;
                                                            const callback : ICefRequestCallback): TCefReturnValue;
begin
  Result := PCefResourceRequestHandler(FData)^.on_before_resource_load(PCefResourceRequestHandler(FData),
                                                                       CefGetData(browser),
                                                                       CefGetData(frame),
                                                                       CefGetData(request),
                                                                       CefGetData(callback));
end;

procedure TCefResourceRequestHandlerRef.GetResourceHandler(const browser          : ICefBrowser;
                                                           const frame            : ICefFrame;
                                                           const request          : ICefRequest;
                                                           var   aResourceHandler : ICefResourceHandler);
var
  TempResourceHandler : PCefResourceHandler;
begin
  TempResourceHandler := PCefResourceRequestHandler(FData)^.get_resource_handler(PCefResourceRequestHandler(FData),
                                                                                 CefGetData(browser),
                                                                                 CefGetData(frame),
                                                                                 CefGetData(request));

  if (TempResourceHandler <> nil) then
    aResourceHandler := TCefResourceHandlerRef.UnWrap(TempResourceHandler)
   else
    aResourceHandler := nil;
end;

procedure TCefResourceRequestHandlerRef.OnResourceRedirect(const browser  : ICefBrowser;
                                                           const frame    : ICefFrame;
                                                           const request  : ICefRequest;
                                                           const response : ICefResponse;
                                                           var   newUrl   : ustring);
var
  TempNewURL : TCefString;
begin
  TempNewURL := CefString(newUrl);
  PCefResourceRequestHandler(FData)^.on_resource_redirect(PCefResourceRequestHandler(FData),
                                                          CefGetData(browser),
                                                          CefGetData(frame),
                                                          CefGetData(request),
                                                          CefGetData(response),
                                                          @TempNewURL);
  newUrl := CefString(@TempNewURL);
end;

function TCefResourceRequestHandlerRef.OnResourceResponse(const browser  : ICefBrowser;
                                                          const frame    : ICefFrame;
                                                          const request  : ICefRequest;
                                                          const response : ICefResponse): Boolean;
begin
  Result := (PCefResourceRequestHandler(FData)^.on_resource_response(PCefResourceRequestHandler(FData),
                                                                     CefGetData(browser),
                                                                     CefGetData(frame),
                                                                     CefGetData(request),
                                                                     CefGetData(response)) <> 0);
end;

procedure TCefResourceRequestHandlerRef.GetResourceResponseFilter(const browser         : ICefBrowser;
                                                                  const frame           : ICefFrame;
                                                                  const request         : ICefRequest;
                                                                  const response        : ICefResponse;
                                                                  var   aResponseFilter : ICefResponseFilter);
var
  TempResponseFilter : PCefResponseFilter;
begin
  TempResponseFilter := PCefResourceRequestHandler(FData)^.get_resource_response_filter(PCefResourceRequestHandler(FData),
                                                                                        CefGetData(browser),
                                                                                        CefGetData(frame),
                                                                                        CefGetData(request),
                                                                                        CefGetData(response));
  if (TempResponseFilter <> nil) then
    aResponseFilter := TCefResponseFilterRef.UnWrap(TempResponseFilter)
   else
    aResponseFilter := nil;
end;

procedure TCefResourceRequestHandlerRef.OnResourceLoadComplete(const browser               : ICefBrowser;
                                                               const frame                 : ICefFrame;
                                                               const request               : ICefRequest;
                                                               const response              : ICefResponse;
                                                                     status                : TCefUrlRequestStatus;
                                                                     receivedContentLength : Int64);
begin
  PCefResourceRequestHandler(FData)^.on_resource_load_complete(PCefResourceRequestHandler(FData),
                                                               CefGetData(browser),
                                                               CefGetData(frame),
                                                               CefGetData(request),
                                                               CefGetData(response),
                                                               status,
                                                               receivedContentLength);
end;

procedure TCefResourceRequestHandlerRef.OnProtocolExecution(const browser          : ICefBrowser;
                                                            const frame            : ICefFrame;
                                                            const request          : ICefRequest;
                                                            var   allowOsExecution : Boolean);
var
  TempAllow : integer;
begin
  TempAllow := ord(allowOsExecution);
  PCefResourceRequestHandler(FData)^.on_protocol_execution(PCefResourceRequestHandler(FData),
                                                           CefGetData(browser),
                                                           CefGetData(frame),
                                                           CefGetData(request),
                                                           @TempAllow);
  allowOsExecution := TempAllow <> 0;
end;

procedure TCefResourceRequestHandlerRef.RemoveReferences;
begin
  //
end;


// TCustomResourceRequestHandler

constructor TCustomResourceRequestHandler.Create(const events : IChromiumEvents);
begin
  inherited Create;

  InitializeVars;

  FEvents := Pointer(events);

  if (events <> nil) and events.MustCreateCookieAccessFilter then
    FCookieAccessFilter := TCustomCookieAccessFilter.Create(events);
end;

procedure TCustomResourceRequestHandler.BeforeDestruction;
begin
  InitializeVars;

  inherited BeforeDestruction;
end;

procedure TCustomResourceRequestHandler.RemoveReferences;
begin
  FEvents := nil;

  if (FCookieAccessFilter <> nil) then FCookieAccessFilter.RemoveReferences;
end;

procedure TCustomResourceRequestHandler.InitializeVars;
begin
  FCookieAccessFilter := nil;
  FEvents             := nil;
end;

procedure TCustomResourceRequestHandler.GetCookieAccessFilter(const browser : ICefBrowser;
                                                              const frame   : ICefFrame;
                                                              const request : ICefRequest;
                                                              var   aFilter : ICefCookieAccessFilter);
begin
  if (FCookieAccessFilter <> nil) then
    aFilter := FCookieAccessFilter
   else
    aFilter := nil;
end;

function TCustomResourceRequestHandler.OnBeforeResourceLoad(const browser  : ICefBrowser;
                                                            const frame    : ICefFrame;
                                                            const request  : ICefRequest;
                                                            const callback : ICefRequestCallback): TCefReturnValue;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnBeforeResourceLoad(browser, frame, request, callback)
   else
    Result := inherited OnBeforeResourceLoad(browser, frame, request, callback);
end;

procedure TCustomResourceRequestHandler.GetResourceHandler(const browser          : ICefBrowser;
                                                           const frame            : ICefFrame;
                                                           const request          : ICefRequest;
                                                           var   aResourceHandler : ICefResourceHandler);
begin
  if (FEvents <> nil) then
    IChromiumEvents(FEvents).doOnGetResourceHandler(browser, frame, request, aResourceHandler)
   else
    inherited GetResourceHandler(browser, frame, request, aResourceHandler);
end;

procedure TCustomResourceRequestHandler.OnResourceRedirect(const browser  : ICefBrowser;
                                                           const frame    : ICefFrame;
                                                           const request  : ICefRequest;
                                                           const response : ICefResponse;
                                                           var   newUrl   : ustring);
begin
  if (FEvents <> nil) then
    IChromiumEvents(FEvents).doOnResourceRedirect(browser, frame, request, response, newUrl);
end;

function TCustomResourceRequestHandler.OnResourceResponse(const browser  : ICefBrowser;
                                                          const frame    : ICefFrame;
                                                          const request  : ICefRequest;
                                                          const response : ICefResponse): Boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnResourceResponse(browser, frame, request, response)
   else
    Result := inherited OnResourceResponse(browser, frame, request, response);
end;

procedure TCustomResourceRequestHandler.GetResourceResponseFilter(const browser         : ICefBrowser;
                                                                  const frame           : ICefFrame;
                                                                  const request         : ICefRequest;
                                                                  const response        : ICefResponse;
                                                                  var   aResponseFilter : ICefResponseFilter);
begin
  if (FEvents <> nil) then
    IChromiumEvents(FEvents).doOnGetResourceResponseFilter(browser, frame, request, response, aResponseFilter)
   else
    inherited GetResourceResponseFilter(browser, frame, request, response, aResponseFilter);
end;

procedure TCustomResourceRequestHandler.OnResourceLoadComplete(const browser               : ICefBrowser;
                                                               const frame                 : ICefFrame;
                                                               const request               : ICefRequest;
                                                               const response              : ICefResponse;
                                                                     status                : TCefUrlRequestStatus;
                                                                     receivedContentLength : Int64);
begin
  if (FEvents <> nil) then
    IChromiumEvents(FEvents).doOnResourceLoadComplete(browser, frame, request, response, status, receivedContentLength);
end;

procedure TCustomResourceRequestHandler.OnProtocolExecution(const browser          : ICefBrowser;
                                                            const frame            : ICefFrame;
                                                            const request          : ICefRequest;
                                                            var   allowOsExecution : Boolean);
begin
  if (FEvents <> nil) then
    IChromiumEvents(FEvents).doOnProtocolExecution(browser, frame, request, allowOsExecution);
end;



end.
