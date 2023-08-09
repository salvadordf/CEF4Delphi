unit uCEFLoadHandler;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes, uCEFApplicationCore;

type
  TCefLoadHandlerOwn = class(TCefBaseRefCountedOwn, ICefLoadHandler)
    protected
      procedure OnLoadingStateChange(const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean); virtual;
      procedure OnLoadStart(const browser: ICefBrowser; const frame: ICefFrame; transitionType: TCefTransitionType); virtual;
      procedure OnLoadEnd(const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: Integer); virtual;
      procedure OnLoadError(const browser: ICefBrowser; const frame: ICefFrame; errorCode: TCefErrorCode; const errorText, failedUrl: ustring); virtual;

      procedure RemoveReferences; virtual;

    public
      constructor Create; virtual;
  end;

  TCustomLoadHandler = class(TCefLoadHandlerOwn)
    protected
      FEvents : Pointer;

      procedure OnLoadingStateChange(const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean); override;
      procedure OnLoadStart(const browser: ICefBrowser; const frame: ICefFrame; transitionType: TCefTransitionType); override;
      procedure OnLoadEnd(const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: Integer); override;
      procedure OnLoadError(const browser: ICefBrowser; const frame: ICefFrame; errorCode: TCefErrorCode; const errorText, failedUrl: ustring); override;

      procedure RemoveReferences; override;

    public
      constructor Create(const events : IChromiumEvents); reintroduce; virtual;
      destructor  Destroy; override;
  end;

  TCustomRenderLoadHandler = class(TCefLoadHandlerOwn)
    protected
      FCefApp : TCefApplicationCore;

      procedure OnLoadingStateChange(const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean); override;
      procedure OnLoadStart(const browser: ICefBrowser; const frame: ICefFrame; transitionType: TCefTransitionType); override;
      procedure OnLoadEnd(const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: Integer); override;
      procedure OnLoadError(const browser: ICefBrowser; const frame: ICefFrame; errorCode: TCefErrorCode; const errorText, failedUrl: ustring); override;

      procedure RemoveReferences; override;

    public
      constructor Create(const aCefApp : TCefApplicationCore); reintroduce; virtual;
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

procedure cef_load_handler_on_loading_state_change(self         : PCefLoadHandler;
                                                   browser      : PCefBrowser;
                                                   isLoading    : integer;
                                                   canGoBack    : integer;
                                                   canGoForward : Integer); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefLoadHandlerOwn) then
    TCefLoadHandlerOwn(TempObject).OnLoadingStateChange(TCefBrowserRef.UnWrap(browser),
                                                        isLoading <> 0,
                                                        canGoBack <> 0,
                                                        canGoForward <> 0);
end;

procedure cef_load_handler_on_load_start(self            : PCefLoadHandler;
                                         browser         : PCefBrowser;
                                         frame           : PCefFrame;
                                         transition_type : TCefTransitionType); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefLoadHandlerOwn) then
    TCefLoadHandlerOwn(TempObject).OnLoadStart(TCefBrowserRef.UnWrap(browser),
                                               TCefFrameRef.UnWrap(frame),
                                               transition_type);
end;

procedure cef_load_handler_on_load_end(self           : PCefLoadHandler;
                                       browser        : PCefBrowser;
                                       frame          : PCefFrame;
                                       httpStatusCode : Integer); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefLoadHandlerOwn) then
    TCefLoadHandlerOwn(TempObject).OnLoadEnd(TCefBrowserRef.UnWrap(browser),
                                             TCefFrameRef.UnWrap(frame),
                                             httpStatusCode);
end;

procedure cef_load_handler_on_load_error(      self      : PCefLoadHandler;
                                               browser   : PCefBrowser;
                                               frame     : PCefFrame;
                                               errorCode : TCefErrorCode;
                                         const errorText : PCefString;
                                         const failedUrl : PCefString); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefLoadHandlerOwn) then
    TCefLoadHandlerOwn(TempObject).OnLoadError(TCefBrowserRef.UnWrap(browser),
                                               TCefFrameRef.UnWrap(frame),
                                               errorCode,
                                               CefString(errorText),
                                               CefString(failedUrl));
end;

constructor TCefLoadHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefLoadHandler));

  with PCefLoadHandler(FData)^ do
    begin
      on_loading_state_change := {$IFDEF FPC}@{$ENDIF}cef_load_handler_on_loading_state_change;
      on_load_start           := {$IFDEF FPC}@{$ENDIF}cef_load_handler_on_load_start;
      on_load_end             := {$IFDEF FPC}@{$ENDIF}cef_load_handler_on_load_end;
      on_load_error           := {$IFDEF FPC}@{$ENDIF}cef_load_handler_on_load_error;
    end;
end;

procedure TCefLoadHandlerOwn.OnLoadEnd(const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: Integer);
begin
  //
end;

procedure TCefLoadHandlerOwn.OnLoadError(const browser: ICefBrowser; const frame: ICefFrame; errorCode: TCefErrorCode; const errorText, failedUrl: ustring);
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

procedure TCefLoadHandlerOwn.RemoveReferences;
begin
  //
end;

// TCustomLoadHandler

constructor TCustomLoadHandler.Create(const events : IChromiumEvents);
begin
  inherited Create;

  FEvents := Pointer(events);
end;

destructor TCustomLoadHandler.Destroy;
begin
  RemoveReferences;

  inherited Destroy;
end;

procedure TCustomLoadHandler.RemoveReferences;
begin
  FEvents := nil;
end;

procedure TCustomLoadHandler.OnLoadEnd(const browser        : ICefBrowser;
                                       const frame          : ICefFrame;
                                             httpStatusCode : Integer);
begin
  if (FEvents <> nil) then IChromiumEvents(FEvents).doOnLoadEnd(browser, frame, httpStatusCode);
end;

procedure TCustomLoadHandler.OnLoadError(const browser   : ICefBrowser;
                                         const frame     : ICefFrame;
                                               errorCode : TCefErrorCode;
                                         const errorText : ustring;
                                         const failedUrl : ustring);
begin
  if (FEvents <> nil) then IChromiumEvents(FEvents).doOnLoadError(browser, frame, errorCode, errorText, failedUrl);
end;

procedure TCustomLoadHandler.OnLoadingStateChange(const browser      : ICefBrowser;
                                                        isLoading    : Boolean;
                                                        canGoBack    : Boolean;
                                                        canGoForward : Boolean);
begin
  if (FEvents <> nil) then IChromiumEvents(FEvents).doOnLoadingStateChange(browser, isLoading, canGoBack, canGoForward);
end;

procedure TCustomLoadHandler.OnLoadStart(const browser        : ICefBrowser;
                                         const frame          : ICefFrame;
                                               transitionType : TCefTransitionType);
begin
  if (FEvents <> nil) then IChromiumEvents(FEvents).doOnLoadStart(browser, frame, transitionType);
end;


// TCustomRenderLoadHandler

constructor TCustomRenderLoadHandler.Create(const aCefApp : TCefApplicationCore);
begin
  inherited Create;

  FCefApp := aCefApp;
end;

destructor TCustomRenderLoadHandler.Destroy;
begin
  RemoveReferences;

  inherited Destroy;
end;

procedure TCustomRenderLoadHandler.RemoveReferences;
begin
  FCefApp := nil;
end;

procedure TCustomRenderLoadHandler.OnLoadingStateChange(const browser      : ICefBrowser;
                                                              isLoading    : Boolean;
                                                              canGoBack    : Boolean;
                                                              canGoForward : Boolean);
begin
  try
    if (FCefApp <> nil) then
      IApplicationCoreEvents(FCefApp).doOnLoadingStateChange(browser, isLoading, canGoBack, canGoForward);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomRenderLoadHandler.OnLoadingStateChange', e) then raise;
  end;
end;

procedure TCustomRenderLoadHandler.OnLoadStart(const browser        : ICefBrowser;
                                               const frame          : ICefFrame;
                                                     transitionType : TCefTransitionType);
begin
  try
    if (FCefApp <> nil) then
      IApplicationCoreEvents(FCefApp).doOnLoadStart(browser, frame, transitionType);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomRenderLoadHandler.OnLoadStart', e) then raise;
  end;
end;

procedure TCustomRenderLoadHandler.OnLoadEnd(const browser        : ICefBrowser;
                                             const frame          : ICefFrame;
                                                   httpStatusCode : Integer);
begin
  try
    if (FCefApp <> nil) then
      IApplicationCoreEvents(FCefApp).doOnLoadEnd(browser, frame, httpStatusCode);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomRenderLoadHandler.OnLoadEnd', e) then raise;
  end;
end;

procedure TCustomRenderLoadHandler.OnLoadError(const browser   : ICefBrowser;
                                               const frame     : ICefFrame;
                                                     errorCode : TCefErrorCode;
                                               const errorText : ustring;
                                               const failedUrl : ustring);
begin
  try
    if (FCefApp <> nil) then
      IApplicationCoreEvents(FCefApp).doOnLoadError(browser, frame, errorCode, errorText, failedUrl);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomRenderLoadHandler.OnLoadError', e) then raise;
  end;
end;

end.
