unit uCEFDisplayHandler;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes,
  {$ELSE}
  Classes,
  {$ENDIF}
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  /// <summary>
  /// Event handler related to browser display state.
  /// The functions of this interface will be called on the UI thread.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_display_handler_capi.h">CEF source file: /include/capi/cef_display_handler_capi.h (cef_display_handler_t)</see></para>
  /// </remarks>
  TCefDisplayHandlerOwn = class(TCefBaseRefCountedOwn, ICefDisplayHandler)
    protected
      /// <summary>
      /// Called when a frame's address has changed.
      /// </summary>
      procedure OnAddressChange(const browser: ICefBrowser; const frame: ICefFrame; const url: ustring); virtual;
      /// <summary>
      /// Called when the page title changes.
      /// </summary>
      procedure OnTitleChange(const browser: ICefBrowser; const title: ustring); virtual;
      /// <summary>
      /// Called when the page icon changes.
      /// </summary>
      procedure OnFaviconUrlChange(const browser: ICefBrowser; const iconUrls: TStrings); virtual;
      /// <summary>
      /// Called when web content in the page has toggled fullscreen mode. If
      /// |fullscreen| is true (1) the content will automatically be sized to fill
      /// the browser content area. If |fullscreen| is false (0) the content will
      /// automatically return to its original size and position. With Alloy style
      /// the client is responsible for triggering the fullscreen transition (for
      /// example, by calling ICefWindow.SetFullscreen when using Views). With
      /// Chrome style the fullscreen transition will be triggered automatically.
      /// The ICefWindowDelegate.OnWindowFullscreenTransition function will be
      /// called during the fullscreen transition for notification purposes.
      /// </summary>
      procedure OnFullScreenModeChange(const browser: ICefBrowser; fullscreen: Boolean); virtual;
      /// <summary>
      /// Called when the browser is about to display a tooltip. |text| contains the
      /// text that will be displayed in the tooltip. To handle the display of the
      /// tooltip yourself return true (1). Otherwise, you can optionally modify
      /// |text| and then return false (0) to allow the browser to display the
      /// tooltip. When window rendering is disabled the application is responsible
      /// for drawing tooltips and the return value is ignored.
      /// </summary>
      function  OnTooltip(const browser: ICefBrowser; var text: ustring): Boolean; virtual;
      /// <summary>
      /// Called when the browser receives a status message. |value| contains the
      /// text that will be displayed in the status message.
      /// </summary>
      procedure OnStatusMessage(const browser: ICefBrowser; const value: ustring); virtual;
      /// <summary>
      /// Called to display a console message. Return true (1) to stop the message
      /// from being output to the console.
      /// </summary>
      function  OnConsoleMessage(const browser: ICefBrowser; level: TCefLogSeverity; const message_, source: ustring; line: Integer): Boolean; virtual;
      /// <summary>
      /// Called when auto-resize is enabled via
      /// ICefBrowserHost.SetAutoResizeEnabled and the contents have auto-
      /// resized. |new_size| will be the desired size in view coordinates. Return
      /// true (1) if the resize was handled or false (0) for default handling.
      /// </summary>
      function  OnAutoResize(const browser: ICefBrowser; const new_size: PCefSize): Boolean; virtual;
      /// <summary>
      /// Called when the overall page loading progress has changed. |progress|
      /// ranges from 0.0 to 1.0.
      /// </summary>
      procedure OnLoadingProgressChange(const browser: ICefBrowser; const progress: double); virtual;
      /// <summary>
      /// Called when the browser's cursor has changed. If |type| is CT_CUSTOM then
      /// |custom_cursor_info| will be populated with the custom cursor information.
      /// Return true (1) if the cursor change was handled or false (0) for default
      /// handling.
      /// </summary>
      procedure OnCursorChange(const browser: ICefBrowser; cursor_: TCefCursorHandle; CursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo; var aResult : boolean); virtual;
      /// <summary>
      /// Called when the browser's access to an audio and/or video source has
      /// changed.
      /// </summary>
      procedure OnMediaAccessChange(const browser: ICefBrowser; has_video_access, has_audio_access: boolean); virtual;
      /// <summary>
      /// Custom procedure to clear all references.
      /// </summary>
      procedure RemoveReferences; virtual;

    public
      constructor Create; virtual;
  end;

  TCustomDisplayHandler = class(TCefDisplayHandlerOwn)
    protected
      FEvents : Pointer;

      procedure OnAddressChange(const browser: ICefBrowser; const frame: ICefFrame; const url: ustring); override;
      procedure OnTitleChange(const browser: ICefBrowser; const title: ustring); override;
      procedure OnFaviconUrlChange(const browser: ICefBrowser; const iconUrls: TStrings); override;
      procedure OnFullScreenModeChange(const browser: ICefBrowser; fullscreen: Boolean); override;
      function  OnTooltip(const browser: ICefBrowser; var text: ustring): Boolean; override;
      procedure OnStatusMessage(const browser: ICefBrowser; const value: ustring); override;
      function  OnConsoleMessage(const browser: ICefBrowser; level: TCefLogSeverity; const message_, source: ustring; line: Integer): Boolean; override;
      function  OnAutoResize(const browser: ICefBrowser; const new_size: PCefSize): Boolean; override;
      procedure OnLoadingProgressChange(const browser: ICefBrowser; const progress: double); override;
      procedure OnCursorChange(const browser: ICefBrowser; cursor_: TCefCursorHandle; CursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo; var aResult : boolean); override;
      procedure OnMediaAccessChange(const browser: ICefBrowser; has_video_access, has_audio_access: boolean); override;

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
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBrowser, uCEFFrame, uCEFStringList;


procedure cef_display_handler_on_address_change(      self    : PCefDisplayHandler;
                                                      browser : PCefBrowser;
                                                      frame   : PCefFrame;
                                                const url     : PCefString); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefDisplayHandlerOwn) then
    TCefDisplayHandlerOwn(TempObject).OnAddressChange(TCefBrowserRef.UnWrap(browser),
                                                      TCefFrameRef.UnWrap(frame),
                                                      CefString(url));
end;

procedure cef_display_handler_on_title_change(      self    : PCefDisplayHandler;
                                                    browser : PCefBrowser;
                                              const title   : PCefString); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefDisplayHandlerOwn) then
    TCefDisplayHandlerOwn(TempObject).OnTitleChange(TCefBrowserRef.UnWrap(browser),
                                                    CefString(title));
end;

procedure cef_display_handler_on_favicon_urlchange(self      : PCefDisplayHandler;
                                                   browser   : PCefBrowser;
                                                   icon_urls : TCefStringList); stdcall;
var
  TempSL     : TStringList;
  TempCefSL  : ICefStringList;
  TempObject : TObject;
begin
  TempSL := nil;

  try
    try
      TempObject := CefGetObject(self);

      if (TempObject <> nil) and (TempObject is TCefDisplayHandlerOwn) then
        begin
          TempSL    := TStringList.Create;
          TempCefSL := TCefStringListRef.Create(icon_urls);
          TempCefSL.CopyToStrings(TempSL);

          TCefDisplayHandlerOwn(TempObject).OnFaviconUrlChange(TCefBrowserRef.UnWrap(browser),
                                                               TempSL);
        end;
    except
      on e : exception do
        if CustomExceptionHandler('cef_display_handler_on_favicon_urlchange', e) then raise;
    end;
  finally
    if (TempSL <> nil) then FreeAndNil(TempSL);
  end;
end;

procedure cef_display_handler_on_fullscreen_mode_change(self       : PCefDisplayHandler;
                                                        browser    : PCefBrowser;
                                                        fullscreen : Integer); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefDisplayHandlerOwn) then
    TCefDisplayHandlerOwn(TempObject).OnFullScreenModeChange(TCefBrowserRef.UnWrap(browser),
                                                             fullscreen <> 0);
end;

function cef_display_handler_on_tooltip(self    : PCefDisplayHandler;
                                        browser : PCefBrowser;
                                        text    : PCefString): Integer; stdcall;
var
  TempText   : ustring;
  TempObject : TObject;
begin
  Result     := Ord(False);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefDisplayHandlerOwn) then
    begin
      TempText := CefStringClearAndGet(text);
      Result   := Ord(TCefDisplayHandlerOwn(TempObject).OnTooltip(TCefBrowserRef.UnWrap(browser),
                                                                  TempText));
      if (text <> nil) then text^ := CefStringAlloc(TempText);
    end;
end;

procedure cef_display_handler_on_status_message(      self    : PCefDisplayHandler;
                                                      browser : PCefBrowser;
                                                const value   : PCefString); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefDisplayHandlerOwn) then
    TCefDisplayHandlerOwn(TempObject).OnStatusMessage(TCefBrowserRef.UnWrap(browser),
                                                      CefString(value));
end;

function cef_display_handler_on_console_message(      self     : PCefDisplayHandler;
                                                      browser  : PCefBrowser;
                                                      level    : TCefLogSeverity;
                                                const message_ : PCefString;
                                                const source   : PCefString;
                                                      line     : Integer): Integer; stdcall;
var
  TempObject : TObject;
begin
  Result     := Ord(False);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefDisplayHandlerOwn) then
    Result := Ord(TCefDisplayHandlerOwn(TempObject).OnConsoleMessage(TCefBrowserRef.UnWrap(browser),
                                                                     level,
                                                                     CefString(message_),
                                                                     CefString(source),
                                                                     line));
end;

function cef_display_handler_on_auto_resize(      self     : PCefDisplayHandler;
                                                  browser  : PCefBrowser;
                                            const new_size : PCefSize): Integer; stdcall;
var
  TempObject : TObject;
begin
  Result     := Ord(False);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefDisplayHandlerOwn) then
    Result := Ord(TCefDisplayHandlerOwn(TempObject).OnAutoResize(TCefBrowserRef.UnWrap(browser),
                                                                 new_size));
end;


procedure cef_display_handler_on_loading_progress_change(self     : PCefDisplayHandler;
                                                         browser  : PCefBrowser;
                                                         progress : double); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefDisplayHandlerOwn) then
    TCefDisplayHandlerOwn(TempObject).OnLoadingProgressChange(TCefBrowserRef.UnWrap(browser),
                                                              progress);
end;

function cef_display_handler_on_cursor_change(      self               : PCefDisplayHandler;
                                                    browser            : PCefBrowser;
                                                    cursor             : TCefCursorHandle;
                                                    type_              : TCefCursorType;
                                              const custom_cursor_info : PCefCursorInfo): Integer; stdcall;
var
  TempObject : TObject;
  TempResult : boolean;
begin
  TempResult := False;
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefDisplayHandlerOwn) then
    TCefDisplayHandlerOwn(TempObject).OnCursorChange(TCefBrowserRef.UnWrap(browser),
                                                     cursor,
                                                     type_,
                                                     custom_cursor_info,
                                                     TempResult);

  Result := Ord(TempResult);
end;

procedure cef_display_handler_on_media_access_change(self: PCefDisplayHandler; browser: PCefBrowser; has_video_access, has_audio_access: integer); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefDisplayHandlerOwn) then
    TCefDisplayHandlerOwn(TempObject).OnMediaAccessChange(TCefBrowserRef.UnWrap(browser),
                                                          has_video_access <> 0,
                                                          has_audio_access <> 0);
end;

constructor TCefDisplayHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefDisplayHandler));

  with PCefDisplayHandler(FData)^ do
    begin
      on_address_change          := {$IFDEF FPC}@{$ENDIF}cef_display_handler_on_address_change;
      on_title_change            := {$IFDEF FPC}@{$ENDIF}cef_display_handler_on_title_change;
      on_favicon_urlchange       := {$IFDEF FPC}@{$ENDIF}cef_display_handler_on_favicon_urlchange;
      on_fullscreen_mode_change  := {$IFDEF FPC}@{$ENDIF}cef_display_handler_on_fullscreen_mode_change;
      on_tooltip                 := {$IFDEF FPC}@{$ENDIF}cef_display_handler_on_tooltip;
      on_status_message          := {$IFDEF FPC}@{$ENDIF}cef_display_handler_on_status_message;
      on_console_message         := {$IFDEF FPC}@{$ENDIF}cef_display_handler_on_console_message;
      on_auto_resize             := {$IFDEF FPC}@{$ENDIF}cef_display_handler_on_auto_resize;
      on_loading_progress_change := {$IFDEF FPC}@{$ENDIF}cef_display_handler_on_loading_progress_change;
      on_cursor_change           := {$IFDEF FPC}@{$ENDIF}cef_display_handler_on_cursor_change;
      on_media_access_change     := {$IFDEF FPC}@{$ENDIF}cef_display_handler_on_media_access_change;
    end;
end;

procedure TCefDisplayHandlerOwn.OnAddressChange(const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
begin
  //
end;

function TCefDisplayHandlerOwn.OnConsoleMessage(const browser: ICefBrowser; level: TCefLogSeverity; const message_, source: ustring; line: Integer): Boolean;
begin
  Result := False;
end;

function TCefDisplayHandlerOwn.OnAutoResize(const browser: ICefBrowser; const new_size: PCefSize): Boolean;
begin
  Result := False;
end;

procedure TCefDisplayHandlerOwn.OnLoadingProgressChange(const browser: ICefBrowser; const progress: double);
begin
  //
end;

procedure TCefDisplayHandlerOwn.OnCursorChange(const browser: ICefBrowser; cursor_: TCefCursorHandle; CursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo; var aResult : boolean);
begin
  aResult := False;
end;

procedure TCefDisplayHandlerOwn.OnMediaAccessChange(const browser: ICefBrowser; has_video_access, has_audio_access: boolean);
begin
  //
end;

procedure TCefDisplayHandlerOwn.OnFaviconUrlChange(const browser: ICefBrowser; const iconUrls: TStrings);
begin
  //
end;

procedure TCefDisplayHandlerOwn.OnFullScreenModeChange(const browser: ICefBrowser; fullscreen: Boolean);
begin
  //
end;

procedure TCefDisplayHandlerOwn.OnStatusMessage(const browser: ICefBrowser; const value: ustring);
begin
  //
end;

procedure TCefDisplayHandlerOwn.OnTitleChange(const browser: ICefBrowser; const title: ustring);
begin
  //
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

constructor TCustomDisplayHandler.Create(const events : IChromiumEvents);
begin
  inherited Create;

  FEvents := Pointer(events);
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
  if (FEvents <> nil) then
    IChromiumEvents(FEvents).doOnAddressChange(browser, frame, url);
end;

function TCustomDisplayHandler.OnConsoleMessage(const browser  : ICefBrowser;
                                                      level    : TCefLogSeverity;
                                                const message_ : ustring;
                                                const source   : ustring;
                                                      line     : Integer): Boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnConsoleMessage(browser, level, message_, source, line)
   else
    Result := inherited OnConsoleMessage(browser, level, message_, source, line);
end;

function TCustomDisplayHandler.OnAutoResize(const browser: ICefBrowser; const new_size: PCefSize): Boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnAutoResize(browser, new_size)
   else
    Result := inherited OnAutoResize(browser, new_size);
end;

procedure TCustomDisplayHandler.OnLoadingProgressChange(const browser: ICefBrowser; const progress: double);
begin
  if (FEvents <> nil) then
    IChromiumEvents(FEvents).doOnLoadingProgressChange(browser, progress);
end;

procedure TCustomDisplayHandler.OnCursorChange(const browser          : ICefBrowser;
                                                     cursor_          : TCefCursorHandle;
                                                     cursorType       : TCefCursorType;
                                               const customCursorInfo : PCefCursorInfo;
                                               var   aResult          : boolean);
begin
  if (FEvents <> nil) then
    IChromiumEvents(FEvents).doOnCursorChange(browser, cursor_, cursorType, customCursorInfo, aResult);
end;

procedure TCustomDisplayHandler.OnMediaAccessChange(const browser: ICefBrowser; has_video_access, has_audio_access: boolean);
begin
  if (FEvents <> nil) then
    IChromiumEvents(FEvents).doOnMediaAccessChange(browser, has_video_access, has_audio_access);
end;

procedure TCustomDisplayHandler.OnFaviconUrlChange(const browser: ICefBrowser; const iconUrls: TStrings);
begin
  if (FEvents <> nil) then
    IChromiumEvents(FEvents).doOnFaviconUrlChange(browser, iconUrls);
end;

procedure TCustomDisplayHandler.OnFullScreenModeChange(const browser: ICefBrowser; fullscreen: Boolean);
begin
  if (FEvents <> nil) then
    IChromiumEvents(FEvents).doOnFullScreenModeChange(browser, fullscreen);
end;

procedure TCustomDisplayHandler.OnStatusMessage(const browser: ICefBrowser; const value: ustring);
begin
  if (FEvents <> nil) then
    IChromiumEvents(FEvents).doOnStatusMessage(browser, value);
end;

procedure TCustomDisplayHandler.OnTitleChange(const browser: ICefBrowser; const title: ustring);
begin
  if (FEvents <> nil) then
    IChromiumEvents(FEvents).doOnTitleChange(browser, title);
end;

function TCustomDisplayHandler.OnTooltip(const browser: ICefBrowser; var text: ustring): Boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnTooltip(browser, text)
   else
    Result := inherited OnTooltip(browser, text);
end;

end.
