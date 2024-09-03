unit uCEFBrowserView;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes, System.SysUtils,
  {$ELSE}
  Classes, SysUtils,
  {$ENDIF}
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes, uCEFView;

type
  /// <summary>
  /// A View hosting a ICefBrowser instance. Methods must be called on the
  /// browser process UI thread unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_browser_view_capi.h">CEF source file: /include/capi/views/cef_browser_view_capi.h (cef_browser_view_t)</see></para>
  /// </remarks>
  TCefBrowserViewRef = class(TCefViewRef, ICefBrowserView)
    protected
      /// <summary>
      /// Returns the ICefBrowser hosted by this BrowserView. Will return NULL if
      /// the browser has not yet been created or has already been destroyed.
      /// </summary>
      function  GetBrowser : ICefBrowser;
      /// <summary>
      /// Returns the Chrome toolbar associated with this BrowserView. Only
      /// supported when using Chrome style. The ICefBrowserViewDelegate.GetChromeToolbarType
      /// function must return a value other than
      /// CEF_CTT_NONE and the toolbar will not be available until after this
      /// BrowserView is added to a ICefWindow and
      /// ICefViewDelegate.OnWindowChanged() has been called.
      /// </summary>
      function  GetChromeToolbar : ICefView;
      /// <summary>
      /// Sets whether normal priority accelerators are first forwarded to the web
      /// content (`keydown` event handler) or ICefKeyboardHandler. Normal priority
      /// accelerators can be registered via ICefWindow.SetAccelerator (with
      /// |high_priority|=false) or internally for standard accelerators supported
      /// by Chrome style. If |prefer_accelerators| is true then the matching
      /// accelerator will be triggered immediately (calling
      /// ICefWindowDelegate.OnAccelerator or ICefCommandHandler.OnChromeCommand
      /// respectively) and the event will not be forwarded to the web content or
      /// ICefKeyboardHandler first. If |prefer_accelerators| is false then the
      /// matching accelerator will only be triggered if the event is not handled by
      /// web content (`keydown` event handler that calls `event.preventDefault()`)
      /// or by ICefKeyboardHandler. The default value is false.
      /// </summary>
      procedure SetPreferAccelerators(prefer_accelerators: boolean);
      /// <summary>
      /// Returns the runtime style for this BrowserView (ALLOY or CHROME). See
      /// TCefRuntimeStyle documentation for details.
      /// </summary>
      function GetRuntimeStyle : TCefRuntimeStyle;

    public
      /// <summary>
      /// Returns a ICefBrowserView instance using a PCefBrowserView data pointer.
      /// </summary>
      class function UnWrap(data: Pointer): ICefBrowserView;
      /// <summary>
      /// Create a new BrowserView. The underlying cef_browser_t will not be created
      /// until this view is added to the views hierarchy. The optional |extra_info|
      /// parameter provides an opportunity to specify extra information specific to
      /// the created browser that will be passed to
      /// cef_render_process_handler_t::on_browser_created() in the render process.
      /// </summary>
      class function CreateBrowserView(const client: ICefClient; const url: ustring; const settings: TCefBrowserSettings; const extra_info: ICefDictionaryValue; const request_context: ICefRequestContext; const delegate: ICefBrowserViewDelegate): ICefBrowserView;
      /// <summary>
      /// Returns the BrowserView associated with |browser|.
      /// </summary>
      class function GetForBrowser(const browser: ICefBrowser): ICefBrowserView;
  end;

implementation

uses
  uCEFLibFunctions, uCEFMiscFunctions, uCEFBrowser;

function TCefBrowserViewRef.GetBrowser : ICefBrowser;
begin
  Result := TCefBrowserRef.UnWrap(PCefBrowserView(FData)^.get_browser(PCefBrowserView(FData)));
end;

function TCefBrowserViewRef.GetChromeToolbar : ICefView;
begin
  Result := TCefViewRef.UnWrap(PCefBrowserView(FData)^.get_chrome_toolbar(PCefBrowserView(FData)));
end;

procedure TCefBrowserViewRef.SetPreferAccelerators(prefer_accelerators: boolean);
begin
  PCefBrowserView(FData)^.set_prefer_accelerators(PCefBrowserView(FData),
                                                  ord(prefer_accelerators));
end;

function TCefBrowserViewRef.GetRuntimeStyle : TCefRuntimeStyle;
begin
  Result := PCefBrowserView(FData)^.get_runtime_style(PCefBrowserView(FData));
end;

class function TCefBrowserViewRef.UnWrap(data: Pointer): ICefBrowserView;
begin
  if (data <> nil) then
    Result := Create(data) as ICefBrowserView
   else
    Result := nil;
end;

class function TCefBrowserViewRef.CreateBrowserView(const client          : ICefClient;
                                                    const url             : ustring;
                                                    const settings        : TCefBrowserSettings;
                                                    const extra_info      : ICefDictionaryValue;
                                                    const request_context : ICefRequestContext;
                                                    const delegate        : ICefBrowserViewDelegate): ICefBrowserView;

var
  TempURL         : TCefString;
  TempBrowserView : PCefBrowserView;
begin
  Result := nil;

  if (client <> nil) and (delegate <> nil) then
    begin
      TempURL         := CefString(url);
      TempBrowserView := cef_browser_view_create(CefGetData(client),
                                                 @TempURL,
                                                 @settings,
                                                 CefGetData(extra_info),
                                                 CefGetData(request_context),
                                                 CefGetData(delegate));

      if (TempBrowserView <> nil) then
        Result := Create(TempBrowserView) as ICefBrowserView;
    end;
end;

class function TCefBrowserViewRef.GetForBrowser(const browser: ICefBrowser): ICefBrowserView;
var
  TempBrowserView : PCefBrowserView;
begin
  Result := nil;

  if (browser <> nil) then
    begin
      TempBrowserView := cef_browser_view_get_for_browser(CefGetData(browser));

      if (TempBrowserView <> nil) then
        Result := Create(TempBrowserView) as ICefBrowserView;
    end;
end;

end.

