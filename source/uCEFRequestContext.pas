unit uCEFRequestContext;

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
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes, uCEFCompletionCallback, uCEFPreferenceManager;

type
  /// <summary>
  /// A request context provides request handling for a set of related browser or
  /// URL request objects. A request context can be specified when creating a new
  /// browser via the cef_browser_host_t static factory functions or when creating
  /// a new URL request via the cef_urlrequest_t static factory functions. Browser
  /// objects with different request contexts will never be hosted in the same
  /// render process. Browser objects with the same request context may or may not
  /// be hosted in the same render process depending on the process model. Browser
  /// objects created indirectly via the JavaScript window.open function or
  /// targeted links will share the same render process and the same request
  /// context as the source browser. When running in single-process mode there is
  /// only a single render process (the main process) and so all browsers created
  /// in single-process mode will share the same request context. This will be the
  /// first request context passed into a cef_browser_host_t static factory
  /// function and all other request context objects will be ignored.
  /// </summary>
  TCefRequestContextRef = class(TCefPreferenceManagerRef, ICefRequestContext)
    protected
      /// <summary>
      /// Returns true (1) if this object is pointing to the same context as |that|
      /// object.
      /// </summary>
      function  IsSame(const other: ICefRequestContext): Boolean;

      /// <summary>
      /// Returns true (1) if this object is sharing the same storage as |that|
      /// object.
      /// </summary>
      function  IsSharingWith(const other: ICefRequestContext): Boolean;

      /// <summary>
      /// Returns true (1) if this object is the global context. The global context
      /// is used by default when creating a browser or URL request with a NULL
      /// context argument.
      /// </summary>
      function  IsGlobal: Boolean;

      /// <summary>
      /// Returns the handler for this context if any.
      /// </summary>
      function  GetHandler: ICefRequestContextHandler;

      /// <summary>
      /// Returns the cache path for this object. If NULL an "incognito mode" in-
      /// memory cache is being used.
      /// </summary>
      function  GetCachePath: ustring;

      /// <summary>
      /// Returns the cookie manager for this object. If |callback| is non-NULL it
      /// will be executed asnychronously on the UI thread after the manager's
      /// storage has been initialized.
      /// </summary>
      function  GetCookieManager(const callback: ICefCompletionCallback): ICefCookieManager;
      function  GetCookieManagerProc(const callback: TCefCompletionCallbackProc): ICefCookieManager;

      /// <summary>
      /// Register a scheme handler factory for the specified |scheme_name| and
      /// optional |domain_name|. An NULL |domain_name| value for a standard scheme
      /// will cause the factory to match all domain names. The |domain_name| value
      /// will be ignored for non-standard schemes. If |scheme_name| is a built-in
      /// scheme and no handler is returned by |factory| then the built-in scheme
      /// handler factory will be called. If |scheme_name| is a custom scheme then
      /// you must also implement the cef_app_t::on_register_custom_schemes()
      /// function in all processes. This function may be called multiple times to
      /// change or remove the factory that matches the specified |scheme_name| and
      /// optional |domain_name|. Returns false (0) if an error occurs. This
      /// function may be called on any thread in the browser process.
      /// </summary>
      function  RegisterSchemeHandlerFactory(const schemeName, domainName: ustring; const factory: ICefSchemeHandlerFactory): Boolean;

      /// <summary>
      /// Clear all registered scheme handler factories. Returns false (0) on error.
      /// This function may be called on any thread in the browser process.
      /// </summary>
      function  ClearSchemeHandlerFactories: Boolean;

      /// <summary>
      /// Clears all certificate exceptions that were added as part of handling
      /// cef_request_handler_t::on_certificate_error(). If you call this it is
      /// recommended that you also call close_all_connections() or you risk not
      /// being prompted again for server certificates if you reconnect quickly. If
      /// |callback| is non-NULL it will be executed on the UI thread after
      /// completion.
      /// </summary>
      procedure ClearCertificateExceptions(const callback: ICefCompletionCallback);

      /// <summary>
      /// Clears all HTTP authentication credentials that were added as part of
      /// handling GetAuthCredentials. If |callback| is non-NULL it will be executed
      /// on the UI thread after completion.
      /// </summary>
      procedure ClearHttpAuthCredentials(const callback: ICefCompletionCallback);

      /// <summary>
      /// Clears all active and idle connections that Chromium currently has. This
      /// is only recommended if you have released all other CEF objects but don't
      /// yet want to call cef_shutdown(). If |callback| is non-NULL it will be
      /// executed on the UI thread after completion.
      /// </summary>
      procedure CloseAllConnections(const callback: ICefCompletionCallback);

      /// <summary>
      /// Attempts to resolve |origin| to a list of associated IP addresses.
      /// |callback| will be executed on the UI thread after completion.
      /// </summary>
      procedure ResolveHost(const origin: ustring; const callback: ICefResolveCallback);

      /// <summary>
      /// Returns the MediaRouter object associated with this context.  If
      /// |callback| is non-NULL it will be executed asnychronously on the UI thread
      /// after the manager's context has been initialized.
      /// </summary>
      function  GetMediaRouter(const callback: ICefCompletionCallback): ICefMediaRouter;

      /// <summary>
      /// Returns the current value for |content_type| that applies for the
      /// specified URLs. If both URLs are NULL the default value will be returned.
      /// Returns nullptr if no value is configured. Must be called on the browser
      /// process UI thread.
      /// </summary>
      function  GetWebsiteSetting(const requesting_url, top_level_url: ustring; content_type: TCefContentSettingTypes): ICefValue;

      /// <summary>
      /// Sets the current value for |content_type| for the specified URLs in the
      /// default scope. If both URLs are NULL, and the context is not incognito,
      /// the default value will be set. Pass nullptr for |value| to remove the
      /// default value for this content type.
      ///
      /// WARNING: Incorrect usage of this function may cause instability or
      /// security issues in Chromium. Make sure that you first understand the
      /// potential impact of any changes to |content_type| by reviewing the related
      /// source code in Chromium. For example, if you plan to modify
      /// CEF_CONTENT_SETTING_TYPE_POPUPS, first review and understand the usage of
      /// ContentSettingsType::POPUPS in Chromium:
      /// https://source.chromium.org/search?q=ContentSettingsType::POPUPS
      /// </summary>
      procedure SetWebsiteSetting(const requesting_url, top_level_url: ustring; content_type: TCefContentSettingTypes; const value: ICefValue);

      /// <summary>
      /// Returns the current value for |content_type| that applies for the
      /// specified URLs. If both URLs are NULL the default value will be returned.
      /// Returns CEF_CONTENT_SETTING_VALUE_DEFAULT if no value is configured. Must
      /// be called on the browser process UI thread.
      /// </summary>
      function  GetContentSetting(const requesting_url, top_level_url: ustring; content_type: TCefContentSettingTypes): TCefContentSettingValues;

      /// <summary>
      /// Sets the current value for |content_type| for the specified URLs in the
      /// default scope. If both URLs are NULL, and the context is not incognito,
      /// the default value will be set. Pass CEF_CONTENT_SETTING_VALUE_DEFAULT for
      /// |value| to use the default value for this content type.
      ///
      /// WARNING: Incorrect usage of this function may cause instability or
      /// security issues in Chromium. Make sure that you first understand the
      /// potential impact of any changes to |content_type| by reviewing the related
      /// source code in Chromium. For example, if you plan to modify
      /// CEF_CONTENT_SETTING_TYPE_POPUPS, first review and understand the usage of
      /// ContentSettingsType::POPUPS in Chromium:
      /// https://source.chromium.org/search?q=ContentSettingsType::POPUPS
      /// </summary>
      procedure SetContentSetting(const requesting_url, top_level_url: ustring; content_type: TCefContentSettingTypes; value: TCefContentSettingValues);

      /// <summary>
      /// Sets the Chrome color scheme for all browsers that share this request
      /// context. |variant| values of SYSTEM, LIGHT and DARK change the underlying
      /// color mode (e.g. light vs dark). Other |variant| values determine how
      /// |user_color| will be applied in the current color mode. If |user_color| is
      /// transparent (0) the default color will be used.
      /// </summary>
      procedure SetChromeColorScheme(variant: TCefColorVariant; user_color: TCefColor);

      /// <summary>
      /// Returns the current Chrome color scheme mode (SYSTEM, LIGHT or DARK). Must
      /// be called on the browser process UI thread.
      /// </summary>
      function GetChromeColorSchemeMode: TCefColorVariant;

      /// <summary>
      /// Returns the current Chrome color scheme color, or transparent (0) for the
      /// default color. Must be called on the browser process UI thread.
      /// </summary>
      function GetChromeColorSchemeColor: TCefColor;

      /// <summary>
      /// Returns the current Chrome color scheme variant. Must be called on the
      /// browser process UI thread.
      /// </summary>
      function GetChromeColorSchemeVariant: TCefColorVariant;

    public
      class function UnWrap(data: Pointer): ICefRequestContext; reintroduce;
      /// <summary>
      /// Returns the global context object.
      /// </summary>
      class function Global: ICefRequestContext; reintroduce;
      /// <summary>
      /// Creates a new context object with the specified |settings| and optional
      /// |handler|.
      /// </summary>
      /// <param name="settings">Pointer to TCefRequestContextSettings.</param>
      /// <param name="handler">Optional handler for the request context.</param>
      class function New(const settings: PCefRequestContextSettings; const handler: ICefRequestContextHandler = nil): ICefRequestContext; overload;
      /// <summary>
      /// Creates a new context object with the specified settings and optional
      /// |handler|.
      /// </summary>
      /// <param name="aCache">The directory where cache data for this request context will be stored on disk. See TCefRequestContextSettings.cache_path for more information.</param>
      /// <param name="aAcceptLanguageList">Comma delimited ordered list of language codes without any whitespace that will be used in the "Accept-Language" HTTP header. See TCefRequestContextSettings.accept_language_list for more information.</param>
      /// <param name="aCookieableSchemesList">Comma delimited list of schemes supported by the associated ICefCookieManager. See TCefRequestContextSettings.cookieable_schemes_list for more information.</param>
      /// <param name="aCookieableSchemesExcludeDefaults">Setting this parameter to true will disable all loading and saving of cookies. See TCefRequestContextSettings.cookieable_schemes_list for more information.</param>
      /// <param name="aPersistSessionCookies">To persist session cookies (cookies without an expiry date or validity interval) by default when using the global cookie manager set this value to true. See TCefRequestContextSettings.persist_session_cookies for more information.</param>
      /// <param name="handler">Optional handler for the request context.</param>
      class function New(const aCache, aAcceptLanguageList, aCookieableSchemesList : ustring; aCookieableSchemesExcludeDefaults, aPersistSessionCookies : boolean; const handler: ICefRequestContextHandler = nil): ICefRequestContext; overload;
      /// <summary>
      /// Creates a new context object that shares storage with |other| and uses an
      /// optional |handler|.
      /// </summary>
      /// <param name="other">Another ICefRequestContext instance that will share storage with the new ICefRequestContext instance.</param>
      /// <param name="handler">Optional handler for the request context.</param>
      class function Shared(const other: ICefRequestContext; const handler: ICefRequestContextHandler): ICefRequestContext;
  end;

  TCefClearCertificateExceptionsCompletionCallback = class(TCefCustomCompletionCallback)
    protected
      procedure OnComplete; override;
  end;

  TCefClearHttpAuthCredentialsCompletionCallback = class(TCefCustomCompletionCallback)
    protected
      procedure OnComplete; override;
  end;

  TCefCloseAllConnectionsCompletionCallback = class(TCefCustomCompletionCallback)
    protected
      procedure OnComplete; override;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFCookieManager, uCEFRequestContextHandler,
  uCEFStringList, uCEFMediaRouter, uCEFValue;

function TCefRequestContextRef.ClearSchemeHandlerFactories: Boolean;
begin
  Result := PCefRequestContext(FData)^.clear_scheme_handler_factories(PCefRequestContext(FData)) <> 0;
end;

function TCefRequestContextRef.GetCachePath: ustring;
begin
  Result := CefStringFreeAndGet(PCefRequestContext(FData)^.get_cache_path(PCefRequestContext(FData)));
end;

function TCefRequestContextRef.GetCookieManager(const callback: ICefCompletionCallback): ICefCookieManager;
begin
  Result := TCefCookieManagerRef.UnWrap(PCefRequestContext(FData)^.get_cookie_manager(PCefRequestContext(FData), CefGetData(callback)));
end;

function TCefRequestContextRef.GetCookieManagerProc(const callback: TCefCompletionCallbackProc): ICefCookieManager;
begin
  Result := GetCookieManager(TCefFastCompletionCallback.Create(callback));
end;

function TCefRequestContextRef.GetHandler: ICefRequestContextHandler;
var
  TempHandler : PCefRequestContextHandler;
begin
  TempHandler := PCefRequestContext(FData)^.get_handler(PCefRequestContext(FData));

  if (TempHandler <> nil) then
    Result := TCefRequestContextHandlerRef.UnWrap(TempHandler)
   else
    Result := nil;
end;

class function TCefRequestContextRef.Global: ICefRequestContext;
begin
  Result := UnWrap(cef_request_context_get_global_context());
end;

function TCefRequestContextRef.IsGlobal: Boolean;
begin
  Result := PCefRequestContext(FData)^.is_global(PCefRequestContext(FData)) <> 0;
end;

function TCefRequestContextRef.IsSame(const other: ICefRequestContext): Boolean;
begin
  Result := PCefRequestContext(FData)^.is_same(PCefRequestContext(FData), CefGetData(other)) <> 0;
end;

function TCefRequestContextRef.IsSharingWith(const other: ICefRequestContext): Boolean;
begin
  Result := PCefRequestContext(FData)^.is_sharing_with(PCefRequestContext(FData), CefGetData(other)) <> 0;
end;

class function TCefRequestContextRef.New(const settings : PCefRequestContextSettings;
                                         const handler  : ICefRequestContextHandler): ICefRequestContext;
begin
  Result := UnWrap(cef_request_context_create_context(settings, CefGetData(handler)));
end;

class function TCefRequestContextRef.New(const aCache                            : ustring;
                                         const aAcceptLanguageList               : ustring;
                                         const aCookieableSchemesList            : ustring;
                                               aCookieableSchemesExcludeDefaults : boolean;
                                               aPersistSessionCookies            : boolean;
                                         const handler                           : ICefRequestContextHandler): ICefRequestContext;
var
  TempSettings : TCefRequestContextSettings;
begin
  TempSettings.size                                 := SizeOf(TCefRequestContextSettings);
  TempSettings.cache_path                           := CefString(aCache);
  TempSettings.persist_session_cookies              := Ord(aPersistSessionCookies);
  TempSettings.accept_language_list                 := CefString(aAcceptLanguageList);
  TempSettings.cookieable_schemes_list              := CefString(aCookieableSchemesList);
  TempSettings.cookieable_schemes_exclude_defaults  := Ord(aCookieableSchemesExcludeDefaults);

  Result := UnWrap(cef_request_context_create_context(@TempSettings, CefGetData(handler)));
end;

procedure TCefRequestContextRef.ClearCertificateExceptions(const callback: ICefCompletionCallback);
begin
  PCefRequestContext(FData)^.clear_certificate_exceptions(PCefRequestContext(FData), CefGetData(callback));
end;

procedure TCefRequestContextRef.ClearHttpAuthCredentials(const callback: ICefCompletionCallback);
begin
  PCefRequestContext(FData)^.clear_http_auth_credentials(PCefRequestContext(FData), CefGetData(callback));
end;

procedure TCefRequestContextRef.CloseAllConnections(const callback: ICefCompletionCallback);
begin
  PCefRequestContext(FData)^.close_all_connections(PCefRequestContext(FData), CefGetData(callback));
end;

procedure TCefRequestContextRef.ResolveHost(const origin   : ustring;
                                            const callback : ICefResolveCallback);
var
  TempOrigin : TCefString;
begin
  TempOrigin := CefString(origin);
  PCefRequestContext(FData)^.resolve_host(PCefRequestContext(FData), @TempOrigin, CefGetData(callback));
end;

function TCefRequestContextRef.GetMediaRouter(const callback: ICefCompletionCallback): ICefMediaRouter;
begin
  Result := TCefMediaRouterRef.UnWrap(PCefRequestContext(FData)^.get_media_router(PCefRequestContext(FData), CefGetData(callback)));
end;

function TCefRequestContextRef.GetWebsiteSetting(const requesting_url, top_level_url: ustring; content_type: TCefContentSettingTypes): ICefValue;
var
  TempRequestingURL, TempTopLevelURL : TCefString;
begin
  TempRequestingURL := CefString(requesting_url);
  TempTopLevelURL   := CefString(top_level_url);
  Result            := TCefValueRef.UnWrap(PCefRequestContext(FData)^.get_website_setting(PCefRequestContext(FData), @TempRequestingURL, @TempTopLevelURL, content_type));
end;

procedure TCefRequestContextRef.SetWebsiteSetting(const requesting_url, top_level_url: ustring; content_type: TCefContentSettingTypes; const value: ICefValue);
var
  TempRequestingURL, TempTopLevelURL : TCefString;
begin
  TempRequestingURL := CefString(requesting_url);
  TempTopLevelURL   := CefString(top_level_url);
  PCefRequestContext(FData)^.set_website_setting(PCefRequestContext(FData), @TempRequestingURL, @TempTopLevelURL, content_type, CefGetData(value));
end;

function TCefRequestContextRef.GetContentSetting(const requesting_url, top_level_url: ustring; content_type: TCefContentSettingTypes): TCefContentSettingValues;
var
  TempRequestingURL, TempTopLevelURL : TCefString;
begin
  TempRequestingURL := CefString(requesting_url);
  TempTopLevelURL   := CefString(top_level_url);
  Result            := PCefRequestContext(FData)^.get_content_setting(PCefRequestContext(FData), @TempRequestingURL, @TempTopLevelURL, content_type);
end;

procedure TCefRequestContextRef.SetContentSetting(const requesting_url, top_level_url: ustring; content_type: TCefContentSettingTypes; value: TCefContentSettingValues);
var
  TempRequestingURL, TempTopLevelURL : TCefString;
begin
  TempRequestingURL := CefString(requesting_url);
  TempTopLevelURL   := CefString(top_level_url);
  PCefRequestContext(FData)^.set_content_setting(PCefRequestContext(FData), @TempRequestingURL, @TempTopLevelURL, content_type, value);
end;

procedure TCefRequestContextRef.SetChromeColorScheme(variant: TCefColorVariant; user_color: TCefColor);
begin
  PCefRequestContext(FData)^.set_chrome_color_scheme(PCefRequestContext(FData), variant, user_color);
end;

function TCefRequestContextRef.GetChromeColorSchemeMode: TCefColorVariant;
begin
  Result := PCefRequestContext(FData)^.get_chrome_color_scheme_mode(PCefRequestContext(FData));
end;

function TCefRequestContextRef.GetChromeColorSchemeColor: TCefColor;
begin
  Result := PCefRequestContext(FData)^.get_chrome_color_scheme_color(PCefRequestContext(FData));
end;

function TCefRequestContextRef.GetChromeColorSchemeVariant: TCefColorVariant;
begin
  Result := PCefRequestContext(FData)^.get_chrome_color_scheme_variant(PCefRequestContext(FData));
end;

function TCefRequestContextRef.RegisterSchemeHandlerFactory(const schemeName : ustring;
                                                            const domainName : ustring;
                                                            const factory    : ICefSchemeHandlerFactory): Boolean;
var
  TempScheme, TempDomain : TCefString;
begin
  TempScheme := CefString(schemeName);
  TempDomain := CefString(domainName);
  Result     := PCefRequestContext(FData)^.register_scheme_handler_factory(PCefRequestContext(FData),
                                                                           @TempScheme,
                                                                           @TempDomain,
                                                                           CefGetData(factory)) <> 0;
end;

class function TCefRequestContextRef.Shared(const other   : ICefRequestContext;
                                            const handler : ICefRequestContextHandler): ICefRequestContext;
begin
  Result := UnWrap(cef_create_context_shared(CefGetData(other), CefGetData(handler)));
end;

class function TCefRequestContextRef.UnWrap(data: Pointer): ICefRequestContext;
begin
  if (data <> nil) then
    Result := Create(data) as ICefRequestContext
   else
    Result := nil;
end;


// TCefClearCertificateExceptionsCompletionCallback

procedure TCefClearCertificateExceptionsCompletionCallback.OnComplete;
begin
  try
    try
      if (FEvents <> nil) then IChromiumEvents(FEvents).doCertificateExceptionsCleared;
    except
      on e : exception do
        if CustomExceptionHandler('TCefClearCertificateExceptionsCompletionCallback.OnComplete', e) then raise;
    end;
  finally
    FEvents := nil;
  end;
end;


// TCefClearHttpAuthCredentialsCompletionCallback

procedure TCefClearHttpAuthCredentialsCompletionCallback.OnComplete;
begin
  try
    try
      if (FEvents <> nil) then IChromiumEvents(FEvents).doHttpAuthCredentialsCleared;
    except
      on e : exception do
        if CustomExceptionHandler('TCefClearHttpAuthCredentialsCompletionCallback.OnComplete', e) then raise;
    end;
  finally
    FEvents := nil;
  end;
end;


// TCefCloseAllConnectionsCompletionCallback

procedure TCefCloseAllConnectionsCompletionCallback.OnComplete;
begin
  try
    try
      if (FEvents <> nil) then IChromiumEvents(FEvents).doAllConnectionsClosed;
    except
      on e : exception do
        if CustomExceptionHandler('TCefCloseAllConnectionsCompletionCallback.OnComplete', e) then raise;
    end;
  finally
    FEvents := nil;
  end;
end;

end.
