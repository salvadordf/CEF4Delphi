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
      /// Load an extension.
      ///
      /// If extension resources will be read from disk using the default load
      /// implementation then |root_directory| should be the absolute path to the
      /// extension resources directory and |manifest| should be NULL. If extension
      /// resources will be provided by the client (e.g. via cef_request_handler_t
      /// and/or cef_extension_handler_t) then |root_directory| should be a path
      /// component unique to the extension (if not absolute this will be internally
      /// prefixed with the PK_DIR_RESOURCES path) and |manifest| should contain the
      /// contents that would otherwise be read from the "manifest.json" file on
      /// disk.
      ///
      /// The loaded extension will be accessible in all contexts sharing the same
      /// storage (HasExtension returns true (1)). However, only the context on
      /// which this function was called is considered the loader (DidLoadExtension
      /// returns true (1)) and only the loader will receive
      /// cef_request_context_handler_t callbacks for the extension.
      ///
      /// cef_extension_handler_t::OnExtensionLoaded will be called on load success
      /// or cef_extension_handler_t::OnExtensionLoadFailed will be called on load
      /// failure.
      ///
      /// If the extension specifies a background script via the "background"
      /// manifest key then cef_extension_handler_t::OnBeforeBackgroundBrowser will
      /// be called to create the background browser. See that function for
      /// additional information about background scripts.
      ///
      /// For visible extension views the client application should evaluate the
      /// manifest to determine the correct extension URL to load and then pass that
      /// URL to the cef_browser_host_t::CreateBrowser* function after the extension
      /// has loaded. For example, the client can look for the "browser_action"
      /// manifest key as documented at
      /// https://developer.chrome.com/extensions/browserAction. Extension URLs take
      /// the form "chrome-extension://<extension_id>/<path>".
      ///
      /// Browsers that host extensions differ from normal browsers as follows:
      ///  - Can access chrome.* JavaScript APIs if allowed by the manifest. Visit
      ///    chrome://extensions-support for the list of extension APIs currently
      ///    supported by CEF.
      ///  - Main frame navigation to non-extension content is blocked.
      ///  - Pinch-zooming is disabled.
      ///  - CefBrowserHost::GetExtension returns the hosted extension.
      ///  - CefBrowserHost::IsBackgroundHost returns true for background hosts.
      ///
      /// See https://developer.chrome.com/extensions for extension implementation
      /// and usage documentation.
      /// </summary>
      procedure LoadExtension(const root_directory: ustring; const manifest: ICefDictionaryValue; const handler: ICefExtensionHandler);

      /// <summary>
      /// Returns true (1) if this context was used to load the extension identified
      /// by |extension_id|. Other contexts sharing the same storage will also have
      /// access to the extension (see HasExtension). This function must be called
      /// on the browser process UI thread.
      /// </summary>
      function  DidLoadExtension(const extension_id: ustring): boolean;

      /// <summary>
      /// Returns true (1) if this context has access to the extension identified by
      /// |extension_id|. This may not be the context that was used to load the
      /// extension (see DidLoadExtension). This function must be called on the
      /// browser process UI thread.
      /// </summary>
      function  HasExtension(const extension_id: ustring): boolean;

      /// <summary>
      /// Retrieve the list of all extensions that this context has access to (see
      /// HasExtension). |extension_ids| will be populated with the list of
      /// extension ID values. Returns true (1) on success. This function must be
      /// called on the browser process UI thread.
      /// </summary>
      function  GetExtensions(const extension_ids: TStringList): boolean;

      /// <summary>
      /// Returns the extension matching |extension_id| or NULL if no matching
      /// extension is accessible in this context (see HasExtension). This function
      /// must be called on the browser process UI thread.
      /// </summary>
      function  GetExtension(const extension_id: ustring): ICefExtension;

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
      class function New(const settings: PCefRequestContextSettings; const handler: ICefRequestContextHandler = nil): ICefRequestContext; overload;
      class function New(const aCache, aAcceptLanguageList, aCookieableSchemesList : ustring; aCookieableSchemesExcludeDefaults, aPersistSessionCookies, aPersistUserPreferences : boolean; const handler: ICefRequestContextHandler = nil): ICefRequestContext; overload;

      /// <summary>
      /// Creates a new context object that shares storage with |other| and uses an
      /// optional |handler|.
      /// </summary>
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
  uCEFExtension, uCEFStringList, uCEFMediaRouter, uCEFValue;

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
                                               aPersistUserPreferences           : boolean;
                                         const handler                           : ICefRequestContextHandler): ICefRequestContext;
var
  TempSettings : TCefRequestContextSettings;
begin
  TempSettings.size                                 := SizeOf(TCefRequestContextSettings);
  TempSettings.cache_path                           := CefString(aCache);
  TempSettings.persist_session_cookies              := Ord(aPersistSessionCookies);
  TempSettings.persist_user_preferences             := Ord(aPersistUserPreferences);
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

procedure TCefRequestContextRef.LoadExtension(const root_directory: ustring; const manifest: ICefDictionaryValue; const handler: ICefExtensionHandler);
var
  TempDir : TCefString;
begin
  TempDir := CefString(root_directory);
  PCefRequestContext(FData)^.load_extension(PCefRequestContext(FData), @TempDir, CefGetData(manifest), CefGetData(handler));
end;

function TCefRequestContextRef.DidLoadExtension(const extension_id: ustring): boolean;
var
  TempID : TCefString;
begin
  TempID := CefString(extension_id);
  Result := PCefRequestContext(FData)^.did_load_extension(PCefRequestContext(FData), @TempID) <> 0;
end;

function TCefRequestContextRef.HasExtension(const extension_id: ustring): boolean;
var
  TempID : TCefString;
begin
  TempID := CefString(extension_id);
  Result := PCefRequestContext(FData)^.has_extension(PCefRequestContext(FData), @TempID) <> 0;
end;

function TCefRequestContextRef.GetExtensions(const extension_ids: TStringList): boolean;
var
  TempSL : ICefStringList;
begin
  Result := False;
  TempSL := TCefStringListOwn.Create;

  if (PCefRequestContext(FData)^.get_extensions(PCefRequestContext(FData), TempSL.Handle) <> 0) then
    begin
      TempSL.CopyToStrings(extension_ids);
      Result := True;
    end;
end;

function TCefRequestContextRef.GetExtension(const extension_id: ustring): ICefExtension;
var
  TempID : TCefString;
begin
  TempID := CefString(extension_id);
  Result := TCefExtensionRef.UnWrap(PCefRequestContext(FData)^.get_extension(PCefRequestContext(FData), @TempID));
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
