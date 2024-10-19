unit uCEFInterfaces;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
    {$IFDEF MSWINDOWS}WinApi.Windows,{$ENDIF} System.Classes,
  {$ELSE}
    {$IFDEF MSWINDOWS}Windows,{$ENDIF} Classes,
  {$ENDIF}
  uCEFTypes, uCEFSchemeRegistrar;

type
  ICefBrowser = interface;
  ICefFrame = interface;
  ICefFrameHandler = interface;
  ICefRequest = interface;
  ICefv8Value = interface;
  ICefV8Exception = interface;
  ICefV8StackTrace = interface;
  ICefDomVisitor = interface;
  ICefDomDocument = interface;
  ICefDomNode = interface;
  ICefv8Context = interface;
  ICefListValue = interface;
  ICefBinaryValue = interface;
  ICefDictionaryValue = interface;
  ICefClient = interface;
  ICefUrlrequestClient = interface;
  ICefBrowserHost = interface;
  ICefTask = interface;
  ICefTaskRunner = interface;
  ICefFileDialogCallback = interface;
  ICefUnresponsiveProcessCallback = interface;
  ICefPrintHandler = interface;
  ICefPrintDialogCallback = interface;
  ICefPrintJobCallback = interface;
  ICefRequestContext = interface;
  ICefAccessibilityHandler = interface;
  ICefDragData = interface;
  ICefNavigationEntry = interface;
  ICefSslInfo = interface;
  ICefSSLStatus = interface;
  ICefImage = interface;
  IChromiumEvents = interface;
  ICefThread = interface;
  ICefWaitableEvent = interface;
  ICefX509CertPrincipal = interface;
  ICefX509Certificate = interface;
  ICefSelectClientCertificateCallback = interface;
  ICefCommandLine = interface;
  ICefRequestHandler = interface;
  ICefResourceRequestHandler = interface;
  ICefCookieAccessFilter = interface;
  ICefResourceBundleHandler = interface;
  ICefBrowserProcessHandler = interface;
  ICefRenderProcessHandler = interface;
  ICefProcessMessage = interface;
  ICefLifeSpanHandler = interface;
  ICefCommandHandler = interface;
  ICefStreamReader = interface;
  ICefLoadHandler = interface;
  ICefServer = interface;
  ICefServerHandler = interface;
  ICefContextMenuParams = interface;
  ICefMenuModel = interface;
  ICefRunContextMenuCallback = interface;
  ICefRunQuickMenuCallback = interface;
  ICefDownloadItem = interface;
  ICefBeforeDownloadCallback = interface;
  ICefJsDialogCallback = interface;
  ICefDownloadItemCallback = interface;
  ICefResourceSkipCallback = interface;
  ICefResourceReadCallback = interface;
  ICefResourceHandler = interface;
  ICefResponse = interface;
  ICefResponseFilter = interface;
  ICefAuthCallback = interface;
  ICefCallback = interface;
  ICefDragHandler = interface;
  ICefFindHandler = interface;
  ICefCookieManager = interface;
  ICefDisplay = interface;
  ICefLayout = interface;
  ICefBoxLayout = interface;
  ICefFillLayout = interface;
  ICefView = interface;
  ICefBrowserView = interface;
  ICefButton = interface;
  ICefPanel = interface;
  ICefScrollView = interface;
  ICefTextfield = interface;
  ICefViewDelegate = interface;
  ICefWindow = interface;
  ICefLabelButton = interface;
  ICefMenuButton = interface;
  ICefUrlRequest = interface;
  ICefPostDataElement = interface;
  ICefRegistration = interface;
  ICefMediaRouter = interface;
  ICefMediaObserver = interface;
  ICefMediaRoute = interface;
  ICefMediaRouteCreateCallback = interface;
  ICefMediaSink = interface;
  ICefMediaSinkDeviceInfoCallback = interface;
  ICefMediaSource = interface;
  ICefAudioHandler = interface;
  ICefDevToolsMessageObserver = interface;
  ICefValue = interface;
  ICefPrintSettings = interface;
  ICefMediaAccessCallback = interface;
  ICefMediaAccessHandler = interface;
  ICefPermissionPromptCallback = interface;
  ICefPermissionHandler = interface;
  ICefSharedMemoryRegion = interface;
  ICefSharedProcessMessageBuilder = interface;
  ICefBrowserViewDelegate = interface;
  ICefMenuButtonPressedLock = interface;
  ICefRequestContextHandler = interface;

  TCefv8ValueArray         = array of ICefv8Value;
  TCefX509CertificateArray = array of ICefX509Certificate;
  TCefBinaryValueArray     = array of ICefBinaryValue;
  TCefPostDataElementArray = array of ICefPostDataElement;
  TCefMediaRouteArray      = array of ICefMediaRoute;
  TCefMediaSinkArray       = array of ICefMediaSink;
  TCefDisplayArray         = array of ICefDisplay;

  /// <summary>
  /// Custom record with media sink information.
  /// </summary>
  TCefMediaSinkInfo = record
    ID          : ustring;
    Name        : ustring;
    IconType    : TCefMediaSinkIconType;
    SinkType    : TCefMediaType;
    SinkIntf    : ICefMediaSink;
  end;
  TCefMediaSinkInfoArray = array of TCefMediaSinkInfo;

  /// <summary>
  /// Custom record with media route information.
  /// </summary>
  TCefMediaRouteInfo = record
    ID        : ustring;
    SourceID  : ustring;
    SinkID    : ustring;
    RouteIntf : ICefMediaRoute;
  end;
  TCefMediaRouteInfoArray = array of TCefMediaRouteInfo;

  /// <summary>
  /// Custom record with media source information.
  /// </summary>
  TCefMediaSourceInfo = record
    ID         : ustring;
    Valid      : boolean;
    SourceType : TCefMediaType;
    SourceIntf : ICefMediaSource;
  end;
  TCefMediaSourceInfoArray = array of TCefMediaSourceInfo;

  {*
   *******************************************
   **** Callback procedures and functions ****
   *******************************************
  *}

  TCefEndTracingCallbackProc           = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const tracingFile: ustring);
  TCefFastTaskProc                     = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure();
  TCefv8ArrayBufferReleaseCallbackProc = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(buffer : Pointer);
  TCefWebPluginIsUnstableProc          = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const path: ustring; unstable: Boolean);
  TCefV8AccessorGetterProc             = {$IFDEF DELPHI12_UP}reference to{$ENDIF} function(const name: ustring; const object_: ICefv8Value; var value: ICefv8Value; var exception: ustring): Boolean;
  TCefV8AccessorSetterProc             = {$IFDEF DELPHI12_UP}reference to{$ENDIF} function(const name: ustring; const object_, value: ICefv8Value; var exception: ustring): Boolean;
  TCefV8InterceptorGetterByNameProc    = {$IFDEF DELPHI12_UP}reference to{$ENDIF} function(const name: ustring; const object_: ICefv8Value; var value: ICefv8Value; var exception: ustring): Boolean;
  TCefV8InterceptorSetterByNameProc    = {$IFDEF DELPHI12_UP}reference to{$ENDIF} function(const name: ustring; const object_, value: ICefv8Value; var exception: ustring): Boolean;
  TCefV8InterceptorGetterByIndexProc   = {$IFDEF DELPHI12_UP}reference to{$ENDIF} function(index: integer; const object_: ICefv8Value; var value: ICefv8Value; var exception: ustring): Boolean;
  TCefV8InterceptorSetterByIndexProc   = {$IFDEF DELPHI12_UP}reference to{$ENDIF} function(index: integer; const object_, value: ICefv8Value; var exception: ustring): Boolean;
  TOnPdfPrintFinishedProc              = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const path: ustring; ok: Boolean);
  TCefDomVisitorProc                   = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const document: ICefDomDocument);
  TCefDomVisitorProc2                  = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const browser : ICefBrowser; const frame: ICefFrame; const document: ICefDomDocument);
  TCefDomVisitorProc3                  = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const browser : ICefBrowser; const frame: ICefFrame; const document: ICefDomDocument; const aValue : ustring);
  TCefStringVisitorProc                = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const str: ustring);
  TCefRunFileDialogCallbackProc        = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const filePaths: TStrings);
  TCefCompletionCallbackProc           = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure();
  TCefSetCookieCallbackProc            = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(success: Boolean);
  TCefDeleteCookiesCallbackProc        = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(numDeleted: Integer);
  TCefNavigationEntryVisitorProc       = {$IFDEF DELPHI12_UP}reference to{$ENDIF} function(const entry: ICefNavigationEntry; current: Boolean; index, total: Integer): Boolean;
  TOnDownloadImageFinishedProc         = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const imageUrl: ustring; httpStatusCode: Integer; const image: ICefImage);
  TCefCookieVisitorProc                = {$IFDEF DELPHI12_UP}reference to{$ENDIF} function(const name, value, domain, path: ustring; secure, httponly, hasExpires: Boolean; const creation, lastAccess, expires: TDateTime; count, total: Integer; same_site : TCefCookieSameSite; priority : TCefCookiePriority; out deleteCookie: Boolean): Boolean;
  TCefMediaRouteCreateCallbackProc     = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(result: TCefMediaRouterCreateResult; const error: ustring; const route: ICefMediaRoute);
  TCefMediaSinkDeviceInfoCallbackProc  = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const ip_address: ustring; port: integer; const model_name: ustring);


  {*
   *******************************************
   ************ Custom interfaces ************
   *******************************************
  *}

  /// <summary>
  /// Custom interface used to handle all the CEF functions related to CefStringList.
  /// </summary>
  ICefStringList = interface
    ['{DB24F301-2F64-48D6-A72E-33697748147E}']
    function  GetHandle: TCefStringList;
    function  GetSize: NativeUInt;
    function  GetValue(Index: NativeUInt): ustring;
    procedure Append(const Value: ustring);
    procedure Clear;
    function  Copy : TCefStringList;
    procedure CopyToStrings(const aStrings : TStrings);
    procedure AddStrings(const aStrings : TStrings);

    property Handle                   : TCefStringList read GetHandle;
    property Size                     : NativeUInt     read GetSize;
    property Value[index: NativeUInt] : ustring        read GetValue;
  end;

  /// <summary>
  /// Custom interface used to handle all the CEF functions related to CefStringMap.
  /// </summary>
  ICefStringMap = interface
    ['{A33EBC01-B23A-4918-86A4-E24A243B342F}']
    function  GetHandle: TCefStringMap;
    function  GetSize: NativeUInt;
    function  Find(const Key: ustring): ustring;
    function  GetKey(Index: NativeUInt): ustring;
    function  GetValue(Index: NativeUInt): ustring;
    function  Append(const Key, Value: ustring) : boolean;
    procedure Clear;

    property Handle                   : TCefStringMap read GetHandle;
    property Size                     : NativeUInt    read GetSize;
    property Key[index: NativeUInt]   : ustring       read GetKey;
    property Value[index: NativeUInt] : ustring       read GetValue;
  end;

  /// <summary>
  /// Custom interface used to handle all the CEF functions related to CefStringMultimap.
  /// </summary>
  ICefStringMultimap = interface
    ['{583ED0C2-A9D6-4034-A7C9-20EC7E47F0C7}']
    function  GetHandle: TCefStringMultimap;
    function  GetSize: NativeUInt;
    function  FindCount(const Key: ustring): NativeUInt;
    function  GetEnumerate(const Key: ustring; ValueIndex: NativeUInt): ustring;
    function  GetKey(Index: NativeUInt): ustring;
    function  GetValue(Index: NativeUInt): ustring;
    function  Append(const Key, Value: ustring) : boolean;
    procedure Clear;

    property Handle                                                  : TCefStringMultimap read GetHandle;
    property Size                                                    : NativeUInt         read GetSize;
    property Key[index: NativeUInt]                                  : ustring            read GetKey;
    property Value[index: NativeUInt]                                : ustring            read GetValue;
    property Enumerate[const aKey: ustring; aValueIndex: NativeUInt] : ustring            read GetEnumerate;
  end;

  /// <summary>
  /// Custom interface used to handle all the TCefApplicationCore events.
  /// </summary>
  IApplicationCoreEvents = interface
    ['{55E99E25-A05D-46D5-B3A4-C8C2E71C1F4D}']

    // ICefApp
    procedure doOnBeforeCommandLineProcessing(const processType: ustring; const commandLine: ICefCommandLine);
    procedure doOnRegisterCustomSchemes(const registrar: TCefSchemeRegistrarRef);

    // ICefBrowserProcessHandler
    procedure doOnRegisterCustomPreferences(type_: TCefPreferencesType; registrar: PCefPreferenceRegistrar);
    procedure doOnContextInitialized;
    procedure doOnBeforeChildProcessLaunch(const commandLine: ICefCommandLine);
    procedure doOnAlreadyRunningAppRelaunch(const commandLine: ICefCommandLine; const current_directory: ustring; var aResult: boolean);
    procedure doOnScheduleMessagePumpWork(const delayMs: Int64);
    procedure doGetDefaultClient(var aClient : ICefClient);
    procedure doGetDefaultRequestContextHandler(var aRequestContextHandler : ICefRequestContextHandler);

    // ICefResourceBundleHandler
    function  doGetLocalizedString(stringid: Integer; var stringVal: ustring): Boolean;
    function  doGetDataResource(resourceId: Integer; var data: Pointer; var dataSize: NativeUInt): Boolean;
    function  doGetDataResourceForScale(resourceId: Integer; scaleFactor: TCefScaleFactor; var data: Pointer; var dataSize: NativeUInt): Boolean;

    // ICefRenderProcessHandler
    procedure doOnWebKitInitialized;
    procedure doOnBrowserCreated(const browser: ICefBrowser; const extra_info: ICefDictionaryValue);
    procedure doOnBrowserDestroyed(const browser: ICefBrowser);
    procedure doOnContextCreated(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context);
    procedure doOnContextReleased(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context);
    procedure doOnUncaughtException(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context; const V8Exception: ICefV8Exception; const stackTrace: ICefV8StackTrace);
    procedure doOnFocusedNodeChanged(const browser: ICefBrowser; const frame: ICefFrame; const node: ICefDomNode);
    procedure doOnProcessMessageReceived(const browser: ICefBrowser; const frame: ICefFrame; sourceProcess: TCefProcessId; const aMessage: ICefProcessMessage; var aHandled : boolean);

    // ICefLoadHandler
    procedure doOnLoadingStateChange(const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
    procedure doOnLoadStart(const browser: ICefBrowser; const frame: ICefFrame; transitionType: TCefTransitionType);
    procedure doOnLoadEnd(const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: Integer);
    procedure doOnLoadError(const browser: ICefBrowser; const frame: ICefFrame; errorCode: TCefErrorCode; const errorText, failedUrl: ustring);
  end;

  /// <summary>
  /// Custom interface used to handle all the TChromiumCore events.
  /// </summary>
  IChromiumEvents = interface
    ['{0C139DB1-0349-4D7F-8155-76FEA6A0126D}']
    procedure GetSettings(var settings: TCefBrowserSettings);

    // ICefClient
    function  doOnProcessMessageReceived(const browser: ICefBrowser; const frame: ICefFrame; sourceProcess: TCefProcessId; const message: ICefProcessMessage): Boolean;

    // ICefLoadHandler
    procedure doOnLoadingStateChange(const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
    procedure doOnLoadStart(const browser: ICefBrowser; const frame: ICefFrame; transitionType: TCefTransitionType);
    procedure doOnLoadEnd(const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: Integer);
    procedure doOnLoadError(const browser: ICefBrowser; const frame: ICefFrame; errorCode: TCefErrorCode; const errorText, failedUrl: ustring);

    // ICefFocusHandler
    procedure doOnTakeFocus(const browser: ICefBrowser; next: Boolean);
    function  doOnSetFocus(const browser: ICefBrowser; source: TCefFocusSource): Boolean;
    procedure doOnGotFocus(const browser: ICefBrowser);

    // ICefContextMenuHandler
    procedure doOnBeforeContextMenu(const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; const model: ICefMenuModel);
    function  doRunContextMenu(const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; const model: ICefMenuModel; const callback: ICefRunContextMenuCallback): Boolean;
    function  doOnContextMenuCommand(const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; commandId: Integer; eventFlags: TCefEventFlags): Boolean;
    procedure doOnContextMenuDismissed(const browser: ICefBrowser; const frame: ICefFrame);
    function  doRunQuickMenu(const browser: ICefBrowser; const frame: ICefFrame; location: PCefPoint; size: PCefSize; edit_state_flags: TCefQuickMenuEditStateFlags; const callback: ICefRunQuickMenuCallback): boolean;
    function  doOnQuickMenuCommand(const browser: ICefBrowser; const frame: ICefFrame; command_id: integer; event_flags: TCefEventFlags): boolean;
    procedure doOnQuickMenuDismissed(const browser: ICefBrowser; const frame: ICefFrame);

    // ICefKeyboardHandler
    function  doOnPreKeyEvent(const browser: ICefBrowser; const event: PCefKeyEvent; osEvent: TCefEventHandle; out isKeyboardShortcut: Boolean): Boolean;
    function  doOnKeyEvent(const browser: ICefBrowser; const event: PCefKeyEvent; osEvent: TCefEventHandle): Boolean;

    // ICefDisplayHandler
    procedure doOnAddressChange(const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
    procedure doOnTitleChange(const browser: ICefBrowser; const title: ustring);
    procedure doOnFaviconUrlChange(const browser: ICefBrowser; const iconUrls: TStrings);
    procedure doOnFullScreenModeChange(const browser: ICefBrowser; fullscreen: Boolean);
    function  doOnTooltip(const browser: ICefBrowser; var text: ustring): Boolean;
    procedure doOnStatusMessage(const browser: ICefBrowser; const value: ustring);
    function  doOnConsoleMessage(const browser: ICefBrowser; level: TCefLogSeverity; const message, source: ustring; line: Integer): Boolean;
    function  doOnAutoResize(const browser: ICefBrowser; const new_size: PCefSize): Boolean;
    procedure doOnLoadingProgressChange(const browser: ICefBrowser; const progress: double);
    procedure doOnCursorChange(const browser: ICefBrowser; cursor_: TCefCursorHandle; cursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo; var aResult : boolean);
    procedure doOnMediaAccessChange(const browser: ICefBrowser; has_video_access, has_audio_access: boolean);

    // ICefDownloadHandler
    function  doOnCanDownload(const browser: ICefBrowser; const url, request_method: ustring): boolean;
    function  doOnBeforeDownload(const browser: ICefBrowser; const downloadItem: ICefDownloadItem; const suggestedName: ustring; const callback: ICefBeforeDownloadCallback): boolean;
    procedure doOnDownloadUpdated(const browser: ICefBrowser; const downloadItem: ICefDownloadItem; const callback: ICefDownloadItemCallback);

    // ICefJsDialogHandler
    function  doOnJsdialog(const browser: ICefBrowser; const originUrl: ustring; dialogType: TCefJsDialogType; const messageText, defaultPromptText: ustring; const callback: ICefJsDialogCallback; out suppressMessage: Boolean): Boolean;
    function  doOnBeforeUnloadDialog(const browser: ICefBrowser; const messageText: ustring; isReload: Boolean; const callback: ICefJsDialogCallback): Boolean;
    procedure doOnResetDialogState(const browser: ICefBrowser);
    procedure doOnDialogClosed(const browser: ICefBrowser);

    // ICefLifeSpanHandler
    function  doOnBeforePopup(const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean): Boolean;
    procedure doOnBeforeDevToolsPopup(const browser: ICefBrowser; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var use_default_window: boolean);
    procedure doOnAfterCreated(const browser: ICefBrowser);
    procedure doOnBeforeClose(const browser: ICefBrowser);
    function  doOnClose(const browser: ICefBrowser): Boolean;

    // ICefRequestHandler
    function  doOnBeforeBrowse(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; user_gesture, isRedirect: Boolean): Boolean;
    function  doOnOpenUrlFromTab(const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean): Boolean;
    procedure doGetResourceRequestHandler_ReqHdlr(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; is_navigation, is_download: boolean; const request_initiator: ustring; var disable_default_handling: boolean; var aResourceRequestHandler : ICefResourceRequestHandler);
    function  doOnGetAuthCredentials(const browser: ICefBrowser; const originUrl: ustring; isProxy: Boolean; const host: ustring; port: Integer; const realm, scheme: ustring; const callback: ICefAuthCallback): Boolean;
    function  doOnCertificateError(const browser: ICefBrowser; certError: TCefErrorcode; const requestUrl: ustring; const sslInfo: ICefSslInfo; const callback: ICefCallback): Boolean;
    function  doOnSelectClientCertificate(const browser: ICefBrowser; isProxy: boolean; const host: ustring; port: integer; certificatesCount: NativeUInt; const certificates: TCefX509CertificateArray; const callback: ICefSelectClientCertificateCallback): boolean;
    procedure doOnRenderViewReady(const browser: ICefBrowser);
    function  doOnRenderProcessUnresponsive(const browser: ICefBrowser; const callback: ICefUnresponsiveProcessCallback): boolean;
    procedure doOnRenderProcessResponsive(const browser: ICefBrowser);
    procedure doOnRenderProcessTerminated(const browser: ICefBrowser; status: TCefTerminationStatus; error_code: integer; const error_string: ustring);
    procedure doOnDocumentAvailableInMainFrame(const browser: ICefBrowser);

    // ICefResourceRequestHandler
    function  doOnBeforeResourceLoad(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const callback: ICefCallback): TCefReturnValue;
    procedure doOnGetResourceHandler(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; var aResourceHandler: ICefResourceHandler);
    procedure doOnResourceRedirect(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; var newUrl: ustring);
    function  doOnResourceResponse(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse): Boolean;
    procedure doOnGetResourceResponseFilter(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; var aResponseFilter: ICefResponseFilter);
    procedure doOnResourceLoadComplete(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; status: TCefUrlRequestStatus; receivedContentLength: Int64);
    procedure doOnProtocolExecution(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; var allowOsExecution: Boolean);

    // ICefCookieAccessFilter
    function  doCanSendCookie(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const cookie: PCefCookie): boolean;
    function  doCanSaveCookie(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; const cookie: PCefCookie): boolean;

    // ICefDialogHandler
    function  doOnFileDialog(const browser: ICefBrowser; mode: TCefFileDialogMode; const title, defaultFilePath: ustring; const acceptFilters, accept_extensions, accept_descriptions: TStrings; const callback: ICefFileDialogCallback): Boolean;

    // ICefRenderHandler
    procedure doOnGetAccessibilityHandler(var aAccessibilityHandler : ICefAccessibilityHandler);
    function  doOnGetRootScreenRect(const browser: ICefBrowser; var rect: TCefRect): Boolean;
    procedure doOnGetViewRect(const browser: ICefBrowser; var rect: TCefRect);
    function  doOnGetScreenPoint(const browser: ICefBrowser; viewX, viewY: Integer; var screenX, screenY: Integer): Boolean;
    function  doOnGetScreenInfo(const browser: ICefBrowser; var screenInfo: TCefScreenInfo): Boolean;
    procedure doOnPopupShow(const browser: ICefBrowser; show: Boolean);
    procedure doOnPopupSize(const browser: ICefBrowser; const rect: PCefRect);
    procedure doOnPaint(const browser: ICefBrowser; type_: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; const buffer: Pointer; width, height: Integer);
    procedure doOnAcceleratedPaint(const browser: ICefBrowser; type_: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; const info: PCefAcceleratedPaintInfo);
    procedure doGetTouchHandleSize(const browser: ICefBrowser; orientation: TCefHorizontalAlignment; var size: TCefSize);
    procedure doOnTouchHandleStateChanged(const browser: ICefBrowser; const state: TCefTouchHandleState);
    function  doOnStartDragging(const browser: ICefBrowser; const dragData: ICefDragData; allowedOps: TCefDragOperations; x, y: Integer): Boolean;
    procedure doOnUpdateDragCursor(const browser: ICefBrowser; operation: TCefDragOperation);
    procedure doOnScrollOffsetChanged(const browser: ICefBrowser; x, y: Double);
    procedure doOnIMECompositionRangeChanged(const browser: ICefBrowser; const selected_range: PCefRange; character_boundsCount: NativeUInt; const character_bounds: PCefRect);
    procedure doOnTextSelectionChanged(const browser: ICefBrowser; const selected_text: ustring; const selected_range: PCefRange);
    procedure doOnVirtualKeyboardRequested(const browser: ICefBrowser; input_mode: TCefTextInpuMode);

    // ICefDragHandler
    function  doOnDragEnter(const browser: ICefBrowser; const dragData: ICefDragData; mask: TCefDragOperations): Boolean;
    procedure doOnDraggableRegionsChanged(const browser: ICefBrowser; const frame: ICefFrame; regionsCount: NativeUInt; const regions: PCefDraggableRegionArray);

    // ICefFindHandler
    procedure doOnFindResult(const browser: ICefBrowser; identifier, count: Integer; const selectionRect: PCefRect; activeMatchOrdinal: Integer; finalUpdate: Boolean);

    // ICefRequestContextHandler
    procedure doOnRequestContextInitialized(const request_context: ICefRequestContext);
    procedure doGetResourceRequestHandler_ReqCtxHdlr(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; is_navigation, is_download: boolean; const request_initiator: ustring; var disable_default_handling: boolean; var aResourceRequestHandler : ICefResourceRequestHandler);

    // ICefMediaObserver
    procedure doOnSinks(const sinks: TCefMediaSinkArray);
    procedure doOnRoutes(const routes: TCefMediaRouteArray);
    procedure doOnRouteStateChanged(const route: ICefMediaRoute; state: TCefMediaRouteConnectionState);
    procedure doOnRouteMessageReceived(const route: ICefMediaRoute; const message_: ustring);

    // ICefAudioHandler
    procedure doOnGetAudioParameters(const browser: ICefBrowser; var params: TCefAudioParameters; var aResult: boolean);
    procedure doOnAudioStreamStarted(const browser: ICefBrowser; const params: TCefAudioParameters; channels: integer);
    procedure doOnAudioStreamPacket(const browser: ICefBrowser; const data : PPSingle; frames: integer; pts: int64);
    procedure doOnAudioStreamStopped(const browser: ICefBrowser);
    procedure doOnAudioStreamError(const browser: ICefBrowser; const message_: ustring);

    // ICefDevToolsMessageObserver
    procedure doOnDevToolsMessage(const browser: ICefBrowser; const message_: Pointer; message_size: NativeUInt; var aHandled: boolean);
    procedure doOnDevToolsMethodResult(const browser: ICefBrowser; message_id: integer; success: boolean; const result: Pointer; result_size: NativeUInt);
    procedure doOnDevToolsEvent(const browser: ICefBrowser; const method: ustring; const params: Pointer; params_size: NativeUInt);
    procedure doOnDevToolsAgentAttached(const browser: ICefBrowser);
    procedure doOnDevToolsAgentDetached(const browser: ICefBrowser);

    // ICefPrintHandler
    procedure doOnPrintStart(const browser: ICefBrowser);
    procedure doOnPrintSettings(const browser: ICefBrowser; const settings: ICefPrintSettings; getDefaults: boolean);
    procedure doOnPrintDialog(const browser: ICefBrowser; hasSelection: boolean; const callback: ICefPrintDialogCallback; var aResult : boolean);
    procedure doOnPrintJob(const browser: ICefBrowser; const documentName, PDFFilePath: ustring; const callback: ICefPrintJobCallback; var aResult : boolean);
    procedure doOnPrintReset(const browser: ICefBrowser);
    procedure doOnGetPDFPaperSize(const browser: ICefBrowser; deviceUnitsPerInch: Integer; var aResult : TCefSize);

    // ICefFrameHandler
    procedure doOnFrameCreated(const browser: ICefBrowser; const frame: ICefFrame);
    procedure doOnFrameAttached(const browser: ICefBrowser; const frame: ICefFrame; reattached: boolean);
    procedure doOnFrameDetached(const browser: ICefBrowser; const frame: ICefFrame);
    procedure doOnMainFrameChanged(const browser: ICefBrowser; const old_frame, new_frame: ICefFrame);

    // ICefCommandHandler
    function  doOnChromeCommand(const browser: ICefBrowser; command_id: integer; disposition: TCefWindowOpenDisposition): boolean;
    function  doOnIsChromeAppMenuItemVisible(const browser: ICefBrowser; command_id: integer): boolean;
    function  doOnIsChromeAppMenuItemEnabled(const browser: ICefBrowser; command_id: integer): boolean;
    function  doOnIsChromePageActionIconVisible(icon_type: TCefChromePageActionIconType): boolean;
    function  doOnIsChromeToolbarButtonVisible(button_type: TCefChromeToolbarButtonType): boolean;

    // ICefPermissionHandler
    function  doOnRequestMediaAccessPermission(const browser: ICefBrowser; const frame: ICefFrame; const requesting_origin: ustring; requested_permissions: cardinal; const callback: ICefMediaAccessCallback): boolean;
    function  doOnShowPermissionPrompt(const browser: ICefBrowser; prompt_id: uint64; const requesting_origin: ustring; requested_permissions: cardinal; const callback: ICefPermissionPromptCallback): boolean;
    procedure doOnDismissPermissionPrompt(const browser: ICefBrowser; prompt_id: uint64; result: TCefPermissionRequestResult);

    // Custom
    procedure doCookiesDeleted(numDeleted : integer);
    procedure doPdfPrintFinished(aResultOK : boolean);
    procedure doTextResultAvailable(const aText : ustring);
    procedure doUpdatePreferences(const aBrowser: ICefBrowser);
    procedure doUpdateOwnPreferences;
    function  doSavePreferences : boolean;
    procedure doResolvedHostAvailable(result: TCefErrorCode; const resolvedIps: TStrings);
    function  doNavigationVisitorResultAvailable(const entry: ICefNavigationEntry; current: Boolean; index, total: Integer) : boolean;
    procedure doDownloadImageFinished(const imageUrl: ustring; httpStatusCode: Integer; const image: ICefImage);
    procedure doOnCookiesStoreFlushed;
    procedure doCertificateExceptionsCleared;
    procedure doHttpAuthCredentialsCleared;
    procedure doAllConnectionsClosed;
    procedure doOnExecuteTaskOnCefThread(aTaskID : cardinal);
    procedure doOnCookiesVisited(const name_, value, domain, path: ustring; secure, httponly, hasExpires: Boolean; const creation, lastAccess, expires: TDateTime; count, total, aID : Integer; same_site : TCefCookieSameSite; priority : TCefCookiePriority; var aDeleteCookie, aResult : Boolean);
    procedure doOnCookieVisitorDestroyed(aID : integer);
    procedure doOnCookieSet(aSuccess : boolean; aID : integer);
    procedure doUpdateZoomStep(aInc : boolean);
    procedure doUpdateZoomPct(aInc : boolean);
    procedure doSetZoomLevel(const aValue : double);
    procedure doSetZoomPct(const aValue : double);
    procedure doSetZoomStep(aValue : byte);
    procedure doReadZoom;
    procedure doMediaRouteCreateFinished(result: TCefMediaRouterCreateResult; const error: ustring; const route: ICefMediaRoute);
    procedure doOnMediaSinkDeviceInfo(const ip_address: ustring; port: integer; const model_name: ustring);
    procedure doBrowserNavigation(aTask : TCefBrowserNavigation);
    procedure doSetAudioMuted(aValue : boolean);
    procedure doToggleAudioMuted;
    procedure doEnableFocus;
    function  doTryCloseBrowser : boolean;
    function  MustCreateAudioHandler : boolean;
    function  MustCreateCommandHandler : boolean;
    function  MustCreateLoadHandler : boolean;
    function  MustCreateFocusHandler : boolean;
    function  MustCreateContextMenuHandler : boolean;
    function  MustCreateDialogHandler : boolean;
    function  MustCreateKeyboardHandler : boolean;
    function  MustCreateDisplayHandler : boolean;
    function  MustCreateDownloadHandler : boolean;
    function  MustCreateJsDialogHandler : boolean;
    function  MustCreateLifeSpanHandler : boolean;
    function  MustCreateRenderHandler : boolean;
    function  MustCreateRequestHandler : boolean;
    function  MustCreateDragHandler : boolean;
    function  MustCreateFindHandler : boolean;
    function  MustCreateResourceRequestHandler : boolean;
    function  MustCreateCookieAccessFilter : boolean;
    function  MustCreateMediaObserver : boolean;
    function  MustCreatePrintHandler : boolean;
    function  MustCreateFrameHandler : boolean;
    function  MustCreatePermissionHandler : boolean;
    function  GetComponentID : integer;

    property ComponentID : integer read GetComponentID;
  end;

  /// <summary>
  /// Custom interface used to handle all the TCEFServerComponent events.
  /// </summary>
  IServerEvents = interface
    ['{06A1B3C6-0967-4F6C-A751-8AA3A29E2FF5}']
    // ICefServerHandler
    procedure doOnServerCreated(const server: ICefServer);
    procedure doOnServerDestroyed(const server: ICefServer);
    procedure doOnClientConnected(const server: ICefServer; connection_id: Integer);
    procedure doOnClientDisconnected(const server: ICefServer; connection_id: Integer);
    procedure doOnHttpRequest(const server: ICefServer; connection_id: Integer; const client_address: ustring; const request: ICefRequest);
    procedure doOnWebSocketRequest(const server: ICefServer; connection_id: Integer; const client_address: ustring; const request: ICefRequest; const callback: ICefCallback);
    procedure doOnWebSocketConnected(const server: ICefServer; connection_id: Integer);
    procedure doOnWebSocketMessage(const server: ICefServer; connection_id: Integer; const data: Pointer; data_size: NativeUInt);
  end;

  /// <summary>
  /// Custom interface used to handle all the TCEFUrlRequestClientComponent events.
  /// </summary>
  ICEFUrlRequestClientEvents = interface
    ['{1AA800A7-56A1-43CA-A224-49368F18BDD8}']
    // ICefUrlrequestClient
    procedure doOnRequestComplete(const request: ICefUrlRequest);
    procedure doOnUploadProgress(const request: ICefUrlRequest; current, total: Int64);
    procedure doOnDownloadProgress(const request: ICefUrlRequest; current, total: Int64);
    procedure doOnDownloadData(const request: ICefUrlRequest; data: Pointer; dataLength: NativeUInt);
    function  doOnGetAuthCredentials(isProxy: Boolean; const host: ustring; port: Integer; const realm, scheme: ustring; const callback: ICefAuthCallback): Boolean;

    // Custom
    procedure doOnCreateURLRequest;
    function  GetComponentID : integer;

    property ComponentID : integer read GetComponentID;
  end;

  /// <summary>
  /// Custom interface used to handle the ICefViewDelegate events.
  /// </summary>
  ICefViewDelegateEvents = interface
    ['{74DDDB37-8F08-4672-BDB6-55CA2CD374ED}']
    // ICefViewDelegate
    procedure doOnGetPreferredSize(const view: ICefView; var aResult : TCefSize);
    procedure doOnGetMinimumSize(const view: ICefView; var aResult : TCefSize);
    procedure doOnGetMaximumSize(const view: ICefView; var aResult : TCefSize);
    procedure doOnGetHeightForWidth(const view: ICefView; width: Integer; var aResult: Integer);
    procedure doOnParentViewChanged(const view: ICefView; added: boolean; const parent: ICefView);
    procedure doOnChildViewChanged(const view: ICefView; added: boolean; const child: ICefView);
    procedure doOnWindowChanged(const view: ICefView; added: boolean);
    procedure doOnLayoutChanged(const view: ICefView; new_bounds: TCefRect);
    procedure doOnFocus(const view: ICefView);
    procedure doOnBlur(const view: ICefView);
    procedure doOnThemeChanged(const view: ICefView);

    // Custom
    procedure doCreateCustomView;
    function  GetComponentID : integer;

    property ComponentID : integer read GetComponentID;
  end;

  /// <summary>
  /// Custom interface used to handle all the ICefTextfieldDelegate events.
  /// </summary>
  ICefTextfieldDelegateEvents = interface(ICefViewDelegateEvents)
    ['{682480E0-C786-4E65-B950-4FF2B13B97B9}']
    procedure doOnKeyEvent(const textfield: ICefTextfield; const event: TCefKeyEvent; var aResult : boolean);
    procedure doOnAfterUserAction(const textfield: ICefTextfield);
  end;

  /// <summary>
  /// Custom interface used to handle all the ICefBrowserViewDelegate events.
  /// </summary>
  ICefBrowserViewDelegateEvents = interface(ICefViewDelegateEvents)
    ['{AB94B875-63C6-4FEF-BB30-0816402ABA1C}']
    procedure doOnBrowserCreated(const browser_view: ICefBrowserView; const browser: ICefBrowser);
    procedure doOnBrowserDestroyed(const browser_view: ICefBrowserView; const browser: ICefBrowser);
    procedure doOnGetDelegateForPopupBrowserView(const browser_view: ICefBrowserView; const settings: TCefBrowserSettings; const client: ICefClient; is_devtools: boolean; var aResult : ICefBrowserViewDelegate);
    procedure doOnPopupBrowserViewCreated(const browser_view, popup_browser_view: ICefBrowserView; is_devtools: boolean; var aResult : boolean);
    procedure doOnGetChromeToolbarType(const browser_view: ICefBrowserView; var aChromeToolbarType: TCefChromeToolbarType);
    procedure doOnUseFramelessWindowForPictureInPicture(const browser_view: ICefBrowserView; var aResult: boolean);
    procedure doOnGestureCommand(const browser_view: ICefBrowserView; gesture_command: TCefGestureCommand; var aResult : boolean);
    procedure doOnGetBrowserRuntimeStyle(var aResult : TCefRuntimeStyle);
  end;

  /// <summary>
  /// Custom interface used to handle all the ICefButtonDelegate events.
  /// </summary>
  ICefButtonDelegateEvents = interface(ICefViewDelegateEvents)
    ['{E8DF70BE-5DEB-42CF-AF86-B0FF1040498E}']
    procedure doOnButtonPressed(const button: ICefButton);
    procedure doOnButtonStateChanged(const button: ICefButton);
  end;

  /// <summary>
  /// Custom interface used to handle all the ICefMenuButtonDelegate events.
  /// </summary>
  ICefMenuButtonDelegateEvents = interface(ICefButtonDelegateEvents)
    ['{DA36DD60-7609-4576-BB8E-6A55FD48C680}']
    procedure doOnMenuButtonPressed(const menu_button: ICefMenuButton; const screen_point: TCefPoint; const button_pressed_lock: ICefMenuButtonPressedLock);
  end;

  /// <summary>
  /// Custom interface used to handle all the ICefPanelDelegate events.
  /// </summary>
  ICefPanelDelegateEvents = interface(ICefViewDelegateEvents)
    ['{F1F2963F-82C3-48F0-9B9C-7C213BACB96B}']
  end;

  /// <summary>
  /// Custom interface used to handle all the ICefWindowDelegate events.
  /// </summary>
  ICefWindowDelegateEvents = interface(ICefPanelDelegateEvents)
    ['{05C19A41-E75D-459E-AD4D-C8A0CA4A49D3}']
    procedure doOnWindowCreated(const window_: ICefWindow);
    procedure doOnWindowClosing(const window_: ICefWindow);
    procedure doOnWindowDestroyed(const window_: ICefWindow);
    procedure doOnWindowActivationChanged(const window_: ICefWindow; active: boolean);
    procedure doOnWindowBoundsChanged(const window_: ICefWindow; const new_bounds: TCefRect);
    procedure doOnWindowFullscreenTransition(const window_: ICefWindow; is_completed: boolean);
    procedure doOnGetParentWindow(const window_: ICefWindow; var is_menu, can_activate_menu: boolean; var aResult : ICefWindow);
    procedure doOnIsWindowModalDialog(const window_: ICefWindow; var aResult : boolean);
    procedure doOnGetInitialBounds(const window_: ICefWindow; var aResult : TCefRect);
    procedure doOnGetInitialShowState(const window_: ICefWindow; var aResult : TCefShowState);
    procedure doOnIsFrameless(const window_: ICefWindow; var aResult : boolean);
    procedure doOnWithStandardWindowButtons(const window_: ICefWindow; var aResult : boolean);
    procedure doOnGetTitlebarHeight(const window_: ICefWindow; var titlebar_height: Single; var aResult : boolean);
    procedure doOnAcceptsFirstMouse(const window_: ICefWindow; var aResult: TCefState);
    procedure doOnCanResize(const window_: ICefWindow; var aResult : boolean);
    procedure doOnCanMaximize(const window_: ICefWindow; var aResult : boolean);
    procedure doOnCanMinimize(const window_: ICefWindow; var aResult : boolean);
    procedure doOnCanClose(const window_: ICefWindow; var aResult : boolean);
    procedure doOnAccelerator(const window_: ICefWindow; command_id: Integer; var aResult : boolean);
    procedure doOnKeyEvent(const window_: ICefWindow; const event: TCefKeyEvent; var aResult : boolean);
    procedure doOnThemeColorsChanged(const window_: ICefWindow; chrome_theme: Integer);
    procedure doOnGetWindowRuntimeStyle(var aResult: TCefRuntimeStyle);
    procedure doOnGetLinuxWindowProperties(const window_: ICefWindow; var properties: TLinuxWindowProperties; var aResult: boolean);
  end;

  {*
   *******************************************
   ************** CEF interfaces *************
   *******************************************
  *}

  /// <summary>
  /// All ref-counted framework interfaces must inherit from this interface.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefBaseRefCounted">Implements TCefBaseRefCounted</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_base_capi.h">CEF source file: /include/capi/cef_base_capi.h (cef_base_ref_counted_t)</see></para>
  /// </remarks>
  ICefBaseRefCounted = interface
    ['{1F9A7B44-DCDC-4477-9180-3ADD44BDEB7B}']
    /// <summary>
    /// Called to increment the reference count for the object. Should be called
    /// for every new copy of a pointer to a given object.
    /// </summary>
    function Wrap: Pointer;
    /// <summary>
    /// Compares the aData pointer with the FData field if the current instance.
    /// </summary>
    function SameAs(aData : Pointer) : boolean; overload;
    function SameAs(const aBaseRefCounted : ICefBaseRefCounted) : boolean; overload;
    /// <summary>
    /// Returns true (1) if the current reference count is 1.
    /// </summary>
    function HasOneRef : boolean;
    /// <summary>
    /// Returns true (1) if the current reference count is at least 1.
    /// </summary>
    function HasAtLeastOneRef : boolean;
    /// <summary>
    /// Releases all other instances.
    /// </summary>
    procedure DestroyOtherRefs;
  end;

  /// <summary>
  /// Callback interface for ICefBrowserHost.RunFileDialog. The functions of
  /// this interface will be called on the browser process UI thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefRunFileDialogCallback">Implements TCefRunFileDialogCallback</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_browser_capi.h">CEF source file: /include/capi/cef_browser_capi.h (cef_run_file_dialog_callback_t)</see></para>
  /// </remarks>
  ICefRunFileDialogCallback = interface(ICefBaseRefCounted)
    ['{59FCECC6-E897-45BA-873B-F09586C4BE47}']
    /// <summary>
    /// Called asynchronously after the file dialog is dismissed. |file_paths|
    /// will be a single value or a list of values depending on the dialog mode.
    /// If the selection was cancelled |file_paths| will be NULL.
    /// </summary>
    procedure OnFileDialogDismissed(const filePaths: TStrings);
  end;

  /// <summary>
  /// Callback interface for ICefBrowserHost.GetNavigationEntries. The
  /// functions of this interface will be called on the browser process UI thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefNavigationEntryVisitor">Implements TCefNavigationEntryVisitor</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_browser_capi.h">CEF source file: /include/capi/cef_browser_capi.h (cef_navigation_entry_visitor_t)</see></para>
  /// </remarks>
  ICefNavigationEntryVisitor = interface(ICefBaseRefCounted)
    ['{CC4D6BC9-0168-4C2C-98BA-45E9AA9CD619}']
    /// <summary>
    /// Method that will be executed. Do not keep a reference to |entry| outside
    /// of this callback. Return true (1) to continue visiting entries or false
    /// (0) to stop. |current| is true (1) if this entry is the currently loaded
    /// navigation entry. |index| is the 0-based index of this entry and |total|
    /// is the total number of entries.
    /// </summary>
    function Visit(const entry: ICefNavigationEntry; current: Boolean; index, total: Integer): Boolean;
  end;

  /// <summary>
  /// Callback interface for ICefBrowserHost.PrintToPDF. The functions of this
  /// interface will be called on the browser process UI thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefPdfPrintCallback">Implements TCefPdfPrintCallback</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_browser_capi.h">CEF source file: /include/capi/cef_browser_capi.h (cef_pdf_print_callback_t)</see></para>
  /// </remarks>
  ICefPdfPrintCallback = interface(ICefBaseRefCounted)
    ['{F1CC58E9-2C30-4932-91AE-467C8D8EFB8E}']
    /// <summary>
    /// Method that will be executed when the PDF printing has completed. |path|
    /// is the output path. |ok| will be true (1) if the printing completed
    /// successfully or false (0) otherwise.
    /// </summary>
    procedure OnPdfPrintFinished(const path: ustring; ok: Boolean);
  end;

  /// <summary>
  /// Callback interface for ICefBrowserHost.DownloadImage. The functions of
  /// this interface will be called on the browser process UI thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefDownloadImageCallback">Implements TCefDownloadImageCallback</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_browser_capi.h">CEF source file: /include/capi/cef_browser_capi.h (cef_download_image_callback_t)</see></para>
  /// </remarks>
  ICefDownloadImageCallback = interface(ICefBaseRefCounted)
    ['{0C6E9032-27DF-4584-95C6-DC3C7CB63727}']
    /// <summary>
    /// Method that will be executed when the image download has completed.
    /// |image_url| is the URL that was downloaded and |http_status_code| is the
    /// resulting HTTP status code. |image| is the resulting image, possibly at
    /// multiple scale factors, or NULL if the download failed.
    /// </summary>
    procedure OnDownloadImageFinished(const imageUrl: ustring; httpStatusCode: Integer; const image: ICefImage);
  end;

  /// <summary>
  /// Interface used to represent the browser process aspects of a browser. The
  /// functions of this interface can only be called in the browser process. They
  /// may be called on any thread in that process unless otherwise indicated in
  /// the comments.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefBrowserHost">Implements TCefBrowserHost</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_browser_capi.h">CEF source file: /include/capi/cef_browser_capi.h (cef_browser_host_t)</see></para>
  /// </remarks>
  ICefBrowserHost = interface(ICefBaseRefCounted)
    ['{53AE02FF-EF5D-48C3-A43E-069DA9535424}']
    /// <summary>
    /// Returns the hosted browser object.
    /// </summary>
    function  GetBrowser: ICefBrowser;
    /// <summary>
    /// <para>Request that the browser close. Closing a browser is a multi-stage process
    /// that may complete either synchronously or asynchronously, and involves
    /// events such as TChromiumCore.OnClose (Alloy style only),
    /// TChromiumCore.OnBeforeClose, and a top-level window close
    /// handler such as TCEFWindowComponent.OnCanClose (or platform-specific
    /// equivalent). In some cases a close request may be delayed or canceled by
    /// the user. Using TryCloseBrowser() instead of CloseBrowser() is
    /// recommended for most use cases. See TChromiumCore.OnClose
    /// documentation for detailed usage and examples.</para>
    ///
    /// <para>If |aForceClose| is false (0) then JavaScript unload handlers, if any, may
    /// be fired and the close may be delayed or canceled by the user. If
    /// |aForceClose| is true (1) then the user will not be prompted and the close
    /// will proceed immediately (possibly asynchronously). If browser close is
    /// delayed and not canceled the default behavior is to call the top-level
    /// window close handler once the browser is ready to be closed. This default
    /// behavior can be changed for Alloy style browsers by implementing
    /// TChromiumCore.OnClose. IsReadyToBeClosed() can be used
    /// to detect mandatory browser close events when customizing close behavior
    /// on the browser process UI thread.</para>
    /// </summary>
    procedure CloseBrowser(forceClose: Boolean);
    /// <summary>
    /// Helper for closing a browser. This is similar in behavior to
    /// CLoseBrowser(false) but returns a boolean to reflect the immediate
    /// close status. Call this function from a top-level window close handler
    /// such as TCEFWindowComponent.OnCanClose (or platform-specific equivalent)
    /// to request that the browser close, and return the result to indicate if
    /// the window close should proceed. Returns false (0) if the close will be
    /// delayed (JavaScript unload handlers triggered but still pending) or true
    /// (1) if the close will proceed immediately (possibly asynchronously). See
    /// CloseBrowser() documentation for additional usage information. This
    /// function must be called on the browser process UI thread.
    /// </summary>
    function  TryCloseBrowser: Boolean;
    /// <summary>
    /// Returns true (1) if the browser is ready to be closed, meaning that the
    /// close has already been initiated and that JavaScript unload handlers have
    /// already executed or should be ignored. This can be used from a top-level
    /// window close handler such as TCEFWindowComponent.OnCanClose (or platform-
    /// specific equivalent) to distringuish between potentially cancelable
    /// browser close events (like the user clicking the top-level window close
    /// button before browser close has started) and mandatory browser close
    /// events (like JavaScript `window.close()` or after browser close has
    /// started in response to [Try]CloseBrowser()). Not completing the browser
    /// close for mandatory close events (when this function returns true (1))
    /// will leave the browser in a partially closed state that interferes with
    /// proper functioning. See CloseBrowser() documentation for additional usage
    /// information. This function must be called on the browser process UI
    /// thread.
    /// </summary>
    function  IsReadyToBeClosed: Boolean;
    /// <summary>
    /// Set whether the browser is focused.
    /// </summary>
    procedure SetFocus(focus: Boolean);
    /// <summary>
    /// Retrieve the window handle (if any) for this browser. If this browser is
    /// wrapped in a ICefBrowserView this function should be called on the
    /// browser process UI thread and it will return the handle for the top-level
    /// native window.
    /// </summary>
    function  GetWindowHandle: TCefWindowHandle;
    /// <summary>
    /// Retrieve the window handle (if any) of the browser that opened this
    /// browser. Will return NULL for non-popup browsers or if this browser is
    /// wrapped in a ICefBrowserView. This function can be used in combination
    /// with custom handling of modal windows.
    /// </summary>
    function  GetOpenerWindowHandle: TCefWindowHandle;
    /// <summary>
    /// Returns true (1) if this browser is wrapped in a ICefBrowserView.
    /// </summary>
    function  HasView: Boolean;
    /// <summary>
    /// Returns the client for this browser.
    /// </summary>
    function  GetClient: ICefClient;
    /// <summary>
    /// Returns the request context for this browser.
    /// </summary>
    function  GetRequestContext: ICefRequestContext;
    /// <summary>
    /// Returns true (1) if this browser can execute the specified zoom command.
    /// This function can only be called on the UI thread.
    /// </summary>
    function  CanZoom(command: TCefZoomCommand): boolean;
    /// <summary>
    /// Execute a zoom command in this browser. If called on the UI thread the
    /// change will be applied immediately. Otherwise, the change will be applied
    /// asynchronously on the UI thread.
    /// </summary>
    procedure Zoom(command: TCefZoomCommand);
    /// <summary>
    /// Get the default zoom level. This value will be 0.0 by default but can be
    /// configured. This function can only be called on the UI thread.
    /// </summary>
    function GetDefaultZoomLevel: Double;
    /// <summary>
    /// Get the current zoom level. This function can only be called on the UI
    /// thread.
    /// </summary>
    function  GetZoomLevel: Double;
    /// <summary>
    /// Change the zoom level to the specified value. Specify 0.0 to reset the
    /// zoom level to the default. If called on the UI thread the change will be
    /// applied immediately. Otherwise, the change will be applied asynchronously
    /// on the UI thread.
    /// </summary>
    procedure SetZoomLevel(const zoomLevel: Double);
    /// <summary>
    /// Call to run a file chooser dialog. Only a single file chooser dialog may
    /// be pending at any given time. |mode| represents the type of dialog to
    /// display. |title| to the title to be used for the dialog and may be NULL to
    /// show the default title ("Open" or "Save" depending on the mode).
    /// |default_file_path| is the path with optional directory and/or file name
    /// component that will be initially selected in the dialog. |accept_filters|
    /// are used to restrict the selectable file types and may any combination of
    /// (a) valid lower-cased MIME types (e.g. "text/*" or "image/*"), (b)
    /// individual file extensions (e.g. ".txt" or ".png"), or (c) combined
    /// description and file extension delimited using "|" and ";" (e.g. "Image
    /// Types|.png;.gif;.jpg"). |callback| will be executed after the dialog is
    /// dismissed or immediately if another dialog is already pending. The dialog
    /// will be initiated asynchronously on the UI thread.
    /// </summary>
    procedure RunFileDialog(mode: TCefFileDialogMode; const title, defaultFilePath: ustring; const acceptFilters: TStrings; const callback: ICefRunFileDialogCallback);
    /// <summary>
    /// Call to run a file chooser dialog. Only a single file chooser dialog may
    /// be pending at any given time. |mode| represents the type of dialog to
    /// display. |title| to the title to be used for the dialog and may be NULL to
    /// show the default title ("Open" or "Save" depending on the mode).
    /// |default_file_path| is the path with optional directory and/or file name
    /// component that will be initially selected in the dialog. |accept_filters|
    /// are used to restrict the selectable file types and may any combination of
    /// (a) valid lower-cased MIME types (e.g. "text/*" or "image/*"), (b)
    /// individual file extensions (e.g. ".txt" or ".png"), or (c) combined
    /// description and file extension delimited using "|" and ";" (e.g. "Image
    /// Types|.png;.gif;.jpg"). |callback| will be executed after the dialog is
    /// dismissed or immediately if another dialog is already pending. The dialog
    /// will be initiated asynchronously on the UI thread.
    /// </summary>
    procedure RunFileDialogProc(mode: TCefFileDialogMode; const title, defaultFilePath: ustring; const acceptFilters: TStrings; const callback: TCefRunFileDialogCallbackProc);
    /// <summary>
    /// Download the file at |url| using ICefDownloadHandler.
    /// </summary>
    procedure StartDownload(const url: ustring);
    /// <summary>
    /// Download |image_url| and execute |callback| on completion with the images
    /// received from the renderer. If |is_favicon| is true (1) then cookies are
    /// not sent and not accepted during download. Images with density independent
    /// pixel (DIP) sizes larger than |max_image_size| are filtered out from the
    /// image results. Versions of the image at different scale factors may be
    /// downloaded up to the maximum scale factor supported by the system. If
    /// there are no image results <= |max_image_size| then the smallest image is
    /// resized to |max_image_size| and is the only result. A |max_image_size| of
    /// 0 means unlimited. If |bypass_cache| is true (1) then |image_url| is
    /// requested from the server even if it is present in the browser cache.
    /// </summary>
    procedure DownloadImage(const imageUrl: ustring; isFavicon: Boolean; maxImageSize: cardinal; bypassCache: Boolean; const callback: ICefDownloadImageCallback);
    /// <summary>
    /// Print the current browser contents.
    /// </summary>
    procedure Print;
    /// <summary>
    /// Print the current browser contents to the PDF file specified by |path| and
    /// execute |callback| on completion. The caller is responsible for deleting
    /// |path| when done. For PDF printing to work on Linux you must implement the
    /// ICefPrintHandler.GetPdfPaperSize function.
    /// </summary>
    procedure PrintToPdf(const path: ustring; settings: PCefPdfPrintSettings; const callback: ICefPdfPrintCallback);
    /// <summary>
    /// Print the current browser contents to the PDF file specified by |path| and
    /// execute |callback| on completion. The caller is responsible for deleting
    /// |path| when done. For PDF printing to work on Linux you must implement the
    /// ICefPrintHandler.GetPdfPaperSize function.
    /// </summary>
    procedure PrintToPdfProc(const path: ustring; settings: PCefPdfPrintSettings; const callback: TOnPdfPrintFinishedProc);
    /// <summary>
    /// Search for |searchText|. |forward| indicates whether to search forward or
    /// backward within the page. |matchCase| indicates whether the search should
    /// be case-sensitive. |findNext| indicates whether this is the first request
    /// or a follow-up. The search will be restarted if |searchText| or
    /// |matchCase| change. The search will be stopped if |searchText| is NULL.
    /// The ICefFindHandler instance, if any, returned via
    /// ICefClient.GetFindHandler will be called to report find results.
    /// </summary>
    procedure Find(const searchText: ustring; forward_, matchCase, findNext: Boolean);
    /// <summary>
    /// Cancel all searches that are currently going on.
    /// </summary>
    procedure StopFinding(clearSelection: Boolean);
    /// <summary>
    /// Open developer tools (DevTools) in its own browser. The DevTools browser
    /// will remain associated with this browser. If the DevTools browser is
    /// already open then it will be focused, in which case the |windowInfo|,
    /// |client| and |settings| parameters will be ignored. If
    /// |inspectElementAt| is non-NULL then the element at the specified (x,y)
    /// location will be inspected. The |windowInfo| parameter will be ignored if
    /// this browser is wrapped in a ICefBrowserView.
    /// </summary>
    procedure ShowDevTools(const windowInfo: PCefWindowInfo; const client: ICefClient; const settings: PCefBrowserSettings; inspectElementAt: PCefPoint);
    /// <summary>
    /// Explicitly close the associated DevTools browser, if any.
    /// </summary>
    procedure CloseDevTools;
    /// <summary>
    /// Returns true (1) if this browser currently has an associated DevTools
    /// browser. Must be called on the browser process UI thread.
    /// </summary>
    function  HasDevTools: Boolean;
    /// <summary>
    /// Send a function call message over the DevTools protocol. |message| must be
    /// a UTF8-encoded JSON dictionary that contains "id" (int), "function"
    /// (string) and "params" (dictionary, optional) values. See the DevTools
    /// protocol documentation at https://chromedevtools.github.io/devtools-
    /// protocol/ for details of supported functions and the expected "params"
    /// dictionary contents. |message| will be copied if necessary. This function
    /// will return true (1) if called on the UI thread and the message was
    /// successfully submitted for validation, otherwise false (0). Validation
    /// will be applied asynchronously and any messages that fail due to
    /// formatting errors or missing parameters may be discarded without
    /// notification. Prefer ExecuteDevToolsMethod if a more structured approach
    /// to message formatting is desired.
    ///
    /// Every valid function call will result in an asynchronous function result
    /// or error message that references the sent message "id". Event messages are
    /// received while notifications are enabled (for example, between function
    /// calls for "Page.enable" and "Page.disable"). All received messages will be
    /// delivered to the observer(s) registered with AddDevToolsMessageObserver.
    /// See ICefDevToolsMessageObserver.OnDevToolsMessage documentation for
    /// details of received message contents.
    ///
    /// Usage of the SendDevToolsMessage, ExecuteDevToolsMethod and
    /// AddDevToolsMessageObserver functions does not require an active DevTools
    /// front-end or remote-debugging session. Other active DevTools sessions will
    /// continue to function independently. However, any modification of global
    /// browser state by one session may not be reflected in the UI of other
    /// sessions.
    ///
    /// Communication with the DevTools front-end (when displayed) can be logged
    /// for development purposes by passing the `--devtools-protocol-log-
    /// file=<path>` command-line flag.
    /// </summary>
    function  SendDevToolsMessage(const message_: ustring): boolean;
    /// <summary>
    /// Execute a function call over the DevTools protocol. This is a more
    /// structured version of SendDevToolsMessage. |message_id| is an incremental
    /// number that uniquely identifies the message (pass 0 to have the next
    /// number assigned automatically based on previous values). |function| is the
    /// function name. |params| are the function parameters, which may be NULL.
    /// See the DevTools protocol documentation (linked above) for details of
    /// supported functions and the expected |params| dictionary contents. This
    /// function will return the assigned message ID if called on the UI thread
    /// and the message was successfully submitted for validation, otherwise 0.
    /// See the SendDevToolsMessage documentation for additional usage
    /// information.
    /// </summary>
    function  ExecuteDevToolsMethod(message_id: integer; const method: ustring; const params: ICefDictionaryValue): Integer;
    /// <summary>
    /// Add an observer for DevTools protocol messages (function results and
    /// events). The observer will remain registered until the returned
    /// Registration object is destroyed. See the SendDevToolsMessage
    /// documentation for additional usage information.
    /// </summary>
    function  AddDevToolsMessageObserver(const observer: ICefDevToolsMessageObserver): ICefRegistration;
    /// <summary>
    /// Retrieve a snapshot of current navigation entries as values sent to the
    /// specified visitor. If |current_only| is true (1) only the current
    /// navigation entry will be sent, otherwise all navigation entries will be
    /// sent.
    /// </summary>
    procedure GetNavigationEntries(const visitor: ICefNavigationEntryVisitor; currentOnly: Boolean);
    /// <summary>
    /// Retrieve a snapshot of current navigation entries as values sent to the
    /// specified visitor. If |current_only| is true (1) only the current
    /// navigation entry will be sent, otherwise all navigation entries will be
    /// sent.
    /// </summary>
    procedure GetNavigationEntriesProc(const proc: TCefNavigationEntryVisitorProc; currentOnly: Boolean);
    /// <summary>
    /// If a misspelled word is currently selected in an editable node calling
    /// this function will replace it with the specified |word|.
    /// </summary>
    procedure ReplaceMisspelling(const word: ustring);
    /// <summary>
    /// Add the specified |word| to the spelling dictionary.
    /// </summary>
    procedure AddWordToDictionary(const word: ustring);
    /// <summary>
    /// Returns true (1) if window rendering is disabled.
    /// </summary>
    function  IsWindowRenderingDisabled: Boolean;
    /// <summary>
    /// Notify the browser that the widget has been resized. The browser will
    /// first call ICefRenderHandler.GetViewRect to get the new size and then
    /// call ICefRenderHandler.OnPaint asynchronously with the updated
    /// regions. This function is only used when window rendering is disabled.
    /// </summary>
    procedure WasResized;
    /// <summary>
    /// Notify the browser that it has been hidden or shown. Layouting and
    /// ICefRenderHandler.OnPaint notification will stop when the browser is
    /// hidden. This function is only used when window rendering is disabled.
    /// </summary>
    procedure WasHidden(hidden: Boolean);
    /// <summary>
    /// Send a notification to the browser that the screen info has changed. The
    /// browser will then call ICefRenderHandler.GetScreenInfo to update the
    /// screen information with the new values. This simulates moving the webview
    /// window from one display to another, or changing the properties of the
    /// current display. This function is only used when window rendering is
    /// disabled.
    /// </summary>
    procedure NotifyScreenInfoChanged;
    /// <summary>
    /// Invalidate the view. The browser will call ICefRenderHandler.OnPaint
    /// asynchronously. This function is only used when window rendering is
    /// disabled.
    /// </summary>
    procedure Invalidate(kind: TCefPaintElementType);
    /// <summary>
    /// Issue a BeginFrame request to Chromium.  Only valid when
    /// TCefWindowInfo.external_begin_frame_enabled is set to true (1).
    /// </summary>
    procedure SendExternalBeginFrame;
    /// <summary>
    /// Send a key event to the browser.
    /// </summary>
    procedure SendKeyEvent(const event: PCefKeyEvent);
    /// <summary>
    /// Send a mouse click event to the browser. The |x| and |y| coordinates are
    /// relative to the upper-left corner of the view.
    /// </summary>
    procedure SendMouseClickEvent(const event: PCefMouseEvent; type_: TCefMouseButtonType; mouseUp: Boolean; clickCount: Integer);
    /// <summary>
    /// Send a mouse move event to the browser. The |x| and |y| coordinates are
    /// relative to the upper-left corner of the view.
    /// </summary>
    procedure SendMouseMoveEvent(const event: PCefMouseEvent; mouseLeave: Boolean);
    /// <summary>
    /// Send a mouse wheel event to the browser. The |x| and |y| coordinates are
    /// relative to the upper-left corner of the view. The |deltaX| and |deltaY|
    /// values represent the movement delta in the X and Y directions
    /// respectively. In order to scroll inside select popups with window
    /// rendering disabled ICefRenderHandler.GetScreenPoint should be
    /// implemented properly.
    /// </summary>
    procedure SendMouseWheelEvent(const event: PCefMouseEvent; deltaX, deltaY: Integer);
    /// <summary>
    /// Send a touch event to the browser for a windowless browser.
    /// </summary>
    procedure SendTouchEvent(const event: PCefTouchEvent);
    /// <summary>
    /// Send a capture lost event to the browser.
    /// </summary>
    procedure SendCaptureLostEvent;
    /// <summary>
    /// Notify the browser that the window hosting it is about to be moved or
    /// resized. This function is only used on Windows and Linux.
    /// </summary>
    procedure NotifyMoveOrResizeStarted;
    /// <summary>
    /// Returns the maximum rate in frames per second (fps) that
    /// ICefRenderHandler.OnPaint will be called for a windowless browser. The
    /// actual fps may be lower if the browser cannot generate frames at the
    /// requested rate. The minimum value is 1 and the maximum value is 60
    /// (default 30). This function can only be called on the UI thread.
    /// </summary>
    function  GetWindowlessFrameRate : Integer;
    /// <summary>
    /// Set the maximum rate in frames per second (fps) that
    /// ICefRenderHandler.OnPaint will be called for a windowless browser.
    /// The actual fps may be lower if the browser cannot generate frames at the
    /// requested rate. The minimum value is 1 and the maximum value is 60
    /// (default 30). Can also be set at browser creation via
    /// TCefBrowserSettings.windowless_frame_rate.
    /// </summary>
    procedure SetWindowlessFrameRate(frameRate: Integer);
    /// <summary>
    /// Begins a new composition or updates the existing composition. Blink has a
    /// special node (a composition node) that allows the input function to change
    /// text without affecting other DOM nodes. |text| is the optional text that
    /// will be inserted into the composition node. |underlines| is an optional
    /// set of ranges that will be underlined in the resulting text.
    /// |replacement_range| is an optional range of the existing text that will be
    /// replaced. |selection_range| is an optional range of the resulting text
    /// that will be selected after insertion or replacement. The
    /// |replacement_range| value is only used on OS X.
    ///
    /// This function may be called multiple times as the composition changes.
    /// When the client is done making changes the composition should either be
    /// canceled or completed. To cancel the composition call
    /// ImeCancelComposition. To complete the composition call either
    /// ImeCommitText or ImeFinishComposingText. Completion is usually signaled
    /// when:
    ///
    /// 1. The client receives a WM_IME_COMPOSITION message with a GCS_RESULTSTR
    ///    flag (on Windows), or;
    /// 2. The client receives a "commit" signal of GtkIMContext (on Linux), or;
    /// 3. insertText of NSTextInput is called (on Mac).
    ///
    /// This function is only used when window rendering is disabled.
    /// </summary>
    procedure IMESetComposition(const text: ustring; const underlines : TCefCompositionUnderlineDynArray; const replacement_range, selection_range : PCefRange);
    /// <summary>
    /// Completes the existing composition by optionally inserting the specified
    /// |text| into the composition node. |replacement_range| is an optional range
    /// of the existing text that will be replaced. |relative_cursor_pos| is where
    /// the cursor will be positioned relative to the current cursor position. See
    /// comments on ImeSetComposition for usage. The |replacement_range| and
    /// |relative_cursor_pos| values are only used on OS X. This function is only
    /// used when window rendering is disabled.
    /// </summary>
    procedure IMECommitText(const text: ustring; const replacement_range : PCefRange; relative_cursor_pos : integer);
    /// <summary>
    /// Completes the existing composition by applying the current composition
    /// node contents. If |keep_selection| is false (0) the current selection, if
    /// any, will be discarded. See comments on ImeSetComposition for usage. This
    /// function is only used when window rendering is disabled.
    /// </summary>
    procedure IMEFinishComposingText(keep_selection : boolean);
    /// <summary>
    /// Cancels the existing composition and discards the composition node
    /// contents without applying them. See comments on ImeSetComposition for
    /// usage. This function is only used when window rendering is disabled.
    /// </summary>
    procedure IMECancelComposition;
    /// <summary>
    /// Call this function when the user drags the mouse into the web view (before
    /// calling DragTargetDragOver/DragTargetLeave/DragTargetDrop). |drag_data|
    /// should not contain file contents as this type of data is not allowed to be
    /// dragged into the web view. File contents can be removed using
    /// ICefDragData.ResetFileContents (for example, if |drag_data| comes from
    /// ICefRenderHandler.StartDragging). This function is only used when
    /// window rendering is disabled.
    /// </summary>
    procedure DragTargetDragEnter(const dragData: ICefDragData; const event: PCefMouseEvent; allowedOps: TCefDragOperations);
    /// <summary>
    /// Call this function each time the mouse is moved across the web view during
    /// a drag operation (after calling DragTargetDragEnter and before calling
    /// DragTargetDragLeave/DragTargetDrop). This function is only used when
    /// window rendering is disabled.
    /// </summary>
    procedure DragTargetDragOver(const event: PCefMouseEvent; allowedOps: TCefDragOperations);
    /// <summary>
    /// Call this function when the user drags the mouse out of the web view
    /// (after calling DragTargetDragEnter). This function is only used when
    /// window rendering is disabled.
    /// </summary>
    procedure DragTargetDragLeave;
    /// <summary>
    /// Call this function when the user completes the drag operation by dropping
    /// the object onto the web view (after calling DragTargetDragEnter). The
    /// object being dropped is |drag_data|, given as an argument to the previous
    /// DragTargetDragEnter call. This function is only used when window rendering
    /// is disabled.
    /// </summary>
    procedure DragTargetDrop(const event: PCefMouseEvent);
    /// <summary>
    /// Call this function when the drag operation started by a
    /// ICefRenderHandler.StartDragging call has ended either in a drop or by
    /// being cancelled. |x| and |y| are mouse coordinates relative to the upper-
    /// left corner of the view. If the web view is both the drag source and the
    /// drag target then all DragTarget* functions should be called before
    /// DragSource* mthods. This function is only used when window rendering is
    /// disabled.
    /// </summary>
    procedure DragSourceEndedAt(x, y: Integer; op: TCefDragOperation);
    /// <summary>
    /// Call this function when the drag operation started by a
    /// ICefRenderHandler.StartDragging call has completed. This function may
    /// be called immediately without first calling DragSourceEndedAt to cancel a
    /// drag operation. If the web view is both the drag source and the drag
    /// target then all DragTarget* functions should be called before DragSource*
    /// mthods. This function is only used when window rendering is disabled.
    /// </summary>
    procedure DragSourceSystemDragEnded;
    /// <summary>
    /// Returns the current visible navigation entry for this browser. This
    /// function can only be called on the UI thread.
    /// </summary>
    function  GetVisibleNavigationEntry : ICefNavigationEntry;
    /// <summary>
    /// Set accessibility state for all frames. |accessibility_state| may be
    /// default, enabled or disabled. If |accessibility_state| is STATE_DEFAULT
    /// then accessibility will be disabled by default and the state may be
    /// further controlled with the "force-renderer-accessibility" and "disable-
    /// renderer-accessibility" command-line switches. If |accessibility_state| is
    /// STATE_ENABLED then accessibility will be enabled. If |accessibility_state|
    /// is STATE_DISABLED then accessibility will be completely disabled.
    ///
    /// For windowed browsers accessibility will be enabled in Complete mode
    /// (which corresponds to kAccessibilityModeComplete in Chromium). In this
    /// mode all platform accessibility objects will be created and managed by
    /// Chromium's internal implementation. The client needs only to detect the
    /// screen reader and call this function appropriately. For example, on macOS
    /// the client can handle the @"AXEnhancedUserStructure" accessibility
    /// attribute to detect VoiceOver state changes and on Windows the client can
    /// handle WM_GETOBJECT with OBJID_CLIENT to detect accessibility readers.
    ///
    /// For windowless browsers accessibility will be enabled in TreeOnly mode
    /// (which corresponds to kAccessibilityModeWebContentsOnly in Chromium). In
    /// this mode renderer accessibility is enabled, the full tree is computed,
    /// and events are passed to CefAccessibiltyHandler, but platform
    /// accessibility objects are not created. The client may implement platform
    /// accessibility objects using CefAccessibiltyHandler callbacks if desired.
    /// </summary>
    procedure SetAccessibilityState(accessibilityState: TCefState);
    /// <summary>
    /// Enable notifications of auto resize via
    /// ICefDisplayHandler.OnAutoResize. Notifications are disabled by
    /// default. |min_size| and |max_size| define the range of allowed sizes.
    /// </summary>
    procedure SetAutoResizeEnabled(enabled: boolean; const min_size, max_size: PCefSize);
    /// <summary>
    /// Set whether the browser's audio is muted.
    /// </summary>
    procedure SetAudioMuted(mute: boolean);
    /// <summary>
    /// Returns true (1) if the browser's audio is muted.  This function can only
    /// be called on the UI thread.
    /// </summary>
    function  IsAudioMuted : boolean;
    /// <summary>
    /// Returns true (1) if the renderer is currently in browser fullscreen. This
    /// differs from window fullscreen in that browser fullscreen is entered using
    /// the JavaScript Fullscreen API and modifies CSS attributes such as the
    /// ::backdrop pseudo-element and :fullscreen pseudo-structure. This function
    /// can only be called on the UI thread.
    /// </summary>
    function IsFullscreen : boolean;
    /// <summary>
    /// Requests the renderer to exit browser fullscreen. In most cases exiting
    /// window fullscreen should also exit browser fullscreen. With Alloy style
    /// this function should be called in response to a user action such as
    /// clicking the green traffic light button on MacOS
    /// (ICefWindowDelegate.OnWindowFullscreenTransition callback) or pressing
    /// the "ESC" key (ICefKeyboardHandler.OnPreKeyEvent callback). With
    /// Chrome style these standard exit actions are handled internally but
    /// new/additional user actions can use this function. Set |will_cause_resize|
    /// to true (1) if exiting browser fullscreen will cause a view resize.
    /// </summary>
    procedure ExitFullscreen(will_cause_resize: boolean);
    /// <summary>
    /// Returns true (1) if a Chrome command is supported and enabled. Values for
    /// |command_id| can be found in the cef_command_ids.h file. This function can
    /// only be called on the UI thread. Only used with Chrome style.
    /// </summary>
    /// <remarks>
    /// <para><see cref="uCEFConstants">See the IDC_* constants in uCEFConstants.pas for all the |command_id| values.</see></para>
    /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:chrome/app/chrome_command_ids.h">The command_id values are also available in chrome/app/chrome_command_ids.h</see></para>
    /// </remarks>
    function CanExecuteChromeCommand(command_id: integer): boolean;
    /// <summary>
    /// Execute a Chrome command. Values for |command_id| can be found in the
    /// cef_command_ids.h file. |disposition| provides information about the
    /// intended command target. Only used with Chrome style.
    /// </summary>
    /// <remarks>
    /// <para><see cref="uCEFConstants">See the IDC_* constants in uCEFConstants.pas for all the |command_id| values.</see></para>
    /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:chrome/app/chrome_command_ids.h">The command_id values are also available in chrome/app/chrome_command_ids.h</see></para>
    /// </remarks>
    procedure ExecuteChromeCommand(command_id: integer; disposition: TCefWindowOpenDisposition);
    /// <summary>
    /// Returns true (1) if the render process associated with this browser is
    /// currently unresponsive as indicated by a lack of input event processing
    /// for at least 15 seconds. To receive associated state change notifications
    /// and optionally handle an unresponsive render process implement
    /// ICefRequestHandler.OnRenderProcessUnresponsive.
    /// </summary>
    /// <remarks>
    /// <para>This function can only be called on the CEF UI thread.</para>
    /// </remarks>
    function IsRenderProcessUnresponsive : boolean;
    /// <summary>
    /// Returns the runtime style for this browser (ALLOY or CHROME). See
    /// TCefRuntimeStyle documentation for details.
    /// </summary>
    /// <remarks>
    /// <para>This function can only be called on the CEF UI thread.</para>
    /// </remarks>
    function GetRuntimeStyle : TCefRuntimeStyle;
    /// <summary>
    /// Returns the hosted browser object.
    /// </summary>
    property Browser                    : ICefBrowser              read GetBrowser;
    /// <summary>
    /// Retrieve the window handle (if any) for this browser. If this browser is
    /// wrapped in a ICefBrowserView this function should be called on the
    /// browser process UI thread and it will return the handle for the top-level
    /// native window.
    /// </summary>
    property WindowHandle               : TCefWindowHandle         read GetWindowHandle;
    /// <summary>
    /// Retrieve the window handle (if any) of the browser that opened this
    /// browser. Will return NULL for non-popup browsers or if this browser is
    /// wrapped in a ICefBrowserView. This function can be used in combination
    /// with custom handling of modal windows.
    /// </summary>
    property OpenerWindowHandle         : TCefWindowHandle         read GetOpenerWindowHandle;
    /// <summary>
    /// Get the current zoom level. The default zoom level is 0.0. This function
    /// can only be called on the UI thread.
    /// </summary>
    property ZoomLevel                  : Double                   read GetZoomLevel                 write SetZoomLevel;
    /// <summary>
    /// Get the default zoom level. This value will be 0.0 by default but can be
    /// configured with the Chrome runtime.
    /// </summary>
    /// <remarks>
    /// <para>This property can only be used on the CEF UI thread.</para>
    /// </remarks>
    property DefaultZoomLevel           : Double                   read GetDefaultZoomLevel;
    /// <summary>
    /// Returns the request context for this browser.
    /// </summary>
    property RequestContext             : ICefRequestContext       read GetRequestContext;
    /// <summary>
    /// Retrieve a snapshot of current navigation entries as values sent to the
    /// specified visitor. If |current_only| is true (1) only the current
    /// navigation entry will be sent, otherwise all navigation entries will be
    /// sent.
    /// </summary>
    property VisibleNavigationEntry     : ICefNavigationEntry      read GetVisibleNavigationEntry;
    /// <summary>
    /// Returns the runtime style for this browser (ALLOY or CHROME). See
    /// TCefRuntimeStyle documentation for details.
    /// </summary>
    /// <remarks>
    /// <para>This property can only be used on the CEF UI thread.</para>
    /// </remarks>
    property RuntimeStyle               : TCefRuntimeStyle         read GetRuntimeStyle;
  end;

  /// <summary>
  /// Interface representing a message. Can be used on any process and thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefProcessMessage">Implements TCefProcessMessage</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_process_message_capi.h">CEF source file: /include/capi/cef_process_message_capi.h (cef_process_message_t)</see></para>
  /// </remarks>
  ICefProcessMessage = interface(ICefBaseRefCounted)
    ['{E0B1001A-8777-425A-869B-29D40B8B93B1}']
    /// <summary>
    /// Returns true (1) if this object is valid. Do not call any other functions
    /// if this function returns false (0).
    /// </summary>
    function IsValid: Boolean;
    /// <summary>
    /// Returns true (1) if the values of this object are read-only. Some APIs may
    /// expose read-only objects.
    /// </summary>
    function IsReadOnly: Boolean;
    /// <summary>
    /// Returns a writable copy of this object. Returns nullptr when message
    /// contains a shared memory region.
    /// </summary>
    function Copy: ICefProcessMessage;
    /// <summary>
    /// Returns the message name.
    /// </summary>
    function GetName: ustring;
    /// <summary>
    /// Returns the list of arguments. Returns nullptr when message contains a
    /// shared memory region.
    /// </summary>
    function GetArgumentList: ICefListValue;
    /// <summary>
    /// Returns the shared memory region. Returns nullptr when message contains an
    /// argument list.
    /// </summary>
    function GetSharedMemoryRegion: ICefSharedMemoryRegion;
    /// <summary>
    /// Returns the message name.
    /// </summary>
    property Name               : ustring                 read GetName;
    /// <summary>
    /// Returns the list of arguments. Returns nullptr when message contains a
    /// shared memory region.
    /// </summary>
    property ArgumentList       : ICefListValue           read GetArgumentList;
    /// <summary>
    /// Returns the shared memory region. Returns nullptr when message contains an
    /// argument list.
    /// </summary>
    property SharedMemoryRegion : ICefSharedMemoryRegion  read GetSharedMemoryRegion;
  end;

  /// <summary>
  /// Interface used to represent a browser. When used in the browser process the
  /// functions of this interface may be called on any thread unless otherwise
  /// indicated in the comments. When used in the render process the functions of
  /// this interface may only be called on the main thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefBrowser">Implements TCefBrowser</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_browser_capi.h">CEF source file: /include/capi/cef_browser_capi.h (cef_browser_t)</see></para>
  /// </remarks>
  ICefBrowser = interface(ICefBaseRefCounted)
    ['{BA003C2E-CF15-458F-9D4A-FE3CEFCF3EEF}']
    /// <summary>
    /// True if this object is currently valid. This will return false (0) after
    /// ICefLifeSpanHandler.OnBeforeClose is called.
    /// </summary>
    function  IsValid: boolean;
    /// <summary>
    /// Returns the browser host object. This function can only be called in the
    /// browser process.
    /// </summary>
    function  GetHost: ICefBrowserHost;
    /// <summary>
    /// Returns true (1) if the browser can navigate backwards.
    /// </summary>
    function  CanGoBack: Boolean;
    /// <summary>
    /// Navigate backwards.
    /// </summary>
    procedure GoBack;
    /// <summary>
    /// Returns true (1) if the browser can navigate forwards.
    /// </summary>
    function  CanGoForward: Boolean;
    /// <summary>
    /// Navigate forwards.
    /// </summary>
    procedure GoForward;
    /// <summary>
    /// Returns true (1) if the browser is currently loading.
    /// </summary>
    function  IsLoading: Boolean;
    /// <summary>
    /// Reload the current page.
    /// </summary>
    procedure Reload;
    /// <summary>
    /// Reload the current page ignoring any cached data.
    /// </summary>
    procedure ReloadIgnoreCache;
    /// <summary>
    /// Stop loading the page.
    /// </summary>
    procedure StopLoad;
    /// <summary>
    /// Returns the globally unique identifier for this browser. This value is
    /// also used as the tabId for extension APIs.
    /// </summary>
    function  GetIdentifier: Integer;
    /// <summary>
    /// Returns true (1) if this object is pointing to the same handle as |that|
    /// object.
    /// </summary>
    function  IsSame(const that: ICefBrowser): Boolean;
    /// <summary>
    /// Returns true (1) if the browser is a popup.
    /// </summary>
    function  IsPopup: Boolean;
    /// <summary>
    /// Returns true (1) if a document has been loaded in the browser.
    /// </summary>
    function  HasDocument: Boolean;
    /// <summary>
    /// Returns the main (top-level) frame for the browser. In the browser process
    /// this will return a valid object until after
    /// ICefLifeSpanHandler.OnBeforeClose is called. In the renderer process
    /// this will return NULL if the main frame is hosted in a different renderer
    /// process (e.g. for cross-origin sub-frames). The main frame object will
    /// change during cross-origin navigation or re-navigation after renderer
    /// process termination (due to crashes, etc).
    /// </summary>
    function  GetMainFrame: ICefFrame;
    /// <summary>
    /// Returns the focused frame for the browser.
    /// </summary>
    function  GetFocusedFrame: ICefFrame;
    /// <summary>
    /// Returns the frame with the specified identifier, or NULL if not found.
    /// </summary>
    function  GetFrameByIdentifier(const identifier: ustring): ICefFrame;
    /// <summary>
    /// Returns the frame with the specified name, or NULL if not found.
    /// </summary>
    function  GetFrameByName(const name: ustring): ICefFrame;
    /// <summary>
    /// Returns the number of frames that currently exist.
    /// </summary>
    function  GetFrameCount: NativeUInt;
    /// <summary>
    /// Returns the identifiers of all existing frames.
    /// </summary>
    function  GetFrameIdentifiers(var aFrameIdentifiers : TStrings) : boolean;
    /// <summary>
    /// Returns the names of all existing frames.
    /// </summary>
    function  GetFrameNames(var aFrameNames : TStrings) : boolean;

    /// <summary>
    /// Returns the main (top-level) frame for the browser. In the browser process
    /// this will return a valid object until after
    /// ICefLifeSpanHandler.OnBeforeClose is called. In the renderer process
    /// this will return NULL if the main frame is hosted in a different renderer
    /// process (e.g. for cross-origin sub-frames). The main frame object will
    /// change during cross-origin navigation or re-navigation after renderer
    /// process termination (due to crashes, etc).
    /// </summary>
    property MainFrame    : ICefFrame       read GetMainFrame;
    /// <summary>
    /// Returns the focused frame for the browser.
    /// </summary>
    property FocusedFrame : ICefFrame       read GetFocusedFrame;
    /// <summary>
    /// Returns the number of frames that currently exist.
    /// </summary>
    property FrameCount   : NativeUInt      read GetFrameCount;
    /// <summary>
    /// Returns the browser host object. This function can only be called in the
    /// browser process.
    /// </summary>
    property Host         : ICefBrowserHost read GetHost;
    /// <summary>
    /// Returns the globally unique identifier for this browser. This value is
    /// also used as the tabId for extension APIs.
    /// </summary>
    property Identifier   : Integer         read GetIdentifier;
  end;

  /// <summary>
  /// Interface used to represent a single element in the request post data. The
  /// functions of this interface may be called on any thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefPostDataElement">Implements TCefPostDataElement</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_request_capi.h">CEF source file: /include/capi/cef_request_capi.h (cef_post_data_element_t)</see></para>
  /// </remarks>
  ICefPostDataElement = interface(ICefBaseRefCounted)
    ['{3353D1B8-0300-4ADC-8D74-4FF31C77D13C}']
    /// <summary>
    /// Returns true (1) if this object is read-only.
    /// </summary>
    function  IsReadOnly: Boolean;
    /// <summary>
    /// Remove all contents from the post data element.
    /// </summary>
    procedure SetToEmpty;
    /// <summary>
    /// The post data element will represent a file.
    /// </summary>
    procedure SetToFile(const fileName: ustring);
    /// <summary>
    /// The post data element will represent bytes.  The bytes passed in will be
    /// copied.
    /// </summary>
    procedure SetToBytes(size: NativeUInt; const bytes: Pointer);
    /// <summary>
    /// Return the type of this post data element.
    /// </summary>
    function  GetType: TCefPostDataElementType;
    /// <summary>
    /// Return the file name.
    /// </summary>
    function  GetFile: ustring;
    /// <summary>
    /// Return the number of bytes.
    /// </summary>
    function  GetBytesCount: NativeUInt;
    /// <summary>
    /// Read up to |size| bytes into |bytes| and return the number of bytes
    /// actually read.
    /// </summary>
    function  GetBytes(size: NativeUInt; bytes: Pointer): NativeUInt;
  end;

  /// <summary>
  /// Interface used to represent post data for a web request. The functions of
  /// this interface may be called on any thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefPostData">Implements TCefPostData</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_request_capi.h">CEF source file: /include/capi/cef_request_capi.h (cef_post_data_t)</see></para>
  /// </remarks>
  ICefPostData = interface(ICefBaseRefCounted)
    ['{1E677630-9339-4732-BB99-D6FE4DE4AEC0}']
    /// <summary>
    /// Returns true (1) if this object is read-only.
    /// </summary>
    function  IsReadOnly: Boolean;
    /// <summary>
    /// Returns true (1) if the underlying POST data includes elements that are
    /// not represented by this ICefPostData object (for example, multi-part
    /// file upload data). Modifying ICefPostData objects with excluded
    /// elements may result in the request failing.
    /// </summary>
    function  HasExcludedElements: Boolean;
    /// <summary>
    /// Returns the number of existing post data elements.
    /// </summary>
    function  GetElementCount: NativeUInt;
    /// <summary>
    /// Retrieve the post data elements.
    /// </summary>
    procedure GetElements(elementsCount: NativeUInt; var elements: TCefPostDataElementArray);
    /// <summary>
    /// Remove the specified post data element.  Returns true (1) if the removal
    /// succeeds.
    /// </summary>
    function  RemoveElement(const element: ICefPostDataElement): Boolean;
    /// <summary>
    /// Add the specified post data element.  Returns true (1) if the add
    /// succeeds.
    /// </summary>
    function  AddElement(const element: ICefPostDataElement): Boolean;
    /// <summary>
    /// Remove all existing post data elements.
    /// </summary>
    procedure RemoveElements;
  end;

  /// <summary>
  /// Interface used to represent a web request. The functions of this interface
  /// may be called on any thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefRequest">Implements TCefRequest</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_request_capi.h">CEF source file: /include/capi/cef_request_capi.h (cef_request_t)</see></para>
  /// </remarks>
  ICefRequest = interface(ICefBaseRefCounted)
    ['{FB4718D3-7D13-4979-9F4C-D7F6C0EC592A}']
    /// <summary>
    /// Returns true (1) if this object is read-only.
    /// </summary>
    function  IsReadOnly: Boolean;
    /// <summary>
    /// Get the fully qualified URL.
    /// </summary>
    function  GetUrl: ustring;
    /// <summary>
    /// Set the fully qualified URL.
    /// </summary>
    procedure SetUrl(const value: ustring);
    /// <summary>
    /// Get the request function type. The value will default to POST if post data
    /// is provided and GET otherwise.
    /// </summary>
    function  GetMethod: ustring;
    /// <summary>
    /// Set the request function type.
    /// </summary>
    procedure SetMethod(const value: ustring);
    /// <summary>
    /// Set the referrer URL and policy. If non-NULL the referrer URL must be
    /// fully qualified with an HTTP or HTTPS scheme component. Any username,
    /// password or ref component will be removed.
    /// </summary>
    procedure SetReferrer(const referrerUrl: ustring; policy: TCefReferrerPolicy);
    /// <summary>
    /// Get the referrer URL.
    /// </summary>
    function  GetReferrerUrl: ustring;
    /// <summary>
    /// Get the referrer policy.
    /// </summary>
    function  GetReferrerPolicy: TCefReferrerPolicy;
    /// <summary>
    /// Get the post data.
    /// </summary>
    function  GetPostData: ICefPostData;
    /// <summary>
    /// Set the post data.
    /// </summary>
    procedure SetPostData(const value: ICefPostData);
    /// <summary>
    /// Get the header values. Will not include the Referer value if any.
    /// </summary>
    procedure GetHeaderMap(const HeaderMap: ICefStringMultimap);
    /// <summary>
    /// Set the header values. If a Referer value exists in the header map it will
    /// be removed and ignored.
    /// </summary>
    procedure SetHeaderMap(const HeaderMap: ICefStringMultimap);
    /// <summary>
    /// Returns the first header value for |name| or an NULL string if not found.
    /// Will not return the Referer value if any. Use GetHeaderMap instead if
    /// |name| might have multiple values.
    /// </summary>
    function  GetHeaderByName(const name: ustring): ustring;
    /// <summary>
    /// Set the header |name| to |value|. If |overwrite| is true (1) any existing
    /// values will be replaced with the new value. If |overwrite| is false (0)
    /// any existing values will not be overwritten. The Referer value cannot be
    /// set using this function.
    /// </summary>
    procedure SetHeaderByName(const name, value: ustring; overwrite: boolean);
    /// <summary>
    /// Get the flags used in combination with ICefUrlRequest. See
    /// TCefUrlRequestFlags for supported values.
    /// </summary>
    function  GetFlags: TCefUrlRequestFlags;
    /// <summary>
    /// Set the flags used in combination with ICefUrlRequest.  See
    /// TCefUrlRequestFlags for supported values.
    /// </summary>
    procedure SetFlags(flags: TCefUrlRequestFlags);
    /// <summary>
    /// Get the URL to the first party for cookies used in combination with
    /// ICefUrlRequest.
    /// </summary>
    function  GetFirstPartyForCookies: ustring;
    /// <summary>
    /// Set the URL to the first party for cookies used in combination with
    /// ICefUrlRequest.
    /// </summary>
    procedure SetFirstPartyForCookies(const url: ustring);
    /// <summary>
    /// Set all values at one time. This method corresponds to TCefRequest.set_ and cef_request_t.set
    /// </summary>
    procedure Assign(const url, method: ustring; const postData: ICefPostData; const headerMap: ICefStringMultimap);
    /// <summary>
    /// Get the resource type for this request. Only available in the browser
    /// process.
    /// </summary>
    function  GetResourceType: TCefResourceType;
    /// <summary>
    /// Get the transition type for this request. Only available in the browser
    /// process and only applies to requests that represent a main frame or sub-
    /// frame navigation.
    /// </summary>
    function  GetTransitionType: TCefTransitionType;
    /// <summary>
    /// Returns the globally unique identifier for this request or 0 if not
    /// specified. Can be used by ICefResourceRequestHandler implementations
    /// in the browser process to track a single request across multiple
    /// callbacks.
    /// </summary>
    function  GetIdentifier: UInt64;
    /// <summary>
    /// Get the fully qualified URL.
    /// </summary>
    property Url                  : ustring               read GetUrl                    write SetUrl;
    /// <summary>
    /// Get the request function type. The value will default to POST if post data
    /// is provided and GET otherwise.
    /// </summary>
    property Method               : ustring               read GetMethod                 write SetMethod;
    /// <summary>
    /// Get the referrer URL.
    /// </summary>
    property ReferrerUrl          : ustring               read GetReferrerUrl;
    /// <summary>
    /// Get the referrer policy.
    /// </summary>
    property ReferrerPolicy       : TCefReferrerPolicy    read GetReferrerPolicy;
    /// <summary>
    /// Get the post data.
    /// </summary>
    property PostData             : ICefPostData          read GetPostData               write SetPostData;
    /// <summary>
    /// Get the flags used in combination with ICefUrlRequest. See
    /// TCefUrlRequestFlags for supported values.
    /// </summary>
    property Flags                : TCefUrlRequestFlags   read GetFlags                  write SetFlags;
    /// <summary>
    /// Get the URL to the first party for cookies used in combination with
    /// ICefUrlRequest.
    /// </summary>
    property FirstPartyForCookies : ustring               read GetFirstPartyForCookies   write SetFirstPartyForCookies;
    ///  <summary>
    /// Get the resource type for this request. Only available in the browser
    /// process.
    ///  </summary>
    property ResourceType         : TCefResourceType      read GetResourceType;
    /// <summary>
    /// Get the transition type for this request. Only available in the browser
    /// process and only applies to requests that represent a main frame or sub-
    /// frame navigation.
    /// </summary>
    property TransitionType       : TCefTransitionType    read GetTransitionType;
    /// <summary>
    /// Returns the globally unique identifier for this request or 0 if not
    /// specified. Can be used by ICefResourceRequestHandler implementations
    /// in the browser process to track a single request across multiple
    /// callbacks.
    /// </summary>
    property Identifier           : UInt64                read GetIdentifier;
  end;

  /// <summary>
  /// Implement this interface to receive string values asynchronously.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefStringVisitor">Implements TCefStringVisitor</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_string_visitor_capi.h">CEF source file: /include/capi/cef_string_visitor_capi.h (cef_string_visitor_t)</see></para>
  /// </remarks>
  ICefStringVisitor = interface(ICefBaseRefCounted)
    ['{63ED4D6C-2FC8-4537-964B-B84C008F6158}']
    /// <summary>
    /// Method that will be executed.
    /// </summary>
    procedure Visit(const str: ustring);
  end;

  /// <summary>
  /// Interface used to represent a frame in the browser window. When used in the
  /// browser process the functions of this interface may be called on any thread
  /// unless otherwise indicated in the comments. When used in the render process
  /// the functions of this interface may only be called on the main thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefFrame">Implements TCefFrame</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_frame_capi.h">CEF source file: /include/capi/cef_frame_capi.h (cef_frame_t)</see></para>
  /// </remarks>
  ICefFrame = interface(ICefBaseRefCounted)
    ['{8FD3D3A6-EA3A-4A72-8501-0276BD5C3D1D}']
    /// <summary>
    /// True if this object is currently attached to a valid frame.
    /// </summary>
    function  IsValid: Boolean;
    /// <summary>
    /// Execute undo in this frame.
    /// </summary>
    procedure Undo;
    /// <summary>
    /// Execute redo in this frame.
    /// </summary>
    procedure Redo;
    /// <summary>
    /// Execute cut in this frame.
    /// </summary>
    procedure Cut;
    /// <summary>
    /// Execute copy in this frame.
    /// </summary>
    procedure Copy;
    /// <summary>
    /// Execute paste in this frame.
    /// </summary>
    procedure Paste;
    /// <summary>
    /// Execute delete in this frame.
    /// </summary>
    procedure Del;
    /// <summary>
    /// Execute select all in this frame.
    /// </summary>
    procedure SelectAll;
    /// <summary>
    /// Save this frame's HTML source to a temporary file and open it in the
    /// default text viewing application. This function can only be called from
    /// the browser process.
    /// </summary>
    procedure ViewSource;
    /// <summary>
    /// Retrieve this frame's HTML source as a string sent to the specified
    /// visitor.
    /// </summary>
    procedure GetSource(const visitor: ICefStringVisitor);
    /// <summary>
    /// Retrieve this frame's HTML source as a string sent to the specified
    /// visitor.
    /// </summary>
    procedure GetSourceProc(const proc: TCefStringVisitorProc);
    /// <summary>
    /// Retrieve this frame's display text as a string sent to the specified
    /// visitor.
    /// </summary>
    procedure GetText(const visitor: ICefStringVisitor);
    /// <summary>
    /// Retrieve this frame's display text as a string sent to the specified
    /// visitor.
    /// </summary>
    procedure GetTextProc(const proc: TCefStringVisitorProc);
    /// <summary>
    /// Load the request represented by the |request| object.
    ///
    /// WARNING: This function will fail with "bad IPC message" reason
    /// INVALID_INITIATOR_ORIGIN (213) unless you first navigate to the request
    /// origin using some other mechanism (LoadURL, link click, etc).
    /// </summary>
    procedure LoadRequest(const request: ICefRequest);
    /// <summary>
    /// Load the specified |url|.
    /// </summary>
    procedure LoadUrl(const url: ustring);
    /// <summary>
    /// Execute a string of JavaScript code in this frame. The |script_url|
    /// parameter is the URL where the script in question can be found, if any.
    /// The renderer may request this URL to show the developer the source of the
    /// error.  The |start_line| parameter is the base line number to use for
    /// error reporting.
    /// </summary>
    procedure ExecuteJavaScript(const code, scriptUrl: ustring; startLine: Integer);
    /// <summary>
    /// Returns true (1) if this is the main (top-level) frame.
    /// </summary>
    function  IsMain: Boolean;
    /// <summary>
    /// Returns true (1) if this is the focused frame.
    /// </summary>
    function  IsFocused: Boolean;
    /// <summary>
    /// Returns the name for this frame. If the frame has an assigned name (for
    /// example, set via the iframe "name" attribute) then that value will be
    /// returned. Otherwise a unique name will be constructed based on the frame
    /// parent hierarchy. The main (top-level) frame will always have an NULL name
    /// value.
    /// </summary>
    function  GetName: ustring;
    /// <summary>
    /// Returns the globally unique identifier for this frame or empty if the
    /// underlying frame does not yet exist.
    /// </summary>
    function  GetIdentifier: ustring;
    /// <summary>
    /// Returns the parent of this frame or NULL if this is the main (top-level)
    /// frame.
    /// </summary>
    function  GetParent: ICefFrame;
    /// <summary>
    /// Returns the URL currently loaded in this frame.
    /// </summary>
    function  GetUrl: ustring;
    /// <summary>
    /// Returns the browser that this frame belongs to.
    /// </summary>
    function  GetBrowser: ICefBrowser;
    /// <summary>
    /// Get the V8 context associated with the frame. This function can only be
    /// called from the render process.
    /// </summary>
    function  GetV8Context: ICefv8Context;
    /// <summary>
    /// Visit the DOM document. This function can only be called from the render
    /// process.
    /// </summary>
    procedure VisitDom(const visitor: ICefDomVisitor);
    /// <summary>
    /// Visit the DOM document. This function can only be called from the render
    /// process.
    /// </summary>
    procedure VisitDomProc(const proc: TCefDomVisitorProc);
    /// <summary>
    /// Create a new URL request that will be treated as originating from this
    /// frame and the associated browser. Use TCustomCefUrlrequestClient.Create instead if
    /// you do not want the request to have this association, in which case it may
    /// be handled differently (see documentation on that function). A request
    /// created with this function may only originate from the browser process,
    /// and will behave as follows:
    ///   - It may be intercepted by the client via CefResourceRequestHandler or
    ///     CefSchemeHandlerFactory.
    ///   - POST data may only contain a single element of type PDE_TYPE_FILE or
    ///     PDE_TYPE_BYTES.
    ///
    /// The |request| object will be marked as read-only after calling this
    /// function.
    /// </summary>
    function  CreateUrlRequest(const request: ICefRequest; const client: ICefUrlrequestClient): ICefUrlRequest;
    /// <summary>
    /// Send a message to the specified |target_process|. Ownership of the message
    /// contents will be transferred and the |message| reference will be
    /// invalidated. Message delivery is not guaranteed in all cases (for example,
    /// if the browser is closing, navigating, or if the target process crashes).
    /// Send an ACK message back from the target process if confirmation is
    /// required.
    /// </summary>
    procedure SendProcessMessage(targetProcess: TCefProcessId; const message_: ICefProcessMessage);

    /// <summary>
    /// Returns the name for this frame. If the frame has an assigned name (for
    /// example, set via the iframe "name" attribute) then that value will be
    /// returned. Otherwise a unique name will be constructed based on the frame
    /// parent hierarchy. The main (top-level) frame will always have an NULL name
    /// value.
    /// </summary>
    property Name       : ustring     read GetName;
    /// <summary>
    /// Returns the URL currently loaded in this frame.
    /// </summary>
    property Url        : ustring     read GetUrl;
    /// <summary>
    /// Returns the browser that this frame belongs to.
    /// </summary>
    property Browser    : ICefBrowser read GetBrowser;
    /// <summary>
    /// Returns the parent of this frame or NULL if this is the main (top-level)
    /// frame.
    /// </summary>
    property Parent     : ICefFrame   read GetParent;
    /// <summary>
    /// Returns the globally unique identifier for this frame or empty if the
    /// underlying frame does not yet exist.
    /// </summary>
    property Identifier : ustring     read GetIdentifier;
  end;

  /// <summary>
  /// Implement this interface to handle events related to ICefFrame life span.
  /// The order of callbacks is:
  ///
  /// (1) During initial ICefBrowserHost creation and navigation of the main
  /// frame:
  /// - ICefFrameHandler.OnFrameCreated => The initial main frame
  /// object has been created. Any commands will be queued until the frame is attached.
  /// - ICefFrameHandler.OnMainFrameChanged => The initial main frame object
  /// has been assigned to the browser.
  /// - ICefLifeSpanHandler.OnAfterCreated => The browser is now valid and
  /// can be used.
  /// - ICefFrameHandler.OnFrameAttached => The initial main frame object is
  /// now connected to its peer in the renderer process. Commands can be routed.
  ///
  /// (2) During further ICefBrowserHost navigation/loading of the main frame
  ///     and/or sub-frames:
  /// - ICefFrameHandler.OnFrameCreated => A new main frame or sub-frame
  /// object has been created. Any commands will be queued until the frame is attached.
  /// - ICefFrameHandler.OnFrameAttached => A new main frame or sub-frame
  /// object is now connected to its peer in the renderer process. Commands can be routed.
  /// - ICefFrameHandler.OnFrameDetached => An existing main frame or sub-
  /// frame object has lost its connection to the renderer process. If multiple
  ///   objects are detached at the same time then notifications will be sent for
  ///   any sub-frame objects before the main frame object. Commands can no longer
  ///   be routed and will be discarded.
  /// - ICefFrameHandler.OnMainFrameChanged => A new main frame object has
  /// been assigned to the browser. This will only occur with cross-origin navigation
  ///   or re-navigation after renderer process termination (due to crashes, etc).
  ///
  /// (3) During final ICefBrowserHost destruction of the main frame:
  /// - ICefFrameHandler.OnFrameDetached => Any sub-frame objects have lost
  /// their connection to the renderer process. Commands can no longer be routed and
  ///   will be discarded.
  /// - ICefLifeSpanHandler.OnBeforeClose => The browser has been destroyed.
  /// - ICefFrameHandler.OnFrameDetached => The main frame object have lost
  /// its connection to the renderer process. Notifications will be sent for any
  ///   sub-frame objects before the main frame object. Commands can no longer be
  ///   routed and will be discarded.
  /// - ICefFrameHandler.OnMainFrameChanged => The final main frame object has
  ///   been removed from the browser.
  ///
  /// Cross-origin navigation and/or loading receives special handling.
  ///
  /// When the main frame navigates to a different origin the OnMainFrameChanged
  /// callback (2) will be executed with the old and new main frame objects.
  ///
  /// When a new sub-frame is loaded in, or an existing sub-frame is navigated to,
  /// a different origin from the parent frame, a temporary sub-frame object will
  /// first be created in the parent's renderer process. That temporary sub-frame
  /// will then be discarded after the real cross-origin sub-frame is created in
  /// the new/target renderer process. The client will receive cross-origin
  /// navigation callbacks (2) for the transition from the temporary sub-frame to
  /// the real sub-frame. The temporary sub-frame will not recieve or execute
  /// commands during this transitional period (any sent commands will be
  /// discarded).
  ///
  /// When a new popup browser is created in a different origin from the parent
  /// browser, a temporary main frame object for the popup will first be created
  /// in the parent's renderer process. That temporary main frame will then be
  /// discarded after the real cross-origin main frame is created in the
  /// new/target renderer process. The client will recieve creation and initial
  /// navigation callbacks (1) for the temporary main frame, followed by cross-
  /// origin navigation callbacks (2) for the transition from the temporary main
  /// frame to the real main frame. The temporary main frame may receive and
  /// execute commands during this transitional period (any sent commands may be
  /// executed, but the behavior is potentially undesirable since they execute in
  /// the parent browser's renderer process and not the new/target renderer
  /// process).
  ///
  /// Callbacks will not be executed for placeholders that may be created during
  /// pre-commit navigation for sub-frames that do not yet exist in the renderer
  /// process. Placeholders will have ICefFrame.GetIdentifier() == -4.
  ///
  /// The functions of this interface will be called on the UI thread unless
  /// otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefFrameHandler">Implements TCefFrameHandler</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_frame_handler_capi.h">CEF source file: /include/capi/cef_frame_handler_capi.h (cef_frame_handler_t)</see></para>
  /// </remarks>
  ICefFrameHandler = interface(ICefBaseRefCounted)
    ['{B437128C-F7CB-4F75-83CF-A257B98C0B6E}']
    /// <summary>
    /// Called when a new frame is created. This will be the first notification
    /// that references |frame|. Any commands that require transport to the
    /// associated renderer process (LoadRequest, SendProcessMessage, GetSource,
    /// etc.) will be queued until OnFrameAttached is called for |frame|.
    /// </summary>
    procedure OnFrameCreated(const browser: ICefBrowser; const frame: ICefFrame);
    /// <summary>
    /// Called when a frame can begin routing commands to/from the associated
    /// renderer process. |reattached| will be true (1) if the frame was re-
    /// attached after exiting the BackForwardCache. Any commands that were queued
    /// have now been dispatched.
    /// </summary>
    procedure OnFrameAttached(const browser: ICefBrowser; const frame: ICefFrame; reattached: boolean);
    /// <summary>
    /// Called when a frame loses its connection to the renderer process and will
    /// be destroyed. Any pending or future commands will be discarded and
    /// ICefFrame.IsValid() will now return false (0) for |frame|. If called
    /// after ICefLifeSpanHandler.OnBeforeClose() during browser
    /// destruction then ICefBrowser.IsValid() will return false (0) for
    /// |browser|.
    /// </summary>
    procedure OnFrameDetached(const browser: ICefBrowser; const frame: ICefFrame);
    /// <summary>
    /// Called when the main frame changes due to (a) initial browser creation,
    /// (b) final browser destruction, (c) cross-origin navigation or (d) re-
    /// navigation after renderer process termination (due to crashes, etc).
    /// |old_frame| will be NULL and |new_frame| will be non-NULL when a main
    /// frame is assigned to |browser| for the first time. |old_frame| will be
    /// non-NULL and |new_frame| will be NULL and  when a main frame is removed
    /// from |browser| for the last time. Both |old_frame| and |new_frame| will be
    /// non-NULL for cross-origin navigations or re-navigation after renderer
    /// process termination. This function will be called after on_frame_created()
    /// for |new_frame| and/or after OnFrameDetached() for |old_frame|. If
    /// called after ICefLifeSpanHandler.OnBeforeClose() during browser
    /// destruction then ICefBrowser.IsValid() will return false (0) for
    /// |browser|.
    /// </summary>
    procedure OnMainFrameChanged(const browser: ICefBrowser; const old_frame, new_frame: ICefFrame);
    /// <summary>
    /// Custom procedure to clear all references.
    /// </summary>
    procedure RemoveReferences;
  end;

  /// <summary>
  /// Interface used to read data from a stream. The functions of this interface
  /// may be called on any thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefStreamReader">Implements TCefStreamReader</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_stream_capi.h">CEF source file: /include/capi/cef_stream_capi.h (cef_stream_reader_t)</see></para>
  /// </remarks>
  ICefCustomStreamReader = interface(ICefBaseRefCounted)
    ['{BBCFF23A-6FE7-4C28-B13E-6D2ACA5C83B7}']
    /// <summary>
    /// Read raw binary data.
    /// </summary>
    function Read(ptr: Pointer; size, n: NativeUInt): NativeUInt;
    /// <summary>
    /// Seek to the specified offset position. |whence| may be any one of
    /// SEEK_CUR, SEEK_END or SEEK_SET. Return zero on success and non-zero on
    /// failure.
    /// </summary>
    function Seek(offset: Int64; whence: Integer): Integer;
    /// <summary>
    /// Return the current offset position.
    /// </summary>
    function Tell: Int64;
    /// <summary>
    /// Return non-zero if at end of file.
    /// </summary>
    function Eof: Boolean;
    /// <summary>
    /// Return true (1) if this handler performs work like accessing the file
    /// system which may block. Used as a hint for determining the thread to
    /// access the handler from.
    /// </summary>
    function MayBlock: Boolean;
  end;

  /// <summary>
  /// Interface used to read data from a stream. The functions of this interface
  /// may be called on any thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefStreamReader">Implements TCefStreamReader</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_stream_capi.h">CEF source file: /include/capi/cef_stream_capi.h (cef_stream_reader_t)</see></para>
  /// </remarks>
  ICefStreamReader = interface(ICefBaseRefCounted)
    ['{DD5361CB-E558-49C5-A4BD-D1CE84ADB277}']
    /// <summary>
    /// Read raw binary data.
    /// </summary>
    function Read(ptr: Pointer; size, n: NativeUInt): NativeUInt;
    /// <summary>
    /// Seek to the specified offset position. |whence| may be any one of
    /// SEEK_CUR, SEEK_END or SEEK_SET. Return zero on success and non-zero on
    /// failure.
    /// </summary>
    function Seek(offset: Int64; whence: Integer): Integer;
    /// <summary>
    /// Return the current offset position.
    /// </summary>
    function Tell: Int64;
    /// <summary>
    /// Return non-zero if at end of file.
    /// </summary>
    function Eof: Boolean;
    /// <summary>
    /// Return true (1) if this handler performs work like accessing the file
    /// system which may block. Used as a hint for determining the thread to
    /// access the handler from.
    /// </summary>
    function MayBlock: Boolean;
  end;

  /// <summary>
  /// Structure the client can implement to provide a custom stream reader. The
  /// functions of this structure may be called on any thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefReadHandler">Implements TCefReadHandler</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_stream_capi.h">CEF source file: /include/capi/cef_stream_capi.h (cef_read_handler_tcef_stream_reader_t)</see></para>
  /// </remarks>
  ICefReadHandler = interface(ICefBaseRefCounted)
    ['{10152506-B2F8-4765-BBBC-9CA8A85A2C87}']
    /// <summary>
    /// Read raw binary data.
    /// </summary>
    function Read(ptr: Pointer; size, n: NativeUInt): NativeUInt;
    /// <summary>
    /// Seek to the specified offset position. |whence| may be any one of
    /// SEEK_CUR, SEEK_END or SEEK_SET. Return zero on success and non-zero on
    /// failure.
    /// </summary>
    function Seek(offset: Int64; whence: Integer): Integer;
    /// <summary>
    /// Return the current offset position.
    /// </summary>
    function Tell: Int64;
    /// <summary>
    /// Return non-zero if at end of file.
    /// </summary>
    function Eof: Boolean;
    /// <summary>
    /// Return true (1) if this handler performs work like accessing the file
    /// system which may block. Used as a hint for determining the thread to
    /// access the handler from.
    /// </summary>
    function MayBlock: Boolean;
  end;

  /// <summary>
  /// Interface the client can implement to provide a custom stream writer. The
  /// functions of this interface may be called on any thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefWriteHandler">Implements TCefWriteHandler</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_stream_capi.h">CEF source file: /include/capi/cef_stream_capi.h (cef_write_handler_t)</see></para>
  /// </remarks>
  ICefWriteHandler = interface(ICefBaseRefCounted)
    ['{F2431888-4EAB-421E-9EC3-320BE695AF30}']
    /// <summary>
    /// Write raw binary data.
    /// </summary>
    function Write(const ptr: Pointer; size, n: NativeUInt): NativeUInt;
    /// <summary>
    /// Seek to the specified offset position. |whence| may be any one of
    /// SEEK_CUR, SEEK_END or SEEK_SET. Return zero on success and non-zero on
    /// failure.
    /// </summary>
    function Seek(offset: Int64; whence: Integer): Integer;
    /// <summary>
    /// Return the current offset position.
    /// </summary>
    function Tell: Int64;
    /// <summary>
    /// Flush the stream.
    /// </summary>
    function Flush: Integer;
    /// <summary>
    /// Return true (1) if this handler performs work like accessing the file
    /// system which may block. Used as a hint for determining the thread to
    /// access the handler from.
    /// </summary>
    function MayBlock: Boolean;
  end;

  /// <summary>
  /// Interface used to write data to a stream. The functions of this interface
  /// may be called on any thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefStreamWriter">Implements TCefStreamWriter</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_stream_capi.h">CEF source file: /include/capi/cef_stream_capi.h (cef_stream_writer_t)</see></para>
  /// </remarks>
  ICefStreamWriter = interface(ICefBaseRefCounted)
    ['{4AA6C477-7D8A-4D5A-A704-67F900A827E7}']
    /// <summary>
    /// Write raw binary data.
    /// </summary>
    function Write(const ptr: Pointer; size, n: NativeUInt): NativeUInt;
    /// <summary>
    /// Seek to the specified offset position. |whence| may be any one of
    /// SEEK_CUR, SEEK_END or SEEK_SET. Returns zero on success and non-zero on
    /// failure.
    /// </summary>
    function Seek(offset: Int64; whence: Integer): Integer;
    /// <summary>
    /// Return the current offset position.
    /// </summary>
    function Tell: Int64;
    /// <summary>
    /// Flush the stream.
    /// </summary>
    function Flush: Integer;
    /// <summary>
    /// Returns true (1) if this writer performs work like accessing the file
    /// system which may block. Used as a hint for determining the thread to
    /// access the writer from.
    /// </summary>
    function MayBlock: Boolean;
  end;

  /// <summary>
  /// Interface used to represent a web response. The functions of this interface
  /// may be called on any thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefResponse">Implements TCefResponse</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_response_capi.h">CEF source file: /include/capi/cef_response_capi.h (cef_response_t)</see></para>
  /// </remarks>
  ICefResponse = interface(ICefBaseRefCounted)
    ['{E9C896E4-59A8-4B96-AB5E-6EA3A498B7F1}']
    /// <summary>
    /// Returns true (1) if this object is read-only.
    /// </summary>
    function  IsReadOnly: Boolean;
    /// <summary>
    /// Get the response error code. Returns ERR_NONE if there was no error.
    /// </summary>
    function  GetError: TCefErrorCode;
    /// <summary>
    /// Set the response error code. This can be used by custom scheme handlers to
    /// return errors during initial request processing.
    /// </summary>
    procedure SetError(error: TCefErrorCode);
    /// <summary>
    /// Get the response status code.
    /// </summary>
    function  GetStatus: Integer;
    /// <summary>
    /// Set the response status code.
    /// </summary>
    procedure SetStatus(status: Integer);
    /// <summary>
    /// Get the response status text.
    /// </summary>
    function  GetStatusText: ustring;
    /// <summary>
    /// Set the response status text.
    /// </summary>
    procedure SetStatusText(const StatusText: ustring);
    /// <summary>
    /// Get the response mime type.
    /// </summary>
    function  GetMimeType: ustring;
    /// <summary>
    /// Set the response mime type.
    /// </summary>
    procedure SetMimeType(const mimetype: ustring);
    /// <summary>
    /// Get the response charset.
    /// </summary>
    function  GetCharset: ustring;
    /// <summary>
    /// Set the response charset.
    /// </summary>
    procedure SetCharset(const charset: ustring);
    /// <summary>
    /// Get the value for the specified response header field.
    /// </summary>
    function  GetHeaderByName(const name: ustring): ustring;
    /// <summary>
    /// Set the header |name| to |value|. If |overwrite| is true (1) any existing
    /// values will be replaced with the new value. If |overwrite| is false (0)
    /// any existing values will not be overwritten.
    /// </summary>
    procedure SetHeaderByName(const name, value: ustring; overwrite: boolean);
    /// <summary>
    /// Get all response header fields.
    /// </summary>
    procedure GetHeaderMap(const headerMap: ICefStringMultimap);
    /// <summary>
    /// Set all response header fields.
    /// </summary>
    procedure SetHeaderMap(const headerMap: ICefStringMultimap);
    /// <summary>
    /// Get the resolved URL after redirects or changed as a result of HSTS.
    /// </summary>
    function  GetURL: ustring;
    /// <summary>
    /// Set the resolved URL after redirects or changed as a result of HSTS.
    /// </summary>
    procedure SetURL(const url: ustring);
    /// <summary>
    /// Get the response status code.
    /// </summary>
    property Status     : Integer       read GetStatus      write SetStatus;
    /// <summary>
    /// Get the response status text.
    /// </summary>
    property StatusText : ustring       read GetStatusText  write SetStatusText;
    /// <summary>
    /// Get the response mime type.
    /// </summary>
    property MimeType   : ustring       read GetMimeType    write SetMimeType;
    /// <summary>
    /// Get the response charset.
    /// </summary>
    property Charset    : ustring       read GetCharset     write SetCharset;
    /// <summary>
    /// Get the response error code. Returns ERR_NONE if there was no error.
    /// </summary>
    property Error      : TCefErrorCode read GetError       write SetError;
    /// <summary>
    /// Get the resolved URL after redirects or changed as a result of HSTS.
    /// </summary>
    property URL        : ustring       read GetURL         write SetURL;
  end;

  /// <summary>
  /// Interface used to represent a download item.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefDownloadItem">Implements TCefDownloadItem</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_download_item_capi.h">CEF source file: /include/capi/cef_download_item_capi.h (cef_download_item_t)</see></para>
  /// </remarks>
  ICefDownloadItem = interface(ICefBaseRefCounted)
    ['{B34BD320-A82E-4185-8E84-B98E5EEC803F}']
    /// <summary>
    /// Returns true (1) if this object is valid. Do not call any other functions
    /// if this function returns false (0).
    /// </summary>
    function IsValid: Boolean;
    /// <summary>
    /// Returns true (1) if the download is in progress.
    /// </summary>
    function IsInProgress: Boolean;
    /// <summary>
    /// Returns true (1) if the download is complete.
    /// </summary>
    function IsComplete: Boolean;
    /// <summary>
    /// Returns true (1) if the download has been canceled.
    /// </summary>
    function IsCanceled: Boolean;
    /// <summary>
    /// Returns true (1) if the download has been interrupted.
    /// </summary>
    function IsInterrupted: Boolean;
    /// <summary>
    /// Returns the most recent interrupt reason.
    /// </summary>
    function GetInterruptReason: TCefDownloadInterruptReason;
    /// <summary>
    /// Returns a simple speed estimate in bytes/s.
    /// </summary>
    function GetCurrentSpeed: Int64;
    /// <summary>
    /// Returns the rough percent complete or -1 if the receive total size is
    /// unknown.
    /// </summary>
    function GetPercentComplete: Integer;
    /// <summary>
    /// Returns the total number of bytes.
    /// </summary>
    function GetTotalBytes: Int64;
    /// <summary>
    /// Returns the number of received bytes.
    /// </summary>
    function GetReceivedBytes: Int64;
    /// <summary>
    /// Returns the time that the download started.
    /// </summary>
    function GetStartTime: TDateTime;
    /// <summary>
    /// Returns the time that the download ended.
    /// </summary>
    function GetEndTime: TDateTime;
    /// <summary>
    /// Returns the full path to the downloaded or downloading file.
    /// </summary>
    function GetFullPath: ustring;
    /// <summary>
    /// Returns the unique identifier for this download.
    /// </summary>
    function GetId: Cardinal;
    /// <summary>
    /// Returns the URL.
    /// </summary>
    function GetUrl: ustring;
    /// <summary>
    /// Returns the original URL before any redirections.
    /// </summary>
    function GetOriginalUrl: ustring;
    /// <summary>
    /// Returns the suggested file name.
    /// </summary>
    function GetSuggestedFileName: ustring;
    /// <summary>
    /// Returns the content disposition.
    /// </summary>
    function GetContentDisposition: ustring;
    /// <summary>
    /// Returns the mime type.
    /// </summary>
    function GetMimeType: ustring;
    /// <summary>
    /// Returns a simple speed estimate in bytes/s.
    /// </summary>
    property CurrentSpeed        : Int64                         read GetCurrentSpeed;
    /// <summary>
    /// Returns the rough percent complete or -1 if the receive total size is
    /// unknown.
    /// </summary>
    property PercentComplete     : Integer                       read GetPercentComplete;
    /// <summary>
    /// Returns the total number of bytes.
    /// </summary>
    property TotalBytes          : Int64                         read GetTotalBytes;
    /// <summary>
    /// Returns the number of received bytes.
    /// </summary>
    property ReceivedBytes       : Int64                         read GetReceivedBytes;
    /// <summary>
    /// Returns the time that the download started.
    /// </summary>
    property StartTime           : TDateTime                     read GetStartTime;
    /// <summary>
    /// Returns the time that the download ended.
    /// </summary>
    property EndTime             : TDateTime                     read GetEndTime;
    /// <summary>
    /// Returns the full path to the downloaded or downloading file.
    /// </summary>
    property FullPath            : ustring                       read GetFullPath;
    /// <summary>
    /// Returns the unique identifier for this download.
    /// </summary>
    property Id                  : Cardinal                      read GetId;
    /// <summary>
    /// Returns the URL.
    /// </summary>
    property Url                 : ustring                       read GetUrl;
    /// <summary>
    /// Returns the original URL before any redirections.
    /// </summary>
    property OriginalUrl         : ustring                       read GetOriginalUrl;
    /// <summary>
    /// Returns the suggested file name.
    /// </summary>
    property SuggestedFileName   : ustring                       read GetSuggestedFileName;
    /// <summary>
    /// Returns the content disposition.
    /// </summary>
    property ContentDisposition  : ustring                       read GetContentDisposition;
    /// <summary>
    /// Returns the mime type.
    /// </summary>
    property MimeType            : ustring                       read GetMimeType;
    /// <summary>
    /// Returns the most recent interrupt reason.
    /// </summary>
    property InterruptReason     : TCefDownloadInterruptReason   read GetInterruptReason;
  end;

  /// <summary>
  /// Callback interface used to asynchronously continue a download.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefBeforeDownloadCallback">Implements TCefBeforeDownloadCallback</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_download_handler_capi.h">CEF source file: /include/capi/cef_download_handler_capi.h (cef_before_download_callback_t)</see></para>
  /// </remarks>
  ICefBeforeDownloadCallback = interface(ICefBaseRefCounted)
    ['{5A81AF75-CBA2-444D-AD8E-522160F36433}']
    /// <summary>
    /// Call to continue the download. Set |download_path| to the full file path
    /// for the download including the file name or leave blank to use the
    /// suggested name and the default temp directory. Set |show_dialog| to true
    /// (1) if you do wish to show the default "Save As" dialog.
    /// </summary>
    procedure Cont(const downloadPath: ustring; showDialog: Boolean);
  end;

  /// <summary>
  /// Callback interface used to asynchronously cancel a download.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefDownloadItemCallback">Implements TCefDownloadItemCallback</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_download_handler_capi.h">CEF source file: /include/capi/cef_download_handler_capi.h (cef_download_item_callback_t)</see></para>
  /// </remarks>
  ICefDownloadItemCallback = interface(ICefBaseRefCounted)
    ['{498F103F-BE64-4D5F-86B7-B37EC69E1735}']
    /// <summary>
    /// Call to cancel the download.
    /// </summary>
    procedure Cancel;
    /// <summary>
    /// Call to pause the download.
    /// </summary>
    procedure Pause;
    /// <summary>
    /// Call to resume the download.
    /// </summary>
    procedure Resume;
  end;

  /// <summary>
  /// Interface used to handle file downloads. The functions of this interface
  /// will called on the browser process UI thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefDownloadHandler">Implements TCefDownloadHandler</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_download_handler_capi.h">CEF source file: /include/capi/cef_download_handler_capi.h (cef_download_handler_t)</see></para>
  /// </remarks>
  ICefDownloadHandler = interface(ICefBaseRefCounted)
    ['{3137F90A-5DC5-43C1-858D-A269F28EF4F1}']
    /// <summary>
    /// Called before a download begins in response to a user-initiated action
    /// (e.g. alt + link click or link click that returns a `Content-Disposition:
    /// attachment` response from the server). |url| is the target download URL
    /// and |request_function| is the target function (GET, POST, etc). Return
    /// true (1) to proceed with the download or false (0) to cancel the download.
    /// </summary>
    function  CanDownload(const browser: ICefBrowser; const url, request_method: ustring): boolean;
    /// <summary>
    /// Called before a download begins. |suggested_name| is the suggested name
    /// for the download file. Return true (1) and execute |callback| either
    /// asynchronously or in this function to continue or cancel the download.
    /// Return false (0) to proceed with default handling (cancel with Alloy
    /// style, download shelf with Chrome style). Do not keep a reference to
    /// |download_item| outside of this function.
    /// </summary>
    function  OnBeforeDownload(const browser: ICefBrowser; const downloadItem: ICefDownloadItem; const suggestedName: ustring; const callback: ICefBeforeDownloadCallback): boolean;
    /// <summary>
    /// Called when a download's status or progress information has been updated.
    /// This may be called multiple times before and after OnBeforeDownload.
    /// Execute |callback| either asynchronously or in this function to cancel the
    /// download if desired. Do not keep a reference to |download_item| outside of
    /// this function.
    /// </summary>
    procedure OnDownloadUpdated(const browser: ICefBrowser; const downloadItem: ICefDownloadItem; const callback: ICefDownloadItemCallback);
    /// <summary>
    /// Custom procedure to clear all references.
    /// </summary>
    procedure RemoveReferences;
  end;

  /// <summary>
  /// Interface representing a V8 exception. The functions of this interface may
  /// be called on any render process thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefV8Exception">Implements TCefV8Exception</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_v8_capi.h">CEF source file: /include/capi/cef_v8_capi.h (cef_v8exception_t)</see></para>
  /// </remarks>
  ICefV8Exception = interface(ICefBaseRefCounted)
    ['{7E422CF0-05AC-4A60-A029-F45105DCE6A4}']
    /// <summary>
    /// Returns the exception message.
    /// </summary>
    function GetMessage: ustring;
    /// <summary>
    /// Returns the line of source code that the exception occurred within.
    /// </summary>
    function GetSourceLine: ustring;
    /// <summary>
    /// Returns the resource name for the script from where the function causing
    /// the error originates.
    /// </summary>
    function GetScriptResourceName: ustring;
    /// <summary>
    /// Returns the 1-based number of the line where the error occurred or 0 if
    /// the line number is unknown.
    /// </summary>
    function GetLineNumber: Integer;
    /// <summary>
    /// Returns the index within the script of the first character where the error
    /// occurred.
    /// </summary>
    function GetStartPosition: Integer;
    /// <summary>
    /// Returns the index within the script of the last character where the error
    /// occurred.
    /// </summary>
    function GetEndPosition: Integer;
    /// <summary>
    /// Returns the index within the line of the first character where the error
    /// occurred.
    /// </summary>
    function GetStartColumn: Integer;
    /// <summary>
    /// Returns the index within the line of the last character where the error
    /// occurred.
    /// </summary>
    function GetEndColumn: Integer;
    /// <summary>
    /// Returns the exception message.
    /// </summary>
    property Message            : ustring read GetMessage;
    /// <summary>
    /// Returns the line of source code that the exception occurred within.
    /// </summary>
    property SourceLine         : ustring read GetSourceLine;
    /// <summary>
    /// Returns the resource name for the script from where the function causing
    /// the error originates.
    /// </summary>
    property ScriptResourceName : ustring read GetScriptResourceName;
    /// <summary>
    /// Returns the 1-based number of the line where the error occurred or 0 if
    /// the line number is unknown.
    /// </summary>
    property LineNumber         : Integer read GetLineNumber;
    /// <summary>
    /// Returns the index within the script of the first character where the error
    /// occurred.
    /// </summary>
    property StartPosition      : Integer read GetStartPosition;
    /// <summary>
    /// Returns the index within the script of the last character where the error
    /// occurred.
    /// </summary>
    property EndPosition        : Integer read GetEndPosition;
    /// <summary>
    /// Returns the index within the line of the first character where the error
    /// occurred.
    /// </summary>
    property StartColumn        : Integer read GetStartColumn;
    /// <summary>
    /// Returns the index within the line of the last character where the error
    /// occurred.
    /// </summary>
    property EndColumn          : Integer read GetEndColumn;
  end;

  /// <summary>
  /// Callback interface that is passed to ICefV8value.CreateArrayBuffer.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefv8ArrayBufferReleaseCallback">Implements TCefv8ArrayBufferReleaseCallback</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_v8_capi.h">CEF source file: /include/capi/cef_v8_capi.h (cef_v8array_buffer_release_callback_t)</see></para>
  /// </remarks>
  ICefv8ArrayBufferReleaseCallback = interface(ICefBaseRefCounted)
    ['{4EAAB422-D046-43DF-B1F0-5503116A5816}']
    /// <summary>
    /// Called to release |buffer| when the ArrayBuffer JS object is garbage
    /// collected. |buffer| is the value that was passed to CreateArrayBuffer
    /// along with this object.
    /// </summary>
    procedure ReleaseBuffer(buffer : Pointer);
  end;

  /// <summary>
  /// Interface representing a V8 context handle. V8 handles can only be accessed
  /// from the thread on which they are created. Valid threads for creating a V8
  /// handle include the render process main thread (TID_RENDERER) and WebWorker
  /// threads. A task runner for posting tasks on the associated thread can be
  /// retrieved via the ICefV8context.GetTaskRunner() function.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefV8Context">Implements TCefV8Context</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_v8_capi.h">CEF source file: /include/capi/cef_v8_capi.h (cef_v8context_t)</see></para>
  /// </remarks>
  ICefv8Context = interface(ICefBaseRefCounted)
    ['{2295A11A-8773-41F2-AD42-308C215062D9}']
    /// <summary>
    /// Returns the task runner associated with this context. V8 handles can only
    /// be accessed from the thread on which they are created. This function can
    /// be called on any render process thread.
    /// </summary>
    function GetTaskRunner: ICefTaskRunner;
    /// <summary>
    /// Returns true (1) if the underlying handle is valid and it can be accessed
    /// on the current thread. Do not call any other functions if this function
    /// returns false (0).
    /// </summary>
    function IsValid: Boolean;
    /// <summary>
    /// Returns the browser for this context. This function will return an NULL
    /// reference for WebWorker contexts.
    /// </summary>
    function GetBrowser: ICefBrowser;
    /// <summary>
    /// Returns the frame for this context. This function will return an NULL
    /// reference for WebWorker contexts.
    /// </summary>
    function GetFrame: ICefFrame;
    /// <summary>
    /// Returns the global object for this context. The context must be entered
    /// before calling this function.
    /// </summary>
    function GetGlobal: ICefv8Value;
    /// <summary>
    /// Enter this context. A context must be explicitly entered before creating a
    /// V8 Object, Array, Function or Date asynchronously. exit() must be called
    /// the same number of times as enter() before releasing this context. V8
    /// objects belong to the context in which they are created. Returns true (1)
    /// if the scope was entered successfully.
    /// </summary>
    function Enter: Boolean;
    /// <summary>
    /// Exit this context. Call this function only after calling enter(). Returns
    /// true (1) if the scope was exited successfully.
    /// </summary>
    function Exit: Boolean;
    /// <summary>
    /// Returns true (1) if this object is pointing to the same handle as |that|
    /// object.
    /// </summary>
    function IsSame(const that: ICefv8Context): Boolean;
    /// <summary>
    /// Execute a string of JavaScript code in this V8 context. The |script_url|
    /// parameter is the URL where the script in question can be found, if any.
    /// The |start_line| parameter is the base line number to use for error
    /// reporting. On success |retval| will be set to the return value, if any,
    /// and the function will return true (1). On failure |exception| will be set
    /// to the exception, if any, and the function will return false (0).
    /// </summary>
    function Eval(const code: ustring; const script_url: ustring; start_line: integer; var retval: ICefv8Value; var exception: ICefV8Exception): Boolean;
    /// <summary>
    /// Returns the browser for this context. This function will return an NULL
    /// reference for WebWorker contexts.
    /// </summary>
    property Browser  : ICefBrowser read GetBrowser;
    /// <summary>
    /// Returns the frame for this context. This function will return an NULL
    /// reference for WebWorker contexts.
    /// </summary>
    property Frame    : ICefFrame   read GetFrame;
    /// <summary>
    /// Returns the global object for this context. The context must be entered
    /// before calling this function.
    /// </summary>
    property Global   : ICefv8Value read GetGlobal;
  end;

  /// <summary>
  /// Interface that should be implemented to handle V8 function calls. The
  /// functions of this interface will be called on the thread associated with the
  /// V8 function.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefv8Handler">Implements TCefv8Handler</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_v8_capi.h">CEF source file: /include/capi/cef_v8_capi.h (cef_v8handler_t)</see></para>
  /// </remarks>
  ICefv8Handler = interface(ICefBaseRefCounted)
    ['{F94CDC60-FDCB-422D-96D5-D2A775BD5D73}']
    /// <summary>
    /// Handle execution of the function identified by |name|. |object| is the
    /// receiver ('this' object) of the function. |arguments| is the list of
    /// arguments passed to the function. If execution succeeds set |retval| to
    /// the function return value. If execution fails set |exception| to the
    /// exception that will be thrown. Return true (1) if execution was handled.
    /// </summary>
    function Execute(const name: ustring; const object_: ICefv8Value; const arguments: TCefv8ValueArray; var retval: ICefv8Value; var exception: ustring): Boolean;
  end;

  /// <summary>
  /// Interface that should be implemented to handle V8 interceptor calls. The
  /// functions of this interface will be called on the thread associated with the
  /// V8 interceptor. Interceptor's named property handlers (with first argument
  /// of type CefString) are called when object is indexed by string. Indexed
  /// property handlers (with first argument of type int) are called when object
  /// is indexed by integer.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefV8Interceptor">Implements TCefV8Interceptor</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_v8_capi.h">CEF source file: /include/capi/cef_v8_capi.h (cef_v8interceptor_t)</see></para>
  /// </remarks>
  ICefV8Interceptor = interface(ICefBaseRefCounted)
    ['{B3B8FD7C-A916-4B25-93A2-2892AC324F21}']
    /// <summary>
    /// Handle retrieval of the interceptor value identified by |name|. |object|
    /// is the receiver ('this' object) of the interceptor. If retrieval succeeds,
    /// set |retval| to the return value. If the requested value does not exist,
    /// don't set either |retval| or |exception|. If retrieval fails, set
    /// |exception| to the exception that will be thrown. If the property has an
    /// associated accessor, it will be called only if you don't set |retval|.
    /// Return true (1) if interceptor retrieval was handled, false (0) otherwise.
    /// </summary>
    function GetByName(const name: ustring; const object_: ICefv8Value; var retval: ICefv8Value; var exception: ustring): boolean;
    /// <summary>
    /// Handle retrieval of the interceptor value identified by |index|. |object|
    /// is the receiver ('this' object) of the interceptor. If retrieval succeeds,
    /// set |retval| to the return value. If the requested value does not exist,
    /// don't set either |retval| or |exception|. If retrieval fails, set
    /// |exception| to the exception that will be thrown. Return true (1) if
    /// interceptor retrieval was handled, false (0) otherwise.
    /// </summary>
    function GetByIndex(index: integer; const object_: ICefv8Value; var retval: ICefv8Value; var exception: ustring): boolean;
    /// <summary>
    /// Handle assignment of the interceptor value identified by |name|. |object|
    /// is the receiver ('this' object) of the interceptor. |value| is the new
    /// value being assigned to the interceptor. If assignment fails, set
    /// |exception| to the exception that will be thrown. This setter will always
    /// be called, even when the property has an associated accessor. Return true
    /// (1) if interceptor assignment was handled, false (0) otherwise.
    /// </summary>
    function SetByName(const name: ustring; const object_, value: ICefv8Value; var exception: ustring): boolean;
    /// <summary>
    /// Handle assignment of the interceptor value identified by |index|. |object|
    /// is the receiver ('this' object) of the interceptor. |value| is the new
    /// value being assigned to the interceptor. If assignment fails, set
    /// |exception| to the exception that will be thrown. Return true (1) if
    /// interceptor assignment was handled, false (0) otherwise.
    /// </summary>
    function SetByIndex(index: integer; const object_, value: ICefv8Value; var exception: ustring): boolean;
  end;

  /// <summary>
  /// Interface that should be implemented to handle V8 accessor calls. Accessor
  /// identifiers are registered by calling ICefV8value.SetValue(). The
  /// functions of this interface will be called on the thread associated with the
  /// V8 accessor.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefV8Accessor">Implements TCefV8Accessor</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_v8_capi.h">CEF source file: /include/capi/cef_v8_capi.h (cef_v8accessor_t)</see></para>
  /// </remarks>
  ICefV8Accessor = interface(ICefBaseRefCounted)
    ['{DCA6D4A2-726A-4E24-AA64-5E8C731D868A}']
    /// <summary>
    /// Handle retrieval the accessor value identified by |name|. |object| is the
    /// receiver ('this' object) of the accessor. If retrieval succeeds set
    /// |retval| to the return value. If retrieval fails set |exception| to the
    /// exception that will be thrown. Return true (1) if accessor retrieval was
    /// handled.
    /// </summary>
    function Get(const name: ustring; const object_: ICefv8Value; var retval: ICefv8Value; var exception: ustring): Boolean;
    /// <summary>
    /// Handle assignment of the accessor value identified by |name|. |object| is
    /// the receiver ('this' object) of the accessor. |value| is the new value
    /// being assigned to the accessor. If assignment fails set |exception| to the
    /// exception that will be thrown. Return true (1) if accessor assignment was
    /// handled.
    /// </summary>
    function Set_(const name: ustring; const object_, value: ICefv8Value; var exception: ustring): Boolean;
  end;

  /// <summary>
  /// Implement this interface for asynchronous task execution. If the task is
  /// posted successfully and if the associated message loop is still running then
  /// the execute() function will be called on the target thread. If the task
  /// fails to post then the task object may be destroyed on the source thread
  /// instead of the target thread. For this reason be cautious when performing
  /// work in the task object destructor.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefTask">Implements TCefTask</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_task_capi.h">CEF source file: /include/capi/cef_task_capi.h (cef_task_t)</see></para>
  /// </remarks>
  ICefTask = interface(ICefBaseRefCounted)
    ['{0D965470-4A86-47CE-BD39-A8770021AD7E}']
    /// <summary>
    /// Method that will be executed on the target thread.
    /// </summary>
    procedure Execute;
  end;

  /// <summary>
  /// Interface that facilitates managing the browser-related tasks. The functions
  /// of this structure may only be called on the UI thread.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_task_manager_capi.h">CEF source file: /include/capi/cef_task_manager_capi.h (cef_task_manager_t)</see></para>
  /// </remarks>
  ICefTaskManager = interface(ICefBaseRefCounted)
    ['{913B6FE7-1543-407D-9A23-8C232103965B}']
    /// <summary>
    /// Returns the number of tasks currently tracked by the task manager. Returns
    /// 0 if the function was called from the incorrect thread.
    /// </summary>
    function GetTasksCount : NativeUInt;
    /// <summary>
    /// Gets the list of task IDs currently tracked by the task manager. Tasks
    /// that share the same process id will always be consecutive. The list will
    /// be sorted in a way that reflects the process tree: the browser process
    /// will be first, followed by the gpu process if it exists. Related processes
    /// (e.g., a subframe process and its parent) will be kept together if
    /// possible. Callers can expect this ordering to be stable when a process is
    /// added or removed. The task IDs are unique within the application lifespan.
    /// Returns false (0) if the function was called from the incorrect thread.
    /// </summary>
    function GetTaskIdsList(var task_ids: TCefCustomInt64Array): boolean;
    /// <summary>
    /// Gets information about the task with |task_id|. Returns true (1) if the
    /// information about the task was successfully retrieved and false (0) if the
    /// |task_id| is invalid or the function was called from the incorrect thread.
    /// </summary>
    function GetTaskInfo(const task_id: int64; var info: TCustomTaskInfo): boolean;
    /// <summary>
    /// Attempts to terminate a task with |task_id|. Returns false (0) if the
    /// |task_id| is invalid, the call is made from an incorrect thread, or if the
    /// task cannot be terminated.
    /// </summary>
    function KillTask(task_id: int64): boolean;
    /// <summary>
    /// Returns the task ID associated with the main task for |browser_id| (value
    /// from cef_browser_t::GetIdentifier). Returns -1 if |browser_id| is invalid,
    /// does not currently have an associated task, or the function was called
    /// from the incorrect thread.
    /// </summary>
    function GetTaskIdForBrowserId(browser_id: Integer): int64;
  end;

  /// <summary>
  /// Interface that asynchronously executes tasks on the associated thread. It is
  /// safe to call the functions of this interface on any thread.
  ///
  /// CEF maintains multiple internal threads that are used for handling different
  /// types of tasks in different processes. The TCefThreadId definitions in
  /// cef_types.h list the common CEF threads. Task runners are also available for
  /// other CEF threads as appropriate (for example, V8 WebWorker threads).
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefTaskRunner">Implements TCefTaskRunner</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_task_capi.h">CEF source file: /include/capi/cef_task_capi.h (cef_task_runner_t)</see></para>
  /// </remarks>
  ICefTaskRunner = interface(ICefBaseRefCounted)
    ['{6A500FA3-77B7-4418-8EA8-6337EED1337B}']
    /// <summary>
    /// Returns true (1) if this object is pointing to the same task runner as
    /// |that| object.
    /// </summary>
    function IsSame(const that: ICefTaskRunner): Boolean;
    /// <summary>
    /// Returns true (1) if this task runner belongs to the current thread.
    /// </summary>
    function BelongsToCurrentThread: Boolean;
    /// <summary>
    /// Returns true (1) if this task runner is for the specified CEF thread.
    /// </summary>
    function BelongsToThread(threadId: TCefThreadId): Boolean;
    /// <summary>
    /// Post a task for execution on the thread associated with this task runner.
    /// Execution will occur asynchronously.
    /// </summary>
    function PostTask(const task: ICefTask): Boolean;
    /// <summary>
    /// Post a task for delayed execution on the thread associated with this task
    /// runner. Execution will occur asynchronously. Delayed tasks are not
    /// supported on V8 WebWorker threads and will be executed without the
    /// specified delay.
    /// </summary>
    function PostDelayedTask(const task: ICefTask; delayMs: Int64): Boolean;
  end;

  /// <summary>
  /// A simple thread abstraction that establishes a message loop on a new thread.
  /// The consumer uses ICefTaskRunner to execute code on the thread's message
  /// loop. The thread is terminated when the ICefThread object is destroyed or
  /// stop() is called. All pending tasks queued on the thread's message loop will
  /// run to completion before the thread is terminated. cef_thread_create() can
  /// be called on any valid CEF thread in either the browser or render process.
  /// This interface should only be used for tasks that require a dedicated
  /// thread. In most cases you can post tasks to an existing CEF thread instead
  /// of creating a new one; see cef_task.h for details.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefThread">Implements TCefThread</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_thread_capi.h">CEF source file: /include/capi/cef_thread_capi.h (cef_thread_t)</see></para>
  /// </remarks>
  ICefThread = interface(ICefBaseRefCounted)
    ['{26B30EA5-F44A-4C40-97DF-67FD9E73A4FF}']
    /// <summary>
    /// Returns the ICefTaskRunner that will execute code on this thread's
    /// message loop. This function is safe to call from any thread.
    /// </summary>
    function  GetTaskRunner : ICefTaskRunner;
    /// <summary>
    /// Returns the platform thread ID. It will return the same value after stop()
    /// is called. This function is safe to call from any thread.
    /// </summary>
    function  GetPlatformThreadID : TCefPlatformThreadId;
    /// <summary>
    /// Stop and join the thread. This function must be called from the same
    /// thread that called cef_thread_create(). Do not call this function if
    /// cef_thread_create() was called with a |stoppable| value of false (0).
    /// </summary>
    procedure Stop;
    /// <summary>
    /// Returns true (1) if the thread is currently running. This function must be
    /// called from the same thread that called cef_thread_create().
    /// </summary>
    function  IsRunning : boolean;
  end;

  /// <summary>
  /// WaitableEvent is a thread synchronization tool that allows one thread to
  /// wait for another thread to finish some work. This is equivalent to using a
  /// Lock+ConditionVariable to protect a simple boolean value. However, using
  /// WaitableEvent in conjunction with a Lock to wait for a more complex state
  /// change (e.g., for an item to be added to a queue) is not recommended. In
  /// that case consider using a ConditionVariable instead of a WaitableEvent. It
  /// is safe to create and/or signal a WaitableEvent from any thread. Blocking on
  /// a WaitableEvent by calling the *wait() functions is not allowed on the
  /// browser process UI or IO threads.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefWaitableEvent">Implements TCefWaitableEvent</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_waitable_event_capi.h">CEF source file: /include/capi/cef_waitable_event_capi.h (cef_waitable_event_t)</see></para>
  /// </remarks>
  ICefWaitableEvent = interface(ICefBaseRefCounted)
    ['{965C90C9-3DAE-457F-AA64-E04FF508094A}']
    /// <summary>
    /// Put the event in the un-signaled state.
    /// </summary>
    procedure Reset;
    /// <summary>
    /// Put the event in the signaled state. This causes any thread blocked on
    /// Wait to be woken up.
    /// </summary>
    procedure Signal;
    /// <summary>
    /// Returns true (1) if the event is in the signaled state, else false (0). If
    /// the event was created with |automatic_reset| set to true (1) then calling
    /// this function will also cause a reset.
    /// </summary>
    function  IsSignaled : boolean;
    /// <summary>
    /// Wait indefinitely for the event to be signaled. This function will not
    /// return until after the call to signal() has completed. This function
    /// cannot be called on the browser process UI or IO threads.
    /// </summary>
    procedure Wait;
    /// <summary>
    /// Wait up to |max_ms| milliseconds for the event to be signaled. Returns
    /// true (1) if the event was signaled. A return value of false (0) does not
    /// necessarily mean that |max_ms| was exceeded. This function will not return
    /// until after the call to signal() has completed. This function cannot be
    /// called on the browser process UI or IO threads.
    /// </summary>
    function  TimedWait(max_ms: int64): boolean;
  end;

  /// <summary>
  /// Interface representing a V8 value handle. V8 handles can only be accessed
  /// from the thread on which they are created. Valid threads for creating a V8
  /// handle include the render process main thread (TID_RENDERER) and WebWorker
  /// threads. A task runner for posting tasks on the associated thread can be
  /// retrieved via the ICefv8context.GetTaskRunner() function.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefv8Value">Implements TCefv8Value</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_v8_capi.h">CEF source file: /include/capi/cef_v8_capi.h (cef_v8value_t)</see></para>
  /// </remarks>
  ICefv8Value = interface(ICefBaseRefCounted)
    ['{52319B8D-75A8-422C-BD4B-16FA08CC7F42}']
    /// <summary>
    /// Returns true (1) if the underlying handle is valid and it can be accessed
    /// on the current thread. Do not call any other functions if this function
    /// returns false (0).
    /// </summary>
    function IsValid: Boolean;
    /// <summary>
    /// True if the value type is undefined.
    /// </summary>
    function IsUndefined: Boolean;
    /// <summary>
    /// True if the value type is null.
    /// </summary>
    function IsNull: Boolean;
    /// <summary>
    /// True if the value type is bool.
    /// </summary>
    function IsBool: Boolean;
    /// <summary>
    /// True if the value type is int.
    /// </summary>
    function IsInt: Boolean;
    /// <summary>
    /// True if the value type is unsigned int.
    /// </summary>
    function IsUInt: Boolean;
    /// <summary>
    /// True if the value type is double.
    /// </summary>
    function IsDouble: Boolean;
    /// <summary>
    /// True if the value type is Date.
    /// </summary>
    function IsDate: Boolean;
    /// <summary>
    /// True if the value type is string.
    /// </summary>
    function IsString: Boolean;
    /// <summary>
    /// True if the value type is object.
    /// </summary>
    function IsObject: Boolean;
    /// <summary>
    /// True if the value type is array.
    /// </summary>
    function IsArray: Boolean;
    /// <summary>
    /// True if the value type is an ArrayBuffer.
    /// </summary>
    function IsArrayBuffer: Boolean;
    /// <summary>
    /// True if the value type is function.
    /// </summary>
    function IsFunction: Boolean;
    /// <summary>
    /// True if the value type is a Promise.
    /// </summary>
    function IsPromise: Boolean;
    /// <summary>
    /// Returns true (1) if this object is pointing to the same handle as |that|
    /// object.
    /// </summary>
    function IsSame(const that: ICefv8Value): Boolean;
    /// <summary>
    /// Return a bool value.
    /// </summary>
    function GetBoolValue: Boolean;
    /// <summary>
    /// Return an int value.
    /// </summary>
    function GetIntValue: Integer;
    /// <summary>
    /// Return an unsigned int value.
    /// </summary>
    function GetUIntValue: Cardinal;
    /// <summary>
    /// Return a double value.
    /// </summary>
    function GetDoubleValue: Double;
    /// <summary>
    /// Return a Date value.
    /// </summary>
    function GetDateValue: TDateTime;
    /// <summary>
    /// Return a string value.
    /// </summary>
    function GetStringValue: ustring;
    /// <summary>
    /// Returns true (1) if this is a user created object.
    /// </summary>
    function IsUserCreated: Boolean;
    /// <summary>
    /// Returns true (1) if the last function call resulted in an exception. This
    /// attribute exists only in the scope of the current CEF value object.
    /// </summary>
    function HasException: Boolean;
    /// <summary>
    /// Returns the exception resulting from the last function call. This
    /// attribute exists only in the scope of the current CEF value object.
    /// </summary>
    function GetException: ICefV8Exception;
    /// <summary>
    /// Clears the last exception and returns true (1) on success.
    /// </summary>
    function ClearException: Boolean;
    /// <summary>
    /// Returns true (1) if this object will re-throw future exceptions. This
    /// attribute exists only in the scope of the current CEF value object.
    /// </summary>
    function WillRethrowExceptions: Boolean;
    /// <summary>
    /// Set whether this object will re-throw future exceptions. By default
    /// exceptions are not re-thrown. If a exception is re-thrown the current
    /// context should not be accessed again until after the exception has been
    /// caught and not re-thrown. Returns true (1) on success. This attribute
    /// exists only in the scope of the current CEF value object.
    /// </summary>
    function SetRethrowExceptions(rethrow: Boolean): Boolean;
    /// <summary>
    /// Returns true (1) if the object has a value with the specified identifier.
    /// </summary>
    function HasValueByKey(const key: ustring): Boolean;
    /// <summary>
    /// Returns true (1) if the object has a value with the specified identifier.
    /// </summary>
    function HasValueByIndex(index: Integer): Boolean;
    /// <summary>
    /// Deletes the value with the specified identifier and returns true (1) on
    /// success. Returns false (0) if this function is called incorrectly or an
    /// exception is thrown. For read-only and don't-delete values this function
    /// will return true (1) even though deletion failed.
    /// </summary>
    function DeleteValueByKey(const key: ustring): Boolean;
    /// <summary>
    /// Deletes the value with the specified identifier and returns true (1) on
    /// success. Returns false (0) if this function is called incorrectly,
    /// deletion fails or an exception is thrown. For read-only and don't-delete
    /// values this function will return true (1) even though deletion failed.
    /// </summary>
    function DeleteValueByIndex(index: Integer): Boolean;
    /// <summary>
    /// Returns the value with the specified identifier on success. Returns NULL
    /// if this function is called incorrectly or an exception is thrown.
    /// </summary>
    function GetValueByKey(const key: ustring): ICefv8Value;
    /// <summary>
    /// Returns the value with the specified identifier on success. Returns NULL
    /// if this function is called incorrectly or an exception is thrown.
    /// </summary>
    function GetValueByIndex(index: Integer): ICefv8Value;
    /// <summary>
    /// Associates a value with the specified identifier and returns true (1) on
    /// success. Returns false (0) if this function is called incorrectly or an
    /// exception is thrown. For read-only values this function will return true
    /// (1) even though assignment failed.
    /// </summary>
    function SetValueByKey(const key: ustring; const value: ICefv8Value; attribute: TCefV8PropertyAttributes): Boolean;
    /// <summary>
    /// Associates a value with the specified identifier and returns true (1) on
    /// success. Returns false (0) if this function is called incorrectly or an
    /// exception is thrown. For read-only values this function will return true
    /// (1) even though assignment failed.
    /// </summary>
    function SetValueByIndex(index: Integer; const value: ICefv8Value): Boolean;
    /// <summary>
    /// Registers an identifier and returns true (1) on success. Access to the
    /// identifier will be forwarded to the ICefV8Accessor instance passed to
    /// cef_v8value_create_object(). Returns false (0) if this
    /// function is called incorrectly or an exception is thrown. For read-only
    /// values this function will return true (1) even though assignment failed.
    /// </summary>
    function SetValueByAccessor(const key: ustring; attribute: TCefV8PropertyAttributes): Boolean;
    /// <summary>
    /// Read the keys for the object's values into the specified vector. Integer-
    /// based keys will also be returned as strings.
    /// </summary>
    function GetKeys(const keys: TStrings): Integer;
    /// <summary>
    /// Sets the user data for this object and returns true (1) on success.
    /// Returns false (0) if this function is called incorrectly. This function
    /// can only be called on user created objects.
    /// </summary>
    function SetUserData(const data: ICefv8Value): Boolean;
    /// <summary>
    /// Returns the user data, if any, assigned to this object.
    /// </summary>
    function GetUserData: ICefv8Value;
    /// <summary>
    /// Returns the amount of externally allocated memory registered for the
    /// object.
    /// </summary>
    function GetExternallyAllocatedMemory: Integer;
    /// <summary>
    /// Adjusts the amount of registered external memory for the object. Used to
    /// give V8 an indication of the amount of externally allocated memory that is
    /// kept alive by JavaScript objects. V8 uses this information to decide when
    /// to perform global garbage collection. Each ICefv8Value tracks the amount
    /// of external memory associated with it and automatically decreases the
    /// global total by the appropriate amount on its destruction.
    /// |change_in_bytes| specifies the number of bytes to adjust by. This
    /// function returns the number of bytes associated with the object after the
    /// adjustment. This function can only be called on user created objects.
    /// </summary>
    function AdjustExternallyAllocatedMemory(changeInBytes: Integer): Integer;
    /// <summary>
    /// Returns the number of elements in the array.
    /// </summary>
    function GetArrayLength: Integer;
    /// <summary>
    /// Returns the ReleaseCallback object associated with the ArrayBuffer or NULL
    /// if the ArrayBuffer was not created with CreateArrayBuffer.
    /// </summary>
    function GetArrayBufferReleaseCallback : ICefv8ArrayBufferReleaseCallback;
    /// <summary>
    /// Prevent the ArrayBuffer from using it's memory block by setting the length
    /// to zero. This operation cannot be undone. If the ArrayBuffer was created
    /// with CreateArrayBuffer then
    /// ICefv8ArrayBufferReleaseCallback.ReleaseBuffer will be called to
    /// release the underlying buffer.
    /// </summary>
    function NeuterArrayBuffer : boolean;
    /// <summary>
    /// Returns the length (in bytes) of the ArrayBuffer.
    /// </summary>
    function GetArrayBufferByteLength: NativeUInt;
    /// <summary>
    /// Returns a pointer to the beginning of the memory block for this
    /// ArrayBuffer backing store. The returned pointer is valid as long as the
    /// ICefv8value is alive.
    /// </summary>
    function GetArrayBufferData: Pointer;
    /// <summary>
    /// Returns the function name.
    /// </summary>
    function GetFunctionName: ustring;
    /// <summary>
    /// Returns the function handler or NULL if not a CEF-created function.
    /// </summary>
    function GetFunctionHandler: ICefv8Handler;
    /// <summary>
    /// Execute the function using the current V8 context. This function should
    /// only be called from within the scope of a ICefv8Handler or
    /// ICefV8Accessor callback, or in combination with calling enter() and
    /// exit() on a stored ICefv8Context reference. |object| is the receiver
    /// ('this' object) of the function. If |object| is NULL the current context's
    /// global object will be used. |arguments| is the list of arguments that will
    /// be passed to the function. Returns the function return value on success.
    /// Returns NULL if this function is called incorrectly or an exception is
    /// thrown.
    /// </summary>
    function ExecuteFunction(const obj: ICefv8Value; const arguments: TCefv8ValueArray): ICefv8Value;
    /// <summary>
    /// Execute the function using the specified V8 context. |object| is the
    /// receiver ('this' object) of the function. If |object| is NULL the
    /// specified context's global object will be used. |arguments| is the list of
    /// arguments that will be passed to the function. Returns the function return
    /// value on success. Returns NULL if this function is called incorrectly or
    /// an exception is thrown.
    /// </summary>
    function ExecuteFunctionWithContext(const context: ICefv8Context; const obj: ICefv8Value; const arguments: TCefv8ValueArray): ICefv8Value;
    /// <summary>
    /// Resolve the Promise using the current V8 context. This function should
    /// only be called from within the scope of a ICefv8Handler or
    /// ICefV8Accessor callback, or in combination with calling enter() and
    /// exit() on a stored ICefv8Context reference. |arg| is the argument passed
    /// to the resolved promise. Returns true (1) on success. Returns false (0) if
    /// this function is called incorrectly or an exception is thrown.
    /// </summary>
    function ResolvePromise(const arg: ICefv8Value): boolean;
    /// <summary>
    /// Reject the Promise using the current V8 context. This function should only
    /// be called from within the scope of a ICefv8Handler or ICefV8Accessor
    /// callback, or in combination with calling enter() and exit() on a stored
    /// ICefv8Context reference. Returns true (1) on success. Returns false (0)
    /// if this function is called incorrectly or an exception is thrown.
    /// </summary>
    function RejectPromise(const errorMsg: ustring): boolean;
  end;

  /// <summary>
  /// Interface representing a V8 stack frame handle. V8 handles can only be
  /// accessed from the thread on which they are created. Valid threads for
  /// creating a V8 handle include the render process main thread (TID_RENDERER)
  /// and WebWorker threads. A task runner for posting tasks on the associated
  /// thread can be retrieved via the ICefv8context.GetTaskRunner() function.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefV8StackFrame">Implements TCefV8StackFrame</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_v8_capi.h">CEF source file: /include/capi/cef_v8_capi.h (cef_v8stack_frame_t)</see></para>
  /// </remarks>
  ICefV8StackFrame = interface(ICefBaseRefCounted)
    ['{BA1FFBF4-E9F2-4842-A827-DC220F324286}']
    /// <summary>
    /// Returns true (1) if the underlying handle is valid and it can be accessed
    /// on the current thread. Do not call any other functions if this function
    /// returns false (0).
    /// </summary>
    function IsValid: Boolean;
    /// <summary>
    /// Returns the name of the resource script that contains the function.
    /// </summary>
    function GetScriptName: ustring;
    /// <summary>
    /// Returns the name of the resource script that contains the function or the
    /// sourceURL value if the script name is undefined and its source ends with a
    /// "//@ sourceURL=..." string.
    /// </summary>
    function GetScriptNameOrSourceUrl: ustring;
    /// <summary>
    /// Returns the name of the function.
    /// </summary>
    function GetFunctionName: ustring;
    /// <summary>
    /// Returns the 1-based line number for the function call or 0 if unknown.
    /// </summary>
    function GetLineNumber: Integer;
    /// <summary>
    /// Returns the 1-based column offset on the line for the function call or 0
    /// if unknown.
    /// </summary>
    function GetColumn: Integer;
    /// <summary>
    /// Returns true (1) if the function was compiled using eval().
    /// </summary>
    function IsEval: Boolean;
    /// <summary>
    /// Returns true (1) if the function was called as a constructor via "new".
    /// </summary>
    function IsConstructor: Boolean;
    /// <summary>
    /// Returns the name of the resource script that contains the function.
    /// </summary>
    property ScriptName             : ustring read GetScriptName;
    /// <summary>
    /// Returns the name of the resource script that contains the function or the
    /// sourceURL value if the script name is undefined and its source ends with a
    /// "//@ sourceURL=..." string.
    /// </summary>
    property ScriptNameOrSourceUrl  : ustring read GetScriptNameOrSourceUrl;
    /// <summary>
    /// Returns the name of the function.
    /// </summary>
    property FunctionName           : ustring read GetFunctionName;
    /// <summary>
    /// Returns the 1-based line number for the function call or 0 if unknown.
    /// </summary>
    property LineNumber             : Integer read GetLineNumber;
    /// <summary>
    /// Returns the 1-based column offset on the line for the function call or 0
    /// if unknown.
    /// </summary>
    property Column                 : Integer read GetColumn;
  end;

  /// <summary>
  /// Interface representing a V8 stack trace handle. V8 handles can only be
  /// accessed from the thread on which they are created. Valid threads for
  /// creating a V8 handle include the render process main thread (TID_RENDERER)
  /// and WebWorker threads. A task runner for posting tasks on the associated
  /// thread can be retrieved via the ICefv8context.GetTaskRunner() function.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefV8StackTrace">Implements TCefV8StackTrace</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_v8_capi.h">CEF source file: /include/capi/cef_v8_capi.h (cef_v8stack_trace_t)</see></para>
  /// </remarks>
  ICefV8StackTrace = interface(ICefBaseRefCounted)
    ['{32111C84-B7F7-4E3A-92B9-7CA1D0ADB613}']
    /// <summary>
    /// Returns true (1) if the underlying handle is valid and it can be accessed
    /// on the current thread. Do not call any other functions if this function
    /// returns false (0).
    /// </summary>
    function IsValid: Boolean;
    /// <summary>
    /// Returns the number of stack frames.
    /// </summary>
    function GetFrameCount: Integer;
    /// <summary>
    /// Returns the stack frame at the specified 0-based index.
    /// </summary>
    function GetFrame(index: Integer): ICefV8StackFrame;
    /// <summary>
    /// Returns the number of stack frames.
    /// </summary>
    property FrameCount            : Integer          read GetFrameCount;
    /// <summary>
    /// Returns the stack frame at the specified 0-based index.
    /// </summary>
    property Frame[index: Integer] : ICefV8StackFrame read GetFrame;
  end;

  /// <summary>
  /// Interface that supports the reading of XML data via the libxml streaming
  /// API. The functions of this interface should only be called on the thread
  /// that creates the object.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefXmlReader">Implements TCefXmlReader</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_xml_reader_capi.h">CEF source file: /include/capi/cef_xml_reader_capi.h (cef_xml_reader_t)</see></para>
  /// </remarks>
  ICefXmlReader = interface(ICefBaseRefCounted)
    ['{0DE686C3-A8D7-45D2-82FD-92F7F4E62A90}']
    /// <summary>
    /// Moves the cursor to the next node in the document. This function must be
    /// called at least once to set the current cursor position. Returns true (1)
    /// if the cursor position was set successfully.
    /// </summary>
    function MoveToNextNode: Boolean;
    /// <summary>
    /// Close the document. This should be called directly to ensure that cleanup
    /// occurs on the correct thread.
    /// </summary>
    function Close: Boolean;
    /// <summary>
    /// Returns true (1) if an error has been reported by the XML parser.
    /// </summary>
    function HasError: Boolean;
    /// <summary>
    /// Returns the error string.
    /// </summary>
    function GetError: ustring;
    /// <summary>
    /// Returns the node type.
    /// </summary>
    function GetType: TCefXmlNodeType;
    /// <summary>
    /// Returns the node depth. Depth starts at 0 for the root node.
    /// </summary>
    function GetDepth: Integer;
    /// <summary>
    /// Returns the local name. See http://www.w3.org/TR/REC-xml-names/#NT-
    /// LocalPart for additional details.
    /// </summary>
    function GetLocalName: ustring;
    /// <summary>
    /// Returns the namespace prefix. See http://www.w3.org/TR/REC-xml-names/ for
    /// additional details.
    /// </summary>
    function GetPrefix: ustring;
    /// <summary>
    /// Returns the qualified name, equal to (Prefix:)LocalName. See
    /// http://www.w3.org/TR/REC-xml-names/#ns-qualnames for additional details.
    /// </summary>
    function GetQualifiedName: ustring;
    /// <summary>
    /// Returns the URI defining the namespace associated with the node. See
    /// http://www.w3.org/TR/REC-xml-names/ for additional details.
    /// </summary>
    function GetNamespaceUri: ustring;
    /// <summary>
    /// Returns the base URI of the node. See http://www.w3.org/TR/xmlbase/ for
    /// additional details.
    /// </summary>
    function GetBaseUri: ustring;
    /// <summary>
    /// Returns the xml:lang scope within which the node resides. See
    /// http://www.w3.org/TR/REC-xml/#sec-lang-tag for additional details.
    /// </summary>
    function GetXmlLang: ustring;
    /// <summary>
    /// Returns true (1) if the node represents an NULL element. "<a/>" is
    /// considered NULL but "<a></a>" is not.
    /// </summary>
    function IsEmptyElement: Boolean;
    /// <summary>
    /// Returns true (1) if the node has a text value.
    /// </summary>
    function HasValue: Boolean;
    /// <summary>
    /// Returns the text value.
    /// </summary>
    function GetValue: ustring;
    /// <summary>
    /// Returns true (1) if the node has attributes.
    /// </summary>
    function HasAttributes: Boolean;
    /// <summary>
    /// Returns the number of attributes.
    /// </summary>
    function GetAttributeCount: NativeUInt;
    /// <summary>
    /// Returns the value of the attribute at the specified 0-based index.
    /// </summary>
    function GetAttributeByIndex(index: Integer): ustring;
    /// <summary>
    /// Returns the value of the attribute with the specified qualified name.
    /// </summary>
    function GetAttributeByQName(const qualifiedName: ustring): ustring;
    /// <summary>
    /// Returns the value of the attribute with the specified local name and
    /// namespace URI.
    /// </summary>
    function GetAttributeByLName(const localName, namespaceURI: ustring): ustring;
    /// <summary>
    /// Returns an XML representation of the current node's children.
    /// </summary>
    function GetInnerXml: ustring;
    /// <summary>
    /// Returns an XML representation of the current node including its children.
    /// </summary>
    function GetOuterXml: ustring;
    /// <summary>
    /// Returns the line number for the current node.
    /// </summary>
    function GetLineNumber: Integer;
    /// <summary>
    /// Moves the cursor to the attribute at the specified 0-based index. Returns
    /// true (1) if the cursor position was set successfully.
    /// </summary>
    function MoveToAttributeByIndex(index: Integer): Boolean;
    /// <summary>
    /// Moves the cursor to the attribute with the specified qualified name.
    /// Returns true (1) if the cursor position was set successfully.
    /// </summary>
    function MoveToAttributeByQName(const qualifiedName: ustring): Boolean;
    /// <summary>
    /// Moves the cursor to the attribute with the specified local name and
    /// namespace URI. Returns true (1) if the cursor position was set
    /// successfully.
    /// </summary>
    function MoveToAttributeByLName(const localName, namespaceURI: ustring): Boolean;
    /// <summary>
    /// Moves the cursor to the first attribute in the current element. Returns
    /// true (1) if the cursor position was set successfully.
    /// </summary>
    function MoveToFirstAttribute: Boolean;
    /// <summary>
    /// Moves the cursor to the next attribute in the current element. Returns
    /// true (1) if the cursor position was set successfully.
    /// </summary>
    function MoveToNextAttribute: Boolean;
    /// <summary>
    /// Moves the cursor back to the carrying element. Returns true (1) if the
    /// cursor position was set successfully.
    /// </summary>
    function MoveToCarryingElement: Boolean;
  end;

  /// <summary>
  /// Interface that supports the reading of zip archives via the zlib unzip API.
  /// The functions of this interface should only be called on the thread that
  /// creates the object.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefZipReader">Implements TCefZipReader</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_zip_reader_capi.h">CEF source file: /include/capi/cef_zip_reader_capi.h (cef_zip_reader_t)</see></para>
  /// </remarks>
  ICefZipReader = interface(ICefBaseRefCounted)
    ['{3B6C591F-9877-42B3-8892-AA7B27DA34A8}']
    /// <summary>
    /// Moves the cursor to the first file in the archive. Returns true (1) if the
    /// cursor position was set successfully.
    /// </summary>
    function MoveToFirstFile: Boolean;
    /// <summary>
    /// Moves the cursor to the next file in the archive. Returns true (1) if the
    /// cursor position was set successfully.
    /// </summary>
    function MoveToNextFile: Boolean;
    /// <summary>
    /// Moves the cursor to the specified file in the archive. If |caseSensitive|
    /// is true (1) then the search will be case sensitive. Returns true (1) if
    /// the cursor position was set successfully.
    /// </summary>
    function MoveToFile(const fileName: ustring; caseSensitive: Boolean): Boolean;
    /// <summary>
    /// Closes the archive. This should be called directly to ensure that cleanup
    /// occurs on the correct thread.
    /// </summary>
    function Close: Boolean;
    /// <summary>
    /// Returns the name of the file.
    /// </summary>
    function GetFileName: ustring;
    /// <summary>
    /// Returns the uncompressed size of the file.
    /// </summary>
    function GetFileSize: Int64;
    /// <summary>
    /// Returns the last modified timestamp for the file.
    /// </summary>
    function GetFileLastModified: TCefBaseTime;
    /// <summary>
    /// Opens the file for reading of uncompressed data. A read password may
    /// optionally be specified.
    /// </summary>
    function OpenFile(const password: ustring): Boolean;
    /// <summary>
    /// Closes the file.
    /// </summary>
    function CloseFile: Boolean;
    /// <summary>
    /// Read uncompressed file contents into the specified buffer. Returns < 0 if
    /// an error occurred, 0 if at the end of file, or the number of bytes read.
    /// </summary>
    function ReadFile(buffer: Pointer; bufferSize: NativeUInt): Integer;
    /// <summary>
    /// Returns the current offset in the uncompressed file contents.
    /// </summary>
    function Tell: Int64;
    /// <summary>
    /// Returns true (1) if at end of the file contents.
    /// </summary>
    function Eof: Boolean;
  end;

  /// <summary>
  /// Interface used to represent a DOM node. The functions of this interface
  /// should only be called on the render process main thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefDomNode">Implements TCefDomNode</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_dom_capi.h">CEF source file: /include/capi/cef_dom_capi.h (cef_domnode_t)</see></para>
  /// </remarks>
  ICefDomNode = interface(ICefBaseRefCounted)
    ['{96C03C9E-9C98-491A-8DAD-1947332232D6}']
    /// <summary>
    /// Returns the type for this node.
    /// </summary>
    function  GetType: TCefDomNodeType;
    /// <summary>
    /// Returns true (1) if this is a text node.
    /// </summary>
    function  IsText: Boolean;
    /// <summary>
    /// Returns true (1) if this is an element node.
    /// </summary>
    function  IsElement: Boolean;
    /// <summary>
    /// Returns true (1) if this is an editable node.
    /// </summary>
    function  IsEditable: Boolean;
    /// <summary>
    /// Returns true (1) if this is a form control element node.
    /// </summary>
    function  IsFormControlElement: Boolean;
    /// <summary>
    /// Returns the type of this form control element node.
    /// </summary>
    function  GetFormControlElementType: TCefDomFormControlType;
    /// <summary>
    /// Returns true (1) if this object is pointing to the same handle as |that|
    /// object.
    /// </summary>
    function  IsSame(const that: ICefDomNode): Boolean;
    /// <summary>
    /// Returns the name of this node.
    /// </summary>
    function  GetName: ustring;
    /// <summary>
    /// Returns the value of this node.
    /// </summary>
    function  GetValue: ustring;
    /// <summary>
    /// Set the value of this node. Returns true (1) on success.
    /// </summary>
    function  SetValue(const value: ustring): Boolean;
    /// <summary>
    /// Returns the contents of this node as markup.
    /// </summary>
    function  GetAsMarkup: ustring;
    /// <summary>
    /// Returns the document associated with this node.
    /// </summary>
    function  GetDocument: ICefDomDocument;
    /// <summary>
    /// Returns the parent node.
    /// </summary>
    function  GetParent: ICefDomNode;
    /// <summary>
    /// Returns the previous sibling node.
    /// </summary>
    function  GetPreviousSibling: ICefDomNode;
    /// <summary>
    /// Returns the next sibling node.
    /// </summary>
    function  GetNextSibling: ICefDomNode;
    /// <summary>
    /// Returns true (1) if this node has child nodes.
    /// </summary>
    function  HasChildren: Boolean;
    /// <summary>
    /// Return the first child node.
    /// </summary>
    function  GetFirstChild: ICefDomNode;
    /// <summary>
    /// Returns the last child node.
    /// </summary>
    function  GetLastChild: ICefDomNode;
    /// <summary>
    /// Returns the tag name of this element.
    /// </summary>
    function  GetElementTagName: ustring;
    /// <summary>
    /// Returns true (1) if this element has attributes.
    /// </summary>
    function  HasElementAttributes: Boolean;
    /// <summary>
    /// Returns true (1) if this element has an attribute named |attrName|.
    /// </summary>
    function  HasElementAttribute(const attrName: ustring): Boolean;
    /// <summary>
    /// Returns the element attribute named |attrName|.
    /// </summary>
    function  GetElementAttribute(const attrName: ustring): ustring;
    /// <summary>
    /// Returns a ICefStringMap of all element attributes.
    /// </summary>
    procedure GetElementAttributes(const attrMap: ICefStringMap); overload;
    /// <summary>
    /// Returns a TStrings of all element attributes.
    /// </summary>
    procedure GetElementAttributes(var attrList: TStrings); overload;
    /// <summary>
    /// Set the value for the element attribute named |attrName|. Returns true (1)
    /// on success.
    /// </summary>
    function  SetElementAttribute(const attrName, value: ustring): Boolean;
    /// <summary>
    /// Returns the inner text of the element.
    /// </summary>
    function  GetElementInnerText: ustring;
    /// <summary>
    /// Returns the bounds of the element in device pixels. Use
    /// "window.devicePixelRatio" to convert to/from CSS pixels.
    /// </summary>
    function  GetElementBounds: TCefRect;
    /// <summary>
    /// Returns the type for this node.
    /// </summary>
    property NodeType         : TCefDomNodeType read GetType;
    /// <summary>
    /// Returns the name of this node.
    /// </summary>
    property Name             : ustring         read GetName;
    /// <summary>
    /// Returns the contents of this node as markup.
    /// </summary>
    property AsMarkup         : ustring         read GetAsMarkup;
    /// <summary>
    /// Returns the document associated with this node.
    /// </summary>
    property Document         : ICefDomDocument read GetDocument;
    /// <summary>
    /// Returns the parent node.
    /// </summary>
    property Parent           : ICefDomNode     read GetParent;
    /// <summary>
    /// Returns the previous sibling node.
    /// </summary>
    property PreviousSibling  : ICefDomNode     read GetPreviousSibling;
    /// <summary>
    /// Returns the next sibling node.
    /// </summary>
    property NextSibling      : ICefDomNode     read GetNextSibling;
    /// <summary>
    /// Return the first child node.
    /// </summary>
    property FirstChild       : ICefDomNode     read GetFirstChild;
    /// <summary>
    /// Returns the last child node.
    /// </summary>
    property LastChild        : ICefDomNode     read GetLastChild;
    /// <summary>
    /// Returns the tag name of this element.
    /// </summary>
    property ElementTagName   : ustring         read GetElementTagName;
    /// <summary>
    /// Returns the inner text of the element.
    /// </summary>
    property ElementInnerText : ustring         read GetElementInnerText;
    /// <summary>
    /// Returns the bounds of the element in device pixels. Use
    /// "window.devicePixelRatio" to convert to/from CSS pixels.
    /// </summary>
    property ElementBounds    : TCefRect        read GetElementBounds;
  end;

  /// <summary>
  /// Interface used to represent a DOM document. The functions of this interface
  /// should only be called on the render process main thread thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefDomDocument">Implements TCefDomDocument</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_dom_capi.h">CEF source file: /include/capi/cef_dom_capi.h (cef_domdocument_t)</see></para>
  /// </remarks>
  ICefDomDocument = interface(ICefBaseRefCounted)
    ['{08E74052-45AF-4F69-A578-98A5C3959426}']
    /// <summary>
    /// Returns the document type.
    /// </summary>
    function GetType: TCefDomDocumentType;
    /// <summary>
    /// Returns the root document node.
    /// </summary>
    function GetDocument: ICefDomNode;
    /// <summary>
    /// Returns the BODY node of an HTML document.
    /// </summary>
    function GetBody: ICefDomNode;
    /// <summary>
    /// Returns the HEAD node of an HTML document.
    /// </summary>
    function GetHead: ICefDomNode;
    /// <summary>
    /// Returns the title of an HTML document.
    /// </summary>
    function GetTitle: ustring;
    /// <summary>
    /// Returns the document element with the specified ID value.
    /// </summary>
    function GetElementById(const id: ustring): ICefDomNode;
    /// <summary>
    /// Returns the node that currently has keyboard focus.
    /// </summary>
    function GetFocusedNode: ICefDomNode;
    /// <summary>
    /// Returns true (1) if a portion of the document is selected.
    /// </summary>
    function HasSelection: Boolean;
    /// <summary>
    /// Returns the selection offset within the start node.
    /// </summary>
    function GetSelectionStartOffset: Integer;
    /// <summary>
    /// Returns the selection offset within the end node.
    /// </summary>
    function GetSelectionEndOffset: Integer;
    /// <summary>
    /// Returns the contents of this selection as markup.
    /// </summary>
    function GetSelectionAsMarkup: ustring;
    /// <summary>
    /// Returns the contents of this selection as text.
    /// </summary>
    function GetSelectionAsText: ustring;
    /// <summary>
    /// Returns the base URL for the document.
    /// </summary>
    function GetBaseUrl: ustring;
    /// <summary>
    /// Returns a complete URL based on the document base URL and the specified
    /// partial URL.
    /// </summary>
    function GetCompleteUrl(const partialURL: ustring): ustring;
    /// <summary>
    /// Returns the document type.
    /// </summary>
    property DocType              : TCefDomDocumentType read GetType;
    /// <summary>
    /// Returns the root document node.
    /// </summary>
    property Document             : ICefDomNode         read GetDocument;
    /// <summary>
    /// Returns the BODY node of an HTML document.
    /// </summary>
    property Body                 : ICefDomNode         read GetBody;
    /// <summary>
    /// Returns the HEAD node of an HTML document.
    /// </summary>
    property Head                 : ICefDomNode         read GetHead;
    /// <summary>
    /// Returns the title of an HTML document.
    /// </summary>
    property Title                : ustring             read GetTitle;
    /// <summary>
    /// Returns the node that currently has keyboard focus.
    /// </summary>
    property FocusedNode          : ICefDomNode         read GetFocusedNode;
    /// <summary>
    /// Returns the selection offset within the start node.
    /// </summary>
    property SelectionStartOffset : Integer             read GetSelectionStartOffset;
    /// <summary>
    /// Returns the selection offset within the end node.
    /// </summary>
    property SelectionEndOffset   : Integer             read GetSelectionEndOffset;
    /// <summary>
    /// Returns the contents of this selection as markup.
    /// </summary>
    property SelectionAsMarkup    : ustring             read GetSelectionAsMarkup;
    /// <summary>
    /// Returns the contents of this selection as text.
    /// </summary>
    property SelectionAsText      : ustring             read GetSelectionAsText;
    /// <summary>
    /// Returns the base URL for the document.
    /// </summary>
    property BaseUrl              : ustring             read GetBaseUrl;
  end;

  /// <summary>
  /// Interface to implement for visiting the DOM. The functions of this interface
  /// will be called on the render process main thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefDomVisitor">Implements TCefDomVisitor</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_dom_capi.h">CEF source file: /include/capi/cef_dom_capi.h (cef_domvisitor_t)</see></para>
  /// </remarks>
  ICefDomVisitor = interface(ICefBaseRefCounted)
    ['{30398428-3196-4531-B968-2DDBED36F6B0}']
    /// <summary>
    /// Method executed for visiting the DOM. The document object passed to this
    /// function represents a snapshot of the DOM at the time this function is
    /// executed. DOM objects are only valid for the scope of this function. Do
    /// not keep references to or attempt to access any DOM objects outside the
    /// scope of this function.
    /// </summary>
    procedure visit(const document: ICefDomDocument);
  end;

  /// <summary>
  /// Interface to implement for visiting cookie values. The functions of this
  /// interface will always be called on the UI thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefCookieVisitor">Implements TCefCookieVisitor</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_cookie_capi.h">CEF source file: /include/capi/cef_cookie_capi.h (cef_cookie_visitor_t)</see></para>
  /// </remarks>
  ICefCookieVisitor = interface(ICefBaseRefCounted)
    ['{8378CF1B-84AB-4FDB-9B86-34DDABCCC402}']
    /// <summary>
    /// Method that will be called once for each cookie. |count| is the 0-based
    /// index for the current cookie. |total| is the total number of cookies. Set
    /// |deleteCookie| to true (1) to delete the cookie currently being visited.
    /// Return false (0) to stop visiting cookies. This function may never be
    /// called if no cookies are found.
    /// </summary>
    function visit(const name, value, domain, path: ustring; secure, httponly, hasExpires: Boolean; const creation, lastAccess, expires: TDateTime; count, total: Integer; same_site : TCefCookieSameSite; priority : TCefCookiePriority; out deleteCookie: Boolean): Boolean;
  end;

  /// <summary>
  /// Interface used to create and/or parse command line arguments. Arguments with
  /// "--", "-" and, on Windows, "/" prefixes are considered switches. Switches
  /// will always precede any arguments without switch prefixes. Switches can
  /// optionally have a value specified using the "=" delimiter (e.g.
  /// "-switch=value"). An argument of "--" will terminate switch parsing with all
  /// subsequent tokens, regardless of prefix, being interpreted as non-switch
  /// arguments. Switch names should be lowercase ASCII and will be converted to
  /// such if necessary. Switch values will retain the original case and UTF8
  /// encoding. This interface can be used before cef_initialize() is called.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefCommandLine">Implements TCefCommandLine</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_command_line_capi.h">CEF source file: /include/capi/cef_command_line_capi.h (cef_command_line_t)</see></para>
  /// </remarks>
  ICefCommandLine = interface(ICefBaseRefCounted)
    ['{6B43D21B-0F2C-4B94-B4E6-4AF0D7669D8E}']
    /// <summary>
    /// Returns true (1) if this object is valid. Do not call any other functions
    /// if this function returns false (0).
    /// </summary>
    function  IsValid: Boolean;
    /// <summary>
    /// Returns true (1) if the values of this object are read-only. Some APIs may
    /// expose read-only objects.
    /// </summary>
    function  IsReadOnly: Boolean;
    /// <summary>
    /// Returns a writable copy of this object.
    /// </summary>
    function  Copy: ICefCommandLine;
    /// <summary>
    /// Initialize the command line with the specified |argc| and |argv| values.
    /// The first argument must be the name of the program. This function is only
    /// supported on non-Windows platforms.
    /// </summary>
    procedure InitFromArgv(argc: Integer; const argv: PPAnsiChar);
    /// <summary>
    /// Initialize the command line with the string returned by calling
    /// GetCommandLineW(). This function is only supported on Windows.
    /// </summary>
    procedure InitFromString(const commandLine: ustring);
    /// <summary>
    /// Reset the command-line switches and arguments but leave the program
    /// component unchanged.
    /// </summary>
    procedure Reset;
    /// <summary>
    /// Constructs and returns the represented command line string. Use this
    /// function cautiously because quoting behavior is unclear.
    /// </summary>
    function  GetCommandLineString: ustring;
    /// <summary>
    /// Retrieve the original command line string as a vector of strings. The argv
    /// array: `{ program, [(--|-|/)switch[=value]]*, [--], [argument]* }`
    /// </summary>
    procedure GetArgv(var args: TStrings);
    /// <summary>
    /// Get the program part of the command line string (the first item).
    /// </summary>
    function  GetProgram: ustring;
    /// <summary>
    /// Set the program part of the command line string (the first item).
    /// </summary>
    procedure SetProgram(const prog: ustring);
    /// <summary>
    /// Returns true (1) if the command line has switches.
    /// </summary>
    function  HasSwitches: Boolean;
    /// <summary>
    /// Returns true (1) if the command line contains the given switch.
    /// </summary>
    function  HasSwitch(const name: ustring): Boolean;
    /// <summary>
    /// Returns the value associated with the given switch. If the switch has no
    /// value or isn't present this function returns the NULL string.
    /// </summary>
    function  GetSwitchValue(const name: ustring): ustring;
    /// <summary>
    /// Returns the map of switch names and values. If a switch has no value an
    /// NULL string is returned.
    /// </summary>
    function  GetSwitches(var switches: TStrings): boolean; overload;
    /// <summary>
    /// Returns the map of switch names and values. If a switch has no value an
    /// NULL string is returned.
    /// </summary>
    function  GetSwitches(var SwitchKeys, SwitchValues: TStringList): boolean; overload;
    /// <summary>
    /// Add a switch to the end of the command line.
    /// </summary>
    procedure AppendSwitch(const name: ustring);
    /// <summary>
    /// Add a switch with the specified value to the end of the command line. If
    /// the switch has no value pass an NULL value string.
    /// </summary>
    procedure AppendSwitchWithValue(const name, value: ustring);
    /// <summary>
    /// True if there are remaining command line arguments.
    /// </summary>
    function  HasArguments: Boolean;
    /// <summary>
    /// Get the remaining command line arguments.
    /// </summary>
    procedure GetArguments(var arguments: TStrings);
    /// <summary>
    /// Add an argument to the end of the command line.
    /// </summary>
    procedure AppendArgument(const argument: ustring);
    /// <summary>
    /// Insert a command before the current command. Common for debuggers, like
    /// "valgrind" or "gdb --args".
    /// </summary>
    procedure PrependWrapper(const wrapper: ustring);
    /// <summary>
    /// Constructs and returns the represented command line string. Use this
    /// function cautiously because quoting behavior is unclear.
    /// </summary>
    property  CommandLineString  : ustring   read GetCommandLineString;
  end;

  /// <summary>
  /// Generic callback interface used for managing the lifespan of a registration.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefRegistration">Implements TCefRegistration</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_registration_capi.h">CEF source file: /include/capi/cef_registration_capi.h (cef_registration_t)</see></para>
  /// </remarks>
  ICefRegistration = interface(ICefBaseRefCounted)
    ['{9226018F-7A56-4F2E-AF01-43268E33EE6B}']
  end;

  /// <summary>
  /// Callback interface for ICefBrowserHost.AddDevToolsMessageObserver. The
  /// functions of this interface will be called on the browser process UI thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefDevToolsMessageObserver">Implements TCefDevToolsMessageObserver</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_devtools_message_observer_capi.h">CEF source file: /include/capi/cef_devtools_message_observer_capi.h (cef_dev_tools_message_observer_t)</see></para>
  /// </remarks>
  ICefDevToolsMessageObserver = interface(ICefBaseRefCounted)
    ['{76E5BB2B-7F69-4BC9-94C7-B55C61CE630F}']
    /// <summary>
    /// Method that will be called on receipt of a DevTools protocol message.
    /// |browser| is the originating browser instance. |message| is a UTF8-encoded
    /// JSON dictionary representing either a function result or an event.
    /// |message| is only valid for the scope of this callback and should be
    /// copied if necessary. Return true (1) if the message was handled or false
    /// (0) if the message should be further processed and passed to the
    /// OnDevToolsMethodResult or OnDevToolsEvent functions as appropriate.
    ///
    /// Method result dictionaries include an "id" (int) value that identifies the
    /// orginating function call sent from
    /// ICefBrowserHost.SendDevToolsMessage, and optionally either a "result"
    /// (dictionary) or "error" (dictionary) value. The "error" dictionary will
    /// contain "code" (int) and "message" (string) values. Event dictionaries
    /// include a "function" (string) value and optionally a "params" (dictionary)
    /// value. See the DevTools protocol documentation at
    /// https://chromedevtools.github.io/devtools-protocol/ for details of
    /// supported function calls and the expected "result" or "params" dictionary
    /// contents. JSON dictionaries can be parsed using the CefParseJSON function
    /// if desired, however be aware of performance considerations when parsing
    /// large messages (some of which may exceed 1MB in size).
    /// </summary>
    procedure OnDevToolsMessage(const browser: ICefBrowser; const message_: Pointer; message_size: NativeUInt; var aHandled: boolean);
    /// <summary>
    /// Method that will be called after attempted execution of a DevTools
    /// protocol function. |browser| is the originating browser instance.
    /// |message_id| is the "id" value that identifies the originating function
    /// call message. If the function succeeded |success| will be true (1) and
    /// |result| will be the UTF8-encoded JSON "result" dictionary value (which
    /// may be NULL). If the function failed |success| will be false (0) and
    /// |result| will be the UTF8-encoded JSON "error" dictionary value. |result|
    /// is only valid for the scope of this callback and should be copied if
    /// necessary. See the OnDevToolsMessage documentation for additional details
    /// on |result| contents.
    /// </summary>
    procedure OnDevToolsMethodResult(const browser: ICefBrowser; message_id: integer; success: boolean; const result: Pointer; result_size: NativeUInt);
    /// <summary>
    /// Method that will be called on receipt of a DevTools protocol event.
    /// |browser| is the originating browser instance. |function| is the
    /// "function" value. |params| is the UTF8-encoded JSON "params" dictionary
    /// value (which may be NULL). |params| is only valid for the scope of this
    /// callback and should be copied if necessary. See the OnDevToolsMessage
    /// documentation for additional details on |params| contents.
    /// </summary>
    procedure OnDevToolsEvent(const browser: ICefBrowser; const method: ustring; const params: Pointer; params_size: NativeUInt);
    /// <summary>
    /// Method that will be called when the DevTools agent has attached. |browser|
    /// is the originating browser instance. This will generally occur in response
    /// to the first message sent while the agent is detached.
    /// </summary>
    procedure OnDevToolsAgentAttached(const browser: ICefBrowser);
    /// <summary>
    /// Method that will be called when the DevTools agent has detached. |browser|
    /// is the originating browser instance. Any function results that were
    /// pending before the agent became detached will not be delivered, and any
    /// active event subscriptions will be canceled.
    /// </summary>
    procedure OnDevToolsAgentDetached(const browser: ICefBrowser);
  end;

  /// <summary>
  /// Supports discovery of and communication with media devices on the local
  /// network via the Cast and DIAL protocols. The functions of this interface may
  /// be called on any browser process thread unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefMediaRouter">Implements TCefMediaRouter</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_media_router_capi.h">CEF source file: /include/capi/cef_media_router_capi.h (cef_media_router_t)</see></para>
  /// </remarks>
  ICefMediaRouter = interface(ICefBaseRefCounted)
    ['{F18C3880-CB8D-48F9-9D74-DCFF4B9E88DF}']
    /// <summary>
    /// Add an observer for MediaRouter events. The observer will remain
    /// registered until the returned Registration object is destroyed.
    /// </summary>
    function  AddObserver(const observer: ICefMediaObserver): ICefRegistration;
    /// <summary>
    /// Returns a MediaSource object for the specified media source URN. Supported
    /// URN schemes include "cast:" and "dial:", and will be already known by the
    /// client application (e.g. "cast:<appId>?clientId=<clientId>").
    /// </summary>
    function  GetSource(const urn: ustring): ICefMediaSource;
    /// <summary>
    /// Trigger an asynchronous call to ICefMediaObserver.OnSinks on all
    /// registered observers.
    /// </summary>
    procedure NotifyCurrentSinks;
    /// <summary>
    /// Create a new route between |source| and |sink|. Source and sink must be
    /// valid, compatible (as reported by ICefMediaSink.IsCompatibleWith), and
    /// a route between them must not already exist. |callback| will be executed
    /// on success or failure. If route creation succeeds it will also trigger an
    /// asynchronous call to ICefMediaObserver.OnRoutes on all registered
    /// observers.
    /// </summary>
    procedure CreateRoute(const source: ICefMediaSource; const sink: ICefMediaSink; const callback: ICefMediaRouteCreateCallback);
    /// <summary>
    /// Trigger an asynchronous call to ICefMediaObserver.OnRoutes on all
    /// registered observers.
    /// </summary>
    procedure NotifyCurrentRoutes;
  end;

  /// <summary>
  /// Implemented by the client to observe MediaRouter events and registered via
  /// ICefMediaRouter.AddObserver. The functions of this interface will be
  /// called on the browser process UI thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefMediaObserver">Implements TCefMediaObserver</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_media_router_capi.h">CEF source file: /include/capi/cef_media_router_capi.h (cef_media_observer_t)</see></para>
  /// </remarks>
  ICefMediaObserver = interface(ICefBaseRefCounted)
    ['{0B27C8D1-63E3-4F69-939F-DCAD518654A3}']
    /// <summary>
    /// The list of available media sinks has changed or
    /// ICefMediaRouter.NotifyCurrentSinks was called.
    /// </summary>
    procedure OnSinks(const sinks: TCefMediaSinkArray);
    /// <summary>
    /// The list of available media routes has changed or
    /// ICefMediaRouter.NotifyCurrentRoutes was called.
    /// </summary>
    procedure OnRoutes(const routes: TCefMediaRouteArray);
    /// <summary>
    /// The connection state of |route| has changed.
    /// </summary>
    procedure OnRouteStateChanged(const route: ICefMediaRoute; state: TCefMediaRouteConnectionState);
    /// <summary>
    /// A message was received over |route|. |message| is only valid for the scope
    /// of this callback and should be copied if necessary.
    /// </summary>
    procedure OnRouteMessageReceived(const route: ICefMediaRoute; const message_: ustring);
  end;

  /// <summary>
  /// Represents the route between a media source and sink. Instances of this
  /// object are created via ICefMediaRouter.CreateRoute and retrieved via
  /// ICefMediaObserver.OnRoutes. Contains the status and metadata of a
  /// routing operation. The functions of this interface may be called on any
  /// browser process thread unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefMediaRoute">Implements TCefMediaRoute</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_media_router_capi.h">CEF source file: /include/capi/cef_media_router_capi.h (cef_media_route_t)</see></para>
  /// </remarks>
  ICefMediaRoute = interface(ICefBaseRefCounted)
    ['{D8959122-DD19-4933-B4D9-DF829062A0D3}']
    /// <summary>
    /// Returns the ID for this route.
    /// </summary>
    function  GetId: ustring;
    /// <summary>
    /// Returns the source associated with this route.
    /// </summary>
    function  GetSource: ICefMediaSource;
    /// <summary>
    /// Returns the sink associated with this route.
    /// </summary>
    function  GetSink: ICefMediaSink;
    /// <summary>
    /// Send a message over this route. |message_| will be copied if necessary.
    /// </summary>
    procedure SendRouteMessage(const message_: ustring);
    /// <summary>
    /// Terminate this route. Will result in an asynchronous call to
    /// ICefMediaObserver.OnRoutes on all registered observers.
    /// </summary>
    procedure Terminate;
    /// <summary>
    /// Returns the ID for this route.
    /// </summary>
    property ID     : ustring         read GetId;
    /// <summary>
    /// Returns the source associated with this route.
    /// </summary>
    property Source : ICefMediaSource read GetSource;
    /// <summary>
    /// Returns the sink associated with this route.
    /// </summary>
    property Sink   : ICefMediaSink   read GetSink;
  end;

  /// <summary>
  /// Callback interface for ICefMediaRouter.CreateRoute. The functions of
  /// this interface will be called on the browser process UI thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefMediaRouteCreateCallback">Implements TCefMediaRouteCreateCallback</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_media_router_capi.h">CEF source file: /include/capi/cef_media_router_capi.h (cef_media_route_create_callback_t)</see></para>
  /// </remarks>
  ICefMediaRouteCreateCallback = interface(ICefBaseRefCounted)
    ['{8848CBFE-36AC-4AC8-BC10-386B69FB27BE}']
    /// <summary>
    /// Method that will be executed when the route creation has finished.
    /// |result| will be CEF_MRCR_OK if the route creation succeeded. |error| will
    /// be a description of the error if the route creation failed. |route| is the
    /// resulting route, or NULL if the route creation failed.
    /// </summary>
    procedure OnMediaRouteCreateFinished(result: TCefMediaRouterCreateResult; const error: ustring; const route: ICefMediaRoute);
  end;

  /// <summary>
  /// Callback interface for ICefMediaSink.GetDeviceInfo. The functions of
  /// this interface will be called on the browser process UI thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefMediaSinkDeviceInfoCallback">Implements TCefMediaSinkDeviceInfoCallback</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_media_router_capi.h">CEF source file: /include/capi/cef_media_router_capi.h (cef_media_sink_device_info_callback_t)</see></para>
  /// </remarks>
  ICefMediaSinkDeviceInfoCallback = interface(ICefBaseRefCounted)
    ['{633898DD-4169-45D0-ADDD-6E68B3686E0D}']
    /// <summary>
    /// Method that will be executed asyncronously once device information has
    /// been retrieved.
    /// </summary>
    procedure OnMediaSinkDeviceInfo(const ip_address: ustring; port: integer; const model_name: ustring);
  end;

  /// <summary>
  /// Represents a sink to which media can be routed. Instances of this object are
  /// retrieved via ICefMediaObserver.OnSinks. The functions of this interface
  /// may be called on any browser process thread unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefMediaSink">Implements TCefMediaSink</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_media_router_capi.h">CEF source file: /include/capi/cef_media_router_capi.h (cef_media_sink_t)</see></para>
  /// </remarks>
  ICefMediaSink = interface(ICefBaseRefCounted)
    ['{EDA1A4B2-2A4C-42DD-A7DF-901BF93D908D}']
    /// <summary>
    /// Returns the ID for this sink.
    /// </summary>
    function  GetId: ustring;
    /// <summary>
    /// Returns the name of this sink.
    /// </summary>
    function  GetName: ustring;
    /// <summary>
    /// Returns the icon type for this sink.
    /// </summary>
    function  GetIconType: TCefMediaSinkIconType;
    /// <summary>
    /// Asynchronously retrieves device info.
    /// </summary>
    procedure GetDeviceInfo(const callback: ICefMediaSinkDeviceInfoCallback);
    /// <summary>
    /// Returns true (1) if this sink accepts content via Cast.
    /// </summary>
    function  IsCastSink: boolean;
    /// <summary>
    /// Returns true (1) if this sink accepts content via DIAL.
    /// </summary>
    function  IsDialSink: boolean;
    /// <summary>
    /// Returns true (1) if this sink is compatible with |source|.
    /// </summary>
    function  IsCompatibleWith(const source: ICefMediaSource): boolean;

    /// <summary>
    /// Returns the ID for this sink.
    /// </summary>
    property ID          : ustring               read GetId;
    /// <summary>
    /// Returns the name of this sink.
    /// </summary>
    property Name        : ustring               read GetName;
    /// <summary>
    /// Returns the icon type for this sink.
    /// </summary>
    property IconType    : TCefMediaSinkIconType read GetIconType;
  end;

  /// <summary>
  /// Represents a source from which media can be routed. Instances of this object
  /// are retrieved via ICefMediaRouter.GetSource. The functions of this
  /// interface may be called on any browser process thread unless otherwise
  /// indicated.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefMediaSource">Implements TCefMediaSource</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_media_router_capi.h">CEF source file: /include/capi/cef_media_router_capi.h (cef_media_source_t)</see></para>
  /// </remarks>
  ICefMediaSource = interface(ICefBaseRefCounted)
    ['{734ED6E4-6498-43ED-AAA4-6B993EDC30BE}']
    /// <summary>
    /// Returns the ID (media source URN or URL) for this source.
    /// </summary>
    function GetId : ustring;
    /// <summary>
    /// Returns true (1) if this source outputs its content via Cast.
    /// </summary>
    function IsCastSource : boolean;
    /// <summary>
    /// Returns true (1) if this source outputs its content via DIAL.
    /// </summary>
    function IsDialSource : boolean;
    /// <summary>
    /// Returns the ID (media source URN or URL) for this source.
    /// </summary>
    property ID : ustring read GetId;
  end;

  /// <summary>
  /// Interface used to implement a custom resource bundle interface. See
  /// TCefSettings for additional options related to resource bundle loading. The
  /// functions of this interface may be called on multiple threads.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefResourceBundleHandler">Implements TCefResourceBundleHandler</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_resource_bundle_handler_capi.h">CEF source file: /include/capi/cef_resource_bundle_handler_capi.h (cef_resource_bundle_handler_t)</see></para>
  /// </remarks>
  ICefResourceBundleHandler = interface(ICefBaseRefCounted)
    ['{09C264FD-7E03-41E3-87B3-4234E82B5EA2}']
    /// <summary>
    /// Called to retrieve a localized translation for the specified |string_id|.
    /// To provide the translation set |string| to the translation string and
    /// return true (1). To use the default translation return false (0). Include
    /// cef_pack_strings.h for a listing of valid string ID values.
    /// </summary>
    function GetLocalizedString(stringId: Integer; var stringVal: ustring): Boolean;
    /// <summary>
    /// Called to retrieve data for the specified scale independent |resource_id|.
    /// To provide the resource data set |data| and |data_size| to the data
    /// pointer and size respectively and return true (1). To use the default
    /// resource data return false (0). The resource data will not be copied and
    /// must remain resident in memory. Include cef_pack_resources.h for a listing
    /// of valid resource ID values.
    /// </summary>
    function GetDataResource(resourceId: Integer; var data: Pointer; var dataSize: NativeUInt): Boolean;
    /// <summary>
    /// Called to retrieve data for the specified |resource_id| nearest the scale
    /// factor |scale_factor|. To provide the resource data set |data| and
    /// |data_size| to the data pointer and size respectively and return true (1).
    /// To use the default resource data return false (0). The resource data will
    /// not be copied and must remain resident in memory. Include
    /// cef_pack_resources.h for a listing of valid resource ID values.
    /// </summary>
    function GetDataResourceForScale(resourceId: Integer; scaleFactor: TCefScaleFactor; var data: Pointer; var dataSize: NativeUInt): Boolean;
    /// <summary>
    /// Custom procedure to clear all references.
    /// </summary>
    procedure RemoveReferences;
  end;

  /// <summary>
  /// Interface used to implement browser process callbacks. The functions of this
  /// interface will be called on the browser process main thread unless otherwise
  /// indicated.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefBrowserProcessHandler">Implements TCefBrowserProcessHandler</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_browser_process_handler_capi.h">CEF source file: /include/capi/cef_browser_process_handler_capi.h (cef_browser_process_handler_t)</see></para>
  /// </remarks>
  ICefBrowserProcessHandler = interface(ICefBaseRefCounted)
    ['{27291B7A-C0AE-4EE0-9115-15C810E22F6C}']
    /// <summary>
    /// <para>Provides an opportunity to register custom preferences prior to global and
    /// request context initialization.</para>
    /// <para>If |type| is CEF_PREFERENCES_TYPE_GLOBAL the registered preferences can be
    /// accessed via ICefPreferenceManager.GetGlobalPreferences after
    /// OnContextInitialized is called. Global preferences are registered a single
    /// time at application startup. See related TCefSettings.cache_path
    /// configuration.</para>
    /// <para>If |type| is CEF_PREFERENCES_TYPE_REQUEST_CONTEXT the preferences can be
    /// accessed via the ICefRequestContext after
    /// ICefRequestContextHandler.OnRequestContextInitialized is called.
    /// Request context preferences are registered each time a new
    /// ICefRequestContext is created. It is intended but not required that all
    /// request contexts have the same registered preferences. See related
    /// TCefRequestContextSettings.cache_path configuration.</para>
    /// <para>Do not keep a reference to the |registrar| object. This function is called
    /// on the browser process UI thread.</para>
    /// </summary>
    procedure OnRegisterCustomPreferences(type_: TCefPreferencesType; registrar: PCefPreferenceRegistrar);
    /// <summary>
    /// Called on the browser process UI thread immediately after the CEF context
    /// has been initialized.
    /// </summary>
    procedure OnContextInitialized;
    /// <summary>
    /// Called before a child process is launched. Will be called on the browser
    /// process UI thread when launching a render process and on the browser
    /// process IO thread when launching a GPU process. Provides an opportunity to
    /// modify the child process command line. Do not keep a reference to
    /// |command_line| outside of this function.
    /// </summary>
    procedure OnBeforeChildProcessLaunch(const commandLine: ICefCommandLine);
    /// <summary>
    /// <para>Implement this function to provide app-specific behavior when an already
    /// running app is relaunched with the same TCefSettings.root_cache_path value.
    /// For example, activate an existing app window or create a new app window.
    /// |command_line| will be read-only. Do not keep a reference to
    /// |command_line| outside of this function. Return true (1) if the relaunch
    /// is handled or false (0) for default relaunch behavior. Default behavior
    /// will create a new default styled Chrome window.</para>
    /// <para>To avoid cache corruption only a single app instance is allowed to run for
    /// a given TCefSettings.root_cache_path value. On relaunch the app checks a
    /// process singleton lock and then forwards the new launch arguments to the
    /// already running app process before exiting early. Client apps should
    /// therefore check the cef_initialize() return value for early exit before
    /// proceeding.</para>
    /// <para>This function will be called on the browser process UI thread.</para>
    /// </summary>
    procedure OnAlreadyRunningAppRelaunch(const commandLine: ICefCommandLine; const current_directory: ustring; var aResult: boolean);
    /// <summary>
    /// Called from any thread when work has been scheduled for the browser
    /// process main (UI) thread. This callback is used in combination with
    /// TCefSettings.external_message_pump and GlobalCEFApp.DoMessageLoopWork in
    /// cases where the CEF message loop must be integrated into an existing
    /// application message loop (see additional comments and warnings on
    /// GlobalCEFApp.DoMessageLoopWork). This callback should schedule a
    /// GlobalCEFApp.DoMessageLoopWork call to happen on the main (UI) thread.
    /// |delay_ms| is the requested delay in milliseconds. If |delay_ms| is <= 0
    /// then the call should happen reasonably soon. If |delay_ms| is > 0 then the
    /// call should be scheduled to happen after the specified delay and any
    /// currently pending scheduled call should be cancelled.
    /// </summary>
    procedure OnScheduleMessagePumpWork(const delayMs: Int64);
    /// <summary>
    /// Return the default client for use with a newly created browser window
    /// (TCefBrowser object). If null is returned the TCefBrowser will be
    /// unmanaged (no callbacks will be executed for that TCefBrowser) and
    /// application shutdown will be blocked until the browser window is closed
    /// manually. This function is currently only used with Chrome style when
    /// creating new browser windows via Chrome UI.
    /// </summary>
    procedure GetDefaultClient(var aClient : ICefClient);
    /// <summary>
    /// Return the default handler for use with a new user or incognito profile
    /// (TCefRequestContext object). If null is returned the
    /// TCefRequestContext will be unmanaged (no callbacks will be executed for
    /// that TCefRequestContext). This function is currently only used with
    /// Chrome style when creating new browser windows via Chrome UI.
    /// </summary>
    procedure GetDefaultRequestContextHandler(var aRequestContextHandler : ICefRequestContextHandler);
    /// <summary>
    /// Custom procedure to clear all references.
    /// </summary>
    procedure RemoveReferences;
  end;

  /// <summary>
  /// Interface used to implement render process callbacks. The functions of this
  /// interface will be called on the render process main thread (TID_RENDERER)
  /// unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefRenderProcessHandler">Implements TCefRenderProcessHandler</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_render_process_handler_capi.h">CEF source file: /include/capi/cef_render_process_handler_capi.h (cef_render_process_handler_t)</see></para>
  /// </remarks>
  ICefRenderProcessHandler = interface(ICefBaseRefCounted)
    ['{FADEE3BC-BF66-430A-BA5D-1EE3782ECC58}']
    /// <summary>
    /// Called after WebKit has been initialized.
    /// </summary>
    procedure OnWebKitInitialized;
    /// <summary>
    /// Called after a browser has been created. When browsing cross-origin a new
    /// browser will be created before the old browser with the same identifier is
    /// destroyed. |extra_info| is an optional read-only value originating from
    /// cef_browser_host_create_browser(),
    /// cef_browser_host_create_browser_sync(),
    /// ICefLifeSpanHandler.OnBeforePopup or
    /// cef_browser_view_create().
    /// </summary>
    procedure OnBrowserCreated(const browser: ICefBrowser; const extra_info: ICefDictionaryValue);
    /// <summary>
    /// Called before a browser is destroyed.
    /// </summary>
    procedure OnBrowserDestroyed(const browser: ICefBrowser);
    /// <summary>
    /// Return the handler for browser load status events.
    /// </summary>
    function  GetLoadHandler : ICefLoadHandler;
    /// <summary>
    /// Called immediately after the V8 context for a frame has been created. To
    /// retrieve the JavaScript 'window' object use the
    /// ICefv8context.GetGlobal function. V8 handles can only be accessed
    /// from the thread on which they are created. A task runner for posting tasks
    /// on the associated thread can be retrieved via the
    /// ICefv8context.GetTaskRunner() function.
    /// </summary>
    procedure OnContextCreated(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context);
    /// <summary>
    /// Called immediately before the V8 context for a frame is released. No
    /// references to the context should be kept after this function is called.
    /// </summary>
    procedure OnContextReleased(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context);
    /// <summary>
    /// Called for global uncaught exceptions in a frame. Execution of this
    /// callback is disabled by default. To enable set
    /// TCefSettings.uncaught_exception_stack_size > 0.
    /// </summary>
    procedure OnUncaughtException(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context; const V8Exception: ICefV8Exception; const stackTrace: ICefV8StackTrace);
    /// <summary>
    /// Called when a new node in the the browser gets focus. The |node| value may
    /// be NULL if no specific node has gained focus. The node object passed to
    /// this function represents a snapshot of the DOM at the time this function
    /// is executed. DOM objects are only valid for the scope of this function. Do
    /// not keep references to or attempt to access any DOM objects outside the
    /// scope of this function.
    /// </summary>
    procedure OnFocusedNodeChanged(const browser: ICefBrowser; const frame: ICefFrame; const node: ICefDomNode);
    /// <summary>
    /// Called when a new message is received from a different process. Return
    /// true (1) if the message was handled or false (0) otherwise. It is safe to
    /// keep a reference to |message| outside of this callback.
    /// </summary>
    function  OnProcessMessageReceived(const browser: ICefBrowser; const frame: ICefFrame; sourceProcess: TCefProcessId; const aMessage: ICefProcessMessage): Boolean;
    /// <summary>
    /// Custom procedure to clear all references.
    /// </summary>
    procedure RemoveReferences;
  end;

  /// <summary>
  /// Implement this interface to provide handler implementations. Methods will be
  /// called by the process and/or thread indicated.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefApp">Implements TCefApp</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_app_capi.h">CEF source file: /include/capi/cef_app_capi.h (cef_app_t)</see></para>
  /// </remarks>
  ICefApp = interface(ICefBaseRefCounted)
    ['{970CA670-9070-4642-B188-7D8A22DAEED4}']
    /// <summary>
    /// Provides an opportunity to view and/or modify command-line arguments
    /// before processing by CEF and Chromium. The |process_type| value will be
    /// NULL for the browser process. Do not keep a reference to the
    /// ICefCommandLine object passed to this function. The
    /// TCefSettings.command_line_args_disabled value can be used to start with
    /// an NULL command-line object. Any values specified in CefSettings that
    /// equate to command-line arguments will be set before this function is
    /// called. Be cautious when using this function to modify command-line
    /// arguments for non-browser processes as this may result in undefined
    /// behavior including crashes.
    /// </summary>
    procedure OnBeforeCommandLineProcessing(const processType: ustring; const commandLine: ICefCommandLine);
    /// <summary>
    /// Provides an opportunity to register custom schemes. Do not keep a
    /// reference to the |registrar| object. This function is called on the main
    /// thread for each process and the registered schemes should be the same
    /// across all processes.
    /// </summary>
    procedure OnRegisterCustomSchemes(const registrar: TCefSchemeRegistrarRef);
    /// <summary>
    /// Return the handler for resource bundle events. If no handler is returned
    /// resources will be loaded from pack files. This function is called by the
    /// browser and render processes on multiple threads.
    /// </summary>
    procedure GetResourceBundleHandler(var aHandler : ICefResourceBundleHandler);
    /// <summary>
    /// Return the handler for functionality specific to the browser process. This
    /// function is called on multiple threads in the browser process.
    /// </summary>
    procedure GetBrowserProcessHandler(var aHandler : ICefBrowserProcessHandler);
    /// <summary>
    /// Return the handler for functionality specific to the render process. This
    /// function is called on the render process main thread.
    /// </summary>
    procedure GetRenderProcessHandler(var aHandler : ICefRenderProcessHandler);
    /// <summary>
    /// Custom procedure to clear all references.
    /// </summary>
    procedure RemoveReferences;
  end;

  /// <summary>
  /// Generic callback interface used for asynchronous completion.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefCompletionCallback">Implements TCefCompletionCallback</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_callback_capi.h">CEF source file: /include/capi/cef_callback_capi.h (cef_completion_callback_t)</see></para>
  /// </remarks>
  ICefCompletionCallback = interface(ICefBaseRefCounted)
    ['{A8ECCFBB-FEE0-446F-AB32-AD69A7478D57}']
    /// <summary>
    /// Method that will be called once the task is complete.
    /// </summary>
    procedure OnComplete;
  end;

  /// <summary>
  /// Interface to implement to be notified of asynchronous completion via
  /// ICefCookieManager.SetCookie.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefSetCookieCallback">Implements TCefSetCookieCallback</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_cookie_capi.h">CEF source file: /include/capi/cef_cookie_capi.h (cef_set_cookie_callback_t)</see></para>
  /// </remarks>
  ICefSetCookieCallback = interface(ICefBaseRefCounted)
    ['{16E14B6F-CB0A-4F9D-A008-239E0BC7B892}']
    /// <summary>
    /// Method that will be called upon completion. |success| will be true (1) if
    /// the cookie was set successfully.
    /// </summary>
    procedure OnComplete(success: Boolean);
  end;

  /// <summary>
  /// Interface to implement to be notified of asynchronous completion via
  /// ICefCookieManager.DeleteCookies.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefDeleteCookiesCallback">Implements TCefDeleteCookiesCallback</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_cookie_capi.h">CEF source file: /include/capi/cef_cookie_capi.h (cef_delete_cookies_callback_t)</see></para>
  /// </remarks>
  ICefDeleteCookiesCallback = interface(ICefBaseRefCounted)
    ['{758B79A1-B9E8-4F0D-94A0-DCE5AFADE33D}']
    /// <summary>
    /// Method that will be called upon completion. |num_deleted| will be the
    /// number of cookies that were deleted.
    /// </summary>
    procedure OnComplete(numDeleted: Integer);
  end;

  /// <summary>
  /// Interface used for managing cookies. The functions of this interface may be
  /// called on any thread unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefCookieManager">Implements TCefCookieManager</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_cookie_capi.h">CEF source file: /include/capi/cef_cookie_capi.h (cef_cookie_manager_t)</see></para>
  /// </remarks>
  ICefCookieManager = Interface(ICefBaseRefCounted)
    ['{CC1749E6-9AD3-4283-8430-AF6CBF3E8785}']
    /// <summary>
    /// Visit all cookies on the UI thread. The returned cookies are ordered by
    /// longest path, then by earliest creation date. Returns false (0) if cookies
    /// cannot be accessed.
    /// </summary>
    function  VisitAllCookies(const visitor: ICefCookieVisitor): Boolean;
    function  VisitAllCookiesProc(const visitor: TCefCookieVisitorProc): Boolean;
    /// <summary>
    /// Visit a subset of cookies on the UI thread. The results are filtered by
    /// the given url scheme, host, domain and path. If |includeHttpOnly| is true
    /// (1) HTTP-only cookies will also be included in the results. The returned
    /// cookies are ordered by longest path, then by earliest creation date.
    /// Returns false (0) if cookies cannot be accessed.
    /// </summary>
    function  VisitUrlCookies(const url: ustring; includeHttpOnly: Boolean; const visitor: ICefCookieVisitor): Boolean;
    function  VisitUrlCookiesProc(const url: ustring; includeHttpOnly: Boolean; const visitor: TCefCookieVisitorProc): Boolean;
    /// <summary>
    /// Sets a cookie given a valid URL and explicit user-provided cookie
    /// attributes. This function expects each attribute to be well-formed. It
    /// will check for disallowed characters (e.g. the ';' character is disallowed
    /// within the cookie value attribute) and fail without setting the cookie if
    /// such characters are found. If |callback| is non-NULL it will be executed
    /// asnychronously on the UI thread after the cookie has been set. Returns
    /// false (0) if an invalid URL is specified or if cookies cannot be accessed.
    /// </summary>
    function  SetCookie(const url, name, value, domain, path: ustring; secure, httponly, hasExpires: Boolean; const creation, lastAccess, expires: TDateTime; same_site : TCefCookieSameSite; priority : TCefCookiePriority; const callback: ICefSetCookieCallback): Boolean;
    function  SetCookieProc(const url: ustring; const name, value, domain, path: ustring; secure, httponly, hasExpires: Boolean; const creation, lastAccess, expires: TDateTime; same_site : TCefCookieSameSite; priority : TCefCookiePriority; const callback: TCefSetCookieCallbackProc): Boolean;
    /// <summary>
    /// Delete all cookies that match the specified parameters. If both |url| and
    /// |cookie_name| values are specified all host and domain cookies matching
    /// both will be deleted. If only |url| is specified all host cookies (but not
    /// domain cookies) irrespective of path will be deleted. If |url| is NULL all
    /// cookies for all hosts and domains will be deleted. If |callback| is non-
    /// NULL it will be executed asnychronously on the UI thread after the cookies
    /// have been deleted. Returns false (0) if a non-NULL invalid URL is
    /// specified or if cookies cannot be accessed. Cookies can alternately be
    /// deleted using the Visit*Cookies() functions.
    /// </summary>
    function  DeleteCookies(const url, cookieName: ustring; const callback: ICefDeleteCookiesCallback): Boolean;
    function  DeleteCookiesProc(const url, cookieName: ustring; const callback: TCefDeleteCookiesCallbackProc): Boolean;
    /// <summary>
    /// Flush the backing store (if any) to disk. If |callback| is non-NULL it
    /// will be executed asnychronously on the UI thread after the flush is
    /// complete. Returns false (0) if cookies cannot be accessed.
    /// </summary>
    function  FlushStore(const callback: ICefCompletionCallback): Boolean;
    function  FlushStoreProc(const proc: TCefCompletionCallbackProc): Boolean;
  end;

  /// <summary>
  /// Generic callback interface used for asynchronous continuation.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefCallback">Implements TCefCallback</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_callback_capi.h">CEF source file: /include/capi/cef_callback_capi.h (cef_callback_t)</see></para>
  /// </remarks>
  ICefCallback = interface(ICefBaseRefCounted)
    ['{1B8C449F-E2D6-4B78-9BBA-6F47E8BCDF37}']
    /// <summary>
    /// Continue processing.
    /// </summary>
      procedure Cont;
    /// <summary>
    /// Cancel processing.
    /// </summary>
    procedure Cancel;
  end;

  /// <summary>
  /// Callback for asynchronous continuation of ICefResourceHandler.skip.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefResourceSkipCallback">Implements TCefResourceSkipCallback</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_resource_handler_capi.h">CEF source file: /include/capi/cef_resource_handler_capi.h (cef_resource_skip_callback_t)</see></para>
  /// </remarks>
  ICefResourceSkipCallback = interface(ICefBaseRefCounted)
    ['{5ADDE93E-5858-41FD-81E8-ED8BF710D92A}']
    /// <summary>
    /// Callback for asynchronous continuation of skip(). If |bytes_skipped| > 0
    /// then either skip() will be called again until the requested number of
    /// bytes have been skipped or the request will proceed. If |bytes_skipped| <=
    /// 0 the request will fail with ERR_REQUEST_RANGE_NOT_SATISFIABLE.
    /// </summary>
    procedure Cont(bytes_skipped: int64);
  end;

  /// <summary>
  /// Callback for asynchronous continuation of ICefResourceHandler.read.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefResourceReadCallback">Implements TCefResourceReadCallback</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_resource_handler_capi.h">CEF source file: /include/capi/cef_resource_handler_capi.h (cef_resource_read_callback_t)</see></para>
  /// </remarks>
  ICefResourceReadCallback = interface(ICefBaseRefCounted)
    ['{7669335F-7A4B-4657-86CA-C02B12369602}']
    /// <summary>
    /// Callback for asynchronous continuation of read(). If |bytes_read| == 0 the
    /// response will be considered complete. If |bytes_read| > 0 then read() will
    /// be called again until the request is complete (based on either the result
    /// or the expected content length). If |bytes_read| < 0 then the request will
    /// fail and the |bytes_read| value will be treated as the error code.
    /// </summary>
    procedure Cont(bytes_read: int64);
  end;

  /// <summary>
  /// Interface used to implement a custom request handler interface. The
  /// functions of this interface will be called on the IO thread unless otherwise
  /// indicated.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefResourceHandler">Implements TCefResourceHandler</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_resource_handler_capi.h">CEF source file: /include/capi/cef_resource_handler_capi.h (cef_resource_handler_t)</see></para>
  /// </remarks>
  ICefResourceHandler = interface(ICefBaseRefCounted)
    ['{BD3EA208-AAAD-488C-BFF2-76993022F2B5}']
    /// <summary>
    /// Open the response stream. To handle the request immediately set
    /// |handle_request| to true (1) and return true (1). To decide at a later
    /// time set |handle_request| to false (0), return true (1), and execute
    /// |callback| to continue or cancel the request. To cancel the request
    /// immediately set |handle_request| to true (1) and return false (0). This
    /// function will be called in sequence but not from a dedicated thread. For
    /// backwards compatibility set |handle_request| to false (0) and return false
    /// (0) and the ProcessRequest function will be called.
    /// </summary>
    function  open(const request: ICefRequest; var handle_request: boolean; const callback: ICefCallback): boolean;
    /// <summary>
    /// Begin processing the request. To handle the request return true (1) and
    /// call ICefCallback.cont() once the response header information is
    /// available (ICefCallback.cont() can also be called from inside this
    /// function if header information is available immediately). To cancel the
    /// request return false (0).
    /// </summary>
    /// <remarks>
    /// <para>WARNING: This function is deprecated. Use Open instead.</para>
    /// </remarks>
    function  ProcessRequest(const request: ICefRequest; const callback: ICefCallback): boolean;
    /// <summary>
    /// Retrieve response header information. If the response length is not known
    /// set |response_length| to -1 and read_response() will be called until it
    /// returns false (0). If the response length is known set |response_length|
    /// to a positive value and read_response() will be called until it returns
    /// false (0) or the specified number of bytes have been read. Use the
    /// |response| object to set the mime type, http status code and other
    /// optional header values. To redirect the request to a new URL set
    /// |redirectUrl| to the new URL. |redirectUrl| can be either a relative or
    /// fully qualified URL. It is also possible to set |response| to a redirect
    /// http status code and pass the new URL via a Location header. Likewise with
    /// |redirectUrl| it is valid to set a relative or fully qualified URL as the
    /// Location header value. If an error occured while setting up the request
    /// you can call set_error() on |response| to indicate the error condition.
    /// </summary>
    procedure GetResponseHeaders(const response: ICefResponse; out responseLength: Int64; out redirectUrl: ustring);
    /// <summary>
    /// Skip response data when requested by a Range header. Skip over and discard
    /// |bytes_to_skip| bytes of response data. If data is available immediately
    /// set |bytes_skipped| to the number of bytes skipped and return true (1). To
    /// read the data at a later time set |bytes_skipped| to 0, return true (1)
    /// and execute |callback| when the data is available. To indicate failure set
    /// |bytes_skipped| to < 0 (e.g. -2 for ERR_FAILED) and return false (0). This
    /// function will be called in sequence but not from a dedicated thread.
    /// </summary>
    function  skip(bytes_to_skip: int64; var bytes_skipped: Int64; const callback: ICefResourceSkipCallback): boolean;
    /// <summary>
    /// Read response data. If data is available immediately copy up to
    /// |bytes_to_read| bytes into |data_out|, set |bytes_read| to the number of
    /// bytes copied, and return true (1). To read the data at a later time keep a
    /// pointer to |data_out|, set |bytes_read| to 0, return true (1) and execute
    /// |callback| when the data is available (|data_out| will remain valid until
    /// the callback is executed). To indicate response completion set
    /// |bytes_read| to 0 and return false (0). To indicate failure set
    /// |bytes_read| to < 0 (e.g. -2 for ERR_FAILED) and return false (0). This
    /// function will be called in sequence but not from a dedicated thread. For
    /// backwards compatibility set |bytes_read| to -1 and return false (0) and
    /// the ReadResponse function will be called.
    /// </summary>
    function  read(const data_out: Pointer; bytes_to_read: Integer; var bytes_read: Integer; const callback: ICefResourceReadCallback): boolean;
    /// <summary>
    /// Read response data. If data is available immediately copy up to
    /// |bytes_to_read| bytes into |data_out|, set |bytes_read| to the number of
    /// bytes copied, and return true (1). To read the data at a later time set
    /// |bytes_read| to 0, return true (1) and call ICefCallback.cont() when
    /// the data is available. To indicate response completion return false (0).
    /// </summary>
    /// <remarks>
    /// <para>WARNING: This function is deprecated. Use Skip and Read instead.</para>
    /// </remarks>
    function  ReadResponse(const dataOut: Pointer; bytesToRead: Integer; var bytesRead: Integer; const callback: ICefCallback): boolean;
    /// <summary>
    /// Request processing has been canceled.
    /// </summary>
    procedure Cancel;
  end;

  /// <summary>
  /// Interface that creates ICefResourceHandler instances for handling scheme
  /// requests. The functions of this interface will always be called on the IO
  /// thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefSchemeHandlerFactory">Implements TCefSchemeHandlerFactory</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_scheme_capi.h">CEF source file: /include/capi/cef_scheme_capi.h (cef_scheme_handler_factory_t)</see></para>
  /// </remarks>
  ICefSchemeHandlerFactory = interface(ICefBaseRefCounted)
    ['{4D9B7960-B73B-4EBD-9ABE-6C1C43C245EB}']
    /// <summary>
    /// Return a new resource handler instance to handle the request or an NULL
    /// reference to allow default handling of the request. |browser| and |frame|
    /// will be the browser window and frame respectively that originated the
    /// request or NULL if the request did not originate from a browser window
    /// (for example, if the request came from ICefUrlRequest). The |request|
    /// object passed to this function cannot be modified.
    /// </summary>
    function New(const browser: ICefBrowser; const frame: ICefFrame; const schemeName: ustring; const request: ICefRequest): ICefResourceHandler;
  end;

  /// <summary>
  /// Callback interface used for asynchronous continuation of authentication
  /// requests.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefAuthCallback">Implements TCefAuthCallback</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_auth_callback_capi.h">CEF source file: /include/capi/cef_auth_callback_capi.h (cef_auth_callback_t)</see></para>
  /// </remarks>
  ICefAuthCallback = interface(ICefBaseRefCounted)
    ['{500C2023-BF4D-4FF7-9C04-165E5C389131}']
    /// <summary>
    /// Continue the authentication request.
    /// </summary>
    procedure Cont(const username, password: ustring);
    /// <summary>
    /// Cancel the authentication request.
    /// </summary>
    procedure Cancel;
  end;

  /// <summary>
  /// Callback interface used for asynchronous continuation of JavaScript dialog
  /// requests.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefJsDialogCallback">Implements TCefJsDialogCallback</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_jsdialog_handler_capi.h">CEF source file: /include/capi/cef_jsdialog_handler_capi.h (cef_jsdialog_callback_t)</see></para>
  /// </remarks>
  ICefJsDialogCallback = interface(ICefBaseRefCounted)
    ['{187B2156-9947-4108-87AB-32E559E1B026}']
    /// <summary>
    /// Continue the JS dialog request. Set |success| to true (1) if the OK button
    /// was pressed. The |user_input| value should be specified for prompt
    /// dialogs.
    /// </summary>
    procedure Cont(success: Boolean; const userInput: ustring);
  end;

  /// <summary>
  /// Provides information about the context menu state. The functions of this
  /// interface can only be accessed on browser process the UI thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefContextMenuParams">Implements TCefContextMenuParams</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_context_menu_handler_capi.h">CEF source file: /include/capi/cef_context_menu_handler_capi.h (cef_context_menu_params_t)</see></para>
  /// </remarks>
  ICefContextMenuParams = interface(ICefBaseRefCounted)
    ['{E31BFA9E-D4E2-49B7-A05D-20018C8794EB}']
    /// <summary>
    /// Returns the X coordinate of the mouse where the context menu was invoked.
    /// Coords are relative to the associated RenderView's origin.
    /// </summary>
    function GetXCoord: Integer;
    /// <summary>
    /// Returns the Y coordinate of the mouse where the context menu was invoked.
    /// Coords are relative to the associated RenderView's origin.
    /// </summary>
    function GetYCoord: Integer;
    /// <summary>
    /// Returns flags representing the type of node that the context menu was
    /// invoked on.
    /// </summary>
    function GetTypeFlags: TCefContextMenuTypeFlags;
    /// <summary>
    /// Returns the URL of the link, if any, that encloses the node that the
    /// context menu was invoked on.
    /// </summary>
    function GetLinkUrl: ustring;
    /// <summary>
    /// Returns the link URL, if any, to be used ONLY for "copy link address". We
    /// don't validate this field in the frontend process.
    /// </summary>
    function GetUnfilteredLinkUrl: ustring;
    /// <summary>
    /// Returns the source URL, if any, for the element that the context menu was
    /// invoked on. Example of elements with source URLs are img, audio, and
    /// video.
    /// </summary>
    function GetSourceUrl: ustring;
    /// <summary>
    /// Returns true (1) if the context menu was invoked on an image which has
    /// non-NULL contents.
    /// </summary>
    function HasImageContents: Boolean;
    /// <summary>
    /// Returns the title text or the alt text if the context menu was invoked on
    /// an image.
    /// </summary>
    function GetTitleText: ustring;
    /// <summary>
    /// Returns the URL of the top level page that the context menu was invoked
    /// on.
    /// </summary>
    function GetPageUrl: ustring;
    /// <summary>
    /// Returns the URL of the subframe that the context menu was invoked on.
    /// </summary>
    function GetFrameUrl: ustring;
    /// <summary>
    /// Returns the character encoding of the subframe that the context menu was
    /// invoked on.
    /// </summary>
    function GetFrameCharset: ustring;
    /// <summary>
    /// Returns the type of context node that the context menu was invoked on.
    /// </summary>
    function GetMediaType: TCefContextMenuMediaType;
    /// <summary>
    /// Returns flags representing the actions supported by the media element, if
    /// any, that the context menu was invoked on.
    /// </summary>
    function GetMediaStateFlags: TCefContextMenuMediaStateFlags;
    /// <summary>
    /// Returns the text of the selection, if any, that the context menu was
    /// invoked on.
    /// </summary>
    function GetSelectionText: ustring;
    /// <summary>
    /// Returns the text of the misspelled word, if any, that the context menu was
    /// invoked on.
    /// </summary>
    function GetMisspelledWord: ustring;
    /// <summary>
    /// Returns true (1) if suggestions exist, false (0) otherwise. Fills in
    /// |suggestions| from the spell check service for the misspelled word if
    /// there is one.
    /// </summary>
    function GetDictionarySuggestions(const suggestions: TStringList): Boolean;
    /// <summary>
    /// Returns true (1) if the context menu was invoked on an editable node.
    /// </summary>
    function IsEditable: Boolean;
    /// <summary>
    /// Returns true (1) if the context menu was invoked on an editable node where
    /// spell-check is enabled.
    /// </summary>
    function IsSpellCheckEnabled: Boolean;
    /// <summary>
    /// Returns flags representing the actions supported by the editable node, if
    /// any, that the context menu was invoked on.
    /// </summary>
    function GetEditStateFlags: TCefContextMenuEditStateFlags;
    /// <summary>
    /// Returns true (1) if the context menu contains items specified by the
    /// renderer process.
    /// </summary>
    function IsCustomMenu: Boolean;

    /// <summary>
    /// Returns the X coordinate of the mouse where the context menu was invoked.
    /// Coords are relative to the associated RenderView's origin.
    /// </summary>
    property XCoord            : Integer                        read GetXCoord;
    /// <summary>
    /// Returns the Y coordinate of the mouse where the context menu was invoked.
    /// Coords are relative to the associated RenderView's origin.
    /// </summary>
    property YCoord            : Integer                        read GetYCoord;
    /// <summary>
    /// Returns flags representing the type of node that the context menu was
    /// invoked on.
    /// </summary>
    property TypeFlags         : TCefContextMenuTypeFlags       read GetTypeFlags;
    /// <summary>
    /// Returns the URL of the link, if any, that encloses the node that the
    /// context menu was invoked on.
    /// </summary>
    property LinkUrl           : ustring                        read GetLinkUrl;
    /// <summary>
    /// Returns the link URL, if any, to be used ONLY for "copy link address". We
    /// don't validate this field in the frontend process.
    /// </summary>
    property UnfilteredLinkUrl : ustring                        read GetUnfilteredLinkUrl;
    /// <summary>
    /// Returns the source URL, if any, for the element that the context menu was
    /// invoked on. Example of elements with source URLs are img, audio, and
    /// video.
    /// </summary>
    property SourceUrl         : ustring                        read GetSourceUrl;
    /// <summary>
    /// Returns the title text or the alt text if the context menu was invoked on
    /// an image.
    /// </summary>
    property TitleText         : ustring                        read GetTitleText;
    /// <summary>
    /// Returns the URL of the top level page that the context menu was invoked
    /// on.
    /// </summary>
    property PageUrl           : ustring                        read GetPageUrl;
    /// <summary>
    /// Returns the URL of the subframe that the context menu was invoked on.
    /// </summary>
    property FrameUrl          : ustring                        read GetFrameUrl;
    /// <summary>
    /// Returns the character encoding of the subframe that the context menu was
    /// invoked on.
    /// </summary>
    property FrameCharset      : ustring                        read GetFrameCharset;
    /// <summary>
    /// Returns the type of context node that the context menu was invoked on.
    /// </summary>
    property MediaType         : TCefContextMenuMediaType       read GetMediaType;
    /// <summary>
    /// Returns flags representing the actions supported by the media element, if
    /// any, that the context menu was invoked on.
    /// </summary>
    property MediaStateFlags   : TCefContextMenuMediaStateFlags read GetMediaStateFlags;
    /// <summary>
    /// Returns the text of the selection, if any, that the context menu was
    /// invoked on.
    /// </summary>
    property SelectionText     : ustring                        read GetSelectionText;
    /// <summary>
    /// Returns the text of the misspelled word, if any, that the context menu was
    /// invoked on.
    /// </summary>
    property MisspelledWord    : ustring                        read GetMisspelledWord;
    /// <summary>
    /// Returns flags representing the actions supported by the editable node, if
    /// any, that the context menu was invoked on.
    /// </summary>
    property EditStateFlags    : TCefContextMenuEditStateFlags  read GetEditStateFlags;
  end;

  /// <summary>
  /// Supports creation and modification of menus. See TCefMenuId for the
  /// command ids that have default implementations. All user-defined command ids
  /// should be between MENU_ID_USER_FIRST and MENU_ID_USER_LAST. The functions of
  /// this interface can only be accessed on the browser process the UI thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefMenuModel">Implements TCefMenuModel</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_menu_model_capi.h">CEF source file: /include/capi/cef_menu_model_capi.h (cef_menu_model_t)</see></para>
  /// </remarks>
  ICefMenuModel = interface(ICefBaseRefCounted)
    ['{40AF19D3-8B4E-44B8-8F89-DEB5907FC495}']
    /// <summary>
    /// Returns true (1) if this menu is a submenu.
    /// </summary>
    function IsSubMenu: Boolean;
    /// <summary>
    /// Clears the menu. Returns true (1) on success.
    /// </summary>
    function Clear: Boolean;
    /// <summary>
    /// Returns the number of items in this menu.
    /// </summary>
    function GetCount: NativeUInt;
    /// <summary>
    /// Add a separator to the menu. Returns true (1) on success.
    /// </summary>
    function AddSeparator: Boolean;
    /// <summary>
    /// Add an item to the menu. Returns true (1) on success.
    /// </summary>
    function AddItem(commandId: Integer; const text: ustring): Boolean;
    /// <summary>
    /// Add a check item to the menu. Returns true (1) on success.
    /// </summary>
    function AddCheckItem(commandId: Integer; const text: ustring): Boolean;
    /// <summary>
    /// Add a radio item to the menu. Only a single item with the specified
    /// |group_id| can be checked at a time. Returns true (1) on success.
    /// </summary>
    function AddRadioItem(commandId: Integer; const text: ustring; groupId: Integer): Boolean;
    /// <summary>
    /// Add a sub-menu to the menu. The new sub-menu is returned.
    /// </summary>
    function AddSubMenu(commandId: Integer; const text: ustring): ICefMenuModel;
    /// <summary>
    /// Insert a separator in the menu at the specified |index|. Returns true (1)
    /// on success.
    /// </summary>
    function InsertSeparatorAt(index: NativeUInt): Boolean;
    /// <summary>
    /// Insert an item in the menu at the specified |index|. Returns true (1) on
    /// success.
    /// </summary>
    function InsertItemAt(index: NativeUInt; commandId: Integer; const text: ustring): Boolean;
    /// <summary>
    /// Insert a check item in the menu at the specified |index|. Returns true (1)
    /// on success.
    /// </summary>
    function InsertCheckItemAt(index: NativeUInt; commandId: Integer; const text: ustring): Boolean;
    /// <summary>
    /// Insert a radio item in the menu at the specified |index|. Only a single
    /// item with the specified |group_id| can be checked at a time. Returns true
    /// (1) on success.
    /// </summary>
    function InsertRadioItemAt(index: NativeUInt; commandId: Integer; const text: ustring; groupId: Integer): Boolean;
    /// <summary>
    /// Insert a sub-menu in the menu at the specified |index|. The new sub-menu
    /// is returned.
    /// </summary>
    function InsertSubMenuAt(index: NativeUInt; commandId: Integer; const text: ustring): ICefMenuModel;
    /// <summary>
    /// Removes the item with the specified |command_id|. Returns true (1) on
    /// success.
    /// </summary>
    function Remove(commandId: Integer): Boolean;
    /// <summary>
    /// Removes the item at the specified |index|. Returns true (1) on success.
    /// </summary>
    function RemoveAt(index: NativeUInt): Boolean;
    /// <summary>
    /// Returns the index associated with the specified |command_id| or -1 if not
    /// found due to the command id not existing in the menu.
    /// </summary>
    function GetIndexOf(commandId: Integer): Integer;
    /// <summary>
    /// Returns the command id at the specified |index| or -1 if not found due to
    /// invalid range or the index being a separator.
    /// </summary>
    function GetCommandIdAt(index: NativeUInt): Integer;
    /// <summary>
    /// Sets the command id at the specified |index|. Returns true (1) on success.
    /// </summary>
    function SetCommandIdAt(index: NativeUInt; commandId: Integer): Boolean;
    /// <summary>
    /// Returns the label for the specified |command_id| or NULL if not found.
    /// </summary>
    function GetLabel(commandId: Integer): ustring;
    /// <summary>
    /// Returns the label at the specified |index| or NULL if not found due to
    /// invalid range or the index being a separator.
    /// </summary>
    function GetLabelAt(index: NativeUInt): ustring;
    /// <summary>
    /// Sets the label for the specified |command_id|. Returns true (1) on
    /// success.
    /// </summary>
    function SetLabel(commandId: Integer; const text: ustring): Boolean;
    /// <summary>
    /// Set the label at the specified |index|. Returns true (1) on success.
    /// </summary>
    function SetLabelAt(index: NativeUInt; const text: ustring): Boolean;
    /// <summary>
    /// Returns the item type for the specified |command_id|.
    /// </summary>
    function GetType(commandId: Integer): TCefMenuItemType;
    /// <summary>
    /// Returns the item type at the specified |index|.
    /// </summary>
    function GetTypeAt(index: NativeUInt): TCefMenuItemType;
    /// <summary>
    /// Returns the group id for the specified |command_id| or -1 if invalid.
    /// </summary>
    function GetGroupId(commandId: Integer): Integer;
    /// <summary>
    /// Returns the group id at the specified |index| or -1 if invalid.
    /// </summary>
    function GetGroupIdAt(index: NativeUInt): Integer;
    /// <summary>
    /// Sets the group id for the specified |command_id|. Returns true (1) on
    /// success.
    /// </summary>
    function SetGroupId(commandId, groupId: Integer): Boolean;
    /// <summary>
    /// Sets the group id at the specified |index|. Returns true (1) on success.
    /// </summary>
    function SetGroupIdAt(index: NativeUInt; groupId: Integer): Boolean;
    /// <summary>
    /// Returns the submenu for the specified |command_id| or NULL if invalid.
    /// </summary>
    function GetSubMenu(commandId: Integer): ICefMenuModel;
    /// <summary>
    /// Returns the submenu at the specified |index| or NULL if invalid.
    /// </summary>
    function GetSubMenuAt(index: NativeUInt): ICefMenuModel;
    /// <summary>
    /// Returns true (1) if the specified |command_id| is visible.
    /// </summary>
    function IsVisible(commandId: Integer): Boolean;
    /// <summary>
    /// Returns true (1) if the specified |index| is visible.
    /// </summary>
    function isVisibleAt(index: NativeUInt): Boolean;
    /// <summary>
    /// Change the visibility of the specified |command_id|. Returns true (1) on
    /// success.
    /// </summary>
    function SetVisible(commandId: Integer; visible: Boolean): Boolean;
    /// <summary>
    /// Change the visibility at the specified |index|. Returns true (1) on
    /// success.
    /// </summary>
    function SetVisibleAt(index: NativeUInt; visible: Boolean): Boolean;
    /// <summary>
    /// Returns true (1) if the specified |command_id| is enabled.
    /// </summary>
    function IsEnabled(commandId: Integer): Boolean;
    /// <summary>
    /// Returns true (1) if the specified |index| is enabled.
    /// </summary>
    function IsEnabledAt(index: NativeUInt): Boolean;
    /// <summary>
    /// Change the enabled status of the specified |command_id|. Returns true (1)
    /// on success.
    /// </summary>
    function SetEnabled(commandId: Integer; enabled: Boolean): Boolean;
    /// <summary>
    /// Change the enabled status at the specified |index|. Returns true (1) on
    /// success.
    /// </summary>
    function SetEnabledAt(index: NativeUInt; enabled: Boolean): Boolean;
    /// <summary>
    /// Returns true (1) if the specified |command_id| is checked. Only applies to
    /// check and radio items.
    /// </summary>
    function IsChecked(commandId: Integer): Boolean;
    /// <summary>
    /// Returns true (1) if the specified |index| is checked. Only applies to
    /// check and radio items.
    /// </summary>
    function IsCheckedAt(index: NativeUInt): Boolean;
    /// <summary>
    /// Check the specified |command_id|. Only applies to check and radio items.
    /// Returns true (1) on success.
    /// </summary>
    function setChecked(commandId: Integer; checked: Boolean): Boolean;
    /// <summary>
    /// Check the specified |index|. Only applies to check and radio items.
    /// Returns true (1) on success.
    /// </summary>
    function setCheckedAt(index: NativeUInt; checked: Boolean): Boolean;
    /// <summary>
    /// Returns true (1) if the specified |command_id| has a keyboard accelerator
    /// assigned.
    /// </summary>
    function HasAccelerator(commandId: Integer): Boolean;
    /// <summary>
    /// Returns true (1) if the specified |index| has a keyboard accelerator
    /// assigned.
    /// </summary>
    function HasAcceleratorAt(index: NativeUInt): Boolean;
    /// <summary>
    /// Set the keyboard accelerator for the specified |command_id|. |key_code|
    /// can be any virtual key or character value. Returns true (1) on success.
    /// </summary>
    function SetAccelerator(commandId, keyCode: Integer; shiftPressed, ctrlPressed, altPressed: Boolean): Boolean;
    /// <summary>
    /// Set the keyboard accelerator at the specified |index|. |key_code| can be
    /// any virtual key or character value. Returns true (1) on success.
    /// </summary>
    function SetAcceleratorAt(index: NativeUInt; keyCode: Integer; shiftPressed, ctrlPressed, altPressed: Boolean): Boolean;
    /// <summary>
    /// Remove the keyboard accelerator for the specified |command_id|. Returns
    /// true (1) on success.
    /// </summary>
    function RemoveAccelerator(commandId: Integer): Boolean;
    /// <summary>
    /// Remove the keyboard accelerator at the specified |index|. Returns true (1)
    /// on success.
    /// </summary>
    function RemoveAcceleratorAt(index: NativeUInt): Boolean;
    /// <summary>
    /// Retrieves the keyboard accelerator for the specified |command_id|. Returns
    /// true (1) on success.
    /// </summary>
    function GetAccelerator(commandId: Integer; out keyCode: Integer; out shiftPressed, ctrlPressed, altPressed: Boolean): Boolean;
    /// <summary>
    /// Retrieves the keyboard accelerator for the specified |index|. Returns true
    /// (1) on success.
    /// </summary>
    function GetAcceleratorAt(index: NativeUInt; out keyCode: Integer; out shiftPressed, ctrlPressed, altPressed: Boolean): Boolean;
    /// <summary>
    /// Set the explicit color for |command_id| and |color_type| to |color|.
    /// Specify a |color| value of 0 to remove the explicit color. If no explicit
    /// color or default color is set for |color_type| then the system color will
    /// be used. Returns true (1) on success.
    /// </summary>
    function SetColor(commandId: Integer; colorType: TCefMenuColorType; color: TCefColor): Boolean;
    /// <summary>
    /// Set the explicit color for |command_id| and |index| to |color|. Specify a
    /// |color| value of 0 to remove the explicit color. Specify an |index| value
    /// of -1 to set the default color for items that do not have an explicit
    /// color set. If no explicit color or default color is set for |color_type|
    /// then the system color will be used. Returns true (1) on success.
    /// </summary>
    function SetColorAt(index: Integer; colorType: TCefMenuColorType; color: TCefColor): Boolean;
    /// <summary>
    /// Returns in |color| the color that was explicitly set for |command_id| and
    /// |color_type|. If a color was not set then 0 will be returned in |color|.
    /// Returns true (1) on success.
    /// </summary>
    function GetColor(commandId: Integer; colorType: TCefMenuColorType; out color: TCefColor): Boolean;
    /// <summary>
    /// Returns in |color| the color that was explicitly set for |command_id| and
    /// |color_type|. Specify an |index| value of -1 to return the default color
    /// in |color|. If a color was not set then 0 will be returned in |color|.
    /// Returns true (1) on success.
    /// </summary>
    function GetColorAt(index: Integer; colorType: TCefMenuColorType; out color: TCefColor): Boolean;
    /// <summary>
    /// Sets the font list for the specified |command_id|. If |font_list| is NULL
    /// the system font will be used. Returns true (1) on success. The format is
    /// "<FONT_FAMILY_LIST>,[STYLES] <SIZE>", where:
    /// - FONT_FAMILY_LIST is a comma-separated list of font family names,
    /// - STYLES is an optional space-separated list of style names
    ///   (case-sensitive "Bold" and "Italic" are supported), and
    /// - SIZE is an integer font size in pixels with the suffix "px".
    ///
    /// Here are examples of valid font description strings:
    /// - "Arial, Helvetica, Bold Italic 14px"
    /// - "Arial, 14px"
    /// </summary>
    function SetFontList(commandId: Integer; const fontList: ustring): Boolean;
    /// <summary>
    /// Sets the font list for the specified |index|. Specify an |index| value of
    /// -1 to set the default font. If |font_list| is NULL the system font will be
    /// used. Returns true (1) on success. The format is
    /// "<FONT_FAMILY_LIST>,[STYLES] <SIZE>", where:
    /// - FONT_FAMILY_LIST is a comma-separated list of font family names,
    /// - STYLES is an optional space-separated list of style names
    ///   (case-sensitive "Bold" and "Italic" are supported), and
    /// - SIZE is an integer font size in pixels with the suffix "px".
    ///
    /// Here are examples of valid font description strings:
    /// - "Arial, Helvetica, Bold Italic 14px"
    /// - "Arial, 14px"
    /// </summary>
    function SetFontListAt(index: Integer; const fontList: ustring): Boolean;
  end;

  /// <summary>
  /// Interface that wraps other data value types. Complex types (binary,
  /// dictionary and list) will be referenced but not owned by this object. Can be
  /// used on any process and thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefValue">Implements TCefValue</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_values_capi.h">CEF source file: /include/capi/cef_values_capi.h (cef_value_t)</see></para>
  /// </remarks>
  ICefValue = interface(ICefBaseRefCounted)
    ['{66F9F439-B12B-4EC3-A945-91AE4EF4D4BA}']
    /// <summary>
    /// Returns true (1) if the underlying data is valid. This will always be true
    /// (1) for simple types. For complex types (binary, dictionary and list) the
    /// underlying data may become invalid if owned by another object (e.g. list
    /// or dictionary) and that other object is then modified or destroyed. This
    /// value object can be re-used by calling Set*() even if the underlying data
    /// is invalid.
    /// </summary>
    function IsValid: Boolean;
    /// <summary>
    /// Returns true (1) if the underlying data is owned by another object.
    /// </summary>
    function IsOwned: Boolean;
    /// <summary>
    /// Returns true (1) if the underlying data is read-only. Some APIs may expose
    /// read-only objects.
    /// </summary>
    function IsReadOnly: Boolean;
    /// <summary>
    /// Returns true (1) if this object and |that| object have the same underlying
    /// data. If true (1) modifications to this object will also affect |that|
    /// object and vice-versa.
    /// </summary>
    function IsSame(const that: ICefValue): Boolean;
    /// <summary>
    /// Returns true (1) if this object and |that| object have an equivalent
    /// underlying value but are not necessarily the same object.
    /// </summary>
    function IsEqual(const that: ICefValue): Boolean;
    /// <summary>
    /// Returns a copy of this object. The underlying data will also be copied.
    /// </summary>
    function Copy: ICefValue;
    /// <summary>
    /// Returns the underlying value type.
    /// </summary>
    function GetType: TCefValueType;
    /// <summary>
    /// Returns the underlying value as type bool.
    /// </summary>
    function GetBool: Boolean;
    /// <summary>
    /// Returns the underlying value as type int.
    /// </summary>
    function GetInt: Integer;
    /// <summary>
    /// Returns the underlying value as type double.
    /// </summary>
    function GetDouble: Double;
    /// <summary>
    /// Returns the underlying value as type string.
    /// </summary>
    function GetString: ustring;
    /// <summary>
    /// Returns the underlying value as type binary. The returned reference may
    /// become invalid if the value is owned by another object or if ownership is
    /// transferred to another object in the future. To maintain a reference to
    /// the value after assigning ownership to a dictionary or list pass this
    /// object to the set_value() function instead of passing the returned
    /// reference to set_binary().
    /// </summary>
    function GetBinary: ICefBinaryValue;
    /// <summary>
    /// Returns the underlying value as type dictionary. The returned reference
    /// may become invalid if the value is owned by another object or if ownership
    /// is transferred to another object in the future. To maintain a reference to
    /// the value after assigning ownership to a dictionary or list pass this
    /// object to the set_value() function instead of passing the returned
    /// reference to set_dictionary().
    /// </summary>
    function GetDictionary: ICefDictionaryValue;
    /// <summary>
    /// Returns the underlying value as type list. The returned reference may
    /// become invalid if the value is owned by another object or if ownership is
    /// transferred to another object in the future. To maintain a reference to
    /// the value after assigning ownership to a dictionary or list pass this
    /// object to the set_value() function instead of passing the returned
    /// reference to set_list().
    /// </summary>
    function GetList: ICefListValue;
    /// <summary>
    /// Sets the underlying value as type null. Returns true (1) if the value was
    /// set successfully.
    /// </summary>
    function SetNull: Boolean;
    /// <summary>
    /// Sets the underlying value as type bool. Returns true (1) if the value was
    /// set successfully.
    /// </summary>
    function SetBool(value: boolean): Boolean;
    /// <summary>
    /// Sets the underlying value as type int. Returns true (1) if the value was
    /// set successfully.
    /// </summary>
    function SetInt(value: Integer): Boolean;
    /// <summary>
    /// Sets the underlying value as type double. Returns true (1) if the value
    /// was set successfully.
    /// </summary>
    function SetDouble(value: Double): Boolean;
    /// <summary>
    /// Sets the underlying value as type string. Returns true (1) if the value
    /// was set successfully.
    /// </summary>
    function SetString(const value: ustring): Boolean;
    /// <summary>
    /// Sets the underlying value as type binary. Returns true (1) if the value
    /// was set successfully. This object keeps a reference to |value| and
    /// ownership of the underlying data remains unchanged.
    /// </summary>
    function SetBinary(const value: ICefBinaryValue): Boolean;
    /// <summary>
    /// Sets the underlying value as type dict. Returns true (1) if the value was
    /// set successfully. This object keeps a reference to |value| and ownership
    /// of the underlying data remains unchanged.
    /// </summary>
    function SetDictionary(const value: ICefDictionaryValue): Boolean;
    /// <summary>
    /// Sets the underlying value as type list. Returns true (1) if the value was
    /// set successfully. This object keeps a reference to |value| and ownership
    /// of the underlying data remains unchanged.
    /// </summary>
    function SetList(const value: ICefListValue): Boolean;
  end;

  /// <summary>
  /// Interface representing a binary value. Can be used on any process and
  /// thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefBinaryValue">Implements TCefBinaryValue</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_values_capi.h">CEF source file: /include/capi/cef_values_capi.h (cef_binary_value_t)</see></para>
  /// </remarks>
  ICefBinaryValue = interface(ICefBaseRefCounted)
    ['{974AA40A-9C5C-4726-81F0-9F0D46D7C5B3}']
    /// <summary>
    /// Returns true (1) if this object is valid. This object may become invalid
    /// if the underlying data is owned by another object (e.g. list or
    /// dictionary) and that other object is then modified or destroyed. Do not
    /// call any other functions if this function returns false (0).
    /// </summary>
    function IsValid: Boolean;
    /// <summary>
    /// Returns true (1) if this object is currently owned by another object.
    /// </summary>
    function IsOwned: Boolean;
    /// <summary>
    /// Returns true (1) if this object and |that| object have the same underlying
    /// data.
    /// </summary>
    function IsSame(const that: ICefBinaryValue): Boolean;
    /// <summary>
    /// Returns true (1) if this object and |that| object have an equivalent
    /// underlying value but are not necessarily the same object.
    /// </summary>
    function IsEqual(const that: ICefBinaryValue): Boolean;
    /// <summary>
    /// Returns a copy of this object. The data in this object will also be
    /// copied.
    /// </summary>
    function Copy: ICefBinaryValue;
    /// <summary>
    /// Returns a pointer to the beginning of the memory block. The returned
    /// pointer is valid as long as the ICefBinaryValue is alive.
    /// </summary>
    function GetRawData: Pointer;
    /// <summary>
    /// Returns the data size.
    /// </summary>
    function GetSize: NativeUInt;
    /// <summary>
    /// Read up to |buffer_size| number of bytes into |buffer|. Reading begins at
    /// the specified byte |data_offset|. Returns the number of bytes read.
    /// </summary>
    function GetData(buffer: Pointer; bufferSize, dataOffset: NativeUInt): NativeUInt;
    /// <summary>
    /// Returns the data size.
    /// </summary>
    property Size  : NativeUInt   read GetSize;
  end;

  /// <summary>
  /// Interface representing a dictionary value. Can be used on any process and
  /// thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefDictionaryValue">Implements TCefDictionaryValue</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_values_capi.h">CEF source file: /include/capi/cef_values_capi.h (cef_dictionary_value_t)</see></para>
  /// </remarks>
  ICefDictionaryValue = interface(ICefBaseRefCounted)
    ['{B9638559-54DC-498C-8185-233EEF12BC69}']
    /// <summary>
    /// Returns true (1) if this object is valid. This object may become invalid
    /// if the underlying data is owned by another object (e.g. list or
    /// dictionary) and that other object is then modified or destroyed. Do not
    /// call any other functions if this function returns false (0).
    /// </summary>
    function IsValid: Boolean;
    /// <summary>
    /// Returns true (1) if this object is currently owned by another object.
    /// </summary>
    function isOwned: Boolean;
    /// <summary>
    /// Returns true (1) if the values of this object are read-only. Some APIs may
    /// expose read-only objects.
    /// </summary>
    function IsReadOnly: Boolean;
    /// <summary>
    /// Returns true (1) if this object and |that| object have the same underlying
    /// data. If true (1) modifications to this object will also affect |that|
    /// object and vice-versa.
    /// </summary>
    function IsSame(const that: ICefDictionaryValue): Boolean;
    /// <summary>
    /// Returns true (1) if this object and |that| object have an equivalent
    /// underlying value but are not necessarily the same object.
    /// </summary>
    function IsEqual(const that: ICefDictionaryValue): Boolean;
    /// <summary>
    /// Returns a writable copy of this object. If |exclude_NULL_children| is true
    /// (1) any NULL dictionaries or lists will be excluded from the copy.
    /// </summary>
    function Copy(excludeEmptyChildren: Boolean): ICefDictionaryValue;
    /// <summary>
    /// Returns the number of values.
    /// </summary>
    function GetSize: NativeUInt;
    /// <summary>
    /// Removes all values. Returns true (1) on success.
    /// </summary>
    function Clear: Boolean;
    /// <summary>
    /// Returns true (1) if the current dictionary has a value for the given key.
    /// </summary>
    function HasKey(const key: ustring): Boolean;
    /// <summary>
    /// Reads all keys for this dictionary into the specified vector.
    /// </summary>
    function GetKeys(const keys: TStrings): Boolean;
    /// <summary>
    /// Removes the value at the specified key. Returns true (1) is the value was
    /// removed successfully.
    /// </summary>
    function Remove(const key: ustring): Boolean;
    /// <summary>
    /// Returns the value type for the specified key.
    /// </summary>
    function GetType(const key: ustring): TCefValueType;
    /// <summary>
    /// Returns the value at the specified key. For simple types the returned
    /// value will copy existing data and modifications to the value will not
    /// modify this object. For complex types (binary, dictionary and list) the
    /// returned value will reference existing data and modifications to the value
    /// will modify this object.
    /// </summary>
    function GetValue(const key: ustring): ICefValue;
    /// <summary>
    /// Returns the value at the specified key as type bool.
    /// </summary>
    function GetBool(const key: ustring): Boolean;
    /// <summary>
    /// Returns the value at the specified key as type int.
    /// </summary>
    function GetInt(const key: ustring): Integer;
    /// <summary>
    /// Returns the value at the specified key as type double.
    /// </summary>
    function GetDouble(const key: ustring): Double;
    /// <summary>
    /// Returns the value at the specified key as type string.
    /// </summary>
    function GetString(const key: ustring): ustring;
    /// <summary>
    /// Returns the value at the specified key as type binary. The returned value
    /// will reference existing data.
    /// </summary>
    function GetBinary(const key: ustring): ICefBinaryValue;
    /// <summary>
    /// Returns the value at the specified key as type dictionary. The returned
    /// value will reference existing data and modifications to the value will
    /// modify this object.
    /// </summary>
    function GetDictionary(const key: ustring): ICefDictionaryValue;
    /// <summary>
    /// Returns the value at the specified key as type list. The returned value
    /// will reference existing data and modifications to the value will modify
    /// this object.
    /// </summary>
    function GetList(const key: ustring): ICefListValue;
    /// <summary>
    /// Sets the value at the specified key. Returns true (1) if the value was set
    /// successfully. If |value| represents simple data then the underlying data
    /// will be copied and modifications to |value| will not modify this object.
    /// If |value| represents complex data (binary, dictionary or list) then the
    /// underlying data will be referenced and modifications to |value| will
    /// modify this object.
    /// </summary>
    function SetValue(const key: ustring; const value: ICefValue): Boolean;
    /// <summary>
    /// Sets the value at the specified key as type null. Returns true (1) if the
    /// value was set successfully.
    /// </summary>
    function SetNull(const key: ustring): Boolean;
    /// <summary>
    /// Sets the value at the specified key as type bool. Returns true (1) if the
    /// value was set successfully.
    /// </summary>
    function SetBool(const key: ustring; value: Boolean): Boolean;
    /// <summary>
    /// Sets the value at the specified key as type int. Returns true (1) if the
    /// value was set successfully.
    /// </summary>
    function SetInt(const key: ustring; value: Integer): Boolean;
    /// <summary>
    /// Sets the value at the specified key as type double. Returns true (1) if
    /// the value was set successfully.
    /// </summary>
    function SetDouble(const key: ustring; value: Double): Boolean;
    /// <summary>
    /// Sets the value at the specified key as type string. Returns true (1) if
    /// the value was set successfully.
    /// </summary>
    function SetString(const key, value: ustring): Boolean;
    /// <summary>
    /// Sets the value at the specified key as type binary. Returns true (1) if
    /// the value was set successfully. If |value| is currently owned by another
    /// object then the value will be copied and the |value| reference will not
    /// change. Otherwise, ownership will be transferred to this object and the
    /// |value| reference will be invalidated.
    /// </summary>
    function SetBinary(const key: ustring; const value: ICefBinaryValue): Boolean;
    /// <summary>
    /// Sets the value at the specified key as type dict. Returns true (1) if the
    /// value was set successfully. If |value| is currently owned by another
    /// object then the value will be copied and the |value| reference will not
    /// change. Otherwise, ownership will be transferred to this object and the
    /// |value| reference will be invalidated.
    /// </summary>
    function SetDictionary(const key: ustring; const value: ICefDictionaryValue): Boolean;
    /// <summary>
    /// Sets the value at the specified key as type list. Returns true (1) if the
    /// value was set successfully. If |value| is currently owned by another
    /// object then the value will be copied and the |value| reference will not
    /// change. Otherwise, ownership will be transferred to this object and the
    /// |value| reference will be invalidated.
    /// </summary>
    function SetList(const key: ustring; const value: ICefListValue): Boolean;
  end;

  /// <summary>
  /// Interface representing a list value. Can be used on any process and thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefListValue">Implements TCefListValue</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_values_capi.h">CEF source file: /include/capi/cef_values_capi.h (cef_list_value_t)</see></para>
  /// </remarks>
  ICefListValue = interface(ICefBaseRefCounted)
    ['{09174B9D-0CC6-4360-BBB0-3CC0117F70F6}']
    /// <summary>
    /// Returns true (1) if this object is valid. This object may become invalid
    /// if the underlying data is owned by another object (e.g. list or
    /// dictionary) and that other object is then modified or destroyed. Do not
    /// call any other functions if this function returns false (0).
    /// </summary>
    function IsValid: Boolean;
    /// <summary>
    /// Returns true (1) if this object is currently owned by another object.
    /// </summary>
    function IsOwned: Boolean;
    /// <summary>
    /// Returns true (1) if the values of this object are read-only. Some APIs may
    /// expose read-only objects.
    /// </summary>
    function IsReadOnly: Boolean;
    /// <summary>
    /// Returns true (1) if this object and |that| object have the same underlying
    /// data. If true (1) modifications to this object will also affect |that|
    /// object and vice-versa.
    /// </summary>
    function IsSame(const that: ICefListValue): Boolean;
    /// <summary>
    /// Returns true (1) if this object and |that| object have an equivalent
    /// underlying value but are not necessarily the same object.
    /// </summary>
    function IsEqual(const that: ICefListValue): Boolean;
    /// <summary>
    /// Returns a writable copy of this object.
    /// </summary>
    function Copy: ICefListValue;
    /// <summary>
    /// Sets the number of values. If the number of values is expanded all new
    /// value slots will default to type null. Returns true (1) on success.
    /// </summary>
    function SetSize(size: NativeUInt): Boolean;
    /// <summary>
    /// Returns the number of values.
    /// </summary>
    function GetSize: NativeUInt;
    /// <summary>
    /// Removes all values. Returns true (1) on success.
    /// </summary>
    function Clear: Boolean;
    /// <summary>
    /// Removes the value at the specified index.
    /// </summary>
    function Remove(index: NativeUInt): Boolean;
    /// <summary>
    /// Returns the value type at the specified index.
    /// </summary>
    function GetType(index: NativeUInt): TCefValueType;
    /// <summary>
    /// Returns the value at the specified index. For simple types the returned
    /// value will copy existing data and modifications to the value will not
    /// modify this object. For complex types (binary, dictionary and list) the
    /// returned value will reference existing data and modifications to the value
    /// will modify this object.
    /// </summary>
    function GetValue(index: NativeUInt): ICefValue;
    /// <summary>
    /// Returns the value at the specified index as type bool.
    /// </summary>
    function GetBool(index: NativeUInt): Boolean;
    /// <summary>
    /// Returns the value at the specified index as type int.
    /// </summary>
    function GetInt(index: NativeUInt): Integer;
    /// <summary>
    /// Returns the value at the specified index as type double.
    /// </summary>
    function GetDouble(index: NativeUInt): Double;
    /// <summary>
    /// Returns the value at the specified index as type string.
    /// </summary>
    function GetString(index: NativeUInt): ustring;
    /// <summary>
    /// Returns the value at the specified index as type binary. The returned
    /// value will reference existing data.
    /// </summary>
    function GetBinary(index: NativeUInt): ICefBinaryValue;
    /// <summary>
    /// Returns the value at the specified index as type dictionary. The returned
    /// value will reference existing data and modifications to the value will
    /// modify this object.
    /// </summary>
    function GetDictionary(index: NativeUInt): ICefDictionaryValue;
    /// <summary>
    /// Returns the value at the specified index as type list. The returned value
    /// will reference existing data and modifications to the value will modify
    /// this object.
    /// </summary>
    function GetList(index: NativeUInt): ICefListValue;
    /// <summary>
    /// Sets the value at the specified index. Returns true (1) if the value was
    /// set successfully. If |value| represents simple data then the underlying
    /// data will be copied and modifications to |value| will not modify this
    /// object. If |value| represents complex data (binary, dictionary or list)
    /// then the underlying data will be referenced and modifications to |value|
    /// will modify this object.
    /// </summary>
    function SetValue(index: NativeUInt; const value: ICefValue): Boolean;
    /// <summary>
    /// Sets the value at the specified index as type null. Returns true (1) if
    /// the value was set successfully.
    /// </summary>
    function SetNull(index: NativeUInt): Boolean;
    /// <summary>
    /// Sets the value at the specified index as type bool. Returns true (1) if
    /// the value was set successfully.
    /// </summary>
    function SetBool(index: NativeUInt; value: Boolean): Boolean;
    /// <summary>
    /// Sets the value at the specified index as type int. Returns true (1) if the
    /// value was set successfully.
    /// </summary>
    function SetInt(index: NativeUInt; value: Integer): Boolean;
    /// <summary>
    /// Sets the value at the specified index as type double. Returns true (1) if
    /// the value was set successfully.
    /// </summary>
    function SetDouble(index: NativeUInt; value: Double): Boolean;
    /// <summary>
    /// Sets the value at the specified index as type string. Returns true (1) if
    /// the value was set successfully.
    /// </summary>
    function SetString(index: NativeUInt; const value: ustring): Boolean;
    /// <summary>
    /// Sets the value at the specified index as type binary. Returns true (1) if
    /// the value was set successfully. If |value| is currently owned by another
    /// object then the value will be copied and the |value| reference will not
    /// change. Otherwise, ownership will be transferred to this object and the
    /// |value| reference will be invalidated.
    /// </summary>
    function SetBinary(index: NativeUInt; const value: ICefBinaryValue): Boolean;
    /// <summary>
    /// Sets the value at the specified index as type dict. Returns true (1) if
    /// the value was set successfully. If |value| is currently owned by another
    /// object then the value will be copied and the |value| reference will not
    /// change. Otherwise, ownership will be transferred to this object and the
    /// |value| reference will be invalidated.
    /// </summary>
    function SetDictionary(index: NativeUInt; const value: ICefDictionaryValue): Boolean;
    /// <summary>
    /// Sets the value at the specified index as type list. Returns true (1) if
    /// the value was set successfully. If |value| is currently owned by another
    /// object then the value will be copied and the |value| reference will not
    /// change. Otherwise, ownership will be transferred to this object and the
    /// |value| reference will be invalidated.
    /// </summary>
    function SetList(index: NativeUInt; const value: ICefListValue): Boolean;
  end;

  /// <summary>
  /// Implement this interface to handle events related to browser life span. The
  /// functions of this interface will be called on the UI thread unless otherwise
  /// indicated.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefLifeSpanHandler">Implements TCefLifeSpanHandler</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_life_span_handler_capi.h">CEF source file: /include/capi/cef_life_span_handler_capi.h (cef_life_span_handler_t)</see></para>
  /// </remarks>
  ICefLifeSpanHandler = interface(ICefBaseRefCounted)
    ['{0A3EB782-A319-4C35-9B46-09B2834D7169}']
    /// <summary>
    /// Called on the UI thread before a new popup browser is created. The
    /// |browser| and |frame| values represent the source of the popup request.
    /// The |target_url| and |target_frame_name| values indicate where the popup
    /// browser should navigate and may be NULL if not specified with the request.
    /// The |target_disposition| value indicates where the user intended to open
    /// the popup (e.g. current tab, new tab, etc). The |user_gesture| value will
    /// be true (1) if the popup was opened via explicit user gesture (e.g.
    /// clicking a link) or false (0) if the popup opened automatically (e.g. via
    /// the DomContentLoaded event). The |popupFeatures| structure contains
    /// additional information about the requested popup window. To allow creation
    /// of the popup browser optionally modify |windowInfo|, |client|, |settings|
    /// and |no_javascript_access| and return false (0). To cancel creation of the
    /// popup browser return true (1). The |client| and |settings| values will
    /// default to the source browser's values. If the |no_javascript_access|
    /// value is set to false (0) the new browser will not be scriptable and may
    /// not be hosted in the same renderer process as the source browser. Any
    /// modifications to |windowInfo| will be ignored if the parent browser is
    /// wrapped in a ICefBrowserView. Popup browser creation will be canceled
    /// if the parent browser is destroyed before the popup browser creation
    /// completes (indicated by a call to OnAfterCreated for the popup browser).
    /// The |extra_info| parameter provides an opportunity to specify extra
    /// information specific to the created popup browser that will be passed to
    /// ICefRenderProcessHandler.OnBrowserCreated in the render process.
    /// </summary>
    function  OnBeforePopup(const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean): Boolean;
    /// <summary>
    /// <para>Called on the UI thread before a new DevTools popup browser is created.
    /// The |browser| value represents the source of the popup request. Optionally
    /// modify |windowInfo|, |client|, |settings| and |extra_info| values. The
    /// |client|, |settings| and |extra_info| values will default to the source
    /// browser's values. Any modifications to |windowInfo| will be ignored if the
    /// parent browser is Views-hosted (wrapped in a ICefBrowserView).</para>
    /// <para>The |extra_info| parameter provides an opportunity to specify extra
    /// information specific to the created popup browser that will be passed to
    /// ICefRenderProcessHandler.OnBrowserCreated() in the render process.
    /// The existing |extra_info| object, if any, will be read-only but may be
    /// replaced with a new object.</para>
    /// <para>Views-hosted source browsers will create Views-hosted DevTools popups
    /// unless |use_default_window| is set to to true (1). DevTools popups can be
    /// blocked by returning true (1) from ICefCommandHandler.OnChromeCommand
    /// for IDC_DEV_TOOLS. Only used with Chrome style.</para>
    /// </summary>
    procedure OnBeforeDevToolsPopup(const browser: ICefBrowser; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var use_default_window: boolean);
    /// <summary>
    /// Called after a new browser is created. It is now safe to begin performing
    /// actions with |browser|. ICefFrameHandler callbacks related to initial
    /// main frame creation will arrive before this callback. See
    /// ICefFrameHandler documentation for additional usage information.
    /// </summary>
    procedure OnAfterCreated(const browser: ICefBrowser);
    /// <summary>
    /// <para>Called when an Alloy style browser is ready to be closed, meaning that the
    /// close has already been initiated and that JavaScript unload handlers have
    /// already executed or should be ignored. This may result directly from a
    /// call to TChromiumCore.[Try]CloseBrowser() or indirectly if the
    /// browser's top-level parent window was created by CEF and the user attempts
    /// to close that window (by clicking the 'X', for example). TChromiumCore.OnClose will
    /// not be called if the browser's host window/view has already been destroyed
    /// (via parent window/view hierarchy tear-down, for example), as it is no
    /// longer possible to customize the close behavior at that point.</para>
    ///
    /// <para>An application should handle top-level parent window close notifications
    /// by calling TChromiumCore.TryCloseBrowser() or
    /// TChromiumCore.CloseBrowser(false) instead of allowing the window
    /// to close immediately (see the examples below). This gives CEF an
    /// opportunity to process JavaScript unload handlers and optionally cancel
    /// the close before TChromiumCore.OnClose is called.</para>
    ///
    /// <para>When windowed rendering is enabled CEF will create an internal child
    /// window/view to host the browser. In that case returning false (0) from
    /// TChromiumCore.OnClose will send the standard close notification to the browser's top-
    /// level parent window (e.g. WM_CLOSE on Windows, performClose: on OS X,
    /// "delete_event" on Linux or TCEFWindowComponent.OnCanClose callback
    /// from Views).</para>
    ///
    /// <para>When windowed rendering is disabled there is no internal window/view and
    /// returning false (0) from TChromiumCore.OnClose will cause the browser object to be
    /// destroyed immediately.</para>
    ///
    /// <para>If the browser's top-level parent window requires a non-standard close
    /// notification then send that notification from TChromiumCore.OnClose and return true
    /// (1). You are still required to complete the browser close as soon as
    /// possible (either by calling TChromiumCore.[Try]CloseBrowser() or by proceeding with
    /// window/view hierarchy tear-down), otherwise the browser will be left in a
    /// partially closed state that interferes with proper functioning. Top-level
    /// windows created on the browser process UI thread can alternately call
    /// TChromiumCore.IsReadyToBeClosed() in the close handler to check
    /// close status instead of relying on custom TChromiumCore.OnClose handling. See
    /// documentation on that function for additional details.</para>
    ///
    /// <para>The TChromiumCore.OnBeforeClose event will be called
    /// after TChromiumCore.OnClose (if TChromiumCore.OnClose is called) and immediately before the
    /// browser object is destroyed. The application should only exit after
    /// TChromiumCore.OnBeforeClose has been called for all existing browsers.</para>
    ///
    /// <para>The below examples describe what should happen during window close when
    /// the browser is parented to an application-provided top-level window.</para>
    ///
    /// <para>Example 1: Using TChromiumCore.TryCloseBrowser(). This is
    /// recommended for clients using standard close handling and windows created
    /// on the browser process UI thread.</para>
    /// <code>
    /// 1.  User clicks the window close button which sends a close notification
    ///     to the application's top-level window.
    /// 2.  Application's top-level window receives the close notification and
    ///     calls TChromiumCore.TryCloseBrowser() (similar to calling TChromiumCore.CloseBrowser(false)).
    ///     TChromiumCore.TryCloseBrowser() returns false so the client cancels the window
    ///     close.
    /// 3.  JavaScript 'onbeforeunload' handler executes and shows the close
    ///     confirmation dialog (which can be overridden via TChromiumCore.OnBeforeUnloadDialog).
    /// 4.  User approves the close.
    /// 5.  JavaScript 'onunload' handler executes.
    /// 6.  Application's TChromiumCore.OnClose handler is called and returns false (0) by
    ///     default.
    /// 7.  CEF sends a close notification to the application's top-level window
    ///     (because TChromiumCore.OnClose returned false).
    /// 8.  Application's top-level window receives the close notification and
    ///     calls TryCloseBrowser(). TryCloseBrowser() returns true so the client
    ///     allows the window close.
    /// 9.  Application's top-level window is destroyed, triggering destruction
    ///     of the child browser window.
    /// 10. Application's TChromiumCore.OnBeforeClose handler is called and the browser object
    ///     is destroyed.
    /// 11. Application exits by calling TCefApplicationCore.QuitMessageLoop if no other browsers
    ///     exist.
    /// </code>
    ///
    /// <para>Example 2: Using TChromiumCore.CloseBrowser(false) and
    /// implementing the TChromiumCore.OnClose event. This is recommended for clients
    /// using non-standard close handling or windows that were not created on the
    /// browser process UI thread.</para>
    /// <code>
    /// 1.  User clicks the window close button which sends a close notification
    ///     to the application's top-level window.
    /// 2.  Application's top-level window receives the close notification and:
    ///     A. Calls ICefBrowserHost.CloseBrowser(false).
    ///     B. Cancels the window close.
    /// 3.  JavaScript 'onbeforeunload' handler executes and shows the close
    ///     confirmation dialog (which can be overridden via TChromiumCore.OnBeforeUnloadDialog).
    /// 4.  User approves the close.
    /// 5.  JavaScript 'onunload' handler executes.
    /// 6.  Application's TChromiumCore.OnClose handler is called. Application will:
    ///     A. Set a flag to indicate that the next top-level window close attempt
    ///        will be allowed.
    ///     B. Return false.
    /// 7.  CEF sends a close notification to the application's top-level window
    ///     (because TChromiumCore.OnClose returned false).
    /// 8.  Application's top-level window receives the close notification and
    ///     allows the window to close based on the flag from #6A.
    /// 9.  Application's top-level window is destroyed, triggering destruction
    ///     of the child browser window.
    /// 10. Application's TChromiumCore.OnBeforeClose handler is called and the browser object
    ///     is destroyed.
    /// 11. Application exits by calling TCefApplicationCore.QuitMessageLoop if no other browsers exist.
    /// </code>
    /// </summary>
    function  DoClose(const browser: ICefBrowser): Boolean;
    /// <summary>
    /// Called just before a browser is destroyed. Release all references to the
    /// browser object and do not attempt to execute any functions on the browser
    /// object (other than IsValid, GetIdentifier or IsSame) after this callback
    /// returns. ICefFrameHandler callbacks related to final main frame
    /// destruction will arrive after this callback and ICefBrowser.IsValid
    /// will return false (0) at that time. Any in-progress network requests
    /// associated with |browser| will be aborted when the browser is destroyed,
    /// and ICefResourceRequestHandler callbacks related to those requests may
    /// still arrive on the IO thread after this callback. See ICefFrameHandler
    /// and DoClose() documentation for additional usage information.
    /// </summary>
    procedure OnBeforeClose(const browser: ICefBrowser);
    /// <summary>
    /// Custom procedure to clear all references.
    /// </summary>
    procedure RemoveReferences;
  end;

  /// <summary>
  /// Implement this interface to handle events related to commands. The functions
  /// of this interface will be called on the UI thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefCommandHandler">Implements TCefCommandHandler</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_command_handler_capi.h">CEF source file: /include/capi/cef_command_handler_capi.h (cef_command_handler_t)</see></para>
  /// </remarks>
  ICefCommandHandler = interface(ICefBaseRefCounted)
    ['{7C931B93-53DC-4607-AABB-2CB4AEF7FB96}']
    /// <summary>
    /// Called to execute a Chrome command triggered via menu selection or
    /// keyboard shortcut. Values for |command_id| can be found in the
    /// cef_command_ids.h file. |disposition| provides information about the
    /// intended command target. Return true (1) if the command was handled or
    /// false (0) for the default implementation. For context menu commands this
    /// will be called after ICefContextMenuHandler.OnContextMenuCommand.
    /// Only used with Chrome style.
    /// </summary>
    function  OnChromeCommand(const browser: ICefBrowser; command_id: integer; disposition: TCefWindowOpenDisposition): boolean;
    /// <summary>
    /// Called to check if a Chrome app menu item should be visible. Values for
    /// |command_id| can be found in the cef_command_ids.h file. Only called for
    /// menu items that would be visible by default. Only used with Chrome style.
    /// </summary>
    function  OnIsChromeAppMenuItemVisible(const browser: ICefBrowser; command_id: integer): boolean;
    /// <summary>
    /// Called to check if a Chrome app menu item should be enabled. Values for
    /// |command_id| can be found in the cef_command_ids.h file. Only called for
    /// menu items that would be enabled by default. Only used with Chrome style.
    /// </summary>
    function  OnIsChromeAppMenuItemEnabled(const browser: ICefBrowser; command_id: integer): boolean;
    /// <summary>
    /// Called during browser creation to check if a Chrome page action icon
    /// should be visible. Only called for icons that would be visible by default.
    /// Only used with Chrome style.
    /// </summary>
    function  OnIsChromePageActionIconVisible(icon_type: TCefChromePageActionIconType): boolean;
    /// <summary>
    /// Called during browser creation to check if a Chrome toolbar button should
    /// be visible. Only called for buttons that would be visible by default. Only
    /// used with Chrome style.
    /// </summary>
    function  OnIsChromeToolbarButtonVisible(button_type: TCefChromeToolbarButtonType): boolean;
    /// <summary>
    /// Custom procedure to clear all references.
    /// </summary>
    procedure RemoveReferences;
  end;

  /// <summary>
  /// Implement this interface to handle events related to browser load status.
  /// The functions of this interface will be called on the browser process UI
  /// thread or render process main thread (TID_RENDERER).
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefLoadHandler">Implements TCefLoadHandler</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_load_handler_capi.h">CEF source file: /include/capi/cef_load_handler_capi.h (cef_load_handler_t)</see></para>
  /// </remarks>
  ICefLoadHandler = interface(ICefBaseRefCounted)
    ['{2C63FB82-345D-4A5B-9858-5AE7A85C9F49}']
    /// <summary>
    /// Called when the loading state has changed. This callback will be executed
    /// twice -- once when loading is initiated either programmatically or by user
    /// action, and once when loading is terminated due to completion,
    /// cancellation of failure. It will be called before any calls to OnLoadStart
    /// and after all calls to OnLoadError and/or OnLoadEnd.
    /// </summary>
    procedure OnLoadingStateChange(const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
    /// <summary>
    /// Called after a navigation has been committed and before the browser begins
    /// loading contents in the frame. The |frame| value will never be NULL --
    /// call the IsMain() function to check if this frame is the main frame.
    /// |transition_type| provides information about the source of the navigation
    /// and an accurate value is only available in the browser process. Multiple
    /// frames may be loading at the same time. Sub-frames may start or continue
    /// loading after the main frame load has ended. This function will not be
    /// called for same page navigations (fragments, history state, etc.) or for
    /// navigations that fail or are canceled before commit. For notification of
    /// overall browser load status use OnLoadingStateChange instead.
    /// </summary>
    procedure OnLoadStart(const browser: ICefBrowser; const frame: ICefFrame; transitionType: TCefTransitionType);
    /// <summary>
    /// Called when the browser is done loading a frame. The |frame| value will
    /// never be NULL -- call the IsMain() function to check if this frame is the
    /// main frame. Multiple frames may be loading at the same time. Sub-frames
    /// may start or continue loading after the main frame load has ended. This
    /// function will not be called for same page navigations (fragments, history
    /// state, etc.) or for navigations that fail or are canceled before commit.
    /// For notification of overall browser load status use OnLoadingStateChange
    /// instead.
    /// </summary>
    procedure OnLoadEnd(const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: Integer);
    /// <summary>
    /// Called when a navigation fails or is canceled. This function may be called
    /// by itself if before commit or in combination with OnLoadStart/OnLoadEnd if
    /// after commit. |errorCode| is the error code number, |errorText| is the
    /// error text and |failedUrl| is the URL that failed to load. See
    /// net\base\net_error_list.h for complete descriptions of the error codes.
    /// </summary>
    procedure OnLoadError(const browser: ICefBrowser; const frame: ICefFrame; errorCode: TCefErrorCode; const errorText, failedUrl: ustring);
    /// <summary>
    /// Custom procedure to clear all references.
    /// </summary>
    procedure RemoveReferences;
  end;

  /// <summary>
  /// Implement this interface to filter resource response content. The functions
  /// of this interface will be called on the browser process IO thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefResponseFilter">Implements TCefResponseFilter</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_response_filter_capi.h">CEF source file: /include/capi/cef_response_filter_capi.h (cef_response_filter_t)</see></para>
  /// </remarks>
  ICefResponseFilter = interface(ICefBaseRefCounted)
    ['{5013BC3C-F1AE-407A-A571-A4C6B1D6831E}']
    /// <summary>
    /// Initialize the response filter. Will only be called a single time. The
    /// filter will not be installed if this function returns false (0).
    /// </summary>
    function InitFilter: Boolean;
    /// <summary>
    /// <para>Called to filter a chunk of data. Expected usage is as follows:</para>
    /// <code>
    ///  1. Read input data from |data_in| and set |data_in_read| to the number of
    ///     bytes that were read up to a maximum of |data_in_size|. |data_in| will
    ///     be NULL if |data_in_size| is zero.
    ///  2. Write filtered output data to |data_out| and set |data_out_written| to
    ///     the number of bytes that were written up to a maximum of
    ///     |data_out_size|. If no output data was written then all data must be
    ///     read from |data_in| (user must set |data_in_read| = |data_in_size|).
    ///  3. Return RESPONSE_FILTER_DONE if all output data was written or
    ///     RESPONSE_FILTER_NEED_MORE_DATA if output data is still pending.
    /// </code>
    /// <para>This function will be called repeatedly until the input buffer has been
    /// fully read (user sets |data_in_read| = |data_in_size|) and there is no
    /// more input data to filter (the resource response is complete). This
    /// function may then be called an additional time with an NULL input buffer
    /// if the user filled the output buffer (set |data_out_written| =
    /// |data_out_size|) and returned RESPONSE_FILTER_NEED_MORE_DATA to indicate
    /// that output data is still pending.</para>
    /// <para>Calls to this function will stop when one of the following conditions is
    /// met:</para>
    /// <code>
    ///  1. There is no more input data to filter (the resource response is
    ///     complete) and the user sets |data_out_written| = 0 or returns
    ///     RESPONSE_FILTER_DONE to indicate that all data has been written, or;
    ///  2. The user returns RESPONSE_FILTER_ERROR to indicate an error.
    /// </code>
    /// <para>Do not keep a reference to the buffers passed to this function.</para>
    /// </summary>
    function Filter(data_in: Pointer; data_in_size: NativeUInt; var data_in_read: NativeUInt; data_out: Pointer; data_out_size : NativeUInt; var data_out_written: NativeUInt): TCefResponseFilterStatus;
  end;

  /// <summary>
  /// Implement this interface to handle events related to browser requests. The
  /// functions of this interface will be called on the thread indicated.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefRequestHandler">Implements TCefRequestHandler</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_request_handler_capi.h">CEF source file: /include/capi/cef_request_handler_capi.h (cef_request_handler_t)</see></para>
  /// </remarks>
  ICefRequestHandler = interface(ICefBaseRefCounted)
    ['{050877A9-D1F8-4EB3-B58E-50DC3E3D39FD}']
    /// <summary>
    /// Called on the UI thread before browser navigation. Return true (1) to
    /// cancel the navigation or false (0) to allow the navigation to proceed. The
    /// |request| object cannot be modified in this callback.
    /// ICefLoadHandler.OnLoadingStateChange will be called twice in all
    /// cases. If the navigation is allowed ICefLoadHandler.OnLoadStart and
    /// ICefLoadHandler.OnLoadEnd will be called. If the navigation is
    /// canceled ICefLoadHandler.OnLoadError will be called with an
    /// |errorCode| value of ERR_ABORTED. The |user_gesture| value will be true
    /// (1) if the browser navigated via explicit user gesture (e.g. clicking a
    /// link) or false (0) if it navigated automatically (e.g. via the
    /// DomContentLoaded event).
    /// </summary>
    function  OnBeforeBrowse(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; user_gesture, isRedirect: Boolean): Boolean;
    /// <summary>
    /// Called on the UI thread before OnBeforeBrowse in certain limited cases
    /// where navigating a new or different browser might be desirable. This
    /// includes user-initiated navigation that might open in a special way (e.g.
    /// links clicked via middle-click or ctrl + left-click) and certain types of
    /// cross-origin navigation initiated from the renderer process (e.g.
    /// navigating the top-level frame to/from a file URL). The |browser| and
    /// |frame| values represent the source of the navigation. The
    /// |target_disposition| value indicates where the user intended to navigate
    /// the browser based on standard Chromium behaviors (e.g. current tab, new
    /// tab, etc). The |user_gesture| value will be true (1) if the browser
    /// navigated via explicit user gesture (e.g. clicking a link) or false (0) if
    /// it navigated automatically (e.g. via the DomContentLoaded event). Return
    /// true (1) to cancel the navigation or false (0) to allow the navigation to
    /// proceed in the source browser's top-level frame.
    /// </summary>
    function  OnOpenUrlFromTab(const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean): Boolean;
    /// <summary>
    /// Called on the browser process IO thread before a resource request is
    /// initiated. The |browser| and |frame| values represent the source of the
    /// request. |request| represents the request contents and cannot be modified
    /// in this callback. |is_navigation| will be true (1) if the resource request
    /// is a navigation. |is_download| will be true (1) if the resource request is
    /// a download. |request_initiator| is the origin (scheme + domain) of the
    /// page that initiated the request. Set |disable_default_handling| to true
    /// (1) to disable default handling of the request, in which case it will need
    /// to be handled via ICefResourceRequestHandler.GetResourceHandler or it
    /// will be canceled. To allow the resource load to proceed with default
    /// handling return NULL. To specify a handler for the resource return a
    /// ICefResourceRequestHandler object. If this callback returns NULL the
    /// same function will be called on the associated
    /// ICefRequestContextHandler, if any.
    /// </summary>
    procedure GetResourceRequestHandler(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; is_navigation, is_download: boolean; const request_initiator: ustring; var disable_default_handling: boolean; var aResourceRequestHandler : ICefResourceRequestHandler);
    /// <summary>
    /// Called on the IO thread when the browser needs credentials from the user.
    /// |origin_url| is the origin making this authentication request. |isProxy|
    /// indicates whether the host is a proxy server. |host| contains the hostname
    /// and |port| contains the port number. |realm| is the realm of the challenge
    /// and may be NULL. |scheme| is the authentication scheme used, such as
    /// "basic" or "digest", and will be NULL if the source of the request is an
    /// FTP server. Return true (1) to continue the request and call
    /// ICefAuthCallback.cont() either in this function or at a later time
    /// when the authentication information is available. Return false (0) to
    /// cancel the request immediately.
    /// </summary>
    function  GetAuthCredentials(const browser: ICefBrowser; const originUrl: ustring; isProxy: Boolean; const host: ustring; port: Integer; const realm, scheme: ustring; const callback: ICefAuthCallback): Boolean;
    /// <summary>
    /// Called on the UI thread to handle requests for URLs with an invalid SSL
    /// certificate. Return true (1) and call ICefCallback functions either in
    /// this function or at a later time to continue or cancel the request. Return
    /// false (0) to cancel the request immediately. If
    /// TCefSettings.ignore_certificate_errors is set all invalid certificates
    /// will be accepted without calling this function.
    /// </summary>
    function  OnCertificateError(const browser: ICefBrowser; certError: TCefErrorcode; const requestUrl: ustring; const sslInfo: ICefSslInfo; const callback: ICefCallback): Boolean;
    /// <summary>
    /// Called on the UI thread when a client certificate is being requested for
    /// authentication. Return false (0) to use the default behavior and
    /// automatically select the first certificate available. Return true (1) and
    /// call ICefSelectClientCertificateCallback.Select either in this
    /// function or at a later time to select a certificate. Do not call Select or
    /// call it with NULL to continue without using any certificate. |isProxy|
    /// indicates whether the host is an HTTPS proxy or the origin server. |host|
    /// and |port| contains the hostname and port of the SSL server.
    /// |certificates| is the list of certificates to choose from; this list has
    /// already been pruned by Chromium so that it only contains certificates from
    /// issuers that the server trusts.
    /// </summary>
    function  OnSelectClientCertificate(const browser: ICefBrowser; isProxy: boolean; const host: ustring; port: integer; certificatesCount: NativeUInt; const certificates: TCefX509CertificateArray; const callback: ICefSelectClientCertificateCallback): boolean;
    /// <summary>
    /// Called on the browser process UI thread when the render view associated
    /// with |browser| is ready to receive/handle IPC messages in the render
    /// process.
    /// </summary>
    procedure OnRenderViewReady(const browser: ICefBrowser);
    /// <summary>
    /// Called on the browser process UI thread when the render process is
    /// unresponsive as indicated by a lack of input event processing for at least
    /// 15 seconds. Return false (0) for the default behavior which is an
    /// indefinite wait with Alloy style or display of the "Page unresponsive"
    /// dialog with Chrome style. Return true (1) and don't execute the callback
    /// for an indefinite wait without display of the Chrome style dialog. Return
    /// true (1) and call ICefUnresponsiveProcessCallback.Wait either in this
    /// function or at a later time to reset the wait timer, potentially
    /// triggering another call to this function if the process remains
    /// unresponsive. Return true (1) and call
    /// ICefUnresponsiveProcessCallback.Terminate either in this function or
    /// at a later time to terminate the unresponsive process, resulting in a call
    /// to OnRenderProcessTerminated. OnRenderProcessResponsive will be called if
    /// the process becomes responsive after this function is called. This
    /// functionality depends on the hang monitor which can be disabled by passing
    /// the `--disable-hang-monitor` command-line flag.
    /// </summary>
    function  OnRenderProcessUnresponsive(const browser: ICefBrowser; const callback: ICefUnresponsiveProcessCallback): boolean;
    /// <summary>
    /// Called on the browser process UI thread when the render process becomes
    /// responsive after previously being unresponsive. See documentation on
    /// OnRenderProcessUnresponsive.
    /// </summary>
    procedure OnRenderProcessResponsive(const browser: ICefBrowser);
    /// <summary>
    /// Called on the browser process UI thread when the render process terminates
    /// unexpectedly. |status| indicates how the process terminated. |error_code|
    /// and |error_string| represent the error that would be displayed in Chrome's
    /// "Aw, Snap!" view. Possible |error_code| values include TCefResultCode
    /// non-normal exit values and platform-specific crash values (for example, a
    /// Posix signal or Windows hardware exception).
    /// </summary>
    procedure OnRenderProcessTerminated(const browser: ICefBrowser; status: TCefTerminationStatus; error_code: integer; const error_string: ustring);
    /// <summary>
    /// Called on the browser process UI thread when the window.document object of
    /// the main frame has been created.
    /// </summary>
    procedure OnDocumentAvailableInMainFrame(const browser: ICefBrowser);
    /// <summary>
    /// Custom procedure to clear all references.
    /// </summary>
    procedure RemoveReferences;
  end;

  /// <summary>
  /// Implement this interface to handle events related to browser requests. The
  /// functions of this interface will be called on the IO thread unless otherwise
  /// indicated.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefResourceRequestHandler">Implements TCefResourceRequestHandler</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_resource_request_handler_capi.h">CEF source file: /include/capi/cef_resource_request_handler_capi.h (cef_resource_request_handler_t)</see></para>
  /// </remarks>
  ICefResourceRequestHandler = interface(ICefBaseRefCounted)
    ['{CFA42A38-EA91-4A95-95CE-178BCD412411}']
    /// <summary>
    /// Called on the IO thread before a resource request is loaded. The |browser|
    /// and |frame| values represent the source of the request, and may be NULL
    /// for requests originating from service workers or ICefUrlRequest. To
    /// optionally filter cookies for the request return a
    /// ICefCookieAccessFilter object. The |request| object cannot not be
    /// modified in this callback.
    /// </summary>
    procedure GetCookieAccessFilter(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; var aFilter: ICefCookieAccessFilter);
    /// <summary>
    /// Called on the IO thread before a resource request is loaded. The |browser|
    /// and |frame| values represent the source of the request, and may be NULL
    /// for requests originating from service workers or ICefUrlRequest. To
    /// redirect or change the resource load optionally modify |request|.
    /// Modification of the request URL will be treated as a redirect. Return
    /// RV_CONTINUE to continue the request immediately. Return RV_CONTINUE_ASYNC
    /// and call ICefCallback functions at a later time to continue or cancel
    /// the request asynchronously. Return RV_CANCEL to cancel the request
    /// immediately.
    /// </summary>
    function  OnBeforeResourceLoad(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const callback: ICefCallback): TCefReturnValue;
    /// <summary>
    /// Called on the IO thread before a resource is loaded. The |browser| and
    /// |frame| values represent the source of the request, and may be NULL for
    /// requests originating from service workers or ICefUrlRequest. To allow
    /// the resource to load using the default network loader return NULL. To
    /// specify a handler for the resource return a ICefResourceHandler object.
    /// The |request| object cannot not be modified in this callback.
    /// </summary>
    procedure GetResourceHandler(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; var aResourceHandler : ICefResourceHandler);
    /// <summary>
    /// Called on the IO thread when a resource load is redirected. The |browser|
    /// and |frame| values represent the source of the request, and may be NULL
    /// for requests originating from service workers or ICefUrlRequest. The
    /// |request| parameter will contain the old URL and other request-related
    /// information. The |response| parameter will contain the response that
    /// resulted in the redirect. The |new_url| parameter will contain the new URL
    /// and can be changed if desired. The |request| and |response| objects cannot
    /// be modified in this callback.
    /// </summary>
    procedure OnResourceRedirect(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; var newUrl: ustring);
    /// <summary>
    /// Called on the IO thread when a resource response is received. The
    /// |browser| and |frame| values represent the source of the request, and may
    /// be NULL for requests originating from service workers or ICefUrlRequest.
    /// To allow the resource load to proceed without modification return false
    /// (0). To redirect or retry the resource load optionally modify |request|
    /// and return true (1). Modification of the request URL will be treated as a
    /// redirect. Requests handled using the default network loader cannot be
    /// redirected in this callback. The |response| object cannot be modified in
    /// this callback.
    ///
    /// WARNING: Redirecting using this function is deprecated. Use
    /// OnBeforeResourceLoad or GetResourceHandler to perform redirects.
    /// </summary>
    function  OnResourceResponse(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse): Boolean;
    /// <summary>
    /// Called on the IO thread to optionally filter resource response content.
    /// The |browser| and |frame| values represent the source of the request, and
    /// may be NULL for requests originating from service workers or
    /// ICefUrlRequest. |request| and |response| represent the request and
    /// response respectively and cannot be modified in this callback.
    /// </summary>
    procedure GetResourceResponseFilter(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; var aResourceFilter: ICefResponseFilter);
    /// <summary>
    /// Called on the IO thread when a resource load has completed. The |browser|
    /// and |frame| values represent the source of the request, and may be NULL
    /// for requests originating from service workers or ICefUrlRequest.
    /// |request| and |response| represent the request and response respectively
    /// and cannot be modified in this callback. |status| indicates the load
    /// completion status. |received_content_length| is the number of response
    /// bytes actually read. This function will be called for all requests,
    /// including requests that are aborted due to CEF shutdown or destruction of
    /// the associated browser. In cases where the associated browser is destroyed
    /// this callback may arrive after the ICefLifeSpanHandler.OnBeforeClose
    /// callback for that browser. The ICefFrame.IsValid function can be used
    /// to test for this situation, and care should be taken not to call |browser|
    /// or |frame| functions that modify state (like LoadURL, SendProcessMessage,
    /// etc.) if the frame is invalid.
    /// </summary>
    procedure OnResourceLoadComplete(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; status: TCefUrlRequestStatus; receivedContentLength: Int64);
    /// <summary>
    /// Called on the IO thread to handle requests for URLs with an unknown
    /// protocol component. The |browser| and |frame| values represent the source
    /// of the request, and may be NULL for requests originating from service
    /// workers or ICefUrlRequest. |request| cannot be modified in this
    /// callback. Set |allow_os_execution| to true (1) to attempt execution via
    /// the registered OS protocol handler, if any. SECURITY WARNING: YOU SHOULD
    /// USE THIS METHOD TO ENFORCE RESTRICTIONS BASED ON SCHEME, HOST OR OTHER URL
    /// ANALYSIS BEFORE ALLOWING OS EXECUTION.
    /// </summary>
    procedure OnProtocolExecution(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; var allowOsExecution: Boolean);
    /// <summary>
    /// Custom procedure to clear all references.
    /// </summary>
    procedure RemoveReferences;
  end;

  /// <summary>
  /// Implement this interface to filter cookies that may be sent or received from
  /// resource requests. The functions of this interface will be called on the IO
  /// thread unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefCookieAccessFilter">Implements TCefCookieAccessFilter</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_resource_request_handler_capi.h">CEF source file: /include/capi/cef_resource_request_handler_capi.h (cef_cookie_access_filter_t)</see></para>
  /// </remarks>
  ICefCookieAccessFilter = interface(ICefBaseRefCounted)
    ['{65ECD862-F55F-46E4-8AC3-2AE90DCC86F5}']
    /// <summary>
    /// Called on the IO thread before a resource request is sent. The |browser|
    /// and |frame| values represent the source of the request, and may be NULL
    /// for requests originating from service workers or ICefUrlRequest.
    /// |request| cannot be modified in this callback. Return true (1) if the
    /// specified cookie can be sent with the request or false (0) otherwise.
    /// </summary>
    function CanSendCookie(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const cookie: PCefCookie): boolean;
    /// <summary>
    /// Called on the IO thread after a resource response is received. The
    /// |browser| and |frame| values represent the source of the request, and may
    /// be NULL for requests originating from service workers or ICefUrlRequest.
    /// |request| cannot be modified in this callback. Return true (1) if the
    /// specified cookie returned with the response can be saved or false (0)
    /// otherwise.
    /// </summary>
    function CanSaveCookie(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; const cookie: PCefCookie): boolean;
    /// <summary>
    /// Custom procedure to clear all references.
    /// </summary>
    procedure RemoveReferences;
  end;

  /// <summary>
  /// Implement this interface to handle events related to browser display state.
  /// The functions of this interface will be called on the UI thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefDisplayHandler">Implements TCefDisplayHandler</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_display_handler_capi.h">CEF source file: /include/capi/cef_display_handler_capi.h (cef_display_handler_t)</see></para>
  /// </remarks>
  ICefDisplayHandler = interface(ICefBaseRefCounted)
    ['{1EC7C76D-6969-41D1-B26D-079BCFF054C4}']
    /// <summary>
    /// Called when a frame's address has changed.
    /// </summary>
    procedure OnAddressChange(const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
    /// <summary>
    /// Called when the page title changes.
    /// </summary>
    procedure OnTitleChange(const browser: ICefBrowser; const title: ustring);
    /// <summary>
    /// Called when the page icon changes.
    /// </summary>
    procedure OnFaviconUrlChange(const browser: ICefBrowser; const icon_urls: TStrings);
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
    procedure OnFullScreenModeChange(const browser: ICefBrowser; fullscreen: Boolean);
    /// <summary>
    /// Called when the browser is about to display a tooltip. |text| contains the
    /// text that will be displayed in the tooltip. To handle the display of the
    /// tooltip yourself return true (1). Otherwise, you can optionally modify
    /// |text| and then return false (0) to allow the browser to display the
    /// tooltip. When window rendering is disabled the application is responsible
    /// for drawing tooltips and the return value is ignored.
    /// </summary>
    function  OnTooltip(const browser: ICefBrowser; var text: ustring): Boolean;
    /// <summary>
    /// Called when the browser receives a status message. |value| contains the
    /// text that will be displayed in the status message.
    /// </summary>
    procedure OnStatusMessage(const browser: ICefBrowser; const value: ustring);
    /// <summary>
    /// Called to display a console message. Return true (1) to stop the message
    /// from being output to the console.
    /// </summary>
    function  OnConsoleMessage(const browser: ICefBrowser; level: TCefLogSeverity; const message_, source: ustring; line: Integer): Boolean;
    /// <summary>
    /// Called when auto-resize is enabled via
    /// ICefBrowserHost.SetAutoResizeEnabled and the contents have auto-
    /// resized. |new_size| will be the desired size in view coordinates. Return
    /// true (1) if the resize was handled or false (0) for default handling.
    /// </summary>
    function  OnAutoResize(const browser: ICefBrowser; const new_size: PCefSize): Boolean;
    /// <summary>
    /// Called when the overall page loading progress has changed. |progress|
    /// ranges from 0.0 to 1.0.
    /// </summary>
    procedure OnLoadingProgressChange(const browser: ICefBrowser; const progress: double);
    /// <summary>
    /// Called when the browser's cursor has changed. If |type| is CT_CUSTOM then
    /// |custom_cursor_info| will be populated with the custom cursor information.
    /// Return true (1) if the cursor change was handled or false (0) for default
    /// handling.
    /// </summary>
    procedure OnCursorChange(const browser: ICefBrowser; cursor_: TCefCursorHandle; CursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo; var aResult : boolean);
    /// <summary>
    /// Called when the browser's access to an audio and/or video source has
    /// changed.
    /// </summary>
    procedure OnMediaAccessChange(const browser: ICefBrowser; has_video_access, has_audio_access: boolean);
    /// <summary>
    /// Custom procedure to clear all references.
    /// </summary>
    procedure RemoveReferences;
  end;

  /// <summary>
  /// Implement this interface to handle events related to focus. The functions of
  /// this interface will be called on the UI thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefFocusHandler">Implements TCefFocusHandler</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_focus_handler_capi.h">CEF source file: /include/capi/cef_focus_handler_capi.h (cef_focus_handler_t)</see></para>
  /// </remarks>
  ICefFocusHandler = interface(ICefBaseRefCounted)
    ['{BB7FA3FA-7B1A-4ADC-8E50-12A24018DD90}']
    /// <summary>
    /// Called when the browser component is about to loose focus. For instance,
    /// if focus was on the last HTML element and the user pressed the TAB key.
    /// |next| will be true (1) if the browser is giving focus to the next
    /// component and false (0) if the browser is giving focus to the previous
    /// component.
    /// </summary>
    procedure OnTakeFocus(const browser: ICefBrowser; next: Boolean);
    /// <summary>
    /// Called when the browser component is requesting focus. |source| indicates
    /// where the focus request is originating from. Return false (0) to allow the
    /// focus to be set or true (1) to cancel setting the focus.
    /// </summary>
    function  OnSetFocus(const browser: ICefBrowser; source: TCefFocusSource): Boolean;
    /// <summary>
    /// Called when the browser component has received focus.
    /// </summary>
    procedure OnGotFocus(const browser: ICefBrowser);
    /// <summary>
    /// Custom procedure to clear all references.
    /// </summary>
    procedure RemoveReferences;
  end;

  /// <summary>
  /// Implement this interface to handle events related to keyboard input. The
  /// functions of this interface will be called on the UI thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefKeyboardHandler">Implements TCefKeyboardHandler</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_keyboard_handler_capi.h">CEF source file: /include/capi/cef_keyboard_handler_capi.h (cef_keyboard_handler_t)</see></para>
  /// </remarks>
  ICefKeyboardHandler = interface(ICefBaseRefCounted)
    ['{0512F4EC-ED88-44C9-90D3-5C6D03D3B146}']
    /// <summary>
    /// Called before a keyboard event is sent to the renderer. |event| contains
    /// information about the keyboard event. |os_event| is the operating system
    /// event message, if any. Return true (1) if the event was handled or false
    /// (0) otherwise. If the event will be handled in on_key_event() as a
    /// keyboard shortcut set |is_keyboard_shortcut| to true (1) and return false
    /// (0).
    /// </summary>
    function OnPreKeyEvent(const browser: ICefBrowser; const event: PCefKeyEvent; osEvent: TCefEventHandle; out isKeyboardShortcut: Boolean): Boolean;
    /// <summary>
    /// Called after the renderer and JavaScript in the page has had a chance to
    /// handle the event. |event| contains information about the keyboard event.
    /// |os_event| is the operating system event message, if any. Return true (1)
    /// if the keyboard event was handled or false (0) otherwise.
    /// </summary>
    function OnKeyEvent(const browser: ICefBrowser; const event: PCefKeyEvent; osEvent: TCefEventHandle): Boolean;
    /// <summary>
    /// Custom procedure to clear all references.
    /// </summary>
    procedure RemoveReferences;
  end;

  /// <summary>
  /// Implement this interface to handle events related to JavaScript dialogs. The
  /// functions of this interface will be called on the UI thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefJsDialogHandler">Implements TCefJsDialogHandler</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_jsdialog_handler_capi.h">CEF source file: /include/capi/cef_jsdialog_handler_capi.h (cef_jsdialog_handler_t)</see></para>
  /// </remarks>
  ICefJsDialogHandler = interface(ICefBaseRefCounted)
    ['{64E18F86-DAC5-4ED1-8589-44DE45B9DB56}']
    /// <summary>
    /// Called to run a JavaScript dialog. If |origin_url| is non-NULL it can be
    /// passed to the CefFormatUrlForSecurityDisplay function to retrieve a secure
    /// and user-friendly display string. The |default_prompt_text| value will be
    /// specified for prompt dialogs only. Set |suppress_message| to true (1) and
    /// return false (0) to suppress the message (suppressing messages is
    /// preferable to immediately executing the callback as this is used to detect
    /// presumably malicious behavior like spamming alert messages in
    /// onbeforeunload). Set |suppress_message| to false (0) and return false (0)
    /// to use the default implementation (the default implementation will show
    /// one modal dialog at a time and suppress any additional dialog requests
    /// until the displayed dialog is dismissed). Return true (1) if the
    /// application will use a custom dialog or if the callback has been executed
    /// immediately. Custom dialogs may be either modal or modeless. If a custom
    /// dialog is used the application must execute |callback| once the custom
    /// dialog is dismissed.
    /// </summary>
    function  OnJsdialog(const browser: ICefBrowser; const originUrl: ustring; dialogType: TCefJsDialogType; const messageText, defaultPromptText: ustring; const callback: ICefJsDialogCallback; out suppressMessage: Boolean): Boolean;
    /// <summary>
    /// Called to run a dialog asking the user if they want to leave a page.
    /// Return false (0) to use the default dialog implementation. Return true (1)
    /// if the application will use a custom dialog or if the callback has been
    /// executed immediately. Custom dialogs may be either modal or modeless. If a
    /// custom dialog is used the application must execute |callback| once the
    /// custom dialog is dismissed.
    /// </summary>
    function  OnBeforeUnloadDialog(const browser: ICefBrowser; const messageText: ustring; isReload: Boolean; const callback: ICefJsDialogCallback): Boolean;
    /// <summary>
    /// Called to cancel any pending dialogs and reset any saved dialog state.
    /// Will be called due to events like page navigation irregardless of whether
    /// any dialogs are currently pending.
    /// </summary>
    procedure OnResetDialogState(const browser: ICefBrowser);
    /// <summary>
    /// Called when the dialog is closed.
    /// </summary>
    procedure OnDialogClosed(const browser: ICefBrowser);
    /// <summary>
    /// Custom procedure to clear all references.
    /// </summary>
    procedure RemoveReferences;
  end;

  /// <summary>
  /// Implement this interface to handle audio events.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefAudioHandler">Implements TCefAudioHandler</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_audio_handler_capi.h">CEF source file: /include/capi/cef_audio_handler_capi.h (cef_audio_handler_t)</see></para>
  /// </remarks>
  ICefAudioHandler = interface(ICefBaseRefCounted)
    ['{8963271A-0B94-4279-82C8-FB2EA7B3CDEC}']
    /// <summary>
    /// Called on the UI thread to allow configuration of audio stream parameters.
    /// Return true (1) to proceed with audio stream capture, or false (0) to
    /// cancel it. All members of |params| can optionally be configured here, but
    /// they are also pre-filled with some sensible defaults.
    /// </summary>
    procedure OnGetAudioParameters(const browser: ICefBrowser; var params: TCefAudioParameters; var aResult: boolean);
    /// <summary>
    /// Called on a browser audio capture thread when the browser starts streaming
    /// audio. OnAudioStreamStopped will always be called after
    /// OnAudioStreamStarted; both functions may be called multiple times for the
    /// same browser. |params| contains the audio parameters like sample rate and
    /// channel layout. |channels| is the number of channels.
    /// </summary>
    procedure OnAudioStreamStarted(const browser: ICefBrowser; const params: TCefAudioParameters; channels: integer);
    /// <summary>
    /// Called on the audio stream thread when a PCM packet is received for the
    /// stream. |data| is an array representing the raw PCM data as a floating
    /// point type, i.e. 4-byte value(s). |frames| is the number of frames in the
    /// PCM packet. |pts| is the presentation timestamp (in milliseconds since the
    /// Unix Epoch) and represents the time at which the decompressed packet
    /// should be presented to the user. Based on |frames| and the
    /// |channel_layout| value passed to OnAudioStreamStarted you can calculate
    /// the size of the |data| array in bytes.
    /// </summary>
    procedure OnAudioStreamPacket(const browser: ICefBrowser; const data : PPSingle; frames: integer; pts: int64);
    /// <summary>
    /// Called on the UI thread when the stream has stopped. OnAudioSteamStopped
    /// will always be called after OnAudioStreamStarted; both functions may be
    /// called multiple times for the same stream.
    /// </summary>
    procedure OnAudioStreamStopped(const browser: ICefBrowser);
    /// <summary>
    /// Called on the UI or audio stream thread when an error occurred. During the
    /// stream creation phase this callback will be called on the UI thread while
    /// in the capturing phase it will be called on the audio stream thread. The
    /// stream will be stopped immediately.
    /// </summary>
    procedure OnAudioStreamError(const browser: ICefBrowser; const message_: ustring);
    /// <summary>
    /// Custom procedure to clear all references.
    /// </summary>
    procedure RemoveReferences;
  end;

  /// <summary>
  /// Callback interface used for continuation of custom context menu display.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefRunContextMenuCallback">Implements TCefRunContextMenuCallback</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_context_menu_handler_capi.h">CEF source file: /include/capi/cef_context_menu_handler_capi.h (cef_run_context_menu_callback_t)</see></para>
  /// </remarks>
  ICefRunContextMenuCallback = interface(ICefBaseRefCounted)
    ['{44C3C6E3-B64D-4F6E-A318-4A0F3A72EB00}']
    /// <summary>
    /// Complete context menu display by selecting the specified |command_id| and
    /// |event_flags|.
    /// </summary>
    procedure Cont(commandId: Integer; eventFlags: TCefEventFlags);
    /// <summary>
    /// Cancel context menu display.
    /// </summary>
    procedure Cancel;
  end;

  /// <summary>
  /// Callback interface used for continuation of custom quick menu display.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefRunQuickMenuCallback">Implements TCefRunQuickMenuCallback</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_context_menu_handler_capi.h">CEF source file: /include/capi/cef_context_menu_handler_capi.h (cef_run_quick_menu_callback_t)</see></para>
  /// </remarks>
  ICefRunQuickMenuCallback = interface(ICefBaseRefCounted)
    ['{11AD68BF-0055-4106-8F6B-B576F90D812F}']
    /// <summary>
    /// Complete quick menu display by selecting the specified |command_id| and
    /// |event_flags|.
    /// </summary>
    procedure Cont(command_id: Integer; event_flags: TCefEventFlags);
    /// <summary>
    /// Cancel quick menu display.
    /// </summary>
    procedure Cancel;
  end;

  /// <summary>
  /// Implement this interface to handle context menu events. The functions of
  /// this interface will be called on the UI thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefContextMenuHandler">Implements TCefContextMenuHandler</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_context_menu_handler_capi.h">CEF source file: /include/capi/cef_context_menu_handler_capi.h (cef_context_menu_handler_t)</see></para>
  /// </remarks>
  ICefContextMenuHandler = interface(ICefBaseRefCounted)
    ['{C2951895-4087-49D5-BA18-4D9BA4F5EDD7}']
    /// <summary>
    /// Called before a context menu is displayed. |params| provides information
    /// about the context menu state. |model| initially contains the default
    /// context menu. The |model| can be cleared to show no context menu or
    /// modified to show a custom menu. Do not keep references to |params| or
    /// |model| outside of this callback.
    /// </summary>
    procedure OnBeforeContextMenu(const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; const model: ICefMenuModel);
    /// <summary>
    /// Called to allow custom display of the context menu. |params| provides
    /// information about the context menu state. |model| contains the context
    /// menu model resulting from OnBeforeContextMenu. For custom display return
    /// true (1) and execute |callback| either synchronously or asynchronously
    /// with the selected command ID. For default display return false (0). Do not
    /// keep references to |params| or |model| outside of this callback.
    /// </summary>
    function  RunContextMenu(const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; const model: ICefMenuModel; const callback: ICefRunContextMenuCallback): Boolean;
    /// <summary>
    /// Called to execute a command selected from the context menu. Return true
    /// (1) if the command was handled or false (0) for the default
    /// implementation. See TCefMenuId for the command ids that have default
    /// implementations. All user-defined command ids should be between
    /// MENU_ID_USER_FIRST and MENU_ID_USER_LAST. |params| will have the same
    /// values as what was passed to on_before_context_menu(). Do not keep a
    /// reference to |params| outside of this callback.
    /// </summary>
    function  OnContextMenuCommand(const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; commandId: Integer; eventFlags: TCefEventFlags): Boolean;
    /// <summary>
    /// Called when the context menu is dismissed irregardless of whether the menu
    /// was canceled or a command was selected.
    /// </summary>
    procedure OnContextMenuDismissed(const browser: ICefBrowser; const frame: ICefFrame);
    /// <summary>
    /// Called to allow custom display of the quick menu for a windowless browser.
    /// |location| is the top left corner of the selected region. |size| is the
    /// size of the selected region. |edit_state_flags| is a combination of flags
    /// that represent the state of the quick menu. Return true (1) if the menu
    /// will be handled and execute |callback| either synchronously or
    /// asynchronously with the selected command ID. Return false (0) to cancel
    /// the menu.
    /// </summary>
    function  RunQuickMenu(const browser: ICefBrowser; const frame: ICefFrame; location: PCefPoint; size: PCefSize; edit_state_flags: TCefQuickMenuEditStateFlags; const callback: ICefRunQuickMenuCallback): boolean;
    /// <summary>
    /// Called to execute a command selected from the quick menu for a windowless
    /// browser. Return true (1) if the command was handled or false (0) for the
    /// default implementation. See TCefMenuId for command IDs that have
    /// default implementations.
    /// </summary>
    function  OnQuickMenuCommand(const browser: ICefBrowser; const frame: ICefFrame; command_id: integer; event_flags: TCefEventFlags): boolean;
    /// <summary>
    /// Called when the quick menu for a windowless browser is dismissed
    /// irregardless of whether the menu was canceled or a command was selected.
    /// </summary>
    procedure OnQuickMenuDismissed(const browser: ICefBrowser; const frame: ICefFrame);
    /// <summary>
    /// Custom procedure to clear all references.
    /// </summary>
    procedure RemoveReferences;
  end;

  /// <summary>
  /// Implement this interface to receive accessibility notification when
  /// accessibility events have been registered. The functions of this interface
  /// will be called on the UI thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefAccessibilityHandler">Implements TCefAccessibilityHandler</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_accessibility_handler_capi.h">CEF source file: /include/capi/cef_accessibility_handler_capi.h (cef_accessibility_handler_t)</see></para>
  /// </remarks>
  ICefAccessibilityHandler = interface(ICefBaseRefCounted)
    ['{1878C3C7-7692-44AB-BFE0-6C387106816B}']
    /// <summary>
    /// Called after renderer process sends accessibility tree changes to the
    /// browser process.
    /// </summary>
    procedure OnAccessibilityTreeChange(const value: ICefValue);
    /// <summary>
    /// Called after renderer process sends accessibility location changes to the
    /// browser process.
    /// </summary>
    procedure OnAccessibilityLocationChange(const value: ICefValue);
  end;

  /// <summary>
  /// Implement this interface to handle dialog events. The functions of this
  /// interface will be called on the browser process UI thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefDialogHandler">Implements TCefDialogHandler</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_dialog_handler_capi.h">CEF source file: /include/capi/cef_dialog_handler_capi.h (cef_dialog_handler_t)</see></para>
  /// </remarks>
  ICefDialogHandler = interface(ICefBaseRefCounted)
    ['{7763F4B2-8BE1-4E80-AC43-8B825850DC67}']
    /// <summary>
    /// <para>Called to run a file chooser dialog. |mode| represents the type of dialog
    /// to display. |title| to the title to be used for the dialog and may be NULL
    /// to show the default title ("Open" or "Save" depending on the mode).</para>
    /// <para>|default_file_path| is the path with optional directory and/or file name
    /// component that should be initially selected in the dialog.</para>
    /// <para>|accept_filters| are used to restrict the selectable file types and may be
    /// any combination of valid lower-cased MIME types (e.g. "text/*" or
    /// "image/*") and individual file extensions (e.g. ".txt" or ".png").</para>
    /// <para>|accept_extensions| provides the semicolon-delimited expansion of MIME
    /// types to file extensions (if known, or NULL string otherwise).</para>
    /// <para>|accept_descriptions| provides the descriptions for MIME types (if known,
    /// or NULL string otherwise). For example, the "image/*" mime type might have
    /// extensions ".png;.jpg;.bmp;..." and description "Image Files".</para>
    /// <para>|accept_filters|, |accept_extensions| and |accept_descriptions| will all
    /// be the same size. To display a custom dialog return true (1) and execute
    /// |callback| either inline or at a later time. To display the default dialog
    /// return false (0). If this function returns false (0) it may be called an
    /// additional time for the same dialog (both before and after MIME type
    /// expansion).</para>
    /// </summary>
    function OnFileDialog(const browser: ICefBrowser; mode: TCefFileDialogMode; const title, defaultFilePath: ustring; const acceptFilters, accept_extensions, accept_descriptions: TStrings; const callback: ICefFileDialogCallback): Boolean;
    /// <summary>
    /// Custom procedure to clear all references.
    /// </summary>
    procedure RemoveReferences;
  end;

  /// <summary>
  /// Implement this interface to handle events when window rendering is disabled.
  /// The functions of this interface will be called on the UI thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefRenderHandler">Implements TCefRenderHandler</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_render_handler_capi.h">CEF source file: /include/capi/cef_render_handler_capi.h (cef_render_handler_t)</see></para>
  /// </remarks>
  ICefRenderHandler = interface(ICefBaseRefCounted)
    ['{1FC1C22B-085A-4741-9366-5249B88EC410}']
    /// <summary>
    /// Return the handler for accessibility notifications. If no handler is
    /// provided the default implementation will be used.
    /// </summary>
    procedure GetAccessibilityHandler(var aAccessibilityHandler : ICefAccessibilityHandler);
    /// <summary>
    /// Called to retrieve the root window rectangle in screen DIP coordinates.
    /// Return true (1) if the rectangle was provided. If this function returns
    /// false (0) the rectangle from GetViewRect will be used.
    /// </summary>
    function  GetRootScreenRect(const browser: ICefBrowser; var rect: TCefRect): Boolean;
    /// <summary>
    /// Called to retrieve the view rectangle in screen DIP coordinates. This
    /// function must always provide a non-NULL rectangle.
    /// </summary>
    procedure GetViewRect(const browser: ICefBrowser; var rect: TCefRect);
    /// <summary>
    /// Called to retrieve the translation from view DIP coordinates to screen
    /// coordinates. Windows/Linux should provide screen device (pixel)
    /// coordinates and MacOS should provide screen DIP coordinates. Return true
    /// (1) if the requested coordinates were provided.
    /// </summary>
    function  GetScreenPoint(const browser: ICefBrowser; viewX, viewY: Integer; var screenX, screenY: Integer): Boolean;
    /// <summary>
    /// Called to allow the client to fill in the CefScreenInfo object with
    /// appropriate values. Return true (1) if the |screen_info| structure has
    /// been modified.
    ///
    /// If the screen info rectangle is left NULL the rectangle from GetViewRect
    /// will be used. If the rectangle is still NULL or invalid popups may not be
    /// drawn correctly.
    /// </summary>
    function  GetScreenInfo(const browser: ICefBrowser; var screenInfo: TCefScreenInfo): Boolean;
    /// <summary>
    /// Called when the browser wants to show or hide the popup widget. The popup
    /// should be shown if |show| is true (1) and hidden if |show| is false (0).
    /// </summary>
    procedure OnPopupShow(const browser: ICefBrowser; show: Boolean);
    /// <summary>
    /// Called when the browser wants to move or resize the popup widget. |rect|
    /// contains the new location and size in view coordinates.
    /// </summary>
    procedure OnPopupSize(const browser: ICefBrowser; const rect: PCefRect);
    /// <summary>
    /// Called when an element should be painted. Pixel values passed to this
    /// function are scaled relative to view coordinates based on the value of
    /// TCefScreenInfo.device_scale_factor returned from GetScreenInfo. |type|
    /// indicates whether the element is the view or the popup widget. |buffer|
    /// contains the pixel data for the whole image. |dirtyRects| contains the set
    /// of rectangles in pixel coordinates that need to be repainted. |buffer|
    /// will be |width|*|height|*4 bytes in size and represents a BGRA image with
    /// an upper-left origin. This function is only called when
    /// TCefWindowInfo.shared_texture_enabled is set to false (0).
    /// </summary>
    procedure OnPaint(const browser: ICefBrowser; kind: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; const buffer: Pointer; width, height: Integer);
    /// <summary>
    /// <para>Called when an element has been rendered to the shared texture handle.
    /// |type| indicates whether the element is the view or the popup widget.
    /// |dirtyRects| contains the set of rectangles in pixel coordinates that need
    /// to be repainted. |info| contains the shared handle; on Windows it is a
    /// HANDLE to a texture that can be opened with D3D11 OpenSharedResource, on
    /// macOS it is an IOSurface pointer that can be opened with Metal or OpenGL,
    /// and on Linux it contains several planes, each with an fd to the underlying
    /// system native buffer.</para>
    /// <para>The underlying implementation uses a pool to deliver frames. As a result,
    /// the handle may differ every frame depending on how many frames are in-
    /// progress. The handle's resource cannot be cached and cannot be accessed
    /// outside of this callback. It should be reopened each time this callback is
    /// executed and the contents should be copied to a texture owned by the
    /// client application. The contents of |info| will be released back to the
    /// pool after this callback returns.</para>
    /// </summary>
    procedure OnAcceleratedPaint(const browser: ICefBrowser; kind: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; const info: PCefAcceleratedPaintInfo);
    /// <summary>
    /// Called to retrieve the size of the touch handle for the specified
    /// |orientation|.
    /// </summary>
    procedure GetTouchHandleSize(const browser: ICefBrowser; orientation: TCefHorizontalAlignment; var size: TCefSize);
    /// <summary>
    /// Called when touch handle state is updated. The client is responsible for
    /// rendering the touch handles.
    /// </summary>
    procedure OnTouchHandleStateChanged(const browser: ICefBrowser; const state: TCefTouchHandleState);
    /// <summary>
    /// Called when the user starts dragging content in the web view. Contextual
    /// information about the dragged content is supplied by |drag_data|. (|x|,
    /// |y|) is the drag start location in screen coordinates. OS APIs that run a
    /// system message loop may be used within the StartDragging call.
    ///
    /// Return false (0) to abort the drag operation. Don't call any of
    /// ICefBrowserHost.DragSource*Ended* functions after returning false (0).
    ///
    /// Return true (1) to handle the drag operation. Call
    /// ICefBrowserHost.DragSourceEndedAt and DragSourceSystemDragEnded either
    /// synchronously or asynchronously to inform the web view that the drag
    /// operation has ended.
    /// </summary>
    function  OnStartDragging(const browser: ICefBrowser; const dragData: ICefDragData; allowedOps: TCefDragOperations; x, y: Integer): Boolean;
    /// <summary>
    /// Called when the web view wants to update the mouse cursor during a drag &
    /// drop operation. |operation| describes the allowed operation (none, move,
    /// copy, link).
    /// </summary>
    procedure OnUpdateDragCursor(const browser: ICefBrowser; operation: TCefDragOperation);
    /// <summary>
    /// Called when the scroll offset has changed.
    /// </summary>
    procedure OnScrollOffsetChanged(const browser: ICefBrowser; x, y: Double);
    /// <summary>
    /// Called when the IME composition range has changed. |selected_range| is the
    /// range of characters that have been selected. |character_bounds| is the
    /// bounds of each character in view coordinates.
    /// </summary>
    procedure OnIMECompositionRangeChanged(const browser: ICefBrowser; const selected_range: PCefRange; character_boundsCount: NativeUInt; const character_bounds: PCefRect);
    /// <summary>
    /// Called when text selection has changed for the specified |browser|.
    /// |selected_text| is the currently selected text and |selected_range| is the
    /// character range.
    /// </summary>
    procedure OnTextSelectionChanged(const browser: ICefBrowser; const selected_text: ustring; const selected_range: PCefRange);
    /// <summary>
    /// Called when an on-screen keyboard should be shown or hidden for the
    /// specified |browser|. |input_mode| specifies what kind of keyboard should
    /// be opened. If |input_mode| is CEF_TEXT_INPUT_MODE_NONE, any existing
    /// keyboard for this browser should be hidden.
    /// </summary>
    procedure OnVirtualKeyboardRequested(const browser: ICefBrowser; input_mode: TCefTextInpuMode);
    /// <summary>
    /// Custom procedure to clear all references.
    /// </summary>
    procedure RemoveReferences;
  end;

  /// <summary>
  /// Implement this interface to provide handler implementations.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefClient">Implements TCefClient</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_client_capi.h">CEF source file: /include/capi/cef_client_capi.h (cef_client_t)</see></para>
  /// </remarks>
  ICefClient = interface(ICefBaseRefCounted)
    ['{1D502075-2FF0-4E13-A112-9E541CD811F4}']
    /// <summary>
    /// Return the handler for audio rendering events.
    /// </summary>
    procedure GetAudioHandler(var aHandler : ICefAudioHandler);
    /// <summary>
    /// Return the handler for commands. If no handler is provided the default
    /// implementation will be used.
    /// </summary>
    procedure GetCommandHandler(var aHandler : ICefCommandHandler);
    /// <summary>
    /// Return the handler for context menus. If no handler is provided the
    /// default implementation will be used.
    /// </summary>
    procedure GetContextMenuHandler(var aHandler : ICefContextMenuHandler);
    /// <summary>
    /// Return the handler for dialogs. If no handler is provided the default
    /// implementation will be used.
    /// </summary>
    procedure GetDialogHandler(var aHandler : ICefDialogHandler);
    /// <summary>
    /// Return the handler for browser display state events.
    /// </summary>
    procedure GetDisplayHandler(var aHandler : ICefDisplayHandler);
    /// <summary>
    /// Return the handler for download events. If no handler is returned
    /// downloads will not be allowed.
    /// </summary>
    procedure GetDownloadHandler(var aHandler : ICefDownloadHandler);
    /// <summary>
    /// Return the handler for drag events.
    /// </summary>
    procedure GetDragHandler(var aHandler : ICefDragHandler);
    /// <summary>
    /// Return the handler for find result events.
    /// </summary>
    procedure GetFindHandler(var aHandler : ICefFindHandler);
    /// <summary>
    /// Return the handler for focus events.
    /// </summary>
    procedure GetFocusHandler(var aHandler : ICefFocusHandler);
    /// <summary>
    /// Return the handler for events related to ICefFrame lifespan. This
    /// function will be called once during ICefBrowser creation and the result
    /// will be cached for performance reasons.
    /// </summary>
    procedure GetFrameHandler(var aHandler : ICefFrameHandler);
    /// <summary>
    /// Return the handler for permission requests.
    /// </summary>
    procedure GetPermissionHandler(var aHandler: ICefPermissionHandler);
    /// <summary>
    /// Return the handler for JavaScript dialogs. If no handler is provided the
    /// default implementation will be used.
    /// </summary>
    procedure GetJsdialogHandler(var aHandler : ICefJsdialogHandler);
    /// <summary>
    /// Return the handler for keyboard events.
    /// </summary>
    procedure GetKeyboardHandler(var aHandler : ICefKeyboardHandler);
    /// <summary>
    /// Return the handler for browser life span events.
    /// </summary>
    procedure GetLifeSpanHandler(var aHandler : ICefLifeSpanHandler);
    /// <summary>
    /// Return the handler for browser load status events.
    /// </summary>
    procedure GetLoadHandler(var aHandler : ICefLoadHandler);
    /// <summary>
    /// Return the handler for printing on Linux. If a print handler is not
    /// provided then printing will not be supported on the Linux platform.
    /// </summary>
    procedure GetPrintHandler(var aHandler : ICefPrintHandler);
    /// <summary>
    /// Return the handler for off-screen rendering events.
    /// </summary>
    procedure GetRenderHandler(var aHandler : ICefRenderHandler);
    /// <summary>
    /// Return the handler for browser request events.
    /// </summary>
    procedure GetRequestHandler(var aHandler : ICefRequestHandler);
    /// <summary>
    /// Called when a new message is received from a different process. Return
    /// true (1) if the message was handled or false (0) otherwise.  It is safe to
    /// keep a reference to |message| outside of this callback.
    /// </summary>
    function  OnProcessMessageReceived(const browser: ICefBrowser; const frame: ICefFrame; sourceProcess: TCefProcessId; const message_ : ICefProcessMessage): Boolean;
    /// <summary>
    /// Custom procedure to clear all references.
    /// </summary>
    procedure RemoveReferences;
  end;

  /// <summary>
  /// Interface used to make a URL request. URL requests are not associated with a
  /// browser instance so no ICefClient callbacks will be executed. URL requests
  /// can be created on any valid CEF thread in either the browser or render
  /// process. Once created the functions of the URL request object must be
  /// accessed on the same thread that created it.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefUrlRequest">Implements TCefUrlRequest</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_urlrequest_capi.h">CEF source file: /include/capi/cef_urlrequest_capi.h (cef_urlrequest_t)</see></para>
  /// </remarks>
  ICefUrlRequest = interface(ICefBaseRefCounted)
    ['{59226AC1-A0FA-4D59-9DF4-A65C42391A67}']
    /// <summary>
    /// Returns the request object used to create this URL request. The returned
    /// object is read-only and should not be modified.
    /// </summary>
    function  GetRequest: ICefRequest;
    /// <summary>
    /// Returns the client.
    /// </summary>
    function  GetClient: ICefUrlrequestClient;
    /// <summary>
    /// Returns the request status.
    /// </summary>
    function  GetRequestStatus: TCefUrlRequestStatus;
    /// <summary>
    /// Returns the request error if status is UR_CANCELED or UR_FAILED, or 0
    /// otherwise.
    /// </summary>
    function  GetRequestError: Integer;
    /// <summary>
    /// Returns the response, or NULL if no response information is available.
    /// Response information will only be available after the upload has
    /// completed. The returned object is read-only and should not be modified.
    /// </summary>
    function  GetResponse: ICefResponse;
    /// <summary>
    /// Returns true (1) if the response body was served from the cache. This
    /// includes responses for which revalidation was required.
    /// </summary>
    function  GetResponseWasCached: boolean;
    /// <summary>
    /// Cancel the request.
    /// </summary>
    procedure Cancel;
    /// <summary>
    /// Returns the request object used to create this URL request. The returned
    /// object is read-only and should not be modified.
    /// </summary>
    property Request           : ICefRequest           read GetRequest;
    /// <summary>
    /// Returns the client.
    /// </summary>
    property Client            : ICefUrlrequestClient  read Getclient;
    /// <summary>
    /// Returns the request status.
    /// </summary>
    property RequestStatus     : TCefUrlRequestStatus  read GetRequestStatus;
    /// <summary>
    /// Returns the request error if status is UR_CANCELED or UR_FAILED, or 0
    /// otherwise.
    /// </summary>
    property RequestError      : Integer               read GetRequestError;
    /// <summary>
    /// Returns true (1) if the response body was served from the cache. This
    /// includes responses for which revalidation was required.
    /// </summary>
    property Response          : ICefResponse          read GetResponse;
    /// <summary>
    /// Returns true (1) if the response body was served from the cache. This
    /// includes responses for which revalidation was required.
    /// </summary>
    property ResponseWasCached : boolean               read GetResponseWasCached;
  end;

  /// <summary>
  /// Interface that should be implemented by the ICefUrlRequest client. The
  /// functions of this interface will be called on the same thread that created
  /// the request unless otherwise documented.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefUrlrequestClient">Implements TCefUrlrequestClient</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_urlrequest_capi.h">CEF source file: /include/capi/cef_urlrequest_capi.h (cef_urlrequest_client_t)</see></para>
  /// </remarks>
  ICefUrlrequestClient = interface(ICefBaseRefCounted)
    ['{114155BD-C248-4651-9A4F-26F3F9A4F737}']
    /// <summary>
    /// Notifies the client that the request has completed. Use the
    /// ICefUrlRequest.GetRequestStatus function to determine if the request
    /// was successful or not.
    /// </summary>
    procedure OnRequestComplete(const request: ICefUrlRequest);
    /// <summary>
    /// Notifies the client of upload progress. |current| denotes the number of
    /// bytes sent so far and |total| is the total size of uploading data (or -1
    /// if chunked upload is enabled). This function will only be called if the
    /// UR_FLAG_REPORT_UPLOAD_PROGRESS flag is set on the request.
    /// </summary>
    procedure OnUploadProgress(const request: ICefUrlRequest; current, total: Int64);
    /// <summary>
    /// Notifies the client of download progress. |current| denotes the number of
    /// bytes received up to the call and |total| is the expected total size of
    /// the response (or -1 if not determined).
    /// </summary>
    procedure OnDownloadProgress(const request: ICefUrlRequest; current, total: Int64);
    /// <summary>
    /// Called when some part of the response is read. |data| contains the current
    /// bytes received since the last call. This function will not be called if
    /// the UR_FLAG_NO_DOWNLOAD_DATA flag is set on the request.
    /// </summary>
    procedure OnDownloadData(const request: ICefUrlRequest; data: Pointer; dataLength: NativeUInt);
    /// <summary>
    /// Called on the IO thread when the browser needs credentials from the user.
    /// |isProxy| indicates whether the host is a proxy server. |host| contains
    /// the hostname and |port| contains the port number. Return true (1) to
    /// continue the request and call ICefAuthCallback.cont() when the
    /// authentication information is available. If the request has an associated
    /// browser/frame then returning false (0) will result in a call to
    /// GetAuthCredentials on the ICefRequestHandler associated with that
    /// browser, if any. Otherwise, returning false (0) will cancel the request
    /// immediately. This function will only be called for requests initiated from
    /// the browser process.
    /// </summary>
    function  OnGetAuthCredentials(isProxy: Boolean; const host: ustring; port: Integer; const realm, scheme: ustring; const callback: ICefAuthCallback): Boolean;
    /// <summary>
    /// Custom procedure to clear all references.
    /// </summary>
    procedure RemoveReferences;
  end;

  /// <summary>
  /// Implement this interface to receive notification when tracing has completed.
  /// The functions of this interface will be called on the browser process UI
  /// thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefEndTracingCallback">Implements TCefEndTracingCallback</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_trace_capi.h">CEF source file: /include/capi/cef_trace_capi.h (cef_end_tracing_callback_t)</see></para>
  /// </remarks>
  ICefEndTracingCallback = interface(ICefBaseRefCounted)
    ['{79020EBE-9D1D-49A6-9714-8778FE8929F2}']
    /// <summary>
    /// Called after all processes have sent their trace data. |tracing_file| is
    /// the path at which tracing data was written. The client is responsible for
    /// deleting |tracing_file|.
    /// </summary>
    procedure OnEndTracingComplete(const tracingFile: ustring);
  end;

  /// <summary>
  /// Callback interface for asynchronous continuation of file dialog requests.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefFileDialogCallback">Implements TCefFileDialogCallback</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_dialog_handler_capi.h">CEF source file: /include/capi/cef_dialog_handler_capi.h (cef_file_dialog_callback_t)</see></para>
  /// </remarks>
  ICefFileDialogCallback = interface(ICefBaseRefCounted)
    ['{1AF659AB-4522-4E39-9C52-184000D8E3C7}']
    /// <summary>
    /// Continue the file selection. |file_paths| should be a single value or a
    /// list of values depending on the dialog mode. An NULL |file_paths| value is
    /// treated the same as calling cancel().
    /// </summary>
    procedure Cont(const filePaths: TStrings);
    /// <summary>
    /// Cancel the file selection.
    /// </summary>
    procedure Cancel;
  end;

  /// <summary>
  /// Callback structure for asynchronous handling of an unresponsive process.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefUnresponsiveProcessCallback">Implements TCefUnresponsiveProcessCallback</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_unresponsive_process_callback_capi.h">CEF source file: /include/capi/cef_unresponsive_process_callback_capi.h (cef_unresponsive_process_callback_t)</see></para>
  /// </remarks>
  ICefUnresponsiveProcessCallback = interface(ICefBaseRefCounted)
    ['{3E4F2B66-5AAF-4906-B946-C114D0E43C13}']
    /// <summary>
    /// Reset the timeout for the unresponsive process.
    /// </summary>
    procedure Wait;
    /// <summary>
    /// Terminate the unresponsive process.
    /// </summary>
    procedure Terminate;
  end;

  /// <summary>
  /// Interface used to represent drag data. The functions of this interface may
  /// be called on any thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefDragData">Implements TCefDragData</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_drag_data_capi.h">CEF source file: /include/capi/cef_drag_data_capi.h (cef_drag_data_t)</see></para>
  /// </remarks>
  ICefDragData = interface(ICefBaseRefCounted)
    ['{FBB6A487-F633-4055-AB3E-6619EDE75683}']
    /// <summary>
    /// Returns a copy of the current object.
    /// </summary>
    function  Clone: ICefDragData;
    /// <summary>
    /// Returns true (1) if this object is read-only.
    /// </summary>
    function  IsReadOnly: Boolean;
    /// <summary>
    /// Returns true (1) if the drag data is a link.
    /// </summary>
    function  IsLink: Boolean;
    /// <summary>
    /// Returns true (1) if the drag data is a text or html fragment.
    /// </summary>
    function  IsFragment: Boolean;
    /// <summary>
    /// Returns true (1) if the drag data is a file.
    /// </summary>
    function  IsFile: Boolean;
    /// <summary>
    /// Return the link URL that is being dragged.
    /// </summary>
    function  GetLinkUrl: ustring;
    /// <summary>
    /// Return the title associated with the link being dragged.
    /// </summary>
    function  GetLinkTitle: ustring;
    /// <summary>
    /// Return the metadata, if any, associated with the link being dragged.
    /// </summary>
    function  GetLinkMetadata: ustring;
    /// <summary>
    /// Return the plain text fragment that is being dragged.
    /// </summary>
    function  GetFragmentText: ustring;
    /// <summary>
    /// Return the text/html fragment that is being dragged.
    /// </summary>
    function  GetFragmentHtml: ustring;
    /// <summary>
    /// Return the base URL that the fragment came from. This value is used for
    /// resolving relative URLs and may be NULL.
    /// </summary>
    function  GetFragmentBaseUrl: ustring;
    /// <summary>
    /// Return the name of the file being dragged out of the browser window.
    /// </summary>
    function  GetFileName: ustring;
    /// <summary>
    /// Write the contents of the file being dragged out of the web view into
    /// |writer|. Returns the number of bytes sent to |writer|. If |writer| is
    /// NULL this function will return the size of the file contents in bytes.
    /// Call get_file_name() to get a suggested name for the file.
    /// </summary>
    function  GetFileContents(const writer: ICefStreamWriter): NativeUInt;
    /// <summary>
    /// Retrieve the list of file names that are being dragged into the browser
    /// window.
    /// </summary>
    function  GetFileNames(var names: TStrings): Integer;
    /// <summary>
    /// Retrieve the list of file paths that are being dragged into the browser
    /// window.
    /// </summary>
    function  GetFilePaths(var paths: TStrings): Integer;
    /// <summary>
    /// Set the link URL that is being dragged.
    /// </summary>
    procedure SetLinkUrl(const url: ustring);
    /// <summary>
    /// Set the title associated with the link being dragged.
    /// </summary>
    procedure SetLinkTitle(const title: ustring);
    /// <summary>
    /// Set the metadata associated with the link being dragged.
    /// </summary>
    procedure SetLinkMetadata(const data: ustring);
    /// <summary>
    /// Set the plain text fragment that is being dragged.
    /// </summary>
    procedure SetFragmentText(const text: ustring);
    /// <summary>
    /// Set the text/html fragment that is being dragged.
    /// </summary>
    procedure SetFragmentHtml(const html: ustring);
    /// <summary>
    /// Set the base URL that the fragment came from.
    /// </summary>
    procedure SetFragmentBaseUrl(const baseUrl: ustring);
    /// <summary>
    /// Reset the file contents. You should do this before calling
    /// ICefBrowserHost.DragTargetDragEnter as the web view does not allow us
    /// to drag in this kind of data.
    /// </summary>
    procedure ResetFileContents;
    /// <summary>
    /// Add a file that is being dragged into the webview.
    /// </summary>
    procedure AddFile(const path, displayName: ustring);
    /// <summary>
    /// Clear list of filenames.
    /// </summary>
    procedure ClearFilenames;
    /// <summary>
    /// Get the image representation of drag data. May return NULL if no image
    /// representation is available.
    /// </summary>
    function  GetImage : ICefImage;
    /// <summary>
    /// Get the image hotspot (drag start location relative to image dimensions).
    /// </summary>
    function  GetImageHotspot : TCefPoint;
    /// <summary>
    /// Returns true (1) if an image representation of drag data is available.
    /// </summary>
    function  HasImage : boolean;
  end;

  /// <summary>
  /// Implement this interface to handle events related to dragging. The functions
  /// of this interface will be called on the UI thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefDragHandler">Implements TCefDragHandler</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_drag_handler_capi.h">CEF source file: /include/capi/cef_drag_handler_capi.h (cef_drag_handler_t)</see></para>
  /// </remarks>
  ICefDragHandler = interface(ICefBaseRefCounted)
    ['{59A89579-5B18-489F-A25C-5CC25FF831FC}']
    /// <summary>
    /// Called when an external drag event enters the browser window. |dragData|
    /// contains the drag event data and |mask| represents the type of drag
    /// operation. Return false (0) for default drag handling behavior or true (1)
    /// to cancel the drag event.
    /// </summary>
    function  OnDragEnter(const browser: ICefBrowser; const dragData: ICefDragData; mask: TCefDragOperations): Boolean;
    /// <summary>
    /// Called whenever draggable regions for the browser window change. These can
    /// be specified using the '-webkit-app-region: drag/no-drag' CSS-property. If
    /// draggable regions are never defined in a document this function will also
    /// never be called. If the last draggable region is removed from a document
    /// this function will be called with an NULL vector.
    /// </summary>
    procedure OnDraggableRegionsChanged(const browser: ICefBrowser; const frame: ICefFrame; regionsCount: NativeUInt; const regions: PCefDraggableRegionArray);
    /// <summary>
    /// Custom procedure to clear all references.
    /// </summary>
    procedure RemoveReferences;
  end;

  /// <summary>
  /// Implement this interface to handle events related to find results. The
  /// functions of this interface will be called on the UI thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefFindHandler">Implements TCefFindHandler</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_find_handler_capi.h">CEF source file: /include/capi/cef_find_handler_capi.h (cef_find_handler_t)</see></para>
  /// </remarks>
  ICefFindHandler = interface(ICefBaseRefCounted)
    ['{F20DF234-BD43-42B3-A80B-D354A9E5B787}']
    /// <summary>
    /// Called to report find results returned by ICefBrowserHost.find().
    /// |identifer| is a unique incremental identifier for the currently active
    /// search, |count| is the number of matches currently identified,
    /// |selectionRect| is the location of where the match was found (in window
    /// coordinates), |activeMatchOrdinal| is the current position in the search
    /// results, and |finalUpdate| is true (1) if this is the last find
    /// notification.
    /// </summary>
    procedure OnFindResult(const browser: ICefBrowser; identifier, count: Integer; const selectionRect: PCefRect; activeMatchOrdinal: Integer; finalUpdate: Boolean);
    /// <summary>
    /// Custom procedure to clear all references.
    /// </summary>
    procedure RemoveReferences;
  end;

  /// <summary>
  /// Implement this interface to provide handler implementations. The handler
  /// instance will not be released until all objects related to the context have
  /// been destroyed.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefRequestContextHandler">Implements TCefRequestContextHandler</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_request_context_handler_capi.h">CEF source file: /include/capi/cef_request_context_handler_capi.h (cef_request_context_handler_t)</see></para>
  /// </remarks>
  ICefRequestContextHandler = interface(ICefBaseRefCounted)
    ['{76EB1FA7-78DF-4FD5-ABB3-1CDD3E73A140}']
    /// <summary>
    /// Called on the browser process UI thread immediately after the request
    /// context has been initialized.
    /// </summary>
    procedure OnRequestContextInitialized(const request_context: ICefRequestContext);
    /// <summary>
    /// Called on the browser process IO thread before a resource request is
    /// initiated. The |browser| and |frame| values represent the source of the
    /// request, and may be NULL for requests originating from service workers or
    /// ICefUrlRequest. |request| represents the request contents and cannot be
    /// modified in this callback. |is_navigation| will be true (1) if the
    /// resource request is a navigation. |is_download| will be true (1) if the
    /// resource request is a download. |request_initiator| is the origin (scheme
    /// + domain) of the page that initiated the request. Set
    /// |disable_default_handling| to true (1) to disable default handling of the
    /// request, in which case it will need to be handled via
    /// ICefResourceRequestHandler.GetResourceHandler or it will be canceled.
    /// To allow the resource load to proceed with default handling return NULL.
    /// To specify a handler for the resource return a
    /// ICefResourceRequestHandler object. This function will not be called if
    /// the client associated with |browser| returns a non-NULL value from
    /// ICefRequestHandler.GetResourceRequestHandler for the same request
    /// (identified by ICefRequest.GetIdentifier).
    /// </summary>
    procedure GetResourceRequestHandler(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; is_navigation, is_download: boolean; const request_initiator: ustring; var disable_default_handling: boolean; var aResourceRequestHandler : ICefResourceRequestHandler);
    /// <summary>
    /// Custom procedure to clear all references.
    /// </summary>
    procedure RemoveReferences;
  end;

  /// <summary>
  /// Callback interface for ICefRequestContext.ResolveHost.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefResolveCallback">Implements TCefResolveCallback</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_request_context_capi.h">CEF source file: /include/capi/cef_request_context_capi.h (cef_resolve_callback_t)</see></para>
  /// </remarks>
  ICefResolveCallback = interface(ICefBaseRefCounted)
    ['{0C0EA252-7968-4163-A1BE-A1453576DD06}']
    /// <summary>
    /// Called on the UI thread after the ResolveHost request has completed.
    /// |result| will be the result code. |resolved_ips| will be the list of
    /// resolved IP addresses or NULL if the resolution failed.
    /// </summary>
    procedure OnResolveCompleted(result: TCefErrorCode; const resolvedIps: TStrings);
  end;

  /// <summary>
  /// Manage access to preferences. Many built-in preferences are registered by
  /// Chromium. Custom preferences can be registered in
  /// ICefBrowserProcessHandler.OnRegisterCustomPreferences.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefPreferenceManager">Implements TCefPreferenceManager</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_preference_capi.h">CEF source file: /include/capi/cef_preference_capi.h (cef_preference_manager_t)</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_preference_manager_capi.h">CEF source file: /include/capi/cef_preference_manager_capi.h (cef_preference_manager_t)</see></para>
  /// </remarks>
  ICefPreferenceManager = interface(ICefBaseRefCounted)
    ['{E8231D35-D028-4E64-BFDB-7E4596027DEC}']
    /// <summary>
    /// Returns true (1) if a preference with the specified |name| exists. This
    /// function must be called on the browser process UI thread.
    /// </summary>
    function  HasPreference(const name: ustring): Boolean;
    /// <summary>
    /// Returns the value for the preference with the specified |name|. Returns
    /// NULL if the preference does not exist. The returned object contains a copy
    /// of the underlying preference value and modifications to the returned
    /// object will not modify the underlying preference value. This function must
    /// be called on the browser process UI thread.
    /// </summary>
    function  GetPreference(const name: ustring): ICefValue;
    /// <summary>
    /// Returns all preferences as a dictionary. If |include_defaults| is true (1)
    /// then preferences currently at their default value will be included. The
    /// returned object contains a copy of the underlying preference values and
    /// modifications to the returned object will not modify the underlying
    /// preference values. This function must be called on the browser process UI
    /// thread.
    /// </summary>
    function  GetAllPreferences(includeDefaults: Boolean): ICefDictionaryValue;
    /// <summary>
    /// Returns true (1) if the preference with the specified |name| can be
    /// modified using SetPreference. As one example preferences set via the
    /// command-line usually cannot be modified. This function must be called on
    /// the browser process UI thread.
    /// </summary>
    function  CanSetPreference(const name: ustring): Boolean;
    /// <summary>
    /// Set the |value| associated with preference |name|. Returns true (1) if the
    /// value is set successfully and false (0) otherwise. If |value| is NULL the
    /// preference will be restored to its default value. If setting the
    /// preference fails then |error| will be populated with a detailed
    /// description of the problem. This function must be called on the browser
    /// process UI thread.
    /// </summary>
    function  SetPreference(const name: ustring; const value: ICefValue; out error: ustring): Boolean;
  end;

  /// <summary>
  /// A request context provides request handling for a set of related browser or
  /// URL request objects. A request context can be specified when creating a new
  /// browser via the ICefBrowserHost static factory functions or when creating
  /// a new URL request via the ICefUrlRequest static factory functions. Browser
  /// objects with different request contexts will never be hosted in the same
  /// render process. Browser objects with the same request context may or may not
  /// be hosted in the same render process depending on the process model. Browser
  /// objects created indirectly via the JavaScript window.open function or
  /// targeted links will share the same render process and the same request
  /// context as the source browser. When running in single-process mode there is
  /// only a single render process (the main process) and so all browsers created
  /// in single-process mode will share the same request context. This will be the
  /// first request context passed into a ICefBrowserHost static factory
  /// function and all other request context objects will be ignored.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefRequestContext">Implements TCefRequestContext</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_request_context_capi.h">CEF source file: /include/capi/cef_request_context_capi.h (cef_request_context_t)</see></para>
  /// </remarks>
  ICefRequestContext = interface(ICefPreferenceManager)
    ['{5830847A-2971-4BD5-ABE6-21451F8923F7}']
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
    /// <summary>
    /// Returns the cookie manager for this object. If |callback| is non-NULL it
    /// will be executed asnychronously on the UI thread after the manager's
    /// storage has been initialized.
    /// </summary>
    function  GetCookieManagerProc(const callback: TCefCompletionCallbackProc): ICefCookieManager;
    /// <summary>
    /// Register a scheme handler factory for the specified |scheme_name| and
    /// optional |domain_name|. An NULL |domain_name| value for a standard scheme
    /// will cause the factory to match all domain names. The |domain_name| value
    /// will be ignored for non-standard schemes. If |scheme_name| is a built-in
    /// scheme and no handler is returned by |factory| then the built-in scheme
    /// handler factory will be called. If |scheme_name| is a custom scheme then
    /// you must also implement the ICefApp.OnRegisterCustomSchemes()
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
    /// ICefRequestHandler.OnCertificateError(). If you call this it is
    /// recommended that you also call CloseAllConnections() or you risk not
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
    /// <summary>
    /// Returns the cache path for this object. If NULL an "incognito mode" in-
    /// memory cache is being used.
    /// </summary>
    property  CachePath                : ustring          read GetCachePath;
    /// <summary>
    /// Returns true (1) if this object is the global context. The global context
    /// is used by default when creating a browser or URL request with a NULL
    /// context argument.
    /// </summary>
    property  IsGlobalContext          : boolean          read IsGlobal;
    /// <summary>
    /// Returns the current Chrome color scheme mode (SYSTEM, LIGHT or DARK). Must
    /// be called on the browser process UI thread.
    /// </summary>
    property  ChromeColorSchemeMode    : TCefColorVariant read GetChromeColorSchemeMode;
    /// <summary>
    /// Returns the current Chrome color scheme color, or transparent (0) for the
    /// default color. Must be called on the browser process UI thread.
    /// </summary>
    property  ChromeColorSchemeColor   : TCefColor        read GetChromeColorSchemeColor;
    /// <summary>
    /// Returns the current Chrome color scheme variant. Must be called on the
    /// browser process UI thread.
    /// </summary>
    property  ChromeColorSchemeVariant : TCefColorVariant read GetChromeColorSchemeVariant;
  end;

  /// <summary>
  /// Interface representing print settings.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefPrintSettings">Implements TCefPrintSettings</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_print_settings_capi.h">CEF source file: /include/capi/cef_print_settings_capi.h (cef_print_settings_t)</see></para>
  /// </remarks>
  ICefPrintSettings = Interface(ICefBaseRefCounted)
    ['{ACBD2395-E9C1-49E5-B7F3-344DAA4A0F12}']
    /// <summary>
    /// Returns true (1) if this object is valid. Do not call any other functions
    /// if this function returns false (0).
    /// </summary>
    function  IsValid: Boolean;
    /// <summary>
    /// Returns true (1) if the values of this object are read-only. Some APIs may
    /// expose read-only objects.
    /// </summary>
    function  IsReadOnly: Boolean;
    /// <summary>
    /// Set the page orientation.
    /// </summary>
    procedure SetOrientation(landscape: Boolean);
    /// <summary>
    /// Returns true (1) if the orientation is landscape.
    /// </summary>
    function  IsLandscape: Boolean;
    /// <summary>
    /// Set the printer printable area in device units. Some platforms already
    /// provide flipped area. Set |landscape_needs_flip| to false (0) on those
    /// platforms to avoid double flipping.
    /// </summary>
    procedure SetPrinterPrintableArea(const physicalSizeDeviceUnits: PCefSize; const printableAreaDeviceUnits: PCefRect; landscapeNeedsFlip: Boolean);
    /// <summary>
    /// Set the device name.
    /// </summary>
    procedure SetDeviceName(const name: ustring);
    /// <summary>
    /// Get the device name.
    /// </summary>
    function  GetDeviceName: ustring;
    /// <summary>
    /// Set the DPI (dots per inch).
    /// </summary>
    procedure SetDpi(dpi: Integer);
    /// <summary>
    /// Get the DPI (dots per inch).
    /// </summary>
    function  GetDpi: Integer;
    /// <summary>
    /// Set the page ranges.
    /// </summary>
    procedure SetPageRanges(const ranges: TCefRangeArray);
    /// <summary>
    /// Returns the number of page ranges that currently exist.
    /// </summary>
    function  GetPageRangesCount: NativeUInt;
    /// <summary>
    /// Retrieve the page ranges.
    /// </summary>
    procedure GetPageRanges(out ranges: TCefRangeArray);
    /// <summary>
    /// Set whether only the selection will be printed.
    /// </summary>
    procedure SetSelectionOnly(selectionOnly: Boolean);
    /// <summary>
    /// Returns true (1) if only the selection will be printed.
    /// </summary>
    function  IsSelectionOnly: Boolean;
    /// <summary>
    /// Set whether pages will be collated.
    /// </summary>
    procedure SetCollate(collate: Boolean);
    /// <summary>
    /// Returns true (1) if pages will be collated.
    /// </summary>
    function  WillCollate: Boolean;
    /// <summary>
    /// Set the color model.
    /// </summary>
    procedure SetColorModel(model: TCefColorModel);
    /// <summary>
    /// Get the color model.
    /// </summary>
    function  GetColorModel: TCefColorModel;
    /// <summary>
    /// Set the number of copies.
    /// </summary>
    procedure SetCopies(copies: Integer);
    /// <summary>
    /// Get the number of copies.
    /// </summary>
    function  GetCopies: Integer;
    /// <summary>
    /// Set the duplex mode.
    /// </summary>
    procedure SetDuplexMode(mode: TCefDuplexMode);
    /// <summary>
    /// Get the duplex mode.
    /// </summary>
    function  GetDuplexMode: TCefDuplexMode;
    /// <summary>
    /// Returns true (1) if the orientation is landscape.
    /// </summary>
    property Landscape      : Boolean         read IsLandscape      write SetOrientation;
    /// <summary>
    /// Get the device name.
    /// </summary>
    property DeviceName     : ustring         read GetDeviceName    write SetDeviceName;
    /// <summary>
    /// Get the DPI (dots per inch).
    /// </summary>
    property Dpi            : Integer         read GetDpi           write SetDpi;
    /// <summary>
    /// Returns true (1) if only the selection will be printed.
    /// </summary>
    property SelectionOnly  : Boolean         read IsSelectionOnly  write SetSelectionOnly;
    /// <summary>
    /// Returns true (1) if pages will be collated.
    /// </summary>
    property Collate        : Boolean         read WillCollate      write SetCollate;
    /// <summary>
    /// Get the color model.
    /// </summary>
    property ColorModel     : TCefColorModel  read GetColorModel    write SetColorModel;
    /// <summary>
    /// Get the number of copies.
    /// </summary>
    property Copies         : Integer         read GetCopies        write SetCopies;
    /// <summary>
    /// Get the duplex mode.
    /// </summary>
    property DuplexMode     : TCefDuplexMode  read GetDuplexMode    write SetDuplexMode;
  end;

  /// <summary>
  /// Callback interface for asynchronous continuation of print dialog requests.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefPrintDialogCallback">Implements TCefPrintDialogCallback</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_print_handler_capi.h">CEF source file: /include/capi/cef_print_handler_capi.h (cef_print_dialog_callback_t)</see></para>
  /// </remarks>
  ICefPrintDialogCallback = interface(ICefBaseRefCounted)
    ['{1D7FB71E-0019-4A80-95ED-91DDD019253B}']
    /// <summary>
    /// Continue printing with the specified |settings|.
    /// </summary>
    procedure cont(const settings: ICefPrintSettings);
    /// <summary>
    /// Cancel the printing.
    /// </summary>
    procedure cancel;
  end;

  /// <summary>
  /// Callback interface for asynchronous continuation of print job requests.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefPrintJobCallback">Implements TCefPrintJobCallback</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_print_handler_capi.h">CEF source file: /include/capi/cef_print_handler_capi.h (cef_print_job_callback_t)</see></para>
  /// </remarks>
  ICefPrintJobCallback = interface(ICefBaseRefCounted)
    ['{5554852A-052C-464B-A868-B618C7E7E2FD}']
    /// <summary>
    /// Indicate completion of the print job.
    /// </summary>
    procedure cont;
  end;

  /// <summary>
  /// Implement this interface to handle printing on Linux. Each browser will have
  /// only one print job in progress at a time. The functions of this interface
  /// will be called on the browser process UI thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefPrintHandler">Implements TCefPrintHandler</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_print_handler_capi.h">CEF source file: /include/capi/cef_print_handler_capi.h (cef_print_handler_t)</see></para>
  /// </remarks>
  ICefPrintHandler = interface(ICefBaseRefCounted)
    ['{2831D5C9-6E2B-4A30-A65A-0F4435371EFC}']
    /// <summary>
    /// Called when printing has started for the specified |browser|. This
    /// function will be called before the other OnPrint*() functions and
    /// irrespective of how printing was initiated (e.g.
    /// ICefBrowserHost.print(), JavaScript window.print() or PDF extension
    /// print button).
    /// </summary>
    procedure OnPrintStart(const browser: ICefBrowser);
    /// <summary>
    /// Synchronize |settings| with client state. If |get_defaults| is true (1)
    /// then populate |settings| with the default print settings. Do not keep a
    /// reference to |settings| outside of this callback.
    /// </summary>
    procedure OnPrintSettings(const browser: ICefBrowser; const settings: ICefPrintSettings; getDefaults: boolean);
    /// <summary>
    /// Show the print dialog. Execute |callback| once the dialog is dismissed.
    /// Return true (1) if the dialog will be displayed or false (0) to cancel the
    /// printing immediately.
    /// </summary>
    procedure OnPrintDialog(const browser: ICefBrowser; hasSelection: boolean; const callback: ICefPrintDialogCallback; var aResult: boolean);
    /// <summary>
    /// Send the print job to the printer. Execute |callback| once the job is
    /// completed. Return true (1) if the job will proceed or false (0) to cancel
    /// the job immediately.
    /// </summary>
    procedure OnPrintJob(const browser: ICefBrowser; const documentName, PDFFilePath: ustring; const callback: ICefPrintJobCallback; var aResult: boolean);
    /// <summary>
    /// Reset client state related to printing.
    /// </summary>
    procedure OnPrintReset(const browser: ICefBrowser);
    /// <summary>
    /// Return the PDF paper size in device units. Used in combination with
    /// ICefBrowserHost.PrintToPdf().
    /// </summary>
    procedure GetPDFPaperSize(const browser: ICefBrowser; deviceUnitsPerInch: integer; var aResult: TCefSize);
    /// <summary>
    /// Custom procedure to clear all references.
    /// </summary>
    procedure RemoveReferences;
  end;

  /// <summary>
  /// Interface used to represent an entry in navigation history.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefNavigationEntry">Implements TCefNavigationEntry</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_navigation_entry_capi.h">CEF source file: /include/capi/cef_navigation_entry_capi.h (cef_navigation_entry_t)</see></para>
  /// </remarks>
  ICefNavigationEntry = interface(ICefBaseRefCounted)
    ['{D17B4B37-AA45-42D9-B4E4-AAB6FE2AB297}']
    /// <summary>
    /// Returns true (1) if this object is valid. Do not call any other functions
    /// if this function returns false (0).
    /// </summary>
    function IsValid: Boolean;
    /// <summary>
    /// Returns the actual URL of the page. For some pages this may be data: URL
    /// or similar. Use get_display_url() to return a display-friendly version.
    /// </summary>
    function GetUrl: ustring;
    /// <summary>
    /// Returns a display-friendly version of the URL.
    /// </summary>
    function GetDisplayUrl: ustring;
    /// <summary>
    /// Returns the original URL that was entered by the user before any
    /// redirects.
    /// </summary>
    function GetOriginalUrl: ustring;
    /// <summary>
    /// Returns the title set by the page. This value may be NULL.
    /// </summary>
    function GetTitle: ustring;
    /// <summary>
    /// Returns the transition type which indicates what the user did to move to
    /// this page from the previous page.
    /// </summary>
    function GetTransitionType: TCefTransitionType;
    /// <summary>
    /// Returns true (1) if this navigation includes post data.
    /// </summary>
    function HasPostData: Boolean;
    /// <summary>
    /// Returns the time for the last known successful navigation completion. A
    /// navigation may be completed more than once if the page is reloaded. May be
    /// 0 if the navigation has not yet completed.
    /// </summary>
    function GetCompletionTime: TDateTime;
    /// <summary>
    /// Returns the HTTP status code for the last known successful navigation
    /// response. May be 0 if the response has not yet been received or if the
    /// navigation has not yet completed.
    /// </summary>
    function GetHttpStatusCode: Integer;
    /// <summary>
    /// Returns the SSL information for this navigation entry.
    /// </summary>
    function GetSSLStatus: ICefSSLStatus;
    /// <summary>
    /// Returns the actual URL of the page. For some pages this may be data: URL
    /// or similar. Use get_display_url() to return a display-friendly version.
    /// </summary>
    property Url              : ustring             read GetUrl;
    /// <summary>
    /// Returns a display-friendly version of the URL.
    /// </summary>
    property DisplayUrl       : ustring             read GetDisplayUrl;
    /// <summary>
    /// Returns the original URL that was entered by the user before any
    /// redirects.
    /// </summary>
    property OriginalUrl      : ustring             read GetOriginalUrl;
    /// <summary>
    /// Returns the title set by the page. This value may be NULL.
    /// </summary>
    property Title            : ustring             read GetTitle;
    /// <summary>
    /// Returns the transition type which indicates what the user did to move to
    /// this page from the previous page.
    /// </summary>
    property TransitionType   : TCefTransitionType  read GetTransitionType;
    /// <summary>
    /// Returns the time for the last known successful navigation completion. A
    /// navigation may be completed more than once if the page is reloaded. May be
    /// 0 if the navigation has not yet completed.
    /// </summary>
    property CompletionTime   : TDateTime           read GetCompletionTime;
    /// <summary>
    /// Returns the HTTP status code for the last known successful navigation
    /// response. May be 0 if the response has not yet been received or if the
    /// navigation has not yet completed.
    /// </summary>
    property HttpStatusCode   : Integer             read GetHttpStatusCode;
    /// <summary>
    /// Returns the SSL information for this navigation entry.
    /// </summary>
    property SSLStatus        : ICefSSLStatus       read GetSSLStatus;
  end;

  /// <summary>
  /// Interface representing the issuer or subject field of an X.509 certificate.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefX509CertPrincipal">Implements TCefX509CertPrincipal</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_x509_certificate_capi.h">CEF source file: /include/capi/cef_x509_certificate_capi.h (cef_x509cert_principal_t)</see></para>
  /// </remarks>
  ICefX509CertPrincipal = interface(ICefBaseRefCounted)
    ['{CD3621ED-7D68-4A1F-95B5-190C7001B65F}']
    /// <summary>
    /// Returns a name that can be used to represent the issuer. It tries in this
    /// order: Common Name (CN), Organization Name (O) and Organizational Unit
    /// Name (OU) and returns the first non-NULL one found.
    /// </summary>
    function  GetDisplayName: ustring;
    /// <summary>
    /// Returns the common name.
    /// </summary>
    function  GetCommonName: ustring;
    /// <summary>
    /// Returns the locality name.
    /// </summary>
    function  GetLocalityName: ustring;
    /// <summary>
    /// Returns the state or province name.
    /// </summary>
    function  GetStateOrProvinceName: ustring;
    /// <summary>
    /// Returns the country name.
    /// </summary>
    function  GetCountryName: ustring;
    /// <summary>
    /// Retrieve the list of organization names.
    /// </summary>
    procedure GetOrganizationNames(const names: TStrings);
    /// <summary>
    /// Retrieve the list of organization unit names.
    /// </summary>
    procedure GetOrganizationUnitNames(const names: TStrings);
  end;

  /// <summary>
  /// Interface representing a X.509 certificate.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefX509Certificate">Implements TCefX509Certificate</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_x509_certificate_capi.h">CEF source file: /include/capi/cef_x509_certificate_capi.h (cef_x509certificate_t)</see></para>
  /// </remarks>
  ICefX509Certificate = interface(ICefBaseRefCounted)
    ['{C897979D-F068-4428-82DF-4221612FF7E0}']
    /// <summary>
    /// Returns the subject of the X.509 certificate. For HTTPS server
    /// certificates this represents the web server.  The common name of the
    /// subject should match the host name of the web server.
    /// </summary>
    function  GetSubject: ICefX509CertPrincipal;
    /// <summary>
    /// Returns the issuer of the X.509 certificate.
    /// </summary>
    function  GetIssuer: ICefX509CertPrincipal;
    /// <summary>
    /// Returns the DER encoded serial number for the X.509 certificate. The value
    /// possibly includes a leading 00 byte.
    /// </summary>
    function  GetSerialNumber: ICefBinaryValue;
    /// <summary>
    /// Returns the date before which the X.509 certificate is invalid.
    /// CefBaseTime.GetTimeT() will return 0 if no date was specified.
    /// </summary>
    function  GetValidStart: TCefBaseTime;
    /// <summary>
    /// Returns the date after which the X.509 certificate is invalid.
    /// CefBaseTime.GetTimeT() will return 0 if no date was specified.
    /// </summary>
    function  GetValidExpiry: TCefBaseTime;
    /// <summary>
    /// Returns the date before which the X.509 certificate is invalid.
    /// CefBaseTime.GetTimeT() will return 0 if no date was specified.
    /// </summary>
    function  GetValidStartAsDateTime: TDateTime;
    /// <summary>
    /// Returns the date after which the X.509 certificate is invalid.
    /// CefBaseTime.GetTimeT() will return 0 if no date was specified.
    /// </summary>
    function  GetValidExpiryAsDateTime: TDateTime;
    /// <summary>
    /// Returns the DER encoded data for the X.509 certificate.
    /// </summary>
    function  GetDerEncoded: ICefBinaryValue;
    /// <summary>
    /// Returns the PEM encoded data for the X.509 certificate.
    /// </summary>
    function  GetPemEncoded: ICefBinaryValue;
    /// <summary>
    /// Returns the number of certificates in the issuer chain. If 0, the
    /// certificate is self-signed.
    /// </summary>
    function  GetIssuerChainSize: NativeUInt;
    /// <summary>
    /// Returns the DER encoded data for the certificate issuer chain. If we
    /// failed to encode a certificate in the chain it is still present in the
    /// array but is an NULL string.
    /// </summary>
    procedure GetDEREncodedIssuerChain(chainCount: NativeUInt; var chain : TCefBinaryValueArray);
    /// <summary>
    /// Returns the PEM encoded data for the certificate issuer chain. If we
    /// failed to encode a certificate in the chain it is still present in the
    /// array but is an NULL string.
    /// </summary>
    procedure GetPEMEncodedIssuerChain(chainCount: NativeUInt; var chain : TCefBinaryValueArray);
  end;

  /// <summary>
  /// Interface representing SSL information.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefSslInfo">Implements TCefSslInfo</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_ssl_info_capi.h">CEF source file: /include/capi/cef_ssl_info_capi.h (cef_sslinfo_t)</see></para>
  /// </remarks>
  ICefSslInfo = interface(ICefBaseRefCounted)
    ['{67EC86BD-DE7D-453D-908F-AD15626C514F}']
    /// <summary>
    /// Returns a bitmask containing any and all problems verifying the server
    /// certificate.
    /// </summary>
    function GetCertStatus: TCefCertStatus;
    /// <summary>
    /// Returns the X.509 certificate.
    /// </summary>
    function GetX509Certificate: ICefX509Certificate;
  end;

  /// <summary>
  /// Interface representing the SSL information for a navigation entry.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefSSLStatus">Implements TCefSSLStatus</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_ssl_status_capi.h">CEF source file: /include/capi/cef_ssl_status_capi.h (cef_sslstatus_t)</see></para>
  /// </remarks>
  ICefSSLStatus = interface(ICefBaseRefCounted)
    ['{E3F004F2-03D5-46A2-91D0-510C50F3B225}']
    /// <summary>
    /// Returns true (1) if the status is related to a secure SSL/TLS connection.
    /// </summary>
    function IsSecureConnection: boolean;
    /// <summary>
    /// Returns a bitmask containing any and all problems verifying the server
    /// certificate.
    /// </summary>
    function GetCertStatus: TCefCertStatus;
    /// <summary>
    /// Returns the SSL version used for the SSL connection.
    /// </summary>
    function GetSSLVersion: TCefSSLVersion;
    /// <summary>
    /// Returns a bitmask containing the page security content status.
    /// </summary>
    function GetContentStatus: TCefSSLContentStatus;
    /// <summary>
    /// Returns the X.509 certificate.
    /// </summary>
    function GetX509Certificate: ICefX509Certificate;
  end;

  /// <summary>
  /// Callback interface used to select a client certificate for authentication.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefSelectClientCertificateCallback">Implements TCefSelectClientCertificateCallback</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_request_handler_capi.h">CEF source file: /include/capi/cef_request_handler_capi.h (cef_select_client_certificate_callback_t)</see></para>
  /// </remarks>
  ICefSelectClientCertificateCallback = interface(ICefBaseRefCounted)
    ['{003E3D09-ADE8-4C6E-A174-079D3D616608}']
    /// <summary>
    /// Chooses the specified certificate for client certificate authentication.
    /// NULL value means that no client certificate should be used.
    /// </summary>
    procedure Select(const cert: ICefX509Certificate);
  end;

  /// <summary>
  /// Interface used for retrieving resources from the resource bundle (*.pak)
  /// files loaded by CEF during startup or via the ICefResourceBundleHandler
  /// returned from ICefApp.GetResourceBundleHandler. See TCefSettings for
  /// additional options related to resource bundle loading. The functions of this
  /// interface may be called on any thread unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefResourceBundle">Implements TCefResourceBundle</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_resource_bundle_capi.h">CEF source file: /include/capi/cef_resource_bundle_capi.h (cef_resource_bundle_t)</see></para>
  /// </remarks>
  ICefResourceBundle = interface(ICefBaseRefCounted)
    ['{3213CF97-C854-452B-B615-39192F8D07DC}']
    /// <summary>
    /// Returns the localized string for the specified |string_id| or an NULL
    /// string if the value is not found. Include cef_pack_strings.h for a listing
    /// of valid string ID values.
    /// </summary>
    function GetLocalizedString(stringId: Integer): ustring;
    /// <summary>
    /// Returns a ICefBinaryValue containing the decompressed contents of the
    /// specified scale independent |resource_id| or NULL if not found. Include
    /// cef_pack_resources.h for a listing of valid resource ID values.
    /// </summary>
    function GetDataResource(resourceId: Integer): ICefBinaryValue;
    /// <summary>
    /// Returns a ICefBinaryValue containing the decompressed contents of the
    /// specified |resource_id| nearest the scale factor |scale_factor| or NULL if
    /// not found. Use a |scale_factor| value of SCALE_FACTOR_NONE for scale
    /// independent resources or call GetDataResource instead.Include
    /// cef_pack_resources.h for a listing of valid resource ID values.
    /// </summary>
    function GetDataResourceForScale(resourceId: Integer; scaleFactor: TCefScaleFactor): ICefBinaryValue;
  end;

  /// <summary>
  /// Container for a single image represented at different scale factors. All
  /// image representations should be the same size in density independent pixel
  /// (DIP) units. For example, if the image at scale factor 1.0 is 100x100 pixels
  /// then the image at scale factor 2.0 should be 200x200 pixels -- both images
  /// will display with a DIP size of 100x100 units. The functions of this
  /// interface can be called on any browser process thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefImage">Implements TCefImage</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_image_capi.h">CEF source file: /include/capi/cef_image_capi.h (cef_image_t)</see></para>
  /// </remarks>
  ICefImage = interface(ICefBaseRefCounted)
    ['{E2C2F424-26A2-4498-BB45-DA23219831BE}']
    /// <summary>
    /// Returns true (1) if this Image is NULL.
    /// </summary>
    function IsEmpty: Boolean;
    /// <summary>
    /// Returns true (1) if this Image and |that| Image share the same underlying
    /// storage. Will also return true (1) if both images are NULL.
    /// </summary>
    function IsSame(const that: ICefImage): Boolean;
    /// <summary>
    /// Add a bitmap image representation for |scale_factor|. Only 32-bit
    /// RGBA/BGRA formats are supported. |pixel_width| and |pixel_height| are the
    /// bitmap representation size in pixel coordinates. |pixel_data| is the array
    /// of pixel data and should be |pixel_width| x |pixel_height| x 4 bytes in
    /// size. |color_type| and |alpha_type| values specify the pixel format.
    /// </summary>
    function AddBitmap(scaleFactor: Single; pixelWidth, pixelHeight: Integer; colorType: TCefColorType; alphaType: TCefAlphaType; const pixelData: Pointer; pixelDataSize: NativeUInt): Boolean;
    /// <summary>
    /// Add a PNG image representation for |scale_factor|. |png_data| is the image
    /// data of size |png_data_size|. Any alpha transparency in the PNG data will
    /// be maintained.
    /// </summary>
    function AddPng(scaleFactor: Single; const pngData: Pointer; pngDataSize: NativeUInt): Boolean;
    /// <summary>
    /// Create a JPEG image representation for |scale_factor|. |jpeg_data| is the
    /// image data of size |jpeg_data_size|. The JPEG format does not support
    /// transparency so the alpha byte will be set to 0xFF for all pixels.
    /// </summary>
    function AddJpeg(scaleFactor: Single; const jpegData: Pointer; jpegDataSize: NativeUInt): Boolean;
    /// <summary>
    /// Returns the image width in density independent pixel (DIP) units.
    /// </summary>
    function GetWidth: NativeUInt;
    /// <summary>
    /// Returns the image height in density independent pixel (DIP) units.
    /// </summary>
    function GetHeight: NativeUInt;
    /// <summary>
    /// Returns true (1) if this image contains a representation for
    /// |scale_factor|.
    /// </summary>
    function HasRepresentation(scaleFactor: Single): Boolean;
    /// <summary>
    /// Removes the representation for |scale_factor|. Returns true (1) on
    /// success.
    /// </summary>
    function RemoveRepresentation(scaleFactor: Single): Boolean;
    /// <summary>
    /// Returns information for the representation that most closely matches
    /// |scale_factor|. |actual_scale_factor| is the actual scale factor for the
    /// representation. |pixel_width| and |pixel_height| are the representation
    /// size in pixel coordinates. Returns true (1) on success.
    /// </summary>
    function GetRepresentationInfo(scaleFactor: Single; var actualScaleFactor: Single; var pixelWidth, pixelHeight: Integer): Boolean;
    /// <summary>
    /// Returns the bitmap representation that most closely matches
    /// |scale_factor|. Only 32-bit RGBA/BGRA formats are supported. |color_type|
    /// and |alpha_type| values specify the desired output pixel format.
    /// |pixel_width| and |pixel_height| are the output representation size in
    /// pixel coordinates. Returns a ICefBinaryValue containing the pixel data
    /// on success or NULL on failure.
    /// </summary>
    function GetAsBitmap(scaleFactor: Single; colorType: TCefColorType; alphaType: TCefAlphaType; var pixelWidth, pixelHeight: Integer): ICefBinaryValue;
    /// <summary>
    /// Returns the PNG representation that most closely matches |scale_factor|.
    /// If |with_transparency| is true (1) any alpha transparency in the image
    /// will be represented in the resulting PNG data. |pixel_width| and
    /// |pixel_height| are the output representation size in pixel coordinates.
    /// Returns a ICefBinaryValue containing the PNG image data on success or
    /// NULL on failure.
    /// </summary>
    function GetAsPng(scaleFactor: Single; withTransparency: Boolean; var pixelWidth, pixelHeight: Integer): ICefBinaryValue;
    /// <summary>
    /// Returns the JPEG representation that most closely matches |scale_factor|.
    /// |quality| determines the compression level with 0 == lowest and 100 ==
    /// highest. The JPEG format does not support alpha transparency and the alpha
    /// channel, if any, will be discarded. |pixel_width| and |pixel_height| are
    /// the output representation size in pixel coordinates. Returns a
    /// ICefBinaryValue containing the JPEG image data on success or NULL on
    /// failure.
    /// </summary>
    function GetAsJpeg(scaleFactor: Single; quality: Integer; var pixelWidth, pixelHeight: Integer): ICefBinaryValue;
    /// <summary>
    /// Returns the image width in density independent pixel (DIP) units.
    /// </summary>
    property Width  : NativeUInt read GetWidth;
    /// <summary>
    /// Returns the image height in density independent pixel (DIP) units.
    /// </summary>
    property Height : NativeUInt read GetHeight;
  end;

  /// <summary>
  /// Implement this interface to handle menu model events. The functions of this
  /// interface will be called on the browser process UI thread unless otherwise
  /// indicated.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefMenuModelDelegate">Implements TCefMenuModelDelegate</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_menu_model_delegate_capi.h">CEF source file: /include/capi/cef_menu_model_delegate_capi.h (cef_menu_model_delegate_t)</see></para>
  /// </remarks>
  ICefMenuModelDelegate = interface(ICefBaseRefCounted)
    ['{1430D202-2795-433E-9A35-C79A0996F316}']
    /// <summary>
    /// Perform the action associated with the specified |command_id| and optional
    /// |event_flags|.
    /// </summary>
    procedure ExecuteCommand(const menuModel: ICefMenuModel; commandId: Integer; eventFlags: TCefEventFlags);
    /// <summary>
    /// Called when the user moves the mouse outside the menu and over the owning
    /// window.
    /// </summary>
    procedure MouseOutsideMenu(const menuModel: ICefMenuModel; const screenPoint: PCefPoint);
    /// <summary>
    /// Called on unhandled open submenu keyboard commands. |is_rtl| will be true
    /// (1) if the menu is displaying a right-to-left language.
    /// </summary>
    procedure UnhandledOpenSubmenu(const menuModel: ICefMenuModel; isRTL: boolean);
    /// <summary>
    /// Called on unhandled close submenu keyboard commands. |is_rtl| will be true
    /// (1) if the menu is displaying a right-to-left language.
    /// </summary>
    procedure UnhandledCloseSubmenu(const menuModel: ICefMenuModel; isRTL: boolean);
    /// <summary>
    /// The menu is about to show.
    /// </summary>
    procedure MenuWillShow(const menuModel: ICefMenuModel);
    /// <summary>
    /// The menu has closed.
    /// </summary>
    procedure MenuClosed(const menuModel: ICefMenuModel);
    /// <summary>
    /// Optionally modify a menu item label. Return true (1) if |label| was
    /// modified.
    /// </summary>
    function  FormatLabel(const menuModel: ICefMenuModel; var label_ : ustring) : boolean;
  end;

  /// <summary>
  /// Interface representing a server that supports HTTP and WebSocket requests.
  /// Server capacity is limited and is intended to handle only a small number of
  /// simultaneous connections (e.g. for communicating between applications on
  /// localhost). The functions of this interface are safe to call from any thread
  /// in the brower process unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefServer">Implements TCefServer</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_server_capi.h">CEF source file: /include/capi/cef_server_capi.h (cef_server_t)</see></para>
  /// </remarks>
  ICefServer = interface(ICefBaseRefCounted)
    ['{41D41764-A74B-4552-B166-C77E70549047}']
    /// <summary>
    /// Returns the task runner for the dedicated server thread.
    /// </summary>
    function  GetTaskRunner : ICefTaskRunner;
    /// <summary>
    /// Stop the server and shut down the dedicated server thread. See
    /// ICefServerHandler.OnServerCreated documentation for a description of
    /// server lifespan.
    /// </summary>
    procedure Shutdown;
    /// <summary>
    /// Returns true (1) if the server is currently running and accepting incoming
    /// connections. See ICefServerHandler.OnServerCreated documentation for a
    /// description of server lifespan. This function must be called on the
    /// dedicated server thread.
    /// </summary>
    function  IsRunning : boolean;
    /// <summary>
    /// Returns the server address including the port number.
    /// </summary>
    function  GetAddress : ustring;
    /// <summary>
    /// Returns true (1) if the server currently has a connection. This function
    /// must be called on the dedicated server thread.
    /// </summary>
    function  HasConnection : boolean;
    /// <summary>
    /// Returns true (1) if |connection_id| represents a valid connection. This
    /// function must be called on the dedicated server thread.
    /// </summary>
    function  IsValidConnection(connection_id: Integer) : boolean;
    /// <summary>
    /// Send an HTTP 200 "OK" response to the connection identified by
    /// |connection_id|. |content_type| is the response content type (e.g.
    /// "text/html"), |data| is the response content, and |data_size| is the size
    /// of |data| in bytes. The contents of |data| will be copied. The connection
    /// will be closed automatically after the response is sent.
    /// </summary>
    procedure SendHttp200response(connection_id: Integer; const content_type: ustring; const data: Pointer; data_size: NativeUInt);
    /// <summary>
    /// Send an HTTP 404 "Not Found" response to the connection identified by
    /// |connection_id|. The connection will be closed automatically after the
    /// response is sent.
    /// </summary>
    procedure SendHttp404response(connection_id: Integer);
    /// <summary>
    /// Send an HTTP 500 "Internal Server Error" response to the connection
    /// identified by |connection_id|. |error_message| is the associated error
    /// message. The connection will be closed automatically after the response is
    /// sent.
    /// </summary>
    procedure SendHttp500response(connection_id: Integer; const error_message: ustring);
    /// <summary>
    /// Send a custom HTTP response to the connection identified by
    /// |connection_id|. |response_code| is the HTTP response code sent in the
    /// status line (e.g. 200), |content_type| is the response content type sent
    /// as the "Content-Type" header (e.g. "text/html"), |content_length| is the
    /// expected content length, and |extra_headers| is the map of extra response
    /// headers. If |content_length| is >= 0 then the "Content-Length" header will
    /// be sent. If |content_length| is 0 then no content is expected and the
    /// connection will be closed automatically after the response is sent. If
    /// |content_length| is < 0 then no "Content-Length" header will be sent and
    /// the client will continue reading until the connection is closed. Use the
    /// SendRawData function to send the content, if applicable, and call
    /// CloseConnection after all content has been sent.
    /// </summary>
    procedure SendHttpResponse(connection_id, response_code: Integer; const content_type: ustring; content_length: int64; const extra_headers: ICefStringMultimap);
    /// <summary>
    /// Send raw data directly to the connection identified by |connection_id|.
    /// |data| is the raw data and |data_size| is the size of |data| in bytes. The
    /// contents of |data| will be copied. No validation of |data| is performed
    /// internally so the client should be careful to send the amount indicated by
    /// the "Content-Length" header, if specified. See SendHttpResponse
    /// documentation for intended usage.
    /// </summary>
    procedure SendRawData(connection_id: Integer; const data: Pointer; data_size: NativeUInt);
    /// <summary>
    /// Close the connection identified by |connection_id|. See SendHttpResponse
    /// documentation for intended usage.
    /// </summary>
    procedure CloseConnection(connection_id: Integer);
    /// <summary>
    /// Send a WebSocket message to the connection identified by |connection_id|.
    /// |data| is the response content and |data_size| is the size of |data| in
    /// bytes. The contents of |data| will be copied. See
    /// ICefServerHandler.OnWebSocketRequest documentation for intended usage.
    /// </summary>
    procedure SendWebSocketMessage(connection_id: Integer; const data: Pointer; data_size: NativeUInt);
  end;

  /// <summary>
  /// Implement this interface to handle HTTP server requests. A new thread will
  /// be created for each ICefServer.CreateServer call (the "dedicated server
  /// thread"), and the functions of this interface will be called on that thread.
  /// It is therefore recommended to use a different ICefServerHandler instance
  /// for each ICefServer.CreateServer call to avoid thread safety issues in
  /// the ICefServerHandler implementation.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefServerHandler">Implements TCefServerHandler</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_server_capi.h">CEF source file: /include/capi/cef_server_capi.h (cef_server_handler_t)</see></para>
  /// </remarks>
  ICefServerHandler = interface(ICefBaseRefCounted)
    ['{AFB64A63-44C9-44CD-959B-D8E20F549879}']
    /// <summary>
    /// Called when |server| is created. If the server was started successfully
    /// then ICefServer.IsRunning will return true (1). The server will
    /// continue running until ICefServerShutdown is called, after which time
    /// OnServerDestroyed will be called. If the server failed to start then
    /// OnServerDestroyed will be called immediately after this function returns.
    /// </summary>
    procedure OnServerCreated(const server: ICefServer);
    /// <summary>
    /// Called when |server| is destroyed. The server thread will be stopped after
    /// this function returns. The client should release any references to
    /// |server| when this function is called. See OnServerCreated documentation
    /// for a description of server lifespan.
    /// </summary>
    procedure OnServerDestroyed(const server: ICefServer);
    /// <summary>
    /// Called when a client connects to |server|. |connection_id| uniquely
    /// identifies the connection. Each call to this function will have a matching
    /// call to OnClientDisconnected.
    /// </summary>
    procedure OnClientConnected(const server: ICefServer; connection_id: Integer);
    /// <summary>
    /// Called when a client disconnects from |server|. |connection_id| uniquely
    /// identifies the connection. The client should release any data associated
    /// with |connection_id| when this function is called and |connection_id|
    /// should no longer be passed to ICefServer functions. Disconnects can
    /// originate from either the client or the server. For example, the server
    /// will disconnect automatically after a ICefServer.SendHttpXXXResponse
    /// function is called.
    /// </summary>
    procedure OnClientDisconnected(const server: ICefServer; connection_id: Integer);
    /// <summary>
    /// Called when |server| receives an HTTP request. |connection_id| uniquely
    /// identifies the connection, |client_address| is the requesting IPv4 or IPv6
    /// client address including port number, and |request| contains the request
    /// contents (URL, function, headers and optional POST data). Call
    /// ICefServer functions either synchronously or asynchronusly to send a
    /// response.
    /// </summary>
    procedure OnHttpRequest(const server: ICefServer; connection_id: Integer; const client_address: ustring; const request: ICefRequest);
    /// <summary>
    /// Called when |server| receives a WebSocket request. |connection_id|
    /// uniquely identifies the connection, |client_address| is the requesting
    /// IPv4 or IPv6 client address including port number, and |request| contains
    /// the request contents (URL, function, headers and optional POST data).
    /// Execute |callback| either synchronously or asynchronously to accept or
    /// decline the WebSocket connection. If the request is accepted then
    /// OnWebSocketConnected will be called after the WebSocket has connected and
    /// incoming messages will be delivered to the OnWebSocketMessage callback. If
    /// the request is declined then the client will be disconnected and
    /// OnClientDisconnected will be called. Call the
    /// ICefServer.SendWebSocketMessage function after receiving the
    /// OnWebSocketConnected callback to respond with WebSocket messages.
    /// </summary>
    procedure OnWebSocketRequest(const server: ICefServer; connection_id: Integer; const client_address: ustring; const request: ICefRequest; const callback: ICefCallback);
    /// <summary>
    /// Called after the client has accepted the WebSocket connection for |server|
    /// and |connection_id| via the OnWebSocketRequest callback. See
    /// OnWebSocketRequest documentation for intended usage.
    /// </summary>
    procedure OnWebSocketConnected(const server: ICefServer; connection_id: Integer);
    /// <summary>
    /// Called when |server| receives an WebSocket message. |connection_id|
    /// uniquely identifies the connection, |data| is the message content and
    /// |data_size| is the size of |data| in bytes. Do not keep a reference to
    /// |data| outside of this function. See OnWebSocketRequest documentation for
    /// intended usage.
    /// </summary>
    procedure OnWebSocketMessage(const server: ICefServer; connection_id: Integer; const data: Pointer; data_size: NativeUInt);
  end;

  /// <summary>
  /// Callback interface used for asynchronous continuation of media access
  /// permission requests.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefMediaAccessCallback">Implements TCefMediaAccessCallback</see></para>
  /// This record is declared twice with almost identical parameters. "allowed_permissions" is defined as int and uint32.
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_media_access_handler_capi.h">CEF source file: /include/capi/cef_media_access_handler_capi.h (cef_media_access_callback_t)</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_permission_handler_capi.h">CEF source file: /include/capi/cef_permission_handler_capi.h (cef_media_access_callback_t)</see></para>
  /// </remarks>
  ICefMediaAccessCallback = interface(ICefBaseRefCounted)
    ['{66F6F5F4-8489-408B-B9ED-6B705C2E2010}']
    /// <summary>
    /// Call to allow or deny media access. If this callback was initiated in
    /// response to a getUserMedia (indicated by
    /// CEF_MEDIA_PERMISSION_DEVICE_AUDIO_CAPTURE and/or
    /// CEF_MEDIA_PERMISSION_DEVICE_VIDEO_CAPTURE being set) then
    /// |allowed_permissions| must match |required_permissions| passed to
    /// OnRequestMediaAccessPermission.
    /// </summary>
    procedure cont(allowed_permissions: TCefMediaAccessPermissionTypes);
    /// <summary>
    /// Cancel the media access request.
    /// </summary>
    procedure cancel;
  end;

  /// <summary>
  /// Implement this interface to handle events related to media access permission
  /// requests. The functions of this interface will be called on the browser
  /// process UI thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefMediaAccessHandler">Implements TCefMediaAccessHandler</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_media_access_handler_capi.h">CEF source file: /include/capi/cef_media_access_handler_capi.h (cef_media_access_handler_t)</see></para>
  /// </remarks>
  ICefMediaAccessHandler = interface(ICefBaseRefCounted)
    ['{8ED04C4A-05F2-46FD-89C4-E6114000D219}']
    function OnRequestMediaAccessPermission(const browser: ICefBrowser; const frame: ICefFrame; const requesting_url: ustring; requested_permissions: TCefMediaAccessPermissionTypes; const callback: ICefMediaAccessCallback): boolean;
    /// <summary>
    /// Custom procedure to clear all references.
    /// </summary>
    procedure RemoveReferences;
  end;

  /// <summary>
  /// Callback interface used for asynchronous continuation of permission prompts.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefPermissionPromptCallback">Implements TCefPermissionPromptCallback</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_permission_handler_capi.h">CEF source file: /include/capi/cef_permission_handler_capi.h (cef_permission_prompt_callback_t)</see></para>
  /// </remarks>
  ICefPermissionPromptCallback = interface(ICefBaseRefCounted)
    ['{F8827C7D-7B14-499E-B38A-5F9FEB1FD6A6}']
    /// <summary>
    /// Complete the permissions request with the specified |result|.
    /// </summary>
    procedure cont(result: TCefPermissionRequestResult);
  end;

  /// <summary>
  /// Implement this interface to handle events related to permission requests.
  /// The functions of this interface will be called on the browser process UI
  /// thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefPermissionHandler">Implements TCefPermissionHandler</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_permission_handler_capi.h">CEF source file: /include/capi/cef_permission_handler_capi.h (cef_permission_handler_t)</see></para>
  /// </remarks>
  ICefPermissionHandler = interface(ICefBaseRefCounted)
    ['{DC079268-FB08-44DA-B216-35C5C339B341}']
    /// <summary>
    /// <para>Called when a page requests permission to access media.</para>
    /// <para>|requesting_origin| is the URL origin requesting permission.</para>
    /// <para>|requested_permissions| is a combination of values from
    /// TCefMediaAccessPermissionTypes that represent the requested
    /// permissions.</para>
    /// <para>Return true (1) and call ICefMediaAccessCallback
    /// functions either in this function or at a later time to continue or cancel
    /// the request.</para>
    /// <para>Return false (0) to proceed with default handling. With
    /// Chrome style, default handling will display the permission request UI.</para>
    /// <para>With Alloy style, default handling will deny the request. This function
    /// will not be called if the "--enable-media-stream" command-line switch is
    /// used to grant all permissions.</para>
    /// </summary>
    function  OnRequestMediaAccessPermission(const browser: ICefBrowser; const frame: ICefFrame; const requesting_origin: ustring; requested_permissions: cardinal; const callback: ICefMediaAccessCallback): boolean;
    /// <summary>
    /// Called when a page should show a permission prompt. |prompt_id| uniquely
    /// identifies the prompt. |requesting_origin| is the URL origin requesting
    /// permission. |requested_permissions| is a combination of values from
    /// TCefPermissionRequestTypes that represent the requested permissions.
    /// Return true (1) and call ICefPermissionPromptCallback.Continue either
    /// in this function or at a later time to continue or cancel the request.
    /// Return false (0) to proceed with default handling. With Chrome style,
    /// default handling will display the permission prompt UI. With Alloy
    /// style, default handling is CEF_PERMISSION_RESULT_IGNORE.
    /// </summary>
    function  OnShowPermissionPrompt(const browser: ICefBrowser; prompt_id: uint64; const requesting_origin: ustring; requested_permissions: cardinal; const callback: ICefPermissionPromptCallback): boolean;
    /// <summary>
    /// Called when a permission prompt handled via OnShowPermissionPrompt is
    /// dismissed. |prompt_id| will match the value that was passed to
    /// OnShowPermissionPrompt. |result| will be the value passed to
    /// ICefPermissionPromptCallback.Continue or CEF_PERMISSION_RESULT_IGNORE
    /// if the dialog was dismissed for other reasons such as navigation, browser
    /// closure, etc. This function will not be called if OnShowPermissionPrompt
    /// returned false (0) for |prompt_id|.
    /// </summary>
    procedure OnDismissPermissionPrompt(const browser: ICefBrowser; prompt_id: uint64; result: TCefPermissionRequestResult);
    /// <summary>
    /// Custom procedure to clear all references.
    /// </summary>
    procedure RemoveReferences;
  end;

  /// <summary>
  /// Interface that wraps platform-dependent share memory region mapping.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefSharedMemoryRegion">Implements TCefSharedMemoryRegion</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_shared_memory_region_capi.h">CEF source file: /include/capi/cef_shared_memory_region_capi.h (cef_shared_memory_region_t)</see></para>
  /// </remarks>
  ICefSharedMemoryRegion = interface(ICefBaseRefCounted)
    ['{2828D0E1-44D0-4C6F-8C63-5CA6036DDA82}']
    /// <summary>
    /// Returns true (1) if the mapping is valid.
    /// </summary>
    function IsValid: boolean;
    /// <summary>
    /// Returns the size of the mapping in bytes. Returns 0 for invalid instances.
    /// </summary>
    function Size: NativeUInt;
    /// <summary>
    /// Returns the pointer to the memory. Returns nullptr for invalid instances.
    /// The returned pointer is only valid for the life span of this object.
    /// </summary>
    function Memory: pointer;
  end;

  /// <summary>
  /// Interface that builds a ICefProcessMessage containing a shared memory
  /// region. This interface is not thread-safe but may be used exclusively on a
  /// different thread from the one which constructed it.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefSharedProcessMessageBuilder">Implements TCefSharedProcessMessageBuilder</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_shared_process_message_builder_capi.h">CEF source file: /include/capi/cef_shared_process_message_builder_capi.h (cef_shared_process_message_builder_t)</see></para>
  /// </remarks>
  ICefSharedProcessMessageBuilder = interface(ICefBaseRefCounted)
    ['{B2AF627F-33FA-44F1-B943-FC4F120C84F8}']
    /// <summary>
    /// Returns true (1) if the builder is valid.
    /// </summary>
    function IsValid: boolean;
    /// <summary>
    /// Returns the size of the shared memory region in bytes. Returns 0 for
    /// invalid instances.
    /// </summary>
    function Size: NativeUInt;
    /// <summary>
    /// Returns the pointer to the writable memory. Returns nullptr for invalid
    /// instances. The returned pointer is only valid for the life span of this
    /// object.
    /// </summary>
    function Memory: pointer;
    /// <summary>
    /// Creates a new ICefProcessMessage from the data provided to the builder.
    /// Returns nullptr for invalid instances. Invalidates the builder instance.
    /// </summary>
    function Build: ICefProcessMessage;
  end;


  {*
   *********************************
   ************* Views *************
   *********************************
  *}

  /// <summary>
  /// This interface typically, but not always, corresponds to a physical display
  /// connected to the system. A fake Display may exist on a headless system, or a
  /// Display may correspond to a remote, virtual display. All size and position
  /// values are in density independent pixel (DIP) coordinates unless otherwise
  /// indicated. Methods must be called on the browser process UI thread unless
  /// otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefDisplay">Implements TCefDisplay</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_display_capi.h">CEF source file: /include/capi/views/cef_display_capi.h (cef_display_t)</see></para>
  /// </remarks>
  ICefDisplay = interface(ICefBaseRefCounted)
    ['{EC2D3606-DB4C-4894-8D38-B8F99E091965}']
    /// <summary>
    /// Returns the unique identifier for this Display.
    /// </summary>
    function  GetID : int64;
    /// <summary>
    /// Returns this Display's device pixel scale factor. This specifies how much
    /// the UI should be scaled when the actual output has more pixels than
    /// standard displays (which is around 100~120dpi). The potential return
    /// values differ by platform.
    /// </summary>
    function  GetDeviceScaleFactor : Single;
    /// <summary>
    /// Convert |point| from DIP coordinates to pixel coordinates using this
    /// Display's device scale factor.
    /// </summary>
    procedure ConvertPointToPixels(var point: TCefPoint);
    /// <summary>
    /// Convert |point| from pixel coordinates to DIP coordinates using this
    /// Display's device scale factor.
    /// </summary>
    procedure ConvertPointFromPixels(var point: TCefPoint);
    /// <summary>
    /// Returns this Display's bounds in DIP screen coordinates. This is the full
    /// size of the display.
    /// </summary>
    function  GetBounds : TCefRect;
    /// <summary>
    /// Returns this Display's work area in DIP screen coordinates. This excludes
    /// areas of the display that are occupied with window manager toolbars, etc.
    /// </summary>
    function  GetWorkArea : TCefRect;
    /// <summary>
    /// Returns this Display's rotation in degrees.
    /// </summary>
    function  GetRotation : Integer;
    /// <summary>
    /// Returns the unique identifier for this Display.
    /// </summary>
    property  ID                : int64      read GetID;
    /// <summary>
    /// Returns this Display's device pixel scale factor. This specifies how much
    /// the UI should be scaled when the actual output has more pixels than
    /// standard displays (which is around 100~120dpi). The potential return
    /// values differ by platform.
    /// </summary>
    property  DeviceScaleFactor : Single     read GetDeviceScaleFactor;
    /// <summary>
    /// Returns this Display's bounds in DIP screen coordinates. This is the full
    /// size of the display.
    /// </summary>
    property  Bounds            : TCefRect   read GetBounds;
    /// <summary>
    /// Returns this Display's work area in DIP screen coordinates. This excludes
    /// areas of the display that are occupied with window manager toolbars, etc.
    /// </summary>
    property  WorkArea          : TCefRect   read GetWorkArea;
    /// <summary>
    /// Returns this Display's rotation in degrees.
    /// </summary>
    property  Rotation          : Integer    read GetRotation;
  end;

  /// <summary>
  /// A Layout handles the sizing of the children of a Panel according to
  /// implementation-specific heuristics. Methods must be called on the browser
  /// process UI thread unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefLayout">Implements TCefLayout</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_layout_capi.h">CEF source file: /include/capi/views/cef_layout_capi.h (cef_layout_t)</see></para>
  /// </remarks>
  ICefLayout = interface(ICefBaseRefCounted)
    ['{0EC7AE4B-1672-4D0B-B617-0BDA72F3C7F4}']
    /// <summary>
    /// Returns this Layout as a BoxLayout or NULL if this is not a BoxLayout.
    /// </summary>
    function AsBoxLayout : ICefBoxLayout;
    /// <summary>
    /// Returns this Layout as a FillLayout or NULL if this is not a FillLayout.
    /// </summary>
    function AsFillLayout : ICefFillLayout;
    /// <summary>
    /// Returns true (1) if this Layout is valid.
    /// </summary>
    function IsValid : boolean;

    property Valid              : boolean    read IsValid;
  end;

  /// <summary>
  /// A Layout manager that arranges child views vertically or horizontally in a
  /// side-by-side fashion with spacing around and between the child views. The
  /// child views are always sized according to their preferred size. If the
  /// host's bounds provide insufficient space, child views will be clamped.
  /// Excess space will not be distributed. Methods must be called on the browser
  /// process UI thread unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefBoxLayout">Implements TCefBoxLayout</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_box_layout_capi.h">CEF source file: /include/capi/views/cef_box_layout_capi.h (cef_box_layout_t)</see></para>
  /// </remarks>
  ICefBoxLayout = interface(ICefLayout)
    ['{E59FCCAE-A371-4C21-98D3-93D3217016AE}']
    /// <summary>
    /// Set the flex weight for the given |view|. Using the preferred size as the
    /// basis, free space along the main axis is distributed to views in the ratio
    /// of their flex weights. Similarly, if the views will overflow the parent,
    /// space is subtracted in these ratios. A flex of 0 means this view is not
    /// resized. Flex values must not be negative.
    /// </summary>
    procedure SetFlexForView(const view: ICefView; flex: Integer);
    /// <summary>
    /// Clears the flex for the given |view|, causing it to use the default flex
    /// specified via TCefBoxLayoutSettings.default_flex.
    /// </summary>
    procedure ClearFlexForView(const view: ICefView);
  end;

  /// <summary>
  /// A simple Layout that causes the associated Panel's one child to be sized to
  /// match the bounds of its parent. Methods must be called on the browser
  /// process UI thread unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefFillLayout">Implements TCefFillLayout</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_fill_layout_capi.h">CEF source file: /include/capi/views/cef_fill_layout_capi.h (cef_fill_layout_t)</see></para>
  /// </remarks>
  ICefFillLayout = interface(ICefLayout)
    ['{3DB214F2-7F27-4306-82C9-8166160422B1}']
  end;

  /// <summary>
  /// Controller for an overlay that contains a contents View added via
  /// ICefWindow.AddOverlayView. Methods exposed by this controller should be
  /// called in preference to functions of the same name exposed by the contents
  /// View unless otherwise indicated. Methods must be called on the browser
  /// process UI thread unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefOverlayController">Implements TCefOverlayController</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_overlay_controller_capi.h">CEF source file: /include/capi/views/cef_overlay_controller_capi.h (cef_overlay_controller_t)</see></para>
  /// </remarks>
  ICefOverlayController = interface(ICefBaseRefCounted)
    ['{13E1F3D2-32FF-4D30-A30E-D67B6A4846AB}']
    /// <summary>
    /// Returns true (1) if this object is valid.
    /// </summary>
    function  IsValid: boolean;
    /// <summary>
    /// Returns true (1) if this object is the same as |that| object.
    /// </summary>
    function  IsSame(const that: ICefOverlayController): boolean;
    /// <summary>
    /// Returns the contents View for this overlay.
    /// </summary>
    function  GetContentsView: ICefView;
    /// <summary>
    /// Returns the top-level Window hosting this overlay. Use this function
    /// instead of calling get_window() on the contents View.
    /// </summary>
    function  GetWindow: ICefWindow;
    /// <summary>
    /// Returns the docking mode for this overlay.
    /// </summary>
    function  GetDockingMode: TCefDockingMode;
    /// <summary>
    /// Destroy this overlay.
    /// </summary>
    procedure DestroyOverlay;
    /// <summary>
    /// Sets the bounds (size and position) of this overlay. This will set the
    /// bounds of the contents View to match and trigger a re-layout if necessary.
    /// |bounds| is in parent coordinates and any insets configured on this
    /// overlay will be ignored. Use this function only for overlays created with
    /// a docking mode value of CEF_DOCKING_MODE_CUSTOM. With other docking modes
    /// modify the insets of this overlay and/or layout of the contents View and
    /// call size_to_preferred_size() instead to calculate the new size and re-
    /// position the overlay if necessary.
    /// </summary>
    procedure SetBounds(const bounds: TCefRect);
    /// <summary>
    /// Returns the bounds (size and position) of this overlay in parent
    /// coordinates.
    /// </summary>
    function  GetBounds: TCefRect;
    /// <summary>
    /// Returns the bounds (size and position) of this overlay in DIP screen
    /// coordinates.
    /// </summary>
    function  GetBoundsInScreen: TCefRect;
    /// <summary>
    /// Sets the size of this overlay without changing the position. This will set
    /// the size of the contents View to match and trigger a re-layout if
    /// necessary. |size| is in parent coordinates and any insets configured on
    /// this overlay will be ignored. Use this function only for overlays created
    /// with a docking mode value of CEF_DOCKING_MODE_CUSTOM. With other docking
    /// modes modify the insets of this overlay and/or layout of the contents View
    /// and call size_to_preferred_size() instead to calculate the new size and
    /// re-position the overlay if necessary.
    /// </summary>
    procedure SetSize(const size: TCefSize);
    /// <summary>
    /// Returns the size of this overlay in parent coordinates.
    /// </summary>
    function  GetSize: TCefSize;
    /// <summary>
    /// Sets the position of this overlay without changing the size. |position| is
    /// in parent coordinates and any insets configured on this overlay will be
    /// ignored. Use this function only for overlays created with a docking mode
    /// value of CEF_DOCKING_MODE_CUSTOM. With other docking modes modify the
    /// insets of this overlay and/or layout of the contents View and call
    /// size_to_preferred_size() instead to calculate the new size and re-position
    /// the overlay if necessary.
    /// </summary>
    procedure SetPosition(const position: TCefPoint);
    /// <summary>
    /// Returns the position of this overlay in parent coordinates.
    /// </summary>
    function  GetPosition: TCefPoint;
    /// <summary>
    /// Sets the insets for this overlay. |insets| is in parent coordinates. Use
    /// this function only for overlays created with a docking mode value other
    /// than CEF_DOCKING_MODE_CUSTOM.
    /// </summary>
    procedure SetInsets(const insets: TCefInsets);
    /// <summary>
    /// Returns the insets for this overlay in parent coordinates.
    /// </summary>
    function  GetInsets: TCefInsets;
    /// <summary>
    /// Size this overlay to its preferred size and trigger a re-layout if
    /// necessary. The position of overlays created with a docking mode value of
    /// CEF_DOCKING_MODE_CUSTOM will not be modified by calling this function.
    /// With other docking modes this function may re-position the overlay if
    /// necessary to accommodate the new size and any insets configured on the
    /// contents View.
    /// </summary>
    procedure SizeToPreferredSize;
    /// <summary>
    /// Sets whether this overlay is visible. Overlays are hidden by default. If
    /// this overlay is hidden then it and any child Views will not be drawn and,
    /// if any of those Views currently have focus, then focus will also be
    /// cleared. Painting is scheduled as needed.
    /// </summary>
    procedure SetVisible(visible: boolean);
    /// <summary>
    /// Returns whether this overlay is visible. A View may be visible but still
    /// not drawn in a Window if any parent Views are hidden. Call is_drawn() to
    /// determine whether this overlay and all parent Views are visible and will
    /// be drawn.
    /// </summary>
    function  IsVisible: boolean;
    /// <summary>
    /// Returns whether this overlay is visible and drawn in a Window. A View is
    /// drawn if it and all parent Views are visible. To determine if the
    /// containing Window is visible to the user on-screen call is_visible() on
    /// the Window.
    /// </summary>
    function  IsDrawn: boolean;
    /// <summary>
    /// Returns the contents View for this overlay.
    /// </summary>
    property ContentsView   : ICefView          read GetContentsView;
    /// <summary>
    /// Returns the top-level Window hosting this overlay. Use this function
    /// instead of calling get_window() on the contents View.
    /// </summary>
    property Window         : ICefWindow        read GetWindow;
    /// <summary>
    /// Returns the docking mode for this overlay.
    /// </summary>
    property DockingMode    : TCefDockingMode   read GetDockingMode;
    /// <summary>
    /// Returns the bounds (size and position) of this overlay in parent
    /// coordinates.
    /// </summary>
    property Bounds         : TCefRect          read GetBounds           write SetBounds;
    /// <summary>
    /// Returns the bounds (size and position) of this overlay in DIP screen
    /// coordinates.
    /// </summary>
    property BoundsInScreen : TCefRect          read GetBoundsInScreen;
    /// <summary>
    /// Returns the size of this overlay in parent coordinates.
    /// </summary>
    property Size           : TCefSize          read GetSize             write SetSize;
    /// <summary>
    /// Returns the position of this overlay in parent coordinates.
    /// </summary>
    property Position       : TCefPoint         read GetPosition         write SetPosition;
    /// <summary>
    /// Returns the insets for this overlay in parent coordinates.
    /// </summary>
    property Insets         : TCefInsets        read GetInsets           write SetInsets;
    /// <summary>
    /// Returns whether this overlay is visible. A View may be visible but still
    /// not drawn in a Window if any parent Views are hidden. Call is_drawn() to
    /// determine whether this overlay and all parent Views are visible and will
    /// be drawn.
    /// </summary>
    property Visible        : boolean           read IsVisible           write SetVisible;
    /// <summary>
    /// Returns whether this overlay is visible and drawn in a Window. A View is
    /// drawn if it and all parent Views are visible. To determine if the
    /// containing Window is visible to the user on-screen call is_visible() on
    /// the Window.
    /// </summary>
    property Drawn          : boolean           read IsDrawn;
  end;

  /// <summary>
  /// A View is a rectangle within the views View hierarchy. It is the base
  /// interface for all Views. All size and position values are in density
  /// independent pixels (DIP) unless otherwise indicated. Methods must be called
  /// on the browser process UI thread unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefView">Implements TCefView</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_view_capi.h">CEF source file: /include/capi/views/cef_view_capi.h (cef_view_t)</see></para>
  /// </remarks>
  ICefView = interface(ICefBaseRefCounted)
    ['{E9AF950A-F4E8-420C-BD1F-F26F4FDFA48D}']
    /// <summary>
    /// Returns this View as a BrowserView or NULL if this is not a BrowserView.
    /// </summary>
    function  AsBrowserView : ICefBrowserView;
    /// <summary>
    /// Returns this View as a Button or NULL if this is not a Button.
    /// </summary>
    function  AsButton : ICefButton;
    /// <summary>
    /// Returns this View as a Panel or NULL if this is not a Panel.
    /// </summary>
    function  AsPanel : ICefPanel;
    /// <summary>
    /// Returns this View as a ScrollView or NULL if this is not a ScrollView.
    /// </summary>
    function  AsScrollView : ICefScrollView;
    /// <summary>
    /// Returns this View as a Textfield or NULL if this is not a Textfield.
    /// </summary>
    function  AsTextfield : ICefTextfield;
    /// <summary>
    /// Returns the type of this View as a string. Used primarily for testing
    /// purposes.
    /// </summary>
    function  GetTypeString : ustring;
    /// <summary>
    /// Returns a string representation of this View which includes the type and
    /// various type-specific identifying attributes. If |include_children| is
    /// true (1) any child Views will also be included. Used primarily for testing
    /// purposes.
    /// </summary>
    function  ToStringEx(include_children: boolean): ustring;
    /// <summary>
    /// Returns true (1) if this View is valid.
    /// </summary>
    function  IsValid : boolean;
    /// <summary>
    /// Returns true (1) if this View is currently attached to another View. A
    /// View can only be attached to one View at a time.
    /// </summary>
    function  IsAttached : boolean;
    /// <summary>
    /// Returns true (1) if this View is the same as |that| View.
    /// </summary>
    function  IsSame(const that: ICefView): boolean;
    /// <summary>
    /// Returns the delegate associated with this View, if any.
    /// </summary>
    function  GetDelegate : ICefViewDelegate;
    /// <summary>
    /// Returns the top-level Window hosting this View, if any.
    /// </summary>
    function  GetWindow : ICefWindow;
    /// <summary>
    /// Returns the ID for this View.
    /// </summary>
    function  GetID : Integer;
    /// <summary>
    /// Sets the ID for this View. ID should be unique within the subtree that you
    /// intend to search for it. 0 is the default ID for views.
    /// </summary>
    procedure SetID(id_: Integer);
    /// <summary>
    /// Returns the group id of this View, or -1 if not set.
    /// </summary>
    function  GetGroupID : Integer;
    /// <summary>
    /// A group id is used to tag Views which are part of the same logical group.
    /// Focus can be moved between views with the same group using the arrow keys.
    /// The group id is immutable once it's set.
    /// </summary>
    procedure SetGroupID(group_id: Integer);
    /// <summary>
    /// Returns the View that contains this View, if any.
    /// </summary>
    function  GetParentView : ICefView;
    /// <summary>
    /// Recursively descends the view tree starting at this View, and returns the
    /// first child that it encounters with the given ID. Returns NULL if no
    /// matching child view is found.
    /// </summary>
    function  GetViewForID(id_: Integer): ICefView;
    /// <summary>
    /// Sets the bounds (size and position) of this View. |bounds| is in parent
    /// coordinates, or DIP screen coordinates if there is no parent.
    /// </summary>
    procedure SetBounds(const bounds_: TCefRect);
    /// <summary>
    /// Returns the bounds (size and position) of this View in parent coordinates,
    /// or DIP screen coordinates if there is no parent.
    /// </summary>
    function  GetBounds : TCefRect;
    /// <summary>
    /// Returns the bounds (size and position) of this View in DIP screen
    /// coordinates.
    /// </summary>
    function  GetBoundsInScreen : TCefRect;
    /// <summary>
    /// Sets the size of this View without changing the position. |size| in parent
    /// coordinates, or DIP screen coordinates if there is no parent.
    /// </summary>
    procedure SetSize(const size_: TCefSize);
    /// <summary>
    /// Returns the size of this View in parent coordinates, or DIP screen
    /// coordinates if there is no parent.
    /// </summary>
    function  GetSize : TCefSize;
    /// <summary>
    /// Sets the position of this View without changing the size. |position| is in
    /// parent coordinates, or DIP screen coordinates if there is no parent.
    /// </summary>
    procedure SetPosition(const position_: TCefPoint);
    /// <summary>
    /// Returns the position of this View. Position is in parent coordinates, or
    /// DIP screen coordinates if there is no parent.
    /// </summary>
    function  GetPosition : TCefPoint;
    /// <summary>
    /// Sets the insets for this View. |insets| is in parent coordinates, or DIP
    /// screen coordinates if there is no parent.
    /// </summary>
    procedure SetInsets(const insets: TCefInsets);
    /// <summary>
    /// Returns the insets for this View in parent coordinates, or DIP screen
    /// coordinates if there is no parent.
    /// </summary>
    function  GetInsets: TCefInsets;
    /// <summary>
    /// Returns the size this View would like to be if enough space is available.
    /// Size is in parent coordinates, or DIP screen coordinates if there is no
    /// parent.
    /// </summary>
    function  GetPreferredSize : TCefSize;
    /// <summary>
    /// Size this View to its preferred size. Size is in parent coordinates, or
    /// DIP screen coordinates if there is no parent.
    /// </summary>
    procedure SizeToPreferredSize;
    /// <summary>
    /// Returns the minimum size for this View. Size is in parent coordinates, or
    /// DIP screen coordinates if there is no parent.
    /// </summary>
    function  GetMinimumSize : TCefSize;
    /// <summary>
    /// Returns the maximum size for this View. Size is in parent coordinates, or
    /// DIP screen coordinates if there is no parent.
    /// </summary>
    function  GetMaximumSize : TCefSize;
    /// <summary>
    /// Returns the height necessary to display this View with the provided width.
    /// </summary>
    function  GetHeightForWidth(width: Integer): Integer;
    /// <summary>
    /// Indicate that this View and all parent Views require a re-layout. This
    /// ensures the next call to layout() will propagate to this View even if the
    /// bounds of parent Views do not change.
    /// </summary>
    procedure InvalidateLayout;
    /// <summary>
    /// Sets whether this View is visible. Windows are hidden by default and other
    /// views are visible by default. This View and any parent views must be set
    /// as visible for this View to be drawn in a Window. If this View is set as
    /// hidden then it and any child views will not be drawn and, if any of those
    /// views currently have focus, then focus will also be cleared. Painting is
    /// scheduled as needed. If this View is a Window then calling this function
    /// is equivalent to calling the Window show() and hide() functions.
    /// </summary>
    procedure SetVisible(visible_: boolean);
    /// <summary>
    /// Returns whether this View is visible. A view may be visible but still not
    /// drawn in a Window if any parent views are hidden. If this View is a Window
    /// then a return value of true (1) indicates that this Window is currently
    /// visible to the user on-screen. If this View is not a Window then call
    /// is_drawn() to determine whether this View and all parent views are visible
    /// and will be drawn.
    /// </summary>
    function  IsVisible : boolean;
    /// <summary>
    /// Returns whether this View is visible and drawn in a Window. A view is
    /// drawn if it and all parent views are visible. If this View is a Window
    /// then calling this function is equivalent to calling is_visible().
    /// Otherwise, to determine if the containing Window is visible to the user
    /// on-screen call is_visible() on the Window.
    /// </summary>
    function  IsDrawn : boolean;
    /// <summary>
    /// Set whether this View is enabled. A disabled View does not receive
    /// keyboard or mouse inputs. If |enabled| differs from the current value the
    /// View will be repainted. Also, clears focus if the focused View is
    /// disabled.
    /// </summary>
    procedure SetEnabled(enabled_: boolean);
    /// <summary>
    /// Returns whether this View is enabled.
    /// </summary>
    function  IsEnabled : boolean;
    /// <summary>
    /// Sets whether this View is capable of taking focus. It will clear focus if
    /// the focused View is set to be non-focusable. This is false (0) by default
    /// so that a View used as a container does not get the focus.
    /// </summary>
    procedure SetFocusable(focusable_: boolean);
    /// <summary>
    /// Returns true (1) if this View is focusable, enabled and drawn.
    /// </summary>
    function  IsFocusable : boolean;
    /// <summary>
    /// Return whether this View is focusable when the user requires full keyboard
    /// access, even though it may not be normally focusable.
    /// </summary>
    function  IsAccessibilityFocusable : boolean;
    /// <summary>
    /// Request keyboard focus. If this View is focusable it will become the
    /// focused View.
    /// </summary>
    procedure RequestFocus;
    /// <summary>
    /// Sets the background color for this View. The background color will be
    /// automatically reset when ICefViewDelegate.OnThemeChanged is called.
    /// </summary>
    procedure SetBackgroundColor(color: TCefColor);
    /// <summary>
    /// Returns the background color for this View. If the background color is
    /// unset then the current `GetThemeColor(CEF_ColorPrimaryBackground)` value
    /// will be returned. If this View belongs to an overlay (created with
    /// ICefWindow.AddOverlayView), and the background color is unset, then a
    /// value of transparent (0) will be returned.
    /// </summary>
    function  GetBackgroundColor : TCefColor;
    /// <summary>
    /// Returns the current theme color associated with |color_id|, or the
    /// placeholder color (red) if unset. See cef_color_ids.h for standard ID
    /// values. Standard colors can be overridden and custom colors can be added
    /// using ICefWindow.SetThemeColor.
    /// </summary>
    function  GetThemeColor(color_id: integer): TCefColor;
    /// <summary>
    /// Convert |point| from this View's coordinate system to DIP screen
    /// coordinates. This View must belong to a Window when calling this function.
    /// Returns true (1) if the conversion is successful or false (0) otherwise.
    /// Use ICefDisplay.ConvertPointToPixels() after calling this function
    /// if further conversion to display-specific pixel coordinates is desired.
    /// </summary>
    function  ConvertPointToScreen(var point: TCefPoint): boolean;
    /// <summary>
    /// Convert |point| to this View's coordinate system from DIP screen
    /// coordinates. This View must belong to a Window when calling this function.
    /// Returns true (1) if the conversion is successful or false (0) otherwise.
    /// Use ICefDisplay.ConvertPointFromPixels() before calling this
    /// function if conversion from display-specific pixel coordinates is
    /// necessary.
    /// </summary>
    function  ConvertPointFromScreen(var point: TCefPoint): boolean;
    /// <summary>
    /// Convert |point| from this View's coordinate system to that of the Window.
    /// This View must belong to a Window when calling this function. Returns true
    /// (1) if the conversion is successful or false (0) otherwise.
    /// </summary>
    function  ConvertPointToWindow(var point: TCefPoint): boolean;
    /// <summary>
    /// Convert |point| to this View's coordinate system from that of the Window.
    /// This View must belong to a Window when calling this function. Returns true
    /// (1) if the conversion is successful or false (0) otherwise.
    /// </summary>
    function  ConvertPointFromWindow(var point: TCefPoint): boolean;
    /// <summary>
    /// Convert |point| from this View's coordinate system to that of |view|.
    /// |view| needs to be in the same Window but not necessarily the same view
    /// hierarchy. Returns true (1) if the conversion is successful or false (0)
    /// otherwise.
    /// </summary>
    function  ConvertPointToView(const view : ICefView; var point: TCefPoint): boolean;
    /// <summary>
    /// Convert |point| to this View's coordinate system from that |view|. |view|
    /// needs to be in the same Window but not necessarily the same view
    /// hierarchy. Returns true (1) if the conversion is successful or false (0)
    /// otherwise.
    /// </summary>
    function  ConvertPointFromView(const view : ICefView; var point: TCefPoint): boolean;
    /// <summary>
    /// Returns true (1) if this View is valid.
    /// </summary>
    property Valid                  : boolean          read IsValid;
    /// <summary>
    /// Returns true (1) if this View is currently attached to another View. A
    /// View can only be attached to one View at a time.
    /// </summary>
    property Attached               : boolean          read IsAttached;
    /// <summary>
    /// Returns the delegate associated with this View, if any.
    /// </summary>
    property Delegate               : ICefViewDelegate read GetDelegate;
    /// <summary>
    /// Returns the top-level Window hosting this View, if any.
    /// </summary>
    property Window                 : ICefWindow       read GetWindow;
    /// <summary>
    /// Returns the View that contains this View, if any.
    /// </summary>
    property ParentView             : ICefView         read GetParentView;
    /// <summary>
    /// Returns the bounds (size and position) of this View in DIP screen
    /// coordinates.
    /// </summary>
    property BoundsInScreen         : TCefRect         read GetBoundsInScreen;
    /// <summary>
    /// Returns the size this View would like to be if enough space is available.
    /// Size is in parent coordinates, or DIP screen coordinates if there is no
    /// parent.
    /// </summary>
    property PreferredSize          : TCefSize         read GetPreferredSize;
    /// <summary>
    /// Returns the minimum size for this View. Size is in parent coordinates, or
    /// DIP screen coordinates if there is no parent.
    /// </summary>
    property MinimumSize            : TCefSize         read GetMinimumSize;
    /// <summary>
    /// Returns the maximum size for this View. Size is in parent coordinates, or
    /// DIP screen coordinates if there is no parent.
    /// </summary>
    property MaximumSize            : TCefSize         read GetMaximumSize;
    /// <summary>
    /// Returns whether this View is visible. A view may be visible but still not
    /// drawn in a Window if any parent views are hidden. If this View is a Window
    /// then a return value of true (1) indicates that this Window is currently
    /// visible to the user on-screen. If this View is not a Window then call
    /// is_drawn() to determine whether this View and all parent views are visible
    /// and will be drawn.
    /// </summary>
    property Visible                : boolean          read IsVisible                  write SetVisible;
    /// <summary>
    /// Returns whether this View is visible and drawn in a Window. A view is
    /// drawn if it and all parent views are visible. If this View is a Window
    /// then calling this function is equivalent to calling is_visible().
    /// Otherwise, to determine if the containing Window is visible to the user
    /// on-screen call is_visible() on the Window.
    /// </summary>
    property Drawn                  : boolean          read IsDrawn;
    /// <summary>
    /// Returns whether this View is enabled.
    /// </summary>
    property Enabled                : boolean          read IsEnabled                  write SetEnabled;
    /// <summary>
    /// Returns true (1) if this View is focusable, enabled and drawn.
    /// </summary>
    property Focusable              : boolean          read IsFocusable                write SetFocusable;
    /// <summary>
    /// Return whether this View is focusable when the user requires full keyboard
    /// access, even though it may not be normally focusable.
    /// </summary>
    property AccessibilityFocusable : boolean          read IsAccessibilityFocusable;
    /// <summary>
    /// Returns the background color for this View. If the background color is
    /// unset then the current `GetThemeColor(CEF_ColorPrimaryBackground)` value
    /// will be returned. If this View belongs to an overlay (created with
    /// ICefWindow.AddOverlayView), and the background color is unset, then a
    /// value of transparent (0) will be returned.
    /// </summary>
    property BackgroundColor        : TCefColor        read GetBackgroundColor         write SetBackgroundColor;
    /// <summary>
    /// Returns the ID for this View.
    /// </summary>
    property ID                     : integer          read GetID                      write SetID;
    /// <summary>
    /// Returns the group id of this View, or -1 if not set.
    /// </summary>
    property GroupID                : integer          read GetGroupID                 write SetGroupID;
    /// <summary>
    /// Returns the bounds (size and position) of this View in parent coordinates,
    /// or DIP screen coordinates if there is no parent.
    /// </summary>
    property Bounds                 : TCefRect         read GetBounds                  write SetBounds;
    /// <summary>
    /// Returns the size of this View in parent coordinates, or DIP screen
    /// coordinates if there is no parent.
    /// </summary>
    property Size                   : TCefSize         read GetSize                    write SetSize;
    /// <summary>
    /// Returns the position of this View. Position is in parent coordinates, or
    /// DIP screen coordinates if there is no parent.
    /// </summary>
    property Position               : TCefPoint        read GetPosition                write SetPosition;
    /// <summary>
    /// Returns the insets for this View in parent coordinates, or DIP screen
    /// coordinates if there is no parent.
    /// </summary>
    property Insets                 : TCefInsets       read GetInsets                  write SetInsets;
    /// <summary>
    /// Returns the type of this View as a string. Used primarily for testing
    /// purposes.
    /// </summary>
    property TypeString             : ustring          read GetTypeString;
  end;

  /// <summary>
  /// Implement this interface to handle view events. All size and position values
  /// are in density independent pixels (DIP) unless otherwise indicated. The
  /// functions of this interface will be called on the browser process UI thread
  /// unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefViewDelegate">Implements TCefViewDelegate</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_view_delegate_capi.h">CEF source file: /include/capi/views/cef_view_delegate_capi.h (cef_view_delegate_t)</see></para>
  /// </remarks>
  ICefViewDelegate = interface(ICefBaseRefCounted)
    ['{5F900206-B969-4E51-B56C-0FF38D749C72}']
    /// <summary>
    /// Return the preferred size for |view|. The Layout will use this information
    /// to determine the display size.
    /// </summary>
    procedure OnGetPreferredSize(const view: ICefView; var aResult : TCefSize);
    /// <summary>
    /// Return the minimum size for |view|.
    /// </summary>
    procedure OnGetMinimumSize(const view: ICefView; var aResult : TCefSize);
    /// <summary>
    /// Return the maximum size for |view|.
    /// </summary>
    procedure OnGetMaximumSize(const view: ICefView; var aResult : TCefSize);
    /// <summary>
    /// Return the height necessary to display |view| with the provided |width|.
    /// If not specified the result of get_preferred_size().height will be used by
    /// default. Override if |view|'s preferred height depends upon the width (for
    /// example, with Labels).
    /// </summary>
    procedure OnGetHeightForWidth(const view: ICefView; width: Integer; var aResult: Integer);
    /// <summary>
    /// Called when the parent of |view| has changed. If |view| is being added to
    /// |parent| then |added| will be true (1). If |view| is being removed from
    /// |parent| then |added| will be false (0). If |view| is being reparented the
    /// remove notification will be sent before the add notification. Do not
    /// modify the view hierarchy in this callback.
    /// </summary>
    procedure OnParentViewChanged(const view: ICefView; added: boolean; const parent: ICefView);
    /// <summary>
    /// Called when a child of |view| has changed. If |child| is being added to
    /// |view| then |added| will be true (1). If |child| is being removed from
    /// |view| then |added| will be false (0). If |child| is being reparented the
    /// remove notification will be sent to the old parent before the add
    /// notification is sent to the new parent. Do not modify the view hierarchy
    /// in this callback.
    /// </summary>
    procedure OnChildViewChanged(const view: ICefView; added: boolean; const child: ICefView);
    /// <summary>
    /// Called when |view| is added or removed from the ICefWindow.
    /// </summary>
    procedure OnWindowChanged(const view: ICefView; added: boolean);
    /// <summary>
    /// Called when the layout of |view| has changed.
    /// </summary>
    procedure OnLayoutChanged(const view: ICefView; new_bounds: TCefRect);
    /// <summary>
    /// Called when |view| gains focus.
    /// </summary>
    procedure OnFocus(const view: ICefView);
    /// <summary>
    /// Called when |view| loses focus.
    /// </summary>
    procedure OnBlur(const view: ICefView);
    /// <summary>
    /// <para>Called when the theme for |view| has changed, after the new theme colors
    /// have already been applied. Views are notified via the component hierarchy
    /// in depth-first reverse order (children before parents).</para>
    /// <para>This will be called in the following cases:</para>
    /// <code>
    /// 1. When |view|, or a parent of |view|, is added to a Window.
    /// 2. When the native/OS or Chrome theme changes for the Window that contains
    ///    |view|. See ICefWindowDelegate.OnThemeColorsChanged documentation.
    /// 3. When the client explicitly calls ICefWindow.ThemeChanged on the
    ///    Window that contains |view|.
    /// </code>
    /// <para>Optionally use this callback to override the new per-View theme colors by
    /// calling ICefView.SetBackgroundColor or the appropriate component-
    /// specific function. See ICefWindow.SetThemeColor documentation for how
    /// to customize additional Window theme colors.</para>
    /// <summary>
    procedure OnThemeChanged(const view: ICefView);
  end;

  /// <summary>
  /// A Textfield supports editing of text. This control is custom rendered with
  /// no platform-specific code. Methods must be called on the browser process UI
  /// thread unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefTextfield">Implements TCefTextfield</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_textfield_capi.h">CEF source file: /include/capi/views/cef_textfield_capi.h (cef_textfield_t)</see></para>
  /// </remarks>
  ICefTextfield = interface(ICefView)
    ['{B5E30155-DEA5-4CBF-BC9D-578CBCA586D9}']
    /// <summary>
    /// Sets whether the text will be displayed as asterisks.
    /// </summary>
    procedure SetPasswordInput(password_input: boolean);
    /// <summary>
    /// Returns true (1) if the text will be displayed as asterisks.
    /// </summary>
    function  IsPasswordInput : boolean;
    /// <summary>
    /// Sets whether the text will read-only.
    /// </summary>
    procedure SetReadOnly(read_only: boolean);
    /// <summary>
    /// Returns true (1) if the text is read-only.
    /// </summary>
    function  IsReadOnly : boolean;
    /// <summary>
    /// Returns the currently displayed text.
    /// </summary>
    function  GetText : ustring;
    /// <summary>
    /// Sets the contents to |text|. The cursor will be moved to end of the text
    /// if the current position is outside of the text range.
    /// </summary>
    procedure SetText(const text_: ustring);
    /// <summary>
    /// Appends |text| to the previously-existing text.
    /// </summary>
    procedure AppendText(const text_: ustring);
    /// <summary>
    /// Inserts |text| at the current cursor position replacing any selected text.
    /// </summary>
    procedure InsertOrReplaceText(const text_: ustring);
    /// <summary>
    /// Returns true (1) if there is any selected text.
    /// </summary>
    function  HasSelection : boolean;
    /// <summary>
    /// Returns the currently selected text.
    /// </summary>
    function  GetSelectedText : ustring;
    /// <summary>
    /// Selects all text. If |reversed| is true (1) the range will end at the
    /// logical beginning of the text; this generally shows the leading portion of
    /// text that overflows its display area.
    /// </summary>
    procedure SelectAll(reversed: boolean);
    /// <summary>
    /// Clears the text selection and sets the caret to the end.
    /// </summary>
    procedure ClearSelection;
    /// <summary>
    /// Returns the selected logical text range.
    /// </summary>
    function  GetSelectedRange : TCefRange;
    /// <summary>
    /// Selects the specified logical text range.
    /// </summary>
    procedure SelectRange(const range: TCefRange);
    /// <summary>
    /// Returns the current cursor position.
    /// </summary>
    function  GetCursorPosition : NativeUInt;
    /// <summary>
    /// Sets the text color.
    /// </summary>
    procedure SetTextColor(color: TCefColor);
    /// <summary>
    /// Returns the text color.
    /// </summary>
    function  GetTextColor : TCefColor;
    /// <summary>
    /// Sets the selection text color.
    /// </summary>
    procedure SetSelectionTextColor(color: TCefColor);
    /// <summary>
    /// Returns the selection text color.
    /// </summary>
    function  GetSelectionTextColor : TCefColor;
    /// <summary>
    /// Sets the selection background color.
    /// </summary>
    procedure SetSelectionBackgroundColor(color: TCefColor);
    /// <summary>
    /// Returns the selection background color.
    /// </summary>
    function  GetSelectionBackgroundColor : TCefColor;
    /// <summary>
    /// Sets the font list. The format is "<FONT_FAMILY_LIST>,[STYLES] <SIZE>",
    /// where:
    /// - FONT_FAMILY_LIST is a comma-separated list of font family names,
    /// - STYLES is an optional space-separated list of style names (case-sensitive
    ///   "Bold" and "Italic" are supported), and
    /// - SIZE is an integer font size in pixels with the suffix "px".
    ///
    /// Here are examples of valid font description strings:
    /// - "Arial, Helvetica, Bold Italic 14px"
    /// - "Arial, 14px"
    /// </summary>
    procedure SetFontList(const font_list: ustring);
    /// <summary>
    /// Applies |color| to the specified |range| without changing the default
    /// color. If |range| is NULL the color will be set on the complete text
    /// contents.
    /// </summary>
    procedure ApplyTextColor(color: TCefColor; const range: TCefRange);
    /// <summary>
    /// Applies |style| to the specified |range| without changing the default
    /// style. If |add| is true (1) the style will be added, otherwise the style
    /// will be removed. If |range| is NULL the style will be set on the complete
    /// text contents.
    /// </summary>
    procedure ApplyTextStyle(style: TCefTextStyle; add: boolean; const range: TCefRange);
    /// <summary>
    /// Returns true (1) if the action associated with the specified command id is
    /// enabled. See additional comments on execute_command().
    /// </summary>
    function  IsCommandEnabled(command_id: TCefTextFieldCommands): boolean;
    /// <summary>
    /// Performs the action associated with the specified command id.
    /// </summary>
    procedure ExecuteCommand(command_id: TCefTextFieldCommands);
    /// <summary>
    /// Clears Edit history.
    /// </summary>
    procedure ClearEditHistory;
    /// <summary>
    /// Sets the placeholder text that will be displayed when the Textfield is
    /// NULL.
    /// </summary>
    procedure SetPlaceholderText(const text_: ustring);
    /// <summary>
    /// Returns the placeholder text that will be displayed when the Textfield is
    /// NULL.
    /// </summary>
    function  GetPlaceholderText : ustring;
    /// <summary>
    /// Sets the placeholder text color.
    /// </summary>
    procedure SetPlaceholderTextColor(color: TCefColor);
    /// <summary>
    /// Set the accessible name that will be exposed to assistive technology (AT).
    /// </summary>
    procedure SetAccessibleName(const name: ustring);
    /// <summary>
    /// Returns true (1) if the text will be displayed as asterisks.
    /// </summary>
    property  PasswordInput            : boolean       read IsPasswordInput               write SetPasswordInput;
    /// <summary>
    /// Returns true (1) if the text is read-only.
    /// </summary>
    property  ReadOnly                 : boolean       read IsReadOnly                    write SetReadOnly;
    /// <summary>
    /// Returns the currently displayed text.
    /// </summary>
    property  Text                     : ustring       read GetText                       write SetText;
    /// <summary>
    /// Returns the currently selected text.
    /// </summary>
    property  SelectedText             : ustring       read GetSelectedText;
    /// <summary>
    /// Returns the text color.
    /// </summary>
    property  TextColor                : TCefColor     read GetTextColor                  write SetTextColor;
    /// <summary>
    /// Returns the selection text color.
    /// </summary>
    property  SelectionTextColor       : TCefColor     read GetSelectionTextColor         write SetSelectionTextColor;
    /// <summary>
    /// Returns the selection background color.
    /// </summary>
    property  SelectionBackgroundColor : TCefColor     read GetSelectionBackgroundColor   write SetSelectionBackgroundColor;
    /// <summary>
    /// Returns the placeholder text that will be displayed when the Textfield is
    /// NULL.
    /// </summary>
    property  PlaceholderText          : ustring       read GetPlaceholderText            write SetPlaceholderText;
  end;

  /// <summary>
  /// Implement this interface to handle Textfield events. The functions of this
  /// interface will be called on the browser process UI thread unless otherwise
  /// indicated.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefTextfieldDelegate">Implements TCefTextfieldDelegate</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_textfield_delegate_capi.h">CEF source file: /include/capi/views/cef_textfield_delegate_capi.h (cef_textfield_delegate_t)</see></para>
  /// </remarks>
  ICefTextfieldDelegate = interface(ICefViewDelegate)
    ['{72612994-92BB-4DE9-BB38-6F49FB45F94B}']
    /// <summary>
    /// Called when |textfield| receives a keyboard event. |event| contains
    /// information about the keyboard event. Return true (1) if the keyboard
    /// event was handled or false (0) otherwise for default handling.
    /// </summary>
    procedure OnKeyEvent(const textfield: ICefTextfield; const event: TCefKeyEvent; var aResult : boolean);
    /// <summary>
    /// Called after performing a user action that may change |textfield|.
    /// </summary>
    procedure OnAfterUserAction(const textfield: ICefTextfield);
  end;

  /// <summary>
  /// A ScrollView will show horizontal and/or vertical scrollbars when necessary
  /// based on the size of the attached content view. Methods must be called on
  /// the browser process UI thread unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefScrollView">Implements TCefScrollView</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_scroll_view_capi.h">CEF source file: /include/capi/views/cef_scroll_view_capi.h (cef_scroll_view_t)</see></para>
  /// </remarks>
  ICefScrollView = interface(ICefView)
    ['{55DF2883-0574-4F10-B6F5-DE4730964B5B}']
    /// <summary>
    /// Set the content View. The content View must have a specified size (e.g.
    /// via ICefView.SetBounds or ICefViewDelegate.GetPreferredSize).
    /// </summary>
    procedure SetContentView(const view: ICefView);
    /// <summary>
    /// Returns the content View.
    /// </summary>
    function  GetContentView : ICefView;
    /// <summary>
    /// Returns the visible region of the content View.
    /// </summary>
    function  GetVisibleContentRect : TCefRect;
    /// <summary>
    /// Returns true (1) if the horizontal scrollbar is currently showing.
    /// </summary>
    function  HasHorizontalScrollbar : boolean;
    /// <summary>
    /// Returns the height of the horizontal scrollbar.
    /// </summary>
    function  GetHorizontalScrollbarHeight : Integer;
    /// <summary>
    /// Returns true (1) if the vertical scrollbar is currently showing.
    /// </summary>
    function  HasVerticalScrollbar : boolean;
    /// <summary>
    /// Returns the width of the vertical scrollbar.
    /// </summary>
    function  GetVerticalScrollbarWidth : Integer;
    /// <summary>
    /// Returns the content View.
    /// </summary>
    property  ContentView               : ICefView      read GetContentView                write SetContentView;
    /// <summary>
    /// Returns the visible region of the content View.
    /// </summary>
    property  VisibleContentRect        : TCefRect      read GetVisibleContentRect;
    /// <summary>
    /// Returns the height of the horizontal scrollbar.
    /// </summary>
    property  HorizontalScrollbarHeight : Integer       read GetHorizontalScrollbarHeight;
    /// <summary>
    /// Returns the width of the vertical scrollbar.
    /// </summary>
    property  VerticalScrollbarWidth    : Integer       read GetVerticalScrollbarWidth;
  end;

  /// <summary>
  /// A Panel is a container in the views hierarchy that can contain other Views
  /// as children. Methods must be called on the browser process UI thread unless
  /// otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefPanel">Implements TCefPanel</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_panel_capi.h">CEF source file: /include/capi/views/cef_panel_capi.h (cef_panel_t)</see></para>
  /// </remarks>
  ICefPanel = interface(ICefView)
    ['{6F2F680A-3637-4438-81B8-79AD6C02252D}']
    /// <summary>
    /// Returns this Panel as a Window or NULL if this is not a Window.
    /// </summary>
    function  GetAsWindow : ICefWindow;
    /// <summary>
    /// Set this Panel's Layout to FillLayout and return the FillLayout object.
    /// </summary>
    function  SetToFillLayout : ICefFillLayout;
    /// <summary>
    /// Set this Panel's Layout to BoxLayout and return the BoxLayout object.
    /// </summary>
    function  SetToBoxLayout(const settings: TCefBoxLayoutSettings): ICefBoxLayout;
    /// <summary>
    /// Get the Layout.
    /// </summary>
    function  GetLayout : ICefLayout;
    /// <summary>
    /// Lay out the child Views (set their bounds based on sizing heuristics
    /// specific to the current Layout).
    /// </summary>
    procedure Layout;
    /// <summary>
    /// Add a child View.
    /// </summary>
    procedure AddChildView(const view: ICefView);
    /// <summary>
    /// Add a child View at the specified |index|. If |index| matches the result
    /// of GetChildCount() then the View will be added at the end.
    /// </summary>
    procedure AddChildViewAt(const view: ICefView; index: Integer);
    /// <summary>
    /// Move the child View to the specified |index|. A negative value for |index|
    /// will move the View to the end.
    /// </summary>
    procedure ReorderChildView(const view: ICefView; index: Integer);
    /// <summary>
    /// Remove a child View. The View can then be added to another Panel.
    /// </summary>
    procedure RemoveChildView(const view: ICefView);
    /// <summary>
    /// Remove all child Views. The removed Views will be deleted if the client
    /// holds no references to them.
    /// </summary>
    procedure RemoveAllChildViews;
    /// <summary>
    /// Returns the number of child Views.
    /// </summary>
    function  GetChildViewCount : NativeUInt;
    /// <summary>
    /// Returns the child View at the specified |index|.
    /// </summary>
    function  GetChildViewAt(index: Integer): ICefView;
    /// <summary>
    /// Returns this Panel as a Window or NULL if this is not a Window.
    /// </summary>
    property AsWindow : ICefWindow    read GetAsWindow;
  end;

  /// <summary>
  /// Implement this interface to handle Panel events. The functions of this
  /// interface will be called on the browser process UI thread unless otherwise
  /// indicated.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefPanelDelegate">Implements TCefPanelDelegate</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_panel_delegate_capi.h">CEF source file: /include/capi/views/cef_panel_delegate_capi.h (cef_panel_delegate_t)</see></para>
  /// </remarks>
  ICefPanelDelegate = interface(ICefViewDelegate)
    ['{305D453F-FEBA-48ED-AE33-5D978823EA96}']
  end;

  /// <summary>
  /// A View hosting a ICefBrowser instance. Methods must be called on the
  /// browser process UI thread unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefBrowserView">Implements TCefBrowserView</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_browser_view_capi.h">CEF source file: /include/capi/views/cef_browser_view_capi.h (cef_browser_view_t)</see></para>
  /// </remarks>
  ICefBrowserView = interface(ICefView)
    ['{A617EE5D-B933-4E14-9FC0-7E88E9B6C051}']
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
    /// <summary>
    /// Returns the runtime style for this BrowserView (ALLOY or CHROME). See
    /// TCefRuntimeStyle documentation for details.
    /// </summary>
    property RuntimeStyle : TCefRuntimeStyle read GetRuntimeStyle;
  end;

  /// <summary>
  /// Implement this interface to handle BrowserView events. The functions of this
  /// interface will be called on the browser process UI thread unless otherwise
  /// indicated.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefBrowserViewDelegate">Implements TCefBrowserViewDelegate</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_browser_view_delegate_capi.h">CEF source file: /include/capi/views/cef_browser_view_delegate_capi.h (cef_browser_view_delegate_t)</see></para>
  /// </remarks>
  ICefBrowserViewDelegate = interface(ICefViewDelegate)
    ['{578A0DD4-2E7D-4061-B4DB-7C3CDC7A90C0}']
    /// <summary>
    /// Called when |browser| associated with |browser_view| is created. This
    /// function will be called after ICefLifeSpanHandler.OnAfterCreated()
    /// is called for |browser| and before OnPopupBrowserViewCreated() is
    /// called for |browser|'s parent delegate if |browser| is a popup.
    /// </summary>
    procedure OnBrowserCreated(const browser_view: ICefBrowserView; const browser: ICefBrowser);
    /// <summary>
    /// Called when |browser| associated with |browser_view| is destroyed. Release
    /// all references to |browser| and do not attempt to execute any functions on
    /// |browser| after this callback returns. This function will be called before
    /// ICefLifeSpanHandler.OnBeforeClose() is called for |browser|.
    /// </summary>
    procedure OnBrowserDestroyed(const browser_view: ICefBrowserView; const browser: ICefBrowser);
    /// <summary>
    /// Called before a new popup BrowserView is created. The popup originated
    /// from |browser_view|. |settings| and |client| are the values returned from
    /// ICefLifeSpanHandler.OnBeforePopup(). |is_devtools| will be true (1)
    /// if the popup will be a DevTools browser. Return the delegate that will be
    /// used for the new popup BrowserView.
    /// </summary>
    procedure OnGetDelegateForPopupBrowserView(const browser_view: ICefBrowserView; const settings: TCefBrowserSettings; const client: ICefClient; is_devtools: boolean; var aResult : ICefBrowserViewDelegate);
    /// <summary>
    /// Called after |popup_browser_view| is created. This function will be called
    /// after ICefLifeSpanHandler.OnAfterCreated() and OnBrowserCreated()
    /// are called for the new popup browser. The popup originated from
    /// |browser_view|. |is_devtools| will be true (1) if the popup is a DevTools
    /// browser. Optionally add |popup_browser_view| to the views hierarchy
    /// yourself and return true (1). Otherwise return false (0) and a default
    /// ICefWindow will be created for the popup.
    /// </summary>
    procedure OnPopupBrowserViewCreated(const browser_view, popup_browser_view: ICefBrowserView; is_devtools: boolean; var aResult : boolean);
    /// <summary>
    /// Returns the Chrome toolbar type that will be available via
    /// ICefBrowserView.GetChromeToolbar(). See that function for related
    /// documentation.
    /// </summary>
    procedure OnGetChromeToolbarType(const browser_view: ICefBrowserView; var aResult : TCefChromeToolbarType);
    /// <summary>
    /// Return true (1) to create frameless windows for Document picture-in-
    /// picture popups. Content in frameless windows should specify draggable
    /// regions using "-webkit-app-region: drag" CSS.
    /// </summary>
    procedure OnUseFramelessWindowForPictureInPicture(const browser_view: ICefBrowserView; var aResult: boolean);
    /// <summary>
    /// Called when |browser_view| receives a gesture command. Return true (1) to
    /// handle (or disable) a |gesture_command| or false (0) to propagate the
    /// gesture to the browser for default handling. With Chrome style these
    /// commands can also be handled via ICefCommandHandler.OnChromeCommand.
    /// </summary>
    procedure OnGestureCommand(const browser_view: ICefBrowserView; gesture_command: TCefGestureCommand; var aResult : boolean);
    /// <summary>
    /// Optionally change the runtime style for this BrowserView. See
    /// TCefRuntimeStyle documentation for details.
    /// </summary>
    procedure OnGetBrowserRuntimeStyle(var aResult : TCefRuntimeStyle);
  end;

  /// <summary>
  /// A View representing a button. Depending on the specific type, the button
  /// could be implemented by a native control or custom rendered. Methods must be
  /// called on the browser process UI thread unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefButton">Implements TCefButton</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_button_capi.h">CEF source file: /include/capi/views/cef_button_capi.h (cef_button_t)</see></para>
  /// </remarks>
  ICefButton = interface(ICefView)
    ['{D3D2E8A0-9F9C-4BD8-B495-655976534281}']
    /// <summary>
    /// Returns this Button as a LabelButton or NULL if this is not a LabelButton.
    /// </summary>
    function  AsLabelButton : ICefLabelButton;
    /// <summary>
    /// Sets the current display state of the Button.
    /// </summary>
    procedure SetState(state_: TCefButtonState);
    /// <summary>
    /// Returns the current display state of the Button.
    /// </summary>
    function  GetState : TCefButtonState;
    /// <summary>
    /// Sets the Button will use an ink drop effect for displaying state changes.
    /// </summary>
    procedure SetInkDropEnabled(enabled_: boolean);
    /// <summary>
    /// Sets the tooltip text that will be displayed when the user hovers the
    /// mouse cursor over the Button.
    /// </summary>
    procedure SetTooltipText(const tooltip_text: ustring);
    /// <summary>
    /// Sets the accessible name that will be exposed to assistive technology
    /// (AT).
    /// </summary>
    procedure SetAccessibleName(const name: ustring);

    property  State : TCefButtonState read GetState write SetState;
  end;

  /// <summary>
  /// Implement this interface to handle Button events. The functions of this
  /// interface will be called on the browser process UI thread unless otherwise
  /// indicated.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefButtonDelegate">Implements TCefButtonDelegate</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_button_delegate_capi.h">CEF source file: /include/capi/views/cef_button_delegate_capi.h (cef_button_delegate_t)</see></para>
  /// </remarks>
  ICefButtonDelegate = interface(ICefViewDelegate)
    ['{EA1EB5A4-DFB0-4A13-A23B-54FAF9401B39}']
    /// <summary>
    /// Called when |button| is pressed.
    /// </summary>
    procedure OnButtonPressed(const button: ICefButton);
    /// <summary>
    /// Called when the state of |button| changes.
    /// </summary>
    procedure OnButtonStateChanged(const button: ICefButton);
  end;

  /// <summary>
  /// LabelButton is a button with optional text and/or icon. Methods must be
  /// called on the browser process UI thread unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefLabelButton">Implements TCefLabelButton</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_label_button_capi.h">CEF source file: /include/capi/views/cef_label_button_capi.h (cef_label_button_t)</see></para>
  /// </remarks>
  ICefLabelButton = interface(ICefButton)
    ['{A99FD4F3-7EE6-4796-8BF6-EC367D51EED8}']
    /// <summary>
    /// Returns this LabelButton as a MenuButton or NULL if this is not a
    /// MenuButton.
    /// </summary>
    function  AsMenuButton : ICefMenuButton;
    /// <summary>
    /// Sets the text shown on the LabelButton. By default |text| will also be
    /// used as the accessible name.
    /// </summary>
    procedure SetText(const text_: ustring);
    /// <summary>
    /// Returns the text shown on the LabelButton.
    /// </summary>
    function  GetText : ustring;
    /// <summary>
    /// Sets the image shown for |button_state|. When this Button is drawn if no
    /// image exists for the current state then the image for
    /// CEF_BUTTON_STATE_NORMAL, if any, will be shown.
    /// </summary>
    procedure SetImage(button_state: TCefButtonState; const image: ICefImage);
    /// <summary>
    /// Returns the image shown for |button_state|. If no image exists for that
    /// state then the image for CEF_BUTTON_STATE_NORMAL will be returned.
    /// </summary>
    function  GetImage(button_state: TCefButtonState): ICefImage;
    /// <summary>
    /// Sets the text color shown for the specified button |for_state| to |color|.
    /// </summary>
    procedure SetTextColor(for_state: TCefButtonState; color: TCefColor);
    /// <summary>
    /// Sets the text colors shown for the non-disabled states to |color|.
    /// </summary>
    procedure SetEnabledTextColors(color: TCefColor);
    /// <summary>
    /// Sets the font list. The format is "<FONT_FAMILY_LIST>,[STYLES] <SIZE>",
    /// where:
    /// - FONT_FAMILY_LIST is a comma-separated list of font family names,
    /// - STYLES is an optional space-separated list of style names (case-sensitive
    ///   "Bold" and "Italic" are supported), and
    /// - SIZE is an integer font size in pixels with the suffix "px".
    ///
    /// Here are examples of valid font description strings:
    /// - "Arial, Helvetica, Bold Italic 14px"
    /// - "Arial, 14px"
    /// </summary>
    procedure SetFontList(const font_list: ustring);
    /// <summary>
    /// Sets the horizontal alignment; reversed in RTL. Default is
    /// CEF_HORIZONTAL_ALIGNMENT_CENTER.
    /// </summary>
    procedure SetHorizontalAlignment(alignment: TCefHorizontalAlignment);
    /// <summary>
    /// Reset the minimum size of this LabelButton to |size|.
    /// </summary>
    procedure SetMinimumSize(const size_: TCefSize);
    /// <summary>
    /// Reset the maximum size of this LabelButton to |size|.
    /// </summary>
    procedure SetMaximumSize(const size_: TCefSize);
    /// <summary>
    /// Returns the text shown on the LabelButton.
    /// </summary>
    property  Text  : ustring       read GetText write SetText;
  end;

  /// <summary>
  /// MenuButton is a button with optional text, icon and/or menu marker that
  /// shows a menu when clicked with the left mouse button. All size and position
  /// values are in density independent pixels (DIP) unless otherwise indicated.
  /// Methods must be called on the browser process UI thread unless otherwise
  /// indicated.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefMenuButton">Implements TCefMenuButton</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_menu_button_capi.h">CEF source file: /include/capi/views/cef_menu_button_capi.h (cef_menu_button_t)</see></para>
  /// </remarks>
  ICefMenuButton = interface(ICefLabelButton)
    ['{62BFE81A-7810-400B-83C6-76D1DF133710}']
    /// <summary>
    /// Show a menu with contents |menu_model|. |screen_point| specifies the menu
    /// position in screen coordinates. |anchor_position| specifies how the menu
    /// will be anchored relative to |screen_point|. This function should be
    /// called from ICefMenuButtonDelegate.OnMenuButtonPressed().
    /// </summary>
    procedure ShowMenu(const menu_model: ICefMenuModel; const screen_point: TCefPoint; anchor_position: TCefMenuAnchorPosition);
    /// <summary>
    /// Show the menu for this button. Results in a call to
    /// ICefMenuButtonDelegate.OnMenuButtonPressed().
    /// </summary>
    procedure TriggerMenu;
  end;

  /// <summary>
  /// MenuButton pressed lock is released when this object is destroyed.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefMenuButtonPressedLock">Implements TCefMenuButtonPressedLock</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_menu_button_delegate_capi.h">CEF source file: /include/capi/views/cef_menu_button_delegate_capi.h (cef_menu_button_pressed_lock_t)</see></para>
  /// </remarks>
  ICefMenuButtonPressedLock = interface(ICefBaseRefCounted)
    ['{71498C53-0B1D-4A05-98A0-3E589F2A1683}']
  end;

  /// <summary>
  /// Implement this interface to handle MenuButton events. The functions of this
  /// interface will be called on the browser process UI thread unless otherwise
  /// indicated.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefMenuButtonDelegate">Implements TCefMenuButtonDelegate</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_menu_button_delegate_capi.h">CEF source file: /include/capi/views/cef_menu_button_delegate_capi.h (cef_menu_button_delegate_t)</see></para>
  /// </remarks>
  ICefMenuButtonDelegate = interface(ICefButtonDelegate)
    ['{D0E89A75-463A-4766-8701-BD8D24B11E9F}']
    /// <summary>
    /// Called when |button| is pressed. Call ICefMenuButton.ShowMenu() to
    /// show a popup menu at |screen_point|. When showing a custom popup such as a
    /// window keep a reference to |button_pressed_lock| until the popup is hidden
    /// to maintain the pressed button state.
    /// </summary>
    procedure OnMenuButtonPressed(const menu_button: ICefMenuButton; const screen_point: TCefPoint; const button_pressed_lock: ICefMenuButtonPressedLock);
  end;

  /// <summary>
  /// A Window is a top-level Window/widget in the Views hierarchy. By default it
  /// will have a non-client area with title bar, icon and buttons that supports
  /// moving and resizing. All size and position values are in density independent
  /// pixels (DIP) unless otherwise indicated. Methods must be called on the
  /// browser process UI thread unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefWindow">Implements TCefWindow</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_window_capi.h">CEF source file: /include/capi/views/cef_window_capi.h (cef_window_t)</see></para>
  /// </remarks>
  ICefWindow = interface(ICefPanel)
    ['{C450C974-BF0A-4968-A6BE-153CEAD10DA6}']
    /// <summary>
    /// Show the Window.
    /// </summary>
    procedure Show;
    /// <summary>
    /// Show the Window as a browser modal dialog relative to |browser_view|. A
    /// parent Window must be returned via
    /// ICefWindowDelegate.GetParentWindow() and |browser_view| must belong
    /// to that parent Window. While this Window is visible, |browser_view| will
    /// be disabled while other controls in the parent Window remain enabled.
    /// Navigating or destroying the |browser_view| will close this Window
    /// automatically. Alternately, use show() and return true (1) from
    /// ICefWindowDelegate.IsWindowModalDialog() for a window modal dialog
    /// where all controls in the parent Window are disabled.
    /// </summary>
    procedure ShowAsBrowserModalDialog(const browser_view: ICefBrowserView);
    /// <summary>
    /// Hide the Window.
    /// </summary>
    procedure Hide;
    /// <summary>
    /// Sizes the Window to |size| and centers it in the current display.
    /// </summary>
    procedure CenterWindow(const size_: TCefSize);
    /// <summary>
    /// Close the Window.
    /// </summary>
    procedure Close;
    /// <summary>
    /// Returns true (1) if the Window has been closed.
    /// </summary>
    function  IsClosed : boolean;
    /// <summary>
    /// Activate the Window, assuming it already exists and is visible.
    /// </summary>
    procedure Activate;
    /// <summary>
    /// Deactivate the Window, making the next Window in the Z order the active
    /// Window.
    /// </summary>
    procedure Deactivate;
    /// <summary>
    /// Returns whether the Window is the currently active Window.
    /// </summary>
    function  IsActive : boolean;
    /// <summary>
    /// Bring this Window to the top of other Windows in the Windowing system.
    /// </summary>
    procedure BringToTop;
    /// <summary>
    /// Set the Window to be on top of other Windows in the Windowing system.
    /// </summary>
    procedure SetAlwaysOnTop(on_top: boolean);
    /// <summary>
    /// Returns whether the Window has been set to be on top of other Windows in
    /// the Windowing system.
    /// </summary>
    function  IsAlwaysOnTop : boolean;
    /// <summary>
    /// Maximize the Window.
    /// </summary>
    procedure Maximize;
    /// <summary>
    /// Minimize the Window.
    /// </summary>
    procedure Minimize;
    /// <summary>
    /// Restore the Window.
    /// </summary>
    procedure Restore;
    /// <summary>
    /// Set fullscreen Window state. The
    /// ICefWindowDelegate.OnWindowFullscreenTransition function will be
    /// called during the fullscreen transition for notification purposes.
    /// </summary>
    procedure SetFullscreen(fullscreen: boolean);
    /// <summary>
    /// Returns true (1) if the Window is maximized.
    /// </summary>
    function  IsMaximized : boolean;
    /// <summary>
    /// Returns true (1) if the Window is minimized.
    /// </summary>
    function  IsMinimized : boolean;
    /// <summary>
    /// Returns true (1) if the Window is fullscreen.
    /// </summary>
    function  IsFullscreen : boolean;
    /// <summary>
    /// Set the Window title.
    /// </summary>
    procedure SetTitle(const title_: ustring);
    /// <summary>
    /// Get the Window title.
    /// </summary>
    function  GetTitle : ustring;
    /// <summary>
    /// Set the Window icon. This should be a 16x16 icon suitable for use in the
    /// Windows's title bar.
    /// </summary>
    procedure SetWindowIcon(const image: ICefImage);
    /// <summary>
    /// Get the Window icon.
    /// </summary>
    function  GetWindowIcon : ICefImage;
    /// <summary>
    /// Set the Window App icon. This should be a larger icon for use in the host
    /// environment app switching UI. On Windows, this is the ICON_BIG used in
    /// Alt-Tab list and Windows taskbar. The Window icon will be used by default
    /// if no Window App icon is specified.
    /// </summary>
    procedure SetWindowAppIcon(const image: ICefImage);
    /// <summary>
    /// Get the Window App icon.
    /// </summary>
    function  GetWindowAppIcon : ICefImage;
    /// <summary>
    /// Add a View that will be overlayed on the Window contents with absolute
    /// positioning and high z-order. Positioning is controlled by |docking_mode|
    /// as described below. Setting |can_activate| to true (1) will allow the
    /// overlay view to receive input focus. The returned cef_overlay_controller_t
    /// object is used to control the overlay. Overlays are hidden by default.
    ///
    /// With CEF_DOCKING_MODE_CUSTOM:
    ///   1. The overlay is initially hidden, sized to |view|'s preferred size,
    ///      and positioned in the top-left corner.
    ///   2. Optionally change the overlay position and/or size by calling
    ///      CefOverlayController methods.
    ///   3. Call ICefOverlayController.SetVisible(true) to show the overlay.
    ///   4. The overlay will be automatically re-sized if |view|'s layout
    ///      changes. Optionally change the overlay position and/or size when
    ///      OnLayoutChanged is called on the Window's delegate to indicate a
    ///      change in Window bounds.
    ///
    /// With other docking modes:
    ///   1. The overlay is initially hidden, sized to |view|'s preferred size,
    ///      and positioned based on |docking_mode|.
    ///   2. Call ICefOverlayController.SetVisible(true) to show the overlay.
    ///   3. The overlay will be automatically re-sized if |view|'s layout changes
    ///      and re-positioned as appropriate when the Window resizes.
    ///
    /// Overlays created by this function will receive a higher z-order then any
    /// child Views added previously. It is therefore recommended to call this
    /// function last after all other child Views have been added so that the
    /// overlay displays as the top-most child of the Window.
    /// </summary>
    function  AddOverlayView(const view: ICefView; docking_mode: TCefDockingMode; can_activate: boolean): ICefOverlayController;
    /// <summary>
    /// Show a menu with contents |menu_model|. |screen_point| specifies the menu
    /// position in screen coordinates. |anchor_position| specifies how the menu
    /// will be anchored relative to |screen_point|.
    /// </summary>
    procedure ShowMenu(const menu_model: ICefMenuModel; const screen_point: TCefPoint; anchor_position : TCefMenuAnchorPosition);
    /// <summary>
    /// Cancel the menu that is currently showing, if any.
    /// </summary>
    procedure CancelMenu;
    /// <summary>
    /// Returns the Display that most closely intersects the bounds of this
    /// Window. May return NULL if this Window is not currently displayed.
    /// </summary>
    function  GetDisplay : ICefDisplay;
    /// <summary>
    /// Returns the bounds (size and position) of this Window's client area.
    /// Position is in screen coordinates.
    /// </summary>
    function  GetClientAreaBoundsInScreen : TCefRect;
    /// <summary>
    /// Set the regions where mouse events will be intercepted by this Window to
    /// support drag operations. Call this function with an NULL vector to clear
    /// the draggable regions. The draggable region bounds should be in window
    /// coordinates.
    /// </summary>
    procedure SetDraggableRegions(regionsCount: NativeUInt; const regions: PCefDraggableRegionArray);
    /// <summary>
    /// Retrieve the platform window handle for this Window.
    /// </summary>
    function  GetWindowHandle : TCefWindowHandle;
    /// <summary>
    /// Simulate a key press. |key_code| is the VKEY_* value from Chromium's
    /// ui/events/keycodes/keyboard_codes.h header (VK_* values on Windows).
    /// |event_flags| is some combination of EVENTFLAG_SHIFT_DOWN,
    /// EVENTFLAG_CONTROL_DOWN and/or EVENTFLAG_ALT_DOWN. This function is exposed
    /// primarily for testing purposes.
    /// </summary>
    procedure SendKeyPress(key_code: Integer; event_flags: cardinal);
    /// <summary>
    /// Simulate a mouse move. The mouse cursor will be moved to the specified
    /// (screen_x, screen_y) position. This function is exposed primarily for
    /// testing purposes.
    /// </summary>
    procedure SendMouseMove(screen_x, screen_y: Integer);
    /// <summary>
    /// Simulate mouse down and/or mouse up events. |button| is the mouse button
    /// type. If |mouse_down| is true (1) a mouse down event will be sent. If
    /// |mouse_up| is true (1) a mouse up event will be sent. If both are true (1)
    /// a mouse down event will be sent followed by a mouse up event (equivalent
    /// to clicking the mouse button). The events will be sent using the current
    /// cursor position so make sure to call send_mouse_move() first to position
    /// the mouse. This function is exposed primarily for testing purposes.
    /// </summary>
    procedure SendMouseEvents(button: TCefMouseButtonType; mouse_down, mouse_up: boolean);
    /// <summary>
    /// <para>Set the keyboard accelerator for the specified |command_id|. |key_code|
    /// can be any virtual key or character value. Required modifier keys are
    /// specified by |shift_pressed|, |ctrl_pressed| and/or |alt_pressed|.
    /// ICefWindowDelegate.OnAccelerator will be called if the keyboard
    /// combination is triggered while this window has focus.</para>
    /// <para>The |high_priority| value will be considered if a child ICefBrowserView
    /// has focus when the keyboard combination is triggered. If |high_priority|
    /// is true (1) then the key event will not be forwarded to the web content
    /// (`keydown` event handler) or ICefKeyboardHandler first. If
    /// |high_priority| is false (0) then the behavior will depend on the
    /// ICefBrowserView.SetPreferAccelerators configuration.</para>
    /// </summary>
    procedure SetAccelerator(command_id, key_code : Integer; shift_pressed, ctrl_pressed, alt_pressed, high_priority: boolean);
    /// <summary>
    /// Remove the keyboard accelerator for the specified |command_id|.
    /// </summary>
    procedure RemoveAccelerator(command_id: Integer);
    /// <summary>
    /// Remove all keyboard accelerators.
    /// </summary>
    procedure RemoveAllAccelerators;
    /// <summary>
    /// <para>Override a standard theme color or add a custom color associated with
    /// |color_id|. See cef_color_ids.h for standard ID values. Recommended usage
    /// is as follows:</para>
    /// <code>
    /// 1. Customize the default native/OS theme by calling SetThemeColor before
    ///    showing the first Window. When done setting colors call
    ///    ICefWindow.ThemeChanged to trigger ICefViewDelegate.OnThemeChanged
    ///    notifications.
    /// 2. Customize the current native/OS or Chrome theme after it changes by
    ///    calling SetThemeColor from the ICefWindowDelegate.OnThemeColorsChanged
    ///    callback. ICefViewDelegate.OnThemeChanged notifications will then be
    ///    triggered automatically.
    /// </code>
    /// <para>The configured color will be available immediately via
    /// ICefView.GetThemeColor and will be applied to each View in this
    /// Window's component hierarchy when ICefViewDelegate.OnThemeChanged is
    /// called. See OnThemeColorsChanged documentation for additional details.</para>
    /// <para>Clients wishing to add custom colors should use |color_id| values >=
    /// CEF_ChromeColorsEnd.</para>
    /// </summary>
    procedure SetThemeColor(color_id: integer; color: TCefColor);
    /// <summary>
    /// <para>Trigger ICefViewDelegate.OnThemeChanged callbacks for each View in
    /// this Window's component hierarchy. Unlike a native/OS or Chrome theme
    /// change this function does not reset theme colors to standard values and
    /// does not result in a call to ICefWindowDelegate.OnThemeColorsChanged.</para>
    /// <para>Do not call this function from ICefWindowDelegate.OnThemeColorsChanged
    /// or ICefViewDelegate.OnThemeChanged.</para>
    /// </summary>
    procedure ThemeChanged;
    /// <summary>
    /// Returns the runtime style for this Window (ALLOY or CHROME). See
    /// TCefRuntimeStyle documentation for details.
    /// </summary>
    function GetRuntimeStyle: TCefRuntimeStyle;

    /// <summary>
    /// Get the Window title.
    /// </summary>
    property Title                    : ustring            read GetTitle                     write SetTitle;
    /// <summary>
    /// Get the Window icon.
    /// </summary>
    property WindowIcon               : ICefImage          read GetWindowIcon                write SetWindowIcon;
    /// <summary>
    /// Get the Window App icon.
    /// </summary>
    property WindowAppIcon            : ICefImage          read GetWindowAppIcon             write SetWindowAppIcon;
    /// <summary>
    /// Returns the Display that most closely intersects the bounds of this
    /// Window. May return NULL if this Window is not currently displayed.
    /// </summary>
    property Display                  : ICefDisplay        read GetDisplay;
    /// <summary>
    /// Returns the bounds (size and position) of this Window's client area.
    /// Position is in screen coordinates.
    /// </summary>
    property ClientAreaBoundsInScreen : TCefRect           read GetClientAreaBoundsInScreen;
    /// <summary>
    /// Retrieve the platform window handle for this Window.
    /// </summary>
    property WindowHandle             : TCefWindowHandle   read GetWindowHandle;
    /// <summary>
    /// Returns the runtime style for this Window (ALLOY or CHROME). See
    /// TCefRuntimeStyle documentation for details.
    /// </summary>
    property RuntimeStyle             : TCefRuntimeStyle   read GetRuntimeStyle;
  end;

  /// <summary>
  /// Implement this interface to handle window events. The functions of this
  /// interface will be called on the browser process UI thread unless otherwise
  /// indicated.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefWindowDelegate">Implements TCefWindowDelegate</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_window_delegate_capi.h">CEF source file: /include/capi/views/cef_window_delegate_capi.h (cef_window_delegate_t)</see></para>
  /// </remarks>
  ICefWindowDelegate = interface(ICefPanelDelegate)
    ['{52D4EE2C-303B-42B6-A35F-30D03834A23F}']
    /// <summary>
    /// Called when |window| is created.
    /// </summary>
    procedure OnWindowCreated(const window_: ICefWindow);
    /// <summary>
    /// Called when |window| is closing.
    /// </summary>
    procedure OnWindowClosing(const window_: ICefWindow);
    /// <summary>
    /// Called when |window| is destroyed. Release all references to |window| and
    /// do not attempt to execute any functions on |window| after this callback
    /// returns.
    /// </summary>
    procedure OnWindowDestroyed(const window_: ICefWindow);
    /// <summary>
    /// Called when |window| is activated or deactivated.
    /// </summary>
    procedure OnWindowActivationChanged(const window_: ICefWindow; active: boolean);
    /// <summary>
    /// Called when |window| bounds have changed. |new_bounds| will be in DIP
    /// screen coordinates.
    /// </summary>
    procedure OnWindowBoundsChanged(const window_: ICefWindow; const new_bounds: TCefRect);
    /// <summary>
    /// Called when |window| is transitioning to or from fullscreen mode. On MacOS
    /// the transition occurs asynchronously with |is_competed| set to false (0)
    /// when the transition starts and true (1) after the transition completes. On
    /// other platforms the transition occurs synchronously with |is_completed|
    /// set to true (1) after the transition completes. With Alloy style you must
    /// also implement ICefDisplayHandler.OnFullscreenModeChange to handle
    /// fullscreen transitions initiated by browser content.
    /// </summary>
    procedure OnWindowFullscreenTransition(const window_: ICefWindow; is_completed: boolean);
    /// <summary>
    /// Return the parent for |window| or NULL if the |window| does not have a
    /// parent. Windows with parents will not get a taskbar button. Set |is_menu|
    /// to true (1) if |window| will be displayed as a menu, in which case it will
    /// not be clipped to the parent window bounds. Set |can_activate_menu| to
    /// false (0) if |is_menu| is true (1) and |window| should not be activated
    /// (given keyboard focus) when displayed.
    /// </summary>
    procedure OnGetParentWindow(const window_: ICefWindow; var is_menu, can_activate_menu: boolean; var aResult : ICefWindow);
    /// <summary>
    /// Return true (1) if |window| should be created as a window modal dialog.
    /// Only called when a Window is returned via get_parent_window() with
    /// |is_menu| set to false (0). All controls in the parent Window will be
    /// disabled while |window| is visible. This functionality is not supported by
    /// all Linux window managers. Alternately, use
    /// ICefWindow.ShowAsBrowserModalDialog() for a browser modal dialog
    /// that works on all platforms.
    /// </summary>
    procedure OnIsWindowModalDialog(const window_: ICefWindow; var aResult: boolean);
    /// <summary>
    /// Return the initial bounds for |window| in density independent pixel (DIP)
    /// coordinates. If this function returns an NULL CefRect then
    /// GetPreferredSize() will be called to retrieve the size, and the window
    /// will be placed on the screen with origin (0,0). This function can be used
    /// in combination with ICefView.GetBoundsInScreen() to restore the
    /// previous window bounds.
    /// </summary>
    procedure OnGetInitialBounds(const window_: ICefWindow; var aResult : TCefRect);
    /// <summary>
    /// Return the initial show state for |window|.
    /// </summary>
    procedure OnGetInitialShowState(const window_: ICefWindow; var aResult : TCefShowState);
    /// <summary>
    /// Return true (1) if |window| should be created without a frame or title
    /// bar. The window will be resizable if can_resize() returns true (1). Use
    /// ICefWindow.SetDraggableRegions() to specify draggable regions.
    /// </summary>
    procedure OnIsFrameless(const window_: ICefWindow; var aResult : boolean);
    /// <summary>
    /// Return true (1) if |window| should be created with standard window buttons
    /// like close, minimize and zoom. This function is only supported on macOS.
    /// </summary>
    procedure OnWithStandardWindowButtons(const window_: ICefWindow; var aResult : boolean);
    /// <summary>
    /// Return whether the titlebar height should be overridden, and sets the
    /// height of the titlebar in |titlebar_height|. On macOS, it can also be used
    /// to adjust the vertical position of the traffic light buttons in frameless
    /// windows. The buttons will be positioned halfway down the titlebar at a
    /// height of |titlebar_height| / 2.
    /// </summary>
    procedure OnGetTitlebarHeight(const window_: ICefWindow; var titlebar_height: Single; var aResult : boolean);
    /// <summary>
    /// <para>Return whether the view should accept the initial mouse-down event,
    /// allowing it to respond to click-through behavior. If STATE_ENABLED is
    /// returned, the view will be sent a mouseDown: message for an initial mouse-
    /// down event, activating the view with one click, instead of clicking first
    /// to make the window active and then clicking the view.</para>
    /// <para>This function is only supported on macOS. For more details, refer to the
    /// documentation of acceptsFirstMouse.</para>
    /// </summary>
    procedure OnAcceptsFirstMouse(const window_: ICefWindow; var aResult: TCefState);
    /// <summary>
    /// Return true (1) if |window| can be resized.
    /// </summary>
    procedure OnCanResize(const window_: ICefWindow; var aResult : boolean);
    /// <summary>
    /// Return true (1) if |window| can be maximized.
    /// </summary>
    procedure OnCanMaximize(const window_: ICefWindow; var aResult : boolean);
    /// <summary>
    /// Return true (1) if |window| can be minimized.
    /// </summary>
    procedure OnCanMinimize(const window_: ICefWindow; var aResult : boolean);
    /// <summary>
    /// Return true (1) if |window| can be closed. This will be called for user-
    /// initiated window close actions and when ICefWindow.close() is called.
    /// </summary>
    procedure OnCanClose(const window_: ICefWindow; var aResult : boolean);
    /// <summary>
    /// Called when a keyboard accelerator registered with
    /// ICefWindow.SetAccelerator is triggered. Return true (1) if the
    /// accelerator was handled or false (0) otherwise.
    /// </summary>
    procedure OnAccelerator(const window_: ICefWindow; command_id: Integer; var aResult : boolean);
    /// <summary>
    /// Called after all other controls in the window have had a chance to handle
    /// the event. |event| contains information about the keyboard event. Return
    /// true (1) if the keyboard event was handled or false (0) otherwise.
    /// </summary>
    procedure OnKeyEvent(const window_: ICefWindow; const event: TCefKeyEvent; var aResult : boolean);
    /// <summary>
    /// <para>Called after the native/OS or Chrome theme for |window| has changed.
    /// |chrome_theme| will be true (1) if the notification is for a Chrome theme.</para>
    /// <para>Native/OS theme colors are configured globally and do not need to be
    /// customized for each Window individually. An example of a native/OS theme
    /// change that triggers this callback is when the user switches between dark
    /// and light mode during application lifespan. Native/OS theme changes can be
    /// disabled by passing the `--force-dark-mode` or `--force-light-mode`
    /// command-line flag.</para>
    /// <para>Chrome theme colors will be applied and this callback will be triggered
    /// if/when a BrowserView is added to the Window's component hierarchy. Chrome
    /// theme colors can be configured on a per-RequestContext basis using
    /// ICefRequestContext.SetChromeColorScheme or (Chrome style only) by
    /// visiting chrome://settings/manageProfile. Any theme changes using those
    /// mechanisms will also trigger this callback. Chrome theme colors will be
    /// persisted and restored from disk cache.</para>
    /// <para>This callback is not triggered on Window creation so clients that wish to
    /// customize the initial native/OS theme must call
    /// ICefWindow.SetThemeColor and ICefWindow.ThemeChanged before showing
    /// the first Window.</para>
    /// <para>Theme colors will be reset to standard values before this callback is
    /// called for the first affected Window. Call ICefWindow.SetThemeColor
    /// from inside this callback to override a standard color or add a custom
    /// color. ICefViewDelegate.OnThemeChanged will be called after this
    /// callback for the complete |window| component hierarchy.</para>
    /// </summary>
    procedure OnThemeColorsChanged(const window_: ICefWindow; chrome_theme: Integer);
    /// <summary>
    /// Optionally change the runtime style for this Window. See
    /// TCefRuntimeStyle documentation for details.
    /// </summary>
    procedure OnGetWindowRuntimeStyle(var aResult: TCefRuntimeStyle);
    /// <summary>
    /// Return Linux-specific window properties for correctly handling by window
    /// managers.
    /// </summary>
    procedure OnGetLinuxWindowProperties(const window_: ICefWindow; var properties: TLinuxWindowProperties; var aResult: boolean);
  end;

implementation

end.
