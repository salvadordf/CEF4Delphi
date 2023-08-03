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
//        Copyright © 2023 Salvador Diaz Fau. All rights reserved.
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
  ICefGetExtensionResourceCallback = interface;
  ICefExtensionHandler = interface;
  ICefExtension = interface;
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

  TCefv8ValueArray         = array of ICefv8Value;
  TCefX509CertificateArray = array of ICefX509Certificate;
  TCefBinaryValueArray     = array of ICefBinaryValue;
  TCefFrameIdentifierArray = array of int64;
  TCefPostDataElementArray = array of ICefPostDataElement;
  TCefMediaRouteArray      = array of ICefMediaRoute;
  TCefMediaSinkArray       = array of ICefMediaSink;
  TCefDisplayArray         = array of ICefDisplay;

  TCefMediaSinkInfo = record
    ID          : ustring;
    Name        : ustring;
    IconType    : TCefMediaSinkIconType;
    SinkType    : TCefMediaType;
    SinkIntf    : ICefMediaSink;
  end;
  TCefMediaSinkInfoArray = array of TCefMediaSinkInfo;

  TCefMediaRouteInfo = record
    ID        : ustring;
    SourceID  : ustring;
    SinkID    : ustring;
    RouteIntf : ICefMediaRoute;
  end;
  TCefMediaRouteInfoArray = array of TCefMediaRouteInfo;

  TCefMediaSourceInfo = record
    ID         : ustring;
    Valid      : boolean;
    SourceType : TCefMediaType;
    SourceIntf : ICefMediaSource;
  end;
  TCefMediaSourceInfoArray = array of TCefMediaSourceInfo;


  // *******************************************
  // **** Callback procedures and functions ****
  // *******************************************

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



  // *******************************************
  // ************ Custom interfaces ************
  // *******************************************

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

  IApplicationCoreEvents = interface
    ['{55E99E25-A05D-46D5-B3A4-C8C2E71C1F4D}']

    // ICefApp
    procedure doOnBeforeCommandLineProcessing(const processType: ustring; const commandLine: ICefCommandLine);
    procedure doOnRegisterCustomSchemes(const registrar: TCefSchemeRegistrarRef);

    // ICefBrowserProcessHandler
    procedure doOnRegisterCustomPreferences(type_: TCefPreferencesType; registrar: PCefPreferenceRegistrar);
    procedure doOnContextInitialized;
    procedure doOnBeforeChildProcessLaunch(const commandLine: ICefCommandLine);
    procedure doOnScheduleMessagePumpWork(const delayMs: Int64);
    procedure doGetDefaultClient(var aClient : ICefClient);

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
    procedure doOnBeforeDownload(const browser: ICefBrowser; const downloadItem: ICefDownloadItem; const suggestedName: ustring; const callback: ICefBeforeDownloadCallback);
    procedure doOnDownloadUpdated(const browser: ICefBrowser; const downloadItem: ICefDownloadItem; const callback: ICefDownloadItemCallback);

    // ICefJsDialogHandler
    function  doOnJsdialog(const browser: ICefBrowser; const originUrl: ustring; dialogType: TCefJsDialogType; const messageText, defaultPromptText: ustring; const callback: ICefJsDialogCallback; out suppressMessage: Boolean): Boolean;
    function  doOnBeforeUnloadDialog(const browser: ICefBrowser; const messageText: ustring; isReload: Boolean; const callback: ICefJsDialogCallback): Boolean;
    procedure doOnResetDialogState(const browser: ICefBrowser);
    procedure doOnDialogClosed(const browser: ICefBrowser);

    // ICefLifeSpanHandler
    function  doOnBeforePopup(const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean): Boolean;
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
    procedure doOnRenderProcessTerminated(const browser: ICefBrowser; status: TCefTerminationStatus);
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
    function  doOnFileDialog(const browser: ICefBrowser; mode: TCefFileDialogMode; const title, defaultFilePath: ustring; const acceptFilters: TStrings; const callback: ICefFileDialogCallback): Boolean;

    // ICefRenderHandler
    procedure doOnGetAccessibilityHandler(var aAccessibilityHandler : ICefAccessibilityHandler);
    function  doOnGetRootScreenRect(const browser: ICefBrowser; var rect: TCefRect): Boolean;
    procedure doOnGetViewRect(const browser: ICefBrowser; var rect: TCefRect);
    function  doOnGetScreenPoint(const browser: ICefBrowser; viewX, viewY: Integer; var screenX, screenY: Integer): Boolean;
    function  doOnGetScreenInfo(const browser: ICefBrowser; var screenInfo: TCefScreenInfo): Boolean;
    procedure doOnPopupShow(const browser: ICefBrowser; show: Boolean);
    procedure doOnPopupSize(const browser: ICefBrowser; const rect: PCefRect);
    procedure doOnPaint(const browser: ICefBrowser; type_: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; const buffer: Pointer; width, height: Integer);
    procedure doOnAcceleratedPaint(const browser: ICefBrowser; type_: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; shared_handle: Pointer);
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

    // ICefExtensionHandler
    procedure doOnExtensionLoadFailed(result: TCefErrorcode);
    procedure doOnExtensionLoaded(const extension: ICefExtension);
    procedure doOnExtensionUnloaded(const extension: ICefExtension);
    function  doOnExtensionBeforeBackgroundBrowser(const extension: ICefExtension; const url: ustring; var client: ICefClient; var settings: TCefBrowserSettings) : boolean;
    function  doOnExtensionBeforeBrowser(const extension: ICefExtension; const browser, active_browser: ICefBrowser; index: Integer; const url: ustring; active: boolean; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings) : boolean;
    procedure doOnExtensionGetActiveBrowser(const extension: ICefExtension; const browser: ICefBrowser; include_incognito: boolean; var aRsltBrowser: ICefBrowser);
    function  doOnExtensionCanAccessBrowser(const extension: ICefExtension; const browser: ICefBrowser; include_incognito: boolean; const target_browser: ICefBrowser): boolean;
    function  doOnExtensionGetExtensionResource(const extension: ICefExtension; const browser: ICefBrowser; const file_: ustring; const callback: ICefGetExtensionResourceCallback): boolean;

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
  end;

  IServerEvents = interface
    ['{06A1B3C6-0967-4F6C-A751-8AA3A29E2FF5}']
    procedure doOnServerCreated(const server: ICefServer);
    procedure doOnServerDestroyed(const server: ICefServer);
    procedure doOnClientConnected(const server: ICefServer; connection_id: Integer);
    procedure doOnClientDisconnected(const server: ICefServer; connection_id: Integer);
    procedure doOnHttpRequest(const server: ICefServer; connection_id: Integer; const client_address: ustring; const request: ICefRequest);
    procedure doOnWebSocketRequest(const server: ICefServer; connection_id: Integer; const client_address: ustring; const request: ICefRequest; const callback: ICefCallback);
    procedure doOnWebSocketConnected(const server: ICefServer; connection_id: Integer);
    procedure doOnWebSocketMessage(const server: ICefServer; connection_id: Integer; const data: Pointer; data_size: NativeUInt);
  end;

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
  end;



  // *******************************************
  // ************** CEF interfaces *************
  // *******************************************

  /// <summary>
  /// All ref-counted framework interfaces must inherit from this interface.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefBaseRefCounted">Implements TCefBaseRefCounted</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_base_capi.h">CEF source file: /include/capi/cef_base_capi.h (cef_base_ref_counted_t)</see></para>
  /// </remarks>
  ICefBaseRefCounted = interface
    ['{1F9A7B44-DCDC-4477-9180-3ADD44BDEB7B}']
    function Wrap: Pointer;
    function SameAs(aData : Pointer) : boolean; overload;
    function SameAs(const aBaseRefCounted : ICefBaseRefCounted) : boolean; overload;
    function HasOneRef : boolean;
    function HasAtLeastOneRef : boolean;
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
    function  GetBrowser: ICefBrowser;
    procedure CloseBrowser(forceClose: Boolean);
    function  TryCloseBrowser: Boolean;
    procedure SetFocus(focus: Boolean);
    function  GetWindowHandle: TCefWindowHandle;
    function  GetOpenerWindowHandle: TCefWindowHandle;
    function  HasView: Boolean;
    function  GetRequestContext: ICefRequestContext;
    function  GetZoomLevel: Double;
    procedure SetZoomLevel(const zoomLevel: Double);
    procedure RunFileDialog(mode: TCefFileDialogMode; const title, defaultFilePath: ustring; const acceptFilters: TStrings; const callback: ICefRunFileDialogCallback);
    procedure RunFileDialogProc(mode: TCefFileDialogMode; const title, defaultFilePath: ustring; const acceptFilters: TStrings; const callback: TCefRunFileDialogCallbackProc);
    procedure StartDownload(const url: ustring);
    procedure DownloadImage(const imageUrl: ustring; isFavicon: Boolean; maxImageSize: cardinal; bypassCache: Boolean; const callback: ICefDownloadImageCallback);
    procedure Print;
    procedure PrintToPdf(const path: ustring; settings: PCefPdfPrintSettings; const callback: ICefPdfPrintCallback);
    procedure PrintToPdfProc(const path: ustring; settings: PCefPdfPrintSettings; const callback: TOnPdfPrintFinishedProc);
    procedure Find(const searchText: ustring; forward_, matchCase, findNext: Boolean);
    procedure StopFinding(clearSelection: Boolean);
    procedure ShowDevTools(const windowInfo: PCefWindowInfo; const client: ICefClient; const settings: PCefBrowserSettings; inspectElementAt: PCefPoint);
    procedure CloseDevTools;
    function  HasDevTools: Boolean;
    function  SendDevToolsMessage(const message_: ustring): boolean;
    function  ExecuteDevToolsMethod(message_id: integer; const method: ustring; const params: ICefDictionaryValue): Integer;
    function  AddDevToolsMessageObserver(const observer: ICefDevToolsMessageObserver): ICefRegistration;
    procedure GetNavigationEntries(const visitor: ICefNavigationEntryVisitor; currentOnly: Boolean);
    procedure GetNavigationEntriesProc(const proc: TCefNavigationEntryVisitorProc; currentOnly: Boolean);
    procedure ReplaceMisspelling(const word: ustring);
    procedure AddWordToDictionary(const word: ustring);
    function  IsWindowRenderingDisabled: Boolean;
    procedure WasResized;
    procedure WasHidden(hidden: Boolean);
    procedure NotifyScreenInfoChanged;
    procedure Invalidate(kind: TCefPaintElementType);
    procedure SendExternalBeginFrame;
    procedure SendKeyEvent(const event: PCefKeyEvent);
    procedure SendMouseClickEvent(const event: PCefMouseEvent; type_: TCefMouseButtonType; mouseUp: Boolean; clickCount: Integer);
    procedure SendMouseMoveEvent(const event: PCefMouseEvent; mouseLeave: Boolean);
    procedure SendMouseWheelEvent(const event: PCefMouseEvent; deltaX, deltaY: Integer);
    procedure SendTouchEvent(const event: PCefTouchEvent);
    procedure SendCaptureLostEvent;
    procedure NotifyMoveOrResizeStarted;
    function  GetWindowlessFrameRate : Integer;
    procedure SetWindowlessFrameRate(frameRate: Integer);
    procedure IMESetComposition(const text: ustring; const underlines : TCefCompositionUnderlineDynArray; const replacement_range, selection_range : PCefRange);
    procedure IMECommitText(const text: ustring; const replacement_range : PCefRange; relative_cursor_pos : integer);
    procedure IMEFinishComposingText(keep_selection : boolean);
    procedure IMECancelComposition;
    procedure DragTargetDragEnter(const dragData: ICefDragData; const event: PCefMouseEvent; allowedOps: TCefDragOperations);
    procedure DragTargetDragOver(const event: PCefMouseEvent; allowedOps: TCefDragOperations);
    procedure DragTargetDragLeave;
    procedure DragTargetDrop(const event: PCefMouseEvent);
    procedure DragSourceEndedAt(x, y: Integer; op: TCefDragOperation);
    procedure DragSourceSystemDragEnded;
    function  GetVisibleNavigationEntry : ICefNavigationEntry;
    procedure SetAccessibilityState(accessibilityState: TCefState);
    procedure SetAutoResizeEnabled(enabled: boolean; const min_size, max_size: PCefSize);
    function  GetExtension : ICefExtension;
    function  IsBackgroundHost : boolean;
    procedure SetAudioMuted(mute: boolean);
    function  IsAudioMuted : boolean;

    property Browser                    : ICefBrowser              read GetBrowser;
    property WindowHandle               : TCefWindowHandle         read GetWindowHandle;
    property OpenerWindowHandle         : TCefWindowHandle         read GetOpenerWindowHandle;
    property ZoomLevel                  : Double                   read GetZoomLevel                 write SetZoomLevel;
    property RequestContext             : ICefRequestContext       read GetRequestContext;
    property VisibleNavigationEntry     : ICefNavigationEntry      read GetVisibleNavigationEntry;
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
    function IsValid: Boolean;
    function IsReadOnly: Boolean;
    function Copy: ICefProcessMessage;
    function GetName: ustring;
    function GetArgumentList: ICefListValue;
    function GetSharedMemoryRegion: ICefSharedMemoryRegion;

    property Name               : ustring                 read GetName;
    property ArgumentList       : ICefListValue           read GetArgumentList;
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
    function  IsValid: boolean;
    function  GetHost: ICefBrowserHost;
    function  CanGoBack: Boolean;
    procedure GoBack;
    function  CanGoForward: Boolean;
    procedure GoForward;
    function  IsLoading: Boolean;
    procedure Reload;
    procedure ReloadIgnoreCache;
    procedure StopLoad;
    function  GetIdentifier: Integer;
    function  IsSame(const that: ICefBrowser): Boolean;
    function  IsPopup: Boolean;
    function  HasDocument: Boolean;
    function  GetMainFrame: ICefFrame;
    function  GetFocusedFrame: ICefFrame;
    function  GetFrameByident(const identifier: Int64): ICefFrame;
    function  GetFrame(const name: ustring): ICefFrame;
    function  GetFrameCount: NativeUInt;
    function  GetFrameIdentifiers(var aFrameCount : NativeUInt; var aFrameIdentifierArray : TCefFrameIdentifierArray) : boolean;
    function  GetFrameNames(var aFrameNames : TStrings) : boolean;

    property MainFrame    : ICefFrame       read GetMainFrame;
    property FocusedFrame : ICefFrame       read GetFocusedFrame;
    property FrameCount   : NativeUInt      read GetFrameCount;
    property Host         : ICefBrowserHost read GetHost;
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
    function  IsReadOnly: Boolean;
    procedure SetToEmpty;
    procedure SetToFile(const fileName: ustring);
    procedure SetToBytes(size: NativeUInt; const bytes: Pointer);
    function  GetType: TCefPostDataElementType;
    function  GetFile: ustring;
    function  GetBytesCount: NativeUInt;
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
    function  IsReadOnly: Boolean;
    function  HasExcludedElements: Boolean;
    function  GetElementCount: NativeUInt;
    procedure GetElements(elementsCount: NativeUInt; var elements: TCefPostDataElementArray);
    function  RemoveElement(const element: ICefPostDataElement): Boolean;
    function  AddElement(const element: ICefPostDataElement): Boolean;
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
    function  IsReadOnly: Boolean;
    function  GetUrl: ustring;
    function  GetMethod: ustring;
    function  GetPostData: ICefPostData;
    procedure GetHeaderMap(const HeaderMap: ICefStringMultimap);
    procedure SetUrl(const value: ustring);
    procedure SetMethod(const value: ustring);
    procedure SetReferrer(const referrerUrl: ustring; policy: TCefReferrerPolicy);
    function  GetReferrerUrl: ustring;
    function  GetReferrerPolicy: TCefReferrerPolicy;
    procedure SetPostData(const value: ICefPostData);
    procedure SetHeaderMap(const HeaderMap: ICefStringMultimap);
    function  GetHeaderByName(const name: ustring): ustring;
    procedure SetHeaderByName(const name, value: ustring; overwrite: boolean);
    function  GetFlags: TCefUrlRequestFlags;
    procedure SetFlags(flags: TCefUrlRequestFlags);
    function  GetFirstPartyForCookies: ustring;
    procedure SetFirstPartyForCookies(const url: ustring);
    procedure Assign(const url, method: ustring; const postData: ICefPostData; const headerMap: ICefStringMultimap);
    function  GetResourceType: TCefResourceType;
    function  GetTransitionType: TCefTransitionType;
    function  GetIdentifier: UInt64;

    property Url                  : ustring               read GetUrl                    write SetUrl;
    property Method               : ustring               read GetMethod                 write SetMethod;
    property ReferrerUrl          : ustring               read GetReferrerUrl;
    property ReferrerPolicy       : TCefReferrerPolicy    read GetReferrerPolicy;
    property PostData             : ICefPostData          read GetPostData               write SetPostData;
    property Flags                : TCefUrlRequestFlags   read GetFlags                  write SetFlags;
    property FirstPartyForCookies : ustring               read GetFirstPartyForCookies   write SetFirstPartyForCookies;
    property ResourceType         : TCefResourceType      read GetResourceType;
    property TransitionType       : TCefTransitionType    read GetTransitionType;
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
    function  IsValid: Boolean;
    procedure Undo;
    procedure Redo;
    procedure Cut;
    procedure Copy;
    procedure Paste;
    procedure Del;
    procedure SelectAll;
    procedure ViewSource;
    procedure GetSource(const visitor: ICefStringVisitor);
    procedure GetSourceProc(const proc: TCefStringVisitorProc);
    procedure GetText(const visitor: ICefStringVisitor);
    procedure GetTextProc(const proc: TCefStringVisitorProc);
    procedure LoadRequest(const request: ICefRequest);
    procedure LoadUrl(const url: ustring);
    procedure ExecuteJavaScript(const code, scriptUrl: ustring; startLine: Integer);
    function  IsMain: Boolean;
    function  IsFocused: Boolean;
    function  GetName: ustring;
    function  GetIdentifier: Int64;
    function  GetParent: ICefFrame;
    function  GetUrl: ustring;
    function  GetBrowser: ICefBrowser;
    function  GetV8Context: ICefv8Context;
    procedure VisitDom(const visitor: ICefDomVisitor);
    procedure VisitDomProc(const proc: TCefDomVisitorProc);
    function  CreateUrlRequest(const request: ICefRequest; const client: ICefUrlrequestClient): ICefUrlRequest;
    procedure SendProcessMessage(targetProcess: TCefProcessId; const message_: ICefProcessMessage);

    property Name       : ustring     read GetName;
    property Url        : ustring     read GetUrl;
    property Browser    : ICefBrowser read GetBrowser;
    property Parent     : ICefFrame   read GetParent;
    property Identifier : int64       read GetIdentifier;
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
    /// cef_frame_t::is_valid() will now return false (0) for |frame|. If called
    /// after cef_life_span_handler_t::on_before_close() during browser
    /// destruction then cef_browser_t::is_valid() will return false (0) for
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
    /// for |new_frame| and/or after on_frame_detached() for |old_frame|. If
    /// called after cef_life_span_handler_t::on_before_close() during browser
    /// destruction then cef_browser_t::is_valid() will return false (0) for
    /// |browser|.
    ///
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
    function Read(ptr: Pointer; size, n: NativeUInt): NativeUInt;
    function Seek(offset: Int64; whence: Integer): Integer;
    function Tell: Int64;
    function Eof: Boolean;
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
    function Read(ptr: Pointer; size, n: NativeUInt): NativeUInt;
    function Seek(offset: Int64; whence: Integer): Integer;
    function Tell: Int64;
    function Eof: Boolean;
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
    function Write(const ptr: Pointer; size, n: NativeUInt): NativeUInt;
    function Seek(offset: Int64; whence: Integer): Integer;
    function Tell: Int64;
    function Flush: Integer;
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
    function Write(const ptr: Pointer; size, n: NativeUInt): NativeUInt;
    function Seek(offset: Int64; whence: Integer): Integer;
    function Tell: Int64;
    function Flush: Integer;
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
    function  IsReadOnly: Boolean;
    function  GetError: TCefErrorCode;
    procedure SetError(error: TCefErrorCode);
    function  GetStatus: Integer;
    procedure SetStatus(status: Integer);
    function  GetStatusText: ustring;
    procedure SetStatusText(const StatusText: ustring);
    function  GetMimeType: ustring;
    procedure SetMimeType(const mimetype: ustring);
    function  GetCharset: ustring;
    procedure SetCharset(const charset: ustring);
    function  GetHeaderByName(const name: ustring): ustring;
    procedure SetHeaderByName(const name, value: ustring; overwrite: boolean);
    procedure GetHeaderMap(const headerMap: ICefStringMultimap);
    procedure SetHeaderMap(const headerMap: ICefStringMultimap);
    function  GetURL: ustring;
    procedure SetURL(const url: ustring);

    property Status     : Integer       read GetStatus      write SetStatus;
    property StatusText : ustring       read GetStatusText  write SetStatusText;
    property MimeType   : ustring       read GetMimeType    write SetMimeType;
    property Charset    : ustring       read GetCharset     write SetCharset;
    property Error      : TCefErrorCode read GetError       write SetError;
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
    function IsValid: Boolean;
    function IsInProgress: Boolean;
    function IsComplete: Boolean;
    function IsCanceled: Boolean;
    function IsInterrupted: Boolean;
    function GetInterruptReason: TCefDownloadInterruptReason;
    function GetCurrentSpeed: Int64;
    function GetPercentComplete: Integer;
    function GetTotalBytes: Int64;
    function GetReceivedBytes: Int64;
    function GetStartTime: TDateTime;
    function GetEndTime: TDateTime;
    function GetFullPath: ustring;
    function GetId: Cardinal;
    function GetUrl: ustring;
    function GetOriginalUrl: ustring;
    function GetSuggestedFileName: ustring;
    function GetContentDisposition: ustring;
    function GetMimeType: ustring;

    property CurrentSpeed        : Int64                         read GetCurrentSpeed;
    property PercentComplete     : Integer                       read GetPercentComplete;
    property TotalBytes          : Int64                         read GetTotalBytes;
    property ReceivedBytes       : Int64                         read GetReceivedBytes;
    property StartTime           : TDateTime                     read GetStartTime;
    property EndTime             : TDateTime                     read GetEndTime;
    property FullPath            : ustring                       read GetFullPath;
    property Id                  : Cardinal                      read GetId;
    property Url                 : ustring                       read GetUrl;
    property OriginalUrl         : ustring                       read GetOriginalUrl;
    property SuggestedFileName   : ustring                       read GetSuggestedFileName;
    property ContentDisposition  : ustring                       read GetContentDisposition;
    property MimeType            : ustring                       read GetMimeType;
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
    /// for the download file. By default the download will be canceled. Execute
    /// |callback| either asynchronously or in this function to continue the
    /// download if desired. Do not keep a reference to |download_item| outside of
    /// this function.
    /// </summary>
    procedure OnBeforeDownload(const browser: ICefBrowser; const downloadItem: ICefDownloadItem; const suggestedName: ustring; const callback: ICefBeforeDownloadCallback);
    /// <summary>
    /// Called when a download's status or progress information has been updated.
    /// This may be called multiple times before and after on_before_download().
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
    function GetMessage: ustring;
    function GetSourceLine: ustring;
    function GetScriptResourceName: ustring;
    function GetLineNumber: Integer;
    function GetStartPosition: Integer;
    function GetEndPosition: Integer;
    function GetStartColumn: Integer;
    function GetEndColumn: Integer;

    property Message            : ustring read GetMessage;
    property SourceLine         : ustring read GetSourceLine;
    property ScriptResourceName : ustring read GetScriptResourceName;
    property LineNumber         : Integer read GetLineNumber;
    property StartPosition      : Integer read GetStartPosition;
    property EndPosition        : Integer read GetEndPosition;
    property StartColumn        : Integer read GetStartColumn;
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
    function GetTaskRunner: ICefTaskRunner;
    function IsValid: Boolean;
    function GetBrowser: ICefBrowser;
    function GetFrame: ICefFrame;
    function GetGlobal: ICefv8Value;
    function Enter: Boolean;
    function Exit: Boolean;
    function IsSame(const that: ICefv8Context): Boolean;
    function Eval(const code: ustring; const script_url: ustring; start_line: integer; var retval: ICefv8Value; var exception: ICefV8Exception): Boolean;

    property Browser  : ICefBrowser read GetBrowser;
    property Frame    : ICefFrame   read GetFrame;
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
    function GetByName(const name: ustring; const object_: ICefv8Value; var retval: ICefv8Value; var exception: ustring): boolean;
    function GetByIndex(index: integer; const object_: ICefv8Value; var retval: ICefv8Value; var exception: ustring): boolean;
    function SetByName(const name: ustring; const object_, value: ICefv8Value; var exception: ustring): boolean;
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
    function Get(const name: ustring; const object_: ICefv8Value; var retval: ICefv8Value; var exception: ustring): Boolean;
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
    procedure Execute;
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
    function IsSame(const that: ICefTaskRunner): Boolean;
    function BelongsToCurrentThread: Boolean;
    function BelongsToThread(threadId: TCefThreadId): Boolean;
    function PostTask(const task: ICefTask): Boolean;
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
    function  GetTaskRunner : ICefTaskRunner;
    function  GetPlatformThreadID : TCefPlatformThreadId;
    procedure Stop;
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
    procedure Reset;
    procedure Signal;
    function  IsSignaled : boolean;
    procedure Wait;
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
    function IsValid: Boolean;
    function IsUndefined: Boolean;
    function IsNull: Boolean;
    function IsBool: Boolean;
    function IsInt: Boolean;
    function IsUInt: Boolean;
    function IsDouble: Boolean;
    function IsDate: Boolean;
    function IsString: Boolean;
    function IsObject: Boolean;
    function IsArray: Boolean;
    function IsArrayBuffer: Boolean;
    function IsFunction: Boolean;
    function IsPromise: Boolean;
    function IsSame(const that: ICefv8Value): Boolean;
    function GetBoolValue: Boolean;
    function GetIntValue: Integer;
    function GetUIntValue: Cardinal;
    function GetDoubleValue: Double;
    function GetDateValue: TDateTime;
    function GetStringValue: ustring;
    function IsUserCreated: Boolean;
    function HasException: Boolean;
    function GetException: ICefV8Exception;
    function ClearException: Boolean;
    function WillRethrowExceptions: Boolean;
    function SetRethrowExceptions(rethrow: Boolean): Boolean;
    function HasValueByKey(const key: ustring): Boolean;
    function HasValueByIndex(index: Integer): Boolean;
    function DeleteValueByKey(const key: ustring): Boolean;
    function DeleteValueByIndex(index: Integer): Boolean;
    function GetValueByKey(const key: ustring): ICefv8Value;
    function GetValueByIndex(index: Integer): ICefv8Value;
    function SetValueByKey(const key: ustring; const value: ICefv8Value; attribute: TCefV8PropertyAttributes): Boolean;
    function SetValueByIndex(index: Integer; const value: ICefv8Value): Boolean;
    function SetValueByAccessor(const key: ustring; settings: TCefV8AccessControls; attribute: TCefV8PropertyAttributes): Boolean;
    function GetKeys(const keys: TStrings): Integer;
    function SetUserData(const data: ICefv8Value): Boolean;
    function GetUserData: ICefv8Value;
    function GetExternallyAllocatedMemory: Integer;
    function AdjustExternallyAllocatedMemory(changeInBytes: Integer): Integer;
    function GetArrayLength: Integer;
    function GetArrayBufferReleaseCallback : ICefv8ArrayBufferReleaseCallback;
    function NeuterArrayBuffer : boolean;
    function GetFunctionName: ustring;
    function GetFunctionHandler: ICefv8Handler;
    function ExecuteFunction(const obj: ICefv8Value; const arguments: TCefv8ValueArray): ICefv8Value;
    function ExecuteFunctionWithContext(const context: ICefv8Context; const obj: ICefv8Value; const arguments: TCefv8ValueArray): ICefv8Value;
    function ResolvePromise(const arg: ICefv8Value): boolean;
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
    function IsValid: Boolean;
    function GetScriptName: ustring;
    function GetScriptNameOrSourceUrl: ustring;
    function GetFunctionName: ustring;
    function GetLineNumber: Integer;
    function GetColumn: Integer;
    function IsEval: Boolean;
    function IsConstructor: Boolean;

    property ScriptName             : ustring read GetScriptName;
    property ScriptNameOrSourceUrl  : ustring read GetScriptNameOrSourceUrl;
    property FunctionName           : ustring read GetFunctionName;
    property LineNumber             : Integer read GetLineNumber;
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
    function IsValid: Boolean;
    function GetFrameCount: Integer;
    function GetFrame(index: Integer): ICefV8StackFrame;

    property FrameCount            : Integer          read GetFrameCount;
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
    function MoveToNextNode: Boolean;
    function Close: Boolean;
    function HasError: Boolean;
    function GetError: ustring;
    function GetType: TCefXmlNodeType;
    function GetDepth: Integer;
    function GetLocalName: ustring;
    function GetPrefix: ustring;
    function GetQualifiedName: ustring;
    function GetNamespaceUri: ustring;
    function GetBaseUri: ustring;
    function GetXmlLang: ustring;
    function IsEmptyElement: Boolean;
    function HasValue: Boolean;
    function GetValue: ustring;
    function HasAttributes: Boolean;
    function GetAttributeCount: NativeUInt;
    function GetAttributeByIndex(index: Integer): ustring;
    function GetAttributeByQName(const qualifiedName: ustring): ustring;
    function GetAttributeByLName(const localName, namespaceURI: ustring): ustring;
    function GetInnerXml: ustring;
    function GetOuterXml: ustring;
    function GetLineNumber: Integer;
    function MoveToAttributeByIndex(index: Integer): Boolean;
    function MoveToAttributeByQName(const qualifiedName: ustring): Boolean;
    function MoveToAttributeByLName(const localName, namespaceURI: ustring): Boolean;
    function MoveToFirstAttribute: Boolean;
    function MoveToNextAttribute: Boolean;
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
    function MoveToFirstFile: Boolean;
    function MoveToNextFile: Boolean;
    function MoveToFile(const fileName: ustring; caseSensitive: Boolean): Boolean;
    function Close: Boolean;
    function GetFileName: ustring;
    function GetFileSize: Int64;
    function GetFileLastModified: TCefBaseTime;
    function OpenFile(const password: ustring): Boolean;
    function CloseFile: Boolean;
    function ReadFile(buffer: Pointer; bufferSize: NativeUInt): Integer;
    function Tell: Int64;
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
    function  GetType: TCefDomNodeType;
    function  IsText: Boolean;
    function  IsElement: Boolean;
    function  IsEditable: Boolean;
    function  IsFormControlElement: Boolean;
    function  GetFormControlElementType: ustring;
    function  IsSame(const that: ICefDomNode): Boolean;
    function  GetName: ustring;
    function  GetValue: ustring;
    function  SetValue(const value: ustring): Boolean;
    function  GetAsMarkup: ustring;
    function  GetDocument: ICefDomDocument;
    function  GetParent: ICefDomNode;
    function  GetPreviousSibling: ICefDomNode;
    function  GetNextSibling: ICefDomNode;
    function  HasChildren: Boolean;
    function  GetFirstChild: ICefDomNode;
    function  GetLastChild: ICefDomNode;
    function  GetElementTagName: ustring;
    function  HasElementAttributes: Boolean;
    function  HasElementAttribute(const attrName: ustring): Boolean;
    function  GetElementAttribute(const attrName: ustring): ustring;
    procedure GetElementAttributes(const attrMap: ICefStringMap); overload;
    procedure GetElementAttributes(var attrList: TStrings); overload;
    function  SetElementAttribute(const attrName, value: ustring): Boolean;
    function  GetElementInnerText: ustring;
    function  GetElementBounds: TCefRect;

    property NodeType         : TCefDomNodeType read GetType;
    property Name             : ustring         read GetName;
    property AsMarkup         : ustring         read GetAsMarkup;
    property Document         : ICefDomDocument read GetDocument;
    property Parent           : ICefDomNode     read GetParent;
    property PreviousSibling  : ICefDomNode     read GetPreviousSibling;
    property NextSibling      : ICefDomNode     read GetNextSibling;
    property FirstChild       : ICefDomNode     read GetFirstChild;
    property LastChild        : ICefDomNode     read GetLastChild;
    property ElementTagName   : ustring         read GetElementTagName;
    property ElementInnerText : ustring         read GetElementInnerText;
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
    function GetType: TCefDomDocumentType;
    function GetDocument: ICefDomNode;
    function GetBody: ICefDomNode;
    function GetHead: ICefDomNode;
    function GetTitle: ustring;
    function GetElementById(const id: ustring): ICefDomNode;
    function GetFocusedNode: ICefDomNode;
    function HasSelection: Boolean;
    function GetSelectionStartOffset: Integer;
    function GetSelectionEndOffset: Integer;
    function GetSelectionAsMarkup: ustring;
    function GetSelectionAsText: ustring;
    function GetBaseUrl: ustring;
    function GetCompleteUrl(const partialURL: ustring): ustring;

    property DocType              : TCefDomDocumentType read GetType;
    property Document             : ICefDomNode         read GetDocument;
    property Body                 : ICefDomNode         read GetBody;
    property Head                 : ICefDomNode         read GetHead;
    property Title                : ustring             read GetTitle;
    property FocusedNode          : ICefDomNode         read GetFocusedNode;
    property SelectionStartOffset : Integer             read GetSelectionStartOffset;
    property SelectionEndOffset   : Integer             read GetSelectionEndOffset;
    property SelectionAsMarkup    : ustring             read GetSelectionAsMarkup;
    property SelectionAsText      : ustring             read GetSelectionAsText;
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
    function  IsValid: Boolean;
    function  IsReadOnly: Boolean;
    function  Copy: ICefCommandLine;
    procedure InitFromArgv(argc: Integer; const argv: PPAnsiChar);
    procedure InitFromString(const commandLine: ustring);
    procedure Reset;
    function  GetCommandLineString: ustring;
    procedure GetArgv(var args: TStrings);
    function  GetProgram: ustring;
    procedure SetProgram(const prog: ustring);
    function  HasSwitches: Boolean;
    function  HasSwitch(const name: ustring): Boolean;
    function  GetSwitchValue(const name: ustring): ustring;
    function  GetSwitches(var switches: TStrings): boolean; overload;
    function  GetSwitches(var SwitchKeys, SwitchValues: TStringList): boolean; overload;
    procedure AppendSwitch(const name: ustring);
    procedure AppendSwitchWithValue(const name, value: ustring);
    function  HasArguments: Boolean;
    procedure GetArguments(var arguments: TStrings);
    procedure AppendArgument(const argument: ustring);
    procedure PrependWrapper(const wrapper: ustring);

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
    /// cef_browser_host_t::SendDevToolsMessage, and optionally either a "result"
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
    /// Trigger an asynchronous call to cef_media_observer_t::OnSinks on all
    /// registered observers.
    /// </summary>
    procedure NotifyCurrentSinks;
    /// <summary>
    /// Create a new route between |source| and |sink|. Source and sink must be
    /// valid, compatible (as reported by cef_media_sink_t::IsCompatibleWith), and
    /// a route between them must not already exist. |callback| will be executed
    /// on success or failure. If route creation succeeds it will also trigger an
    /// asynchronous call to cef_media_observer_t::OnRoutes on all registered
    /// observers.
    /// </summary>
    procedure CreateRoute(const source: ICefMediaSource; const sink: ICefMediaSink; const callback: ICefMediaRouteCreateCallback);
    /// <summary>
    /// Trigger an asynchronous call to cef_media_observer_t::OnRoutes on all
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
    /// cef_media_router_t::NotifyCurrentSinks was called.
    /// </summary>
    procedure OnSinks(const sinks: TCefMediaSinkArray);
    /// <summary>
    /// The list of available media routes has changed or
    /// cef_media_router_t::NotifyCurrentRoutes was called.
    /// </summary>
    procedure OnRoutes(const routes: TCefMediaRouteArray);
    /// <summary>
    /// The connection state of |route| has changed.
    /// </summary>
    procedure OnRouteStateChanged(const route: ICefMediaRoute; state: TCefMediaRouteConnectionState);
    /// <summary>
    /// A message was recieved over |route|. |message| is only valid for the scope
    /// of this callback and should be copied if necessary.
    /// </summary>
    procedure OnRouteMessageReceived(const route: ICefMediaRoute; const message_: ustring);
  end;

  /// <summary>
  /// Custom interface used to handle the ICefMediaObserver events in a component.
  /// </summary>
  ICefMediaObserverEvents = interface
    ['{267D5287-08DB-49D6-AF6E-B27C66C6E5D4}']
    procedure doOnSinks(const sinks: TCefMediaSinkArray);
    procedure doOnRoutes(const routes: TCefMediaRouteArray);
    procedure doOnRouteStateChanged(const route: ICefMediaRoute; state: TCefMediaRouteConnectionState);
    procedure doOnRouteMessageReceived(const route: ICefMediaRoute; const message_: ustring);
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
    function  GetId: ustring;
    function  GetSource: ICefMediaSource;
    function  GetSink: ICefMediaSink;
    procedure SendRouteMessage(const message_: ustring);
    procedure Terminate;

    property ID     : ustring         read GetId;
    property Source : ICefMediaSource read GetSource;
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
    /// Provides an opportunity to register custom preferences prior to global and
    /// request context initialization.
    ///
    /// If |type| is CEF_PREFERENCES_TYPE_GLOBAL the registered preferences can be
    /// accessed via ICefPreferenceManager.GetGlobalPreferences after
    /// OnContextInitialized is called. Global preferences are registered a single
    /// time at application startup. See related TCefSettings.cache_path and
    /// TCefSettings.persist_user_preferences configuration.
    ///
    /// If |type| is CEF_PREFERENCES_TYPE_REQUEST_CONTEXT the preferences can be
    /// accessed via the ICefRequestContext after
    /// ICefRequestContextHandler.OnRequestContextInitialized is called.
    /// Request context preferences are registered each time a new
    /// ICefRequestContext is created. It is intended but not required that all
    /// request contexts have the same registered preferences. See related
    /// TCefRequestContextSettings.cache_path and
    /// TCefRequestContextSettings.persist_user_preferences configuration.
    ///
    /// Do not keep a reference to the |registrar| object. This function is called
    /// on the browser process UI thread.
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
    /// Return the default client for use with a newly created browser window. If
    /// null is returned the browser will be unmanaged (no callbacks will be
    /// executed for that browser) and application shutdown will be blocked until
    /// the browser window is closed manually. This function is currently only
    /// used with the chrome runtime.
    /// </summary>
    procedure GetDefaultClient(var aClient : ICefClient);
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
    /// cef_command_line_t object passed to this function. The
    /// cef_settings_t.command_line_args_disabled value can be used to start with
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
    /// Return the handler for resource bundle events. If
    /// cef_settings_t.pack_loading_disabled is true (1) a handler must be
    /// returned. If no handler is returned resources will be loaded from pack
    /// files. This function is called by the browser and render processes on
    /// multiple threads.
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
    function  VisitAllCookies(const visitor: ICefCookieVisitor): Boolean;
    function  VisitAllCookiesProc(const visitor: TCefCookieVisitorProc): Boolean;
    function  VisitUrlCookies(const url: ustring; includeHttpOnly: Boolean; const visitor: ICefCookieVisitor): Boolean;
    function  VisitUrlCookiesProc(const url: ustring; includeHttpOnly: Boolean; const visitor: TCefCookieVisitorProc): Boolean;
    function  SetCookie(const url, name, value, domain, path: ustring; secure, httponly, hasExpires: Boolean; const creation, lastAccess, expires: TDateTime; same_site : TCefCookieSameSite; priority : TCefCookiePriority; const callback: ICefSetCookieCallback): Boolean;
    function  SetCookieProc(const url: ustring; const name, value, domain, path: ustring; secure, httponly, hasExpires: Boolean; const creation, lastAccess, expires: TDateTime; same_site : TCefCookieSameSite; priority : TCefCookiePriority; const callback: TCefSetCookieCallbackProc): Boolean;
    function  DeleteCookies(const url, cookieName: ustring; const callback: ICefDeleteCookiesCallback): Boolean;
    function  DeleteCookiesProc(const url, cookieName: ustring; const callback: TCefDeleteCookiesCallbackProc): Boolean;
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
    procedure Cont;
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
    /// WARNING: This function is deprecated. Use Open instead.
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
    /// WARNING: This function is deprecated. Use Skip and Read instead.
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
    procedure Cont(const username, password: ustring);
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
    ///
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
    ///
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
    function IsSubMenu: Boolean;
    function Clear: Boolean;
    function GetCount: NativeUInt;
    function AddSeparator: Boolean;
    function AddItem(commandId: Integer; const text: ustring): Boolean;
    function AddCheckItem(commandId: Integer; const text: ustring): Boolean;
    function AddRadioItem(commandId: Integer; const text: ustring; groupId: Integer): Boolean;
    function AddSubMenu(commandId: Integer; const text: ustring): ICefMenuModel;
    function InsertSeparatorAt(index: NativeUInt): Boolean;
    function InsertItemAt(index: NativeUInt; commandId: Integer; const text: ustring): Boolean;
    function InsertCheckItemAt(index: NativeUInt; commandId: Integer; const text: ustring): Boolean;
    function InsertRadioItemAt(index: NativeUInt; commandId: Integer; const text: ustring; groupId: Integer): Boolean;
    function InsertSubMenuAt(index: NativeUInt; commandId: Integer; const text: ustring): ICefMenuModel;
    function Remove(commandId: Integer): Boolean;
    function RemoveAt(index: NativeUInt): Boolean;
    function GetIndexOf(commandId: Integer): Integer;
    function GetCommandIdAt(index: NativeUInt): Integer;
    function SetCommandIdAt(index: NativeUInt; commandId: Integer): Boolean;
    function GetLabel(commandId: Integer): ustring;
    function GetLabelAt(index: NativeUInt): ustring;
    function SetLabel(commandId: Integer; const text: ustring): Boolean;
    function SetLabelAt(index: NativeUInt; const text: ustring): Boolean;
    function GetType(commandId: Integer): TCefMenuItemType;
    function GetTypeAt(index: NativeUInt): TCefMenuItemType;
    function GetGroupId(commandId: Integer): Integer;
    function GetGroupIdAt(index: NativeUInt): Integer;
    function SetGroupId(commandId, groupId: Integer): Boolean;
    function SetGroupIdAt(index: NativeUInt; groupId: Integer): Boolean;
    function GetSubMenu(commandId: Integer): ICefMenuModel;
    function GetSubMenuAt(index: NativeUInt): ICefMenuModel;
    function IsVisible(commandId: Integer): Boolean;
    function isVisibleAt(index: NativeUInt): Boolean;
    function SetVisible(commandId: Integer; visible: Boolean): Boolean;
    function SetVisibleAt(index: NativeUInt; visible: Boolean): Boolean;
    function IsEnabled(commandId: Integer): Boolean;
    function IsEnabledAt(index: NativeUInt): Boolean;
    function SetEnabled(commandId: Integer; enabled: Boolean): Boolean;
    function SetEnabledAt(index: NativeUInt; enabled: Boolean): Boolean;
    function IsChecked(commandId: Integer): Boolean;
    function IsCheckedAt(index: NativeUInt): Boolean;
    function setChecked(commandId: Integer; checked: Boolean): Boolean;
    function setCheckedAt(index: NativeUInt; checked: Boolean): Boolean;
    function HasAccelerator(commandId: Integer): Boolean;
    function HasAcceleratorAt(index: NativeUInt): Boolean;
    function SetAccelerator(commandId, keyCode: Integer; shiftPressed, ctrlPressed, altPressed: Boolean): Boolean;
    function SetAcceleratorAt(index: NativeUInt; keyCode: Integer; shiftPressed, ctrlPressed, altPressed: Boolean): Boolean;
    function RemoveAccelerator(commandId: Integer): Boolean;
    function RemoveAcceleratorAt(index: NativeUInt): Boolean;
    function GetAccelerator(commandId: Integer; out keyCode: Integer; out shiftPressed, ctrlPressed, altPressed: Boolean): Boolean;
    function GetAcceleratorAt(index: NativeUInt; out keyCode: Integer; out shiftPressed, ctrlPressed, altPressed: Boolean): Boolean;
    function SetColor(commandId: Integer; colorType: TCefMenuColorType; color: TCefColor): Boolean;
    function SetColorAt(index: Integer; colorType: TCefMenuColorType; color: TCefColor): Boolean;
    function GetColor(commandId: Integer; colorType: TCefMenuColorType; out color: TCefColor): Boolean;
    function GetColorAt(index: Integer; colorType: TCefMenuColorType; out color: TCefColor): Boolean;
    function SetFontList(commandId: Integer; const fontList: ustring): Boolean;
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
    function IsValid: Boolean;
    function IsOwned: Boolean;
    function IsReadOnly: Boolean;
    function IsSame(const that: ICefValue): Boolean;
    function IsEqual(const that: ICefValue): Boolean;
    function Copy: ICefValue;
    function GetType: TCefValueType;
    function GetBool: Boolean;
    function GetInt: Integer;
    function GetDouble: Double;
    function GetString: ustring;
    function GetBinary: ICefBinaryValue;
    function GetDictionary: ICefDictionaryValue;
    function GetList: ICefListValue;
    function SetNull: Boolean;
    function SetBool(value: boolean): Boolean;
    function SetInt(value: Integer): Boolean;
    function SetDouble(value: Double): Boolean;
    function SetString(const value: ustring): Boolean;
    function SetBinary(const value: ICefBinaryValue): Boolean;
    function SetDictionary(const value: ICefDictionaryValue): Boolean;
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
    function IsValid: Boolean;
    function IsOwned: Boolean;
    function IsSame(const that: ICefBinaryValue): Boolean;
    function IsEqual(const that: ICefBinaryValue): Boolean;
    function Copy: ICefBinaryValue;
    function GetSize: NativeUInt;
    function GetData(buffer: Pointer; bufferSize, dataOffset: NativeUInt): NativeUInt;

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
    function IsValid: Boolean;
    function isOwned: Boolean;
    function IsReadOnly: Boolean;
    function IsSame(const that: ICefDictionaryValue): Boolean;
    function IsEqual(const that: ICefDictionaryValue): Boolean;
    function Copy(excludeEmptyChildren: Boolean): ICefDictionaryValue;
    function GetSize: NativeUInt;
    function Clear: Boolean;
    function HasKey(const key: ustring): Boolean;
    function GetKeys(const keys: TStrings): Boolean;
    function Remove(const key: ustring): Boolean;
    function GetType(const key: ustring): TCefValueType;
    function GetValue(const key: ustring): ICefValue;
    function GetBool(const key: ustring): Boolean;
    function GetInt(const key: ustring): Integer;
    function GetDouble(const key: ustring): Double;
    function GetString(const key: ustring): ustring;
    function GetBinary(const key: ustring): ICefBinaryValue;
    function GetDictionary(const key: ustring): ICefDictionaryValue;
    function GetList(const key: ustring): ICefListValue;
    function SetValue(const key: ustring; const value: ICefValue): Boolean;
    function SetNull(const key: ustring): Boolean;
    function SetBool(const key: ustring; value: Boolean): Boolean;
    function SetInt(const key: ustring; value: Integer): Boolean;
    function SetDouble(const key: ustring; value: Double): Boolean;
    function SetString(const key, value: ustring): Boolean;
    function SetBinary(const key: ustring; const value: ICefBinaryValue): Boolean;
    function SetDictionary(const key: ustring; const value: ICefDictionaryValue): Boolean;
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
    function IsValid: Boolean;
    function IsOwned: Boolean;
    function IsReadOnly: Boolean;
    function IsSame(const that: ICefListValue): Boolean;
    function IsEqual(const that: ICefListValue): Boolean;
    function Copy: ICefListValue;
    function SetSize(size: NativeUInt): Boolean;
    function GetSize: NativeUInt;
    function Clear: Boolean;
    function Remove(index: NativeUInt): Boolean;
    function GetType(index: NativeUInt): TCefValueType;
    function GetValue(index: NativeUInt): ICefValue;
    function GetBool(index: NativeUInt): Boolean;
    function GetInt(index: NativeUInt): Integer;
    function GetDouble(index: NativeUInt): Double;
    function GetString(index: NativeUInt): ustring;
    function GetBinary(index: NativeUInt): ICefBinaryValue;
    function GetDictionary(index: NativeUInt): ICefDictionaryValue;
    function GetList(index: NativeUInt): ICefListValue;
    function SetValue(index: NativeUInt; const value: ICefValue): Boolean;
    function SetNull(index: NativeUInt): Boolean;
    function SetBool(index: NativeUInt; value: Boolean): Boolean;
    function SetInt(index: NativeUInt; value: Integer): Boolean;
    function SetDouble(index: NativeUInt; value: Double): Boolean;
    function SetString(index: NativeUInt; const value: ustring): Boolean;
    function SetBinary(index: NativeUInt; const value: ICefBinaryValue): Boolean;
    function SetDictionary(index: NativeUInt; const value: ICefDictionaryValue): Boolean;
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
    /// wrapped in a cef_browser_view_t. Popup browser creation will be canceled
    /// if the parent browser is destroyed before the popup browser creation
    /// completes (indicated by a call to OnAfterCreated for the popup browser).
    /// The |extra_info| parameter provides an opportunity to specify extra
    /// information specific to the created popup browser that will be passed to
    /// cef_render_process_handler_t::on_browser_created() in the render process.
    /// </summary>
    function  OnBeforePopup(const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean): Boolean;
    /// <summary>
    /// Called after a new browser is created. It is now safe to begin performing
    /// actions with |browser|. cef_frame_handler_t callbacks related to initial
    /// main frame creation will arrive before this callback. See
    /// cef_frame_handler_t documentation for additional usage information.
    /// </summary>
    procedure OnAfterCreated(const browser: ICefBrowser);
    /// <summary>
    /// Called when a browser has recieved a request to close. This may result
    /// directly from a call to cef_browser_host_t::*close_browser() or indirectly
    /// if the browser is parented to a top-level window created by CEF and the
    /// user attempts to close that window (by clicking the 'X', for example). The
    /// do_close() function will be called after the JavaScript 'onunload' event
    /// has been fired.
    ///
    /// An application should handle top-level owner window close notifications by
    /// calling cef_browser_host_t::try_close_browser() or
    /// cef_browser_host_t::CloseBrowser(false (0)) instead of allowing the window
    /// to close immediately (see the examples below). This gives CEF an
    /// opportunity to process the 'onbeforeunload' event and optionally cancel
    /// the close before do_close() is called.
    ///
    /// When windowed rendering is enabled CEF will internally create a window or
    /// view to host the browser. In that case returning false (0) from do_close()
    /// will send the standard close notification to the browser's top-level owner
    /// window (e.g. WM_CLOSE on Windows, performClose: on OS X, "delete_event" on
    /// Linux or cef_window_delegate_t::can_close() callback from Views). If the
    /// browser's host window/view has already been destroyed (via view hierarchy
    /// tear-down, for example) then do_close() will not be called for that
    /// browser since is no longer possible to cancel the close.
    ///
    /// When windowed rendering is disabled returning false (0) from do_close()
    /// will cause the browser object to be destroyed immediately.
    ///
    /// If the browser's top-level owner window requires a non-standard close
    /// notification then send that notification from do_close() and return true
    /// (1).
    ///
    /// The cef_life_span_handler_t::on_before_close() function will be called
    /// after do_close() (if do_close() is called) and immediately before the
    /// browser object is destroyed. The application should only exit after
    /// on_before_close() has been called for all existing browsers.
    ///
    /// The below examples describe what should happen during window close when
    /// the browser is parented to an application-provided top-level window.
    ///
    /// Example 1: Using cef_browser_host_t::try_close_browser(). This is
    /// recommended for clients using standard close handling and windows created
    /// on the browser process UI thread.
    /// 1.  User clicks the window close button which sends a close notification
    ///     to the application's top-level window.
    /// 2.  Application's top-level window receives the close notification and
    ///     calls TryCloseBrowser() (which internally calls CloseBrowser(false)).
    ///     TryCloseBrowser() returns false so the client cancels the window
    ///     close.
    /// 3.  JavaScript 'onbeforeunload' handler executes and shows the close
    ///     confirmation dialog (which can be overridden via
    ///     CefJSDialogHandler::OnBeforeUnloadDialog()).
    /// 4.  User approves the close.
    /// 5.  JavaScript 'onunload' handler executes.
    /// 6.  CEF sends a close notification to the application's top-level window
    ///     (because DoClose() returned false by default).
    /// 7.  Application's top-level window receives the close notification and
    ///     calls TryCloseBrowser(). TryCloseBrowser() returns true so the client
    ///     allows the window close.
    /// 8.  Application's top-level window is destroyed.
    /// 9.  Application's on_before_close() handler is called and the browser object is destroyed.
    /// 10. Application exits by calling cef_quit_message_loop() if no other browsers exist.
    ///
    /// Example 2: Using cef_browser_host_t::CloseBrowser(false (0)) and
    /// implementing the do_close() callback. This is recommended for clients
    /// using non-standard close handling or windows that were not created on the
    /// browser process UI thread.
    /// 1.  User clicks the window close button which sends a close notification
    ///     to the application's top-level window.
    /// 2.  Application's top-level window receives the close notification and:
    ///     A. Calls CefBrowserHost::CloseBrowser(false).
    ///     B. Cancels the window close.
    /// 3.  JavaScript 'onbeforeunload' handler executes and shows the close
    ///     confirmation dialog (which can be overridden via
    ///     CefJSDialogHandler::OnBeforeUnloadDialog()).
    /// 4.  User approves the close.
    /// 5.  JavaScript 'onunload' handler executes.
    /// 6.  Application's do_close() handler is called. Application will:
    ///     A. Set a flag to indicate that the next close attempt will be allowed.
    ///     B. Return false.
    /// 7.  CEF sends an close notification to the application's top-level window.
    /// 8.  Application's top-level window receives the close notification and
    ///     allows the window to close based on the flag from #6B.
    /// 9.  Application's top-level window is destroyed.
    /// 10. Application's on_before_close() handler is called and the browser object is destroyed.
    /// 11. Application exits by calling cef_quit_message_loop() if no other browsers exist.
    /// </summary>
    function  DoClose(const browser: ICefBrowser): Boolean;
    /// <summary>
    /// Called just before a browser is destroyed. Release all references to the
    /// browser object and do not attempt to execute any functions on the browser
    /// object (other than IsValid, GetIdentifier or IsSame) after this callback
    /// returns. cef_frame_handler_t callbacks related to final main frame
    /// destruction will arrive after this callback and cef_browser_t::IsValid
    /// will return false (0) at that time. Any in-progress network requests
    /// associated with |browser| will be aborted when the browser is destroyed,
    /// and cef_resource_request_handler_t callbacks related to those requests may
    /// still arrive on the IO thread after this callback. See cef_frame_handler_t
    /// and do_close() documentation for additional usage information.
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
    /// will be called after cef_context_menu_handler_t::OnContextMenuCommand.
    /// Only used with the Chrome runtime.
    /// </summary>
    function  OnChromeCommand(const browser: ICefBrowser; command_id: integer; disposition: TCefWindowOpenDisposition): boolean;
    /// <summary>
    /// Called to check if a Chrome app menu item should be visible. Values for
    /// |command_id| can be found in the cef_command_ids.h file. Only called for
    /// menu items that would be visible by default. Only used with the Chrome
    /// runtime.
    /// </summary>
    function  OnIsChromeAppMenuItemVisible(const browser: ICefBrowser; command_id: integer): boolean;
    /// <summary>
    /// Called to check if a Chrome app menu item should be enabled. Values for
    /// |command_id| can be found in the cef_command_ids.h file. Only called for
    /// menu items that would be enabled by default. Only used with the Chrome
    /// runtime.
    /// </summary>
    function  OnIsChromeAppMenuItemEnabled(const browser: ICefBrowser; command_id: integer): boolean;
    /// <summary>
    /// Called during browser creation to check if a Chrome page action icon
    /// should be visible. Only called for icons that would be visible by default.
    /// Only used with the Chrome runtime.
    /// </summary>
    function  OnIsChromePageActionIconVisible(icon_type: TCefChromePageActionIconType): boolean;
    /// <summary>
    /// Called during browser creation to check if a Chrome toolbar button should
    /// be visible. Only called for buttons that would be visible by default. Only
    /// used with the Chrome runtime.
    /// </summary>
    function  OnIsChromeToolbarButtonVisible(button_type: TCefChromeToolbarButtonType): boolean;
    /// <summary>
    /// Custom procedure to clear all references.
    /// </summary>
    procedure RemoveReferences;
  end;

  /// <summary>
  /// Callback interface used for asynchronous continuation of
  /// ICefExtensionHandler.GetExtensionResource.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefGetExtensionResourceCallback">Implements TCefGetExtensionResourceCallback</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_extension_handler_capi.h">CEF source file: /include/capi/cef_extension_handler_capi.h (cef_get_extension_resource_callback_t)</see></para>
  /// </remarks>
  ICefGetExtensionResourceCallback = interface(ICefBaseRefCounted)
    ['{579C8602-8252-40D0-9E0A-501F32C36C42}']
    /// <summary>
    /// Continue the request. Read the resource contents from |stream|.
    /// </summary>
    procedure cont(const stream: ICefStreamReader);
    /// <summary>
    /// Cancel the request.
    /// </summary>
    procedure cancel;
  end;

  /// <summary>
  /// Implement this interface to handle events related to browser extensions. The
  /// functions of this interface will be called on the UI thread. See
  /// ICefRequestContext.LoadExtension for information about extension
  /// loading.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefExtensionHandler">Implements TCefExtensionHandler</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_extension_handler_capi.h">CEF source file: /include/capi/cef_extension_handler_capi.h (cef_extension_handler_t)</see></para>
  /// </remarks>
  ICefExtensionHandler = interface(ICefBaseRefCounted)
    ['{3234008F-D809-459D-963D-23BA50219648}']
    /// <summary>
    /// Called if the cef_request_context_t::LoadExtension request fails. |result|
    /// will be the error code.
    /// </summary>
    procedure OnExtensionLoadFailed(result: TCefErrorcode);
    /// <summary>
    /// Called if the cef_request_context_t::LoadExtension request succeeds.
    /// |extension| is the loaded extension.
    /// </summary>
    procedure OnExtensionLoaded(const extension: ICefExtension);
    /// <summary>
    /// Called after the cef_extension_t::Unload request has completed.
    /// </summary>
    procedure OnExtensionUnloaded(const extension: ICefExtension);
    /// <summary>
    /// Called when an extension needs a browser to host a background script
    /// specified via the "background" manifest key. The browser will have no
    /// visible window and cannot be displayed. |extension| is the extension that
    /// is loading the background script. |url| is an internally generated
    /// reference to an HTML page that will be used to load the background script
    /// via a "<script>" src attribute. To allow creation of the browser
    /// optionally modify |client| and |settings| and return false (0). To cancel
    /// creation of the browser (and consequently cancel load of the background
    /// script) return true (1). Successful creation will be indicated by a call
    /// to cef_life_span_handler_t::OnAfterCreated, and
    /// cef_browser_host_t::IsBackgroundHost will return true (1) for the
    /// resulting browser. See https://developer.chrome.com/extensions/event_pages
    /// for more information about extension background script usage.
    /// </summary>
    function  OnBeforeBackgroundBrowser(const extension: ICefExtension; const url: ustring; var client: ICefClient; var settings: TCefBrowserSettings) : boolean;
    /// <summary>
    /// Called when an extension API (e.g. chrome.tabs.create) requests creation
    /// of a new browser. |extension| and |browser| are the source of the API
    /// call. |active_browser| may optionally be specified via the windowId
    /// property or returned via the get_active_browser() callback and provides
    /// the default |client| and |settings| values for the new browser. |index| is
    /// the position value optionally specified via the index property. |url| is
    /// the URL that will be loaded in the browser. |active| is true (1) if the
    /// new browser should be active when opened.  To allow creation of the
    /// browser optionally modify |windowInfo|, |client| and |settings| and return
    /// false (0). To cancel creation of the browser return true (1). Successful
    /// creation will be indicated by a call to
    /// cef_life_span_handler_t::OnAfterCreated. Any modifications to |windowInfo|
    /// will be ignored if |active_browser| is wrapped in a cef_browser_view_t.
    /// </summary>
    function  OnBeforeBrowser(const extension: ICefExtension; const browser, active_browser: ICefBrowser; index: Integer; const url: ustring; active: boolean; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings) : boolean;
    /// <summary>
    /// Called when no tabId is specified to an extension API call that accepts a
    /// tabId parameter (e.g. chrome.tabs.*). |extension| and |browser| are the
    /// source of the API call. Return the browser that will be acted on by the
    /// API call or return NULL to act on |browser|. The returned browser must
    /// share the same cef_request_context_t as |browser|. Incognito browsers
    /// should not be considered unless the source extension has incognito access
    /// enabled, in which case |include_incognito| will be true (1).
    /// </summary>
    procedure GetActiveBrowser(const extension: ICefExtension; const browser: ICefBrowser; include_incognito: boolean; var aRsltBrowser: ICefBrowser);
    /// <summary>
    /// Called when the tabId associated with |target_browser| is specified to an
    /// extension API call that accepts a tabId parameter (e.g. chrome.tabs.*).
    /// |extension| and |browser| are the source of the API call. Return true (1)
    /// to allow access of false (0) to deny access. Access to incognito browsers
    /// should not be allowed unless the source extension has incognito access
    /// enabled, in which case |include_incognito| will be true (1).
    /// </summary>
    function  CanAccessBrowser(const extension: ICefExtension; const browser: ICefBrowser; include_incognito: boolean; const target_browser: ICefBrowser): boolean;
    /// <summary>
    /// Called to retrieve an extension resource that would normally be loaded
    /// from disk (e.g. if a file parameter is specified to
    /// chrome.tabs.executeScript). |extension| and |browser| are the source of
    /// the resource request. |file| is the requested relative file path. To
    /// handle the resource request return true (1) and execute |callback| either
    /// synchronously or asynchronously. For the default behavior which reads the
    /// resource from the extension directory on disk return false (0).
    /// Localization substitutions will not be applied to resources handled via
    /// this function.
    ///  </summary>
    function  GetExtensionResource(const extension: ICefExtension; const browser: ICefBrowser; const file_: ustring; const callback: ICefGetExtensionResourceCallback): boolean;
    /// <summary>
    /// Custom procedure to clear all references.
    /// </summary>
    procedure RemoveReferences;
  end;

  /// <summary>
  /// Object representing an extension. Methods may be called on any thread unless
  /// otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefExtension">Implements TCefExtension</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_extension_capi.h">CEF source file: /include/capi/cef_extension_capi.h (cef_extension_t)</see></para>
  /// </remarks>
  ICefExtension = interface(ICefBaseRefCounted)
    ['{D30D1C64-A26F-49C0-AEB7-C55EC68951CA}']
    function  GetIdentifier : ustring;
    function  GetPath : ustring;
    function  GetManifest : ICefDictionaryValue;
    function  IsSame(const that : ICefExtension) : boolean;
    function  GetHandler : ICefExtensionHandler;
    function  GetLoaderContext : ICefRequestContext;
    function  IsLoaded : boolean;
    procedure unload;
    function  GetBrowserActionPopup : ustring;
    function  GetBrowserActionIcon : ustring;
    function  GetPageActionPopup : ustring;
    function  GetPageActionIcon : ustring;
    function  GetOptionsPage : ustring;
    function  GetOptionsUIPage : ustring;
    function  GetBackgroundPage : ustring;
    function  GetURL : ustring;

    property  Identifier          : ustring               read GetIdentifier;
    property  Path                : ustring               read GetPath;
    property  Manifest            : ICefDictionaryValue   read GetManifest;
    property  Handler             : ICefExtensionHandler  read GetHandler;
    property  LoaderContext       : ICefRequestContext    read GetLoaderContext;
    property  BrowserActionPopup  : ustring               read GetBrowserActionPopup;
    property  BrowserActionIcon   : ustring               read GetBrowserActionIcon;
    property  PageActionPopup     : ustring               read GetPageActionPopup;
    property  PageActionIcon      : ustring               read GetPageActionIcon;
    property  OptionsPage         : ustring               read GetOptionsPage;
    property  OptionsUIPage       : ustring               read GetOptionsUIPage;
    property  BackgroundPage      : ustring               read GetBackgroundPage;
    property  URL                 : ustring               read GetURL;
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
    procedure OnLoadError(const browser: ICefBrowser; const frame: ICefFrame; errorCode: Integer; const errorText, failedUrl: ustring);
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
    function InitFilter: Boolean;
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
    /// cef_load_handler_t::OnLoadingStateChange will be called twice in all
    /// cases. If the navigation is allowed cef_load_handler_t::OnLoadStart and
    /// cef_load_handler_t::OnLoadEnd will be called. If the navigation is
    /// canceled cef_load_handler_t::OnLoadError will be called with an
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
    /// to be handled via cef_resource_request_handler_t::GetResourceHandler or it
    /// will be canceled. To allow the resource load to proceed with default
    /// handling return NULL. To specify a handler for the resource return a
    /// cef_resource_request_handler_t object. If this callback returns NULL the
    /// same function will be called on the associated
    /// cef_request_context_handler_t, if any.
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
    /// cef_auth_callback_t::cont() either in this function or at a later time
    /// when the authentication information is available. Return false (0) to
    /// cancel the request immediately.
    /// </summary>
    function  GetAuthCredentials(const browser: ICefBrowser; const originUrl: ustring; isProxy: Boolean; const host: ustring; port: Integer; const realm, scheme: ustring; const callback: ICefAuthCallback): Boolean;
    /// <summary>
    /// Called on the UI thread to handle requests for URLs with an invalid SSL
    /// certificate. Return true (1) and call cef_callback_t functions either in
    /// this function or at a later time to continue or cancel the request. Return
    /// false (0) to cancel the request immediately. If
    /// cef_settings_t.ignore_certificate_errors is set all invalid certificates
    /// will be accepted without calling this function.
    /// </summary>
    function  OnCertificateError(const browser: ICefBrowser; certError: TCefErrorcode; const requestUrl: ustring; const sslInfo: ICefSslInfo; const callback: ICefCallback): Boolean;
    /// <summary>
    /// Called on the UI thread when a client certificate is being requested for
    /// authentication. Return false (0) to use the default behavior and
    /// automatically select the first certificate available. Return true (1) and
    /// call cef_select_client_certificate_callback_t::Select either in this
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
    /// Called on the browser process UI thread when the render process terminates
    /// unexpectedly. |status| indicates how the process terminated.
    /// </summary>
    procedure OnRenderProcessTerminated(const browser: ICefBrowser; status: TCefTerminationStatus);
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
    /// for requests originating from service workers or cef_urlrequest_t. To
    /// optionally filter cookies for the request return a
    /// cef_cookie_access_filter_t object. The |request| object cannot not be
    /// modified in this callback.
    /// </summary>
    procedure GetCookieAccessFilter(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; var aFilter: ICefCookieAccessFilter);
    /// <summary>
    /// Called on the IO thread before a resource request is loaded. The |browser|
    /// and |frame| values represent the source of the request, and may be NULL
    /// for requests originating from service workers or cef_urlrequest_t. To
    /// redirect or change the resource load optionally modify |request|.
    /// Modification of the request URL will be treated as a redirect. Return
    /// RV_CONTINUE to continue the request immediately. Return RV_CONTINUE_ASYNC
    /// and call cef_callback_t functions at a later time to continue or cancel
    /// the request asynchronously. Return RV_CANCEL to cancel the request
    /// immediately.
    /// </summary>
    function  OnBeforeResourceLoad(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const callback: ICefCallback): TCefReturnValue;
    /// <summary>
    /// Called on the IO thread before a resource is loaded. The |browser| and
    /// |frame| values represent the source of the request, and may be NULL for
    /// requests originating from service workers or cef_urlrequest_t. To allow
    /// the resource to load using the default network loader return NULL. To
    /// specify a handler for the resource return a cef_resource_handler_t object.
    /// The |request| object cannot not be modified in this callback.
    /// </summary>
    procedure GetResourceHandler(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; var aResourceHandler : ICefResourceHandler);
    /// <summary>
    /// Called on the IO thread when a resource load is redirected. The |browser|
    /// and |frame| values represent the source of the request, and may be NULL
    /// for requests originating from service workers or cef_urlrequest_t. The
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
    /// be NULL for requests originating from service workers or cef_urlrequest_t.
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
    /// cef_urlrequest_t. |request| and |response| represent the request and
    /// response respectively and cannot be modified in this callback.
    /// </summary>
    procedure GetResourceResponseFilter(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; var aResourceFilter: ICefResponseFilter);
    /// <summary>
    /// Called on the IO thread when a resource load has completed. The |browser|
    /// and |frame| values represent the source of the request, and may be NULL
    /// for requests originating from service workers or cef_urlrequest_t.
    /// |request| and |response| represent the request and response respectively
    /// and cannot be modified in this callback. |status| indicates the load
    /// completion status. |received_content_length| is the number of response
    /// bytes actually read. This function will be called for all requests,
    /// including requests that are aborted due to CEF shutdown or destruction of
    /// the associated browser. In cases where the associated browser is destroyed
    /// this callback may arrive after the cef_life_span_handler_t::OnBeforeClose
    /// callback for that browser. The cef_frame_t::IsValid function can be used
    /// to test for this situation, and care should be taken not to call |browser|
    /// or |frame| functions that modify state (like LoadURL, SendProcessMessage,
    /// etc.) if the frame is invalid.
    /// </summary>
    procedure OnResourceLoadComplete(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; status: TCefUrlRequestStatus; receivedContentLength: Int64);
    /// <summary>
    /// Called on the IO thread to handle requests for URLs with an unknown
    /// protocol component. The |browser| and |frame| values represent the source
    /// of the request, and may be NULL for requests originating from service
    /// workers or cef_urlrequest_t. |request| cannot be modified in this
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
    function CanSendCookie(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const cookie: PCefCookie): boolean;
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
    /// automatically return to its original size and position. The client is
    /// responsible for resizing the browser if desired.
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
    /// cef_browser_host_t::SetAutoResizeEnabled and the contents have auto-
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
    /// implementation. See cef_menu_id_t for the command ids that have default
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
    /// default implementation. See cef_menu_id_t for command IDs that have
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
    /// Called to run a file chooser dialog. |mode| represents the type of dialog
    /// to display. |title| to the title to be used for the dialog and may be NULL
    /// to show the default title ("Open" or "Save" depending on the mode).
    /// |default_file_path| is the path with optional directory and/or file name
    /// component that should be initially selected in the dialog.
    /// |accept_filters| are used to restrict the selectable file types and may
    /// any combination of (a) valid lower-cased MIME types (e.g. "text/*" or
    /// "image/*"), (b) individual file extensions (e.g. ".txt" or ".png"), or (c)
    /// combined description and file extension delimited using "|" and ";" (e.g.
    /// "Image Types|.png;.gif;.jpg"). To display a custom dialog return true (1)
    /// and execute |callback| either inline or at a later time. To display the
    /// default dialog return false (0).
    /// </summary>
    function OnFileDialog(const browser: ICefBrowser; mode: TCefFileDialogMode; const title, defaultFilePath: ustring; const acceptFilters: TStrings; const callback: ICefFileDialogCallback): Boolean;
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
    /// CefScreenInfo.device_scale_factor returned from GetScreenInfo. |type|
    /// indicates whether the element is the view or the popup widget. |buffer|
    /// contains the pixel data for the whole image. |dirtyRects| contains the set
    /// of rectangles in pixel coordinates that need to be repainted. |buffer|
    /// will be |width|*|height|*4 bytes in size and represents a BGRA image with
    /// an upper-left origin. This function is only called when
    /// cef_window_tInfo::shared_texture_enabled is set to false (0).
    /// </summary>
    procedure OnPaint(const browser: ICefBrowser; kind: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; const buffer: Pointer; width, height: Integer);
    /// <summary>
    /// Called when an element has been rendered to the shared texture handle.
    /// |type| indicates whether the element is the view or the popup widget.
    /// |dirtyRects| contains the set of rectangles in pixel coordinates that need
    /// to be repainted. |shared_handle| is the handle for a D3D11 Texture2D that
    /// can be accessed via ID3D11Device using the OpenSharedResource function.
    /// This function is only called when cef_window_tInfo::shared_texture_enabled
    /// is set to true (1), and is currently only supported on Windows.
    /// </summary>
    procedure OnAcceleratedPaint(const browser: ICefBrowser; kind: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; shared_handle: Pointer);
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
    /// cef_browser_host_t::DragSource*Ended* functions after returning false (0).
    ///
    /// Return true (1) to handle the drag operation. Call
    /// cef_browser_host_t::DragSourceEndedAt and DragSourceSystemDragEnded either
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
    /// Return the handler for events related to cef_frame_t lifespan. This
    /// function will be called once during cef_browser_t creation and the result
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
    function  GetRequest: ICefRequest;
    function  GetRequestStatus: TCefUrlRequestStatus;
    function  GetRequestError: Integer;
    function  GetResponse: ICefResponse;
    function  GetResponseWasCached: boolean;
    procedure Cancel;

    property Request           : ICefRequest           read GetRequest;
    property RequestStatus     : TCefUrlRequestStatus  read GetRequestStatus;
    property RequestError      : Integer               read GetRequestError;
    property Response          : ICefResponse          read GetResponse;
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
    procedure OnRequestComplete(const request: ICefUrlRequest);
    procedure OnUploadProgress(const request: ICefUrlRequest; current, total: Int64);
    procedure OnDownloadProgress(const request: ICefUrlRequest; current, total: Int64);
    procedure OnDownloadData(const request: ICefUrlRequest; data: Pointer; dataLength: NativeUInt);
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
  /// Interface used to represent drag data. The functions of this interface may
  /// be called on any thread.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefDragData">Implements TCefDragData</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_drag_data_capi.h">CEF source file: /include/capi/cef_drag_data_capi.h (cef_drag_data_t)</see></para>
  /// </remarks>
  ICefDragData = interface(ICefBaseRefCounted)
    ['{FBB6A487-F633-4055-AB3E-6619EDE75683}']
    function  Clone: ICefDragData;
    function  IsReadOnly: Boolean;
    function  IsLink: Boolean;
    function  IsFragment: Boolean;
    function  IsFile: Boolean;
    function  GetLinkUrl: ustring;
    function  GetLinkTitle: ustring;
    function  GetLinkMetadata: ustring;
    function  GetFragmentText: ustring;
    function  GetFragmentHtml: ustring;
    function  GetFragmentBaseUrl: ustring;
    function  GetFileName: ustring;
    function  GetFileContents(const writer: ICefStreamWriter): NativeUInt;
    function  GetFileNames(var names: TStrings): Integer;
    procedure SetLinkUrl(const url: ustring);
    procedure SetLinkTitle(const title: ustring);
    procedure SetLinkMetadata(const data: ustring);
    procedure SetFragmentText(const text: ustring);
    procedure SetFragmentHtml(const html: ustring);
    procedure SetFragmentBaseUrl(const baseUrl: ustring);
    procedure ResetFileContents;
    procedure AddFile(const path, displayName: ustring);
    procedure ClearFilenames;
    function  GetImage : ICefImage;
    function  GetImageHotspot : TCefPoint;
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
    /// Called to report find results returned by cef_browser_host_t::find().
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
    /// cef_urlrequest_t. |request| represents the request contents and cannot be
    /// modified in this callback. |is_navigation| will be true (1) if the
    /// resource request is a navigation. |is_download| will be true (1) if the
    /// resource request is a download. |request_initiator| is the origin (scheme
    /// + domain) of the page that initiated the request. Set
    /// |disable_default_handling| to true (1) to disable default handling of the
    /// request, in which case it will need to be handled via
    /// cef_resource_request_handler_t::GetResourceHandler or it will be canceled.
    /// To allow the resource load to proceed with default handling return NULL.
    /// To specify a handler for the resource return a
    /// cef_resource_request_handler_t object. This function will not be called if
    /// the client associated with |browser| returns a non-NULL value from
    /// cef_request_handler_t::GetResourceRequestHandler for the same request
    /// (identified by cef_request_t::GetIdentifier).
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
    function  HasPreference(const name: ustring): Boolean;
    function  GetPreference(const name: ustring): ICefValue;
    function  GetAllPreferences(includeDefaults: Boolean): ICefDictionaryValue;
    function  CanSetPreference(const name: ustring): Boolean;
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
    function  IsSame(const other: ICefRequestContext): Boolean;
    function  IsSharingWith(const other: ICefRequestContext): Boolean;
    function  IsGlobal: Boolean;
    function  GetHandler: ICefRequestContextHandler;
    function  GetCachePath: ustring;
    function  GetCookieManager(const callback: ICefCompletionCallback): ICefCookieManager;
    function  GetCookieManagerProc(const callback: TCefCompletionCallbackProc): ICefCookieManager;
    function  RegisterSchemeHandlerFactory(const schemeName, domainName: ustring; const factory: ICefSchemeHandlerFactory): Boolean;
    function  ClearSchemeHandlerFactories: Boolean;
    procedure ClearCertificateExceptions(const callback: ICefCompletionCallback);
    procedure ClearHttpAuthCredentials(const callback: ICefCompletionCallback);
    procedure CloseAllConnections(const callback: ICefCompletionCallback);
    procedure ResolveHost(const origin: ustring; const callback: ICefResolveCallback);
    procedure LoadExtension(const root_directory: ustring; const manifest: ICefDictionaryValue; const handler: ICefExtensionHandler);
    function  DidLoadExtension(const extension_id: ustring): boolean;
    function  HasExtension(const extension_id: ustring): boolean;
    function  GetExtensions(const extension_ids: TStringList): boolean;
    function  GetExtension(const extension_id: ustring): ICefExtension;
    function  GetMediaRouter(const callback: ICefCompletionCallback): ICefMediaRouter;
    function  GetWebsiteSetting(const requesting_url, top_level_url: ustring; content_type: TCefContentSettingTypes): ICefValue;
    procedure SetWebsiteSetting(const requesting_url, top_level_url: ustring; content_type: TCefContentSettingTypes; const value: ICefValue);
    function  GetContentSetting(const requesting_url, top_level_url: ustring; content_type: TCefContentSettingTypes): TCefContentSettingValues;
    procedure SetContentSetting(const requesting_url, top_level_url: ustring; content_type: TCefContentSettingTypes; value: TCefContentSettingValues);

    property  CachePath        : ustring         read GetCachePath;
    property  IsGlobalContext  : boolean         read IsGlobal;
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
    function  IsValid: Boolean;
    function  IsReadOnly: Boolean;
    procedure SetOrientation(landscape: Boolean);
    function  IsLandscape: Boolean;
    procedure SetPrinterPrintableArea(const physicalSizeDeviceUnits: PCefSize; const printableAreaDeviceUnits: PCefRect; landscapeNeedsFlip: Boolean);
    procedure SetDeviceName(const name: ustring);
    function  GetDeviceName: ustring;
    procedure SetDpi(dpi: Integer);
    function  GetDpi: Integer;
    procedure SetPageRanges(const ranges: TCefRangeArray);
    function  GetPageRangesCount: NativeUInt;
    procedure GetPageRanges(out ranges: TCefRangeArray);
    procedure SetSelectionOnly(selectionOnly: Boolean);
    function  IsSelectionOnly: Boolean;
    procedure SetCollate(collate: Boolean);
    function  WillCollate: Boolean;
    procedure SetColorModel(model: TCefColorModel);
    function  GetColorModel: TCefColorModel;
    procedure SetCopies(copies: Integer);
    function  GetCopies: Integer;
    procedure SetDuplexMode(mode: TCefDuplexMode);
    function  GetDuplexMode: TCefDuplexMode;

    property Landscape      : Boolean         read IsLandscape      write SetOrientation;
    property DeviceName     : ustring         read GetDeviceName    write SetDeviceName;
    property Dpi            : Integer         read GetDpi           write SetDpi;
    property SelectionOnly  : Boolean         read IsSelectionOnly  write SetSelectionOnly;
    property Collate        : Boolean         read WillCollate      write SetCollate;
    property ColorModel     : TCefColorModel  read GetColorModel    write SetColorModel;
    property Copies         : Integer         read GetCopies        write SetCopies;
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
    /// cef_browser_host_t::print(), JavaScript window.print() or PDF extension
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
    /// cef_browser_host_t::print_to_pdf().
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
    function IsValid: Boolean;
    function GetUrl: ustring;
    function GetDisplayUrl: ustring;
    function GetOriginalUrl: ustring;
    function GetTitle: ustring;
    function GetTransitionType: TCefTransitionType;
    function HasPostData: Boolean;
    function GetCompletionTime: TDateTime;
    function GetHttpStatusCode: Integer;
    function GetSSLStatus: ICefSSLStatus;

    property Url              : ustring             read GetUrl;
    property DisplayUrl       : ustring             read GetDisplayUrl;
    property OriginalUrl      : ustring             read GetOriginalUrl;
    property Title            : ustring             read GetTitle;
    property TransitionType   : TCefTransitionType  read GetTransitionType;
    property CompletionTime   : TDateTime           read GetCompletionTime;
    property HttpStatusCode   : Integer             read GetHttpStatusCode;
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
    function  GetDisplayName: ustring;
    function  GetCommonName: ustring;
    function  GetLocalityName: ustring;
    function  GetStateOrProvinceName: ustring;
    function  GetCountryName: ustring;
    procedure GetOrganizationNames(const names: TStrings);
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
    function  GetSubject: ICefX509CertPrincipal;
    function  GetIssuer: ICefX509CertPrincipal;
    function  GetSerialNumber: ICefBinaryValue;
    function  GetValidStart: TCefBaseTime;
    function  GetValidExpiry: TCefBaseTime;
    function  GetValidStartAsDateTime: TDateTime;
    function  GetValidExpiryAsDateTime: TDateTime;
    function  GetDerEncoded: ICefBinaryValue;
    function  GetPemEncoded: ICefBinaryValue;
    function  GetIssuerChainSize: NativeUInt;
    procedure GetDEREncodedIssuerChain(chainCount: NativeUInt; var chain : TCefBinaryValueArray);
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
    function GetCertStatus: TCefCertStatus;
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
    function IsSecureConnection: boolean;
    function GetCertStatus: TCefCertStatus;
    function GetSSLVersion: TCefSSLVersion;
    function GetContentStatus: TCefSSLContentStatus;
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
    function GetLocalizedString(stringId: Integer): ustring;
    function GetDataResource(resourceId: Integer): ICefBinaryValue;
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
    function IsEmpty: Boolean;
    function IsSame(const that: ICefImage): Boolean;
    function AddBitmap(scaleFactor: Single; pixelWidth, pixelHeight: Integer; colorType: TCefColorType; alphaType: TCefAlphaType; const pixelData: Pointer; pixelDataSize: NativeUInt): Boolean;
    function AddPng(scaleFactor: Single; const pngData: Pointer; pngDataSize: NativeUInt): Boolean;
    function AddJpeg(scaleFactor: Single; const jpegData: Pointer; jpegDataSize: NativeUInt): Boolean;
    function GetWidth: NativeUInt;
    function GetHeight: NativeUInt;
    function HasRepresentation(scaleFactor: Single): Boolean;
    function RemoveRepresentation(scaleFactor: Single): Boolean;
    function GetRepresentationInfo(scaleFactor: Single; var actualScaleFactor: Single; var pixelWidth, pixelHeight: Integer): Boolean;
    function GetAsBitmap(scaleFactor: Single; colorType: TCefColorType; alphaType: TCefAlphaType; var pixelWidth, pixelHeight: Integer): ICefBinaryValue;
    function GetAsPng(scaleFactor: Single; withTransparency: Boolean; var pixelWidth, pixelHeight: Integer): ICefBinaryValue;
    function GetAsJpeg(scaleFactor: Single; quality: Integer; var pixelWidth, pixelHeight: Integer): ICefBinaryValue;

    property Width  : NativeUInt read GetWidth;
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
    procedure ExecuteCommand(const menuModel: ICefMenuModel; commandId: Integer; eventFlags: TCefEventFlags);
    procedure MouseOutsideMenu(const menuModel: ICefMenuModel; const screenPoint: PCefPoint);
    procedure UnhandledOpenSubmenu(const menuModel: ICefMenuModel; isRTL: boolean);
    procedure UnhandledCloseSubmenu(const menuModel: ICefMenuModel; isRTL: boolean);
    procedure MenuWillShow(const menuModel: ICefMenuModel);
    procedure MenuClosed(const menuModel: ICefMenuModel);
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
    function  GetTaskRunner : ICefTaskRunner;
    procedure Shutdown;
    function  IsRunning : boolean;
    function  GetAddress : ustring;
    function  HasConnection : boolean;
    function  IsValidConnection(connection_id: Integer) : boolean;
    procedure SendHttp200response(connection_id: Integer; const content_type: ustring; const data: Pointer; data_size: NativeUInt);
    procedure SendHttp404response(connection_id: Integer);
    procedure SendHttp500response(connection_id: Integer; const error_message: ustring);
    procedure SendHttpResponse(connection_id, response_code: Integer; const content_type: ustring; content_length: int64; const extra_headers: ICefStringMultimap);
    procedure SendRawData(connection_id: Integer; const data: Pointer; data_size: NativeUInt);
    procedure CloseConnection(connection_id: Integer);
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
    procedure OnServerCreated(const server: ICefServer);
    procedure OnServerDestroyed(const server: ICefServer);
    procedure OnClientConnected(const server: ICefServer; connection_id: Integer);
    procedure OnClientDisconnected(const server: ICefServer; connection_id: Integer);
    procedure OnHttpRequest(const server: ICefServer; connection_id: Integer; const client_address: ustring; const request: ICefRequest);
    procedure OnWebSocketRequest(const server: ICefServer; connection_id: Integer; const client_address: ustring; const request: ICefRequest; const callback: ICefCallback);
    procedure OnWebSocketConnected(const server: ICefServer; connection_id: Integer);
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
    /// Called when a page requests permission to access media.
    /// |requesting_origin| is the URL origin requesting permission.
    /// |requested_permissions| is a combination of values from
    /// cef_media_access_permission_types_t that represent the requested
    /// permissions. Return true (1) and call cef_media_access_callback_t
    /// functions either in this function or at a later time to continue or cancel
    /// the request. Return false (0) to proceed with default handling. With the
    /// Chrome runtime, default handling will display the permission request UI.
    /// With the Alloy runtime, default handling will deny the request. This
    /// function will not be called if the "--enable-media-stream" command-line
    /// switch is used to grant all permissions.
    /// </summary>
    function  OnRequestMediaAccessPermission(const browser: ICefBrowser; const frame: ICefFrame; const requesting_origin: ustring; requested_permissions: cardinal; const callback: ICefMediaAccessCallback): boolean;
    /// <summary>
    /// Called when a page should show a permission prompt. |prompt_id| uniquely
    /// identifies the prompt. |requesting_origin| is the URL origin requesting
    /// permission. |requested_permissions| is a combination of values from
    /// cef_permission_request_types_t that represent the requested permissions.
    /// Return true (1) and call cef_permission_prompt_callback_t::Continue either
    /// in this function or at a later time to continue or cancel the request.
    /// Return false (0) to proceed with default handling. With the Chrome
    /// runtime, default handling will display the permission prompt UI. With the
    /// Alloy runtime, default handling is CEF_PERMISSION_RESULT_IGNORE.
    /// </summary>
    function  OnShowPermissionPrompt(const browser: ICefBrowser; prompt_id: uint64; const requesting_origin: ustring; requested_permissions: cardinal; const callback: ICefPermissionPromptCallback): boolean;
    /// <summary>
    /// Called when a permission prompt handled via OnShowPermissionPrompt is
    /// dismissed. |prompt_id| will match the value that was passed to
    /// OnShowPermissionPrompt. |result| will be the value passed to
    /// cef_permission_prompt_callback_t::Continue or CEF_PERMISSION_RESULT_IGNORE
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
    function IsValid: boolean;
    function Size: NativeUInt;
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
    function IsValid: boolean;
    function Size: NativeUInt;
    function Memory: pointer;
    function Build: ICefProcessMessage;
  end;



  // *********************************
  // ************* Views *************
  // *********************************

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
    function  GetID : int64;
    function  GetDeviceScaleFactor : Single;
    procedure ConvertPointToPixels(var point: TCefPoint);
    procedure ConvertPointFromPixels(var point: TCefPoint);
    function  GetBounds : TCefRect;
    function  GetWorkArea : TCefRect;
    function  GetRotation : Integer;

    property  ID                : int64      read GetID;
    property  DeviceScaleFactor : Single     read GetDeviceScaleFactor;
    property  Bounds            : TCefRect   read GetBounds;
    property  WorkArea          : TCefRect   read GetWorkArea;
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
    function AsBoxLayout : ICefBoxLayout;
    function AsFillLayout : ICefFillLayout;
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
    procedure SetFlexForView(const view: ICefView; flex: Integer);
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
    function  IsValid: boolean;
    function  IsSame(const that: ICefOverlayController): boolean;
    function  GetContentsView: ICefView;
    function  GetWindow: ICefWindow;
    function  GetDockingMode: TCefDockingMode;
    procedure DestroyOverlay;
    procedure SetBounds(const bounds: TCefRect);
    function  GetBounds: TCefRect;
    function  GetBoundsInScreen: TCefRect;
    procedure SetSize(const size: TCefSize);
    function  GetSize: TCefSize;
    procedure SetPosition(const position: TCefPoint);
    function  GetPosition: TCefPoint;
    procedure SetInsets(const insets: TCefInsets);
    function  GetInsets: TCefInsets;
    procedure SizeToPreferredSize;
    procedure SetVisible(visible: boolean);
    function  IsVisible: boolean;
    function  IsDrawn: boolean;
    property ContentsView   : ICefView          read GetContentsView;
    property Window         : ICefWindow        read GetWindow;
    property DockingMode    : TCefDockingMode   read GetDockingMode;
    property Bounds         : TCefRect          read GetBounds           write SetBounds;
    property BoundsInScreen : TCefRect          read GetBoundsInScreen;
    property Size           : TCefSize          read GetSize             write SetSize;
    property Position       : TCefPoint         read GetPosition         write SetPosition;
    property Insets         : TCefInsets        read GetInsets           write SetInsets;
    property Visible        : boolean           read IsVisible           write SetVisible;
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
    function  AsBrowserView : ICefBrowserView;
    function  AsButton : ICefButton;
    function  AsPanel : ICefPanel;
    function  AsScrollView : ICefScrollView;
    function  AsTextfield : ICefTextfield;
    function  GetTypeString : ustring;
    function  ToStringEx(include_children: boolean): ustring;
    function  IsValid : boolean;
    function  IsAttached : boolean;
    function  IsSame(const that: ICefView): boolean;
    function  GetDelegate : ICefViewDelegate;
    function  GetWindow : ICefWindow;
    function  GetID : Integer;
    procedure SetID(id_: Integer);
    function  GetGroupID : Integer;
    procedure SetGroupID(group_id: Integer);
    function  GetParentView : ICefView;
    function  GetViewForID(id_: Integer): ICefView;
    procedure SetBounds(const bounds_: TCefRect);
    function  GetBounds : TCefRect;
    function  GetBoundsInScreen : TCefRect;
    procedure SetSize(const size_: TCefSize);
    function  GetSize : TCefSize;
    procedure SetPosition(const position_: TCefPoint);
    function  GetPosition : TCefPoint;
    procedure SetInsets(const insets: TCefInsets);
    function  GetInsets: TCefInsets;
    function  GetPreferredSize : TCefSize;
    procedure SizeToPreferredSize;
    function  GetMinimumSize : TCefSize;
    function  GetMaximumSize : TCefSize;
    function  GetHeightForWidth(width: Integer): Integer;
    procedure InvalidateLayout;
    procedure SetVisible(visible_: boolean);
    function  IsVisible : boolean;
    function  IsDrawn : boolean;
    procedure SetEnabled(enabled_: boolean);
    function  IsEnabled : boolean;
    procedure SetFocusable(focusable_: boolean);
    function  IsFocusable : boolean;
    function  IsAccessibilityFocusable : boolean;
    procedure RequestFocus;
    procedure SetBackgroundColor(color: TCefColor);
    function  GetBackgroundColor : TCefColor;
    function  ConvertPointToScreen(var point: TCefPoint): boolean;
    function  ConvertPointFromScreen(var point: TCefPoint): boolean;
    function  ConvertPointToWindow(var point: TCefPoint): boolean;
    function  ConvertPointFromWindow(var point: TCefPoint): boolean;
    function  ConvertPointToView(const view : ICefView; var point: TCefPoint): boolean;
    function  ConvertPointFromView(const view : ICefView; var point: TCefPoint): boolean;

    property Valid                  : boolean          read IsValid;
    property Attached               : boolean          read IsAttached;
    property Delegate               : ICefViewDelegate read GetDelegate;
    property Window                 : ICefWindow       read GetWindow;
    property ParentView             : ICefView         read GetParentView;
    property BoundsInScreen         : TCefRect         read GetBoundsInScreen;
    property PreferredSize          : TCefSize         read GetPreferredSize;
    property MinimumSize            : TCefSize         read GetMinimumSize;
    property MaximumSize            : TCefSize         read GetMaximumSize;
    property Visible                : boolean          read IsVisible                  write SetVisible;
    property Drawn                  : boolean          read IsDrawn;
    property Enabled                : boolean          read IsEnabled                  write SetEnabled;
    property Focusable              : boolean          read IsFocusable                write SetFocusable;
    property AccessibilityFocusable : boolean          read IsAccessibilityFocusable;
    property BackgroundColor        : TCefColor        read GetBackgroundColor         write SetBackgroundColor;
    property ID                     : integer          read GetID                      write SetID;
    property GroupID                : integer          read GetGroupID                 write SetGroupID;
    property Bounds                 : TCefRect         read GetBounds                  write SetBounds;
    property Size                   : TCefSize         read GetSize                    write SetSize;
    property Position               : TCefPoint        read GetPosition                write SetPosition;
    property Insets                 : TCefInsets       read GetInsets                  write SetInsets;
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
    procedure OnGetPreferredSize(const view: ICefView; var aResult : TCefSize);
    procedure OnGetMinimumSize(const view: ICefView; var aResult : TCefSize);
    procedure OnGetMaximumSize(const view: ICefView; var aResult : TCefSize);
    procedure OnGetHeightForWidth(const view: ICefView; width: Integer; var aResult: Integer);
    procedure OnParentViewChanged(const view: ICefView; added: boolean; const parent: ICefView);
    procedure OnChildViewChanged(const view: ICefView; added: boolean; const child: ICefView);
    procedure OnWindowChanged(const view: ICefView; added: boolean);
    procedure OnLayoutChanged(const view: ICefView; new_bounds: TCefRect);
    procedure OnFocus(const view: ICefView);
    procedure OnBlur(const view: ICefView);
  end;

  /// <summary>
  /// Custom interface used to handle the ICefViewDelegate events in a component.
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

    // Custom
    procedure doCreateCustomView;
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
    procedure SetPasswordInput(password_input: boolean);
    function  IsPasswordInput : boolean;
    procedure SetReadOnly(read_only: boolean);
    function  IsReadOnly : boolean;
    function  GetText : ustring;
    procedure SetText(const text_: ustring);
    procedure AppendText(const text_: ustring);
    procedure InsertOrReplaceText(const text_: ustring);
    function  HasSelection : boolean;
    function  GetSelectedText : ustring;
    procedure SelectAll(reversed: boolean);
    procedure ClearSelection;
    function  GetSelectedRange : TCefRange;
    procedure SelectRange(const range: TCefRange);
    function  GetCursorPosition : NativeUInt;
    procedure SetTextColor(color: TCefColor);
    function  GetTextColor : TCefColor;
    procedure SetSelectionTextColor(color: TCefColor);
    function  GetSelectionTextColor : TCefColor;
    procedure SetSelectionBackgroundColor(color: TCefColor);
    function  GetSelectionBackgroundColor : TCefColor;
    procedure SetFontList(const font_list: ustring);
    procedure ApplyTextColor(color: TCefColor; const range: TCefRange);
    procedure ApplyTextStyle(style: TCefTextStyle; add: boolean; const range: TCefRange);
    function  IsCommandEnabled(command_id: TCefTextFieldCommands): boolean;
    procedure ExecuteCommand(command_id: TCefTextFieldCommands);
    procedure ClearEditHistory;
    procedure SetPlaceholderText(const text_: ustring);
    function  GetPlaceholderText : ustring;
    procedure SetPlaceholderTextColor(color: TCefColor);
    procedure SetAccessibleName(const name: ustring);

    property  PasswordInput            : boolean       read IsPasswordInput               write SetPasswordInput;
    property  ReadOnly                 : boolean       read IsReadOnly                    write SetReadOnly;
    property  Text                     : ustring       read GetText                       write SetText;
    property  SelectedText             : ustring       read GetSelectedText;
    property  TextColor                : TCefColor     read GetTextColor                  write SetTextColor;
    property  SelectionTextColor       : TCefColor     read GetSelectionTextColor         write SetSelectionTextColor;
    property  SelectionBackgroundColor : TCefColor     read GetSelectionBackgroundColor   write SetSelectionBackgroundColor;
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
    procedure OnKeyEvent(const textfield: ICefTextfield; const event: TCefKeyEvent; var aResult : boolean);
    procedure OnAfterUserAction(const textfield: ICefTextfield);
  end;

  ICefTextfieldDelegateEvents = interface(ICefViewDelegateEvents)
    ['{682480E0-C786-4E65-B950-4FF2B13B97B9}']
    procedure doOnKeyEvent(const textfield: ICefTextfield; const event: TCefKeyEvent; var aResult : boolean);
    procedure doOnAfterUserAction(const textfield: ICefTextfield);
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
    procedure SetContentView(const view: ICefView);
    function  GetContentView : ICefView;
    function  GetVisibleContentRect : TCefRect;
    function  HasHorizontalScrollbar : boolean;
    function  GetHorizontalScrollbarHeight : Integer;
    function  HasVerticalScrollbar : boolean;
    function  GetVerticalScrollbarWidth : Integer;

    property  ContentView               : ICefView      read GetContentView                write SetContentView;
    property  VisibleContentRect        : TCefRect      read GetVisibleContentRect;
    property  HorizontalScrollbarHeight : Integer       read GetHorizontalScrollbarHeight;
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
    function  GetAsWindow : ICefWindow;
    function  SetToFillLayout : ICefFillLayout;
    function  SetToBoxLayout(const settings: TCefBoxLayoutSettings): ICefBoxLayout;
    function  GetLayout : ICefLayout;
    procedure Layout;
    procedure AddChildView(const view: ICefView);
    procedure AddChildViewAt(const view: ICefView; index: Integer);
    procedure ReorderChildView(const view: ICefView; index: Integer);
    procedure RemoveChildView(const view: ICefView);
    procedure RemoveAllChildViews;
    function  GetChildViewCount : NativeUInt;
    function  GetChildViewAt(index: Integer): ICefView;

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
  /// Custom interface used to handle the ICefPanelDelegate events in a component.
  /// </summary>
  ICefPanelDelegateEvents = interface(ICefViewDelegateEvents)
    ['{F1F2963F-82C3-48F0-9B9C-7C213BACB96B}']
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
    function  GetBrowser : ICefBrowser;
    function  GetChromeToolbar : ICefView;
    procedure SetPreferAccelerators(prefer_accelerators: boolean);
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
    procedure OnBrowserCreated(const browser_view: ICefBrowserView; const browser: ICefBrowser);
    procedure OnBrowserDestroyed(const browser_view: ICefBrowserView; const browser: ICefBrowser);
    procedure OnGetDelegateForPopupBrowserView(const browser_view: ICefBrowserView; const settings: TCefBrowserSettings; const client: ICefClient; is_devtools: boolean; var aResult : ICefBrowserViewDelegate);
    procedure OnPopupBrowserViewCreated(const browser_view, popup_browser_view: ICefBrowserView; is_devtools: boolean; var aResult : boolean);
    function  GetChromeToolbarType: TCefChromeToolbarType;
    procedure OnGestureCommand(const browser_view: ICefBrowserView; gesture_command: TCefGestureCommand; var aResult : boolean);

    property ChromeToolbarType: TCefChromeToolbarType read GetChromeToolbarType;
  end;

  /// <summary>
  /// Custom interface used to handle the ICefBrowserViewDelegate events in a component.
  /// </summary>
  ICefBrowserViewDelegateEvents = interface(ICefViewDelegateEvents)
    ['{AB94B875-63C6-4FEF-BB30-0816402ABA1C}']
    procedure doOnBrowserCreated(const browser_view: ICefBrowserView; const browser: ICefBrowser);
    procedure doOnBrowserDestroyed(const browser_view: ICefBrowserView; const browser: ICefBrowser);
    procedure doOnGetDelegateForPopupBrowserView(const browser_view: ICefBrowserView; const settings: TCefBrowserSettings; const client: ICefClient; is_devtools: boolean; var aResult : ICefBrowserViewDelegate);
    procedure doOnPopupBrowserViewCreated(const browser_view, popup_browser_view: ICefBrowserView; is_devtools: boolean; var aResult : boolean);
    procedure doOnGetChromeToolbarType(var aChromeToolbarType: TCefChromeToolbarType);
    procedure doOnGestureCommand(const browser_view: ICefBrowserView; gesture_command: TCefGestureCommand; var aResult : boolean);
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
    function  AsLabelButton : ICefLabelButton;
    procedure SetState(state_: TCefButtonState);
    function  GetState : TCefButtonState;
    procedure SetInkDropEnabled(enabled_: boolean);
    procedure SetTooltipText(const tooltip_text: ustring);
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
    procedure OnButtonPressed(const button: ICefButton);
    procedure OnButtonStateChanged(const button: ICefButton);
  end;

  /// <summary>
  /// Custom interface used to handle the ICefButtonDelegate events in a component.
  /// </summary>
  ICefButtonDelegateEvents = interface(ICefViewDelegateEvents)
    ['{E8DF70BE-5DEB-42CF-AF86-B0FF1040498E}']
    procedure doOnButtonPressed(const button: ICefButton);
    procedure doOnButtonStateChanged(const button: ICefButton);
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
    function  AsMenuButton : ICefMenuButton;
    procedure SetText(const text_: ustring);
    function  GetText : ustring;
    procedure SetImage(button_state: TCefButtonState; const image: ICefImage);
    function  GetImage(button_state: TCefButtonState): ICefImage;
    procedure SetTextColor(for_state: TCefButtonState; color: TCefColor);
    procedure SetEnabledTextColors(color: TCefColor);
    procedure SetFontList(const font_list: ustring);
    procedure SetHorizontalAlignment(alignment: TCefHorizontalAlignment);
    procedure SetMinimumSize(const size_: TCefSize);
    procedure SetMaximumSize(const size_: TCefSize);

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
    procedure ShowMenu(const menu_model: ICefMenuModel; const screen_point: TCefPoint; anchor_position: TCefMenuAnchorPosition);
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
    procedure OnMenuButtonPressed(const menu_button: ICefMenuButton; const screen_point: TCefPoint; const button_pressed_lock: ICefMenuButtonPressedLock);
  end;

  /// <summary>
  /// Custom interface used to handle the ICefMenuButtonDelegate events in a component.
  /// </summary>
  ICefMenuButtonDelegateEvents = interface(ICefButtonDelegateEvents)
    ['{DA36DD60-7609-4576-BB8E-6A55FD48C680}']
    procedure doOnMenuButtonPressed(const menu_button: ICefMenuButton; const screen_point: TCefPoint; const button_pressed_lock: ICefMenuButtonPressedLock);
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
    procedure Show;
    procedure ShowAsBrowserModalDialog(const browser_view: ICefBrowserView);
    procedure Hide;
    procedure CenterWindow(const size_: TCefSize);
    procedure Close;
    function  IsClosed : boolean;
    procedure Activate;
    procedure Deactivate;
    function  IsActive : boolean;
    procedure BringToTop;
    procedure SetAlwaysOnTop(on_top: boolean);
    function  IsAlwaysOnTop : boolean;
    procedure Maximize;
    procedure Minimize;
    procedure Restore;
    procedure SetFullscreen(fullscreen: boolean);
    function  IsMaximized : boolean;
    function  IsMinimized : boolean;
    function  IsFullscreen : boolean;
    procedure SetTitle(const title_: ustring);
    function  GetTitle : ustring;
    procedure SetWindowIcon(const image: ICefImage);
    function  GetWindowIcon : ICefImage;
    procedure SetWindowAppIcon(const image: ICefImage);
    function  GetWindowAppIcon : ICefImage;
    function  AddOverlayView(const view: ICefView; docking_mode: TCefDockingMode): ICefOverlayController;
    procedure ShowMenu(const menu_model: ICefMenuModel; const screen_point: TCefPoint; anchor_position : TCefMenuAnchorPosition);
    procedure CancelMenu;
    function  GetDisplay : ICefDisplay;
    function  GetClientAreaBoundsInScreen : TCefRect;
    procedure SetDraggableRegions(regionsCount: NativeUInt; const regions: PCefDraggableRegionArray);
    function  GetWindowHandle : TCefWindowHandle;
    procedure SendKeyPress(key_code: Integer; event_flags: cardinal);
    procedure SendMouseMove(screen_x, screen_y: Integer);
    procedure SendMouseEvents(button: TCefMouseButtonType; mouse_down, mouse_up: boolean);
    procedure SetAccelerator(command_id, key_code : Integer; shift_pressed, ctrl_pressed, alt_pressed: boolean);
    procedure RemoveAccelerator(command_id: Integer);
    procedure RemoveAllAccelerators;

    property Title                    : ustring            read GetTitle                     write SetTitle;
    property WindowIcon               : ICefImage          read GetWindowIcon                write SetWindowIcon;
    property WindowAppIcon            : ICefImage          read GetWindowAppIcon             write SetWindowAppIcon;
    property Display                  : ICefDisplay        read GetDisplay;
    property ClientAreaBoundsInScreen : TCefRect           read GetClientAreaBoundsInScreen;
    property WindowHandle             : TCefWindowHandle   read GetWindowHandle;
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
    procedure OnWindowCreated(const window_: ICefWindow);
    procedure OnWindowClosing(const window_: ICefWindow);
    procedure OnWindowDestroyed(const window_: ICefWindow);
    procedure OnWindowActivationChanged(const window_: ICefWindow; active: boolean);
    procedure OnWindowBoundsChanged(const window_: ICefWindow; const new_bounds: TCefRect);
    procedure OnGetParentWindow(const window_: ICefWindow; var is_menu, can_activate_menu: boolean; var aResult : ICefWindow);
    procedure OnIsWindowModalDialog(const window_: ICefWindow; var aResult: boolean);
    procedure OnGetInitialBounds(const window_: ICefWindow; var aResult : TCefRect);
    procedure OnGetInitialShowState(const window_: ICefWindow; var aResult : TCefShowState);
    procedure OnIsFrameless(const window_: ICefWindow; var aResult : boolean);
    procedure OnWithStandardWindowButtons(const window_: ICefWindow; var aResult : boolean);
    procedure OnGetTitlebarHeight(const window_: ICefWindow; var titlebar_height: Single; var aResult : boolean);
    procedure OnCanResize(const window_: ICefWindow; var aResult : boolean);
    procedure OnCanMaximize(const window_: ICefWindow; var aResult : boolean);
    procedure OnCanMinimize(const window_: ICefWindow; var aResult : boolean);
    procedure OnCanClose(const window_: ICefWindow; var aResult : boolean);
    procedure OnAccelerator(const window_: ICefWindow; command_id: Integer; var aResult : boolean);
    procedure OnKeyEvent(const window_: ICefWindow; const event: TCefKeyEvent; var aResult : boolean);
    procedure OnWindowFullscreenTransition(const window_: ICefWindow; is_completed: boolean);
  end;

  /// <summary>
  /// Custom interface used to handle the ICefWindowDelegate events in a component.
  /// </summary>
  ICefWindowDelegateEvents = interface(ICefPanelDelegateEvents)
    ['{05C19A41-E75D-459E-AD4D-C8A0CA4A49D3}']
    procedure doOnWindowCreated(const window_: ICefWindow);
    procedure doOnWindowClosing(const window_: ICefWindow);
    procedure doOnWindowDestroyed(const window_: ICefWindow);
    procedure doOnWindowActivationChanged(const window_: ICefWindow; active: boolean);
    procedure doOnWindowBoundsChanged(const window_: ICefWindow; const new_bounds: TCefRect);
    procedure doOnGetParentWindow(const window_: ICefWindow; var is_menu, can_activate_menu: boolean; var aResult : ICefWindow);
    procedure doOnIsWindowModalDialog(const window_: ICefWindow; var aResult : boolean);
    procedure doOnGetInitialBounds(const window_: ICefWindow; var aResult : TCefRect);
    procedure doOnGetInitialShowState(const window_: ICefWindow; var aResult : TCefShowState);
    procedure doOnIsFrameless(const window_: ICefWindow; var aResult : boolean);
    procedure doOnWithStandardWindowButtons(const window_: ICefWindow; var aResult : boolean);
    procedure doOnGetTitlebarHeight(const window_: ICefWindow; var titlebar_height: Single; var aResult : boolean);
    procedure doOnCanResize(const window_: ICefWindow; var aResult : boolean);
    procedure doOnCanMaximize(const window_: ICefWindow; var aResult : boolean);
    procedure doOnCanMinimize(const window_: ICefWindow; var aResult : boolean);
    procedure doOnCanClose(const window_: ICefWindow; var aResult : boolean);
    procedure doOnAccelerator(const window_: ICefWindow; command_id: Integer; var aResult : boolean);
    procedure doOnKeyEvent(const window_: ICefWindow; const event: TCefKeyEvent; var aResult : boolean);
    procedure doOnWindowFullscreenTransition(const window_: ICefWindow; is_completed: boolean);
  end;

implementation

end.
