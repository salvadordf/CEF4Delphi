// ************************************************************************
// ***************************** CEF4Delphi *******************************
// ************************************************************************
//
// CEF4Delphi is based on DCEF3 which uses CEF3 to embed a chromium-based
// browser in Delphi applications.
//
// The original license of DCEF3 still applies to CEF4Delphi.
//
// For more information about CEF4Delphi visit :
//         https://www.briskbard.com/index.php?lang=en&pageid=cef
//
//        Copyright � 2018 Salvador D�az Fau. All rights reserved.
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

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  {$IFDEF MSWINDOWS}WinApi.Windows,{$ENDIF} System.Classes,
  {$ELSE}
  Windows, Classes,
  {$ENDIF}
  uCEFTypes, uCEFSchemeRegistrar;

type
  ICefBrowser = interface;
  ICefFrame = interface;
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
  ICefResourceBundleHandler = interface;
  ICefBrowserProcessHandler = interface;
  ICefRenderProcessHandler = interface;
  ICefProcessMessage = interface;
  ICefLifeSpanHandler = interface;
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
  ICefDownloadItem = interface;
  ICefBeforeDownloadCallback = interface;
  ICefJsDialogCallback = interface;
  ICefDownloadItemCallback = interface;
  ICefRequestCallback = interface;
  ICefResourceHandler = interface;
  ICefResponse = interface;
  ICefResponseFilter = interface;
  ICefAuthCallback = interface;
  ICefCallback = interface;
  ICefDragHandler = interface;
  ICefFindHandler = interface;

  TCefv8ValueArray         = array of ICefv8Value;
  TCefX509CertificateArray = array of ICefX509Certificate;
  TCefBinaryValueArray     = array of ICefBinaryValue;
  TCefFrameIdentifierArray = array of int64;



  // *******************************************
  // ***************** Events ******************
  // *******************************************


  TOnRegisterCustomSchemes           = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const registrar: TCefSchemeRegistrarRef) {$IFNDEF DELPHI12_UP}of object{$ENDIF};
  TOnRenderThreadCreatedEvent        = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const extraInfo: ICefListValue) {$IFNDEF DELPHI12_UP}of object{$ENDIF};
  TOnWebKitInitializedEvent          = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure() {$IFNDEF DELPHI12_UP}of object{$ENDIF};
  TOnBrowserCreatedEvent             = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const browser: ICefBrowser) {$IFNDEF DELPHI12_UP}of object{$ENDIF};
  TOnBrowserDestroyedEvent           = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const browser: ICefBrowser) {$IFNDEF DELPHI12_UP}of object{$ENDIF};
  TOnContextCreatedEvent             = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context) {$IFNDEF DELPHI12_UP}of object{$ENDIF};
  TOnContextReleasedEvent            = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context) {$IFNDEF DELPHI12_UP}of object{$ENDIF};
  TOnUncaughtExceptionEvent          = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context; const exception: ICefV8Exception; const stackTrace: ICefV8StackTrace) {$IFNDEF DELPHI12_UP}of object{$ENDIF};
  TOnFocusedNodeChangedEvent         = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const browser: ICefBrowser; const frame: ICefFrame; const node: ICefDomNode) {$IFNDEF DELPHI12_UP}of object{$ENDIF};
  TOnProcessMessageReceivedEvent     = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const browser: ICefBrowser; sourceProcess: TCefProcessId; const message: ICefProcessMessage; var aHandled : boolean) {$IFNDEF DELPHI12_UP}of object{$ENDIF};
  TOnContextInitializedEvent         = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure() {$IFNDEF DELPHI12_UP}of object{$ENDIF};
  TOnBeforeChildProcessLaunchEvent   = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const commandLine: ICefCommandLine) {$IFNDEF DELPHI12_UP}of object{$ENDIF};
  TOnRenderProcessThreadCreatedEvent = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const extraInfo: ICefListValue) {$IFNDEF DELPHI12_UP}of object{$ENDIF};
  TOnScheduleMessagePumpWorkEvent    = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const delayMs: Int64) {$IFNDEF DELPHI12_UP}of object{$ENDIF};
  TOnGetDataResourceEvent            = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(resourceId: Integer; out data: Pointer; out dataSize: NativeUInt; var aResult : Boolean) {$IFNDEF DELPHI12_UP}of object{$ENDIF};
  TOnGetLocalizedStringEvent         = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(stringId: Integer; out stringVal: ustring; var aResult : Boolean) {$IFNDEF DELPHI12_UP}of object{$ENDIF};
  TOnGetDataResourceForScaleEvent    = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(resourceId: Integer; scaleFactor: TCefScaleFactor; out data: Pointer; out dataSize: NativeUInt; var aResult : Boolean) {$IFNDEF DELPHI12_UP}of object{$ENDIF};
  TOnCDMRegistrationCompleteEvent    = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(result : TCefCDMRegistrationError; const error_message : ustring) {$IFNDEF DELPHI12_UP}of object{$ENDIF};


  // *******************************************
  // **** Callback procedures and functions ****
  // *******************************************


  TOnPdfPrintFinishedProc            = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const path: ustring; ok: Boolean);
  TCefDomVisitorProc                 = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const document: ICefDomDocument);
  TCefDomVisitorProc2                = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const browser : ICefBrowser; const document: ICefDomDocument);
  TCefStringVisitorProc              = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const str: ustring);
  TCefRunFileDialogCallbackProc      = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(selectedAcceptFilter: Integer; const filePaths: TStrings);
  TCefCompletionCallbackProc         = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure;
  TCefSetCookieCallbackProc          = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(success: Boolean);
  TCefDeleteCookiesCallbackProc      = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(numDeleted: Integer);
  TCefNavigationEntryVisitorProc     = {$IFDEF DELPHI12_UP}reference to{$ENDIF} function(const entry: ICefNavigationEntry; current: Boolean; index, total: Integer): Boolean;
  TOnDownloadImageFinishedProc       = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const imageUrl: ustring; httpStatusCode: Integer; const image: ICefImage);
  TCefCookieVisitorProc              = {$IFDEF DELPHI12_UP}reference to{$ENDIF} function(const name, value, domain, path: ustring; secure, httponly, hasExpires: Boolean; const creation, lastAccess, expires: TDateTime; count, total: Integer; out deleteCookie: Boolean): Boolean;



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

    property Handle                                                : TCefStringMultimap read GetHandle;
    property Size                                                  : NativeUInt         read GetSize;
    property Key[index: NativeUInt]                                : ustring            read GetKey;
    property Value[index: NativeUInt]                              : ustring            read GetValue;
    property Enumerate[const Key: ustring; ValueIndex: NativeUInt] : ustring            read GetEnumerate;
  end;

  IChromiumEvents = interface
    ['{0C139DB1-0349-4D7F-8155-76FEA6A0126D}']
    procedure GetSettings(var settings: TCefBrowserSettings);

    // ICefClient
    function  doOnProcessMessageReceived(const browser: ICefBrowser; sourceProcess: TCefProcessId; const message: ICefProcessMessage): Boolean;

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

    // ICefDownloadHandler
    procedure doOnBeforeDownload(const browser: ICefBrowser; const downloadItem: ICefDownloadItem; const suggestedName: ustring; const callback: ICefBeforeDownloadCallback);
    procedure doOnDownloadUpdated(const browser: ICefBrowser; const downloadItem: ICefDownloadItem; const callback: ICefDownloadItemCallback);

    // ICefJsDialogHandler
    function  doOnJsdialog(const browser: ICefBrowser; const originUrl: ustring; dialogType: TCefJsDialogType; const messageText, defaultPromptText: ustring; const callback: ICefJsDialogCallback; out suppressMessage: Boolean): Boolean;
    function  doOnBeforeUnloadDialog(const browser: ICefBrowser; const messageText: ustring; isReload: Boolean; const callback: ICefJsDialogCallback): Boolean;
    procedure doOnResetDialogState(const browser: ICefBrowser);
    procedure doOnDialogClosed(const browser: ICefBrowser);

    // ICefLifeSpanHandler
    function  doOnBeforePopup(const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var noJavascriptAccess: Boolean): Boolean;
    procedure doOnAfterCreated(const browser: ICefBrowser);
    procedure doOnBeforeClose(const browser: ICefBrowser);
    function  doOnClose(const browser: ICefBrowser): Boolean;

    // ICefRequestHandler
    function  doOnBeforeBrowse(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; user_gesture, isRedirect: Boolean): Boolean;
    function  doOnOpenUrlFromTab(const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean): Boolean;
    function  doOnBeforeResourceLoad(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const callback: ICefRequestCallback): TCefReturnValue;
    function  doOnGetResourceHandler(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest): ICefResourceHandler;
    procedure doOnResourceRedirect(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; var newUrl: ustring);
    function  doOnResourceResponse(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse): Boolean;
    function  doOnGetResourceResponseFilter(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse): ICefResponseFilter;
    procedure doOnResourceLoadComplete(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; status: TCefUrlRequestStatus; receivedContentLength: Int64);
    function  doOnGetAuthCredentials(const browser: ICefBrowser; const frame: ICefFrame; isProxy: Boolean; const host: ustring; port: Integer; const realm, scheme: ustring; const callback: ICefAuthCallback): Boolean;
    function  doCanGetCookies(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest): boolean;
    function  doCanSetCookie(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const cookie : PCefCookie): boolean;
    function  doOnQuotaRequest(const browser: ICefBrowser; const originUrl: ustring; newSize: Int64; const callback: ICefRequestCallback): Boolean;
    procedure doOnProtocolExecution(const browser: ICefBrowser; const url: ustring; out allowOsExecution: Boolean);
    function  doOnCertificateError(const browser: ICefBrowser; certError: TCefErrorcode; const requestUrl: ustring; const sslInfo: ICefSslInfo; const callback: ICefRequestCallback): Boolean;
    function  doOnSelectClientCertificate(const browser: ICefBrowser; isProxy: boolean; const host: ustring; port: integer; certificatesCount: NativeUInt; const certificates: TCefX509CertificateArray; const callback: ICefSelectClientCertificateCallback): boolean;
    procedure doOnPluginCrashed(const browser: ICefBrowser; const pluginPath: ustring);
    procedure doOnRenderViewReady(const browser: ICefBrowser);
    procedure doOnRenderProcessTerminated(const browser: ICefBrowser; status: TCefTerminationStatus);

    // ICefDialogHandler
    function  doOnFileDialog(const browser: ICefBrowser; mode: TCefFileDialogMode; const title, defaultFilePath: ustring; const acceptFilters: TStrings; selectedAcceptFilter: Integer; const callback: ICefFileDialogCallback): Boolean;

    // ICefRenderHandler
    procedure doOnGetAccessibilityHandler(var aAccessibilityHandler : ICefAccessibilityHandler);
    function  doOnGetRootScreenRect(const browser: ICefBrowser; var rect: TCefRect): Boolean;
    function  doOnGetViewRect(const browser: ICefBrowser; var rect: TCefRect): Boolean;
    function  doOnGetScreenPoint(const browser: ICefBrowser; viewX, viewY: Integer; var screenX, screenY: Integer): Boolean;
    function  doOnGetScreenInfo(const browser: ICefBrowser; var screenInfo: TCefScreenInfo): Boolean;
    procedure doOnPopupShow(const browser: ICefBrowser; show: Boolean);
    procedure doOnPopupSize(const browser: ICefBrowser; const rect: PCefRect);
    procedure doOnPaint(const browser: ICefBrowser; kind: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; const buffer: Pointer; width, height: Integer);
    procedure doOnCursorChange(const browser: ICefBrowser; cursor: TCefCursorHandle; cursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo);
    function  doOnStartDragging(const browser: ICefBrowser; const dragData: ICefDragData; allowedOps: TCefDragOperations; x, y: Integer): Boolean;
    procedure doOnUpdateDragCursor(const browser: ICefBrowser; operation: TCefDragOperation);
    procedure doOnScrollOffsetChanged(const browser: ICefBrowser; x, y: Double);
    procedure doOnIMECompositionRangeChanged(const browser: ICefBrowser; const selected_range: PCefRange; character_boundsCount: NativeUInt; const character_bounds: PCefRect);
    procedure doOnTextSelectionChanged(const browser: ICefBrowser; const selected_text: ustring; const selected_range: PCefRange);

    // ICefDragHandler
    function  doOnDragEnter(const browser: ICefBrowser; const dragData: ICefDragData; mask: TCefDragOperations): Boolean;
    procedure doOnDraggableRegionsChanged(const browser: ICefBrowser; regionsCount: NativeUInt; regions: PCefDraggableRegionArray);

    // ICefFindHandler
    procedure doOnFindResult(const browser: ICefBrowser; identifier, count: Integer; const selectionRect: PCefRect; activeMatchOrdinal: Integer; finalUpdate: Boolean);

    // Custom
    procedure doCookiesDeleted(numDeleted : integer);
    procedure doPdfPrintFinished(aResultOK : boolean);
    procedure doTextResultAvailable(const aText : string);
    procedure doUpdatePreferences(const aBrowser: ICefBrowser);
    procedure doUpdateOwnPreferences;
    function  doSavePreferences : boolean;
    procedure doResolvedHostAvailable(result: TCefErrorCode; const resolvedIps: TStrings);
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



  // *******************************************
  // ************** CEF interfaces *************
  // *******************************************


  // TCefBaseRefCounted
  // /include/capi/cef_base_capi.h (cef_base_ref_counted_t)
  ICefBaseRefCounted = interface
    ['{1F9A7B44-DCDC-4477-9180-3ADD44BDEB7B}']
    function Wrap: Pointer;
    function SameAs(aData : Pointer) : boolean;
  end;

  // TCefRunFileDialogCallback
  // /include/capi/cef_browser_capi.h (cef_run_file_dialog_callback_t)
  ICefRunFileDialogCallback = interface(ICefBaseRefCounted)
    ['{59FCECC6-E897-45BA-873B-F09586C4BE47}']
    procedure OnFileDialogDismissed(selectedAcceptFilter: Integer; const filePaths: TStrings);
  end;

  // TCefNavigationEntryVisitor
  // /include/capi/cef_browser_capi.h (cef_navigation_entry_visitor_t)
  ICefNavigationEntryVisitor = interface(ICefBaseRefCounted)
    ['{CC4D6BC9-0168-4C2C-98BA-45E9AA9CD619}']
    function Visit(const entry: ICefNavigationEntry; current: Boolean; index, total: Integer): Boolean;
  end;

  // TCefPdfPrintCallback
  // /include/capi/cef_browser_capi.h (cef_pdf_print_callback_t)
  ICefPdfPrintCallback = interface(ICefBaseRefCounted)
    ['{F1CC58E9-2C30-4932-91AE-467C8D8EFB8E}']
    procedure OnPdfPrintFinished(const path: ustring; ok: Boolean);
  end;

  // TCefDownloadImageCallback
  // /include/capi/cef_browser_capi.h (cef_download_image_callback_t)
  ICefDownloadImageCallback = interface(ICefBaseRefCounted)
    ['{0C6E9032-27DF-4584-95C6-DC3C7CB63727}']
    procedure OnDownloadImageFinished(const imageUrl: ustring; httpStatusCode: Integer; const image: ICefImage);
  end;

  // TCefBrowserHost
  // /include/capi/cef_browser_capi.h (cef_browser_host_t)
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
    procedure RunFileDialog(mode: TCefFileDialogMode; const title, defaultFilePath: ustring; const acceptFilters: TStrings; selectedAcceptFilter: Integer; const callback: ICefRunFileDialogCallback);
    procedure RunFileDialogProc(mode: TCefFileDialogMode; const title, defaultFilePath: ustring; const acceptFilters: TStrings; selectedAcceptFilter: Integer; const callback: TCefRunFileDialogCallbackProc);
    procedure StartDownload(const url: ustring);
    procedure DownloadImage(const imageUrl: ustring; isFavicon: Boolean; maxImageSize: cardinal; bypassCache: Boolean; const callback: ICefDownloadImageCallback);
    procedure Print;
    procedure PrintToPdf(const path: ustring; settings: PCefPdfPrintSettings; const callback: ICefPdfPrintCallback);
    procedure PrintToPdfProc(const path: ustring; settings: PCefPdfPrintSettings; const callback: TOnPdfPrintFinishedProc);
    procedure Find(identifier: Integer; const searchText: ustring; forward, matchCase, findNext: Boolean);
    procedure StopFinding(clearSelection: Boolean);
    procedure ShowDevTools(const windowInfo: PCefWindowInfo; const client: ICefClient; const settings: PCefBrowserSettings; inspectElementAt: PCefPoint);
    procedure CloseDevTools;
    function  HasDevTools: Boolean;
    procedure GetNavigationEntries(const visitor: ICefNavigationEntryVisitor; currentOnly: Boolean);
    procedure GetNavigationEntriesProc(const proc: TCefNavigationEntryVisitorProc; currentOnly: Boolean);
    procedure SetMouseCursorChangeDisabled(disabled: Boolean);
    function  IsMouseCursorChangeDisabled: Boolean;
    procedure ReplaceMisspelling(const word: ustring);
    procedure AddWordToDictionary(const word: ustring);
    function  IsWindowRenderingDisabled: Boolean;
    procedure WasResized;
    procedure WasHidden(hidden: Boolean);
    procedure NotifyScreenInfoChanged;
    procedure Invalidate(kind: TCefPaintElementType);
    procedure SendKeyEvent(const event: PCefKeyEvent);
    procedure SendMouseClickEvent(const event: PCefMouseEvent; kind: TCefMouseButtonType; mouseUp: Boolean; clickCount: Integer);
    procedure SendMouseMoveEvent(const event: PCefMouseEvent; mouseLeave: Boolean);
    procedure SendMouseWheelEvent(const event: PCefMouseEvent; deltaX, deltaY: Integer);
    procedure SendFocusEvent(setFocus: Boolean);
    procedure SendCaptureLostEvent;
    procedure NotifyMoveOrResizeStarted;
    function  GetWindowlessFrameRate : Integer;
    procedure SetWindowlessFrameRate(frameRate: Integer);
    procedure IMESetComposition(const text: ustring; underlinesCount : NativeUInt; const underlines : PCefCompositionUnderline; const replacement_range, selection_range : PCefRange);
    procedure IMECommitText(const text: ustring; const replacement_range : PCefRange; relative_cursor_pos : integer);
    procedure IMEFinishComposingText(keep_selection : boolean);
    procedure IMECancelComposition;
    procedure DragTargetDragEnter(const dragData: ICefDragData; const event: PCefMouseEvent; allowedOps: TCefDragOperations);
    procedure DragTargetDragOver(const event: PCefMouseEvent; allowedOps: TCefDragOperations);
    procedure DragTargetDragLeave;
    procedure DragTargetDrop(event: PCefMouseEvent);
    procedure DragSourceEndedAt(x, y: Integer; op: TCefDragOperation);
    procedure DragSourceSystemDragEnded;
    function  GetVisibleNavigationEntry : ICefNavigationEntry;
    procedure SetAccessibilityState(accessibilityState: TCefState);
    procedure SetAutoResizeEnabled(enabled: boolean; const min_size, max_size: PCefSize);
    function  GetExtension : ICefExtension;
    function  IsBackgroundHost : boolean;

    property Browser                : ICefBrowser              read GetBrowser;
    property WindowHandle           : TCefWindowHandle         read GetWindowHandle;
    property OpenerWindowHandle     : TCefWindowHandle         read GetOpenerWindowHandle;
    property ZoomLevel              : Double                   read GetZoomLevel               write SetZoomLevel;
    property RequestContext         : ICefRequestContext       read GetRequestContext;
    property VisibleNavigationEntry : ICefNavigationEntry      read GetVisibleNavigationEntry;
  end;

  // TCefProcessMessage
  // /include/capi/cef_process_message_capi.h (cef_process_message_t)
  ICefProcessMessage = interface(ICefBaseRefCounted)
    ['{E0B1001A-8777-425A-869B-29D40B8B93B1}']
    function IsValid: Boolean;
    function IsReadOnly: Boolean;
    function Copy: ICefProcessMessage;
    function GetName: ustring;
    function GetArgumentList: ICefListValue;
    property Name: ustring read GetName;
    property ArgumentList: ICefListValue read GetArgumentList;
  end;

  // TCefBrowser
  // /include/capi/cef_browser_capi.h (cef_browser_t)
  ICefBrowser = interface(ICefBaseRefCounted)
    ['{BA003C2E-CF15-458F-9D4A-FE3CEFCF3EEF}']
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
    function  SendProcessMessage(targetProcess: TCefProcessId; const ProcMessage: ICefProcessMessage): Boolean;

    property MainFrame    : ICefFrame       read GetMainFrame;
    property FocusedFrame : ICefFrame       read GetFocusedFrame;
    property FrameCount   : NativeUInt      read GetFrameCount;
    property Host         : ICefBrowserHost read GetHost;
    property Identifier   : Integer         read GetIdentifier;
  end;

  // TCefPostDataElement
  // /include/capi/cef_request_capi.h (cef_post_data_element_t)
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

  // TCefPostData
  // /include/capi/cef_request_capi.h (cef_post_data_t)
  ICefPostData = interface(ICefBaseRefCounted)
    ['{1E677630-9339-4732-BB99-D6FE4DE4AEC0}']
    function  IsReadOnly: Boolean;
    function  HasExcludedElements: Boolean;
    function  GetCount: NativeUInt;
    function  GetElements(Count: NativeUInt): IInterfaceList; // list of ICefPostDataElement
    function  RemoveElement(const element: ICefPostDataElement): Integer;
    function  AddElement(const element: ICefPostDataElement): Integer;
    procedure RemoveElements;
  end;

  // TCefRequest
  // /include/capi/cef_request_capi.h (cef_request_t)
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

  // TCefStringVisitor
  // /include/capi/cef_string_visitor_capi.h (cef_string_visitor_t)
  ICefStringVisitor = interface(ICefBaseRefCounted)
    ['{63ED4D6C-2FC8-4537-964B-B84C008F6158}']
    procedure Visit(const str: ustring);
  end;

  // TCefFrame
  // /include/capi/cef_frame_capi.h (cef_frame_t)
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
    procedure LoadString(const str, url: ustring);
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

    property Name       : ustring     read GetName;
    property Url        : ustring     read GetUrl;
    property Browser    : ICefBrowser read GetBrowser;
    property Parent     : ICefFrame   read GetParent;
    property Identifier : int64       read GetIdentifier;
  end;

  // TCefStreamReader
  // /include/capi/cef_stream_capi.h (cef_stream_reader_t)
  ICefCustomStreamReader = interface(ICefBaseRefCounted)
    ['{BBCFF23A-6FE7-4C28-B13E-6D2ACA5C83B7}']
    function Read(ptr: Pointer; size, n: NativeUInt): NativeUInt;
    function Seek(offset: Int64; whence: Integer): Integer;
    function Tell: Int64;
    function Eof: Boolean;
    function MayBlock: Boolean;
  end;

  // TCefStreamReader
  // /include/capi/cef_stream_capi.h (cef_stream_reader_t)
  ICefStreamReader = interface(ICefBaseRefCounted)
    ['{DD5361CB-E558-49C5-A4BD-D1CE84ADB277}']
    function Read(ptr: Pointer; size, n: NativeUInt): NativeUInt;
    function Seek(offset: Int64; whence: Integer): Integer;
    function Tell: Int64;
    function Eof: Boolean;
    function MayBlock: Boolean;
  end;

  // TCefWriteHandler
  // /include/capi/cef_stream_capi.h (cef_write_handler_t)
  ICefWriteHandler = interface(ICefBaseRefCounted)
    ['{F2431888-4EAB-421E-9EC3-320BE695AF30}']
    function Write(const ptr: Pointer; size, n: NativeUInt): NativeUInt;
    function Seek(offset: Int64; whence: Integer): Integer;
    function Tell: Int64;
    function Flush: Integer;
    function MayBlock: Boolean;
  end;

  // TCefStreamWriter
  // /include/capi/cef_stream_capi.h (cef_stream_writer_t)
  ICefStreamWriter = interface(ICefBaseRefCounted)
    ['{4AA6C477-7D8A-4D5A-A704-67F900A827E7}']
    function Write(const ptr: Pointer; size, n: NativeUInt): NativeUInt;
    function Seek(offset: Int64; whence: Integer): Integer;
    function Tell: Int64;
    function Flush: Integer;
    function MayBlock: Boolean;
  end;

  // TCefResponse
  // /include/capi/cef_response_capi.h (cef_response_t)
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
    function  GetHeader(const name: ustring): ustring;
    procedure GetHeaderMap(const headerMap: ICefStringMultimap);
    procedure SetHeaderMap(const headerMap: ICefStringMultimap);
    function  GetURL: ustring;
    procedure SetURL(const url: ustring);

    property Status     : Integer       read GetStatus      write SetStatus;
    property StatusText : ustring       read GetStatusText  write SetStatusText;
    property MimeType   : ustring       read GetMimeType    write SetMimeType;
    property Error      : TCefErrorCode read GetError       write SetError;
    property URL        : ustring       read GetURL         write SetURL;
  end;

  // TCefDownloadItem
  // /include/capi/cef_download_item_capi.h (cef_download_item_t)
  ICefDownloadItem = interface(ICefBaseRefCounted)
    ['{B34BD320-A82E-4185-8E84-B98E5EEC803F}']
    function IsValid: Boolean;
    function IsInProgress: Boolean;
    function IsComplete: Boolean;
    function IsCanceled: Boolean;
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

    property CurrentSpeed       : Int64     read GetCurrentSpeed;
    property PercentComplete    : Integer   read GetPercentComplete;
    property TotalBytes         : Int64     read GetTotalBytes;
    property ReceivedBytes      : Int64     read GetReceivedBytes;
    property StartTime          : TDateTime read GetStartTime;
    property EndTime            : TDateTime read GetEndTime;
    property FullPath           : ustring   read GetFullPath;
    property Id                 : Cardinal  read GetId;
    property Url                : ustring   read GetUrl;
    property OriginalUrl        : ustring   read GetOriginalUrl;
    property SuggestedFileName  : ustring   read GetSuggestedFileName;
    property ContentDisposition : ustring   read GetContentDisposition;
    property MimeType           : ustring   read GetMimeType;
  end;

  // TCefBeforeDownloadCallback
  // /include/capi/cef_download_handler_capi.h (cef_before_download_callback_t)
  ICefBeforeDownloadCallback = interface(ICefBaseRefCounted)
    ['{5A81AF75-CBA2-444D-AD8E-522160F36433}']
    procedure Cont(const downloadPath: ustring; showDialog: Boolean);
  end;

  // TCefDownloadItemCallback
  // /include/capi/cef_download_handler_capi.h (cef_download_item_callback_t)
  ICefDownloadItemCallback = interface(ICefBaseRefCounted)
    ['{498F103F-BE64-4D5F-86B7-B37EC69E1735}']
    procedure Cancel;
    procedure Pause;
    procedure Resume;
  end;

  // TCefDownloadHandler
  // /include/capi/cef_download_handler_capi.h (cef_download_handler_t)
  ICefDownloadHandler = interface(ICefBaseRefCounted)
    ['{3137F90A-5DC5-43C1-858D-A269F28EF4F1}']
    procedure OnBeforeDownload(const browser: ICefBrowser; const downloadItem: ICefDownloadItem; const suggestedName: ustring; const callback: ICefBeforeDownloadCallback);
    procedure OnDownloadUpdated(const browser: ICefBrowser; const downloadItem: ICefDownloadItem; const callback: ICefDownloadItemCallback);

    procedure RemoveReferences; // custom procedure to clear all references
  end;

  // TCefV8Exception
  // /include/capi/cef_v8_capi.h (cef_v8exception_t)
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

  // TCefv8ArrayBufferReleaseCallback
  // /include/capi/cef_v8_capi.h (cef_v8array_buffer_release_callback_t)
  ICefv8ArrayBufferReleaseCallback = interface(ICefBaseRefCounted)
    ['{4EAAB422-D046-43DF-B1F0-5503116A5816}']
    procedure ReleaseBuffer(buffer : Pointer);
  end;

  // TCefV8Context
  // /include/capi/cef_v8_capi.h (cef_v8context_t)
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

  // TCefv8Handler
  // /include/capi/cef_v8_capi.h (cef_v8handler_t)
  ICefv8Handler = interface(ICefBaseRefCounted)
    ['{F94CDC60-FDCB-422D-96D5-D2A775BD5D73}']
    function Execute(const name: ustring; const obj: ICefv8Value; const arguments: TCefv8ValueArray; var retval: ICefv8Value; var exception: ustring): Boolean;
  end;

  // TCefV8Interceptor
  // /include/capi/cef_v8_capi.h (cef_v8interceptor_t)
  ICefV8Interceptor = interface(ICefBaseRefCounted)
    ['{B3B8FD7C-A916-4B25-93A2-2892AC324F21}']
    function GetByName(const name: ustring; const obj: ICefv8Value; out retval: ICefv8Value; const exception: ustring): boolean;
    function GetByIndex(index: integer; const obj: ICefv8Value; out retval: ICefv8Value; const exception: ustring): boolean;
    function SetByName(const name: ustring; const obj, value: ICefv8Value; const exception: ustring): boolean;
    function SetByIndex(index: integer; const obj, value: ICefv8Value; const exception: ustring): boolean;
  end;

  // TCefV8Accessor
  // /include/capi/cef_v8_capi.h (cef_v8accessor_t)
  ICefV8Accessor = interface(ICefBaseRefCounted)
    ['{DCA6D4A2-726A-4E24-AA64-5E8C731D868A}']
    function Get(const name: ustring; const obj: ICefv8Value; out retval: ICefv8Value; var exception: ustring): Boolean;
    function Put(const name: ustring; const obj, value: ICefv8Value; var exception: ustring): Boolean;
  end;

  // TCefTask
  // /include/capi/cef_task_capi.h (cef_task_t)
  ICefTask = interface(ICefBaseRefCounted)
    ['{0D965470-4A86-47CE-BD39-A8770021AD7E}']
    procedure Execute;
  end;

  // TCefTaskRunner
  // /include/capi/cef_task_capi.h (cef_task_runner_t)
  ICefTaskRunner = interface(ICefBaseRefCounted)
    ['{6A500FA3-77B7-4418-8EA8-6337EED1337B}']
    function IsSame(const that: ICefTaskRunner): Boolean;
    function BelongsToCurrentThread: Boolean;
    function BelongsToThread(threadId: TCefThreadId): Boolean;
    function PostTask(const task: ICefTask): Boolean;
    function PostDelayedTask(const task: ICefTask; delayMs: Int64): Boolean;
  end;

  // TCefThread
  // /include/capi/cef_thread_capi.h (cef_thread_t)
  ICefThread = interface(ICefBaseRefCounted)
    ['{26B30EA5-F44A-4C40-97DF-67FD9E73A4FF}']
    function  GetTaskRunner : ICefTaskRunner;
    function  GetPlatformThreadID : TCefPlatformThreadId;
    procedure Stop;
    function  IsRunning : boolean;
  end;

  // TCefWaitableEvent
  // /include/capi/cef_waitable_event_capi.h (cef_waitable_event_t)
  ICefWaitableEvent = interface(ICefBaseRefCounted)
    ['{965C90C9-3DAE-457F-AA64-E04FF508094A}']
    procedure Reset;
    procedure Signal;
    function  IsSignaled : boolean;
    procedure Wait;
    function  TimedWait(max_ms: int64): boolean;
  end;

  // TCefv8Value
  // /include/capi/cef_v8_capi.h (cef_v8value_t)
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
  end;

  // TCefV8StackFrame
  // /include/capi/cef_v8_capi.h (cef_v8stack_frame_t)
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

  // TCefV8StackTrace
  // /include/capi/cef_v8_capi.h (cef_v8stack_trace_t)
  ICefV8StackTrace = interface(ICefBaseRefCounted)
    ['{32111C84-B7F7-4E3A-92B9-7CA1D0ADB613}']
    function IsValid: Boolean;
    function GetFrameCount: Integer;
    function GetFrame(index: Integer): ICefV8StackFrame;

    property FrameCount            : Integer          read GetFrameCount;
    property Frame[index: Integer] : ICefV8StackFrame read GetFrame;
  end;

  // TCefXmlReader
  // /include/capi/cef_xml_reader_capi.h (cef_xml_reader_t)
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

  // TCefZipReader
  // /include/capi/cef_zip_reader_capi.h (cef_zip_reader_t)
  ICefZipReader = interface(ICefBaseRefCounted)
    ['{3B6C591F-9877-42B3-8892-AA7B27DA34A8}']
    function MoveToFirstFile: Boolean;
    function MoveToNextFile: Boolean;
    function MoveToFile(const fileName: ustring; caseSensitive: Boolean): Boolean;
    function Close: Boolean;
    function GetFileName: ustring;
    function GetFileSize: Int64;
    function GetFileLastModified: TCefTime;
    function OpenFile(const password: ustring): Boolean;
    function CloseFile: Boolean;
    function ReadFile(buffer: Pointer; bufferSize: NativeUInt): Integer;
    function Tell: Int64;
    function Eof: Boolean;
  end;

  // TCefDomNode
  // /include/capi/cef_dom_capi.h (cef_domnode_t)
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
    procedure GetElementAttributes(const attrMap: ICefStringMap);
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

  // TCefDomDocument
  // /include/capi/cef_dom_capi.h (cef_domdocument_t)
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

  // TCefDomVisitor
  // /include/capi/cef_dom_capi.h (cef_domvisitor_t)
  ICefDomVisitor = interface(ICefBaseRefCounted)
    ['{30398428-3196-4531-B968-2DDBED36F6B0}']
    procedure visit(const document: ICefDomDocument);
  end;

  // TCefCookieVisitor
  // /include/capi/cef_cookie_capi.h (cef_cookie_visitor_t)
  ICefCookieVisitor = interface(ICefBaseRefCounted)
    ['{8378CF1B-84AB-4FDB-9B86-34DDABCCC402}']
    function visit(const name, value, domain, path: ustring; secure, httponly, hasExpires: Boolean; const creation, lastAccess, expires: TDateTime; count, total: Integer; out deleteCookie: Boolean): Boolean;
  end;

  // TCefCommandLine
  // /include/capi/cef_command_line_capi.h (cef_command_line_t)
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
    procedure GetSwitches(var switches: TStrings);
    procedure AppendSwitch(const name: ustring);
    procedure AppendSwitchWithValue(const name, value: ustring);
    function  HasArguments: Boolean;
    procedure GetArguments(var arguments: TStrings);
    procedure AppendArgument(const argument: ustring);
    procedure PrependWrapper(const wrapper: ustring);

    property  CommandLineString  : ustring   read GetCommandLineString;
  end;

  // TCefResourceBundleHandler
  // /include/capi/cef_resource_bundle_handler_capi.h (cef_resource_bundle_handler_t)
  ICefResourceBundleHandler = interface(ICefBaseRefCounted)
    ['{09C264FD-7E03-41E3-87B3-4234E82B5EA2}']
    function GetLocalizedString(stringId: Integer; var stringVal: ustring): Boolean;
    function GetDataResource(resourceId: Integer; var data: Pointer; var dataSize: NativeUInt): Boolean;
    function GetDataResourceForScale(resourceId: Integer; scaleFactor: TCefScaleFactor; var data: Pointer; var dataSize: NativeUInt): Boolean;
  end;

  // TCefBrowserProcessHandler
  // /include/capi/cef_browser_process_handler_capi.h (cef_browser_process_handler_t)
  ICefBrowserProcessHandler = interface(ICefBaseRefCounted)
    ['{27291B7A-C0AE-4EE0-9115-15C810E22F6C}']
    procedure OnContextInitialized;
    procedure OnBeforeChildProcessLaunch(const commandLine: ICefCommandLine);
    procedure OnRenderProcessThreadCreated(const extraInfo: ICefListValue);
    procedure GetPrintHandler(var aHandler : ICefPrintHandler);
    procedure OnScheduleMessagePumpWork(const delayMs: Int64);
  end;

  // TCefRenderProcessHandler
  // /include/capi/cef_render_process_handler_capi.h (cef_render_process_handler_t)
  ICefRenderProcessHandler = interface(ICefBaseRefCounted)
    ['{FADEE3BC-BF66-430A-BA5D-1EE3782ECC58}']
    procedure OnRenderThreadCreated(const extraInfo: ICefListValue);
    procedure OnWebKitInitialized;
    procedure OnBrowserCreated(const browser: ICefBrowser);
    procedure OnBrowserDestroyed(const browser: ICefBrowser);
    function  GetLoadHandler : ICefLoadHandler;
    procedure OnContextCreated(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context);
    procedure OnContextReleased(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context);
    procedure OnUncaughtException(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context; const V8Exception: ICefV8Exception; const stackTrace: ICefV8StackTrace);
    procedure OnFocusedNodeChanged(const browser: ICefBrowser; const frame: ICefFrame; const node: ICefDomNode);
    function  OnProcessMessageReceived(const browser: ICefBrowser; sourceProcess: TCefProcessId; const aMessage: ICefProcessMessage): Boolean;
  end;

  // TCefApp
  // /include/capi/cef_app_capi.h (cef_app_t)
  ICefApp = interface(ICefBaseRefCounted)
    ['{970CA670-9070-4642-B188-7D8A22DAEED4}']
    procedure OnBeforeCommandLineProcessing(const processType: ustring; const commandLine: ICefCommandLine);
    procedure OnRegisterCustomSchemes(const registrar: TCefSchemeRegistrarRef);
    procedure GetResourceBundleHandler(var aHandler : ICefResourceBundleHandler);
    procedure GetBrowserProcessHandler(var aHandler : ICefBrowserProcessHandler);
    procedure GetRenderProcessHandler(var aHandler : ICefRenderProcessHandler);
  end;

  // TCefCompletionCallback
  // /include/capi/cef_callback_capi.h (cef_completion_callback_t)
  ICefCompletionCallback = interface(ICefBaseRefCounted)
    ['{A8ECCFBB-FEE0-446F-AB32-AD69A7478D57}']
    procedure OnComplete;
  end;

  // TCefSetCookieCallback
  // /include/capi/cef_cookie_capi.h (cef_set_cookie_callback_t)
  ICefSetCookieCallback = interface(ICefBaseRefCounted)
    ['{16E14B6F-CB0A-4F9D-A008-239E0BC7B892}']
    procedure OnComplete(success: Boolean);
  end;

  // TCefDeleteCookiesCallback
  // /include/capi/cef_cookie_capi.h (cef_delete_cookies_callback_t)
  ICefDeleteCookiesCallback = interface(ICefBaseRefCounted)
    ['{758B79A1-B9E8-4F0D-94A0-DCE5AFADE33D}']
    procedure OnComplete(numDeleted: Integer);
  end;

  // TCefCookieManager
  // /include/capi/cef_cookie_capi.h (cef_cookie_manager_t)
  ICefCookieManager = Interface(ICefBaseRefCounted)
    ['{CC1749E6-9AD3-4283-8430-AF6CBF3E8785}']
    procedure SetSupportedSchemes(const schemes: TStrings; const callback: ICefCompletionCallback);
    procedure SetSupportedSchemesProc(const schemes: TStrings; const callback: TCefCompletionCallbackProc);
    function  VisitAllCookies(const visitor: ICefCookieVisitor): Boolean;
    function  VisitAllCookiesProc(const visitor: TCefCookieVisitorProc): Boolean;
    function  VisitUrlCookies(const url: ustring; includeHttpOnly: Boolean; const visitor: ICefCookieVisitor): Boolean;
    function  VisitUrlCookiesProc(const url: ustring; includeHttpOnly: Boolean; const visitor: TCefCookieVisitorProc): Boolean;
    function  SetCookie(const url: ustring; const name, value, domain, path: ustring; secure, httponly, hasExpires: Boolean; const creation, lastAccess, expires: TDateTime; const callback: ICefSetCookieCallback): Boolean;
    function  SetCookieProc(const url: ustring; const name, value, domain, path: ustring; secure, httponly, hasExpires: Boolean; const creation, lastAccess, expires: TDateTime; const callback: TCefSetCookieCallbackProc): Boolean;
    function  DeleteCookies(const url, cookieName: ustring; const callback: ICefDeleteCookiesCallback): Boolean;
    function  DeleteCookiesProc(const url, cookieName: ustring; const callback: TCefDeleteCookiesCallbackProc): Boolean;
    function  SetStoragePath(const path: ustring; persistSessionCookies: Boolean; const callback: ICefCompletionCallback): Boolean;
    function  SetStoragePathProc(const path: ustring; persistSessionCookies: Boolean; const callback: TCefCompletionCallbackProc): Boolean;
    function  FlushStore(const handler: ICefCompletionCallback): Boolean;
    function  FlushStoreProc(const proc: TCefCompletionCallbackProc): Boolean;
  end;

  // TCefWebPluginInfo
  // /include/capi/cef_web_plugin_capi.h (cef_web_plugin_info_t)
  ICefWebPluginInfo = interface(ICefBaseRefCounted)
    ['{AA879E58-F649-44B1-AF9C-655FF5B79A02}']
    function GetName: ustring;
    function GetPath: ustring;
    function GetVersion: ustring;
    function GetDescription: ustring;

    property Name         : ustring read GetName;
    property Path         : ustring read GetPath;
    property Version      : ustring read GetVersion;
    property Description  : ustring read GetDescription;
  end;

  // TCefCallback
  // /include/capi/cef_callback_capi.h (cef_callback_t)
  ICefCallback = interface(ICefBaseRefCounted)
    ['{1B8C449F-E2D6-4B78-9BBA-6F47E8BCDF37}']
    procedure Cont;
    procedure Cancel;
  end;

  // TCefResourceHandler
  // /include/capi/cef_resource_handler_capi.h (cef_resource_handler_t)
  ICefResourceHandler = interface(ICefBaseRefCounted)
  ['{BD3EA208-AAAD-488C-BFF2-76993022F2B5}']
    function  ProcessRequest(const request: ICefRequest; const callback: ICefCallback): Boolean;
    procedure GetResponseHeaders(const response: ICefResponse; out responseLength: Int64; out redirectUrl: ustring);
    function  ReadResponse(const dataOut: Pointer; bytesToRead: Integer; var bytesRead: Integer; const callback: ICefCallback): Boolean;
    function  CanGetCookie(const cookie: PCefCookie): Boolean;
    function  CanSetCookie(const cookie: PCefCookie): Boolean;
    procedure Cancel;
  end;

  // TCefSchemeHandlerFactory
  // /include/capi/cef_scheme_capi.h (cef_scheme_handler_factory_t)
  ICefSchemeHandlerFactory = interface(ICefBaseRefCounted)
    ['{4D9B7960-B73B-4EBD-9ABE-6C1C43C245EB}']
    function New(const browser: ICefBrowser; const frame: ICefFrame; const schemeName: ustring; const request: ICefRequest): ICefResourceHandler;
  end;

  // TCefAuthCallback
  // /include/capi/cef_auth_callback_capi.h (cef_auth_callback_t)
  ICefAuthCallback = interface(ICefBaseRefCounted)
    ['{500C2023-BF4D-4FF7-9C04-165E5C389131}']
    procedure Cont(const username, password: ustring);
    procedure Cancel;
  end;

  // TCefJsDialogCallback
  // /include/capi/cef_jsdialog_handler_capi.h (cef_jsdialog_callback_t)
  ICefJsDialogCallback = interface(ICefBaseRefCounted)
    ['{187B2156-9947-4108-87AB-32E559E1B026}']
    procedure Cont(success: Boolean; const userInput: ustring);
  end;

  // TCefContextMenuParams
  // /include/capi/cef_context_menu_handler_capi.h (cef_context_menu_params_t)
  ICefContextMenuParams = interface(ICefBaseRefCounted)
    ['{E31BFA9E-D4E2-49B7-A05D-20018C8794EB}']
    function GetXCoord: Integer;
    function GetYCoord: Integer;
    function GetTypeFlags: TCefContextMenuTypeFlags;
    function GetLinkUrl: ustring;
    function GetUnfilteredLinkUrl: ustring;
    function GetSourceUrl: ustring;
    function HasImageContents: Boolean;
    function GetTitleText: ustring;
    function GetPageUrl: ustring;
    function GetFrameUrl: ustring;
    function GetFrameCharset: ustring;
    function GetMediaType: TCefContextMenuMediaType;
    function GetMediaStateFlags: TCefContextMenuMediaStateFlags;
    function GetSelectionText: ustring;
    function GetMisspelledWord: ustring;
    function GetDictionarySuggestions(const suggestions: TStringList): Boolean;
    function IsEditable: Boolean;
    function IsSpellCheckEnabled: Boolean;
    function GetEditStateFlags: TCefContextMenuEditStateFlags;
    function IsCustomMenu: Boolean;
    function IsPepperMenu: Boolean;

    property XCoord            : Integer                        read GetXCoord;
    property YCoord            : Integer                        read GetYCoord;
    property TypeFlags         : TCefContextMenuTypeFlags       read GetTypeFlags;
    property LinkUrl           : ustring                        read GetLinkUrl;
    property UnfilteredLinkUrl : ustring                        read GetUnfilteredLinkUrl;
    property SourceUrl         : ustring                        read GetSourceUrl;
    property TitleText         : ustring                        read GetTitleText;
    property PageUrl           : ustring                        read GetPageUrl;
    property FrameUrl          : ustring                        read GetFrameUrl;
    property FrameCharset      : ustring                        read GetFrameCharset;
    property MediaType         : TCefContextMenuMediaType       read GetMediaType;
    property MediaStateFlags   : TCefContextMenuMediaStateFlags read GetMediaStateFlags;
    property SelectionText     : ustring                        read GetSelectionText;
    property EditStateFlags    : TCefContextMenuEditStateFlags  read GetEditStateFlags;
  end;

  // TCefMenuModel
  // /include/capi/cef_menu_model_capi.h (cef_menu_model_t)
  ICefMenuModel = interface(ICefBaseRefCounted)
    ['{40AF19D3-8B4E-44B8-8F89-DEB5907FC495}']
    function IsSubMenu: Boolean;
    function Clear: Boolean;
    function GetCount: Integer;
    function AddSeparator: Boolean;
    function AddItem(commandId: Integer; const text: ustring): Boolean;
    function AddCheckItem(commandId: Integer; const text: ustring): Boolean;
    function AddRadioItem(commandId: Integer; const text: ustring; groupId: Integer): Boolean;
    function AddSubMenu(commandId: Integer; const text: ustring): ICefMenuModel;
    function InsertSeparatorAt(index: Integer): Boolean;
    function InsertItemAt(index, commandId: Integer; const text: ustring): Boolean;
    function InsertCheckItemAt(index, commandId: Integer; const text: ustring): Boolean;
    function InsertRadioItemAt(index, commandId: Integer; const text: ustring; groupId: Integer): Boolean;
    function InsertSubMenuAt(index, commandId: Integer; const text: ustring): ICefMenuModel;
    function Remove(commandId: Integer): Boolean;
    function RemoveAt(index: Integer): Boolean;
    function GetIndexOf(commandId: Integer): Integer;
    function GetCommandIdAt(index: Integer): Integer;
    function SetCommandIdAt(index, commandId: Integer): Boolean;
    function GetLabel(commandId: Integer): ustring;
    function GetLabelAt(index: Integer): ustring;
    function SetLabel(commandId: Integer; const text: ustring): Boolean;
    function SetLabelAt(index: Integer; const text: ustring): Boolean;
    function GetType(commandId: Integer): TCefMenuItemType;
    function GetTypeAt(index: Integer): TCefMenuItemType;
    function GetGroupId(commandId: Integer): Integer;
    function GetGroupIdAt(index: Integer): Integer;
    function SetGroupId(commandId, groupId: Integer): Boolean;
    function SetGroupIdAt(index, groupId: Integer): Boolean;
    function GetSubMenu(commandId: Integer): ICefMenuModel;
    function GetSubMenuAt(index: Integer): ICefMenuModel;
    function IsVisible(commandId: Integer): Boolean;
    function isVisibleAt(index: Integer): Boolean;
    function SetVisible(commandId: Integer; visible: Boolean): Boolean;
    function SetVisibleAt(index: Integer; visible: Boolean): Boolean;
    function IsEnabled(commandId: Integer): Boolean;
    function IsEnabledAt(index: Integer): Boolean;
    function SetEnabled(commandId: Integer; enabled: Boolean): Boolean;
    function SetEnabledAt(index: Integer; enabled: Boolean): Boolean;
    function IsChecked(commandId: Integer): Boolean;
    function IsCheckedAt(index: Integer): Boolean;
    function setChecked(commandId: Integer; checked: Boolean): Boolean;
    function setCheckedAt(index: Integer; checked: Boolean): Boolean;
    function HasAccelerator(commandId: Integer): Boolean;
    function HasAcceleratorAt(index: Integer): Boolean;
    function SetAccelerator(commandId, keyCode: Integer; shiftPressed, ctrlPressed, altPressed: Boolean): Boolean;
    function SetAcceleratorAt(index, keyCode: Integer; shiftPressed, ctrlPressed, altPressed: Boolean): Boolean;
    function RemoveAccelerator(commandId: Integer): Boolean;
    function RemoveAcceleratorAt(index: Integer): Boolean;
    function GetAccelerator(commandId: Integer; out keyCode: Integer; out shiftPressed, ctrlPressed, altPressed: Boolean): Boolean;
    function GetAcceleratorAt(index: Integer; out keyCode: Integer; out shiftPressed, ctrlPressed, altPressed: Boolean): Boolean;
    function SetColor(commandId: Integer; colorType: TCefMenuColorType; color: TCefColor): Boolean;
    function SetColorAt(index: Integer; colorType: TCefMenuColorType; color: TCefColor): Boolean;
    function GetColor(commandId: Integer; colorType: TCefMenuColorType; out color: TCefColor): Boolean;
    function GetColorAt(index: Integer; colorType: TCefMenuColorType; out color: TCefColor): Boolean;
    function SetFontList(commandId: Integer; const fontList: ustring): Boolean;
    function SetFontListAt(index: Integer; const fontList: ustring): Boolean;
  end;

  // TCefValue
  // /include/capi/cef_values_capi.h (cef_value_t)
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
    function SetBool(value: Integer): Boolean;
    function SetInt(value: Integer): Boolean;
    function SetDouble(value: Double): Boolean;
    function SetString(const value: ustring): Boolean;
    function SetBinary(const value: ICefBinaryValue): Boolean;
    function SetDictionary(const value: ICefDictionaryValue): Boolean;
    function SetList(const value: ICefListValue): Boolean;
  end;

  // TCefBinaryValue
  // /include/capi/cef_values_capi.h (cef_binary_value_t)
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

  // TCefDictionaryValue
  // /include/capi/cef_values_capi.h (cef_dictionary_value_t)
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

  // TCefListValue
  // /include/capi/cef_values_capi.h (cef_list_value_t)
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

  // TCefLifeSpanHandler
  // /include/capi/cef_life_span_handler_capi.h (cef_life_span_handler_t)
  ICefLifeSpanHandler = interface(ICefBaseRefCounted)
    ['{0A3EB782-A319-4C35-9B46-09B2834D7169}']
    function  OnBeforePopup(const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var noJavascriptAccess: Boolean): Boolean;
    procedure OnAfterCreated(const browser: ICefBrowser);
    function  DoClose(const browser: ICefBrowser): Boolean;
    procedure OnBeforeClose(const browser: ICefBrowser);

    procedure RemoveReferences; // custom procedure to clear all references
  end;

  // TCefGetExtensionResourceCallback
  // /include/capi/cef_extension_handler_capi.h (cef_get_extension_resource_callback_t)
  ICefGetExtensionResourceCallback = interface(ICefBaseRefCounted)
    ['{579C8602-8252-40D0-9E0A-501F32C36C42}']
    procedure cont(const stream: ICefStreamReader);
    procedure cancel;
  end;

  // TCefExtensionHandler
  // /include/capi/cef_extension_handler_capi.h (cef_extension_handler_t)
  ICefExtensionHandler = interface(ICefBaseRefCounted)
    ['{3234008F-D809-459D-963D-23BA50219648}']
    procedure OnExtensionLoadFailed(result: TCefErrorcode);
    procedure OnExtensionLoaded(const extension: ICefExtension);
    procedure OnExtensionUnloaded(const extension: ICefExtension);
    function  OnBeforeBackgroundBrowser(const extension: ICefExtension; const url: ustring; var client: ICefClient; var settings: TCefBrowserSettings) : boolean;
    function  OnBeforeBrowser(const extension: ICefExtension; const browser, active_browser: ICefBrowser; index: Integer; const url: ustring; active: boolean; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings) : boolean;
    function  GetActiveBrowser(const extension: ICefExtension; const browser: ICefBrowser; include_incognito: boolean): ICefBrowser;
    function  CanAccessBrowser(const extension: ICefExtension; const browser: ICefBrowser; include_incognito: boolean; const target_browser: ICefBrowser): boolean;
    function  GetExtensionResource(const extension: ICefExtension; const browser: ICefBrowser; const file_: ustring; const callback: ICefGetExtensionResourceCallback): boolean;
  end;

  // TCefExtension
  // /include/capi/cef_extension_capi.h (cef_extension_t)
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

    property  Identifier    : ustring              read GetIdentifier;
    property  Path          : ustring              read GetPath;
    property  Manifest      : ICefDictionaryValue  read GetManifest;
    property  Handler       : ICefExtensionHandler read GetHandler;
    property  LoaderContext : ICefRequestContext   read GetLoaderContext;
  end;

  // TCefLoadHandler
  // /include/capi/cef_load_handler_capi.h (cef_load_handler_t)
  ICefLoadHandler = interface(ICefBaseRefCounted)
    ['{2C63FB82-345D-4A5B-9858-5AE7A85C9F49}']
    procedure OnLoadingStateChange(const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
    procedure OnLoadStart(const browser: ICefBrowser; const frame: ICefFrame; transitionType: TCefTransitionType);
    procedure OnLoadEnd(const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: Integer);
    procedure OnLoadError(const browser: ICefBrowser; const frame: ICefFrame; errorCode: Integer; const errorText, failedUrl: ustring);

    procedure RemoveReferences; // custom procedure to clear all references
  end;

  // TCefRequestCallback
  // /include/capi/cef_request_handler_capi.h (cef_request_callback_t)
  ICefRequestCallback = interface(ICefBaseRefCounted)
    ['{A35B8FD5-226B-41A8-A763-1940787D321C}']
    procedure Cont(allow: Boolean);
    procedure Cancel;
  end;

  // TCefResponseFilter
  // /include/capi/cef_response_filter_capi.h (cef_response_filter_t)
  ICefResponseFilter = interface(ICefBaseRefCounted)
    ['{5013BC3C-F1AE-407A-A571-A4C6B1D6831E}']
    function InitFilter: Boolean;
    function Filter(data_in: Pointer; data_in_size: NativeUInt; var data_in_read: NativeUInt; data_out: Pointer; data_out_size : NativeUInt; var data_out_written: NativeUInt): TCefResponseFilterStatus;
  end;

  // TCefRequestHandler
  // /include/capi/cef_request_handler_capi.h (cef_request_handler_t)
  ICefRequestHandler = interface(ICefBaseRefCounted)
    ['{050877A9-D1F8-4EB3-B58E-50DC3E3D39FD}']
    function  OnBeforeBrowse(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; user_gesture, isRedirect: Boolean): Boolean;
    function  OnOpenUrlFromTab(const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean): Boolean;
    function  OnBeforeResourceLoad(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const callback: ICefRequestCallback): TCefReturnValue;
    function  GetResourceHandler(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest): ICefResourceHandler;
    procedure OnResourceRedirect(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; var newUrl: ustring);
    function  OnResourceResponse(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse): Boolean;
    function  GetResourceResponseFilter(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse): ICefResponseFilter;
    procedure OnResourceLoadComplete(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; status: TCefUrlRequestStatus; receivedContentLength: Int64);
    function  GetAuthCredentials(const browser: ICefBrowser; const frame: ICefFrame; isProxy: Boolean; const host: ustring; port: Integer; const realm, scheme: ustring; const callback: ICefAuthCallback): Boolean;
    function  CanGetCookies(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest): boolean;
    function  CanSetCookie(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const cookie : PCefCookie): boolean;
    function  OnQuotaRequest(const browser: ICefBrowser; const originUrl: ustring; newSize: Int64; const callback: ICefRequestCallback): Boolean;
    procedure OnProtocolExecution(const browser: ICefBrowser; const url: ustring; out allowOsExecution: Boolean);
    function  OnCertificateError(const browser: ICefBrowser; certError: TCefErrorcode; const requestUrl: ustring; const sslInfo: ICefSslInfo; const callback: ICefRequestCallback): Boolean;
    function  OnSelectClientCertificate(const browser: ICefBrowser; isProxy: boolean; const host: ustring; port: integer; certificatesCount: NativeUInt; const certificates: TCefX509CertificateArray; const callback: ICefSelectClientCertificateCallback): boolean;
    procedure OnPluginCrashed(const browser: ICefBrowser; const pluginPath: ustring);
    procedure OnRenderViewReady(const browser: ICefBrowser);
    procedure OnRenderProcessTerminated(const browser: ICefBrowser; status: TCefTerminationStatus);

    procedure RemoveReferences; // custom procedure to clear all references
  end;

  // TCefDisplayHandler
  // /include/capi/cef_display_handler_capi.h (cef_display_handler_t)
  ICefDisplayHandler = interface(ICefBaseRefCounted)
    ['{1EC7C76D-6969-41D1-B26D-079BCFF054C4}']
    procedure OnAddressChange(const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
    procedure OnTitleChange(const browser: ICefBrowser; const title: ustring);
    procedure OnFaviconUrlChange(const browser: ICefBrowser; const icon_urls: TStrings);
    procedure OnFullScreenModeChange(const browser: ICefBrowser; fullscreen: Boolean);
    function  OnTooltip(const browser: ICefBrowser; var text: ustring): Boolean;
    procedure OnStatusMessage(const browser: ICefBrowser; const value: ustring);
    function  OnConsoleMessage(const browser: ICefBrowser; level: TCefLogSeverity; const message_, source: ustring; line: Integer): Boolean;
    function  OnAutoResize(const browser: ICefBrowser; const new_size: PCefSize): Boolean;
    procedure OnLoadingProgressChange(const browser: ICefBrowser; const progress: double);

    procedure RemoveReferences; // custom procedure to clear all references
  end;

  // TCefFocusHandler
  // /include/capi/cef_focus_handler_capi.h (cef_focus_handler_t)
  ICefFocusHandler = interface(ICefBaseRefCounted)
    ['{BB7FA3FA-7B1A-4ADC-8E50-12A24018DD90}']
    procedure OnTakeFocus(const browser: ICefBrowser; next: Boolean);
    function  OnSetFocus(const browser: ICefBrowser; source: TCefFocusSource): Boolean;
    procedure OnGotFocus(const browser: ICefBrowser);

    procedure RemoveReferences; // custom procedure to clear all references
  end;

  // TCefKeyboardHandler
  // /include/capi/cef_keyboard_handler_capi.h (cef_keyboard_handler_t)
  ICefKeyboardHandler = interface(ICefBaseRefCounted)
    ['{0512F4EC-ED88-44C9-90D3-5C6D03D3B146}']
    function OnPreKeyEvent(const browser: ICefBrowser; const event: PCefKeyEvent; osEvent: TCefEventHandle; out isKeyboardShortcut: Boolean): Boolean;
    function OnKeyEvent(const browser: ICefBrowser; const event: PCefKeyEvent; osEvent: TCefEventHandle): Boolean;

    procedure RemoveReferences; // custom procedure to clear all references
  end;

  // TCefJsDialogHandler
  // /include/capi/cef_jsdialog_handler_capi.h (cef_jsdialog_handler_t)
  ICefJsDialogHandler = interface(ICefBaseRefCounted)
    ['{64E18F86-DAC5-4ED1-8589-44DE45B9DB56}']
    function  OnJsdialog(const browser: ICefBrowser; const originUrl: ustring; dialogType: TCefJsDialogType; const messageText, defaultPromptText: ustring; const callback: ICefJsDialogCallback; out suppressMessage: Boolean): Boolean;
    function  OnBeforeUnloadDialog(const browser: ICefBrowser; const messageText: ustring; isReload: Boolean; const callback: ICefJsDialogCallback): Boolean;
    procedure OnResetDialogState(const browser: ICefBrowser);
    procedure OnDialogClosed(const browser: ICefBrowser);

    procedure RemoveReferences; // custom procedure to clear all references
  end;

  // TCefRunContextMenuCallback
  // /include/capi/cef_context_menu_handler_capi.h (cef_run_context_menu_callback_t)
  ICefRunContextMenuCallback = interface(ICefBaseRefCounted)
    ['{44C3C6E3-B64D-4F6E-A318-4A0F3A72EB00}']
    procedure Cont(commandId: Integer; eventFlags: TCefEventFlags);
    procedure Cancel;
  end;

  // TCefContextMenuHandler
  // /include/capi/cef_context_menu_handler_capi.h (cef_context_menu_handler_t)
  ICefContextMenuHandler = interface(ICefBaseRefCounted)
    ['{C2951895-4087-49D5-BA18-4D9BA4F5EDD7}']
    procedure OnBeforeContextMenu(const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; const model: ICefMenuModel);
    function  RunContextMenu(const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; const model: ICefMenuModel; const callback: ICefRunContextMenuCallback): Boolean;
    function  OnContextMenuCommand(const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; commandId: Integer; eventFlags: TCefEventFlags): Boolean;
    procedure OnContextMenuDismissed(const browser: ICefBrowser; const frame: ICefFrame);

    procedure RemoveReferences; // custom procedure to clear all references
  end;

  // TCefAccessibilityHandler
  // /include/capi/cef_accessibility_handler_capi.h (cef_accessibility_handler_t)
  ICefAccessibilityHandler = interface(ICefBaseRefCounted)
    ['{1878C3C7-7692-44AB-BFE0-6C387106816B}']
    procedure OnAccessibilityTreeChange(const value: ICefValue);
    procedure OnAccessibilityLocationChange(const value: ICefValue);
  end;

  // TCefDialogHandler
  // /include/capi/cef_dialog_handler_capi.h (cef_dialog_handler_t)
  ICefDialogHandler = interface(ICefBaseRefCounted)
    ['{7763F4B2-8BE1-4E80-AC43-8B825850DC67}']
    function OnFileDialog(const browser: ICefBrowser; mode: TCefFileDialogMode; const title, defaultFilePath: ustring; const acceptFilters: TStrings; selectedAcceptFilter: Integer; const callback: ICefFileDialogCallback): Boolean;

    procedure RemoveReferences; // custom procedure to clear all references
  end;

  // TCefRenderHandler
  // /include/capi/cef_render_handler_capi.h (cef_render_handler_t)
  ICefRenderHandler = interface(ICefBaseRefCounted)
    ['{1FC1C22B-085A-4741-9366-5249B88EC410}']
    procedure GetAccessibilityHandler(var aAccessibilityHandler : ICefAccessibilityHandler);
    function  GetRootScreenRect(const browser: ICefBrowser; var rect: TCefRect): Boolean;
    function  GetViewRect(const browser: ICefBrowser; var rect: TCefRect): Boolean;
    function  GetScreenPoint(const browser: ICefBrowser; viewX, viewY: Integer; var screenX, screenY: Integer): Boolean;
    function  GetScreenInfo(const browser: ICefBrowser; var screenInfo: TCefScreenInfo): Boolean;
    procedure OnPopupShow(const browser: ICefBrowser; show: Boolean);
    procedure OnPopupSize(const browser: ICefBrowser; const rect: PCefRect);
    procedure OnPaint(const browser: ICefBrowser; kind: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; const buffer: Pointer; width, height: Integer);
    procedure OnCursorChange(const browser: ICefBrowser; cursor: TCefCursorHandle; CursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo);
    function  OnStartDragging(const browser: ICefBrowser; const dragData: ICefDragData; allowedOps: TCefDragOperations; x, y: Integer): Boolean;
    procedure OnUpdateDragCursor(const browser: ICefBrowser; operation: TCefDragOperation);
    procedure OnScrollOffsetChanged(const browser: ICefBrowser; x, y: Double);
    procedure OnIMECompositionRangeChanged(const browser: ICefBrowser; const selected_range: PCefRange; character_boundsCount: NativeUInt; const character_bounds: PCefRect);
    procedure OnTextSelectionChanged(const browser: ICefBrowser; const selected_text: ustring; const selected_range: PCefRange);

    procedure RemoveReferences; // custom procedure to clear all references
  end;

  // TCefClient
  // /include/capi/cef_client_capi.h (cef_client_t)
  ICefClient = interface(ICefBaseRefCounted)
    ['{1D502075-2FF0-4E13-A112-9E541CD811F4}']
    procedure GetContextMenuHandler(var aHandler : ICefContextMenuHandler);
    procedure GetDialogHandler(var aHandler : ICefDialogHandler);
    procedure GetDisplayHandler(var aHandler : ICefDisplayHandler);
    procedure GetDownloadHandler(var aHandler : ICefDownloadHandler);
    procedure GetDragHandler(var aHandler : ICefDragHandler);
    procedure GetFindHandler(var aHandler : ICefFindHandler);
    procedure GetFocusHandler(var aHandler : ICefFocusHandler);
    procedure GetJsdialogHandler(var aHandler : ICefJsdialogHandler);
    procedure GetKeyboardHandler(var aHandler : ICefKeyboardHandler);
    procedure GetLifeSpanHandler(var aHandler : ICefLifeSpanHandler);
    procedure GetLoadHandler(var aHandler : ICefLoadHandler);
    procedure GetRenderHandler(var aHandler : ICefRenderHandler);
    procedure GetRequestHandler(var aHandler : ICefRequestHandler);
    function  OnProcessMessageReceived(const browser: ICefBrowser; sourceProcess: TCefProcessId; const message_ : ICefProcessMessage): Boolean;

    procedure RemoveReferences; // custom procedure to clear all references
  end;

  // TCefUrlRequest
  // /include/capi/cef_urlrequest_capi.h (cef_urlrequest_t)
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

  // TCefUrlrequestClient
  // /include/capi/cef_urlrequest_capi.h (cef_urlrequest_client_t)
  ICefUrlrequestClient = interface(ICefBaseRefCounted)
    ['{114155BD-C248-4651-9A4F-26F3F9A4F737}']
    procedure OnRequestComplete(const request: ICefUrlRequest);
    procedure OnUploadProgress(const request: ICefUrlRequest; current, total: Int64);
    procedure OnDownloadProgress(const request: ICefUrlRequest; current, total: Int64);
    procedure OnDownloadData(const request: ICefUrlRequest; data: Pointer; dataLength: NativeUInt);
    function  OnGetAuthCredentials(isProxy: Boolean; const host: ustring; port: Integer; const realm, scheme: ustring; const callback: ICefAuthCallback): Boolean;
  end;

  // TCefWebPluginInfoVisitor
  // /include/capi/cef_web_plugin_capi.h (cef_web_plugin_info_visitor_t)
  ICefWebPluginInfoVisitor = interface(ICefBaseRefCounted)
    ['{7523D432-4424-4804-ACAD-E67D2313436E}']
    function Visit(const info: ICefWebPluginInfo; count, total: Integer): Boolean;
  end;

  // TCefWebPluginUnstableCallback
  // /include/capi/cef_web_plugin_capi.h (cef_web_plugin_unstable_callback_t)
  ICefWebPluginUnstableCallback = interface(ICefBaseRefCounted)
    ['{67459829-EB47-4B7E-9D69-2EE77DF0E71E}']
    procedure IsUnstable(const path: ustring; unstable: Boolean);
  end;

  // TCefRegisterCDMCallback
  // /include/capi/cef_web_plugin_capi.h (cef_register_cdm_callback_t)
  ICefRegisterCDMCallback = interface(ICefBaseRefCounted)
    ['{6C39AB3B-F724-483F-ABA0-37F6E0AECF35}']
    procedure OnCDMRegistrationComplete(result: TCefCDMRegistrationError; const error_message: ustring);
  end;

  // TCefEndTracingCallback
  // /include/capi/cef_trace_capi.h (cef_end_tracing_callback_t)
  ICefEndTracingCallback = interface(ICefBaseRefCounted)
    ['{79020EBE-9D1D-49A6-9714-8778FE8929F2}']
    procedure OnEndTracingComplete(const tracingFile: ustring);
  end;

  // TCefFileDialogCallback
  // /include/capi/cef_dialog_handler_capi.h (cef_file_dialog_callback_t)
  ICefFileDialogCallback = interface(ICefBaseRefCounted)
    ['{1AF659AB-4522-4E39-9C52-184000D8E3C7}']
    procedure Cont(selectedAcceptFilter: Integer; const filePaths: TStrings);
    procedure Cancel;
  end;

  // TCefDragData
  // /include/capi/cef_drag_data_capi.h (cef_drag_data_t)
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
    function  GetImage : ICefImage;
    function  GetImageHotspot : TCefPoint;
    function  HasImage : boolean;
  end;

  // TCefDragHandler
  // /include/capi/cef_drag_handler_capi.h (cef_drag_handler_t)
  ICefDragHandler = interface(ICefBaseRefCounted)
    ['{59A89579-5B18-489F-A25C-5CC25FF831FC}']
    function  OnDragEnter(const browser: ICefBrowser; const dragData: ICefDragData; mask: TCefDragOperations): Boolean;
    procedure OnDraggableRegionsChanged(const browser: ICefBrowser; regionsCount: NativeUInt; regions: PCefDraggableRegionArray);

    procedure RemoveReferences; // custom procedure to clear all references
  end;

  // TCefFindHandler
  // /include/capi/cef_find_handler_capi.h (cef_find_handler_t)
  ICefFindHandler = interface(ICefBaseRefCounted)
    ['{F20DF234-BD43-42B3-A80B-D354A9E5B787}']
    procedure OnFindResult(const browser: ICefBrowser; identifier, count: Integer; const selectionRect: PCefRect; activeMatchOrdinal: Integer; finalUpdate: Boolean);

    procedure RemoveReferences; // custom procedure to clear all references
  end;

  // TCefRequestContextHandler
  // /include/capi/cef_request_context_handler_capi.h (cef_request_context_handler_t)
  ICefRequestContextHandler = interface(ICefBaseRefCounted)
    ['{76EB1FA7-78DF-4FD5-ABB3-1CDD3E73A140}']
    procedure OnRequestContextInitialized(const request_context: ICefRequestContext);
    function  GetCookieManager: ICefCookieManager;
    function  OnBeforePluginLoad(const mimeType, pluginUrl:ustring; isMainFrame : boolean; const topOriginUrl: ustring; const pluginInfo: ICefWebPluginInfo; pluginPolicy: PCefPluginPolicy): Boolean;
  end;

  // TCefResolveCallback
  // /include/capi/cef_request_context_capi.h (cef_resolve_callback_t)
  ICefResolveCallback = interface(ICefBaseRefCounted)
    ['{0C0EA252-7968-4163-A1BE-A1453576DD06}']
    procedure OnResolveCompleted(result: TCefErrorCode; const resolvedIps: TStrings);
  end;

  // TCefRequestContext
  // /include/capi/cef_request_context_capi.h (cef_request_context_t)
  ICefRequestContext = interface(ICefBaseRefCounted)
    ['{5830847A-2971-4BD5-ABE6-21451F8923F7}']
    function  IsSame(const other: ICefRequestContext): Boolean;
    function  IsSharingWith(const other: ICefRequestContext): Boolean;
    function  IsGlobal: Boolean;
    function  GetHandler: ICefRequestContextHandler;
    function  GetCachePath: ustring;
    function  GetDefaultCookieManager(const callback: ICefCompletionCallback): ICefCookieManager;
    function  GetDefaultCookieManagerProc(const callback: TCefCompletionCallbackProc): ICefCookieManager;
    function  RegisterSchemeHandlerFactory(const schemeName, domainName: ustring; const factory: ICefSchemeHandlerFactory): Boolean;
    function  ClearSchemeHandlerFactories: Boolean;
    procedure PurgePluginListCache(reloadPages: Boolean);
    function  HasPreference(const name: ustring): Boolean;
    function  GetPreference(const name: ustring): ICefValue;
    function  GetAllPreferences(includeDefaults: Boolean): ICefDictionaryValue;
    function  CanSetPreference(const name: ustring): Boolean;
    function  SetPreference(const name: ustring; const value: ICefValue; out error: ustring): Boolean;
    procedure ClearCertificateExceptions(const callback: ICefCompletionCallback);
    procedure CloseAllConnections(const callback: ICefCompletionCallback);
    procedure ResolveHost(const origin: ustring; const callback: ICefResolveCallback);
    function  ResolveHostCached(const origin: ustring; const resolvedIps: TStrings): TCefErrorCode;
    procedure LoadExtension(const root_directory: ustring; const manifest: ICefDictionaryValue; const handler: ICefExtensionHandler);
    function  DidLoadExtension(const extension_id: ustring): boolean;
    function  HasExtension(const extension_id: ustring): boolean;
    function  GetExtensions(const extension_ids: TStringList): boolean;
    function  GetExtension(const extension_id: ustring): ICefExtension;

    property  CachePath        : ustring  read GetCachePath;
    property  IsGlobalContext  : boolean  read IsGlobal;
  end;

  // TCefPrintSettings
  // /include/capi/cef_print_settings_capi.h (cef_print_settings_t)
  ICefPrintSettings = Interface(ICefBaseRefCounted)
    ['{ACBD2395-E9C1-49E5-B7F3-344DAA4A0F12}']
    function  IsValid: Boolean;
    function  IsReadOnly: Boolean;
    function  Copy: ICefPrintSettings;
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

  // TCefPrintDialogCallback
  // /include/capi/cef_print_handler_capi.h (cef_print_dialog_callback_t)
  ICefPrintDialogCallback = interface(ICefBaseRefCounted)
    ['{1D7FB71E-0019-4A80-95ED-91DDD019253B}']
    procedure cont(const settings: ICefPrintSettings);
    procedure cancel;
  end;

  // TCefPrintJobCallback
  // /include/capi/cef_print_handler_capi.h (cef_print_job_callback_t)
  ICefPrintJobCallback = interface(ICefBaseRefCounted)
    ['{5554852A-052C-464B-A868-B618C7E7E2FD}']
    procedure cont;
  end;

  // TCefPrintHandler
  // /include/capi/cef_print_handler_capi.h (cef_print_handler_t)
  ICefPrintHandler = interface(ICefBaseRefCounted)
    ['{2831D5C9-6E2B-4A30-A65A-0F4435371EFC}']
    procedure OnPrintStart(const browser: ICefBrowser);
    procedure OnPrintSettings(const browser: ICefBrowser; const settings: ICefPrintSettings; getDefaults: boolean);
    function  OnPrintDialog(const browser: ICefBrowser; hasSelection: boolean; const callback: ICefPrintDialogCallback): boolean;
    function  OnPrintJob(const browser: ICefBrowser; const documentName, PDFFilePath: ustring; const callback: ICefPrintJobCallback): boolean;
    procedure OnPrintReset(const browser: ICefBrowser);
    function  GetPDFPaperSize(deviceUnitsPerInch: Integer): TCefSize;
  end;

  // TCefNavigationEntry
  // /include/capi/cef_navigation_entry_capi.h (cef_navigation_entry_t)
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

  // TCefX509CertPrincipal
  // /include/capi/cef_x509_certificate_capi.h (cef_x509cert_principal_t)
  ICefX509CertPrincipal = interface(ICefBaseRefCounted)
    ['{CD3621ED-7D68-4A1F-95B5-190C7001B65F}']
    function  GetDisplayName: ustring;
    function  GetCommonName: ustring;
    function  GetLocalityName: ustring;
    function  GetStateOrProvinceName: ustring;
    function  GetCountryName: ustring;
    procedure GetStreetAddresses(const addresses: TStrings);
    procedure GetOrganizationNames(const names: TStrings);
    procedure GetOrganizationUnitNames(const names: TStrings);
    procedure GetDomainComponents(const components: TStrings);
  end;

  // TCefX509Certificate
  // /include/capi/cef_x509_certificate_capi.h (cef_x509certificate_t)
  ICefX509Certificate = interface(ICefBaseRefCounted)
    ['{C897979D-F068-4428-82DF-4221612FF7E0}']
    function  GetSubject: ICefX509CertPrincipal;
    function  GetIssuer: ICefX509CertPrincipal;
    function  GetSerialNumber: ICefBinaryValue;
    function  GetValidStart: TCefTime;
    function  GetValidExpiry: TCefTime;
    function  GetDerEncoded: ICefBinaryValue;
    function  GetPemEncoded: ICefBinaryValue;
    function  GetIssuerChainSize: NativeUInt;
    procedure GetDEREncodedIssuerChain(chainCount: NativeUInt; var chain : TCefBinaryValueArray);
    procedure GetPEMEncodedIssuerChain(chainCount: NativeUInt; var chain : TCefBinaryValueArray);
  end;

  // TCefSslInfo
  // /include/capi/cef_ssl_info_capi.h (cef_sslinfo_t)
  ICefSslInfo = interface(ICefBaseRefCounted)
    ['{67EC86BD-DE7D-453D-908F-AD15626C514F}']
    function GetCertStatus: TCefCertStatus;
    function GetX509Certificate: ICefX509Certificate;
  end;

  // TCefSSLStatus
  // /include/capi/cef_ssl_status_capi.h (cef_sslstatus_t)
  ICefSSLStatus = interface(ICefBaseRefCounted)
    ['{E3F004F2-03D5-46A2-91D0-510C50F3B225}']
    function IsSecureConnection: boolean;
    function GetCertStatus: TCefCertStatus;
    function GetSSLVersion: TCefSSLVersion;
    function GetContentStatus: TCefSSLContentStatus;
    function GetX509Certificate: ICefX509Certificate;
  end;

  // TCefSelectClientCertificateCallback
  // /include/capi/cef_request_handler_capi.h (cef_select_client_certificate_callback_t)
  ICefSelectClientCertificateCallback = interface(ICefBaseRefCounted)
    ['{003E3D09-ADE8-4C6E-A174-079D3D616608}']
    procedure Select(const cert: ICefX509Certificate);
  end;

  // TCefResourceBundle
  // /include/capi/cef_resource_bundle_capi.h (cef_resource_bundle_t)
  ICefResourceBundle = interface(ICefBaseRefCounted)
    ['{3213CF97-C854-452B-B615-39192F8D07DC}']
    function GetLocalizedString(stringId: Integer): ustring;
    function GetDataResource(resourceId: Integer; var data: Pointer; var dataSize: NativeUInt): Boolean;
    function GetDataResourceForScale(resourceId: Integer; scaleFactor: TCefScaleFactor; var data: Pointer; var dataSize: NativeUInt): Boolean;
  end;

  // TCefImage
  // /include/capi/cef_image_capi.h (cef_image_t)
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
    function GetRepresentationInfo(scaleFactor: Single; actualScaleFactor: PSingle; pixelWidth, pixelHeight: PInteger): Boolean;
    function GetAsBitmap(scaleFactor: Single; colorType: TCefColorType; alphaType: TCefAlphaType; pixelWidth, pixelHeight: PInteger): ICefBinaryValue;
    function GetAsPng(scaleFactor: Single; withTransparency: Boolean; pixelWidth, pixelHeight: PInteger): ICefBinaryValue;
    function GetAsJpeg(scaleFactor: Single; quality: Integer; pixelWidth, pixelHeight: PInteger): ICefBinaryValue;

    property Width  : NativeUInt read GetWidth;
    property Height : NativeUInt read GetHeight;
  end;

  // TCefMenuModelDelegate
  // /include/capi/cef_menu_model_delegate_capi.h (cef_menu_model_delegate_t)
  ICefMenuModelDelegate = interface(ICefBaseRefCounted)
    ['{1430D202-2795-433E-9A35-C79A0996F316}']
    procedure ExecuteCommand(const menuModel: ICefMenuModel; commandId: Integer; eventFlags: TCefEventFlags);
    procedure MouseOutsideMenu(const menuModel: ICefMenuModel; const screenPoint: PCefPoint);
    procedure UnhandledOpenSubmenu(const menuModel: ICefMenuModel; isRTL: boolean);
    procedure UnhandledCloseSubmenu(const menuModel: ICefMenuModel; isRTL: boolean);
    procedure MenuWillShow(const menuModel: ICefMenuModel);
    procedure MenuClosed(const menuModel: ICefMenuModel);
    function  FormatLabel(const menuModel: ICefMenuModel; const label_ : ustring) : boolean;
  end;

  // TCefServer
  // /include/capi/cef_server_capi.h (cef_server_t)
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

  // TCefServerHandler
  // /include/capi/cef_server_capi.h (cef_server_handler_t)
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

implementation

end.
