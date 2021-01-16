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

unit uCEFInterfaces;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

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
  ICefWebPluginInfo = interface;
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
    Description : ustring;
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
  // ***************** Events ******************
  // *******************************************


  TOnRegisterCustomSchemesEvent      = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const registrar: TCefSchemeRegistrarRef) {$IFNDEF DELPHI12_UP}{$IFNDEF FPC}of object{$ENDIF}{$ENDIF};
  TOnRenderThreadCreatedEvent        = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const extraInfo: ICefListValue) {$IFNDEF DELPHI12_UP}{$IFNDEF FPC}of object{$ENDIF}{$ENDIF};
  TOnWebKitInitializedEvent          = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure() {$IFNDEF DELPHI12_UP}{$IFNDEF FPC}of object{$ENDIF}{$ENDIF};
  TOnBrowserCreatedEvent             = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const browser: ICefBrowser; const extra_info: ICefDictionaryValue) {$IFNDEF DELPHI12_UP}{$IFNDEF FPC}of object{$ENDIF}{$ENDIF};
  TOnBrowserDestroyedEvent           = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const browser: ICefBrowser) {$IFNDEF DELPHI12_UP}{$IFNDEF FPC}of object{$ENDIF}{$ENDIF};
  TOnContextCreatedEvent             = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context) {$IFNDEF DELPHI12_UP}{$IFNDEF FPC}of object{$ENDIF}{$ENDIF};
  TOnContextReleasedEvent            = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context) {$IFNDEF DELPHI12_UP}{$IFNDEF FPC}of object{$ENDIF}{$ENDIF};
  TOnUncaughtExceptionEvent          = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context; const exception: ICefV8Exception; const stackTrace: ICefV8StackTrace) {$IFNDEF DELPHI12_UP}{$IFNDEF FPC}of object{$ENDIF}{$ENDIF};
  TOnFocusedNodeChangedEvent         = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const browser: ICefBrowser; const frame: ICefFrame; const node: ICefDomNode) {$IFNDEF DELPHI12_UP}{$IFNDEF FPC}of object{$ENDIF}{$ENDIF};
  TOnProcessMessageReceivedEvent     = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const browser: ICefBrowser; const frame: ICefFrame; sourceProcess: TCefProcessId; const message: ICefProcessMessage; var aHandled : boolean) {$IFNDEF DELPHI12_UP}{$IFNDEF LCL} of object {$ENDIF} {$ENDIF};
  TOnGetCookieableSchemesEvent       = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(var schemes: TStringList; var include_defaults : boolean) {$IFNDEF DELPHI12_UP}{$IFNDEF LCL} of object {$ENDIF} {$ENDIF};
  TOnContextInitializedEvent         = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure() {$IFNDEF DELPHI12_UP}{$IFNDEF FPC} of object{$ENDIF}{$ENDIF};
  TOnBeforeChildProcessLaunchEvent   = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const commandLine: ICefCommandLine) {$IFNDEF DELPHI12_UP}{$IFNDEF FPC}of object{$ENDIF}{$ENDIF};
  TOnRenderProcessThreadCreatedEvent = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const extraInfo: ICefListValue) {$IFNDEF DELPHI12_UP}{$IFNDEF FPC}of object{$ENDIF}{$ENDIF};
  TOnScheduleMessagePumpWorkEvent    = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const delayMs: Int64) {$IFNDEF DELPHI12_UP}{$IFNDEF FPC} of object{$ENDIF}{$ENDIF};
  TOnGetDefaultClientEvent           = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(var aClient : ICefClient) {$IFNDEF DELPHI12_UP}{$IFNDEF FPC} of object{$ENDIF}{$ENDIF};
  TOnGetDataResourceEvent            = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(resourceId: Integer; out data: Pointer; out dataSize: NativeUInt; var aResult : Boolean) {$IFNDEF DELPHI12_UP}{$IFNDEF FPC}of object{$ENDIF}{$ENDIF};
  TOnGetLocalizedStringEvent         = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(stringId: Integer; out stringVal: ustring; var aResult : Boolean) {$IFNDEF DELPHI12_UP}{$IFNDEF FPC}of object{$ENDIF}{$ENDIF};
  TOnGetDataResourceForScaleEvent    = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(resourceId: Integer; scaleFactor: TCefScaleFactor; out data: Pointer; out dataSize: NativeUInt; var aResult : Boolean) {$IFNDEF DELPHI12_UP}{$IFNDEF FPC}of object{$ENDIF}{$ENDIF};
  TOnCDMRegistrationCompleteEvent    = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(result : TCefCDMRegistrationError; const error_message : ustring) {$IFNDEF DELPHI12_UP}{$IFNDEF FPC}of object{$ENDIF}{$ENDIF};
  TOnRenderLoadStart                 = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const browser: ICefBrowser; const frame: ICefFrame; transitionType: TCefTransitionType) {$IFNDEF DELPHI12_UP}{$IFNDEF FPC}of object{$ENDIF}{$ENDIF};
  TOnRenderLoadEnd                   = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: Integer) {$IFNDEF DELPHI12_UP}{$IFNDEF FPC}of object{$ENDIF}{$ENDIF};
  TOnRenderLoadError                 = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const browser: ICefBrowser; const frame: ICefFrame; errorCode: TCefErrorCode; const errorText, failedUrl: ustring) {$IFNDEF DELPHI12_UP}{$IFNDEF FPC}of object{$ENDIF}{$ENDIF};
  TOnRenderLoadingStateChange        = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean) {$IFNDEF DELPHI12_UP}{$IFNDEF FPC}of object{$ENDIF}{$ENDIF};
  TOnPrintStartEvent                 = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const browser: ICefBrowser) {$IFNDEF DELPHI12_UP}{$IFNDEF FPC}of object{$ENDIF}{$ENDIF};
  TOnPrintSettingsEvent              = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const browser: ICefBrowser; const settings: ICefPrintSettings; getDefaults: boolean) {$IFNDEF DELPHI12_UP}{$IFNDEF FPC}of object{$ENDIF}{$ENDIF};
  TOnPrintDialogEvent                = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const browser: ICefBrowser; hasSelection: boolean; const callback: ICefPrintDialogCallback; var aResult : boolean) {$IFNDEF DELPHI12_UP}{$IFNDEF FPC}of object{$ENDIF}{$ENDIF};
  TOnPrintJobEvent                   = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const browser: ICefBrowser; const documentName, PDFFilePath: ustring; const callback: ICefPrintJobCallback; var aResult : boolean) {$IFNDEF DELPHI12_UP}{$IFNDEF FPC}of object{$ENDIF}{$ENDIF};
  TOnPrintResetEvent                 = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const browser: ICefBrowser) {$IFNDEF DELPHI12_UP}{$IFNDEF FPC}of object{$ENDIF}{$ENDIF};
  TOnGetPDFPaperSizeEvent            = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(deviceUnitsPerInch: Integer; var aResult : TCefSize) {$IFNDEF DELPHI12_UP}{$IFNDEF FPC}of object{$ENDIF}{$ENDIF};

  // *******************************************
  // **** Callback procedures and functions ****
  // *******************************************


  TCefEndTracingCallbackProc           = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const tracingFile: ustring);
  TCefRegisterCDMProc                  = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(result: TCefCDMRegistrationError; const error_message: ustring);
  TCefFastTaskProc                     = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure();
  TCefv8ArrayBufferReleaseCallbackProc = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(buffer : Pointer);
  TCefWebPluginInfoVisitorProc         = {$IFDEF DELPHI12_UP}reference to{$ENDIF} function(const info: ICefWebPluginInfo; count, total: Integer): Boolean;
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
  TCefRunFileDialogCallbackProc        = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(selectedAcceptFilter: Integer; const filePaths: TStrings);
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
    procedure doOnCursorChange(const browser: ICefBrowser; cursor: TCefCursorHandle; cursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo; var aResult : boolean);

    // ICefDownloadHandler
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
    function  doOnQuotaRequest(const browser: ICefBrowser; const originUrl: ustring; newSize: Int64; const callback: ICefRequestCallback): Boolean;
    function  doOnCertificateError(const browser: ICefBrowser; certError: TCefErrorcode; const requestUrl: ustring; const sslInfo: ICefSslInfo; const callback: ICefRequestCallback): Boolean;
    function  doOnSelectClientCertificate(const browser: ICefBrowser; isProxy: boolean; const host: ustring; port: integer; certificatesCount: NativeUInt; const certificates: TCefX509CertificateArray; const callback: ICefSelectClientCertificateCallback): boolean;
    procedure doOnPluginCrashed(const browser: ICefBrowser; const pluginPath: ustring);
    procedure doOnRenderViewReady(const browser: ICefBrowser);
    procedure doOnRenderProcessTerminated(const browser: ICefBrowser; status: TCefTerminationStatus);
    procedure doOnDocumentAvailableInMainFrame(const browser: ICefBrowser);

    // ICefResourceRequestHandler
    function  doOnBeforeResourceLoad(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const callback: ICefRequestCallback): TCefReturnValue;
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
    function  doOnFileDialog(const browser: ICefBrowser; mode: TCefFileDialogMode; const title, defaultFilePath: ustring; const acceptFilters: TStrings; selectedAcceptFilter: Integer; const callback: ICefFileDialogCallback): Boolean;

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
    function  doOnBeforePluginLoad(const mimeType, pluginUrl:ustring; isMainFrame : boolean; const topOriginUrl: ustring; const pluginInfo: ICefWebPluginInfo; var pluginPolicy: TCefPluginPolicy): Boolean;
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
    procedure doUpdateSize(aLeft, aTop, aWidth, aHeight : integer);
    procedure doSendCaptureLostEvent;
    procedure doUpdateXWindowVisibility(aVisible : boolean);
    function  MustCreateAudioHandler : boolean;
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
    function  MustCreateRequestContextHandler : boolean;
    function  MustCreateMediaObserver : boolean;
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


  // TCefBaseRefCounted
  // /include/capi/cef_base_capi.h (cef_base_ref_counted_t)
  ICefBaseRefCounted = interface
    ['{1F9A7B44-DCDC-4477-9180-3ADD44BDEB7B}']
    function Wrap: Pointer;
    function SameAs(aData : Pointer) : boolean; overload;
    function SameAs(const aBaseRefCounted : ICefBaseRefCounted) : boolean; overload;
    function HasOneRef : boolean;
    function HasAtLeastOneRef : boolean;
    procedure DestroyOtherRefs;
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
    procedure Find(identifier: Integer; const searchText: ustring; forward_, matchCase, findNext: Boolean);
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
    procedure SendFocusEvent(aSetFocus: Boolean);
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
    function  GetElementCount: NativeUInt;
    procedure GetElements(elementsCount: NativeUInt; var elements: TCefPostDataElementArray);
    function  RemoveElement(const element: ICefPostDataElement): Boolean;
    function  AddElement(const element: ICefPostDataElement): Boolean;
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
    function Execute(const name: ustring; const object_: ICefv8Value; const arguments: TCefv8ValueArray; var retval: ICefv8Value; var exception: ustring): Boolean;
  end;

  // TCefV8Interceptor
  // /include/capi/cef_v8_capi.h (cef_v8interceptor_t)
  ICefV8Interceptor = interface(ICefBaseRefCounted)
    ['{B3B8FD7C-A916-4B25-93A2-2892AC324F21}']
    function GetByName(const name: ustring; const object_: ICefv8Value; var retval: ICefv8Value; var exception: ustring): boolean;
    function GetByIndex(index: integer; const object_: ICefv8Value; var retval: ICefv8Value; var exception: ustring): boolean;
    function SetByName(const name: ustring; const object_, value: ICefv8Value; var exception: ustring): boolean;
    function SetByIndex(index: integer; const object_, value: ICefv8Value; var exception: ustring): boolean;
  end;

  // TCefV8Accessor
  // /include/capi/cef_v8_capi.h (cef_v8accessor_t)
  ICefV8Accessor = interface(ICefBaseRefCounted)
    ['{DCA6D4A2-726A-4E24-AA64-5E8C731D868A}']
    function Get(const name: ustring; const object_: ICefv8Value; var retval: ICefv8Value; var exception: ustring): Boolean;
    function Set_(const name: ustring; const object_, value: ICefv8Value; var exception: ustring): Boolean;
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
    function visit(const name, value, domain, path: ustring; secure, httponly, hasExpires: Boolean; const creation, lastAccess, expires: TDateTime; count, total: Integer; same_site : TCefCookieSameSite; priority : TCefCookiePriority; out deleteCookie: Boolean): Boolean;
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

  // TCefRegistration
  // /include/capi/cef_registration_capi.h (cef_registration_t)
  ICefRegistration = interface(ICefBaseRefCounted)
    ['{9226018F-7A56-4F2E-AF01-43268E33EE6B}']
  end;

  // TCefDevToolsMessageObserver
  // /include/capi/cef_devtools_message_observer_capi.h (cef_dev_tools_message_observer_t)
  ICefDevToolsMessageObserver = interface(ICefBaseRefCounted)
    ['{76E5BB2B-7F69-4BC9-94C7-B55C61CE630F}']
    procedure OnDevToolsMessage(const browser: ICefBrowser; const message_: Pointer; message_size: NativeUInt; var aHandled: boolean);
    procedure OnDevToolsMethodResult(const browser: ICefBrowser; message_id: integer; success: boolean; const result: Pointer; result_size: NativeUInt);
    procedure OnDevToolsEvent(const browser: ICefBrowser; const method: ustring; const params: Pointer; params_size: NativeUInt);
    procedure OnDevToolsAgentAttached(const browser: ICefBrowser);
    procedure OnDevToolsAgentDetached(const browser: ICefBrowser);
  end;

  // TCefMediaRouter
  // /include/capi/cef_media_router_capi.h (cef_media_router_t)
  ICefMediaRouter = interface(ICefBaseRefCounted)
    ['{F18C3880-CB8D-48F9-9D74-DCFF4B9E88DF}']
    function  AddObserver(const observer: ICefMediaObserver): ICefRegistration;
    function  GetSource(const urn: ustring): ICefMediaSource;
    procedure NotifyCurrentSinks;
    procedure CreateRoute(const source: ICefMediaSource; const sink: ICefMediaSink; const callback: ICefMediaRouteCreateCallback);
    procedure NotifyCurrentRoutes;
  end;

  // TCefMediaObserver
  // /include/capi/cef_media_router_capi.h (cef_media_observer_t)
  ICefMediaObserver = interface(ICefBaseRefCounted)
    ['{0B27C8D1-63E3-4F69-939F-DCAD518654A3}']
    procedure OnSinks(const sinks: TCefMediaSinkArray);
    procedure OnRoutes(const routes: TCefMediaRouteArray);
    procedure OnRouteStateChanged(const route: ICefMediaRoute; state: TCefMediaRouteConnectionState);
    procedure OnRouteMessageReceived(const route: ICefMediaRoute; const message_: ustring);
  end;

  ICefMediaObserverEvents = interface
    ['{267D5287-08DB-49D6-AF6E-B27C66C6E5D4}']
    procedure doOnSinks(const sinks: TCefMediaSinkArray);
    procedure doOnRoutes(const routes: TCefMediaRouteArray);
    procedure doOnRouteStateChanged(const route: ICefMediaRoute; state: TCefMediaRouteConnectionState);
    procedure doOnRouteMessageReceived(const route: ICefMediaRoute; const message_: ustring);
  end;

  // TCefMediaRoute
  // /include/capi/cef_media_router_capi.h (cef_media_observer_t)
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

  // TCefMediaRouteCreateCallback
  // /include/capi/cef_media_router_capi.h (cef_media_route_create_callback_t)
  ICefMediaRouteCreateCallback = interface(ICefBaseRefCounted)
    ['{8848CBFE-36AC-4AC8-BC10-386B69FB27BE}']
    procedure OnMediaRouteCreateFinished(result: TCefMediaRouterCreateResult; const error: ustring; const route: ICefMediaRoute);
  end;

  //
  ICefMediaSinkDeviceInfoCallback = interface(ICefBaseRefCounted)
    ['{633898DD-4169-45D0-ADDD-6E68B3686E0D}']
    procedure OnMediaSinkDeviceInfo(const ip_address: ustring; port: integer; const model_name: ustring);
  end;

  // TCefMediaSink
  // /include/capi/cef_media_router_capi.h (cef_media_sink_t)
  ICefMediaSink = interface(ICefBaseRefCounted)
    ['{EDA1A4B2-2A4C-42DD-A7DF-901BF93D908D}']
    function  GetId: ustring;
    function  GetName: ustring;
    function  GetDescription: ustring;
    function  GetIconType: TCefMediaSinkIconType;
    procedure GetDeviceInfo(const callback: ICefMediaSinkDeviceInfoCallback);
    function  IsCastSink: boolean;
    function  IsDialSink: boolean;
    function  IsCompatibleWith(const source: ICefMediaSource): boolean;

    property ID          : ustring               read GetId;
    property Name        : ustring               read GetName;
    property Description : ustring               read GetDescription;
    property IconType    : TCefMediaSinkIconType read GetIconType;
  end;

  // TCefMediaSource
  // /include/capi/cef_media_router_capi.h (cef_media_source_t)
  ICefMediaSource = interface(ICefBaseRefCounted)
    ['{734ED6E4-6498-43ED-AAA4-6B993EDC30BE}']
    function GetId : ustring;
    function IsCastSource : boolean;
    function IsDialSource : boolean;

    property ID : ustring read GetId;
  end;


  // TCefResourceBundleHandler
  // /include/capi/cef_resource_bundle_handler_capi.h (cef_resource_bundle_handler_t)
  ICefResourceBundleHandler = interface(ICefBaseRefCounted)
    ['{09C264FD-7E03-41E3-87B3-4234E82B5EA2}']
    function GetLocalizedString(stringId: Integer; var stringVal: ustring): Boolean;
    function GetDataResource(resourceId: Integer; var data: Pointer; var dataSize: NativeUInt): Boolean;
    function GetDataResourceForScale(resourceId: Integer; scaleFactor: TCefScaleFactor; var data: Pointer; var dataSize: NativeUInt): Boolean;

    procedure RemoveReferences; // custom procedure to clear all references
  end;

  // TCefBrowserProcessHandler
  // /include/capi/cef_browser_process_handler_capi.h (cef_browser_process_handler_t)
  ICefBrowserProcessHandler = interface(ICefBaseRefCounted)
    ['{27291B7A-C0AE-4EE0-9115-15C810E22F6C}']
    procedure GetCookieableSchemes(var schemes: TStringList; var include_defaults : boolean);
    procedure OnContextInitialized;
    procedure OnBeforeChildProcessLaunch(const commandLine: ICefCommandLine);
    procedure GetPrintHandler(var aHandler : ICefPrintHandler);
    procedure OnScheduleMessagePumpWork(const delayMs: Int64);
    procedure GetDefaultClient(var aClient : ICefClient);

    procedure RemoveReferences; // custom procedure to clear all references
  end;

  // TCefRenderProcessHandler
  // /include/capi/cef_render_process_handler_capi.h (cef_render_process_handler_t)
  ICefRenderProcessHandler = interface(ICefBaseRefCounted)
    ['{FADEE3BC-BF66-430A-BA5D-1EE3782ECC58}']
    procedure OnWebKitInitialized;
    procedure OnBrowserCreated(const browser: ICefBrowser; const extra_info: ICefDictionaryValue);
    procedure OnBrowserDestroyed(const browser: ICefBrowser);
    function  GetLoadHandler : ICefLoadHandler;
    procedure OnContextCreated(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context);
    procedure OnContextReleased(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context);
    procedure OnUncaughtException(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context; const V8Exception: ICefV8Exception; const stackTrace: ICefV8StackTrace);
    procedure OnFocusedNodeChanged(const browser: ICefBrowser; const frame: ICefFrame; const node: ICefDomNode);
    function  OnProcessMessageReceived(const browser: ICefBrowser; const frame: ICefFrame; sourceProcess: TCefProcessId; const aMessage: ICefProcessMessage): Boolean;

    procedure RemoveReferences; // custom procedure to clear all references
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

    procedure RemoveReferences; // custom procedure to clear all references
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
    procedure SetSupportedSchemes(const schemes: TStrings; include_defaults: boolean; const callback: ICefCompletionCallback);
    procedure SetSupportedSchemesProc(const schemes: TStrings; include_defaults: boolean; const callback: TCefCompletionCallbackProc);
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

  // TCefResourceSkipCallback
  // /include/capi/cef_resource_handler_capi.h (cef_resource_skip_callback_t)
  ICefResourceSkipCallback = interface(ICefBaseRefCounted)
    ['{5ADDE93E-5858-41FD-81E8-ED8BF710D92A}']
    procedure Cont(bytes_skipped: int64);
  end;

  // TCefResourceReadCallback
  // /include/capi/cef_resource_handler_capi.h (cef_resource_read_callback_t)
  ICefResourceReadCallback = interface(ICefBaseRefCounted)
    ['{7669335F-7A4B-4657-86CA-C02B12369602}']
    procedure Cont(bytes_read: int64);
  end;

  // TCefResourceHandler
  // /include/capi/cef_resource_handler_capi.h (cef_resource_handler_t)
  ICefResourceHandler = interface(ICefBaseRefCounted)
    ['{BD3EA208-AAAD-488C-BFF2-76993022F2B5}']
    function  open(const request: ICefRequest; var handle_request: boolean; const callback: ICefCallback): boolean;
    function  ProcessRequest(const request: ICefRequest; const callback: ICefCallback): boolean; // deprecated
    procedure GetResponseHeaders(const response: ICefResponse; out responseLength: Int64; out redirectUrl: ustring);
    function  skip(bytes_to_skip: int64; var bytes_skipped: Int64; const callback: ICefResourceSkipCallback): boolean;
    function  read(const data_out: Pointer; bytes_to_read: Integer; var bytes_read: Integer; const callback: ICefResourceReadCallback): boolean;
    function  ReadResponse(const dataOut: Pointer; bytesToRead: Integer; var bytesRead: Integer; const callback: ICefCallback): boolean; // deprecated
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
    function SetBool(value: boolean): Boolean;
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
    function  OnBeforePopup(const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean): Boolean;
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
    procedure GetActiveBrowser(const extension: ICefExtension; const browser: ICefBrowser; include_incognito: boolean; var aRsltBrowser: ICefBrowser);
    function  CanAccessBrowser(const extension: ICefExtension; const browser: ICefBrowser; include_incognito: boolean; const target_browser: ICefBrowser): boolean;
    function  GetExtensionResource(const extension: ICefExtension; const browser: ICefBrowser; const file_: ustring; const callback: ICefGetExtensionResourceCallback): boolean;

    procedure RemoveReferences; // custom procedure to clear all references
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
  // /include/capi/cef_request_callback_capi.h (cef_request_callback_t)
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
    procedure GetResourceRequestHandler(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; is_navigation, is_download: boolean; const request_initiator: ustring; var disable_default_handling: boolean; var aResourceRequestHandler : ICefResourceRequestHandler);
    function  GetAuthCredentials(const browser: ICefBrowser; const originUrl: ustring; isProxy: Boolean; const host: ustring; port: Integer; const realm, scheme: ustring; const callback: ICefAuthCallback): Boolean;
    function  OnQuotaRequest(const browser: ICefBrowser; const originUrl: ustring; newSize: Int64; const callback: ICefRequestCallback): Boolean;
    function  OnCertificateError(const browser: ICefBrowser; certError: TCefErrorcode; const requestUrl: ustring; const sslInfo: ICefSslInfo; const callback: ICefRequestCallback): Boolean;
    function  OnSelectClientCertificate(const browser: ICefBrowser; isProxy: boolean; const host: ustring; port: integer; certificatesCount: NativeUInt; const certificates: TCefX509CertificateArray; const callback: ICefSelectClientCertificateCallback): boolean;
    procedure OnPluginCrashed(const browser: ICefBrowser; const pluginPath: ustring);
    procedure OnRenderViewReady(const browser: ICefBrowser);
    procedure OnRenderProcessTerminated(const browser: ICefBrowser; status: TCefTerminationStatus);
    procedure OnDocumentAvailableInMainFrame(const browser: ICefBrowser);

    procedure RemoveReferences; // custom procedure to clear all references
  end;

  // TCefResourceRequestHandler
  // /include/capi/cef_resource_request_handler_capi.h (cef_resource_request_handler_t)
  ICefResourceRequestHandler = interface(ICefBaseRefCounted)
    ['{CFA42A38-EA91-4A95-95CE-178BCD412411}']
    procedure GetCookieAccessFilter(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; var aFilter: ICefCookieAccessFilter);
    function  OnBeforeResourceLoad(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const callback: ICefRequestCallback): TCefReturnValue;
    procedure GetResourceHandler(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; var aResourceHandler : ICefResourceHandler);
    procedure OnResourceRedirect(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; var newUrl: ustring);
    function  OnResourceResponse(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse): Boolean;
    procedure GetResourceResponseFilter(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; var aResourceFilter: ICefResponseFilter);
    procedure OnResourceLoadComplete(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; status: TCefUrlRequestStatus; receivedContentLength: Int64);
    procedure OnProtocolExecution(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; var allowOsExecution: Boolean);

    procedure RemoveReferences; // custom procedure to clear all references
  end;

  // TCefCookieAccessFilter
  // /include/capi/cef_resource_request_handler_capi.h (cef_cookie_access_filter_t)
  ICefCookieAccessFilter = interface(ICefBaseRefCounted)
    ['{65ECD862-F55F-46E4-8AC3-2AE90DCC86F5}']
    function CanSendCookie(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const cookie: PCefCookie): boolean;
    function CanSaveCookie(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; const cookie: PCefCookie): boolean;

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
    procedure OnCursorChange(const browser: ICefBrowser; cursor: TCefCursorHandle; CursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo; var aResult : boolean);

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

  // TCefAudioHandler
  // /include/capi/cef_audio_handler_capi.h (cef_audio_handler_t)
  ICefAudioHandler = interface(ICefBaseRefCounted)
    ['{8963271A-0B94-4279-82C8-FB2EA7B3CDEC}']
    procedure OnGetAudioParameters(const browser: ICefBrowser; var params: TCefAudioParameters; var aResult: boolean);
    procedure OnAudioStreamStarted(const browser: ICefBrowser; const params: TCefAudioParameters; channels: integer);
    procedure OnAudioStreamPacket(const browser: ICefBrowser; const data : PPSingle; frames: integer; pts: int64);
    procedure OnAudioStreamStopped(const browser: ICefBrowser);
    procedure OnAudioStreamError(const browser: ICefBrowser; const message_: ustring);

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
    procedure GetViewRect(const browser: ICefBrowser; var rect: TCefRect);
    function  GetScreenPoint(const browser: ICefBrowser; viewX, viewY: Integer; var screenX, screenY: Integer): Boolean;
    function  GetScreenInfo(const browser: ICefBrowser; var screenInfo: TCefScreenInfo): Boolean;
    procedure OnPopupShow(const browser: ICefBrowser; show: Boolean);
    procedure OnPopupSize(const browser: ICefBrowser; const rect: PCefRect);
    procedure OnPaint(const browser: ICefBrowser; kind: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; const buffer: Pointer; width, height: Integer);
    procedure OnAcceleratedPaint(const browser: ICefBrowser; kind: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; shared_handle: Pointer);
    function  OnStartDragging(const browser: ICefBrowser; const dragData: ICefDragData; allowedOps: TCefDragOperations; x, y: Integer): Boolean;
    procedure OnUpdateDragCursor(const browser: ICefBrowser; operation: TCefDragOperation);
    procedure OnScrollOffsetChanged(const browser: ICefBrowser; x, y: Double);
    procedure OnIMECompositionRangeChanged(const browser: ICefBrowser; const selected_range: PCefRange; character_boundsCount: NativeUInt; const character_bounds: PCefRect);
    procedure OnTextSelectionChanged(const browser: ICefBrowser; const selected_text: ustring; const selected_range: PCefRange);
    procedure OnVirtualKeyboardRequested(const browser: ICefBrowser; input_mode: TCefTextInpuMode);

    procedure RemoveReferences; // custom procedure to clear all references
  end;

  // TCefClient
  // /include/capi/cef_client_capi.h (cef_client_t)
  ICefClient = interface(ICefBaseRefCounted)
    ['{1D502075-2FF0-4E13-A112-9E541CD811F4}']
    procedure GetAudioHandler(var aHandler : ICefAudioHandler);
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
    function  OnProcessMessageReceived(const browser: ICefBrowser; const frame: ICefFrame; sourceProcess: TCefProcessId; const message_ : ICefProcessMessage): Boolean;

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

    procedure RemoveReferences; // custom procedure to clear all references
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
    procedure OnDraggableRegionsChanged(const browser: ICefBrowser; const frame: ICefFrame; regionsCount: NativeUInt; const regions: PCefDraggableRegionArray);

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
    function  OnBeforePluginLoad(const mimeType, pluginUrl:ustring; isMainFrame : boolean; const topOriginUrl: ustring; const pluginInfo: ICefWebPluginInfo; var pluginPolicy: TCefPluginPolicy): Boolean;
    procedure GetResourceRequestHandler(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; is_navigation, is_download: boolean; const request_initiator: ustring; var disable_default_handling: boolean; var aResourceRequestHandler : ICefResourceRequestHandler);

    procedure RemoveReferences; // custom procedure to clear all references
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
    function  GetCookieManager(const callback: ICefCompletionCallback): ICefCookieManager;
    function  GetCookieManagerProc(const callback: TCefCompletionCallbackProc): ICefCookieManager;
    function  RegisterSchemeHandlerFactory(const schemeName, domainName: ustring; const factory: ICefSchemeHandlerFactory): Boolean;
    function  ClearSchemeHandlerFactories: Boolean;
    procedure PurgePluginListCache(reloadPages: Boolean);
    function  HasPreference(const name: ustring): Boolean;
    function  GetPreference(const name: ustring): ICefValue;
    function  GetAllPreferences(includeDefaults: Boolean): ICefDictionaryValue;
    function  CanSetPreference(const name: ustring): Boolean;
    function  SetPreference(const name: ustring; const value: ICefValue; out error: ustring): Boolean;
    procedure ClearCertificateExceptions(const callback: ICefCompletionCallback);
    procedure ClearHttpAuthCredentials(const callback: ICefCompletionCallback);
    procedure CloseAllConnections(const callback: ICefCompletionCallback);
    procedure ResolveHost(const origin: ustring; const callback: ICefResolveCallback);
    procedure LoadExtension(const root_directory: ustring; const manifest: ICefDictionaryValue; const handler: ICefExtensionHandler);
    function  DidLoadExtension(const extension_id: ustring): boolean;
    function  HasExtension(const extension_id: ustring): boolean;
    function  GetExtensions(const extension_ids: TStringList): boolean;
    function  GetExtension(const extension_id: ustring): ICefExtension;
    function  GetMediaRouter: ICefMediaRouter;

    property  CachePath        : ustring         read GetCachePath;
    property  IsGlobalContext  : boolean         read IsGlobal;
    property  MediaRouter      : ICefMediaRouter read GetMediaRouter;
  end;

  // TCefPrintSettings
  // /include/capi/cef_print_settings_capi.h (cef_print_settings_t)
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
    procedure OnPrintDialog(const browser: ICefBrowser; hasSelection: boolean; const callback: ICefPrintDialogCallback; var aResult: boolean);
    procedure OnPrintJob(const browser: ICefBrowser; const documentName, PDFFilePath: ustring; const callback: ICefPrintJobCallback; var aResult: boolean);
    procedure OnPrintReset(const browser: ICefBrowser);
    procedure GetPDFPaperSize(deviceUnitsPerInch: integer; var aResult: TCefSize);

    procedure RemoveReferences; // custom procedure to clear all references
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
    function GetDataResource(resourceId: Integer): ICefBinaryValue;
    function GetDataResourceForScale(resourceId: Integer; scaleFactor: TCefScaleFactor): ICefBinaryValue;
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
    function GetRepresentationInfo(scaleFactor: Single; var actualScaleFactor: Single; var pixelWidth, pixelHeight: Integer): Boolean;
    function GetAsBitmap(scaleFactor: Single; colorType: TCefColorType; alphaType: TCefAlphaType; var pixelWidth, pixelHeight: Integer): ICefBinaryValue;
    function GetAsPng(scaleFactor: Single; withTransparency: Boolean; var pixelWidth, pixelHeight: Integer): ICefBinaryValue;
    function GetAsJpeg(scaleFactor: Single; quality: Integer; var pixelWidth, pixelHeight: Integer): ICefBinaryValue;

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
    function  FormatLabel(const menuModel: ICefMenuModel; var label_ : ustring) : boolean;
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




  // *********************************
  // ************* Views *************
  // *********************************


  // TCefDisplay
  // /include/capi/views/cef_display_capi.h (cef_display_t)
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

  // TCefLayout
  // /include/capi/views/cef_layout_capi.h (cef_layout_t)
  ICefLayout = interface(ICefBaseRefCounted)
    ['{0EC7AE4B-1672-4D0B-B617-0BDA72F3C7F4}']
    function AsBoxLayout : ICefBoxLayout;
    function AsFillLayout : ICefFillLayout;
    function IsValid : boolean;

    property Valid              : boolean    read IsValid;
  end;

  // TCefBoxLayout
  // /include/capi/views/cef_box_layout_capi.h (cef_box_layout_t)
  ICefBoxLayout = interface(ICefLayout)
    ['{E59FCCAE-A371-4C21-98D3-93D3217016AE}']
    procedure SetFlexForView(const view: ICefView; flex: Integer);
    procedure ClearFlexForView(const view: ICefView);
  end;

  // TCefFillLayout
  // /include/capi/views/cef_fill_layout_capi.h (cef_fill_layout_t)
  ICefFillLayout = interface(ICefLayout)
    ['{3DB214F2-7F27-4306-82C9-8166160422B1}']
  end;

  // TCefView
  // /include/capi/views/cef_view_capi.h (cef_view_t)
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
    property TypeString             : ustring          read GetTypeString;
  end;

  // TCefViewDelegate
  // /include/capi/views/cef_view_delegate_capi.h (cef_view_delegate_t)
  ICefViewDelegate = interface(ICefBaseRefCounted)
    ['{5F900206-B969-4E51-B56C-0FF38D749C72}']
    procedure OnGetPreferredSize(const view: ICefView; var aResult : TCefSize);
    procedure OnGetMinimumSize(const view: ICefView; var aResult : TCefSize);
    procedure OnGetMaximumSize(const view: ICefView; var aResult : TCefSize);
    procedure OnGetHeightForWidth(const view: ICefView; width: Integer; var aResult: Integer);
    procedure OnParentViewChanged(const view: ICefView; added: boolean; const parent: ICefView);
    procedure OnChildViewChanged(const view: ICefView; added: boolean; const child: ICefView);
    procedure OnFocus(const view: ICefView);
    procedure OnBlur(const view: ICefView);
  end;

  ICefViewDelegateEvents = interface
    ['{74DDDB37-8F08-4672-BDB6-55CA2CD374ED}']
    // ICefViewDelegate
    procedure doOnGetPreferredSize(const view: ICefView; var aResult : TCefSize);
    procedure doOnGetMinimumSize(const view: ICefView; var aResult : TCefSize);
    procedure doOnGetMaximumSize(const view: ICefView; var aResult : TCefSize);
    procedure doOnGetHeightForWidth(const view: ICefView; width: Integer; var aResult: Integer);
    procedure doOnParentViewChanged(const view: ICefView; added: boolean; const parent: ICefView);
    procedure doOnChildViewChanged(const view: ICefView; added: boolean; const child: ICefView);
    procedure doOnFocus(const view: ICefView);
    procedure doOnBlur(const view: ICefView);

    // Custom
    procedure doCreateCustomView;
  end;

  // TCefTextfield
  // /include/capi/views/cef_textfield_capi.h (cef_textfield_t)
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

  // TCefTextfieldDelegate
  // /include/capi/views/cef_textfield_delegate_capi.h (cef_textfield_delegate_t)
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

  // TCefScrollView
  // /include/capi/views/cef_scroll_view_capi.h (cef_scroll_view_t)
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

  // TCefPanel
  // /include/capi/views/cef_panel_capi.h (cef_panel_t)
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

  // TCefPanelDelegate
  // /include/capi/views/cef_panel_delegate_capi.h (cef_panel_delegate_t)
  ICefPanelDelegate = interface(ICefViewDelegate)
    ['{305D453F-FEBA-48ED-AE33-5D978823EA96}']
  end;

  ICefPanelDelegateEvents = interface(ICefViewDelegateEvents)
    ['{F1F2963F-82C3-48F0-9B9C-7C213BACB96B}']
  end;

  // TCefBrowserView
  // /include/capi/views/cef_browser_view_capi.h (cef_browser_view_t)
  ICefBrowserView = interface(ICefView)
    ['{A617EE5D-B933-4E14-9FC0-7E88E9B6C051}']
    function  GetBrowser : ICefBrowser;
    procedure SetPreferAccelerators(prefer_accelerators: boolean);
  end;

  // TCefBrowserViewDelegate
  // /include/capi/views/cef_browser_view_delegate_capi.h (cef_browser_view_delegate_t)
  ICefBrowserViewDelegate = interface(ICefViewDelegate)
    ['{578A0DD4-2E7D-4061-B4DB-7C3CDC7A90C0}']
    procedure OnBrowserCreated(const browser_view: ICefBrowserView; const browser: ICefBrowser);
    procedure OnBrowserDestroyed(const browser_view: ICefBrowserView; const browser: ICefBrowser);
    procedure OnGetDelegateForPopupBrowserView(const browser_view: ICefBrowserView; const settings: TCefBrowserSettings; const client: ICefClient; is_devtools: boolean; var aResult : ICefBrowserViewDelegate);
    procedure OnPopupBrowserViewCreated(const browser_view, popup_browser_view: ICefBrowserView; is_devtools: boolean; var aResult : boolean);
  end;

  ICefBrowserViewDelegateEvents = interface(ICefViewDelegateEvents)
    ['{AB94B875-63C6-4FEF-BB30-0816402ABA1C}']
    procedure doOnBrowserCreated(const browser_view: ICefBrowserView; const browser: ICefBrowser);
    procedure doOnBrowserDestroyed(const browser_view: ICefBrowserView; const browser: ICefBrowser);
    procedure doOnGetDelegateForPopupBrowserView(const browser_view: ICefBrowserView; const settings: TCefBrowserSettings; const client: ICefClient; is_devtools: boolean; var aResult : ICefBrowserViewDelegate);
    procedure doOnPopupBrowserViewCreated(const browser_view, popup_browser_view: ICefBrowserView; is_devtools: boolean; var aResult : boolean);
  end;

  // TCefButton
  // /include/capi/views/cef_button_capi.h (cef_button_t)
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

  // TCefButtonDelegate
  // /include/capi/views/cef_button_delegate_capi.h (cef_button_delegate_t)
  ICefButtonDelegate = interface(ICefViewDelegate)
    ['{EA1EB5A4-DFB0-4A13-A23B-54FAF9401B39}']
    procedure OnButtonPressed(const button: ICefButton);
    procedure OnButtonStateChanged(const button: ICefButton);
  end;

  ICefButtonDelegateEvents = interface(ICefViewDelegateEvents)
    ['{E8DF70BE-5DEB-42CF-AF86-B0FF1040498E}']
    procedure doOnButtonPressed(const button: ICefButton);
    procedure doOnButtonStateChanged(const button: ICefButton);
  end;

  // TCefLabelButton
  // /include/capi/views/cef_label_button_capi.h (cef_label_button_t)
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

  // TCefMenuButton
  // /include/capi/views/cef_menu_button_capi.h (cef_menu_button_t)
  ICefMenuButton = interface(ICefLabelButton)
    ['{62BFE81A-7810-400B-83C6-76D1DF133710}']
    procedure ShowMenu(const menu_model: ICefMenuModel; const screen_point: TCefPoint; anchor_position: TCefMenuAnchorPosition);
    procedure TriggerMenu;
  end;

  // TCefMenuButtonPressedLock
  // /include/capi/views/cef_menu_button_delegate_capi.h (cef_menu_button_pressed_lock_t)
  ICefMenuButtonPressedLock = interface(ICefBaseRefCounted)
    ['{71498C53-0B1D-4A05-98A0-3E589F2A1683}']
  end;

  // TCefMenuButtonDelegate
  // /include/capi/views/cef_menu_button_delegate_capi.h (cef_menu_button_delegate_t)
  ICefMenuButtonDelegate = interface(ICefButtonDelegate)
    ['{D0E89A75-463A-4766-8701-BD8D24B11E9F}']
    procedure OnMenuButtonPressed(const menu_button: ICefMenuButton; const screen_point: TCefPoint; const button_pressed_lock: ICefMenuButtonPressedLock);
  end;

  ICefMenuButtonDelegateEvents = interface(ICefButtonDelegateEvents)
    ['{DA36DD60-7609-4576-BB8E-6A55FD48C680}']
    procedure doOnMenuButtonPressed(const menu_button: ICefMenuButton; const screen_point: TCefPoint; const button_pressed_lock: ICefMenuButtonPressedLock);
  end;

  // TCefWindow
  // /include/capi/views/cef_window_capi.h (cef_window_t)
  ICefWindow = interface(ICefPanel)
    ['{C450C974-BF0A-4968-A6BE-153CEAD10DA6}']
    procedure Show;
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

  // TCefWindowDelegate
  // /include/capi/views/cef_window_delegate_capi.h (cef_window_delegate_t)
  ICefWindowDelegate = interface(ICefPanelDelegate)
    ['{52D4EE2C-303B-42B6-A35F-30D03834A23F}']
    procedure OnWindowCreated(const window: ICefWindow);
    procedure OnWindowDestroyed(const window: ICefWindow);
    procedure OnGetParentWindow(const window: ICefWindow; var is_menu, can_activate_menu: boolean; var aResult : ICefWindow);
    procedure OnGetInitialBounds(const window: ICefWindow; var aResult : TCefRect);
    procedure OnIsFrameless(const window: ICefWindow; var aResult : boolean);
    procedure OnCanResize(const window: ICefWindow; var aResult : boolean);
    procedure OnCanMaximize(const window: ICefWindow; var aResult : boolean);
    procedure OnCanMinimize(const window: ICefWindow; var aResult : boolean);
    procedure OnCanClose(const window: ICefWindow; var aResult : boolean);
    procedure OnAccelerator(const window: ICefWindow; command_id: Integer; var aResult : boolean);
    procedure OnKeyEvent(const window: ICefWindow; const event: TCefKeyEvent; var aResult : boolean);
  end;

  ICefWindowDelegateEvents = interface(ICefPanelDelegateEvents)
    ['{05C19A41-E75D-459E-AD4D-C8A0CA4A49D3}']
    procedure doOnWindowCreated(const window: ICefWindow);
    procedure doOnWindowDestroyed(const window: ICefWindow);
    procedure doOnGetParentWindow(const window: ICefWindow; var is_menu, can_activate_menu: boolean; var aResult : ICefWindow);
    procedure doOnGetInitialBounds(const window: ICefWindow; var aResult : TCefRect);
    procedure doOnIsFrameless(const window: ICefWindow; var aResult : boolean);
    procedure doOnCanResize(const window: ICefWindow; var aResult : boolean);
    procedure doOnCanMaximize(const window: ICefWindow; var aResult : boolean);
    procedure doOnCanMinimize(const window: ICefWindow; var aResult : boolean);
    procedure doOnCanClose(const window: ICefWindow; var aResult : boolean);
    procedure doOnAccelerator(const window: ICefWindow; command_id: Integer; var aResult : boolean);
    procedure doOnKeyEvent(const window: ICefWindow; const event: TCefKeyEvent; var aResult : boolean);
  end;

implementation

end.
