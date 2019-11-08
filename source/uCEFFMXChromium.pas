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
//        Copyright © 2019 Salvador Diaz Fau. All rights reserved.
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

unit uCEFFMXChromium;

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  System.Classes, System.Types,
  {$IFDEF MSWINDOWS}
  WinApi.Windows, WinApi.Messages,
  {$ENDIF}
  FMX.Types, FMX.Platform, FMX.Forms,
  uCEFTypes, uCEFInterfaces, uCEFLibFunctions, uCEFMiscFunctions, uCEFClient,
  uCEFConstants, uCEFTask, uCEFChromiumEvents, uCEFChromiumOptions, uCEFChromiumFontOptions,
  uCEFPDFPrintOptions;

type
  {$IFNDEF FPC}{$IFDEF DELPHI16_UP}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}{$ENDIF}
  TFMXChromium = class(TComponent, IChromiumEvents)
    protected
      FHandler                : ICefClient;
      FBrowser                : ICefBrowser;
      FBrowserId              : Integer;
      FReqContextHandler      : ICefRequestContextHandler;
      FResourceRequestHandler : ICefResourceRequestHandler;
      FDefaultUrl             : ustring;
      FOptions                : TChromiumOptions;
      FFontOptions            : TChromiumFontOptions;
      FPDFPrintOptions        : TPDFPrintOptions;
      FDefaultEncoding        : ustring;
      FProxyType              : integer;
      FProxyScheme            : TCefProxyScheme;
      FProxyServer            : ustring;
      FProxyPort              : integer;
      FProxyUsername          : ustring;
      FProxyPassword          : ustring;
      FProxyScriptURL         : ustring;
      FProxyByPassList        : ustring;
      FMaxConnectionsPerProxy : integer;
      FUpdatePreferences      : boolean;
      FCustomHeaderName       : ustring;
      FCustomHeaderValue      : ustring;
      FAddCustomHeader        : boolean;
      FDoNotTrack             : boolean;
      FSendReferrer           : boolean;
      FHyperlinkAuditing      : boolean;
      FRunAllFlashInAllowMode : boolean;
      FAllowOutdatedPlugins   : boolean;
      FAlwaysAuthorizePlugins : boolean;
      FSpellChecking          : boolean;
      FSpellCheckerDicts      : ustring;
      FZoomStep               : byte;
      FPrefsFileName          : string;
      FIsOSR                  : boolean;
      FInitialized            : boolean;
      FClosing                : boolean;
      FSafeSearch             : boolean;
      FYouTubeRestrict        : integer;
      FPrintingEnabled        : boolean;
      FWindowInfo             : TCefWindowInfo;
      FBrowserSettings        : TCefBrowserSettings;
      FDevWindowInfo          : TCefWindowInfo;
      FDevBrowserSettings     : TCefBrowserSettings;
      FDragOperations         : TCefDragOperations;
      FDragAndDropInitialized : boolean;
      FWebRTCIPHandlingPolicy : TCefWebRTCHandlingPolicy;
      FWebRTCMultipleRoutes   : TCefState;
      FWebRTCNonProxiedUDP    : TCefState;

      {$IFDEF MSWINDOWS}
      FOldBrowserCompWndPrc   : TFNWndProc;
      FOldWidgetCompWndPrc    : TFNWndProc;
      FOldRenderCompWndPrc    : TFNWndProc;
      FBrowserCompHWND        : THandle;
      FWidgetCompHWND         : THandle;
      FRenderCompHWND         : THandle;
      FBrowserCompStub        : Pointer;
      FWidgetCompStub         : Pointer;
      FRenderCompStub         : Pointer;
      {$ENDIF}

      // ICefClient
      FOnProcessMessageReceived       : TOnProcessMessageReceived;

      // ICefLoadHandler
      FOnLoadStart                    : TOnLoadStart;
      FOnLoadEnd                      : TOnLoadEnd;
      FOnLoadError                    : TOnLoadError;
      FOnLoadingStateChange           : TOnLoadingStateChange;

      // ICefFocusHandler
      FOnTakeFocus                    : TOnTakeFocus;
      FOnSetFocus                     : TOnSetFocus;
      FOnGotFocus                     : TOnGotFocus;

      // ICefContextMenuHandler
      FOnBeforeContextMenu            : TOnBeforeContextMenu;
      FOnRunContextMenu               : TOnRunContextMenu;
      FOnContextMenuCommand           : TOnContextMenuCommand;
      FOnContextMenuDismissed         : TOnContextMenuDismissed;

      // ICefKeyboardHandler
      FOnPreKeyEvent                  : TOnPreKeyEvent;
      FOnKeyEvent                     : TOnKeyEvent;

      // ICefDisplayHandler
      FOnAddressChange                : TOnAddressChange;
      FOnTitleChange                  : TOnTitleChange;
      FOnFavIconUrlChange             : TOnFavIconUrlChange;
      FOnFullScreenModeChange         : TOnFullScreenModeChange;
      FOnTooltip                      : TOnTooltip;
      FOnStatusMessage                : TOnStatusMessage;
      FOnConsoleMessage               : TOnConsoleMessage;
      FOnAutoResize                   : TOnAutoResize;
      FOnLoadingProgressChange        : TOnLoadingProgressChange;

      // ICefDownloadHandler
      FOnBeforeDownload               : TOnBeforeDownload;
      FOnDownloadUpdated              : TOnDownloadUpdated;

      // ICefJsDialogHandler
      FOnJsdialog                     : TOnJsdialog;
      FOnBeforeUnloadDialog           : TOnBeforeUnloadDialog;
      FOnResetDialogState             : TOnResetDialogState;
      FOnDialogClosed                 : TOnDialogClosed;

      // ICefLifeSpanHandler
      FOnBeforePopup                  : TOnBeforePopup;
      FOnAfterCreated                 : TOnAfterCreated;
      FOnBeforeClose                  : TOnBeforeClose;
      FOnClose                        : TOnClose;

      // ICefRequestHandler
      FOnBeforeBrowse                      : TOnBeforeBrowse;
      FOnOpenUrlFromTab                    : TOnOpenUrlFromTab;
      FOnGetAuthCredentials                : TOnGetAuthCredentials;
      FOnQuotaRequest                      : TOnQuotaRequest;
      FOnCertificateError                  : TOnCertificateError;
      FOnSelectClientCertificate           : TOnSelectClientCertificate;
      FOnPluginCrashed                     : TOnPluginCrashed;
      FOnRenderViewReady                   : TOnRenderViewReady;
      FOnRenderProcessTerminated           : TOnRenderProcessTerminated;
      FOnGetResourceRequestHandler_ReqHdlr : TOnGetResourceRequestHandler;

      // ICefResourceRequestHandler
      FOnBeforeResourceLoad           : TOnBeforeResourceLoad;
      FOnGetResourceHandler           : TOnGetResourceHandler;
      FOnResourceRedirect             : TOnResourceRedirect;
      FOnResourceResponse             : TOnResourceResponse;
      FOnGetResourceResponseFilter    : TOnGetResourceResponseFilter;
      FOnResourceLoadComplete         : TOnResourceLoadComplete;
      FOnProtocolExecution            : TOnProtocolExecution;

      // ICefCookieAccessFilter
      FOnCanSendCookie                : TOnCanSendCookie;
      FOnCanSaveCookie                : TOnCanSaveCookie;

      // ICefDialogHandler
      FOnFileDialog                   : TOnFileDialog;

      // ICefRenderHandler
      FOnGetAccessibilityHandler      : TOnGetAccessibilityHandler;
      FOnGetRootScreenRect            : TOnGetRootScreenRect;
      FOnGetViewRect                  : TOnGetViewRect;
      FOnGetScreenPoint               : TOnGetScreenPoint;
      FOnGetScreenInfo                : TOnGetScreenInfo;
      FOnPopupShow                    : TOnPopupShow;
      FOnPopupSize                    : TOnPopupSize;
      FOnPaint                        : TOnPaint;
      FOnAcceleratedPaint             : TOnAcceleratedPaint;
      FOnCursorChange                 : TOnCursorChange;
      FOnScrollOffsetChanged          : TOnScrollOffsetChanged;
      FOnIMECompositionRangeChanged   : TOnIMECompositionRangeChanged;
      FOnTextSelectionChanged         : TOnTextSelectionChanged;
      FOnVirtualKeyboardRequested     : TOnVirtualKeyboardRequested;

      // ICefDragHandler
      FOnDragEnter                    : TOnDragEnter;
      FOnDraggableRegionsChanged      : TOnDraggableRegionsChanged;

      // ICefFindHandler
      FOnFindResult                   : TOnFindResult;

      // ICefRequestContextHandler
      FOnRequestContextInitialized             : TOnRequestContextInitialized;
      FOnBeforePluginLoad                      : TOnBeforePluginLoad;
      FOnGetResourceRequestHandler_ReqCtxHdlr  : TOnGetResourceRequestHandler;

      // Custom
      FOnTextResultAvailable              : TOnTextResultAvailableEvent;
      FOnPdfPrintFinished                 : TOnPdfPrintFinishedEvent;
      FOnCookiesDeleted                   : TOnCookiesDeletedEvent;
      FOnResolvedHostAvailable            : TOnResolvedIPsAvailableEvent;
      FOnNavigationVisitorResultAvailable : TOnNavigationVisitorResultAvailableEvent;
      FOnDownloadImageFinished            : TOnDownloadImageFinishedEvent;
      FOnCookiesFlushed                   : TNotifyEvent;
      FOnCertificateExceptionsCleared     : TNotifyEvent;
      FOnHttpAuthCredentialsCleared       : TNotifyEvent;
      FOnAllConnectionsClosed             : TNotifyEvent;
      FOnExecuteTaskOnCefThread           : TOnExecuteTaskOnCefThread;
      FOnCookiesVisited                   : TOnCookiesVisited;
      FOnCookieVisitorDestroyed           : TOnCookieVisitorDestroyed;
      FOnCookieSet                        : TOnCookieSet;
      {$IFDEF MSWINDOWS}
      FOnBrowserCompMsg                   : TOnCompMsgEvent;
      FOnWidgetCompMsg                    : TOnCompMsgEvent;
      FOnRenderCompMsg                    : TOnCompMsgEvent;
      {$ENDIF}

      function  GetIsLoading : boolean;
      function  GetMultithreadApp : boolean;
      function  GetHasDocument : boolean;
      function  GetHasView : boolean;
      function  GetHasDevTools : boolean;
      function  GetHasClientHandler : boolean;
      function  GetHasBrowser : boolean;
      function  GetCanGoBack : boolean;
      function  GetCanGoForward : boolean;
      function  GetDocumentURL : ustring;
      function  GetZoomLevel : double;
      function  GetZoomPct : double;
      function  GetIsPopUp : boolean;
      function  GetWindowHandle : THandle;
      function  GetWindowlessFrameRate : integer;
      function  GetFrameIsFocused : boolean;
      function  GetInitialized : boolean;
      function  GetVisibleNavigationEntry : ICefNavigationEntry;
      function  GetHasValidMainFrame : boolean;
      function  GetFrameCount : NativeUInt;
      function  GetRequestContextCache : ustring;
      function  GetRequestContextIsGlobal : boolean;
      function  GetAudioMuted : boolean;

      procedure SetDoNotTrack(aValue : boolean);
      procedure SetSendReferrer(aValue : boolean);
      procedure SetHyperlinkAuditing(aValue : boolean);
      procedure SetRunAllFlashInAllowMode(aValue : boolean);
      procedure SetAllowOutdatedPlugins(aValue : boolean);
      procedure SetAlwaysAuthorizePlugins(aValue : boolean);
      procedure SetSpellChecking(aValue : boolean);
      procedure SetSpellCheckerDicts(const aValue : ustring);
      procedure SetWebRTCIPHandlingPolicy(aValue : TCefWebRTCHandlingPolicy);
      procedure SetWebRTCMultipleRoutes(aValue : TCefState);
      procedure SetWebRTCNonProxiedUDP(aValue : TCefState);
      procedure SetProxyType(aValue : integer);
      procedure SetProxyScheme(aValue : TCefProxyScheme);
      procedure SetProxyServer(const aValue : ustring);
      procedure SetProxyPort(aValue : integer);
      procedure SetProxyUsername(const aValue : ustring);
      procedure SetProxyPassword(const aValue : ustring);
      procedure SetProxyScriptURL(const aValue : ustring);
      procedure SetProxyByPassList(const aValue : ustring);
      procedure SetMaxConnectionsPerProxy(const aValue : integer);
      procedure SetCustomHeaderName(const aValue : ustring);
      procedure SetCustomHeaderValue(const aValue : ustring);
      procedure SetZoomLevel(const aValue : double);
      procedure SetZoomPct(const aValue : double);
      procedure SetZoomStep(aValue : byte);
      procedure SetWindowlessFrameRate(aValue : integer);
      procedure SetAudioMuted(aValue : boolean);
      procedure SetSafeSearch(aValue : boolean);
      procedure SetYouTubeRestrict(aValue : integer);
      procedure SetPrintingEnabled(aValue : boolean);
      procedure SetOnRequestContextInitialized(const aValue : TOnRequestContextInitialized);
      procedure SetOnBeforePluginLoad(const aValue : TOnBeforePluginLoad);

      function  CreateBrowserHost(aWindowInfo : PCefWindowInfo; const aURL : ustring; const aSettings : PCefBrowserSettings; const aExtraInfo : ICefDictionaryValue; const aContext : ICefRequestContext): boolean;
      function  CreateBrowserHostSync(aWindowInfo : PCefWindowInfo; const aURL : ustring; const aSettings : PCefBrowserSettings; const aExtraInfo : ICefDictionaryValue; const aContext : ICefRequestContext): boolean;

      procedure DestroyClientHandler;
      procedure DestroyReqContextHandler;
      procedure DestroyResourceRequestHandler;
      procedure ClearBrowserReference;
      procedure CreateReqContextHandler;
      procedure CreateResourceRequestHandler;

      procedure InitializeEvents;
      procedure InitializeSettings(var aSettings : TCefBrowserSettings);

      procedure GetPrintPDFSettings(var aSettings : TCefPdfPrintSettings; const aTitle, aURL : ustring);

      function  UpdateProxyPrefs(const aBrowser: ICefBrowser) : boolean;
      function  UpdatePreference(const aBrowser: ICefBrowser; const aName : ustring; aValue : boolean) : boolean; overload;
      function  UpdatePreference(const aBrowser: ICefBrowser; const aName : ustring; aValue : integer) : boolean; overload;
      function  UpdatePreference(const aBrowser: ICefBrowser; const aName : ustring; const aValue : double) : boolean; overload;
      function  UpdatePreference(const aBrowser: ICefBrowser; const aName, aValue : ustring) : boolean; overload;
      function  UpdatePreference(const aBrowser: ICefBrowser; const aName : ustring; const aValue : TStringList) : boolean; overload;
      function  UpdateStringListPref(const aBrowser: ICefBrowser; const aName, aValue : ustring) : boolean;

      procedure HandleDictionary(const aDict : ICefDictionaryValue; var aResultSL : TStringList; const aRoot : string);
      procedure HandleNull(const aValue : ICefValue; var aResultSL : TStringList; const aRoot, aKey : string);
      procedure HandleBool(const aValue : ICefValue; var aResultSL : TStringList; const aRoot, aKey : string);
      procedure HandleInteger(const aValue : ICefValue; var aResultSL : TStringList; const aRoot, aKey : string);
      procedure HandleDouble(const aValue : ICefValue; var aResultSL : TStringList; const aRoot, aKey : string);
      procedure HandleString(const aValue : ICefValue; var aResultSL : TStringList; const aRoot, aKey : string);
      procedure HandleBinary(const aValue : ICefValue; var aResultSL : TStringList; const aRoot, aKey : string);
      procedure HandleList(const aValue : ICefValue; var aResultSL : TStringList; const aRoot, aKey : string);
      procedure HandleInvalid(const aValue : ICefValue; var aResultSL : TStringList; const aRoot, aKey : string);

      procedure ApplyZoomStep;
      function  GetParentForm : TCustomForm;

      {$IFDEF MSWINDOWS}
      procedure InitializeWindowInfo(aParentHandle : HWND; aParentRect : TRect; const aWindowName : ustring); virtual;
      procedure FreeAndNilStub(var aStub : pointer);
      procedure CreateStub(const aMethod : TWndMethod; var aStub : Pointer);
      procedure RestoreCompWndProc(var aOldWnd: THandle; aNewWnd: THandle; var aProc: TFNWndProc);
      procedure BrowserCompWndProc(var aMessage: TMessage);
      procedure WidgetCompWndProc(var aMessage: TMessage);
      procedure RenderCompWndProc(var aMessage: TMessage);
      {$ENDIF}

      // IChromiumEvents
      procedure GetSettings(var aSettings : TCefBrowserSettings);

      // ICefClient
      function  doOnProcessMessageReceived(const browser: ICefBrowser; const frame: ICefFrame; sourceProcess: TCefProcessId; const aMessage: ICefProcessMessage): Boolean; virtual;

      // ICefLoadHandler
      procedure doOnLoadStart(const browser: ICefBrowser; const frame: ICefFrame; transitionType: TCefTransitionType); virtual;
      procedure doOnLoadEnd(const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: Integer); virtual;
      procedure doOnLoadError(const browser: ICefBrowser; const frame: ICefFrame; errorCode: TCefErrorCode; const errorText, failedUrl: ustring); virtual;
      procedure doOnLoadingStateChange(const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean); virtual;

      // ICefFocusHandler
      procedure doOnTakeFocus(const browser: ICefBrowser; next: Boolean); virtual;
      function  doOnSetFocus(const browser: ICefBrowser; source: TCefFocusSource): Boolean; virtual;
      procedure doOnGotFocus(const browser: ICefBrowser); virtual;

      // ICefContextMenuHandler
      procedure doOnBeforeContextMenu(const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; const model: ICefMenuModel); virtual;
      function  doRunContextMenu(const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; const model: ICefMenuModel; const callback: ICefRunContextMenuCallback): Boolean; virtual;
      function  doOnContextMenuCommand(const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; commandId: Integer; eventFlags: TCefEventFlags): Boolean; virtual;
      procedure doOnContextMenuDismissed(const browser: ICefBrowser; const frame: ICefFrame); virtual;

      // ICefKeyboardHandler
      function  doOnPreKeyEvent(const browser: ICefBrowser; const event: PCefKeyEvent; osEvent: TCefEventHandle; out isKeyboardShortcut: Boolean): Boolean; virtual;
      function  doOnKeyEvent(const browser: ICefBrowser; const event: PCefKeyEvent; osEvent: TCefEventHandle): Boolean; virtual;

      // ICefDisplayHandler
      procedure doOnAddressChange(const browser: ICefBrowser; const frame: ICefFrame; const url: ustring); virtual;
      procedure doOnTitleChange(const browser: ICefBrowser; const title: ustring); virtual;
      procedure doOnFaviconUrlChange(const browser: ICefBrowser; const iconUrls: TStrings); virtual;
      procedure doOnFullScreenModeChange(const browser: ICefBrowser; fullscreen: Boolean); virtual;
      function  doOnTooltip(const browser: ICefBrowser; var text: ustring): Boolean; virtual;
      procedure doOnStatusMessage(const browser: ICefBrowser; const value: ustring); virtual;
      function  doOnConsoleMessage(const browser: ICefBrowser; level: TCefLogSeverity; const aMessage, source: ustring; line: Integer): Boolean; virtual;
      function  doOnAutoResize(const browser: ICefBrowser; const new_size: PCefSize): Boolean; virtual;
      procedure doOnLoadingProgressChange(const browser: ICefBrowser; const progress: double); virtual;

      // ICefDownloadHandler
      procedure doOnBeforeDownload(const browser: ICefBrowser; const downloadItem: ICefDownloadItem; const suggestedName: ustring; const callback: ICefBeforeDownloadCallback); virtual;
      procedure doOnDownloadUpdated(const browser: ICefBrowser; const downloadItem: ICefDownloadItem; const callback: ICefDownloadItemCallback); virtual;

      // ICefJsDialogHandler
      function  doOnJsdialog(const browser: ICefBrowser; const originUrl: ustring; dialogType: TCefJsDialogType; const messageText, defaultPromptText: ustring; const callback: ICefJsDialogCallback; out suppressMessage: Boolean): Boolean; virtual;
      function  doOnBeforeUnloadDialog(const browser: ICefBrowser; const messageText: ustring; isReload: Boolean; const callback: ICefJsDialogCallback): Boolean; virtual;
      procedure doOnResetDialogState(const browser: ICefBrowser); virtual;
      procedure doOnDialogClosed(const browser: ICefBrowser); virtual;

      // ICefLifeSpanHandler
      function  doOnBeforePopup(const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean): Boolean; virtual;
      procedure doOnAfterCreated(const browser: ICefBrowser); virtual;
      procedure doOnBeforeClose(const browser: ICefBrowser); virtual;
      function  doOnClose(const browser: ICefBrowser): Boolean; virtual;

      // ICefRequestHandler
      function  doOnBeforeBrowse(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; user_gesture, isRedirect: Boolean): Boolean; virtual;
      function  doOnOpenUrlFromTab(const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean): Boolean; virtual;
      procedure doGetResourceRequestHandler_ReqHdlr(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; is_navigation, is_download: boolean; const request_initiator: ustring; var disable_default_handling: boolean; var aResourceRequestHandler : ICefResourceRequestHandler); virtual;
      function  doOnGetAuthCredentials(const browser: ICefBrowser; const originUrl: ustring; isProxy: Boolean; const host: ustring; port: Integer; const realm, scheme: ustring; const callback: ICefAuthCallback): Boolean; virtual;
      function  doOnQuotaRequest(const browser: ICefBrowser; const originUrl: ustring; newSize: Int64; const callback: ICefRequestCallback): Boolean; virtual;
      function  doOnCertificateError(const browser: ICefBrowser; certError: TCefErrorcode; const requestUrl: ustring; const sslInfo: ICefSslInfo; const callback: ICefRequestCallback): Boolean; virtual;
      function  doOnSelectClientCertificate(const browser: ICefBrowser; isProxy: boolean; const host: ustring; port: integer; certificatesCount: NativeUInt; const certificates: TCefX509CertificateArray; const callback: ICefSelectClientCertificateCallback): boolean; virtual;
      procedure doOnPluginCrashed(const browser: ICefBrowser; const pluginPath: ustring); virtual;
      procedure doOnRenderViewReady(const browser: ICefBrowser); virtual;
      procedure doOnRenderProcessTerminated(const browser: ICefBrowser; status: TCefTerminationStatus); virtual;

      // ICefResourceRequestHandler
      function  doOnBeforeResourceLoad(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const callback: ICefRequestCallback): TCefReturnValue; virtual;
      procedure doOnGetResourceHandler(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; var aResourceHandler : ICefResourceHandler); virtual;
      procedure doOnResourceRedirect(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; var newUrl: ustring); virtual;
      function  doOnResourceResponse(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse): Boolean; virtual;
      procedure doOnGetResourceResponseFilter(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; var aResponseFilter: ICefResponseFilter);
      procedure doOnResourceLoadComplete(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; status: TCefUrlRequestStatus; receivedContentLength: Int64); virtual;
      procedure doOnProtocolExecution(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; var allowOsExecution: Boolean); virtual;

      // ICefCookieAccessFilter
      function  doCanSendCookie(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const cookie: PCefCookie): boolean; virtual;
      function  doCanSaveCookie(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; const cookie: PCefCookie): boolean; virtual;

      // ICefDialogHandler
      function  doOnFileDialog(const browser: ICefBrowser; mode: TCefFileDialogMode; const title, defaultFilePath: ustring; const acceptFilters: TStrings; selectedAcceptFilter: Integer; const callback: ICefFileDialogCallback): Boolean; virtual;

      // ICefRenderHandler
      procedure doOnGetAccessibilityHandler(var aAccessibilityHandler : ICefAccessibilityHandler); virtual;
      function  doOnGetRootScreenRect(const browser: ICefBrowser; var rect: TCefRect): Boolean; virtual;
      procedure doOnGetViewRect(const browser: ICefBrowser; var rect: TCefRect); virtual;
      function  doOnGetScreenPoint(const browser: ICefBrowser; viewX, viewY: Integer; var screenX, screenY: Integer): Boolean; virtual;
      function  doOnGetScreenInfo(const browser: ICefBrowser; var screenInfo: TCefScreenInfo): Boolean; virtual;
      procedure doOnPopupShow(const browser: ICefBrowser; show: Boolean); virtual;
      procedure doOnPopupSize(const browser: ICefBrowser; const rect: PCefRect); virtual;
      procedure doOnPaint(const browser: ICefBrowser; kind: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; const buffer: Pointer; width, height: Integer); virtual;
      procedure doOnAcceleratedPaint(const browser: ICefBrowser; kind: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; shared_handle: Pointer); virtual;
      procedure doOnCursorChange(const browser: ICefBrowser; cursor: TCefCursorHandle; cursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo); virtual;
      function  doOnStartDragging(const browser: ICefBrowser; const dragData: ICefDragData; allowedOps: TCefDragOperations; x, y: Integer): Boolean; virtual;
      procedure doOnUpdateDragCursor(const browser: ICefBrowser; operation: TCefDragOperation); virtual;
      procedure doOnScrollOffsetChanged(const browser: ICefBrowser; x, y: Double); virtual;
      procedure doOnIMECompositionRangeChanged(const browser: ICefBrowser; const selected_range: PCefRange; character_boundsCount: NativeUInt; const character_bounds: PCefRect); virtual;
      procedure doOnTextSelectionChanged(const browser: ICefBrowser; const selected_text: ustring; const selected_range: PCefRange); virtual;
      procedure doOnVirtualKeyboardRequested(const browser: ICefBrowser; input_mode: TCefTextInpuMode); virtual;

      // ICefDragHandler
      function  doOnDragEnter(const browser: ICefBrowser; const dragData: ICefDragData; mask: TCefDragOperations): Boolean; virtual;
      procedure doOnDraggableRegionsChanged(const browser: ICefBrowser; const frame: ICefFrame; regionsCount: NativeUInt; regions: PCefDraggableRegionArray); virtual;

      // ICefFindHandler
      procedure doOnFindResult(const browser: ICefBrowser; identifier, count: Integer; const selectionRect: PCefRect; activeMatchOrdinal: Integer; finalUpdate: Boolean); virtual;

      // ICefRequestContextHandler
      procedure doOnRequestContextInitialized(const request_context: ICefRequestContext); virtual;
      function  doOnBeforePluginLoad(const mimeType, pluginUrl:ustring; isMainFrame : boolean; const topOriginUrl: ustring; const pluginInfo: ICefWebPluginInfo; var pluginPolicy: TCefPluginPolicy): Boolean; virtual;
      procedure doGetResourceRequestHandler_ReqCtxHdlr(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; is_navigation, is_download: boolean; const request_initiator: ustring; var disable_default_handling: boolean; var aResourceRequestHandler : ICefResourceRequestHandler); virtual;

      // Custom
      procedure doCookiesDeleted(numDeleted : integer); virtual;
      procedure doPdfPrintFinished(aResultOK : boolean); virtual;
      procedure doTextResultAvailable(const aText : ustring); virtual;
      procedure doUpdatePreferences(const aBrowser: ICefBrowser); virtual;
      procedure doUpdateOwnPreferences; virtual;
      function  doSavePreferences : boolean; virtual;
      procedure doResolvedHostAvailable(result: TCefErrorCode; const resolvedIps: TStrings); virtual;
      function  doNavigationVisitorResultAvailable(const entry: ICefNavigationEntry; current: Boolean; index, total: Integer) : boolean; virtual;
      procedure doDownloadImageFinished(const imageUrl: ustring; httpStatusCode: Integer; const image: ICefImage); virtual;
      procedure doOnCookiesStoreFlushed; virtual;
      procedure doCertificateExceptionsCleared; virtual;
      procedure doHttpAuthCredentialsCleared; virtual;
      procedure doAllConnectionsClosed; virtual;
      procedure doOnExecuteTaskOnCefThread(aTaskID : cardinal); virtual;
      procedure doOnCookiesVisited(const name_, value, domain, path: ustring; secure, httponly, hasExpires: Boolean; const creation, lastAccess, expires: TDateTime; count, total, aID : Integer; var aDeleteCookie, aResult : Boolean); virtual;
      procedure doOnCookieVisitorDestroyed(aID : integer); virtual;
      procedure doOnCookieSet(aSuccess : boolean; aID : integer); virtual;
      function  MustCreateLoadHandler : boolean; virtual;
      function  MustCreateFocusHandler : boolean; virtual;
      function  MustCreateContextMenuHandler : boolean; virtual;
      function  MustCreateDialogHandler : boolean; virtual;
      function  MustCreateKeyboardHandler : boolean; virtual;
      function  MustCreateDisplayHandler : boolean; virtual;
      function  MustCreateDownloadHandler : boolean; virtual;
      function  MustCreateJsDialogHandler : boolean; virtual;
      function  MustCreateLifeSpanHandler : boolean; virtual;
      function  MustCreateRenderHandler : boolean; virtual;
      function  MustCreateRequestHandler : boolean; virtual;
      function  MustCreateDragHandler : boolean; virtual;
      function  MustCreateFindHandler : boolean; virtual;
      function  MustCreateResourceRequestHandler : boolean; virtual;
      function  MustCreateCookieAccessFilter : boolean; virtual;
      function  MustCreateRequestContextHandler : boolean; virtual;

    public
      constructor Create(AOwner: TComponent); override;
      destructor  Destroy; override;
      procedure   AfterConstruction; override;
      procedure   BeforeDestruction; override;
      function    CreateClientHandler(aIsOSR : boolean = True) : boolean; overload;
      function    CreateClientHandler(var aClient : ICefClient; aIsOSR : boolean = True) : boolean; overload;
      procedure   CloseBrowser(aForceClose : boolean);
      function    ShareRequestContext(var aContext : ICefRequestContext; const aHandler : ICefRequestContextHandler = nil) : boolean;

      function    CreateBrowser(const aWindowName : ustring = ''; const aContext : ICefRequestContext = nil; const aExtraInfo : ICefDictionaryValue = nil) : boolean; overload; virtual;
      {$IFDEF MSWINDOWS}
      function    CreateBrowser(aParentHandle : HWND; aParentRect : TRect; const aWindowName : ustring = ''; const aContext : ICefRequestContext = nil; const aExtraInfo : ICefDictionaryValue = nil) : boolean; overload; virtual;
      {$ENDIF}

      procedure   LoadURL(const aURL : ustring; const aFrameName : ustring = ''); overload;
      procedure   LoadURL(const aURL : ustring; const aFrame : ICefFrame); overload;
      procedure   LoadURL(const aURL : ustring; const aFrameIdentifier : int64); overload;
      procedure   LoadString(const aHTML : ustring; const aFrameName : ustring = ''); overload;
      procedure   LoadString(const aHTML : ustring; const aFrame : ICefFrame); overload;
      procedure   LoadString(const aHTML : ustring; const aFrameIdentifier : int64); overload;
      procedure   LoadResource(const aStream : TCustomMemoryStream; const aMimeType, aCharset : string; const aFrameName : ustring = ''); overload;
      procedure   LoadResource(const aStream : TCustomMemoryStream; const aMimeType, aCharset : string; const aFrame : ICefFrame); overload;
      procedure   LoadResource(const aStream : TCustomMemoryStream; const aMimeType, aCharset : string; const aFrameIdentifier : int64); overload;
      procedure   LoadRequest(const aRequest: ICefRequest);

      procedure   GoBack;
      procedure   GoForward;
      procedure   Reload;
      procedure   ReloadIgnoreCache;
      procedure   StopLoad;
      procedure   StartDownload(const aURL : ustring);
      procedure   DownloadImage(const imageUrl: ustring; isFavicon: Boolean; maxImageSize: cardinal; bypassCache: Boolean);

      procedure   SimulateMouseWheel(aDeltaX, aDeltaY : integer);
      function    ClearCertificateExceptions(aClearImmediately : boolean = True) : boolean;
      function    ClearHttpAuthCredentials(aClearImmediately : boolean = True) : boolean;
      function    CloseAllConnections(aCloseImmediately : boolean = True) : boolean;
      procedure   RetrieveHTML(const aFrameName : ustring = ''); overload;
      procedure   RetrieveHTML(const aFrame : ICefFrame); overload;
      procedure   RetrieveHTML(const aFrameIdentifier : int64); overload;
      procedure   RetrieveText(const aFrameName : ustring = ''); overload;
      procedure   RetrieveText(const aFrame : ICefFrame); overload;
      procedure   RetrieveText(const aFrameIdentifier : int64); overload;
      procedure   GetNavigationEntries(currentOnly: Boolean);
      function    GetFrameNames(var aFrameNames : TStrings) : boolean;
      function    GetFrameIdentifiers(var aFrameCount : NativeUInt; var aFrameIdentifierArray : TCefFrameIdentifierArray) : boolean;
      procedure   ExecuteJavaScript(const aCode, aScriptURL : ustring; const aFrameName : ustring = ''; aStartLine : integer = 0); overload;
      procedure   ExecuteJavaScript(const aCode, aScriptURL : ustring; const aFrame : ICefFrame; aStartLine : integer = 0); overload;
      procedure   ExecuteJavaScript(const aCode, aScriptURL : ustring; const aFrameIdentifier : int64; aStartLine : integer = 0); overload;
      procedure   UpdatePreferences;
      procedure   SavePreferences(const aFileName : string);
      procedure   ResolveHost(const aURL : ustring);
      function    IsSameBrowser(const aBrowser : ICefBrowser) : boolean;
      function    ExecuteTaskOnCefThread(aCefThreadId : TCefThreadId; aTaskID : cardinal; aDelayMs : Int64 = 0) : boolean;

      function    DeleteCookies(const url : ustring = ''; const cookieName : ustring = ''; aDeleteImmediately : boolean = False) : boolean;
      function    VisitAllCookies(aID : integer = 0) : boolean;
      function    VisitURLCookies(const url : ustring; includeHttpOnly : boolean = False; aID : integer = 0) : boolean;
      function    SetCookie(const url: ustring; const name, value, domain, path: ustring; secure, httponly, hasExpires: Boolean; const creation, lastAccess, expires: TDateTime; aSetImmediately : boolean = True; aID : integer = 0): Boolean;
      function    FlushCookieStore(aFlushImmediately : boolean = True) : boolean;

      procedure   ShowDevTools(inspectElementAt: TPoint);
      procedure   CloseDevTools;

      procedure   Find(aIdentifier : integer; const aSearchText : ustring; aForward, aMatchCase, aFindNext : Boolean);
      procedure   StopFinding(aClearSelection : Boolean);

      procedure   Print;
      procedure   PrintToPDF(const aFilePath, aTitle, aURL : ustring);

      procedure   ClipboardCopy;
      procedure   ClipboardPaste;
      procedure   ClipboardCut;
      procedure   ClipboardUndo;
      procedure   ClipboardRedo;
      procedure   ClipboardDel;
      procedure   SelectAll;

      procedure   IncZoomStep;
      procedure   DecZoomStep;
      procedure   ResetZoomStep;

      procedure   MoveFormTo(const x, y: Integer);
      procedure   MoveFormBy(const x, y: Integer);
      procedure   ResizeFormWidthTo(const x : Integer);
      procedure   ResizeFormHeightTo(const y : Integer);
      procedure   SetFormLeftTo(const x : Integer);
      procedure   SetFormTopTo(const y : Integer);

      procedure   WasResized;
      procedure   WasHidden(hidden: Boolean);
      procedure   NotifyScreenInfoChanged;
      procedure   NotifyMoveOrResizeStarted;
      procedure   Invalidate(kind: TCefPaintElementType = PET_VIEW);
      procedure   SendExternalBeginFrame;
      procedure   SendKeyEvent(const event: PCefKeyEvent);
      procedure   SendMouseClickEvent(const event: PCefMouseEvent; kind: TCefMouseButtonType; mouseUp: Boolean; clickCount: Integer);
      procedure   SendMouseMoveEvent(const event: PCefMouseEvent; mouseLeave: Boolean);
      procedure   SendMouseWheelEvent(const event: PCefMouseEvent; deltaX, deltaY: Integer);
      procedure   SendTouchEvent(const event: PCefTouchEvent);
      procedure   SendFocusEvent(setFocus: Boolean);
      procedure   SendCaptureLostEvent;

      procedure   SendProcessMessage(targetProcess: TCefProcessId; const ProcMessage: ICefProcessMessage; const aFrameName : ustring = ''); overload;
      procedure   SendProcessMessage(targetProcess: TCefProcessId; const ProcMessage: ICefProcessMessage; const aFrame : ICefFrame); overload;
      procedure   SendProcessMessage(targetProcess: TCefProcessId; const ProcMessage: ICefProcessMessage; const aFrameIdentifier : int64); overload;

      function    CreateUrlRequest(const request: ICefRequest; const client: ICefUrlrequestClient; const aFrameName : ustring = ''): ICefUrlRequest; overload;
      function    CreateUrlRequest(const request: ICefRequest; const client: ICefUrlrequestClient; const aFrame : ICefFrame): ICefUrlRequest; overload;
      function    CreateUrlRequest(const request: ICefRequest; const client: ICefUrlrequestClient; const aFrameIdentifier : int64): ICefUrlRequest; overload;

      procedure   SetFocus(focus: Boolean);
      procedure   SetAccessibilityState(accessibilityState: TCefState);

      procedure   DragTargetDragEnter(const dragData: ICefDragData; const event: PCefMouseEvent; allowedOps: TCefDragOperations);
      procedure   DragTargetDragOver(const event: PCefMouseEvent; allowedOps: TCefDragOperations);
      procedure   DragTargetDragLeave;
      procedure   DragTargetDrop(event: PCefMouseEvent);
      procedure   DragSourceEndedAt(x, y: Integer; op: TCefDragOperation);
      procedure   DragSourceSystemDragEnded;


      property  DefaultUrl              : ustring                      read FDefaultUrl               write FDefaultUrl;
      property  Options                 : TChromiumOptions             read FOptions                  write FOptions;
      property  FontOptions             : TChromiumFontOptions         read FFontOptions              write FFontOptions;
      property  PDFPrintOptions         : TPDFPrintOptions             read FPDFPrintOptions          write FPDFPrintOptions;
      property  DefaultEncoding         : ustring                      read FDefaultEncoding          write FDefaultEncoding;
      property  BrowserId               : integer                      read FBrowserId;
      property  Browser                 : ICefBrowser                  read FBrowser;
      property  CefClient               : ICefClient                   read FHandler;
      property  ReqContextHandler       : ICefRequestContextHandler    read FReqContextHandler;
      property  ResourceRequestHandler  : ICefResourceRequestHandler   read FResourceRequestHandler;
      property  CefWindowInfo           : TCefWindowInfo               read FWindowInfo;
      property  VisibleNavigationEntry  : ICefNavigationEntry          read GetVisibleNavigationEntry;
      property  MultithreadApp          : boolean                      read GetMultithreadApp;
      property  IsLoading               : boolean                      read GetIsLoading;
      property  HasDocument             : boolean                      read GetHasDocument;
      property  HasView                 : boolean                      read GetHasView;
      property  HasDevTools             : boolean                      read GetHasDevTools;
      property  HasClientHandler        : boolean                      read GetHasClientHandler;
      property  HasBrowser              : boolean                      read GetHasBrowser;
      property  CanGoBack               : boolean                      read GetCanGoBack;
      property  CanGoForward            : boolean                      read GetCanGoForward;
      property  IsPopUp                 : boolean                      read GetIsPopUp;
      property  WindowHandle            : THandle                      read GetWindowHandle;
      property  BrowserHandle           : THandle                      read FBrowserCompHWND;
      property  WidgetHandle            : THandle                      read FWidgetCompHWND;
      property  RenderHandle            : THandle                      read FRenderCompHWND;
      property  FrameIsFocused          : boolean                      read GetFrameIsFocused;
      property  Initialized             : boolean                      read GetInitialized;
      property  RequestContextCache     : ustring                      read GetRequestContextCache;
      property  RequestContextIsGlobal  : boolean                      read GetRequestContextIsGlobal;
      property  DocumentURL             : ustring                      read GetDocumentURL;
      property  ZoomLevel               : double                       read GetZoomLevel              write SetZoomLevel;
      property  ZoomPct                 : double                       read GetZoomPct                write SetZoomPct;
      property  ZoomStep                : byte                         read FZoomStep                 write SetZoomStep;
      property  WindowlessFrameRate     : integer                      read GetWindowlessFrameRate    write SetWindowlessFrameRate;
      property  CustomHeaderName        : ustring                      read FCustomHeaderName         write SetCustomHeaderName;
      property  CustomHeaderValue       : ustring                      read FCustomHeaderValue        write SetCustomHeaderValue;
      property  DoNotTrack              : boolean                      read FDoNotTrack               write SetDoNotTrack;
      property  SendReferrer            : boolean                      read FSendReferrer             write SetSendReferrer;
      property  HyperlinkAuditing       : boolean                      read FHyperlinkAuditing        write SetHyperlinkAuditing;
      property  RunAllFlashInAllowMode  : boolean                      read FRunAllFlashInAllowMode   write SetRunAllFlashInAllowMode;
      property  AllowOutdatedPlugins    : boolean                      read FAllowOutdatedPlugins     write SetAllowOutdatedPlugins;
      property  AlwaysAuthorizePlugins  : boolean                      read FAlwaysAuthorizePlugins   write SetAlwaysAuthorizePlugins;
      property  SpellChecking           : boolean                      read FSpellChecking            write SetSpellChecking;
      property  SpellCheckerDicts       : ustring                      read FSpellCheckerDicts        write SetSpellCheckerDicts;
      property  HasValidMainFrame       : boolean                      read GetHasValidMainFrame;
      property  FrameCount              : NativeUInt                   read GetFrameCount;
      property  DragOperations          : TCefDragOperations           read FDragOperations           write FDragOperations;
      property  AudioMuted              : boolean                      read GetAudioMuted             write SetAudioMuted;
      property  SafeSearch              : boolean                      read FSafeSearch               write SetSafeSearch;
      property  YouTubeRestrict         : integer                      read FYouTubeRestrict          write SetYouTubeRestrict;
      property  PrintingEnabled         : boolean                      read FPrintingEnabled          write SetPrintingEnabled;

      property  WebRTCIPHandlingPolicy  : TCefWebRTCHandlingPolicy     read FWebRTCIPHandlingPolicy   write SetWebRTCIPHandlingPolicy;
      property  WebRTCMultipleRoutes    : TCefState                    read FWebRTCMultipleRoutes     write SetWebRTCMultipleRoutes;
      property  WebRTCNonproxiedUDP     : TCefState                    read FWebRTCNonProxiedUDP      write SetWebRTCNonProxiedUDP;

      property  ProxyType               : integer                      read FProxyType                write SetProxyType;
      property  ProxyScheme             : TCefProxyScheme              read FProxyScheme              write SetProxyScheme;
      property  ProxyServer             : ustring                      read FProxyServer              write SetProxyServer;
      property  ProxyPort               : integer                      read FProxyPort                write SetProxyPort;
      property  ProxyUsername           : ustring                      read FProxyUsername            write SetProxyUsername;
      property  ProxyPassword           : ustring                      read FProxyPassword            write SetProxyPassword;
      property  ProxyScriptURL          : ustring                      read FProxyScriptURL           write SetProxyScriptURL;
      property  ProxyByPassList         : ustring                      read FProxyByPassList          write SetProxyByPassList;
      property  MaxConnectionsPerProxy  : integer                      read FMaxConnectionsPerProxy   write SetMaxConnectionsPerProxy;

    published
      property  OnTextResultAvailable              : TOnTextResultAvailableEvent              read FOnTextResultAvailable              write FOnTextResultAvailable;
      property  OnPdfPrintFinished                 : TOnPdfPrintFinishedEvent                 read FOnPdfPrintFinished                 write FOnPdfPrintFinished;
      property  OnCookiesDeleted                   : TOnCookiesDeletedEvent                   read FOnCookiesDeleted                   write FOnCookiesDeleted;
      property  OnResolvedHostAvailable            : TOnResolvedIPsAvailableEvent             read FOnResolvedHostAvailable            write FOnResolvedHostAvailable;
      property  OnNavigationVisitorResultAvailable : TOnNavigationVisitorResultAvailableEvent read FOnNavigationVisitorResultAvailable write FOnNavigationVisitorResultAvailable;
      property  OnDownloadImageFinishedEvent       : TOnDownloadImageFinishedEvent            read FOnDownloadImageFinished            write FOnDownloadImageFinished;
      property  OnCookiesFlushed                   : TNotifyEvent                             read FOnCookiesFlushed                   write FOnCookiesFlushed;
      property  OnCertificateExceptionsCleared     : TNotifyEvent                             read FOnCertificateExceptionsCleared     write FOnCertificateExceptionsCleared;
      property  OnHttpAuthCredentialsCleared       : TNotifyEvent                             read FOnHttpAuthCredentialsCleared       write FOnHttpAuthCredentialsCleared;
      property  OnAllConnectionsClosed             : TNotifyEvent                             read FOnAllConnectionsClosed             write FOnAllConnectionsClosed;
      property  OnExecuteTaskOnCefThread           : TOnExecuteTaskOnCefThread                read FOnExecuteTaskOnCefThread           write FOnExecuteTaskOnCefThread;
      property  OnCookiesVisited                   : TOnCookiesVisited                        read FOnCookiesVisited                   write FOnCookiesVisited;
      property  OnCookieVisitorDestroyed           : TOnCookieVisitorDestroyed                read FOnCookieVisitorDestroyed           write FOnCookieVisitorDestroyed;
      property  OnCookieSet                        : TOnCookieSet                             read FOnCookieSet                        write FOnCookieSet;
      {$IFDEF MSWINDOWS}
      property  OnBrowserCompMsg        : TOnCompMsgEvent              read FOnBrowserCompMsg         write FOnBrowserCompMsg;
      property  OnWidgetCompMsg         : TOnCompMsgEvent              read FOnWidgetCompMsg          write FOnWidgetCompMsg;
      property  OnRenderCompMsg         : TOnCompMsgEvent              read FOnRenderCompMsg          write FOnRenderCompMsg;
      {$ENDIF}

      // ICefClient
      property OnProcessMessageReceived         : TOnProcessMessageReceived         read FOnProcessMessageReceived         write FOnProcessMessageReceived;

      // ICefLoadHandler
      property OnLoadStart                      : TOnLoadStart                      read FOnLoadStart                      write FOnLoadStart;
      property OnLoadEnd                        : TOnLoadEnd                        read FOnLoadEnd                        write FOnLoadEnd;
      property OnLoadError                      : TOnLoadError                      read FOnLoadError                      write FOnLoadError;
      property OnLoadingStateChange             : TOnLoadingStateChange             read FOnLoadingStateChange             write FOnLoadingStateChange;

      // ICefFocusHandler
      property OnTakeFocus                      : TOnTakeFocus                      read FOnTakeFocus                      write FOnTakeFocus;
      property OnSetFocus                       : TOnSetFocus                       read FOnSetFocus                       write FOnSetFocus;
      property OnGotFocus                       : TOnGotFocus                       read FOnGotFocus                       write FOnGotFocus;

      // ICefContextMenuHandler
      property OnBeforeContextMenu              : TOnBeforeContextMenu              read FOnBeforeContextMenu              write FOnBeforeContextMenu;
      property OnRunContextMenu                 : TOnRunContextMenu                 read FOnRunContextMenu                 write FOnRunContextMenu;
      property OnContextMenuCommand             : TOnContextMenuCommand             read FOnContextMenuCommand             write FOnContextMenuCommand;
      property OnContextMenuDismissed           : TOnContextMenuDismissed           read FOnContextMenuDismissed           write FOnContextMenuDismissed;

      // ICefKeyboardHandler
      property OnPreKeyEvent                    : TOnPreKeyEvent                    read FOnPreKeyEvent                    write FOnPreKeyEvent;
      property OnKeyEvent                       : TOnKeyEvent                       read FOnKeyEvent                       write FOnKeyEvent;

      // ICefDisplayHandler
      property OnAddressChange                  : TOnAddressChange                  read FOnAddressChange                  write FOnAddressChange;
      property OnTitleChange                    : TOnTitleChange                    read FOnTitleChange                    write FOnTitleChange;
      property OnFavIconUrlChange               : TOnFavIconUrlChange               read FOnFavIconUrlChange               write FOnFavIconUrlChange;
      property OnFullScreenModeChange           : TOnFullScreenModeChange           read FOnFullScreenModeChange           write FOnFullScreenModeChange;
      property OnTooltip                        : TOnTooltip                        read FOnTooltip                        write FOnTooltip;
      property OnStatusMessage                  : TOnStatusMessage                  read FOnStatusMessage                  write FOnStatusMessage;
      property OnConsoleMessage                 : TOnConsoleMessage                 read FOnConsoleMessage                 write FOnConsoleMessage;
      property OnAutoResize                     : TOnAutoResize                     read FOnAutoResize                     write FOnAutoResize;
      property OnLoadingProgressChange          : TOnLoadingProgressChange          read FOnLoadingProgressChange          write FOnLoadingProgressChange;

      // ICefDownloadHandler
      property OnBeforeDownload                 : TOnBeforeDownload                 read FOnBeforeDownload                 write FOnBeforeDownload;
      property OnDownloadUpdated                : TOnDownloadUpdated                read FOnDownloadUpdated                write FOnDownloadUpdated;

      // ICefJsDialogHandler
      property OnJsdialog                       : TOnJsdialog                       read FOnJsdialog                       write FOnJsdialog;
      property OnBeforeUnloadDialog             : TOnBeforeUnloadDialog             read FOnBeforeUnloadDialog             write FOnBeforeUnloadDialog;
      property OnResetDialogState               : TOnResetDialogState               read FOnResetDialogState               write FOnResetDialogState;
      property OnDialogClosed                   : TOnDialogClosed                   read FOnDialogClosed                   write FOnDialogClosed;

      // ICefLifeSpanHandler
      property OnBeforePopup                    : TOnBeforePopup                    read FOnBeforePopup                    write FOnBeforePopup;
      property OnAfterCreated                   : TOnAfterCreated                   read FOnAfterCreated                   write FOnAfterCreated;
      property OnBeforeClose                    : TOnBeforeClose                    read FOnBeforeClose                    write FOnBeforeClose;
      property OnClose                          : TOnClose                          read FOnClose                          write FOnClose;

      // ICefRequestHandler
      property OnBeforeBrowse                      : TOnBeforeBrowse                   read FOnBeforeBrowse                      write FOnBeforeBrowse;
      property OnOpenUrlFromTab                    : TOnOpenUrlFromTab                 read FOnOpenUrlFromTab                    write FOnOpenUrlFromTab;
      property OnGetAuthCredentials                : TOnGetAuthCredentials             read FOnGetAuthCredentials                write FOnGetAuthCredentials;
      property OnQuotaRequest                      : TOnQuotaRequest                   read FOnQuotaRequest                      write FOnQuotaRequest;
      property OnCertificateError                  : TOnCertificateError               read FOnCertificateError                  write FOnCertificateError;
      property OnSelectClientCertificate           : TOnSelectClientCertificate        read FOnSelectClientCertificate           write FOnSelectClientCertificate;
      property OnPluginCrashed                     : TOnPluginCrashed                  read FOnPluginCrashed                     write FOnPluginCrashed;
      property OnRenderViewReady                   : TOnRenderViewReady                read FOnRenderViewReady                   write FOnRenderViewReady;
      property OnRenderProcessTerminated           : TOnRenderProcessTerminated        read FOnRenderProcessTerminated           write FOnRenderProcessTerminated;
      property OnGetResourceRequestHandler_ReqHdlr : TOnGetResourceRequestHandler      read FOnGetResourceRequestHandler_ReqHdlr write FOnGetResourceRequestHandler_ReqHdlr;

      // ICefResourceRequestHandler
      property OnBeforeResourceLoad             : TOnBeforeResourceLoad             read FOnBeforeResourceLoad             write FOnBeforeResourceLoad;
      property OnGetResourceHandler             : TOnGetResourceHandler             read FOnGetResourceHandler             write FOnGetResourceHandler;
      property OnResourceRedirect               : TOnResourceRedirect               read FOnResourceRedirect               write FOnResourceRedirect;
      property OnResourceResponse               : TOnResourceResponse               read FOnResourceResponse               write FOnResourceResponse;
      property OnGetResourceResponseFilter      : TOnGetResourceResponseFilter      read FOnGetResourceResponseFilter      write FOnGetResourceResponseFilter;
      property OnResourceLoadComplete           : TOnResourceLoadComplete           read FOnResourceLoadComplete           write FOnResourceLoadComplete;
      property OnProtocolExecution              : TOnProtocolExecution              read FOnProtocolExecution              write FOnProtocolExecution;

      // ICefCookieAccessFilter
      property OnCanSendCookie                  : TOnCanSendCookie                  read FOnCanSendCookie                  write FOnCanSendCookie;
      property OnCanSaveCookie                  : TOnCanSaveCookie                  read FOnCanSaveCookie                  write FOnCanSaveCookie;

      // ICefDialogHandler
      property OnFileDialog                     : TOnFileDialog                     read FOnFileDialog                     write FOnFileDialog;

      // ICefRenderHandler
      property OnGetAccessibilityHandler        : TOnGetAccessibilityHandler        read FOnGetAccessibilityHandler        write FOnGetAccessibilityHandler;
      property OnGetRootScreenRect              : TOnGetRootScreenRect              read FOnGetRootScreenRect              write FOnGetRootScreenRect;
      property OnGetViewRect                    : TOnGetViewRect                    read FOnGetViewRect                    write FOnGetViewRect;
      property OnGetScreenPoint                 : TOnGetScreenPoint                 read FOnGetScreenPoint                 write FOnGetScreenPoint;
      property OnGetScreenInfo                  : TOnGetScreenInfo                  read FOnGetScreenInfo                  write FOnGetScreenInfo;
      property OnPopupShow                      : TOnPopupShow                      read FOnPopupShow                      write FOnPopupShow;
      property OnPopupSize                      : TOnPopupSize                      read FOnPopupSize                      write FOnPopupSize;
      property OnPaint                          : TOnPaint                          read FOnPaint                          write FOnPaint;
      property OnAcceleratedPaint               : TOnAcceleratedPaint               read FOnAcceleratedPaint               write FOnAcceleratedPaint;
      property OnCursorChange                   : TOnCursorChange                   read FOnCursorChange                   write FOnCursorChange;
      property OnScrollOffsetChanged            : TOnScrollOffsetChanged            read FOnScrollOffsetChanged            write FOnScrollOffsetChanged;
      property OnIMECompositionRangeChanged     : TOnIMECompositionRangeChanged     read FOnIMECompositionRangeChanged     write FOnIMECompositionRangeChanged;
      property OnTextSelectionChanged           : TOnTextSelectionChanged           read FOnTextSelectionChanged           write FOnTextSelectionChanged;
      property OnVirtualKeyboardRequested       : TOnVirtualKeyboardRequested       read FOnVirtualKeyboardRequested       write FOnVirtualKeyboardRequested;

      // ICefDragHandler
      property OnDragEnter                      : TOnDragEnter                      read FOnDragEnter                      write FOnDragEnter;
      property OnDraggableRegionsChanged        : TOnDraggableRegionsChanged        read FOnDraggableRegionsChanged        write FOnDraggableRegionsChanged;

      // ICefFindHandler
      property OnFindResult                     : TOnFindResult                     read FOnFindResult                     write FOnFindResult;

      // ICefRequestContextHandler
      property OnRequestContextInitialized            : TOnRequestContextInitialized      read FOnRequestContextInitialized            write SetOnRequestContextInitialized;
      property OnBeforePluginLoad                     : TOnBeforePluginLoad               read FOnBeforePluginLoad                     write SetOnBeforePluginLoad;
      property OnGetResourceRequestHandler_ReqCtxHdlr : TOnGetResourceRequestHandler      read FOnGetResourceRequestHandler_ReqCtxHdlr write FOnGetResourceRequestHandler_ReqCtxHdlr;
  end;

// *********************************************************
// ********************** ATTENTION ! **********************
// *********************************************************
// **                                                     **
// **  MANY OF THE EVENTS IN CEF4DELPHI COMPONENTS LIKE   **
// **  TCHROMIUM, TFMXCHROMIUM OR TCEFAPPLICATION ARE     **
// **  EXECUTED IN A CEF THREAD BY DEFAULT.               **
// **                                                     **
// **  WINDOWS CONTROLS MUST BE CREATED AND DESTROYED IN  **
// **  THE SAME THREAD TO AVOID ERRORS.                   **
// **  SOME OF THEM RECREATE THE HANDLERS IF THEY ARE     **
// **  MODIFIED AND CAN CAUSE THE SAME ERRORS.            **
// **                                                     **
// **  DON'T CREATE, MODIFY OR DESTROY WINDOWS CONTROLS   **
// **  INSIDE THE CEF4DELPHI EVENTS AND USE               **
// **  SYNCHRONIZATION OBJECTS TO PROTECT VARIABLES AND   **
// **  FIELDS IF THEY ARE ALSO USED IN THE MAIN THREAD.   **
// **                                                     **
// **  READ THIS FOR MORE INFORMATION :                   **
// **  https://www.briskbard.com/index.php?pageid=cef     **
// **                                                     **
// **  USE OUR FORUMS FOR MORE QUESTIONS :                **
// **  https://www.briskbard.com/forum/                   **
// **                                                     **
// *********************************************************
// *********************************************************

implementation

uses
  System.SysUtils, System.Math,
  uCEFBrowser, uCEFValue, uCEFDictionaryValue, uCEFStringMultimap, uCEFFrame,
  uCEFApplicationCore, uCEFProcessMessage, uCEFRequestContext, uCEFCookieManager,
  uCEFPDFPrintCallback, uCEFResolveCallback, uCEFDeleteCookiesCallback, uCEFStringVisitor,
  uCEFListValue, uCEFNavigationEntryVisitor, uCEFDownloadImageCallBack,
  uCEFRequestContextHandler, uCEFCookieVisitor, uCEFSetCookieCallback,
  uCEFResourceRequestHandler;

constructor TFMXChromium.Create(AOwner: TComponent);
begin
  FBrowser                := nil;
  FBrowserId              := 0;
  FClosing                := False;
  FIsOSR                  := False;
  FInitialized            := False;
  FDefaultUrl             := 'about:blank';
  FHandler                := nil;
  FReqContextHandler      := nil;
  FResourceRequestHandler := nil;
  FOptions                := nil;
  FFontOptions            := nil;
  FDefaultEncoding        := '';
  FPDFPrintOptions        := nil;
  FUpdatePreferences      := False;
  FCustomHeaderName       := '';
  FCustomHeaderValue      := '';
  FPrefsFileName          := '';
  FAddCustomHeader        := False;
  FDoNotTrack             := True;
  FSendReferrer           := True;
  FHyperlinkAuditing      := False;
  FRunAllFlashInAllowMode := False;
  FAllowOutdatedPlugins   := False;
  FAlwaysAuthorizePlugins := False;
  FSpellChecking          := True;
  FSpellCheckerDicts      := '';
  FZoomStep               := ZOOM_STEP_DEF;
  FSafeSearch             := False;
  FYouTubeRestrict        := YOUTUBE_RESTRICT_OFF;
  FPrintingEnabled        := True;

  {$IFDEF MSWINDOWS}
  FOldBrowserCompWndPrc   := nil;
  FOldWidgetCompWndPrc    := nil;
  FOldRenderCompWndPrc    := nil;
  FBrowserCompHWND        := 0;
  FWidgetCompHWND         := 0;
  FRenderCompHWND         := 0;
  FBrowserCompStub        := nil;
  FWidgetCompStub         := nil;
  FRenderCompStub         := nil;
  {$ENDIF}

  FDragOperations         := DRAG_OPERATION_NONE;
  FDragAndDropInitialized := False;

  FWebRTCIPHandlingPolicy := hpDefault;
  FWebRTCMultipleRoutes   := STATE_DEFAULT;
  FWebRTCNonProxiedUDP    := STATE_DEFAULT;

  FProxyType              := CEF_PROXYTYPE_SYSTEM;
  FProxyScheme            := psHTTP;
  FProxyServer            := '';
  FProxyPort              := 80;
  FProxyUsername          := '';
  FProxyPassword          := '';
  FProxyScriptURL         := '';
  FProxyByPassList        := '';
  FMaxConnectionsPerProxy := CEF_MAX_CONNECTIONS_PER_PROXY_DEFAULT_VALUE;

  FillChar(FWindowInfo,    SizeOf(TCefWindowInfo), 0);
  FillChar(FDevWindowInfo, SizeOf(TCefWindowInfo), 0);

  InitializeSettings(FBrowserSettings);
  InitializeSettings(FDevBrowserSettings);

  InitializeEvents;

  inherited Create(AOwner);
end;

destructor TFMXChromium.Destroy;
begin
  try
    try
      ClearBrowserReference;

      if (FFontOptions     <> nil) then FreeAndNil(FFontOptions);
      if (FOptions         <> nil) then FreeAndNil(FOptions);
      if (FPDFPrintOptions <> nil) then FreeAndNil(FPDFPrintOptions);
    except
      on e : exception do
        if CustomExceptionHandler('TFMXChromium.Destroy', e) then raise;
    end;
  finally
    inherited Destroy;
  end;
end;

procedure TFMXChromium.BeforeDestruction;
begin
  {$IFDEF MSWINDOWS}
  RestoreCompWndProc(FBrowserCompHWND, 0, FOldBrowserCompWndPrc);
  FreeAndNilStub(FBrowserCompStub);

  RestoreCompWndProc(FWidgetCompHWND, 0, FOldWidgetCompWndPrc);
  FreeAndNilStub(FWidgetCompStub);

  RestoreCompWndProc(FRenderCompHWND, 0, FOldRenderCompWndPrc);
  FreeAndNilStub(FRenderCompStub);
  {$ENDIF}

  DestroyClientHandler;
  DestroyReqContextHandler;
  DestroyResourceRequestHandler;

  inherited BeforeDestruction;
end;

procedure TFMXChromium.ClearBrowserReference;
begin
  FBrowser   := nil;
  FBrowserId := 0;
end;

procedure TFMXChromium.DestroyClientHandler;
begin
  try
    if (FHandler <> nil) then
      begin
        FHandler.RemoveReferences;
        FHandler := nil;
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TFMXChromium.DestroyClientHandler', e) then raise;
  end;
end;

procedure TFMXChromium.DestroyReqContextHandler;
begin
  try
    if (FReqContextHandler <> nil) then
      begin
        FReqContextHandler.RemoveReferences;
        FReqContextHandler := nil;
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TFMXChromium.DestroyReqContextHandler', e) then raise;
  end;
end;

procedure TFMXChromium.CreateReqContextHandler;
begin
  if MustCreateRequestContextHandler and
     (FReqContextHandler = nil) then
    FReqContextHandler := TCustomRequestContextHandler.Create(self);
end;

procedure TFMXChromium.DestroyResourceRequestHandler;
begin
  try
    if (FResourceRequestHandler <> nil) then
      begin
        FResourceRequestHandler.RemoveReferences;
        FResourceRequestHandler := nil;
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TFMXChromium.DestroyResourceRequestHandler', e) then raise;
  end;
end;

procedure TFMXChromium.CreateResourceRequestHandler;
begin
  if MustCreateResourceRequestHandler and
     (FResourceRequestHandler = nil) then
    FResourceRequestHandler := TCustomResourceRequestHandler.Create(self);
end;

procedure TFMXChromium.AfterConstruction;
begin
  inherited AfterConstruction;

  try
    if not(csDesigning in ComponentState) then
      begin
        FOptions         := TChromiumOptions.Create;
        FFontOptions     := TChromiumFontOptions.Create;
        FPDFPrintOptions := TPDFPrintOptions.Create;
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TFMXChromium.AfterConstruction', e) then raise;
  end;
end;

function TFMXChromium.CreateClientHandler(aIsOSR : boolean) : boolean;
begin
  Result := False;

  try
    if (FHandler = nil) then
      begin
        FIsOSR   := aIsOsr;
        FHandler := TCustomClientHandler.Create(Self);
        Result   := True;
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TFMXChromium.CreateClientHandler', e) then raise;
  end;
end;

function TFMXChromium.CreateClientHandler(var aClient : ICefClient; aIsOSR : boolean) : boolean;
begin
  if CreateClientHandler(aIsOSR) then
    begin
      aClient := FHandler;
      Result  := True;
    end
   else
    Result := False;
end;

procedure TFMXChromium.InitializeEvents;
begin
  // ICefClient
  FOnProcessMessageReceived       := nil;

  // ICefLoadHandler
  FOnLoadStart                    := nil;
  FOnLoadEnd                      := nil;
  FOnLoadError                    := nil;
  FOnLoadingStateChange           := nil;

  // ICefFocusHandler
  FOnTakeFocus                    := nil;
  FOnSetFocus                     := nil;
  FOnGotFocus                     := nil;

  // ICefContextMenuHandler
  FOnBeforeContextMenu            := nil;
  FOnRunContextMenu               := nil;
  FOnContextMenuCommand           := nil;
  FOnContextMenuDismissed         := nil;

  // ICefKeyboardHandler
  FOnPreKeyEvent                  := nil;
  FOnKeyEvent                     := nil;

  // ICefDisplayHandler
  FOnAddressChange                := nil;
  FOnTitleChange                  := nil;
  FOnFavIconUrlChange             := nil;
  FOnFullScreenModeChange         := nil;
  FOnTooltip                      := nil;
  FOnStatusMessage                := nil;
  FOnConsoleMessage               := nil;
  FOnAutoResize                   := nil;
  FOnLoadingProgressChange        := nil;

  // ICefDownloadHandler
  FOnBeforeDownload               := nil;
  FOnDownloadUpdated              := nil;

  // ICefJsDialogHandler
  FOnJsdialog                     := nil;
  FOnBeforeUnloadDialog           := nil;
  FOnResetDialogState             := nil;
  FOnDialogClosed                 := nil;

  // ICefLifeSpanHandler
  FOnBeforePopup                  := nil;
  FOnAfterCreated                 := nil;
  FOnBeforeClose                  := nil;
  FOnClose                        := nil;

  // ICefRequestHandler
  FOnBeforeBrowse                      := nil;
  FOnOpenUrlFromTab                    := nil;
  FOnGetAuthCredentials                := nil;
  FOnQuotaRequest                      := nil;
  FOnCertificateError                  := nil;
  FOnSelectClientCertificate           := nil;
  FOnPluginCrashed                     := nil;
  FOnRenderViewReady                   := nil;
  FOnRenderProcessTerminated           := nil;
  FOnGetResourceRequestHandler_ReqHdlr := nil;

  // ICefResourceRequestHandler
  FOnBeforeResourceLoad           := nil;
  FOnGetResourceHandler           := nil;
  FOnResourceRedirect             := nil;
  FOnResourceResponse             := nil;
  FOnGetResourceResponseFilter    := nil;
  FOnResourceLoadComplete         := nil;
  FOnProtocolExecution            := nil;

  // ICefCookieAccessFilter
  FOnCanSendCookie                := nil;
  FOnCanSaveCookie                := nil;

  // ICefDialogHandler
  FOnFileDialog                   := nil;

  // ICefRenderHandler
  FOnGetAccessibilityHandler      := nil;
  FOnGetRootScreenRect            := nil;
  FOnGetViewRect                  := nil;
  FOnGetScreenPoint               := nil;
  FOnGetScreenInfo                := nil;
  FOnPopupShow                    := nil;
  FOnPopupSize                    := nil;
  FOnPaint                        := nil;
  FOnAcceleratedPaint             := nil;
  FOnCursorChange                 := nil;
  FOnScrollOffsetChanged          := nil;
  FOnIMECompositionRangeChanged   := nil;
  FOnTextSelectionChanged         := nil;
  FOnVirtualKeyboardRequested     := nil;

  // ICefDragHandler
  FOnDragEnter                    := nil;
  FOnDraggableRegionsChanged      := nil;

  // ICefFindHandler
  FOnFindResult                   := nil;

  // ICefRequestContextHandler
  FOnRequestContextInitialized            := nil;
  FOnBeforePluginLoad                     := nil;
  FOnGetResourceRequestHandler_ReqCtxHdlr := nil;

  // Custom
  FOnTextResultAvailable              := nil;
  FOnPdfPrintFinished                 := nil;
  FOnCookiesDeleted                   := nil;
  FOnResolvedHostAvailable            := nil;
  FOnNavigationVisitorResultAvailable := nil;
  FOnDownloadImageFinished            := nil;
  FOnCookiesFlushed                   := nil;
  FOnCertificateExceptionsCleared     := nil;
  FOnHttpAuthCredentialsCleared       := nil;
  FOnAllConnectionsClosed             := nil;
  FOnExecuteTaskOnCefThread           := nil;
  FOnCookiesVisited                   := nil;
  FOnCookieVisitorDestroyed           := nil;
  FOnCookieSet                        := nil;
end;

function TFMXChromium.CreateBrowser(const aWindowName  : ustring;
                                    const aContext     : ICefRequestContext;
                                    const aExtraInfo   : ICefDictionaryValue) : boolean;
var
  TempNewContext, TempGlobalContext : ICefRequestContext;
begin
  Result         := False;
  TempNewContext := nil;

  try
    try
      // GlobalCEFApp.GlobalContextInitialized has to be TRUE before creating any browser
      // even if you use a custom request context.
      // If you create a browser in the initialization of your app, make sure you call this
      // function when GlobalCEFApp.GlobalContextInitialized is TRUE.
      // Use the GlobalCEFApp.OnContextInitialized event to know when
      // GlobalCEFApp.GlobalContextInitialized is set to TRUE.

      if not(csDesigning in ComponentState) and
         not(FClosing)         and
         (FBrowser     =  nil) and
         (FBrowserId   =  0)   and
         (GlobalCEFApp <> nil) and
         GlobalCEFApp.GlobalContextInitialized and
         CreateClientHandler then
        begin
          GetSettings(FBrowserSettings);
          WindowInfoAsWindowless(FWindowInfo, 0, aWindowName);

          if (aContext = nil) then
            begin
              CreateReqContextHandler;

              if (FReqContextHandler <> nil) then
                begin
                  TempGlobalContext := TCefRequestContextRef.Global();
                  TempNewContext    := TCefRequestContextRef.Shared(TempGlobalContext, FReqContextHandler);
                end;
            end
           else
            TempNewContext := aContext;

          if GlobalCEFApp.MultiThreadedMessageLoop then
            Result := CreateBrowserHost(@FWindowInfo, FDefaultUrl, @FBrowserSettings, aExtraInfo, TempNewContext)
           else
            Result := CreateBrowserHostSync(@FWindowInfo, FDefaultUrl, @FBrowserSettings, aExtraInfo, TempNewContext);
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TFMXChromium.CreateBrowser', e) then raise;
    end;
  finally
    TempGlobalContext := nil;
    TempNewContext    := nil;
  end;
end;

{$IFDEF MSWINDOWS}
function TFMXChromium.CreateBrowser(      aParentHandle : HWND;
                                          aParentRect   : TRect;
                                    const aWindowName   : ustring;
                                    const aContext      : ICefRequestContext;
                                    const aExtraInfo    : ICefDictionaryValue) : boolean;
var
  TempNewContext, TempGlobalContext : ICefRequestContext;
begin
  Result            := False;
  TempNewContext    := nil;
  TempGlobalContext := nil;

  try
    try
      // GlobalCEFApp.GlobalContextInitialized has to be TRUE before creating any browser
      // even if you use a custom request context.
      // If you create a browser in the initialization of your app, make sure you call this
      // function when GlobalCEFApp.GlobalContextInitialized is TRUE.
      // Use the GlobalCEFApp.OnContextInitialized event to know when
      // GlobalCEFApp.GlobalContextInitialized is set to TRUE.

      if not(csDesigning in ComponentState) and
         not(FClosing)         and
         (FBrowser     =  nil) and
         (FBrowserId   =  0)   and
         (GlobalCEFApp <> nil) and
         GlobalCEFApp.GlobalContextInitialized  and
         CreateClientHandler(aParentHandle = 0) then
        begin
          GetSettings(FBrowserSettings);
          InitializeWindowInfo(aParentHandle, aParentRect, aWindowName);
          CreateResourceRequestHandler;

          if (aContext = nil) then
            begin
              CreateReqContextHandler;

              if (FReqContextHandler = nil) then
                TempNewContext := nil
               else
                begin
                  TempGlobalContext := TCefRequestContextRef.Global();
                  TempNewContext    := TCefRequestContextRef.Shared(TempGlobalContext, FReqContextHandler);
                end;
            end
           else
            TempNewContext := aContext;

          if GlobalCEFApp.MultiThreadedMessageLoop then
            Result := CreateBrowserHost(@FWindowInfo, FDefaultUrl, @FBrowserSettings, aExtraInfo, TempNewContext)
           else
            Result := CreateBrowserHostSync(@FWindowInfo, FDefaultUrl, @FBrowserSettings, aExtraInfo, TempNewContext);
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TFMXChromium.CreateBrowser', e) then raise;
    end;
  finally
    TempGlobalContext := nil;
    TempNewContext    := nil;
  end;
end;

procedure TFMXChromium.InitializeWindowInfo(      aParentHandle : HWND;
                                                  aParentRect   : TRect;
                                            const aWindowName   : ustring);
begin
  if FIsOSR then
    WindowInfoAsWindowless(FWindowInfo, 0, aWindowName)
   else
    WindowInfoAsChild(FWindowInfo, aParentHandle, aParentRect, aWindowName);
end;
{$ENDIF}

function TFMXChromium.ShareRequestContext(var   aContext : ICefRequestContext;
                                          const aHandler : ICefRequestContextHandler) : boolean;
begin
  Result   := False;
  aContext := nil;

  if Initialized then
    begin
      aContext := TCefRequestContextRef.Shared(FBrowser.Host.RequestContext, aHandler);
      Result   := (aContext <> nil);
    end;
end;

procedure TFMXChromium.CloseBrowser(aForceClose : boolean);
begin
  if Initialized then FBrowser.Host.CloseBrowser(aForceClose);
end;

function TFMXChromium.CreateBrowserHost(      aWindowInfo : PCefWindowInfo;
                                        const aURL        : ustring;
                                        const aSettings   : PCefBrowserSettings;
                                        const aExtraInfo  : ICefDictionaryValue;
                                        const aContext    : ICefRequestContext): boolean;
var
  TempURL : TCefString;
begin
  TempURL := CefString(aURL);
  Result  := cef_browser_host_create_browser(aWindowInfo, FHandler.Wrap, @TempURL, aSettings, CefGetData(aExtraInfo), CefGetData(aContext)) <> 0;
end;

function TFMXChromium.CreateBrowserHostSync(      aWindowInfo : PCefWindowInfo;
                                            const aURL        : ustring;
                                            const aSettings   : PCefBrowserSettings;
                                            const aExtraInfo  : ICefDictionaryValue;
                                            const aContext    : ICefRequestContext): boolean;
var
  TempURL : TCefString;
begin
  TempURL  := CefString(aURL);
  FBrowser := TCefBrowserRef.UnWrap(cef_browser_host_create_browser_sync(aWindowInfo, FHandler.Wrap, @TempURL, aSettings, CefGetData(aExtraInfo), CefGetData(aContext)));

  if (FBrowser <> nil) then
    begin
      FBrowserId   := FBrowser.Identifier;
      FInitialized := (FBrowserId <> 0);
      Result       := FInitialized;
    end
   else
    Result := False;
end;

procedure TFMXChromium.Find(aIdentifier : integer; const aSearchText : ustring; aForward, aMatchCase, aFindNext : Boolean);
begin
  if Initialized then FBrowser.Host.Find(aIdentifier, aSearchText, aForward, aMatchCase, aFindNext);
end;

procedure TFMXChromium.StopFinding(aClearSelection : Boolean);
begin
  if Initialized then FBrowser.Host.StopFinding(aClearSelection);
end;

procedure TFMXChromium.Print;
begin
  if Initialized then FBrowser.Host.Print;
end;

procedure TFMXChromium.PrintToPDF(const aFilePath, aTitle, aURL : ustring);
var
  TempSettings : TCefPdfPrintSettings;
  TempCallback : ICefPdfPrintCallback;
begin
  if Initialized then
    begin
      GetPrintPDFSettings(TempSettings, aTitle, aURL);
      TempCallback := TCefCustomPDFPrintCallBack.Create(self);
      FBrowser.Host.PrintToPdf(aFilePath, @TempSettings, TempCallback);
    end;
end;

procedure TFMXChromium.ClipboardCopy;
var
  TempFrame : ICefFrame;
begin
  if Initialized then
    begin
      TempFrame := FBrowser.FocusedFrame;
      if (TempFrame = nil) then TempFrame := FBrowser.MainFrame;

      if (TempFrame <> nil) and TempFrame.IsValid then TempFrame.Copy;
    end;
end;

procedure TFMXChromium.ClipboardPaste;
var
  TempFrame : ICefFrame;
begin
  if Initialized then
    begin
      TempFrame := FBrowser.FocusedFrame;
      if (TempFrame = nil) then TempFrame := FBrowser.MainFrame;

      if (TempFrame <> nil) and TempFrame.IsValid then TempFrame.Paste;
    end;
end;

procedure TFMXChromium.ClipboardCut;
var
  TempFrame : ICefFrame;
begin
  if Initialized then
    begin
      TempFrame := FBrowser.FocusedFrame;
      if (TempFrame = nil) then TempFrame := FBrowser.MainFrame;

      if (TempFrame <> nil) and TempFrame.IsValid then TempFrame.Cut;
    end;
end;

procedure TFMXChromium.ClipboardUndo;
var
  TempFrame : ICefFrame;
begin
  if Initialized then
    begin
      TempFrame := FBrowser.FocusedFrame;
      if (TempFrame = nil) then TempFrame := FBrowser.MainFrame;

      if (TempFrame <> nil) and TempFrame.IsValid then TempFrame.Undo;
    end;
end;

procedure TFMXChromium.ClipboardRedo;
var
  TempFrame : ICefFrame;
begin
  if Initialized then
    begin
      TempFrame := FBrowser.FocusedFrame;
      if (TempFrame = nil) then TempFrame := FBrowser.MainFrame;

      if (TempFrame <> nil) and TempFrame.IsValid then TempFrame.Redo;
    end;
end;

procedure TFMXChromium.ClipboardDel;
var
  TempFrame : ICefFrame;
begin
  if Initialized then
    begin
      TempFrame := FBrowser.FocusedFrame;
      if (TempFrame = nil) then TempFrame := FBrowser.MainFrame;

      if (TempFrame <> nil) and TempFrame.IsValid then TempFrame.Del;
    end;
end;

procedure TFMXChromium.SelectAll;
var
  TempFrame : ICefFrame;
begin
  if Initialized then
    begin
      TempFrame := FBrowser.FocusedFrame;
      if (TempFrame = nil) then TempFrame := FBrowser.MainFrame;

      if (TempFrame <> nil) and TempFrame.IsValid then TempFrame.SelectAll;
    end;
end;

procedure TFMXChromium.GetPrintPDFSettings(var aSettings : TCefPdfPrintSettings; const aTitle, aURL : ustring);
begin
  if (FPDFPrintOptions <> nil) then
    begin
      aSettings.header_footer_title   := CefString(aTitle);
      aSettings.header_footer_url     := CefString(aURL);
      aSettings.page_width            := FPDFPrintOptions.page_width;
      aSettings.page_height           := FPDFPrintOptions.page_height;
      aSettings.scale_factor          := FPDFPrintOptions.scale_factor;
      aSettings.margin_top            := FPDFPrintOptions.margin_top;
      aSettings.margin_right          := FPDFPrintOptions.margin_right;
      aSettings.margin_bottom         := FPDFPrintOptions.margin_bottom;
      aSettings.margin_left           := FPDFPrintOptions.margin_left;
      aSettings.margin_type           := FPDFPrintOptions.margin_type;
      aSettings.header_footer_enabled := Ord(FPDFPrintOptions.header_footer_enabled);
      aSettings.selection_only        := Ord(FPDFPrintOptions.selection_only);
      aSettings.landscape             := Ord(FPDFPrintOptions.landscape);
      aSettings.backgrounds_enabled   := Ord(FPDFPrintOptions.backgrounds_enabled);
    end;
end;

procedure TFMXChromium.GetSettings(var aSettings : TCefBrowserSettings);
begin
  if (FFontOptions <> nil) and (FOptions <> nil) then
    begin
      aSettings.size                            := SizeOf(TCefBrowserSettings);
      aSettings.windowless_frame_rate           := FOptions.WindowlessFrameRate;
      aSettings.standard_font_family            := CefString(FFontOptions.StandardFontFamily);
      aSettings.fixed_font_family               := CefString(FFontOptions.FixedFontFamily);
      aSettings.serif_font_family               := CefString(FFontOptions.SerifFontFamily);
      aSettings.sans_serif_font_family          := CefString(FFontOptions.SansSerifFontFamily);
      aSettings.cursive_font_family             := CefString(FFontOptions.CursiveFontFamily);
      aSettings.fantasy_font_family             := CefString(FFontOptions.FantasyFontFamily);
      aSettings.default_font_size               := FFontOptions.DefaultFontSize;
      aSettings.default_fixed_font_size         := FFontOptions.DefaultFixedFontSize;
      aSettings.minimum_font_size               := FFontOptions.MinimumFontSize;
      aSettings.minimum_logical_font_size       := FFontOptions.MinimumLogicalFontSize;
      aSettings.remote_fonts                    := FFontOptions.RemoteFonts;
      aSettings.default_encoding                := CefString(DefaultEncoding);
      aSettings.javascript                      := FOptions.Javascript;
      aSettings.javascript_close_windows        := FOptions.JavascriptCloseWindows;
      aSettings.javascript_access_clipboard     := FOptions.JavascriptAccessClipboard;
      aSettings.javascript_dom_paste            := FOptions.JavascriptDomPaste;
      aSettings.plugins                         := FOptions.Plugins;
      aSettings.universal_access_from_file_urls := FOptions.UniversalAccessFromFileUrls;
      aSettings.file_access_from_file_urls      := FOptions.FileAccessFromFileUrls;
      aSettings.web_security                    := FOptions.WebSecurity;
      aSettings.image_loading                   := FOptions.ImageLoading;
      aSettings.image_shrink_standalone_to_fit  := FOptions.ImageShrinkStandaloneToFit;
      aSettings.text_area_resize                := FOptions.TextAreaResize;
      aSettings.tab_to_links                    := FOptions.TabToLinks;
      aSettings.local_storage                   := FOptions.LocalStorage;
      aSettings.databases                       := FOptions.Databases;
      aSettings.application_cache               := FOptions.ApplicationCache;
      aSettings.webgl                           := FOptions.Webgl;
      aSettings.background_color                := FOptions.BackgroundColor;
      aSettings.accept_language_list            := CefString(FOptions.AcceptLanguageList);
    end;
end;

procedure TFMXChromium.InitializeSettings(var aSettings : TCefBrowserSettings);
begin
  aSettings.size                            := SizeOf(TCefBrowserSettings);
  aSettings.windowless_frame_rate           := 30;
  aSettings.standard_font_family            := CefString('');
  aSettings.fixed_font_family               := CefString('');
  aSettings.serif_font_family               := CefString('');
  aSettings.sans_serif_font_family          := CefString('');
  aSettings.cursive_font_family             := CefString('');
  aSettings.fantasy_font_family             := CefString('');
  aSettings.default_font_size               := 0;
  aSettings.default_fixed_font_size         := 0;
  aSettings.minimum_font_size               := 0;
  aSettings.minimum_logical_font_size       := 0;
  aSettings.remote_fonts                    := STATE_DEFAULT;
  aSettings.default_encoding                := CefString('');
  aSettings.javascript                      := STATE_DEFAULT;
  aSettings.javascript_close_windows        := STATE_DEFAULT;
  aSettings.javascript_access_clipboard     := STATE_DEFAULT;
  aSettings.javascript_dom_paste            := STATE_DEFAULT;
  aSettings.plugins                         := STATE_DEFAULT;
  aSettings.universal_access_from_file_urls := STATE_DEFAULT;
  aSettings.file_access_from_file_urls      := STATE_DEFAULT;
  aSettings.web_security                    := STATE_DEFAULT;
  aSettings.image_loading                   := STATE_DEFAULT;
  aSettings.image_shrink_standalone_to_fit  := STATE_DEFAULT;
  aSettings.text_area_resize                := STATE_DEFAULT;
  aSettings.tab_to_links                    := STATE_DEFAULT;
  aSettings.local_storage                   := STATE_DEFAULT;
  aSettings.databases                       := STATE_DEFAULT;
  aSettings.application_cache               := STATE_DEFAULT;
  aSettings.webgl                           := STATE_DEFAULT;
  aSettings.background_color                := 0;
  aSettings.accept_language_list            := CefString('');
end;

// Leave aFrameName empty to load the URL in the main frame
procedure TFMXChromium.LoadURL(const aURL : ustring; const aFrameName : ustring);
var
  TempFrame : ICefFrame;
begin
  if Initialized then
    begin
      if (length(aFrameName) > 0) then
        TempFrame := FBrowser.GetFrame(aFrameName)
       else
        TempFrame := FBrowser.MainFrame;

      if (TempFrame <> nil) and TempFrame.IsValid then TempFrame.LoadUrl(aURL);
    end;
end;

procedure TFMXChromium.LoadURL(const aURL : ustring; const aFrame : ICefFrame);
begin
  if Initialized and (aFrame <> nil) and aFrame.IsValid then aFrame.LoadUrl(aURL);
end;

procedure TFMXChromium.LoadURL(const aURL : ustring; const aFrameIdentifier : int64);
var
  TempFrame : ICefFrame;
begin
  if Initialized then
    begin
      if (aFrameIdentifier <> 0) then
        TempFrame := FBrowser.GetFrameByident(aFrameIdentifier)
       else
        TempFrame := FBrowser.MainFrame;

      if (TempFrame <> nil) and TempFrame.IsValid then TempFrame.LoadUrl(aURL);
    end;
end;

// Leave aFrameName empty to load the URL in the main frame
procedure TFMXChromium.LoadString(const aHTML : ustring; const aFrameName : ustring);
var
  TempFrame : ICefFrame;
begin
  if Initialized and (length(aHTML) > 0) then
    begin
      if (length(aFrameName) > 0) then
        TempFrame := FBrowser.GetFrame(aFrameName)
       else
        TempFrame := FBrowser.MainFrame;

      if (TempFrame <> nil) and TempFrame.IsValid then
        TempFrame.LoadUrl(CefGetDataURI(aHTML, 'text/html'));
    end;
end;

procedure TFMXChromium.LoadString(const aHTML : ustring; const aFrame : ICefFrame);
begin
  if Initialized and (length(aHTML) > 0) and (aFrame <> nil) and aFrame.IsValid then
    aFrame.LoadUrl(CefGetDataURI(aHTML, 'text/html'));
end;

procedure TFMXChromium.LoadString(const aHTML : ustring; const aFrameIdentifier : int64);
var
  TempFrame : ICefFrame;
begin
  if Initialized and (length(aHTML) > 0) then
    begin
      if (aFrameIdentifier <> 0) then
        TempFrame := FBrowser.GetFrameByident(aFrameIdentifier)
       else
        TempFrame := FBrowser.MainFrame;

      if (TempFrame <> nil) and TempFrame.IsValid then
        TempFrame.LoadUrl(CefGetDataURI(aHTML, 'text/html'));
    end;
end;

// Leave aFrameName empty to load the URL in the main frame
procedure TFMXChromium.LoadResource(const aStream : TCustomMemoryStream; const aMimeType, aCharset : string; const aFrameName : ustring);
var
  TempFrame : ICefFrame;
begin
  if Initialized and (aStream <> nil) and (aStream.Size > 0) then
    begin
      if (length(aFrameName) > 0) then
        TempFrame := FBrowser.GetFrame(aFrameName)
       else
        TempFrame := FBrowser.MainFrame;

      if (TempFrame <> nil) and TempFrame.IsValid then
        TempFrame.LoadUrl(CefGetDataURI(aStream.Memory, aStream.Size, aMimeType, aCharset));
    end;
end;

procedure TFMXChromium.LoadResource(const aStream : TCustomMemoryStream; const aMimeType, aCharset : string; const aFrame : ICefFrame);
begin
  if Initialized and (aStream <> nil) and (aStream.Size > 0) and (aFrame <> nil) and aFrame.IsValid then
    aFrame.LoadUrl(CefGetDataURI(aStream.Memory, aStream.Size, aMimeType, aCharset));
end;

procedure TFMXChromium.LoadResource(const aStream : TCustomMemoryStream; const aMimeType, aCharset : string; const aFrameIdentifier : int64);
var
  TempFrame : ICefFrame;
begin
  if Initialized and (aStream <> nil) and (aStream.Size > 0) then
    begin
      if (aFrameIdentifier <> 0) then
        TempFrame := FBrowser.GetFrameByident(aFrameIdentifier)
       else
        TempFrame := FBrowser.MainFrame;

      if (TempFrame <> nil) and TempFrame.IsValid then
        TempFrame.LoadUrl(CefGetDataURI(aStream.Memory, aStream.Size, aMimeType, aCharset));
    end;
end;

// WARNING: This function will fail with "bad IPC message" reason
// INVALID_INITIATOR_ORIGIN (213) unless you first navigate to the request
// origin using some other mechanism (LoadURL, link click, etc).
procedure TFMXChromium.LoadRequest(const aRequest: ICefRequest);
var
  TempFrame : ICefFrame;
begin
  if Initialized then
    begin
      TempFrame := FBrowser.MainFrame;
      if (TempFrame <> nil) and TempFrame.IsValid then TempFrame.LoadRequest(aRequest);
    end;
end;

procedure TFMXChromium.GoBack;
begin
  if Initialized and CanGoBack then FBrowser.GoBack;
end;

procedure TFMXChromium.GoForward;
begin
  if Initialized and CanGoForward then FBrowser.GoForward;
end;

procedure TFMXChromium.Reload;
begin
  if Initialized then FBrowser.Reload;
end;

procedure TFMXChromium.ReloadIgnoreCache;
begin
  if Initialized then FBrowser.ReloadIgnoreCache;
end;

procedure TFMXChromium.StopLoad;
begin
  if Initialized then FBrowser.StopLoad;
end;

procedure TFMXChromium.StartDownload(const aURL : ustring);
begin
  if Initialized then FBrowser.Host.StartDownload(aURL);
end;

// Use the OnDownloadImageFinished event to receive the image
procedure TFMXChromium.DownloadImage(const imageUrl     : ustring;
                                           isFavicon    : boolean;
                                           maxImageSize : cardinal;
                                           bypassCache  : boolean);
var
  TempCallback : ICefDownloadImageCallback;
begin
  try
    if Initialized and (FBrowser.Host <> nil) then
      begin
        TempCallback := TCefCustomDownloadImageCallback.Create(self);
        FBrowser.Host.DownloadImage(imageUrl, isFavicon, maxImageSize, bypassCache, TempCallback);
      end;
  finally
    TempCallback := nil;
  end;
end;

function TFMXChromium.GetIsLoading : boolean;
begin
  Result := Initialized and FBrowser.IsLoading;
end;

function TFMXChromium.GetMultithreadApp : boolean;
begin
  Result := (GlobalCEFApp <> nil) and GlobalCEFApp.MultiThreadedMessageLoop;
end;

function TFMXChromium.GetHasDocument : boolean;
begin
  Result := Initialized and FBrowser.HasDocument;
end;

function TFMXChromium.GetHasView : boolean;
begin
  Result := Initialized and FBrowser.Host.HasView;
end;

function TFMXChromium.GetHasDevTools : boolean;
begin
  Result := Initialized and FBrowser.Host.HasDevTools;
end;

function TFMXChromium.GetHasClientHandler : boolean;
begin
  Result := (FHandler <> nil);
end;

function TFMXChromium.GetHasBrowser : boolean;
begin
  Result := (FBrowser <> nil);
end;

function TFMXChromium.GetWindowHandle : THandle;
begin
  if Initialized then
    Result := FBrowser.Host.WindowHandle
   else
    Result := 0;
end;

function TFMXChromium.GetFrameIsFocused : boolean;
begin
  Result := Initialized and (FBrowser.FocusedFrame <> nil);
end;

function TFMXChromium.GetWindowlessFrameRate : integer;
begin
  if Initialized then
    Result := FBrowser.Host.GetWindowlessFrameRate
   else
    Result := 0;
end;

function TFMXChromium.GetVisibleNavigationEntry : ICefNavigationEntry;
begin
  if Initialized then
    Result := FBrowser.Host.VisibleNavigationEntry
   else
    Result := nil;
end;

function TFMXChromium.GetHasValidMainFrame : boolean;
begin
  Result := Initialized and (FBrowser.MainFrame <> nil) and FBrowser.MainFrame.IsValid;
end;

function TFMXChromium.GetFrameCount : NativeUInt;
begin
  if Initialized then
    Result := FBrowser.GetFrameCount
   else
    Result := 0;
end;

function TFMXChromium.GetRequestContextCache : ustring;
begin
  if Initialized then
    Result := FBrowser.host.RequestContext.CachePath
   else
    if (GlobalCEFApp <> nil) then
      Result := GlobalCEFApp.cache
     else
      Result := '';
end;

function TFMXChromium.GetRequestContextIsGlobal : boolean;
begin
  Result := Initialized and FBrowser.host.RequestContext.IsGlobal;
end;

function TFMXChromium.GetAudioMuted : boolean;
begin
  Result := Initialized and FBrowser.host.IsAudioMuted;
end;

procedure TFMXChromium.SetAudioMuted(aValue : boolean);
begin
  if Initialized then FBrowser.Host.SetAudioMuted(aValue);
end;

procedure TFMXChromium.SetWindowlessFrameRate(aValue : integer);
begin
  if Initialized then FBrowser.Host.SetWindowlessFrameRate(aValue);
end;

function TFMXChromium.GetCanGoBack : boolean;
begin
  Result := Initialized and FBrowser.CanGoBack;
end;

function TFMXChromium.GetCanGoForward : boolean;
begin
  Result := Initialized and FBrowser.CanGoForward;
end;

function TFMXChromium.GetIsPopUp : boolean;
begin
  Result := Initialized and FBrowser.IsPopUp;
end;

function TFMXChromium.GetInitialized : boolean;
begin
  Result := FInitialized and not(FClosing) and (FBrowser <> nil);
end;

function TFMXChromium.GetDocumentURL : ustring;
var
  TempFrame : ICefFrame;
begin
  Result := '';

  if Initialized then
    begin
      TempFrame := FBrowser.MainFrame;
      if (TempFrame <> nil) and TempFrame.IsValid then Result := TempFrame.URL;
    end;
end;

function TFMXChromium.GetZoomLevel : double;
begin
  Result := 0;

  if Initialized then Result := FBrowser.Host.ZoomLevel;
end;

procedure TFMXChromium.SetZoomLevel(const aValue : double);
begin
  if Initialized then FBrowser.Host.ZoomLevel := aValue;
end;

function TFMXChromium.GetZoomPct : double;
begin
  Result := power(1.2, ZoomLevel) * 100;
end;

procedure TFMXChromium.SetZoomPct(const aValue : double);
begin
  if Initialized and (aValue > 0) then ZoomLevel := LogN(1.2, aValue / 100);
end;

procedure TFMXChromium.ApplyZoomStep;
begin
  case FZoomStep of
    ZOOM_STEP_25  : ZoomPct := 25;
    ZOOM_STEP_33  : ZoomPct := 33;
    ZOOM_STEP_50  : ZoomPct := 50;
    ZOOM_STEP_67  : ZoomPct := 67;
    ZOOM_STEP_75  : ZoomPct := 75;
    ZOOM_STEP_90  : ZoomPct := 90;
    ZOOM_STEP_100 : ZoomPct := 100;
    ZOOM_STEP_110 : ZoomPct := 110;
    ZOOM_STEP_125 : ZoomPct := 125;
    ZOOM_STEP_150 : ZoomPct := 150;
    ZOOM_STEP_175 : ZoomPct := 175;
    ZOOM_STEP_200 : ZoomPct := 200;
    ZOOM_STEP_250 : ZoomPct := 250;
    ZOOM_STEP_300 : ZoomPct := 300;
    ZOOM_STEP_400 : ZoomPct := 400;
    ZOOM_STEP_500 : ZoomPct := 500;
  end;
end;

procedure TFMXChromium.SetZoomStep(aValue : byte);
begin
  if Initialized and (aValue in [ZOOM_STEP_MIN..ZOOM_STEP_MAX]) then
    begin
      FZoomStep := aValue;
      ApplyZoomStep;
    end;
end;

procedure TFMXChromium.IncZoomStep;
begin
  if Initialized and (FZoomStep < ZOOM_STEP_MAX) then
    begin
      inc(FZoomStep);
      ApplyZoomStep;
    end;
end;

procedure TFMXChromium.DecZoomStep;
begin
  if Initialized and (FZoomStep > ZOOM_STEP_MIN) then
    begin
      dec(FZoomStep);
      ApplyZoomStep;
    end;
end;

procedure TFMXChromium.ResetZoomStep;
begin
  ZoomStep := ZOOM_STEP_DEF;
end;

procedure TFMXChromium.SetDoNotTrack(aValue : boolean);
begin
  if (FDoNotTrack <> aValue) then
    begin
      FDoNotTrack        := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TFMXChromium.SetSendReferrer(aValue : boolean);
begin
  if (FSendReferrer <> aValue) then
    begin
      FSendReferrer      := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TFMXChromium.SetHyperlinkAuditing(aValue : boolean);
begin
  if (FHyperlinkAuditing <> aValue) then
    begin
      FHyperlinkAuditing := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TFMXChromium.SetRunAllFlashInAllowMode(aValue : boolean);
begin
  if (FRunAllFlashInAllowMode <> aValue) then
    begin
      FRunAllFlashInAllowMode := aValue;
      FUpdatePreferences      := True;
    end;
end;

procedure TFMXChromium.SetAllowOutdatedPlugins(aValue : boolean);
begin
  if (FAllowOutdatedPlugins <> aValue) then
    begin
      FAllowOutdatedPlugins := aValue;
      FUpdatePreferences    := True;
    end;
end;

procedure TFMXChromium.SetAlwaysAuthorizePlugins(aValue : boolean);
begin
  if (FAlwaysAuthorizePlugins <> aValue) then
    begin
      FAlwaysAuthorizePlugins := aValue;
      FUpdatePreferences      := True;
    end;
end;

procedure TFMXChromium.SetSpellChecking(aValue : boolean);
begin
  if (FSpellChecking <> aValue) then
    begin
      FSpellChecking     := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TFMXChromium.SetSpellCheckerDicts(const aValue : ustring);
begin
  if (FSpellCheckerDicts <> aValue) then
    begin
      FSpellCheckerDicts := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TFMXChromium.SetSafeSearch(aValue : boolean);
begin
  if (FSafeSearch <> aValue) then
    begin
      FSafeSearch        := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TFMXChromium.SetYouTubeRestrict(aValue : integer);
begin
  if (FYouTubeRestrict <> aValue) then
    begin
      FYouTubeRestrict   := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TFMXChromium.SetPrintingEnabled(aValue : boolean);
begin
  if (FPrintingEnabled <> aValue) then
    begin
      FPrintingEnabled   := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TFMXChromium.SetOnRequestContextInitialized(const aValue : TOnRequestContextInitialized);
begin
  FOnRequestContextInitialized := aValue;

  CreateReqContextHandler;
end;

procedure TFMXChromium.SetOnBeforePluginLoad(const aValue : TOnBeforePluginLoad);
begin
  FOnBeforePluginLoad := aValue;

  CreateReqContextHandler;
end;

procedure TFMXChromium.SetWebRTCIPHandlingPolicy(aValue : TCefWebRTCHandlingPolicy);
begin
  if (FWebRTCIPHandlingPolicy <> aValue) then
    begin
      FWebRTCIPHandlingPolicy := aValue;
      FUpdatePreferences      := True;
    end;
end;

procedure TFMXChromium.SetWebRTCMultipleRoutes(aValue : TCefState);
begin
  if (FWebRTCMultipleRoutes <> aValue) then
    begin
      FWebRTCMultipleRoutes := aValue;
      FUpdatePreferences    := True;
    end;
end;

procedure TFMXChromium.SetWebRTCNonProxiedUDP(aValue : TCefState);
begin
  if (FWebRTCNonProxiedUDP <> aValue) then
    begin
      FWebRTCNonProxiedUDP := aValue;
      FUpdatePreferences   := True;
    end;
end;

procedure TFMXChromium.SetProxyType(aValue : integer);
begin
  if (FProxyType <> aValue) then
    begin
      FProxyType         := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TFMXChromium.SetProxyScheme(aValue : TCefProxyScheme);
begin
  if (FProxyScheme <> aValue) then
    begin
      FProxyScheme       := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TFMXChromium.SetProxyServer(const aValue : ustring);
begin
  if (FProxyServer <> aValue) then
    begin
      FProxyServer       := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TFMXChromium.SetProxyPort(aValue : integer);
begin
  if (FProxyPort <> aValue) then
    begin
      FProxyPort         := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TFMXChromium.SetProxyUsername(const aValue : ustring);
begin
  if (FProxyUsername <> aValue) then
    begin
      FProxyUsername     := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TFMXChromium.SetProxyPassword(const aValue : ustring);
begin
  if (FProxyPassword <> aValue) then
    begin
      FProxyPassword     := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TFMXChromium.SetProxyScriptURL(const aValue : ustring);
begin
  if (FProxyScriptURL <> aValue) then
    begin
      FProxyScriptURL    := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TFMXChromium.SetProxyByPassList(const aValue : ustring);
begin
  if (FProxyByPassList <> aValue) then
    begin
      FProxyByPassList   := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TFMXChromium.SetMaxConnectionsPerProxy(const aValue : integer);
begin
  if (FMaxConnectionsPerProxy <> aValue) and
     (aValue in [CEF_MAX_CONNECTIONS_PER_PROXY_MIN_VALUE..CEF_MAX_CONNECTIONS_PER_PROXY_MAX_VALUE]) then
    begin
      FMaxConnectionsPerProxy := aValue;
      FUpdatePreferences      := True;
    end;
end;

procedure TFMXChromium.SetCustomHeaderName(const aValue : ustring);
begin
  if (FCustomHeaderName <> aValue) then
    begin
      FCustomHeaderName := aValue;
      FAddCustomHeader  := (length(FCustomHeaderName) > 0) and (length(FCustomHeaderValue) > 0);
    end;
end;

procedure TFMXChromium.SetCustomHeaderValue(const aValue : ustring);
begin
  if (FCustomHeaderValue <> aValue) then
    begin
      FCustomHeaderValue := aValue;
      FAddCustomHeader   := (length(FCustomHeaderName) > 0) and (length(FCustomHeaderValue) > 0);
    end;
end;

// If aDeleteImmediately is false TChromium.DeleteCookies triggers the TChromium.OnCookiesDeleted
// event when the cookies are deleted.
function TFMXChromium.DeleteCookies(const url, cookieName: ustring; aDeleteImmediately : boolean) : boolean;
var
  TempManager  : ICefCookieManager;
  TempCallback : ICefDeleteCookiesCallback;
begin
  Result := False;

  if Initialized and (FBrowser.Host <> nil) and (FBrowser.Host.RequestContext <> nil) then
    begin
      TempManager := FBrowser.Host.RequestContext.GetCookieManager(nil);

      if (TempManager <> nil) then
        try
          if aDeleteImmediately then
            TempCallBack := nil
           else
            TempCallback := TCefCustomDeleteCookiesCallback.Create(self);

          Result := TempManager.DeleteCookies(url, cookieName, TempCallback);
        finally
          TempCallback := nil;
        end;
    end;
end;

// TFMXChromium.VisitAllCookies triggers the TFMXChromium.OnCookiesVisited event for each cookie
// aID is an optional parameter to identify which VisitAllCookies call has triggered the
// OnCookiesVisited event.
function TFMXChromium.VisitAllCookies(aID : integer) : boolean;
var
  TempManager : ICefCookieManager;
  TempVisitor : ICefCookieVisitor;
begin
  Result := False;

  if Initialized and (FBrowser.Host <> nil) and (FBrowser.Host.RequestContext <> nil) then
    begin
      TempManager := FBrowser.Host.RequestContext.GetCookieManager(nil);

      if (TempManager <> nil) then
        try
          TempVisitor := TCefCustomCookieVisitor.Create(self, aID);
          Result      := TempManager.VisitAllCookies(TempVisitor);
        finally
          TempVisitor := nil;
        end;
    end;
end;

// TFMXChromium.VisitURLCookies triggers the TFMXChromium.OnCookiesVisited event for each cookie
// aID is an optional parameter to identify which VisitURLCookies call has triggered the
// OnCookiesVisited event.
function TFMXChromium.VisitURLCookies(const url : ustring; includeHttpOnly : boolean; aID : integer) : boolean;
var
  TempManager : ICefCookieManager;
  TempVisitor : ICefCookieVisitor;
begin
  Result := False;

  if Initialized and (FBrowser.Host <> nil) and (FBrowser.Host.RequestContext <> nil) then
    begin
      TempManager := FBrowser.Host.RequestContext.GetCookieManager(nil);

      if (TempManager <> nil) then
        try
          TempVisitor := TCefCustomCookieVisitor.Create(self, aID);
          Result      := TempManager.VisitUrlCookies(url, includeHttpOnly, TempVisitor);
        finally
          TempVisitor := nil;
        end;
    end;
end;

// TFMXChromium.SetCookie triggers the TFMXChromium.OnCookieSet event when the cookie has been set
// aID is an optional parameter to identify which SetCookie call has triggered the
// OnCookieSet event.
function TFMXChromium.SetCookie(const url, name, value, domain, path: ustring;
                                      secure, httponly, hasExpires: Boolean;
                                const creation, lastAccess, expires: TDateTime;
                                      aSetImmediately : boolean;
                                      aID : integer): Boolean;
var
  TempManager  : ICefCookieManager;
  TempCallback : ICefSetCookieCallback;
begin
  Result := False;

  if Initialized and (FBrowser.Host <> nil) and (FBrowser.Host.RequestContext <> nil) then
    begin
      TempManager := FBrowser.Host.RequestContext.GetCookieManager(nil);

      if (TempManager <> nil) then
        try
          if aSetImmediately then
            TempCallback := nil
           else
            TempCallback := TCefCustomSetCookieCallback.Create(self, aID);

          Result := TempManager.SetCookie(url, name, value, domain, path,
                                          secure, httponly, hasExpires,
                                          creation, lastAccess, expires,
                                          TempCallback);
        finally
          TempCallback := nil;
        end;
    end;
end;

// If aFlushImmediately is false then OnCookiesFlushed is triggered when the cookies are flushed
function TFMXChromium.FlushCookieStore(aFlushImmediately : boolean) : boolean;
var
  TempManager  : ICefCookieManager;
  TempCallback : ICefCompletionCallback;
begin
  Result := False;

  if Initialized and (FBrowser.Host <> nil) and (FBrowser.Host.RequestContext <> nil) then
    begin
      TempManager := FBrowser.Host.RequestContext.GetCookieManager(nil);

      if (TempManager <> nil) then
        try
          if aFlushImmediately then
            TempCallback := nil
           else
            TempCallback := TCefFlushStoreCompletionCallback.Create(self);

          Result := TempManager.FlushStore(TempCallback);
        finally
          TempCallback := nil;
        end;
    end;
end;

// If aClearImmediately is false then OnCertificateExceptionsCleared is triggered when the exceptions are cleared
function TFMXChromium.ClearCertificateExceptions(aClearImmediately : boolean) : boolean;
var
  TempCallback : ICefCompletionCallback;
begin
  Result := False;

  if Initialized and (FBrowser.Host <> nil) and (FBrowser.Host.RequestContext <> nil) then
    try
      if aClearImmediately then
        TempCallback := nil
       else
        TempCallback := TCefClearCertificateExceptionsCompletionCallback.Create(self);

      FBrowser.Host.RequestContext.ClearCertificateExceptions(TempCallback);
      Result := True;
    finally
      TempCallback := nil;
    end;
end;

// If aClearImmediately is false then OnHttpAuthCredentialsCleared is triggered when the credeintials are cleared
function TFMXChromium.ClearHttpAuthCredentials(aClearImmediately : boolean) : boolean;
var
  TempCallback : ICefCompletionCallback;
begin
  Result := False;

  if Initialized and (FBrowser.Host <> nil) and (FBrowser.Host.RequestContext <> nil) then
    try
      if aClearImmediately then
        TempCallback := nil
       else
        TempCallback := TCefClearHttpAuthCredentialsCompletionCallback.Create(self);

      FBrowser.Host.RequestContext.ClearHttpAuthCredentials(TempCallback);
      Result := True;
    finally
      TempCallback := nil;
    end;
end;

// If aCloseImmediately is false then OnAllConnectionsClosed is triggered when the connections are closed
function TFMXChromium.CloseAllConnections(aCloseImmediately : boolean) : boolean;
var
  TempCallback : ICefCompletionCallback;
begin
  Result := False;

  if Initialized and (FBrowser.Host <> nil) and (FBrowser.Host.RequestContext <> nil) then
    try
      if aCloseImmediately then
        TempCallback := nil
       else
        TempCallback := TCefCloseAllConnectionsCompletionCallback.Create(self);

      FBrowser.Host.RequestContext.CloseAllConnections(TempCallback);
      Result := True;
    finally
      TempCallback := nil;
    end;
end;

// Leave aFrameName empty to get the HTML source from the main frame
procedure TFMXChromium.RetrieveHTML(const aFrameName : ustring);
var
  TempFrame   : ICefFrame;
  TempVisitor : ICefStringVisitor;
begin
  if Initialized then
    begin
      if (length(aFrameName) > 0) then
        TempFrame := FBrowser.GetFrame(aFrameName)
       else
        TempFrame := FBrowser.MainFrame;

      if (TempFrame <> nil) and TempFrame.IsValid then
        try
          TempVisitor := TCustomCefStringVisitor.Create(self);
          TempFrame.GetSource(TempVisitor);
        finally
          TempVisitor := nil;
        end;
    end;
end;

procedure TFMXChromium.RetrieveHTML(const aFrame : ICefFrame);
var
  TempVisitor : ICefStringVisitor;
begin
  if Initialized and (aFrame <> nil) and aFrame.IsValid then
    try
      TempVisitor := TCustomCefStringVisitor.Create(self);
      aFrame.GetSource(TempVisitor);
    finally
      TempVisitor := nil;
    end;
end;

procedure TFMXChromium.RetrieveHTML(const aFrameIdentifier : int64);
var
  TempFrame   : ICefFrame;
  TempVisitor : ICefStringVisitor;
begin
  if Initialized then
    begin
      if (aFrameIdentifier <> 0) then
        TempFrame := FBrowser.GetFrameByident(aFrameIdentifier)
       else
        TempFrame := FBrowser.MainFrame;

      if (TempFrame <> nil) and TempFrame.IsValid then
        try
          TempVisitor := TCustomCefStringVisitor.Create(self);
          TempFrame.GetSource(TempVisitor);
        finally
          TempVisitor := nil;
        end;
    end;
end;

// Leave aFrameName empty to get the HTML source from the main frame
procedure TFMXChromium.RetrieveText(const aFrameName : ustring);
var
  TempFrame   : ICefFrame;
  TempVisitor : ICefStringVisitor;
begin
  if Initialized then
    begin
      if (length(aFrameName) > 0) then
        TempFrame := FBrowser.GetFrame(aFrameName)
       else
        TempFrame := FBrowser.MainFrame;

      if (TempFrame <> nil) and TempFrame.IsValid then
        try
          TempVisitor := TCustomCefStringVisitor.Create(self);
          TempFrame.GetText(TempVisitor);
        finally
          TempVisitor := nil;
        end;
    end;
end;

procedure TFMXChromium.RetrieveText(const aFrame : ICefFrame);
var
  TempVisitor : ICefStringVisitor;
begin
  if Initialized and (aFrame <> nil) and aFrame.IsValid then
    try
      TempVisitor := TCustomCefStringVisitor.Create(self);
      aFrame.GetText(TempVisitor);
    finally
      TempVisitor := nil;
    end;
end;

procedure TFMXChromium.RetrieveText(const aFrameIdentifier : int64);
var
  TempFrame   : ICefFrame;
  TempVisitor : ICefStringVisitor;
begin
  if Initialized then
    begin
      if (aFrameIdentifier <> 0) then
        TempFrame := FBrowser.GetFrameByident(aFrameIdentifier)
       else
        TempFrame := FBrowser.MainFrame;

      if (TempFrame <> nil) and TempFrame.IsValid then
        try
          TempVisitor := TCustomCefStringVisitor.Create(self);
          TempFrame.GetText(TempVisitor);
        finally
          TempVisitor := nil;
        end;
    end;
end;

procedure TFMXChromium.GetNavigationEntries(currentOnly: Boolean);
var
  TempVisitor : ICefNavigationEntryVisitor;
begin
  if Initialized then
    try
      TempVisitor := TCustomCefNavigationEntryVisitor.Create(self);
      FBrowser.Host.GetNavigationEntries(TempVisitor, currentOnly);
    finally
      TempVisitor := nil;
    end;
end;

function TFMXChromium.GetFrameNames(var aFrameNames : TStrings) : boolean;
begin
  Result := Initialized and FBrowser.GetFrameNames(aFrameNames);
end;

function TFMXChromium.GetFrameIdentifiers(var aFrameCount : NativeUInt; var aFrameIdentifierArray : TCefFrameIdentifierArray) : boolean;
begin
  Result := Initialized and FBrowser.GetFrameIdentifiers(aFrameCount, aFrameIdentifierArray);
end;

procedure TFMXChromium.UpdatePreferences;
var
  TempTask: ICefTask;
begin
  if Initialized then
    try
      TempTask := TCefUpdatePrefsTask.Create(self);
      CefPostTask(TID_UI, TempTask);
    finally
      TempTask := nil;
    end;
end;

procedure TFMXChromium.SavePreferences(const aFileName : string);
var
  TempTask: ICefTask;
begin
  if Initialized and (length(aFileName) > 0) then
    try
      FPrefsFileName := aFileName;
      TempTask       := TCefSavePrefsTask.Create(self);
      CefPostTask(TID_UI, TempTask);
    finally
      TempTask := nil;
    end;
end;

procedure TFMXChromium.ResolveHost(const aURL : ustring);
var
  TempCallback : ICefResolveCallback;
begin
  // Results will be received in the OnResolvedHostAvailable event of this class
  if Initialized and (length(aURL) > 0) then
    try
      TempCallback := TCefCustomResolveCallback.Create(self);
      FBrowser.Host.RequestContext.ResolveHost(aURL, TempCallback);
    finally
      TempCallback := nil;
    end;
end;

function TFMXChromium.IsSameBrowser(const aBrowser : ICefBrowser) : boolean;
begin
  Result := Initialized and (aBrowser <> nil) and FBrowser.IsSame(aBrowser);
end;

// Calling ExecuteTaskOnCefThread function will trigger the TChromium.OnExecuteTaskOnCefThread event.
// "aCefThreadId" indicates the CEF thread on which TChromium.OnExecuteTaskOnCefThread will be executed.
// "aTaskID" is a custom ID used to identify the task that triggered the TChromium.OnExecuteTaskOnCefThread event.
// "aDelayMs" is an optional delay in milliseconds to trigger the TChromium.OnExecuteTaskOnCefThread event.
function TFMXChromium.ExecuteTaskOnCefThread(aCefThreadId : TCefThreadId; aTaskID : cardinal; aDelayMs : Int64) : boolean;
var
  TempTask : ICefTask;
begin
  Result := False;

  try
    if Initialized then
      begin
        TempTask := TCefGenericTask.Create(self, aTaskID);

        if (aDelayMs <> 0) then
          Result := CefPostDelayedTask(aCefThreadId, TempTask, aDelayMs)
         else
          Result := CefPostTask(aCefThreadId, TempTask);
      end;
  finally
    TempTask := nil;
  end;
end;

procedure TFMXChromium.SimulateMouseWheel(aDeltaX, aDeltaY : integer);
var
  TempEvent : TCefMouseEvent;
begin
  if Initialized then
    begin
      TempEvent.x         := 0;
      TempEvent.y         := 0;
      TempEvent.modifiers := EVENTFLAG_NONE;
      FBrowser.Host.SendMouseWheelEvent(@TempEvent, aDeltaX, aDeltaY);
    end;
end;

procedure TFMXChromium.doUpdatePreferences(const aBrowser: ICefBrowser);
begin
  FUpdatePreferences := False;

  // The preferences registered in CEF are defined in :
  // /libcef/browser/prefs/browser_prefs.cc

  UpdateProxyPrefs(aBrowser);
  UpdatePreference(aBrowser, 'enable_do_not_track',                  FDoNotTrack);
  UpdatePreference(aBrowser, 'enable_referrers',                     FSendReferrer);
  UpdatePreference(aBrowser, 'enable_a_ping',                        FHyperlinkAuditing);
  UpdatePreference(aBrowser, 'plugins.run_all_flash_in_allow_mode',  FRunAllFlashInAllowMode);
  UpdatePreference(aBrowser, 'plugins.allow_outdated',               FAllowOutdatedPlugins);
  UpdatePreference(aBrowser, 'plugins.always_authorize',             FAlwaysAuthorizePlugins);
  UpdatePreference(aBrowser, 'browser.enable_spellchecking',         FSpellChecking);
  UpdateStringListPref(aBrowser, 'spellcheck.dictionaries',          FSpellCheckerDicts);
  UpdatePreference(aBrowser, 'settings.force_google_safesearch',     FSafeSearch);
  UpdatePreference(aBrowser, 'settings.force_youtube_restrict',      FYouTubeRestrict);
  UpdatePreference(aBrowser, 'printing.enabled',                     FPrintingEnabled);

  if FRunAllFlashInAllowMode then
    UpdatePreference(aBrowser, 'profile.default_content_setting_values.plugins', 1);

  case FWebRTCIPHandlingPolicy of
    hpDefaultPublicAndPrivateInterfaces :
      UpdatePreference(aBrowser, 'webrtc.ip_handling_policy', 'default_public_and_private_interfaces');

    hpDefaultPublicInterfaceOnly :
      UpdatePreference(aBrowser, 'webrtc.ip_handling_policy', 'default_public_interface_only');

    hpDisableNonProxiedUDP :
      UpdatePreference(aBrowser, 'webrtc.ip_handling_policy', 'disable_non_proxied_udp');
  end;

  if (FWebRTCMultipleRoutes <> STATE_DEFAULT) then
    UpdatePreference(aBrowser, 'webrtc.multiple_routes_enabled', (FWebRTCMultipleRoutes = STATE_ENABLED));

  if (FWebRTCNonProxiedUDP <> STATE_DEFAULT) then
    UpdatePreference(aBrowser, 'webrtc.nonproxied_udp_enabled', (FWebRTCNonProxiedUDP = STATE_ENABLED));
end;

procedure TFMXChromium.doUpdateOwnPreferences;
begin
  if Initialized then doUpdatePreferences(FBrowser);
end;

function TFMXChromium.UpdateProxyPrefs(const aBrowser: ICefBrowser) : boolean;
var
  TempError : ustring;
  TempProxy : ICefValue;
  TempValue : ICefValue;
  TempDict  : ICefDictionaryValue;
begin
  Result := False;

  try
    try
      if (aBrowser      <> nil) and
         (aBrowser.Host <> nil) and
         aBrowser.Host.RequestContext.CanSetPreference('proxy') then
        begin
          TempProxy := TCefValueRef.New;
          TempValue := TCefValueRef.New;
          TempDict  := TCefDictionaryValueRef.New;

          case FProxyType of
            CEF_PROXYTYPE_AUTODETECT :
              begin
                TempValue.SetString('auto_detect');
                TempDict.SetValue('mode', TempValue);
              end;

            CEF_PROXYTYPE_SYSTEM :
              begin
                TempValue.SetString('system');
                TempDict.SetValue('mode', TempValue);
              end;

            CEF_PROXYTYPE_FIXED_SERVERS :
              begin
                TempValue.SetString('fixed_servers');
                TempDict.SetValue('mode', TempValue);

                case FProxyScheme of
                  psSOCKS4 : TempDict.SetString('server', 'socks4://' + FProxyServer + ':' + inttostr(FProxyPort));
                  psSOCKS5 : TempDict.SetString('server', 'socks5://' + FProxyServer + ':' + inttostr(FProxyPort));
                  else       TempDict.SetString('server', FProxyServer + ':' + inttostr(FProxyPort));
                end;

                if (length(FProxyByPassList) > 0) then TempDict.SetString('bypass_list', FProxyByPassList);
              end;

            CEF_PROXYTYPE_PAC_SCRIPT :
              begin
                TempValue.SetString('pac_script');
                TempDict.SetValue('mode', TempValue);
                TempDict.SetString('pac_url', FProxyScriptURL);
              end;

            else    // CEF_PROXYTYPE_DIRECT
              begin
                TempValue.SetString('direct');
                TempDict.SetValue('mode', TempValue);
              end;
          end;

          Result := TempProxy.SetDictionary(TempDict) and
                    aBrowser.Host.RequestContext.SetPreference('proxy', TempProxy, TempError);

          if not(Result) then
            OutputDebugMessage('TFMXChromium.UpdateProxyPrefs error : ' + quotedstr(TempError));
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TFMXChromium.UpdateProxyPrefs', e) then raise;
    end;
  finally
    TempProxy := nil;
    TempValue := nil;
    TempDict  := nil;
  end;
end;

function TFMXChromium.UpdatePreference(const aBrowser: ICefBrowser; const aName : ustring; aValue : boolean) : boolean;
var
  TempError : ustring;
  TempValue : ICefValue;
begin
  Result := False;

  try
    try
      if (aBrowser      <> nil) and
         (aBrowser.Host <> nil) and
         aBrowser.Host.RequestContext.CanSetPreference(aName) then
        begin
          TempValue := TCefValueRef.New;

          if aValue then
            TempValue.SetBool(1)
           else
            TempValue.SetBool(0);

          Result := aBrowser.Host.RequestContext.SetPreference(aName, TempValue, TempError);

          if not(Result) then
            OutputDebugMessage('TFMXChromium.UpdatePreference error : ' + quotedstr(TempError));
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TFMXChromium.UpdatePreference', e) then raise;
    end;
  finally
    TempValue := nil;
  end;
end;

function TFMXChromium.UpdatePreference(const aBrowser: ICefBrowser; const aName : ustring; aValue : integer) : boolean;
var
  TempError : ustring;
  TempValue : ICefValue;
begin
  Result := False;

  try
    try
      if (aBrowser      <> nil) and
         (aBrowser.Host <> nil) and
         aBrowser.Host.RequestContext.CanSetPreference(aName) then
        begin
          TempValue := TCefValueRef.New;
          TempValue.SetInt(aValue);
          Result := aBrowser.Host.RequestContext.SetPreference(aName, TempValue, TempError);

          if not(Result) then
            OutputDebugMessage('TFMXChromium.UpdatePreference error : ' + quotedstr(TempError));
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TFMXChromium.UpdatePreference', e) then raise;
    end;
  finally
    TempValue := nil;
  end;
end;

function TFMXChromium.UpdatePreference(const aBrowser: ICefBrowser; const aName : ustring; const aValue : double) : boolean;
var
  TempError : ustring;
  TempValue : ICefValue;
begin
  Result := False;

  try
    try
      if (aBrowser      <> nil) and
         (aBrowser.Host <> nil) and
         aBrowser.Host.RequestContext.CanSetPreference(aName) then
        begin
          TempValue := TCefValueRef.New;
          TempValue.SetDouble(aValue);
          Result := aBrowser.Host.RequestContext.SetPreference(aName, TempValue, TempError);

          if not(Result) then
            OutputDebugMessage('TFMXChromium.UpdatePreference error : ' + quotedstr(TempError));
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TFMXChromium.UpdatePreference', e) then raise;
    end;
  finally
    TempValue := nil;
  end;
end;

function TFMXChromium.UpdatePreference(const aBrowser: ICefBrowser; const aName, aValue : ustring) : boolean;
var
  TempError : ustring;
  TempValue : ICefValue;
begin
  Result := False;

  try
    try
      if (aBrowser      <> nil) and
         (aBrowser.Host <> nil) and
         aBrowser.Host.RequestContext.CanSetPreference(aName) then
        begin
          TempValue := TCefValueRef.New;
          TempValue.SetString(aValue);
          Result := aBrowser.Host.RequestContext.SetPreference(aName, TempValue, TempError);

          if not(Result) then
            OutputDebugMessage('TFMXChromium.UpdatePreference error : ' + quotedstr(TempError));
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TFMXChromium.UpdatePreference', e) then raise;
    end;
  finally
    TempValue := nil;
  end;
end;

function TFMXChromium.UpdatePreference(const aBrowser: ICefBrowser; const aName : ustring; const aValue : TStringList) : boolean;
var
  TempError : ustring;
  TempValue : ICefValue;
  TempList  : ICefListValue;
  i         : NativeUInt;
  TempSize  : NativeUInt;
begin
  Result := False;

  try
    try
      if (aValue        <> nil) and
         (aValue.Count   > 0)   and
         (aBrowser      <> nil) and
         (aBrowser.Host <> nil) and
         aBrowser.Host.RequestContext.CanSetPreference(aName) then
        begin
          TempSize := aValue.Count;
          TempList := TCefListValueRef.New;

          if TempList.SetSize(TempSize) then
            begin
              i := 0;
              while (i < TempSize) do
                begin
                  TempList.SetString(i, aValue[i]);
                  inc(i);
                end;

              TempValue := TCefValueRef.New;
              Result    := TempValue.SetList(TempList) and
                           aBrowser.Host.RequestContext.SetPreference(aName, TempValue, TempError);

              if not(Result) then
                OutputDebugMessage('TFMXChromium.UpdatePreference error : ' + quotedstr(TempError));
            end;
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TFMXChromium.UpdatePreference', e) then raise;
    end;
  finally
    TempValue := nil;
    TempList  := nil;
  end;
end;

function TFMXChromium.UpdateStringListPref(const aBrowser: ICefBrowser; const aName, aValue : ustring) : boolean;
var
  TempSL : TStringList;
begin
  Result := False;
  TempSL := nil;

  try
    if (length(aName) > 0) and (length(aValue) > 0) then
      begin
        TempSL           := TStringList.Create;
        TempSL.CommaText := aValue;
        Result           := UpdatePreference(aBrowser, aName, TempSL);
      end;
  finally
    if (TempSL <> nil) then FreeAndNil(TempSL);
  end;
end;

procedure TFMXChromium.HandleNull(const aValue : ICefValue; var aResultSL : TStringList; const aRoot, aKey : string);
var
  TempKey : string;
begin
  if (aRoot <> '') then
    TempKey := aRoot + '.' + aKey
   else
    TempKey := aKey;

  if (length(TempKey) > 0) then
    aResultSL.Add(TempKey + ' : -null-')
   else
    aResultSL.Add('-null-');
end;

procedure TFMXChromium.HandleBool(const aValue : ICefValue; var aResultSL : TStringList; const aRoot, aKey : string);
var
  TempKey : string;
begin
  if (aRoot <> '') then
    TempKey := aRoot + '.' + aKey
   else
    TempKey := aKey;

  if (length(TempKey) > 0) then
    aResultSL.Add(TempKey + ' : ' + BoolToStr(aValue.GetBool, true))
   else
    aResultSL.Add(BoolToStr(aValue.GetBool, true));
end;

procedure TFMXChromium.HandleInteger(const aValue : ICefValue; var aResultSL : TStringList; const aRoot, aKey : string);
var
  TempKey : string;
begin
  if (aRoot <> '') then
    TempKey := aRoot + '.' + aKey
   else
    TempKey := aKey;

  if (length(TempKey) > 0) then
    aResultSL.Add(TempKey + ' : ' + IntToStr(aValue.GetInt))
   else
    aResultSL.Add(IntToStr(aValue.GetInt));
end;

procedure TFMXChromium.HandleDouble(const aValue : ICefValue; var aResultSL : TStringList; const aRoot, aKey : string);
var
  TempKey : string;
begin
  if (aRoot <> '') then
    TempKey := aRoot + '.' + aKey
   else
    TempKey := aKey;

  if (length(TempKey) > 0) then
    aResultSL.Add(TempKey + ' : ' + FloatToStr(aValue.GetDouble))
   else
    aResultSL.Add(FloatToStr(aValue.GetDouble));
end;

procedure TFMXChromium.HandleString(const aValue : ICefValue; var aResultSL : TStringList; const aRoot, aKey : string);
var
  TempKey : string;
begin
  if (aRoot <> '') then
    TempKey := aRoot + '.' + aKey
   else
    TempKey := aKey;

  if (length(TempKey) > 0) then
    aResultSL.Add(TempKey + ' : ' + aValue.GetString)
   else
    aResultSL.Add(aValue.GetString);
end;

procedure TFMXChromium.HandleBinary(const aValue : ICefValue; var aResultSL : TStringList; const aRoot, aKey : string);
var
  TempKey : string;
begin
  if (aRoot <> '') then
    TempKey := aRoot + '.' + aKey
   else
    TempKey := aKey;

  if (length(TempKey) > 0) then
    aResultSL.Add(TempKey + ' : -binary-')
   else
    aResultSL.Add('-binary-');
end;

procedure TFMXChromium.HandleList(const aValue : ICefValue; var aResultSL : TStringList; const aRoot, aKey : string);
var
  TempKey, TempResult : string;
  i, j : integer;
  TempList : ICefListValue;
  TempValue : ICefValue;
  TempSL : TStringList;
begin
  if (aRoot <> '') then
    TempKey := aRoot + '.' + aKey
   else
    TempKey := aKey;

  TempList := aValue.GetList;
  TempSL   := TStringList.Create;

  i := 0;
  j := TempList.GetSize;

  TempResult := '(' + inttostr(j) + '){';

  while (i < j) do
    begin
      TempValue := TempList.GetValue(i);

      case TempValue.GetType of
        VTYPE_NULL       : TempResult := TempResult + '-null-,';
        VTYPE_BOOL       : TempResult := TempResult + BoolToStr(TempValue.GetBool, true) + ',';
        VTYPE_INT        : TempResult := TempResult + IntToStr(TempValue.GetInt) + ',';
        VTYPE_DOUBLE     : TempResult := TempResult + FloatToStr(TempValue.GetDouble) + ',';
        VTYPE_STRING     : TempResult := TempResult + TempValue.GetString + ',';
        VTYPE_BINARY     : TempResult := TempResult + '-binary-,';
        VTYPE_DICTIONARY :
          begin
            TempSL.Clear;
            HandleDictionary(TempValue.GetDictionary, TempSL, '');
            TempResult := TempResult + TempSL.CommaText + ',';
          end;

        VTYPE_LIST       :
          begin
            TempSL.Clear;
            HandleList(TempValue, TempSL, '', '');
            TempResult := TempResult + TempSL.CommaText + ',';
          end;

        else TempResult := TempResult + '-invalid-,';
      end;

      inc(i);
    end;

  i := length(TempResult);
  if (i > 0) and (TempResult[i] = ',') then TempResult := copy(TempResult, 1, pred(i));
  TempResult := TempResult + '}';

  if (length(TempKey) > 0) then
    aResultSL.Add(TempKey + ' : ' + TempResult)
   else
    aResultSL.Add(TempResult);

  TempSL.Free;
end;

procedure TFMXChromium.HandleInvalid(const aValue : ICefValue; var aResultSL : TStringList; const aRoot, aKey : string);
var
  TempKey : string;
begin
  if (aRoot <> '') then
    TempKey := aRoot + '.' + aKey
   else
    TempKey := aKey;

  if (length(TempKey) > 0) then
    aResultSL.Add(TempKey + ' : -invalid-')
   else
    aResultSL.Add('-invalid-');
end;

procedure TFMXChromium.HandleDictionary(const aDict : ICefDictionaryValue; var aResultSL : TStringList; const aRoot : string);
var
  TempKeys : TStringList;
  i, j : integer;
  TempValue : ICefValue;
  TempNewKey : string;
begin
  TempKeys := nil;

  try
    try
      if (aDict <> nil) then
        begin
          TempKeys := TStringList.Create;
          aDict.GetKeys(TempKeys);

          i := 0;
          j := TempKeys.Count;

          while (i < j) do
            begin
              TempValue := aDict.GetValue(TempKeys[i]);

              case TempValue.GetType of
                VTYPE_NULL       : HandleNull(TempValue, aResultSL, aRoot, TempKeys[i]);
                VTYPE_BOOL       : HandleBool(TempValue, aResultSL, aRoot, TempKeys[i]);
                VTYPE_INT        : HandleInteger(TempValue, aResultSL, aRoot, TempKeys[i]);
                VTYPE_DOUBLE     : HandleDouble(TempValue, aResultSL, aRoot, TempKeys[i]);
                VTYPE_STRING     : HandleString(TempValue, aResultSL, aRoot, TempKeys[i]);
                VTYPE_BINARY     : HandleBinary(TempValue, aResultSL, aRoot, TempKeys[i]);
                VTYPE_LIST       : HandleList(TempValue, aResultSL, aRoot, TempKeys[i]);
                VTYPE_DICTIONARY :
                  begin
                    if (length(aRoot) > 0) then
                      TempNewKey := aRoot + '.' + TempKeys[i]
                     else
                      TempNewKey := TempKeys[i];

                    HandleDictionary(TempValue.GetDictionary, aResultSL, TempNewKey);
                  end;

                else
                  HandleInvalid(TempValue, aResultSL, aRoot, TempKeys[i]);
              end;

              inc(i);
            end;

        end;
    except
      on e : exception do
        if CustomExceptionHandler('TFMXChromium.HandleDictionary', e) then raise;
    end;
  finally
    if (TempKeys <> nil) then TempKeys.Free;
  end;
end;

function TFMXChromium.doSavePreferences : boolean;
var
  TempDict  : ICefDictionaryValue;
  TempPrefs : TStringList;
begin
  Result    := False;
  TempPrefs := nil;

  try
    try
      if Initialized then
        begin
          TempPrefs := TStringList.Create;
          TempDict  := FBrowser.Host.RequestContext.GetAllPreferences(True);
          HandleDictionary(TempDict, TempPrefs, '');
          TempPrefs.SaveToFile(FPrefsFileName);
          Result    := True;
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TFMXChromium.Internal_SavePreferences', e) then raise;
    end;
  finally
    if (TempPrefs <> nil) then FreeAndNil(TempPrefs);
  end;
end;

procedure TFMXChromium.doResolvedHostAvailable(result: TCefErrorCode; const resolvedIps: TStrings);
begin
  if assigned(FOnResolvedHostAvailable) then FOnResolvedHostAvailable(self, result, resolvedIps);
end;

function TFMXChromium.doNavigationVisitorResultAvailable(const entry   : ICefNavigationEntry;
                                                               current : Boolean;
                                                               index   : Integer;
                                                               total   : Integer) : boolean;
begin
  Result := False;

  if assigned(FOnNavigationVisitorResultAvailable) then
    FOnNavigationVisitorResultAvailable(entry, current, index, total, Result);
end;

procedure TFMXChromium.doDownloadImageFinished(const imageUrl       : ustring;
                                                     httpStatusCode : Integer;
                                               const image          : ICefImage);
begin
  if assigned(FOnDownloadImageFinished) then
    FOnDownloadImageFinished(self, imageUrl, httpStatusCode, image);
end;

procedure TFMXChromium.doOnCookiesStoreFlushed;
begin
  if assigned(FOnCookiesFlushed) then FOnCookiesFlushed(self);
end;

procedure TFMXChromium.doCertificateExceptionsCleared;
begin
  if assigned(FOnCertificateExceptionsCleared) then FOnCertificateExceptionsCleared(self);
end;

procedure TFMXChromium.doHttpAuthCredentialsCleared;
begin
  if assigned(FOnHttpAuthCredentialsCleared) then FOnHttpAuthCredentialsCleared(self);
end;

procedure TFMXChromium.doAllConnectionsClosed;
begin
  if assigned(FOnAllConnectionsClosed) then FOnAllConnectionsClosed(self);
end;

procedure TFMXChromium.doOnExecuteTaskOnCefThread(aTaskID : cardinal);
begin
  if assigned(FOnExecuteTaskOnCefThread) then FOnExecuteTaskOnCefThread(self, aTaskID);
end;

procedure TFMXChromium.doOnCookiesVisited(const name_, value, domain, path: ustring;
                                                secure, httponly, hasExpires: Boolean;
                                          const creation, lastAccess, expires: TDateTime;
                                                count, total, aID : Integer;
                                          var   aDeleteCookie, aResult : Boolean);
begin
  if assigned(FOnCookiesVisited) then
    FOnCookiesVisited(self, name, value, domain, path,
                      secure, httponly, hasExpires,
                      creation, lastAccess, expires,
                      count, total, aID,
                      aDeleteCookie, aResult);
end;

procedure TFMXChromium.doOnCookieVisitorDestroyed(aID : integer);
begin
  if assigned(FOnCookieVisitorDestroyed) then
    FOnCookieVisitorDestroyed(self, aID);
end;

procedure TFMXChromium.doOnCookieSet(aSuccess : boolean; aID : integer);
begin
  if assigned(FOnCookieSet) then FOnCookieSet(self, aSuccess, aID);
end;

function TFMXChromium.MustCreateLoadHandler : boolean;
begin
  Result := assigned(FOnLoadStart) or
            assigned(FOnLoadEnd)   or
            assigned(FOnLoadError) or
            assigned(FOnLoadingStateChange);
end;

function TFMXChromium.MustCreateFocusHandler : boolean;
begin
  Result := assigned(FOnTakeFocus) or
            assigned(FOnSetFocus)  or
            assigned(FOnGotFocus);
end;

function TFMXChromium.MustCreateContextMenuHandler : boolean;
begin
  Result := assigned(FOnBeforeContextMenu)  or
            assigned(FOnRunContextMenu)     or
            assigned(FOnContextMenuCommand) or
            assigned(FOnContextMenuDismissed);
end;

function TFMXChromium.MustCreateDialogHandler : boolean;
begin
  Result := assigned(FOnFileDialog);
end;

function TFMXChromium.MustCreateKeyboardHandler : boolean;
begin
  Result := assigned(FOnPreKeyEvent) or
            assigned(FOnKeyEvent);
end;

function TFMXChromium.MustCreateDisplayHandler : boolean;
begin
  Result := assigned(FOnAddressChange)         or
            assigned(FOnTitleChange)           or
            assigned(FOnFavIconUrlChange)      or
            assigned(FOnFullScreenModeChange)  or
            assigned(FOnTooltip)               or
            assigned(FOnStatusMessage)         or
            assigned(FOnConsoleMessage)        or
            assigned(FOnAutoResize)            or
            assigned(FOnLoadingProgressChange);
end;

function TFMXChromium.MustCreateDownloadHandler : boolean;
begin
  Result := assigned(FOnBeforeDownload) or
            assigned(FOnDownloadUpdated);
end;

function TFMXChromium.MustCreateJsDialogHandler : boolean;
begin
  Result := assigned(FOnJsdialog)           or
            assigned(FOnBeforeUnloadDialog) or
            assigned(FOnResetDialogState)   or
            assigned(FOnDialogClosed);
end;

function TFMXChromium.MustCreateLifeSpanHandler : boolean;
begin
  Result := True;
end;

function TFMXChromium.MustCreateRenderHandler : boolean;
begin
  Result := FIsOSR;
end;

function TFMXChromium.MustCreateRequestHandler : boolean;
begin
  Result := True;
end;

function TFMXChromium.MustCreateDragHandler : boolean;
begin
  Result := assigned(FOnDragEnter) or
            assigned(FOnDraggableRegionsChanged);
end;

function TFMXChromium.MustCreateFindHandler : boolean;
begin
  Result := assigned(FOnFindResult);
end;

function TFMXChromium.MustCreateResourceRequestHandler : boolean;
begin
  Result := assigned(FOnBeforeResourceLoad) or
            assigned(FOnGetResourceHandler) or
            assigned(FOnResourceRedirect) or
            assigned(FOnResourceResponse) or
            assigned(FOnGetResourceResponseFilter) or
            assigned(FOnResourceLoadComplete) or
            assigned(FOnProtocolExecution) or
            MustCreateCookieAccessFilter;
end;

function TFMXChromium.MustCreateCookieAccessFilter : boolean;
begin
  Result := assigned(FOnCanSendCookie) or
            assigned(FOnCanSaveCookie);
end;

function TFMXChromium.MustCreateRequestContextHandler : boolean;
begin
  Result := assigned(FOnRequestContextInitialized) or
            assigned(FOnBeforePluginLoad) or
            assigned(FOnGetResourceRequestHandler_ReqCtxHdlr) or
            MustCreateResourceRequestHandler;
end;

procedure TFMXChromium.doTextResultAvailable(const aText : ustring);
begin
  if assigned(FOnTextResultAvailable) then FOnTextResultAvailable(self, aText);
end;

procedure TFMXChromium.ExecuteJavaScript(const aCode, aScriptURL, aFrameName : ustring; aStartLine : integer);
var
  TempFrame : ICefFrame;
begin
  try
    if Initialized then
      begin
        if (length(aFrameName) > 0) then
          TempFrame := FBrowser.GetFrame(aFrameName)
         else
          TempFrame := FBrowser.MainFrame;

        if (TempFrame <> nil) and TempFrame.IsValid then
          TempFrame.ExecuteJavaScript(aCode, aScriptURL, aStartLine);
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TFMXChromium.ExecuteJavaScript', e) then raise;
  end;
end;

procedure TFMXChromium.ExecuteJavaScript(const aCode, aScriptURL : ustring; const aFrame : ICefFrame; aStartLine : integer);
begin
  try
    if Initialized and (aFrame <> nil) and aFrame.IsValid then
      aFrame.ExecuteJavaScript(aCode, aScriptURL, aStartLine);
  except
    on e : exception do
      if CustomExceptionHandler('TFMXChromium.ExecuteJavaScript', e) then raise;
  end;
end;

procedure TFMXChromium.ExecuteJavaScript(const aCode, aScriptURL : ustring; const aFrameIdentifier : int64; aStartLine : integer = 0);
var
  TempFrame : ICefFrame;
begin
  try
    if Initialized then
      begin
        if (aFrameIdentifier <> 0) then
          TempFrame := FBrowser.GetFrameByident(aFrameIdentifier)
         else
          TempFrame := FBrowser.MainFrame;

        if (TempFrame <> nil) and TempFrame.IsValid then
          TempFrame.ExecuteJavaScript(aCode, aScriptURL, aStartLine);
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TFMXChromium.ExecuteJavaScript', e) then raise;
  end;
end;

procedure TFMXChromium.doCookiesDeleted(numDeleted : integer);
begin
  if assigned(FOnCookiesDeleted) then FOnCookiesDeleted(self, numDeleted);
end;

procedure TFMXChromium.doPdfPrintFinished(aResultOK : boolean);
begin
  if assigned(FOnPdfPrintFinished) then FOnPdfPrintFinished(self, aResultOK);
end;

procedure TFMXChromium.ShowDevTools(inspectElementAt: TPoint);
var
  TempPoint  : TCefPoint;
  TempClient : ICefClient;
  TempPPoint : PCefPoint;
begin
  try
    try
      if Initialized then
        begin
          InitializeSettings(FDevBrowserSettings);
          WindowInfoAsPopUp(FDevWindowInfo, WindowHandle, DEVTOOLS_WINDOWNAME);

          TempClient := TCustomClientHandler.Create(Self, True);

          if (inspectElementAt.x <> low(integer)) and
             (inspectElementAt.y <> low(integer)) then
            begin
              TempPoint.x := inspectElementAt.x;
              TempPoint.y := inspectElementAt.y;
              TempPPoint  := @TempPoint;
            end
           else
            TempPPoint := nil;

          FBrowser.Host.ShowDevTools(@FDevWindowInfo, TempClient, @FDevBrowserSettings, TempPPoint);
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TFMXChromium.ShowDevTools', e) then raise;
    end;
  finally
    TempClient := nil;
  end;
end;

procedure TFMXChromium.CloseDevTools;
begin
  if Initialized then FBrowser.Host.CloseDevTools;
end;

function TFMXChromium.doOnClose(const browser: ICefBrowser): Boolean;
var
  TempAction : TCefCloseBrowserAction;
begin
  Result     := False;
  TempAction := cbaClose;

  // TempAction values
  // -----------------
  // cbaCancel : stop closing the browser
  // cbaClose  : continue closing the browser
  // cbaDelay  : stop closing the browser momentarily. Used when the application
  //             needs to execute some custom processes before closing the
  //             browser. This is usually needed to destroy a TCEFWindowParent
  //             in the main thread before closing the browser.
  if Assigned(FOnClose) then FOnClose(Self, browser, TempAction);

  case TempAction of
    cbaCancel : Result := True;

    cbaDelay :
      begin
        Result := True;
        if (browser <> nil) and (FBrowserId = browser.Identifier) then FClosing := True;
      end;

    else
      if (browser <> nil) and (FBrowserId = browser.Identifier) then FClosing := True;
  end;
end;

procedure TFMXChromium.doOnBeforeClose(const browser: ICefBrowser);
begin
  if (browser <> nil) and (FBrowserId = browser.Identifier) then
    begin
      FInitialized := False;
      DestroyResourceRequestHandler;
      DestroyReqContextHandler;
      ClearBrowserReference;
      DestroyClientHandler;
    end;

  if Assigned(FOnBeforeClose) then FOnBeforeClose(Self, browser);
end;

procedure TFMXChromium.doOnAddressChange(const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
begin
  if Assigned(FOnAddressChange) then FOnAddressChange(Self, browser, frame, url);
end;

procedure TFMXChromium.doOnAfterCreated(const browser: ICefBrowser);
begin
  if MultithreadApp and (FBrowser = nil) and (browser <> nil) then
    begin
      FBrowser     := browser;
      FBrowserId   := browser.Identifier;
      FInitialized := (FBrowserId <> 0);
    end;

  doUpdatePreferences(browser);

  if Assigned(FOnAfterCreated) then FOnAfterCreated(Self, browser);
end;

function TFMXChromium.doOnBeforeBrowse(const browser      : ICefBrowser;
                                       const frame        : ICefFrame;
                                       const request      : ICefRequest;
                                             user_gesture : Boolean;
                                             isRedirect   : Boolean): Boolean;
begin
  Result := False;

  if FUpdatePreferences then doUpdatePreferences(browser);

  if Assigned(FOnBeforeBrowse) then FOnBeforeBrowse(Self, browser, frame, request, user_gesture, isRedirect, Result);
end;

procedure TFMXChromium.doOnBeforeContextMenu(const browser : ICefBrowser;
                                             const frame   : ICefFrame;
                                             const params  : ICefContextMenuParams;
                                             const model   : ICefMenuModel);
begin
  if Assigned(FOnBeforeContextMenu) then FOnBeforeContextMenu(Self, browser, frame, params, model);
end;

function TFMXChromium.doRunContextMenu(const browser  : ICefBrowser;
                                       const frame    : ICefFrame;
                                       const params   : ICefContextMenuParams;
                                       const model    : ICefMenuModel;
                                       const callback : ICefRunContextMenuCallback): Boolean;
begin
  Result := False;

  if Assigned(FOnRunContextMenu) then FOnRunContextMenu(Self, browser, frame, params, model, callback, Result);
end;

procedure TFMXChromium.doOnBeforeDownload(const browser       : ICefBrowser;
                                          const downloadItem  : ICefDownloadItem;
                                          const suggestedName : ustring;
                                          const callback      : ICefBeforeDownloadCallback);
begin
  if Assigned(FOnBeforeDownload) then FOnBeforeDownload(Self, browser, downloadItem, suggestedName, callback);
end;

function TFMXChromium.doOnBeforePopup(const browser            : ICefBrowser;
                                      const frame              : ICefFrame;
                                      const targetUrl          : ustring;
                                      const targetFrameName    : ustring;
                                            targetDisposition  : TCefWindowOpenDisposition;
                                            userGesture        : Boolean;
                                      const popupFeatures      : TCefPopupFeatures;
                                      var   windowInfo         : TCefWindowInfo;
                                      var   client             : ICefClient;
                                      var   settings           : TCefBrowserSettings;
                                      var   extra_info         : ICefDictionaryValue;
                                      var   noJavascriptAccess : Boolean): Boolean;
begin
  Result := False;

  if Assigned(FOnBeforePopup) then
    FOnBeforePopup(Self, browser, frame, targetUrl, targetFrameName,
                   targetDisposition, userGesture, popupFeatures, windowInfo, client,
                   settings, extra_info, noJavascriptAccess, Result);
end;

function TFMXChromium.doOnBeforeResourceLoad(const browser  : ICefBrowser;
                                             const frame    : ICefFrame;
                                             const request  : ICefRequest;
                                             const callback : ICefRequestCallback): TCefReturnValue;
var
  TempHeaderMap : ICefStringMultimap;
begin
  if FAddCustomHeader then
    try
      TempHeaderMap := TCefStringMultimapOwn.Create;
      request.GetHeaderMap(TempHeaderMap);
      TempHeaderMap.Append(FCustomHeaderName, FCustomHeaderValue);
      request.SetHeaderMap(TempHeaderMap);
    finally
      TempHeaderMap := nil;
    end;

  if not(FSendReferrer) then request.SetReferrer('', REFERRER_POLICY_NO_REFERRER);

  Result := RV_CONTINUE;

  if Assigned(FOnBeforeResourceLoad) then FOnBeforeResourceLoad(Self, browser, frame, request, callback, Result);
end;

function TFMXChromium.doOnBeforeUnloadDialog(const browser     : ICefBrowser;
                                             const messageText : ustring;
                                                   isReload    : Boolean;
                                             const callback    : ICefJsDialogCallback): Boolean;
begin
  Result := False;

  if Assigned(FOnBeforeUnloadDialog) then FOnBeforeUnloadDialog(Self, browser, messageText, isReload, callback, Result);
end;

function TFMXChromium.doOnCertificateError(const browser    : ICefBrowser;
                                                 certError  : TCefErrorcode;
                                           const requestUrl : ustring;
                                           const sslInfo    : ICefSslInfo;
                                           const callback   : ICefRequestCallback): Boolean;
begin
  Result := False;

  if Assigned(FOnCertificateError) then
    FOnCertificateError(Self, browser, certError, requestUrl, sslInfo, callback, Result);
end;

function TFMXChromium.doOnConsoleMessage(const browser  : ICefBrowser;
                                               level    : TCefLogSeverity;
                                         const aMessage : ustring;
                                         const source   : ustring;
                                               line     : Integer): Boolean;
begin
  Result := False;

  if Assigned(FOnConsoleMessage) then FOnConsoleMessage(Self, browser, level, aMessage, source, line, Result);
end;

function TFMXChromium.doOnAutoResize(const browser  : ICefBrowser;
                                     const new_size : PCefSize): Boolean;
begin
  Result := False;

  if Assigned(FOnAutoResize) then FOnAutoResize(Self, browser, new_size, Result);
end;

procedure TFMXChromium.doOnLoadingProgressChange(const browser: ICefBrowser; const progress: double);
begin
  if assigned(FOnLoadingProgressChange) then FOnLoadingProgressChange(self, browser, progress);
end;

function TFMXChromium.doOnContextMenuCommand(const browser    : ICefBrowser;
                                             const frame      : ICefFrame;
                                             const params     : ICefContextMenuParams;
                                                   commandId  : Integer;
                                                   eventFlags : TCefEventFlags): Boolean;
begin
  Result := False;

  if Assigned(FOnContextMenuCommand) then
    FOnContextMenuCommand(Self, browser, frame, params, commandId, eventFlags, Result);
end;

procedure TFMXChromium.doOnContextMenuDismissed(const browser: ICefBrowser; const frame: ICefFrame);
begin
  if Assigned(FOnContextMenuDismissed) then FOnContextMenuDismissed(Self, browser, frame);
end;

procedure TFMXChromium.doOnCursorChange(const browser          : ICefBrowser;
                                              cursor           : TCefCursorHandle;
                                              cursorType       : TCefCursorType;
                                        const customCursorInfo : PCefCursorInfo);
begin
  if assigned(FOnCursorChange) then FOnCursorChange(self, browser, cursor, cursorType, customCursorInfo);
end;

procedure TFMXChromium.doOnDialogClosed(const browser: ICefBrowser);
begin
  if Assigned(FOnDialogClosed) then FOnDialogClosed(Self, browser);
end;

procedure TFMXChromium.doOnDownloadUpdated(const browser      : ICefBrowser;
                                           const downloadItem : ICefDownloadItem;
                                           const callback     : ICefDownloadItemCallback);
begin
  if Assigned(FOnDownloadUpdated) then FOnDownloadUpdated(Self, browser, downloadItem, callback);
end;

function TFMXChromium.doOnDragEnter(const browser  : ICefBrowser;
                                    const dragData : ICefDragData;
                                          mask     : TCefDragOperations): Boolean;
begin
  Result := False;

  if Assigned(FOnDragEnter) then FOnDragEnter(Self, browser, dragData, mask, Result);
end;

procedure TFMXChromium.doOnDraggableRegionsChanged(const browser      : ICefBrowser;
                                                   const frame        : ICefFrame;
                                                         regionsCount : NativeUInt;
                                                         regions      : PCefDraggableRegionArray);
begin
  if Assigned(FOnDraggableRegionsChanged) then FOnDraggableRegionsChanged(Self, browser, frame, regionsCount, regions);
end;

procedure TFMXChromium.doOnFaviconUrlChange(const browser: ICefBrowser; const iconUrls: TStrings);
begin
  if Assigned(FOnFavIconUrlChange) then FOnFavIconUrlChange(Self, browser, iconUrls);
end;

function TFMXChromium.doOnFileDialog(const browser              : ICefBrowser;
                                           mode                 : TCefFileDialogMode;
                                     const title                : ustring;
                                     const defaultFilePath      : ustring;
                                     const acceptFilters        : TStrings;
                                           selectedAcceptFilter : Integer;
                                     const callback             : ICefFileDialogCallback): Boolean;
begin
  Result := False;

  if Assigned(FOnFileDialog) then
    FOnFileDialog(Self, browser, mode, title, defaultFilePath, acceptFilters,
                  selectedAcceptFilter, callback, Result);
end;

procedure TFMXChromium.doOnFindResult(const browser            : ICefBrowser;
                                            identifier         : integer;
                                            count              : Integer;
                                      const selectionRect      : PCefRect;
                                            activeMatchOrdinal : Integer;
                                            finalUpdate        : Boolean);
begin
  if Assigned(FOnFindResult) then
    FOnFindResult(Self, browser, identifier, count, selectionRect, activeMatchOrdinal, finalUpdate);
end;

procedure TFMXChromium.doOnRequestContextInitialized(const request_context: ICefRequestContext);
begin
  if assigned(FOnRequestContextInitialized) then FOnRequestContextInitialized(self, request_context);
end;

function TFMXChromium.doOnBeforePluginLoad(const mimeType     : ustring;
                                           const pluginUrl    : ustring;
                                                 isMainFrame  : boolean;
                                           const topOriginUrl : ustring;
                                           const pluginInfo   : ICefWebPluginInfo;
                                           var   pluginPolicy : TCefPluginPolicy): Boolean;
begin
  Result := False;

  if assigned(FOnBeforePluginLoad) then
    FOnBeforePluginLoad(self, mimeType, pluginUrl, isMainFrame, topOriginUrl, pluginInfo, pluginPolicy, Result);
end;

procedure TFMXChromium.doGetResourceRequestHandler_ReqCtxHdlr(const browser                  : ICefBrowser;
                                                              const frame                    : ICefFrame;
                                                              const request                  : ICefRequest;
                                                                    is_navigation            : boolean;
                                                                    is_download              : boolean;
                                                              const request_initiator        : ustring;
                                                              var   disable_default_handling : boolean;
                                                              var   aResourceRequestHandler  : ICefResourceRequestHandler);
begin
  if (FResourceRequestHandler <> nil) then
    aResourceRequestHandler := FResourceRequestHandler;

  if Assigned(FOnGetResourceRequestHandler_ReqCtxHdlr) then
    FOnGetResourceRequestHandler_ReqCtxHdlr(self, browser, frame, request, is_navigation, is_download,
                                            request_initiator, disable_default_handling,
                                            aResourceRequestHandler);
end;

procedure TFMXChromium.doOnFullScreenModeChange(const browser: ICefBrowser; fullscreen: Boolean);
begin
  if Assigned(FOnFullScreenModeChange) then FOnFullScreenModeChange(Self, browser, fullscreen);
end;

function TFMXChromium.doOnGetAuthCredentials(const browser   : ICefBrowser;
                                             const originUrl : ustring;
                                                   isProxy   : Boolean;
                                             const host      : ustring;
                                                   port      : Integer;
                                             const realm     : ustring;
                                             const scheme    : ustring;
                                             const callback  : ICefAuthCallback): Boolean;
begin
  Result := False;

  if isProxy then
    begin
      if (FProxyType = CEF_PROXYTYPE_FIXED_SERVERS) and (callback <> nil) then
        begin
          Result := True;
          callback.cont(FProxyUsername, FProxyPassword);
        end;
    end
   else
    if Assigned(FOnGetAuthCredentials) then
      FOnGetAuthCredentials(Self, browser, originUrl, isProxy, host, port, realm, scheme, callback, Result);
end;

function TFMXChromium.doCanSendCookie(const browser : ICefBrowser;
                                      const frame   : ICefFrame;
                                      const request : ICefRequest;
                                      const cookie  : PCefCookie): boolean;
begin
  Result := True;

  if assigned(FOnCanSendCookie) then FOnCanSendCookie(self, browser, frame, request, cookie, Result);
end;

function TFMXChromium.doCanSaveCookie(const browser  : ICefBrowser;
                                      const frame    : ICefFrame;
                                      const request  : ICefRequest;
                                      const response : ICefResponse;
                                      const cookie   : PCefCookie): boolean;
begin
  Result := True;

  if assigned(FOnCanSaveCookie) then FOnCanSaveCookie(self, browser, frame, request, response, cookie, Result);
end;

procedure TFMXChromium.doOnGetResourceHandler(const browser          : ICefBrowser;
                                              const frame            : ICefFrame;
                                              const request          : ICefRequest;
                                              var   aResourceHandler : ICefResourceHandler);
begin
  aResourceHandler := nil;

  if Assigned(FOnGetResourceHandler) then
    FOnGetResourceHandler(Self, browser, frame, request, aResourceHandler);
end;

procedure TFMXChromium.doOnGetAccessibilityHandler(var aAccessibilityHandler : ICefAccessibilityHandler);
begin
  if assigned(FOnGetAccessibilityHandler) then FOnGetAccessibilityHandler(Self, aAccessibilityHandler);
end;

function TFMXChromium.doOnGetRootScreenRect(const browser: ICefBrowser; var rect: TCefRect): Boolean;
begin
  Result := False;

  if Assigned(FOnGetRootScreenRect) then FOnGetRootScreenRect(Self, browser, rect, Result);
end;

function TFMXChromium.doOnGetScreenInfo(const browser: ICefBrowser; var screenInfo: TCefScreenInfo): Boolean;
begin
  Result := False;

  if Assigned(FOnGetScreenInfo) then FOnGetScreenInfo(Self, browser, screenInfo, Result);
end;

function TFMXChromium.doOnGetScreenPoint(const browser: ICefBrowser; viewX, viewY: Integer; var screenX, screenY: Integer): Boolean;
begin
  Result := False;

  if Assigned(FOnGetScreenPoint) then FOnGetScreenPoint(Self, browser, viewX, viewY, screenX, screenY, Result);
end;

procedure TFMXChromium.doOnGetViewRect(const browser: ICefBrowser; var rect: TCefRect);
begin
  if Assigned(FOnGetViewRect) then FOnGetViewRect(Self, browser, rect);
end;

procedure TFMXChromium.doOnGotFocus(const browser: ICefBrowser);
begin
  if Assigned(FOnGotFocus) then FOnGotFocus(Self, browser)
end;

function TFMXChromium.doOnJsdialog(const browser           : ICefBrowser;
                                   const originUrl         : ustring;
                                         dialogType        : TCefJsDialogType;
                                   const messageText       : ustring;
                                   const defaultPromptText : ustring;
                                   const callback          : ICefJsDialogCallback;
                                   out   suppressMessage   : Boolean): Boolean;
begin
  Result := False;

  if not(Initialized) then
    suppressMessage := True
   else
    begin
      suppressMessage := False;

      if Assigned(FOnJsdialog) then
        FOnJsdialog(Self, browser, originUrl, dialogType, messageText,
                    defaultPromptText, callback, suppressMessage, Result);
    end;
end;

function TFMXChromium.doOnKeyEvent(const browser : ICefBrowser;
                                   const event   : PCefKeyEvent;
                                         osEvent : TCefEventHandle): Boolean;
begin
  Result := False;

  if Assigned(FOnKeyEvent) then FOnKeyEvent(Self, browser, event, osEvent, Result);
end;

procedure TFMXChromium.doOnLoadEnd(const browser        : ICefBrowser;
                                   const frame          : ICefFrame;
                                         httpStatusCode : Integer);
begin
  if Assigned(FOnLoadEnd) then FOnLoadEnd(Self, browser, frame, httpStatusCode);
end;

procedure TFMXChromium.doOnLoadError(const browser   : ICefBrowser;
                                     const frame     : ICefFrame;
                                           errorCode : TCefErrorCode;
                                     const errorText : ustring;
                                     const failedUrl : ustring);
begin
  if Assigned(FOnLoadError) then FOnLoadError(Self, browser, frame, errorCode, errorText, failedUrl);
end;

procedure TFMXChromium.doOnLoadingStateChange(const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
begin
  if Assigned(FOnLoadingStateChange) then FOnLoadingStateChange(Self, browser, isLoading, canGoBack, canGoForward);
end;

procedure TFMXChromium.doOnLoadStart(const browser: ICefBrowser; const frame: ICefFrame; transitionType: TCefTransitionType);
begin
  if Assigned(FOnLoadStart) then FOnLoadStart(Self, browser, frame, transitionType);
end;

function TFMXChromium.doOnOpenUrlFromTab(const browser           : ICefBrowser;
                                         const frame             : ICefFrame;
                                         const targetUrl         : ustring;
                                               targetDisposition : TCefWindowOpenDisposition;
                                               userGesture       : Boolean): Boolean;
begin
  Result := False;

  if Assigned(FOnOpenUrlFromTab) then
    FOnOpenUrlFromTab(Self, browser, frame, targetUrl, targetDisposition, userGesture, Result);
end;

procedure TFMXChromium.doGetResourceRequestHandler_ReqHdlr(const browser                  : ICefBrowser;
                                                           const frame                    : ICefFrame;
                                                           const request                  : ICefRequest;
                                                                 is_navigation            : boolean;
                                                                 is_download              : boolean;
                                                           const request_initiator        : ustring;
                                                           var   disable_default_handling : boolean;
                                                           var   aResourceRequestHandler  : ICefResourceRequestHandler);
begin
  if (FResourceRequestHandler <> nil) then
    aResourceRequestHandler := FResourceRequestHandler;

  if Assigned(FOnGetResourceRequestHandler_ReqHdlr) then
    FOnGetResourceRequestHandler_ReqHdlr(self, browser, frame, request, is_navigation, is_download,
                                         request_initiator, disable_default_handling,
                                         aResourceRequestHandler);
end;

procedure TFMXChromium.doOnPaint(const browser         : ICefBrowser;
                                       kind            : TCefPaintElementType;
                                       dirtyRectsCount : NativeUInt;
                                 const dirtyRects      : PCefRectArray;
                                 const buffer          : Pointer;
                                       width           : Integer;
                                       height          : Integer);
begin
  if Assigned(FOnPaint) then FOnPaint(Self, browser, kind, dirtyRectsCount, dirtyRects, buffer, width, height);
end;

procedure TFMXChromium.doOnAcceleratedPaint(const browser         : ICefBrowser;
                                                  kind            : TCefPaintElementType;
                                                  dirtyRectsCount : NativeUInt;
                                            const dirtyRects      : PCefRectArray;
                                                  shared_handle   : Pointer);
begin
  if Assigned(FOnAcceleratedPaint) then FOnAcceleratedPaint(Self, browser, kind, dirtyRectsCount, dirtyRects, shared_handle);
end;

function TFMXChromium.doOnSelectClientCertificate(const browser           : ICefBrowser;
                                                        isProxy           : boolean;
                                                  const host              : ustring;
                                                        port              : integer;
                                                        certificatesCount : NativeUInt;
                                                  const certificates      : TCefX509CertificateArray;
                                                  const callback          : ICefSelectClientCertificateCallback): boolean;
begin
  Result := False;

  if assigned(FOnSelectClientCertificate) then
    FOnSelectClientCertificate(self, browser, isProxy, host, port, certificatesCount, certificates, callback, Result);
end;

procedure TFMXChromium.doOnPluginCrashed(const browser: ICefBrowser; const pluginPath: ustring);
begin
  if Assigned(FOnPluginCrashed) then FOnPluginCrashed(Self, browser, pluginPath);
end;

procedure TFMXChromium.doOnPopupShow(const browser: ICefBrowser; show: Boolean);
begin
  if assigned(FOnPopupShow) then FOnPopupShow(self, browser, show);
end;

procedure TFMXChromium.doOnPopupSize(const browser: ICefBrowser; const rect: PCefRect);
begin
  if assigned(FOnPopupSize) then FOnPopupSize(self, browser, rect);
end;

function TFMXChromium.doOnPreKeyEvent(const browser            : ICefBrowser;
                                      const event              : PCefKeyEvent;
                                            osEvent            : TCefEventHandle;
                                      out   isKeyboardShortcut : Boolean): Boolean;
begin
  Result := False;

  if Assigned(FOnPreKeyEvent) then FOnPreKeyEvent(Self, browser, event, osEvent, isKeyboardShortcut, Result);
end;

function TFMXChromium.doOnProcessMessageReceived(const browser       : ICefBrowser;
                                                 const frame         : ICefFrame;
                                                       sourceProcess : TCefProcessId;
                                                 const aMessage      : ICefProcessMessage): Boolean;
begin
  Result := False;

  if Assigned(FOnProcessMessageReceived) then
    FOnProcessMessageReceived(Self, browser, frame, sourceProcess, aMessage, Result);
end;

procedure TFMXChromium.doOnProtocolExecution(const browser          : ICefBrowser;
                                             const frame            : ICefFrame;
                                             const request          : ICefRequest;
                                             var   allowOsExecution : Boolean);
begin
  if Assigned(FOnProtocolExecution) then FOnProtocolExecution(Self, browser, frame, request, allowOsExecution);
end;

function TFMXChromium.doOnQuotaRequest(const browser   : ICefBrowser;
                                       const originUrl : ustring;
                                             newSize   : Int64;
                                       const callback  : ICefRequestCallback): Boolean;
begin
  Result := False;

  if Assigned(FOnQuotaRequest) then FOnQuotaRequest(Self, browser, originUrl, newSize, callback, Result);
end;

procedure TFMXChromium.doOnRenderProcessTerminated(const browser: ICefBrowser; status: TCefTerminationStatus);
begin
  if Assigned(FOnRenderProcessTerminated) then FOnRenderProcessTerminated(Self, browser, status);
end;

procedure TFMXChromium.doOnRenderViewReady(const browser: ICefBrowser);
{$IFDEF MSWINDOWS}
var
  OldBrowserCompHWND, OldWidgetCompHWND, OldRenderCompHWND: THandle;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  if (browser            <> nil)        and
     (browser.Host       <> nil)        and
     (browser.Identifier =  FBrowserId) then
    begin
      OldBrowserCompHWND := FBrowserCompHWND;
      OldWidgetCompHWND := FWidgetCompHWND;
      OldRenderCompHWND := FRenderCompHWND;

      FBrowserCompHWND := browser.Host.WindowHandle;

      if (FBrowserCompHWND <> 0) then
        FWidgetCompHWND := FindWindowEx(FBrowserCompHWND, 0, 'Chrome_WidgetWin_0', '');

      if (FWidgetCompHWND <> 0) then
        FRenderCompHWND := FindWindowEx(FWidgetCompHWND, 0, 'Chrome_RenderWidgetHostHWND', 'Chrome Legacy Window');

      RestoreCompWndProc(OldBrowserCompHWND, FBrowserCompHWND, FOldBrowserCompWndPrc);
      if assigned(FOnBrowserCompMsg) and (FBrowserCompHWND <> 0) and (FOldBrowserCompWndPrc = nil) then
        begin
          CreateStub(BrowserCompWndProc, FBrowserCompStub);
          FOldBrowserCompWndPrc := TFNWndProc(SetWindowLongPtr(FBrowserCompHWND,
                                                               GWLP_WNDPROC,
                                                               NativeInt(FBrowserCompStub)));
        end;

      RestoreCompWndProc(OldWidgetCompHWND, FWidgetCompHWND, FOldWidgetCompWndPrc);
      if assigned(FOnWidgetCompMsg) and (FWidgetCompHWND <> 0) and (FOldWidgetCompWndPrc = nil) then
        begin
          CreateStub(WidgetCompWndProc, FWidgetCompStub);
          FOldWidgetCompWndPrc := TFNWndProc(SetWindowLongPtr(FWidgetCompHWND,
                                                              GWLP_WNDPROC,
                                                              NativeInt(FWidgetCompStub)));
        end;

      RestoreCompWndProc(OldRenderCompHWND, FRenderCompHWND, FOldRenderCompWndPrc);
      if assigned(FOnRenderCompMsg) and (FRenderCompHWND <> 0) and (FOldRenderCompWndPrc = nil) then
        begin
          CreateStub(RenderCompWndProc, FRenderCompStub);
          FOldRenderCompWndPrc := TFNWndProc(SetWindowLongPtr(FRenderCompHWND,
                                                              GWLP_WNDPROC,
                                                              NativeInt(FRenderCompStub)));
        end;
    end;
  {$ENDIF}

  if Assigned(FOnRenderViewReady) then FOnRenderViewReady(Self, browser);
end;

procedure TFMXChromium.doOnResetDialogState(const browser: ICefBrowser);
begin
  if Assigned(FOnResetDialogState) then FOnResetDialogState(Self, browser);
end;

procedure TFMXChromium.doOnResourceRedirect(const browser  : ICefBrowser;
                                            const frame    : ICefFrame;
                                            const request  : ICefRequest;
                                            const response : ICefResponse;
                                            var   newUrl   : ustring);
begin
  if Assigned(FOnResourceRedirect) then FOnResourceRedirect(Self, browser, frame, request, response, newUrl);
end;

function TFMXChromium.doOnResourceResponse(const browser  : ICefBrowser;
                                           const frame    : ICefFrame;
                                           const request  : ICefRequest;
                                           const response : ICefResponse): Boolean;
begin
  Result := False;

  if Assigned(FOnResourceResponse) then FOnResourceResponse(Self, browser, frame, request, response, Result);
end;

procedure TFMXChromium.doOnGetResourceResponseFilter(const browser         : ICefBrowser;
                                                     const frame           : ICefFrame;
                                                     const request         : ICefRequest;
                                                     const response        : ICefResponse;
                                                     var   aResponseFilter : ICefResponseFilter);
begin
  aResponseFilter := nil;

  if Assigned(FOnGetResourceResponseFilter) then
    FOnGetResourceResponseFilter(self, browser, frame, request, response, aResponseFilter);
end;

procedure TFMXChromium.doOnResourceLoadComplete(const browser               : ICefBrowser;
                                                const frame                 : ICefFrame;
                                                const request               : ICefRequest;
                                                const response              : ICefResponse;
                                                      status                : TCefUrlRequestStatus;
                                                      receivedContentLength : Int64);
begin
  if Assigned(FOnResourceLoadComplete) then
    FOnResourceLoadComplete(self, browser, frame, request, response, status, receivedContentLength);
end;

procedure TFMXChromium.doOnScrollOffsetChanged(const browser: ICefBrowser; x, y: Double);
begin
  if Assigned(FOnScrollOffsetChanged) then FOnScrollOffsetChanged(Self, browser, x, y);
end;

procedure TFMXChromium.doOnIMECompositionRangeChanged(const browser               : ICefBrowser;
                                                      const selected_range        : PCefRange;
                                                            character_boundsCount : NativeUInt;
                                                      const character_bounds      : PCefRect);
begin
  if assigned(FOnIMECompositionRangeChanged) then
    FOnIMECompositionRangeChanged(self, browser, selected_range, character_boundsCount, character_bounds);
end;

procedure TFMXChromium.doOnTextSelectionChanged(const browser        : ICefBrowser;
                                                const selected_text  : ustring;
                                                const selected_range : PCefRange);
begin
  if assigned(FOnTextSelectionChanged) then
    FOnTextSelectionChanged(self, browser, selected_text, selected_range);
end;

procedure TFMXChromium.doOnVirtualKeyboardRequested(const browser    : ICefBrowser;
                                                          input_mode : TCefTextInpuMode);
begin
  if assigned(FOnVirtualKeyboardRequested) then
    FOnVirtualKeyboardRequested(self, browser, input_mode);
end;

function TFMXChromium.doOnSetFocus(const browser: ICefBrowser; source: TCefFocusSource): Boolean;
begin
  Result := False;

  if Assigned(FOnSetFocus) then FOnSetFocus(Self, browser, source, Result);
end;

function TFMXChromium.doOnStartDragging(const browser    : ICefBrowser;
                                        const dragData   : ICefDragData;
                                              allowedOps : TCefDragOperations;
                                              x          : integer;
                                              y          : Integer): Boolean;
begin
  Result := False;
end;

procedure TFMXChromium.doOnStatusMessage(const browser: ICefBrowser; const value: ustring);
begin
  if Assigned(FOnStatusMessage) then FOnStatusMessage(Self, browser, value);
end;

procedure TFMXChromium.doOnTakeFocus(const browser: ICefBrowser; next: Boolean);
begin
  if Assigned(FOnTakeFocus) then FOnTakeFocus(Self, browser, next);
end;

procedure TFMXChromium.doOnTitleChange(const browser: ICefBrowser; const title: ustring);
begin
  if Assigned(FOnTitleChange) then FOnTitleChange(Self, browser, title);
end;

function TFMXChromium.doOnTooltip(const browser: ICefBrowser; var text: ustring): Boolean;
begin
  Result := False;

  if Assigned(FOnTooltip) then FOnTooltip(Self, browser, text, Result);
end;

procedure TFMXChromium.doOnUpdateDragCursor(const browser: ICefBrowser; operation: TCefDragOperation);
begin
  //
end;

function TFMXChromium.GetParentForm : TCustomForm;
var
  TempComp : TComponent;
begin
  Result   := nil;
  TempComp := Owner;

  while (TempComp <> nil) do
    if (TempComp is TCustomForm) then
      begin
        Result := TCustomForm(TempComp);
        exit;
      end
     else
      TempComp := TempComp.owner;
end;

{$IFDEF MSWINDOWS}
procedure TFMXChromium.CreateStub(const aMethod : TWndMethod; var aStub : Pointer);
begin
  if (aStub = nil) then aStub := MakeObjectInstance(aMethod);
end;

procedure TFMXChromium.FreeAndNilStub(var aStub : pointer);
begin
  if (aStub <> nil) then
    begin
      FreeObjectInstance(aStub);
      aStub := nil;
    end;
end;

procedure TFMXChromium.RestoreCompWndProc(var aOldWnd: THandle; aNewWnd: THandle; var aProc: TFNWndProc);
begin
  if (aOldWnd <> 0) and (aOldWnd <> aNewWnd) and (aProc <> nil) then
    begin
      SetWindowLongPtr(aOldWnd, GWLP_WNDPROC, NativeInt(aProc));
      aProc := nil;
      aOldWnd := 0;
    end;
end;

procedure TFMXChromium.BrowserCompWndProc(var aMessage: TMessage);
var
  TempHandled : boolean;
begin
  try
    TempHandled := False;

    try
      if assigned(FOnBrowserCompMsg) then
        FOnBrowserCompMsg(aMessage, TempHandled);

      if not(TempHandled)               and
         (FOldBrowserCompWndPrc <> nil) and
         (FBrowserCompHWND      <> 0)   then
        aMessage.Result := CallWindowProc(FOldBrowserCompWndPrc,
                                          FBrowserCompHWND,
                                          aMessage.Msg,
                                          aMessage.wParam,
                                          aMessage.lParam);
    finally
      if aMessage.Msg = WM_DESTROY then
        RestoreCompWndProc(FBrowserCompHWND, 0, FOldBrowserCompWndPrc);
    end;
  except
    on e : exception do
      if CustomExceptionHandler('TFMXChromium.BrowserCompWndProc', e) then raise;
  end;
end;

procedure TFMXChromium.WidgetCompWndProc(var aMessage: TMessage);
var
  TempHandled : boolean;
begin
  try
    TempHandled := False;

    try
      if assigned(FOnWidgetCompMsg) then
        FOnWidgetCompMsg(aMessage, TempHandled);

      if not(TempHandled)              and
         (FOldWidgetCompWndPrc <> nil) and
         (FWidgetCompHWND      <> 0)   then
        aMessage.Result := CallWindowProc(FOldWidgetCompWndPrc,
                                          FWidgetCompHWND,
                                          aMessage.Msg,
                                          aMessage.wParam,
                                          aMessage.lParam);
    finally
      if aMessage.Msg = WM_DESTROY then
        RestoreCompWndProc(FWidgetCompHWND, 0, FOldWidgetCompWndPrc);
    end;
  except
    on e : exception do
      if CustomExceptionHandler('TFMXChromium.WidgetCompWndProc', e) then raise;
  end;
end;

procedure TFMXChromium.RenderCompWndProc(var aMessage: TMessage);
var
  TempHandled : boolean;
begin
  try
    TempHandled := False;

    try
      if assigned(FOnRenderCompMsg) then
        FOnRenderCompMsg(aMessage, TempHandled);

      if not(TempHandled)              and
         (FOldRenderCompWndPrc <> nil) and
         (FRenderCompHWND      <> 0)   then
        aMessage.Result := CallWindowProc(FOldRenderCompWndPrc,
                                          FRenderCompHWND,
                                          aMessage.Msg,
                                          aMessage.wParam,
                                          aMessage.lParam);
    finally
      if aMessage.Msg = WM_DESTROY then
        RestoreCompWndProc(FRenderCompHWND, 0, FOldRenderCompWndPrc);
    end;
  except
    on e : exception do
      if CustomExceptionHandler('TFMXChromium.RenderCompWndProc', e) then raise;
  end;
end;
{$ENDIF}

procedure TFMXChromium.MoveFormTo(const x, y: Integer);
var
  TempForm : TCustomForm;
  {$IFDEF DELPHI17_UP}
  TempRect : TRect;
  {$ENDIF}
begin
  TempForm := GetParentForm;
  {$IFDEF DELPHI17_UP}
  if (TempForm <> nil) then
    begin
      TempRect.Left   := min(max(x, max(screen.DesktopLeft, 0)), screen.DesktopWidth  - TempForm.Width);
      TempRect.Top    := min(max(y, max(screen.DesktopTop,  0)), screen.DesktopHeight - TempForm.Height);
      TempRect.Right  := TempRect.Left + TempForm.Width  - 1;
      TempRect.Bottom := TempRect.Top  + TempForm.Height - 1;

      TempForm.SetBounds(TempRect.Left, TempRect.Top, TempRect.Right - TempRect.Left + 1, TempRect.Bottom - TempRect.Top + 1);
    end;
  {$ELSE}
  TempForm.SetBounds(x, y, TempForm.Width, TempForm.Height);
  {$ENDIF}
end;

procedure TFMXChromium.MoveFormBy(const x, y: Integer);
var
  TempForm : TCustomForm;
  {$IFDEF DELPHI17_UP}
  TempRect : TRect;
  {$ENDIF}
begin
  TempForm := GetParentForm;
  {$IFDEF DELPHI17_UP}
  if (TempForm <> nil) then
    begin
      TempRect.Left   := min(max(TempForm.Left + x, max(screen.DesktopLeft, 0)), screen.DesktopWidth  - TempForm.Width);
      TempRect.Top    := min(max(TempForm.Top  + y, max(screen.DesktopTop,  0)), screen.DesktopHeight - TempForm.Height);
      TempRect.Right  := TempRect.Left + TempForm.Width  - 1;
      TempRect.Bottom := TempRect.Top  + TempForm.Height - 1;

      TempForm.SetBounds(TempRect.Left, TempRect.Top, TempRect.Right - TempRect.Left + 1, TempRect.Bottom - TempRect.Top + 1);
    end;
  {$ELSE}
  TempForm.SetBounds(TempForm.Left + x, TempForm.Top + y, TempForm.Width, TempForm.Height);
  {$ENDIF}
end;

procedure TFMXChromium.ResizeFormWidthTo(const x : Integer);
var
  TempForm : TCustomForm;
  TempX, TempDeltaX : integer;
begin
  TempForm := GetParentForm;

  if (TempForm <> nil) then
    begin
      TempX          := max(x, 100);
      TempDeltaX     := TempForm.Width  - TempForm.ClientWidth;
      TempForm.Width := TempX + TempDeltaX;
    end;
end;

procedure TFMXChromium.ResizeFormHeightTo(const y : Integer);
var
  TempForm : TCustomForm;
  TempY, TempDeltaY : integer;
begin
  TempForm := GetParentForm;

  if (TempForm <> nil) then
    begin
      TempY           := max(y, 100);
      TempDeltaY      := TempForm.Height - TempForm.ClientHeight;
      TempForm.Height := TempY + TempDeltaY;
    end;
end;

procedure TFMXChromium.SetFormLeftTo(const x : Integer);
var
  TempForm : TCustomForm;
begin
  TempForm := GetParentForm;

  if (TempForm <> nil) then
    {$IFDEF DELPHI17_UP}
    TempForm.Left := min(max(x, max(screen.DesktopLeft, 0)), screen.DesktopWidth  - TempForm.Width);
    {$ELSE}
    TempForm.Left := x;
    {$ENDIF}
end;

procedure TFMXChromium.SetFormTopTo(const y : Integer);
var
  TempForm : TCustomForm;
begin
  TempForm := GetParentForm;

  if (TempForm <> nil) then
    {$IFDEF DELPHI17_UP}
    TempForm.Top := min(max(y, max(screen.DesktopTop, 0)), screen.DesktopHeight - TempForm.Height);
    {$ELSE}
    TempForm.Top := y;
    {$ENDIF}
end;

procedure TFMXChromium.WasResized;
begin
  if Initialized then FBrowser.Host.WasResized;
end;

procedure TFMXChromium.WasHidden(hidden: Boolean);
begin
  if Initialized then FBrowser.Host.WasHidden(hidden);
end;

procedure TFMXChromium.NotifyScreenInfoChanged;
begin
  if Initialized then FBrowser.Host.NotifyScreenInfoChanged;
end;

procedure TFMXChromium.NotifyMoveOrResizeStarted;
begin
  if Initialized then FBrowser.Host.NotifyMoveOrResizeStarted;
end;

procedure TFMXChromium.Invalidate(kind: TCefPaintElementType);
begin
  if Initialized then
    begin
      if FIsOSR then
        FBrowser.Host.Invalidate(kind)
       else
        if (RenderHandle <> 0) then
          InvalidateRect(RenderHandle, nil, False)
         else
          InvalidateRect(WindowHandle, nil, False);
    end;
end;

procedure TFMXChromium.SendExternalBeginFrame;
begin
  if Initialized then FBrowser.Host.SendExternalBeginFrame;
end;

procedure TFMXChromium.SendKeyEvent(const event: PCefKeyEvent);
begin
  if Initialized then FBrowser.Host.SendKeyEvent(event);
end;

procedure TFMXChromium.SendMouseClickEvent(const event      : PCefMouseEvent;
                                                 kind       : TCefMouseButtonType;
                                                 mouseUp    : Boolean;
                                                 clickCount : Integer);
begin
  if Initialized then FBrowser.Host.SendMouseClickEvent(event, kind, mouseUp, clickCount);
end;

procedure TFMXChromium.SendMouseMoveEvent(const event: PCefMouseEvent; mouseLeave: Boolean);
begin
  if Initialized then FBrowser.Host.SendMouseMoveEvent(event, mouseLeave);
end;

procedure TFMXChromium.SendMouseWheelEvent(const event: PCefMouseEvent; deltaX, deltaY: Integer);
begin
  if Initialized then FBrowser.Host.SendMouseWheelEvent(event, deltaX, deltaY);
end;

procedure TFMXChromium.SendTouchEvent(const event: PCefTouchEvent);
begin
  if Initialized then FBrowser.Host.SendTouchEvent(event);
end;

procedure TFMXChromium.SendFocusEvent(setFocus: Boolean);
begin
  if Initialized then FBrowser.Host.SendFocusEvent(setFocus);
end;

procedure TFMXChromium.SendCaptureLostEvent;
begin
  if Initialized then FBrowser.Host.SendCaptureLostEvent;
end;

procedure TFMXChromium.SetFocus(focus: Boolean);
begin
  if Initialized then FBrowser.Host.SetFocus(focus);
end;

procedure TFMXChromium.SetAccessibilityState(accessibilityState: TCefState);
begin
  if Initialized then FBrowser.Host.SetAccessibilityState(accessibilityState);
end;

procedure TFMXChromium.SendProcessMessage(targetProcess: TCefProcessId; const ProcMessage: ICefProcessMessage; const aFrameName : ustring);
var
  TempFrame : ICefFrame;
begin
  try
    if Initialized then
      begin
        if (length(aFrameName) > 0) then
          TempFrame := FBrowser.GetFrame(aFrameName)
         else
          TempFrame := FBrowser.MainFrame;

        if (TempFrame <> nil) and TempFrame.IsValid then
          TempFrame.SendProcessMessage(targetProcess, ProcMessage);
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TFMXChromium.SendProcessMessage', e) then raise;
  end;
end;

procedure TFMXChromium.SendProcessMessage(targetProcess: TCefProcessId; const ProcMessage: ICefProcessMessage; const aFrame : ICefFrame);
begin
  try
    if Initialized and (aFrame <> nil) and aFrame.IsValid then
      aFrame.SendProcessMessage(targetProcess, ProcMessage);
  except
    on e : exception do
      if CustomExceptionHandler('TFMXChromium.SendProcessMessage', e) then raise;
  end;
end;

procedure TFMXChromium.SendProcessMessage(targetProcess: TCefProcessId; const ProcMessage: ICefProcessMessage; const aFrameIdentifier : int64);
var
  TempFrame : ICefFrame;
begin
  try
    if Initialized then
      begin
        if (aFrameIdentifier <> 0) then
          TempFrame := FBrowser.GetFrameByident(aFrameIdentifier)
         else
          TempFrame := FBrowser.MainFrame;

        if (TempFrame <> nil) and TempFrame.IsValid then
          TempFrame.SendProcessMessage(targetProcess, ProcMessage);
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TFMXChromium.SendProcessMessage', e) then raise;
  end;
end;

function TFMXChromium.CreateUrlRequest(const request: ICefRequest; const client: ICefUrlrequestClient; const aFrameName : ustring = ''): ICefUrlRequest;
var
  TempFrame : ICefFrame;
begin
  Result := nil;

  try
    if Initialized then
      begin
        if (length(aFrameName) > 0) then
          TempFrame := FBrowser.GetFrame(aFrameName)
         else
          TempFrame := FBrowser.MainFrame;

        if (TempFrame <> nil) and TempFrame.IsValid then
          Result := TempFrame.CreateUrlRequest(request, client);
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TFMXChromium.CreateUrlRequest', e) then raise;
  end;
end;

function TFMXChromium.CreateUrlRequest(const request: ICefRequest; const client: ICefUrlrequestClient; const aFrame : ICefFrame): ICefUrlRequest;
begin
  Result := nil;

  try
    if Initialized and (aFrame <> nil) and aFrame.IsValid then
      Result := aFrame.CreateUrlRequest(request, client);
  except
    on e : exception do
      if CustomExceptionHandler('TFMXChromium.CreateUrlRequest', e) then raise;
  end;
end;

function TFMXChromium.CreateUrlRequest(const request: ICefRequest; const client: ICefUrlrequestClient; const aFrameIdentifier : int64): ICefUrlRequest;
var
  TempFrame : ICefFrame;
begin
  Result := nil;

  try
    if Initialized then
      begin
        if (aFrameIdentifier <> 0) then
          TempFrame := FBrowser.GetFrameByident(aFrameIdentifier)
         else
          TempFrame := FBrowser.MainFrame;

        if (TempFrame <> nil) and TempFrame.IsValid then
          Result := TempFrame.CreateUrlRequest(request, client);
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TFMXChromium.CreateUrlRequest', e) then raise;
  end;
end;

procedure TFMXChromium.DragTargetDragEnter(const dragData: ICefDragData; const event: PCefMouseEvent; allowedOps: TCefDragOperations);
begin
  if Initialized then FBrowser.Host.DragTargetDragEnter(dragData, event, allowedOps);
end;

procedure TFMXChromium.DragTargetDragOver(const event: PCefMouseEvent; allowedOps: TCefDragOperations);
begin
  if Initialized then FBrowser.Host.DragTargetDragOver(event, allowedOps);
end;

procedure TFMXChromium.DragTargetDragLeave;
begin
  if Initialized then FBrowser.Host.DragTargetDragLeave;
end;

procedure TFMXChromium.DragTargetDrop(event: PCefMouseEvent);
begin
  if Initialized then FBrowser.Host.DragTargetDrop(event);
end;

procedure TFMXChromium.DragSourceEndedAt(x, y: Integer; op: TCefDragOperation);
begin
  if Initialized then FBrowser.Host.DragSourceEndedAt(x, y, op);
end;

procedure TFMXChromium.DragSourceSystemDragEnded;
begin
  if Initialized then FBrowser.Host.DragSourceSystemDragEnded;
end;

end.
