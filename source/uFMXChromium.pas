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
//        Copyright © 2018 Salvador Diaz Fau. All rights reserved.
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

unit uFMXChromium;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  System.Classes, System.Types,
  {$IFDEF MSWINDOWS}
  WinApi.Windows, WinApi.Messages,
  {$ENDIF}
  FMX.Types, FMX.Platform, FMX.Controls, FMX.Forms,
  uCEFTypes, uCEFInterfaces, uCEFLibFunctions, uCEFMiscFunctions, uCEFClient,
  uCEFConstants, uCEFTask, uCEFChromiumEvents, uCEFChromiumOptions, uCEFChromiumFontOptions,
  uCEFPDFPrintOptions;

type
  TFMXChromium = class(TComponent, IChromiumEvents)
    protected
      FHandler                : ICefClient;
      FBrowser                : ICefBrowser;
      FBrowserId              : Integer;
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
      FCookiePrefs            : integer;
      FImagesPrefs            : integer;
      FZoomStep               : byte;
      FPrefsFileName          : string;
      FIsOSR                  : boolean;
      FInitialized            : boolean;
      FClosing                : boolean;
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
      FOnBeforeBrowse                 : TOnBeforeBrowse;
      FOnOpenUrlFromTab               : TOnOpenUrlFromTab;
      FOnBeforeResourceLoad           : TOnBeforeResourceLoad;
      FOnGetResourceHandler           : TOnGetResourceHandler;
      FOnResourceRedirect             : TOnResourceRedirect;
      FOnResourceResponse             : TOnResourceResponse;
      FOnGetResourceResponseFilter    : TOnGetResourceResponseFilter;
      FOnResourceLoadComplete         : TOnResourceLoadComplete;
      FOnGetAuthCredentials           : TOnGetAuthCredentials;
      FOnCanGetCookies                : TOnCanGetCookies;
      FOnCanSetCookie                 : TOnCanSetCookie;
      FOnQuotaRequest                 : TOnQuotaRequest;
      FOnProtocolExecution            : TOnProtocolExecution;
      FOnCertificateError             : TOnCertificateError;
      FOnSelectClientCertificate      : TOnSelectClientCertificate;
      FOnPluginCrashed                : TOnPluginCrashed;
      FOnRenderViewReady              : TOnRenderViewReady;
      FOnRenderProcessTerminated      : TOnRenderProcessTerminated;

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
      FOnCursorChange                 : TOnCursorChange;
      FOnScrollOffsetChanged          : TOnScrollOffsetChanged;
      FOnIMECompositionRangeChanged   : TOnIMECompositionRangeChanged;
      FOnTextSelectionChanged         : TOnTextSelectionChanged;

      // ICefDragHandler
      FOnDragEnter                    : TOnDragEnter;
      FOnDraggableRegionsChanged      : TOnDraggableRegionsChanged;

      // ICefFindHandler
      FOnFindResult                   : TOnFindResult;

      // Custom
      FOnTextResultAvailable          : TOnTextResultAvailableEvent;
      FOnPdfPrintFinished             : TOnPdfPrintFinishedEvent;
      FOnCookiesDeleted               : TOnCookiesDeletedEvent;
      FOnResolvedHostAvailable        : TOnResolvedIPsAvailableEvent;
      {$IFDEF MSWINDOWS}
      FOnBrowserCompMsg               : TOnCompMsgEvent;
      FOnWidgetCompMsg                : TOnCompMsgEvent;
      FOnRenderCompMsg                : TOnCompMsgEvent;
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
      procedure SetCookiePrefs(aValue : integer);
      procedure SetImagesPrefs(aValue : integer);
      procedure SetProxyType(aValue : integer);
      procedure SetProxyScheme(aValue : TCefProxyScheme);
      procedure SetProxyServer(const aValue : ustring);
      procedure SetProxyPort(aValue : integer);
      procedure SetProxyUsername(const aValue : ustring);
      procedure SetProxyPassword(const aValue : ustring);
      procedure SetProxyScriptURL(const aValue : ustring);
      procedure SetProxyByPassList(const aValue : ustring);
      procedure SetCustomHeaderName(const aValue : ustring);
      procedure SetCustomHeaderValue(const aValue : ustring);
      procedure SetZoomLevel(const aValue : double);
      procedure SetZoomPct(const aValue : double);
      procedure SetZoomStep(aValue : byte);
      procedure SetWindowlessFrameRate(aValue : integer);


      function  CreateBrowserHost(aWindowInfo : PCefWindowInfo; const aURL : ustring; const aSettings : PCefBrowserSettings; const aContext : ICefRequestContext): boolean;
      function  CreateBrowserHostSync(aWindowInfo : PCefWindowInfo; const aURL : ustring; const aSettings : PCefBrowserSettings; const aContext : ICefRequestContext): boolean;

      procedure DestroyClientHandler;

      procedure ClearBrowserReference;

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

      function  MustCreateLoadHandler : boolean; virtual;
      function  MustCreateFocusHandler : boolean; virtual;
      function  MustCreateContextMenuHandler : boolean; virtual;
      function  MustCreateDialogHandler : boolean; virtual;
      function  MustCreateKeyboardHandler : boolean; virtual;
      function  MustCreateDisplayHandler : boolean; virtual;
      function  MustCreateDownloadHandler : boolean; virtual;
      function  MustCreateJsDialogHandler : boolean; virtual;
      function  MustCreateDragHandler : boolean; virtual;
      function  MustCreateFindHandler : boolean; virtual;

      procedure ApplyZoomStep;
      function  GetParentForm : TCustomForm;

      {$IFDEF MSWINDOWS}
      procedure FreeAndNilStub(var aStub : pointer);
      procedure CreateStub(const aMethod : TWndMethod; var aStub : Pointer);
      procedure BrowserCompWndProc(var aMessage: TMessage);
      procedure WidgetCompWndProc(var aMessage: TMessage);
      procedure RenderCompWndProc(var aMessage: TMessage);
      {$ENDIF}

      // IChromiumEvents
      procedure GetSettings(var aSettings : TCefBrowserSettings);

      // ICefClient
      function  doOnProcessMessageReceived(const browser: ICefBrowser; sourceProcess: TCefProcessId; const aMessage: ICefProcessMessage): Boolean; virtual;

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
      function  doOnBeforePopup(const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var noJavascriptAccess: Boolean): Boolean; virtual;
      procedure doOnAfterCreated(const browser: ICefBrowser); virtual;
      procedure doOnBeforeClose(const browser: ICefBrowser); virtual;
      function  doOnClose(const browser: ICefBrowser): Boolean; virtual;

      // ICefRequestHandler
      function  doOnBeforeBrowse(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; user_gesture, isRedirect: Boolean): Boolean; virtual;
      function  doOnOpenUrlFromTab(const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean): Boolean; virtual;
      function  doOnBeforeResourceLoad(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const callback: ICefRequestCallback): TCefReturnValue; virtual;
      function  doOnGetResourceHandler(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest): ICefResourceHandler; virtual;
      procedure doOnResourceRedirect(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; var newUrl: ustring); virtual;
      function  doOnResourceResponse(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse): Boolean; virtual;
      function  doOnGetResourceResponseFilter(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse): ICefResponseFilter; virtual;
      procedure doOnResourceLoadComplete(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; status: TCefUrlRequestStatus; receivedContentLength: Int64); virtual;
      function  doOnGetAuthCredentials(const browser: ICefBrowser; const frame: ICefFrame; isProxy: Boolean; const host: ustring; port: Integer; const realm, scheme: ustring; const callback: ICefAuthCallback): Boolean; virtual;
      function  doCanGetCookies(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest): boolean; virtual;
      function  doCanSetCookie(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const cookie : PCefCookie): boolean; virtual;
      function  doOnQuotaRequest(const browser: ICefBrowser; const originUrl: ustring; newSize: Int64; const callback: ICefRequestCallback): Boolean; virtual;
      procedure doOnProtocolExecution(const browser: ICefBrowser; const url: ustring; out allowOsExecution: Boolean); virtual;
      function  doOnCertificateError(const browser: ICefBrowser; certError: TCefErrorcode; const requestUrl: ustring; const sslInfo: ICefSslInfo; const callback: ICefRequestCallback): Boolean; virtual;
      function  doOnSelectClientCertificate(const browser: ICefBrowser; isProxy: boolean; const host: ustring; port: integer; certificatesCount: NativeUInt; const certificates: TCefX509CertificateArray; const callback: ICefSelectClientCertificateCallback): boolean; virtual;
      procedure doOnPluginCrashed(const browser: ICefBrowser; const pluginPath: ustring); virtual;
      procedure doOnRenderViewReady(const browser: ICefBrowser); virtual;
      procedure doOnRenderProcessTerminated(const browser: ICefBrowser; status: TCefTerminationStatus); virtual;

      // ICefDialogHandler
      function  doOnFileDialog(const browser: ICefBrowser; mode: TCefFileDialogMode; const title, defaultFilePath: ustring; const acceptFilters: TStrings; selectedAcceptFilter: Integer; const callback: ICefFileDialogCallback): Boolean; virtual;

      // ICefRenderHandler
      procedure doOnGetAccessibilityHandler(var aAccessibilityHandler : ICefAccessibilityHandler); virtual;
      function  doOnGetRootScreenRect(const browser: ICefBrowser; var rect: TCefRect): Boolean; virtual;
      function  doOnGetViewRect(const browser: ICefBrowser; var rect: TCefRect): Boolean; virtual;
      function  doOnGetScreenPoint(const browser: ICefBrowser; viewX, viewY: Integer; var screenX, screenY: Integer): Boolean; virtual;
      function  doOnGetScreenInfo(const browser: ICefBrowser; var screenInfo: TCefScreenInfo): Boolean; virtual;
      procedure doOnPopupShow(const browser: ICefBrowser; show: Boolean); virtual;
      procedure doOnPopupSize(const browser: ICefBrowser; const rect: PCefRect); virtual;
      procedure doOnPaint(const browser: ICefBrowser; kind: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; const buffer: Pointer; width, height: Integer); virtual;
      procedure doOnCursorChange(const browser: ICefBrowser; cursor: TCefCursorHandle; cursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo); virtual;
      function  doOnStartDragging(const browser: ICefBrowser; const dragData: ICefDragData; allowedOps: TCefDragOperations; x, y: Integer): Boolean; virtual;
      procedure doOnUpdateDragCursor(const browser: ICefBrowser; operation: TCefDragOperation); virtual;
      procedure doOnScrollOffsetChanged(const browser: ICefBrowser; x, y: Double); virtual;
      procedure doOnIMECompositionRangeChanged(const browser: ICefBrowser; const selected_range: PCefRange; character_boundsCount: NativeUInt; const character_bounds: PCefRect); virtual;
      procedure doOnTextSelectionChanged(const browser: ICefBrowser; const selected_text: ustring; const selected_range: PCefRange); virtual;

      // ICefDragHandler
      function  doOnDragEnter(const browser: ICefBrowser; const dragData: ICefDragData; mask: TCefDragOperations): Boolean; virtual;
      procedure doOnDraggableRegionsChanged(const browser: ICefBrowser; regionsCount: NativeUInt; regions: PCefDraggableRegionArray); virtual;

      // ICefFindHandler
      procedure doOnFindResult(const browser: ICefBrowser; identifier, count: Integer; const selectionRect: PCefRect; activeMatchOrdinal: Integer; finalUpdate: Boolean); virtual;

      // Custom
      procedure doCookiesDeleted(numDeleted : integer); virtual;
      procedure doPdfPrintFinished(aResultOK : boolean); virtual;
      procedure doTextResultAvailable(const aText : ustring); virtual;
      procedure doUpdatePreferences(const aBrowser: ICefBrowser); virtual;
      procedure doUpdateOwnPreferences; virtual;
      function  doSavePreferences : boolean; virtual;
      procedure doResolvedHostAvailable(result: TCefErrorCode; const resolvedIps: TStrings); virtual;

    public
      constructor Create(AOwner: TComponent); override;
      destructor  Destroy; override;
      procedure   AfterConstruction; override;
      procedure   BeforeDestruction; override;
      function    CreateClientHandler(aIsOSR : boolean = True) : boolean; overload;
      function    CreateClientHandler(var aClient : ICefClient; aIsOSR : boolean = True) : boolean; overload;
      procedure   CloseBrowser(aForceClose : boolean);
      function    ShareRequestContext(var aContext : ICefRequestContext; const aHandler : ICefRequestContextHandler = nil) : boolean;
      function    CreateBrowser(const aWindowName : ustring = ''; const aContext : ICefRequestContext = nil; const aCookiesPath : ustring = ''; aPersistSessionCookies : boolean = False) : boolean; overload; virtual;
      {$IFDEF MSWINDOWS}
      function    CreateBrowser(aParentHandle : HWND; aParentRect : TRect; const aWindowName : ustring = ''; const aContext : ICefRequestContext = nil; const aCookiesPath : ustring = ''; aPersistSessionCookies : boolean = False) : boolean; overload; virtual;
      {$ENDIF}

      procedure   LoadURL(const aURL : ustring; const aFrameName : ustring = ''); overload;
      procedure   LoadURL(const aURL : ustring; const aFrame : ICefFrame); overload;
      procedure   LoadURL(const aURL : ustring; const aFrameIdentifier : int64); overload;
      procedure   LoadString(const aString : ustring; const aURL : ustring = '');
      procedure   LoadRequest(const aRequest: ICefRequest);

      procedure   GoBack;
      procedure   GoForward;
      procedure   Reload;
      procedure   ReloadIgnoreCache;
      procedure   StopLoad;
      procedure   StartDownload(const aURL : ustring);

      procedure   SimulateMouseWheel(aDeltaX, aDeltaY : integer);
      function    DeleteCookies(const url : ustring = ''; const cookieName : ustring = '') : boolean;
      procedure   RetrieveHTML(const aFrameName : ustring = ''); overload;
      procedure   RetrieveHTML(const aFrame : ICefFrame); overload;
      procedure   RetrieveHTML(const aFrameIdentifier : int64); overload;
      procedure   RetrieveText(const aFrameName : ustring = ''); overload;
      procedure   RetrieveText(const aFrame : ICefFrame); overload;
      procedure   RetrieveText(const aFrameIdentifier : int64); overload;
      function    GetFrameNames(var aFrameNames : TStrings) : boolean;
      function    GetFrameIdentifiers(var aFrameCount : NativeUInt; var aFrameIdentifierArray : TCefFrameIdentifierArray) : boolean;
      procedure   ExecuteJavaScript(const aCode, aScriptURL : ustring; const aFrameName : ustring = ''; aStartLine : integer = 0); overload;
      procedure   ExecuteJavaScript(const aCode, aScriptURL : ustring; const aFrame : ICefFrame; aStartLine : integer = 0); overload;
      procedure   ExecuteJavaScript(const aCode, aScriptURL : ustring; const aFrameIdentifier : int64; aStartLine : integer = 0); overload;
      procedure   UpdatePreferences;
      procedure   SavePreferences(const aFileName : string);
      procedure   ResolveHost(const aURL : ustring);
      function    IsSameBrowser(const aBrowser : ICefBrowser) : boolean;

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
      procedure   SendKeyEvent(const event: PCefKeyEvent);
      procedure   SendMouseClickEvent(const event: PCefMouseEvent; kind: TCefMouseButtonType; mouseUp: Boolean; clickCount: Integer);
      procedure   SendMouseMoveEvent(const event: PCefMouseEvent; mouseLeave: Boolean);
      procedure   SendMouseWheelEvent(const event: PCefMouseEvent; deltaX, deltaY: Integer);
      procedure   SendFocusEvent(setFocus: Boolean);
      procedure   SendCaptureLostEvent;
      function    SendProcessMessage(targetProcess: TCefProcessId; const ProcMessage: ICefProcessMessage): Boolean;
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
      property  CookiePrefs             : integer                      read FCookiePrefs              write SetCookiePrefs;
      property  ImagesPrefs             : integer                      read FImagesPrefs              write SetImagesPrefs;
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

    published
      property  OnTextResultAvailable   : TOnTextResultAvailableEvent  read FOnTextResultAvailable    write FOnTextResultAvailable;
      property  OnPdfPrintFinished      : TOnPdfPrintFinishedEvent     read FOnPdfPrintFinished       write FOnPdfPrintFinished;
      property  OnCookiesDeleted        : TOnCookiesDeletedEvent       read FOnCookiesDeleted         write FOnCookiesDeleted;
      property  OnResolvedHostAvailable : TOnResolvedIPsAvailableEvent read FOnResolvedHostAvailable  write FOnResolvedHostAvailable;
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
      property OnBeforeBrowse                   : TOnBeforeBrowse                   read FOnBeforeBrowse                   write FOnBeforeBrowse;
      property OnOpenUrlFromTab                 : TOnOpenUrlFromTab                 read FOnOpenUrlFromTab                 write FOnOpenUrlFromTab;
      property OnBeforeResourceLoad             : TOnBeforeResourceLoad             read FOnBeforeResourceLoad             write FOnBeforeResourceLoad;
      property OnGetResourceHandler             : TOnGetResourceHandler             read FOnGetResourceHandler             write FOnGetResourceHandler;
      property OnResourceRedirect               : TOnResourceRedirect               read FOnResourceRedirect               write FOnResourceRedirect;
      property OnResourceResponse               : TOnResourceResponse               read FOnResourceResponse               write FOnResourceResponse;
      property OnGetResourceResponseFilter      : TOnGetResourceResponseFilter      read FOnGetResourceResponseFilter      write FOnGetResourceResponseFilter;
      property OnResourceLoadComplete           : TOnResourceLoadComplete           read FOnResourceLoadComplete           write FOnResourceLoadComplete;
      property OnGetAuthCredentials             : TOnGetAuthCredentials             read FOnGetAuthCredentials             write FOnGetAuthCredentials;
      property OnCanGetCookies                  : TOnCanGetCookies                  read FOnCanGetCookies                  write FOnCanGetCookies;
      property OnCanSetCookie                   : TOnCanSetCookie                   read FOnCanSetCookie                   write FOnCanSetCookie;
      property OnQuotaRequest                   : TOnQuotaRequest                   read FOnQuotaRequest                   write FOnQuotaRequest;
      property OnProtocolExecution              : TOnProtocolExecution              read FOnProtocolExecution              write FOnProtocolExecution;
      property OnCertificateError               : TOnCertificateError               read FOnCertificateError               write FOnCertificateError;
      property OnSelectClientCertificate        : TOnSelectClientCertificate        read FOnSelectClientCertificate        write FOnSelectClientCertificate;
      property OnPluginCrashed                  : TOnPluginCrashed                  read FOnPluginCrashed                  write FOnPluginCrashed;
      property OnRenderViewReady                : TOnRenderViewReady                read FOnRenderViewReady                write FOnRenderViewReady;
      property OnRenderProcessTerminated        : TOnRenderProcessTerminated        read FOnRenderProcessTerminated        write FOnRenderProcessTerminated;

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
      property OnCursorChange                   : TOnCursorChange                   read FOnCursorChange                   write FOnCursorChange;
      property OnScrollOffsetChanged            : TOnScrollOffsetChanged            read FOnScrollOffsetChanged            write FOnScrollOffsetChanged;
      property OnIMECompositionRangeChanged     : TOnIMECompositionRangeChanged     read FOnIMECompositionRangeChanged     write FOnIMECompositionRangeChanged;
      property OnTextSelectionChanged           : TOnTextSelectionChanged           read FOnTextSelectionChanged           write FOnTextSelectionChanged;

      // ICefDragHandler
      property OnDragEnter                      : TOnDragEnter                      read FOnDragEnter                      write FOnDragEnter;
      property OnDraggableRegionsChanged        : TOnDraggableRegionsChanged        read FOnDraggableRegionsChanged        write FOnDraggableRegionsChanged;

      // ICefFindHandler
      property OnFindResult                     : TOnFindResult                     read FOnFindResult                     write FOnFindResult;

  end;

implementation

uses
  System.SysUtils, System.Math,
  uCEFBrowser, uCEFValue, uCEFDictionaryValue, uCEFStringMultimap, uCEFFrame,
  uCEFApplication, uCEFProcessMessage, uCEFRequestContext,
  uCEFPDFPrintCallback, uCEFResolveCallback, uCEFDeleteCookiesCallback, uCEFStringVisitor,
  uCEFListValue;

constructor TFMXChromium.Create(AOwner: TComponent);
begin
  FBrowser                := nil;
  FBrowserId              := 0;
  FClosing                := False;
  FIsOSR                  := False;
  FInitialized            := False;
  FDefaultUrl             := 'about:blank';
  FHandler                := nil;
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
  FCookiePrefs            := CEF_CONTENT_SETTING_ALLOW;
  FImagesPrefs            := CEF_CONTENT_SETTING_ALLOW;
  FZoomStep               := ZOOM_STEP_DEF;

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

  FProxyType         := CEF_PROXYTYPE_DIRECT;
  FProxyScheme       := psHTTP;
  FProxyServer       := '';
  FProxyPort         := 80;
  FProxyUsername     := '';
  FProxyPassword     := '';
  FProxyScriptURL    := '';
  FProxyByPassList   := '';

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
  if (FBrowserCompHWND <> 0) and (FOldBrowserCompWndPrc <> nil) then
    begin
      SetWindowLongPtr(FBrowserCompHWND, GWL_WNDPROC, NativeInt(FOldBrowserCompWndPrc));
      FreeAndNilStub(FBrowserCompStub);
      FOldBrowserCompWndPrc := nil;
    end;

  if (FWidgetCompHWND <> 0) and (FOldWidgetCompWndPrc <> nil) then
    begin
      SetWindowLongPtr(FWidgetCompHWND, GWL_WNDPROC, NativeInt(FOldWidgetCompWndPrc));
      FreeAndNilStub(FWidgetCompStub);
      FOldWidgetCompWndPrc := nil;
    end;

  if (FRenderCompHWND <> 0) and (FOldRenderCompWndPrc <> nil) then
    begin
      SetWindowLongPtr(FRenderCompHWND, GWL_WNDPROC, NativeInt(FOldRenderCompWndPrc));
      FreeAndNilStub(FRenderCompStub);
      FOldRenderCompWndPrc := nil;
    end;
  {$ENDIF}

  DestroyClientHandler;

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
        FHandler := TCustomClientHandler.Create(Self,
                                                MustCreateLoadHandler,
                                                MustCreateFocusHandler,
                                                MustCreateContextMenuHandler,
                                                MustCreateDialogHandler,
                                                MustCreateKeyboardHandler,
                                                MustCreateDisplayHandler,
                                                MustCreateDownloadHandler,
                                                MustCreateJsDialogHandler,
                                                True,
                                                FIsOSR,
                                                True,
                                                MustCreateDragHandler,
                                                MustCreateFindHandler);

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
  FOnBeforeBrowse                 := nil;
  FOnOpenUrlFromTab               := nil;
  FOnBeforeResourceLoad           := nil;
  FOnGetResourceHandler           := nil;
  FOnResourceRedirect             := nil;
  FOnResourceResponse             := nil;
  FOnGetResourceResponseFilter    := nil;
  FOnResourceLoadComplete         := nil;
  FOnGetAuthCredentials           := nil;
  FOnCanGetCookies                := nil;
  FOnCanSetCookie                 := nil;
  FOnQuotaRequest                 := nil;
  FOnProtocolExecution            := nil;
  FOnCertificateError             := nil;
  FOnSelectClientCertificate      := nil;
  FOnPluginCrashed                := nil;
  FOnRenderViewReady              := nil;
  FOnRenderProcessTerminated      := nil;

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
  FOnCursorChange                 := nil;
  FOnScrollOffsetChanged          := nil;
  FOnIMECompositionRangeChanged   := nil;
  FOnTextSelectionChanged         := nil;

  // ICefDragHandler
  FOnDragEnter                    := nil;
  FOnDraggableRegionsChanged      := nil;

  // ICefFindHandler
  FOnFindResult                   := nil;

  // Custom
  FOnTextResultAvailable          := nil;
  FOnPdfPrintFinished             := nil;
  FOnCookiesDeleted               := nil;
  FOnResolvedHostAvailable        := nil;
end;

function TFMXChromium.CreateBrowser(const aWindowName  : ustring;
                                    const aContext     : ICefRequestContext;
                                    const aCookiesPath : ustring;
                                          aPersistSessionCookies : boolean) : boolean;
var
  TempCookieManager : ICefCookieManager;
begin
  Result := False;

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

        if (aContext <> nil) and (length(aCookiesPath) > 0) then
          begin
            TempCookieManager := aContext.GetDefaultCookieManager(nil);

            if (TempCookieManager = nil) or
               not(TempCookieManager.SetStoragePath(aCookiesPath, aPersistSessionCookies, nil)) then
              OutputDebugMessage('TFMXChromium.CreateBrowser error : cookies cannot be accessed');
          end;


        if GlobalCEFApp.MultiThreadedMessageLoop then
          Result := CreateBrowserHost(@FWindowInfo, FDefaultUrl, @FBrowserSettings, aContext)
         else
          Result := CreateBrowserHostSync(@FWindowInfo, FDefaultUrl, @FBrowserSettings, aContext);
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TFMXChromium.CreateBrowser', e) then raise;
  end;
end;

{$IFDEF MSWINDOWS}
function TFMXChromium.CreateBrowser(      aParentHandle          : HWND;
                                          aParentRect            : TRect;
                                    const aWindowName            : ustring;
                                    const aContext               : ICefRequestContext;
                                    const aCookiesPath           : ustring;
                                          aPersistSessionCookies : boolean) : boolean;
var
  TempCookieManager : ICefCookieManager;
begin
  Result := False;

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

        if FIsOSR then
          WindowInfoAsWindowless(FWindowInfo, 0, aWindowName)
         else
          WindowInfoAsChild(FWindowInfo, aParentHandle, aParentRect, aWindowName);


        if (aContext <> nil) and (length(aCookiesPath) > 0) then
          begin
            TempCookieManager := aContext.GetDefaultCookieManager(nil);

            if (TempCookieManager = nil) or
               not(TempCookieManager.SetStoragePath(aCookiesPath, aPersistSessionCookies, nil)) then
              OutputDebugMessage('TChromium.CreateBrowser error : cookies cannot be accessed');
          end;


        if GlobalCEFApp.MultiThreadedMessageLoop then
          Result := CreateBrowserHost(@FWindowInfo, FDefaultUrl, @FBrowserSettings, aContext)
         else
          Result := CreateBrowserHostSync(@FWindowInfo, FDefaultUrl, @FBrowserSettings, aContext);
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TFMXChromium.CreateBrowser', e) then raise;
  end;
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

function TFMXChromium.CreateBrowserHost(aWindowInfo     : PCefWindowInfo;
                                        const aURL      : ustring;
                                        const aSettings : PCefBrowserSettings;
                                        const aContext  : ICefRequestContext): Boolean;
var
  TempURL : TCefString;
begin
  TempURL := CefString(aURL);
  Result  := cef_browser_host_create_browser(aWindowInfo, FHandler.Wrap, @TempURL, aSettings, CefGetData(aContext)) <> 0;
end;

function TFMXChromium.CreateBrowserHostSync(aWindowInfo     : PCefWindowInfo;
                                            const aURL      : ustring;
                                            const aSettings : PCefBrowserSettings;
                                            const aContext  : ICefRequestContext): boolean;
var
  TempURL : TCefString;
begin
  TempURL  := CefString(aURL);
  FBrowser := TCefBrowserRef.UnWrap(cef_browser_host_create_browser_sync(aWindowInfo, FHandler.Wrap, @TempURL, aSettings, CefGetData(aContext)));

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

      if (TempFrame <> nil) then TempFrame.Copy;
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

      if (TempFrame <> nil) then TempFrame.Paste;
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

      if (TempFrame <> nil) then TempFrame.Cut;
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

      if (TempFrame <> nil) then TempFrame.Undo;
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

      if (TempFrame <> nil) then TempFrame.Redo;
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

      if (TempFrame <> nil) then TempFrame.Del;
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

      if (TempFrame <> nil) then TempFrame.SelectAll;
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
procedure TFMXChromium.LoadURL(const aURL : ustring; const aFrameName : ustring = '');
var
  TempFrame : ICefFrame;
begin
  if Initialized then
    begin
      if (length(aFrameName) > 0) then
        TempFrame := FBrowser.GetFrame(aFrameName)
       else
        TempFrame := FBrowser.MainFrame;

      if (TempFrame <> nil) then TempFrame.LoadUrl(aURL);
    end;
end;

procedure TFMXChromium.LoadURL(const aURL : ustring; const aFrame : ICefFrame);
begin
  if Initialized and (aFrame <> nil) then aFrame.LoadUrl(aURL);
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

      if (TempFrame <> nil) then TempFrame.LoadUrl(aURL);
    end;
end;

procedure TFMXChromium.LoadString(const aString : ustring; const aURL : ustring);
var
  TempFrame : ICefFrame;
begin
  if Initialized then
    begin
      TempFrame := FBrowser.MainFrame;
      if (TempFrame <> nil) then TempFrame.LoadString(aString, aURL);
    end;
end;

procedure TFMXChromium.LoadRequest(const aRequest: ICefRequest);
var
  TempFrame : ICefFrame;
begin
  if Initialized then
    begin
      TempFrame := FBrowser.MainFrame;
      if (TempFrame <> nil) then TempFrame.LoadRequest(aRequest);
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
      if (TempFrame <> nil) then Result := TempFrame.URL;
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

procedure TFMXChromium.SetCookiePrefs(aValue : integer);
begin
  if (FCookiePrefs <> aValue) then
    begin
      FCookiePrefs       := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TFMXChromium.SetImagesPrefs(aValue : integer);
begin
  if (FImagesPrefs <> aValue) then
    begin
      FImagesPrefs       := aValue;
      FUpdatePreferences := True;
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

function TFMXChromium.DeleteCookies(const url, cookieName: ustring) : boolean;
var
  TempManager  : ICefCookieManager;
  TempCallback : ICefDeleteCookiesCallback;
begin
  Result := False;

  if Initialized and (FBrowser.Host <> nil) and (FBrowser.Host.RequestContext <> nil) then
    begin
      TempManager := FBrowser.Host.RequestContext.GetDefaultCookieManager(nil);

      if (TempManager <> nil) then
        begin
          TempCallback := TCefCustomDeleteCookiesCallback.Create(self);
          Result       := TempManager.DeleteCookies(url, cookieName, TempCallback);
        end;
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

      if (TempFrame <> nil) then
        begin
          TempVisitor := TCustomCefStringVisitor.Create(self);
          TempFrame.GetSource(TempVisitor);
        end;
    end;
end;

procedure TFMXChromium.RetrieveHTML(const aFrame : ICefFrame);
var
  TempVisitor : ICefStringVisitor;
begin
  if Initialized and (aFrame <> nil) then
    begin
      TempVisitor := TCustomCefStringVisitor.Create(self);
      aFrame.GetSource(TempVisitor);
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

      if (TempFrame <> nil) then
        begin
          TempVisitor := TCustomCefStringVisitor.Create(self);
          TempFrame.GetSource(TempVisitor);
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

      if (TempFrame <> nil) then
        begin
          TempVisitor := TCustomCefStringVisitor.Create(self);
          TempFrame.GetText(TempVisitor);
        end;
    end;
end;

procedure TFMXChromium.RetrieveText(const aFrame : ICefFrame);
var
  TempVisitor : ICefStringVisitor;
begin
  if Initialized and (aFrame <> nil) then
    begin
      TempVisitor := TCustomCefStringVisitor.Create(self);
      aFrame.GetText(TempVisitor);
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

      if (TempFrame <> nil) then
        begin
          TempVisitor := TCustomCefStringVisitor.Create(self);
          TempFrame.GetText(TempVisitor);
        end;
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
    begin
      TempTask := TCefUpdatePrefsTask.Create(self);
      CefPostTask(TID_UI, TempTask);
    end;
end;

procedure TFMXChromium.SavePreferences(const aFileName : string);
var
  TempTask: ICefTask;
begin
  if Initialized and (length(aFileName) > 0) then
    begin
      FPrefsFileName := aFileName;
      TempTask       := TCefSavePrefsTask.Create(self);
      CefPostTask(TID_UI, TempTask);
    end;
end;

procedure TFMXChromium.ResolveHost(const aURL : ustring);
var
  TempCallback : ICefResolveCallback;
begin
  // Results will be received in the OnResolvedHostAvailable event of this class
  if Initialized and (length(aURL) > 0) then
    begin
      TempCallback := TCefCustomResolveCallback.Create(self);
      FBrowser.Host.RequestContext.ResolveHost(aURL, TempCallback);
    end;
end;

function TFMXChromium.IsSameBrowser(const aBrowser : ICefBrowser) : boolean;
begin
  Result := Initialized and (aBrowser <> nil) and FBrowser.IsSame(aBrowser);
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

  UpdateProxyPrefs(aBrowser);
  UpdatePreference(aBrowser, 'enable_do_not_track',                  FDoNotTrack);
  UpdatePreference(aBrowser, 'enable_referrers',                     FSendReferrer);
  UpdatePreference(aBrowser, 'enable_a_ping',                        FHyperlinkAuditing);
  UpdatePreference(aBrowser, 'plugins.run_all_flash_in_allow_mode',  FRunAllFlashInAllowMode);
  UpdatePreference(aBrowser, 'plugins.allow_outdated',               FAllowOutdatedPlugins);
  UpdatePreference(aBrowser, 'plugins.always_authorize',             FAlwaysAuthorizePlugins);
  UpdatePreference(aBrowser, 'browser.enable_spellchecking',         FSpellChecking);
  UpdateStringListPref(aBrowser, 'spellcheck.dictionaries',          FSpellCheckerDicts);

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
end;

function TFMXChromium.UpdatePreference(const aBrowser: ICefBrowser; const aName : ustring; aValue : boolean) : boolean;
var
  TempError : ustring;
  TempValue : ICefValue;
begin
  Result := False;

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
end;

function TFMXChromium.UpdatePreference(const aBrowser: ICefBrowser; const aName : ustring; aValue : integer) : boolean;
var
  TempError : ustring;
  TempValue : ICefValue;
begin
  Result := False;

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
end;

function TFMXChromium.UpdatePreference(const aBrowser: ICefBrowser; const aName : ustring; const aValue : double) : boolean;
var
  TempError : ustring;
  TempValue : ICefValue;
begin
  Result := False;

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
end;

function TFMXChromium.UpdatePreference(const aBrowser: ICefBrowser; const aName, aValue : ustring) : boolean;
var
  TempError : ustring;
  TempValue : ICefValue;
begin
  Result := False;

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

function TFMXChromium.MustCreateDragHandler : boolean;
begin
  Result := assigned(FOnDragEnter) or
            assigned(FOnDraggableRegionsChanged);
end;

function TFMXChromium.MustCreateFindHandler : boolean;
begin
  Result := assigned(FOnFindResult);
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

        if (TempFrame <> nil) then
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
    if Initialized and (aFrame <> nil) then
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

        if (TempFrame <> nil) then
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

function TFMXChromium.doOnClose(const browser: ICefBrowser): Boolean;
begin
  Result := False;

  if (browser <> nil) and (FBrowserId = browser.Identifier) then FClosing := True;

  if Assigned(FOnClose) then FOnClose(Self, browser, Result);
end;

procedure TFMXChromium.doOnBeforeClose(const browser: ICefBrowser);
begin
  if (browser <> nil) and (FBrowserId = browser.Identifier) then
    begin
      FInitialized := False;
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
                                      var   noJavascriptAccess : Boolean): Boolean;
begin
  Result := False;

  if Assigned(FOnBeforePopup) then
    FOnBeforePopup(Self, browser, frame, targetUrl, targetFrameName,
                   targetDisposition, userGesture, popupFeatures, windowInfo, client,
                   settings, noJavascriptAccess, Result);
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
                                                         regionsCount : NativeUInt;
                                                         regions      : PCefDraggableRegionArray);
begin
  if Assigned(FOnDraggableRegionsChanged) then FOnDraggableRegionsChanged(Self, browser, regionsCount, regions);
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

procedure TFMXChromium.doOnFullScreenModeChange(const browser: ICefBrowser; fullscreen: Boolean);
begin
  if Assigned(FOnFullScreenModeChange) then FOnFullScreenModeChange(Self, browser, fullscreen);
end;

function TFMXChromium.doOnGetAuthCredentials(const browser  : ICefBrowser;
                                             const frame    : ICefFrame;
                                                   isProxy  : Boolean;
                                             const host     : ustring;
                                                   port     : Integer;
                                             const realm    : ustring;
                                             const scheme   : ustring;
                                             const callback : ICefAuthCallback): Boolean;
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
    if (frame <> nil) and frame.IsMain and Assigned(FOnGetAuthCredentials) then
      FOnGetAuthCredentials(Self, browser, frame, isProxy, host, port, realm, scheme, callback, Result);
end;

function TFMXChromium.doCanGetCookies(const browser : ICefBrowser;
                                      const frame   : ICefFrame;
                                      const request : ICefRequest): boolean;
begin
  Result := True;

  if assigned(FOnCanGetCookies) then FOnCanGetCookies(self, browser, frame, request, Result);
end;

function TFMXChromium.doCanSetCookie(const browser : ICefBrowser;
                                     const frame   : ICefFrame;
                                     const request : ICefRequest;
                                     const cookie  : PCefCookie): boolean;
begin
  Result := True;

  if assigned(FOnCanSetCookie) then FOnCanSetCookie(self, browser, frame, request, cookie, Result);
end;

function TFMXChromium.doOnGetResourceHandler(const browser : ICefBrowser;
                                             const frame   : ICefFrame;
                                             const request : ICefRequest): ICefResourceHandler;
begin
  Result := nil;

  if Assigned(FOnGetResourceHandler) then
    FOnGetResourceHandler(Self, browser, frame, request, Result);
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

function TFMXChromium.doOnGetViewRect(const browser: ICefBrowser; var rect: TCefRect): Boolean;
begin
  Result := False;

  if Assigned(FOnGetViewRect) then FOnGetViewRect(Self, browser, rect, Result);
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
                                                       sourceProcess : TCefProcessId;
                                                 const aMessage      : ICefProcessMessage): Boolean;
begin
  Result := False;

  if Assigned(FOnProcessMessageReceived) then
    FOnProcessMessageReceived(Self, browser, sourceProcess, aMessage, Result);
end;

procedure TFMXChromium.doOnProtocolExecution(const browser          : ICefBrowser;
                                             const url              : ustring;
                                             out   allowOsExecution : Boolean);
begin
  if Assigned(FOnProtocolExecution) then FOnProtocolExecution(Self, browser, url, allowOsExecution);
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
begin
  {$IFDEF MSWINDOWS}
  if (browser            <> nil)        and
     (browser.Host       <> nil)        and
     (browser.Identifier =  FBrowserId) then
    begin
      FBrowserCompHWND := browser.Host.WindowHandle;

      if (FBrowserCompHWND <> 0) then
        FWidgetCompHWND := FindWindowEx(FBrowserCompHWND, 0, 'Chrome_WidgetWin_0', '');

      if (FWidgetCompHWND <> 0) then
        FRenderCompHWND := FindWindowEx(FWidgetCompHWND, 0, 'Chrome_RenderWidgetHostHWND', 'Chrome Legacy Window');

      if assigned(FOnBrowserCompMsg) and (FBrowserCompHWND <> 0) and (FOldBrowserCompWndPrc = nil) then
        begin
          CreateStub(BrowserCompWndProc, FBrowserCompStub);
          FOldBrowserCompWndPrc := TFNWndProc(SetWindowLongPtr(FBrowserCompHWND,
                                                               GWL_WNDPROC,
                                                               NativeInt(FBrowserCompStub)));
        end;

      if assigned(FOnWidgetCompMsg) and (FWidgetCompHWND <> 0) and (FOldWidgetCompWndPrc = nil) then
        begin
          CreateStub(WidgetCompWndProc, FWidgetCompStub);
          FOldWidgetCompWndPrc := TFNWndProc(SetWindowLongPtr(FWidgetCompHWND,
                                                              GWL_WNDPROC,
                                                              NativeInt(FWidgetCompStub)));
        end;

      if assigned(FOnRenderCompMsg) and (FRenderCompHWND <> 0) and (FOldRenderCompWndPrc = nil) then
        begin
          CreateStub(RenderCompWndProc, FRenderCompStub);
          FOldRenderCompWndPrc := TFNWndProc(SetWindowLongPtr(FRenderCompHWND,
                                                              GWL_WNDPROC,
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
function TFMXChromium.doOnGetResourceResponseFilter(const browser  : ICefBrowser;
                                                    const frame    : ICefFrame;
                                                    const request  : ICefRequest;
                                                    const response : ICefResponse) : ICefResponseFilter;
begin
  Result := nil;

  if Assigned(FOnGetResourceResponseFilter) then
    FOnGetResourceResponseFilter(self, browser, frame, request, response, Result);
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

procedure TFMXChromium.BrowserCompWndProc(var aMessage: TMessage);
var
  TempHandled : boolean;
begin
  try
    TempHandled := False;

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

function TFMXChromium.SendProcessMessage(targetProcess: TCefProcessId; const ProcMessage: ICefProcessMessage): Boolean;
begin
  Result := Initialized and FBrowser.SendProcessMessage(targetProcess, ProcMessage);
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
