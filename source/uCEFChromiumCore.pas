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
//        Copyright © 2020 Salvador Diaz Fau. All rights reserved.
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

unit uCEFChromiumCore;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  {$IFDEF MSWINDOWS}WinApi.Windows, WinApi.Messages, WinApi.ActiveX, WinApi.CommCtrl,{$ENDIF} System.Classes, System.SyncObjs,
  {$ELSE}
    {$IFDEF MSWINDOWS}Windows, ActiveX, CommCtrl,{$ENDIF} Classes,
    {$IFDEF FPC}
    LCLProc, LCLType, LCLIntf, LResources, LMessages, InterfaceBase,
    {$ELSE}
    Messages,
    {$ENDIF}
    SyncObjs,
  {$ENDIF}
  uCEFTypes, uCEFInterfaces, uCEFLibFunctions, uCEFMiscFunctions, uCEFClient,
  uCEFConstants, uCEFTask, uCEFDomVisitor, uCEFChromiumEvents,
  {$IFDEF MSWINDOWS}uCEFDragAndDropMgr,{$ENDIF}
  uCEFChromiumOptions, uCEFChromiumFontOptions, uCEFPDFPrintOptions;

type
  {$IFNDEF FPC}{$IFDEF DELPHI16_UP}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}{$ENDIF}
  TChromiumCore = class(TComponent, IChromiumEvents)
    protected
      {$IFDEF MSWINDOWS}
      FCompHandle             : HWND;
      {$ENDIF}
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
      FZoomStepCS             : TCriticalSection;
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
      {$IFDEF MSWINDOWS}
      FDragDropManager        : TCEFDragAndDropMgr;
      FDropTargetWnd          : HWND;
      {$ENDIF}
      FDragAndDropInitialized : boolean;
      FWebRTCIPHandlingPolicy : TCefWebRTCHandlingPolicy;
      FWebRTCMultipleRoutes   : TCefState;
      FWebRTCNonProxiedUDP    : TCefState;
      FAcceptLanguageList     : ustring;
      FAcceptCookies          : TCefCookiePref;
      FBlock3rdPartyCookies   : boolean;

      {$IFDEF MSWINDOWS}
      FOldBrowserCompWndPrc   : TFNWndProc;
      FOldWidgetCompWndPrc    : TFNWndProc;
      FOldRenderCompWndPrc    : TFNWndProc;
      FBrowserCompStub        : Pointer;
      FWidgetCompStub         : Pointer;
      FRenderCompStub         : Pointer;
      {$ENDIF}
      FBrowserCompHWND        : THandle;
      FWidgetCompHWND         : THandle;
      FRenderCompHWND         : THandle;

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
      FOnStartDragging                : TOnStartDragging;
      FOnUpdateDragCursor             : TOnUpdateDragCursor;
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
      FOnPrefsAvailable                   : TOnPrefsAvailableEvent;
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
      FOnZoomPctAvailable                 : TOnZoomPctAvailable;
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
      function  GetZoomStep : byte;
      function  GetIsPopUp : boolean;
      function  GetWindowHandle : TCefWindowHandle;
      function  GetWindowlessFrameRate : integer;
      function  GetFrameIsFocused : boolean;
      function  GetInitialized : boolean;
      function  GetVisibleNavigationEntry : ICefNavigationEntry;
      function  GetHasValidMainFrame : boolean;
      function  GetFrameCount : NativeUInt;
      function  GetRequestContextCache : ustring;
      function  GetRequestContextIsGlobal : boolean;
      function  GetAudioMuted : boolean;
      function  GetParentFormHandle : TCefWindowHandle; virtual;

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
      procedure SetAcceptLanguageList(const aValue : ustring);
      procedure SetAcceptCookies(const aValue : TCefCookiePref);
      procedure SetBlock3rdPartyCookies(const aValue : boolean);
      procedure SetOnRequestContextInitialized(const aValue : TOnRequestContextInitialized);
      procedure SetOnBeforePluginLoad(const aValue : TOnBeforePluginLoad);

      function  CreateBrowserHost(aWindowInfo : PCefWindowInfo; const aURL : ustring; const aSettings : PCefBrowserSettings; const aExtraInfo : ICefDictionaryValue; const aContext : ICefRequestContext): boolean;
      function  CreateBrowserHostSync(aWindowInfo : PCefWindowInfo; const aURL : ustring; const aSettings : PCefBrowserSettings; const aExtraInfo : ICefDictionaryValue; const aContext : ICefRequestContext): Boolean;

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

      function  ExecuteUpdateZoomStepTask(aInc : boolean) : boolean;
      function  ExecuteUpdateZoomPctTask(aInc : boolean) : boolean;
      function  ExecuteReadZoomTask : boolean;
      function  ExecuteSetZoomPctTask(const aValue : double) : boolean;
      function  ExecuteSetZoomLevelTask(const aValue : double) : boolean;
      function  ExecuteSetZoomStepTask(aValue : byte) : boolean;

      procedure UpdateHostZoomLevel(const aValue : double);
      procedure UpdateHostZoomPct(const aValue : double);

      procedure DelayedDragging;
      procedure InitializeWindowInfo(aParentHandle : TCefWindowHandle; aParentRect : TRect; const aWindowName : ustring); virtual;
      procedure DefaultInitializeDevToolsWindowInfo(aDevToolsWnd: TCefWindowHandle; const aClientRect: TRect; const aWindowName: ustring);

      {$IFDEF MSWINDOWS}
      procedure PrefsAvailableMsg(aResultOK : boolean);
      function  SendCompMessage(aMsg : cardinal; wParam : cardinal = 0; lParam : integer = 0) : boolean;
      procedure ToMouseEvent(grfKeyState : Longint; pt : TPoint; var aMouseEvent : TCefMouseEvent);
      procedure WndProc(var aMessage: TMessage);
      procedure CreateStub(const aMethod : TWndMethod; var aStub : Pointer);
      procedure FreeAndNilStub(var aStub : pointer);
      function  InstallCompWndProc(aWnd: THandle; aStub: Pointer): TFNWndProc;
      procedure RestoreCompWndProc(var aOldWnd: THandle; aNewWnd: THandle; var aProc: TFNWndProc);
      procedure CallOldCompWndProc(aProc: TFNWndProc; aWnd: THandle; var aMessage: TMessage);
      procedure BrowserCompWndProc(var aMessage: TMessage);
      procedure WidgetCompWndProc(var aMessage: TMessage);
      procedure RenderCompWndProc(var aMessage: TMessage);
      function  CopyDCToBitmapStream(aSrcDC : HDC; const aSrcRect : TRect; var aStream : TStream) : boolean;
      {$ENDIF}

      procedure DragDropManager_OnDragEnter(Sender: TObject; const aDragData : ICefDragData; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint);
      procedure DragDropManager_OnDragOver(Sender: TObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint);
      procedure DragDropManager_OnDragLeave(Sender: TObject);
      procedure DragDropManager_OnDrop(Sender: TObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint);

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
      procedure doOnGetResourceResponseFilter(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; var aResponseFilter: ICefResponseFilter); virtual;
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
      procedure doOnPaint(const browser: ICefBrowser; type_: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; const buffer: Pointer; width, height: Integer); virtual;
      procedure doOnAcceleratedPaint(const browser: ICefBrowser; type_: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; shared_handle: Pointer); virtual;
      procedure doOnCursorChange(const browser: ICefBrowser; cursor: TCefCursorHandle; cursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo); virtual;
      function  doOnStartDragging(const browser: ICefBrowser; const dragData: ICefDragData; allowedOps: TCefDragOperations; x, y: Integer): Boolean; virtual;
      procedure doOnUpdateDragCursor(const browser: ICefBrowser; operation: TCefDragOperation); virtual;
      procedure doOnScrollOffsetChanged(const browser: ICefBrowser; x, y: Double); virtual;
      procedure doOnIMECompositionRangeChanged(const browser: ICefBrowser; const selected_range: PCefRange; character_boundsCount: NativeUInt; const character_bounds: PCefRect); virtual;
      procedure doOnTextSelectionChanged(const browser: ICefBrowser; const selected_text: ustring; const selected_range: PCefRange); virtual;
      procedure doOnVirtualKeyboardRequested(const browser: ICefBrowser; input_mode: TCefTextInpuMode); virtual;

      // ICefDragHandler
      function  doOnDragEnter(const browser: ICefBrowser; const dragData: ICefDragData; mask: TCefDragOperations): Boolean; virtual;
      procedure doOnDraggableRegionsChanged(const browser: ICefBrowser; const frame: ICefFrame; regionsCount: NativeUInt; const regions: PCefDraggableRegionArray); virtual;

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
      procedure doUpdateZoomStep(aInc : boolean); virtual;
      procedure doUpdateZoomPct(aInc : boolean); virtual;
      procedure doReadZoom; virtual;
      procedure doSetZoomLevel(const aValue : double); virtual;
      procedure doSetZoomPct(const aValue : double); virtual;
      procedure doSetZoomStep(aValue : byte); virtual;
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

      property  ParentFormHandle   : TCefWindowHandle   read   GetParentFormHandle;

    public
      constructor Create(AOwner: TComponent); override;
      destructor  Destroy; override;
      procedure   AfterConstruction; override;
      procedure   BeforeDestruction; override;
      function    CreateClientHandler(aIsOSR : boolean = True) : boolean; overload;
      function    CreateClientHandler(var aClient : ICefClient; aIsOSR : boolean = True) : boolean; overload;
      procedure   CloseBrowser(aForceClose : boolean);
      function    ShareRequestContext(var aContext : ICefRequestContext; const aHandler : ICefRequestContextHandler = nil) : boolean;
      {$IFDEF MSWINDOWS}
      procedure   InitializeDragAndDrop(const aDropTargetWnd : HWND);
      procedure   ShutdownDragAndDrop;
      {$ENDIF MSWINDOWS}

      function    CreateBrowser(aParentHandle : TCefWindowHandle; aParentRect : TRect; const aWindowName : ustring = ''; const aContext : ICefRequestContext = nil; const aExtraInfo : ICefDictionaryValue = nil) : boolean; overload; virtual;

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
      function    SetNewBrowserParent(aNewParentHwnd : HWND) : boolean;
      procedure   ResolveHost(const aURL : ustring);
      function    IsSameBrowser(const aBrowser : ICefBrowser) : boolean;
      function    ExecuteTaskOnCefThread(aCefThreadId : TCefThreadId; aTaskID : cardinal; aDelayMs : Int64 = 0) : boolean;

      function    DeleteCookies(const url : ustring = ''; const cookieName : ustring = ''; aDeleteImmediately : boolean = False) : boolean;
      function    VisitAllCookies(aID : integer = 0) : boolean;
      function    VisitURLCookies(const url : ustring; includeHttpOnly : boolean = False; aID : integer = 0) : boolean;
      function    SetCookie(const url, name_, value, domain, path: ustring; secure, httponly, hasExpires: Boolean; const creation, lastAccess, expires: TDateTime; aSetImmediately : boolean = True; aID : integer = 0): Boolean;
      function    FlushCookieStore(aFlushImmediately : boolean = True) : boolean;
      procedure   UpdateSupportedSchemes(const aSchemes : TStrings; aIncludeDefaults : boolean = True);

      procedure   ShowDevTools(const inspectElementAt: TPoint; aWindowInfo: PCefWindowInfo);
      procedure   CloseDevTools(const aDevToolsWnd : TCefWindowHandle = 0);

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
      procedure   IncZoomPct;
      procedure   DecZoomPct;
      procedure   ResetZoomStep;
      procedure   ResetZoomLevel;
      procedure   ResetZoomPct;
      procedure   ReadZoom;

      procedure   WasResized;
      procedure   WasHidden(hidden: Boolean);
      procedure   NotifyScreenInfoChanged;
      procedure   NotifyMoveOrResizeStarted;
      procedure   Invalidate(type_: TCefPaintElementType = PET_VIEW);
      procedure   SendExternalBeginFrame;
      procedure   SendKeyEvent(const event: PCefKeyEvent);
      procedure   SendMouseClickEvent(const event: PCefMouseEvent; type_: TCefMouseButtonType; mouseUp: Boolean; clickCount: Integer);
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
      procedure   DragTargetDrop(const event: PCefMouseEvent);
      procedure   DragSourceEndedAt(x, y: Integer; op: TCefDragOperation);
      procedure   DragSourceSystemDragEnded;

      procedure   IMESetComposition(const text: ustring; const underlines : TCefCompositionUnderlineDynArray; const replacement_range, selection_range : PCefRange);
      procedure   IMECommitText(const text: ustring; const replacement_range : PCefRange; relative_cursor_pos : integer);
      procedure   IMEFinishComposingText(keep_selection : boolean);
      procedure   IMECancelComposition;


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
      property  WindowHandle            : TCefWindowHandle             read GetWindowHandle;
      {$IFDEF MSWINDOWS}
      property  BrowserHandle           : THandle                      read FBrowserCompHWND;
      property  WidgetHandle            : THandle                      read FWidgetCompHWND;
      property  RenderHandle            : THandle                      read FRenderCompHWND;
      {$ENDIF}
      property  FrameIsFocused          : boolean                      read GetFrameIsFocused;
      property  Initialized             : boolean                      read GetInitialized;
      property  RequestContextCache     : ustring                      read GetRequestContextCache;
      property  RequestContextIsGlobal  : boolean                      read GetRequestContextIsGlobal;
      property  DocumentURL             : ustring                      read GetDocumentURL;
      property  ZoomLevel               : double                       read GetZoomLevel              write SetZoomLevel;
      property  ZoomPct                 : double                       read GetZoomPct                write SetZoomPct;
      property  ZoomStep                : byte                         read GetZoomStep               write SetZoomStep;
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
      property  AcceptLanguageList      : ustring                      read FAcceptLanguageList       write SetAcceptLanguageList;
      property  AcceptCookies           : TCefCookiePref               read FAcceptCookies            write SetAcceptCookies;
      property  Block3rdPartyCookies    : boolean                      read FBlock3rdPartyCookies     write SetBlock3rdPartyCookies;

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
      property  OnPrefsAvailable                   : TOnPrefsAvailableEvent                   read FOnPrefsAvailable                   write FOnPrefsAvailable;
      property  OnCookiesDeleted                   : TOnCookiesDeletedEvent                   read FOnCookiesDeleted                   write FOnCookiesDeleted;
      property  OnResolvedHostAvailable            : TOnResolvedIPsAvailableEvent             read FOnResolvedHostAvailable            write FOnResolvedHostAvailable;
      property  OnNavigationVisitorResultAvailable : TOnNavigationVisitorResultAvailableEvent read FOnNavigationVisitorResultAvailable write FOnNavigationVisitorResultAvailable;
      property  OnDownloadImageFinished            : TOnDownloadImageFinishedEvent            read FOnDownloadImageFinished            write FOnDownloadImageFinished;
      property  OnCookiesFlushed                   : TNotifyEvent                             read FOnCookiesFlushed                   write FOnCookiesFlushed;
      property  OnCertificateExceptionsCleared     : TNotifyEvent                             read FOnCertificateExceptionsCleared     write FOnCertificateExceptionsCleared;
      property  OnHttpAuthCredentialsCleared       : TNotifyEvent                             read FOnHttpAuthCredentialsCleared       write FOnHttpAuthCredentialsCleared;
      property  OnAllConnectionsClosed             : TNotifyEvent                             read FOnAllConnectionsClosed             write FOnAllConnectionsClosed;
      property  OnExecuteTaskOnCefThread           : TOnExecuteTaskOnCefThread                read FOnExecuteTaskOnCefThread           write FOnExecuteTaskOnCefThread;
      property  OnCookiesVisited                   : TOnCookiesVisited                        read FOnCookiesVisited                   write FOnCookiesVisited;
      property  OnCookieVisitorDestroyed           : TOnCookieVisitorDestroyed                read FOnCookieVisitorDestroyed           write FOnCookieVisitorDestroyed;
      property  OnCookieSet                        : TOnCookieSet                             read FOnCookieSet                        write FOnCookieSet;
      property  OnZoomPctAvailable                 : TOnZoomPctAvailable                      read FOnZoomPctAvailable                 write FOnZoomPctAvailable;
      {$IFDEF MSWINDOWS}
      property  OnBrowserCompMsg                   : TOnCompMsgEvent                          read FOnBrowserCompMsg                   write FOnBrowserCompMsg;
      property  OnWidgetCompMsg                    : TOnCompMsgEvent                          read FOnWidgetCompMsg                    write FOnWidgetCompMsg;
      property  OnRenderCompMsg                    : TOnCompMsgEvent                          read FOnRenderCompMsg                    write FOnRenderCompMsg;
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
      property OnStartDragging                  : TOnStartDragging                  read FOnStartDragging                  write FOnStartDragging;
      property OnUpdateDragCursor               : TOnUpdateDragCursor               read FOnUpdateDragCursor               write FOnUpdateDragCursor;
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
  {$IFDEF DELPHI16_UP}
  System.SysUtils, System.Math,
  {$ELSE}
  SysUtils, Math,
  {$ENDIF}
  uCEFBrowser, uCEFValue, uCEFDictionaryValue, uCEFStringMultimap, uCEFFrame,
  uCEFApplicationCore, uCEFProcessMessage, uCEFRequestContext,
  {$IFDEF MSWINDOWS}uCEFOLEDragAndDrop,{$ENDIF}
  uCEFPDFPrintCallback, uCEFResolveCallback, uCEFDeleteCookiesCallback, uCEFStringVisitor,
  uCEFListValue, uCEFNavigationEntryVisitor, uCEFDownloadImageCallBack, uCEFCookieManager,
  uCEFRequestContextHandler, uCEFCookieVisitor, uCEFSetCookieCallback, uCEFResourceRequestHandler;

constructor TChromiumCore.Create(AOwner: TComponent);
begin
  FBrowser                := nil;
  FBrowserId              := 0;
  {$IFDEF MSWINDOWS}
  FCompHandle             := 0;
  {$ENDIF}
  FClosing                := False;
  FInitialized            := False;
  FIsOSR                  := False;
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
  FZoomStepCS             := nil;
  FSafeSearch             := False;
  FYouTubeRestrict        := YOUTUBE_RESTRICT_OFF;
  FPrintingEnabled        := True;
  FAcceptLanguageList     := '';
  FAcceptCookies          := cpAllow;
  FBlock3rdPartyCookies   := False;

  {$IFDEF MSWINDOWS}
  FOldBrowserCompWndPrc   := nil;
  FOldWidgetCompWndPrc    := nil;
  FOldRenderCompWndPrc    := nil;
  FBrowserCompStub        := nil;
  FWidgetCompStub         := nil;
  FRenderCompStub         := nil;
  {$ENDIF MSWINDOWS}
  FBrowserCompHWND        := 0;
  FWidgetCompHWND         := 0;
  FRenderCompHWND         := 0;

  FDragOperations         := DRAG_OPERATION_NONE;
  {$IFDEF MSWINDOWS}
  FDragDropManager        := nil;
  FDropTargetWnd          := 0;
  {$ENDIF MSWINDOWS}
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

destructor TChromiumCore.Destroy;
begin
  try
    try
      {$IFDEF MSWINDOWS}
      if (FDragDropManager <> nil) then FreeAndNil(FDragDropManager);

      if (FCompHandle <> 0) then
        begin
          DeallocateHWnd(FCompHandle);
          FCompHandle := 0;
        end;
      {$ENDIF MSWINDOWS}

      ClearBrowserReference;

      if (FFontOptions     <> nil) then FreeAndNil(FFontOptions);
      if (FOptions         <> nil) then FreeAndNil(FOptions);
      if (FPDFPrintOptions <> nil) then FreeAndNil(FPDFPrintOptions);
      if (FZoomStepCS      <> nil) then FreeAndNil(FZoomStepCS);
    except
      on e : exception do
        if CustomExceptionHandler('TChromiumCore.Destroy', e) then raise;
    end;
  finally
    inherited Destroy;
  end;
end;

procedure TChromiumCore.BeforeDestruction;
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

procedure TChromiumCore.ClearBrowserReference;
begin
  FBrowser   := nil;
  FBrowserId := 0;
end;

{$IFDEF MSWINDOWS}
{$IFNDEF FPC}
// Windows XP and newer (older Delphi version < XE don't have them and newer
// require a call to InitCommonControl what isn't necessary.
{type
  SUBCLASSPROC = function(hWnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM;
    uIdSubclass: UINT_PTR; dwRefData: DWORD_PTR): LRESULT; stdcall;
  TSubClassProc = SUBCLASSPROC;

function SetWindowSubclass(hWnd: HWND; pfnSubclass: SUBCLASSPROC; uIdSubclass: UINT_PTR; dwRefData: DWORD_PTR): BOOL; stdcall;
  external comctl32 name 'SetWindowSubclass';
//function GetWindowSubclass(hWnd: HWND; pfnSubclass: SUBCLASSPROC; uIdSubclass: UINT_PTR; var pdwRefData: DWORD_PTR): BOOL; stdcall;
//  external comctl32 name 'GetWindowSubclass';
function RemoveWindowSubclass(hWnd: HWND; pfnSubclass: SUBCLASSPROC; uIdSubclass: UINT_PTR): BOOL; stdcall;
  external comctl32 name 'RemoveWindowSubclass';
function DefSubclassProc(hWnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
  external comctl32 name 'DefSubclassProc';

// We stick with the original implementation because the WndProc stub is a lot
// faster than the WindowSubClass stub that uses the slow GetProp(hWnd). Which
// is extremly slow in Windows 10 1809 and newer.
}

procedure TChromiumCore.CreateStub(const aMethod : TWndMethod; var aStub : Pointer);
begin
  if (aStub = nil) then aStub := MakeObjectInstance(aMethod);
end;

procedure TChromiumCore.FreeAndNilStub(var aStub : pointer);
begin
  if (aStub <> nil) then
    begin
      FreeObjectInstance(aStub);
      aStub := nil;
    end;
end;

function TChromiumCore.InstallCompWndProc(aWnd: THandle; aStub: Pointer): TFNWndProc;
begin
  Result := TFNWndProc(SetWindowLongPtr(aWnd, GWLP_WNDPROC, NativeInt(aStub)));
end;

procedure TChromiumCore.RestoreCompWndProc(var aOldWnd: THandle; aNewWnd: THandle; var aProc: TFNWndProc);
begin
  if (aOldWnd <> 0) and (aOldWnd <> aNewWnd) and (aProc <> nil) then
    begin
      SetWindowLongPtr(aOldWnd, GWLP_WNDPROC, NativeInt(aProc));
      aProc := nil;
      aOldWnd := 0;
    end;
end;

procedure TChromiumCore.CallOldCompWndProc(aProc: TFNWndProc; aWnd: THandle; var aMessage: TMessage);
begin
  if (aProc <> nil) and (aWnd <> 0) then
    aMessage.Result := CallWindowProc(aProc, aWnd, aMessage.Msg, aMessage.wParam, aMessage.lParam);
end;

{$ELSE}

procedure TChromiumCore.CreateStub(const aMethod : TWndMethod; var aStub : Pointer);
begin
  if (aStub = nil) then
    begin
      GetMem(aStub, SizeOf(TWndMethod));
      TWndMethod(aStub^) := aMethod;
    end;
end;

procedure TChromiumCore.FreeAndNilStub(var aStub : pointer);
begin
  if (aStub <> nil) then
    begin
      FreeMem(aStub);
      aStub := nil;
    end;
end;

function CompSubClassProc(hWnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM;
  uIdSubclass: UINT_PTR; dwRefData: DWORD_PTR): LRESULT; stdcall;
var
  m: TWndMethod;
  Msg: TMessage;
begin
  Msg.msg := uMsg;
  Msg.wParam := wparam;
  Msg.lParam := lParam;
  Msg.Result := 0;

  m := TWndMethod(Pointer(dwRefData)^);
  m(Msg);
  Result := Msg.Result;
end;

function TChromiumCore.InstallCompWndProc(aWnd: THandle; aStub: Pointer): TFNWndProc;
begin
  Result := nil;
  if (aWnd <> 0) and (aStub <> nil) then
    begin
      SetWindowSubclass(aWnd, @CompSubClassProc, 1, NativeInt(aStub));
      Result := TFNWndProc(1); // IdSubClass
    end;
end;

procedure TChromiumCore.RestoreCompWndProc(var aOldWnd: THandle; aNewWnd: THandle; var aProc: TFNWndProc);
begin
  if (aOldWnd <> 0) and (aOldWnd <> aNewWnd) and (aProc <> nil) then
    begin
      RemoveWindowSubclass(aOldWnd, @CompSubClassProc, 1);
      aProc := nil;
      aOldWnd := 0;
    end;
end;

procedure TChromiumCore.CallOldCompWndProc(aProc: TFNWndProc; aWnd: THandle; var aMessage: TMessage);
begin
  if (aProc <> nil) and (aWnd <> 0) then
    aMessage.Result := DefSubclassProc(aWnd, aMessage.Msg, aMessage.wParam, aMessage.lParam);
end;
{$ENDIF}
{$ENDIF}

procedure TChromiumCore.DestroyClientHandler;
begin
  try
    if (FHandler <> nil) then
      begin
        FHandler.RemoveReferences;
        FHandler := nil;
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TChromiumCore.DestroyClientHandler', e) then raise;
  end;
end;

procedure TChromiumCore.DestroyReqContextHandler;
begin
  try
    if (FReqContextHandler <> nil) then
      begin
        FReqContextHandler.RemoveReferences;
        FReqContextHandler := nil;
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TChromiumCore.DestroyReqContextHandler', e) then raise;
  end;
end;

procedure TChromiumCore.CreateReqContextHandler;
begin
  if MustCreateRequestContextHandler and
     (FReqContextHandler = nil) then
    FReqContextHandler := TCustomRequestContextHandler.Create(self);
end;

procedure TChromiumCore.DestroyResourceRequestHandler;
begin
  try
    if (FResourceRequestHandler <> nil) then
      begin
        FResourceRequestHandler.RemoveReferences;
        FResourceRequestHandler := nil;
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TChromiumCore.DestroyResourceRequestHandler', e) then raise;
  end;
end;

procedure TChromiumCore.CreateResourceRequestHandler;
begin
  if MustCreateResourceRequestHandler and
     (FResourceRequestHandler = nil) then
    FResourceRequestHandler := TCustomResourceRequestHandler.Create(self);
end;

procedure TChromiumCore.AfterConstruction;
begin
  inherited AfterConstruction;

  try
    if not(csDesigning in ComponentState) then
      begin
        {$IFDEF MSWINDOWS}
        FCompHandle      := AllocateHWnd({$IFDEF FPC}@{$ENDIF}WndProc);
        {$ENDIF}
        FOptions         := TChromiumOptions.Create;
        FFontOptions     := TChromiumFontOptions.Create;
        FPDFPrintOptions := TPDFPrintOptions.Create;
        FZoomStepCS      := TCriticalSection.Create;
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TChromiumCore.AfterConstruction', e) then raise;
  end;
end;

function TChromiumCore.CreateClientHandler(aIsOSR : boolean) : boolean;
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
      if CustomExceptionHandler('TChromiumCore.CreateClientHandler', e) then raise;
  end;
end;

function TChromiumCore.CreateClientHandler(var aClient : ICefClient; aIsOSR : boolean) : boolean;
begin
  if CreateClientHandler(aIsOSR) then
    begin
      CreateResourceRequestHandler;

      aClient := FHandler;
      Result  := True;
    end
   else
    Result := False;
end;

procedure TChromiumCore.InitializeEvents;
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
  FOnStartDragging                := nil;
  FOnUpdateDragCursor             := nil;
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
  FOnPrefsAvailable                   := nil;
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
  FOnZoomPctAvailable                 := nil;

  {$IFDEF MSWINDOWS}
  FOnBrowserCompMsg                   := nil;
  FOnWidgetCompMsg                    := nil;
  FOnRenderCompMsg                    := nil;
  {$ENDIF}
end;

function TChromiumCore.CreateBrowser(      aParentHandle  : TCefWindowHandle;
                                           aParentRect    : TRect;
                                     const aWindowName    : ustring;
                                     const aContext       : ICefRequestContext;
                                     const aExtraInfo     : ICefDictionaryValue) : boolean;
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
         GlobalCEFApp.GlobalContextInitialized  and
         CreateClientHandler(aParentHandle = 0) then
        begin
          GetSettings(FBrowserSettings);
          InitializeWindowInfo(aParentHandle, aParentRect, aWindowName);
          CreateResourceRequestHandler;

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
        if CustomExceptionHandler('TChromiumCore.CreateBrowser', e) then raise;
    end;
  finally
    TempGlobalContext := nil;
    TempNewContext    := nil;
  end;
end;

procedure TChromiumCore.InitializeWindowInfo(      aParentHandle : TCefWindowHandle;
                                                   aParentRect   : TRect;
                                             const aWindowName   : ustring);
begin
  {$IFDEF MSWINDOWS}
  if FIsOSR then
    WindowInfoAsWindowless(FWindowInfo, ParentFormHandle, aWindowName)
   else
    WindowInfoAsChild(FWindowInfo, aParentHandle, aParentRect, aWindowName);
  {$ELSE}
  if FIsOSR then
    WindowInfoAsWindowless(FWindowInfo, 0)
   else
    WindowInfoAsChild(FWindowInfo, aParentHandle, aParentRect);
  {$ENDIF}
end;

procedure TChromiumCore.DefaultInitializeDevToolsWindowInfo(      aDevToolsWnd : TCefWindowHandle;
                                                            const aClientRect  : TRect;
                                                            const aWindowName  : ustring);
begin
  {$IFDEF MSWINDOWS}
  if (aDevToolsWnd <> 0) then
    WindowInfoAsChild(FDevWindowInfo, aDevToolsWnd, aClientRect, aWindowName)
   else
    WindowInfoAsPopUp(FDevWindowInfo, WindowHandle, DEVTOOLS_WINDOWNAME);
  {$ELSE}
  if (aDevToolsWnd <> 0) then
    WindowInfoAsChild(FDevWindowInfo, aDevToolsWnd, aClientRect)
   else
    WindowInfoAsPopUp(FDevWindowInfo, WindowHandle);
  {$ENDIF}
end;

function TChromiumCore.ShareRequestContext(var   aContext : ICefRequestContext;
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

{$IFDEF MSWINDOWS}
procedure TChromiumCore.InitializeDragAndDrop(const aDropTargetWnd : HWND);
var
  TempDropTarget : IDropTarget;
begin
  if FIsOSR and
     not(FDragAndDropInitialized) and
     (FDragDropManager = nil) and
     (aDropTargetWnd <> 0) then
    begin
      FDropTargetWnd                  := aDropTargetWnd;

      FDragDropManager                := TCEFDragAndDropMgr.Create;
      FDragDropManager.OnDragEnter    := {$IFDEF FPC}@{$ENDIF}DragDropManager_OnDragEnter;
      FDragDropManager.OnDragOver     := {$IFDEF FPC}@{$ENDIF}DragDropManager_OnDragOver;
      FDragDropManager.OnDragLeave    := {$IFDEF FPC}@{$ENDIF}DragDropManager_OnDragLeave;
      FDragDropManager.OnDrop         := {$IFDEF FPC}@{$ENDIF}DragDropManager_OnDrop;

      TempDropTarget                  := TOLEDropTarget.Create(FDragDropManager);

      RegisterDragDrop(FDropTargetWnd, TempDropTarget);

      FDragAndDropInitialized := True;
    end;
end;

procedure TChromiumCore.ShutdownDragAndDrop;
begin
  if FDragAndDropInitialized and (FDropTargetWnd <> 0) then
    begin
      RevokeDragDrop(FDropTargetWnd);
      FDragAndDropInitialized := False;
    end;
end;

procedure TChromiumCore.ToMouseEvent(grfKeyState : Longint; pt : TPoint; var aMouseEvent : TCefMouseEvent);
begin
  if (FDropTargetWnd <> 0) then
    begin
      MapWindowPoints(0, FDropTargetWnd, pt, 1);
      aMouseEvent.x         := pt.x;
      aMouseEvent.y         := pt.y;
      aMouseEvent.modifiers := GetCefMouseModifiers(grfKeyState);
    end;
end;
{$ENDIF}

procedure TChromiumCore.DragDropManager_OnDragEnter(Sender: TObject; const aDragData : ICefDragData; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint);
{$IFDEF MSWINDOWS}
var
  TempMouseEvent : TCefMouseEvent;
  TempAllowedOps : TCefDragOperations;
{$ENDIF}
begin
  if (GlobalCEFApp <> nil) then
    begin
      {$IFDEF MSWINDOWS}
      ToMouseEvent(grfKeyState, pt, TempMouseEvent);
      DropEffectToDragOperation(dwEffect, TempAllowedOps);
      DeviceToLogical(TempMouseEvent, GlobalCEFApp.DeviceScaleFactor);

      DragTargetDragEnter(aDragData, @TempMouseEvent, TempAllowedOps);
      DragTargetDragOver(@TempMouseEvent, TempAllowedOps);

      DragOperationToDropEffect(FDragOperations, dwEffect);
      {$ENDIF}
    end;
end;

procedure TChromiumCore.DragDropManager_OnDragOver(Sender: TObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint);
{$IFDEF MSWINDOWS}
var
  TempMouseEvent : TCefMouseEvent;
  TempAllowedOps : TCefDragOperations;
{$ENDIF}
begin
  if (GlobalCEFApp <> nil) then
    begin
      {$IFDEF MSWINDOWS}
      ToMouseEvent(grfKeyState, pt, TempMouseEvent);
      DropEffectToDragOperation(dwEffect, TempAllowedOps);
      DeviceToLogical(TempMouseEvent, GlobalCEFApp.DeviceScaleFactor);

      DragTargetDragOver(@TempMouseEvent, TempAllowedOps);

      DragOperationToDropEffect(FDragOperations, dwEffect);
      {$ENDIF}
    end;
end;

procedure TChromiumCore.DragDropManager_OnDragLeave(Sender: TObject);
begin
  DragTargetDragLeave;
end;

procedure TChromiumCore.DragDropManager_OnDrop(Sender: TObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint);
{$IFDEF MSWINDOWS}
var
  TempMouseEvent : TCefMouseEvent;
  TempAllowedOps : TCefDragOperations;
{$ENDIF}
begin
  if (GlobalCEFApp <> nil) then
    begin
      {$IFDEF MSWINDOWS}
      ToMouseEvent(grfKeyState, pt, TempMouseEvent);
      DropEffectToDragOperation(dwEffect, TempAllowedOps);
      DeviceToLogical(TempMouseEvent, GlobalCEFApp.DeviceScaleFactor);

      DragTargetDragOver(@TempMouseEvent, TempAllowedOps);
      DragTargetDrop(@TempMouseEvent);

      DragOperationToDropEffect(FDragOperations, dwEffect);
      {$ENDIF}
    end;
end;

procedure TChromiumCore.CloseBrowser(aForceClose : boolean);
begin
  if Initialized then FBrowser.Host.CloseBrowser(aForceClose);
end;

function TChromiumCore.CreateBrowserHost(      aWindowInfo : PCefWindowInfo;
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

function TChromiumCore.CreateBrowserHostSync(      aWindowInfo : PCefWindowInfo;
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

procedure TChromiumCore.Find(aIdentifier : integer; const aSearchText : ustring; aForward, aMatchCase, aFindNext : Boolean);
begin
  if Initialized then FBrowser.Host.Find(aIdentifier, aSearchText, aForward, aMatchCase, aFindNext);
end;

procedure TChromiumCore.StopFinding(aClearSelection : Boolean);
begin
  if Initialized then FBrowser.Host.StopFinding(aClearSelection);
end;

procedure TChromiumCore.Print;
begin
  if Initialized then FBrowser.Host.Print;
end;

procedure TChromiumCore.PrintToPDF(const aFilePath, aTitle, aURL : ustring);
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

procedure TChromiumCore.ClipboardCopy;
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

procedure TChromiumCore.ClipboardPaste;
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

procedure TChromiumCore.ClipboardCut;
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

procedure TChromiumCore.ClipboardUndo;
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

procedure TChromiumCore.ClipboardRedo;
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

procedure TChromiumCore.ClipboardDel;
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

procedure TChromiumCore.SelectAll;
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

procedure TChromiumCore.GetPrintPDFSettings(var aSettings : TCefPdfPrintSettings; const aTitle, aURL : ustring);
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

procedure TChromiumCore.GetSettings(var aSettings : TCefBrowserSettings);
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

procedure TChromiumCore.InitializeSettings(var aSettings : TCefBrowserSettings);
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
procedure TChromiumCore.LoadURL(const aURL : ustring; const aFrameName : ustring);
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

procedure TChromiumCore.LoadURL(const aURL : ustring; const aFrame : ICefFrame);
begin
  if Initialized and (aFrame <> nil) and aFrame.IsValid then aFrame.LoadUrl(aURL);
end;

procedure TChromiumCore.LoadURL(const aURL : ustring; const aFrameIdentifier : int64);
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
procedure TChromiumCore.LoadString(const aHTML : ustring; const aFrameName : ustring);
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

procedure TChromiumCore.LoadString(const aHTML : ustring; const aFrame : ICefFrame);
begin
  if Initialized and (length(aHTML) > 0) and (aFrame <> nil) and aFrame.IsValid then
    aFrame.LoadUrl(CefGetDataURI(aHTML, 'text/html'));
end;

procedure TChromiumCore.LoadString(const aHTML : ustring; const aFrameIdentifier : int64);
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
procedure TChromiumCore.LoadResource(const aStream : TCustomMemoryStream; const aMimeType, aCharset : string; const aFrameName : ustring);
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

procedure TChromiumCore.LoadResource(const aStream : TCustomMemoryStream; const aMimeType, aCharset : string; const aFrame : ICefFrame);
begin
  if Initialized and (aStream <> nil) and (aStream.Size > 0) and (aFrame <> nil) and aFrame.IsValid then
    aFrame.LoadUrl(CefGetDataURI(aStream.Memory, aStream.Size, aMimeType, aCharset));
end;

procedure TChromiumCore.LoadResource(const aStream : TCustomMemoryStream; const aMimeType, aCharset : string; const aFrameIdentifier : int64);
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
procedure TChromiumCore.LoadRequest(const aRequest: ICefRequest);
var
  TempFrame : ICefFrame;
begin
  if Initialized then
    begin
      TempFrame := FBrowser.MainFrame;
      if (TempFrame <> nil) and TempFrame.IsValid then TempFrame.LoadRequest(aRequest);
    end;
end;

procedure TChromiumCore.GoBack;
begin
  if Initialized and CanGoBack then FBrowser.GoBack;
end;

procedure TChromiumCore.GoForward;
begin
  if Initialized and CanGoForward then FBrowser.GoForward;
end;

procedure TChromiumCore.Reload;
begin
  if Initialized then FBrowser.Reload;
end;

procedure TChromiumCore.ReloadIgnoreCache;
begin
  if Initialized then FBrowser.ReloadIgnoreCache;
end;

procedure TChromiumCore.StopLoad;
begin
  if Initialized then FBrowser.StopLoad;
end;

procedure TChromiumCore.StartDownload(const aURL : ustring);
begin
  if Initialized then FBrowser.Host.StartDownload(aURL);
end;

// Use the OnDownloadImageFinished event to receive the image
procedure TChromiumCore.DownloadImage(const imageUrl     : ustring;
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

function TChromiumCore.GetIsLoading : boolean;
begin
  Result := Initialized and FBrowser.IsLoading;
end;

function TChromiumCore.GetMultithreadApp : boolean;
begin
  Result := (GlobalCEFApp <> nil) and GlobalCEFApp.MultiThreadedMessageLoop;
end;

function TChromiumCore.GetHasDocument : boolean;
begin
  Result := Initialized and FBrowser.HasDocument;
end;

function TChromiumCore.GetHasView : boolean;
begin
  Result := Initialized and FBrowser.Host.HasView;
end;

function TChromiumCore.GetHasDevTools : boolean;
begin
  Result := Initialized and FBrowser.Host.HasDevTools;
end;

function TChromiumCore.GetHasClientHandler : boolean;
begin
  Result := (FHandler <> nil);
end;

function TChromiumCore.GetHasBrowser : boolean;
begin
  Result := (FBrowser <> nil);
end;

function TChromiumCore.GetWindowHandle : TCefWindowHandle;
begin
  if Initialized then
    Result := FBrowser.Host.WindowHandle
   else
    Result := 0;
end;

function TChromiumCore.GetFrameIsFocused : boolean;
begin
  Result := Initialized and (FBrowser.FocusedFrame <> nil);
end;

function TChromiumCore.GetWindowlessFrameRate : integer;
begin
  if Initialized then
    Result := FBrowser.Host.GetWindowlessFrameRate
   else
    Result := 0;
end;

function TChromiumCore.GetVisibleNavigationEntry : ICefNavigationEntry;
begin
  if Initialized then
    Result := FBrowser.Host.VisibleNavigationEntry
   else
    Result := nil;
end;

function TChromiumCore.GetHasValidMainFrame : boolean;
begin
  Result := Initialized and (FBrowser.MainFrame <> nil) and FBrowser.MainFrame.IsValid;
end;

function TChromiumCore.GetFrameCount : NativeUInt;
begin
  if Initialized then
    Result := FBrowser.GetFrameCount
   else
    Result := 0;
end;

function TChromiumCore.GetRequestContextCache : ustring;
begin
  if Initialized then
    Result := FBrowser.host.RequestContext.CachePath
   else
    if (GlobalCEFApp <> nil) then
      Result := GlobalCEFApp.cache
     else
      Result := '';
end;

function TChromiumCore.GetRequestContextIsGlobal : boolean;
begin
  Result := Initialized and FBrowser.host.RequestContext.IsGlobal;
end;

function TChromiumCore.GetAudioMuted : boolean;
begin
  Result := Initialized and FBrowser.host.IsAudioMuted;
end;

function TChromiumCore.GetParentFormHandle : TCefWindowHandle;
begin
  Result := 0;
end;

procedure TChromiumCore.SetAudioMuted(aValue : boolean);
begin
  if Initialized then FBrowser.Host.SetAudioMuted(aValue);
end;

procedure TChromiumCore.SetWindowlessFrameRate(aValue : integer);
begin
  if Initialized then FBrowser.Host.SetWindowlessFrameRate(aValue);
end;

function TChromiumCore.GetCanGoBack : boolean;
begin
  Result := Initialized and FBrowser.CanGoBack;
end;

function TChromiumCore.GetCanGoForward : boolean;
begin
  Result := Initialized and FBrowser.CanGoForward;
end;

function TChromiumCore.GetIsPopUp : boolean;
begin
  Result := Initialized and FBrowser.IsPopUp;
end;

function TChromiumCore.GetInitialized : boolean;
begin
  Result := FInitialized and not(FClosing) and (FBrowser <> nil);
end;

function TChromiumCore.GetDocumentURL : ustring;
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

function TChromiumCore.GetZoomLevel : double;
begin
  Result := 0;

  if Initialized then Result := FBrowser.Host.ZoomLevel;
end;

function TChromiumCore.GetZoomPct : double;
begin
  Result := power(1.2, ZoomLevel) * 100;
end;

function TChromiumCore.GetZoomStep : byte;
begin
  Result := ZOOM_STEP_DEF;

  if (FZoomStepCS <> nil) then
    try
      FZoomStepCS.Acquire;
      Result := FZoomStep;
    finally
      FZoomStepCS.Release;
    end;
end;

procedure TChromiumCore.SetZoomLevel(const aValue : double);
begin
  if CefCurrentlyOn(TID_UI) then
    doSetZoomLevel(aValue)
   else
    ExecuteSetZoomLevelTask(aValue);
end;

procedure TChromiumCore.SetZoomPct(const aValue : double);
begin
  if CefCurrentlyOn(TID_UI) then
    doSetZoomPct(aValue)
   else
    ExecuteSetZoomPctTask(aValue);
end;

procedure TChromiumCore.SetZoomStep(aValue : byte);
begin
  if CefCurrentlyOn(TID_UI) then
    doSetZoomStep(aValue)
   else
    ExecuteSetZoomStepTask(aValue);
end;

// Increments the Zoom Step value and triggers the TChromium.OnZoomPctAvailable event with the new value
procedure TChromiumCore.IncZoomStep;
begin
  if CefCurrentlyOn(TID_UI) then
    doUpdateZoomStep(True)
   else
    ExecuteUpdateZoomStepTask(True);
end;

// Decrements the Zoom Step value and triggers the TChromium.OnZoomPctAvailable event with the new value
procedure TChromiumCore.DecZoomStep;
begin
  if CefCurrentlyOn(TID_UI) then
    doUpdateZoomStep(False)
   else
    ExecuteUpdateZoomStepTask(False);
end;

// Increments the Zoom Percent value and triggers the TChromium.OnZoomPctAvailable event with the new value
procedure TChromiumCore.IncZoomPct;
begin
  if CefCurrentlyOn(TID_UI) then
    doUpdateZoomPct(True)
   else
    ExecuteUpdateZoomPctTask(True);
end;

// Decrements the Zoom Percent value and triggers the TChromium.OnZoomPctAvailable event with the new value
procedure TChromiumCore.DecZoomPct;
begin
  if CefCurrentlyOn(TID_UI) then
    doUpdateZoomPct(False)
   else
    ExecuteUpdateZoomPctTask(False);
end;

// Sets the Zoom Step to the default value and triggers the TChromium.OnZoomPctAvailable event
procedure TChromiumCore.ResetZoomStep;
begin
  ZoomStep := ZOOM_STEP_DEF;
end;

// Sets the Zoom Level to the default value and triggers the TChromium.OnZoomPctAvailable event
procedure TChromiumCore.ResetZoomLevel;
begin
  ZoomLevel := 0;
end;

// Sets the Zoom Percent to the default value and triggers the TChromium.OnZoomPctAvailable event
procedure TChromiumCore.ResetZoomPct;
begin
  ZoomPct := ZoomStepValues[ZOOM_STEP_DEF];
end;

// Triggers the TChromium.OnZoomPctAvailable event with the current Zoom Percent value
procedure TChromiumCore.ReadZoom;
begin
  if CefCurrentlyOn(TID_UI) then
    doReadZoom
   else
    ExecuteReadZoomTask;
end;

function TChromiumCore.ExecuteUpdateZoomStepTask(aInc : boolean) : boolean;
var
  TempTask : ICefTask;
begin
  Result := False;

  try
    if Initialized then
      begin
        TempTask := TCefUpdateZoomStepTask.Create(self, aInc);
        Result   := CefPostTask(TID_UI, TempTask);
      end;
  finally
    TempTask := nil;
  end;
end;

function TChromiumCore.ExecuteUpdateZoomPctTask(aInc : boolean) : boolean;
var
  TempTask : ICefTask;
begin
  Result := False;

  try
    if Initialized then
      begin
        TempTask := TCefUpdateZoomPctTask.Create(self, aInc);
        Result   := CefPostTask(TID_UI, TempTask);
      end;
  finally
    TempTask := nil;
  end;
end;

function TChromiumCore.ExecuteReadZoomTask : boolean;
var
  TempTask : ICefTask;
begin
  Result := False;

  try
    if Initialized then
      begin
        TempTask := TCefReadZoomTask.Create(self);
        Result   := CefPostTask(TID_UI, TempTask);
      end;
  finally
    TempTask := nil;
  end;
end;

function TChromiumCore.ExecuteSetZoomPctTask(const aValue : double) : boolean;
var
  TempTask : ICefTask;
begin
  Result := False;

  try
    if Initialized then
      begin
        TempTask := TCefSetZoomPctTask.Create(self, aValue);
        Result   := CefPostTask(TID_UI, TempTask);
      end;
  finally
    TempTask := nil;
  end;
end;

function TChromiumCore.ExecuteSetZoomLevelTask(const aValue : double) : boolean;
var
  TempTask : ICefTask;
begin
  Result := False;

  try
    if Initialized then
      begin
        TempTask := TCefSetZoomLevelTask.Create(self, aValue);
        Result   := CefPostTask(TID_UI, TempTask);
      end;
  finally
    TempTask := nil;
  end;
end;

function TChromiumCore.ExecuteSetZoomStepTask(aValue : byte) : boolean;
var
  TempTask : ICefTask;
begin
  Result := False;

  try
    if Initialized then
      begin
        TempTask := TCefSetZoomStepTask.Create(self, aValue);
        Result   := CefPostTask(TID_UI, TempTask);
      end;
  finally
    TempTask := nil;
  end;
end;

procedure TChromiumCore.SetDoNotTrack(aValue : boolean);
begin
  if (FDoNotTrack <> aValue) then
    begin
      FDoNotTrack        := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TChromiumCore.SetSendReferrer(aValue : boolean);
begin
  if (FSendReferrer <> aValue) then
    begin
      FSendReferrer      := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TChromiumCore.SetHyperlinkAuditing(aValue : boolean);
begin
  if (FHyperlinkAuditing <> aValue) then
    begin
      FHyperlinkAuditing := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TChromiumCore.SetRunAllFlashInAllowMode(aValue : boolean);
begin
  if (FRunAllFlashInAllowMode <> aValue) then
    begin
      FRunAllFlashInAllowMode := aValue;
      FUpdatePreferences      := True;
    end;
end;

procedure TChromiumCore.SetAllowOutdatedPlugins(aValue : boolean);
begin
  if (FAllowOutdatedPlugins <> aValue) then
    begin
      FAllowOutdatedPlugins := aValue;
      FUpdatePreferences    := True;
    end;
end;

procedure TChromiumCore.SetAlwaysAuthorizePlugins(aValue : boolean);
begin
  if (FAlwaysAuthorizePlugins <> aValue) then
    begin
      FAlwaysAuthorizePlugins := aValue;
      FUpdatePreferences      := True;
    end;
end;

procedure TChromiumCore.SetSpellChecking(aValue : boolean);
begin
  if (FSpellChecking <> aValue) then
    begin
      FSpellChecking     := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TChromiumCore.SetSpellCheckerDicts(const aValue : ustring);
begin
  if (FSpellCheckerDicts <> aValue) then
    begin
      FSpellCheckerDicts := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TChromiumCore.SetSafeSearch(aValue : boolean);
begin
  if (FSafeSearch <> aValue) then
    begin
      FSafeSearch        := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TChromiumCore.SetYouTubeRestrict(aValue : integer);
begin
  if (FYouTubeRestrict <> aValue) then
    begin
      FYouTubeRestrict   := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TChromiumCore.SetPrintingEnabled(aValue : boolean);
begin
  if (FPrintingEnabled <> aValue) then
    begin
      FPrintingEnabled   := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TChromiumCore.SetAcceptLanguageList(const aValue : ustring);
begin
  if (FAcceptLanguageList <> aValue) then
    begin
      FAcceptLanguageList := aValue;
      FUpdatePreferences  := True;
    end;
end;

procedure TChromiumCore.SetAcceptCookies(const aValue : TCefCookiePref);
begin
  if (FAcceptCookies <> aValue) then
    begin
      FAcceptCookies     := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TChromiumCore.SetBlock3rdPartyCookies(const aValue : boolean);
begin
  if (FBlock3rdPartyCookies <> aValue) then
    begin
      FBlock3rdPartyCookies := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TChromiumCore.SetOnRequestContextInitialized(const aValue : TOnRequestContextInitialized);
begin
  FOnRequestContextInitialized := aValue;

  CreateReqContextHandler;
end;

procedure TChromiumCore.SetOnBeforePluginLoad(const aValue : TOnBeforePluginLoad);
begin
  FOnBeforePluginLoad := aValue;

  CreateReqContextHandler;
end;

procedure TChromiumCore.UpdateHostZoomLevel(const aValue : double);
begin
  if Initialized then FBrowser.Host.ZoomLevel := aValue;
end;

procedure TChromiumCore.UpdateHostZoomPct(const aValue : double);
begin
  if (aValue > 0) then UpdateHostZoomLevel(LogN(1.2, aValue / 100));
end;

procedure TChromiumCore.SetWebRTCIPHandlingPolicy(aValue : TCefWebRTCHandlingPolicy);
begin
  if (FWebRTCIPHandlingPolicy <> aValue) then
    begin
      FWebRTCIPHandlingPolicy := aValue;
      FUpdatePreferences      := True;
    end;
end;

procedure TChromiumCore.SetWebRTCMultipleRoutes(aValue : TCefState);
begin
  if (FWebRTCMultipleRoutes <> aValue) then
    begin
      FWebRTCMultipleRoutes := aValue;
      FUpdatePreferences    := True;
    end;
end;

procedure TChromiumCore.SetWebRTCNonProxiedUDP(aValue : TCefState);
begin
  if (FWebRTCNonProxiedUDP <> aValue) then
    begin
      FWebRTCNonProxiedUDP := aValue;
      FUpdatePreferences   := True;
    end;
end;

procedure TChromiumCore.SetProxyType(aValue : integer);
begin
  if (FProxyType <> aValue) then
    begin
      FProxyType         := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TChromiumCore.SetProxyScheme(aValue : TCefProxyScheme);
begin
  if (FProxyScheme <> aValue) then
    begin
      FProxyScheme       := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TChromiumCore.SetProxyServer(const aValue : ustring);
begin
  if (FProxyServer <> aValue) then
    begin
      FProxyServer       := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TChromiumCore.SetProxyPort(aValue : integer);
begin
  if (FProxyPort <> aValue) then
    begin
      FProxyPort         := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TChromiumCore.SetProxyUsername(const aValue : ustring);
begin
  if (FProxyUsername <> aValue) then
    begin
      FProxyUsername     := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TChromiumCore.SetProxyPassword(const aValue : ustring);
begin
  if (FProxyPassword <> aValue) then
    begin
      FProxyPassword     := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TChromiumCore.SetProxyScriptURL(const aValue : ustring);
begin
  if (FProxyScriptURL <> aValue) then
    begin
      FProxyScriptURL    := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TChromiumCore.SetProxyByPassList(const aValue : ustring);
begin
  if (FProxyByPassList <> aValue) then
    begin
      FProxyByPassList   := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TChromiumCore.SetMaxConnectionsPerProxy(const aValue : integer);
begin
  if (FMaxConnectionsPerProxy <> aValue) and
     (aValue in [CEF_MAX_CONNECTIONS_PER_PROXY_MIN_VALUE..CEF_MAX_CONNECTIONS_PER_PROXY_MAX_VALUE]) then
    begin
      FMaxConnectionsPerProxy := aValue;
      FUpdatePreferences      := True;
    end;
end;

procedure TChromiumCore.SetCustomHeaderName(const aValue : ustring);
begin
  if (FCustomHeaderName <> aValue) then
    begin
      FCustomHeaderName := aValue;
      FAddCustomHeader  := (length(FCustomHeaderName) > 0) and (length(FCustomHeaderValue) > 0);
    end;
end;

procedure TChromiumCore.SetCustomHeaderValue(const aValue : ustring);
begin
  if (FCustomHeaderValue <> aValue) then
    begin
      FCustomHeaderValue := aValue;
      FAddCustomHeader   := (length(FCustomHeaderName) > 0) and (length(FCustomHeaderValue) > 0);
    end;
end;

// If aDeleteImmediately is false TChromiumCore.DeleteCookies triggers the TChromiumCore.OnCookiesDeleted
// event when the cookies are deleted.
function TChromiumCore.DeleteCookies(const url, cookieName: ustring; aDeleteImmediately : boolean) : boolean;
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

// TChromiumCore.VisitAllCookies triggers the TChromiumCore.OnCookiesVisited event for each cookie
// aID is an optional parameter to identify which VisitAllCookies call has triggered the
// OnCookiesVisited event.
// TChromiumCore.OnCookiesVisited may not be triggered if the cookie store is empty but the
// TChromium.OnCookieVisitorDestroyed event will always be triggered to signal when the browser
// when the visit is over.
function TChromiumCore.VisitAllCookies(aID : integer) : boolean;
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

// TChromiumCore.VisitURLCookies triggers the TChromiumCore.OnCookiesVisited event for each cookie
// aID is an optional parameter to identify which VisitURLCookies call has triggered the
// OnCookiesVisited event.
// TChromiumCore.OnCookiesVisited may not be triggered if the cookie store is empty but the
// TChromium.OnCookieVisitorDestroyed event will always be triggered to signal when the browser
// when the visit is over.
function TChromiumCore.VisitURLCookies(const url             : ustring;
                                             includeHttpOnly : boolean;
                                             aID             : integer) : boolean;
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

// TChromiumCore.SetCookie triggers the TChromiumCore.OnCookieSet event when the cookie has been set
// aID is an optional parameter to identify which SetCookie call has triggered the
// OnCookieSet event.
function TChromiumCore.SetCookie(const url, name_, value, domain, path: ustring;
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

          Result := TempManager.SetCookie(url, name_, value, domain, path,
                                          secure, httponly, hasExpires,
                                          creation, lastAccess, expires,
                                          TempCallback);
        finally
          TempCallback := nil;
        end;
    end;
end;

// If aFlushImmediately is false then OnCookiesFlushed is triggered when the cookies are flushed
function TChromiumCore.FlushCookieStore(aFlushImmediately : boolean) : boolean;
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

procedure TChromiumCore.UpdateSupportedSchemes(const aSchemes : TStrings; aIncludeDefaults : boolean);
var
  TempManager : ICefCookieManager;
begin
  if Initialized and (FBrowser.Host <> nil) and (FBrowser.Host.RequestContext <> nil) then
    begin
      TempManager := FBrowser.Host.RequestContext.GetCookieManager(nil);

      if (TempManager <> nil) then
        try
          TempManager.SetSupportedSchemes(aSchemes, aIncludeDefaults, nil);
        finally
          TempManager := nil;
        end;
    end;
end;

// If aClearImmediately is false then OnCertificateExceptionsCleared is triggered when the exceptions are cleared
function TChromiumCore.ClearCertificateExceptions(aClearImmediately : boolean) : boolean;
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
function TChromiumCore.ClearHttpAuthCredentials(aClearImmediately : boolean) : boolean;
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
function TChromiumCore.CloseAllConnections(aCloseImmediately : boolean) : boolean;
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
procedure TChromiumCore.RetrieveHTML(const aFrameName : ustring);
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

procedure TChromiumCore.RetrieveHTML(const aFrame : ICefFrame);
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

procedure TChromiumCore.RetrieveHTML(const aFrameIdentifier : int64);
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
procedure TChromiumCore.RetrieveText(const aFrameName : ustring);
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

procedure TChromiumCore.RetrieveText(const aFrame : ICefFrame);
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

procedure TChromiumCore.RetrieveText(const aFrameIdentifier : int64);
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

procedure TChromiumCore.GetNavigationEntries(currentOnly: Boolean);
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

function TChromiumCore.GetFrameNames(var aFrameNames : TStrings) : boolean;
begin
  Result := Initialized and FBrowser.GetFrameNames(aFrameNames);
end;

function TChromiumCore.GetFrameIdentifiers(var aFrameCount : NativeUInt; var aFrameIdentifierArray : TCefFrameIdentifierArray) : boolean;
begin
  Result := Initialized and FBrowser.GetFrameIdentifiers(aFrameCount, aFrameIdentifierArray);
end;

procedure TChromiumCore.UpdatePreferences;
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

procedure TChromiumCore.SavePreferences(const aFileName : string);
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

function TChromiumCore.SetNewBrowserParent(aNewParentHwnd : HWND) : boolean;
{$IFDEF MSWINDOWS}
var
  TempHandle : HWND;
{$ENDIF}
begin
  Result := False;

  {$IFDEF MSWINDOWS}
  if Initialized then
    begin
      TempHandle := FBrowser.Host.WindowHandle;
      Result     := (TempHandle <> 0) and (SetParent(TempHandle, aNewParentHwnd) <> 0);
    end;
  {$ENDIF}
end;

procedure TChromiumCore.ResolveHost(const aURL : ustring);
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

function TChromiumCore.IsSameBrowser(const aBrowser : ICefBrowser) : boolean;
begin
  Result := Initialized and (aBrowser <> nil) and FBrowser.IsSame(aBrowser);
end;

// Calling ExecuteTaskOnCefThread function will trigger the TChromiumCore.OnExecuteTaskOnCefThread event.
// "aCefThreadId" indicates the CEF thread on which TChromiumCore.OnExecuteTaskOnCefThread will be executed.
// "aTaskID" is a custom ID used to identify the task that triggered the TChromiumCore.OnExecuteTaskOnCefThread event.
// "aDelayMs" is an optional delay in milliseconds to trigger the TChromiumCore.OnExecuteTaskOnCefThread event.
function TChromiumCore.ExecuteTaskOnCefThread(aCefThreadId : TCefThreadId; aTaskID : cardinal; aDelayMs : Int64) : boolean;
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

procedure TChromiumCore.SimulateMouseWheel(aDeltaX, aDeltaY : integer);
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

procedure TChromiumCore.doUpdatePreferences(const aBrowser: ICefBrowser);
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
  UpdatePreference(aBrowser, 'intl.accept_languages',                FAcceptLanguageList);

  case FAcceptCookies of
    cpAllow : UpdatePreference(aBrowser, 'profile.default_content_setting_values.cookies', CEF_COOKIE_PREF_ALLOW);
    cpBlock : UpdatePreference(aBrowser, 'profile.default_content_setting_values.cookies', CEF_COOKIE_PREF_BLOCK);
    else      UpdatePreference(aBrowser, 'profile.default_content_setting_values.cookies', CEF_COOKIE_PREF_DEFAULT);
  end;

  UpdatePreference(aBrowser, 'profile.managed_default_content_settings.cookies', CEF_COOKIE_PREF_DEFAULT);
  UpdatePreference(aBrowser, 'profile.block_third_party_cookies', FBlock3rdPartyCookies);

  if (FMaxConnectionsPerProxy <> CEF_MAX_CONNECTIONS_PER_PROXY_DEFAULT_VALUE) then
    UpdatePreference(aBrowser, 'net.max_connections_per_proxy', FMaxConnectionsPerProxy);

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

procedure TChromiumCore.doUpdateOwnPreferences;
begin
  if Initialized then doUpdatePreferences(FBrowser);
end;

function TChromiumCore.UpdateProxyPrefs(const aBrowser: ICefBrowser) : boolean;
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
            OutputDebugMessage('TChromiumCore.UpdateProxyPrefs error : ' + quotedstr(TempError));
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TChromiumCore.UpdateProxyPrefs', e) then raise;
    end;
  finally
    TempProxy := nil;
    TempValue := nil;
    TempDict  := nil;
  end;
end;

function TChromiumCore.UpdatePreference(const aBrowser: ICefBrowser; const aName : ustring; aValue : boolean) : boolean;
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
          TempValue.SetBool(aValue);
          Result := aBrowser.Host.RequestContext.SetPreference(aName, TempValue, TempError);

          if not(Result) then
            OutputDebugMessage('TChromiumCore.UpdatePreference error : ' + quotedstr(TempError));
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TChromiumCore.UpdatePreference', e) then raise;
    end;
  finally
    TempValue := nil;
  end;
end;

function TChromiumCore.UpdatePreference(const aBrowser: ICefBrowser; const aName : ustring; aValue : integer) : boolean;
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
            OutputDebugMessage('TChromiumCore.UpdatePreference error : ' + quotedstr(TempError));
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TChromiumCore.UpdatePreference', e) then raise;
    end;
  finally
    TempValue := nil;
  end;
end;

function TChromiumCore.UpdatePreference(const aBrowser: ICefBrowser; const aName : ustring; const aValue : double) : boolean;
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
            OutputDebugMessage('TChromiumCore.UpdatePreference error : ' + quotedstr(TempError));
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TChromiumCore.UpdatePreference', e) then raise;
    end;
  finally
    TempValue := nil;
  end;
end;

function TChromiumCore.UpdatePreference(const aBrowser: ICefBrowser; const aName, aValue : ustring) : boolean;
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
            OutputDebugMessage('TChromiumCore.UpdatePreference error : ' + quotedstr(TempError));
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TChromiumCore.UpdatePreference', e) then raise;
    end;
  finally
    TempValue := nil;
  end;
end;

function TChromiumCore.UpdatePreference(const aBrowser: ICefBrowser; const aName : ustring; const aValue : TStringList) : boolean;
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
                OutputDebugMessage('TChromiumCore.UpdatePreference error : ' + quotedstr(TempError));
            end;
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TChromiumCore.UpdatePreference', e) then raise;
    end;
  finally
    TempValue := nil;
    TempList  := nil;
  end;
end;

function TChromiumCore.UpdateStringListPref(const aBrowser: ICefBrowser; const aName, aValue : ustring) : boolean;
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

procedure TChromiumCore.HandleNull(const aValue : ICefValue; var aResultSL : TStringList; const aRoot, aKey : string);
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

procedure TChromiumCore.HandleBool(const aValue : ICefValue; var aResultSL : TStringList; const aRoot, aKey : string);
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

procedure TChromiumCore.HandleInteger(const aValue : ICefValue; var aResultSL : TStringList; const aRoot, aKey : string);
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

procedure TChromiumCore.HandleDouble(const aValue : ICefValue; var aResultSL : TStringList; const aRoot, aKey : string);
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

procedure TChromiumCore.HandleString(const aValue : ICefValue; var aResultSL : TStringList; const aRoot, aKey : string);
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

procedure TChromiumCore.HandleBinary(const aValue : ICefValue; var aResultSL : TStringList; const aRoot, aKey : string);
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

procedure TChromiumCore.HandleList(const aValue : ICefValue; var aResultSL : TStringList; const aRoot, aKey : string);
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

procedure TChromiumCore.HandleInvalid(const aValue : ICefValue; var aResultSL : TStringList; const aRoot, aKey : string);
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

procedure TChromiumCore.HandleDictionary(const aDict : ICefDictionaryValue; var aResultSL : TStringList; const aRoot : string);
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
        if CustomExceptionHandler('TChromiumCore.HandleDictionary', e) then raise;
    end;
  finally
    if (TempKeys <> nil) then TempKeys.Free;
  end;
end;

function TChromiumCore.doSavePreferences : boolean;
{$IFDEF MSWINDOWS}
var
  TempDict  : ICefDictionaryValue;
  TempPrefs : TStringList;
{$ENDIF}
begin
  Result    := False;
  {$IFDEF MSWINDOWS}
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
        if CustomExceptionHandler('TChromiumCore.Internal_SavePreferences', e) then raise;
    end;
  finally
    SendCompMessage(CEF_PREFERENCES_SAVED, Ord(Result));
    if (TempPrefs <> nil) then FreeAndNil(TempPrefs);
  end;
  {$ENDIF}
end;

procedure TChromiumCore.doResolvedHostAvailable(result: TCefErrorCode; const resolvedIps: TStrings);
begin
  if assigned(FOnResolvedHostAvailable) then FOnResolvedHostAvailable(self, result, resolvedIps);
end;

function TChromiumCore.doNavigationVisitorResultAvailable(const entry   : ICefNavigationEntry;
                                                                current : Boolean;
                                                                index   : Integer;
                                                                total   : Integer) : boolean;
begin
  Result := False;

  if assigned(FOnNavigationVisitorResultAvailable) then
    FOnNavigationVisitorResultAvailable(entry, current, index, total, Result);
end;

procedure TChromiumCore.doDownloadImageFinished(const imageUrl       : ustring;
                                                      httpStatusCode : Integer;
                                                const image          : ICefImage);
begin
  if assigned(FOnDownloadImageFinished) then
    FOnDownloadImageFinished(self, imageUrl, httpStatusCode, image);
end;

procedure TChromiumCore.doOnCookiesStoreFlushed;
begin
  if assigned(FOnCookiesFlushed) then FOnCookiesFlushed(self);
end;

procedure TChromiumCore.doCertificateExceptionsCleared;
begin
  if assigned(FOnCertificateExceptionsCleared) then FOnCertificateExceptionsCleared(self);
end;

procedure TChromiumCore.doHttpAuthCredentialsCleared;
begin
  if assigned(FOnHttpAuthCredentialsCleared) then FOnHttpAuthCredentialsCleared(self);
end;

procedure TChromiumCore.doAllConnectionsClosed;
begin
  if assigned(FOnAllConnectionsClosed) then FOnAllConnectionsClosed(self);
end;

procedure TChromiumCore.doOnExecuteTaskOnCefThread(aTaskID : cardinal);
begin
  if assigned(FOnExecuteTaskOnCefThread) then FOnExecuteTaskOnCefThread(self, aTaskID);
end;

procedure TChromiumCore.doOnCookiesVisited(const name_, value, domain, path: ustring;
                                                 secure, httponly, hasExpires: Boolean;
                                           const creation, lastAccess, expires: TDateTime;
                                                 count, total, aID : Integer;
                                           var   aDeleteCookie, aResult : Boolean);
begin
  if assigned(FOnCookiesVisited) then
    FOnCookiesVisited(self, name_, value, domain, path,
                      secure, httponly, hasExpires,
                      creation, lastAccess, expires,
                      count, total, aID,
                      aDeleteCookie, aResult);
end;

procedure TChromiumCore.doOnCookieVisitorDestroyed(aID : integer);
begin
  if assigned(FOnCookieVisitorDestroyed) then
    FOnCookieVisitorDestroyed(self, aID);
end;

procedure TChromiumCore.doOnCookieSet(aSuccess : boolean; aID : integer);
begin
  if assigned(FOnCookieSet) then FOnCookieSet(self, aSuccess, aID);
end;

procedure TChromiumCore.doUpdateZoomStep(aInc : boolean);
var
  TempPct, TempPrev, TempNext : double;
  i : integer;
begin
  if not(Initialized) or (FZoomStepCS = nil) then exit;

  try
    FZoomStepCS.Acquire;

    if (FZoomStep in [ZOOM_STEP_MIN..ZOOM_STEP_MAX]) then
      begin
        if aInc then
          begin
            if (FZoomStep < ZOOM_STEP_MAX) then
              begin
                inc(FZoomStep);
                UpdateHostZoomPct(ZoomStepValues[FZoomStep]);
              end;
          end
         else
          if (FZoomStep > ZOOM_STEP_MIN) then
          begin
            dec(FZoomStep);
            UpdateHostZoomPct(ZoomStepValues[FZoomStep]);
          end;
      end
     else
      begin
        TempPct  := ZoomPct;
        TempPrev := 0;
        i        := ZOOM_STEP_MIN;

        repeat
          if (i <= ZOOM_STEP_MAX) then
            TempNext := ZoomStepValues[i]
           else
            TempNext := ZoomStepValues[ZOOM_STEP_MAX] * 2;

          if (TempPct > TempPrev) and (TempPct < TempNext) then
            begin
              if aInc then
                begin
                  if (i <= ZOOM_STEP_MAX) then
                    begin
                      FZoomStep := i;
                      UpdateHostZoomPct(ZoomStepValues[FZoomStep]);
                    end;
                end
               else
                if (i > ZOOM_STEP_MIN) then
                  begin
                    FZoomStep := pred(i);
                    UpdateHostZoomPct(ZoomStepValues[FZoomStep]);
                  end;

              i := ZOOM_STEP_MAX + 2;
            end
           else
            begin
              TempPrev := TempNext;
              inc(i);
            end;

        until (i > succ(ZOOM_STEP_MAX));
      end;
  finally
    FZoomStepCS.Release;

    if assigned(FOnZoomPctAvailable) then FOnZoomPctAvailable(self, ZoomPct);
  end;
end;

procedure TChromiumCore.doUpdateZoomPct(aInc : boolean);
var
  TempNewZoom : double;
  i : integer;
begin
  if not(Initialized) or (FZoomStepCS = nil) then exit;

  TempNewZoom := ZoomPct;

  try
    FZoomStepCS.Acquire;

    if aInc then
      TempNewZoom := min(TempNewZoom + ZOOM_PCT_DELTA, ZoomStepValues[ZOOM_STEP_MAX])
     else
      TempNewZoom := max(TempNewZoom - ZOOM_PCT_DELTA, ZoomStepValues[ZOOM_STEP_MIN]);

    for i := ZOOM_STEP_MIN to ZOOM_STEP_MAX do
      if (TempNewZoom = ZoomStepValues[i]) then break;

    FZoomStep := i;
    UpdateHostZoomPct(TempNewZoom);
  finally
    FZoomStepCS.Release;

    if assigned(FOnZoomPctAvailable) then FOnZoomPctAvailable(self, TempNewZoom);
  end;
end;

procedure TChromiumCore.doReadZoom;
begin
  if Initialized and assigned(FOnZoomPctAvailable) then
    FOnZoomPctAvailable(self, ZoomPct);
end;

procedure TChromiumCore.doSetZoomLevel(const aValue : double);
var
  TempZoom : double;
  i : integer;
begin
  if not(Initialized) or (FZoomStepCS = nil) then exit;

  try
    FZoomStepCS.Acquire;

    UpdateHostZoomLevel(aValue);
    TempZoom := ZoomPct;

    for i := ZOOM_STEP_MIN to ZOOM_STEP_MAX do
      if (TempZoom = ZoomStepValues[i]) then break;

    FZoomStep := i;
  finally
    FZoomStepCS.Release;

    if assigned(FOnZoomPctAvailable) then FOnZoomPctAvailable(self, ZoomPct);
  end;
end;

procedure TChromiumCore.doSetZoomPct(const aValue : double);
var
  TempZoom : double;
  i : integer;
begin
  if not(Initialized) or (FZoomStepCS = nil) then exit;

  try
    FZoomStepCS.Acquire;

    if (aValue >= ZoomStepValues[ZOOM_STEP_MIN]) and
       (aValue <= ZoomStepValues[ZOOM_STEP_MAX]) then
      begin
        UpdateHostZoomPct(aValue);
        TempZoom := ZoomPct;

        for i := ZOOM_STEP_MIN to ZOOM_STEP_MAX do
          if (TempZoom = ZoomStepValues[i]) then break;

        FZoomStep := i;
      end;
  finally
    FZoomStepCS.Release;

    if assigned(FOnZoomPctAvailable) then FOnZoomPctAvailable(self, ZoomPct);
  end;
end;

procedure TChromiumCore.doSetZoomStep(aValue : byte);
begin
  if not(Initialized) or (FZoomStepCS = nil) then exit;

  try
    FZoomStepCS.Acquire;

    if (aValue in [ZOOM_STEP_MIN..ZOOM_STEP_MAX]) then
      begin
        FZoomStep := aValue;
        UpdateHostZoomPct(ZoomStepValues[aValue]);
      end;
  finally
    FZoomStepCS.Release;

    if assigned(FOnZoomPctAvailable) then FOnZoomPctAvailable(self, ZoomPct);
  end;
end;

function TChromiumCore.MustCreateLoadHandler : boolean;
begin
  Result := assigned(FOnLoadStart) or
            assigned(FOnLoadEnd)   or
            assigned(FOnLoadError) or
            assigned(FOnLoadingStateChange);
end;

function TChromiumCore.MustCreateFocusHandler : boolean;
begin
  Result := assigned(FOnTakeFocus) or
            assigned(FOnSetFocus)  or
            assigned(FOnGotFocus);
end;

function TChromiumCore.MustCreateContextMenuHandler : boolean;
begin
  Result := assigned(FOnBeforeContextMenu)  or
            assigned(FOnRunContextMenu)     or
            assigned(FOnContextMenuCommand) or
            assigned(FOnContextMenuDismissed);
end;

function TChromiumCore.MustCreateDialogHandler : boolean;
begin
  Result := assigned(FOnFileDialog);
end;

function TChromiumCore.MustCreateKeyboardHandler : boolean;
begin
  Result := assigned(FOnPreKeyEvent) or
            assigned(FOnKeyEvent);
end;

function TChromiumCore.MustCreateDisplayHandler : boolean;
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

function TChromiumCore.MustCreateDownloadHandler : boolean;
begin
  Result := assigned(FOnBeforeDownload) or
            assigned(FOnDownloadUpdated);
end;

function TChromiumCore.MustCreateJsDialogHandler : boolean;
begin
  Result := assigned(FOnJsdialog)           or
            assigned(FOnBeforeUnloadDialog) or
            assigned(FOnResetDialogState)   or
            assigned(FOnDialogClosed);
end;

function TChromiumCore.MustCreateLifeSpanHandler : boolean;
begin
  Result := True;
end;

function TChromiumCore.MustCreateRenderHandler : boolean;
begin
  Result := FIsOSR;
end;

function TChromiumCore.MustCreateRequestHandler : boolean;
begin
  Result := True;
end;

function TChromiumCore.MustCreateDragHandler : boolean;
begin
  Result := assigned(FOnDragEnter) or
            assigned(FOnDraggableRegionsChanged);
end;

function TChromiumCore.MustCreateFindHandler : boolean;
begin
  Result := assigned(FOnFindResult);
end;

function TChromiumCore.MustCreateResourceRequestHandler : boolean;
begin
  Result := assigned(FOnBeforeResourceLoad)        or
            assigned(FOnGetResourceHandler)        or
            assigned(FOnResourceRedirect)          or
            assigned(FOnResourceResponse)          or
            assigned(FOnGetResourceResponseFilter) or
            assigned(FOnResourceLoadComplete)      or
            assigned(FOnProtocolExecution)         or
            MustCreateCookieAccessFilter;
end;

function TChromiumCore.MustCreateCookieAccessFilter : boolean;
begin
  Result := assigned(FOnCanSendCookie) or
            assigned(FOnCanSaveCookie);
end;

function TChromiumCore.MustCreateRequestContextHandler : boolean;
begin
  Result := assigned(FOnRequestContextInitialized) or
            assigned(FOnBeforePluginLoad) or
            assigned(FOnGetResourceRequestHandler_ReqCtxHdlr) or
            MustCreateResourceRequestHandler;
end;

{$IFDEF MSWINDOWS}
procedure TChromiumCore.PrefsAvailableMsg(aResultOK : boolean);
begin
  if assigned(FOnPrefsAvailable) then FOnPrefsAvailable(self, aResultOK);
end;

function TChromiumCore.SendCompMessage(aMsg : cardinal; wParam : cardinal; lParam : integer) : boolean;
begin
  Result := (FCompHandle <> 0) and PostMessage(FCompHandle, aMsg, wParam, lParam);
end;
{$ENDIF}

procedure TChromiumCore.doTextResultAvailable(const aText : ustring);
begin
  if assigned(FOnTextResultAvailable) then FOnTextResultAvailable(self, aText);
end;

procedure TChromiumCore.ExecuteJavaScript(const aCode, aScriptURL, aFrameName : ustring; aStartLine : integer);
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
      if CustomExceptionHandler('TChromiumCore.ExecuteJavaScript', e) then raise;
  end;
end;

procedure TChromiumCore.ExecuteJavaScript(const aCode, aScriptURL : ustring; const aFrame : ICefFrame; aStartLine : integer);
begin
  try
    if Initialized and (aFrame <> nil) and aFrame.IsValid then
      aFrame.ExecuteJavaScript(aCode, aScriptURL, aStartLine);
  except
    on e : exception do
      if CustomExceptionHandler('TChromiumCore.ExecuteJavaScript', e) then raise;
  end;
end;

procedure TChromiumCore.ExecuteJavaScript(const aCode, aScriptURL : ustring; const aFrameIdentifier : int64; aStartLine : integer = 0);
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
      if CustomExceptionHandler('TChromiumCore.ExecuteJavaScript', e) then raise;
  end;
end;

procedure TChromiumCore.doCookiesDeleted(numDeleted : integer);
begin
  if assigned(FOnCookiesDeleted) then FOnCookiesDeleted(self, numDeleted);
end;

procedure TChromiumCore.doPdfPrintFinished(aResultOK : boolean);
begin
  if assigned(FOnPdfPrintFinished) then FOnPdfPrintFinished(self, aResultOK);
end;

procedure TChromiumCore.ShowDevTools(const inspectElementAt: TPoint; aWindowInfo: PCefWindowInfo);
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
          if aWindowInfo = nil then
            DefaultInitializeDevToolsWindowInfo(0, Rect(0, 0, 0, 0), '')
           else
             if aWindowInfo <> @FDevWindowInfo then
               FDevWindowInfo := aWindowInfo^;

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
        if CustomExceptionHandler('TChromiumCore.ShowDevTools', e) then raise;
    end;
  finally
    TempClient := nil;
  end;
end;

procedure TChromiumCore.CloseDevTools(const aDevToolsWnd : TCefWindowHandle);
begin
  if Initialized then
    begin
      {$IFDEF MSWINDOWS}
      if (aDevToolsWnd <> 0) then
        SetParent(GetWindow(aDevToolsWnd, GW_CHILD), 0);
      {$ENDIF}

      if Initialized and (FBrowser <> nil) then FBrowser.Host.CloseDevTools;
    end;
end;

{$IFDEF MSWINDOWS}
procedure TChromiumCore.WndProc(var aMessage: TMessage);
begin
  case aMessage.Msg of
    CEF_PREFERENCES_SAVED : PrefsAvailableMsg(aMessage.WParam <> 0);
    CEF_STARTDRAGGING     : DelayedDragging;

    else aMessage.Result := DefWindowProc(FCompHandle, aMessage.Msg, aMessage.WParam, aMessage.LParam);
  end;
end;

procedure TChromiumCore.BrowserCompWndProc(var aMessage: TMessage);
var
  TempHandled : boolean;
begin
  try
    TempHandled := False;

    try
      if assigned(FOnBrowserCompMsg) then
        FOnBrowserCompMsg(aMessage, TempHandled);

      if not(TempHandled) then
        CallOldCompWndProc(FOldBrowserCompWndPrc, FBrowserCompHWND, aMessage);
    finally
      if aMessage.Msg = WM_DESTROY then
        RestoreCompWndProc(FBrowserCompHWND, 0, FOldBrowserCompWndPrc);
    end;
  except
    on e : exception do
      if CustomExceptionHandler('TChromiumCore.BrowserCompWndProc', e) then raise;
  end;
end;

procedure TChromiumCore.WidgetCompWndProc(var aMessage: TMessage);
var
  TempHandled : boolean;
begin
  try
    TempHandled := False;

    try
      if assigned(FOnWidgetCompMsg) then
        FOnWidgetCompMsg(aMessage, TempHandled);

      if not(TempHandled) then
        CallOldCompWndProc(FOldWidgetCompWndPrc, FWidgetCompHWND, aMessage);
    finally
      if aMessage.Msg = WM_DESTROY then
        RestoreCompWndProc(FWidgetCompHWND, 0, FOldWidgetCompWndPrc);
    end;
  except
    on e : exception do
      if CustomExceptionHandler('TChromiumCore.WidgetCompWndProc', e) then raise;
  end;
end;

procedure TChromiumCore.RenderCompWndProc(var aMessage: TMessage);
var
  TempHandled : boolean;
begin
  try
    TempHandled := False;

    try
      if assigned(FOnRenderCompMsg) then
        FOnRenderCompMsg(aMessage, TempHandled);

      if not(TempHandled) then
        CallOldCompWndProc(FOldRenderCompWndPrc, FRenderCompHWND, aMessage);
    finally
      if aMessage.Msg = WM_DESTROY then
        RestoreCompWndProc(FRenderCompHWND, 0, FOldRenderCompWndPrc);
    end;
  except
    on e : exception do
      if CustomExceptionHandler('TChromiumCore.RenderCompWndProc', e) then raise;
  end;
end;
{$ENDIF}

function TChromiumCore.doOnClose(const browser: ICefBrowser): Boolean;
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

procedure TChromiumCore.doOnBeforeClose(const browser: ICefBrowser);
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

procedure TChromiumCore.doOnAddressChange(const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
begin
  if Assigned(FOnAddressChange) then FOnAddressChange(Self, browser, frame, url);
end;

procedure TChromiumCore.doOnAfterCreated(const browser: ICefBrowser);
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

function TChromiumCore.doOnBeforeBrowse(const browser      : ICefBrowser;
                                        const frame        : ICefFrame;
                                        const request      : ICefRequest;
                                              user_gesture : Boolean;
                                              isRedirect   : Boolean): Boolean;
begin
  Result := False;

  if FUpdatePreferences then doUpdatePreferences(browser);

  if Assigned(FOnBeforeBrowse) then FOnBeforeBrowse(Self, browser, frame, request, user_gesture, isRedirect, Result);
end;

procedure TChromiumCore.doOnBeforeContextMenu(const browser : ICefBrowser;
                                              const frame   : ICefFrame;
                                              const params  : ICefContextMenuParams;
                                              const model   : ICefMenuModel);
begin
  if Assigned(FOnBeforeContextMenu) then FOnBeforeContextMenu(Self, browser, frame, params, model);
end;

function TChromiumCore.doRunContextMenu(const browser  : ICefBrowser;
                                        const frame    : ICefFrame;
                                        const params   : ICefContextMenuParams;
                                        const model    : ICefMenuModel;
                                        const callback : ICefRunContextMenuCallback): Boolean;
begin
  Result := False;

  if Assigned(FOnRunContextMenu) then FOnRunContextMenu(Self, browser, frame, params, model, callback, Result);
end;

procedure TChromiumCore.doOnBeforeDownload(const browser       : ICefBrowser;
                                           const downloadItem  : ICefDownloadItem;
                                           const suggestedName : ustring;
                                           const callback      : ICefBeforeDownloadCallback);
begin
  if Assigned(FOnBeforeDownload) then FOnBeforeDownload(Self, browser, downloadItem, suggestedName, callback);
end;

function TChromiumCore.doOnBeforePopup(const browser            : ICefBrowser;
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

function TChromiumCore.doOnBeforeResourceLoad(const browser  : ICefBrowser;
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

function TChromiumCore.doOnBeforeUnloadDialog(const browser     : ICefBrowser;
                                              const messageText : ustring;
                                                    isReload    : Boolean;
                                              const callback    : ICefJsDialogCallback): Boolean;
begin
  Result := False;

  if Assigned(FOnBeforeUnloadDialog) then FOnBeforeUnloadDialog(Self, browser, messageText, isReload, callback, Result);
end;

function TChromiumCore.doOnCertificateError(const browser    : ICefBrowser;
                                                  certError  : TCefErrorcode;
                                            const requestUrl : ustring;
                                            const sslInfo    : ICefSslInfo;
                                            const callback   : ICefRequestCallback): Boolean;
begin
  Result := False;

  if Assigned(FOnCertificateError) then
    FOnCertificateError(Self, browser, certError, requestUrl, sslInfo, callback, Result);
end;

function TChromiumCore.doOnConsoleMessage(const browser  : ICefBrowser;
                                                level    : TCefLogSeverity;
                                          const aMessage : ustring;
                                          const source   : ustring;
                                                line     : Integer): Boolean;
begin
  Result := False;

  if Assigned(FOnConsoleMessage) then FOnConsoleMessage(Self, browser, level, aMessage, source, line, Result);
end;

function TChromiumCore.doOnAutoResize(const browser  : ICefBrowser;
                                      const new_size : PCefSize): Boolean;
begin
  Result := False;

  if Assigned(FOnAutoResize) then FOnAutoResize(Self, browser, new_size, Result);
end;

procedure TChromiumCore.doOnLoadingProgressChange(const browser: ICefBrowser; const progress: double);
begin
  if assigned(FOnLoadingProgressChange) then FOnLoadingProgressChange(self, browser, progress);
end;

function TChromiumCore.doOnContextMenuCommand(const browser    : ICefBrowser;
                                              const frame      : ICefFrame;
                                              const params     : ICefContextMenuParams;
                                                    commandId  : Integer;
                                                    eventFlags : TCefEventFlags): Boolean;
begin
  Result := False;

  if Assigned(FOnContextMenuCommand) then
    FOnContextMenuCommand(Self, browser, frame, params, commandId, eventFlags, Result);
end;

procedure TChromiumCore.doOnContextMenuDismissed(const browser: ICefBrowser; const frame: ICefFrame);
begin
  if Assigned(FOnContextMenuDismissed) then FOnContextMenuDismissed(Self, browser, frame);
end;

procedure TChromiumCore.doOnCursorChange(const browser          : ICefBrowser;
                                               cursor           : TCefCursorHandle;
                                               cursorType       : TCefCursorType;
                                         const customCursorInfo : PCefCursorInfo);
begin
  if assigned(FOnCursorChange) then FOnCursorChange(self, browser, cursor, cursorType, customCursorInfo);
end;

procedure TChromiumCore.doOnDialogClosed(const browser: ICefBrowser);
begin
  if Assigned(FOnDialogClosed) then FOnDialogClosed(Self, browser);
end;

procedure TChromiumCore.doOnDownloadUpdated(const browser      : ICefBrowser;
                                            const downloadItem : ICefDownloadItem;
                                            const callback     : ICefDownloadItemCallback);
begin
  if Assigned(FOnDownloadUpdated) then FOnDownloadUpdated(Self, browser, downloadItem, callback);
end;

function TChromiumCore.doOnDragEnter(const browser  : ICefBrowser;
                                     const dragData : ICefDragData;
                                           mask     : TCefDragOperations): Boolean;
begin
  Result := False;

  if Assigned(FOnDragEnter) then FOnDragEnter(Self, browser, dragData, mask, Result);
end;

procedure TChromiumCore.doOnDraggableRegionsChanged(const browser      : ICefBrowser;
                                                    const frame        : ICefFrame;
                                                          regionsCount : NativeUInt;
                                                    const regions      : PCefDraggableRegionArray);
begin
  if Assigned(FOnDraggableRegionsChanged) then FOnDraggableRegionsChanged(Self, browser, frame, regionsCount, regions);
end;

procedure TChromiumCore.doOnFaviconUrlChange(const browser: ICefBrowser; const iconUrls: TStrings);
begin
  if Assigned(FOnFavIconUrlChange) then FOnFavIconUrlChange(Self, browser, iconUrls);
end;

function TChromiumCore.doOnFileDialog(const browser              : ICefBrowser;
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

procedure TChromiumCore.doOnFindResult(const browser            : ICefBrowser;
                                             identifier         : integer;
                                             count              : Integer;
                                       const selectionRect      : PCefRect;
                                             activeMatchOrdinal : Integer;
                                             finalUpdate        : Boolean);
begin
  if Assigned(FOnFindResult) then
    FOnFindResult(Self, browser, identifier, count, selectionRect, activeMatchOrdinal, finalUpdate);
end;

procedure TChromiumCore.doOnRequestContextInitialized(const request_context: ICefRequestContext);
begin
  if assigned(FOnRequestContextInitialized) then FOnRequestContextInitialized(self, request_context);
end;

function TChromiumCore.doOnBeforePluginLoad(const mimeType     : ustring;
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

procedure TChromiumCore.doGetResourceRequestHandler_ReqCtxHdlr(const browser                  : ICefBrowser;
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

procedure TChromiumCore.doOnFullScreenModeChange(const browser: ICefBrowser; fullscreen: Boolean);
begin
  if Assigned(FOnFullScreenModeChange) then FOnFullScreenModeChange(Self, browser, fullscreen);
end;

function TChromiumCore.doOnGetAuthCredentials(const browser   : ICefBrowser;
                                              const originUrl : ustring;
                                                    isProxy   : Boolean;
                                              const host      : ustring;
                                                    port      : Integer;
                                              const realm     : ustring;
                                              const scheme    : ustring;
                                              const callback  : ICefAuthCallback): Boolean;
begin
  Result := False;

  if isProxy and (FProxyType = CEF_PROXYTYPE_FIXED_SERVERS) and (callback <> nil) then
    begin
      Result := True;
      callback.cont(FProxyUsername, FProxyPassword);
    end
   else
    if Assigned(FOnGetAuthCredentials) then
      FOnGetAuthCredentials(Self, browser, originUrl, isProxy, host, port, realm, scheme, callback, Result);
end;

function TChromiumCore.doCanSendCookie(const browser : ICefBrowser;
                                       const frame   : ICefFrame;
                                       const request : ICefRequest;
                                       const cookie  : PCefCookie): boolean;
begin
  Result := True;

  if assigned(FOnCanSendCookie) then FOnCanSendCookie(self, browser, frame, request, cookie, Result);
end;

function TChromiumCore.doCanSaveCookie(const browser  : ICefBrowser;
                                       const frame    : ICefFrame;
                                       const request  : ICefRequest;
                                       const response : ICefResponse;
                                       const cookie   : PCefCookie): boolean;
begin
  Result := True;

  if assigned(FOnCanSaveCookie) then FOnCanSaveCookie(self, browser, frame, request, response, cookie, Result);
end;

procedure TChromiumCore.doOnGetResourceHandler(const browser          : ICefBrowser;
                                               const frame            : ICefFrame;
                                               const request          : ICefRequest;
                                               var   aResourceHandler : ICefResourceHandler);
begin
  aResourceHandler := nil;

  if Assigned(FOnGetResourceHandler) then
    FOnGetResourceHandler(Self, browser, frame, request, aResourceHandler);
end;

procedure TChromiumCore.doOnGetAccessibilityHandler(var aAccessibilityHandler : ICefAccessibilityHandler);
begin
  if assigned(FOnGetAccessibilityHandler) then FOnGetAccessibilityHandler(Self, aAccessibilityHandler);
end;

function TChromiumCore.doOnGetRootScreenRect(const browser: ICefBrowser; var rect: TCefRect): Boolean;
begin
  Result := False;

  if Assigned(FOnGetRootScreenRect) then FOnGetRootScreenRect(Self, browser, rect, Result);
end;

function TChromiumCore.doOnGetScreenInfo(const browser: ICefBrowser; var screenInfo: TCefScreenInfo): Boolean;
begin
  Result := False;

  if Assigned(FOnGetScreenInfo) then FOnGetScreenInfo(Self, browser, screenInfo, Result);
end;

function TChromiumCore.doOnGetScreenPoint(const browser: ICefBrowser; viewX, viewY: Integer; var screenX, screenY: Integer): Boolean;
begin
  Result := False;

  if Assigned(FOnGetScreenPoint) then FOnGetScreenPoint(Self, browser, viewX, viewY, screenX, screenY, Result);
end;

procedure TChromiumCore.doOnGetViewRect(const browser: ICefBrowser; var rect: TCefRect);
begin
  if Assigned(FOnGetViewRect) then FOnGetViewRect(Self, browser, rect);
end;

procedure TChromiumCore.doOnGotFocus(const browser: ICefBrowser);
begin
  if Assigned(FOnGotFocus) then FOnGotFocus(Self, browser)
end;

function TChromiumCore.doOnJsdialog(const browser           : ICefBrowser;
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

function TChromiumCore.doOnKeyEvent(const browser : ICefBrowser;
                                    const event   : PCefKeyEvent;
                                          osEvent : TCefEventHandle): Boolean;
begin
  Result := False;

  if Assigned(FOnKeyEvent) then FOnKeyEvent(Self, browser, event, osEvent, Result);
end;

procedure TChromiumCore.doOnLoadEnd(const browser        : ICefBrowser;
                                    const frame          : ICefFrame;
                                          httpStatusCode : Integer);
begin
  if Assigned(FOnLoadEnd) then FOnLoadEnd(Self, browser, frame, httpStatusCode);
end;

procedure TChromiumCore.doOnLoadError(const browser   : ICefBrowser;
                                      const frame     : ICefFrame;
                                            errorCode : TCefErrorCode;
                                      const errorText : ustring;
                                      const failedUrl : ustring);
begin
  if Assigned(FOnLoadError) then FOnLoadError(Self, browser, frame, errorCode, errorText, failedUrl);
end;

procedure TChromiumCore.doOnLoadingStateChange(const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
begin
  if Assigned(FOnLoadingStateChange) then FOnLoadingStateChange(Self, browser, isLoading, canGoBack, canGoForward);
end;

procedure TChromiumCore.doOnLoadStart(const browser: ICefBrowser; const frame: ICefFrame; transitionType: TCefTransitionType);
begin
  if Assigned(FOnLoadStart) then FOnLoadStart(Self, browser, frame, transitionType);
end;

function TChromiumCore.doOnOpenUrlFromTab(const browser           : ICefBrowser;
                                          const frame             : ICefFrame;
                                          const targetUrl         : ustring;
                                                targetDisposition : TCefWindowOpenDisposition;
                                                userGesture       : Boolean): Boolean;
begin
  Result := False;

  if Assigned(FOnOpenUrlFromTab) then
    FOnOpenUrlFromTab(Self, browser, frame, targetUrl, targetDisposition, userGesture, Result);
end;

procedure TChromiumCore.doGetResourceRequestHandler_ReqHdlr(const browser                  : ICefBrowser;
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

procedure TChromiumCore.doOnPaint(const browser         : ICefBrowser;
                                        type_           : TCefPaintElementType;
                                        dirtyRectsCount : NativeUInt;
                                  const dirtyRects      : PCefRectArray;
                                  const buffer          : Pointer;
                                        width           : Integer;
                                        height          : Integer);
begin
  if Assigned(FOnPaint) then FOnPaint(Self, browser, type_, dirtyRectsCount, dirtyRects, buffer, width, height);
end;

procedure TChromiumCore.doOnAcceleratedPaint(const browser         : ICefBrowser;
                                                   type_           : TCefPaintElementType;
                                                   dirtyRectsCount : NativeUInt;
                                             const dirtyRects      : PCefRectArray;
                                                   shared_handle   : Pointer);
begin
  if Assigned(FOnAcceleratedPaint) then FOnAcceleratedPaint(Self, browser, type_, dirtyRectsCount, dirtyRects, shared_handle);
end;

function TChromiumCore.doOnSelectClientCertificate(const browser           : ICefBrowser;
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

procedure TChromiumCore.doOnPluginCrashed(const browser: ICefBrowser; const pluginPath: ustring);
begin
  if Assigned(FOnPluginCrashed) then FOnPluginCrashed(Self, browser, pluginPath);
end;

procedure TChromiumCore.doOnPopupShow(const browser: ICefBrowser; show: Boolean);
begin
  if assigned(FOnPopupShow) then FOnPopupShow(self, browser, show);
end;

procedure TChromiumCore.doOnPopupSize(const browser: ICefBrowser; const rect: PCefRect);
begin
  if assigned(FOnPopupSize) then FOnPopupSize(self, browser, rect);
end;

function TChromiumCore.doOnPreKeyEvent(const browser            : ICefBrowser;
                                   const event              : PCefKeyEvent;
                                         osEvent            : TCefEventHandle;
                                   out   isKeyboardShortcut : Boolean): Boolean;
begin
  Result := False;

  if Assigned(FOnPreKeyEvent) then FOnPreKeyEvent(Self, browser, event, osEvent, isKeyboardShortcut, Result);
end;

function TChromiumCore.doOnProcessMessageReceived(const browser       : ICefBrowser;
                                              const frame         : ICefFrame;
                                                    sourceProcess : TCefProcessId;
                                              const aMessage      : ICefProcessMessage): Boolean;
begin
  Result := False;

  if Assigned(FOnProcessMessageReceived) then
    FOnProcessMessageReceived(Self, browser, frame, sourceProcess, aMessage, Result);
end;

procedure TChromiumCore.doOnProtocolExecution(const browser          : ICefBrowser;
                                          const frame            : ICefFrame;
                                          const request          : ICefRequest;
                                          var   allowOsExecution : Boolean);
begin
  if Assigned(FOnProtocolExecution) then FOnProtocolExecution(Self, browser, frame, request, allowOsExecution);
end;

function TChromiumCore.doOnQuotaRequest(const browser   : ICefBrowser;
                                        const originUrl : ustring;
                                              newSize   : Int64;
                                        const callback  : ICefRequestCallback): Boolean;
begin
  Result := False;

  if Assigned(FOnQuotaRequest) then FOnQuotaRequest(Self, browser, originUrl, newSize, callback, Result);
end;

procedure TChromiumCore.doOnRenderProcessTerminated(const browser: ICefBrowser; status: TCefTerminationStatus);
begin
  if Assigned(FOnRenderProcessTerminated) then FOnRenderProcessTerminated(Self, browser, status);
end;

{$IFDEF MSWINDOWS}
function EnumProcOSRChromeWidgetWin0(hWnd: HWND; lParam: LPARAM): BOOL; stdcall;
var
  ClsName: array[0..256] of Char;
begin
  ClsName[GetClassName(hWnd, ClsName, 256)] := #0;
  if StrComp(ClsName, 'Chrome_WidgetWin_0') = 0 then
  begin
    PHandle(lParam)^ := hWnd;
    Result := False;
  end
  else
    Result := True;
end;
{$ENDIF MSWINDOWS}

procedure TChromiumCore.doOnRenderViewReady(const browser: ICefBrowser);
{$IFDEF MSWINDOWS}
var
  OldBrowserCompHWND, OldWidgetCompHWND, OldRenderCompHWND: THandle;
{$ENDIF}
begin
  if (browser            <> nil)        and
     (browser.Host       <> nil)        and
     (browser.Identifier =  FBrowserId) then
    begin
      {$IFDEF MSWINDOWS}
      OldBrowserCompHWND := FBrowserCompHWND;
      OldWidgetCompHWND  := FWidgetCompHWND;
      OldRenderCompHWND  := FRenderCompHWND;
      {$ENDIF}

      FBrowserCompHWND := browser.Host.WindowHandle;
      {$IFDEF MSWINDOWS}
      if (FBrowserCompHWND <> 0) then
        begin
          FWidgetCompHWND := FindWindowEx(FBrowserCompHWND, 0, 'Chrome_WidgetWin_0', '');

          if (FWidgetCompHWND = 0) and FIsOSR and CefCurrentlyOn(TID_UI) then
            begin
              // The WidgetCompHWND window doesn't have a HwndParent (Owner). If we are in OSR mode this
              // causes popup menus that are opened by CEF to stay open if the user clicks somewhere else.
              // With this code we search for the Widget window in the UI Thread's window list and set
              // the Browser window as its HwndParent. This works around the bug.
              EnumThreadWindows(GetCurrentThreadId, @EnumProcOSRChromeWidgetWin0, NativeInt(@FWidgetCompHWND));

              if (FWidgetCompHWND <> 0) then
                SetWindowLongPtr(FWidgetCompHWND, GWLP_HWNDPARENT, NativeInt(FBrowserCompHWND));
            end;
        end;

      if (FWidgetCompHWND <> 0) then
        FRenderCompHWND := FindWindowEx(FWidgetCompHWND, 0, 'Chrome_RenderWidgetHostHWND', 'Chrome Legacy Window');

      RestoreCompWndProc(OldBrowserCompHWND, FBrowserCompHWND, FOldBrowserCompWndPrc);
      if assigned(FOnBrowserCompMsg) and (FBrowserCompHWND <> 0) and (FOldBrowserCompWndPrc = nil) then
        begin
          CreateStub({$IFDEF FPC}@{$ENDIF}BrowserCompWndProc, FBrowserCompStub);
          FOldBrowserCompWndPrc := InstallCompWndProc(FBrowserCompHWND, FBrowserCompStub);
        end;

      RestoreCompWndProc(OldWidgetCompHWND, FWidgetCompHWND, FOldWidgetCompWndPrc);
      if assigned(FOnWidgetCompMsg) and (FWidgetCompHWND <> 0) and (FOldWidgetCompWndPrc = nil) then
        begin
          CreateStub({$IFDEF FPC}@{$ENDIF}WidgetCompWndProc, FWidgetCompStub);
          FOldWidgetCompWndPrc := InstallCompWndProc(FWidgetCompHWND, FWidgetCompStub);
        end;

      RestoreCompWndProc(OldRenderCompHWND, FRenderCompHWND, FOldRenderCompWndPrc);
      if assigned(FOnRenderCompMsg) and (FRenderCompHWND <> 0) and (FOldRenderCompWndPrc = nil) then
        begin
          CreateStub({$IFDEF FPC}@{$ENDIF}RenderCompWndProc, FRenderCompStub);
          FOldRenderCompWndPrc := InstallCompWndProc(FRenderCompHWND, FRenderCompStub);
        end;
      {$ENDIF}
    end;

  if Assigned(FOnRenderViewReady) then FOnRenderViewReady(Self, browser);
end;

procedure TChromiumCore.doOnResetDialogState(const browser: ICefBrowser);
begin
  if Assigned(FOnResetDialogState) then FOnResetDialogState(Self, browser);
end;

procedure TChromiumCore.doOnResourceRedirect(const browser  : ICefBrowser;
                                             const frame    : ICefFrame;
                                             const request  : ICefRequest;
                                             const response : ICefResponse;
                                             var   newUrl   : ustring);
begin
  if Assigned(FOnResourceRedirect) then FOnResourceRedirect(Self, browser, frame, request, response, newUrl);
end;

function TChromiumCore.doOnResourceResponse(const browser  : ICefBrowser;
                                            const frame    : ICefFrame;
                                            const request  : ICefRequest;
                                            const response : ICefResponse): Boolean;
begin
  Result := False;

  if Assigned(FOnResourceResponse) then FOnResourceResponse(Self, browser, frame, request, response, Result);
end;

procedure TChromiumCore.doOnGetResourceResponseFilter(const browser         : ICefBrowser;
                                                      const frame           : ICefFrame;
                                                      const request         : ICefRequest;
                                                      const response        : ICefResponse;
                                                      var   aResponseFilter : ICefResponseFilter);
begin
  aResponseFilter := nil;

  if Assigned(FOnGetResourceResponseFilter) then
    FOnGetResourceResponseFilter(self, browser, frame, request, response, aResponseFilter);
end;

procedure TChromiumCore.doOnResourceLoadComplete(const browser               : ICefBrowser;
                                                 const frame                 : ICefFrame;
                                                 const request               : ICefRequest;
                                                 const response              : ICefResponse;
                                                       status                : TCefUrlRequestStatus;
                                                       receivedContentLength : Int64);
begin
  if Assigned(FOnResourceLoadComplete) then
    FOnResourceLoadComplete(self, browser, frame, request, response, status, receivedContentLength);
end;

procedure TChromiumCore.doOnScrollOffsetChanged(const browser: ICefBrowser; x, y: Double);
begin
  if Assigned(FOnScrollOffsetChanged) then FOnScrollOffsetChanged(Self, browser, x, y);
end;

procedure TChromiumCore.doOnIMECompositionRangeChanged(const browser               : ICefBrowser;
                                                       const selected_range        : PCefRange;
                                                             character_boundsCount : NativeUInt;
                                                       const character_bounds      : PCefRect);
begin
  if assigned(FOnIMECompositionRangeChanged) then
    FOnIMECompositionRangeChanged(self, browser, selected_range, character_boundsCount, character_bounds);
end;

procedure TChromiumCore.doOnTextSelectionChanged(const browser        : ICefBrowser;
                                                 const selected_text  : ustring;
                                                 const selected_range : PCefRange);
begin
  if assigned(FOnTextSelectionChanged) then
    FOnTextSelectionChanged(self, browser, selected_text, selected_range);
end;

procedure TChromiumCore.doOnVirtualKeyboardRequested(const browser    : ICefBrowser;
                                                           input_mode : TCefTextInpuMode);
begin
  if assigned(FOnVirtualKeyboardRequested) then
    FOnVirtualKeyboardRequested(self, browser, input_mode);
end;

function TChromiumCore.doOnSetFocus(const browser: ICefBrowser; source: TCefFocusSource): Boolean;
begin
  Result := False;

  if Assigned(FOnSetFocus) then FOnSetFocus(Self, browser, source, Result);
end;

function TChromiumCore.doOnStartDragging(const browser    : ICefBrowser;
                                         const dragData   : ICefDragData;
                                               allowedOps : TCefDragOperations;
                                               x          : integer;
                                               y          : Integer): Boolean;
begin
  Result := False;
  {$IFDEF MSWINDOWS}
  if FDragAndDropInitialized and
     FDragDropManager.CloneDragData(dragData, allowedOps) then
    begin
      Result := True;
      SendCompMessage(CEF_STARTDRAGGING);
    end;

  if Assigned(FOnStartDragging) then FOnStartDragging(Self, browser, dragData, allowedOps, x, y, Result);
  {$ENDIF}
end;

procedure TChromiumCore.DelayedDragging;
{$IFDEF MSWINDOWS}
var
  TempOperation : TCefDragOperation;
  TempPoint     : TPoint;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  if FDragAndDropInitialized and (FDropTargetWnd <> 0) and (GlobalCEFApp <> nil) then
    begin
      FDragOperations := DRAG_OPERATION_NONE;
      TempOperation   := FDragDropManager.StartDragging;
      FDragOperations := DRAG_OPERATION_NONE;

      GetCursorPos(TempPoint);
      MapWindowPoints(0, FDropTargetWnd, TempPoint, 1);
      DeviceToLogical(TempPoint, GlobalCEFApp.DeviceScaleFactor);

      DragSourceEndedAt(TempPoint.x, TempPoint.y, TempOperation);
      DragSourceSystemDragEnded;
    end;
  {$ENDIF}
end;

procedure TChromiumCore.doOnStatusMessage(const browser: ICefBrowser; const value: ustring);
begin
  if Assigned(FOnStatusMessage) then FOnStatusMessage(Self, browser, value);
end;

procedure TChromiumCore.doOnTakeFocus(const browser: ICefBrowser; next: Boolean);
begin
  if Assigned(FOnTakeFocus) then FOnTakeFocus(Self, browser, next);
end;

procedure TChromiumCore.doOnTitleChange(const browser: ICefBrowser; const title: ustring);
begin
  if Assigned(FOnTitleChange) then FOnTitleChange(Self, browser, title);
end;

function TChromiumCore.doOnTooltip(const browser: ICefBrowser; var text: ustring): Boolean;
begin
  Result := False;

  if Assigned(FOnTooltip) then FOnTooltip(Self, browser, text, Result);
end;

procedure TChromiumCore.doOnUpdateDragCursor(const browser: ICefBrowser; operation: TCefDragOperation);
begin
  if FDragAndDropInitialized then FDragOperations := operation;

  if Assigned(FOnUpdateDragCursor) then FOnUpdateDragCursor(Self, browser, operation);
end;

procedure TChromiumCore.WasResized;
begin
  if Initialized then FBrowser.Host.WasResized;
end;

procedure TChromiumCore.WasHidden(hidden: Boolean);
begin
  if Initialized then FBrowser.Host.WasHidden(hidden);
end;

procedure TChromiumCore.NotifyScreenInfoChanged;
begin
  if Initialized then FBrowser.Host.NotifyScreenInfoChanged;
end;

procedure TChromiumCore.NotifyMoveOrResizeStarted;
begin
  if Initialized then FBrowser.Host.NotifyMoveOrResizeStarted;
end;

procedure TChromiumCore.Invalidate(type_: TCefPaintElementType);
begin
  if Initialized then
    begin
      if FIsOSR then
        FBrowser.Host.Invalidate(type_)
       {$IFDEF MSWINDOWS}
       else
        if (RenderHandle <> 0) then
          InvalidateRect(RenderHandle, nil, False)
         else
          InvalidateRect(WindowHandle, nil, False);
       {$ENDIF}
    end;
end;

procedure TChromiumCore.SendExternalBeginFrame;
begin
  if Initialized then FBrowser.Host.SendExternalBeginFrame;
end;

procedure TChromiumCore.SendKeyEvent(const event: PCefKeyEvent);
begin
  if Initialized then FBrowser.Host.SendKeyEvent(event);
end;

procedure TChromiumCore.SendMouseClickEvent(const event      : PCefMouseEvent;
                                                  type_      : TCefMouseButtonType;
                                                  mouseUp    : Boolean;
                                                  clickCount : Integer);
begin
  if Initialized then FBrowser.Host.SendMouseClickEvent(event, type_, mouseUp, clickCount);
end;

procedure TChromiumCore.SendMouseMoveEvent(const event: PCefMouseEvent; mouseLeave: Boolean);
begin
  if Initialized then FBrowser.Host.SendMouseMoveEvent(event, mouseLeave);
end;

procedure TChromiumCore.SendMouseWheelEvent(const event: PCefMouseEvent; deltaX, deltaY: Integer);
begin
  if Initialized then FBrowser.Host.SendMouseWheelEvent(event, deltaX, deltaY);
end;

procedure TChromiumCore.SendTouchEvent(const event: PCefTouchEvent);
begin
  if Initialized then FBrowser.Host.SendTouchEvent(event);
end;

procedure TChromiumCore.SendFocusEvent(setFocus: Boolean);
begin
  if Initialized then FBrowser.Host.SendFocusEvent(setFocus);
end;

procedure TChromiumCore.SendCaptureLostEvent;
begin
  if Initialized then FBrowser.Host.SendCaptureLostEvent;
end;

procedure TChromiumCore.SetFocus(focus: Boolean);
begin
  if Initialized then FBrowser.Host.SetFocus(focus);
end;

procedure TChromiumCore.SetAccessibilityState(accessibilityState: TCefState);
begin
  if Initialized then FBrowser.Host.SetAccessibilityState(accessibilityState);
end;

procedure TChromiumCore.SendProcessMessage(targetProcess: TCefProcessId; const ProcMessage: ICefProcessMessage; const aFrameName : ustring);
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
      if CustomExceptionHandler('TChromiumCore.SendProcessMessage', e) then raise;
  end;
end;

procedure TChromiumCore.SendProcessMessage(targetProcess: TCefProcessId; const ProcMessage: ICefProcessMessage; const aFrame : ICefFrame);
begin
  try
    if Initialized and (aFrame <> nil) and aFrame.IsValid then
      aFrame.SendProcessMessage(targetProcess, ProcMessage);
  except
    on e : exception do
      if CustomExceptionHandler('TChromiumCore.SendProcessMessage', e) then raise;
  end;
end;

procedure TChromiumCore.SendProcessMessage(targetProcess: TCefProcessId; const ProcMessage: ICefProcessMessage; const aFrameIdentifier : int64);
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
      if CustomExceptionHandler('TChromiumCore.SendProcessMessage', e) then raise;
  end;
end;

function TChromiumCore.CreateUrlRequest(const request: ICefRequest; const client: ICefUrlrequestClient; const aFrameName : ustring): ICefUrlRequest;
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
      if CustomExceptionHandler('TChromiumCore.CreateUrlRequest', e) then raise;
  end;
end;

function TChromiumCore.CreateUrlRequest(const request: ICefRequest; const client: ICefUrlrequestClient; const aFrame : ICefFrame): ICefUrlRequest;
begin
  Result := nil;

  try
    if Initialized and (aFrame <> nil) and aFrame.IsValid then
      Result := aFrame.CreateUrlRequest(request, client);
  except
    on e : exception do
      if CustomExceptionHandler('TChromiumCore.CreateUrlRequest', e) then raise;
  end;
end;

function TChromiumCore.CreateUrlRequest(const request: ICefRequest; const client: ICefUrlrequestClient; const aFrameIdentifier : int64): ICefUrlRequest;
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
      if CustomExceptionHandler('TChromiumCore.CreateUrlRequest', e) then raise;
  end;
end;

procedure TChromiumCore.DragTargetDragEnter(const dragData: ICefDragData; const event: PCefMouseEvent; allowedOps: TCefDragOperations);
begin
  if Initialized then FBrowser.Host.DragTargetDragEnter(dragData, event, allowedOps);
end;

procedure TChromiumCore.DragTargetDragOver(const event: PCefMouseEvent; allowedOps: TCefDragOperations);
begin
  if Initialized then FBrowser.Host.DragTargetDragOver(event, allowedOps);
end;

procedure TChromiumCore.DragTargetDragLeave;
begin
  if Initialized then FBrowser.Host.DragTargetDragLeave;
end;

procedure TChromiumCore.DragTargetDrop(const event: PCefMouseEvent);
begin
  if Initialized then FBrowser.Host.DragTargetDrop(event);
end;

procedure TChromiumCore.DragSourceEndedAt(x, y: Integer; op: TCefDragOperation);
begin
  if Initialized then FBrowser.Host.DragSourceEndedAt(x, y, op);
end;

procedure TChromiumCore.DragSourceSystemDragEnded;
begin
  if Initialized then FBrowser.Host.DragSourceSystemDragEnded;
end;

procedure TChromiumCore.IMESetComposition(const text              : ustring;
                                          const underlines        : TCefCompositionUnderlineDynArray;
                                          const replacement_range : PCefRange;
                                          const selection_range   : PCefRange);
begin
  if Initialized then
    FBrowser.Host.IMESetComposition(text, underlines, replacement_range, selection_range);
end;

procedure TChromiumCore.IMECommitText(const text                : ustring;
                                      const replacement_range   : PCefRange;
                                            relative_cursor_pos : integer);
begin
  if Initialized then
    FBrowser.Host.IMECommitText(text, replacement_range, relative_cursor_pos);
end;

procedure TChromiumCore.IMEFinishComposingText(keep_selection : boolean);
begin
  if Initialized then FBrowser.Host.IMEFinishComposingText(keep_selection);
end;

procedure TChromiumCore.IMECancelComposition;
begin
  if Initialized then FBrowser.Host.IMECancelComposition;
end;

{$IFDEF MSWINDOWS}
function TChromiumCore.CopyDCToBitmapStream(aSrcDC : HDC; const aSrcRect : TRect; var aStream : TStream) : boolean;
var
  TempDstDC     : HDC;
  TempWidth     : Integer;
  TempHeight    : Integer;
  TempInfo      : TBitmapInfo;
  TempBits      : Pointer;
  TempNewBitmap : HBITMAP;
  TempOldBitmap : HBITMAP;
  TempHeader    : TBitmapFileHeader;
begin
  Result := False;
  if (aSrcDC = 0) or (aStream = nil) then exit;

  TempDstDC := CreateCompatibleDC(aSrcDC);
  if (TempDstDC = 0) then exit;

  TempWidth  := aSrcRect.Right  - aSrcRect.Left;
  TempHeight := aSrcRect.Bottom - aSrcRect.Top;
  TempBits   := nil;

  if (TempWidth > 0) and (TempHeight > 0) then
    begin
      ZeroMemory(@TempInfo,   SizeOf(TBitmapInfo));
      ZeroMemory(@TempHeader, SizeOf(TBitmapFileHeader));

      TempInfo.bmiHeader.biSize        := SizeOf(TBitmapInfoHeader);
      TempInfo.bmiHeader.biWidth       := TempWidth;
      TempInfo.bmiHeader.biHeight      := TempHeight;
      TempInfo.bmiHeader.biPlanes      := 1;
      TempInfo.bmiHeader.biBitCount    := 32;
      TempInfo.bmiHeader.biCompression := BI_RGB;
      TempInfo.bmiHeader.biSizeImage   := TempWidth * TempHeight * SizeOf(TRGBQuad);

      TempNewBitmap := CreateDIBSection(TempDstDC, TempInfo, DIB_RGB_COLORS, TempBits, 0, 0);

      if (TempNewBitmap <> 0) then
        try
          TempOldBitmap := SelectObject(TempDstDC, TempNewBitmap);

          if BitBlt(TempDstDC, 0, 0, TempWidth, TempHeight, aSrcDC, aSrcRect.Left, aSrcRect.Top, SRCCOPY) then
            begin
              TempHeader.bfType    := $4D42; // "BM" bitmap header
              TempHeader.bfOffBits := sizeof(TBitmapFileHeader) + TempInfo.bmiHeader.biSize + TempInfo.bmiHeader.biClrUsed * SizeOf(TRGBQuad);
              TempHeader.bfSize    := TempHeader.bfOffBits + TempInfo.bmiHeader.biSizeImage;

              aStream.position := 0;

              aStream.Write(TempHeader,         SizeOf(TBitmapFileHeader));
              aStream.Write(TempInfo.bmiHeader, SizeOf(TBitmapInfoHeader));
              aStream.Write(TempBits^,          TempInfo.bmiHeader.biSizeImage);

              aStream.position := 0;
              Result           := True;
            end;

          SelectObject(TempDstDC, TempOldBitmap);
        finally
          DeleteObject(TempNewBitmap);
        end;
    end;

  ReleaseDC(0, TempDstDC);
end;
{$ENDIF}

end.
