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
//        Copyright © 2017 Salvador Díaz Fau. All rights reserved.
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

unit uCEFChromium;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  WinApi.Windows, WinApi.Messages, System.Classes, Vcl.Controls, Vcl.Graphics, Vcl.Forms, WinApi.ActiveX,
  {$ELSE}
  Windows, Messages, Classes, Controls, Graphics, Forms, ActiveX,
  {$ENDIF}
  uCEFTypes, uCEFInterfaces, uCEFLibFunctions, uCEFMiscFunctions, uCEFClient,
  uCEFPDFPrintCallback, uCEFStringVisitor, uCEFConstants, uCEFTask,
  uCEFDeleteCookiesCallback, uCEFDomVisitor, uCEFChromiumEvents,
  uCEFChromiumOptions, uCEFChromiumFontOptions, uCEFPDFPrintOptions,
  uCEFDragAndDropMgr;

type
  TChromium = class(TComponent, IChromiumEvents)
    protected
      FCompHandle             : HWND;
      FVisitor                : ICefStringVisitor;
      FPDFPrintcb             : ICefPdfPrintCallback;
      FCookiDeletercb         : ICefDeleteCookiesCallback;
      FHandler                : ICefClient;
      FBrowser                : ICefBrowser;
      FBrowserId              : Integer;
      FDefaultUrl             : ustring;
      FOptions                : TChromiumOptions;
      FFontOptions            : TChromiumFontOptions;
      FPDFPrintOptions        : TPDFPrintOptions;
      FDefaultEncoding        : ustring;
      FProxyType              : integer;
      FProxyServer            : string;
      FProxyPort              : integer;
      FProxyUsername          : string;
      FProxyPassword          : string;
      FProxyScriptURL         : string;
      FProxyByPassList        : string;
      FUpdatePreferences      : boolean;
      FCustomHeaderName       : string;
      FCustomHeaderValue      : string;
      FAddCustomHeader        : boolean;
      FDoNotTrack             : boolean;
      FSendReferrer           : boolean;
      FHyperlinkAuditing      : boolean;
      FCookiePrefs            : integer;
      FImagesPrefs            : integer;
      FCMStoragePath          : ustring;
      FZoomStep               : byte;
      FWindowName             : string;
      FPrefsFileName          : string;
      FIsOSR                  : boolean;
      FInitialized            : boolean;
      FClosing                : boolean;
      FWindowInfo             : TCefWindowInfo;
      FBrowserSettings        : TCefBrowserSettings;
      FDevWindowInfo          : TCefWindowInfo;
      FDevBrowserSettings     : TCefBrowserSettings;
      FDragOperations         : TCefDragOperations;
      FDragDropManager        : TCEFDragAndDropMgr;
      FDropTargetCtrl         : TWinControl;
      FDragAndDropInitialized : boolean;

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

      // ICefDownloadHandler
      FOnBeforeDownload               : TOnBeforeDownload;
      FOnDownloadUpdated              : TOnDownloadUpdated;

      // ICefGeolocationHandler
      FOnRequestGeolocationPermission : TOnRequestGeolocationPermission;
      FOnCancelGeolocationPermission  : TOnCancelGeolocationPermission;

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
      FOnStartDragging                : TOnStartDragging;
      FOnUpdateDragCursor             : TOnUpdateDragCursor;
      FOnScrollOffsetChanged          : TOnScrollOffsetChanged;
      FOnIMECompositionRangeChanged   : TOnIMECompositionRangeChanged;

      // ICefDragHandler
      FOnDragEnter                    : TOnDragEnter;
      FOnDraggableRegionsChanged      : TOnDraggableRegionsChanged;

      // ICefFindHandler
      FOnFindResult                   : TOnFindResult;

      // Custom
      FOnTextResultAvailable          : TOnTextResultAvailableEvent;
      FOnPdfPrintFinished             : TOnPdfPrintFinishedEvent;
      FOnPrefsAvailable               : TNotifyEvent;
      FOnCookiesDeleted               : TOnCookiesDeletedEvent;

      function  GetIsLoading : boolean;
      function  GetMultithreadApp : boolean;
      function  GetHasDocument : boolean;
      function  GetHasView : boolean;
      function  GetHasDevTools : boolean;
      function  GetHasClientHandler : boolean;
      function  GetHasBrowser : boolean;
      function  GetCanGoBack : boolean;
      function  GetCanGoForward : boolean;
      function  GetDocumentURL : string;
      function  GetZoomLevel : double;
      function  GetZoomPct : double;
      function  GetIsPopUp : boolean;
      function  GetWindowHandle : THandle;
      function  GetWindowlessFrameRate : integer;
      function  GetFrameIsFocused : boolean;
      function  GetInitialized : boolean;
      function  GetVisibleNavigationEntry : ICefNavigationEntry;
      function  GetHasValidMainFrame : boolean;

      procedure SetDoNotTrack(aValue : boolean);
      procedure SetSendReferrer(aValue : boolean);
      procedure SetHyperlinkAuditing(aValue : boolean);
      procedure SetCookiePrefs(aValue : integer);
      procedure SetImagesPrefs(aValue : integer);
      procedure SetProxyType(aValue : integer);
      procedure SetProxyServer(const aValue : string);
      procedure SetProxyPort(aValue : integer);
      procedure SetProxyUsername(const aValue : string);
      procedure SetProxyPassword(const aValue : string);
      procedure SetProxyScriptURL(const aValue : string);
      procedure SetProxyByPassList(const aValue : string);
      procedure SetCustomHeaderName(const aValue : string);
      procedure SetCustomHeaderValue(const aValue : string);
      procedure SetCMStoragePath(const aValue : ustring);
      procedure SetZoomLevel(const aValue : double);
      procedure SetZoomPct(const aValue : double);
      procedure SetZoomStep(aValue : byte);
      procedure SetWindowlessFrameRate(aValue : integer);


      function  CreateBrowserHost(aWindowInfo : PCefWindowInfo; const aURL : ustring; const aSettings : PCefBrowserSettings; const aContext : ICefRequestContext): Boolean;
      function  CreateBrowserHostSync(aWindowInfo : PCefWindowInfo; const aURL : ustring; const aSettings : PCefBrowserSettings; const aContext : ICefRequestContext): ICefBrowser;

      procedure InitializeEvents;
      procedure InitializeSettings(var aSettings : TCefBrowserSettings);

      procedure GetSettings(var aSettings : TCefBrowserSettings);
      procedure GetPrintPDFSettings(var aSettings : TCefPdfPrintSettings; const aTitle, aURL : string);

      function  UpdateProxyPrefs : boolean;
      function  UpdatePreference(const aName : string; aValue : boolean) : boolean; overload;
      function  UpdatePreference(const aName : string; aValue : integer) : boolean; overload;
      function  UpdatePreference(const aName : string; const aValue : double) : boolean; overload;
      function  UpdatePreference(const aName, aValue : string) : boolean; overload;

      procedure HandleDictionary(const aDict : ICefDictionaryValue; var aResultSL : TStringList; const aRoot : string);
      procedure HandleNull(const aValue : ICefValue; var aResultSL : TStringList; const aRoot, aKey : string);
      procedure HandleBool(const aValue : ICefValue; var aResultSL : TStringList; const aRoot, aKey : string);
      procedure HandleInteger(const aValue : ICefValue; var aResultSL : TStringList; const aRoot, aKey : string);
      procedure HandleDouble(const aValue : ICefValue; var aResultSL : TStringList; const aRoot, aKey : string);
      procedure HandleString(const aValue : ICefValue; var aResultSL : TStringList; const aRoot, aKey : string);
      procedure HandleBinary(const aValue : ICefValue; var aResultSL : TStringList; const aRoot, aKey : string);
      procedure HandleList(const aValue : ICefValue; var aResultSL : TStringList; const aRoot, aKey : string);
      procedure HandleInvalid(const aValue : ICefValue; var aResultSL : TStringList; const aRoot, aKey : string);

      procedure PrefsAvailableMsg(var aMessage : TMessage);
      function  GetParentForm : TCustomForm;
      procedure ApplyZoomStep;
      procedure DelayedDragging;
      function  SendCompMessage(aMsg : cardinal; wParam : cardinal = 0; lParam : integer = 0) : boolean;
      procedure ToMouseEvent(grfKeyState : Longint; pt : TPoint; var aMouseEvent : TCefMouseEvent);
      procedure WndProc(var aMessage: TMessage);

      procedure DragDropManager_OnDragEnter(Sender: TObject; const aDragData : ICefDragData; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint);
      procedure DragDropManager_OnDragOver(Sender: TObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint);
      procedure DragDropManager_OnDragLeave(Sender: TObject);
      procedure DragDropManager_OnDrop(Sender: TObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint);

      // ICefClient
      function  doOnProcessMessageReceived(const browser: ICefBrowser; sourceProcess: TCefProcessId; const aMessage: ICefProcessMessage): Boolean; virtual;

      // ICefLoadHandler
      procedure doOnLoadStart(const browser: ICefBrowser; const frame: ICefFrame; transitionType: TCefTransitionType); virtual;
      procedure doOnLoadEnd(const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: Integer); virtual;
      procedure doOnLoadError(const browser: ICefBrowser; const frame: ICefFrame; errorCode: Integer; const errorText, failedUrl: ustring); virtual;
      procedure doOnLoadingStateChange(const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean); virtual;

      // ICefFocusHandler
      procedure doOnTakeFocus(const browser: ICefBrowser; next: Boolean); virtual;
      function  doOnSetFocus(const browser: ICefBrowser; source: TCefFocusSource): Boolean; virtual;
      procedure doOnGotFocus(const browser: ICefBrowser); virtual;

      // ICefContextMenuHandler
      procedure doOnBeforeContextMenu(const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; const model: ICefMenuModel); virtual;
      function  doOnContextMenuCommand(const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; commandId: Integer; eventFlags: TCefEventFlags): Boolean; virtual;
      procedure doOnContextMenuDismissed(const browser: ICefBrowser; const frame: ICefFrame); virtual;

      // ICefKeyboardHandler
      function  doOnPreKeyEvent(const browser: ICefBrowser; const event: PCefKeyEvent; osEvent: TCefEventHandle; out isKeyboardShortcut: Boolean): Boolean; virtual;
      function  doOnKeyEvent(const browser: ICefBrowser; const event: PCefKeyEvent; osEvent: TCefEventHandle): Boolean; virtual;

      // ICefDisplayHandler
      procedure doOnAddressChange(const browser: ICefBrowser; const frame: ICefFrame; const url: ustring); virtual;
      procedure doOnTitleChange(const browser: ICefBrowser; const title: ustring); virtual;
      procedure doOnFaviconUrlChange(const browser: ICefBrowser; iconUrls: TStrings); virtual;
      procedure doOnFullScreenModeChange(const browser: ICefBrowser; fullscreen: Boolean); virtual;
      function  doOnTooltip(const browser: ICefBrowser; var text: ustring): Boolean; virtual;
      procedure doOnStatusMessage(const browser: ICefBrowser; const value: ustring); virtual;
      function  doOnConsoleMessage(const browser: ICefBrowser; const aMessage, source: ustring; line: Integer): Boolean; virtual;

      // ICefDownloadHandler
      procedure doOnBeforeDownload(const browser: ICefBrowser; const downloadItem: ICefDownloadItem; const suggestedName: ustring; const callback: ICefBeforeDownloadCallback); virtual;
      procedure doOnDownloadUpdated(const browser: ICefBrowser; const downloadItem: ICefDownloadItem; const callback: ICefDownloadItemCallback); virtual;

      // ICefGeolocationHandler
      function  doOnRequestGeolocationPermission(const browser: ICefBrowser; const requestingUrl: ustring; requestId: Integer; const callback: ICefGeolocationCallback): Boolean; virtual;
      procedure doOnCancelGeolocationPermission(const browser: ICefBrowser; requestId: Integer); virtual;

      // ICefJsDialogHandler
      function  doOnJsdialog(const browser: ICefBrowser; const originUrl: ustring; dialogType: TCefJsDialogType; const messageText, defaultPromptText: ustring; const callback: ICefJsDialogCallback; out suppressMessage: Boolean): Boolean; virtual;
      function  doOnBeforeUnloadDialog(const browser: ICefBrowser; const messageText: ustring; isReload: Boolean; const callback: ICefJsDialogCallback): Boolean; virtual;
      procedure doOnResetDialogState(const browser: ICefBrowser); virtual;
      procedure doOnDialogClosed(const browser: ICefBrowser); virtual;

      // ICefLifeSpanHandler
      function  doOnBeforePopup(const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var noJavascriptAccess: Boolean): Boolean; virtual;
      procedure doOnAfterCreated(const browser: ICefBrowser); virtual;
      procedure doOnBeforeClose(const browser: ICefBrowser); virtual;
      function  doOnClose(const browser: ICefBrowser): Boolean; virtual;

      // ICefRequestHandler
      function  doOnBeforeBrowse(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; isRedirect: Boolean): Boolean; virtual;
      function  doOnOpenUrlFromTab(const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean): Boolean; virtual;
      function  doOnBeforeResourceLoad(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const callback: ICefRequestCallback): TCefReturnValue; virtual;
      function  doOnGetResourceHandler(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest): ICefResourceHandler; virtual;
      procedure doOnResourceRedirect(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; var newUrl: ustring); virtual;
      function  doOnResourceResponse(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse): Boolean; virtual;
      function  doOnGetResourceResponseFilter(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse): ICefResponseFilter; virtual;
      procedure doOnResourceLoadComplete(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; status: TCefUrlRequestStatus; receivedContentLength: Int64); virtual;
      function  doOnGetAuthCredentials(const browser: ICefBrowser; const frame: ICefFrame; isProxy: Boolean; const host: ustring; port: Integer; const realm, scheme: ustring; const callback: ICefAuthCallback): Boolean; virtual;
      function  doOnQuotaRequest(const browser: ICefBrowser; const originUrl: ustring; newSize: Int64; const callback: ICefRequestCallback): Boolean; virtual;
      procedure doOnProtocolExecution(const browser: ICefBrowser; const url: ustring; out allowOsExecution: Boolean); virtual;
      function  doOnCertificateError(const browser: ICefBrowser; certError: TCefErrorcode; const requestUrl: ustring; const sslInfo: ICefSslInfo; const callback: ICefRequestCallback): Boolean; virtual;
      function  doOnSelectClientCertificate(const browser: ICefBrowser; isProxy: boolean; const host: ustring; port: integer; certificatesCount: NativeUInt; const certificates: TCefX509CertificateArray; const callback: ICefSelectClientCertificateCallback): boolean; virtual;
      procedure doOnPluginCrashed(const browser: ICefBrowser; const pluginPath: ustring); virtual;
      procedure doOnRenderViewReady(const browser: ICefBrowser); virtual;
      procedure doOnRenderProcessTerminated(const browser: ICefBrowser; status: TCefTerminationStatus); virtual;

      // ICefDialogHandler
      function  doOnFileDialog(const browser: ICefBrowser; mode: TCefFileDialogMode; const title, defaultFilePath: ustring; acceptFilters: TStrings; selectedAcceptFilter: Integer; const callback: ICefFileDialogCallback): Boolean; virtual;

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

      // ICefDragHandler
      function  doOnDragEnter(const browser: ICefBrowser; const dragData: ICefDragData; mask: TCefDragOperations): Boolean; virtual;
      procedure doOnDraggableRegionsChanged(const browser: ICefBrowser; regionsCount: NativeUInt; regions: PCefDraggableRegionArray); virtual;

      // ICefFindHandler
      procedure doOnFindResult(const browser: ICefBrowser; identifier, count: Integer; const selectionRect: PCefRect; activeMatchOrdinal: Integer; finalUpdate: Boolean); virtual;

    public
      constructor Create(AOwner: TComponent); override;
      destructor  Destroy; override;
      procedure   AfterConstruction; override;
      function    CreateClientHandler(aIsOSR : boolean) : boolean; overload;
      function    CreateClientHandler(var aClient : ICefClient) : boolean; overload;
      procedure   CloseBrowser(aForceClose : boolean);
      function    CreateBrowser(const aBrowserParent : TWinControl = nil; const aWindowName : string = '') : boolean; overload;
      function    CreateBrowser(aParentHandle : HWND; aParentRect : TRect; const aWindowName : string = '') : boolean; overload;
      procedure   InitializeDragAndDrop(const aDropTargetCtrl : TWinControl);
      procedure   ShutdownDragAndDrop;

      // Internal procedures.
      // Only tasks, visitors or callbacks should use them in the right thread/process.
      procedure   Internal_CookiesDeleted(numDeleted : integer);
      procedure   Internal_GetHTML;
      procedure   Internal_PdfPrintFinished(aResultOK : boolean);
      procedure   Internal_TextResultAvailable(const aText : string);
      procedure   Internal_UpdatePreferences;
      procedure   Internal_SavePreferences;

      procedure   LoadURL(const aURL : ustring);
      procedure   LoadString(const aString : ustring; const aURL : ustring = '');
      procedure   LoadRequest(const aRequest: ICefRequest);

      procedure   GoBack;
      procedure   GoForward;
      procedure   Reload;
      procedure   ReloadIgnoreCache;
      procedure   StopLoad;
      procedure   StartDownload(const aURL : ustring);

      procedure   SimulateMouseWheel(aDeltaX, aDeltaY : integer);
      procedure   DeleteCookies;
      procedure   RetrieveHTML;
      procedure   ExecuteJavaScript(const aCode, aScriptURL : ustring; aStartLine : integer = 0);
      procedure   UpdatePreferences;
      procedure   SavePreferences(const aFileName : string);
      function    SetNewBrowserParent(aNewParentHwnd : HWND) : boolean;

      procedure   ShowDevTools(inspectElementAt: TPoint; const aDevTools : TWinControl);
      procedure   CloseDevTools(const aDevTools : TWinControl = nil);

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
      procedure   Invalidate(kind: TCefPaintElementType);
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
      property  FrameIsFocused          : boolean                      read GetFrameIsFocused;
      property  Initialized             : boolean                      read GetInitialized;
      property  CookiePrefs             : integer                      read FCookiePrefs              write SetCookiePrefs;
      property  ImagesPrefs             : integer                      read FImagesPrefs              write SetImagesPrefs;
      property  CMStoragePath           : ustring                      read FCMStoragePath            write SetCMStoragePath;
      property  DocumentURL             : string                       read GetDocumentURL;
      property  WindowName              : string                       read FWindowName               write FWindowName;
      property  ZoomLevel               : double                       read GetZoomLevel              write SetZoomLevel;
      property  ZoomPct                 : double                       read GetZoomPct                write SetZoomPct;
      property  ZoomStep                : byte                         read FZoomStep                 write SetZoomStep;
      property  WindowlessFrameRate     : integer                      read GetWindowlessFrameRate    write SetWindowlessFrameRate;
      property  CustomHeaderName        : string                       read FCustomHeaderName         write SetCustomHeaderName;
      property  CustomHeaderValue       : string                       read FCustomHeaderValue        write SetCustomHeaderValue;
      property  DoNotTrack              : boolean                      read FDoNotTrack               write SetDoNotTrack;
      property  SendReferrer            : boolean                      read FSendReferrer             write SetSendReferrer;
      property  HyperlinkAuditing       : boolean                      read FHyperlinkAuditing        write SetHyperlinkAuditing;
      property  HasValidMainFrame       : boolean                      read GetHasValidMainFrame;
      property  DragOperations          : TCefDragOperations           read FDragOperations           write FDragOperations;

      property  ProxyType               : integer                      read FProxyType                write SetProxyType;
      property  ProxyServer             : string                       read FProxyServer              write SetProxyServer;
      property  ProxyPort               : integer                      read FProxyPort                write SetProxyPort;
      property  ProxyUsername           : string                       read FProxyUsername            write SetProxyUsername;
      property  ProxyPassword           : string                       read FProxyPassword            write SetProxyPassword;
      property  ProxyScriptURL          : string                       read FProxyScriptURL           write SetProxyScriptURL;
      property  ProxyByPassList         : string                       read FProxyByPassList          write SetProxyByPassList;

    published
      property  OnTextResultAvailable   : TOnTextResultAvailableEvent  read FOnTextResultAvailable    write FOnTextResultAvailable;
      property  OnPdfPrintFinished      : TOnPdfPrintFinishedEvent     read FOnPdfPrintFinished       write FOnPdfPrintFinished;
      property  OnPrefsAvailable        : TNotifyEvent                 read FOnPrefsAvailable         write FOnPrefsAvailable;
      property  OnCookiesDeleted        : TOnCookiesDeletedEvent       read FOnCookiesDeleted         write FOnCookiesDeleted;

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

      // ICefDownloadHandler
      property OnBeforeDownload                 : TOnBeforeDownload                 read FOnBeforeDownload                 write FOnBeforeDownload;
      property OnDownloadUpdated                : TOnDownloadUpdated                read FOnDownloadUpdated                write FOnDownloadUpdated;

      // ICefGeolocationHandler
      property OnRequestGeolocationPermission   : TOnRequestGeolocationPermission   read FOnRequestGeolocationPermission   write FOnRequestGeolocationPermission;
      property OnCancelGeolocationPermission    : TOnCancelGeolocationPermission    read FOnCancelGeolocationPermission    write FOnCancelGeolocationPermission;

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
      property OnStartDragging                  : TOnStartDragging                  read FOnStartDragging                  write FOnStartDragging;
      property OnUpdateDragCursor               : TOnUpdateDragCursor               read FOnUpdateDragCursor               write FOnUpdateDragCursor;
      property OnScrollOffsetChanged            : TOnScrollOffsetChanged            read FOnScrollOffsetChanged            write FOnScrollOffsetChanged;
      property OnIMECompositionRangeChanged     : TOnIMECompositionRangeChanged     read FOnIMECompositionRangeChanged     write FOnIMECompositionRangeChanged;

      // ICefDragHandler
      property OnDragEnter                      : TOnDragEnter                      read FOnDragEnter                      write FOnDragEnter;
      property OnDraggableRegionsChanged        : TOnDraggableRegionsChanged        read FOnDraggableRegionsChanged        write FOnDraggableRegionsChanged;

      // ICefFindHandler
      property OnFindResult                     : TOnFindResult                     read FOnFindResult                     write FOnFindResult;

  end;

implementation

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils, System.Math,
  {$ELSE}
  SysUtils, Math,
  {$ENDIF}
  uCEFBrowser, uCEFValue, uCEFDictionaryValue, uCEFStringMultimap, uCEFCookieManager, uCEFFrame,
  uCEFApplication, uCEFProcessMessage, uOLEDragAndDrop;

constructor TChromium.Create(AOwner: TComponent);
begin
  FBrowser                := nil;
  FBrowserId              := 0;
  FCompHandle             := 0;
  FClosing                := False;
  FInitialized            := False;
  FIsOSR                  := False;
  FDefaultUrl             := 'about:blank';
  FHandler                := nil;
  FOptions                := nil;
  FFontOptions            := nil;
  FDefaultEncoding        := '';
  FVisitor                := nil;
  FPDFPrintcb             := nil;
  FCookiDeletercb         := nil;
  FPDFPrintOptions        := nil;
  FUpdatePreferences      := False;
  FCustomHeaderName       := '';
  FCustomHeaderValue      := '';
  FPrefsFileName          := '';
  FAddCustomHeader        := False;
  FDoNotTrack             := True;
  FSendReferrer           := True;
  FHyperlinkAuditing      := False;
  FCookiePrefs            := CEF_CONTENT_SETTING_ALLOW;
  FImagesPrefs            := CEF_CONTENT_SETTING_ALLOW;
  FZoomStep               := ZOOM_STEP_DEF;
  FWindowName             := '';
  FDragOperations         := DRAG_OPERATION_NONE;
  FDragDropManager        := nil;
  FDropTargetCtrl         := nil;
  FDragAndDropInitialized := False;

  FProxyType         := CEF_PROXYTYPE_DIRECT;
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

destructor TChromium.Destroy;
begin
  try
    try
      if (FDragDropManager <> nil) then FreeAndNil(FDragDropManager);

      if (FCompHandle <> 0) then
        begin
          DeallocateHWnd(FCompHandle);
          FCompHandle := 0;
        end;

      FBrowser        := nil;
      FBrowserId      := 0;
      FHandler        := nil;
      FVisitor        := nil;
      FPDFPrintcb     := nil;
      FCookiDeletercb := nil;

      if (FFontOptions     <> nil) then FreeAndNil(FFontOptions);
      if (FOptions         <> nil) then FreeAndNil(FOptions);
      if (FPDFPrintOptions <> nil) then FreeAndNil(FPDFPrintOptions);
    except
      on e : exception do
        if CustomExceptionHandler('TChromium.Destroy', e) then raise;
    end;
  finally
    inherited Destroy;
  end;
end;

procedure TChromium.AfterConstruction;
begin
  inherited AfterConstruction;

  try
    if not(csDesigning in ComponentState) then
      begin
        FCompHandle      := AllocateHWnd(WndProc);
        FOptions         := TChromiumOptions.Create;
        FFontOptions     := TChromiumFontOptions.Create;
        FPDFPrintOptions := TPDFPrintOptions.Create;
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TChromium.AfterConstruction', e) then raise;
  end;
end;

function TChromium.CreateClientHandler(aIsOSR : boolean) : boolean;
begin
  Result := False;

  try
    if (FHandler = nil) then
      begin
        FIsOSR   := aIsOsr;
        FHandler := TVCLClientHandler.Create(Self, FIsOSR);
        Result   := True;
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TChromium.CreateClientHandler', e) then raise;
  end;
end;

function TChromium.CreateClientHandler(var aClient : ICefClient) : boolean;
begin
  if CreateClientHandler(True) then
    begin
      aClient := FHandler;
      Result  := True;
    end
   else
    Result := False;
end;

procedure TChromium.InitializeEvents;
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

  // ICefDownloadHandler
  FOnBeforeDownload               := nil;
  FOnDownloadUpdated              := nil;

  // ICefGeolocationHandler
  FOnRequestGeolocationPermission := nil;
  FOnCancelGeolocationPermission  := nil;

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
  FOnStartDragging                := nil;
  FOnUpdateDragCursor             := nil;
  FOnScrollOffsetChanged          := nil;
  FOnIMECompositionRangeChanged   := nil;

  // ICefDragHandler
  FOnDragEnter                    := nil;
  FOnDraggableRegionsChanged      := nil;

  // ICefFindHandler
  FOnFindResult                   := nil;

  // Custom
  FOnTextResultAvailable          := nil;
  FOnPdfPrintFinished             := nil;
  FOnPrefsAvailable               := nil;
  FOnCookiesDeleted               := nil;
end;

function TChromium.CreateBrowser(const aBrowserParent : TWinControl; const aWindowName : string) : boolean;
var
  TempHandle : HWND;
  TempRect : TRect;
begin
  if (aBrowserParent <> nil) then
    begin
      TempHandle := aBrowserParent.Handle;
      TempRect   := aBrowserParent.ClientRect;
    end
   else
    begin
      TempHandle := 0;
      TempRect   := rect(0, 0, 0, 0);
    end;

  Result := CreateBrowser(TempHandle, TempRect, aWindowName);
end;

function TChromium.CreateBrowser(aParentHandle : HWND; aParentRect : TRect; const aWindowName : string = '') : boolean;
begin
  Result := False;

  try
    if not(csDesigning in ComponentState) and
       not(FClosing)         and
       (FBrowser     =  nil) and
       (FBrowserId   =  0)   and
       (GlobalCEFApp <> nil) and
       CreateClientHandler(aParentHandle = 0) then
      begin
        GetSettings(FBrowserSettings);

        if FIsOSR then
          WindowInfoAsWindowless(FWindowInfo, FCompHandle, aWindowName)
         else
          WindowInfoAsChild(FWindowInfo, aParentHandle, aParentRect, aWindowName);


        if MultithreadApp then
          Result := CreateBrowserHost(@FWindowInfo, FDefaultUrl, @FBrowserSettings, nil)
         else
          begin
            FBrowser := CreateBrowserHostSync(@FWindowInfo, FDefaultUrl, @FBrowserSettings, nil);

            if (FBrowser <> nil) then
              begin
                FBrowserId   := FBrowser.Identifier;
                FInitialized := (FBrowserId <> 0);
                Result       := True;
              end;
          end;
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TChromium.CreateBrowser', e) then raise;
  end;
end;

procedure TChromium.InitializeDragAndDrop(const aDropTargetCtrl : TWinControl);
var
  TempDropTarget : IDropTarget;
begin
  if FIsOSR and
     not(FDragAndDropInitialized) and
     (FDragDropManager = nil) and
     (aDropTargetCtrl <> nil) then
    begin
      FDropTargetCtrl                 := aDropTargetCtrl;

      FDragDropManager                := TCEFDragAndDropMgr.Create;
      FDragDropManager.OnDragEnter    := DragDropManager_OnDragEnter;
      FDragDropManager.OnDragOver     := DragDropManager_OnDragOver;
      FDragDropManager.OnDragLeave    := DragDropManager_OnDragLeave;
      FDragDropManager.OnDrop         := DragDropManager_OnDrop;

      TempDropTarget                  := TOLEDropTarget.Create(FDragDropManager);

      RegisterDragDrop(FDropTargetCtrl.Handle, TempDropTarget);

      FDragAndDropInitialized := True;
    end;
end;

procedure TChromium.ShutdownDragAndDrop;
begin
  if FDragAndDropInitialized and (FDropTargetCtrl <> nil) then
    begin
      RevokeDragDrop(FDropTargetCtrl.Handle);
      FDragAndDropInitialized := False;
    end;
end;

procedure TChromium.ToMouseEvent(grfKeyState : Longint; pt : TPoint; var aMouseEvent : TCefMouseEvent);
begin
  if (FDropTargetCtrl <> nil) then
    begin
      pt                    := FDropTargetCtrl.ScreenToClient(pt);
      aMouseEvent.x         := pt.x;
      aMouseEvent.y         := pt.y;
      aMouseEvent.modifiers := GetCefMouseModifiers(grfKeyState);
    end;
end;

procedure TChromium.DragDropManager_OnDragEnter(Sender: TObject; const aDragData : ICefDragData; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint);
var
  TempMouseEvent : TCefMouseEvent;
  TempAllowedOps : TCefDragOperations;
begin
  if (GlobalCEFApp <> nil) then
    begin
      ToMouseEvent(grfKeyState, pt, TempMouseEvent);
      DropEffectToDragOperation(dwEffect, TempAllowedOps);
      DeviceToLogical(TempMouseEvent, GlobalCEFApp.DeviceScaleFactor);

      DragTargetDragEnter(aDragData, @TempMouseEvent, TempAllowedOps);
      DragTargetDragOver(@TempMouseEvent, TempAllowedOps);

      DragOperationToDropEffect(FDragOperations, dwEffect);
    end;
end;

procedure TChromium.DragDropManager_OnDragOver(Sender: TObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint);
var

  TempMouseEvent : TCefMouseEvent;
  TempAllowedOps : TCefDragOperations;

begin

  if (GlobalCEFApp <> nil) then

    begin

      ToMouseEvent(grfKeyState, pt, TempMouseEvent);
      DropEffectToDragOperation(dwEffect, TempAllowedOps);
      DeviceToLogical(TempMouseEvent, GlobalCEFApp.DeviceScaleFactor);

      DragTargetDragOver(@TempMouseEvent, TempAllowedOps);

      DragOperationToDropEffect(FDragOperations, dwEffect);
    end;
end;

procedure TChromium.DragDropManager_OnDragLeave(Sender: TObject);
begin

  DragTargetDragLeave;
end;

procedure TChromium.DragDropManager_OnDrop(Sender: TObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint);
var

  TempMouseEvent : TCefMouseEvent;
  TempAllowedOps : TCefDragOperations;

begin

  if (GlobalCEFApp <> nil) then

    begin

      ToMouseEvent(grfKeyState, pt, TempMouseEvent);
      DropEffectToDragOperation(dwEffect, TempAllowedOps);
      DeviceToLogical(TempMouseEvent, GlobalCEFApp.DeviceScaleFactor);

      DragTargetDragOver(@TempMouseEvent, TempAllowedOps);
      DragTargetDrop(@TempMouseEvent);

      DragOperationToDropEffect(FDragOperations, dwEffect);
    end;
end;

procedure TChromium.CloseBrowser(aForceClose : boolean);
begin
  if Initialized then FBrowser.Host.CloseBrowser(aForceClose);
end;

function TChromium.CreateBrowserHost(aWindowInfo     : PCefWindowInfo;
                                     const aURL      : ustring;
                                     const aSettings : PCefBrowserSettings;
                                     const aContext  : ICefRequestContext): Boolean;
var
  TempURL : TCefString;
begin
  TempURL := CefString(aURL);
  Result  := cef_browser_host_create_browser(aWindowInfo, FHandler.Wrap, @TempURL, aSettings, CefGetData(aContext)) <> 0;
end;

function TChromium.CreateBrowserHostSync(aWindowInfo     : PCefWindowInfo;
                                         const aURL      : ustring;
                                         const aSettings : PCefBrowserSettings;
                                         const aContext  : ICefRequestContext): ICefBrowser;
var
  TempURL     : TCefString;
  TempBrowser : PCefBrowser;
begin
  TempURL     := CefString(aURL);
  TempBrowser := cef_browser_host_create_browser_sync(aWindowInfo, FHandler.Wrap, @TempURL, aSettings, CefGetData(aContext));
  Result      := TCefBrowserRef.UnWrap(TempBrowser);
end;

procedure TChromium.Find(aIdentifier : integer; const aSearchText : ustring; aForward, aMatchCase, aFindNext : Boolean);
begin
  if Initialized then FBrowser.Host.Find(aIdentifier, aSearchText, aForward, aMatchCase, aFindNext);
end;

procedure TChromium.StopFinding(aClearSelection : Boolean);
begin
  if Initialized then FBrowser.Host.StopFinding(aClearSelection);
end;

procedure TChromium.Print;
begin
  if Initialized then FBrowser.Host.Print;
end;

procedure TChromium.PrintToPDF(const aFilePath, aTitle, aURL : ustring);
var
  TempSettings : TCefPdfPrintSettings;
begin
  if Initialized then
    begin
      GetPrintPDFSettings(TempSettings, aTitle, aURL);
      if (FPDFPrintcb = nil) then FPDFPrintcb := TCefCustomPDFPrintCallBack.Create(self);
      FBrowser.Host.PrintToPdf(aFilePath, @TempSettings, FPDFPrintcb);
    end;
end;

procedure TChromium.ClipboardCopy;
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

procedure TChromium.ClipboardPaste;
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

procedure TChromium.ClipboardCut;
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

procedure TChromium.ClipboardUndo;
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

procedure TChromium.ClipboardRedo;
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

procedure TChromium.ClipboardDel;
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

procedure TChromium.SelectAll;
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

procedure TChromium.GetPrintPDFSettings(var aSettings : TCefPdfPrintSettings; const aTitle, aURL : string);
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
      aSettings.header_footer_enabled := FPDFPrintOptions.header_footer_enabled;
      aSettings.selection_only        := FPDFPrintOptions.selection_only;
      aSettings.landscape             := FPDFPrintOptions.landscape;
      aSettings.backgrounds_enabled   := FPDFPrintOptions.backgrounds_enabled;
    end;
end;

procedure TChromium.GetSettings(var aSettings : TCefBrowserSettings);
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

procedure TChromium.InitializeSettings(var aSettings : TCefBrowserSettings);
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

procedure TChromium.LoadURL(const aURL : ustring);
var
  TempFrame : ICefFrame;
begin
  if Initialized then
    begin
      TempFrame := FBrowser.MainFrame;
      if (TempFrame <> nil) then TempFrame.LoadUrl(aURL);
    end;
end;

procedure TChromium.LoadString(const aString : ustring; const aURL : ustring);
var
  TempFrame : ICefFrame;
begin
  if Initialized then
    begin
      TempFrame := FBrowser.MainFrame;
      if (TempFrame <> nil) then TempFrame.LoadString(aString, aURL);
    end;
end;

procedure TChromium.LoadRequest(const aRequest: ICefRequest);
var
  TempFrame : ICefFrame;
begin
  if Initialized then
    begin
      TempFrame := FBrowser.MainFrame;
      if (TempFrame <> nil) then TempFrame.LoadRequest(aRequest);
    end;
end;

procedure TChromium.GoBack;
begin
  if Initialized and CanGoBack then FBrowser.GoBack;
end;

procedure TChromium.GoForward;
begin
  if Initialized and CanGoForward then FBrowser.GoForward;
end;

procedure TChromium.Reload;
begin
  if Initialized then FBrowser.Reload;
end;

procedure TChromium.ReloadIgnoreCache;
begin
  if Initialized then FBrowser.ReloadIgnoreCache;
end;

procedure TChromium.StopLoad;
begin
  if Initialized then FBrowser.StopLoad;
end;

procedure TChromium.StartDownload(const aURL : ustring);
begin
  if Initialized then FBrowser.Host.StartDownload(aURL);
end;

function TChromium.GetIsLoading : boolean;
begin
  Result := Initialized and FBrowser.IsLoading;
end;

function TChromium.GetMultithreadApp : boolean;
begin
  Result := True;

  try
    if (GlobalCEFApp <> nil) then Result := GlobalCEFApp.MultiThreadedMessageLoop;
  except
    on e : exception do
      if CustomExceptionHandler('TChromium.GetMultithreadApp', e) then raise;
  end;
end;

function TChromium.GetHasDocument : boolean;
begin
  Result := Initialized and FBrowser.HasDocument;
end;

function TChromium.GetHasView : boolean;
begin
  Result := Initialized and FBrowser.Host.HasView;
end;

function TChromium.GetHasDevTools : boolean;
begin
  Result := Initialized and FBrowser.Host.HasDevTools;
end;

function TChromium.GetHasClientHandler : boolean;
begin
  Result := (FHandler <> nil);
end;

function TChromium.GetHasBrowser : boolean;
begin
  Result := (FBrowser <> nil);
end;

function TChromium.GetWindowHandle : THandle;
begin
  if Initialized then
    Result := FBrowser.Host.WindowHandle
   else
    Result := 0;
end;

function TChromium.GetFrameIsFocused : boolean;
begin
  Result := Initialized and (FBrowser.FocusedFrame <> nil);
end;

function TChromium.GetWindowlessFrameRate : integer;
begin
  if Initialized then
    Result := FBrowser.Host.GetWindowlessFrameRate
   else
    Result := 0;
end;

function TChromium.GetVisibleNavigationEntry : ICefNavigationEntry;
begin
  if Initialized then
    Result := FBrowser.Host.VisibleNavigationEntry
   else
    Result := nil;
end;

function TChromium.GetHasValidMainFrame : boolean;
begin
  Result := Initialized and (FBrowser.MainFrame <> nil) and FBrowser.MainFrame.IsValid;
end;

procedure TChromium.SetWindowlessFrameRate(aValue : integer);
begin
  if Initialized then FBrowser.Host.SetWindowlessFrameRate(aValue);
end;

function TChromium.GetCanGoBack : boolean;
begin
  Result := Initialized and FBrowser.CanGoBack;
end;

function TChromium.GetCanGoForward : boolean;
begin
  Result := Initialized and FBrowser.CanGoForward;
end;

function TChromium.GetIsPopUp : boolean;
begin
  Result := Initialized and FBrowser.IsPopUp;
end;

function TChromium.GetInitialized : boolean;
begin
  Result := FInitialized and not(FClosing) and (FBrowser <> nil);
end;

function TChromium.GetDocumentURL : string;
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

function TChromium.GetZoomLevel : double;
begin
  Result := 0;

  if Initialized then Result := FBrowser.Host.ZoomLevel;
end;

procedure TChromium.SetZoomLevel(const aValue : double);
begin
  if Initialized then FBrowser.Host.ZoomLevel := aValue;
end;

function TChromium.GetZoomPct : double;
begin
  Result := power(1.2, ZoomLevel) * 100;
end;

procedure TChromium.SetZoomPct(const aValue : double);
begin
  if Initialized and (aValue > 0) then ZoomLevel := LogN(1.2, aValue / 100);
end;

procedure TChromium.ApplyZoomStep;
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

procedure TChromium.SetZoomStep(aValue : byte);
begin
  if Initialized and (aValue in [ZOOM_STEP_MIN..ZOOM_STEP_MAX]) then
    begin
      FZoomStep := aValue;
      ApplyZoomStep;
    end;
end;

procedure TChromium.IncZoomStep;
begin
  if Initialized and (FZoomStep < ZOOM_STEP_MAX) then
    begin
      inc(FZoomStep);
      ApplyZoomStep;
    end;
end;

procedure TChromium.DecZoomStep;
begin
  if Initialized and (FZoomStep > ZOOM_STEP_MIN) then
    begin
      dec(FZoomStep);
      ApplyZoomStep;
    end;
end;

procedure TChromium.ResetZoomStep;
begin
  ZoomStep := ZOOM_STEP_DEF;
end;

procedure TChromium.SetDoNotTrack(aValue : boolean);
begin
  if (FDoNotTrack <> aValue) then
    begin
      FDoNotTrack        := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TChromium.SetSendReferrer(aValue : boolean);
begin
  if (FSendReferrer <> aValue) then
    begin
      FSendReferrer      := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TChromium.SetHyperlinkAuditing(aValue : boolean);
begin
  if (FHyperlinkAuditing <> aValue) then
    begin
      FHyperlinkAuditing := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TChromium.SetCookiePrefs(aValue : integer);
begin
  if (FCookiePrefs <> aValue) then
    begin
      FCookiePrefs       := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TChromium.SetImagesPrefs(aValue : integer);
begin
  if (FImagesPrefs <> aValue) then
    begin
      FImagesPrefs       := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TChromium.SetProxyType(aValue : integer);
begin
  if (FProxyType <> aValue) then
    begin
      FProxyType         := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TChromium.SetProxyServer(const aValue : string);
begin
  if (FProxyServer <> aValue) then
    begin
      FProxyServer       := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TChromium.SetProxyPort(aValue : integer);
begin
  if (FProxyPort <> aValue) then
    begin
      FProxyPort         := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TChromium.SetProxyUsername(const aValue : string);
begin
  if (FProxyUsername <> aValue) then
    begin
      FProxyUsername     := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TChromium.SetProxyPassword(const aValue : string);
begin
  if (FProxyPassword <> aValue) then
    begin
      FProxyPassword     := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TChromium.SetProxyScriptURL(const aValue : string);
begin
  if (FProxyScriptURL <> aValue) then
    begin
      FProxyScriptURL    := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TChromium.SetProxyByPassList(const aValue : string);
begin
  if (FProxyByPassList <> aValue) then
    begin
      FProxyByPassList   := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TChromium.SetCustomHeaderName(const aValue : string);
begin
  if (FCustomHeaderName <> aValue) then
    begin
      FCustomHeaderName := aValue;
      FAddCustomHeader  := (length(FCustomHeaderName) > 0) and (length(FCustomHeaderValue) > 0);
    end;
end;

procedure TChromium.SetCustomHeaderValue(const aValue : string);
begin
  if (FCustomHeaderValue <> aValue) then
    begin
      FCustomHeaderValue := aValue;
      FAddCustomHeader   := (length(FCustomHeaderName) > 0) and (length(FCustomHeaderValue) > 0);
    end;
end;

procedure TChromium.SetCMStoragePath(const aValue : ustring);
var
  CookieManager : ICefCookieManager;
begin
  FCMStoragePath := aValue;
  CookieManager  := TCefCookieManagerRef.Global(nil);

  if (CookieManager <> nil) then
    CookieManager.SetStoragePath(FCMStoragePath, False, nil);
end;

procedure TChromium.Internal_GetHTML;
var
  TempFrame : ICefFrame;
begin
  if Initialized then
    begin
      TempFrame := FBrowser.MainFrame;
      if (TempFrame <> nil) then
        begin
          if (FVisitor = nil) then FVisitor := TCustomCefStringVisitor.Create(self);
          TempFrame.GetSource(FVisitor);
        end;
    end;
end;

procedure TChromium.DeleteCookies;
var
  TempTask: ICefTask;
begin
  if Initialized then
    begin
      if (FCookiDeletercb = nil) then FCookiDeletercb := TCefCustomDeleteCookiesCallback.Create(self);
      TempTask := TCefDeleteCookiesTask.Create(FCookiDeletercb);
      CefPostTask(TID_IO, TempTask);
    end;
end;

procedure TChromium.RetrieveHTML;
var
  TempTask: ICefTask;
begin
  if Initialized then
    begin
      TempTask := TCefGetHTMLTask.Create(self);
      CefPostTask(TID_UI, TempTask);
    end;
end;

procedure TChromium.UpdatePreferences;
var
  TempTask: ICefTask;
begin
  if Initialized then
    begin
      TempTask := TCefUpdatePrefsTask.Create(self);
      CefPostTask(TID_UI, TempTask);
    end;
end;

procedure TChromium.SavePreferences(const aFileName : string);
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

function TChromium.SetNewBrowserParent(aNewParentHwnd : HWND) : boolean;
var
  TempHandle : HWND;
begin
  Result := False;

  if Initialized then
    begin
      TempHandle := FBrowser.Host.WindowHandle;
      Result     := (TempHandle <> 0) and (SetParent(TempHandle, aNewParentHwnd) <> 0);
    end;
end;

procedure TChromium.SimulateMouseWheel(aDeltaX, aDeltaY : integer);
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

procedure TChromium.Internal_UpdatePreferences;
begin
  FUpdatePreferences := False;

  UpdateProxyPrefs;
  UpdatePreference('enable_do_not_track', FDoNotTrack);
  UpdatePreference('enable_referrers',    FSendReferrer);
  UpdatePreference('enable_a_ping',       FHyperlinkAuditing);
end;

function TChromium.UpdateProxyPrefs : boolean;
var
  TempError : ustring;
  TempProxy : ICefValue;
  TempValue : ICefValue;
  TempDict  : ICefDictionaryValue;
begin
  Result := False;

  try
    if (FBrowser <> nil) and FBrowser.Host.RequestContext.CanSetPreference('proxy') then
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
              TempDict.SetString('server', FProxyServer + ':' + inttostr(FProxyPort));
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
                  FBrowser.Host.RequestContext.SetPreference('proxy', TempProxy, TempError);

        if not(Result) then
          OutputDebugMessage('TChromium.UpdateProxyPrefs error : ' + quotedstr(TempError));
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TChromium.UpdateProxyPrefs', e) then raise;
  end;
end;

function TChromium.UpdatePreference(const aName : string; aValue : boolean) : boolean;
var
  TempError : ustring;
  TempValue : ICefValue;
begin
  Result := False;

  try
    if (FBrowser <> nil) and FBrowser.Host.RequestContext.CanSetPreference(aName) then
      begin
        TempValue := TCefValueRef.New;

        if aValue then
          TempValue.SetBool(1)
         else
          TempValue.SetBool(0);

        Result := FBrowser.Host.RequestContext.SetPreference(aName, TempValue, TempError);

        if not(Result) then
          OutputDebugMessage('TChromium.UpdatePreference error : ' + quotedstr(TempError));
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TChromium.UpdatePreference', e) then raise;
  end;
end;

function TChromium.UpdatePreference(const aName : string; aValue : integer) : boolean;
var
  TempError : ustring;
  TempValue : ICefValue;
begin
  Result := False;

  try
    if (FBrowser <> nil) and FBrowser.Host.RequestContext.CanSetPreference(aName) then
      begin
        TempValue := TCefValueRef.New;
        TempValue.SetInt(aValue);
        Result := FBrowser.Host.RequestContext.SetPreference(aName, TempValue, TempError);

        if not(Result) then
          OutputDebugMessage('TChromium.UpdatePreference error : ' + quotedstr(TempError));
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TChromium.UpdatePreference', e) then raise;
  end;
end;

function TChromium.UpdatePreference(const aName : string; const aValue : double) : boolean;
var
  TempError : ustring;
  TempValue : ICefValue;
begin
  Result := False;

  try
    if (FBrowser <> nil) and FBrowser.Host.RequestContext.CanSetPreference(aName) then
      begin
        TempValue := TCefValueRef.New;
        TempValue.SetDouble(aValue);
        Result := FBrowser.Host.RequestContext.SetPreference(aName, TempValue, TempError);

        if not(Result) then
          OutputDebugMessage('TChromium.UpdatePreference error : ' + quotedstr(TempError));
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TChromium.UpdatePreference', e) then raise;
  end;
end;

function TChromium.UpdatePreference(const aName, aValue : string) : boolean;
var
  TempError : ustring;
  TempValue : ICefValue;
begin
  Result := False;

  try
    if (FBrowser <> nil) and FBrowser.Host.RequestContext.CanSetPreference(aName) then
      begin
        TempValue := TCefValueRef.New;
        TempValue.SetString(aValue);
        Result := FBrowser.Host.RequestContext.SetPreference(aName, TempValue, TempError);

        if not(Result) then
          OutputDebugMessage('TChromium.UpdatePreference error : ' + quotedstr(TempError));
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TChromium.UpdatePreference', e) then raise;
  end;
end;

procedure TChromium.HandleNull(const aValue : ICefValue; var aResultSL : TStringList; const aRoot, aKey : string);
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

procedure TChromium.HandleBool(const aValue : ICefValue; var aResultSL : TStringList; const aRoot, aKey : string);
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

procedure TChromium.HandleInteger(const aValue : ICefValue; var aResultSL : TStringList; const aRoot, aKey : string);
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

procedure TChromium.HandleDouble(const aValue : ICefValue; var aResultSL : TStringList; const aRoot, aKey : string);
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

procedure TChromium.HandleString(const aValue : ICefValue; var aResultSL : TStringList; const aRoot, aKey : string);
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

procedure TChromium.HandleBinary(const aValue : ICefValue; var aResultSL : TStringList; const aRoot, aKey : string);
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

procedure TChromium.HandleList(const aValue : ICefValue; var aResultSL : TStringList; const aRoot, aKey : string);
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
        VTYPE_BOOL       : TempResult := TempResult + BoolToStr(aValue.GetBool, true) + ',';
        VTYPE_INT        : TempResult := TempResult + IntToStr(aValue.GetInt) + ',';
        VTYPE_DOUBLE     : TempResult := TempResult + FloatToStr(aValue.GetDouble) + ',';
        VTYPE_STRING     : TempResult := TempResult + aValue.GetString + ',';
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

procedure TChromium.HandleInvalid(const aValue : ICefValue; var aResultSL : TStringList; const aRoot, aKey : string);
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

procedure TChromium.HandleDictionary(const aDict : ICefDictionaryValue; var aResultSL : TStringList; const aRoot : string);
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
        if CustomExceptionHandler('TChromium.HandleDictionary', e) then raise;
    end;
  finally
    if (TempKeys <> nil) then TempKeys.Free;
  end;
end;

procedure TChromium.Internal_SavePreferences;
var
  TempDict  : ICefDictionaryValue;
  TempPrefs : TStringList;
begin
  TempPrefs := nil;

  try
    try
      if Initialized then
        begin
          TempPrefs := TStringList.Create;
          TempDict  := FBrowser.Host.RequestContext.GetAllPreferences(True);
          HandleDictionary(TempDict, TempPrefs, '');
          TempPrefs.SaveToFile(FPrefsFileName);
          SendCompMessage(CEF_PREFERENCES_SAVED);
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TChromium.Internal_SavePreferences', e) then raise;
    end;
  finally
    if (TempPrefs <> nil) then FreeAndNil(TempPrefs);
  end;
end;

procedure TChromium.PrefsAvailableMsg(var aMessage : TMessage);
begin
  if assigned(FOnPrefsAvailable) then FOnPrefsAvailable(self);
end;

function TChromium.SendCompMessage(aMsg : cardinal; wParam : cardinal; lParam : integer) : boolean;
begin
  Result := (FCompHandle <> 0) and PostMessage(FCompHandle, aMsg, wParam, lParam);
end;

procedure TChromium.Internal_TextResultAvailable(const aText : string);
begin
  if assigned(FOnTextResultAvailable) then FOnTextResultAvailable(self, aText);
end;

procedure TChromium.ExecuteJavaScript(const aCode, aScriptURL : ustring; aStartLine : integer);
var
  TempFrame : ICefFrame;
begin
  try
    if Initialized then
      begin
        TempFrame := FBrowser.MainFrame;

        if (TempFrame <> nil) then
          TempFrame.ExecuteJavaScript(aCode, aScriptURL, aStartLine);
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TChromium.ExecuteJavaScript', e) then raise;
  end;
end;

procedure TChromium.Internal_CookiesDeleted(numDeleted : integer);
begin
  if assigned(FOnCookiesDeleted) then FOnCookiesDeleted(self, numDeleted);
end;

procedure TChromium.Internal_PdfPrintFinished(aResultOK : boolean);
begin
  if assigned(FOnPdfPrintFinished) then FOnPdfPrintFinished(self, aResultOK);
end;

procedure TChromium.ShowDevTools(inspectElementAt: TPoint; const aDevTools : TWinControl);
var
  TempPoint : TCefPoint;
begin
  if not(Initialized) or HasDevTools then Exit;

  InitializeSettings(FDevBrowserSettings);

  if (aDevTools <> nil) then
    WindowInfoAsChild(FDevWindowInfo, aDevTools.Handle, aDevTools.ClientRect, aDevTools.Name)
   else
    WindowInfoAsPopUp(FDevWindowInfo, WindowHandle, DEVTOOLS_WINDOWNAME);


  if (inspectElementAt.x <> low(integer)) and
     (inspectElementAt.y <> low(integer)) then
    begin
      TempPoint.x := inspectElementAt.x;
      TempPoint.y := inspectElementAt.y;

      FBrowser.Host.ShowDevTools(@FDevWindowInfo, TCefClientOwn.Create as ICefClient, @FDevBrowserSettings, @TempPoint);
    end
   else
    FBrowser.Host.ShowDevTools(@FDevWindowInfo, TCefClientOwn.Create as ICefClient, @FDevBrowserSettings, nil);
end;

procedure TChromium.CloseDevTools(const aDevTools : TWinControl);
begin
  if Initialized then
    begin
      if (aDevTools <> nil) then
        begin
          {$IFDEF DELPHI16_UP}
          WinApi.Windows.SetParent(GetWindow(aDevTools.Handle, GW_CHILD), 0);
          {$ELSE}
          Windows.SetParent(GetWindow(aDevTools.Handle, GW_CHILD), 0);
          {$ENDIF}
        end;

      if (FBrowser <> nil) then FBrowser.Host.CloseDevTools;
    end;
end;

procedure TChromium.WndProc(var aMessage: TMessage);
begin
  case aMessage.Msg of
    CEF_PREFERENCES_SAVED : PrefsAvailableMsg(aMessage);
    CEF_STARTDRAGGING     : DelayedDragging;

    else aMessage.Result := DefWindowProc(FCompHandle, aMessage.Msg, aMessage.WParam, aMessage.LParam);
  end;
end;

function TChromium.doOnClose(const browser: ICefBrowser): Boolean;
begin
  Result := False;

  if (browser <> nil) and (FBrowserId = browser.Identifier) then FClosing := True;

  if Assigned(FOnClose) then FOnClose(Self, browser, Result);
end;

procedure TChromium.doOnBeforeClose(const browser: ICefBrowser);
begin
  if (browser <> nil) and (FBrowserId = browser.Identifier) then
    begin
      FInitialized := False;
      FBrowser     := nil;
      FBrowserId   := 0;
      FHandler     := nil;
    end;

  if Assigned(FOnBeforeClose) then FOnBeforeClose(Self, browser);
end;

procedure TChromium.doOnAddressChange(const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
begin
  if Assigned(FOnAddressChange) then FOnAddressChange(Self, browser, frame, url);
end;

procedure TChromium.doOnAfterCreated(const browser: ICefBrowser);
begin
  if MultithreadApp and (FBrowser = nil) then
    begin
      FBrowser := browser;
      if (FBrowser <> nil) then FBrowserId := FBrowser.Identifier;
    end;

  Internal_UpdatePreferences;

  FInitialized := (FBrowser <> nil) and (FBrowserId <> 0);

  if Assigned(FOnAfterCreated) then FOnAfterCreated(Self, browser);
end;

function TChromium.doOnBeforeBrowse(const browser    : ICefBrowser;
                                    const frame      : ICefFrame;
                                    const request    : ICefRequest;
                                          isRedirect : Boolean): Boolean;
begin
  Result := False;

  if FUpdatePreferences then Internal_UpdatePreferences;

  if Assigned(FOnBeforeBrowse) then FOnBeforeBrowse(Self, browser, frame, request, isRedirect, Result);
end;

procedure TChromium.doOnBeforeContextMenu(const browser : ICefBrowser;
                                          const frame   : ICefFrame;
                                          const params  : ICefContextMenuParams;
                                          const model   : ICefMenuModel);
begin
  if Assigned(FOnBeforeContextMenu) then FOnBeforeContextMenu(Self, browser, frame, params, model);
end;

procedure TChromium.doOnBeforeDownload(const browser       : ICefBrowser;
                                       const downloadItem  : ICefDownloadItem;
                                       const suggestedName : ustring;
                                       const callback      : ICefBeforeDownloadCallback);
begin
  if Assigned(FOnBeforeDownload) then FOnBeforeDownload(Self, browser, downloadItem, suggestedName, callback);
end;

function TChromium.doOnBeforePopup(const browser            : ICefBrowser;
                                   const frame              : ICefFrame;
                                   const targetUrl          : ustring;
                                   const targetFrameName    : ustring;
                                         targetDisposition  : TCefWindowOpenDisposition;
                                         userGesture        : Boolean;
                                   var   popupFeatures      : TCefPopupFeatures;
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

function TChromium.doOnBeforeResourceLoad(const browser  : ICefBrowser;
                                          const frame    : ICefFrame;
                                          const request  : ICefRequest;
                                          const callback : ICefRequestCallback): TCefReturnValue;
var
  TempHeaderMap : ICefStringMultimap;
begin
  if FAddCustomHeader then
    begin
      TempHeaderMap := TCefStringMultimapOwn.Create;
      request.GetHeaderMap(TempHeaderMap);
      TempHeaderMap.Append(FCustomHeaderName, FCustomHeaderValue);
      request.SetHeaderMap(TempHeaderMap);
      TempHeaderMap := nil;
    end;

  if not(FSendReferrer) then request.SetReferrer('', REFERRER_POLICY_NEVER);

  Result := RV_CONTINUE;

  if Assigned(FOnBeforeResourceLoad) then FOnBeforeResourceLoad(Self, browser, frame, request, callback, Result);
end;

function TChromium.doOnBeforeUnloadDialog(const browser     : ICefBrowser;
                                          const messageText : ustring;
                                                isReload    : Boolean;
                                          const callback    : ICefJsDialogCallback): Boolean;
begin
  Result := False;

  if Assigned(FOnBeforeUnloadDialog) then FOnBeforeUnloadDialog(Self, browser, messageText, isReload, callback, Result);
end;

procedure TChromium.doOnCancelGeolocationPermission(const browser : ICefBrowser; requestId : Integer);
begin
  if Assigned(FOnCancelGeolocationPermission) then
    FOnCancelGeolocationPermission(Self, browser, requestId);
end;

function TChromium.doOnCertificateError(const browser    : ICefBrowser;
                                              certError  : TCefErrorcode;
                                        const requestUrl : ustring;
                                        const sslInfo    : ICefSslInfo;
                                        const callback   : ICefRequestCallback): Boolean;
begin
  Result := False;

  if Assigned(FOnCertificateError) then
    FOnCertificateError(Self, browser, certError, requestUrl, sslInfo, callback, Result);
end;

function TChromium.doOnConsoleMessage(const browser  : ICefBrowser;
                                      const aMessage : ustring;
                                      const source   : ustring;
                                            line     : Integer): Boolean;
begin
  Result := False;

  if Assigned(FOnConsoleMessage) then FOnConsoleMessage(Self, browser, aMessage, source, line, Result);
end;

function TChromium.doOnContextMenuCommand(const browser    : ICefBrowser;
                                          const frame      : ICefFrame;
                                          const params     : ICefContextMenuParams;
                                                commandId  : Integer;
                                                eventFlags : TCefEventFlags): Boolean;
begin
  Result := False;

  if Assigned(FOnContextMenuCommand) then
    FOnContextMenuCommand(Self, browser, frame, params, commandId, eventFlags, Result);
end;

procedure TChromium.doOnContextMenuDismissed(const browser: ICefBrowser; const frame: ICefFrame);
begin
  if Assigned(FOnContextMenuDismissed) then FOnContextMenuDismissed(Self, browser, frame);
end;

procedure TChromium.doOnCursorChange(const browser          : ICefBrowser;
                                           cursor           : TCefCursorHandle;
                                           cursorType       : TCefCursorType;
                                     const customCursorInfo : PCefCursorInfo);
begin
  if assigned(FOnCursorChange) then FOnCursorChange(self, browser, cursor, cursorType, customCursorInfo);
end;

procedure TChromium.doOnDialogClosed(const browser: ICefBrowser);
begin
  if Assigned(FOnDialogClosed) then FOnDialogClosed(Self, browser);
end;

procedure TChromium.doOnDownloadUpdated(const browser      : ICefBrowser;
                                        const downloadItem : ICefDownloadItem;
                                        const callback     : ICefDownloadItemCallback);
begin
  if Assigned(FOnDownloadUpdated) then FOnDownloadUpdated(Self, browser, downloadItem, callback);
end;

function TChromium.doOnDragEnter(const browser  : ICefBrowser;
                                 const dragData : ICefDragData;
                                       mask     : TCefDragOperations): Boolean;
begin
  Result := False;

  if Assigned(FOnDragEnter) then FOnDragEnter(Self, browser, dragData, mask, Result);
end;

procedure TChromium.doOnDraggableRegionsChanged(const browser      : ICefBrowser;
                                                      regionsCount : NativeUInt;
                                                      regions      : PCefDraggableRegionArray);
begin
  if Assigned(FOnDraggableRegionsChanged) then FOnDraggableRegionsChanged(Self, browser, regionsCount, regions);
end;

procedure TChromium.doOnFaviconUrlChange(const browser: ICefBrowser; iconUrls: TStrings);
begin
  if Assigned(FOnFavIconUrlChange) then FOnFavIconUrlChange(Self, browser, iconUrls);
end;

function TChromium.doOnFileDialog(const browser              : ICefBrowser;
                                        mode                 : TCefFileDialogMode;
                                  const title                : ustring;
                                  const defaultFilePath      : ustring;
                                        acceptFilters        : TStrings;
                                        selectedAcceptFilter : Integer;
                                  const callback             : ICefFileDialogCallback): Boolean;
begin
  Result := False;

  if Assigned(FOnFileDialog) then
    FOnFileDialog(Self, browser, mode, title, defaultFilePath, acceptFilters,
                  selectedAcceptFilter, callback, Result);
end;

procedure TChromium.doOnFindResult(const browser            : ICefBrowser;
                                         identifier         : integer;
                                         count              : Integer;
                                   const selectionRect      : PCefRect;
                                         activeMatchOrdinal : Integer;
                                         finalUpdate        : Boolean);
begin
  if Assigned(FOnFindResult) then
    FOnFindResult(Self, browser, identifier, count, selectionRect, activeMatchOrdinal, finalUpdate);
end;

procedure TChromium.doOnFullScreenModeChange(const browser: ICefBrowser; fullscreen: Boolean);
begin
  if Assigned(FOnFullScreenModeChange) then FOnFullScreenModeChange(Self, browser, fullscreen);
end;

function TChromium.doOnGetAuthCredentials(const browser  : ICefBrowser;
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

function TChromium.doOnGetResourceHandler(const browser : ICefBrowser;
                                          const frame   : ICefFrame;
                                          const request : ICefRequest): ICefResourceHandler;
begin
  Result := nil;

  if Assigned(FOnGetResourceHandler) then
    FOnGetResourceHandler(Self, browser, frame, request, Result);
end;

procedure TChromium.doOnGetAccessibilityHandler(var aAccessibilityHandler : ICefAccessibilityHandler);
begin
  if assigned(FOnGetAccessibilityHandler) then FOnGetAccessibilityHandler(Self, aAccessibilityHandler);
end;

function TChromium.doOnGetRootScreenRect(const browser: ICefBrowser; var rect: TCefRect): Boolean;
begin
  Result := False;

  if Assigned(FOnGetRootScreenRect) then FOnGetRootScreenRect(Self, browser, rect, Result);
end;

function TChromium.doOnGetScreenInfo(const browser: ICefBrowser; var screenInfo: TCefScreenInfo): Boolean;
begin
  Result := False;

  if Assigned(FOnGetScreenInfo) then FOnGetScreenInfo(Self, browser, screenInfo, Result);
end;

function TChromium.doOnGetScreenPoint(const browser: ICefBrowser; viewX, viewY: Integer; var screenX, screenY: Integer): Boolean;
begin
  Result := False;

  if Assigned(FOnGetScreenPoint) then FOnGetScreenPoint(Self, browser, viewX, viewY, screenX, screenY, Result);
end;

function TChromium.doOnGetViewRect(const browser: ICefBrowser; var rect: TCefRect): Boolean;
begin
  Result := False;

  if Assigned(FOnGetViewRect) then FOnGetViewRect(Self, browser, rect, Result);
end;

procedure TChromium.doOnGotFocus(const browser: ICefBrowser);
begin
  if Assigned(FOnGotFocus) then FOnGotFocus(Self, browser)
end;

function TChromium.doOnJsdialog(const browser           : ICefBrowser;
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
    if Assigned(FOnJsdialog) then
      FOnJsdialog(Self, browser, originUrl, dialogType, messageText,
                  defaultPromptText, callback, suppressMessage, Result);
end;

function TChromium.doOnKeyEvent(const browser : ICefBrowser;
                                const event   : PCefKeyEvent;
                                      osEvent : TCefEventHandle): Boolean;
begin
  Result := False;

  if Assigned(FOnKeyEvent) then FOnKeyEvent(Self, browser, event, osEvent, Result);
end;

procedure TChromium.doOnLoadEnd(const browser        : ICefBrowser;
                                const frame          : ICefFrame;
                                      httpStatusCode : Integer);
begin
  if Assigned(FOnLoadEnd) then FOnLoadEnd(Self, browser, frame, httpStatusCode);
end;

procedure TChromium.doOnLoadError(const browser   : ICefBrowser;
                                  const frame     : ICefFrame;
                                        errorCode : Integer;
                                  const errorText : ustring;
                                  const failedUrl : ustring);
begin
  if Assigned(FOnLoadError) then FOnLoadError(Self, browser, frame, errorCode, errorText, failedUrl);
end;

procedure TChromium.doOnLoadingStateChange(const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
begin
  if Assigned(FOnLoadingStateChange) then FOnLoadingStateChange(Self, browser, isLoading, canGoBack, canGoForward);
end;

procedure TChromium.doOnLoadStart(const browser: ICefBrowser; const frame: ICefFrame; transitionType: TCefTransitionType);
begin
  if Assigned(FOnLoadStart) then FOnLoadStart(Self, browser, frame, transitionType);
end;

function TChromium.doOnOpenUrlFromTab(const browser           : ICefBrowser;
                                      const frame             : ICefFrame;
                                      const targetUrl         : ustring;
                                            targetDisposition : TCefWindowOpenDisposition;
                                            userGesture       : Boolean): Boolean;
begin
  Result := False;

  if Assigned(FOnOpenUrlFromTab) then
    FOnOpenUrlFromTab(Self, browser, frame, targetUrl, targetDisposition, userGesture, Result);
end;

procedure TChromium.doOnPaint(const browser         : ICefBrowser;
                                    kind            : TCefPaintElementType;
                                    dirtyRectsCount : NativeUInt;
                              const dirtyRects      : PCefRectArray;
                              const buffer          : Pointer;
                                    width           : Integer;
                                    height          : Integer);
begin
  if Assigned(FOnPaint) then FOnPaint(Self, browser, kind, dirtyRectsCount, dirtyRects, buffer, width, height);
end;

function TChromium.doOnSelectClientCertificate(const browser           : ICefBrowser;
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

procedure TChromium.doOnPluginCrashed(const browser: ICefBrowser; const pluginPath: ustring);
begin
  if Assigned(FOnPluginCrashed) then FOnPluginCrashed(Self, browser, pluginPath);
end;

procedure TChromium.doOnPopupShow(const browser: ICefBrowser; show: Boolean);
begin
  if assigned(FOnPopupShow) then FOnPopupShow(self, browser, show);
end;

procedure TChromium.doOnPopupSize(const browser: ICefBrowser; const rect: PCefRect);
begin
  if assigned(FOnPopupSize) then FOnPopupSize(self, browser, rect);
end;

function TChromium.doOnPreKeyEvent(const browser            : ICefBrowser;
                                   const event              : PCefKeyEvent;
                                         osEvent            : TCefEventHandle;
                                   out   isKeyboardShortcut : Boolean): Boolean;
begin
  Result := False;

  if Assigned(FOnPreKeyEvent) then FOnPreKeyEvent(Self, browser, event, osEvent, isKeyboardShortcut, Result);
end;

function TChromium.doOnProcessMessageReceived(const browser       : ICefBrowser;
                                                    sourceProcess : TCefProcessId;
                                              const aMessage      : ICefProcessMessage): Boolean;
begin
  Result := False;

  if Assigned(FOnProcessMessageReceived) then
    FOnProcessMessageReceived(Self, browser, sourceProcess, aMessage, Result);
end;

procedure TChromium.doOnProtocolExecution(const browser          : ICefBrowser;
                                          const url              : ustring;
                                          out   allowOsExecution : Boolean);
begin
  if Assigned(FOnProtocolExecution) then FOnProtocolExecution(Self, browser, url, allowOsExecution);
end;

function TChromium.doOnQuotaRequest(const browser   : ICefBrowser;
                                    const originUrl : ustring;
                                          newSize   : Int64;
                                    const callback  : ICefRequestCallback): Boolean;
begin
  Result := False;

  if Assigned(FOnQuotaRequest) then FOnQuotaRequest(Self, browser, originUrl, newSize, callback, Result);
end;

procedure TChromium.doOnRenderProcessTerminated(const browser: ICefBrowser; status: TCefTerminationStatus);
begin
  if Assigned(FOnRenderProcessTerminated) then FOnRenderProcessTerminated(Self, browser, status);
end;

procedure TChromium.doOnRenderViewReady(const browser: ICefBrowser);
begin
  if Assigned(FOnRenderViewReady) then FOnRenderViewReady(Self, browser);
end;

function TChromium.doOnRequestGeolocationPermission(const browser       : ICefBrowser;
                                                    const requestingUrl : ustring;
                                                          requestId     : Integer;
                                                    const callback      : ICefGeolocationCallback): Boolean;
begin
  Result := False;

  if Assigned(FOnRequestGeolocationPermission) then
    FOnRequestGeolocationPermission(Self, browser, requestingUrl, requestId, callback, Result);
end;

procedure TChromium.doOnResetDialogState(const browser: ICefBrowser);
begin
  if Assigned(FOnResetDialogState) then FOnResetDialogState(Self, browser);
end;

procedure TChromium.doOnResourceRedirect(const browser  : ICefBrowser;
                                         const frame    : ICefFrame;
                                         const request  : ICefRequest;
                                         const response : ICefResponse;
                                         var   newUrl   : ustring);
begin
  if Assigned(FOnResourceRedirect) then FOnResourceRedirect(Self, browser, frame, request, response, newUrl);
end;

function TChromium.doOnResourceResponse(const browser  : ICefBrowser;
                                        const frame    : ICefFrame;
                                        const request  : ICefRequest;
                                        const response : ICefResponse): Boolean;
begin
  Result := False;

  if Assigned(FOnResourceResponse) then FOnResourceResponse(Self, browser, frame, request, response, Result);
end;
function TChromium.doOnGetResourceResponseFilter(const browser  : ICefBrowser;
                                                 const frame    : ICefFrame;
                                                 const request  : ICefRequest;
                                                 const response : ICefResponse) : ICefResponseFilter;
begin
  Result := nil;

  if Assigned(FOnGetResourceResponseFilter) then
    FOnGetResourceResponseFilter(self, browser, frame, request, response, Result);
end;

procedure TChromium.doOnResourceLoadComplete(const browser               : ICefBrowser;
                                             const frame                 : ICefFrame;
                                             const request               : ICefRequest;
                                             const response              : ICefResponse;
                                                   status                : TCefUrlRequestStatus;
                                                   receivedContentLength : Int64);
begin
  if Assigned(FOnResourceLoadComplete) then
    FOnResourceLoadComplete(self, browser, frame, request, response, status, receivedContentLength);
end;

procedure TChromium.doOnScrollOffsetChanged(const browser: ICefBrowser; x, y: Double);
begin
  if Assigned(FOnScrollOffsetChanged) then FOnScrollOffsetChanged(Self, browser, x, y);
end;

procedure TChromium.doOnIMECompositionRangeChanged(const browser               : ICefBrowser;
                                                   const selected_range        : PCefRange;
                                                         character_boundsCount : NativeUInt;
                                                   const character_bounds      : PCefRect);
begin
  if assigned(FOnIMECompositionRangeChanged) then
    FOnIMECompositionRangeChanged(self, browser, selected_range, character_boundsCount, character_bounds);
end;

function TChromium.doOnSetFocus(const browser: ICefBrowser; source: TCefFocusSource): Boolean;
begin
  Result := False;

  if Assigned(FOnSetFocus) then FOnSetFocus(Self, browser, source, Result);
end;

function TChromium.doOnStartDragging(const browser    : ICefBrowser;
                                     const dragData   : ICefDragData;
                                           allowedOps : TCefDragOperations;
                                           x          : integer;
                                           y          : Integer): Boolean;
begin
  Result := False;

  if FDragAndDropInitialized and
     FDragDropManager.CloneDragData(dragData, allowedOps) then
    begin
      Result := True;
      SendCompMessage(CEF_STARTDRAGGING);
    end;

  if Assigned(FOnStartDragging) then FOnStartDragging(Self, browser, dragData, allowedOps, x, y, Result);
end;

procedure TChromium.DelayedDragging;
var
  TempOperation : TCefDragOperation;
  TempPoint     : TPoint;
begin
  if FDragAndDropInitialized and (FDropTargetCtrl <> nil) and (GlobalCEFApp <> nil) then
    begin
      FDragOperations := DRAG_OPERATION_NONE;
      TempOperation   := FDragDropManager.StartDragging;
      FDragOperations := DRAG_OPERATION_NONE;

      GetCursorPos(TempPoint);
      TempPoint := FDropTargetCtrl.ScreenToClient(TempPoint);
      DeviceToLogical(TempPoint, GlobalCEFApp.DeviceScaleFactor);

      DragSourceEndedAt(TempPoint.x, TempPoint.y, TempOperation);
      DragSourceSystemDragEnded;
    end;
end;

procedure TChromium.doOnStatusMessage(const browser: ICefBrowser; const value: ustring);
begin
  if Assigned(FOnStatusMessage) then FOnStatusMessage(Self, browser, value);
end;

procedure TChromium.doOnTakeFocus(const browser: ICefBrowser; next: Boolean);
begin
  if Assigned(FOnTakeFocus) then FOnTakeFocus(Self, browser, next);
end;

procedure TChromium.doOnTitleChange(const browser: ICefBrowser; const title: ustring);
begin
  if Assigned(FOnTitleChange) then FOnTitleChange(Self, browser, title);
end;

function TChromium.doOnTooltip(const browser: ICefBrowser; var text: ustring): Boolean;
begin
  Result := False;

  if Assigned(FOnTooltip) then FOnTooltip(Self, browser, text, Result);
end;

procedure TChromium.doOnUpdateDragCursor(const browser: ICefBrowser; operation: TCefDragOperation);
begin
  if FDragAndDropInitialized then FDragOperations := operation;

  if Assigned(FOnUpdateDragCursor) then FOnUpdateDragCursor(Self, browser, operation);
end;

function TChromium.GetParentForm : TCustomForm;
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

procedure TChromium.MoveFormTo(const x, y: Integer);
var
  TempForm : TCustomForm;
  TempRect : TRect;
begin
  TempForm := GetParentForm;

  if (TempForm <> nil) then
    begin
      TempRect.Left   := min(max(x, max(screen.DesktopLeft, 0)), screen.DesktopWidth  - TempForm.Width);
      TempRect.Top    := min(max(y, max(screen.DesktopTop,  0)), screen.DesktopHeight - TempForm.Height);
      TempRect.Right  := TempRect.Left + TempForm.Width  - 1;
      TempRect.Bottom := TempRect.Top  + TempForm.Height - 1;

      TempForm.SetBounds(TempRect.Left, TempRect.Top, TempRect.Right - TempRect.Left + 1, TempRect.Bottom - TempRect.Top + 1);
    end;
end;

procedure TChromium.MoveFormBy(const x, y: Integer);
var
  TempForm : TCustomForm;
  TempRect : TRect;
begin
  TempForm := GetParentForm;

  if (TempForm <> nil) then
    begin
      TempRect.Left   := min(max(TempForm.Left + x, max(screen.DesktopLeft, 0)), screen.DesktopWidth  - TempForm.Width);
      TempRect.Top    := min(max(TempForm.Top  + y, max(screen.DesktopTop,  0)), screen.DesktopHeight - TempForm.Height);
      TempRect.Right  := TempRect.Left + TempForm.Width  - 1;
      TempRect.Bottom := TempRect.Top  + TempForm.Height - 1;

      TempForm.SetBounds(TempRect.Left, TempRect.Top, TempRect.Right - TempRect.Left + 1, TempRect.Bottom - TempRect.Top + 1);
    end;
end;

procedure TChromium.ResizeFormWidthTo(const x : Integer);
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

procedure TChromium.ResizeFormHeightTo(const y : Integer);
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

procedure TChromium.SetFormLeftTo(const x : Integer);
var
  TempForm : TCustomForm;
begin
  TempForm := GetParentForm;

  if (TempForm <> nil) then
    TempForm.Left := min(max(x, max(screen.DesktopLeft, 0)), screen.DesktopWidth  - TempForm.Width);
end;

procedure TChromium.SetFormTopTo(const y : Integer);
var
  TempForm : TCustomForm;
begin
  TempForm := GetParentForm;

  if (TempForm <> nil) then
    TempForm.Top := min(max(y, max(screen.DesktopTop, 0)), screen.DesktopHeight - TempForm.Height);
end;

procedure TChromium.WasResized;
begin
  if Initialized then FBrowser.Host.WasResized;
end;

procedure TChromium.WasHidden(hidden: Boolean);
begin
  if Initialized then FBrowser.Host.WasHidden(hidden);
end;

procedure TChromium.NotifyScreenInfoChanged;
begin
  if Initialized then FBrowser.Host.NotifyScreenInfoChanged;
end;

procedure TChromium.NotifyMoveOrResizeStarted;
begin
  if Initialized then FBrowser.Host.NotifyMoveOrResizeStarted;
end;

procedure TChromium.Invalidate(kind: TCefPaintElementType);
begin
  if Initialized then FBrowser.Host.Invalidate(kind);
end;

procedure TChromium.SendKeyEvent(const event: PCefKeyEvent);
begin
  if Initialized then FBrowser.Host.SendKeyEvent(event);
end;

procedure TChromium.SendMouseClickEvent(const event      : PCefMouseEvent;
                                              kind       : TCefMouseButtonType;
                                              mouseUp    : Boolean;
                                              clickCount : Integer);
begin
  if Initialized then FBrowser.Host.SendMouseClickEvent(event, kind, mouseUp, clickCount);
end;

procedure TChromium.SendMouseMoveEvent(const event: PCefMouseEvent; mouseLeave: Boolean);
begin
  if Initialized then FBrowser.Host.SendMouseMoveEvent(event, mouseLeave);
end;

procedure TChromium.SendMouseWheelEvent(const event: PCefMouseEvent; deltaX, deltaY: Integer);
begin
  if Initialized then FBrowser.Host.SendMouseWheelEvent(event, deltaX, deltaY);
end;

procedure TChromium.SendFocusEvent(setFocus: Boolean);
begin
  if Initialized then FBrowser.Host.SendFocusEvent(setFocus);
end;

procedure TChromium.SendCaptureLostEvent;
begin
  if Initialized then FBrowser.Host.SendCaptureLostEvent;
end;

procedure TChromium.SetFocus(focus: Boolean);
begin
  if Initialized then FBrowser.Host.SetFocus(focus);
end;

procedure TChromium.SetAccessibilityState(accessibilityState: TCefState);
begin
  if Initialized then FBrowser.Host.SetAccessibilityState(accessibilityState);
end;

function TChromium.SendProcessMessage(targetProcess: TCefProcessId; const ProcMessage: ICefProcessMessage): Boolean;
begin
  Result := Initialized and FBrowser.SendProcessMessage(targetProcess, ProcMessage);
end;

procedure TChromium.DragTargetDragEnter(const dragData: ICefDragData; const event: PCefMouseEvent; allowedOps: TCefDragOperations);
begin
  if Initialized then FBrowser.Host.DragTargetDragEnter(dragData, event, allowedOps);
end;

procedure TChromium.DragTargetDragOver(const event: PCefMouseEvent; allowedOps: TCefDragOperations);
begin
  if Initialized then FBrowser.Host.DragTargetDragOver(event, allowedOps);
end;

procedure TChromium.DragTargetDragLeave;
begin
  if Initialized then FBrowser.Host.DragTargetDragLeave;
end;

procedure TChromium.DragTargetDrop(event: PCefMouseEvent);
begin
  if Initialized then FBrowser.Host.DragTargetDrop(event);
end;

procedure TChromium.DragSourceEndedAt(x, y: Integer; op: TCefDragOperation);
begin
  if Initialized then FBrowser.Host.DragSourceEndedAt(x, y, op);
end;

procedure TChromium.DragSourceSystemDragEnded;
begin
  if Initialized then FBrowser.Host.DragSourceSystemDragEnded;
end;

end.
