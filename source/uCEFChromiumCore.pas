unit uCEFChromiumCore;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
    {$IFDEF MSWINDOWS}WinApi.Windows, WinApi.Messages, WinApi.ActiveX, WinApi.CommCtrl,{$ENDIF}
    System.Classes, System.SyncObjs, System.Types,
  {$ELSE}
    {$IFDEF MSWINDOWS}Windows, ActiveX, CommCtrl,{$ENDIF} Classes,
    {$IFDEF FPC}
      LCLProc, LCLType, LCLIntf, LResources, LMessages, InterfaceBase,
      {$IFDEF LINUX}xlib,{$ENDIF}
    {$ELSE}
      Messages,
    {$ENDIF}
    SyncObjs,
  {$ENDIF}
  uCEFTypes, uCEFInterfaces, uCEFLibFunctions, uCEFMiscFunctions, uCEFClient,
  uCEFConstants, uCEFTask, uCEFDomVisitor, uCEFChromiumEvents,
  {$IFDEF MSWINDOWS}uCEFDragAndDropMgr,{$ENDIF}
  {$IFDEF LINUX}uCEFLinuxTypes, uCEFLinuxFunctions,{$ENDIF}
  uCEFChromiumOptions, uCEFChromiumFontOptions, uCEFPDFPrintOptions,
  uCEFBrowserViewComponent;

type
  TBrowserInfoList = class;

  /// <summary>
  ///  Parent class of TChromium and TFMXChromium that puts together all browser procedures, functions, properties and events in one place.
  ///  It has all you need to create, modify and destroy a web browser.
  /// </summary>
  TChromiumCore = class(TComponent, IChromiumEvents)
    protected
      {$IFDEF MSWINDOWS}
      FCompHandle               : HWND;
      {$ENDIF}
      FHandler                  : ICefClient;
      FBrowsersCS               : TCriticalSection;
      FBrowsers                 : TBrowserInfoList;
      FBrowserId                : integer;
      FMultiBrowserMode         : boolean;
      FReqContextHandler        : ICefRequestContextHandler;
      FResourceRequestHandler   : ICefResourceRequestHandler;
      FMediaObserver            : ICefMediaObserver;
      FMediaObserverReg         : ICefRegistration;
      FDevToolsMsgObserver      : ICefDevToolsMessageObserver;
      FDevToolsMsgObserverReg   : ICefRegistration;
      FExtensionHandler         : ICefExtensionHandler;
      FDefaultUrl               : ustring;
      FOptions                  : TChromiumOptions;
      FFontOptions              : TChromiumFontOptions;
      FPDFPrintOptions          : TPDFPrintOptions;
      FDefaultEncoding          : ustring;
      FProxyType                : integer;
      FProxyScheme              : TCefProxyScheme;
      FProxyServer              : ustring;
      FProxyPort                : integer;
      FProxyUsername            : ustring;
      FProxyPassword            : ustring;
      FProxyScriptURL           : ustring;
      FProxyByPassList          : ustring;
      FMaxConnectionsPerProxy   : integer;
      FUpdatePreferences        : boolean;
      FCustomHeaderName         : ustring;
      FCustomHeaderValue        : ustring;
      FAddCustomHeader          : boolean;
      FDoNotTrack               : boolean;
      FSendReferrer             : boolean;
      FHyperlinkAuditing        : boolean;
      FAllowOutdatedPlugins     : boolean;
      FAlwaysAuthorizePlugins   : boolean;
      FAlwaysOpenPDFExternally  : boolean;
      FSpellChecking            : boolean;
      FSpellCheckerDicts        : ustring;
      FZoomStep                 : byte;
      FZoomStepCS               : TCriticalSection;
      FPrefsFileName            : string;
      FIsOSR                    : boolean;
      FSafeSearch               : boolean;
      FOffline                  : boolean;
      FYouTubeRestrict          : integer;
      FPrintingEnabled          : boolean;
      FWindowInfo               : TCefWindowInfo;
      FBrowserSettings          : TCefBrowserSettings;
      FDevWindowInfo            : TCefWindowInfo;
      FDevBrowserSettings       : TCefBrowserSettings;
      FDragOperations           : TCefDragOperations;
      {$IFDEF MSWINDOWS}
      FDragDropManager          : TCEFDragAndDropMgr;
      FDropTargetWnd            : HWND;
      {$ENDIF}
      FDragAndDropInitialized   : boolean;
      FWebRTCIPHandlingPolicy   : TCefWebRTCHandlingPolicy;
      FWebRTCMultipleRoutes     : TCefState;
      FWebRTCNonProxiedUDP      : TCefState;
      FAcceptLanguageList       : ustring;
      FAcceptCookies            : TCefCookiePref;
      FBlock3rdPartyCookies     : boolean;
      FDefaultWindowInfoExStyle : DWORD;
      FQuicAllowed              : boolean;
      FJavascriptEnabled        : boolean;
      FLoadImagesAutomatically  : boolean;
      FBatterySaverModeState    : TCefBatterySaverModeState;
      FHighEfficiencyModeState  : TCefHighEfficiencyModeState;
      FCanFocus                 : boolean;
      FEnableFocusDelayMs       : cardinal;

      {$IFDEF LINUX}
      FXDisplay                 : PXDisplay;
      {$ENDIF}

      {$IFDEF MSWINDOWS}
      FOldBrowserCompWndPrc   : TFNWndProc;
      FOldWidgetCompWndPrc    : TFNWndProc;
      FOldRenderCompWndPrc    : TFNWndProc;
      FBrowserCompStub        : Pointer;
      FWidgetCompStub         : Pointer;
      FRenderCompStub         : Pointer;
      FBrowserCompHWND        : THandle;
      FWidgetCompHWND         : THandle;
      FRenderCompHWND         : THandle;
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
      FOnRunQuickMenu                 : TOnRunQuickMenuEvent;
      FOnQuickMenuCommand             : TOnQuickMenuCommandEvent;
      FOnQuickMenuDismissed           : TOnQuickMenuDismissedEvent;

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
      FOnCursorChange                 : TOnCursorChange;
      FOnMediaAccessChange            : TOnMediaAccessChange;

      // ICefDownloadHandler
      FOnCanDownload                  : TOnCanDownloadEvent;
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
      FOnCertificateError                  : TOnCertificateError;
      FOnSelectClientCertificate           : TOnSelectClientCertificate;
      FOnRenderViewReady                   : TOnRenderViewReady;
      FOnRenderProcessTerminated           : TOnRenderProcessTerminated;
      FOnGetResourceRequestHandler_ReqHdlr : TOnGetResourceRequestHandler;
      FOnDocumentAvailableInMainFrame      : TOnDocumentAvailableInMainFrame;

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
      FOnGetTouchHandleSize           : TOnGetTouchHandleSize;
      FOnTouchHandleStateChanged      : TOnTouchHandleStateChanged;
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
      FOnGetResourceRequestHandler_ReqCtxHdlr  : TOnGetResourceRequestHandler;

      // ICefMediaObserver
      FOnSinks                            : TOnSinksEvent;
      FOnRoutes                           : TOnRoutesEvent;
      FOnRouteStateChanged                : TOnRouteStateChangedEvent;
      FOnRouteMessageReceived             : TOnRouteMessageReceivedEvent;

      // ICefAudioHandler
      FOnGetAudioParameters               : TOnGetAudioParametersEvent;
      FOnAudioStreamStarted               : TOnAudioStreamStartedEvent;
      FOnAudioStreamPacket                : TOnAudioStreamPacketEvent;
      FOnAudioStreamStopped               : TOnAudioStreamStoppedEvent;
      FOnAudioStreamError                 : TOnAudioStreamErrorEvent;

      // ICefDevToolsMessageObserver
      FOnDevToolsMessage                  : TOnDevToolsMessageEvent;
      FOnDevToolsRawMessage               : TOnDevToolsRawMessageEvent;
      FOnDevToolsMethodResult             : TOnDevToolsMethodResultEvent;
      FOnDevToolsMethodRawResult          : TOnDevToolsMethodRawResultEvent;
      FOnDevToolsEvent                    : TOnDevToolsEventEvent;
      FOnDevToolsRawEvent                 : TOnDevToolsEventRawEvent;
      FOnDevToolsAgentAttached            : TOnDevToolsAgentAttachedEvent;
      FOnDevToolsAgentDetached            : TOnDevToolsAgentDetachedEvent;

      // ICefExtensionHandler
      FOnExtensionLoadFailed              : TOnExtensionLoadFailedEvent;
      FOnExtensionLoaded                  : TOnExtensionLoadedEvent;
      FOnExtensionUnloaded                : TOnExtensionUnloadedEvent;
      FOnExtensionBeforeBackgroundBrowser : TOnBeforeBackgroundBrowserEvent;
      FOnExtensionBeforeBrowser           : TOnBeforeBrowserEvent;
      FOnExtensionGetActiveBrowser        : TOnGetActiveBrowserEvent;
      FOnExtensionCanAccessBrowser        : TOnCanAccessBrowserEvent;
      FOnExtensionGetExtensionResource    : TOnGetExtensionResourceEvent;

      // ICefPrintHandler
      FOnPrintStart                       : TOnPrintStartEvent;
      FOnPrintSettings                    : TOnPrintSettingsEvent;
      FOnPrintDialog                      : TOnPrintDialogEvent;
      FOnPrintJob                         : TOnPrintJobEvent;
      FOnPrintReset                       : TOnPrintResetEvent;
      FOnGetPDFPaperSize                  : TOnGetPDFPaperSizeEvent;

      // ICefFrameHandler
      FOnFrameCreated                     : TOnFrameCreated;
      FOnFrameAttached                    : TOnFrameAttached;
      FOnFrameDetached                    : TOnFrameDetached;
      FOnMainFrameChanged                 : TOnMainFrameChanged;

      // ICefCommandHandler
      FOnChromeCommand                    : TOnChromeCommandEvent;
      FOnIsChromeAppMenuItemVisible       : TOnIsChromeAppMenuItemVisibleEvent;
      FOnIsChromeAppMenuItemEnabled       : TOnIsChromeAppMenuItemEnabledEvent;
      FOnIsChromePageActionIconVisible    : TOnIsChromePageActionIconVisibleEvent;
      FOnIsChromeToolbarButtonVisible     : TOnIsChromeToolbarButtonVisibleEvent;


      // ICefPermissionHandler
      FOnRequestMediaAccessPermission     : TOnRequestMediaAccessPermissionEvent;
      FOnShowPermissionPrompt             : TOnShowPermissionPromptEvent;
      FOnDismissPermissionPrompt          : TOnDismissPermissionPromptEvent;

      // Custom
      FOnTextResultAvailable              : TOnTextResultAvailableEvent;
      FOnPdfPrintFinished                 : TOnPdfPrintFinishedEvent;
      FOnPrefsAvailable                   : TOnPrefsAvailableEvent;
      FOnPrefsUpdated                     : TNotifyEvent;
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
      FOnMediaRouteCreateFinished         : TOnMediaRouteCreateFinishedEvent;
      FOnMediaSinkDeviceInfo              : TOnMediaSinkDeviceInfoEvent;
      FOnCanFocus                         : TNotifyEvent;
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
      function  GetOpenerWindowHandle : TCefWindowHandle;
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
      function  GetRequestContext : ICefRequestContext;
      function  GetMediaRouter : ICefMediaRouter;
      function  GetBrowser : ICefBrowser;
      function  GetBrowserId : integer;
      function  GetBrowserById(aID : integer) : ICefBrowser;
      function  GetBrowserCount : integer;
      function  GetBrowserIdByIndex(aIndex : integer) : integer;
      {$IFDEF LINUX}
      function  GetXDisplay : PXDisplay;
      {$ENDIF}

      procedure SetDoNotTrack(aValue : boolean);
      procedure SetSendReferrer(aValue : boolean);
      procedure SetHyperlinkAuditing(aValue : boolean);
      procedure SetAllowOutdatedPlugins(aValue : boolean);
      procedure SetAlwaysAuthorizePlugins(aValue : boolean);
      procedure SetAlwaysOpenPDFExternally(aValue : boolean);
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
      procedure SetOffline(aValue : boolean);
      procedure SetYouTubeRestrict(aValue : integer);
      procedure SetPrintingEnabled(aValue : boolean);
      procedure SetAcceptLanguageList(const aValue : ustring);
      procedure SetAcceptCookies(const aValue : TCefCookiePref);
      procedure SetBlock3rdPartyCookies(const aValue : boolean);
      procedure SetMultiBrowserMode(aValue : boolean);
      procedure SetQuicAllowed(aValue : boolean);
      procedure SetJavascriptEnabled(aValue : boolean);
      procedure SetLoadImagesAutomatically(aValue : boolean);
      procedure SetBatterySaverModeState(aValue : TCefBatterySaverModeState);
      procedure SetHighEfficiencyModeState(aValue : TCefHighEfficiencyModeState);
      procedure SetDefaultUrl(const aValue : ustring);

      function  CreateBrowserHost(aWindowInfo : PCefWindowInfo; const aURL : ustring; const aSettings : PCefBrowserSettings; const aExtraInfo : ICefDictionaryValue; const aContext : ICefRequestContext): boolean;
      function  CreateBrowserHostSync(aWindowInfo : PCefWindowInfo; const aURL : ustring; const aSettings : PCefBrowserSettings; const aExtraInfo : ICefDictionaryValue; const aContext : ICefRequestContext): Boolean;

      procedure DestroyAllBrowsers;
      procedure DestroyClientHandler;
      procedure DestroyReqContextHandler;
      procedure DestroyResourceRequestHandler;
      procedure DestroyMediaObserver;
      procedure DestroyDevToolsMsgObserver;
      procedure DestroyExtensionHandler;
      procedure DestroyAllHandlersAndObservers;

      procedure CreateResourceRequestHandler; virtual;
      procedure CreateMediaObserver; virtual;
      procedure CreateDevToolsMsgObserver; virtual;
      procedure CreateExtensionHandler; virtual;
      procedure CreateRequestContextHandler; virtual;
      procedure CreateOptionsClasses; virtual;
      procedure CreateSyncObjects; virtual;
      procedure CreateBrowserInfoList; virtual;
      {$IFDEF MSWINDOWS}
      procedure CreateWindowWithWndProc; virtual;
      {$ENDIF}

      procedure InitializeEvents;
      procedure InitializeSettings(var aSettings : TCefBrowserSettings);

      function  UpdateProxyPrefs(const aBrowser: ICefBrowser) : boolean;
      function  UpdatePreference(const aBrowser: ICefBrowser; const aName : ustring; aValue : boolean) : boolean; overload;
      function  UpdatePreference(const aBrowser: ICefBrowser; const aName : ustring; aValue : integer) : boolean; overload;
      function  UpdatePreference(const aBrowser: ICefBrowser; const aName : ustring; const aValue : double) : boolean; overload;
      function  UpdatePreference(const aBrowser: ICefBrowser; const aName, aValue : ustring) : boolean; overload;
      function  UpdatePreference(const aBrowser: ICefBrowser; const aName : ustring; const aValue : TStringList) : boolean; overload;
      function  UpdateStringListPref(const aBrowser: ICefBrowser; const aName, aValue : ustring) : boolean;

      function  ExecuteUpdateZoomStepTask(aInc : boolean) : boolean;
      function  ExecuteUpdateZoomPctTask(aInc : boolean) : boolean;
      function  ExecuteReadZoomTask : boolean;
      function  ExecuteSetZoomPctTask(const aValue : double) : boolean;
      function  ExecuteSetZoomLevelTask(const aValue : double) : boolean;
      function  ExecuteSetZoomStepTask(aValue : byte) : boolean;
      function  ExecuteBrowserNavigationTask(aTask : TCefBrowserNavigation) : boolean;

      procedure UpdateHostZoomLevel(const aValue : double);
      procedure UpdateHostZoomPct(const aValue : double);

      procedure DelayedDragging;
      procedure InitializeWindowInfo(aParentHandle : TCefWindowHandle; aParentRect : TRect; const aWindowName : ustring); virtual;
      procedure DefaultInitializeDevToolsWindowInfo(aDevToolsWnd: TCefWindowHandle; const aClientRect: TRect; const aWindowName: ustring);

      function  AddBrowser(const aBrowser : ICefBrowser) : boolean;
      function  RemoveBrowser(const aBrowser : ICefBrowser) : boolean;
      procedure SetBrowserIsClosing(aID : integer);

      {$IFDEF MSWINDOWS}
      procedure PrefsAvailableMsg(aResultOK : boolean);
      function  SendCompMessage(aMsg : cardinal; aWParam : WPARAM = 0; aLParam : LPARAM = 0) : boolean;
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
      procedure RestoreOldCompWndProc;
      function  CopyDCToBitmapStream(aSrcDC : HDC; const aSrcRect : TRect; const aStream : TStream) : boolean;
      {$ENDIF}

      procedure DragDropManager_OnDragEnter(Sender: TObject; const aDragData : ICefDragData; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint);
      procedure DragDropManager_OnDragOver(Sender: TObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint);
      procedure DragDropManager_OnDragLeave(Sender: TObject);
      procedure DragDropManager_OnDrop(Sender: TObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint);

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
      function  doRunQuickMenu(const browser: ICefBrowser; const frame: ICefFrame; location: PCefPoint; size: PCefSize; edit_state_flags: TCefQuickMenuEditStateFlags; const callback: ICefRunQuickMenuCallback): boolean; virtual;
      function  doOnQuickMenuCommand(const browser: ICefBrowser; const frame: ICefFrame; command_id: integer; event_flags: TCefEventFlags): boolean; virtual;
      procedure doOnQuickMenuDismissed(const browser: ICefBrowser; const frame: ICefFrame); virtual;

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
      procedure doOnCursorChange(const browser: ICefBrowser; cursor_: TCefCursorHandle; cursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo; var aResult : boolean); virtual;
      procedure doOnMediaAccessChange(const browser: ICefBrowser; has_video_access, has_audio_access: boolean); virtual;

      // ICefDownloadHandler
      function  doOnCanDownload(const browser: ICefBrowser; const url, request_method: ustring): boolean;
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
      function  doOnCertificateError(const browser: ICefBrowser; certError: TCefErrorcode; const requestUrl: ustring; const sslInfo: ICefSslInfo; const callback: ICefCallback): Boolean; virtual;
      function  doOnSelectClientCertificate(const browser: ICefBrowser; isProxy: boolean; const host: ustring; port: integer; certificatesCount: NativeUInt; const certificates: TCefX509CertificateArray; const callback: ICefSelectClientCertificateCallback): boolean; virtual;
      procedure doOnRenderViewReady(const browser: ICefBrowser); virtual;
      procedure doOnRenderProcessTerminated(const browser: ICefBrowser; status: TCefTerminationStatus); virtual;
      procedure doOnDocumentAvailableInMainFrame(const browser: ICefBrowser); virtual;

      // ICefResourceRequestHandler
      function  doOnBeforeResourceLoad(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const callback: ICefCallback): TCefReturnValue; virtual;
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
      function  doOnFileDialog(const browser: ICefBrowser; mode: TCefFileDialogMode; const title, defaultFilePath: ustring; const acceptFilters: TStrings; const callback: ICefFileDialogCallback): Boolean; virtual;

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
      procedure doGetTouchHandleSize(const browser: ICefBrowser; orientation: TCefHorizontalAlignment; var size: TCefSize); virtual;
      procedure doOnTouchHandleStateChanged(const browser: ICefBrowser; const state: TCefTouchHandleState); virtual;
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
      procedure doGetResourceRequestHandler_ReqCtxHdlr(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; is_navigation, is_download: boolean; const request_initiator: ustring; var disable_default_handling: boolean; var aResourceRequestHandler : ICefResourceRequestHandler); virtual;

      // ICefMediaObserver
      procedure doOnSinks(const sinks: TCefMediaSinkArray); virtual;
      procedure doOnRoutes(const routes: TCefMediaRouteArray); virtual;
      procedure doOnRouteStateChanged(const route: ICefMediaRoute; state: TCefMediaRouteConnectionState); virtual;
      procedure doOnRouteMessageReceived(const route: ICefMediaRoute; const message_: ustring); virtual;

      // ICefAudioHandler
      procedure doOnGetAudioParameters(const browser: ICefBrowser; var params: TCefAudioParameters; var aResult: boolean); virtual;
      procedure doOnAudioStreamStarted(const browser: ICefBrowser; const params: TCefAudioParameters; channels: integer); virtual;
      procedure doOnAudioStreamPacket(const browser: ICefBrowser; const data : PPSingle; frames: integer; pts: int64); virtual;
      procedure doOnAudioStreamStopped(const browser: ICefBrowser); virtual;
      procedure doOnAudioStreamError(const browser: ICefBrowser; const message_: ustring); virtual;

      // ICefDevToolsMessageObserver
      procedure doOnDevToolsMessage(const browser: ICefBrowser; const message_: Pointer; message_size: NativeUInt; var aHandled: boolean); virtual;
      procedure doOnDevToolsMethodResult(const browser: ICefBrowser; message_id: integer; success: boolean; const result: Pointer; result_size: NativeUInt); virtual;
      procedure doOnDevToolsEvent(const browser: ICefBrowser; const method: ustring; const params: Pointer; params_size: NativeUInt); virtual;
      procedure doOnDevToolsAgentAttached(const browser: ICefBrowser); virtual;
      procedure doOnDevToolsAgentDetached(const browser: ICefBrowser); virtual;

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
      procedure GetSettings(var aSettings : TCefBrowserSettings);
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
      procedure doOnCookiesVisited(const name_, value, domain, path: ustring; secure, httponly, hasExpires: Boolean; const creation, lastAccess, expires: TDateTime; count, total, aID : Integer; same_site : TCefCookieSameSite; priority : TCefCookiePriority; var aDeleteCookie, aResult : Boolean); virtual;
      procedure doOnCookieVisitorDestroyed(aID : integer); virtual;
      procedure doOnCookieSet(aSuccess : boolean; aID : integer); virtual;
      procedure doUpdateZoomStep(aInc : boolean); virtual;
      procedure doUpdateZoomPct(aInc : boolean); virtual;
      procedure doReadZoom; virtual;
      procedure doSetZoomLevel(const aValue : double); virtual;
      procedure doSetZoomPct(const aValue : double); virtual;
      procedure doSetZoomStep(aValue : byte); virtual;
      procedure doMediaRouteCreateFinished(result: TCefMediaRouterCreateResult; const error: ustring; const route: ICefMediaRoute); virtual;
      procedure doOnMediaSinkDeviceInfo(const ip_address: ustring; port: integer; const model_name: ustring); virtual;
      procedure doBrowserNavigation(aTask : TCefBrowserNavigation); virtual;
      procedure doSetAudioMuted(aValue : boolean); virtual;
      procedure doToggleAudioMuted; virtual;
      procedure doEnableFocus; virtual;
      function  MustCreateAudioHandler : boolean; virtual;
      function  MustCreateCommandHandler : boolean; virtual;
      function  MustCreateDevToolsMessageObserver : boolean; virtual;
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
      function  MustCreateMediaObserver : boolean; virtual;
      function  MustCreateExtensionHandler : boolean; virtual;
      function  MustCreatePrintHandler : boolean; virtual;
      function  MustCreateFrameHandler : boolean; virtual;
      function  MustCreatePermissionHandler : boolean; virtual;

      property  ParentFormHandle   : TCefWindowHandle   read   GetParentFormHandle;

    public
      constructor Create(AOwner: TComponent); override;
      destructor  Destroy; override;
      procedure   AfterConstruction; override;
      /// <summary>
      /// Used to create the client handler which will also create most of the browser handlers needed for the browser.
      /// </summary>
      function    CreateClientHandler(aIsOSR : boolean = True) : boolean; overload;
      /// <summary>
      /// Used to create the client handler when a browser requests a new browser in a popup window or tab in the TChromiumCore.OnBeforePopup event.
      /// </summary>
      function    CreateClientHandler(var aClient : ICefClient; aIsOSR : boolean = True) : boolean; overload;
      /// <summary>
      /// Request that the browser close. The JavaScript 'onbeforeunload' event will
      /// be fired. If |aForceClose| is false (0) the event handler, if any, will be
      /// allowed to prompt the user and the user can optionally cancel the close.
      /// If |aForceClose| is true (1) the prompt will not be displayed and the
      /// close will proceed. Results in a call to
      /// ICefLifeSpanHandler.DoClose() if the event handler allows the close
      /// or if |aForceClose| is true (1). See ICefLifeSpanHandler.DoClose()
      /// documentation for additional usage information.
      /// </summary>
      procedure   CloseBrowser(aForceClose : boolean);
      /// <summary>
      /// Calls CloseBrowser for all the browsers handled by this TChromiumCore instance.
      /// </summary>
      procedure   CloseAllBrowsers;
      /// <summary>
      /// Helper for closing a browser. Call this function from the top-level window
      /// close handler (if any). Internally this calls CloseBrowser(false (0)) if
      /// the close has not yet been initiated. This function returns false (0)
      /// while the close is pending and true (1) after the close has completed. See
      /// CloseBrowser() and ICefLifeSpanHandler.DoClose() documentation for
      /// additional usage information. This function must be called on the browser
      /// process UI thread.
      /// </summary>
      function    TryCloseBrowser : boolean;
      /// <summary>
      /// Select the browser with the aID identifier when TChromiumCore uses the
      /// multi-browser mode.
      /// </summary>
      function    SelectBrowser(aID : integer) : boolean;
      /// <summary>
      /// Returns the index in the browsers array of the browser with the aID
      /// identifier when TChromiumCore uses the multi-browser mode.
      /// </summary>
      function    IndexOfBrowserID(aID : integer) : integer;
      /// <summary>
      /// Creates a new request context in the aContext parameter that shares
      /// storage with the request context of the current browser and uses an
      /// optional handler.
      /// </summary>
      function    ShareRequestContext(var aContext : ICefRequestContext; const aHandler : ICefRequestContextHandler = nil) : boolean;
      {$IFDEF MSWINDOWS}
      /// <summary>
      /// Used with browsers in OSR mode to initialize drag and drop in Windows.
      /// </summary>
      procedure   InitializeDragAndDrop(const aDropTargetWnd : HWND);
      /// <summary>
      /// Used with browsers in OSR mode to shutdown drag and drop in Windows.
      /// </summary>
      procedure   ShutdownDragAndDrop;
      /// <summary>
      /// Used to reparent the browser to a different TCEFWindowParent.
      /// </summary>
      function    SetNewBrowserParent(aNewParentHwnd : HWND) : boolean;
      {$ENDIF MSWINDOWS}
      /// <summary>
      /// <para>Used to create the browser after the global request context has been
      /// initialized. You need to set all properties and events before calling
      /// this function because it will only create the internal handlers needed
      /// for those events and the property values will be used in the browser
      /// initialization.</para>
      /// <para>The browser will be fully initialized when the TChromiumCore.OnAfterCreated
      /// event is triggered.</para>
      /// </summary>
      function    CreateBrowser(aParentHandle : TCefWindowHandle; aParentRect : TRect; const aWindowName : ustring = ''; const aContext : ICefRequestContext = nil; const aExtraInfo : ICefDictionaryValue = nil; aForceAsPopup : boolean = False) : boolean; overload; virtual;
      /// <summary>
      /// <para>Used to create the browser after the global request context has been
      /// initialized. You need to set all properties and events before calling
      /// this function because it will only create the internal handlers needed
      /// for those events and the property values will be used in the browser
      /// initialization.</para>
      /// <para>The browser will be fully initialized when the TChromiumCore.OnAfterCreated
      /// event is triggered.</para>
      /// </summary>
      function    CreateBrowser(const aURL : ustring; const aBrowserViewComp : TCEFBrowserViewComponent; const aContext : ICefRequestContext = nil; const aExtraInfo : ICefDictionaryValue = nil) : boolean; overload; virtual;
      /// <summary>
      /// Used to navigate to a URL in the specified frame or the main frame.
      /// </summary>
      procedure   LoadURL(const aURL : ustring; const aFrameName : ustring = ''); overload;
      /// <summary>
      /// Used to navigate to a URL in the specified frame or the main frame.
      /// </summary>
      procedure   LoadURL(const aURL : ustring; const aFrame : ICefFrame); overload;
      /// <summary>
      /// Used to navigate to a URL in the specified frame or the main frame.
      /// </summary>
      procedure   LoadURL(const aURL : ustring; const aFrameIdentifier : int64); overload;
      /// <summary>
      /// Used to load a DATA URI with the HTML string contents in the specified frame or the main frame.
      /// </summary>
      procedure   LoadString(const aHTML : ustring; const aFrameName : ustring = ''); overload;
      /// <summary>
      /// Used to load a DATA URI with the HTML string contents in the specified frame or the main frame.
      /// </summary>
      procedure   LoadString(const aHTML : ustring; const aFrame : ICefFrame); overload;
      /// <summary>
      /// Used to load a DATA URI with the HTML string contents in the specified frame or the main frame.
      /// </summary>
      procedure   LoadString(const aHTML : ustring; const aFrameIdentifier : int64); overload;
      /// <summary>
      /// Used to load a DATA URI with the stream contents in the specified frame or the main frame.
      /// The DATA URI will be configured with the mime type and charset specified in the parameters.
      /// </summary>
      procedure   LoadResource(const aStream : TCustomMemoryStream; const aMimeType, aCharset : string; const aFrameName : ustring = ''); overload;
      /// <summary>
      /// Used to load a DATA URI with the stream contents in the specified frame or the main frame.
      /// The DATA URI will be configured with the mime type and charset specified in the parameters.
      /// </summary>
      procedure   LoadResource(const aStream : TCustomMemoryStream; const aMimeType, aCharset : string; const aFrame : ICefFrame); overload;
      /// <summary>
      /// Used to load a DATA URI with the stream contents in the specified frame or the main frame.
      /// The DATA URI will be configured with the mime type and charset specified in the parameters.
      /// </summary>
      procedure   LoadResource(const aStream : TCustomMemoryStream; const aMimeType, aCharset : string; const aFrameIdentifier : int64); overload;
      /// <summary>
      /// Load the request represented by the aRequest object.
      /// </summary>
      /// <remarks>
      /// WARNING: This function will fail with bad IPC message reason
      /// INVALID_INITIATOR_ORIGIN (213) unless you first navigate to the request
      /// origin using some other mechanism (LoadURL, link click, etc).
      /// </remarks>
      procedure   LoadRequest(const aRequest: ICefRequest);
      /// <summary>
      /// Navigate backwards.
      /// </summary>
      procedure   GoBack;
      /// <summary>
      /// Navigate forwards.
      /// </summary>
      procedure   GoForward;
      /// <summary>
      /// Reload the current page.
      /// </summary>
      procedure   Reload;
      /// <summary>
      /// Reload the current page ignoring any cached data.
      /// </summary>
      procedure   ReloadIgnoreCache;
      /// <summary>
      /// Stop loading the page.
      /// </summary>
      procedure   StopLoad;
      /// <summary>
      /// Starts downloading a file in the specified URL.
      /// </summary>
      procedure   StartDownload(const aURL : ustring);
      /// <summary>
      /// Starts downloading an image in the specified URL.
      /// Use the TChromiumCore.OnDownloadImageFinished event to receive the image.
      /// </summary>
      procedure   DownloadImage(const imageUrl: ustring; isFavicon: Boolean; maxImageSize: cardinal; bypassCache: Boolean);
      /// <summary>
      /// Calls ICefBrowserHost.SendMouseWheelEvent to simulate a simple mouse wheel event.
      /// Use TChromiumCore.SendMouseWheelEvent if you need to specify the mouse coordinates or the event flags.
      /// </summary>
      procedure   SimulateMouseWheel(aDeltaX, aDeltaY : integer);
      /// <summary>
      /// <para>Clears all certificate exceptions that were added as part of handling
      /// OnCertificateError. If you call this it is recommended that you also call
      /// CloseAllConnections() or you risk not being prompted again for server
      /// certificates if you reconnect quickly.</para>
      /// <para>If aClearImmediately is false then OnCertificateExceptionsCleared is
      /// triggered when the exceptions are cleared.</para>
      /// </summary>
      function    ClearCertificateExceptions(aClearImmediately : boolean = True) : boolean;
      /// <summary>
      /// <para>Clears all HTTP authentication credentials that were added as part of
      /// handling GetAuthCredentials. If |callback| is non-NULL it will be executed
      /// on the UI thread after completion.</para>
      /// <para>If aClearImmediately is false then OnHttpAuthCredentialsCleared is triggered
      /// when the credeintials are cleared.</para>
      /// </summary>
      function    ClearHttpAuthCredentials(aClearImmediately : boolean = True) : boolean;
      /// <summary>
      /// Clears all active and idle connections that Chromium currently has. This
      /// is only recommended if you have released all other CEF objects but don't
      /// yet want to call cef_shutdown().
      /// </summary>
      function    CloseAllConnections(aCloseImmediately : boolean = True) : boolean;
      /// <summary>
      /// <para>Retrieve all the HTML content from the specified frame or the main frame.
      /// Leave aFrameName empty to get the HTML source from the main frame.</para>
      /// <para>It uses a CefStringVisitor to get the HTML content asynchronously and the
      /// result will be received in the TChromiumCore.OnTextResultAvailable event.</para>
      /// </summary>
      procedure   RetrieveHTML(const aFrameName : ustring = ''); overload;
      /// <summary>
      /// <para>Retrieve all the HTML content from the specified frame or the main frame.
      /// Set aFrame to nil to get the HTML source from the main frame.</para>
      /// <para>It uses a CefStringVisitor to get the HTML content asynchronously and the
      /// result will be received in the TChromiumCore.OnTextResultAvailable event.</para>
      /// </summary>
      procedure   RetrieveHTML(const aFrame : ICefFrame); overload;
      /// <summary>
      /// Retrieve all the HTML content from the specified frame or the main frame.
      /// Set aFrameIdentifier to zero to get the HTML source from the main frame.
      /// It uses a CefStringVisitor to get the HTML content asynchronously and the
      /// result will be received in the TChromiumCore.OnTextResultAvailable event.
      /// </summary>
      procedure   RetrieveHTML(const aFrameIdentifier : int64); overload;
      /// <summary>
      /// Retrieve all the text content from the specified frame or the main frame.
      /// Leave aFrameName empty to get the text from the main frame.
      /// It uses a CefStringVisitor to get the text asynchronously and the
      /// result will be received in the TChromiumCore.OnTextResultAvailable event.
      /// </summary>
      procedure   RetrieveText(const aFrameName : ustring = ''); overload;
      /// <summary>
      /// Retrieve all the text content from the specified frame or the main frame.
      /// Set aFrame to nil to get the text from the main frame.
      /// It uses a CefStringVisitor to get the text asynchronously and the
      /// result will be received in the TChromiumCore.OnTextResultAvailable event.
      /// </summary>
      procedure   RetrieveText(const aFrame : ICefFrame); overload;
      /// <summary>
      /// Retrieve all the text content from the specified frame or the main frame.
      /// Set aFrameIdentifier to zero to get the text from the main frame.
      /// It uses a CefStringVisitor to get the text asynchronously and the
      /// result will be received in the TChromiumCore.OnTextResultAvailable event.
      /// </summary>
      procedure   RetrieveText(const aFrameIdentifier : int64); overload;
      /// <summary>
      /// Retrieve a snapshot of current navigation entries asynchronously. The
      /// TChromiumCore.OnNavigationVisitorResultAvailable event will be triggered
      /// for each navigation entry.
      /// </summary>
      procedure   GetNavigationEntries(currentOnly: Boolean);
      /// <summary>
      /// Returns the names of all existing frames.
      /// </summary>
      function    GetFrameNames(var aFrameNames : TStrings) : boolean;
      /// <summary>
      /// Returns the identifiers of all existing frames.
      /// </summary>
      function    GetFrameIdentifiers(var aFrameCount : NativeUInt; var aFrameIdentifierArray : TCefFrameIdentifierArray) : boolean;
      /// <summary>
      /// Execute a string of JavaScript code in this frame.
      /// </summary>
      /// <param name="aCode">JavaScript code.</param>
      /// <param name="aScriptURL">The URL where the script in question can be found, if any. The renderer may request this URL to show the developer the source of the error.</param>
      /// <param name="aFrameName">Name of the frame where the JavaScript code will be executed. This name is generated automatically by Chromium. See ICefBrowser.GetFrameNames.</param>
      /// <param name="aStartLine">The base line number to use for error reporting.</param>
      procedure   ExecuteJavaScript(const aCode, aScriptURL : ustring; const aFrameName : ustring = ''; aStartLine : integer = 0); overload;
      /// <summary>
      /// Execute a string of JavaScript code in this frame.
      /// </summary>
      /// <param name="aCode">JavaScript code.</param>
      /// <param name="aScriptURL">The URL where the script in question can be found, if any. The renderer may request this URL to show the developer the source of the error.</param>
      /// <param name="aFrame">Frame where the JavaScript code will be executed.</param>
      /// <param name="aStartLine">The base line number to use for error reporting.</param>
      procedure   ExecuteJavaScript(const aCode, aScriptURL : ustring; const aFrame : ICefFrame; aStartLine : integer = 0); overload;
      /// <summary>
      /// Execute a string of JavaScript code in this frame.
      /// </summary>
      /// <param name="aCode">JavaScript code.</param>
      /// <param name="aScriptURL">The URL where the script in question can be found, if any. The renderer may request this URL to show the developer the source of the error.</param>
      /// <param name="aFrameIdentifier">Frame where the JavaScript code will be executed.</param>
      /// <param name="aStartLine">The base line number to use for error reporting.</param>
      procedure   ExecuteJavaScript(const aCode, aScriptURL : ustring; const aFrameIdentifier : int64; aStartLine : integer = 0); overload;
      /// <summary>
      /// Used to update the browser preferences using the TChromiumCore property values asynchronously.
      /// </summary>
      procedure   UpdatePreferences;
      /// <summary>
      /// Save the browser preferences as a text file.
      /// </summary>
      procedure   SavePreferences(const aFileName : string);
      /// <summary>
      /// Calls CefRequestContext.ResolveHost to resolve the domain in the URL parameter
      /// to a list of IP addresses.
      /// The result will be received in the TChromiumCore.OnResolvedHostAvailable event.
      /// </summary>
      procedure   ResolveHost(const aURL : ustring);
      /// <summary>
      /// Used to check if the browser parameter is the same as the selected browser in TChromiumCore.
      /// </summary>
      function    IsSameBrowser(const aBrowser : ICefBrowser) : boolean;
      /// <summary>
      /// Calling ExecuteTaskOnCefThread function will trigger the TChromiumCore.OnExecuteTaskOnCefThread event.
      /// </summary>
      /// <param name="aCefThreadId">Indicates the CEF thread on which TChromiumCore.OnExecuteTaskOnCefThread will be executed.</param>
      /// <param name="aTaskID">Custom ID used to identify the task that triggered the TChromiumCore.OnExecuteTaskOnCefThread event.</param>
      /// <param name="aDelayMs">Optional delay in milliseconds to trigger the TChromiumCore.OnExecuteTaskOnCefThread event.</param>
      function    ExecuteTaskOnCefThread(aCefThreadId : TCefThreadId; aTaskID : cardinal; aDelayMs : Int64 = 0) : boolean;
      /// <summary>
      /// This procedure calls the Emulation.setUserAgentOverride DevTools method to override the user agent string.
      /// </summary>
      procedure   SetUserAgentOverride(const aUserAgent : ustring; const aAcceptLanguage : ustring = ''; const aPlatform : ustring = '');
      /// <summary>
      /// This procedure calls the Storage.clearDataForOrigin DevTools method to clear the storage data for a given origin.
      /// </summary>
      procedure   ClearDataForOrigin(const aOrigin : ustring; aStorageTypes : TCefClearDataStorageTypes = cdstAll);
      /// <summary>
      /// This procedure calls the Network.clearBrowserCache DevTools method to clear the cache data.
      /// </summary>
      procedure   ClearCache;
      /// <summary>
      /// Enable or disable the browser's audio.
      /// </summary>
      procedure   ToggleAudioMuted;
      /// <summary>
      /// Used to delete cookies immediately or asynchronously. If aDeleteImmediately is false TChromiumCore.DeleteCookies triggers
      /// the TChromiumCore.OnCookiesDeleted event when the cookies are deleted.
      /// </summary>
      function    DeleteCookies(const url : ustring = ''; const cookieName : ustring = ''; aDeleteImmediately : boolean = False) : boolean;
      /// <summary>
      /// <para>TChromiumCore.VisitAllCookies triggers the TChromiumCore.OnCookiesVisited event for each cookie
      /// aID is an optional parameter to identify which VisitAllCookies call has triggered the
      /// OnCookiesVisited event.</para>
      /// <para>TChromiumCore.OnCookiesVisited may not be triggered if the cookie store is empty but the
      /// TChromium.OnCookieVisitorDestroyed event will always be triggered to signal when the browser
      /// when the visit is over.</para>
      /// </summary>
      function    VisitAllCookies(aID : integer = 0) : boolean;
      /// <summary>
      /// <para>TChromiumCore.VisitURLCookies triggers the TChromiumCore.OnCookiesVisited event for each cookie
      /// aID is an optional parameter to identify which VisitURLCookies call has triggered the
      /// OnCookiesVisited event.</para>
      /// <para>TChromiumCore.OnCookiesVisited may not be triggered if the cookie store is empty but the
      /// TChromium.OnCookieVisitorDestroyed event will always be triggered to signal when the browser
      /// when the visit is over.</para>
      /// </summary>
      function    VisitURLCookies(const url : ustring; includeHttpOnly : boolean = False; aID : integer = 0) : boolean;
      /// <summary>
      /// TChromiumCore.SetCookie triggers the TChromiumCore.OnCookieSet event when the cookie has been set
      /// aID is an optional parameter to identify which SetCookie call has triggered the
      /// OnCookieSet event.
      /// </summary>
      function    SetCookie(const url, name_, value, domain, path: ustring; secure, httponly, hasExpires: Boolean; const creation, lastAccess, expires: TDateTime; same_site : TCefCookieSameSite; priority : TCefCookiePriority; aSetImmediately : boolean = True; aID : integer = 0): Boolean;
      /// <summary>
      /// Flush the backing store (if any) to disk.
      /// </summary>
      /// <param name="aFlushImmediately">If aFlushImmediately is false the cookies will be flushed on the CEF UI thread and the OnCookiesFlushed event will be triggered.</param>
      /// <returns>Returns false (0) if cookies cannot be accessed.</returns>
      function    FlushCookieStore(aFlushImmediately : boolean = True) : boolean;
      /// <summary>
      /// Open developer tools (DevTools) in its own browser. If inspectElementAt has a valid point
      /// with coordinates different than low(integer) then the element at the specified location
      /// will be inspected. If the DevTools browser is already open then it will be focused.
      /// </summary>
      procedure   ShowDevTools(const inspectElementAt: TPoint; aWindowInfo: PCefWindowInfo);
      /// <summary>
      /// close the developer tools.
      /// </summary>
      procedure   CloseDevTools; overload;
      /// <summary>
      /// close the developer tools.
      /// </summary>
      procedure   CloseDevTools(const aDevToolsWnd : TCefWindowHandle); overload;
      /// <summary>
      /// <para>Send a function call message over the DevTools protocol. |message_| must be
      /// a UTF8-encoded JSON dictionary that contains "id" (int), "function"
      /// (string) and "params" (dictionary, optional) values. See the DevTools
      /// protocol documentation at https://chromedevtools.github.io/devtools-
      /// protocol/ for details of supported functions and the expected "params"
      /// dictionary contents. |message_| will be copied if necessary. This function
      /// will return true (1) if called on the UI thread and the message was
      /// successfully submitted for validation, otherwise false (0). Validation
      /// will be applied asynchronously and any messages that fail due to
      /// formatting errors or missing parameters may be discarded without
      /// notification. Prefer ExecuteDevToolsMethod if a more structured approach
      /// to message formatting is desired.</para>
      /// <para>Every valid function call will result in an asynchronous function result
      /// or error message that references the sent message "id". Event messages are
      /// received while notifications are enabled (for example, between function
      /// calls for "Page.enable" and "Page.disable"). All received messages will be
      /// delivered to the observer(s) registered with AddDevToolsMessageObserver.
      /// See ICefDevToolsMessageObserver.OnDevToolsMessage documentation for
      /// details of received message contents.</para>
      /// <para>Usage of the SendDevToolsMessage, ExecuteDevToolsMethod and
      /// AddDevToolsMessageObserver functions does not require an active DevTools
      /// front-end or remote-debugging session. Other active DevTools sessions will
      /// continue to function independently. However, any modification of global
      /// browser state by one session may not be reflected in the UI of other
      /// sessions.</para>
      /// <para>Communication with the DevTools front-end (when displayed) can be logged
      /// for development purposes by passing the `--devtools-protocol-log-
      /// file=<path>` command-line flag.</para>
      /// </summary>
      function    SendDevToolsMessage(const message_: ustring): boolean;
      /// <summary>
      /// <para>Execute a function call over the DevTools protocol. This is a more
      /// structured version of SendDevToolsMessage. |message_id| is an incremental
      /// number that uniquely identifies the message (pass 0 to have the next
      /// number assigned automatically based on previous values). |function| is the
      /// function name. |params| are the function parameters, which may be NULL.</para>
      /// <para>See the DevTools protocol documentation (linked above) for details of
      /// supported functions and the expected |params| dictionary contents. This
      /// function will return the assigned message ID if called on the UI thread
      /// and the message was successfully submitted for validation, otherwise 0.</para>
      /// <para>See the SendDevToolsMessage documentation for additional usage
      /// information.</para>
      /// </summary>
      function    ExecuteDevToolsMethod(message_id: integer; const method: ustring; const params: ICefDictionaryValue): Integer;
      /// <summary>
      /// Add an observer for DevTools protocol messages (function results and
      /// events). The observer will remain registered until the returned
      /// Registration object is destroyed. See the SendDevToolsMessage
      /// documentation for additional usage information.
      /// </summary>
      function    AddDevToolsMessageObserver(const observer: ICefDevToolsMessageObserver): ICefRegistration;
      /// <summary>
      /// <para>Search for |searchText|. |forward| indicates whether to search forward or
      /// backward within the page. |matchCase| indicates whether the search should
      /// be case-sensitive. |findNext| indicates whether this is the first request
      /// or a follow-up. The search will be restarted if |searchText| or
      /// |matchCase| change. The search will be stopped if |searchText| is NULL.</para>
      /// <para>OnFindResult will be triggered to report find results.</para>
      /// </summary>
      procedure   Find(const aSearchText : ustring; aForward, aMatchCase, aFindNext : Boolean);
      /// <summary>
      /// Cancel all searches that are currently going on.
      /// </summary>
      procedure   StopFinding(aClearSelection : Boolean);
      /// <summary>
      /// Print the current browser contents.
      /// </summary>
      procedure   Print;
      /// <summary>
      /// <para>Print the current browser contents to the PDF file specified by |path| and
      /// execute |callback| on completion. The caller is responsible for deleting
      /// |path| when done. For PDF printing to work on Linux you must implement the
      /// ICefPrintHandler.GetPdfPaperSize function.</para>
      /// <para>The TChromiumCore.OnPdfPrintFinished event will be triggered when the PDF
      /// file is created.</para>
      /// </summary>
      procedure   PrintToPDF(const aFilePath : ustring);
      /// <summary>
      /// Execute copy on the focused frame.
      /// </summary>
      procedure   ClipboardCopy;
      /// <summary>
      /// Execute paste on the focused frame.
      /// </summary>
      procedure   ClipboardPaste;
      /// <summary>
      /// Execute cut on the focused frame.
      /// </summary>
      procedure   ClipboardCut;
      /// <summary>
      /// Execute undo on the focused frame.
      /// </summary>
      procedure   ClipboardUndo;
      /// <summary>
      /// Execute redo on the focused frame.
      /// </summary>
      procedure   ClipboardRedo;
      /// <summary>
      /// Execute delete on the focused frame.
      /// </summary>
      procedure   ClipboardDel;
      /// <summary>
      /// Execute select all on the focused frame.
      /// </summary>
      procedure   SelectAll;
      /// <summary>
      /// Increase the zoom value. This procedure triggers the TChromium.OnZoomPctAvailable event with the new zoom value.
      /// </summary>
      procedure   IncZoomStep;
      /// <summary>
      /// Decrease the zoom value. This procedure triggers the TChromium.OnZoomPctAvailable event with the new zoom value.
      /// </summary>
      procedure   DecZoomStep;
      /// <summary>
      /// Increase the zoom value. This procedure triggers the TChromium.OnZoomPctAvailable event with the new zoom value.
      /// </summary>
      procedure   IncZoomPct;
      /// <summary>
      /// Decrease the zoom value. This procedure triggers the TChromium.OnZoomPctAvailable event with the new zoom value.
      /// </summary>
      procedure   DecZoomPct;
      /// <summary>
      /// Reset the zoom value. This procedure triggers the TChromium.OnZoomPctAvailable event with the new zoom value.
      /// </summary>
      procedure   ResetZoomStep;
      /// <summary>
      /// Reset the zoom value. This procedure triggers the TChromium.OnZoomPctAvailable event with the new zoom value.
      /// </summary>
      procedure   ResetZoomLevel;
      /// <summary>
      /// Reset the zoom value. This procedure triggers the TChromium.OnZoomPctAvailable event with the new zoom value.
      /// </summary>
      procedure   ResetZoomPct;
      /// <summary>
      /// Read the zoom value. This procedure triggers the TChromium.OnZoomPctAvailable event with the zoom value.
      /// </summary>
      procedure   ReadZoom;
      /// <summary>
      /// Notify the browser that the widget has been resized. The browser will
      /// first call ICefRenderHandler.GetViewRect to get the new size and then
      /// call ICefRenderHandler.OnPaint asynchronously with the updated
      /// regions. This function is only used when window rendering is disabled.
      /// </summary>
      procedure   WasResized;
      /// <summary>
      /// Notify the browser that it has been hidden or shown. Layouting and
      /// ICefRenderHandler.OnPaint notification will stop when the browser is
      /// hidden. This function is only used when window rendering is disabled.
      /// </summary>
      procedure   WasHidden(hidden: Boolean);
      /// <summary>
      /// Send a notification to the browser that the screen info has changed. The
      /// browser will then call ICefRenderHandler.GetScreenInfo to update the
      /// screen information with the new values. This simulates moving the webview
      /// window from one display to another, or changing the properties of the
      /// current display. This function is only used when window rendering is
      /// disabled.
      /// </summary>
      procedure   NotifyScreenInfoChanged;
      /// <summary>
      /// Notify the browser that the window hosting it is about to be moved or
      /// resized. This function is only used on Windows and Linux.
      /// </summary>
      procedure   NotifyMoveOrResizeStarted;
      /// <summary>
      /// Invalidate the view. The browser will call ICefRenderHandler.OnPaint
      /// asynchronously. This function is only used when window rendering is
      /// disabled.
      /// </summary>
      procedure   Invalidate(type_: TCefPaintElementType = PET_VIEW);
      /// <summary>
      /// Issue a BeginFrame request to Chromium.  Only valid when
      /// TCefWindowInfo.external_begin_frame_enabled is set to true (1).
      /// </summary>
      procedure   SendExternalBeginFrame;
      /// <summary>
      /// Send a key event to the browser.
      /// </summary>
      procedure   SendKeyEvent(const event: PCefKeyEvent);
      /// <summary>
      /// Send a mouse click event to the browser. The |x| and |y| coordinates are
      /// relative to the upper-left corner of the view.
      /// </summary>
      procedure   SendMouseClickEvent(const event: PCefMouseEvent; type_: TCefMouseButtonType; mouseUp: Boolean; clickCount: Integer);
      /// <summary>
      /// Send a mouse move event to the browser. The |x| and |y| coordinates are
      /// relative to the upper-left corner of the view.
      /// </summary>
      procedure   SendMouseMoveEvent(const event: PCefMouseEvent; mouseLeave: Boolean);
      /// <summary>
      /// Send a mouse wheel event to the browser. The |x| and |y| coordinates are
      /// relative to the upper-left corner of the view. The |deltaX| and |deltaY|
      /// values represent the movement delta in the X and Y directions
      /// respectively. In order to scroll inside select popups with window
      /// rendering disabled ICefRenderHandler.GetScreenPoint should be
      /// implemented properly.
      /// </summary>
      procedure   SendMouseWheelEvent(const event: PCefMouseEvent; deltaX, deltaY: Integer);
      /// <summary>
      /// Send a touch event to the browser for a windowless browser.
      /// </summary>
      procedure   SendTouchEvent(const event: PCefTouchEvent);
      /// <summary>
      /// Send a capture lost event to the browser.
      /// </summary>
      procedure   SendCaptureLostEvent;
      /// <summary>
      /// Send a message to the specified |targetProcess|. Ownership of the message
      /// contents will be transferred and the |ProcMessage| reference will be
      /// invalidated. Message delivery is not guaranteed in all cases (for example,
      /// if the browser is closing, navigating, or if the target process crashes).
      /// Send an ACK message back from the target process if confirmation is
      /// required.
      /// </summary>
      procedure   SendProcessMessage(targetProcess: TCefProcessId; const ProcMessage: ICefProcessMessage; const aFrameName : ustring = ''); overload;
      /// <summary>
      /// Send a message to the specified |targetProcess|. Ownership of the message
      /// contents will be transferred and the |ProcMessage| reference will be
      /// invalidated. Message delivery is not guaranteed in all cases (for example,
      /// if the browser is closing, navigating, or if the target process crashes).
      /// Send an ACK message back from the target process if confirmation is
      /// required.
      /// </summary>
      procedure   SendProcessMessage(targetProcess: TCefProcessId; const ProcMessage: ICefProcessMessage; const aFrame : ICefFrame); overload;
      /// <summary>
      /// Send a message to the specified |targetProcess|. Ownership of the message
      /// contents will be transferred and the |ProcMessage| reference will be
      /// invalidated. Message delivery is not guaranteed in all cases (for example,
      /// if the browser is closing, navigating, or if the target process crashes).
      /// Send an ACK message back from the target process if confirmation is
      /// required.
      /// </summary>
      procedure   SendProcessMessage(targetProcess: TCefProcessId; const ProcMessage: ICefProcessMessage; const aFrameIdentifier : int64); overload;
      /// <summary>
      /// <para>Create a new URL request that will be treated as originating from this
      /// frame and the associated browser. Use TCustomCefUrlrequestClient.Create instead if
      /// you do not want the request to have this association, in which case it may
      /// be handled differently (see documentation on that function). A request
      /// created with this function may only originate from the browser process,
      /// and will behave as follows:</para>
      /// </code>
      ///   - It may be intercepted by the client via CefResourceRequestHandler or
      ///     CefSchemeHandlerFactory.
      ///   - POST data may only contain a single element of type PDE_TYPE_FILE or
      ///     PDE_TYPE_BYTES.
      /// </code>
      /// <para>The |request| object will be marked as read-only after calling this
      /// function.</para>
      /// </summary>
      function    CreateUrlRequest(const request: ICefRequest; const client: ICefUrlrequestClient; const aFrameName : ustring = ''): ICefUrlRequest; overload;
      /// <summary>
      /// <para>Create a new URL request that will be treated as originating from this
      /// frame and the associated browser. Use TCustomCefUrlrequestClient.Create instead if
      /// you do not want the request to have this association, in which case it may
      /// be handled differently (see documentation on that function). A request
      /// created with this function may only originate from the browser process,
      /// and will behave as follows:</para>
      /// <code>
      ///   - It may be intercepted by the client via CefResourceRequestHandler or
      ///     CefSchemeHandlerFactory.
      ///   - POST data may only contain a single element of type PDE_TYPE_FILE or
      ///     PDE_TYPE_BYTES.
      /// </code>
      /// <para>The |request| object will be marked as read-only after calling this
      /// function.</para>
      /// </summary>
      function    CreateUrlRequest(const request: ICefRequest; const client: ICefUrlrequestClient; const aFrame : ICefFrame): ICefUrlRequest; overload;
      /// <summary>
      /// <para>Create a new URL request that will be treated as originating from this
      /// frame and the associated browser. Use TCustomCefUrlrequestClient.Create instead if
      /// you do not want the request to have this association, in which case it may
      /// be handled differently (see documentation on that function). A request
      /// created with this function may only originate from the browser process,
      /// and will behave as follows:</para>
      /// <code>
      ///   - It may be intercepted by the client via CefResourceRequestHandler or
      ///     CefSchemeHandlerFactory.
      ///   - POST data may only contain a single element of type PDE_TYPE_FILE or
      ///     PDE_TYPE_BYTES.
      /// </code>
      /// <para>The |request| object will be marked as read-only after calling this
      /// function.</para>
      /// </summary>
      function    CreateUrlRequest(const request: ICefRequest; const client: ICefUrlrequestClient; const aFrameIdentifier : int64): ICefUrlRequest; overload;
      /// <summary>
      /// Set whether the browser is focused.
      /// </summary>
      procedure   SetFocus(focus: Boolean);
      /// <summary>
      /// <para>Set accessibility state for all frames. |accessibility_state| may be
      /// default, enabled or disabled. If |accessibility_state| is STATE_DEFAULT
      /// then accessibility will be disabled by default and the state may be
      /// further controlled with the "force-renderer-accessibility" and "disable-
      /// renderer-accessibility" command-line switches. If |accessibility_state| is
      /// STATE_ENABLED then accessibility will be enabled. If |accessibility_state|
      /// is STATE_DISABLED then accessibility will be completely disabled.</para>
      /// <para>For windowed browsers accessibility will be enabled in Complete mode
      /// (which corresponds to kAccessibilityModeComplete in Chromium). In this
      /// mode all platform accessibility objects will be created and managed by
      /// Chromium's internal implementation. The client needs only to detect the
      /// screen reader and call this function appropriately. For example, on macOS
      /// the client can handle the @"AXEnhancedUserStructure" accessibility
      /// attribute to detect VoiceOver state changes and on Windows the client can
      /// handle WM_GETOBJECT with OBJID_CLIENT to detect accessibility readers.</para>
      /// <para>For windowless browsers accessibility will be enabled in TreeOnly mode
      /// (which corresponds to kAccessibilityModeWebContentsOnly in Chromium). In
      /// this mode renderer accessibility is enabled, the full tree is computed,
      /// and events are passed to CefAccessibiltyHandler, but platform
      /// accessibility objects are not created. The client may implement platform
      /// accessibility objects using CefAccessibiltyHandler callbacks if desired.</para>
      /// </summary>
      procedure   SetAccessibilityState(accessibilityState: TCefState);
      /// <summary>
      /// Call this function when the user drags the mouse into the web view (before
      /// calling DragTargetDragOver/DragTargetLeave/DragTargetDrop). |drag_data|
      /// should not contain file contents as this type of data is not allowed to be
      /// dragged into the web view. File contents can be removed using
      /// ICefDragData.ResetFileContents (for example, if |drag_data| comes from
      /// ICefRenderHandler.StartDragging). This function is only used when
      /// window rendering is disabled.
      /// </summary>
      procedure   DragTargetDragEnter(const dragData: ICefDragData; const event: PCefMouseEvent; allowedOps: TCefDragOperations);
      /// <summary>
      /// Call this function each time the mouse is moved across the web view during
      /// a drag operation (after calling DragTargetDragEnter and before calling
      /// DragTargetDragLeave/DragTargetDrop). This function is only used when
      /// window rendering is disabled.
      /// </summary>
      procedure   DragTargetDragOver(const event: PCefMouseEvent; allowedOps: TCefDragOperations);
      /// <summary>
      /// Call this function when the user drags the mouse out of the web view
      /// (after calling DragTargetDragEnter). This function is only used when
      /// window rendering is disabled.
      /// </summary>
      procedure   DragTargetDragLeave;
      /// <summary>
      /// Call this function when the user completes the drag operation by dropping
      /// the object onto the web view (after calling DragTargetDragEnter). The
      /// object being dropped is |drag_data|, given as an argument to the previous
      /// DragTargetDragEnter call. This function is only used when window rendering
      /// is disabled.
      /// </summary>
      procedure   DragTargetDrop(const event: PCefMouseEvent);
      /// <summary>
      /// Call this function when the drag operation started by a
      /// ICefRenderHandler.StartDragging call has ended either in a drop or by
      /// being cancelled. |x| and |y| are mouse coordinates relative to the upper-
      /// left corner of the view. If the web view is both the drag source and the
      /// drag target then all DragTarget* functions should be called before
      /// DragSource* mthods. This function is only used when window rendering is
      /// disabled.
      /// </summary>
      procedure   DragSourceEndedAt(x, y: Integer; op: TCefDragOperation);
      /// <summary>
      /// Call this function when the drag operation started by a
      /// ICefRenderHandler.StartDragging call has completed. This function may
      /// be called immediately without first calling DragSourceEndedAt to cancel a
      /// drag operation. If the web view is both the drag source and the drag
      /// target then all DragTarget* functions should be called before DragSource*
      /// mthods. This function is only used when window rendering is disabled.
      /// </summary>
      procedure   DragSourceSystemDragEnded;
      /// <summary>
      /// <para>Begins a new composition or updates the existing composition. Blink has a
      /// special node (a composition node) that allows the input function to change
      /// text without affecting other DOM nodes. |text| is the optional text that
      /// will be inserted into the composition node. |underlines| is an optional
      /// set of ranges that will be underlined in the resulting text.</para>
      /// <para>|replacement_range| is an optional range of the existing text that will be
      /// replaced. |selection_range| is an optional range of the resulting text
      /// that will be selected after insertion or replacement. The
      /// |replacement_range| value is only used on OS X.</para>
      /// <para>This function may be called multiple times as the composition changes.
      /// When the client is done making changes the composition should either be
      /// canceled or completed. To cancel the composition call
      /// ImeCancelComposition. To complete the composition call either
      /// ImeCommitText or ImeFinishComposingText. Completion is usually signaled
      /// when:</para>
      /// <code>
      /// 1. The client receives a WM_IME_COMPOSITION message with a GCS_RESULTSTR
      ///    flag (on Windows), or;
      /// 2. The client receives a "commit" signal of GtkIMContext (on Linux), or;
      /// 3. insertText of NSTextInput is called (on Mac).
      /// </code>
      /// <para>This function is only used when window rendering is disabled.</para>
      /// </summary>
      procedure   IMESetComposition(const text: ustring; const underlines : TCefCompositionUnderlineDynArray; const replacement_range, selection_range : PCefRange);
      /// <summary>
      /// Completes the existing composition by optionally inserting the specified
      /// |text| into the composition node. |replacement_range| is an optional range
      /// of the existing text that will be replaced. |relative_cursor_pos| is where
      /// the cursor will be positioned relative to the current cursor position. See
      /// comments on ImeSetComposition for usage. The |replacement_range| and
      /// |relative_cursor_pos| values are only used on OS X. This function is only
      /// used when window rendering is disabled.
      /// </summary>
      procedure   IMECommitText(const text: ustring; const replacement_range : PCefRange; relative_cursor_pos : integer);
      /// <summary>
      /// Completes the existing composition by applying the current composition
      /// node contents. If |keep_selection| is false (0) the current selection, if
      /// any, will be discarded. See comments on ImeSetComposition for usage. This
      /// function is only used when window rendering is disabled.
      /// </summary>
      procedure   IMEFinishComposingText(keep_selection : boolean);
      /// <summary>
      /// Cancels the existing composition and discards the composition node
      /// contents without applying them. See comments on ImeSetComposition for
      /// usage. This function is only used when window rendering is disabled.
      /// </summary>
      procedure   IMECancelComposition;
      /// <summary>
      /// If a misspelled word is currently selected in an editable node calling
      /// this function will replace it with the specified |word|.
      /// </summary>
      procedure   ReplaceMisspelling(const aWord : ustring);
      /// <summary>
      /// Add the specified |word| to the spelling dictionary.
      /// </summary>
      procedure   AddWordToDictionary(const aWord : ustring);
      {$IFDEF LINUX}
      /// <summary>
      /// Used in Linux to resize the browser contents.
      /// </summary>
      procedure   UpdateBrowserSize(aLeft, aTop, aWidth, aHeight : integer);
      /// <summary>
      /// Used in Linux to update the browser visibility.
      /// </summary>
      procedure   UpdateXWindowVisibility(aVisible : boolean);
      {$ENDIF}
      /// <summary>
      /// Add an observer for MediaRouter events. The observer will remain
      /// registered until the returned Registration object is destroyed.
      /// </summary>
      function    AddObserver(const observer: ICefMediaObserver): ICefRegistration;
      /// <summary>
      /// Returns a MediaSource object for the specified media source URN. Supported
      /// URN schemes include "cast:" and "dial:", and will be already known by the
      /// client application (e.g. "cast:<appId>?clientId=<clientId>").
      /// </summary>
      function    GetSource(const urn: ustring): ICefMediaSource;
      /// <summary>
      /// Trigger an asynchronous call to ICefMediaObserver.OnSinks on all
      /// registered observers.
      /// </summary>
      procedure   NotifyCurrentSinks;
      /// <summary>
      /// Trigger an asynchronous call to ICefMediaObserver.OnRoutes on all
      /// registered observers.
      /// </summary>
      procedure   NotifyCurrentRoutes;
      /// <summary>
      /// <para>Create a new route between |source| and |sink|. Source and sink must be
      /// valid, compatible (as reported by ICefMediaSink.IsCompatibleWith), and
      /// a route between them must not already exist. |callback| will be executed
      /// on success or failure. If route creation succeeds it will also trigger an
      /// asynchronous call to ICefMediaObserver.OnRoutes on all registered
      /// observers.</para>
      /// <para>This procedure is asynchronous and the result, ICefMediaRoute and the error
      /// message will be available in the TChromium.OnMediaRouteCreateFinished event.</para>
      /// </summary>
      procedure   CreateRoute(const source: ICefMediaSource; const sink: ICefMediaSink);
      /// <summary>
      /// <para>Asynchronously retrieves device info.</para>
      /// <para>This procedure will trigger OnMediaSinkDeviceInfo with the device info.</para>
      /// </summary>
      procedure   GetDeviceInfo(const aMediaSink: ICefMediaSink);
      /// <summary>
      /// <para>Load an extension.</para>
      /// <para>If extension resources will be read from disk using the default load
      /// implementation then |root_directory| should be the absolute path to the
      /// extension resources directory and |manifest| should be NULL. If extension
      /// resources will be provided by the client (e.g. via cef_request_handler_t
      /// and/or cef_extension_handler_t) then |root_directory| should be a path
      /// component unique to the extension (if not absolute this will be internally
      /// prefixed with the PK_DIR_RESOURCES path) and |manifest| should contain the
      /// contents that would otherwise be read from the "manifest.json" file on
      /// disk.</para>
      /// <para>The loaded extension will be accessible in all contexts sharing the same
      /// storage (HasExtension returns true (1)). However, only the context on
      /// which this function was called is considered the loader (DidLoadExtension
      /// returns true (1)) and only the loader will receive
      /// TCustomRequestContextHandler callbacks for the extension.</para>
      /// <para>TCustomExtensionHandler.OnExtensionLoaded will be called on load success
      /// or TCustomExtensionHandler.OnExtensionLoadFailed will be called on load
      /// failure.</para>
      /// <para>If the extension specifies a background script via the "background"
      /// manifest key then TCustomExtensionHandler.OnBeforeBackgroundBrowser will
      /// be called to create the background browser. See that function for
      /// additional information about background scripts.</para>
      /// <para>For visible extension views the client application should evaluate the
      /// manifest to determine the correct extension URL to load and then pass that
      /// URL to the ICefBrowserHost.CreateBrowser* function after the extension
      /// has loaded. For example, the client can look for the "browser_action"
      /// manifest key as documented at
      /// https://developer.chrome.com/extensions/browserAction. Extension URLs take
      /// the form "chrome-extension://<extension_id>/<path>".</para>
      /// <para>Browsers that host extensions differ from normal browsers as follows:</para>
      /// <code>
      ///  - Can access chrome.* JavaScript APIs if allowed by the manifest. Visit
      ///    chrome://extensions-support for the list of extension APIs currently
      ///    supported by CEF.
      ///  - Main frame navigation to non-extension content is blocked.
      ///  - Pinch-zooming is disabled.
      ///  - CefBrowserHost::GetExtension returns the hosted extension.
      ///  - CefBrowserHost::IsBackgroundHost returns true for background hosts.
      /// </code>
      /// <para>See https://developer.chrome.com/extensions for extension implementation
      /// and usage documentation.</para>
      /// </summary>
      function    LoadExtension(const root_directory: ustring; const manifest: ICefDictionaryValue = nil; const handler: ICefExtensionHandler = nil; const requestContext : ICefRequestContext = nil) : boolean;
      /// <summary>
      /// Returns true (1) if this context was used to load the extension identified
      /// by |extension_id|. Other contexts sharing the same storage will also have
      /// access to the extension (see HasExtension). This function must be called
      /// on the browser process UI thread.
      /// </summary>
      function    DidLoadExtension(const extension_id: ustring): boolean;
      /// <summary>
      /// Returns true (1) if this context has access to the extension identified by
      /// |extension_id|. This may not be the context that was used to load the
      /// extension (see DidLoadExtension). This function must be called on the
      /// browser process UI thread.
      /// </summary>
      function    HasExtension(const extension_id: ustring): boolean;
      /// <summary>
      /// Retrieve the list of all extensions that this context has access to (see
      /// HasExtension). |extension_ids| will be populated with the list of
      /// extension ID values. Returns true (1) on success. This function must be
      /// called on the browser process UI thread.
      /// </summary>
      function    GetExtensions(const extension_ids: TStringList): boolean;
      /// <summary>
      /// Returns the extension matching |extension_id| or NULL if no matching
      /// extension is accessible in this context (see HasExtension). This function
      /// must be called on the browser process UI thread.
      /// </summary>
      function    GetExtension(const extension_id: ustring): ICefExtension;
      /// <summary>
      /// Returns the current value for |content_type| that applies for the
      /// specified URLs. If both URLs are NULL the default value will be returned.
      /// Returns nullptr if no value is configured. Must be called on the browser
      /// process UI thread.
      /// </summary>
      function    GetWebsiteSetting(const requesting_url, top_level_url: ustring; content_type: TCefContentSettingTypes): ICefValue;
      /// <summary>
      /// <para>Sets the current value for |content_type| for the specified URLs in the
      /// default scope. If both URLs are NULL, and the context is not incognito,
      /// the default value will be set. Pass nullptr for |value| to remove the
      /// default value for this content type.</para>
      /// <para>WARNING: Incorrect usage of this function may cause instability or
      /// security issues in Chromium. Make sure that you first understand the
      /// potential impact of any changes to |content_type| by reviewing the related
      /// source code in Chromium. For example, if you plan to modify
      /// CEF_CONTENT_SETTING_TYPE_POPUPS, first review and understand the usage of
      /// ContentSettingsType::POPUPS in Chromium:
      /// https://source.chromium.org/search?q=ContentSettingsType::POPUPS</para>
      /// </summary>
      procedure   SetWebsiteSetting(const requesting_url, top_level_url: ustring; content_type: TCefContentSettingTypes; const value: ICefValue);
      /// <summary>
      /// Returns the current value for |content_type| that applies for the
      /// specified URLs. If both URLs are NULL the default value will be returned.
      /// Returns CEF_CONTENT_SETTING_VALUE_DEFAULT if no value is configured. Must
      /// be called on the browser process UI thread.
      /// </summary>
      function    GetContentSetting(const requesting_url, top_level_url: ustring; content_type: TCefContentSettingTypes): TCefContentSettingValues;
      /// <summary>
      /// <para>Sets the current value for |content_type| for the specified URLs in the
      /// default scope. If both URLs are NULL, and the context is not incognito,
      /// the default value will be set. Pass CEF_CONTENT_SETTING_VALUE_DEFAULT for
      /// |value| to use the default value for this content type.</para>
      /// <para>WARNING: Incorrect usage of this function may cause instability or
      /// security issues in Chromium. Make sure that you first understand the
      /// potential impact of any changes to |content_type| by reviewing the related
      /// source code in Chromium. For example, if you plan to modify
      /// CEF_CONTENT_SETTING_TYPE_POPUPS, first review and understand the usage of
      /// ContentSettingsType::POPUPS in Chromium:
      /// https://source.chromium.org/search?q=ContentSettingsType::POPUPS</para>
      /// </summary>
      procedure   SetContentSetting(const requesting_url, top_level_url: ustring; content_type: TCefContentSettingTypes; value: TCefContentSettingValues);
      /// <summary>
      /// First URL loaded by the browser after its creation.
      /// </summary>
      property  DefaultUrl                    : ustring                      read FDefaultUrl                  write SetDefaultUrl;
      /// <summary>
      /// Properties used to fill the TCefBrowserSettings record which is used during the browser creation.
      /// </summary>
      property  Options                       : TChromiumOptions             read FOptions                     write FOptions;
      /// <summary>
      /// Font properties used to fill the TCefBrowserSettings record which is used during the browser creation.
      /// </summary>
      property  FontOptions                   : TChromiumFontOptions         read FFontOptions                 write FFontOptions;
      /// <summary>
      /// Properties used to fill the TCefPdfPrintSettings record which is used in the TChromiumCore.PrintToPDF call.
      /// </summary>
      property  PDFPrintOptions               : TPDFPrintOptions             read FPDFPrintOptions             write FPDFPrintOptions;
      /// <summary>
      /// Default encoding for Web content. If empty "ISO-8859-1" will be used. Also
      /// configurable using the "default-encoding" command-line switch. It's used during the browser creation.
      /// </summary>
      property  DefaultEncoding               : ustring                      read FDefaultEncoding             write FDefaultEncoding;
      /// <summary>
      /// Globally unique identifier for the seleted browser.
      /// </summary>
      property  BrowserId                     : integer                      read GetBrowserId;
      /// <summary>
      /// Returns a ICefBrowser instance of the selected browser.
      /// </summary>
      property  Browser                       : ICefBrowser                  read GetBrowser;
      /// <summary>
      ///	Returns a ICefBrowser instance of the browser with the specified id.
      /// </summary>
      property  BrowserById[id : integer]     : ICefBrowser                  read GetBrowserById;
      /// <summary>
      ///  Returns the number of browsers in the browser array when the multi-browser mode is enabled.
      /// </summary>
      property  BrowserCount                  : integer                      read GetBrowserCount;
      /// <summary>
      /// Returns the identifier of the browser in the specified array position when the multi-browser mode is enabled.
      /// </summary>
      property  BrowserIdByIndex[i : integer] : integer                      read GetBrowserIdByIndex;
      /// <summary>
      /// Returns the ICefClient instance used by the selected browser.
      /// </summary>
      property  CefClient                     : ICefClient                   read FHandler;
      /// <summary>
      /// Returns the ICefRequestContextHandler instance used in this browser.
      /// </summary>
      property  ReqContextHandler             : ICefRequestContextHandler    read FReqContextHandler;
      /// <summary>
      /// Returns the ICefResourceRequestHandler instance used in this browser.
      /// </summary>
      property  ResourceRequestHandler        : ICefResourceRequestHandler   read FResourceRequestHandler;
      /// <summary>
      /// Returns the TCefWindowInfo record used when the browser was created.
      /// </summary>
      property  CefWindowInfo                 : TCefWindowInfo               read FWindowInfo;
      /// <summary>
      /// Returns the current visible navigation entry for this browser. This
      /// property can only be read on the UI thread.
      /// </summary>
      property  VisibleNavigationEntry        : ICefNavigationEntry          read GetVisibleNavigationEntry;
      /// <summary>
      /// Returns a ICefRequestContext instance used by the selected browser.
      /// </summary>
      property  RequestContext                : ICefRequestContext           read GetRequestContext;
      /// <summary>
      /// Returns a ICefMediaRouter instance used by the selected browser.
      /// </summary>
      property  MediaRouter                   : ICefMediaRouter              read GetMediaRouter;
      /// <summary>
      /// Returns a ICefMediaObserver instance used by the selected browser.
      /// </summary>
      property  MediaObserver                 : ICefMediaObserver            read FMediaObserver;
      /// <summary>
      /// Returns the ICefRegistration instance obtained when the default MediaObserver was added.
      /// </summary>
      property  MediaObserverReg              : ICefRegistration             read FMediaObserverReg;
      /// <summary>
      /// Returns a ICefDevToolsMessageObserver instance used by the selected browser.
      /// </summary>
      property  DevToolsMsgObserver           : ICefDevToolsMessageObserver  read FDevToolsMsgObserver;
      /// <summary>
      /// Returns the ICefRegistration instance obtained when the default DevToolsMessageObserver was added.
      /// </summary>
      property  DevToolsMsgObserverReg        : ICefRegistration             read FDevToolsMsgObserverReg;
      /// <summary>
      /// Returns a ICefExtensionHandler instance used by the selected browser.
      /// </summary>
      property  ExtensionHandler              : ICefExtensionHandler         read FExtensionHandler;
      /// <summary>
      ///	Returns the value of GlobalCEFApp.MultiThreadedMessageLoop.
      /// </summary>
      property  MultithreadApp                : boolean                      read GetMultithreadApp;
      /// <summary>
      /// Calls ICefBrowser.IsLoading and returns true if the browser is currently loading.
      /// </summary>
      property  IsLoading                     : boolean                      read GetIsLoading;
      /// <summary>
      /// Calls ICefBrowser.HasDocument and returns true if a document has been loaded in the browser.
      /// </summary>
      property  HasDocument                   : boolean                      read GetHasDocument;
      /// <summary>
      /// Calls ICefBrowserHost.HasView and returns true if this browser is wrapped in a ICefBrowserView.
      /// </summary>
      property  HasView                       : boolean                      read GetHasView;
      /// <summary>
      /// Calls ICefBrowserHost.HasDevTools and returns true if this browser currently has an associated DevTools browser.
      /// </summary>
      property  HasDevTools                   : boolean                      read GetHasDevTools;
      /// <summary>
      /// Returns true if ICefClient has a valid value.
      /// </summary>
      property  HasClientHandler              : boolean                      read GetHasClientHandler;
      /// <summary>
      /// Returns true if this component has a valid selected browser.
      /// </summary>
      property  HasBrowser                    : boolean                      read GetHasBrowser;
      /// <summary>
      /// Calls ICefBrowser.CanGoBack and returns true if the browser can navigate back.
      /// </summary>
      property  CanGoBack                     : boolean                      read GetCanGoBack;
      /// <summary>
      /// Calls ICefBrowser.CanGoForward and returns true if the browser can navigate forward.
      /// </summary>
      property  CanGoForward                  : boolean                      read GetCanGoForward;
      /// <summary>
      /// Calls ICefBrowser.IsPopUp and returns true if the window is a popup window.
      /// </summary>
      property  IsPopUp                       : boolean                      read GetIsPopUp;
      /// <summary>
      /// Calls ICefBrowserHost.GetWindowHandle and returns the window handle for this browser.
      /// </summary>
      property  WindowHandle                  : TCefWindowHandle             read GetWindowHandle;
      /// <summary>
      /// Calls ICefBrowserHost.GetOpenerWindowHandle and returns the window handle of the browser that opened this browser.
      /// </summary>
      property  OpenerWindowHandle            : TCefWindowHandle             read GetOpenerWindowHandle;
      {$IFDEF MSWINDOWS}
      /// <summary>
      /// Handle of one to the child controls created automatically by CEF to show the web contents.
      /// </summary>
      property  BrowserHandle                 : THandle                      read FBrowserCompHWND;
      /// <summary>
      /// Handle of one to the child controls created automatically by CEF to show the web contents.
      /// </summary>
      property  WidgetHandle                  : THandle                      read FWidgetCompHWND;
      /// <summary>
      /// Handle of one to the child controls created automatically by CEF to show the web contents.
      /// </summary>
      property  RenderHandle                  : THandle                      read FRenderCompHWND;
      {$ENDIF}
      /// <summary>
      /// Returns true if ICefBrowser.FocusedFrame has a valid value.
      /// </summary>
      property  FrameIsFocused                : boolean                      read GetFrameIsFocused;
      /// <summary>
      /// Returns true when the browser is fully initialized and it's not being closed.
      /// </summary>
      property  Initialized                   : boolean                      read GetInitialized;
      /// <summary>
      /// Returns the cache value in ICefRequestContext.CachePath.
      /// </summary>
      property  RequestContextCache           : ustring                      read GetRequestContextCache;
      /// <summary>
      /// Calls ICefRequestContext.IsGlobal to check if the request context is the global context or it's independent.
      /// </summary>
      property  RequestContextIsGlobal        : boolean                      read GetRequestContextIsGlobal;
      /// <summary>
      /// Returns the URL of the main frame.
      /// </summary>
      property  DocumentURL                   : ustring                      read GetDocumentURL;
      /// <summary>
      /// Returns the current zoom value. This property is based on the CefBrowserHost.ZoomLevel value which can only be read in the CEF UI thread.
      /// </summary>
      property  ZoomLevel                     : double                       read GetZoomLevel                 write SetZoomLevel;
      /// <summary>
      /// Returns the current zoom value. This property is based on the CefBrowserHost.ZoomLevel value which can only be read in the CEF UI thread.
      /// </summary>
      property  ZoomPct                       : double                       read GetZoomPct                   write SetZoomPct;
      /// <summary>
      /// Returns the current zoom value. This property is based on the CefBrowserHost.ZoomLevel value which can only be read in the CEF UI thread.
      /// </summary>
      property  ZoomStep                      : byte                         read GetZoomStep                  write SetZoomStep;
      /// <summary>
      /// Returns the maximum rate in frames per second (fps) that OnPaint will be called for a browser in OSR mode.
      /// </summary>
      property  WindowlessFrameRate           : integer                      read GetWindowlessFrameRate       write SetWindowlessFrameRate;
      /// <summary>
      /// Custom HTTP header name added to all requests.
      /// </summary>
      property  CustomHeaderName              : ustring                      read FCustomHeaderName            write SetCustomHeaderName;
      /// <summary>
      /// Custom HTTP header value added to all requests.
      /// </summary>
      property  CustomHeaderValue             : ustring                      read FCustomHeaderValue           write SetCustomHeaderValue;
      /// <summary>
      /// Set to True if you want to send the DNT header.
      /// </summary>
      property  DoNotTrack                    : boolean                      read FDoNotTrack                  write SetDoNotTrack;
      /// <summary>
      /// Set to True if you want to send the referer header.
      /// </summary>
      property  SendReferrer                  : boolean                      read FSendReferrer                write SetSendReferrer;
      /// <summary>
      /// Enable hyperlink auditing.
      /// </summary>
      property  HyperlinkAuditing             : boolean                      read FHyperlinkAuditing           write SetHyperlinkAuditing;
      /// <summary>
      /// Allow using outdated plugins.
      /// </summary>
      property  AllowOutdatedPlugins          : boolean                      read FAllowOutdatedPlugins        write SetAllowOutdatedPlugins;
      /// <summary>
      /// Always authorize plugins.
      /// </summary>
      property  AlwaysAuthorizePlugins        : boolean                      read FAlwaysAuthorizePlugins      write SetAlwaysAuthorizePlugins;
      /// <summary>
      /// Always open PDF files externally.
      /// </summary>
      property  AlwaysOpenPDFExternally       : boolean                      read FAlwaysOpenPDFExternally     write SetAlwaysOpenPDFExternally;
      /// <summary>
      /// Set to True if you want to enable the spell checker.
      /// </summary>
      property  SpellChecking                 : boolean                      read FSpellChecking               write SetSpellChecking;
      /// <summary>
      /// Comma delimited list of language codes used by the spell checker, for example "es-ES,en-US,fr-FR,de-DE,it-IT".
      /// </summary>
      property  SpellCheckerDicts             : ustring                      read FSpellCheckerDicts           write SetSpellCheckerDicts;
      /// <summary>
      /// Returns true if the main frame exists and it's valid.
      /// </summary>
      property  HasValidMainFrame             : boolean                      read GetHasValidMainFrame;
      /// <summary>
      /// Returns the number of frames that currently exist.
      /// </summary>
      property  FrameCount                    : NativeUInt                   read GetFrameCount;
      /// <summary>
      ///	Returns the TcefDragOperation value used during drag and drop.
      /// </summary>
      property  DragOperations                : TCefDragOperations           read FDragOperations              write FDragOperations;
      /// <summary>
      /// Returns true if the browser's audio is muted.
      /// </summary>
      property  AudioMuted                    : boolean                      read GetAudioMuted                write SetAudioMuted;
      /// <summary>
      /// Forces the Google safesearch in the browser preferences.
      /// </summary>
      property  SafeSearch                    : boolean                      read FSafeSearch                  write SetSafeSearch;
      /// <summary>
      /// Forces the YouTube restrictions in the browser preferences.
      /// </summary>
      property  YouTubeRestrict               : integer                      read FYouTubeRestrict             write SetYouTubeRestrict;
      /// <summary>
      /// Enables printing in the browser preferences.
      /// </summary>
      property  PrintingEnabled               : boolean                      read FPrintingEnabled             write SetPrintingEnabled;
      /// <summary>
      /// Set the accept language list in the browser preferences.
      /// </summary>
      property  AcceptLanguageList            : ustring                      read FAcceptLanguageList          write SetAcceptLanguageList;
      /// <summary>
      /// Sets the cookies policy value in the browser preferences.
      /// </summary>
      property  AcceptCookies                 : TCefCookiePref               read FAcceptCookies               write SetAcceptCookies;
      /// <summary>
      /// Blocks third party cookies in the browser preferences.
      /// </summary>
      property  Block3rdPartyCookies          : boolean                      read FBlock3rdPartyCookies        write SetBlock3rdPartyCookies;
      /// <summary>
      /// Enables the multi-browser mode that allows TChromiumCore to handle several browsers with one component. These browsers are usually the main browser, popup windows and new tabs.
      /// </summary>
      property  MultiBrowserMode              : boolean                      read FMultiBrowserMode            write SetMultiBrowserMode;
      /// <summary>
      /// Default ExStyle value used to initialize the browser. A value of WS_EX_NOACTIVATE can be used as a workaround for some focus issues in CEF.
      /// </summary>
      property  DefaultWindowInfoExStyle      : DWORD                        read FDefaultWindowInfoExStyle    write FDefaultWindowInfoExStyle;
      /// <summary>
      /// Uses the Network.emulateNetworkConditions DevTool method to set the browser in offline mode.
      /// </summary>
      property  Offline                       : boolean                      read FOffline                     write SetOffline;
      /// <summary>
      /// Enables the Quic protocol in the browser preferences.
      /// </summary>
      property  QuicAllowed                   : boolean                      read FQuicAllowed                 write SetQuicAllowed;
      /// <summary>
      /// Enables JavaScript in the browser preferences.
      /// </summary>
      property  JavascriptEnabled             : boolean                      read FJavascriptEnabled           write SetJavascriptEnabled;
      /// <summary>
      /// Enables automatic image loading in the browser preferences.
      /// </summary>
      property  LoadImagesAutomatically       : boolean                      read FLoadImagesAutomatically     write SetLoadImagesAutomatically;
      /// <summary>
      /// Battery saver mode state.
      /// </summary>
      property  BatterySaverModeState         : TCefBatterySaverModeState    read FBatterySaverModeState       write SetBatterySaverModeState;
      /// <summary>
      /// High efficiency mode state.
      /// </summary>
      property  HighEfficiencyModeState       : TCefHighEfficiencyModeState  read FHighEfficiencyModeState     write SetHighEfficiencyModeState;
      /// <summary>
      /// Indicates whether the browser can receive focus.
      /// </summary>
      property  CanFocus                      : boolean                      read FCanFocus;
      /// <summary>
      /// Delay in milliseconds to enable browser focus.
      /// </summary>
      property  EnableFocusDelayMs            : cardinal                     read FEnableFocusDelayMs          write FEnableFocusDelayMs;
      {$IFDEF LINUX}
      /// <summary>
      /// Gets the Xdisplay pointer in Linux.
      /// </summary>
      property  XDisplay                      : PXDisplay                    read GetXDisplay;
      {$ENDIF}
      /// <summary>
      /// WebRTC handling policy setting in the browser preferences.
      /// </summary>
      property  WebRTCIPHandlingPolicy        : TCefWebRTCHandlingPolicy     read FWebRTCIPHandlingPolicy      write SetWebRTCIPHandlingPolicy;
      /// <summary>
      /// WebRTC multiple routes setting in the browser preferences.
      /// </summary>
      property  WebRTCMultipleRoutes          : TCefState                    read FWebRTCMultipleRoutes        write SetWebRTCMultipleRoutes;
      /// <summary>
      /// WebRTC nonproxied UDP setting in the browser preferences.
      /// </summary>
      property  WebRTCNonproxiedUDP           : TCefState                    read FWebRTCNonProxiedUDP         write SetWebRTCNonProxiedUDP;
      /// <summary>
      /// Proxy type: CEF_PROXYTYPE_DIRECT, CEF_PROXYTYPE_AUTODETECT, CEF_PROXYTYPE_SYSTEM, CEF_PROXYTYPE_FIXED_SERVERS or CEF_PROXYTYPE_PAC_SCRIPT.
      /// </summary>
      property  ProxyType                     : integer                      read FProxyType                   write SetProxyType;
      /// <summary>
      /// Proxy scheme
      /// </summary>
      property  ProxyScheme                   : TCefProxyScheme              read FProxyScheme                 write SetProxyScheme;
      /// <summary>
      /// Proxy server address
      /// </summary>
      property  ProxyServer                   : ustring                      read FProxyServer                 write SetProxyServer;
      /// <summary>
      /// Proxy server port
      /// </summary>
      property  ProxyPort                     : integer                      read FProxyPort                   write SetProxyPort;
      /// <summary>
      /// Proxy username
      /// </summary>
      property  ProxyUsername                 : ustring                      read FProxyUsername               write SetProxyUsername;
      /// <summary>
      /// Proxy password
      /// </summary>
      property  ProxyPassword                 : ustring                      read FProxyPassword               write SetProxyPassword;
      /// <summary>
      /// URL of the PAC script file.
      /// </summary>
      property  ProxyScriptURL                : ustring                      read FProxyScriptURL              write SetProxyScriptURL;
      /// <summary>
      /// This tells chromium to bypass any specified proxy for the given semi-colon-separated list of hosts.
      /// </summary>
      property  ProxyByPassList               : ustring                      read FProxyByPassList             write SetProxyByPassList;
      /// <summary>
      /// Sets the maximum connections per proxy value in the browser preferences (experimental).
      /// </summary>
      property  MaxConnectionsPerProxy        : integer                      read FMaxConnectionsPerProxy      write SetMaxConnectionsPerProxy;

    published
      /// <summary>
      /// Triggered after a TChromiumCore.RetrieveHTML or TChromiumCore.RetrieveText call with the HTML or text results.
      /// </summary>
      property  OnTextResultAvailable              : TOnTextResultAvailableEvent              read FOnTextResultAvailable              write FOnTextResultAvailable;
      /// <summary>
      ///	Triggered after a TChromiumCore.PrintToPDF call when the PDF has been created.
      /// </summary>
      property  OnPdfPrintFinished                 : TOnPdfPrintFinishedEvent                 read FOnPdfPrintFinished                 write FOnPdfPrintFinished;
      /// <summary>
      /// Triggered after a TChromiumCore.SavePreferences call when the preferences have been saved.
      /// </summary>
      property  OnPrefsAvailable                   : TOnPrefsAvailableEvent                   read FOnPrefsAvailable                   write FOnPrefsAvailable;
      /// <summary>
      /// Triggered when the browser preferences have been updated.
      /// </summary>
      property  OnPrefsUpdated                     : TNotifyEvent                             read FOnPrefsUpdated                     write FOnPrefsUpdated;
      /// <summary>
      /// Triggered after a TChromiumCore.DeleteCookies call when the cookies have been deleted.
      /// </summary>
      property  OnCookiesDeleted                   : TOnCookiesDeletedEvent                   read FOnCookiesDeleted                   write FOnCookiesDeleted;
      /// <summary>
      /// Triggered after a TChromiumCore.ResolveHost call with the host information.
      /// </summary>
      property  OnResolvedHostAvailable            : TOnResolvedIPsAvailableEvent             read FOnResolvedHostAvailable            write FOnResolvedHostAvailable;
      /// <summary>
      ///	Triggered after a TChromiumCore.GetNavigationEntries call with a navigation entry.
      /// </summary>
      property  OnNavigationVisitorResultAvailable : TOnNavigationVisitorResultAvailableEvent read FOnNavigationVisitorResultAvailable write FOnNavigationVisitorResultAvailable;
      /// <summary>
      ///	Triggered after a TChromiumCore.DownloadImage call when the download is complete.
      /// </summary>
      property  OnDownloadImageFinished            : TOnDownloadImageFinishedEvent            read FOnDownloadImageFinished            write FOnDownloadImageFinished;
      /// <summary>
      ///	Triggered after a TChromiumCore.FlushCookieStore call when the cookies are flushed.
      /// </summary>
      property  OnCookiesFlushed                   : TNotifyEvent                             read FOnCookiesFlushed                   write FOnCookiesFlushed;
      /// <summary>
      /// Triggered after a TChromiumCore.ClearCertificateExceptions call when the exceptions are cleared.
      /// </summary>
      property  OnCertificateExceptionsCleared     : TNotifyEvent                             read FOnCertificateExceptionsCleared     write FOnCertificateExceptionsCleared;
      /// <summary>
      /// Triggered after a TChromiumCore.ClearHttpAuthCredentials call when the credentials are cleared.
      /// </summary>
      property  OnHttpAuthCredentialsCleared       : TNotifyEvent                             read FOnHttpAuthCredentialsCleared       write FOnHttpAuthCredentialsCleared;
      /// <summary>
      /// Triggered after a TChromiumCore.CloseAllConnections call when the connections are closed.
      /// </summary>
      property  OnAllConnectionsClosed             : TNotifyEvent                             read FOnAllConnectionsClosed             write FOnAllConnectionsClosed;
      /// <summary>
      /// Triggered after a TChromiumCore.ExecuteTaskOnCefThread call in the context of the specified CEF thread.
      /// </summary>
      property  OnExecuteTaskOnCefThread           : TOnExecuteTaskOnCefThread                read FOnExecuteTaskOnCefThread           write FOnExecuteTaskOnCefThread;
      /// <summary>
      /// Triggered after a TChromiumCore.VisitAllCookies call with cookie information.
      /// </summary>
      property  OnCookiesVisited                   : TOnCookiesVisited                        read FOnCookiesVisited                   write FOnCookiesVisited;
      /// <summary>
      /// 	Triggered after a TChromiumCore.VisitAllCookies call when the IcefCookieVisitor has been destroyed.
      /// </summary>
      property  OnCookieVisitorDestroyed           : TOnCookieVisitorDestroyed                read FOnCookieVisitorDestroyed           write FOnCookieVisitorDestroyed;
      /// <summary>
      /// Triggered after a TChromiumCore.SetCookie call when the cookie has been set.
      /// </summary>
      property  OnCookieSet                        : TOnCookieSet                             read FOnCookieSet                        write FOnCookieSet;
      /// <summary>
      /// Triggered after a call to any of the procedures to increase, decrease or reset the zoom with the new zoom value.
      /// </summary>
      property  OnZoomPctAvailable                 : TOnZoomPctAvailable                      read FOnZoomPctAvailable                 write FOnZoomPctAvailable;
      /// <summary>
      /// Triggered after a TChromiumCore.CreateRoute call when the route is created.
      /// </summary>
      property  OnMediaRouteCreateFinished         : TOnMediaRouteCreateFinishedEvent         read FOnMediaRouteCreateFinished         write FOnMediaRouteCreateFinished;
      /// <summary>
      /// Triggered after a TChromiumCore.GetDeviceInfo call with the device info.
      /// </summary>
      property  OnMediaSinkDeviceInfo              : TOnMediaSinkDeviceInfoEvent              read FOnMediaSinkDeviceInfo              write FOnMediaSinkDeviceInfo;
      /// <summary>
      /// Triggered when the browser is capable of being focused.
      /// </summary>
      property  OnCanFocus                         : TNotifyEvent                             read FOnCanFocus                         write FOnCanFocus;
      {$IFDEF MSWINDOWS}
      /// <summary>
      /// Triggered for all messages sent to the child controls created by CEF to show the web contents.
      /// </summary>
      property  OnBrowserCompMsg                   : TOnCompMsgEvent                          read FOnBrowserCompMsg                   write FOnBrowserCompMsg;
      /// <summary>
      /// Triggered for all messages sent to the child controls created by CEF to show the web contents.
      /// </summary>
      property  OnWidgetCompMsg                    : TOnCompMsgEvent                          read FOnWidgetCompMsg                    write FOnWidgetCompMsg;
      /// <summary>
      /// Triggered for all messages sent to the child controls created by CEF to show the web contents.
      /// </summary>
      property  OnRenderCompMsg                    : TOnCompMsgEvent                          read FOnRenderCompMsg                    write FOnRenderCompMsg;
      {$ENDIF}

      /// <summary>
      /// Called when a new message is received from a different process. Return
      /// true (1) if the message was handled or false (0) otherwise.  It is safe to
      /// keep a reference to |message| outside of this callback.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_client_capi.h">CEF source file: /include/capi/cef_client_capi.h (cef_client_t)</see></para>
      /// </remarks>
      property OnProcessMessageReceived         : TOnProcessMessageReceived         read FOnProcessMessageReceived         write FOnProcessMessageReceived;
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
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_load_handler_capi.h">CEF source file: /include/capi/cef_load_handler_capi.h (cef_load_handler_t)</see></para>
      /// </remarks>
      property OnLoadStart                      : TOnLoadStart                      read FOnLoadStart                      write FOnLoadStart;
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
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_load_handler_capi.h">CEF source file: /include/capi/cef_load_handler_capi.h (cef_load_handler_t)</see></para>
      /// </remarks>
      property OnLoadEnd                        : TOnLoadEnd                        read FOnLoadEnd                        write FOnLoadEnd;
      /// <summary>
      /// Called when a navigation fails or is canceled. This function may be called
      /// by itself if before commit or in combination with OnLoadStart/OnLoadEnd if
      /// after commit. |errorCode| is the error code number, |errorText| is the
      /// error text and |failedUrl| is the URL that failed to load. See
      /// net\base\net_error_list.h for complete descriptions of the error codes.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_load_handler_capi.h">CEF source file: /include/capi/cef_load_handler_capi.h (cef_load_handler_t)</see></para>
      /// </remarks>
      property OnLoadError                      : TOnLoadError                      read FOnLoadError                      write FOnLoadError;
      /// <summary>
      /// Called when the loading state has changed. This callback will be executed
      /// twice -- once when loading is initiated either programmatically or by user
      /// action, and once when loading is terminated due to completion,
      /// cancellation of failure. It will be called before any calls to OnLoadStart
      /// and after all calls to OnLoadError and/or OnLoadEnd.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_load_handler_capi.h">CEF source file: /include/capi/cef_load_handler_capi.h (cef_load_handler_t)</see></para>
      /// </remarks>
      property OnLoadingStateChange             : TOnLoadingStateChange             read FOnLoadingStateChange             write FOnLoadingStateChange;
      /// <summary>
      /// Called when the browser component is about to loose focus. For instance,
      /// if focus was on the last HTML element and the user pressed the TAB key.
      /// |next| will be true (1) if the browser is giving focus to the next
      /// component and false (0) if the browser is giving focus to the previous
      /// component.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_focus_handler_capi.h">CEF source file: /include/capi/cef_focus_handler_capi.h (cef_focus_handler_t)</see></para>
      /// </remarks>
      property OnTakeFocus                      : TOnTakeFocus                      read FOnTakeFocus                      write FOnTakeFocus;
      /// <summary>
      /// Called when the browser component is requesting focus. |source| indicates
      /// where the focus request is originating from. Return false (0) to allow the
      /// focus to be set or true (1) to cancel setting the focus.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_focus_handler_capi.h">CEF source file: /include/capi/cef_focus_handler_capi.h (cef_focus_handler_t)</see></para>
      /// </remarks>
      property OnSetFocus                       : TOnSetFocus                       read FOnSetFocus                       write FOnSetFocus;
      /// <summary>
      /// Called when the browser component has received focus.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_focus_handler_capi.h">CEF source file: /include/capi/cef_focus_handler_capi.h (cef_focus_handler_t)</see></para>
      /// </remarks>
      property OnGotFocus                       : TOnGotFocus                       read FOnGotFocus                       write FOnGotFocus;
      /// <summary>
      /// Called before a context menu is displayed. |params| provides information
      /// about the context menu state. |model| initially contains the default
      /// context menu. The |model| can be cleared to show no context menu or
      /// modified to show a custom menu. Do not keep references to |params| or
      /// |model| outside of this callback.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_context_menu_handler_capi.h">CEF source file: /include/capi/cef_context_menu_handler_capi.h (cef_context_menu_handler_t)</see></para>
      /// </remarks>
      property OnBeforeContextMenu              : TOnBeforeContextMenu              read FOnBeforeContextMenu              write FOnBeforeContextMenu;
      /// <summary>
      /// Called to allow custom display of the context menu. |params| provides
      /// information about the context menu state. |model| contains the context
      /// menu model resulting from OnBeforeContextMenu. For custom display return
      /// true (1) and execute |callback| either synchronously or asynchronously
      /// with the selected command ID. For default display return false (0). Do not
      /// keep references to |params| or |model| outside of this callback.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_context_menu_handler_capi.h">CEF source file: /include/capi/cef_context_menu_handler_capi.h (cef_context_menu_handler_t)</see></para>
      /// </remarks>
      property OnRunContextMenu                 : TOnRunContextMenu                 read FOnRunContextMenu                 write FOnRunContextMenu;
      /// <summary>
      /// Called to execute a command selected from the context menu. Return true
      /// (1) if the command was handled or false (0) for the default
      /// implementation. See cef_menu_id_t for the command ids that have default
      /// implementations. All user-defined command ids should be between
      /// MENU_ID_USER_FIRST and MENU_ID_USER_LAST. |params| will have the same
      /// values as what was passed to on_before_context_menu(). Do not keep a
      /// reference to |params| outside of this callback.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_context_menu_handler_capi.h">CEF source file: /include/capi/cef_context_menu_handler_capi.h (cef_context_menu_handler_t)</see></para>
      /// </remarks>
      property OnContextMenuCommand             : TOnContextMenuCommand             read FOnContextMenuCommand             write FOnContextMenuCommand;
      /// <summary>
      /// Called when the context menu is dismissed irregardless of whether the menu
      /// was canceled or a command was selected.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_context_menu_handler_capi.h">CEF source file: /include/capi/cef_context_menu_handler_capi.h (cef_context_menu_handler_t)</see></para>
      /// </remarks>
      property OnContextMenuDismissed           : TOnContextMenuDismissed           read FOnContextMenuDismissed           write FOnContextMenuDismissed;
      /// <summary>
      /// Called to allow custom display of the quick menu for a windowless browser.
      /// |location| is the top left corner of the selected region. |size| is the
      /// size of the selected region. |edit_state_flags| is a combination of flags
      /// that represent the state of the quick menu. Return true (1) if the menu
      /// will be handled and execute |callback| either synchronously or
      /// asynchronously with the selected command ID. Return false (0) to cancel
      /// the menu.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_context_menu_handler_capi.h">CEF source file: /include/capi/cef_context_menu_handler_capi.h (cef_context_menu_handler_t)</see></para>
      /// </remarks>
      property OnRunQuickMenu                   : TOnRunQuickMenuEvent              read FOnRunQuickMenu                   write FOnRunQuickMenu;
      /// <summary>
      /// Called to execute a command selected from the quick menu for a windowless
      /// browser. Return true (1) if the command was handled or false (0) for the
      /// default implementation. See cef_menu_id_t for command IDs that have
      /// default implementations.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_context_menu_handler_capi.h">CEF source file: /include/capi/cef_context_menu_handler_capi.h (cef_context_menu_handler_t)</see></para>
      /// </remarks>
      property OnQuickMenuCommand               : TOnQuickMenuCommandEvent          read FOnQuickMenuCommand               write FOnQuickMenuCommand;
      /// <summary>
      /// Called when the quick menu for a windowless browser is dismissed
      /// irregardless of whether the menu was canceled or a command was selected.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_context_menu_handler_capi.h">CEF source file: /include/capi/cef_context_menu_handler_capi.h (cef_context_menu_handler_t)</see></para>
      /// </remarks>
      property OnQuickMenuDismissed             : TOnQuickMenuDismissedEvent        read FOnQuickMenuDismissed             write FOnQuickMenuDismissed;
      /// <summary>
      /// Called before a keyboard event is sent to the renderer. |event| contains
      /// information about the keyboard event. |os_event| is the operating system
      /// event message, if any. Return true (1) if the event was handled or false
      /// (0) otherwise. If the event will be handled in on_key_event() as a
      /// keyboard shortcut set |is_keyboard_shortcut| to true (1) and return false
      /// (0).
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_keyboard_handler_capi.h">CEF source file: /include/capi/cef_keyboard_handler_capi.h (cef_keyboard_handler_t)</see></para>
      /// </remarks>
      property OnPreKeyEvent                    : TOnPreKeyEvent                    read FOnPreKeyEvent                    write FOnPreKeyEvent;
      /// <summary>
      /// Called after the renderer and JavaScript in the page has had a chance to
      /// handle the event. |event| contains information about the keyboard event.
      /// |os_event| is the operating system event message, if any. Return true (1)
      /// if the keyboard event was handled or false (0) otherwise.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_keyboard_handler_capi.h">CEF source file: /include/capi/cef_keyboard_handler_capi.h (cef_keyboard_handler_t)</see></para>
      /// </remarks>
      property OnKeyEvent                       : TOnKeyEvent                       read FOnKeyEvent                       write FOnKeyEvent;
      /// <summary>
      /// Called when a frame's address has changed.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_display_handler_capi.h">CEF source file: /include/capi/cef_display_handler_capi.h (cef_display_handler_t)</see></para>
      /// </remarks>
      property OnAddressChange                  : TOnAddressChange                  read FOnAddressChange                  write FOnAddressChange;
      /// <summary>
      /// Called when the page title changes.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_display_handler_capi.h">CEF source file: /include/capi/cef_display_handler_capi.h (cef_display_handler_t)</see></para>
      /// </remarks>
      property OnTitleChange                    : TOnTitleChange                    read FOnTitleChange                    write FOnTitleChange;
      /// <summary>
      /// Called when the page icon changes.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_display_handler_capi.h">CEF source file: /include/capi/cef_display_handler_capi.h (cef_display_handler_t)</see></para>
      /// </remarks>
      property OnFavIconUrlChange               : TOnFavIconUrlChange               read FOnFavIconUrlChange               write FOnFavIconUrlChange;
      /// <summary>
      /// Called when web content in the page has toggled fullscreen mode. If
      /// |fullscreen| is true (1) the content will automatically be sized to fill
      /// the browser content area. If |fullscreen| is false (0) the content will
      /// automatically return to its original size and position. With the Alloy
      /// runtime the client is responsible for triggering the fullscreen transition
      /// (for example, by calling ICefWindow.SetFullscreen when using Views).
      /// With the Chrome runtime the fullscreen transition will be triggered
      /// automatically. The ICefWindowDelegate.OnWindowFullscreenTransition
      /// function will be called during the fullscreen transition for notification
      /// purposes.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_display_handler_capi.h">CEF source file: /include/capi/cef_display_handler_capi.h (cef_display_handler_t)</see></para>
      /// </remarks>
      property OnFullScreenModeChange           : TOnFullScreenModeChange           read FOnFullScreenModeChange           write FOnFullScreenModeChange;
      /// <summary>
      /// Called when the browser is about to display a tooltip. |text| contains the
      /// text that will be displayed in the tooltip. To handle the display of the
      /// tooltip yourself return true (1). Otherwise, you can optionally modify
      /// |text| and then return false (0) to allow the browser to display the
      /// tooltip. When window rendering is disabled the application is responsible
      /// for drawing tooltips and the return value is ignored.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_display_handler_capi.h">CEF source file: /include/capi/cef_display_handler_capi.h (cef_display_handler_t)</see></para>
      /// </remarks>
      property OnTooltip                        : TOnTooltip                        read FOnTooltip                        write FOnTooltip;
      /// <summary>
      /// Called when the browser receives a status message. |value| contains the
      /// text that will be displayed in the status message.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_display_handler_capi.h">CEF source file: /include/capi/cef_display_handler_capi.h (cef_display_handler_t)</see></para>
      /// </remarks>
      property OnStatusMessage                  : TOnStatusMessage                  read FOnStatusMessage                  write FOnStatusMessage;
      /// <summary>
      /// Called to display a console message. Return true (1) to stop the message
      /// from being output to the console.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_display_handler_capi.h">CEF source file: /include/capi/cef_display_handler_capi.h (cef_display_handler_t)</see></para>
      /// </remarks>
      property OnConsoleMessage                 : TOnConsoleMessage                 read FOnConsoleMessage                 write FOnConsoleMessage;
      /// <summary>
      /// Called when auto-resize is enabled via
      /// cef_browser_host_t::SetAutoResizeEnabled and the contents have auto-
      /// resized. |new_size| will be the desired size in view coordinates. Return
      /// true (1) if the resize was handled or false (0) for default handling.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_display_handler_capi.h">CEF source file: /include/capi/cef_display_handler_capi.h (cef_display_handler_t)</see></para>
      /// </remarks>
      property OnAutoResize                     : TOnAutoResize                     read FOnAutoResize                     write FOnAutoResize;
      /// <summary>
      /// Called when the overall page loading progress has changed. |progress|
      /// ranges from 0.0 to 1.0.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_display_handler_capi.h">CEF source file: /include/capi/cef_display_handler_capi.h (cef_display_handler_t)</see></para>
      /// </remarks>
      property OnLoadingProgressChange          : TOnLoadingProgressChange          read FOnLoadingProgressChange          write FOnLoadingProgressChange;
      /// <summary>
      /// Called when the browser's cursor has changed. If |type| is CT_CUSTOM then
      /// |custom_cursor_info| will be populated with the custom cursor information.
      /// Return true (1) if the cursor change was handled or false (0) for default
      /// handling.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_display_handler_capi.h">CEF source file: /include/capi/cef_display_handler_capi.h (cef_display_handler_t)</see></para>
      /// </remarks>
      property OnCursorChange                   : TOnCursorChange                   read FOnCursorChange                   write FOnCursorChange;
      /// <summary>
      /// Called when the browser's access to an audio and/or video source has
      /// changed.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_display_handler_capi.h">CEF source file: /include/capi/cef_display_handler_capi.h (cef_display_handler_t)</see></para>
      /// </remarks>
      property OnMediaAccessChange              : TOnMediaAccessChange              read FOnMediaAccessChange              write FOnMediaAccessChange;

      /// <summary>
      /// Called before a download begins in response to a user-initiated action
      /// (e.g. alt + link click or link click that returns a `Content-Disposition:
      /// attachment` response from the server). |url| is the target download URL
      /// and |request_function| is the target function (GET, POST, etc). Return
      /// true (1) to proceed with the download or false (0) to cancel the download.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_download_handler_capi.h">CEF source file: /include/capi/cef_download_handler_capi.h (cef_download_handler_t)</see></para>
      /// </remarks>
      property OnCanDownload                    : TOnCanDownloadEvent               read FOnCanDownload                    write FOnCanDownload;
      /// <summary>
      /// Called before a download begins. |suggested_name| is the suggested name
      /// for the download file. By default the download will be canceled. Execute
      /// |callback| either asynchronously or in this function to continue the
      /// download if desired. Do not keep a reference to |download_item| outside of
      /// this function.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_download_handler_capi.h">CEF source file: /include/capi/cef_download_handler_capi.h (cef_download_handler_t)</see></para>
      /// </remarks>
      property OnBeforeDownload                 : TOnBeforeDownload                 read FOnBeforeDownload                 write FOnBeforeDownload;
      /// <summary>
      /// Called when a download's status or progress information has been updated.
      /// This may be called multiple times before and after OnBeforeDownload.
      /// Execute |callback| either asynchronously or in this function to cancel the
      /// download if desired. Do not keep a reference to |download_item| outside of
      /// this function.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_download_handler_capi.h">CEF source file: /include/capi/cef_download_handler_capi.h (cef_download_handler_t)</see></para>
      /// </remarks>
      property OnDownloadUpdated                : TOnDownloadUpdated                read FOnDownloadUpdated                write FOnDownloadUpdated;
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
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_jsdialog_handler_capi.h">CEF source file: /include/capi/cef_jsdialog_handler_capi.h (cef_jsdialog_handler_t)</see></para>
      /// </remarks>
      property OnJsdialog                       : TOnJsdialog                       read FOnJsdialog                       write FOnJsdialog;
      /// <summary>
      /// Called to run a dialog asking the user if they want to leave a page.
      /// Return false (0) to use the default dialog implementation. Return true (1)
      /// if the application will use a custom dialog or if the callback has been
      /// executed immediately. Custom dialogs may be either modal or modeless. If a
      /// custom dialog is used the application must execute |callback| once the
      /// custom dialog is dismissed.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_jsdialog_handler_capi.h">CEF source file: /include/capi/cef_jsdialog_handler_capi.h (cef_jsdialog_handler_t)</see></para>
      /// </remarks>
      property OnBeforeUnloadDialog             : TOnBeforeUnloadDialog             read FOnBeforeUnloadDialog             write FOnBeforeUnloadDialog;
      /// <summary>
      /// Called to cancel any pending dialogs and reset any saved dialog state.
      /// Will be called due to events like page navigation irregardless of whether
      /// any dialogs are currently pending.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_jsdialog_handler_capi.h">CEF source file: /include/capi/cef_jsdialog_handler_capi.h (cef_jsdialog_handler_t)</see></para>
      /// </remarks>
      property OnResetDialogState               : TOnResetDialogState               read FOnResetDialogState               write FOnResetDialogState;
      /// <summary>
      /// Called when the dialog is closed.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_jsdialog_handler_capi.h">CEF source file: /include/capi/cef_jsdialog_handler_capi.h (cef_jsdialog_handler_t)</see></para>
      /// </remarks>
      property OnDialogClosed                   : TOnDialogClosed                   read FOnDialogClosed                   write FOnDialogClosed;
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
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_life_span_handler_capi.h">CEF source file: /include/capi/cef_life_span_handler_capi.h (cef_life_span_handler_t)</see></para>
      /// </remarks>
      property OnBeforePopup                    : TOnBeforePopup                    read FOnBeforePopup                    write FOnBeforePopup;
      /// <summary>
      /// Called after a new browser is created. It is now safe to begin performing
      /// actions with |browser|. ICefFrameHandler callbacks related to initial
      /// main frame creation will arrive before this callback. See
      /// ICefFrameHandler documentation for additional usage information.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_life_span_handler_capi.h">CEF source file: /include/capi/cef_life_span_handler_capi.h (cef_life_span_handler_t)</see></para>
      /// </remarks>
      property OnAfterCreated                   : TOnAfterCreated                   read FOnAfterCreated                   write FOnAfterCreated;
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
      /// and OnClose() documentation for additional usage information.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_life_span_handler_capi.h">CEF source file: /include/capi/cef_life_span_handler_capi.h (cef_life_span_handler_t)</see></para>
      /// </remarks>
      property OnBeforeClose                    : TOnBeforeClose                    read FOnBeforeClose                    write FOnBeforeClose;
      /// <summary>
      /// <para>Called when a browser has recieved a request to close. This may result
      /// directly from a call to ICefBrowserHost.*CloseBrowser or indirectly
      /// if the browser is parented to a top-level window created by CEF and the
      /// user attempts to close that window (by clicking the 'X', for example). The
      /// OnClose function will be called after the JavaScript 'onunload' event
      /// has been fired.</para>
      ///
      /// <para>An application should handle top-level owner window close notifications by
      /// calling ICefBrowserHost.TryCloseBrowser or
      /// ICefBrowserHost.CloseBrowser(false) instead of allowing the window
      /// to close immediately (see the examples below). This gives CEF an
      /// opportunity to process the 'onbeforeunload' event and optionally cancel
      /// the close before OnClose is called.</para>
      ///
      /// <para>When windowed rendering is enabled CEF will internally create a window or
      /// view to host the browser. In that case returning false (0) from OnClose()
      /// will send the standard close notification to the browser's top-level owner
      /// window (e.g. WM_CLOSE on Windows, performClose: on OS X, "delete_event" on
      /// Linux or ICefWindowDelegate.CanClose callback from Views). If the
      /// browser's host window/view has already been destroyed (via view hierarchy
      /// tear-down, for example) then OnClose() will not be called for that
      /// browser since is no longer possible to cancel the close.</para>
      ///
      /// <para>When windowed rendering is disabled returning false (0) from OnClose()
      /// will cause the browser object to be destroyed immediately.</para>
      ///
      /// <para>If the browser's top-level owner window requires a non-standard close
      /// notification then send that notification from OnClose() and return true.</para>
      ///
      /// <para>The ICefLifeSpanHandler.OnBeforeClose function will be called
      /// after OnClose() (if OnClose() is called) and immediately before the
      /// browser object is destroyed. The application should only exit after
      /// OnBeforeClose() has been called for all existing browsers.</para>
      ///
      /// <para>The below examples describe what should happen during window close when
      /// the browser is parented to an application-provided top-level window.</para>
      ///
      /// <para>Example 1: Using ICefBrowserHost.TryCloseBrowser(). This is
      /// recommended for clients using standard close handling and windows created
      /// on the browser process UI thread.</para>
      /// <code>
      /// 1.  User clicks the window close button which sends a close notification
      ///     to the application's top-level window.
      /// 2.  Application's top-level window receives the close notification and
      ///     calls TryCloseBrowser() (which internally calls CloseBrowser(false)).
      ///     TryCloseBrowser() returns false so the client cancels the window
      ///     close.
      /// 3.  JavaScript 'onbeforeunload' handler executes and shows the close
      ///     confirmation dialog (which can be overridden via
      ///     ICefJSDialogHandler.OnBeforeUnloadDialog()).
      /// 4.  User approves the close.
      /// 5.  JavaScript 'onunload' handler executes.
      /// 6.  CEF sends a close notification to the application's top-level window
      ///     (because OnClose() returned false by default).
      /// 7.  Application's top-level window receives the close notification and
      ///     calls TryCloseBrowser(). TryCloseBrowser() returns true so the client
      ///     allows the window close.
      /// 8.  Application's top-level window is destroyed.
      /// 9.  Application's OnBeforeClose() handler is called and the browser object is destroyed.
      /// 10. Application exits by calling cef_quit_message_loop() if no other browsers exist.
      /// </code>
      /// <para>Example 2: Using ICefBrowserHost::CloseBrowser(false) and
      /// implementing the OnClose() callback. This is recommended for clients
      /// using non-standard close handling or windows that were not created on the
      /// browser process UI thread.</para>
      /// <code>
      /// 1.  User clicks the window close button which sends a close notification
      ///     to the application's top-level window.
      /// 2.  Application's top-level window receives the close notification and:
      ///     A. Calls ICefBrowserHost.CloseBrowser(false).
      ///     B. Cancels the window close.
      /// 3.  JavaScript 'onbeforeunload' handler executes and shows the close
      ///     confirmation dialog (which can be overridden via
      ///     ICefJSDialogHandler.OnBeforeUnloadDialog()).
      /// 4.  User approves the close.
      /// 5.  JavaScript 'onunload' handler executes.
      /// 6.  Application's OnClose() handler is called. Application will:
      ///     A. Set a flag to indicate that the next close attempt will be allowed.
      ///     B. Return false.
      /// 7.  CEF sends an close notification to the application's top-level window.
      /// 8.  Application's top-level window receives the close notification and
      ///     allows the window to close based on the flag from #6B.
      /// 9.  Application's top-level window is destroyed.
      /// 10. Application's OnBeforeClose() handler is called and the browser object is destroyed.
      /// 11. Application exits by calling cef_quit_message_loop() if no other browsers exist.
      /// </code>
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_life_span_handler_capi.h">CEF source file: /include/capi/cef_life_span_handler_capi.h (cef_life_span_handler_t)</see></para>
      /// </remarks>
      property OnClose                          : TOnClose                          read FOnClose                          write FOnClose;
      /// <summary>
      /// <para>Called on the UI thread before browser navigation. Return true (1) to
      /// cancel the navigation or false (0) to allow the navigation to proceed. The
      /// |request| object cannot be modified in this callback.</para>
      /// <para>ICefLoadHandler.OnLoadingStateChange will be called twice in all
      /// cases. If the navigation is allowed ICefLoadHandler.OnLoadStart and
      /// ICefLoadHandler.OnLoadEnd will be called. If the navigation is
      /// canceled ICefLoadHandler.OnLoadError will be called with an
      /// |errorCode| value of ERR_ABORTED. The |user_gesture| value will be true
      /// (1) if the browser navigated via explicit user gesture (e.g. clicking a
      /// link) or false (0) if it navigated automatically (e.g. via the
      /// DomContentLoaded event).</para>
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_request_handler_capi.h">CEF source file: /include/capi/cef_request_handler_capi.h (cef_request_handler_t)</see></para>
      /// </remarks>
      property OnBeforeBrowse                      : TOnBeforeBrowse                   read FOnBeforeBrowse                      write FOnBeforeBrowse;
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
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_request_handler_capi.h">CEF source file: /include/capi/cef_request_handler_capi.h (cef_request_handler_t)</see></para>
      /// </remarks>
      property OnOpenUrlFromTab                    : TOnOpenUrlFromTab                 read FOnOpenUrlFromTab                    write FOnOpenUrlFromTab;
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
      /// <remarks>
      /// <para>This event will be called on the browser process CEF IO thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_request_handler_capi.h">CEF source file: /include/capi/cef_request_handler_capi.h (cef_request_handler_t)</see></para>
      /// </remarks>
      property OnGetAuthCredentials                : TOnGetAuthCredentials             read FOnGetAuthCredentials                write FOnGetAuthCredentials;
      /// <summary>
      /// Called on the UI thread to handle requests for URLs with an invalid SSL
      /// certificate. Return true (1) and call ICefCallback functions either in
      /// this function or at a later time to continue or cancel the request. Return
      /// false (0) to cancel the request immediately. If
      /// TCefSettings.ignore_certificate_errors is set all invalid certificates
      /// will be accepted without calling this function.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_request_handler_capi.h">CEF source file: /include/capi/cef_request_handler_capi.h (cef_request_handler_t)</see></para>
      /// </remarks>
      property OnCertificateError                  : TOnCertificateError               read FOnCertificateError                  write FOnCertificateError;
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
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_request_handler_capi.h">CEF source file: /include/capi/cef_request_handler_capi.h (cef_request_handler_t)</see></para>
      /// </remarks>
      property OnSelectClientCertificate           : TOnSelectClientCertificate        read FOnSelectClientCertificate           write FOnSelectClientCertificate;
      /// <summary>
      /// Called on the browser process UI thread when the render view associated
      /// with |browser| is ready to receive/handle IPC messages in the render
      /// process.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_request_handler_capi.h">CEF source file: /include/capi/cef_request_handler_capi.h (cef_request_handler_t)</see></para>
      /// </remarks>
      property OnRenderViewReady                   : TOnRenderViewReady                read FOnRenderViewReady                   write FOnRenderViewReady;
      /// <summary>
      /// Called on the browser process UI thread when the render process terminates
      /// unexpectedly. |status| indicates how the process terminated.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_request_handler_capi.h">CEF source file: /include/capi/cef_request_handler_capi.h (cef_request_handler_t)</see></para>
      /// </remarks>
      property OnRenderProcessTerminated           : TOnRenderProcessTerminated        read FOnRenderProcessTerminated           write FOnRenderProcessTerminated;
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
      /// <remarks>
      /// <para>This event will be called on the browser process CEF IO thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_request_handler_capi.h">CEF source file: /include/capi/cef_request_handler_capi.h (cef_request_handler_t)</see></para>
      /// </remarks>
      property OnGetResourceRequestHandler_ReqHdlr : TOnGetResourceRequestHandler      read FOnGetResourceRequestHandler_ReqHdlr write FOnGetResourceRequestHandler_ReqHdlr;
      /// <summary>
      /// Called on the browser process UI thread when the window.document object of
      /// the main frame has been created.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_request_handler_capi.h">CEF source file: /include/capi/cef_request_handler_capi.h (cef_request_handler_t)</see></para>
      /// </remarks>
      property OnDocumentAvailableInMainFrame      : TOnDocumentAvailableInMainFrame   read FOnDocumentAvailableInMainFrame      write FOnDocumentAvailableInMainFrame;
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
      /// <remarks>
      /// <para>This event will be called on the browser process CEF IO thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_resource_request_handler_capi.h">CEF source file: /include/capi/cef_resource_request_handler_capi.h (cef_resource_request_handler_t)</see></para>
      /// </remarks>
      property OnBeforeResourceLoad             : TOnBeforeResourceLoad             read FOnBeforeResourceLoad             write FOnBeforeResourceLoad;
      /// <summary>
      /// Called on the IO thread before a resource is loaded. The |browser| and
      /// |frame| values represent the source of the request, and may be NULL for
      /// requests originating from service workers or ICefUrlRequest. To allow
      /// the resource to load using the default network loader return NULL. To
      /// specify a handler for the resource return a ICefResourceHandler object.
      /// The |request| object cannot not be modified in this callback.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF IO thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_resource_request_handler_capi.h">CEF source file: /include/capi/cef_resource_request_handler_capi.h (cef_resource_request_handler_t)</see></para>
      /// </remarks>
      property OnGetResourceHandler             : TOnGetResourceHandler             read FOnGetResourceHandler             write FOnGetResourceHandler;
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
      /// <remarks>
      /// <para>This event will be called on the browser process CEF IO thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_resource_request_handler_capi.h">CEF source file: /include/capi/cef_resource_request_handler_capi.h (cef_resource_request_handler_t)</see></para>
      /// </remarks>
      property OnResourceRedirect               : TOnResourceRedirect               read FOnResourceRedirect               write FOnResourceRedirect;
      /// <summary>
      /// <para>Called on the IO thread when a resource response is received. The
      /// |browser| and |frame| values represent the source of the request, and may
      /// be NULL for requests originating from service workers or ICefUrlRequest.
      /// To allow the resource load to proceed without modification return false
      /// (0). To redirect or retry the resource load optionally modify |request|
      /// and return true (1). Modification of the request URL will be treated as a
      /// redirect. Requests handled using the default network loader cannot be
      /// redirected in this callback. The |response| object cannot be modified in
      /// this callback.</para>
      /// <para>WARNING: Redirecting using this function is deprecated. Use
      /// OnBeforeResourceLoad or GetResourceHandler to perform redirects.</para>
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF IO thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_resource_request_handler_capi.h">CEF source file: /include/capi/cef_resource_request_handler_capi.h (cef_resource_request_handler_t)</see></para>
      /// </remarks>
      property OnResourceResponse               : TOnResourceResponse               read FOnResourceResponse               write FOnResourceResponse;
      /// <summary>
      /// Called on the IO thread to optionally filter resource response content.
      /// The |browser| and |frame| values represent the source of the request, and
      /// may be NULL for requests originating from service workers or
      /// ICefUrlRequest. |request| and |response| represent the request and
      /// response respectively and cannot be modified in this callback.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF IO thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_resource_request_handler_capi.h">CEF source file: /include/capi/cef_resource_request_handler_capi.h (cef_resource_request_handler_t)</see></para>
      /// </remarks>
      property OnGetResourceResponseFilter      : TOnGetResourceResponseFilter      read FOnGetResourceResponseFilter      write FOnGetResourceResponseFilter;
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
      /// <remarks>
      /// <para>This event will be called on the browser process CEF IO thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_resource_request_handler_capi.h">CEF source file: /include/capi/cef_resource_request_handler_capi.h (cef_resource_request_handler_t)</see></para>
      /// </remarks>
      property OnResourceLoadComplete           : TOnResourceLoadComplete           read FOnResourceLoadComplete           write FOnResourceLoadComplete;
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
      /// <remarks>
      /// <para>This event will be called on the browser process CEF IO thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_resource_request_handler_capi.h">CEF source file: /include/capi/cef_resource_request_handler_capi.h (cef_resource_request_handler_t)</see></para>
      /// </remarks>
      property OnProtocolExecution              : TOnProtocolExecution              read FOnProtocolExecution              write FOnProtocolExecution;
      /// <summary>
      /// Called on the IO thread before a resource request is sent. The |browser|
      /// and |frame| values represent the source of the request, and may be NULL
      /// for requests originating from service workers or ICefUrlRequest.
      /// |request| cannot be modified in this callback. Return true (1) if the
      /// specified cookie can be sent with the request or false (0) otherwise.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF IO thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_resource_request_handler_capi.h">CEF source file: /include/capi/cef_resource_request_handler_capi.h (cef_cookie_access_filter_t)</see></para>
      /// </remarks>
      property OnCanSendCookie                  : TOnCanSendCookie                  read FOnCanSendCookie                  write FOnCanSendCookie;
      /// <summary>
      /// Called on the IO thread after a resource response is received. The
      /// |browser| and |frame| values represent the source of the request, and may
      /// be NULL for requests originating from service workers or ICefUrlRequest.
      /// |request| cannot be modified in this callback. Return true (1) if the
      /// specified cookie returned with the response can be saved or false (0)
      /// otherwise.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF IO thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_resource_request_handler_capi.h">CEF source file: /include/capi/cef_resource_request_handler_capi.h (cef_cookie_access_filter_t)</see></para>
      /// </remarks>
      property OnCanSaveCookie                  : TOnCanSaveCookie                  read FOnCanSaveCookie                  write FOnCanSaveCookie;
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
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_dialog_handler_capi.h">CEF source file: /include/capi/cef_dialog_handler_capi.h (cef_dialog_handler_t)</see></para>
      /// </remarks>
      property OnFileDialog                     : TOnFileDialog                     read FOnFileDialog                     write FOnFileDialog;
      /// <summary>
      /// Return the handler for accessibility notifications. If no handler is
      /// provided the default implementation will be used.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_render_handler_capi.h">CEF source file: /include/capi/cef_render_handler_capi.h (cef_render_handler_t)</see></para>
      /// </remarks>
      property OnGetAccessibilityHandler        : TOnGetAccessibilityHandler        read FOnGetAccessibilityHandler        write FOnGetAccessibilityHandler;
      /// <summary>
      /// Called to retrieve the root window rectangle in screen DIP coordinates.
      /// Return true (1) if the rectangle was provided. If this function returns
      /// false (0) the rectangle from OnGetViewRect will be used.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_render_handler_capi.h">CEF source file: /include/capi/cef_render_handler_capi.h (cef_render_handler_t)</see></para>
      /// </remarks>
      property OnGetRootScreenRect              : TOnGetRootScreenRect              read FOnGetRootScreenRect              write FOnGetRootScreenRect;
      /// <summary>
      /// Called to retrieve the view rectangle in screen DIP coordinates. This
      /// function must always provide a non-NULL rectangle.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_render_handler_capi.h">CEF source file: /include/capi/cef_render_handler_capi.h (cef_render_handler_t)</see></para>
      /// </remarks>
      property OnGetViewRect                    : TOnGetViewRect                    read FOnGetViewRect                    write FOnGetViewRect;
      /// <summary>
      /// Called to retrieve the translation from view DIP coordinates to screen
      /// coordinates. Windows/Linux should provide screen device (pixel)
      /// coordinates and MacOS should provide screen DIP coordinates. Return true
      /// (1) if the requested coordinates were provided.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_render_handler_capi.h">CEF source file: /include/capi/cef_render_handler_capi.h (cef_render_handler_t)</see></para>
      /// </remarks>
      property OnGetScreenPoint                 : TOnGetScreenPoint                 read FOnGetScreenPoint                 write FOnGetScreenPoint;
      /// <summary>
      /// Called to allow the client to fill in the TCefScreenInfo object with
      /// appropriate values. Return true (1) if the |screen_info| structure has
      /// been modified.
      /// If the screen info rectangle is left NULL the rectangle from OnGetViewRect
      /// will be used. If the rectangle is still NULL or invalid popups may not be
      /// drawn correctly.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_render_handler_capi.h">CEF source file: /include/capi/cef_render_handler_capi.h (cef_render_handler_t)</see></para>
      /// </remarks>
      property OnGetScreenInfo                  : TOnGetScreenInfo                  read FOnGetScreenInfo                  write FOnGetScreenInfo;
      /// <summary>
      /// Called when the browser wants to show or hide the popup widget. The popup
      /// should be shown if |show| is true (1) and hidden if |show| is false (0).
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_render_handler_capi.h">CEF source file: /include/capi/cef_render_handler_capi.h (cef_render_handler_t)</see></para>
      /// </remarks>
      property OnPopupShow                      : TOnPopupShow                      read FOnPopupShow                      write FOnPopupShow;
      /// <summary>
      /// Called when the browser wants to move or resize the popup widget. |rect|
      /// contains the new location and size in view coordinates.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_render_handler_capi.h">CEF source file: /include/capi/cef_render_handler_capi.h (cef_render_handler_t)</see></para>
      /// </remarks>
      property OnPopupSize                      : TOnPopupSize                      read FOnPopupSize                      write FOnPopupSize;
      /// <summary>
      /// Called when an element should be painted. Pixel values passed to this
      /// function are scaled relative to view coordinates based on the value of
      /// TCefScreenInfo.device_scale_factor returned from OnGetScreenInfo. |type|
      /// indicates whether the element is the view or the popup widget. |buffer|
      /// contains the pixel data for the whole image. |dirtyRects| contains the set
      /// of rectangles in pixel coordinates that need to be repainted. |buffer|
      /// will be |width|*|height|*4 bytes in size and represents a BGRA image with
      /// an upper-left origin. This function is only called when
      /// TCefWindowInfo.shared_texture_enabled is set to false (0).
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_render_handler_capi.h">CEF source file: /include/capi/cef_render_handler_capi.h (cef_render_handler_t)</see></para>
      /// </remarks>
      property OnPaint                          : TOnPaint                          read FOnPaint                          write FOnPaint;
      /// <summary>
      /// Called when an element has been rendered to the shared texture handle.
      /// |type| indicates whether the element is the view or the popup widget.
      /// |dirtyRects| contains the set of rectangles in pixel coordinates that need
      /// to be repainted. |shared_handle| is the handle for a D3D11 Texture2D that
      /// can be accessed via ID3D11Device using the OpenSharedResource function.
      /// This function is only called when TCefWindowInfo.shared_texture_enabled
      /// is set to true (1), and is currently only supported on Windows.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_render_handler_capi.h">CEF source file: /include/capi/cef_render_handler_capi.h (cef_render_handler_t)</see></para>
      /// </remarks>
      property OnAcceleratedPaint               : TOnAcceleratedPaint               read FOnAcceleratedPaint               write FOnAcceleratedPaint;
      /// <summary>
      /// Called to retrieve the size of the touch handle for the specified
      /// |orientation|.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_render_handler_capi.h">CEF source file: /include/capi/cef_render_handler_capi.h (cef_render_handler_t)</see></para>
      /// </remarks>
      property OnGetTouchHandleSize             : TOnGetTouchHandleSize             read FOnGetTouchHandleSize             write FOnGetTouchHandleSize;
      /// <summary>
      /// Called when touch handle state is updated. The client is responsible for
      /// rendering the touch handles.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_render_handler_capi.h">CEF source file: /include/capi/cef_render_handler_capi.h (cef_render_handler_t)</see></para>
      /// </remarks>
      property OnTouchHandleStateChanged        : TOnTouchHandleStateChanged        read FOnTouchHandleStateChanged        write FOnTouchHandleStateChanged;
      /// <summary>
      /// <para>Called when the user starts dragging content in the web view. Contextual
      /// information about the dragged content is supplied by |drag_data|. (|x|,
      /// |y|) is the drag start location in screen coordinates. OS APIs that run a
      /// system message loop may be used within the StartDragging call.</para>
      /// <para>Return false (0) to abort the drag operation. Don't call any of
      /// ICefBrowserHost.DragSource*Ended* functions after returning false (0).</para>
      /// <para>Return true (1) to handle the drag operation. Call
      /// ICefBrowserHost.DragSourceEndedAt and DragSourceSystemDragEnded either
      /// synchronously or asynchronously to inform the web view that the drag
      /// operation has ended.</para>
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_render_handler_capi.h">CEF source file: /include/capi/cef_render_handler_capi.h (cef_render_handler_t)</see></para>
      /// </remarks>
      property OnStartDragging                  : TOnStartDragging                  read FOnStartDragging                  write FOnStartDragging;
      /// <summary>
      /// Called when the web view wants to update the mouse cursor during a drag &
      /// drop operation. |operation| describes the allowed operation (none, move,
      /// copy, link).
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_render_handler_capi.h">CEF source file: /include/capi/cef_render_handler_capi.h (cef_render_handler_t)</see></para>
      /// </remarks>
      property OnUpdateDragCursor               : TOnUpdateDragCursor               read FOnUpdateDragCursor               write FOnUpdateDragCursor;
      /// <summary>
      /// Called when the scroll offset has changed.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_render_handler_capi.h">CEF source file: /include/capi/cef_render_handler_capi.h (cef_render_handler_t)</see></para>
      /// </remarks>
      property OnScrollOffsetChanged            : TOnScrollOffsetChanged            read FOnScrollOffsetChanged            write FOnScrollOffsetChanged;
      /// <summary>
      /// Called when the IME composition range has changed. |selected_range| is the
      /// range of characters that have been selected. |character_bounds| is the
      /// bounds of each character in view coordinates.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_render_handler_capi.h">CEF source file: /include/capi/cef_render_handler_capi.h (cef_render_handler_t)</see></para>
      /// </remarks>
      property OnIMECompositionRangeChanged     : TOnIMECompositionRangeChanged     read FOnIMECompositionRangeChanged     write FOnIMECompositionRangeChanged;
      /// <summary>
      /// Called when text selection has changed for the specified |browser|.
      /// |selected_text| is the currently selected text and |selected_range| is the
      /// character range.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_render_handler_capi.h">CEF source file: /include/capi/cef_render_handler_capi.h (cef_render_handler_t)</see></para>
      /// </remarks>
      property OnTextSelectionChanged           : TOnTextSelectionChanged           read FOnTextSelectionChanged           write FOnTextSelectionChanged;
      /// <summary>
      /// Called when an on-screen keyboard should be shown or hidden for the
      /// specified |browser|. |input_mode| specifies what kind of keyboard should
      /// be opened. If |input_mode| is CEF_TEXT_INPUT_MODE_NONE, any existing
      /// keyboard for this browser should be hidden.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_render_handler_capi.h">CEF source file: /include/capi/cef_render_handler_capi.h (cef_render_handler_t)</see></para>
      /// </remarks>
      property OnVirtualKeyboardRequested       : TOnVirtualKeyboardRequested       read FOnVirtualKeyboardRequested       write FOnVirtualKeyboardRequested;
      /// <summary>
      /// Called when an external drag event enters the browser window. |dragData|
      /// contains the drag event data and |mask| represents the type of drag
      /// operation. Return false (0) for default drag handling behavior or true (1)
      /// to cancel the drag event.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_drag_handler_capi.h">CEF source file: /include/capi/cef_drag_handler_capi.h (cef_drag_handler_t)</see></para>
      /// </remarks>
      property OnDragEnter                      : TOnDragEnter                      read FOnDragEnter                      write FOnDragEnter;
      /// <summary>
      /// Called whenever draggable regions for the browser window change. These can
      /// be specified using the '-webkit-app-region: drag/no-drag' CSS-property. If
      /// draggable regions are never defined in a document this function will also
      /// never be called. If the last draggable region is removed from a document
      /// this function will be called with an NULL vector.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_drag_handler_capi.h">CEF source file: /include/capi/cef_drag_handler_capi.h (cef_drag_handler_t)</see></para>
      /// </remarks>
      property OnDraggableRegionsChanged        : TOnDraggableRegionsChanged        read FOnDraggableRegionsChanged        write FOnDraggableRegionsChanged;
      /// <summary>
      /// Called to report find results returned by ICefBrowserHost.find().
      /// |identifer| is a unique incremental identifier for the currently active
      /// search, |count| is the number of matches currently identified,
      /// |selectionRect| is the location of where the match was found (in window
      /// coordinates), |activeMatchOrdinal| is the current position in the search
      /// results, and |finalUpdate| is true (1) if this is the last find
      /// notification.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_find_handler_capi.h">CEF source file: /include/capi/cef_find_handler_capi.h (cef_find_handler_t)</see></para>
      /// </remarks>
      property OnFindResult                     : TOnFindResult                     read FOnFindResult                     write FOnFindResult;
      /// <summary>
      /// Called on the browser process UI thread immediately after the request
      /// context has been initialized.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_request_context_handler_capi.h">CEF source file: /include/capi/cef_request_context_handler_capi.h (cef_request_context_handler_t)</see></para>
      /// </remarks>
      property OnRequestContextInitialized            : TOnRequestContextInitialized      read FOnRequestContextInitialized            write FOnRequestContextInitialized;
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
      /// <remarks>
      /// <para>This event will be called on the browser process CEF IO thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_request_context_handler_capi.h">CEF source file: /include/capi/cef_request_context_handler_capi.h (cef_request_context_handler_t)</see></para>
      /// </remarks>
      property OnGetResourceRequestHandler_ReqCtxHdlr : TOnGetResourceRequestHandler      read FOnGetResourceRequestHandler_ReqCtxHdlr write FOnGetResourceRequestHandler_ReqCtxHdlr;
      /// <summary>
      /// The list of available media sinks has changed or
      /// ICefMediaRouter.NotifyCurrentSinks was called.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_media_router_capi.h">CEF source file: /include/capi/cef_media_router_capi.h (cef_media_observer_t)</see></para>
      /// </remarks>
      property OnSinks                                : TOnSinksEvent                     read FOnSinks                                write FOnSinks;
      /// <summary>
      /// The list of available media routes has changed or
      /// ICefMediaRouter.NotifyCurrentRoutes was called.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_media_router_capi.h">CEF source file: /include/capi/cef_media_router_capi.h (cef_media_observer_t)</see></para>
      /// </remarks>
      property OnRoutes                               : TOnRoutesEvent                    read FOnRoutes                               write FOnRoutes;
      /// <summary>
      /// The connection state of |route| has changed.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_media_router_capi.h">CEF source file: /include/capi/cef_media_router_capi.h (cef_media_observer_t)</see></para>
      /// </remarks>
      property OnRouteStateChanged                    : TOnRouteStateChangedEvent         read FOnRouteStateChanged                    write FOnRouteStateChanged;
      /// <summary>
      /// A message was recieved over |route|. |message| is only valid for the scope
      /// of this callback and should be copied if necessary.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_media_router_capi.h">CEF source file: /include/capi/cef_media_router_capi.h (cef_media_observer_t)</see></para>
      /// </remarks>
      property OnRouteMessageReceived                 : TOnRouteMessageReceivedEvent      read FOnRouteMessageReceived                 write FOnRouteMessageReceived;
      /// <summary>
      /// Called on the UI thread to allow configuration of audio stream parameters.
      /// Return true (1) to proceed with audio stream capture, or false (0) to
      /// cancel it. All members of |params| can optionally be configured here, but
      /// they are also pre-filled with some sensible defaults.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_audio_handler_capi.h">CEF source file: /include/capi/cef_audio_handler_capi.h (cef_audio_handler_t)</see></para>
      /// </remarks>
      property OnGetAudioParameters                   : TOnGetAudioParametersEvent        read FOnGetAudioParameters                   write FOnGetAudioParameters;
      /// <summary>
      /// Called on a browser audio capture thread when the browser starts streaming
      /// audio. OnAudioStreamStopped will always be called after
      /// OnAudioStreamStarted; both functions may be called multiple times for the
      /// same browser. |params| contains the audio parameters like sample rate and
      /// channel layout. |channels| is the number of channels.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on a browser audio capture thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_audio_handler_capi.h">CEF source file: /include/capi/cef_audio_handler_capi.h (cef_audio_handler_t)</see></para>
      /// </remarks>
      property OnAudioStreamStarted                   : TOnAudioStreamStartedEvent        read FOnAudioStreamStarted                   write FOnAudioStreamStarted;
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
      /// <remarks>
      /// <para>This event will be called on a browser audio capture thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_audio_handler_capi.h">CEF source file: /include/capi/cef_audio_handler_capi.h (cef_audio_handler_t)</see></para>
      /// </remarks>
      property OnAudioStreamPacket                    : TOnAudioStreamPacketEvent         read FOnAudioStreamPacket                    write FOnAudioStreamPacket;
      /// <summary>
      /// Called on the UI thread when the stream has stopped. OnAudioSteamStopped
      /// will always be called after OnAudioStreamStarted; both functions may be
      /// called multiple times for the same stream.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_audio_handler_capi.h">CEF source file: /include/capi/cef_audio_handler_capi.h (cef_audio_handler_t)</see></para>
      /// </remarks>
      property OnAudioStreamStopped                   : TOnAudioStreamStoppedEvent        read FOnAudioStreamStopped                   write FOnAudioStreamStopped;
      /// <summary>
      /// Called on the UI or audio stream thread when an error occurred. During the
      /// stream creation phase this callback will be called on the UI thread while
      /// in the capturing phase it will be called on the audio stream thread. The
      /// stream will be stopped immediately.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread or a browser audio capture thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_audio_handler_capi.h">CEF source file: /include/capi/cef_audio_handler_capi.h (cef_audio_handler_t)</see></para>
      /// </remarks>
      property OnAudioStreamError                     : TOnAudioStreamErrorEvent          read FOnAudioStreamError                     write FOnAudioStreamError;
      /// <summary>
      /// <para>Method that will be called on receipt of a DevTools protocol message.
      /// |browser| is the originating browser instance. |message| is a UTF8-encoded
      /// JSON dictionary representing either a function result or an event.</para>
      /// <para>|message| is only valid for the scope of this callback and should be
      /// copied if necessary. Return true (1) if the message was handled or false
      /// (0) if the message should be further processed and passed to the
      /// OnDevToolsMethodResult or OnDevToolsEvent functions as appropriate.</para>
      /// <para>Method result dictionaries include an "id" (int) value that identifies the
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
      /// large messages (some of which may exceed 1MB in size).</para>
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_devtools_message_observer_capi.h">CEF source file: /include/capi/cef_devtools_message_observer_capi.h (cef_dev_tools_message_observer_t)</see></para>
      /// </remarks>
      property OnDevToolsMessage                      : TOnDevToolsMessageEvent           read FOnDevToolsMessage                      write FOnDevToolsMessage;
      /// <summary>
      /// <para>Method that will be called on receipt of a DevTools protocol message.
      /// |browser| is the originating browser instance. |message| is a UTF8-encoded
      /// JSON dictionary representing either a function result or an event.</para>
      /// <para>|message| is only valid for the scope of this callback and should be
      /// copied if necessary. Return true (1) if the message was handled or false
      /// (0) if the message should be further processed and passed to the
      /// OnDevToolsMethodResult or OnDevToolsEvent functions as appropriate.</para>
      /// <para>Method result dictionaries include an "id" (int) value that identifies the
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
      /// large messages (some of which may exceed 1MB in size).</para>
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_devtools_message_observer_capi.h">CEF source file: /include/capi/cef_devtools_message_observer_capi.h (cef_dev_tools_message_observer_t)</see></para>
      /// </remarks>
      property OnDevToolsRawMessage                   : TOnDevToolsRawMessageEvent        read FOnDevToolsRawMessage                   write FOnDevToolsRawMessage;
      /// <summary>
      /// <para>Method that will be called after attempted execution of a DevTools
      /// protocol function. |browser| is the originating browser instance.</para>
      /// <para>|message_id| is the "id" value that identifies the originating function
      /// call message. If the function succeeded |success| will be true (1) and
      /// |result| will be the UTF8-encoded JSON "result" dictionary value (which
      /// may be NULL). If the function failed |success| will be false (0) and
      /// |result| will be the UTF8-encoded JSON "error" dictionary value. |result|
      /// is only valid for the scope of this callback and should be copied if
      /// necessary. See the OnDevToolsMessage documentation for additional details
      /// on |result| contents.</para>
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_devtools_message_observer_capi.h">CEF source file: /include/capi/cef_devtools_message_observer_capi.h (cef_dev_tools_message_observer_t)</see></para>
      /// </remarks>
      property OnDevToolsMethodResult                 : TOnDevToolsMethodResultEvent      read FOnDevToolsMethodResult                 write FOnDevToolsMethodResult;
      /// <summary>
      /// <para>Method that will be called after attempted execution of a DevTools
      /// protocol function. |browser| is the originating browser instance.</para>
      /// <para>|message_id| is the "id" value that identifies the originating function
      /// call message. If the function succeeded |success| will be true (1) and
      /// |result| will be the UTF8-encoded JSON "result" dictionary value (which
      /// may be NULL). If the function failed |success| will be false (0) and
      /// |result| will be the UTF8-encoded JSON "error" dictionary value. |result|
      /// is only valid for the scope of this callback and should be copied if
      /// necessary. See the OnDevToolsMessage documentation for additional details
      /// on |result| contents.</para>
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_devtools_message_observer_capi.h">CEF source file: /include/capi/cef_devtools_message_observer_capi.h (cef_dev_tools_message_observer_t)</see></para>
      /// </remarks>
      property OnDevToolsMethodRawResult              : TOnDevToolsMethodRawResultEvent   read FOnDevToolsMethodRawResult              write FOnDevToolsMethodRawResult;
      /// <summary>
      /// Method that will be called on receipt of a DevTools protocol event.
      /// |browser| is the originating browser instance. |function| is the
      /// "function" value. |params| is the UTF8-encoded JSON "params" dictionary
      /// value (which may be NULL). |params| is only valid for the scope of this
      /// callback and should be copied if necessary. See the OnDevToolsMessage
      /// documentation for additional details on |params| contents.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_devtools_message_observer_capi.h">CEF source file: /include/capi/cef_devtools_message_observer_capi.h (cef_dev_tools_message_observer_t)</see></para>
      /// </remarks>
      property OnDevToolsEvent                        : TOnDevToolsEventEvent             read FOnDevToolsEvent                        write FOnDevToolsEvent;
      /// <summary>
      /// Method that will be called on receipt of a DevTools protocol event.
      /// |browser| is the originating browser instance. |function| is the
      /// "function" value. |params| is the UTF8-encoded JSON "params" dictionary
      /// value (which may be NULL). |params| is only valid for the scope of this
      /// callback and should be copied if necessary. See the OnDevToolsMessage
      /// documentation for additional details on |params| contents.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_devtools_message_observer_capi.h">CEF source file: /include/capi/cef_devtools_message_observer_capi.h (cef_dev_tools_message_observer_t)</see></para>
      /// </remarks>
      property OnDevToolsRawEvent                     : TOnDevToolsEventRawEvent          read FOnDevToolsRawEvent                     write FOnDevToolsRawEvent;
      /// <summary>
      /// Method that will be called when the DevTools agent has attached. |browser|
      /// is the originating browser instance. This will generally occur in response
      /// to the first message sent while the agent is detached.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_devtools_message_observer_capi.h">CEF source file: /include/capi/cef_devtools_message_observer_capi.h (cef_dev_tools_message_observer_t)</see></para>
      /// </remarks>
      property OnDevToolsAgentAttached                : TOnDevToolsAgentAttachedEvent     read FOnDevToolsAgentAttached                write FOnDevToolsAgentAttached;
      /// <summary>
      /// Method that will be called when the DevTools agent has detached. |browser|
      /// is the originating browser instance. Any function results that were
      /// pending before the agent became detached will not be delivered, and any
      /// active event subscriptions will be canceled.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_devtools_message_observer_capi.h">CEF source file: /include/capi/cef_devtools_message_observer_capi.h (cef_dev_tools_message_observer_t)</see></para>
      /// </remarks>
      property OnDevToolsAgentDetached                : TOnDevToolsAgentDetachedEvent     read FOnDevToolsAgentDetached                write FOnDevToolsAgentDetached;
      /// <summary>
      /// Called if the ICefRequestContext.LoadExtension request fails. |result|
      /// will be the error code.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_extension_handler_capi.h">CEF source file: /include/capi/cef_extension_handler_capi.h (cef_extension_handler_t)</see></para>
      /// </remarks>
      property OnExtensionLoadFailed                  : TOnExtensionLoadFailedEvent       read FOnExtensionLoadFailed                  write FOnExtensionLoadFailed;
      /// <summary>
      /// Called if the ICefRequestContext.LoadExtension request succeeds.
      /// |extension| is the loaded extension.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_extension_handler_capi.h">CEF source file: /include/capi/cef_extension_handler_capi.h (cef_extension_handler_t)</see></para>
      /// </remarks>
      property OnExtensionLoaded                      : TOnExtensionLoadedEvent           read FOnExtensionLoaded                      write FOnExtensionLoaded;
      /// <summary>
      /// Called after the ICefExtension.Unload request has completed.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_extension_handler_capi.h">CEF source file: /include/capi/cef_extension_handler_capi.h (cef_extension_handler_t)</see></para>
      /// </remarks>
      property OnExtensionUnloaded                    : TOnExtensionUnloadedEvent         read FOnExtensionUnloaded                    write FOnExtensionUnloaded;
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
      /// to ICefLifeSpanHandler.OnAfterCreated, and
      /// ICefBrowserHost.IsBackgroundHost will return true (1) for the
      /// resulting browser. See https://developer.chrome.com/extensions/event_pages
      /// for more information about extension background script usage.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_extension_handler_capi.h">CEF source file: /include/capi/cef_extension_handler_capi.h (cef_extension_handler_t)</see></para>
      /// </remarks>
      property OnExtensionBeforeBackgroundBrowser     : TOnBeforeBackgroundBrowserEvent   read FOnExtensionBeforeBackgroundBrowser     write FOnExtensionBeforeBackgroundBrowser;
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
      /// ICefLifeSpanHandler.OnAfterCreated. Any modifications to |windowInfo|
      /// will be ignored if |active_browser| is wrapped in a ICefBrowserView.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_extension_handler_capi.h">CEF source file: /include/capi/cef_extension_handler_capi.h (cef_extension_handler_t)</see></para>
      /// </remarks>
      property OnExtensionBeforeBrowser               : TOnBeforeBrowserEvent             read FOnExtensionBeforeBrowser               write FOnExtensionBeforeBrowser;
      /// <summary>
      /// Called when no tabId is specified to an extension API call that accepts a
      /// tabId parameter (e.g. chrome.tabs.*). |extension| and |browser| are the
      /// source of the API call. Return the browser that will be acted on by the
      /// API call or return NULL to act on |browser|. The returned browser must
      /// share the same ICefRequestContext as |browser|. Incognito browsers
      /// should not be considered unless the source extension has incognito access
      /// enabled, in which case |include_incognito| will be true (1).
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_extension_handler_capi.h">CEF source file: /include/capi/cef_extension_handler_capi.h (cef_extension_handler_t)</see></para>
      /// </remarks>
      property OnExtensionGetActiveBrowser            : TOnGetActiveBrowserEvent          read FOnExtensionGetActiveBrowser            write FOnExtensionGetActiveBrowser;
      /// <summary>
      /// Called when the tabId associated with |target_browser| is specified to an
      /// extension API call that accepts a tabId parameter (e.g. chrome.tabs.*).
      /// |extension| and |browser| are the source of the API call. Return true (1)
      /// to allow access of false (0) to deny access. Access to incognito browsers
      /// should not be allowed unless the source extension has incognito access
      /// enabled, in which case |include_incognito| will be true (1).
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_extension_handler_capi.h">CEF source file: /include/capi/cef_extension_handler_capi.h (cef_extension_handler_t)</see></para>
      /// </remarks>
      property OnExtensionCanAccessBrowser            : TOnCanAccessBrowserEvent          read FOnExtensionCanAccessBrowser            write FOnExtensionCanAccessBrowser;
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
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_extension_handler_capi.h">CEF source file: /include/capi/cef_extension_handler_capi.h (cef_extension_handler_t)</see></para>
      /// </remarks>
      property OnExtensionGetExtensionResource        : TOnGetExtensionResourceEvent      read FOnExtensionGetExtensionResource        write FOnExtensionGetExtensionResource;
      {$IFDEF LINUX}
      /// <summary>
      /// Called when printing has started for the specified |browser|. This
      /// function will be called before the other OnPrint*() functions and
      /// irrespective of how printing was initiated (e.g.
      /// ICefBrowserHost.print(), JavaScript window.print() or PDF extension
      /// print button).
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_print_handler_capi.h">CEF source file: /include/capi/cef_print_handler_capi.h (cef_print_handler_t)</see></para>
      /// </remarks>
      property OnPrintStart                           : TOnPrintStartEvent                read FOnPrintStart                           write FOnPrintStart;
      /// <summary>
      /// Synchronize |settings| with client state. If |get_defaults| is true (1)
      /// then populate |settings| with the default print settings. Do not keep a
      /// reference to |settings| outside of this callback.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_print_handler_capi.h">CEF source file: /include/capi/cef_print_handler_capi.h (cef_print_handler_t)</see></para>
      /// </remarks>
      property OnPrintSettings                        : TOnPrintSettingsEvent             read FOnPrintSettings                        write FOnPrintSettings;
      /// <summary>
      /// Show the print dialog. Execute |callback| once the dialog is dismissed.
      /// Return true (1) if the dialog will be displayed or false (0) to cancel the
      /// printing immediately.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_print_handler_capi.h">CEF source file: /include/capi/cef_print_handler_capi.h (cef_print_handler_t)</see></para>
      /// </remarks>
      property OnPrintDialog                          : TOnPrintDialogEvent               read FOnPrintDialog                          write FOnPrintDialog;
      /// <summary>
      /// Send the print job to the printer. Execute |callback| once the job is
      /// completed. Return true (1) if the job will proceed or false (0) to cancel
      /// the job immediately.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_print_handler_capi.h">CEF source file: /include/capi/cef_print_handler_capi.h (cef_print_handler_t)</see></para>
      /// </remarks>
      property OnPrintJob                             : TOnPrintJobEvent                  read FOnPrintJob                             write FOnPrintJob;
      /// <summary>
      /// Reset client state related to printing.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_print_handler_capi.h">CEF source file: /include/capi/cef_print_handler_capi.h (cef_print_handler_t)</see></para>
      /// </remarks>
      property OnPrintReset                           : TOnPrintResetEvent                read FOnPrintReset                           write FOnPrintReset;
      /// <summary>
      /// Return the PDF paper size in device units. Used in combination with
      /// ICefBrowserHost.PrintToPdf().
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_print_handler_capi.h">CEF source file: /include/capi/cef_print_handler_capi.h (cef_print_handler_t)</see></para>
      /// </remarks>
      property OnGetPDFPaperSize                      : TOnGetPDFPaperSizeEvent           read FOnGetPDFPaperSize                      write FOnGetPDFPaperSize;
      {$ENDIF}
      /// <summary>
      /// Called when a new frame is created. This will be the first notification
      /// that references |frame|. Any commands that require transport to the
      /// associated renderer process (LoadRequest, SendProcessMessage, GetSource,
      /// etc.) will be queued until OnFrameAttached is called for |frame|.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_frame_handler_capi.h">CEF source file: /include/capi/cef_frame_handler_capi.h (cef_frame_handler_t)</see></para>
      /// </remarks>
      property OnFrameCreated                         : TOnFrameCreated                   read FOnFrameCreated                         write FOnFrameCreated;
      /// <summary>
      /// Called when a frame can begin routing commands to/from the associated
      /// renderer process. |reattached| will be true (1) if the frame was re-
      /// attached after exiting the BackForwardCache. Any commands that were queued
      /// have now been dispatched.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_frame_handler_capi.h">CEF source file: /include/capi/cef_frame_handler_capi.h (cef_frame_handler_t)</see></para>
      /// </remarks>
      property OnFrameAttached                        : TOnFrameAttached                  read FOnFrameAttached                        write FOnFrameAttached;
      /// <summary>
      /// Called when a frame loses its connection to the renderer process and will
      /// be destroyed. Any pending or future commands will be discarded and
      /// ICefFrame.IsValid() will now return false (0) for |frame|. If called
      /// after ICefLifeSpanHandler.OnBeforeClose() during browser
      /// destruction then ICefBrowser.IsValid() will return false (0) for
      /// |browser|.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_frame_handler_capi.h">CEF source file: /include/capi/cef_frame_handler_capi.h (cef_frame_handler_t)</see></para>
      /// </remarks>
      property OnFrameDetached                        : TOnFrameDetached                  read FOnFrameDetached                        write FOnFrameDetached;
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
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_frame_handler_capi.h">CEF source file: /include/capi/cef_frame_handler_capi.h (cef_frame_handler_t)</see></para>
      /// </remarks>
      property OnMainFrameChanged                     : TOnMainFrameChanged               read FOnMainFrameChanged                     write FOnMainFrameChanged;
      /// <summary>
      /// Called to execute a Chrome command triggered via menu selection or
      /// keyboard shortcut. Values for |command_id| can be found in the
      /// cef_command_ids.h file. |disposition| provides information about the
      /// intended command target. Return true (1) if the command was handled or
      /// false (0) for the default implementation. For context menu commands this
      /// will be called after ICefContextMenuHandler.OnContextMenuCommand.
      /// </summary>
      /// <remarks>
      /// <para>Only used with the Chrome runtime.</para>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_command_handler_capi.h">CEF source file: /include/capi/cef_command_handler_capi.h (cef_command_handler_t)</see></para>
      /// </remarks>
      property OnChromeCommand                        : TOnChromeCommandEvent                 read FOnChromeCommand                    write FOnChromeCommand;
      /// <summary>
      /// Called to check if a Chrome app menu item should be visible. Values for
      /// |command_id| can be found in the cef_command_ids.h file. Only called for
      /// menu items that would be visible by default.
      /// </summary>
      /// <remarks>
      /// <para>Only used with the Chrome runtime.</para>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_command_handler_capi.h">CEF source file: /include/capi/cef_command_handler_capi.h (cef_command_handler_t)</see></para>
      /// </remarks>
      property OnIsChromeAppMenuItemVisible           : TOnIsChromeAppMenuItemVisibleEvent    read FOnIsChromeAppMenuItemVisible       write FOnIsChromeAppMenuItemVisible;
      /// <summary>
      /// Called to check if a Chrome app menu item should be enabled. Values for
      /// |command_id| can be found in the cef_command_ids.h file. Only called for
      /// menu items that would be enabled by default.
      /// </summary>
      /// <remarks>
      /// <para>Only used with the Chrome runtime.</para>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_command_handler_capi.h">CEF source file: /include/capi/cef_command_handler_capi.h (cef_command_handler_t)</see></para>
      /// </remarks>
      property OnIsChromeAppMenuItemEnabled           : TOnIsChromeAppMenuItemEnabledEvent    read FOnIsChromeAppMenuItemEnabled       write FOnIsChromeAppMenuItemEnabled;
      /// <summary>
      /// Called during browser creation to check if a Chrome page action icon
      /// should be visible. Only called for icons that would be visible by default.
      /// </summary>
      /// <remarks>
      /// <para>Only used with the Chrome runtime.</para>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_command_handler_capi.h">CEF source file: /include/capi/cef_command_handler_capi.h (cef_command_handler_t)</see></para>
      /// </remarks>
      property OnIsChromePageActionIconVisible        : TOnIsChromePageActionIconVisibleEvent read FOnIsChromePageActionIconVisible    write FOnIsChromePageActionIconVisible;
      /// <summary>
      /// Called during browser creation to check if a Chrome toolbar button should
      /// be visible. Only called for buttons that would be visible by default.
      /// </summary>
      /// <remarks>
      /// <para>Only used with the Chrome runtime.</para>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_command_handler_capi.h">CEF source file: /include/capi/cef_command_handler_capi.h (cef_command_handler_t)</see></para>
      /// </remarks>
      property OnIsChromeToolbarButtonVisible         : TOnIsChromeToolbarButtonVisibleEvent  read FOnIsChromeToolbarButtonVisible     write FOnIsChromeToolbarButtonVisible;
      /// <summary>
      /// <para>Called when a page requests permission to access media.
      /// |requesting_origin| is the URL origin requesting permission.</para>
      /// <para>|requested_permissions| is a combination of values from
      /// TCefMediaAccessPermissionTypes that represent the requested
      /// permissions. Return true (1) and call ICefMediaAccessCallback
      /// functions either in this function or at a later time to continue or cancel
      /// the request. Return false (0) to proceed with default handling. With the
      /// Chrome runtime, default handling will display the permission request UI.</para>
      /// <para>With the Alloy runtime, default handling will deny the request. This
      /// function will not be called if the "--enable-media-stream" command-line
      /// switch is used to grant all permissions.</para>
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_permission_handler_capi.h">CEF source file: /include/capi/cef_permission_handler_capi.h (cef_permission_handler_t)</see></para>
      /// </remarks>
      property OnRequestMediaAccessPermission         : TOnRequestMediaAccessPermissionEvent read FOnRequestMediaAccessPermission      write FOnRequestMediaAccessPermission;
      /// <summary>
      /// <para>Called when a page should show a permission prompt. |prompt_id| uniquely
      /// identifies the prompt. |requesting_origin| is the URL origin requesting
      /// permission. |requested_permissions| is a combination of values from
      /// TCefPermissionRequestTypes that represent the requested permissions.</para>
      /// <para>Return true (1) and call ICefPermissionPromptCallback.Continue either
      /// in this function or at a later time to continue or cancel the request.</para>
      /// <para>Return false (0) to proceed with default handling. With the Chrome
      /// runtime, default handling will display the permission prompt UI. With the
      /// Alloy runtime, default handling is CEF_PERMISSION_RESULT_IGNORE.</para>
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_permission_handler_capi.h">CEF source file: /include/capi/cef_permission_handler_capi.h (cef_permission_handler_t)</see></para>
      /// </remarks>
      property OnShowPermissionPrompt                 : TOnShowPermissionPromptEvent         read FOnShowPermissionPrompt              write FOnShowPermissionPrompt;
      /// <summary>
      /// Called when a permission prompt handled via OnShowPermissionPrompt is
      /// dismissed. |prompt_id| will match the value that was passed to
      /// OnShowPermissionPrompt. |result| will be the value passed to
      /// ICefPermissionPromptCallback.Continue or CEF_PERMISSION_RESULT_IGNORE
      /// if the dialog was dismissed for other reasons such as navigation, browser
      /// closure, etc. This function will not be called if OnShowPermissionPrompt
      /// returned false (0) for |prompt_id|.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the browser process CEF UI thread.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_permission_handler_capi.h">CEF source file: /include/capi/cef_permission_handler_capi.h (cef_permission_handler_t)</see></para>
      /// </remarks>
      property OnDismissPermissionPrompt              : TOnDismissPermissionPromptEvent      read FOnDismissPermissionPrompt           write FOnDismissPermissionPrompt;
  end;

  TBrowserInfo = class
    protected
      FBrowser   : ICefBrowser;
      FIsClosing : boolean;
      FID        : integer;

      function GetIsValid : boolean;

    public
      constructor Create(const aBrowser : ICefBrowser); reintroduce;
      destructor  Destroy; override;

      property Browser    : ICefBrowser   read FBrowser;
      property ID         : integer       read FID;
      property IsClosing  : boolean       read FIsClosing   write FIsClosing;
      property IsValid    : boolean       read GetIsValid;
  end;

  TBrowserInfoList = class(TList)
    protected
      procedure SetBrowserIsClosing(aID : integer; aValue : boolean);

      function  GetBrowserIsClosing(aID : integer) : boolean;
      function  GetBrowserIsValid(aID : integer) : boolean;
      function  GetBrowser(aID : integer) : ICefBrowser;
      function  GetFirstBrowser : ICefBrowser;
      function  GetFirstID : integer;

    public
      destructor Destroy; override;
      function   AddBrowser(const aBrowser : ICefBrowser) : boolean;
      function   RemoveBrowser(const aBrowser : ICefBrowser) : boolean;
      function   SearchBrowser(aID : integer) : integer;
      procedure  FreeAndClearAllItems;
      procedure  CloseAllBrowsers;

      property BrowserIsClosing[aID : integer] : boolean       read GetBrowserIsClosing  write SetBrowserIsClosing;
      property BrowserIsValid[aID : integer]   : boolean       read GetBrowserIsValid;
      property Browser[aID : integer]          : ICefBrowser   read GetBrowser;
      property FirstBrowser                    : ICefBrowser   read GetFirstBrowser;
      property FirstID                         : integer       read GetFirstID;
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
    {$IFDEF FPC}
      {$IFDEF LINUX}x, xatom,
        {$IFDEF LCLGTK2}gdk2x, gtk2,{$ENDIF}
        {$IFDEF LCLGTK3}LazGdk3, LazGtk3, LazGLib2,{$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
  uCEFBrowser, uCEFValue, uCEFDictionaryValue, uCEFStringMultimap, uCEFFrame,
  uCEFApplicationCore, uCEFProcessMessage, uCEFRequestContext,
  {$IFDEF MSWINDOWS}uCEFOLEDragAndDrop,{$ENDIF}
  uCEFPDFPrintCallback, uCEFResolveCallback, uCEFDeleteCookiesCallback,
  uCEFStringVisitor, uCEFListValue, uCEFNavigationEntryVisitor,
  uCEFDownloadImageCallBack, uCEFCookieManager, uCEFRequestContextHandler,
  uCEFCookieVisitor, uCEFSetCookieCallback, uCEFResourceRequestHandler,
  uCEFMediaObserver, uCEFMediaRouteCreateCallback ,uCEFDevToolsMessageObserver,
  uCEFMediaSinkDeviceInfoCallback, uCEFJson, uCEFExtensionHandler;

constructor TChromiumCore.Create(AOwner: TComponent);
begin
  FBrowsersCS              := nil;
  FBrowsers                := nil;
  FBrowserId               := 0;
  FMultiBrowserMode        := False;
  {$IFDEF MSWINDOWS}
  FCompHandle              := 0;
  {$ENDIF}
  FIsOSR                   := False;
  FDefaultUrl              := ABOUTBLANK_URI;
  FHandler                 := nil;
  FReqContextHandler       := nil;
  FResourceRequestHandler  := nil;
  FMediaObserver           := nil;
  FMediaObserverReg        := nil;
  FDevToolsMsgObserver     := nil;
  FDevToolsMsgObserverReg  := nil;
  FExtensionHandler        := nil;
  FOptions                 := nil;
  FFontOptions             := nil;
  FDefaultEncoding         := '';
  FPDFPrintOptions         := nil;
  FUpdatePreferences       := False;
  FCustomHeaderName        := '';
  FCustomHeaderValue       := '';
  FPrefsFileName           := '';
  FAddCustomHeader         := False;
  FDoNotTrack              := True;
  FSendReferrer            := True;
  FAllowOutdatedPlugins    := False;
  FAlwaysAuthorizePlugins  := False;
  FAlwaysOpenPDFExternally := False;
  FSpellChecking           := True;
  FSpellCheckerDicts       := '';
  FZoomStep                := ZOOM_STEP_DEF;
  FZoomStepCS              := nil;
  FSafeSearch              := False;
  FYouTubeRestrict         := YOUTUBE_RESTRICT_OFF;
  FPrintingEnabled         := True;
  FAcceptLanguageList      := '';
  FAcceptCookies           := cpAllow;
  FBlock3rdPartyCookies    := False;
  FOffline                 := False;
  FQuicAllowed             := True;
  FJavascriptEnabled       := True;
  FLoadImagesAutomatically := True;
  FBatterySaverModeState   := bsmsDefault;
  FHighEfficiencyModeState := kDefault;
  FCanFocus                := False;
  FEnableFocusDelayMs      := CEF_DEFAULT_ENABLEFOCUSDELAY;
  {$IFDEF LINUX}
  FXDisplay                := nil;
  {$ENDIF}

  if (GlobalCEFApp <> nil) then
    FHyperlinkAuditing := GlobalCEFApp.HyperlinkAuditing
   else
    FHyperlinkAuditing := True;

  //
  // Some focus issues in CEF seem to be fixed when you use WS_EX_NOACTIVATE in
  // FDefaultWindowInfoExStyle to initialize the browser with that ExStyle but
  // it may cause side effects. Read these links for more information :
  // https://www.briskbard.com/forum/viewtopic.php?f=10&t=723
  // https://bitbucket.org/chromiumembedded/cef/issues/1856/branch-2526-cef-activates-browser-window
  //
  // It's necessary to set this property before the CreateBrowser call.
  //
  FDefaultWindowInfoExStyle := 0;
  //FDefaultWindowInfoExStyle := WS_EX_NOACTIVATE;

  {$IFDEF MSWINDOWS}
  FOldBrowserCompWndPrc   := nil;
  FOldWidgetCompWndPrc    := nil;
  FOldRenderCompWndPrc    := nil;
  FBrowserCompStub        := nil;
  FWidgetCompStub         := nil;
  FRenderCompStub         := nil;
  FBrowserCompHWND        := 0;
  FWidgetCompHWND         := 0;
  FRenderCompHWND         := 0;
  {$ENDIF MSWINDOWS}

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
      DestroyAllHandlersAndObservers;

      {$IFDEF MSWINDOWS}
      RestoreOldCompWndProc;

      if (FDragDropManager <> nil) then FreeAndNil(FDragDropManager);

      if (FCompHandle <> 0) then
        begin
          DeallocateHWnd(FCompHandle);
          FCompHandle := 0;
        end;
      {$ENDIF MSWINDOWS}

      DestroyAllBrowsers;

      if (FFontOptions     <> nil) then FreeAndNil(FFontOptions);
      if (FOptions         <> nil) then FreeAndNil(FOptions);
      if (FPDFPrintOptions <> nil) then FreeAndNil(FPDFPrintOptions);
      if (FZoomStepCS      <> nil) then FreeAndNil(FZoomStepCS);
      if (FBrowsersCS      <> nil) then FreeAndNil(FBrowsersCS);
    except
      on e : exception do
        if CustomExceptionHandler('TChromiumCore.Destroy', e) then raise;
    end;
  finally
    inherited Destroy;
  end;
end;

procedure TChromiumCore.DestroyAllBrowsers;
begin
  if (FBrowsersCS <> nil) then
    try
      FBrowsersCS.Acquire;
      if (FBrowsers <> nil) then FreeAndNil(FBrowsers);
    finally
      FBrowsersCS.Release;
    end;
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

procedure TChromiumCore.DestroyMediaObserver;
begin
  FMediaObserverReg := nil;
  FMediaObserver    := nil;
end;

procedure TChromiumCore.DestroyDevToolsMsgObserver;
begin
  FDevToolsMsgObserverReg := nil;
  FDevToolsMsgObserver    := nil;
end;

procedure TChromiumCore.DestroyExtensionHandler;
begin
  try
    if (FExtensionHandler <> nil) then
      begin
        FExtensionHandler.RemoveReferences;
        FExtensionHandler := nil;
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TChromiumCore.DestroyExtensionHandler', e) then raise;
  end;
end;

procedure TChromiumCore.DestroyAllHandlersAndObservers;
begin
  DestroyExtensionHandler;
  DestroyDevToolsMsgObserver;
  DestroyMediaObserver;
  DestroyResourceRequestHandler;
  DestroyReqContextHandler;
  DestroyClientHandler;
end;

procedure TChromiumCore.CreateMediaObserver;
begin
  if MustCreateMediaObserver and
     (FMediaObserver = nil) then
    FMediaObserver := TCustomMediaObserver.Create(self);
end;

procedure TChromiumCore.CreateDevToolsMsgObserver;
begin
  if MustCreateDevToolsMessageObserver and
     (FDevToolsMsgObserver = nil) then
    FDevToolsMsgObserver := TCustomDevToolsMessageObserver.Create(self);
end;

procedure TChromiumCore.CreateExtensionHandler;
begin
  if MustCreateExtensionHandler and
     (FExtensionHandler = nil) then
    FExtensionHandler := TCustomExtensionHandler.Create(self);
end;

procedure TChromiumCore.CreateResourceRequestHandler;
begin
  if MustCreateResourceRequestHandler and
     (FResourceRequestHandler = nil) then
    FResourceRequestHandler := TCustomResourceRequestHandler.Create(self);
end;

procedure TChromiumCore.CreateOptionsClasses;
begin
  if (Owner = nil) or not(csDesigning in ComponentState) then
    begin
      FOptions         := TChromiumOptions.Create;
      FFontOptions     := TChromiumFontOptions.Create;
      FPDFPrintOptions := TPDFPrintOptions.Create;
    end;
end;

procedure TChromiumCore.CreateSyncObjects;
begin
  if (Owner = nil) or not(csDesigning in ComponentState) then
    begin
      FZoomStepCS := TCriticalSection.Create;
      FBrowsersCS := TCriticalSection.Create;
    end;
end;

procedure TChromiumCore.CreateRequestContextHandler;
begin
  if (Owner = nil) or not(csDesigning in ComponentState) then
    FReqContextHandler := TCustomRequestContextHandler.Create(self);
end;

{$IFDEF MSWINDOWS}
procedure TChromiumCore.CreateWindowWithWndProc;
begin
  if (Owner = nil) or not(csDesigning in ComponentState) then
    FCompHandle := AllocateHWnd({$IFDEF FPC}@{$ENDIF}WndProc);
end;
{$ENDIF}

procedure TChromiumCore.CreateBrowserInfoList;
begin
  if (Owner = nil) or not(csDesigning in ComponentState) then
    FBrowsers := TBrowserInfoList.Create;
end;

procedure TChromiumCore.AfterConstruction;
begin
  inherited AfterConstruction;

  CreateOptionsClasses;
  CreateSyncObjects;
  {$IFDEF MSWINDOWS}
  CreateWindowWithWndProc;
  {$ENDIF}
  CreateBrowserInfoList;
  CreateRequestContextHandler;
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
      CreateMediaObserver;
      CreateDevToolsMsgObserver;
      CreateExtensionHandler;

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
  FOnRunQuickMenu                 := nil;
  FOnQuickMenuCommand             := nil;
  FOnQuickMenuDismissed           := nil;

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
  FOnCursorChange                 := nil;
  FOnMediaAccessChange            := nil;

  // ICefDownloadHandler
  FOnCanDownload                  := nil;
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
  FOnCertificateError                  := nil;
  FOnSelectClientCertificate           := nil;
  FOnRenderViewReady                   := nil;
  FOnRenderProcessTerminated           := nil;
  FOnGetResourceRequestHandler_ReqHdlr := nil;
  FOnDocumentAvailableInMainFrame      := nil;

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
  FOnGetTouchHandleSize           := nil;
  FOnTouchHandleStateChanged      := nil;
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
  FOnGetResourceRequestHandler_ReqCtxHdlr := nil;

  // ICefMediaObserver
  FOnSinks                            := nil;
  FOnRoutes                           := nil;
  FOnRouteStateChanged                := nil;
  FOnRouteMessageReceived             := nil;

  // ICefAudioHandler
  FOnGetAudioParameters               := nil;
  FOnAudioStreamStarted               := nil;
  FOnAudioStreamPacket                := nil;
  FOnAudioStreamStopped               := nil;
  FOnAudioStreamError                 := nil;

  // ICefDevToolsMessageObserver
  FOnDevToolsMessage                  := nil;
  FOnDevToolsRawMessage               := nil;
  FOnDevToolsMethodResult             := nil;
  FOnDevToolsMethodRawResult          := nil;
  FOnDevToolsEvent                    := nil;
  FOnDevToolsRawEvent                 := nil;
  FOnDevToolsAgentAttached            := nil;
  FOnDevToolsAgentDetached            := nil;

  // ICefExtensionHandler
  FOnExtensionLoadFailed              := nil;
  FOnExtensionLoaded                  := nil;
  FOnExtensionUnloaded                := nil;
  FOnExtensionBeforeBackgroundBrowser := nil;
  FOnExtensionBeforeBrowser           := nil;
  FOnExtensionGetActiveBrowser        := nil;
  FOnExtensionCanAccessBrowser        := nil;
  FOnExtensionGetExtensionResource    := nil;

  // ICefPrintHandler
  FOnPrintStart                       := nil;
  FOnPrintSettings                    := nil;
  FOnPrintDialog                      := nil;
  FOnPrintJob                         := nil;
  FOnPrintReset                       := nil;
  FOnGetPDFPaperSize                  := nil;

  // ICefFrameHandler
  FOnFrameCreated                     := nil;
  FOnFrameAttached                    := nil;
  FOnFrameDetached                    := nil;
  FOnMainFrameChanged                 := nil;

  // ICefCommandHandler
  FOnChromeCommand                    := nil;
  FOnIsChromeAppMenuItemVisible       := nil;
  FOnIsChromeAppMenuItemEnabled       := nil;
  FOnIsChromePageActionIconVisible    := nil;
  FOnIsChromeToolbarButtonVisible     := nil;

  // ICefPermissionHandler
  FOnRequestMediaAccessPermission     := nil;
  FOnShowPermissionPrompt             := nil;
  FOnDismissPermissionPrompt          := nil;

  // Custom
  FOnTextResultAvailable              := nil;
  FOnPdfPrintFinished                 := nil;
  FOnPrefsAvailable                   := nil;
  FOnPrefsUpdated                     := nil;
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
  FOnMediaRouteCreateFinished         := nil;
  FOnMediaSinkDeviceInfo              := nil;
  FOnCanFocus                         := nil;

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
                                     const aExtraInfo     : ICefDictionaryValue;
                                           aForceAsPopup  : boolean) : boolean;
var
  TempNewContext, TempOldContext : ICefRequestContext;
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
         (BrowserId    =  0)   and
         (GlobalCEFApp <> nil) and
         GlobalCEFApp.GlobalContextInitialized and
         CreateClientHandler(not(ValidCefWindowHandle(aParentHandle))) then
        begin
          GetSettings(FBrowserSettings);

          if aForceAsPopup then
            WindowInfoAsPopUp(FWindowInfo, aParentHandle, aWindowName)
           else
            InitializeWindowInfo(aParentHandle, aParentRect, aWindowName);

          CreateResourceRequestHandler;
          CreateMediaObserver;
          CreateDevToolsMsgObserver;
          CreateExtensionHandler;

          if (aContext = nil) then
            TempOldContext := TCefRequestContextRef.Global()
           else
            TempOldContext := aContext;

          TempNewContext := TCefRequestContextRef.Shared(TempOldContext, FReqContextHandler);

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
    TempOldContext := nil;
    TempNewContext := nil;
  end;
end;

function TChromiumCore.CreateBrowser(const aURL             : ustring;
                                     const aBrowserViewComp : TCEFBrowserViewComponent;
                                     const aContext         : ICefRequestContext;
                                     const aExtraInfo       : ICefDictionaryValue) : boolean;
var
  TempNewContext, TempOldContext : ICefRequestContext;
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
         (BrowserId         = 0)   and
         (aBrowserViewComp <> nil) and
         (GlobalCEFApp     <> nil) and
         GlobalCEFApp.GlobalContextInitialized and
         CreateClientHandler(False) then
        begin
          GetSettings(FBrowserSettings);
          CreateResourceRequestHandler;
          CreateMediaObserver;
          CreateDevToolsMsgObserver;
          CreateExtensionHandler;

          if (aContext = nil) then
            TempOldContext := TCefRequestContextRef.Global()
           else
            TempOldContext := aContext;

          TempNewContext := TCefRequestContextRef.Shared(TempOldContext, FReqContextHandler);

          Result := aBrowserViewComp.CreateBrowserView(FHandler, aURL, FBrowserSettings, aExtraInfo, TempNewContext);
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TChromiumCore.CreateBrowser', e) then raise;
    end;
  finally
    TempOldContext := nil;
    TempNewContext := nil;
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
    WindowInfoAsChild(FWindowInfo, aParentHandle, aParentRect, aWindowName, FDefaultWindowInfoExStyle);
  {$ELSE}
  if FIsOSR then
    WindowInfoAsWindowless(FWindowInfo, aParentHandle)
   else
    WindowInfoAsChild(FWindowInfo, aParentHandle, aParentRect);
  {$ENDIF}
end;

procedure TChromiumCore.DefaultInitializeDevToolsWindowInfo(      aDevToolsWnd : TCefWindowHandle;
                                                            const aClientRect  : TRect;
                                                            const aWindowName  : ustring);
begin
  if (ValidCefWindowHandle(aDevToolsWnd)) then
  {$IFDEF MSWINDOWS}
    WindowInfoAsChild(FDevWindowInfo, aDevToolsWnd, aClientRect, aWindowName)
   else
    WindowInfoAsPopUp(FDevWindowInfo, WindowHandle, DEVTOOLS_WINDOWNAME);
  {$ELSE}
    WindowInfoAsChild(FDevWindowInfo, aDevToolsWnd, aClientRect)
   else
    // WindowInfoAsPopUp only exists for Windows. The Linux version of cefclient
    // calls WindowInfoAsChild with aParent set to NULL to create a popup window.
    WindowInfoAsPopUp(FDevWindowInfo, aDevToolsWnd);
  {$ENDIF}
end;

function TChromiumCore.ShareRequestContext(var   aContext : ICefRequestContext;
                                           const aHandler : ICefRequestContextHandler) : boolean;
begin
  Result   := False;
  aContext := nil;

  if Initialized then
    begin
      aContext := TCefRequestContextRef.Shared(Browser.Host.RequestContext, aHandler);
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
  if Initialized then
    Browser.Host.CloseBrowser(aForceClose);
end;

procedure TChromiumCore.CloseAllBrowsers;
begin
  if (FBrowsersCS <> nil) then
    try
      FBrowsersCS.Acquire;
      if (FBrowsers <> nil) then FBrowsers.CloseAllBrowsers;
    finally
      FBrowsersCS.Release;
    end;
end;

function TChromiumCore.TryCloseBrowser : boolean;
begin
  if Initialized then
    Result := Browser.Host.TryCloseBrowser
   else
    Result := True;
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
  TempURL     : TCefString;
  TempBrowser : ICefBrowser;
begin
  try
    TempURL     := CefString(aURL);
    TempBrowser := TCefBrowserRef.UnWrap(cef_browser_host_create_browser_sync(aWindowInfo, FHandler.Wrap, @TempURL, aSettings, CefGetData(aExtraInfo), CefGetData(aContext)));
    Result      := assigned(TempBrowser);
  finally
    TempBrowser := nil;
  end;
end;

procedure TChromiumCore.Find(const aSearchText : ustring; aForward, aMatchCase, aFindNext : Boolean);
begin
  if Initialized then
    Browser.Host.Find(aSearchText, aForward, aMatchCase, aFindNext);
end;

procedure TChromiumCore.StopFinding(aClearSelection : Boolean);
begin
  if Initialized then
    Browser.Host.StopFinding(aClearSelection);
end;

procedure TChromiumCore.Print;
begin
  if Initialized then
    Browser.Host.Print;
end;

procedure TChromiumCore.PrintToPDF(const aFilePath : ustring);
var
  TempSettings : TCefPdfPrintSettings;
  TempCallback : ICefPdfPrintCallback;
begin
  if Initialized and (FPDFPrintOptions <> nil) then
    begin
      FPDFPrintOptions.CopyToSettings(TempSettings);
      TempCallback := TCefCustomPDFPrintCallBack.Create(self);
      Browser.Host.PrintToPdf(aFilePath, @TempSettings, TempCallback);
    end;
end;

procedure TChromiumCore.ClipboardCopy;
var
  TempFrame : ICefFrame;
begin
  if Initialized then
    begin
      TempFrame := Browser.FocusedFrame;
      if (TempFrame = nil) then TempFrame := Browser.MainFrame;

      if (TempFrame <> nil) and TempFrame.IsValid then TempFrame.Copy;
    end;
end;

procedure TChromiumCore.ClipboardPaste;
var
  TempFrame : ICefFrame;
begin
  if Initialized then
    begin
      TempFrame := Browser.FocusedFrame;
      if (TempFrame = nil) then TempFrame := Browser.MainFrame;

      if (TempFrame <> nil) and TempFrame.IsValid then TempFrame.Paste;
    end;
end;

procedure TChromiumCore.ClipboardCut;
var
  TempFrame : ICefFrame;
begin
  if Initialized then
    begin
      TempFrame := Browser.FocusedFrame;
      if (TempFrame = nil) then TempFrame := Browser.MainFrame;

      if (TempFrame <> nil) and TempFrame.IsValid then TempFrame.Cut;
    end;
end;

procedure TChromiumCore.ClipboardUndo;
var
  TempFrame : ICefFrame;
begin
  if Initialized then
    begin
      TempFrame := Browser.FocusedFrame;
      if (TempFrame = nil) then TempFrame := Browser.MainFrame;

      if (TempFrame <> nil) and TempFrame.IsValid then TempFrame.Undo;
    end;
end;

procedure TChromiumCore.ClipboardRedo;
var
  TempFrame : ICefFrame;
begin
  if Initialized then
    begin
      TempFrame := Browser.FocusedFrame;
      if (TempFrame = nil) then TempFrame := Browser.MainFrame;

      if (TempFrame <> nil) and TempFrame.IsValid then TempFrame.Redo;
    end;
end;

procedure TChromiumCore.ClipboardDel;
var
  TempFrame : ICefFrame;
begin
  if Initialized then
    begin
      TempFrame := Browser.FocusedFrame;
      if (TempFrame = nil) then TempFrame := Browser.MainFrame;

      if (TempFrame <> nil) and TempFrame.IsValid then TempFrame.Del;
    end;
end;

procedure TChromiumCore.SelectAll;
var
  TempFrame : ICefFrame;
begin
  if Initialized then
    begin
      TempFrame := Browser.FocusedFrame;
      if (TempFrame = nil) then TempFrame := Browser.MainFrame;

      if (TempFrame <> nil) and TempFrame.IsValid then TempFrame.SelectAll;
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
      aSettings.default_encoding                := CefString(FDefaultEncoding);
      aSettings.javascript                      := FOptions.Javascript;
      aSettings.javascript_close_windows        := FOptions.JavascriptCloseWindows;
      aSettings.javascript_access_clipboard     := FOptions.JavascriptAccessClipboard;
      aSettings.javascript_dom_paste            := FOptions.JavascriptDomPaste;
      aSettings.image_loading                   := FOptions.ImageLoading;
      aSettings.image_shrink_standalone_to_fit  := FOptions.ImageShrinkStandaloneToFit;
      aSettings.text_area_resize                := FOptions.TextAreaResize;
      aSettings.tab_to_links                    := FOptions.TabToLinks;
      aSettings.local_storage                   := FOptions.LocalStorage;
      aSettings.databases                       := FOptions.Databases;
      aSettings.webgl                           := FOptions.Webgl;
      aSettings.background_color                := FOptions.BackgroundColor;
      aSettings.accept_language_list            := CefString(FOptions.AcceptLanguageList);
      aSettings.chrome_status_bubble            := FOptions.ChromeStatusBubble;
    end;
end;

procedure TChromiumCore.InitializeSettings(var aSettings : TCefBrowserSettings);
begin
  FillChar(aSettings, SizeOf(TCefBrowserSettings), 0);
  aSettings.size                  := SizeOf(TCefBrowserSettings);
  aSettings.windowless_frame_rate := CEF_OSR_FRAMERATE_DEFAULT;  // Use CEF_OSR_SHARED_TEXTURES_FRAMERATE_DEFAULT if the shared textures are enabled.
end;

// Leave aFrameName empty to load the URL in the main frame
procedure TChromiumCore.LoadURL(const aURL : ustring; const aFrameName : ustring);
var
  TempFrame : ICefFrame;
begin
  if Initialized then
    begin
      if (length(aFrameName) > 0) then
        TempFrame := Browser.GetFrame(aFrameName)
       else
        TempFrame := Browser.MainFrame;

      if (TempFrame <> nil) and TempFrame.IsValid then TempFrame.LoadUrl(aURL);
    end;
end;

procedure TChromiumCore.LoadURL(const aURL : ustring; const aFrame : ICefFrame);
begin
  if Initialized and (aFrame <> nil) and aFrame.IsValid then
    aFrame.LoadUrl(aURL);
end;

procedure TChromiumCore.LoadURL(const aURL : ustring; const aFrameIdentifier : int64);
var
  TempFrame : ICefFrame;
begin
  if Initialized then
    begin
      if (aFrameIdentifier <> 0) then
        TempFrame := Browser.GetFrameByident(aFrameIdentifier)
       else
        TempFrame := Browser.MainFrame;

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
        TempFrame := Browser.GetFrame(aFrameName)
       else
        TempFrame := Browser.MainFrame;

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
        TempFrame := Browser.GetFrameByident(aFrameIdentifier)
       else
        TempFrame := Browser.MainFrame;

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
        TempFrame := Browser.GetFrame(aFrameName)
       else
        TempFrame := Browser.MainFrame;

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
        TempFrame := Browser.GetFrameByident(aFrameIdentifier)
       else
        TempFrame := Browser.MainFrame;

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
      TempFrame := Browser.MainFrame;
      if (TempFrame <> nil) and TempFrame.IsValid then TempFrame.LoadRequest(aRequest);
    end;
end;

function TChromiumCore.ExecuteBrowserNavigationTask(aTask : TCefBrowserNavigation) : boolean;
var
  TempTask : ICefTask;
begin
  Result := False;

  try
    if Initialized then
      begin
        TempTask := TCefBrowserNavigationTask.Create(self, aTask);
        Result   := CefPostTask(TID_UI, TempTask);
      end;
  finally
    TempTask := nil;
  end;
end;

procedure TChromiumCore.GoBack;
begin
  ExecuteBrowserNavigationTask(bnBack);
end;

procedure TChromiumCore.GoForward;
begin
  ExecuteBrowserNavigationTask(bnForward);
end;

procedure TChromiumCore.Reload;
begin
  ExecuteBrowserNavigationTask(bnReload);
end;

procedure TChromiumCore.ReloadIgnoreCache;
begin
  ExecuteBrowserNavigationTask(bnReloadIgnoreCache);
end;

procedure TChromiumCore.StopLoad;
begin
  ExecuteBrowserNavigationTask(bnStopLoad);
end;

procedure TChromiumCore.StartDownload(const aURL : ustring);
begin
  if Initialized then
    Browser.Host.StartDownload(aURL);
end;

procedure TChromiumCore.DownloadImage(const imageUrl     : ustring;
                                            isFavicon    : boolean;
                                            maxImageSize : cardinal;
                                            bypassCache  : boolean);
var
  TempCallback : ICefDownloadImageCallback;
begin
  try
    if Initialized then
      begin
        TempCallback := TCefCustomDownloadImageCallback.Create(self);
        Browser.Host.DownloadImage(imageUrl, isFavicon, maxImageSize, bypassCache, TempCallback);
      end;
  finally
    TempCallback := nil;
  end;
end;

function TChromiumCore.GetIsLoading : boolean;
begin
  Result := Initialized and Browser.IsLoading;
end;

function TChromiumCore.GetMultithreadApp : boolean;
begin
  Result := (GlobalCEFApp <> nil) and GlobalCEFApp.MultiThreadedMessageLoop;
end;

function TChromiumCore.GetHasDocument : boolean;
begin
  Result := Initialized and Browser.HasDocument;
end;

function TChromiumCore.GetHasView : boolean;
begin
  Result := Initialized and Browser.Host.HasView;
end;

function TChromiumCore.GetHasDevTools : boolean;
begin
  Result := Initialized and Browser.Host.HasDevTools;
end;

function TChromiumCore.GetHasClientHandler : boolean;
begin
  Result := (FHandler <> nil);
end;

function TChromiumCore.GetHasBrowser : boolean;
begin
  Result := (Browser <> nil);
end;

function TChromiumCore.GetWindowHandle : TCefWindowHandle;
begin
  InitializeWindowHandle(Result);

  if Initialized then
    Result := Browser.Host.WindowHandle;
end;

function TChromiumCore.GetOpenerWindowHandle : TCefWindowHandle;
begin
  InitializeWindowHandle(Result);

  if Initialized then
    Result := Browser.Host.OpenerWindowHandle;
end;

function TChromiumCore.GetFrameIsFocused : boolean;
begin
  Result := Initialized and (Browser.FocusedFrame <> nil);
end;

function TChromiumCore.GetWindowlessFrameRate : integer;
begin
  if Initialized then
    Result := Browser.Host.GetWindowlessFrameRate
   else
    Result := 0;
end;

function TChromiumCore.GetVisibleNavigationEntry : ICefNavigationEntry;
begin
  if Initialized then
    Result := Browser.Host.VisibleNavigationEntry
   else
    Result := nil;
end;

function TChromiumCore.GetBrowser : ICefBrowser;
begin
  Result := nil;

  if (FBrowsersCS <> nil) then
    try
      FBrowsersCS.Acquire;

      if (FBrowsers <> nil) then
        begin
          if FMultiBrowserMode then
            Result := FBrowsers.Browser[FBrowserId]
           else
            Result := FBrowsers.FirstBrowser;
        end;
    finally
      FBrowsersCS.Release;
    end;
end;

function TChromiumCore.GetBrowserId : integer;
begin
  Result := 0;

  if (FBrowsersCS <> nil) then
    try
      FBrowsersCS.Acquire;
      Result := FBrowserId;
    finally
      FBrowsersCS.Release;
    end;
end;

function TChromiumCore.GetBrowserById(aID : integer) : ICefBrowser;
begin
  Result := nil;

  if (FBrowsersCS <> nil) then
    try
      FBrowsersCS.Acquire;
      if (FBrowsers <> nil) then
        Result := FBrowsers.Browser[aID];
    finally
      FBrowsersCS.Release;
    end;
end;

function TChromiumCore.GetBrowserCount : integer;
begin
  Result := 0;

  if (FBrowsersCS <> nil) then
    try
      FBrowsersCS.Acquire;
      if (FBrowsers <> nil) then
        Result := FBrowsers.Count;
    finally
      FBrowsersCS.Release;
    end;
end;

function TChromiumCore.GetBrowserIdByIndex(aIndex : integer) : integer;
begin
  Result := 0;

  if (FBrowsersCS <> nil) then
    try
      FBrowsersCS.Acquire;
      if (FBrowsers <> nil) and (aIndex >= 0) and (aIndex < FBrowsers.Count) then
        Result := TBrowserInfo(FBrowsers[aIndex]).ID;
    finally
      FBrowsersCS.Release;
    end;
end;

{$IFDEF LINUX}
function TChromiumCore.GetXDisplay : PXDisplay;
{$IFDEF FPC}
var
  TempParent : TCefWindowHandle;
{$ENDIF}
begin
  if (FXDisplay = nil) then
    begin
      {$IFDEF FPC}
        {$IFDEF LCLGTK2}
        TempParent := ParentFormHandle;

        if ValidCefWindowHandle(TempParent) and
           (PGtkWidget(TempParent)^.Window <> nil) then
          FXDisplay := GDK_WINDOW_XDISPLAY(PGtkWidget(TempParent)^.Window);
        {$ENDIF}
        {$IFDEF LCLGTK3}
        TempParent := ParentFormHandle;
        {
        if ValidCefWindowHandle(TempParent) and
           (PGtkWidget(TempParent)^.Window <> nil) then
          FXDisplay := GDK_WINDOW_XDISPLAY(PGtkWidget(TempParent)^.Window);  }
        FXDisplay := gdk_x11_get_default_xdisplay();
        {$ENDIF}
      {$ENDIF}

      // GlobalCEFApp.XDisplay can only be called in the CEF UI thread.
      if (FXDisplay = nil) and (GlobalCEFApp <> nil) then
        FXDisplay := GlobalCEFApp.XDisplay;
    end;

  Result := FXDisplay;
end;
{$ENDIF}

function TChromiumCore.GetHasValidMainFrame : boolean;
begin
  Result := Initialized and (Browser.MainFrame <> nil) and Browser.MainFrame.IsValid;
end;

function TChromiumCore.GetFrameCount : NativeUInt;
begin
  if Initialized then
    Result := Browser.GetFrameCount
   else
    Result := 0;
end;

function TChromiumCore.GetRequestContextCache : ustring;
begin
  if Initialized then
    Result := Browser.host.RequestContext.CachePath
   else
    if (GlobalCEFApp <> nil) then
      Result := GlobalCEFApp.cache
     else
      Result := '';
end;

function TChromiumCore.GetRequestContextIsGlobal : boolean;
begin
  Result := Initialized and Browser.host.RequestContext.IsGlobal;
end;

function TChromiumCore.GetAudioMuted : boolean;
begin
  Result := Initialized and Browser.host.IsAudioMuted;
end;

function TChromiumCore.GetParentFormHandle : TCefWindowHandle;
begin
  InitializeWindowHandle(Result);
end;

procedure TChromiumCore.SetMultiBrowserMode(aValue : boolean);
begin
  if not(Initialized) then FMultiBrowserMode := aValue;
end;

procedure TChromiumCore.SetQuicAllowed(aValue : boolean);
begin
  if (FQuicAllowed <> aValue) then
    begin
      FQuicAllowed       := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TChromiumCore.SetJavascriptEnabled(aValue : boolean);
begin
  if (FJavascriptEnabled <> aValue) then
    begin
      FJavascriptEnabled := aValue;
      FUpdatePreferences := True;
    end;
end;

procedure TChromiumCore.SetLoadImagesAutomatically(aValue : boolean);
begin
  if (FLoadImagesAutomatically <> aValue) then
    begin
      FLoadImagesAutomatically := aValue;
      FUpdatePreferences       := True;
    end;
end;

procedure TChromiumCore.SetBatterySaverModeState(aValue : TCefBatterySaverModeState);
begin
  if (FBatterySaverModeState <> aValue) then
    begin
      FBatterySaverModeState := aValue;
      FUpdatePreferences     := True;
    end;
end;

procedure TChromiumCore.SetHighEfficiencyModeState(aValue : TCefHighEfficiencyModeState);
begin
  if (FHighEfficiencyModeState <> aValue) then
    begin
      FHighEfficiencyModeState := aValue;
      FUpdatePreferences       := True;
    end;
end;

procedure TChromiumCore.SetDefaultUrl(const aValue : ustring);
begin
  FDefaultUrl := trim(aValue);

  // Use 'about:blank' if FDefaultUrl is empty to avoid a memory leak when the browser is destroyed.
  // https://github.com/salvadordf/CEF4Delphi/issues/404
  if (Length(FDefaultUrl) = 0) then
    FDefaultUrl := ABOUTBLANK_URI;
end;

procedure TChromiumCore.SetAudioMuted(aValue : boolean);
var
  TempTask : ICefTask;
begin
  if CefCurrentlyOn(TID_UI) then
    doSetAudioMuted(aValue)
   else
    if Initialized then
      try
        TempTask := TCefSetAudioMutedTask.Create(self, aValue);
        CefPostTask(TID_UI, TempTask);
      finally
        TempTask := nil;
      end;
end;

procedure TChromiumCore.SetWindowlessFrameRate(aValue : integer);
begin
  if Initialized then
    Browser.Host.SetWindowlessFrameRate(aValue);
end;

function TChromiumCore.GetCanGoBack : boolean;
begin
  Result := Initialized and Browser.CanGoBack;
end;

function TChromiumCore.GetCanGoForward : boolean;
begin
  Result := Initialized and Browser.CanGoForward;
end;

function TChromiumCore.GetIsPopUp : boolean;
begin
  Result := Initialized and Browser.IsPopUp;
end;

function TChromiumCore.GetInitialized : boolean;
begin
  Result := False;

  if (FBrowsersCS <> nil) then
    try
      FBrowsersCS.Acquire;
      Result := (FBrowserId <> 0) and (FBrowsers <> nil) and FBrowsers.BrowserIsValid[FBrowserId];
    finally
      FBrowsersCS.Release;
    end;
end;

function TChromiumCore.GetDocumentURL : ustring;
var
  TempFrame : ICefFrame;
begin
  Result := '';

  if Initialized then
    begin
      TempFrame := Browser.MainFrame;
      if (TempFrame <> nil) and TempFrame.IsValid then Result := TempFrame.URL;
    end;
end;

function TChromiumCore.GetZoomLevel : double;
begin
  if Initialized then
    Result := Browser.Host.ZoomLevel
   else
    Result := 0;
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

procedure TChromiumCore.SetAlwaysOpenPDFExternally(aValue : boolean);
begin
  if (FAlwaysOpenPDFExternally <> aValue) then
    begin
      FAlwaysOpenPDFExternally := aValue;
      FUpdatePreferences       := True;
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

procedure TChromiumCore.SetOffline(aValue : boolean);
var
  TempParams : ICefDictionaryValue;
begin
  if (FOffline <> aValue) then
    try
      FOffline := aValue;

      TempParams := TCefDictionaryValueRef.New;
      TempParams.SetBool('offline', FOffline);
      TempParams.SetDouble('latency', 0);
      TempParams.SetDouble('downloadThroughput', 0);
      TempParams.SetDouble('uploadThroughput', 0);

      ExecuteDevToolsMethod(0, 'Network.emulateNetworkConditions', TempParams);
    finally
      TempParams := nil;
    end;
end;

procedure TChromiumCore.SetUserAgentOverride(const aUserAgent, aAcceptLanguage, aPlatform : ustring);
var
  TempParams : ICefDictionaryValue;
begin
  try
    TempParams := TCefDictionaryValueRef.New;
    TempParams.SetString('userAgent', aUserAgent);

    if (length(aAcceptLanguage) > 0) then
      TempParams.SetString('acceptLanguage', aAcceptLanguage);

    if (length(aPlatform) > 0) then
      TempParams.SetString('platform', aPlatform);

    ExecuteDevToolsMethod(0, 'Emulation.setUserAgentOverride', TempParams);
  finally
    TempParams := nil;
  end;
end;

procedure TChromiumCore.ClearDataForOrigin(const aOrigin : ustring; aStorageTypes : TCefClearDataStorageTypes);
var
  TempParams : ICefDictionaryValue;
begin
  try
    TempParams := TCefDictionaryValueRef.New;
    TempParams.SetString('origin', aOrigin);

    case aStorageTypes of
      cdstAppCache        : TempParams.SetString('storageTypes', 'appcache');
      cdstCookies         : TempParams.SetString('storageTypes', 'cookies');
      cdstFileSystems     : TempParams.SetString('storageTypes', 'file_systems');
      cdstIndexeddb       : TempParams.SetString('storageTypes', 'indexeddb');
      cdstLocalStorage    : TempParams.SetString('storageTypes', 'local_storage');
      cdstShaderCache     : TempParams.SetString('storageTypes', 'shader_cache');
      cdstWebsql          : TempParams.SetString('storageTypes', 'websql');
      cdstServiceWorkers  : TempParams.SetString('storageTypes', 'service_workers');
      cdstCacheStorage    : TempParams.SetString('storageTypes', 'cache_storage');
      else                  TempParams.SetString('storageTypes', 'all');
    end;

    ExecuteDevToolsMethod(0, 'Storage.clearDataForOrigin', TempParams);
  finally
    TempParams := nil;
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

procedure TChromiumCore.UpdateHostZoomLevel(const aValue : double);
begin
  if Initialized then
    Browser.Host.ZoomLevel := aValue;
end;

procedure TChromiumCore.UpdateHostZoomPct(const aValue : double);
begin
  if (aValue > 0) then
    UpdateHostZoomLevel(LogN(1.2, aValue / 100));
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

function TChromiumCore.DeleteCookies(const url, cookieName: ustring; aDeleteImmediately : boolean) : boolean;
var
  TempManager  : ICefCookieManager;
  TempCallback : ICefDeleteCookiesCallback;
  TempContext  : ICefRequestContext;
begin
  Result := False;

  if Initialized then
    begin
      TempContext := Browser.Host.RequestContext;

      if (TempContext <> nil) then
        begin
          TempManager := TempContext.GetCookieManager(nil);

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
end;

function TChromiumCore.VisitAllCookies(aID : integer) : boolean;
var
  TempManager : ICefCookieManager;
  TempVisitor : ICefCookieVisitor;
  TempContext : ICefRequestContext;
begin
  Result := False;

  if Initialized then
    begin
      TempContext := Browser.Host.RequestContext;

      if (TempContext <> nil) then
        begin
          TempManager := TempContext.GetCookieManager(nil);

          if (TempManager <> nil) then
            try
              TempVisitor := TCefCustomCookieVisitor.Create(self, aID);
              Result      := TempManager.VisitAllCookies(TempVisitor);
            finally
              TempVisitor := nil;
            end;
        end;
    end;
end;

function TChromiumCore.VisitURLCookies(const url             : ustring;
                                             includeHttpOnly : boolean;
                                             aID             : integer) : boolean;
var
  TempManager : ICefCookieManager;
  TempVisitor : ICefCookieVisitor;
  TempContext : ICefRequestContext;
begin
  Result := False;

  if Initialized then
    begin
      TempContext := Browser.Host.RequestContext;

      if (TempContext <> nil) then
        begin
          TempManager := TempContext.GetCookieManager(nil);

          if (TempManager <> nil) then
            try
              TempVisitor := TCefCustomCookieVisitor.Create(self, aID);
              Result      := TempManager.VisitUrlCookies(url, includeHttpOnly, TempVisitor);
            finally
              TempVisitor := nil;
            end;
        end;
    end;
end;

function TChromiumCore.SetCookie(const url, name_, value, domain, path: ustring;
                                       secure, httponly, hasExpires: Boolean;
                                 const creation, lastAccess, expires: TDateTime;
                                       same_site : TCefCookieSameSite;
                                       priority : TCefCookiePriority;
                                       aSetImmediately : boolean;
                                       aID : integer): Boolean;
var
  TempManager  : ICefCookieManager;
  TempCallback : ICefSetCookieCallback;
  TempContext  : ICefRequestContext;
begin
  Result := False;

  if Initialized then
    begin
      TempContext := Browser.Host.RequestContext;

      if (TempContext <> nil) then
        begin
          TempManager := TempContext.GetCookieManager(nil);

          if (TempManager <> nil) then
            try
              if aSetImmediately then
                TempCallback := nil
               else
                TempCallback := TCefCustomSetCookieCallback.Create(self, aID);

              Result := TempManager.SetCookie(url, name_, value, domain, path,
                                              secure, httponly, hasExpires,
                                              creation, lastAccess, expires,
                                              same_site, priority,
                                              TempCallback);
            finally
              TempCallback := nil;
            end;
        end;
    end;
end;

function TChromiumCore.FlushCookieStore(aFlushImmediately : boolean) : boolean;
var
  TempManager  : ICefCookieManager;
  TempCallback : ICefCompletionCallback;
  TempContext  : ICefRequestContext;
begin
  Result := False;

  if Initialized then
    begin
      TempContext := Browser.Host.RequestContext;

      if (TempContext <> nil) then
        begin
          TempManager := TempContext.GetCookieManager(nil);

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
end;

function TChromiumCore.ClearCertificateExceptions(aClearImmediately : boolean) : boolean;
var
  TempCallback : ICefCompletionCallback;
  TempContext  : ICefRequestContext;
begin
  Result := False;

  if Initialized then
    begin
      TempContext := Browser.Host.RequestContext;

      if (TempContext <> nil) then
        try
          if aClearImmediately then
            TempCallback := nil
           else
            TempCallback := TCefClearCertificateExceptionsCompletionCallback.Create(self);

          TempContext.ClearCertificateExceptions(TempCallback);
          Result := True;
        finally
          TempCallback := nil;
        end;
    end;
end;

function TChromiumCore.ClearHttpAuthCredentials(aClearImmediately : boolean) : boolean;
var
  TempCallback : ICefCompletionCallback;
  TempContext  : ICefRequestContext;
begin
  Result := False;

  if Initialized then
    begin
      TempContext := Browser.Host.RequestContext;

      if (TempContext <> nil) then
        try
          if aClearImmediately then
            TempCallback := nil
           else
            TempCallback := TCefClearHttpAuthCredentialsCompletionCallback.Create(self);

          TempContext.ClearHttpAuthCredentials(TempCallback);
          Result := True;
        finally
          TempCallback := nil;
        end;
    end;
end;

// If aCloseImmediately is false then OnAllConnectionsClosed is triggered when the connections are closed
function TChromiumCore.CloseAllConnections(aCloseImmediately : boolean) : boolean;
var
  TempCallback : ICefCompletionCallback;
  TempContext  : ICefRequestContext;
begin
  Result := False;

  if Initialized then
    begin
      TempContext := Browser.Host.RequestContext;

      if (TempContext <> nil) then
        try
          if aCloseImmediately then
            TempCallback := nil
           else
            TempCallback := TCefCloseAllConnectionsCompletionCallback.Create(self);

          TempContext.CloseAllConnections(TempCallback);
          Result := True;
        finally
          TempCallback := nil;
        end;
    end;
end;

// Leave aFrameName empty to get the HTML source from the main frame
// The TChromiumCore.OnTextResultAvailable event will be triggered with the HTML contents
procedure TChromiumCore.RetrieveHTML(const aFrameName : ustring);
var
  TempFrame   : ICefFrame;
  TempVisitor : ICefStringVisitor;
begin
  if Initialized then
    begin
      if (length(aFrameName) > 0) then
        TempFrame := Browser.GetFrame(aFrameName)
       else
        TempFrame := Browser.MainFrame;

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
  TempFrame   : ICefFrame;
  TempVisitor : ICefStringVisitor;
begin
  if Initialized then
    try
      if (aFrame <> nil) and aFrame.IsValid then
        TempFrame := aFrame
       else
        TempFrame := Browser.MainFrame;

      TempVisitor := TCustomCefStringVisitor.Create(self);
      TempFrame.GetSource(TempVisitor);
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
        TempFrame := Browser.GetFrameByident(aFrameIdentifier)
       else
        TempFrame := Browser.MainFrame;

      if (TempFrame <> nil) and TempFrame.IsValid then
        try
          TempVisitor := TCustomCefStringVisitor.Create(self);
          TempFrame.GetSource(TempVisitor);
        finally
          TempVisitor := nil;
        end;
    end;
end;

procedure TChromiumCore.RetrieveText(const aFrameName : ustring);
var
  TempFrame   : ICefFrame;
  TempVisitor : ICefStringVisitor;
begin
  if Initialized then
    begin
      if (length(aFrameName) > 0) then
        TempFrame := Browser.GetFrame(aFrameName)
       else
        TempFrame := Browser.MainFrame;

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
  TempFrame   : ICefFrame;
  TempVisitor : ICefStringVisitor;
begin
  if Initialized then
    try
      if (aFrame <> nil) and aFrame.IsValid then
        TempFrame := aFrame
       else
        TempFrame := Browser.MainFrame;

      TempVisitor := TCustomCefStringVisitor.Create(self);
      TempFrame.GetText(TempVisitor);
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
        TempFrame := Browser.GetFrameByident(aFrameIdentifier)
       else
        TempFrame := Browser.MainFrame;

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
      Browser.Host.GetNavigationEntries(TempVisitor, currentOnly);
    finally
      TempVisitor := nil;
    end;
end;

function TChromiumCore.GetFrameNames(var aFrameNames : TStrings) : boolean;
begin
  Result := Initialized and Browser.GetFrameNames(aFrameNames);
end;

function TChromiumCore.GetFrameIdentifiers(var aFrameCount : NativeUInt; var aFrameIdentifierArray : TCefFrameIdentifierArray) : boolean;
begin
  Result := Initialized and Browser.GetFrameIdentifiers(aFrameCount, aFrameIdentifierArray);
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

{$IFDEF MSWINDOWS}
function TChromiumCore.SetNewBrowserParent(aNewParentHwnd : HWND) : boolean;
var
  TempHandle : HWND;
begin
  Result := False;

  if Initialized then
    begin
      TempHandle := Browser.Host.WindowHandle;
      Result     := (TempHandle <> 0) and (SetParent(TempHandle, aNewParentHwnd) <> 0);
    end;
end;
{$ENDIF}

procedure TChromiumCore.ResolveHost(const aURL : ustring);
var
  TempCallback : ICefResolveCallback;
begin
  if Initialized and (length(aURL) > 0) then
    try
      TempCallback := TCefCustomResolveCallback.Create(self);
      Browser.Host.RequestContext.ResolveHost(aURL, TempCallback);
    finally
      TempCallback := nil;
    end;
end;

function TChromiumCore.IsSameBrowser(const aBrowser : ICefBrowser) : boolean;
begin
  Result := Initialized and (aBrowser <> nil) and Browser.IsSame(aBrowser);
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

procedure TChromiumCore.ClearCache;
begin
  ExecuteDevToolsMethod(0, 'Network.clearBrowserCache', nil);
end;

procedure TChromiumCore.ToggleAudioMuted;
var
  TempTask : ICefTask;
begin
  if CefCurrentlyOn(TID_UI) then
    doToggleAudioMuted
   else
    if Initialized then
      try
        TempTask := TCefToggleAudioMutedTask.Create(self);
        CefPostTask(TID_UI, TempTask);
      finally
        TempTask := nil;
      end;
end;

function TChromiumCore.GetRequestContext : ICefRequestContext;
begin
  if Initialized then
    Result := Browser.Host.RequestContext
   else
    Result := nil;
end;

function TChromiumCore.GetMediaRouter : ICefMediaRouter;
var
  TempRequestContext : ICefRequestContext;
begin
  TempRequestContext := RequestContext;

  if (TempRequestContext <> nil) then
    Result := TempRequestContext.GetMediaRouter(nil)
   else
    Result := nil;
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
      Browser.Host.SendMouseWheelEvent(@TempEvent, aDeltaX, aDeltaY);
    end;
end;

procedure TChromiumCore.doUpdatePreferences(const aBrowser: ICefBrowser);
var
  TempLanguagesList : ustring;
begin
  FUpdatePreferences := False;

  // The preferences registered in CEF are defined in :
  // /libcef/browser/prefs/browser_prefs.cc

  UpdateProxyPrefs(aBrowser);
  UpdatePreference(aBrowser, 'enable_do_not_track',                  FDoNotTrack);
  UpdatePreference(aBrowser, 'enable_referrers',                     FSendReferrer);
  UpdatePreference(aBrowser, 'enable_a_ping',                        FHyperlinkAuditing);
  UpdatePreference(aBrowser, 'plugins.allow_outdated',               FAllowOutdatedPlugins);
  UpdatePreference(aBrowser, 'plugins.always_authorize',             FAlwaysAuthorizePlugins);
  UpdatePreference(aBrowser, 'plugins.always_open_pdf_externally',   FAlwaysOpenPDFExternally);
  UpdatePreference(aBrowser, 'browser.enable_spellchecking',         FSpellChecking);
  UpdateStringListPref(aBrowser, 'spellcheck.dictionaries',          FSpellCheckerDicts);
  UpdatePreference(aBrowser, 'settings.force_google_safesearch',     FSafeSearch);
  UpdatePreference(aBrowser, 'settings.force_youtube_restrict',      FYouTubeRestrict);
  UpdatePreference(aBrowser, 'printing.enabled',                     FPrintingEnabled);

  TempLanguagesList := FAcceptLanguageList;

  if (length(TempLanguagesList) = 0) and (FOptions <> nil) then
    TempLanguagesList := FOptions.AcceptLanguageList;

  if (length(TempLanguagesList) = 0) then
    TempLanguagesList := GlobalCEFApp.AcceptLanguageList;

  if (length(TempLanguagesList) = 0) then
    TempLanguagesList := 'en-US,en';

  UpdatePreference(aBrowser, 'intl.accept_languages', TempLanguagesList);

  case FAcceptCookies of
    cpAllow : UpdatePreference(aBrowser, 'profile.default_content_setting_values.cookies', CEF_COOKIE_PREF_ALLOW);
    cpBlock : UpdatePreference(aBrowser, 'profile.default_content_setting_values.cookies', CEF_COOKIE_PREF_BLOCK);
    else      UpdatePreference(aBrowser, 'profile.default_content_setting_values.cookies', CEF_COOKIE_PREF_DEFAULT);
  end;

  UpdatePreference(aBrowser, 'profile.block_third_party_cookies', FBlock3rdPartyCookies);

  if (FMaxConnectionsPerProxy <> CEF_MAX_CONNECTIONS_PER_PROXY_DEFAULT_VALUE) then
    UpdatePreference(aBrowser, 'net.max_connections_per_proxy', FMaxConnectionsPerProxy);

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

  UpdatePreference(aBrowser, 'net.quic_allowed',               FQuicAllowed);

  UpdatePreference(aBrowser, 'webkit.webprefs.javascript_enabled',         FJavascriptEnabled);
  UpdatePreference(aBrowser, 'webkit.webprefs.loads_images_automatically', FLoadImagesAutomatically);

  if (FHighEfficiencyModeState <> kDefault) then
    UpdatePreference(aBrowser, 'performance_tuning.high_efficiency_mode.state', integer(FHighEfficiencyModeState));

  if (FBatterySaverModeState <> bsmsDefault) then
    UpdatePreference(aBrowser, 'performance_tuning.battery_saver_mode.state', integer(FBatterySaverModeState));

  if assigned(FOnPrefsUpdated) then
    FOnPrefsUpdated(self);
end;

procedure TChromiumCore.doUpdateOwnPreferences;
begin
  if Initialized then doUpdatePreferences(Browser);
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

function TChromiumCore.doSavePreferences : boolean;
begin
  Result := Initialized and
            TCEFJson.SaveToFile(Browser.Host.RequestContext.GetAllPreferences(True), FPrefsFileName);

  {$IFDEF MSWINDOWS}
  SendCompMessage(CEF_PREFERENCES_SAVED, Ord(Result));
  {$ENDIF}
end;

procedure TChromiumCore.doResolvedHostAvailable(result: TCefErrorCode; const resolvedIps: TStrings);
begin
  if assigned(FOnResolvedHostAvailable) then
    FOnResolvedHostAvailable(self, result, resolvedIps);
end;

function TChromiumCore.doNavigationVisitorResultAvailable(const entry   : ICefNavigationEntry;
                                                                current : Boolean;
                                                                index   : Integer;
                                                                total   : Integer) : boolean;
begin
  Result := False;

  if assigned(FOnNavigationVisitorResultAvailable) then
    FOnNavigationVisitorResultAvailable(self, entry, current, index, total, Result);
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
  if assigned(FOnCookiesFlushed) then
    FOnCookiesFlushed(self);
end;

procedure TChromiumCore.doCertificateExceptionsCleared;
begin
  if assigned(FOnCertificateExceptionsCleared) then
    FOnCertificateExceptionsCleared(self);
end;

procedure TChromiumCore.doHttpAuthCredentialsCleared;
begin
  if assigned(FOnHttpAuthCredentialsCleared) then
    FOnHttpAuthCredentialsCleared(self);
end;

procedure TChromiumCore.doAllConnectionsClosed;
begin
  if assigned(FOnAllConnectionsClosed) then
    FOnAllConnectionsClosed(self);
end;

procedure TChromiumCore.doOnExecuteTaskOnCefThread(aTaskID : cardinal);
begin
  if assigned(FOnExecuteTaskOnCefThread) then
    FOnExecuteTaskOnCefThread(self, aTaskID);
end;

procedure TChromiumCore.doOnCookiesVisited(const name_, value, domain, path: ustring;
                                                 secure, httponly, hasExpires: Boolean;
                                           const creation, lastAccess, expires: TDateTime;
                                                 count, total, aID : Integer;
                                                 same_site : TCefCookieSameSite;
                                                 priority : TCefCookiePriority;
                                           var   aDeleteCookie, aResult : Boolean);
begin
  if assigned(FOnCookiesVisited) then
    FOnCookiesVisited(self, name_, value, domain, path,
                      secure, httponly, hasExpires,
                      creation, lastAccess, expires,
                      count, total, aID,
                      same_site, priority,
                      aDeleteCookie, aResult);
end;

procedure TChromiumCore.doOnCookieVisitorDestroyed(aID : integer);
begin
  if assigned(FOnCookieVisitorDestroyed) then
    FOnCookieVisitorDestroyed(self, aID);
end;

procedure TChromiumCore.doOnCookieSet(aSuccess : boolean; aID : integer);
begin
  if assigned(FOnCookieSet) then
    FOnCookieSet(self, aSuccess, aID);
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

    if assigned(FOnZoomPctAvailable) then
      FOnZoomPctAvailable(self, ZoomPct);
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

    if assigned(FOnZoomPctAvailable) then
      FOnZoomPctAvailable(self, TempNewZoom);
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

    if assigned(FOnZoomPctAvailable) then
      FOnZoomPctAvailable(self, ZoomPct);
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

    if assigned(FOnZoomPctAvailable) then
      FOnZoomPctAvailable(self, ZoomPct);
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

    if assigned(FOnZoomPctAvailable) then
      FOnZoomPctAvailable(self, ZoomPct);
  end;
end;

procedure TChromiumCore.doMediaRouteCreateFinished(      result : TCefMediaRouterCreateResult;
                                                   const error  : ustring;
                                                   const route  : ICefMediaRoute);
begin
  if assigned(FOnMediaRouteCreateFinished) then
    FOnMediaRouteCreateFinished(self, result, error, route);
end;

procedure TChromiumCore.doOnMediaSinkDeviceInfo(const ip_address: ustring; port: integer; const model_name: ustring);
begin
  if assigned(FOnMediaSinkDeviceInfo) then
    FOnMediaSinkDeviceInfo(self, ip_address, port, model_name);
end;

procedure TChromiumCore.doBrowserNavigation(aTask : TCefBrowserNavigation);
begin
  if Initialized then
    case aTask of
      bnBack              : if CanGoBack    then Browser.GoBack;
      bnForward           : if CanGoForward then Browser.GoForward;
      bnReload            : Browser.Reload;
      bnReloadIgnoreCache : Browser.ReloadIgnoreCache;
      bnStopLoad          : Browser.StopLoad;
    end;
end;

procedure TChromiumCore.doSetAudioMuted(aValue : boolean);
begin
  if Initialized then
    Browser.Host.SetAudioMuted(aValue);
end;

procedure TChromiumCore.doToggleAudioMuted;
begin
  if Initialized then
    AudioMuted := not(AudioMuted);
end;

procedure TChromiumCore.doEnableFocus;
begin
  FCanFocus := True;

  if assigned(FOnCanFocus) then
    FOnCanFocus(self);
end;

{$IFDEF LINUX}
procedure TChromiumCore.UpdateBrowserSize(aLeft, aTop, aWidth, aHeight : integer);
{$IFDEF FPC}
var
  TempHandle   : TCefWindowHandle;
  TempChanges  : TXWindowChanges;
  TempXDisplay : PXDisplay;
{$ENDIF}
begin
  {$IFDEF FPC}
  TempHandle := WindowHandle;

  if ValidCefWindowHandle(TempHandle) then
    begin
      TempXDisplay := XDisplay;

      if (TempXDisplay <> nil) then
        begin
          TempChanges.x      := aLeft;
          TempChanges.y      := aTop;
          TempChanges.width  := aWidth;
          TempChanges.height := aHeight;

          XConfigureWindow(TempXDisplay, TempHandle, CWX or CWY or CWHeight or CWWidth, @TempChanges);
        end;
    end;
  {$ENDIF}
end;

procedure TChromiumCore.UpdateXWindowVisibility(aVisible : boolean);
{$IFDEF FPC}
var
  TempXDisplay : PXDisplay;
  TempHandle   : TCefWindowHandle;
  TempState    : TAtom;
  TempHidden   : TAtom;
{$ENDIF}
begin
  {$IFDEF FPC}
  TempHandle := WindowHandle;

  if ValidCefWindowHandle(TempHandle) then
    begin
      TempXDisplay := XDisplay;

      if (TempXDisplay <> nil) then
        begin
          TempState := XInternAtom(TempXDisplay, '_NET_WM_STATE', False);

          if aVisible then
            XChangeProperty(TempXDisplay, TempHandle, TempState, XA_ATOM, 32, PropModeReplace, nil, 0)
           else
            begin
              TempHidden := XInternAtom(TempXDisplay, '_NET_WM_STATE_HIDDEN', False);
              XChangeProperty(TempXDisplay, TempHandle, TempState, XA_ATOM, 32, PropModeReplace, @TempHidden, 1);
            end;
        end;
    end;
  {$ENDIF}
end;
{$ENDIF}

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
  Result := assigned(FOnBeforeContextMenu)    or
            assigned(FOnRunContextMenu)       or
            assigned(FOnContextMenuCommand)   or
            assigned(FOnContextMenuDismissed) or
            assigned(FOnRunQuickMenu)         or
            assigned(FOnQuickMenuCommand)     or
            assigned(FOnQuickMenuDismissed);
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
            assigned(FOnLoadingProgressChange) or
            assigned(FOnCursorChange)          or
            assigned(FOnMediaAccessChange);
end;

function TChromiumCore.MustCreateDownloadHandler : boolean;
begin
  Result := assigned(FOnCanDownload)    or
            assigned(FOnBeforeDownload) or
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

function TChromiumCore.MustCreateMediaObserver : boolean;
begin
  Result := assigned(FOnSinks)             or
            assigned(FOnRoutes)            or
            assigned(FOnRouteStateChanged) or
            assigned(FOnRouteMessageReceived);
end;

function TChromiumCore.MustCreateAudioHandler : boolean;
begin
  Result := assigned(FOnGetAudioParameters) or
            assigned(FOnAudioStreamStarted) or
            assigned(FOnAudioStreamPacket)  or
            assigned(FOnAudioStreamStopped) or
            assigned(FOnAudioStreamError);
end;

function TChromiumCore.MustCreateDevToolsMessageObserver : boolean;
begin
  Result := assigned(FOnDevToolsMessage)         or
            assigned(FOnDevToolsRawMessage)      or
            assigned(FOnDevToolsMethodResult)    or
            assigned(FOnDevToolsMethodRawResult) or
            assigned(FOnDevToolsEvent)           or
            assigned(FOnDevToolsRawEvent)        or
            assigned(FOnDevToolsAgentAttached)   or
            assigned(FOnDevToolsAgentDetached);
end;

function TChromiumCore.MustCreateExtensionHandler : boolean;
begin
  Result := assigned(FOnExtensionLoadFailed)              or
            assigned(FOnExtensionLoaded)                  or
            assigned(FOnExtensionUnloaded)                or
            assigned(FOnExtensionBeforeBackgroundBrowser) or
            assigned(FOnExtensionBeforeBrowser)           or
            assigned(FOnExtensionGetActiveBrowser)        or
            assigned(FOnExtensionCanAccessBrowser)        or
            assigned(FOnExtensionGetExtensionResource);
end;

function TChromiumCore.MustCreatePrintHandler : boolean;
begin
  Result := assigned(FOnPrintStart)    or
            assigned(FOnPrintSettings) or
            assigned(FOnPrintDialog)   or
            assigned(FOnPrintJob)      or
            assigned(FOnPrintReset)    or
            assigned(FOnGetPDFPaperSize);
end;

function TChromiumCore.MustCreateFrameHandler : boolean;
begin
  Result := assigned(FOnFrameCreated)  or
            assigned(FOnFrameAttached) or
            assigned(FOnFrameDetached) or
            assigned(FOnMainFrameChanged);
end;

function TChromiumCore.MustCreatePermissionHandler : boolean;
begin
  Result := assigned(FOnRequestMediaAccessPermission) or
            assigned(FOnShowPermissionPrompt)         or
            assigned(FOnDismissPermissionPrompt);
end;

function TChromiumCore.MustCreateCommandHandler : boolean;
begin
  Result := assigned(FOnChromeCommand) or
            assigned(FOnIsChromeAppMenuItemVisible) or
            assigned(FOnIsChromeAppMenuItemEnabled) or
            assigned(FOnIsChromePageActionIconVisible) or
            assigned(FOnIsChromeToolbarButtonVisible);
end;

{$IFDEF MSWINDOWS}
procedure TChromiumCore.PrefsAvailableMsg(aResultOK : boolean);
begin
  if assigned(FOnPrefsAvailable) then
    FOnPrefsAvailable(self, aResultOK);
end;

function TChromiumCore.SendCompMessage(aMsg : cardinal; aWParam : WPARAM; aLParam : LPARAM) : boolean;
begin
  Result := (FCompHandle <> 0) and PostMessage(FCompHandle, aMsg, aWParam, aLParam);
end;
{$ENDIF}

procedure TChromiumCore.doTextResultAvailable(const aText : ustring);
begin
  if assigned(FOnTextResultAvailable) then
    FOnTextResultAvailable(self, aText);
end;

procedure TChromiumCore.ExecuteJavaScript(const aCode, aScriptURL, aFrameName : ustring; aStartLine : integer);
var
  TempFrame : ICefFrame;
begin
  try
    if Initialized then
      begin
        if (length(aFrameName) > 0) then
          TempFrame := Browser.GetFrame(aFrameName)
         else
          TempFrame := Browser.MainFrame;

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
          TempFrame := Browser.GetFrameByident(aFrameIdentifier)
         else
          TempFrame := Browser.MainFrame;

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
  if assigned(FOnCookiesDeleted) then
    FOnCookiesDeleted(self, numDeleted);
end;

procedure TChromiumCore.doPdfPrintFinished(aResultOK : boolean);
begin
  if assigned(FOnPdfPrintFinished) then
    FOnPdfPrintFinished(self, aResultOK);
end;

procedure TChromiumCore.ShowDevTools(const inspectElementAt: TPoint; aWindowInfo: PCefWindowInfo);
var
  TempPoint  : TCefPoint;
  TempClient : ICefClient;
  TempPPoint : PCefPoint;
  TempHandle : TCefWindowHandle;
begin
  try
    try
      if Initialized then
        begin
          InitializeSettings(FDevBrowserSettings);

          if (aWindowInfo = nil) then
            begin
              InitializeWindowHandle(TempHandle);
              DefaultInitializeDevToolsWindowInfo(TempHandle, Rect(0, 0, 0, 0), '');
            end
           else
            if (aWindowInfo <> @FDevWindowInfo) then
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

          Browser.Host.ShowDevTools(@FDevWindowInfo, TempClient, @FDevBrowserSettings, TempPPoint);
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TChromiumCore.ShowDevTools', e) then raise;
    end;
  finally
    TempClient := nil;
  end;
end;

procedure TChromiumCore.CloseDevTools;
begin
  if Initialized then
    Browser.Host.CloseDevTools;
end;

procedure TChromiumCore.CloseDevTools(const aDevToolsWnd : TCefWindowHandle);
begin
  if Initialized then
    begin
      {$IFDEF MSWINDOWS}
      if ValidCefWindowHandle(aDevToolsWnd) then
        SetParent(GetWindow(aDevToolsWnd, GW_CHILD), 0);
      {$ENDIF}

      Browser.Host.CloseDevTools;
    end;
end;

function TChromiumCore.SendDevToolsMessage(const message_: ustring): boolean;
begin
  Result := Initialized and Browser.Host.SendDevToolsMessage(message_);
end;

function TChromiumCore.ExecuteDevToolsMethod(message_id: integer; const method: ustring; const params: ICefDictionaryValue): Integer;
begin
  if Initialized then
    Result := Browser.Host.ExecuteDevToolsMethod(message_id, method, params)
   else
    Result := 0;
end;

function TChromiumCore.AddDevToolsMessageObserver(const observer: ICefDevToolsMessageObserver): ICefRegistration;
begin
  if Initialized then
    Result := Browser.Host.AddDevToolsMessageObserver(observer)
   else
    Result := nil;
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
        FOnBrowserCompMsg(self, aMessage, TempHandled);

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
        FOnWidgetCompMsg(self, aMessage, TempHandled);

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
        FOnRenderCompMsg(self, aMessage, TempHandled);

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

procedure TChromiumCore.RestoreOldCompWndProc;
begin
  RestoreCompWndProc(FBrowserCompHWND, 0, FOldBrowserCompWndPrc);
  FreeAndNilStub(FBrowserCompStub);

  RestoreCompWndProc(FWidgetCompHWND, 0, FOldWidgetCompWndPrc);
  FreeAndNilStub(FWidgetCompStub);

  RestoreCompWndProc(FRenderCompHWND, 0, FOldRenderCompWndPrc);
  FreeAndNilStub(FRenderCompStub);
end;
{$ENDIF}

function TChromiumCore.RemoveBrowser(const aBrowser : ICefBrowser) : boolean;
begin
  Result := False;

  if (FBrowsersCS <> nil) then
    try
      FBrowsersCS.Acquire;

      if (FBrowsers <> nil) and
         FBrowsers.RemoveBrowser(aBrowser) and
         (FBrowserId = aBrowser.Identifier) then
        FBrowserId := FBrowsers.FirstID;
    finally
      FBrowsersCS.Release;
    end;
end;

function TChromiumCore.AddBrowser(const aBrowser : ICefBrowser) : boolean;
begin
  Result := False;

  if (FBrowsersCS <> nil) then
    try
      FBrowsersCS.Acquire;

      if (FBrowsers <> nil) and FBrowsers.AddBrowser(aBrowser) then
        begin
          Result := True;

          if (FBrowserId = 0) then
            FBrowserId := aBrowser.Identifier;
        end;
    finally
      FBrowsersCS.Release;
    end;
end;

function TChromiumCore.SelectBrowser(aID : integer) : boolean;
begin
  Result := False;

  if (FBrowsersCS <> nil) then
    try
      FBrowsersCS.Acquire;

      if FMultiBrowserMode and (FBrowsers <> nil) and (FBrowsers.SearchBrowser(aID) >= 0) then
        begin
          FBrowserId := aID;
          Result     := True;
        end;
    finally
      FBrowsersCS.Release;
    end;
end;

function TChromiumCore.IndexOfBrowserID(aID : integer) : integer;
begin
  Result := -1;

  if (FBrowsersCS <> nil) then
    try
      FBrowsersCS.Acquire;

      if (FBrowsers <> nil) then
        Result := FBrowsers.SearchBrowser(aID);
    finally
      FBrowsersCS.Release;
    end;
end;

procedure TChromiumCore.SetBrowserIsClosing(aID : integer);
begin
  if (FBrowsersCS <> nil) then
    try
      FBrowsersCS.Acquire;
      if (FBrowsers <> nil) then
        FBrowsers.BrowserIsClosing[aID] := True;
    finally
      FBrowsersCS.Release;
    end;
end;

function TChromiumCore.doOnClose(const browser: ICefBrowser): Boolean;
var
  TempAction : TCefCloseBrowserAction;
  TempID     : Integer;
begin
  Result     := False;
  TempAction := cbaClose;
  TempID     := browser.Identifier;

  // TempAction values
  // -----------------
  // cbaCancel : stop closing the browser
  // cbaClose  : continue closing the browser
  // cbaDelay  : stop closing the browser momentarily. Used when the application
  //             needs to execute some custom processes before closing the
  //             browser. This is usually needed to destroy a TCEFWindowParent
  //             in the main thread before closing the browser.
  if assigned(FOnClose) then
    FOnClose(Self, browser, TempAction);

  case TempAction of
    cbaCancel : Result := True;

    cbaDelay :
      begin
        Result := True;
        SetBrowserIsClosing(TempID);
      end;

    else
      SetBrowserIsClosing(TempID);
  end;
end;

procedure TChromiumCore.doOnBeforeClose(const browser: ICefBrowser);
begin
  RemoveBrowser(browser);

  if (BrowserCount = 0) then
    DestroyAllHandlersAndObservers;

  if assigned(FOnBeforeClose) then
    FOnBeforeClose(Self, browser);
end;

procedure TChromiumCore.doOnAddressChange(const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
begin
  if assigned(FOnAddressChange) then
    FOnAddressChange(Self, browser, frame, url);
end;

procedure TChromiumCore.doOnAfterCreated(const browser: ICefBrowser);
var
  TempTask : ICefTask;
begin
  AddBrowser(browser);
  doUpdatePreferences(browser);

  if (FMediaObserver <> nil) and (FMediaObserverReg = nil) then
    FMediaObserverReg := AddObserver(FMediaObserver);

  if (FDevToolsMsgObserver <> nil) and (FDevToolsMsgObserverReg = nil) then
    FDevToolsMsgObserverReg := AddDevToolsMessageObserver(FDevToolsMsgObserver);

  if assigned(FOnAfterCreated) then
    FOnAfterCreated(Self, browser);

  try
    // The browser requires some time to create associated internal objects
    // before being able to accept the focus.
    // https://bitbucket.org/chromiumembedded/cef/src/14dd0c0d06166d8198980b7fd5ed2d5f526e8990/tests/cefclient/browser/osr_window_win.cc#lines-949
    TempTask := TCefEnableFocusTask.Create(self);

    if (FEnableFocusDelayMs = 0) then
      CefPostTask(TID_UI, TempTask)
     else    
      CefPostDelayedTask(TID_UI, TempTask, FEnableFocusDelayMs);
  finally
    TempTask := nil;
  end;
end;

function TChromiumCore.doOnBeforeBrowse(const browser      : ICefBrowser;
                                        const frame        : ICefFrame;
                                        const request      : ICefRequest;
                                              user_gesture : Boolean;
                                              isRedirect   : Boolean): Boolean;
begin
  Result := False;

  if FUpdatePreferences then
    doUpdatePreferences(browser);

  if assigned(FOnBeforeBrowse) then
    FOnBeforeBrowse(Self, browser, frame, request, user_gesture, isRedirect, Result);
end;

procedure TChromiumCore.doOnBeforeContextMenu(const browser : ICefBrowser;
                                              const frame   : ICefFrame;
                                              const params  : ICefContextMenuParams;
                                              const model   : ICefMenuModel);
begin
  if assigned(FOnBeforeContextMenu) then
    FOnBeforeContextMenu(Self, browser, frame, params, model);
end;

function TChromiumCore.doRunContextMenu(const browser  : ICefBrowser;
                                        const frame    : ICefFrame;
                                        const params   : ICefContextMenuParams;
                                        const model    : ICefMenuModel;
                                        const callback : ICefRunContextMenuCallback): Boolean;
begin
  Result := False;

  if assigned(FOnRunContextMenu) then
    FOnRunContextMenu(Self, browser, frame, params, model, callback, Result);
end;

function TChromiumCore.doOnCanDownload(const browser        : ICefBrowser;
                                       const url            : ustring;
                                       const request_method : ustring): boolean;
begin
  Result := True;

  if assigned(FOnCanDownload) then
    FOnCanDownload(Self, browser, url, request_method, Result);
end;

procedure TChromiumCore.doOnBeforeDownload(const browser       : ICefBrowser;
                                           const downloadItem  : ICefDownloadItem;
                                           const suggestedName : ustring;
                                           const callback      : ICefBeforeDownloadCallback);
begin
  if assigned(FOnBeforeDownload) then
    FOnBeforeDownload(Self, browser, downloadItem, suggestedName, callback);
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

  if assigned(FOnBeforePopup) then
    FOnBeforePopup(Self, browser, frame, targetUrl, targetFrameName,
                   targetDisposition, userGesture, popupFeatures, windowInfo, client,
                   settings, extra_info, noJavascriptAccess, Result);
end;

function TChromiumCore.doOnBeforeResourceLoad(const browser  : ICefBrowser;
                                              const frame    : ICefFrame;
                                              const request  : ICefRequest;
                                              const callback : ICefCallback): TCefReturnValue;
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

  if not(FSendReferrer) then
    request.SetReferrer('', REFERRER_POLICY_NO_REFERRER);

  Result := RV_CONTINUE;

  if assigned(FOnBeforeResourceLoad) then
    FOnBeforeResourceLoad(Self, browser, frame, request, callback, Result);
end;

function TChromiumCore.doOnBeforeUnloadDialog(const browser     : ICefBrowser;
                                              const messageText : ustring;
                                                    isReload    : Boolean;
                                              const callback    : ICefJsDialogCallback): Boolean;
begin
  Result := False;

  if assigned(FOnBeforeUnloadDialog) then
    FOnBeforeUnloadDialog(Self, browser, messageText, isReload, callback, Result);
end;

function TChromiumCore.doOnCertificateError(const browser    : ICefBrowser;
                                                  certError  : TCefErrorcode;
                                            const requestUrl : ustring;
                                            const sslInfo    : ICefSslInfo;
                                            const callback   : ICefCallback): Boolean;
begin
  Result := False;

  if assigned(FOnCertificateError) then
    FOnCertificateError(Self, browser, certError, requestUrl, sslInfo, callback, Result);
end;

function TChromiumCore.doOnConsoleMessage(const browser  : ICefBrowser;
                                                level    : TCefLogSeverity;
                                          const aMessage : ustring;
                                          const source   : ustring;
                                                line     : Integer): Boolean;
begin
  Result := False;

  if assigned(FOnConsoleMessage) then
    FOnConsoleMessage(Self, browser, level, aMessage, source, line, Result);
end;

function TChromiumCore.doOnAutoResize(const browser  : ICefBrowser;
                                      const new_size : PCefSize): Boolean;
begin
  Result := False;

  if assigned(FOnAutoResize) then
    FOnAutoResize(Self, browser, new_size, Result);
end;

procedure TChromiumCore.doOnLoadingProgressChange(const browser: ICefBrowser; const progress: double);
begin
  if assigned(FOnLoadingProgressChange) then
    FOnLoadingProgressChange(self, browser, progress);
end;

function TChromiumCore.doOnContextMenuCommand(const browser    : ICefBrowser;
                                              const frame      : ICefFrame;
                                              const params     : ICefContextMenuParams;
                                                    commandId  : Integer;
                                                    eventFlags : TCefEventFlags): Boolean;
begin
  Result := False;

  if assigned(FOnContextMenuCommand) then
    FOnContextMenuCommand(Self, browser, frame, params, commandId, eventFlags, Result);
end;

procedure TChromiumCore.doOnContextMenuDismissed(const browser: ICefBrowser; const frame: ICefFrame);
begin
  if assigned(FOnContextMenuDismissed) then
    FOnContextMenuDismissed(Self, browser, frame);
end;

function TChromiumCore.doRunQuickMenu(const browser          : ICefBrowser;
                                      const frame            : ICefFrame;
                                            location         : PCefPoint;
                                            size             : PCefSize;
                                            edit_state_flags : TCefQuickMenuEditStateFlags;
                                      const callback         : ICefRunQuickMenuCallback): boolean;
begin
  Result := False;

  if assigned(FOnRunQuickMenu) then
    FOnRunQuickMenu(Self, browser, frame, location^, size^, edit_state_flags, callback, Result);
end;

function TChromiumCore.doOnQuickMenuCommand(const browser     : ICefBrowser;
                                            const frame       : ICefFrame;
                                                  command_id  : integer;
                                                  event_flags : TCefEventFlags): boolean;
begin
  Result := False;

  if assigned(FOnQuickMenuCommand) then
    FOnQuickMenuCommand(Self, browser, frame, command_id, event_flags, Result);
end;

procedure TChromiumCore.doOnQuickMenuDismissed(const browser : ICefBrowser;
                                               const frame   : ICefFrame);
begin
  if assigned(FOnQuickMenuDismissed) then
    FOnQuickMenuDismissed(Self, browser, frame);
end;

procedure TChromiumCore.doOnCursorChange(const browser          : ICefBrowser;
                                               cursor_          : TCefCursorHandle;
                                               cursorType       : TCefCursorType;
                                         const customCursorInfo : PCefCursorInfo;
                                         var   aResult          : boolean);
begin
  aResult := False;

  if assigned(FOnCursorChange) then
    FOnCursorChange(self, browser, cursor_, cursorType, customCursorInfo, aResult);
end;

procedure TChromiumCore.doOnMediaAccessChange(const browser: ICefBrowser; has_video_access, has_audio_access: boolean);
begin
  if assigned(FOnMediaAccessChange) then
    FOnMediaAccessChange(self, browser, has_video_access, has_audio_access);
end;

procedure TChromiumCore.doOnDialogClosed(const browser: ICefBrowser);
begin
  if assigned(FOnDialogClosed) then
    FOnDialogClosed(Self, browser);
end;

procedure TChromiumCore.doOnDownloadUpdated(const browser      : ICefBrowser;
                                            const downloadItem : ICefDownloadItem;
                                            const callback     : ICefDownloadItemCallback);
begin
  if assigned(FOnDownloadUpdated) then
    FOnDownloadUpdated(Self, browser, downloadItem, callback);
end;

function TChromiumCore.doOnDragEnter(const browser  : ICefBrowser;
                                     const dragData : ICefDragData;
                                           mask     : TCefDragOperations): Boolean;
begin
  Result := False;

  if assigned(FOnDragEnter) then
    FOnDragEnter(Self, browser, dragData, mask, Result);
end;

procedure TChromiumCore.doOnDraggableRegionsChanged(const browser      : ICefBrowser;
                                                    const frame        : ICefFrame;
                                                          regionsCount : NativeUInt;
                                                    const regions      : PCefDraggableRegionArray);
begin
  if assigned(FOnDraggableRegionsChanged) then
    FOnDraggableRegionsChanged(Self, browser, frame, regionsCount, regions);
end;

procedure TChromiumCore.doOnFaviconUrlChange(const browser: ICefBrowser; const iconUrls: TStrings);
begin
  if assigned(FOnFavIconUrlChange) then
    FOnFavIconUrlChange(Self, browser, iconUrls);
end;

function TChromiumCore.doOnFileDialog(const browser              : ICefBrowser;
                                            mode                 : TCefFileDialogMode;
                                      const title                : ustring;
                                      const defaultFilePath      : ustring;
                                      const acceptFilters        : TStrings;
                                      const callback             : ICefFileDialogCallback): Boolean;
begin
  Result := False;

  if assigned(FOnFileDialog) then
    FOnFileDialog(Self, browser, mode, title, defaultFilePath, acceptFilters, callback, Result);
end;

procedure TChromiumCore.doOnFindResult(const browser            : ICefBrowser;
                                             identifier         : integer;
                                             count              : Integer;
                                       const selectionRect      : PCefRect;
                                             activeMatchOrdinal : Integer;
                                             finalUpdate        : Boolean);
begin
  if assigned(FOnFindResult) then
    FOnFindResult(Self, browser, identifier, count, selectionRect, activeMatchOrdinal, finalUpdate);
end;

procedure TChromiumCore.doOnRequestContextInitialized(const request_context: ICefRequestContext);
begin
  if assigned(FOnRequestContextInitialized) then
    FOnRequestContextInitialized(self, request_context);
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

  if assigned(FOnGetResourceRequestHandler_ReqCtxHdlr) then
    FOnGetResourceRequestHandler_ReqCtxHdlr(self, browser, frame, request, is_navigation, is_download,
                                            request_initiator, disable_default_handling,
                                            aResourceRequestHandler);
end;

procedure TChromiumCore.doOnSinks(const sinks: TCefMediaSinkArray);
begin
  if assigned(FOnSinks) then
    FOnSinks(self, sinks);
end;

procedure TChromiumCore.doOnRoutes(const routes: TCefMediaRouteArray);
begin
  if assigned(FOnRoutes) then
    FOnRoutes(self, routes);
end;

procedure TChromiumCore.doOnRouteStateChanged(const route : ICefMediaRoute;
                                                    state : TCefMediaRouteConnectionState);
begin
  if assigned(FOnRouteStateChanged) then
    FOnRouteStateChanged(self, route, state);
end;

procedure TChromiumCore.doOnRouteMessageReceived(const route    : ICefMediaRoute;
                                                 const message_ : ustring);
begin
  if assigned(FOnRouteMessageReceived) then
    FOnRouteMessageReceived(self, route, message_);
end;

procedure TChromiumCore.doOnGetAudioParameters(const browser : ICefBrowser;
                                               var   params  : TCefAudioParameters;
                                               var   aResult : boolean);
begin
  if assigned(FOnGetAudioParameters) then
    FOnGetAudioParameters(self, browser, params, aResult);
end;

procedure TChromiumCore.doOnAudioStreamStarted(const browser  : ICefBrowser;
                                               const params   : TCefAudioParameters;
                                                     channels : integer);
begin
  if assigned(FOnAudioStreamStarted) then
    FOnAudioStreamStarted(self, browser, params, channels);
end;

procedure TChromiumCore.doOnAudioStreamPacket(const browser : ICefBrowser;
                                              const data    : PPSingle;
                                                    frames  : integer;
                                                    pts     : int64);
begin
  if assigned(FOnAudioStreamPacket) then
    FOnAudioStreamPacket(self, browser, data, frames, pts);
end;

procedure TChromiumCore.doOnAudioStreamStopped(const browser: ICefBrowser);
begin
  if assigned(FOnAudioStreamStopped) then
    FOnAudioStreamStopped(self, browser);
end;

procedure TChromiumCore.doOnAudioStreamError(const browser  : ICefBrowser;
                                             const message_ : ustring);
begin
  if assigned(FOnAudioStreamError) then
    FOnAudioStreamError(self, browser, message_);
end;

// ICefDevToolsMessageObserver
procedure TChromiumCore.doOnDevToolsMessage(const browser      : ICefBrowser;
                                            const message_     : Pointer;
                                                  message_size : NativeUInt;
                                            var   aHandled     : boolean);
var
  TempValue : ICefValue;
begin
  if assigned(FOnDevToolsRawMessage) then
    FOnDevToolsRawMessage(self, browser, message_, message_size, aHandled)
   else
    if assigned(FOnDevToolsMessage) then
      try
        TempValue := TCEFJson.Parse(message_, message_size);
        FOnDevToolsMessage(self, browser, TempValue, aHandled);
      finally
        TempValue := nil;
      end;
end;

procedure TChromiumCore.doOnDevToolsMethodResult(const browser     : ICefBrowser;
                                                       message_id  : integer;
                                                       success     : boolean;
                                                 const result      : Pointer;
                                                       result_size : NativeUInt);
var
  TempValue : ICefValue;
begin
  if assigned(FOnDevToolsMethodRawResult) then
    FOnDevToolsMethodRawResult(self, browser, message_id, success, result, result_size)
   else
    if assigned(FOnDevToolsMethodResult) then
      try
        TempValue := TCEFJson.Parse(result, result_size);
        FOnDevToolsMethodResult(self, browser, message_id, success, TempValue);
      finally
        TempValue := nil;
      end;
end;

procedure TChromiumCore.doOnDevToolsEvent(const browser     : ICefBrowser;
                                          const method      : ustring;
                                          const params      : Pointer;
                                                params_size : NativeUInt);
var
  TempValue : ICefValue;
begin
  if assigned(FOnDevToolsRawEvent) then
    FOnDevToolsRawEvent(self, browser, method, params, params_size)
   else
    if assigned(FOnDevToolsEvent) then
      try
        TempValue := TCEFJson.Parse(params, params_size);
        FOnDevToolsEvent(self, browser, method, TempValue);
      finally
        TempValue := nil;
      end;
end;

procedure TChromiumCore.doOnDevToolsAgentAttached(const browser: ICefBrowser);
begin
  if assigned(FOnDevToolsAgentAttached) then
    FOnDevToolsAgentAttached(self, browser);
end;

procedure TChromiumCore.doOnDevToolsAgentDetached(const browser: ICefBrowser);
begin
  if assigned(FOnDevToolsAgentDetached) then
    FOnDevToolsAgentDetached(self, browser);
end;

procedure TChromiumCore.doOnExtensionLoadFailed(result: TCefErrorcode);
begin
  if assigned(FOnExtensionLoadFailed) then
    FOnExtensionLoadFailed(self, result);
end;

procedure TChromiumCore.doOnExtensionLoaded(const extension: ICefExtension);
begin
  if assigned(FOnExtensionLoaded) then
    FOnExtensionLoaded(self, extension);
end;

procedure TChromiumCore.doOnExtensionUnloaded(const extension: ICefExtension);
begin
  if assigned(FOnExtensionUnloaded) then
    FOnExtensionUnloaded(self, extension);
end;

function TChromiumCore.doOnExtensionBeforeBackgroundBrowser(const extension : ICefExtension;
                                                            const url       : ustring;
                                                            var   client    : ICefClient;
                                                            var   settings  : TCefBrowserSettings) : boolean;
begin
  Result := False;

  if assigned(FOnExtensionBeforeBackgroundBrowser) then
    FOnExtensionBeforeBackgroundBrowser(self, extension, url, client, settings, Result);
end;

function TChromiumCore.doOnExtensionBeforeBrowser(const extension      : ICefExtension;
                                                  const browser        : ICefBrowser;
                                                  const active_browser : ICefBrowser;
                                                        index          : Integer;
                                                  const url            : ustring;
                                                        active         : boolean;
                                                  var   windowInfo     : TCefWindowInfo;
                                                  var   client         : ICefClient;
                                                  var   settings       : TCefBrowserSettings) : boolean;
begin
  Result := False;

  if assigned(FOnExtensionBeforeBrowser) then
    FOnExtensionBeforeBrowser(self, extension, browser, active_browser, index, url,
                              active, windowInfo, client, settings, Result);
end;

procedure TChromiumCore.doOnExtensionGetActiveBrowser(const extension         : ICefExtension;
                                                      const browser           : ICefBrowser;
                                                            include_incognito : boolean;
                                                      var   aRsltBrowser      : ICefBrowser);
begin
  aRsltBrowser := nil;

  if assigned(FOnExtensionGetActiveBrowser) then
    FOnExtensionGetActiveBrowser(self, extension, browser, include_incognito, aRsltBrowser);
end;

function TChromiumCore.doOnExtensionCanAccessBrowser(const extension         : ICefExtension;
                                                     const browser           : ICefBrowser;
                                                           include_incognito : boolean;
                                                     const target_browser    : ICefBrowser): boolean;
begin
  Result := False;

  if assigned(FOnExtensionCanAccessBrowser) then
    FOnExtensionCanAccessBrowser(self, extension, browser, include_incognito, target_browser, Result);
end;

function TChromiumCore.doOnExtensionGetExtensionResource(const extension : ICefExtension;
                                                         const browser   : ICefBrowser;
                                                         const file_     : ustring;
                                                         const callback  : ICefGetExtensionResourceCallback): boolean;
begin
  Result := False;

  if assigned(FOnExtensionGetExtensionResource) then
    FOnExtensionGetExtensionResource(self, extension, browser, file_, callback, Result);
end;

procedure TChromiumCore.doOnPrintStart(const browser: ICefBrowser);
begin
  if assigned(FOnPrintStart) then
    FOnPrintStart(self, browser);
end;

procedure TChromiumCore.doOnPrintSettings(const browser     : ICefBrowser;
                                          const settings    : ICefPrintSettings;
                                                getDefaults : boolean);
begin
  if assigned(FOnPrintSettings) then
    FOnPrintSettings(self, browser, settings, getDefaults);
end;

procedure TChromiumCore.doOnPrintDialog(const browser      : ICefBrowser;
                                              hasSelection : boolean;
                                        const callback     : ICefPrintDialogCallback;
                                        var   aResult      : boolean);
begin
  if assigned(FOnPrintDialog) then
    FOnPrintDialog(self, browser, hasSelection, callback, aResult);
end;

procedure TChromiumCore.doOnPrintJob(const browser      : ICefBrowser;
                                     const documentName : ustring;
                                     const PDFFilePath  : ustring;
                                     const callback     : ICefPrintJobCallback;
                                     var   aResult      : boolean);
begin
  if assigned(FOnPrintJob) then
    FOnPrintJob(self, browser, documentName, PDFFilePath, callback, aResult);
end;

procedure TChromiumCore.doOnPrintReset(const browser: ICefBrowser);
begin
  if assigned(FOnPrintReset) then
    FOnPrintReset(self, browser);
end;

procedure TChromiumCore.doOnGetPDFPaperSize(const browser            : ICefBrowser;
                                                  deviceUnitsPerInch : Integer;
                                            var   aResult            : TCefSize);
begin
  if assigned(FOnGetPDFPaperSize) then
    FOnGetPDFPaperSize(self, browser, deviceUnitsPerInch, aResult);
end;

procedure TChromiumCore.doOnFrameCreated(const browser: ICefBrowser; const frame: ICefFrame);
begin
  if assigned(FOnFrameCreated) then
    FOnFrameCreated(self, browser, frame);
end;

procedure TChromiumCore.doOnFrameAttached(const browser: ICefBrowser; const frame: ICefFrame; reattached: boolean);
begin
  if assigned(FOnFrameAttached) then
    FOnFrameAttached(self, browser, frame, reattached);
end;

procedure TChromiumCore.doOnFrameDetached(const browser: ICefBrowser; const frame: ICefFrame);
begin
  if assigned(FOnFrameDetached) then
    FOnFrameDetached(self, browser, frame);
end;

procedure TChromiumCore.doOnMainFrameChanged(const browser: ICefBrowser; const old_frame, new_frame: ICefFrame);
begin
  if assigned(FOnMainFrameChanged) then
    FOnMainFrameChanged(self, browser, old_frame, new_frame);
end;

function TChromiumCore.doOnChromeCommand(const browser     : ICefBrowser;
                                               command_id  : integer;
                                               disposition : TCefWindowOpenDisposition): boolean;
begin
  Result := False;

  if assigned(FOnChromeCommand) then
    FOnChromeCommand(self, browser, command_id, disposition, Result);
end;

function TChromiumCore.doOnIsChromeAppMenuItemVisible(const browser: ICefBrowser; command_id: integer): boolean;
begin
  Result := True;

  if assigned(FOnIsChromeAppMenuItemVisible) then
    FOnIsChromeAppMenuItemVisible(self, browser, command_id, Result);
end;

function TChromiumCore.doOnIsChromeAppMenuItemEnabled(const browser: ICefBrowser; command_id: integer): boolean;
begin
  Result := True;

  if assigned(FOnIsChromeAppMenuItemEnabled) then
    FOnIsChromeAppMenuItemEnabled(self, browser, command_id, Result);
end;

function TChromiumCore.doOnIsChromePageActionIconVisible(icon_type: TCefChromePageActionIconType): boolean;
begin
  Result := True;

  if assigned(FOnIsChromePageActionIconVisible) then
    FOnIsChromePageActionIconVisible(self, icon_type, Result);
end;

function TChromiumCore.doOnIsChromeToolbarButtonVisible(button_type: TCefChromeToolbarButtonType): boolean;
begin
  Result := True;

  if assigned(FOnIsChromeToolbarButtonVisible) then
    FOnIsChromeToolbarButtonVisible(self, button_type, Result);
end;


function TChromiumCore.doOnRequestMediaAccessPermission(const browser               : ICefBrowser;
                                                        const frame                 : ICefFrame;
                                                        const requesting_origin     : ustring;
                                                              requested_permissions : cardinal;
                                                        const callback              : ICefMediaAccessCallback): boolean;
begin
  Result := False;

  if assigned(FOnRequestMediaAccessPermission) then
    FOnRequestMediaAccessPermission(self, browser, frame, requesting_origin, requested_permissions, callback, Result);
end;

function TChromiumCore.doOnShowPermissionPrompt(const browser               : ICefBrowser;
                                                      prompt_id             : uint64;
                                                const requesting_origin     : ustring;
                                                      requested_permissions : cardinal;
                                                const callback              : ICefPermissionPromptCallback): boolean;
begin
  Result := False;

  if assigned(FOnShowPermissionPrompt) then
    FOnShowPermissionPrompt(self, browser, prompt_id, requesting_origin, requested_permissions, callback, Result);
end;

procedure TChromiumCore.doOnDismissPermissionPrompt(const browser   : ICefBrowser;
                                                          prompt_id : uint64;
                                                          result    : TCefPermissionRequestResult);
begin
  if assigned(FOnDismissPermissionPrompt) then
    FOnDismissPermissionPrompt(self, browser, prompt_id, result);
end;

procedure TChromiumCore.doOnFullScreenModeChange(const browser    : ICefBrowser;
                                                       fullscreen : Boolean);
begin
  if assigned(FOnFullScreenModeChange) then
    FOnFullScreenModeChange(Self, browser, fullscreen);
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
    if assigned(FOnGetAuthCredentials) then
      FOnGetAuthCredentials(Self, browser, originUrl, isProxy, host, port, realm, scheme, callback, Result);
end;

function TChromiumCore.doCanSendCookie(const browser : ICefBrowser;
                                       const frame   : ICefFrame;
                                       const request : ICefRequest;
                                       const cookie  : PCefCookie): boolean;
begin
  Result := True;

  if assigned(FOnCanSendCookie) then
    FOnCanSendCookie(self, browser, frame, request, cookie, Result);
end;

function TChromiumCore.doCanSaveCookie(const browser  : ICefBrowser;
                                       const frame    : ICefFrame;
                                       const request  : ICefRequest;
                                       const response : ICefResponse;
                                       const cookie   : PCefCookie): boolean;
begin
  Result := True;

  if assigned(FOnCanSaveCookie) then
    FOnCanSaveCookie(self, browser, frame, request, response, cookie, Result);
end;

procedure TChromiumCore.doOnGetResourceHandler(const browser          : ICefBrowser;
                                               const frame            : ICefFrame;
                                               const request          : ICefRequest;
                                               var   aResourceHandler : ICefResourceHandler);
begin
  aResourceHandler := nil;

  if assigned(FOnGetResourceHandler) then
    FOnGetResourceHandler(Self, browser, frame, request, aResourceHandler);
end;

procedure TChromiumCore.doOnGetAccessibilityHandler(var aAccessibilityHandler : ICefAccessibilityHandler);
begin
  if assigned(FOnGetAccessibilityHandler) then
    FOnGetAccessibilityHandler(Self, aAccessibilityHandler);
end;

function TChromiumCore.doOnGetRootScreenRect(const browser: ICefBrowser; var rect: TCefRect): Boolean;
begin
  Result := False;

  if assigned(FOnGetRootScreenRect) then
    FOnGetRootScreenRect(Self, browser, rect, Result);
end;

function TChromiumCore.doOnGetScreenInfo(const browser: ICefBrowser; var screenInfo: TCefScreenInfo): Boolean;
begin
  Result := False;

  if assigned(FOnGetScreenInfo) then
    FOnGetScreenInfo(Self, browser, screenInfo, Result);
end;

function TChromiumCore.doOnGetScreenPoint(const browser: ICefBrowser; viewX, viewY: Integer; var screenX, screenY: Integer): Boolean;
begin
  Result := False;

  if assigned(FOnGetScreenPoint) then
    FOnGetScreenPoint(Self, browser, viewX, viewY, screenX, screenY, Result);
end;

procedure TChromiumCore.doOnGetViewRect(const browser: ICefBrowser; var rect: TCefRect);
begin
  if assigned(FOnGetViewRect) then
    FOnGetViewRect(Self, browser, rect);
end;

procedure TChromiumCore.doOnGotFocus(const browser: ICefBrowser);
begin
  if assigned(FOnGotFocus) then
    FOnGotFocus(Self, browser)
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

      if assigned(FOnJsdialog) then
        FOnJsdialog(Self, browser, originUrl, dialogType, messageText,
                    defaultPromptText, callback, suppressMessage, Result);
    end;
end;

function TChromiumCore.doOnKeyEvent(const browser : ICefBrowser;
                                    const event   : PCefKeyEvent;
                                          osEvent : TCefEventHandle): Boolean;
begin
  Result := False;

  if assigned(FOnKeyEvent) then
    FOnKeyEvent(Self, browser, event, osEvent, Result);
end;

procedure TChromiumCore.doOnLoadEnd(const browser        : ICefBrowser;
                                    const frame          : ICefFrame;
                                          httpStatusCode : Integer);
begin
  if assigned(FOnLoadEnd) then
    FOnLoadEnd(Self, browser, frame, httpStatusCode);
end;

procedure TChromiumCore.doOnLoadError(const browser   : ICefBrowser;
                                      const frame     : ICefFrame;
                                            errorCode : TCefErrorCode;
                                      const errorText : ustring;
                                      const failedUrl : ustring);
begin
  if assigned(FOnLoadError) then
    FOnLoadError(Self, browser, frame, errorCode, errorText, failedUrl);
end;

procedure TChromiumCore.doOnLoadingStateChange(const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
begin
  if assigned(FOnLoadingStateChange) then
    FOnLoadingStateChange(Self, browser, isLoading, canGoBack, canGoForward);
end;

procedure TChromiumCore.doOnLoadStart(const browser: ICefBrowser; const frame: ICefFrame; transitionType: TCefTransitionType);
begin
  if assigned(FOnLoadStart) then
    FOnLoadStart(Self, browser, frame, transitionType);
end;

function TChromiumCore.doOnOpenUrlFromTab(const browser           : ICefBrowser;
                                          const frame             : ICefFrame;
                                          const targetUrl         : ustring;
                                                targetDisposition : TCefWindowOpenDisposition;
                                                userGesture       : Boolean): Boolean;
begin
  Result := False;

  if assigned(FOnOpenUrlFromTab) then
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

  if assigned(FOnGetResourceRequestHandler_ReqHdlr) then
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
  if assigned(FOnPaint) then
    FOnPaint(Self, browser, type_, dirtyRectsCount, dirtyRects, buffer, width, height);
end;

procedure TChromiumCore.doOnAcceleratedPaint(const browser         : ICefBrowser;
                                                   type_           : TCefPaintElementType;
                                                   dirtyRectsCount : NativeUInt;
                                             const dirtyRects      : PCefRectArray;
                                                   shared_handle   : Pointer);
begin
  if assigned(FOnAcceleratedPaint) then
    FOnAcceleratedPaint(Self, browser, type_, dirtyRectsCount, dirtyRects, shared_handle);
end;

procedure TChromiumCore.doGetTouchHandleSize(const browser: ICefBrowser; orientation: TCefHorizontalAlignment; var size: TCefSize);
begin
  if assigned(FOnGetTouchHandleSize) then
    FOnGetTouchHandleSize(Self, browser, orientation, size);
end;

procedure TChromiumCore.doOnTouchHandleStateChanged(const browser: ICefBrowser; const state: TCefTouchHandleState);
begin
  if assigned(FOnTouchHandleStateChanged) then
    FOnTouchHandleStateChanged(Self, browser, state);
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

procedure TChromiumCore.doOnPopupShow(const browser: ICefBrowser; show: Boolean);
begin
  if assigned(FOnPopupShow) then
    FOnPopupShow(self, browser, show);
end;

procedure TChromiumCore.doOnPopupSize(const browser: ICefBrowser; const rect: PCefRect);
begin
  if assigned(FOnPopupSize) then
    FOnPopupSize(self, browser, rect);
end;

function TChromiumCore.doOnPreKeyEvent(const browser            : ICefBrowser;
                                       const event              : PCefKeyEvent;
                                             osEvent            : TCefEventHandle;
                                       out   isKeyboardShortcut : Boolean): Boolean;
begin
  Result := False;

  if assigned(FOnPreKeyEvent) then
    FOnPreKeyEvent(Self, browser, event, osEvent, isKeyboardShortcut, Result);
end;

function TChromiumCore.doOnProcessMessageReceived(const browser       : ICefBrowser;
                                                  const frame         : ICefFrame;
                                                        sourceProcess : TCefProcessId;
                                                  const aMessage      : ICefProcessMessage): Boolean;
begin
  Result := False;

  if assigned(FOnProcessMessageReceived) then
    FOnProcessMessageReceived(Self, browser, frame, sourceProcess, aMessage, Result);
end;

procedure TChromiumCore.doOnProtocolExecution(const browser          : ICefBrowser;
                                              const frame            : ICefFrame;
                                              const request          : ICefRequest;
                                              var   allowOsExecution : Boolean);
begin
  if assigned(FOnProtocolExecution) then
    FOnProtocolExecution(Self, browser, frame, request, allowOsExecution);
end;

procedure TChromiumCore.doOnRenderProcessTerminated(const browser: ICefBrowser; status: TCefTerminationStatus);
begin
  if assigned(FOnRenderProcessTerminated) then
    FOnRenderProcessTerminated(Self, browser, status);
end;

procedure TChromiumCore.doOnDocumentAvailableInMainFrame(const browser: ICefBrowser);
begin
  if assigned(FOnDocumentAvailableInMainFrame) then
    FOnDocumentAvailableInMainFrame(Self, browser);
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
  if (browser            <> nil) and
     (browser.Host       <> nil) and
     (browser.Identifier =  BrowserId) then
    begin
      {$IFDEF MSWINDOWS}
      OldBrowserCompHWND := FBrowserCompHWND;
      OldWidgetCompHWND  := FWidgetCompHWND;
      OldRenderCompHWND  := FRenderCompHWND;
      FBrowserCompHWND   := browser.Host.WindowHandle;

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

  if assigned(FOnRenderViewReady) then
    FOnRenderViewReady(Self, browser);
end;

procedure TChromiumCore.doOnResetDialogState(const browser: ICefBrowser);
begin
  if assigned(FOnResetDialogState) then
    FOnResetDialogState(Self, browser);
end;

procedure TChromiumCore.doOnResourceRedirect(const browser  : ICefBrowser;
                                             const frame    : ICefFrame;
                                             const request  : ICefRequest;
                                             const response : ICefResponse;
                                             var   newUrl   : ustring);
begin
  if assigned(FOnResourceRedirect) then
    FOnResourceRedirect(Self, browser, frame, request, response, newUrl);
end;

function TChromiumCore.doOnResourceResponse(const browser  : ICefBrowser;
                                            const frame    : ICefFrame;
                                            const request  : ICefRequest;
                                            const response : ICefResponse): Boolean;
begin
  Result := False;

  if assigned(FOnResourceResponse) then
    FOnResourceResponse(Self, browser, frame, request, response, Result);
end;

procedure TChromiumCore.doOnGetResourceResponseFilter(const browser         : ICefBrowser;
                                                      const frame           : ICefFrame;
                                                      const request         : ICefRequest;
                                                      const response        : ICefResponse;
                                                      var   aResponseFilter : ICefResponseFilter);
begin
  aResponseFilter := nil;

  if assigned(FOnGetResourceResponseFilter) then
    FOnGetResourceResponseFilter(self, browser, frame, request, response, aResponseFilter);
end;

procedure TChromiumCore.doOnResourceLoadComplete(const browser               : ICefBrowser;
                                                 const frame                 : ICefFrame;
                                                 const request               : ICefRequest;
                                                 const response              : ICefResponse;
                                                       status                : TCefUrlRequestStatus;
                                                       receivedContentLength : Int64);
begin
  if assigned(FOnResourceLoadComplete) then
    FOnResourceLoadComplete(self, browser, frame, request, response, status, receivedContentLength);
end;

procedure TChromiumCore.doOnScrollOffsetChanged(const browser: ICefBrowser; x, y: Double);
begin
  if assigned(FOnScrollOffsetChanged) then
    FOnScrollOffsetChanged(Self, browser, x, y);
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

  if assigned(FOnSetFocus) then
    FOnSetFocus(Self, browser, source, Result);
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

  if assigned(FOnStartDragging) then
    FOnStartDragging(Self, browser, dragData, allowedOps, x, y, Result);
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
  if assigned(FOnStatusMessage) then
    FOnStatusMessage(Self, browser, value);
end;

procedure TChromiumCore.doOnTakeFocus(const browser: ICefBrowser; next: Boolean);
begin
  if assigned(FOnTakeFocus) then
    FOnTakeFocus(Self, browser, next);
end;

procedure TChromiumCore.doOnTitleChange(const browser: ICefBrowser; const title: ustring);
begin
  if assigned(FOnTitleChange) then
    FOnTitleChange(Self, browser, title);
end;

function TChromiumCore.doOnTooltip(const browser: ICefBrowser; var text: ustring): Boolean;
begin
  Result := False;

  if assigned(FOnTooltip) then
    FOnTooltip(Self, browser, text, Result);
end;

procedure TChromiumCore.doOnUpdateDragCursor(const browser: ICefBrowser; operation: TCefDragOperation);
begin
  if FDragAndDropInitialized then FDragOperations := operation;

  if assigned(FOnUpdateDragCursor) then
    FOnUpdateDragCursor(Self, browser, operation);
end;

procedure TChromiumCore.WasResized;
begin
  if Initialized then
    Browser.Host.WasResized;
end;

procedure TChromiumCore.WasHidden(hidden: Boolean);
begin
  if Initialized then
    Browser.Host.WasHidden(hidden);
end;

procedure TChromiumCore.NotifyScreenInfoChanged;
begin
  if Initialized then
    Browser.Host.NotifyScreenInfoChanged;
end;

procedure TChromiumCore.NotifyMoveOrResizeStarted;
begin
  if Initialized then
    Browser.Host.NotifyMoveOrResizeStarted;
end;

procedure TChromiumCore.Invalidate(type_: TCefPaintElementType);
begin
  if Initialized then
    begin
      if FIsOSR then
        Browser.Host.Invalidate(type_)
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
  if Initialized then
    Browser.Host.SendExternalBeginFrame;
end;

procedure TChromiumCore.SendKeyEvent(const event: PCefKeyEvent);
begin
  if Initialized then
    Browser.Host.SendKeyEvent(event);
end;

procedure TChromiumCore.SendMouseClickEvent(const event      : PCefMouseEvent;
                                                  type_      : TCefMouseButtonType;
                                                  mouseUp    : Boolean;
                                                  clickCount : Integer);
begin
  if Initialized then
    Browser.Host.SendMouseClickEvent(event, type_, mouseUp, clickCount);
end;

procedure TChromiumCore.SendMouseMoveEvent(const event: PCefMouseEvent; mouseLeave: Boolean);
begin
  if Initialized then
    Browser.Host.SendMouseMoveEvent(event, mouseLeave);
end;

procedure TChromiumCore.SendMouseWheelEvent(const event: PCefMouseEvent; deltaX, deltaY: Integer);
begin
  if Initialized then
    Browser.Host.SendMouseWheelEvent(event, deltaX, deltaY);
end;

procedure TChromiumCore.SendTouchEvent(const event: PCefTouchEvent);
begin
  if Initialized then
    Browser.Host.SendTouchEvent(event);
end;

procedure TChromiumCore.SendCaptureLostEvent;
{$IFDEF LINUX}{$IFDEF FPC}
var
  TempXDisplay : PXDisplay;
{$ENDIF}{$ENDIF}
begin
  if not(Initialized) then exit;

  {$IFDEF LINUX}{$IFDEF FPC}
  TempXDisplay := XDisplay;

  if (TempXDisplay <> nil) then
    XSetInputFocus(TempXDisplay, X.None, RevertToNone, CurrentTime);
  {$ENDIF}{$ENDIF}

  Browser.Host.SendCaptureLostEvent;
end;

procedure TChromiumCore.SetFocus(focus: Boolean);
begin
  if (not(FIsOSR) or FCanFocus) and
     Initialized then
    Browser.Host.SetFocus(focus);
end;

procedure TChromiumCore.SetAccessibilityState(accessibilityState: TCefState);
begin
  if Initialized then
    Browser.Host.SetAccessibilityState(accessibilityState);
end;

procedure TChromiumCore.SendProcessMessage(targetProcess: TCefProcessId; const ProcMessage: ICefProcessMessage; const aFrameName : ustring);
var
  TempFrame : ICefFrame;
begin
  try
    if Initialized then
      begin
        if (length(aFrameName) > 0) then
          TempFrame := Browser.GetFrame(aFrameName)
         else
          TempFrame := Browser.MainFrame;

        if (TempFrame <> nil) and TempFrame.IsValid then
          TempFrame.SendProcessMessage(targetProcess, ProcMessage);
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TChromiumCore.SendProcessMessage', e) then raise;
  end;
end;

procedure TChromiumCore.SendProcessMessage(targetProcess: TCefProcessId; const ProcMessage: ICefProcessMessage; const aFrame : ICefFrame);
var
  TempFrame : ICefFrame;
begin
  try
    if Initialized then
      begin
        if (aFrame <> nil) and aFrame.IsValid then
          TempFrame := aFrame
         else
          TempFrame := Browser.MainFrame;

        if (TempFrame <> nil) and TempFrame.IsValid then
          TempFrame.SendProcessMessage(targetProcess, ProcMessage);
      end;
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
          TempFrame := Browser.GetFrameByident(aFrameIdentifier)
         else
          TempFrame := Browser.MainFrame;

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
          TempFrame := Browser.GetFrame(aFrameName)
         else
          TempFrame := Browser.MainFrame;

        if (TempFrame <> nil) and TempFrame.IsValid then
          Result := TempFrame.CreateUrlRequest(request, client);
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TChromiumCore.CreateUrlRequest', e) then raise;
  end;
end;

function TChromiumCore.CreateUrlRequest(const request: ICefRequest; const client: ICefUrlrequestClient; const aFrame : ICefFrame): ICefUrlRequest;
var
  TempFrame : ICefFrame;
begin
  Result := nil;

  try
    if Initialized then
      begin
        if (aFrame <> nil) and aFrame.IsValid then
          Result := aFrame.CreateUrlRequest(request, client)
         else
          begin
            TempFrame := Browser.MainFrame;

            if (TempFrame <> nil) and TempFrame.IsValid then
              Result := TempFrame.CreateUrlRequest(request, client);
          end;
      end;
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
          TempFrame := Browser.GetFrameByident(aFrameIdentifier)
         else
          TempFrame := Browser.MainFrame;

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
  if Initialized then
    Browser.Host.DragTargetDragEnter(dragData, event, allowedOps);
end;

procedure TChromiumCore.DragTargetDragOver(const event: PCefMouseEvent; allowedOps: TCefDragOperations);
begin
  if Initialized then
    Browser.Host.DragTargetDragOver(event, allowedOps);
end;

procedure TChromiumCore.DragTargetDragLeave;
begin
  if Initialized then
    Browser.Host.DragTargetDragLeave;
end;

procedure TChromiumCore.DragTargetDrop(const event: PCefMouseEvent);
begin
  if Initialized then
    Browser.Host.DragTargetDrop(event);
end;

procedure TChromiumCore.DragSourceEndedAt(x, y: Integer; op: TCefDragOperation);
begin
  if Initialized then
    Browser.Host.DragSourceEndedAt(x, y, op);
end;

procedure TChromiumCore.DragSourceSystemDragEnded;
begin
  if Initialized then
    Browser.Host.DragSourceSystemDragEnded;
end;

procedure TChromiumCore.IMESetComposition(const text              : ustring;
                                          const underlines        : TCefCompositionUnderlineDynArray;
                                          const replacement_range : PCefRange;
                                          const selection_range   : PCefRange);
begin
  if Initialized then
    Browser.Host.IMESetComposition(text, underlines, replacement_range, selection_range);
end;

procedure TChromiumCore.IMECommitText(const text                : ustring;
                                      const replacement_range   : PCefRange;
                                            relative_cursor_pos : integer);
begin
  if Initialized then
    Browser.Host.IMECommitText(text, replacement_range, relative_cursor_pos);
end;

procedure TChromiumCore.IMEFinishComposingText(keep_selection : boolean);
begin
  if Initialized then
    Browser.Host.IMEFinishComposingText(keep_selection);
end;

procedure TChromiumCore.IMECancelComposition;
begin
  if Initialized then
    Browser.Host.IMECancelComposition;
end;

procedure TChromiumCore.ReplaceMisspelling(const aWord : ustring);
begin
  if Initialized then
    Browser.Host.ReplaceMisspelling(aWord);
end;

procedure TChromiumCore.AddWordToDictionary(const aWord : ustring);
begin
  if Initialized then
    Browser.Host.AddWordToDictionary(aWord);
end;

function TChromiumCore.AddObserver(const observer: ICefMediaObserver): ICefRegistration;
var
  TempMediaRouter : ICefMediaRouter;
begin
  Result          := nil;
  TempMediaRouter := MediaRouter;

  if (TempMediaRouter <> nil) then
    Result := TempMediaRouter.AddObserver(observer);
end;

function TChromiumCore.GetSource(const urn: ustring): ICefMediaSource;
var
  TempMediaRouter : ICefMediaRouter;
begin
  Result          := nil;
  TempMediaRouter := MediaRouter;

  if (TempMediaRouter <> nil) then
    Result := TempMediaRouter.GetSource(urn);
end;

procedure TChromiumCore.NotifyCurrentSinks;
var
  TempMediaRouter : ICefMediaRouter;
begin
  TempMediaRouter := MediaRouter;

  if (TempMediaRouter <> nil) then
    TempMediaRouter.NotifyCurrentSinks;
end;

procedure TChromiumCore.NotifyCurrentRoutes;
var
  TempMediaRouter : ICefMediaRouter;
begin
  TempMediaRouter := MediaRouter;

  if (TempMediaRouter <> nil) then
    TempMediaRouter.NotifyCurrentRoutes;
end;

procedure TChromiumCore.CreateRoute(const source: ICefMediaSource; const sink: ICefMediaSink);
var
  TempMediaRouter : ICefMediaRouter;
  TempCallback    : ICefMediaRouteCreateCallback;
begin
  TempMediaRouter := MediaRouter;

  if (TempMediaRouter <> nil) then
    try
      TempCallback := TCefCustomMediaRouteCreateCallback.Create(self);
      TempMediaRouter.CreateRoute(source, sink, TempCallback);
    finally
      TempCallback := nil;
    end;
end;

procedure TChromiumCore.GetDeviceInfo(const aMediaSink: ICefMediaSink);
var
  TempCallback : ICefMediaSinkDeviceInfoCallback;
begin
  if (aMediaSink <> nil) then
    try
      TempCallback := TCefCustomMediaSinkDeviceInfoCallback.Create(self);
      aMediaSink.GetDeviceInfo(TempCallback);
    finally
      TempCallback := nil;
    end;
end;

function TChromiumCore.LoadExtension(const root_directory : ustring;
                                     const manifest       : ICefDictionaryValue;
                                     const handler        : ICefExtensionHandler;
                                     const requestContext : ICefRequestContext) : boolean;
var
  TempContext : ICefRequestContext;
  TempHandler : ICefExtensionHandler;
begin
  Result := False;

  // The global context must be initalized to load extensions
  // This TChromium must not be initialized before loading an extension.
  if (GlobalCEFApp = nil) or
     not(GlobalCEFApp.GlobalContextInitialized) or
     Initialized then
    exit;

  // We use a custom request context if available or the global request context if not.
  if (requestContext <> nil) then
    TempContext := requestContext
   else
    TempContext := TCefRequestContextRef.Global;

  if (handler <> nil) then
    TempHandler := handler
   else
    begin
      // All TChromium events must be assigned before calling LoadExtension.
      CreateExtensionHandler;
      TempHandler := FExtensionHandler;
    end;

  if (TempContext <> nil) then
    try
      TempContext.LoadExtension(root_directory, manifest, TempHandler);
      Result := True;
    finally
      TempHandler := nil;
    end;
end;

function TChromiumCore.DidLoadExtension(const extension_id: ustring): boolean;
var
  TempContext : ICefRequestContext;
begin
  Result := False;

  if Initialized then
    begin
      TempContext := Browser.Host.RequestContext;
      Result      := (TempContext <> nil) and
                     TempContext.DidLoadExtension(extension_id);
    end;
end;

function TChromiumCore.HasExtension(const extension_id: ustring): boolean;
var
  TempContext : ICefRequestContext;
begin
  Result := False;

  if Initialized then
    begin
      TempContext := Browser.Host.RequestContext;
      Result      := (TempContext <> nil) and
                     TempContext.HasExtension(extension_id);
    end;
end;

function TChromiumCore.GetExtensions(const extension_ids: TStringList): boolean;
var
  TempContext : ICefRequestContext;
begin
  Result := False;

  if Initialized and (extension_ids <> nil) then
    begin
      TempContext := Browser.Host.RequestContext;
      Result      := (TempContext <> nil) and
                     TempContext.GetExtensions(extension_ids);
    end;
end;

function TChromiumCore.GetExtension(const extension_id: ustring): ICefExtension;
var
  TempContext : ICefRequestContext;
begin
  Result := nil;

  if Initialized then
    begin
      TempContext := Browser.Host.RequestContext;

      if (TempContext <> nil) then
        Result := TempContext.GetExtension(extension_id);
    end;
end;

function TChromiumCore.GetWebsiteSetting(const requesting_url, top_level_url: ustring; content_type: TCefContentSettingTypes): ICefValue;
var
  TempContext : ICefRequestContext;
begin
  Result := nil;

  if Initialized then
    begin
      TempContext := Browser.Host.RequestContext;

      if (TempContext <> nil) then
        Result := TempContext.GetWebsiteSetting(requesting_url, top_level_url, content_type);
    end;
end;

procedure TChromiumCore.SetWebsiteSetting(const requesting_url, top_level_url: ustring; content_type: TCefContentSettingTypes; const value: ICefValue);
var
  TempContext : ICefRequestContext;
begin
  if Initialized then
    begin
      TempContext := Browser.Host.RequestContext;

      if (TempContext <> nil) then
        TempContext.SetWebsiteSetting(requesting_url, top_level_url, content_type, value);
    end;
end;

function TChromiumCore.GetContentSetting(const requesting_url, top_level_url: ustring; content_type: TCefContentSettingTypes): TCefContentSettingValues;
var
  TempContext : ICefRequestContext;
begin
  Result := CEF_CONTENT_SETTING_VALUE_DEFAULT;

  if Initialized then
    begin
      TempContext := Browser.Host.RequestContext;

      if (TempContext <> nil) then
        Result := TempContext.GetContentSetting(requesting_url, top_level_url, content_type);
    end;
end;

procedure TChromiumCore.SetContentSetting(const requesting_url, top_level_url: ustring; content_type: TCefContentSettingTypes; value: TCefContentSettingValues);
var
  TempContext : ICefRequestContext;
begin
  if Initialized then
    begin
      TempContext := Browser.Host.RequestContext;

      if (TempContext <> nil) then
        TempContext.SetContentSetting(requesting_url, top_level_url, content_type, value);
    end;
end;

{$IFDEF MSWINDOWS}
function TChromiumCore.CopyDCToBitmapStream(aSrcDC : HDC; const aSrcRect : TRect; const aStream : TStream) : boolean;
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


// ******************************************
// ************** TBrowserInfo **************
// ******************************************

constructor TBrowserInfo.Create(const aBrowser : ICefBrowser);
begin
  inherited Create;

  FBrowser   := aBrowser;
  FID        := aBrowser.Identifier;
  FIsClosing := False;
end;

destructor TBrowserInfo.Destroy;
begin
  FBrowser := nil;

  inherited Destroy;
end;

function TBrowserInfo.GetIsValid : boolean;
begin
  Result := not(FIsClosing) and (FBrowser <> nil) and FBrowser.IsValid;
end;


// ******************************************
// *********** TBrowserInfoList *************
// ******************************************

destructor TBrowserInfoList.Destroy;
begin
  FreeAndClearAllItems;

  inherited Destroy;
end;

function TBrowserInfoList.AddBrowser(const aBrowser : ICefBrowser) : boolean;
var
  i : integer;
  TempInfo : TBrowserInfo;
begin
  Result := False;

  if (aBrowser <> nil) then
    begin
      i := SearchBrowser(aBrowser.Identifier);

      if (i < 0) then
        begin
          TempInfo := TBrowserInfo.Create(aBrowser);

          if (Add(TempInfo) >= 0) then
            Result := True
           else
            TempInfo.Free;
        end;
    end;
end;

function TBrowserInfoList.RemoveBrowser(const aBrowser : ICefBrowser) : boolean;
var
  i : integer;
begin
  Result := False;

  if (aBrowser <> nil) then
    begin
      i := SearchBrowser(aBrowser.Identifier);

      if (i >= 0) then
        begin
          TBrowserInfo(Items[i]).Free;
          Delete(i);
          Result := True;
        end;
    end;
end;

function TBrowserInfoList.SearchBrowser(aID : integer) : integer;
var
  i : integer;
begin
  i := 0;

  while (i < Count) do
    if (TBrowserInfo(Items[i]).ID = aID) then
      begin
        Result := i;
        exit;
      end
     else
      inc(i);

  Result := -1;
end;

procedure TBrowserInfoList.SetBrowserIsClosing(aID : integer; aValue : boolean);
var
  i : integer;
begin
  i := SearchBrowser(aID);
  if (i >= 0) then TBrowserInfo(Items[i]).IsClosing := aValue;
end;

function TBrowserInfoList.GetBrowserIsClosing(aID : integer) : boolean;
var
  i : integer;
begin
  i := SearchBrowser(aID);
  Result := (i >= 0) and TBrowserInfo(Items[i]).IsClosing;
end;

function TBrowserInfoList.GetBrowserIsValid(aID : integer) : boolean;
var
  i : integer;
begin
  i := SearchBrowser(aID);
  Result := (i >= 0) and TBrowserInfo(Items[i]).IsValid;
end;

function TBrowserInfoList.GetBrowser(aID : integer) : ICefBrowser;
var
  i : integer;
begin
  i := SearchBrowser(aID);

  if (i >= 0) then
    Result := TBrowserInfo(Items[i]).Browser
   else
    Result := nil;
end;

function TBrowserInfoList.GetFirstBrowser : ICefBrowser;
begin
  if (Count > 0) then
    Result := TBrowserInfo(Items[0]).Browser
   else
    Result := nil;
end;

function TBrowserInfoList.GetFirstID : integer;
begin
  if (Count > 0) then
    Result := TBrowserInfo(Items[0]).ID
   else
    Result := 0;
end;

procedure TBrowserInfoList.FreeAndClearAllItems;
var
  i : integer;
begin
  i := pred(Count);

  while (i >= 0) do
    begin
      TBrowserInfo(Items[i]).Free;
      dec(i);
    end;

  Clear;
end;

procedure TBrowserInfoList.CloseAllBrowsers;
var
  i : integer;
begin
  try
    i := pred(Count);
    while (i >= 0) do
      begin
        if (TBrowserInfo(Items[i]).Browser.Host <> nil) then
          TBrowserInfo(Items[i]).Browser.Host.CloseBrowser(True);

        dec(i);
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TBrowserInfoList.CloseAllBrowsers', e) then raise;
  end;
end;

end.
