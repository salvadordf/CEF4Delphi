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

unit uCEFTypes;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
    {$IFDEF MSWINDOWS}
      WinApi.Windows,
    {$ELSE}
      System.Types,
    {$ENDIF}
    System.Math;
  {$ELSE}
    {$IFDEF FPC}{$IFDEF LINUX}xlib, ctypes,{$ENDIF}{$ENDIF}
    {$IFDEF MSWINDOWS}Windows,{$ENDIF} Math;
  {$ENDIF}

type
  PCefStringWide = ^TCefStringWide;
  PCefDictionaryValue = ^TCefDictionaryValue;
  PCefListValue = ^TCefListValue;
  PCefBrowser = ^TCefBrowser;
  PCefValue = ^TCefValue;
  PCefBinaryValue = ^TCefBinaryValue;
  PCefSchemeRegistrar = ^TCefSchemeRegistrar;
  PCefCommandLine = ^TCefCommandLine;
  PCefBaseRefCounted = ^TCefBaseRefCounted;
  PCefBaseScoped = ^TCefBaseScoped;
  PCefWindowInfo = ^TCefWindowInfo;
  PCefSettings = ^TCefSettings;
  PCefStringUtf8 = ^TCefStringUtf8;
  PCefStringUtf16 = ^TCefStringUtf16;
  PCefStringUserFreeWide = ^TCefStringUserFreeWide;
  PCefStringUserFreeUtf8 = ^TCefStringUserFreeUtf8;
  PCefStringUserFreeUtf16 = ^TCefStringUserFreeUtf16;
  PCefMainArgs = ^TCefMainArgs;
  PCefColor = ^TCefColor;
  PCefBrowserHost = ^TCefBrowserHost;
  PCefClient = ^TCefClient;
  PCefPrintHandler = ^TCefPrintHandler;
  PCefResourceBundleHandler = ^TCefResourceBundleHandler;
  PCefBrowserProcessHandler = ^TCefBrowserProcessHandler;
  PCefContextMenuHandler = ^TCefContextMenuHandler;
  PCefAccessibilityHandler = ^TCefAccessibilityHandler;
  PCefFrame = ^TCefFrame;
  PCefApp = ^TCefApp;
  PCefServer = ^TCefServer;
  PCefServerHandler = ^TCefServerHandler;
  PCefStringVisitor = ^TCefStringVisitor;
  PCefRequest = ^TCefRequest;
  PCefPostData = ^TCefPostData;
  PCefPostDataElement = ^TCefPostDataElement;
  PPCefPostDataElement = ^PCefPostDataElement;
  PCefv8Context = ^TCefv8Context;
  PCefV8Interceptor = ^TCefV8Interceptor;
  PCefTask = ^TCefTask;
  PCefv8Value = ^TCefv8Value;
  PCefTime = ^TCefTime;
  PCefV8Exception = ^TCefV8Exception;
  PCefv8ArrayBufferReleaseCallback = ^TCefv8ArrayBufferReleaseCallback;
  PCefv8Handler = ^TCefv8Handler;
  PPCefV8Value = ^PCefV8ValueArray;
  PCefDomVisitor = ^TCefDomVisitor;
  PCefDomDocument = ^TCefDomDocument;
  PCefDomNode = ^TCefDomNode;
  PCefContextMenuParams = ^TCefContextMenuParams;
  PCefMenuModel = ^TCefMenuModel;
  PCefRunContextMenuCallback = ^TCefRunContextMenuCallback;
  PCefDialogHandler = ^TCefDialogHandler;
  PCefFileDialogCallback = ^TCefFileDialogCallback;
  PCefDisplayHandler = ^TCefDisplayHandler;
  PCefDownloadHandler = ^TCefDownloadHandler;
  PCefDownloadItem = ^TCefDownloadItem;
  PCefBeforeDownloadCallback = ^TCefBeforeDownloadCallback;
  PCefDownloadItemCallback = ^TCefDownloadItemCallback;
  PCefDragHandler = ^TCefDragHandler;
  PCefDragData = ^TCefDragData;
  PCefDraggableRegionArray = ^TCefDraggableRegionArray;
  PCefDraggableRegion = ^TCefDraggableRegion;
  PCefRect = ^TCefRect;
  PCefPoint = ^TCefPoint;
  PCefSize = ^TCefSize;
  PCefRectArray = ^TCefRectArray;
  PCefRange = ^TCefRange;
  PCefStreamWriter = ^TCefStreamWriter;
  PCefFindHandler = ^TCefFindHandler;
  PCefFocusHandler = ^TCefFocusHandler;
  PCefJsDialogHandler = ^TCefJsDialogHandler;
  PCefJsDialogCallback = ^TCefJsDialogCallback;
  PCefKeyboardHandler = ^TCefKeyboardHandler;
  PCefKeyEvent = ^TCefKeyEvent;
  PCefLifeSpanHandler = ^TCefLifeSpanHandler;
  PCefGetExtensionResourceCallback = ^TCefGetExtensionResourceCallback;
  PCefExtensionHandler = ^TCefExtensionHandler;
  PCefAudioHandler = ^TCefAudioHandler;
  PCefAudioParameters = ^TCefAudioParameters;
  PCefExtension = ^TCefExtension;
  PCefPopupFeatures = ^TCefPopupFeatures;
  PCefBrowserSettings = ^TCefBrowserSettings;
  PCefLoadHandler = ^TCefLoadHandler;
  PCefRenderHandler = ^TCefRenderHandler;
  PCefScreenInfo = ^TCefScreenInfo;
  PCefRenderProcessHandler = ^TCefRenderProcessHandler;
  PCefCursorInfo = ^TCefCursorInfo;
  PCefThread = ^TCefThread;
  PCefWaitableEvent = ^TCefWaitableEvent;
  PCefV8StackTrace = ^TCefV8StackTrace;
  PCefV8StackFrame = ^TCefV8StackFrame;
  PCefProcessMessage = ^TCefProcessMessage;
  PCefRequestHandler = ^TCefRequestHandler;
  PCefRequestCallback = ^TCefRequestCallback;
  PCefResourceSkipCallback = ^TCefResourceSkipCallback;
  PCefResourceReadCallback = ^TCefResourceReadCallback;
  PCefResourceHandler = ^TCefResourceHandler;
  PCefResourceRequestHandler = ^TCefResourceRequestHandler;
  PCefCookieAccessFilter = ^TCefCookieAccessFilter;
  PCefResponse = ^TCefResponse;
  PCefResponseFilter = ^TCefResponseFilter;
  PCefAuthCallback = ^TCefAuthCallback;
  PCefSslInfo = ^TCefSslInfo;
  PCefSSLStatus = ^TCefSSLStatus;
  PCefSelectClientCertificateCallback = ^TCefSelectClientCertificateCallback;
  PCefCallback = ^TCefCallback;
  PCefCookie = ^TCefCookie;
  PCefRequestContext = ^TCefRequestContext;
  PCefRequestContextHandler = ^TCefRequestContextHandler;
  PCefCompletionCallback = ^TCefCompletionCallback;
  PCefCookieManager = ^TCefCookieManager;
  PCefSchemeHandlerFactory = ^TCefSchemeHandlerFactory;
  PCefResolveCallback = ^TCefResolveCallback;
  PCefWebPluginInfo = ^TCefWebPluginInfo;
  PCefPluginPolicy = ^TCefPluginPolicy;
  PCefCookieVisitor = ^TCefCookieVisitor;
  PCefSetCookieCallback = ^TCefSetCookieCallback;
  PCefDeleteCookiesCallback = ^TCefDeleteCookiesCallback;
  PCefRunFileDialogCallback = ^TCefRunFileDialogCallback;
  PCefDownloadImageCallback = ^TCefDownloadImageCallback;
  PCefImage = ^TCefImage;
  PCefPdfPrintSettings = ^TCefPdfPrintSettings;
  PCefPdfPrintCallback = ^TCefPdfPrintCallback;
  PCefNavigationEntryVisitor = ^TCefNavigationEntryVisitor;
  PCefNavigationEntry = ^TCefNavigationEntry;
  PCefMouseEvent = ^TCefMouseEvent;
  PCefTouchEvent = ^TCefTouchEvent;
  PCefPrintSettings = ^TCefPrintSettings;
  PCefPrintDialogCallback = ^TCefPrintDialogCallback;
  PCefPrintJobCallback = ^TCefPrintJobCallback;
  PCefUrlParts = ^TCefUrlParts;
  PCefStreamReader = ^TCefStreamReader;
  PCefReadHandler = ^TCefReadHandler;
  PCefWriteHandler = ^TCefWriteHandler;
  PCefV8Accessor = ^TCefV8Accessor;
  PCefXmlReader = ^TCefXmlReader;
  PCefZipReader = ^TCefZipReader;
  PCefUrlRequestClient = ^TCefUrlRequestClient;
  PCefUrlRequest = ^TCefUrlRequest;
  PCefWebPluginInfoVisitor = ^TCefWebPluginInfoVisitor;
  PCefWebPluginUnstableCallback = ^TCefWebPluginUnstableCallback;
  PCefRegisterCDMCallback = ^TCefRegisterCDMCallback;
  PCefTaskRunner = ^TCefTaskRunner;
  PCefEndTracingCallback = ^TCefEndTracingCallback;
  PCefRequestContextSettings = ^TCefRequestContextSettings;
  PCefResourceBundle = ^TCefResourceBundle;
  PCefMenuModelDelegate = ^TCefMenuModelDelegate;
  PCefInsets = ^TCefInsets;
  PCefCompositionUnderline = ^TCefCompositionUnderline;
  PCefX509CertPrincipal = ^TCefX509CertPrincipal;
  PCefX509Certificate = ^TCefX509Certificate;
  PPCefX509Certificate = ^PCefX509Certificate;
  PCefDisplay = ^TCefDisplay;
  PPCefDisplay = ^PCefDisplay;
  PCefLayout = ^TCefLayout;
  PCefBoxLayout = ^TCefBoxLayout;
  PCefFillLayout = ^TCefFillLayout;
  PCefView = ^TCefView;
  PCefViewDelegate = ^TCefViewDelegate;
  PCefTextfield = ^TCefTextfield;
  PCefTextfieldDelegate = ^TCefTextfieldDelegate;
  PCefScrollView = ^TCefScrollView;
  PCefPanel = ^TCefPanel;
  PCefPanelDelegate = ^TCefPanelDelegate;
  PCefBrowserView = ^TCefBrowserView;
  PCefBrowserViewDelegate = ^TCefBrowserViewDelegate;
  PCefButton = ^TCefButton;
  PCefButtonDelegate = ^TCefButtonDelegate;
  PCefLabelButton = ^TCefLabelButton;
  PCefMenuButton = ^TCefMenuButton;
  PCefMenuButtonPressedLock = ^TCefMenuButtonPressedLock;
  PCefMenuButtonDelegate = ^TCefMenuButtonDelegate;
  PCefWindow = ^TCefWindow;
  PCefWindowDelegate = ^TCefWindowDelegate;
  PCefBoxLayoutSettings = ^TCefBoxLayoutSettings;
  PCefRegistration = ^TCefRegistration;
  PCefDevToolsMessageObserver = ^TCefDevToolsMessageObserver;
  PCefMediaRouter = ^TCefMediaRouter;
  PCefMediaRoute = ^TCefMediaRoute;
  PPCefMediaRoute = ^PCefMediaRoute;
  PCefMediaRouteCreateCallback = ^TCefMediaRouteCreateCallback;
  PCefMediaObserver = ^TCefMediaObserver;
  PCefMediaSink = ^TCefMediaSink;
  PPCefMediaSink = ^PCefMediaSink;
  PCefMediaSinkDeviceInfoCallback = ^TCefMediaSinkDeviceInfoCallback;
  PCefMediaSource = ^TCefMediaSource;
  PCefMediaSinkDeviceInfo = ^TCefMediaSinkDeviceInfo;

  {$IFDEF LINUX}
  PXEvent = pointer;
  {$IFNDEF FPC}
    // TODO: Find the FMX unit where PXDisplay is declared
    PXDisplay = pointer;
  {$ENDIF}
  {$ENDIF}


  {$IFDEF MSWINDOWS}
  TCefWindowHandle = HWND;     // /include/internal/cef_types_win.h (cef_window_handle_t)
  TCefCursorHandle = HCURSOR;  // /include/internal/cef_types_win.h (cef_cursor_handle_t)
  TCefEventHandle  = PMsg;     // /include/internal/cef_types_win.h (cef_event_handle_t)
  {$ENDIF}
  {$IFDEF MACOS}
  TCefWindowHandle = Pointer;  // /include/internal/cef_types_mac.h (cef_window_handle_t)
  TCefCursorHandle = Pointer;  // /include/internal/cef_types_mac.h (cef_cursor_handle_t)
  TCefEventHandle  = Pointer;  // /include/internal/cef_types_mac.h (cef_event_handle_t)
  {$ENDIF}
  {$IFDEF LINUX}
    {$IFDEF FPC}
    TCefWindowHandle = culong;   // /include/internal/cef_types_linux.h (cef_window_handle_t)
    TCefCursorHandle = culong;   // /include/internal/cef_types_linux.h (cef_cursor_handle_t)
    {$ELSE}
    TCefWindowHandle = LongWord;   // /include/internal/cef_types_linux.h (cef_window_handle_t)
    TCefCursorHandle = LongWord;   // /include/internal/cef_types_linux.h (cef_cursor_handle_t)
    {$ENDIF}
  TCefEventHandle = PXEvent;  // /include/internal/cef_types_linux.h (cef_event_handle_t)
  {$ENDIF}




  // All these types are declared as constants in uCEFConstants.pas.
  // Search the type name between the parentheses in uCEFConstants.pas.
  //
  // Open the original header file for more details :
  // https://github.com/chromiumembedded/cef/blob/master/include/internal/cef_types.h
  // https://github.com/chromiumembedded/cef/blob/master/include/internal/cef_thread_internal.h
  // https://github.com/chromiumembedded/cef/blob/master/include/internal/cef_string_list.h
  // https://github.com/chromiumembedded/cef/blob/master/include/internal/cef_string_map.h
  // https://github.com/chromiumembedded/cef/blob/master/include/internal/cef_string_multimap.h

  TCefPlatformThreadId             = DWORD;       // /include/internal/cef_thread_internal.h (cef_platform_thread_id_t)
  TCefPlatformThreadHandle         = DWORD;       // /include/internal/cef_thread_internal.h (cef_platform_thread_handle_t)
  TCefTransitionType               = Cardinal;    // /include/internal/cef_types.h (cef_transition_type_t)
  TCefColor                        = Cardinal;    // /include/internal/cef_types.h (cef_color_t)
  TCefErrorCode                    = Integer;     // /include/internal/cef_types.h (cef_errorcode_t)
  TCefCertStatus                   = Integer;     // /include/internal/cef_types.h (cef_cert_status_t)
  TCefSSLVersion                   = integer;     // /include/internal/cef_types.h (cef_ssl_version_t)
  TCefStringList                   = Pointer;     // /include/internal/cef_string_list.h (cef_string_list_t)
  TCefStringMap                    = Pointer;     // /include/internal/cef_string_map.h (cef_string_map_t)
  TCefStringMultimap               = Pointer;     // /include/internal/cef_string_multimap.h (cef_string_multimap_t)
  TCefUriUnescapeRule              = Integer;     // /include/internal/cef_types.h (cef_uri_unescape_rule_t)
  TCefDomEventCategory             = Integer;     // /include/internal/cef_types.h (cef_dom_event_category_t)
  TCefEventFlags                   = Cardinal;    // /include/internal/cef_types.h (cef_event_flags_t)
  TCefDragOperations               = Cardinal;    // /include/internal/cef_types.h (cef_drag_operations_mask_t)
  TCefDragOperation                = Cardinal;    // /include/internal/cef_types.h (cef_drag_operations_mask_t)
  TCefV8AccessControls             = Cardinal;    // /include/internal/cef_types.h (cef_v8_accesscontrol_t)
  TCefV8PropertyAttributes         = Cardinal;    // /include/internal/cef_types.h (cef_v8_propertyattribute_t)
  TCefUrlRequestFlags              = Cardinal;    // /include/internal/cef_types.h (cef_urlrequest_flags_t)
  TCefContextMenuTypeFlags         = Cardinal;    // /include/internal/cef_types.h (cef_context_menu_type_flags_t)
  TCefContextMenuMediaStateFlags   = Cardinal;    // /include/internal/cef_types.h (cef_context_menu_media_state_flags_t)
  TCefContextMenuEditStateFlags    = Cardinal;    // /include/internal/cef_types.h (cef_context_menu_edit_state_flags_t)
  TCefJsonWriterOptions            = Cardinal;    // /include/internal/cef_types.h (cef_json_writer_options_t)
  TCefSSLContentStatus             = Cardinal;    // /include/internal/cef_types.h (cef_ssl_content_status_t)
  TCefLogSeverity                  = Cardinal;    // /include/internal/cef_types.h (cef_log_severity_t)
  TCefFileDialogMode               = Cardinal;    // /include/internal/cef_types.h (cef_file_dialog_mode_t)
  TCefDuplexMode                   = Integer;     // /include/internal/cef_types.h (cef_duplex_mode_t)
  TCefSchemeOptions                = Integer;     // /include/internal/cef_types.h (cef_scheme_options_t)
  TCefMediaRouterCreateResult      = Integer;     // /include/internal/cef_types.h (cef_media_route_create_result_t)
  TCefCookiePriority               = Integer;     // /include/internal/cef_types.h (cef_cookie_priority_t)
  TCefTextFieldCommands            = Integer;     // /include/internal/cef_types.h (cef_text_field_commands_t)





{$IFDEF FPC}
  NativeInt   = PtrInt;
  NativeUInt  = PtrUInt;
  PNativeInt  = ^NativeInt;
  PNativeUInt = ^NativeUInt;
  ustring     = type UnicodeString;
  rbstring    = type AnsiString;
{$ELSE}
  {$IFNDEF DELPHI12_UP}
    NativeUInt  = Cardinal;
    PNativeUInt = ^NativeUInt;
    NativeInt   = Integer;
    uint16      = Word;
    ustring     = type WideString;
    rbstring    = type AnsiString;
  {$ELSE}
    ustring     = type string;
    rbstring    = type RawByteString;
    {$IFNDEF DELPHI15_UP}
      NativeUInt  = Cardinal;
      PNativeUInt = ^NativeUInt;
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

  TCefCustomByteArray = array of byte; // Needed only for backwards compatibility with old Delphi versions

  {$IFDEF MSWINDOWS}
  TMyMemoryStatusEx = record
     dwLength : DWORD;
     dwMemoryLoad : DWORD;
     ullTotalPhys : uint64;
     ullAvailPhys : uint64;
     ullTotalPageFile : uint64;
     ullAvailPageFile : uint64;
     ullTotalVirtual : uint64;
     ullAvailVirtual : uint64;
     ullAvailExtendedVirtual : uint64;
  end;
  {$ENDIF}

  PPSingle = ^PSingle;

  Char16  = WideChar;
  PChar16 = PWideChar;

  // /include/internal/cef_string_types.h (cef_string_wide_t)
  TCefStringWide = record
    str    : PWideChar;
    length : NativeUInt;
    dtor   : procedure(str: PWideChar); stdcall;
  end;

  // /include/internal/cef_string_types.h (cef_string_utf8_t)
  TCefStringUtf8 = record
    str    : PAnsiChar;
    length : NativeUInt;
    dtor   : procedure(str: PAnsiChar); stdcall;
  end;

  // /include/internal/cef_string_types.h (cef_string_utf16_t)
  TCefStringUtf16 = record
    str    : PChar16;
    length : NativeUInt;
    dtor   : procedure(str: PChar16); stdcall;
  end;

  TCefStringUserFreeWide  = type TCefStringWide;
  TCefStringUserFreeUtf8  = type TCefStringUtf8;
  TCefStringUserFreeUtf16 = type TCefStringUtf16;

  TCefChar = Char16;
  PCefChar = PChar16;
  TCefStringUserFree = TCefStringUserFreeUtf16;
  PCefStringUserFree = PCefStringUserFreeUtf16;
  TCefString = TCefStringUtf16;
  PCefString = PCefStringUtf16;

  TFileVersionInfo = record
    MajorVer : uint16;
    MinorVer : uint16;
    Release  : uint16;
    Build    : uint16;
  end;

  // Used in TChromium.Onclose
  // -------------------------
  // cbaCancel : stop closing the browser
  // cbaClose  : continue closing the browser
  // cbaDelay  : stop closing the browser momentarily. Used when the application
  //             needs to execute some custom processes before closing the
  //             browser. This is usually needed to destroy a TCEFWindowParent
  //             in the main thread before closing the browser.
  TCefCloseBrowserAction = (cbaClose, cbaDelay, cbaCancel);

  TCefProcessType = (ptBrowser, ptRenderer, ptZygote, ptGPU, ptUtility, ptOther);

  // Used in TChromium preferences to allow or block cookies.
  TCefCookiePref = (cpDefault, cpAllow, cpBlock);

  // Used by TCefBrowserNavigationTask to navigate in the right CEF thread
  TCefBrowserNavigation = (bnBack, bnForward, bnReload, bnReloadIgnoreCache, bnStopLoad);

  TCefAplicationStatus = (asLoading,
                          asLoaded,
                          asInitialized,
                          asShuttingDown,
                          asUnloaded,
                          asErrorMissingFiles,
                          asErrorDLLVersion,
                          asErrorLoadingLibrary,
                          asErrorInitializingLibrary,
                          asErrorExecutingProcess);

  TCefProxyScheme = (psHTTP, psSOCKS4, psSOCKS5);

  TCefClearDataStorageTypes = (cdstAppCache,
                               cdstCookies,
                               cdstFileSystems,
                               cdstIndexeddb,
                               cdstLocalStorage,
                               cdstShaderCache,
                               cdstWebsql,
                               cdstServiceWorkers,
                               cdstCacheStorage,
                               cdstAll);

  TCefAutoplayPolicy = (appDefault,
                        appDocumentUserActivationRequired,
                        appNoUserGestureRequired,
                        appUserGestureRequired);

  TCefWebRTCHandlingPolicy = (
    hpDefault,
    hpDefaultPublicAndPrivateInterfaces,
    hpDefaultPublicInterfaceOnly,
    hpDisableNonProxiedUDP
  );

  // Used by TCefMediaSinkInfo and TCefMediaSourceInfo
  TCefMediaType = (mtCast, mtDial, mtUnknown);

  // /include/internal/cef_types_win.h (cef_main_args_t)
  TCefMainArgs = record
    {$IFDEF MSWINDOWS}
    instance : HINST;
    {$ELSE}
    argc     : Integer;
    argv     : PPChar;
    {$ENDIF}
  end;

  // /include/internal/cef_types.h (cef_rect_t)
  TCefRect = record
    x      : Integer;
    y      : Integer;
    width  : Integer;
    height : Integer;
  end;
  TCefRectArray    = array[0..(High(Integer) div SizeOf(TCefRect))-1] of TCefRect;
  TCefRectDynArray = array of TCefRect;

  // /include/internal/cef_types.h (cef_point_t)
  TCefPoint = record
    x  : Integer;
    y  : Integer;
  end;

  // /include/internal/cef_types.h (cef_size_t)
  TCefSize = record
    width  : Integer;
    height : Integer;
  end;

  // /include/internal/cef_types.h (cef_range_t)
  TCefRange = record
    from  : Integer;
    to_   : Integer;
  end;
  TCefRangeArray = array of TCefRange;

  // /include/internal/cef_types.h (cef_cursor_info_t)
  TCefCursorInfo = record
    hotspot            : TCefPoint;
    image_scale_factor : Single;
    buffer             : Pointer;
    size               : TCefSize;
  end;

  // /include/internal/cef_types.h (cef_urlparts_t)
  TCefUrlParts = record
    spec      : TCefString;
    scheme    : TCefString;
    username  : TCefString;
    password  : TCefString;
    host      : TCefString;
    port      : TCefString;
    origin    : TCefString;
    path      : TCefString;
    query     : TCefString;
    fragment  : TCefString;
  end;

  TUrlParts = record
    spec     : ustring;
    scheme   : ustring;
    username : ustring;
    password : ustring;
    host     : ustring;
    port     : ustring;
    origin   : ustring;
    path     : ustring;
    query    : ustring;
    fragment : ustring;
  end;

  // /include/internal/cef_types.h (cef_insets_t)
  TCefInsets = record
    top    : Integer;
    left   : Integer;
    bottom : Integer;
    right  : Integer;
  end;

  // /include/internal/cef_types.h (cef_state_t)
  TCefState = (
    STATE_DEFAULT = 0,
    STATE_ENABLED,
    STATE_DISABLED
  );

  // /include/internal/cef_types.h (cef_scale_factor_t)
  TCefScaleFactor = (
    SCALE_FACTOR_NONE = 0,
    SCALE_FACTOR_100P,
    SCALE_FACTOR_125P,
    SCALE_FACTOR_133P,
    SCALE_FACTOR_140P,
    SCALE_FACTOR_150P,
    SCALE_FACTOR_180P,
    SCALE_FACTOR_200P,
    SCALE_FACTOR_250P,
    SCALE_FACTOR_300P
  );

  // /include/internal/cef_types.h (cef_value_type_t)
  TCefValueType = (
    VTYPE_INVALID = 0,
    VTYPE_NULL,
    VTYPE_BOOL,
    VTYPE_INT,
    VTYPE_DOUBLE,
    VTYPE_STRING,
    VTYPE_BINARY,
    VTYPE_DICTIONARY,
    VTYPE_LIST
  );

  // /include/internal/cef_types.h (cef_media_route_connection_state_t)
  TCefMediaRouteConnectionState = (
    CEF_MRCS_UNKNOWN,
    CEF_MRCS_CONNECTING,
    CEF_MRCS_CONNECTED,
    CEF_MRCS_CLOSED,
    CEF_MRCS_TERMINATED
  );

  // /include/internal/cef_types.h (cef_media_sink_icon_type_t)
  TCefMediaSinkIconType = (
    CEF_MSIT_CAST,
    CEF_MSIT_CAST_AUDIO_GROUP,
    CEF_MSIT_CAST_AUDIO,
    CEF_MSIT_MEETING,
    CEF_MSIT_HANGOUT,
    CEF_MSIT_EDUCATION,
    CEF_MSIT_WIRED_DISPLAY,
    CEF_MSIT_GENERIC,
    CEF_MSIT_TOTAL_COUNT
  );

  // /include/internal/cef_types.h (cef_referrer_policy_t)
  TCefReferrerPolicy = (
    REFERRER_POLICY_CLEAR_REFERRER_ON_TRANSITION_FROM_SECURE_TO_INSECURE,    // same value as REFERRER_POLICY_DEFAULT
    REFERRER_POLICY_REDUCE_REFERRER_GRANULARITY_ON_TRANSITION_CROSS_ORIGIN,
    REFERRER_POLICY_ORIGIN_ONLY_ON_TRANSITION_CROSS_ORIGIN,
    REFERRER_POLICY_NEVER_CLEAR_REFERRER,
    REFERRER_POLICY_ORIGIN,
    REFERRER_POLICY_CLEAR_REFERRER_ON_TRANSITION_CROSS_ORIGIN,
    REFERRER_POLICY_ORIGIN_CLEAR_ON_TRANSITION_FROM_SECURE_TO_INSECURE,
    REFERRER_POLICY_NO_REFERRER // REFERRER_POLICY_LAST_VALUE = REFERRER_POLICY_NO_REFERRER
  );

  // /include/internal/cef_types.h (cef_postdataelement_type_t)
  TCefPostDataElementType = (
    PDE_TYPE_EMPTY  = 0,
    PDE_TYPE_BYTES,
    PDE_TYPE_FILE
  );

  // /include/internal/cef_types.h (cef_resource_type_t)
  TCefResourceType = (
    RT_MAIN_FRAME,
    RT_SUB_FRAME,
    RT_STYLESHEET,
    RT_SCRIPT,
    RT_IMAGE,
    RT_FONT_RESOURCE,
    RT_SUB_RESOURCE,
    RT_OBJECT,
    RT_MEDIA,
    RT_WORKER,
    RT_SHARED_WORKER,
    RT_PREFETCH,
    RT_FAVICON,
    RT_XHR,
    RT_PING,
    RT_SERVICE_WORKER,
    RT_CSP_REPORT,
    RT_PLUGIN_RESOURCE
  );

  // /include/internal/cef_types.h (cef_dom_document_type_t)
  TCefDomDocumentType = (
    DOM_DOCUMENT_TYPE_UNKNOWN = 0,
    DOM_DOCUMENT_TYPE_HTML,
    DOM_DOCUMENT_TYPE_XHTML,
    DOM_DOCUMENT_TYPE_PLUGIN
  );

  // /include/internal/cef_types.h (cef_dom_node_type_t)
  TCefDomNodeType = (
    DOM_NODE_TYPE_UNSUPPORTED = 0,
    DOM_NODE_TYPE_ELEMENT,
    DOM_NODE_TYPE_ATTRIBUTE,
    DOM_NODE_TYPE_TEXT,
    DOM_NODE_TYPE_CDATA_SECTION,
    DOM_NODE_TYPE_PROCESSING_INSTRUCTIONS,
    DOM_NODE_TYPE_COMMENT,
    DOM_NODE_TYPE_DOCUMENT,
    DOM_NODE_TYPE_DOCUMENT_TYPE,
    DOM_NODE_TYPE_DOCUMENT_FRAGMENT
  );

  // /include/internal/cef_types.h (cef_context_menu_media_type_t)
  TCefContextMenuMediaType = (
    CM_MEDIATYPE_NONE,
    CM_MEDIATYPE_IMAGE,
    CM_MEDIATYPE_VIDEO,
    CM_MEDIATYPE_AUDIO,
    CM_MEDIATYPE_FILE,
    CM_MEDIATYPE_PLUGIN
  );

  // /include/internal/cef_types.h (cef_menu_item_type_t)
  TCefMenuItemType = (
    MENUITEMTYPE_NONE,
    MENUITEMTYPE_COMMAND,
    MENUITEMTYPE_CHECK,
    MENUITEMTYPE_RADIO,
    MENUITEMTYPE_SEPARATOR,
    MENUITEMTYPE_SUBMENU
  );

  // /include/internal/cef_types.h (cef_focus_source_t)
  TCefFocusSource = (
    FOCUS_SOURCE_NAVIGATION = 0,
    FOCUS_SOURCE_SYSTEM
  );

  // /include/internal/cef_types.h (cef_jsdialog_type_t)
  TCefJsDialogType = (
    JSDIALOGTYPE_ALERT = 0,
    JSDIALOGTYPE_CONFIRM,
    JSDIALOGTYPE_PROMPT
  );

  // /include/internal/cef_types.h (cef_key_event_type_t)
  TCefKeyEventType = (
    KEYEVENT_RAWKEYDOWN = 0,
    KEYEVENT_KEYDOWN,
    KEYEVENT_KEYUP,
    KEYEVENT_CHAR
  );

  // /include/internal/cef_types.h (cef_window_open_disposition_t)
  TCefWindowOpenDisposition = (
    WOD_UNKNOWN,
    WOD_CURRENT_TAB,
    WOD_SINGLETON_TAB,
    WOD_NEW_FOREGROUND_TAB,
    WOD_NEW_BACKGROUND_TAB,
    WOD_NEW_POPUP,
    WOD_NEW_WINDOW,
    WOD_SAVE_TO_DISK,
    WOD_OFF_THE_RECORD,
    WOD_IGNORE_ACTION
  );

  // /include/internal/cef_types.h (cef_text_input_mode_t)
  TCefTextInpuMode = (
    CEF_TEXT_INPUT_MODE_DEFAULT,
    CEF_TEXT_INPUT_MODE_NONE,
    CEF_TEXT_INPUT_MODE_TEXT,
    CEF_TEXT_INPUT_MODE_TEL,
    CEF_TEXT_INPUT_MODE_URL,
    CEF_TEXT_INPUT_MODE_EMAIL,
    CEF_TEXT_INPUT_MODE_NUMERIC,
    CEF_TEXT_INPUT_MODE_DECIMAL,
    CEF_TEXT_INPUT_MODE_SEARCH    // CEF_TEXT_INPUT_MODE_MAX = CEF_TEXT_INPUT_MODE_SEARCH
  );

  // /include/internal/cef_types.h (cef_touch_event_type_t)
  TCefTouchEeventType = (
    CEF_TET_RELEASED = 0,
    CEF_TET_PRESSED,
    CEF_TET_MOVED,
    CEF_TET_CANCELLED
  );

  // /include/internal/cef_types.h (cef_pointer_type_t)
  TCefPointerType = (
    CEF_POINTER_TYPE_TOUCH = 0,
    CEF_POINTER_TYPE_MOUSE,
    CEF_POINTER_TYPE_PEN,
    CEF_POINTER_TYPE_ERASER,
    CEF_POINTER_TYPE_UNKNOWN
  );

  // /include/internal/cef_types.h (cef_channel_layout_t)
  TCefChannelLayout = (
    CEF_CHANNEL_LAYOUT_NONE = 0,
    CEF_CHANNEL_LAYOUT_UNSUPPORTED,
    CEF_CHANNEL_LAYOUT_MONO,
    CEF_CHANNEL_LAYOUT_STEREO,
    CEF_CHANNEL_LAYOUT_2_1,
    CEF_CHANNEL_LAYOUT_SURROUND,
    CEF_CHANNEL_LAYOUT_4_0,
    CEF_CHANNEL_LAYOUT_2_2,
    CEF_CHANNEL_LAYOUT_QUAD,
    CEF_CHANNEL_LAYOUT_5_0,
    CEF_CHANNEL_LAYOUT_5_1,
    CEF_CHANNEL_LAYOUT_5_0_BACK,
    CEF_CHANNEL_LAYOUT_5_1_BACK,
    CEF_CHANNEL_LAYOUT_7_0,
    CEF_CHANNEL_LAYOUT_7_1,
    CEF_CHANNEL_LAYOUT_7_1_WIDE,
    CEF_CHANNEL_LAYOUT_STEREO_DOWNMIX,
    CEF_CHANNEL_LAYOUT_2POINT1,
    CEF_CHANNEL_LAYOUT_3_1,
    CEF_CHANNEL_LAYOUT_4_1,
    CEF_CHANNEL_LAYOUT_6_0,
    CEF_CHANNEL_LAYOUT_6_0_FRONT,
    CEF_CHANNEL_LAYOUT_HEXAGONAL,
    CEF_CHANNEL_LAYOUT_6_1,
    CEF_CHANNEL_LAYOUT_6_1_BACK,
    CEF_CHANNEL_LAYOUT_6_1_FRONT,
    CEF_CHANNEL_LAYOUT_7_0_FRONT,
    CEF_CHANNEL_LAYOUT_7_1_WIDE_BACK,
    CEF_CHANNEL_LAYOUT_OCTAGONAL,
    CEF_CHANNEL_LAYOUT_DISCRETE,
    CEF_CHANNEL_LAYOUT_STEREO_AND_KEYBOARD_MIC,
    CEF_CHANNEL_LAYOUT_4_1_QUAD_SIDE,
    CEF_CHANNEL_LAYOUT_BITSTREAM  // CEF_CHANNEL_LAYOUT_MAX = CEF_CHANNEL_LAYOUT_BITSTREAM
  );

  // /include/internal/cef_types.h (cef_cookie_same_site_t)
  TCefCookieSameSite = (
    CEF_COOKIE_SAME_SITE_UNSPECIFIED,
    CEF_COOKIE_SAME_SITE_NO_RESTRICTION,
    CEF_COOKIE_SAME_SITE_LAX_MODE,
    CEF_COOKIE_SAME_SITE_STRICT_MODE
  );

  // /include/internal/cef_types.h (cef_paint_element_type_t)
  TCefPaintElementType = (
    PET_VIEW,
    PET_POPUP
  );

  // /include/internal/cef_types.h (cef_cursor_type_t)
  TCefCursorType = (
    CT_POINTER = 0,
    CT_CROSS,
    CT_HAND,
    CT_IBEAM,
    CT_WAIT,
    CT_HELP,
    CT_EASTRESIZE,
    CT_NORTHRESIZE,
    CT_NORTHEASTRESIZE,
    CT_NORTHWESTRESIZE,
    CT_SOUTHRESIZE,
    CT_SOUTHEASTRESIZE,
    CT_SOUTHWESTRESIZE,
    CT_WESTRESIZE,
    CT_NORTHSOUTHRESIZE,
    CT_EASTWESTRESIZE,
    CT_NORTHEASTSOUTHWESTRESIZE,
    CT_NORTHWESTSOUTHEASTRESIZE,
    CT_COLUMNRESIZE,
    CT_ROWRESIZE,
    CT_MIDDLEPANNING,
    CT_EASTPANNING,
    CT_NORTHPANNING,
    CT_NORTHEASTPANNING,
    CT_NORTHWESTPANNING,
    CT_SOUTHPANNING,
    CT_SOUTHEASTPANNING,
    CT_SOUTHWESTPANNING,
    CT_WESTPANNING,
    CT_MOVE,
    CT_VERTICALTEXT,
    CT_CELL,
    CT_CONTEXTMENU,
    CT_ALIAS,
    CT_PROGRESS,
    CT_NODROP,
    CT_COPY,
    CT_NONE,
    CT_NOTALLOWED,
    CT_ZOOMIN,
    CT_ZOOMOUT,
    CT_GRAB,
    CT_GRABBING,
    CT_MIDDLE_PANNING_VERTICAL,
    CT_MIDDLE_PANNING_HORIZONTAL,
    CT_CUSTOM,
    CT_DND_NONE,
    CT_DND_MOVE,
    CT_DND_COPY,
    CT_DND_LIN
  );

  // /include/internal/cef_types.h (cef_navigation_type_t)
  TCefNavigationType = (
    NAVIGATION_LINK_CLICKED,
    NAVIGATION_FORM_SUBMITTED,
    NAVIGATION_BACK_FORWARD,
    NAVIGATION_RELOAD,
    NAVIGATION_FORM_RESUBMITTED,
    NAVIGATION_OTHER
  );

  // /include/internal/cef_types.h (cef_process_id_t)
  TCefProcessId = (
    PID_BROWSER,
    PID_RENDERER
  );

  // /include/internal/cef_types.h (cef_thread_id_t)
  TCefThreadId = (
    TID_UI,
    TID_FILE_BACKGROUND,   // TID_FILE = TID_FILE_BACKGROUND
    TID_FILE_USER_VISIBLE,
    TID_FILE_USER_BLOCKING,
    TID_PROCESS_LAUNCHER,
    TID_IO,
    TID_RENDERER
  );

  // /include/internal/cef_types.h (cef_thread_priority_t)
  TCefThreadPriority = (
    TP_BACKGROUND,
    TP_NORMAL,
    TP_DISPLAY,
    TP_REALTIME_AUDIO
  );

  // /include/internal/cef_types.h (cef_message_loop_type_t)
  TCefMessageLoopType = (
    ML_TYPE_DEFAULT,
    ML_TYPE_UI,
    ML_TYPE_IO
  );

  // /include/internal/cef_types.h (cef_com_init_mode_t)
  TCefCOMInitMode = (
    COM_INIT_MODE_NONE,
    COM_INIT_MODE_STA,
    COM_INIT_MODE_MTA
  );

  // /include/internal/cef_types.h (cef_mouse_button_type_t)
  TCefMouseButtonType = (
    MBT_LEFT,
    MBT_MIDDLE,
    MBT_RIGHT
  );

  // /include/internal/cef_types.h (cef_return_value_t)
  TCefReturnValue = (
    RV_CANCEL = 0,
    RV_CONTINUE,
    RV_CONTINUE_ASYNC
  );

  // /include/internal/cef_types.h (cef_urlrequest_status_t)
  TCefUrlRequestStatus = (
    UR_UNKNOWN = 0,
    UR_SUCCESS,
    UR_IO_PENDING,
    UR_CANCELED,
    UR_FAILED
  );

  // /include/internal/cef_types.h (cef_termination_status_t)
  TCefTerminationStatus = (
    TS_ABNORMAL_TERMINATION,
    TS_PROCESS_WAS_KILLED,
    TS_PROCESS_CRASHED,
    TS_PROCESS_OOM
  );

  // /include/internal/cef_types.h (cef_path_key_t)
  TCefPathKey = (
    PK_DIR_CURRENT,
    PK_DIR_EXE,
    PK_DIR_MODULE,
    PK_DIR_TEMP,
    PK_FILE_EXE,
    PK_FILE_MODULE,
    PK_LOCAL_APP_DATA,
    PK_USER_DATA,
    PK_DIR_RESOURCES
  );

  // /include/internal/cef_types.h (cef_storage_type_t)
  TCefStorageType = (
    ST_LOCALSTORAGE = 0,
    ST_SESSIONSTORAGE
  );

  // /include/internal/cef_types.h (cef_response_filter_status_t)
  TCefResponseFilterStatus = (
    RESPONSE_FILTER_NEED_MORE_DATA,
    RESPONSE_FILTER_DONE,
    RESPONSE_FILTER_ERROR
  );

  // /include/internal/cef_types.h (cef_plugin_policy_t)
  TCefPluginPolicy = (
    PLUGIN_POLICY_ALLOW,
    PLUGIN_POLICY_DETECT_IMPORTANT,
    PLUGIN_POLICY_BLOCK,
    PLUGIN_POLICY_DISABLE
  );

  // cef/libcef/common/cef_switches.cc (values for the --plugin-policy switch)
  TCefPluginPolicySwitch = (
    PLUGIN_POLICY_SWITCH_ALLOW, // Default value
    PLUGIN_POLICY_SWITCH_DETECT,
    PLUGIN_POLICY_SWITCH_BLOCK
  );

  // /include/internal/cef_types.h (cef_color_type_t)
  TCefColorType = (
    CEF_COLOR_TYPE_RGBA_8888,
    CEF_COLOR_TYPE_BGRA_8888
  );

  // /include/internal/cef_types.h (cef_alpha_type_t)
  TCefAlphaType = (
    CEF_ALPHA_TYPE_OPAQUE,
    CEF_ALPHA_TYPE_PREMULTIPLIED,
    CEF_ALPHA_TYPE_POSTMULTIPLIED
  );

  // /include/internal/cef_types.h (cef_text_style_t)
  TCefTextStyle = (
    CEF_TEXT_STYLE_BOLD,
    CEF_TEXT_STYLE_ITALIC,
    CEF_TEXT_STYLE_STRIKE,
    CEF_TEXT_STYLE_DIAGONAL_STRIKE,
    CEF_TEXT_STYLE_UNDERLINE
  );

  // /include/internal/cef_types.h (cef_main_axis_alignment_t)
  TCefMainAxisAlignment = (
    CEF_MAIN_AXIS_ALIGNMENT_START,
    CEF_MAIN_AXIS_ALIGNMENT_CENTER,
    CEF_MAIN_AXIS_ALIGNMENT_END
  );

  // /include/internal/cef_types.h (cef_cross_axis_alignment_t)
  TCefCrossAxisAlignment = (
    CEF_CROSS_AXIS_ALIGNMENT_STRETCH,
    CEF_CROSS_AXIS_ALIGNMENT_START,
    CEF_CROSS_AXIS_ALIGNMENT_CENTER,
    CEF_CROSS_AXIS_ALIGNMENT_END
  );

  // /include/internal/cef_types.h (cef_pdf_print_margin_type_t)
  TCefPdfPrintMarginType = (
    PDF_PRINT_MARGIN_DEFAULT,
    PDF_PRINT_MARGIN_NONE,
    PDF_PRINT_MARGIN_MINIMUM,
    PDF_PRINT_MARGIN_CUSTOM
  );

  // /include/internal/cef_types.h (cef_color_model_t)
  TCefColorModel = (
    COLOR_MODEL_UNKNOWN,
    COLOR_MODEL_GRAY,
    COLOR_MODEL_COLOR,
    COLOR_MODEL_CMYK,
    COLOR_MODEL_CMY,
    COLOR_MODEL_KCMY,
    COLOR_MODEL_CMY_K,
    COLOR_MODEL_BLACK,
    COLOR_MODEL_GRAYSCALE,
    COLOR_MODEL_RGB,
    COLOR_MODEL_RGB16,
    COLOR_MODEL_RGBA,
    COLOR_MODEL_COLORMODE_COLOR,
    COLOR_MODEL_COLORMODE_MONOCHROME,
    COLOR_MODEL_HP_COLOR_COLOR,
    COLOR_MODEL_HP_COLOR_BLACK,
    COLOR_MODEL_PRINTOUTMODE_NORMAL,
    COLOR_MODEL_PRINTOUTMODE_NORMAL_GRAY,
    COLOR_MODEL_PROCESSCOLORMODEL_CMYK,
    COLOR_MODEL_PROCESSCOLORMODEL_GREYSCALE,
    COLOR_MODEL_PROCESSCOLORMODEL_RGB
  );

  // /include/internal/cef_types.h (cef_json_parser_options_t)
  TCefJsonParserOptions = (
    JSON_PARSER_RFC = 0,
    JSON_PARSER_ALLOW_TRAILING_COMMAS = 1 shl 0
  );

  // /include/internal/cef_types.h (cef_xml_encoding_type_t)
  TCefXmlEncodingType = (
    XML_ENCODING_NONE = 0,
    XML_ENCODING_UTF8,
    XML_ENCODING_UTF16LE,
    XML_ENCODING_UTF16BE,
    XML_ENCODING_ASCII
  );

  // /include/internal/cef_types.h (cef_xml_node_type_t)
  TCefXmlNodeType = (
    XML_NODE_UNSUPPORTED = 0,
    XML_NODE_PROCESSING_INSTRUCTION,
    XML_NODE_DOCUMENT_TYPE,
    XML_NODE_ELEMENT_START,
    XML_NODE_ELEMENT_END,
    XML_NODE_ATTRIBUTE,
    XML_NODE_TEXT,
    XML_NODE_CDATA,
    XML_NODE_ENTITY_REFERENCE,
    XML_NODE_WHITESPACE,
    XML_NODE_COMMENT
  );

  // /include/internal/cef_types.h (cef_dom_event_phase_t)
  TCefDomEventPhase = (
    DOM_EVENT_PHASE_UNKNOWN = 0,
    DOM_EVENT_PHASE_CAPTURING,
    DOM_EVENT_PHASE_AT_TARGET,
    DOM_EVENT_PHASE_BUBBLING
  );

  // /include/internal/cef_types.h (cef_button_state_t)
  TCefButtonState = (
    CEF_BUTTON_STATE_NORMAL,
    CEF_BUTTON_STATE_HOVERED,
    CEF_BUTTON_STATE_PRESSED,
    CEF_BUTTON_STATE_DISABLED
  );

  // /include/internal/cef_types.h (cef_horizontal_alignment_t)
  TCefHorizontalAlignment = (
    CEF_HORIZONTAL_ALIGNMENT_LEFT,
    CEF_HORIZONTAL_ALIGNMENT_CENTER,
    CEF_HORIZONTAL_ALIGNMENT_RIGHT
  );

  // /include/internal/cef_types.h (cef_menu_anchor_position_t)
  TCefMenuAnchorPosition = (
    CEF_MENU_ANCHOR_TOPLEFT,
    CEF_MENU_ANCHOR_TOPRIGHT,
    CEF_MENU_ANCHOR_BOTTOMCENTER
  );

  // /include/internal/cef_types.h (cef_menu_color_type_t)
  TCefMenuColorType = (
    CEF_MENU_COLOR_TEXT,
    CEF_MENU_COLOR_TEXT_HOVERED,
    CEF_MENU_COLOR_TEXT_ACCELERATOR,
    CEF_MENU_COLOR_TEXT_ACCELERATOR_HOVERED,
    CEF_MENU_COLOR_BACKGROUND,
    CEF_MENU_COLOR_BACKGROUND_HOVERED,
    CEF_MENU_COLOR_COUNT
  );

  // /include/internal/cef_types.h (cef_cdm_registration_error_t)
  TCefCDMRegistrationError = (
    CEF_CDM_REGISTRATION_ERROR_NONE,
    CEF_CDM_REGISTRATION_ERROR_INCORRECT_CONTENTS,
    CEF_CDM_REGISTRATION_ERROR_INCOMPATIBLE,
    CEF_CDM_REGISTRATION_ERROR_NOT_SUPPORTED
  );

  // Values for browser preference "net.network_prediction_options"
  // https://source.chromium.org/chromium/chromium/src/+/master:chrome/browser/net/prediction_options.h
  TCefNetworkPredictionOptions = (
    CEF_NETWORK_PREDICTION_ALWAYS,
    CEF_NETWORK_PREDICTION_WIFI_ONLY,
    CEF_NETWORK_PREDICTION_NEVER
    // CEF_NETWORK_PREDICTION_DEFAULT = CEF_NETWORK_PREDICTION_WIFI_ONLY;
  );

  // /include/internal/cef_types.h (cef_composition_underline_style_t)
  TCefCompositionUnderlineStyle = (
    CEF_CUS_SOLID,
    CEF_CUS_DOT,
    CEF_CUS_DASH,
    CEF_CUS_NONE
  );

  // /include/internal/cef_types.h (cef_composition_underline_t)
  TCefCompositionUnderline = record
    range            : TCefRange;
    color            : TCefColor;
    background_color : TCefColor;
    thick            : integer;
    style            : TCefCompositionUnderlineStyle;
  end;
  TCefCompositionUnderlineDynArray = array of TCefCompositionUnderline;

  // /include/internal/cef_time.h (cef_time_t)
  TCefTime = record
    year         : Integer;
    month        : Integer;
    day_of_week  : Integer;
    day_of_month : Integer;
    hour         : Integer;
    minute       : Integer;
    second       : Integer;
    millisecond  : Integer;
  end;

  // /include/internal/cef_types.h (cef_box_layout_settings_t)
  TCefBoxLayoutSettings = record
    horizontal                       : Integer;
    inside_border_horizontal_spacing : Integer;
    inside_border_vertical_spacing   : Integer;
    inside_border_insets             : TCefInsets;
    between_child_spacing            : Integer;
    main_axis_alignment              : TCefMainAxisAlignment;
    cross_axis_alignment             : TCefCrossAxisAlignment;
    minimum_cross_axis_size          : Integer;
    default_flex                     : Integer;
  end;

  // /include/internal/cef_types.h (cef_settings_t)
  TCefSettings = record
    size                                     : NativeUInt;
    no_sandbox                               : Integer;
    browser_subprocess_path                  : TCefString;
    framework_dir_path                       : TCefString;
    main_bundle_path                         : TCefString;  // Only used in macOS
    chrome_runtime                           : Integer;
    multi_threaded_message_loop              : Integer;
    external_message_pump                    : Integer;
    windowless_rendering_enabled             : Integer;
    command_line_args_disabled               : Integer;
    cache_path                               : TCefString;
    root_cache_path                          : TCefString;
    user_data_path                           : TCefString;
    persist_session_cookies                  : Integer;
    persist_user_preferences                 : Integer;
    user_agent                               : TCefString;
    product_version                          : TCefString;
    locale                                   : TCefString;
    log_file                                 : TCefString;
    log_severity                             : TCefLogSeverity;
    javascript_flags                         : TCefString;
    resources_dir_path                       : TCefString;
    locales_dir_path                         : TCefString;
    pack_loading_disabled                    : Integer;
    remote_debugging_port                    : Integer;
    uncaught_exception_stack_size            : Integer;
    ignore_certificate_errors                : Integer;
    background_color                         : TCefColor;
    accept_language_list                     : TCefString;
    application_client_id_for_file_scanning  : TCefString;
  end;

  // /include/internal/cef_types_win.h (cef_window_info_t)
  TCefWindowInfo = record
    {$IFDEF MSWINDOWS}
    ex_style                      : DWORD;
    window_name                   : TCefString;
    style                         : DWORD;
    x                             : Integer;
    y                             : Integer;
    width                         : Integer;
    height                        : Integer;
    parent_window                 : TCefWindowHandle;
    menu                          : HMENU;
    windowless_rendering_enabled  : Integer;
    shared_texture_enabled        : Integer;
    external_begin_frame_enabled  : Integer;
    window                        : TCefWindowHandle;
    {$ENDIF}
    {$IFDEF MACOS}
    window_name                   : TCefString;
    x                             : Integer;
    y                             : Integer;
    width                         : Integer;
    height                        : Integer;
    hidden                        : Integer;
    parent_view                   : TCefWindowHandle;
    windowless_rendering_enabled  : Integer;
    shared_texture_enabled        : Integer;
    external_begin_frame_enabled  : Integer;
    view                          : TCefWindowHandle;
    {$ENDIF}
    {$IFDEF LINUX}
    window_name                   : TCefString;
    x                             : uint32;
    y                             : uint32;
    width                         : uint32;
    height                        : uint32;
    parent_window                 : TCefWindowHandle;
    windowless_rendering_enabled  : Integer;
    shared_texture_enabled        : Integer;
    external_begin_frame_enabled  : Integer;
    window                        : TCefWindowHandle;
    {$ENDIF}
  end;

  // /include/internal/cef_types.h (cef_draggable_region_t)
  TCefDraggableRegion = record
    bounds    : TCefRect;
    draggable : Integer;
  end;

  TCefDraggableRegionArray = array[0..(High(Integer) div SizeOf(TCefDraggableRegion))-1]  of TCefDraggableRegion;

  // /include/internal/cef_types.h (cef_key_event_t)
  TCefKeyEvent = record
    kind                    : TCefKeyEventType;  // called 'type' in the original CEF source code
    modifiers               : TCefEventFlags;
    windows_key_code        : Integer;
    native_key_code         : Integer;
    is_system_key           : Integer;
    character               : WideChar;
    unmodified_character    : WideChar;
    focus_on_editable_field : Integer;
  end;

  // /include/internal/cef_types.h (cef_popup_features_t)
  TCefPopupFeatures = record
    x                 : Integer;
    xSet              : Integer;
    y                 : Integer;
    ySet              : Integer;
    width             : Integer;
    widthSet          : Integer;
    height            : Integer;
    heightSet         : Integer;
    menuBarVisible    : Integer;
    statusBarVisible  : Integer;
    toolBarVisible    : Integer;
    scrollbarsVisible : Integer;
  end;

  // /include/internal/cef_types.h (cef_browser_settings_t)
  TCefBrowserSettings = record
    size                            : NativeUInt;
    windowless_frame_rate           : Integer;
    standard_font_family            : TCefString;
    fixed_font_family               : TCefString;
    serif_font_family               : TCefString;
    sans_serif_font_family          : TCefString;
    cursive_font_family             : TCefString;
    fantasy_font_family             : TCefString;
    default_font_size               : Integer;
    default_fixed_font_size         : Integer;
    minimum_font_size               : Integer;
    minimum_logical_font_size       : Integer;
    default_encoding                : TCefString;
    remote_fonts                    : TCefState;
    javascript                      : TCefState;
    javascript_close_windows        : TCefState;
    javascript_access_clipboard     : TCefState;
    javascript_dom_paste            : TCefState;
    plugins                         : TCefState;
    universal_access_from_file_urls : TCefState;
    file_access_from_file_urls      : TCefState;
    web_security                    : TCefState;
    image_loading                   : TCefState;
    image_shrink_standalone_to_fit  : TCefState;
    text_area_resize                : TCefState;
    tab_to_links                    : TCefState;
    local_storage                   : TCefState;
    databases                       : TCefState;
    application_cache               : TCefState;
    webgl                           : TCefState;
    background_color                : TCefColor;
    accept_language_list            : TCefString;
  end;

  // /include/internal/cef_types.h (cef_screen_info_t)
  TCefScreenInfo = record
    device_scale_factor : Single;
    depth               : Integer;
    depth_per_component : Integer;
    is_monochrome       : Integer;
    rect                : TCefRect;
    available_rect      : TCefRect;
  end;

  // /include/internal/cef_types.h (cef_request_context_settings_t)
  TCefRequestContextSettings = record
    size                           : NativeUInt;
    cache_path                     : TCefString;
    persist_session_cookies        : Integer;
    persist_user_preferences       : Integer;
    ignore_certificate_errors      : Integer;
    accept_language_list           : TCefString;
  end;

  // /include/internal/cef_types.h (cef_cookie_t)
  TCefCookie = record
    name        : TCefString;
    value       : TCefString;
    domain      : TCefString;
    path        : TCefString;
    secure      : Integer;
    httponly    : Integer;
    creation    : TCefTime;
    last_access : TCefTime;
    has_expires : Integer;
    expires     : TCefTime;
    same_site   : TCefCookieSameSite;
    priority    : TCefCookiePriority;
  end;

  TCookie = record
    name        : ustring;
    value       : ustring;
    domain      : ustring;
    path        : ustring;
    creation    : TDateTime;
    last_access : TDateTime;
    expires     : TDateTime;
    secure      : boolean;
    httponly    : boolean;
    has_expires : boolean;
    same_site   : TCefCookieSameSite;
    priority    : TCefCookiePriority;
  end;

  // /include/internal/cef_types.h (cef_pdf_print_settings_t)
  TCefPdfPrintSettings = record
    header_footer_title   : TCefString;
    header_footer_url     : TCefString;
    page_width            : Integer;
    page_height           : Integer;
    scale_factor          : Integer;
    margin_top            : Integer;
    margin_right          : Integer;
    margin_bottom         : Integer;
    margin_left           : Integer;
    margin_type           : TCefPdfPrintMarginType;
    header_footer_enabled : Integer;
    selection_only        : Integer;
    landscape             : Integer;
    backgrounds_enabled   : Integer;
  end;

  // /include/internal/cef_types.h (cef_mouse_event_t)
  TCefMouseEvent = record
    x         : Integer;
    y         : Integer;
    modifiers : TCefEventFlags;
  end;

  // /include/internal/cef_types.h (cef_touch_event_t)
  TCefTouchEvent = record
    id             : integer;
    x              : single;
    y              : single;
    radius_x       : single;
    radius_y       : single;
    rotation_angle : single;
    pressure       : single;
    type_          : TCefTouchEeventType;
    modifiers      : TCefEventFlags;
    pointer_type   : TCefPointerType;
  end;

  // /include/internal/cef_types.h (cef_audio_parameters_t)
  TCefAudioParameters = record
    channel_layout    : TCefChannelLayout;
    sample_rate       : integer;
    frames_per_buffer : integer;
  end;

  // /include/internal/cef_types.h (cef_media_sink_device_info_t)
  TCefMediaSinkDeviceInfo = record
    ip_address : TCefString;
    port       : integer;
    model_name : TCefString;
  end;

  // /include/capi/cef_base_capi.h (cef_base_ref_counted_t)
  TCefBaseRefCounted = record
    size                 : NativeUInt;
    add_ref              : procedure(self: PCefBaseRefCounted); stdcall;
    release              : function(self: PCefBaseRefCounted): Integer; stdcall;
    has_one_ref          : function(self: PCefBaseRefCounted): Integer; stdcall;
    has_at_least_one_ref : function(self: PCefBaseRefCounted): Integer; stdcall;
  end;

  // /include/capi/cef_base_capi.h (cef_base_scoped_t)
  TCefBaseScoped = record
    size  : NativeUInt;
    del   : procedure(self: PCefBaseScoped); stdcall;
  end;

  // /include/capi/cef_stream_capi.h (cef_stream_writer_t)
  TCefStreamWriter = record
    base      : TCefBaseRefCounted;
    write     : function(self: PCefStreamWriter; const ptr: Pointer; size, n: NativeUInt): NativeUInt; stdcall;
    seek      : function(self: PCefStreamWriter; offset: Int64; whence: Integer): Integer; stdcall;
    tell      : function(self: PCefStreamWriter): Int64; stdcall;
    flush     : function(self: PCefStreamWriter): Integer; stdcall;
    may_block : function(self: PCefStreamWriter): Integer; stdcall;
  end;

  // /include/capi/cef_x509_certificate_capi.h (cef_x509cert_principal_t)
  TCefX509CertPrincipal = record
    base                        : TCefBaseRefCounted;
    get_display_name            : function(self: PCefX509CertPrincipal): PCefStringUserFree; stdcall;
    get_common_name             : function(self: PCefX509CertPrincipal): PCefStringUserFree; stdcall;
    get_locality_name           : function(self: PCefX509CertPrincipal): PCefStringUserFree; stdcall;
    get_state_or_province_name  : function(self: PCefX509CertPrincipal): PCefStringUserFree; stdcall;
    get_country_name            : function(self: PCefX509CertPrincipal): PCefStringUserFree; stdcall;
    get_street_addresses        : procedure(self: PCefX509CertPrincipal; addresses: TCefStringList); stdcall;
    get_organization_names      : procedure(self: PCefX509CertPrincipal; names: TCefStringList); stdcall;
    get_organization_unit_names : procedure(self: PCefX509CertPrincipal; names: TCefStringList); stdcall;
    get_domain_components       : procedure(self: PCefX509CertPrincipal; components: TCefStringList); stdcall;
  end;

  // /include/capi/cef_x509_certificate_capi.h (cef_x509certificate_t)
  TCefX509Certificate = record
    base                        : TCefBaseRefCounted;
    get_subject                 : function(self: PCefX509Certificate): PCefX509CertPrincipal; stdcall;
    get_issuer                  : function(self: PCefX509Certificate): PCefX509CertPrincipal; stdcall;
    get_serial_number           : function(self: PCefX509Certificate): PCefBinaryValue; stdcall;
    get_valid_start             : function(self: PCefX509Certificate): TCefTime; stdcall;
    get_valid_expiry            : function(self: PCefX509Certificate): TCefTime; stdcall;
    get_derencoded              : function(self: PCefX509Certificate): PCefBinaryValue; stdcall;
    get_pemencoded              : function(self: PCefX509Certificate): PCefBinaryValue; stdcall;
    get_issuer_chain_size       : function(self: PCefX509Certificate): NativeUInt; stdcall;
    get_derencoded_issuer_chain : procedure(self: PCefX509Certificate; var chainCount: NativeUInt; var chain: PCefBinaryValue); stdcall;
    get_pemencoded_issuer_chain : procedure(self: PCefX509Certificate; var chainCount: NativeUInt; var chain: PCefBinaryValue); stdcall;
  end;

  // /include/capi/cef_ssl_info_capi.h (cef_sslinfo_t)
  TCefSslInfo = record
    base                : TCefBaseRefCounted;
    get_cert_status     : function(self: PCefSslInfo): TCefCertStatus; stdcall;
    get_x509certificate : function(self: PCefSslInfo): PCefX509Certificate; stdcall;
  end;

  // /include/capi/cef_ssl_status_capi.h (cef_sslstatus_t)
  TCefSSLStatus = record
    base                 : TCefBaseRefCounted;
    is_secure_connection : function(self: PCefSSLStatus): integer; stdcall;
    get_cert_status      : function(self: PCefSSLStatus): TCefCertStatus; stdcall;
    get_sslversion       : function(self: PCefSSLStatus): TCefSSLVersion; stdcall;
    get_content_status   : function(self: PCefSSLStatus): TCefSSLContentStatus; stdcall;
    get_x509certificate  : function(self: PCefSSLStatus): PCefX509Certificate; stdcall;
  end;

  // /include/capi/cef_request_handler_capi.h (cef_select_client_certificate_callback_t)
  TCefSelectClientCertificateCallback = record
    base   : TCefBaseRefCounted;
    select : procedure(self: PCefSelectClientCertificateCallback; cert: PCefX509Certificate); stdcall;
  end;

  // /include/capi/cef_context_menu_handler_capi.h (cef_run_context_menu_callback_t)
  TCefRunContextMenuCallback = record
    base   : TCefBaseRefCounted;
    cont   : procedure(self: PCefRunContextMenuCallback; command_id: Integer; event_flags: TCefEventFlags); stdcall;
    cancel : procedure(self: PCefRunContextMenuCallback); stdcall;
  end;

  // /include/capi/cef_dialog_handler_capi.h (cef_file_dialog_callback_t)
  TCefFileDialogCallback = record
    base   : TCefBaseRefCounted;
    cont   : procedure(self: PCefFileDialogCallback; selected_accept_filter: Integer; file_paths: TCefStringList); stdcall;
    cancel : procedure(self: PCefFileDialogCallback); stdcall;
  end;

  // /include/capi/cef_dialog_handler_capi.h (cef_dialog_handler_t)
  TCefDialogHandler = record
    base           : TCefBaseRefCounted;
    on_file_dialog : function(self: PCefDialogHandler; browser: PCefBrowser; mode: TCefFileDialogMode; const title, default_file_path: PCefString; accept_filters: TCefStringList; selected_accept_filter: Integer; callback: PCefFileDialogCallback): Integer; stdcall;
  end;

  // /include/capi/cef_display_handler_capi.h (cef_display_handler_t)
  TCefDisplayHandler = record
    base                       : TCefBaseRefCounted;
    on_address_change          : procedure(self: PCefDisplayHandler; browser: PCefBrowser; frame: PCefFrame; const url: PCefString); stdcall;
    on_title_change            : procedure(self: PCefDisplayHandler; browser: PCefBrowser; const title: PCefString); stdcall;
    on_favicon_urlchange       : procedure(self: PCefDisplayHandler; browser: PCefBrowser; icon_urls: TCefStringList); stdcall;
    on_fullscreen_mode_change  : procedure(self: PCefDisplayHandler; browser: PCefBrowser; fullscreen: Integer); stdcall;
    on_tooltip                 : function(self: PCefDisplayHandler; browser: PCefBrowser; text: PCefString): Integer; stdcall;
    on_status_message          : procedure(self: PCefDisplayHandler; browser: PCefBrowser; const value: PCefString); stdcall;
    on_console_message         : function(self: PCefDisplayHandler; browser: PCefBrowser; level: TCefLogSeverity; const message_, source: PCefString; line: Integer): Integer; stdcall;
    on_auto_resize             : function(self: PCefDisplayHandler; browser: PCefBrowser; const new_size: PCefSize): Integer; stdcall;
    on_loading_progress_change : procedure(self: PCefDisplayHandler; browser: PCefBrowser; progress: double); stdcall;
    on_cursor_change           : function(self: PCefDisplayHandler; browser: PCefBrowser; cursor: TCefCursorHandle; type_: TCefCursorType; const custom_cursor_info: PCefCursorInfo): Integer; stdcall;
  end;

  // /include/capi/cef_download_handler_capi.h (cef_download_handler_t)
  TCefDownloadHandler = record
    base                : TCefBaseRefCounted;
    on_before_download  : procedure(self: PCefDownloadHandler; browser: PCefBrowser; download_item: PCefDownloadItem; const suggested_name: PCefString; callback: PCefBeforeDownloadCallback); stdcall;
    on_download_updated : procedure(self: PCefDownloadHandler; browser: PCefBrowser; download_item: PCefDownloadItem; callback: PCefDownloadItemCallback); stdcall;
  end;

  // /include/capi/cef_drag_handler_capi.h (cef_drag_handler_t)
  TCefDragHandler = record
    base                         : TCefBaseRefCounted;
    on_drag_enter                : function(self: PCefDragHandler; browser: PCefBrowser; dragData: PCefDragData; mask: TCefDragOperations): Integer; stdcall;
    on_draggable_regions_changed : procedure(self: PCefDragHandler; browser: PCefBrowser; frame: PCefFrame; regionsCount: NativeUInt; const regions: PCefDraggableRegionArray); stdcall;
  end;

  // /include/capi/cef_find_handler_capi.h (cef_find_handler_t)
  TCefFindHandler = record
    base           : TCefBaseRefCounted;
    on_find_result : procedure(self: PCefFindHandler; browser: PCefBrowser; identifier, count: Integer; const selection_rect: PCefRect; active_match_ordinal, final_update: Integer); stdcall;
  end;

  // /include/capi/cef_focus_handler_capi.h (cef_focus_handler_t)
  TCefFocusHandler = record
    base          : TCefBaseRefCounted;
    on_take_focus : procedure(self: PCefFocusHandler; browser: PCefBrowser; next: Integer); stdcall;
    on_set_focus  : function(self: PCefFocusHandler; browser: PCefBrowser; source: TCefFocusSource): Integer; stdcall;
    on_got_focus  : procedure(self: PCefFocusHandler; browser: PCefBrowser); stdcall;
  end;

  // /include/capi/cef_jsdialog_handler_capi.h (cef_jsdialog_handler_t)
  TCefJsDialogHandler = record
    base                    : TCefBaseRefCounted;
    on_jsdialog             : function(self: PCefJsDialogHandler; browser: PCefBrowser; const origin_url: PCefString; dialog_type: TCefJsDialogType; const message_text, default_prompt_text: PCefString; callback: PCefJsDialogCallback; suppress_message: PInteger): Integer; stdcall;
    on_before_unload_dialog : function(self: PCefJsDialogHandler; browser: PCefBrowser; const message_text: PCefString; is_reload: Integer; callback: PCefJsDialogCallback): Integer; stdcall;
    on_reset_dialog_state   : procedure(self: PCefJsDialogHandler; browser: PCefBrowser); stdcall;
    on_dialog_closed        : procedure(self: PCefJsDialogHandler; browser: PCefBrowser); stdcall;
  end;

  // /include/capi/cef_jsdialog_handler_capi.h (cef_jsdialog_callback_t)
  TCefJsDialogCallback = record
    base : TCefBaseRefCounted;
    cont : procedure(self: PCefJsDialogCallback; success: Integer; const user_input: PCefString); stdcall;
  end;

  // /include/capi/cef_keyboard_handler_capi.h (cef_keyboard_handler_t)
  TCefKeyboardHandler = record
    base             : TCefBaseRefCounted;
    on_pre_key_event : function(self: PCefKeyboardHandler; browser: PCefBrowser; const event: PCefKeyEvent; os_event: TCefEventHandle; is_keyboard_shortcut: PInteger): Integer; stdcall;
    on_key_event     : function(self: PCefKeyboardHandler; browser: PCefBrowser; const event: PCefKeyEvent; os_event: TCefEventHandle): Integer; stdcall;
  end;

  // /include/capi/cef_life_span_handler_capi.h (cef_life_span_handler_t)
  TCefLifeSpanHandler = record
    base              : TCefBaseRefCounted;
    on_before_popup   : function(self: PCefLifeSpanHandler; browser: PCefBrowser; frame: PCefFrame; const target_url, target_frame_name: PCefString; target_disposition: TCefWindowOpenDisposition; user_gesture: Integer; const popupFeatures: PCefPopupFeatures; windowInfo: PCefWindowInfo; var client: PCefClient; settings: PCefBrowserSettings; var extra_info: PCefDictionaryValue; no_javascript_access: PInteger): Integer; stdcall;
    on_after_created  : procedure(self: PCefLifeSpanHandler; browser: PCefBrowser); stdcall;
    do_close          : function(self: PCefLifeSpanHandler; browser: PCefBrowser): Integer; stdcall;
    on_before_close   : procedure(self: PCefLifeSpanHandler; browser: PCefBrowser); stdcall;
  end;

  // /include/capi/cef_registration_capi.h (cef_registration_t)
  TCefRegistration = record
    base  : TCefBaseRefCounted;
  end;

  // /include/capi/cef_devtools_message_observer_capi.h (cef_dev_tools_message_observer_t)
  TCefDevToolsMessageObserver = record
    base                        : TCefBaseRefCounted;
    on_dev_tools_message        : function(self: PCefDevToolsMessageObserver; browser: PCefBrowser; const message_: Pointer; message_size: NativeUInt): Integer; stdcall;
    on_dev_tools_method_result  : procedure(self: PCefDevToolsMessageObserver; browser: PCefBrowser; message_id, success: Integer; const result: Pointer; result_size: NativeUInt); stdcall;
    on_dev_tools_event          : procedure(self: PCefDevToolsMessageObserver; browser: PCefBrowser; const method: PCefString; const params: Pointer; params_size: NativeUInt); stdcall;
    on_dev_tools_agent_attached : procedure(self: PCefDevToolsMessageObserver; browser: PCefBrowser); stdcall;
    on_dev_tools_agent_detached : procedure(self: PCefDevToolsMessageObserver; browser: PCefBrowser); stdcall;
  end;

  // /include/capi/cef_media_router_capi.h (cef_media_router_t)
  TCefMediaRouter = record
    base                  : TCefBaseRefCounted;
    add_observer          : function(self: PCefMediaRouter; observer: PCefMediaObserver): PCefRegistration; stdcall;
    get_source            : function(self: PCefMediaRouter; const urn: PCefString): PCefMediaSource; stdcall;
    notify_current_sinks  : procedure(self: PCefMediaRouter); stdcall;
    create_route          : procedure(self: PCefMediaRouter; source: PCefMediaSource; sink: PCefMediaSink; callback: PCefMediaRouteCreateCallback); stdcall;
    notify_current_routes : procedure(self: PCefMediaRouter); stdcall;
  end;

  // /include/capi/cef_media_router_capi.h (cef_media_observer_t)
  TCefMediaObserver = record
    base                      : TCefBaseRefCounted;
    on_sinks                  : procedure(self: PCefMediaObserver; sinksCount: NativeUInt; const sinks: PPCefMediaSink); stdcall;
    on_routes                 : procedure(self: PCefMediaObserver; routesCount: NativeUInt; const routes: PPCefMediaRoute); stdcall;
    on_route_state_changed    : procedure(self: PCefMediaObserver; route: PCefMediaRoute; state: TCefMediaRouteConnectionState); stdcall;
    on_route_message_received : procedure(self: PCefMediaObserver; route: PCefMediaRoute; const message_: Pointer; message_size: NativeUInt); stdcall;
  end;

  // /include/capi/cef_media_router_capi.h (cef_media_route_t)
  TCefMediaRoute = record
    base                  : TCefBaseRefCounted;
    get_id                : function(self: PCefMediaRoute): PCefStringUserFree; stdcall;
    get_source            : function(self: PCefMediaRoute): PCefMediaSource; stdcall;
    get_sink              : function(self: PCefMediaRoute): PCefMediaSink; stdcall;
    send_route_message    : procedure(self: PCefMediaRoute; const message_: Pointer; message_size: NativeUInt); stdcall;
    terminate             : procedure(self: PCefMediaRoute); stdcall;
  end;

  // /include/capi/cef_media_router_capi.h (cef_media_route_create_callback_t)
  TCefMediaRouteCreateCallback = record
    base                           : TCefBaseRefCounted;
    on_media_route_create_finished : procedure(self: PCefMediaRouteCreateCallback; result: TCefMediaRouterCreateResult; const error: PCefString; route: PCefMediaRoute); stdcall;
  end;

  // /include/capi/cef_media_router_capi.h (cef_media_sink_t)
  TCefMediaSink = record
    base                  : TCefBaseRefCounted;
    get_id                : function(self: PCefMediaSink): PCefStringUserFree; stdcall;
    get_name              : function(self: PCefMediaSink): PCefStringUserFree; stdcall;
    get_description       : function(self: PCefMediaSink): PCefStringUserFree; stdcall;
    get_icon_type         : function(self: PCefMediaSink): TCefMediaSinkIconType; stdcall;
    get_device_info       : procedure(self: PCefMediaSink; callback: PCefMediaSinkDeviceInfoCallback); stdcall;
    is_cast_sink          : function(self: PCefMediaSink): Integer; stdcall;
    is_dial_sink          : function(self: PCefMediaSink): Integer; stdcall;
    is_compatible_with    : function(self: PCefMediaSink; source: PCefMediaSource): Integer; stdcall;
  end;

  // /include/capi/cef_media_router_capi.h (cef_media_sink_device_info_callback_t)
  TCefMediaSinkDeviceInfoCallback = record
    base                      : TCefBaseRefCounted;
    on_media_sink_device_info : procedure(self: PCefMediaSinkDeviceInfoCallback; device_info: PCefMediaSinkDeviceInfo); stdcall;
  end;

  // /include/capi/cef_media_router_capi.h (cef_media_source_t)
  TCefMediaSource = record
    base                  : TCefBaseRefCounted;
    get_id                : function(self: PCefMediaSource): PCefStringUserFree; stdcall;
    is_cast_source        : function(self: PCefMediaSource): Integer; stdcall;
    is_dial_source        : function(self: PCefMediaSource): Integer; stdcall;
  end;

  // /include/capi/cef_extension_handler_capi.h (cef_get_extension_resource_callback_t)
  TCefGetExtensionResourceCallback = record
    base   : TCefBaseRefCounted;
    cont   : procedure(self: PCefGetExtensionResourceCallback; stream: PCefStreamReader); stdcall;
    cancel : procedure(self: PCefGetExtensionResourceCallback); stdcall;
  end;

  // /include/capi/cef_extension_handler_capi.h (cef_extension_handler_t)
  TCefExtensionHandler = record
    base                          : TCefBaseRefCounted;
    on_extension_load_failed      : procedure(self: PCefExtensionHandler; result: TCefErrorcode); stdcall;
    on_extension_loaded           : procedure(self: PCefExtensionHandler; extension: PCefExtension); stdcall;
    on_extension_unloaded         : procedure(self: PCefExtensionHandler; extension: PCefExtension); stdcall;
    on_before_background_browser  : function(self: PCefExtensionHandler; extension: PCefExtension; const url: PCefString; var client: PCefClient; settings: PCefBrowserSettings) : Integer; stdcall;
    on_before_browser             : function(self: PCefExtensionHandler; extension: PCefExtension; browser, active_browser: PCefBrowser; index: Integer; const url: PCefString; active: Integer; windowInfo: PCefWindowInfo; var client: PCefClient; settings: PCefBrowserSettings) : Integer; stdcall;
    get_active_browser            : function(self: PCefExtensionHandler; extension: PCefExtension; browser: PCefBrowser; include_incognito: Integer): PCefBrowser; stdcall;
    can_access_browser            : function(self: PCefExtensionHandler; extension: PCefExtension; browser: PCefBrowser; include_incognito: Integer; target_browser: PCefBrowser): Integer; stdcall;
    get_extension_resource        : function(self: PCefExtensionHandler; extension: PCefExtension; browser: PCefBrowser; const file_: PCefString; callback: PCefGetExtensionResourceCallback): Integer; stdcall;
  end;

  // /include/capi/cef_audio_handler_capi.h (cef_audio_handler_t)
  TCefAudioHandler = record
    base                          : TCefBaseRefCounted;
    get_audio_parameters          : function(self: PCefAudioHandler; browser: PCefBrowser; params: PCefAudioParameters): Integer; stdcall;
    on_audio_stream_started       : procedure(self: PCefAudioHandler; browser: PCefBrowser; const params: PCefAudioParameters; channels: integer); stdcall;
    on_audio_stream_packet        : procedure(self: PCefAudioHandler; browser: PCefBrowser; const data : PPSingle; frames: integer; pts: int64); stdcall;
    on_audio_stream_stopped       : procedure(self: PCefAudioHandler; browser: PCefBrowser); stdcall;
    on_audio_stream_error         : procedure(self: PCefAudioHandler; browser: PCefBrowser; const message_: PCefString); stdcall;
  end;

  // /include/capi/cef_extension_capi.h (cef_extension_t)
  TCefExtension = record
    base                : TCefBaseRefCounted;
    get_identifier      : function(self: PCefExtension) : PCefStringUserFree; stdcall;
    get_path            : function(self: PCefExtension) : PCefStringUserFree; stdcall;
    get_manifest        : function(self: PCefExtension) : PCefDictionaryValue; stdcall;
    is_same             : function(self, that: PCefExtension) : Integer; stdcall;
    get_handler         : function(self: PCefExtension) : PCefExtensionHandler; stdcall;
    get_loader_context  : function(self: PCefExtension) : PCefRequestContext; stdcall;
    is_loaded           : function(self: PCefExtension) : Integer; stdcall;
    unload              : procedure(self: PCefExtension); stdcall;
  end;

  // /include/capi/cef_load_handler_capi.h (cef_load_handler_t)
  TCefLoadHandler = record
    base                    : TCefBaseRefCounted;
    on_loading_state_change : procedure(self: PCefLoadHandler; browser: PCefBrowser; isLoading, canGoBack, canGoForward: Integer); stdcall;
    on_load_start           : procedure(self: PCefLoadHandler; browser: PCefBrowser; frame: PCefFrame; transition_type: TCefTransitionType); stdcall;
    on_load_end             : procedure(self: PCefLoadHandler; browser: PCefBrowser; frame: PCefFrame; httpStatusCode: Integer); stdcall;
    on_load_error           : procedure(self: PCefLoadHandler; browser: PCefBrowser; frame: PCefFrame; errorCode: TCefErrorCode; const errorText, failedUrl: PCefString); stdcall;
  end;

  // /include/capi/cef_render_handler_capi.h (cef_render_handler_t)
  TCefRenderHandler = record
    base                              : TCefBaseRefCounted;
    get_accessibility_handler         : function(self: PCefRenderHandler): PCefAccessibilityHandler; stdcall;
    get_root_screen_rect              : function(self: PCefRenderHandler; browser: PCefBrowser; rect: PCefRect): Integer; stdcall;
    get_view_rect                     : procedure(self: PCefRenderHandler; browser: PCefBrowser; rect: PCefRect); stdcall;
    get_screen_point                  : function(self: PCefRenderHandler; browser: PCefBrowser; viewX, viewY: Integer; screenX, screenY: PInteger): Integer; stdcall;
    get_screen_info                   : function(self: PCefRenderHandler; browser: PCefBrowser; screen_info: PCefScreenInfo): Integer; stdcall;
    on_popup_show                     : procedure(self: PCefRenderHandler; browser: PCefBrowser; show: Integer); stdcall;
    on_popup_size                     : procedure(self: PCefRenderHandler; browser: PCefBrowser; const rect: PCefRect); stdcall;
    on_paint                          : procedure(self: PCefRenderHandler; browser: PCefBrowser; type_: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; const buffer: Pointer; width, height: Integer); stdcall;
    on_accelerated_paint              : procedure(self: PCefRenderHandler; browser: PCefBrowser; type_: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; shared_handle: Pointer); stdcall;
    start_dragging                    : function(self: PCefRenderHandler; browser: PCefBrowser; drag_data: PCefDragData; allowed_ops: TCefDragOperations; x, y: Integer): Integer; stdcall;
    update_drag_cursor                : procedure(self: PCefRenderHandler; browser: PCefBrowser; operation: TCefDragOperation); stdcall;
    on_scroll_offset_changed          : procedure(self: PCefRenderHandler; browser: PCefBrowser; x, y: Double); stdcall;
    on_ime_composition_range_changed  : procedure(self: PCefRenderHandler; browser: PCefBrowser; const selected_range: PCefRange; character_boundsCount: NativeUInt; const character_bounds: PCefRect); stdcall;
    on_text_selection_changed         : procedure(self: PCefRenderHandler; browser: PCefBrowser; const selected_text: PCefString; const selected_range: PCefRange); stdcall;
    on_virtual_keyboard_requested     : procedure(self: PCefRenderHandler; browser: PCefBrowser; input_mode: TCefTextInpuMode); stdcall;
  end;

  // /include/capi/cef_v8_capi.h (cef_v8stack_trace_t)
  TCefV8StackTrace = record
    base            : TCefBaseRefCounted;
    is_valid        : function(self: PCefV8StackTrace): Integer; stdcall;
    get_frame_count : function(self: PCefV8StackTrace): Integer; stdcall;
    get_frame       : function(self: PCefV8StackTrace; index: Integer): PCefV8StackFrame; stdcall;
  end;

  // /include/capi/cef_v8_capi.h (cef_v8stack_frame_t)
  TCefV8StackFrame = record
    base                          : TCefBaseRefCounted;
    is_valid                      : function(self: PCefV8StackFrame): Integer; stdcall;
    get_script_name               : function(self: PCefV8StackFrame): PCefStringUserFree; stdcall;
    get_script_name_or_source_url : function(self: PCefV8StackFrame): PCefStringUserFree; stdcall;
    get_function_name             : function(self: PCefV8StackFrame): PCefStringUserFree; stdcall;
    get_line_number               : function(self: PCefV8StackFrame): Integer; stdcall;
    get_column                    : function(self: PCefV8StackFrame): Integer; stdcall;
    is_eval                       : function(self: PCefV8StackFrame): Integer; stdcall;
    is_constructor                : function(self: PCefV8StackFrame): Integer; stdcall;
  end;

  // TCefStreamReader is used with ICefStreamReader and ICefCustomStreamReader !!!!
  // /include/capi/cef_stream_capi.h (cef_stream_reader_t)
  TCefStreamReader = record
    base      : TCefBaseRefCounted;
    read      : function(self: PCefStreamReader; ptr: Pointer; size, n: NativeUInt): NativeUInt; stdcall;
    seek      : function(self: PCefStreamReader; offset: Int64; whence: Integer): Integer; stdcall;
    tell      : function(self: PCefStreamReader): Int64; stdcall;
    eof       : function(self: PCefStreamReader): Integer; stdcall;
    may_block : function(self: PCefStreamReader): Integer; stdcall;
  end;

  // /include/capi/cef_stream_capi.h (cef_read_handler_t)
  TCefReadHandler = record
    base      : TCefBaseRefCounted;
    read      : function(self: PCefReadHandler; ptr: Pointer; size, n: NativeUInt): NativeUInt; stdcall;
    seek      : function(self: PCefReadHandler; offset: Int64; whence: Integer): Integer; stdcall;
    tell      : function(self: PCefReadHandler): Int64; stdcall;
    eof       : function(self: PCefReadHandler): Integer; stdcall;
    may_block : function(self: PCefReadHandler): Integer; stdcall;
  end;

  // /include/capi/cef_stream_capi.h (cef_write_handler_t)
  TCefWriteHandler = record
    base      : TCefBaseRefCounted;
    write     : function(self: PCefWriteHandler; const ptr: Pointer; size, n: NativeUInt): NativeUInt; stdcall;
    seek      : function(self: PCefWriteHandler; offset: Int64; whence: Integer): Integer; stdcall;
    tell      : function(self: PCefWriteHandler): Int64; stdcall;
    flush     : function(self: PCefWriteHandler): Integer; stdcall;
    may_block : function(self: PCefWriteHandler): Integer; stdcall;
  end;

  // /include/capi/cef_xml_reader_capi.h (cef_xml_reader_t)
  TCefXmlReader = record
    base                      : TCefBaseRefCounted;
    move_to_next_node         : function(self: PCefXmlReader): Integer; stdcall;
    close                     : function(self: PCefXmlReader): Integer; stdcall;
    has_error                 : function(self: PCefXmlReader): Integer; stdcall;
    get_error                 : function(self: PCefXmlReader): PCefStringUserFree; stdcall;
    get_type                  : function(self: PCefXmlReader): TCefXmlNodeType; stdcall;
    get_depth                 : function(self: PCefXmlReader): Integer; stdcall;
    get_local_name            : function(self: PCefXmlReader): PCefStringUserFree; stdcall;
    get_prefix                : function(self: PCefXmlReader): PCefStringUserFree; stdcall;
    get_qualified_name        : function(self: PCefXmlReader): PCefStringUserFree; stdcall;
    get_namespace_uri         : function(self: PCefXmlReader): PCefStringUserFree; stdcall;
    get_base_uri              : function(self: PCefXmlReader): PCefStringUserFree; stdcall;
    get_xml_lang              : function(self: PCefXmlReader): PCefStringUserFree; stdcall;
    is_empty_element          : function(self: PCefXmlReader): Integer; stdcall;
    has_value                 : function(self: PCefXmlReader): Integer; stdcall;
    get_value                 : function(self: PCefXmlReader): PCefStringUserFree; stdcall;
    has_attributes            : function(self: PCefXmlReader): Integer; stdcall;
    get_attribute_count       : function(self: PCefXmlReader): NativeUInt; stdcall;
    get_attribute_byindex     : function(self: PCefXmlReader; index: Integer): PCefStringUserFree; stdcall;
    get_attribute_byqname     : function(self: PCefXmlReader; const qualifiedName: PCefString): PCefStringUserFree; stdcall;
    get_attribute_bylname     : function(self: PCefXmlReader; const localName, namespaceURI: PCefString): PCefStringUserFree; stdcall;
    get_inner_xml             : function(self: PCefXmlReader): PCefStringUserFree; stdcall;
    get_outer_xml             : function(self: PCefXmlReader): PCefStringUserFree; stdcall;
    get_line_number           : function(self: PCefXmlReader): Integer; stdcall;
    move_to_attribute_byindex : function(self: PCefXmlReader; index: Integer): Integer; stdcall;
    move_to_attribute_byqname : function(self: PCefXmlReader; const qualifiedName: PCefString): Integer; stdcall;
    move_to_attribute_bylname : function(self: PCefXmlReader; const localName, namespaceURI: PCefString): Integer; stdcall;
    move_to_first_attribute   : function(self: PCefXmlReader): Integer; stdcall;
    move_to_next_attribute    : function(self: PCefXmlReader): Integer; stdcall;
    move_to_carrying_element  : function(self: PCefXmlReader): Integer; stdcall;
  end;

  // /include/capi/cef_zip_reader_capi.h (cef_zip_reader_t)
  TCefZipReader = record
    base                    : TCefBaseRefCounted;
    move_to_first_file      : function(self: PCefZipReader): Integer; stdcall;
    move_to_next_file       : function(self: PCefZipReader): Integer; stdcall;
    move_to_file            : function(self: PCefZipReader; const fileName: PCefString; caseSensitive: Integer): Integer; stdcall;
    close                   : function(Self: PCefZipReader): Integer; stdcall;
    get_file_name           : function(Self: PCefZipReader): PCefStringUserFree; stdcall;
    get_file_size           : function(Self: PCefZipReader): Int64; stdcall;
    get_file_last_modified  : function(Self: PCefZipReader): TCefTime; stdcall;
    open_file               : function(Self: PCefZipReader; const password: PCefString): Integer; stdcall;
    close_file              : function(Self: PCefZipReader): Integer; stdcall;
    read_file               : function(Self: PCefZipReader; buffer: Pointer; bufferSize: NativeUInt): Integer; stdcall;
    tell                    : function(Self: PCefZipReader): Int64; stdcall;
    eof                     : function(Self: PCefZipReader): Integer; stdcall;
  end;

  // /include/capi/cef_urlrequest_capi.h (cef_urlrequest_client_t)
  TCefUrlrequestClient = record
    base                  : TCefBaseRefCounted;
    on_request_complete   : procedure(self: PCefUrlRequestClient; request: PCefUrlRequest); stdcall;
    on_upload_progress    : procedure(self: PCefUrlRequestClient; request: PCefUrlRequest; current, total: Int64); stdcall;
    on_download_progress  : procedure(self: PCefUrlRequestClient; request: PCefUrlRequest; current, total: Int64); stdcall;
    on_download_data      : procedure(self: PCefUrlRequestClient; request: PCefUrlRequest; const data: Pointer; data_length: NativeUInt); stdcall;
    get_auth_credentials  : function(self: PCefUrlRequestClient; isProxy: Integer; const host: PCefString; port: Integer; const realm, scheme: PCefString; callback: PCefAuthCallback): Integer; stdcall;
  end;

  // /include/capi/cef_urlrequest_capi.h (cef_urlrequest_t)
  TCefUrlRequest = record
    base                : TCefBaseRefCounted;
    get_request         : function(self: PCefUrlRequest): PCefRequest; stdcall;
    get_client          : function(self: PCefUrlRequest): PCefUrlRequestClient; stdcall;
    get_request_status  : function(self: PCefUrlRequest): TCefUrlRequestStatus; stdcall;
    get_request_error   : function(self: PCefUrlRequest): TCefErrorcode; stdcall;
    get_response        : function(self: PCefUrlRequest): PCefResponse; stdcall;
    response_was_cached : function(self: PCefUrlRequest): integer; stdcall;
    cancel              : procedure(self: PCefUrlRequest); stdcall;
  end;

  // /include/capi/cef_web_plugin_capi.h (cef_web_plugin_info_visitor_t)
  TCefWebPluginInfoVisitor = record
    base  : TCefBaseRefCounted;
    visit : function(self: PCefWebPluginInfoVisitor; info: PCefWebPluginInfo; count, total: Integer): Integer; stdcall;
  end;

  // /include/capi/cef_web_plugin_capi.h (cef_web_plugin_unstable_callback_t)
  TCefWebPluginUnstableCallback = record
    base        : TCefBaseRefCounted;
    is_unstable : procedure(self: PCefWebPluginUnstableCallback; const path: PCefString; unstable: Integer); stdcall;
  end;

  // /include/capi/cef_web_plugin_capi.h (cef_register_cdm_callback_t)
  TCefRegisterCDMCallback = record
    base                          : TCefBaseRefCounted;
    on_cdm_registration_complete  : procedure(self:PCefRegisterCDMCallback; result: TCefCDMRegistrationError; const error_message: PCefString); stdcall;
  end;

  // /include/capi/cef_thread_capi.h (cef_thread_t)
  TCefThread = record
    base                    : TCefBaseRefCounted;
    get_task_runner         : function(self: PCefThread): PCefTaskRunner; stdcall;
    get_platform_thread_id  : function(self: PCefThread): TCefPlatformThreadId; stdcall;
    stop                    : procedure(self: PCefThread); stdcall;
    is_running              : function(self: PCefThread): integer; stdcall;
  end;

  // /include/capi/cef_waitable_event_capi.h (cef_waitable_event_t)
  TCefWaitableEvent = record
    base        : TCefBaseRefCounted;
    reset       : procedure(self: PCefWaitableEvent); stdcall;
    signal      : procedure(self: PCefWaitableEvent); stdcall;
    is_signaled : function(self: PCefWaitableEvent): integer; stdcall;
    wait        : procedure(self: PCefWaitableEvent); stdcall;
    timed_wait  : function(self: PCefWaitableEvent; max_ms: int64): integer; stdcall;
  end;

  // /include/capi/cef_task_capi.h (cef_task_runner_t)
  TCefTaskRunner = record
    base                      : TCefBaseRefCounted;
    is_same                   : function(self, that: PCefTaskRunner): Integer; stdcall;
    belongs_to_current_thread : function(self: PCefTaskRunner): Integer; stdcall;
    belongs_to_thread         : function(self: PCefTaskRunner; threadId: TCefThreadId): Integer; stdcall;
    post_task                 : function(self: PCefTaskRunner; task: PCefTask): Integer; stdcall;
    post_delayed_task         : function(self: PCefTaskRunner; task: PCefTask; delay_ms: Int64): Integer; stdcall;
  end;

  // /include/capi/cef_trace_capi.h (cef_end_tracing_callback_t)
  TCefEndTracingCallback = record
    base                    : TCefBaseRefCounted;
    on_end_tracing_complete : procedure(self: PCefEndTracingCallback; const tracing_file: PCefString); stdcall;
  end;

  // /include/capi/cef_resource_bundle_capi.h (cef_resource_bundle_t)
  TCefResourceBundle = record
    base                        : TCefBaseRefCounted;
    get_localized_string        : function(self: PCefResourceBundle; string_id: Integer): PCefStringUserFree; stdcall;
    get_data_resource           : function(self: PCefResourceBundle; resource_id: Integer): PCefBinaryValue; stdcall;
    get_data_resource_for_scale : function(self: PCefResourceBundle; resource_id: Integer; scale_factor: TCefScaleFactor): PCefBinaryValue; stdcall;
  end;

  // /include/capi/cef_menu_model_delegate_capi.h (cef_menu_model_delegate_t)
  TCefMenuModelDelegate = record
    base                    : TCefBaseRefCounted;
    execute_command         : procedure(self: PCefMenuModelDelegate; menu_model: PCefMenuModel; command_id: Integer; event_flags: TCefEventFlags); stdcall;
    mouse_outside_menu      : procedure(self: PCefMenuModelDelegate; menu_model: PCefMenuModel; const screen_point: PCefPoint); stdcall;
    unhandled_open_submenu  : procedure(self: PCefMenuModelDelegate; menu_model: PCefMenuModel; is_rtl: integer); stdcall;
    unhandled_close_submenu : procedure(self: PCefMenuModelDelegate; menu_model: PCefMenuModel; is_rtl: integer); stdcall;
    menu_will_show          : procedure(self: PCefMenuModelDelegate; menu_model: PCefMenuModel); stdcall;
    menu_closed             : procedure(self: PCefMenuModelDelegate; menu_model: PCefMenuModel); stdcall;
    format_label            : function(self: PCefMenuModelDelegate; menu_model: PCefMenuModel; label_ : PCefString) : integer; stdcall;
  end;

  // /include/capi/cef_process_message_capi.h (cef_process_message_t)
  TCefProcessMessage = record
    base              : TCefBaseRefCounted;
    is_valid          : function(self: PCefProcessMessage): Integer; stdcall;
    is_read_only      : function(self: PCefProcessMessage): Integer; stdcall;
    copy              : function(self: PCefProcessMessage): PCefProcessMessage; stdcall;
    get_name          : function(self: PCefProcessMessage): PCefStringUserFree; stdcall;
    get_argument_list : function(self: PCefProcessMessage): PCefListValue; stdcall;
  end;

  // /include/capi/cef_render_process_handler_capi.h (cef_render_process_handler_t)
  TCefRenderProcessHandler = record
    base                        : TCefBaseRefCounted;
    on_web_kit_initialized      : procedure(self: PCefRenderProcessHandler); stdcall;
    on_browser_created          : procedure(self: PCefRenderProcessHandler; browser: PCefBrowser; extra_info: PCefDictionaryValue); stdcall;
    on_browser_destroyed        : procedure(self: PCefRenderProcessHandler; browser: PCefBrowser); stdcall;
    get_load_handler            : function(self: PCefRenderProcessHandler): PCefLoadHandler; stdcall;
    on_context_created          : procedure(self: PCefRenderProcessHandler; browser: PCefBrowser; frame: PCefFrame; context: PCefv8Context); stdcall;
    on_context_released         : procedure(self: PCefRenderProcessHandler; browser: PCefBrowser; frame: PCefFrame; context: PCefv8Context); stdcall;
    on_uncaught_exception       : procedure(self: PCefRenderProcessHandler; browser: PCefBrowser; frame: PCefFrame; context: PCefv8Context; exception: PCefV8Exception; stackTrace: PCefV8StackTrace); stdcall;
    on_focused_node_changed     : procedure(self: PCefRenderProcessHandler; browser: PCefBrowser; frame: PCefFrame; node: PCefDomNode); stdcall;
    on_process_message_received : function(self: PCefRenderProcessHandler; browser: PCefBrowser; frame: PCefFrame; source_process: TCefProcessId; message_: PCefProcessMessage): Integer; stdcall;
  end;

  // /include/capi/cef_request_handler_capi.h (cef_request_handler_t)
  TCefRequestHandler = record
    base                                : TCefBaseRefCounted;
    on_before_browse                    : function(self: PCefRequestHandler; browser: PCefBrowser; frame: PCefFrame; request: PCefRequest; user_gesture, isRedirect: Integer): Integer; stdcall;
    on_open_urlfrom_tab                 : function(self: PCefRequestHandler; browser: PCefBrowser; frame: PCefFrame; const target_url: PCefString; target_disposition: TCefWindowOpenDisposition; user_gesture: Integer): Integer; stdcall;
    get_resource_request_handler        : function(self: PCefRequestHandler; browser: PCefBrowser; frame: PCefFrame; request: PCefRequest; is_navigation, is_download: Integer; const request_initiator: PCefString; disable_default_handling: PInteger): PCefResourceRequestHandler; stdcall;
    get_auth_credentials                : function(self: PCefRequestHandler; browser: PCefBrowser; const origin_url: PCefString; isProxy: Integer; const host: PCefString; port: Integer; const realm, scheme: PCefString; callback: PCefAuthCallback): Integer; stdcall;
    on_quota_request                    : function(self: PCefRequestHandler; browser: PCefBrowser; const origin_url: PCefString; new_size: Int64; callback: PCefRequestCallback): Integer; stdcall;
    on_certificate_error                : function(self: PCefRequestHandler; browser: PCefBrowser; cert_error: TCefErrorcode; const request_url: PCefString; ssl_info: PCefSslInfo; callback: PCefRequestCallback): Integer; stdcall;
    on_select_client_certificate        : function(self: PCefRequestHandler; browser: PCefBrowser; isProxy: integer; const host: PCefString; port: integer; certificatesCount: NativeUInt; const certificates: PPCefX509Certificate; callback: PCefSelectClientCertificateCallback): integer; stdcall;
    on_plugin_crashed                   : procedure(self: PCefRequestHandler; browser: PCefBrowser; const plugin_path: PCefString); stdcall;
    on_render_view_ready                : procedure(self: PCefRequestHandler; browser: PCefBrowser); stdcall;
    on_render_process_terminated        : procedure(self: PCefRequestHandler; browser: PCefBrowser; status: TCefTerminationStatus); stdcall;
    on_document_available_in_main_frame : procedure(self: PCefRequestHandler; browser: PCefBrowser); stdcall;
  end;

  // /include/capi/cef_request_callback_capi.h (cef_request_callback_t)
  TCefRequestCallback = record
    base   : TCefBaseRefCounted;
    cont   : procedure(self: PCefRequestCallback; allow: Integer); stdcall;
    cancel : procedure(self: PCefRequestCallback); stdcall;
  end;

  // /include/capi/cef_resource_handler_capi.h (cef_resource_skip_callback_t)
  TCefResourceSkipCallback = record
    base   : TCefBaseRefCounted;
    cont   : procedure(self: PCefResourceSkipCallback; bytes_skipped: int64); stdcall;
  end;

  // /include/capi/cef_resource_handler_capi.h (cef_resource_read_callback_t)
  TCefResourceReadCallback = record
    base   : TCefBaseRefCounted;
    cont   : procedure(self: PCefResourceReadCallback; bytes_read: int64); stdcall;
  end;

  // /include/capi/cef_resource_handler_capi.h (cef_resource_handler_t)
  TCefResourceHandler = record
    base                  : TCefBaseRefCounted;
    open                  : function(self: PCefResourceHandler; request: PCefRequest; handle_request: PInteger; callback: PCefCallback): Integer; stdcall;
    process_request       : function(self: PCefResourceHandler; request: PCefRequest; callback: PCefCallback): Integer; stdcall; // deprecated
    get_response_headers  : procedure(self: PCefResourceHandler; response: PCefResponse; response_length: PInt64; redirectUrl: PCefString); stdcall;
    skip                  : function(self: PCefResourceHandler; bytes_to_skip: int64; bytes_skipped: PInt64; callback: PCefResourceSkipCallback): Integer; stdcall;
    read                  : function(self: PCefResourceHandler; data_out: Pointer; bytes_to_read: Integer; bytes_read: PInteger; callback: PCefResourceReadCallback): Integer; stdcall;
    read_response         : function(self: PCefResourceHandler; data_out: Pointer; bytes_to_read: Integer; bytes_read: PInteger; callback: PCefCallback): Integer; stdcall; // deprecated
    cancel                : procedure(self: PCefResourceHandler); stdcall;
  end;

  // /include/capi/cef_resource_request_handler_capi.h (cef_resource_request_handler_t)
  TCefResourceRequestHandler = record
    base                          : TCefBaseRefCounted;
    get_cookie_access_filter      : function(self: PCefResourceRequestHandler; browser: PCefBrowser; frame: PCefFrame; request: PCefRequest): PCefCookieAccessFilter; stdcall;
    on_before_resource_load       : function(self: PCefResourceRequestHandler; browser: PCefBrowser; frame: PCefFrame; request: PCefRequest; callback: PCefRequestCallback): TCefReturnValue; stdcall;
    get_resource_handler          : function(self: PCefResourceRequestHandler; browser: PCefBrowser; frame: PCefFrame; request: PCefRequest): PCefResourceHandler; stdcall;
    on_resource_redirect          : procedure(self: PCefResourceRequestHandler; browser: PCefBrowser; frame: PCefFrame; request: PCefRequest; response: PCefResponse; new_url: PCefString); stdcall;
    on_resource_response          : function(self: PCefResourceRequestHandler; browser: PCefBrowser; frame: PCefFrame; request: PCefRequest; response: PCefResponse): Integer; stdcall;
    get_resource_response_filter  : function(self: PCefResourceRequestHandler; browser: PCefBrowser; frame: PCefFrame; request: PCefRequest; response: PCefResponse): PCefResponseFilter; stdcall;
    on_resource_load_complete     : procedure(self: PCefResourceRequestHandler; browser: PCefBrowser; frame: PCefFrame; request: PCefRequest; response: PCefResponse; status: TCefUrlRequestStatus; received_content_length: Int64); stdcall;
    on_protocol_execution         : procedure(self: PCefResourceRequestHandler; browser: PCefBrowser; frame: PCefFrame; request: PCefRequest; allow_os_execution: PInteger); stdcall;
  end;

  // /include/capi/cef_resource_request_handler_capi.h (cef_cookie_access_filter_t)
  TCefCookieAccessFilter = record
    base                  : TCefBaseRefCounted;
    can_send_cookie       : function(self: PCefCookieAccessFilter; browser: PCefBrowser; frame: PCefFrame; request: PCefRequest; const cookie: PCefCookie): Integer; stdcall;
    can_save_cookie       : function(self: PCefCookieAccessFilter; browser: PCefBrowser; frame: PCefFrame; request: PCefRequest; response: PCefResponse; const cookie: PCefCookie): Integer; stdcall;
  end;

  // /include/capi/cef_response_capi.h (cef_response_t)
  TCefResponse = record
    base                : TCefBaseRefCounted;
    is_read_only        : function(self: PCefResponse): Integer; stdcall;
    get_error           : function(self: PCefResponse): TCefErrorCode; stdcall;
    set_error           : procedure(self: PCefResponse; error: TCefErrorCode); stdcall;
    get_status          : function(self: PCefResponse): Integer; stdcall;
    set_status          : procedure(self: PCefResponse; status: Integer); stdcall;
    get_status_text     : function(self: PCefResponse): PCefStringUserFree; stdcall;
    set_status_text     : procedure(self: PCefResponse; const statusText: PCefString); stdcall;
    get_mime_type       : function(self: PCefResponse): PCefStringUserFree; stdcall;
    set_mime_type       : procedure(self: PCefResponse; const mimeType: PCefString); stdcall;
    get_charset         : function(self: PCefResponse): PCefStringUserFree; stdcall;
    set_charset         : procedure(self: PCefResponse; const charset: PCefString); stdcall;
    get_header_by_name  : function(self: PCefResponse; const name: PCefString): PCefStringUserFree; stdcall;
    set_header_by_name  : procedure(self: PCefResponse; const name: PCefString; const value: PCefString; overwrite: integer); stdcall;
    get_header_map      : procedure(self: PCefResponse; headerMap: TCefStringMultimap); stdcall;
    set_header_map      : procedure(self: PCefResponse; headerMap: TCefStringMultimap); stdcall;
    get_url             : function(self: PCefResponse): PCefStringUserFree; stdcall;
    set_url             : procedure(self: PCefResponse; const url: PCefString); stdcall;
  end;

  // /include/capi/cef_response_filter_capi.h (cef_response_filter_t)
  TCefResponseFilter = record
    base        : TCefBaseRefCounted;
    init_filter : function(self: PCefResponseFilter): Integer; stdcall;
    filter      : function(self: PCefResponseFilter; data_in: Pointer; data_in_size: NativeUInt; var data_in_read: NativeUInt; data_out: Pointer; data_out_size : NativeUInt; var data_out_written: NativeUInt): TCefResponseFilterStatus; stdcall;
  end;

  // /include/capi/cef_auth_callback_capi.h (cef_auth_callback_t)
  TCefAuthCallback = record
    base   : TCefBaseRefCounted;
    cont   : procedure(self: PCefAuthCallback; const username, password: PCefString); stdcall;
    cancel : procedure(self: PCefAuthCallback); stdcall;
  end;

  // /include/capi/cef_callback_capi.h (cef_callback_t)
  TCefCallback = record
    base   : TCefBaseRefCounted;
    cont   : procedure(self: PCefCallback); stdcall;
    cancel : procedure(self: PCefCallback); stdcall;
  end;

  // /include/capi/cef_request_context_capi.h (cef_request_context_t)
  TCefRequestContext = record
    base                            : TCefBaseRefCounted;
    is_same                         : function(self, other: PCefRequestContext): Integer; stdcall;
    is_sharing_with                 : function(self, other: PCefRequestContext): Integer; stdcall;
    is_global                       : function(self: PCefRequestContext): Integer; stdcall;
    get_handler                     : function(self: PCefRequestContext): PCefRequestContextHandler; stdcall;
    get_cache_path                  : function(self: PCefRequestContext): PCefStringUserFree; stdcall;
    get_cookie_manager              : function(self: PCefRequestContext; callback: PCefCompletionCallback): PCefCookieManager; stdcall;
    register_scheme_handler_factory : function(self: PCefRequestContext; const scheme_name, domain_name: PCefString; factory: PCefSchemeHandlerFactory): Integer; stdcall;
    clear_scheme_handler_factories  : function(self: PCefRequestContext): Integer; stdcall;
    purge_plugin_list_cache         : procedure(self: PCefRequestContext; reload_pages: Integer); stdcall;
    has_preference                  : function(self: PCefRequestContext; const name: PCefString): Integer; stdcall;
    get_preference                  : function(self: PCefRequestContext; const name: PCefString): PCefValue; stdcall;
    get_all_preferences             : function(self: PCefRequestContext; include_defaults: Integer): PCefDictionaryValue; stdcall;
    can_set_preference              : function(self: PCefRequestContext; const name: PCefString): Integer; stdcall;
    set_preference                  : function(self: PCefRequestContext; const name: PCefString; value: PCefValue; error: PCefString): Integer; stdcall;
    clear_certificate_exceptions    : procedure(self: PCefRequestContext; callback: PCefCompletionCallback); stdcall;
    clear_http_auth_credentials     : procedure(self: PCefRequestContext; callback: PCefCompletionCallback); stdcall;
    close_all_connections           : procedure(self: PCefRequestContext; callback: PCefCompletionCallback); stdcall;
    resolve_host                    : procedure(self: PCefRequestContext; const origin: PCefString; callback: PCefResolveCallback); stdcall;
    load_extension                  : procedure(self: PCefRequestContext; const root_directory: PCefString; manifest: PCefDictionaryValue; handler: PCefExtensionHandler); stdcall;
    did_load_extension              : function(self: PCefRequestContext; const extension_id: PCefString): Integer; stdcall;
    has_extension                   : function(self: PCefRequestContext; const extension_id: PCefString): Integer; stdcall;
    get_extensions                  : function(self: PCefRequestContext; extension_ids: TCefStringList): Integer; stdcall;
    get_extension                   : function(self: PCefRequestContext; const extension_id: PCefString): PCefExtension; stdcall;
    get_media_router                : function(self: PCefRequestContext): PCefMediaRouter; stdcall;
  end;

  // /include/capi/cef_request_context_handler_capi.h (cef_request_context_handler_t)
  TCefRequestContextHandler = record
    base                            : TCefBaseRefCounted;
    on_request_context_initialized  : procedure(self: PCefRequestContextHandler; request_context: PCefRequestContext); stdcall;
    on_before_plugin_load           : function(self: PCefRequestContextHandler; const mime_type, plugin_url : PCefString; is_main_frame : integer; const top_origin_url: PCefString; plugin_info: PCefWebPluginInfo; plugin_policy: PCefPluginPolicy): Integer; stdcall;
    get_resource_request_handler    : function(self: PCefRequestContextHandler; browser: PCefBrowser; frame: PCefFrame; request: PCefRequest; is_navigation, is_download: Integer; const request_initiator: PCefString; disable_default_handling: PInteger): PCefResourceRequestHandler; stdcall;
  end;

  // /include/capi/cef_callback_capi.h (cef_completion_callback_t)
  TCefCompletionCallback = record
    base        : TCefBaseRefCounted;
    on_complete : procedure(self: PCefCompletionCallback); stdcall;
  end;

  // /include/capi/cef_cookie_capi.h (cef_cookie_manager_t)
  TCefCookieManager = record
    base                  : TCefBaseRefCounted;
    set_supported_schemes : procedure(self: PCefCookieManager; schemes: TCefStringList; include_defaults: Integer; callback: PCefCompletionCallback); stdcall;
    visit_all_cookies     : function(self: PCefCookieManager; visitor: PCefCookieVisitor): Integer; stdcall;
    visit_url_cookies     : function(self: PCefCookieManager; const url: PCefString; includeHttpOnly: Integer; visitor: PCefCookieVisitor): Integer; stdcall;
    set_cookie            : function(self: PCefCookieManager; const url: PCefString; const cookie: PCefCookie; callback: PCefSetCookieCallback): Integer; stdcall;
    delete_cookies        : function(self: PCefCookieManager; const url, cookie_name: PCefString; callback: PCefDeleteCookiesCallback): Integer; stdcall;
    flush_store           : function(self: PCefCookieManager; callback: PCefCompletionCallback): Integer; stdcall;
  end;

  // /include/capi/cef_scheme_capi.h (cef_scheme_handler_factory_t)
  TCefSchemeHandlerFactory = record
    base   : TCefBaseRefCounted;
    create : function(self: PCefSchemeHandlerFactory; browser: PCefBrowser; frame: PCefFrame; const scheme_name: PCefString; request: PCefRequest): PCefResourceHandler; stdcall;
  end;

  // /include/capi/cef_request_context_capi.h (cef_resolve_callback_t)
  TCefResolveCallback = record
    base                 : TCefBaseRefCounted;
    on_resolve_completed : procedure(self: PCefResolveCallback; result: TCefErrorCode; resolved_ips: TCefStringList); stdcall;
  end;

  // /include/capi/cef_web_plugin_capi.h (cef_web_plugin_info_t)
  TCefWebPluginInfo = record
    base            : TCefBaseRefCounted;
    get_name        : function(self: PCefWebPluginInfo): PCefStringUserFree; stdcall;
    get_path        : function(self: PCefWebPluginInfo): PCefStringUserFree; stdcall;
    get_version     : function(self: PCefWebPluginInfo): PCefStringUserFree; stdcall;
    get_description : function(self: PCefWebPluginInfo): PCefStringUserFree; stdcall;
  end;

  // /include/capi/cef_cookie_capi.h (cef_cookie_visitor_t)
  TCefCookieVisitor = record
    base  : TCefBaseRefCounted;
    visit : function(self: PCefCookieVisitor; const cookie: PCefCookie; count, total: Integer; deleteCookie: PInteger): Integer; stdcall;
  end;

  // /include/capi/cef_cookie_capi.h (cef_set_cookie_callback_t)
  TCefSetCookieCallback = record
    base        : TCefBaseRefCounted;
    on_complete : procedure(self: PCefSetCookieCallback; success: Integer); stdcall;
  end;

  // /include/capi/cef_cookie_capi.h (cef_delete_cookies_callback_t)
  TCefDeleteCookiesCallback = record
    base        : TCefBaseRefCounted;
    on_complete : procedure(self: PCefDeleteCookiesCallback; num_deleted: Integer); stdcall;
  end;

  // /include/capi/cef_browser_capi.h (cef_run_file_dialog_callback_t)
  TCefRunFileDialogCallback = record
    base                     : TCefBaseRefCounted;
    on_file_dialog_dismissed : procedure(self: PCefRunFileDialogCallback; selected_accept_filter: Integer; file_paths: TCefStringList); stdcall;
  end;

  // /include/capi/cef_browser_capi.h (cef_download_image_callback_t)
  TCefDownloadImageCallback = record
    base                       : TCefBaseRefCounted;
    on_download_image_finished : procedure(self: PCefDownloadImageCallback; const image_url: PCefString; http_status_code: Integer; image: PCefImage); stdcall;
  end;

  // /include/capi/cef_image_capi.h (cef_image_t)
  TCefImage = record
    base                    : TCefBaseRefCounted;
    is_empty                : function(self: PCefImage): Integer; stdcall;
    is_same                 : function(self, that: PCefImage): Integer; stdcall;
    add_bitmap              : function(self: PCefImage; scale_factor: Single; pixel_width, pixel_height: Integer; color_type: TCefColorType; alpha_type: TCefAlphaType; const pixel_data: Pointer; pixel_data_size: NativeUInt): Integer; stdcall;
    add_png                 : function(self: PCefImage; scale_factor: Single; const png_data: Pointer; png_data_size: NativeUInt): Integer; stdcall;
    add_jpeg                : function(self: PCefImage; scale_factor: Single; const jpeg_data: Pointer; jpeg_data_size: NativeUInt): Integer; stdcall;
    get_width               : function(self: PCefImage): NativeUInt; stdcall;
    get_height              : function(self: PCefImage): NativeUInt; stdcall;
    has_representation      : function(self: PCefImage; scale_factor: Single): Integer; stdcall;
    remove_representation   : function(self: PCefImage; scale_factor: Single): Integer; stdcall;
    get_representation_info : function(self: PCefImage; scale_factor: Single; actual_scale_factor: PSingle; pixel_width, pixel_height: PInteger): Integer; stdcall;
    get_as_bitmap           : function(self: PCefImage; scale_factor: Single; color_type: TCefColorType; alpha_type: TCefAlphaType; pixel_width, pixel_height: PInteger): PCefBinaryValue; stdcall;
    get_as_png              : function(self: PCefImage; scale_factor: Single; with_transparency: Integer; pixel_width, pixel_height: PInteger): PCefBinaryValue; stdcall;
    get_as_jpeg             : function(self: PCefImage; scale_factor: Single; quality: Integer; pixel_width, pixel_height: PInteger): PCefBinaryValue; stdcall;
  end;

  // /include/capi/cef_browser_capi.h (cef_pdf_print_callback_t)
  TCefPdfPrintCallback = record
    base                  : TCefBaseRefCounted;
    on_pdf_print_finished : procedure(self: PCefPdfPrintCallback; const path: PCefString; ok: Integer); stdcall;
  end;

  // /include/capi/cef_browser_capi.h (cef_navigation_entry_visitor_t)
  TCefNavigationEntryVisitor = record
    base  : TCefBaseRefCounted;
    visit : function(self: PCefNavigationEntryVisitor; entry: PCefNavigationEntry; current, index, total: Integer): Integer; stdcall;
  end;

  // /include/capi/cef_navigation_entry_capi.h (cef_navigation_entry_t)
  TCefNavigationEntry = record
    base                  : TCefBaseRefCounted;
    is_valid              : function(self: PCefNavigationEntry): Integer; stdcall;
    get_url               : function(self: PCefNavigationEntry): PCefStringUserFree; stdcall;
    get_display_url       : function(self: PCefNavigationEntry): PCefStringUserFree; stdcall;
    get_original_url      : function(self: PCefNavigationEntry): PCefStringUserFree; stdcall;
    get_title             : function(self: PCefNavigationEntry): PCefStringUserFree; stdcall;
    get_transition_type   : function(self: PCefNavigationEntry): TCefTransitionType; stdcall;
    has_post_data         : function(self: PCefNavigationEntry): Integer; stdcall;
    get_completion_time   : function(self: PCefNavigationEntry): TCefTime; stdcall;
    get_http_status_code  : function(self: PCefNavigationEntry): Integer; stdcall;
    get_sslstatus         : function(self: PCefNavigationEntry): PCefSSLStatus; stdcall;
  end;

  // /include/capi/cef_print_settings_capi.h (cef_print_settings_t)
  TCefPrintSettings = record
    base                        : TCefBaseRefCounted;
    is_valid                    : function(self: PCefPrintSettings): Integer; stdcall;
    is_read_only                : function(self: PCefPrintSettings): Integer; stdcall;
    set_orientation             : procedure(self: PCefPrintSettings; landscape: Integer); stdcall;
    is_landscape                : function(self: PCefPrintSettings): Integer; stdcall;
    set_printer_printable_area  : procedure(self: PCefPrintSettings; const physical_size_device_units: PCefSize; const printable_area_device_units: PCefRect; landscape_needs_flip: Integer); stdcall;
    set_device_name             : procedure(self: PCefPrintSettings; const name: PCefString); stdcall;
    get_device_name             : function(self: PCefPrintSettings): PCefStringUserFree; stdcall;
    set_dpi                     : procedure(self: PCefPrintSettings; dpi: Integer); stdcall;
    get_dpi                     : function(self: PCefPrintSettings): Integer; stdcall;
    set_page_ranges             : procedure(self: PCefPrintSettings; rangesCount: NativeUInt; ranges: PCefRange); stdcall;
    get_page_ranges_count       : function(self: PCefPrintSettings): NativeUInt; stdcall;
    get_page_ranges             : procedure(self: PCefPrintSettings; rangesCount: PNativeUInt; ranges: PCefRange); stdcall;
    set_selection_only          : procedure(self: PCefPrintSettings; selection_only: Integer); stdcall;
    is_selection_only           : function(self: PCefPrintSettings): Integer; stdcall;
    set_collate                 : procedure(self: PCefPrintSettings; collate: Integer); stdcall;
    will_collate                : function(self: PCefPrintSettings): Integer; stdcall;
    set_color_model             : procedure(self: PCefPrintSettings; model: TCefColorModel); stdcall;
    get_color_model             : function(self: PCefPrintSettings): TCefColorModel; stdcall;
    set_copies                  : procedure(self: PCefPrintSettings; copies: Integer); stdcall;
    get_copies                  : function(self: PCefPrintSettings): Integer; stdcall;
    set_duplex_mode             : procedure(self: PCefPrintSettings; mode: TCefDuplexMode); stdcall;
    get_duplex_mode             : function(self: PCefPrintSettings): TCefDuplexMode; stdcall;
  end;

  // /include/capi/cef_print_handler_capi.h (cef_print_dialog_callback_t)
  TCefPrintDialogCallback = record
    base   : TCefBaseRefCounted;
    cont   : procedure(self: PCefPrintDialogCallback; settings: PCefPrintSettings); stdcall;
    cancel : procedure(self: PCefPrintDialogCallback); stdcall;
  end;

  // /include/capi/cef_print_handler_capi.h (cef_print_job_callback_t)
  TCefPrintJobCallback = record
    base : TCefBaseRefCounted;
    cont : procedure(self: PCefPrintJobCallback); stdcall;
  end;

  // /include/capi/cef_print_handler_capi.h (cef_print_handler_t)
  TCefPrintHandler = record
    base                : TCefBaseRefCounted;
    on_print_start      : procedure(self: PCefPrintHandler; browser: PCefBrowser); stdcall;
    on_print_settings   : procedure(self: PCefPrintHandler; browser: PCefBrowser; settings: PCefPrintSettings; get_defaults: Integer); stdcall;
    on_print_dialog     : function(self: PCefPrintHandler; browser: PCefBrowser; has_selection: Integer; callback: PCefPrintDialogCallback): Integer; stdcall;
    on_print_job        : function(self: PCefPrintHandler; browser: PCefBrowser; const document_name, pdf_file_path: PCefString; callback: PCefPrintJobCallback): Integer; stdcall;
    on_print_reset      : procedure(self: PCefPrintHandler; browser: PCefBrowser); stdcall;
    get_pdf_paper_size  : function(self: PCefPrintHandler; device_units_per_inch: Integer): TCefSize; stdcall;
  end;

  // /include/capi/cef_drag_data_capi.h (cef_drag_data_t)
  TCefDragData = record
    base                  : TCefBaseRefCounted;
    clone                 : function(self: PCefDragData): PCefDragData; stdcall;
    is_read_only          : function(self: PCefDragData): Integer; stdcall;
    is_link               : function(self: PCefDragData): Integer; stdcall;
    is_fragment           : function(self: PCefDragData): Integer; stdcall;
    is_file               : function(self: PCefDragData): Integer; stdcall;
    get_link_url          : function(self: PCefDragData): PCefStringUserFree; stdcall;
    get_link_title        : function(self: PCefDragData): PCefStringUserFree; stdcall;
    get_link_metadata     : function(self: PCefDragData): PCefStringUserFree; stdcall;
    get_fragment_text     : function(self: PCefDragData): PCefStringUserFree; stdcall;
    get_fragment_html     : function(self: PCefDragData): PCefStringUserFree; stdcall;
    get_fragment_base_url : function(self: PCefDragData): PCefStringUserFree; stdcall;
    get_file_name         : function(self: PCefDragData): PCefStringUserFree; stdcall;
    get_file_contents     : function(self: PCefDragData; writer: PCefStreamWriter): NativeUInt; stdcall;
    get_file_names        : function(self: PCefDragData; names: TCefStringList): Integer; stdcall;
    set_link_url          : procedure(self: PCefDragData; const url: PCefString); stdcall;
    set_link_title        : procedure(self: PCefDragData; const title: PCefString); stdcall;
    set_link_metadata     : procedure(self: PCefDragData; const data: PCefString); stdcall;
    set_fragment_text     : procedure(self: PCefDragData; const text: PCefString); stdcall;
    set_fragment_html     : procedure(self: PCefDragData; const html: PCefString); stdcall;
    set_fragment_base_url : procedure(self: PCefDragData; const base_url: PCefString); stdcall;
    reset_file_contents   : procedure(self: PCefDragData); stdcall;
    add_file              : procedure(self: PCefDragData; const path, display_name: PCefString); stdcall;
    get_image             : function(self: PCefDragData): PCefImage; stdcall;
    get_image_hotspot     : function(self: PCefDragData): PCefPoint; stdcall;
    has_image             : function(self: PCefDragData): Integer; stdcall;
  end;

  // /include/capi/cef_command_line_capi.h (cef_command_line_t)
  TCefCommandLine = record
    base                      : TCefBaseRefCounted;
    is_valid                  : function(self: PCefCommandLine): Integer; stdcall;
    is_read_only              : function(self: PCefCommandLine): Integer; stdcall;
    copy                      : function(self: PCefCommandLine): PCefCommandLine; stdcall;
    init_from_argv            : procedure(self: PCefCommandLine; argc: Integer; const argv: PPAnsiChar); stdcall;
    init_from_string          : procedure(self: PCefCommandLine; const command_line: PCefString); stdcall;
    reset                     : procedure(self: PCefCommandLine); stdcall;
    get_argv                  : procedure(self: PCefCommandLine; argv: TCefStringList); stdcall;
    get_command_line_string   : function(self: PCefCommandLine): PCefStringUserFree; stdcall;
    get_program               : function(self: PCefCommandLine): PCefStringUserFree; stdcall;
    set_program               : procedure(self: PCefCommandLine; const program_: PCefString); stdcall;
    has_switches              : function(self: PCefCommandLine): Integer; stdcall;
    has_switch                : function(self: PCefCommandLine; const name: PCefString): Integer; stdcall;
    get_switch_value          : function(self: PCefCommandLine; const name: PCefString): PCefStringUserFree; stdcall;
    get_switches              : procedure(self: PCefCommandLine; switches: TCefStringMap); stdcall;
    append_switch             : procedure(self: PCefCommandLine; const name: PCefString); stdcall;
    append_switch_with_value  : procedure(self: PCefCommandLine; const name, value: PCefString); stdcall;
    has_arguments             : function(self: PCefCommandLine): Integer; stdcall;
    get_arguments             : procedure(self: PCefCommandLine; arguments: TCefStringList); stdcall;
    append_argument           : procedure(self: PCefCommandLine; const argument: PCefString); stdcall;
    prepend_wrapper           : procedure(self: PCefCommandLine; const wrapper: PCefString); stdcall;
  end;

  // /include/capi/cef_scheme_capi.h (cef_scheme_registrar_t)
  TCefSchemeRegistrar = record
    base              : TCefBaseScoped;
    add_custom_scheme : function(self: PCefSchemeRegistrar; const scheme_name: PCefString; options : TCefSchemeOptions): Integer; stdcall;
  end;

  // /include/capi/cef_values_capi.h (cef_binary_value_t)
  TCefBinaryValue = record
    base      : TCefBaseRefCounted;
    is_valid  : function(self: PCefBinaryValue): Integer; stdcall;
    is_owned  : function(self: PCefBinaryValue): Integer; stdcall;
    is_same   : function(self, that: PCefBinaryValue):Integer; stdcall;
    is_equal  : function(self, that: PCefBinaryValue): Integer; stdcall;
    copy      : function(self: PCefBinaryValue): PCefBinaryValue; stdcall;
    get_size  : function(self: PCefBinaryValue): NativeUInt; stdcall;
    get_data  : function(self: PCefBinaryValue; buffer: Pointer; buffer_size, data_offset: NativeUInt): NativeUInt; stdcall;
  end;

  // /include/capi/cef_values_capi.h (cef_value_t)
  TCefValue = record
    base            : TCefBaseRefCounted;
    is_valid        : function(self: PCefValue): Integer; stdcall;
    is_owned        : function(self: PCefValue): Integer; stdcall;
    is_read_only    : function(self: PCefValue): Integer; stdcall;
    is_same         : function(self, that: PCefValue): Integer; stdcall;
    is_equal        : function(self, that: PCefValue): Integer; stdcall;
    copy            : function(self: PCefValue): PCefValue; stdcall;
    get_type        : function(self: PCefValue): TCefValueType; stdcall;
    get_bool        : function(self: PCefValue): Integer; stdcall;
    get_int         : function(self: PCefValue): Integer; stdcall;
    get_double      : function(self: PCefValue): Double; stdcall;
    get_string      : function(self: PCefValue): PCefStringUserFree; stdcall;
    get_binary      : function(self: PCefValue): PCefBinaryValue; stdcall;
    get_dictionary  : function(self: PCefValue): PCefDictionaryValue; stdcall;
    get_list        : function(self: PCefValue): PCefListValue; stdcall;
    set_null        : function(self: PCefValue): Integer; stdcall;
    set_bool        : function(self: PCefValue; value: Integer): Integer; stdcall;
    set_int         : function(self: PCefValue; value: Integer): Integer; stdcall;
    set_double      : function(self: PCefValue; value: Double): Integer; stdcall;
    set_string      : function(self: PCefValue; const value: PCefString): Integer; stdcall;
    set_binary      : function(self: PCefValue; value: PCefBinaryValue): Integer; stdcall;
    set_dictionary  : function(self: PCefValue; value: PCefDictionaryValue): Integer; stdcall;
    set_list        : function(self: PCefValue; value: PCefListValue): Integer; stdcall;
  end;

  // /include/capi/cef_values_capi.h (cef_dictionary_value_t)
  TCefDictionaryValue = record
    base            : TCefBaseRefCounted;
    is_valid        : function(self: PCefDictionaryValue): Integer; stdcall;
    is_owned        : function(self: PCefDictionaryValue): Integer; stdcall;
    is_read_only    : function(self: PCefDictionaryValue): Integer; stdcall;
    is_same         : function(self, that: PCefDictionaryValue): Integer; stdcall;
    is_equal        : function(self, that: PCefDictionaryValue): Integer; stdcall;
    copy            : function(self: PCefDictionaryValue; exclude_empty_children: Integer): PCefDictionaryValue; stdcall;
    get_size        : function(self: PCefDictionaryValue): NativeUInt; stdcall;
    clear           : function(self: PCefDictionaryValue): Integer; stdcall;
    has_key         : function(self: PCefDictionaryValue; const key: PCefString): Integer; stdcall;
    get_keys        : function(self: PCefDictionaryValue; const keys: TCefStringList): Integer; stdcall;
    remove          : function(self: PCefDictionaryValue; const key: PCefString): Integer; stdcall;
    get_type        : function(self: PCefDictionaryValue; const key: PCefString): TCefValueType; stdcall;
    get_value       : function(self: PCefDictionaryValue; const key: PCefString): PCefValue; stdcall;
    get_bool        : function(self: PCefDictionaryValue; const key: PCefString): Integer; stdcall;
    get_int         : function(self: PCefDictionaryValue; const key: PCefString): Integer; stdcall;
    get_double      : function(self: PCefDictionaryValue; const key: PCefString): Double; stdcall;
    get_string      : function(self: PCefDictionaryValue; const key: PCefString): PCefStringUserFree; stdcall;
    get_binary      : function(self: PCefDictionaryValue; const key: PCefString): PCefBinaryValue; stdcall;
    get_dictionary  : function(self: PCefDictionaryValue; const key: PCefString): PCefDictionaryValue; stdcall;
    get_list        : function(self: PCefDictionaryValue; const key: PCefString): PCefListValue; stdcall;
    set_value       : function(self: PCefDictionaryValue; const key: PCefString; value: PCefValue): Integer; stdcall;
    set_null        : function(self: PCefDictionaryValue; const key: PCefString): Integer; stdcall;
    set_bool        : function(self: PCefDictionaryValue; const key: PCefString; value: Integer): Integer; stdcall;
    set_int         : function(self: PCefDictionaryValue; const key: PCefString; value: Integer): Integer; stdcall;
    set_double      : function(self: PCefDictionaryValue; const key: PCefString; value: Double): Integer; stdcall;
    set_string      : function(self: PCefDictionaryValue; const key: PCefString; value: PCefString): Integer; stdcall;
    set_binary      : function(self: PCefDictionaryValue; const key: PCefString; value: PCefBinaryValue): Integer; stdcall;
    set_dictionary  : function(self: PCefDictionaryValue; const key: PCefString; value: PCefDictionaryValue): Integer; stdcall;
    set_list        : function(self: PCefDictionaryValue; const key: PCefString; value: PCefListValue): Integer; stdcall;
  end;

  // /include/capi/cef_values_capi.h (cef_list_value_t)
  TCefListValue = record
    base            : TCefBaseRefCounted;
    is_valid        : function(self: PCefListValue): Integer; stdcall;
    is_owned        : function(self: PCefListValue): Integer; stdcall;
    is_read_only    : function(self: PCefListValue): Integer; stdcall;
    is_same         : function(self, that: PCefListValue): Integer; stdcall;
    is_equal        : function(self, that: PCefListValue): Integer; stdcall;
    copy            : function(self: PCefListValue): PCefListValue; stdcall;
    set_size        : function(self: PCefListValue; size: NativeUInt): Integer; stdcall;
    get_size        : function(self: PCefListValue): NativeUInt; stdcall;
    clear           : function(self: PCefListValue): Integer; stdcall;
    remove          : function(self: PCefListValue; index: NativeUInt): Integer; stdcall;
    get_type        : function(self: PCefListValue; index: NativeUInt): TCefValueType; stdcall;
    get_value       : function(self: PCefListValue; index: NativeUInt): PCefValue; stdcall;
    get_bool        : function(self: PCefListValue; index: NativeUInt): Integer; stdcall;
    get_int         : function(self: PCefListValue; index: NativeUInt): Integer; stdcall;
    get_double      : function(self: PCefListValue; index: NativeUInt): Double; stdcall;
    get_string      : function(self: PCefListValue; index: NativeUInt): PCefStringUserFree; stdcall;
    get_binary      : function(self: PCefListValue; index: NativeUInt): PCefBinaryValue; stdcall;
    get_dictionary  : function(self: PCefListValue; index: NativeUInt): PCefDictionaryValue; stdcall;
    get_list        : function(self: PCefListValue; index: NativeUInt): PCefListValue; stdcall;
    set_value       : function(self: PCefListValue; index: NativeUInt; value: PCefValue): Integer; stdcall;
    set_null        : function(self: PCefListValue; index: NativeUInt): Integer; stdcall;
    set_bool        : function(self: PCefListValue; index: NativeUInt; value: Integer): Integer; stdcall;
    set_int         : function(self: PCefListValue; index: NativeUInt; value: Integer): Integer; stdcall;
    set_double      : function(self: PCefListValue; index: NativeUInt; value: Double): Integer; stdcall;
    set_string      : function(self: PCefListValue; index: NativeUInt; value: PCefString): Integer; stdcall;
    set_binary      : function(self: PCefListValue; index: NativeUInt; value: PCefBinaryValue): Integer; stdcall;
    set_dictionary  : function(self: PCefListValue; index: NativeUInt; value: PCefDictionaryValue): Integer; stdcall;
    set_list        : function(self: PCefListValue; index: NativeUInt; value: PCefListValue): Integer; stdcall;
  end;

  // /include/capi/cef_string_visitor_capi.h (cef_string_visitor_t)
  TCefStringVisitor = record
    base  : TCefBaseRefCounted;
    visit : procedure(self: PCefStringVisitor; const str: PCefString); stdcall;
  end;

  // /include/capi/cef_request_capi.h (cef_post_data_element_t)
  TCefPostDataElement = record
    base            : TCefBaseRefCounted;
    is_read_only    : function(self: PCefPostDataElement): Integer; stdcall;
    set_to_empty    : procedure(self: PCefPostDataElement); stdcall;
    set_to_file     : procedure(self: PCefPostDataElement; const fileName: PCefString); stdcall;
    set_to_bytes    : procedure(self: PCefPostDataElement; size: NativeUInt; const bytes: Pointer); stdcall;
    get_type        : function(self: PCefPostDataElement): TCefPostDataElementType; stdcall;
    get_file        : function(self: PCefPostDataElement): PCefStringUserFree; stdcall;
    get_bytes_count : function(self: PCefPostDataElement): NativeUInt; stdcall;
    get_bytes       : function(self: PCefPostDataElement; size: NativeUInt; bytes: Pointer): NativeUInt; stdcall;
  end;

  // /include/capi/cef_request_capi.h (cef_post_data_t)
  TCefPostData = record
    base                  : TCefBaseRefCounted;
    is_read_only          : function(self: PCefPostData): Integer; stdcall;
    has_excluded_elements : function(self: PCefPostData): Integer; stdcall;
    get_element_count     : function(self: PCefPostData): NativeUInt; stdcall;
    get_elements          : procedure(self: PCefPostData; var elementsCount: NativeUInt; var elements: PCefPostDataElement); stdcall;
    remove_element        : function(self: PCefPostData; element: PCefPostDataElement): Integer; stdcall;
    add_element           : function(self: PCefPostData; element: PCefPostDataElement): Integer; stdcall;
    remove_elements       : procedure(self: PCefPostData); stdcall;
  end;

  // /include/capi/cef_request_capi.h (cef_request_t)
  TCefRequest = record
    base                        : TCefBaseRefCounted;
    is_read_only                : function(self: PCefRequest): Integer; stdcall;
    get_url                     : function(self: PCefRequest): PCefStringUserFree; stdcall;
    set_url                     : procedure(self: PCefRequest; const url: PCefString); stdcall;
    get_method                  : function(self: PCefRequest): PCefStringUserFree; stdcall;
    set_method                  : procedure(self: PCefRequest; const method: PCefString); stdcall;
    set_referrer                : procedure(self: PCefRequest; const referrer_url: PCefString; policy: TCefReferrerPolicy); stdcall;
    get_referrer_url            : function(self: PCefRequest): PCefStringUserFree; stdcall;
    get_referrer_policy         : function(self: PCefRequest): TCefReferrerPolicy; stdcall;
    get_post_data               : function(self: PCefRequest): PCefPostData; stdcall;
    set_post_data               : procedure(self: PCefRequest; postData: PCefPostData); stdcall;
    get_header_map              : procedure(self: PCefRequest; headerMap: TCefStringMultimap); stdcall;
    set_header_map              : procedure(self: PCefRequest; headerMap: TCefStringMultimap); stdcall;
    get_header_by_name          : function(self: PCefRequest; const name: PCefString): PCefStringUserFree; stdcall;
    set_header_by_name          : procedure(self: PCefRequest; const name, value: PCefString; overwrite: integer); stdcall;
    set_                        : procedure(self: PCefRequest; const url, method: PCefString; postData: PCefPostData; headerMap: TCefStringMultimap); stdcall;
    get_flags                   : function(self: PCefRequest): Integer; stdcall;
    set_flags                   : procedure(self: PCefRequest; flags: Integer); stdcall;
    get_first_party_for_cookies : function(self: PCefRequest): PCefStringUserFree; stdcall;
    set_first_party_for_cookies : procedure(self: PCefRequest; const url: PCefString); stdcall;
    get_resource_type           : function(self: PCefRequest): TCefResourceType; stdcall;
    get_transition_type         : function(self: PCefRequest): TCefTransitionType; stdcall;
    get_identifier              : function(self: PCefRequest): UInt64; stdcall;
  end;

  // /include/capi/cef_task_capi.h (cef_task_t)
  TCefTask = record
    base    : TCefBaseRefCounted;
    execute : procedure(self: PCefTask); stdcall;
  end;

  // /include/capi/cef_dom_capi.h (cef_domvisitor_t)
  TCefDomVisitor = record
    base  : TCefBaseRefCounted;
    visit : procedure(self: PCefDomVisitor; document: PCefDomDocument); stdcall;
  end;

  // /include/capi/cef_menu_model_capi.h (cef_menu_model_t)
  TCefMenuModel = record
    base                  : TCefBaseRefCounted;
    is_sub_menu           : function(self: PCefMenuModel): Integer; stdcall;
    clear                 : function(self: PCefMenuModel): Integer; stdcall;
    get_count             : function(self: PCefMenuModel): Integer; stdcall;
    add_separator         : function(self: PCefMenuModel): Integer; stdcall;
    add_item              : function(self: PCefMenuModel; command_id: Integer; const text: PCefString): Integer; stdcall;
    add_check_item        : function(self: PCefMenuModel; command_id: Integer; const text: PCefString): Integer; stdcall;
    add_radio_item        : function(self: PCefMenuModel; command_id: Integer; const text: PCefString; group_id: Integer): Integer; stdcall;
    add_sub_menu          : function(self: PCefMenuModel; command_id: Integer; const text: PCefString): PCefMenuModel; stdcall;
    insert_separator_at   : function(self: PCefMenuModel; index: Integer): Integer; stdcall;
    insert_item_at        : function(self: PCefMenuModel; index, command_id: Integer; const text: PCefString): Integer; stdcall;
    insert_check_item_at  : function(self: PCefMenuModel; index, command_id: Integer; const text: PCefString): Integer; stdcall;
    insert_radio_item_at  : function(self: PCefMenuModel; index, command_id: Integer; const text: PCefString; group_id: Integer): Integer; stdcall;
    insert_sub_menu_at    : function(self: PCefMenuModel; index, command_id: Integer; const text: PCefString): PCefMenuModel; stdcall;
    remove                : function(self: PCefMenuModel; command_id: Integer): Integer; stdcall;
    remove_at             : function(self: PCefMenuModel; index: Integer): Integer; stdcall;
    get_index_of          : function(self: PCefMenuModel; command_id: Integer): Integer; stdcall;
    get_command_id_at     : function(self: PCefMenuModel; index: Integer): Integer; stdcall;
    set_command_id_at     : function(self: PCefMenuModel; index, command_id: Integer): Integer; stdcall;
    get_label             : function(self: PCefMenuModel; command_id: Integer): PCefStringUserFree; stdcall;
    get_label_at          : function(self: PCefMenuModel; index: Integer): PCefStringUserFree; stdcall;
    set_label             : function(self: PCefMenuModel; command_id: Integer; const text: PCefString): Integer; stdcall;
    set_label_at          : function(self: PCefMenuModel; index: Integer; const text: PCefString): Integer; stdcall;
    get_type              : function(self: PCefMenuModel; command_id: Integer): TCefMenuItemType; stdcall;
    get_type_at           : function(self: PCefMenuModel; index: Integer): TCefMenuItemType; stdcall;
    get_group_id          : function(self: PCefMenuModel; command_id: Integer): Integer; stdcall;
    get_group_id_at       : function(self: PCefMenuModel; index: Integer): Integer; stdcall;
    set_group_id          : function(self: PCefMenuModel; command_id, group_id: Integer): Integer; stdcall;
    set_group_id_at       : function(self: PCefMenuModel; index, group_id: Integer): Integer; stdcall;
    get_sub_menu          : function(self: PCefMenuModel; command_id: Integer): PCefMenuModel; stdcall;
    get_sub_menu_at       : function(self: PCefMenuModel; index: Integer): PCefMenuModel; stdcall;
    is_visible            : function(self: PCefMenuModel; command_id: Integer): Integer; stdcall;
    is_visible_at         : function(self: PCefMenuModel; index: Integer): Integer; stdcall;
    set_visible           : function(self: PCefMenuModel; command_id, visible: Integer): Integer; stdcall;
    set_visible_at        : function(self: PCefMenuModel; index, visible: Integer): Integer; stdcall;
    is_enabled            : function(self: PCefMenuModel; command_id: Integer): Integer; stdcall;
    is_enabled_at         : function(self: PCefMenuModel; index: Integer): Integer; stdcall;
    set_enabled           : function(self: PCefMenuModel; command_id, enabled: Integer): Integer; stdcall;
    set_enabled_at        : function(self: PCefMenuModel; index, enabled: Integer): Integer; stdcall;
    is_checked            : function(self: PCefMenuModel; command_id: Integer): Integer; stdcall;
    is_checked_at         : function(self: PCefMenuModel; index: Integer): Integer; stdcall;
    set_checked           : function(self: PCefMenuModel; command_id, checked: Integer): Integer; stdcall;
    set_checked_at        : function(self: PCefMenuModel; index, checked: Integer): Integer; stdcall;
    has_accelerator       : function(self: PCefMenuModel; command_id: Integer): Integer; stdcall;
    has_accelerator_at    : function(self: PCefMenuModel; index: Integer): Integer; stdcall;
    set_accelerator       : function(self: PCefMenuModel; command_id, key_code, shift_pressed, ctrl_pressed, alt_pressed: Integer): Integer; stdcall;
    set_accelerator_at    : function(self: PCefMenuModel; index, key_code, shift_pressed, ctrl_pressed, alt_pressed: Integer): Integer; stdcall;
    remove_accelerator    : function(self: PCefMenuModel; command_id: Integer): Integer; stdcall;
    remove_accelerator_at : function(self: PCefMenuModel; index: Integer): Integer; stdcall;
    get_accelerator       : function(self: PCefMenuModel; command_id: Integer; key_code, shift_pressed, ctrl_pressed, alt_pressed: PInteger): Integer; stdcall;
    get_accelerator_at    : function(self: PCefMenuModel; index: Integer; key_code, shift_pressed, ctrl_pressed, alt_pressed: PInteger): Integer; stdcall;
    set_color             : function(self: PCefMenuModel; command_id: Integer; color_type: TCefMenuColorType; color: TCefColor): Integer; stdcall;
    set_color_at          : function(self: PCefMenuModel; index: Integer; color_type: TCefMenuColorType; color: TCefColor): Integer; stdcall;
    get_color             : function(self: PCefMenuModel; command_id: Integer; color_type: TCefMenuColorType; color: PCefColor): Integer; stdcall;
    get_color_at          : function(self: PCefMenuModel; index: Integer; color_type: TCefMenuColorType; color: PCefColor): Integer; stdcall;
    set_font_list         : function(self: PCefMenuModel; command_id: Integer; const font_list: PCefString): Integer; stdcall;
    set_font_list_at      : function(self: PCefMenuModel; index: Integer; const font_list: PCefString): Integer; stdcall;
  end;

  // /include/capi/cef_context_menu_handler_capi.h (cef_context_menu_params_t)
  TCefContextMenuParams = record
    base                        : TCefBaseRefCounted;
    get_xcoord                  : function(self: PCefContextMenuParams): Integer; stdcall;
    get_ycoord                  : function(self: PCefContextMenuParams): Integer; stdcall;
    get_type_flags              : function(self: PCefContextMenuParams): TCefContextMenuTypeFlags; stdcall;
    get_link_url                : function(self: PCefContextMenuParams): PCefStringUserFree; stdcall;
    get_unfiltered_link_url     : function(self: PCefContextMenuParams): PCefStringUserFree; stdcall;
    get_source_url              : function(self: PCefContextMenuParams): PCefStringUserFree; stdcall;
    has_image_contents          : function(self: PCefContextMenuParams): Integer; stdcall;
    get_title_text              : function(self: PCefContextMenuParams): PCefStringUserFree; stdcall;
    get_page_url                : function(self: PCefContextMenuParams): PCefStringUserFree; stdcall;
    get_frame_url               : function(self: PCefContextMenuParams): PCefStringUserFree; stdcall;
    get_frame_charset           : function(self: PCefContextMenuParams): PCefStringUserFree; stdcall;
    get_media_type              : function(self: PCefContextMenuParams): TCefContextMenuMediaType; stdcall;
    get_media_state_flags       : function(self: PCefContextMenuParams): TCefContextMenuMediaStateFlags; stdcall;
    get_selection_text          : function(self: PCefContextMenuParams): PCefStringUserFree; stdcall;
    get_misspelled_word         : function(self: PCefContextMenuParams): PCefStringUserFree; stdcall;
    get_dictionary_suggestions  : function(self: PCefContextMenuParams; suggestions: TCefStringList): Integer; stdcall;
    is_editable                 : function(self: PCefContextMenuParams): Integer; stdcall;
    is_spell_check_enabled      : function(self: PCefContextMenuParams): Integer; stdcall;
    get_edit_state_flags        : function(self: PCefContextMenuParams): TCefContextMenuEditStateFlags; stdcall;
    is_custom_menu              : function(self: PCefContextMenuParams): Integer; stdcall;
    is_pepper_menu              : function(self: PCefContextMenuParams): Integer; stdcall;
  end;

  // /include/capi/cef_download_item_capi.h (cef_download_item_t)
  TCefDownloadItem = record
    base                    : TCefBaseRefCounted;
    is_valid                : function(self: PCefDownloadItem): Integer; stdcall;
    is_in_progress          : function(self: PCefDownloadItem): Integer; stdcall;
    is_complete             : function(self: PCefDownloadItem): Integer; stdcall;
    is_canceled             : function(self: PCefDownloadItem): Integer; stdcall;
    get_current_speed       : function(self: PCefDownloadItem): Int64; stdcall;
    get_percent_complete    : function(self: PCefDownloadItem): Integer; stdcall;
    get_total_bytes         : function(self: PCefDownloadItem): Int64; stdcall;
    get_received_bytes      : function(self: PCefDownloadItem): Int64; stdcall;
    get_start_time          : function(self: PCefDownloadItem): TCefTime; stdcall;
    get_end_time            : function(self: PCefDownloadItem): TCefTime; stdcall;
    get_full_path           : function(self: PCefDownloadItem): PCefStringUserFree; stdcall;
    get_id                  : function(self: PCefDownloadItem): Cardinal; stdcall;
    get_url                 : function(self: PCefDownloadItem): PCefStringUserFree; stdcall;
    get_original_url        : function(self: PCefDownloadItem): PCefStringUserFree; stdcall;
    get_suggested_file_name : function(self: PCefDownloadItem): PCefStringUserFree; stdcall;
    get_content_disposition : function(self: PCefDownloadItem): PCefStringUserFree; stdcall;
    get_mime_type           : function(self: PCefDownloadItem): PCefStringUserFree; stdcall;
  end;

  // /include/capi/cef_download_handler_capi.h (cef_before_download_callback_t)
  TCefBeforeDownloadCallback = record
    base : TCefBaseRefCounted;
    cont : procedure(self: PCefBeforeDownloadCallback; const download_path: PCefString; show_dialog: Integer); stdcall;
  end;

  // /include/capi/cef_download_handler_capi.h (cef_download_item_callback_t)
  TCefDownloadItemCallback = record
    base   : TCefBaseRefCounted;
    cancel : procedure(self: PCefDownloadItemCallback); stdcall;
    pause  : procedure(self: PCefDownloadItemCallback); stdcall;
    resume : procedure(self: PCefDownloadItemCallback); stdcall;
  end;

  // /include/capi/cef_dom_capi.h (cef_domnode_t)
  TCefDomNode = record
    base                          : TCefBaseRefCounted;
    get_type                      : function(self: PCefDomNode): TCefDomNodeType; stdcall;
    is_text                       : function(self: PCefDomNode): Integer; stdcall;
    is_element                    : function(self: PCefDomNode): Integer; stdcall;
    is_editable                   : function(self: PCefDomNode): Integer; stdcall;
    is_form_control_element       : function(self: PCefDomNode): Integer; stdcall;
    get_form_control_element_type : function(self: PCefDomNode): PCefStringUserFree; stdcall;
    is_same                       : function(self, that: PCefDomNode): Integer; stdcall;
    get_name                      : function(self: PCefDomNode): PCefStringUserFree; stdcall;
    get_value                     : function(self: PCefDomNode): PCefStringUserFree; stdcall;
    set_value                     : function(self: PCefDomNode; const value: PCefString): Integer; stdcall;
    get_as_markup                 : function(self: PCefDomNode): PCefStringUserFree; stdcall;
    get_document                  : function(self: PCefDomNode): PCefDomDocument; stdcall;
    get_parent                    : function(self: PCefDomNode): PCefDomNode; stdcall;
    get_previous_sibling          : function(self: PCefDomNode): PCefDomNode; stdcall;
    get_next_sibling              : function(self: PCefDomNode): PCefDomNode; stdcall;
    has_children                  : function(self: PCefDomNode): Integer; stdcall;
    get_first_child               : function(self: PCefDomNode): PCefDomNode; stdcall;
    get_last_child                : function(self: PCefDomNode): PCefDomNode; stdcall;
    get_element_tag_name          : function(self: PCefDomNode): PCefStringUserFree; stdcall;
    has_element_attributes        : function(self: PCefDomNode): Integer; stdcall;
    has_element_attribute         : function(self: PCefDomNode; const attrName: PCefString): Integer; stdcall;
    get_element_attribute         : function(self: PCefDomNode; const attrName: PCefString): PCefStringUserFree; stdcall;
    get_element_attributes        : procedure(self: PCefDomNode; attrMap: TCefStringMap); stdcall;
    set_element_attribute         : function(self: PCefDomNode; const attrName, value: PCefString): Integer; stdcall;
    get_element_inner_text        : function(self: PCefDomNode): PCefStringUserFree; stdcall;
    get_element_bounds            : function(self: PCefDomNode): TCefRect; stdcall;
  end;

  // /include/capi/cef_dom_capi.h (cef_domdocument_t)
  TCefDomDocument = record
    base                        : TCefBaseRefCounted;
    get_type                    : function(self: PCefDomDocument): TCefDomDocumentType; stdcall;
    get_document                : function(self: PCefDomDocument): PCefDomNode; stdcall;
    get_body                    : function(self: PCefDomDocument): PCefDomNode; stdcall;
    get_head                    : function(self: PCefDomDocument): PCefDomNode; stdcall;
    get_title                   : function(self: PCefDomDocument): PCefStringUserFree; stdcall;
    get_element_by_id           : function(self: PCefDomDocument; const id: PCefString): PCefDomNode; stdcall;
    get_focused_node            : function(self: PCefDomDocument): PCefDomNode; stdcall;
    has_selection               : function(self: PCefDomDocument): Integer; stdcall;
    get_selection_start_offset  : function(self: PCefDomDocument): Integer; stdcall;
    get_selection_end_offset    : function(self: PCefDomDocument): Integer; stdcall;
    get_selection_as_markup     : function(self: PCefDomDocument): PCefStringUserFree; stdcall;
    get_selection_as_text       : function(self: PCefDomDocument): PCefStringUserFree; stdcall;
    get_base_url                : function(self: PCefDomDocument): PCefStringUserFree; stdcall;
    get_complete_url            : function(self: PCefDomDocument; const partialURL: PCefString): PCefStringUserFree; stdcall;
  end;

  PCefV8ValueArray = array[0..(High(Integer) div SizeOf(Pointer)) - 1] of PCefV8Value;

  // /include/capi/cef_v8_capi.h (cef_v8handler_t)
  TCefv8Handler = record
    base    : TCefBaseRefCounted;
    execute : function(self: PCefv8Handler; const name: PCefString; object_: PCefv8Value; argumentsCount: NativeUInt; const arguments: PPCefV8Value; var retval: PCefV8Value; exception: PCefString): Integer; stdcall;
  end;

  // /include/capi/cef_v8_capi.h (cef_v8exception_t)
  TCefV8Exception = record
    base                      : TCefBaseRefCounted;
    get_message               : function(self: PCefV8Exception): PCefStringUserFree; stdcall;
    get_source_line           : function(self: PCefV8Exception): PCefStringUserFree; stdcall;
    get_script_resource_name  : function(self: PCefV8Exception): PCefStringUserFree; stdcall;
    get_line_number           : function(self: PCefV8Exception): Integer; stdcall;
    get_start_position        : function(self: PCefV8Exception): Integer; stdcall;
    get_end_position          : function(self: PCefV8Exception): Integer; stdcall;
    get_start_column          : function(self: PCefV8Exception): Integer; stdcall;
    get_end_column            : function(self: PCefV8Exception): Integer; stdcall;
  end;

  // /include/capi/cef_v8_capi.h (cef_v8array_buffer_release_callback_t)
  TCefv8ArrayBufferReleaseCallback = record
    base                      : TCefBaseRefCounted;
    release_buffer            : procedure(self: PCefv8ArrayBufferReleaseCallback; buffer : Pointer); stdcall;
  end;

  // /include/capi/cef_v8_capi.h (cef_v8value_t)
  TCefv8Value = record
    base                                : TCefBaseRefCounted;
    is_valid                            : function(self: PCefv8Value): Integer; stdcall;
    is_undefined                        : function(self: PCefv8Value): Integer; stdcall;
    is_null                             : function(self: PCefv8Value): Integer; stdcall;
    is_bool                             : function(self: PCefv8Value): Integer; stdcall;
    is_int                              : function(self: PCefv8Value): Integer; stdcall;
    is_uint                             : function(self: PCefv8Value): Integer; stdcall;
    is_double                           : function(self: PCefv8Value): Integer; stdcall;
    is_date                             : function(self: PCefv8Value): Integer; stdcall;
    is_string                           : function(self: PCefv8Value): Integer; stdcall;
    is_object                           : function(self: PCefv8Value): Integer; stdcall;
    is_array                            : function(self: PCefv8Value): Integer; stdcall;
    is_array_buffer                     : function(self: PCefv8Value): Integer; stdcall;
    is_function                         : function(self: PCefv8Value): Integer; stdcall;
    is_same                             : function(self, that: PCefv8Value): Integer; stdcall;
    get_bool_value                      : function(self: PCefv8Value): Integer; stdcall;
    get_int_value                       : function(self: PCefv8Value): Integer; stdcall;
    get_uint_value                      : function(self: PCefv8Value): Cardinal; stdcall;
    get_double_value                    : function(self: PCefv8Value): Double; stdcall;
    get_date_value                      : function(self: PCefv8Value): TCefTime; stdcall;
    get_string_value                    : function(self: PCefv8Value): PCefStringUserFree; stdcall;
    is_user_created                     : function(self: PCefv8Value): Integer; stdcall;
    has_exception                       : function(self: PCefv8Value): Integer; stdcall;
    get_exception                       : function(self: PCefv8Value): PCefV8Exception; stdcall;
    clear_exception                     : function(self: PCefv8Value): Integer; stdcall;
    will_rethrow_exceptions             : function(self: PCefv8Value): Integer; stdcall;
    set_rethrow_exceptions              : function(self: PCefv8Value; rethrow: Integer): Integer; stdcall;
    has_value_bykey                     : function(self: PCefv8Value; const key: PCefString): Integer; stdcall;
    has_value_byindex                   : function(self: PCefv8Value; index: Integer): Integer; stdcall;
    delete_value_bykey                  : function(self: PCefv8Value; const key: PCefString): Integer; stdcall;
    delete_value_byindex                : function(self: PCefv8Value; index: Integer): Integer; stdcall;
    get_value_bykey                     : function(self: PCefv8Value; const key: PCefString): PCefv8Value; stdcall;
    get_value_byindex                   : function(self: PCefv8Value; index: Integer): PCefv8Value; stdcall;
    set_value_bykey                     : function(self: PCefv8Value; const key: PCefString; value: PCefv8Value; attribute: TCefV8PropertyAttributes): Integer; stdcall;
    set_value_byindex                   : function(self: PCefv8Value; index: Integer; value: PCefv8Value): Integer; stdcall;
    set_value_byaccessor                : function(self: PCefv8Value; const key: PCefString; settings: Integer; attribute: TCefV8PropertyAttributes): Integer; stdcall;
    get_keys                            : function(self: PCefv8Value; keys: TCefStringList): Integer; stdcall;
    set_user_data                       : function(self: PCefv8Value; user_data: PCefBaseRefCounted): Integer; stdcall;
    get_user_data                       : function(self: PCefv8Value): PCefBaseRefCounted; stdcall;
    get_externally_allocated_memory     : function(self: PCefv8Value): Integer; stdcall;
    adjust_externally_allocated_memory  : function(self: PCefv8Value; change_in_bytes: Integer): Integer; stdcall;
    get_array_length                    : function(self: PCefv8Value): Integer; stdcall;
    get_array_buffer_release_callback   : function(self: PCefv8Value): PCefv8ArrayBufferReleaseCallback; stdcall;
    neuter_array_buffer                 : function(self: PCefv8Value): Integer; stdcall;
    get_function_name                   : function(self: PCefv8Value): PCefStringUserFree; stdcall;
    get_function_handler                : function(self: PCefv8Value): PCefv8Handler; stdcall;
    execute_function                    : function(self: PCefv8Value; obj: PCefv8Value; argumentsCount: NativeUInt; const arguments: PPCefV8Value): PCefv8Value; stdcall;
    execute_function_with_context       : function(self: PCefv8Value; context: PCefv8Context; obj: PCefv8Value; argumentsCount: NativeUInt; const arguments: PPCefV8Value): PCefv8Value; stdcall;
  end;

  // /include/capi/cef_v8_capi.h (cef_v8context_t)
  TCefV8Context = record
    base            : TCefBaseRefCounted;
    get_task_runner : function(self: PCefv8Context): PCefTaskRunner; stdcall;
    is_valid        : function(self: PCefv8Context): Integer; stdcall;
    get_browser     : function(self: PCefv8Context): PCefBrowser; stdcall;
    get_frame       : function(self: PCefv8Context): PCefFrame; stdcall;
    get_global      : function(self: PCefv8Context): PCefv8Value; stdcall;
    enter           : function(self: PCefv8Context): Integer; stdcall;
    exit            : function(self: PCefv8Context): Integer; stdcall;
    is_same         : function(self, that: PCefv8Context): Integer; stdcall;
    eval            : function(self: PCefv8Context; const code, script_url: PCefString; start_line: integer; var retval: PCefv8Value; var exception: PCefV8Exception): Integer; stdcall;
  end;

  // /include/capi/cef_v8_capi.h (cef_v8interceptor_t)
  TCefV8Interceptor = record
    base        : TCefBaseRefCounted;
    get_byname  : function(self: PCefV8Interceptor; const name: PCefString; object_: PCefV8Value; out retval: PCefv8Value; exception: PCefString): integer; stdcall;
    get_byindex : function(self: PCefV8Interceptor; index: integer; object_: PCefV8Value; out retval: PCefv8Value; exception: PCefString): integer; stdcall;
    set_byname  : function(self: PCefV8Interceptor; const name: PCefString; object_, value: PCefv8Value; exception: PCefString): integer; stdcall;
    set_byindex : function(self: PCefV8Interceptor; index: integer; object_, value: PCefv8Value; exception: PCefString): integer; stdcall;
  end;

  // /include/capi/cef_v8_capi.h (cef_v8accessor_t)
  TCefV8Accessor = record
    base : TCefBaseRefCounted;
    get  : function(self: PCefV8Accessor; const name: PCefString; object_: PCefv8Value; out retval: PCefv8Value; exception: PCefString): Integer; stdcall;
    set_ : function(self: PCefV8Accessor; const name: PCefString; object_, value: PCefv8Value; exception: PCefString): Integer; stdcall;
  end;

  // /include/capi/cef_frame_capi.h (cef_frame_t)
  TCefFrame = record
    base                 : TCefBaseRefCounted;
    is_valid             : function(self: PCefFrame): Integer; stdcall;
    undo                 : procedure(self: PCefFrame); stdcall;
    redo                 : procedure(self: PCefFrame); stdcall;
    cut                  : procedure(self: PCefFrame); stdcall;
    copy                 : procedure(self: PCefFrame); stdcall;
    paste                : procedure(self: PCefFrame); stdcall;
    del                  : procedure(self: PCefFrame); stdcall;
    select_all           : procedure(self: PCefFrame); stdcall;
    view_source          : procedure(self: PCefFrame); stdcall;
    get_source           : procedure(self: PCefFrame; visitor: PCefStringVisitor); stdcall;
    get_text             : procedure(self: PCefFrame; visitor: PCefStringVisitor); stdcall;
    load_request         : procedure(self: PCefFrame; request: PCefRequest); stdcall;
    load_url             : procedure(self: PCefFrame; const url: PCefString); stdcall;
    execute_java_script  : procedure(self: PCefFrame; const code, script_url: PCefString; start_line: Integer); stdcall;
    is_main              : function(self: PCefFrame): Integer; stdcall;
    is_focused           : function(self: PCefFrame): Integer; stdcall;
    get_name             : function(self: PCefFrame): PCefStringUserFree; stdcall;
    get_identifier       : function(self: PCefFrame): Int64; stdcall;
    get_parent           : function(self: PCefFrame): PCefFrame; stdcall;
    get_url              : function(self: PCefFrame): PCefStringUserFree; stdcall;
    get_browser          : function(self: PCefFrame): PCefBrowser; stdcall;
    get_v8context        : function(self: PCefFrame): PCefv8Context; stdcall;
    visit_dom            : procedure(self: PCefFrame; visitor: PCefDomVisitor); stdcall;
    create_urlrequest    : function(self: PCefFrame; request: PCefRequest; client: PCefUrlrequestClient): PCefUrlRequest; stdcall;
    send_process_message : procedure(self: PCefFrame; target_process: TCefProcessId; message_: PCefProcessMessage); stdcall;
  end;

  // /include/capi/cef_accessibility_handler_capi.h (cef_accessibility_handler_t)
  TCefAccessibilityHandler = record
    base                             : TCefBaseRefCounted;
    on_accessibility_tree_change     : procedure(self: PCefAccessibilityHandler; value: PCefValue); stdcall;
    on_accessibility_location_change : procedure(self: PCefAccessibilityHandler; value: PCefValue); stdcall;
  end;

  // /include/capi/cef_context_menu_handler_capi.h (cef_context_menu_handler_t)
  TCefContextMenuHandler = record
    base                      : TCefBaseRefCounted;
    on_before_context_menu    : procedure(self: PCefContextMenuHandler; browser: PCefBrowser; frame: PCefFrame; params: PCefContextMenuParams; model: PCefMenuModel); stdcall;
    run_context_menu          : function(self: PCefContextMenuHandler; browser: PCefBrowser; frame: PCefFrame; params: PCefContextMenuParams; model: PCefMenuModel; callback: PCefRunContextMenuCallback): Integer; stdcall;
    on_context_menu_command   : function(self: PCefContextMenuHandler; browser: PCefBrowser; frame: PCefFrame; params: PCefContextMenuParams; command_id: Integer; event_flags: TCefEventFlags): Integer; stdcall;
    on_context_menu_dismissed : procedure(self: PCefContextMenuHandler; browser: PCefBrowser; frame: PCefFrame); stdcall;
  end;

  // /include/capi/cef_client_capi.h (cef_client_t)
  TCefClient = record
    base                        : TCefBaseRefCounted;
    get_audio_handler           : function(self: PCefClient): PCefAudioHandler; stdcall;
    get_context_menu_handler    : function(self: PCefClient): PCefContextMenuHandler; stdcall;
    get_dialog_handler          : function(self: PCefClient): PCefDialogHandler; stdcall;
    get_display_handler         : function(self: PCefClient): PCefDisplayHandler; stdcall;
    get_download_handler        : function(self: PCefClient): PCefDownloadHandler; stdcall;
    get_drag_handler            : function(self: PCefClient): PCefDragHandler; stdcall;
    get_find_handler            : function(self: PCefClient): PCefFindHandler; stdcall;
    get_focus_handler           : function(self: PCefClient): PCefFocusHandler; stdcall;
    get_jsdialog_handler        : function(self: PCefClient): PCefJsDialogHandler; stdcall;
    get_keyboard_handler        : function(self: PCefClient): PCefKeyboardHandler; stdcall;
    get_life_span_handler       : function(self: PCefClient): PCefLifeSpanHandler; stdcall;
    get_load_handler            : function(self: PCefClient): PCefLoadHandler; stdcall;
    get_render_handler          : function(self: PCefClient): PCefRenderHandler; stdcall;
    get_request_handler         : function(self: PCefClient): PCefRequestHandler; stdcall;
    on_process_message_received : function(self: PCefClient; browser: PCefBrowser; frame: PCefFrame; source_process: TCefProcessId; message_: PCefProcessMessage): Integer; stdcall;
  end;

  // /include/capi/cef_browser_capi.h (cef_browser_host_t)
  TCefBrowserHost = record
    base                              : TCefBaseRefCounted;
    get_browser                       : function(self: PCefBrowserHost): PCefBrowser; stdcall;
    close_browser                     : procedure(self: PCefBrowserHost; force_close: Integer); stdcall;
    try_close_browser                 : function(self: PCefBrowserHost): Integer; stdcall;
    set_focus                         : procedure(self: PCefBrowserHost; focus: Integer); stdcall;
    get_window_handle                 : function(self: PCefBrowserHost): TCefWindowHandle; stdcall;
    get_opener_window_handle          : function(self: PCefBrowserHost): TCefWindowHandle; stdcall;
    has_view                          : function(self: PCefBrowserHost): Integer; stdcall;
    get_client                        : function(self: PCefBrowserHost): PCefClient; stdcall;
    get_request_context               : function(self: PCefBrowserHost): PCefRequestContext; stdcall;
    get_zoom_level                    : function(self: PCefBrowserHost): Double; stdcall;
    set_zoom_level                    : procedure(self: PCefBrowserHost; zoomLevel: Double); stdcall;
    run_file_dialog                   : procedure(self: PCefBrowserHost; mode: TCefFileDialogMode; const title, default_file_path: PCefString; accept_filters: TCefStringList; selected_accept_filter: Integer; callback: PCefRunFileDialogCallback); stdcall;
    start_download                    : procedure(self: PCefBrowserHost; const url: PCefString); stdcall;
    download_image                    : procedure(self: PCefBrowserHost; const image_url: PCefString; is_favicon: Integer; max_image_size: Cardinal; bypass_cache: Integer; callback: PCefDownloadImageCallback); stdcall;
    print                             : procedure(self: PCefBrowserHost); stdcall;
    print_to_pdf                      : procedure(self: PCefBrowserHost; const path: PCefString; const settings: PCefPdfPrintSettings; callback: PCefPdfPrintCallback); stdcall;
    find                              : procedure(self: PCefBrowserHost; identifier: Integer; const searchText: PCefString; forward_, matchCase, findNext: Integer); stdcall;
    stop_finding                      : procedure(self: PCefBrowserHost; clearSelection: Integer); stdcall;
    show_dev_tools                    : procedure(self: PCefBrowserHost; const windowInfo: PCefWindowInfo; client: PCefClient; const settings: PCefBrowserSettings; const inspect_element_at: PCefPoint); stdcall;
    close_dev_tools                   : procedure(self: PCefBrowserHost); stdcall;
    has_dev_tools                     : function(self: PCefBrowserHost): Integer; stdcall;
    send_dev_tools_message            : function(self: PCefBrowserHost; const message_: Pointer; message_size: NativeUInt): Integer; stdcall;
    execute_dev_tools_method          : function(self: PCefBrowserHost; message_id: integer; const method: PCefString; params: PCefDictionaryValue): Integer; stdcall;
    add_dev_tools_message_observer    : function(self: PCefBrowserHost; observer: PCefDevToolsMessageObserver): PCefRegistration; stdcall;
    get_navigation_entries            : procedure(self: PCefBrowserHost; visitor: PCefNavigationEntryVisitor; current_only: Integer); stdcall;
    replace_misspelling               : procedure(self: PCefBrowserHost; const word: PCefString); stdcall;
    add_word_to_dictionary            : procedure(self: PCefBrowserHost; const word: PCefString); stdcall;
    is_window_rendering_disabled      : function(self: PCefBrowserHost): Integer; stdcall;
    was_resized                       : procedure(self: PCefBrowserHost); stdcall;
    was_hidden                        : procedure(self: PCefBrowserHost; hidden: Integer); stdcall;
    notify_screen_info_changed        : procedure(self: PCefBrowserHost); stdcall;
    invalidate                        : procedure(self: PCefBrowserHost; type_: TCefPaintElementType); stdcall;
    send_external_begin_frame         : procedure(self: PCefBrowserHost); stdcall;
    send_key_event                    : procedure(self: PCefBrowserHost; const event: PCefKeyEvent); stdcall;
    send_mouse_click_event            : procedure(self: PCefBrowserHost; const event: PCefMouseEvent; type_: TCefMouseButtonType; mouseUp, clickCount: Integer); stdcall;
    send_mouse_move_event             : procedure(self: PCefBrowserHost; const event: PCefMouseEvent; mouseLeave: Integer); stdcall;
    send_mouse_wheel_event            : procedure(self: PCefBrowserHost; const event: PCefMouseEvent; deltaX, deltaY: Integer); stdcall;
    send_touch_event                  : procedure(self: PCefBrowserHost; const event: PCefTouchEvent); stdcall;
    send_focus_event                  : procedure(self: PCefBrowserHost; setFocus: Integer); stdcall;
    send_capture_lost_event           : procedure(self: PCefBrowserHost); stdcall;
    notify_move_or_resize_started     : procedure(self: PCefBrowserHost); stdcall;
    get_windowless_frame_rate         : function(self: PCefBrowserHost): Integer; stdcall;
    set_windowless_frame_rate         : procedure(self: PCefBrowserHost; frame_rate: Integer); stdcall;
    ime_set_composition               : procedure(self: PCefBrowserHost; const text: PCefString; underlinesCount : NativeUInt; const underlines : PCefCompositionUnderline; const replacement_range, selection_range : PCefRange); stdcall;
    ime_commit_text                   : procedure(self: PCefBrowserHost; const text: PCefString; const replacement_range : PCefRange; relative_cursor_pos : integer); stdcall;
    ime_finish_composing_text         : procedure(self: PCefBrowserHost; keep_selection : integer); stdcall;
    ime_cancel_composition            : procedure(self: PCefBrowserHost); stdcall;
    drag_target_drag_enter            : procedure(self: PCefBrowserHost; drag_data: PCefDragData; const event: PCefMouseEvent; allowed_ops: TCefDragOperations); stdcall;
    drag_target_drag_over             : procedure(self: PCefBrowserHost; const event: PCefMouseEvent; allowed_ops: TCefDragOperations); stdcall;
    drag_target_drag_leave            : procedure(self: PCefBrowserHost); stdcall;
    drag_target_drop                  : procedure(self: PCefBrowserHost; const event: PCefMouseEvent); stdcall;
    drag_source_ended_at              : procedure(self: PCefBrowserHost; x, y: Integer; op: TCefDragOperation); stdcall;
    drag_source_system_drag_ended     : procedure(self: PCefBrowserHost); stdcall;
    get_visible_navigation_entry      : function(self: PCefBrowserHost): PCefNavigationEntry; stdcall;
    set_accessibility_state           : procedure(self: PCefBrowserHost; accessibility_state: TCefState); stdcall;
    set_auto_resize_enabled           : procedure(self: PCefBrowserHost; enabled: integer; const min_size, max_size: PCefSize); stdcall;
    get_extension                     : function(self: PCefBrowserHost): PCefExtension; stdcall;
    is_background_host                : function(self: PCefBrowserHost): integer; stdcall;
    set_audio_muted                   : procedure(self: PCefBrowserHost; mute: integer); stdcall;
    is_audio_muted                    : function(self: PCefBrowserHost): integer; stdcall;
  end;

  // /include/capi/cef_browser_capi.h (cef_browser_t)
  TCefBrowser = record
    base                  : TCefBaseRefCounted;
    get_host              : function(self: PCefBrowser): PCefBrowserHost; stdcall;
    can_go_back           : function(self: PCefBrowser): Integer; stdcall;
    go_back               : procedure(self: PCefBrowser); stdcall;
    can_go_forward        : function(self: PCefBrowser): Integer; stdcall;
    go_forward            : procedure(self: PCefBrowser); stdcall;
    is_loading            : function(self: PCefBrowser): Integer; stdcall;
    reload                : procedure(self: PCefBrowser); stdcall;
    reload_ignore_cache   : procedure(self: PCefBrowser); stdcall;
    stop_load             : procedure(self: PCefBrowser); stdcall;
    get_identifier        : function(self: PCefBrowser): Integer; stdcall;
    is_same               : function(self, that: PCefBrowser): Integer; stdcall;
    is_popup              : function(self: PCefBrowser): Integer; stdcall;
    has_document          : function(self: PCefBrowser): Integer; stdcall;
    get_main_frame        : function(self: PCefBrowser): PCefFrame; stdcall;
    get_focused_frame     : function(self: PCefBrowser): PCefFrame; stdcall;
    get_frame_byident     : function(self: PCefBrowser; identifier: Int64): PCefFrame; stdcall;
    get_frame             : function(self: PCefBrowser; const name: PCefString): PCefFrame; stdcall;
    get_frame_count       : function(self: PCefBrowser): NativeUInt; stdcall;
    get_frame_identifiers : procedure(self: PCefBrowser; var identifiersCount: NativeUInt; var identifiers: Int64); stdcall;
    get_frame_names       : procedure(self: PCefBrowser; names: TCefStringList); stdcall;
  end;

  // /include/capi/cef_resource_bundle_handler_capi.h (cef_resource_bundle_handler_t)
  TCefResourceBundleHandler = record
    base                        : TCefBaseRefCounted;
    get_localized_string        : function(self: PCefResourceBundleHandler; string_id: Integer; string_val: PCefString): Integer; stdcall;
    get_data_resource           : function(self: PCefResourceBundleHandler; resource_id: Integer; var data: Pointer; var data_size: NativeUInt): Integer; stdcall;
    get_data_resource_for_scale : function(self: PCefResourceBundleHandler; resource_id: Integer; scale_factor: TCefScaleFactor; var data: Pointer; var data_size: NativeUInt): Integer; stdcall;
  end;

  // /include/capi/cef_browser_process_handler_capi.h (cef_browser_process_handler_t)
  TCefBrowserProcessHandler = record
    base                              : TCefBaseRefCounted;
    get_cookieable_schemes            : procedure(self: PCefBrowserProcessHandler; schemes: TCefStringList; include_defaults: PInteger); stdcall;
    on_context_initialized            : procedure(self: PCefBrowserProcessHandler); stdcall;
    on_before_child_process_launch    : procedure(self: PCefBrowserProcessHandler; command_line: PCefCommandLine); stdcall;
    get_print_handler                 : function(self: PCefBrowserProcessHandler): PCefPrintHandler; stdcall;
    on_schedule_message_pump_work     : procedure(self: PCefBrowserProcessHandler; delay_ms: Int64); stdcall;
    get_default_client                : function(self: PCefBrowserProcessHandler): PCefClient; stdcall;
  end;

  // /include/capi/cef_app_capi.h (cef_app_t)
  TCefApp = record
    base                              : TCefBaseRefCounted;
    on_before_command_line_processing : procedure(self: PCefApp; const process_type: PCefString; command_line: PCefCommandLine); stdcall;
    on_register_custom_schemes        : procedure(self: PCefApp; registrar: PCefSchemeRegistrar); stdcall;
    get_resource_bundle_handler       : function(self: PCefApp): PCefResourceBundleHandler; stdcall;
    get_browser_process_handler       : function(self: PCefApp): PCefBrowserProcessHandler; stdcall;
    get_render_process_handler        : function(self: PCefApp): PCefRenderProcessHandler; stdcall;
  end;

  // /include/capi/cef_server_capi.h (cef_server_t)
  TCefServer = record
    base                    : TCefBaseRefCounted;
    get_task_runner         : function(self: PCefServer): PCefTaskRunner; stdcall;
    shutdown                : procedure(self: PCefServer); stdcall;
    is_running              : function(self: PCefServer): Integer; stdcall;
    get_address             : function(self: PCefServer): PCefStringUserFree; stdcall;
    has_connection          : function(self: PCefServer): Integer; stdcall;
    is_valid_connection     : function(self: PCefServer; connection_id: Integer): Integer; stdcall;
    send_http200response    : procedure(self: PCefServer; connection_id: Integer; const content_type: PCefString; const data: Pointer; data_size: NativeUInt); stdcall;
    send_http404response    : procedure(self: PCefServer; connection_id: Integer); stdcall;
    send_http500response    : procedure(self: PCefServer; connection_id: Integer; const error_message: PCefString); stdcall;
    send_http_response      : procedure(self: PCefServer; connection_id, response_code: Integer; const content_type: PCefString; content_length: int64; extra_headers: TCefStringMultimap); stdcall;
    send_raw_data           : procedure(self: PCefServer; connection_id: Integer; const data: Pointer; data_size: NativeUInt); stdcall;
    close_connection        : procedure(self: PCefServer; connection_id: Integer); stdcall;
    send_web_socket_message : procedure(self: PCefServer; connection_id: Integer; const data: Pointer; data_size: NativeUInt); stdcall;
  end;

  // /include/capi/cef_server_capi.h (cef_server_handler_t)
  TCefServerHandler = record
    base                    : TCefBaseRefCounted;
    on_server_created       : procedure(self: PCefServerHandler; server: PCefServer); stdcall;
    on_server_destroyed     : procedure(self: PCefServerHandler; server: PCefServer); stdcall;
    on_client_connected     : procedure(self: PCefServerHandler; server: PCefServer; connection_id: Integer); stdcall;
    on_client_disconnected  : procedure(self: PCefServerHandler; server: PCefServer; connection_id: Integer); stdcall;
    on_http_request         : procedure(self: PCefServerHandler; server: PCefServer; connection_id: Integer; const client_address: PCefString; request: PCefRequest); stdcall;
    on_web_socket_request   : procedure(self: PCefServerHandler; server: PCefServer; connection_id: Integer; const client_address: PCefString; request: PCefRequest; callback: PCefCallback); stdcall;
    on_web_socket_connected : procedure(self: PCefServerHandler; server: PCefServer; connection_id: Integer); stdcall;
    on_web_socket_message   : procedure(self: PCefServerHandler; server: PCefServer; connection_id: Integer; const data: Pointer; data_size: NativeUInt); stdcall;
  end;



  // *********************************
  // ************* Views *************
  // *********************************
  //
  //  (*) Has CEF creation function
  //  (d) Has delegate
  //
  // ----------------          ----------------------
  // | TCefView (d) | -------> | TCefTextfield (*d) |
  // ----------------    |     ----------------------
  //                     |
  //                     |     ----------------------
  //                     |---> | TCefScrollView (*) |
  //                     |     ----------------------
  //                     |
  //                     |     ------------------          -------------------
  //                     |---> | TCefPanel (*d) | -------> | TCefWindow (*d) |
  //                     |     ------------------          -------------------
  //                     |
  //                     |     ------------------------
  //                     |---> | TCefBrowserView (*d) |
  //                     |     ------------------------
  //                     |
  //                     |     ------------------          -----------------------          -----------------------
  //                     |---> | TCefButton (d) | -------> | TCefLabelButton (*) | -------> | TCefMenuButton (*d) |
  //                           ------------------          -----------------------          -----------------------
  //
  //
  // --------------          -----------------
  // | TCefLayout | -------> | TCefBoxLayout |
  // --------------    |     -----------------
  //                   |
  //                   |     ------------------
  //                   |---> | TCefFillLayout |
  //                         ------------------
  //

  // /include/capi/views/cef_display_capi.h (cef_display_t)
  TCefDisplay = record
    base                      : TCefBaseRefCounted;
    get_id                    : function(self: PCefDisplay): int64; stdcall;
    get_device_scale_factor   : function(self: PCefDisplay): Single; stdcall;
    convert_point_to_pixels   : procedure(self: PCefDisplay; point: PCefPoint); stdcall;
    convert_point_from_pixels : procedure(self: PCefDisplay; point: PCefPoint); stdcall;
    get_bounds                : function(self: PCefDisplay): TCefRect; stdcall;
    get_work_area             : function(self: PCefDisplay): TCefRect; stdcall;
    get_rotation              : function(self: PCefDisplay): Integer; stdcall;
  end;

  // /include/capi/views/cef_layout_capi.h (cef_layout_t)
  TCefLayout = record
    base                    : TCefBaseRefCounted;
    as_box_layout           : function(self: PCefLayout): PCefBoxLayout; stdcall;
    as_fill_layout          : function(self: PCefLayout): PCefFillLayout; stdcall;
    is_valid                : function(self: PCefLayout): Integer; stdcall;
  end;

  // /include/capi/views/cef_box_layout_capi.h (cef_box_layout_t)
  TCefBoxLayout = record
    base                    : TCefLayout;
    set_flex_for_view       : procedure(self: PCefBoxLayout; view: PCefView; flex: Integer); stdcall;
    clear_flex_for_view     : procedure(self: PCefBoxLayout; view: PCefView); stdcall;
  end;

  // /include/capi/views/cef_fill_layout_capi.h (cef_fill_layout_t)
  TCefFillLayout = record
    base                    : TCefLayout;
  end;

  // /include/capi/views/cef_view_capi.h (cef_view_t)
  TCefView = record
    base                        : TCefBaseRefCounted;
    as_browser_view             : function(self: PCefView): PCefBrowserView; stdcall;
    as_button                   : function(self: PCefView): PCefButton; stdcall;
    as_panel                    : function(self: PCefView): PCefPanel; stdcall;
    as_scroll_view              : function(self: PCefView): PCefScrollView; stdcall;
    as_textfield                : function(self: PCefView): PCefTextfield; stdcall;
    get_type_string             : function(self: PCefView): PCefStringUserFree; stdcall;
    to_string                   : function(self: PCefView; include_children: Integer): PCefStringUserFree; stdcall;
    is_valid                    : function(self: PCefView): Integer; stdcall;
    is_attached                 : function(self: PCefView): Integer; stdcall;
    is_same                     : function(self, that: PCefView): Integer; stdcall;
    get_delegate                : function(self: PCefView): PCefViewDelegate; stdcall;
    get_window                  : function(self: PCefView): PCefWindow; stdcall;
    get_id                      : function(self: PCefView): Integer; stdcall;
    set_id                      : procedure(self: PCefView; id: Integer); stdcall;
    get_group_id                : function(self: PCefView): Integer; stdcall;
    set_group_id                : procedure(self: PCefView; group_id: Integer); stdcall;
    get_parent_view             : function(self: PCefView): PCefView; stdcall;
    get_view_for_id             : function(self: PCefView; id: Integer): PCefView; stdcall;
    set_bounds                  : procedure(self: PCefView; const bounds: PCefRect); stdcall;
    get_bounds                  : function(self: PCefView): TCefRect; stdcall;
    get_bounds_in_screen        : function(self: PCefView): TCefRect; stdcall;
    set_size                    : procedure(self: PCefView; const size: PCefSize); stdcall;
    get_size                    : function(self: PCefView): TCefSize; stdcall;
    set_position                : procedure(self: PCefView; const position: PCefPoint); stdcall;
    get_position                : function(self: PCefView): TCefPoint; stdcall;
    get_preferred_size          : function(self: PCefView): TCefSize; stdcall;
    size_to_preferred_size      : procedure(self: PCefView); stdcall;
    get_minimum_size            : function(self: PCefView): TCefSize; stdcall;
    get_maximum_size            : function(self: PCefView): TCefSize; stdcall;
    get_height_for_width        : function(self: PCefView; width: Integer): Integer; stdcall;
    invalidate_layout           : procedure(self: PCefView); stdcall;
    set_visible                 : procedure(self: PCefView; visible: Integer); stdcall;
    is_visible                  : function(self: PCefView): Integer; stdcall;
    is_drawn                    : function(self: PCefView): Integer; stdcall;
    set_enabled                 : procedure(self: PCefView; enabled: Integer); stdcall;
    is_enabled                  : function(self: PCefView): Integer; stdcall;
    set_focusable               : procedure(self: PCefView; focusable: Integer); stdcall;
    is_focusable                : function(self: PCefView): Integer; stdcall;
    is_accessibility_focusable  : function(self: PCefView): Integer; stdcall;
    request_focus               : procedure(self: PCefView); stdcall;
    set_background_color        : procedure(self: PCefView; color: TCefColor); stdcall;
    get_background_color        : function(self: PCefView): TCefColor; stdcall;
    convert_point_to_screen     : function(self: PCefView; point: PCefPoint): Integer; stdcall;
    convert_point_from_screen   : function(self: PCefView; point: PCefPoint): Integer; stdcall;
    convert_point_to_window     : function(self: PCefView; point: PCefPoint): Integer; stdcall;
    convert_point_from_window   : function(self: PCefView; point: PCefPoint): Integer; stdcall;
    convert_point_to_view       : function(self, view: PCefView; point: PCefPoint): Integer; stdcall;
    convert_point_from_view     : function(self, view: PCefView; point: PCefPoint): Integer; stdcall;
  end;

  // /include/capi/views/cef_view_delegate_capi.h (cef_view_delegate_t)
  TCefViewDelegate = record
    base                        : TCefBaseRefCounted;
    get_preferred_size          : function(self: PCefViewDelegate; view: PCefView): TCefSize; stdcall;
    get_minimum_size            : function(self: PCefViewDelegate; view: PCefView): TCefSize; stdcall;
    get_maximum_size            : function(self: PCefViewDelegate; view: PCefView): TCefSize; stdcall;
    get_height_for_width        : function(self: PCefViewDelegate; view: PCefView; width: Integer): Integer; stdcall;
    on_parent_view_changed      : procedure(self: PCefViewDelegate; view: PCefView; added: Integer; parent: PCefView); stdcall;
    on_child_view_changed       : procedure(self: PCefViewDelegate; view: PCefView; added: Integer; child: PCefView); stdcall;
    on_focus                    : procedure(self: PCefViewDelegate; view: PCefView); stdcall;
    on_blur                     : procedure(self: PCefViewDelegate; view: PCefView); stdcall;
  end;

  // /include/capi/views/cef_textfield_capi.h (cef_textfield_t)
  TCefTextfield = record
    base                           : TCefView;
    set_password_input             : procedure(self: PCefTextfield; password_input: Integer); stdcall;
    is_password_input              : function(self: PCefTextfield): Integer; stdcall;
    set_read_only                  : procedure(self: PCefTextfield; read_only: Integer); stdcall;
    is_read_only                   : function(self: PCefTextfield): Integer; stdcall;
    get_text                       : function(self: PCefTextfield): PCefStringUserFree; stdcall;
    set_text                       : procedure(self: PCefTextfield; const text: PCefString); stdcall;
    append_text                    : procedure(self: PCefTextfield; const text: PCefString); stdcall;
    insert_or_replace_text         : procedure(self: PCefTextfield; const text: PCefString); stdcall;
    has_selection                  : function(self: PCefTextfield): Integer; stdcall;
    get_selected_text              : function(self: PCefTextfield): PCefStringUserFree; stdcall;
    select_all                     : procedure(self: PCefTextfield; reversed: Integer); stdcall;
    clear_selection                : procedure(self: PCefTextfield); stdcall;
    get_selected_range             : function(self: PCefTextfield): TCefRange; stdcall;
    select_range                   : procedure(self: PCefTextfield; const range: PCefRange); stdcall;
    get_cursor_position            : function(self: PCefTextfield): NativeUInt; stdcall;
    set_text_color                 : procedure(self: PCefTextfield; color: TCefColor); stdcall;
    get_text_color                 : function(self: PCefTextfield): TCefColor; stdcall;
    set_selection_text_color       : procedure(self: PCefTextfield; color: TCefColor); stdcall;
    get_selection_text_color       : function(self: PCefTextfield): TCefColor; stdcall;
    set_selection_background_color : procedure(self: PCefTextfield; color: TCefColor); stdcall;
    get_selection_background_color : function(self: PCefTextfield): TCefColor; stdcall;
    set_font_list                  : procedure(self: PCefTextfield; const font_list: PCefString); stdcall;
    apply_text_color               : procedure(self: PCefTextfield; color: TCefColor; const range: PCefRange); stdcall;
    apply_text_style               : procedure(self: PCefTextfield; style: TCefTextStyle; add: Integer; const range: PCefRange); stdcall;
    is_command_enabled             : function(self: PCefTextfield; command_id: TCefTextFieldCommands): Integer; stdcall;
    execute_command                : procedure(self: PCefTextfield; command_id: TCefTextFieldCommands); stdcall;
    clear_edit_history             : procedure(self: PCefTextfield); stdcall;
    set_placeholder_text           : procedure(self: PCefTextfield; const text: PCefString); stdcall;
    get_placeholder_text           : function(self: PCefTextfield): PCefStringUserFree; stdcall;
    set_placeholder_text_color     : procedure(self: PCefTextfield; color: TCefColor); stdcall;
    set_accessible_name            : procedure(self: PCefTextfield; const name: PCefString); stdcall;
  end;

  // /include/capi/views/cef_textfield_delegate_capi.h (cef_textfield_delegate_t)
  TCefTextfieldDelegate = record
    base                           : TCefViewDelegate;
    on_key_event                   : function(self: PCefTextfieldDelegate; textfield: PCefTextfield; const event: PCefKeyEvent): Integer; stdcall;
    on_after_user_action           : procedure(self: PCefTextfieldDelegate; textfield: PCefTextfield); stdcall;
  end;

  // /include/capi/views/cef_scroll_view_capi.h (cef_scroll_view_t)
  TCefScrollView = record
    base                            : TCefView;
    set_content_view                : procedure(self: PCefScrollView; view: PCefView); stdcall;
    get_content_view                : function(self: PCefScrollView): PCefView; stdcall;
    get_visible_content_rect        : function(self: PCefScrollView): TCefRect; stdcall;
    has_horizontal_scrollbar        : function(self: PCefScrollView): Integer; stdcall;
    get_horizontal_scrollbar_height : function(self: PCefScrollView): Integer; stdcall;
    has_vertical_scrollbar          : function(self: PCefScrollView): Integer; stdcall;
    get_vertical_scrollbar_width    : function(self: PCefScrollView): Integer; stdcall;
  end;

  // /include/capi/views/cef_panel_capi.h (cef_panel_t)
  TCefPanel = record
    base                            : TCefView;
    as_window                       : function(self: PCefPanel): PCefWindow; stdcall;
    set_to_fill_layout              : function(self: PCefPanel): PCefFillLayout; stdcall;
    set_to_box_layout               : function(self: PCefPanel; const settings: PCefBoxLayoutSettings): PCefBoxLayout; stdcall;
    get_layout                      : function(self: PCefPanel): PCefLayout; stdcall;
    layout                          : procedure(self: PCefPanel); stdcall;
    add_child_view                  : procedure(self: PCefPanel; view: PCefView); stdcall;
    add_child_view_at               : procedure(self: PCefPanel; view: PCefView; index: Integer); stdcall;
    reorder_child_view              : procedure(self: PCefPanel; view: PCefView; index: Integer); stdcall;
    remove_child_view               : procedure(self: PCefPanel; view: PCefView); stdcall;
    remove_all_child_views          : procedure(self: PCefPanel); stdcall;
    get_child_view_count            : function(self: PCefPanel): NativeUInt; stdcall;
    get_child_view_at               : function(self: PCefPanel; index: Integer): PCefView; stdcall;
  end;

  // /include/capi/views/cef_panel_delegate_capi.h (cef_panel_delegate_t)
  TCefPanelDelegate = record
    base                            : TCefViewDelegate;
  end;

  // /include/capi/views/cef_browser_view_capi.h (cef_browser_view_t)
  TCefBrowserView = record
    base                            : TCefView;
    get_browser                     : function(self: PCefBrowserView): PCefBrowser; stdcall;
    set_prefer_accelerators         : procedure(self: PCefBrowserView; prefer_accelerators: Integer); stdcall;
  end;

  // /include/capi/views/cef_browser_view_delegate_capi.h (cef_browser_view_delegate_t)
  TCefBrowserViewDelegate = record
    base                                : TCefViewDelegate;
    on_browser_created                  : procedure(self: PCefBrowserViewDelegate; browser_view: PCefBrowserView; browser: PCefBrowser); stdcall;
    on_browser_destroyed                : procedure(self: PCefBrowserViewDelegate; browser_view: PCefBrowserView; browser: PCefBrowser); stdcall;
    get_delegate_for_popup_browser_view : function(self: PCefBrowserViewDelegate; browser_view: PCefBrowserView; const settings: PCefBrowserSettings; client: PCefClient; is_devtools: Integer): PCefBrowserViewDelegate; stdcall;
    on_popup_browser_view_created       : function(self: PCefBrowserViewDelegate; browser_view, popup_browser_view: PCefBrowserView; is_devtools: Integer): Integer; stdcall;
  end;

  // /include/capi/views/cef_button_capi.h (cef_button_t)
  TCefButton = record
    base                            : TCefView;
    as_label_button                 : function(self: PCefButton): PCefLabelButton; stdcall;
    set_state                       : procedure(self: PCefButton; state: TCefButtonState); stdcall;
    get_state                       : function(self: PCefButton): TCefButtonState; stdcall;
    set_ink_drop_enabled            : procedure(self: PCefButton; enabled: Integer); stdcall;
    set_tooltip_text                : procedure(self: PCefButton; const tooltip_text: PCefString); stdcall;
    set_accessible_name             : procedure(self: PCefButton; const name: PCefString); stdcall;
  end;

  // /include/capi/views/cef_button_delegate_capi.h (cef_button_delegate_t)
  TCefButtonDelegate = record
    base                            : TCefViewDelegate;
    on_button_pressed               : procedure(self: PCefButtonDelegate; button: PCefButton); stdcall;
    on_button_state_changed         : procedure(self: PCefButtonDelegate; button: PCefButton); stdcall;
  end;

  // /include/capi/views/cef_label_button_capi.h (cef_label_button_t)
  TCefLabelButton = record
    base                            : TCefButton;
    as_menu_button                  : function(self: PCefLabelButton): PCefMenuButton; stdcall;
    set_text                        : procedure(self: PCefLabelButton; const text: PCefString); stdcall;
    get_text                        : function(self: PCefLabelButton): PCefStringUserFree; stdcall;
    set_image                       : procedure(self: PCefLabelButton; button_state: TCefButtonState; image: PCefImage); stdcall;
    get_image                       : function(self: PCefLabelButton; button_state: TCefButtonState): PCefImage; stdcall;
    set_text_color                  : procedure(self: PCefLabelButton; for_state: TCefButtonState; color: TCefColor); stdcall;
    set_enabled_text_colors         : procedure(self: PCefLabelButton; color: TCefColor); stdcall;
    set_font_list                   : procedure(self: PCefLabelButton; const font_list: PCefString); stdcall;
    set_horizontal_alignment        : procedure(self: PCefLabelButton; alignment: TCefHorizontalAlignment); stdcall;
    set_minimum_size                : procedure(self: PCefLabelButton; const size: PCefSize); stdcall;
    set_maximum_size                : procedure(self: PCefLabelButton; const size: PCefSize); stdcall;
  end;

  // /include/capi/views/cef_menu_button_capi.h (cef_menu_button_t)
  TCefMenuButton = record
    base                            : TCefLabelButton;
    show_menu                       : procedure(self: PCefMenuButton; menu_model: PCefMenuModel; const screen_point: PCefPoint; anchor_position: TCefMenuAnchorPosition); stdcall;
    trigger_menu                    : procedure(self: PCefMenuButton); stdcall;
  end;

  // /include/capi/views/cef_menu_button_delegate_capi.h (cef_menu_button_pressed_lock_t)
  TCefMenuButtonPressedLock = record
    base                    : TCefBaseRefCounted;
  end;

  // /include/capi/views/cef_menu_button_delegate_capi.h (cef_menu_button_delegate_t)
  TCefMenuButtonDelegate = record
    base                    : TCefButtonDelegate;
    on_menu_button_pressed  : procedure(self: PCefMenuButtonDelegate; menu_button: PCefMenuButton; const screen_point: PCefPoint; button_pressed_lock: PCefMenuButtonPressedLock); stdcall;
  end;

  // /include/capi/views/cef_window_capi.h (cef_window_t)
  TCefWindow = record
    base                             : TCefPanel;
    show                             : procedure(self: PCefWindow); stdcall;
    hide                             : procedure(self: PCefWindow); stdcall;
    center_window                    : procedure(self: PCefWindow; const size: PCefSize); stdcall;
    close                            : procedure(self: PCefWindow); stdcall;
    is_closed                        : function(self: PCefWindow): Integer; stdcall;
    activate                         : procedure(self: PCefWindow); stdcall;
    deactivate                       : procedure(self: PCefWindow); stdcall;
    is_active                        : function(self: PCefWindow): Integer; stdcall;
    bring_to_top                     : procedure(self: PCefWindow); stdcall;
    set_always_on_top                : procedure(self: PCefWindow; on_top: Integer); stdcall;
    is_always_on_top                 : function(self: PCefWindow): Integer; stdcall;
    maximize                         : procedure(self: PCefWindow); stdcall;
    minimize                         : procedure(self: PCefWindow); stdcall;
    restore                          : procedure(self: PCefWindow); stdcall;
    set_fullscreen                   : procedure(self: PCefWindow; fullscreen: Integer); stdcall;
    is_maximized                     : function(self: PCefWindow): Integer; stdcall;
    is_minimized                     : function(self: PCefWindow): Integer; stdcall;
    is_fullscreen                    : function(self: PCefWindow): Integer; stdcall;
    set_title                        : procedure(self: PCefWindow; const title: PCefString); stdcall;
    get_title                        : function(self: PCefWindow): PCefStringUserFree; stdcall;
    set_window_icon                  : procedure(self: PCefWindow; image: PCefImage); stdcall;
    get_window_icon                  : function(self: PCefWindow): PCefImage; stdcall;
    set_window_app_icon              : procedure(self: PCefWindow; image: PCefImage); stdcall;
    get_window_app_icon              : function(self: PCefWindow): PCefImage; stdcall;
    show_menu                        : procedure(self: PCefWindow; menu_model: PCefMenuModel; const screen_point: PCefPoint; anchor_position : TCefMenuAnchorPosition); stdcall;
    cancel_menu                      : procedure(self: PCefWindow); stdcall;
    get_display                      : function(self: PCefWindow): PCefDisplay; stdcall;
    get_client_area_bounds_in_screen : function(self: PCefWindow): TCefRect; stdcall;
    set_draggable_regions            : procedure(self: PCefWindow; regionsCount: NativeUInt; const regions: PCefDraggableRegionArray); stdcall;
    get_window_handle                : function(self: PCefWindow): TCefWindowHandle; stdcall;
    send_key_press                   : procedure(self: PCefWindow; key_code: Integer; event_flags: cardinal); stdcall;
    send_mouse_move                  : procedure(self: PCefWindow; screen_x, screen_y: Integer); stdcall;
    send_mouse_events                : procedure(self: PCefWindow; button: TCefMouseButtonType; mouse_down, mouse_up: Integer); stdcall;
    set_accelerator                  : procedure(self: PCefWindow; command_id, key_code, shift_pressed, ctrl_pressed, alt_pressed: Integer); stdcall;
    remove_accelerator               : procedure(self: PCefWindow; command_id: Integer); stdcall;
    remove_all_accelerators          : procedure(self: PCefWindow); stdcall;
  end;

  // /include/capi/views/cef_window_delegate_capi.h (cef_window_delegate_t)
  TCefWindowDelegate = record
    base                             : TCefPanelDelegate;
    on_window_created                : procedure(self: PCefWindowDelegate; window: PCefWindow); stdcall;
    on_window_destroyed              : procedure(self: PCefWindowDelegate; window: PCefWindow); stdcall;
    get_parent_window                : function(self: PCefWindowDelegate; window: PCefWindow; is_menu, can_activate_menu: PInteger): PCefWindow; stdcall;
    get_initial_bounds               : function(self: PCefWindowDelegate; window: PCefWindow): TCefRect; stdcall;
    is_frameless                     : function(self: PCefWindowDelegate; window: PCefWindow): Integer; stdcall;
    can_resize                       : function(self: PCefWindowDelegate; window: PCefWindow): Integer; stdcall;
    can_maximize                     : function(self: PCefWindowDelegate; window: PCefWindow): Integer; stdcall;
    can_minimize                     : function(self: PCefWindowDelegate; window: PCefWindow): Integer; stdcall;
    can_close                        : function(self: PCefWindowDelegate; window: PCefWindow): Integer; stdcall;
    on_accelerator                   : function(self: PCefWindowDelegate; window: PCefWindow; command_id: Integer): Integer; stdcall;
    on_key_event                     : function(self: PCefWindowDelegate; window: PCefWindow; const event: PCefKeyEvent): Integer; stdcall;
  end;

implementation

end.
