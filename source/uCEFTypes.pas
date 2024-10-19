unit uCEFTypes;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
    {$IFDEF MSWINDOWS}
      WinApi.Windows,
    {$ELSE}
      System.Types, {$IFDEF LINUX}uCEFLinuxTypes,{$ENDIF}
    {$ENDIF}
    System.Math,
  {$ELSE}
    {$IFDEF FPC}{$IFDEF LINUX}xlib, ctypes,{$ENDIF}{$ENDIF}
    {$IFDEF MSWINDOWS}Windows,{$ENDIF} Math,
  {$ENDIF}
  uCEFConstants;

type
  PCefStringWide = ^TCefStringWide;
  PCefDictionaryValue = ^TCefDictionaryValue;
  PCefListValue = ^TCefListValue;
  PCefBrowser = ^TCefBrowser;
  PCefValue = ^TCefValue;
  PCefBinaryValue = ^TCefBinaryValue;
  PCefSchemeRegistrar = ^TCefSchemeRegistrar;
  PCefPreferenceRegistrar = ^TCefPreferenceRegistrar;
  PCefPreferenceManager = ^TCefPreferenceManager;
  PCefCommandLine = ^TCefCommandLine;
  PCefCommandHandler = ^TCefCommandHandler;
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
  PCefRunQuickMenuCallback = ^TCefRunQuickMenuCallback;
  PCefAccessibilityHandler = ^TCefAccessibilityHandler;
  PCefFrame = ^TCefFrame;
  PCefFrameHandler = ^TCefFrameHandler;
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
  PCefTaskManager = ^TCefTaskManager;
  PCefv8Value = ^TCefv8Value;
  PCefBaseTime = ^TCefBaseTime;
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
  PCefUnresponsiveProcessCallback = ^TCefUnresponsiveProcessCallback;
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
  PCefAudioHandler = ^TCefAudioHandler;
  PCefAudioParameters = ^TCefAudioParameters;
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
  PCefMediaAccessCallback = ^TCefMediaAccessCallback;
  PCefMediaAccessHandler = ^TCefMediaAccessHandler;
  PCefPermissionHandler = ^TCefPermissionHandler;
  PCefSharedMemoryRegion = ^TCefSharedMemoryRegion;
  PCefSharedProcessMessageBuilder = ^TCefSharedProcessMessageBuilder;
  PCefPermissionPromptCallback = ^TCefPermissionPromptCallback;
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
  PCefTouchHandleState = ^TCefTouchHandleState;
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
  PCefOverlayController = ^TCefOverlayController;
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
  PCefAcceleratedPaintInfo = ^TCefAcceleratedPaintInfo;
  PCefLinuxWindowProperties = ^TCefLinuxWindowProperties;
  PCefTaskInfo = ^TCefTaskInfo;

{$IFDEF FPC}
  NativeInt   = PtrInt;
  NativeUInt  = PtrUInt;
  PNativeInt  = ^NativeInt;
  PNativeUInt = ^NativeUInt;
  /// <summary>
  /// String type used by CEF. ustring was created to use the same type in Delphi and Lazarus.
  /// </summary>
  ustring     = type UnicodeString;
  rbstring    = type AnsiString;
{$ELSE}
  {$IFNDEF DELPHI12_UP}
    NativeUInt  = Cardinal;
    PNativeUInt = ^NativeUInt;
    NativeInt   = Integer;
    uint16      = Word;
    /// <summary>
    /// String type used by CEF. ustring was created to use the same type in Delphi and Lazarus.
    /// </summary>
    ustring     = type WideString;
    rbstring    = type AnsiString;
    {$IFNDEF DELPHI7_UP}
    uint64     = type int64;
    PPAnsiChar = array of PChar;
    {$ENDIF}
  {$ELSE}
    /// <summary>
    /// String type used by CEF. ustring was created to use the same type in Delphi and Lazarus.
    /// </summary>
    ustring     = type string;
    rbstring    = type RawByteString;
    {$IFNDEF DELPHI15_UP}
      NativeUInt  = Cardinal;
      PNativeUInt = ^NativeUInt;
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

  {$IFDEF MSWINDOWS}
  /// <summary>
  /// Native Window handle.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types_win.h">CEF source file: /include/internal/cef_types_win.h (cef_window_handle_t)</see></para>
  /// </remarks>
  TCefWindowHandle = type HWND;
  /// <summary>
  /// Native Cursor handle.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types_win.h">CEF source file: /include/internal/cef_types_win.h (cef_cursor_handle_t)</see></para>
  /// </remarks>
  TCefCursorHandle = type HCURSOR;
  /// <summary>
  /// Native event handle.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types_win.h">CEF source file: /include/internal/cef_types_win.h (cef_event_handle_t)</see></para>
  /// </remarks>
  TCefEventHandle  = type PMsg;
  {$IFNDEF FPC}
  /// <summary>
  /// Missing HANDLE declaration.
  /// </summary>
  HANDLE           = type NativeUInt;
  {$ENDIF}
  /// <summary>
  /// Native texture handle.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types_win.h">CEF source file: /include/internal/cef_types_win.h (cef_shared_texture_handle_t)</see></para>
  /// </remarks>
  TCefSharedTextureHandle  = type HANDLE;
  {$ENDIF}

  {$IFDEF MACOSX}
  /// <summary>
  /// Native Window handle.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types_mac.h">CEF source file: /include/internal/cef_types_mac.h (cef_window_handle_t)</see></para>
  /// </remarks>
  TCefWindowHandle = type {$IFDEF FPC}PtrUInt{$ELSE}Pointer{$ENDIF};
  /// <summary>
  /// Native Cursor handle.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types_mac.h">CEF source file: /include/internal/cef_types_mac.h (cef_cursor_handle_t)</see></para>
  /// </remarks>
  TCefCursorHandle = type {$IFDEF FPC}PtrUInt{$ELSE}Pointer{$ENDIF};
  /// <summary>
  /// Native event handle.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types_mac.h">CEF source file: /include/internal/cef_types_mac.h (cef_event_handle_t)</see></para>
  /// </remarks>
  TCefEventHandle  = type {$IFDEF FPC}PtrUInt{$ELSE}Pointer{$ENDIF};
  /// <summary>
  /// Native texture handle.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types_win.h">CEF source file: /include/internal/cef_types_mac.h (cef_shared_texture_handle_t)</see></para>
  /// </remarks>
  TCefSharedTextureHandle  = {$IFDEF FPC}type PtrUInt{$ELSE}Pointer{$ENDIF};
  {$ENDIF}

  {$IFDEF LINUX}
  /// <summary>
  /// Native Window handle.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types_linux.h">CEF source file: /include/internal/cef_types_linux.h (cef_window_handle_t)</see></para>
  /// </remarks>
  TCefWindowHandle = type {$IFDEF FPC}culong{$ELSE}LongWord{$ENDIF};
  /// <summary>
  /// Native Cursor handle.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types_linux.h">CEF source file: /include/internal/cef_types_linux.h (cef_cursor_handle_t)</see></para>
  /// </remarks>
  TCefCursorHandle = type {$IFDEF FPC}culong{$ELSE}LongWord{$ENDIF};
  /// <summary>
  /// Native event handle.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types_linux.h">CEF source file: /include/internal/cef_types_linux.h (cef_event_handle_t)</see></para>
  /// </remarks>
  TCefEventHandle = type PXEvent;
  {$ENDIF}

  {$IFDEF ANDROID}
  /// <summary>
  /// Native Window handle.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types_win.h">CEF source file: /include/internal/cef_types_win.h (cef_window_handle_t)</see></para>
  /// </remarks>
  TCefWindowHandle = type UIntPtr;
  /// <summary>
  /// Native Cursor handle.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types_win.h">CEF source file: /include/internal/cef_types_win.h (cef_cursor_handle_t)</see></para>
  /// </remarks>
  TCefCursorHandle = type UIntPtr;
  /// <summary>
  /// Native event handle.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types_win.h">CEF source file: /include/internal/cef_types_win.h (cef_event_handle_t)</see></para>
  /// </remarks>
  TCefEventHandle  = type UIntPtr;
  {$ENDIF}

  /// <summary>
  /// Describes how to interpret the components of a pixel.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types_color.h">CEF source file: /include/internal/cef_types_color.h (cef_color_type_t)</see></para>
  /// </remarks>
  TCefColorType = (
    /// <summary>
    /// RGBA with 8 bits per pixel (32bits total).
    /// </summary>
    CEF_COLOR_TYPE_RGBA_8888,
    /// <summary>
    /// BGRA with 8 bits per pixel (32bits total).
    /// </summary>
    CEF_COLOR_TYPE_BGRA_8888
  );

  {$IFDEF LINUX}
  /// <summary>
  /// Structure containing the plane information of the shared texture.
  /// Sync with native_pixmap_handle.h
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types_linux.h">CEF source file: /include/internal/cef_types_linux.h (cef_accelerated_paint_native_pixmap_plane_t)</see></para>
  /// </remarks>
  TCefAcceleratedPaintNativePixmapPlaneInfo = record
    /// <summary>
    /// The strides in bytes to be used when accessing the buffers via
    /// a memory mapping. One per plane per entry.
    /// </summary>
    stride  : Cardinal;
    /// <summary>
    /// The offsets in bytes to be used when accessing the buffers via
    /// a memory mapping. One per plane per entry.
    /// </summary>
    offset  : uint64;
    /// <summary>
    /// Size in bytes of the plane is necessary to map the buffers.
    /// </summary>
    size    : uint64;
    /// <summary>
    /// File descriptor for the underlying memory object (usually dmabuf).
    /// </summary>
    fd      : integer;
  end;
  {$ENDIF}

  /// <summary>
  /// Structure containing shared texture information for the OnAcceleratedPaint
  /// callback. Resources will be released to the underlying pool for reuse when
  /// the callback returns from client code.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types_win.h">CEF source file: /include/internal/cef_types_win.h (cef_accelerated_paint_info_t)</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types_mac.h">CEF source file: /include/internal/cef_types_mac.h (cef_accelerated_paint_info_t)</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types_linux.h">CEF source file: /include/internal/cef_types_linux.h (cef_accelerated_paint_info_t)</see></para>
  /// </remarks>
  TCefAcceleratedPaintInfo = record
    {$IFDEF MSWINDOWS}
    /// <summary>
    /// Handle for the shared texture. The shared texture is instantiated
    /// without a keyed mutex.
    /// </summary>
    shared_texture_handle   : TCefSharedTextureHandle;
    /// <summary>
    /// The pixel format of the texture.
    /// </summary>
    format                  : TCefColorType;
    {$ENDIF}

    {$IFDEF MACOSX}
    /// <summary>
    /// Handle for the shared texture IOSurface.
    /// </summary>
    shared_texture_io_surface : TCefSharedTextureHandle;
    /// <summary>
    /// The pixel format of the texture.
    /// </summary>
    format                    : TCefColorType;
    {$ENDIF}

    {$IFDEF LINUX}
    /// <summary>
    /// Planes of the shared texture, usually file descriptors of dmabufs.
    /// </summary>
    planes                  : array [0..pred(CEF_KACCELERATEDPAINTMAXPLANES)] of TCefAcceleratedPaintNativePixmapPlaneInfo;
    /// <summary>
    /// Plane count.
    /// </summary>
    plane_count             : integer;
    /// <summary>
    /// Modifier could be used with EGL driver.
    /// </summary>
    modifier                : uint64;
    /// <summary>
    /// The pixel format of the texture.
    /// </summary>
    format                  : TCefColorType;
    {$ENDIF}
  end;

  /// <summary>
  /// Platform thread ID.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_thread_internal.h">CEF source file: /include/internal/cef_thread_internal.h (cef_platform_thread_id_t)</see></para>
  /// </remarks>
  TCefPlatformThreadId             = type DWORD;

  /// <summary>
  /// Platform thread handle.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_thread_internal.h">CEF source file: /include/internal/cef_thread_internal.h (cef_platform_thread_handle_t)</see></para>
  /// </remarks>
  TCefPlatformThreadHandle         = type DWORD;

  /// <summary>
  /// Transition type for a request. Made up of one source value and 0 or more
  /// qualifiers.
  /// </summary>
  /// <remarks>
  /// <para>See the uCEFConstants unit for all possible values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_transition_type_t)</see></para>
  /// </remarks>
  TCefTransitionType               = type Cardinal;

  /// <summary>
  /// 32-bit ARGB color value, not premultiplied. The color components are always
  /// in a known order. Equivalent to the SkColor type.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_color_t)</see></para>
  /// </remarks>
  TCefColor                        = type Cardinal;

  /// <summary>
  /// Supported error code values.
  /// <code>
  /// Ranges:
  ///     0- 99 System related errors
  ///   100-199 Connection related errors
  ///   200-299 Certificate errors
  ///   300-399 HTTP errors
  ///   400-499 Cache errors
  ///   500-599 ?
  ///   600-699 <Obsolete: FTP errors>
  ///   700-799 Certificate manager errors
  ///   800-899 DNS resolver errors
  /// </code>
  /// </summary>
  /// <remarks>
  /// <para>See the uCEFConstants unit for all possible values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_errorcode_t)</see></para>
  /// <para><see href="https://chromium.googlesource.com/chromium/src/+/master/net/base/net_error_list.h">For the complete list of error values see include/base/internal/cef_net_error_list.h which includes this Chromium source file /net/base/net_error_list.h</see></para>
  /// </remarks>
  TCefErrorCode                    = type Integer;

  /// <summary>
  /// Supported certificate status code values. See net\cert\cert_status_flags.h
  /// for more information. CERT_STATUS_NONE is new in CEF because we use an
  /// enum while cert_status_flags.h uses a typedef and static const variables.
  /// </summary>
  /// <remarks>
  /// <para>See the uCEFConstants unit for all possible values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_cert_status_t)</see></para>
  /// </remarks>
  TCefCertStatus                   = type Integer;

  /// <summary>
  /// Supported SSL version values. 
  /// </summary>
  /// <remarks>
  /// <para>See the uCEFConstants unit for all possible values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_ssl_version_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:net/ssl/ssl_connection_status_flags.h">See net/ssl/ssl_connection_status_flags.h for more information.</see></para>
  /// </remarks>
  TCefSSLVersion                   = type integer;

  /// <summary>
  /// CEF string maps are a set of key/value string pairs.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_string_list.h">CEF source file: /include/internal/cef_string_list.h (cef_string_list_t)</see></para>
  /// </remarks>
  TCefStringList                   = type Pointer;

  /// <summary>
  /// CEF string maps are a set of key/value string pairs.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_string_map.h">CEF source file: /include/internal/cef_string_map.h (cef_string_map_t)</see></para>
  /// </remarks>
  TCefStringMap                    = type Pointer;

  /// <summary>
  /// CEF string multimaps are a set of key/value string pairs.
  /// More than one value can be assigned to a single key.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_string_multimap.h">CEF source file: /include/internal/cef_string_multimap.h (cef_string_multimap_t)</see></para>
  /// </remarks>
  TCefStringMultimap               = type Pointer;

  /// <summary>
  /// URI unescape rules passed to CefURIDecode().
  /// </summary>
  /// <remarks>
  /// <para>See the uCEFConstants unit for all possible values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_uri_unescape_rule_t)</see></para>
  /// </remarks>
  TCefUriUnescapeRule              = type Integer;

  /// <summary>
  /// DOM event category flags.
  /// </summary>
  /// <remarks>
  /// <para>See the uCEFConstants unit for all possible values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_dom_event_category_t)</see></para>
  /// </remarks>
  TCefDomEventCategory             = type Integer;

  /// <summary>
  /// Supported event bit flags.
  /// </summary>
  /// <remarks>
  /// <para>See the uCEFConstants unit for all possible values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_event_flags_t)</see></para>
  /// </remarks>
  TCefEventFlags                   = type Cardinal;

  /// <summary>
  /// "Verb" of a drag-and-drop operation as negotiated between the source and
  /// destination. These constants match their equivalents in WebCore's
  /// DragActions.h and should not be renumbered.
  /// </summary>
  /// <remarks>
  /// <para>See the uCEFConstants unit for all possible values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_drag_operations_mask_t)</see></para>
  /// </remarks>
  TCefDragOperations               = type Cardinal;
  TCefDragOperation                = type Cardinal;

  /// <summary>
  /// V8 property attribute values.
  /// </summary>
  /// <remarks>
  /// <para>See the uCEFConstants unit for all possible values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_v8_propertyattribute_t)</see></para>
  /// </remarks>
  TCefV8PropertyAttributes         = type Cardinal;

  /// <summary>
  /// Flags used to customize the behavior of CefURLRequest.
  /// </summary>
  /// <remarks>
  /// <para>See the uCEFConstants unit for all possible values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_urlrequest_flags_t)</see></para>
  /// </remarks>
  TCefUrlRequestFlags              = type Cardinal;

  /// <summary>
  /// Supported context menu type flags.
  /// </summary>
  /// <remarks>
  /// <para>See the uCEFConstants unit for all possible values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_context_menu_type_flags_t)</see></para>
  /// </remarks>
  TCefContextMenuTypeFlags         = type Cardinal;

  /// <summary>
  /// Supported context menu media state bit flags. These constants match their
  /// equivalents in Chromium's ContextMenuData::MediaFlags and should not be
  /// renumbered.
  /// </summary>
  /// <remarks>
  /// <para>See the uCEFConstants unit for all possible values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_context_menu_media_state_flags_t)</see></para>
  /// </remarks>
  TCefContextMenuMediaStateFlags   = type Cardinal;

  /// <summary>
  /// Supported context menu edit state bit flags. These constants match their
  /// equivalents in Chromium's ContextMenuDataEditFlags and should not be
  /// renumbered.
  /// </summary>
  /// <remarks>
  /// <para>See the uCEFConstants unit for all possible values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_context_menu_edit_state_flags_t)</see></para>
  /// </remarks>
  TCefContextMenuEditStateFlags    = type Cardinal;

  /// <summary>
  /// Options that can be passed to CefWriteJSON.
  /// </summary>
  /// <remarks>
  /// <para>See the uCEFConstants unit for all possible values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_json_writer_options_t)</see></para>
  /// </remarks>
  TCefJsonWriterOptions            = type Cardinal;

  /// <summary>
  /// Supported SSL content status flags. See content/public/common/ssl_status.h
  /// for more information.
  /// </summary>
  /// <remarks>
  /// <para>See the uCEFConstants unit for all possible values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_ssl_content_status_t)</see></para>
  /// </remarks>
  TCefSSLContentStatus             = type Cardinal;

  /// <summary>
  /// Log severity levels.
  /// </summary>
  /// <remarks>
  /// <para>See the uCEFConstants unit for all possible values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_log_severity_t)</see></para>
  /// </remarks>
  TCefLogSeverity                  = type Cardinal;

  /// <summary>
  /// Supported file dialog modes.
  /// </summary>
  /// <remarks>
  /// <para>See the uCEFConstants unit for all possible values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_file_dialog_mode_t)</see></para>
  /// </remarks>
  TCefFileDialogMode               = type Cardinal;

  /// <summary>
  /// Print job duplex mode values.
  /// </summary>
  /// <remarks>
  /// <para>See the uCEFConstants unit for all possible values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_duplex_mode_t)</see></para>
  /// </remarks>
  TCefDuplexMode                   = type Integer;

  /// <summary>
  /// Configuration options for registering a custom scheme.
  /// These values are used when calling AddCustomScheme.
  /// </summary>
  /// <remarks>
  /// <para>See the uCEFConstants unit for all possible values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_scheme_options_t)</see></para>
  /// </remarks>
  TCefSchemeOptions                = type Integer;

  /// <summary>
  /// Result codes for ICefMediaRouter.CreateRoute. Should be kept in sync with
  /// Chromium's media_router::mojom::RouteRequestResultCode type.
  /// </summary>
  /// <remarks>
  /// <para>See the uCEFConstants unit for all possible values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_media_route_create_result_t)</see></para>
  /// </remarks>
  TCefMediaRouterCreateResult      = type Integer;

  /// <summary>
  /// Cookie priority values.
  /// </summary>
  /// <remarks>
  /// <para>See the uCEFConstants unit for all possible values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_cookie_priority_t)</see></para>
  /// </remarks>
  TCefCookiePriority               = type Integer;

  /// <summary>
  /// Represents commands available to TextField.
  /// </summary>
  /// <remarks>
  /// <para>See the uCEFConstants unit for all possible values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_text_field_commands_t)</see></para>
  /// </remarks>
  TCefTextFieldCommands            = type Integer;

  /// <summary>
  /// Chrome toolbar types.
  /// </summary>
  /// <remarks>
  /// <para>See the uCEFConstants unit for all possible values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_chrome_toolbar_type_t)</see></para>
  /// </remarks>
  TCefChromeToolbarType            = type Integer;

  /// <summary>
  /// Docking modes supported by ICefWindow.AddOverlay.
  /// </summary>
  /// <remarks>
  /// <para>See the uCEFConstants unit for all possible values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_docking_mode_t)</see></para>
  /// </remarks>
  TCefDockingMode                  = type Integer;

  /// <summary>
  /// Show states supported by ICefWindowDelegate.GetInitialShowState.
  /// </summary>
  /// <remarks>
  /// <para>See the uCEFConstants unit for all possible values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_show_state_t)</see></para>
  /// </remarks>
  TCefShowState                    = type Integer;

  /// <summary>
  /// Supported quick menu state bit flags.
  /// </summary>
  /// <remarks>
  /// <para>See the uCEFConstants unit for all possible values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_quick_menu_edit_state_flags_t)</see></para>
  /// </remarks>
  TCefQuickMenuEditStateFlags      = type Integer;

  /// <summary>
  /// Values indicating what state of the touch handle is set.
  /// </summary>
  /// <remarks>
  /// <para>See the uCEFConstants unit for all possible values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_touch_handle_state_flags_t)</see></para>
  /// </remarks>
  TCefTouchHandleStateFlags        = type Integer;

  /// <summary>
  /// Media access permissions used by OnRequestMediaAccessPermission.
  /// </summary>
  /// <remarks>
  /// <para>See the uCEFConstants unit for all possible values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_media_access_permission_types_t)</see></para>
  /// </remarks>
  TCefMediaAccessPermissionTypes   = type Integer;

  /// <summary>
  /// Permission types used with OnShowPermissionPrompt. Some types are
  /// platform-specific or only supported with Chrome style. Should be kept
  /// in sync with Chromium's permissions::RequestType type.
  /// </summary>
  /// <remarks>
  /// <para>See the uCEFConstants unit for all possible values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_permission_request_types_t)</see></para>
  /// </remarks>
  TCefPermissionRequestTypes       = type Integer;

  /// <summary>
  /// Download interrupt reasons. Should be kept in sync with
  /// Chromium's download::DownloadInterruptReason type.
  /// </summary>
  /// <remarks>
  /// <para>See the uCEFConstants unit for all possible values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_download_interrupt_reason_t)</see></para>
  /// </remarks>
  TCefDownloadInterruptReason      = type Integer;

  /// <summary>
  /// Supported menu IDs. Non-English translations can be provided for the
  /// IDS_MENU_* strings in ICefResourceBundleHandler.GetLocalizedString().
  /// </summary>
  /// <remarks>
  /// <para>See the uCEFConstants unit for all possible values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_menu_id_t)</see></para>
  /// </remarks>
  TCefMenuId                       = type Integer;

  /// <summary>
  /// Log items prepended to each log line.
  /// </summary>
  /// <remarks>
  /// <para>See the uCEFConstants unit for all possible values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_log_items_t)</see></para>
  /// </remarks>
  TCefLogItems                     = type Cardinal;

  /// <summary>
  /// Process result codes. This is not a comprehensive list, as result codes
  /// might also include platform-specific crash values (Posix signal or Windows
  /// hardware exception), or internal-only implementation values.
  /// </summary>
  /// <remarks>
  /// <para>See the uCEFConstants unit for all possible values.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resultcode_t)</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:content/public/common/result_codes.h">See Chromium's content::ResultCode type.</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:chrome/common/chrome_result_codes.h">See chrome::ResultCode type.</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:sandbox/win/src/sandbox_types.h">See sandbox::TerminationCodes type.</see></para>
  /// </remarks>
  TCefResultCode                   = type Integer;


  /// <summary>
  /// Array of byte. Needed only for backwards compatibility with old Delphi versions.
  /// </summary>
  TCefCustomByteArray = array of byte;

  /// <summary>
  /// Custom array of int64.
  /// </summary>
  TCefCustomInt64Array = array of int64;

  {$IFDEF MSWINDOWS}
  /// <summary>
  /// Record used with GetGlobalMemoryStatusEx to get the memory status.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://learn.microsoft.com/en-us/windows/win32/api/sysinfoapi/ns-sysinfoapi-memorystatusex">See the MEMORYSTATUSEX structure.</see></para>
  /// </remarks>
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
  LPMEMORYSTATUSEX = ^TMyMemoryStatusEx;

  /// <summary>
  /// Record used with RtlGetVersion to get the Windows version information.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdm/ns-wdm-_osversioninfoexw">See the OSVERSIONINFOEXW structure.</see></para>
  /// </remarks>
  TOSVersionInfoEx = record
    dwOSVersionInfoSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformId: DWORD;
    szCSDVersion: array[0..127] of WideChar;
    wServicePackMajor: WORD;
    wServicePackMinor: WORD;
    wSuiteMask: WORD;
    wProductType: BYTE;
    wReserved:BYTE;
  end;

  {$IFDEF DELPHI14_UP}
  /// <summary>
  /// Record used with GetSystemMetrics(SM_DIGITIZER) to get the digitizer properties.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://learn.microsoft.com/es-es/windows/win32/api/winuser/nf-winuser-getsystemmetrics">See the GetSystemMetrics function.</see></para>
  /// </remarks>
  TDigitizerStatus = record
    IntegratedTouch : boolean;
    ExternalTouch   : boolean;
    IntegratedPen   : boolean;
    ExternalPen     : boolean;
    MultiInput      : boolean;
    Ready           : boolean;
  end;
  {$ENDIF}
  {$ENDIF}

  PPSingle = ^PSingle;

  Char16  = WideChar;
  PChar16 = PWideChar;

  /// <summary>
  /// CEF wide string type definition. Whomever allocates |str| is responsible for
  /// providing an appropriate |dtor| implementation that will free the string in
  /// the same memory space. When reusing an existing string structure make sure
  /// to call |dtor| for the old value before assigning new |str| and |dtor|
  /// values. Static strings will have a NULL |dtor| value. Using the below
  /// functions if you want this managed for you.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_string_types.h">CEF source file: /include/internal/cef_string_types.h (cef_string_wide_t)</see></para>
  /// </remarks>
  TCefStringWide = record
    str    : PWideChar;
    length : NativeUInt;
    dtor   : procedure(str: PWideChar); stdcall;
  end;

  /// <summary>
  /// CEF utf8 string type definition. Whomever allocates |str| is responsible for
  /// providing an appropriate |dtor| implementation that will free the string in
  /// the same memory space. When reusing an existing string structure make sure
  /// to call |dtor| for the old value before assigning new |str| and |dtor|
  /// values. Static strings will have a NULL |dtor| value. Using the below
  /// functions if you want this managed for you.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_string_types.h">CEF source file: /include/internal/cef_string_types.h (cef_string_utf8_t)</see></para>
  /// </remarks>
  TCefStringUtf8 = record
    str    : PAnsiChar;
    length : NativeUInt;
    dtor   : procedure(str: PAnsiChar); stdcall;
  end;

  /// <summary>
  /// CEF utf16 string type definition. Whomever allocates |str| is responsible for
  /// providing an appropriate |dtor| implementation that will free the string in
  /// the same memory space. When reusing an existing string structure make sure
  /// to call |dtor| for the old value before assigning new |str| and |dtor|
  /// values. Static strings will have a NULL |dtor| value. Using the below
  /// functions if you want this managed for you.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_string_types.h">CEF source file: /include/internal/cef_string_types.h (cef_string_utf16_t)</see></para>
  /// </remarks>
  TCefStringUtf16 = record
    str    : PChar16;
    length : NativeUInt;
    dtor   : procedure(str: PChar16); stdcall;
  end;

  /// <summary>
  /// String record used by the CEF C API.
  /// The CEF interface is built with the UTF16 string type as the default.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_string.h">CEF source file: /include/internal/cef_string.h (cef_string_t)</see></para>
  /// </remarks>
  TCefString = TCefStringUtf16;
  PCefString = PCefStringUtf16;

  TCefStringUserFreeWide  = type TCefStringWide;
  TCefStringUserFreeUtf8  = type TCefStringUtf8;
  TCefStringUserFreeUtf16 = type TCefStringUtf16;

  TCefChar = Char16;
  PCefChar = PChar16;
  TCefStringUserFree = TCefStringUserFreeUtf16;
  PCefStringUserFree = PCefStringUserFreeUtf16;

  /// <summary>
  /// Record used by GetDLLVersion to get the DLL version information
  /// </summary>
  TFileVersionInfo = record
    MajorVer : uint16;
    MinorVer : uint16;
    Release  : uint16;
    Build    : uint16;
  end;

  /// <summary>
  /// <para>Action taken after the TChromium.Onclose event.</para>
  /// <para>cbaCancel : stop closing the browser.</para>
  /// <para>cbaClose  : continue closing the browser.</para>
  /// <para>cbaDelay  : stop closing the browser momentarily. Used when the application
  ///             needs to execute some custom processes before closing the
  ///             browser. This is usually needed to destroy a TCEFWindowParent
  ///             in the main thread before closing the browser.</para>
  /// </summary>
  TCefCloseBrowserAction = (cbaClose, cbaDelay, cbaCancel);

  /// <summary>
  /// Sub-process types of Chromium.
  /// </summary>
  TCefProcessType = (ptBrowser, ptRenderer, ptZygote, ptGPU, ptUtility, ptBroker, ptCrashpad, ptOther);

  /// <summary>
  /// Used in TChromium preferences to allow or block cookies.
  /// </summary>
  TCefCookiePref = (cpDefault, cpAllow, cpBlock);

  /// <summary>
  /// Used by TCefBrowserNavigationTask to navigate in the right CEF thread.
  /// </summary>
  TCefBrowserNavigation = (bnBack, bnForward, bnReload, bnReloadIgnoreCache, bnStopLoad);

  /// <summary>
  /// Status of TCefAplicationCore.
  /// </summary>
  TCefAplicationStatus = (asLoading,
                          asLoaded,
                          asInitialized,
                          asShuttingDown,
                          asUnloaded,
                          asErrorMissingFiles,
                          asErrorDLLVersion,
                          asErrorWindowsVersion,
                          asErrorLoadingLibrary,
                          asErrorInitializingLibrary,
                          asErrorExecutingProcess);

  /// <summary>
  /// Color mode in UI for platforms that support it.
  /// </summary>
  TCefUIColorMode = (
    /// <summary>
    /// System default.
    /// </summary>
    uicmSystemDefault,
    /// <summary>
    /// Forces light color mode in UI for platforms that support it.
    /// </summary>
    uicmForceLight,
    /// <summary>
    /// Forces dark color mode in UI for platforms that support it.
    /// </summary>
    uicmForceDark);

  /// <summary>
  /// Supported proxy schemes in Chromium.
  /// </summary>
  TCefProxyScheme = (psHTTP, psSOCKS4, psSOCKS5);

  /// <summary>
  /// Event type used by TChromiumCore.SimulateKeyEvent
  /// </summary>
  TSimulatedCefKeyEventType = (ketKeyDown,
                               ketKeyUp,
                               ketRawKeyDown,
                               ketChar);

  /// <summary>
  /// Storage types used by the Storage.clearDataForOrigin DevTools method in TChromiumCore.ClearDataForOrigin.
  /// </summary>
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

  /// <summary>
  /// Autoplay policy types used by TCefApplicationCore.AutoplayPolicy. See the --autoplay-policy switch.
  /// </summary>
  TCefAutoplayPolicy = (appDefault,
                        /// <summary>
                        /// Autoplay policy that requires a document user activation.
                        /// </summary>
                        appDocumentUserActivationRequired,
                        /// <summary>
                        /// Autoplay policy that does not require any user gesture.
                        /// </summary>
                        appNoUserGestureRequired,
                        /// <summary>
                        /// Autoplay policy to require a user gesture in order to play.
                        /// </summary>
                        appUserGestureRequired);

  /// <summary>
  /// WebRTC handling policy types used by TChromiumCore.WebRTCIPHandlingPolicy.
  /// </summary>
  TCefWebRTCHandlingPolicy = (
    /// <summary>
    /// WebRTC will use all available interfaces when searching for the best path.
    /// </summary>
    hpDefault,
    /// <summary>
    /// WebRTC will only use the interface connecting to the public Internet,
    /// but may connect using private IP addresses.
    /// </summary>
    hpDefaultPublicAndPrivateInterfaces,
    /// <summary>
    /// WebRTC will only use the interface connecting to the public Internet,
    /// and will not connect using private IP addresses.
    /// </summary>
    hpDefaultPublicInterfaceOnly,
    /// <summary>
    /// WebRTC will use TCP on the public-facing interface, and will only use
    /// UDP if supported by a configured proxy.
    /// </summary>
    hpDisableNonProxiedUDP
  );

  /// <summary>
  /// Values used by the --net-log-capture-mode command line switch.
  /// Sets the granularity of events to capture in the network log.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:content/browser/network_service_instance_impl.cc">network_service_instance_impl.cc</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:net/log/net_log_capture_mode.h">net_log_capture_mode.h</see></para>
  /// </remarks>
  TCefNetLogCaptureMode = (
    /// <summary>
    /// <para>Default logging level, which is expected to be light-weight and
    /// does best-effort stripping of privacy/security sensitive data.</para>
    /// <code>
    ///  * Includes most HTTP request/response headers, but strips cookies and
    ///    auth.
    ///  * Does not include the full bytes read/written to sockets.
    /// </code>
    /// </summary>
    nlcmDefault,
    /// <summary>
    /// <para>Logging level that includes everything from kDefault, plus sensitive data
    /// that it may have strippped.</para>
    /// <code>
    ///  * Includes cookies and authentication headers.
    ///  * Does not include the full bytes read/written to sockets.
    /// </code>
    /// </summary>
    nlcmIncludeSensitive,
    /// <summary>
    /// <para>Logging level that includes everything that is possible to be logged.</para>
    /// <code>
    ///  * Includes the actual bytes read/written to sockets
    ///  * Will result in large log files.
    /// </code>
    /// </summary>
    nlcmEverything
  );

  /// <summary>
  /// Values used by the battery saver mode state preference.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:components/performance_manager/public/user_tuning/prefs.h">components/performance_manager/public/user_tuning/prefs.h</see></para>
  /// </remarks>
  TCefBatterySaverModeState = (
    bsmsDisabled = 0,
    bsmsEnabledBelowThreshold = 1,
    bsmsEnabledOnBattery = 2,
    bsmsEnabled = 3,
    /// <summary>
    /// Custom value used to update the preferences only when there's a non-default value.
    /// </summary>
    bsmsDefault = 4
  );

  /// <summary>
  /// Values used by the high efficiency mode state preference.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:components/performance_manager/public/user_tuning/prefs.h">components/performance_manager/public/user_tuning/prefs.h</see></para>
  /// </remarks>
  TCefHighEfficiencyModeState = (
    kDisabled = 0,
    kEnabled = 1,
    kEnabledOnTimer = 2,
    /// <summary>
    /// Custom value used to update the preferences only when there's a non-default value.
    /// </summary>
    kDefault = 3
  );

  /// <summary>
  /// Used by TCEFFileDialogInfo.
  /// </summary>
  TCEFDialogType = (dtOpen, dtOpenMultiple, dtOpenFolder, dtSave);

  /// <summary>
  /// Used by TCefMediaSinkInfo and TCefMediaSourceInfo.
  /// </summary>
  TCefMediaType = (mtCast, mtDial, mtUnknown);

  /// <summary>
  /// Structure representing CefExecuteProcess arguments.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types_win.h">CEF source file: /include/internal/cef_types_win.h (cef_main_args_t)</see></para>
  /// </remarks>
  TCefMainArgs = record
    {$IFDEF MSWINDOWS}
    instance : HINST;
    {$ELSE}
    argc     : Integer;
    argv     : PPAnsiChar;
    {$ENDIF}
  end;

  /// <summary>
  /// Structure representing a rectangle.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types_geometry.h">CEF source file: /include/internal/cef_types_geometry.h (cef_rect_t)</see></para>
  /// </remarks>
  TCefRect = record
    x      : Integer;
    y      : Integer;
    width  : Integer;
    height : Integer;
  end;
  TCefRectArray    = array[0..(High(Integer) div SizeOf(TCefRect))-1] of TCefRect;
  TCefRectDynArray = array of TCefRect;

  /// <summary>
  /// Structure representing a point.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types_geometry.h">CEF source file: /include/internal/cef_types_geometry.h (cef_point_t)</see></para>
  /// </remarks>
  TCefPoint = record
    x  : Integer;
    y  : Integer;
  end;

  /// <summary>
  /// Structure representing a size.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types_geometry.h">CEF source file: /include/internal/cef_types_geometry.h (cef_size_t)</see></para>
  /// </remarks>
  TCefSize = record
    width  : Integer;
    height : Integer;
  end;

  /// <summary>
  /// Structure representing a range.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_range_t)</see></para>
  /// </remarks>
  TCefRange = record
    from  : cardinal;
    to_   : cardinal;
  end;
  TCefRangeArray = array of TCefRange;

  /// <summary>
  /// Structure representing cursor information. |buffer| will be
  /// |size.width|*|size.height|*4 bytes in size and represents a BGRA image with
  /// an upper-left origin.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_cursor_info_t)</see></para>
  /// </remarks>
  TCefCursorInfo = record
    hotspot            : TCefPoint;
    image_scale_factor : Single;
    buffer             : Pointer;
    size               : TCefSize;
  end;

  /// <summary>
  /// Linux window properties, such as X11's WM_CLASS or Wayland's app_id.
  /// Those are passed to CefWindowDelegate, so the client can set them
  /// for the CefWindow's top-level. Thus, allowing window managers to correctly
  /// display the application's information (e.g., icons).
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_linux_window_properties_t)</see></para>
  /// </remarks>
  TCefLinuxWindowProperties = record
    /// <summary>
    /// Main window's Wayland's app_id
    /// </summary>
    wayland_app_id : TCefString;
    /// <summary>
    /// Main window's WM_CLASS_CLASS in X11
    /// </summary>
    wm_class_class : TCefString;
    /// <summary>
    /// Main window's WM_CLASS_NAME in X11
    /// </summary>
    wm_class_name  : TCefString;
    /// <summary>
    /// Main window's WM_WINDOW_ROLE in X11
    /// </summary>
    wm_role_name   : TCefString;
  end;

  /// <summary>
  /// String version of TCefLinuxWindowProperties
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_linux_window_properties_t)</see></para>
  /// </remarks>
  TLinuxWindowProperties = record
    /// <summary>
    /// Main window's Wayland's app_id
    /// </summary>
    wayland_app_id : ustring;
    /// <summary>
    /// Main window's WM_CLASS_CLASS in X11
    /// </summary>
    wm_class_class : ustring;
    /// <summary>
    /// Main window's WM_CLASS_NAME in X11
    /// </summary>
    wm_class_name  : ustring;
    /// <summary>
    /// Main window's WM_WINDOW_ROLE in X11
    /// </summary>
    wm_role_name   : ustring;
  end;

  /// <summary>
  /// URL component parts.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_urlparts_t)</see></para>
  /// </remarks>
  TCefUrlParts = record
    /// <summary>
    /// The complete URL specification.
    /// </summary>
    spec      : TCefString;
    /// <summary>
    /// Scheme component not including the colon (e.g., "http").
    /// </summary>
    scheme    : TCefString;
    /// <summary>
    /// User name component.
    /// </summary>
    username  : TCefString;
    /// <summary>
    /// Password component.
    /// </summary>
    password  : TCefString;
    /// <summary>
    /// Host component. This may be a hostname, an IPv4 address or an IPv6 literal
    /// surrounded by square brackets (e.g., "[2001:db8::1]").
    /// </summary>
    host      : TCefString;
    /// <summary>
    /// Port number component.
    /// </summary>
    port      : TCefString;
    /// <summary>
    /// Origin contains just the scheme, host, and port from a URL. Equivalent to
    /// clearing any username and password, replacing the path with a slash, and
    /// clearing everything after that. This value will be empty for non-standard
    /// URLs.
    /// </summary>
    origin    : TCefString;
    /// <summary>
    /// Path component including the first slash following the host.
    /// </summary>
    path      : TCefString;
    /// <summary>
    /// Query string component (i.e., everything following the '?').
    /// </summary>
    query     : TCefString;
    /// <summary>
    /// Fragment (hash) identifier component (i.e., the string following the '#').
    /// </summary>
    fragment  : TCefString;
  end;

  /// <summary>
  /// String version of TCefUrlParts
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_urlparts_t)</see></para>
  /// </remarks>
  TUrlParts = record
    /// <summary>
    /// The complete URL specification.
    /// </summary>
    spec     : ustring;
    /// <summary>
    /// Scheme component not including the colon (e.g., "http").
    /// </summary>
    scheme   : ustring;
    /// <summary>
    /// User name component.
    /// </summary>
    username : ustring;
    /// <summary>
    /// Password component.
    /// </summary>
    password : ustring;
    /// <summary>
    /// Host component. This may be a hostname, an IPv4 address or an IPv6 literal
    /// surrounded by square brackets (e.g., "[2001:db8::1]").
    /// </summary>
    host     : ustring;
    /// <summary>
    /// Port number component.
    /// </summary>
    port     : ustring;
    /// <summary>
    /// Origin contains just the scheme, host, and port from a URL. Equivalent to
    /// clearing any username and password, replacing the path with a slash, and
    /// clearing everything after that. This value will be empty for non-standard
    /// URLs.
    /// </summary>
    origin   : ustring;
    /// <summary>
    /// Path component including the first slash following the host.
    /// </summary>
    path     : ustring;
    /// <summary>
    /// Query string component (i.e., everything following the '?').
    /// </summary>
    query    : ustring;
    /// <summary>
    /// Fragment (hash) identifier component (i.e., the string following the '#').
    /// </summary>
    fragment : ustring;
  end;

  /// <summary>
  /// Structure representing insets.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types_geometry.h">CEF source file: /include/internal/cef_types_geometry.h (cef_insets_t)</see></para>
  /// </remarks>
  TCefInsets = record
    top    : Integer;
    left   : Integer;
    bottom : Integer;
    right  : Integer;
  end;

  /// <summary>
  /// Represents the state of a setting.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_state_t)</see></para>
  /// </remarks>
  TCefState = (
    /// <summary>
    /// Use the default state for the setting.
    /// </summary>
    STATE_DEFAULT = 0,
    /// <summary>
    /// Enable or allow the setting.
    /// </summary>
    STATE_ENABLED,
    /// <summary>
    /// Disable or disallow the setting.
    /// </summary>
    STATE_DISABLED
  );

  /// <summary>
  /// Supported UI scale factors for the platform. SCALE_FACTOR_NONE is used for
  /// density independent resources such as string, html/js files or an image that
  /// can be used for any scale factors (such as wallpapers).
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_scale_factor_t)</see></para>
  /// </remarks>
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

  /// <summary>
  /// Supported value types.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_value_type_t)</see></para>
  /// </remarks>
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

  /// <summary>
  /// Connection state for a MediaRoute object.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_media_route_connection_state_t)</see></para>
  /// </remarks>
  TCefMediaRouteConnectionState = (
    CEF_MRCS_UNKNOWN,
    CEF_MRCS_CONNECTING,
    CEF_MRCS_CONNECTED,
    CEF_MRCS_CLOSED,
    CEF_MRCS_TERMINATED
  );

  /// <summary>
  /// Icon types for a MediaSink object. Should be kept in sync with Chromium's
  /// media_router::SinkIconType type.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_media_sink_icon_type_t)</see></para>
  /// </remarks>
  TCefMediaSinkIconType = (
    CEF_MSIT_CAST,
    CEF_MSIT_CAST_AUDIO_GROUP,
    CEF_MSIT_CAST_AUDIO,
    CEF_MSIT_MEETING,
    CEF_MSIT_HANGOUT,
    CEF_MSIT_EDUCATION,
    CEF_MSIT_WIRED_DISPLAY,
    CEF_MSIT_GENERIC,
    /// <summary>
    /// The total number of values.
    /// </summary>
    CEF_MSIT_TOTAL_COUNT
  );

  /// <summary>
  /// Policy for how the Referrer HTTP header value will be sent during
  /// navigation. If the `--no-referrers` command-line flag is specified then the
  /// policy value will be ignored and the Referrer value will never be sent. Must
  /// be kept synchronized with net::URLRequest::ReferrerPolicy from Chromium.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_referrer_policy_t)</see></para>
  /// </remarks>
  TCefReferrerPolicy = (
    /// <summary>
    /// Clear the referrer header if the header value is HTTPS but the request
    /// destination is HTTP. This is the default behavior. It has the same
    /// value as REFERRER_POLICY_DEFAULT.
    /// </summary>
    REFERRER_POLICY_CLEAR_REFERRER_ON_TRANSITION_FROM_SECURE_TO_INSECURE,
    /// <summary>
    /// A slight variant on CLEAR_REFERRER_ON_TRANSITION_FROM_SECURE_TO_INSECURE:
    /// If the request destination is HTTP, an HTTPS referrer will be cleared. If
    /// the request's destination is cross-origin with the referrer (but does not
    /// downgrade), the referrer's granularity will be stripped down to an origin
    /// rather than a full URL. Same-origin requests will send the full referrer.
    /// </summary>
    REFERRER_POLICY_REDUCE_REFERRER_GRANULARITY_ON_TRANSITION_CROSS_ORIGIN,
    /// <summary>
    /// Strip the referrer down to an origin when the origin of the referrer is
    /// different from the destination's origin.
    /// </summary>
    REFERRER_POLICY_ORIGIN_ONLY_ON_TRANSITION_CROSS_ORIGIN,
    /// <summary>
    /// Never change the referrer.
    /// </summary>
    REFERRER_POLICY_NEVER_CLEAR_REFERRER,
    /// <summary>
    /// Strip the referrer down to the origin regardless of the redirect location.
    /// </summary>
    REFERRER_POLICY_ORIGIN,
    /// <summary>
    /// Clear the referrer when the request's referrer is cross-origin with the
    /// request's destination.
    /// </summary>
    REFERRER_POLICY_CLEAR_REFERRER_ON_TRANSITION_CROSS_ORIGIN,
    /// <summary>
    /// Strip the referrer down to the origin, but clear it entirely if the
    /// referrer value is HTTPS and the destination is HTTP.
    /// </summary>
    REFERRER_POLICY_ORIGIN_CLEAR_ON_TRANSITION_FROM_SECURE_TO_INSECURE,
    /// <summary>
    /// Always clear the referrer regardless of the request destination.
    ///  It has the same value as REFERRER_POLICY_LAST_VALUE
    /// </summary>
    REFERRER_POLICY_NO_REFERRER
  );

  /// <summary>
  /// Post data elements may represent either bytes or files.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_postdataelement_type_t)</see></para>
  /// </remarks>
  TCefPostDataElementType = (
    PDE_TYPE_EMPTY  = 0,
    PDE_TYPE_BYTES,
    PDE_TYPE_FILE
  );

  /// <summary>
  /// Resource type for a request. These constants match their equivalents in
  /// Chromium's ResourceType and should not be renumbered.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_resource_type_t)</see></para>
  /// </remarks>
  TCefResourceType = (
    /// <summary>
    /// Top level page.
    /// </summary>
    RT_MAIN_FRAME,
    /// <summary>
    /// Frame or iframe.
    /// </summary>
    RT_SUB_FRAME,
    /// <summary>
    /// CSS stylesheet.
    /// </summary>
    RT_STYLESHEET,
    /// <summary>
    /// External script.
    /// </summary>
    RT_SCRIPT,
    /// <summary>
    /// Image (jpg/gif/png/etc).
    /// </summary>
    RT_IMAGE,
    /// <summary>
    /// Font.
    /// </summary>
    RT_FONT_RESOURCE,
    /// <summary>
    /// Some other subresource. This is the default type if the actual type is
    /// unknown.
    /// </summary>
    RT_SUB_RESOURCE,
    /// <summary>
    /// Object (or embed) tag for a plugin, or a resource that a plugin requested.
    /// </summary>
    RT_OBJECT,
    /// <summary>
    /// Media resource.
    /// </summary>
    RT_MEDIA,
    /// <summary>
    /// Main resource of a dedicated worker.
    /// </summary>
    RT_WORKER,
    /// <summary>
    /// Main resource of a shared worker.
    /// </summary>
    RT_SHARED_WORKER,
    /// <summary>
    /// Explicitly requested prefetch.
    /// </summary>
    RT_PREFETCH,
    /// <summary>
    /// Favicon.
    /// </summary>
    RT_FAVICON,
    /// <summary>
    /// XMLHttpRequest.
    /// </summary>
    RT_XHR,
    /// <summary>
    /// A request for a "<ping>".
    /// </summary>
    RT_PING,
    /// <summary>
    /// Main resource of a service worker.
    /// </summary>
    RT_SERVICE_WORKER,
    /// <summary>
    /// A report of Content Security Policy violations.
    /// </summary>
    RT_CSP_REPORT,
    /// <summary>
    /// A resource that a plugin requested.
    /// </summary>
    RT_PLUGIN_RESOURCE,
    /// <summary>
    /// This type doesn't exist in CEF and it's here just to fill this position.
    /// </summary>
    RT_EMPTY_FILLER_TYPE_DO_NOT_USE,
    /// <summary>
    /// A main-frame service worker navigation preload request.
    /// </summary>
    RT_NAVIGATION_PRELOAD_MAIN_FRAME,
    /// <summary>
    /// A sub-frame service worker navigation preload request.
    /// </summary>
    RT_NAVIGATION_PRELOAD_SUB_FRAME
  );

  /// <summary>
  /// DOM document types.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_dom_document_type_t)</see></para>
  /// </remarks>
  TCefDomDocumentType = (
    DOM_DOCUMENT_TYPE_UNKNOWN = 0,
    DOM_DOCUMENT_TYPE_HTML,
    DOM_DOCUMENT_TYPE_XHTML,
    DOM_DOCUMENT_TYPE_PLUGIN
  );

  /// <summary>
  /// DOM node types.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_dom_node_type_t)</see></para>
  /// </remarks>
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

  /// <summary>
  /// DOM form control types. Should be kept in sync with Chromium's
  /// blink::mojom::FormControlType type.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_dom_form_control_type_t)</see></para>
  /// </remarks>
  TCefDomFormControlType = (
    DOM_FORM_CONTROL_TYPE_UNSUPPORTED = 0,
    DOM_FORM_CONTROL_TYPE_BUTTON_BUTTON,
    DOM_FORM_CONTROL_TYPE_BUTTON_SUBMIT,
    DOM_FORM_CONTROL_TYPE_BUTTON_RESET,
    DOM_FORM_CONTROL_TYPE_BUTTON_SELECT_LIST,
    DOM_FORM_CONTROL_TYPE_BUTTON_POPOVER,
    DOM_FORM_CONTROL_TYPE_FIELDSET,
    DOM_FORM_CONTROL_TYPE_INPUT_BUTTON,
    DOM_FORM_CONTROL_TYPE_INPUT_CHECKBOX,
    DOM_FORM_CONTROL_TYPE_INPUT_COLOR,
    DOM_FORM_CONTROL_TYPE_INPUT_DATE,
    DOM_FORM_CONTROL_TYPE_INPUT_DATETIME_LOCAL,
    DOM_FORM_CONTROL_TYPE_INPUT_EMAIL,
    DOM_FORM_CONTROL_TYPE_INPUT_FILE,
    DOM_FORM_CONTROL_TYPE_INPUT_HIDDEN,
    DOM_FORM_CONTROL_TYPE_INPUT_IMAGE,
    DOM_FORM_CONTROL_TYPE_INPUT_MONTH,
    DOM_FORM_CONTROL_TYPE_INPUT_NUMBER,
    DOM_FORM_CONTROL_TYPE_INPUT_PASSWORD,
    DOM_FORM_CONTROL_TYPE_INPUT_RADIO,
    DOM_FORM_CONTROL_TYPE_INPUT_RANGE,
    DOM_FORM_CONTROL_TYPE_INPUT_RESET,
    DOM_FORM_CONTROL_TYPE_INPUT_SEARCH,
    DOM_FORM_CONTROL_TYPE_INPUT_SUBMIT,
    DOM_FORM_CONTROL_TYPE_INPUT_TELEPHONE,
    DOM_FORM_CONTROL_TYPE_INPUT_TEXT,
    DOM_FORM_CONTROL_TYPE_INPUT_TIME,
    DOM_FORM_CONTROL_TYPE_INPUT_URL,
    DOM_FORM_CONTROL_TYPE_INPUT_WEEK,
    DOM_FORM_CONTROL_TYPE_OUTPUT,
    DOM_FORM_CONTROL_TYPE_SELECT_ONE,
    DOM_FORM_CONTROL_TYPE_SELECT_MULTIPLE,
    DOM_FORM_CONTROL_TYPE_SELECT_LIST,
    DOM_FORM_CONTROL_TYPE_TEXT_AREA
);

  /// <summary>
  /// Supported context menu media types. These constants match their equivalents
  /// in Chromium's ContextMenuDataMediaType and should not be renumbered.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_context_menu_media_type_t)</see></para>
  /// </remarks>
  TCefContextMenuMediaType = (
    /// <summary>
    /// No special node is in context.
    /// </summary>
    CM_MEDIATYPE_NONE,
    /// <summary>
    /// An image node is selected.
    /// </summary>
    CM_MEDIATYPE_IMAGE,
    /// <summary>
    /// A video node is selected.
    /// </summary>
    CM_MEDIATYPE_VIDEO,
    /// <summary>
    /// An audio node is selected.
    /// </summary>
    CM_MEDIATYPE_AUDIO,
    /// <summary>
    /// An canvas node is selected.
    /// </summary>
    CM_MEDIATYPE_CANVAS,
    /// <summary>
    /// A file node is selected.
    /// </summary>
    CM_MEDIATYPE_FILE,
    /// <summary>
    /// A plugin node is selected.
    /// </summary>
    CM_MEDIATYPE_PLUGIN
  );

  /// <summary>
  /// Supported menu item types.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_menu_item_type_t)</see></para>
  /// </remarks>
  TCefMenuItemType = (
    MENUITEMTYPE_NONE,
    MENUITEMTYPE_COMMAND,
    MENUITEMTYPE_CHECK,
    MENUITEMTYPE_RADIO,
    MENUITEMTYPE_SEPARATOR,
    MENUITEMTYPE_SUBMENU
  );

  /// <summary>
  /// Focus sources.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_focus_source_t)</see></para>
  /// </remarks>
  TCefFocusSource = (
    /// <summary>
    /// The source is explicit navigation via the API (LoadURL(), etc).
    /// </summary>
    FOCUS_SOURCE_NAVIGATION = 0,
    /// <summary>
    /// The source is a system-generated focus event.
    /// </summary>
    FOCUS_SOURCE_SYSTEM
  );

  /// <summary>
  /// Supported JavaScript dialog types.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_jsdialog_type_t)</see></para>
  /// </remarks>
  TCefJsDialogType = (
    JSDIALOGTYPE_ALERT = 0,
    JSDIALOGTYPE_CONFIRM,
    JSDIALOGTYPE_PROMPT
  );

  /// <summary>
  /// Notification that a character was typed. Use this for text input. Key
  /// down events may generate 0, 1, or more than one character event depending
  /// on the key, locale, and operating system.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_key_event_type_t)</see></para>
  /// </remarks>
  TCefKeyEventType = (
    /// <summary>
    /// Notification that a key transitioned from "up" to "down".
    /// </summary>
    KEYEVENT_RAWKEYDOWN = 0,
    /// <summary>
    /// Notification that a key was pressed. This does not necessarily correspond
    /// to a character depending on the key and language. Use KEYEVENT_CHAR for
    /// character input.
    /// </summary>
    KEYEVENT_KEYDOWN,
    /// <summary>
    /// Notification that a key was released.
    /// </summary>
    KEYEVENT_KEYUP,
    /// <summary>
    /// Notification that a character was typed. Use this for text input. Key
    /// down events may generate 0, 1, or more than one character event depending
    /// on the key, locale, and operating system.
    /// </summary>
    KEYEVENT_CHAR
  );

  /// <summary>
  /// The manner in which a link click should be opened. These constants match
  /// their equivalents in Chromium's window_open_disposition.h and should not be
  /// renumbered.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_window_open_disposition_t)</see></para>
  /// </remarks>
  TCefWindowOpenDisposition = (
    /// <summary>
    /// Unknown disposition.
    /// </summary>
    CEF_WOD_UNKNOWN,
    /// <summary>
    /// Current tab. This is the default in most cases.
    /// </summary>
    CEF_WOD_CURRENT_TAB,
    /// <summary>
    /// Indicates that only one tab with the url should exist in the same window.
    /// </summary>
    CEF_WOD_SINGLETON_TAB,
    /// <summary>
    /// Shift key + Middle mouse button or meta/ctrl key while clicking.
    /// </summary>
    CEF_WOD_NEW_FOREGROUND_TAB,
    /// <summary>
    /// Middle mouse button or meta/ctrl key while clicking.
    /// </summary>
    CEF_WOD_NEW_BACKGROUND_TAB,
    /// <summary>
    /// New popup window.
    /// </summary>
    CEF_WOD_NEW_POPUP,
    /// <summary>
    /// Shift key while clicking.
    /// </summary>
    CEF_WOD_NEW_WINDOW,
    /// <summary>
    /// Alt key while clicking.
    /// </summary>
    CEF_WOD_SAVE_TO_DISK,
    /// <summary>
    /// New off-the-record (incognito) window.
    /// </summary>
    CEF_WOD_OFF_THE_RECORD,
    /// <summary>
    /// Special case error condition from the renderer.
    /// </summary>
    CEF_WOD_IGNORE_ACTION,
    /// <summary>
    /// Activates an existing tab containing the url, rather than navigating.
    /// This is similar to SINGLETON_TAB, but searches across all windows from
    /// the current profile and anonymity (instead of just the current one);
    /// closes the current tab on switching if the current tab was the NTP with
    /// no session history; and behaves like CURRENT_TAB instead of
    /// NEW_FOREGROUND_TAB when no existing tab is found.
    /// </summary>
    CEF_WOD_SWITCH_TO_TAB,
    /// <summary>
    /// Creates a new document picture-in-picture window showing a child WebView.
    /// </summary>
    CEF_WOD_NEW_PICTURE_IN_PICTURE
  );

  /// <summary>
  /// Input mode of a virtual keyboard. These constants match their equivalents
  /// in Chromium's text_input_mode.h and should not be renumbered.
  /// See https://html.spec.whatwg.org/#input-modalities:-the-inputmode-attribute
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_text_input_mode_t)</see></para>
  /// </remarks>
  TCefTextInpuMode = (
    CEF_TEXT_INPUT_MODE_DEFAULT,
    CEF_TEXT_INPUT_MODE_NONE,
    CEF_TEXT_INPUT_MODE_TEXT,
    CEF_TEXT_INPUT_MODE_TEL,
    CEF_TEXT_INPUT_MODE_URL,
    CEF_TEXT_INPUT_MODE_EMAIL,
    CEF_TEXT_INPUT_MODE_NUMERIC,
    CEF_TEXT_INPUT_MODE_DECIMAL,
    CEF_TEXT_INPUT_MODE_SEARCH
    {* CEF_TEXT_INPUT_MODE_MAX = CEF_TEXT_INPUT_MODE_SEARCH *}
  );

  /// <summary>
  /// Touch points states types.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_touch_event_type_t)</see></para>
  /// </remarks>
  TCefTouchEeventType = (
    CEF_TET_RELEASED = 0,
    CEF_TET_PRESSED,
    CEF_TET_MOVED,
    CEF_TET_CANCELLED
  );

  /// <summary>
  /// The device type that caused the event.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_pointer_type_t)</see></para>
  /// </remarks>
  TCefPointerType = (
    CEF_POINTER_TYPE_TOUCH = 0,
    CEF_POINTER_TYPE_MOUSE,
    CEF_POINTER_TYPE_PEN,
    CEF_POINTER_TYPE_ERASER,
    CEF_POINTER_TYPE_UNKNOWN
  );

  /// <summary>
  /// Type of touch event in the TChromiumCore.SimulateTouchEvent function.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://chromedevtools.github.io/devtools-protocol/tot/Input/#method-dispatchTouchEvent">See the Input.dispatchTouchEvent DevTools method</see></para>
  /// </remarks>
  TCefSimulatedTouchEventType = (
    touchStart,
    touchEnd,
    touchMove,
    touchCancel
  );

  /// <summary>
  /// Type of mouse event in the TChromiumCore.SimulateMouseEvent function.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://chromedevtools.github.io/devtools-protocol/tot/Input/#method-dispatchMouseEvent">See the Input.dispatchMouseEvent DevTools method</see></para>
  /// </remarks>
  TCefSimulatedMouseEventType = (
    mousePressed,
    mouseReleased,
    mouseMoved,
    mouseWheel
  );

  /// <summary>
  /// Mouse button in the TChromiumCore.SimulateMouseEvent function.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://chromedevtools.github.io/devtools-protocol/tot/Input/#method-dispatchMouseEvent">See the Input.dispatchMouseEvent DevTools method</see></para>
  /// </remarks>
  TCefSimulatedMouseButton = (
    CEF_SIMULATEDMOUSEBUTTON_NONE,
    CEF_SIMULATEDMOUSEBUTTON_LEFT,
    CEF_SIMULATEDMOUSEBUTTON_MIDDLE,
    CEF_SIMULATEDMOUSEBUTTON_RIGHT,
    CEF_SIMULATEDMOUSEBUTTON_BACK,
    CEF_SIMULATEDMOUSEBUTTON_FORWARD
  );

  /// <summary>
  /// Pointer type in the TChromiumCore.SimulateMouseEvent function.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://chromedevtools.github.io/devtools-protocol/tot/Input/#method-dispatchMouseEvent">See the Input.dispatchMouseEvent DevTools method</see></para>
  /// </remarks>
  TCefSimulatedPointerType = (
    CEF_SIMULATEDPOINTERTYPE_MOUSE,
    CEF_SIMULATEDPOINTERTYPE_PEN
  );

  /// <summary>
  /// Key location value used in the TChromiumCore.dispatchKeyEvent DevTools method.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://chromedevtools.github.io/devtools-protocol/1-3/Input/#method-dispatchKeyEvent">See the "Input.dispatchKeyEvent" DevTools method.</see></para>
  /// </remarks>
  TCefKeyLocation = (
    CEF_KEYLOCATION_NONE,
    CEF_KEYLOCATION_LEFT,
    CEF_KEYLOCATION_RIGHT
  );

  /// <summary>
  /// Blink editing commands used by the "Input.dispatchKeyEvent" DevTools method.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://chromedevtools.github.io/devtools-protocol/1-3/Input/#method-dispatchKeyEvent">See the "Input.dispatchKeyEvent" DevTools method.</see></para>
  /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/master:third_party/blink/renderer/core/editing/commands/editor_command_names.h">See the Chromium sources.</see></para>
  /// </remarks>
  TCefEditingCommand = (ecNone,
                        ecAlignCenter,
                        ecAlignJustified,
                        ecAlignLeft,
                        ecAlignRight,
                        ecBackColor,
                        ecBackwardDelete,
                        ecBold,
                        ecCopy,
                        ecCreateLink,
                        ecCut,
                        ecDefaultParagraphSeparator,
                        ecDelete,
                        ecDeleteBackward,
                        ecDeleteBackwardByDecomposingPreviousCharacter,
                        ecDeleteForward,
                        ecDeleteToBeginningOfLine,
                        ecDeleteToBeginningOfParagraph,
                        ecDeleteToEndOfLine,
                        ecDeleteToEndOfParagraph,
                        ecDeleteToMark,
                        ecDeleteWordBackward,
                        ecDeleteWordForward,
                        ecFindString,
                        ecFontName,
                        ecFontSize,
                        ecFontSizeDelta,
                        ecForeColor,
                        ecFormatBlock,
                        ecForwardDelete,
                        ecHiliteColor,
                        ecIgnoreSpelling,
                        ecIndent,
                        ecInsertBacktab,
                        ecInsertHorizontalRule,
                        ecInsertHTML,
                        ecInsertImage,
                        ecInsertLineBreak,
                        ecInsertNewline,
                        ecInsertNewlineInQuotedContent,
                        ecInsertOrderedList,
                        ecInsertParagraph,
                        ecInsertTab,
                        ecInsertText,
                        ecInsertUnorderedList,
                        ecItalic,
                        ecJustifyCenter,
                        ecJustifyFull,
                        ecJustifyLeft,
                        ecJustifyNone,
                        ecJustifyRight,
                        ecMakeTextWritingDirectionLeftToRight,
                        ecMakeTextWritingDirectionNatural,
                        ecMakeTextWritingDirectionRightToLeft,
                        ecMoveBackward,
                        ecMoveBackwardAndModifySelection,
                        ecMoveDown,
                        ecMoveDownAndModifySelection,
                        ecMoveForward,
                        ecMoveForwardAndModifySelection,
                        ecMoveLeft,
                        ecMoveLeftAndModifySelection,
                        ecMovePageDown,
                        ecMovePageDownAndModifySelection,
                        ecMovePageUp,
                        ecMovePageUpAndModifySelection,
                        ecMoveParagraphBackward,
                        ecMoveParagraphBackwardAndModifySelection,
                        ecMoveParagraphForward,
                        ecMoveParagraphForwardAndModifySelection,
                        ecMoveRight,
                        ecMoveRightAndModifySelection,
                        ecMoveToBeginningOfDocument,
                        ecMoveToBeginningOfDocumentAndModifySelection,
                        ecMoveToBeginningOfLine,
                        ecMoveToBeginningOfLineAndModifySelection,
                        ecMoveToBeginningOfParagraph,
                        ecMoveToBeginningOfParagraphAndModifySelection,
                        ecMoveToBeginningOfSentence,
                        ecMoveToBeginningOfSentenceAndModifySelection,
                        ecMoveToEndOfDocument,
                        ecMoveToEndOfDocumentAndModifySelection,
                        ecMoveToEndOfLine,
                        ecMoveToEndOfLineAndModifySelection,
                        ecMoveToEndOfParagraph,
                        ecMoveToEndOfParagraphAndModifySelection,
                        ecMoveToEndOfSentence,
                        ecMoveToEndOfSentenceAndModifySelection,
                        ecMoveToLeftEndOfLine,
                        ecMoveToLeftEndOfLineAndModifySelection,
                        ecMoveToRightEndOfLine,
                        ecMoveToRightEndOfLineAndModifySelection,
                        ecMoveUp,
                        ecMoveUpAndModifySelection,
                        ecMoveWordBackward,
                        ecMoveWordBackwardAndModifySelection,
                        ecMoveWordForward,
                        ecMoveWordForwardAndModifySelection,
                        ecMoveWordLeft,
                        ecMoveWordLeftAndModifySelection,
                        ecMoveWordRight,
                        ecMoveWordRightAndModifySelection,
                        ecOutdent,
                        ecOverWrite,
                        ecPaste,
                        ecPasteAndMatchStyle,
                        ecPasteGlobalSelection,
                        ecPrint,
                        ecRedo,
                        ecRemoveFormat,
                        ecScrollLineDown,
                        ecScrollLineUp,
                        ecScrollPageBackward,
                        ecScrollPageForward,
                        ecScrollToBeginningOfDocument,
                        ecScrollToEndOfDocument,
                        ecSelectAll,
                        ecSelectLine,
                        ecSelectParagraph,
                        ecSelectSentence,
                        ecSelectToMark,
                        ecSelectWord,
                        ecSetMark,
                        ecStrikethrough,
                        ecStyleWithCSS,
                        ecSubscript,
                        ecSuperscript,
                        ecSwapWithMark,
                        ecToggleBold,
                        ecToggleItalic,
                        ecToggleUnderline,
                        ecTranspose,
                        ecUnderline,
                        ecUndo,
                        ecUnlink,
                        ecUnscript,
                        ecUnselect,
                        ecUseCSS,
                        ecYank,
                        ecYankAndSelect);

  /// <summary>
  /// Enumerates the various representations of the ordering of audio channels.
  /// Must be kept synchronized with media::ChannelLayout from Chromium.
  /// See media\base\channel_layout.h
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_channel_layout_t)</see></para>
  /// </remarks>
  TCefChannelLayout = (
    CEF_CHANNEL_LAYOUT_NONE = 0,
    CEF_CHANNEL_LAYOUT_UNSUPPORTED = 1,
    /// <summary>Front C</summary>
    CEF_CHANNEL_LAYOUT_MONO = 2,
    /// <summary>Front L, Front R</summary>
    CEF_CHANNEL_LAYOUT_STEREO = 3,
    /// <summary>Front L, Front R, Back C</summary>
    CEF_CHANNEL_LAYOUT_2_1 = 4,
    /// <summary>Front L, Front R, Front C</summary>
    CEF_CHANNEL_LAYOUT_SURROUND = 5,
    /// <summary>Front L, Front R, Front C, Back C</summary>
    CEF_CHANNEL_LAYOUT_4_0 = 6,
    /// <summary>Front L, Front R, Side L, Side R</summary>
    CEF_CHANNEL_LAYOUT_2_2 = 7,
    /// <summary>Front L, Front R, Back L, Back R</summary>
    CEF_CHANNEL_LAYOUT_QUAD = 8,
    /// <summary>Front L, Front R, Front C, Side L, Side R</summary>
    CEF_CHANNEL_LAYOUT_5_0 = 9,
    /// <summary>Front L, Front R, Front C, LFE, Side L, Side R</summary>
    CEF_CHANNEL_LAYOUT_5_1 = 10,
    /// <summary>Front L, Front R, Front C, Back L, Back R</summary>
    CEF_CHANNEL_LAYOUT_5_0_BACK = 11,
    /// <summary>Front L, Front R, Front C, LFE, Back L, Back R</summary>
    CEF_CHANNEL_LAYOUT_5_1_BACK = 12,
    /// <summary>Front L, Front R, Front C, Back L, Back R, Side L, Side R</summary>
    CEF_CHANNEL_LAYOUT_7_0 = 13,
    /// <summary>Front L, Front R, Front C, LFE, Back L, Back R, Side L, Side R</summary>
    CEF_CHANNEL_LAYOUT_7_1 = 14,
    /// <summary>Front L, Front R, Front C, LFE, Front LofC, Front RofC, Side L, Side R</summary>
    CEF_CHANNEL_LAYOUT_7_1_WIDE = 15,
    /// <summary>Front L, Front R</summary>
    CEF_CHANNEL_LAYOUT_STEREO_DOWNMIX = 16,
    /// <summary>Front L, Front R, LFE</summary>
    CEF_CHANNEL_LAYOUT_2POINT1 = 17,
    /// <summary>Front L, Front R, Front C, LFE</summary>
    CEF_CHANNEL_LAYOUT_3_1 = 18,
    /// <summary>Front L, Front R, Front C, LFE, Back C</summary>
    CEF_CHANNEL_LAYOUT_4_1 = 19,
    /// <summary>Front L, Front R, Front C, Back C, Side L, Side R</summary>
    CEF_CHANNEL_LAYOUT_6_0 = 20,
    /// <summary>Front L, Front R, Front LofC, Front RofC, Side L, Side R</summary>
    CEF_CHANNEL_LAYOUT_6_0_FRONT = 21,
    /// <summary>Front L, Front R, Front C, Back L, Back R, Back C</summary>
    CEF_CHANNEL_LAYOUT_HEXAGONAL = 22,
    /// <summary>Front L, Front R, Front C, LFE, Back C, Side L, Side R</summary>
    CEF_CHANNEL_LAYOUT_6_1 = 23,
    /// <summary>Front L, Front R, Front C, LFE, Back L, Back R, Back C</summary>
    CEF_CHANNEL_LAYOUT_6_1_BACK = 24,
    /// <summary>Front L, Front R, LFE, Front LofC, Front RofC, Side L, Side R</summary>
    CEF_CHANNEL_LAYOUT_6_1_FRONT = 25,
    /// <summary>Front L, Front R, Front C, Front LofC, Front RofC, Side L, Side R</summary>
    CEF_CHANNEL_LAYOUT_7_0_FRONT = 26,
    /// <summary>Front L, Front R, Front C, LFE, Back L, Back R, Front LofC, Front RofC</summary>
    CEF_CHANNEL_LAYOUT_7_1_WIDE_BACK = 27,
    /// <summary>Front L, Front R, Front C, Back L, Back R, Back C, Side L, Side R</summary>
    CEF_CHANNEL_LAYOUT_OCTAGONAL = 28,
    /// <summary>Channels are not explicitly mapped to speakers.</summary>
    CEF_CHANNEL_LAYOUT_DISCRETE = 29,
    /// <summary>
    /// Deprecated, but keeping the enum value for UMA consistency.
    /// Front L, Front R, Front C. Front C contains the keyboard mic audio. This
    /// layout is only intended for input for WebRTC. The Front C channel
    /// is stripped away in the WebRTC audio input pipeline and never seen outside
    /// of that.
    /// </summary>
    CEF_CHANNEL_LAYOUT_STEREO_AND_KEYBOARD_MIC = 30,
    /// <summary>Front L, Front R, LFE, Side L, Side R</summary>
    CEF_CHANNEL_LAYOUT_4_1_QUAD_SIDE = 31,
    /// <summary>
    /// Actual channel layout is specified in the bitstream and the actual channel
    /// count is unknown at Chromium media pipeline level (useful for audio
    /// pass-through mode).
    /// </summary>
    CEF_CHANNEL_LAYOUT_BITSTREAM = 32,
    /// <summary>
    /// Front L, Front R, Front C, LFE, Side L, Side R,
    /// Front Height L, Front Height R, Rear Height L, Rear Height R
    /// Will be represented as six channels (5.1) due to eight channel limit
    /// kMaxConcurrentChannels
    /// </summary>
    CEF_CHANNEL_LAYOUT_5_1_4_DOWNMIX = 33,
    /// <summary>Front C, LFE</summary>
    CEF_CHANNEL_LAYOUT_1_1 = 34,
    /// <summary>Front L, Front R, LFE, Back C</summary>
    CEF_CHANNEL_LAYOUT_3_1_BACK = 35
    {* CEF_CHANNEL_LAYOUT_MAX = CEF_CHANNEL_LAYOUT_3_1_BACK *}
  );

  /// <summary>
  /// Cookie same site values.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_cookie_same_site_t)</see></para>
  /// </remarks>
  TCefCookieSameSite = (
    CEF_COOKIE_SAME_SITE_UNSPECIFIED,
    CEF_COOKIE_SAME_SITE_NO_RESTRICTION,
    CEF_COOKIE_SAME_SITE_LAX_MODE,
    CEF_COOKIE_SAME_SITE_STRICT_MODE
  );

  /// <summary>
  /// Paint element types.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_paint_element_type_t)</see></para>
  /// </remarks>
  TCefPaintElementType = (
    PET_VIEW,
    PET_POPUP
  );

  /// <summary>
  /// Cursor type values.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_cursor_type_t)</see></para>
  /// </remarks>
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

  /// <summary>
  /// Navigation types.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_navigation_type_t)</see></para>
  /// </remarks>
  TCefNavigationType = (
    NAVIGATION_LINK_CLICKED,
    NAVIGATION_FORM_SUBMITTED,
    NAVIGATION_BACK_FORWARD,
    NAVIGATION_RELOAD,
    NAVIGATION_FORM_RESUBMITTED,
    NAVIGATION_OTHER
  );

  /// <summary>
  /// Existing process IDs.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_process_id_t)</see></para>
  /// </remarks>
  TCefProcessId = (
    /// <summary>
    /// Browser process.
    /// </summary>
    PID_BROWSER,
    /// <summary>
    /// Renderer process.
    /// </summary>
    PID_RENDERER
  );

  /// <summary>
  /// Existing thread IDs.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_thread_id_t)</see></para>
  /// </remarks>
  TCefThreadId = (
    // BROWSER PROCESS THREADS -- Only available in the browser process.

    /// <summary>
    /// The main thread in the browser. This will be the same as the main
    /// application thread if CefInitialize() is called with a
    /// TCefSettings.multi_threaded_message_loop value of false. Do not perform
    /// blocking tasks on this thread. All tasks posted after
    /// ICefBrowserProcessHandler.OnContextInitialized() and before CefShutdown()
    /// are guaranteed to run. This thread will outlive all other CEF threads.
    /// </summary>
    TID_UI,
    /// <summary>
    /// Used for blocking tasks like file system access where the user won't
    /// notice if the task takes an arbitrarily long time to complete. All tasks
    /// posted after ICefBrowserProcessHandler.OnContextInitialized() and before
    /// CefShutdown() are guaranteed to run.
    /// </summary>
    TID_FILE_BACKGROUND,
    /// <summary>
    /// Used for blocking tasks like file system access that affect UI or
    /// responsiveness of future user interactions. Do not use if an immediate
    /// response to a user interaction is expected. All tasks posted after
    /// ICefBrowserProcessHandler.OnContextInitialized() and before CefShutdown()
    /// are guaranteed to run.
    /// Examples:
    /// - Updating the UI to reflect progress on a long task.
    /// - Loading data that might be shown in the UI after a future user
    ///   interaction.
    /// </summary>
    TID_FILE_USER_VISIBLE,
    /// <summary>
    /// Used for blocking tasks like file system access that affect UI
    /// immediately after a user interaction. All tasks posted after
    /// ICefBrowserProcessHandler.OnContextInitialized() and before CefShutdown()
    /// are guaranteed to run.
    /// Example: Generating data shown in the UI immediately after a click.
    /// </summary>
    TID_FILE_USER_BLOCKING,
    /// <summary>
    /// Used to launch and terminate browser processes.
    /// </summary>
    TID_PROCESS_LAUNCHER,
    /// <summary>
    /// Used to process IPC and network messages. Do not perform blocking tasks on
    /// this thread. All tasks posted after
    /// ICefBrowserProcessHandler.OnContextInitialized() and before CefShutdown()
    /// are guaranteed to run.
    /// </summary>
    TID_IO,
    // RENDER PROCESS THREADS -- Only available in the render process.

    /// <summary>
    /// The main thread in the renderer. Used for all WebKit and V8 interaction.
    /// Tasks may be posted to this thread after
    /// ICefRenderProcessHandler.OnWebKitInitialized but are not guaranteed to
    /// run before sub-process termination (sub-processes may be killed at any
    /// time without warning).
    /// </summary>
    TID_RENDERER
  );

  /// <summary>
  /// Thread priority values listed in increasing order of importance.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_thread_priority_t)</see></para>
  /// </remarks>
  TCefThreadPriority = (
    /// <summary>
    /// Suitable for threads that shouldn't disrupt high priority work.
    /// </summary>
    TP_BACKGROUND,
    /// <summary>
    /// Default priority level.
    /// </summary>
    TP_NORMAL,
    /// <summary>
    /// Suitable for threads which generate data for the display (at ~60Hz).
    /// </summary>
    TP_DISPLAY,
    /// <summary>
    /// Suitable for low-latency, glitch-resistant audio.
    /// </summary>
    TP_REALTIME_AUDIO
  );

  /// <summary>
  /// Flags used to customize the behavior of CefURLRequest.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_message_loop_type_t)</see></para>
  /// </remarks>
  TCefMessageLoopType = (
    /// <summary>
    /// Supports tasks and timers.
    /// </summary>
    ML_TYPE_DEFAULT,
    /// <summary>
    /// Supports tasks, timers and native UI events (e.g. Windows messages).
    /// </summary>
    ML_TYPE_UI,
    /// <summary>
    /// Supports tasks, timers and asynchronous IO events.
    /// </summary>
    ML_TYPE_IO
  );

  /// <summary>
  /// Flags used to customize the behavior of CefURLRequest.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_com_init_mode_t)</see></para>
  /// </remarks>
  TCefCOMInitMode = (
    /// <summary>
    /// No COM initialization.
    /// </summary>
    COM_INIT_MODE_NONE,
    /// <summary>
    /// Initialize COM using single-threaded apartments.
    /// </summary>
    COM_INIT_MODE_STA,
    /// <summary>
    /// Initialize COM using multi-threaded apartments.
    /// </summary>
    COM_INIT_MODE_MTA
  );

  /// <summary>
  /// Mouse button types.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_mouse_button_type_t)</see></para>
  /// </remarks>
  TCefMouseButtonType = (
    MBT_LEFT,
    MBT_MIDDLE,
    MBT_RIGHT
  );

  /// <summary>
  /// Return value types.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_return_value_t)</see></para>
  /// </remarks>
  TCefReturnValue = (
    /// <summary>
    /// Cancel immediately.
    /// </summary>
    RV_CANCEL = 0,
    /// <summary>
    /// Continue immediately.
    /// </summary>
    RV_CONTINUE,
    /// <summary>
    /// Continue asynchronously (usually via a callback).
    /// </summary>
    RV_CONTINUE_ASYNC
  );

  /// <summary>
  /// Flags that represent CefURLRequest status.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_urlrequest_status_t)</see></para>
  /// </remarks>
  TCefUrlRequestStatus = (
    /// <summary>
    /// Unknown status.
    /// </summary>
    UR_UNKNOWN = 0,
    /// <summary>
    /// Request succeeded.
    /// </summary>
    UR_SUCCESS,
    /// <summary>
    /// An IO request is pending, and the caller will be informed when it is
    /// completed.
    /// </summary>
    UR_IO_PENDING,
    /// <summary>
    /// Request was canceled programatically.
    /// </summary>
    UR_CANCELED,
    /// <summary>
    /// Request failed for some reason.
    /// </summary>
    UR_FAILED
  );

  /// <summary>
  /// Process termination status values.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_termination_status_t)</see></para>
  /// </remarks>
  TCefTerminationStatus = (
    /// <summary>
    /// Non-zero exit status.
    /// </summary>
    TS_ABNORMAL_TERMINATION,
    /// <summary>
    /// SIGKILL or task manager kill.
    /// </summary>
    TS_PROCESS_WAS_KILLED,
    /// <summary>
    /// Segmentation fault.
    /// </summary>
    TS_PROCESS_CRASHED,
    /// <summary>
    /// Out of memory. Some platforms may use TS_PROCESS_CRASHED instead.
    /// </summary>
    TS_PROCESS_OOM,
    /// <summary>
    /// Child process never launched.
    /// </summary>
    TS_LAUNCH_FAILED,
    /// <summary>
    /// On Windows, the OS terminated the process due to code integrity failure.
    /// </summary>
    TS_INTEGRITY_FAILURE
  );

  /// <summary>
  /// Process termination status values.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_path_key_t)</see></para>
  /// </remarks>
  TCefPathKey = (
    /// <summary>
    /// Current directory.
    /// </summary>
    PK_DIR_CURRENT,
    /// <summary>
    /// Directory containing PK_FILE_EXE.
    /// </summary>
    PK_DIR_EXE,
    /// <summary>
    /// Directory containing PK_FILE_MODULE.
    /// </summary>
    PK_DIR_MODULE,
    /// <summary>
    /// Temporary directory.
    /// </summary>
    PK_DIR_TEMP,
    /// <summary>
    /// Path and filename of the current executable.
    /// </summary>
    PK_FILE_EXE,
    /// <summary>
    /// Path and filename of the module containing the CEF code (usually the
    /// libcef module).
    /// </summary>
    PK_FILE_MODULE,
    /// <summary>
    /// "Local Settings\Application Data" directory under the user profile
    /// directory on Windows.
    /// </summary>
    PK_LOCAL_APP_DATA,
    /// <summary>
    /// "Application Data" directory under the user profile directory on Windows
    /// and "~/Library/Application Support" directory on MacOS.
    /// </summary>
    PK_USER_DATA,
    /// <summary>
    /// Directory containing application resources. Can be configured via
    /// TCefSettings.resources_dir_path.
    /// </summary>
    PK_DIR_RESOURCES
  );

  /// <summary>
  /// Storage types.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_storage_type_t)</see></para>
  /// </remarks>
  TCefStorageType = (
    ST_LOCALSTORAGE = 0,
    ST_SESSIONSTORAGE
  );

  /// <summary>
  /// Return values for ICefResponseFilter.Filter().
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_response_filter_status_t)</see></para>
  /// </remarks>
  TCefResponseFilterStatus = (
    /// <summary>
    /// Some or all of the pre-filter data was read successfully but more data is
    /// needed in order to continue filtering (filtered output is pending).
    /// </summary>
    RESPONSE_FILTER_NEED_MORE_DATA,
    /// <summary>
    /// Some or all of the pre-filter data was read successfully and all available
    /// filtered output has been written.
    /// </summary>
    RESPONSE_FILTER_DONE,
    /// <summary>
    /// An error occurred during filtering.
    /// </summary>
    RESPONSE_FILTER_ERROR
  );

  /// <summary>
  /// Describes how to interpret the alpha component of a pixel.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_alpha_type_t)</see></para>
  /// </remarks>
  TCefAlphaType = (
    /// <summary>
    /// No transparency. The alpha component is ignored.
    /// </summary>
    CEF_ALPHA_TYPE_OPAQUE,
    /// <summary>
    /// Transparency with pre-multiplied alpha component.
    /// </summary>
    CEF_ALPHA_TYPE_PREMULTIPLIED,
    /// <summary>
    /// Transparency with post-multiplied alpha component.
    /// </summary>
    CEF_ALPHA_TYPE_POSTMULTIPLIED
  );

  /// <summary>
  /// Text style types. Should be kepy in sync with gfx::TextStyle.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_text_style_t)</see></para>
  /// </remarks>
  TCefTextStyle = (
    CEF_TEXT_STYLE_BOLD,
    CEF_TEXT_STYLE_ITALIC,
    CEF_TEXT_STYLE_STRIKE,
    CEF_TEXT_STYLE_DIAGONAL_STRIKE,
    CEF_TEXT_STYLE_UNDERLINE
  );

  /// <summary>
  /// Specifies where along the axis the CefBoxLayout child views should be laid
  /// out. Should be kept in sync with Chromium's views::LayoutAlignment type.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_axis_alignment_t)</see></para>
  /// </remarks>
  TCefAxisAlignment = (
    /// <summary>
    /// Child views will be left/top-aligned.
    /// </summary>
    CEF_AXIS_ALIGNMENT_START,
    /// <summary>
    /// Child views will be center-aligned.
    /// </summary>
    CEF_AXIS_ALIGNMENT_CENTER,
    /// <summary>
    /// Child views will be right/bottom-aligned.
    /// </summary>
    CEF_AXIS_ALIGNMENT_END,
    /// <summary>
    /// Child views will be stretched to fit.
    /// </summary>
    CEF_AXIS_ALIGNMENT_STRETCH
  );

  /// <summary>
  /// Margin type for PDF printing.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_pdf_print_margin_type_t)</see></para>
  /// </remarks>
  TCefPdfPrintMarginType = (
    /// <summary>
    /// Default margins of 1cm (~0.4 inches).
    /// </summary>
    PDF_PRINT_MARGIN_DEFAULT,
    /// <summary>
    /// No margins.
    /// </summary>
    PDF_PRINT_MARGIN_NONE,
    /// <summary>
    /// Custom margins using the |margin_*| values from TCefPdfPrintSettings.
    /// </summary>
    PDF_PRINT_MARGIN_CUSTOM
  );

  /// <summary>
  /// Print job color mode values.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_color_model_t)</see></para>
  /// </remarks>
  TCefColorModel = (
    COLOR_MODEL_UNKNOWN,
    COLOR_MODEL_GRAY,
    COLOR_MODEL_COLOR,
    COLOR_MODEL_CMYK,
    COLOR_MODEL_CMY,
    COLOR_MODEL_KCMY,
    /// <summary>
    /// CMY_K represents CMY+K.
    /// </summary>
    COLOR_MODEL_CMY_K,
    COLOR_MODEL_BLACK,
    COLOR_MODEL_GRAYSCALE,
    COLOR_MODEL_RGB,
    COLOR_MODEL_RGB16,
    COLOR_MODEL_RGBA,
    /// <summary>
    /// Used in samsung printer ppds.
    /// </summary>
    COLOR_MODEL_COLORMODE_COLOR,
    /// <summary>
    /// Used in samsung printer ppds.
    /// </summary>
    COLOR_MODEL_COLORMODE_MONOCHROME,
    /// <summary>
    /// Used in HP color printer ppds.
    /// </summary>
    COLOR_MODEL_HP_COLOR_COLOR,
    /// <summary>
    /// Used in HP color printer ppds.
    /// </summary>
    COLOR_MODEL_HP_COLOR_BLACK,
    /// <summary>
    /// Used in foomatic ppds.
    /// </summary>
    COLOR_MODEL_PRINTOUTMODE_NORMAL,
    /// <summary>
    /// Used in foomatic ppds.
    /// </summary>
    COLOR_MODEL_PRINTOUTMODE_NORMAL_GRAY,
    /// <summary>
    /// Used in canon printer ppds.
    /// </summary>
    COLOR_MODEL_PROCESSCOLORMODEL_CMYK,
    /// <summary>
    /// Used in canon printer ppds.
    /// </summary>
    COLOR_MODEL_PROCESSCOLORMODEL_GREYSCALE,
    /// <summary>
    /// Used in canon printer ppds
    /// </summary>
    COLOR_MODEL_PROCESSCOLORMODEL_RGB
  );

  /// <summary>
  /// Options that can be passed to CefParseJSON.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_json_parser_options_t)</see></para>
  /// </remarks>
  TCefJsonParserOptions = (
    /// <summary>
    /// Parses the input strictly according to RFC 4627. See comments in
    /// Chromium's base/json/json_reader.h file for known limitations/
    /// deviations from the RFC.
    /// </summary>
    JSON_PARSER_RFC = 0,
    /// <summary>
    /// Allows commas to exist after the last element in structures.
    /// </summary>
    JSON_PARSER_ALLOW_TRAILING_COMMAS = 1 shl 0
  );

  /// <summary>
  /// Supported XML encoding types. The parser supports ASCII, ISO-8859-1, and
  /// UTF16 (LE and BE) by default. All other types must be translated to UTF8
  /// before being passed to the parser. If a BOM is detected and the correct
  /// decoder is available then that decoder will be used automatically.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_xml_encoding_type_t)</see></para>
  /// </remarks>
  TCefXmlEncodingType = (
    XML_ENCODING_NONE = 0,
    XML_ENCODING_UTF8,
    XML_ENCODING_UTF16LE,
    XML_ENCODING_UTF16BE,
    XML_ENCODING_ASCII
  );

  /// <summary>
  /// XML node types.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_xml_node_type_t)</see></para>
  /// </remarks>
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

  /// <summary>
  /// DOM event processing phases.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_dom_event_phase_t)</see></para>
  /// </remarks>
  TCefDomEventPhase = (
    DOM_EVENT_PHASE_UNKNOWN = 0,
    DOM_EVENT_PHASE_CAPTURING,
    DOM_EVENT_PHASE_AT_TARGET,
    DOM_EVENT_PHASE_BUBBLING
  );

  /// <summary>
  /// Specifies the button display state.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_button_state_t)</see></para>
  /// </remarks>
  TCefButtonState = (
    CEF_BUTTON_STATE_NORMAL,
    CEF_BUTTON_STATE_HOVERED,
    CEF_BUTTON_STATE_PRESSED,
    CEF_BUTTON_STATE_DISABLED
  );

  /// <summary>
  /// Specifies the horizontal text alignment mode.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_horizontal_alignment_t)</see></para>
  /// </remarks>
  TCefHorizontalAlignment = (
    /// <summary>
    /// Align the text's left edge with that of its display area.
    /// </summary>
    CEF_HORIZONTAL_ALIGNMENT_LEFT,
    /// <summary>
    /// Align the text's center with that of its display area.
    /// </summary>
    CEF_HORIZONTAL_ALIGNMENT_CENTER,
    /// <summary>
    /// Align the text's right edge with that of its display area.
    /// </summary>
    CEF_HORIZONTAL_ALIGNMENT_RIGHT
  );

  /// <summary>
  /// Specifies how a menu will be anchored for non-RTL languages. The opposite
  /// position will be used for RTL languages.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_menu_anchor_position_t)</see></para>
  /// </remarks>
  TCefMenuAnchorPosition = (
    CEF_MENU_ANCHOR_TOPLEFT,
    CEF_MENU_ANCHOR_TOPRIGHT,
    CEF_MENU_ANCHOR_BOTTOMCENTER
  );

  /// <summary>
  /// Supported color types for menu items.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_menu_color_type_t)</see></para>
  /// </remarks>
  TCefMenuColorType = (
    CEF_MENU_COLOR_TEXT,
    CEF_MENU_COLOR_TEXT_HOVERED,
    CEF_MENU_COLOR_TEXT_ACCELERATOR,
    CEF_MENU_COLOR_TEXT_ACCELERATOR_HOVERED,
    CEF_MENU_COLOR_BACKGROUND,
    CEF_MENU_COLOR_BACKGROUND_HOVERED,
    CEF_MENU_COLOR_COUNT
  );

  /// <summary>
  /// Composition underline style.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_composition_underline_style_t)</see></para>
  /// </remarks>
  TCefCompositionUnderlineStyle = (
    CEF_CUS_SOLID,
    CEF_CUS_DOT,
    CEF_CUS_DASH,
    CEF_CUS_NONE
  );

  /// <summary>
  /// Permission request results.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_permission_request_result_t)</see></para>
  /// </remarks>
  TCefPermissionRequestResult = (
    /// <summary>
    /// Accept the permission request as an explicit user action.
    /// </summary>
    CEF_PERMISSION_RESULT_ACCEPT,
    /// <summary>
    /// Deny the permission request as an explicit user action.
    /// </summary>
    CEF_PERMISSION_RESULT_DENY,
    /// <summary>
    /// Dismiss the permission request as an explicit user action.
    /// </summary>
    CEF_PERMISSION_RESULT_DISMISS,
    /// <summary>
    /// Ignore the permission request. If the prompt remains unhandled (e.g.
    /// OnShowPermissionPrompt returns false and there is no default permissions
    /// UI) then any related promises may remain unresolved.
    /// </summary>
    CEF_PERMISSION_RESULT_IGNORE
  );

  /// <summary>
  /// Preferences type passed to
  /// ICefBrowserProcessHandler.OnRegisterCustomPreferences.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_preferences_type_t)</see></para>
  /// </remarks>
  TCefPreferencesType = (
    /// <summary>
    /// Global preferences registered a single time at application startup.
    /// </summary>
    CEF_PREFERENCES_TYPE_GLOBAL,
    /// <summary>
    /// Request context preferences registered each time a new CefRequestContext
    /// is created.
    /// </summary>
    CEF_PREFERENCES_TYPE_REQUEST_CONTEXT
  );

  /// <summary>
  /// Specifies the gesture commands.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_gesture_command_t)</see></para>
  /// </remarks>
  TCefGestureCommand = (
    CEF_GESTURE_COMMAND_BACK,
    CEF_GESTURE_COMMAND_FORWARD
  );

  /// <summary>
  /// Specifies the zoom commands supported by ICefBrowserHost.Zoom.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_zoom_command_t)</see></para>
  /// </remarks>
  TCefZoomCommand = (
    CEF_ZOOM_COMMAND_OUT,
    CEF_ZOOM_COMMAND_RESET,
    CEF_ZOOM_COMMAND_IN
  );

  /// <summary>
  /// Specifies the color variants supported by
  /// ICefRequestContext.SetChromeThemeColor.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_color_variant_t)</see></para>
  /// </remarks>
  TCefColorVariant = (
    CEF_COLOR_VARIANT_SYSTEM,
    CEF_COLOR_VARIANT_LIGHT,
    CEF_COLOR_VARIANT_DARK,
    CEF_COLOR_VARIANT_TONAL_SPOT,
    CEF_COLOR_VARIANT_NEUTRAL,
    CEF_COLOR_VARIANT_VIBRANT,
    CEF_COLOR_VARIANT_EXPRESSIVE
  );

  /// <summary>
  /// Specifies the gesture commands.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_test_cert_type_t)</see></para>
  /// </remarks>
  TCefTestCertType = (
    /// <summary>
    /// Valid certificate using the IP (127.0.0.1). Loads the "ok_cert.pem" file.
    /// </summary>
    CEF_TEST_CERT_OK_IP,
    /// <summary>
    /// Valid certificate using the domain ("localhost"). Loads the
    /// "localhost_cert.pem" file.
    /// </summary>
    CEF_TEST_CERT_OK_DOMAIN,
    /// <summary>
    /// Expired certificate. Loads the "expired_cert.pem" file.
    /// </summary>
    CEF_TEST_CERT_EXPIRED
   );

  /// <summary>
  /// Chrome page action icon types. Should be kept in sync with Chromium's
  /// PageActionIconType type.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_chrome_page_action_icon_type_t)</see></para>
  /// </remarks>
  TCefChromePageActionIconType = (
    CEF_CPAIT_BOOKMARK_STAR,
    CEF_CPAIT_CLICK_TO_CALL,
    CEF_CPAIT_COOKIE_CONTROLS,
    CEF_CPAIT_FILE_SYSTEM_ACCESS,
    CEF_CPAIT_FIND,
    CEF_CPAIT_MEMORY_SAVER,
    CEF_CPAIT_INTENT_PICKER,
    CEF_CPAIT_LOCAL_CARD_MIGRATION,
    CEF_CPAIT_MANAGE_PASSWORDS,
    CEF_CPAIT_PAYMENTS_OFFER_NOTIFICATION,
    CEF_CPAIT_PRICE_TRACKING,
    CEF_CPAIT_PWA_INSTALL,
    CEF_CPAIT_QR_CODE_GENERATOR_DEPRECATED,
    CEF_CPAIT_READER_MODE_DEPRECATED,
    CEF_CPAIT_SAVE_AUTOFILL_ADDRESS,
    CEF_CPAIT_SAVE_CARD,
    CEF_CPAIT_SEND_TAB_TO_SELF_DEPRECATED,
    CEF_CPAIT_SHARING_HUB,
    CEF_CPAIT_SIDE_SEARCH,
    CEF_CPAIT_SMS_REMOTE_FETCHER,
    CEF_CPAIT_TRANSLATE,
    CEF_CPAIT_VIRTUAL_CARD_ENROLL,
    CEF_CPAIT_VIRTUAL_CARD_MANUAL_FALLBACK,
    CEF_CPAIT_ZOOM,
    CEF_CPAIT_SAVE_IBAN,
    CEF_CPAIT_MANDATORY_REAUTH,
    CEF_CPAIT_PRICE_INSIGHTS,
    CEF_CPAIT_PRICE_READ_ANYTHING,
    CEF_CPAIT_PRODUCT_SPECIFICATIONS,
    CEF_CPAIT_LENS_OVERLAY,
    CEF_CPAIT_DISCOUNTS
    {* CEF_CPAIT_MAX_VALUE = CEF_CPAIT_DISCOUNTS *}
  );

  /// <summary>
  /// Specifies the task type variants supported by CefTaskManager.
  /// Should be kept in sync with Chromium's task_manager::Task::Type type.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_task_type_t)</see></para>
  /// </remarks>
  TCefTaskType = (
    CEF_TASK_TYPE_UNKNOWN = 0,
    /// <summary>
    /// The main browser process.
    /// </summary>
    CEF_TASK_TYPE_BROWSER,
    /// <summary>
    /// A graphics process.
    /// </summary>
    CEF_TASK_TYPE_GPU,
    /// <summary>
    /// A Linux zygote process.
    /// </summary>
    CEF_TASK_TYPE_ZYGOTE,
    /// <summary>
    /// A browser utility process.
    /// </summary>
    CEF_TASK_TYPE_UTILITY,
    /// <summary>
    /// A normal WebContents renderer process.
    /// </summary>
    CEF_TASK_TYPE_RENDERER,
    /// <summary>
    /// An extension or app process.
    /// </summary>
    CEF_TASK_TYPE_EXTENSION,
    /// <summary>
    /// A browser plugin guest process.
    /// </summary>
    CEF_TASK_TYPE_GUEST,
    /// <summary>
    /// A plugin process.
    /// </summary>
    CEF_TASK_TYPE_PLUGIN,
    /// <summary>
    /// A sandbox helper process.
    /// </summary>
    CEF_TASK_TYPE_SANDBOX_HELPER,
    /// <summary>
    /// A dedicated worker running on the renderer process.
    /// </summary>
    CEF_TASK_TYPE_DEDICATED_WORKER,
    /// <summary>
    /// A shared worker running on the renderer process.
    /// </summary>
    CEF_TASK_TYPE_SHARED_WORKER,
    /// <summary>
    /// A service worker running on the renderer process.
    /// </summary>
    CEF_TASK_TYPE_SERVICE_WORKER
  );

  /// <summary>
  /// Structure representing task information provided by ICefTaskManager.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_task_info_t)</see></para>
  /// </remarks>
  TCefTaskInfo = record
    /// <summary>
    /// The task ID.
    /// </summary>
    id                     : int64;
    /// <summary>
    /// The task type.
    /// </summary>
    type_                  : TCefTaskType;
    /// <summary>
    /// Set to true (1) if the task is killable.
    /// </summary>
    is_killable            : integer;
    /// <summary>
    /// The task title.
    /// </summary>
    title                  : TCefString;
    /// <summary>
    /// The CPU usage of the process on which the task is running. The value is
    /// in the range zero to number_of_processors * 100%.
    /// </summary>
    cpu_usage              : double;
    /// <summary>
    /// The number of processors available on the system.
    /// </summary>
    number_of_processors   : integer;
    /// <summary>
    /// The memory footprint of the task in bytes. A value of -1 means no valid
    /// value is currently available.
    /// </summary>
    memory                 : int64;
    /// <summary>
    /// The GPU memory usage of the task in bytes. A value of -1 means no valid
    /// value is currently available.
    /// </summary>
    gpu_memory             : int64;
    /// <summary>
    /// Set to true (1) if this task process' GPU resource count is inflated
    /// because it is counting other processes' resources (e.g, the GPU process
    /// has this value set to true because it is the aggregate of all processes).
    /// </summary>
    is_gpu_memory_inflated : integer;
  end;

  /// <summary>
  /// Pascal version of TCefTaskInfo.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_task_info_t)</see></para>
  /// </remarks>
  TCustomTaskInfo = record
    /// <summary>
    /// The task ID.
    /// </summary>
    id                     : int64;
    /// <summary>
    /// The task type.
    /// </summary>
    type_                  : TCefTaskType;
    /// <summary>
    /// Set to true (1) if the task is killable.
    /// </summary>
    is_killable            : boolean;
    /// <summary>
    /// The task title.
    /// </summary>
    title                  : ustring;
    /// <summary>
    /// The CPU usage of the process on which the task is running. The value is
    /// in the range zero to number_of_processors * 100%.
    /// </summary>
    cpu_usage              : double;
    /// <summary>
    /// The number of processors available on the system.
    /// </summary>
    number_of_processors   : integer;
    /// <summary>
    /// The memory footprint of the task in bytes. A value of -1 means no valid
    /// value is currently available.
    /// </summary>
    memory                 : int64;
    /// <summary>
    /// The GPU memory usage of the task in bytes. A value of -1 means no valid
    /// value is currently available.
    /// </summary>
    gpu_memory             : int64;
    /// <summary>
    /// Set to true (1) if this task process' GPU resource count is inflated
    /// because it is counting other processes' resources (e.g, the GPU process
    /// has this value set to true because it is the aggregate of all processes).
    /// </summary>
    is_gpu_memory_inflated : boolean;
  end;

  /// <summary>
  /// Chrome toolbar button types. Should be kept in sync with CEF's internal
  /// ToolbarButtonType type.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_time.h">CEF source file: /include/internal/cef_time.h (cef_chrome_toolbar_button_type_t)</see></para>
  /// </remarks>
  TCefChromeToolbarButtonType = (
    CEF_CTBT_CAST,
    CEF_CTBT_DOWNLOAD,
    CEF_CTBT_SEND_TAB_TO_SELF,
    CEF_CTBT_SIDE_PANEL
    {* CEF_CTBT_MAX_VALUE = CEF_CTBT_SIDE_PANEL *}
  );

  /// <summary>
  /// Touch handle state.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_time.h">CEF source file: /include/internal/cef_time.h (cef_touch_handle_state_t)</see></para>
  /// </remarks>
  TCefTouchHandleState = record
    /// <summary>
    /// Touch handle id. Increments for each new touch handle.
    /// </summary>
    touch_handle_id   : integer;
    /// <summary>
    /// Combination of TCefTouchHandleStateFlags values indicating what state
    /// is set.
    /// </summary>
    flags             : cardinal;
    /// <summary>
    /// Enabled state. Only set if |flags| contains CEF_THS_FLAG_ENABLED.
    /// </summary>
    enabled           : integer;
    /// <summary>
    /// Orientation state. Only set if |flags| contains CEF_THS_FLAG_ORIENTATION.
    /// </summary>
    orientation       : TCefHorizontalAlignment;
    mirror_vertical   : integer;
    mirror_horizontal : integer;
    /// <summary>
    /// Origin state. Only set if |flags| contains CEF_THS_FLAG_ORIGIN.
    /// </summary>
    origin            : TCefPoint;
    /// <summary>
    /// Alpha state. Only set if |flags| contains CEF_THS_FLAG_ALPHA.
    /// </summary>
    alpha             : single;
  end;

  /// <summary>
  /// Structure representing IME composition underline information. This is a thin
  /// wrapper around Blink's WebCompositionUnderline class and should be kept in
  /// sync with that.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_time.h">CEF source file: /include/internal/cef_time.h (cef_composition_underline_t)</see></para>
  /// </remarks>
  TCefCompositionUnderline = record
    /// <summary>
    /// Underline character range.
    /// </summary>
    range            : TCefRange;
    /// <summary>
    /// Text color.
    /// </summary>
    color            : TCefColor;
    /// <summary>
    /// Background color.
    /// </summary>
    background_color : TCefColor;
    /// <summary>
    /// Set to true (1) for thick underline.
    /// </summary>
    thick            : integer;
    /// <summary>
    /// Style.
    /// </summary>
    style            : TCefCompositionUnderlineStyle;
  end;
  TCefCompositionUnderlineDynArray = array of TCefCompositionUnderline;

  /// <summary>
  /// Represents a wall clock time in UTC. Values are not guaranteed to be
  /// monotonically non-decreasing and are subject to large amounts of skew.
  /// Time is stored internally as microseconds since the Windows epoch (1601).
  ///
  /// This is equivalent of Chromium `base::Time` (see base/time/time.h).
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_time.h">CEF source file:  /include/internal/cef_time.h (cef_basetime_t)</see></para>
  /// </remarks>
  TCefBaseTime = type int64;

  /// <summary>
  /// Time information. Values should always be in UTC.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_time.h">CEF source file: /include/internal/cef_time.h (cef_time_t)</see></para>
  /// </remarks>
  TCefTime = record
    /// <summary>
    /// Four or five digit year "2007" (1601 to 30827 on Windows, 1970 to 2038 on
    /// 32-bit POSIX)
    /// </summary>
    year         : Integer;
    /// <summary>
    /// 1-based month (values 1 = January, etc.)
    /// </summary>
    month        : Integer;
    /// <summary>
    /// 0-based day of week (0 = Sunday, etc.)
    /// </summary>
    day_of_week  : Integer;
    /// <summary>
    /// 1-based day of month (1-31)
    /// </summary>
    day_of_month : Integer;
    /// <summary>
    /// Hour within the current day (0-23)
    /// </summary>
    hour         : Integer;
    /// <summary>
    /// Minute within the current hour (0-59)
    /// </summary>
    minute       : Integer;
    /// <summary>
    /// Second within the current minute (0-59 plus leap seconds which may take
    /// it up to 60).
    /// </summary>
    second       : Integer;
    /// <summary>
    /// Milliseconds within the current second (0-999)
    /// </summary>
    millisecond  : Integer;
  end;

  /// <summary>
  /// Initialization settings. Specify NULL or 0 to get the recommended default
  /// values. Many of these and other settings can also configured using command-
  /// line switches.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_box_layout_settings_t)</see></para>
  /// </remarks>
  TCefBoxLayoutSettings = record
    /// <summary>
    /// If true (1) the layout will be horizontal, otherwise the layout will be
    /// vertical.
    /// </summary>
    horizontal                       : Integer;
    /// <summary>
    /// Adds additional horizontal space between the child view area and the host
    /// view border.
    /// </summary>
    inside_border_horizontal_spacing : Integer;
    /// <summary>
    /// Adds additional vertical space between the child view area and the host
    /// view border.
    /// </summary>
    inside_border_vertical_spacing   : Integer;
    /// <summary>
    /// Adds additional space around the child view area.
    /// </summary>
    inside_border_insets             : TCefInsets;
    /// <summary>
    /// Adds additional space between child views.
    /// </summary>
    between_child_spacing            : Integer;
    /// <summary>
    /// Specifies where along the main axis the child views should be laid out.
    /// </summary>
    main_axis_alignment              : TCefAxisAlignment;
    /// <summary>
    /// Specifies where along the cross axis the child views should be laid out.
    /// </summary>
    cross_axis_alignment             : TCefAxisAlignment;
    /// <summary>
    /// Minimum cross axis size.
    /// </summary>
    minimum_cross_axis_size          : Integer;
    /// <summary>
    /// Default flex for views when none is specified via CefBoxLayout methods.
    /// Using the preferred size as the basis, free space along the main axis is
    /// distributed to views in the ratio of their flex weights. Similarly, if the
    /// views will overflow the parent, space is subtracted in these ratios. A
    /// flex of 0 means this view is not resized. Flex values must not be
    /// negative.
    /// </summary>
    default_flex                     : Integer;
  end;

  /// <summary>
  /// Initialization settings. Specify NULL or 0 to get the recommended default
  /// values. Many of these and other settings can also configured using command-
  /// line switches.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_settings_t)</see></para>
  /// </remarks>
  TCefSettings = record
    /// <summary>
    /// Size of this structure.
    /// </summary>
    size                                     : NativeUInt;
    /// <summary>
    /// Set to true (1) to disable the sandbox for sub-processes. See
    /// cef_sandbox_win.h for requirements to enable the sandbox on Windows. Also
    /// configurable using the "no-sandbox" command-line switch.
    /// </summary>
    no_sandbox                               : Integer;
    /// <summary>
    /// The path to a separate executable that will be launched for sub-processes.
    /// If this value is empty on Windows or Linux then the main process
    /// executable will be used. If this value is empty on macOS then a helper
    /// executable must exist at "Contents/Frameworks/<app>
    /// Helper.app/Contents/MacOS/<app> Helper" in the top-level app bundle. See
    /// the comments on CefExecuteProcess() for details. If this value is
    /// non-empty then it must be an absolute path. Also configurable using the
    /// "browser-subprocess-path" command-line switch.
    /// </summary>
    browser_subprocess_path                  : TCefString;
    /// <summary>
    /// The path to the CEF framework directory on macOS. If this value is empty
    /// then the framework must exist at "Contents/Frameworks/Chromium Embedded
    /// Framework.framework" in the top-level app bundle. If this value is
    /// non-empty then it must be an absolute path. Also configurable using the
    /// "framework-dir-path" command-line switch.
    /// </summary>
    framework_dir_path                       : TCefString;
    /// <summary>
    /// The path to the main bundle on macOS. If this value is empty then it
    /// defaults to the top-level app bundle. If this value is non-empty then it
    /// must be an absolute path. Also configurable using the "main-bundle-path"
    /// command-line switch.
    /// </summary>
    main_bundle_path                         : TCefString;
    /// <summary>
    /// Set to true (1) to have the browser process message loop run in a separate
    /// thread. If false (0) then the CefDoMessageLoopWork() function must be
    /// called from your application message loop. This option is only supported
    /// on Windows and Linux.
    /// </summary>
    multi_threaded_message_loop              : Integer;
    /// <summary>
    /// Set to true (1) to control browser process main (UI) thread message pump
    /// scheduling via the ICefBrowserProcessHandler.OnScheduleMessagePumpWork()
    /// callback. This option is recommended for use in combination with the
    /// CefDoMessageLoopWork() function in cases where the CEF message loop must
    /// be integrated into an existing application message loop (see additional
    /// comments and warnings on CefDoMessageLoopWork). Enabling this option is
    /// not recommended for most users; leave this option disabled and use either
    /// the CefRunMessageLoop() function or multi_threaded_message_loop if
    /// possible.
    /// </summary>
    external_message_pump                    : Integer;
    /// <summary>
    /// Set to true (1) to enable windowless (off-screen) rendering support. Do
    /// not enable this value if the application does not use windowless rendering
    /// as it may reduce rendering performance on some systems.
    /// </summary>
    windowless_rendering_enabled             : Integer;
    /// <summary>
    /// Set to true (1) to disable configuration of browser process features using
    /// standard CEF and Chromium command-line arguments. Configuration can still
    /// be specified using CEF data structures or via the
    /// ICefApp.OnBeforeCommandLineProcessing() method.
    /// </summary>
    command_line_args_disabled               : Integer;
    /// <summary>
    /// The directory where data for the global browser cache will be stored on
    /// disk. If this value is non-empty then it must be an absolute path that is
    /// either equal to or a child directory of CefSettings.root_cache_path. If
    /// this value is empty then browsers will be created in "incognito mode"
    /// where in-memory caches are used for storage and no profile-specific data
    /// is persisted to disk (installation-specific data will still be persisted
    /// in root_cache_path). HTML5 databases such as localStorage will only
    /// persist across sessions if a cache path is specified. Can be overridden
    /// for individual CefRequestContext instances via the
    /// TCefRequestContextSettings.cache_path value. Any child directory value will
    /// be ignored and the "default" profile (also a child directory) will be used
    /// instead.
    /// </summary>
    cache_path                               : TCefString;
    /// <summary>
    /// <para>The root directory for installation-specific data and the parent directory
    /// for profile-specific data. All TCefSettings.cache_path and
    /// TCefRequestContextSettings.cache_path values must have this parent
    /// directory in common. If this value is empty and TCefSettings.cache_path is
    /// non-empty then it will default to the TCefSettings.cache_path value. Any
    /// non-empty value must be an absolute path. If both values are empty then
    /// the default platform-specific directory will be used
    /// ("~/.config/cef_user_data" directory on Linux, "~/Library/Application
    /// Support/CEF/User Data" directory on MacOS, "AppData\Local\CEF\User Data"
    /// directory under the user profile directory on Windows). Use of the default
    /// directory is not recommended in production applications (see below).</para>
    /// <para>Multiple application instances writing to the same root_cache_path
    /// directory could result in data corruption. A process singleton lock based
    /// on the root_cache_path value is therefore used to protect against this.
    /// This singleton behavior applies to all CEF-based applications using
    /// version 120 or newer. You should customize root_cache_path for your
    /// application and implement ICefBrowserProcessHandler.OnAlreadyRunningAppRelaunch,
    /// which will then be called on any app relaunch
    /// with the same root_cache_path value.</para>
    /// <para>Failure to set the root_cache_path value correctly may result in startup
    /// crashes or other unexpected behaviors (for example, the sandbox blocking
    /// read/write access to certain files).</para>
    /// </summary>
    root_cache_path                          : TCefString;
    /// <summary>
    /// To persist session cookies (cookies without an expiry date or validity
    /// interval) by default when using the global cookie manager set this value
    /// to true (1). Session cookies are generally intended to be transient and
    /// most Web browsers do not persist them. A |cache_path| value must also be
    /// specified to enable this feature. Also configurable using the
    /// "persist-session-cookies" command-line switch. Can be overridden for
    /// individual CefRequestContext instances via the
    /// TCefRequestContextSettings.persist_session_cookies value.
    /// </summary>
    persist_session_cookies                  : Integer;
    /// <summary>
    /// Value that will be returned as the User-Agent HTTP header. If empty the
    /// default User-Agent string will be used. Also configurable using the
    /// "user-agent" command-line switch.
    /// </summary>
    user_agent                               : TCefString;
    /// <summary>
    /// Value that will be inserted as the product portion of the default
    /// User-Agent string. If empty the Chromium product version will be used. If
    /// |userAgent| is specified this value will be ignored. Also configurable
    /// using the "user-agent-product" command-line switch.
    /// </summary>
    user_agent_product                       : TCefString;
    /// <summary>
    /// The locale string that will be passed to WebKit. If empty the default
    /// locale of "en-US" will be used. This value is ignored on Linux where
    /// locale is determined using environment variable parsing with the
    /// precedence order: LANGUAGE, LC_ALL, LC_MESSAGES and LANG. Also
    /// configurable using the "lang" command-line switch.
    /// </summary>
    locale                                   : TCefString;
    /// <summary>
    /// The directory and file name to use for the debug log. If empty a default
    /// log file name and location will be used. On Windows and Linux a
    /// "debug.log" file will be written in the main executable directory. On
    /// MacOS a "~/Library/Logs/[app name]_debug.log" file will be written where
    /// [app name] is the name of the main app executable. Also configurable using
    /// the "log-file" command-line switch.
    /// </summary>
    log_file                                 : TCefString;
    /// <summary>
    /// The log severity. Only messages of this severity level or higher will be
    /// logged. When set to DISABLE no messages will be written to the log file,
    /// but FATAL messages will still be output to stderr. Also configurable using
    /// the "log-severity" command-line switch with a value of "verbose", "info",
    /// "warning", "error", "fatal" or "disable".
    /// </summary>
    log_severity                             : TCefLogSeverity;
    /// <summary>
    /// The log items prepended to each log line. If not set the default log items
    /// will be used. Also configurable using the "log-items" command-line switch
    /// with a value of "none" for no log items, or a comma-delimited list of
    /// values "pid", "tid", "timestamp" or "tickcount" for custom log items.
    /// </summary>
    log_items                                : TCefLogItems;
    /// <summary>
    /// Custom flags that will be used when initializing the V8 JavaScript engine.
    /// The consequences of using custom flags may not be well tested. Also
    /// configurable using the "js-flags" command-line switch.
    /// </summary>
    javascript_flags                         : TCefString;
    /// <summary>
    /// The fully qualified path for the resources directory. If this value is
    /// empty the *.pak files must be located in the module directory on
    /// Windows/Linux or the app bundle Resources directory on MacOS. If this
    /// value is non-empty then it must be an absolute path. Also configurable
    /// using the "resources-dir-path" command-line switch.
    /// </summary>
    resources_dir_path                       : TCefString;
    /// <summary>
    /// The fully qualified path for the locales directory. If this value is empty
    /// the locales directory must be located in the module directory. If this
    /// value is non-empty then it must be an absolute path. This value is ignored
    /// on MacOS where pack files are always loaded from the app bundle Resources
    /// directory. Also configurable using the "locales-dir-path" command-line
    /// switch.
    /// </summary>
    locales_dir_path                         : TCefString;
    /// <summary>
    /// Set to a value between 1024 and 65535 to enable remote debugging on the
    /// specified port. Also configurable using the "remote-debugging-port"
    /// command-line switch. Specifying 0 via the command-line switch will result
    /// in the selection of an ephemeral port and the port number will be printed
    /// as part of the WebSocket endpoint URL to stderr. If a cache directory path
    /// is provided the port will also be written to the
    /// <cache-dir>/DevToolsActivePort file. Remote debugging can be accessed by
    /// loading the chrome://inspect page in Google Chrome. Port numbers 9222 and
    /// 9229 are discoverable by default. Other port numbers may need to be
    /// configured via "Discover network targets" on the Devices tab.
    /// </summary>
    remote_debugging_port                    : Integer;
    /// <summary>
    /// The number of stack trace frames to capture for uncaught exceptions.
    /// Specify a positive value to enable the
    /// ICefRenderProcessHandler.OnUncaughtException() callback. Specify 0
    /// (default value) and OnUncaughtException() will not be called. Also
    /// configurable using the "uncaught-exception-stack-size" command-line
    /// switch.
    /// </summary>
    uncaught_exception_stack_size            : Integer;
    /// <summary>
    /// Background color used for the browser before a document is loaded and when
    /// no document color is specified. The alpha component must be either fully
    /// opaque (0xFF) or fully transparent (0x00). If the alpha component is fully
    /// opaque then the RGB components will be used as the background color. If
    /// the alpha component is fully transparent for a windowed browser then the
    /// default value of opaque white be used. If the alpha component is fully
    /// transparent for a windowless (off-screen) browser then transparent
    /// painting will be enabled.
    /// </summary>
    background_color                         : TCefColor;
    /// <summary>
    /// Comma delimited ordered list of language codes without any whitespace that
    /// will be used in the "Accept-Language" HTTP request header and
    /// "navigator.language" JS attribute. Can be overridden for individual
    /// ICefRequestContext instances via the
    /// TCefRequestContextSettings.accept_language_list value.
    /// </summary>
    accept_language_list                     : TCefString;
    /// <summary>
    /// Comma delimited list of schemes supported by the associated
    /// ICefCookieManager. If |cookieable_schemes_exclude_defaults| is false (0)
    /// the default schemes ("http", "https", "ws" and "wss") will also be
    /// supported. Not specifying a |cookieable_schemes_list| value and setting
    /// |cookieable_schemes_exclude_defaults| to true (1) will disable all loading
    /// and saving of cookies. These settings will only impact the global
    /// ICefRequestContext. Individual ICefRequestContext instances can be
    /// configured via the TCefRequestContextSettings.cookieable_schemes_list and
    /// TCefRequestContextSettings.cookieable_schemes_exclude_defaults values.
    /// </summary>
    cookieable_schemes_list                  : TCefString;
    cookieable_schemes_exclude_defaults      : integer;
    /// <summary>
    /// <para>Specify an ID to enable Chrome policy management via Platform and OS-user
    /// policies. On Windows, this is a registry key like
    /// "SOFTWARE\\Policies\\Google\\Chrome". On MacOS, this is a bundle ID like
    /// "com.google.Chrome". On Linux, this is an absolute directory path like
    /// "/etc/opt/chrome/policies". Only supported with Chrome style. See
    /// https://support.google.com/chrome/a/answer/9037717 for details.</para>
    /// <para>Chrome Browser Cloud Management integration, when enabled via the
    /// "enable-chrome-browser-cloud-management" command-line flag, will also use
    /// the specified ID. See https://support.google.com/chrome/a/answer/9116814
    /// for details.</para>
    /// </summary>
    chrome_policy_id                        : TCefString;
    /// <summary>
    /// Specify an ID for an ICON resource that can be loaded from the main
    /// executable and used when creating default Chrome windows such as DevTools
    /// and Task Manager. If unspecified the default Chromium ICON (IDR_MAINFRAME
    /// [101]) will be loaded from libcef.dll. Only supported with Chrome style on
    /// Windows.
    /// </summary>
    chrome_app_icon_id                      : Integer;
    {$IF DEFINED(OS_POSIX) AND NOT(DEFINED(ANDROID))}
    /// <summary>
    /// Specify whether signal handlers must be disabled on POSIX systems.
    /// </summary>
    disable_signal_handlers                 : Integer;
    {$IFEND}
  end;

  /// <summary>
  /// <para>CEF supports both a Chrome runtime style (based on the Chrome UI layer) and
  /// an Alloy runtime style (based on the Chromium content layer). Chrome style
  /// provides the full Chrome UI and browser functionality whereas Alloy style
  /// provides less default browser functionality but adds additional client
  /// callbacks and support for windowless (off-screen) rendering. The style type
  /// is individually configured for each window/browser at creation time and
  /// different styles can be mixed during runtime. For additional comparative
  /// details on runtime styles see
  /// https://bitbucket.org/chromiumembedded/cef/wiki/Architecture.md#markdown-header-cef3</para>
  ///
  /// <para>Windowless rendering will always use Alloy style. Windowed rendering with a
  /// default window or client-provided parent window can configure the style via
  /// TCefWindowInfo.runtime_style. Windowed rendering with the Views framework can
  /// configure the style via ICefWindowDelegate.GetWindowRuntimeStyle and
  /// ICefBrowserViewDelegate.GetBrowserRuntimeStyle. Alloy style Windows with the
  /// Views framework can host only Alloy style BrowserViews but Chrome style
  /// Windows can host both style BrowserViews. Additionally, a Chrome style
  /// Window can host at most one Chrome style BrowserView but potentially
  /// multiple Alloy style BrowserViews. See TCefWindowInfo.runtime_style
  /// documentation for any additional platform-specific limitations.</para>
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types_runtime.h">CEF source file: /include/internal/cef_types_runtime.h (cef_runtime_style_t)</see></para>
  /// </remarks>
  TCefRuntimeStyle = (
    /// <summary>
    /// Use the default style. See above documentation for exceptions.
    /// </summary>
    CEF_RUNTIME_STYLE_DEFAULT,
    /// <summary>
    /// Use Chrome style.
    /// </summary>
    CEF_RUNTIME_STYLE_CHROME,
    /// <summary>
    /// Use Alloy style.
    /// </summary>
    CEF_RUNTIME_STYLE_ALLOY
  );

  /// <summary>
  /// Structure representing window information.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types_win.h">CEF source file: /include/internal/cef_types_win.h (cef_window_info_t)</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types_mac.h">CEF source file: /include/internal/cef_types_mac.h (cef_window_info_t)</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types_linux.h">CEF source file: /include/internal/cef_types_linux.h (cef_window_info_t)</see></para>
  /// </remarks>
  TCefWindowInfo = record
    {$IFDEF MSWINDOWS}
    /// <summary>
    /// Standard parameters required by CreateWindowEx()
    /// </summary>
    ex_style                      : DWORD;
    window_name                   : TCefString;
    style                         : DWORD;
    bounds                        : TCefRect;
    parent_window                 : TCefWindowHandle;
    menu                          : HMENU;
    /// <summary>
    /// Set to true (1) to create the browser using windowless (off-screen)
    /// rendering. No window will be created for the browser and all rendering
    /// will occur via the ICefRenderHandler interface. The |parent_window| value
    /// will be used to identify monitor info and to act as the parent window for
    /// dialogs, context menus, etc. If |parent_window| is not provided then the
    /// main screen monitor will be used and some functionality that requires a
    /// parent window may not function correctly. In order to create windowless
    /// browsers the TCefSettings.windowless_rendering_enabled value must be set to
    /// true. Transparent painting is enabled by default but can be disabled by
    /// setting TCefBrowserSettings.background_color to an opaque value.
    /// </summary>
    windowless_rendering_enabled  : Integer;
    /// <summary>
    /// Set to true (1) to enable shared textures for windowless rendering. Only
    /// valid if windowless_rendering_enabled above is also set to true. Currently
    /// only supported on Windows (D3D11).
    /// </summary>
    shared_texture_enabled        : Integer;
    /// <summary>
    /// Set to true (1) to enable the ability to issue BeginFrame requests from
    /// the client application by calling ICefBrowserHost.SendExternalBeginFrame.
    /// </summary>
    external_begin_frame_enabled  : Integer;
    /// <summary>
    /// Handle for the new browser window. Only used with windowed rendering.
    /// </summary>
    window                        : TCefWindowHandle;
    /// <summary>
    /// Optionally change the runtime style. Alloy style will always be used if
    /// |windowless_rendering_enabled| is true. See TCefRuntimeStyle
    /// documentation for details.
    /// </summary>
    runtime_style                 : TCefRuntimeStyle;
    {$ENDIF}
    {$IFDEF MACOSX}
    window_name                   : TCefString;
    /// <summary>
    /// Initial window bounds.
    /// </summary>
    bounds                        : TCefRect;
    /// <summary>
    /// Set to true (1) to create the view initially hidden.
    /// </summary>
    hidden                        : Integer;
    /// <summary>
    /// NSView pointer for the parent view.
    /// </summary>
    parent_view                   : TCefWindowHandle;
    /// <summary>
    /// Set to true (1) to create the browser using windowless (off-screen)
    /// rendering. No view will be created for the browser and all rendering will
    /// occur via the CefRenderHandler interface. The |parent_view| value will be
    /// used to identify monitor info and to act as the parent view for dialogs,
    /// context menus, etc. If |parent_view| is not provided then the main screen
    /// monitor will be used and some functionality that requires a parent view
    /// may not function correctly. In order to create windowless browsers the
    /// TCefSettings.windowless_rendering_enabled value must be set to true.
    /// Transparent painting is enabled by default but can be disabled by setting
    /// TCefBrowserSettings.background_color to an opaque value.
    /// </summary>
    windowless_rendering_enabled  : Integer;
    /// <summary>
    /// Set to true (1) to enable shared textures for windowless rendering. Only
    /// valid if windowless_rendering_enabled above is also set to true. Currently
    /// only supported on Windows (D3D11).
    /// </summary>
    shared_texture_enabled        : Integer;
    /// <summary>
    /// Set to true (1) to enable the ability to issue BeginFrame from the client
    /// application.
    /// </summary>
    external_begin_frame_enabled  : Integer;
    /// <summary>
    /// NSView pointer for the new browser view. Only used with windowed
    /// rendering.
    /// </summary>
    view                          : TCefWindowHandle;
    /// <summary>
    /// Optionally change the runtime style. Alloy style will always be used if
    /// |windowless_rendering_enabled| is true or if |parent_view| is provided.
    /// See TCefRuntimeStyle documentation for details.
    /// </summary>
    runtime_style                 : TCefRuntimeStyle;
    {$ENDIF}
    {$IFDEF LINUX}
    /// <summary>
    /// The initial title of the window, to be set when the window is created.
    /// Some layout managers (e.g., Compiz) can look at the window title
    /// in order to decide where to place the window when it is
    /// created. When this attribute is not empty, the window title will
    /// be set before the window is mapped to the dispay. Otherwise the
    /// title will be initially empty.
    /// </summary>
    window_name                   : TCefString;
    /// <summary>
    /// Initial window bounds.
    /// </summary>
    bounds                        : TCefRect;
    /// <summary>
    /// Pointer for the parent window.
    /// </summary>
    parent_window                 : TCefWindowHandle;
    /// <summary>
    /// Set to true (1) to create the browser using windowless (off-screen)
    /// rendering. No window will be created for the browser and all rendering
    /// will occur via the ICefRenderHandler interface. The |parent_window| value
    /// will be used to identify monitor info and to act as the parent window for
    /// dialogs, context menus, etc. If |parent_window| is not provided then the
    /// main screen monitor will be used and some functionality that requires a
    /// parent window may not function correctly. In order to create windowless
    /// browsers the TCefSettings.windowless_rendering_enabled value must be set to
    /// true. Transparent painting is enabled by default but can be disabled by
    /// setting TCefBrowserSettings.background_color to an opaque value.
    /// </summary>
    windowless_rendering_enabled  : Integer;
    /// <summary>
    /// Set to true (1) to enable shared textures for windowless rendering. Only
    /// valid if windowless_rendering_enabled above is also set to true. Currently
    /// only supported on Windows (D3D11).
    /// </summary>
    shared_texture_enabled        : Integer;
    /// <summary>
    /// Set to true (1) to enable the ability to issue BeginFrame requests from
    /// the client application by calling ICefBrowserHost.SendExternalBeginFrame.
    /// </summary>
    external_begin_frame_enabled  : Integer;
    /// <summary>
    /// Pointer for the new browser window. Only used with windowed rendering.
    /// </summary>
    window                        : TCefWindowHandle;
    /// <summary>
    /// Optionally change the runtime style. Alloy style will always be used if
    /// |windowless_rendering_enabled| is true. See TCefRuntimeStyle
    /// documentation for details.
    /// </summary>
    runtime_style                 : TCefRuntimeStyle;
    {$ENDIF}
  end;

  /// <summary>
  /// Structure representing a draggable region.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_draggable_region_t)</see></para>
  /// </remarks>
  TCefDraggableRegion = record
    /// <summary>
    /// Bounds of the region.
    /// </summary>
    bounds    : TCefRect;
    /// <summary>
    /// True (1) this this region is draggable and false (0) otherwise.
    /// </summary>
    draggable : Integer;
  end;

  TCefDraggableRegionArray = array[0..(High(Integer) div SizeOf(TCefDraggableRegion))-1]  of TCefDraggableRegion;

  /// <summary>
  /// Structure representing keyboard event information.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_key_event_t)</see></para>
  /// </remarks>
  TCefKeyEvent = record
    /// <summary>
    /// The type of keyboard event. It's called 'type' in the original CEF source code.
    /// </summary>
    kind                    : TCefKeyEventType;
    /// <summary>
    /// Bit flags describing any pressed modifier keys. See
    /// TCefEventFlags for values.
    /// </summary>
    modifiers               : TCefEventFlags;
    /// <summary>
    /// The Windows key code for the key event. This value is used by the DOM
    /// specification. Sometimes it comes directly from the event (i.e. on
    /// Windows) and sometimes it's determined using a mapping function. See
    /// WebCore/platform/chromium/KeyboardCodes.h for the list of values.
    /// </summary>
    windows_key_code        : Integer;
    /// <summary>
    /// The actual key code genenerated by the platform.
    /// </summary>
    native_key_code         : Integer;
    /// <summary>
    /// Indicates whether the event is considered a "system key" event (see
    /// http://msdn.microsoft.com/en-us/library/ms646286(VS.85).aspx for details).
    /// This value will always be false on non-Windows platforms.
    /// </summary>
    is_system_key           : Integer;
    /// <summary>
    /// The character generated by the keystroke.
    /// </summary>
    character               : WideChar;
    /// <summary>
    /// Same as |character| but unmodified by any concurrently-held modifiers
    /// (except shift). This is useful for working out shortcut keys.
    /// </summary>
    unmodified_character    : WideChar;
    /// <summary>
    /// True if the focus is currently on an editable field on the page. This is
    /// useful for determining if standard key events should be intercepted.
    /// </summary>
    focus_on_editable_field : Integer;
  end;

  /// <summary>
  /// Popup window features.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_popup_features_t)</see></para>
  /// </remarks>
  TCefPopupFeatures = record
    x                 : Integer;
    xSet              : Integer;
    y                 : Integer;
    ySet              : Integer;
    width             : Integer;
    widthSet          : Integer;
    height            : Integer;
    heightSet         : Integer;
    /// <summary>
    /// True (1) if browser interface elements should be hidden.
    /// </summary>
    isPopup           : Integer;
  end;

  /// <summary>
  /// Browser initialization settings. Specify NULL or 0 to get the recommended
  /// default values. The consequences of using custom values may not be well
  /// tested. Many of these and other settings can also configured using command-
  /// line switches.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_browser_settings_t)</see></para>
  /// </remarks>
  TCefBrowserSettings = record
    /// <summary>
    /// Size of this structure.
    /// </summary>
    size                            : NativeUInt;
    /// <summary>
    /// The maximum rate in frames per second (fps) that ICefRenderHandler.OnPaint
    /// will be called for a windowless browser. The actual fps may be lower if
    /// the browser cannot generate frames at the requested rate. The minimum
    /// value is 1 and the maximum value is 60 (default 30). This value can also
    /// be changed dynamically via ICefBrowserHost.SetWindowlessFrameRate.
    /// </summary>
    windowless_frame_rate           : Integer;
    /// <summary>
    /// Font settings.
    /// </summary>
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
    /// <summary>
    /// Default encoding for Web content. If empty "ISO-8859-1" will be used. Also
    /// configurable using the "default-encoding" command-line switch.
    /// </summary>
    default_encoding                : TCefString;
    /// <summary>
    /// Controls the loading of fonts from remote sources. Also configurable using
    /// the "disable-remote-fonts" command-line switch.
    /// </summary>
    remote_fonts                    : TCefState;
    /// <summary>
    /// Controls whether JavaScript can be executed. Also configurable using the
    /// "disable-javascript" command-line switch.
    /// </summary>
    javascript                      : TCefState;
    /// <summary>
    /// Controls whether JavaScript can be used to close windows that were not
    /// opened via JavaScript. JavaScript can still be used to close windows that
    /// were opened via JavaScript or that have no back/forward history. Also
    /// configurable using the "disable-javascript-close-windows" command-line
    /// switch.
    /// </summary>
    javascript_close_windows        : TCefState;
    /// <summary>
    /// Controls whether JavaScript can access the clipboard. Also configurable
    /// using the "disable-javascript-access-clipboard" command-line switch.
    /// </summary>
    javascript_access_clipboard     : TCefState;
    /// <summary>
    /// Controls whether DOM pasting is supported in the editor via
    /// execCommand("paste"). The |javascript_access_clipboard| setting must also
    /// be enabled. Also configurable using the "disable-javascript-dom-paste"
    /// command-line switch.
    /// </summary>
    javascript_dom_paste            : TCefState;
    /// <summary>
    /// Controls whether image URLs will be loaded from the network. A cached
    /// image will still be rendered if requested. Also configurable using the
    /// "disable-image-loading" command-line switch.
    /// </summary>
    image_loading                   : TCefState;
    /// <summary>
    /// Controls whether standalone images will be shrunk to fit the page. Also
    /// configurable using the "image-shrink-standalone-to-fit" command-line
    /// switch.
    /// </summary>
    image_shrink_standalone_to_fit  : TCefState;
    /// <summary>
    /// Controls whether text areas can be resized. Also configurable using the
    /// "disable-text-area-resize" command-line switch.
    /// </summary>
    text_area_resize                : TCefState;
    /// <summary>
    /// Controls whether the tab key can advance focus to links. Also configurable
    /// using the "disable-tab-to-links" command-line switch.
    /// </summary>
    tab_to_links                    : TCefState;
    /// <summary>
    /// Controls whether local storage can be used. Also configurable using the
    /// "disable-local-storage" command-line switch.
    /// </summary>
    local_storage                   : TCefState;
    /// <summary>
    /// Controls whether databases can be used. Also configurable using the
    /// "disable-databases" command-line switch.
    /// </summary>
    databases                       : TCefState;
    /// <summary>
    /// Controls whether WebGL can be used. Note that WebGL requires hardware
    /// support and may not work on all systems even when enabled. Also
    /// configurable using the "disable-webgl" command-line switch.
    /// </summary>
    webgl                           : TCefState;
    /// <summary>
    /// Background color used for the browser before a document is loaded and when
    /// no document color is specified. The alpha component must be either fully
    /// opaque (0xFF) or fully transparent (0x00). If the alpha component is fully
    /// opaque then the RGB components will be used as the background color. If
    /// the alpha component is fully transparent for a windowed browser then the
    /// TCefSettings.background_color value will be used. If the alpha component is
    /// fully transparent for a windowless (off-screen) browser then transparent
    /// painting will be enabled.
    /// </summary>
    background_color                : TCefColor;
    /// <summary>
    /// Controls whether the Chrome status bubble will be used. Only supported
    /// with Chrome style. For details about the status bubble see
    /// https://www.chromium.org/user-experience/status-bubble/
    /// </summary>
    chrome_status_bubble            : TCefState;
    /// <summary>
    /// Controls whether the Chrome zoom bubble will be shown when zooming. Only
    /// supported with Chrome style.
    /// </summary>
    chrome_zoom_bubble              : TCefState;
  end;

  /// <summary>
  /// Screen information used when window rendering is disabled. This structure is
  /// passed as a parameter to ICefRenderHandler.GetScreenInfo and should be
  /// filled in by the client.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_screen_info_t)</see></para>
  /// </remarks>
  TCefScreenInfo = record
    /// <summary>
    /// Device scale factor. Specifies the ratio between physical and logical
    /// pixels.
    /// </summary>
    device_scale_factor : single;
    /// <summary>
    /// The screen depth in bits per pixel.
    /// </summary>
    depth               : integer;
    /// <summary>
    /// The bits per color component. This assumes that the colors are balanced
    /// equally.
    /// </summary>
    depth_per_component : integer;
    /// <summary>
    /// This can be true for black and white printers.
    /// </summary>
    is_monochrome       : integer;
    /// <summary>
    /// This is set from the rcMonitor member of MONITORINFOEX, to whit:
    ///   "A RECT structure that specifies the display monitor rectangle,
    ///   expressed in virtual-screen coordinates. Note that if the monitor
    ///   is not the primary display monitor, some of the rectangle's
    ///   coordinates may be negative values."
    //
    /// The |rect| and |available_rect| properties are used to determine the
    /// available surface for rendering popup views.
    /// </summary>
    rect                : TCefRect;
    /// <summary>
    /// This is set from the rcWork member of MONITORINFOEX, to whit:
    ///   "A RECT structure that specifies the work area rectangle of the
    ///   display monitor that can be used by applications, expressed in
    ///   virtual-screen coordinates. Windows uses this rectangle to
    ///   maximize an application on the monitor. The rest of the area in
    ///   rcMonitor contains system windows such as the task bar and side
    ///   bars. Note that if the monitor is not the primary display monitor,
    ///   some of the rectangle's coordinates may be negative values".
    //
    /// The |rect| and |available_rect| properties are used to determine the
    /// available surface for rendering popup views.
    /// </summary>
    available_rect      : TCefRect;
  end;

  /// <summary>
  /// Request context initialization settings. Specify NULL or 0 to get the
  /// recommended default values.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_request_context_settings_t)</see></para>
  /// </remarks>
  TCefRequestContextSettings = record
    /// <summary>
    /// Size of this structure.
    /// </summary>
    size                                     : NativeUInt;
    /// <summary>
    /// The directory where cache data for this request context will be stored on
    /// disk. If this value is non-empty then it must be an absolute path that is
    /// either equal to or a child directory of TCefSettings.root_cache_path. If
    /// this value is empty then browsers will be created in "incognito mode"
    /// where in-memory caches are used for storage and no profile-specific data
    /// is persisted to disk (installation-specific data will still be persisted
    /// in root_cache_path). HTML5 databases such as localStorage will only
    /// persist across sessions if a cache path is specified. To share the global
    /// browser cache and related configuration set this value to match the
    /// TCefSettings.cache_path value.
    /// </summary>
    cache_path                               : TCefString;
    /// <summary>
    /// To persist session cookies (cookies without an expiry date or validity
    /// interval) by default when using the global cookie manager set this value
    /// to true (1). Session cookies are generally intended to be transient and
    /// most Web browsers do not persist them. Can be set globally using the
    /// TCefSettings.persist_session_cookies value. This value will be ignored if
    /// |cache_path| is empty or if it matches the TCefSettings.cache_path value.
    /// </summary>
    persist_session_cookies                  : Integer;
    /// <summary>
    /// Comma delimited ordered list of language codes without any whitespace that
    /// will be used in the "Accept-Language" HTTP header. Can be set globally
    /// using the TCefSettings.accept_language_list value or overridden on a per-
    /// browser basis using the TCefBrowserSettings.accept_language_list value. If
    /// all values are empty then "en-US,en" will be used. This value will be
    /// ignored if |cache_path| matches the TCefSettings.cache_path value.
    /// </summary>
    accept_language_list                     : TCefString;
    /// <summary>
    /// Comma delimited list of schemes supported by the associated
    /// ICefCookieManager. If |cookieable_schemes_exclude_defaults| is false (0)
    /// the default schemes ("http", "https", "ws" and "wss") will also be
    /// supported. Not specifying a |cookieable_schemes_list| value and setting
    /// |cookieable_schemes_exclude_defaults| to true (1) will disable all loading
    /// and saving of cookies. These values will be ignored if |cache_path|
    /// matches the TCefSettings.cache_path value.
    /// </summary>
    cookieable_schemes_list                  : TCefString;
    cookieable_schemes_exclude_defaults      : integer;
  end;

  /// <summary>
  /// Cookie information.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_cookie_t)</see></para>
  /// </remarks>
  TCefCookie = record
    /// <summary>
    /// The cookie name.
    /// </summary>
    name        : TCefString;
    /// <summary>
    /// The cookie value.
    /// </summary>
    value       : TCefString;
    /// <summary>
    /// If |domain| is empty a host cookie will be created instead of a domain
    /// cookie. Domain cookies are stored with a leading "." and are visible to
    /// sub-domains whereas host cookies are not.
    /// </summary>
    domain      : TCefString;
    /// <summary>
    /// If |path| is non-empty only URLs at or below the path will get the cookie
    /// value.
    /// </summary>
    path        : TCefString;
    /// <summary>
    /// If |secure| is true the cookie will only be sent for HTTPS requests.
    /// </summary>
    secure      : Integer;
    /// <summary>
    /// If |httponly| is true the cookie will only be sent for HTTP requests.
    /// </summary>
    httponly    : Integer;
    /// <summary>
    /// The cookie creation date. This is automatically populated by the system on
    /// cookie creation.
    /// </summary>
    creation    : TCefBaseTime;
    /// <summary>
    /// The cookie last access date. This is automatically populated by the system
    /// on access.
    /// </summary>
    last_access : TCefBaseTime;
    /// <summary>
    /// The cookie expiration date is only valid if |has_expires| is true.
    /// </summary>
    has_expires : Integer;
    expires     : TCefBaseTime;
    /// <summary>
    /// Same site.
    /// </summary>
    same_site   : TCefCookieSameSite;
    /// <summary>
    /// Priority.
    /// </summary>
    priority    : TCefCookiePriority;
  end;

  /// <summary>
  /// Cookie information.
  /// </summary>
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

  /// <summary>
  /// Structure representing PDF print settings. These values match the parameters
  /// supported by the DevTools Page.printToPDF function. See
  /// https://chromedevtools.github.io/devtools-protocol/tot/Page/#method-printToPDF
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_pdf_print_settings_t)</see></para>
  /// </remarks>
  TCefPdfPrintSettings = record
    /// <summary>
    /// Set to true (1) for landscape mode or false (0) for portrait mode.
    /// </summary>
    landscape             : Integer;
    /// <summary>
    /// Set to true (1) to print background graphics.
    /// </summary>
    print_background      : Integer;
    /// <summary>
    /// The percentage to scale the PDF by before printing (e.g. .5 is 50%).
    /// If this value is less than or equal to zero the default value of 1.0
    /// will be used.
    /// </summary>
    scale                 : double;
    /// <summary>
    /// Output paper size in inches. If either of these values is less than or
    /// equal to zero then the default paper size (letter, 8.5 x 11 inches) will
    /// be used.
    /// </summary>
    paper_width           : double;
    paper_height          : double;
    /// <summary>
    /// Set to true (1) to prefer page size as defined by css. Defaults to false
    /// (0), in which case the content will be scaled to fit the paper size.
    /// </summary>
    prefer_css_page_size  : Integer;
    /// <summary>
    /// Margin type.
    /// </summary>
    margin_type           : TCefPdfPrintMarginType;
    /// <summary>
    /// Margins in inches. Only used if |margin_type| is set to
    /// PDF_PRINT_MARGIN_CUSTOM.
    /// </summary>
    margin_top            : double;
    margin_right          : double;
    margin_bottom         : double;
    margin_left           : double;
    /// <summary>
    /// Paper ranges to print, one based, e.g., '1-5, 8, 11-13'. Pages are printed
    /// in the document order, not in the order specified, and no more than once.
    /// Defaults to empty string, which implies the entire document is printed.
    /// The page numbers are quietly capped to actual page count of the document,
    /// and ranges beyond the end of the document are ignored. If this results in
    /// no pages to print, an error is reported. It is an error to specify a range
    /// with start greater than end.
    /// </summary>
    page_ranges           : TCefString;
    /// <summary>
    /// Set to true (1) to display the header and/or footer. Modify
    /// |header_template| and/or |footer_template| to customize the display.
    /// </summary>
    display_header_footer : Integer;
    /// <summary>
    /// HTML template for the print header. Only displayed if
    /// |display_header_footer| is true (1). Should be valid HTML markup with
    /// the following classes used to inject printing values into them:
    ///
    /// - date: formatted print date
    /// - title: document title
    /// - url: document location
    /// - pageNumber: current page number
    /// - totalPages: total pages in the document
    ///
    /// For example, "<span class=title></span>" would generate a span containing
    /// the title.
    /// </summary>
    header_template       : TCefString;
    /// <summary>
    /// HTML template for the print footer. Only displayed if
    /// |display_header_footer| is true (1). Uses the same format as
    /// |header_template|.
    /// </summary>
    footer_template       : TCefString;
    /// <summary>
    /// Set to true (1) to generate tagged (accessible) PDF.
    /// </summary>
    generate_tagged_pdf   : integer;
    /// <summary>
    /// Set to true (1) to generate a document outline.
    /// </summary>
    generate_document_outline : integer;
  end;

  /// <summary>
  /// Structure representing mouse event information.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_mouse_event_t)</see></para>
  /// </remarks>
  TCefMouseEvent = record
    /// <summary>
    /// X coordinate relative to the left side of the view.
    /// </summary>
    x         : Integer;
    /// <summary>
    /// Y coordinate relative to the top side of the view.
    /// </summary>
    y         : Integer;
    /// <summary>
    /// Bit flags describing any pressed modifier keys. See
    /// TCefEventFlags for values.
    /// </summary>
    modifiers : TCefEventFlags;
  end;

  /// <summary>
  /// Structure representing touch event information.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_touch_event_t)</see></para>
  /// </remarks>
  TCefTouchEvent = record
    /// <summary>
    /// Id of a touch point. Must be unique per touch, can be any number except
    /// -1. Note that a maximum of 16 concurrent touches will be tracked; touches
    /// beyond that will be ignored.
    /// </summary>
    id             : integer;
    /// <summary>
    /// X coordinate relative to the left side of the view.
    /// </summary>
    x              : single;
    /// <summary>
    /// Y coordinate relative to the top side of the view.
    /// </summary>
    y              : single;
    /// <summary>
    /// X radius in pixels. Set to 0 if not applicable.
    /// </summary>
    radius_x       : single;
    /// <summary>
    /// Y radius in pixels. Set to 0 if not applicable.
    /// </summary>
    radius_y       : single;
    /// <summary>
    /// Rotation angle in radians. Set to 0 if not applicable.
    /// </summary>
    rotation_angle : single;
    /// <summary>
    /// The normalized pressure of the pointer input in the range of [0,1].
    /// Set to 0 if not applicable.
    /// </summary>
    pressure       : single;
    /// <summary>
    /// The state of the touch point. Touches begin with one CEF_TET_PRESSED event
    /// followed by zero or more CEF_TET_MOVED events and finally one
    /// CEF_TET_RELEASED or CEF_TET_CANCELLED event. Events not respecting this
    /// order will be ignored.
    /// </summary>
    type_          : TCefTouchEeventType;
    /// <summary>
    /// Bit flags describing any pressed modifier keys. See
    /// TCefEventFlags for values.
    /// </summary>
    modifiers      : TCefEventFlags;
    /// <summary>
    /// The device type that caused the event.
    /// </summary>
    pointer_type   : TCefPointerType;
  end;


  /// <summary>
  /// Structure representing a simulated touch point.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://chromedevtools.github.io/devtools-protocol/tot/Input/#type-TouchPoint">See the Input.TouchPoint type in the DevTools docs.</see></para>
  /// </remarks>
  TCefSimulatedTouchPoint = record
    /// <summary>
    /// Identifier used to track touch sources between events, must be unique within an event. This is an optional value.
    /// </summary>
    id                 : integer;
    /// <summary>
    /// X coordinate of the event relative to the main frame's viewport in CSS pixels.
    /// </summary>
    x                  : integer;
    /// <summary>
    /// Y coordinate of the event relative to the main frame's viewport in CSS pixels. 0 refers to the top of the viewport and Y increases as it proceeds towards the bottom of the viewport.
    /// </summary>
    y                  : integer;
    /// <summary>
    /// X radius of the touch area (default: 1.0). This is an optional value.
    /// </summary>
    radiusX            : single;
    /// <summary>
    /// Y radius of the touch area (default: 1.0). This is an optional value.
    /// </summary>
    radiusY            : single;
    /// <summary>
    /// Rotation angle (default: 0.0). This is an optional value.
    /// </summary>
    rotationAngle      : single;
    /// <summary>
    /// Force (default: 1.0). This is an optional value.
    /// </summary>
    force              : single;
    /// <summary>
    /// The normalized tangential pressure, which has a range of [-1,1] (default: 0). This is an optional value.
    /// </summary>
    tangentialPressure : single;
    /// <summary>
    /// The plane angle between the Y-Z plane and the plane containing both the stylus axis and the Y axis, in degrees of the range [-90,90], a positive tiltX is to the right (default: 0) This is an optional value.
    /// </summary>
    tiltX              : integer;
    /// <summary>
    /// The plane angle between the X-Z plane and the plane containing both the stylus axis and the X axis, in degrees of the range [-90,90], a positive tiltY is towards the user (default: 0). This is an optional value.
    /// </summary>
    tiltY              : integer;
    /// <summary>
    /// The clockwise rotation of a pen stylus around its own major axis, in degrees in the range [0,359] (default: 0).  This is an optional value.
    /// </summary>
    twist              : integer;
  end;
  TCefSimulatedTouchPointArray = array of TCefSimulatedTouchPoint;


  /// <summary>
  /// Structure representing the audio parameters for setting up the audio
  /// handler.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_audio_parameters_t)</see></para>
  /// </remarks>
  TCefAudioParameters = record
    /// <summary>
    /// Layout of the audio channels
    /// </summary>
    channel_layout    : TCefChannelLayout;
    /// <summary>
    /// Sample rate
    /// </summary>
    sample_rate       : integer;
    /// <summary>
    /// Number of frames per buffer
    /// </summary>
    frames_per_buffer : integer;
  end;

  /// <summary>
  /// Device information for a MediaSink object.
  /// handler.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types.h">CEF source file: /include/internal/cef_types.h (cef_media_sink_device_info_t)</see></para>
  /// </remarks>
  TCefMediaSinkDeviceInfo = record
    ip_address : TCefString;
    port       : integer;
    model_name : TCefString;
  end;

  /// <summary>
  /// Supported content setting types. Some types are platform-specific or only
  /// supported with Chrome style. Should be kept in sync with Chromium's
  /// ContentSettingsType type.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types_content_settings.h">CEF source file: /include/internal/cef_types_content_settings.h (cef_content_setting_types_t)</see></para>
  /// </remarks>
  TCefContentSettingTypes = (
    /// <summary>
    /// This setting governs whether cookies are enabled by the user in the
    /// provided context. However, it may be overridden by other settings. This
    /// enum should NOT be read directly to determine whether cookies are enabled;
    /// the client should instead rely on the CookieSettings API.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_COOKIES = 0,
    CEF_CONTENT_SETTING_TYPE_IMAGES,
    CEF_CONTENT_SETTING_TYPE_JAVASCRIPT,

    /// <summary>
    /// This setting governs both popups and unwanted redirects like tab-unders and framebusting.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_POPUPS,

    CEF_CONTENT_SETTING_TYPE_GEOLOCATION,
    CEF_CONTENT_SETTING_TYPE_NOTIFICATIONS,
    CEF_CONTENT_SETTING_TYPE_AUTO_SELECT_CERTIFICATE,
    CEF_CONTENT_SETTING_TYPE_MIXEDSCRIPT,
    CEF_CONTENT_SETTING_TYPE_MEDIASTREAM_MIC,
    CEF_CONTENT_SETTING_TYPE_MEDIASTREAM_CAMERA,
    CEF_CONTENT_SETTING_TYPE_PROTOCOL_HANDLERS,
    CEF_CONTENT_SETTING_TYPE_DEPRECATED_PPAPI_BROKER,
    CEF_CONTENT_SETTING_TYPE_AUTOMATIC_DOWNLOADS,

    /// <summary>
    /// Advanced device-specific functions on MIDI devices. MIDI-SysEx
    /// communications can be used for changing the MIDI device's persistent state
    /// such as firmware.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_MIDI_SYSEX,
    CEF_CONTENT_SETTING_TYPE_SSL_CERT_DECISIONS,
    CEF_CONTENT_SETTING_TYPE_PROTECTED_MEDIA_IDENTIFIER,
    CEF_CONTENT_SETTING_TYPE_APP_BANNER,
    CEF_CONTENT_SETTING_TYPE_SITE_ENGAGEMENT,
    CEF_CONTENT_SETTING_TYPE_DURABLE_STORAGE,
    CEF_CONTENT_SETTING_TYPE_USB_CHOOSER_DATA,
    CEF_CONTENT_SETTING_TYPE_BLUETOOTH_GUARD,
    CEF_CONTENT_SETTING_TYPE_BACKGROUND_SYNC,
    CEF_CONTENT_SETTING_TYPE_AUTOPLAY,
    CEF_CONTENT_SETTING_TYPE_IMPORTANT_SITE_INFO,
    CEF_CONTENT_SETTING_TYPE_PERMISSION_AUTOBLOCKER_DATA,
    CEF_CONTENT_SETTING_TYPE_ADS,
    /// <summary>
    /// Website setting which stores metadata for the subresource filter to aid in
    /// decisions for whether or not to show the UI.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_ADS_DATA,
    /// <summary>
    /// MIDI stands for Musical Instrument Digital Interface. It is a standard
    /// that allows electronic musical instruments, computers, and other devices
    /// to communicate with each other.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_MIDI,
    /// <summary>
    /// This content setting type is for caching password protection service's
    /// verdicts of each origin.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_PASSWORD_PROTECTION,
    /// <summary>
    /// Website setting which stores engagement data for media related to a
    /// specific origin.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_MEDIA_ENGAGEMENT,
    /// <summary>
    /// Content setting which stores whether or not the site can play audible
    /// sound. This will not block playback but instead the user will not hear it.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_SOUND,
    /// <summary>
    /// Website setting which stores the list of client hints that the origin
    /// requested the browser to remember. The browser is expected to send all
    /// client hints in the HTTP request headers for every resource requested
    /// from that origin.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_CLIENT_HINTS,
    /// <summary>
    /// Generic Sensor API covering ambient-light-sensor, accelerometer, gyroscope
    /// and magnetometer are all mapped to a single content_settings_type.
    /// Setting for the Generic Sensor API covering ambient-light-sensor,
    /// accelerometer, gyroscope and magnetometer. These are all mapped to a
    /// single ContentSettingsType.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_SENSORS,
    /// <summary>
    /// Content setting which stores whether or not the user has granted the site
    /// permission to respond to accessibility events, which can be used to
    /// provide a custom accessibility experience. Requires explicit user consent
    /// because some users may not want sites to know they're using assistive
    /// technology.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_ACCESSIBILITY_EVENTS,
    /// <summary>
    /// Used to store whether to allow a website to install a payment handler.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_PAYMENT_HANDLER,
    /// <summary>
    /// Content setting which stores whether to allow sites to ask for permission
    /// to access USB devices. If this is allowed specific device permissions are
    /// stored under USB_CHOOSER_DATA.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_USB_GUARD,
    /// <summary>
    /// Nothing is stored in this setting at present. Please refer to
    /// BackgroundFetchPermissionContext for details on how this permission
    /// is ascertained.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_BACKGROUND_FETCH,
    /// <summary>
    /// Website setting which stores the amount of times the user has dismissed
    /// intent picker UI without explicitly choosing an option.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_INTENT_PICKER_DISPLAY,
    /// <summary>
    /// Used to store whether to allow a website to detect user active/idle state.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_IDLE_DETECTION,
    /// <summary>
    /// Content settings for access to serial ports. The "guard" content setting
    /// stores whether to allow sites to ask for permission to access a port. The
    /// permissions granted to access particular ports are stored in the "chooser
    /// data" website setting.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_SERIAL_GUARD,
    CEF_CONTENT_SETTING_TYPE_SERIAL_CHOOSER_DATA,
    /// <summary>
    /// Nothing is stored in this setting at present. Please refer to
    /// PeriodicBackgroundSyncPermissionContext for details on how this permission
    /// is ascertained.
    /// This content setting is not registered because it does not require access
    /// to any existing providers.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_PERIODIC_BACKGROUND_SYNC,
    /// <summary>
    /// Content setting which stores whether to allow sites to ask for permission
    /// to do Bluetooth scanning.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_BLUETOOTH_SCANNING,
    /// <summary>
    /// Content settings for access to HID devices. The "guard" content setting
    /// stores whether to allow sites to ask for permission to access a device.
    /// The permissions granted to access particular devices are stored in the
    /// "chooser data" website setting.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_HID_GUARD,
    CEF_CONTENT_SETTING_TYPE_HID_CHOOSER_DATA,
    /// <summary>
    /// Wake Lock API, which has two lock types: screen and system locks.
    /// Currently, screen locks do not need any additional permission, and system
    /// locks are always denied while the right UI is worked out.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_WAKE_LOCK_SCREEN,
    CEF_CONTENT_SETTING_TYPE_WAKE_LOCK_SYSTEM,
    /// <summary>
    /// <para>Legacy SameSite cookie behavior. This disables SameSite=Lax-by-default,
    /// SameSite=None requires Secure, and Schemeful Same-Site, forcing the
    /// legacy behavior wherein 1) cookies that don't specify SameSite are treated
    /// as SameSite=None, 2) SameSite=None cookies are not required to be Secure,
    /// and 3) schemeful same-site is not active.</para>
    /// <para>This will also be used to revert to legacy behavior when future changes
    /// in cookie handling are introduced.</para>
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_LEGACY_COOKIE_ACCESS,
    /// <summary>
    /// Content settings which stores whether to allow sites to ask for permission
    /// to save changes to an original file selected by the user through the
    /// File System Access API.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_FILE_SYSTEM_WRITE_GUARD,
    /// <summary>
    /// Used to store whether to allow a website to exchange data with NFC
    /// devices.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_NFC,
    /// <summary>
    /// Website setting to store permissions granted to access particular
    /// Bluetooth devices.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_BLUETOOTH_CHOOSER_DATA,
    /// <summary>
    /// Full access to the system clipboard (sanitized read without user gesture,
    /// and unsanitized read and write with user gesture).
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_CLIPBOARD_READ_WRITE,
    /// <summary>
    /// This is special-cased in the permissions layer to always allow, and as
    /// such doesn't have associated prefs data.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_CLIPBOARD_SANITIZED_WRITE,
    /// <summary>
    /// This content setting type is for caching safe browsing real time url
    /// check's verdicts of each origin.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_SAFE_BROWSING_URL_CHECK_DATA,
    /// <summary>
    /// Used to store whether a site is allowed to request AR or VR sessions with
    /// the WebXr Device API.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_VR,
    CEF_CONTENT_SETTING_TYPE_AR,
    /// <summary>
    /// Content setting which stores whether to allow site to open and read files
    /// and directories selected through the File System Access API.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_FILE_SYSTEM_READ_GUARD,
    /// <summary>
    /// Access to first party storage in a third-party context. Exceptions are
    /// scoped to the combination of requesting/top-level origin, and are managed
    /// through the Storage Access API. For the time being, this content setting
    /// exists in parallel to third-party cookie rules stored in COOKIES.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_STORAGE_ACCESS,
    /// <summary>
    /// Content setting which stores whether to allow a site to control camera
    /// movements. It does not give access to camera.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_CAMERA_PAN_TILT_ZOOM,
    /// <summary>
    /// Content setting for Screen Enumeration and Screen Detail functionality.
    /// Permits access to detailed multi-screen information, like size and
    /// position. Permits placing fullscreen and windowed content on specific
    /// screens. See also: https://w3c.github.io/window-placement
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_WINDOW_MANAGEMENT,
    /// <summary>
    /// Stores whether to allow insecure websites to make private network
    /// requests.
    /// See also: https://wicg.github.io/cors-rfc1918
    /// Set through enterprise policies only.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_INSECURE_PRIVATE_NETWORK,
    /// <summary>
    /// Content setting which stores whether or not a site can access low-level
    /// locally installed font data using the Local Fonts Access API.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_LOCAL_FONTS,
    /// <summary>
    /// Stores per-origin state for permission auto-revocation (for all permission
    /// types).
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_PERMISSION_AUTOREVOCATION_DATA,
    /// <summary>
    /// Stores per-origin state of the most recently selected directory for the
    /// use by the File System Access API.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_FILE_SYSTEM_LAST_PICKED_DIRECTORY,
    /// <summary>
    /// Controls access to the getDisplayMedia API.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_DISPLAY_CAPTURE,
    /// <summary>
    /// Website setting to store permissions metadata granted to paths on the
    /// local file system via the File System Access API.
    /// |FILE_SYSTEM_WRITE_GUARD| is the corresponding "guard" setting. The stored
    /// data represents valid permission only if
    /// |FILE_SYSTEM_ACCESS_EXTENDED_PERMISSION| is enabled via user opt-in.
    /// Otherwise, they represent "recently granted but revoked permission", which
    /// are used to restore the permission.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_FILE_SYSTEM_ACCESS_CHOOSER_DATA,
    /// <summary>
    /// Stores a grant that allows a relying party to send a request for identity
    /// information to specified identity providers, potentially through any
    /// anti-tracking measures that would otherwise prevent it. This setting is
    /// associated with the relying party's origin.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_FEDERATED_IDENTITY_SHARING,
    /// <summary>
    /// Whether to use the v8 optimized JIT for running JavaScript on the page.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_JAVASCRIPT_JIT,
    /// <summary>
    /// Content setting which stores user decisions to allow loading a site over
    /// HTTP. Entries are added by hostname when a user bypasses the HTTPS-First
    /// Mode interstitial warning when a site does not support HTTPS. Allowed
    /// hosts are exact hostname matches -- subdomains of a host on the allowlist
    /// must be separately allowlisted.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_HTTP_ALLOWED,
    /// <summary>
    /// Stores metadata related to form fill, such as e.g. whether user data was
    /// autofilled on a specific website.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_FORMFILL_METADATA,
    /// <summary>
    /// Setting to indicate that there is an active federated sign-in session
    /// between a specified relying party and a specified identity provider for
    /// a specified account. When this is present it allows access to session
    /// management capabilities between the sites. This setting is associated
    /// with the relying party's origin. Obsolete on Nov 2023.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_DEPRECATED_FEDERATED_IDENTITY_ACTIVE_SESSION,
    /// <summary>
    /// Setting to indicate whether Chrome should automatically apply darkening to
    /// web content.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_AUTO_DARK_WEB_CONTENT,
    /// <summary>
    /// Setting to indicate whether Chrome should request the desktop view of a
    /// site instead of the mobile one.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_REQUEST_DESKTOP_SITE,
    /// <summary>
    /// Setting to indicate whether browser should allow signing into a website
    /// via the browser FedCM API.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_FEDERATED_IDENTITY_API,
    /// <summary>
    /// Stores notification interactions per origin for the past 90 days.
    /// Interactions per origin are pre-aggregated over seven-day windows: A
    /// notification interaction or display is assigned to the last Monday
    /// midnight in local time.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_NOTIFICATION_INTERACTIONS,
    /// <summary>
    /// Website setting which stores the last reduced accept language negotiated
    /// for a given origin, to be used on future visits to the origin.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_REDUCED_ACCEPT_LANGUAGE,
    /// <summary>
    /// Website setting which is used for NotificationPermissionReviewService to
    /// store origin blocklist from review notification permissions feature.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_NOTIFICATION_PERMISSION_REVIEW,
    /// <summary>
    /// Website setting to store permissions granted to access particular devices
    /// in private network.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_PRIVATE_NETWORK_GUARD,
    CEF_CONTENT_SETTING_TYPE_PRIVATE_NETWORK_CHOOSER_DATA,
    /// <summary>
    /// Website setting which stores whether the browser has observed the user
    /// signing into an identity-provider based on observing the IdP-SignIn-Status
    /// HTTP header.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_FEDERATED_IDENTITY_IDENTITY_PROVIDER_SIGNIN_STATUS,
    /// <summary>
    /// Website setting which is used for UnusedSitePermissionsService to
    /// store revoked permissions of unused sites from unused site permissions
    /// feature.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_REVOKED_UNUSED_SITE_PERMISSIONS,
    /// <summary>
    /// Similar to STORAGE_ACCESS, but applicable at the page-level rather than
    /// being specific to a frame.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_TOP_LEVEL_STORAGE_ACCESS,
    /// <summary>
    /// Setting to indicate whether user has opted in to allowing auto re-authn
    /// via the FedCM API.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_FEDERATED_IDENTITY_AUTO_REAUTHN_PERMISSION,
    /// <summary>
    /// Website setting which stores whether the user has explicitly registered
    /// a website as an identity-provider.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_FEDERATED_IDENTITY_IDENTITY_PROVIDER_REGISTRATION,
    /// <summary>
    /// Content setting which is used to indicate whether anti-abuse functionality
    /// should be enabled.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_ANTI_ABUSE,
    /// <summary>
    /// Content setting used to indicate whether third-party storage partitioning
    /// should be enabled.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_THIRD_PARTY_STORAGE_PARTITIONING,
    /// <summary>
    /// Used to indicate whether HTTPS-First Mode is enabled on the hostname.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_HTTPS_ENFORCED,
    /// <summary>
    /// Setting for enabling the `getAllScreensMedia` API. Spec link:
    /// https://github.com/screen-share/capture-all-screens
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_ALL_SCREEN_CAPTURE,
    /// <summary>
    /// Stores per origin metadata for cookie controls.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_COOKIE_CONTROLS_METADATA,
    /// <summary>
    /// Content Setting for temporary 3PC accesses granted by user behavior
    /// heuristics.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_TPCD_HEURISTICS_GRANTS,
    /// <summary>
    /// Content Setting for 3PC accesses granted by metadata delivered via the
    /// component updater service. This type will only be used when
    /// `net::features::kTpcdMetadataGrants` is enabled.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_TPCD_METADATA_GRANTS,
    /// <summary>
    /// Content Setting for 3PC accesses granted via 3PC deprecation trial.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_TPCD_TRIAL,
    /// <summary>
    /// Content Setting for 3PC accesses granted via top-level 3PC deprecation
    /// trial. Similar to TPCD_TRIAL, but applicable at the page-level for the
    /// lifetime of the page that served the token, rather than being specific to
    /// a requesting-origin/top-level-site combination and persistent.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_TOP_LEVEL_TPCD_TRIAL,
    /// <summary>
    /// <para>Content Setting for a first-party origin trial that allows websites to
    /// enable third-party cookie deprecation.</para>
    /// <para>ALLOW (default): no effect (e.g. third-party cookies allowed, if
    /// not blocked otherwise).</para>
    /// <para>BLOCK: third-party cookies blocked, but 3PCD mitigations enabled.</para>
    /// </summary>
    CEF_CONTENT_SETTING_TOP_LEVEL_TPCD_ORIGIN_TRIAL,
    /// <summary>
    /// Content setting used to indicate whether entering picture-in-picture
    /// automatically should be enabled.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_AUTO_PICTURE_IN_PICTURE,
    /// <summary>
    /// Whether user has opted into keeping file/directory permissions persistent
    /// between visits for a given origin. When enabled, permission metadata
    /// stored under |FILE_SYSTEM_ACCESS_CHOOSER_DATA| can auto-grant incoming
    /// permission request.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_FILE_SYSTEM_ACCESS_EXTENDED_PERMISSION,
    /// <summary>
    /// Whether the FSA Persistent Permissions restore prompt is eligible to be
    /// shown to the user, for a given origin.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_FILE_SYSTEM_ACCESS_RESTORE_PERMISSION,
    /// <summary>
    /// Whether an application capturing another tab, may scroll and zoom
    /// the captured tab.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_CAPTURED_SURFACE_CONTROL,
    /// <summary>
    /// Content setting for access to smart card readers.
    /// The "guard" content setting stores whether to allow sites to access the
    /// Smart Card API.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_SMART_CARD_GUARD,
    CEF_CONTENT_SETTING_TYPE_SMART_CARD_DATA,
    /// <summary>
    /// Content settings for access to printers for the Web Printing API.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_WEB_PRINTING,
    /// <summary>
    /// Content setting used to indicate whether entering HTML Fullscreen
    /// automatically (i.e. without transient activation) should be enabled.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_AUTOMATIC_FULLSCREEN,
    /// <summary>
    /// Content settings used to indicate that a web app is allowed to prompt the
    /// user for the installation of sub apps.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_SUB_APP_INSTALLATION_PROMPTS,
    /// <summary>
    /// Whether an application can enumerate audio output device.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_SPEAKER_SELECTION,
    /// <summary>
    /// Content settings for access to the Direct Sockets API.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_DIRECT_SOCKETS,
    /// <summary>
    /// Keyboard Lock API allows a site to capture keyboard inputs that would
    /// otherwise be handled by the OS or the browser.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_KEYBOARD_LOCK,
    /// <summary>
    /// Pointer Lock API allows a site to hide the cursor and have exclusive
    /// access to mouse inputs.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_POINTER_LOCK,
    /// <summary>
    /// Website setting which is used for UnusedSitePermissionsService to store
    /// auto-revoked notification permissions from abusive sites.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_REVOKED_ABUSIVE_NOTIFICATION_PERMISSIONS,
    /// <summary>
    /// <para>Content setting that controls tracking protection status per site.</para>
    /// <para>BLOCK: Protections enabled. This is the default state.</para>
    /// <para>ALLOW: Protections disabled.</para>
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_TRACKING_PROTECTION,
    /// <summary>
    /// With this permission, when the application calls `getDisplayMedia()`, a
    /// system audio track can be returned without showing the display media
    /// selection picker. The application can explicitly specify
    /// `systemAudio: 'exclude'` or `video: true` to still show the display media
    /// selection picker if needed. Please note that the setting only works for
    /// WebUI.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_DISPLAY_MEDIA_SYSTEM_AUDIO,
    /// <summary>
    /// Whether to use the higher-tier v8 optimizers for running JavaScript on the
    /// page.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_JAVASCRIPT_OPTIMIZER,
    /// <summary>
    /// <para>Content Setting for the Storage Access Headers persistent origin trial
    /// that allows origins to opt into the storage access header behavior. Should
    /// be scoped to `REQUESTING_ORIGIN_AND_TOP_SCHEMEFUL_SITE_SCOPE` in order to
    /// correspond to the design of persistent origin trials.</para>
    /// <para>ALLOW: storage access request headers will be attached to cross-site
    ///        requests, and url requests will look for response headers from
    ///        origins to retry a request or load with storage access.</para>
    /// <para>BLOCK (default): no effect.</para>
    /// </summary>
    /// <remarks>
    /// <para><see href="https://github.com/cfredric/storage-access-headers">See also: https://github.com/cfredric/storage-access-headers.</see></para>
    /// </remarks>
    CEF_CONTENT_SETTING_TYPE_STORAGE_ACCESS_HEADER_ORIGIN_TRIAL,
    /// <summary>
    /// Whether or not sites can request Hand Tracking data within WebXR Sessions.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_HAND_TRACKING,
    /// <summary>
    /// Website setting to indicate whether user has opted in to allow web apps to
    /// install other web apps.
    /// </summary>
    CEF_CONTENT_SETTING_TYPE_WEB_APP_INSTALLATION
  );

  /// <summary>
  /// Supported content setting values. Should be kept in sync with Chromium's
  /// ContentSetting type.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_types_content_settings.h">CEF source file: /include/internal/cef_types_content_settings.h (cef_content_setting_values_t)</see></para>
  /// </remarks>
  TCefContentSettingValues = (
    CEF_CONTENT_SETTING_VALUE_DEFAULT = 0,
    CEF_CONTENT_SETTING_VALUE_ALLOW,
    CEF_CONTENT_SETTING_VALUE_BLOCK,
    CEF_CONTENT_SETTING_VALUE_ASK,
    CEF_CONTENT_SETTING_VALUE_SESSION_ONLY,
    CEF_CONTENT_SETTING_VALUE_DETECT_IMPORTANT_CONTENT,
    CEF_CONTENT_SETTING_VALUE_NUM_VALUES
  );

  /// <summary>
  /// All ref-counted framework structures must include this structure first.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefBaseRefCounted.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_base_capi.h">CEF source file: /include/capi/cef_base_capi.h (cef_base_ref_counted_t)</see></para>
  /// </remarks>
  TCefBaseRefCounted = record
    size                 : NativeUInt;
    add_ref              : procedure(self: PCefBaseRefCounted); stdcall;
    release              : function(self: PCefBaseRefCounted): Integer; stdcall;
    has_one_ref          : function(self: PCefBaseRefCounted): Integer; stdcall;
    has_at_least_one_ref : function(self: PCefBaseRefCounted): Integer; stdcall;
  end;

  /// <summary>
  /// All scoped framework structures must include this structure first.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefBaseScoped.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_base_capi.h">CEF source file: /include/capi/cef_base_capi.h (cef_base_scoped_t)</see></para>
  /// </remarks>
  TCefBaseScoped = record
    size  : NativeUInt;
    del   : procedure(self: PCefBaseScoped); stdcall;
  end;

  /// <summary>
  /// Structure used to write data to a stream. The functions of this structure
  /// may be called on any thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefStreamWriter.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_stream_capi.h">CEF source file: /include/capi/cef_stream_capi.h (cef_stream_writer_t)</see></para>
  /// </remarks>
  TCefStreamWriter = record
    base      : TCefBaseRefCounted;
    write     : function(self: PCefStreamWriter; const ptr: Pointer; size, n: NativeUInt): NativeUInt; stdcall;
    seek      : function(self: PCefStreamWriter; offset: Int64; whence: Integer): Integer; stdcall;
    tell      : function(self: PCefStreamWriter): Int64; stdcall;
    flush     : function(self: PCefStreamWriter): Integer; stdcall;
    may_block : function(self: PCefStreamWriter): Integer; stdcall;
  end;

  /// <summary>
  /// Structure representing the issuer or subject field of an X.509 certificate.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefX509CertPrincipal.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_x509_certificate_capi.h">CEF source file: /include/capi/cef_x509_certificate_capi.h (cef_x509cert_principal_t)</see></para>
  /// </remarks>
  TCefX509CertPrincipal = record
    base                        : TCefBaseRefCounted;
    get_display_name            : function(self: PCefX509CertPrincipal): PCefStringUserFree; stdcall;
    get_common_name             : function(self: PCefX509CertPrincipal): PCefStringUserFree; stdcall;
    get_locality_name           : function(self: PCefX509CertPrincipal): PCefStringUserFree; stdcall;
    get_state_or_province_name  : function(self: PCefX509CertPrincipal): PCefStringUserFree; stdcall;
    get_country_name            : function(self: PCefX509CertPrincipal): PCefStringUserFree; stdcall;
    get_organization_names      : procedure(self: PCefX509CertPrincipal; names: TCefStringList); stdcall;
    get_organization_unit_names : procedure(self: PCefX509CertPrincipal; names: TCefStringList); stdcall;
  end;

  /// <summary>
  /// Structure representing a X.509 certificate.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefX509Certificate.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_x509_certificate_capi.h">CEF source file: /include/capi/cef_x509_certificate_capi.h (cef_x509certificate_t)</see></para>
  /// </remarks>
  TCefX509Certificate = record
    base                        : TCefBaseRefCounted;
    get_subject                 : function(self: PCefX509Certificate): PCefX509CertPrincipal; stdcall;
    get_issuer                  : function(self: PCefX509Certificate): PCefX509CertPrincipal; stdcall;
    get_serial_number           : function(self: PCefX509Certificate): PCefBinaryValue; stdcall;
    get_valid_start             : function(self: PCefX509Certificate): TCefBaseTime; stdcall;
    get_valid_expiry            : function(self: PCefX509Certificate): TCefBaseTime; stdcall;
    get_derencoded              : function(self: PCefX509Certificate): PCefBinaryValue; stdcall;
    get_pemencoded              : function(self: PCefX509Certificate): PCefBinaryValue; stdcall;
    get_issuer_chain_size       : function(self: PCefX509Certificate): NativeUInt; stdcall;
    get_derencoded_issuer_chain : procedure(self: PCefX509Certificate; var chainCount: NativeUInt; var chain: PCefBinaryValue); stdcall;
    get_pemencoded_issuer_chain : procedure(self: PCefX509Certificate; var chainCount: NativeUInt; var chain: PCefBinaryValue); stdcall;
  end;

  /// <summary>
  /// Structure representing SSL information.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefSslInfo</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_ssl_info_capi.h">CEF source file: /include/capi/cef_ssl_info_capi.h (cef_sslinfo_t)</see></para>
  /// </remarks>
  TCefSslInfo = record
    base                : TCefBaseRefCounted;
    get_cert_status     : function(self: PCefSslInfo): TCefCertStatus; stdcall;
    get_x509certificate : function(self: PCefSslInfo): PCefX509Certificate; stdcall;
  end;

  /// <summary>
  /// Structure representing the SSL information for a navigation entry.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefSSLStatus.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_ssl_status_capi.h">CEF source file: /include/capi/cef_ssl_status_capi.h (cef_sslstatus_t)</see></para>
  /// </remarks>
  TCefSSLStatus = record
    base                 : TCefBaseRefCounted;
    is_secure_connection : function(self: PCefSSLStatus): integer; stdcall;
    get_cert_status      : function(self: PCefSSLStatus): TCefCertStatus; stdcall;
    get_sslversion       : function(self: PCefSSLStatus): TCefSSLVersion; stdcall;
    get_content_status   : function(self: PCefSSLStatus): TCefSSLContentStatus; stdcall;
    get_x509certificate  : function(self: PCefSSLStatus): PCefX509Certificate; stdcall;
  end;

  /// <summary>
  /// Callback structure used to select a client certificate for authentication.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefSelectClientCertificateCallback.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_request_handler_capi.h">CEF source file: /include/capi/cef_request_handler_capi.h (cef_select_client_certificate_callback_t)</see></para>
  /// </remarks>
  TCefSelectClientCertificateCallback = record
    base   : TCefBaseRefCounted;
    select : procedure(self: PCefSelectClientCertificateCallback; cert: PCefX509Certificate); stdcall;
  end;

  /// <summary>
  /// Callback structure used for continuation of custom context menu display.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefRunContextMenuCallback.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_context_menu_handler_capi.h">CEF source file: /include/capi/cef_context_menu_handler_capi.h (cef_run_context_menu_callback_t)</see></para>
  /// </remarks>
  TCefRunContextMenuCallback = record
    base   : TCefBaseRefCounted;
    cont   : procedure(self: PCefRunContextMenuCallback; command_id: Integer; event_flags: TCefEventFlags); stdcall;
    cancel : procedure(self: PCefRunContextMenuCallback); stdcall;
  end;

  /// <summary>
  /// Callback structure for asynchronous continuation of file dialog requests.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefFileDialogCallback.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_dialog_handler_capi.h">CEF source file: /include/capi/cef_dialog_handler_capi.h (cef_file_dialog_callback_t)</see></para>
  /// </remarks>
  TCefFileDialogCallback = record
    base   : TCefBaseRefCounted;
    cont   : procedure(self: PCefFileDialogCallback; file_paths: TCefStringList); stdcall;
    cancel : procedure(self: PCefFileDialogCallback); stdcall;
  end;

  /// <summary>
  /// Callback structure for asynchronous handling of an unresponsive process.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefUnresponsiveProcessCallback.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_unresponsive_process_callback_capi.h">CEF source file: /include/capi/cef_unresponsive_process_callback_capi.h (cef_unresponsive_process_callback_t)</see></para>
  /// </remarks>
  TCefUnresponsiveProcessCallback = record
    base      : TCefBaseRefCounted;
    wait      : procedure(self: PCefUnresponsiveProcessCallback); stdcall;
    terminate : procedure(self: PCefUnresponsiveProcessCallback); stdcall;
  end;

  /// <summary>
  /// Implement this structure to handle dialog events. The functions of this
  /// structure will be called on the browser process UI thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefDialogHandler.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_dialog_handler_capi.h">CEF source file: /include/capi/cef_dialog_handler_capi.h (cef_dialog_handler_t)</see></para>
  /// </remarks>
  TCefDialogHandler = record
    base           : TCefBaseRefCounted;
    on_file_dialog : function(self: PCefDialogHandler; browser: PCefBrowser; mode: TCefFileDialogMode; const title, default_file_path: PCefString; accept_filters, accept_extensions, accept_descriptions: TCefStringList; callback: PCefFileDialogCallback): Integer; stdcall;
  end;

  /// <summary>
  /// Implement this structure to handle events related to browser display state.
  /// The functions of this structure will be called on the UI thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefDisplayHandler.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_display_handler_capi.h">CEF source file: /include/capi/cef_display_handler_capi.h (cef_display_handler_t)</see></para>
  /// </remarks>
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
    on_media_access_change     : procedure(self: PCefDisplayHandler; browser: PCefBrowser; has_video_access, has_audio_access: integer); stdcall;
  end;

  /// <summary>
  /// Structure used to handle file downloads. The functions of this structure
  /// will called on the browser process UI thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefDownloadHandler.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_download_handler_capi.h">CEF source file: /include/capi/cef_download_handler_capi.h (cef_download_handler_t)</see></para>
  /// </remarks>
  TCefDownloadHandler = record
    base                : TCefBaseRefCounted;
    can_download        : function(self: PCefDownloadHandler; browser: PCefBrowser; const url, request_method: PCefString): integer; stdcall;
    on_before_download  : function(self: PCefDownloadHandler; browser: PCefBrowser; download_item: PCefDownloadItem; const suggested_name: PCefString; callback: PCefBeforeDownloadCallback): Integer; stdcall;
    on_download_updated : procedure(self: PCefDownloadHandler; browser: PCefBrowser; download_item: PCefDownloadItem; callback: PCefDownloadItemCallback); stdcall;
  end;

  /// <summary>
  /// Implement this structure to handle events related to dragging. The functions
  /// of this structure will be called on the UI thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefDragHandler.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_drag_handler_capi.h">CEF source file: /include/capi/cef_drag_handler_capi.h (cef_drag_handler_t)</see></para>
  /// </remarks>
  TCefDragHandler = record
    base                         : TCefBaseRefCounted;
    on_drag_enter                : function(self: PCefDragHandler; browser: PCefBrowser; dragData: PCefDragData; mask: TCefDragOperations): Integer; stdcall;
    on_draggable_regions_changed : procedure(self: PCefDragHandler; browser: PCefBrowser; frame: PCefFrame; regionsCount: NativeUInt; const regions: PCefDraggableRegionArray); stdcall;
  end;

  /// <summary>
  /// Implement this structure to handle events related to find results. The
  /// functions of this structure will be called on the UI thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefFindHandler.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_find_handler_capi.h">CEF source file: /include/capi/cef_find_handler_capi.h (cef_find_handler_t)</see></para>
  /// </remarks>
  TCefFindHandler = record
    base           : TCefBaseRefCounted;
    on_find_result : procedure(self: PCefFindHandler; browser: PCefBrowser; identifier, count: Integer; const selection_rect: PCefRect; active_match_ordinal, final_update: Integer); stdcall;
  end;

  /// <summary>
  /// Implement this structure to handle events related to focus. The functions of
  /// this structure will be called on the UI thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefFocusHandler.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_focus_handler_capi.h">CEF source file: /include/capi/cef_focus_handler_capi.h (cef_focus_handler_t)</see></para>
  /// </remarks>
  TCefFocusHandler = record
    base          : TCefBaseRefCounted;
    on_take_focus : procedure(self: PCefFocusHandler; browser: PCefBrowser; next: Integer); stdcall;
    on_set_focus  : function(self: PCefFocusHandler; browser: PCefBrowser; source: TCefFocusSource): Integer; stdcall;
    on_got_focus  : procedure(self: PCefFocusHandler; browser: PCefBrowser); stdcall;
  end;

  /// <summary>
  /// Implement this structure to handle events related to JavaScript dialogs. The
  /// functions of this structure will be called on the UI thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefJsDialogHandler.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_jsdialog_handler_capi.h">CEF source file: /include/capi/cef_jsdialog_handler_capi.h (cef_jsdialog_handler_t)</see></para>
  /// </remarks>
  TCefJsDialogHandler = record
    base                    : TCefBaseRefCounted;
    on_jsdialog             : function(self: PCefJsDialogHandler; browser: PCefBrowser; const origin_url: PCefString; dialog_type: TCefJsDialogType; const message_text, default_prompt_text: PCefString; callback: PCefJsDialogCallback; suppress_message: PInteger): Integer; stdcall;
    on_before_unload_dialog : function(self: PCefJsDialogHandler; browser: PCefBrowser; const message_text: PCefString; is_reload: Integer; callback: PCefJsDialogCallback): Integer; stdcall;
    on_reset_dialog_state   : procedure(self: PCefJsDialogHandler; browser: PCefBrowser); stdcall;
    on_dialog_closed        : procedure(self: PCefJsDialogHandler; browser: PCefBrowser); stdcall;
  end;

  /// <summary>
  /// Callback structure used for asynchronous continuation of JavaScript dialog
  /// requests.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefJsDialogCallback.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_jsdialog_handler_capi.h">CEF source file: /include/capi/cef_jsdialog_handler_capi.h (cef_jsdialog_callback_t)</see></para>
  /// </remarks>
  TCefJsDialogCallback = record
    base : TCefBaseRefCounted;
    cont : procedure(self: PCefJsDialogCallback; success: Integer; const user_input: PCefString); stdcall;
  end;

  /// <summary>
  /// Implement this structure to handle events related to keyboard input. The
  /// functions of this structure will be called on the UI thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefKeyboardHandler.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_keyboard_handler_capi.h">CEF source file: /include/capi/cef_keyboard_handler_capi.h (cef_keyboard_handler_t)</see></para>
  /// </remarks>
  TCefKeyboardHandler = record
    base             : TCefBaseRefCounted;
    on_pre_key_event : function(self: PCefKeyboardHandler; browser: PCefBrowser; const event: PCefKeyEvent; os_event: TCefEventHandle; is_keyboard_shortcut: PInteger): Integer; stdcall;
    on_key_event     : function(self: PCefKeyboardHandler; browser: PCefBrowser; const event: PCefKeyEvent; os_event: TCefEventHandle): Integer; stdcall;
  end;

  /// <summary>
  /// Implement this structure to handle events related to browser life span. The
  /// functions of this structure will be called on the UI thread unless otherwise
  /// indicated.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefLifeSpanHandler.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_life_span_handler_capi.h">CEF source file: /include/capi/cef_life_span_handler_capi.h (cef_life_span_handler_t)</see></para>
  /// </remarks>
  TCefLifeSpanHandler = record
    base                      : TCefBaseRefCounted;
    on_before_popup           : function(self: PCefLifeSpanHandler; browser: PCefBrowser; frame: PCefFrame; const target_url, target_frame_name: PCefString; target_disposition: TCefWindowOpenDisposition; user_gesture: Integer; const popupFeatures: PCefPopupFeatures; windowInfo: PCefWindowInfo; var client: PCefClient; settings: PCefBrowserSettings; var extra_info: PCefDictionaryValue; no_javascript_access: PInteger): Integer; stdcall;
    on_before_dev_tools_popup : procedure(self: PCefLifeSpanHandler; browser: PCefBrowser; windowInfo: PCefWindowInfo; var client: PCefClient; settings: PCefBrowserSettings; var extra_info: PCefDictionaryValue; use_default_window: PInteger); stdcall;
    on_after_created          : procedure(self: PCefLifeSpanHandler; browser: PCefBrowser); stdcall;
    do_close                  : function(self: PCefLifeSpanHandler; browser: PCefBrowser): Integer; stdcall;
    on_before_close           : procedure(self: PCefLifeSpanHandler; browser: PCefBrowser); stdcall;
  end;

  /// <summary>
  /// Generic callback structure used for managing the lifespan of a registration.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefRegistration.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_registration_capi.h">CEF source file: /include/capi/cef_registration_capi.h (cef_registration_t)</see></para>
  /// </remarks>
  TCefRegistration = record
    base  : TCefBaseRefCounted;
  end;

  /// <summary>
  /// Callback structure for ICefBrowserHost.AddDevToolsMessageObserver. The
  /// functions of this structure will be called on the browser process UI thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefDevToolsMessageObserver.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_devtools_message_observer_capi.h">CEF source file: /include/capi/cef_devtools_message_observer_capi.h (cef_dev_tools_message_observer_t)</see></para>
  /// </remarks>
  TCefDevToolsMessageObserver = record
    base                        : TCefBaseRefCounted;
    on_dev_tools_message        : function(self: PCefDevToolsMessageObserver; browser: PCefBrowser; const message_: Pointer; message_size: NativeUInt): Integer; stdcall;
    on_dev_tools_method_result  : procedure(self: PCefDevToolsMessageObserver; browser: PCefBrowser; message_id, success: Integer; const result: Pointer; result_size: NativeUInt); stdcall;
    on_dev_tools_event          : procedure(self: PCefDevToolsMessageObserver; browser: PCefBrowser; const method: PCefString; const params: Pointer; params_size: NativeUInt); stdcall;
    on_dev_tools_agent_attached : procedure(self: PCefDevToolsMessageObserver; browser: PCefBrowser); stdcall;
    on_dev_tools_agent_detached : procedure(self: PCefDevToolsMessageObserver; browser: PCefBrowser); stdcall;
  end;

  /// <summary>
  /// Supports discovery of and communication with media devices on the local
  /// network via the Cast and DIAL protocols. The functions of this structure may
  /// be called on any browser process thread unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefMediaRouter.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_media_router_capi.h">CEF source file: /include/capi/cef_media_router_capi.h (cef_media_router_t)</see></para>
  /// </remarks>
  TCefMediaRouter = record
    base                  : TCefBaseRefCounted;
    add_observer          : function(self: PCefMediaRouter; observer: PCefMediaObserver): PCefRegistration; stdcall;
    get_source            : function(self: PCefMediaRouter; const urn: PCefString): PCefMediaSource; stdcall;
    notify_current_sinks  : procedure(self: PCefMediaRouter); stdcall;
    create_route          : procedure(self: PCefMediaRouter; source: PCefMediaSource; sink: PCefMediaSink; callback: PCefMediaRouteCreateCallback); stdcall;
    notify_current_routes : procedure(self: PCefMediaRouter); stdcall;
  end;

  /// <summary>
  /// Implemented by the client to observe MediaRouter events and registered via
  /// ICefMediaRouter.AddObserver. The functions of this structure will be
  /// called on the browser process UI thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefMediaObserver.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_media_router_capi.h">CEF source file: /include/capi/cef_media_router_capi.h (cef_media_observer_t)</see></para>
  /// </remarks>
  TCefMediaObserver = record
    base                      : TCefBaseRefCounted;
    on_sinks                  : procedure(self: PCefMediaObserver; sinksCount: NativeUInt; const sinks: PPCefMediaSink); stdcall;
    on_routes                 : procedure(self: PCefMediaObserver; routesCount: NativeUInt; const routes: PPCefMediaRoute); stdcall;
    on_route_state_changed    : procedure(self: PCefMediaObserver; route: PCefMediaRoute; state: TCefMediaRouteConnectionState); stdcall;
    on_route_message_received : procedure(self: PCefMediaObserver; route: PCefMediaRoute; const message_: Pointer; message_size: NativeUInt); stdcall;
  end;

  /// <summary>
  /// Represents the route between a media source and sink. Instances of this
  /// object are created via ICefMediaRouter.CreateRoute and retrieved via
  /// ICefMediaObserver.OnRoutes. Contains the status and metadata of a
  /// routing operation. The functions of this structure may be called on any
  /// browser process thread unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefMediaRoute.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_media_router_capi.h">CEF source file: /include/capi/cef_media_router_capi.h (cef_media_route_t)</see></para>
  /// </remarks>
  TCefMediaRoute = record
    base                  : TCefBaseRefCounted;
    get_id                : function(self: PCefMediaRoute): PCefStringUserFree; stdcall;
    get_source            : function(self: PCefMediaRoute): PCefMediaSource; stdcall;
    get_sink              : function(self: PCefMediaRoute): PCefMediaSink; stdcall;
    send_route_message    : procedure(self: PCefMediaRoute; const message_: Pointer; message_size: NativeUInt); stdcall;
    terminate             : procedure(self: PCefMediaRoute); stdcall;
  end;

  /// <summary>
  /// Callback structure for ICefMediaRouter.CreateRoute. The functions of
  /// this structure will be called on the browser process UI thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefMediaRouteCreateCallback.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_media_router_capi.h">CEF source file: /include/capi/cef_media_router_capi.h (cef_media_route_create_callback_t)</see></para>
  /// </remarks>
  TCefMediaRouteCreateCallback = record
    base                           : TCefBaseRefCounted;
    on_media_route_create_finished : procedure(self: PCefMediaRouteCreateCallback; result: TCefMediaRouterCreateResult; const error: PCefString; route: PCefMediaRoute); stdcall;
  end;

  /// <summary>
  /// Represents a sink to which media can be routed. Instances of this object are
  /// retrieved via ICefMediaObserver.OnSinks. The functions of this structure
  /// may be called on any browser process thread unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefMediaSink.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_media_router_capi.h">CEF source file: /include/capi/cef_media_router_capi.h (cef_media_sink_t)</see></para>
  /// </remarks>
  TCefMediaSink = record
    base                  : TCefBaseRefCounted;
    get_id                : function(self: PCefMediaSink): PCefStringUserFree; stdcall;
    get_name              : function(self: PCefMediaSink): PCefStringUserFree; stdcall;
    get_icon_type         : function(self: PCefMediaSink): TCefMediaSinkIconType; stdcall;
    get_device_info       : procedure(self: PCefMediaSink; callback: PCefMediaSinkDeviceInfoCallback); stdcall;
    is_cast_sink          : function(self: PCefMediaSink): Integer; stdcall;
    is_dial_sink          : function(self: PCefMediaSink): Integer; stdcall;
    is_compatible_with    : function(self: PCefMediaSink; source: PCefMediaSource): Integer; stdcall;
  end;

  /// <summary>
  /// Callback structure for ICefMediaSink.GetDeviceInfo. The functions of
  /// this structure will be called on the browser process UI thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefMediaSinkDeviceInfoCallback.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_media_router_capi.h">CEF source file: /include/capi/cef_media_router_capi.h (cef_media_sink_device_info_callback_t)</see></para>
  /// </remarks>
  TCefMediaSinkDeviceInfoCallback = record
    base                      : TCefBaseRefCounted;
    on_media_sink_device_info : procedure(self: PCefMediaSinkDeviceInfoCallback; device_info: PCefMediaSinkDeviceInfo); stdcall;
  end;

  /// <summary>
  /// Represents a source from which media can be routed. Instances of this object
  /// are retrieved via ICefMediaRouter.GetSource. The functions of this
  /// structure may be called on any browser process thread unless otherwise
  /// indicated.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefMediaSource.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_media_router_capi.h">CEF source file: /include/capi/cef_media_router_capi.h (cef_media_source_t)</see></para>
  /// </remarks>
  TCefMediaSource = record
    base                  : TCefBaseRefCounted;
    get_id                : function(self: PCefMediaSource): PCefStringUserFree; stdcall;
    is_cast_source        : function(self: PCefMediaSource): Integer; stdcall;
    is_dial_source        : function(self: PCefMediaSource): Integer; stdcall;
  end;

  /// <summary>
  /// Implement this structure to handle audio events.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefAudioHandler.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_audio_handler_capi.h">CEF source file: /include/capi/cef_audio_handler_capi.h (cef_audio_handler_t)</see></para>
  /// </remarks>
  TCefAudioHandler = record
    base                          : TCefBaseRefCounted;
    get_audio_parameters          : function(self: PCefAudioHandler; browser: PCefBrowser; params: PCefAudioParameters): Integer; stdcall;
    on_audio_stream_started       : procedure(self: PCefAudioHandler; browser: PCefBrowser; const params: PCefAudioParameters; channels: integer); stdcall;
    on_audio_stream_packet        : procedure(self: PCefAudioHandler; browser: PCefBrowser; const data : PPSingle; frames: integer; pts: int64); stdcall;
    on_audio_stream_stopped       : procedure(self: PCefAudioHandler; browser: PCefBrowser); stdcall;
    on_audio_stream_error         : procedure(self: PCefAudioHandler; browser: PCefBrowser; const message_: PCefString); stdcall;
  end;

  /// <summary>
  /// Implement this structure to handle events related to browser load status.
  /// The functions of this structure will be called on the browser process UI
  /// thread or render process main thread (TID_RENDERER).
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefLoadHandler.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_load_handler_capi.h">CEF source file: /include/capi/cef_load_handler_capi.h (cef_load_handler_t)</see></para>
  /// </remarks>
  TCefLoadHandler = record
    base                    : TCefBaseRefCounted;
    on_loading_state_change : procedure(self: PCefLoadHandler; browser: PCefBrowser; isLoading, canGoBack, canGoForward: Integer); stdcall;
    on_load_start           : procedure(self: PCefLoadHandler; browser: PCefBrowser; frame: PCefFrame; transition_type: TCefTransitionType); stdcall;
    on_load_end             : procedure(self: PCefLoadHandler; browser: PCefBrowser; frame: PCefFrame; httpStatusCode: Integer); stdcall;
    on_load_error           : procedure(self: PCefLoadHandler; browser: PCefBrowser; frame: PCefFrame; errorCode: TCefErrorCode; const errorText, failedUrl: PCefString); stdcall;
  end;

  /// <summary>
  /// Implement this structure to handle events when window rendering is disabled.
  /// The functions of this structure will be called on the UI thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefRenderHandler.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_render_handler_capi.h">CEF source file: /include/capi/cef_render_handler_capi.h (cef_render_handler_t)</see></para>
  /// </remarks>
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
    on_accelerated_paint              : procedure(self: PCefRenderHandler; browser: PCefBrowser; type_: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; const info: PCefAcceleratedPaintInfo); stdcall;
    get_touch_handle_size             : procedure(self: PCefRenderHandler; browser: PCefBrowser; orientation: TCefHorizontalAlignment; size: PCefSize); stdcall;
    on_touch_handle_state_changed     : procedure(self: PCefRenderHandler; browser: PCefBrowser; const state: PCefTouchHandleState); stdcall;
    start_dragging                    : function(self: PCefRenderHandler; browser: PCefBrowser; drag_data: PCefDragData; allowed_ops: TCefDragOperations; x, y: Integer): Integer; stdcall;
    update_drag_cursor                : procedure(self: PCefRenderHandler; browser: PCefBrowser; operation: TCefDragOperation); stdcall;
    on_scroll_offset_changed          : procedure(self: PCefRenderHandler; browser: PCefBrowser; x, y: Double); stdcall;
    on_ime_composition_range_changed  : procedure(self: PCefRenderHandler; browser: PCefBrowser; const selected_range: PCefRange; character_boundsCount: NativeUInt; const character_bounds: PCefRect); stdcall;
    on_text_selection_changed         : procedure(self: PCefRenderHandler; browser: PCefBrowser; const selected_text: PCefString; const selected_range: PCefRange); stdcall;
    on_virtual_keyboard_requested     : procedure(self: PCefRenderHandler; browser: PCefBrowser; input_mode: TCefTextInpuMode); stdcall;
  end;

  /// <summary>
  /// Structure that manages custom preference registrations.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by TCefPreferenceRegistrarRef.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_preference_capi.h">CEF source file: /include/capi/cef_preference_capi.h (cef_preference_registrar_t)</see></para>
  /// </remarks>
  TCefPreferenceRegistrar = record
    base              : TCefBaseScoped;
    add_preference    : function(self: PCefPreferenceRegistrar; const name: PCefString; default_value: PCefValue): Integer; stdcall;
  end;

  /// <summary>
  /// Manage access to preferences. Many built-in preferences are registered by
  /// Chromium. Custom preferences can be registered in
  /// ICefBrowserProcessHandler.OnRegisterCustomPreferences.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefPreferenceManager.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_preference_capi.h">CEF source file: /include/capi/cef_preference_capi.h (cef_preference_manager_t)</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_preference_manager_capi.h">CEF source file: /include/capi/cef_preference_manager_capi.h (cef_preference_manager_t)</see></para>
  /// </remarks>
  TCefPreferenceManager = record
    base                            : TCefBaseRefCounted;
    has_preference                  : function(self: PCefPreferenceManager; const name: PCefString): Integer; stdcall;
    get_preference                  : function(self: PCefPreferenceManager; const name: PCefString): PCefValue; stdcall;
    get_all_preferences             : function(self: PCefPreferenceManager; include_defaults: Integer): PCefDictionaryValue; stdcall;
    can_set_preference              : function(self: PCefPreferenceManager; const name: PCefString): Integer; stdcall;
    set_preference                  : function(self: PCefPreferenceManager; const name: PCefString; value: PCefValue; error: PCefString): Integer; stdcall;
  end;

  /// <summary>
  /// Structure representing a V8 stack trace handle. V8 handles can only be
  /// accessed from the thread on which they are created. Valid threads for
  /// creating a V8 handle include the render process main thread (TID_RENDERER)
  /// and WebWorker threads. A task runner for posting tasks on the associated
  /// thread can be retrieved via the ICefv8context.GetTaskRunner() function.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefV8StackTrace.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_v8_capi.h">CEF source file: /include/capi/cef_v8_capi.h (cef_v8stack_trace_t)</see></para>
  /// </remarks>
  TCefV8StackTrace = record
    base            : TCefBaseRefCounted;
    is_valid        : function(self: PCefV8StackTrace): Integer; stdcall;
    get_frame_count : function(self: PCefV8StackTrace): Integer; stdcall;
    get_frame       : function(self: PCefV8StackTrace; index: Integer): PCefV8StackFrame; stdcall;
  end;

  /// <summary>
  /// Structure representing a V8 stack frame handle. V8 handles can only be
  /// accessed from the thread on which they are created. Valid threads for
  /// creating a V8 handle include the render process main thread (TID_RENDERER)
  /// and WebWorker threads. A task runner for posting tasks on the associated
  /// thread can be retrieved via the ICefv8context.GetTaskRunner() function.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefV8StackFrame.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_v8_capi.h">CEF source file: /include/capi/cef_v8_capi.h (cef_v8stack_frame_t)</see></para>
  /// </remarks>
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

  /// <summary>
  /// Structure used to read data from a stream. The functions of this structure
  /// may be called on any thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefStreamReader and ICefCustomStreamReader.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_stream_capi.h">CEF source file: /include/capi/cef_stream_capi.h (cef_stream_reader_t)</see></para>
  /// </remarks>
  TCefStreamReader = record
    base      : TCefBaseRefCounted;
    read      : function(self: PCefStreamReader; ptr: Pointer; size, n: NativeUInt): NativeUInt; stdcall;
    seek      : function(self: PCefStreamReader; offset: Int64; whence: Integer): Integer; stdcall;
    tell      : function(self: PCefStreamReader): Int64; stdcall;
    eof       : function(self: PCefStreamReader): Integer; stdcall;
    may_block : function(self: PCefStreamReader): Integer; stdcall;
  end;

  /// <summary>
  /// Structure the client can implement to provide a custom stream reader. The
  /// functions of this structure may be called on any thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefReadHandler.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_stream_capi.h">CEF source file: /include/capi/cef_stream_capi.h (cef_read_handler_t)</see></para>
  /// </remarks>
  TCefReadHandler = record
    base      : TCefBaseRefCounted;
    read      : function(self: PCefReadHandler; ptr: Pointer; size, n: NativeUInt): NativeUInt; stdcall;
    seek      : function(self: PCefReadHandler; offset: Int64; whence: Integer): Integer; stdcall;
    tell      : function(self: PCefReadHandler): Int64; stdcall;
    eof       : function(self: PCefReadHandler): Integer; stdcall;
    may_block : function(self: PCefReadHandler): Integer; stdcall;
  end;

  /// <summary>
  /// Structure the client can implement to provide a custom stream writer. The
  /// functions of this structure may be called on any thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefWriteHandler.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_stream_capi.h">CEF source file: /include/capi/cef_stream_capi.h (cef_write_handler_t)</see></para>
  /// </remarks>
  TCefWriteHandler = record
    base      : TCefBaseRefCounted;
    write     : function(self: PCefWriteHandler; const ptr: Pointer; size, n: NativeUInt): NativeUInt; stdcall;
    seek      : function(self: PCefWriteHandler; offset: Int64; whence: Integer): Integer; stdcall;
    tell      : function(self: PCefWriteHandler): Int64; stdcall;
    flush     : function(self: PCefWriteHandler): Integer; stdcall;
    may_block : function(self: PCefWriteHandler): Integer; stdcall;
  end;

  /// <summary>
  /// Structure that supports the reading of XML data via the libxml streaming
  /// API. The functions of this structure should only be called on the thread
  /// that creates the object.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefXmlReader.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_xml_reader_capi.h">CEF source file: /include/capi/cef_xml_reader_capi.h (cef_xml_reader_t)</see></para>
  /// </remarks>
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

  /// <summary>
  /// Structure that supports the reading of zip archives via the zlib unzip API.
  /// The functions of this structure should only be called on the thread that
  /// creates the object.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefZipReader.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_zip_reader_capi.h">CEF source file: /include/capi/cef_zip_reader_capi.h (cef_zip_reader_t)</see></para>
  /// </remarks>
  TCefZipReader = record
    base                    : TCefBaseRefCounted;
    move_to_first_file      : function(self: PCefZipReader): Integer; stdcall;
    move_to_next_file       : function(self: PCefZipReader): Integer; stdcall;
    move_to_file            : function(self: PCefZipReader; const fileName: PCefString; caseSensitive: Integer): Integer; stdcall;
    close                   : function(Self: PCefZipReader): Integer; stdcall;
    get_file_name           : function(Self: PCefZipReader): PCefStringUserFree; stdcall;
    get_file_size           : function(Self: PCefZipReader): Int64; stdcall;
    get_file_last_modified  : function(Self: PCefZipReader): TCefBaseTime; stdcall;
    open_file               : function(Self: PCefZipReader; const password: PCefString): Integer; stdcall;
    close_file              : function(Self: PCefZipReader): Integer; stdcall;
    read_file               : function(Self: PCefZipReader; buffer: Pointer; bufferSize: NativeUInt): Integer; stdcall;
    tell                    : function(Self: PCefZipReader): Int64; stdcall;
    eof                     : function(Self: PCefZipReader): Integer; stdcall;
  end;

  /// <summary>
  /// Structure that should be implemented by the ICefUrlRequest client. The
  /// functions of this structure will be called on the same thread that created
  /// the request unless otherwise documented.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefUrlrequestClient.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_urlrequest_capi.h">CEF source file: /include/capi/cef_urlrequest_capi.h (cef_urlrequest_client_t)</see></para>
  /// </remarks>
  TCefUrlrequestClient = record
    base                  : TCefBaseRefCounted;
    on_request_complete   : procedure(self: PCefUrlRequestClient; request: PCefUrlRequest); stdcall;
    on_upload_progress    : procedure(self: PCefUrlRequestClient; request: PCefUrlRequest; current, total: Int64); stdcall;
    on_download_progress  : procedure(self: PCefUrlRequestClient; request: PCefUrlRequest; current, total: Int64); stdcall;
    on_download_data      : procedure(self: PCefUrlRequestClient; request: PCefUrlRequest; const data: Pointer; data_length: NativeUInt); stdcall;
    get_auth_credentials  : function(self: PCefUrlRequestClient; isProxy: Integer; const host: PCefString; port: Integer; const realm, scheme: PCefString; callback: PCefAuthCallback): Integer; stdcall;
  end;

  /// <summary>
  /// Structure used to make a URL request. URL requests are not associated with a
  /// browser instance so no ICefClient callbacks will be executed. URL requests
  /// can be created on any valid CEF thread in either the browser or render
  /// process. Once created the functions of the URL request object must be
  /// accessed on the same thread that created it.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefUrlRequest.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_urlrequest_capi.h">CEF source file: /include/capi/cef_urlrequest_capi.h (cef_urlrequest_t)</see></para>
  /// </remarks>
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

  /// <summary>
  /// A simple thread abstraction that establishes a message loop on a new thread.
  /// The consumer uses ICefTaskRunner to execute code on the thread's message
  /// loop. The thread is terminated when the ICefThread object is destroyed or
  /// stop() is called. All pending tasks queued on the thread's message loop will
  /// run to completion before the thread is terminated. cef_thread_create() can
  /// be called on any valid CEF thread in either the browser or render process.
  /// This structure should only be used for tasks that require a dedicated
  /// thread. In most cases you can post tasks to an existing CEF thread instead
  /// of creating a new one; see cef_task.h for details.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefThread.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_thread_capi.h">CEF source file: /include/capi/cef_thread_capi.h (cef_thread_t)</see></para>
  /// </remarks>
  TCefThread = record
    base                    : TCefBaseRefCounted;
    get_task_runner         : function(self: PCefThread): PCefTaskRunner; stdcall;
    get_platform_thread_id  : function(self: PCefThread): TCefPlatformThreadId; stdcall;
    stop                    : procedure(self: PCefThread); stdcall;
    is_running              : function(self: PCefThread): integer; stdcall;
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
  /// <para>Implemented by ICefWaitableEvent.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_waitable_event_capi.h">CEF source file: /include/capi/cef_waitable_event_capi.h (cef_waitable_event_t)</see></para>
  /// </remarks>
  TCefWaitableEvent = record
    base        : TCefBaseRefCounted;
    reset       : procedure(self: PCefWaitableEvent); stdcall;
    signal      : procedure(self: PCefWaitableEvent); stdcall;
    is_signaled : function(self: PCefWaitableEvent): integer; stdcall;
    wait        : procedure(self: PCefWaitableEvent); stdcall;
    timed_wait  : function(self: PCefWaitableEvent; max_ms: int64): integer; stdcall;
  end;

  /// <summary>
  /// Structure that asynchronously executes tasks on the associated thread. It is
  /// safe to call the functions of this structure on any thread.
  ///
  /// CEF maintains multiple internal threads that are used for handling different
  /// types of tasks in different processes. The TCefThreadId definitions in
  /// cef_types.h list the common CEF threads. Task runners are also available for
  /// other CEF threads as appropriate (for example, V8 WebWorker threads).
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefTaskRunner.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_task_capi.h">CEF source file: /include/capi/cef_task_capi.h (cef_task_runner_t)</see></para>
  /// </remarks>
  TCefTaskRunner = record
    base                      : TCefBaseRefCounted;
    is_same                   : function(self, that: PCefTaskRunner): Integer; stdcall;
    belongs_to_current_thread : function(self: PCefTaskRunner): Integer; stdcall;
    belongs_to_thread         : function(self: PCefTaskRunner; threadId: TCefThreadId): Integer; stdcall;
    post_task                 : function(self: PCefTaskRunner; task: PCefTask): Integer; stdcall;
    post_delayed_task         : function(self: PCefTaskRunner; task: PCefTask; delay_ms: Int64): Integer; stdcall;
  end;

  /// <summary>
  /// Implement this structure to receive notification when tracing has completed.
  /// The functions of this structure will be called on the browser process UI
  /// thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefEndTracingCallback.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_trace_capi.h">CEF source file: /include/capi/cef_trace_capi.h (cef_end_tracing_callback_t)</see></para>
  /// </remarks>
  TCefEndTracingCallback = record
    base                    : TCefBaseRefCounted;
    on_end_tracing_complete : procedure(self: PCefEndTracingCallback; const tracing_file: PCefString); stdcall;
  end;

  /// <summary>
  /// Structure used for retrieving resources from the resource bundle (*.pak)
  /// files loaded by CEF during startup or via the ICefResourceBundleHandler
  /// returned from ICefApp.GetResourceBundleHandler. See TCefSettings for
  /// additional options related to resource bundle loading. The functions of this
  /// structure may be called on any thread unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefResourceBundle.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_resource_bundle_capi.h">CEF source file: /include/capi/cef_resource_bundle_capi.h (cef_resource_bundle_t)</see></para>
  /// </remarks>
  TCefResourceBundle = record
    base                        : TCefBaseRefCounted;
    get_localized_string        : function(self: PCefResourceBundle; string_id: Integer): PCefStringUserFree; stdcall;
    get_data_resource           : function(self: PCefResourceBundle; resource_id: Integer): PCefBinaryValue; stdcall;
    get_data_resource_for_scale : function(self: PCefResourceBundle; resource_id: Integer; scale_factor: TCefScaleFactor): PCefBinaryValue; stdcall;
  end;

  /// <summary>
  /// Implement this structure to handle menu model events. The functions of this
  /// structure will be called on the browser process UI thread unless otherwise
  /// indicated.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefMenuModelDelegate.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_menu_model_delegate_capi.h">CEF source file: /include/capi/cef_menu_model_delegate_capi.h (cef_menu_model_delegate_t)</see></para>
  /// </remarks>
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

  /// <summary>
  /// Structure representing a message. Can be used on any process and thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefProcessMessage.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_process_message_capi.h">CEF source file: /include/capi/cef_process_message_capi.h (cef_process_message_t)</see></para>
  /// </remarks>
  TCefProcessMessage = record
    base                        : TCefBaseRefCounted;
    is_valid                    : function(self: PCefProcessMessage): Integer; stdcall;
    is_read_only                : function(self: PCefProcessMessage): Integer; stdcall;
    copy                        : function(self: PCefProcessMessage): PCefProcessMessage; stdcall;
    get_name                    : function(self: PCefProcessMessage): PCefStringUserFree; stdcall;
    get_argument_list           : function(self: PCefProcessMessage): PCefListValue; stdcall;
    get_shared_memory_region    : function(self: PCefProcessMessage): PCefSharedMemoryRegion; stdcall;
  end;

  /// <summary>
  /// Structure used to implement render process callbacks. The functions of this
  /// structure will be called on the render process main thread (TID_RENDERER)
  /// unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefRenderProcessHandler.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_render_process_handler_capi.h">CEF source file: /include/capi/cef_render_process_handler_capi.h (cef_render_process_handler_t)</see></para>
  /// </remarks>
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

  /// <summary>
  /// Implement this structure to handle events related to browser requests. The
  /// functions of this structure will be called on the thread indicated.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefRequestHandler.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_request_handler_capi.h">CEF source file: /include/capi/cef_request_handler_capi.h (cef_request_handler_t)</see></para>
  /// </remarks>
  TCefRequestHandler = record
    base                                : TCefBaseRefCounted;
    on_before_browse                    : function(self: PCefRequestHandler; browser: PCefBrowser; frame: PCefFrame; request: PCefRequest; user_gesture, isRedirect: Integer): Integer; stdcall;
    on_open_urlfrom_tab                 : function(self: PCefRequestHandler; browser: PCefBrowser; frame: PCefFrame; const target_url: PCefString; target_disposition: TCefWindowOpenDisposition; user_gesture: Integer): Integer; stdcall;
    get_resource_request_handler        : function(self: PCefRequestHandler; browser: PCefBrowser; frame: PCefFrame; request: PCefRequest; is_navigation, is_download: Integer; const request_initiator: PCefString; disable_default_handling: PInteger): PCefResourceRequestHandler; stdcall;
    get_auth_credentials                : function(self: PCefRequestHandler; browser: PCefBrowser; const origin_url: PCefString; isProxy: Integer; const host: PCefString; port: Integer; const realm, scheme: PCefString; callback: PCefAuthCallback): Integer; stdcall;
    on_certificate_error                : function(self: PCefRequestHandler; browser: PCefBrowser; cert_error: TCefErrorcode; const request_url: PCefString; ssl_info: PCefSslInfo; callback: PCefCallback): Integer; stdcall;
    on_select_client_certificate        : function(self: PCefRequestHandler; browser: PCefBrowser; isProxy: integer; const host: PCefString; port: integer; certificatesCount: NativeUInt; const certificates: PPCefX509Certificate; callback: PCefSelectClientCertificateCallback): integer; stdcall;
    on_render_view_ready                : procedure(self: PCefRequestHandler; browser: PCefBrowser); stdcall;
    on_render_process_unresponsive      : function(self: PCefRequestHandler; browser: PCefBrowser; callback: PCefUnresponsiveProcessCallback): integer; stdcall;
    on_render_process_responsive        : procedure(self: PCefRequestHandler; browser: PCefBrowser); stdcall;
    on_render_process_terminated        : procedure(self: PCefRequestHandler; browser: PCefBrowser; status: TCefTerminationStatus; error_code: integer; const error_string: PCefString); stdcall;
    on_document_available_in_main_frame : procedure(self: PCefRequestHandler; browser: PCefBrowser); stdcall;
  end;

  /// <summary>
  /// Callback structure used for asynchronous continuation of media access
  /// permission requests.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefMediaAccessCallback.</para>
  /// This record is declared twice with almost identical parameters. "allowed_permissions" is defined as int and uint32.
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_media_access_handler_capi.h">CEF source file: /include/capi/cef_media_access_handler_capi.h (cef_media_access_callback_t)</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_permission_handler_capi.h">CEF source file: /include/capi/cef_permission_handler_capi.h (cef_media_access_callback_t)</see></para>
  /// </remarks>
  TCefMediaAccessCallback = record
    base   : TCefBaseRefCounted;
    cont   : procedure(self: PCefMediaAccessCallback; allowed_permissions: integer); stdcall;
    cancel : procedure(self: PCefMediaAccessCallback); stdcall;
  end;

  /// <summary>
  /// Implement this structure to handle events related to media access permission
  /// requests. The functions of this structure will be called on the browser
  /// process UI thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefMediaAccessHandler.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_media_access_handler_capi.h">CEF source file: /include/capi/cef_media_access_handler_capi.h (cef_media_access_handler_t)</see></para>
  /// </remarks>
  TCefMediaAccessHandler = record
    base                               : TCefBaseRefCounted;
    on_request_media_access_permission : function(self: PCefMediaAccessHandler; browser: PCefBrowser; frame: PCefFrame; const requesting_url: PCefString; requested_permissions: integer; callback: PCefMediaAccessCallback): integer; stdcall;
  end;

  /// <summary>
  /// Callback structure used for asynchronous continuation of permission prompts.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefPermissionPromptCallback.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_permission_handler_capi.h">CEF source file: /include/capi/cef_permission_handler_capi.h (cef_permission_prompt_callback_t)</see></para>
  /// </remarks>
  TCefPermissionPromptCallback = record
    base : TCefBaseRefCounted;
    cont : procedure(self: PCefPermissionPromptCallback; result: TCefPermissionRequestResult); stdcall;
  end;

  /// <summary>
  /// Implement this structure to handle events related to permission requests.
  /// The functions of this structure will be called on the browser process UI
  /// thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefPermissionHandler.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_permission_handler_capi.h">CEF source file: /include/capi/cef_permission_handler_capi.h (cef_permission_handler_t)</see></para>
  /// </remarks>
  TCefPermissionHandler = record
    base                               : TCefBaseRefCounted;
    on_request_media_access_permission : function(self: PCefPermissionHandler; browser: PCefBrowser; frame: PCefFrame; const requesting_origin: PCefString; requested_permissions: cardinal; callback: PCefMediaAccessCallback): integer; stdcall;
    on_show_permission_prompt          : function(self: PCefPermissionHandler; browser: PCefBrowser; prompt_id: uint64; const requesting_origin: PCefString; requested_permissions: cardinal; callback: PCefPermissionPromptCallback): integer; stdcall;
    on_dismiss_permission_prompt       : procedure(self: PCefPermissionHandler; browser: PCefBrowser; prompt_id: uint64; result: TCefPermissionRequestResult); stdcall;
  end;

  /// <summary>
  /// Structure that wraps platform-dependent share memory region mapping.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefSharedMemoryRegion.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_shared_memory_region_capi.h">CEF source file: /include/capi/cef_shared_memory_region_capi.h (cef_shared_memory_region_t)</see></para>
  /// </remarks>
  TCefSharedMemoryRegion = record
    base      : TCefBaseRefCounted;
    is_valid  : function(self: PCefSharedMemoryRegion): integer; stdcall;
    size      : function(self: PCefSharedMemoryRegion): NativeUInt; stdcall;
    memory    : function(self: PCefSharedMemoryRegion): pointer; stdcall;
  end;

  /// <summary>
  /// Structure that builds a ICefProcessMessage containing a shared memory
  /// region. This structure is not thread-safe but may be used exclusively on a
  /// different thread from the one which constructed it.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefSharedProcessMessageBuilder.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_shared_process_message_builder_capi.h">CEF source file: /include/capi/cef_shared_process_message_builder_capi.h (cef_shared_process_message_builder_t)</see></para>
  /// </remarks>
  TCefSharedProcessMessageBuilder = record
    base      : TCefBaseRefCounted;
    is_valid  : function(self: PCefSharedProcessMessageBuilder): integer; stdcall;
    size      : function(self: PCefSharedProcessMessageBuilder): NativeUInt; stdcall;
    memory    : function(self: PCefSharedProcessMessageBuilder): pointer; stdcall;
    build     : function(self: PCefSharedProcessMessageBuilder): PCefProcessMessage; stdcall;
  end;

  /// <summary>
  /// Callback for asynchronous continuation of ICefResourceHandler.skip().
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefResourceSkipCallback.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_resource_handler_capi.h">CEF source file: /include/capi/cef_resource_handler_capi.h (cef_resource_skip_callback_t)</see></para>
  /// </remarks>
  TCefResourceSkipCallback = record
    base   : TCefBaseRefCounted;
    cont   : procedure(self: PCefResourceSkipCallback; bytes_skipped: int64); stdcall;
  end;

  /// <summary>
  /// Callback for asynchronous continuation of ICefResourceHandler.read().
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefResourceReadCallback.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_resource_handler_capi.h">CEF source file: /include/capi/cef_resource_handler_capi.h (cef_resource_read_callback_t)</see></para>
  /// </remarks>
  TCefResourceReadCallback = record
    base   : TCefBaseRefCounted;
    cont   : procedure(self: PCefResourceReadCallback; bytes_read: int64); stdcall;
  end;

  /// <summary>
  /// Structure used to implement a custom request handler structure. The
  /// functions of this structure will be called on the IO thread unless otherwise
  /// indicated.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefResourceHandler.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_resource_handler_capi.h">CEF source file: /include/capi/cef_resource_handler_capi.h (cef_resource_handler_t)</see></para>
  /// </remarks>
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

  /// <summary>
  /// Implement this structure to handle events related to browser requests. The
  /// functions of this structure will be called on the IO thread unless otherwise
  /// indicated.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefResourceRequestHandler.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_resource_request_handler_capi.h">CEF source file: /include/capi/cef_resource_request_handler_capi.h (cef_resource_request_handler_t)</see></para>
  /// </remarks>
  TCefResourceRequestHandler = record
    base                          : TCefBaseRefCounted;
    get_cookie_access_filter      : function(self: PCefResourceRequestHandler; browser: PCefBrowser; frame: PCefFrame; request: PCefRequest): PCefCookieAccessFilter; stdcall;
    on_before_resource_load       : function(self: PCefResourceRequestHandler; browser: PCefBrowser; frame: PCefFrame; request: PCefRequest; callback: PCefCallback): TCefReturnValue; stdcall;
    get_resource_handler          : function(self: PCefResourceRequestHandler; browser: PCefBrowser; frame: PCefFrame; request: PCefRequest): PCefResourceHandler; stdcall;
    on_resource_redirect          : procedure(self: PCefResourceRequestHandler; browser: PCefBrowser; frame: PCefFrame; request: PCefRequest; response: PCefResponse; new_url: PCefString); stdcall;
    on_resource_response          : function(self: PCefResourceRequestHandler; browser: PCefBrowser; frame: PCefFrame; request: PCefRequest; response: PCefResponse): Integer; stdcall;
    get_resource_response_filter  : function(self: PCefResourceRequestHandler; browser: PCefBrowser; frame: PCefFrame; request: PCefRequest; response: PCefResponse): PCefResponseFilter; stdcall;
    on_resource_load_complete     : procedure(self: PCefResourceRequestHandler; browser: PCefBrowser; frame: PCefFrame; request: PCefRequest; response: PCefResponse; status: TCefUrlRequestStatus; received_content_length: Int64); stdcall;
    on_protocol_execution         : procedure(self: PCefResourceRequestHandler; browser: PCefBrowser; frame: PCefFrame; request: PCefRequest; allow_os_execution: PInteger); stdcall;
  end;

  /// <summary>
  /// Implement this structure to filter cookies that may be sent or received from
  /// resource requests. The functions of this structure will be called on the IO
  /// thread unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefCookieAccessFilter.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_resource_request_handler_capi.h">CEF source file: /include/capi/cef_resource_request_handler_capi.h (cef_cookie_access_filter_t)</see></para>
  /// </remarks>
  TCefCookieAccessFilter = record
    base                  : TCefBaseRefCounted;
    can_send_cookie       : function(self: PCefCookieAccessFilter; browser: PCefBrowser; frame: PCefFrame; request: PCefRequest; const cookie: PCefCookie): Integer; stdcall;
    can_save_cookie       : function(self: PCefCookieAccessFilter; browser: PCefBrowser; frame: PCefFrame; request: PCefRequest; response: PCefResponse; const cookie: PCefCookie): Integer; stdcall;
  end;

  /// <summary>
  /// Structure used to represent a web response. The functions of this structure
  /// may be called on any thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefResponse.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_response_capi.h">CEF source file: /include/capi/cef_response_capi.h (cef_response_t)</see></para>
  /// </remarks>
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

  /// <summary>
  /// Implement this structure to filter resource response content. The functions
  /// of this structure will be called on the browser process IO thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefResponseFilter.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_response_filter_capi.h">CEF source file: /include/capi/cef_response_filter_capi.h (cef_response_filter_t)</see></para>
  /// </remarks>
  TCefResponseFilter = record
    base        : TCefBaseRefCounted;
    init_filter : function(self: PCefResponseFilter): Integer; stdcall;
    filter      : function(self: PCefResponseFilter; data_in: Pointer; data_in_size: NativeUInt; var data_in_read: NativeUInt; data_out: Pointer; data_out_size : NativeUInt; var data_out_written: NativeUInt): TCefResponseFilterStatus; stdcall;
  end;

  /// <summary>
  /// Callback structure used for asynchronous continuation of authentication
  /// requests.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefAuthCallback.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_auth_callback_capi.h">CEF source file: /include/capi/cef_auth_callback_capi.h (cef_auth_callback_t)</see></para>
  /// </remarks>
  TCefAuthCallback = record
    base   : TCefBaseRefCounted;
    cont   : procedure(self: PCefAuthCallback; const username, password: PCefString); stdcall;
    cancel : procedure(self: PCefAuthCallback); stdcall;
  end;

  /// <summary>
  /// Generic callback structure used for asynchronous continuation.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefCallback.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_callback_capi.h">CEF source file: /include/capi/cef_callback_capi.h (cef_callback_t)</see></para>
  /// </remarks>
  TCefCallback = record
    base   : TCefBaseRefCounted;
    cont   : procedure(self: PCefCallback); stdcall;
    cancel : procedure(self: PCefCallback); stdcall;
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
  /// <para>Implemented by ICefRequestContext.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_request_context_capi.h">CEF source file: /include/capi/cef_request_context_capi.h (cef_request_context_t)</see></para>
  /// </remarks>
  TCefRequestContext = record
    base                            : TCefPreferenceManager;
    is_same                         : function(self, other: PCefRequestContext): Integer; stdcall;
    is_sharing_with                 : function(self, other: PCefRequestContext): Integer; stdcall;
    is_global                       : function(self: PCefRequestContext): Integer; stdcall;
    get_handler                     : function(self: PCefRequestContext): PCefRequestContextHandler; stdcall;
    get_cache_path                  : function(self: PCefRequestContext): PCefStringUserFree; stdcall;
    get_cookie_manager              : function(self: PCefRequestContext; callback: PCefCompletionCallback): PCefCookieManager; stdcall;
    register_scheme_handler_factory : function(self: PCefRequestContext; const scheme_name, domain_name: PCefString; factory: PCefSchemeHandlerFactory): Integer; stdcall;
    clear_scheme_handler_factories  : function(self: PCefRequestContext): Integer; stdcall;
    clear_certificate_exceptions    : procedure(self: PCefRequestContext; callback: PCefCompletionCallback); stdcall;
    clear_http_auth_credentials     : procedure(self: PCefRequestContext; callback: PCefCompletionCallback); stdcall;
    close_all_connections           : procedure(self: PCefRequestContext; callback: PCefCompletionCallback); stdcall;
    resolve_host                    : procedure(self: PCefRequestContext; const origin: PCefString; callback: PCefResolveCallback); stdcall;
    get_media_router                : function(self: PCefRequestContext; callback: PCefCompletionCallback): PCefMediaRouter; stdcall;
    get_website_setting             : function(self: PCefRequestContext; const requesting_url, top_level_url: PCefString; content_type: TCefContentSettingTypes): PCefValue; stdcall;
    set_website_setting             : procedure(self: PCefRequestContext; const requesting_url, top_level_url: PCefString; content_type: TCefContentSettingTypes; value: PCefValue); stdcall;
    get_content_setting             : function(self: PCefRequestContext; const requesting_url, top_level_url: PCefString; content_type: TCefContentSettingTypes): TCefContentSettingValues; stdcall;
    set_content_setting             : procedure(self: PCefRequestContext; const requesting_url, top_level_url: PCefString; content_type: TCefContentSettingTypes; value: TCefContentSettingValues); stdcall;
    set_chrome_color_scheme         : procedure(self: PCefRequestContext; variant: TCefColorVariant; user_color: TCefColor); stdcall;
    get_chrome_color_scheme_mode    : function(self: PCefRequestContext): TCefColorVariant; stdcall;
    get_chrome_color_scheme_color   : function(self: PCefRequestContext): TCefColor; stdcall;
    get_chrome_color_scheme_variant : function(self: PCefRequestContext): TCefColorVariant; stdcall;
  end;

  /// <summary>
  /// Implement this structure to provide handler implementations. The handler
  /// instance will not be released until all objects related to the context have
  /// been destroyed.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefRequestContextHandler.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_request_context_handler_capi.h">CEF source file: /include/capi/cef_request_context_handler_capi.h (cef_request_context_handler_t)</see></para>
  /// </remarks>
  TCefRequestContextHandler = record
    base                            : TCefBaseRefCounted;
    on_request_context_initialized  : procedure(self: PCefRequestContextHandler; request_context: PCefRequestContext); stdcall;
    get_resource_request_handler    : function(self: PCefRequestContextHandler; browser: PCefBrowser; frame: PCefFrame; request: PCefRequest; is_navigation, is_download: Integer; const request_initiator: PCefString; disable_default_handling: PInteger): PCefResourceRequestHandler; stdcall;
  end;

  /// <summary>
  /// Generic callback structure used for asynchronous completion.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefCompletionCallback.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_callback_capi.h">CEF source file: /include/capi/cef_callback_capi.h (cef_completion_callback_t)</see></para>
  /// </remarks>
  TCefCompletionCallback = record
    base        : TCefBaseRefCounted;
    on_complete : procedure(self: PCefCompletionCallback); stdcall;
  end;

  /// <summary>
  /// Structure used for managing cookies. The functions of this structure may be
  /// called on any thread unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefCookieManager.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_cookie_capi.h">CEF source file: /include/capi/cef_cookie_capi.h (cef_cookie_manager_t)</see></para>
  /// </remarks>
  TCefCookieManager = record
    base                  : TCefBaseRefCounted;
    visit_all_cookies     : function(self: PCefCookieManager; visitor: PCefCookieVisitor): Integer; stdcall;
    visit_url_cookies     : function(self: PCefCookieManager; const url: PCefString; includeHttpOnly: Integer; visitor: PCefCookieVisitor): Integer; stdcall;
    set_cookie            : function(self: PCefCookieManager; const url: PCefString; const cookie: PCefCookie; callback: PCefSetCookieCallback): Integer; stdcall;
    delete_cookies        : function(self: PCefCookieManager; const url, cookie_name: PCefString; callback: PCefDeleteCookiesCallback): Integer; stdcall;
    flush_store           : function(self: PCefCookieManager; callback: PCefCompletionCallback): Integer; stdcall;
  end;

  /// <summary>
  /// Structure that creates ICefResourceHandler instances for handling scheme
  /// requests. The functions of this structure will always be called on the IO
  /// thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefSchemeHandlerFactory.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_scheme_capi.h">CEF source file: /include/capi/cef_scheme_capi.h (cef_scheme_handler_factory_t)</see></para>
  /// </remarks>
  TCefSchemeHandlerFactory = record
    base   : TCefBaseRefCounted;
    create : function(self: PCefSchemeHandlerFactory; browser: PCefBrowser; frame: PCefFrame; const scheme_name: PCefString; request: PCefRequest): PCefResourceHandler; stdcall;
  end;

  /// <summary>
  /// Callback structure for ICefRequestContext.ResolveHost.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefResolveCallback.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_request_context_capi.h">CEF source file: /include/capi/cef_request_context_capi.h (cef_resolve_callback_t)</see></para>
  /// </remarks>
  TCefResolveCallback = record
    base                 : TCefBaseRefCounted;
    on_resolve_completed : procedure(self: PCefResolveCallback; result: TCefErrorCode; resolved_ips: TCefStringList); stdcall;
  end;

  /// <summary>
  /// Structure to implement for visiting cookie values. The functions of this
  /// structure will always be called on the UI thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefCookieVisitor.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_cookie_capi.h">CEF source file: /include/capi/cef_cookie_capi.h (cef_cookie_visitor_t)</see></para>
  /// </remarks>
  TCefCookieVisitor = record
    base  : TCefBaseRefCounted;
    visit : function(self: PCefCookieVisitor; const cookie: PCefCookie; count, total: Integer; deleteCookie: PInteger): Integer; stdcall;
  end;

  /// <summary>
  /// Structure to implement to be notified of asynchronous completion via
  /// ICefCookieManager.SetCookie().
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefSetCookieCallback.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_cookie_capi.h">CEF source file: /include/capi/cef_cookie_capi.h (cef_set_cookie_callback_t)</see></para>
  /// </remarks>
  TCefSetCookieCallback = record
    base        : TCefBaseRefCounted;
    on_complete : procedure(self: PCefSetCookieCallback; success: Integer); stdcall;
  end;

  /// <summary>
  /// Structure to implement to be notified of asynchronous completion via
  /// ICefCookieManager.DeleteCookies().
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefDeleteCookiesCallback.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_cookie_capi.h">CEF source file: /include/capi/cef_cookie_capi.h (cef_delete_cookies_callback_t)</see></para>
  /// </remarks>
  TCefDeleteCookiesCallback = record
    base        : TCefBaseRefCounted;
    on_complete : procedure(self: PCefDeleteCookiesCallback; num_deleted: Integer); stdcall;
  end;

  /// <summary>
  /// Callback structure for ICefBrowserHost.RunFileDialog. The functions of
  /// this structure will be called on the browser process UI thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefRunFileDialogCallback.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_browser_capi.h">CEF source file: /include/capi/cef_browser_capi.h (cef_run_file_dialog_callback_t)</see></para>
  /// </remarks>
  TCefRunFileDialogCallback = record
    base                     : TCefBaseRefCounted;
    on_file_dialog_dismissed : procedure(self: PCefRunFileDialogCallback; file_paths: TCefStringList); stdcall;
  end;

  /// <summary>
  /// Callback structure for ICefBrowserHost.DownloadImage. The functions of
  /// this structure will be called on the browser process UI thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefDownloadImageCallback.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_browser_capi.h">CEF source file: /include/capi/cef_browser_capi.h (cef_download_image_callback_t)</see></para>
  /// </remarks>
  TCefDownloadImageCallback = record
    base                       : TCefBaseRefCounted;
    on_download_image_finished : procedure(self: PCefDownloadImageCallback; const image_url: PCefString; http_status_code: Integer; image: PCefImage); stdcall;
  end;

  /// <summary>
  /// Container for a single image represented at different scale factors. All
  /// image representations should be the same size in density independent pixel
  /// (DIP) units. For example, if the image at scale factor 1.0 is 100x100 pixels
  /// then the image at scale factor 2.0 should be 200x200 pixels -- both images
  /// will display with a DIP size of 100x100 units. The functions of this
  /// structure can be called on any browser process thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefImage.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_image_capi.h">CEF source file: /include/capi/cef_image_capi.h (cef_image_t)</see></para>
  /// </remarks>
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
    get_representation_info : function(self: PCefImage; scale_factor: Single; actual_scale_factor: System.PSingle; pixel_width, pixel_height: PInteger): Integer; stdcall;
    get_as_bitmap           : function(self: PCefImage; scale_factor: Single; color_type: TCefColorType; alpha_type: TCefAlphaType; pixel_width, pixel_height: PInteger): PCefBinaryValue; stdcall;
    get_as_png              : function(self: PCefImage; scale_factor: Single; with_transparency: Integer; pixel_width, pixel_height: PInteger): PCefBinaryValue; stdcall;
    get_as_jpeg             : function(self: PCefImage; scale_factor: Single; quality: Integer; pixel_width, pixel_height: PInteger): PCefBinaryValue; stdcall;
  end;

  /// <summary>
  /// Callback structure for ICefBrowserHost.PrintToPDF. The functions of this
  /// structure will be called on the browser process UI thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefPdfPrintCallback.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_browser_capi.h">CEF source file: /include/capi/cef_browser_capi.h (cef_pdf_print_callback_t)</see></para>
  /// </remarks>
  TCefPdfPrintCallback = record
    base                  : TCefBaseRefCounted;
    on_pdf_print_finished : procedure(self: PCefPdfPrintCallback; const path: PCefString; ok: Integer); stdcall;
  end;

  /// <summary>
  /// Callback structure for ICefBrowserHost.GetNavigationEntries. The
  /// functions of this structure will be called on the browser process UI thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefNavigationEntryVisitor.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_browser_capi.h">CEF source file: /include/capi/cef_browser_capi.h (cef_navigation_entry_visitor_t)</see></para>
  /// </remarks>
  TCefNavigationEntryVisitor = record
    base  : TCefBaseRefCounted;
    visit : function(self: PCefNavigationEntryVisitor; entry: PCefNavigationEntry; current, index, total: Integer): Integer; stdcall;
  end;

  /// <summary>
  /// Structure used to represent an entry in navigation history.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefNavigationEntry.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_navigation_entry_capi.h">CEF source file: /include/capi/cef_navigation_entry_capi.h (cef_navigation_entry_t)</see></para>
  /// </remarks>
  TCefNavigationEntry = record
    base                  : TCefBaseRefCounted;
    is_valid              : function(self: PCefNavigationEntry): Integer; stdcall;
    get_url               : function(self: PCefNavigationEntry): PCefStringUserFree; stdcall;
    get_display_url       : function(self: PCefNavigationEntry): PCefStringUserFree; stdcall;
    get_original_url      : function(self: PCefNavigationEntry): PCefStringUserFree; stdcall;
    get_title             : function(self: PCefNavigationEntry): PCefStringUserFree; stdcall;
    get_transition_type   : function(self: PCefNavigationEntry): TCefTransitionType; stdcall;
    has_post_data         : function(self: PCefNavigationEntry): Integer; stdcall;
    get_completion_time   : function(self: PCefNavigationEntry): TCefBaseTime; stdcall;
    get_http_status_code  : function(self: PCefNavigationEntry): Integer; stdcall;
    get_sslstatus         : function(self: PCefNavigationEntry): PCefSSLStatus; stdcall;
  end;

  /// <summary>
  /// Structure representing print settings.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefPrintSettings.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_print_settings_capi.h">CEF source file: /include/capi/cef_print_settings_capi.h (cef_print_settings_t)</see></para>
  /// </remarks>
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

  /// <summary>
  /// Callback structure for asynchronous continuation of print dialog requests.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefPrintDialogCallback.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_print_handler_capi.h">CEF source file: /include/capi/cef_print_handler_capi.h (cef_print_dialog_callback_t)</see></para>
  /// </remarks>
  TCefPrintDialogCallback = record
    base   : TCefBaseRefCounted;
    cont   : procedure(self: PCefPrintDialogCallback; settings: PCefPrintSettings); stdcall;
    cancel : procedure(self: PCefPrintDialogCallback); stdcall;
  end;

  /// <summary>
  /// Callback structure for asynchronous continuation of print job requests.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefPrintJobCallback.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_print_handler_capi.h">CEF source file: /include/capi/cef_print_handler_capi.h (cef_print_job_callback_t)</see></para>
  /// </remarks>
  TCefPrintJobCallback = record
    base : TCefBaseRefCounted;
    cont : procedure(self: PCefPrintJobCallback); stdcall;
  end;

  /// <summary>
  /// Implement this structure to handle printing on Linux. Each browser will have
  /// only one print job in progress at a time. The functions of this structure
  /// will be called on the browser process UI thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefPrintHandler.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_print_handler_capi.h">CEF source file: /include/capi/cef_print_handler_capi.h (cef_print_handler_t)</see></para>
  /// </remarks>
  TCefPrintHandler = record
    base                : TCefBaseRefCounted;
    on_print_start      : procedure(self: PCefPrintHandler; browser: PCefBrowser); stdcall;
    on_print_settings   : procedure(self: PCefPrintHandler; browser: PCefBrowser; settings: PCefPrintSettings; get_defaults: Integer); stdcall;
    on_print_dialog     : function(self: PCefPrintHandler; browser: PCefBrowser; has_selection: Integer; callback: PCefPrintDialogCallback): Integer; stdcall;
    on_print_job        : function(self: PCefPrintHandler; browser: PCefBrowser; const document_name, pdf_file_path: PCefString; callback: PCefPrintJobCallback): Integer; stdcall;
    on_print_reset      : procedure(self: PCefPrintHandler; browser: PCefBrowser); stdcall;
    get_pdf_paper_size  : function(self: PCefPrintHandler; browser: PCefBrowser; device_units_per_inch: Integer): TCefSize; stdcall;
  end;

  /// <summary>
  /// Structure used to represent drag data. The functions of this structure may
  /// be called on any thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefDragData.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_drag_data_capi.h">CEF source file: /include/capi/cef_drag_data_capi.h (cef_drag_data_t)</see></para>
  /// </remarks>
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
    get_file_paths        : function(self: PCefDragData; paths: TCefStringList): Integer; stdcall;
    set_link_url          : procedure(self: PCefDragData; const url: PCefString); stdcall;
    set_link_title        : procedure(self: PCefDragData; const title: PCefString); stdcall;
    set_link_metadata     : procedure(self: PCefDragData; const data: PCefString); stdcall;
    set_fragment_text     : procedure(self: PCefDragData; const text: PCefString); stdcall;
    set_fragment_html     : procedure(self: PCefDragData; const html: PCefString); stdcall;
    set_fragment_base_url : procedure(self: PCefDragData; const base_url: PCefString); stdcall;
    reset_file_contents   : procedure(self: PCefDragData); stdcall;
    add_file              : procedure(self: PCefDragData; const path, display_name: PCefString); stdcall;
    clear_filenames       : procedure(self: PCefDragData); stdcall;
    get_image             : function(self: PCefDragData): PCefImage; stdcall;
    get_image_hotspot     : function(self: PCefDragData): PCefPoint; stdcall;
    has_image             : function(self: PCefDragData): Integer; stdcall;
  end;

  /// <summary>
  /// Structure used to create and/or parse command line arguments. Arguments with
  /// "--", "-" and, on Windows, "/" prefixes are considered switches. Switches
  /// will always precede any arguments without switch prefixes. Switches can
  /// optionally have a value specified using the "=" delimiter (e.g.
  /// "-switch=value"). An argument of "--" will terminate switch parsing with all
  /// subsequent tokens, regardless of prefix, being interpreted as non-switch
  /// arguments. Switch names should be lowercase ASCII and will be converted to
  /// such if necessary. Switch values will retain the original case and UTF8
  /// encoding. This structure can be used before cef_initialize() is called.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefCommandLine.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_command_line_capi.h">CEF source file: /include/capi/cef_command_line_capi.h (cef_command_line_t)</see></para>
  /// </remarks>
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

  /// <summary>
  /// Implement this structure to handle events related to commands. The functions
  /// of this structure will be called on the UI thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefCommandHandler.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_command_handler_capi.h">CEF source file: /include/capi/cef_command_handler_capi.h (cef_command_handler_t)</see></para>
  /// </remarks>
  TCefCommandHandler = record
    base                                 : TCefBaseRefCounted;
    on_chrome_command                    : function(self: PCefCommandHandler; browser: PCefBrowser; command_id: integer; disposition: TCefWindowOpenDisposition): Integer; stdcall;
    is_chrome_app_menu_item_visible      : function(self: PCefCommandHandler; browser: PCefBrowser; command_id: integer): integer; stdcall;
    is_chrome_app_menu_item_enabled      : function(self: PCefCommandHandler; browser: PCefBrowser; command_id: integer): integer; stdcall;
    is_chrome_page_action_icon_visible   : function(self: PCefCommandHandler; icon_type: TCefChromePageActionIconType): integer; stdcall;
    is_chrome_toolbar_button_visible     : function(self: PCefCommandHandler; button_type: TCefChromeToolbarButtonType): integer; stdcall;
  end;

  /// <summary>
  /// Structure that manages custom scheme registrations.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by TCefSchemeRegistrarRef.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_scheme_capi.h">CEF source file: /include/capi/cef_scheme_capi.h (cef_scheme_registrar_t)</see></para>
  /// </remarks>
  TCefSchemeRegistrar = record
    base              : TCefBaseScoped;
    add_custom_scheme : function(self: PCefSchemeRegistrar; const scheme_name: PCefString; options : TCefSchemeOptions): Integer; stdcall;
  end;

  /// <summary>
  /// Structure representing a binary value. Can be used on any process and
  /// thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefBinaryValue.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_values_capi.h">CEF source file: /include/capi/cef_values_capi.h (cef_binary_value_t)</see></para>
  /// </remarks>
  TCefBinaryValue = record
    base          : TCefBaseRefCounted;
    is_valid      : function(self: PCefBinaryValue): Integer; stdcall;
    is_owned      : function(self: PCefBinaryValue): Integer; stdcall;
    is_same       : function(self, that: PCefBinaryValue):Integer; stdcall;
    is_equal      : function(self, that: PCefBinaryValue): Integer; stdcall;
    copy          : function(self: PCefBinaryValue): PCefBinaryValue; stdcall;
    get_raw_data  : function(self: PCefBinaryValue): Pointer; stdcall;
    get_size      : function(self: PCefBinaryValue): NativeUInt; stdcall;
    get_data      : function(self: PCefBinaryValue; buffer: Pointer; buffer_size, data_offset: NativeUInt): NativeUInt; stdcall;
  end;

  /// <summary>
  /// Structure that wraps other data value types. Complex types (binary,
  /// dictionary and list) will be referenced but not owned by this object. Can be
  /// used on any process and thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefValue.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_values_capi.h">CEF source file: /include/capi/cef_values_capi.h (cef_value_t)</see></para>
  /// </remarks>
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

  /// <summary>
  /// Structure representing a dictionary value. Can be used on any process and
  /// thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefDictionaryValue.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_values_capi.h">CEF source file: /include/capi/cef_values_capi.h (cef_dictionary_value_t)</see></para>
  /// </remarks>
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

  /// <summary>
  /// Structure representing a list value. Can be used on any process and thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefListValue.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_values_capi.h">CEF source file: /include/capi/cef_values_capi.h (cef_list_value_t)</see></para>
  /// </remarks>
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

  /// <summary>
  /// Implement this structure to receive string values asynchronously.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefStringVisitor.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_string_visitor_capi.h">CEF source file: /include/capi/cef_string_visitor_capi.h (cef_string_visitor_t)</see></para>
  /// </remarks>
  TCefStringVisitor = record
    base  : TCefBaseRefCounted;
    visit : procedure(self: PCefStringVisitor; const str: PCefString); stdcall;
  end;

  /// <summary>
  /// Structure used to represent a single element in the request post data. The
  /// functions of this structure may be called on any thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefPostDataElement.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_request_capi.h">CEF source file: /include/capi/cef_request_capi.h (cef_post_data_element_t)</see></para>
  /// </remarks>
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

  /// <summary>
  /// Structure used to represent post data for a web request. The functions of
  /// this structure may be called on any thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefPostData.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_request_capi.h">CEF source file: /include/capi/cef_request_capi.h (cef_post_data_t)</see></para>
  /// </remarks>
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

  /// <summary>
  /// Structure used to represent a web request. The functions of this structure
  /// may be called on any thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefRequest.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_request_capi.h">CEF source file: /include/capi/cef_request_capi.h (cef_request_t)</see></para>
  /// </remarks>
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

  /// <summary>
  /// Implement this structure for asynchronous task execution. If the task is
  /// posted successfully and if the associated message loop is still running then
  /// the execute() function will be called on the target thread. If the task
  /// fails to post then the task object may be destroyed on the source thread
  /// instead of the target thread. For this reason be cautious when performing
  /// work in the task object destructor.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefTask.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_task_capi.h">CEF source file: /include/capi/cef_task_capi.h (cef_task_t)</see></para>
  /// </remarks>
  TCefTask = record
    base    : TCefBaseRefCounted;
    execute : procedure(self: PCefTask); stdcall;
  end;

  /// <summary>
  /// Structure that facilitates managing the browser-related tasks. The functions
  /// of this structure may only be called on the UI thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefTaskManager.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_task_manager_capi.h">CEF source file: /include/capi/cef_task_manager_capi.h (cef_task_manager_t)</see></para>
  /// </remarks>
  TCefTaskManager = record
    base                       : TCefBaseRefCounted;
    get_tasks_count            : function(self: PCefTaskManager): NativeUInt; stdcall;
    get_task_ids_list          : function(self: PCefTaskManager; task_idsCount: PNativeUInt; task_ids: PInt64): Integer; stdcall;
    get_task_info              : function(self: PCefTaskManager; task_id: int64; info: PCefTaskInfo): Integer; stdcall;
    kill_task                  : function(self: PCefTaskManager; task_id: int64): Integer; stdcall;
    get_task_id_for_browser_id : function(self: PCefTaskManager; browser_id: Integer): int64; stdcall;
  end;

  /// <summary>
  /// Structure to implement for visiting the DOM. The functions of this structure
  /// will be called on the render process main thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefDomVisitor.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_dom_capi.h">CEF source file: /include/capi/cef_dom_capi.h (cef_domvisitor_t)</see></para>
  /// </remarks>
  TCefDomVisitor = record
    base  : TCefBaseRefCounted;
    visit : procedure(self: PCefDomVisitor; document: PCefDomDocument); stdcall;
  end;

  /// <summary>
  /// Supports creation and modification of menus. See TCefMenuId (cef_menu_id_t) for the
  /// command ids that have default implementations. All user-defined command ids
  /// should be between MENU_ID_USER_FIRST and MENU_ID_USER_LAST. The functions of
  /// this structure can only be accessed on the browser process the UI thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefMenuModel.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_menu_model_capi.h">CEF source file: /include/capi/cef_menu_model_capi.h (cef_menu_model_t)</see></para>
  /// </remarks>
  TCefMenuModel = record
    base                  : TCefBaseRefCounted;
    is_sub_menu           : function(self: PCefMenuModel): Integer; stdcall;
    clear                 : function(self: PCefMenuModel): Integer; stdcall;
    get_count             : function(self: PCefMenuModel): NativeUInt; stdcall;
    add_separator         : function(self: PCefMenuModel): Integer; stdcall;
    add_item              : function(self: PCefMenuModel; command_id: Integer; const text: PCefString): Integer; stdcall;
    add_check_item        : function(self: PCefMenuModel; command_id: Integer; const text: PCefString): Integer; stdcall;
    add_radio_item        : function(self: PCefMenuModel; command_id: Integer; const text: PCefString; group_id: Integer): Integer; stdcall;
    add_sub_menu          : function(self: PCefMenuModel; command_id: Integer; const text: PCefString): PCefMenuModel; stdcall;
    insert_separator_at   : function(self: PCefMenuModel; index: NativeUInt): Integer; stdcall;
    insert_item_at        : function(self: PCefMenuModel; index: NativeUInt; command_id: Integer; const text: PCefString): Integer; stdcall;
    insert_check_item_at  : function(self: PCefMenuModel; index: NativeUInt; command_id: Integer; const text: PCefString): Integer; stdcall;
    insert_radio_item_at  : function(self: PCefMenuModel; index: NativeUInt; command_id: Integer; const text: PCefString; group_id: Integer): Integer; stdcall;
    insert_sub_menu_at    : function(self: PCefMenuModel; index: NativeUInt; command_id: Integer; const text: PCefString): PCefMenuModel; stdcall;
    remove                : function(self: PCefMenuModel; command_id: Integer): Integer; stdcall;
    remove_at             : function(self: PCefMenuModel; index: NativeUInt): Integer; stdcall;
    get_index_of          : function(self: PCefMenuModel; command_id: Integer): Integer; stdcall;
    get_command_id_at     : function(self: PCefMenuModel; index: NativeUInt): Integer; stdcall;
    set_command_id_at     : function(self: PCefMenuModel; index: NativeUInt; command_id: Integer): Integer; stdcall;
    get_label             : function(self: PCefMenuModel; command_id: Integer): PCefStringUserFree; stdcall;
    get_label_at          : function(self: PCefMenuModel; index: NativeUInt): PCefStringUserFree; stdcall;
    set_label             : function(self: PCefMenuModel; command_id: Integer; const text: PCefString): Integer; stdcall;
    set_label_at          : function(self: PCefMenuModel; index: NativeUInt; const text: PCefString): Integer; stdcall;
    get_type              : function(self: PCefMenuModel; command_id: Integer): TCefMenuItemType; stdcall;
    get_type_at           : function(self: PCefMenuModel; index: NativeUInt): TCefMenuItemType; stdcall;
    get_group_id          : function(self: PCefMenuModel; command_id: Integer): Integer; stdcall;
    get_group_id_at       : function(self: PCefMenuModel; index: NativeUInt): Integer; stdcall;
    set_group_id          : function(self: PCefMenuModel; command_id, group_id: Integer): Integer; stdcall;
    set_group_id_at       : function(self: PCefMenuModel; index: NativeUInt; group_id: Integer): Integer; stdcall;
    get_sub_menu          : function(self: PCefMenuModel; command_id: Integer): PCefMenuModel; stdcall;
    get_sub_menu_at       : function(self: PCefMenuModel; index: NativeUInt): PCefMenuModel; stdcall;
    is_visible            : function(self: PCefMenuModel; command_id: Integer): Integer; stdcall;
    is_visible_at         : function(self: PCefMenuModel; index: NativeUInt): Integer; stdcall;
    set_visible           : function(self: PCefMenuModel; command_id, visible: Integer): Integer; stdcall;
    set_visible_at        : function(self: PCefMenuModel; index: NativeUInt; visible: Integer): Integer; stdcall;
    is_enabled            : function(self: PCefMenuModel; command_id: Integer): Integer; stdcall;
    is_enabled_at         : function(self: PCefMenuModel; index: NativeUInt): Integer; stdcall;
    set_enabled           : function(self: PCefMenuModel; command_id, enabled: Integer): Integer; stdcall;
    set_enabled_at        : function(self: PCefMenuModel; index: NativeUInt; enabled: Integer): Integer; stdcall;
    is_checked            : function(self: PCefMenuModel; command_id: Integer): Integer; stdcall;
    is_checked_at         : function(self: PCefMenuModel; index: NativeUInt): Integer; stdcall;
    set_checked           : function(self: PCefMenuModel; command_id, checked: Integer): Integer; stdcall;
    set_checked_at        : function(self: PCefMenuModel; index: NativeUInt; checked: Integer): Integer; stdcall;
    has_accelerator       : function(self: PCefMenuModel; command_id: Integer): Integer; stdcall;
    has_accelerator_at    : function(self: PCefMenuModel; index: NativeUInt): Integer; stdcall;
    set_accelerator       : function(self: PCefMenuModel; command_id, key_code, shift_pressed, ctrl_pressed, alt_pressed: Integer): Integer; stdcall;
    set_accelerator_at    : function(self: PCefMenuModel; index: NativeUInt; key_code, shift_pressed, ctrl_pressed, alt_pressed: Integer): Integer; stdcall;
    remove_accelerator    : function(self: PCefMenuModel; command_id: Integer): Integer; stdcall;
    remove_accelerator_at : function(self: PCefMenuModel; index: NativeUInt): Integer; stdcall;
    get_accelerator       : function(self: PCefMenuModel; command_id: Integer; key_code, shift_pressed, ctrl_pressed, alt_pressed: PInteger): Integer; stdcall;
    get_accelerator_at    : function(self: PCefMenuModel; index: NativeUInt; key_code, shift_pressed, ctrl_pressed, alt_pressed: PInteger): Integer; stdcall;
    set_color             : function(self: PCefMenuModel; command_id: Integer; color_type: TCefMenuColorType; color: TCefColor): Integer; stdcall;
    set_color_at          : function(self: PCefMenuModel; index: Integer; color_type: TCefMenuColorType; color: TCefColor): Integer; stdcall;
    get_color             : function(self: PCefMenuModel; command_id: Integer; color_type: TCefMenuColorType; color: PCefColor): Integer; stdcall;
    get_color_at          : function(self: PCefMenuModel; index: Integer; color_type: TCefMenuColorType; color: PCefColor): Integer; stdcall;
    set_font_list         : function(self: PCefMenuModel; command_id: Integer; const font_list: PCefString): Integer; stdcall;
    set_font_list_at      : function(self: PCefMenuModel; index: Integer; const font_list: PCefString): Integer; stdcall;
  end;

  /// <summary>
  /// Provides information about the context menu state. The functions of this
  /// structure can only be accessed on browser process the UI thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefContextMenuParams.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_context_menu_handler_capi.h">CEF source file: /include/capi/cef_context_menu_handler_capi.h (cef_context_menu_params_t)</see></para>
  /// </remarks>
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
  end;

  /// <summary>
  /// Structure used to represent a download item.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefDownloadItem.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_download_item_capi.h">CEF source file: /include/capi/cef_download_item_capi.h (cef_download_item_t)</see></para>
  /// </remarks>
  TCefDownloadItem = record
    base                    : TCefBaseRefCounted;
    is_valid                : function(self: PCefDownloadItem): Integer; stdcall;
    is_in_progress          : function(self: PCefDownloadItem): Integer; stdcall;
    is_complete             : function(self: PCefDownloadItem): Integer; stdcall;
    is_canceled             : function(self: PCefDownloadItem): Integer; stdcall;
    is_interrupted          : function(self: PCefDownloadItem): Integer; stdcall;
    get_interrupt_reason    : function(self: PCefDownloadItem): TCefDownloadInterruptReason; stdcall;
    get_current_speed       : function(self: PCefDownloadItem): Int64; stdcall;
    get_percent_complete    : function(self: PCefDownloadItem): Integer; stdcall;
    get_total_bytes         : function(self: PCefDownloadItem): Int64; stdcall;
    get_received_bytes      : function(self: PCefDownloadItem): Int64; stdcall;
    get_start_time          : function(self: PCefDownloadItem): TCefBaseTime; stdcall;
    get_end_time            : function(self: PCefDownloadItem): TCefBaseTime; stdcall;
    get_full_path           : function(self: PCefDownloadItem): PCefStringUserFree; stdcall;
    get_id                  : function(self: PCefDownloadItem): Cardinal; stdcall;
    get_url                 : function(self: PCefDownloadItem): PCefStringUserFree; stdcall;
    get_original_url        : function(self: PCefDownloadItem): PCefStringUserFree; stdcall;
    get_suggested_file_name : function(self: PCefDownloadItem): PCefStringUserFree; stdcall;
    get_content_disposition : function(self: PCefDownloadItem): PCefStringUserFree; stdcall;
    get_mime_type           : function(self: PCefDownloadItem): PCefStringUserFree; stdcall;
  end;

  /// <summary>
  /// Callback structure used to asynchronously continue a download.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefBeforeDownloadCallback.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_download_handler_capi.h">CEF source file: /include/capi/cef_download_handler_capi.h (cef_before_download_callback_t)</see></para>
  /// </remarks>
  TCefBeforeDownloadCallback = record
    base : TCefBaseRefCounted;
    cont : procedure(self: PCefBeforeDownloadCallback; const download_path: PCefString; show_dialog: Integer); stdcall;
  end;

  /// <summary>
  /// Callback structure used to asynchronously cancel a download.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefDownloadItemCallback.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_download_handler_capi.h">CEF source file: /include/capi/cef_download_handler_capi.h (cef_download_item_callback_t)</see></para>
  /// </remarks>
  TCefDownloadItemCallback = record
    base   : TCefBaseRefCounted;
    cancel : procedure(self: PCefDownloadItemCallback); stdcall;
    pause  : procedure(self: PCefDownloadItemCallback); stdcall;
    resume : procedure(self: PCefDownloadItemCallback); stdcall;
  end;

  /// <summary>
  /// Structure used to represent a DOM node. The functions of this structure
  /// should only be called on the render process main thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefDomNode.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_dom_capi.h">CEF source file: /include/capi/cef_dom_capi.h (cef_domnode_t)</see></para>
  /// </remarks>
  TCefDomNode = record
    base                          : TCefBaseRefCounted;
    get_type                      : function(self: PCefDomNode): TCefDomNodeType; stdcall;
    is_text                       : function(self: PCefDomNode): Integer; stdcall;
    is_element                    : function(self: PCefDomNode): Integer; stdcall;
    is_editable                   : function(self: PCefDomNode): Integer; stdcall;
    is_form_control_element       : function(self: PCefDomNode): Integer; stdcall;
    get_form_control_element_type : function(self: PCefDomNode): TCefDomFormControlType; stdcall;
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

  /// <summary>
  /// Structure used to represent a DOM document. The functions of this structure
  /// should only be called on the render process main thread thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefDomDocument.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_dom_capi.h">CEF source file: /include/capi/cef_dom_capi.h (cef_domdocument_t)</see></para>
  /// </remarks>
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

  /// <summary>
  /// Structure that should be implemented to handle V8 function calls. The
  /// functions of this structure will be called on the thread associated with the
  /// V8 function.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefv8Handler.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_v8_capi.h">CEF source file: /include/capi/cef_v8_capi.h (cef_v8handler_t)</see></para>
  /// </remarks>
  TCefv8Handler = record
    base    : TCefBaseRefCounted;
    execute : function(self: PCefv8Handler; const name: PCefString; object_: PCefv8Value; argumentsCount: NativeUInt; const arguments: PPCefV8Value; var retval: PCefV8Value; exception: PCefString): Integer; stdcall;
  end;

  /// <summary>
  /// Structure representing a V8 exception. The functions of this structure may
  /// be called on any render process thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefV8Exception.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_v8_capi.h">CEF source file: /include/capi/cef_v8_capi.h (cef_v8exception_t)</see></para>
  /// </remarks>
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

  /// <summary>
  /// Callback structure that is passed to ICefv8value.CreateArrayBuffer.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefv8ArrayBufferReleaseCallback.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_v8_capi.h">CEF source file: /include/capi/cef_v8_capi.h (cef_v8array_buffer_release_callback_t)</see></para>
  /// </remarks>
  TCefv8ArrayBufferReleaseCallback = record
    base                      : TCefBaseRefCounted;
    release_buffer            : procedure(self: PCefv8ArrayBufferReleaseCallback; buffer : Pointer); stdcall;
  end;

  /// <summary>
  /// Structure representing a V8 value handle. V8 handles can only be accessed
  /// from the thread on which they are created. Valid threads for creating a V8
  /// handle include the render process main thread (TID_RENDERER) and WebWorker
  /// threads. A task runner for posting tasks on the associated thread can be
  /// retrieved via the ICefv8context.GetTaskRunner() function.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefv8Value.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_v8_capi.h">CEF source file: /include/capi/cef_v8_capi.h (cef_v8value_t)</see></para>
  /// </remarks>
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
    is_promise                          : function(self: PCefv8Value): Integer; stdcall;
    is_same                             : function(self, that: PCefv8Value): Integer; stdcall;
    get_bool_value                      : function(self: PCefv8Value): Integer; stdcall;
    get_int_value                       : function(self: PCefv8Value): Integer; stdcall;
    get_uint_value                      : function(self: PCefv8Value): Cardinal; stdcall;
    get_double_value                    : function(self: PCefv8Value): Double; stdcall;
    get_date_value                      : function(self: PCefv8Value): TCefBaseTime; stdcall;
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
    set_value_byaccessor                : function(self: PCefv8Value; const key: PCefString; attribute: TCefV8PropertyAttributes): Integer; stdcall;
    get_keys                            : function(self: PCefv8Value; keys: TCefStringList): Integer; stdcall;
    set_user_data                       : function(self: PCefv8Value; user_data: PCefBaseRefCounted): Integer; stdcall;
    get_user_data                       : function(self: PCefv8Value): PCefBaseRefCounted; stdcall;
    get_externally_allocated_memory     : function(self: PCefv8Value): Integer; stdcall;
    adjust_externally_allocated_memory  : function(self: PCefv8Value; change_in_bytes: Integer): Integer; stdcall;
    get_array_length                    : function(self: PCefv8Value): Integer; stdcall;
    get_array_buffer_release_callback   : function(self: PCefv8Value): PCefv8ArrayBufferReleaseCallback; stdcall;
    neuter_array_buffer                 : function(self: PCefv8Value): Integer; stdcall;
    get_array_buffer_byte_length        : function(self: PCefv8Value): NativeUInt; stdcall;
    get_array_buffer_data               : function(self: PCefv8Value): Pointer; stdcall;
    get_function_name                   : function(self: PCefv8Value): PCefStringUserFree; stdcall;
    get_function_handler                : function(self: PCefv8Value): PCefv8Handler; stdcall;
    execute_function                    : function(self: PCefv8Value; obj: PCefv8Value; argumentsCount: NativeUInt; const arguments: PPCefV8Value): PCefv8Value; stdcall;
    execute_function_with_context       : function(self: PCefv8Value; context: PCefv8Context; obj: PCefv8Value; argumentsCount: NativeUInt; const arguments: PPCefV8Value): PCefv8Value; stdcall;
    resolve_promise                     : function(self, arg: PCefv8Value): Integer; stdcall;
    reject_promise                      : function(self: PCefv8Value; const errorMsg: PCefString): Integer; stdcall;
  end;

  /// <summary>
  /// Structure representing a V8 context handle. V8 handles can only be accessed
  /// from the thread on which they are created. Valid threads for creating a V8
  /// handle include the render process main thread (TID_RENDERER) and WebWorker
  /// threads. A task runner for posting tasks on the associated thread can be
  /// retrieved via the ICefv8context.GetTaskRunner() function.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefv8Context.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_v8_capi.h">CEF source file: /include/capi/cef_v8_capi.h (cef_v8context_t)</see></para>
  /// </remarks>
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

  /// <summary>
  /// Structure that should be implemented to handle V8 interceptor calls. The
  /// functions of this structure will be called on the thread associated with the
  /// V8 interceptor. Interceptor's named property handlers (with first argument
  /// of type CefString) are called when object is indexed by string. Indexed
  /// property handlers (with first argument of type int) are called when object
  /// is indexed by integer.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefV8Interceptor.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_v8_capi.h">CEF source file: /include/capi/cef_v8_capi.h (cef_v8interceptor_t)</see></para>
  /// </remarks>
  TCefV8Interceptor = record
    base        : TCefBaseRefCounted;
    get_byname  : function(self: PCefV8Interceptor; const name: PCefString; object_: PCefV8Value; out retval: PCefv8Value; exception: PCefString): integer; stdcall;
    get_byindex : function(self: PCefV8Interceptor; index: integer; object_: PCefV8Value; out retval: PCefv8Value; exception: PCefString): integer; stdcall;
    set_byname  : function(self: PCefV8Interceptor; const name: PCefString; object_, value: PCefv8Value; exception: PCefString): integer; stdcall;
    set_byindex : function(self: PCefV8Interceptor; index: integer; object_, value: PCefv8Value; exception: PCefString): integer; stdcall;
  end;

  /// <summary>
  /// Structure that should be implemented to handle V8 accessor calls. Accessor
  /// identifiers are registered by calling ICefv8value.SetValue(). The
  /// functions of this structure will be called on the thread associated with the
  /// V8 accessor.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefV8Accessor.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_v8_capi.h">CEF source file: /include/capi/cef_v8_capi.h (cef_v8accessor_t)</see></para>
  /// </remarks>
  TCefV8Accessor = record
    base : TCefBaseRefCounted;
    get  : function(self: PCefV8Accessor; const name: PCefString; object_: PCefv8Value; out retval: PCefv8Value; exception: PCefString): Integer; stdcall;
    set_ : function(self: PCefV8Accessor; const name: PCefString; object_, value: PCefv8Value; exception: PCefString): Integer; stdcall;
  end;

  /// <summary>
  /// Structure used to represent a frame in the browser window. When used in the
  /// browser process the functions of this structure may be called on any thread
  /// unless otherwise indicated in the comments. When used in the render process
  /// the functions of this structure may only be called on the main thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefFrame.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_frame_capi.h">CEF source file: /include/capi/cef_frame_capi.h (cef_frame_t)</see></para>
  /// </remarks>
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
    get_identifier       : function(self: PCefFrame): PCefStringUserFree; stdcall;
    get_parent           : function(self: PCefFrame): PCefFrame; stdcall;
    get_url              : function(self: PCefFrame): PCefStringUserFree; stdcall;
    get_browser          : function(self: PCefFrame): PCefBrowser; stdcall;
    get_v8context        : function(self: PCefFrame): PCefv8Context; stdcall;
    visit_dom            : procedure(self: PCefFrame; visitor: PCefDomVisitor); stdcall;
    create_urlrequest    : function(self: PCefFrame; request: PCefRequest; client: PCefUrlrequestClient): PCefUrlRequest; stdcall;
    send_process_message : procedure(self: PCefFrame; target_process: TCefProcessId; message_: PCefProcessMessage); stdcall;
  end;

  /// <summary>
  /// Implement this STRUCTURE to handle events related to ICefFrame life span.
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
  /// <para>Implemented by ICefFrameHandler.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_frame_handler_capi.h">CEF source file: /include/capi/cef_frame_handler_capi.h (cef_frame_handler_t)</see></para>
  /// </remarks>
  TCefFrameHandler = record
    base                  : TCefBaseRefCounted;
    on_frame_created      : procedure(self: PCefFrameHandler; browser: PCefBrowser; frame: PCefFrame); stdcall;
    on_frame_attached     : procedure(self: PCefFrameHandler; browser: PCefBrowser; frame: PCefFrame; reattached: integer); stdcall;
    on_frame_detached     : procedure(self: PCefFrameHandler; browser: PCefBrowser; frame: PCefFrame); stdcall;
    on_main_frame_changed : procedure(self: PCefFrameHandler; browser: PCefBrowser; old_frame, new_frame: PCefFrame); stdcall;
  end;

  /// <summary>
  /// Implement this structure to receive accessibility notification when
  /// accessibility events have been registered. The functions of this structure
  /// will be called on the UI thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefAccessibilityHandler.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_accessibility_handler_capi.h">CEF source file: /include/capi/cef_accessibility_handler_capi.h (cef_accessibility_handler_t)</see></para>
  /// </remarks>
  TCefAccessibilityHandler = record
    base                             : TCefBaseRefCounted;
    on_accessibility_tree_change     : procedure(self: PCefAccessibilityHandler; value: PCefValue); stdcall;
    on_accessibility_location_change : procedure(self: PCefAccessibilityHandler; value: PCefValue); stdcall;
  end;

  /// <summary>
  /// Implement this structure to handle context menu events. The functions of
  /// this structure will be called on the UI thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefContextMenuHandler.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_context_menu_handler_capi.h">CEF source file: /include/capi/cef_context_menu_handler_capi.h (cef_context_menu_handler_t)</see></para>
  /// </remarks>
  TCefContextMenuHandler = record
    base                      : TCefBaseRefCounted;
    on_before_context_menu    : procedure(self: PCefContextMenuHandler; browser: PCefBrowser; frame: PCefFrame; params: PCefContextMenuParams; model: PCefMenuModel); stdcall;
    run_context_menu          : function(self: PCefContextMenuHandler; browser: PCefBrowser; frame: PCefFrame; params: PCefContextMenuParams; model: PCefMenuModel; callback: PCefRunContextMenuCallback): Integer; stdcall;
    on_context_menu_command   : function(self: PCefContextMenuHandler; browser: PCefBrowser; frame: PCefFrame; params: PCefContextMenuParams; command_id: Integer; event_flags: TCefEventFlags): Integer; stdcall;
    on_context_menu_dismissed : procedure(self: PCefContextMenuHandler; browser: PCefBrowser; frame: PCefFrame); stdcall;
    run_quick_menu            : function(self: PCefContextMenuHandler; browser: PCefBrowser; frame: PCefFrame; location: PCefPoint; size: PCefSize; edit_state_flags: TCefQuickMenuEditStateFlags; callback: PCefRunQuickMenuCallback): integer; stdcall;
    on_quick_menu_command     : function(self: PCefContextMenuHandler; browser: PCefBrowser; frame: PCefFrame; command_id: integer; event_flags: TCefEventFlags): integer; stdcall;
    on_quick_menu_dismissed   : procedure(self: PCefContextMenuHandler; browser: PCefBrowser; frame: PCefFrame); stdcall;
  end;

  /// <summary>
  /// Callback structure used for continuation of custom quick menu display.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefRunQuickMenuCallback.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_context_menu_handler_capi.h">CEF source file: /include/capi/cef_context_menu_handler_capi.h (cef_run_quick_menu_callback_t)</see></para>
  /// </remarks>
  TCefRunQuickMenuCallback = record
    base   : TCefBaseRefCounted;
    cont   : procedure(self: PCefRunQuickMenuCallback; command_id: integer; event_flags: TCefEventFlags); stdcall;
    cancel : procedure(self: PCefRunQuickMenuCallback); stdcall;
  end;

  /// <summary>
  /// Implement this structure to provide handler implementations.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefClient.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_client_capi.h">CEF source file: /include/capi/cef_client_capi.h (cef_client_t)</see></para>
  /// </remarks>
  TCefClient = record
    base                        : TCefBaseRefCounted;
    get_audio_handler           : function(self: PCefClient): PCefAudioHandler; stdcall;
    get_command_handler         : function(self: PCefClient): PCefCommandHandler; stdcall;
    get_context_menu_handler    : function(self: PCefClient): PCefContextMenuHandler; stdcall;
    get_dialog_handler          : function(self: PCefClient): PCefDialogHandler; stdcall;
    get_display_handler         : function(self: PCefClient): PCefDisplayHandler; stdcall;
    get_download_handler        : function(self: PCefClient): PCefDownloadHandler; stdcall;
    get_drag_handler            : function(self: PCefClient): PCefDragHandler; stdcall;
    get_find_handler            : function(self: PCefClient): PCefFindHandler; stdcall;
    get_focus_handler           : function(self: PCefClient): PCefFocusHandler; stdcall;
    get_frame_handler           : function(self: PCefClient): PCefFrameHandler; stdcall;
    get_permission_handler      : function(self: PCefClient): PCefPermissionHandler; stdcall;
    get_jsdialog_handler        : function(self: PCefClient): PCefJsDialogHandler; stdcall;
    get_keyboard_handler        : function(self: PCefClient): PCefKeyboardHandler; stdcall;
    get_life_span_handler       : function(self: PCefClient): PCefLifeSpanHandler; stdcall;
    get_load_handler            : function(self: PCefClient): PCefLoadHandler; stdcall;
    get_print_handler           : function(self: PCefClient): PCefPrintHandler; stdcall;
    get_render_handler          : function(self: PCefClient): PCefRenderHandler; stdcall;
    get_request_handler         : function(self: PCefClient): PCefRequestHandler; stdcall;
    on_process_message_received : function(self: PCefClient; browser: PCefBrowser; frame: PCefFrame; source_process: TCefProcessId; message_: PCefProcessMessage): Integer; stdcall;
  end;

  /// <summary>
  /// Structure used to represent the browser process aspects of a browser. The
  /// functions of this structure can only be called in the browser process. They
  /// may be called on any thread in that process unless otherwise indicated in
  /// the comments.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefBrowserHost.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_browser_capi.h">CEF source file: /include/capi/cef_browser_capi.h (cef_browser_host_t)</see></para>
  /// </remarks>
  TCefBrowserHost = record
    base                              : TCefBaseRefCounted;
    get_browser                       : function(self: PCefBrowserHost): PCefBrowser; stdcall;
    close_browser                     : procedure(self: PCefBrowserHost; force_close: Integer); stdcall;
    try_close_browser                 : function(self: PCefBrowserHost): Integer; stdcall;
    is_ready_to_be_closed             : function(self: PCefBrowserHost): Integer; stdcall;
    set_focus                         : procedure(self: PCefBrowserHost; focus: Integer); stdcall;
    get_window_handle                 : function(self: PCefBrowserHost): TCefWindowHandle; stdcall;
    get_opener_window_handle          : function(self: PCefBrowserHost): TCefWindowHandle; stdcall;
    has_view                          : function(self: PCefBrowserHost): Integer; stdcall;
    get_client                        : function(self: PCefBrowserHost): PCefClient; stdcall;
    get_request_context               : function(self: PCefBrowserHost): PCefRequestContext; stdcall;
    can_zoom                          : function(self: PCefBrowserHost; command: TCefZoomCommand): Integer; stdcall;
    zoom                              : procedure(self: PCefBrowserHost; command: TCefZoomCommand); stdcall;
    get_default_zoom_level            : function(self: PCefBrowserHost): Double; stdcall;
    get_zoom_level                    : function(self: PCefBrowserHost): Double; stdcall;
    set_zoom_level                    : procedure(self: PCefBrowserHost; zoomLevel: Double); stdcall;
    run_file_dialog                   : procedure(self: PCefBrowserHost; mode: TCefFileDialogMode; const title, default_file_path: PCefString; accept_filters: TCefStringList; callback: PCefRunFileDialogCallback); stdcall;
    start_download                    : procedure(self: PCefBrowserHost; const url: PCefString); stdcall;
    download_image                    : procedure(self: PCefBrowserHost; const image_url: PCefString; is_favicon: Integer; max_image_size: Cardinal; bypass_cache: Integer; callback: PCefDownloadImageCallback); stdcall;
    print                             : procedure(self: PCefBrowserHost); stdcall;
    print_to_pdf                      : procedure(self: PCefBrowserHost; const path: PCefString; const settings: PCefPdfPrintSettings; callback: PCefPdfPrintCallback); stdcall;
    find                              : procedure(self: PCefBrowserHost; const searchText: PCefString; forward_, matchCase, findNext: Integer); stdcall;
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
    set_audio_muted                   : procedure(self: PCefBrowserHost; mute: integer); stdcall;
    is_audio_muted                    : function(self: PCefBrowserHost): integer; stdcall;
    is_fullscreen                     : function(self: PCefBrowserHost): integer; stdcall;
    exit_fullscreen                   : procedure(self: PCefBrowserHost; will_cause_resize: integer); stdcall;
    can_execute_chrome_command        : function(self: PCefBrowserHost; command_id: integer): integer; stdcall;
    execute_chrome_command            : procedure(self: PCefBrowserHost; command_id: integer; disposition: TCefWindowOpenDisposition); stdcall;
    is_render_process_unresponsive    : function(self: PCefBrowserHost): integer; stdcall;
    get_runtime_style                 : function(self: PCefBrowserHost): TCefRuntimeStyle; stdcall;
  end;

  /// <summary>
  /// Structure used to represent a browser. When used in the browser process the
  /// functions of this structure may be called on any thread unless otherwise
  /// indicated in the comments. When used in the render process the functions of
  /// this structure may only be called on the main thread.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefBrowser.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_browser_capi.h">CEF source file: /include/capi/cef_browser_capi.h (cef_browser_t)</see></para>
  /// </remarks>
  TCefBrowser = record
    base                      : TCefBaseRefCounted;
    is_valid                  : function(self: PCefBrowser): Integer; stdcall;
    get_host                  : function(self: PCefBrowser): PCefBrowserHost; stdcall;
    can_go_back               : function(self: PCefBrowser): Integer; stdcall;
    go_back                   : procedure(self: PCefBrowser); stdcall;
    can_go_forward            : function(self: PCefBrowser): Integer; stdcall;
    go_forward                : procedure(self: PCefBrowser); stdcall;
    is_loading                : function(self: PCefBrowser): Integer; stdcall;
    reload                    : procedure(self: PCefBrowser); stdcall;
    reload_ignore_cache       : procedure(self: PCefBrowser); stdcall;
    stop_load                 : procedure(self: PCefBrowser); stdcall;
    get_identifier            : function(self: PCefBrowser): Integer; stdcall;
    is_same                   : function(self, that: PCefBrowser): Integer; stdcall;
    is_popup                  : function(self: PCefBrowser): Integer; stdcall;
    has_document              : function(self: PCefBrowser): Integer; stdcall;
    get_main_frame            : function(self: PCefBrowser): PCefFrame; stdcall;
    get_focused_frame         : function(self: PCefBrowser): PCefFrame; stdcall;
    get_frame_by_identifier   : function(self: PCefBrowser; const identifier: PCefString): PCefFrame; stdcall;
    get_frame_by_name         : function(self: PCefBrowser; const name: PCefString): PCefFrame; stdcall;
    get_frame_count           : function(self: PCefBrowser): NativeUInt; stdcall;
    get_frame_identifiers     : procedure(self: PCefBrowser; identifiers: TCefStringList); stdcall;
    get_frame_names           : procedure(self: PCefBrowser; names: TCefStringList); stdcall;
  end;

  /// <summary>
  /// Structure used to implement a custom resource bundle structure. See
  /// TCefSettings for additional options related to resource bundle loading. The
  /// functions of this structure may be called on multiple threads.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefResourceBundleHandler.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_resource_bundle_handler_capi.h">CEF source file: /include/capi/cef_resource_bundle_handler_capi.h (cef_resource_bundle_handler_t)</see></para>
  /// </remarks>
  TCefResourceBundleHandler = record
    base                        : TCefBaseRefCounted;
    get_localized_string        : function(self: PCefResourceBundleHandler; string_id: Integer; string_val: PCefString): Integer; stdcall;
    get_data_resource           : function(self: PCefResourceBundleHandler; resource_id: Integer; var data: Pointer; var data_size: NativeUInt): Integer; stdcall;
    get_data_resource_for_scale : function(self: PCefResourceBundleHandler; resource_id: Integer; scale_factor: TCefScaleFactor; var data: Pointer; var data_size: NativeUInt): Integer; stdcall;
  end;

  /// <summary>
  /// Structure used to implement browser process callbacks. The functions of this
  /// structure will be called on the browser process main thread unless otherwise
  /// indicated.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefBrowserProcessHandler.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_browser_process_handler_capi.h">CEF source file: /include/capi/cef_browser_process_handler_capi.h (cef_browser_process_handler_t)</see></para>
  /// </remarks>
  TCefBrowserProcessHandler = record
    base                                : TCefBaseRefCounted;
    on_register_custom_preferences      : procedure(self: PCefBrowserProcessHandler; type_: TCefPreferencesType; registrar: PCefPreferenceRegistrar); stdcall;
    on_context_initialized              : procedure(self: PCefBrowserProcessHandler); stdcall;
    on_before_child_process_launch      : procedure(self: PCefBrowserProcessHandler; command_line: PCefCommandLine); stdcall;
    on_already_running_app_relaunch     : function(self: PCefBrowserProcessHandler; command_line: PCefCommandLine; const current_directory: PCefString): integer; stdcall;
    on_schedule_message_pump_work       : procedure(self: PCefBrowserProcessHandler; delay_ms: Int64); stdcall;
    get_default_client                  : function(self: PCefBrowserProcessHandler): PCefClient; stdcall;
    get_default_request_context_handler : function(self: PCefBrowserProcessHandler): PCefRequestContextHandler; stdcall;
  end;

  /// <summary>
  /// Implement this structure to provide handler implementations. Methods will be
  /// called by the process and/or thread indicated.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefApp.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_app_capi.h">CEF source file: /include/capi/cef_app_capi.h (cef_app_t)</see></para>
  /// </remarks>
  TCefApp = record
    base                              : TCefBaseRefCounted;
    on_before_command_line_processing : procedure(self: PCefApp; const process_type: PCefString; command_line: PCefCommandLine); stdcall;
    on_register_custom_schemes        : procedure(self: PCefApp; registrar: PCefSchemeRegistrar); stdcall;
    get_resource_bundle_handler       : function(self: PCefApp): PCefResourceBundleHandler; stdcall;
    get_browser_process_handler       : function(self: PCefApp): PCefBrowserProcessHandler; stdcall;
    get_render_process_handler        : function(self: PCefApp): PCefRenderProcessHandler; stdcall;
  end;

  /// <summary>
  /// Structure representing a server that supports HTTP and WebSocket requests.
  /// Server capacity is limited and is intended to handle only a small number of
  /// simultaneous connections (e.g. for communicating between applications on
  /// localhost). The functions of this structure are safe to call from any thread
  /// in the brower process unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefServer.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_server_capi.h">CEF source file: /include/capi/cef_server_capi.h (cef_server_t)</see></para>
  /// </remarks>
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

  /// <summary>
  /// Implement this structure to handle HTTP server requests. A new thread will
  /// be created for each ICefServer.CreateServer call (the "dedicated server
  /// thread"), and the functions of this structure will be called on that thread.
  /// It is therefore recommended to use a different ICefServerHandler instance
  /// for each ICefServer.CreateServer call to avoid thread safety issues in
  /// the ICefServerHandler implementation.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefServerHandler.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_server_capi.h">CEF source file: /include/capi/cef_server_capi.h (cef_server_handler_t)</see></para>
  /// </remarks>
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


  {*
   *********************************
   ************* Views *************
   *********************************

    (*) Has CEF creation function
    (d) Has delegate

   ----------------          ----------------------
   | TCefView (d) | -------> | TCefTextfield (*d) |
   ----------------    |     ----------------------
                       |
                       |     ----------------------
                       |---> | TCefScrollView (*) |
                       |     ----------------------
                       |
                       |     ------------------          -------------------
                       |---> | TCefPanel (*d) | -------> | TCefWindow (*d) |
                       |     ------------------          -------------------
                       |
                       |     ------------------------
                       |---> | TCefBrowserView (*d) |
                       |     ------------------------
                       |
                       |     ------------------          -----------------------          -----------------------
                       |---> | TCefButton (d) | -------> | TCefLabelButton (*) | -------> | TCefMenuButton (*d) |
                             ------------------          -----------------------          -----------------------


   --------------          -----------------
   | TCefLayout | -------> | TCefBoxLayout |
   --------------    |     -----------------
                     |
                     |     ------------------
                     |---> | TCefFillLayout |
                           ------------------
  *}


  /// <summary>
  /// This structure typically, but not always, corresponds to a physical display
  /// connected to the system. A fake Display may exist on a headless system, or a
  /// Display may correspond to a remote, virtual display. All size and position
  /// values are in density independent pixel (DIP) coordinates unless otherwise
  /// indicated. Methods must be called on the browser process UI thread unless
  /// otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefDisplay.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_display_capi.h">CEF source file: /include/capi/views/cef_display_capi.h (cef_display_t)</see></para>
  /// </remarks>
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

  /// <summary>
  /// A Layout handles the sizing of the children of a Panel according to
  /// implementation-specific heuristics. Methods must be called on the browser
  /// process UI thread unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefLayout.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_layout_capi.h">CEF source file: /include/capi/views/cef_layout_capi.h (cef_layout_t)</see></para>
  /// </remarks>
  TCefLayout = record
    base                    : TCefBaseRefCounted;
    as_box_layout           : function(self: PCefLayout): PCefBoxLayout; stdcall;
    as_fill_layout          : function(self: PCefLayout): PCefFillLayout; stdcall;
    is_valid                : function(self: PCefLayout): Integer; stdcall;
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
  /// <para>Implemented by ICefBoxLayout.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_box_layout_capi.h">CEF source file: /include/capi/views/cef_box_layout_capi.h (cef_box_layout_t)</see></para>
  /// </remarks>
  TCefBoxLayout = record
    base                    : TCefLayout;
    set_flex_for_view       : procedure(self: PCefBoxLayout; view: PCefView; flex: Integer); stdcall;
    clear_flex_for_view     : procedure(self: PCefBoxLayout; view: PCefView); stdcall;
  end;

  /// <summary>
  /// A simple Layout that causes the associated Panel's one child to be sized to
  /// match the bounds of its parent. Methods must be called on the browser
  /// process UI thread unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefFillLayout.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_fill_layout_capi.h">CEF source file: /include/capi/views/cef_fill_layout_capi.h (cef_fill_layout_t)</see></para>
  /// </remarks>
  TCefFillLayout = record
    base                    : TCefLayout;
  end;

  /// <summary>
  /// Controller for an overlay that contains a contents View added via
  /// ICefWindow.AddOverlayView. Methods exposed by this controller should be
  /// called in preference to functions of the same name exposed by the contents
  /// View unless otherwise indicated. Methods must be called on the browser
  /// process UI thread unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefOverlayController.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_overlay_controller_capi.h">CEF source file: /include/capi/views/cef_overlay_controller_capi.h (cef_overlay_controller_t)</see></para>
  /// </remarks>
  TCefOverlayController = record
    base                      : TCefBaseRefCounted;
    is_valid                  : function(self: PCefOverlayController): integer; stdcall;
    is_same                   : function(self, that: PCefOverlayController): integer; stdcall;
    get_contents_view         : function(self: PCefOverlayController): PCefView; stdcall;
    get_window                : function(self: PCefOverlayController): PCefWindow; stdcall;
    get_docking_mode          : function(self: PCefOverlayController): TCefDockingMode; stdcall;
    destroy                   : procedure(self: PCefOverlayController); stdcall;
    set_bounds                : procedure(self: PCefOverlayController; const bounds: PCefRect); stdcall;
    get_bounds                : function(self: PCefOverlayController): TCefRect; stdcall;
    get_bounds_in_screen      : function(self: PCefOverlayController): TCefRect; stdcall;
    set_size                  : procedure(self: PCefOverlayController; const size: PCefSize); stdcall;
    get_size                  : function(self: PCefOverlayController): TCefSize; stdcall;
    set_position              : procedure(self: PCefOverlayController; const position: PCefPoint); stdcall;
    get_position              : function(self: PCefOverlayController): TCefPoint; stdcall;
    set_insets                : procedure(self: PCefOverlayController; const insets: PCefInsets); stdcall;
    get_insets                : function(self: PCefOverlayController): TCefInsets; stdcall;
    size_to_preferred_size    : procedure(self: PCefOverlayController); stdcall;
    set_visible               : procedure(self: PCefOverlayController; visible: integer); stdcall;
    is_visible                : function(self: PCefOverlayController): integer; stdcall;
    is_drawn                  : function(self: PCefOverlayController): integer; stdcall;
  end;

  /// <summary>
  /// A View is a rectangle within the views View hierarchy. It is the base
  /// structure for all Views. All size and position values are in density
  /// independent pixels (DIP) unless otherwise indicated. Methods must be called
  /// on the browser process UI thread unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefView.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_view_capi.h">CEF source file: /include/capi/views/cef_view_capi.h (cef_view_t)</see></para>
  /// </remarks>
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
    set_insets                  : procedure(self: PCefView; const insets: PCefInsets); stdcall;
    get_insets                  : function(self: PCefView): TCefInsets; stdcall;
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
    get_theme_color             : function(self: PCefView; color_id: integer): TCefColor; stdcall;
    convert_point_to_screen     : function(self: PCefView; point: PCefPoint): Integer; stdcall;
    convert_point_from_screen   : function(self: PCefView; point: PCefPoint): Integer; stdcall;
    convert_point_to_window     : function(self: PCefView; point: PCefPoint): Integer; stdcall;
    convert_point_from_window   : function(self: PCefView; point: PCefPoint): Integer; stdcall;
    convert_point_to_view       : function(self, view: PCefView; point: PCefPoint): Integer; stdcall;
    convert_point_from_view     : function(self, view: PCefView; point: PCefPoint): Integer; stdcall;
  end;

  /// <summary>
  /// Implement this structure to handle view events. All size and position values
  /// are in density independent pixels (DIP) unless otherwise indicated. The
  /// functions of this structure will be called on the browser process UI thread
  /// unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefViewDelegate.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_view_delegate_capi.h">CEF source file: /include/capi/views/cef_view_delegate_capi.h (cef_view_delegate_t)</see></para>
  /// </remarks>
  TCefViewDelegate = record
    base                        : TCefBaseRefCounted;
    get_preferred_size          : function(self: PCefViewDelegate; view: PCefView): TCefSize; stdcall;
    get_minimum_size            : function(self: PCefViewDelegate; view: PCefView): TCefSize; stdcall;
    get_maximum_size            : function(self: PCefViewDelegate; view: PCefView): TCefSize; stdcall;
    get_height_for_width        : function(self: PCefViewDelegate; view: PCefView; width: Integer): Integer; stdcall;
    on_parent_view_changed      : procedure(self: PCefViewDelegate; view: PCefView; added: Integer; parent: PCefView); stdcall;
    on_child_view_changed       : procedure(self: PCefViewDelegate; view: PCefView; added: Integer; child: PCefView); stdcall;
    on_window_changed           : procedure(self: PCefViewDelegate; view: PCefView; added: Integer); stdcall;
    on_layout_changed           : procedure(self: PCefViewDelegate; view: PCefView; const new_bounds: PCefRect); stdcall;
    on_focus                    : procedure(self: PCefViewDelegate; view: PCefView); stdcall;
    on_blur                     : procedure(self: PCefViewDelegate; view: PCefView); stdcall;
    on_theme_changed            : procedure(self: PCefViewDelegate; view: PCefView); stdcall;
  end;

  /// <summary>
  /// A Textfield supports editing of text. This control is custom rendered with
  /// no platform-specific code. Methods must be called on the browser process UI
  /// thread unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefTextfield.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_textfield_capi.h">CEF source file: /include/capi/views/cef_textfield_capi.h (cef_textfield_t)</see></para>
  /// </remarks>
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

  /// <summary>
  /// Implement this structure to handle Textfield events. The functions of this
  /// structure will be called on the browser process UI thread unless otherwise
  /// indicated.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefTextfieldDelegate.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_textfield_delegate_capi.h">CEF source file: /include/capi/views/cef_textfield_delegate_capi.h (cef_textfield_delegate_t)</see></para>
  /// </remarks>
  TCefTextfieldDelegate = record
    base                           : TCefViewDelegate;
    on_key_event                   : function(self: PCefTextfieldDelegate; textfield: PCefTextfield; const event: PCefKeyEvent): Integer; stdcall;
    on_after_user_action           : procedure(self: PCefTextfieldDelegate; textfield: PCefTextfield); stdcall;
  end;

  /// <summary>
  /// A ScrollView will show horizontal and/or vertical scrollbars when necessary
  /// based on the size of the attached content view. Methods must be called on
  /// the browser process UI thread unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefScrollView.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_scroll_view_capi.h">CEF source file: /include/capi/views/cef_scroll_view_capi.h (cef_scroll_view_t)</see></para>
  /// </remarks>
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

  /// <summary>
  /// A Panel is a container in the views hierarchy that can contain other Views
  /// as children. Methods must be called on the browser process UI thread unless
  /// otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefPanel.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_panel_capi.h">CEF source file: /include/capi/views/cef_panel_capi.h (cef_panel_t)</see></para>
  /// </remarks>
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

  /// <summary>
  /// Implement this structure to handle Panel events. The functions of this
  /// structure will be called on the browser process UI thread unless otherwise
  /// indicated.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefPanelDelegate.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_panel_delegate_capi.h">CEF source file: /include/capi/views/cef_panel_delegate_capi.h (cef_panel_delegate_t)</see></para>
  /// </remarks>
  TCefPanelDelegate = record
    base                            : TCefViewDelegate;
  end;

  /// <summary>
  /// A View hosting a ICefBrowser instance. Methods must be called on the
  /// browser process UI thread unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefBrowserView.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_browser_view_capi.h">CEF source file: /include/capi/views/cef_browser_view_capi.h (cef_browser_view_t)</see></para>
  /// </remarks>
  TCefBrowserView = record
    base                            : TCefView;
    get_browser                     : function(self: PCefBrowserView): PCefBrowser; stdcall;
    get_chrome_toolbar              : function(self: PCefBrowserView): PCefView; stdcall;
    set_prefer_accelerators         : procedure(self: PCefBrowserView; prefer_accelerators: Integer); stdcall;
    get_runtime_style               : function(self: PCefBrowserView): TCefRuntimeStyle; stdcall;
  end;

  /// <summary>
  /// Implement this structure to handle BrowserView events. The functions of this
  /// structure will be called on the browser process UI thread unless otherwise
  /// indicated.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefBrowserViewDelegate.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_browser_view_delegate_capi.h">CEF source file: /include/capi/views/cef_browser_view_delegate_capi.h (cef_browser_view_delegate_t)</see></para>
  /// </remarks>
  TCefBrowserViewDelegate = record
    base                                        : TCefViewDelegate;
    on_browser_created                          : procedure(self: PCefBrowserViewDelegate; browser_view: PCefBrowserView; browser: PCefBrowser); stdcall;
    on_browser_destroyed                        : procedure(self: PCefBrowserViewDelegate; browser_view: PCefBrowserView; browser: PCefBrowser); stdcall;
    get_delegate_for_popup_browser_view         : function(self: PCefBrowserViewDelegate; browser_view: PCefBrowserView; const settings: PCefBrowserSettings; client: PCefClient; is_devtools: Integer): PCefBrowserViewDelegate; stdcall;
    on_popup_browser_view_created               : function(self: PCefBrowserViewDelegate; browser_view, popup_browser_view: PCefBrowserView; is_devtools: Integer): Integer; stdcall;
    get_chrome_toolbar_type                     : function(self: PCefBrowserViewDelegate; browser_view: PCefBrowserView): TCefChromeToolbarType; stdcall;
    use_frameless_window_for_picture_in_picture : function(self: PCefBrowserViewDelegate; browser_view: PCefBrowserView): integer; stdcall;
    on_gesture_command                          : function(self: PCefBrowserViewDelegate; browser_view: PCefBrowserView; gesture_command: TCefGestureCommand): Integer; stdcall;
    get_browser_runtime_style                   : function(self: PCefBrowserViewDelegate): TCefRuntimeStyle; stdcall;
  end;

  /// <summary>
  /// A View representing a button. Depending on the specific type, the button
  /// could be implemented by a native control or custom rendered. Methods must be
  /// called on the browser process UI thread unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefButton.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_button_capi.h">CEF source file: /include/capi/views/cef_button_capi.h (cef_button_t)</see></para>
  /// </remarks>
  TCefButton = record
    base                            : TCefView;
    as_label_button                 : function(self: PCefButton): PCefLabelButton; stdcall;
    set_state                       : procedure(self: PCefButton; state: TCefButtonState); stdcall;
    get_state                       : function(self: PCefButton): TCefButtonState; stdcall;
    set_ink_drop_enabled            : procedure(self: PCefButton; enabled: Integer); stdcall;
    set_tooltip_text                : procedure(self: PCefButton; const tooltip_text: PCefString); stdcall;
    set_accessible_name             : procedure(self: PCefButton; const name: PCefString); stdcall;
  end;

  /// <summary>
  /// Implement this structure to handle Button events. The functions of this
  /// structure will be called on the browser process UI thread unless otherwise
  /// indicated.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefButtonDelegate.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_button_delegate_capi.h">CEF source file: /include/capi/views/cef_button_delegate_capi.h (cef_button_delegate_t)</see></para>
  /// </remarks>
  TCefButtonDelegate = record
    base                            : TCefViewDelegate;
    on_button_pressed               : procedure(self: PCefButtonDelegate; button: PCefButton); stdcall;
    on_button_state_changed         : procedure(self: PCefButtonDelegate; button: PCefButton); stdcall;
  end;

  /// <summary>
  /// LabelButton is a button with optional text and/or icon. Methods must be
  /// called on the browser process UI thread unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefLabelButton.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_label_button_capi.h">CEF source file: /include/capi/views/cef_label_button_capi.h (cef_label_button_t)</see></para>
  /// </remarks>
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

  /// <summary>
  /// MenuButton is a button with optional text, icon and/or menu marker that
  /// shows a menu when clicked with the left mouse button. All size and position
  /// values are in density independent pixels (DIP) unless otherwise indicated.
  /// Methods must be called on the browser process UI thread unless otherwise
  /// indicated.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefMenuButton.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_menu_button_capi.h">CEF source file: /include/capi/views/cef_menu_button_capi.h (cef_menu_button_t)</see></para>
  /// </remarks>
  TCefMenuButton = record
    base                            : TCefLabelButton;
    show_menu                       : procedure(self: PCefMenuButton; menu_model: PCefMenuModel; const screen_point: PCefPoint; anchor_position: TCefMenuAnchorPosition); stdcall;
    trigger_menu                    : procedure(self: PCefMenuButton); stdcall;
  end;

  /// <summary>
  /// MenuButton pressed lock is released when this object is destroyed.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefMenuButtonPressedLock.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_menu_button_delegate_capi.h">CEF source file: /include/capi/views/cef_menu_button_delegate_capi.h (cef_menu_button_pressed_lock_t)</see></para>
  /// </remarks>
  TCefMenuButtonPressedLock = record
    base                    : TCefBaseRefCounted;
  end;

  /// <summary>
  /// Implement this structure to handle MenuButton events. The functions of this
  /// structure will be called on the browser process UI thread unless otherwise
  /// indicated.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefMenuButtonDelegate.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_menu_button_delegate_capi.h">CEF source file: /include/capi/views/cef_menu_button_delegate_capi.h (cef_menu_button_delegate_t)</see></para>
  /// </remarks>
  TCefMenuButtonDelegate = record
    base                    : TCefButtonDelegate;
    on_menu_button_pressed  : procedure(self: PCefMenuButtonDelegate; menu_button: PCefMenuButton; const screen_point: PCefPoint; button_pressed_lock: PCefMenuButtonPressedLock); stdcall;
  end;

  /// <summary>
  /// A Window is a top-level Window/widget in the Views hierarchy. By default it
  /// will have a non-client area with title bar, icon and buttons that supports
  /// moving and resizing. All size and position values are in density independent
  /// pixels (DIP) unless otherwise indicated. Methods must be called on the
  /// browser process UI thread unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefWindow.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_window_capi.h">CEF source file: /include/capi/views/cef_window_capi.h (cef_window_t)</see></para>
  /// </remarks>
  TCefWindow = record
    base                             : TCefPanel;
    show                             : procedure(self: PCefWindow); stdcall;
    show_as_browser_modal_dialog     : procedure(self: PCefWindow; browser_view: PCefBrowserView); stdcall;
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
    add_overlay_view                 : function(self: PCefWindow; view: PCefView; docking_mode: TCefDockingMode; can_activate: integer): PCefOverlayController; stdcall;
    show_menu                        : procedure(self: PCefWindow; menu_model: PCefMenuModel; const screen_point: PCefPoint; anchor_position : TCefMenuAnchorPosition); stdcall;
    cancel_menu                      : procedure(self: PCefWindow); stdcall;
    get_display                      : function(self: PCefWindow): PCefDisplay; stdcall;
    get_client_area_bounds_in_screen : function(self: PCefWindow): TCefRect; stdcall;
    set_draggable_regions            : procedure(self: PCefWindow; regionsCount: NativeUInt; const regions: PCefDraggableRegionArray); stdcall;
    get_window_handle                : function(self: PCefWindow): TCefWindowHandle; stdcall;
    send_key_press                   : procedure(self: PCefWindow; key_code: Integer; event_flags: cardinal); stdcall;
    send_mouse_move                  : procedure(self: PCefWindow; screen_x, screen_y: Integer); stdcall;
    send_mouse_events                : procedure(self: PCefWindow; button: TCefMouseButtonType; mouse_down, mouse_up: Integer); stdcall;
    set_accelerator                  : procedure(self: PCefWindow; command_id, key_code, shift_pressed, ctrl_pressed, alt_pressed, high_priority: Integer); stdcall;
    remove_accelerator               : procedure(self: PCefWindow; command_id: Integer); stdcall;
    remove_all_accelerators          : procedure(self: PCefWindow); stdcall;
    set_theme_color                  : procedure(self: PCefWindow; color_id: integer; color: TCefColor); stdcall;
    theme_changed                    : procedure(self: PCefWindow); stdcall;
    get_runtime_style                : function(self: PCefWindow): TCefRuntimeStyle; stdcall;
  end;

  /// <summary>
  /// Implement this structure to handle window events. The functions of this
  /// structure will be called on the browser process UI thread unless otherwise
  /// indicated.
  /// </summary>
  /// <remarks>
  /// <para>Implemented by ICefWindowDelegate.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_window_delegate_capi.h">CEF source file: /include/capi/views/cef_window_delegate_capi.h (cef_window_delegate_t)</see></para>
  /// </remarks>
  TCefWindowDelegate = record
    base                             : TCefPanelDelegate;
    on_window_created                : procedure(self: PCefWindowDelegate; window: PCefWindow); stdcall;
    on_window_closing                : procedure(self: PCefWindowDelegate; window: PCefWindow); stdcall;
    on_window_destroyed              : procedure(self: PCefWindowDelegate; window: PCefWindow); stdcall;
    on_window_activation_changed     : procedure(self: PCefWindowDelegate; window: PCefWindow; active: integer); stdcall;
    on_window_bounds_changed         : procedure(self: PCefWindowDelegate; window: PCefWindow; const new_bounds: PCefRect); stdcall;
    on_window_fullscreen_transition  : procedure(self: PCefWindowDelegate; window: PCefWindow; is_completed: integer); stdcall;
    get_parent_window                : function(self: PCefWindowDelegate; window: PCefWindow; is_menu, can_activate_menu: PInteger): PCefWindow; stdcall;
    is_window_modal_dialog           : function(self: PCefWindowDelegate; window: PCefWindow): Integer; stdcall;
    get_initial_bounds               : function(self: PCefWindowDelegate; window: PCefWindow): TCefRect; stdcall;
    get_initial_show_state           : function(self: PCefWindowDelegate; window: PCefWindow): TCefShowState; stdcall;
    is_frameless                     : function(self: PCefWindowDelegate; window: PCefWindow): Integer; stdcall;
    with_standard_window_buttons     : function(self: PCefWindowDelegate; window: PCefWindow): Integer; stdcall;
    get_titlebar_height              : function(self: PCefWindowDelegate; window: PCefWindow; titlebar_height: System.PSingle): Integer; stdcall;
    accepts_first_mouse              : function(self: PCefWindowDelegate; window: PCefWindow): TCefState; stdcall;
    can_resize                       : function(self: PCefWindowDelegate; window: PCefWindow): Integer; stdcall;
    can_maximize                     : function(self: PCefWindowDelegate; window: PCefWindow): Integer; stdcall;
    can_minimize                     : function(self: PCefWindowDelegate; window: PCefWindow): Integer; stdcall;
    can_close                        : function(self: PCefWindowDelegate; window: PCefWindow): Integer; stdcall;
    on_accelerator                   : function(self: PCefWindowDelegate; window: PCefWindow; command_id: Integer): Integer; stdcall;
    on_key_event                     : function(self: PCefWindowDelegate; window: PCefWindow; const event: PCefKeyEvent): Integer; stdcall;
    on_theme_colors_changed          : procedure(self: PCefWindowDelegate; window: PCefWindow; chrome_theme: Integer); stdcall;
    get_window_runtime_style         : function(self: PCefWindowDelegate): TCefRuntimeStyle; stdcall;
    get_linux_window_properties      : function(self: PCefWindowDelegate; window: PCefWindow; properties: PCefLinuxWindowProperties): Integer; stdcall;
  end;

implementation

end.


