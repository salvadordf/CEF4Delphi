unit uCEFApplicationCore;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$IFNDEF FPC}{$IFNDEF DELPHI12_UP}
  // Workaround for "Internal error" in old Delphi versions caused by uint64 handling
  {$R-}
{$ENDIF}{$ENDIF}

interface

uses
  {$IFDEF DELPHI16_UP}
    {$IFDEF MSWINDOWS}WinApi.Windows,{$ENDIF} System.Classes, System.UITypes, System.SyncObjs,
    {$IFDEF FMX}uCEFLinuxTypes,{$ENDIF}
  {$ELSE}
    {$IFDEF MSWINDOWS}Windows,{$ENDIF} Classes, {$IFDEF FPC}dynlibs,{$ENDIF} SyncObjs,
  {$ENDIF}
  {$IFDEF LINUX}
    {$IFDEF FPC}xlib,{$ENDIF} uCEFArgCopy,
  {$ENDIF}
  uCEFTypes, uCEFInterfaces, uCEFApplicationEvents, uCEFBaseRefCounted,
  uCEFSchemeRegistrar, uCEFPreferenceRegistrar, uCEFComponentIdList;

const
  {$I uCEFVersion.inc}

  {$IFDEF MSWINDOWS}
  LIBCEF_DLL     = 'libcef.dll';
  CHROMEELF_DLL  = 'chrome_elf.dll';
  {$ENDIF}

  {$IFDEF MACOSX}
  LIBCEF_DLL     = 'Chromium Embedded Framework';
  LIBCEF_PREFIX  = 'Contents/Frameworks/Chromium Embedded Framework.framework/';
  CHROMEELF_DLL  = '';
  {$ENDIF}

  {$IFDEF LINUX}
  LIBCEF_DLL     = 'libcef.so';
  CHROMEELF_DLL  = '';
  {$ENDIF}

  {$IFDEF ANDROID}
  LIBCEF_DLL     = '';
  CHROMEELF_DLL  = '';
  {$ENDIF}

  // for InitLibLocationFromArgs
  LIBCEF_PAK         = 'cef.pak';
  LIBCEF_LOCALE_DIR  = 'locales';
  LIBCEF_LOCALE_ENUS = 'en-US.pak';

type
  /// <summary>
  ///  Parent class of TCefApplication used to simplify the CEF initialization and destruction.
  /// </summary>
  TCefApplicationCore = class(TInterfacedObject, IApplicationCoreEvents)
    protected
      // Fields used to populate TCefSettings
      FNoSandbox                         : boolean;
      FBrowserSubprocessPath             : ustring;
      FFrameworkDirPath                  : ustring;
      FMainBundlePath                    : ustring; // Only used in macOS
      FMultiThreadedMessageLoop          : boolean;
      FExternalMessagePump               : boolean;
      FWindowlessRenderingEnabled        : boolean;
      FCommandLineArgsDisabled           : boolean;
      FCache                             : ustring;
      FRootCache                         : ustring;
      FPersistSessionCookies             : boolean;
      FUserAgent                         : ustring;
      FUserAgentProduct                  : ustring;
      FLocale                            : ustring;
      FLogFile                           : ustring;
      FLogSeverity                       : TCefLogSeverity;
      FLogItems                          : TCefLogItems;
      FJavaScriptFlags                   : ustring;
      FResourcesDirPath                  : ustring;
      FLocalesDirPath                    : ustring;
      FRemoteDebuggingPort               : integer;
      FUncaughtExceptionStackSize        : integer;
      FIgnoreCertificateErrors           : boolean;
      FBackgroundColor                   : TCefColor;
      FAcceptLanguageList                : ustring;
      FCookieableSchemesList             : ustring;
      FCookieableSchemesExcludeDefaults  : boolean;
      FChromePolicyId                    : ustring;
      FChromeAppIconId                   : integer;
      {$IF DEFINED(OS_POSIX) AND NOT(DEFINED(ANDROID))}
      FDisableSignalHandlers             : boolean;
      {$IFEND}

      // Fields used to set command line switches
      FSingleProcess                     : boolean;
      FEnableMediaStream                 : boolean;
      FEnableSpeechInput                 : boolean;
      FUseFakeUIForMediaStream           : boolean;
      FEnableUsermediaScreenCapturing    : boolean;
      FEnableGPU                         : boolean;
      FEnableFeatures                    : ustring;
      FDisableFeatures                   : ustring;
      FEnableBlinkFeatures               : ustring;
      FDisableBlinkFeatures              : ustring;
      FBlinkSettings                     : ustring;
      FForceFieldTrials                  : ustring;
      FForceFieldTrialParams             : ustring;
      FSmoothScrolling                   : TCefState;
      FMuteAudio                         : boolean;
      FSitePerProcess                    : boolean;
      FDisableWebSecurity                : boolean;
      FDisablePDFExtension               : boolean;
      FDisableSiteIsolationTrials        : boolean;
      FDisableChromeLoginPrompt          : boolean;
      FDisableExtensions                 : boolean;
      FAutoplayPolicy                    : TCefAutoplayPolicy;
      FDisableBackgroundNetworking       : boolean;
      FMetricsRecordingOnly              : boolean;
      FAllowFileAccessFromFiles          : boolean;
      FAllowRunningInsecureContent       : boolean;
      FEnablePrintPreview                : boolean;
      FDefaultEncoding                   : ustring;
      FDisableJavascript                 : boolean;
      FDisableJavascriptCloseWindows     : boolean;
      FDisableJavascriptAccessClipboard  : boolean;
      FDisableJavascriptDomPaste         : boolean;
      FAllowUniversalAccessFromFileUrls  : boolean;
      FDisableImageLoading               : boolean;
      FImageShrinkStandaloneToFit        : boolean;
      FDisableTextAreaResize             : boolean;
      FDisableTabToLinks                 : boolean;
      FEnableProfanityFilter             : boolean;
      FDisableSpellChecking              : boolean;
      FOverrideSpellCheckLang            : ustring;
      FTouchEvents                       : TCefState;
      FDisableReadingFromCanvas          : boolean;
      FHyperlinkAuditing                 : boolean;
      FDisableNewBrowserInfoTimeout      : boolean;
      FDevToolsProtocolLogFile           : ustring;
      FForcedDeviceScaleFactor           : single;
      FDisableZygote                     : boolean; // Only used in Linux
      FUseMockKeyChain                   : boolean; // Only used in macOS
      FDisableRequestHandlingForTesting  : boolean;
      FDisablePopupBlocking              : boolean;
      FDisableBackForwardCache           : boolean;
      FDisableComponentUpdate            : boolean;
      FAllowInsecureLocalhost            : boolean;
      FKioskPrinting                     : boolean;
      FTreatInsecureOriginAsSecure       : ustring;
      FNetLogEnabled                     : boolean;
      FNetLogFile                        : ustring;
      FNetLogCaptureMode                 : TCefNetLogCaptureMode;
      FRemoteAllowOrigins                : ustring;
      FAutoAcceptCamAndMicCapture        : boolean;
      FUIColorMode                       : TCefUIColorMode;
      FDisableHangMonitor                : boolean;
      FHideCrashRestoreBubble            : boolean;
      FPostQuantumKyber                  : TCefState;


      // Fields used during the CEF initialization
      FWindowsSandboxInfo                : pointer;
      {$IFDEF LINUX}
      FArgCopy                           : TCEFArgCopy;
      {$ENDIF}

      // Fields used by custom properties
      FDeleteCache                       : boolean;
      FDeleteCookies                     : boolean;
      FCheckCEFFiles                     : boolean;
      FShowMessageDlg                    : boolean;
      FMissingBinariesException          : boolean;
      FSetCurrentDir                     : boolean;
      FGlobalContextInitialized          : boolean;
      FChromeVersionInfo                 : TFileVersionInfo;
      FLibLoaded                         : boolean;
      FLogProcessInfo                    : boolean;
      FReRaiseExceptions                 : boolean;
      FDeviceScaleFactor                 : single;
      FLocalesRequired                   : ustring;
      FProcessType                       : TCefProcessType;
      FMustCreateResourceBundleHandler   : boolean;
      FMustCreateBrowserProcessHandler   : boolean;
      FMustCreateRenderProcessHandler    : boolean;
      FMustCreateLoadHandler             : boolean;
      FStatus                            : TCefAplicationStatus;
      FMissingLibFiles                   : string;
      FMustFreeLibrary                   : boolean;
      FLastErrorMessage                  : ustring;

      // Internal fields
      FLibHandle                         : {$IFDEF FPC}TLibHandle{$ELSE}THandle{$ENDIF};
      FCustomCommandLines                : TStringList;
      FCustomCommandLineValues           : TStringList;
      FAppSettings                       : TCefSettings;
      FDisableGPUCache                   : boolean;
      FComponentIDList                   : TCEFComponentIdList;

      // ICefApp
      FOnRegisterCustomSchemes           : TOnRegisterCustomSchemesEvent;

      // ICefBrowserProcessHandler
      FOnRegisterCustomPreferences       : TOnRegisterCustomPreferencesEvent;
      FOnContextInitialized              : TOnContextInitializedEvent;
      FOnBeforeChildProcessLaunch        : TOnBeforeChildProcessLaunchEvent;
      FOnAlreadyRunningAppRelaunch       : TOnAlreadyRunningAppRelaunchEvent;
      FOnScheduleMessagePumpWork         : TOnScheduleMessagePumpWorkEvent;
      FOnGetDefaultClient                : TOnGetDefaultClientEvent;
      FOnGetDefaultRequestContextHandler : TOnGetDefaultRequestContextHandlerEvent;

      // ICefResourceBundleHandler
      FOnGetLocalizedString              : TOnGetLocalizedStringEvent;
      FOnGetDataResource                 : TOnGetDataResourceEvent;
      FOnGetDataResourceForScale         : TOnGetDataResourceForScaleEvent;

      // ICefRenderProcessHandler
      FOnWebKitInitialized               : TOnWebKitInitializedEvent;
      FOnBrowserCreated                  : TOnBrowserCreatedEvent;
      FOnBrowserDestroyed                : TOnBrowserDestroyedEvent;
      FOnContextCreated                  : TOnContextCreatedEvent;
      FOnContextReleased                 : TOnContextReleasedEvent;
      FOnUncaughtException               : TOnUncaughtExceptionEvent;
      FOnFocusedNodeChanged              : TOnFocusedNodeChangedEvent;
      FOnProcessMessageReceived          : TOnProcessMessageReceivedEvent;

      // ICefLoadHandler
      FOnLoadingStateChange              : TOnRenderLoadingStateChange;
      FOnLoadStart                       : TOnRenderLoadStart;
      FOnLoadEnd                         : TOnRenderLoadEnd;
      FOnLoadError                       : TOnRenderLoadError;

      procedure SetCache(const aValue : ustring);
      procedure SetRootCache(const aValue : ustring);
      procedure SetBrowserSubprocessPath(const aValue : ustring);
      procedure SetFrameworkDirPath(const aValue : ustring);
      procedure SetResourcesDirPath(const aValue : ustring);
      procedure SetLocalesDirPath(const aValue : ustring);
      {$IFDEF MSWINDOWS}
      procedure SetOsmodalLoop(aValue : boolean);
      {$ENDIF}
      procedure SetKioskPrinting(aValue : boolean);

      function  GetChromeVersion : ustring;
      function  GetLibCefVersion : ustring;
      function  GetLibCefPath : ustring;
      function  GetChromeElfPath : ustring;
      function  GetLocalesDirPath: ustring;
      function  GetResourcesDirPath: ustring;
      function  GetMustCreateResourceBundleHandler : boolean; virtual;
      function  GetMustCreateBrowserProcessHandler : boolean; virtual;
      function  GetMustCreateRenderProcessHandler : boolean; virtual;
      function  GetMustCreateLoadHandler : boolean; virtual;
      function  GetGlobalContextInitialized : boolean;
      function  GetChildProcessesCount : integer;
      function  GetUsedMemory : uint64;
      function  GetTotalSystemMemory : uint64;
      function  GetAvailableSystemMemory : uint64;
      function  GetSystemMemoryLoad : cardinal;
      function  GetApiHashUniversal : ustring;
      function  GetApiHashPlatform : ustring;
      function  GetApiHashCommit : ustring;
      function  GetExitCode : TCefResultCode;
      {$IFDEF LINUX}
      function  GetXDisplay : PXDisplay;
      function  GetArgc : longint;
      function  GetArgv : PPAnsiChar;
      {$ENDIF}

      function  LoadCEFlibrary : boolean; virtual;
      function  Load_cef_api_hash_h : boolean;
      function  Load_cef_app_capi_h : boolean;
      function  Load_cef_app_win_h : boolean;
      function  Load_cef_browser_capi_h : boolean;
      function  Load_cef_command_line_capi_h : boolean;
      function  Load_cef_cookie_capi_h : boolean;
      function  Load_cef_crash_util_h : boolean;
      function  Load_cef_drag_data_capi_h : boolean;
      function  Load_cef_dump_without_crashing_internal_h : boolean;
      function  Load_cef_file_util_capi_h : boolean;
      function  Load_cef_i18n_util_capi_h : boolean;
      function  Load_cef_image_capi_h : boolean;
      function  Load_cef_menu_model_capi_h : boolean;
      function  Load_cef_media_router_capi_h : boolean;
      function  Load_cef_origin_whitelist_capi_h : boolean;
      function  Load_cef_parser_capi_h : boolean;
      function  Load_cef_path_util_capi_h : boolean;
      function  Load_cef_preference_capi_h : boolean;
      function  Load_cef_print_settings_capi_h : boolean;
      function  Load_cef_process_message_capi_h : boolean;
      function  Load_cef_process_util_capi_h : boolean;
      function  Load_cef_request_capi_h : boolean;
      function  Load_cef_request_context_capi_h : boolean;
      function  Load_cef_resource_bundle_capi_h : boolean;
      function  Load_cef_response_capi_h : boolean;
      function  Load_cef_scheme_capi_h : boolean;
      function  Load_cef_server_capi_h : boolean;
      function  Load_cef_shared_process_message_builder_capi_h : boolean;
      function  Load_cef_ssl_info_capi_h : boolean;
      function  Load_cef_stream_capi_h : boolean;
      function  Load_cef_task_capi_h : boolean;
      function  Load_cef_task_manager_capi_h : boolean;
      function  Load_cef_thread_capi_h : boolean;
      function  Load_cef_trace_capi_h : boolean;
      function  Load_cef_urlrequest_capi_h : boolean;
      function  Load_cef_v8_capi_h : boolean;
      function  Load_cef_values_capi_h : boolean;
      function  Load_cef_waitable_event_capi_h : boolean;
      function  Load_cef_xml_reader_capi_h : boolean;
      function  Load_cef_zip_reader_capi_h : boolean;
      function  Load_cef_logging_internal_h : boolean;
      function  Load_cef_string_list_h : boolean;
      function  Load_cef_string_map_h : boolean;
      function  Load_cef_string_multimap_h : boolean;
      function  Load_cef_string_types_h : boolean;
      function  Load_cef_thread_internal_h : boolean;
      function  Load_cef_trace_event_internal_h : boolean;
      function  Load_cef_browser_view_capi_h : boolean;
      function  Load_cef_display_capi_h : boolean;
      function  Load_cef_label_button_capi_h : boolean;
      function  Load_cef_menu_button_capi_h : boolean;
      function  Load_cef_panel_capi_h : boolean;
      function  Load_cef_scroll_view_capi_h : boolean;
      function  Load_cef_textfield_capi_h : boolean;
      function  Load_cef_window_capi_h : boolean;
      function  Load_cef_types_linux_h : boolean;
      function  Load_cef_time_h : boolean;

      // ICefApp
      procedure doOnBeforeCommandLineProcessing(const processType: ustring; const commandLine: ICefCommandLine); virtual;
      procedure doOnRegisterCustomSchemes(const registrar: TCefSchemeRegistrarRef); virtual;

      // ICefBrowserProcessHandler
      procedure doOnRegisterCustomPreferences(type_: TCefPreferencesType; registrar: PCefPreferenceRegistrar); virtual;
      procedure doOnContextInitialized; virtual;
      procedure doOnBeforeChildProcessLaunch(const commandLine: ICefCommandLine); virtual;
      procedure doOnAlreadyRunningAppRelaunch(const commandLine: ICefCommandLine; const current_directory: ustring; var aResult: boolean); virtual;
      procedure doOnScheduleMessagePumpWork(const delayMs: Int64); virtual;
      procedure doGetDefaultClient(var aClient : ICefClient); virtual;
      procedure doGetDefaultRequestContextHandler(var aRequestContextHandler : ICefRequestContextHandler); virtual;

      // ICefResourceBundleHandler
      function  doGetLocalizedString(stringid: Integer; var stringVal: ustring): Boolean; virtual;
      function  doGetDataResource(resourceId: Integer; var data: Pointer; var dataSize: NativeUInt): Boolean; virtual;
      function  doGetDataResourceForScale(resourceId: Integer; scaleFactor: TCefScaleFactor; var data: Pointer; var dataSize: NativeUInt): Boolean; virtual;

      // ICefRenderProcessHandler
      procedure doOnWebKitInitialized; virtual;
      procedure doOnBrowserCreated(const browser: ICefBrowser; const extra_info: ICefDictionaryValue); virtual;
      procedure doOnBrowserDestroyed(const browser: ICefBrowser); virtual;
      procedure doOnContextCreated(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context); virtual;
      procedure doOnContextReleased(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context); virtual;
      procedure doOnUncaughtException(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context; const V8Exception: ICefV8Exception; const stackTrace: ICefV8StackTrace); virtual;
      procedure doOnFocusedNodeChanged(const browser: ICefBrowser; const frame: ICefFrame; const node: ICefDomNode); virtual;
      procedure doOnProcessMessageReceived(const browser: ICefBrowser; const frame: ICefFrame; sourceProcess: TCefProcessId; const aMessage: ICefProcessMessage; var aHandled : boolean); virtual;

      // ICefLoadHandler
      procedure doOnLoadingStateChange(const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean); virtual;
      procedure doOnLoadStart(const browser: ICefBrowser; const frame: ICefFrame; transitionType: TCefTransitionType); virtual;
      procedure doOnLoadEnd(const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: Integer); virtual;
      procedure doOnLoadError(const browser: ICefBrowser; const frame: ICefFrame; errorCode: TCefErrorCode; const errorText, failedUrl: ustring); virtual;

      procedure ShutDown;
      procedure FreeLibcefLibrary;
      function  ExecuteProcess(const aApp : ICefApp) : integer;
      procedure InitializeCefMainArgs(var aCefMainArgs : TCefMainArgs);
      procedure InitializeSettings(var aSettings : TCefSettings);
      function  InitializeLibrary(const aApp : ICefApp) : boolean;
      procedure RenameAndDeleteDir(const aDirectory : string; aKeepCookies : boolean = False);
      procedure DeleteCacheContents(const aDirectory : string);
      procedure DeleteCookiesDB(const aDirectory : string);
      procedure MoveCookiesDB(const aSrcDirectory, aDstDirectory : string);
      function  MultiExeProcessing : boolean;
      function  SingleExeProcessing : boolean;
      procedure BeforeInitSubProcess; virtual;
      function  CheckCEFResources : boolean; virtual;
      {$IFDEF MSWINDOWS}
      function  CheckCEFDLL : boolean; virtual;
      function  CheckWindowsVersion: boolean; virtual;
      {$ENDIF}
      {$IFDEF MACOSX}
      function  CheckMacOSVersion : boolean; virtual;
      {$ENDIF}
      function  CheckOSVersion: boolean; virtual;
      procedure ShowErrorMessageDlg(const aError : string); virtual;
      function  ParseProcessType : TCefProcessType;
      procedure AddCustomCommandLineSwitches(var aKeys, aValues : TStringList); virtual;
      procedure AppendSwitch(var aKeys, aValues : TStringList; const aNewKey : ustring; const aNewValue : ustring = '');
      procedure ReplaceSwitch(var aKeys, aValues : TStringList; const aNewKey : ustring; const aNewValue : ustring = '');
      procedure CleanupFeatures(var aKeys, aValues : TStringList; const aEnableKey, aDisableKey : string);
      procedure ClearSchemeHandlerFactories;

    public
      constructor Create;
      destructor  Destroy; override;
      procedure   AfterConstruction; override;
      /// <summary>
      /// Used to add any command line switch that is not available as a
      /// TCEFApplicationCore property.
      /// </summary>
      procedure   AddCustomCommandLine(const aCommandLine : string; const aValue : string = '');
      /// <summary>
      /// Used to check the CEF binaries manually.
      /// </summary>
      function    CheckCEFLibrary : boolean;
      /// <summary>
      /// Used to initialize CEF in the main browser process. In case CEF is
      /// configured to used the same executable for all processes then all
      /// processes must call this function. CEF can only be initialized once
      /// per process. This is a CEF feature and there's no workaround. This
      /// function returns immediately in when called in the main process and
      /// it blocks the execution when it's called from a CEF subprocess until
      /// that process ends.
      /// </summary>
      function    StartMainProcess : boolean;
      /// <summary>
      /// Used to initialize CEF in the subprocesses. This function can only be
      /// used when CEF is configured to use a different executable for the
      /// subprocesses. This function blocks the execution until the process ends.
      /// </summary>
      function    StartSubProcess : boolean;
      /// <summary>
      /// Perform a single iteration of CEF message loop processing. This function is
      /// provided for cases where the CEF message loop must be integrated into an
      /// existing application message loop. Use of this function is not recommended
      /// for most users; use either the RunMessageLoop function or
      /// TCefSettings.multi_threaded_message_loop if possible. When using this
      /// function care must be taken to balance performance against excessive CPU
      /// usage. It is recommended to enable the TCefSettings.external_message_pump
      /// option when using this function so that
      /// ICefBrowserProcessHandler.OnScheduleMessagePumpWork callbacks can
      /// facilitate the scheduling process. This function should only be called on
      /// the main application thread and only if cef_initialize() is called with a
      /// TCefSettings.multi_threaded_message_loop value of false (0). This function
      /// will not block.
      /// </summary>
      procedure   DoMessageLoopWork;
      /// <summary>
      /// Run the CEF message loop. Use this function instead of an application-
      /// provided message loop to get the best balance between performance and CPU
      /// usage. This function should only be called on the main application thread
      /// and only if cef_initialize() is called with a
      /// TCefSettings.multi_threaded_message_loop value of false (0). This function
      /// will block until a quit message is received by the system.
      /// </summary>
      procedure   RunMessageLoop;
      /// <summary>
      /// Quit the CEF message loop that was started by calling
      /// RunMessageLoop. This function should only be called on the main
      /// application thread and only if RunMessageLoop was used.
      /// </summary>
      procedure   QuitMessageLoop;
      /// <summary>
      /// Update the DeviceScaleFactor value with the current monitor scale.
      /// </summary>
      procedure   UpdateDeviceScaleFactor; virtual;
      {$IFDEF MACOSX}
      /// <summary>
      /// This procedure is only available in MacOS to read some configuration
      /// settings from the command line arguments.
      /// </summary>
      procedure   InitLibLocationFromArgs;
      {$ENDIF}
      /// <summary>
      /// Returns true if a custom component ID is valid before executing a CEF task.
      /// </summary>
      function    ValidComponentID(aComponentID : integer) : boolean;
      /// <summary>
      /// Returns the next component ID and adds this value to the valid ID list.
      /// </summary>
      function    NextComponentID : integer;
      /// <summary>
      /// Removes a component ID from the valid ID list when a component is destroyed.
      /// </summary>
      procedure   RemoveComponentID(aComponentID : integer);
      /// <summary>
      /// DumpWithoutCrashing allows for generating crash dumps with a throttling
      /// mechanism, preventing frequent dumps from being generated in a short period
      /// of time from the same location. The |function_name|, |file_name|, and
      /// |line_number| determine the location of the dump. The
      /// |mseconds_between_dumps| is an interval between consecutive dumps in
      /// milliseconds from the same location.
      /// </summary>
      /// <returns>
      /// Returns true if the dump was successfully generated, false otherwise
      /// </returns>
      /// <remarks>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/base/cef_dump_without_crashing.h">CEF source file: /include/base/cef_dump_without_crashing.h (CefDumpWithoutCrashing)</see></para>
      /// </remarks>
      function    DumpWithoutCrashing(mseconds_between_dumps: int64; const function_name, file_name: ustring; line_number: integer): boolean;
      /// <summary>
      /// DumpWithoutCrashingUnthrottled allows for immediate crash dumping without
      /// any throttling constraints.
      /// </summary>
      /// <returns>
      /// Returns true if the dump was successfully generated, false otherwise
      /// </returns>
      /// <remarks>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/base/cef_dump_without_crashing.h">CEF source file: /include/base/cef_dump_without_crashing.h (CefDumpWithoutCrashingUnthrottled)</see></para>
      /// </remarks>
      function    DumpWithoutCrashingUnthrottled : boolean;

      /// <summary>
      /// Set to true (1) to disable the sandbox for sub-processes. See
      /// cef_sandbox_win.h for requirements to enable the sandbox on Windows. Also
      /// configurable using the "no-sandbox" command-line switch.
      /// </summary>
      property NoSandbox                         : Boolean                                  read FNoSandbox                         write FNoSandbox;
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
      property BrowserSubprocessPath             : ustring                                  read FBrowserSubprocessPath             write SetBrowserSubprocessPath;
      /// <summary>
      /// The path to the CEF framework directory on macOS. If this value is empty
      /// then the framework must exist at "Contents/Frameworks/Chromium Embedded
      /// Framework.framework" in the top-level app bundle. If this value is
      /// non-empty then it must be an absolute path. Also configurable using the
      /// "framework-dir-path" command-line switch.
      /// </summary>
      property FrameworkDirPath                  : ustring                                  read FFrameworkDirPath                  write SetFrameworkDirPath;
      /// <summary>
      /// The path to the main bundle on macOS. If this value is empty then it
      /// defaults to the top-level app bundle. If this value is non-empty then it
      /// must be an absolute path. Also configurable using the "main-bundle-path"
      /// command-line switch.
      /// </summary>
      property MainBundlePath                    : ustring                                  read FMainBundlePath                    write FMainBundlePath;
      /// <summary>
      /// Set to true (1) to have the browser process message loop run in a separate
      /// thread. If false (0) then the CefDoMessageLoopWork() function must be
      /// called from your application message loop. This option is only supported
      /// on Windows and Linux.
      /// </summary>
      property MultiThreadedMessageLoop          : boolean                                  read FMultiThreadedMessageLoop          write FMultiThreadedMessageLoop;
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
      property ExternalMessagePump               : boolean                                  read FExternalMessagePump               write FExternalMessagePump;
      /// <summary>
      /// Set to true (1) to enable windowless (off-screen) rendering support. Do
      /// not enable this value if the application does not use windowless rendering
      /// as it may reduce rendering performance on some systems.
      /// </summary>
      property WindowlessRenderingEnabled        : Boolean                                  read FWindowlessRenderingEnabled        write FWindowlessRenderingEnabled;
      /// <summary>
      /// Set to true (1) to disable configuration of browser process features using
      /// standard CEF and Chromium command-line arguments. Configuration can still
      /// be specified using CEF data structures or via the
      /// ICefApp.OnBeforeCommandLineProcessing() method.
      /// </summary>
      property CommandLineArgsDisabled           : Boolean                                  read FCommandLineArgsDisabled           write FCommandLineArgsDisabled;
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
      property Cache                             : ustring                                  read FCache                             write SetCache;
      /// <summary>
      /// <para>The root directory for installation-specific data and the parent directory
      /// for profile-specific data. All TCefSettings.cache_path and
      /// ICefRequestContextSettings.cache_path values must have this parent
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
      property RootCache                         : ustring                                  read FRootCache                         write SetRootCache;
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
      property PersistSessionCookies             : Boolean                                  read FPersistSessionCookies             write FPersistSessionCookies;
      /// <summary>
      /// Value that will be returned as the User-Agent HTTP header. If empty the
      /// default User-Agent string will be used. Also configurable using the
      /// "user-agent" command-line switch.
      /// </summary>
      property UserAgent                         : ustring                                  read FUserAgent                         write FUserAgent;
      /// <summary>
      /// Value that will be inserted as the product portion of the default
      /// User-Agent string. If empty the Chromium product version will be used. If
      /// |userAgent| is specified this value will be ignored. Also configurable
      /// using the "user-agent-product" command-line switch.
      /// </summary>
      property UserAgentProduct                  : ustring                                  read FUserAgentProduct                  write FUserAgentProduct;
      /// <summary>
      /// The locale string that will be passed to WebKit. If empty the default
      /// locale of "en-US" will be used. This value is ignored on Linux where
      /// locale is determined using environment variable parsing with the
      /// precedence order: LANGUAGE, LC_ALL, LC_MESSAGES and LANG. Also
      /// configurable using the "lang" command-line switch.
      /// </summary>
      property Locale                            : ustring                                  read FLocale                            write FLocale;
      /// <summary>
      /// The directory and file name to use for the debug log. If empty a default
      /// log file name and location will be used. On Windows and Linux a
      /// "debug.log" file will be written in the main executable directory. On
      /// MacOS a "~/Library/Logs/[app name]_debug.log" file will be written where
      /// [app name] is the name of the main app executable. Also configurable using
      /// the "log-file" command-line switch.
      /// </summary>
      property LogFile                           : ustring                                  read FLogFile                           write FLogFile;
      /// <summary>
      /// The log severity. Only messages of this severity level or higher will be
      /// logged. When set to DISABLE no messages will be written to the log file,
      /// but FATAL messages will still be output to stderr. Also configurable using
      /// the "log-severity" command-line switch with a value of "verbose", "info",
      /// "warning", "error", "fatal" or "disable".
      /// </summary>
      property LogSeverity                       : TCefLogSeverity                          read FLogSeverity                       write FLogSeverity;
      /// <summary>
      /// The log items prepended to each log line. If not set the default log items
      /// will be used. Also configurable using the "log-items" command-line switch
      /// with a value of "none" for no log items, or a comma-delimited list of
      /// values "pid", "tid", "timestamp" or "tickcount" for custom log items.
      /// </summary>
      property LogItems                          : TCefLogItems                             read FLogItems                          write FLogItems;
      /// <summary>
      /// Custom flags that will be used when initializing the V8 JavaScript engine.
      /// The consequences of using custom flags may not be well tested. Also
      /// configurable using the "js-flags" command-line switch.
      /// </summary>
      property JavaScriptFlags                   : ustring                                  read FJavaScriptFlags                   write FJavaScriptFlags;
      /// <summary>
      /// The fully qualified path for the resources directory. If this value is
      /// empty the *.pak files must be located in the module directory on
      /// Windows/Linux or the app bundle Resources directory on MacOS. If this
      /// value is non-empty then it must be an absolute path. Also configurable
      /// using the "resources-dir-path" command-line switch.
      /// </summary>
      property ResourcesDirPath                  : ustring                                  read GetResourcesDirPath                write SetResourcesDirPath;
      /// <summary>
      /// The fully qualified path for the locales directory. If this value is empty
      /// the locales directory must be located in the module directory. If this
      /// value is non-empty then it must be an absolute path. This value is ignored
      /// on MacOS where pack files are always loaded from the app bundle Resources
      /// directory. Also configurable using the "locales-dir-path" command-line
      /// switch.
      /// </summary>
      property LocalesDirPath                    : ustring                                  read GetLocalesDirPath                  write SetLocalesDirPath;
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
      property RemoteDebuggingPort               : Integer                                  read FRemoteDebuggingPort               write FRemoteDebuggingPort;
      /// <summary>
      /// The number of stack trace frames to capture for uncaught exceptions.
      /// Specify a positive value to enable the
      /// ICefRenderProcessHandler.OnUncaughtException() callback. Specify 0
      /// (default value) and OnUncaughtException() will not be called. Also
      /// configurable using the "uncaught-exception-stack-size" command-line
      /// switch.
      /// </summary>
      property UncaughtExceptionStackSize        : Integer                                  read FUncaughtExceptionStackSize        write FUncaughtExceptionStackSize;
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
      property BackgroundColor                   : TCefColor                                read FBackgroundColor                   write FBackgroundColor;
      /// <summary>
      /// Comma delimited ordered list of language codes without any whitespace that
      /// will be used in the "Accept-Language" HTTP request header and
      /// "navigator.language" JS attribute. Can be overridden for individual
      /// ICefRequestContext instances via the
      /// TCefRequestContextSettingsCefRequestContextSettings.accept_language_list value.
      /// </summary>
      property AcceptLanguageList                : ustring                                  read FAcceptLanguageList                write FAcceptLanguageList;
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
      property CookieableSchemesList             : ustring                                  read FCookieableSchemesList             write FCookieableSchemesList;
      /// <summary>
      /// See the CookieableSchemesList property.
      /// </summary>
      property CookieableSchemesExcludeDefaults  : boolean                                  read FCookieableSchemesExcludeDefaults  write FCookieableSchemesExcludeDefaults;
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
      property ChromePolicyId                    : ustring                                  read FChromePolicyId                    write FChromePolicyId;
      /// <summary>
      /// Specify an ID for an ICON resource that can be loaded from the main
      /// executable and used when creating default Chrome windows such as DevTools
      /// and Task Manager. If unspecified the default Chromium ICON (IDR_MAINFRAME
      /// [101]) will be loaded from libcef.dll. Only supported with Chrome style on
      /// Windows.
      /// </summary>
      property ChromeAppIconId                   : integer                                  read FChromeAppIconId                   write FChromeAppIconId;
      {$IF DEFINED(OS_POSIX) AND NOT(DEFINED(ANDROID))}
      /// <summary>
      /// Specify whether signal handlers must be disabled on POSIX systems.
      /// </summary>
      property DisableSignalHandlers             : boolean                                  read FDisableSignalHandlers             write FDisableSignalHandlers;
      {$IFEND}
      /// <summary>
      /// Runs the renderer and plugins in the same process as the browser.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://peter.sh/experiments/chromium-command-line-switches/">Uses the following command line switch: --single-process</see></para>
      /// </remarks>
      property SingleProcess                     : Boolean                                  read FSingleProcess                     write FSingleProcess;
      /// <summary>
      /// Enable media (WebRTC audio/video) streaming.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/libcef/common/cef_switches.cc">Uses the following command line switch: --enable-media-stream</see></para>
      /// </remarks>
      property EnableMediaStream                 : boolean                                  read FEnableMediaStream                 write FEnableMediaStream;
      /// <summary>
      /// Enable speech input (x-webkit-speech).
      /// </summary>
      /// <remarks>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/libcef/common/cef_switches.cc">Uses the following command line switch: --enable-speech-input</see></para>
      /// </remarks>
      property EnableSpeechInput                 : boolean                                  read FEnableSpeechInput                 write FEnableSpeechInput;
      /// <summary>
      /// Bypass the media stream infobar by selecting the default device for media streams (e.g. WebRTC). Works with --use-fake-device-for-media-stream.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://peter.sh/experiments/chromium-command-line-switches/">Uses the following command line switch: --single-process</see></para>
      /// </remarks>
      property UseFakeUIForMediaStream           : boolean                                  read FUseFakeUIForMediaStream           write FUseFakeUIForMediaStream;
      /// <summary>
      /// Enable screen capturing support for MediaStream API.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://peter.sh/experiments/chromium-command-line-switches/">Uses the following command line switch: --enable-usermedia-screen-capturing</see></para>
      /// </remarks>
      property EnableUsermediaScreenCapturing    : boolean                                  read FEnableUsermediaScreenCapturing    write FEnableUsermediaScreenCapturing;
      /// <summary>
      /// Enable GPU hardware acceleration.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://peter.sh/experiments/chromium-command-line-switches/">Uses the following command line switch: --disable-gpu</see></para>
      /// <para><see href="https://peter.sh/experiments/chromium-command-line-switches/">Uses the following command line switch: --disable-gpu-compositing</see></para>
      /// </remarks>
      property EnableGPU                         : boolean                                  read FEnableGPU                         write FEnableGPU;
      /// <summary>
      /// List of feature names to enable.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://peter.sh/experiments/chromium-command-line-switches/">Uses the following command line switch: --enable-features</see></para>
      /// <para>The list of features you can enable is here:</para>
      /// <para>https://chromium.googlesource.com/chromium/src/+/master/chrome/common/chrome_features.cc</para>
      /// <para>https://source.chromium.org/chromium/chromium/src/+/main:content/public/common/content_features.cc</para>
      /// <para>https://source.chromium.org/search?q=base::Feature</para>
      /// </remarks>
      property EnableFeatures                    : ustring                                  read FEnableFeatures                    write FEnableFeatures;
      /// <summary>
      /// List of feature names to disable.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://peter.sh/experiments/chromium-command-line-switches/">Uses the following command line switch: --disable-features</see></para>
      /// <para>The list of features you can disable is here:</para>
      /// <para>https://chromium.googlesource.com/chromium/src/+/master/chrome/common/chrome_features.cc</para>
      /// <para>https://source.chromium.org/chromium/chromium/src/+/main:content/public/common/content_features.cc</para>
      /// <para>https://source.chromium.org/search?q=base::Feature</para>
      /// </remarks>
      property DisableFeatures                   : ustring                                  read FDisableFeatures                   write FDisableFeatures;
      /// <summary>
      /// Enable one or more Blink runtime-enabled features.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://peter.sh/experiments/chromium-command-line-switches/">Uses the following command line switch: --enable-blink-features</see></para>
      /// <para>The list of Blink features you can enable is here:</para>
      /// <para>https://cs.chromium.org/chromium/src/third_party/blink/renderer/platform/runtime_enabled_features.json5</para>
      /// </remarks>
      property EnableBlinkFeatures               : ustring                                  read FEnableBlinkFeatures               write FEnableBlinkFeatures;
      /// <summary>
      /// Disable one or more Blink runtime-enabled features.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://peter.sh/experiments/chromium-command-line-switches/">Uses the following command line switch: --disable-blink-features</see></para>
      /// <para>The list of Blink features you can disable is here:</para>
      /// <para>https://cs.chromium.org/chromium/src/third_party/blink/renderer/platform/runtime_enabled_features.json5</para>
      /// </remarks>
      property DisableBlinkFeatures              : ustring                                  read FDisableBlinkFeatures              write FDisableBlinkFeatures;
      /// <summary>
      /// Set blink settings. Format is <name>[=<value],<name>[=<value>],...
      /// The names are declared in Settings.json5. For boolean type, use "true", "false",
      /// or omit '=<value>' part to set to true. For enum type, use the int value of the
      /// enum value. Applied after other command line flags and prefs.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://peter.sh/experiments/chromium-command-line-switches/">Uses the following command line switch: --blink-settings</see></para>
      /// <para>The list of Blink settings you can disable is here:</para>
      /// <para>https://source.chromium.org/chromium/chromium/src/+/master:third_party/blink/renderer/core/frame/settings.json5</para>
      /// </remarks>
      property BlinkSettings                     : ustring                                  read FBlinkSettings                     write FBlinkSettings;
      /// <summary>
      /// This option can be used to force field trials when testing changes locally.
      /// The argument is a list of name and value pairs, separated by slashes.
      /// If a trial name is prefixed with an asterisk, that trial will start activated.
      /// For example, the following argument defines two trials, with the second one
      /// activated: "GoogleNow/Enable/*MaterialDesignNTP/Default/" This option can also
      /// be used by the browser process to send the list of trials to a non-browser
      /// process, using the same format. See FieldTrialList::CreateTrialsFromString()
      /// in field_trial.h for details.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://peter.sh/experiments/chromium-command-line-switches/">Uses the following command line switch: --force-fieldtrials</see></para>
      /// <para>https://source.chromium.org/chromium/chromium/src/+/master:base/base_switches.cc</para>
      /// </remarks>
      property ForceFieldTrials                  : ustring                                  read FForceFieldTrials                  write FForceFieldTrials;
      /// <summary>
      /// This option can be used to force parameters of field trials when testing
      /// changes locally. The argument is a param list of (key, value) pairs prefixed
      /// by an associated (trial, group) pair. You specify the param list for multiple
      /// (trial, group) pairs with a comma separator.
      /// Example: "Trial1.Group1:k1/v1/k2/v2,Trial2.Group2:k3/v3/k4/v4"
      /// Trial names, groups names, parameter names, and value should all be URL
      /// escaped for all non-alphanumeric characters.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://peter.sh/experiments/chromium-command-line-switches/">Uses the following command line switch: --force-fieldtrial-params</see></para>
      /// <para>https://source.chromium.org/chromium/chromium/src/+/master:components/variations/variations_switches.cc</para>
      /// </remarks>
      property ForceFieldTrialParams             : ustring                                  read FForceFieldTrialParams             write FForceFieldTrialParams;
      /// <summary>
      /// On platforms that support it, enables smooth scroll animation.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://peter.sh/experiments/chromium-command-line-switches/">Uses the following command line switch: --enable-smooth-scrolling</see></para>
      /// </remarks>
      property SmoothScrolling                   : TCefState                                read FSmoothScrolling                   write FSmoothScrolling;
      /// <summary>
      /// Mutes audio sent to the audio device so it is not audible during automated testing.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://peter.sh/experiments/chromium-command-line-switches/">Uses the following command line switch: --mute-audio</see></para>
      /// </remarks>
      property MuteAudio                         : boolean                                  read FMuteAudio                         write FMuteAudio;
      /// <summary>
      /// Enforces a one-site-per-process security policy: Each renderer process, for its
      /// whole lifetime, is dedicated to rendering pages for just one site. Thus, pages
      /// from different sites are never in the same process. A renderer process's access
      /// rights are restricted based on its site.All cross-site navigations force process
      /// swaps. <iframe>s are rendered out-of-process whenever the src= is cross-site.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://peter.sh/experiments/chromium-command-line-switches/">Uses the following command line switch: --site-per-process</see></para>
      /// <para>More details here:</para>
      /// <para>https://www.chromium.org/developers/design-documents/site-isolation</para>
      /// <para>https://www.chromium.org/developers/design-documents/process-models</para>
      /// </remarks>
      property SitePerProcess                    : boolean                                  read FSitePerProcess                    write FSitePerProcess;
      /// <summary>
      /// Don't enforce the same-origin policy.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://peter.sh/experiments/chromium-command-line-switches/">Uses the following command line switch: --disable-web-security</see></para>
      /// </remarks>
      property DisableWebSecurity                : boolean                                  read FDisableWebSecurity                write FDisableWebSecurity;
      /// <summary>
      /// Disable the PDF extension.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/libcef/common/cef_switches.cc">Uses the following command line switch: --disable-pdf-extension</see></para>
      /// </remarks>
      property DisablePDFExtension               : boolean                                  read FDisablePDFExtension               write FDisablePDFExtension;
      /// <summary>
      /// Disables site isolation.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://peter.sh/experiments/chromium-command-line-switches/">Uses the following command line switch: --disable-site-isolation-trials</see></para>
      /// </remarks>
      property DisableSiteIsolationTrials        : boolean                                  read FDisableSiteIsolationTrials        write FDisableSiteIsolationTrials;
      /// <summary>
      /// Delegate all login requests to the client GetAuthCredentials callback.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/libcef/common/cef_switches.cc">Uses the following command line switch: --disable-chrome-login-prompt</see></para>
      /// </remarks>
      property DisableChromeLoginPrompt          : boolean                                  read FDisableChromeLoginPrompt          write FDisableChromeLoginPrompt;
      /// <summary>
      /// Disable extensions.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://peter.sh/experiments/chromium-command-line-switches/">Uses the following command line switch: --disable-extensions</see></para>
      /// </remarks>
      property DisableExtensions                 : boolean                                  read FDisableExtensions                 write FDisableExtensions;
      /// <summary>
      /// Autoplay policy.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://peter.sh/experiments/chromium-command-line-switches/">Uses the following command line switch: --autoplay-policy</see></para>
      /// </remarks>
      property AutoplayPolicy                    : TCefAutoplayPolicy                       read FAutoplayPolicy                    write FAutoplayPolicy;
      /// <summary>
      /// Disable several subsystems which run network requests in the background.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://peter.sh/experiments/chromium-command-line-switches/">Uses the following command line switch: --disable-background-networking</see></para>
      /// </remarks>
      property DisableBackgroundNetworking       : boolean                                  read FDisableBackgroundNetworking       write FDisableBackgroundNetworking;
      /// <summary>
      /// Enables the recording of metrics reports but disables reporting.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://peter.sh/experiments/chromium-command-line-switches/">Uses the following command line switch: --metrics-recording-only</see></para>
      /// </remarks>
      property MetricsRecordingOnly              : boolean                                  read FMetricsRecordingOnly              write FMetricsRecordingOnly;
      /// <summary>
      /// By default, file:// URIs cannot read other file:// URIs. This is an override for developers who need the old behavior for testing.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://peter.sh/experiments/chromium-command-line-switches/">Uses the following command line switch: --allow-file-access-from-files</see></para>
      /// </remarks>
      property AllowFileAccessFromFiles          : boolean                                  read FAllowFileAccessFromFiles          write FAllowFileAccessFromFiles;
      /// <summary>
      /// By default, an https page cannot run JavaScript, CSS or plugins from http URLs. This provides an override to get the old insecure behavior.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://peter.sh/experiments/chromium-command-line-switches/">Uses the following command line switch: --allow-running-insecure-content</see></para>
      /// </remarks>
      property AllowRunningInsecureContent       : boolean                                  read FAllowRunningInsecureContent       write FAllowRunningInsecureContent;
      /// <summary>
      /// Enable print preview.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/libcef/common/cef_switches.cc">Uses the following command line switch: --enable-print-preview</see></para>
      /// </remarks>
      property EnablePrintPreview                : boolean                                  read FEnablePrintPreview                write FEnablePrintPreview;
      /// <summary>
      /// Default encoding.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/libcef/common/cef_switches.cc">Uses the following command line switch: --default-encoding</see></para>
      /// </remarks>
      property DefaultEncoding                   : ustring                                  read FDefaultEncoding                   write FDefaultEncoding;
      /// <summary>
      /// Disable JavaScript.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/libcef/common/cef_switches.cc">Uses the following command line switch: --disable-javascript</see></para>
      /// </remarks>
      property DisableJavascript                 : boolean                                  read FDisableJavascript                 write FDisableJavascript;
      /// <summary>
      /// Disable closing of windows via JavaScript.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/libcef/common/cef_switches.cc">Uses the following command line switch: --disable-javascript-close-windows</see></para>
      /// </remarks>
      property DisableJavascriptCloseWindows     : boolean                                  read FDisableJavascriptCloseWindows     write FDisableJavascriptCloseWindows;
      /// <summary>
      /// Disable clipboard access via JavaScript.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/libcef/common/cef_switches.cc">Uses the following command line switch: --disable-javascript-access-clipboard</see></para>
      /// </remarks>
      property DisableJavascriptAccessClipboard  : boolean                                  read FDisableJavascriptAccessClipboard  write FDisableJavascriptAccessClipboard;
      /// <summary>
      /// Disable DOM paste via JavaScript execCommand("paste").
      /// </summary>
      /// <remarks>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/libcef/common/cef_switches.cc">Uses the following command line switch: --disable-javascript-dom-paste</see></para>
      /// </remarks>
      property DisableJavascriptDomPaste         : boolean                                  read FDisableJavascriptDomPaste         write FDisableJavascriptDomPaste;
      /// <summary>
      /// Allow universal access from file URLs.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/libcef/common/cef_switches.cc">Uses the following command line switch: --allow-universal-access-from-files</see></para>
      /// </remarks>
      property AllowUniversalAccessFromFileUrls  : boolean                                  read FAllowUniversalAccessFromFileUrls  write FAllowUniversalAccessFromFileUrls;
      /// <summary>
      /// Disable loading of images from the network. A cached image will still be rendered if requested.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/libcef/common/cef_switches.cc">Uses the following command line switch: --disable-image-loading</see></para>
      /// </remarks>
      property DisableImageLoading               : boolean                                  read FDisableImageLoading               write FDisableImageLoading;
      /// <summary>
      /// Shrink stand-alone images to fit.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/libcef/common/cef_switches.cc">Uses the following command line switch: --image-shrink-standalone-to-fit</see></para>
      /// </remarks>
      property ImageShrinkStandaloneToFit        : boolean                                  read FImageShrinkStandaloneToFit        write FImageShrinkStandaloneToFit;
      /// <summary>
      /// Disable resizing of text areas.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/libcef/common/cef_switches.cc">Uses the following command line switch: --disable-text-area-resize</see></para>
      /// </remarks>
      property DisableTextAreaResize             : boolean                                  read FDisableTextAreaResize             write FDisableTextAreaResize;
      /// <summary>
      /// Disable using the tab key to advance focus to links.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/libcef/common/cef_switches.cc">Uses the following command line switch: --disable-tab-to-links</see></para>
      /// </remarks>
      property DisableTabToLinks                 : boolean                                  read FDisableTabToLinks                 write FDisableTabToLinks;
      /// <summary>
      /// Enable the speech input profanity filter.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/libcef/common/cef_switches.cc">Uses the following command line switch: --enable-profanity-filter</see></para>
      /// </remarks>
      property EnableProfanityFilter             : boolean                                  read FEnableProfanityFilter             write FEnableProfanityFilter;
      /// <summary>
      /// Disable spell checking.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/libcef/common/cef_switches.cc">Uses the following command line switch: --disable-spell-checking</see></para>
      /// </remarks>
      property DisableSpellChecking              : boolean                                  read FDisableSpellChecking              write FDisableSpellChecking;
      /// <summary>
      /// Override the default spellchecking language which comes from locales.pak.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/libcef/common/cef_switches.cc">Uses the following command line switch: --override-spell-check-lang</see></para>
      /// </remarks>
      property OverrideSpellCheckLang            : ustring                                  read FOverrideSpellCheckLang            write FOverrideSpellCheckLang;
      /// <summary>
      /// Enable support for touch event feature detection.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://peter.sh/experiments/chromium-command-line-switches/">Uses the following command line switch: --touch-events</see></para>
      /// </remarks>
      property TouchEvents                       : TCefState                                read FTouchEvents                       write FTouchEvents;
      /// <summary>
      /// Taints all <canvas> elements, regardless of origin.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://peter.sh/experiments/chromium-command-line-switches/">Uses the following command line switch: --disable-reading-from-canvas</see></para>
      /// </remarks>
      property DisableReadingFromCanvas          : boolean                                  read FDisableReadingFromCanvas          write FDisableReadingFromCanvas;
      /// <summary>
      /// Don't send hyperlink auditing pings.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://peter.sh/experiments/chromium-command-line-switches/">Uses the following command line switch: --no-pings</see></para>
      /// </remarks>
      property HyperlinkAuditing                 : boolean                                  read FHyperlinkAuditing                 write FHyperlinkAuditing;
      /// <summary>
      /// Disable the timeout for delivering new browser info to the renderer process.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/libcef/common/cef_switches.cc">Uses the following command line switch: --disable-new-browser-info-timeout</see></para>
      /// </remarks>
      property DisableNewBrowserInfoTimeout      : boolean                                  read FDisableNewBrowserInfoTimeout      write FDisableNewBrowserInfoTimeout;
      /// <summary>
      /// File used for logging DevTools protocol messages.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/libcef/common/cef_switches.cc">Uses the following command line switch: --devtools-protocol-log-file</see></para>
      /// </remarks>
      property DevToolsProtocolLogFile           : ustring                                  read FDevToolsProtocolLogFile           write FDevToolsProtocolLogFile;
      /// <summary>
      /// Overrides the device scale factor for the browser UI and the contents.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://peter.sh/experiments/chromium-command-line-switches/">Uses the following command line switch: --force-device-scale-factor</see></para>
      /// </remarks>
      property ForcedDeviceScaleFactor           : single                                   read FForcedDeviceScaleFactor           write FForcedDeviceScaleFactor;
      /// <summary>
      /// Disables the use of a zygote process for forking child processes. Instead, child processes will be forked and exec'd directly.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://peter.sh/experiments/chromium-command-line-switches/">Uses the following command line switch: --no-zygote</see></para>
      /// </remarks>
      property DisableZygote                     : boolean                                  read FDisableZygote                     write FDisableZygote;
      /// <summary>
      /// Uses mock keychain for testing purposes, which prevents blocking dialogs from causing timeouts.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://peter.sh/experiments/chromium-command-line-switches/">Uses the following command line switch: --use-mock-keychain</see></para>
      /// </remarks>
      property UseMockKeyChain                   : boolean                                  read FUseMockKeyChain                   write FUseMockKeyChain;
      /// <summary>
      /// Disable request handling in CEF to faciliate debugging of network-related issues.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/libcef/common/cef_switches.cc">Uses the following command line switch: --disable-request-handling-for-testing</see></para>
      /// </remarks>
      property DisableRequestHandlingForTesting  : boolean                                  read FDisableRequestHandlingForTesting  write FDisableRequestHandlingForTesting;
      /// <summary>
      /// Disables pop-up blocking.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://peter.sh/experiments/chromium-command-line-switches/">Uses the following command line switch: --disable-popup-blocking</see></para>
      /// </remarks>
      property DisablePopupBlocking              : boolean                                  read FDisablePopupBlocking              write FDisablePopupBlocking;
      /// <summary>
      /// Disables the BackForwardCache feature.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://peter.sh/experiments/chromium-command-line-switches/">Uses the following command line switch: --disable-back-forward-cache</see></para>
      /// </remarks>
      property DisableBackForwardCache           : boolean                                  read FDisableBackForwardCache           write FDisableBackForwardCache;
      /// <summary>
      /// Disable the component updater. Widevine will not be downloaded or initialized.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://peter.sh/experiments/chromium-command-line-switches/">Uses the following command line switch: --disable-component-update</see></para>
      /// </remarks>
      property DisableComponentUpdate            : boolean                                  read FDisableComponentUpdate            write FDisableComponentUpdate;
      /// <summary>
      /// Enables TLS/SSL errors on localhost to be ignored (no interstitial, no blocking of requests).
      /// </summary>
      /// <remarks>
      /// <para><see href="https://peter.sh/experiments/chromium-command-line-switches/">Uses the following command line switch: --allow-insecure-localhost</see></para>
      /// </remarks>
      property AllowInsecureLocalhost            : boolean                                  read FAllowInsecureLocalhost            write FAllowInsecureLocalhost;
      /// <summary>
      /// Enable automatically pressing the print button in print preview.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://peter.sh/experiments/chromium-command-line-switches/">Uses the following command line switch: --kiosk-printing</see></para>
      /// </remarks>
      property KioskPrinting                     : boolean                                  read FKioskPrinting                     write SetKioskPrinting;
      /// <summary>
      /// Treat given (insecure) origins as secure origins.
      /// Multiple origins can be supplied as a comma-separated list.
      /// For the definition of secure contexts, see https://w3c.github.io/webappsec-secure-contexts/
      /// and https://www.w3.org/TR/powerful-features/#is-origin-trustworthy
      /// Example: --unsafely-treat-insecure-origin-as-secure=http://a.test,http://b.test
      /// </summary>
      /// <remarks>
      /// <para><see href="https://peter.sh/experiments/chromium-command-line-switches/">Uses the following command line switch: --unsafely-treat-insecure-origin-as-secure</see></para>
      /// </remarks>
      property TreatInsecureOriginAsSecure       : ustring                                  read FTreatInsecureOriginAsSecure       write FTreatInsecureOriginAsSecure;
      /// <summary>
      /// Enables saving net log events to a file.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://peter.sh/experiments/chromium-command-line-switches/">Uses the following command line switch: --log-net-log</see></para>
      /// </remarks>
      property NetLogEnabled                     : boolean                                  read FNetLogEnabled                     write FNetLogEnabled;
      /// <summary>
      /// File name used to log net events. If a value is given,
      /// it used as the path the the file, otherwise the file is named netlog.json
      /// and placed in the user data directory.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://peter.sh/experiments/chromium-command-line-switches/">Uses the following command line switch: --log-net-log</see></para>
      /// </remarks>
      property NetLogFile                        : ustring                                  read FNetLogFile                        write FNetLogFile;
      /// <summary>
      /// Sets the granularity of events to capture in the network log.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://peter.sh/experiments/chromium-command-line-switches/">Uses the following command line switch: --net-log-capture-mode</see></para>
      /// </remarks>
      property NetLogCaptureMode                 : TCefNetLogCaptureMode                    read FNetLogCaptureMode                 write FNetLogCaptureMode;
      /// <summary>
      /// Enables web socket connections from the specified origins only. '*' allows any origin.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://peter.sh/experiments/chromium-command-line-switches/">Uses the following command line switch: --remote-allow-origins</see></para>
      /// </remarks>
      property RemoteAllowOrigins                : ustring                                  read FRemoteAllowOrigins                write FRemoteAllowOrigins;
      /// <summary>
      /// Bypasses the dialog prompting the user for permission to capture cameras and microphones.
      /// Useful in automatic tests of video-conferencing Web applications. This is nearly
      /// identical to kUseFakeUIForMediaStream, with the exception being that this flag does NOT
      /// affect screen-capture.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://peter.sh/experiments/chromium-command-line-switches/">Uses the following command line switch: --auto-accept-camera-and-microphone-capture</see></para>
      /// </remarks>
      property AutoAcceptCamAndMicCapture        : boolean                                  read FAutoAcceptCamAndMicCapture        write FAutoAcceptCamAndMicCapture;
      /// <summary>
      /// Forces light or dark mode in UI for platforms that support it.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://peter.sh/experiments/chromium-command-line-switches/">Uses the following command line switches: --force-dark-mode --force-light-mode</see></para>
      /// </remarks>
      property UIColorMode                       : TCefUIColorMode                          read FUIColorMode                       write FUIColorMode;
      /// <summary>
      /// Suppresses hang monitor dialogs in renderer processes. This may allow slow unload handlers on a page to prevent the tab from closing, but the Task Manager can be used to terminate the offending process in this case.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://peter.sh/experiments/chromium-command-line-switches/">Uses the following command line switch: --disable-hang-monitor</see></para>
      /// </remarks>
      property DisableHangMonitor                : boolean                                  read FDisableHangMonitor                write FDisableHangMonitor;
      /// <summary>
      /// Does not show the "Restore pages" popup bubble after incorrect shutdown.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://peter.sh/experiments/chromium-command-line-switches/">Uses the following command line switch: --hide-crash-restore-bubble</see></para>
      /// </remarks>
      property HideCrashRestoreBubble            : boolean                                  read FHideCrashRestoreBubble            write FHideCrashRestoreBubble;
      /// <summary>
      /// This option enables a combination of X25519 and Kyber in TLS 1.3.
      /// </summary>
      property TLS13HybridizedKyberSupport       : TCefState                                read FPostQuantumKyber                  write FPostQuantumKyber;
      /// <summary>
      /// Ignores certificate-related errors.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://source.chromium.org/chromium/chromium/src/+/main:components/network_session_configurator/common/network_switch_list.h">Uses the following command line switch: --ignore-certificate-errors</see></para>
      /// </remarks>
      property IgnoreCertificateErrors           : Boolean                                  read FIgnoreCertificateErrors           write FIgnoreCertificateErrors;

      /// <summary>
      /// Pointer to the sandbox info. Currently unused in Delphi and Lazarus.
      /// </summary>
      property WindowsSandboxInfo                : Pointer                                  read FWindowsSandboxInfo                write FWindowsSandboxInfo;
      {$IFDEF LINUX}
      /// <summary>
      /// argc parameter copy used in Linux only.
      /// </summary>
      property argcCopy                          : longint                                  read GetArgc;
      /// <summary>
      /// argv parameter copy used in Linux only.
      /// </summary>
      property argvCopy                          : PPAnsiChar                               read GetArgv;
      {$ENDIF}

      /// <summary>
      /// Used to delete all the cache files before CEF is initialized.
      /// </summary>
      property DeleteCache                       : boolean                                  read FDeleteCache                       write FDeleteCache;
      /// <summary>
      /// Used to delete all the cookies before CEF is initialized.
      /// </summary>
      property DeleteCookies                     : boolean                                  read FDeleteCookies                     write FDeleteCookies;
      /// <summary>
      /// Checks if the CEF binaries are present and the DLL version.
      /// </summary>
      property CheckCEFFiles                     : boolean                                  read FCheckCEFFiles                     write FCheckCEFFiles;
      /// <summary>
      /// Set to true when you need to use a showmessage dialog to show the error messages.
      /// </summary>
      property ShowMessageDlg                    : boolean                                  read FShowMessageDlg                    write FShowMessageDlg;
      /// <summary>
      /// Raise an exception when the CEF binaries check fails.
      /// </summary>
      property MissingBinariesException          : boolean                                  read FMissingBinariesException          write FMissingBinariesException;
      /// <summary>
      ///	Used to set the current directory when the CEF libraries are loaded. This is required if the application is launched from a different application.
      /// </summary>
      property SetCurrentDir                     : boolean                                  read FSetCurrentDir                     write FSetCurrentDir;
      /// <summary>
      ///	Set to True when the global context is initialized and the application can start creating web browsers.
      /// </summary>
      property GlobalContextInitialized          : boolean                                  read GetGlobalContextInitialized;
      /// <summary>
      ///	Returns the major version information from Chromium.
      /// </summary>
      property ChromeMajorVer                    : uint16                                   read FChromeVersionInfo.MajorVer;
      /// <summary>
      ///	Returns the minor version information from Chromium.
      /// </summary>
      property ChromeMinorVer                    : uint16                                   read FChromeVersionInfo.MinorVer;
      /// <summary>
      ///	Returns the release version information from Chromium.
      /// </summary>
      property ChromeRelease                     : uint16                                   read FChromeVersionInfo.Release;
      /// <summary>
      ///	Returns the build version information from Chromium.
      /// </summary>
      property ChromeBuild                       : uint16                                   read FChromeVersionInfo.Build;
      /// <summary>
      ///	Returns the full version information from Chromium.
      /// </summary>
      property ChromeVersion                     : ustring                                  read GetChromeVersion;
      /// <summary>
      ///	Complete libcef version information.
      /// </summary>
      property LibCefVersion                     : ustring                                  read GetLibCefVersion;
      /// <summary>
      ///	Path to libcef.dll or libcef.so
      /// </summary>
      property LibCefPath                        : ustring                                  read GetLibCefPath;
      /// <summary>
      ///	Returns the path to chrome_elf.dll.
      /// </summary>
      property ChromeElfPath                     : ustring                                  read GetChromeElfPath;
      /// <summary>
      ///	Set to true when TCEFApplicationCore has loaded the CEF libraries.
      /// </summary>
      property LibLoaded                         : boolean                                  read FLibLoaded;
      /// <summary>
      ///	Add a debug log information line when the CEF libraries are loaded.
      /// </summary>
      property LogProcessInfo                    : boolean                                  read FLogProcessInfo                    write FLogProcessInfo;
      /// <summary>
      /// Set to true to raise all exceptions.
      /// </summary>
      property ReRaiseExceptions                 : boolean                                  read FReRaiseExceptions                 write FReRaiseExceptions;
      /// <summary>
      /// Returns the device scale factor used in OSR mode.
      /// </summary>
      property DeviceScaleFactor                 : single                                   read FDeviceScaleFactor;
      /// <summary>
      /// List of locale files that will be checked with CheckCEFFiles.
      /// </summary>
      property LocalesRequired                   : ustring                                  read FLocalesRequired                   write FLocalesRequired;
      /// <summary>
      /// CEF process type currently running.
      /// </summary>
      property ProcessType                       : TCefProcessType                          read FProcessType;
      /// <summary>
      /// Force the creation of ICefResourceBundleHandler.
      /// </summary>
      property MustCreateResourceBundleHandler   : boolean                                  read GetMustCreateResourceBundleHandler write FMustCreateResourceBundleHandler;
      /// <summary>
      /// Force the creation of ICefBrowserProcessHandler.
      /// </summary>
      property MustCreateBrowserProcessHandler   : boolean                                  read GetMustCreateBrowserProcessHandler write FMustCreateBrowserProcessHandler;
      /// <summary>
      /// Force the creation of ICefRenderProcessHandler.
      /// </summary>
      property MustCreateRenderProcessHandler    : boolean                                  read GetMustCreateRenderProcessHandler  write FMustCreateRenderProcessHandler;
      /// <summary>
      /// Force the creation of ICefLoadHandler.
      /// </summary>
      property MustCreateLoadHandler             : boolean                                  read GetMustCreateLoadHandler           write FMustCreateLoadHandler;
      {$IFDEF MSWINDOWS}
      /// <summary>
      /// Set to true (1) before calling Windows APIs like TrackPopupMenu that enter a
      /// modal message loop. Set to false (0) after exiting the modal message loop.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/internal/cef_app_win.h">CEF source file: /include/internal/cef_app_win.h (cef_set_osmodal_loop)</see></para>
      /// </remarks>
      property OsmodalLoop                       : boolean                                                                          write SetOsmodalLoop;
      {$ENDIF}
      /// <summary>
      /// Returns the TCEFApplicationCore initialization status.
      /// </summary>
      property Status                            : TCefAplicationStatus                     read FStatus;
      /// <summary>
      /// List of missing CEF library files.
      /// </summary>
      property MissingLibFiles                   : string                                   read FMissingLibFiles;
      /// <summary>
      /// Set to true to free the library handle when TCEFApplicationCore is destroyed.
      /// </summary>
      property MustFreeLibrary                   : boolean                                  read FMustFreeLibrary                   write FMustFreeLibrary;
      /// <summary>
      /// Returns the number of CEF subprocesses running at that moment.
      /// </summary>
      property ChildProcessesCount               : integer                                  read GetChildProcessesCount;
      /// <summary>
      /// Total used memory by all CEF processes.
      /// </summary>
      property UsedMemory                        : uint64                                   read GetUsedMemory;
      /// <summary>
      /// Total system memory in Windows.
      /// </summary>
      property TotalSystemMemory                 : uint64                                   read GetTotalSystemMemory;
      /// <summary>
      /// Calculates the available memory in Windows.
      /// </summary>
      property AvailableSystemMemory             : uint64                                   read GetAvailableSystemMemory;
      /// <summary>
      /// Memory load in Windows.
      /// </summary>
      property SystemMemoryLoad                  : cardinal                                 read GetSystemMemoryLoad;
      /// <summary>
      /// Calls cef_api_hash to get the universal hash.
      /// </summary>
      property ApiHashUniversal                  : ustring                                  read GetApiHashUniversal;
      /// <summary>
      /// Calls cef_api_hash to get the platform hash.
      /// </summary>
      property ApiHashPlatform                   : ustring                                  read GetApiHashPlatform;
      /// <summary>
      ///	Calls cef_api_hash to get the commit hash.
      /// </summary>
      property ApiHashCommit                     : ustring                                  read GetApiHashCommit;
      /// <summary>
      /// This property can optionally be read on the main application thread after
      /// CefInitialize to retrieve the initialization exit code. When CefInitialize
      /// returns true (1) the exit code will be 0 (CEF_RESULT_CODE_NORMAL_EXIT).
      /// Otherwise, see TCefResultCode for possible exit code values including
      /// browser process initialization errors and normal early exit conditions (such
      /// as CEF_RESULT_CODE_NORMAL_EXIT_PROCESS_NOTIFIED for process singleton
      /// relaunch behavior).
      /// </summary>
      property ExitCode                          : TCefResultCode                           read GetExitCode;
      /// <summary>
      /// Last error message that is usually shown when CEF finds a problem at initialization.
      /// </summary>
      property LastErrorMessage                  : ustring                                  read FLastErrorMessage;
      {$IFDEF LINUX}
      /// <summary>
      /// Return the singleton X11 display shared with Chromium. The display is not
      /// thread-safe and must only be accessed on the browser process UI thread.
      /// </summary>
      property XDisplay                          : PXDisplay                                read GetXDisplay;
      {$ENDIF}

      /// <summary>
      /// Provides an opportunity to register custom schemes. Do not keep a
      /// reference to the |registrar| object. This function is called on the main
      /// thread for each process and the registered schemes should be the same
      /// across all processes.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_app_capi.h">CEF source file: /include/capi/cef_app_capi.h (cef_app_t)</see></para>
      /// </remarks>
      property OnRegCustomSchemes                : TOnRegisterCustomSchemesEvent            read FOnRegisterCustomSchemes           write FOnRegisterCustomSchemes;
      /// <summary>
      /// <para>Provides an opportunity to register custom preferences prior to global and
      /// request context initialization.</para>
      /// <para>If |type| is CEF_PREFERENCES_TYPE_GLOBAL the registered preferences can be
      /// accessed via ICefPreferenceManager.GetGlobalPreferences after
      /// OnContextInitialized is called. Global preferences are registered a single
      /// time at application startup. See related TCefSettings.cache_path
      /// configuration.</para>
      /// <para>If |type| is CEF_PREFERENCES_TYPE_REQUEST_CONTEXT the preferences can be
      /// accessed via the ICefRequestContext after
      /// ICefRequestContextHandler.OnRequestContextInitialized is called.
      /// Request context preferences are registered each time a new
      /// ICefRequestContext is created. It is intended but not required that all
      /// request contexts have the same registered preferences. See related
      /// TCefRequestContextSettings.cache_path configuration.</para>
      /// <para>Do not keep a reference to the |registrar| object. This function is called
      /// on the browser process UI thread.</para>
      /// </summary>
      /// <remarks>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_browser_process_handler_capi.h">CEF source file: /include/capi/cef_browser_process_handler_capi.h (cef_browser_process_handler_t)</see></para>
      /// </remarks>
      property OnRegisterCustomPreferences       : TOnRegisterCustomPreferencesEvent        read FOnRegisterCustomPreferences       write FOnRegisterCustomPreferences;
      /// <summary>
      /// Called on the browser process UI thread immediately after the CEF context
      /// has been initialized.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_browser_process_handler_capi.h">CEF source file: /include/capi/cef_browser_process_handler_capi.h (cef_browser_process_handler_t)</see></para>
      /// </remarks>
      property OnContextInitialized              : TOnContextInitializedEvent               read FOnContextInitialized              write FOnContextInitialized;
      /// <summary>
      /// Called before a child process is launched. Will be called on the browser
      /// process UI thread when launching a render process and on the browser
      /// process IO thread when launching a GPU process. Provides an opportunity to
      /// modify the child process command line. Do not keep a reference to
      /// |command_line| outside of this function.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_browser_process_handler_capi.h">CEF source file: /include/capi/cef_browser_process_handler_capi.h (cef_browser_process_handler_t)</see></para>
      /// </remarks>
      property OnBeforeChildProcessLaunch        : TOnBeforeChildProcessLaunchEvent         read FOnBeforeChildProcessLaunch        write FOnBeforeChildProcessLaunch;
      /// <summary>
      /// <para>Implement this function to provide app-specific behavior when an already
      /// running app is relaunched with the same TCefSettings.root_cache_path value.
      /// For example, activate an existing app window or create a new app window.
      /// |command_line| will be read-only. Do not keep a reference to
      /// |command_line| outside of this function. Return true (1) if the relaunch
      /// is handled or false (0) for default relaunch behavior. Default behavior
      /// will create a new default styled Chrome window.</para>
      /// <para>To avoid cache corruption only a single app instance is allowed to run for
      /// a given TCefSettings.root_cache_path value. On relaunch the app checks a
      /// process singleton lock and then forwards the new launch arguments to the
      /// already running app process before exiting early. Client apps should
      /// therefore check the cef_initialize() return value for early exit before
      /// proceeding.</para>
      /// <para>This function will be called on the browser process UI thread.</para>
      /// </summary>
      /// <remarks>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_browser_process_handler_capi.h">CEF source file: /include/capi/cef_browser_process_handler_capi.h (cef_browser_process_handler_t)</see></para>
      /// </remarks>
      property OnAlreadyRunningAppRelaunch       : TOnAlreadyRunningAppRelaunchEvent        read FOnAlreadyRunningAppRelaunch       write FOnAlreadyRunningAppRelaunch;
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
      /// <remarks>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_browser_process_handler_capi.h">CEF source file: /include/capi/cef_browser_process_handler_capi.h (cef_browser_process_handler_t)</see></para>
      /// </remarks>
      property OnScheduleMessagePumpWork         : TOnScheduleMessagePumpWorkEvent          read FOnScheduleMessagePumpWork         write FOnScheduleMessagePumpWork;
      /// <summary>
      /// Return the default client for use with a newly created browser window
      /// (TCefBrowser object). If null is returned the TCefBrowser will be
      /// unmanaged (no callbacks will be executed for that TCefBrowser) and
      /// application shutdown will be blocked until the browser window is closed
      /// manually. This function is currently only used with Chrome style when
      /// creating new browser windows via Chrome UI.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_browser_process_handler_capi.h">CEF source file: /include/capi/cef_browser_process_handler_capi.h (cef_browser_process_handler_t)</see></para>
      /// </remarks>
      property OnGetDefaultClient                : TOnGetDefaultClientEvent                 read FOnGetDefaultClient                write FOnGetDefaultClient;
      /// <summary>
      /// Return the default handler for use with a new user or incognito profile
      /// (TCefRequestContext object). If null is returned the
      /// TCefRequestContext will be unmanaged (no callbacks will be executed for
      /// that TCefRequestContext). This function is currently only used with
      /// Chrome style when creating new browser windows via Chrome UI.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_browser_process_handler_capi.h">CEF source file: /include/capi/cef_browser_process_handler_capi.h (cef_browser_process_handler_t)</see></para>
      /// </remarks>
      property OnGetDefaultRequestContextHandler : TOnGetDefaultRequestContextHandlerEvent  read FOnGetDefaultRequestContextHandler write FOnGetDefaultRequestContextHandler;
      /// <summary>
      /// Called to retrieve a localized translation for the specified |string_id|.
      /// To provide the translation set |string| to the translation string and
      /// return true (1). To use the default translation return false (0). Include
      /// cef_pack_strings.h for a listing of valid string ID values.
      /// </summary>
      /// <remarks>
      /// <para>This event may be called on multiple threads.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_resource_bundle_handler_capi.h">CEF source file: /include/capi/cef_resource_bundle_handler_capi.h (cef_resource_bundle_handler_t)</see></para>
      /// </remarks>
      property OnGetLocalizedString              : TOnGetLocalizedStringEvent               read FOnGetLocalizedString              write FOnGetLocalizedString;
      /// <summary>
      /// Called to retrieve data for the specified scale independent |resource_id|.
      /// To provide the resource data set |data| and |data_size| to the data
      /// pointer and size respectively and return true (1). To use the default
      /// resource data return false (0). The resource data will not be copied and
      /// must remain resident in memory. Include cef_pack_resources.h for a listing
      /// of valid resource ID values.
      /// </summary>
      /// <remarks>
      /// <para>This event may be called on multiple threads.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_resource_bundle_handler_capi.h">CEF source file: /include/capi/cef_resource_bundle_handler_capi.h (cef_resource_bundle_handler_t)</see></para>
      /// </remarks>
      property OnGetDataResource                 : TOnGetDataResourceEvent                  read FOnGetDataResource                 write FOnGetDataResource;
      /// <summary>
      /// Called to retrieve data for the specified |resource_id| nearest the scale
      /// factor |scale_factor|. To provide the resource data set |data| and
      /// |data_size| to the data pointer and size respectively and return true (1).
      /// To use the default resource data return false (0). The resource data will
      /// not be copied and must remain resident in memory. Include
      /// cef_pack_resources.h for a listing of valid resource ID values.
      /// </summary>
      /// <remarks>
      /// <para>This event may be called on multiple threads.</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_resource_bundle_handler_capi.h">CEF source file: /include/capi/cef_resource_bundle_handler_capi.h (cef_resource_bundle_handler_t)</see></para>
      /// </remarks>
      property OnGetDataResourceForScale         : TOnGetDataResourceForScaleEvent          read FOnGetDataResourceForScale         write FOnGetDataResourceForScale;
      /// <summary>
      /// Called after WebKit has been initialized.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the render process main thread (TID_RENDERER)</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_render_process_handler_capi.h">CEF source file: /include/capi/cef_render_process_handler_capi.h (cef_render_process_handler_t)</see></para>
      /// </remarks>
      property OnWebKitInitialized               : TOnWebKitInitializedEvent                read FOnWebKitInitialized               write FOnWebKitInitialized;
      /// <summary>
      /// Called after a browser has been created. When browsing cross-origin a new
      /// browser will be created before the old browser with the same identifier is
      /// destroyed. |extra_info| is an optional read-only value originating from
      /// cef_browser_host_create_browser(),
      /// cef_browser_host_create_browser_sync(),
      /// ICefLifeSpanHandler.OnBeforePopup or
      /// cef_browser_view_create().
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the render process main thread (TID_RENDERER)</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_render_process_handler_capi.h">CEF source file: /include/capi/cef_render_process_handler_capi.h (cef_render_process_handler_t)</see></para>
      /// </remarks>
      property OnBrowserCreated                  : TOnBrowserCreatedEvent                   read FOnBrowserCreated                  write FOnBrowserCreated;
      /// <summary>
      /// Called before a browser is destroyed.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the render process main thread (TID_RENDERER)</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_render_process_handler_capi.h">CEF source file: /include/capi/cef_render_process_handler_capi.h (cef_render_process_handler_t)</see></para>
      /// </remarks>
      property OnBrowserDestroyed                : TOnBrowserDestroyedEvent                 read FOnBrowserDestroyed                write FOnBrowserDestroyed;
      /// <summary>
      /// Called immediately after the V8 context for a frame has been created. To
      /// retrieve the JavaScript 'window' object use the
      /// ICefv8context.GetGlobal function. V8 handles can only be accessed
      /// from the thread on which they are created. A task runner for posting tasks
      /// on the associated thread can be retrieved via the
      /// ICefv8context.GetTaskRunner() function.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the render process main thread (TID_RENDERER)</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_render_process_handler_capi.h">CEF source file: /include/capi/cef_render_process_handler_capi.h (cef_render_process_handler_t)</see></para>
      /// </remarks>
      property OnContextCreated                  : TOnContextCreatedEvent                   read FOnContextCreated                  write FOnContextCreated;
      /// <summary>
      /// Called immediately before the V8 context for a frame is released. No
      /// references to the context should be kept after this function is called.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the render process main thread (TID_RENDERER)</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_render_process_handler_capi.h">CEF source file: /include/capi/cef_render_process_handler_capi.h (cef_render_process_handler_t)</see></para>
      /// </remarks>
      property OnContextReleased                 : TOnContextReleasedEvent                  read FOnContextReleased                 write FOnContextReleased;
      /// <summary>
      /// Called for global uncaught exceptions in a frame. Execution of this
      /// callback is disabled by default. To enable set
      /// TCefSettings.uncaught_exception_stack_size > 0.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the render process main thread (TID_RENDERER)</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_render_process_handler_capi.h">CEF source file: /include/capi/cef_render_process_handler_capi.h (cef_render_process_handler_t)</see></para>
      /// </remarks>
      property OnUncaughtException               : TOnUncaughtExceptionEvent                read FOnUncaughtException               write FOnUncaughtException;
      /// <summary>
      /// Called when a new node in the the browser gets focus. The |node| value may
      /// be NULL if no specific node has gained focus. The node object passed to
      /// this function represents a snapshot of the DOM at the time this function
      /// is executed. DOM objects are only valid for the scope of this function. Do
      /// not keep references to or attempt to access any DOM objects outside the
      /// scope of this function.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the render process main thread (TID_RENDERER)</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_render_process_handler_capi.h">CEF source file: /include/capi/cef_render_process_handler_capi.h (cef_render_process_handler_t)</see></para>
      /// </remarks>
      property OnFocusedNodeChanged              : TOnFocusedNodeChangedEvent               read FOnFocusedNodeChanged              write FOnFocusedNodeChanged;
      /// <summary>
      /// Called when a new message is received from a different process. Return
      /// true (1) if the message was handled or false (0) otherwise. It is safe to
      /// keep a reference to |message| outside of this callback.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the render process main thread (TID_RENDERER)</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_render_process_handler_capi.h">CEF source file: /include/capi/cef_render_process_handler_capi.h (cef_render_process_handler_t)</see></para>
      /// </remarks>
      property OnProcessMessageReceived          : TOnProcessMessageReceivedEvent           read FOnProcessMessageReceived          write FOnProcessMessageReceived;
      /// <summary>
      /// Called when the loading state has changed. This callback will be executed
      /// twice -- once when loading is initiated either programmatically or by user
      /// action, and once when loading is terminated due to completion,
      /// cancellation of failure. It will be called before any calls to OnLoadStart
      /// and after all calls to OnLoadError and/or OnLoadEnd.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the render process main thread (TID_RENDERER)</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_load_handler_capi.h">CEF source file: /include/capi/cef_load_handler_capi.h (cef_load_handler_t)</see></para>
      /// </remarks>
      property OnLoadingStateChange              : TOnRenderLoadingStateChange              read FOnLoadingStateChange              write FOnLoadingStateChange;
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
      /// <para>This event will be called on the render process main thread (TID_RENDERER)</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_load_handler_capi.h">CEF source file: /include/capi/cef_load_handler_capi.h (cef_load_handler_t)</see></para>
      /// </remarks>
      property OnLoadStart                       : TOnRenderLoadStart                       read FOnLoadStart                       write FOnLoadStart;
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
      /// <para>This event will be called on the render process main thread (TID_RENDERER)</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_load_handler_capi.h">CEF source file: /include/capi/cef_load_handler_capi.h (cef_load_handler_t)</see></para>
      /// </remarks>
      property OnLoadEnd                         : TOnRenderLoadEnd                         read FOnLoadEnd                         write FOnLoadEnd;
      /// <summary>
      /// Called when a navigation fails or is canceled. This function may be called
      /// by itself if before commit or in combination with OnLoadStart/OnLoadEnd if
      /// after commit. |errorCode| is the error code number, |errorText| is the
      /// error text and |failedUrl| is the URL that failed to load. See
      /// net\base\net_error_list.h for complete descriptions of the error codes.
      /// </summary>
      /// <remarks>
      /// <para>This event will be called on the render process main thread (TID_RENDERER)</para>
      /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_load_handler_capi.h">CEF source file: /include/capi/cef_load_handler_capi.h (cef_load_handler_t)</see></para>
      /// </remarks>
      property OnLoadError                       : TOnRenderLoadError                       read FOnLoadError                       write FOnLoadError;
  end;

  TCEFDirectoryDeleterThread = class(TThread)
    protected
      FDirectory : string;

      procedure Execute; override;

    public
      constructor Create(const aDirectory : string);
  end;

var
  GlobalCEFApp : TCefApplicationCore = nil;

procedure DestroyGlobalCEFApp;

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
    System.Math, System.IOUtils, System.SysUtils,
    {$IFDEF MSWINDOWS}WinApi.TlHelp32, WinApi.PSAPI,{$ENDIF}
    {$IFDEF LINUX}{$IFDEF FMX}Posix.Unistd, Posix.Stdio,{$ENDIF}{$ENDIF}
    {$IFDEF MACOS}Posix.Stdio,{$ENDIF}
  {$ELSE}
    Math, {$IFDEF DELPHI14_UP}IOUtils,{$ENDIF} SysUtils,
    {$IFDEF FPC}
      {$IFDEF MSWINDOWS}jwatlhelp32, jwapsapi,{$ENDIF}
      {$IFDEF LINUX}lcltype, Forms, InterfaceBase, uCEFLinuxFunctions,{$ENDIF}
    {$ELSE}
      TlHelp32, {$IFDEF MSWINDOWS}PSAPI,{$ENDIF}
    {$ENDIF}
  {$ENDIF}
  {$IFDEF MACOSX}uCEFMacOSFunctions,{$ENDIF}
  uCEFLibFunctions, uCEFMiscFunctions, uCEFCommandLine, uCEFConstants,
  uCEFSchemeHandlerFactory, uCEFCookieManager, uCEFApp, uCEFCompletionCallback,
  uCEFWaitableEvent;

procedure DestroyGlobalCEFApp;
begin
  if (GlobalCEFApp <> nil) then FreeAndNil(GlobalCEFApp);
end;

constructor TCefApplicationCore.Create;
begin
  inherited Create;

  if (GlobalCEFApp = nil) then
    GlobalCEFApp := Self;

  // Fields used to populate TCefSettings
  FNoSandbox                         := True;
  FBrowserSubprocessPath             := '';
  FFrameworkDirPath                  := '';
  FMainBundlePath                    := {$IFDEF MACOSX}GetModulePath{$ELSE}''{$ENDIF};
  FMultiThreadedMessageLoop          := True;
  FExternalMessagePump               := False;
  FWindowlessRenderingEnabled        := False;
  FCommandLineArgsDisabled           := False;
  FCache                             := '';
  FRootCache                         := '';
  FPersistSessionCookies             := False;
  FUserAgent                         := '';
  FUserAgentProduct                  := '';
  FLocale                            := '';
  FLogFile                           := '';
  FLogSeverity                       := LOGSEVERITY_DISABLE;
  FLogItems                          := LOG_ITEMS_DEFAULT;
  FJavaScriptFlags                   := '';
  FResourcesDirPath                  := '';
  FLocalesDirPath                    := '';
  FRemoteDebuggingPort               := 0;
  FUncaughtExceptionStackSize        := 0;
  FIgnoreCertificateErrors           := False;
  FBackgroundColor                   := 0;
  FAcceptLanguageList                := '';
  FCookieableSchemesList             := '';
  FCookieableSchemesExcludeDefaults  := False;
  FChromePolicyId                    := '';
  FChromeAppIconId                   := 0;
  {$IF DEFINED(OS_POSIX) AND NOT(DEFINED(ANDROID))}
  FDisableSignalHandlers             := False;
  {$IFEND}

  // Fields used to set command line switches
  FSingleProcess                     := False;
  FEnableMediaStream                 := False;
  FEnableSpeechInput                 := False;
  FUseFakeUIForMediaStream           := False;
  FEnableUsermediaScreenCapturing    := False;
  FEnableGPU                         := False;
  FEnableFeatures                    := '';
  FDisableFeatures                   := '';
  FEnableBlinkFeatures               := '';
  FDisableBlinkFeatures              := '';
  FBlinkSettings                     := '';
  FForceFieldTrials                  := '';
  FForceFieldTrialParams             := '';
  FSmoothScrolling                   := STATE_DEFAULT;
  FMuteAudio                         := False;
  FSitePerProcess                    := False;
  FDisableWebSecurity                := False;
  FDisablePDFExtension               := False;
  FDisableSiteIsolationTrials        := False;
  FDisableChromeLoginPrompt          := True;
  FDisableExtensions                 := False;
  FAutoplayPolicy                    := appDefault;
  FDisableBackgroundNetworking       := False;
  FMetricsRecordingOnly              := False;
  FAllowFileAccessFromFiles          := False;
  FAllowRunningInsecureContent       := False;
  FEnablePrintPreview                := False;
  FDefaultEncoding                   := '';
  FDisableJavascript                 := False;
  FDisableJavascriptCloseWindows     := False;
  FDisableJavascriptAccessClipboard  := False;
  FDisableJavascriptDomPaste         := False;
  FAllowUniversalAccessFromFileUrls  := False;
  FDisableImageLoading               := False;
  FImageShrinkStandaloneToFit        := False;
  FDisableTextAreaResize             := False;
  FDisableTabToLinks                 := False;
  FEnableProfanityFilter             := False;
  FDisableSpellChecking              := False;
  FOverrideSpellCheckLang            := '';
  FTouchEvents                       := STATE_DEFAULT;
  FDisableReadingFromCanvas          := False;
  FHyperlinkAuditing                 := True;
  FDisableNewBrowserInfoTimeout      := False;
  FDevToolsProtocolLogFile           := '';
  FForcedDeviceScaleFactor           := 0;
  FDisableZygote                     := False;
  FUseMockKeyChain                   := False;
  FDisableRequestHandlingForTesting  := False;
  FDisablePopupBlocking              := False;
  FDisableBackForwardCache           := False;
  FDisableComponentUpdate            := False;
  FAllowInsecureLocalhost            := False;
  FKioskPrinting                     := False;
  FTreatInsecureOriginAsSecure       := '';
  FNetLogEnabled                     := False;
  FNetLogFile                        := '';
  FNetLogCaptureMode                 := nlcmDefault;
  FRemoteAllowOrigins                := '';
  FAutoAcceptCamAndMicCapture        := False;
  FUIColorMode                       := uicmSystemDefault;
  FDisableHangMonitor                := False;
  FHideCrashRestoreBubble            := True;
  FPostQuantumKyber                  := STATE_DEFAULT;

  // Fields used during the CEF initialization
  FWindowsSandboxInfo                := nil;
  {$IFDEF LINUX}
  FArgCopy                           := TCEFArgCopy.Create;
  {$ENDIF}

  // Fields used by custom properties
  FDeleteCache                       := False;
  FDeleteCookies                     := False;
  FCheckCEFFiles                     := {$IFDEF MACOSX}False{$ELSE}True{$ENDIF};
  FShowMessageDlg                    := True;
  FMissingBinariesException          := False;
  FSetCurrentDir                     := False;
  FGlobalContextInitialized          := False;
  FChromeVersionInfo.MajorVer        := CEF_CHROMEELF_VERSION_MAJOR;
  FChromeVersionInfo.MinorVer        := CEF_CHROMEELF_VERSION_MINOR;
  FChromeVersionInfo.Release         := CEF_CHROMEELF_VERSION_RELEASE;
  FChromeVersionInfo.Build           := CEF_CHROMEELF_VERSION_BUILD;
  FLibLoaded                         := False;
  FLogProcessInfo                    := False;
  FReRaiseExceptions                 := False;
  UpdateDeviceScaleFactor;
  FLocalesRequired                   := '';
  FProcessType                       := ParseProcessType;
  FMustCreateResourceBundleHandler   := False;
  FMustCreateBrowserProcessHandler   := True;  // The official CEF sample application always creates this handler in the browser process
  FMustCreateRenderProcessHandler    := True;  // The official CEF sample application always creates this handler in the renderer process
  FMustCreateLoadHandler             := False;
  FStatus                            := asLoading;
  FMissingLibFiles                   := '';
  FMustFreeLibrary                   := False;
  FLastErrorMessage                  := '';
  {$IFDEF MSWINDOWS}
  case FProcessType of
    ptBrowser  : GetDLLVersion(ChromeElfPath, FChromeVersionInfo);
    ptCrashpad :
      // The crashpad handler process must be the last one to be closed
      SetProcessShutdownParameters($100, SHUTDOWN_NORETRY);
    else
      // Subprocesses will be the last to be notified about the Windows shutdown.
      // The main browser process will receive WM_QUERYENDSESSION before the subprocesses
      // and that allows to close the application in the right order.
      // See the MiniBrowser demo for all the details.
      SetProcessShutdownParameters(CHROMIUM_NONBROWSERSHUTDOWNPRIORITY - 1, SHUTDOWN_NORETRY);
  end;
  {$ENDIF}

  // Internal filelds
  FLibHandle                         := 0;
  FCustomCommandLines                := nil;
  FCustomCommandLineValues           := nil;
  FillChar(FAppSettings, SizeOf(TCefSettings), 0);
  FAppSettings.size := SizeOf(TCefSettings);
  FDisableGPUCache                   := True;
  FComponentIDList                   := nil;

  // ICefApp
  FOnRegisterCustomSchemes           := nil;

  // ICefBrowserProcessHandler
  FOnRegisterCustomPreferences       := nil;
  FOnContextInitialized              := nil;
  FOnBeforeChildProcessLaunch        := nil;
  FOnAlreadyRunningAppRelaunch       := nil;
  FOnScheduleMessagePumpWork         := nil;
  FOnGetDefaultClient                := nil;
  FOnGetDefaultRequestContextHandler := nil;

  // ICefResourceBundleHandler
  FOnGetLocalizedString              := nil;
  FOnGetDataResource                 := nil;
  FOnGetDataResourceForScale         := nil;

  // ICefRenderProcessHandler
  FOnWebKitInitialized               := nil;
  FOnBrowserCreated                  := nil;
  FOnBrowserDestroyed                := nil;
  FOnContextCreated                  := nil;
  FOnContextReleased                 := nil;
  FOnUncaughtException               := nil;
  FOnFocusedNodeChanged              := nil;
  FOnProcessMessageReceived          := nil;

  // ICefLoadHandler
  FOnLoadingStateChange              := nil;
  FOnLoadStart                       := nil;
  FOnLoadEnd                         := nil;
  FOnLoadError                       := nil;

  IsMultiThread := True;

  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
end;

destructor TCefApplicationCore.Destroy;
begin
  try
    ClearSchemeHandlerFactories;

    if (GlobalCEFApp = Self) then
      GlobalCEFApp := nil;

    if (FProcessType = ptBrowser) then
      ShutDown;

    FreeLibcefLibrary;

    {$IFDEF LINUX}
    if (FArgCopy                 <> nil) then FreeAndNil(FArgCopy);
    {$ENDIF}
    if (FCustomCommandLines      <> nil) then FreeAndNil(FCustomCommandLines);
    if (FCustomCommandLineValues <> nil) then FreeAndNil(FCustomCommandLineValues);
    if (FComponentIDList         <> nil) then FreeAndNil(FComponentIDList);
  finally
    inherited Destroy;
  end;
end;

procedure TCefApplicationCore.doOnBeforeCommandLineProcessing(const processType : ustring;
                                                              const commandLine : ICefCommandLine);
var
  i : integer;
  TempKeys, TempValues : TStringList;
begin
  TempKeys   := nil;
  TempValues := nil;

  try
    if (commandLine <> nil) and
       commandLine.IsValid and
       (FProcessType = ptBrowser) and
       (processType = '') then
      begin
        TempKeys   := TStringList.Create;
        TempValues := TStringList.Create;
        commandLine.GetSwitches(TempKeys, TempValues);

        AddCustomCommandLineSwitches(TempKeys, TempValues);

        commandLine.Reset;

        i := 0;
        while (i < TempKeys.Count) do
          begin
            if (length(TempKeys[i]) > 0) then
              begin
                if (length(TempValues[i]) > 0) then
                  commandLine.AppendSwitchWithValue(TempKeys[i], TempValues[i])
                 else
                  commandLine.AppendSwitch(TempKeys[i]);
              end;

            inc(i);
          end;
      end;
  finally
    if (TempKeys   <> nil) then FreeAndNil(TempKeys);
    if (TempValues <> nil) then FreeAndNil(TempValues);
  end;
end;

procedure TCefApplicationCore.doOnRegisterCustomSchemes(const registrar: TCefSchemeRegistrarRef);
begin
  if assigned(FOnRegisterCustomSchemes) then
    FOnRegisterCustomSchemes(registrar);
end;

procedure TCefApplicationCore.doOnRegisterCustomPreferences(type_: TCefPreferencesType; registrar: PCefPreferenceRegistrar);
var
  TempRegistrar : TCefPreferenceRegistrarRef;
begin
  if assigned(FOnRegisterCustomPreferences) then
    try
      TempRegistrar := TCefPreferenceRegistrarRef.Create(registrar);
      FOnRegisterCustomPreferences(type_, TempRegistrar);
    finally
      FreeAndNil(TempRegistrar);
    end;
end;

procedure TCefApplicationCore.doOnContextInitialized;
begin
  FGlobalContextInitialized := True;

  if assigned(FOnContextInitialized) then
    FOnContextInitialized();
end;

procedure TCefApplicationCore.doOnBeforeChildProcessLaunch(const commandLine: ICefCommandLine);
begin
  if assigned(FOnBeforeChildProcessLaunch) then
    FOnBeforeChildProcessLaunch(commandLine);
end;

procedure TCefApplicationCore.doOnAlreadyRunningAppRelaunch(const commandLine: ICefCommandLine; const current_directory: ustring; var aResult: boolean);
begin
  if assigned(FOnAlreadyRunningAppRelaunch) then
    FOnAlreadyRunningAppRelaunch(commandLine, current_directory, aResult);
end;

procedure TCefApplicationCore.doOnScheduleMessagePumpWork(const delayMs: Int64);
begin
  if assigned(FOnScheduleMessagePumpWork) then
    FOnScheduleMessagePumpWork(delayMs);
end;

procedure TCefApplicationCore.doGetDefaultClient(var aClient : ICefClient);
begin
  if assigned(FOnGetDefaultClient) then
    FOnGetDefaultClient(aClient);
end;

procedure TCefApplicationCore.doGetDefaultRequestContextHandler(var aRequestContextHandler : ICefRequestContextHandler);
begin
  if assigned(FOnGetDefaultRequestContextHandler) then
    FOnGetDefaultRequestContextHandler(aRequestContextHandler);
end;

function TCefApplicationCore.doGetLocalizedString(stringid: Integer; var stringVal: ustring) : boolean;
begin
  Result := False;

  // The stringId must be one of the values defined in the CEF file :
  // /include/cef_pack_strings.h
  // That file is available in the CEF binaries package.
  if assigned(FOnGetLocalizedString) then
    FOnGetLocalizedString(stringId, stringVal, Result);
end;

function TCefApplicationCore.doGetDataResource(resourceId: Integer; var data: Pointer; var dataSize: NativeUInt) : boolean;
begin
  Result := False;

  // The resourceId must be one of the values defined in the CEF file :
  // /include/cef_pack_resources.h
  // That file is available in the CEF binaries package.
  if assigned(FOnGetDataResource) then
    FOnGetDataResource(resourceId, data, dataSize, Result);
end;

procedure TCefApplicationCore.doOnWebKitInitialized;
begin
  if assigned(FOnWebKitInitialized) then
    FOnWebKitInitialized();
end;

procedure TCefApplicationCore.doOnBrowserCreated(const browser: ICefBrowser; const extra_info: ICefDictionaryValue);
begin
  if assigned(FOnBrowserCreated) then
    FOnBrowserCreated(browser, extra_info);
end;

procedure TCefApplicationCore.doOnBrowserDestroyed(const browser: ICefBrowser);
begin
  if assigned(FOnBrowserDestroyed) then
    FOnBrowserDestroyed(browser);
end;

procedure TCefApplicationCore.doOnContextCreated(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context);
begin
  if assigned(FOnContextCreated) then
    FOnContextCreated(browser, frame, context);
end;

procedure TCefApplicationCore.doOnContextReleased(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context);
begin
  if assigned(FOnContextReleased) then
    FOnContextReleased(browser, frame, context);
end;

procedure TCefApplicationCore.doOnUncaughtException(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context; const V8Exception: ICefV8Exception; const stackTrace: ICefV8StackTrace);
begin
  if assigned(FOnUncaughtException) then
    FOnUncaughtException(browser, frame, context, V8Exception, stackTrace);
end;

procedure TCefApplicationCore.doOnFocusedNodeChanged(const browser: ICefBrowser; const frame: ICefFrame; const node: ICefDomNode);
begin
  if assigned(FOnFocusedNodeChanged) then
    FOnFocusedNodeChanged(browser, frame, node);
end;

procedure TCefApplicationCore.doOnProcessMessageReceived(const browser: ICefBrowser; const frame: ICefFrame; sourceProcess: TCefProcessId; const aMessage: ICefProcessMessage; var aHandled : boolean);
begin
  if assigned(FOnProcessMessageReceived) then
    FOnProcessMessageReceived(browser, frame, sourceProcess, aMessage, aHandled)
   else
    aHandled := False;
end;

procedure TCefApplicationCore.doOnLoadingStateChange(const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
begin
  if assigned(FOnLoadingStateChange) then
    FOnLoadingStateChange(browser, isLoading, canGoBack, canGoForward);
end;

procedure TCefApplicationCore.doOnLoadStart(const browser: ICefBrowser; const frame: ICefFrame; transitionType: TCefTransitionType);
begin
  if assigned(FOnLoadStart) then
    FOnLoadStart(browser, frame, transitionType);
end;

procedure TCefApplicationCore.doOnLoadEnd(const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: Integer);
begin
  if assigned(FOnLoadEnd) then
    FOnLoadEnd(browser, frame, httpStatusCode);
end;

procedure TCefApplicationCore.doOnLoadError(const browser: ICefBrowser; const frame: ICefFrame; errorCode: TCefErrorCode; const errorText, failedUrl: ustring);
begin
  if assigned(FOnLoadError) then
    FOnLoadError(browser, frame, errorCode, errorText, failedUrl);
end;

function TCefApplicationCore.doGetDataResourceForScale(resourceId: Integer; scaleFactor: TCefScaleFactor; var data: Pointer; var dataSize: NativeUInt) : boolean;
begin
  Result := False;

  // The resourceId must be one of the values defined in the CEF file :
  // /include/cef_pack_resources.h
  // That file is available in the CEF binaries package.
  if assigned(FOnGetDataResourceForScale) then
    FOnGetDataResourceForScale(resourceId, scaleFactor, data, dataSize, Result);
end;

procedure TCefApplicationCore.ClearSchemeHandlerFactories;
begin
  try
    if FLibLoaded then
      cef_clear_scheme_handler_factories();
  except
    on e : exception do
      if CustomExceptionHandler('TCefApplicationCore.ClearSchemeHandlerFactories', e) then raise;
  end;
end;

procedure TCefApplicationCore.AfterConstruction;
begin
  inherited AfterConstruction;

  FCustomCommandLines      := TStringList.Create;
  FCustomCommandLineValues := TStringList.Create;
  FComponentIDList         := TCEFComponentIDList.Create;
end;

procedure TCefApplicationCore.AddCustomCommandLine(const aCommandLine, aValue : string);
begin
  if (FCustomCommandLines      <> nil) then FCustomCommandLines.Add(aCommandLine);
  if (FCustomCommandLineValues <> nil) then FCustomCommandLineValues.Add(aValue);
end;

{$IFDEF MACOSX}
// This function is used by the CEF subprocesses in MacOS to read the framework
// and bundle settings from the command line switches.
procedure TCefApplicationCore.InitLibLocationFromArgs;
var
  TempFrameworkPath, TempBundlePath : ustring;
begin
  if GetCommandLineSwitchValue('framework-dir-path', TempFrameworkPath) then
    FrameworkDirPath := TempFrameworkPath;

  if GetCommandLineSwitchValue('main-bundle-path', TempBundlePath) then
    MainBundlePath := TempBundlePath;

  if (TempBundlePath <> '') and (FrameworkDirPath = '') then
    begin
      TempBundlePath := IncludeTrailingPathDelimiter(TempBundlePath) + LIBCEF_PREFIX;

      if FileExists(TempBundlePath + LIBCEF_DLL) then
        FrameworkDirPath := TempBundlePath;
    end;
end;
{$ENDIF}

// This function must only be called by the main executable when the application
// is configured to use a different executable for the subprocesses.
// The process calling ths function must be the browser process.
function TCefApplicationCore.MultiExeProcessing : boolean;
var
  TempApp : ICefApp;
begin
  Result  := False;
  TempApp := nil;

  try
    try
      if (ProcessType = ptBrowser) and
         CheckCEFLibrary and
         LoadCEFlibrary then
        begin
          TempApp := TCustomCefApp.Create(self);

          if InitializeLibrary(TempApp) then
            Result := True
           else
            TempApp.RemoveReferences;
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TCefApplicationCore.MultiExeProcessing', e) then raise;
    end;
  finally
    TempApp := nil;
  end;
end;

// This function will be called by all processes when the application is configured
// to use the same executable for all the processes : browser, render, etc.
function TCefApplicationCore.SingleExeProcessing : boolean;
var
  TempApp : ICefApp;
begin
  Result  := False;
  TempApp := nil;

  try
    try
      if CheckCEFLibrary and LoadCEFlibrary then
        begin
          if (FProcessType <> ptBrowser) then
            BeforeInitSubProcess;

          TempApp := TCustomCefApp.Create(self);

          if (ExecuteProcess(TempApp) < 0) and
             InitializeLibrary(TempApp) then
            Result  := True
           else
            TempApp.RemoveReferences;
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TCefApplicationCore.SingleExeProcessing', e) then raise;
    end;
  finally
    TempApp := nil;
  end;
end;

procedure TCefApplicationCore.BeforeInitSubProcess;
begin
  // Is implemented by TCefApplication
end;

function TCefApplicationCore.GetChromeVersion : ustring;
begin
  Result := FileVersionInfoToString(FChromeVersionInfo);
end;

function TCefApplicationCore.GetLibCefVersion : ustring;
begin
  Result := IntToStr(CEF_SUPPORTED_VERSION_MAJOR)    + '.' +
            IntToStr(CEF_SUPPORTED_VERSION_MINOR)    + '.' +
            IntToStr(CEF_SUPPORTED_VERSION_RELEASE)  + '.' +
            IntToStr(CEF_SUPPORTED_VERSION_BUILD);
end;

function TCefApplicationCore.GetLibCefPath : ustring;
begin
  if (length(FFrameworkDirPath) > 0) then
    Result := IncludeTrailingPathDelimiter(FFrameworkDirPath) + LIBCEF_DLL
   else
    begin
      {$IFDEF LINUX}
      Result := GetModulePath + LIBCEF_DLL;
      {$ELSE}
        {$IFDEF MACOSX}
        Result := GetModulePath + LIBCEF_PREFIX + LIBCEF_DLL;
        {$ELSE}
        Result := LIBCEF_DLL;
        {$ENDIF}
      {$ENDIF}
    end;
end;

function TCefApplicationCore.GetChromeElfPath : ustring;
begin
  if (length(FFrameworkDirPath) > 0) then
    Result := IncludeTrailingPathDelimiter(FFrameworkDirPath) + CHROMEELF_DLL
   else
    Result := CHROMEELF_DLL;
end;

function TCefApplicationCore.GetLocalesDirPath: ustring;
begin
  Result := FLocalesDirPath;
  {$IFNDEF MACOSX}
  if (Result = '') and (FrameworkDirPath <> '') then
    begin
      if FileExists(IncludeTrailingPathDelimiter(FrameworkDirPath + LIBCEF_LOCALE_DIR) + LIBCEF_LOCALE_ENUS) then
        Result := FrameworkDirPath + LIBCEF_LOCALE_DIR;
    end;
  {$ENDIF}
end;

function TCefApplicationCore.GetResourcesDirPath: ustring;
begin
  Result := FResourcesDirPath;
  {$IFNDEF MACOSX}
  if (Result = '') and (FrameworkDirPath <> '') then
    begin
      if FileExists(IncludeTrailingPathDelimiter(FrameworkDirPath) + LIBCEF_PAK) then
        Result := FrameworkDirPath;
    end;
  {$ENDIF}
end;

procedure TCefApplicationCore.SetCache(const aValue : ustring);
begin
  FCache           := CustomAbsolutePath(aValue);
  FDisableGPUCache := (length(FCache) = 0);
end;

procedure TCefApplicationCore.SetRootCache(const aValue : ustring);
begin
  FRootCache := CustomAbsolutePath(aValue);
end;

procedure TCefApplicationCore.SetBrowserSubprocessPath(const aValue : ustring);
begin
  FBrowserSubprocessPath := CustomAbsolutePath(aValue);
end;

procedure TCefApplicationCore.SetFrameworkDirPath(const aValue : ustring);
begin
  FFrameworkDirPath := CustomAbsolutePath(aValue, True);

  {$IFDEF MSWINDOWS}
  if (FProcessType = ptBrowser) then
    GetDLLVersion(ChromeElfPath, FChromeVersionInfo);
  {$ENDIF}
end;

procedure TCefApplicationCore.SetResourcesDirPath(const aValue : ustring);
begin
  FResourcesDirPath := CustomAbsolutePath(aValue, True);
end;

procedure TCefApplicationCore.SetLocalesDirPath(const aValue : ustring);
begin
  FLocalesDirPath := CustomAbsolutePath(aValue, True);
end;

function TCefApplicationCore.CheckCEFResources : boolean;
var
  TempMissingFrm, TempMissingRsc, TempMissingLoc, TempMissingSubProc : boolean;
begin
  Result := False;

  TempMissingSubProc := not(CheckSubprocessPath(FBrowserSubprocessPath, FMissingLibFiles));
  TempMissingFrm     := not(CheckDLLs(FFrameworkDirPath, FMissingLibFiles));
  TempMissingRsc     := not(CheckResources(ResourcesDirPath, FMissingLibFiles));
  TempMissingLoc     := not(CheckLocales(LocalesDirPath, FMissingLibFiles, FLocalesRequired));

  if TempMissingFrm or TempMissingRsc or TempMissingLoc or TempMissingSubProc then
    begin
      FStatus           := asErrorMissingFiles;
      FLastErrorMessage := 'CEF binaries missing !';

      if (length(FMissingLibFiles) > 0) then
        FLastErrorMessage := FLastErrorMessage + CRLF + CRLF +
                             'The missing files are :' + CRLF +
                             trim(FMissingLibFiles);

      ShowErrorMessageDlg(FLastErrorMessage);
    end
   else
    Result := True;
end;

{$IFDEF MSWINDOWS}
function TCefApplicationCore.CheckCEFDLL : boolean;
var
  TempMachine : integer;
  TempVersionInfo : TFileVersionInfo;
begin
  Result := False;

  if CheckDLLVersion(LibCefPath,
                     CEF_SUPPORTED_VERSION_MAJOR,
                     CEF_SUPPORTED_VERSION_MINOR,
                     CEF_SUPPORTED_VERSION_RELEASE,
                     CEF_SUPPORTED_VERSION_BUILD) then
    begin
      if GetDLLHeaderMachine(LibCefPath, TempMachine) then
        case TempMachine of
          CEF_IMAGE_FILE_MACHINE_I386 :
            if Is32BitProcess then
              Result := True
             else
              begin
                FStatus           := asErrorDLLVersion;
                FLastErrorMessage := 'Wrong CEF binaries !' +
                                     CRLF + CRLF +
                                     'Use the 32 bit CEF binaries with 32 bits applications only.';

                ShowErrorMessageDlg(FLastErrorMessage);
              end;

          CEF_IMAGE_FILE_MACHINE_AMD64 :
            if not(Is32BitProcess) then
              Result := True
             else

              begin
                FStatus           := asErrorDLLVersion;
                FLastErrorMessage := 'Wrong CEF binaries !' +
                                     CRLF + CRLF +
                                     'Use the 64 bit CEF binaries with 64 bits applications only.';

                ShowErrorMessageDlg(FLastErrorMessage);
              end;

          else
            begin
              FStatus           := asErrorDLLVersion;
              FLastErrorMessage := 'Unknown CEF binaries !' +
                                   CRLF + CRLF +
                                   'Use only the CEF binaries specified in the CEF4Delphi Readme.md file at ' +
                                   CEF4DELPHI_URL;

              ShowErrorMessageDlg(FLastErrorMessage);
            end;
        end
       else
        Result := True;
    end
   else
    begin
      FStatus    := asErrorDLLVersion;
      FLastErrorMessage := 'Unsupported CEF version !' +
                           CRLF + CRLF +
                           'Use only the CEF binaries specified in the CEF4Delphi Readme.md file at ' +
                           CEF4DELPHI_URL;

      if GetDLLVersion(LibCefPath, TempVersionInfo) then
        FLastErrorMessage := FLastErrorMessage + CRLF + CRLF +
                             'Expected ' + LIBCEF_DLL + ' version : ' + LibCefVersion + CRLF +
                             'Found ' + LIBCEF_DLL + ' version : ' + FileVersionInfoToString(TempVersionInfo);

      ShowErrorMessageDlg(FLastErrorMessage);
    end;
end;

function TCefApplicationCore.CheckWindowsVersion : boolean;
begin
  // Chromium 109 requires Windows 10 or later.
  // https://github.com/salvadordf/CEF4Delphi/issues/452
  if CheckRealWindowsVersion(10, 0) then
    Result := True
   else
    begin
      Result            := False;
      FStatus           := asErrorWindowsVersion;
      FLastErrorMessage := 'Unsupported Windows version !' +
                           CRLF + CRLF +
                           'Chromium requires Windows 10 or later.';
      ShowErrorMessageDlg(FLastErrorMessage);
    end;
end;
{$ENDIF}

{$IFDEF MACOSX}
function TCefApplicationCore.CheckMacOSVersion : boolean;
begin
  // Chromium crashes with the compatibility mode and old OS versions
  // https://source.chromium.org/chromium/chromium/src/+/main:base/mac/mac_util.mm;drc=6166dba74ab72a84a6c7c575cacb438e6bfb2c66;l=338
  // https://support.google.com/chrome/a/answer/7100626?hl=en
  if CheckRealMacOSVersion(11, 0) then
    Result := True
   else
    begin
      Result            := False;
      FStatus           := asErrorWindowsVersion;
      FLastErrorMessage := 'Unsupported macOS version !' +
                           CRLF + CRLF +
                           'Chromium requires macOS 11.0 or later.';
      ShowErrorMessageDlg(FLastErrorMessage);
    end;
end;
{$ENDIF}

function TCefApplicationCore.CheckOSVersion: boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := CheckWindowsVersion;
  {$ELSE}
    {$IFDEF MACOSX}
    Result := CheckMacOSVersion;
    {$ELSE}
    Result := True;  // Linux
    {$ENDIF}
  {$ENDIF}
end;

function TCefApplicationCore.CheckCEFLibrary : boolean;
var
  TempOldDir : string;
begin
  if (FProcessType <> ptBrowser) then
    begin
      Result := True;
      exit;
    end;

  if FSetCurrentDir then
    begin
      TempOldDir := GetCurrentDir;
      chdir(GetModulePath);
    end;

  Result := (not(FCheckCEFFiles) or CheckCEFResources) and CheckOSVersion;
  {$IFDEF MSWINDOWS}
  Result := Result and (not(FCheckCEFFiles) or CheckCEFDLL);
  {$ENDIF}

  if FSetCurrentDir then chdir(TempOldDir);
end;

function TCefApplicationCore.StartMainProcess : boolean;
begin
  if (FStatus <> asLoading) then
    Result := False
   else
    {$IFDEF MACOSX}
    Result := MultiExeProcessing;
    {$ELSE}
    if not(FSingleProcess) and (length(FBrowserSubprocessPath) > 0) then
      Result := MultiExeProcessing
     else
      Result := SingleExeProcessing;
    {$ENDIF}
end;

// This function can only be called by the executable used for the subprocesses.
// The application must be configured to use different executables for the subprocesses.
// The process calling this function can't be the browser process.
function TCefApplicationCore.StartSubProcess : boolean;
var
  TempApp : ICefApp;
begin
  Result  := False;
  TempApp := nil;

  try
    try
      if not(FSingleProcess) and
         (ProcessType <> ptBrowser) and
         LoadCEFlibrary then
        begin
          TempApp := TCustomCefApp.Create(self);

          if (ExecuteProcess(TempApp) >= 0) then
            Result := True
           else
            TempApp.RemoveReferences;
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TCefApplicationCore.StartSubProcess', e) then raise;
    end;
  finally
    TempApp := nil;
  end;
end;

procedure TCefApplicationCore.DoMessageLoopWork;
begin
  if FLibLoaded and
     not(FMultiThreadedMessageLoop) and
     FExternalMessagePump then
    cef_do_message_loop_work();
end;

procedure TCefApplicationCore.RunMessageLoop;
begin
  if FLibLoaded and
     not(FMultiThreadedMessageLoop) and
     not(FExternalMessagePump) then
    cef_run_message_loop();
end;

procedure TCefApplicationCore.QuitMessageLoop;
begin
  if FLibLoaded and
     not(FMultiThreadedMessageLoop) and
     not(FExternalMessagePump) then
    cef_quit_message_loop();
end;

{$IFDEF MSWINDOWS}
procedure TCefApplicationCore.SetOsmodalLoop(aValue : boolean);
begin
  if (FStatus = asInitialized) then cef_set_osmodal_loop(Ord(aValue));
end;
{$ENDIF}

procedure TCefApplicationCore.SetKioskPrinting(aValue : boolean);
begin
  if (FKioskPrinting <> aValue) then
    begin
      FKioskPrinting := aValue;

      if FKioskPrinting then
        FEnablePrintPreview := True;
    end;
end;

procedure TCefApplicationCore.UpdateDeviceScaleFactor;
begin
  if (FForcedDeviceScaleFactor <> 0) then
    FDeviceScaleFactor := FForcedDeviceScaleFactor
   else
    FDeviceScaleFactor := GetDeviceScaleFactor;
end;

function TCefApplicationCore.ValidComponentID(aComponentID : integer) : boolean;
begin
  Result := FComponentIDList.ValidID(aComponentID);
end;

function TCefApplicationCore.NextComponentID : integer;
begin
  Result := FComponentIDList.NextID;
end;

procedure TCefApplicationCore.RemoveComponentID(aComponentID : integer);
begin
  FComponentIDList.RemoveID(aComponentID);
end;

function TCefApplicationCore.DumpWithoutCrashing(mseconds_between_dumps: int64; const function_name, file_name: ustring; line_number: integer): boolean;
var
  TempFunctionName, TempFileName : AnsiString;
begin
  Result := False;

  if (FStatus = asInitialized) then
    begin
      TempFunctionName := AnsiString(function_name);
      TempFileName     := AnsiString(file_name);
      Result           := (cef_dump_without_crashing(mseconds_between_dumps, @TempFunctionName[1], @TempFileName[1], line_number) <> 0);
    end;
end;

function TCefApplicationCore.DumpWithoutCrashingUnthrottled : boolean;
begin
  Result := (FStatus = asInitialized) and
            (cef_dump_without_crashing_unthrottled() <> 0);
end;

procedure TCefApplicationCore.ShutDown;
begin
  try
    if (FStatus = asInitialized) then
      begin
        FStatus := asShuttingDown;
        cef_shutdown();
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TCefApplicationCore.ShutDown', e) then raise;
  end;
end;

procedure TCefApplicationCore.FreeLibcefLibrary;
begin
  try
    try
      if FMustFreeLibrary and (FLibHandle <> 0) then FreeLibrary(FLibHandle);
    except
      on e : exception do
        if CustomExceptionHandler('TCefApplicationCore.FreeLibcefLibrary', e) then raise;
    end;
  finally
    FLibHandle := 0;
    FLibLoaded := False;
    FStatus    := asUnloaded;
  end;
end;

{$WARN SYMBOL_PLATFORM OFF}
procedure TCefApplicationCore.InitializeCefMainArgs(var aCefMainArgs : TCefMainArgs);
begin
  {$IFDEF MSWINDOWS}
  aCefMainArgs.instance := HINSTANCE{$IFDEF FPC}(){$ENDIF};
  {$ENDIF}

  {$IFDEF LINUX}
    // Create a copy of argv on Linux because Chromium mangles the value internally (see CEF issue #620).
    // https://bitbucket.org/chromiumembedded/cef/issues/620/cef3-linux-crash-when-passing-command-line
    if (FArgCopy.argv = nil) then
      begin
        {$IFDEF FPC}
        FArgCopy.CopyFromArgs(argc, argv);
        {$ELSE}
        FArgCopy.CopyFromArgs(ArgCount, ArgValues);
        {$ENDIF}
      end;

    aCefMainArgs.argc := FArgCopy.argc;
    aCefMainArgs.argv := FArgCopy.argv;
  {$ENDIF}

  {$IFDEF MACOSX}
    {$IFDEF FPC}
    aCefMainArgs.argc := argc;
    aCefMainArgs.argv := argv;
    {$ELSE}
    aCefMainArgs.argc := ArgCount;
    aCefMainArgs.argv := ArgValues;
    {$ENDIF}
  {$ENDIF}
end;
{$WARN SYMBOL_PLATFORM ON}

function TCefApplicationCore.ExecuteProcess(const aApp : ICefApp) : integer;
var
  TempArgs : TCefMainArgs;
begin
  Result := -1;
  try
    if (aApp <> nil) then
      begin
        InitializeCefMainArgs(TempArgs);
        Result := cef_execute_process(@TempArgs, aApp.Wrap, FWindowsSandboxInfo);
      end;
  except
    on e : exception do
      begin
        FStatus := asErrorExecutingProcess;
        if CustomExceptionHandler('TCefApplicationCore.ExecuteProcess', e) then raise;
      end;
  end;
end;

procedure TCefApplicationCore.InitializeSettings(var aSettings : TCefSettings);
begin
  aSettings.size                                    := SizeOf(TCefSettings);
  aSettings.no_sandbox                              := Ord(FNoSandbox);
  aSettings.browser_subprocess_path                 := CefString(FBrowserSubprocessPath);
  aSettings.framework_dir_path                      := CefString(FFrameworkDirPath);
  aSettings.main_bundle_path                        := CefString(FMainBundlePath);
  aSettings.multi_threaded_message_loop             := Ord(FMultiThreadedMessageLoop);
  aSettings.external_message_pump                   := Ord(FExternalMessagePump);
  aSettings.windowless_rendering_enabled            := Ord(FWindowlessRenderingEnabled);
  aSettings.command_line_args_disabled              := Ord(FCommandLineArgsDisabled);
  aSettings.cache_path                              := CefString(FCache);
  aSettings.root_cache_path                         := CefString(FRootCache);
  aSettings.persist_session_cookies                 := Ord(FPersistSessionCookies);
  aSettings.user_agent                              := CefString(FUserAgent);
  aSettings.user_agent_product                      := CefString(FUserAgentProduct);
  aSettings.locale                                  := CefString(FLocale);
  aSettings.log_file                                := CefString(FLogFile);
  aSettings.log_severity                            := FLogSeverity;
  aSettings.log_items                               := FLogItems;
  aSettings.javascript_flags                        := CefString(FJavaScriptFlags);
  aSettings.resources_dir_path                      := CefString(ResourcesDirPath);
  aSettings.locales_dir_path                        := CefString(LocalesDirPath);
  aSettings.remote_debugging_port                   := FRemoteDebuggingPort;
  aSettings.uncaught_exception_stack_size           := FUncaughtExceptionStackSize;
  aSettings.background_color                        := FBackgroundColor;
  aSettings.accept_language_list                    := CefString(FAcceptLanguageList);
  aSettings.cookieable_schemes_list                 := CefString(FCookieableSchemesList);
  aSettings.cookieable_schemes_exclude_defaults     := Ord(FCookieableSchemesExcludeDefaults);
  aSettings.chrome_policy_id                        := CefString(FChromePolicyId);
  aSettings.chrome_app_icon_id                      := FChromeAppIconId;
  {$IF DEFINED(OS_POSIX) AND NOT(DEFINED(ANDROID))}
  aSettings.disable_signal_handlers                 := ord(FDisableSignalHandlers);
  {$IFEND}
end;

function TCefApplicationCore.InitializeLibrary(const aApp : ICefApp) : boolean;
var
  TempArgs : TCefMainArgs;
begin
  Result := False;

  try
    try
      if (aApp <> nil) then
        begin
          if FDeleteCache and FDeleteCookies then
            RenameAndDeleteDir(FCache)
           else
            if FDeleteCookies then
              DeleteCookiesDB(IncludeTrailingPathDelimiter(FCache) + 'Network')
             else
              if FDeleteCache then
                RenameAndDeleteDir(FCache, True);

          InitializeSettings(FAppSettings);
          InitializeCefMainArgs(TempArgs);

          if (cef_initialize(@TempArgs, @FAppSettings, aApp.Wrap, FWindowsSandboxInfo) <> 0) then
            begin
              Result  := True;
              FStatus := asInitialized;
            end;
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TCefApplicationCore.InitializeLibrary', e) then raise;
    end;
  finally
    if not(Result) then FStatus := asErrorInitializingLibrary;
  end;
end;

procedure TCefApplicationCore.DeleteCacheContents(const aDirectory : string);
var
  TempFiles : TStringList;
begin
  TempFiles := TStringList.Create;

  try
    TempFiles.Add('Cookies');
    TempFiles.Add('Cookies-journal');
    TempFiles.Add('LocalPrefs.json');

    DeleteDirContents(aDirectory, TempFiles);
  finally
    FreeAndNil(TempFiles);
  end;
end;

procedure TCefApplicationCore.DeleteCookiesDB(const aDirectory : string);
var
  TempFiles : TStringList;
begin
  TempFiles := TStringList.Create;

  try
    TempFiles.Add(IncludeTrailingPathDelimiter(aDirectory) + 'Cookies');
    TempFiles.Add(IncludeTrailingPathDelimiter(aDirectory) + 'Cookies-journal');

    DeleteFileList(TempFiles);
  finally
    FreeAndNil(TempFiles);
  end;
end;

procedure TCefApplicationCore.MoveCookiesDB(const aSrcDirectory, aDstDirectory : string);
var
  TempFiles : TStringList;
  TempSrc, TempDst : string;
begin
  TempFiles := TStringList.Create;

  try
    TempFiles.Add('LocalPrefs.json');

    MoveFileList(TempFiles, aSrcDirectory, aDstDirectory);

    TempSrc := IncludeTrailingPathDelimiter(aSrcDirectory) + 'Network';
    TempDst := IncludeTrailingPathDelimiter(aDstDirectory) + 'Network';

    TempFiles.Clear;
    TempFiles.Add('Cookies');
    TempFiles.Add('Cookies-journal');

    MoveFileList(TempFiles, TempSrc, TempDst);
  finally
    FreeAndNil(TempFiles);
  end;
end;

procedure TCefApplicationCore.RenameAndDeleteDir(const aDirectory : string; aKeepCookies : boolean);
var
  TempOldDir, TempNewDir : string;
  i : integer;
  TempThread : TCEFDirectoryDeleterThread;
begin
  try
    if (length(aDirectory) = 0) or not(DirectoryExists(aDirectory)) then exit;

    TempOldDir := ExcludeTrailingPathDelimiter(aDirectory);

    if (Pos(PathDelim, TempOldDir) > 0) and
       (length(ExtractFileName(TempOldDir)) > 0) then
      begin
        i := 0;

        repeat
          inc(i);
          TempNewDir := TempOldDir + '_' + inttostr(i);
        until not(DirectoryExists(TempNewDir));

        if RenameFile(TempOldDir, TempNewDir) then
          begin
            if aKeepCookies then MoveCookiesDB(TempNewDir, TempOldDir);

            TempThread := TCEFDirectoryDeleterThread.Create(TempNewDir);
            {$IFDEF DELPHI14_UP}
              TempThread.Start;
            {$ELSE}
              {$IFNDEF FPC}
              TempThread.Resume;
              {$ELSE}
              TempThread.Start;
              {$ENDIF}
            {$ENDIF}
          end
         else
          if aKeepCookies then
            DeleteCacheContents(aDirectory)
           else
            DeleteDirContents(aDirectory);
      end
     else
      if aKeepCookies then
        DeleteCacheContents(aDirectory)
       else
        DeleteDirContents(aDirectory);
  except
    on e : exception do
      if CustomExceptionHandler('TCefApplicationCore.RenameAndDeleteDir', e) then raise;
  end;
end;

procedure TCefApplicationCore.ShowErrorMessageDlg(const aError : string);
begin
  OutputDebugMessage(aError);

  if FShowMessageDlg then
    begin
      {$IFDEF MSWINDOWS}
      MessageBox(0, PChar(aError + #0), PChar('Error' + #0), MB_ICONERROR or MB_OK or MB_TOPMOST);
      {$ENDIF}

      {$IFDEF LINUX}
        {$IFDEF FPC}
        if (WidgetSet <> nil) then
          Application.MessageBox(PChar(aError + #0), PChar('Error' + #0), MB_ICONERROR or MB_OK)
         else
          ShowX11Message(aError);
        {$ELSE}
        // TO-DO: Find a way to show message boxes in FMXLinux
        {$ENDIF}
      {$ENDIF}

      {$IFDEF MACOSX}
        {$IFDEF FPC}
        // TO-DO: Find a way to show message boxes in Lazarus/FPC for MacOS
        {$ELSE}
        ShowMessageCF('Error', aError, 10);
        {$ENDIF}
      {$ENDIF}
    end;

  if FMissingBinariesException then
    raise Exception.Create(aError);
end;

function TCefApplicationCore.ParseProcessType : TCefProcessType;
var
  TempValue : ustring;
begin
  if GetCommandLineSwitchValue('type', TempValue) then
    begin
      if (CompareText(TempValue, 'renderer') = 0) then
        Result := ptRenderer
       else
        if (CompareText(TempValue, 'zygote') = 0) then
          Result := ptZygote
         else
          if (CompareText(TempValue, 'gpu-process') = 0) then
            Result := ptGPU
           else
            if (CompareText(TempValue, 'utility') = 0) then
              Result := ptUtility
             else
              if (CompareText(TempValue, 'broker') = 0) then
                Result := ptBroker
               else
                if (CompareText(TempValue, 'crashpad-handler') = 0) then
                  Result := ptCrashpad
                 else
                  Result := ptOther;
    end
   else
    Result := ptBrowser;
end;

procedure TCefApplicationCore.AppendSwitch(var aKeys, aValues : TStringList; const aNewKey, aNewValue : ustring);
var
  TempKey, TempHyphenatedKey : ustring;
  i : integer;
begin
  if (copy(aNewKey, 1, 2) = '--') then
    begin
      TempHyphenatedKey := aNewKey;
      TempKey           := copy(aNewKey, 3, length(aNewKey));
    end
   else
    begin
      TempHyphenatedKey := '--' + aNewKey;
      TempKey           := aNewKey;
    end;

  i := aKeys.IndexOf(TempKey);

  if (i < 0) then
    begin
      i := aKeys.IndexOf(TempHyphenatedKey);

      if (i < 0) then
        begin
          aKeys.Add(aNewKey);
          aValues.Add(aNewValue);
          exit;
        end;
    end;

  if (length(aNewValue) > 0) then
    begin
      if (length(aValues[i]) > 0) then
        aValues[i] := aValues[i] + ',' + aNewValue
       else
        aValues[i] := aNewValue;
    end;
end;

procedure TCefApplicationCore.CleanupFeatures(var aKeys, aValues : TStringList; const aEnableKey, aDisableKey : string);
var
  i, j, k, n : integer;
  TempEnabledValues, TempDisabledValues : TStringList;
  TempEnableKey, TempHyphenatedEnableKey, TempDisableKey, TempHyphenatedDisableKey : ustring;
begin
  if (copy(aEnableKey, 1, 2) = '--') then
    begin
      TempHyphenatedEnableKey := aEnableKey;
      TempEnableKey           := copy(aEnableKey, 3, length(aEnableKey));
    end
   else
    begin
      TempHyphenatedEnableKey := '--' + aEnableKey;
      TempEnableKey           := aEnableKey;
    end;

  if (copy(aDisableKey, 1, 2) = '--') then
    begin
      TempHyphenatedDisableKey := aDisableKey;
      TempDisableKey           := copy(aDisableKey, 3, length(aDisableKey));
    end
   else
    begin
      TempHyphenatedDisableKey := '--' + aDisableKey;
      TempDisableKey           := aDisableKey;
    end;

  i := aKeys.IndexOf(TempEnableKey);
  if (i < 0) then i := aKeys.IndexOf(TempHyphenatedEnableKey);

  j := aKeys.IndexOf(TempDisableKey);
  if (j < 0) then j := aKeys.IndexOf(TempHyphenatedDisableKey);

  if (i < 0) or (j < 0) then exit;

  TempEnabledValues            := TStringList.Create;
  TempDisabledValues           := TStringList.Create;
  TempEnabledValues.CommaText  := aValues[i];
  TempDisabledValues.CommaText := aValues[j];

  k := 0;
  while (k < TempDisabledValues.Count) do
    begin
      if (length(TempDisabledValues[k]) > 0) then
        begin
          n := TempEnabledValues.IndexOf(TempDisabledValues[k]);
          if (n >= 0) then TempEnabledValues.Delete(n);
        end;

      inc(k);
    end;

  if (TempEnabledValues.Count > 0) then
    aValues[i] := TempEnabledValues.CommaText
   else
    begin
      aKeys.Delete(i);
      aValues.Delete(i);
    end;

  FreeAndNil(TempEnabledValues);
  FreeAndNil(TempDisabledValues);
end;

procedure TCefApplicationCore.ReplaceSwitch(var aKeys, aValues : TStringList; const aNewKey, aNewValue : ustring);
var
  TempKey, TempHyphenatedKey : ustring;
  i : integer;
begin
  if (copy(aNewKey, 1, 2) = '--') then
    begin
      TempHyphenatedKey := aNewKey;
      TempKey           := copy(aNewKey, 3, length(aNewKey));
    end
   else
    begin
      TempHyphenatedKey := '--' + aNewKey;
      TempKey           := aNewKey;
    end;

  i := aKeys.IndexOf(TempKey);

  if (i < 0) then
    begin
      i := aKeys.IndexOf(TempHyphenatedKey);

      if (i < 0) then
        begin
          aKeys.Add(aNewKey);
          aValues.Add(aNewValue);
        end
       else
        aValues[i] := aNewValue;
    end
   else
    aValues[i] := aNewValue;
end;

procedure TCefApplicationCore.AddCustomCommandLineSwitches(var aKeys, aValues : TStringList);
var
  i : integer;
  {$IFDEF VER140}
  TempDecimalSeparator : char;  // Only for Delphi 6
  {$ELSE}
  TempFormatSettings : TFormatSettings;
  {$ENDIF}
begin
  if FEnableMediaStream then
    ReplaceSwitch(aKeys, aValues, '--enable-media-stream');

  if FEnableSpeechInput then
    ReplaceSwitch(aKeys, aValues, '--enable-speech-input');

  if FUseFakeUIForMediaStream then
    ReplaceSwitch(aKeys, aValues, '--use-fake-ui-for-media-stream');

  if FEnableUsermediaScreenCapturing then
    ReplaceSwitch(aKeys, aValues, '--enable-usermedia-screen-capturing');

  if not(FEnableGPU) then
    begin
      ReplaceSwitch(aKeys, aValues, '--disable-gpu');
      ReplaceSwitch(aKeys, aValues, '--disable-gpu-compositing');
    end;

  if FSingleProcess then
    ReplaceSwitch(aKeys, aValues, '--single-process');

  case FSmoothScrolling of
    STATE_ENABLED  : ReplaceSwitch(aKeys, aValues, '--enable-smooth-scrolling');
    STATE_DISABLED : ReplaceSwitch(aKeys, aValues, '--disable-smooth-scrolling');
  end;

  case FTouchEvents of
    STATE_ENABLED  : ReplaceSwitch(aKeys, aValues, '--touch-events', 'enabled');
    STATE_DISABLED : ReplaceSwitch(aKeys, aValues, '--touch-events', 'disabled');
  end;

  if FDisableReadingFromCanvas then
    ReplaceSwitch(aKeys, aValues, '--disable-reading-from-canvas');

  if not(FHyperlinkAuditing) then
    ReplaceSwitch(aKeys, aValues, '--no-pings');

  case FAutoplayPolicy of
    appDocumentUserActivationRequired    :
      ReplaceSwitch(aKeys, aValues, '--autoplay-policy', 'document-user-activation-required');

    appNoUserGestureRequired             :
      ReplaceSwitch(aKeys, aValues, '--autoplay-policy', 'no-user-gesture-required');

    appUserGestureRequired               :
      ReplaceSwitch(aKeys, aValues, '--autoplay-policy', 'user-gesture-required');
  end;

  if FDisableGPUCache then
    ReplaceSwitch(aKeys, aValues, '--disable-gpu-shader-disk-cache');

  if FMuteAudio then
    ReplaceSwitch(aKeys, aValues, '--mute-audio');

  if FDisableWebSecurity then
    ReplaceSwitch(aKeys, aValues, '--disable-web-security');

  if FDisablePDFExtension then
    ReplaceSwitch(aKeys, aValues, '--disable-pdf-extension');

  if FDisableSiteIsolationTrials then
    ReplaceSwitch(aKeys, aValues, '--disable-site-isolation-trials');

  if FDisableChromeLoginPrompt then
    ReplaceSwitch(aKeys, aValues, '--disable-chrome-login-prompt');

  if FSitePerProcess then
    ReplaceSwitch(aKeys, aValues, '--site-per-process');

  if FDisableExtensions then
    ReplaceSwitch(aKeys, aValues, '--disable-extensions');

  if FDisableBackgroundNetworking then
    ReplaceSwitch(aKeys, aValues, '--disable-background-networking');

  if FMetricsRecordingOnly then
    ReplaceSwitch(aKeys, aValues, '--metrics-recording-only');

  if FAllowFileAccessFromFiles then
    ReplaceSwitch(aKeys, aValues, '--allow-file-access-from-files');

  if FAllowRunningInsecureContent then
    ReplaceSwitch(aKeys, aValues, '--allow-running-insecure-content');

  if FKioskPrinting then
    ReplaceSwitch(aKeys, aValues, '--kiosk-printing');

  if FEnablePrintPreview then
    ReplaceSwitch(aKeys, aValues, '--enable-print-preview');

  if FDisableNewBrowserInfoTimeout then
    ReplaceSwitch(aKeys, aValues, '--disable-new-browser-info-timeout');

  if (length(FDevToolsProtocolLogFile) > 0) then
    ReplaceSwitch(aKeys, aValues, '--devtools-protocol-log-file', FDevToolsProtocolLogFile);

  if (length(FDefaultEncoding) > 0) then
    ReplaceSwitch(aKeys, aValues, '--default-encoding', FDefaultEncoding);

  if FDisableJavascript then
    ReplaceSwitch(aKeys, aValues, '--disable-javascript');

  if FDisableJavascriptCloseWindows then
    ReplaceSwitch(aKeys, aValues, '--disable-javascript-close-windows');

  if FDisableJavascriptAccessClipboard then
    ReplaceSwitch(aKeys, aValues, '--disable-javascript-access-clipboard');

  if FDisableJavascriptDomPaste then
    ReplaceSwitch(aKeys, aValues, '--disable-javascript-dom-paste');

  if FAllowUniversalAccessFromFileUrls then
    ReplaceSwitch(aKeys, aValues, '--allow-universal-access-from-files');

  if FDisableImageLoading then
    ReplaceSwitch(aKeys, aValues, '--disable-image-loading');

  if FImageShrinkStandaloneToFit then
    ReplaceSwitch(aKeys, aValues, '--image-shrink-standalone-to-fit');

  if FDisableTextAreaResize then
    ReplaceSwitch(aKeys, aValues, '--disable-text-area-resize');

  if FDisableTabToLinks then
    ReplaceSwitch(aKeys, aValues, '--disable-tab-to-links');

  if FEnableProfanityFilter then
    ReplaceSwitch(aKeys, aValues, '--enable-profanity-filter');

  if FDisableSpellChecking then
    ReplaceSwitch(aKeys, aValues, '--disable-spell-checking');

  if (length(FOverrideSpellCheckLang) > 0) then
    ReplaceSwitch(aKeys, aValues, '--override-spell-check-lang', FOverrideSpellCheckLang);

  if FIgnoreCertificateErrors then
    ReplaceSwitch(aKeys, aValues, '--ignore-certificate-errors');

  if (FForcedDeviceScaleFactor <> 0) then
    begin
      {$IFDEF FPC}
      TempFormatSettings.DecimalSeparator := '.';
      ReplaceSwitch(aKeys, aValues, '--force-device-scale-factor', FloatToStr(FForcedDeviceScaleFactor, TempFormatSettings));
      {$ELSE}
        {$IFDEF DELPHI7_UP}
          {$IFDEF DELPHI24_UP}
          TempFormatSettings := TFormatSettings.Create('en-US');
          {$ELSE}
          GetLocaleFormatSettings(GetThreadLocale, TempFormatSettings);
          TempFormatSettings.DecimalSeparator := '.';
          {$ENDIF}
          ReplaceSwitch(aKeys, aValues, '--force-device-scale-factor', FloatToStr(FForcedDeviceScaleFactor, TempFormatSettings));
        {$ELSE}
          TempDecimalSeparator := DecimalSeparator;
          DecimalSeparator     := '.';
          ReplaceSwitch(aKeys, aValues, '--force-device-scale-factor', FloatToStr(FForcedDeviceScaleFactor));
          DecimalSeparator     := TempDecimalSeparator;
        {$ENDIF}
      {$ENDIF}
    end;

  if FDisableZygote then
    ReplaceSwitch(aKeys, aValues, '--no-zygote');

  if FUseMockKeyChain then
    ReplaceSwitch(aKeys, aValues, '--use-mock-keychain');

  if FDisableRequestHandlingForTesting then
    ReplaceSwitch(aKeys, aValues, '--disable-request-handling-for-testing');

  if FDisablePopupBlocking then
    ReplaceSwitch(aKeys, aValues, '--disable-popup-blocking');

  if FDisableBackForwardCache then
    ReplaceSwitch(aKeys, aValues, '--disable-back-forward-cache');

  if FDisableComponentUpdate then
    ReplaceSwitch(aKeys, aValues, '--disable-component-update');

  if FAllowInsecureLocalhost then
    ReplaceSwitch(aKeys, aValues, '--allow-insecure-localhost');

  if (length(FTreatInsecureOriginAsSecure) > 0) then
    ReplaceSwitch(aKeys, aValues, '--unsafely-treat-insecure-origin-as-secure', FTreatInsecureOriginAsSecure);

  if (length(FRemoteAllowOrigins) > 0) then
    ReplaceSwitch(aKeys, aValues, '--remote-allow-origins', FRemoteAllowOrigins);

  if FAutoAcceptCamAndMicCapture then
    ReplaceSwitch(aKeys, aValues, '--auto-accept-camera-and-microphone-capture');

  case FUIColorMode of
    uicmForceDark  : ReplaceSwitch(aKeys, aValues, '--force-dark-mode');
    uicmForceLight : ReplaceSwitch(aKeys, aValues, '--force-light-mode');
  end;

  if FDisableHangMonitor then
    ReplaceSwitch(aKeys, aValues, '--disable-hang-monitor');

  if FHideCrashRestoreBubble then
    ReplaceSwitch(aKeys, aValues, '--hide-crash-restore-bubble');

  if FNetLogEnabled then
    begin
      ReplaceSwitch(aKeys, aValues, '--log-net-log', FNetLogFile);

      case FNetLogCaptureMode of
        nlcmDefault          : ReplaceSwitch(aKeys, aValues, '--net-log-capture-mode', 'Default');
        nlcmIncludeSensitive : ReplaceSwitch(aKeys, aValues, '--net-log-capture-mode', 'IncludeSensitive');
        nlcmEverything       : ReplaceSwitch(aKeys, aValues, '--net-log-capture-mode', 'Everything');
      end;
    end;

  case FPostQuantumKyber of
    STATE_ENABLED  : ReplaceSwitch(aKeys, aValues, '--enable-features',  'PostQuantumKyber');
    STATE_DISABLED : ReplaceSwitch(aKeys, aValues, '--disable-features', 'PostQuantumKyber');
  end;

  // The list of features you can enable is here :
  // https://chromium.googlesource.com/chromium/src/+/master/chrome/common/chrome_features.cc
  // https://source.chromium.org/chromium/chromium/src/+/main:content/public/common/content_features.cc
  // https://source.chromium.org/search?q=base::Feature
  if (length(FEnableFeatures) > 0) then
    AppendSwitch(aKeys, aValues, '--enable-features', FEnableFeatures);

  // The list of features you can disable is here :
  // https://chromium.googlesource.com/chromium/src/+/master/chrome/common/chrome_features.cc
  // https://source.chromium.org/chromium/chromium/src/+/main:content/public/common/content_features.cc
  // https://source.chromium.org/search?q=base::Feature
  if (length(FDisableFeatures) > 0) then
    AppendSwitch(aKeys, aValues, '--disable-features', FDisableFeatures);

  CleanupFeatures(aKeys, aValues, '--enable-features', '--disable-features');

  // The list of Blink features you can enable is here :
  // https://cs.chromium.org/chromium/src/third_party/blink/renderer/platform/runtime_enabled_features.json5
  if (length(FEnableBlinkFeatures) > 0) then
    AppendSwitch(aKeys, aValues, '--enable-blink-features', FEnableBlinkFeatures);

  // The list of Blink features you can disable is here :
  // https://cs.chromium.org/chromium/src/third_party/blink/renderer/platform/runtime_enabled_features.json5
  if (length(FDisableBlinkFeatures) > 0) then
    AppendSwitch(aKeys, aValues, '--disable-blink-features', FDisableBlinkFeatures);

  CleanupFeatures(aKeys, aValues, '--enable-blink-features', '--disable-blink-features');

  // The list of Blink settings you can modify is here :
  // https://source.chromium.org/chromium/chromium/src/+/master:third_party/blink/renderer/core/frame/settings.json5
  if (length(FBlinkSettings) > 0) then
    ReplaceSwitch(aKeys, aValues, '--blink-settings', FBlinkSettings);

  // https://source.chromium.org/chromium/chromium/src/+/master:base/base_switches.cc
  if (length(FForceFieldTrials) > 0) then
    ReplaceSwitch(aKeys, aValues, '--force-fieldtrials', FForceFieldTrials);

  // https://source.chromium.org/chromium/chromium/src/+/master:components/variations/variations_switches.cc
  if (length(FForceFieldTrialParams) > 0) then
    ReplaceSwitch(aKeys, aValues, '--force-fieldtrial-params', FForceFieldTrialParams);

  if (FCustomCommandLines       <> nil) and
     (FCustomCommandLineValues  <> nil) and
     (FCustomCommandLines.Count =  FCustomCommandLineValues.Count) then
    begin
      i := 0;
      while (i < FCustomCommandLines.Count) do
        begin
          if (length(FCustomCommandLines[i]) > 0) then
            ReplaceSwitch(aKeys, aValues, FCustomCommandLines[i], FCustomCommandLineValues[i]);

          inc(i);
        end;
    end;
end;

function TCefApplicationCore.GetMustCreateResourceBundleHandler : boolean;
begin
  Result := ((FSingleProcess or (FProcessType in [ptBrowser, ptRenderer, ptZygote])) and
             (FMustCreateResourceBundleHandler or
              assigned(FOnGetLocalizedString)  or
              assigned(FOnGetDataResource)     or
              assigned(FOnGetDataResourceForScale)));
end;

function TCefApplicationCore.GetMustCreateBrowserProcessHandler : boolean;
begin
  Result := ((FSingleProcess or (FProcessType = ptBrowser)) and
             (FMustCreateBrowserProcessHandler        or
              assigned(FOnContextInitialized)         or
              assigned(FOnBeforeChildProcessLaunch)   or
              assigned(FOnAlreadyRunningAppRelaunch)  or
              assigned(FOnScheduleMessagePumpWork)    or
              assigned(FOnGetDefaultClient)           or
              assigned(FOnGetDefaultRequestContextHandler)));
end;

function TCefApplicationCore.GetMustCreateRenderProcessHandler : boolean;
begin
  Result := ((FSingleProcess or (FProcessType in [ptRenderer, ptZygote])) and
             (FMustCreateRenderProcessHandler     or
              MustCreateLoadHandler               or
              assigned(FOnWebKitInitialized)      or
              assigned(FOnBrowserCreated)         or
              assigned(FOnBrowserDestroyed)       or
              assigned(FOnContextCreated)         or
              assigned(FOnContextReleased)        or
              assigned(FOnUncaughtException)      or
              assigned(FOnFocusedNodeChanged)     or
              assigned(FOnProcessMessageReceived)));
end;

function TCefApplicationCore.GetMustCreateLoadHandler : boolean;
begin
  Result := ((FSingleProcess or (FProcessType in [ptRenderer, ptZygote])) and
             (FMustCreateLoadHandler          or
              assigned(FOnLoadingStateChange) or
              assigned(FOnLoadStart)          or
              assigned(FOnLoadEnd)            or
              assigned(FOnLoadError)));
end;

function TCefApplicationCore.GetGlobalContextInitialized : boolean;
begin
  Result := FGlobalContextInitialized or not(MustCreateBrowserProcessHandler);
end;

function TCefApplicationCore.GetChildProcessesCount : integer;
{$IFDEF MSWINDOWS}
var
  TempHandle  : THandle;
  TempProcess : TProcessEntry32;
  TempPID     : DWORD;
  TempMain, TempSubProc, TempName : string;
{$ENDIF}
begin
  Result := 0;

{$IFDEF MSWINDOWS}
  TempHandle := CreateToolHelp32SnapShot(TH32CS_SNAPPROCESS, 0);
  if (TempHandle = INVALID_HANDLE_VALUE) then exit;

  ZeroMemory(@TempProcess, SizeOf(TProcessEntry32));
  TempProcess.dwSize := Sizeof(TProcessEntry32);
  TempPID            := GetCurrentProcessID;
  TempMain           := ExtractFileName(paramstr(0));
  TempSubProc        := ExtractFileName(FBrowserSubprocessPath);

  Process32First(TempHandle, TempProcess);

  repeat
    if (TempProcess.th32ProcessID       <> TempPID) and
       (TempProcess.th32ParentProcessID =  TempPID) then
      begin
        TempName := TempProcess.szExeFile;
        TempName := ExtractFileName(TempName);

        if (CompareText(TempName, TempMain) = 0) or
           ((length(TempSubProc) > 0) and (CompareText(TempName, TempSubProc) = 0)) then
          inc(Result);
      end;
  until not(Process32Next(TempHandle, TempProcess));

  CloseHandle(TempHandle);
{$ENDIF}
end;

function TCefApplicationCore.GetUsedMemory : uint64;
{$IFDEF MSWINDOWS}
var
  TempHandle   : THandle;
  TempProcess  : TProcessEntry32;
  TempPID      : DWORD;
  TempProcHWND : HWND;
  TempMemCtrs  : TProcessMemoryCounters;
  TempMain, TempSubProc, TempName : string;
{$ENDIF}
begin
  Result := 0;
  {$IFDEF MSWINDOWS}
  TempHandle := CreateToolHelp32SnapShot(TH32CS_SNAPPROCESS, 0);
  if (TempHandle = INVALID_HANDLE_VALUE) then exit;

  ZeroMemory(@TempProcess, SizeOf(TProcessEntry32));
  TempProcess.dwSize := Sizeof(TProcessEntry32);
  TempPID            := GetCurrentProcessID;
  TempMain           := ExtractFileName(paramstr(0));
  TempSubProc        := ExtractFileName(FBrowserSubprocessPath);

  Process32First(TempHandle, TempProcess);

  repeat
    if (TempProcess.th32ProcessID       = TempPID) or
       (TempProcess.th32ParentProcessID = TempPID) then
      begin
        TempName := TempProcess.szExeFile;
        TempName := ExtractFileName(TempName);

        if (CompareText(TempName, TempMain) = 0) or
           ((length(TempSubProc) > 0) and (CompareText(TempName, TempSubProc) = 0)) then
          begin
            TempProcHWND := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, TempProcess.th32ProcessID);

            if (TempProcHWND <> 0) then
              try
                ZeroMemory(@TempMemCtrs, SizeOf(TProcessMemoryCounters));
                TempMemCtrs.cb := SizeOf(TProcessMemoryCounters);

                if GetProcessMemoryInfo(TempProcHWND, {$IFNDEF FPC}@{$ENDIF}TempMemCtrs, TempMemCtrs.cb) then
                  inc(Result, TempMemCtrs.WorkingSetSize);
              finally
                CloseHandle(TempProcHWND);
              end;
          end;
      end;
  until not(Process32Next(TempHandle, TempProcess));

  CloseHandle(TempHandle);
  {$ENDIF}
end;

function TCefApplicationCore.GetTotalSystemMemory : uint64;
{$IFDEF MSWINDOWS}
var
  TempMemStatus : TMyMemoryStatusEx;
{$ENDIF}
begin
  Result := 0;
  {$IFDEF MSWINDOWS}
  ZeroMemory(@TempMemStatus, SizeOf(TMyMemoryStatusEx));
  TempMemStatus.dwLength := SizeOf(TMyMemoryStatusEx);
  if GetGlobalMemoryStatusEx(@TempMemStatus) then
    Result := TempMemStatus.ullTotalPhys;
  {$ENDIF}
end;

function TCefApplicationCore.GetAvailableSystemMemory : uint64;
{$IFDEF MSWINDOWS}
var
  TempMemStatus : TMyMemoryStatusEx;
{$ENDIF}
begin
  Result := 0;
  {$IFDEF MSWINDOWS}
  ZeroMemory(@TempMemStatus, SizeOf(TMyMemoryStatusEx));
  TempMemStatus.dwLength := SizeOf(TMyMemoryStatusEx);
  if GetGlobalMemoryStatusEx(@TempMemStatus) then
    Result := TempMemStatus.ullAvailPhys;
  {$ENDIF}
end;

function TCefApplicationCore.GetSystemMemoryLoad : cardinal;
{$IFDEF MSWINDOWS}
var
  TempMemStatus : TMyMemoryStatusEx;
{$ENDIF}
begin
  Result := 0;

  {$IFDEF MSWINDOWS}
  ZeroMemory(@TempMemStatus, SizeOf(TMyMemoryStatusEx));
  TempMemStatus.dwLength := SizeOf(TMyMemoryStatusEx);
  if GetGlobalMemoryStatusEx(@TempMemStatus) then
    Result := TempMemStatus.dwMemoryLoad;
  {$ENDIF}
end;

function TCefApplicationCore.GetApiHashUniversal : ustring;
var
  TempHash : PAnsiChar;
begin
  Result := '';
  if not(FLibLoaded) then exit;

  TempHash := cef_api_hash(CEF_API_HASH_UNIVERSAL);

  if (TempHash <> nil) then
    Result := ustring(AnsiString(TempHash));
end;

function TCefApplicationCore.GetApiHashPlatform : ustring;
var
  TempHash : PAnsiChar;
begin
  Result := '';
  if not(FLibLoaded) then exit;

  TempHash := cef_api_hash(CEF_API_HASH_PLATFORM);

  if (TempHash <> nil) then
    Result := ustring(AnsiString(TempHash));
end;

function TCefApplicationCore.GetApiHashCommit : ustring;
var
  TempHash : PAnsiChar;
begin
  Result := '';
  if not(FLibLoaded) then exit;

  TempHash := cef_api_hash(CEF_COMMIT_HASH);

  if (TempHash <> nil) then
    Result := ustring(AnsiString(TempHash));
end;

function TCefApplicationCore.GetExitCode : TCefResultCode;
begin
  if FLibLoaded then
    Result := cef_get_exit_code()
   else
    Result := CEF_RESULT_CODE_NORMAL_EXIT;
end;

{$IFDEF LINUX}
function TCefApplicationCore.GetXDisplay : PXDisplay;
begin
  // This property can only be called in the CEF UI thread.
  if FLibLoaded then
    Result := cef_get_xdisplay{$IFDEF FPC}(){$ENDIF}
   else
    Result := nil;
end;

function TCefApplicationCore.GetArgc : longint;
begin
  if (FArgCopy <> nil) then
    Result := FArgCopy.argc
   else
    Result := 0;
end;

function TCefApplicationCore.GetArgv : PPAnsiChar;
begin
  if (FArgCopy <> nil) then
    Result := FArgCopy.argv
   else
    Result := nil;
end;
{$ENDIF}

function TCefApplicationCore.LoadCEFlibrary : boolean;
var
  TempOldDir : string;
  TempError  : {$IFDEF MSWINDOWS}DWORD;{$ELSE}Integer;{$ENDIF}
begin
  Result := False;

  if (FStatus <> asLoading) or FLibLoaded or (FLibHandle <> 0) then
    begin
      FStatus           := asErrorLoadingLibrary;
      FLastErrorMessage := 'GlobalCEFApp can only be initialized once per process.';

      ShowErrorMessageDlg(FLastErrorMessage);
      exit;
    end;

  if FSetCurrentDir then
    begin
      TempOldDir := GetCurrentDir;
      chdir(GetModulePath);
    end;

  {$IFDEF MSWINDOWS}
  FLibHandle := LoadLibraryExW(PWideChar(LibCefPath), 0, LOAD_WITH_ALTERED_SEARCH_PATH);
  {$ELSE}
    {$IFDEF FPC}
    FLibHandle := LoadLibrary(LibCefPath);
    {$ELSE}
    FLibHandle := LoadLibrary(PChar(LibCefPath));
    {$ENDIF}
  {$ENDIF}

  if (FLibHandle = 0) then
    begin
      FStatus := asErrorLoadingLibrary;

      {$IFDEF MSWINDOWS}
      TempError         := GetLastError;
      FLastErrorMessage := 'Error loading ' + LIBCEF_DLL + CRLF + CRLF +
                           'Error code : 0x' + inttohex(TempError, 8) + CRLF +
                           SysErrorMessage(TempError);
      {$ELSE}
        {$IFDEF FPC}
        TempError         := GetLastOSError;
        FLastErrorMessage := 'Error loading ' + LIBCEF_DLL + CRLF + CRLF +
                             'Error code : 0x' + inttohex(TempError, 8) + CRLF +
                             UTF8Decode(GetLoadErrorStr);
        {$ELSE}
        FLastErrorMessage := 'Error loading ' + LIBCEF_DLL;
        {$ENDIF}
      {$ENDIF}

      ShowErrorMessageDlg(FLastErrorMessage);
      exit;
    end;


  if Load_cef_api_hash_h and
     Load_cef_app_capi_h and
     Load_cef_app_win_h and
     Load_cef_browser_capi_h and
     Load_cef_command_line_capi_h and
     Load_cef_cookie_capi_h and
     Load_cef_crash_util_h and
     Load_cef_drag_data_capi_h and
     Load_cef_dump_without_crashing_internal_h and
     Load_cef_file_util_capi_h and
     Load_cef_i18n_util_capi_h and
     Load_cef_image_capi_h and
     Load_cef_menu_model_capi_h and
     Load_cef_media_router_capi_h and
     Load_cef_origin_whitelist_capi_h and
     Load_cef_parser_capi_h and
     Load_cef_path_util_capi_h and
     Load_cef_preference_capi_h and
     Load_cef_print_settings_capi_h and
     Load_cef_process_message_capi_h and
     Load_cef_process_util_capi_h and
     Load_cef_request_capi_h and
     Load_cef_request_context_capi_h and
     Load_cef_resource_bundle_capi_h and
     Load_cef_response_capi_h and
     Load_cef_scheme_capi_h and
     Load_cef_server_capi_h and
     Load_cef_shared_process_message_builder_capi_h and
     Load_cef_ssl_info_capi_h and
     Load_cef_stream_capi_h and
     Load_cef_task_capi_h and
     Load_cef_task_manager_capi_h and
     Load_cef_thread_capi_h and
     Load_cef_trace_capi_h and
     Load_cef_urlrequest_capi_h and
     Load_cef_v8_capi_h and
     Load_cef_values_capi_h and
     Load_cef_waitable_event_capi_h and
     Load_cef_xml_reader_capi_h and
     Load_cef_zip_reader_capi_h and
     Load_cef_logging_internal_h and
     Load_cef_string_list_h and
     Load_cef_string_map_h and
     Load_cef_string_multimap_h and
     Load_cef_string_types_h and
     Load_cef_thread_internal_h and
     Load_cef_trace_event_internal_h and
     Load_cef_browser_view_capi_h and
     Load_cef_display_capi_h and
     Load_cef_label_button_capi_h and
     Load_cef_menu_button_capi_h and
     Load_cef_panel_capi_h and
     Load_cef_scroll_view_capi_h and
     Load_cef_textfield_capi_h and
     Load_cef_window_capi_h and
     Load_cef_types_linux_h and
     Load_cef_time_h then
    begin
      FStatus    := asLoaded;
      FLibLoaded := True;
      Result     := True;

      if FLogProcessInfo then CefDebugLog('Process started', CEF_LOG_SEVERITY_INFO);
    end
   else
    begin
      FStatus           := asErrorDLLVersion;
      FLastErrorMessage := 'Unsupported CEF version !' +
                           CRLF + CRLF +
                           'Use only the CEF binaries specified in the CEF4Delphi Readme.md file at ' +
                           CRLF + CEF4DELPHI_URL;

      ShowErrorMessageDlg(FLastErrorMessage);
    end;

  if FSetCurrentDir then chdir(TempOldDir);
end;

function TCefApplicationCore.Load_cef_api_hash_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_api_hash{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_api_hash');

  Result := assigned(cef_api_hash);
end;

function TCefApplicationCore.Load_cef_app_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_initialize{$IFDEF FPC}){$ENDIF}             := GetProcAddress(FLibHandle, 'cef_initialize');
  {$IFDEF FPC}Pointer({$ENDIF}cef_get_exit_code{$IFDEF FPC}){$ENDIF}          := GetProcAddress(FLibHandle, 'cef_get_exit_code');
  {$IFDEF FPC}Pointer({$ENDIF}cef_shutdown{$IFDEF FPC}){$ENDIF}               := GetProcAddress(FLibHandle, 'cef_shutdown');
  {$IFDEF FPC}Pointer({$ENDIF}cef_execute_process{$IFDEF FPC}){$ENDIF}        := GetProcAddress(FLibHandle, 'cef_execute_process');
  {$IFDEF FPC}Pointer({$ENDIF}cef_do_message_loop_work{$IFDEF FPC}){$ENDIF}   := GetProcAddress(FLibHandle, 'cef_do_message_loop_work');
  {$IFDEF FPC}Pointer({$ENDIF}cef_run_message_loop{$IFDEF FPC}){$ENDIF}       := GetProcAddress(FLibHandle, 'cef_run_message_loop');
  {$IFDEF FPC}Pointer({$ENDIF}cef_quit_message_loop{$IFDEF FPC}){$ENDIF}      := GetProcAddress(FLibHandle, 'cef_quit_message_loop');

  Result := assigned(cef_initialize) and
            assigned(cef_get_exit_code) and
            assigned(cef_shutdown) and
            assigned(cef_execute_process) and
            assigned(cef_do_message_loop_work) and
            assigned(cef_run_message_loop) and
            assigned(cef_quit_message_loop);
end;

function TCefApplicationCore.Load_cef_app_win_h : boolean;
begin
  {$IFDEF MSWINDOWS}
    {$IFDEF FPC}Pointer({$ENDIF}cef_set_osmodal_loop{$IFDEF FPC}){$ENDIF}       := GetProcAddress(FLibHandle, 'cef_set_osmodal_loop');

    Result := assigned(cef_set_osmodal_loop);
  {$ELSE}
    Result := True;
  {$ENDIF}
end;

function TCefApplicationCore.Load_cef_browser_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_browser_host_create_browser{$IFDEF FPC}){$ENDIF}      := GetProcAddress(FLibHandle, 'cef_browser_host_create_browser');
  {$IFDEF FPC}Pointer({$ENDIF}cef_browser_host_create_browser_sync{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_browser_host_create_browser_sync');

  Result := assigned(cef_browser_host_create_browser) and
            assigned(cef_browser_host_create_browser_sync);
end;

function TCefApplicationCore.Load_cef_command_line_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_command_line_create{$IFDEF FPC}){$ENDIF}     := GetProcAddress(FLibHandle, 'cef_command_line_create');
  {$IFDEF FPC}Pointer({$ENDIF}cef_command_line_get_global{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_command_line_get_global');

  Result := assigned(cef_command_line_create) and
            assigned(cef_command_line_get_global);
end;

function TCefApplicationCore.Load_cef_cookie_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_cookie_manager_get_global_manager{$IFDEF FPC}){$ENDIF}   := GetProcAddress(FLibHandle, 'cef_cookie_manager_get_global_manager');

  Result := assigned(cef_cookie_manager_get_global_manager);
end;

function TCefApplicationCore.Load_cef_crash_util_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_crash_reporting_enabled{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_crash_reporting_enabled');
  {$IFDEF FPC}Pointer({$ENDIF}cef_set_crash_key_value{$IFDEF FPC}){$ENDIF}     := GetProcAddress(FLibHandle, 'cef_set_crash_key_value');

  Result := assigned(cef_crash_reporting_enabled) and
            assigned(cef_set_crash_key_value);
end;

function TCefApplicationCore.Load_cef_drag_data_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_drag_data_create{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_drag_data_create');

  Result := assigned(cef_drag_data_create);
end;

function TCefApplicationCore.Load_cef_dump_without_crashing_internal_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_dump_without_crashing{$IFDEF FPC}){$ENDIF}             := GetProcAddress(FLibHandle, 'cef_dump_without_crashing');
  {$IFDEF FPC}Pointer({$ENDIF}cef_dump_without_crashing_unthrottled{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_dump_without_crashing_unthrottled');

  Result := assigned(cef_dump_without_crashing) and
            assigned(cef_dump_without_crashing_unthrottled);
end;

function TCefApplicationCore.Load_cef_file_util_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_create_directory{$IFDEF FPC}){$ENDIF}                   := GetProcAddress(FLibHandle, 'cef_create_directory');
  {$IFDEF FPC}Pointer({$ENDIF}cef_get_temp_directory{$IFDEF FPC}){$ENDIF}                 := GetProcAddress(FLibHandle, 'cef_get_temp_directory');
  {$IFDEF FPC}Pointer({$ENDIF}cef_create_new_temp_directory{$IFDEF FPC}){$ENDIF}          := GetProcAddress(FLibHandle, 'cef_create_new_temp_directory');
  {$IFDEF FPC}Pointer({$ENDIF}cef_create_temp_directory_in_directory{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_create_temp_directory_in_directory');
  {$IFDEF FPC}Pointer({$ENDIF}cef_directory_exists{$IFDEF FPC}){$ENDIF}                   := GetProcAddress(FLibHandle, 'cef_directory_exists');
  {$IFDEF FPC}Pointer({$ENDIF}cef_delete_file{$IFDEF FPC}){$ENDIF}                        := GetProcAddress(FLibHandle, 'cef_delete_file');
  {$IFDEF FPC}Pointer({$ENDIF}cef_zip_directory{$IFDEF FPC}){$ENDIF}                      := GetProcAddress(FLibHandle, 'cef_zip_directory');
  {$IFDEF FPC}Pointer({$ENDIF}cef_load_crlsets_file{$IFDEF FPC}){$ENDIF}                  := GetProcAddress(FLibHandle, 'cef_load_crlsets_file');

  Result := assigned(cef_create_directory) and
            assigned(cef_get_temp_directory) and
            assigned(cef_create_new_temp_directory) and
            assigned(cef_create_temp_directory_in_directory) and
            assigned(cef_directory_exists) and
            assigned(cef_delete_file) and
            assigned(cef_zip_directory) and
            assigned(cef_load_crlsets_file);
end;

function TCefApplicationCore.Load_cef_i18n_util_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_is_rtl{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_is_rtl');

  Result := assigned(cef_is_rtl);
end;

function TCefApplicationCore.Load_cef_image_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_image_create{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_image_create');

  Result := assigned(cef_image_create);
end;

function TCefApplicationCore.Load_cef_menu_model_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_menu_model_create{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_menu_model_create');

  Result := assigned(cef_menu_model_create);
end;

function TCefApplicationCore.Load_cef_media_router_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_media_router_get_global{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_media_router_get_global');

  Result := assigned(cef_media_router_get_global);
end;

function TCefApplicationCore.Load_cef_origin_whitelist_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_add_cross_origin_whitelist_entry{$IFDEF FPC}){$ENDIF}    := GetProcAddress(FLibHandle, 'cef_add_cross_origin_whitelist_entry');
  {$IFDEF FPC}Pointer({$ENDIF}cef_remove_cross_origin_whitelist_entry{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_remove_cross_origin_whitelist_entry');
  {$IFDEF FPC}Pointer({$ENDIF}cef_clear_cross_origin_whitelist{$IFDEF FPC}){$ENDIF}        := GetProcAddress(FLibHandle, 'cef_clear_cross_origin_whitelist');

  Result := assigned(cef_add_cross_origin_whitelist_entry) and
            assigned(cef_remove_cross_origin_whitelist_entry) and
            assigned(cef_clear_cross_origin_whitelist);
end;

function TCefApplicationCore.Load_cef_parser_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_resolve_url{$IFDEF FPC}){$ENDIF}                     := GetProcAddress(FLibHandle, 'cef_resolve_url');
  {$IFDEF FPC}Pointer({$ENDIF}cef_parse_url{$IFDEF FPC}){$ENDIF}                       := GetProcAddress(FLibHandle, 'cef_parse_url');
  {$IFDEF FPC}Pointer({$ENDIF}cef_create_url{$IFDEF FPC}){$ENDIF}                      := GetProcAddress(FLibHandle, 'cef_create_url');
  {$IFDEF FPC}Pointer({$ENDIF}cef_format_url_for_security_display{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_format_url_for_security_display');
  {$IFDEF FPC}Pointer({$ENDIF}cef_get_mime_type{$IFDEF FPC}){$ENDIF}                   := GetProcAddress(FLibHandle, 'cef_get_mime_type');
  {$IFDEF FPC}Pointer({$ENDIF}cef_get_extensions_for_mime_type{$IFDEF FPC}){$ENDIF}    := GetProcAddress(FLibHandle, 'cef_get_extensions_for_mime_type');
  {$IFDEF FPC}Pointer({$ENDIF}cef_base64encode{$IFDEF FPC}){$ENDIF}                    := GetProcAddress(FLibHandle, 'cef_base64encode');
  {$IFDEF FPC}Pointer({$ENDIF}cef_base64decode{$IFDEF FPC}){$ENDIF}                    := GetProcAddress(FLibHandle, 'cef_base64decode');
  {$IFDEF FPC}Pointer({$ENDIF}cef_uriencode{$IFDEF FPC}){$ENDIF}                       := GetProcAddress(FLibHandle, 'cef_uriencode');
  {$IFDEF FPC}Pointer({$ENDIF}cef_uridecode{$IFDEF FPC}){$ENDIF}                       := GetProcAddress(FLibHandle, 'cef_uridecode');
  {$IFDEF FPC}Pointer({$ENDIF}cef_parse_json{$IFDEF FPC}){$ENDIF}                      := GetProcAddress(FLibHandle, 'cef_parse_json');
  {$IFDEF FPC}Pointer({$ENDIF}cef_parse_json_buffer{$IFDEF FPC}){$ENDIF}               := GetProcAddress(FLibHandle, 'cef_parse_json_buffer');
  {$IFDEF FPC}Pointer({$ENDIF}cef_parse_jsonand_return_error{$IFDEF FPC}){$ENDIF}      := GetProcAddress(FLibHandle, 'cef_parse_jsonand_return_error');
  {$IFDEF FPC}Pointer({$ENDIF}cef_write_json{$IFDEF FPC}){$ENDIF}                      := GetProcAddress(FLibHandle, 'cef_write_json');

  Result := assigned(cef_resolve_url) and
            assigned(cef_parse_url) and
            assigned(cef_create_url) and
            assigned(cef_format_url_for_security_display) and
            assigned(cef_get_mime_type) and
            assigned(cef_get_extensions_for_mime_type) and
            assigned(cef_base64encode) and
            assigned(cef_base64decode) and
            assigned(cef_uriencode) and
            assigned(cef_uridecode) and
            assigned(cef_parse_json) and
            assigned(cef_parse_json_buffer) and
            assigned(cef_parse_jsonand_return_error) and
            assigned(cef_write_json);
end;

function TCefApplicationCore.Load_cef_path_util_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_get_path{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_get_path');

  Result := assigned(cef_get_path);
end;

function TCefApplicationCore.Load_cef_preference_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_preference_manager_get_global{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_preference_manager_get_global');

  Result := assigned(cef_preference_manager_get_global);
end;

function TCefApplicationCore.Load_cef_print_settings_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_print_settings_create{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_print_settings_create');

  Result := assigned(cef_print_settings_create);
end;

function TCefApplicationCore.Load_cef_process_message_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_process_message_create{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_process_message_create');

  Result := assigned(cef_process_message_create);
end;

function TCefApplicationCore.Load_cef_process_util_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_launch_process{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_launch_process');

  Result := assigned(cef_launch_process);
end;

function TCefApplicationCore.Load_cef_request_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_request_create{$IFDEF FPC}){$ENDIF}           := GetProcAddress(FLibHandle, 'cef_request_create');
  {$IFDEF FPC}Pointer({$ENDIF}cef_post_data_create{$IFDEF FPC}){$ENDIF}         := GetProcAddress(FLibHandle, 'cef_post_data_create');
  {$IFDEF FPC}Pointer({$ENDIF}cef_post_data_element_create{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_post_data_element_create');

  Result := assigned(cef_request_create) and
            assigned(cef_post_data_create) and
            assigned(cef_post_data_element_create);
end;

function TCefApplicationCore.Load_cef_request_context_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_request_context_get_global_context{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_request_context_get_global_context');
  {$IFDEF FPC}Pointer({$ENDIF}cef_request_context_create_context{$IFDEF FPC}){$ENDIF}     := GetProcAddress(FLibHandle, 'cef_request_context_create_context');
  {$IFDEF FPC}Pointer({$ENDIF}cef_create_context_shared{$IFDEF FPC}){$ENDIF}              := GetProcAddress(FLibHandle, 'cef_create_context_shared');

  Result := assigned(cef_request_context_get_global_context) and
            assigned(cef_request_context_create_context) and
            assigned(cef_create_context_shared);
end;

function TCefApplicationCore.Load_cef_resource_bundle_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_resource_bundle_get_global{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_resource_bundle_get_global');

  Result := assigned(cef_resource_bundle_get_global);
end;

function TCefApplicationCore.Load_cef_response_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_response_create{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_response_create');

  Result := assigned(cef_response_create);
end;

function TCefApplicationCore.Load_cef_scheme_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_register_scheme_handler_factory{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_register_scheme_handler_factory');
  {$IFDEF FPC}Pointer({$ENDIF}cef_clear_scheme_handler_factories{$IFDEF FPC}){$ENDIF}  := GetProcAddress(FLibHandle, 'cef_clear_scheme_handler_factories');

  Result := assigned(cef_register_scheme_handler_factory) and
            assigned(cef_clear_scheme_handler_factories);
end;

function TCefApplicationCore.Load_cef_server_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_server_create{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_server_create');

  Result := assigned(cef_server_create);
end;

function TCefApplicationCore.Load_cef_shared_process_message_builder_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_shared_process_message_builder_create{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_shared_process_message_builder_create');

  Result := assigned(cef_shared_process_message_builder_create);
end;

function TCefApplicationCore.Load_cef_ssl_info_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_is_cert_status_error{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_is_cert_status_error');

  Result := assigned(cef_is_cert_status_error);
end;

function TCefApplicationCore.Load_cef_stream_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_stream_reader_create_for_file{$IFDEF FPC}){$ENDIF}    := GetProcAddress(FLibHandle, 'cef_stream_reader_create_for_file');
  {$IFDEF FPC}Pointer({$ENDIF}cef_stream_reader_create_for_data{$IFDEF FPC}){$ENDIF}    := GetProcAddress(FLibHandle, 'cef_stream_reader_create_for_data');
  {$IFDEF FPC}Pointer({$ENDIF}cef_stream_reader_create_for_handler{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_stream_reader_create_for_handler');
  {$IFDEF FPC}Pointer({$ENDIF}cef_stream_writer_create_for_file{$IFDEF FPC}){$ENDIF}    := GetProcAddress(FLibHandle, 'cef_stream_writer_create_for_file');
  {$IFDEF FPC}Pointer({$ENDIF}cef_stream_writer_create_for_handler{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_stream_writer_create_for_handler');

  Result := assigned(cef_stream_reader_create_for_file) and
            assigned(cef_stream_reader_create_for_data) and
            assigned(cef_stream_reader_create_for_handler) and
            assigned(cef_stream_writer_create_for_file) and
            assigned(cef_stream_writer_create_for_handler);
end;

function TCefApplicationCore.Load_cef_task_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_task_runner_get_for_current_thread{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_task_runner_get_for_current_thread');
  {$IFDEF FPC}Pointer({$ENDIF}cef_task_runner_get_for_thread{$IFDEF FPC}){$ENDIF}         := GetProcAddress(FLibHandle, 'cef_task_runner_get_for_thread');
  {$IFDEF FPC}Pointer({$ENDIF}cef_currently_on{$IFDEF FPC}){$ENDIF}                       := GetProcAddress(FLibHandle, 'cef_currently_on');
  {$IFDEF FPC}Pointer({$ENDIF}cef_post_task{$IFDEF FPC}){$ENDIF}                          := GetProcAddress(FLibHandle, 'cef_post_task');
  {$IFDEF FPC}Pointer({$ENDIF}cef_post_delayed_task{$IFDEF FPC}){$ENDIF}                  := GetProcAddress(FLibHandle, 'cef_post_delayed_task');

  Result := assigned(cef_task_runner_get_for_current_thread) and
            assigned(cef_task_runner_get_for_thread) and
            assigned(cef_currently_on) and
            assigned(cef_post_task) and
            assigned(cef_post_delayed_task);
end;

function TCefApplicationCore.Load_cef_task_manager_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_task_manager_get{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_task_manager_get');

  Result := assigned(cef_task_manager_get);
end;

function TCefApplicationCore.Load_cef_thread_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_thread_create{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_thread_create');

  Result := assigned(cef_thread_create);
end;

function TCefApplicationCore.Load_cef_trace_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_begin_tracing{$IFDEF FPC}){$ENDIF}              := GetProcAddress(FLibHandle, 'cef_begin_tracing');
  {$IFDEF FPC}Pointer({$ENDIF}cef_end_tracing{$IFDEF FPC}){$ENDIF}                := GetProcAddress(FLibHandle, 'cef_end_tracing');
  {$IFDEF FPC}Pointer({$ENDIF}cef_now_from_system_trace_time{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_now_from_system_trace_time');

  Result := assigned(cef_begin_tracing) and
            assigned(cef_end_tracing) and
            assigned(cef_now_from_system_trace_time);
end;

function TCefApplicationCore.Load_cef_urlrequest_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_urlrequest_create{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_urlrequest_create');

  Result := assigned(cef_urlrequest_create);
end;

function TCefApplicationCore.Load_cef_v8_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_v8context_get_current_context{$IFDEF FPC}){$ENDIF}           := GetProcAddress(FLibHandle, 'cef_v8context_get_current_context');
  {$IFDEF FPC}Pointer({$ENDIF}cef_v8context_get_entered_context{$IFDEF FPC}){$ENDIF}           := GetProcAddress(FLibHandle, 'cef_v8context_get_entered_context');
  {$IFDEF FPC}Pointer({$ENDIF}cef_v8context_in_context{$IFDEF FPC}){$ENDIF}                    := GetProcAddress(FLibHandle, 'cef_v8context_in_context');
  {$IFDEF FPC}Pointer({$ENDIF}cef_v8value_create_undefined{$IFDEF FPC}){$ENDIF}                := GetProcAddress(FLibHandle, 'cef_v8value_create_undefined');
  {$IFDEF FPC}Pointer({$ENDIF}cef_v8value_create_null{$IFDEF FPC}){$ENDIF}                     := GetProcAddress(FLibHandle, 'cef_v8value_create_null');
  {$IFDEF FPC}Pointer({$ENDIF}cef_v8value_create_bool{$IFDEF FPC}){$ENDIF}                     := GetProcAddress(FLibHandle, 'cef_v8value_create_bool');
  {$IFDEF FPC}Pointer({$ENDIF}cef_v8value_create_int{$IFDEF FPC}){$ENDIF}                      := GetProcAddress(FLibHandle, 'cef_v8value_create_int');
  {$IFDEF FPC}Pointer({$ENDIF}cef_v8value_create_uint{$IFDEF FPC}){$ENDIF}                     := GetProcAddress(FLibHandle, 'cef_v8value_create_uint');
  {$IFDEF FPC}Pointer({$ENDIF}cef_v8value_create_double{$IFDEF FPC}){$ENDIF}                   := GetProcAddress(FLibHandle, 'cef_v8value_create_double');
  {$IFDEF FPC}Pointer({$ENDIF}cef_v8value_create_date{$IFDEF FPC}){$ENDIF}                     := GetProcAddress(FLibHandle, 'cef_v8value_create_date');
  {$IFDEF FPC}Pointer({$ENDIF}cef_v8value_create_string{$IFDEF FPC}){$ENDIF}                   := GetProcAddress(FLibHandle, 'cef_v8value_create_string');
  {$IFDEF FPC}Pointer({$ENDIF}cef_v8value_create_object{$IFDEF FPC}){$ENDIF}                   := GetProcAddress(FLibHandle, 'cef_v8value_create_object');
  {$IFDEF FPC}Pointer({$ENDIF}cef_v8value_create_array{$IFDEF FPC}){$ENDIF}                    := GetProcAddress(FLibHandle, 'cef_v8value_create_array');
  {$IFDEF FPC}Pointer({$ENDIF}cef_v8value_create_array_buffer{$IFDEF FPC}){$ENDIF}             := GetProcAddress(FLibHandle, 'cef_v8value_create_array_buffer');
  {$IFDEF FPC}Pointer({$ENDIF}cef_v8value_create_array_buffer_with_copy{$IFDEF FPC}){$ENDIF}   := GetProcAddress(FLibHandle, 'cef_v8value_create_array_buffer_with_copy');
  {$IFDEF FPC}Pointer({$ENDIF}cef_v8value_create_function{$IFDEF FPC}){$ENDIF}                 := GetProcAddress(FLibHandle, 'cef_v8value_create_function');
  {$IFDEF FPC}Pointer({$ENDIF}cef_v8value_create_promise{$IFDEF FPC}){$ENDIF}                  := GetProcAddress(FLibHandle, 'cef_v8value_create_promise');
  {$IFDEF FPC}Pointer({$ENDIF}cef_v8stack_trace_get_current{$IFDEF FPC}){$ENDIF}               := GetProcAddress(FLibHandle, 'cef_v8stack_trace_get_current');
  {$IFDEF FPC}Pointer({$ENDIF}cef_register_extension{$IFDEF FPC}){$ENDIF}                      := GetProcAddress(FLibHandle, 'cef_register_extension');

  Result := assigned(cef_v8context_get_current_context) and
            assigned(cef_v8context_get_entered_context) and
            assigned(cef_v8context_in_context) and
            assigned(cef_v8value_create_undefined) and
            assigned(cef_v8value_create_null) and
            assigned(cef_v8value_create_bool) and
            assigned(cef_v8value_create_int) and
            assigned(cef_v8value_create_uint) and
            assigned(cef_v8value_create_double) and
            assigned(cef_v8value_create_date) and
            assigned(cef_v8value_create_string) and
            assigned(cef_v8value_create_object) and
            assigned(cef_v8value_create_array) and
            assigned(cef_v8value_create_array_buffer) and
            assigned(cef_v8value_create_array_buffer_with_copy) and
            assigned(cef_v8value_create_function) and
            assigned(cef_v8value_create_promise) and
            assigned(cef_v8stack_trace_get_current) and
            assigned(cef_register_extension);
end;

function TCefApplicationCore.Load_cef_values_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_value_create{$IFDEF FPC}){$ENDIF}            := GetProcAddress(FLibHandle, 'cef_value_create');
  {$IFDEF FPC}Pointer({$ENDIF}cef_binary_value_create{$IFDEF FPC}){$ENDIF}     := GetProcAddress(FLibHandle, 'cef_binary_value_create');
  {$IFDEF FPC}Pointer({$ENDIF}cef_dictionary_value_create{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_dictionary_value_create');
  {$IFDEF FPC}Pointer({$ENDIF}cef_list_value_create{$IFDEF FPC}){$ENDIF}       := GetProcAddress(FLibHandle, 'cef_list_value_create');

  Result := assigned(cef_value_create) and
            assigned(cef_binary_value_create) and
            assigned(cef_v8stack_trace_get_current) and
            assigned(cef_list_value_create);
end;

function TCefApplicationCore.Load_cef_waitable_event_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_waitable_event_create{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_waitable_event_create');

  Result := assigned(cef_waitable_event_create);
end;

function TCefApplicationCore.Load_cef_xml_reader_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_xml_reader_create{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_xml_reader_create');

  Result := assigned(cef_xml_reader_create);
end;

function TCefApplicationCore.Load_cef_zip_reader_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_zip_reader_create{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_zip_reader_create');

  Result := assigned(cef_zip_reader_create);
end;

function TCefApplicationCore.Load_cef_logging_internal_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_get_min_log_level{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_get_min_log_level');
  {$IFDEF FPC}Pointer({$ENDIF}cef_get_vlog_level{$IFDEF FPC}){$ENDIF}    := GetProcAddress(FLibHandle, 'cef_get_vlog_level');
  {$IFDEF FPC}Pointer({$ENDIF}cef_log{$IFDEF FPC}){$ENDIF}               := GetProcAddress(FLibHandle, 'cef_log');

  Result := assigned(cef_get_min_log_level) and
            assigned(cef_get_vlog_level) and
            assigned(cef_log);
end;

function TCefApplicationCore.Load_cef_string_list_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_list_alloc{$IFDEF FPC}){$ENDIF}  := GetProcAddress(FLibHandle, 'cef_string_list_alloc');
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_list_size{$IFDEF FPC}){$ENDIF}   := GetProcAddress(FLibHandle, 'cef_string_list_size');
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_list_value{$IFDEF FPC}){$ENDIF}  := GetProcAddress(FLibHandle, 'cef_string_list_value');
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_list_append{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_string_list_append');
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_list_clear{$IFDEF FPC}){$ENDIF}  := GetProcAddress(FLibHandle, 'cef_string_list_clear');
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_list_free{$IFDEF FPC}){$ENDIF}   := GetProcAddress(FLibHandle, 'cef_string_list_free');
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_list_copy{$IFDEF FPC}){$ENDIF}   := GetProcAddress(FLibHandle, 'cef_string_list_copy');

  Result := assigned(cef_string_list_alloc) and
            assigned(cef_string_list_size) and
            assigned(cef_string_list_value) and
            assigned(cef_string_list_append) and
            assigned(cef_string_list_clear) and
            assigned(cef_string_list_free) and
            assigned(cef_string_list_copy);
end;

function TCefApplicationCore.Load_cef_string_map_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_map_alloc{$IFDEF FPC}){$ENDIF}  := GetProcAddress(FLibHandle, 'cef_string_map_alloc');
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_map_size{$IFDEF FPC}){$ENDIF}   := GetProcAddress(FLibHandle, 'cef_string_map_size');
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_map_find{$IFDEF FPC}){$ENDIF}   := GetProcAddress(FLibHandle, 'cef_string_map_find');
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_map_key{$IFDEF FPC}){$ENDIF}    := GetProcAddress(FLibHandle, 'cef_string_map_key');
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_map_value{$IFDEF FPC}){$ENDIF}  := GetProcAddress(FLibHandle, 'cef_string_map_value');
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_map_append{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_string_map_append');
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_map_clear{$IFDEF FPC}){$ENDIF}  := GetProcAddress(FLibHandle, 'cef_string_map_clear');
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_map_free{$IFDEF FPC}){$ENDIF}   := GetProcAddress(FLibHandle, 'cef_string_map_free');

  Result := assigned(cef_string_map_alloc) and
            assigned(cef_string_map_size) and
            assigned(cef_string_map_find) and
            assigned(cef_string_map_key) and
            assigned(cef_string_map_value) and
            assigned(cef_string_map_append) and
            assigned(cef_string_map_clear) and
            assigned(cef_string_map_free);
end;

function TCefApplicationCore.Load_cef_string_multimap_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_multimap_alloc{$IFDEF FPC}){$ENDIF}      := GetProcAddress(FLibHandle, 'cef_string_multimap_alloc');
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_multimap_size{$IFDEF FPC}){$ENDIF}       := GetProcAddress(FLibHandle, 'cef_string_multimap_size');
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_multimap_find_count{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_string_multimap_find_count');
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_multimap_enumerate{$IFDEF FPC}){$ENDIF}  := GetProcAddress(FLibHandle, 'cef_string_multimap_enumerate');
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_multimap_key{$IFDEF FPC}){$ENDIF}        := GetProcAddress(FLibHandle, 'cef_string_multimap_key');
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_multimap_value{$IFDEF FPC}){$ENDIF}      := GetProcAddress(FLibHandle, 'cef_string_multimap_value');
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_multimap_append{$IFDEF FPC}){$ENDIF}     := GetProcAddress(FLibHandle, 'cef_string_multimap_append');
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_multimap_clear{$IFDEF FPC}){$ENDIF}      := GetProcAddress(FLibHandle, 'cef_string_multimap_clear');
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_multimap_free{$IFDEF FPC}){$ENDIF}       := GetProcAddress(FLibHandle, 'cef_string_multimap_free');

  Result := assigned(cef_string_multimap_alloc) and
            assigned(cef_string_multimap_size) and
            assigned(cef_string_multimap_find_count) and
            assigned(cef_string_multimap_enumerate) and
            assigned(cef_string_multimap_key) and
            assigned(cef_string_multimap_value) and
            assigned(cef_string_multimap_append) and
            assigned(cef_string_multimap_clear) and
            assigned(cef_string_multimap_free);
end;

function TCefApplicationCore.Load_cef_string_types_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_wide_set{$IFDEF FPC}){$ENDIF}             := GetProcAddress(FLibHandle, 'cef_string_wide_set');
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_utf8_set{$IFDEF FPC}){$ENDIF}             := GetProcAddress(FLibHandle, 'cef_string_utf8_set');
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_utf16_set{$IFDEF FPC}){$ENDIF}            := GetProcAddress(FLibHandle, 'cef_string_utf16_set');
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_wide_clear{$IFDEF FPC}){$ENDIF}           := GetProcAddress(FLibHandle, 'cef_string_wide_clear');
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_utf8_clear{$IFDEF FPC}){$ENDIF}           := GetProcAddress(FLibHandle, 'cef_string_utf8_clear');
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_utf16_clear{$IFDEF FPC}){$ENDIF}          := GetProcAddress(FLibHandle, 'cef_string_utf16_clear');
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_wide_cmp{$IFDEF FPC}){$ENDIF}             := GetProcAddress(FLibHandle, 'cef_string_wide_cmp');
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_utf8_cmp{$IFDEF FPC}){$ENDIF}             := GetProcAddress(FLibHandle, 'cef_string_utf8_cmp');
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_utf16_cmp{$IFDEF FPC}){$ENDIF}            := GetProcAddress(FLibHandle, 'cef_string_utf16_cmp');
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_wide_to_utf8{$IFDEF FPC}){$ENDIF}         := GetProcAddress(FLibHandle, 'cef_string_wide_to_utf8');
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_utf8_to_wide{$IFDEF FPC}){$ENDIF}         := GetProcAddress(FLibHandle, 'cef_string_utf8_to_wide');
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_wide_to_utf16{$IFDEF FPC}){$ENDIF}        := GetProcAddress(FLibHandle, 'cef_string_wide_to_utf16');
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_utf16_to_wide{$IFDEF FPC}){$ENDIF}        := GetProcAddress(FLibHandle, 'cef_string_utf16_to_wide');
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_utf8_to_utf16{$IFDEF FPC}){$ENDIF}        := GetProcAddress(FLibHandle, 'cef_string_utf8_to_utf16');
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_utf16_to_utf8{$IFDEF FPC}){$ENDIF}        := GetProcAddress(FLibHandle, 'cef_string_utf16_to_utf8');
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_ascii_to_wide{$IFDEF FPC}){$ENDIF}        := GetProcAddress(FLibHandle, 'cef_string_ascii_to_wide');
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_ascii_to_utf16{$IFDEF FPC}){$ENDIF}       := GetProcAddress(FLibHandle, 'cef_string_ascii_to_utf16');
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_userfree_wide_alloc{$IFDEF FPC}){$ENDIF}  := GetProcAddress(FLibHandle, 'cef_string_userfree_wide_alloc');
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_userfree_utf8_alloc{$IFDEF FPC}){$ENDIF}  := GetProcAddress(FLibHandle, 'cef_string_userfree_utf8_alloc');
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_userfree_utf16_alloc{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_string_userfree_utf16_alloc');
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_userfree_wide_free{$IFDEF FPC}){$ENDIF}   := GetProcAddress(FLibHandle, 'cef_string_userfree_wide_free');
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_userfree_utf8_free{$IFDEF FPC}){$ENDIF}   := GetProcAddress(FLibHandle, 'cef_string_userfree_utf8_free');
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_userfree_utf16_free{$IFDEF FPC}){$ENDIF}  := GetProcAddress(FLibHandle, 'cef_string_userfree_utf16_free');
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_utf16_to_lower{$IFDEF FPC}){$ENDIF}       := GetProcAddress(FLibHandle, 'cef_string_utf16_to_lower');
  {$IFDEF FPC}Pointer({$ENDIF}cef_string_utf16_to_upper{$IFDEF FPC}){$ENDIF}       := GetProcAddress(FLibHandle, 'cef_string_utf16_to_upper');

  Result := assigned(cef_string_wide_set) and
            assigned(cef_string_utf8_set) and
            assigned(cef_string_utf16_set) and
            assigned(cef_string_wide_clear) and
            assigned(cef_string_utf8_clear) and
            assigned(cef_string_utf16_clear) and
            assigned(cef_string_wide_cmp) and
            assigned(cef_string_utf8_cmp) and
            assigned(cef_string_utf16_cmp) and
            assigned(cef_string_wide_to_utf8) and
            assigned(cef_string_utf8_to_wide) and
            assigned(cef_string_wide_to_utf16) and
            assigned(cef_string_utf16_to_wide) and
            assigned(cef_string_utf8_to_utf16) and
            assigned(cef_string_utf16_to_utf8) and
            assigned(cef_string_ascii_to_wide) and
            assigned(cef_string_ascii_to_utf16) and
            assigned(cef_string_userfree_wide_alloc) and
            assigned(cef_string_userfree_utf8_alloc) and
            assigned(cef_string_userfree_utf16_alloc) and
            assigned(cef_string_userfree_wide_free) and
            assigned(cef_string_userfree_utf8_free) and
            assigned(cef_string_userfree_utf16_free) and
            assigned(cef_string_utf16_to_lower) and
            assigned(cef_string_utf16_to_upper);
end;

function TCefApplicationCore.Load_cef_thread_internal_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_get_current_platform_thread_id{$IFDEF FPC}){$ENDIF}     := GetProcAddress(FLibHandle, 'cef_get_current_platform_thread_id');
  {$IFDEF FPC}Pointer({$ENDIF}cef_get_current_platform_thread_handle{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_get_current_platform_thread_handle');

  Result := assigned(cef_get_current_platform_thread_id) and
            assigned(cef_get_current_platform_thread_handle);
end;

function TCefApplicationCore.Load_cef_trace_event_internal_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_trace_event_instant{$IFDEF FPC}){$ENDIF}         := GetProcAddress(FLibHandle, 'cef_trace_event_instant');
  {$IFDEF FPC}Pointer({$ENDIF}cef_trace_event_begin{$IFDEF FPC}){$ENDIF}           := GetProcAddress(FLibHandle, 'cef_trace_event_begin');
  {$IFDEF FPC}Pointer({$ENDIF}cef_trace_event_end{$IFDEF FPC}){$ENDIF}             := GetProcAddress(FLibHandle, 'cef_trace_event_end');
  {$IFDEF FPC}Pointer({$ENDIF}cef_trace_counter{$IFDEF FPC}){$ENDIF}               := GetProcAddress(FLibHandle, 'cef_trace_counter');
  {$IFDEF FPC}Pointer({$ENDIF}cef_trace_counter_id{$IFDEF FPC}){$ENDIF}            := GetProcAddress(FLibHandle, 'cef_trace_counter_id');
  {$IFDEF FPC}Pointer({$ENDIF}cef_trace_event_async_begin{$IFDEF FPC}){$ENDIF}     := GetProcAddress(FLibHandle, 'cef_trace_event_async_begin');
  {$IFDEF FPC}Pointer({$ENDIF}cef_trace_event_async_step_into{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_trace_event_async_step_into');
  {$IFDEF FPC}Pointer({$ENDIF}cef_trace_event_async_step_past{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_trace_event_async_step_past');
  {$IFDEF FPC}Pointer({$ENDIF}cef_trace_event_async_end{$IFDEF FPC}){$ENDIF}       := GetProcAddress(FLibHandle, 'cef_trace_event_async_end');

  Result := assigned(cef_trace_event_instant) and
            assigned(cef_trace_event_begin) and
            assigned(cef_trace_event_end) and
            assigned(cef_trace_counter) and
            assigned(cef_trace_counter_id) and
            assigned(cef_trace_event_async_begin) and
            assigned(cef_trace_event_async_step_into) and
            assigned(cef_trace_event_async_step_past) and
            assigned(cef_trace_event_async_end);
end;

function TCefApplicationCore.Load_cef_browser_view_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_browser_view_create{$IFDEF FPC}){$ENDIF}          := GetProcAddress(FLibHandle, 'cef_browser_view_create');
  {$IFDEF FPC}Pointer({$ENDIF}cef_browser_view_get_for_browser{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_browser_view_get_for_browser');

  Result := assigned(cef_browser_view_create) and
            assigned(cef_browser_view_get_for_browser);
end;

function TCefApplicationCore.Load_cef_display_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_display_get_primary{$IFDEF FPC}){$ENDIF}                      := GetProcAddress(FLibHandle, 'cef_display_get_primary');
  {$IFDEF FPC}Pointer({$ENDIF}cef_display_get_nearest_point{$IFDEF FPC}){$ENDIF}                := GetProcAddress(FLibHandle, 'cef_display_get_nearest_point');
  {$IFDEF FPC}Pointer({$ENDIF}cef_display_get_matching_bounds{$IFDEF FPC}){$ENDIF}              := GetProcAddress(FLibHandle, 'cef_display_get_matching_bounds');
  {$IFDEF FPC}Pointer({$ENDIF}cef_display_get_count{$IFDEF FPC}){$ENDIF}                        := GetProcAddress(FLibHandle, 'cef_display_get_count');
  {$IFDEF FPC}Pointer({$ENDIF}cef_display_get_alls{$IFDEF FPC}){$ENDIF}                         := GetProcAddress(FLibHandle, 'cef_display_get_alls');
  {$IFDEF FPC}Pointer({$ENDIF}cef_display_convert_screen_point_to_pixels{$IFDEF FPC}){$ENDIF}   := GetProcAddress(FLibHandle, 'cef_display_convert_screen_point_to_pixels');
  {$IFDEF FPC}Pointer({$ENDIF}cef_display_convert_screen_point_from_pixels{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_display_convert_screen_point_from_pixels');
  {$IFDEF FPC}Pointer({$ENDIF}cef_display_convert_screen_rect_to_pixels{$IFDEF FPC}){$ENDIF}    := GetProcAddress(FLibHandle, 'cef_display_convert_screen_rect_to_pixels');
  {$IFDEF FPC}Pointer({$ENDIF}cef_display_convert_screen_rect_from_pixels{$IFDEF FPC}){$ENDIF}  := GetProcAddress(FLibHandle, 'cef_display_convert_screen_rect_from_pixels');

  Result := assigned(cef_display_get_primary) and
            assigned(cef_display_get_nearest_point) and
            assigned(cef_display_get_matching_bounds) and
            assigned(cef_display_get_count) and
            assigned(cef_display_get_alls) and
            assigned(cef_display_convert_screen_point_to_pixels) and
            assigned(cef_display_convert_screen_point_from_pixels) and
            assigned(cef_display_convert_screen_rect_to_pixels) and
            assigned(cef_display_convert_screen_rect_from_pixels);
end;

function TCefApplicationCore.Load_cef_label_button_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_label_button_create{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_label_button_create');

  Result := assigned(cef_label_button_create);
end;

function TCefApplicationCore.Load_cef_menu_button_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_menu_button_create{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_menu_button_create');

  Result := assigned(cef_menu_button_create);
end;

function TCefApplicationCore.Load_cef_panel_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_panel_create{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_panel_create');

  Result := assigned(cef_panel_create);
end;

function TCefApplicationCore.Load_cef_scroll_view_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_scroll_view_create{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_scroll_view_create');

  Result := assigned(cef_scroll_view_create);
end;

function TCefApplicationCore.Load_cef_textfield_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_textfield_create{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_textfield_create');

  Result := assigned(cef_textfield_create);
end;

function TCefApplicationCore.Load_cef_window_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_window_create_top_level{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_window_create_top_level');

  Result := assigned(cef_window_create_top_level);
end;

function TCefApplicationCore.Load_cef_types_linux_h : boolean;
begin
  {$IFDEF LINUX}
  {$IFDEF FPC}Pointer({$ENDIF}cef_get_xdisplay{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_get_xdisplay');

  Result := assigned(cef_get_xdisplay);
  {$ELSE}
  Result := True;
  {$ENDIF}
end;

function TCefApplicationCore.Load_cef_time_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_time_to_timet{$IFDEF FPC}){$ENDIF}      := GetProcAddress(FLibHandle, 'cef_time_to_timet');
  {$IFDEF FPC}Pointer({$ENDIF}cef_time_from_timet{$IFDEF FPC}){$ENDIF}    := GetProcAddress(FLibHandle, 'cef_time_from_timet');
  {$IFDEF FPC}Pointer({$ENDIF}cef_time_to_doublet{$IFDEF FPC}){$ENDIF}    := GetProcAddress(FLibHandle, 'cef_time_to_doublet');
  {$IFDEF FPC}Pointer({$ENDIF}cef_time_from_doublet{$IFDEF FPC}){$ENDIF}  := GetProcAddress(FLibHandle, 'cef_time_from_doublet');
  {$IFDEF FPC}Pointer({$ENDIF}cef_time_now{$IFDEF FPC}){$ENDIF}           := GetProcAddress(FLibHandle, 'cef_time_now');
  {$IFDEF FPC}Pointer({$ENDIF}cef_time_delta{$IFDEF FPC}){$ENDIF}         := GetProcAddress(FLibHandle, 'cef_time_delta');
  {$IFDEF FPC}Pointer({$ENDIF}cef_basetime_now{$IFDEF FPC}){$ENDIF}       := GetProcAddress(FLibHandle, 'cef_basetime_now');
  {$IFDEF FPC}Pointer({$ENDIF}cef_time_to_basetime{$IFDEF FPC}){$ENDIF}   := GetProcAddress(FLibHandle, 'cef_time_to_basetime');
  {$IFDEF FPC}Pointer({$ENDIF}cef_time_from_basetime{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_time_from_basetime');


  Result := assigned(cef_time_to_timet) and
            assigned(cef_time_from_timet) and
            assigned(cef_time_to_doublet) and
            assigned(cef_time_from_doublet) and
            assigned(cef_time_now) and
            assigned(cef_time_delta) and
            assigned(cef_basetime_now) and
            assigned(cef_time_to_basetime) and
            assigned(cef_time_from_basetime);
end;

// TCEFDirectoryDeleterThread

constructor TCEFDirectoryDeleterThread.Create(const aDirectory : string);
begin
  inherited Create(True);

  FDirectory      := aDirectory;
  FreeOnTerminate := True;
end;

procedure TCEFDirectoryDeleterThread.Execute;
begin

  try
    {$IFDEF DELPHI14_UP}
    TDirectory.Delete(FDirectory, True);
    {$ELSE}
    if DeleteDirContents(FDirectory) then RemoveDir(FDirectory);
    {$ENDIF}
  except
    on e : exception do
      if CustomExceptionHandler('TCEFDirectoryDeleterThread.Execute', e) then raise;
  end;
end;

end.
