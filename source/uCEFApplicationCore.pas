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

unit uCEFApplicationCore;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

{$IFNDEF FPC}{$IFNDEF DELPHI12_UP}
  // Workaround for "Internal error" in old Delphi versions caused by uint64 handling
  {$R-}
{$ENDIF}{$ENDIF}

interface

uses
  {$IFDEF DELPHI16_UP}
    {$IFDEF MSWINDOWS}WinApi.Windows,{$ENDIF} System.Classes, System.UITypes,
  {$ELSE}
    {$IFDEF MSWINDOWS}Windows,{$ENDIF} Classes, {$IFDEF FPC}dynlibs,{$ENDIF}
  {$ENDIF}
  {$IFDEF LINUX}{$IFDEF FPC}xlib,{$ENDIF}{$ENDIF}
  uCEFTypes, uCEFInterfaces, uCEFBaseRefCounted, uCEFSchemeRegistrar;

const
  CEF_SUPPORTED_VERSION_MAJOR   = 88;
  CEF_SUPPORTED_VERSION_MINOR   = 1;
  CEF_SUPPORTED_VERSION_RELEASE = 4;
  CEF_SUPPORTED_VERSION_BUILD   = 0;

  CEF_CHROMEELF_VERSION_MAJOR   = 88;
  CEF_CHROMEELF_VERSION_MINOR   = 0;
  CEF_CHROMEELF_VERSION_RELEASE = 4324;
  CEF_CHROMEELF_VERSION_BUILD   = 96;

  {$IFDEF MSWINDOWS}
  LIBCEF_DLL                    = 'libcef.dll';
  CHROMEELF_DLL                 = 'chrome_elf.dll';
  {$ELSE}
  LIBCEF_DLL                    = 'libcef.so';
  CHROMEELF_DLL                 = '';
  {$ENDIF}

type
  TCefApplicationCore = class
    protected
      FCache                             : ustring;
      FRootCache                         : ustring;
      FUserDataPath                      : ustring;
      FUserAgent                         : ustring;
      FProductVersion                    : ustring;
      FLocale                            : ustring;
      FLocalesRequired                   : ustring;
      FLogFile                           : ustring;
      FBrowserSubprocessPath             : ustring;
      FFrameworkDirPath                  : ustring;
      FMainBundlePath                    : ustring; // Only used in macOS
      FChromeRuntime                     : boolean;
      FLogSeverity                       : TCefLogSeverity;
      FJavaScriptFlags                   : ustring;
      FResourcesDirPath                  : ustring;
      FLocalesDirPath                    : ustring;
      FSingleProcess                     : Boolean;
      FNoSandbox                         : Boolean;
      FCommandLineArgsDisabled           : Boolean;
      FPackLoadingDisabled               : Boolean;
      FRemoteDebuggingPort               : Integer;
      FUncaughtExceptionStackSize        : Integer;
      FPersistSessionCookies             : Boolean;
      FPersistUserPreferences            : boolean;
      FIgnoreCertificateErrors           : Boolean;
      FBackgroundColor                   : TCefColor;
      FAcceptLanguageList                : ustring;
      FApplicationClientID               : ustring;
      FWindowsSandboxInfo                : Pointer;
      FWindowlessRenderingEnabled        : Boolean;
      FMultiThreadedMessageLoop          : boolean;
      FExternalMessagePump               : boolean;
      FDeleteCache                       : boolean;
      FDeleteCookies                     : boolean;
      FCustomCommandLines                : TStringList;
      FCustomCommandLineValues           : TStringList;
      FEnableMediaStream                 : boolean;
      FEnableSpeechInput                 : boolean;
      FUseFakeUIForMediaStream           : boolean;
      FEnableUsermediaScreenCapturing    : boolean;
      FEnableGPU                         : boolean;
      FCheckCEFFiles                     : boolean;
      FLibLoaded                         : boolean;
      FSmoothScrolling                   : TCefState;
      FFastUnload                        : boolean;
      FDisableSafeBrowsing               : boolean;
      FEnableHighDPISupport              : boolean;
      FMuteAudio                         : boolean;
      FReRaiseExceptions                 : boolean;
      FShowMessageDlg                    : boolean;
      FMissingBinariesException          : boolean;
      FSetCurrentDir                     : boolean;
      FGlobalContextInitialized          : boolean;
      FSitePerProcess                    : boolean;
      FDisableWebSecurity                : boolean;
      FDisablePDFExtension               : boolean;
      FLogProcessInfo                    : boolean;
      FDisableSiteIsolationTrials        : boolean;
      FDisableChromeLoginPrompt          : boolean;
      FEnableFeatures                    : ustring;
      FDisableFeatures                   : ustring;
      FEnableBlinkFeatures               : ustring;
      FDisableBlinkFeatures              : ustring;
      FBlinkSettings                     : ustring;
      FForceFieldTrials                  : ustring;
      FForceFieldTrialParams             : ustring;
      FChromeVersionInfo                 : TFileVersionInfo;
      {$IFDEF FPC}
      FLibHandle                         : TLibHandle;
      {$ELSE}
      FLibHandle                         : THandle;
      {$ENDIF}
      FOnRegisterCustomSchemes           : TOnRegisterCustomSchemesEvent;
      FAppSettings                       : TCefSettings;
      FCheckDevToolsResources            : boolean;
      FDisableExtensions                 : boolean;
      FDisableGPUCache                   : boolean;
      FStatus                            : TCefAplicationStatus;
      FMissingLibFiles                   : string;
      FProcessType                       : TCefProcessType;
      FWidevinePath                      : ustring;
      FMustFreeLibrary                   : boolean;
      FAutoplayPolicy                    : TCefAutoplayPolicy;
      FDisableBackgroundNetworking       : boolean;
      FMetricsRecordingOnly              : boolean;
      FAllowFileAccessFromFiles          : boolean;
      FAllowRunningInsecureContent       : boolean;
      FSupportedSchemes                  : TStringList;
      FDisableNewBrowserInfoTimeout      : boolean;
      FDevToolsProtocolLogFile           : ustring;
      FDeviceScaleFactor                 : single;
      FForcedDeviceScaleFactor           : single;
      FDisableZygote                     : boolean;

      FPluginPolicy                      : TCefPluginPolicySwitch;
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
      FDisablePlugins                    : boolean;
      FEnableProfanityFilter             : boolean;
      FDisableSpellChecking              : boolean;
      FOverrideSpellCheckLang            : ustring;
      FEnablePrintPreview                : boolean;
      FTouchEvents                       : TCefState;
      FDisableReadingFromCanvas          : boolean;
      FHyperlinkAuditing                 : boolean;

      FMustCreateResourceBundleHandler   : boolean;
      FMustCreateBrowserProcessHandler   : boolean;
      FMustCreateRenderProcessHandler    : boolean;
      FMustCreateLoadHandler             : boolean;
      FMustCreatePrintHandler            : boolean;

      // ICefBrowserProcessHandler
      FOnGetCookieableSchemes            : TOnGetCookieableSchemesEvent;
      FOnContextInitialized              : TOnContextInitializedEvent;
      FOnBeforeChildProcessLaunch        : TOnBeforeChildProcessLaunchEvent;
      FOnScheduleMessagePumpWork         : TOnScheduleMessagePumpWorkEvent;
      FOnGetDefaultClient                : TOnGetDefaultClientEvent;

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

      // ICefRegisterCDMCallback
      FOnCDMRegistrationComplete         : TOnCDMRegistrationCompleteEvent;

      // ICefLoadHandler
      FOnLoadingStateChange              : TOnRenderLoadingStateChange;
      FOnLoadStart                       : TOnRenderLoadStart;
      FOnLoadEnd                         : TOnRenderLoadEnd;
      FOnLoadError                       : TOnRenderLoadError;

      // ICefPrintHandler
      FOnPrintStart                      : TOnPrintStartEvent;
      FOnPrintSettings                   : TOnPrintSettingsEvent;
      FOnPrintDialog                     : TOnPrintDialogEvent;
      FOnPrintJob                        : TOnPrintJobEvent;
      FOnPrintReset                      : TOnPrintResetEvent;
      FOnGetPDFPaperSize                 : TOnGetPDFPaperSizeEvent;

      procedure SetCache(const aValue : ustring);
      procedure SetRootCache(const aValue : ustring);
      procedure SetUserDataPath(const aValue : ustring);
      procedure SetBrowserSubprocessPath(const aValue : ustring);
      procedure SetFrameworkDirPath(const aValue : ustring);
      procedure SetResourcesDirPath(const aValue : ustring);
      procedure SetLocalesDirPath(const aValue : ustring);
      procedure SetOsmodalLoop(aValue : boolean);

      function  GetChromeVersion : ustring;
      function  GetLibCefVersion : ustring;
      function  GetLibCefPath : ustring;
      function  GetChromeElfPath : ustring;
      function  GetMustCreateResourceBundleHandler : boolean; virtual;
      function  GetMustCreateBrowserProcessHandler : boolean; virtual;
      function  GetMustCreateRenderProcessHandler : boolean; virtual;
      function  GetMustCreateLoadHandler : boolean; virtual;
      function  GetMustCreatePrintHandler : boolean; virtual;
      function  GetGlobalContextInitialized : boolean;
      function  GetChildProcessesCount : integer;
      function  GetUsedMemory : uint64;
      function  GetTotalSystemMemory : uint64;
      function  GetAvailableSystemMemory : uint64;
      function  GetSystemMemoryLoad : cardinal;
      {$IFDEF LINUX}
      function  GetXDisplay : PXDisplay;
      {$ENDIF}

      function  LoadCEFlibrary : boolean; virtual;
      function  Load_cef_app_capi_h : boolean;
      function  Load_cef_browser_capi_h : boolean;
      function  Load_cef_command_line_capi_h : boolean;
      function  Load_cef_cookie_capi_h : boolean;
      function  Load_cef_crash_util_h : boolean;
      function  Load_cef_drag_data_capi_h : boolean;
      function  Load_cef_file_util_capi_h : boolean;
      function  Load_cef_image_capi_h : boolean;
      function  Load_cef_menu_model_capi_h : boolean;
      function  Load_cef_media_router_capi_h : boolean;
      function  Load_cef_origin_whitelist_capi_h : boolean;
      function  Load_cef_parser_capi_h : boolean;
      function  Load_cef_path_util_capi_h : boolean;
      function  Load_cef_print_settings_capi_h : boolean;
      function  Load_cef_process_message_capi_h : boolean;
      function  Load_cef_process_util_capi_h : boolean;
      function  Load_cef_request_capi_h : boolean;
      function  Load_cef_request_context_capi_h : boolean;
      function  Load_cef_resource_bundle_capi_h : boolean;
      function  Load_cef_response_capi_h : boolean;
      function  Load_cef_server_capi_h : boolean;
      function  Load_cef_scheme_capi_h : boolean;
      function  Load_cef_ssl_info_capi_h : boolean;
      function  Load_cef_stream_capi_h : boolean;
      function  Load_cef_task_capi_h : boolean;
      function  Load_cef_thread_capi_h : boolean;
      function  Load_cef_trace_capi_h : boolean;
      function  Load_cef_urlrequest_capi_h : boolean;
      function  Load_cef_v8_capi_h : boolean;
      function  Load_cef_values_capi_h : boolean;
      function  Load_cef_waitable_event_capi_h : boolean;
      function  Load_cef_web_plugin_capi_h : boolean;
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

      procedure ShutDown;
      procedure FreeLibcefLibrary;
      function  ExecuteProcess(const aApp : ICefApp) : integer;
      procedure InitializeSettings(var aSettings : TCefSettings);
      function  InitializeLibrary(const aApp : ICefApp) : boolean;
      procedure RenameAndDeleteDir(const aDirectory : string; aKeepCookies : boolean = False);
      procedure DeleteCacheContents(const aDirectory : string);
      procedure DeleteCookiesDB(const aDirectory : string);
      procedure MoveCookiesDB(const aSrcDirectory, aDstDirectory : string);
      function  MultiExeProcessing : boolean;
      function  SingleExeProcessing : boolean;
      procedure BeforeInitSubProcess; virtual;
      function  CheckCEFLibrary : boolean;
      procedure RegisterWidevineCDM;
      procedure ShowErrorMessageDlg(const aError : string); virtual;
      procedure UpdateSupportedSchemes(aIncludeDefaults : boolean = True); virtual;
      function  ParseProcessType : TCefProcessType;
      procedure AddCustomCommandLineSwitches(var aKeys, aValues : TStringList); virtual;
      procedure AppendSwitch(var aKeys, aValues : TStringList; const aNewKey : ustring; const aNewValue : ustring = '');
      procedure ReplaceSwitch(var aKeys, aValues : TStringList; const aNewKey : ustring; const aNewValue : ustring = '');
      procedure CleanupFeatures(var aKeys, aValues : TStringList; const aEnableKey, aDisableKey : string);

    public
      constructor Create;
      destructor  Destroy; override;
      procedure   AfterConstruction; override;
      procedure   AddCustomCommandLine(const aCommandLine : string; const aValue : string = '');
      function    StartMainProcess : boolean;
      function    StartSubProcess : boolean;

      procedure   DoMessageLoopWork;
      procedure   RunMessageLoop;
      procedure   QuitMessageLoop;
      procedure   UpdateDeviceScaleFactor; virtual;

      // Internal procedures. Only ICefApp, ICefBrowserProcessHandler,
      // ICefResourceBundleHandler, ICefRenderProcessHandler, ICefRegisterCDMCallback,
      // ICefLoadHandler and ICefPrintHandler should use them.
      procedure   Internal_OnBeforeCommandLineProcessing(const processType: ustring; const commandLine: ICefCommandLine);
      procedure   Internal_OnRegisterCustomSchemes(const registrar: TCefSchemeRegistrarRef);
      procedure   Internal_OnContextInitialized;
      procedure   Internal_OnBeforeChildProcessLaunch(const commandLine: ICefCommandLine);
      procedure   Internal_OnScheduleMessagePumpWork(const delayMs: Int64);
      function    Internal_GetLocalizedString(stringId: Integer; var stringVal: ustring) : boolean;
      function    Internal_GetDataResource(resourceId: Integer; var data: Pointer; var dataSize: NativeUInt) : boolean;
      function    Internal_GetDataResourceForScale(resourceId: Integer; scaleFactor: TCefScaleFactor; var data: Pointer; var dataSize: NativeUInt) : boolean;
      procedure   Internal_OnWebKitInitialized;
      procedure   Internal_OnBrowserCreated(const browser: ICefBrowser; const extra_info: ICefDictionaryValue);
      procedure   Internal_OnBrowserDestroyed(const browser: ICefBrowser);
      procedure   Internal_OnContextCreated(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context);
      procedure   Internal_OnContextReleased(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context);
      procedure   Internal_OnUncaughtException(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context; const exception: ICefV8Exception; const stackTrace: ICefV8StackTrace);
      procedure   Internal_OnFocusedNodeChanged(const browser: ICefBrowser; const frame: ICefFrame; const node: ICefDomNode);
      procedure   Internal_OnProcessMessageReceived(const browser: ICefBrowser; const frame: ICefFrame; sourceProcess: TCefProcessId; const aMessage: ICefProcessMessage; var aHandled : boolean);
      procedure   Internal_OnCDMRegistrationComplete(result : TCefCDMRegistrationError; const error_message : ustring);
      procedure   Internal_OnLoadingStateChange(const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
      procedure   Internal_OnLoadStart(const browser: ICefBrowser; const frame: ICefFrame; transitionType: TCefTransitionType);
      procedure   Internal_OnLoadEnd(const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: Integer);
      procedure   Internal_OnLoadError(const browser: ICefBrowser; const frame: ICefFrame; errorCode: Integer; const errorText, failedUrl: ustring);
      procedure   Internal_GetCookieableSchemes(var schemes: TStringList; var include_defaults : boolean);
      procedure   Internal_GetDefaultClient(var aClient : ICefClient);
      procedure   Internal_OnPrintStart(const browser: ICefBrowser);
      procedure   Internal_OnPrintSettings(const browser: ICefBrowser; const settings: ICefPrintSettings; getDefaults: boolean);
      procedure   Internal_OnPrintDialog(const browser: ICefBrowser; hasSelection: boolean; const callback: ICefPrintDialogCallback; var aResult : boolean);
      procedure   Internal_OnPrintJob(const browser: ICefBrowser; const documentName, PDFFilePath: ustring; const callback: ICefPrintJobCallback; var aResult : boolean);
      procedure   Internal_OnPrintReset(const browser: ICefBrowser);
      procedure   Internal_OnGetPDFPaperSize(deviceUnitsPerInch: Integer; var aResult : TCefSize);

      // Properties used to populate TCefSettings (cef_settings_t)
      property NoSandbox                         : Boolean                             read FNoSandbox                         write FNoSandbox;
      property BrowserSubprocessPath             : ustring                             read FBrowserSubprocessPath             write SetBrowserSubprocessPath;
      property FrameworkDirPath                  : ustring                             read FFrameworkDirPath                  write SetFrameworkDirPath;
      property MainBundlePath                    : ustring                             read FMainBundlePath                    write FMainBundlePath;  // Only used in macOS
      property ChromeRuntime                     : boolean                             read FChromeRuntime                     write FChromeRuntime;
      property MultiThreadedMessageLoop          : boolean                             read FMultiThreadedMessageLoop          write FMultiThreadedMessageLoop;
      property ExternalMessagePump               : boolean                             read FExternalMessagePump               write FExternalMessagePump;
      property WindowlessRenderingEnabled        : Boolean                             read FWindowlessRenderingEnabled        write FWindowlessRenderingEnabled;
      property CommandLineArgsDisabled           : Boolean                             read FCommandLineArgsDisabled           write FCommandLineArgsDisabled;
      property Cache                             : ustring                             read FCache                             write SetCache;
      property RootCache                         : ustring                             read FRootCache                         write SetRootCache;
      property UserDataPath                      : ustring                             read FUserDataPath                      write SetUserDataPath;
      property PersistSessionCookies             : Boolean                             read FPersistSessionCookies             write FPersistSessionCookies;
      property PersistUserPreferences            : Boolean                             read FPersistUserPreferences            write FPersistUserPreferences;
      property UserAgent                         : ustring                             read FUserAgent                         write FUserAgent;
      property ProductVersion                    : ustring                             read FProductVersion                    write FProductVersion;
      property Locale                            : ustring                             read FLocale                            write FLocale;
      property LogFile                           : ustring                             read FLogFile                           write FLogFile;
      property LogSeverity                       : TCefLogSeverity                     read FLogSeverity                       write FLogSeverity;
      property JavaScriptFlags                   : ustring                             read FJavaScriptFlags                   write FJavaScriptFlags;
      property ResourcesDirPath                  : ustring                             read FResourcesDirPath                  write SetResourcesDirPath;
      property LocalesDirPath                    : ustring                             read FLocalesDirPath                    write SetLocalesDirPath;
      property PackLoadingDisabled               : Boolean                             read FPackLoadingDisabled               write FPackLoadingDisabled;
      property RemoteDebuggingPort               : Integer                             read FRemoteDebuggingPort               write FRemoteDebuggingPort;
      property UncaughtExceptionStackSize        : Integer                             read FUncaughtExceptionStackSize        write FUncaughtExceptionStackSize;
      property IgnoreCertificateErrors           : Boolean                             read FIgnoreCertificateErrors           write FIgnoreCertificateErrors;
      property BackgroundColor                   : TCefColor                           read FBackgroundColor                   write FBackgroundColor;
      property AcceptLanguageList                : ustring                             read FAcceptLanguageList                write FAcceptLanguageList;
      property ApplicationClientID               : ustring                             read FApplicationClientID               write FApplicationClientID;

      // Properties used to set command line switches
      property SingleProcess                     : Boolean                             read FSingleProcess                     write FSingleProcess;                    // --single-process
      property EnableMediaStream                 : boolean                             read FEnableMediaStream                 write FEnableMediaStream;                // --enable-media-stream
      property EnableSpeechInput                 : boolean                             read FEnableSpeechInput                 write FEnableSpeechInput;                // --enable-speech-input
      property UseFakeUIForMediaStream           : boolean                             read FUseFakeUIForMediaStream           write FUseFakeUIForMediaStream;          // --use-fake-ui-for-media-stream
      property EnableUsermediaScreenCapturing    : boolean                             read FEnableUsermediaScreenCapturing    write FEnableUsermediaScreenCapturing;   // --enable-usermedia-screen-capturing
      property EnableGPU                         : boolean                             read FEnableGPU                         write FEnableGPU;                        // --enable-gpu-plugin
      property EnableFeatures                    : ustring                             read FEnableFeatures                    write FEnableFeatures;                   // --enable-features
      property DisableFeatures                   : ustring                             read FDisableFeatures                   write FDisableFeatures;                  // --disable-features
      property EnableBlinkFeatures               : ustring                             read FEnableBlinkFeatures               write FEnableBlinkFeatures;              // --enable-blink-features
      property DisableBlinkFeatures              : ustring                             read FDisableBlinkFeatures              write FDisableBlinkFeatures;             // --disable-blink-features
      property BlinkSettings                     : ustring                             read FBlinkSettings                     write FBlinkSettings;                    // --blink-settings
      property ForceFieldTrials                  : ustring                             read FForceFieldTrials                  write FForceFieldTrials;                 // --force-fieldtrials
      property ForceFieldTrialParams             : ustring                             read FForceFieldTrialParams             write FForceFieldTrialParams;            // --force-fieldtrial-params
      property SmoothScrolling                   : TCefState                           read FSmoothScrolling                   write FSmoothScrolling;                  // --enable-smooth-scrolling
      property FastUnload                        : boolean                             read FFastUnload                        write FFastUnload;                       // --enable-fast-unload
      property DisableSafeBrowsing               : boolean                             read FDisableSafeBrowsing               write FDisableSafeBrowsing;              // --safebrowsing-disable-auto-update
      property MuteAudio                         : boolean                             read FMuteAudio                         write FMuteAudio;                        // --mute-audio
      property SitePerProcess                    : boolean                             read FSitePerProcess                    write FSitePerProcess;                   // --site-per-process
      property DisableWebSecurity                : boolean                             read FDisableWebSecurity                write FDisableWebSecurity;               // --disable-web-security
      property DisablePDFExtension               : boolean                             read FDisablePDFExtension               write FDisablePDFExtension;              // --disable-pdf-extension
      property DisableSiteIsolationTrials        : boolean                             read FDisableSiteIsolationTrials        write FDisableSiteIsolationTrials;       // --disable-site-isolation-trials
      property DisableChromeLoginPrompt          : boolean                             read FDisableChromeLoginPrompt          write FDisableChromeLoginPrompt;         // --disable-chrome-login-prompt
      property DisableExtensions                 : boolean                             read FDisableExtensions                 write FDisableExtensions;                // --disable-extensions
      property AutoplayPolicy                    : TCefAutoplayPolicy                  read FAutoplayPolicy                    write FAutoplayPolicy;                   // --autoplay-policy
      property DisableBackgroundNetworking       : boolean                             read FDisableBackgroundNetworking       write FDisableBackgroundNetworking;      // --disable-background-networking
      property MetricsRecordingOnly              : boolean                             read FMetricsRecordingOnly              write FMetricsRecordingOnly;             // --metrics-recording-only
      property AllowFileAccessFromFiles          : boolean                             read FAllowFileAccessFromFiles          write FAllowFileAccessFromFiles;         // --allow-file-access-from-files
      property AllowRunningInsecureContent       : boolean                             read FAllowRunningInsecureContent       write FAllowRunningInsecureContent;      // --allow-running-insecure-content
      property EnablePrintPreview                : boolean                             read FEnablePrintPreview                write FEnablePrintPreview;               // --enable-print-preview
      property PluginPolicy                      : TCefPluginPolicySwitch              read FPluginPolicy                      write FPluginPolicy;                     // --plugin-policy
      property DefaultEncoding                   : ustring                             read FDefaultEncoding                   write FDefaultEncoding;                  // --default-encoding
      property DisableJavascript                 : boolean                             read FDisableJavascript                 write FDisableJavascript;                // --disable-javascript
      property DisableJavascriptCloseWindows     : boolean                             read FDisableJavascriptCloseWindows     write FDisableJavascriptCloseWindows;    // --disable-javascript-close-windows
      property DisableJavascriptAccessClipboard  : boolean                             read FDisableJavascriptAccessClipboard  write FDisableJavascriptAccessClipboard; // --disable-javascript-access-clipboard
      property DisableJavascriptDomPaste         : boolean                             read FDisableJavascriptDomPaste         write FDisableJavascriptDomPaste;        // --disable-javascript-dom-paste
      property AllowUniversalAccessFromFileUrls  : boolean                             read FAllowUniversalAccessFromFileUrls  write FAllowUniversalAccessFromFileUrls; // --allow-universal-access-from-files
      property DisableImageLoading               : boolean                             read FDisableImageLoading               write FDisableImageLoading;              // --disable-image-loading
      property ImageShrinkStandaloneToFit        : boolean                             read FImageShrinkStandaloneToFit        write FImageShrinkStandaloneToFit;       // --image-shrink-standalone-to-fit
      property DisableTextAreaResize             : boolean                             read FDisableTextAreaResize             write FDisableTextAreaResize;            // --disable-text-area-resize
      property DisableTabToLinks                 : boolean                             read FDisableTabToLinks                 write FDisableTabToLinks;                // --disable-tab-to-links
      property DisablePlugins                    : boolean                             read FDisablePlugins                    write FDisablePlugins;                   // --disable-plugins
      property EnableProfanityFilter             : boolean                             read FEnableProfanityFilter             write FEnableProfanityFilter;            // --enable-profanity-filter
      property DisableSpellChecking              : boolean                             read FDisableSpellChecking              write FDisableSpellChecking;             // --disable-spell-checking
      property OverrideSpellCheckLang            : ustring                             read FOverrideSpellCheckLang            write FOverrideSpellCheckLang;           // --override-spell-check-lang
      property TouchEvents                       : TCefState                           read FTouchEvents                       write FTouchEvents;                      // --touch-events
      property DisableReadingFromCanvas          : boolean                             read FDisableReadingFromCanvas          write FDisableReadingFromCanvas;         // --disable-reading-from-canvas
      property HyperlinkAuditing                 : boolean                             read FHyperlinkAuditing                 write FHyperlinkAuditing;                // --no-pings
      property DisableNewBrowserInfoTimeout      : boolean                             read FDisableNewBrowserInfoTimeout      write FDisableNewBrowserInfoTimeout;     // --disable-new-browser-info-timeout
      property DevToolsProtocolLogFile           : ustring                             read FDevToolsProtocolLogFile           write FDevToolsProtocolLogFile;          // --devtools-protocol-log-file
      property ForcedDeviceScaleFactor           : single                              read FForcedDeviceScaleFactor           write FForcedDeviceScaleFactor;          // --device-scale-factor
      property DisableZygote                     : boolean                             read FDisableZygote                     write FDisableZygote;                    // --no-zygote

      // Properties used during the CEF initialization
      property WindowsSandboxInfo                : Pointer                             read FWindowsSandboxInfo                write FWindowsSandboxInfo;
      property EnableHighDPISupport              : boolean                             read FEnableHighDPISupport              write FEnableHighDPISupport;

      // Custom properties
      property DeleteCache                       : boolean                             read FDeleteCache                       write FDeleteCache;
      property DeleteCookies                     : boolean                             read FDeleteCookies                     write FDeleteCookies;
      property CheckCEFFiles                     : boolean                             read FCheckCEFFiles                     write FCheckCEFFiles;
      property ShowMessageDlg                    : boolean                             read FShowMessageDlg                    write FShowMessageDlg;
      property MissingBinariesException          : boolean                             read FMissingBinariesException          write FMissingBinariesException;
      property SetCurrentDir                     : boolean                             read FSetCurrentDir                     write FSetCurrentDir;
      property GlobalContextInitialized          : boolean                             read GetGlobalContextInitialized;
      property ChromeMajorVer                    : uint16                              read FChromeVersionInfo.MajorVer;
      property ChromeMinorVer                    : uint16                              read FChromeVersionInfo.MinorVer;
      property ChromeRelease                     : uint16                              read FChromeVersionInfo.Release;
      property ChromeBuild                       : uint16                              read FChromeVersionInfo.Build;
      property ChromeVersion                     : ustring                             read GetChromeVersion;
      property LibCefVersion                     : ustring                             read GetLibCefVersion;
      property LibCefPath                        : ustring                             read GetLibCefPath;
      property ChromeElfPath                     : ustring                             read GetChromeElfPath;
      property LibLoaded                         : boolean                             read FLibLoaded;
      property LogProcessInfo                    : boolean                             read FLogProcessInfo                    write FLogProcessInfo;
      property ReRaiseExceptions                 : boolean                             read FReRaiseExceptions                 write FReRaiseExceptions;
      property DeviceScaleFactor                 : single                              read FDeviceScaleFactor;
      property CheckDevToolsResources            : boolean                             read FCheckDevToolsResources            write FCheckDevToolsResources;
      property LocalesRequired                   : ustring                             read FLocalesRequired                   write FLocalesRequired;
      property ProcessType                       : TCefProcessType                     read FProcessType;
      property MustCreateResourceBundleHandler   : boolean                             read GetMustCreateResourceBundleHandler write FMustCreateResourceBundleHandler;
      property MustCreateBrowserProcessHandler   : boolean                             read GetMustCreateBrowserProcessHandler write FMustCreateBrowserProcessHandler;
      property MustCreateRenderProcessHandler    : boolean                             read GetMustCreateRenderProcessHandler  write FMustCreateRenderProcessHandler;
      property MustCreateLoadHandler             : boolean                             read GetMustCreateLoadHandler           write FMustCreateLoadHandler;
      property MustCreatePrintHandler            : boolean                             read GetMustCreatePrintHandler          write FMustCreatePrintHandler;
      property OsmodalLoop                       : boolean                                                                     write SetOsmodalLoop;
      property Status                            : TCefAplicationStatus                read FStatus;
      property MissingLibFiles                   : string                              read FMissingLibFiles;
      property WidevinePath                      : ustring                             read FWidevinePath                      write FWidevinePath;
      property MustFreeLibrary                   : boolean                             read FMustFreeLibrary                   write FMustFreeLibrary;
      property ChildProcessesCount               : integer                             read GetChildProcessesCount;
      property UsedMemory                        : uint64                              read GetUsedMemory;
      property TotalSystemMemory                 : uint64                              read GetTotalSystemMemory;
      property AvailableSystemMemory             : uint64                              read GetAvailableSystemMemory;
      property SystemMemoryLoad                  : cardinal                            read GetSystemMemoryLoad;
      property SupportedSchemes                  : TStringList                         read FSupportedSchemes;
      {$IFDEF LINUX}
      property XDisplay                          : PXDisplay                           read GetXDisplay;
      {$ENDIF}

      // ICefApp
      property OnRegCustomSchemes                : TOnRegisterCustomSchemesEvent       read FOnRegisterCustomSchemes           write FOnRegisterCustomSchemes;

      // ICefBrowserProcessHandler
      property OnGetCookieableSchemes            : TOnGetCookieableSchemesEvent        read FOnGetCookieableSchemes            write FOnGetCookieableSchemes;
      property OnContextInitialized              : TOnContextInitializedEvent          read FOnContextInitialized              write FOnContextInitialized;
      property OnBeforeChildProcessLaunch        : TOnBeforeChildProcessLaunchEvent    read FOnBeforeChildProcessLaunch        write FOnBeforeChildProcessLaunch;
      property OnScheduleMessagePumpWork         : TOnScheduleMessagePumpWorkEvent     read FOnScheduleMessagePumpWork         write FOnScheduleMessagePumpWork;
      property OnGetDefaultClient                : TOnGetDefaultClientEvent            read FOnGetDefaultClient                write FOnGetDefaultClient;

      // ICefResourceBundleHandler
      property OnGetLocalizedString              : TOnGetLocalizedStringEvent          read FOnGetLocalizedString              write FOnGetLocalizedString;
      property OnGetDataResource                 : TOnGetDataResourceEvent             read FOnGetDataResource                 write FOnGetDataResource;
      property OnGetDataResourceForScale         : TOnGetDataResourceForScaleEvent     read FOnGetDataResourceForScale         write FOnGetDataResourceForScale;

      // ICefRenderProcessHandler
      property OnWebKitInitialized               : TOnWebKitInitializedEvent           read FOnWebKitInitialized               write FOnWebKitInitialized;
      property OnBrowserCreated                  : TOnBrowserCreatedEvent              read FOnBrowserCreated                  write FOnBrowserCreated;
      property OnBrowserDestroyed                : TOnBrowserDestroyedEvent            read FOnBrowserDestroyed                write FOnBrowserDestroyed;
      property OnContextCreated                  : TOnContextCreatedEvent              read FOnContextCreated                  write FOnContextCreated;
      property OnContextReleased                 : TOnContextReleasedEvent             read FOnContextReleased                 write FOnContextReleased;
      property OnUncaughtException               : TOnUncaughtExceptionEvent           read FOnUncaughtException               write FOnUncaughtException;
      property OnFocusedNodeChanged              : TOnFocusedNodeChangedEvent          read FOnFocusedNodeChanged              write FOnFocusedNodeChanged;
      property OnProcessMessageReceived          : TOnProcessMessageReceivedEvent      read FOnProcessMessageReceived          write FOnProcessMessageReceived;

      // ICefRegisterCDMCallback
      property OnCDMRegistrationComplete         : TOnCDMRegistrationCompleteEvent     read FOnCDMRegistrationComplete         write FOnCDMRegistrationComplete;

      // ICefLoadHandler
      property OnLoadingStateChange              : TOnRenderLoadingStateChange         read FOnLoadingStateChange              write FOnLoadingStateChange;
      property OnLoadStart                       : TOnRenderLoadStart                  read FOnLoadStart                       write FOnLoadStart;
      property OnLoadEnd                         : TOnRenderLoadEnd                    read FOnLoadEnd                         write FOnLoadEnd;
      property OnLoadError                       : TOnRenderLoadError                  read FOnLoadError                       write FOnLoadError;

      // ICefPrintHandler
      {$IFDEF LINUX}
      property OnPrintStart                      : TOnPrintStartEvent                  read FOnPrintStart                      write FOnPrintStart;
      property OnPrintSettings                   : TOnPrintSettingsEvent               read FOnPrintSettings                   write FOnPrintSettings;
      property OnPrintDialog                     : TOnPrintDialogEvent                 read FOnPrintDialog                     write FOnPrintDialog;
      property OnPrintJob                        : TOnPrintJobEvent                    read FOnPrintJob                        write FOnPrintJob;
      property OnPrintReset                      : TOnPrintResetEvent                  read FOnPrintReset                      write FOnPrintReset;
      property OnGetPDFPaperSize                 : TOnGetPDFPaperSizeEvent             read FOnGetPDFPaperSize                 write FOnGetPDFPaperSize;
      {$ENDIF}
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
  {$ELSE}
    Math, {$IFDEF DELPHI14_UP}IOUtils,{$ENDIF} SysUtils,
    {$IFDEF FPC}
      {$IFDEF MSWINDOWS}jwatlhelp32, jwapsapi,{$ENDIF}
      {$IFDEF LINUX}lcltype, Forms, InterfaceBase,{$ENDIF}
    {$ELSE}
      TlHelp32, {$IFDEF MSWINDOWS}PSAPI,{$ENDIF}
    {$ENDIF}
  {$ENDIF}
  uCEFLibFunctions, uCEFMiscFunctions, uCEFCommandLine, uCEFConstants,
  uCEFSchemeHandlerFactory, uCEFCookieManager, uCEFApp, uCEFRegisterCDMCallback,
  uCEFCompletionCallback, uCEFWaitableEvent;

procedure DestroyGlobalCEFApp;
begin
  if (GlobalCEFApp <> nil) then FreeAndNil(GlobalCEFApp);
end;

constructor TCefApplicationCore.Create;
begin
  inherited Create;
  if GlobalCEFApp = nil then
    GlobalCEFApp := Self;

  FStatus                            := asLoading;
  FMissingLibFiles                   := '';
  FLibHandle                         := 0;
  FCache                             := '';
  FRootCache                         := '';
  FUserDataPath                      := '';
  FUserAgent                         := '';
  FProductVersion                    := '';
  FLocale                            := '';
  FLogFile                           := '';
  FBrowserSubprocessPath             := '';
  FFrameworkDirPath                  := '';
  FMainBundlePath                    := '';
  FChromeRuntime                     := False;
  FLogSeverity                       := LOGSEVERITY_DISABLE;
  FJavaScriptFlags                   := '';
  FResourcesDirPath                  := '';
  FLocalesDirPath                    := '';
  FSingleProcess                     := False;
  FNoSandbox                         := True;
  FCommandLineArgsDisabled           := False;
  FPackLoadingDisabled               := False;
  FRemoteDebuggingPort               := 0;
  FUncaughtExceptionStackSize        := 0;
  FPersistSessionCookies             := False;
  FPersistUserPreferences            := False;
  FIgnoreCertificateErrors           := False;
  FBackgroundColor                   := 0;
  FAcceptLanguageList                := '';
  FApplicationClientID               := '';
  FWindowsSandboxInfo                := nil;
  FWindowlessRenderingEnabled        := False;
  FMultiThreadedMessageLoop          := True;
  FExternalMessagePump               := False;
  FDeleteCache                       := False;
  FDeleteCookies                     := False;
  FEnableMediaStream                 := True;
  FEnableSpeechInput                 := True;
  FUseFakeUIForMediaStream           := False;
  FEnableUsermediaScreenCapturing    := False;
  FEnableGPU                         := False;
  FCustomCommandLines                := nil;
  FCustomCommandLineValues           := nil;
  FCheckCEFFiles                     := True;
  FSmoothScrolling                   := STATE_DEFAULT;
  FFastUnload                        := False;
  FDisableSafeBrowsing               := False;
  FOnRegisterCustomSchemes           := nil;
  FEnableHighDPISupport              := False;
  FMuteAudio                         := False;
  FSitePerProcess                    := False;
  FDisableWebSecurity                := False;
  FDisablePDFExtension               := False;
  FDisableSiteIsolationTrials        := False;
  FDisableChromeLoginPrompt          := False;
  FLogProcessInfo                    := False;
  FReRaiseExceptions                 := False;
  FLibLoaded                         := False;
  FShowMessageDlg                    := True;
  FMissingBinariesException          := False;
  FSetCurrentDir                     := False;
  FGlobalContextInitialized          := False;
  FCheckDevToolsResources            := True;
  FDisableExtensions                 := False;
  FDisableGPUCache                   := True;
  FLocalesRequired                   := '';
  FProcessType                       := ParseProcessType;
  FWidevinePath                      := '';
  FMustFreeLibrary                   := False;
  FAutoplayPolicy                    := appDefault;
  FDisableBackgroundNetworking       := False;
  FMetricsRecordingOnly              := False;
  FAllowFileAccessFromFiles          := False;
  FAllowRunningInsecureContent       := False;
  FPluginPolicy                      := PLUGIN_POLICY_SWITCH_ALLOW;
  FDefaultEncoding                   := '';
  FDisableJavascript                 := False;
  FEnableFeatures                    := '';
  FDisableFeatures                   := '';
  FEnableBlinkFeatures               := '';
  FDisableBlinkFeatures              := '';
  FForceFieldTrials                  := '';
  FForceFieldTrialParams             := '';
  FBlinkSettings                     := '';
  FSupportedSchemes                  := nil;
  FDisableNewBrowserInfoTimeout      := False;
  FDevToolsProtocolLogFile           := '';
  FForcedDeviceScaleFactor           := 0;
  FDisableZygote                     := False;

  FDisableJavascriptCloseWindows     := False;
  FDisableJavascriptAccessClipboard  := False;
  FDisableJavascriptDomPaste         := False;
  FAllowUniversalAccessFromFileUrls  := False;
  FDisableImageLoading               := False;
  FImageShrinkStandaloneToFit        := False;
  FDisableTextAreaResize             := False;
  FDisableTabToLinks                 := False;
  FDisablePlugins                    := False;
  FEnableProfanityFilter             := False;
  FDisableSpellChecking              := False;
  FOverrideSpellCheckLang            := '';
  FEnablePrintPreview                := False;
  FTouchEvents                       := STATE_DEFAULT;
  FDisableReadingFromCanvas          := False;
  FHyperlinkAuditing                 := True;

  FMustCreateResourceBundleHandler   := False;
  FMustCreateBrowserProcessHandler   := True;
  FMustCreateRenderProcessHandler    := False;
  FMustCreateLoadHandler             := False;
  FMustCreatePrintHandler            := False;

  // ICefBrowserProcessHandler
  FOnGetCookieableSchemes            := nil;
  FOnContextInitialized              := nil;
  FOnBeforeChildProcessLaunch        := nil;
  FOnScheduleMessagePumpWork         := nil;
  FOnGetDefaultClient                := nil;

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

  // ICefRegisterCDMCallback
  FOnCDMRegistrationComplete         := nil;

  // ICefLoadHandler
  FOnLoadingStateChange              := nil;
  FOnLoadStart                       := nil;
  FOnLoadEnd                         := nil;
  FOnLoadError                       := nil;

  // ICefPrintHandler
  FOnPrintStart                      := nil;
  FOnPrintSettings                   := nil;
  FOnPrintDialog                     := nil;
  FOnPrintJob                        := nil;
  FOnPrintReset                      := nil;
  FOnGetPDFPaperSize                 := nil;

  UpdateDeviceScaleFactor;

  FillChar(FAppSettings, SizeOf(TCefSettings), 0);
  FAppSettings.size := SizeOf(TCefSettings);

  FChromeVersionInfo.MajorVer    := CEF_CHROMEELF_VERSION_MAJOR;
  FChromeVersionInfo.MinorVer    := CEF_CHROMEELF_VERSION_MINOR;
  FChromeVersionInfo.Release     := CEF_CHROMEELF_VERSION_RELEASE;
  FChromeVersionInfo.Build       := CEF_CHROMEELF_VERSION_BUILD;

  {$IFDEF MSWINDOWS}
  if (FProcessType = ptBrowser) then
    GetDLLVersion(ChromeElfPath, FChromeVersionInfo);
  {$ENDIF}

  IsMultiThread := True;

  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
end;

destructor TCefApplicationCore.Destroy;
begin
  if (GlobalCEFApp = Self) then
    GlobalCEFApp := nil;

  try
    if (FProcessType = ptBrowser) then
      ShutDown;

    FreeLibcefLibrary;

    if (FCustomCommandLines      <> nil) then FreeAndNil(FCustomCommandLines);
    if (FCustomCommandLineValues <> nil) then FreeAndNil(FCustomCommandLineValues);
    if (FSupportedSchemes        <> nil) then FreeAndNil(FSupportedSchemes);
  finally
    inherited Destroy;
  end;
end;

procedure TCefApplicationCore.AfterConstruction;
begin
  inherited AfterConstruction;

  FCustomCommandLines      := TStringList.Create;
  FCustomCommandLineValues := TStringList.Create;
  FSupportedSchemes        := TStringList.Create;
end;

procedure TCefApplicationCore.AddCustomCommandLine(const aCommandLine, aValue : string);
begin
  if (FCustomCommandLines      <> nil) then FCustomCommandLines.Add(aCommandLine);
  if (FCustomCommandLineValues <> nil) then FCustomCommandLineValues.Add(aValue);
end;

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
      Result := LIBCEF_DLL;
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

procedure TCefApplicationCore.SetCache(const aValue : ustring);
begin
  FCache           := CustomAbsolutePath(aValue);
  FDisableGPUCache := (length(FCache) = 0);
end;

procedure TCefApplicationCore.SetRootCache(const aValue : ustring);
begin
  FRootCache := CustomAbsolutePath(aValue);
end;

procedure TCefApplicationCore.SetUserDataPath(const aValue : ustring);
begin
  FUserDataPath := CustomAbsolutePath(aValue);
end;

procedure TCefApplicationCore.SetBrowserSubprocessPath(const aValue : ustring);
begin
  FBrowserSubprocessPath := CustomAbsolutePath(aValue);
end;

procedure TCefApplicationCore.SetFrameworkDirPath(const aValue : ustring);
begin
  FFrameworkDirPath := CustomAbsolutePath(aValue, True);

  {$IFDEF MSWINDOWS}
  if (FProcessType = ptBrowser) then GetDLLVersion(ChromeElfPath, FChromeVersionInfo);
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

function TCefApplicationCore.CheckCEFLibrary : boolean;
var
  TempString, TempOldDir : string;
  TempMissingFrm, TempMissingRsc, TempMissingLoc, TempMissingSubProc : boolean;
  {$IFDEF MSWINDOWS}
  TempMachine : integer;
  TempVersionInfo : TFileVersionInfo;
  {$ENDIF}
begin
  Result := False;

  if not(FCheckCEFFiles) or (FProcessType <> ptBrowser) then
    Result := True
   else
    begin
      if FSetCurrentDir then
        begin
          TempOldDir := GetCurrentDir;
          chdir(GetModulePath);
        end;
      try
        TempMissingSubProc := not(CheckSubprocessPath(FBrowserSubprocessPath, FMissingLibFiles));
        TempMissingFrm     := not(CheckDLLs(FFrameworkDirPath, FMissingLibFiles));
        TempMissingRsc     := not(CheckResources(FResourcesDirPath, FMissingLibFiles, FCheckDevToolsResources, not(FDisableExtensions)));
        TempMissingLoc     := not(CheckLocales(FLocalesDirPath, FMissingLibFiles, FLocalesRequired));

        if TempMissingFrm or TempMissingRsc or TempMissingLoc or TempMissingSubProc then
          begin
            FStatus    := asErrorMissingFiles;
            TempString := 'CEF binaries missing !';

            if (length(FMissingLibFiles) > 0) then
              TempString := TempString + CRLF + CRLF +
                            'The missing files are :' + CRLF +
                            trim(FMissingLibFiles);

            ShowErrorMessageDlg(TempString);
          end
         else
          {$IFDEF MSWINDOWS}
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
                        FStatus    := asErrorDLLVersion;
                        TempString := 'Wrong CEF binaries !' +
                                      CRLF + CRLF +
                                      'Use the 32 bit CEF binaries with 32 bits applications only.';

                        ShowErrorMessageDlg(TempString);
                      end;

                  CEF_IMAGE_FILE_MACHINE_AMD64 :
                    if not(Is32BitProcess) then
                      Result := True
                     else

                      begin
                        FStatus    := asErrorDLLVersion;
                        TempString := 'Wrong CEF binaries !' +
                                      CRLF + CRLF +
                                      'Use the 64 bit CEF binaries with 64 bits applications only.';

                        ShowErrorMessageDlg(TempString);
                      end;

                  else
                    begin
                      FStatus    := asErrorDLLVersion;
                      TempString := 'Unknown CEF binaries !' +
                                    CRLF + CRLF +
                                    'Use only the CEF binaries specified in the CEF4Delphi Readme.md file at ' +
                                    CEF4DELPHI_URL;

                      ShowErrorMessageDlg(TempString);
                    end;
                end
               else
                Result := True;
            end
           else
            begin
              FStatus    := asErrorDLLVersion;
              TempString := 'Unsupported CEF version !' +
                            CRLF + CRLF +
                            'Use only the CEF binaries specified in the CEF4Delphi Readme.md file at ' +
                            CEF4DELPHI_URL;

              if GetDLLVersion(LibCefPath, TempVersionInfo) then
                TempString := TempString + CRLF + CRLF +
                              'Expected ' + LIBCEF_DLL + ' version : ' + LibCefVersion + CRLF +
                              'Found ' + LIBCEF_DLL + ' version : ' + FileVersionInfoToString(TempVersionInfo);

              ShowErrorMessageDlg(TempString);
            end;
          {$ELSE}
            begin
              Result := True;
            end;
          {$ENDIF}
      finally
        if FSetCurrentDir then chdir(TempOldDir);
      end;
    end;
end;

function TCefApplicationCore.StartMainProcess : boolean;
begin
  if (FStatus <> asLoading) then
    Result := False
   else
    if not(FSingleProcess) and (length(FBrowserSubprocessPath) > 0) then
      Result := MultiExeProcessing
     else
      Result := SingleExeProcessing;
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
      if not(FSingleProcess)        and
         (ProcessType <> ptBrowser) and
         LoadCEFlibrary             then
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

procedure TCefApplicationCore.SetOsmodalLoop(aValue : boolean);
begin
  if (FStatus = asInitialized) then cef_set_osmodal_loop(Ord(aValue));
end;

procedure TCefApplicationCore.UpdateDeviceScaleFactor;
begin
  if (FForcedDeviceScaleFactor <> 0) then
    FDeviceScaleFactor := FForcedDeviceScaleFactor
   else
    FDeviceScaleFactor := GetDeviceScaleFactor;
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

function TCefApplicationCore.ExecuteProcess(const aApp : ICefApp) : integer;
var
  TempArgs : TCefMainArgs;
begin
  Result := -1;
  try
    if (aApp <> nil) then
      begin
        {$WARN SYMBOL_PLATFORM OFF}
        {$IFDEF MSWINDOWS}
          TempArgs.instance := HINSTANCE{$IFDEF FPC}(){$ENDIF};
        {$ELSE}
          {$IFDEF LINUX}
            {$IFDEF FPC}
            TempArgs.argc := argc;
            TempArgs.argv := argv;
            {$ELSE}
            TempArgs.argc := ArgCount;
            TempArgs.argv := PPWideChar(ArgValues);
            {$ENDIF}
          {$ELSE}
            // TODO: Find a way to pass the arguments in MacOS
            {$IFDEF FPC}
            TempArgs.argc := 0;
            TempArgs.argv := 0;
            {$ELSE}
            TempArgs.argc := 0;
            TempArgs.argv := 0;
            {$ENDIF}
          {$ENDIF}
        {$ENDIF}
        {$WARN SYMBOL_PLATFORM ON}
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
  aSettings.chrome_runtime                          := Ord(FChromeRuntime);
  aSettings.multi_threaded_message_loop             := Ord(FMultiThreadedMessageLoop);
  aSettings.external_message_pump                   := Ord(FExternalMessagePump);
  aSettings.windowless_rendering_enabled            := Ord(FWindowlessRenderingEnabled);
  aSettings.command_line_args_disabled              := Ord(FCommandLineArgsDisabled);
  aSettings.cache_path                              := CefString(FCache);
  aSettings.root_cache_path                         := CefString(FRootCache);
  aSettings.user_data_path                          := CefString(FUserDataPath);
  aSettings.persist_session_cookies                 := Ord(FPersistSessionCookies);
  aSettings.persist_user_preferences                := Ord(FPersistUserPreferences);
  aSettings.user_agent                              := CefString(FUserAgent);
  aSettings.product_version                         := CefString(FProductVersion);
  aSettings.locale                                  := CefString(FLocale);
  aSettings.log_file                                := CefString(FLogFile);
  aSettings.log_severity                            := FLogSeverity;
  aSettings.javascript_flags                        := CefString(FJavaScriptFlags);
  aSettings.resources_dir_path                      := CefString(FResourcesDirPath);
  aSettings.locales_dir_path                        := CefString(FLocalesDirPath);
  aSettings.pack_loading_disabled                   := Ord(FPackLoadingDisabled);
  aSettings.remote_debugging_port                   := FRemoteDebuggingPort;
  aSettings.uncaught_exception_stack_size           := FUncaughtExceptionStackSize;
  aSettings.ignore_certificate_errors               := Ord(FIgnoreCertificateErrors);
  aSettings.background_color                        := FBackgroundColor;
  aSettings.accept_language_list                    := CefString(FAcceptLanguageList);
  aSettings.application_client_id_for_file_scanning := CefString(FApplicationClientID);
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
              DeleteCookiesDB(FCache)
             else
              if FDeleteCache then
                RenameAndDeleteDir(FCache, True);

          RegisterWidevineCDM;

          InitializeSettings(FAppSettings);

          {$IFDEF MSWINDOWS}
            TempArgs.instance := HINSTANCE{$IFDEF FPC}(){$ENDIF};
          {$ELSE}
            {$WARN SYMBOL_PLATFORM OFF}
            {$IFDEF LINUX}
              {$IFDEF FPC}
              TempArgs.argc := argc;
              TempArgs.argv := argv;
              {$ELSE}
              TempArgs.argc := ArgCount;
              TempArgs.argv := PPWideChar(ArgValues);
              {$ENDIF}
            {$ELSE}
              // TODO: Find a way to pass the arguments in MacOS
              {$IFDEF FPC}
              TempArgs.argc := 0;
              TempArgs.argv := 0;
              {$ELSE}
              TempArgs.argc := 0;
              TempArgs.argv := 0;
              {$ENDIF}
            {$ENDIF}
            {$WARN SYMBOL_PLATFORM ON}
          {$ENDIF}

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
begin
  TempFiles := TStringList.Create;

  try
    TempFiles.Add('Cookies');
    TempFiles.Add('Cookies-journal');

    MoveFileList(TempFiles, aSrcDirectory, aDstDirectory);
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
          TempNewDir := TempOldDir + '(' + inttostr(i) + ')';
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

procedure TCefApplicationCore.RegisterWidevineCDM;
var
  TempPath     : TCefString;
  TempCallback : ICefRegisterCDMCallback;
begin
  try
    try
      if FLibLoaded and (length(FWidevinePath) > 0) and DirectoryExists(FWidevinePath) then
        begin
          TempPath     := CefString(FWidevinePath);
          TempCallback := TCefCustomRegisterCDMCallback.Create(self);

          cef_register_widevine_cdm(@TempPath, TempCallback.Wrap);
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TCefApplicationCore.RegisterWidevineCDM', e) then raise;
    end;
  finally
    TempCallback := nil;
  end;
end;

procedure TCefApplicationCore.ShowErrorMessageDlg(const aError : string);
begin
  OutputDebugMessage(aError);

  if FShowMessageDlg then
    begin
      {$IFDEF MSWINDOWS}
      MessageBox(0, PChar(aError + #0), PChar('Error' + #0), MB_ICONERROR or MB_OK or MB_TOPMOST);
      {$ELSE}
        {$IFDEF LINUX}
          {$IFDEF FPC}
          if (WidgetSet <> nil) then
            Application.MessageBox(PChar(aError + #0), PChar('Error' + #0), MB_ICONERROR or MB_OK)
           else
            ShowX11Message(aError);
          {$ENDIF}
        {$ENDIF}
      {$ENDIF}
    end;

  if FMissingBinariesException then
    raise Exception.Create(aError);
end;

procedure TCefApplicationCore.UpdateSupportedSchemes(aIncludeDefaults : boolean);
var
  TempManager : ICefCookieManager;
begin
  try
    if (FSupportedSchemes       <> nil) and
       (FSupportedSchemes.Count  > 0)   then
      begin
        TempManager := TCefCookieManagerRef.Global(nil);

        if (TempManager <> nil) then
          TempManager.SetSupportedSchemes(FSupportedSchemes, aIncludeDefaults, nil);
      end;
  finally
    TempManager := nil;
  end;
end;

function TCefApplicationCore.ParseProcessType : TCefProcessType;
const
  TYPE_PARAMETER_NAME = '--type=';
var
  i, TempLen : integer;
  TempName, TempValue : string;
begin
  Result  := ptBrowser;
  i       := paramCount;
  TempLen := length(TYPE_PARAMETER_NAME);

  while (i >= 1) and (Result = ptBrowser) do
    begin
      TempName := copy(paramstr(i), 1, TempLen);

      if (CompareText(TempName, TYPE_PARAMETER_NAME) = 0) then
        begin
          TempValue := copy(paramstr(i), succ(TempLen), length(paramstr(i)));

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
                  Result := ptOther;
        end;

      dec(i);
    end;
end;

procedure TCefApplicationCore.Internal_OnContextInitialized;
begin
  FGlobalContextInitialized := True;
  UpdateSupportedSchemes;

  if assigned(FOnContextInitialized) then
    FOnContextInitialized();
end;

procedure TCefApplicationCore.Internal_OnBeforeChildProcessLaunch(const commandLine: ICefCommandLine);
begin
  if assigned(FOnBeforeChildProcessLaunch) then
    FOnBeforeChildProcessLaunch(commandLine);
end;

procedure TCefApplicationCore.Internal_OnScheduleMessagePumpWork(const delayMs: Int64);
begin
  if assigned(FOnScheduleMessagePumpWork) then
    FOnScheduleMessagePumpWork(delayMs);
end;

function TCefApplicationCore.Internal_GetLocalizedString(stringid: Integer; var stringVal: ustring) : boolean;
begin
  Result := False;

  // The stringId must be one of the values defined in the CEF file :
  // /include/cef_pack_strings.h
  // That file is available in the CEF binaries package.
  if assigned(FOnGetLocalizedString) then
    FOnGetLocalizedString(stringId, stringVal, Result);
end;

function TCefApplicationCore.Internal_GetDataResource(resourceId: Integer; var data: Pointer; var dataSize: NativeUInt) : boolean;
begin
  Result := False;

  // The resourceId must be one of the values defined in the CEF file :
  // /include/cef_pack_resources.h
  // That file is available in the CEF binaries package.
  if assigned(FOnGetDataResource) then
    FOnGetDataResource(resourceId, data, dataSize, Result);
end;

function TCefApplicationCore.Internal_GetDataResourceForScale(resourceId: Integer; scaleFactor: TCefScaleFactor; var data: Pointer; var dataSize: NativeUInt) : boolean;
begin
  Result := False;

  // The resourceId must be one of the values defined in the CEF file :
  // /include/cef_pack_resources.h
  // That file is available in the CEF binaries package.
  if assigned(FOnGetDataResourceForScale) then
    FOnGetDataResourceForScale(resourceId, scaleFactor, data, dataSize, Result);
end;

procedure TCefApplicationCore.Internal_OnWebKitInitialized;
begin
  if assigned(FOnWebKitInitialized) then
    FOnWebKitInitialized();
end;

procedure TCefApplicationCore.Internal_OnBrowserCreated(const browser: ICefBrowser; const extra_info: ICefDictionaryValue);
begin
  if assigned(FOnBrowserCreated) then
    FOnBrowserCreated(browser, extra_info);
end;

procedure TCefApplicationCore.Internal_OnBrowserDestroyed(const browser: ICefBrowser);
begin
  if assigned(FOnBrowserDestroyed) then
    FOnBrowserDestroyed(browser);
end;

procedure TCefApplicationCore.Internal_OnContextCreated(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context);
begin
  if assigned(FOnContextCreated) then
    FOnContextCreated(browser, frame, context);
end;

procedure TCefApplicationCore.Internal_OnContextReleased(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context);
begin
  if assigned(FOnContextReleased) then
    FOnContextReleased(browser, frame, context);
end;

procedure TCefApplicationCore.Internal_OnUncaughtException(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context; const exception: ICefV8Exception; const stackTrace: ICefV8StackTrace);
begin
  if assigned(FOnUncaughtException) then
    FOnUncaughtException(browser, frame, context, exception, stackTrace);
end;

procedure TCefApplicationCore.Internal_OnFocusedNodeChanged(const browser: ICefBrowser; const frame: ICefFrame; const node: ICefDomNode);
begin
  if assigned(FOnFocusedNodeChanged) then
    FOnFocusedNodeChanged(browser, frame, node);
end;

procedure TCefApplicationCore.Internal_OnProcessMessageReceived(const browser: ICefBrowser; const frame: ICefFrame; sourceProcess: TCefProcessId; const aMessage: ICefProcessMessage; var aHandled : boolean);
begin
  if assigned(FOnProcessMessageReceived) then
    FOnProcessMessageReceived(browser, frame, sourceProcess, aMessage, aHandled)
   else
    aHandled := False;
end;

procedure TCefApplicationCore.Internal_OnCDMRegistrationComplete(result : TCefCDMRegistrationError; const error_message : ustring);
begin
  if assigned(FOnCDMRegistrationComplete) then
    FOnCDMRegistrationComplete(result, error_message);
end;

procedure TCefApplicationCore.Internal_OnLoadingStateChange(const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
begin
  if assigned(FOnLoadingStateChange) then
    FOnLoadingStateChange(browser, isLoading, canGoBack, canGoForward);
end;

procedure TCefApplicationCore.Internal_OnLoadStart(const browser: ICefBrowser; const frame: ICefFrame; transitionType: TCefTransitionType);
begin
  if assigned(FOnLoadStart) then
    FOnLoadStart(browser, frame, transitionType);
end;

procedure TCefApplicationCore.Internal_OnLoadEnd(const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: Integer);
begin
  if assigned(FOnLoadEnd) then
    FOnLoadEnd(browser, frame, httpStatusCode);
end;

procedure TCefApplicationCore.Internal_OnLoadError(const browser: ICefBrowser; const frame: ICefFrame; errorCode: Integer; const errorText, failedUrl: ustring);
begin
  if assigned(FOnLoadError) then
    FOnLoadError(browser, frame, errorCode, errorText, failedUrl);
end;

procedure TCefApplicationCore.Internal_GetCookieableSchemes(var schemes: TStringList; var include_defaults : boolean);
begin
  if assigned(FOnGetCookieableSchemes) then
    FOnGetCookieableSchemes(schemes, include_defaults);
end;

procedure TCefApplicationCore.Internal_GetDefaultClient(var aClient : ICefClient);
begin
  if assigned(FOnGetDefaultClient) then
    FOnGetDefaultClient(aClient);
end;

procedure TCefApplicationCore.Internal_OnPrintStart(const browser: ICefBrowser);
begin
  if assigned(FOnPrintStart) then
    FOnPrintStart(browser);
end;

procedure TCefApplicationCore.Internal_OnPrintSettings(const browser: ICefBrowser; const settings: ICefPrintSettings; getDefaults: boolean);
begin
  if assigned(FOnPrintSettings) then
    FOnPrintSettings(browser, settings, getDefaults);
end;

procedure TCefApplicationCore.Internal_OnPrintDialog(const browser: ICefBrowser; hasSelection: boolean; const callback: ICefPrintDialogCallback; var aResult : boolean);
begin
  if assigned(FOnPrintDialog) then
    FOnPrintDialog(browser, hasSelection, callback, aResult);
end;

procedure TCefApplicationCore.Internal_OnPrintJob(const browser: ICefBrowser; const documentName, PDFFilePath: ustring; const callback: ICefPrintJobCallback; var aResult : boolean);
begin
  if assigned(FOnPrintJob) then
    FOnPrintJob(browser, documentName, PDFFilePath, callback, aResult);
end;

procedure TCefApplicationCore.Internal_OnPrintReset(const browser: ICefBrowser);
begin
  if assigned(FOnPrintReset) then
    FOnPrintReset(browser);
end;

procedure TCefApplicationCore.Internal_OnGetPDFPaperSize(deviceUnitsPerInch: Integer; var aResult : TCefSize);
begin
  if assigned(FOnGetPDFPaperSize) then
    FOnGetPDFPaperSize(deviceUnitsPerInch, aResult);
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
  TempFormatSettings : TFormatSettings;
begin
  ReplaceSwitch(aKeys, aValues, '--enable-media-stream', IntToStr(Ord(FEnableMediaStream)));
  ReplaceSwitch(aKeys, aValues, '--enable-speech-input', IntToStr(Ord(FEnableSpeechInput)));

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

  if FFastUnload then
    ReplaceSwitch(aKeys, aValues, '--enable-fast-unload');

  if FDisableGPUCache then
    ReplaceSwitch(aKeys, aValues, '--disable-gpu-shader-disk-cache');

  if FDisableSafeBrowsing then
    begin
      ReplaceSwitch(aKeys, aValues, '--disable-client-side-phishing-detection');
      ReplaceSwitch(aKeys, aValues, '--safebrowsing-disable-auto-update');
      ReplaceSwitch(aKeys, aValues, '--safebrowsing-disable-download-protection');
    end;

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

  if FEnablePrintPreview then
    ReplaceSwitch(aKeys, aValues, '--enable-print-preview');

  if FDisableNewBrowserInfoTimeout then
    ReplaceSwitch(aKeys, aValues, '--disable-new-browser-info-timeout');

  if (length(FDevToolsProtocolLogFile) > 0) then
    ReplaceSwitch(aKeys, aValues, '--devtools-protocol-log-file', FDevToolsProtocolLogFile);

  case FPluginPolicy of
    PLUGIN_POLICY_SWITCH_DETECT : ReplaceSwitch(aKeys, aValues, '--plugin-policy', 'detect');
    PLUGIN_POLICY_SWITCH_BLOCK  : ReplaceSwitch(aKeys, aValues, '--plugin-policy', 'block');
  end;

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

  if FDisablePlugins then
    ReplaceSwitch(aKeys, aValues, '--disable-plugins');

  if FEnableProfanityFilter then
    ReplaceSwitch(aKeys, aValues, '--enable-profanity-filter');

  if FDisableSpellChecking then
    ReplaceSwitch(aKeys, aValues, '--disable-spell-checking');

  if (length(FOverrideSpellCheckLang) > 0) then
    ReplaceSwitch(aKeys, aValues, '--override-spell-check-lang', FOverrideSpellCheckLang);

  // This is a workaround for the CEF issue #2899
  // https://bitbucket.org/chromiumembedded/cef/issues/2899/cefsettingsignore_certificate_errors-true
  if FIgnoreCertificateErrors then
    ReplaceSwitch(aKeys, aValues, '--ignore-certificate-errors');

  if (FForcedDeviceScaleFactor <> 0) then
    begin
      {$IFDEF FPC}
      TempFormatSettings.DecimalSeparator := '.';
      {$ELSE}
        {$IFDEF DELPHI26_UP}
        TempFormatSettings := TFormatSettings.Create('en-US');
        {$ELSE}
        GetLocaleFormatSettings(GetThreadLocale, TempFormatSettings);
        TempFormatSettings.DecimalSeparator := '.';
        {$ENDIF}
      {$ENDIF}
      ReplaceSwitch(aKeys, aValues, '--force-device-scale-factor', FloatToStr(FForcedDeviceScaleFactor, TempFormatSettings));
    end;

  if FDisableZygote then
    ReplaceSwitch(aKeys, aValues, '--no-zygote');

  // The list of features you can enable is here :
  // https://chromium.googlesource.com/chromium/src/+/master/chrome/common/chrome_features.cc
  if (length(FEnableFeatures) > 0) then
    AppendSwitch(aKeys, aValues, '--enable-features', FEnableFeatures);

  // The list of features you can disable is here :
  // https://chromium.googlesource.com/chromium/src/+/master/chrome/common/chrome_features.cc
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

procedure TCefApplicationCore.Internal_OnBeforeCommandLineProcessing(const processType : ustring;
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

procedure TCefApplicationCore.Internal_OnRegisterCustomSchemes(const registrar: TCefSchemeRegistrarRef);
begin
  if assigned(FOnRegisterCustomSchemes) then
    FOnRegisterCustomSchemes(registrar);
end;

function TCefApplicationCore.GetMustCreateResourceBundleHandler : boolean;
begin
  Result := ((FSingleProcess or (FProcessType in [ptBrowser, ptRenderer])) and
             (FMustCreateResourceBundleHandler or
              assigned(FOnGetLocalizedString)  or
              assigned(FOnGetDataResource)     or
              assigned(FOnGetDataResourceForScale)));
end;

function TCefApplicationCore.GetMustCreateBrowserProcessHandler : boolean;
begin
  Result := ((FSingleProcess or (FProcessType = ptBrowser)) and
             (FMustCreateBrowserProcessHandler        or
              MustCreatePrintHandler                  or
              assigned(FOnGetCookieableSchemes)       or
              assigned(FOnContextInitialized)         or
              assigned(FOnBeforeChildProcessLaunch)   or
              assigned(FOnScheduleMessagePumpWork))   or
              assigned(FOnGetDefaultClient));
end;

function TCefApplicationCore.GetMustCreateRenderProcessHandler : boolean;
begin
  Result := ((FSingleProcess or (FProcessType = ptRenderer)) and
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
  Result := ((FSingleProcess or (FProcessType = ptRenderer)) and
             (FMustCreateLoadHandler          or
              assigned(FOnLoadingStateChange) or
              assigned(FOnLoadStart)          or
              assigned(FOnLoadEnd)            or
              assigned(FOnLoadError)));
end;

function TCefApplicationCore.GetMustCreatePrintHandler : boolean;
begin
  {$IFDEF LINUX}
  Result := ((FSingleProcess or (FProcessType = ptBrowser)) and
             (FMustCreatePrintHandler    or
              assigned(FOnPrintStart)    or
              assigned(FOnPrintSettings) or
              assigned(FOnPrintDialog)   or
              assigned(FOnPrintJob)      or
              assigned(FOnPrintReset)    or
              assigned(FOnGetPDFPaperSize)));
  {$ELSE}
  Result := False;
  {$ENDIF}
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
              begin
                ZeroMemory(@TempMemCtrs, SizeOf(TProcessMemoryCounters));
                TempMemCtrs.cb := SizeOf(TProcessMemoryCounters);

                if GetProcessMemoryInfo(TempProcHWND, {$IFNDEF FPC}@{$ENDIF}TempMemCtrs, TempMemCtrs.cb) then
                  inc(Result, TempMemCtrs.WorkingSetSize);

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
  if GetGlobalMemoryStatusEx(TempMemStatus) then
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
  if GetGlobalMemoryStatusEx(TempMemStatus) then
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
  if GetGlobalMemoryStatusEx(TempMemStatus) then Result := TempMemStatus.dwMemoryLoad;
  {$ENDIF}
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
{$ENDIF}

function TCefApplicationCore.LoadCEFlibrary : boolean;
var
  TempOldDir, TempString : string;
  {$IFDEF MSWINDOWS}
  TempError : DWORD;
  {$ENDIF}
begin
  Result := False;

  if (FStatus <> asLoading) or FLibLoaded or (FLibHandle <> 0) then
    begin
      FStatus    := asErrorLoadingLibrary;
      TempString := 'GlobalCEFApp can only be initialized once per process.';

      ShowErrorMessageDlg(TempString);
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
      TempError  := GetLastError;
      TempString := 'Error loading ' + LIBCEF_DLL + CRLF + CRLF +
                    'Error code : 0x' + inttohex(TempError, 8) + CRLF +
                    SysErrorMessage(TempError);
      {$ELSE}
      TempString := 'Error loading ' + LIBCEF_DLL;
      {$ENDIF}

      ShowErrorMessageDlg(TempString);
      exit;
    end;


  if Load_cef_app_capi_h and
     Load_cef_browser_capi_h and
     Load_cef_command_line_capi_h and
     Load_cef_cookie_capi_h and
     Load_cef_crash_util_h and
     Load_cef_drag_data_capi_h and
     Load_cef_file_util_capi_h and
     Load_cef_image_capi_h and
     Load_cef_menu_model_capi_h and
     Load_cef_media_router_capi_h and
     Load_cef_origin_whitelist_capi_h and
     Load_cef_parser_capi_h and
     Load_cef_path_util_capi_h and
     Load_cef_print_settings_capi_h and
     Load_cef_process_message_capi_h and
     Load_cef_process_util_capi_h and
     Load_cef_request_capi_h and
     Load_cef_request_context_capi_h and
     Load_cef_resource_bundle_capi_h and
     Load_cef_response_capi_h and
     Load_cef_server_capi_h and
     Load_cef_scheme_capi_h and
     Load_cef_ssl_info_capi_h and
     Load_cef_stream_capi_h and
     Load_cef_task_capi_h and
     Load_cef_thread_capi_h and
     Load_cef_trace_capi_h and
     Load_cef_urlrequest_capi_h and
     Load_cef_v8_capi_h and
     Load_cef_values_capi_h and
     Load_cef_waitable_event_capi_h and
     Load_cef_web_plugin_capi_h and
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
     Load_cef_types_linux_h then
    begin
      FStatus    := asLoaded;
      FLibLoaded := True;
      Result     := True;

      if FLogProcessInfo       then CefDebugLog('Process started', CEF_LOG_SEVERITY_INFO);
      if FEnableHighDPISupport then cef_enable_highdpi_support();
    end
   else
    begin
      FStatus    := asErrorDLLVersion;
      TempString := 'Unsupported CEF version !' +
                    CRLF + CRLF +
                    'Use only the CEF binaries specified in the CEF4Delphi Readme.md file at ' +
                    CRLF + CEF4DELPHI_URL;

      ShowErrorMessageDlg(TempString);
    end;

  if FSetCurrentDir then chdir(TempOldDir);
end;

function TCefApplicationCore.Load_cef_app_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_initialize{$IFDEF FPC}){$ENDIF}             := GetProcAddress(FLibHandle, 'cef_initialize');
  {$IFDEF FPC}Pointer({$ENDIF}cef_shutdown{$IFDEF FPC}){$ENDIF}               := GetProcAddress(FLibHandle, 'cef_shutdown');
  {$IFDEF FPC}Pointer({$ENDIF}cef_execute_process{$IFDEF FPC}){$ENDIF}        := GetProcAddress(FLibHandle, 'cef_execute_process');
  {$IFDEF FPC}Pointer({$ENDIF}cef_do_message_loop_work{$IFDEF FPC}){$ENDIF}   := GetProcAddress(FLibHandle, 'cef_do_message_loop_work');
  {$IFDEF FPC}Pointer({$ENDIF}cef_run_message_loop{$IFDEF FPC}){$ENDIF}       := GetProcAddress(FLibHandle, 'cef_run_message_loop');
  {$IFDEF FPC}Pointer({$ENDIF}cef_quit_message_loop{$IFDEF FPC}){$ENDIF}      := GetProcAddress(FLibHandle, 'cef_quit_message_loop');
  {$IFDEF FPC}Pointer({$ENDIF}cef_set_osmodal_loop{$IFDEF FPC}){$ENDIF}       := GetProcAddress(FLibHandle, 'cef_set_osmodal_loop');
  {$IFDEF FPC}Pointer({$ENDIF}cef_enable_highdpi_support{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_enable_highdpi_support');

  Result := assigned(cef_initialize) and
            assigned(cef_shutdown) and
            assigned(cef_execute_process) and
            assigned(cef_do_message_loop_work) and
            assigned(cef_run_message_loop) and
            assigned(cef_quit_message_loop) and
            assigned(cef_set_osmodal_loop) and
            assigned(cef_enable_highdpi_support);
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

  Result := assigned(cef_parse_url) and
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

function TCefApplicationCore.Load_cef_server_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_server_create{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_server_create');

  Result := assigned(cef_server_create);
end;

function TCefApplicationCore.Load_cef_scheme_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_register_scheme_handler_factory{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_register_scheme_handler_factory');
  {$IFDEF FPC}Pointer({$ENDIF}cef_clear_scheme_handler_factories{$IFDEF FPC}){$ENDIF}  := GetProcAddress(FLibHandle, 'cef_clear_scheme_handler_factories');

  Result := assigned(cef_register_scheme_handler_factory) and
            assigned(cef_clear_scheme_handler_factories);
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
  {$IFDEF FPC}Pointer({$ENDIF}cef_v8context_get_current_context{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_v8context_get_current_context');
  {$IFDEF FPC}Pointer({$ENDIF}cef_v8context_get_entered_context{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_v8context_get_entered_context');
  {$IFDEF FPC}Pointer({$ENDIF}cef_v8context_in_context{$IFDEF FPC}){$ENDIF}          := GetProcAddress(FLibHandle, 'cef_v8context_in_context');
  {$IFDEF FPC}Pointer({$ENDIF}cef_v8value_create_undefined{$IFDEF FPC}){$ENDIF}      := GetProcAddress(FLibHandle, 'cef_v8value_create_undefined');
  {$IFDEF FPC}Pointer({$ENDIF}cef_v8value_create_null{$IFDEF FPC}){$ENDIF}           := GetProcAddress(FLibHandle, 'cef_v8value_create_null');
  {$IFDEF FPC}Pointer({$ENDIF}cef_v8value_create_bool{$IFDEF FPC}){$ENDIF}           := GetProcAddress(FLibHandle, 'cef_v8value_create_bool');
  {$IFDEF FPC}Pointer({$ENDIF}cef_v8value_create_int{$IFDEF FPC}){$ENDIF}            := GetProcAddress(FLibHandle, 'cef_v8value_create_int');
  {$IFDEF FPC}Pointer({$ENDIF}cef_v8value_create_uint{$IFDEF FPC}){$ENDIF}           := GetProcAddress(FLibHandle, 'cef_v8value_create_uint');
  {$IFDEF FPC}Pointer({$ENDIF}cef_v8value_create_double{$IFDEF FPC}){$ENDIF}         := GetProcAddress(FLibHandle, 'cef_v8value_create_double');
  {$IFDEF FPC}Pointer({$ENDIF}cef_v8value_create_date{$IFDEF FPC}){$ENDIF}           := GetProcAddress(FLibHandle, 'cef_v8value_create_date');
  {$IFDEF FPC}Pointer({$ENDIF}cef_v8value_create_string{$IFDEF FPC}){$ENDIF}         := GetProcAddress(FLibHandle, 'cef_v8value_create_string');
  {$IFDEF FPC}Pointer({$ENDIF}cef_v8value_create_object{$IFDEF FPC}){$ENDIF}         := GetProcAddress(FLibHandle, 'cef_v8value_create_object');
  {$IFDEF FPC}Pointer({$ENDIF}cef_v8value_create_array{$IFDEF FPC}){$ENDIF}          := GetProcAddress(FLibHandle, 'cef_v8value_create_array');
  {$IFDEF FPC}Pointer({$ENDIF}cef_v8value_create_array_buffer{$IFDEF FPC}){$ENDIF}   := GetProcAddress(FLibHandle, 'cef_v8value_create_array_buffer');
  {$IFDEF FPC}Pointer({$ENDIF}cef_v8value_create_function{$IFDEF FPC}){$ENDIF}       := GetProcAddress(FLibHandle, 'cef_v8value_create_function');
  {$IFDEF FPC}Pointer({$ENDIF}cef_v8stack_trace_get_current{$IFDEF FPC}){$ENDIF}     := GetProcAddress(FLibHandle, 'cef_v8stack_trace_get_current');
  {$IFDEF FPC}Pointer({$ENDIF}cef_register_extension{$IFDEF FPC}){$ENDIF}            := GetProcAddress(FLibHandle, 'cef_register_extension');

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
            assigned(cef_v8value_create_function) and
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

function TCefApplicationCore.Load_cef_web_plugin_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_visit_web_plugin_info{$IFDEF FPC}){$ENDIF}          := GetProcAddress(FLibHandle, 'cef_visit_web_plugin_info');
  {$IFDEF FPC}Pointer({$ENDIF}cef_refresh_web_plugins{$IFDEF FPC}){$ENDIF}            := GetProcAddress(FLibHandle, 'cef_refresh_web_plugins');
  {$IFDEF FPC}Pointer({$ENDIF}cef_unregister_internal_web_plugin{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_unregister_internal_web_plugin');
  {$IFDEF FPC}Pointer({$ENDIF}cef_register_web_plugin_crash{$IFDEF FPC}){$ENDIF}      := GetProcAddress(FLibHandle, 'cef_register_web_plugin_crash');
  {$IFDEF FPC}Pointer({$ENDIF}cef_is_web_plugin_unstable{$IFDEF FPC}){$ENDIF}         := GetProcAddress(FLibHandle, 'cef_is_web_plugin_unstable');
  {$IFDEF FPC}Pointer({$ENDIF}cef_register_widevine_cdm{$IFDEF FPC}){$ENDIF}          := GetProcAddress(FLibHandle, 'cef_register_widevine_cdm');

  Result := assigned(cef_visit_web_plugin_info) and
            assigned(cef_refresh_web_plugins) and
            assigned(cef_unregister_internal_web_plugin) and
            assigned(cef_register_web_plugin_crash) and
            assigned(cef_is_web_plugin_unstable) and
            assigned(cef_register_widevine_cdm);
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
  {$IFDEF FPC}Pointer({$ENDIF}cef_display_get_primary{$IFDEF FPC}){$ENDIF}         := GetProcAddress(FLibHandle, 'cef_display_get_primary');
  {$IFDEF FPC}Pointer({$ENDIF}cef_display_get_nearest_point{$IFDEF FPC}){$ENDIF}   := GetProcAddress(FLibHandle, 'cef_display_get_nearest_point');
  {$IFDEF FPC}Pointer({$ENDIF}cef_display_get_matching_bounds{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_display_get_matching_bounds');
  {$IFDEF FPC}Pointer({$ENDIF}cef_display_get_count{$IFDEF FPC}){$ENDIF}           := GetProcAddress(FLibHandle, 'cef_display_get_count');
  {$IFDEF FPC}Pointer({$ENDIF}cef_display_get_alls{$IFDEF FPC}){$ENDIF}            := GetProcAddress(FLibHandle, 'cef_display_get_alls');

  Result := assigned(cef_display_get_primary) and
            assigned(cef_display_get_nearest_point) and
            assigned(cef_display_get_matching_bounds) and
            assigned(cef_display_get_count) and
            assigned(cef_display_get_alls);
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
