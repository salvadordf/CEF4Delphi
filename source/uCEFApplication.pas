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

unit uCEFApplication;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
    {$IFDEF MSWINDOWS}WinApi.Windows,{$ENDIF} System.Classes, System.UITypes,
  {$ELSE}
    {$IFDEF MSWINDOWS}Windows,{$ENDIF} Classes, {$IFDEF FPC}dynlibs,{$ENDIF}
  {$ENDIF}
  uCEFTypes, uCEFInterfaces, uCEFBaseRefCounted, uCEFSchemeRegistrar;

const
  CEF_SUPPORTED_VERSION_MAJOR   = 3;
  CEF_SUPPORTED_VERSION_MINOR   = 3497;
  CEF_SUPPORTED_VERSION_RELEASE = 1831;
  CEF_SUPPORTED_VERSION_BUILD   = 0;

  CEF_CHROMEELF_VERSION_MAJOR   = 69;
  CEF_CHROMEELF_VERSION_MINOR   = 0;
  CEF_CHROMEELF_VERSION_RELEASE = 3497;
  CEF_CHROMEELF_VERSION_BUILD   = 100;

  LIBCEF_DLL                    = 'libcef.dll';
  CHROMEELF_DLL                 = 'chrome_elf.dll';

type
  TCefApplication = class
    protected
      FCache                         : ustring;
      FCookies                       : ustring;
      FUserDataPath                  : ustring;
      FUserAgent                     : ustring;
      FProductVersion                : ustring;
      FLocale                        : ustring;
      FLocalesRequired               : ustring;
      FLogFile                       : ustring;
      FBrowserSubprocessPath         : ustring;
      FCustomFlashPath               : ustring;
      FFrameworkDirPath              : ustring;
      FLogSeverity                   : TCefLogSeverity;
      FJavaScriptFlags               : ustring;
      FResourcesDirPath              : ustring;
      FLocalesDirPath                : ustring;
      FSingleProcess                 : Boolean;
      FNoSandbox                     : Boolean;
      FCommandLineArgsDisabled       : Boolean;
      FPackLoadingDisabled           : Boolean;
      FRemoteDebuggingPort           : Integer;
      FUncaughtExceptionStackSize    : Integer;
      FPersistSessionCookies         : Boolean;
      FPersistUserPreferences        : boolean;
      FIgnoreCertificateErrors       : Boolean;
      FEnableNetSecurityExpiration   : boolean;
      FBackgroundColor               : TCefColor;
      FAcceptLanguageList            : ustring;
      FWindowsSandboxInfo            : Pointer;
      FWindowlessRenderingEnabled    : Boolean;
      FMultiThreadedMessageLoop      : boolean;
      FExternalMessagePump           : boolean;
      FDeleteCache                   : boolean;
      FDeleteCookies                 : boolean;
      FCustomCommandLines            : TStringList;
      FCustomCommandLineValues       : TStringList;
      FFlashEnabled                  : boolean;
      FEnableMediaStream             : boolean;
      FEnableSpeechInput             : boolean;
      FEnableGPU                     : boolean;
      FCheckCEFFiles                 : boolean;
      FLibLoaded                     : boolean;
      FSmoothScrolling               : TCefState;
      FFastUnload                    : boolean;
      FDisableSafeBrowsing           : boolean;
      FEnableHighDPISupport          : boolean;
      FMuteAudio                     : boolean;
      FReRaiseExceptions             : boolean;
      FShowMessageDlg                : boolean;
      FSetCurrentDir                 : boolean;
      FGlobalContextInitialized      : boolean;
      FSitePerProcess                : boolean;
      FDisableWebSecurity            : boolean;
      FDisablePDFExtension           : boolean;
      FChromeVersionInfo             : TFileVersionInfo;
      {$IFDEF FPC}
      FLibHandle                     : TLibHandle;
      {$ELSE}
      FLibHandle                     : THandle;
      {$ENDIF}
      FOnRegisterCustomSchemes       : TOnRegisterCustomSchemesEvent;
      FAppSettings                   : TCefSettings;
      FDeviceScaleFactor             : single;
      FCheckDevToolsResources        : boolean;
      FDisableGPUCache               : boolean;
      FStatus                        : TCefAplicationStatus;
      FMissingLibFiles               : string;
      FProcessType                   : TCefProcessType;
      FShutdownWaitTime              : cardinal;
      FWidevinePath                  : ustring;
      FMustFreeLibrary               : boolean;
      FAutoplayPolicy                : TCefAutoplayPolicy;

      FMustCreateResourceBundleHandler : boolean;
      FMustCreateBrowserProcessHandler : boolean;
      FMustCreateRenderProcessHandler  : boolean;

      // ICefBrowserProcessHandler
      FOnContextInitialized          : TOnContextInitializedEvent;
      FOnBeforeChildProcessLaunch    : TOnBeforeChildProcessLaunchEvent;
      FOnRenderProcessThreadCreated  : TOnRenderProcessThreadCreatedEvent;
      FOnScheduleMessagePumpWork     : TOnScheduleMessagePumpWorkEvent;

      // ICefResourceBundleHandler
      FOnGetLocalizedString          : TOnGetLocalizedStringEvent;
      FOnGetDataResource             : TOnGetDataResourceEvent;
      FOnGetDataResourceForScale     : TOnGetDataResourceForScaleEvent;

      // ICefRenderProcessHandler
      FOnRenderThreadCreated         : TOnRenderThreadCreatedEvent;
      FOnWebKitInitialized           : TOnWebKitInitializedEvent;
      FOnBrowserCreated              : TOnBrowserCreatedEvent;
      FOnBrowserDestroyed            : TOnBrowserDestroyedEvent;
      FOnContextCreated              : TOnContextCreatedEvent;
      FOnContextReleased             : TOnContextReleasedEvent;
      FOnUncaughtException           : TOnUncaughtExceptionEvent;
      FOnFocusedNodeChanged          : TOnFocusedNodeChangedEvent;
      FOnProcessMessageReceived      : TOnProcessMessageReceivedEvent;

      // ICefRegisterCDMCallback
      FOnCDMRegistrationComplete     : TOnCDMRegistrationCompleteEvent;

      procedure SetCache(const aValue : ustring);
      procedure SetCookies(const aValue : ustring);
      procedure SetUserDataPath(const aValue : ustring);
      procedure SetBrowserSubprocessPath(const aValue : ustring);
      procedure SetFrameworkDirPath(const aValue : ustring);
      procedure SetResourcesDirPath(const aValue : ustring);
      procedure SetLocalesDirPath(const aValue : ustring);
      procedure SetOsmodalLoop(aValue : boolean);

      function  GetChromeVersion : string;
      function  GetLibCefVersion : string;
      function  GetLibCefPath : string;
      function  GetChromeElfPath : string;
      function  GetMustCreateResourceBundleHandler : boolean;
      function  GetMustCreateBrowserProcessHandler : boolean;
      function  GetMustCreateRenderProcessHandler : boolean;
      function  GetGlobalContextInitialized : boolean;
      function  GetChildProcessesCount : integer;

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
      procedure RenameAndDeleteDir(const aDirectory : string);
      function  MultiExeProcessing : boolean;
      function  SingleExeProcessing : boolean;
      function  CheckCEFLibrary : boolean;
      procedure RegisterWidevineCDM;
      function  FindFlashDLL(var aFileName : string) : boolean;
      procedure ShowErrorMessageDlg(const aError : string); virtual;
      function  ParseProcessType : TCefProcessType;

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
      procedure   UpdateDeviceScaleFactor;

      // Internal procedures. Only TInternalApp, TCefCustomBrowserProcessHandler,
      // ICefResourceBundleHandler, ICefRenderProcessHandler and ICefRegisterCDMCallback
      // should use them.
      procedure   Internal_OnBeforeCommandLineProcessing(const processType: ustring; const commandLine: ICefCommandLine);
      procedure   Internal_OnRegisterCustomSchemes(const registrar: TCefSchemeRegistrarRef);
      procedure   Internal_OnContextInitialized;
      procedure   Internal_OnBeforeChildProcessLaunch(const commandLine: ICefCommandLine);
      procedure   Internal_OnRenderProcessThreadCreated(const extraInfo: ICefListValue);
      procedure   Internal_OnScheduleMessagePumpWork(const delayMs: Int64);
      function    Internal_GetLocalizedString(stringId: Integer; var stringVal: ustring) : boolean;
      function    Internal_GetDataResource(resourceId: Integer; var data: Pointer; var dataSize: NativeUInt) : boolean;
      function    Internal_GetDataResourceForScale(resourceId: Integer; scaleFactor: TCefScaleFactor; var data: Pointer; var dataSize: NativeUInt) : boolean;
      procedure   Internal_OnRenderThreadCreated(const extraInfo: ICefListValue);
      procedure   Internal_OnWebKitInitialized;
      procedure   Internal_OnBrowserCreated(const browser: ICefBrowser);
      procedure   Internal_OnBrowserDestroyed(const browser: ICefBrowser);
      procedure   Internal_OnContextCreated(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context);
      procedure   Internal_OnContextReleased(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context);
      procedure   Internal_OnUncaughtException(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context; const exception: ICefV8Exception; const stackTrace: ICefV8StackTrace);
      procedure   Internal_OnFocusedNodeChanged(const browser: ICefBrowser; const frame: ICefFrame; const node: ICefDomNode);
      procedure   Internal_OnProcessMessageReceived(const browser: ICefBrowser; sourceProcess: TCefProcessId; const aMessage: ICefProcessMessage; var aHandled : boolean);
      procedure   Internal_OnCDMRegistrationComplete(result : TCefCDMRegistrationError; const error_message : ustring);

      property Cache                             : ustring                             read FCache                             write SetCache;
      property Cookies                           : ustring                             read FCookies                           write SetCookies;
      property UserDataPath                      : ustring                             read FUserDataPath                      write SetUserDataPath;
      property UserAgent                         : ustring                             read FUserAgent                         write FUserAgent;
      property ProductVersion                    : ustring                             read FProductVersion                    write FProductVersion;
      property Locale                            : ustring                             read FLocale                            write FLocale;
      property LogFile                           : ustring                             read FLogFile                           write FLogFile;
      property BrowserSubprocessPath             : ustring                             read FBrowserSubprocessPath             write SetBrowserSubprocessPath;
      property FrameworkDirPath                  : ustring                             read FFrameworkDirPath                  write SetFrameworkDirPath;
      property LogSeverity                       : TCefLogSeverity                     read FLogSeverity                       write FLogSeverity;
      property JavaScriptFlags                   : ustring                             read FJavaScriptFlags                   write FJavaScriptFlags;
      property ResourcesDirPath                  : ustring                             read FResourcesDirPath                  write SetResourcesDirPath;
      property LocalesDirPath                    : ustring                             read FLocalesDirPath                    write SetLocalesDirPath;
      property SingleProcess                     : Boolean                             read FSingleProcess                     write FSingleProcess;
      property NoSandbox                         : Boolean                             read FNoSandbox                         write FNoSandbox;
      property CommandLineArgsDisabled           : Boolean                             read FCommandLineArgsDisabled           write FCommandLineArgsDisabled;
      property PackLoadingDisabled               : Boolean                             read FPackLoadingDisabled               write FPackLoadingDisabled;
      property RemoteDebuggingPort               : Integer                             read FRemoteDebuggingPort               write FRemoteDebuggingPort;
      property UncaughtExceptionStackSize        : Integer                             read FUncaughtExceptionStackSize        write FUncaughtExceptionStackSize;
      property PersistSessionCookies             : Boolean                             read FPersistSessionCookies             write FPersistSessionCookies;
      property PersistUserPreferences            : Boolean                             read FPersistUserPreferences            write FPersistUserPreferences;
      property IgnoreCertificateErrors           : Boolean                             read FIgnoreCertificateErrors           write FIgnoreCertificateErrors;
      property EnableNetSecurityExpiration       : boolean                             read FEnableNetSecurityExpiration       write FEnableNetSecurityExpiration;
      property BackgroundColor                   : TCefColor                           read FBackgroundColor                   write FBackgroundColor;
      property AcceptLanguageList                : ustring                             read FAcceptLanguageList                write FAcceptLanguageList;
      property WindowsSandboxInfo                : Pointer                             read FWindowsSandboxInfo                write FWindowsSandboxInfo;
      property WindowlessRenderingEnabled        : Boolean                             read FWindowlessRenderingEnabled        write FWindowlessRenderingEnabled;
      property MultiThreadedMessageLoop          : boolean                             read FMultiThreadedMessageLoop          write FMultiThreadedMessageLoop;
      property ExternalMessagePump               : boolean                             read FExternalMessagePump               write FExternalMessagePump;
      property DeleteCache                       : boolean                             read FDeleteCache                       write FDeleteCache;
      property DeleteCookies                     : boolean                             read FDeleteCookies                     write FDeleteCookies;
      property FlashEnabled                      : boolean                             read FFlashEnabled                      write FFlashEnabled;
      property EnableMediaStream                 : boolean                             read FEnableMediaStream                 write FEnableMediaStream;
      property EnableSpeechInput                 : boolean                             read FEnableSpeechInput                 write FEnableSpeechInput;
      property EnableGPU                         : boolean                             read FEnableGPU                         write FEnableGPU;
      property CheckCEFFiles                     : boolean                             read FCheckCEFFiles                     write FCheckCEFFiles;
      property ShowMessageDlg                    : boolean                             read FShowMessageDlg                    write FShowMessageDlg;
      property SetCurrentDir                     : boolean                             read FSetCurrentDir                     write FSetCurrentDir;
      property GlobalContextInitialized          : boolean                             read GetGlobalContextInitialized;
      property ChromeMajorVer                    : uint16                              read FChromeVersionInfo.MajorVer;
      property ChromeMinorVer                    : uint16                              read FChromeVersionInfo.MinorVer;
      property ChromeRelease                     : uint16                              read FChromeVersionInfo.Release;
      property ChromeBuild                       : uint16                              read FChromeVersionInfo.Build;
      property ChromeVersion                     : string                              read GetChromeVersion;
      property LibCefVersion                     : string                              read GetLibCefVersion;
      property LibCefPath                        : string                              read GetLibCefPath;
      property ChromeElfPath                     : string                              read GetChromeElfPath;
      property SmoothScrolling                   : TCefState                           read FSmoothScrolling                   write FSmoothScrolling;
      property FastUnload                        : boolean                             read FFastUnload                        write FFastUnload;
      property DisableSafeBrowsing               : boolean                             read FDisableSafeBrowsing               write FDisableSafeBrowsing;
      property LibLoaded                         : boolean                             read FLibLoaded;
      property EnableHighDPISupport              : boolean                             read FEnableHighDPISupport              write FEnableHighDPISupport;
      property MuteAudio                         : boolean                             read FMuteAudio                         write FMuteAudio;
      property SitePerProcess                    : boolean                             read FSitePerProcess                    write FSitePerProcess;
      property DisableWebSecurity                : boolean                             read FDisableWebSecurity                write FDisableWebSecurity;
      property DisablePDFExtension               : boolean                             read FDisablePDFExtension               write FDisablePDFExtension;
      property ReRaiseExceptions                 : boolean                             read FReRaiseExceptions                 write FReRaiseExceptions;
      property DeviceScaleFactor                 : single                              read FDeviceScaleFactor;
      property CheckDevToolsResources            : boolean                             read FCheckDevToolsResources            write FCheckDevToolsResources;
      property LocalesRequired                   : ustring                             read FLocalesRequired                   write FLocalesRequired;
      property CustomFlashPath                   : ustring                             read FCustomFlashPath                   write FCustomFlashPath;
      property ProcessType                       : TCefProcessType                     read FProcessType;
      property MustCreateResourceBundleHandler   : boolean                             read GetMustCreateResourceBundleHandler write FMustCreateResourceBundleHandler;
      property MustCreateBrowserProcessHandler   : boolean                             read GetMustCreateBrowserProcessHandler write FMustCreateBrowserProcessHandler;
      property MustCreateRenderProcessHandler    : boolean                             read GetMustCreateRenderProcessHandler  write FMustCreateRenderProcessHandler;
      property OsmodalLoop                       : boolean                                                                     write SetOsmodalLoop;
      property Status                            : TCefAplicationStatus                read FStatus;
      property MissingLibFiles                   : string                              read FMissingLibFiles;
      property ShutdownWaitTime                  : cardinal                            read FShutdownWaitTime                  write FShutdownWaitTime;
      property WidevinePath                      : ustring                             read FWidevinePath                      write FWidevinePath;
      property MustFreeLibrary                   : boolean                             read FMustFreeLibrary                   write FMustFreeLibrary;
      property AutoplayPolicy                    : TCefAutoplayPolicy                  read FAutoplayPolicy                    write FAutoplayPolicy;
      property ChildProcessesCount               : integer                             read GetChildProcessesCount;

      property OnRegCustomSchemes                : TOnRegisterCustomSchemesEvent       read FOnRegisterCustomSchemes           write FOnRegisterCustomSchemes;

      // ICefBrowserProcessHandler
      property OnContextInitialized              : TOnContextInitializedEvent          read FOnContextInitialized              write FOnContextInitialized;
      property OnBeforeChildProcessLaunch        : TOnBeforeChildProcessLaunchEvent    read FOnBeforeChildProcessLaunch        write FOnBeforeChildProcessLaunch;
      property OnRenderProcessThreadCreated      : TOnRenderProcessThreadCreatedEvent  read FOnRenderProcessThreadCreated      write FOnRenderProcessThreadCreated;
      property OnScheduleMessagePumpWork         : TOnScheduleMessagePumpWorkEvent     read FOnScheduleMessagePumpWork         write FOnScheduleMessagePumpWork;

      // ICefResourceBundleHandler
      property OnGetLocalizedString              : TOnGetLocalizedStringEvent          read FOnGetLocalizedString              write FOnGetLocalizedString;
      property OnGetDataResource                 : TOnGetDataResourceEvent             read FOnGetDataResource                 write FOnGetDataResource;
      property OnGetDataResourceForScale         : TOnGetDataResourceForScaleEvent     read FOnGetDataResourceForScale         write FOnGetDataResourceForScale;

      // ICefRenderProcessHandler
      property OnRenderThreadCreated             : TOnRenderThreadCreatedEvent         read FOnRenderThreadCreated             write FOnRenderThreadCreated;
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
  end;

  TCEFCookieInitializerThread = class(TThread)
    protected
      FCookies               : string;
      FPersistSessionCookies : boolean;

      procedure Execute; override;

    public
      constructor Create(const aCookies : string; aPersistSessionCookies : boolean);
  end;

  TCEFDirectoryDeleterThread = class(TThread)
    protected
      FDirectory : string;

      procedure Execute; override;

    public
      constructor Create(const aDirectory : string);
  end;

var
  GlobalCEFApp : TCefApplication = nil;

procedure DestroyGlobalCEFApp;

implementation

uses
  {$IFDEF DELPHI16_UP}
  System.Math, System.IOUtils, System.SysUtils, {$IFDEF MSWINDOWS}WinApi.TlHelp32,{$ENDIF}
  {$ELSE}
    Math, {$IFDEF DELPHI14_UP}IOUtils,{$ENDIF} SysUtils,
    {$IFDEF FPC}
      {$IFDEF MSWINDOWS}jwatlhelp32,{$ENDIF}
    {$ELSE}
      TlHelp32,
    {$ENDIF}
  {$ENDIF}
  uCEFLibFunctions, uCEFMiscFunctions, uCEFCommandLine, uCEFConstants,
  uCEFSchemeHandlerFactory, uCEFCookieManager, uCEFApp, uCEFRegisterCDMCallback,
  uCEFCompletionCallback, uCEFWaitableEvent;

procedure DestroyGlobalCEFApp;
begin
  if (GlobalCEFApp <> nil) then FreeAndNil(GlobalCEFApp);
end;

constructor TCefApplication.Create;
begin
  inherited Create;

  FStatus                        := asLoading;
  FMissingLibFiles               := '';
  FLibHandle                     := 0;
  FCache                         := '';
  FCookies                       := '';
  FUserDataPath                  := '';
  FUserAgent                     := '';
  FProductVersion                := '';
  FLocale                        := '';
  FLogFile                       := '';
  FBrowserSubprocessPath         := '';
  FCustomFlashPath               := '';
  FFrameworkDirPath              := '';
  FLogSeverity                   := LOGSEVERITY_DISABLE;
  FJavaScriptFlags               := '';
  FResourcesDirPath              := '';
  FLocalesDirPath                := '';
  FSingleProcess                 := False;
  FNoSandbox                     := True;
  FCommandLineArgsDisabled       := False;
  FPackLoadingDisabled           := False;
  FRemoteDebuggingPort           := 0;
  FUncaughtExceptionStackSize    := 0;
  FPersistSessionCookies         := False;
  FPersistUserPreferences        := False;
  FIgnoreCertificateErrors       := False;
  FEnableNetSecurityExpiration   := False;
  FBackgroundColor               := 0;
  FAcceptLanguageList            := '';
  FWindowsSandboxInfo            := nil;
  FWindowlessRenderingEnabled    := False;
  FMultiThreadedMessageLoop      := True;
  FExternalMessagePump           := False;
  FDeleteCache                   := False;
  FDeleteCookies                 := False;
  FFlashEnabled                  := True;
  FEnableMediaStream             := True;
  FEnableSpeechInput             := True;
  FEnableGPU                     := False;
  FCustomCommandLines            := nil;
  FCustomCommandLineValues       := nil;
  FCheckCEFFiles                 := True;
  FSmoothScrolling               := STATE_DEFAULT;
  FFastUnload                    := False;
  FDisableSafeBrowsing           := False;
  FOnRegisterCustomSchemes       := nil;
  FEnableHighDPISupport          := False;
  FMuteAudio                     := False;
  FSitePerProcess                := True;
  FDisableWebSecurity            := False;
  FDisablePDFExtension           := False;
  FReRaiseExceptions             := False;
  FLibLoaded                     := False;
  FShowMessageDlg                := True;
  FSetCurrentDir                 := False;
  FGlobalContextInitialized      := False;
  FCheckDevToolsResources        := True;
  FDisableGPUCache               := True;
  FLocalesRequired               := '';
  FProcessType                   := ParseProcessType;
  FShutdownWaitTime              := 0;
  FWidevinePath                  := '';
  FMustFreeLibrary               := False;
  FAutoplayPolicy                := appDefault;

  FMustCreateResourceBundleHandler := False;
  FMustCreateBrowserProcessHandler := True;
  FMustCreateRenderProcessHandler  := False;

  // ICefBrowserProcessHandler
  FOnContextInitialized          := nil;
  FOnBeforeChildProcessLaunch    := nil;
  FOnRenderProcessThreadCreated  := nil;
  FOnScheduleMessagePumpWork     := nil;

  // ICefResourceBundleHandler
  FOnGetLocalizedString          := nil;
  FOnGetDataResource             := nil;
  FOnGetDataResourceForScale     := nil;

  // ICefRenderProcessHandler
  FOnRenderThreadCreated         := nil;
  FOnWebKitInitialized           := nil;
  FOnBrowserCreated              := nil;
  FOnBrowserDestroyed            := nil;
  FOnContextCreated              := nil;
  FOnContextReleased             := nil;
  FOnUncaughtException           := nil;
  FOnFocusedNodeChanged          := nil;
  FOnProcessMessageReceived      := nil;

  // ICefRegisterCDMCallback
  FOnCDMRegistrationComplete     := nil;

  UpdateDeviceScaleFactor;

  FillChar(FAppSettings, SizeOf(TCefSettings), 0);
  FAppSettings.size := SizeOf(TCefSettings);

  FChromeVersionInfo.MajorVer    := CEF_CHROMEELF_VERSION_MAJOR;
  FChromeVersionInfo.MinorVer    := CEF_CHROMEELF_VERSION_MINOR;
  FChromeVersionInfo.Release     := CEF_CHROMEELF_VERSION_RELEASE;
  FChromeVersionInfo.Build       := CEF_CHROMEELF_VERSION_BUILD;

  if (FProcessType = ptBrowser) then GetDLLVersion(ChromeElfPath, FChromeVersionInfo);

  IsMultiThread := True;

  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
end;

destructor TCefApplication.Destroy;
begin
  try
    if (FProcessType = ptBrowser) then
      begin
        if (FShutdownWaitTime > 0) then sleep(FShutdownWaitTime);

        ShutDown;
      end;

    FreeLibcefLibrary;

    if (FCustomCommandLines      <> nil) then FreeAndNil(FCustomCommandLines);
    if (FCustomCommandLineValues <> nil) then FreeAndNil(FCustomCommandLineValues);
  finally
    inherited Destroy;
  end;
end;

procedure TCefApplication.AfterConstruction;
begin
  inherited AfterConstruction;

  FCustomCommandLines      := TStringList.Create;
  FCustomCommandLineValues := TStringList.Create;
end;

procedure TCefApplication.AddCustomCommandLine(const aCommandLine, aValue : string);
begin
  if (FCustomCommandLines      <> nil) then FCustomCommandLines.Add(aCommandLine);
  if (FCustomCommandLineValues <> nil) then FCustomCommandLineValues.Add(aValue);
end;

// This function must only be called by the main executable when the application
// is configured to use a different executable for the subprocesses.
// The process calling ths function must be the browser process.
function TCefApplication.MultiExeProcessing : boolean;
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
          Result  := InitializeLibrary(TempApp);
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TCefApplication.MultiExeProcessing', e) then raise;
    end;
  finally
    TempApp := nil;
  end;
end;

// This function will be called by all processes when the application is configured
// to use the same executable for all the processes : browser, render, etc.
function TCefApplication.SingleExeProcessing : boolean;
var
  TempApp : ICefApp;
begin
  Result  := False;
  TempApp := nil;

  try
    if CheckCEFLibrary and LoadCEFlibrary then
      begin
        TempApp := TCustomCefApp.Create(self);
        Result  := (ExecuteProcess(TempApp) < 0) and InitializeLibrary(TempApp);
      end;
  finally
    TempApp := nil;
  end;
end;

function TCefApplication.GetChromeVersion : string;
begin
  Result := FileVersionInfoToString(FChromeVersionInfo);
end;

function TCefApplication.GetLibCefVersion : string;
begin
  Result := IntToStr(CEF_SUPPORTED_VERSION_MAJOR)    + '.' +
            IntToStr(CEF_SUPPORTED_VERSION_MINOR)    + '.' +
            IntToStr(CEF_SUPPORTED_VERSION_RELEASE)  + '.' +
            IntToStr(CEF_SUPPORTED_VERSION_BUILD);
end;

function TCefApplication.GetLibCefPath : string;
begin
  if (length(FFrameworkDirPath) > 0) then
    Result := IncludeTrailingPathDelimiter(FFrameworkDirPath) + LIBCEF_DLL
   else
    Result := LIBCEF_DLL;
end;

function TCefApplication.GetChromeElfPath : string;
begin
  if (length(FFrameworkDirPath) > 0) then
    Result := IncludeTrailingPathDelimiter(FFrameworkDirPath) + CHROMEELF_DLL
   else
    Result := CHROMEELF_DLL;
end;

procedure TCefApplication.SetCache(const aValue : ustring);
begin
  if (length(aValue) > 0) then
    begin
      if CustomPathIsRelative(aValue) then
        FCache := GetModulePath + aValue
       else
        FCache := aValue;
    end
   else
    FCache := '';

  FDisableGPUCache := (length(FCache) = 0);
end;

procedure TCefApplication.SetCookies(const aValue : ustring);
begin
  if (length(aValue) > 0) then
    begin
      if CustomPathIsRelative(aValue) then
        FCookies := GetModulePath + aValue
       else
        FCookies := aValue;
    end
   else
    FCookies := '';
end;

procedure TCefApplication.SetUserDataPath(const aValue : ustring);
begin
  if (length(aValue) > 0) then
    begin
      if CustomPathIsRelative(aValue) then
        FUserDataPath := GetModulePath + aValue
       else
        FUserDataPath := aValue;
    end
   else
    FUserDataPath := '';
end;

procedure TCefApplication.SetBrowserSubprocessPath(const aValue : ustring);
var
  TempPath : string;
begin
  if (length(aValue) > 0) then
    begin
      if CustomPathIsRelative(aValue) then
        TempPath := GetModulePath + aValue
       else
        TempPath := aValue;

      if FileExists(TempPath) then
        FBrowserSubprocessPath := TempPath
       else
        FBrowserSubprocessPath := '';
    end
   else
    FBrowserSubprocessPath := '';
end;

procedure TCefApplication.SetFrameworkDirPath(const aValue : ustring);
var
  TempPath : string;
begin
  if (length(aValue) > 0) then
    begin
      if CustomPathIsRelative(aValue) then
        TempPath := GetModulePath + aValue
       else
        TempPath := aValue;

      if DirectoryExists(TempPath) then
        FFrameworkDirPath := TempPath
       else
        FFrameworkDirPath := '';
    end
   else
    FFrameworkDirPath := '';

  if (FProcessType = ptBrowser) then GetDLLVersion(ChromeElfPath, FChromeVersionInfo);
end;

procedure TCefApplication.SetResourcesDirPath(const aValue : ustring);
var
  TempPath : string;
begin
  if (length(aValue) > 0) then
    begin
      if CustomPathIsRelative(aValue) then
        TempPath := GetModulePath + aValue
       else
        TempPath := aValue;

      if DirectoryExists(TempPath) then
        FResourcesDirPath := TempPath
       else
        FResourcesDirPath := '';
    end
   else
    FResourcesDirPath := '';
end;

procedure TCefApplication.SetLocalesDirPath(const aValue : ustring);
var
  TempPath : string;
begin
  if (length(aValue) > 0) then
    begin
      if CustomPathIsRelative(aValue) then
        TempPath := GetModulePath + aValue
       else
        TempPath := aValue;

      if DirectoryExists(TempPath) then
        FLocalesDirPath := TempPath
       else
        FLocalesDirPath := '';
    end
   else
    FLocalesDirPath := '';
end;

function TCefApplication.CheckCEFLibrary : boolean;
var
  TempString, TempOldDir : string;
  TempMissingFrm, TempMissingRsc, TempMissingLoc : boolean;
  TempMachine : integer;
  TempVersionInfo : TFileVersionInfo;
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

      TempMissingFrm := not(CheckDLLs(FFrameworkDirPath, FMissingLibFiles));
      TempMissingRsc := not(CheckResources(FResourcesDirPath, FMissingLibFiles, FCheckDevToolsResources));
      TempMissingLoc := not(CheckLocales(FLocalesDirPath, FMissingLibFiles, FLocalesRequired));

      if TempMissingFrm or TempMissingRsc or TempMissingLoc then
        begin
          FStatus    := asErrorMissingFiles;
          TempString := 'CEF3 binaries missing !';

          if (length(FMissingLibFiles) > 0) then
            TempString := TempString + CRLF + CRLF +
                          'The missing files are :' + CRLF +
                          trim(FMissingLibFiles);

          ShowErrorMessageDlg(TempString);
        end
       else
        if CheckDLLVersion(LibCefPath,
                           CEF_SUPPORTED_VERSION_MAJOR,
                           CEF_SUPPORTED_VERSION_MINOR,
                           CEF_SUPPORTED_VERSION_RELEASE,
                           CEF_SUPPORTED_VERSION_BUILD) then
          begin
            if GetDLLHeaderMachine(LibCefPath, TempMachine) then
              case TempMachine of
                IMAGE_FILE_MACHINE_I386 :
                  if Is32BitProcess then
                    Result := True
                   else
                    begin
                      FStatus    := asErrorDLLVersion;
                      TempString := 'Wrong CEF3 binaries !' +
                                    CRLF + CRLF +
                                    'Use the 32 bit CEF3 binaries with 32 bits applications only.';

                      ShowErrorMessageDlg(TempString);
                    end;

                IMAGE_FILE_MACHINE_AMD64 :
                  if not(Is32BitProcess) then
                    Result := True
                   else
                    begin
                      FStatus    := asErrorDLLVersion;
                      TempString := 'Wrong CEF3 binaries !' +
                                    CRLF + CRLF +
                                    'Use the 64 bit CEF3 binaries with 64 bits applications only.';

                      ShowErrorMessageDlg(TempString);
                    end;

                else
                  begin
                    FStatus    := asErrorDLLVersion;
                    TempString := 'Unknown CEF3 binaries !' +
                                  CRLF + CRLF +
                                  'Use only the CEF3 binaries specified in the CEF4Delphi Readme.md file at ' +
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
                          'Use only the CEF3 binaries specified in the CEF4Delphi Readme.md file at ' +
                          CEF4DELPHI_URL;

            if GetDLLVersion(LibCefPath, TempVersionInfo) then
              TempString := TempString + CRLF + CRLF +
                            'Expected ' + LIBCEF_DLL + ' version : ' + LibCefVersion + CRLF +
                            'Found ' + LIBCEF_DLL + ' version : ' + FileVersionInfoToString(TempVersionInfo);

            ShowErrorMessageDlg(TempString);
          end;

      if FSetCurrentDir then chdir(TempOldDir);
    end;
end;

function TCefApplication.StartMainProcess : boolean;
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
function TCefApplication.StartSubProcess : boolean;
var
  TempApp : ICefApp;
begin
  Result  := False;
  TempApp := nil;

  try
    if not(FSingleProcess)        and
       (ProcessType <> ptBrowser) and
       LoadCEFlibrary             then
      begin
        TempApp := TCustomCefApp.Create(self);
        Result  := (ExecuteProcess(TempApp) >= 0);
      end;
  finally
    TempApp := nil;
  end;
end;

procedure TCefApplication.DoMessageLoopWork;
begin
  if FLibLoaded and
     not(FMultiThreadedMessageLoop) and
     FExternalMessagePump then
    cef_do_message_loop_work();
end;

procedure TCefApplication.RunMessageLoop;
begin
  if FLibLoaded and
     not(FMultiThreadedMessageLoop) and
     not(FExternalMessagePump) then
    cef_run_message_loop();
end;

procedure TCefApplication.QuitMessageLoop;
begin
  if FLibLoaded and
     not(FMultiThreadedMessageLoop) and
     not(FExternalMessagePump) then
    cef_quit_message_loop();
end;

procedure TCefApplication.SetOsmodalLoop(aValue : boolean);
begin
  if FLibLoaded then cef_set_osmodal_loop(Ord(aValue));
end;

procedure TCefApplication.UpdateDeviceScaleFactor;
begin
  FDeviceScaleFactor := GetDeviceScaleFactor;
end;

procedure TCefApplication.ShutDown;
begin
  try
    FStatus := asShuttingDown;
    if FLibLoaded then cef_shutdown();
  except
    on e : exception do
      if CustomExceptionHandler('TCefApplication.ShutDown', e) then raise;
  end;
end;

procedure TCefApplication.FreeLibcefLibrary;
begin
  try
    try
      if FMustFreeLibrary and (FLibHandle <> 0) then FreeLibrary(FLibHandle);
    except
      on e : exception do
        if CustomExceptionHandler('TCefApplication.FreeLibcefLibrary', e) then raise;
    end;
  finally
    FLibHandle := 0;
    FLibLoaded := False;
    FStatus    := asUnloaded;
  end;
end;

function TCefApplication.ExecuteProcess(const aApp : ICefApp) : integer;
var
  TempArgs : TCefMainArgs;
begin
  Result := -1;
  try
    if (aApp <> nil) then
      begin
        {$IFDEF MSWINDOWS}
        TempArgs.instance := HINSTANCE{$IFDEF FPC}(){$ENDIF};
        {$ELSE}
        TempArgs.argc     := argc;
        TempArgs.argv     := argv;
        {$ENDIF}

        Result := cef_execute_process(@TempArgs, aApp.Wrap, FWindowsSandboxInfo);
      end;
  except
    on e : exception do
      begin
        FStatus := asErrorExecutingProcess;
        if CustomExceptionHandler('TCefApplication.ExecuteProcess', e) then raise;
      end;
  end;
end;

procedure TCefApplication.InitializeSettings(var aSettings : TCefSettings);
begin
  aSettings.size                            := SizeOf(TCefSettings);
  aSettings.no_sandbox                      := Ord(FNoSandbox);
  aSettings.browser_subprocess_path         := CefString(FBrowserSubprocessPath);
  aSettings.framework_dir_path              := CefString(FFrameworkDirPath);
  aSettings.multi_threaded_message_loop     := Ord(FMultiThreadedMessageLoop);
  aSettings.external_message_pump           := Ord(FExternalMessagePump);
  aSettings.windowless_rendering_enabled    := Ord(FWindowlessRenderingEnabled);
  aSettings.command_line_args_disabled      := Ord(FCommandLineArgsDisabled);
  aSettings.cache_path                      := CefString(FCache);
  aSettings.user_data_path                  := CefString(FUserDataPath);
  aSettings.persist_session_cookies         := Ord(FPersistSessionCookies);
  aSettings.persist_user_preferences        := Ord(FPersistUserPreferences);
  aSettings.user_agent                      := CefString(FUserAgent);
  aSettings.product_version                 := CefString(FProductVersion);
  aSettings.locale                          := CefString(FLocale);
  aSettings.log_file                        := CefString(FLogFile);
  aSettings.log_severity                    := FLogSeverity;
  aSettings.javascript_flags                := CefString(FJavaScriptFlags);
  aSettings.resources_dir_path              := CefString(FResourcesDirPath);
  aSettings.locales_dir_path                := CefString(FLocalesDirPath);
  aSettings.pack_loading_disabled           := Ord(FPackLoadingDisabled);
  aSettings.remote_debugging_port           := FRemoteDebuggingPort;
  aSettings.uncaught_exception_stack_size   := FUncaughtExceptionStackSize;
  aSettings.ignore_certificate_errors       := Ord(FIgnoreCertificateErrors);
  aSettings.enable_net_security_expiration  := Ord(FEnableNetSecurityExpiration);
  aSettings.background_color                := FBackgroundColor;
  aSettings.accept_language_list            := CefString(FAcceptLanguageList);
end;

function TCefApplication.InitializeLibrary(const aApp : ICefApp) : boolean;
var
  TempArgs : TCefMainArgs;
begin
  Result := False;

  try
    try
      if (aApp <> nil) then
        begin
          if FDeleteCache   then RenameAndDeleteDir(FCache);
          if FDeleteCookies then RenameAndDeleteDir(FCookies);

          RegisterWidevineCDM;

          InitializeSettings(FAppSettings);

          {$IFDEF MSWINDOWS}
          TempArgs.instance := HINSTANCE{$IFDEF FPC}(){$ENDIF};
          {$ELSE}
          TempArgs.argc     := argc;
          TempArgs.argv     := argv;
          {$ENDIF}

          if (cef_initialize(@TempArgs, @FAppSettings, aApp.Wrap, FWindowsSandboxInfo) <> 0) then
            begin
              Result  := True;
              FStatus := asInitialized;
            end;
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TCefApplication.InitializeLibrary', e) then raise;
    end;
  finally
    if not(Result) then FStatus := asErrorInitializingLibrary;
  end;
end;

procedure TCefApplication.RenameAndDeleteDir(const aDirectory : string);
var
  TempOldDir, TempNewDir : string;
  i : integer;
  TempThread : TCEFDirectoryDeleterThread;
begin
  try
    if (length(aDirectory) = 0) or not(DirectoryExists(aDirectory)) then exit;

    TempOldDir := ExcludeTrailingPathDelimiter(aDirectory);

    if (Pos(PathDelim, TempOldDir {$IFNDEF FPC}, 1{$ENDIF}) > 0) and
       (length(ExtractFileName(TempOldDir)) > 0) then
      begin
        i := 0;

        repeat
          inc(i);
          TempNewDir := TempOldDir + '(' + inttostr(i) + ')';
        until not(DirectoryExists(TempNewDir));

        if MoveFileW(PWideChar(TempOldDir + chr(0)), PWideChar(TempNewDir + chr(0))) then
          begin
            TempThread := TCEFDirectoryDeleterThread.Create(TempNewDir);
            {$IFDEF DELPHI14_UP}
            TempThread.Start;
            {$ELSE}
            TempThread.Resume;
            {$ENDIF}
          end
         else
          DeleteDirContents(aDirectory);
      end
     else
      DeleteDirContents(aDirectory);
  except
    on e : exception do
      if CustomExceptionHandler('TCefApplication.RenameAndDeleteDir', e) then raise;
  end;
end;

procedure TCefApplication.RegisterWidevineCDM;
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
        if CustomExceptionHandler('TCefApplication.RegisterWidevineCDM', e) then raise;
    end;
  finally
    TempCallback := nil;
  end;
end;

function TCefApplication.FindFlashDLL(var aFileName : string) : boolean;
var
  TempSearchRec : TSearchRec;
  TempProductName, TempPath : string;
begin
  Result    := False;
  aFileName := '';

  try
    if (length(FCustomFlashPath) > 0) then
      begin
        TempPath := IncludeTrailingPathDelimiter(FCustomFlashPath);

        if (FindFirst(TempPath + '*.dll', faAnyFile, TempSearchRec) = 0) then
          begin
            repeat
              if (TempSearchRec.Attr <> faDirectory) and
                 GetStringFileInfo(TempPath + TempSearchRec.Name, 'ProductName', TempProductName) and
                 (CompareText(TempProductName, 'Shockwave Flash') = 0) then
                begin
                  aFileName := TempPath + TempSearchRec.Name;
                  Result    := True;
                end;
            until Result or (FindNext(TempSearchRec) <> 0);

            FindClose(TempSearchRec);
          end;
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TCefApplication.FindFlashDLL', e) then raise;
  end;
end;

procedure TCefApplication.ShowErrorMessageDlg(const aError : string);
begin
  OutputDebugMessage(aError);

  if FShowMessageDlg then
    begin
      {$IFDEF MSWINDOWS}
      MessageBox(0, PChar(aError + #0), PChar('Error' + #0), MB_ICONERROR or MB_OK or MB_TOPMOST);
      {$ENDIF}
    end;
end;

function TCefApplication.ParseProcessType : TCefProcessType;
const
  TYPE_PARAMETER_NAME = '--type=';
var
  i, TempLen : integer;
  TempName, TempValue : string;
begin
  Result  := ptBrowser;
  i       := pred(paramCount);
  TempLen := length(TYPE_PARAMETER_NAME);

  while (i >= 0) and (Result = ptBrowser) do
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
                Result := ptOther;
        end;

      dec(i);
    end;
end;

procedure TCefApplication.Internal_OnContextInitialized;
var
  TempThread : TCEFCookieInitializerThread;
begin
  FGlobalContextInitialized := True;

  if (length(FCookies) > 0) then
    begin
      TempThread := TCEFCookieInitializerThread.Create(FCookies, FPersistSessionCookies);
      {$IFDEF DELPHI14_UP}
      TempThread.Start;
      {$ELSE}
      TempThread.Resume;
      {$ENDIF}
    end;

  if assigned(FOnContextInitialized) then FOnContextInitialized();
end;

procedure TCefApplication.Internal_OnBeforeChildProcessLaunch(const commandLine: ICefCommandLine);
begin
  if assigned(FOnBeforeChildProcessLaunch) then FOnBeforeChildProcessLaunch(commandLine);
end;

procedure TCefApplication.Internal_OnRenderProcessThreadCreated(const extraInfo: ICefListValue);
begin
  if assigned(FOnRenderProcessThreadCreated) then FOnRenderProcessThreadCreated(extraInfo);
end;

procedure TCefApplication.Internal_OnScheduleMessagePumpWork(const delayMs: Int64);
begin
  if assigned(FOnScheduleMessagePumpWork) then FOnScheduleMessagePumpWork(delayMs);
end;

function TCefApplication.Internal_GetLocalizedString(stringid: Integer; var stringVal: ustring) : boolean;
begin
  Result := False;

  // The stringId must be one of the values defined in the CEF file :
  // /include/cef_pack_strings.h
  // That file is available in the CEF3 binaries package.
  if assigned(FOnGetLocalizedString) then FOnGetLocalizedString(stringId, stringVal, Result);
end;

function TCefApplication.Internal_GetDataResource(resourceId: Integer; var data: Pointer; var dataSize: NativeUInt) : boolean;
begin
  Result := False;

  // The resourceId must be one of the values defined in the CEF file :
  // /include/cef_pack_resources.h
  // That file is available in the CEF3 binaries package.
  if assigned(FOnGetDataResource) then FOnGetDataResource(resourceId, data, dataSize, Result);
end;

function TCefApplication.Internal_GetDataResourceForScale(resourceId: Integer; scaleFactor: TCefScaleFactor; var data: Pointer; var dataSize: NativeUInt) : boolean;
begin
  Result := False;

  // The resourceId must be one of the values defined in the CEF file :
  // /include/cef_pack_resources.h
  // That file is available in the CEF3 binaries package.
  if assigned(FOnGetDataResourceForScale) then FOnGetDataResourceForScale(resourceId, scaleFactor, data, dataSize, Result);
end;

procedure TCefApplication.Internal_OnRenderThreadCreated(const extraInfo: ICefListValue);
begin
  if assigned(FOnRenderThreadCreated) then FOnRenderThreadCreated(extraInfo);
end;

procedure TCefApplication.Internal_OnWebKitInitialized;
begin
  if assigned(FOnWebKitInitialized) then FOnWebKitInitialized();
end;

procedure TCefApplication.Internal_OnBrowserCreated(const browser: ICefBrowser);
begin
  if assigned(FOnBrowserCreated) then FOnBrowserCreated(browser);
end;

procedure TCefApplication.Internal_OnBrowserDestroyed(const browser: ICefBrowser);
begin
  if assigned(FOnBrowserDestroyed) then FOnBrowserDestroyed(browser);
end;

procedure TCefApplication.Internal_OnContextCreated(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context);
begin
  if assigned(FOnContextCreated) then FOnContextCreated(browser, frame, context);
end;

procedure TCefApplication.Internal_OnContextReleased(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context);
begin
  if assigned(FOnContextReleased) then FOnContextReleased(browser, frame, context);
end;

procedure TCefApplication.Internal_OnUncaughtException(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context; const exception: ICefV8Exception; const stackTrace: ICefV8StackTrace);
begin
  if assigned(FOnUncaughtException) then FOnUncaughtException(browser, frame, context, exception, stackTrace);
end;

procedure TCefApplication.Internal_OnFocusedNodeChanged(const browser: ICefBrowser; const frame: ICefFrame; const node: ICefDomNode);
begin
  if assigned(FOnFocusedNodeChanged) then FOnFocusedNodeChanged(browser, frame, node);
end;

procedure TCefApplication.Internal_OnProcessMessageReceived(const browser: ICefBrowser; sourceProcess: TCefProcessId; const aMessage: ICefProcessMessage; var aHandled : boolean);
begin
  if assigned(FOnProcessMessageReceived) then
    FOnProcessMessageReceived(browser, sourceProcess, aMessage, aHandled)
   else
    aHandled := False;
end;

procedure TCefApplication.Internal_OnCDMRegistrationComplete(result : TCefCDMRegistrationError; const error_message : ustring);
begin
  if assigned(FOnCDMRegistrationComplete) then FOnCDMRegistrationComplete(result, error_message);
end;

procedure TCefApplication.Internal_OnBeforeCommandLineProcessing(const processType : ustring;
                                                                 const commandLine : ICefCommandLine);
var
  i : integer;
  TempVersionInfo : TFileVersionInfo;
  TempFileName : string;
begin
  if (commandLine <> nil) and (FProcessType = ptBrowser) and (processType = '') then
    begin
      if FindFlashDLL(TempFileName) and
         GetDLLVersion(TempFileName, TempVersionInfo) then
        begin
          if FEnableGPU then commandLine.AppendSwitch('--enable-gpu-plugin');

          commandLine.AppendSwitch('--enable-accelerated-plugins');
          commandLine.AppendSwitchWithValue('--ppapi-flash-path',    TempFileName);
          commandLine.AppendSwitchWithValue('--ppapi-flash-version', FileVersionInfoToString(TempVersionInfo));
        end
       else
        if FFlashEnabled then
          begin
            if FEnableGPU then commandLine.AppendSwitch('--enable-gpu-plugin');

            commandLine.AppendSwitch('--enable-accelerated-plugins');
            commandLine.AppendSwitch('--enable-system-flash');
          end;

      commandLine.AppendSwitchWithValue('--enable-media-stream',     IntToStr(Ord(FEnableMediaStream)));
      commandLine.AppendSwitchWithValue('--enable-speech-input',     IntToStr(Ord(FEnableSpeechInput)));

      if not(FEnableGPU) then
        begin
          commandLine.AppendSwitch('--disable-gpu');
          commandLine.AppendSwitch('--disable-gpu-compositing');
        end;

      if FSingleProcess then
        commandLine.AppendSwitch('--single-process');

      case FSmoothScrolling of
        STATE_ENABLED  : commandLine.AppendSwitch('--enable-smooth-scrolling');
        STATE_DISABLED : commandLine.AppendSwitch('--disable-smooth-scrolling');
      end;

      case FAutoplayPolicy of
        appDocumentUserActivationRequired    :
          commandLine.AppendSwitchWithValue('--autoplay-policy', 'document-user-activation-required');

        appNoUserGestureRequired             :
          commandLine.AppendSwitchWithValue('--autoplay-policy', 'no-user-gesture-required');

        appUserGestureRequired               :
          commandLine.AppendSwitchWithValue('--autoplay-policy', 'user-gesture-required');

        appUserGestureRequiredForCrossOrigin :
          commandLine.AppendSwitchWithValue('--autoplay-policy', 'user-gesture-required-for-cross-origin');
      end;

      if FFastUnload then
        commandLine.AppendSwitch('--enable-fast-unload');

      if FDisableGPUCache then
        commandLine.AppendSwitch('--disable-gpu-shader-disk-cache');

      if FDisableSafeBrowsing then
        begin
          commandLine.AppendSwitch('--disable-client-side-phishing-detection');
          commandLine.AppendSwitch('--safebrowsing-disable-auto-update');
          commandLine.AppendSwitch('--safebrowsing-disable-download-protection');
        end;

      if FMuteAudio then
        commandLine.AppendSwitch('--mute-audio');

      if FDisableWebSecurity then
        commandLine.AppendSwitch('--disable-web-security');

      if FDisablePDFExtension then
        commandLine.AppendSwitch('--disable-pdf-extension');

      if FSitePerProcess then
        commandLine.AppendSwitch('--site-per-process');

      if (FCustomCommandLines       <> nil) and
         (FCustomCommandLineValues  <> nil) and
         (FCustomCommandLines.Count =  FCustomCommandLineValues.Count) then
        begin
          i := 0;

          while (i < FCustomCommandLines.Count) do
            begin
              if (length(FCustomCommandLines[i]) > 0) then
                begin
                  if (length(FCustomCommandLineValues[i]) > 0) then
                    commandLine.AppendSwitchWithValue(FCustomCommandLines[i], FCustomCommandLineValues[i])
                   else
                    commandLine.AppendSwitch(FCustomCommandLines[i]);
                end;

              inc(i);
            end;
        end;
    end;
end;

procedure TCefApplication.Internal_OnRegisterCustomSchemes(const registrar: TCefSchemeRegistrarRef);
begin
  if assigned(FOnRegisterCustomSchemes) then FOnRegisterCustomSchemes(registrar);
end;

function TCefApplication.GetMustCreateResourceBundleHandler : boolean;
begin
  Result := FSingleProcess or
            ((FProcessType in [ptBrowser, ptRenderer]) and
             (FMustCreateResourceBundleHandler or
              assigned(FOnGetLocalizedString)  or
              assigned(FOnGetDataResource)     or
              assigned(FOnGetDataResourceForScale)));
end;

function TCefApplication.GetMustCreateBrowserProcessHandler : boolean;
begin
  Result := FSingleProcess or
            ((FProcessType = ptBrowser) and
             (FMustCreateBrowserProcessHandler        or
              assigned(FOnContextInitialized)         or
              assigned(FOnBeforeChildProcessLaunch)   or
              assigned(FOnRenderProcessThreadCreated) or
              assigned(FOnScheduleMessagePumpWork)));
end;

function TCefApplication.GetMustCreateRenderProcessHandler : boolean;
begin
  Result := FSingleProcess or
            ((FProcessType = ptRenderer) and
             (FMustCreateRenderProcessHandler     or
              assigned(FOnRenderThreadCreated)    or
              assigned(FOnWebKitInitialized)      or
              assigned(FOnBrowserCreated)         or
              assigned(FOnBrowserDestroyed)       or
              assigned(FOnContextCreated)         or
              assigned(FOnContextReleased)        or
              assigned(FOnUncaughtException)      or
              assigned(FOnFocusedNodeChanged)     or
              assigned(FOnProcessMessageReceived)));
end;

function TCefApplication.GetGlobalContextInitialized : boolean;
begin
  Result := FGlobalContextInitialized or not(MustCreateBrowserProcessHandler);
end;

function TCefApplication.GetChildProcessesCount : integer;
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
  TempHandle         := CreateToolHelp32SnapShot(TH32CS_SNAPPROCESS, 0);
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

function TCefApplication.LoadCEFlibrary : boolean;
var
  TempOldDir, TempString : string;
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
  FLibHandle := LoadLibraryEx(PChar(LibCefPath), 0, LOAD_WITH_ALTERED_SEARCH_PATH);
  {$ELSE}
  FLibHandle := LoadLibrary(PChar(LibCefPath));
  {$ENDIF}

  if (FLibHandle = 0) then
    begin
      FStatus    := asErrorLoadingLibrary;
      TempString := 'Error loading libcef.dll' + CRLF + CRLF +
                    'Error code : 0x' + inttohex(GetLastError, 8);

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

      if FEnableHighDPISupport then cef_enable_highdpi_support();
    end
   else
    begin
      FStatus    := asErrorDLLVersion;
      TempString := 'Unsupported CEF version !' +
                    CRLF + CRLF +
                    'Use only the CEF3 binaries specified in the CEF4Delphi Readme.md file at ' +
                    CRLF + CEF4DELPHI_URL;

      ShowErrorMessageDlg(TempString);
    end;

  if FSetCurrentDir then chdir(TempOldDir);
end;

function TCefApplication.Load_cef_app_capi_h : boolean;
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

function TCefApplication.Load_cef_browser_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_browser_host_create_browser{$IFDEF FPC}){$ENDIF}      := GetProcAddress(FLibHandle, 'cef_browser_host_create_browser');
  {$IFDEF FPC}Pointer({$ENDIF}cef_browser_host_create_browser_sync{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_browser_host_create_browser_sync');

  Result := assigned(cef_browser_host_create_browser) and
            assigned(cef_browser_host_create_browser_sync);
end;

function TCefApplication.Load_cef_command_line_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_command_line_create{$IFDEF FPC}){$ENDIF}     := GetProcAddress(FLibHandle, 'cef_command_line_create');
  {$IFDEF FPC}Pointer({$ENDIF}cef_command_line_get_global{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_command_line_get_global');

  Result := assigned(cef_command_line_create) and
            assigned(cef_command_line_get_global);
end;

function TCefApplication.Load_cef_cookie_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_cookie_manager_get_global_manager{$IFDEF FPC}){$ENDIF}   := GetProcAddress(FLibHandle, 'cef_cookie_manager_get_global_manager');
  {$IFDEF FPC}Pointer({$ENDIF}cef_cookie_manager_get_blocking_manager{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_cookie_manager_get_blocking_manager');
  {$IFDEF FPC}Pointer({$ENDIF}cef_cookie_manager_create_manager{$IFDEF FPC}){$ENDIF}       := GetProcAddress(FLibHandle, 'cef_cookie_manager_create_manager');

  Result := assigned(cef_cookie_manager_get_global_manager) and
            assigned(cef_cookie_manager_get_blocking_manager) and
            assigned(cef_cookie_manager_create_manager);
end;

function TCefApplication.Load_cef_crash_util_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_crash_reporting_enabled{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_crash_reporting_enabled');
  {$IFDEF FPC}Pointer({$ENDIF}cef_set_crash_key_value{$IFDEF FPC}){$ENDIF}     := GetProcAddress(FLibHandle, 'cef_set_crash_key_value');

  Result := assigned(cef_crash_reporting_enabled) and
            assigned(cef_set_crash_key_value);
end;

function TCefApplication.Load_cef_drag_data_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_drag_data_create{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_drag_data_create');

  Result := assigned(cef_drag_data_create);
end;

function TCefApplication.Load_cef_file_util_capi_h : boolean;
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

function TCefApplication.Load_cef_image_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_image_create{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_image_create');

  Result := assigned(cef_image_create);
end;

function TCefApplication.Load_cef_menu_model_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_menu_model_create{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_menu_model_create');

  Result := assigned(cef_menu_model_create);
end;

function TCefApplication.Load_cef_origin_whitelist_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_add_cross_origin_whitelist_entry{$IFDEF FPC}){$ENDIF}    := GetProcAddress(FLibHandle, 'cef_add_cross_origin_whitelist_entry');
  {$IFDEF FPC}Pointer({$ENDIF}cef_remove_cross_origin_whitelist_entry{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_remove_cross_origin_whitelist_entry');
  {$IFDEF FPC}Pointer({$ENDIF}cef_clear_cross_origin_whitelist{$IFDEF FPC}){$ENDIF}        := GetProcAddress(FLibHandle, 'cef_clear_cross_origin_whitelist');

  Result := assigned(cef_add_cross_origin_whitelist_entry) and
            assigned(cef_remove_cross_origin_whitelist_entry) and
            assigned(cef_clear_cross_origin_whitelist);
end;

function TCefApplication.Load_cef_parser_capi_h : boolean;
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
            assigned(cef_parse_jsonand_return_error) and
            assigned(cef_write_json);
end;

function TCefApplication.Load_cef_path_util_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_get_path{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_get_path');

  Result := assigned(cef_get_path);
end;

function TCefApplication.Load_cef_print_settings_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_print_settings_create{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_print_settings_create');

  Result := assigned(cef_print_settings_create);
end;

function TCefApplication.Load_cef_process_message_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_process_message_create{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_process_message_create');

  Result := assigned(cef_process_message_create);
end;

function TCefApplication.Load_cef_process_util_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_launch_process{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_launch_process');

  Result := assigned(cef_launch_process);
end;

function TCefApplication.Load_cef_request_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_request_create{$IFDEF FPC}){$ENDIF}           := GetProcAddress(FLibHandle, 'cef_request_create');
  {$IFDEF FPC}Pointer({$ENDIF}cef_post_data_create{$IFDEF FPC}){$ENDIF}         := GetProcAddress(FLibHandle, 'cef_post_data_create');
  {$IFDEF FPC}Pointer({$ENDIF}cef_post_data_element_create{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_post_data_element_create');

  Result := assigned(cef_request_create) and
            assigned(cef_post_data_create) and
            assigned(cef_post_data_element_create);
end;

function TCefApplication.Load_cef_request_context_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_request_context_get_global_context{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_request_context_get_global_context');
  {$IFDEF FPC}Pointer({$ENDIF}cef_request_context_create_context{$IFDEF FPC}){$ENDIF}     := GetProcAddress(FLibHandle, 'cef_request_context_create_context');
  {$IFDEF FPC}Pointer({$ENDIF}cef_create_context_shared{$IFDEF FPC}){$ENDIF}              := GetProcAddress(FLibHandle, 'cef_create_context_shared');

  Result := assigned(cef_request_context_get_global_context) and
            assigned(cef_request_context_create_context) and
            assigned(cef_create_context_shared);
end;

function TCefApplication.Load_cef_resource_bundle_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_resource_bundle_get_global{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_resource_bundle_get_global');

  Result := assigned(cef_resource_bundle_get_global);
end;

function TCefApplication.Load_cef_response_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_response_create{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_response_create');

  Result := assigned(cef_response_create);
end;

function TCefApplication.Load_cef_server_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_server_create{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_server_create');

  Result := assigned(cef_server_create);
end;

function TCefApplication.Load_cef_scheme_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_register_scheme_handler_factory{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_register_scheme_handler_factory');
  {$IFDEF FPC}Pointer({$ENDIF}cef_clear_scheme_handler_factories{$IFDEF FPC}){$ENDIF}  := GetProcAddress(FLibHandle, 'cef_clear_scheme_handler_factories');

  Result := assigned(cef_register_scheme_handler_factory) and
            assigned(cef_clear_scheme_handler_factories);
end;

function TCefApplication.Load_cef_ssl_info_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_is_cert_status_error{$IFDEF FPC}){$ENDIF}        := GetProcAddress(FLibHandle, 'cef_is_cert_status_error');
  {$IFDEF FPC}Pointer({$ENDIF}cef_is_cert_status_minor_error{$IFDEF FPC}){$ENDIF}  := GetProcAddress(FLibHandle, 'cef_is_cert_status_minor_error');

  Result := assigned(cef_is_cert_status_error) and
            assigned(cef_is_cert_status_minor_error);
end;

function TCefApplication.Load_cef_stream_capi_h : boolean;
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

function TCefApplication.Load_cef_task_capi_h : boolean;
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

function TCefApplication.Load_cef_thread_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_thread_create{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_thread_create');

  Result := assigned(cef_thread_create);
end;

function TCefApplication.Load_cef_trace_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_begin_tracing{$IFDEF FPC}){$ENDIF}              := GetProcAddress(FLibHandle, 'cef_begin_tracing');
  {$IFDEF FPC}Pointer({$ENDIF}cef_end_tracing{$IFDEF FPC}){$ENDIF}                := GetProcAddress(FLibHandle, 'cef_end_tracing');
  {$IFDEF FPC}Pointer({$ENDIF}cef_now_from_system_trace_time{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_now_from_system_trace_time');

  Result := assigned(cef_begin_tracing) and
            assigned(cef_end_tracing) and
            assigned(cef_now_from_system_trace_time);
end;

function TCefApplication.Load_cef_urlrequest_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_urlrequest_create{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_urlrequest_create');

  Result := assigned(cef_urlrequest_create);
end;

function TCefApplication.Load_cef_v8_capi_h : boolean;
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

function TCefApplication.Load_cef_values_capi_h : boolean;
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

function TCefApplication.Load_cef_waitable_event_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_waitable_event_create{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_waitable_event_create');

  Result := assigned(cef_waitable_event_create);
end;

function TCefApplication.Load_cef_web_plugin_capi_h : boolean;
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

function TCefApplication.Load_cef_xml_reader_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_xml_reader_create{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_xml_reader_create');

  Result := assigned(cef_xml_reader_create);
end;

function TCefApplication.Load_cef_zip_reader_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_zip_reader_create{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_zip_reader_create');

  Result := assigned(cef_zip_reader_create);
end;

function TCefApplication.Load_cef_logging_internal_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_get_min_log_level{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_get_min_log_level');
  {$IFDEF FPC}Pointer({$ENDIF}cef_get_vlog_level{$IFDEF FPC}){$ENDIF}    := GetProcAddress(FLibHandle, 'cef_get_vlog_level');
  {$IFDEF FPC}Pointer({$ENDIF}cef_log{$IFDEF FPC}){$ENDIF}               := GetProcAddress(FLibHandle, 'cef_log');

  Result := assigned(cef_get_min_log_level) and
            assigned(cef_get_vlog_level) and
            assigned(cef_log);
end;

function TCefApplication.Load_cef_string_list_h : boolean;
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

function TCefApplication.Load_cef_string_map_h : boolean;
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

function TCefApplication.Load_cef_string_multimap_h : boolean;
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

function TCefApplication.Load_cef_string_types_h : boolean;
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

function TCefApplication.Load_cef_thread_internal_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_get_current_platform_thread_id{$IFDEF FPC}){$ENDIF}     := GetProcAddress(FLibHandle, 'cef_get_current_platform_thread_id');
  {$IFDEF FPC}Pointer({$ENDIF}cef_get_current_platform_thread_handle{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_get_current_platform_thread_handle');

  Result := assigned(cef_get_current_platform_thread_id) and
            assigned(cef_get_current_platform_thread_handle);
end;

function TCefApplication.Load_cef_trace_event_internal_h : boolean;
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

function TCefApplication.Load_cef_browser_view_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_browser_view_create{$IFDEF FPC}){$ENDIF}          := GetProcAddress(FLibHandle, 'cef_browser_view_create');
  {$IFDEF FPC}Pointer({$ENDIF}cef_browser_view_get_for_browser{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_browser_view_get_for_browser');

  Result := assigned(cef_browser_view_create) and
            assigned(cef_browser_view_get_for_browser);
end;

function TCefApplication.Load_cef_display_capi_h : boolean;
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

function TCefApplication.Load_cef_label_button_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_label_button_create{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_label_button_create');

  Result := assigned(cef_label_button_create);
end;

function TCefApplication.Load_cef_menu_button_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_menu_button_create{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_menu_button_create');

  Result := assigned(cef_menu_button_create);
end;

function TCefApplication.Load_cef_panel_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_panel_create{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_panel_create');

  Result := assigned(cef_panel_create);
end;

function TCefApplication.Load_cef_scroll_view_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_scroll_view_create{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_scroll_view_create');

  Result := assigned(cef_scroll_view_create);
end;

function TCefApplication.Load_cef_textfield_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_textfield_create{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_textfield_create');

  Result := assigned(cef_textfield_create);
end;

function TCefApplication.Load_cef_window_capi_h : boolean;
begin
  {$IFDEF FPC}Pointer({$ENDIF}cef_window_create_top_level{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_window_create_top_level');

  Result := assigned(cef_window_create_top_level);
end;

function TCefApplication.Load_cef_types_linux_h : boolean;
begin
  {$IFDEF LINUX}
  {$IFDEF FPC}Pointer({$ENDIF}cef_get_xdisplay{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'cef_get_xdisplay');

  Result := assigned(cef_get_xdisplay);
  {$ELSE}
  Result := True;
  {$ENDIF}
end;


// TCEFCookieInitializerThread

constructor TCEFCookieInitializerThread.Create(const aCookies : string; aPersistSessionCookies : boolean);
begin
  inherited Create(True);

  FCookies               := aCookies;
  FPersistSessionCookies := aPersistSessionCookies;
  FreeOnTerminate        := True;
end;

procedure TCEFCookieInitializerThread.Execute;
var
  TempCookieManager : ICefCookieManager;
  TempCallBack      : ICefCompletionCallback;
  TempEvent         : ICefWaitableEvent;
begin
  try
    try
      if (length(FCookies) > 0) then
        begin
          TempEvent         := TCefWaitableEventRef.New(True, False);
          TempCallBack      := TCefEventCompletionCallback.Create(TempEvent);
          TempCookieManager := TCefCookieManagerRef.Global(TempCallBack);

          if TempEvent.TimedWait(5000) and (TempCookieManager <> nil) then
            TempCookieManager.SetStoragePath(FCookies, FPersistSessionCookies, nil);
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TCEFCookieInitializerThread.Execute', e) then raise;
    end;
  finally
    TempCookieManager := nil;
    TempCallBack      := nil;
    TempEvent         := nil;
  end;
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
