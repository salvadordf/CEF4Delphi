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

unit uCEFApplication;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  WinApi.Windows, System.Classes, System.UITypes,
  {$ELSE}
  Windows, Classes,
  {$ENDIF}
  uCEFTypes, uCEFInterfaces, uCEFBaseRefCounted, uCEFSchemeRegistrar;

const
  CEF_SUPPORTED_VERSION_MAJOR   = 3;
  CEF_SUPPORTED_VERSION_MINOR   = 3112;
  CEF_SUPPORTED_VERSION_RELEASE = 1656;
  CEF_SUPPORTED_VERSION_BUILD   = 0;

  CEF_CHROMEELF_VERSION_MAJOR   = 60;
  CEF_CHROMEELF_VERSION_MINOR   = 0;
  CEF_CHROMEELF_VERSION_RELEASE = 3112;
  CEF_CHROMEELF_VERSION_BUILD   = 90;

  LIBCEF_DLL                    = 'libcef.dll';
  CHROMEELF_DLL                 = 'chrome_elf.dll';

type
  TInternalApp = class;

  TCefApplication = class
    protected
      FMustShutDown                  : boolean;
      FCache                         : ustring;
      FCookies                       : ustring;
      FUserDataPath                  : ustring;
      FUserAgent                     : ustring;
      FProductVersion                : ustring;
      FLocale                        : ustring;
      FLogFile                       : ustring;
      FBrowserSubprocessPath         : ustring;
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
      FApp                           : TInternalApp;
      FAppIntf                       : ICefApp;
      FCustomCommandLines            : TStringList;
      FCustomCommandLineValues       : TStringList;
      FFlashEnabled                  : boolean;
      FEnableSpellingService         : boolean;
      FEnableMediaStream             : boolean;
      FEnableSpeechInput             : boolean;
      FEnableGPU                     : boolean;
      FCheckCEFFiles                 : boolean;
      FLibLoaded                     : boolean;
      FSmoothScrolling               : boolean;
      FFastUnload                    : boolean;
      FDisableSafeBrowsing           : boolean;
      FEnableHighDPISupport          : boolean;
      FMuteAudio                     : boolean;
      FReRaiseExceptions             : boolean;
      FUpdateChromeVer               : boolean;
      FChromeVersionInfo             : TFileVersionInfo;
      FLibHandle                     : THandle;
      FOnRegisterCustomSchemes       : TOnRegisterCustomSchemes;
      FResourceBundleHandler         : ICefResourceBundleHandler;
      FBrowserProcessHandler         : ICefBrowserProcessHandler;
      FRenderProcessHandler          : ICefRenderProcessHandler;
      FAppSettings                   : TCefSettings;
      FDeviceScaleFactor             : single;

      procedure SetFrameworkDirPath(const aValue : ustring);
      procedure SetResourcesDirPath(const aValue : ustring);
      procedure SetLocalesDirPath(const aValue : ustring);

      function  GetChromeVersion : string;
      function  GetLibCefPath : string;
      function  GetChromeElfPath : string;

      function  LoadCEFlibrary : boolean;
      function  Load_cef_app_capi_h : boolean;
      function  Load_cef_browser_capi_h : boolean;
      function  Load_cef_command_line_capi_h : boolean;
      function  Load_cef_cookie_capi_h : boolean;
      function  Load_cef_crash_util_h : boolean;
      function  Load_cef_drag_data_capi_h : boolean;
      function  Load_cef_file_util_capi_h : boolean;
      function  Load_cef_geolocation_capi_h : boolean;
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

      procedure ShutDown;
      function  ExecuteProcess : integer;
      procedure InitializeSettings(var aSettings : TCefSettings);
      function  InitializeLibrary : boolean;
      function  CreateInternalApp : boolean;
      function  MultiExeProcessing : boolean;
      function  SingleExeProcessing : boolean;
      function  CheckCEFLibrary : boolean;
      procedure DeleteDirContents(const aDirectory : string);

      procedure App_OnBeforeCommandLineProc(const processType: ustring; const commandLine: ICefCommandLine);
      procedure App_OnRegCustomSchemes(const registrar: TCefSchemeRegistrarRef);
      procedure App_OnGetResourceBundleHandler(var aCefResourceBundleHandler : ICefResourceBundleHandler);
      procedure App_OnGetBrowserProcessHandler(var aCefBrowserProcessHandler : ICefBrowserProcessHandler);
      procedure App_OnGetRenderProcessHandler(var aCefRenderProcessHandler : ICefRenderProcessHandler);

    public
      constructor Create(aUpdateChromeVer : boolean = True);
      destructor  Destroy; override;
      procedure   AfterConstruction; override;
      procedure   AddCustomCommandLine(const aCommandLine : string; const aValue : string = '');
      function    StartMainProcess : boolean;
      function    StartSubProcess : boolean;
      procedure   UpdateDeviceScaleFactor;

      property Cache                       : ustring                         read FCache                          write FCache;
      property Cookies                     : ustring                         read FCookies                        write FCookies;
      property UserDataPath                : ustring                         read FUserDataPath                   write FUserDataPath;
      property UserAgent                   : ustring                         read FUserAgent                      write FUserAgent;
      property ProductVersion              : ustring                         read FProductVersion                 write FProductVersion;
      property Locale                      : ustring                         read FLocale                         write FLocale;
      property LogFile                     : ustring                         read FLogFile                        write FLogFile;
      property BrowserSubprocessPath       : ustring                         read FBrowserSubprocessPath          write FBrowserSubprocessPath;
      property FrameworkDirPath            : ustring                         read FFrameworkDirPath               write SetFrameworkDirPath;
      property LogSeverity                 : TCefLogSeverity                 read FLogSeverity                    write FLogSeverity;
      property JavaScriptFlags             : ustring                         read FJavaScriptFlags                write FJavaScriptFlags;
      property ResourcesDirPath            : ustring                         read FResourcesDirPath               write SetResourcesDirPath;
      property LocalesDirPath              : ustring                         read FLocalesDirPath                 write SetLocalesDirPath;
      property SingleProcess               : Boolean                         read FSingleProcess                  write FSingleProcess;
      property NoSandbox                   : Boolean                         read FNoSandbox                      write FNoSandbox;
      property CommandLineArgsDisabled     : Boolean                         read FCommandLineArgsDisabled        write FCommandLineArgsDisabled;
      property PackLoadingDisabled         : Boolean                         read FPackLoadingDisabled            write FPackLoadingDisabled;
      property RemoteDebuggingPort         : Integer                         read FRemoteDebuggingPort            write FRemoteDebuggingPort;
      property UncaughtExceptionStackSize  : Integer                         read FUncaughtExceptionStackSize     write FUncaughtExceptionStackSize;
      property PersistSessionCookies       : Boolean                         read FPersistSessionCookies          write FPersistSessionCookies;
      property PersistUserPreferences      : Boolean                         read FPersistUserPreferences         write FPersistUserPreferences;
      property IgnoreCertificateErrors     : Boolean                         read FIgnoreCertificateErrors        write FIgnoreCertificateErrors;
      property EnableNetSecurityExpiration : boolean                         read FEnableNetSecurityExpiration    write FEnableNetSecurityExpiration;
      property BackgroundColor             : TCefColor                       read FBackgroundColor                write FBackgroundColor;
      property AcceptLanguageList          : ustring                         read FAcceptLanguageList             write FAcceptLanguageList;
      property WindowsSandboxInfo          : Pointer                         read FWindowsSandboxInfo             write FWindowsSandboxInfo;
      property WindowlessRenderingEnabled  : Boolean                         read FWindowlessRenderingEnabled     write FWindowlessRenderingEnabled;
      property MultiThreadedMessageLoop    : boolean                         read FMultiThreadedMessageLoop       write FMultiThreadedMessageLoop;
      property ExternalMessagePump         : boolean                         read FExternalMessagePump            write FExternalMessagePump;
      property DeleteCache                 : boolean                         read FDeleteCache                    write FDeleteCache;
      property DeleteCookies               : boolean                         read FDeleteCookies                  write FDeleteCookies;
      property FlashEnabled                : boolean                         read FFlashEnabled                   write FFlashEnabled;
      property EnableSpellingService       : boolean                         read FEnableSpellingService          write FEnableSpellingService;
      property EnableMediaStream           : boolean                         read FEnableMediaStream              write FEnableMediaStream;
      property EnableSpeechInput           : boolean                         read FEnableSpeechInput              write FEnableSpeechInput;
      property EnableGPU                   : boolean                         read FEnableGPU                      write FEnableGPU;
      property CheckCEFFiles               : boolean                         read FCheckCEFFiles                  write FCheckCEFFiles;
      property ChromeMajorVer              : uint16                          read FChromeVersionInfo.MajorVer;
      property ChromeMinorVer              : uint16                          read FChromeVersionInfo.MinorVer;
      property ChromeRelease               : uint16                          read FChromeVersionInfo.Release;
      property ChromeBuild                 : uint16                          read FChromeVersionInfo.Build;
      property ChromeVersion               : string                          read GetChromeVersion;
      property LibCefPath                  : string                          read GetLibCefPath;
      property ChromeElfPath               : string                          read GetChromeElfPath;
      property OnRegCustomSchemes          : TOnRegisterCustomSchemes        read FOnRegisterCustomSchemes        write FOnRegisterCustomSchemes;
      property ResourceBundleHandler       : ICefResourceBundleHandler       read FResourceBundleHandler          write FResourceBundleHandler;
      property BrowserProcessHandler       : ICefBrowserProcessHandler       read FBrowserProcessHandler          write FBrowserProcessHandler;
      property RenderProcessHandler        : ICefRenderProcessHandler        read FRenderProcessHandler           write FRenderProcessHandler;
      property SmoothScrolling             : boolean                         read FSmoothScrolling                write FSmoothScrolling;
      property FastUnload                  : boolean                         read FFastUnload                     write FFastUnload;
      property DisableSafeBrowsing         : boolean                         read FDisableSafeBrowsing            write FDisableSafeBrowsing;
      property LibLoaded                   : boolean                         read FLibLoaded;
      property EnableHighDPISupport        : boolean                         read FEnableHighDPISupport           write FEnableHighDPISupport;
      property MuteAudio                   : boolean                         read FMuteAudio                      write FMuteAudio;
      property ReRaiseExceptions           : boolean                         read FReRaiseExceptions              write FReRaiseExceptions;
      property DeviceScaleFactor           : single                          read FDeviceScaleFactor;
  end;

  TCefAppOwn = class(TCefBaseRefCountedOwn, ICefApp)
    protected
      procedure OnBeforeCommandLineProcessing(const processType: ustring; const commandLine: ICefCommandLine); virtual; abstract;
      procedure OnRegisterCustomSchemes(const registrar: TCefSchemeRegistrarRef); virtual; abstract;
      function  GetResourceBundleHandler: ICefResourceBundleHandler; virtual; abstract;
      function  GetBrowserProcessHandler: ICefBrowserProcessHandler; virtual; abstract;
      function  GetRenderProcessHandler: ICefRenderProcessHandler; virtual; abstract;

    public
      constructor Create; virtual;
  end;

  TInternalApp = class(TCefAppOwn)
    protected
      FOnBeforeCommandLineProcessing : TOnBeforeCommandLineProcessing;
      FOnRegisterCustomSchemes       : TOnRegisterCustomSchemes;
      FOnGetResourceBundleHandler    : TOnGetResourceBundleHandler;
      FOnGetBrowserProcessHandler    : TOnGetBrowserProcessHandler;
      FOnGetRenderProcessHandler     : TOnGetRenderProcessHandler;

      procedure OnBeforeCommandLineProcessing(const processType: ustring; const commandLine: ICefCommandLine); override;
      procedure OnRegisterCustomSchemes(const registrar: TCefSchemeRegistrarRef); override;
      function  GetResourceBundleHandler: ICefResourceBundleHandler; override;
      function  GetBrowserProcessHandler: ICefBrowserProcessHandler; override;
      function  GetRenderProcessHandler: ICefRenderProcessHandler; override;

    public
      constructor Create; override;

      property OnBeforeCommandLineProc       : TOnBeforeCommandLineProcessing  read FOnBeforeCommandLineProcessing  write FOnBeforeCommandLineProcessing;
      property OnRegCustomSchemes            : TOnRegisterCustomSchemes        read FOnRegisterCustomSchemes        write FOnRegisterCustomSchemes;
      property OnGetResourceBundleHandler    : TOnGetResourceBundleHandler     read FOnGetResourceBundleHandler     write FOnGetResourceBundleHandler;
      property OnGetBrowserProcessHandler    : TOnGetBrowserProcessHandler     read FOnGetBrowserProcessHandler     write FOnGetBrowserProcessHandler;
      property OnGetRenderProcessHandler     : TOnGetRenderProcessHandler      read FOnGetRenderProcessHandler      write FOnGetRenderProcessHandler;
  end;

var
  GlobalCEFApp : TCefApplication = nil;

implementation

uses
  {$IFDEF DELPHI16_UP}
  System.Math, System.IOUtils, System.SysUtils, Vcl.Dialogs,
  {$ELSE}
  Math, {$IFDEF DELPHI12_UP}IOUtils,{$ENDIF} SysUtils, Dialogs,
  {$ENDIF}
  uCEFLibFunctions, uCEFMiscFunctions, uCEFCommandLine, uCEFConstants,
  uCEFSchemeHandlerFactory;

constructor TCefApplication.Create(aUpdateChromeVer : boolean);
begin
  inherited Create;

  FLibHandle                     := 0;
  FMustShutDown                  := False;
  FCache                         := '';
  FCookies                       := '';
  FUserDataPath                  := '';
  FUserAgent                     := '';
  FProductVersion                := '';
  FLocale                        := '';
  FLogFile                       := '';
  FBrowserSubprocessPath         := '';
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
  FApp                           := nil;
  FAppIntf                       := nil;
  FFlashEnabled                  := True;
  FEnableSpellingService         := True;
  FEnableMediaStream             := True;
  FEnableSpeechInput             := True;
  FEnableGPU                     := True;
  FCustomCommandLines            := nil;
  FCustomCommandLineValues       := nil;
  FCheckCEFFiles                 := True;
  FSmoothScrolling               := False;
  FFastUnload                    := False;
  FDisableSafeBrowsing           := False;
  FOnRegisterCustomSchemes       := nil;
  FResourceBundleHandler         := nil;
  FBrowserProcessHandler         := nil;
  FRenderProcessHandler          := nil;
  FEnableHighDPISupport          := False;
  FMuteAudio                     := False;
  FReRaiseExceptions             := False;
  FLibLoaded                     := False;
  FUpdateChromeVer               := aUpdateChromeVer;

  UpdateDeviceScaleFactor;

  FAppSettings.size := SizeOf(TCefSettings);
  FillChar(FAppSettings, FAppSettings.size, 0);

  FChromeVersionInfo.MajorVer    := CEF_CHROMEELF_VERSION_MAJOR;
  FChromeVersionInfo.MinorVer    := CEF_CHROMEELF_VERSION_MINOR;
  FChromeVersionInfo.Release     := CEF_CHROMEELF_VERSION_RELEASE;
  FChromeVersionInfo.Build       := CEF_CHROMEELF_VERSION_BUILD;

  if FUpdateChromeVer then GetDLLVersion(ChromeElfPath, FChromeVersionInfo);

  IsMultiThread := True;

  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
end;

destructor TCefApplication.Destroy;
begin
  ShutDown;

  FAppIntf := nil;
  FApp     := nil;

  if (FCustomCommandLines      <> nil) then FreeAndNil(FCustomCommandLines);
  if (FCustomCommandLineValues <> nil) then FreeAndNil(FCustomCommandLineValues);

  inherited Destroy;
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

function TCefApplication.CreateInternalApp : boolean;
begin
  Result := False;

  try
    if (FApp = nil) then
      begin
        FApp                            := TInternalApp.Create;
        FApp.OnBeforeCommandLineProc    := App_OnBeforeCommandLineProc;
        FApp.OnRegCustomSchemes         := App_OnRegCustomSchemes;
        FApp.OnGetResourceBundleHandler := App_OnGetResourceBundleHandler;
        FApp.OnGetBrowserProcessHandler := App_OnGetBrowserProcessHandler;
        FApp.OnGetRenderProcessHandler  := App_OnGetRenderProcessHandler;

        FAppIntf := FApp as ICefApp;
        Result   := (FAppIntf <> nil);
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TCefApplication.CreateInternalApp', e) then raise;
  end;
end;

function TCefApplication.MultiExeProcessing : boolean;
begin
  Result := False;

  try
    if CheckCEFLibrary then
      begin
        FMustShutDown := True;
        Result        := LoadCEFlibrary    and
                         CreateInternalApp and
                         InitializeLibrary;
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TCefApplication.MultiExeProcessing', e) then raise;
  end;
end;

function TCefApplication.SingleExeProcessing : boolean;
begin
  Result := False;

  try
    if CheckCEFLibrary      and
       LoadCEFlibrary       and
       CreateInternalApp    and
       (ExecuteProcess < 0) then
      begin
        FMustShutDown := True;
        Result        := InitializeLibrary;
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TCefApplication.SingleExeProcessing', e) then raise;
  end;
end;

function TCefApplication.GetChromeVersion : string;
begin
  Result := inttostr(FChromeVersionInfo.MajorVer) + '.' +
            inttostr(FChromeVersionInfo.MinorVer) + '.' +
            inttostr(FChromeVersionInfo.Release)  + '.' +
            inttostr(FChromeVersionInfo.Build);
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

procedure TCefApplication.SetFrameworkDirPath(const aValue : ustring);
begin
  if (length(aValue) > 0) and DirectoryExists(aValue) then
    begin
      if CustomPathIsRelative(aValue) then
        FFrameworkDirPath := ExtractFilePath(ParamStr(0)) + aValue
       else
        FFrameworkDirPath := aValue;
    end
   else
    FFrameworkDirPath := '';

  if FUpdateChromeVer then GetDLLVersion(ChromeElfPath, FChromeVersionInfo);
end;

procedure TCefApplication.SetResourcesDirPath(const aValue : ustring);
begin
  if (length(aValue) > 0) and DirectoryExists(aValue) then
    begin
      if CustomPathIsRelative(aValue) then
        FResourcesDirPath := ExtractFilePath(ParamStr(0)) + aValue
       else
        FResourcesDirPath := aValue;
    end
   else
    FResourcesDirPath := '';
end;

procedure TCefApplication.SetLocalesDirPath(const aValue : ustring);
begin
  if (length(aValue) > 0) and DirectoryExists(aValue) then
    begin
      if CustomPathIsRelative(aValue) then
        FLocalesDirPath := ExtractFilePath(ParamStr(0)) + aValue
       else
        FLocalesDirPath := aValue;
    end
   else
    FLocalesDirPath := '';
end;

function TCefApplication.CheckCEFLibrary : boolean;
begin
  Result := False;

  if not(FCheckCEFFiles) then
    Result := True
   else
    if CheckDLLs(FFrameworkDirPath) and
       CheckResources(FResourcesDirPath) and
       CheckLocales(FLocalesDirPath) then
      begin
        if CheckDLLVersion(LibCefPath,
                           CEF_SUPPORTED_VERSION_MAJOR,
                           CEF_SUPPORTED_VERSION_MINOR,
                           CEF_SUPPORTED_VERSION_RELEASE,
                           CEF_SUPPORTED_VERSION_BUILD) then
          Result := True
         else
          MessageDlg('Unsupported CEF version !', mtError, [mbOk], 0);
      end
     else
      MessageDlg('CEF binaries missing !', mtError, [mbOk], 0);
end;

function TCefApplication.StartMainProcess : boolean;
begin
  if not(FSingleProcess) and (length(FBrowserSubprocessPath) > 0) then
    Result := MultiExeProcessing
   else
    Result := SingleExeProcessing;
end;

function TCefApplication.StartSubProcess : boolean;
begin
  Result := False;

  try
    Result := not(FSingleProcess) and
              LoadCEFlibrary      and
              CreateInternalApp   and
              (ExecuteProcess >= 0);
  except
    on e : exception do
      if CustomExceptionHandler('TCefApplication.StartSubProcess', e) then raise;
  end;
end;

procedure TCefApplication.UpdateDeviceScaleFactor;
begin
  FDeviceScaleFactor := GetDeviceScaleFactor;
end;

procedure TCefApplication.ShutDown;
begin
  try
    if (FLibHandle <> 0) then
      begin
        if FMustShutDown then cef_shutdown;

        FreeLibrary(FLibHandle);
        FLibHandle := 0;
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TCefApplication.ShutDown', e) then raise;
  end;
end;

function TCefApplication.ExecuteProcess : integer;
var
  TempArgs : TCefMainArgs;
begin
  TempArgs.instance := HINSTANCE;
  Result            := cef_execute_process(@TempArgs, FApp.Wrap, FWindowsSandboxInfo);
end;

procedure TCefApplication.InitializeSettings(var aSettings : TCefSettings);
begin
  aSettings.size                            := SizeOf(TCefSettings);
  aSettings.single_process                  := Ord(FSingleProcess);
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

function TCefApplication.InitializeLibrary : boolean;
begin
  Result := False;

  try
    if FDeleteCache   then DeleteDirContents(FCache);
    if FDeleteCookies then DeleteDirContents(FCookies);

    InitializeSettings(FAppSettings);
    Result := (cef_initialize(@HInstance, @FAppSettings, FApp.Wrap, FWindowsSandboxInfo) <> 0);
  except
    on e : exception do
      if CustomExceptionHandler('TCefApplication.InitializeLibrary', e) then raise;
  end;
end;

procedure TCefApplication.DeleteDirContents(const aDirectory : string);
{$IFNDEF DELPHI12_UP}
var
  TempRec : TSearchRec;
{$ENDIF}
begin
  try
    if (length(aDirectory) > 0) and DirectoryExists(aDirectory) then
      begin
        {$IFDEF DELPHI12_UP}
        TDirectory.Delete(aDirectory, True);
        {$ELSE}
        if (FindFirst(aDirectory + '\*', faAnyFile, TempRec) = 0) then
          begin
            try
              repeat
                if ((TempRec.Attr and faDirectory) <> 0) then
                  begin
                    if (TempRec.Name <> '.') and (TempRec.Name <> '..') then
                      DeleteDirContents(aDirectory + '\' + TempRec.Name)
                  end
                else
                 DeleteFile(aDirectory + '\' + TempRec.Name);
              until (FindNext(TempRec) <> 0);
            finally
              FindClose(TempRec);
            end;
          end;
        {$ENDIF}
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TCefApplication.DeleteDirContents', e) then raise;
  end;
end;

procedure TCefApplication.App_OnBeforeCommandLineProc(const processType : ustring;
                                                      const commandLine : ICefCommandLine);
var
  i : integer;
begin
  if (commandLine <> nil) then
    begin
      if FFlashEnabled then
        begin
          if FEnableGPU then commandLine.AppendSwitch('--enable-gpu-plugin');

          commandLine.AppendSwitch('--enable-accelerated-plugins');
          commandLine.AppendSwitch('--enable-system-flash');
        end;

      commandLine.AppendSwitchWithValue('--enable-spelling-service', IntToStr(Ord(FEnableSpellingService)));
      commandLine.AppendSwitchWithValue('--enable-media-stream',     IntToStr(Ord(FEnableMediaStream)));
      commandLine.AppendSwitchWithValue('--enable-speech-input',     IntToStr(Ord(FEnableSpeechInput)));

      if not(FEnableGPU) then
        begin
          commandLine.AppendSwitch('--disable-gpu');
          commandLine.AppendSwitch('--disable-gpu-compositing');
        end;

      if FSmoothScrolling then
        commandLine.AppendSwitch('--enable-smooth-scrolling');

      if FFastUnload then
        commandLine.AppendSwitch('--enable-fast-unload');

      if FDisableSafeBrowsing then
        begin
          commandLine.AppendSwitch('--disable-client-side-phishing-detection');
          commandLine.AppendSwitch('--safebrowsing-disable-auto-update');
          commandLine.AppendSwitch('--safebrowsing-disable-download-protection');
        end;

      if FMuteAudio then
        commandLine.AppendSwitch('--mute-audio');

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

procedure TCefApplication.App_OnRegCustomSchemes(const registrar: TCefSchemeRegistrarRef);
begin
  if assigned(FOnRegisterCustomSchemes) then FOnRegisterCustomSchemes(registrar);
end;

procedure TCefApplication.App_OnGetResourceBundleHandler(var aCefResourceBundleHandler : ICefResourceBundleHandler);
begin
  aCefResourceBundleHandler := FResourceBundleHandler;
end;

procedure TCefApplication.App_OnGetBrowserProcessHandler(var aCefBrowserProcessHandler : ICefBrowserProcessHandler);
begin
  aCefBrowserProcessHandler := FBrowserProcessHandler;
end;

procedure TCefApplication.App_OnGetRenderProcessHandler(var aCefRenderProcessHandler : ICefRenderProcessHandler);
begin
  aCefRenderProcessHandler := FRenderProcessHandler;
end;

function TCefApplication.LoadCEFlibrary : boolean;
begin
  FLibHandle := LoadLibraryEx(PChar(LibCefPath), 0, LOAD_WITH_ALTERED_SEARCH_PATH);

  if (FLibHandle = 0) then
    begin
      Result := False;
      OutputDebugMessage('TCefApplication.LoadCEFlibrary error: Cannot load libcef.dll. Error code : 0x' + inttohex(GetLastError, 8));
      exit;
    end;


  if Load_cef_app_capi_h and
     Load_cef_browser_capi_h and
     Load_cef_command_line_capi_h and
     Load_cef_cookie_capi_h and
     Load_cef_crash_util_h and
     Load_cef_drag_data_capi_h and
     Load_cef_file_util_capi_h and
     Load_cef_geolocation_capi_h and
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
     Load_cef_trace_event_internal_h then
    begin
      FLibLoaded := True;
      Result     := True;

      if FEnableHighDPISupport then cef_enable_highdpi_support;
    end
   else
    begin
      Result := False;
      OutputDebugMessage('TCefApplication.LoadCEFlibrary error: Unsupported CEF version !');
    end;
end;

function TCefApplication.Load_cef_app_capi_h : boolean;
begin
  cef_initialize             := GetProcAddress(FLibHandle, 'cef_initialize');
  cef_shutdown               := GetProcAddress(FLibHandle, 'cef_shutdown');
  cef_execute_process        := GetProcAddress(FLibHandle, 'cef_execute_process');
  cef_do_message_loop_work   := GetProcAddress(FLibHandle, 'cef_do_message_loop_work');
  cef_run_message_loop       := GetProcAddress(FLibHandle, 'cef_run_message_loop');
  cef_quit_message_loop      := GetProcAddress(FLibHandle, 'cef_quit_message_loop');
  cef_set_osmodal_loop       := GetProcAddress(FLibHandle, 'cef_set_osmodal_loop');
  cef_enable_highdpi_support := GetProcAddress(FLibHandle, 'cef_enable_highdpi_support');

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
  cef_browser_host_create_browser      := GetProcAddress(FLibHandle, 'cef_browser_host_create_browser');
  cef_browser_host_create_browser_sync := GetProcAddress(FLibHandle, 'cef_browser_host_create_browser_sync');

  Result := assigned(cef_browser_host_create_browser) and
            assigned(cef_browser_host_create_browser_sync);
end;

function TCefApplication.Load_cef_command_line_capi_h : boolean;
begin
  cef_command_line_create     := GetProcAddress(FLibHandle, 'cef_command_line_create');
  cef_command_line_get_global := GetProcAddress(FLibHandle, 'cef_command_line_get_global');

  Result := assigned(cef_command_line_create) and
            assigned(cef_command_line_get_global);
end;

function TCefApplication.Load_cef_cookie_capi_h : boolean;
begin
  cef_cookie_manager_get_global_manager := GetProcAddress(FLibHandle, 'cef_cookie_manager_get_global_manager');
  cef_cookie_manager_create_manager     := GetProcAddress(FLibHandle, 'cef_cookie_manager_create_manager');

  Result := assigned(cef_cookie_manager_get_global_manager) and
            assigned(cef_cookie_manager_create_manager);
end;

function TCefApplication.Load_cef_crash_util_h : boolean;
begin
  cef_crash_reporting_enabled := GetProcAddress(FLibHandle, 'cef_crash_reporting_enabled');
  cef_set_crash_key_value     := GetProcAddress(FLibHandle, 'cef_set_crash_key_value');

  Result := assigned(cef_crash_reporting_enabled) and
            assigned(cef_set_crash_key_value);
end;

function TCefApplication.Load_cef_drag_data_capi_h : boolean;
begin
  cef_drag_data_create := GetProcAddress(FLibHandle, 'cef_drag_data_create');

  Result := assigned(cef_drag_data_create);
end;

function TCefApplication.Load_cef_file_util_capi_h : boolean;
begin
  cef_create_directory                   := GetProcAddress(FLibHandle, 'cef_create_directory');
  cef_get_temp_directory                 := GetProcAddress(FLibHandle, 'cef_get_temp_directory');
  cef_create_new_temp_directory          := GetProcAddress(FLibHandle, 'cef_create_new_temp_directory');
  cef_create_temp_directory_in_directory := GetProcAddress(FLibHandle, 'cef_create_temp_directory_in_directory');
  cef_directory_exists                   := GetProcAddress(FLibHandle, 'cef_directory_exists');
  cef_delete_file                        := GetProcAddress(FLibHandle, 'cef_delete_file');
  cef_zip_directory                      := GetProcAddress(FLibHandle, 'cef_zip_directory');
  cef_load_crlsets_file                  := GetProcAddress(FLibHandle, 'cef_load_crlsets_file');

  Result := assigned(cef_create_directory) and
            assigned(cef_get_temp_directory) and
            assigned(cef_create_new_temp_directory) and
            assigned(cef_create_temp_directory_in_directory) and
            assigned(cef_directory_exists) and
            assigned(cef_delete_file) and
            assigned(cef_zip_directory) and
            assigned(cef_load_crlsets_file);
end;

function TCefApplication.Load_cef_geolocation_capi_h : boolean;
begin
  cef_get_geolocation := GetProcAddress(FLibHandle, 'cef_get_geolocation');

  Result := assigned(cef_get_geolocation);
end;

function TCefApplication.Load_cef_image_capi_h : boolean;
begin
  cef_image_create := GetProcAddress(FLibHandle, 'cef_image_create');

  Result := assigned(cef_image_create);
end;

function TCefApplication.Load_cef_menu_model_capi_h : boolean;
begin
  cef_menu_model_create := GetProcAddress(FLibHandle, 'cef_menu_model_create');

  Result := assigned(cef_menu_model_create);
end;

function TCefApplication.Load_cef_origin_whitelist_capi_h : boolean;
begin
  cef_add_cross_origin_whitelist_entry    := GetProcAddress(FLibHandle, 'cef_add_cross_origin_whitelist_entry');
  cef_remove_cross_origin_whitelist_entry := GetProcAddress(FLibHandle, 'cef_remove_cross_origin_whitelist_entry');
  cef_clear_cross_origin_whitelist        := GetProcAddress(FLibHandle, 'cef_clear_cross_origin_whitelist');

  Result := assigned(cef_add_cross_origin_whitelist_entry) and
            assigned(cef_remove_cross_origin_whitelist_entry) and
            assigned(cef_clear_cross_origin_whitelist);
end;

function TCefApplication.Load_cef_parser_capi_h : boolean;
begin
  cef_parse_url                       := GetProcAddress(FLibHandle, 'cef_parse_url');
  cef_create_url                      := GetProcAddress(FLibHandle, 'cef_create_url');
  cef_format_url_for_security_display := GetProcAddress(FLibHandle, 'cef_format_url_for_security_display');
  cef_get_mime_type                   := GetProcAddress(FLibHandle, 'cef_get_mime_type');
  cef_get_extensions_for_mime_type    := GetProcAddress(FLibHandle, 'cef_get_extensions_for_mime_type');
  cef_base64encode                    := GetProcAddress(FLibHandle, 'cef_base64encode');
  cef_base64decode                    := GetProcAddress(FLibHandle, 'cef_base64decode');
  cef_uriencode                       := GetProcAddress(FLibHandle, 'cef_uriencode');
  cef_uridecode                       := GetProcAddress(FLibHandle, 'cef_uridecode');
  cef_parse_json                      := GetProcAddress(FLibHandle, 'cef_parse_json');
  cef_parse_jsonand_return_error      := GetProcAddress(FLibHandle, 'cef_parse_jsonand_return_error');
  cef_write_json                      := GetProcAddress(FLibHandle, 'cef_write_json');

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
  cef_get_path := GetProcAddress(FLibHandle, 'cef_get_path');

  Result := assigned(cef_get_path);
end;

function TCefApplication.Load_cef_print_settings_capi_h : boolean;
begin
  cef_print_settings_create := GetProcAddress(FLibHandle, 'cef_print_settings_create');

  Result := assigned(cef_print_settings_create);
end;

function TCefApplication.Load_cef_process_message_capi_h : boolean;
begin
  cef_process_message_create := GetProcAddress(FLibHandle, 'cef_process_message_create');

  Result := assigned(cef_process_message_create);
end;

function TCefApplication.Load_cef_process_util_capi_h : boolean;
begin
  cef_launch_process := GetProcAddress(FLibHandle, 'cef_launch_process');

  Result := assigned(cef_launch_process);
end;

function TCefApplication.Load_cef_request_capi_h : boolean;
begin
  cef_request_create           := GetProcAddress(FLibHandle, 'cef_request_create');
  cef_post_data_create         := GetProcAddress(FLibHandle, 'cef_post_data_create');
  cef_post_data_element_create := GetProcAddress(FLibHandle, 'cef_post_data_element_create');

  Result := assigned(cef_request_create) and
            assigned(cef_post_data_create) and
            assigned(cef_post_data_element_create);
end;

function TCefApplication.Load_cef_request_context_capi_h : boolean;
begin
  cef_request_context_get_global_context := GetProcAddress(FLibHandle, 'cef_request_context_get_global_context');
  cef_request_context_create_context     := GetProcAddress(FLibHandle, 'cef_request_context_create_context');
  cef_create_context_shared              := GetProcAddress(FLibHandle, 'cef_create_context_shared');

  Result := assigned(cef_request_context_get_global_context) and
            assigned(cef_request_context_create_context) and
            assigned(cef_create_context_shared);
end;

function TCefApplication.Load_cef_resource_bundle_capi_h : boolean;
begin
  cef_resource_bundle_get_global := GetProcAddress(FLibHandle, 'cef_resource_bundle_get_global');

  Result := assigned(cef_resource_bundle_get_global);
end;

function TCefApplication.Load_cef_response_capi_h : boolean;
begin
  cef_response_create := GetProcAddress(FLibHandle, 'cef_response_create');

  Result := assigned(cef_response_create);
end;

function TCefApplication.Load_cef_scheme_capi_h : boolean;
begin
  cef_register_scheme_handler_factory := GetProcAddress(FLibHandle, 'cef_register_scheme_handler_factory');
  cef_clear_scheme_handler_factories  := GetProcAddress(FLibHandle, 'cef_clear_scheme_handler_factories');

  Result := assigned(cef_register_scheme_handler_factory) and
            assigned(cef_clear_scheme_handler_factories);
end;

function TCefApplication.Load_cef_ssl_info_capi_h : boolean;
begin
  cef_is_cert_status_error        := GetProcAddress(FLibHandle, 'cef_is_cert_status_error');
  cef_is_cert_status_minor_error  := GetProcAddress(FLibHandle, 'cef_is_cert_status_minor_error');

  Result := assigned(cef_is_cert_status_error) and
            assigned(cef_is_cert_status_minor_error);
end;

function TCefApplication.Load_cef_stream_capi_h : boolean;
begin
  cef_stream_reader_create_for_file    := GetProcAddress(FLibHandle, 'cef_stream_reader_create_for_file');
  cef_stream_reader_create_for_data    := GetProcAddress(FLibHandle, 'cef_stream_reader_create_for_data');
  cef_stream_reader_create_for_handler := GetProcAddress(FLibHandle, 'cef_stream_reader_create_for_handler');
  cef_stream_writer_create_for_file    := GetProcAddress(FLibHandle, 'cef_stream_writer_create_for_file');
  cef_stream_writer_create_for_handler := GetProcAddress(FLibHandle, 'cef_stream_writer_create_for_handler');

  Result := assigned(cef_stream_reader_create_for_file) and
            assigned(cef_stream_reader_create_for_data) and
            assigned(cef_stream_reader_create_for_handler) and
            assigned(cef_stream_writer_create_for_file) and
            assigned(cef_stream_writer_create_for_handler);
end;

function TCefApplication.Load_cef_task_capi_h : boolean;
begin
  cef_task_runner_get_for_current_thread := GetProcAddress(FLibHandle, 'cef_task_runner_get_for_current_thread');
  cef_task_runner_get_for_thread         := GetProcAddress(FLibHandle, 'cef_task_runner_get_for_thread');
  cef_currently_on                       := GetProcAddress(FLibHandle, 'cef_currently_on');
  cef_post_task                          := GetProcAddress(FLibHandle, 'cef_post_task');
  cef_post_delayed_task                  := GetProcAddress(FLibHandle, 'cef_post_delayed_task');

  Result := assigned(cef_task_runner_get_for_current_thread) and
            assigned(cef_task_runner_get_for_thread) and
            assigned(cef_currently_on) and
            assigned(cef_post_task) and
            assigned(cef_post_delayed_task);
end;

function TCefApplication.Load_cef_thread_capi_h : boolean;
begin
  cef_thread_create := GetProcAddress(FLibHandle, 'cef_thread_create');

  Result := assigned(cef_thread_create);
end;

function TCefApplication.Load_cef_trace_capi_h : boolean;
begin
  cef_begin_tracing              := GetProcAddress(FLibHandle, 'cef_begin_tracing');
  cef_end_tracing                := GetProcAddress(FLibHandle, 'cef_end_tracing');
  cef_now_from_system_trace_time := GetProcAddress(FLibHandle, 'cef_now_from_system_trace_time');

  Result := assigned(cef_begin_tracing) and
            assigned(cef_end_tracing) and
            assigned(cef_now_from_system_trace_time);
end;

function TCefApplication.Load_cef_urlrequest_capi_h : boolean;
begin
  cef_urlrequest_create := GetProcAddress(FLibHandle, 'cef_urlrequest_create');

  Result := assigned(cef_urlrequest_create);
end;

function TCefApplication.Load_cef_v8_capi_h : boolean;
begin
  cef_v8context_get_current_context := GetProcAddress(FLibHandle, 'cef_v8context_get_current_context');
  cef_v8context_get_entered_context := GetProcAddress(FLibHandle, 'cef_v8context_get_entered_context');
  cef_v8context_in_context          := GetProcAddress(FLibHandle, 'cef_v8context_in_context');
  cef_v8value_create_undefined      := GetProcAddress(FLibHandle, 'cef_v8value_create_undefined');
  cef_v8value_create_null           := GetProcAddress(FLibHandle, 'cef_v8value_create_null');
  cef_v8value_create_bool           := GetProcAddress(FLibHandle, 'cef_v8value_create_bool');
  cef_v8value_create_int            := GetProcAddress(FLibHandle, 'cef_v8value_create_int');
  cef_v8value_create_uint           := GetProcAddress(FLibHandle, 'cef_v8value_create_uint');
  cef_v8value_create_double         := GetProcAddress(FLibHandle, 'cef_v8value_create_double');
  cef_v8value_create_date           := GetProcAddress(FLibHandle, 'cef_v8value_create_date');
  cef_v8value_create_string         := GetProcAddress(FLibHandle, 'cef_v8value_create_string');
  cef_v8value_create_object         := GetProcAddress(FLibHandle, 'cef_v8value_create_object');
  cef_v8value_create_array          := GetProcAddress(FLibHandle, 'cef_v8value_create_array');
  cef_v8value_create_function       := GetProcAddress(FLibHandle, 'cef_v8value_create_function');
  cef_v8stack_trace_get_current     := GetProcAddress(FLibHandle, 'cef_v8stack_trace_get_current');
  cef_register_extension            := GetProcAddress(FLibHandle, 'cef_register_extension');

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
            assigned(cef_v8value_create_function) and
            assigned(cef_v8stack_trace_get_current) and
            assigned(cef_register_extension);
end;

function TCefApplication.Load_cef_values_capi_h : boolean;
begin
  cef_value_create            := GetProcAddress(FLibHandle, 'cef_value_create');
  cef_binary_value_create     := GetProcAddress(FLibHandle, 'cef_binary_value_create');
  cef_dictionary_value_create := GetProcAddress(FLibHandle, 'cef_dictionary_value_create');
  cef_list_value_create       := GetProcAddress(FLibHandle, 'cef_list_value_create');

  Result := assigned(cef_value_create) and
            assigned(cef_binary_value_create) and
            assigned(cef_v8stack_trace_get_current) and
            assigned(cef_list_value_create);
end;

function TCefApplication.Load_cef_waitable_event_capi_h : boolean;
begin
  cef_waitable_event_create := GetProcAddress(FLibHandle, 'cef_waitable_event_create');

  Result := assigned(cef_waitable_event_create);
end;

function TCefApplication.Load_cef_web_plugin_capi_h : boolean;
begin
  cef_visit_web_plugin_info          := GetProcAddress(FLibHandle, 'cef_visit_web_plugin_info');
  cef_refresh_web_plugins            := GetProcAddress(FLibHandle, 'cef_refresh_web_plugins');
  cef_unregister_internal_web_plugin := GetProcAddress(FLibHandle, 'cef_unregister_internal_web_plugin');
  cef_register_web_plugin_crash      := GetProcAddress(FLibHandle, 'cef_register_web_plugin_crash');
  cef_is_web_plugin_unstable         := GetProcAddress(FLibHandle, 'cef_is_web_plugin_unstable');
  cef_register_widevine_cdm          := GetProcAddress(FLibHandle, 'cef_register_widevine_cdm');

  Result := assigned(cef_visit_web_plugin_info) and
            assigned(cef_refresh_web_plugins) and
            assigned(cef_unregister_internal_web_plugin) and
            assigned(cef_register_web_plugin_crash) and
            assigned(cef_is_web_plugin_unstable) and
            assigned(cef_register_widevine_cdm);
end;

function TCefApplication.Load_cef_xml_reader_capi_h : boolean;
begin
  cef_xml_reader_create := GetProcAddress(FLibHandle, 'cef_xml_reader_create');

  Result := assigned(cef_xml_reader_create);
end;

function TCefApplication.Load_cef_zip_reader_capi_h : boolean;
begin
  cef_zip_reader_create := GetProcAddress(FLibHandle, 'cef_zip_reader_create');

  Result := assigned(cef_zip_reader_create);
end;

function TCefApplication.Load_cef_logging_internal_h : boolean;
begin
  cef_get_min_log_level := GetProcAddress(FLibHandle, 'cef_get_min_log_level');
  cef_get_vlog_level    := GetProcAddress(FLibHandle, 'cef_get_vlog_level');
  cef_log               := GetProcAddress(FLibHandle, 'cef_log');

  Result := assigned(cef_get_min_log_level) and
            assigned(cef_get_vlog_level) and
            assigned(cef_log);
end;

function TCefApplication.Load_cef_string_list_h : boolean;
begin
  cef_string_list_alloc  := GetProcAddress(FLibHandle, 'cef_string_list_alloc');
  cef_string_list_size   := GetProcAddress(FLibHandle, 'cef_string_list_size');
  cef_string_list_value  := GetProcAddress(FLibHandle, 'cef_string_list_value');
  cef_string_list_append := GetProcAddress(FLibHandle, 'cef_string_list_append');
  cef_string_list_clear  := GetProcAddress(FLibHandle, 'cef_string_list_clear');
  cef_string_list_free   := GetProcAddress(FLibHandle, 'cef_string_list_free');
  cef_string_list_copy   := GetProcAddress(FLibHandle, 'cef_string_list_copy');

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
  cef_string_map_alloc  := GetProcAddress(FLibHandle, 'cef_string_map_alloc');
  cef_string_map_size   := GetProcAddress(FLibHandle, 'cef_string_map_size');
  cef_string_map_find   := GetProcAddress(FLibHandle, 'cef_string_map_find');
  cef_string_map_key    := GetProcAddress(FLibHandle, 'cef_string_map_key');
  cef_string_map_value  := GetProcAddress(FLibHandle, 'cef_string_map_value');
  cef_string_map_append := GetProcAddress(FLibHandle, 'cef_string_map_append');
  cef_string_map_clear  := GetProcAddress(FLibHandle, 'cef_string_map_clear');
  cef_string_map_free   := GetProcAddress(FLibHandle, 'cef_string_map_free');

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
  cef_string_multimap_alloc      := GetProcAddress(FLibHandle, 'cef_string_multimap_alloc');
  cef_string_multimap_size       := GetProcAddress(FLibHandle, 'cef_string_multimap_size');
  cef_string_multimap_find_count := GetProcAddress(FLibHandle, 'cef_string_multimap_find_count');
  cef_string_multimap_enumerate  := GetProcAddress(FLibHandle, 'cef_string_multimap_enumerate');
  cef_string_multimap_key        := GetProcAddress(FLibHandle, 'cef_string_multimap_key');
  cef_string_multimap_value      := GetProcAddress(FLibHandle, 'cef_string_multimap_value');
  cef_string_multimap_append     := GetProcAddress(FLibHandle, 'cef_string_multimap_append');
  cef_string_multimap_clear      := GetProcAddress(FLibHandle, 'cef_string_multimap_clear');
  cef_string_multimap_free       := GetProcAddress(FLibHandle, 'cef_string_multimap_free');

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
  cef_string_wide_set             := GetProcAddress(FLibHandle, 'cef_string_wide_set');
  cef_string_utf8_set             := GetProcAddress(FLibHandle, 'cef_string_utf8_set');
  cef_string_utf16_set            := GetProcAddress(FLibHandle, 'cef_string_utf16_set');
  cef_string_wide_clear           := GetProcAddress(FLibHandle, 'cef_string_wide_clear');
  cef_string_utf8_clear           := GetProcAddress(FLibHandle, 'cef_string_utf8_clear');
  cef_string_utf16_clear          := GetProcAddress(FLibHandle, 'cef_string_utf16_clear');
  cef_string_wide_cmp             := GetProcAddress(FLibHandle, 'cef_string_wide_cmp');
  cef_string_utf8_cmp             := GetProcAddress(FLibHandle, 'cef_string_utf8_cmp');
  cef_string_utf16_cmp            := GetProcAddress(FLibHandle, 'cef_string_utf16_cmp');
  cef_string_wide_to_utf8         := GetProcAddress(FLibHandle, 'cef_string_wide_to_utf8');
  cef_string_utf8_to_wide         := GetProcAddress(FLibHandle, 'cef_string_utf8_to_wide');
  cef_string_wide_to_utf16        := GetProcAddress(FLibHandle, 'cef_string_wide_to_utf16');
  cef_string_utf16_to_wide        := GetProcAddress(FLibHandle, 'cef_string_utf16_to_wide');
  cef_string_utf8_to_utf16        := GetProcAddress(FLibHandle, 'cef_string_utf8_to_utf16');
  cef_string_utf16_to_utf8        := GetProcAddress(FLibHandle, 'cef_string_utf16_to_utf8');
  cef_string_ascii_to_wide        := GetProcAddress(FLibHandle, 'cef_string_ascii_to_wide');
  cef_string_ascii_to_utf16       := GetProcAddress(FLibHandle, 'cef_string_ascii_to_utf16');
  cef_string_userfree_wide_alloc  := GetProcAddress(FLibHandle, 'cef_string_userfree_wide_alloc');
  cef_string_userfree_utf8_alloc  := GetProcAddress(FLibHandle, 'cef_string_userfree_utf8_alloc');
  cef_string_userfree_utf16_alloc := GetProcAddress(FLibHandle, 'cef_string_userfree_utf16_alloc');
  cef_string_userfree_wide_free   := GetProcAddress(FLibHandle, 'cef_string_userfree_wide_free');
  cef_string_userfree_utf8_free   := GetProcAddress(FLibHandle, 'cef_string_userfree_utf8_free');
  cef_string_userfree_utf16_free  := GetProcAddress(FLibHandle, 'cef_string_userfree_utf16_free');
  cef_string_utf16_to_lower       := GetProcAddress(FLibHandle, 'cef_string_utf16_to_lower');
  cef_string_utf16_to_upper       := GetProcAddress(FLibHandle, 'cef_string_utf16_to_upper');

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
  cef_get_current_platform_thread_id     := GetProcAddress(FLibHandle, 'cef_get_current_platform_thread_id');
  cef_get_current_platform_thread_handle := GetProcAddress(FLibHandle, 'cef_get_current_platform_thread_handle');

  Result := assigned(cef_get_current_platform_thread_id) and
            assigned(cef_get_current_platform_thread_handle);
end;

function TCefApplication.Load_cef_trace_event_internal_h : boolean;
begin
  cef_trace_event_instant         := GetProcAddress(FLibHandle, 'cef_trace_event_instant');
  cef_trace_event_begin           := GetProcAddress(FLibHandle, 'cef_trace_event_begin');
  cef_trace_event_end             := GetProcAddress(FLibHandle, 'cef_trace_event_end');
  cef_trace_counter               := GetProcAddress(FLibHandle, 'cef_trace_counter');
  cef_trace_counter_id            := GetProcAddress(FLibHandle, 'cef_trace_counter_id');
  cef_trace_event_async_begin     := GetProcAddress(FLibHandle, 'cef_trace_event_async_begin');
  cef_trace_event_async_step_into := GetProcAddress(FLibHandle, 'cef_trace_event_async_step_into');
  cef_trace_event_async_step_past := GetProcAddress(FLibHandle, 'cef_trace_event_async_step_past');
  cef_trace_event_async_end       := GetProcAddress(FLibHandle, 'cef_trace_event_async_end');

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


// TCefAppOwn

procedure cef_app_on_before_command_line_processing(self: PCefApp;
                                                    const process_type: PCefString;
                                                          command_line: PCefCommandLine); stdcall;
begin
  TCefAppOwn(CefGetObject(self)).OnBeforeCommandLineProcessing(CefString(process_type),
                                                               TCefCommandLineRef.UnWrap(command_line));
end;

procedure cef_app_on_register_custom_schemes(self: PCefApp; registrar: PCefSchemeRegistrar); stdcall;
var
  TempWrapper : TCefSchemeRegistrarRef;
begin
  TempWrapper := nil;

  try
    try
      TempWrapper := TCefSchemeRegistrarRef.Create(registrar);

      TCefAppOwn(CefGetObject(self)).OnRegisterCustomSchemes(TempWrapper);
    except
      on e : exception do
        if CustomExceptionHandler('cef_app_on_register_custom_schemes', e) then raise;
    end;
  finally
    if (TempWrapper <> nil) then FreeAndNil(TempWrapper);
  end;
end;

function cef_app_get_resource_bundle_handler(self: PCefApp): PCefResourceBundleHandler; stdcall;
begin
  Result := CefGetData(TCefAppOwn(CefGetObject(self)).GetResourceBundleHandler());
end;

function cef_app_get_browser_process_handler(self: PCefApp): PCefBrowserProcessHandler; stdcall;
begin
  Result := CefGetData(TCefAppOwn(CefGetObject(self)).GetBrowserProcessHandler());
end;

function cef_app_get_render_process_handler(self: PCefApp): PCefRenderProcessHandler; stdcall;
begin
  Result := CefGetData(TCefAppOwn(CefGetObject(self)).GetRenderProcessHandler());
end;


constructor TCefAppOwn.Create;
begin
  inherited CreateData(SizeOf(TCefApp));

  with PCefApp(FData)^ do
    begin
      on_before_command_line_processing := cef_app_on_before_command_line_processing;
      on_register_custom_schemes        := cef_app_on_register_custom_schemes;
      get_resource_bundle_handler       := cef_app_get_resource_bundle_handler;
      get_browser_process_handler       := cef_app_get_browser_process_handler;
      get_render_process_handler        := cef_app_get_render_process_handler;
    end;
end;

// TInternalApp

procedure TInternalApp.OnBeforeCommandLineProcessing(const processType: ustring; const commandLine: ICefCommandLine);
begin
  if Assigned(FOnBeforeCommandLineProcessing) then FOnBeforeCommandLineProcessing(processType, commandLine);
end;

procedure TInternalApp.OnRegisterCustomSchemes(const registrar: TCefSchemeRegistrarRef);
begin
  if Assigned(FOnRegisterCustomSchemes) then FOnRegisterCustomSchemes(registrar);
end;

function TInternalApp.GetResourceBundleHandler: ICefResourceBundleHandler;
begin
  Result := nil;
  if assigned(FOnGetResourceBundleHandler) then FOnGetResourceBundleHandler(Result);
end;

function TInternalApp.GetBrowserProcessHandler: ICefBrowserProcessHandler;
begin
  Result := nil;
  if assigned(FOnGetBrowserProcessHandler) then FOnGetBrowserProcessHandler(Result);
end;

function TInternalApp.GetRenderProcessHandler: ICefRenderProcessHandler;
begin
  Result := nil;
  if assigned(FOnGetRenderProcessHandler) then FOnGetRenderProcessHandler(Result);
end;

constructor TInternalApp.Create;
begin
  inherited Create;

  FOnBeforeCommandLineProcessing := nil;
  FOnRegisterCustomSchemes       := nil;
  FOnGetResourceBundleHandler    := nil;
  FOnGetBrowserProcessHandler    := nil;
  FOnGetRenderProcessHandler     := nil;
end;

end.
