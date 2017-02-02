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

interface

uses
  WinApi.Windows,
  uCEFTypes, uCEFInterfaces, uCEFBase;

type
  TInternalApp = class;

  TCefApplication = class
    protected
      FLoaded                      : boolean;
      FMustShutDown                : boolean;
      FCache                       : ustring;
      FCookies                     : ustring;
      FUserDataPath                : ustring;
      FUserAgent                   : ustring;
      FProductVersion              : ustring;
      FLocale                      : ustring;
      FLogFile                     : ustring;
      FBrowserSubprocessPath       : ustring;
      FFrameworkDirPath            : ustring;
      FLogSeverity                 : TCefLogSeverity;
      FJavaScriptFlags             : ustring;
      FResourcesDirPath            : ustring;
      FLocalesDirPath              : ustring;
      FSingleProcess               : Boolean;
      FNoSandbox                   : Boolean;
      FCommandLineArgsDisabled     : Boolean;
      FPackLoadingDisabled         : Boolean;
      FRemoteDebuggingPort         : Integer;
      FUncaughtExceptionStackSize  : Integer;
      FContextSafetyImplementation : Integer;
      FPersistSessionCookies       : Boolean;
      FPersistUserPreferences      : boolean;
      FIgnoreCertificateErrors     : Boolean;
      FEnableNetSecurityExpiration : boolean;
      FBackgroundColor             : TCefColor;
      FAcceptLanguageList          : ustring;
      FWindowsSandboxInfo          : Pointer;
      FWindowlessRenderingEnabled  : Boolean;
      FMultiThreadedMessageLoop    : boolean;
      FExternalMessagePump         : boolean;
      FDeleteCache                 : boolean;
      FDeleteCookies               : boolean;
      FApp                         : TInternalApp;
      FAppIntf                     : ICefApp;
      FCustomCommandLine           : ustring;
      FFlashEnabled                : boolean;

      procedure ShutDown;
      function  ExecuteProcess : integer;
      procedure InitializeSettings(var aSettings : TCefSettings);
      function  InitializeLibrary : boolean;
      function  CreateInternalApp : boolean;
      function  MultiExeProcessing : boolean;
      function  SingleExeProcessing : boolean;

      procedure App_OnBeforeCommandLineProc(const processType: ustring; const commandLine: ICefCommandLine);

    public
      constructor Create;
      destructor  Destroy; override;
      function    StartMainProcess : boolean;
      function    StartSubProcess : boolean;

      property Cache                       : ustring           read FCache                       write FCache;
      property Cookies                     : ustring           read FCookies                     write FCookies;
      property UserDataPath                : ustring           read FUserDataPath                write FUserDataPath;
      property UserAgent                   : ustring           read FUserAgent                   write FUserAgent;
      property ProductVersion              : ustring           read FProductVersion              write FProductVersion;
      property Locale                      : ustring           read FLocale                      write FLocale;
      property LogFile                     : ustring           read FLogFile                     write FLogFile;
      property BrowserSubprocessPath       : ustring           read FBrowserSubprocessPath       write FBrowserSubprocessPath;
      property FrameworkDirPath            : ustring           read FFrameworkDirPath            write FFrameworkDirPath;
      property LogSeverity                 : TCefLogSeverity   read FLogSeverity                 write FLogSeverity;
      property JavaScriptFlags             : ustring           read FJavaScriptFlags             write FJavaScriptFlags;
      property ResourcesDirPath            : ustring           read FResourcesDirPath            write FResourcesDirPath;
      property LocalesDirPath              : ustring           read FLocalesDirPath              write FLocalesDirPath;
      property SingleProcess               : Boolean           read FSingleProcess               write FSingleProcess;
      property NoSandbox                   : Boolean           read FNoSandbox                   write FNoSandbox;
      property CommandLineArgsDisabled     : Boolean           read FCommandLineArgsDisabled     write FCommandLineArgsDisabled;
      property PackLoadingDisabled         : Boolean           read FPackLoadingDisabled         write FPackLoadingDisabled;
      property RemoteDebuggingPort         : Integer           read FRemoteDebuggingPort         write FRemoteDebuggingPort;
      property UncaughtExceptionStackSize  : Integer           read FUncaughtExceptionStackSize  write FUncaughtExceptionStackSize;
      property ContextSafetyImplementation : Integer           read FContextSafetyImplementation write FContextSafetyImplementation;
      property PersistSessionCookies       : Boolean           read FPersistSessionCookies       write FPersistSessionCookies;
      property PersistUserPreferences      : Boolean           read FPersistUserPreferences      write FPersistUserPreferences;
      property IgnoreCertificateErrors     : Boolean           read FIgnoreCertificateErrors     write FIgnoreCertificateErrors;
      property EnableNetSecurityExpiration : boolean           read FEnableNetSecurityExpiration write FEnableNetSecurityExpiration;
      property BackgroundColor             : TCefColor         read FBackgroundColor             write FBackgroundColor;
      property AcceptLanguageList          : ustring           read FAcceptLanguageList          write FAcceptLanguageList;
      property WindowsSandboxInfo          : Pointer           read FWindowsSandboxInfo          write FWindowsSandboxInfo;
      property WindowlessRenderingEnabled  : Boolean           read FWindowlessRenderingEnabled  write FWindowlessRenderingEnabled;
      property MultiThreadedMessageLoop    : boolean           read FMultiThreadedMessageLoop    write FMultiThreadedMessageLoop;
      property ExternalMessagePump         : boolean           read FExternalMessagePump         write FExternalMessagePump;
      property DeleteCache                 : boolean           read FDeleteCache                 write FDeleteCache;
      property DeleteCookies               : boolean           read FDeleteCookies               write FDeleteCookies;
      property CustomCommandLine           : ustring           read FCustomCommandLine           write FCustomCommandLine;
      property FlashEnabled                : boolean           read FFlashEnabled                write FFlashEnabled;
  end;

  TCefAppOwn = class(TCefBaseOwn, ICefApp)
    protected
      procedure OnBeforeCommandLineProcessing(const processType: ustring; const commandLine: ICefCommandLine); virtual; abstract;
      procedure OnRegisterCustomSchemes(const registrar: ICefSchemeRegistrar); virtual; abstract;
      function  GetResourceBundleHandler: ICefResourceBundleHandler; virtual; abstract;
      function  GetBrowserProcessHandler: ICefBrowserProcessHandler; virtual; abstract;
      function  GetRenderProcessHandler: ICefRenderProcessHandler; virtual; abstract;

    public
      constructor Create; virtual;
  end;

  TInternalApp = class(TCefAppOwn)
    protected
      FResourceBundleHandler         : ICefResourceBundleHandler;
      FBrowserProcessHandler         : ICefBrowserProcessHandler;
      FRenderProcessHandler          : ICefRenderProcessHandler;
      FOnBeforeCommandLineProcessing : TOnBeforeCommandLineProcessing;
      FOnRegisterCustomSchemes       : TOnRegisterCustomSchemes;

      procedure OnBeforeCommandLineProcessing(const processType: ustring; const commandLine: ICefCommandLine); override;
      procedure OnRegisterCustomSchemes(const registrar: ICefSchemeRegistrar); override;
      function  GetResourceBundleHandler: ICefResourceBundleHandler; override;
      function  GetBrowserProcessHandler: ICefBrowserProcessHandler; override;
      function  GetRenderProcessHandler: ICefRenderProcessHandler; override;

    public
      constructor Create; override;

      property ResourceBundleHandler   : ICefResourceBundleHandler       read FResourceBundleHandler          write FResourceBundleHandler;
      property BrowserProcessHandler   : ICefBrowserProcessHandler       read FBrowserProcessHandler          write FBrowserProcessHandler;
      property RenderProcessHandler    : ICefRenderProcessHandler        read FRenderProcessHandler           write FRenderProcessHandler;
      property OnBeforeCommandLineProc : TOnBeforeCommandLineProcessing  read FOnBeforeCommandLineProcessing  write FOnBeforeCommandLineProcessing;
      property OnRegCustomSchemes      : TOnRegisterCustomSchemes        read FOnRegisterCustomSchemes        write FOnRegisterCustomSchemes;
  end;

var
  GlobalCEFApp : TCefApplication;

implementation

uses
  System.Math, System.IOUtils, System.SysUtils,
  uCEFLibFunctions, uCEFMiscFunctions, uCEFSchemeRegistrar, uCEFCommandLine;

constructor TCefApplication.Create;
begin
  inherited Create;

  FLoaded                      := False;
  FMustShutDown                := False;
  FCache                       := '';
  FCookies                     := '';
  FUserDataPath                := '';
  FUserAgent                   := '';
  FProductVersion              := '';
  FLocale                      := '';
  FLogFile                     := '';
  FBrowserSubprocessPath       := '';
  FFrameworkDirPath            := '';
  FLogSeverity                 := LOGSEVERITY_DISABLE;
  FJavaScriptFlags             := '';
  FResourcesDirPath            := '';
  FLocalesDirPath              := '';
  FSingleProcess               := False;
  FNoSandbox                   := False;
  FCommandLineArgsDisabled     := False;
  FPackLoadingDisabled         := False;
  FRemoteDebuggingPort         := 0;
  FUncaughtExceptionStackSize  := 0;
  FContextSafetyImplementation := 0;
  FPersistSessionCookies       := False;
  FPersistUserPreferences      := False;
  FIgnoreCertificateErrors     := False;
  FEnableNetSecurityExpiration := False;
  FBackgroundColor             := 0;
  FAcceptLanguageList          := '';
  FWindowsSandboxInfo          := nil;
  FWindowlessRenderingEnabled  := False;
  FMultiThreadedMessageLoop    := True;
  FExternalMessagePump         := False;
  FDeleteCache                 := False;
  FDeleteCookies               := False;
  FApp                         := nil;
  FAppIntf                     := nil;
  FFlashEnabled                := True;
  FCustomCommandLine           := '';

  IsMultiThread := True;

  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
end;

destructor TCefApplication.Destroy;
begin
  if FLoaded then ShutDown;

  FAppIntf := nil;
  FApp     := nil;

  inherited Destroy;
end;

function TCefApplication.CreateInternalApp : boolean;
begin
  Result := False;

  try
    if (FApp = nil) then
      begin
        FApp                         := TInternalApp.Create;
        FApp.OnBeforeCommandLineProc := App_OnBeforeCommandLineProc;
        FAppIntf                     := FApp as ICefApp;
        Result                       := (FAppIntf <> nil);
      end;
  except
    on e : exception do
      begin
        {$IFDEF DEBUG}
        OutputDebugString(PWideChar('TCefApplication.CreateInternalApp error: ' + e.Message + chr(0)));
        {$ENDIF}
      end;
  end;
end;

function TCefApplication.MultiExeProcessing : boolean;
begin
  Result := False;

  try
    FLoaded       := True;
    FMustShutDown := True;
    Result        := CreateInternalApp and InitializeLibrary;
  except
    on e : exception do
      begin
        {$IFDEF DEBUG}
        OutputDebugString(PWideChar('TCefApplication.MultiExeProcessing error: ' + e.Message + chr(0)));
        {$ENDIF}
      end;
  end;
end;

function TCefApplication.SingleExeProcessing : boolean;
begin
  Result := False;

  try
    FLoaded := True;

    if CreateInternalApp and (ExecuteProcess < 0) then
      begin
        FMustShutDown := True;
        Result        := InitializeLibrary;
      end;
  except
    on e : exception do
      begin
        {$IFDEF DEBUG}
        OutputDebugString(PWideChar('TCefApplication.SingleExeProcessing error: ' + e.Message + chr(0)));
        {$ENDIF}
      end;
  end;
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
    if not(FSingleProcess) then
      begin
        FLoaded := True;
        Result  := CreateInternalApp and (ExecuteProcess >= 0);
      end;
  except
    on e : exception do
      begin
        {$IFDEF DEBUG}
        OutputDebugString(PWideChar('TCefApplication.StartSubProcess error: ' + e.Message + chr(0)));
        {$ENDIF}
      end;
  end;
end;

procedure TCefApplication.ShutDown;
begin
  try
    if FMustShutDown then cef_shutdown;
  except
    on e : exception do
      begin
        {$IFDEF DEBUG}
        OutputDebugString(PWideChar('TCefApplication.ShutDown error: ' + e.Message + chr(0)));
        {$ENDIF}
      end;
  end;
end;

function TCefApplication.ExecuteProcess : integer;
var
  TempArgs : TCefMainArgs;
begin
  TempArgs.instance := HINSTANCE;
  Result            := cef_execute_process(@TempArgs, CefGetData(FAppIntf), FWindowsSandboxInfo);
end;

procedure TCefApplication.InitializeSettings(var aSettings : TCefSettings);
begin
  aSettings.size                            := SizeOf(TCefSettings);
  aSettings.single_process                  := Ord(FSingleProcess);
  aSettings.no_sandbox                      := Ord(FNoSandbox);
  aSettings.framework_dir_path              := CefString(FFrameworkDirPath);
  aSettings.multi_threaded_message_loop     := Ord(FMultiThreadedMessageLoop);
  aSettings.external_message_pump           := Ord(FExternalMessagePump);
  aSettings.windowless_rendering_enabled    := Ord(FWindowlessRenderingEnabled);
  aSettings.cache_path                      := CefString(FCache);
  aSettings.user_data_path                  := CefString(FUserDataPath);
  aSettings.persist_session_cookies         := Ord(FPersistSessionCookies);
  aSettings.persist_user_preferences        := Ord(FPersistUserPreferences);
  aSettings.browser_subprocess_path         := CefString(FBrowserSubprocessPath);
  aSettings.command_line_args_disabled      := Ord(FCommandLineArgsDisabled);
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
  aSettings.context_safety_implementation   := FContextSafetyImplementation;
  aSettings.ignore_certificate_errors       := Ord(FIgnoreCertificateErrors);
  aSettings.enable_net_security_expiration  := Ord(FEnableNetSecurityExpiration);
  aSettings.background_color                := FBackgroundColor;
  aSettings.accept_language_list            := CefString(FAcceptLanguageList);
end;

function TCefApplication.InitializeLibrary : boolean;
var
  TempSettings : TCefSettings;
begin
  Result := False;

  try
    if FDeleteCache   and (length(FCache)   > 0) then TDirectory.Delete(FCache,   True);
    if FDeleteCookies and (length(FCookies) > 0) then TDirectory.Delete(FCookies, True);

    InitializeSettings(TempSettings);
    Result := (cef_initialize(@HInstance, @TempSettings, CefGetData(FAppIntf), FWindowsSandboxInfo) <> 0);
  except
    on e : exception do
      begin
        {$IFDEF DEBUG}
        OutputDebugString(PWideChar('TCefApplication.InitializeLibrary error: ' + e.Message + chr(0)));
        {$ENDIF}
      end;
  end;
end;

procedure TCefApplication.App_OnBeforeCommandLineProc(const processType : ustring;
                                                      const commandLine : ICefCommandLine);
begin
  if (commandLine <> nil) then
    begin
      if FFlashEnabled then
        begin
          commandLine.AppendSwitch('--enable-gpu-plugin');
          commandLine.AppendSwitch('--enable-accelerated-plugins');
          commandLine.AppendSwitch('--enable-system-flash');
        end;

      if (length(FCustomCommandLine) > 0) then commandLine.AppendSwitch(FCustomCommandLine);
    end;
end;

// TCefAppOwn

procedure cef_app_on_before_command_line_processing(self: PCefApp;
  const process_type: PCefString; command_line: PCefCommandLine); stdcall;
begin
  with TCefAppOwn(CefGetObject(self)) do
    OnBeforeCommandLineProcessing(CefString(process_type), TCefCommandLineRef.UnWrap(command_line));
end;

procedure cef_app_on_register_custom_schemes(self: PCefApp; registrar: PCefSchemeRegistrar); stdcall;
begin
  with TCefAppOwn(CefGetObject(self)) do
    OnRegisterCustomSchemes(TCefSchemeRegistrarRef.UnWrap(registrar));
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

procedure TInternalApp.OnRegisterCustomSchemes(const registrar: ICefSchemeRegistrar);
begin

  if Assigned(FOnRegisterCustomSchemes) then FOnRegisterCustomSchemes(registrar);
end;

function TInternalApp.GetResourceBundleHandler: ICefResourceBundleHandler;
begin
  Result := FResourceBundleHandler;
end;

function TInternalApp.GetBrowserProcessHandler: ICefBrowserProcessHandler;
begin
  Result := FBrowserProcessHandler;
end;

function TInternalApp.GetRenderProcessHandler: ICefRenderProcessHandler;
begin
  Result := FRenderProcessHandler;
end;

constructor TInternalApp.Create;
begin
  inherited Create;

  FResourceBundleHandler         := nil;
  FBrowserProcessHandler         := nil;
  FRenderProcessHandler          := nil;
  FOnBeforeCommandLineProcessing := nil;
  FOnRegisterCustomSchemes       := nil;
end;

end.
