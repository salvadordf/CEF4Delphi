unit uCEFBrowserProcessHandler;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes,
  {$ELSE}
  Classes,
  {$ENDIF}
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes, uCEFApplicationCore, uCEFPreferenceRegistrar;

type
  TCefBrowserProcessHandlerOwn = class(TCefBaseRefCountedOwn, ICefBrowserProcessHandler)
    protected
      procedure OnRegisterCustomPreferences(type_: TCefPreferencesType; registrar: PCefPreferenceRegistrar); virtual; abstract;
      procedure OnContextInitialized; virtual; abstract;
      procedure OnBeforeChildProcessLaunch(const commandLine: ICefCommandLine); virtual; abstract;
      procedure OnAlreadyRunningAppRelaunch(const commandLine: ICefCommandLine; const current_directory: ustring; var aResult: boolean); virtual; abstract;
      procedure OnScheduleMessagePumpWork(const delayMs: Int64); virtual; abstract;
      procedure GetDefaultClient(var aClient : ICefClient); virtual;
      procedure GetDefaultRequestContextHandler(var aRequestContextHandler : ICefRequestContextHandler); virtual;

      procedure RemoveReferences; virtual; abstract;

    public
      constructor Create; virtual;
  end;

  TCefCustomBrowserProcessHandler = class(TCefBrowserProcessHandlerOwn)
    protected
      FCefApp       : TCefApplicationCore;

      procedure OnRegisterCustomPreferences(type_: TCefPreferencesType; registrar: PCefPreferenceRegistrar); override;
      procedure OnContextInitialized; override;
      procedure OnBeforeChildProcessLaunch(const commandLine: ICefCommandLine); override;
      procedure OnAlreadyRunningAppRelaunch(const commandLine: ICefCommandLine; const current_directory: ustring; var aResult: boolean); override;
      procedure OnScheduleMessagePumpWork(const delayMs: Int64); override;
      procedure GetDefaultClient(var aClient : ICefClient); override;
      procedure GetDefaultRequestContextHandler(var aRequestContextHandler : ICefRequestContextHandler); override;

      procedure RemoveReferences; override;

    public
      constructor Create(const aCefApp : TCefApplicationCore); reintroduce;
      destructor  Destroy; override;
  end;

implementation

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  uCEFMiscFunctions, uCEFLibFunctions, uCEFCommandLine, uCEFListValue, uCEFConstants, uCEFStringList;

procedure cef_browser_process_handler_on_register_custom_preferences(self: PCefBrowserProcessHandler; type_: TCefPreferencesType; registrar: PCefPreferenceRegistrar); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  // We have to wrap registrar inside TCefBrowserProcessHandlerOwn.OnRegisterCustomPreferences to avoid a circular reference
  if (TempObject <> nil) and
     (TempObject is TCefBrowserProcessHandlerOwn) then
    TCefBrowserProcessHandlerOwn(TempObject).OnRegisterCustomPreferences(type_, registrar);
end;

procedure cef_browser_process_handler_on_context_initialized(self: PCefBrowserProcessHandler); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and
     (TempObject is TCefBrowserProcessHandlerOwn) then
    TCefBrowserProcessHandlerOwn(TempObject).OnContextInitialized;
end;

procedure cef_browser_process_handler_on_before_child_process_launch(self         : PCefBrowserProcessHandler;
                                                                     command_line : PCefCommandLine); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and
     (TempObject is TCefBrowserProcessHandlerOwn) then
    TCefBrowserProcessHandlerOwn(TempObject).OnBeforeChildProcessLaunch(TCefCommandLineRef.UnWrap(command_line));
end;

function cef_browser_process_handler_on_already_running_app_relaunch(      self              : PCefBrowserProcessHandler;
                                                                           command_line      : PCefCommandLine;
                                                                     const current_directory : PCefString): integer; stdcall;
var
  TempObject : TObject;
  TempResult : boolean;
begin
  TempObject := CefGetObject(self);
  TempResult := False;

  if (TempObject <> nil) and
     (TempObject is TCefBrowserProcessHandlerOwn) then
    TCefBrowserProcessHandlerOwn(TempObject).OnAlreadyRunningAppRelaunch(TCefCommandLineRef.UnWrap(command_line),
                                                                         CefString(current_directory),
                                                                         TempResult);

  Result := Ord(TempResult);
end;

procedure cef_browser_process_handler_on_schedule_message_pump_work(self     : PCefBrowserProcessHandler;
                                                                    delay_ms : Int64); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and
     (TempObject is TCefBrowserProcessHandlerOwn) then
    TCefBrowserProcessHandlerOwn(TempObject).OnScheduleMessagePumpWork(delay_ms);
end;

function cef_browser_process_handler_get_default_client(self: PCefBrowserProcessHandler): PCefClient; stdcall;
var
  TempObject : TObject;
  TempClient : ICefClient;
begin
  Result     := nil;
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and
     (TempObject is TCefBrowserProcessHandlerOwn) then
    try
      TempClient := nil;
      TCefBrowserProcessHandlerOwn(TempObject).GetDefaultClient(TempClient);
      if (TempClient <> nil) then Result := TempClient.Wrap;
    finally
      TempClient := nil;
    end;
end;

constructor TCefBrowserProcessHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefBrowserProcessHandler));

  with PCefBrowserProcessHandler(FData)^ do
    begin
      on_register_custom_preferences   := {$IFDEF FPC}@{$ENDIF}cef_browser_process_handler_on_register_custom_preferences;
      on_context_initialized           := {$IFDEF FPC}@{$ENDIF}cef_browser_process_handler_on_context_initialized;
      on_before_child_process_launch   := {$IFDEF FPC}@{$ENDIF}cef_browser_process_handler_on_before_child_process_launch;
      on_already_running_app_relaunch  := {$IFDEF FPC}@{$ENDIF}cef_browser_process_handler_on_already_running_app_relaunch;
      on_schedule_message_pump_work    := {$IFDEF FPC}@{$ENDIF}cef_browser_process_handler_on_schedule_message_pump_work;
      get_default_client               := {$IFDEF FPC}@{$ENDIF}cef_browser_process_handler_get_default_client;
    end;
end;

procedure TCefBrowserProcessHandlerOwn.GetDefaultClient(var aClient : ICefClient);
begin
  aClient := nil;
end;

procedure TCefBrowserProcessHandlerOwn.GetDefaultRequestContextHandler(var aRequestContextHandler : ICefRequestContextHandler);
begin
  aRequestContextHandler := nil;
end;


// TCefCustomBrowserProcessHandler


constructor TCefCustomBrowserProcessHandler.Create(const aCefApp : TCefApplicationCore);
begin
  inherited Create;

  FCefApp := aCefApp;
end;

destructor TCefCustomBrowserProcessHandler.Destroy;
begin
  RemoveReferences;

  inherited Destroy;
end;
procedure TCefCustomBrowserProcessHandler.RemoveReferences;
begin
  FCefApp := nil;
end;

procedure TCefCustomBrowserProcessHandler.OnRegisterCustomPreferences(type_     : TCefPreferencesType;
                                                                      registrar : PCefPreferenceRegistrar);
begin
    try
      if (FCefApp <> nil) then
        IApplicationCoreEvents(FCefApp).doOnRegisterCustomPreferences(type_, registrar);
    except
      on e : exception do
        if CustomExceptionHandler('TCefCustomBrowserProcessHandler.OnRegisterCustomPreferences', e) then raise;
    end;
end;

procedure TCefCustomBrowserProcessHandler.OnContextInitialized;
begin
  try
    if (FCefApp <> nil) then
      IApplicationCoreEvents(FCefApp).doOnContextInitialized;
  except
    on e : exception do
      if CustomExceptionHandler('TCefCustomBrowserProcessHandler.OnContextInitialized', e) then raise;
  end;
end;

procedure TCefCustomBrowserProcessHandler.OnBeforeChildProcessLaunch(const commandLine: ICefCommandLine);
begin
  try
    if (FCefApp <> nil) then
      IApplicationCoreEvents(FCefApp).doOnBeforeChildProcessLaunch(commandLine);
  except
    on e : exception do
      if CustomExceptionHandler('TCefCustomBrowserProcessHandler.OnBeforeChildProcessLaunch', e) then raise;
  end;
end;

procedure TCefCustomBrowserProcessHandler.OnAlreadyRunningAppRelaunch(const commandLine: ICefCommandLine; const current_directory: ustring; var aResult: boolean);
begin
  try
    if (FCefApp <> nil) then
      IApplicationCoreEvents(FCefApp).doOnAlreadyRunningAppRelaunch(commandLine, current_directory, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCefCustomBrowserProcessHandler.OnAlreadyRunningAppRelaunch', e) then raise;
  end;
end;

procedure TCefCustomBrowserProcessHandler.OnScheduleMessagePumpWork(const delayMs: Int64);
begin
  try
    if (FCefApp <> nil) then
      IApplicationCoreEvents(FCefApp).doOnScheduleMessagePumpWork(delayMs);
  except
    on e : exception do
      if CustomExceptionHandler('TCefCustomBrowserProcessHandler.OnScheduleMessagePumpWork', e) then raise;
  end;
end;

procedure TCefCustomBrowserProcessHandler.GetDefaultClient(var aClient : ICefClient);
begin
  try
    if (FCefApp <> nil) then
      IApplicationCoreEvents(FCefApp).doGetDefaultClient(aClient);
  except
    on e : exception do
      if CustomExceptionHandler('TCefCustomBrowserProcessHandler.GetDefaultClient', e) then raise;
  end;
end;

procedure TCefCustomBrowserProcessHandler.GetDefaultRequestContextHandler(var aRequestContextHandler : ICefRequestContextHandler);
begin
  try
    if (FCefApp <> nil) then
      IApplicationCoreEvents(FCefApp).doGetDefaultRequestContextHandler(aRequestContextHandler);
  except
    on e : exception do
      if CustomExceptionHandler('TCefCustomBrowserProcessHandler.GetDefaultRequestContextHandler', e) then raise;
  end;
end;

end.
