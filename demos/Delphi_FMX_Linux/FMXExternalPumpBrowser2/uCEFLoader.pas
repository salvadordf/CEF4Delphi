unit uCEFLoader;

interface

uses
  // This unit *MUST NOT* have any reference to FMX units.
  // The reason for this is that Delphi will change the initialization order
  // and then FMX (including GTK) will be initialized before this unit.
  // That's the reason why we use GlobalCEFWorkScheduler even if this is an FMX
  // project.
  // Read the answer to this question for more more information :
  // https://stackoverflow.com/questions/52103407/changing-the-initialization-order-of-the-unit-in-delphi
  System.IOUtils,
  FMUX.Api, // FMUX.Api is part of the FMXLinux project
  uCEFApplication, uCEFConstants, uCEFTimerWorkScheduler, uCEFLinuxFunctions,
  uCEFLinuxTypes;

procedure InitializeGTK;

implementation

function CustomX11ErrorHandler(Display:PDisplay; ErrorEv:PXErrorEvent):longint;cdecl;
begin
  Result := 0;
end;

function CustomXIOErrorHandler(Display:PDisplay):longint;cdecl;
begin
  Result := 0;
end;

procedure GlobalCEFApp_OnScheduleMessagePumpWork(const aDelayMS : int64);
begin
  if (GlobalCEFTimerWorkScheduler <> nil) then
    GlobalCEFTimerWorkScheduler.ScheduleMessagePumpWork(aDelayMS);
end;

procedure InitializeGlobalCEFApp;
begin
  GlobalCEFApp                            := TCefApplication.Create;
  GlobalCEFApp.WindowlessRenderingEnabled := True;
  GlobalCEFApp.ExternalMessagePump        := True;
  GlobalCEFApp.MultiThreadedMessageLoop   := False;
  GlobalCEFApp.DisableZygote              := True;
  GlobalCEFApp.OnScheduleMessagePumpWork  := GlobalCEFApp_OnScheduleMessagePumpWork;

  // Use these settings if you already have the CEF binaries in a directory called "cef" inside your home directory.
  // You can also use the "Deployment" window but debugging might be slower.
  GlobalCEFApp.FrameworkDirPath      := TPath.GetHomePath + TPath.DirectorySeparatorChar + 'cef';
  GlobalCEFApp.ResourcesDirPath      := GlobalCEFApp.FrameworkDirPath;
  GlobalCEFApp.LocalesDirPath        := GlobalCEFApp.FrameworkDirPath + TPath.DirectorySeparatorChar + 'locales';
  GlobalCEFApp.cache                 := GlobalCEFApp.FrameworkDirPath + TPath.DirectorySeparatorChar + 'cache';
  GlobalCEFApp.BrowserSubprocessPath := GlobalCEFApp.FrameworkDirPath + TPath.DirectorySeparatorChar + 'FMXExternalPumpBrowser2_sp';

  // TCEFTimerWorkScheduler will call cef_do_message_loop_work when
  // it's told in the GlobalCEFApp.OnScheduleMessagePumpWork event.
  // GlobalCEFTimerWorkScheduler needs to be created before the
  // GlobalCEFApp.StartMainProcess call.
  // We use CreateDelayed in order to have a single thread in the process while
  // CEF is initialized.
  GlobalCEFTimerWorkScheduler := TCEFTimerWorkScheduler.Create;

  {$IFDEF DEBUG}
  GlobalCEFApp.LogFile     := TPath.GetHomePath + TPath.DirectorySeparatorChar + 'debug.log';
  GlobalCEFApp.LogSeverity := LOGSEVERITY_INFO;
  {$ENDIF}

  // This is a workaround to fix a Chromium initialization crash.
  // The current FMX solution to initialize CEF with a loader unit
  // creates a race condition with the media key controller in Chromium.
  GlobalCEFApp.DisableFeatures := 'HardwareMediaKeyHandling';

  GlobalCEFApp.StartMainProcess;
end;

procedure FmuxLog(S: PChar); cdecl;
begin
  Writeln(S);
end;

procedure InitializeGTK;
begin
  FmuxSetLog(FmuxLog);
  FmuxInit(FMUX_INIT_NOWAYLAND);

  // Install xlib error handlers so that the application won't be terminated
  // on non-fatal errors. Must be done after initializing GTK.
  XSetErrorHandler(@CustomX11ErrorHandler);
  XSetIOErrorHandler(@CustomXIOErrorHandler);

  // GTK is now initialized and we can read the screen scale.
  GlobalCEFApp.UpdateDeviceScaleFactor;
end;

initialization
  InitializeGlobalCEFApp;

finalization
  if (GlobalCEFTimerWorkScheduler <> nil) then GlobalCEFTimerWorkScheduler.StopScheduler;
  DestroyGlobalCEFApp;
  DestroyGlobalCEFTimerWorkScheduler;

end.
