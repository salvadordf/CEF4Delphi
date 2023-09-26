program FMXExternalPumpBrowser2_sp;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.IOUtils,
  uCEFApplicationCore, uCEFConstants;

begin
  GlobalCEFApp                            := TCefApplicationCore.Create;
  GlobalCEFApp.WindowlessRenderingEnabled := True;
  GlobalCEFApp.ExternalMessagePump        := True;
  GlobalCEFApp.MultiThreadedMessageLoop   := False;

  // Use these settings if you already have the CEF binaries in a directory called "cef" inside your home directory.
  // You can also use the "Deployment" window but debugging might be slower.
  GlobalCEFApp.FrameworkDirPath     := TPath.GetHomePath + TPath.DirectorySeparatorChar + 'cef';
  GlobalCEFApp.ResourcesDirPath     := GlobalCEFApp.FrameworkDirPath;
  GlobalCEFApp.LocalesDirPath       := GlobalCEFApp.FrameworkDirPath + TPath.DirectorySeparatorChar + 'locales';
  GlobalCEFApp.cache                := GlobalCEFApp.FrameworkDirPath + TPath.DirectorySeparatorChar + 'cache';

  {$IFDEF DEBUG}
  GlobalCEFApp.LogFile     := TPath.GetHomePath + TPath.DirectorySeparatorChar + 'debug.log';
  GlobalCEFApp.LogSeverity := LOGSEVERITY_INFO;
  {$ENDIF}

  // This is a workaround to fix a Chromium initialization crash.
  // The current FMX solution to initialize CEF with a loader unit
  // creates a race condition with the media key controller in Chromium.
  GlobalCEFApp.DisableFeatures := 'HardwareMediaKeyHandling';

  GlobalCEFApp.StartSubProcess;
  DestroyGlobalCEFApp;
end.
