program FMXExternalPumpBrowser;

{$I ..\..\..\source\cef.inc}

uses
  {$IFDEF DELPHI17_UP}
  System.StartUpCopy,
  {$ENDIF }
  FMX.Forms,
  uCEFApplication,
  uCEFTimerWorkScheduler,
  uCEFMacOSFunctions,
  uFMXExternalPumpBrowser in 'uFMXExternalPumpBrowser.pas' {FMXExternalPumpBrowserFrm},
  uFMXApplicationService in 'uFMXApplicationService.pas';

{$R *.res}

begin
  {$IFDEF DEBUG}
  // Copy the CEF framework and the CEF helpers locally instead of deploying
  // them to debug faster.
  // Copy the "Chromium Embedded Framework.framework" directory into the same
  // directory where this project is deployed on the Mac.
  // The 4 "helper" projects in this group should also be deployed in the same
  // directory as this project.
  // CopyCEFHelpers requires that the helper projects end with "_helper",
  // "_helper_gpu", "_helper_plugin" and "_helper_renderer".
  CopyCEFFramework;
  CopyCEFHelpers('FMXExternalPumpBrowser');
  {$ENDIF}

  CreateGlobalCEFApp;

  if GlobalCEFApp.StartMainProcess then
    begin
      Application.Initialize;
      Application.CreateForm(TFMXExternalPumpBrowserFrm, FMXExternalPumpBrowserFrm);
      Application.Run;

      // The form needs to be destroyed *BEFORE* stopping the scheduler.
      FMXExternalPumpBrowserFrm.Free;

      if (GlobalCEFTimerWorkScheduler <> nil) then
        GlobalCEFTimerWorkScheduler.StopScheduler;
    end;

  DestroyGlobalCEFApp;
  DestroyGlobalCEFTimerWorkScheduler;
end.
