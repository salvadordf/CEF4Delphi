unit GlobalCefApplication;

{$mode ObjFPC}{$H+}
{$IFDEF MSWINDOWS}{$I ..\..\..\source\cef.inc}{$ELSE}{$I ../../../source/cef.inc}{$ENDIF}

interface

uses
  uCEFApplication, uCEFWorkScheduler, FileUtil;

procedure CreateGlobalCEFApp;

implementation

procedure GlobalCEFApp_OnScheduleMessagePumpWork(const aDelayMS : int64);
begin
  if (GlobalCEFWorkScheduler <> nil) then GlobalCEFWorkScheduler.ScheduleMessagePumpWork(aDelayMS);
end;

procedure CreateGlobalCEFApp;
begin
  if GlobalCEFApp <> nil then
    exit;

  // TCEFWorkScheduler will call cef_do_message_loop_work when
  // it's told in the GlobalCEFApp.OnScheduleMessagePumpWork event.
  // GlobalCEFWorkScheduler needs to be created before the
  // GlobalCEFApp.StartMainProcess call.
  GlobalCEFWorkScheduler := TCEFWorkScheduler.Create(nil);

  GlobalCEFApp                           := TCefApplication.Create;
  GlobalCEFApp.ExternalMessagePump       := True;
  GlobalCEFApp.MultiThreadedMessageLoop  := False;
  GlobalCEFApp.OnScheduleMessagePumpWork := @GlobalCEFApp_OnScheduleMessagePumpWork;

  {$IFnDEF MACOSX}
  (* Use AppHelper as subprocess, instead of the main exe *)
  //GlobalCEFApp.BrowserSubprocessPath := 'AppHelper' + GetExeExt;
  {$ENDIF}

  {$IFDEF MACOSX}
  (* Enable the below to prevent being asked for permission to access "Chromium Safe Storage"
     If set to true, Cookies will not be encrypted.
  *)
  //GlobalCEFApp.UseMockKeyChain := True;
  {$ENDIF}
  {$IFDEF LINUX}
  // This is a workaround for the 'GPU is not usable error' issue :
  // https://bitbucket.org/chromiumembedded/cef/issues/2964/gpu-is-not-usable-error-during-cef
  GlobalCEFApp.DisableZygote := True; // this property adds the "--no-zygote" command line switch
  {$ENDIF}
  {
  GlobalCEFApp.LogFile     := 'cef.log';
  GlobalCEFApp.LogSeverity := LOGSEVERITY_VERBOSE;
  }
end;

end.

