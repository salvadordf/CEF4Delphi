unit GlobalCefApplication;

{$mode ObjFPC}{$H+}
{$I ../../../source/cef.inc}

interface

uses
  uCEFApplication, uCEFWorkScheduler, uCEFConstants;

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

  (* Enable the below to prevent being asked for permission to access "Chromium Safe Storage"
     If set to true, Cookies will not be encrypted.
  *)
  GlobalCEFApp.UseMockKeyChain := True;
  //GlobalCEFApp.EnableGPU := False;
  //GlobalCEFApp.LogFile     := 'debug.log';
  //GlobalCEFApp.LogSeverity := LOGSEVERITY_VERBOSE;

end;

end.

