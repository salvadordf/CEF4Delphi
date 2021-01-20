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
  System.SyncObjs,
  uCEFApplication, uCEFConstants, uCEFWorkScheduler;

implementation

var
  CEFContextInitEvent : TEvent;

procedure GlobalCEFApp_OnScheduleMessagePumpWork(const aDelayMS : int64);
begin
  if (GlobalCEFWorkScheduler <> nil) then
    GlobalCEFWorkScheduler.ScheduleMessagePumpWork(aDelayMS);
end;

procedure GlobalCEFApp_OnContextInitialized;
begin
  CEFContextInitEvent.SetEvent;
end;

procedure InitializeGlobalCEFApp;
begin
  // TCEFWorkScheduler will call cef_do_message_loop_work when
  // it's told in the GlobalCEFApp.OnScheduleMessagePumpWork event.
  // GlobalCEFWorkScheduler needs to be created before the
  // GlobalCEFApp.StartMainProcess call.
  // We use CreateDelayed in order to have a single thread in the process while
  // CEF is initialized.
  GlobalCEFWorkScheduler := TCEFWorkScheduler.CreateDelayed;

  GlobalCEFApp                            := TCefApplication.Create;
  GlobalCEFApp.WindowlessRenderingEnabled := True;
  GlobalCEFApp.EnableHighDPISupport       := True;
  GlobalCEFApp.ExternalMessagePump        := True;
  GlobalCEFApp.MultiThreadedMessageLoop   := False;
  GlobalCEFApp.DisableZygote              := True;
  GlobalCEFApp.OnScheduleMessagePumpWork  := GlobalCEFApp_OnScheduleMessagePumpWork;
  GlobalCEFApp.OnContextInitialized       := GlobalCEFApp_OnContextInitialized;
  GlobalCEFApp.BrowserSubprocessPath      := 'FMXExternalPumpBrowser2_sp';
  GlobalCEFApp.LogFile                    := 'debug.log';
  GlobalCEFApp.LogSeverity                := LOGSEVERITY_INFO;

  if GlobalCEFApp.StartMainProcess then
    begin
      // Wait until the context is initialized
      CEFContextInitEvent.WaitFor(10000);
      // Now we can create the GlobalCEFWorkScheduler background thread
      GlobalCEFWorkScheduler.CreateThread;
    end;
end;

initialization
  CEFContextInitEvent := TEvent.Create;
  InitializeGlobalCEFApp;

finalization
  if (GlobalCEFWorkScheduler <> nil) then GlobalCEFWorkScheduler.StopScheduler;
  DestroyGlobalCEFApp;
  DestroyGlobalCEFWorkScheduler;
  CEFContextInitEvent.Free;

end.
