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

unit GlobalCefApplication;

{$mode ObjFPC}{$H+}
{$I cef.inc}

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

