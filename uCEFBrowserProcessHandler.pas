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

unit uCEFBrowserProcessHandler;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

interface

uses
  uCEFBase, uCEFInterfaces, uCEFTypes;

type
  TCefBrowserProcessHandlerOwn = class(TCefBaseOwn, ICefBrowserProcessHandler)
  protected
    procedure OnContextInitialized; virtual;
    procedure OnBeforeChildProcessLaunch(const commandLine: ICefCommandLine); virtual;
    procedure OnRenderProcessThreadCreated(const extraInfo: ICefListValue); virtual;
    procedure OnScheduleMessagePumpWork(delayMs: Int64); virtual;
  public
    constructor Create; virtual;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFCommandLine, uCEFListValue;

procedure cef_browser_process_handler_on_context_initialized(self: PCefBrowserProcessHandler); stdcall;
begin
  with TCefBrowserProcessHandlerOwn(CefGetObject(self)) do
    OnContextInitialized;
end;

procedure cef_browser_process_handler_on_before_child_process_launch(
  self: PCefBrowserProcessHandler; command_line: PCefCommandLine); stdcall;
begin
  with TCefBrowserProcessHandlerOwn(CefGetObject(self)) do
    OnBeforeChildProcessLaunch(TCefCommandLineRef.UnWrap(command_line));
end;

procedure cef_browser_process_handler_on_render_process_thread_created(
  self: PCefBrowserProcessHandler; extra_info: PCefListValue); stdcall;
begin
  with TCefBrowserProcessHandlerOwn(CefGetObject(self)) do
    OnRenderProcessThreadCreated(TCefListValueRef.UnWrap(extra_info));
end;

procedure cef_browser_process_handler_on_schedule_message_pump_work(self: PCefBrowserProcessHandler; delay_ms: Int64); stdcall;
begin
  TCefBrowserProcessHandlerOwn(CefGetObject(self)).OnScheduleMessagePumpWork(delay_ms);
end;

constructor TCefBrowserProcessHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefBrowserProcessHandler));

  with PCefBrowserProcessHandler(FData)^ do
    begin
      on_context_initialized := cef_browser_process_handler_on_context_initialized;
      on_before_child_process_launch := cef_browser_process_handler_on_before_child_process_launch;
      on_render_process_thread_created := cef_browser_process_handler_on_render_process_thread_created;
      get_print_handler := nil; // linux
      on_schedule_message_pump_work := cef_browser_process_handler_on_schedule_message_pump_work;
    end;
end;

procedure TCefBrowserProcessHandlerOwn.OnBeforeChildProcessLaunch(const commandLine: ICefCommandLine);
begin

end;

procedure TCefBrowserProcessHandlerOwn.OnContextInitialized;
begin

end;

procedure TCefBrowserProcessHandlerOwn.OnRenderProcessThreadCreated(const extraInfo: ICefListValue);
begin

end;

procedure TCefBrowserProcessHandlerOwn.OnScheduleMessagePumpWork(delayMs: Int64);
begin

end;

end.
