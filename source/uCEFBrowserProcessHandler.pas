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

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes, uCEFApplication;

type
  TCefBrowserProcessHandlerOwn = class(TCefBaseRefCountedOwn, ICefBrowserProcessHandler)
    protected
      procedure OnContextInitialized; virtual; abstract;
      procedure OnBeforeChildProcessLaunch(const commandLine: ICefCommandLine); virtual; abstract;
      procedure OnRenderProcessThreadCreated(const extraInfo: ICefListValue); virtual; abstract;
      function  GetPrintHandler : ICefPrintHandler; virtual;
      procedure OnScheduleMessagePumpWork(const delayMs: Int64); virtual; abstract;

    public
      constructor Create; virtual;
  end;

  TCefCustomBrowserProcessHandler = class(TCefBrowserProcessHandlerOwn)
    protected
      FCefApp : TCefApplication;

      procedure OnContextInitialized; override;
      procedure OnBeforeChildProcessLaunch(const commandLine: ICefCommandLine); override;
      procedure OnRenderProcessThreadCreated(const extraInfo: ICefListValue); override;
      procedure OnScheduleMessagePumpWork(const delayMs: Int64); override;

    public
      constructor Create(const aCefApp : TCefApplication); reintroduce;
      destructor  Destroy; override;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFCommandLine, uCEFListValue, uCEFConstants;

procedure cef_browser_process_handler_on_context_initialized(self: PCefBrowserProcessHandler); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefBrowserProcessHandlerOwn) then
    TCefBrowserProcessHandlerOwn(TempObject).OnContextInitialized;
end;

procedure cef_browser_process_handler_on_before_child_process_launch(self         : PCefBrowserProcessHandler;
                                                                     command_line : PCefCommandLine); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefBrowserProcessHandlerOwn) then
    TCefBrowserProcessHandlerOwn(TempObject).OnBeforeChildProcessLaunch(TCefCommandLineRef.UnWrap(command_line));
end;

procedure cef_browser_process_handler_on_render_process_thread_created(self       : PCefBrowserProcessHandler;
                                                                       extra_info : PCefListValue); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefBrowserProcessHandlerOwn) then
    TCefBrowserProcessHandlerOwn(TempObject).OnRenderProcessThreadCreated(TCefListValueRef.UnWrap(extra_info));
end;

function cef_browser_process_handler_get_print_handler(self: PCefBrowserProcessHandler): PCefPrintHandler; stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefBrowserProcessHandlerOwn) then
    Result := CefGetData(TCefBrowserProcessHandlerOwn(TempObject).GetPrintHandler)
   else
    Result := nil;
end;

procedure cef_browser_process_handler_on_schedule_message_pump_work(self     : PCefBrowserProcessHandler;
                                                                    delay_ms : Int64); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefBrowserProcessHandlerOwn) then
    TCefBrowserProcessHandlerOwn(TempObject).OnScheduleMessagePumpWork(delay_ms);
end;

constructor TCefBrowserProcessHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefBrowserProcessHandler));

  with PCefBrowserProcessHandler(FData)^ do
    begin
      on_context_initialized           := cef_browser_process_handler_on_context_initialized;
      on_before_child_process_launch   := cef_browser_process_handler_on_before_child_process_launch;
      on_render_process_thread_created := cef_browser_process_handler_on_render_process_thread_created;
      get_print_handler                := cef_browser_process_handler_get_print_handler;
      on_schedule_message_pump_work    := cef_browser_process_handler_on_schedule_message_pump_work;
    end;
end;

function TCefBrowserProcessHandlerOwn.GetPrintHandler : ICefPrintHandler;
begin
  Result := nil; // only linux
end;


// TCefCustomBrowserProcessHandler


constructor TCefCustomBrowserProcessHandler.Create(const aCefApp : TCefApplication);
begin
  inherited Create;

  FCefApp := aCefApp;
end;

destructor TCefCustomBrowserProcessHandler.Destroy;
begin
  FCefApp := nil;

  inherited Destroy;
end;

procedure TCefCustomBrowserProcessHandler.OnContextInitialized;
begin
  if (FCefApp <> nil) then FCefApp.Internal_OnContextInitialized;
end;

procedure TCefCustomBrowserProcessHandler.OnBeforeChildProcessLaunch(const commandLine: ICefCommandLine);
begin
  if (FCefApp <> nil) then FCefApp.Internal_OnBeforeChildProcessLaunch(commandLine);
end;

procedure TCefCustomBrowserProcessHandler.OnRenderProcessThreadCreated(const extraInfo: ICefListValue);
begin
  if (FCefApp <> nil) then FCefApp.Internal_OnRenderProcessThreadCreated(extraInfo);
end;

procedure TCefCustomBrowserProcessHandler.OnScheduleMessagePumpWork(const delayMs: Int64);
begin
  if (FCefApp <> nil) then FCefApp.Internal_OnScheduleMessagePumpWork(delayMs);
end;

end.
