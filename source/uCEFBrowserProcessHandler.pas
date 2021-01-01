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

unit uCEFBrowserProcessHandler;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes,
  {$ELSE}
  Classes,
  {$ENDIF}
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes, uCEFApplicationCore;

type
  TCefBrowserProcessHandlerOwn = class(TCefBaseRefCountedOwn, ICefBrowserProcessHandler)
    protected
      procedure GetCookieableSchemes(var schemes: TStringList; var include_defaults : boolean); virtual; abstract;
      procedure OnContextInitialized; virtual; abstract;
      procedure OnBeforeChildProcessLaunch(const commandLine: ICefCommandLine); virtual; abstract;
      procedure GetPrintHandler(var aHandler : ICefPrintHandler); virtual;
      procedure OnScheduleMessagePumpWork(const delayMs: Int64); virtual; abstract;
      procedure GetDefaultClient(var aClient : ICefClient); virtual;

      procedure RemoveReferences; virtual; abstract;

    public
      constructor Create; virtual;
  end;

  TCefCustomBrowserProcessHandler = class(TCefBrowserProcessHandlerOwn)
    protected
      FCefApp       : TCefApplicationCore;
      FPrintHandler : ICefPrintHandler;

      procedure GetCookieableSchemes(var schemes: TStringList; var include_defaults : boolean); override;
      procedure OnContextInitialized; override;
      procedure OnBeforeChildProcessLaunch(const commandLine: ICefCommandLine); override;
      procedure GetPrintHandler(var aHandler : ICefPrintHandler); override;
      procedure OnScheduleMessagePumpWork(const delayMs: Int64); override;
      procedure GetDefaultClient(var aClient : ICefClient); override;

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
  uCEFMiscFunctions, uCEFLibFunctions, uCEFCommandLine, uCEFListValue, uCEFConstants, uCEFStringList,
  uCEFPrintHandler;

procedure cef_browser_process_handler_get_cookieable_schemes(self             : PCefBrowserProcessHandler;
                                                             schemes          : TCefStringList;
                                                             include_defaults : PInteger); stdcall;
var
  TempSL     : TStringList;
  TempCefSL  : ICefStringList;
  TempObject : TObject;
  TempIncDef : boolean;
begin
  TempSL := nil;

  try
    try
      TempObject := CefGetObject(self);

      if (schemes <> nil) and (TempObject <> nil) and (TempObject is TCefBrowserProcessHandlerOwn) then
        begin
          TempIncDef := (include_defaults^ <> 0);
          TempSL     := TStringList.Create;
          TempCefSL  := TCefStringListRef.Create(schemes);
          TempCefSL.CopyToStrings(TempSL);

          TCefBrowserProcessHandlerOwn(TempObject).GetCookieableSchemes(TempSL, TempIncDef);

          TempCefSL.Clear;
          TempCefSL.AddStrings(TempSL);

          include_defaults^ := ord(TempIncDef);
        end;
    except
      on e : exception do
        if CustomExceptionHandler('cef_browser_process_handler_get_cookieable_schemes', e) then raise;
    end;
  finally
    if (TempSL <> nil) then FreeAndNil(TempSL);
    TempCefSL := nil;
  end;
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

function cef_browser_process_handler_get_print_handler(self: PCefBrowserProcessHandler): PCefPrintHandler; stdcall;
var
  TempObject  : TObject;
  TempHandler : ICefPrintHandler;
begin
  Result     := nil;
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and
     (TempObject is TCefBrowserProcessHandlerOwn) then
    try
      TempHandler := nil;
      TCefBrowserProcessHandlerOwn(TempObject).GetPrintHandler(TempHandler);
      if (TempHandler <> nil) then Result := TempHandler.Wrap;
    finally
      TempHandler := nil;
    end;
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
      get_cookieable_schemes           := {$IFDEF FPC}@{$ENDIF}cef_browser_process_handler_get_cookieable_schemes;
      on_context_initialized           := {$IFDEF FPC}@{$ENDIF}cef_browser_process_handler_on_context_initialized;
      on_before_child_process_launch   := {$IFDEF FPC}@{$ENDIF}cef_browser_process_handler_on_before_child_process_launch;
      get_print_handler                := {$IFDEF FPC}@{$ENDIF}cef_browser_process_handler_get_print_handler;
      on_schedule_message_pump_work    := {$IFDEF FPC}@{$ENDIF}cef_browser_process_handler_on_schedule_message_pump_work;
      get_default_client               := {$IFDEF FPC}@{$ENDIF}cef_browser_process_handler_get_default_client;
    end;
end;

procedure TCefBrowserProcessHandlerOwn.GetPrintHandler(var aHandler : ICefPrintHandler);
begin
  aHandler := nil;
end;

procedure TCefBrowserProcessHandlerOwn.GetDefaultClient(var aClient : ICefClient);
begin
  aClient := nil;
end;


// TCefCustomBrowserProcessHandler


constructor TCefCustomBrowserProcessHandler.Create(const aCefApp : TCefApplicationCore);
begin
  inherited Create;

  FCefApp := aCefApp;

  if (FCefApp <> nil) and FCefApp.MustCreatePrintHandler then
    FPrintHandler := TCustomPrintHandler.Create(FCefApp)
   else
    FPrintHandler := nil;
end;

destructor TCefCustomBrowserProcessHandler.Destroy;
begin
  RemoveReferences;

  inherited Destroy;
end;

procedure TCefCustomBrowserProcessHandler.RemoveReferences;
begin
  if (FPrintHandler <> nil) then
    FPrintHandler.RemoveReferences;

  FCefApp       := nil;
  FPrintHandler := nil;
end;

procedure TCefCustomBrowserProcessHandler.GetCookieableSchemes(var schemes          : TStringList;
                                                               var include_defaults : boolean);
begin
  try
    if (FCefApp <> nil) then FCefApp.Internal_GetCookieableSchemes(schemes, include_defaults);
  except
    on e : exception do
      if CustomExceptionHandler('TCefCustomBrowserProcessHandler.GetCookieableSchemes', e) then raise;
  end;
end;

procedure TCefCustomBrowserProcessHandler.OnContextInitialized;
begin
  try
    if (FCefApp <> nil) then FCefApp.Internal_OnContextInitialized;
  except
    on e : exception do
      if CustomExceptionHandler('TCefCustomBrowserProcessHandler.OnContextInitialized', e) then raise;
  end;
end;

procedure TCefCustomBrowserProcessHandler.OnBeforeChildProcessLaunch(const commandLine: ICefCommandLine);
begin
  try
    if (FCefApp <> nil) then FCefApp.Internal_OnBeforeChildProcessLaunch(commandLine);
  except
    on e : exception do
      if CustomExceptionHandler('TCefCustomBrowserProcessHandler.OnBeforeChildProcessLaunch', e) then raise;
  end;
end;

procedure TCefCustomBrowserProcessHandler.GetPrintHandler(var aHandler : ICefPrintHandler);
begin
  if (FPrintHandler <> nil) then
    aHandler := FPrintHandler
   else
    inherited GetPrintHandler(aHandler);
end;

procedure TCefCustomBrowserProcessHandler.OnScheduleMessagePumpWork(const delayMs: Int64);
begin
  try
    if (FCefApp <> nil) then FCefApp.Internal_OnScheduleMessagePumpWork(delayMs);
  except
    on e : exception do
      if CustomExceptionHandler('TCefCustomBrowserProcessHandler.OnScheduleMessagePumpWork', e) then raise;
  end;
end;

procedure TCefCustomBrowserProcessHandler.GetDefaultClient(var aClient : ICefClient);
begin
  try
    if (FCefApp <> nil) then FCefApp.Internal_GetDefaultClient(aClient);
  except
    on e : exception do
      if CustomExceptionHandler('TCefCustomBrowserProcessHandler.GetDefaultClient', e) then raise;
  end;
end;

end.
