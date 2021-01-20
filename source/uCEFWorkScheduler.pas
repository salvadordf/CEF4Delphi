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

unit uCEFWorkScheduler;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
    {$IFDEF MSWINDOWS}WinApi.Windows, WinApi.Messages,{$ENDIF} System.Classes,
  {$ELSE}
    {$IFDEF MSWINDOWS}Windows,{$ENDIF} Classes,
    {$IFDEF FPC}
    LCLProc, LCLType, LCLIntf, LResources, LMessages, InterfaceBase, {$IFNDEF MSWINDOWS}forms,{$ENDIF}
    {$ELSE}
    Messages,
    {$ENDIF}
  {$ENDIF}
  uCEFConstants, uCEFWorkSchedulerThread;

type
  {$IFNDEF FPC}{$IFDEF DELPHI16_UP}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}{$ENDIF}
  TCEFWorkScheduler = class(TComponent)
    protected
      FThread             : TCEFWorkSchedulerThread;
      FDepleteWorkCycles  : cardinal;
      FDepleteWorkDelay   : cardinal;
      FDefaultInterval    : integer;
      FStopped            : boolean;
      {$IFDEF MSWINDOWS}
      {$WARN SYMBOL_PLATFORM OFF}
      FCompHandle         : HWND;
      FPriority           : TThreadPriority;
      {$WARN SYMBOL_PLATFORM ON}
      {$ENDIF}

      procedure DestroyThread;
      procedure DepleteWork;
      {$IFDEF MSWINDOWS}
      procedure WndProc(var aMessage: TMessage);
      procedure DeallocateWindowHandle;
      {$ELSE}
      {$IFDEF FPC}
      procedure ScheduleWorkAsync(Data: PtrInt);
      {$ENDIF}
      {$ENDIF}
      procedure NextPulse(aInterval : integer);
      procedure ScheduleWork(const delay_ms : int64);
      procedure DoWork;
      procedure DoMessageLoopWork;
      procedure Initialize;

      procedure SetDefaultInterval(aValue : integer);
      {$IFDEF MSWINDOWS}
      {$WARN SYMBOL_PLATFORM OFF}
      procedure SetPriority(aValue : TThreadPriority);
      {$WARN SYMBOL_PLATFORM ON}
      {$ENDIF}

      procedure Thread_OnPulse(Sender : TObject);

    public
      constructor Create(AOwner: TComponent); override;
      constructor CreateDelayed;
      destructor  Destroy; override;
      procedure   ScheduleMessagePumpWork(const delay_ms : int64);
      procedure   StopScheduler;
      procedure   CreateThread;

    published
      {$IFDEF MSWINDOWS}
      {$WARN SYMBOL_PLATFORM OFF}
      property    Priority           : TThreadPriority  read FPriority            write SetPriority         default  tpNormal;
      {$WARN SYMBOL_PLATFORM ON}
      {$ENDIF}
      property    DefaultInterval    : integer          read FDefaultInterval     write SetDefaultInterval  default  CEF_TIMER_MAXDELAY;
      property    DepleteWorkCycles  : cardinal         read FDepleteWorkCycles   write FDepleteWorkCycles  default  CEF_TIMER_DEPLETEWORK_CYCLES;
      property    DepleteWorkDelay   : cardinal         read FDepleteWorkDelay    write FDepleteWorkDelay   default  CEF_TIMER_DEPLETEWORK_DELAY;
  end;

var
  GlobalCEFWorkScheduler : TCEFWorkScheduler = nil;

{$IFDEF FPC}
procedure Register;
{$ENDIF}

procedure DestroyGlobalCEFWorkScheduler;

implementation

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils, System.Math,
  {$ELSE}
  SysUtils, Math,
  {$ENDIF}
  uCEFMiscFunctions, uCEFApplicationCore, uCEFTypes;

procedure DestroyGlobalCEFWorkScheduler;
begin
  if (GlobalCEFWorkScheduler <> nil) then FreeAndNil(GlobalCEFWorkScheduler);
end;

constructor TCEFWorkScheduler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Initialize;

  if not(csDesigning in ComponentState) then
    begin
      {$IFDEF MSWINDOWS}
      if (GlobalCEFApp <> nil) and
         ((GlobalCEFApp.ProcessType = ptBrowser) or GlobalCEFApp.SingleProcess) then
        FCompHandle := AllocateHWnd({$IFDEF FPC}@{$ENDIF}WndProc);
      {$ENDIF}

      CreateThread;
    end;
end;

constructor TCEFWorkScheduler.CreateDelayed;
begin
  inherited Create(nil);

  Initialize;

  if not(csDesigning in ComponentState) then
    begin
      {$IFDEF MSWINDOWS}
      if (GlobalCEFApp <> nil) and
         ((GlobalCEFApp.ProcessType = ptBrowser) or GlobalCEFApp.SingleProcess) then
        FCompHandle := AllocateHWnd({$IFDEF FPC}@{$ENDIF}WndProc);
      {$ENDIF}
    end;
end;

destructor TCEFWorkScheduler.Destroy;
begin
  DestroyThread;
  {$IFDEF MSWINDOWS}
  DeallocateWindowHandle;
  {$ENDIF}
  inherited Destroy;
end;

procedure TCEFWorkScheduler.Initialize;
begin
  FThread             := nil;
  FStopped            := False;
  {$IFDEF MSWINDOWS}
  {$WARN SYMBOL_PLATFORM OFF}
  FCompHandle         := 0;
  FPriority           := tpNormal;
  {$WARN SYMBOL_PLATFORM ON}
  {$ENDIF}
  FDefaultInterval    := CEF_TIMER_MAXDELAY;
  FDepleteWorkCycles  := CEF_TIMER_DEPLETEWORK_CYCLES;
  FDepleteWorkDelay   := CEF_TIMER_DEPLETEWORK_DELAY;
end;

procedure TCEFWorkScheduler.CreateThread;
begin
  if (FThread <> nil) then exit;

  FThread                 := TCEFWorkSchedulerThread.Create;
  {$IFDEF MSWINDOWS}
  FThread.Priority        := FPriority;
  {$ENDIF}
  FThread.DefaultInterval := FDefaultInterval;
  FThread.OnPulse         := {$IFDEF FPC}@{$ENDIF}Thread_OnPulse;
  {$IFDEF DELPHI14_UP}
  FThread.Start;
  {$ELSE}
  {$IFNDEF FPC}
  FThread.Resume;
  {$ELSE}
  FThread.Start;
  {$ENDIF}
  {$ENDIF}
end;

procedure TCEFWorkScheduler.DestroyThread;
begin
  try
    if (FThread <> nil) then
      begin
        FThread.Terminate;
        FThread.NextPulse(0);
        FThread.WaitFor;
        FreeAndNil(FThread);
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TCEFWorkScheduler.DestroyThread', e) then raise;
  end;
end;

{$IFDEF MSWINDOWS}
procedure TCEFWorkScheduler.WndProc(var aMessage: TMessage);
begin
  if (aMessage.Msg = CEF_PUMPHAVEWORK) then
    ScheduleWork(aMessage.lParam)
   else
    aMessage.Result := DefWindowProc(FCompHandle, aMessage.Msg, aMessage.WParam, aMessage.LParam);
end;

procedure TCEFWorkScheduler.DeallocateWindowHandle;
begin
  if (FCompHandle <> 0) then
    begin
      DeallocateHWnd(FCompHandle);
      FCompHandle := 0;
    end;
end;
{$ENDIF}

procedure TCEFWorkScheduler.DoMessageLoopWork;
begin
  if (GlobalCEFApp <> nil) then GlobalCEFApp.DoMessageLoopWork;
end;

procedure TCEFWorkScheduler.SetDefaultInterval(aValue : integer);
begin
  FDefaultInterval := aValue;
  if (FThread <> nil) then FThread.DefaultInterval := aValue;
end;

{$IFDEF MSWINDOWS}
{$WARN SYMBOL_PLATFORM OFF}
procedure TCEFWorkScheduler.SetPriority(aValue : TThreadPriority);
begin
  FPriority := aValue;
  if (FThread <> nil) then FThread.Priority := aValue;
end;
{$WARN SYMBOL_PLATFORM ON}
{$ENDIF}

procedure TCEFWorkScheduler.DepleteWork;
var
  i : cardinal;
begin
  i := FDepleteWorkCycles;

  while (i > 0) do
    begin
      DoMessageLoopWork;
      Sleep(FDepleteWorkDelay);
      dec(i);
    end;
end;

procedure TCEFWorkScheduler.ScheduleMessagePumpWork(const delay_ms : int64);
begin
  {$IFDEF MSWINDOWS}
  if not(FStopped) and (FCompHandle <> 0) then
    PostMessage(FCompHandle, CEF_PUMPHAVEWORK, 0, LPARAM(delay_ms));
  {$ELSE}
  if not(FStopped) then
    {$IFDEF FPC}
    Application.QueueAsyncCall(@ScheduleWorkAsync, integer(delay_ms));
    {$ELSE}
    TThread.Queue(nil, procedure
                       begin
                         ScheduleWork(delay_ms);
                       end);
    {$ENDIF}
  {$ENDIF}
end;

{$IFNDEF MSWINDOWS}{$IFDEF FPC}
procedure TCEFWorkScheduler.ScheduleWorkAsync(Data: PtrInt);
begin
  ScheduleWork(integer(Data));
end;
{$ENDIF}{$ENDIF}

procedure TCEFWorkScheduler.StopScheduler;
begin
  FStopped := True;
  NextPulse(0);
  DepleteWork;
  {$IFDEF MSWINDOWS}
  DeallocateWindowHandle;
  {$ENDIF}
end;

procedure TCEFWorkScheduler.Thread_OnPulse(Sender: TObject);
begin
  if not(FStopped) then DoMessageLoopWork;
end;

procedure TCEFWorkScheduler.DoWork;
begin
  DoMessageLoopWork;
  NextPulse(FDefaultInterval);
end;

procedure TCEFWorkScheduler.ScheduleWork(const delay_ms : int64);
begin
  if not(FStopped) then
    begin
      if (delay_ms <= 0) then
        DoWork
       else
        NextPulse(delay_ms);
    end;
end;

procedure TCEFWorkScheduler.NextPulse(aInterval : integer);
begin
  if (FThread <> nil) then FThread.NextPulse(aInterval);
end;

{$IFDEF FPC}
procedure Register;
begin
  {$I res/tcefworkscheduler.lrs}
  RegisterComponents('Chromium', [TCEFWorkScheduler]);
end;
{$ENDIF}

end.
