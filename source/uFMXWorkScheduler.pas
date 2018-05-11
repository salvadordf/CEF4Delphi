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
//        Copyright � 2018 Salvador D�az Fau. All rights reserved.
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

unit uFMXWorkScheduler;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  System.Classes, System.Types,
  FMX.Types, FMX.Controls,
  uCEFConstants, uCEFWorkSchedulerThread;

type
  TFMXWorkScheduler = class(TComponent)
    protected
      FThread             : TCEFWorkSchedulerThread;
      FDepleteWorkCycles  : cardinal;
      FDepleteWorkDelay   : cardinal;
      FDefaultInterval    : integer;
      FStopped            : boolean;
      {$IFDEF MSWINDOWS}
      {$WARN SYMBOL_PLATFORM OFF}
      FPriority           : TThreadPriority;
      {$WARN SYMBOL_PLATFORM ON}
      {$ENDIF}

      procedure CreateThread;
      procedure DestroyThread;
      procedure DepleteWork;
      procedure NextPulse(aInterval : integer);
      procedure DoWork;
      procedure DoMessageLoopWork;

      procedure SetDefaultInterval(aValue : integer);
      {$IFDEF MSWINDOWS}
      {$WARN SYMBOL_PLATFORM OFF}
      procedure SetPriority(aValue : TThreadPriority);
      {$WARN SYMBOL_PLATFORM ON}
      {$ENDIF}


      procedure Thread_OnPulse(Sender : TObject);

    public
      constructor Create(AOwner: TComponent); override;
      destructor  Destroy; override;
      procedure   AfterConstruction; override;
      procedure   ScheduleMessagePumpWork(const delay_ms : int64);
      procedure   StopScheduler;
      procedure   ScheduleWork(const delay_ms : int64);

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
  GlobalFMXWorkScheduler : TFMXWorkScheduler = nil;

implementation

uses
  {$IFDEF MSWINDOWS}WinApi.Windows,{$ENDIF} System.SysUtils, System.Math,
  FMX.Platform, FMX.Platform.Win, FMX.Forms,
  uCEFMiscFunctions, uCEFApplication;

constructor TFMXWorkScheduler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FThread             := nil;
  FStopped            := False;
  {$IFDEF MSWINDOWS}
  {$WARN SYMBOL_PLATFORM OFF}
  FPriority           := tpNormal;
  {$WARN SYMBOL_PLATFORM ON}
  {$ENDIF}
  FDefaultInterval    := CEF_TIMER_MAXDELAY;
  FDepleteWorkCycles  := CEF_TIMER_DEPLETEWORK_CYCLES;
  FDepleteWorkDelay   := CEF_TIMER_DEPLETEWORK_DELAY;
end;

destructor TFMXWorkScheduler.Destroy;
begin
  DestroyThread;

  inherited Destroy;
end;

procedure TFMXWorkScheduler.AfterConstruction;
begin
  inherited AfterConstruction;

  if not(csDesigning in ComponentState) then CreateThread;
end;

procedure TFMXWorkScheduler.CreateThread;
begin
  FThread                 := TCEFWorkSchedulerThread.Create;
  {$IFDEF MSWINDOWS}
  FThread.Priority        := FPriority;
  {$ENDIF}
  FThread.DefaultInterval := FDefaultInterval;
  FThread.OnPulse         := Thread_OnPulse;
  FThread.Start;
end;

procedure TFMXWorkScheduler.DestroyThread;
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
      if CustomExceptionHandler('TFMXWorkScheduler.DestroyThread', e) then raise;
  end;
end;

procedure TFMXWorkScheduler.DoMessageLoopWork;
begin
  if (GlobalCEFApp <> nil) then GlobalCEFApp.DoMessageLoopWork;
end;

procedure TFMXWorkScheduler.SetDefaultInterval(aValue : integer);
begin
  FDefaultInterval := aValue;
  if (FThread <> nil) then FThread.DefaultInterval := aValue;
end;

{$IFDEF MSWINDOWS}
{$WARN SYMBOL_PLATFORM OFF}
procedure TFMXWorkScheduler.SetPriority(aValue : TThreadPriority);
begin
  FPriority := aValue;
  if (FThread <> nil) then FThread.Priority := aValue;
end;
{$WARN SYMBOL_PLATFORM ON}
{$ENDIF}

procedure TFMXWorkScheduler.DepleteWork;
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

procedure TFMXWorkScheduler.ScheduleMessagePumpWork(const delay_ms : int64);
{$IFDEF MSWINDOWS}
var
  TempHandle : HWND;
{$ENDIF}
begin
  if not(FStopped) then
    begin
      {$IFDEF MSWINDOWS}
      {$IFDEF DELPHI17_UP}
      TempHandle := ApplicationHWND;
      {$ELSE}
      TempHandle := FmxHandleToHWND(Application.MainForm.Handle);
      {$ENDIF}
      if (TempHandle <> 0) then
        WinApi.Windows.PostMessage(TempHandle, CEF_PUMPHAVEWORK, 0, LPARAM(delay_ms));
      {$ENDIF}
    end;
end;

procedure TFMXWorkScheduler.StopScheduler;
begin
  FStopped := True;
  NextPulse(0);
  DepleteWork;
end;

procedure TFMXWorkScheduler.Thread_OnPulse(Sender: TObject);
begin
  if not(FStopped) then DoMessageLoopWork;
end;

procedure TFMXWorkScheduler.DoWork;
begin
  DoMessageLoopWork;
  NextPulse(FDefaultInterval);
end;

procedure TFMXWorkScheduler.ScheduleWork(const delay_ms : int64);
begin
  if not(FStopped) then
    begin
      if (delay_ms <= 0) then
        DoWork
       else
        NextPulse(delay_ms);
    end;
end;

procedure TFMXWorkScheduler.NextPulse(aInterval : integer);
begin
  if (FThread <> nil) then FThread.NextPulse(aInterval);
end;

end.
