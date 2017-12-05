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

unit uCEFWorkScheduler;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  WinApi.Windows, WinApi.Messages, System.Classes, Vcl.Controls, Vcl.Graphics, Vcl.Forms,
  {$ELSE}
  Windows, Messages, Classes, Controls, Graphics, Forms,
  {$ENDIF}
  uCEFTypes, uCEFInterfaces, uCEFLibFunctions, uCEFMiscFunctions, uCEFConstants;

const
  TIMER_NIDEVENT           = 1;
  TIMER_DEPLETEWORK_CYCLES = 10;
  TIMER_DEPLETEWORK_DELAY  = 50;

type
  TCEFWorkScheduler = class(TComponent)
    protected
      FCompHandle         : HWND;
      FDepleteWorkCycles  : cardinal;
      FDepleteWorkDelay   : cardinal;
      FTimerPending       : boolean;
      FIsActive           : boolean;
      FReentrancyDetected : boolean;
      FStopped            : boolean;

      procedure WndProc(var aMessage: TMessage);
      function  SendCompMessage(aMsg, wParam : cardinal; lParam : integer) : boolean;
      procedure CreateTimer(const delay_ms : int64);
      procedure TimerTimeout;
      procedure DoWork;
      procedure ScheduleWork(const delay_ms : int64);
      procedure DoMessageLoopWork;
      function  PerformMessageLoopWork : boolean;
      procedure DestroyTimer;
      procedure DeallocateWindowHandle;
      procedure DepleteWork;

    public
      constructor Create(AOwner: TComponent); override;
      destructor  Destroy; override;
      procedure   AfterConstruction; override;
      procedure   ScheduleMessagePumpWork(const delay_ms : int64);
      procedure   StopScheduler;

      property    IsTimerPending : boolean   read FTimerPending;

    published
      property    DepleteWorkCycles  : cardinal    read FDepleteWorkCycles   write FDepleteWorkCycles  default  TIMER_DEPLETEWORK_CYCLES;
      property    DepleteWorkDelay   : cardinal    read FDepleteWorkDelay    write FDepleteWorkDelay   default  TIMER_DEPLETEWORK_DELAY;
  end;

implementation

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils, System.Math,
  {$ELSE}
  SysUtils, Math,
  {$ENDIF}
  uCEFApplication;


constructor TCEFWorkScheduler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCompHandle         := 0;
  FTimerPending       := False;
  FIsActive           := False;
  FReentrancyDetected := False;
  FStopped            := False;
  FDepleteWorkCycles  := TIMER_DEPLETEWORK_CYCLES;
  FDepleteWorkDelay   := TIMER_DEPLETEWORK_DELAY;
end;

destructor TCEFWorkScheduler.Destroy;
begin
  DestroyTimer;
  DeallocateWindowHandle;

  inherited Destroy;
end;

procedure TCEFWorkScheduler.AfterConstruction;
begin
  inherited AfterConstruction;

  if not(csDesigning in ComponentState) then
    FCompHandle := AllocateHWnd(WndProc);
end;

procedure TCEFWorkScheduler.WndProc(var aMessage: TMessage);
begin
  case aMessage.Msg of
    WM_TIMER          : TimerTimeout;
    CEF_PUMPHAVEWORK  : ScheduleWork(aMessage.lParam);
    else aMessage.Result := DefWindowProc(FCompHandle, aMessage.Msg, aMessage.WParam, aMessage.LParam);
  end;
end;

function TCEFWorkScheduler.SendCompMessage(aMsg, wParam : cardinal; lParam : integer) : boolean;
begin
  Result := not(FStopped) and (FCompHandle <> 0) and PostMessage(FCompHandle, aMsg, wParam, lParam);
end;

procedure TCEFWorkScheduler.CreateTimer(const delay_ms : int64);
begin
  if not(FTimerPending) and
     not(FStopped) and
     (delay_ms > 0) and
     (SetTimer(FCompHandle, TIMER_NIDEVENT, cardinal(delay_ms), nil) <> 0) then
    FTimerPending := True;
end;

procedure TCEFWorkScheduler.DestroyTimer;
begin
  if FTimerPending and KillTimer(FCompHandle, TIMER_NIDEVENT) then FTimerPending := False;
end;

procedure TCEFWorkScheduler.DeallocateWindowHandle;
begin
  if (FCompHandle <> 0) then
    begin
      DeallocateHWnd(FCompHandle);
      FCompHandle := 0;
    end;
end;

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
  SendCompMessage(CEF_PUMPHAVEWORK, 0, LPARAM(delay_ms));
end;

procedure TCEFWorkScheduler.StopScheduler;
begin
  FStopped := True;
  DestroyTimer;
  DepleteWork;
  DeallocateWindowHandle;
end;

procedure TCEFWorkScheduler.TimerTimeout;
begin
  if not(FStopped) then
    begin
      DestroyTimer;
      DoWork;
    end;
end;

procedure TCEFWorkScheduler.DoWork;
var
  TempWasReentrant : boolean;
begin
  TempWasReentrant := PerformMessageLoopWork;

  if TempWasReentrant then
    ScheduleMessagePumpWork(0)
   else
    if not(IsTimerPending) then
      ScheduleMessagePumpWork(CEF_TIMER_DELAY_PLACEHOLDER);
end;

procedure TCEFWorkScheduler.ScheduleWork(const delay_ms : int64);
begin
  if FStopped or
     ((delay_ms = CEF_TIMER_DELAY_PLACEHOLDER) and IsTimerPending) then
    exit;

  DestroyTimer;

  if (delay_ms <= 0) then
    DoWork
   else
    if (delay_ms > CEF_TIMER_MAXDELAY) then
      CreateTimer(CEF_TIMER_MAXDELAY)
     else
      CreateTimer(delay_ms);
end;

procedure TCEFWorkScheduler.DoMessageLoopWork;
begin
  if (GlobalCEFApp <> nil) then GlobalCEFApp.DoMessageLoopWork;
end;

function TCEFWorkScheduler.PerformMessageLoopWork : boolean;
begin
  Result := False;

  if FIsActive then
    begin
      FReentrancyDetected := True;
      exit;
    end;

  FReentrancyDetected := False;

  FIsActive := True;
  DoMessageLoopWork;
  FIsActive := False;

  Result := FReentrancyDetected;
end;

end.
