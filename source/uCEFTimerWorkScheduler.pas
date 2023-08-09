unit uCEFTimerWorkScheduler;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{.$DEFINE USEEVENTPIPE}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
    System.Classes, System.SyncObjs, {$IFDEF MSWINDOWS}WinApi.Windows, WinApi.Messages,{$ENDIF}
    {$IFDEF FMX}
    FMX.Types, uCEFMacOSCustomCocoaTimer,
    {$ELSE}
    Vcl.ExtCtrls,
    {$ENDIF}
  {$ELSE}
    Classes, SyncObjs, {$IFDEF MSWINDOWS}Windows,{$ENDIF} ExtCtrls,
    {$IFDEF FPC}
    LMessages, Forms,
    {$ELSE}
    Messages,
    {$ENDIF}
  {$ENDIF}
  {$IFDEF USEEVENTPIPE}uCEFLinuxEventPipe,{$ENDIF} uCEFTypes, uCEFConstants,
  uCEFApplicationCore;

type
  TOnAllowEvent = procedure(Sender: TObject; var allow : boolean) of object;

  TCEFTimerWorkScheduler = class
    protected
      FTimer              : {$IFDEF MACOS}TCustomCocoaTimer{$ELSE}TTimer{$ENDIF};
      FDepleteWorkCycles  : cardinal;
      FDepleteWorkDelay   : cardinal;
      FStopped            : boolean;
      FIsActive           : boolean;
      FReentrancyDetected : boolean;
      FOnAllowDoWork      : TOnAllowEvent;
      {$IFDEF USEEVENTPIPE}
      FEventPipe          : TCEFLinuxEventPipe;
      FDelayedWorkTime    : TCefTime;
      {$ENDIF}
      {$IFDEF MSWINDOWS}
      FCompHandle         : HWND;
      {$ENDIF}

      function  GetIsTimerPending : boolean;

      procedure Timer_OnTimer(Sender: TObject);
      {$IFDEF USEEVENTPIPE}
      procedure FEventPipe_OnPrepare(Sender: TObject; var aTimeout: integer);
      procedure FEventPipe_OnCheck(Sender: TObject; var aMustDispatch: boolean);
      procedure FEventPipe_OnDispatch(Sender: TObject);
      {$ENDIF}

      procedure Initialize;
      procedure CreateTimer;
      procedure DestroyTimer;
      procedure KillTimer;
      procedure SetTimer(aInterval : integer);
      procedure DoWork;
      function  PerformMessageLoopWork : boolean;
      procedure DoMessageLoopWork;
      procedure OnScheduleWork(delay_ms : integer);
      procedure DepleteWork;
      {$IFDEF MSWINDOWS}
      procedure WndProc(var aMessage: TMessage);
      procedure AllocateWindowHandle;
      procedure DeallocateWindowHandle;
      {$ELSE}
      {$IFDEF FPC}
      procedure OnScheduleWorkAsync(Data: PtrInt);
      {$ENDIF}
      {$ENDIF}

    public
      constructor Create;
      destructor  Destroy; override;
      procedure   StopScheduler;
      procedure   ScheduleMessagePumpWork(const delay_ms : int64);

      property    DepleteWorkCycles  : cardinal         read FDepleteWorkCycles   write FDepleteWorkCycles;
      property    DepleteWorkDelay   : cardinal         read FDepleteWorkDelay    write FDepleteWorkDelay;
      property    IsTimerPending     : boolean          read GetIsTimerPending;
      property    OnAllowDoWork      : TOnAllowEvent    read FOnAllowDoWork       write FOnAllowDoWork;
  end;

var
  GlobalCEFTimerWorkScheduler : TCEFTimerWorkScheduler = nil;

procedure DestroyGlobalCEFTimerWorkScheduler;

implementation

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils, System.Math, {$IFDEF MACOS}System.RTTI, FMX.Forms, FMX.Platform,{$ENDIF}
  {$ELSE}
  SysUtils, Math,
  {$ENDIF}
  uCEFMiscFunctions;

procedure DestroyGlobalCEFTimerWorkScheduler;
begin
  if (GlobalCEFTimerWorkScheduler <> nil) then
    FreeAndNil(GlobalCEFTimerWorkScheduler);
end;

constructor TCEFTimerWorkScheduler.Create;
begin
  inherited Create;

  Initialize;

  {$IFDEF MSWINDOWS}
  AllocateWindowHandle;
  {$ENDIF}

  {$IFDEF USEEVENTPIPE}
  FEventPipe             := TCEFLinuxEventPipe.Create;
  FEventPipe.OnPrepare   := {$IFDEF FPC}@{$ENDIF}FEventPipe_OnPrepare;
  FEventPipe.OnCheck     := {$IFDEF FPC}@{$ENDIF}FEventPipe_OnCheck;
  FEventPipe.OnDispatch  := {$IFDEF FPC}@{$ENDIF}FEventPipe_OnDispatch;
  FEventPipe.InitializePipe;
  {$ENDIF}
end;

destructor TCEFTimerWorkScheduler.Destroy;
begin
  DestroyTimer;

  {$IFDEF MSWINDOWS}
  DeallocateWindowHandle;
  {$ENDIF}

  {$IFDEF USEEVENTPIPE}
  if assigned(FEventPipe) then
    FreeAndNil(FEventPipe);
  {$ENDIF}

  inherited Destroy;
end;

procedure TCEFTimerWorkScheduler.Initialize;
begin
  {$IFDEF MSWINDOWS}
  FCompHandle := 0;
  {$ENDIF}

  {$IFDEF USEEVENTPIPE}
  FEventPipe := nil;
  InitializeCefTime(FDelayedWorkTime);
  {$ENDIF}

  FOnAllowDoWork      := nil;
  FTimer              := nil;
  FStopped            := False;
  FIsActive           := False;
  FReentrancyDetected := False;
  FDepleteWorkCycles  := CEF_TIMER_DEPLETEWORK_CYCLES;
  FDepleteWorkDelay   := CEF_TIMER_DEPLETEWORK_DELAY;
end;

{$IFDEF MSWINDOWS}
procedure TCEFTimerWorkScheduler.WndProc(var aMessage: TMessage);
begin
  if (aMessage.Msg = CEF_PUMPHAVEWORK) then
    OnScheduleWork(aMessage.lParam)
   else
    aMessage.Result := DefWindowProc(FCompHandle, aMessage.Msg, aMessage.WParam, aMessage.LParam);
end;

procedure TCEFTimerWorkScheduler.AllocateWindowHandle;
begin
  if (FCompHandle = 0) and (GlobalCEFApp <> nil) and
     ((GlobalCEFApp.ProcessType = ptBrowser) or GlobalCEFApp.SingleProcess) then
    FCompHandle := AllocateHWnd({$IFDEF FPC}@{$ENDIF}WndProc);
end;

procedure TCEFTimerWorkScheduler.DeallocateWindowHandle;
begin
  if (FCompHandle <> 0) then
    begin
      DeallocateHWnd(FCompHandle);
      FCompHandle := 0;
    end;
end;
{$ENDIF}

{$IFDEF USEEVENTPIPE}
procedure TCEFTimerWorkScheduler.FEventPipe_OnPrepare(Sender: TObject; var aTimeout: integer);
begin
  aTimeout := GetTimeIntervalMilliseconds(FDelayedWorkTime);
end;

procedure TCEFTimerWorkScheduler.FEventPipe_OnCheck(Sender: TObject; var aMustDispatch: boolean);
var
  TempValue : integer;
begin
  if FEventPipe.HasData or FEventPipe.HasPendingData then
    begin
      TempValue := 0;
      FEventPipe.Read(TempValue);
      OnScheduleWork(TempValue);
    end;

  aMustDispatch := GetTimeIntervalMilliseconds(FDelayedWorkTime) = 0;
end;

procedure TCEFTimerWorkScheduler.FEventPipe_OnDispatch(Sender: TObject);
begin
  KillTimer;
  DoWork;
end;
{$ENDIF}

procedure TCEFTimerWorkScheduler.StopScheduler;
begin
  FStopped := True;
  KillTimer;
  DepleteWork;
end;

procedure TCEFTimerWorkScheduler.DepleteWork;
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

{$IFNDEF MSWINDOWS}{$IFDEF FPC}
procedure TCEFTimerWorkScheduler.OnScheduleWorkAsync(Data: PtrInt);
begin
  OnScheduleWork(integer(Data));
end;
{$ENDIF}{$ENDIF}

procedure TCEFTimerWorkScheduler.CreateTimer;
begin
  if (FTimer = nil) then
    begin
      {$IFDEF MACOS}
      FTimer         := TCustomCocoaTimer.Create;
      {$ELSE}
      FTimer         := TTimer.Create(nil);
      {$ENDIF}
      FTimer.OnTimer := {$IFDEF FPC}@{$ENDIF}Timer_OnTimer;
      FTimer.Enabled := False;
    end;
end;

procedure TCEFTimerWorkScheduler.DestroyTimer;
begin
  if (FTimer <> nil) then
    FreeAndNil(FTimer);
end;

procedure TCEFTimerWorkScheduler.KillTimer;
begin
  {$IFDEF USEEVENTPIPE}
  InitializeCefTime(FDelayedWorkTime);
  {$ELSE}
  if (FTimer <> nil) then
    FTimer.Enabled := False;
  {$ENDIF}
end;

procedure TCEFTimerWorkScheduler.SetTimer(aInterval : integer);
begin
  {$IFDEF USEEVENTPIPE}
  FDelayedWorkTime := DoubleToCefTime((DoubleTimeNow + aInterval) / 1000);
  {$ELSE}
  if (FTimer = nil) then
    CreateTimer;

  FTimer.Interval  := aInterval;
  FTimer.Enabled   := True;
  {$ENDIF}
end;

function TCEFTimerWorkScheduler.GetIsTimerPending : boolean;
begin
  {$IFDEF USEEVENTPIPE}
  Result := GetTimeIntervalMilliseconds(FDelayedWorkTime) > 0;
  {$ELSE}
  Result := (FTimer <> nil) and FTimer.Enabled;
  {$ENDIF}
end;

procedure TCEFTimerWorkScheduler.OnScheduleWork(delay_ms : integer);
begin
  if FStopped or
     ((delay_ms = high(integer)) and IsTimerPending) then
    exit;

  KillTimer;

  if (delay_ms <= 0) then
    DoWork
   else
    SetTimer(min(delay_ms, CEF_TIMER_MAXDELAY));
end;

procedure TCEFTimerWorkScheduler.Timer_OnTimer(Sender: TObject);
begin
  KillTimer;
  DoWork;
end;

procedure TCEFTimerWorkScheduler.DoWork;
begin
  if PerformMessageLoopWork then
    ScheduleMessagePumpWork(0)
   else
    if not(IsTimerPending) then
      ScheduleMessagePumpWork(high(integer));
end;

function TCEFTimerWorkScheduler.PerformMessageLoopWork : boolean;
begin
  Result := False;

  if FIsActive then
    begin
      FReentrancyDetected := True;
      exit;
    end;

  FReentrancyDetected := False;
  DoMessageLoopWork;
  Result := FReentrancyDetected;
end;

procedure TCEFTimerWorkScheduler.DoMessageLoopWork;
var
  TempAllow : boolean;
begin
  TempAllow := True;

  if assigned(FOnAllowDoWork) then
    FOnAllowDoWork(self, TempAllow);

  if TempAllow and (GlobalCEFApp <> nil) then
    try
      FIsActive := True;
      GlobalCEFApp.DoMessageLoopWork;
    finally
      FIsActive := False;
    end;
end;

procedure TCEFTimerWorkScheduler.ScheduleMessagePumpWork(const delay_ms : int64);
begin
  if FStopped then exit;

  {$IFDEF MSWINDOWS}
    if (FCompHandle <> 0) then
      PostMessage(FCompHandle, CEF_PUMPHAVEWORK, 0, LPARAM(delay_ms));
  {$ENDIF}

  {$IFDEF LINUX}
    {$IFDEF FPC}
      {$IFDEF USEEVENTPIPE}
      if assigned(FEventPipe) then
        FEventPipe.Write(integer(delay_ms));
      {$ELSE}
      Application.QueueAsyncCall(@OnScheduleWorkAsync, integer(delay_ms));
      {$ENDIF}
    {$ELSE}
    TThread.ForceQueue(nil, procedure
                            begin
                              OnScheduleWork(integer(delay_ms));
                            end);
    {$ENDIF}
  {$ENDIF}

  {$IFDEF MACOS}
    {$IFDEF FPC}
    Application.QueueAsyncCall(@OnScheduleWorkAsync, integer(delay_ms));
    {$ELSE}
    TThread.ForceQueue(nil, procedure
                            begin
                              OnScheduleWork(integer(delay_ms));
                            end);
    {$ENDIF}
  {$ENDIF}
end;

end.
