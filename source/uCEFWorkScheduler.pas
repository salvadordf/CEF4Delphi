unit uCEFWorkScheduler;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

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
  uCEFConstants, uCEFWorkSchedulerQueueThread, uCEFWorkSchedulerThread;


type
  {$IFNDEF FPC}{$IFDEF DELPHI16_UP}[ComponentPlatformsAttribute(pfidWindows)]{$ENDIF}{$ENDIF}
  /// <summary>
  /// <para>Implementation of an external message pump for VCL and LCL.</para>
  /// <para>Read the GlobalCEFApp.OnScheduleMessagePumpWork documentation for all the details.</para>
  /// </summary>
  TCEFWorkScheduler = class(TComponent)
    protected
      FThread             : TCEFWorkSchedulerThread;
      FQueueThread        : TCEFWorkSchedulerQueueThread;
      FDepleteWorkCycles  : cardinal;
      FDepleteWorkDelay   : cardinal;
      FDefaultInterval    : integer;
      FStopped            : boolean;
      FUseQueueThread     : boolean;
      {$IFDEF MSWINDOWS}
      {$WARN SYMBOL_PLATFORM OFF}
      FCompHandle         : HWND;
      FPriority           : TThreadPriority;
      {$WARN SYMBOL_PLATFORM ON}
      {$ENDIF}

      procedure CreateQueueThread;
      procedure DestroyQueueThread;
      procedure QueueThread_OnPulse(Sender : TObject; aDelay : integer);

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
      /// <summary>
      /// Full constructor of TCEFWorkScheduler. This constructor also creates the internal threads.
      /// </summary>
      constructor Create(AOwner: TComponent); override;
      /// <summary>
      /// Partial constructor of TCEFWorkScheduler. This constructor doesn't create any threads.
      /// Call TCEFWorkScheduler.CreateThread when necessary.
      /// </summary>
      constructor CreateDelayed;
      /// <summary>
      /// TCEFWorkScheduler destructor.
      /// </summary>
      destructor  Destroy; override;
      /// <summary>
      /// Called from GlobalCEFApp.OnScheduleMessagePumpWork to schedule
      /// a GlobalCEFApp.DoMessageLoopWork call asynchronously to perform a single
      /// iteration of CEF message loop processing.
      /// </summary>
      /// <param name="delay_ms">Requested delay in milliseconds.</param>
      procedure   ScheduleMessagePumpWork(const delay_ms : int64);
      /// <summary>
      /// Stop the scheduler. This function must be called after the destruction of all the forms in the application.
      /// </summary>
      procedure   StopScheduler;
      /// <summary>
      /// Creates all the internal threads used by TCEFWorkScheduler.
      /// </summary>
      procedure   CreateThread;

    published
      {$IFDEF MSWINDOWS}
      {$WARN SYMBOL_PLATFORM OFF}
      /// <summary>
      /// Priority of TCEFWorkSchedulerThread in Windows.
      /// </summary>
      property    Priority           : TThreadPriority  read FPriority            write SetPriority         default  tpNormal;
      {$WARN SYMBOL_PLATFORM ON}
      {$ENDIF}
      /// <summary>
      /// Default interval in milliseconds to do the next GlobalCEFApp.DoMessageLoopWork call.
      /// </summary>
      property    DefaultInterval    : integer          read FDefaultInterval     write SetDefaultInterval  default  CEF_TIMER_MAXDELAY;
      /// <summary>
      /// Number of cycles used to deplete the remaining messages in the work loop.
      /// </summary>
      property    DepleteWorkCycles  : cardinal         read FDepleteWorkCycles   write FDepleteWorkCycles  default  CEF_TIMER_DEPLETEWORK_CYCLES;
      /// <summary>
      /// Delay in milliseconds between the cycles used to deplete the remaining messages in the work loop.
      /// </summary>
      property    DepleteWorkDelay   : cardinal         read FDepleteWorkDelay    write FDepleteWorkDelay   default  CEF_TIMER_DEPLETEWORK_DELAY;
      /// <summary>
      /// Use a custom queue thread instead of Windows messages or any other way to schedule the next pump work.
      /// </summary>
      property    UseQueueThread     : boolean          read FUseQueueThread      write FUseQueueThread     default  False;
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
  DestroyQueueThread;
  {$IFDEF MSWINDOWS}
  DeallocateWindowHandle;
  {$ENDIF}
  inherited Destroy;
end;

procedure TCEFWorkScheduler.Initialize;
begin
  FUseQueueThread     := False;
  FThread             := nil;
  FQueueThread        := nil;
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

  if FUseQueueThread then
    CreateQueueThread;
end;

procedure TCEFWorkScheduler.CreateQueueThread;
begin
  FQueueThread         := TCEFWorkSchedulerQueueThread.Create;
  FQueueThread.OnPulse := {$IFDEF FPC}@{$ENDIF}QueueThread_OnPulse;
  {$IFDEF DELPHI14_UP}
  FQueueThread.Start;
  {$ELSE}
  {$IFNDEF FPC}
  FQueueThread.Resume;
  {$ELSE}
  FQueueThread.Start;
  {$ENDIF}
  {$ENDIF}
end;

procedure TCEFWorkScheduler.DestroyQueueThread;
begin
  try
    if (FQueueThread <> nil) then
      begin
        FQueueThread.Terminate;
        FQueueThread.StopThread;
        FQueueThread.WaitFor;
        FreeAndNil(FQueueThread);
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TCEFWorkScheduler.DestroyQueueThread', e) then raise;
  end;
end;

procedure TCEFWorkScheduler.QueueThread_OnPulse(Sender : TObject; aDelay : integer);
begin
  ScheduleWork(aDelay);
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

{$WARN SYMBOL_PLATFORM OFF}
procedure TCEFWorkScheduler.SetPriority(aValue : TThreadPriority);
begin
  FPriority := aValue;
  if (FThread <> nil) then FThread.Priority := aValue;
end;
{$WARN SYMBOL_PLATFORM ON}
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
  if FStopped then exit;

  if FUseQueueThread and (FQueueThread <> nil) and FQueueThread.Ready then
    FQueueThread.EnqueueValue(integer(delay_ms))
   else
    begin
      {$IFDEF MSWINDOWS}
      if (FCompHandle <> 0) then
        PostMessage(FCompHandle, CEF_PUMPHAVEWORK, 0, LPARAM(delay_ms));
      {$ELSE}
      {$IFDEF FPC}
      Application.QueueAsyncCall(@ScheduleWorkAsync, integer(delay_ms));
      {$ELSE}
      TThread.ForceQueue(nil, procedure
                              begin
                                ScheduleWork(delay_ms);
                              end);
      {$ENDIF}
      {$ENDIF}
    end;
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
