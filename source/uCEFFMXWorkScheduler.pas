unit uCEFFMXWorkScheduler;

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  System.Classes, System.Types,
  FMX.Types, FMX.Controls,
  uCEFConstants, uCEFWorkSchedulerQueueThread, uCEFWorkSchedulerThread;

type
  {$IFNDEF FPC}{$IFDEF DELPHI16_UP}[ComponentPlatformsAttribute(pfidWindows or pfidOSX or pfidLinux)]{$ENDIF}{$ENDIF}
  /// <summary>
  /// <para>Implementation of an external message pump for FMX.</para>
  /// <para>Read the GlobalCEFApp.OnScheduleMessagePumpWork documentation for all the details.</para>
  /// </summary>
  TFMXWorkScheduler = class(TComponent)
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
      FPriority           : TThreadPriority;
      {$WARN SYMBOL_PLATFORM ON}
      {$ENDIF}

      procedure CreateQueueThread;
      procedure DestroyQueueThread;
      procedure QueueThread_OnPulse(Sender : TObject; aDelay : integer);

      procedure DestroyThread;
      procedure DepleteWork;
      procedure NextPulse(aInterval : integer);
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
      /// Full constructor of TFMXWorkScheduler. This constructor also creates the internal threads.
      /// </summary>
      constructor Create(AOwner: TComponent); override;
      /// <summary>
      /// Partial constructor of TFMXWorkScheduler. This constructor doesn't create any threads.
      /// Call TFMXWorkScheduler.CreateThread when necessary.
      /// </summary>
      constructor CreateDelayed;
      /// <summary>
      /// TFMXWorkScheduler destructor.
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
      /// Schedule a GlobalCEFApp.DoMessageLoopWork call synchronously to perform a single
      /// iteration of CEF message loop processing.
      /// </summary>
      procedure   ScheduleWork(const delay_ms : int64);
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
  GlobalFMXWorkScheduler : TFMXWorkScheduler = nil;

procedure DestroyGlobalFMXWorkScheduler;

implementation

uses
  {$IFDEF MSWINDOWS}WinApi.Windows,{$ENDIF} System.SysUtils, System.Math,
  FMX.Platform, {$IFDEF MSWINDOWS}FMX.Platform.Win,{$ENDIF} FMX.Forms,
  uCEFMiscFunctions, uCEFApplicationCore;

procedure DestroyGlobalFMXWorkScheduler;
begin
  if (GlobalFMXWorkScheduler <> nil) then FreeAndNil(GlobalFMXWorkScheduler);
end;

constructor TFMXWorkScheduler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Initialize;

  if not(csDesigning in ComponentState) then CreateThread;
end;

constructor TFMXWorkScheduler.CreateDelayed;
begin
  inherited Create(nil);

  Initialize;
end;

destructor TFMXWorkScheduler.Destroy;
begin
  DestroyThread;
  DestroyQueueThread;

  inherited Destroy;
end;

procedure TFMXWorkScheduler.Initialize;
begin
  FUseQueueThread     := False;
  FThread             := nil;
  FQueueThread        := nil;
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

procedure TFMXWorkScheduler.CreateThread;
begin
  if (FThread <> nil) then exit;

  FThread                 := TCEFWorkSchedulerThread.Create;
  {$IFDEF MSWINDOWS}
  FThread.Priority        := FPriority;
  {$ENDIF}
  FThread.DefaultInterval := FDefaultInterval;
  FThread.OnPulse         := Thread_OnPulse;
  FThread.Start;

  if FUseQueueThread then
    CreateQueueThread;
end;

procedure TFMXWorkScheduler.CreateQueueThread;
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

procedure TFMXWorkScheduler.DestroyQueueThread;
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
      if CustomExceptionHandler('TFMXWorkScheduler.DestroyQueueThread', e) then raise;
  end;
end;

procedure TFMXWorkScheduler.QueueThread_OnPulse(Sender : TObject; aDelay : integer);
begin
  ScheduleWork(aDelay);
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
begin
  if FStopped then exit;

  if FUseQueueThread and (FQueueThread <> nil) and FQueueThread.Ready then
    FQueueThread.EnqueueValue(integer(delay_ms))
   else
    {$IFDEF DELPHI25_UP}
    TThread.ForceQueue(nil,
    {$ELSE}
    TThread.Queue(nil,
    {$ENDIF}
      procedure
      begin
        ScheduleWork(delay_ms);
      end);
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
