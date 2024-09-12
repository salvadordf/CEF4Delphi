unit uCEFTask;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefTaskOwn = class(TCefBaseRefCountedOwn, ICefTask)
    protected
      procedure Execute; virtual;

    public
      constructor Create; virtual;
  end;

  TCefTaskRef = class(TCefBaseRefCountedRef, ICefTask)
    protected
      procedure Execute; virtual;

    public
      class function UnWrap(data: Pointer): ICefTask;
  end;

  TCefFastTask = class(TCefTaskOwn)
    protected
      FMethod: TCefFastTaskProc;

      procedure Execute; override;

    public
      class procedure New(threadId: TCefThreadId; const method: TCefFastTaskProc);
      class procedure NewDelayed(threadId: TCefThreadId; Delay: Int64; const method: TCefFastTaskProc);
      constructor Create(const method: TCefFastTaskProc); reintroduce;
  end;

  /// <summary>
  /// Custom class used to execute CEF tasks with CEF4Delphi components.
  /// </summary>
  TCefManagedTask = class(TCefTaskOwn)
    protected
      FComponentID : integer;
      FEvents      : Pointer;

      function CanExecute: boolean; virtual;

    public
      constructor Create; override;
      destructor  Destroy; override;
  end;

  /// <summary>
  /// Custom class used to execute CEF tasks with a TChromiumCore component.
  /// </summary>
  TCefChromiumTask = class(TCefManagedTask)
    public
      constructor Create(const aEvents : IChromiumEvents); reintroduce;
  end;

  /// <summary>
  /// Custom class used to execute CEF tasks with a TCEFViewComponent component.
  /// </summary>
  TCefViewDelegateTask = class(TCefManagedTask)
    public
      constructor Create(const aEvents : ICefViewDelegateEvents); reintroduce;
  end;

  /// <summary>
  /// Custom class used to execute CEF tasks with a TCEFUrlRequestClientComponent component.
  /// </summary>
  TCefURLRequestClientTask = class(TCefManagedTask)
    public
      constructor Create(const aEvents : ICEFUrlRequestClientEvents); reintroduce;
  end;

  TCefUpdatePrefsTask = class(TCefChromiumTask)
    protected
      procedure Execute; override;
  end;

  TCefSavePrefsTask = class(TCefChromiumTask)
    protected
      procedure Execute; override;
  end;

  TCefURLRequestTask = class(TCefURLRequestClientTask)
    protected
      procedure Execute; override;
  end;

  TCefGenericTask = class(TCefChromiumTask)
    protected
      FTaskID : cardinal;

      procedure Execute; override;

    public
      constructor Create(const aEvents : IChromiumEvents; aTaskID : cardinal); reintroduce;
  end;

  TCefUpdateZoomStepTask = class(TCefChromiumTask)
    protected
      FInc : boolean;

      procedure Execute; override;

    public
      constructor Create(const aEvents : IChromiumEvents; aInc : boolean); reintroduce;
  end;

  TCefUpdateZoomPctTask = class(TCefChromiumTask)
    protected
      FInc : boolean;

      procedure Execute; override;

    public
      constructor Create(const aEvents : IChromiumEvents; aInc : boolean); reintroduce;
  end;

  TCefReadZoomTask = class(TCefChromiumTask)
    protected
      procedure Execute; override;
  end;

  TCefSetZoomLevelTask = class(TCefChromiumTask)
    protected
      FValue : double;

      procedure Execute; override;

    public
      constructor Create(const aEvents : IChromiumEvents; const aValue : double); reintroduce;
  end;

  TCefSetZoomPctTask = class(TCefChromiumTask)
    protected
      FValue : double;

      procedure Execute; override;

    public
      constructor Create(const aEvents : IChromiumEvents; const aValue : double); reintroduce;
  end;

  TCefSetZoomStepTask = class(TCefChromiumTask)
    protected
      FValue : byte;

      procedure Execute; override;

    public
      constructor Create(const aEvents : IChromiumEvents; aValue : byte); reintroduce;
  end;

  TCefCreateCustomViewTask = class(TCefViewDelegateTask)
    protected
      procedure Execute; override;
  end;

  TCefBrowserNavigationTask = class(TCefChromiumTask)
    protected
      FTask : TCefBrowserNavigation;

      procedure Execute; override;

    public
      constructor Create(const aEvents : IChromiumEvents; aTask : TCefBrowserNavigation); reintroduce;
  end;

  TCefSetAudioMutedTask = class(TCefChromiumTask)
    protected
      FValue  : boolean;

      procedure Execute; override;

    public
      constructor Create(const aEvents : IChromiumEvents; aValue : boolean); reintroduce;
  end;

  TCefToggleAudioMutedTask = class(TCefChromiumTask)
    protected
      procedure Execute; override;
  end;

  TCefEnableFocusTask = class(TCefChromiumTask)
    protected
      procedure Execute; override;
  end;

  TCefTryCloseBrowserTask = class(TCefChromiumTask)
    protected
      procedure Execute; override;
  end;

implementation

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  uCEFMiscFunctions, uCEFLibFunctions, uCEFCookieManager, uCEFUrlRequest, uCEFApplicationCore;


// TCefTaskOwn

procedure cef_task_execute(self: PCefTask); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefTaskOwn) then
    TCefTaskOwn(TempObject).Execute;
end;

constructor TCefTaskOwn.Create;
begin
  inherited CreateData(SizeOf(TCefTask));

  PCefTask(FData)^.execute := {$IFDEF FPC}@{$ENDIF}cef_task_execute;
end;

procedure TCefTaskOwn.Execute;
begin
  //
end;



// TCefTaskRef

procedure TCefTaskRef.Execute;
begin
  PCefTask(FData)^.execute(PCefTask(FData));
end;

class function TCefTaskRef.UnWrap(data: Pointer): ICefTask;
begin
  if (data <> nil) then
    Result := Create(data) as ICefTask
   else
    Result := nil;
end;



// TCefFastTask

constructor TCefFastTask.Create(const method: TCefFastTaskProc);
begin
  inherited Create;

  FMethod := method;
end;

procedure TCefFastTask.Execute;
begin
  FMethod();
end;

class procedure TCefFastTask.New(threadId: TCefThreadId; const method: TCefFastTaskProc);
begin
  CefPostTask(threadId, Create(method));
end;

class procedure TCefFastTask.NewDelayed(threadId: TCefThreadId; Delay: Int64; const method: TCefFastTaskProc);
begin
  CefPostDelayedTask(threadId, Create(method), Delay);
end;



// TCefManagedTask

constructor TCefManagedTask.Create;
begin
  inherited Create;

  FComponentID := 0;
  FEvents      := nil;
end;

destructor TCefManagedTask.Destroy;
begin
  FEvents := nil;

  inherited Destroy;
end;

function TCefManagedTask.CanExecute: boolean;
begin
  Result := (FEvents <> nil) and
            assigned(GlobalCEFApp) and
            GlobalCEFApp.ValidComponentID(FComponentID);
end;



// TCefChromiumTask

constructor TCefChromiumTask.Create(const aEvents : IChromiumEvents);
begin
  inherited Create;

  FComponentID := aEvents.ComponentID;
  FEvents      := Pointer(aEvents);
end;



// TCefViewDelegateTask

constructor TCefViewDelegateTask.Create(const aEvents : ICefViewDelegateEvents);
begin
  inherited Create;

  FComponentID := aEvents.ComponentID;
  FEvents      := Pointer(aEvents);
end;



// TCefURLRequestClientTask

constructor TCefURLRequestClientTask.Create(const aEvents : ICEFUrlRequestClientEvents);
begin
  inherited Create;

  FComponentID := aEvents.ComponentID;
  FEvents      := Pointer(aEvents);
end;



// TCefUpdatePrefsTask

procedure TCefUpdatePrefsTask.Execute;
begin
  try
    try
      if CanExecute then
        IChromiumEvents(FEvents).doUpdateOwnPreferences;
    except
      on e : exception do
        if CustomExceptionHandler('TCefUpdatePrefsTask.Execute', e) then raise;
    end;
  finally
    FEvents := nil;
  end;
end;


// TCefSavePrefsTask

procedure TCefSavePrefsTask.Execute;
begin
  try
    try
      if CanExecute then
        IChromiumEvents(FEvents).doSavePreferences;
    except
      on e : exception do
        if CustomExceptionHandler('TCefSavePrefsTask.Execute', e) then raise;
    end;
  finally
    FEvents := nil;
  end;
end;



// TCefURLRequestTask

procedure TCefURLRequestTask.Execute;
begin
  try
    try
      if CanExecute then
        ICEFUrlRequestClientEvents(FEvents).doOnCreateURLRequest;
    except
      on e : exception do
        if CustomExceptionHandler('TCefURLRequestTask.Execute', e) then raise;
    end;
  finally
    FEvents := nil;
  end;
end;



// TCefGenericTask

procedure TCefGenericTask.Execute;
begin
  try
    try
      if CanExecute then
        IChromiumEvents(FEvents).doOnExecuteTaskOnCefThread(FTaskID);
    except
      on e : exception do
        if CustomExceptionHandler('TCefGenericTask.Execute', e) then raise;
    end;
  finally
    FEvents := nil;
  end;
end;

constructor TCefGenericTask.Create(const aEvents : IChromiumEvents; aTaskID : cardinal);
begin
  inherited Create(aEvents);

  FTaskID := aTaskID;
end;



// TCefUpdateZoomStepTask

procedure TCefUpdateZoomStepTask.Execute;
begin
  try
    try
      if CanExecute then
        IChromiumEvents(FEvents).doUpdateZoomStep(FInc);
    except
      on e : exception do
        if CustomExceptionHandler('TCefUpdateZoomStepTask.Execute', e) then raise;
    end;
  finally
    FEvents := nil;
  end;
end;

constructor TCefUpdateZoomStepTask.Create(const aEvents : IChromiumEvents; aInc : boolean);
begin
  inherited Create(aEvents);

  FInc := aInc;
end;



// TCefUpdateZoomPctTask

procedure TCefUpdateZoomPctTask.Execute;
begin
  try
    try
      if CanExecute then
        IChromiumEvents(FEvents).doUpdateZoomPct(FInc);
    except
      on e : exception do
        if CustomExceptionHandler('TCefUpdateZoomPctTask.Execute', e) then raise;
    end;
  finally
    FEvents := nil;
  end;
end;

constructor TCefUpdateZoomPctTask.Create(const aEvents : IChromiumEvents; aInc : boolean);
begin
  inherited Create(aEvents);

  FInc := aInc;
end;



// TCefReadZoomTask

procedure TCefReadZoomTask.Execute;
begin
  try
    try
      if CanExecute then
        IChromiumEvents(FEvents).doReadZoom;
    except
      on e : exception do
        if CustomExceptionHandler('TCefReadZoomTask.Execute', e) then raise;
    end;
  finally
    FEvents := nil;
  end;
end;



// TCefSetZoomLevelTask

procedure TCefSetZoomLevelTask.Execute;
begin
  try
    try
      if CanExecute then
        IChromiumEvents(FEvents).doSetZoomLevel(FValue);
    except
      on e : exception do
        if CustomExceptionHandler('TCefSetZoomLevelTask.Execute', e) then raise;
    end;
  finally
    FEvents := nil;
  end;
end;

constructor TCefSetZoomLevelTask.Create(const aEvents : IChromiumEvents; const aValue : double);
begin
  inherited Create(aEvents);

  FValue := aValue;
end;



// TCefSetZoomPctTask

procedure TCefSetZoomPctTask.Execute;
begin
  try
    try
      if CanExecute then
        IChromiumEvents(FEvents).doSetZoomPct(FValue);
    except
      on e : exception do
        if CustomExceptionHandler('TCefSetZoomPctTask.Execute', e) then raise;
    end;
  finally
    FEvents := nil;
  end;
end;

constructor TCefSetZoomPctTask.Create(const aEvents : IChromiumEvents; const aValue : double);
begin
  inherited Create(aEvents);

  FValue := aValue;
end;



// TCefSetZoomStepTask

procedure TCefSetZoomStepTask.Execute;
begin
  try
    try
      if CanExecute then
        IChromiumEvents(FEvents).doSetZoomStep(FValue);
    except
      on e : exception do
        if CustomExceptionHandler('TCefSetZoomStepTask.Execute', e) then raise;
    end;
  finally
    FEvents := nil;
  end;
end;

constructor TCefSetZoomStepTask.Create(const aEvents : IChromiumEvents; aValue : byte);
begin
  inherited Create(aEvents);

  FValue := aValue;
end;



// TCefCreateCustomViewTask

procedure TCefCreateCustomViewTask.Execute;
begin
  try
    try
      if CanExecute then
        ICefViewDelegateEvents(FEvents).doCreateCustomView;
    except
      on e : exception do
        if CustomExceptionHandler('ICefViewDelegateEvents.Execute', e) then raise;
    end;
  finally
    FEvents := nil;
  end;
end;



// TCefBrowserNavigationTask

procedure TCefBrowserNavigationTask.Execute;
begin
  try
    try
      if CanExecute then
        IChromiumEvents(FEvents).doBrowserNavigation(FTask);
    except
      on e : exception do
        if CustomExceptionHandler('TCefBrowserNavigationTask.Execute', e) then raise;
    end;
  finally
    FEvents := nil;
  end;
end;

constructor TCefBrowserNavigationTask.Create(const aEvents : IChromiumEvents; aTask : TCefBrowserNavigation);
begin
  inherited Create(aEvents);

  FTask := aTask;
end;



// TCefSetAudioMutedTask

procedure TCefSetAudioMutedTask.Execute;
begin
  try
    try
      if CanExecute then
        IChromiumEvents(FEvents).doSetAudioMuted(FValue);
    except
      on e : exception do
        if CustomExceptionHandler('TCefSetAudioMutedTask.Execute', e) then raise;
    end;
  finally
    FEvents := nil;
  end;
end;

constructor TCefSetAudioMutedTask.Create(const aEvents : IChromiumEvents; aValue : boolean);
begin
  inherited Create(aEvents);

  FValue := aValue;
end;



// TCefToggleAudioMutedTask

procedure TCefToggleAudioMutedTask.Execute;
begin
  try
    try
      if CanExecute then
        IChromiumEvents(FEvents).doToggleAudioMuted;
    except
      on e : exception do
        if CustomExceptionHandler('TCefToggleAudioMutedTask.Execute', e) then raise;
    end;
  finally
    FEvents := nil;
  end;
end;



// TCefEnableFocusTask

procedure TCefEnableFocusTask.Execute;
begin
  try
    try
      if CanExecute then
        IChromiumEvents(FEvents).doEnableFocus;
    except
      on e : exception do
        if CustomExceptionHandler('TCefEnableFocusTask.Execute', e) then raise;
    end;
  finally
    FEvents := nil;
  end;
end;


// TCefTryCloseBrowserTask

procedure TCefTryCloseBrowserTask.Execute;
begin
  try
    try
      if CanExecute then
        IChromiumEvents(FEvents).doTryCloseBrowser;
    except
      on e : exception do
        if CustomExceptionHandler('TCefTryCloseBrowserTask.Execute', e) then raise;
    end;
  finally
    FEvents := nil;
  end;
end;

end.
