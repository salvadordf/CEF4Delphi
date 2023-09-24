unit uCEFWorkSchedulerQueueThread;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
    System.Classes, System.SyncObjs, System.SysUtils,
  {$ELSE}
    Classes, SyncObjs, SysUtils,
  {$ENDIF}
  uCEFConstants;

type
  TOnPulseEvent = procedure(Sender: TObject; aDelay: integer) of object;
  TValuesArray  = array of integer;

  TCEFWorkSchedulerQueueThread = class(TThread)
    protected
      FQueueCS         : TCriticalSection;
      FEvent           : TEvent;
      FWaiting         : boolean;
      FStop            : boolean;
      FReady           : boolean;
      FValues          : TValuesArray;
      FOnPulse         : TOnPulseEvent;

      function  GetAvailableValue : integer;
      function  GetHasPendingValue : boolean;
      function  Lock : boolean;
      procedure Unlock;
      function  CanContinue : boolean;
      procedure ReadAllPendingValues;
      procedure DoOnPulse;

      procedure Execute; override;

      property    AvailableValue  : integer         read GetAvailableValue;
      property    HasPendingValue : boolean         read GetHasPendingValue;

    public
      constructor Create;
      destructor  Destroy; override;
      procedure   AfterConstruction; override;
      procedure   StopThread;
      procedure   EnqueueValue(aValue : integer);

      property    Ready           : boolean         read FReady;
      property    OnPulse         : TOnPulseEvent   read FOnPulse         write FOnPulse;
  end;

implementation

constructor TCEFWorkSchedulerQueueThread.Create;
begin
  inherited Create(True);

  FReady          := False;
  FQueueCS        := nil;
  FWaiting        := False;
  FStop           := False;
  FEvent          := nil;
  FValues         := nil;
  FOnPulse        := nil;
  FreeOnTerminate := False;
end;

destructor TCEFWorkSchedulerQueueThread.Destroy;
begin
  if (FEvent   <> nil) then FreeAndNil(FEvent);
  if (FQueueCS <> nil) then FreeAndNil(FQueueCS);

  if (FValues <> nil) then
    begin
      Finalize(FValues);
      FValues := nil;
    end;

  inherited Destroy;
end;

procedure TCEFWorkSchedulerQueueThread.AfterConstruction;
begin
  inherited AfterConstruction;

  FEvent   := TEvent.Create(nil, False, False, '');
  FQueueCS := TCriticalSection.Create;
end;

function TCEFWorkSchedulerQueueThread.Lock : boolean;
begin
  if (FQueueCS <> nil) then
    begin
      FQueueCS.Acquire;
      Result := True;
    end
   else
    Result := False;
end;

procedure TCEFWorkSchedulerQueueThread.Unlock;
begin
  if (FQueueCS <> nil) then FQueueCS.Release;
end;

function TCEFWorkSchedulerQueueThread.GetAvailableValue : integer;
var
  TempLen : integer;
  TempNewValues : TValuesArray;
begin
  Result := 0;

  if Lock then
    try
      TempLen := length(FValues);

      if (TempLen > 0) then
        begin
          Result := FValues[0];

          if (TempLen = 1) then
            SetLength(FValues, 0)
           else
            begin
              SetLength(TempNewValues, TempLen - 1);
              TempNewValues := copy(FValues, 1, high(integer));
              SetLength(FValues, length(TempNewValues));
              FValues := Copy(TempNewValues);
              Finalize(TempNewValues);
            end;
        end;
    finally
      Unlock;
    end;
end;

function TCEFWorkSchedulerQueueThread.GetHasPendingValue : boolean;
begin
  Result := False;

  if Lock then
    try
      if not(Terminated) and not(FStop) then
        begin
          FWaiting := False;
          Result   := (Length(FValues) > 0);
        end;
    finally
      Unlock;
    end;
end;

procedure TCEFWorkSchedulerQueueThread.StopThread;
begin
  if Lock then
    try
      FStop := True;

      if FWaiting then
        begin
          FWaiting := False;
          FEvent.SetEvent;
        end;
    finally
      Unlock;
    end;
end;

procedure TCEFWorkSchedulerQueueThread.EnqueueValue(aValue : integer);
begin
  if Lock then
    try
      SetLength(FValues, Length(FValues) + 1);
      FValues[Length(FValues) - 1] := aValue;

      if FWaiting then
        begin
          FWaiting := False;
          FEvent.SetEvent;
        end;
    finally
      Unlock;
    end;
end;

procedure TCEFWorkSchedulerQueueThread.ReadAllPendingValues;
begin
  while HasPendingValue do
    if not(Terminated) then
      Synchronize({$IFDEF FPC}self, @{$ENDIF}DoOnPulse);
end;

procedure TCEFWorkSchedulerQueueThread.DoOnPulse;
begin
  if assigned(FOnPulse) then
    FOnPulse(self, AvailableValue);
end;

function TCEFWorkSchedulerQueueThread.CanContinue : boolean;
begin
  Result := False;

  if Lock then
    try
      if not(Terminated) and not(FStop) then
        begin
          Result   := True;
          FWaiting := True;
          FEvent.ResetEvent;
        end;
    finally
      Unlock;
    end;
end;

procedure TCEFWorkSchedulerQueueThread.Execute;
begin
  FReady := True;

  while CanContinue do
    begin
      FEvent.WaitFor(INFINITE);
      ReadAllPendingValues;
    end;
end;

end.
