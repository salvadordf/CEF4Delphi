unit uCEFWaitableEvent;

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
  TCefWaitableEventRef = class(TCefBaseRefCountedRef, ICefWaitableEvent)
    protected
      procedure Reset;
      procedure Signal;
      function  IsSignaled : boolean;
      procedure Wait;
      function  TimedWait(max_ms: int64): boolean;

    public
      class function UnWrap(data: Pointer): ICefWaitableEvent;
      class function New(automatic_reset, initially_signaled : boolean): ICefWaitableEvent;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;

procedure TCefWaitableEventRef.Reset;
begin
  PCefWaitableEvent(FData)^.reset(PCefWaitableEvent(FData));
end;

procedure TCefWaitableEventRef.Signal;
begin
  PCefWaitableEvent(FData)^.signal(PCefWaitableEvent(FData));
end;

function TCefWaitableEventRef.IsSignaled : boolean;
begin
  Result := (PCefWaitableEvent(FData)^.is_signaled(PCefWaitableEvent(FData)) <> 0);
end;

procedure TCefWaitableEventRef.Wait;
begin
  PCefWaitableEvent(FData)^.wait(PCefWaitableEvent(FData));
end;

function TCefWaitableEventRef.TimedWait(max_ms: int64): boolean;
begin
  Result := (PCefWaitableEvent(FData)^.timed_wait(PCefWaitableEvent(FData), max_ms) <> 0);
end;

class function TCefWaitableEventRef.UnWrap(data: Pointer): ICefWaitableEvent;
begin
  if (data <> nil) then
    Result := Create(data) as ICefWaitableEvent
   else
    Result := nil;
end;

class function TCefWaitableEventRef.New(automatic_reset, initially_signaled : boolean): ICefWaitableEvent;
begin
  Result := UnWrap(cef_waitable_event_create(Ord(automatic_reset), Ord(initially_signaled)));
end;

end.
