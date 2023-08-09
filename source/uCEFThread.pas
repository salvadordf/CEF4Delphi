unit uCEFThread;

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
  TCefThreadRef = class(TCefBaseRefCountedRef, ICefThread)
    protected
      function  GetTaskRunner : ICefTaskRunner;
      function  GetPlatformThreadID : TCefPlatformThreadId;
      procedure Stop;
      function  IsRunning : boolean;
    public
      class function UnWrap(data: Pointer): ICefThread;
      class function New(const display_name: ustring; priority: TCefThreadPriority; message_loop_type: TCefMessageLoopType; stoppable: integer; com_init_mode: TCefCOMInitMode): ICefThread;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFTaskRunner;

function TCefThreadRef.GetTaskRunner : ICefTaskRunner;
begin
  Result := TCefTaskRunnerRef.UnWrap(PCefThread(FData)^.get_task_runner(PCefThread(FData)));
end;

function TCefThreadRef.GetPlatformThreadID : TCefPlatformThreadId;
begin
  Result := PCefThread(FData)^.get_platform_thread_id(PCefThread(FData));
end;

procedure TCefThreadRef.Stop;
begin
  PCefThread(FData)^.stop(PCefThread(FData));
end;

function TCefThreadRef.IsRunning: Boolean;
begin
  Result := (PCefThread(FData)^.is_running(PCefThread(FData)) <> 0);
end;

class function TCefThreadRef.UnWrap(data: Pointer): ICefThread;
begin
  if (data <> nil) then
    Result := Create(data) as ICefThread
   else
    Result := nil;
end;

class function TCefThreadRef.New(const display_name: ustring; priority: TCefThreadPriority; message_loop_type: TCefMessageLoopType; stoppable: integer; com_init_mode: TCefCOMInitMode): ICefThread;
var
  TempString : TCefString;
begin
  TempString := CefString(display_name);
  Result     := UnWrap(cef_thread_create(@TempString, priority, message_loop_type, stoppable, com_init_mode));
end;

end.
