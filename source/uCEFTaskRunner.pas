unit uCEFTaskRunner;

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
  TCefTaskRunnerRef = class(TCefBaseRefCountedRef, ICefTaskRunner)
    protected
      function IsSame(const that: ICefTaskRunner): Boolean;
      function BelongsToCurrentThread: Boolean;
      function BelongsToThread(threadId: TCefThreadId): Boolean;
      function PostTask(const task: ICefTask): Boolean;
      function PostDelayedTask(const task: ICefTask; delayMs: Int64): Boolean;
    public
      /// <summary>
      /// Returns a ICefTaskRunner instance using a PCefTaskRunner data pointer.
      /// </summary>
      class function UnWrap(data: Pointer): ICefTaskRunner;
      /// <summary>
      /// Returns the task runner for the current thread. Only CEF threads will have
      /// task runners. An NULL reference will be returned if this function is called
      /// on an invalid thread.
      /// </summary>
      class function GetForCurrentThread: ICefTaskRunner;
      /// <summary>
      /// Returns the task runner for the specified CEF thread.
      /// </summary>
      class function GetForThread(threadId: TCefThreadId): ICefTaskRunner;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;


function TCefTaskRunnerRef.BelongsToCurrentThread: Boolean;
begin
  Result := PCefTaskRunner(FData)^.belongs_to_current_thread(PCefTaskRunner(FData)) <> 0;
end;

function TCefTaskRunnerRef.BelongsToThread(threadId: TCefThreadId): Boolean;
begin
  Result := PCefTaskRunner(FData)^.belongs_to_thread(PCefTaskRunner(FData), threadId) <> 0;
end;

class function TCefTaskRunnerRef.GetForCurrentThread: ICefTaskRunner;
begin
  Result := UnWrap(cef_task_runner_get_for_current_thread());
end;

class function TCefTaskRunnerRef.GetForThread(threadId: TCefThreadId): ICefTaskRunner;
begin
  Result := UnWrap(cef_task_runner_get_for_thread(threadId));
end;

function TCefTaskRunnerRef.IsSame(const that: ICefTaskRunner): Boolean;
begin
  Result := PCefTaskRunner(FData)^.is_same(PCefTaskRunner(FData), CefGetData(that)) <> 0;
end;

function TCefTaskRunnerRef.PostDelayedTask(const task: ICefTask; delayMs: Int64): Boolean;
begin
  Result := PCefTaskRunner(FData)^.post_delayed_task(PCefTaskRunner(FData), CefGetData(task), delayMs) <> 0;
end;

function TCefTaskRunnerRef.PostTask(const task: ICefTask): Boolean;
begin
  Result := PCefTaskRunner(FData)^.post_task(PCefTaskRunner(FData), CefGetData(task)) <> 0;
end;

class function TCefTaskRunnerRef.UnWrap(data: Pointer): ICefTaskRunner;
begin
  if (data <> nil) then
    Result := Create(data) as ICefTaskRunner
   else
    Result := nil;
end;

end.
