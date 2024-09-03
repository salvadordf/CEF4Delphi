unit uCEFTaskManager;

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
  /// <summary>
  /// Class that facilitates managing the browser-related tasks.
  /// </summary>
  /// <remarks>
  /// <para>The functions of this class may only be called on the CEF UI thread.</para>
  /// <para><see cref="uCEFTypes|TCefTaskManager">Implements TCefTaskManager</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_task_manager_capi.h">CEF source file: /include/capi/cef_task_manager_capi.h (cef_task_manager_t)</see></para>
  /// </remarks>
  TCefTaskManagerRef = class(TCefBaseRefCountedRef, ICefTaskManager)
    protected
      /// <summary>
      /// Returns the number of tasks currently tracked by the task manager. Returns
      /// 0 if the function was called from the incorrect thread.
      /// </summary>
      /// <remarks>
      /// <para>This function may only be called on the CEF UI thread.</para>
      /// </remarks>
      function GetTasksCount : NativeUInt;
      /// <summary>
      /// Gets the list of task IDs currently tracked by the task manager. Tasks
      /// that share the same process id will always be consecutive. The list will
      /// be sorted in a way that reflects the process tree: the browser process
      /// will be first, followed by the gpu process if it exists. Related processes
      /// (e.g., a subframe process and its parent) will be kept together if
      /// possible. Callers can expect this ordering to be stable when a process is
      /// added or removed. The task IDs are unique within the application lifespan.
      /// Returns false (0) if the function was called from the incorrect thread.
      /// </summary>
      /// <remarks>
      /// <para>This function may only be called on the CEF UI thread.</para>
      /// </remarks>
      function GetTaskIdsList(var task_ids: TCefCustomInt64Array): boolean;
      /// <summary>
      /// Gets information about the task with |task_id|. Returns true (1) if the
      /// information about the task was successfully retrieved and false (0) if the
      /// |task_id| is invalid or the function was called from the incorrect thread.
      /// </summary>
      /// <remarks>
      /// <para>This function may only be called on the CEF UI thread.</para>
      /// </remarks>
      function GetTaskInfo(const task_id: int64; var info: TCustomTaskInfo): boolean;
      /// <summary>
      /// Attempts to terminate a task with |task_id|. Returns false (0) if the
      /// |task_id| is invalid, the call is made from an incorrect thread, or if the
      /// task cannot be terminated.
      /// </summary>
      /// <remarks>
      /// <para>This function may only be called on the CEF UI thread.</para>
      /// </remarks>
      function KillTask(task_id: int64): boolean;
      /// <summary>
      /// Returns the task ID associated with the main task for |browser_id| (value
      /// from cef_browser_t::GetIdentifier). Returns -1 if |browser_id| is invalid,
      /// does not currently have an associated task, or the function was called
      /// from the incorrect thread.
      /// </summary>
      /// <remarks>
      /// <para>This function may only be called on the CEF UI thread.</para>
      /// </remarks>
      function GetTaskIdForBrowserId(browser_id: Integer): int64;

    public
      class function UnWrap(data: Pointer): ICefTaskManager;
      /// <summary>
      /// Returns the global task manager.
      /// </summary>
      /// <remarks>
      /// <para>This function may only be called on the CEF UI thread.</para>
      /// </remarks>
      class function New(): ICefTaskManager;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;

class function TCefTaskManagerRef.UnWrap(data: Pointer): ICefTaskManager;
begin
  if (data <> nil) then
    Result := Create(data) as ICefTaskManager
   else
    Result := nil;
end;

class function TCefTaskManagerRef.New(): ICefTaskManager;
begin
  Result := UnWrap(cef_task_manager_get());
end;

function TCefTaskManagerRef.GetTasksCount : NativeUInt;
begin
  Result := PCefTaskManager(FData)^.get_tasks_count(PCefTaskManager(FData));
end;

function TCefTaskManagerRef.GetTaskIdsList(var task_ids: TCefCustomInt64Array): boolean;
var
  TempCount, i : NativeUInt;
begin
  Result    := False;
  TempCount := GetTasksCount();

  if (TempCount = 0) then exit;

  SetLength(task_ids, TempCount);

  i := 0;
  while (i < TempCount) do
    begin
      task_ids[i] := 0;
      inc(i);
    end;

  Result := (PCefTaskManager(FData)^.get_task_ids_list(PCefTaskManager(FData), @TempCount, @task_ids[0]) <> 0);
end;

function TCefTaskManagerRef.GetTaskInfo(const task_id: int64; var info: TCustomTaskInfo): boolean;
var
  TempInfo : TCefTaskInfo;
begin
  Result := False;

  CefStringInitialize(@TempInfo.title);

  if (PCefTaskManager(FData)^.get_task_info(PCefTaskManager(FData), task_id, @TempInfo) <> 0) then
    begin
      info.id                     := TempInfo.id;
      info.type_                  := TempInfo.type_;
      info.is_killable            := TempInfo.is_killable <> 0;
      info.title                  := CefString(@TempInfo.title);
      info.cpu_usage              := TempInfo.cpu_usage;
      info.number_of_processors   := TempInfo.number_of_processors;
      info.memory                 := TempInfo.memory;
      info.gpu_memory             := TempInfo.gpu_memory;
      info.is_gpu_memory_inflated := TempInfo.is_gpu_memory_inflated <> 0;

      Result := True;
    end;
end;

function TCefTaskManagerRef.KillTask(task_id: int64): boolean;
begin
  Result := (PCefTaskManager(FData)^.kill_task(PCefTaskManager(FData), task_id) <> 0);
end;

function TCefTaskManagerRef.GetTaskIdForBrowserId(browser_id: Integer): int64;
begin
  Result := PCefTaskManager(FData)^.get_task_id_for_browser_id(PCefTaskManager(FData), browser_id);
end;

end.
