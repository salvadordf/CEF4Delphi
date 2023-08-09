unit uCEFv8StackTrace;

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
  TCefV8StackTraceRef = class(TCefBaseRefCountedRef, ICefV8StackTrace)
  protected
    function IsValid: Boolean;
    function GetFrameCount: Integer;
    function GetFrame(index: Integer): ICefV8StackFrame;
  public
    class function UnWrap(data: Pointer): ICefV8StackTrace;
    class function Current(frameLimit: Integer): ICefV8StackTrace;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFv8StackFrame;

class function TCefV8StackTraceRef.Current(frameLimit: Integer): ICefV8StackTrace;
begin
  Result := UnWrap(cef_v8stack_trace_get_current(frameLimit));
end;

function TCefV8StackTraceRef.GetFrame(index: Integer): ICefV8StackFrame;
begin
  Result := TCefV8StackFrameRef.UnWrap(PCefV8StackTrace(FData)^.get_frame(PCefV8StackTrace(FData), index));
end;

function TCefV8StackTraceRef.GetFrameCount: Integer;
begin
  Result := PCefV8StackTrace(FData)^.get_frame_count(PCefV8StackTrace(FData));
end;

function TCefV8StackTraceRef.IsValid: Boolean;
begin
  Result := PCefV8StackTrace(FData)^.is_valid(PCefV8StackTrace(FData)) <> 0;
end;

class function TCefV8StackTraceRef.UnWrap(data: Pointer): ICefV8StackTrace;
begin
  if (data <> nil) then
    Result := Create(data) as ICefV8StackTrace
   else
    Result := nil;
end;

end.
