unit uCEFv8StackFrame;

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
  TCefV8StackFrameRef = class(TCefBaseRefCountedRef, ICefV8StackFrame)
  protected
    function IsValid: Boolean;
    function GetScriptName: ustring;
    function GetScriptNameOrSourceUrl: ustring;
    function GetFunctionName: ustring;
    function GetLineNumber: Integer;
    function GetColumn: Integer;
    function IsEval: Boolean;
    function IsConstructor: Boolean;
  public
    class function UnWrap(data: Pointer): ICefV8StackFrame;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;

function TCefV8StackFrameRef.GetColumn: Integer;
begin
  Result := PCefV8StackFrame(FData)^.get_column(PCefV8StackFrame(FData));
end;

function TCefV8StackFrameRef.GetFunctionName: ustring;
begin
  Result := CefStringFreeAndGet(PCefV8StackFrame(FData)^.get_function_name(PCefV8StackFrame(FData)));
end;

function TCefV8StackFrameRef.GetLineNumber: Integer;
begin
  Result := PCefV8StackFrame(FData)^.get_line_number(PCefV8StackFrame(FData));
end;

function TCefV8StackFrameRef.GetScriptName: ustring;
begin
  Result := CefStringFreeAndGet(PCefV8StackFrame(FData)^.get_script_name(PCefV8StackFrame(FData)));
end;

function TCefV8StackFrameRef.GetScriptNameOrSourceUrl: ustring;
begin
  Result := CefStringFreeAndGet(PCefV8StackFrame(FData)^.get_script_name_or_source_url(PCefV8StackFrame(FData)));
end;

function TCefV8StackFrameRef.IsConstructor: Boolean;
begin
  Result := PCefV8StackFrame(FData)^.is_constructor(PCefV8StackFrame(FData)) <> 0;
end;

function TCefV8StackFrameRef.IsEval: Boolean;
begin
  Result := PCefV8StackFrame(FData)^.is_eval(PCefV8StackFrame(FData)) <> 0;
end;

function TCefV8StackFrameRef.IsValid: Boolean;
begin
  Result := PCefV8StackFrame(FData)^.is_valid(PCefV8StackFrame(FData)) <> 0;
end;

class function TCefV8StackFrameRef.UnWrap(data: Pointer): ICefV8StackFrame;
begin
  if (data <> nil) then
    Result := Create(data) as ICefV8StackFrame
   else
    Result := nil;
end;

end.
