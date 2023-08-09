unit uCEFv8Context;

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
  TCefv8ContextRef = class(TCefBaseRefCountedRef, ICefv8Context)
    protected
      function GetTaskRunner: ICefTaskRunner;
      function IsValid: Boolean;
      function GetBrowser: ICefBrowser;
      function GetFrame: ICefFrame;
      function GetGlobal: ICefv8Value;
      function Enter: Boolean;
      function Exit: Boolean;
      function IsSame(const that: ICefv8Context): Boolean;
      function Eval(const code: ustring; const script_url: ustring; start_line: integer; var retval: ICefv8Value; var exception: ICefV8Exception): Boolean;

    public
      class function UnWrap(data: Pointer): ICefv8Context;
      class function Current: ICefv8Context;
      class function Entered: ICefv8Context;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBrowser, uCEFFrame, uCEFv8Value, uCEFTaskRunner, uCEFv8Exception;

class function TCefv8ContextRef.Current: ICefv8Context;
begin
  Result := UnWrap(cef_v8context_get_current_context());
end;

function TCefv8ContextRef.Enter: Boolean;
begin
  Result := PCefv8Context(FData)^.enter(PCefv8Context(FData)) <> 0;
end;

class function TCefv8ContextRef.Entered: ICefv8Context;
begin
  Result := UnWrap(cef_v8context_get_entered_context());
end;

function TCefv8ContextRef.Exit: Boolean;
begin
  Result := PCefv8Context(FData)^.exit(PCefv8Context(FData)) <> 0;
end;

function TCefv8ContextRef.GetBrowser: ICefBrowser;
begin
  Result := TCefBrowserRef.UnWrap(PCefv8Context(FData)^.get_browser(PCefv8Context(FData)));
end;

function TCefv8ContextRef.GetFrame: ICefFrame;
begin
  Result := TCefFrameRef.UnWrap(PCefv8Context(FData)^.get_frame(PCefv8Context(FData)))
end;

function TCefv8ContextRef.GetGlobal: ICefv8Value;
begin
  Result := TCefv8ValueRef.UnWrap(PCefv8Context(FData)^.get_global(PCefv8Context(FData)));
end;

function TCefv8ContextRef.GetTaskRunner: ICefTaskRunner;
begin
  Result := TCefTaskRunnerRef.UnWrap(PCefv8Context(FData)^.get_task_runner(PCefv8Context(FData)));
end;

function TCefv8ContextRef.IsSame(const that: ICefv8Context): Boolean;
begin
  Result := PCefv8Context(FData)^.is_same(PCefv8Context(FData), CefGetData(that)) <> 0;
end;

function TCefv8ContextRef.IsValid: Boolean;
begin
  Result := PCefv8Context(FData)^.is_valid(PCefv8Context(FData)) <> 0;
end;

function TCefv8ContextRef.Eval(const code       : ustring;
                               const script_url : ustring;
                                     start_line : integer;
                               var   retval     : ICefv8Value;
                               var   exception  : ICefV8Exception): Boolean;
var
  TempCode, TempScriptURL : TCefString;
  TempValue : PCefv8Value;
  TempException : PCefV8Exception;
begin
  TempCode      := CefString(code);
  TempScriptURL := CefString(script_url);
  TempValue     := nil;
  TempException := nil;

  Result := (PCefv8Context(FData)^.eval(PCefv8Context(FData), @TempCode, @TempScriptURL, start_line, TempValue, TempException) <> 0);

  retval    := TCefv8ValueRef.UnWrap(TempValue);
  exception := TCefV8ExceptionRef.UnWrap(TempException);
end;

class function TCefv8ContextRef.UnWrap(data: Pointer): ICefv8Context;
begin
  if (data <> nil) then
    Result := Create(data) as ICefv8Context
   else
    Result := nil;
end;

end.
