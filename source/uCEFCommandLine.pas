unit uCEFCommandLine;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes, System.SysUtils,
  {$ELSE}
  Classes, SysUtils,
  {$ENDIF}
  uCEFBaseRefCounted, uCEFTypes, uCEFInterfaces;

type
  TCefCommandLineRef = class(TCefBaseRefCountedRef, ICefCommandLine)
    protected
      function  IsValid: Boolean;
      function  IsReadOnly: Boolean;
      function  Copy: ICefCommandLine;
      procedure InitFromArgv(argc: Integer; const argv: PPAnsiChar);
      procedure InitFromString(const commandLine: ustring);
      procedure Reset;
      function  GetCommandLineString: ustring;
      procedure GetArgv(var args: TStrings);
      function  GetProgram: ustring;
      procedure SetProgram(const prog: ustring);
      function  HasSwitches: Boolean;
      function  HasSwitch(const name: ustring): Boolean;
      function  GetSwitchValue(const name: ustring): ustring;
      function  GetSwitches(var switches: TStrings): boolean; overload;
      function  GetSwitches(var SwitchKeys, SwitchValues: TStringList): boolean; overload;
      procedure AppendSwitch(const name: ustring);
      procedure AppendSwitchWithValue(const name, value: ustring);
      function  HasArguments: Boolean;
      procedure GetArguments(var arguments: TStrings);
      procedure AppendArgument(const argument: ustring);
      procedure PrependWrapper(const wrapper: ustring);

    public
      /// <summary>
      /// Returns a ICefCommandLine instance using a PCefCommandLine data pointer.
      /// </summary>
      class function UnWrap(data: Pointer): ICefCommandLine;
      /// <summary>
      /// Create a new ICefCommandLine instance.
      /// </summary>
      class function New: ICefCommandLine;
      /// <summary>
      /// Returns the singleton global ICefCommandLine object. The returned object
      /// will be read-only.
      /// </summary>
      class function Global: ICefCommandLine;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFStringMap, uCefStringList;

procedure TCefCommandLineRef.AppendArgument(const argument: ustring);
var
  TempArgument : TCefString;
begin
  TempArgument := CefString(argument);
  PCefCommandLine(FData)^.append_argument(PCefCommandLine(FData), @TempArgument);
end;

procedure TCefCommandLineRef.AppendSwitch(const name: ustring);
var
  TempName : TCefString;
begin
  TempName := CefString(name);
  PCefCommandLine(FData)^.append_switch(PCefCommandLine(FData), @TempName);
end;

procedure TCefCommandLineRef.AppendSwitchWithValue(const name, value: ustring);
var
  TempName, TempValue : TCefString;
begin
  TempName  := CefString(name);
  TempValue := CefString(value);
  PCefCommandLine(FData)^.append_switch_with_value(PCefCommandLine(FData), @TempName, @TempValue);
end;

function TCefCommandLineRef.Copy: ICefCommandLine;
begin
  Result := UnWrap(PCefCommandLine(FData)^.copy(PCefCommandLine(FData)));
end;

procedure TCefCommandLineRef.GetArguments(var arguments : TStrings);
var
  TempSL : ICefStringList;
begin
  if (arguments <> nil) then
    begin
      TempSL := TCefStringListOwn.Create;
      PCefCommandLine(FData)^.get_arguments(PCefCommandLine(FData), TempSL.Handle);
      TempSL.CopyToStrings(arguments);
    end;
end;

procedure TCefCommandLineRef.GetArgv(var args: TStrings);
var
  TempSL : ICefStringList;
begin
  if (args <> nil) then
    begin
      TempSL := TCefStringListOwn.Create;
      PCefCommandLine(FData)^.get_argv(PCefCommandLine(FData), TempSL.Handle);
      TempSL.CopyToStrings(args);
    end;
end;

function TCefCommandLineRef.GetCommandLineString: ustring;
begin
  Result := CefStringFreeAndGet(PCefCommandLine(FData)^.get_command_line_string(PCefCommandLine(FData)));
end;

function TCefCommandLineRef.GetProgram: ustring;
begin
  Result := CefStringFreeAndGet(PCefCommandLine(FData)^.get_program(PCefCommandLine(FData)));
end;

function TCefCommandLineRef.GetSwitches(var switches: TStrings): boolean;
var
  TempStrMap : ICefStringMap;
  i, j : NativeUInt;
  TempKey, TempValue : ustring;
begin
  Result     := False;
  TempStrMap := nil;

  try
    try
      if (switches <> nil) then
        begin
          TempStrMap := TCefStringMapOwn.Create;
          PCefCommandLine(FData)^.get_switches(PCefCommandLine(FData), TempStrMap.Handle);

          i := 0;
          j := TempStrMap.Size;

          while (i < j) do
            begin
              TempKey   := TempStrMap.Key[i];
              TempValue := TempStrMap.Value[i];

              if (length(TempKey) > 0) and (length(TempValue) > 0) then
                {$IFDEF VER140}
                switches.Add(TempKey + '=' + TempValue)  // Only for Delphi 6
                {$ELSE}
                switches.Add(TempKey + switches.NameValueSeparator + TempValue)
                {$ENDIF}
               else
                if (length(TempKey) > 0) then
                  switches.Add(TempKey)
                 else
                  if (length(TempValue) > 0) then
                    switches.Add(TempValue);

              inc(i);
            end;

          Result := (j > 0);
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TCefCommandLineRef.GetSwitches', e) then raise;
    end;
  finally
    TempStrMap := nil;
  end;
end;

function TCefCommandLineRef.GetSwitches(var SwitchKeys, SwitchValues: TStringList): boolean;
var
  TempStrMap : ICefStringMap;
  i, j : NativeUInt;
begin
  Result     := False;
  TempStrMap := nil;

  try
    try
      if (SwitchKeys <> nil) and (SwitchValues <> nil) then
        begin
          TempStrMap := TCefStringMapOwn.Create;
          PCefCommandLine(FData)^.get_switches(PCefCommandLine(FData), TempStrMap.Handle);

          i := 0;
          j := TempStrMap.Size;

          while (i < j) do
            begin
              SwitchKeys.Add(TempStrMap.Key[i]);
              SwitchValues.Add(TempStrMap.Value[i]);
              inc(i);
            end;

          Result := (j > 0);
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TCefCommandLineRef.GetSwitches', e) then raise;
    end;
  finally
    TempStrMap := nil;
  end;
end;

function TCefCommandLineRef.GetSwitchValue(const name: ustring): ustring;
var
  TempName : TCefString;
begin
  TempName := CefString(name);
  Result   := CefStringFreeAndGet(PCefCommandLine(FData)^.get_switch_value(PCefCommandLine(FData), @TempName));
end;

class function TCefCommandLineRef.Global: ICefCommandLine;
begin
  Result := UnWrap(cef_command_line_get_global());
end;

function TCefCommandLineRef.HasArguments: Boolean;
begin
  Result := PCefCommandLine(FData)^.has_arguments(PCefCommandLine(FData)) <> 0;
end;

function TCefCommandLineRef.HasSwitch(const name: ustring): Boolean;
var
  TempName : TCefString;
begin
  TempName := CefString(name);
  Result   := PCefCommandLine(FData)^.has_switch(PCefCommandLine(FData), @TempName) <> 0;
end;

function TCefCommandLineRef.HasSwitches: Boolean;
begin
  Result := PCefCommandLine(FData)^.has_switches(PCefCommandLine(FData)) <> 0;
end;

procedure TCefCommandLineRef.InitFromArgv(argc: Integer; const argv: PPAnsiChar);
begin
  PCefCommandLine(FData)^.init_from_argv(PCefCommandLine(FData), argc, argv);
end;

procedure TCefCommandLineRef.InitFromString(const commandLine: ustring);
var
  TempCommandLine : TCefString;
begin
  TempCommandLine := CefString(commandLine);
  PCefCommandLine(FData)^.init_from_string(PCefCommandLine(FData), @TempCommandLine);
end;

function TCefCommandLineRef.IsReadOnly: Boolean;
begin
  Result := PCefCommandLine(FData)^.is_read_only(PCefCommandLine(FData)) <> 0;
end;

function TCefCommandLineRef.IsValid: Boolean;
begin
  Result := PCefCommandLine(FData)^.is_valid(PCefCommandLine(FData)) <> 0;
end;

class function TCefCommandLineRef.New: ICefCommandLine;
begin
  Result := UnWrap(cef_command_line_create());
end;

procedure TCefCommandLineRef.PrependWrapper(const wrapper: ustring);
var
  TempWrapper : TCefString;
begin
  TempWrapper := CefString(wrapper);
  PCefCommandLine(FData)^.prepend_wrapper(PCefCommandLine(FData), @TempWrapper);
end;

procedure TCefCommandLineRef.Reset;
begin
  PCefCommandLine(FData)^.reset(PCefCommandLine(FData));
end;

procedure TCefCommandLineRef.SetProgram(const prog: ustring);
var
  TempProgram : TCefString;
begin
  TempProgram := CefString(prog);
  PCefCommandLine(FData)^.set_program(PCefCommandLine(FData), @TempProgram);
end;

class function TCefCommandLineRef.UnWrap(data: Pointer): ICefCommandLine;
begin
  if (data <> nil) then
    Result := Create(data) as ICefCommandLine
   else
    Result := nil;
end;

end.
