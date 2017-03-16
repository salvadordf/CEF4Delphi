// ************************************************************************
// ***************************** CEF4Delphi *******************************
// ************************************************************************
//
// CEF4Delphi is based on DCEF3 which uses CEF3 to embed a chromium-based
// browser in Delphi applications.
//
// The original license of DCEF3 still applies to CEF4Delphi.
//
// For more information about CEF4Delphi visit :
//         https://www.briskbard.com/index.php?lang=en&pageid=cef
//
//        Copyright © 2017 Salvador Díaz Fau. All rights reserved.
//
// ************************************************************************
// ************ vvvv Original license and comments below vvvv *************
// ************************************************************************
(*
 *                       Delphi Chromium Embedded 3
 *
 * Usage allowed under the restrictions of the Lesser GNU General Public License
 * or alternatively the restrictions of the Mozilla Public License 1.1
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * Unit owner : Henri Gourvest <hgourvest@gmail.com>
 * Web site   : http://www.progdigy.com
 * Repository : http://code.google.com/p/delphichromiumembedded/
 * Group      : http://groups.google.com/group/delphichromiumembedded
 *
 * Embarcadero Technologies, Inc is not permitted to use or redistribute
 * this source code without explicit permission.
 *
 *)

unit uCEFCommandLine;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes,
  {$ELSE}
  Classes,
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
      procedure GetArgv(args: TStrings);
      function  GetProgram: ustring;
      procedure SetProgram(const prog: ustring);
      function  HasSwitches: Boolean;
      function  HasSwitch(const name: ustring): Boolean;
      function  GetSwitchValue(const name: ustring): ustring;
      procedure GetSwitches(switches: TStrings);
      procedure AppendSwitch(const name: ustring);
      procedure AppendSwitchWithValue(const name, value: ustring);
      function  HasArguments: Boolean;
      procedure GetArguments(arguments: TStrings);
      procedure AppendArgument(const argument: ustring);
      procedure PrependWrapper(const wrapper: ustring);

    public
      class function UnWrap(data: Pointer): ICefCommandLine;
      class function New: ICefCommandLine;
      class function Global: ICefCommandLine;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;

procedure TCefCommandLineRef.AppendArgument(const argument: ustring);
var
  a: TCefString;
begin
  a := CefString(argument);
  PCefCommandLine(FData).append_argument(PCefCommandLine(FData), @a);
end;

procedure TCefCommandLineRef.AppendSwitch(const name: ustring);
var
  n: TCefString;
begin
  n := CefString(name);
  PCefCommandLine(FData).append_switch(PCefCommandLine(FData), @n);
end;

procedure TCefCommandLineRef.AppendSwitchWithValue(const name, value: ustring);
var
  n, v: TCefString;
begin
  n := CefString(name);
  v := CefString(value);
  PCefCommandLine(FData).append_switch_with_value(PCefCommandLine(FData), @n, @v);
end;

function TCefCommandLineRef.Copy: ICefCommandLine;
begin
  Result := UnWrap(PCefCommandLine(FData).copy(PCefCommandLine(FData)));
end;

procedure TCefCommandLineRef.GetArguments(arguments: TStrings);
var
  list: TCefStringList;
  i: Integer;
  str: TCefString;
begin
  list := cef_string_list_alloc;
  try
    PCefCommandLine(FData).get_arguments(PCefCommandLine(FData), list);
    for i := 0 to cef_string_list_size(list) - 1 do
    begin
      FillChar(str, SizeOf(str), 0);
      cef_string_list_value(list, i, @str);
      arguments.Add(CefStringClearAndGet(str));
    end;
  finally
    cef_string_list_free(list);
  end;
end;

procedure TCefCommandLineRef.GetArgv(args: TStrings);
var
  list: TCefStringList;
  i: Integer;
  str: TCefString;
begin
  list := cef_string_list_alloc;
  try
    PCefCommandLine(FData).get_argv(FData, list);
    for i := 0 to cef_string_list_size(list) - 1 do
    begin
      FillChar(str, SizeOf(str), 0);
      cef_string_list_value(list, i, @str);
      args.Add(CefStringClearAndGet(str));
    end;
  finally
    cef_string_list_free(list);
  end;
end;

function TCefCommandLineRef.GetCommandLineString: ustring;
begin
  Result := CefStringFreeAndGet(PCefCommandLine(FData).get_command_line_string(PCefCommandLine(FData)));
end;

function TCefCommandLineRef.GetProgram: ustring;
begin
  Result := CefStringFreeAndGet(PCefCommandLine(FData).get_program(PCefCommandLine(FData)));
end;

procedure TCefCommandLineRef.GetSwitches(switches: TStrings);
var
  list: TCefStringList;
  i: Integer;
  str: TCefString;
begin
  list := cef_string_list_alloc;
  try
    PCefCommandLine(FData).get_switches(PCefCommandLine(FData), list);
    for i := 0 to cef_string_list_size(list) - 1 do
    begin
      FillChar(str, SizeOf(str), 0);
      cef_string_list_value(list, i, @str);
      switches.Add(CefStringClearAndGet(str));
    end;
  finally
    cef_string_list_free(list);
  end;
end;

function TCefCommandLineRef.GetSwitchValue(const name: ustring): ustring;
var
  n: TCefString;
begin
  n := CefString(name);
  Result := CefStringFreeAndGet(PCefCommandLine(FData).get_switch_value(PCefCommandLine(FData), @n));
end;

class function TCefCommandLineRef.Global: ICefCommandLine;
begin
  Result := UnWrap(cef_command_line_get_global);
end;

function TCefCommandLineRef.HasArguments: Boolean;
begin
  Result := PCefCommandLine(FData).has_arguments(PCefCommandLine(FData)) <> 0;
end;

function TCefCommandLineRef.HasSwitch(const name: ustring): Boolean;
var
  n: TCefString;
begin
  n := CefString(name);
  Result := PCefCommandLine(FData).has_switch(PCefCommandLine(FData), @n) <> 0;
end;

function TCefCommandLineRef.HasSwitches: Boolean;
begin
  Result := PCefCommandLine(FData).has_switches(PCefCommandLine(FData)) <> 0;
end;

procedure TCefCommandLineRef.InitFromArgv(argc: Integer;
  const argv: PPAnsiChar);
begin
  PCefCommandLine(FData).init_from_argv(PCefCommandLine(FData), argc, argv);
end;

procedure TCefCommandLineRef.InitFromString(const commandLine: ustring);
var
  cl: TCefString;
begin
  cl := CefString(commandLine);
  PCefCommandLine(FData).init_from_string(PCefCommandLine(FData), @cl);
end;

function TCefCommandLineRef.IsReadOnly: Boolean;
begin
  Result := PCefCommandLine(FData).is_read_only(PCefCommandLine(FData)) <> 0;
end;

function TCefCommandLineRef.IsValid: Boolean;
begin
  Result := PCefCommandLine(FData).is_valid(PCefCommandLine(FData)) <> 0;
end;

class function TCefCommandLineRef.New: ICefCommandLine;
begin
  Result := UnWrap(cef_command_line_create);
end;

procedure TCefCommandLineRef.PrependWrapper(const wrapper: ustring);
var
  w: TCefString;
begin
  w := CefString(wrapper);
  PCefCommandLine(FData).prepend_wrapper(PCefCommandLine(FData), @w);
end;

procedure TCefCommandLineRef.Reset;
begin
  PCefCommandLine(FData).reset(PCefCommandLine(FData));
end;

procedure TCefCommandLineRef.SetProgram(const prog: ustring);
var
  p: TCefString;
begin
  p := CefString(prog);
  PCefCommandLine(FData).set_program(PCefCommandLine(FData), @p);
end;

class function TCefCommandLineRef.UnWrap(data: Pointer): ICefCommandLine;
begin
  if data <> nil then
    Result := Create(data) as ICefCommandLine else
    Result := nil;
end;

end.
