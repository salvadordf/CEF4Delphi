// ************************************************************************
// ***************************** CEF4Delphi *******************************
// ************************************************************************
//
// CEF4Delphi is based on DCEF3 which uses CEF to embed a chromium-based
// browser in Delphi applications.
//
// The original license of DCEF3 still applies to CEF4Delphi.
//
// For more information about CEF4Delphi visit :
//         https://www.briskbard.com/index.php?lang=en&pageid=cef
//
//        Copyright © 2021 Salvador Diaz Fau. All rights reserved.
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

unit uCEFv8StackFrame;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

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
