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

unit uCEFv8StackTrace;

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
