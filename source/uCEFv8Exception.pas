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

unit uCEFv8Exception;

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
  TCefV8ExceptionRef = class(TCefBaseRefCountedRef, ICefV8Exception)
    protected
      function GetMessage: ustring;
      function GetSourceLine: ustring;
      function GetScriptResourceName: ustring;
      function GetLineNumber: Integer;
      function GetStartPosition: Integer;
      function GetEndPosition: Integer;
      function GetStartColumn: Integer;
      function GetEndColumn: Integer;

    public
      class function UnWrap(data: Pointer): ICefV8Exception;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;


function TCefV8ExceptionRef.GetEndColumn: Integer;
begin
  Result := PCefV8Exception(FData)^.get_end_column(PCefV8Exception(FData));
end;

function TCefV8ExceptionRef.GetEndPosition: Integer;
begin
  Result := PCefV8Exception(FData)^.get_end_position(PCefV8Exception(FData));
end;

function TCefV8ExceptionRef.GetLineNumber: Integer;
begin
  Result := PCefV8Exception(FData)^.get_line_number(PCefV8Exception(FData));
end;

function TCefV8ExceptionRef.GetMessage: ustring;
begin
  Result := CefStringFreeAndGet(PCefV8Exception(FData)^.get_message(PCefV8Exception(FData)));
end;

function TCefV8ExceptionRef.GetScriptResourceName: ustring;
begin
  Result := CefStringFreeAndGet(PCefV8Exception(FData)^.get_script_resource_name(PCefV8Exception(FData)));
end;

function TCefV8ExceptionRef.GetSourceLine: ustring;
begin
  Result := CefStringFreeAndGet(PCefV8Exception(FData)^.get_source_line(PCefV8Exception(FData)));
end;

function TCefV8ExceptionRef.GetStartColumn: Integer;
begin
  Result := PCefV8Exception(FData)^.get_start_column(PCefV8Exception(FData));
end;

function TCefV8ExceptionRef.GetStartPosition: Integer;
begin
  Result := PCefV8Exception(FData)^.get_start_position(PCefV8Exception(FData));
end;

class function TCefV8ExceptionRef.UnWrap(data: Pointer): ICefV8Exception;
begin
  if (data <> nil) then
    Result := Create(data) as ICefV8Exception
   else
    Result := nil;
end;

end.
