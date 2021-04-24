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

unit uCEFProcessMessage;

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
  TCefProcessMessageRef = class(TCefBaseRefCountedRef, ICefProcessMessage)
    protected
      function IsValid: Boolean;
      function IsReadOnly: Boolean;
      function Copy: ICefProcessMessage;
      function GetName: ustring;
      function GetArgumentList: ICefListValue;

    public
      class function UnWrap(data: Pointer): ICefProcessMessage;
      class function New(const name: ustring): ICefProcessMessage;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFListValue;

function TCefProcessMessageRef.Copy: ICefProcessMessage;
begin
  Result := UnWrap(PCefProcessMessage(FData)^.copy(PCefProcessMessage(FData)));
end;

function TCefProcessMessageRef.GetArgumentList: ICefListValue;
begin
  Result := TCefListValueRef.UnWrap(PCefProcessMessage(FData)^.get_argument_list(PCefProcessMessage(FData)));
end;

function TCefProcessMessageRef.GetName: ustring;
begin
  Result := CefStringFreeAndGet(PCefProcessMessage(FData)^.get_name(PCefProcessMessage(FData)));
end;

function TCefProcessMessageRef.IsReadOnly: Boolean;
begin
  Result := PCefProcessMessage(FData)^.is_read_only(PCefProcessMessage(FData)) <> 0;
end;

function TCefProcessMessageRef.IsValid: Boolean;
begin
  Result := PCefProcessMessage(FData)^.is_valid(PCefProcessMessage(FData)) <> 0;
end;

class function TCefProcessMessageRef.New(const name: ustring): ICefProcessMessage;
var
  TempString : TCefString;
begin
  TempString := CefString(name);
  Result     := UnWrap(cef_process_message_create(@TempString));
end;

class function TCefProcessMessageRef.UnWrap(data: Pointer): ICefProcessMessage;
begin
  if (data <> nil) then
    Result := Create(data) as ICefProcessMessage
   else
    Result := nil;
end;

end.
