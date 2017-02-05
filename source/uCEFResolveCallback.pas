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

unit uCEFResolveCallback;

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
  uCEFBase, uCEFInterfaces, uCEFTypes;

type
  TCefResolveCallbackOwn = class(TCefBaseOwn, ICefResolveCallback)
  protected
    procedure OnResolveCompleted(result: TCefErrorCode; resolvedIps: TStrings); virtual; abstract;
  public
    constructor Create; virtual;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;

procedure cef_resolve_callback_on_resolve_completed(self: PCefResolveCallback;
  result: TCefErrorCode; resolved_ips: TCefStringList); stdcall;
var
  list: TStringList;
  i: Integer;
  str: TCefString;
begin
  list := TStringList.Create;
  try
    for i := 0 to cef_string_list_size(resolved_ips) - 1 do
    begin
      FillChar(str, SizeOf(str), 0);
      cef_string_list_value(resolved_ips, i, @str);
      list.Add(CefStringClearAndGet(str));
    end;
    with TCefResolveCallbackOwn(CefGetObject(self)) do
      OnResolveCompleted(result, list);
  finally
    list.Free;
  end;
end;

// TCefResolveCallbackOwn

constructor TCefResolveCallbackOwn.Create;
begin
  CreateData(SizeOf(TCefResolveCallback), False);
  with PCefResolveCallback(FData)^ do
    on_resolve_completed := cef_resolve_callback_on_resolve_completed;
end;

end.
