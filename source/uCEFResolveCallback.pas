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
  System.Classes, System.SysUtils,
  {$ELSE}
  Classes, SysUtils,
  {$ENDIF}
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefResolveCallbackOwn = class(TCefBaseRefCountedOwn, ICefResolveCallback)
    protected
      procedure OnResolveCompleted(result: TCefErrorCode; const resolvedIps: TStrings); virtual; abstract;
    public
      constructor Create; virtual;
  end;

  TCefCustomResolveCallback = class(TCefResolveCallbackOwn)
    protected
      FChromiumBrowser : TObject;
      procedure OnResolveCompleted(result: TCefErrorCode; const resolvedIps: TStrings); override;

    public
      constructor Create(const aChromiumBrowser : TObject); reintroduce;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFChromium;

procedure cef_resolve_callback_on_resolve_completed(self: PCefResolveCallback;
                                                    result: TCefErrorCode;
                                                    resolved_ips: TCefStringList); stdcall;
var
  TempSL : TStringList;
  i, j : Integer;
  str: TCefString;
begin
  TempSL := nil;

  try
    try
      TempSL := TStringList.Create;
      i      := 0;
      j      := cef_string_list_size(resolved_ips);

      while (i < j) do
        begin
          FillChar(str, SizeOf(str), 0);
          cef_string_list_value(resolved_ips, i, @str);
          TempSL.Add(CefStringClearAndGet(str));
          inc(i);
        end;

      TCefResolveCallbackOwn(CefGetObject(self)).OnResolveCompleted(result, TempSL);
    except
      on e : exception do
        if CustomExceptionHandler('cef_resolve_callback_on_resolve_completed', e) then raise;
    end;
  finally
    if (TempSL <> nil) then FreeAndNil(TempSL);
  end;
end;

// TCefResolveCallbackOwn

constructor TCefResolveCallbackOwn.Create;
begin
  CreateData(SizeOf(TCefResolveCallback));

  with PCefResolveCallback(FData)^ do
    on_resolve_completed := cef_resolve_callback_on_resolve_completed;
end;

// TCefCustomResolveCallback

constructor TCefCustomResolveCallback.Create(const aChromiumBrowser : TObject);
begin
  inherited Create;

  FChromiumBrowser := aChromiumBrowser;
end;

procedure TCefCustomResolveCallback.OnResolveCompleted(result: TCefErrorCode; const resolvedIps: TStrings);
begin
  if (FChromiumBrowser <> nil) and (FChromiumBrowser is TChromium) then
    TChromium(FChromiumBrowser).Internal_ResolvedHostAvailable(result, resolvedIps);
end;

end.
