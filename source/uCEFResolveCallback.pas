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

unit uCEFResolveCallback;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

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
      FEvents : Pointer;

      procedure OnResolveCompleted(result: TCefErrorCode; const resolvedIps: TStrings); override;

    public
      constructor Create(const aEvents : IChromiumEvents); reintroduce;
      destructor  Destroy; override;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFStringList;

procedure cef_resolve_callback_on_resolve_completed(self         : PCefResolveCallback;
                                                    result       : TCefErrorCode;
                                                    resolved_ips : TCefStringList); stdcall;
var
  TempSL     : TStringList;
  TempCefSL  : ICefStringList;
  TempObject : TObject;
begin
  TempSL := nil;

  try
    try
      TempObject := CefGetObject(self);

      if (TempObject <> nil) and (TempObject is TCefResolveCallbackOwn) then
        begin
          TempSL    := TStringList.Create;
          TempCefSL := TCefStringListRef.Create(resolved_ips);
          TempCefSL.CopyToStrings(TempSL);

          TCefResolveCallbackOwn(TempObject).OnResolveCompleted(result, TempSL);
        end;
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
  inherited CreateData(SizeOf(TCefResolveCallback));

  with PCefResolveCallback(FData)^ do
    on_resolve_completed := {$IFDEF FPC}@{$ENDIF}cef_resolve_callback_on_resolve_completed;
end;

// TCefCustomResolveCallback

constructor TCefCustomResolveCallback.Create(const aEvents : IChromiumEvents);
begin
  inherited Create;

  FEvents := Pointer(aEvents);
end;

destructor TCefCustomResolveCallback.Destroy;
begin
  FEvents := nil;

  inherited Destroy;
end;

procedure TCefCustomResolveCallback.OnResolveCompleted(result: TCefErrorCode; const resolvedIps: TStrings);
begin
  try
    try
      if (FEvents <> nil) then IChromiumEvents(FEvents).doResolvedHostAvailable(result, resolvedIps);
    except
      on e : exception do
        if CustomExceptionHandler('TCefCustomResolveCallback.OnResolveCompleted', e) then raise;
    end;
  finally
    FEvents := nil;
  end;
end;

end.
