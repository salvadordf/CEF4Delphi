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

unit uCEFFindHandler;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFTypes, uCEFInterfaces;

type
  TCefFindHandlerOwn = class(TCefBaseRefCountedOwn, ICefFindHandler)
    protected
      procedure OnFindResult(const browser: ICefBrowser; identifier, count: Integer; const selectionRect: PCefRect; activeMatchOrdinal: Integer; finalUpdate: Boolean); virtual; abstract;

      procedure RemoveReferences; virtual;

    public
      constructor Create; virtual;
  end;

  TCustomFindHandler = class(TCefFindHandlerOwn)
    protected
      FEvents : Pointer;

      procedure OnFindResult(const browser: ICefBrowser; identifier, count: Integer; const selectionRect: PCefRect; activeMatchOrdinal: Integer; finalUpdate: Boolean); override;

      procedure RemoveReferences; override;

    public
      constructor Create(const events : IChromiumEvents); reintroduce; virtual;
      destructor  Destroy; override;
  end;

implementation

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBrowser;

procedure cef_find_handler_on_find_result(      self                 : PCefFindHandler;
                                                browser              : PCefBrowser;
                                                identifier           : Integer;
                                                count                : Integer;
                                          const selection_rect       : PCefRect;
                                                active_match_ordinal : integer;
                                                final_update         : Integer); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefFindHandlerOwn) then
    TCefFindHandlerOwn(TempObject).OnFindResult(TCefBrowserRef.UnWrap(browser),
                                                identifier,
                                                count,
                                                selection_rect,
                                                active_match_ordinal,
                                                final_update <> 0);
end;

constructor TCefFindHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefFindHandler));

  PCefFindHandler(FData)^.on_find_result := {$IFDEF FPC}@{$ENDIF}cef_find_handler_on_find_result;
end;

procedure TCefFindHandlerOwn.RemoveReferences;
begin
  //
end;

// TCustomFindHandler

constructor TCustomFindHandler.Create(const events : IChromiumEvents);
begin
  inherited Create;

  FEvents := Pointer(events);
end;

destructor TCustomFindHandler.Destroy;
begin
  RemoveReferences;

  inherited Destroy;
end;

procedure TCustomFindHandler.RemoveReferences;
begin
  FEvents := nil;
end;

procedure TCustomFindHandler.OnFindResult(const browser            : ICefBrowser;
                                                identifier         : Integer;
                                                count              : Integer;
                                          const selectionRect      : PCefRect;
                                                activeMatchOrdinal : Integer;
                                                finalUpdate        : Boolean);
begin
  if (FEvents <> nil) then IChromiumEvents(FEvents).doOnFindResult(browser, identifier, count, selectionRect, activeMatchOrdinal, finalUpdate);
end;

end.
