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

unit uCEFFocusHandler;

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
  TCefFocusHandlerOwn = class(TCefBaseRefCountedOwn, ICefFocusHandler)
    protected
      procedure OnTakeFocus(const browser: ICefBrowser; next: Boolean); virtual;
      function  OnSetFocus(const browser: ICefBrowser; source: TCefFocusSource): Boolean; virtual;
      procedure OnGotFocus(const browser: ICefBrowser); virtual;

      procedure RemoveReferences; virtual;

    public
      constructor Create; virtual;
  end;

  TCustomFocusHandler = class(TCefFocusHandlerOwn)
    protected
      FEvents : Pointer;

      procedure OnTakeFocus(const browser: ICefBrowser; next: Boolean); override;
      function  OnSetFocus(const browser: ICefBrowser; source: TCefFocusSource): Boolean; override;
      procedure OnGotFocus(const browser: ICefBrowser); override;

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

procedure cef_focus_handler_on_take_focus(self    : PCefFocusHandler;
                                          browser : PCefBrowser;
                                          next    : Integer); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefFocusHandlerOwn) then
    TCefFocusHandlerOwn(TempObject).OnTakeFocus(TCefBrowserRef.UnWrap(browser),
                                                next <> 0);
end;

function cef_focus_handler_on_set_focus(self    : PCefFocusHandler;
                                        browser : PCefBrowser;
                                        source  : TCefFocusSource): Integer; stdcall;
var
  TempObject : TObject;
begin
  Result     := Ord(False);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefFocusHandlerOwn) then
    Result := Ord(TCefFocusHandlerOwn(TempObject).OnSetFocus(TCefBrowserRef.UnWrap(browser),
                                                             source))
end;

procedure cef_focus_handler_on_got_focus(self    : PCefFocusHandler;
                                         browser : PCefBrowser); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefFocusHandlerOwn) then
    TCefFocusHandlerOwn(TempObject).OnGotFocus(TCefBrowserRef.UnWrap(browser));
end;

constructor TCefFocusHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefFocusHandler));

  with PCefFocusHandler(FData)^ do
    begin
      on_take_focus := {$IFDEF FPC}@{$ENDIF}cef_focus_handler_on_take_focus;
      on_set_focus  := {$IFDEF FPC}@{$ENDIF}cef_focus_handler_on_set_focus;
      on_got_focus  := {$IFDEF FPC}@{$ENDIF}cef_focus_handler_on_got_focus;
    end;
end;

function TCefFocusHandlerOwn.OnSetFocus(const browser: ICefBrowser; source: TCefFocusSource): Boolean;
begin
  Result := False;
end;

procedure TCefFocusHandlerOwn.OnGotFocus(const browser: ICefBrowser);
begin
  //
end;

procedure TCefFocusHandlerOwn.OnTakeFocus(const browser: ICefBrowser; next: Boolean);
begin
  //
end;

procedure TCefFocusHandlerOwn.RemoveReferences;
begin
  //
end;

// TCustomFocusHandler

constructor TCustomFocusHandler.Create(const events : IChromiumEvents);
begin
  inherited Create;

  FEvents := Pointer(events);
end;

destructor TCustomFocusHandler.Destroy;
begin
  RemoveReferences;

  inherited Destroy;
end;

procedure TCustomFocusHandler.RemoveReferences;
begin
  FEvents := nil;
end;

procedure TCustomFocusHandler.OnGotFocus(const browser: ICefBrowser);
begin
  if (FEvents <> nil) then IChromiumEvents(FEvents).doOnGotFocus(browser);
end;

function TCustomFocusHandler.OnSetFocus(const browser: ICefBrowser; source: TCefFocusSource): Boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnSetFocus(browser, source)
   else
    Result := inherited OnSetFocus(browser, source);
end;

procedure TCustomFocusHandler.OnTakeFocus(const browser: ICefBrowser; next: Boolean);
begin
  if (FEvents <> nil) then IChromiumEvents(FEvents).doOnTakeFocus(browser, next);
end;

end.

