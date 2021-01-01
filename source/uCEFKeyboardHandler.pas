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

unit uCEFKeyboardHandler;

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
  TCefKeyboardHandlerOwn = class(TCefBaseRefCountedOwn, ICefKeyboardHandler)
    protected
      function OnPreKeyEvent(const browser: ICefBrowser; const event: PCefKeyEvent; osEvent: TCefEventHandle; out isKeyboardShortcut: Boolean): Boolean; virtual;
      function OnKeyEvent(const browser: ICefBrowser; const event: PCefKeyEvent; osEvent: TCefEventHandle): Boolean; virtual;

      procedure RemoveReferences; virtual;

    public
      constructor Create; virtual;
  end;

  TCustomKeyboardHandler = class(TCefKeyboardHandlerOwn)
    protected
      FEvents : Pointer;

      function OnPreKeyEvent(const browser: ICefBrowser; const event: PCefKeyEvent; osEvent: TCefEventHandle; out isKeyboardShortcut: Boolean): Boolean; override;
      function OnKeyEvent(const browser: ICefBrowser; const event: PCefKeyEvent; osEvent: TCefEventHandle): Boolean; override;

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

function cef_keyboard_handler_on_pre_key_event(      self                 : PCefKeyboardHandler;
                                                     browser              : PCefBrowser;
                                               const event                : PCefKeyEvent;
                                                     os_event             : TCefEventHandle;
                                                     is_keyboard_shortcut : PInteger): Integer; stdcall;
var
  TempShortcut : Boolean;
  TempObject   : TObject;
begin
  Result       := Ord(False);
  TempShortcut := is_keyboard_shortcut^ <> 0;
  TempObject   := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefKeyboardHandlerOwn) then
    Result := Ord(TCefKeyboardHandlerOwn(TempObject).OnPreKeyEvent(TCefBrowserRef.UnWrap(browser),
                                                                   event,
                                                                   os_event,
                                                                   TempShortcut));

  is_keyboard_shortcut^ := Ord(TempShortcut);
end;

function cef_keyboard_handler_on_key_event(      self     : PCefKeyboardHandler;
                                                 browser  : PCefBrowser;
                                           const event    : PCefKeyEvent;
                                                 os_event : TCefEventHandle): Integer; stdcall;
var
  TempObject : TObject;
begin
  Result     := Ord(False);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefKeyboardHandlerOwn) then
    Result := Ord(TCefKeyboardHandlerOwn(TempObject).OnKeyEvent(TCefBrowserRef.UnWrap(browser),
                                                                event,
                                                                os_event));
end;

constructor TCefKeyboardHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefKeyboardHandler));

  with PCefKeyboardHandler(FData)^ do
    begin
      on_pre_key_event  := {$IFDEF FPC}@{$ENDIF}cef_keyboard_handler_on_pre_key_event;
      on_key_event      := {$IFDEF FPC}@{$ENDIF}cef_keyboard_handler_on_key_event;
    end;
end;

function TCefKeyboardHandlerOwn.OnPreKeyEvent(const browser: ICefBrowser; const event: PCefKeyEvent; osEvent: TCefEventHandle; out isKeyboardShortcut: Boolean): Boolean;
begin
  Result := False;
end;

function TCefKeyboardHandlerOwn.OnKeyEvent(const browser: ICefBrowser; const event: PCefKeyEvent; osEvent: TCefEventHandle): Boolean;
begin
  Result := False;
end;

procedure TCefKeyboardHandlerOwn.RemoveReferences;
begin
  //
end;

// TCustomKeyboardHandler

constructor TCustomKeyboardHandler.Create(const events : IChromiumEvents);
begin
  inherited Create;

  FEvents := Pointer(events);
end;

destructor TCustomKeyboardHandler.Destroy;
begin
  RemoveReferences;

  inherited Destroy;
end;

procedure TCustomKeyboardHandler.RemoveReferences;
begin
  FEvents := nil;
end;

function TCustomKeyboardHandler.OnKeyEvent(const browser : ICefBrowser;
                                           const event   : PCefKeyEvent;
                                                 osEvent : TCefEventHandle): Boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnKeyEvent(browser, event, osEvent)
   else
    Result := inherited OnKeyEvent(browser, event, osEvent);
end;

function TCustomKeyboardHandler.OnPreKeyEvent(const browser            : ICefBrowser;
                                              const event              : PCefKeyEvent;
                                                    osEvent            : TCefEventHandle;
                                              out   isKeyboardShortcut : Boolean): Boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnPreKeyEvent(browser, event, osEvent, isKeyboardShortcut)
   else
    Result := inherited OnPreKeyEvent(browser, event, osEvent, isKeyboardShortcut);
end;

end.
