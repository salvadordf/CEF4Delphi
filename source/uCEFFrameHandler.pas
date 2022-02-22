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
//        Copyright © 2022 Salvador Diaz Fau. All rights reserved.
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

unit uCEFFrameHandler;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefFrameHandlerOwn = class(TCefBaseRefCountedOwn, ICefFrameHandler)
    protected
      procedure OnFrameCreated(const browser: ICefBrowser; const frame: ICefFrame); virtual;
      procedure OnFrameAttached(const browser: ICefBrowser; const frame: ICefFrame; reattached: boolean); virtual;
      procedure OnFrameDetached(const browser: ICefBrowser; const frame: ICefFrame); virtual;
      procedure OnMainFrameChanged(const browser: ICefBrowser; const old_frame, new_frame: ICefFrame); virtual;

      procedure RemoveReferences; virtual;

    public
      constructor Create; virtual;
  end;

  TCustomFrameHandler = class(TCefFrameHandlerOwn)
    protected
      FEvents : Pointer;

      procedure OnFrameCreated(const browser: ICefBrowser; const frame: ICefFrame); override;
      procedure OnFrameAttached(const browser: ICefBrowser; const frame: ICefFrame; reattached: boolean); override;
      procedure OnFrameDetached(const browser: ICefBrowser; const frame: ICefFrame); override;
      procedure OnMainFrameChanged(const browser: ICefBrowser; const old_frame, new_frame: ICefFrame); override;

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
  uCEFMiscFunctions, uCEFLibFunctions, uCEFClient, uCEFBrowser, uCEFFrame;


// TCefFrameHandlerOwn

procedure cef_frame_handler_on_frame_created(self: PCefFrameHandler; browser: PCefBrowser; frame: PCefFrame); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefFrameHandlerOwn) then
    TCefFrameHandlerOwn(TempObject).OnFrameCreated(TCefBrowserRef.UnWrap(browser),
                                                   TCefFrameRef.UnWrap(frame));
end;

procedure cef_frame_handler_on_frame_attached(self: PCefFrameHandler; browser: PCefBrowser; frame: PCefFrame; reattached: integer); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefFrameHandlerOwn) then
    TCefFrameHandlerOwn(TempObject).OnFrameAttached(TCefBrowserRef.UnWrap(browser),
                                                    TCefFrameRef.UnWrap(frame),
                                                    reattached <> 0);
end;

procedure cef_frame_handler_on_frame_detached(self: PCefFrameHandler; browser: PCefBrowser; frame: PCefFrame); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefFrameHandlerOwn) then
    TCefFrameHandlerOwn(TempObject).OnFrameDetached(TCefBrowserRef.UnWrap(browser),
                                                    TCefFrameRef.UnWrap(frame));
end;

procedure cef_frame_handler_on_main_frame_changed(self: PCefFrameHandler; browser: PCefBrowser; old_frame, new_frame: PCefFrame); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefFrameHandlerOwn) then
    TCefFrameHandlerOwn(TempObject).OnMainFrameChanged(TCefBrowserRef.UnWrap(browser),
                                                       TCefFrameRef.UnWrap(old_frame),
                                                       TCefFrameRef.UnWrap(new_frame));
end;

constructor TCefFrameHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefFrameHandler));

  with PCefFrameHandler(FData)^ do
    begin
      on_frame_created      := {$IFDEF FPC}@{$ENDIF}cef_frame_handler_on_frame_created;
      on_frame_attached     := {$IFDEF FPC}@{$ENDIF}cef_frame_handler_on_frame_attached;
      on_frame_detached     := {$IFDEF FPC}@{$ENDIF}cef_frame_handler_on_frame_detached;
      on_main_frame_changed := {$IFDEF FPC}@{$ENDIF}cef_frame_handler_on_main_frame_changed;
    end;
end;

procedure TCefFrameHandlerOwn.OnFrameCreated(const browser: ICefBrowser; const frame: ICefFrame);
begin
  //
end;

procedure TCefFrameHandlerOwn.OnFrameAttached(const browser: ICefBrowser; const frame: ICefFrame; reattached: boolean);
begin
  //
end;

procedure TCefFrameHandlerOwn.OnFrameDetached(const browser: ICefBrowser; const frame: ICefFrame);
begin
  //
end;

procedure TCefFrameHandlerOwn.OnMainFrameChanged(const browser: ICefBrowser; const old_frame, new_frame: ICefFrame);
begin
  //
end;

procedure TCefFrameHandlerOwn.RemoveReferences;
begin
  //
end;


// TCustomFrameHandler

constructor TCustomFrameHandler.Create(const events : IChromiumEvents);
begin
  inherited Create;

  FEvents := Pointer(events);
end;

destructor TCustomFrameHandler.Destroy;
begin
  RemoveReferences;

  inherited Destroy;
end;

procedure TCustomFrameHandler.RemoveReferences;
begin
  FEvents := nil;
end;

procedure TCustomFrameHandler.OnFrameCreated(const browser: ICefBrowser; const frame: ICefFrame);
begin
  if (FEvents <> nil) then
    IChromiumEvents(FEvents).doOnFrameCreated(browser, frame);
end;

procedure TCustomFrameHandler.OnFrameAttached(const browser: ICefBrowser; const frame: ICefFrame; reattached: boolean);
begin
  if (FEvents <> nil) then
    IChromiumEvents(FEvents).doOnFrameAttached(browser, frame, reattached);
end;

procedure TCustomFrameHandler.OnFrameDetached(const browser: ICefBrowser; const frame: ICefFrame);
begin
  if (FEvents <> nil) then
    IChromiumEvents(FEvents).doOnFrameDetached(browser, frame);
end;

procedure TCustomFrameHandler.OnMainFrameChanged(const browser: ICefBrowser; const old_frame, new_frame: ICefFrame);
begin
  if (FEvents <> nil) then
    IChromiumEvents(FEvents).doOnMainFrameChanged(browser, old_frame, new_frame);
end;

end.
