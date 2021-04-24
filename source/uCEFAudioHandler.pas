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

unit uCEFAudioHandler;

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
  TCefAudioHandlerOwn = class(TCefBaseRefCountedOwn, ICefAudioHandler)
    protected
      procedure OnGetAudioParameters(const browser: ICefBrowser; var params: TCefAudioParameters; var aResult: boolean); virtual;
      procedure OnAudioStreamStarted(const browser: ICefBrowser; const params: TCefAudioParameters; channels: integer); virtual;
      procedure OnAudioStreamPacket(const browser: ICefBrowser; const data : PPSingle; frames: integer; pts: int64); virtual;
      procedure OnAudioStreamStopped(const browser: ICefBrowser); virtual;
      procedure OnAudioStreamError(const browser: ICefBrowser; const message_: ustring); virtual;

      procedure RemoveReferences; virtual;

    public
      constructor Create; virtual;
  end;

  TCustomAudioHandler = class(TCefAudioHandlerOwn)
    protected
      FEvents : Pointer;

      procedure OnGetAudioParameters(const browser: ICefBrowser; var params: TCefAudioParameters; var aResult: boolean); override;
      procedure OnAudioStreamStarted(const browser: ICefBrowser; const params: TCefAudioParameters; channels: integer); override;
      procedure OnAudioStreamPacket(const browser: ICefBrowser; const data : PPSingle; frames: integer; pts: int64); override;
      procedure OnAudioStreamStopped(const browser: ICefBrowser); override;
      procedure OnAudioStreamError(const browser: ICefBrowser; const message_: ustring); override;

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

function cef_audio_handler_get_audio_parameters(self    : PCefAudioHandler;
                                                browser : PCefBrowser;
                                                params  : PCefAudioParameters): Integer; stdcall;
var
  TempObject : TObject;
  TempParams : TCefAudioParameters;
  TempResult : boolean;
begin
  TempObject := CefGetObject(self);
  TempResult := False;

  if (TempObject <> nil) and (TempObject is TCefAudioHandlerOwn) then
    begin
      TempParams := params^;
      TCefAudioHandlerOwn(TempObject).OnGetAudioParameters(TCefBrowserRef.UnWrap(browser),
                                                           TempParams,
                                                           TempResult);
      if TempResult then
        params^ := TempParams;
    end;

  Result := ord(TempResult);
end;

procedure cef_audio_handler_on_audio_stream_started(      self     : PCefAudioHandler;
                                                          browser  : PCefBrowser;
                                                    const params   : PCefAudioParameters;
                                                          channels : integer); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefAudioHandlerOwn) then
    TCefAudioHandlerOwn(TempObject).OnAudioStreamStarted(TCefBrowserRef.UnWrap(browser),
                                                         params^,
                                                         channels);
end;

procedure cef_audio_handler_on_audio_stream_packet(      self    : PCefAudioHandler;
                                                         browser : PCefBrowser;
                                                   const data    : PPSingle;
                                                         frames  : integer;
                                                         pts     : int64); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefAudioHandlerOwn) then
    TCefAudioHandlerOwn(TempObject).OnAudioStreamPacket(TCefBrowserRef.UnWrap(browser),
                                                        data,
                                                        frames,
                                                        pts);
end;

procedure cef_audio_handler_on_audio_stream_stopped(self: PCefAudioHandler; browser: PCefBrowser); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefAudioHandlerOwn) then
    TCefAudioHandlerOwn(TempObject).OnAudioStreamStopped(TCefBrowserRef.UnWrap(browser));
end;

procedure cef_audio_handler_on_audio_stream_error(self: PCefAudioHandler; browser: PCefBrowser; const message_: PCefString); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefAudioHandlerOwn) then
    TCefAudioHandlerOwn(TempObject).OnAudioStreamError(TCefBrowserRef.UnWrap(browser),
                                                       CefString(message_));
end;

constructor TCefAudioHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefAudioHandler));

  with PCefAudioHandler(FData)^ do
    begin
      get_audio_parameters    := {$IFDEF FPC}@{$ENDIF}cef_audio_handler_get_audio_parameters;
      on_audio_stream_started := {$IFDEF FPC}@{$ENDIF}cef_audio_handler_on_audio_stream_started;
      on_audio_stream_packet  := {$IFDEF FPC}@{$ENDIF}cef_audio_handler_on_audio_stream_packet;
      on_audio_stream_stopped := {$IFDEF FPC}@{$ENDIF}cef_audio_handler_on_audio_stream_stopped;
      on_audio_stream_error   := {$IFDEF FPC}@{$ENDIF}cef_audio_handler_on_audio_stream_error;
    end;
end;

procedure TCefAudioHandlerOwn.OnGetAudioParameters(const browser : ICefBrowser;
                                                   var   params  : TCefAudioParameters;
                                                   var   aResult : boolean);
begin
  //
end;

procedure TCefAudioHandlerOwn.OnAudioStreamStarted(const browser  : ICefBrowser;
                                                   const params   : TCefAudioParameters;
                                                         channels : integer);
begin
  //
end;

procedure TCefAudioHandlerOwn.OnAudioStreamPacket(const browser : ICefBrowser;
                                                  const data    : PPSingle;
                                                        frames  : integer;
                                                        pts     : int64);
begin
  //
end;

procedure TCefAudioHandlerOwn.OnAudioStreamStopped(const browser : ICefBrowser);
begin
  //
end;

procedure TCefAudioHandlerOwn.OnAudioStreamError(const browser  : ICefBrowser;
                                                 const message_ : ustring);
begin
  //
end;

procedure TCefAudioHandlerOwn.RemoveReferences;
begin
  //
end;


// TCustomAudioHandler

constructor TCustomAudioHandler.Create(const events : IChromiumEvents);
begin
  inherited Create;

  FEvents := Pointer(events);
end;

destructor TCustomAudioHandler.Destroy;
begin
  RemoveReferences;

  inherited Destroy;
end;

procedure TCustomAudioHandler.RemoveReferences;
begin
  FEvents := nil;
end;

procedure TCustomAudioHandler.OnGetAudioParameters(const browser : ICefBrowser;
                                                   var   params  : TCefAudioParameters;
                                                   var   aResult : boolean);
begin
  if (FEvents <> nil) then
    IChromiumEvents(FEvents).doOnGetAudioParameters(browser,
                                                    params,
                                                    aResult);
end;

procedure TCustomAudioHandler.OnAudioStreamStarted(const browser  : ICefBrowser;
                                                   const params   : TCefAudioParameters;
                                                         channels : integer);
begin
  if (FEvents <> nil) then
    IChromiumEvents(FEvents).doOnAudioStreamStarted(browser,
                                                    params,
                                                    channels);
end;

procedure TCustomAudioHandler.OnAudioStreamPacket(const browser : ICefBrowser;
                                                  const data    : PPSingle;
                                                        frames  : integer;
                                                        pts     : int64);
begin
  if (FEvents <> nil) then
    IChromiumEvents(FEvents).doOnAudioStreamPacket(browser,
                                                   data,
                                                   frames,
                                                   pts);
end;

procedure TCustomAudioHandler.OnAudioStreamStopped(const browser : ICefBrowser);
begin
  if (FEvents <> nil) then
    IChromiumEvents(FEvents).doOnAudioStreamStopped(browser);
end;

procedure TCustomAudioHandler.OnAudioStreamError(const browser  : ICefBrowser;
                                                 const message_ : ustring);
begin
  if (FEvents <> nil) then
    IChromiumEvents(FEvents).doOnAudioStreamError(browser,
                                                  message_);
end;

end.
