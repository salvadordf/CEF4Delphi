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
//        Copyright © 2020 Salvador Diaz Fau. All rights reserved.
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
      procedure OnAudioStreamStarted(const browser: ICefBrowser; audio_stream_id, channels: integer; channel_layout: TCefChannelLayout; sample_rate, frames_per_buffer: integer); virtual;
      procedure OnAudioStreamPacket(const browser: ICefBrowser; audio_stream_id: integer; const data : PPSingle; frames: integer; pts: int64); virtual;
      procedure OnAudioStreamStopped(const browser: ICefBrowser; audio_stream_id: integer); virtual;

      procedure RemoveReferences; virtual;

    public
      constructor Create; virtual;
  end;

  TCustomAudioHandler = class(TCefAudioHandlerOwn)
    protected
      FEvents : Pointer;

      procedure OnAudioStreamStarted(const browser: ICefBrowser; audio_stream_id, channels: integer; channel_layout: TCefChannelLayout; sample_rate, frames_per_buffer: integer); override;
      procedure OnAudioStreamPacket(const browser: ICefBrowser; audio_stream_id: integer; const data : PPSingle; frames: integer; pts: int64); override;
      procedure OnAudioStreamStopped(const browser: ICefBrowser; audio_stream_id: integer); override;

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

procedure cef_audio_handler_on_audio_stream_started(self              : PCefAudioHandler;
                                                    browser           : PCefBrowser;
                                                    audio_stream_id   : integer;
                                                    channels          : integer;
                                                    channel_layout    : TCefChannelLayout;
                                                    sample_rate       : integer;
                                                    frames_per_buffer : integer); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefAudioHandlerOwn) then
    TCefAudioHandlerOwn(TempObject).OnAudioStreamStarted(TCefBrowserRef.UnWrap(browser),
                                                         audio_stream_id,
                                                         channels,
                                                         channel_layout,
                                                         sample_rate,
                                                         frames_per_buffer);
end;

procedure cef_audio_handler_on_audio_stream_packet(      self            : PCefAudioHandler;
                                                         browser         : PCefBrowser;
                                                         audio_stream_id : integer;
                                                   const data            : PPSingle;
                                                         frames          : integer;
                                                         pts             : int64); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefAudioHandlerOwn) then
    TCefAudioHandlerOwn(TempObject).OnAudioStreamPacket(TCefBrowserRef.UnWrap(browser),
                                                        audio_stream_id,
                                                        data,
                                                        frames,
                                                        pts);
end;

procedure cef_audio_handler_on_audio_stream_stopped(self            : PCefAudioHandler;
                                                    browser         : PCefBrowser;
                                                    audio_stream_id : integer); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefAudioHandlerOwn) then
    TCefAudioHandlerOwn(TempObject).OnAudioStreamStopped(TCefBrowserRef.UnWrap(browser),
                                                         audio_stream_id);
end;

constructor TCefAudioHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefAudioHandler));

  with PCefAudioHandler(FData)^ do
    begin
      on_audio_stream_started := {$IFDEF FPC}@{$ENDIF}cef_audio_handler_on_audio_stream_started;
      on_audio_stream_packet  := {$IFDEF FPC}@{$ENDIF}cef_audio_handler_on_audio_stream_packet;
      on_audio_stream_stopped := {$IFDEF FPC}@{$ENDIF}cef_audio_handler_on_audio_stream_stopped;
    end;
end;

procedure TCefAudioHandlerOwn.OnAudioStreamStarted(const browser           : ICefBrowser;
                                                         audio_stream_id   : integer;
                                                         channels          : integer;
                                                         channel_layout    : TCefChannelLayout;
                                                         sample_rate       : integer;
                                                         frames_per_buffer : integer);
begin
  //
end;

procedure TCefAudioHandlerOwn.OnAudioStreamPacket(const browser         : ICefBrowser;
                                                        audio_stream_id : integer;
                                                  const data            : PPSingle;
                                                        frames          : integer;
                                                        pts             : int64);
begin
  //
end;

procedure TCefAudioHandlerOwn.OnAudioStreamStopped(const browser         : ICefBrowser;
                                                         audio_stream_id : integer);
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

procedure TCustomAudioHandler.OnAudioStreamStarted(const browser           : ICefBrowser;
                                                         audio_stream_id   : integer;
                                                         channels          : integer;
                                                         channel_layout    : TCefChannelLayout;
                                                         sample_rate       : integer;
                                                         frames_per_buffer : integer);
begin
  if (FEvents <> nil) then
    IChromiumEvents(FEvents).doOnAudioStreamStarted(browser,
                                                    audio_stream_id,
                                                    channels,
                                                    channel_layout,
                                                    sample_rate,
                                                    frames_per_buffer);
end;

procedure TCustomAudioHandler.OnAudioStreamPacket(const browser         : ICefBrowser;
                                                        audio_stream_id : integer;
                                                  const data            : PPSingle;
                                                        frames          : integer;
                                                        pts             : int64);
begin
  if (FEvents <> nil) then
    IChromiumEvents(FEvents).doOnAudioStreamPacket(browser,
                                                   audio_stream_id,
                                                   data,
                                                   frames,
                                                   pts);
end;

procedure TCustomAudioHandler.OnAudioStreamStopped(const browser         : ICefBrowser;
                                                         audio_stream_id : integer);
begin
  if (FEvents <> nil) then
    IChromiumEvents(FEvents).doOnAudioStreamStopped(browser,
                                                    audio_stream_id);
end;

end.
