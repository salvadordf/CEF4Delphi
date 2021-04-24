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

unit uCEFWorkSchedulerThread;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes, System.SyncObjs,
  {$ELSE}
  Classes, SyncObjs,
  {$ENDIF}
  uCEFConstants;

type
  TCEFWorkSchedulerThread = class(TThread)
    protected
      FCritSect        : TCriticalSection;
      FInterval        : integer;
      FEvent           : TEvent;
      FOnPulse         : TNotifyEvent;
      FPulsing         : boolean;
      FMustReset       : boolean;
      FDefaultInterval : integer;

      function  Lock : boolean;
      procedure Unlock;
      function  CanPulse(var aInterval : integer) : boolean;
      procedure DoOnPulseEvent;
      procedure EventTimeOut;
      procedure SignaledEvent;
      procedure Execute; override;

    public
      constructor Create;
      destructor  Destroy; override;
      procedure   AfterConstruction; override;
      procedure   NextPulse(aInterval : integer);

      property    DefaultInterval : integer       read FDefaultInterval  write FDefaultInterval   default CEF_TIMER_MAXDELAY;
      property    OnPulse         : TNotifyEvent  read FOnPulse          write FOnPulse;
  end;

implementation

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils, System.Math;
  {$ELSE}
  SysUtils, Math;
  {$ENDIF}

constructor TCEFWorkSchedulerThread.Create;
begin
  FOnPulse         := nil;
  FCritSect        := nil;
  FPulsing         := False;
  FEvent           := nil;
  FDefaultInterval := CEF_TIMER_MAXDELAY;
  FInterval        := FDefaultInterval;
  FMustReset       := False;

  inherited Create(True);

  FreeOnTerminate := False;
end;

destructor TCEFWorkSchedulerThread.Destroy;
begin
  if (FEvent    <> nil) then FreeAndNil(FEvent);
  if (FCritSect <> nil) then FreeAndNil(FCritSect);

  inherited Destroy;
end;

procedure TCEFWorkSchedulerThread.AfterConstruction;
begin
  inherited AfterConstruction;

  FEvent    := TEvent.Create(nil, False, False, '');
  FCritSect := TCriticalSection.Create;
end;

procedure TCEFWorkSchedulerThread.DoOnPulseEvent;
begin
  if assigned(FOnPulse) then FOnPulse(self);
end;

function TCEFWorkSchedulerThread.Lock : boolean;
begin
  if not(Terminated) and (FCritSect <> nil) then
    begin
      FCritSect.Acquire;
      Result := True;
    end
   else
    Result := False;
end;

procedure TCEFWorkSchedulerThread.Unlock;
begin
  if (FCritSect <> nil) then FCritSect.Release;
end;

procedure TCEFWorkSchedulerThread.NextPulse(aInterval : integer);
begin
  if Lock then
    try
      FInterval  := min(aInterval, CEF_TIMER_MAXDELAY);
      FMustReset := True;

      if FPulsing then
        begin
          FPulsing := False;
          FEvent.SetEvent;
        end;
    finally
      Unlock;
    end;
end;

procedure TCEFWorkSchedulerThread.EventTimeOut;
begin
  if Lock then
    try
      if FMustReset then
        begin
          FInterval  := FDefaultInterval;
          FMustReset := False;
        end;

      FPulsing := False;
    finally
      Unlock;
      if not(Terminated) then Synchronize({$IFDEF FPC}self, @{$ENDIF}DoOnPulseEvent);
    end;
end;

procedure TCEFWorkSchedulerThread.SignaledEvent;
begin
  if Lock then
    try
      FPulsing := False;
    finally
      Unlock;
    end;
end;

function TCEFWorkSchedulerThread.CanPulse(var aInterval : integer) : boolean;
begin
  Result := False;

  if Lock then
    try
      aInterval := FInterval;

      if (aInterval > 0) then
        begin
          Result   := True;
          FPulsing := True;
          FEvent.ResetEvent;
        end;
    finally
      Unlock;
    end;
end;

procedure TCEFWorkSchedulerThread.Execute;
var
  TempInterval : integer;
begin
  while CanPulse(TempInterval) do
    if (FEvent.WaitFor(TempInterval) = wrTimeout) then
      EventTimeOut
     else
      SignaledEvent;
end;

end.
