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

unit uCEFMacOSCustomCocoaTimer;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

{$IFDEF MACOS}
uses
  System.Classes, System.TypInfo,
  Macapi.ObjectiveC, Macapi.Foundation, Macapi.CocoaTypes, Macapi.ObjCRuntime,
  uCEFMacOSInterfaces;

type
  TCustomCocoaTimer = class(TOCLocal)
    private
      FTimer    : NSTimer;
      FOnTimer  : TNotifyEvent;
      FInterval : integer;
      FEnabled  : boolean;

      procedure CreateNSTimer;
      procedure DestroyNSTimer;

      procedure SetEnabled(aValue : boolean);

    public
      constructor Create;
      destructor  Destroy; override;
      function    GetObjectiveCClass: PTypeInfo; override;
      procedure   timerTimeout(timer: NSTimer); cdecl;

      property OnTimer   : TNotifyEvent  read FOnTimer   write FOnTimer;
      property Interval  : integer       read FInterval  write FInterval;
      property Enabled   : boolean       read FEnabled   write SetEnabled;
  end;
{$ENDIF}

implementation

{$IFDEF MACOS}
uses
  uCEFMacOSFunctions;

constructor TCustomCocoaTimer.Create;
begin
  inherited Create;

  FTimer    := nil;
  FOnTimer  := nil;
  FInterval := 1000;
  FEnabled  := False;
end;

destructor TCustomCocoaTimer.Destroy;
begin
  DestroyNSTimer;

  inherited Destroy;
end;

procedure TCustomCocoaTimer.DestroyNSTimer;
begin
  if (FTimer <> nil) then
    begin
      FTimer.invalidate;
      FTimer := nil;
    end;

  FEnabled := False;
end;

procedure TCustomCocoaTimer.CreateNSTimer;
var
  TempInterval : NSTimeInterval;
  TempRunLoop  : NSRunLoop;
begin
  if (FTimer <> nil) then
    DestroyNSTimer;

  TempInterval := FInterval / 1000;
  FTimer       := TNSTimer.Wrap(TNSTimer.OCClass.timerWithTimeInterval(TempInterval, GetObjectID, sel_getUid('timerTimeout:'), nil, False));

  if (FTimer <> nil) then
    begin
      TempRunLoop := TNSRunloop.Wrap(TNSRunLoop.OCClass.currentRunLoop);
      TempRunLoop.addTimer(FTimer, NSRunLoopCommonModes);
      //TempRunLoop.addTimer(FTimer, NSEventTrackingRunLoopMode);
      FEnabled := True;
    end;
end;

function TCustomCocoaTimer.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(ICustomCocoaTimer);
end;

procedure TCustomCocoaTimer.timerTimeout(timer: NSTimer);
begin
  if Assigned(FOnTimer) then
    FOnTimer(self);
end;

procedure TCustomCocoaTimer.SetEnabled(aValue : boolean);
begin
  if (FEnabled = aValue) then exit;

  if aValue then
    CreateNSTimer
   else
    DestroyNSTimer;
end;
{$ENDIF}

end.
