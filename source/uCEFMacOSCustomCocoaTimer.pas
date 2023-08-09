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
