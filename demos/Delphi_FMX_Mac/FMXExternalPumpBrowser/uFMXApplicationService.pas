unit uFMXApplicationService;

{$I ..\..\..\source\cef.inc}

// This unit is based in the TFMXApplicationService class created by Takashi Yamamoto
// https://www.gesource.jp/weblog/?p=7367

interface

uses
  System.TypInfo, Macapi.Foundation, Macapi.CoreFoundation, Macapi.ObjectiveC,
  Macapi.Helpers, Macapi.CocoaTypes, Macapi.AppKit, FMX.Platform,
  uCEFMacOSInterfaces;

type
  TFMXApplicationService = class;

  TFMXApplicationDelegateEx = class(TOCLocal, IFMXApplicationDelegate)
    protected
      FAppService : TFMXApplicationService;

    public
      constructor Create(const aAppService : TFMXApplicationService);
      function    GetObjectiveCClass: PTypeInfo; override;

      // CrAppProtocol
      function  isHandlingSendEvent: boolean; cdecl;

      // CrAppControlProtocol
      procedure setHandlingSendEvent(handlingSendEvent: boolean); cdecl;

      // IFMXApplicationDelegate
      procedure onMenuClicked(sender: NSMenuItem); cdecl;

      // NSApplicationDelegate
      function  applicationShouldTerminate(Notification: NSNotification): NSInteger; cdecl;
      procedure applicationWillTerminate(Notification: NSNotification); cdecl;
      procedure applicationDidFinishLaunching(Notification: NSNotification); cdecl;
      procedure applicationDidHide(Notification: NSNotification); cdecl;
      procedure applicationDidUnhide(Notification: NSNotification); cdecl;
      function  applicationDockMenu(sender: NSApplication): NSMenu; cdecl;
  end;

  TFMXApplicationService = class(TInterfacedObject, IFMXApplicationService)
    protected
      FNewDelegate : IFMXApplicationDelegate;
      FOldDelegate : IFMXApplicationDelegate;

      FHandlingSendEventOverride : boolean;

      procedure ReplaceNSApplicationDelegate;
      procedure RestoreNSApplicationDelegate;

      function  GetPrivateFieldAsBoolean(const aFieldName : string) : boolean;

    public
      constructor Create;
      procedure   AfterConstruction; override;

      // IFMXApplicationService
      procedure Run;
      function  HandleMessage: Boolean;
      procedure WaitMessage;
      function  GetDefaultTitle: string;
      function  GetTitle: string;
      procedure SetTitle(const Value: string);
      function  GetVersionString: string;
      procedure Terminate;
      function  Terminating: Boolean;
      function  Running: Boolean;

      // IFMXApplicationServiceEx
      function  GetHandlingSendEvent : boolean;
      procedure SetHandlingSendEvent(aValue : boolean);

      // NSApplicationDelegate
      function  applicationShouldTerminate(Notification: NSNotification): NSInteger;
      procedure applicationWillTerminate(Notification: NSNotification);
      procedure applicationDidFinishLaunching(Notification: NSNotification);
      procedure applicationDidHide(Notification: NSNotification);
      procedure applicationDidUnhide(Notification: NSNotification);
      function  applicationDockMenu(sender: NSApplication): NSMenu;

      // IFMXApplicationDelegate
      procedure onMenuClicked(sender: NSMenuItem);

      class procedure AddPlatformService;

      class var OldFMXApplicationService: IFMXApplicationService;
      class var NewFMXApplicationService: IFMXApplicationService;

      property  DefaultTitle         : string    read GetDefaultTitle;
      property  Title                : string    read GetTitle                write SetTitle;
      property  AppVersion           : string    read GetVersionString;
      property  HandlingSendEvent    : boolean   read GetHandlingSendEvent    write SetHandlingSendEvent;
  end;

implementation

uses
  System.RTTI, FMX.Forms, FMX.Helpers.Mac, System.Messaging,
  uFMXExternalPumpBrowser, uCEFFMXWorkScheduler, uCEFApplication, uCEFConstants,
  uCEFMacOSFunctions;

// TFMXApplicationDelegateEx
constructor TFMXApplicationDelegateEx.Create(const aAppService : TFMXApplicationService);
begin
  inherited Create;

  FAppService := aAppService;
end;

function TFMXApplicationDelegateEx.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(CrAppControlProtocol);
end;

function TFMXApplicationDelegateEx.isHandlingSendEvent: Boolean;
begin
  Result := (FAppService <> nil) and FAppService.HandlingSendEvent;
end;

procedure TFMXApplicationDelegateEx.setHandlingSendEvent(handlingSendEvent: boolean);
begin
  if (FAppService <> nil) then
    FAppService.HandlingSendEvent := handlingSendEvent;
end;

function TFMXApplicationDelegateEx.applicationShouldTerminate(Notification: NSNotification): NSInteger;
begin
  if assigned(FAppService) then
    Result := FAppService.applicationShouldTerminate(Notification)
   else
    Result := 0;
end;

procedure TFMXApplicationDelegateEx.applicationWillTerminate(Notification: NSNotification);
begin
  if assigned(FAppService) then
    FAppService.applicationWillTerminate(Notification);
end;

procedure TFMXApplicationDelegateEx.applicationDidFinishLaunching(Notification: NSNotification);
begin
  if assigned(FAppService) then
    FAppService.applicationDidFinishLaunching(Notification);
end;

procedure TFMXApplicationDelegateEx.applicationDidHide(Notification: NSNotification);
begin
  if assigned(FAppService) then
    FAppService.applicationDidHide(Notification);
end;

procedure TFMXApplicationDelegateEx.applicationDidUnhide(Notification: NSNotification);
begin
  if assigned(FAppService) then
    FAppService.applicationDidUnhide(Notification);
end;

function TFMXApplicationDelegateEx.applicationDockMenu(sender: NSApplication): NSMenu;
begin
  if assigned(FAppService) then
    Result := FAppService.applicationDockMenu(sender)
   else
    Result := nil;
end;

procedure TFMXApplicationDelegateEx.onMenuClicked(sender: NSMenuItem);
begin
  if assigned(FAppService) then
    FAppService.onMenuClicked(sender);
end;

//  TFMXApplicationService
constructor TFMXApplicationService.Create;
begin
  inherited Create;

  FNewDelegate := nil;
  FOldDelegate := nil;

  FHandlingSendEventOverride := False;
end;

procedure TFMXApplicationService.AfterConstruction;
begin
  inherited AfterConstruction;

  ReplaceNSApplicationDelegate;
end;

procedure TFMXApplicationService.ReplaceNSApplicationDelegate;
var
  TempNSApplication : NSApplication;
begin
  TempNSApplication := TNSApplication.Wrap(TNSApplication.OCClass.sharedApplication);
  FNewDelegate      := IFMXApplicationDelegate(TFMXApplicationDelegateEx.Create(self));
  FOldDelegate      := IFMXApplicationDelegate(TempNSApplication.delegate);

  TempNSApplication.setDelegate(NSApplicationDelegate(FNewDelegate));
end;

procedure TFMXApplicationService.RestoreNSApplicationDelegate;
var
  TempNSApplication : NSApplication;
begin
  if assigned(FOldDelegate) then
    begin
      TempNSApplication := TNSApplication.Wrap(TNSApplication.OCClass.sharedApplication);
      TempNSApplication.setDelegate(FOldDelegate);
      FOldDelegate := nil;
    end;
end;

function TFMXApplicationService.GetPrivateFieldAsBoolean(const aFieldName : string) : boolean;
var
  TempContext  : TRttiContext;
  TempRttiType : TRttiType;
  TempField    : TRttiField;
  TempService  : TObject;
begin
  // This function is based on this answer in stackoverflow :
  // https://stackoverflow.com/questions/28135592/showmodal-form-that-opens-nsopenpanel-is-force-closed-in-delphi-firemonkey-osx
  Result      := False;
  TempService := TObject(TPlatformServices.Current.GetPlatformService(IFMXWindowService));

  if (TempService <> nil) then
    begin
      TempRttiType := TempContext.GetType(TempService.ClassType);

      if (TempRttiType <> nil) then
        begin
          TempField := TempRttiType.GetField(aFieldName);
          Result    := (TempField <> nil) and
                       TempField.GetValue(TempService).AsBoolean;
        end;
    end;
end;

procedure TFMXApplicationService.SetHandlingSendEvent(aValue : boolean);
begin
  FHandlingSendEventOverride := aValue;
end;

function TFMXApplicationService.GetHandlingSendEvent : boolean;
begin
  // We need to know when NSApp.sendEvent is being called and TPlatformCocoa
  // has a private field called FDisableClosePopups with that information.
  // In order to read that field we have to use RTTI.
  Result := FHandlingSendEventOverride or
            GetPrivateFieldAsBoolean('FDisableClosePopups');
end;

class procedure TFMXApplicationService.AddPlatformService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXApplicationService, IInterface(OldFMXApplicationService)) then
    begin
      TPlatformServices.Current.RemovePlatformService(IFMXApplicationService);
      NewFMXApplicationService := TFMXApplicationService.Create;
      TPlatformServices.Current.AddPlatformService(IFMXApplicationService, NewFMXApplicationService);
    end;
end;

function TFMXApplicationService.GetDefaultTitle: string;
begin
  Result := OldFMXApplicationService.GetDefaultTitle;
end;

function TFMXApplicationService.GetTitle: string;
begin
  Result := OldFMXApplicationService.GetTitle;
end;

function TFMXApplicationService.GetVersionString: string;
begin
  {$IFDEF DELPHI22_UP}
  Result := OldFMXApplicationService.GetVersionString;
  {$ELSE DELPHI22_UP}
  Result := 'unsupported yet';
  {$ENDIF DELPHI22_UP}
end;

procedure TFMXApplicationService.Run;
begin
  OldFMXApplicationService.Run;
end;

procedure TFMXApplicationService.SetTitle(const Value: string);
begin
  OldFMXApplicationService.SetTitle(Value);
end;

procedure TFMXApplicationService.Terminate;
begin
  OldFMXApplicationService.Terminate;
end;

function TFMXApplicationService.Terminating: Boolean;
begin
  Result := OldFMXApplicationService.Terminating;
end;

procedure TFMXApplicationService.WaitMessage;
begin
  OldFMXApplicationService.WaitMessage;
end;

function TFMXApplicationService.Running: Boolean;
begin
  {$IFDEF DELPHI24_UP}
  Result := OldFMXApplicationService.Running;
  {$ELSE}
  Result := True;
  {$ENDIF}
end;

function TFMXApplicationService.applicationShouldTerminate(Notification: NSNotification): NSInteger;
begin
  if assigned(FOldDelegate) then
    Result := FOldDelegate.applicationShouldTerminate(Notification)
   else
    Result := 0;
end;

procedure TFMXApplicationService.applicationWillTerminate(Notification: NSNotification);
begin
  RestoreNSApplicationDelegate;

  if assigned(FOldDelegate) then
    FOldDelegate.applicationWillTerminate(Notification);
end;

procedure TFMXApplicationService.applicationDidFinishLaunching(Notification: NSNotification);
begin
  if assigned(FOldDelegate) then
    FOldDelegate.applicationDidFinishLaunching(Notification);
end;

procedure TFMXApplicationService.applicationDidHide(Notification: NSNotification);
begin
  if assigned(FOldDelegate) then
    FOldDelegate.applicationDidHide(Notification);
end;

procedure TFMXApplicationService.applicationDidUnhide(Notification: NSNotification);
begin
  if assigned(FOldDelegate) then
    FOldDelegate.applicationDidUnhide(Notification);
end;

function TFMXApplicationService.applicationDockMenu(sender: NSApplication): NSMenu;
begin
  if assigned(FOldDelegate) then
    Result := FOldDelegate.applicationDockMenu(sender)
   else
    Result := nil;
end;

procedure TFMXApplicationService.onMenuClicked(sender: NSMenuItem);
begin
  if assigned(FOldDelegate) then
    FOldDelegate.onMenuClicked(sender);
end;

function TFMXApplicationService.HandleMessage: Boolean;
begin
  Result := OldFMXApplicationService.HandleMessage;
end;

end.
