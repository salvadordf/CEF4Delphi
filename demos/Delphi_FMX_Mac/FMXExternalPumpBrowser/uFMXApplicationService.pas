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
unit uFMXApplicationService;

{$I cef.inc}

// This unit is based in the TFMXApplicationService class created by Takashi Yamamoto
// https://www.gesource.jp/weblog/?p=7367

interface

uses
  Macapi.Foundation, Macapi.CoreFoundation, Macapi.ObjectiveC, Macapi.Helpers,
  Macapi.CocoaTypes, Macapi.AppKit, FMX.Platform;

type
  TFMXApplicationService = class;

  ICrAppProtocol = interface(NSApplicationDelegate)
    ['{2071D289-9A54-4AD7-BD83-E521ACD5C528}']
    function isHandlingSendEvent: boolean; cdecl;
  end;

  ICrAppControlProtocol = interface(ICrAppProtocol)
    ['{BCCDF64D-E8D7-4E0B-83BC-30F87145576C}']
    procedure setHandlingSendEvent(handlingSendEvent: boolean); cdecl;
  end;

  TCrAppProtocol = class(TOCLocal, ICrAppControlProtocol)
  private
    FAppService : TFMXApplicationService;

  public
    constructor Create(const aAppService : TFMXApplicationService);

    // ICrAppProtocol
    function  isHandlingSendEvent: boolean; cdecl;

    // ICrAppControlProtocol
    procedure setHandlingSendEvent(handlingSendEvent: boolean); cdecl;

    // NSApplicationDelegate
    function  applicationShouldTerminate(Notification: NSNotification): NSInteger; cdecl;
    procedure applicationWillTerminate(Notification: NSNotification); cdecl;
    procedure applicationDidFinishLaunching(Notification: NSNotification); cdecl;
    procedure applicationDidHide(Notification: NSNotification); cdecl;
    procedure applicationDidUnhide(Notification: NSNotification); cdecl;
    function  applicationDockMenu(sender: NSApplication): NSMenu; cdecl;
  end;

  TFMXApplicationService = class(TInterfacedObject, IFMXApplicationService)
    private
      FNewDelegate          : ICrAppControlProtocol;
      FOldDelegate          : NSApplicationDelegate;

    protected
      class var OldFMXApplicationService: IFMXApplicationService;
      class var NewFMXApplicationService: IFMXApplicationService;

      procedure ReplaceNSApplicationDelegate;
      procedure RestoreNSApplicationDelegate;

      function  GetHandlingSendEvent : boolean;

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

      // NSApplicationDelegate
      function  applicationShouldTerminate(Notification: NSNotification): NSInteger;
      procedure applicationWillTerminate(Notification: NSNotification);
      procedure applicationDidFinishLaunching(Notification: NSNotification);
      procedure applicationDidHide(Notification: NSNotification);
      procedure applicationDidUnhide(Notification: NSNotification);
      function  applicationDockMenu(sender: NSApplication): NSMenu;

      class procedure AddPlatformService;

      property  DefaultTitle         : string    read GetDefaultTitle;
      property  Title                : string    read GetTitle                write SetTitle;
      property  AppVersion           : string    read GetVersionString;
      property  HandlingSendEvent    : boolean   read GetHandlingSendEvent;
  end;

implementation

uses
  System.RTTI, FMX.Forms,
  uFMXExternalPumpBrowser, uCEFFMXWorkScheduler, uCEFApplication, uCEFConstants;

// TCrAppProtocol
constructor TCrAppProtocol.Create(const aAppService : TFMXApplicationService);
begin
  inherited Create;

  FAppService := aAppService;
end;

function TCrAppProtocol.isHandlingSendEvent: Boolean;
begin
  Result := (FAppService <> nil) and FAppService.HandlingSendEvent;
end;

procedure TCrAppProtocol.setHandlingSendEvent(handlingSendEvent: boolean);
begin
  //
end;

function TCrAppProtocol.applicationShouldTerminate(Notification: NSNotification): NSInteger;
begin
  if assigned(FAppService) then
    Result := FAppService.applicationShouldTerminate(Notification)
   else
    Result := 0;
end;

procedure TCrAppProtocol.applicationWillTerminate(Notification: NSNotification);
begin
  if assigned(FAppService) then
    FAppService.applicationWillTerminate(Notification);
end;

procedure TCrAppProtocol.applicationDidFinishLaunching(Notification: NSNotification);
begin
  if assigned(FAppService) then
    FAppService.applicationDidFinishLaunching(Notification);
end;

procedure TCrAppProtocol.applicationDidHide(Notification: NSNotification);
begin
  if assigned(FAppService) then
    FAppService.applicationDidHide(Notification);
end;

procedure TCrAppProtocol.applicationDidUnhide(Notification: NSNotification);
begin
  if assigned(FAppService) then
    FAppService.applicationDidUnhide(Notification);
end;

function TCrAppProtocol.applicationDockMenu(sender: NSApplication): NSMenu;
begin
  if assigned(FAppService) then
    Result := FAppService.applicationDockMenu(sender)
   else
    Result := nil;
end;

//  TFMXApplicationService
constructor TFMXApplicationService.Create;
begin
  inherited Create;

  FNewDelegate          := nil;
  FOldDelegate          := nil;
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
  FNewDelegate      := ICrAppControlProtocol(TCrAppProtocol.Create(self));
  FOldDelegate      := NSApplicationDelegate(TempNSApplication.delegate);

  TempNSApplication.setDelegate(FNewDelegate);
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

function TFMXApplicationService.GetHandlingSendEvent : boolean;
var
  TempContext  : TRttiContext;
  TempRttiType : TRttiType;
  TempField    : TRttiField;
  TempService  : TObject;
begin
  Result := False;

  // We need to know when NSApp.sendEvent is being called and TPlatformCocoa
  // has a private field with that information. In order to read that field we
  // have to use RTTI.
  // This function is based on this answer in stackoverflow :
  // https://stackoverflow.com/questions/28135592/showmodal-form-that-opens-nsopenpanel-is-force-closed-in-delphi-firemonkey-osx

  TempService := TObject(TPlatformServices.Current.GetPlatformService(IFMXWindowService));

  if (TempService <> nil) then
    begin
      TempRttiType := TempContext.GetType(TempService.ClassType);

      if (TempRttiType <> nil) then
        begin
          TempField := TempRttiType.GetField('FDisableClosePopups');
          Result    := (TempField <> nil) and
                       TempField.GetValue(TempService).AsBoolean;
        end;
    end;
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

function TFMXApplicationService.HandleMessage: Boolean;
begin
  Result := OldFMXApplicationService.HandleMessage;
end;

end.
