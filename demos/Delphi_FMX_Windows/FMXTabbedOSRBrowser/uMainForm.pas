unit uMainForm;

{$I ..\..\..\source\cef.inc}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Types, System.Actions,
  System.UITypes, System.Classes, System.Variants, FMX.Types, FMX.Controls,
  FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl, FMX.ActnList,
  System.Math, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts,
  uCEFFMXWorkScheduler;


const
  CEF_INITIALIZED      = WM_APP + $100;
  CEF_DESTROYTAB       = WM_APP + $101;
  CEF_SHOWBROWSER      = WM_APP + $102;

  HOMEPAGE_URL        = 'https://www.google.com';
  DEFAULT_TAB_CAPTION = 'New tab';

type
  TMainForm = class(TForm)
    ButtonLay: TLayout;
    AddTabBtn: TSpeedButton;
    RemoveTabBtn: TSpeedButton;
    BrowserTabCtrl: TTabControl;
    ActionList1: TActionList;
    AddTabAction: TAction;
    RemoveTabAction: TAction;
    PrevTabBtn: TSpeedButton;
    NextTabBtn: TSpeedButton;
    ShowTabsBtn: TSpeedButton;
    PrevTabAction: TAction;
    NextTabAction: TAction;
    ShowTabsAction: TAction;

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    procedure AddTabActionExecute(Sender: TObject);
    procedure RemoveTabActionExecute(Sender: TObject);
    procedure BrowserTabCtrlChange(Sender: TObject);
    procedure PrevTabActionExecute(Sender: TObject);
    procedure NextTabActionExecute(Sender: TObject);
    procedure ShowTabsActionExecute(Sender: TObject);

  protected
    // Variables to control when can we destroy the form safely
    FCanClose   : boolean;
    FClosing    : boolean;

    FLastTabID  : cardinal; // Used by NextTabID to generate unique tab IDs

    {$IFDEF DELPHI17_UP}
    FMouseWheelService : IFMXMouseService;
    {$ENDIF}

    function  GetNextTabID : cardinal;
    function  CloseAllTabs : boolean;
    procedure CloseSelectedTab;

    property  NextTabID : cardinal   read GetNextTabID;
  public
    procedure InitializeUserInterface;
    procedure DestroyTab(aTabID : cardinal);
    procedure ResizeBrowser(aTabID : cardinal);
    procedure SendCaptureLostEvent;
    procedure NotifyMoveOrResizeStarted;
    function  GetMousePosition(var aPoint : TPointF) : boolean;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
    {$IFDEF MSWINDOWS}
    procedure HandleSYSCHAR(const aMessage : TMsg);
    procedure HandleSYSKEYDOWN(const aMessage : TMsg);
    procedure HandleSYSKEYUP(const aMessage : TMsg);
    procedure HandleKEYDOWN(const aMessage : TMsg);
    procedure HandleKEYUP(const aMessage : TMsg);
    function  HandlePOINTER(const aMessage : TMsg) : boolean;
    function  PostCustomMessage(aMsg : cardinal; aWParam : WPARAM = 0; aLParam : LPARAM = 0) : boolean;
    {$ENDIF}
  end;

var
  MainForm: TMainForm;

procedure CreateGlobalCEFApp;

implementation

{$R *.fmx}

uses
  FMX.Platform, FMX.Platform.Win,
  uCEFMiscFunctions, uCEFApplication, uCEFConstants, uBrowserTab,
  uFMXApplicationService;

// This demo shows how to use a TTabControl with TFrames that include
// CEF4Delphi browsers in OSR mode and using a message pump.

// Instead of a regular TTabItem we use a custom TBrowserTab class that
// inherits from TTabItem and instead of a regular TFrame we use a custom
// TBrowserFrame class that inherits from TFrame.

// To create a new tab you need to call TBrowserTab.CreateBrowser in the last
// step to create all the browser components and initialize the browser.

// To close a tab you have to call TBrowserTab.CloseBrowser and wait for a
// CEF_DESTROYTAB message that includes TBrowserTab.TabID in TMessage.wParam.
// Then you find the tab with that unique TabID and you free it.

// TBrowserFrame has all the usual code to close CEF4Delphi browsers following
// a similar destruction sequence than the FMXExternalPumpBrowser demo :
//
// 1- TChromium.CloseBrowser(True) will trigger TChromium.OnClose and the default
//    implementation will destroy the internal browser immediately, which will
//    trigger the TChromium.OnBeforeClose event.
// 2- TChromium.OnBeforeClose sends a CEF_DESTROYTAB message to the main form
//    to free the tab.

// To close safely this demo you must close all the browser tabs first following
// this steps :
//
// 1. FormCloseQuery sets CanClose to FALSE and calls CloseAllTabs and FClosing
//    is set to TRUE.
// 2. Each tab will send a CEF_DESTROYTAB message to free that tab.
// 3. When TTabControl has no tabs then we can set FCanClose to TRUE and close
//    the main form.

procedure GlobalCEFApp_OnContextInitialized;
begin
  {$IFDEF MSWINDOWS}
  if (MainForm <> nil) then
    MainForm.PostCustomMessage(CEF_INITIALIZED);
  {$ENDIF}
end;

procedure GlobalCEFApp_OnScheduleMessagePumpWork(const aDelayMS : int64);
begin
  if (GlobalFMXWorkScheduler <> nil) then
    GlobalFMXWorkScheduler.ScheduleMessagePumpWork(aDelayMS);
end;

procedure CreateGlobalCEFApp;
begin
  GlobalCEFApp                            := TCefApplication.Create;
  GlobalCEFApp.WindowlessRenderingEnabled := True;
  GlobalCEFApp.ExternalMessagePump        := True;
  GlobalCEFApp.MultiThreadedMessageLoop   := False;
  GlobalCEFApp.cache                      := 'cache';
  GlobalCEFApp.OnScheduleMessagePumpWork  := GlobalCEFApp_OnScheduleMessagePumpWork;
  GlobalCEFApp.OnContextInitialized       := GlobalCEFApp_OnContextInitialized;
  {$IFDEF DEBUG}
  //GlobalCEFApp.LogFile                    := 'debug.log';
  //GlobalCEFApp.LogSeverity                := LOGSEVERITY_INFO;
  {$ENDIF}

  // TFMXWorkScheduler will call cef_do_message_loop_work when
  // it's told in the GlobalCEFApp.OnScheduleMessagePumpWork event.
  // GlobalFMXWorkScheduler needs to be created before the
  // GlobalCEFApp.StartMainProcess call.
  GlobalFMXWorkScheduler := TFMXWorkScheduler.Create(nil);
end;

procedure TMainForm.NotifyMoveOrResizeStarted;
var
  i : integer;
begin
  if (BrowserTabCtrl = nil) then exit;

  i := pred(BrowserTabCtrl.TabCount);

  while (i >= 0) do
    begin
      TBrowserTab(BrowserTabCtrl.Tabs[i]).NotifyMoveOrResizeStarted;
      dec(i);
    end;
end;

function TMainForm.GetMousePosition(var aPoint : TPointF) : boolean;
begin
  {$IFDEF DELPHI17_UP}
  if (FMouseWheelService <> nil) then
    begin
      aPoint := FMouseWheelService.GetMousePos;
      Result := True;
    end
   else
    begin
      aPoint.x := 0;
      aPoint.y := 0;
      Result   := False;
    end;
  {$ELSE}
  TempPointF := Platform.GetMousePos;
  Result     := True;
  {$ENDIF}
end;

procedure TMainForm.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  PositionChanged: Boolean;
begin
  PositionChanged := (ALeft <> Left) or (ATop <> Top);

  inherited SetBounds(ALeft, ATop, AWidth, AHeight);

  if PositionChanged then
    NotifyMoveOrResizeStarted;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FCanClose;

  if not(FClosing) then
    begin
      FClosing          := True;
      ButtonLay.Enabled := False;

      if not(CloseAllTabs) then CanClose := True;
    end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  TFMXApplicationService.AddPlatformService;

  FCanClose   := False;
  FClosing    := False;
  FLastTabID  := 0;

  {$IFDEF DELPHI17_UP}
  if TPlatformServices.Current.SupportsPlatformService(IFMXMouseService) then
    FMouseWheelService := TPlatformServices.Current.GetPlatformService(IFMXMouseService) as IFMXMouseService;
  {$ENDIF}
end;

procedure TMainForm.FormResize(Sender: TObject);
var
  i : integer;
begin
  i := pred(BrowserTabCtrl.TabCount);

  while (i >= 0) do
    begin
      TBrowserTab(BrowserTabCtrl.Tabs[i]).ResizeBrowser;
      dec(i);
    end;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.GlobalContextInitialized then
    InitializeUserInterface;
end;

procedure TMainForm.InitializeUserInterface;
begin
  if not(ButtonLay.Enabled) then
    begin
      ButtonLay.Enabled := True;
      Caption           := 'FMX Tabbed OSR Browser';
      cursor            := crDefault;
      if (BrowserTabCtrl.TabCount <= 0) then AddTabAction.Execute;
    end;
end;

function TMainForm.GetNextTabID : cardinal;
begin
  inc(FLastTabID);
  Result := FLastTabID;
end;

procedure TMainForm.AddTabActionExecute(Sender: TObject);
var
  TempNewTab : TBrowserTab;
begin
  TempNewTab        := TBrowserTab.Create(BrowserTabCtrl, NextTabID, DEFAULT_TAB_CAPTION);
  TempNewTab.Parent := BrowserTabCtrl;

  BrowserTabCtrl.TabIndex := pred(BrowserTabCtrl.TabCount);

  TempNewTab.CreateBrowser(HOMEPAGE_URL);
end;

procedure TMainForm.RemoveTabActionExecute(Sender: TObject);
begin
  CloseSelectedTab;
end;

procedure TMainForm.DestroyTab(aTabID : cardinal);
var
  i : integer;
  TempText : string;
begin
  i := pred(BrowserTabCtrl.TabCount);

  while (i >= 0) do
    if (TBrowserTab(BrowserTabCtrl.Tabs[i]).TabID = aTabID) then
      begin
        BrowserTabCtrl.Delete(i);
        break;
      end
     else
      dec(i);

  if FClosing then
    begin
      if (BrowserTabCtrl.TabCount <= 0) then
        begin
          FCanClose := True;
          Close;
        end;
    end
   else
    begin
      // Sometimes TTabControl doesn't draw the new selected tab correctly.
      // Changing TTabItem.Text forces the component to redraw all the tabs.
      // A nicer solution would be to use a custom ttabcontrol that publishes
      // the TTabControl.RealignTabs procedure.
      if (BrowserTabCtrl.ActiveTab <> nil) then
        begin
          TempText := BrowserTabCtrl.ActiveTab.Text;
          BrowserTabCtrl.ActiveTab.Text := TempText + ' ';
          BrowserTabCtrl.ActiveTab.Text := TempText;
        end;
    end;
end;

procedure TMainForm.BrowserTabCtrlChange(Sender: TObject);
var
  i : integer;
  TempTab : TBrowserTab;
begin
  i := pred(BrowserTabCtrl.TabCount);

  while (i >= 0) do
    begin
      TempTab := TBrowserTab(BrowserTabCtrl.Tabs[i]);

      if (BrowserTabCtrl.TabIndex = i) then
        TempTab.ShowBrowser
       else
        TempTab.HideBrowser;

      dec(i);
    end;
end;

function TMainForm.CloseAllTabs : boolean;
var
  i : integer;
begin
  Result := False;
  i      := pred(BrowserTabCtrl.TabCount);

  while (i >= 0) do
    begin
      TBrowserTab(BrowserTabCtrl.Tabs[i]).CloseBrowser;
      Result := True;
      dec(i);
    end;
end;

procedure TMainForm.ResizeBrowser(aTabID : cardinal);
var
  i : integer;
begin
  i := pred(BrowserTabCtrl.TabCount);

  while (i >= 0) do
    if (TBrowserTab(BrowserTabCtrl.Tabs[i]).TabID = aTabID) then
      begin
        TBrowserTab(BrowserTabCtrl.Tabs[i]).ResizeBrowser;
        break;
      end
     else
      dec(i);
end;

procedure TMainForm.CloseSelectedTab;
begin
  if (BrowserTabCtrl.ActiveTab <> nil) then
    TBrowserTab(BrowserTabCtrl.ActiveTab).CloseBrowser;
end;

procedure TMainForm.SendCaptureLostEvent;
begin
  if (BrowserTabCtrl.ActiveTab <> nil) then
    TBrowserTab(BrowserTabCtrl.ActiveTab).SendCaptureLostEvent;
end;

procedure TMainForm.NextTabActionExecute(Sender: TObject);
begin
  if (BrowserTabCtrl.TabIndex < pred(BrowserTabCtrl.TabCount)) then
    BrowserTabCtrl.TabIndex := BrowserTabCtrl.TabIndex + 1;
end;

procedure TMainForm.PrevTabActionExecute(Sender: TObject);
begin
  if (BrowserTabCtrl.TabIndex > 0) then
    BrowserTabCtrl.TabIndex := BrowserTabCtrl.TabIndex - 1;
end;

procedure TMainForm.ShowTabsActionExecute(Sender: TObject);
begin
  if (BrowserTabCtrl.TabPosition = TTabPosition.PlatformDefault) then
    BrowserTabCtrl.TabPosition := TTabPosition.None
   else
    BrowserTabCtrl.TabPosition := TTabPosition.PlatformDefault;
end;

{$IFDEF MSWINDOWS}
procedure TMainForm.HandleSYSCHAR(const aMessage : TMsg);
begin
  if (BrowserTabCtrl.ActiveTab <> nil) then
    TBrowserTab(BrowserTabCtrl.ActiveTab).HandleSYSCHAR(aMessage);
end;

procedure TMainForm.HandleSYSKEYDOWN(const aMessage : TMsg);
begin
  if (BrowserTabCtrl.ActiveTab <> nil) then
    TBrowserTab(BrowserTabCtrl.ActiveTab).HandleSYSKEYDOWN(aMessage);
end;

procedure TMainForm.HandleSYSKEYUP(const aMessage : TMsg);
begin
  if (BrowserTabCtrl.ActiveTab <> nil) then
    TBrowserTab(BrowserTabCtrl.ActiveTab).HandleSYSKEYUP(aMessage);
end;

procedure TMainForm.HandleKEYDOWN(const aMessage : TMsg);
begin
  if (BrowserTabCtrl.ActiveTab <> nil) then
    TBrowserTab(BrowserTabCtrl.ActiveTab).HandleKEYDOWN(aMessage);
end;

procedure TMainForm.HandleKEYUP(const aMessage : TMsg);
begin
  if (BrowserTabCtrl.ActiveTab <> nil) then
    TBrowserTab(BrowserTabCtrl.ActiveTab).HandleKEYUP(aMessage);
end;

function TMainForm.HandlePOINTER(const aMessage : TMsg) : boolean;
begin
  Result := (BrowserTabCtrl.ActiveTab <> nil) and
            TBrowserTab(BrowserTabCtrl.ActiveTab).HandlePOINTER(aMessage);
end;

function TMainForm.PostCustomMessage(aMsg : cardinal; aWParam : WPARAM; aLParam : LPARAM) : boolean;
var
  TempHWND : HWND;
begin
  TempHWND := FmxHandleToHWND(Handle);
  Result   := (TempHWND <> 0) and WinApi.Windows.PostMessage(TempHWND, aMsg, aWParam, aLParam);
end;
{$ENDIF}

end.
