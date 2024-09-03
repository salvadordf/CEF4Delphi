unit uBrowserWindowEx;

{$mode objfpc}{$H+}
{$IFDEF MSWINDOWS}{$I ..\..\..\source\cef.inc}{$ELSE}{$I ../../../source/cef.inc}{$ENDIF}

interface

uses
  GlobalCefApplication,
  uCEFLazarusCocoa, // required for Cocoa
  SysUtils, Messages, Forms, Controls,
  Dialogs, ExtCtrls, StdCtrls, LMessages, Buttons,
  uCEFTypes, uCEFInterfaces,
  uCEFWorkScheduler, uCEFBrowserWindow, Classes;

type

  { TForm1 }

  TForm1 = class(TForm)
    AddressEdtLeft: TComboBox;
    AddressEdtRight: TComboBox;
    AddressPnlRight: TPanel;
    BtnCloseApp: TButton;
    BtnCloseForm: TButton;
    BtnModal: TButton;
    GoBtnLeft: TButton;
    AddressPnlLeft: TPanel;
    GoBtnRight: TButton;
    ImageList1: TImageList;
    Panel1: TPanel;
    PanelRight: TPanel;
    PanelLeft: TPanel;
    CloseBtnLeft: TSpeedButton;
    CloseBtnRight: TSpeedButton;
    OpenBtnLeft: TSpeedButton;
    OpenBtnRight: TSpeedButton;
    Splitter1: TSplitter;

    procedure BtnCloseAppClick(Sender: TObject);
    procedure BtnCloseFormClick(Sender: TObject);
    procedure BtnModalClick(Sender: TObject);
    procedure Chromium1BeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean; var Result: Boolean);
    procedure Chromium1OpenUrlFromTab(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; out Result: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);

    procedure OpenBtnLeftClick(Sender: TObject);
    procedure LeftBrowserCreated(Sender: TObject);
    procedure GoBtnLeftClick(Sender: TObject);
    procedure CloseBtnLeftClick(Sender: TObject);
    procedure LeftBrowserClosed(Sender: TObject);

    procedure OpenBtnRightClick(Sender: TObject);
    procedure RightBrowserCreated(Sender: TObject);
    procedure GoBtnRightClick(Sender: TObject);
    procedure CloseBtnRightClick(Sender: TObject);
    procedure RightBrowserClosed(Sender: TObject);

    procedure DoShowModal(Data: PtrInt);
    procedure MaybeTerminateApp(Sender: TObject);
    procedure MaybeCloseApp(Sender: TObject);
  protected
    FBrowserLeft, FBrowserRight: TBrowserWindow;
    FClosingBrowsers: TList;

    {$IFDEF WINDOWS}
    procedure WMEnterMenuLoop(var aMessage: TMessage); message WM_ENTERMENULOOP;
    procedure WMExitMenuLoop(var aMessage: TMessage); message WM_EXITMENULOOP;
    {$ENDIF}

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

  end;

var
  Form1: TForm1;


implementation

{$R *.lfm}

// This is a demo with the simplest web browser you can build using CEF4Delphi and
// it doesn't show any sign of progress like other web browsers do.

// Remember that it may take a few seconds to load if Windows update, your antivirus or
// any other windows service is using your hard drive.

// Depending on your internet connection it may take longer than expected.

// Please check that your firewall or antivirus are not blocking this application
// or the domain "google.com". If you don't live in the US, you'll be redirected to
// another domain which will take a little time too.

// This demo uses a TChromium and a TCEFLinkedWindowParent

// We need to use TCEFLinkedWindowParent in Linux to update the browser
// visibility and size automatically.

// Destruction steps
// =================
// 1. FormCloseQuery sets CanClose to FALSE calls TChromium.CloseBrowser which triggers the TChromium.OnClose event.
// 2. TChromium.OnClose sets aAction to cbaClose to destroy the browser, which triggers the TChromium.OnBeforeClose event.
// 3. TChromium.OnBeforeClose sets FCanClose := True and sends CEF_BEFORECLOSE to close the form.

uses
  uCEFApplication;

{ TForm1 }

procedure TForm1.OpenBtnLeftClick(Sender: TObject);
begin
  FBrowserLeft := TBrowserWindow.Create(Self);
  FBrowserLeft.Chromium.OnBeforePopup    := @Chromium1BeforePopup;
  FBrowserLeft.Chromium.OnOpenUrlFromTab := @Chromium1OpenUrlFromTab;
  FBrowserLeft.OnBrowserCreated := @LeftBrowserCreated;
  FBrowserLeft.OnBrowserClosed  := @LeftBrowserClosed;
  FBrowserLeft.Align  := alClient;
  FBrowserLeft.Parent := PanelLeft;
  FBrowserLeft.Chromium.RuntimeStyle := CEF_RUNTIME_STYLE_ALLOY;
  OpenBtnLeft.Enabled := False;
  GoBtnLeftClick(nil);
end;

procedure TForm1.LeftBrowserCreated(Sender: TObject);
begin
  AddressPnlLeft.Enabled := True;
end;

procedure TForm1.GoBtnLeftClick(Sender: TObject);
begin
  FBrowserLeft.LoadURL(UTF8Decode(AddressEdtLeft.Text));
end;

procedure TForm1.CloseBtnLeftClick(Sender: TObject);
begin
  AddressPnlLeft.Enabled := False;
  FClosingBrowsers.Add(FBrowserLeft);
  FBrowserLeft.CloseBrowser(True);
  FBrowserLeft := nil;
  //FreeAndNil(FBrowserLeft);
  OpenBtnLeft.Enabled := True;
end;

procedure TForm1.LeftBrowserClosed(Sender: TObject);
begin
  FClosingBrowsers.Remove(Sender);
  Sender.Free;
end;


procedure TForm1.OpenBtnRightClick(Sender: TObject);
begin
  FBrowserRight := TBrowserWindow.Create(Self);
  FBrowserRight.Chromium.OnBeforePopup    := @Chromium1BeforePopup;
  FBrowserRight.Chromium.OnOpenUrlFromTab := @Chromium1OpenUrlFromTab;
  FBrowserRight.OnBrowserCreated := @RightBrowserCreated;
  {$IFDEF MACOSX}
  FBrowserRight.OnBrowserClosed := @RightBrowserClosed;
  {$ENDIF}
  FBrowserRight.Align  := alClient;
  FBrowserRight.Parent := PanelRight;
  FBrowserRight.Chromium.RuntimeStyle := CEF_RUNTIME_STYLE_ALLOY;

  OpenBtnRight.Enabled := False;
  GoBtnRightClick(nil);
end;

procedure TForm1.RightBrowserCreated(Sender: TObject);
begin
  AddressPnlRight.Enabled := True;
end;

procedure TForm1.GoBtnRightClick(Sender: TObject);
begin
  FBrowserRight.LoadURL(UTF8Decode(AddressEdtRight.Text));
end;

procedure TForm1.CloseBtnRightClick(Sender: TObject);
begin
  AddressPnlRight.Enabled := False;
  {$IFDEF MACOSX}
  FClosingBrowsers.Add(FBrowserRight);
  FBrowserRight.CloseBrowser(True);
  FBrowserRight := nil;
  {$ELSE}
  FreeAndNil(FBrowserRight);
  {$ENDIF}
  OpenBtnRight.Enabled := True;
end;

procedure TForm1.RightBrowserClosed(Sender: TObject);
begin
  FClosingBrowsers.Remove(Sender);
  Sender.Free;
end;

{$IFDEF WINDOWS}
procedure TForm1.WMEnterMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := True;
end;

procedure TForm1.WMExitMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := False;
end;
{$ENDIF}

constructor TForm1.Create(TheOwner: TComponent);
begin
  FClosingBrowsers := TList.Create;
  inherited Create(TheOwner);
end;

destructor TForm1.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FClosingBrowsers);
end;

procedure TForm1.Chromium1BeforePopup(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const targetUrl,
  targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition;
  userGesture: Boolean; const popupFeatures: TCefPopupFeatures;
  var windowInfo: TCefWindowInfo; var client: ICefClient;
  var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue;
  var noJavascriptAccess: Boolean; var Result: Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [CEF_WOD_NEW_FOREGROUND_TAB, CEF_WOD_NEW_BACKGROUND_TAB, CEF_WOD_NEW_POPUP, CEF_WOD_NEW_WINDOW]);
end;

procedure TForm1.MaybeTerminateApp(Sender: TObject);
begin
  Sender.Free;
  FClosingBrowsers.Remove(Sender);
  if FClosingBrowsers.Count = 0 then
    Application.Terminate;
end;

procedure TForm1.BtnCloseAppClick(Sender: TObject);
begin
  // Does not call CloseQuery
  Hide;
  {$IFDEF MACOSX}
  (* This demo takes no precaution against the App being closed by outher means
     while waiting for all browsers to close
  *)
  if FBrowserLeft <> nil then begin
    FBrowserLeft.OnBrowserClosed := @MaybeTerminateApp;
    CloseBtnLeftClick(nil);
  end;
  if FBrowserRight <> nil then begin
    FBrowserRight.OnBrowserClosed := @MaybeTerminateApp;
    CloseBtnRightClick(nil);
  end;
  if FClosingBrowsers.Count = 0 then
    Application.Terminate;
  {$ELSE}
  if FBrowserLeft <> nil then
    FBrowserLeft.WaitForBrowserClosed;
  if FBrowserRight <> nil then
    FBrowserRight.WaitForBrowserClosed;
  Application.Terminate;
  {$ENDIF}
end;

procedure TForm1.BtnCloseFormClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.BtnModalClick(Sender: TObject);
begin
  {$IFDEF MACOSX}
  Application.QueueAsyncCall(@DoShowModal, 0);
  {$ELSE}
  DoShowModal(0);
  {$ENDIF}
end;

procedure TForm1.DoShowModal(Data: PtrInt);
var
  m: TForm1;
begin
  m := TForm1.Create(Application);
  m.BtnModal.Enabled := False;
  m.Caption := 'MOD';
  m.BtnCloseApp.Visible := False;
  m.ShowModal;
  m.Free;
end;

procedure TForm1.Chromium1OpenUrlFromTab(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring;
  targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; out
  Result: Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [CEF_WOD_NEW_FOREGROUND_TAB, CEF_WOD_NEW_BACKGROUND_TAB, CEF_WOD_NEW_POPUP, CEF_WOD_NEW_WINDOW]);
end;

procedure TForm1.MaybeCloseApp(Sender: TObject);
begin
  Sender.Free;
  FClosingBrowsers.Remove(Sender);
  if FClosingBrowsers.Count = 0 then
    Close;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  Hide;
  if FBrowserLeft <> nil then begin
    FBrowserLeft.OnBrowserClosed := @MaybeCloseApp;
    CloseBtnLeftClick(nil);
  end;
  if FBrowserRight <> nil then begin
    FBrowserRight.OnBrowserClosed := @MaybeCloseApp;
    CloseBtnRightClick(nil);
  end;
  CanClose := FClosingBrowsers.Count = 0;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  OpenBtnLeftClick(nil);
end;

initialization
  {$IFDEF DARWIN}  // $IFDEF MACOSX
  AddCrDelegate;
  {$ENDIF}
  if GlobalCEFApp = nil then begin
    CreateGlobalCEFApp;
    if not GlobalCEFApp.StartMainProcess then begin
      DestroyGlobalCEFApp;
      DestroyGlobalCEFWorkScheduler;
      halt(0); // exit the subprocess
    end;
  end;

finalization
  (* Destroy from this unit, which is used after "Interfaces". So this happens before the Application object is destroyed *)
  if GlobalCEFWorkScheduler <> nil then
    GlobalCEFWorkScheduler.StopScheduler;
  DestroyGlobalCEFApp;
  DestroyGlobalCEFWorkScheduler;

end.

