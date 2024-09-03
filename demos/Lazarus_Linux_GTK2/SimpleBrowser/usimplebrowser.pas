unit uSimpleBrowser;

{$mode objfpc}{$H+}   

{$I ../../../source/cef.inc}

interface

uses                                     
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, LMessages,
  uCEFConstants, uCEFTypes, uCEFInterfaces, uCEFChromiumEvents, uCEFChromiumWindow;

type
  { TForm1 }
  TForm1 = class(TForm)
    AddressEdt: TEdit;
    AddressPnl: TPanel;
    ChromiumWindow1: TChromiumWindow;
    GoBtn: TButton;
    Timer1: TTimer;

    procedure GoBtnClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);

    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);

    procedure ChromiumWindow1AfterCreated(Sender: TObject);
    procedure ChromiumWindow1BeforeClose(Sender: TObject);     
  private

  protected
    // Variables to control when can we destroy the form safely
    FCanClose : boolean;  // Set to True in TChromium.OnBeforeClose
    FClosing  : boolean;  // Set to True in the CloseQuery event.

    // CEF needs to handle these messages to call TChromium.NotifyMoveOrResizeStarted
    procedure WMMove(var Message: TLMMove); message LM_MOVE;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    procedure WMWindowPosChanged(var Message: TLMWindowPosChanged); message LM_WINDOWPOSCHANGED;

    procedure BrowserCloseMainForm(Data: PtrInt);
    procedure Chromium_OnBeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean; var Result: Boolean);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}   

uses
  uCEFApplication;

// This is a demo with the simplest web browser you can build using CEF4Delphi and
// it doesn't show any sign of progress like other web browsers do.

// Remember that it may take a few seconds to load if Windows update, your antivirus or
// any other windows service is using your hard drive.

// Depending on your internet connection it may take longer than expected.

// This demo uses a TChromiumWindow component which should *ONLY* be used for extremely
// simple applications with a simple browser. For any other configuration it's
// recommended using a TChromium with a TCEFWindowParent as shown in the SimpleBrowser2 demo.

// Please check that your firewall or antivirus are not blocking this application
// or the domain "google.com". If you don't live in the US, you'll be redirected to
// another domain which will take a little time too.

// Destruction steps
// =================
// 1. The FormCloseQuery event sets CanClose to False and calls TChromiumWindow.CloseBrowser,
//    which triggers the TChromiumWindow.OnBeforeClose event.
// 2. The TChromiumWindow.OnBeforeClose sets FCanClose to true and closes the
//    the form in the main application thread.

{ TForm1 }          

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := FCanClose;

  if not(FClosing) then
    begin
      FClosing := True;
      Visible  := False;
      ChromiumWindow1.CloseBrowser(True);
    end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FCanClose := False;
  FClosing  := False;

  // The browser will load the URL in AddressEdt initially.
  ChromiumWindow1.ChromiumBrowser.DefaultURL   := UTF8Decode(AddressEdt.Text);
  ChromiumWindow1.ChromiumBrowser.RuntimeStyle := CEF_RUNTIME_STYLE_ALLOY;
end;

procedure TForm1.Chromium_OnBeforePopup(Sender: TObject;
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

procedure TForm1.ChromiumWindow1AfterCreated(Sender: TObject);
begin
  Caption            := 'Simple Browser';
  AddressPnl.Enabled := True;
end;     

procedure TForm1.GoBtnClick(Sender: TObject);
begin
  // This will load the URL in the edit box
  ChromiumWindow1.LoadURL(UTF8Decode(AddressEdt.Text));
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  if ChromiumWindow1.Initialized then exit;

  // For simplicity, this demo blocks all popup windows and new tabs
  ChromiumWindow1.ChromiumBrowser.OnBeforePopup := @Chromium_OnBeforePopup;

  // You *MUST* call CreateBrowser to create and initialize the browser.
  // This will trigger the AfterCreated event when the browser is fully
  // initialized and ready to receive commands.

  // GlobalCEFApp.GlobalContextInitialized has to be TRUE before creating any browser
  // If it's not initialized yet, we use a simple timer to create the browser later.  

  // Linux needs a visible form to create a browser so we need to use the
  // TForm.OnActivate event instead of the TForm.OnShow event

  if not(ChromiumWindow1.CreateBrowser) then Timer1.Enabled := True;
end;

procedure TForm1.ChromiumWindow1BeforeClose(Sender: TObject);
begin                  
  // We must wait until all browsers trigger the TChromium.OnBeforeClose event
  // in order to close the application safely or we will have shutdown issues.
  FCanClose := True;
  Application.QueueAsyncCall(@BrowserCloseMainForm, 0);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  if not(ChromiumWindow1.CreateBrowser) and not(ChromiumWindow1.Initialized) then
    Timer1.Enabled := True;
end;

procedure TForm1.WMMove(var Message: TLMMove);
begin
  inherited;
  ChromiumWindow1.NotifyMoveOrResizeStarted;
end;

procedure TForm1.WMSize(var Message: TLMSize);
begin
  inherited;
  ChromiumWindow1.NotifyMoveOrResizeStarted;
end;

procedure TForm1.WMWindowPosChanged(var Message: TLMWindowPosChanged);
begin
  inherited;
  ChromiumWindow1.NotifyMoveOrResizeStarted;
end;

procedure TForm1.BrowserCloseMainForm(Data: PtrInt);
begin
  Close;
end;

end.

