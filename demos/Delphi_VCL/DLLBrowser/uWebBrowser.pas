unit uWebBrowser;

{$I ..\..\..\source\cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  {$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ExtCtrls,
  {$ENDIF}
  uCEFApplication, uCEFWindowParent, uCEFChromiumWindow, uCEFInterfaces, uCEFTypes, uCEFConstants,
  uCEFWinControl, uCEFLinkedWinControlBase;

type
  TWebBrowserFrm = class(TForm)
    ChromiumWindow1: TChromiumWindow;
    Timer1: TTimer;
    procedure FormShow(Sender: TObject);
    procedure ChromiumWindow1AfterCreated(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure ChromiumWindow1BeforeClose(Sender: TObject);
    procedure ChromiumWindow1Close(Sender: TObject);
  private

    procedure Chromium_OnBeforePopup(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame; const targetUrl,
      targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition;
      userGesture: Boolean; const popupFeatures: TCefPopupFeatures;
      var windowInfo: TCefWindowInfo; var client: ICefClient;
      var settings: TCefBrowserSettings;
      var extra_info: ICefDictionaryValue;
      var noJavascriptAccess: Boolean;
      var Result: Boolean);

    // You have to handle this two messages to call NotifyMoveOrResizeStarted or some page elements will be misaligned.
    procedure WMMove(var aMessage : TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage : TMessage); message WM_MOVING;
    // You also have to handle these two messages to set GlobalCEFApp.OsmodalLoop
    procedure WMEnterMenuLoop(var aMessage: TMessage); message WM_ENTERMENULOOP;
    procedure WMExitMenuLoop(var aMessage: TMessage); message WM_EXITMENULOOP;
  protected
    // Variables to control when can we destroy the form safely
    FCanClose : boolean;  // Set to True in TChromium.OnBeforeClose
    FClosing  : boolean;  // Set to True in the CloseQuery event.
  end;

var
  WebBrowserFrm: TWebBrowserFrm;

implementation

{$R *.dfm}

// This is a demo with the simplest web browser you can build using CEF4Delphi and
// it doesn't show any sign of progress like other web browsers do.

// Remember that it may take a few seconds to load if Windows update, your antivirus or
// any other windows service is using your hard drive.

// Depending on your internet connection it may take longer than expected.

// Please check that your firewall or antivirus are not blocking this application
// or the domain "google.com". If you don't live in the US, you'll be redirected to
// another domain which will take a little time too.

// Destruction steps
// =================
// 1. The FormCloseQuery event sets CanClose to False and calls TChromiumWindow.CloseBrowser, which triggers the TChromiumWindow.OnClose event.
// 2. The TChromiumWindow.OnClose event calls TChromiumWindow.DestroyChildWindow which triggers the TChromiumWindow.OnBeforeClose event.
// 3. TChromiumWindow.OnBeforeClose sets FCanClose to True and closes the form.


procedure TWebBrowserFrm.ChromiumWindow1AfterCreated(Sender: TObject);
begin
  ChromiumWindow1.LoadURL('https://www.google.com');
end;

procedure TWebBrowserFrm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FCanClose;

  if not(FClosing) then
    begin
      FClosing := True;
      Visible  := False;
      ChromiumWindow1.CloseBrowser(True);
    end;
end;

procedure TWebBrowserFrm.FormCreate(Sender: TObject);
begin
  FCanClose := False;
  FClosing  := False;
end;

procedure TWebBrowserFrm.FormShow(Sender: TObject);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  ChromiumWindow1.ChromiumBrowser.OnBeforePopup := Chromium_OnBeforePopup;

  // You *MUST* call CreateBrowser to create and initialize the browser.
  // This will trigger the AfterCreated event when the browser is fully
  // initialized and ready to receive commands.

  // GlobalCEFApp.GlobalContextInitialized has to be TRUE before creating any browser
  // If it's not initialized yet, we use a simple timer to create the browser later.
  if not(ChromiumWindow1.CreateBrowser) then Timer1.Enabled := True;
end;

procedure TWebBrowserFrm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  if not(ChromiumWindow1.CreateBrowser) and not(ChromiumWindow1.Initialized) then
    Timer1.Enabled := True;
end;

procedure TWebBrowserFrm.ChromiumWindow1BeforeClose(Sender: TObject);
begin
  FCanClose := True;
  PostMessage(Handle, WM_CLOSE, 0, 0);
end;

procedure TWebBrowserFrm.ChromiumWindow1Close(Sender: TObject);
begin
  // DestroyChildWindow will destroy the child window created by CEF at the top of the Z order.
  if not(ChromiumWindow1.DestroyChildWindow) then
    begin
      FCanClose := True;
      Close;
    end;
end;

procedure TWebBrowserFrm.Chromium_OnBeforePopup(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const targetUrl,
  targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition;
  userGesture: Boolean; const popupFeatures: TCefPopupFeatures;
  var windowInfo: TCefWindowInfo; var client: ICefClient;
  var settings: TCefBrowserSettings;
  var extra_info: ICefDictionaryValue;
  var noJavascriptAccess: Boolean;
  var Result: Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [CEF_WOD_NEW_FOREGROUND_TAB, CEF_WOD_NEW_BACKGROUND_TAB, CEF_WOD_NEW_POPUP, CEF_WOD_NEW_WINDOW]);
end;

procedure TWebBrowserFrm.WMMove(var aMessage : TWMMove);
begin
  inherited;

  if (ChromiumWindow1 <> nil) then ChromiumWindow1.NotifyMoveOrResizeStarted;
end;

procedure TWebBrowserFrm.WMMoving(var aMessage : TMessage);
begin
  inherited;

  if (ChromiumWindow1 <> nil) then ChromiumWindow1.NotifyMoveOrResizeStarted;
end;

procedure TWebBrowserFrm.WMEnterMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := True;
end;

procedure TWebBrowserFrm.WMExitMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := False;
end;

end.
