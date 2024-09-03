unit uMainForm;

{$I ..\..\..\source\cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  {$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  {$ENDIF}
  // CEF4Delphi units
  uCEFChromium, uCEFWindowParent, uCEFInterfaces, uCEFConstants, uCEFTypes,
  uCEFWinControl, uCEFChromiumCore,
  // WebView4Delphi units
  uWVBrowser, uWVWinControl, uWVWindowParent, uWVTypes, uWVConstants,
  uWVTypeLibrary, uWVLibFunctions, uWVLoader, uWVInterfaces, uWVCoreWebView2Args,
  uWVBrowserBase;

type
  TMainForm = class(TForm)
    CEFAddressPnl: TPanel;
    CEFAddressEdt: TEdit;
    CEFGoBtn: TButton;
    Chromium1: TChromium;
    CEFWindowParent1: TCEFWindowParent;
    CEFPnl: TPanel;
    Splitter1: TSplitter;
    WVPnl: TPanel;
    WVAddressPnl: TPanel;
    WVAddressEdt: TEdit;
    WVGoBtn: TButton;
    WVWindowParent1: TWVWindowParent;
    WVBrowser1: TWVBrowser;

    procedure CEFGoBtnClick(Sender: TObject);
    procedure WVGoBtnClick(Sender: TObject);

    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    procedure Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1BeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess, Result: Boolean);
    procedure Chromium1OpenUrlFromTab(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; out Result: Boolean);

    procedure WVBrowser1AfterCreated(Sender: TObject);

  protected
    // Variables to control when can we destroy the form safely
    FCanClose : boolean;  // Set to True in TChromium.OnBeforeClose
    FClosing  : boolean;  // Set to True in the CloseQuery event.

    // You have to handle this two messages to call NotifyMoveOrResizeStarted or some page elements will be misaligned.
    procedure WMMove(var aMessage : TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage : TMessage); message WM_MOVING;
    // You also have to handle these two messages to set GlobalCEFApp.OsmodalLoop
    procedure WMEnterMenuLoop(var aMessage: TMessage); message WM_ENTERMENULOOP;
    procedure WMExitMenuLoop(var aMessage: TMessage); message WM_EXITMENULOOP;

    procedure BrowserCreatedMsg(var aMessage : TMessage); message CEF_AFTERCREATED;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

procedure CreateGlobalCEFApp;

implementation

{$R *.dfm}

uses
  uCEFApplication, uCefMiscFunctions;

// This is a demo with the simplest web browser you can build using CEF4Delphi and WebView4Delphi
// it doesn't show any sign of progress like other web browsers do.

// Remember that it may take a few seconds to load if Windows update, your antivirus or
// any other windows service is using your hard drive.

// Depending on your internet connection it may take longer than expected.

// This demo requires declaring two environment variables in Tools -> Options... -> IDE -> Environment Variables
// * CEF4DELPHI must point to the root directory where the CEF4Delphi project is located.
//     For example, C:\Users\<my_username>\Documents\Embarcadero\Studio\Projects\CEF4Delphi
// * WEBVIEW4DELPHI must point to the root directory where the WebView4Delphi project is located.
//     For example, C:\Users\<my_username>\Documents\Embarcadero\Studio\Projects\WebView4Delphi
//
// See that the Search path in the project options use CEF4DELPHI and WEBVIEW4DELPHI to locate the sources of those projects.

// Destruction steps
// =================
// 1. FormCloseQuery sets CanClose to FALSE, destroys CEFWindowParent1 and calls TChromium.CloseBrowser which triggers the TChromium.OnBeforeClose event.
// 2. TChromium.OnBeforeClose sets FCanClose := True and sends WM_CLOSE to the form.

procedure CreateGlobalCEFApp;
begin
  GlobalCEFApp                            := TCefApplication.Create;
  GlobalCEFApp.cache                      := 'cache';
  GlobalCEFApp.EnablePrintPreview         := True;
  GlobalCEFApp.EnableGPU                  := True;
  //GlobalCEFApp.LogFile                    := 'cefdebug.log';
  //GlobalCEFApp.LogSeverity                := LOGSEVERITY_INFO;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if Chromium1.Initialized then
    begin
      CanClose := FCanClose;

      if not(FClosing) then
        begin
          FClosing := True;
          Visible  := False;
          Chromium1.CloseBrowser(True);
          CEFWindowParent1.Free;
        end;
    end
   else
    CanClose := True;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FCanClose := False;
  FClosing  := False;

  GlobalWebView2Loader                   := TWVLoader.Create(nil);
  GlobalWebView2Loader.UserDataFolder    := ExtractFileDir(Application.ExeName) + '\CustomCache';
  GlobalWebView2Loader.UseInternalLoader := True;
  //GlobalWebView2Loader.DebugLog          := dlEnabled;
  //GlobalWebView2Loader.DebugLogLevel     := dllInfo;
  GlobalWebView2Loader.StartWebView2;
end;

procedure TMainForm.Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  // Now the browser is fully initialized we can send a message to the main form
  // to load the initial web page.
  PostMessage(Handle, CEF_AFTERCREATED, 0, 0);
end;

procedure TMainForm.Chromium1BeforeClose(Sender: TObject;
  const browser: ICefBrowser);
begin
  FCanClose := True;
  PostMessage(Handle, WM_CLOSE, 0, 0);
end;

procedure TMainForm.Chromium1BeforePopup(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const targetUrl,
  targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition;
  userGesture: Boolean; const popupFeatures: TCefPopupFeatures;
  var windowInfo: TCefWindowInfo; var client: ICefClient;
  var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue;
  var noJavascriptAccess, Result: Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [CEF_WOD_NEW_FOREGROUND_TAB,
                                   CEF_WOD_NEW_BACKGROUND_TAB,
                                   CEF_WOD_NEW_POPUP,
                                   CEF_WOD_NEW_WINDOW]);
end;

procedure TMainForm.Chromium1OpenUrlFromTab(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition;
  userGesture: Boolean; out Result: Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [CEF_WOD_NEW_FOREGROUND_TAB,
                                   CEF_WOD_NEW_BACKGROUND_TAB,
                                   CEF_WOD_NEW_POPUP,
                                   CEF_WOD_NEW_WINDOW]);
end;

procedure TMainForm.BrowserCreatedMsg(var aMessage : TMessage);
begin
  CEFWindowParent1.UpdateSize;
end;

procedure TMainForm.CEFGoBtnClick(Sender: TObject);
begin
  if Chromium1.Initialized then
    Chromium1.LoadURL(CEFAddressEdt.Text)
   else
    begin
      Chromium1.DefaultUrl := CEFAddressEdt.Text;
      Chromium1.CreateBrowser(CEFWindowParent1);
    end;
end;

procedure TMainForm.WMMove(var aMessage : TWMMove);
begin
  inherited;

  if (Chromium1 <> nil) then
    Chromium1.NotifyMoveOrResizeStarted;

  if (WVBrowser1 <> nil) then
    WVBrowser1.NotifyParentWindowPositionChanged;
end;

procedure TMainForm.WMMoving(var aMessage : TMessage);
begin
  inherited;

  if (Chromium1 <> nil) then
    Chromium1.NotifyMoveOrResizeStarted;

  if (WVBrowser1 <> nil) then
    WVBrowser1.NotifyParentWindowPositionChanged;
end;

procedure TMainForm.WVBrowser1AfterCreated(Sender: TObject);
begin
  WVWindowParent1.UpdateSize;
end;

procedure TMainForm.WVGoBtnClick(Sender: TObject);
begin
  if WVBrowser1.Initialized then
    WVBrowser1.Navigate(WVAddressEdt.Text)
   else
    begin
      WVBrowser1.DefaultUrl := WVAddressEdt.Text;
      WVBrowser1.CreateBrowser(WVWindowParent1.Handle);
    end;
end;

procedure TMainForm.WMEnterMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then
    GlobalCEFApp.OsmodalLoop := True;
end;

procedure TMainForm.WMExitMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then
    GlobalCEFApp.OsmodalLoop := False;
end;

end.
