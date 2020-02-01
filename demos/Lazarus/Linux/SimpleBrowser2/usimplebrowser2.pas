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
//        Copyright © 2020 Salvador Diaz Fau. All rights reserved.
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

unit uSimpleBrowser2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  uCEFChromium, uCEFWindowParent, uCEFConstants, uCEFTypes, uCEFInterfaces,
  uCEFChromiumEvents;

type

  { TForm1 }

  TForm1 = class(TForm)
    AddressEdt: TEdit;
    GoBtn: TButton;
    CEFWindowParent1: TCEFWindowParent;
    Chromium1: TChromium;
    AddressPnl: TPanel;
    Timer1: TTimer;
    procedure Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser
      );
    procedure Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1BeforePopup(Sender: TObject; const browser: ICefBrowser;
      const frame: ICefFrame; const targetUrl, targetFrameName: ustring;
      targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean;
      const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
      var client: ICefClient; var settings: TCefBrowserSettings;
      var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean;
      var Result: Boolean);
    procedure Chromium1Close(Sender: TObject; const browser: ICefBrowser;
      var aAction: TCefCloseBrowserAction);
    procedure Chromium1OpenUrlFromTab(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame;
      const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition;
      userGesture: Boolean; out Result: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GoBtnClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
                 
  protected
    // Variables to control when can we destroy the form safely
    FCanClose  : boolean;  // Set to True in TChromium.OnBeforeClose
    FClosing   : boolean;  // Set to True in the CloseQuery event.

    function  SendCompMessage(aMsg : cardinal) : boolean;

    procedure BrowserCreated;
    procedure BrowserDestroyMsg(Data: PtrInt);
    procedure BrowserCloseFormMsg(Data: PtrInt);
  public

  end;

var
  Form1: TForm1;

procedure CreateGlobalCEFApp;

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

// This demo uses a TChromium and a TCEFWindowParent

// Destruction steps
// =================
// 1. FormCloseQuery sets CanClose to FALSE calls TChromium.CloseBrowser which triggers the TChromium.OnClose event.
// 2. TChromium.OnClose sends a CEFBROWSER_DESTROY message to destroy CEFWindowParent1 in the main thread, which triggers the TChromium.OnBeforeClose event.
// 3. TChromium.OnBeforeClose sets FCanClose := True and sends CEF_BEFORECLOSE to close the form.

uses
  uCEFApplication;

{ TForm1 }


procedure CreateGlobalCEFApp;
var
  TempHome, TempBinDir : ustring;
begin
  TempHome     := IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME'));
  TempBinDir   := TempHome + 'Lazarus/CEF4Delphi/bin';

  GlobalCEFApp               := TCefApplication.Create;
  GlobalCEFApp.SetCurrentDir := True;

  if DirectoryExists(TempBinDir) then
    begin
      GlobalCEFApp.FrameworkDirPath := TempBinDir;
      GlobalCEFApp.ResourcesDirPath := TempBinDir;
      GlobalCEFApp.LocalesDirPath   := TempBinDir + '/locales';
    end;

  // Add a debug log in the BIN directory
  GlobalCEFApp.LogFile     := 'cef.log';
  GlobalCEFApp.LogSeverity := LOGSEVERITY_VERBOSE;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FCanClose   := False;
  FClosing    := False;
end;

procedure TForm1.FormShow(Sender: TObject);
begin                                             
  Chromium1.DefaultURL := UTF8Decode(AddressEdt.Text);

  // You *MUST* call CreateBrowser to create and initialize the browser.
  // This will trigger the AfterCreated event when the browser is fully
  // initialized and ready to receive commands.

  // GlobalCEFApp.GlobalContextInitialized has to be TRUE before creating any browser
  // If it's not initialized yet, we use a simple timer to create the browser later.
  if not(Chromium1.CreateBrowser(CEFWindowParent1)) then Timer1.Enabled := True;
end;

procedure TForm1.GoBtnClick(Sender: TObject);
begin
  // This will load the URL in the edit box
  Chromium1.LoadURL(UTF8Decode(AddressEdt.Text));
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  if not(Chromium1.CreateBrowser(CEFWindowParent1)) and not(Chromium1.Initialized) then
    Timer1.Enabled := True;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := FCanClose;

  if not(FClosing) then
    begin
      FClosing := True;
      Visible  := False;
      Chromium1.CloseBrowser(True);
    end;
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
  Result := (targetDisposition in [WOD_NEW_FOREGROUND_TAB, WOD_NEW_BACKGROUND_TAB, WOD_NEW_POPUP, WOD_NEW_WINDOW]);
end;

procedure TForm1.Chromium1Close(Sender: TObject; const browser: ICefBrowser;
  var aAction: TCefCloseBrowserAction);
begin
  SendCompMessage(CEF_DESTROY);
  aAction := cbaDelay;
end;

procedure TForm1.Chromium1AfterCreated(Sender: TObject;
  const browser: ICefBrowser);
begin
  // Now the browser is fully initialized we can initialize the UI.
  TThread.Queue(nil, @BrowserCreated);
end;

procedure TForm1.BrowserCreated;
begin
  Caption            := 'Simple Browser 2';
  AddressPnl.Enabled := True;
end;

procedure TForm1.Chromium1BeforeClose(Sender: TObject;
  const browser: ICefBrowser);
begin
  FCanClose := True;
  SendCompMessage(CEF_BEFORECLOSE);
end;

procedure TForm1.Chromium1OpenUrlFromTab(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring;
  targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; out
  Result: Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [WOD_NEW_FOREGROUND_TAB, WOD_NEW_BACKGROUND_TAB, WOD_NEW_POPUP, WOD_NEW_WINDOW]);
end;

procedure TForm1.BrowserDestroyMsg(Data: PtrInt);
begin
  CEFWindowParent1.Free;
end;

procedure TForm1.BrowserCloseFormMsg(Data: PtrInt);
begin
  Close;
end;

function TForm1.SendCompMessage(aMsg : cardinal) : boolean;
begin
  case aMsg of
    CEF_DESTROY      : Application.QueueAsyncCall(@BrowserDestroyMsg, 0);
    CEF_BEFORECLOSE  : Application.QueueAsyncCall(@BrowserCloseFormMsg, 0);
  end;
end;

end.

