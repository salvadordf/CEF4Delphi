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

unit uHelloWorldExt;

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  {$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  {$ENDIF}
  uCEFChromium, uCEFWindowParent, uCEFInterfaces, uCEFConstants, uCEFTypes,
  uCEFWinControl, uCEFSentinel, uCEFChromiumCore;

const
  CEF_EXT_LOADED         = WM_APP + $B01;
  CEF_EXT_UNLOADED       = WM_APP + $B02;
  CEF_EXT_ERROR          = WM_APP + $B03;
  CEF_EXT_POPUP_LOADED   = WM_APP + $B04;
  CEF_EXT_POPUP_ERROR    = WM_APP + $B05;

  DESTROY_MAIN_WINDOWPARENT = 1;
  DESTROY_EXT_WINDOWPARENT  = 2;

type
  TForm1 = class(TForm)
    AddressPnl: TPanel;
    AddressEdt: TEdit;
    GoBtn: TButton;
    Timer1: TTimer;
    Chromium1: TChromium;
    CEFWindowParent1: TCEFWindowParent;
    ExtensionChr: TChromium;
    ExtensionPnl: TPanel;
    ExtensionMem: TMemo;
    Panel1: TPanel;
    LoadExtensionBtn: TButton;
    UnloadExtensionBtn: TButton;
    CEFWindowParent2: TCEFWindowParent;
    LoadPopupPageBtn: TButton;

    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    procedure Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1Close(Sender: TObject; const browser: ICefBrowser; var aAction : TCefCloseBrowserAction);
    procedure Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1BeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess, Result: Boolean);
    procedure Chromium1OpenUrlFromTab(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; out Result: Boolean);

    procedure ExtensionChrExtensionLoaded(Sender: TObject; const extension: ICefExtension);
    procedure ExtensionChrExtensionLoadFailed(Sender: TObject; result: Integer);
    procedure ExtensionChrExtensionUnloaded(Sender: TObject; const extension: ICefExtension);
    procedure ExtensionChrLoadEnd(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: Integer);
    procedure ExtensionChrLoadError(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; errorCode: Integer; const errorText, failedUrl: ustring);
    procedure ExtensionChrClose(Sender: TObject; const browser: ICefBrowser; var aAction: TCefCloseBrowserAction);
    procedure ExtensionChrBeforeClose(Sender: TObject; const browser: ICefBrowser);

    procedure GoBtnClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure LoadExtensionBtnClick(Sender: TObject);
    procedure UnloadExtensionBtnClick(Sender: TObject);
    procedure LoadPopupPageBtnClick(Sender: TObject);

  protected
    FCanClose  : boolean;
    FClosing   : boolean;
    FExtension : ICefExtension;

    procedure WMMove(var aMessage : TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage : TMessage); message WM_MOVING;
    procedure WMEnterMenuLoop(var aMessage: TMessage); message WM_ENTERMENULOOP;
    procedure WMExitMenuLoop(var aMessage: TMessage); message WM_EXITMENULOOP;

    procedure BrowserCreatedMsg(var aMessage : TMessage); message CEF_AFTERCREATED;
    procedure BrowserDestroyMsg(var aMessage : TMessage); message CEF_DESTROY;
    procedure ExtensionLoadedMsg(var aMessage : TMessage); message CEF_EXT_LOADED;
    procedure ExtensionUnloadedMsg(var aMessage : TMessage); message CEF_EXT_UNLOADED;
    procedure ExtensionErrorMsg(var aMessage : TMessage); message CEF_EXT_ERROR;
    procedure ExtensionPopupLoadedMsg(var aMessage : TMessage); message CEF_EXT_POPUP_LOADED;
    procedure ExtensionPopupErrorMsg(var aMessage : TMessage); message CEF_EXT_POPUP_ERROR;

    procedure UpdateButtons;
  end;

var
  Form1: TForm1;

procedure CreateGlobalCEFApp;

implementation

{$R *.dfm}

uses
  uCEFApplication, uCefMiscFunctions, uCEFJson;

// This is a simple demo showing how to load, unload and show the popup page
// from a chrome extension.

// This demo only allows to load the extension once.

// The extension loaded is called "Hello World" and it's located in the
// CEF4Delphi\bin\hello directory. It only has a browser action popup html with
// the "Hello World" message and no other functionality.

// It's necessary to unload the extension before you close the form. Notice that
// both TChromium components must be properly destroyed before allowing the form
// to close.

// This demo is just a template for other demos and it has a browser loading
// google.com but in this case the extension doesn't use that browser.

// Load chrome://extensions-support/ to get a list of supported APIs

// Destruction steps
// =================
// 1. FormCloseQuery sets CanClose to FALSE calls TChromium.CloseBrowser in both
//    browsers which triggers the TChromium.OnClose event.
// 2. TChromium.OnClose sends a CEFBROWSER_DESTROY message to destroy
//    CEFWindowParent1 and CEFWindowParent2 in the main thread, which triggers
//    the TChromium.OnBeforeClose event.
// 3. TChromium.OnBeforeClose sets FCanClose := True and sends WM_CLOSE to the form.

procedure CreateGlobalCEFApp;
begin
  GlobalCEFApp               := TCefApplication.Create;
  GlobalCEFApp.LogFile       := 'debug.log';
  GlobalCEFApp.LogSeverity   := LOGSEVERITY_VERBOSE;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if (FExtension <> nil) then
    begin
      CanClose := False;
      showmessage('Unload the extension before closing this demo');
      exit;
    end;

  CanClose := FCanClose and
              (CEFWindowParent1 = nil) and
              ((CEFWindowParent2 = nil) or not(ExtensionChr.Initialized));

  if not(FClosing) then
    begin
      FClosing := True;
      Visible  := False;
      Chromium1.CloseBrowser(True);
      ExtensionChr.CloseBrowser(True);
    end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FCanClose  := False;
  FClosing   := False;
  FExtension := nil;
  Chromium1.DefaultURL := AddressEdt.Text;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FExtension := nil;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  if not(Chromium1.CreateBrowser(CEFWindowParent1)) then
    Timer1.Enabled := True;

  UpdateButtons;
end;

procedure TForm1.Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  PostMessage(Handle, CEF_AFTERCREATED, 0, 0);
end;

procedure TForm1.Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
begin
  FCanClose := True;
  PostMessage(Handle, WM_CLOSE, 0, 0);
end;

procedure TForm1.Chromium1BeforePopup(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const targetUrl,
  targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition;
  userGesture: Boolean; const popupFeatures: TCefPopupFeatures;
  var windowInfo: TCefWindowInfo; var client: ICefClient;
  var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue;
  var noJavascriptAccess, Result: Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [WOD_NEW_FOREGROUND_TAB, WOD_NEW_BACKGROUND_TAB, WOD_NEW_POPUP, WOD_NEW_WINDOW]);
end;

procedure TForm1.Chromium1Close(Sender: TObject;
  const browser: ICefBrowser; var aAction : TCefCloseBrowserAction);
begin
  PostMessage(Handle, CEF_DESTROY, DESTROY_MAIN_WINDOWPARENT, 0);
  aAction := cbaDelay;
end;

procedure TForm1.Chromium1OpenUrlFromTab(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition;
  userGesture: Boolean; out Result: Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [WOD_NEW_FOREGROUND_TAB, WOD_NEW_BACKGROUND_TAB, WOD_NEW_POPUP, WOD_NEW_WINDOW]);
end;

procedure TForm1.ExtensionChrBeforeClose(Sender: TObject;
  const browser: ICefBrowser);
begin
  FCanClose := True;
  PostMessage(Handle, WM_CLOSE, 0, 0);
end;

procedure TForm1.ExtensionChrClose(Sender: TObject; const browser: ICefBrowser;
  var aAction: TCefCloseBrowserAction);
begin
  PostMessage(Handle, CEF_DESTROY, DESTROY_EXT_WINDOWPARENT, 0);
  aAction := cbaDelay;
end;

procedure TForm1.ExtensionChrExtensionLoaded(Sender: TObject; const extension: ICefExtension);
begin
  FExtension := extension;

  PostMessage(Handle, CEF_EXT_LOADED, 0, 0);
end;

procedure TForm1.ExtensionChrExtensionLoadFailed(Sender: TObject; result: Integer);
begin
  PostMessage(Handle, CEF_EXT_ERROR, 0, result);
end;

procedure TForm1.ExtensionChrExtensionUnloaded(Sender: TObject; const extension: ICefExtension);
begin
  if (extension <> nil) and (FExtension <> nil) and extension.IsSame(FExtension) then
    begin
      FExtension := nil;
      PostMessage(Handle, CEF_EXT_UNLOADED, 0, 0);
    end;
end;

procedure TForm1.ExtensionChrLoadEnd(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: Integer);
begin
  PostMessage(Handle, CEF_EXT_POPUP_LOADED, 0, 0);
end;

procedure TForm1.ExtensionChrLoadError(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; errorCode: Integer;
  const errorText, failedUrl: ustring);
begin
  PostMessage(Handle, CEF_EXT_POPUP_ERROR, 0, errorCode);
end;

procedure TForm1.BrowserCreatedMsg(var aMessage : TMessage);
begin
  Caption            := 'Hello World Extension Demo';
  AddressPnl.Enabled := True;
end;

procedure TForm1.BrowserDestroyMsg(var aMessage : TMessage);
begin
  case aMessage.WParam of
    DESTROY_MAIN_WINDOWPARENT : FreeAndNil(CEFWindowParent1);
    DESTROY_EXT_WINDOWPARENT  : FreeAndNil(CEFWindowParent2);
  end;
end;

procedure TForm1.GoBtnClick(Sender: TObject);
begin
  Chromium1.LoadURL(AddressEdt.Text);
end;

procedure TForm1.UpdateButtons;
begin
  // This demo only allows to load the extension once.

  if ExtensionChr.Initialized then
    begin
      LoadExtensionBtn.Enabled   := False;
      UnloadExtensionBtn.Enabled := (FExtension <> nil);
      LoadPopupPageBtn.Enabled   := False;
    end
   else
    begin
      LoadExtensionBtn.Enabled   := (FExtension =  nil);
      UnloadExtensionBtn.Enabled := (FExtension <> nil);
      LoadPopupPageBtn.Enabled   := (FExtension <> nil);
    end;
end;

procedure TForm1.ExtensionLoadedMsg(var aMessage : TMessage);
var
  TempManifest : TStringList;
begin
  if (FExtension = nil) then exit;

  ExtensionMem.Lines.Add('--------------------------------');
  ExtensionMem.Lines.Add('Extension loaded successfully!');
  ExtensionMem.Lines.Add('Identifier: ' + FExtension.Identifier);
  ExtensionMem.Lines.Add('Path: ' + FExtension.Path);
  ExtensionMem.Lines.Add('IsLoaded: ' + BoolToStr(FExtension.IsLoaded, True));
  ExtensionMem.Lines.Add('Popup: ' + FExtension.BrowserActionPopup);
  ExtensionMem.Lines.Add('Icon: ' + FExtension.BrowserActionIcon);
  ExtensionMem.Lines.Add('URL: ' + FExtension.URL);

  TempManifest := TStringList.Create;

  if TCEFJson.Write(FExtension.Manifest, TempManifest) then
    begin
      ExtensionMem.Lines.Add('Manifest: ' + FExtension.Path);
      ExtensionMem.Lines.AddStrings(TempManifest);
    end;

  TempManifest.Free;

  UpdateButtons;
end;

procedure TForm1.ExtensionUnloadedMsg(var aMessage : TMessage);
begin
  ExtensionMem.Lines.Add('--------------------------------');
  ExtensionMem.Lines.Add('Extension unloaded successfully!');
  UpdateButtons;
end;

procedure TForm1.ExtensionErrorMsg(var aMessage : TMessage);
begin
  ExtensionMem.Lines.Add('--------------------------------');
  ExtensionMem.Lines.Add('Extension load failed. Result : ' + inttostr(aMessage.LParam));
  UpdateButtons;
end;

procedure TForm1.ExtensionPopupLoadedMsg(var aMessage : TMessage);
begin
  ExtensionMem.Lines.Add('--------------------------------');
  ExtensionMem.Lines.Add('Extension PopUp page loaded successfully!');
  UpdateButtons;
end;

procedure TForm1.ExtensionPopupErrorMsg(var aMessage : TMessage);
begin
  ExtensionMem.Lines.Add('--------------------------------');
  ExtensionMem.Lines.Add('Extension PopUp page load failed. Error code : ' + inttostr(aMessage.LParam));
  UpdateButtons;
end;

procedure TForm1.LoadExtensionBtnClick(Sender: TObject);
var
  TempExtensionDirectoryPath : ustring;
begin
  if (FExtension = nil) then
    begin
      TempExtensionDirectoryPath := GetModulePath + 'hello';
      ExtensionChr.LoadExtension(TempExtensionDirectoryPath);
    end;
end;

procedure TForm1.LoadPopupPageBtnClick(Sender: TObject);
begin
  if (FExtension = nil) or ExtensionChr.Initialized then exit;

  ExtensionChr.DefaultURL := FExtension.URL + FExtension.BrowserActionPopup;
  ExtensionChr.CreateBrowser(CEFWindowParent2);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  if not(Chromium1.CreateBrowser(CEFWindowParent1)) and not(Chromium1.Initialized) then
    Timer1.Enabled := True;
end;

procedure TForm1.UnloadExtensionBtnClick(Sender: TObject);
begin
  if (FExtension <> nil) then
    FExtension.Unload;
end;

procedure TForm1.WMMove(var aMessage : TWMMove);
begin
  inherited;

  if (Chromium1 <> nil) then Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TForm1.WMMoving(var aMessage : TMessage);
begin
  inherited;

  if (Chromium1 <> nil) then Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TForm1.WMEnterMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then
    GlobalCEFApp.OsmodalLoop := True;
end;

procedure TForm1.WMExitMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then
    GlobalCEFApp.OsmodalLoop := False;
end;

end.
