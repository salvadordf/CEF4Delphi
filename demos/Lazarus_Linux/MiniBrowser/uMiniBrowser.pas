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

unit uMiniBrowser;  

{$mode objfpc}{$H+}

{$I cef.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Menus, ComCtrls, SyncObjs, LMessages,
  uCEFChromium, uCEFWindowParent, uCEFInterfaces, uCEFApplication, uCEFTypes,
  uCEFConstants, uCEFWinControl, uCEFChromiumEvents, uCEFLinkedWindowParent;

type
  { TMiniBrowserFrm }
  TMiniBrowserFrm = class(TForm)
    CEFLinkedWindowParent1: TCEFLinkedWindowParent;
    HideDevTools1: TMenuItem;
    NavControlPnl: TPanel;
    NavButtonPnl: TPanel;
    StatusBar1: TStatusBar;
    URLEditPnl: TPanel;
    BackBtn: TButton;
    ForwardBtn: TButton;
    ReloadBtn: TButton;
    Chromium1: TChromium;
    StopBtn: TButton;
    URLCbx: TComboBox;
    ConfigPnl: TPanel;
    ConfigBtn: TButton;
    PopupMenu1: TPopupMenu;
    ShowDevTools1: TMenuItem;
    GoBtn: TButton;
    N2: TMenuItem;
    PrintinPDF1: TMenuItem;
    Print1: TMenuItem;
    N3: TMenuItem;
    Zoom1: TMenuItem;
    Inczoom1: TMenuItem;
    Deczoom1: TMenuItem;
    Resetzoom1: TMenuItem;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    N4: TMenuItem;
    Openfile1: TMenuItem;
    Timer1: TTimer;
    OpenfilewithaDAT1: TMenuItem;
                                                                            
    procedure Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1Close(Sender: TObject; const browser: ICefBrowser; var aAction : TCefCloseBrowserAction);
    procedure Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1BeforePluginLoad(Sender: TObject; const mimeType, pluginUrl: ustring; isMainFrame: boolean; const topOriginUrl: ustring; const pluginInfo: ICefWebPluginInfo; var pluginPolicy: TCefPluginPolicy; var aResult: boolean);
    procedure Chromium1GotFocus(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1LoadingStateChange(Sender: TObject; const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
    procedure Chromium1TitleChange(Sender: TObject; const browser: ICefBrowser; const title: ustring);
    procedure Chromium1AddressChange(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
    procedure Chromium1BeforeContextMenu(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; const model: ICefMenuModel);
    procedure Chromium1StatusMessage(Sender: TObject; const browser: ICefBrowser; const value: ustring);
    procedure Chromium1PreKeyEvent(Sender: TObject; const browser: ICefBrowser; const event: PCefKeyEvent; osEvent: TCefEventHandle; out isKeyboardShortcut, Result: Boolean);
    procedure Chromium1KeyEvent(Sender: TObject; const browser: ICefBrowser; const event: PCefKeyEvent; osEvent: TCefEventHandle; out Result: Boolean);
    procedure Chromium1ContextMenuCommand(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; commandId: Integer; eventFlags: Cardinal; out Result: Boolean);
    procedure Chromium1PdfPrintFinished(Sender: TObject; aResultOK: Boolean);
    procedure Chromium1LoadError(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; errorCode: Integer; const errorText, failedUrl: ustring);

    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);    
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
                                                 
    procedure CEFLinkedWindowParent1Enter(Sender: TObject);
    procedure CEFLinkedWindowParent1Exit(Sender: TObject);
    procedure HideDevTools1Click(Sender: TObject);

    procedure Timer1Timer(Sender: TObject);

    procedure BackBtnClick(Sender: TObject);
    procedure ForwardBtnClick(Sender: TObject); 
    procedure ReloadBtnClick(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
    procedure GoBtnClick(Sender: TObject);
    procedure ConfigBtnClick(Sender: TObject);

    procedure ShowDevTools1Click(Sender: TObject);
    procedure PrintinPDF1Click(Sender: TObject);
    procedure Print1Click(Sender: TObject);
    procedure Inczoom1Click(Sender: TObject);
    procedure Deczoom1Click(Sender: TObject);
    procedure Resetzoom1Click(Sender: TObject);
    procedure Openfile1Click(Sender: TObject);      
    procedure OpenfilewithaDAT1Click(Sender: TObject);

  protected
    // Variables to control when can we destroy the form safely
    FCanClose : boolean;  // Set to True in TChromium.OnBeforeClose
    FClosing  : boolean;  // Set to True in the CloseQuery event.

    FBrowserCS            : TCriticalSection;
    FBrowserAddress       : string;
    FBrowserIsLoading     : boolean;
    FBrowserCanGoBack     : boolean;
    FBrowserCanGoForward  : boolean;
    FBrowserStatusText    : string;
    FBrowserTitle         : string;

    procedure SetBrowserAddress(const aValue : string);
    procedure SetBrowserIsLoading(aValue : boolean);
    procedure SetBrowserCanGoBack(aValue : boolean);
    procedure SetBrowserCanGoForward(aValue : boolean);
    procedure SetBrowserStatusText(const aValue : string);
    procedure SetBrowserTitle(const aValue : string);

    function  GetBrowserAddress : string;
    function  GetBrowserIsLoading : boolean;
    function  GetBrowserCanGoBack : boolean;
    function  GetBrowserCanGoForward : boolean;
    function  GetBrowserStatusText : string;
    function  GetBrowserTitle : string;

    procedure ShowDevTools(aPoint : TPoint); overload;
    procedure ShowDevTools; overload;
    procedure HideDevTools;

    // CEF needs to handle these messages to call TChromium.NotifyMoveOrResizeStarted
    procedure WMMove(var Message: TLMMove); message LM_MOVE;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    procedure WMWindowPosChanged(var Message: TLMWindowPosChanged); message LM_WINDOWPOSCHANGED;

    procedure SendCompMessage(aMsg : cardinal; Data: PtrInt = 0);

    procedure BrowserCreatedMsg(Data: PtrInt);
    procedure BrowserCloseFormMsg(Data: PtrInt);
    procedure BrowserUpdateAddressMsg(Data: PtrInt);
    procedure BrowserUpdateLoadingStateMsg(Data: PtrInt);
    procedure BrowserUpdateStatusTextMsg(Data: PtrInt);
    procedure BrowserUpdateTitleMsg(Data: PtrInt);
    procedure BrowserShowDevToolsMsg(Data: PtrInt);
    procedure BrowserHideDevToolsMsg(Data: PtrInt);
    procedure BrowserPrintPDFEndMsg(Data: PtrInt);

    property  BrowserAddress       : string              read GetBrowserAddress      write SetBrowserAddress;
    property  BrowserIsLoading     : boolean             read GetBrowserIsLoading    write SetBrowserIsLoading;
    property  BrowserCanGoBack     : boolean             read GetBrowserCanGoBack    write SetBrowserCanGoBack;
    property  BrowserCanGoForward  : boolean             read GetBrowserCanGoForward write SetBrowserCanGoForward;
    property  BrowserStatusText    : string              read GetBrowserStatusText   write SetBrowserStatusText;
    property  BrowserTitle         : string              read GetBrowserTitle        write SetBrowserTitle;
  end;

var
  MiniBrowserFrm : TMiniBrowserFrm;

procedure CreateGlobalCEFApp;

implementation

{$R *.lfm}

uses
  uCEFMiscFunctions, uCefClient;

const
  CEF_UPDATEADDRESS      = 1;
  CEF_UPDATELOADINGSTATE = 2;
  CEF_UPDATESTATUSTEXT   = 3;
  CEF_UPDATETITLE        = 4;
  CEF_SHOWDEVTOOLS       = 5;
  CEF_HIDEDEVTOOLS       = 6;       
  CEF_PDFPRINTEND        = 7;

  MINIBROWSER_CONTEXTMENU_SHOWDEVTOOLS    = MENU_ID_USER_FIRST + 1;
  MINIBROWSER_CONTEXTMENU_HIDEDEVTOOLS    = MENU_ID_USER_FIRST + 2;

// Destruction steps
// =================
// 1. FormCloseQuery sets CanClose to FALSE calls TChromium.CloseBrowser which triggers the TChromium.OnClose event.
// 2. TChromium.OnClose sends a CEFBROWSER_DESTROY message to destroy CEFWindowParent1 in the main thread, which triggers the TChromium.OnBeforeClose event.
// 3. TChromium.OnBeforeClose sets FCanClose := True and sends WM_CLOSE to the form.

procedure CreateGlobalCEFApp;
begin
  GlobalCEFApp                     := TCefApplication.Create;
  GlobalCEFApp.cache               := 'cache';
  //GlobalCEFApp.LogFile             := 'debug.log';
  //GlobalCEFApp.LogSeverity         := LOGSEVERITY_INFO;
  //GlobalCEFApp.EnablePrintPreview  := True;
end;

{Property setters and getters}
{%Region}
procedure TMiniBrowserFrm.SetBrowserAddress(const aValue : string);
begin
  FBrowserCS.Acquire;
  FBrowserAddress := aValue;
  FBrowserCS.Release;
end;

procedure TMiniBrowserFrm.SetBrowserIsLoading(aValue : boolean);
begin
  FBrowserCS.Acquire;
  FBrowserIsLoading := aValue;
  FBrowserCS.Release;
end;

procedure TMiniBrowserFrm.SetBrowserCanGoBack(aValue : boolean);
begin
  FBrowserCS.Acquire;
  FBrowserCanGoBack := aValue;
  FBrowserCS.Release;
end;

procedure TMiniBrowserFrm.SetBrowserCanGoForward(aValue : boolean);
begin
  FBrowserCS.Acquire;
  FBrowserCanGoForward := aValue;
  FBrowserCS.Release;
end;

procedure TMiniBrowserFrm.SetBrowserStatusText(const aValue : string);
begin
  FBrowserCS.Acquire;
  FBrowserStatusText := aValue;
  FBrowserCS.Release;
end;

procedure TMiniBrowserFrm.SetBrowserTitle(const aValue : string);
begin
  FBrowserCS.Acquire;
  FBrowserTitle := aValue;
  FBrowserCS.Release;
end;

function TMiniBrowserFrm.GetBrowserAddress : string;
begin
  FBrowserCS.Acquire;
  Result := FBrowserAddress;
  FBrowserCS.Release;
end;

function TMiniBrowserFrm.GetBrowserIsLoading : boolean;
begin
  FBrowserCS.Acquire;
  Result := FBrowserIsLoading;
  FBrowserCS.Release;
end;

function TMiniBrowserFrm.GetBrowserCanGoBack : boolean;
begin
  FBrowserCS.Acquire;
  Result := FBrowserCanGoBack;
  FBrowserCS.Release;
end;

function TMiniBrowserFrm.GetBrowserCanGoForward : boolean;
begin
  FBrowserCS.Acquire;
  Result := FBrowserCanGoForward;
  FBrowserCS.Release;
end;

function TMiniBrowserFrm.GetBrowserStatusText : string;
begin
  FBrowserCS.Acquire;
  Result := FBrowserStatusText;
  FBrowserCS.Release;
end;

function TMiniBrowserFrm.GetBrowserTitle : string;
begin
  FBrowserCS.Acquire;
  Result := FBrowserTitle;
  FBrowserCS.Release;
end;
{%Endregion}

{Button events}
{%Region}
procedure TMiniBrowserFrm.BackBtnClick(Sender: TObject);
begin
  Chromium1.GoBack;
end;

procedure TMiniBrowserFrm.ForwardBtnClick(Sender: TObject);
begin
  Chromium1.GoForward;
end;

procedure TMiniBrowserFrm.ReloadBtnClick(Sender: TObject);
begin
  Chromium1.Reload;
end;

procedure TMiniBrowserFrm.StopBtnClick(Sender: TObject);
begin
  Chromium1.StopLoad;
end;      

procedure TMiniBrowserFrm.GoBtnClick(Sender: TObject);
begin
  Chromium1.LoadURL(UTF8Decode(URLCbx.Text));
end;

procedure TMiniBrowserFrm.ConfigBtnClick(Sender: TObject);
var
  TempPoint : TPoint;
begin
  TempPoint.x := ConfigBtn.left;
  TempPoint.y := ConfigBtn.top + ConfigBtn.Height;
  TempPoint   := ConfigPnl.ClientToScreen(TempPoint);

  PopupMenu1.Popup(TempPoint.x, TempPoint.y);
end;
{%Endregion}

{Config menu events}
{%Region}
procedure TMiniBrowserFrm.Inczoom1Click(Sender: TObject);
begin
  Chromium1.IncZoomStep;
end;

procedure TMiniBrowserFrm.Deczoom1Click(Sender: TObject);
begin
  Chromium1.DecZoomStep;
end;

procedure TMiniBrowserFrm.Resetzoom1Click(Sender: TObject);
begin
  Chromium1.ResetZoomLevel;
end;

procedure TMiniBrowserFrm.Print1Click(Sender: TObject);
begin
  // TODO: Linux applications need to use the Printer Handler events in
  // GlobalCEFApp before printing.
  //Chromium1.Print;
end;

procedure TMiniBrowserFrm.PrintinPDF1Click(Sender: TObject);
begin                         
  // TODO: Linux applications need to use the Printer Handler events in
  // GlobalCEFApp before printing.
  {
  SaveDialog1.DefaultExt := 'pdf';
  SaveDialog1.Filter     := 'PDF files (*.pdf)|*.PDF';

  if SaveDialog1.Execute and (length(SaveDialog1.FileName) > 0) then
    Chromium1.PrintToPDF(SaveDialog1.FileName, Chromium1.DocumentURL, Chromium1.DocumentURL);
  }
end;

procedure TMiniBrowserFrm.ShowDevTools1Click(Sender: TObject);
begin
  ShowDevTools;
end;        

procedure TMiniBrowserFrm.Openfile1Click(Sender: TObject);
begin
  OpenDialog1.Filter := 'Any file (*.*)|*.*';

  if OpenDialog1.Execute then
    begin
      // This is a quick solution to load files. The file URL should be properly encoded.
      Chromium1.LoadURL('file:///' + OpenDialog1.FileName);
    end;
end;

procedure TMiniBrowserFrm.OpenfilewithaDAT1Click(Sender: TObject);
var
  TempFile : TMemoryStream;
begin
  TempFile := nil;

  try
    try
      OpenDialog1.Filter := 'HTML files (*.html)|*.HTML;*.HTM|PDF files (*.pdf)|*.PDF';

      if OpenDialog1.Execute then
        begin
          TempFile := TMemoryStream.Create;
          TempFile.LoadFromFile(OpenDialog1.FileName);

          if (OpenDialog1.FilterIndex = 1) then
            Chromium1.LoadResource(TempFile, 'text/html', 'utf-8')
           else
            Chromium1.LoadResource(TempFile, 'application/pdf', 'utf-8');
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TMiniBrowserFrm.OpenfilewithaDAT1Click', e) then raise;
    end;
  finally
    if (TempFile <> nil) then FreeAndNil(TempFile);
  end;
end;

procedure TMiniBrowserFrm.HideDevTools1Click(Sender: TObject);
begin
  HideDevTools;
end;
{%Endregion}

{Form events}
{%Region}   
procedure TMiniBrowserFrm.FormActivate(Sender: TObject);
begin
  // You *MUST* call CreateBrowser to create and initialize the browser.
  // This will trigger the AfterCreated event when the browser is fully
  // initialized and ready to receive commands.

  // GlobalCEFApp.GlobalContextInitialized has to be TRUE before creating any browser
  // If it's not initialized yet, we use a simple timer to create the browser later.

  // Linux needs a visible form to create a browser so we need to use the
  // TForm.OnActivate event instead of the TForm.OnShow event

  if not(Chromium1.Initialized) and
     not(Chromium1.CreateBrowser(CEFLinkedWindowParent1.Handle, CEFLinkedWindowParent1.BoundsRect)) then
    Timer1.Enabled := True;
end;

procedure TMiniBrowserFrm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FCanClose;

  if not(FClosing) then
    begin
      FClosing := True;
      Visible  := False;

      // if TChromium.MultiBrowserMode is enabled then we have to close all
      // stored browsers and not only the main browser.
      Chromium1.CloseAllBrowsers;
    end;
end;

procedure TMiniBrowserFrm.FormCreate(Sender: TObject);
begin
  FCanClose              := False;
  FClosing               := False;

  FBrowserAddress        := '';
  FBrowserIsLoading      := False;
  FBrowserCanGoBack      := False;
  FBrowserCanGoForward   := False;
  FBrowserStatusText     := '';
  FBrowserTitle          := '';

  FBrowserCS             := TCriticalSection.Create;

  // The MultiBrowserMode store all the browser references in TChromium.
  // The first browser reference is the browser in the main form.
  // When MiniBrowser allows CEF to create child popup browsers it will also
  // store their reference inside TChromium and you can use all the TChromium's
  // methods and properties to manipulate those browsers.
  // To do that call TChromium.SelectBrowser with the browser ID that will be
  // used when you call any method or property in TChromium.
  Chromium1.MultiBrowserMode := True;
  Chromium1.DefaultURL       := UTF8Decode(URLCbx.Text);

  // WebRTC's IP leaking can lowered/avoided by setting these preferences
  // To test this go to https://www.browserleaks.com/webrtc
  Chromium1.WebRTCIPHandlingPolicy := hpDisableNonProxiedUDP;
  Chromium1.WebRTCMultipleRoutes   := STATE_DISABLED;
  Chromium1.WebRTCNonproxiedUDP    := STATE_DISABLED;
end;

procedure TMiniBrowserFrm.FormDestroy(Sender: TObject);
begin
  FBrowserCS.Free;
end;
{%Endregion}

{Lazarus form messages}
{%Region}
procedure TMiniBrowserFrm.WMMove(var Message: TLMMove);
begin
  inherited;
  Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TMiniBrowserFrm.WMSize(var Message: TLMSize);
begin
  inherited;
  Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TMiniBrowserFrm.WMWindowPosChanged(var Message: TLMWindowPosChanged);
begin
  inherited;
  Chromium1.NotifyMoveOrResizeStarted;
end;
{%Endregion}

{TCEFLinkedWindowParent events}
{%Region}
procedure TMiniBrowserFrm.CEFLinkedWindowParent1Exit(Sender: TObject);
begin
  if not(csDesigning in ComponentState) then
    Chromium1.SendCaptureLostEvent;
end;

procedure TMiniBrowserFrm.CEFLinkedWindowParent1Enter(Sender: TObject);
begin
  if not(csDesigning in ComponentState) and
     Chromium1.Initialized and
     not(Chromium1.FrameIsFocused) then
    Chromium1.SendFocusEvent(True);
end;
{%Endregion}

{Timer events}
{%Region}
procedure TMiniBrowserFrm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  if not(Chromium1.CreateBrowser(CEFLinkedWindowParent1.Handle, CEFLinkedWindowParent1.BoundsRect)) and
     not(Chromium1.Initialized) then
    Timer1.Enabled := True;
end;
{%Endregion}

{Chromium events}
{%Region}
procedure TMiniBrowserFrm.Chromium1AddressChange(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
begin
  if Chromium1.IsSameBrowser(browser) then
    begin
      BrowserAddress := url;
      SendCompMessage(CEF_UPDATEADDRESS);
    end;
end;

procedure TMiniBrowserFrm.Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  if Chromium1.IsSameBrowser(browser) then
    SendCompMessage(CEF_AFTERCREATED);
end;

procedure TMiniBrowserFrm.Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
begin
  // The main browser is being destroyed
  if (Chromium1.BrowserId = 0) then
    begin
      // We must wait until all browsers trigger the TChromium.OnBeforeClose event
      // in order to close the application safely or we will have shutdown issues.
      FCanClose := True;
      SendCompMessage(CEF_BEFORECLOSE);
    end;
end;

procedure TMiniBrowserFrm.Chromium1BeforeContextMenu(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; const model: ICefMenuModel);
begin
  if Chromium1.IsSameBrowser(browser) then
    begin
      model.AddSeparator;
      model.AddItem(MINIBROWSER_CONTEXTMENU_HIDEDEVTOOLS, 'Hide DevTools');
      model.AddItem(MINIBROWSER_CONTEXTMENU_SHOWDEVTOOLS, 'Show DevTools');
    end
   else
    model.AddItem(MINIBROWSER_CONTEXTMENU_SHOWDEVTOOLS, 'Show DevTools');
end;

procedure TMiniBrowserFrm.Chromium1Close(Sender: TObject; const browser: ICefBrowser; var aAction : TCefCloseBrowserAction);
begin
  if (browser <> nil) and
     (Chromium1.BrowserId = browser.Identifier) then
    begin
      // continue closing the browser
      aAction := cbaClose;
    end;
end;

procedure TMiniBrowserFrm.Chromium1ContextMenuCommand(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; commandId: Integer;
  eventFlags: Cardinal; out Result: Boolean);
var
  TempParam : cardinal;
  TempInfo : TCefWindowInfo;
  TempClient : ICefClient;
  TempSettings : TCefBrowserSettings;
begin
  Result := False;

  if Chromium1.IsSameBrowser(browser) then
    case commandId of
      MINIBROWSER_CONTEXTMENU_HIDEDEVTOOLS :
        SendCompMessage(CEF_HIDEDEVTOOLS);

      MINIBROWSER_CONTEXTMENU_SHOWDEVTOOLS :
        begin
          TempParam := ((params.XCoord and $FFFF) shl 16) or (params.YCoord and $FFFF);
          SendCompMessage(CEF_SHOWDEVTOOLS, PtrInt(TempParam));
        end;
    end
   else
    case commandId of
      MINIBROWSER_CONTEXTMENU_SHOWDEVTOOLS :
        try
          WindowInfoAsPopUp(TempInfo, browser.Host.WindowHandle, 'DevTools');
          TempClient := TCustomClientHandler.Create(Chromium1, True);
          FillChar(TempSettings, SizeOf(TCefBrowserSettings), 0);
          browser.Host.ShowDevTools(@TempInfo, TempClient, @TempSettings, nil);
        finally
          TempClient := nil
        end;
    end;
end;        

procedure TMiniBrowserFrm.Chromium1PreKeyEvent(Sender: TObject;
  const browser: ICefBrowser; const event: PCefKeyEvent; osEvent: TCefEventHandle;
  out isKeyboardShortcut, Result: Boolean);
begin
  Result := False;

  if (event <> nil) and
     (event^.kind in [KEYEVENT_KEYDOWN, KEYEVENT_KEYUP]) and
     (event^.windows_key_code = VKEY_F12) then
    isKeyboardShortcut := True;
end;

procedure TMiniBrowserFrm.Chromium1KeyEvent(Sender: TObject;
  const browser: ICefBrowser; const event: PCefKeyEvent; osEvent: TCefEventHandle;
  out Result: Boolean);
begin
  Result := False;

  if (event <> nil) and (event^.windows_key_code = VKEY_F12) then
    case event^.kind of
      KEYEVENT_KEYDOWN :
        Result := True;

      KEYEVENT_KEYUP :
        begin
          //  SendCompMessage(CEF_HIDEDEVTOOLS)
          SendCompMessage(CEF_SHOWDEVTOOLS);
          Result := True;
        end;
    end;
end;

procedure TMiniBrowserFrm.Chromium1LoadError(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; errorCode: Integer;
  const errorText, failedUrl: ustring);
var
  TempString : ustring;
begin
  if (errorCode = ERR_ABORTED) then exit;

  TempString := '<html><body bgcolor="white">' +
                '<h2>Failed to load URL ' + failedUrl +
                ' with error ' + errorText +
                ' (' + inttostr(errorCode) + ').</h2></body></html>';

  Chromium1.LoadString(TempString, frame);
end;

procedure TMiniBrowserFrm.Chromium1LoadingStateChange(Sender: TObject;
  const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
begin
  if not(Chromium1.IsSameBrowser(browser)) or FClosing then exit;

  BrowserIsLoading    := isLoading;
  BrowserCanGoBack    := canGoBack;
  BrowserCanGoForward := canGoForward;

  SendCompMessage(CEF_UPDATELOADINGSTATE);
end;

procedure TMiniBrowserFrm.Chromium1PdfPrintFinished(Sender: TObject; aResultOK: Boolean);
begin                              
  SendCompMessage(CEF_PDFPRINTEND, PtrInt(ord(aResultOK)));
end;

procedure TMiniBrowserFrm.Chromium1StatusMessage(Sender: TObject;
  const browser: ICefBrowser; const value: ustring);
begin
  if Chromium1.IsSameBrowser(browser) then
    begin
      BrowserStatusText := value;
      SendCompMessage(CEF_UPDATESTATUSTEXT);
    end;
end;

procedure TMiniBrowserFrm.Chromium1TitleChange(Sender: TObject;
  const browser: ICefBrowser; const title: ustring);
begin
  if not(Chromium1.IsSameBrowser(browser)) then exit;

  if (length(title) > 0) then
    BrowserTitle := 'MiniBrowser - ' + title
   else
    BrowserTitle := 'MiniBrowser - ' + Chromium1.DocumentURL;

  SendCompMessage(CEF_UPDATETITLE);
end;

procedure TMiniBrowserFrm.Chromium1BeforePluginLoad(Sender: TObject;
  const mimeType, pluginUrl: ustring; isMainFrame: boolean;
  const topOriginUrl: ustring; const pluginInfo: ICefWebPluginInfo;
  var pluginPolicy: TCefPluginPolicy; var aResult: boolean);
begin
  // Always allow the PDF plugin to load.
  if (pluginPolicy <> PLUGIN_POLICY_ALLOW) and
     (CompareText(mimeType, 'application/pdf') = 0) then
    begin
      pluginPolicy := PLUGIN_POLICY_ALLOW;
      aResult      := True;
    end
   else
    aResult := False;
end;

procedure TMiniBrowserFrm.Chromium1GotFocus(Sender: TObject;
  const browser: ICefBrowser);
begin
  CEFLinkedWindowParent1.SetFocus;
end;
{%Endregion}

{Custom form messages}
{%Region}
procedure TMiniBrowserFrm.BrowserCreatedMsg(Data: PtrInt);
begin
  Caption               := 'MiniBrowser';
  NavControlPnl.Enabled := True;
  CEFLinkedWindowParent1.UpdateSize;
end;

procedure TMiniBrowserFrm.BrowserCloseFormMsg(Data: PtrInt);
begin
  Close;
end;

procedure TMiniBrowserFrm.BrowserShowDevToolsMsg(Data: PtrInt);
var
  TempPoint : TPoint;
begin
  if (Data <> 0) then
    begin
      TempPoint.x := (Data shr 16) and $FFFF;
      TempPoint.y := Data and $FFFF;
      ShowDevTools(TempPoint);
    end
   else
    ShowDevTools;
end;

procedure TMiniBrowserFrm.BrowserHideDevToolsMsg(Data: PtrInt);
begin
  HideDevTools;
  Chromium1.SetFocus(True);
end;

procedure TMiniBrowserFrm.BrowserPrintPDFEndMsg(Data: PtrInt);
begin
  if (Data <> 0) then
    showmessage('The PDF file was generated successfully')
   else
    showmessage('There was a problem generating the PDF file.');
end;

procedure TMiniBrowserFrm.BrowserUpdateAddressMsg(Data: PtrInt);
var
  TempURL : string;
begin
  TempURL := BrowserAddress;

  if (URLCbx.Items.IndexOf(TempURL) < 0) then
     URLCbx.Items.Add(TempURL);

  URLCbx.Text := TempURL;
end;

procedure TMiniBrowserFrm.BrowserUpdateLoadingStateMsg(Data: PtrInt);
begin
  BackBtn.Enabled    := BrowserCanGoBack;
  ForwardBtn.Enabled := BrowserCanGoForward;

  if BrowserIsLoading then
    begin
      ReloadBtn.Enabled := False;
      StopBtn.Enabled   := True;
    end
   else
    begin
      ReloadBtn.Enabled := True;
      StopBtn.Enabled   := False;
    end;
end;

procedure TMiniBrowserFrm.BrowserUpdateStatusTextMsg(Data: PtrInt);
begin
  StatusBar1.Panels[0].Text := BrowserStatusText;
end;

procedure TMiniBrowserFrm.BrowserUpdateTitleMsg(Data: PtrInt);
begin
  Caption := BrowserTitle;
end;

procedure TMiniBrowserFrm.SendCompMessage(aMsg : cardinal; Data: PtrInt);
begin
  case aMsg of
    CEF_AFTERCREATED       : Application.QueueAsyncCall(@BrowserCreatedMsg, Data);
    CEF_BEFORECLOSE        : Application.QueueAsyncCall(@BrowserCloseFormMsg, Data);
    CEF_UPDATEADDRESS      : Application.QueueAsyncCall(@BrowserUpdateAddressMsg, Data);
    CEF_UPDATELOADINGSTATE : Application.QueueAsyncCall(@BrowserUpdateLoadingStateMsg, Data);
    CEF_UPDATESTATUSTEXT   : Application.QueueAsyncCall(@BrowserUpdateStatusTextMsg, Data);
    CEF_UPDATETITLE        : Application.QueueAsyncCall(@BrowserUpdateTitleMsg, Data);
    CEF_SHOWDEVTOOLS       : Application.QueueAsyncCall(@BrowserShowDevToolsMsg, Data);
    CEF_HIDEDEVTOOLS       : Application.QueueAsyncCall(@BrowserHideDevToolsMsg, Data);
    CEF_PDFPRINTEND        : Application.QueueAsyncCall(@BrowserPrintPDFEndMsg, Data);
  end;
end;
{%Endregion}

{DevTools procedures}
{%Region}
procedure TMiniBrowserFrm.ShowDevTools(aPoint : TPoint);
begin
  Chromium1.ShowDevTools(aPoint);
end;

procedure TMiniBrowserFrm.ShowDevTools;
var
  TempPoint : TPoint;
begin
  TempPoint.x := low(integer);
  TempPoint.y := low(integer);
  ShowDevTools(TempPoint);
end;

procedure TMiniBrowserFrm.HideDevTools;
begin
  Chromium1.CloseDevTools;
end;
{%Endregion}

end.
