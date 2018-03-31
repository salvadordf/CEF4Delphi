// ************************************************************************
// ***************************** CEF4Delphi *******************************
// ************************************************************************
//
// CEF4Delphi is based on DCEF3 which uses CEF3 to embed a chromium-based
// browser in Delphi applications.
//
// The original license of DCEF3 still applies to CEF4Delphi.
//
// For more information about CEF4Delphi visit :
//         https://www.briskbard.com/index.php?lang=en&pageid=cef
//
//        Copyright © 2018 Salvador Díaz Fau. All rights reserved.
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

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Menus,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, System.Types, Vcl.ComCtrls, Vcl.ClipBrd,
  System.UITypes, Vcl.AppEvnts, Winapi.ActiveX, Winapi.ShlObj,
  {$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Menus,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Types, ComCtrls, ClipBrd, AppEvnts, ActiveX, ShlObj,
  {$ENDIF}
  uCEFChromium, uCEFWindowParent, uCEFInterfaces, uCEFApplication, uCEFTypes, uCEFConstants;

const
  MINIBROWSER_SHOWDEVTOOLS    = WM_APP + $101;
  MINIBROWSER_HIDEDEVTOOLS    = WM_APP + $102;
  MINIBROWSER_COPYHTML        = WM_APP + $103;
  MINIBROWSER_SHOWRESPONSE    = WM_APP + $104;
  MINIBROWSER_COPYFRAMEIDS    = WM_APP + $105;
  MINIBROWSER_COPYFRAMENAMES  = WM_APP + $106;
  MINIBROWSER_SAVEPREFERENCES = WM_APP + $107;
  MINIBROWSER_COPYALLTEXT     = WM_APP + $108;
  MINIBROWSER_TAKESNAPSHOT    = WM_APP + $109;

  MINIBROWSER_HOMEPAGE = 'https://www.google.com';

  MINIBROWSER_CONTEXTMENU_SHOWDEVTOOLS    = MENU_ID_USER_FIRST + 1;
  MINIBROWSER_CONTEXTMENU_HIDEDEVTOOLS    = MENU_ID_USER_FIRST + 2;
  MINIBROWSER_CONTEXTMENU_COPYHTML        = MENU_ID_USER_FIRST + 3;
  MINIBROWSER_CONTEXTMENU_JSWRITEDOC      = MENU_ID_USER_FIRST + 4;
  MINIBROWSER_CONTEXTMENU_JSPRINTDOC      = MENU_ID_USER_FIRST + 5;
  MINIBROWSER_CONTEXTMENU_SHOWRESPONSE    = MENU_ID_USER_FIRST + 6;
  MINIBROWSER_CONTEXTMENU_COPYFRAMEIDS    = MENU_ID_USER_FIRST + 7;
  MINIBROWSER_CONTEXTMENU_COPYFRAMENAMES  = MENU_ID_USER_FIRST + 8;
  MINIBROWSER_CONTEXTMENU_SAVEPREFERENCES = MENU_ID_USER_FIRST + 9;
  MINIBROWSER_CONTEXTMENU_COPYALLTEXT     = MENU_ID_USER_FIRST + 10;
  MINIBROWSER_CONTEXTMENU_TAKESNAPSHOT    = MENU_ID_USER_FIRST + 11;

type
  TMiniBrowserFrm = class(TForm)
    NavControlPnl: TPanel;
    NavButtonPnl: TPanel;
    URLEditPnl: TPanel;
    BackBtn: TButton;
    ForwardBtn: TButton;
    ReloadBtn: TButton;
    CEFWindowParent1: TCEFWindowParent;
    Chromium1: TChromium;
    StopBtn: TButton;
    DevTools: TCEFWindowParent;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    URLCbx: TComboBox;
    ConfigPnl: TPanel;
    ConfigBtn: TButton;
    PopupMenu1: TPopupMenu;
    DevTools1: TMenuItem;
    N1: TMenuItem;
    Preferences1: TMenuItem;
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
    ApplicationEvents1: TApplicationEvents;
    OpenDialog1: TOpenDialog;
    N4: TMenuItem;
    Openfile1: TMenuItem;
    Resolvehost1: TMenuItem;
    Timer1: TTimer;
    procedure FormShow(Sender: TObject);
    procedure BackBtnClick(Sender: TObject);
    procedure ForwardBtnClick(Sender: TObject);
    procedure ReloadBtnClick(Sender: TObject);
    procedure Chromium1AfterCreated(Sender: TObject;
      const browser: ICefBrowser);
    procedure Chromium1LoadingStateChange(Sender: TObject;
      const browser: ICefBrowser; isLoading, canGoBack,
      canGoForward: Boolean);
    procedure Chromium1TitleChange(Sender: TObject;
      const browser: ICefBrowser; const title: ustring);
    procedure Chromium1AddressChange(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame;
      const url: ustring);
    procedure Chromium1BeforeContextMenu(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame;
      const params: ICefContextMenuParams; const model: ICefMenuModel);
    procedure Chromium1StatusMessage(Sender: TObject;
      const browser: ICefBrowser; const value: ustring);
    procedure Chromium1TextResultAvailable(Sender: TObject;
      const aText: string);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure DevTools1Click(Sender: TObject);
    procedure Preferences1Click(Sender: TObject);
    procedure ConfigBtnClick(Sender: TObject);
    procedure GoBtnClick(Sender: TObject);
    procedure PrintinPDF1Click(Sender: TObject);
    procedure Print1Click(Sender: TObject);
    procedure Inczoom1Click(Sender: TObject);
    procedure Deczoom1Click(Sender: TObject);
    procedure Resetzoom1Click(Sender: TObject);
    procedure Chromium1FullScreenModeChange(Sender: TObject;
      const browser: ICefBrowser; fullscreen: Boolean);
    procedure Chromium1PreKeyEvent(Sender: TObject;
      const browser: ICefBrowser; const event: PCefKeyEvent; osEvent: PMsg;
      out isKeyboardShortcut, Result: Boolean);
    procedure Chromium1KeyEvent(Sender: TObject;
      const browser: ICefBrowser; const event: PCefKeyEvent; osEvent: PMsg;
      out Result: Boolean);
    procedure ApplicationEvents1Message(var Msg: tagMSG;
      var Handled: Boolean);
    procedure Openfile1Click(Sender: TObject);
    procedure Chromium1ContextMenuCommand(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame;
      const params: ICefContextMenuParams; commandId: Integer;
      eventFlags: Cardinal; out Result: Boolean);
    procedure Chromium1PdfPrintFinished(Sender: TObject;
      aResultOK: Boolean);
    procedure Chromium1ResourceResponse(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; const response: ICefResponse;
      out Result: Boolean);
    procedure StopBtnClick(Sender: TObject);
    procedure Resolvehost1Click(Sender: TObject);
    procedure Chromium1ResolvedHostAvailable(Sender: TObject;
      result: Integer; const resolvedIps: TStrings);
    procedure Timer1Timer(Sender: TObject);
    procedure Chromium1PrefsAvailable(Sender: TObject; aResultOK: Boolean);
    procedure Chromium1BeforeDownload(Sender: TObject;
      const browser: ICefBrowser; const downloadItem: ICefDownloadItem;
      const suggestedName: ustring;
      const callback: ICefBeforeDownloadCallback);
    procedure Chromium1DownloadUpdated(Sender: TObject;
      const browser: ICefBrowser; const downloadItem: ICefDownloadItem;
      const callback: ICefDownloadItemCallback);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Chromium1BeforeResourceLoad(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; const callback: ICefRequestCallback;
      out Result: TCefReturnValue);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Chromium1Close(Sender: TObject; const browser: ICefBrowser;
      out Result: Boolean);
    procedure Chromium1BeforeClose(Sender: TObject;
      const browser: ICefBrowser);
    procedure Chromium1RenderCompMsg(var aMessage : TMessage; var aHandled: Boolean);

  protected
    FResponse : TStringList;
    FRequest  : TStringList;
    // Variables to control when can we destroy the form safely
    FCanClose : boolean;  // Set to True in TChromium.OnBeforeClose
    FClosing  : boolean;  // Set to True in the CloseQuery event.

    procedure AddURL(const aURL : string);

    procedure ShowDevTools(aPoint : TPoint); overload;
    procedure ShowDevTools; overload;
    procedure HideDevTools;

    procedure HandleKeyUp(const aMsg : TMsg; var aHandled : boolean);
    procedure HandleKeyDown(const aMsg : TMsg; var aHandled : boolean);

    procedure InspectRequest(const aRequest : ICefRequest);
    procedure InspectResponse(const aResponse : ICefResponse);

    procedure BrowserCreatedMsg(var aMessage : TMessage); message CEF_AFTERCREATED;
    procedure BrowserDestroyMsg(var aMessage : TMessage); message CEF_DESTROY;
    procedure ShowDevToolsMsg(var aMessage : TMessage); message MINIBROWSER_SHOWDEVTOOLS;
    procedure HideDevToolsMsg(var aMessage : TMessage); message MINIBROWSER_HIDEDEVTOOLS;
    procedure CopyAllTextMsg(var aMessage : TMessage); message MINIBROWSER_COPYALLTEXT;
    procedure CopyHTMLMsg(var aMessage : TMessage); message MINIBROWSER_COPYHTML;
    procedure CopyFramesIDsMsg(var aMessage : TMessage); message MINIBROWSER_COPYFRAMEIDS;
    procedure CopyFramesNamesMsg(var aMessage : TMessage); message MINIBROWSER_COPYFRAMENAMES;
    procedure ShowResponseMsg(var aMessage : TMessage); message MINIBROWSER_SHOWRESPONSE;
    procedure SavePreferencesMsg(var aMessage : TMessage); message MINIBROWSER_SAVEPREFERENCES;
    procedure TakeSnapshotMsg(var aMessage : TMessage); message MINIBROWSER_TAKESNAPSHOT;
    procedure WMMove(var aMessage : TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage : TMessage); message WM_MOVING;
    procedure WMEnterMenuLoop(var aMessage: TMessage); message WM_ENTERMENULOOP;
    procedure WMExitMenuLoop(var aMessage: TMessage); message WM_EXITMENULOOP;

  public
    procedure ShowStatusText(const aText : string);

  end;

var
  MiniBrowserFrm : TMiniBrowserFrm;

implementation

{$R *.dfm}

uses
  uPreferences, uCefStringMultimap, uSimpleTextViewer;

// Destruction steps
// =================
// 1. FormCloseQuery sets CanClose to FALSE calls TChromium.CloseBrowser which triggers the TChromium.OnClose event.
// 2. TChromium.OnClose sends a CEFBROWSER_DESTROY message to destroy CEFWindowParent1 in the main thread, which triggers the TChromium.OnBeforeClose event.
// 3. TChromium.OnBeforeClose sets FCanClose := True and sends WM_CLOSE to the form.

procedure TMiniBrowserFrm.BackBtnClick(Sender: TObject);
begin
  Chromium1.GoBack;
end;

procedure TMiniBrowserFrm.ForwardBtnClick(Sender: TObject);
begin
  Chromium1.GoForward;
end;

procedure TMiniBrowserFrm.GoBtnClick(Sender: TObject);
begin
  Chromium1.LoadURL(URLCbx.Text);
end;

procedure TMiniBrowserFrm.ReloadBtnClick(Sender: TObject);
begin
  Chromium1.Reload;
end;

procedure TMiniBrowserFrm.Resetzoom1Click(Sender: TObject);
begin
  Chromium1.ResetZoomStep;
end;

procedure TMiniBrowserFrm.Resolvehost1Click(Sender: TObject);
var
  TempURL : string;
begin
  TempURL := inputbox('Resolve host', 'URL :', 'http://google.com');
  if (length(TempURL) > 0) then Chromium1.ResolveHost(TempURL);
end;

procedure TMiniBrowserFrm.Chromium1AddressChange(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
begin
  if Chromium1.IsSameBrowser(browser) then AddURL(url);
end;

procedure TMiniBrowserFrm.Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  if Chromium1.IsSameBrowser(browser) then
    PostMessage(Handle, CEF_AFTERCREATED, 0, 0)
   else
    SendMessage(browser.Host.WindowHandle, WM_SETICON, 1, application.Icon.Handle); // Use the same icon in the popup window
end;

procedure TMiniBrowserFrm.Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
begin
  if (Chromium1.BrowserId = 0) then // The main browser is being destroyed
    begin
      FCanClose := True;
      PostMessage(Handle, WM_CLOSE, 0, 0);
    end;
end;

procedure TMiniBrowserFrm.Chromium1BeforeContextMenu(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; const model: ICefMenuModel);
begin
  if not(Chromium1.IsSameBrowser(browser)) then exit;

  model.AddSeparator;
  model.AddItem(MINIBROWSER_CONTEXTMENU_TAKESNAPSHOT,    'Take snapshot...');
  model.AddSeparator;
  model.AddItem(MINIBROWSER_CONTEXTMENU_COPYALLTEXT,     'Copy displayed text to clipboard');
  model.AddItem(MINIBROWSER_CONTEXTMENU_COPYHTML,        'Copy HTML to clipboard');
  model.AddItem(MINIBROWSER_CONTEXTMENU_COPYFRAMEIDS,    'Copy HTML frame identifiers to clipboard');
  model.AddItem(MINIBROWSER_CONTEXTMENU_COPYFRAMENAMES,  'Copy HTML frame names to clipboard');
  model.AddSeparator;
  model.AddItem(MINIBROWSER_CONTEXTMENU_SAVEPREFERENCES, 'Save preferences as...');
  model.AddSeparator;
  model.AddItem(MINIBROWSER_CONTEXTMENU_JSWRITEDOC,      'Modify HTML document');
  model.AddItem(MINIBROWSER_CONTEXTMENU_JSPRINTDOC,      'Print using Javascript');
  model.AddItem(MINIBROWSER_CONTEXTMENU_SHOWRESPONSE,    'Show server headers');

  if DevTools.Visible then
    model.AddItem(MINIBROWSER_CONTEXTMENU_HIDEDEVTOOLS, 'Hide DevTools')
   else
    model.AddItem(MINIBROWSER_CONTEXTMENU_SHOWDEVTOOLS, 'Show DevTools');
end;

function PathToMyDocuments : string;
var
  Allocator : IMalloc;
  Path      : pchar;
  idList    : PItemIDList;
begin
  Result   := '';
  Path     := nil;
  idList   := nil;

  try
    if (SHGetMalloc(Allocator) = S_OK) then
      begin
        GetMem(Path, MAX_PATH);
        if (SHGetSpecialFolderLocation(0, CSIDL_PERSONAL, idList) = S_OK) and
           SHGetPathFromIDList(idList, Path) then
          Result := string(Path);
      end;
  finally
    if (Path   <> nil) then FreeMem(Path);
    if (idList <> nil) then Allocator.Free(idList);
  end;
end;

procedure TMiniBrowserFrm.Chromium1BeforeDownload(Sender: TObject;
  const browser: ICefBrowser; const downloadItem: ICefDownloadItem;
  const suggestedName: ustring;
  const callback: ICefBeforeDownloadCallback);
var
  TempMyDocuments, TempFullPath, TempName : string;
begin
  if not(Chromium1.IsSameBrowser(browser)) or
     (downloadItem = nil) or
     not(downloadItem.IsValid) then
    exit;

  TempMyDocuments := PathToMyDocuments;

  if (length(suggestedName) > 0) then
    TempName := suggestedName
   else
    TempName := 'DownloadedFile';

  if (length(TempMyDocuments) > 0) then
    TempFullPath := IncludeTrailingPathDelimiter(TempMyDocuments) + TempName
   else
    TempFullPath := TempName;

  callback.cont(TempFullPath, False);
end;

procedure TMiniBrowserFrm.Chromium1BeforeResourceLoad(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const request: ICefRequest; const callback: ICefRequestCallback;
  out Result: TCefReturnValue);
begin
  Result := RV_CONTINUE;

  if Chromium1.IsSameBrowser(browser) and
     (frame <> nil) and
     frame.IsMain then
    InspectRequest(request);
end;

procedure TMiniBrowserFrm.Chromium1Close(Sender: TObject; const browser: ICefBrowser; out Result: Boolean);
begin
  if (browser <> nil) and (Chromium1.BrowserId = browser.Identifier) then
    begin
      PostMessage(Handle, CEF_DESTROY, 0, 0);
      Result := True;
    end
   else
    Result := False;
end;

procedure TMiniBrowserFrm.Chromium1ContextMenuCommand(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; commandId: Integer;
  eventFlags: Cardinal; out Result: Boolean);
var
  TempParam : WParam;
begin
  Result := False;

  if not(Chromium1.IsSameBrowser(browser)) then exit;

  case commandId of
    MINIBROWSER_CONTEXTMENU_HIDEDEVTOOLS :
      PostMessage(Handle, MINIBROWSER_HIDEDEVTOOLS, 0, 0);

    MINIBROWSER_CONTEXTMENU_SHOWDEVTOOLS :
      begin
        TempParam := ((params.XCoord and $FFFF) shl 16) or (params.YCoord and $FFFF);
        PostMessage(Handle, MINIBROWSER_SHOWDEVTOOLS, TempParam, 0);
      end;

    MINIBROWSER_CONTEXTMENU_COPYALLTEXT :
      PostMessage(Handle, MINIBROWSER_COPYALLTEXT, 0, 0);

    MINIBROWSER_CONTEXTMENU_COPYHTML :
      PostMessage(Handle, MINIBROWSER_COPYHTML, 0, 0);

    MINIBROWSER_CONTEXTMENU_COPYFRAMEIDS :
      PostMessage(Handle, MINIBROWSER_COPYFRAMEIDS, 0, 0);

    MINIBROWSER_CONTEXTMENU_COPYFRAMENAMES :
      PostMessage(Handle, MINIBROWSER_COPYFRAMENAMES, 0, 0);

    MINIBROWSER_CONTEXTMENU_SHOWRESPONSE :
      PostMessage(Handle, MINIBROWSER_SHOWRESPONSE, 0, 0);

    MINIBROWSER_CONTEXTMENU_SAVEPREFERENCES :
      PostMessage(Handle, MINIBROWSER_SAVEPREFERENCES, 0, 0);

    MINIBROWSER_CONTEXTMENU_TAKESNAPSHOT :
      PostMessage(Handle, MINIBROWSER_TAKESNAPSHOT, 0, 0);

    MINIBROWSER_CONTEXTMENU_JSWRITEDOC :
      if (browser <> nil) and (browser.MainFrame <> nil) then
        browser.MainFrame.ExecuteJavaScript(
          'var css = ' + chr(39) + '@page {size: A4; margin: 0;} @media print {html, body {width: 210mm; height: 297mm;}}' + chr(39) + '; ' +
          'var style = document.createElement(' + chr(39) + 'style' + chr(39) + '); ' +
          'style.type = ' + chr(39) + 'text/css' + chr(39) + '; ' +
          'style.appendChild(document.createTextNode(css)); ' +
          'document.head.appendChild(style);',
          'about:blank', 0);

    MINIBROWSER_CONTEXTMENU_JSPRINTDOC :
      if (browser <> nil) and (browser.MainFrame <> nil) then
        browser.MainFrame.ExecuteJavaScript('window.print();', 'about:blank', 0);
  end;
end;

procedure TMiniBrowserFrm.Chromium1DownloadUpdated(Sender: TObject;
  const browser: ICefBrowser; const downloadItem: ICefDownloadItem;
  const callback: ICefDownloadItemCallback);
var
  TempString : string;
begin
  if not(Chromium1.IsSameBrowser(browser)) then exit;

  if downloadItem.IsComplete then
    ShowStatusText(downloadItem.FullPath + ' completed')
   else
    if downloadItem.IsCanceled then
      ShowStatusText(downloadItem.FullPath + ' canceled')
     else
      if downloadItem.IsInProgress then
        begin
          if (downloadItem.PercentComplete >= 0) then
            TempString := downloadItem.FullPath + ' : ' + inttostr(downloadItem.PercentComplete) + '%'
           else
            TempString := downloadItem.FullPath + ' : ' + inttostr(downloadItem.ReceivedBytes) + ' bytes received';

          ShowStatusText(TempString);
        end;
end;

procedure TMiniBrowserFrm.Chromium1FullScreenModeChange(Sender: TObject;
  const browser: ICefBrowser; fullscreen: Boolean);
begin                    
  if not(Chromium1.IsSameBrowser(browser)) then exit;

  // This event is executed in a CEF thread and this can cause problems when
  // you change the 'Enabled' and 'Visible' properties from VCL components.
  // It's recommended to change the 'Enabled' and 'Visible' properties
  // in the main application thread and not in a CEF thread.
  // It's much safer to use PostMessage to send a message to the main form with
  // all this information and update those properties in the procedure handling
  // that message.

  if fullscreen then
    begin
      NavControlPnl.Visible := False;
      StatusBar1.Visible    := False;

      if (WindowState = wsMaximized) then WindowState := wsNormal;

      BorderIcons := [];
      BorderStyle := bsNone;
      WindowState := wsMaximized;
    end
   else
    begin
      BorderIcons := [biSystemMenu, biMinimize, biMaximize];
      BorderStyle := bsSizeable;
      WindowState := wsNormal;

      NavControlPnl.Visible := True;
      StatusBar1.Visible    := True;
    end;
end;

procedure TMiniBrowserFrm.Chromium1KeyEvent(Sender: TObject;
  const browser: ICefBrowser; const event: PCefKeyEvent; osEvent: PMsg;
  out Result: Boolean);
var
  TempMsg : TMsg;
begin
  Result := False;

  if not(Chromium1.IsSameBrowser(browser)) then exit;

  if (event <> nil) and (osEvent <> nil) then
    case osEvent.Message of
      WM_KEYUP :
        begin
          TempMsg := osEvent^;

          HandleKeyUp(TempMsg, Result);
        end;

      WM_KEYDOWN :
        begin
          TempMsg := osEvent^;

          HandleKeyDown(TempMsg, Result);
        end;
    end;
end;

procedure TMiniBrowserFrm.ApplicationEvents1Message(var Msg: tagMSG;
  var Handled: Boolean);
begin
  case Msg.message of
    WM_KEYUP   : HandleKeyUp(Msg, Handled);
    WM_KEYDOWN : HandleKeyDown(Msg, Handled);
  end;
end;

procedure TMiniBrowserFrm.HandleKeyUp(const aMsg : TMsg; var aHandled : boolean);
var
  TempMessage : TMessage;
  TempKeyMsg  : TWMKey;
begin
  TempMessage.Msg     := aMsg.message;
  TempMessage.wParam  := aMsg.wParam;
  TempMessage.lParam  := aMsg.lParam;
  TempKeyMsg          := TWMKey(TempMessage);

  if (TempKeyMsg.CharCode = VK_F12) then
    begin
      aHandled := True;

      if DevTools.Visible then
        PostMessage(Handle, MINIBROWSER_HIDEDEVTOOLS, 0, 0)
       else
        PostMessage(Handle, MINIBROWSER_SHOWDEVTOOLS, 0, 0);
    end;
end;

procedure TMiniBrowserFrm.HandleKeyDown(const aMsg : TMsg; var aHandled : boolean);
var
  TempMessage : TMessage;
  TempKeyMsg  : TWMKey;
begin
  TempMessage.Msg     := aMsg.message;
  TempMessage.wParam  := aMsg.wParam;
  TempMessage.lParam  := aMsg.lParam;
  TempKeyMsg          := TWMKey(TempMessage);

  if (TempKeyMsg.CharCode = VK_F12) then aHandled := True;
end;

procedure TMiniBrowserFrm.Chromium1LoadingStateChange(Sender: TObject;
  const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
begin
  if not(Chromium1.IsSameBrowser(browser)) or FClosing then exit;

  // This event is executed in a CEF thread and this can cause problems when
  // you change the 'Enabled' and 'Visible' properties from VCL components.
  // It's recommended to change the 'Enabled' and 'Visible' properties
  // in the main application thread and not in a CEF thread.
  // It's much safer to use PostMessage to send a message to the main form with
  // all this information and update those properties in the procedure handling
  // that message.

  BackBtn.Enabled    := canGoBack;
  ForwardBtn.Enabled := canGoForward;

  if isLoading then
    begin
      StatusBar1.Panels[0].Text := 'Loading...';
      ReloadBtn.Enabled         := False;
      StopBtn.Enabled           := True;
    end
   else
    begin
      StatusBar1.Panels[0].Text := 'Finished';
      ReloadBtn.Enabled         := True;
      StopBtn.Enabled           := False;
    end;
end;

procedure TMiniBrowserFrm.Chromium1PdfPrintFinished(Sender: TObject; aResultOK: Boolean);
begin
  if aResultOK then
    showmessage('The PDF file was generated successfully')
   else
    showmessage('There was a problem generating the PDF file.');
end;

procedure TMiniBrowserFrm.Chromium1PrefsAvailable(Sender: TObject; aResultOK: Boolean);
begin
  if aResultOK then
    showmessage('The preferences file was generated successfully')
   else
    showmessage('There was a problem generating the preferences file.');
end;

procedure TMiniBrowserFrm.Chromium1PreKeyEvent(Sender: TObject;
  const browser: ICefBrowser; const event: PCefKeyEvent; osEvent: PMsg;
  out isKeyboardShortcut, Result: Boolean);
begin
  Result := False;

  if Chromium1.IsSameBrowser(browser) and
     (event <> nil) and
     (event.kind in [KEYEVENT_KEYDOWN, KEYEVENT_KEYUP]) and
     (event.windows_key_code = VK_F12) then
    isKeyboardShortcut := True;
end;

procedure TMiniBrowserFrm.Chromium1RenderCompMsg(var aMessage : TMessage; var aHandled: Boolean);
begin
  if not(FClosing) and (aMessage.Msg = WM_MOUSEMOVE) then
    begin
      StatusBar1.Panels[2].Text := 'x : ' + inttostr(aMessage.lParam and $FFFF);
      StatusBar1.Panels[3].Text := 'y : ' + inttostr((aMessage.lParam and $FFFF0000) shr 16);
    end;
end;

procedure TMiniBrowserFrm.Chromium1ResolvedHostAvailable(Sender: TObject;
  result: Integer; const resolvedIps: TStrings);
begin
  if (result = ERR_NONE) then
    showmessage('Resolved IPs : ' + resolvedIps.CommaText)
   else
    showmessage('There was a problem resolving the host.' + CRLF +
                'Error code : ' + inttostr(result));
end;

procedure TMiniBrowserFrm.InspectRequest(const aRequest : ICefRequest);
var
  TempHeaderMap : ICefStringMultimap;
  i, j : integer;
begin
  if (aRequest <> nil) then
    begin
      FRequest.Clear;

      TempHeaderMap := TCefStringMultimapOwn.Create;
      aRequest.GetHeaderMap(TempHeaderMap);

      i := 0;
      j := TempHeaderMap.Size;

      while (i < j) do
        begin
          FRequest.Add(TempHeaderMap.Key[i] + '=' + TempHeaderMap.Value[i]);
          inc(i);
        end;
    end;
end;

procedure TMiniBrowserFrm.InspectResponse(const aResponse : ICefResponse);
var
  TempHeaderMap : ICefStringMultimap;
  i, j : integer;
begin
  if (aResponse <> nil) then
    begin
      FResponse.Clear;

      TempHeaderMap := TCefStringMultimapOwn.Create;
      aResponse.GetHeaderMap(TempHeaderMap);

      i := 0;
      j := TempHeaderMap.Size;

      while (i < j) do
        begin
          FResponse.Add(TempHeaderMap.Key[i] + '=' + TempHeaderMap.Value[i]);
          inc(i);
        end;
    end;
end;

procedure TMiniBrowserFrm.Chromium1ResourceResponse(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const request: ICefRequest; const response: ICefResponse;
  out Result: Boolean);
begin
  Result := False;

  if Chromium1.IsSameBrowser(browser) and
     (frame <> nil) and
     frame.IsMain then
    InspectResponse(response);
end;

procedure TMiniBrowserFrm.ShowStatusText(const aText : string);
begin
  if not(FClosing) then StatusBar1.Panels[1].Text := aText;
end;

procedure TMiniBrowserFrm.StopBtnClick(Sender: TObject);
begin
  Chromium1.StopLoad;
end;

procedure TMiniBrowserFrm.Chromium1StatusMessage(Sender: TObject;
  const browser: ICefBrowser; const value: ustring);
begin
  if Chromium1.IsSameBrowser(browser) then ShowStatusText(value);
end;

procedure TMiniBrowserFrm.Chromium1TextResultAvailable(Sender: TObject; const aText: string);
begin
  clipboard.AsText := aText;
end;

procedure TMiniBrowserFrm.Chromium1TitleChange(Sender: TObject;
  const browser: ICefBrowser; const title: ustring);
begin
  if not(Chromium1.IsSameBrowser(browser)) then exit;

  if (title <> '') then
    caption := 'MiniBrowser - ' + title
   else
    caption := 'MiniBrowser';
end;

procedure TMiniBrowserFrm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FCanClose;

  if not(FClosing) then
    begin
      FClosing := True;
      Visible  := False;
      Chromium1.CloseBrowser(True);
    end;
end;

procedure TMiniBrowserFrm.FormCreate(Sender: TObject);
begin
  FCanClose := False;
  FClosing  := False;
  FResponse := TStringList.Create;
  FRequest  := TStringList.Create;
end;

procedure TMiniBrowserFrm.FormDestroy(Sender: TObject);
begin
  FResponse.Free;
  FRequest.Free;
end;

procedure TMiniBrowserFrm.FormShow(Sender: TObject);
begin
  ShowStatusText('Initializing browser. Please wait...');

  // WebRTC's IP leaking can lowered/avoided by setting these preferences
  // To test this go to https://www.browserleaks.com/webrtc
  Chromium1.WebRTCIPHandlingPolicy := hpDisableNonProxiedUDP;
  Chromium1.WebRTCMultipleRoutes   := STATE_DISABLED;
  Chromium1.WebRTCNonproxiedUDP    := STATE_DISABLED;

  // GlobalCEFApp.GlobalContextInitialized has to be TRUE before creating any browser
  // If it's not initialized yet, we use a simple timer to create the browser later.
  if not(Chromium1.CreateBrowser(CEFWindowParent1, '')) then Timer1.Enabled := True;
end;

procedure TMiniBrowserFrm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  if not(Chromium1.CreateBrowser(CEFWindowParent1, '')) and not(Chromium1.Initialized) then
    Timer1.Enabled := True;
end;

procedure TMiniBrowserFrm.BrowserCreatedMsg(var aMessage : TMessage);
begin
  CEFWindowParent1.UpdateSize;
  NavControlPnl.Enabled := True;
  AddURL(MINIBROWSER_HOMEPAGE);
  Chromium1.LoadURL(MINIBROWSER_HOMEPAGE);
end;

procedure TMiniBrowserFrm.BrowserDestroyMsg(var aMessage : TMessage);
begin
  CEFWindowParent1.Free;
end;

procedure TMiniBrowserFrm.AddURL(const aURL : string);
begin
  if (URLCbx.Items.IndexOf(aURL) < 0) then URLCbx.Items.Add(aURL);

  URLCbx.Text := aURL;
end;

procedure TMiniBrowserFrm.ShowDevToolsMsg(var aMessage : TMessage);
var
  TempPoint : TPoint;
begin
  TempPoint.x := (aMessage.wParam shr 16) and $FFFF;
  TempPoint.y := aMessage.wParam and $FFFF;
  ShowDevTools(TempPoint);
end;

procedure TMiniBrowserFrm.HideDevToolsMsg(var aMessage : TMessage);
begin
  HideDevTools;
end;

procedure TMiniBrowserFrm.Inczoom1Click(Sender: TObject);
begin
  Chromium1.IncZoomStep;
end;

procedure TMiniBrowserFrm.Openfile1Click(Sender: TObject);
begin
  // This is a quick solution to load files. The file URL should be properly encoded.
  if OpenDialog1.Execute then Chromium1.LoadURL('file:///' + OpenDialog1.FileName);
end;

procedure TMiniBrowserFrm.PopupMenu1Popup(Sender: TObject);
begin
  if DevTools.Visible then
    DevTools1.Caption := 'Hide DevTools'
   else
    DevTools1.Caption := 'Show DevTools';
end;

procedure TMiniBrowserFrm.Preferences1Click(Sender: TObject);
begin
  case Chromium1.ProxyScheme of
    psSOCKS4 : PreferencesFrm.ProxySchemeCb.ItemIndex := 1;
    psSOCKS5 : PreferencesFrm.ProxySchemeCb.ItemIndex := 2;
    else       PreferencesFrm.ProxySchemeCb.ItemIndex := 0;
  end;

  PreferencesFrm.ProxyTypeCbx.ItemIndex  := Chromium1.ProxyType;
  PreferencesFrm.ProxyServerEdt.Text     := Chromium1.ProxyServer;
  PreferencesFrm.ProxyPortEdt.Text       := inttostr(Chromium1.ProxyPort);
  PreferencesFrm.ProxyUsernameEdt.Text   := Chromium1.ProxyUsername;
  PreferencesFrm.ProxyPasswordEdt.Text   := Chromium1.ProxyPassword;
  PreferencesFrm.ProxyScriptURLEdt.Text  := Chromium1.ProxyScriptURL;
  PreferencesFrm.ProxyByPassListEdt.Text := Chromium1.ProxyByPassList;
  PreferencesFrm.HeaderNameEdt.Text      := Chromium1.CustomHeaderName;
  PreferencesFrm.HeaderValueEdt.Text     := Chromium1.CustomHeaderValue;

  if (PreferencesFrm.ShowModal = mrOk) then
    begin
      Chromium1.ProxyType         := PreferencesFrm.ProxyTypeCbx.ItemIndex;
      Chromium1.ProxyServer       := PreferencesFrm.ProxyServerEdt.Text;
      Chromium1.ProxyPort         := strtoint(PreferencesFrm.ProxyPortEdt.Text);
      Chromium1.ProxyUsername     := PreferencesFrm.ProxyUsernameEdt.Text;
      Chromium1.ProxyPassword     := PreferencesFrm.ProxyPasswordEdt.Text;
      Chromium1.ProxyScriptURL    := PreferencesFrm.ProxyScriptURLEdt.Text;
      Chromium1.ProxyByPassList   := PreferencesFrm.ProxyByPassListEdt.Text;
      Chromium1.CustomHeaderName  := PreferencesFrm.HeaderNameEdt.Text;
      Chromium1.CustomHeaderValue := PreferencesFrm.HeaderValueEdt.Text;

      case PreferencesFrm.ProxySchemeCb.ItemIndex of
        1  : Chromium1.ProxyScheme := psSOCKS4;
        2  : Chromium1.ProxyScheme := psSOCKS5;
        else Chromium1.ProxyScheme := psHTTP;
      end;

      Chromium1.UpdatePreferences;
    end;
end;

procedure TMiniBrowserFrm.Print1Click(Sender: TObject);
begin
  Chromium1.Print;
end;

procedure TMiniBrowserFrm.PrintinPDF1Click(Sender: TObject);
begin
  SaveDialog1.DefaultExt := 'pdf';
  SaveDialog1.Filter     := 'PDF files (*.pdf)|*.PDF';

  if SaveDialog1.Execute and (length(SaveDialog1.FileName) > 0) then
    Chromium1.PrintToPDF(SaveDialog1.FileName, Chromium1.DocumentURL, Chromium1.DocumentURL);
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

procedure TMiniBrowserFrm.CopyHTMLMsg(var aMessage : TMessage);
begin
  Chromium1.RetrieveHTML;
end;

procedure TMiniBrowserFrm.CopyAllTextMsg(var aMessage : TMessage);
begin
  Chromium1.RetrieveText;
end;

procedure TMiniBrowserFrm.CopyFramesIDsMsg(var aMessage : TMessage);
var
  i          : NativeUInt;
  TempCount  : NativeUInt;
  TempArray  : TCefFrameIdentifierArray;
  TempString : string;
begin
  TempCount := Chromium1.FrameCount;

  if Chromium1.GetFrameIdentifiers(TempCount, TempArray) then
    begin
      TempString := '';
      i          := 0;

      while (i < TempCount) do
        begin
          TempString := TempString + inttostr(TempArray[i]) + CRLF;
          inc(i);
        end;

      clipboard.AsText := TempString;
    end;
end;

procedure TMiniBrowserFrm.CopyFramesNamesMsg(var aMessage : TMessage);
var
  TempSL : TStringList;
begin
  try
    TempSL := TStringList.Create;

    if Chromium1.GetFrameNames(TStrings(TempSL)) then clipboard.AsText := TempSL.Text;
  finally
    FreeAndNil(TempSL);
  end;
end;

procedure TMiniBrowserFrm.ShowResponseMsg(var aMessage : TMessage);
begin
  SimpleTextViewerFrm.Memo1.Lines.Clear;

  SimpleTextViewerFrm.Memo1.Lines.Add('--------------------------');
  SimpleTextViewerFrm.Memo1.Lines.Add('Request headers : ');
  SimpleTextViewerFrm.Memo1.Lines.Add('--------------------------');
  if (FRequest <> nil) then SimpleTextViewerFrm.Memo1.Lines.AddStrings(FRequest);

  SimpleTextViewerFrm.Memo1.Lines.Add('');

  SimpleTextViewerFrm.Memo1.Lines.Add('--------------------------');
  SimpleTextViewerFrm.Memo1.Lines.Add('Response headers : ');
  SimpleTextViewerFrm.Memo1.Lines.Add('--------------------------');
  if (FResponse <> nil) then SimpleTextViewerFrm.Memo1.Lines.AddStrings(FResponse);

  SimpleTextViewerFrm.ShowModal;
end;

procedure TMiniBrowserFrm.SavePreferencesMsg(var aMessage : TMessage);
begin
  SaveDialog1.DefaultExt := 'txt';
  SaveDialog1.Filter     := 'Text files (*.txt)|*.TXT';

  if SaveDialog1.Execute and (length(SaveDialog1.FileName) > 0) then
    Chromium1.SavePreferences(SaveDialog1.FileName);
end;

procedure TMiniBrowserFrm.TakeSnapshotMsg(var aMessage : TMessage);
var
  TempBitmap : TBitmap;
begin
  TempBitmap := nil;

  try
    SaveDialog1.DefaultExt := 'bmp';
    SaveDialog1.Filter     := 'Bitmap files (*.bmp)|*.BMP';

    if SaveDialog1.Execute and
       (length(SaveDialog1.FileName) > 0) and
       Chromium1.TakeSnapshot(TempBitmap) then
      TempBitmap.SaveToFile(SaveDialog1.FileName);
  finally
    if (TempBitmap <> nil) then FreeAndNil(TempBitmap);
  end;
end;

procedure TMiniBrowserFrm.WMMove(var aMessage : TWMMove);
begin
  inherited;

  if (Chromium1 <> nil) then Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TMiniBrowserFrm.WMMoving(var aMessage : TMessage);
begin
  inherited;

  if (Chromium1 <> nil) then Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TMiniBrowserFrm.WMEnterMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := True;
end;

procedure TMiniBrowserFrm.WMExitMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := False;
end;

procedure TMiniBrowserFrm.Deczoom1Click(Sender: TObject);
begin
  Chromium1.DecZoomStep;
end;

procedure TMiniBrowserFrm.DevTools1Click(Sender: TObject);
begin
  if DevTools.Visible then
    HideDevTools
   else
    ShowDevTools;
end;

procedure TMiniBrowserFrm.ShowDevTools(aPoint : TPoint);
begin
  Splitter1.Visible := True;
  DevTools.Visible  := True;
  DevTools.Width    := Width div 4;
  Chromium1.ShowDevTools(aPoint, DevTools);
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
  Chromium1.CloseDevTools(DevTools);
  Splitter1.Visible := False;
  DevTools.Visible  := False;
  DevTools.Width    := 0;
end;

end.
