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
//        Copyright © 2017 Salvador Díaz Fau. All rights reserved.
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
  {$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Menus,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Types, ComCtrls, ClipBrd,
  {$ENDIF}
  uCEFChromium, uCEFWindowParent, uCEFInterfaces, uCEFApplication, uCEFTypes, uCEFConstants;

const
  MINIBROWSER_CREATED       = WM_APP + $100;
  MINIBROWSER_SHOWDEVTOOLS  = WM_APP + $101;
  MINIBROWSER_HIDEDEVTOOLS  = WM_APP + $102;
  MINIBROWSER_COPYHTML      = WM_APP + $103;
  MINIBROWSER_VISITDOM      = WM_APP + $104;

  MINIBROWSER_HOMEPAGE = 'https://www.google.com';

  MINIBROWSER_CONTEXTMENU_SHOWDEVTOOLS = MENU_ID_USER_FIRST + 1;
  MINIBROWSER_CONTEXTMENU_HIDEDEVTOOLS = MENU_ID_USER_FIRST + 2;
  MINIBROWSER_CONTEXTMENU_SHOWJSALERT  = MENU_ID_USER_FIRST + 3;
  MINIBROWSER_CONTEXTMENU_SETJSEVENT   = MENU_ID_USER_FIRST + 4;
  MINIBROWSER_CONTEXTMENU_COPYHTML     = MENU_ID_USER_FIRST + 5;
  MINIBROWSER_CONTEXTMENU_VISITDOM     = MENU_ID_USER_FIRST + 6;

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
    procedure Chromium1ContextMenuCommand(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame;
      const params: ICefContextMenuParams; commandId: Integer;
      eventFlags: TCefEventFlags; out Result: Boolean);
    procedure Chromium1ProcessMessageReceived(Sender: TObject;
      const browser: ICefBrowser; sourceProcess: TCefProcessId;
      const message: ICefProcessMessage; out Result: Boolean);
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

  protected
    procedure AddURL(const aURL : string);

    procedure ShowDevTools(aPoint : TPoint); overload;
    procedure ShowDevTools; overload;
    procedure HideDevTools;

    procedure BrowserCreatedMsg(var aMessage : TMessage); message MINIBROWSER_CREATED;
    procedure ShowDevToolsMsg(var aMessage : TMessage); message MINIBROWSER_SHOWDEVTOOLS;
    procedure HideDevToolsMsg(var aMessage : TMessage); message MINIBROWSER_HIDEDEVTOOLS;
    procedure CopyHTMLMsg(var aMessage : TMessage); message MINIBROWSER_COPYHTML;
    procedure VisitDOMMsg(var aMessage : TMessage); message MINIBROWSER_VISITDOM;
  public

  end;

var
  MiniBrowserFrm : TMiniBrowserFrm;

implementation

{$R *.dfm}

uses
  uPreferences, uCEFProcessMessage;

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

procedure TMiniBrowserFrm.Chromium1AddressChange(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
begin
  AddURL(url);
end;

procedure TMiniBrowserFrm.Chromium1AfterCreated(Sender: TObject;
  const browser: ICefBrowser);
begin
  PostMessage(Handle, MINIBROWSER_CREATED, 0, 0);
end;

procedure TMiniBrowserFrm.Chromium1BeforeContextMenu(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; const model: ICefMenuModel);
begin
  model.AddSeparator;
  model.AddItem(MINIBROWSER_CONTEXTMENU_SHOWJSALERT, 'Show JS Alert');
  model.AddItem(MINIBROWSER_CONTEXTMENU_SETJSEVENT,  'Set mouseover event');
  model.AddItem(MINIBROWSER_CONTEXTMENU_COPYHTML,    'Copy HTML to clipboard');
  model.AddItem(MINIBROWSER_CONTEXTMENU_VISITDOM,    'Visit DOM');

  if DevTools.Visible then
    model.AddItem(MINIBROWSER_CONTEXTMENU_HIDEDEVTOOLS, 'Hide DevTools')
   else
    model.AddItem(MINIBROWSER_CONTEXTMENU_SHOWDEVTOOLS, 'Show DevTools');
end;

procedure TMiniBrowserFrm.Chromium1ContextMenuCommand(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; commandId: Integer;
  eventFlags: TCefEventFlags; out Result: Boolean);
var
  TempParam : WParam;
begin
  Result := False;

  case commandId of
    MINIBROWSER_CONTEXTMENU_HIDEDEVTOOLS :
      PostMessage(Handle, MINIBROWSER_HIDEDEVTOOLS, 0, 0);

    MINIBROWSER_CONTEXTMENU_SHOWDEVTOOLS :
      begin
        TempParam := ((params.XCoord and $FFFF) shl 16) or (params.YCoord and $FFFF);
        PostMessage(Handle, MINIBROWSER_SHOWDEVTOOLS, TempParam, 0);
      end;

    MINIBROWSER_CONTEXTMENU_SHOWJSALERT :
      if (browser <> nil) and (browser.MainFrame <> nil) then
        browser.MainFrame.ExecuteJavaScript('alert(''JavaScript execute works!'');', 'about:blank', 0);

    MINIBROWSER_CONTEXTMENU_SETJSEVENT :
      if (browser <> nil) and (browser.MainFrame <> nil) then
        browser.MainFrame.ExecuteJavaScript(
          'document.body.addEventListener("mouseover", function(evt){'+
            'function getpath(n){'+
              'var ret = "<" + n.nodeName + ">";'+
              'if (n.parentNode){return getpath(n.parentNode) + ret} else '+
              'return ret'+
            '};'+
            'app.mouseover(getpath(evt.target))}'+
          ')', 'about:blank', 0);

    MINIBROWSER_CONTEXTMENU_COPYHTML :
      PostMessage(Handle, MINIBROWSER_COPYHTML, 0, 0);

    MINIBROWSER_CONTEXTMENU_VISITDOM :
      PostMessage(Handle, MINIBROWSER_VISITDOM, 0, 0);
  end;
end;

procedure TMiniBrowserFrm.Chromium1LoadingStateChange(Sender: TObject;
  const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
begin
  BackBtn.Enabled    := canGoBack;
  ForwardBtn.Enabled := canGoForward;
  ReloadBtn.Enabled  := not(isLoading);
  StopBtn.Enabled    := isLoading;
end;

procedure TMiniBrowserFrm.Chromium1ProcessMessageReceived(Sender: TObject;
  const browser: ICefBrowser; sourceProcess: TCefProcessId;
  const message: ICefProcessMessage; out Result: Boolean);
begin
  if (message <> nil) and (message.Name = 'mouseover') and (message.ArgumentList <> nil) then
    begin
      StatusBar1.Panels[0].Text := message.ArgumentList.GetString(0);
      Result                    := True;
    end
   else
    Result := False;
end;

procedure TMiniBrowserFrm.Chromium1StatusMessage(Sender: TObject;
  const browser: ICefBrowser; const value: ustring);
begin
  StatusBar1.Panels[0].Text := value;
end;

procedure TMiniBrowserFrm.Chromium1TextResultAvailable(Sender: TObject; const aText: string);
begin
  clipboard.AsText := aText;
end;

procedure TMiniBrowserFrm.Chromium1TitleChange(Sender: TObject;
  const browser: ICefBrowser; const title: ustring);
begin
  if (title <> '') then
    caption := 'MiniBrowser - ' + title
   else
    caption := 'MiniBrowser';
end;

procedure TMiniBrowserFrm.FormShow(Sender: TObject);
begin
  Chromium1.CreateBrowser(CEFWindowParent1, '');
end;

procedure TMiniBrowserFrm.BrowserCreatedMsg(var aMessage : TMessage);
begin
  NavControlPnl.Enabled := True;
  AddURL(MINIBROWSER_HOMEPAGE);
  Chromium1.LoadURL(MINIBROWSER_HOMEPAGE);
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

procedure TMiniBrowserFrm.PopupMenu1Popup(Sender: TObject);
begin
  if DevTools.Visible then
    DevTools1.Caption := 'Hide DevTools'
   else
    DevTools1.Caption := 'Show DevTools';
end;

procedure TMiniBrowserFrm.Preferences1Click(Sender: TObject);
begin
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

procedure TMiniBrowserFrm.VisitDOMMsg(var aMessage : TMessage);
var
  TempMsg : ICefProcessMessage;
begin
  // Only works using a TCefCustomRenderProcessHandler. See MiniBrowser demo.
  if Chromium1.Initialized then
    begin
      // Use the ArgumentList property if you need to pass some parameters.
      TempMsg := TCefProcessMessageRef.New('retrievedom'); // Same name than TCefCustomRenderProcessHandler.MessageName
      Chromium1.Browser.SendProcessMessage(PID_RENDERER, TempMsg);
    end;
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
