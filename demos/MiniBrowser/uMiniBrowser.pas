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
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, System.Types, Vcl.ComCtrls,
  {$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Types, ComCtrls,
  {$ENDIF}
  uCEFChromium, uCEFWindowParent, uCEFInterfaces, uCEFApplication, uCEFTypes, uCEFConstants;

const
  MINIBROWSER_CREATED       = WM_APP + $100;
  MINIBROWSER_SHOWDEVTOOLS  = WM_APP + $101;
  MINIBROWSER_HIDEDEVTOOLS  = WM_APP + $102;

  MINIBROWSER_HOMEPAGE = 'http://www.google.com';

  MINIBROWSER_CONTEXTMENU_SHOWDEVTOOLS = MENU_ID_USER_FIRST + 1;
  MINIBROWSER_CONTEXTMENU_HIDEDEVTOOLS = MENU_ID_USER_FIRST + 2;
  MINIBROWSER_CONTEXTMENU_SHOWJSALERT  = MENU_ID_USER_FIRST + 3;
  MINIBROWSER_CONTEXTMENU_SETJSEVENT   = MENU_ID_USER_FIRST + 4;

type
  TMiniBrowserFrm = class(TForm)
    NavControlPnl: TPanel;
    NavButtonPnl: TPanel;
    URLEditPnl: TPanel;
    BackBtn: TButton;
    ForwardBtn: TButton;
    ReloadBtn: TButton;
    URLEdt: TEdit;
    CEFWindowParent1: TCEFWindowParent;
    Chromium1: TChromium;
    StopBtn: TButton;
    DevTools: TCEFWindowParent;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    procedure FormShow(Sender: TObject);
    procedure URLEdtKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
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

  protected
    procedure ShowDevTools(aPoint : TPoint);
    procedure HideDevTools;

    procedure BrowserCreatedMsg(var aMessage : TMessage); message MINIBROWSER_CREATED;
    procedure ShowDevToolsMsg(var aMessage : TMessage); message MINIBROWSER_SHOWDEVTOOLS;
    procedure HideDevToolsMsg(var aMessage : TMessage); message MINIBROWSER_HIDEDEVTOOLS;
  public

  end;

var
  MiniBrowserFrm : TMiniBrowserFrm;

implementation

{$R *.dfm}

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

procedure TMiniBrowserFrm.Chromium1AddressChange(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
begin
  URLEdt.Text := url;
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
  model.AddItem(MINIBROWSER_CONTEXTMENU_SETJSEVENT, 'Set mouseover event');

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

procedure TMiniBrowserFrm.Chromium1TitleChange(Sender: TObject;
  const browser: ICefBrowser; const title: ustring);
begin
  if (title <> '') then
    caption := 'MiniBrowser - ' + title
   else
    caption := 'MiniBrowser';
end;

procedure TMiniBrowserFrm.URLEdtKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = 13) then Chromium1.LoadURL(URLEdt.Text);
end;

procedure TMiniBrowserFrm.FormShow(Sender: TObject);
begin
  Chromium1.CreateBrowser(CEFWindowParent1, '');
end;

procedure TMiniBrowserFrm.BrowserCreatedMsg(var aMessage : TMessage);
begin
  NavControlPnl.Enabled := True;
  URLEdt.Text           := MINIBROWSER_HOMEPAGE;
  Chromium1.LoadURL(MINIBROWSER_HOMEPAGE);
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

procedure TMiniBrowserFrm.ShowDevTools(aPoint : TPoint);
begin
  Splitter1.Visible := True;
  DevTools.Visible  := True;
  DevTools.Width    := Width div 4;
  Chromium1.ShowDevTools(aPoint, DevTools);
end;

procedure TMiniBrowserFrm.HideDevTools;
begin
  Chromium1.CloseDevTools(DevTools);
  Splitter1.Visible := False;
  DevTools.Visible  := False;
  DevTools.Width    := 0;
end;

end.
