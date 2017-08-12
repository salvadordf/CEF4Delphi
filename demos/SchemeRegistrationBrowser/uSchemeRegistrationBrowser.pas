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

unit uSchemeRegistrationBrowser;

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Menus,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, System.Types, Vcl.ComCtrls, Vcl.ClipBrd,
  System.UITypes,
  {$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Menus,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Types, ComCtrls, ClipBrd,
  {$ENDIF}
  uCEFChromium, uCEFWindowParent, uCEFInterfaces, uCEFApplication, uCEFTypes, uCEFConstants;

const
  MINIBROWSER_CREATED        = WM_APP + $100;

  MINIBROWSER_CONTEXTMENU_REGSCHEME    = MENU_ID_USER_FIRST + 1;
  MINIBROWSER_CONTEXTMENU_CLEARFACT    = MENU_ID_USER_FIRST + 2;

type
  TSchemeRegistrationBrowserFrm = class(TForm)
    AddressBarPnl: TPanel;
    GoBtn: TButton;
    CEFWindowParent1: TCEFWindowParent;
    Chromium1: TChromium;
    AddressCbx: TComboBox;
    procedure Chromium1AfterCreated(Sender: TObject;
      const browser: ICefBrowser);
    procedure Chromium1BeforeContextMenu(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame;
      const params: ICefContextMenuParams; const model: ICefMenuModel);
    procedure Chromium1ContextMenuCommand(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame;
      const params: ICefContextMenuParams; commandId: Integer;
      eventFlags: Cardinal; out Result: Boolean);
    procedure GoBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure BrowserCreatedMsg(var aMessage : TMessage); message MINIBROWSER_CREATED;
    procedure WMMove(var aMessage : TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage : TMessage); message WM_MOVING;
  public
    { Public declarations }
  end;

var
  SchemeRegistrationBrowserFrm: TSchemeRegistrationBrowserFrm;

implementation

{$R *.dfm}

uses
  uCEFSchemeHandlerFactory, uHelloScheme;

procedure TSchemeRegistrationBrowserFrm.Chromium1AfterCreated(
  Sender: TObject; const browser: ICefBrowser);
begin
  PostMessage(Handle, MINIBROWSER_CREATED, 0, 0);
end;

procedure TSchemeRegistrationBrowserFrm.Chromium1BeforeContextMenu(
  Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; const model: ICefMenuModel);
begin
  model.AddItem(MINIBROWSER_CONTEXTMENU_REGSCHEME,   'Register scheme');
  model.AddItem(MINIBROWSER_CONTEXTMENU_CLEARFACT,   'Clear schemes');
end;

procedure TSchemeRegistrationBrowserFrm.Chromium1ContextMenuCommand(
  Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; commandId: Integer;
  eventFlags: Cardinal; out Result: Boolean);
var
  TempFactory: ICefSchemeHandlerFactory;
begin
  Result := False;

  case commandId of
    MINIBROWSER_CONTEXTMENU_REGSCHEME :
      if (browser <> nil) and
         (browser.host <> nil) and
         (browser.host.RequestContext <> nil) then
        begin
          // You can register the Scheme Handler Factory in the DPR file or later, for example in a context menu command.
          TempFactory := TCefSchemeHandlerFactoryOwn.Create(THelloScheme);
          if not(browser.host.RequestContext.RegisterSchemeHandlerFactory('hello', '', TempFactory)) then
            MessageDlg('RegisterSchemeHandlerFactory error !', mtError, [mbOk], 0);
        end;

    MINIBROWSER_CONTEXTMENU_CLEARFACT :
      if (browser <> nil) and
         (browser.host <> nil) and
         (browser.host.RequestContext <> nil) then
        begin
          if not(browser.host.RequestContext.ClearSchemeHandlerFactories) then
            MessageDlg('ClearSchemeHandlerFactories error !', mtError, [mbOk], 0);
        end;
  end;
end;

procedure TSchemeRegistrationBrowserFrm.FormShow(Sender: TObject);
begin
  Chromium1.CreateBrowser(CEFWindowParent1, '');
end;

procedure TSchemeRegistrationBrowserFrm.GoBtnClick(Sender: TObject);
begin
  Chromium1.LoadURL(AddressCbx.Text);
end;

procedure TSchemeRegistrationBrowserFrm.BrowserCreatedMsg(var aMessage : TMessage);
begin
  AddressBarPnl.Enabled := True;
  GoBtn.Click;
end;

procedure TSchemeRegistrationBrowserFrm.WMMove(var aMessage : TWMMove);
begin
  inherited;

  if (Chromium1 <> nil) then Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TSchemeRegistrationBrowserFrm.WMMoving(var aMessage : TMessage);
begin
  inherited;

  if (Chromium1 <> nil) then Chromium1.NotifyMoveOrResizeStarted;
end;

end.
