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

unit uDOMVisitor;

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
  MINIBROWSER_VISITDOM               = WM_APP + $101;

  MINIBROWSER_CONTEXTMENU_VISITDOM   = MENU_ID_USER_FIRST + 1;

  DOMVISITOR_MSGNAME  = 'domvisitor';
  RETRIEVEDOM_MSGNAME = 'retrievedom';

type
  TDOMVisitorFrm = class(TForm)
    CEFWindowParent1: TCEFWindowParent;
    Chromium1: TChromium;
    AddressBarPnl: TPanel;
    GoBtn: TButton;
    AddressEdt: TEdit;
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    procedure GoBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Chromium1AfterCreated(Sender: TObject;
      const browser: ICefBrowser);
    procedure Chromium1BeforeContextMenu(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame;
      const params: ICefContextMenuParams; const model: ICefMenuModel);
    procedure Chromium1ContextMenuCommand(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame;
      const params: ICefContextMenuParams; commandId: Integer;
      eventFlags: Cardinal; out Result: Boolean);
    procedure Chromium1ProcessMessageReceived(Sender: TObject;
      const browser: ICefBrowser; sourceProcess: TCefProcessId;
      const message: ICefProcessMessage; out Result: Boolean);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure BrowserCreatedMsg(var aMessage : TMessage); message CEF_AFTERCREATED;
    procedure VisitDOMMsg(var aMessage : TMessage); message MINIBROWSER_VISITDOM;
    procedure WMMove(var aMessage : TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage : TMessage); message WM_MOVING;

    procedure ShowStatusText(const aText : string);
  public
    { Public declarations }
  end;

var
  DOMVisitorFrm: TDOMVisitorFrm;

procedure GlobalCEFApp_OnProcessMessageReceived(const browser       : ICefBrowser;
                                                      sourceProcess : TCefProcessId;
                                                const message       : ICefProcessMessage;
                                                var   aHandled      : boolean);

implementation

{$R *.dfm}

uses
  uCEFProcessMessage, uCEFMiscFunctions, uCEFSchemeRegistrar, uCEFRenderProcessHandler,
  uCEFv8Handler, uCEFDomVisitor, uCEFDomNode, uCEFTask;

procedure SimpleDOMIteration(const aDocument: ICefDomDocument);
var
  TempHead, TempChild : ICefDomNode;
begin
  try
    if (aDocument <> nil) then
      begin
        TempHead := aDocument.Head;

        if (TempHead <> nil) then
          begin
            TempChild := TempHead.FirstChild;

            while (TempChild <> nil) do
              begin
                CefLog('CEF4Delphi', 1, CEF_LOG_SEVERITY_ERROR, 'Head child element : ' + TempChild.Name);
                TempChild := TempChild.NextSibling;
              end;
          end;
      end;
  except
    on e : exception do
      if CustomExceptionHandler('SimpleDOMIteration', e) then raise;
  end;
end;

procedure SimpleNodeSearch(const aDocument: ICefDomDocument);
const
  NODE_ID = 'lst-ib'; // node found in google.com homepage
var
  TempNode : ICefDomNode;
begin
  try
    if (aDocument <> nil) then
      begin
        TempNode := aDocument.GetElementById(NODE_ID);

        if (TempNode <> nil) then
          CefLog('CEF4Delphi', 1, CEF_LOG_SEVERITY_ERROR, NODE_ID + ' element name : ' + TempNode.Name);

        TempNode := aDocument.GetFocusedNode;

        if (TempNode <> nil) then
          CefLog('CEF4Delphi', 1, CEF_LOG_SEVERITY_ERROR, 'Focused element name : ' + TempNode.Name);
      end;
  except
    on e : exception do
      if CustomExceptionHandler('SimpleNodeSearch', e) then raise;
  end;
end;

procedure DOMVisitor_OnDocAvailable(const browser: ICefBrowser; const document: ICefDomDocument);
var
  msg: ICefProcessMessage;
begin
  // This function is called from a different process.
  // document is only valid inside this function.
  // As an example, this function only writes the document title to the 'debug.log' file.
  CefLog('CEF4Delphi', 1, CEF_LOG_SEVERITY_ERROR, 'document.Title : ' + document.Title);

  // Simple DOM iteration example
  SimpleDOMIteration(document);

  // Simple DOM searches
  SimpleNodeSearch(document);

  // Sending back some custom results to the browser process
  // Notice that the DOMVISITOR_MSGNAME message name needs to be recognized in
  // Chromium1ProcessMessageReceived
  msg := TCefProcessMessageRef.New(DOMVISITOR_MSGNAME);
  msg.ArgumentList.SetString(0, 'document.Title : ' + document.Title);
  browser.SendProcessMessage(PID_BROWSER, msg);
end;

procedure GlobalCEFApp_OnProcessMessageReceived(const browser       : ICefBrowser;
                                                      sourceProcess : TCefProcessId;
                                                const message       : ICefProcessMessage;
                                                var   aHandled      : boolean);
var
  TempFrame   : ICefFrame;
  TempVisitor : TCefFastDomVisitor2;
begin
  if (browser <> nil) and (message.name = RETRIEVEDOM_MSGNAME) then
    begin
      TempFrame := browser.MainFrame;

      if (TempFrame <> nil) then
        begin
          TempVisitor := TCefFastDomVisitor2.Create(browser, DOMVisitor_OnDocAvailable);
          TempFrame.VisitDom(TempVisitor);
        end;

      aHandled := True;
    end
   else
    aHandled := False;
end;

procedure TDOMVisitorFrm.Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  PostMessage(Handle, CEF_AFTERCREATED, 0, 0);
end;

procedure TDOMVisitorFrm.Chromium1BeforeContextMenu(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; const model: ICefMenuModel);
begin
  model.AddItem(MINIBROWSER_CONTEXTMENU_VISITDOM,    'Visit DOM in CEF');
end;

procedure TDOMVisitorFrm.Chromium1ContextMenuCommand(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; commandId: Integer;
  eventFlags: Cardinal; out Result: Boolean);
begin
  Result := False;

  case commandId of
    MINIBROWSER_CONTEXTMENU_VISITDOM :
      PostMessage(Handle, MINIBROWSER_VISITDOM, 0, 0);
  end;
end;

procedure TDOMVisitorFrm.Chromium1ProcessMessageReceived(Sender: TObject;
  const browser: ICefBrowser; sourceProcess: TCefProcessId;
  const message: ICefProcessMessage; out Result: Boolean);
begin
  Result := False;

  if (message = nil) or (message.ArgumentList = nil) then exit;

  if (message.Name = DOMVISITOR_MSGNAME) then
    begin
      // Message received from the DOMVISITOR in CEF
      ShowStatusText('DOM Visitor result text : ' + message.ArgumentList.GetString(0));
      Result := True;
    end;
end;

procedure TDOMVisitorFrm.FormShow(Sender: TObject);
begin
  // GlobalCEFApp.GlobalContextInitialized has to be TRUE before creating any browser
  // If it's not initialized yet, we use a simple timer to create the browser later.
  if not(Chromium1.CreateBrowser(CEFWindowParent1, '')) then Timer1.Enabled := True;
end;

procedure TDOMVisitorFrm.GoBtnClick(Sender: TObject);
begin
  Chromium1.LoadURL(AddressEdt.Text);
end;

procedure TDOMVisitorFrm.BrowserCreatedMsg(var aMessage : TMessage);
begin
  CEFWindowParent1.UpdateSize;
  AddressBarPnl.Enabled := True;
  GoBtn.Click;
end;

procedure TDOMVisitorFrm.VisitDOMMsg(var aMessage : TMessage);
var
  TempMsg : ICefProcessMessage;
begin
  // Only works using a TCefCustomRenderProcessHandler.
  // Use the ArgumentList property if you need to pass some parameters.
  TempMsg := TCefProcessMessageRef.New(RETRIEVEDOM_MSGNAME); // Same name than TCefCustomRenderProcessHandler.MessageName
  Chromium1.SendProcessMessage(PID_RENDERER, TempMsg);
end;

procedure TDOMVisitorFrm.WMMove(var aMessage : TWMMove);
begin
  inherited;

  if (Chromium1 <> nil) then Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TDOMVisitorFrm.WMMoving(var aMessage : TMessage);
begin
  inherited;

  if (Chromium1 <> nil) then Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TDOMVisitorFrm.ShowStatusText(const aText : string);
begin
  StatusBar1.Panels[0].Text := aText;
end;

procedure TDOMVisitorFrm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  if not(Chromium1.CreateBrowser(CEFWindowParent1, '')) and not(Chromium1.Initialized) then
    Timer1.Enabled := True;
end;

end.
