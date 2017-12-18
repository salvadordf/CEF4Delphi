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

unit uPostDataInspector;

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  {$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  {$ENDIF}
  uCEFChromium, uCEFWindowParent, uCEFInterfaces, uCEFApplication, uCEFTypes, uCEFConstants;

const
  POSTDATA_MSGNAME   = 'postdatainfo';

type
  TPostDataInspectorFrm = class(TForm)
    StatusBar1: TStatusBar;
    CEFWindowParent1: TCEFWindowParent;
    Chromium1: TChromium;
    NavControlPnl: TPanel;
    Edit1: TEdit;
    GoBtn: TButton;
    Timer1: TTimer;
    procedure GoBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Chromium1ProcessMessageReceived(Sender: TObject;
      const browser: ICefBrowser; sourceProcess: TCefProcessId;
      const message: ICefProcessMessage; out Result: Boolean);
    procedure Chromium1AfterCreated(Sender: TObject;
      const browser: ICefBrowser);
    procedure Timer1Timer(Sender: TObject);
  protected

    procedure BrowserCreatedMsg(var aMessage : TMessage); message CEF_AFTERCREATED;
    procedure WMMove(var aMessage : TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage : TMessage); message WM_MOVING;
    procedure WMEnterMenuLoop(var aMessage: TMessage); message WM_ENTERMENULOOP;
    procedure WMExitMenuLoop(var aMessage: TMessage); message WM_EXITMENULOOP;
  public
    { Public declarations }
  end;

var
  PostDataInspectorFrm: TPostDataInspectorFrm;

procedure GlobalCEFApp_OnBeforeNavigation(const browser         : ICefBrowser;
                                          const frame           : ICefFrame;
                                          const request         : ICefRequest;
                                                navigationType  : TCefNavigationType;
                                                isRedirect      : Boolean;
                                          var   aStopNavigation : boolean);

implementation

{$R *.dfm}

uses
  uCEFRenderProcessHandler, uCEFProcessMessage;

// This demo shows an alternative way to inspect the POST data in read only mode.
// This data is not always available in the request of TChromium.OnBeforeBrowse
// so this demo uses the TCefCustomRenderProcessHandler.OnBeforeNavigationEvent event
// and sends the results in a process message.
// That message is received in the Chromium1ProcessMessageReceived function.

// If you need more control over the POST data register an scheme.
// See the SchemeRegistrationBrowser demo.

procedure GlobalCEFApp_OnBeforeNavigation(const browser         : ICefBrowser;
                                          const frame           : ICefFrame;
                                          const request         : ICefRequest;
                                                navigationType  : TCefNavigationType;
                                                isRedirect      : Boolean;
                                          var   aStopNavigation : boolean);
var
  msg: ICefProcessMessage;
  TempString : string;
begin
  aStopNavigation := False;

  if (request = nil) then
    TempString := 'no request'
   else
    if (request.postdata = nil) then
      TempString := 'no postdata'
     else
      TempString := 'postdata elements : ' + inttostr(request.postdata.GetCount);

  msg := TCefProcessMessageRef.New(POSTDATA_MSGNAME);
  msg.ArgumentList.SetString(0, TempString);
  browser.SendProcessMessage(PID_BROWSER, msg);
end;

procedure TPostDataInspectorFrm.Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  PostMessage(Handle, CEF_AFTERCREATED, 0, 0);
end;

procedure TPostDataInspectorFrm.Chromium1ProcessMessageReceived(
  Sender: TObject; const browser: ICefBrowser;
  sourceProcess: TCefProcessId; const message: ICefProcessMessage;
  out Result: Boolean);
begin
  Result := False;

  if (message = nil) or (message.ArgumentList = nil) then exit;

  // Many of these events are received in different threads and the VCL
  // doesn't like to create and destroy components in different threads.

  // It's safer to store the results and send a message to the main thread to show them.

  // The message names is also used in the ProcessHandler_OnBeforeNavigationEvent function of the DPR file.

  if (message.Name = POSTDATA_MSGNAME) then
    begin
      StatusBar1.Panels[0].Text := message.ArgumentList.GetString(0); // this doesn't create/destroy components
      Result := True;
    end;
end;

procedure TPostDataInspectorFrm.FormShow(Sender: TObject);
begin
  // GlobalCEFApp.GlobalContextInitialized has to be TRUE before creating any browser
  // If it's not initialized yet, we use a simple timer to create the browser later.
  if not(Chromium1.CreateBrowser(CEFWindowParent1, '')) then Timer1.Enabled := True;
end;

procedure TPostDataInspectorFrm.GoBtnClick(Sender: TObject);
begin
  Chromium1.LoadURL(Edit1.Text);
end;

procedure TPostDataInspectorFrm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  if not(Chromium1.CreateBrowser(CEFWindowParent1, '')) and not(Chromium1.Initialized) then
    Timer1.Enabled := True;
end;

procedure TPostDataInspectorFrm.WMMove(var aMessage : TWMMove);
begin
  inherited;

  if (Chromium1 <> nil) then Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TPostDataInspectorFrm.WMMoving(var aMessage : TMessage);
begin
  inherited;

  if (Chromium1 <> nil) then Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TPostDataInspectorFrm.WMEnterMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := True;
end;

procedure TPostDataInspectorFrm.WMExitMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := False;
end;

procedure TPostDataInspectorFrm.BrowserCreatedMsg(var aMessage : TMessage);
begin
  CEFWindowParent1.UpdateSize;
  NavControlPnl.Enabled := True;
  GoBtn.Click;
end;

end.
