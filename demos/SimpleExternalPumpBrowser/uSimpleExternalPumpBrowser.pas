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

unit uSimpleExternalPumpBrowser;

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
  uCEFChromium, uCEFWindowParent, uCEFTypes, uCEFConstants, uCEFInterfaces, uCEFWorkScheduler,
  uCEFChromiumWindow;

type
  TSimpleExternalPumpBrowserFrm = class(TForm)
    AddressPnl: TPanel;
    GoBtn: TButton;
    Timer1: TTimer;
    URLCbx: TComboBox;
    ChromiumWindow1: TChromiumWindow;

    procedure GoBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);

    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    // These 3 TChromiumWindow events are called in the main app thread, so you can do whatever you want with the GUI.
    procedure ChromiumWindow1AfterCreated(Sender: TObject);
    procedure ChromiumWindow1BeforeClose(Sender: TObject);
    procedure ChromiumWindow1Close(Sender: TObject);

  protected
    FCanClose : boolean;
    FClosing  : boolean;

    procedure WMMove(var aMessage : TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage : TMessage); message WM_MOVING;
    procedure WMEnterMenuLoop(var aMessage: TMessage); message WM_ENTERMENULOOP;
    procedure WMExitMenuLoop(var aMessage: TMessage); message WM_EXITMENULOOP;
  end;

var
  SimpleExternalPumpBrowserFrm : TSimpleExternalPumpBrowserFrm;
  GlobalCEFWorkScheduler : TCEFWorkScheduler = nil;

procedure GlobalCEFApp_OnScheduleMessagePumpWork(const aDelayMS : int64);

implementation

{$R *.dfm}

uses
  uCEFApplication;

// This demo has a simple browser with a TChromiumWindow using the "External message pump" mode
// to schedule the cef_do_message_loop_work calls thanks to the TCEFWorkScheduler class.

// It was necessary to destroy the browser with the following destruction sequence :
// 1. The FormCloseQuery event sets CanClose to False and calls TChromiumWindow.CloseBrowser, which triggers the TChromiumWindow.OnClose event.
// 2. The TChromiumWindow.OnClose event calls TChromiumWindow.DestroyChildWindow which triggers the TChromiumWindow.OnBeforeClose event.
// 3. TChromiumWindow.OnBeforeClose sets FCanClose to True and closes the form.

procedure GlobalCEFApp_OnScheduleMessagePumpWork(const aDelayMS : int64);
begin
  if (GlobalCEFWorkScheduler <> nil) then GlobalCEFWorkScheduler.ScheduleMessagePumpWork(aDelayMS);
end;

procedure TSimpleExternalPumpBrowserFrm.FormCreate(Sender: TObject);
begin
  FCanClose := False;
  FClosing  := False;
end;

procedure TSimpleExternalPumpBrowserFrm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FCanClose;

  if not(FClosing) then
    begin
      FClosing           := True;
      Visible            := False;
      AddressPnl.Enabled := False;
      ChromiumWindow1.CloseBrowser(True);
    end;
end;

procedure TSimpleExternalPumpBrowserFrm.FormShow(Sender: TObject);
begin
  // GlobalCEFApp.GlobalContextInitialized has to be TRUE before creating any browser
  // If it's not initialized yet, we use a simple timer to create the browser later.
  if not(ChromiumWindow1.CreateBrowser) then Timer1.Enabled := True;
end;

procedure TSimpleExternalPumpBrowserFrm.ChromiumWindow1AfterCreated(Sender: TObject);
begin
  Caption            := 'Simple External Pump Browser';
  AddressPnl.Enabled := True;
  GoBtn.Click;
end;

procedure TSimpleExternalPumpBrowserFrm.ChromiumWindow1BeforeClose(Sender: TObject);
begin
  FCanClose := True;
  Close;
end;

procedure TSimpleExternalPumpBrowserFrm.ChromiumWindow1Close(Sender: TObject);
begin
  // DestroyChildWindow will destroy the child window created by CEF at the top of the Z order.
  ChromiumWindow1.DestroyChildWindow;
end;

procedure TSimpleExternalPumpBrowserFrm.GoBtnClick(Sender: TObject);
begin
  ChromiumWindow1.LoadURL(URLCbx.Text);
end;

procedure TSimpleExternalPumpBrowserFrm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  if not(ChromiumWindow1.CreateBrowser) and not(ChromiumWindow1.Initialized) then
    Timer1.Enabled := True;
end;

procedure TSimpleExternalPumpBrowserFrm.WMMove(var aMessage : TWMMove);
begin
  inherited;

  if (ChromiumWindow1 <> nil) then ChromiumWindow1.NotifyMoveOrResizeStarted;
end;

procedure TSimpleExternalPumpBrowserFrm.WMMoving(var aMessage : TMessage);
begin
  inherited;

  if (ChromiumWindow1 <> nil) then ChromiumWindow1.NotifyMoveOrResizeStarted;
end;

procedure TSimpleExternalPumpBrowserFrm.WMEnterMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := True;
end;

procedure TSimpleExternalPumpBrowserFrm.WMExitMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := False;
end;

end.
