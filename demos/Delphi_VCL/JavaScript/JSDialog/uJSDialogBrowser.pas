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

unit uJSDialogBrowser;

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, System.SyncObjs, System.UITypes,
  {$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, SyncObjs,
  {$ENDIF}
  uCEFChromium, uCEFWindowParent, uCEFChromiumWindow, uCEFInterfaces, uCEFTypes, uCEFConstants,
  uCEFWinControl, uCEFSentinel;

const
  CEFBROWSER_SHOWJSDIALOG               = WM_APP + $101;

type
  TJSDialogBrowserFrm = class(TForm)
    ChromiumWindow1: TChromiumWindow;
    AddressPnl: TPanel;
    AddressEdt: TEdit;
    GoBtn: TButton;
    Timer1: TTimer;
    procedure GoBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ChromiumWindow1AfterCreated(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ChromiumWindow1Close(Sender: TObject);
    procedure ChromiumWindow1BeforeClose(Sender: TObject);
    procedure CEFSentinel1Close(Sender: TObject);

  protected
    FJSDialogInfoCS    : TCriticalSection;
    FOriginUrl         : ustring;
    FMessageText       : ustring;
    FDefaultPromptText : ustring;
    FPendingDlg        : boolean;
    FDialogType        : TCefJsDialogType;
    FCallback          : ICefJsDialogCallback;

    // Variables to control when can we destroy the form safely
    FCanClose : boolean;  // Set to True in TChromium.OnBeforeClose
    FClosing  : boolean;  // Set to True in the CloseQuery event.

    procedure Chromium_OnJsdialog(Sender: TObject; const browser: ICefBrowser; const originUrl: ustring; dialogType: TCefJsDialogType; const messageText, defaultPromptText: ustring; const callback: ICefJsDialogCallback; out suppressMessage: Boolean; out Result: Boolean);
    procedure Chromium_OnBeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean; var Result: Boolean);

    procedure WMMove(var aMessage : TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage : TMessage); message WM_MOVING;
    procedure WMEnterMenuLoop(var aMessage: TMessage); message WM_ENTERMENULOOP;
    procedure WMExitMenuLoop(var aMessage: TMessage); message WM_EXITMENULOOP;
    procedure ShowJSDialogMsg(var aMessage: TMessage); message CEFBROWSER_SHOWJSDIALOG;

  end;

var
  JSDialogBrowserFrm: TJSDialogBrowserFrm;

procedure CreateGlobalCEFApp;

implementation

{$R *.dfm}

uses
  uCEFApplication;

// This is a demo with custom JS dialogs

// Destruction steps
// =================
// 1. The FormCloseQuery event sets CanClose to False and calls TChromiumWindow.CloseBrowser, which triggers the TChromiumWindow.OnClose event.
// 2. The TChromiumWindow.OnClose event calls TChromiumWindow.DestroyChildWindow which triggers the TChromiumWindow.OnBeforeClose event.
// 3. TChromiumWindow.OnBeforeClose sets FCanClose := True and sends WM_CLOSE to the form.

procedure CreateGlobalCEFApp;
begin
  GlobalCEFApp                  := TCefApplication.Create;
  //GlobalCEFApp.LogFile          := 'cef.log';
  //GlobalCEFApp.LogSeverity      := LOGSEVERITY_VERBOSE;
end;

procedure TJSDialogBrowserFrm.FormCreate(Sender: TObject);
begin
  FJSDialogInfoCS    := TCriticalSection.Create;
  FOriginUrl         := '';
  FMessageText       := '';
  FDefaultPromptText := '';
  FPendingDlg        := False;
  FDialogType        := JSDIALOGTYPE_ALERT;
  FCallback          := nil;
  FCanClose          := False;
  FClosing           := False;
end;

procedure TJSDialogBrowserFrm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FCanClose;

  if not(FClosing) then
    begin
      FClosing := True;
      Visible  := False;
      ChromiumWindow1.CloseBrowser(True);
    end;
end;

procedure TJSDialogBrowserFrm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FJSDialogInfoCS);
  FCallback          := nil;
end;

procedure TJSDialogBrowserFrm.FormShow(Sender: TObject);
begin
  ChromiumWindow1.ChromiumBrowser.OnJsdialog    := Chromium_OnJsdialog;
  ChromiumWindow1.ChromiumBrowser.OnBeforePopup := Chromium_OnBeforePopup;

  // GlobalCEFApp.GlobalContextInitialized has to be TRUE before creating any browser
  // If it's not initialized yet, we use a simple timer to create the browser later.
  if not(ChromiumWindow1.CreateBrowser) then Timer1.Enabled := True;
end;

procedure TJSDialogBrowserFrm.CEFSentinel1Close(Sender: TObject);
begin
  FCanClose := True;
  PostMessage(Handle, WM_CLOSE, 0, 0);
end;

procedure TJSDialogBrowserFrm.ChromiumWindow1AfterCreated(Sender: TObject);
begin
  Caption            := 'JS Dialog Browser';
  AddressPnl.Enabled := True;
  GoBtn.Click;
end;

procedure TJSDialogBrowserFrm.GoBtnClick(Sender: TObject);
begin
  ChromiumWindow1.LoadURL(AddressEdt.Text);
end;

procedure TJSDialogBrowserFrm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  if not(ChromiumWindow1.CreateBrowser) and not(ChromiumWindow1.Initialized) then
    Timer1.Enabled := True;
end;

procedure TJSDialogBrowserFrm.WMMove(var aMessage : TWMMove);
begin
  inherited;

  if (ChromiumWindow1 <> nil) then ChromiumWindow1.NotifyMoveOrResizeStarted;
end;

procedure TJSDialogBrowserFrm.WMMoving(var aMessage : TMessage);
begin
  inherited;

  if (ChromiumWindow1 <> nil) then ChromiumWindow1.NotifyMoveOrResizeStarted;
end;

procedure TJSDialogBrowserFrm.WMEnterMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := True;
end;

procedure TJSDialogBrowserFrm.WMExitMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := False;
end;

procedure TJSDialogBrowserFrm.Chromium_OnJsdialog(Sender : TObject;
                                                  const browser           : ICefBrowser;
                                                  const originUrl         : ustring;
                                                        dialogType        : TCefJsDialogType;
                                                  const messageText       : ustring;
                                                  const defaultPromptText : ustring;
                                                  const callback          : ICefJsDialogCallback;
                                                  out   suppressMessage   : Boolean;
                                                  out   Result            : Boolean);
begin
  // In this event we must store the dialog information and post a message to the main form to show the dialog
  FJSDialogInfoCS.Acquire;

  if FPendingDlg then
    begin
      Result          := False;
      suppressMessage := True;
    end
   else
    begin
      FOriginUrl         := originUrl;
      FMessageText       := messageText;
      FDefaultPromptText := defaultPromptText;
      FDialogType        := dialogType;
      FCallback          := callback;
      FPendingDlg        := True;
      Result             := True;
      suppressMessage    := False;

      PostMessage(Handle, CEFBROWSER_SHOWJSDIALOG, 0, 0);
    end;

  FJSDialogInfoCS.Release;
end;

procedure TJSDialogBrowserFrm.ChromiumWindow1BeforeClose(Sender: TObject);
begin
  FCanClose := True;
  PostMessage(Handle, WM_CLOSE, 0, 0);
end;

procedure TJSDialogBrowserFrm.ChromiumWindow1Close(Sender: TObject);
begin
  // DestroyChildWindow will destroy the child window created by CEF at the top of the Z order.
  if not(ChromiumWindow1.DestroyChildWindow) then
    begin
      FCanClose := True;
      PostMessage(Handle, WM_CLOSE, 0, 0);
    end;
end;

procedure TJSDialogBrowserFrm.Chromium_OnBeforePopup(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const targetUrl,
  targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition;
  userGesture: Boolean; const popupFeatures: TCefPopupFeatures;
  var windowInfo: TCefWindowInfo; var client: ICefClient;
  var settings: TCefBrowserSettings;
  var extra_info: ICefDictionaryValue;
  var noJavascriptAccess: Boolean;
  var Result: Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [WOD_NEW_FOREGROUND_TAB, WOD_NEW_BACKGROUND_TAB, WOD_NEW_POPUP, WOD_NEW_WINDOW]);
end;

procedure TJSDialogBrowserFrm.ShowJSDialogMsg(var aMessage: TMessage);
var
  TempCaption : string;
begin
  // Here we show the dialog and reset the information.
  // showmessage, MessageDlg and InputBox should be replaced by nicer custom forms with the same functionality.

  FJSDialogInfoCS.Acquire;

  if FPendingDlg then
    begin
      TempCaption := 'JavaScript message from : ' + FOriginUrl;

      case FDialogType of
        JSDIALOGTYPE_ALERT   : showmessage(TempCaption + CRLF + CRLF + FMessageText);
        JSDIALOGTYPE_CONFIRM : FCallback.cont((MessageDlg(TempCaption + CRLF + CRLF + FMessageText, mtConfirmation, [mbYes, mbNo], 0, mbYes) = mrYes), '');
        JSDIALOGTYPE_PROMPT  : FCallback.cont(True, InputBox(TempCaption, FMessageText, FDefaultPromptText));
      end;
    end;

  FOriginUrl         := '';
  FMessageText       := '';
  FDefaultPromptText := '';
  FPendingDlg        := False;
  FDialogType        := JSDIALOGTYPE_ALERT;
  FCallback          := nil;

  FJSDialogInfoCS.Release;
end;

end.
