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

unit uJSDialogBrowser;

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, System.SyncObjs,
  {$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, SyncObjs,
  {$ENDIF}
  uCEFChromium, uCEFWindowParent, uCEFChromiumWindow, uCEFInterfaces, uCEFTypes, uCEFConstants;

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

  protected
    FJSDialogInfoCS    : TCriticalSection;
    FOriginUrl         : ustring;
    FMessageText       : ustring;
    FDefaultPromptText : ustring;
    FPendingDlg        : boolean;
    FDialogType        : TCefJsDialogType;
    FCallback          : ICefJsDialogCallback;

    procedure ChromiumBrowser_OnJsdialog(Sender: TObject; const browser: ICefBrowser; const originUrl: ustring; dialogType: TCefJsDialogType; const messageText, defaultPromptText: ustring; const callback: ICefJsDialogCallback; out suppressMessage: Boolean; out Result: Boolean);

    procedure WMMove(var aMessage : TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage : TMessage); message WM_MOVING;
    procedure WMEnterMenuLoop(var aMessage: TMessage); message WM_ENTERMENULOOP;
    procedure WMExitMenuLoop(var aMessage: TMessage); message WM_EXITMENULOOP;
    procedure ShowJSDialogMsg(var aMessage: TMessage); message CEFBROWSER_SHOWJSDIALOG;

  end;

var
  JSDialogBrowserFrm: TJSDialogBrowserFrm;

implementation

{$R *.dfm}

uses
  uCEFApplication;

// This is a demo with custom JS dialogs

procedure TJSDialogBrowserFrm.FormCreate(Sender: TObject);
begin
  FJSDialogInfoCS    := TCriticalSection.Create;
  FOriginUrl         := '';
  FMessageText       := '';
  FDefaultPromptText := '';
  FPendingDlg        := False;
  FDialogType        := JSDIALOGTYPE_ALERT;
  FCallback          := nil;
end;

procedure TJSDialogBrowserFrm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FJSDialogInfoCS);
  FCallback          := nil;
end;

procedure TJSDialogBrowserFrm.FormShow(Sender: TObject);
begin
  ChromiumWindow1.ChromiumBrowser.OnJsdialog := ChromiumBrowser_OnJsdialog;

  // GlobalCEFApp.GlobalContextInitialized has to be TRUE before creating any browser
  // If it's not initialized yet, we use a simple timer to create the browser later.
  if not(ChromiumWindow1.CreateBrowser) then Timer1.Enabled := True;
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

procedure TJSDialogBrowserFrm.ChromiumBrowser_OnJsdialog(Sender : TObject;
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
