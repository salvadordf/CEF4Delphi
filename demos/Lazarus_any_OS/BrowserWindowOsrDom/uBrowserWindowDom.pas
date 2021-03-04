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

unit uBrowserWindowDom;

{$mode objfpc}{$H+}
{$I cef.inc}

interface

uses
  GlobalCefApplication,
  uCEFLazarusCocoa, // required for Cocoa
  SysUtils, Messages, Forms, Controls, Dialogs, ExtCtrls, StdCtrls, LMessages,
  Menus, Graphics, uCEFTypes, uCEFInterfaces, uHelperProcessDom,
  uCEFWorkScheduler, uCEFLazarusBrowserWindow, uCEFProcessMessage,
  uCEFLazarusOsrBrowserWindow, Classes;

type

  { TForm1 }

  TForm1 = class(TForm)
    AddressEdt: TComboBox;
    GoBtn: TButton;
    AddressPnl: TPanel;
    LazarusOsrBrowserWindow1: TLazarusOsrBrowserWindow;
    mDomHere: TMenuItem;
    MenuItem1: TMenuItem;
    PopupMenu1: TPopupMenu;

    procedure Chromium1BeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean; var Result: Boolean);
    procedure Chromium1OpenUrlFromTab(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; out Result: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);

    procedure GoBtnClick(Sender: TObject);
    procedure LazarusBrowserWindow1BrowserClosed(Sender: TObject);
    procedure LazarusOsrBrowserWindow1MouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
  private
    FCurRect: TRect;
    procedure DoOnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; var AHandled: Boolean);
    procedure DoOnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; var AHandled: Boolean);
    procedure DoOnPaint(Sender: TObject);
    procedure DoProcessMessageReceived(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame;
      sourceProcess: TCefProcessId; const message: ICefProcessMessage; out
      Result: Boolean);
  protected
    {$IFDEF WINDOWS}
    procedure WMEnterMenuLoop(var aMessage: TMessage); message WM_ENTERMENULOOP;
    procedure WMExitMenuLoop(var aMessage: TMessage); message WM_EXITMENULOOP;
    procedure WMDpiChanged(var Message: TMessage); message WM_DPICHANGED;
    {$ENDIF}

  public

  end;

var
  Form1: TForm1;


implementation

{$R *.lfm}

uses
  uCEFApplication;

{ TForm1 }

procedure TForm1.GoBtnClick(Sender: TObject);
begin
  LazarusOsrBrowserWindow1.Chromium.LoadURL(UTF8Decode(AddressEdt.Text));
end;

procedure TForm1.LazarusBrowserWindow1BrowserClosed(Sender: TObject);
begin
  Close;
end;

procedure TForm1.LazarusOsrBrowserWindow1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  TempMsg : ICefProcessMessage;
begin
  TempMsg := TCefProcessMessageRef.New(MSG_REQUEST_DOM_R); // Same name than TCefCustomRenderProcessHandler.MessageName
  TempMsg.ArgumentList.SetInt(0, X);
  TempMsg.ArgumentList.SetInt(1, Y);
  LazarusOsrBrowserWindow1.Chromium.SendProcessMessage(PID_RENDERER, TempMsg);
end;

procedure TForm1.DoOnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; var AHandled: Boolean);
begin
  AHandled := Button = mbRight;

  if Button = mbRight then begin
    PopupMenu1.PopUp(X, Y);
  end;
end;

procedure TForm1.DoOnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; var AHandled: Boolean);
begin
  AHandled := Button = mbRight;
end;

procedure TForm1.DoOnPaint(Sender: TObject);
begin
  if (FCurRect.Width > 0) and (FCurRect.Height > 0) then begin
    LazarusOsrBrowserWindow1.Canvas.Brush.Style := bsClear;
    LazarusOsrBrowserWindow1.Canvas.Pen.Style := psSolid;
    LazarusOsrBrowserWindow1.Canvas.Pen.Color := clRed;
    LazarusOsrBrowserWindow1.Canvas.Rectangle(FCurRect);
  end;
end;

procedure TForm1.DoProcessMessageReceived(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  sourceProcess: TCefProcessId; const message: ICefProcessMessage; out
  Result: Boolean);
begin
  Result := False;
  case message.Name of
    MSG_RESPONSE_DOM_R: begin
      FCurRect.Left := message.ArgumentList.GetInt(0);
      FCurRect.Top := message.ArgumentList.GetInt(1);
      FCurRect.Width := message.ArgumentList.GetInt(2);
      FCurRect.Height := message.ArgumentList.GetInt(3);
      Result := True;
      LazarusOsrBrowserWindow1.Invalidate;
    end;
  end;
end;

{$IFDEF WINDOWS}
procedure TForm1.WMEnterMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := True;
end;

procedure TForm1.WMExitMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := False;
end;

procedure TForm1.WMDpiChanged(var Message: TMessage);
begin
  inherited;

  if (GlobalCEFApp <> nil) then
    GlobalCEFApp.UpdateDeviceScaleFactor;

  if (LazarusOsrBrowserWindow1.Chromium <> nil) then
    begin
      LazarusOsrBrowserWindow1.Chromium.NotifyScreenInfoChanged;
      LazarusOsrBrowserWindow1.Chromium.WasResized;
    end;
end;

{$ENDIF}

procedure TForm1.Chromium1BeforePopup(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const targetUrl,
  targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition;
  userGesture: Boolean; const popupFeatures: TCefPopupFeatures;
  var windowInfo: TCefWindowInfo; var client: ICefClient;
  var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue;
  var noJavascriptAccess: Boolean; var Result: Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [WOD_NEW_FOREGROUND_TAB, WOD_NEW_BACKGROUND_TAB, WOD_NEW_POPUP, WOD_NEW_WINDOW]);
end;

procedure TForm1.Chromium1OpenUrlFromTab(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring;
  targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; out
  Result: Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [WOD_NEW_FOREGROUND_TAB, WOD_NEW_BACKGROUND_TAB, WOD_NEW_POPUP, WOD_NEW_WINDOW]);
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  LazarusOsrBrowserWindow1.CloseBrowser(True);

  CanClose := LazarusOsrBrowserWindow1.IsClosed;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FCurRect.Width := 0;
  LazarusOsrBrowserWindow1.Chromium.OnProcessMessageReceived := @DoProcessMessageReceived;
  LazarusOsrBrowserWindow1.OnMouseDown := @DoOnMouseDown;
  LazarusOsrBrowserWindow1.OnMouseUp := @DoOnMouseUp;
  LazarusOsrBrowserWindow1.OnPaint  := @DoOnPaint;

  LazarusOsrBrowserWindow1.Chromium.LoadURL(UTF8Decode(AddressEdt.Text));
end;

initialization
  {$IFDEF DARWIN}  // $IFDEF MACOSX
  AddCrDelegate;
  {$ENDIF}
  if GlobalCEFApp = nil then begin
    CreateGlobalCEFApp;
    if not GlobalCEFApp.StartMainProcess then begin
      DestroyGlobalCEFApp;
      DestroyGlobalCEFWorkScheduler;
      halt(0); // exit the subprocess
    end;
  end;

finalization
  (* Destroy from this unit, which is used after "Interfaces". So this happens before the Application object is destroyed *)
  if GlobalCEFWorkScheduler <> nil then
    GlobalCEFWorkScheduler.StopScheduler;
  DestroyGlobalCEFApp;
  DestroyGlobalCEFWorkScheduler;

end.

