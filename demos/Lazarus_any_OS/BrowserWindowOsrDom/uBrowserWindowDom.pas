unit uBrowserWindowDom;

{$mode objfpc}{$H+}
{$IFDEF MSWINDOWS}{$I ..\..\..\source\cef.inc}{$ELSE}{$I ../../../source/cef.inc}{$ENDIF}

interface

uses
  GlobalCefApplication,
  uCEFLazarusCocoa, // required for Cocoa
  SysUtils, Messages, Forms, Controls, Dialogs, ExtCtrls, StdCtrls, LMessages,
  Menus, Graphics, uCEFTypes, uCEFInterfaces, uHelperProcessDom,
  uCEFWorkScheduler, uCEFBrowserWindow, uCEFProcessMessage,
  uCEFOsrBrowserWindow, Classes;

type

  { TForm1 }

  TForm1 = class(TForm)
    AddressEdt: TComboBox;
    GoBtn: TButton;
    AddressPnl: TPanel;
    OsrBrowserWindow1: TOsrBrowserWindow;
    mDomHere: TMenuItem;
    MenuItem1: TMenuItem;
    PopupMenu1: TPopupMenu;

    procedure Chromium1BeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean; var Result: Boolean);
    procedure Chromium1OpenUrlFromTab(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; out Result: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);

    procedure GoBtnClick(Sender: TObject);
    procedure OsrBrowserWindow1BrowserClosed(Sender: TObject);
    procedure OsrBrowserWindow1MouseMove(Sender: TObject;
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
  OsrBrowserWindow1.Chromium.LoadURL(UTF8Decode(AddressEdt.Text));
end;

procedure TForm1.OsrBrowserWindow1BrowserClosed(Sender: TObject);
begin
  Close;
end;

procedure TForm1.OsrBrowserWindow1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  TempMsg : ICefProcessMessage;
begin
  TempMsg := TCefProcessMessageRef.New(MSG_REQUEST_DOM_R); // Same name than TCefCustomRenderProcessHandler.MessageName
  TempMsg.ArgumentList.SetInt(0, X);
  TempMsg.ArgumentList.SetInt(1, Y);
  OsrBrowserWindow1.Chromium.SendProcessMessage(PID_RENDERER, TempMsg);
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
    OsrBrowserWindow1.Canvas.Brush.Style := bsClear;
    OsrBrowserWindow1.Canvas.Pen.Style := psSolid;
    OsrBrowserWindow1.Canvas.Pen.Color := clRed;
    OsrBrowserWindow1.Canvas.Rectangle(FCurRect);
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
      OsrBrowserWindow1.Invalidate;
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

  if (OsrBrowserWindow1.Chromium <> nil) then
    begin
      OsrBrowserWindow1.Chromium.NotifyScreenInfoChanged;
      OsrBrowserWindow1.Chromium.WasResized;
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
  Result := (targetDisposition in [CEF_WOD_NEW_FOREGROUND_TAB, CEF_WOD_NEW_BACKGROUND_TAB, CEF_WOD_NEW_POPUP, CEF_WOD_NEW_WINDOW]);
end;

procedure TForm1.Chromium1OpenUrlFromTab(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring;
  targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; out
  Result: Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [CEF_WOD_NEW_FOREGROUND_TAB, CEF_WOD_NEW_BACKGROUND_TAB, CEF_WOD_NEW_POPUP, CEF_WOD_NEW_WINDOW]);
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  OsrBrowserWindow1.CloseBrowser(True);

  CanClose := OsrBrowserWindow1.IsClosed;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FCurRect.Width := 0;
  OsrBrowserWindow1.Chromium.OnProcessMessageReceived := @DoProcessMessageReceived;
  OsrBrowserWindow1.OnMouseDown := @DoOnMouseDown;
  OsrBrowserWindow1.OnMouseUp := @DoOnMouseUp;
  OsrBrowserWindow1.OnPaint  := @DoOnPaint;

  OsrBrowserWindow1.Chromium.LoadURL(UTF8Decode(AddressEdt.Text));
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

