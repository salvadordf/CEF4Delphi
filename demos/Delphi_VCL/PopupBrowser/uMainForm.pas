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

unit uMainForm;

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
  uCEFChromium, uCEFWindowParent, uCEFInterfaces, uCEFConstants, uCEFTypes, uChildForm,
  Vcl.AppEvnts, uCEFWinControl, uCEFSentinel, uCEFChromiumCore;

const
  CEF_CREATENEXTCHILD  = WM_APP + $A50;
  CEF_CHILDDESTROYED   = WM_APP + $A51;

type
  TMainForm = class(TForm)
    AddressPnl: TPanel;
    AddressEdt: TEdit;
    GoBtn: TButton;
    Timer1: TTimer;
    Chromium1: TChromium;
    CEFWindowParent1: TCEFWindowParent;
    AppEvents: TApplicationEvents;

    procedure GoBtnClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure AppEventsMessage(var Msg: tagMSG; var Handled: Boolean);

    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    procedure Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1BeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean; var Result: Boolean);
    procedure Chromium1Close(Sender: TObject; const browser: ICefBrowser; var aAction: TCefCloseBrowserAction);
    procedure Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);

  protected
    FChildForm       : TChildForm;
    FCriticalSection : TCriticalSection;
    FCanClose        : boolean;  // Set to True in TChromium.OnBeforeClose
    FClosingMainForm : boolean;  // Set to True in the CloseQuery event.
    FClosingChildren : boolean;  // Set to True in the CloseQuery event.

    function  GetPopupChildCount : integer;

    procedure ClosePopupChildren;

    procedure WMMove(var aMessage : TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage : TMessage); message WM_MOVING;
    procedure WMEnterMenuLoop(var aMessage: TMessage); message WM_ENTERMENULOOP;
    procedure WMExitMenuLoop(var aMessage: TMessage); message WM_EXITMENULOOP;

    procedure BrowserCreatedMsg(var aMessage : TMessage); message CEF_AFTERCREATED;
    procedure BrowserDestroyMsg(var aMessage : TMessage); message CEF_DESTROY;
    procedure CreateNextChildMsg(var aMessage : TMessage); message CEF_CREATENEXTCHILD;
    procedure ChildDestroyedMsg(var aMessage : TMessage); message CEF_CHILDDESTROYED;

  public
    function  CreateClientHandler(var windowInfo : TCefWindowInfo; var client : ICefClient; const targetFrameName : string; const popupFeatures : TCefPopupFeatures) : boolean;

    property  PopupChildCount : integer  read  GetPopupChildCount;
  end;

var
  MainForm: TMainForm;

procedure CreateGlobalCEFApp;

implementation

{$R *.dfm}

uses
  uCEFApplication, uCEFMiscFunctions;

// This is demo shows how to create popup windows in CEF.

// You need to understand The SimpleBrowser2 and SimpleOSRBrowser demos completely before trying to understand this demo.

// When TChromium needs to show a new popup window it executes TChromium.OnBeforePopup.

// VCL components *MUST* be created and destroyed in the main thread but CEF executes the
// TChromium.OnBeforePopup in a different thread.

// For this reason this demo creates a hidden popup form (TChildForm) in case CEF needs to show a popup window.
// TChromium.OnBeforePopup calls TChildForm.CreateClientHandler to initialize some parameters and create the new ICefClient.
// After that, it sends a CEF_CREATENEXTCHILD message to show the popup form and create a new one.

// All the child forms must be correctly destroyed before closing the main form. Read the code comments in uChildForm.pas
// to know how the popup windows are destroyed.

// The main form sends a WM_CLOSE to all active popup forms and waits until all of them have sent a CEF_CHILDDESTROYED message.

// Destruction steps
// =================
// 1. FormCloseQuery sets CanClose to FALSE and it closes all child forms.
// 2. When all the child forms are closed then FormCloseQuery is triggered again, sets CanClose to FALSE calls TChromium.CloseBrowser which triggers the TChromium.OnClose event.
// 3. TChromium.OnClose sends a CEFBROWSER_DESTROY message to destroy CEFWindowParent1 in the main thread, which triggers the TChromium.OnBeforeClose event.
// 4. TChromium.OnBeforeClose sets FCanClose := True and sends WM_CLOSE to the form.

procedure CreateGlobalCEFApp;
begin
  GlobalCEFApp                            := TCefApplication.Create;
  GlobalCEFApp.WindowlessRenderingEnabled := True;
  GlobalCEFApp.EnableHighDPISupport       := True;
  //GlobalCEFApp.LogFile                    := 'debug.log';
  //GlobalCEFApp.LogSeverity                := LOGSEVERITY_INFO;
  {
  GlobalCEFApp.FrameworkDirPath     := 'c:\cef';
  GlobalCEFApp.ResourcesDirPath     := 'c:\cef';
  GlobalCEFApp.LocalesDirPath       := 'c:\cef\locales';
  GlobalCEFApp.cache                := 'c:\cef\cache';
  GlobalCEFApp.UserDataPath         := 'c:\cef\User Data';
  }
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  FClosingChildren := True;
  Visible          := False;

  if (PopupChildCount > 0) then
    begin
      ClosePopupChildren;
      CanClose := False;
    end
   else
    begin
      CanClose := FCanClose;

      if not(FClosingMainForm) then
        begin
          FClosingMainForm := True;
          Visible          := False;
          Chromium1.CloseBrowser(True);
        end;
    end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FClosingChildren                  := False;
  FClosingMainForm                  := False;
  FCanClose                         := False;
  FCriticalSection                  := TCriticalSection.Create;

  Chromium1.DefaultURL              := AddressEdt.Text;
  Chromium1.Options.BackgroundColor := CefColorSetARGB($FF, $FF, $FF, $FF);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FCriticalSection);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  // You *MUST* call CreateBrowser to create and initialize the browser.
  // This will trigger the AfterCreated event when the browser is fully
  // initialized and ready to receive commands.

  // GlobalCEFApp.GlobalContextInitialized has to be TRUE before creating any browser
  // If it's not initialized yet, we use a simple timer to create the browser later.
  if not(Chromium1.CreateBrowser(CEFWindowParent1)) then Timer1.Enabled := True;
end;

procedure TMainForm.Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  // Now the browser is fully initialized we can send a message to the main form to load the initial web page.
  PostMessage(Handle, CEF_AFTERCREATED, 0, 0);
end;

procedure TMainForm.Chromium1BeforePopup(      Sender             : TObject;
                                         const browser            : ICefBrowser;
                                         const frame              : ICefFrame;
                                         const targetUrl          : ustring;
                                         const targetFrameName    : ustring;
                                               targetDisposition  : TCefWindowOpenDisposition;
                                               userGesture        : Boolean;
                                         const popupFeatures      : TCefPopupFeatures;
                                         var   windowInfo         : TCefWindowInfo;
                                         var   client             : ICefClient;
                                         var   settings           : TCefBrowserSettings;
                                         var   extra_info         : ICefDictionaryValue;
                                         var   noJavascriptAccess : Boolean;
                                         var   Result             : Boolean);
begin
  case targetDisposition of
    WOD_NEW_FOREGROUND_TAB,
    WOD_NEW_BACKGROUND_TAB,
    WOD_NEW_WINDOW : Result := True;  // For simplicity, this demo blocks new tabs and new windows.

    WOD_NEW_POPUP  : Result := not(CreateClientHandler(windowInfo, client, targetFrameName, popupFeatures));

    else Result := False;
  end;
end;

procedure TMainForm.Chromium1Close(Sender: TObject; const browser: ICefBrowser; var aAction: TCefCloseBrowserAction);
begin
  PostMessage(Handle, CEF_DESTROY, 0, 0);
  aAction := cbaDelay;
end;

procedure TMainForm.Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
begin
  FCanClose := True;
  PostMessage(Handle, WM_CLOSE, 0, 0);
end;

function TMainForm.CreateClientHandler(var   windowInfo      : TCefWindowInfo;
                                       var   client          : ICefClient;
                                       const targetFrameName : string;
                                       const popupFeatures   : TCefPopupFeatures) : boolean;
begin
  try
    FCriticalSection.Acquire;

    Result := (FChildForm <> nil) and
              FChildForm.CreateClientHandler(windowInfo, client, targetFrameName, popupFeatures) and
              PostMessage(Handle, CEF_CREATENEXTCHILD, 0, 0);
  finally
    FCriticalSection.Release;
  end;
end;

function TMainForm.GetPopupChildCount : integer;
var
  i        : integer;
  TempForm : TCustomForm;
begin
  Result := 0;
  i      := pred(screen.CustomFormCount);

  while (i >= 0) do
    begin
      TempForm := screen.CustomForms[i];

      // Only count the fully initialized child forms and not the one waiting to be used.

      if (TempForm is TChildForm) and
         TChildForm(TempForm).ClientInitialized then
        inc(Result);

      dec(i);
    end;
end;

procedure TMainForm.ClosePopupChildren;
var
  i        : integer;
  TempForm : TCustomForm;
begin
  i := pred(screen.CustomFormCount);

  while (i >= 0) do
    begin
      TempForm := screen.CustomForms[i];

      // Only send WM_CLOSE to fully initialized child forms.

      if (TempForm is TChildForm) and
         TChildForm(TempForm).ClientInitialized and
         not(TChildForm(TempForm).Closing) then
        PostMessage(TChildForm(TempForm).Handle, WM_CLOSE, 0, 0);

      dec(i);
    end;
end;

procedure TMainForm.AppEventsMessage(var Msg: tagMSG; var Handled: Boolean);
begin
  case Msg.message of
    WM_SYSCHAR    : if (screen.FocusedForm is TChildForm) then TChildForm(screen.FocusedForm).HandleSysCharMsg(Msg, Handled);
    WM_SYSKEYDOWN : if (screen.FocusedForm is TChildForm) then TChildForm(screen.FocusedForm).HandleSysKeyDownMsg(Msg, Handled);
    WM_SYSKEYUP   : if (screen.FocusedForm is TChildForm) then TChildForm(screen.FocusedForm).HandleSysKeyUpMsg(Msg, Handled);
    WM_KEYDOWN    : if (screen.FocusedForm is TChildForm) then TChildForm(screen.FocusedForm).HandleKeyDownMsg(Msg, Handled);
    WM_KEYUP      : if (screen.FocusedForm is TChildForm) then TChildForm(screen.FocusedForm).HandleKeyUpMsg(Msg, Handled);
    WM_CHAR       : if (screen.FocusedForm is TChildForm) then TChildForm(screen.FocusedForm).HandleCharMsg(Msg, Handled);
    WM_MOUSEWHEEL : if (screen.FocusedForm is TChildForm) then TChildForm(screen.FocusedForm).HandleMouseWheelMsg(Msg, Handled);

    else Handled := False;
  end;
end;

procedure TMainForm.BrowserCreatedMsg(var aMessage : TMessage);
begin
  FChildForm         := TChildForm.Create(self);
  Caption            := 'Popup Browser';
  AddressPnl.Enabled := True;
end;

procedure TMainForm.BrowserDestroyMsg(var aMessage : TMessage);
begin
  CEFWindowParent1.Free;
end;

procedure TMainForm.CreateNextChildMsg(var aMessage : TMessage);
begin
  try
    FCriticalSection.Acquire;

    if (FChildForm <> nil) then
      PostMessage(FChildForm.Handle, CEF_SHOWCHILD, 0, 0);

    FChildForm := TChildForm.Create(self);
  finally
    FCriticalSection.Release;
  end;
end;

procedure TMainForm.ChildDestroyedMsg(var aMessage : TMessage);
begin
  if FClosingChildren and (PopupChildCount = 0) then Close;
end;

procedure TMainForm.GoBtnClick(Sender: TObject);
begin
  // This will load the URL in the edit box
  Chromium1.LoadURL(AddressEdt.Text);
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  if not(Chromium1.CreateBrowser(CEFWindowParent1)) and not(Chromium1.Initialized) then
    Timer1.Enabled := True;
end;

procedure TMainForm.WMMove(var aMessage : TWMMove);
begin
  inherited;

  if (Chromium1 <> nil) then Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TMainForm.WMMoving(var aMessage : TMessage);
begin
  inherited;

  if (Chromium1 <> nil) then Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TMainForm.WMEnterMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := True;
end;

procedure TMainForm.WMExitMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := False;
end;

end.
