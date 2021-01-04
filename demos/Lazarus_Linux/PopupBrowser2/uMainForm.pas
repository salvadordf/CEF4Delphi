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

{$mode objfpc}{$H+}

{$I cef.inc}

interface

uses                                                       
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  LMessages, SyncObjs,
  uCEFChromium, uCEFWindowParent, uCEFInterfaces, uCEFConstants, uCEFTypes,
  uChildForm, uCEFWinControl, uCEFChromiumEvents, uCEFLinkedWindowParent;

const
  CEF_CREATENEXTCHILD  = $A50;
  CEF_CHILDDESTROYED   = $A51;
  CEF_INITIALIZED      = $A52;

type

  { TMainForm }

  TMainForm = class(TForm)
    AddressPnl: TPanel;
    AddressEdt: TEdit;
    CEFLinkedWindowParent1: TCEFLinkedWindowParent;
    GoBtn: TButton;
    Chromium1: TChromium;

    procedure CEFLinkedWindowParent1Enter(Sender: TObject);
    procedure CEFLinkedWindowParent1Exit(Sender: TObject);

    procedure GoBtnClick(Sender: TObject);
                                             
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    procedure Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1BeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean; var Result: Boolean);
    procedure Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1Close(Sender: TObject; const browser: ICefBrowser; var aAction: TCefCloseBrowserAction);   
    procedure Chromium1GotFocus(Sender: TObject; const browser: ICefBrowser);

  protected
    FChildForm       : TChildForm;
    FCriticalSection : TCriticalSection;
    FCanClose        : boolean;  // Set to True in TChromium.OnBeforeClose
    FClosingMainForm : boolean;  // Set to True in the CloseQuery event.
    FClosingChildren : boolean;  // Set to True in the CloseQuery event.

    function  GetPopupChildCount : integer;

    procedure ClosePopupChildren;
    procedure CreateHiddenChildForm;

    procedure WMMove(var aMessage: TLMMove); message LM_MOVE;
    procedure WMSize(var aMessage: TLMSize); message LM_SIZE;
    procedure WMWindowPosChanged(var aMessage: TLMWindowPosChanged); message LM_WINDOWPOSCHANGED;

    procedure BrowserCreatedMsg(Data: PtrInt);
    procedure BrowserInitializedMsg(Data: PtrInt);
    procedure BrowserCreateNextChildMsg(Data: PtrInt);
    procedure BrowserChildDestroyedMsg(Data: PtrInt);
    procedure BrowserCloseFormMsg(Data: PtrInt);

    property  PopupChildCount : integer  read  GetPopupChildCount;

  public
    function  CreateClientHandler(var windowInfo : TCefWindowInfo; var client : ICefClient; const targetFrameName : string; const popupFeatures : TCefPopupFeatures) : boolean;
    procedure SendCompMessage(aMsg : cardinal; aData : PtrInt = 0);
  end;

var
  MainForm: TMainForm;

procedure CreateGlobalCEFApp;

implementation

{$R *.lfm}

uses
  LCLIntf,
  uCEFApplication, uCEFMiscFunctions;

// This is demo shows how to create popup windows in CEF.

// You need to understand the SimpleBrowser2 demo completely before trying to understand this demo.

// When TChromium needs to show a new popup window it executes TChromium.OnBeforePopup.

// LCL components *MUST* be created and destroyed in the main thread but CEF executes the
// TChromium.OnBeforePopup in a different thread.

// For this reason this demo creates a hidden popup form (TChildForm) in case CEF needs to show a popup window.
// TChromium.OnBeforePopup calls TChildForm.CreateClientHandler to initialize some parameters and create the new ICefClient.
// After that, it sends a CEF_CREATENEXTCHILD message to show the popup form and create a new one.

// All the child forms must be correctly destroyed before closing the main form. Read the code comments in uChildForm.pas
// to know how the popup windows are destroyed.

// The main form close all active popup forms and waits until all of them have sent a CEF_CHILDDESTROYED message.

// Destruction steps
// =================
// 1. FormCloseQuery sets CanClose to FALSE and it closes all child forms.
// 2. When all the child forms are closed then FormCloseQuery is triggered again, sets CanClose to FALSE calls TChromium.CloseBrowser which triggers the TChromium.OnClose event.
// 3. TChromium.OnClose sets aAction to cbaClose which triggers the TChromium.OnBeforeClose event.
// 4. TChromium.OnBeforeClose sets FCanClose := True and sends a CEF_BEFORECLOSE message to close the form in the main thread.


// Many other demos use a timer to create the browser but this demo sends a
// CEF_INITIALIZED message in the GlobalCEFApp.OnContextInitialized and
// TForm.OnActivate events
procedure GlobalCEFApp_OnContextInitialized;
begin
  if (MainForm <> nil) and MainForm.Showing and MainForm.Focused then
    MainForm.SendCompMessage(CEF_INITIALIZED);
end;

procedure CreateGlobalCEFApp;
begin
  GlobalCEFApp                      := TCefApplication.Create;
  GlobalCEFApp.OnContextInitialized := @GlobalCEFApp_OnContextInitialized;
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

  // CEF can't find the HTML if we load file:///filename.html in Linux so we
  // add the full path manually.
  // The "Click me to open a file" button in PopupBrowser.html will not work
  // because of this limitation.
  AddressEdt.Text := 'file://' + IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0))) + 'PopupBrowser.html';
                                                              
  Chromium1.DefaultURL              := UTF8Decode(AddressEdt.Text);
  Chromium1.Options.BackgroundColor := CefColorSetARGB($FF, $FF, $FF, $FF);

  CreateHiddenChildForm;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FCriticalSection);
end;

procedure TMainForm.Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  // Now the browser is fully initialized we can send a message to the main form
  // to enable the user interface.
  SendCompMessage(CEF_AFTERCREATED);
end;

procedure TMainForm.Chromium1BeforePopup(Sender : TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const targetUrl,
  targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition;
  userGesture: Boolean; const popupFeatures: TCefPopupFeatures;
  var windowInfo: TCefWindowInfo; var client: ICefClient;
  var settings: TCefBrowserSettings;
  var extra_info: ICefDictionaryValue;
  var noJavascriptAccess: Boolean;
  var Result: Boolean);
begin
  case targetDisposition of
    WOD_NEW_FOREGROUND_TAB,
    WOD_NEW_BACKGROUND_TAB,
    WOD_NEW_WINDOW : Result := True;  // For simplicity, this demo blocks new tabs and new windows.

    WOD_NEW_POPUP  : Result := not(CreateClientHandler(windowInfo, client, targetFrameName, popupFeatures));

    else Result := False;
  end;
end;

function TMainForm.CreateClientHandler(var   windowInfo      : TCefWindowInfo;
                                       var   client          : ICefClient;
                                       const targetFrameName : string;
                                       const popupFeatures   : TCefPopupFeatures) : boolean;
begin
  try
    FCriticalSection.Acquire;

    if (FChildForm <> nil) and
       FChildForm.CreateClientHandler(windowInfo, client, targetFrameName, popupFeatures) then
      begin
        SendCompMessage(CEF_CREATENEXTCHILD);
        Result := True;
      end;
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
        TempForm.Close;

      dec(i);
    end;
end;

procedure TMainForm.GoBtnClick(Sender: TObject);
begin
  // This will load the URL in the edit box
  Chromium1.LoadURL(UTF8Decode(AddressEdt.Text));
end;      

// This is a workaround for the CEF issue #2026
// https://bitbucket.org/chromiumembedded/cef/issues/2026/multiple-major-keyboard-focus-issues-on
// We use CEFLinkedWindowParent1.OnEnter, CEFLinkedWindowParent1.OnExit and
// TChromium.OnGotFocus to avoid most of the focus issues.
// CEFLinkedWindowParent1.TabStop must be TRUE.
procedure TMainForm.CEFLinkedWindowParent1Enter(Sender: TObject);
begin
  if not(csDesigning in ComponentState) and
     Chromium1.Initialized and
     not(Chromium1.FrameIsFocused) then
    Chromium1.SendFocusEvent(True);
end;

procedure TMainForm.CEFLinkedWindowParent1Exit(Sender: TObject);
begin
  if not(csDesigning in ComponentState) then
    Chromium1.SendCaptureLostEvent;
end;

procedure TMainForm.Chromium1GotFocus(Sender: TObject;
  const browser: ICefBrowser);
begin
  CEFLinkedWindowParent1.SetFocus;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  // Linux needs a fully formed window to add a Chromium browser so we use this
  // event to send a CEF_INITIALIZED message that will create the browser.
  SendCompMessage(CEF_INITIALIZED);
end;

procedure TMainForm.Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
begin
  // We must wait until all browsers trigger the TChromium.OnBeforeClose event
  // in order to close the application safely or we will have shutdown issues.
  FCanClose := True;
  SendCompMessage(CEF_BEFORECLOSE);
end;

procedure TMainForm.Chromium1Close(Sender: TObject; const browser: ICefBrowser;
  var aAction: TCefCloseBrowserAction);
begin
  // continue closing the browser
  aAction := cbaClose;
end;

procedure TMainForm.WMMove(var aMessage : TLMMove);
begin
  inherited;
  Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TMainForm.WMSize(var aMessage: TLMSize);
begin
  inherited;
  Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TMainForm.WMWindowPosChanged(var aMessage: TLMWindowPosChanged);
begin
  inherited;
  Chromium1.NotifyMoveOrResizeStarted;
end;      

procedure TMainForm.CreateHiddenChildForm;
begin
  // Linux requires a fully formed window in order to add a Chromium browser so
  // we show the next popup window outside the visible screen space and then we
  // hide it.
  FChildForm               := TChildForm.Create(self);
  FChildForm.Top           := -1000;
  FChildForm.Left          := -1000;
  FChildForm.Show;
  FChildForm.Hide;
  // Center the child form on the screen by default
  FChildForm.Top           := (screen.Height - FChildForm.Height) div 2;
  FChildForm.Left          := (screen.Width  - FChildForm.Width)  div 2;
end;

procedure TMainForm.BrowserCreatedMsg(Data: PtrInt);
begin
  Caption            := 'Popup Browser';
  cursor             := crDefault;
  AddressPnl.Enabled := True;
end;

procedure TMainForm.BrowserInitializedMsg(Data: PtrInt);
begin
  if not(Chromium1.Initialized) then
    Chromium1.CreateBrowser(CEFLinkedWindowParent1.Handle, CEFLinkedWindowParent1.BoundsRect);
end;

procedure TMainForm.BrowserChildDestroyedMsg(Data: PtrInt);
begin
  if FClosingChildren and (PopupChildCount = 0) then Close;
end;

procedure TMainForm.BrowserCreateNextChildMsg(Data: PtrInt);
begin
  try
    FCriticalSection.Acquire;

    if (FChildForm <> nil) then
      begin
        FChildForm.ApplyPopupFeatures;
        FChildForm.Show;
        // SetActiveWindow should bring the child form to the front...
        SetActiveWindow(FChildForm.Handle);
      end;

    CreateHiddenChildForm;
  finally
    FCriticalSection.Release;
  end;
end;            

procedure TMainForm.BrowserCloseFormMsg(Data: PtrInt);
begin
  Close;
end;

procedure TMainForm.SendCompMessage(aMsg : cardinal; aData : PtrInt);
begin
  case aMsg of
    CEF_INITIALIZED     : Application.QueueAsyncCall(@BrowserInitializedMsg, aData);
    CEF_AFTERCREATED    : Application.QueueAsyncCall(@BrowserCreatedMsg, aData);
    CEF_CREATENEXTCHILD : Application.QueueAsyncCall(@BrowserCreateNextChildMsg, aData);
    CEF_CHILDDESTROYED  : Application.QueueAsyncCall(@BrowserChildDestroyedMsg, aData);  
    CEF_BEFORECLOSE     : Application.QueueAsyncCall(@BrowserCloseFormMsg, aData);
  end;
end;

end.
