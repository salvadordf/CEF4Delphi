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

unit uChildForm;

{$mode objfpc}{$H+}

{$I cef.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  LMessages, SyncObjs,
  uCEFChromium, uCEFTypes, uCEFInterfaces, uCEFConstants, uCEFWindowParent,
  uCEFWinControl, uCEFLinkedWindowParent, uCEFChromiumEvents;
                                     
const
  CEF_CLOSECHILD = $A52;

type

  { TChildForm }

  TChildForm = class(TForm)
    CEFLinkedWindowParent1: TCEFLinkedWindowParent;
    Chromium1: TChromium;

    procedure CEFLinkedWindowParent1Enter(Sender: TObject);
    procedure CEFLinkedWindowParent1Exit(Sender: TObject);

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    procedure Chromium1BeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean; var Result: Boolean);
    procedure Chromium1TitleChange(Sender: TObject; const browser: ICefBrowser; const title: ustring);
    procedure Chromium1Close(Sender: TObject; const browser: ICefBrowser; var aAction : TCefCloseBrowserAction);
    procedure Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);     
    procedure Chromium1GotFocus(Sender: TObject; const browser: ICefBrowser);
    procedure FormShow(Sender: TObject);

  protected
    FCanClose          : boolean;
    FClosing           : boolean;
    FClientInitialized : boolean;
    FPopupFeatures     : TCefPopupFeatures;

    procedure WMMove(var aMessage: TLMMove); message LM_MOVE;
    procedure WMSize(var aMessage: TLMSize); message LM_SIZE;
    procedure WMWindowPosChanged(var aMessage: TLMWindowPosChanged); message LM_WINDOWPOSCHANGED;

    procedure BrowserCloseFormMsg(Data: PtrInt);

  public
    procedure AfterConstruction; override;
    function  CreateClientHandler(var windowInfo : TCefWindowInfo; var client : ICefClient; const targetFrameName : string; const popupFeatures : TCefPopupFeatures) : boolean;
    procedure ApplyPopupFeatures;
    procedure SendCompMessage(aMsg : cardinal; aData : PtrInt = 0);

    property  ClientInitialized : boolean   read FClientInitialized;
    property  Closing           : boolean   read FClosing;
  end;

implementation

{$R *.dfm}

uses
  Math,
  uCEFMiscFunctions, uCEFApplication, uMainForm;

// Destruction steps
// =================
// 1. FormCloseQuery sets CanClose to FALSE calls TChromium.CloseBrowser which triggers the TChromium.OnClose event.
// 2. TChromium.OnClose sends a CEFBROWSER_DESTROY message to destroy CEFWindowParent1 in the main thread, which triggers the TChromium.OnBeforeClose event.
// 3. TChromium.OnBeforeClose sets FCanClose := True and sends WM_CLOSE to the form.

procedure TChildForm.AfterConstruction;
begin
  inherited AfterConstruction;

  CreateHandle;

  CEFLinkedWindowParent1.CreateHandle;
end;

function TChildForm.CreateClientHandler(var   windowInfo      : TCefWindowInfo;
                                        var   client          : ICefClient;
                                        const targetFrameName : string;
                                        const popupFeatures   : TCefPopupFeatures) : boolean;
var
  TempRect : TRect;
begin
  if Chromium1.CreateClientHandler(client, False) then
    begin
      Result             := True;
      FClientInitialized := True;
      FPopupFeatures     := popupFeatures;
      TempRect           := CEFLinkedWindowParent1.BoundsRect;

      if (FPopupFeatures.widthset  <> 0) then TempRect.Right  := max(FPopupFeatures.width,  100);
      if (FPopupFeatures.heightset <> 0) then TempRect.Bottom := max(FPopupFeatures.height, 100);

      WindowInfoAsChild(windowInfo, CEFLinkedWindowParent1.Handle, TempRect, '');
    end
   else
    Result := False;
end;

procedure TChildForm.ApplyPopupFeatures;
begin
  if (FPopupFeatures.xset      <> 0) then Chromium1.SetFormLeftTo(FPopupFeatures.x);
  if (FPopupFeatures.yset      <> 0) then Chromium1.SetFormTopTo(FPopupFeatures.y);
  if (FPopupFeatures.widthset  <> 0) then Chromium1.ResizeFormWidthTo(FPopupFeatures.width);
  if (FPopupFeatures.heightset <> 0) then Chromium1.ResizeFormHeightTo(FPopupFeatures.height);
end;

procedure TChildForm.Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
begin
  // We must wait until all browsers trigger the TChromium.OnBeforeClose event
  // in order to close the application safely or we will have shutdown issues.
  FCanClose := True;
  SendCompMessage(CEF_BEFORECLOSE);
end;

procedure TChildForm.Chromium1Close(Sender: TObject; const browser: ICefBrowser; var aAction : TCefCloseBrowserAction);
begin
  // continue closing the browser
  aAction := cbaClose;
end;

procedure TChildForm.Chromium1TitleChange(Sender: TObject; const browser: ICefBrowser; const title: ustring);
begin
  Caption := title;
end;

procedure TChildForm.WMMove(var aMessage : TLMMove);
begin
  inherited;
  Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TChildForm.WMSize(var aMessage: TLMSize);
begin
  inherited;
  Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TChildForm.WMWindowPosChanged(var aMessage: TLMWindowPosChanged);
begin
  inherited;
  Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TChildForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TChildForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FCanClose;

  if not(FClosing) then
    begin
      FClosing := True;
      Visible  := False;
      Chromium1.CloseBrowser(True);
    end;
end;

procedure TChildForm.Chromium1BeforePopup(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const targetUrl,
  targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition;
  userGesture: Boolean; const popupFeatures: TCefPopupFeatures;
  var windowInfo: TCefWindowInfo; var client: ICefClient;
  var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue;
  var noJavascriptAccess: Boolean; var Result: Boolean);
begin
  case targetDisposition of
    WOD_NEW_FOREGROUND_TAB,
    WOD_NEW_BACKGROUND_TAB,
    WOD_NEW_WINDOW : Result := True;  // For simplicity, this demo blocks new tabs and new windows.

    WOD_NEW_POPUP : Result := not(TMainForm(Owner).CreateClientHandler(windowInfo, client, targetFrameName, popupFeatures));

    else Result := False;
  end;
end;

procedure TChildForm.FormCreate(Sender: TObject);
begin
  FCanClose          := False;
  FClosing           := False;
  FClientInitialized := False;
end;

// This is a workaround for the CEF issue #2026
// https://bitbucket.org/chromiumembedded/cef/issues/2026/multiple-major-keyboard-focus-issues-on
// We use CEFLinkedWindowParent1.OnEnter, CEFLinkedWindowParent1.OnExit and
// TChromium.OnGotFocus to avoid most of the focus issues.
// CEFLinkedWindowParent1.TabStop must be TRUE.    
procedure TChildForm.CEFLinkedWindowParent1Enter(Sender: TObject);
begin
  if not(csDesigning in ComponentState) and
     Chromium1.Initialized and
     not(Chromium1.FrameIsFocused) then
    Chromium1.SendFocusEvent(True);
end;

procedure TChildForm.CEFLinkedWindowParent1Exit(Sender: TObject);
begin
  if not(csDesigning in ComponentState) then
    Chromium1.SendCaptureLostEvent;
end;

procedure TChildForm.Chromium1GotFocus(Sender: TObject;
  const browser: ICefBrowser);
begin
  CEFLinkedWindowParent1.SetFocus;
end;

procedure TChildForm.FormShow(Sender: TObject);
begin

end;

procedure TChildForm.FormDestroy(Sender: TObject);
begin
  if FClientInitialized then
    TMainForm(Owner).SendCompMessage(CEF_CHILDDESTROYED);
end;

procedure TChildForm.BrowserCloseFormMsg(Data: PtrInt);
begin
  Close;
end;

procedure TChildForm.SendCompMessage(aMsg : cardinal; aData : PtrInt);
begin
  case aMsg of
    CEF_BEFORECLOSE : Application.QueueAsyncCall(@BrowserCloseFormMsg, aData);
  end;
end;

end.
