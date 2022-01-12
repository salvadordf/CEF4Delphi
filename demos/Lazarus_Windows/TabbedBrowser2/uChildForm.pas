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
//        Copyright © 2022 Salvador Diaz Fau. All rights reserved.
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

{$MODE Delphi}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  System.SyncObjs, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls,
  {$ELSE}
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, SyncObjs,
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  {$ENDIF}
  uCEFChromium, uCEFTypes, uCEFInterfaces, uCEFConstants, uCEFWindowParent, uCEFWinControl,
  uCEFChromiumCore;

const
  CEF_UPDATECAPTION = WM_APP + $A55;

type
  TChildForm = class(TForm)
    Chromium1: TChromium;
    CEFWindowParent1: TCEFWindowParent;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    procedure Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1BeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean; var Result: Boolean);
    procedure Chromium1TitleChange(Sender: TObject; const browser: ICefBrowser; const title: ustring);
    procedure Chromium1Close(Sender: TObject; const browser: ICefBrowser; var aAction : TCefCloseBrowserAction);
    procedure Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1OpenUrlFromTab(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; out Result: Boolean);

  protected
    FCriticalSection   : TCriticalSection;
    FCanClose          : boolean;
    FClosing           : boolean;
    FBrowserWasCreated : boolean;
    FTitle             : string;
    FPopupFeatures     : TCefPopupFeatures;

    function  GetInitialized : boolean;

    procedure WMMove(var aMessage : TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage : TMessage); message WM_MOVING;
    procedure WMEnterMenuLoop(var aMessage: TMessage); message WM_ENTERMENULOOP;
    procedure WMExitMenuLoop(var aMessage: TMessage); message WM_EXITMENULOOP;
    procedure BrowserDestroyMsg(var aMessage : TMessage); message CEF_DESTROY;
    procedure BrowserUpdateCaptionMsg(var aMessage : TMessage); message CEF_UPDATECAPTION;

  public
    procedure AfterConstruction; override;
    function  CreateBrowser(const aHomepage : string) : boolean;
    function  CreateClientHandler(var windowInfo : TCefWindowInfo; var client : ICefClient; const targetFrameName : string; const popupFeatures : TCefPopupFeatures) : boolean;
    procedure ApplyPopupFeatures;

    property  Initialized : boolean   read GetInitialized;
    property  Closing     : boolean   read FClosing;
  end;

implementation

{$R *.lfm}

uses
  {$IFDEF DELPHI16_UP}
  System.Math,
  {$ELSE}
  Math,
  {$ENDIF}
  uCEFMiscFunctions, uCEFApplication, uMainForm;

// Destruction steps
// =================
// 1. FormCloseQuery sets CanClose to FALSE calls TChromium.CloseBrowser which
//    triggers the TChromium.OnClose event.
// 2. TChromium.OnClose sends a CEFBROWSER_DESTROY message to destroy CEFWindowParent1
//    in the main thread, which triggers the TChromium.OnBeforeClose event.
// 3. TChromium.OnBeforeClose sets FCanClose := True and sends WM_CLOSE to the form.

procedure TChildForm.AfterConstruction;
begin
  inherited AfterConstruction;

  CreateHandle;

  CEFWindowParent1.CreateHandle;
end;

function TChildForm.CreateClientHandler(var   windowInfo      : TCefWindowInfo;
                                        var   client          : ICefClient;
                                        const targetFrameName : string;
                                        const popupFeatures   : TCefPopupFeatures) : boolean;
var
  TempRect : TRect;
begin
  if CEFWindowParent1.HandleAllocated and
     Chromium1.CreateClientHandler(client, False) then
    begin
      Result         := True;
      FPopupFeatures := popupFeatures;
      TempRect       := CEFWindowParent1.ClientRect;

      if (FPopupFeatures.widthset  <> 0) then TempRect.Right  := max(FPopupFeatures.width,  100);
      if (FPopupFeatures.heightset <> 0) then TempRect.Bottom := max(FPopupFeatures.height, 100);

      WindowInfoAsChild(windowInfo, CEFWindowParent1.Handle, TempRect, '');
    end
   else
    Result := False;
end;

function TChildForm.CreateBrowser(const aHomepage : string) : boolean;
begin
  Chromium1.DefaultURL := aHomepage;
  Result               := Chromium1.CreateBrowser(CEFWindowParent1);
end;

procedure TChildForm.ApplyPopupFeatures;
begin
  if (FPopupFeatures.xset      <> 0) then Chromium1.SetFormLeftTo(FPopupFeatures.x);
  if (FPopupFeatures.yset      <> 0) then Chromium1.SetFormTopTo(FPopupFeatures.y);
  if (FPopupFeatures.widthset  <> 0) then Chromium1.ResizeFormWidthTo(FPopupFeatures.width);
  if (FPopupFeatures.heightset <> 0) then Chromium1.ResizeFormHeightTo(FPopupFeatures.height);
end;

procedure TChildForm.Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  FBrowserWasCreated := True;
end;

procedure TChildForm.Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
begin
  FCanClose := True;
  PostMessage(Handle, WM_CLOSE, 0, 0);
end;

procedure TChildForm.Chromium1BeforePopup(      Sender             : TObject;
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
  Result := not(TMainForm(Owner).DoOnBeforePopup(windowInfo, client, targetFrameName, popupFeatures, targetDisposition));
end;

procedure TChildForm.Chromium1OpenUrlFromTab(      Sender            : TObject;
                                             const browser           : ICefBrowser;
                                             const frame             : ICefFrame;
                                             const targetUrl         : ustring;
                                                   targetDisposition : TCefWindowOpenDisposition;
                                                   userGesture       : Boolean;
                                             out   Result            : Boolean);
begin
  Result := not(TMainForm(Owner).DoOpenUrlFromTab(targetUrl, targetDisposition));
end;

procedure TChildForm.Chromium1Close(Sender: TObject; const browser: ICefBrowser; var aAction : TCefCloseBrowserAction);
begin
  PostMessage(Handle, CEF_DESTROY, 0, 0);
  aAction := cbaDelay;
end;

procedure TChildForm.Chromium1TitleChange(Sender: TObject; const browser: ICefBrowser; const title: ustring);
begin
  try
    FCriticalSection.Acquire;
    FTitle := title;
  finally
    FCriticalSection.Release;
    PostMessage(Handle, CEF_UPDATECAPTION, 0, 0);
  end;
end;

function TChildForm.GetInitialized : boolean;
begin
  Result := Chromium1.Initialized;
end;

procedure TChildForm.WMMove(var aMessage : TWMMove);
begin
  inherited;

  if (Chromium1 <> nil) then Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TChildForm.WMMoving(var aMessage : TMessage);
begin
  inherited;

  if (Chromium1 <> nil) then Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TChildForm.WMEnterMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := True;
end;

procedure TChildForm.WMExitMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := False;
end;

procedure TChildForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TChildForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if FBrowserWasCreated then
    begin
      CanClose := FCanClose;

      if not(FClosing) then
        begin
          FClosing := True;
          Visible  := False;
          Chromium1.CloseBrowser(True);
        end;
    end
   else
    CanClose := True;
end;

procedure TChildForm.FormCreate(Sender: TObject);
begin
  FCriticalSection   := TCriticalSection.Create;
  FBrowserWasCreated := False;
  FCanClose          := False;
  FClosing           := False;
end;

procedure TChildForm.FormDestroy(Sender: TObject);
begin
  FCriticalSection.Free;

  if FBrowserWasCreated and TMainForm(Owner).HandleAllocated then
    PostMessage(TMainForm(Owner).Handle, CEF_CHILDDESTROYED, 0, 0);
end;

procedure TChildForm.BrowserDestroyMsg(var aMessage : TMessage);
begin
  CEFWindowParent1.Free;
end;

procedure TChildForm.BrowserUpdateCaptionMsg(var aMessage : TMessage);
begin
  try
    FCriticalSection.Acquire;
    Caption := FTitle;
  finally
    FCriticalSection.Release;
  end;
end;

end.
