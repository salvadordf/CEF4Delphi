unit uchildform;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  LMessages, SyncObjs,
  uCEFChromium, uCEFTypes, uCEFInterfaces, uCEFConstants, uCEFWindowParent,
  uCEFWinControl, uCEFLinkedWindowParent, uCEFChromiumEvents;

type

  { TChildForm }

  TChildForm = class(TForm)
    CEFLinkedWindowParent1: TCEFLinkedWindowParent;
    Chromium1: TChromium;

    procedure CEFLinkedWindowParent1Enter(Sender: TObject);
    procedure CEFLinkedWindowParent1Exit(Sender: TObject);

    procedure Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1BeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; popup_id: Integer; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean; var Result: Boolean);
    procedure Chromium1GotFocus(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1TitleChange(Sender: TObject; const browser: ICefBrowser; const title: ustring);

    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

  protected
    FCanClose          : boolean;
    FClosing           : boolean;
    FClientInitialized : boolean;
    FPopupFeatures     : TCefPopupFeatures;
    FCaption           : ustring;          

    procedure WMMove(var aMessage: TLMMove); message LM_MOVE;
    procedure WMSize(var aMessage: TLMSize); message LM_SIZE;
    procedure WMWindowPosChanged(var aMessage: TLMWindowPosChanged); message LM_WINDOWPOSCHANGED;

    procedure BrowserCloseFormMsg(Data: PtrInt);
    procedure BrowserSetFocusMsg(Data: PtrInt);
    procedure BrowserTitleChangeMsg(Data: PtrInt);

  public
    function  CreateClientHandler(var windowInfo : TCefWindowInfo; var client : ICefClient; const targetFrameName : string; const popupFeatures : TCefPopupFeatures) : boolean;
    procedure ApplyPopupFeatures;
    procedure SendCompMessage(aMsg : cardinal; aData : PtrInt = 0);

    property  ClientInitialized : boolean   read FClientInitialized;
    property  Closing           : boolean   read FClosing;
  end;

var
  ChildForm: TChildForm;

implementation

{$R *.lfm}

uses
  Math,
  uCEFMiscFunctions, uCEFApplication, uCEFWindowInfoWrapper, uMainForm;

{ TChildForm }

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

      TCEFWindowInfoWrapper.AsChild(windowInfo, CEFLinkedWindowParent1.Handle, TempRect);
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
    Chromium1.SetFocus(True);
end;

procedure TChildForm.CEFLinkedWindowParent1Exit(Sender: TObject);
begin
  if not(csDesigning in ComponentState) then
    Chromium1.SendCaptureLostEvent;
end;

procedure TChildForm.Chromium1BeforeClose(Sender: TObject;
  const browser: ICefBrowser);
begin
  if (Chromium1.BrowserId = 0) then
    begin
      // We must wait until all browsers trigger the TChromium.OnBeforeClose event
      // in order to close the application safely or we will have shutdown issues.
      FCanClose := True;
      SendCompMessage(CEF_BEFORECLOSE);
    end;
end;

procedure TChildForm.Chromium1BeforePopup(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; popup_id: Integer;
  const targetUrl, targetFrameName: ustring;
  targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean;
  const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
  var client: ICefClient; var settings: TCefBrowserSettings;
  var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean;
  var Result: Boolean);
begin
  case targetDisposition of
    CEF_WOD_NEW_FOREGROUND_TAB,
    CEF_WOD_NEW_BACKGROUND_TAB,
    CEF_WOD_NEW_WINDOW : Result := True;  // For simplicity, this demo blocks new tabs and new windows.

    CEF_WOD_NEW_POPUP : Result := not(TMainForm(Owner).CreateClientHandler(windowInfo, client, targetFrameName, popupFeatures));

    else Result := False;
  end;
end;

procedure TChildForm.Chromium1GotFocus(Sender: TObject;
  const browser: ICefBrowser);
begin
  if Chromium1.IsSameBrowser(browser) then
    SendCompMessage(CEF_SETFOCUS);
end;

procedure TChildForm.Chromium1TitleChange(Sender: TObject;
  const browser: ICefBrowser; const title: ustring);
begin
  if Chromium1.IsSameBrowser(browser) then
    begin
      FCaption := title;
      SendCompMessage(CEF_TITLECHANGE);
    end;
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
      FreeAndNil(CEFLinkedWindowParent1);
    end;
end;

procedure TChildForm.FormCreate(Sender: TObject);
begin
  FCanClose          := False;
  FClosing           := False;
  FClientInitialized := False;   

  // CEF requires a native widget
  CEFLinkedWindowParent1.SetQTWidgetAsNative;
end;

procedure TChildForm.FormDestroy(Sender: TObject);
begin
  if FClientInitialized then
    TMainForm(Owner).SendCompMessage(CEF_CHILDDESTROYED);
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

procedure TChildForm.BrowserCloseFormMsg(Data: PtrInt);
begin
  Close;
end;

procedure TChildForm.BrowserSetFocusMsg(Data: PtrInt);
begin
  CEFLinkedWindowParent1.SetFocus;
end;

procedure TChildForm.BrowserTitleChangeMsg(Data: PtrInt);
begin
  Caption := UTF8Encode(FCaption);
end;

procedure TChildForm.SendCompMessage(aMsg : cardinal; aData : PtrInt);
begin
  case aMsg of
    CEF_BEFORECLOSE : Application.QueueAsyncCall(@BrowserCloseFormMsg, aData);
    CEF_SETFOCUS    : Application.QueueAsyncCall(@BrowserSetFocusMsg, aData);
    CEF_TITLECHANGE : Application.QueueAsyncCall(@BrowserTitleChangeMsg, aData);
  end;
end;

end.

