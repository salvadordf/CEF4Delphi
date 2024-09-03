unit uMainForm;

{$I ..\..\..\source\cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, System.SyncObjs,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ToolWin, Vcl.Buttons, Vcl.ExtCtrls,
  {$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, SyncObjs,
  Controls, Forms, Dialogs, ComCtrls, ToolWin, Buttons, ExtCtrls,
  {$ENDIF}
  uCEFApplication, uCEFInterfaces, uCEFTypes, uCEFConstants, uChildForm, uBrowserTab;

const
  CEF_INITIALIZED     = WM_APP + $A50;
  CEF_DESTROYTAB      = WM_APP + $A51;
  CEF_CREATENEXTCHILD = WM_APP + $A52;
  CEF_CREATENEXTTAB   = WM_APP + $A53;
  CEF_CHILDDESTROYED  = WM_APP + $A54;

  HOMEPAGE_URL        = 'https://www.google.com';
  DEFAULT_TAB_CAPTION = 'New tab';

type
  TMainForm = class(TForm)
    BrowserPageCtrl: TPageControl;
    ButtonPnl: TPanel;
    AddTabBtn: TSpeedButton;
    RemoveTabBtn: TSpeedButton;

    procedure AddTabBtnClick(Sender: TObject);
    procedure RemoveTabBtnClick(Sender: TObject);

    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);

  protected
    FHiddenTab       : TBrowserTab;
    FChildForm       : TChildForm;
    FCriticalSection : TCriticalSection;
    FChildCounter    : cardinal; // Used to create unique child form names.
    FCanClose        : boolean;
    FClosing         : boolean;  // Set to True in the CloseQuery event.
    FLastTabID       : cardinal; // Used by NextTabID to generate unique tab IDs
    FPendingURL      : string;

    function  GetNextTabID : cardinal;
    function  GetPopupChildCount : integer;
    function  GetBrowserTabCount : integer;

    procedure EnableButtonPnl;
    function  CloseAllBrowsers : boolean;
    procedure CloseTab(aIndex : integer);
    procedure CreateHiddenBrowsers;
    procedure CreateChildForm;

    procedure CEFInitializedMsg(var aMessage : TMessage); message CEF_INITIALIZED;
    procedure DestroyTabMsg(var aMessage : TMessage); message CEF_DESTROYTAB;
    procedure CreateNextChildMsg(var aMessage : TMessage); message CEF_CREATENEXTCHILD;
    procedure CreateNextTabMsg(var aMessage : TMessage); message CEF_CREATENEXTTAB;
    procedure ChildDestroyedMsg(var aMessage : TMessage); message CEF_CHILDDESTROYED;
    procedure WMMove(var aMessage : TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage : TMessage); message WM_MOVING;
    procedure WMEnterMenuLoop(var aMessage: TMessage); message WM_ENTERMENULOOP;
    procedure WMExitMenuLoop(var aMessage: TMessage); message WM_EXITMENULOOP;
    procedure WMQueryEndSession(var aMessage: TWMQueryEndSession); message WM_QUERYENDSESSION;

    property  NextTabID       : cardinal   read GetNextTabID;
    property  PopupChildCount : integer    read GetPopupChildCount;
    property  BrowserTabCount : integer    read GetBrowserTabCount;

  public
    function  DoOnBeforePopup(var windowInfo : TCefWindowInfo; var client : ICefClient; const targetFrameName : string; const popupFeatures : TCefPopupFeatures; targetDisposition : TCefWindowOpenDisposition) : boolean;
    function  DoOpenUrlFromTab(const targetUrl : string; targetDisposition : TCefWindowOpenDisposition) : boolean;
  end;

var
  MainForm: TMainForm;

procedure CreateGlobalCEFApp;

implementation

{$R *.dfm}


// This demo shows how to use a TPageControl with TFrames that include
// CEF4Delphi browsers.

// Instead of a regular TTabSheet we use a custom TBrowserTab class that
// inherits from TTabSheet and instead of a regular TFrame we use a custom
// TBrowserFrame class that inherits from TFrame.

// To create a new tab you need to call TBrowserTab.CreateBrowser in the last
// step to create all the browser components and initialize the browser.

// To close a tab you have to call TBrowserTab.CloseBrowser and wait for a
// CEF_DESTROYTAB message that includes TBrowserTab.TabID in TMessage.wParam.
// Then you find the tab with that unique TabID and free it.

// TBrowserFrame has all the usual code to close CEF4Delphi browsers following
// a similar destruction sequence than the MiniBrowser demo :
//
// 1. TBrowserTab.CloseBrowser calls TChromium.CloseBrowser which triggers the
//    TChromium.OnClose event.
// 2. TChromium.OnClose sends a CEF_DESTROY message to destroy CEFWindowParent1
//    in the main thread, which triggers the TChromium.OnBeforeClose event.
// 3. TChromium.OnBeforeClose executes the TBrowserFrame.OnBrowserDestroyed
//    event which will be used in TBrowserTab to send a CEF_DESTROYTAB message
//    to the main form to free the tab.

// This demo also uses custom forms to open popup browsers in the same way as
// the PopupBrowser2 demo. Please, read the code comments in that demo for all
// details about handling the custom child forms.

// Additionally, this demo also creates new tabs when a browser triggers the
// TChromium.OnBeforePopup event.

// VCL components *MUST* be created and destroyed in the main thread but CEF
// executes the TChromium.OnBeforePopup in a different thread.

// For this reason this demo creates a hidden popup form (TChildForm) and a
// hidden TBrowserTab in case CEF needs to show a popup window.

// TChromium.OnBeforePopup calls TMainForm.DoOnBeforePopup to handle all the
// events in the same place.

// TMainForm.DoOnBeforePopup will call CreateClientHandler to initialize some
// parameters and create the new ICefClient using the hidden form or tab.

// After that, it sends a custom message to show the popup form or tab and create
// a new one.

// To close safely this demo you must close all the browser tabs first following
// this steps :
//
// 1. FormCloseQuery sets CanClose to FALSE and calls CloseAllBrowsers and FClosing
//    is set to TRUE.
// 2. Each tab will send a CEF_DESTROYTAB message to the main form to free that tab.
// 3. Each child form will send a CEF_CHILDDESTROYED message to the main form.
// 4. When TPageControl has no tabs and all the child forms are also closed then we
//    can set FCanClose to TRUE and send a WM_CLOSE message to the main form to
//    close the application.

procedure GlobalCEFApp_OnContextInitialized;
begin
  if (MainForm <> nil) and MainForm.HandleAllocated then
    PostMessage(MainForm.Handle, CEF_INITIALIZED, 0, 0);
end;

procedure CreateGlobalCEFApp;
begin
  GlobalCEFApp                      := TCefApplication.Create;
  GlobalCEFApp.cache                := 'cache';
  GlobalCEFApp.EnablePrintPreview   := True;
  GlobalCEFApp.OnContextInitialized := GlobalCEFApp_OnContextInitialized;
end;

procedure TMainForm.EnableButtonPnl;
begin
  if not(ButtonPnl.Enabled) then
    begin
      ButtonPnl.Enabled := True;
      Caption           := 'Tabbed Browser 2';
      cursor            := crDefault;
      if (BrowserTabCount = 0) then AddTabBtn.Click;
    end;
end;

function TMainForm.GetNextTabID : cardinal;
begin
  inc(FLastTabID);
  Result := FLastTabID;
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
      // Only count the fully initialized child forms and not the one waiting to be used.
      TempForm := screen.CustomForms[i];
      if (TempForm is TChildForm) and
         TChildForm(TempForm).Initialized then
        inc(Result);

      dec(i);
    end;
end;

function TMainForm.GetBrowserTabCount : integer;
var
  i : integer;
begin
  Result := 0;
  i      := pred(BrowserPageCtrl.PageCount);

  while (i >= 0) do
    begin
      // Only count the fully initialized browser tabs and not the one waiting to be used.

      if TBrowserTab(BrowserPageCtrl.Pages[i]).Initialized then
        inc(Result);

      dec(i);
    end;
end;

procedure TMainForm.AddTabBtnClick(Sender: TObject);
var
  TempNewTab : TBrowserTab;
begin
  TempNewTab             := TBrowserTab.Create(self, NextTabID, DEFAULT_TAB_CAPTION);
  TempNewTab.PageControl := BrowserPageCtrl;

  BrowserPageCtrl.ActivePageIndex := pred(BrowserPageCtrl.PageCount);

  TempNewTab.CreateBrowser(HOMEPAGE_URL);
end;

procedure TMainForm.CEFInitializedMsg(var aMessage : TMessage);
begin
  EnableButtonPnl;
  CreateHiddenBrowsers;
end;

procedure TMainForm.DestroyTabMsg(var aMessage : TMessage);
var
  i : integer;
  TempTab : TBrowserTab;
begin
  // Every tab sends a CEF_DESTROYTAB message when its browser has been destroyed
  // and then we can destroy the TBrowserTab control.
  i := 0;
  while (i < BrowserPageCtrl.PageCount) do
    begin
      TempTab := TBrowserTab(BrowserPageCtrl.Pages[i]);

      if (TempTab.TabID = aMessage.wParam) then
        begin
          TempTab.Free;
          break;
        end
       else
        inc(i);
    end;

  // Here we check if this was the last initialized browser to close the
  // application safely.
  if FClosing and (PopupChildCount = 0) and (BrowserTabCount = 0) then
    begin
      FCanClose := True;
      PostMessage(Handle, WM_CLOSE, 0, 0);
    end;
end;

procedure TMainForm.ChildDestroyedMsg(var aMessage : TMessage);
begin
  // Every destroyed child form sends a CEF_CHILDDESTROYED message
  // Here we check if this was the last initialized browser to close the
  // application safely.
  if FClosing and (PopupChildCount = 0) and (BrowserTabCount = 0) then
    begin
      FCanClose := True;
      PostMessage(Handle, WM_CLOSE, 0, 0);
    end;
end;

procedure TMainForm.CreateChildForm;
begin
  inc(FChildCounter);

  FChildForm         := TChildForm.Create(self);
  FChildForm.Name    := 'ChildForm_' + IntToStr(FChildCounter);
end;


procedure TMainForm.CreateNextChildMsg(var aMessage : TMessage);
begin
  try
    FCriticalSection.Acquire;

    if (FChildForm <> nil) then
      begin
        if (aMessage.lParam <> 0) then
          FChildForm.CreateBrowser(FPendingURL)

         else
          FChildForm.ApplyPopupFeatures;

        FChildForm.Show;
      end;

    CreateChildForm;
  finally
    FCriticalSection.Release;
  end;
end;

procedure TMainForm.CreateNextTabMsg(var aMessage : TMessage);
begin
  try
    FCriticalSection.Acquire;

    if (FHiddenTab <> nil) then
      begin
        FHiddenTab.TabVisible := True;
        FHiddenTab.PageIndex  := pred(BrowserPageCtrl.PageCount);

        if (aMessage.lParam <> 0) then
          FHiddenTab.CreateBrowser(FPendingURL);

        BrowserPageCtrl.ActivePageIndex := FHiddenTab.PageIndex;
      end;

    FHiddenTab             := TBrowserTab.Create(self, NextTabID, DEFAULT_TAB_CAPTION);
    FHiddenTab.PageControl := BrowserPageCtrl;
    FHiddenTab.TabVisible  := False;
    FHiddenTab.CreateFrame;
  finally
    FCriticalSection.Release;
  end;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FCanClose;

  if not(FClosing) then
    begin
      FClosing          := True;
      ButtonPnl.Enabled := False;

      if not(CloseAllBrowsers) then
        begin
          FCanClose := True;
          PostMessage(Handle, WM_CLOSE, 0, 0);
        end;
    end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FCanClose        := False;
  FClosing         := False;
  FLastTabID       := 0;
  FChildForm       := nil;
  FHiddenTab       := nil;
  FChildCounter    := 0;
  FCriticalSection := TCriticalSection.Create;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FCriticalSection);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.GlobalContextInitialized then
    begin
      EnableButtonPnl;
      CreateHiddenBrowsers;
    end;
end;

procedure TMainForm.RemoveTabBtnClick(Sender: TObject);
begin
  // Call TBrowserTab.CloseBrowser in the active tab
  CloseTab(BrowserPageCtrl.ActivePageIndex);
end;

function TMainForm.CloseAllBrowsers : boolean;
var
  i        : integer;
  TempForm : TCustomForm;
  TempTab  : TBrowserTab;
begin
  Result := False;
  i      := pred(screen.CustomFormCount);
  while (i >= 0) do
    begin
      TempForm := screen.CustomForms[i];

      if (TempForm is TChildForm) and
         TChildForm(TempForm).Initialized and
         not(TChildForm(TempForm).Closing) then
        begin
          PostMessage(TempForm.Handle, WM_CLOSE, 0, 0);
          Result := True;
        end;

      dec(i);
    end;

  i := pred(BrowserPageCtrl.PageCount);
  while (i >= 0) do
    begin
      TempTab := TBrowserTab(BrowserPageCtrl.Pages[i]);

      if TempTab.Initialized and not(TempTab.Closing) then
        begin
          TempTab.CloseBrowser;
          Result := True;
        end;

      dec(i);
    end;
end;

procedure TMainForm.CloseTab(aIndex : integer);
begin
  if (aIndex >= 0) and (aIndex < BrowserPageCtrl.PageCount) then
    TBrowserTab(BrowserPageCtrl.Pages[aIndex]).CloseBrowser;
end;

procedure TMainForm.CreateHiddenBrowsers;
begin
  try
    FCriticalSection.Acquire;

    if (FChildForm = nil) then
      CreateChildForm;

    if (FHiddenTab = nil) then
      begin
        FHiddenTab             := TBrowserTab.Create(self, NextTabID, DEFAULT_TAB_CAPTION);
        FHiddenTab.PageControl := BrowserPageCtrl;
        FHiddenTab.TabVisible  := False;
        FHiddenTab.CreateFrame;
      end;
  finally
    FCriticalSection.Release;
  end;
end;

procedure TMainForm.WMMove(var aMessage : TWMMove);
var
  i : integer;
begin
  inherited;

  i := 0;
  while (i < BrowserPageCtrl.PageCount) do
    begin
      TBrowserTab(BrowserPageCtrl.Pages[i]).NotifyMoveOrResizeStarted;
      inc(i);
    end;
end;

procedure TMainForm.WMMoving(var aMessage : TMessage);
var
  i : integer;
begin
  inherited;

  i := 0;
  while (i < BrowserPageCtrl.PageCount) do
    begin
      TBrowserTab(BrowserPageCtrl.Pages[i]).NotifyMoveOrResizeStarted;
      inc(i);
    end;
end;

procedure TMainForm.WMEnterMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then
    GlobalCEFApp.OsmodalLoop := True;
end;

procedure TMainForm.WMExitMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then
    GlobalCEFApp.OsmodalLoop := False;
end;

procedure TMainForm.WMQueryEndSession(var aMessage: TWMQueryEndSession);
begin
  // We return False (0) to close the browser correctly while we can.
  // This is not what Microsoft recommends doing when an application receives
  // WM_QUERYENDSESSION but at least we avoid TApplication calling HALT when
  // it receives WM_ENDSESSION.
  // The CEF subprocesses may receive WM_QUERYENDSESSION and WM_ENDSESSION
  // before the main process and they may crash before closing the main form.
  aMessage.Result := 0;
  PostMessage(Handle, WM_CLOSE, 0, 0);
end;

function TMainForm.DoOnBeforePopup(var   windowInfo        : TCefWindowInfo;
                                   var   client            : ICefClient;
                                   const targetFrameName   : string;
                                   const popupFeatures     : TCefPopupFeatures;
                                         targetDisposition : TCefWindowOpenDisposition) : boolean;
begin
  try
    FCriticalSection.Acquire;

    case targetDisposition of
      CEF_WOD_NEW_FOREGROUND_TAB,
      CEF_WOD_NEW_BACKGROUND_TAB :
        Result := (FHiddenTab <> nil) and
                  FHiddenTab.CreateClientHandler(windowInfo, client, targetFrameName, popupFeatures) and
                  PostMessage(Handle, CEF_CREATENEXTTAB, 0, ord(False));

      CEF_WOD_NEW_WINDOW,
      CEF_WOD_NEW_POPUP :
        Result := (FChildForm <> nil) and
                  FChildForm.CreateClientHandler(windowInfo, client, targetFrameName, popupFeatures) and
                  PostMessage(Handle, CEF_CREATENEXTCHILD, 0, ord(False));

      else Result := False;
    end;
  finally
    FCriticalSection.Release;
  end;
end;

function TMainForm.DoOpenUrlFromTab(const targetUrl         : string;
                                          targetDisposition : TCefWindowOpenDisposition) : boolean;
begin
  try
    FCriticalSection.Acquire;

    case targetDisposition of
      CEF_WOD_NEW_FOREGROUND_TAB,
      CEF_WOD_NEW_BACKGROUND_TAB :
        begin
          FPendingURL := targetUrl;
          Result      := PostMessage(Handle, CEF_CREATENEXTTAB, 0, ord(True));
        end;

      CEF_WOD_NEW_WINDOW,
      CEF_WOD_NEW_POPUP :
        begin
          FPendingURL := targetUrl;
          Result      := PostMessage(Handle, CEF_CREATENEXTCHILD, 0, ord(True));
        end

      else Result := False;
    end;
  finally
    FCriticalSection.Release;
  end;
end;

end.
