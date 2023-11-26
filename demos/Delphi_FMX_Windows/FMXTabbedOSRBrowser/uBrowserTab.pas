unit uBrowserTab;

{$I ..\..\..\source\cef.inc}

interface

uses
  Winapi.Windows, System.Classes, System.SysUtils, FMX.Forms, FMX.Types,
  FMX.Controls, FMX.TabControl,
  uBrowserFrame;

type
  TBrowserTab = class(TTabItem)
    protected
      FBrowserFrame : TBrowserFrame;
      FTabID        : cardinal;

      function    GetParentForm : TCustomForm;

      procedure   BrowserFrame_OnBrowserDestroyed(Sender: TObject);
      procedure   BrowserFrame_OnBrowserTitleChange(Sender: TObject; const aTitle : string);
      procedure   BrowserFrame_OnBrowserNeedsResize(Sender: TObject);

    public
      constructor Create(AOwner: TComponent; aTabID : cardinal; const aCaption : string); reintroduce;
      procedure   NotifyMoveOrResizeStarted;
      procedure   CreateBrowser(const aHomepage : string);
      procedure   CloseBrowser;
      procedure   ResizeBrowser;
      procedure   FocusBrowser;
      procedure   ShowBrowser;
      procedure   HideBrowser;
      procedure   SendCaptureLostEvent;
      {$IFDEF MSWINDOWS}
      procedure   HandleSYSCHAR(const aMessage : TMsg);
      procedure   HandleSYSKEYDOWN(const aMessage : TMsg);
      procedure   HandleSYSKEYUP(const aMessage : TMsg);
      procedure   HandleKEYDOWN(const aMessage : TMsg);
      procedure   HandleKEYUP(const aMessage : TMsg);
      function    HandlePOINTER(const aMessage : TMsg) : boolean;
      function    PostFormMessage(aMsg : cardinal; aWParam : WPARAM = 0; aLParam : LPARAM = 0) : boolean;
      {$ENDIF}

      property    TabID      : cardinal     read FTabID;
      property    ParentForm : TCustomForm  read GetParentForm;
  end;

implementation
uses
  uMainForm, uCEFConstants;

constructor TBrowserTab.Create(AOwner: TComponent; aTabID : cardinal; const aCaption : string);
begin
  inherited Create(AOwner);

  FTabID        := aTabID;
  Text          := aCaption;
  FBrowserFrame := nil;
  Name          := 'BrowserTab' + inttostr(aTabID);
end;

function TBrowserTab.GetParentForm : TCustomForm;
var
  TempParent : TFMXObject;
begin
  TempParent := Parent;

  while (TempParent <> nil) and not(TempParent is TCustomForm) do
    TempParent := TempParent.Parent;

  if (TempParent <> nil) and (TempParent is TCustomForm) then
    Result := TCustomForm(TempParent)
   else
    Result := nil;
end;

procedure TBrowserTab.SendCaptureLostEvent;
begin
  if (FBrowserFrame <> nil) then
    FBrowserFrame.SendCaptureLostEvent;
end;

procedure TBrowserTab.NotifyMoveOrResizeStarted;
begin
  if (FBrowserFrame <> nil) then
    FBrowserFrame.NotifyMoveOrResizeStarted;
end;

procedure TBrowserTab.CreateBrowser(const aHomepage : string);
begin
  FBrowserFrame                      := TBrowserFrame.Create(self);
  FBrowserFrame.Parent               := self;
  FBrowserFrame.Align                := TAlignLayout.Client;
  FBrowserFrame.Visible              := True;
  FBrowserFrame.Homepage             := aHomepage;
  FBrowserFrame.Name                 := 'BrowserFrame' + inttostr(FTabID);
  FBrowserFrame.OnBrowserDestroyed   := BrowserFrame_OnBrowserDestroyed;
  FBrowserFrame.OnBrowserTitleChange := BrowserFrame_OnBrowserTitleChange;
  FBrowserFrame.OnBrowserNeedsResize := BrowserFrame_OnBrowserNeedsResize;

  FBrowserFrame.CreateBrowser;
end;

procedure TBrowserTab.CloseBrowser;
begin
  if (FBrowserFrame <> nil) then FBrowserFrame.CloseBrowser;
end;

procedure TBrowserTab.ResizeBrowser;
begin
  if (FBrowserFrame <> nil) then FBrowserFrame.ResizeBrowser;
end;

procedure TBrowserTab.FocusBrowser;
begin
  if (FBrowserFrame <> nil) then FBrowserFrame.FocusBrowser;
end;

procedure TBrowserTab.ShowBrowser;
begin
  if (FBrowserFrame <> nil) then FBrowserFrame.ShowBrowser;
end;

procedure TBrowserTab.HideBrowser;
begin
  if (FBrowserFrame <> nil) then FBrowserFrame.HideBrowser;
end;

procedure TBrowserTab.BrowserFrame_OnBrowserDestroyed(Sender: TObject);
begin
  // This event is executed in a CEF thread so we have to send a message to
  // destroy the tab in the main application thread.
  {$IFDEF MSWINDOWS}
  PostFormMessage(CEF_DESTROYTAB, TabID);
  {$ENDIF}
end;

procedure TBrowserTab.BrowserFrame_OnBrowserTitleChange(Sender: TObject; const aTitle : string);
begin
  Text := aTitle;
end;

procedure TBrowserTab.BrowserFrame_OnBrowserNeedsResize(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}
  PostFormMessage(CEF_PENDINGRESIZE, TabID);
  {$ENDIF}
end;

{$IFDEF MSWINDOWS}
procedure TBrowserTab.HandleSYSCHAR(const aMessage : TMsg);
begin
  if (FBrowserFrame <> nil) then
    FBrowserFrame.HandleSYSCHAR(aMessage);
end;

procedure TBrowserTab.HandleSYSKEYDOWN(const aMessage : TMsg);
begin
  if (FBrowserFrame <> nil) then
    FBrowserFrame.HandleSYSKEYDOWN(aMessage);
end;

procedure TBrowserTab.HandleSYSKEYUP(const aMessage : TMsg);
begin
  if (FBrowserFrame <> nil) then
    FBrowserFrame.HandleSYSKEYUP(aMessage);
end;

procedure TBrowserTab.HandleKEYDOWN(const aMessage : TMsg);
begin
  if (FBrowserFrame <> nil) then
    FBrowserFrame.HandleKEYDOWN(aMessage);
end;

procedure TBrowserTab.HandleKEYUP(const aMessage : TMsg);
begin
  if (FBrowserFrame <> nil) then
    FBrowserFrame.HandleKEYUP(aMessage);
end;

function TBrowserTab.HandlePOINTER(const aMessage : TMsg) : boolean;
begin
  Result := (FBrowserFrame <> nil) and
            FBrowserFrame.HandlePOINTER(aMessage);
end;

function TBrowserTab.PostFormMessage(aMsg : cardinal; aWParam : WPARAM; aLParam : LPARAM) : boolean;
var
  TempForm : TCustomForm;
begin
  TempForm := ParentForm;
  Result   := (TempForm <> nil) and
              (TempForm is TMainForm) and
              TMainForm(TempForm).PostCustomMessage(aMsg, aWParam, aLParam);
end;
{$ENDIF}

end.
