unit uBrowserTab;

{$mode objfpc}{$H+}

{$I ../../../source/cef.inc}

interface

uses
  LCLIntf, LCLType, LMessages, Classes, Messages, ComCtrls, Controls,
  Forms,
  uBrowserFrame;

type
  TBrowserTab = class(TTabSheet)
    protected
      FBrowserFrame : TBrowserFrame;
      FTabID        : cardinal;

      function    GetParentForm : TCustomForm;

      procedure   SendFormMessage(aMsg : cardinal; aData : PtrInt = 0);

      procedure   BrowserFrame_OnBrowserDestroyed(Sender: TObject);
      procedure   BrowserFrame_OnBrowserTitleChange(Sender: TObject; const aTitle : string);

      property    ParentForm : TCustomForm  read GetParentForm;

    public
      constructor Create(AOwner: TComponent; aTabID : cardinal; const aCaption : string); reintroduce;
      procedure   NotifyMoveOrResizeStarted;
      procedure   CreateBrowser(const aHomepage : string);
      procedure   CloseBrowser;

      property    TabID      : cardinal   read FTabID;
  end;

implementation

uses
  uMainForm;

constructor TBrowserTab.Create(AOwner: TComponent; aTabID : cardinal; const aCaption : string);
begin
  inherited Create(AOwner);

  FTabID        := aTabID;
  Caption       := aCaption;
  FBrowserFrame := nil;
end;

function TBrowserTab.GetParentForm : TCustomForm;
var
  TempParent : TWinControl;
begin
  TempParent := Parent;

  while (TempParent <> nil) and not(TempParent is TCustomForm) do
    TempParent := TempParent.Parent;

  if (TempParent <> nil) and (TempParent is TCustomForm) then
    Result := TCustomForm(TempParent)
   else
    Result := nil;
end;

procedure TBrowserTab.SendFormMessage(aMsg : cardinal; aData : PtrInt);
var
  TempForm : TCustomForm;
begin
  TempForm := ParentForm;
  if (TempForm <> nil) and (TempForm is TMainForm) then
    TMainForm(TempForm).SendCompMessage(aMsg, aData);
end;

procedure TBrowserTab.NotifyMoveOrResizeStarted;
begin
  FBrowserFrame.NotifyMoveOrResizeStarted;
end;

procedure TBrowserTab.CreateBrowser(const aHomepage : string);
begin
  FBrowserFrame                      := TBrowserFrame.Create(self);
  FBrowserFrame.Parent               := self;
  FBrowserFrame.Align                := alClient;
  FBrowserFrame.Visible              := True;
  FBrowserFrame.Homepage             := aHomepage;
  FBrowserFrame.OnBrowserDestroyed   := @BrowserFrame_OnBrowserDestroyed;
  FBrowserFrame.OnBrowserTitleChange := @BrowserFrame_OnBrowserTitleChange;

  FBrowserFrame.CreateBrowser;
end;

procedure TBrowserTab.CloseBrowser;
begin
  if (FBrowserFrame <> nil) then FBrowserFrame.CloseBrowser;
end;

procedure TBrowserTab.BrowserFrame_OnBrowserDestroyed(Sender: TObject);
begin                        
  // This event is executed in a CEF thread so we have to use
  // Application.QueueAsyncCall to destroy the tab in the main application
  // thread.
  SendFormMessage(CEF_DESTROYTAB, PtrInt(TabID));
end;

procedure TBrowserTab.BrowserFrame_OnBrowserTitleChange(Sender: TObject; const aTitle : string);
begin
  Caption := aTitle;
end;

end.
