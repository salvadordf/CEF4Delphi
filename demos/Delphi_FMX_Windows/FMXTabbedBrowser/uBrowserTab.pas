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

unit uBrowserTab;

{$I cef.inc}

interface

uses
  System.Classes, System.SysUtils, FMX.Forms, FMX.Types, FMX.Controls, FMX.TabControl,
  uBrowserFrame;

type
  TBrowserTab = class(TTabItem)
    protected
      FBrowserFrame : TBrowserFrame;
      FTabID        : cardinal;

      function    GetParentForm : TCustomForm;

      procedure   BrowserFrame_OnBrowserDestroyed(Sender: TObject);
      procedure   BrowserFrame_OnBrowserTitleChange(Sender: TObject; const aTitle : string);
      procedure   BrowserFrame_OnBrowserClosing(Sender: TObject);

    public
      constructor Create(AOwner: TComponent; aTabID : cardinal; const aCaption : string); reintroduce;
      procedure   NotifyMoveOrResizeStarted;
      procedure   DestroyWindowParent;
      procedure   CreateBrowser(const aHomepage : string);
      procedure   CloseBrowser;
      procedure   ResizeBrowser;
      procedure   ShowBrowser;
      procedure   HideBrowser;
      function    PostFormMessage(aMsg : cardinal; wParam : cardinal = 0; lParam : integer = 0) : boolean;

      property    TabID      : cardinal     read FTabID;
      property    ParentForm : TCustomForm  read GetParentForm;
  end;

implementation
uses
  uMainForm;

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

function TBrowserTab.PostFormMessage(aMsg, wParam : cardinal; lParam : integer) : boolean;
var
  TempForm : TCustomForm;
begin
  TempForm := ParentForm;
  Result   := (TempForm <> nil) and
              (TempForm is TMainForm) and
              TMainForm(TempForm).PostCustomMessage(aMsg, wParam, lParam);
end;

procedure TBrowserTab.NotifyMoveOrResizeStarted;
begin
  if (FBrowserFrame <> nil) then
    FBrowserFrame.NotifyMoveOrResizeStarted;
end;

procedure TBrowserTab.DestroyWindowParent;
begin
  if (FBrowserFrame <> nil) then
    FBrowserFrame.DestroyWindowParent;
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
  FBrowserFrame.OnBrowserClosing     := BrowserFrame_OnBrowserClosing;

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
  PostFormMessage(CEF_DESTROYTAB, TabID);
end;

procedure TBrowserTab.BrowserFrame_OnBrowserTitleChange(Sender: TObject; const aTitle : string);
begin
  // This event is executed in a CEF thread so we have to use TThread.Queue to
  // set the "Text" property in the main application thread.
  TThread.Queue(nil, procedure
                     begin
                       Text := aTitle;
                     end);
end;

procedure TBrowserTab.BrowserFrame_OnBrowserClosing(Sender: TObject);
begin
  // This event is executed in a CEF thread so we have to send a message to
  // destroy TFMXWindowParent in the main application thread.
  PostFormMessage(CEF_DESTROYWINPARENT, TabID);
end;

end.
