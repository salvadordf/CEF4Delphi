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

unit uCEFChromiumWindow;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
    {$IFDEF MSWINDOWS}WinApi.Windows, WinApi.Messages,{$ENDIF} System.Classes, Vcl.Controls,
  {$ELSE}
    {$IFDEF MSWINDOWS}Windows,{$ENDIF} Classes, Forms, Controls, Graphics,
    {$IFDEF FPC}
    LCLProc, LCLType, LCLIntf, LResources, LMessages, InterfaceBase,
    {$ELSE}
    Messages,
    {$ENDIF}
  {$ENDIF}
  uCEFWindowParent, uCEFChromium, uCEFInterfaces, uCEFConstants, uCEFTypes,
  uCEFWinControl, uCEFLinkedWinControlBase;

type
  {$IFNDEF FPC}{$IFDEF DELPHI16_UP}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}{$ENDIF}

  { TChromiumWindow }

  TChromiumWindow = class(TCEFLinkedWinControlBase)
    protected
      FChromium       : TChromium;
      FOnClose        : TNotifyEvent;
      FOnBeforeClose  : TNotifyEvent;
      FOnAfterCreated : TNotifyEvent;
      FUseSetFocus    : boolean;

      function    GetChromium: TChromium; override;
      function    GetUseSetFocus: Boolean; override;
      function    GetBrowserInitialized : boolean;
      {$IFDEF MSWINDOWS}
      procedure   OnCloseMsg(var aMessage : TMessage); message CEF_DOONCLOSE;
      procedure   OnAfterCreatedMsg(var aMessage : TMessage); message CEF_AFTERCREATED;
      {$ENDIF}
      procedure   WebBrowser_OnClose(Sender: TObject; const browser: ICefBrowser; var aAction : TCefCloseBrowserAction);
      procedure   WebBrowser_OnBeforeClose(Sender: TObject; const browser: ICefBrowser);
      procedure   WebBrowser_OnAfterCreated(Sender: TObject; const browser: ICefBrowser);
      {$IFDEF FPC}
      procedure   WebBrowser_OnGotFocus(Sender: TObject; const browser: ICefBrowser);
      procedure   BrowserSetFocusMsg(Data: PtrInt);
      procedure   BrowserAfterCreated(Data: PtrInt);
      procedure   BrowserOnCLose(Data: PtrInt);
      {$ENDIF}
      procedure   DoEnter; override;
      procedure   DoExit; override;

   public
      constructor Create(AOwner: TComponent); override;
      procedure   AfterConstruction; override;
      function    CreateBrowser : boolean;
      procedure   CloseBrowser(aForceClose : boolean);
      procedure   LoadURL(const aURL : ustring);
      procedure   NotifyMoveOrResizeStarted;

      property ChromiumBrowser  : TChromium       read GetChromium;
      property Initialized      : boolean         read GetBrowserInitialized;

    published
      property UseSetFocus      : boolean         read FUseSetFocus      write FUseSetFocus default True;
      property OnClose          : TNotifyEvent    read FOnClose          write FOnClose;
      property OnBeforeClose    : TNotifyEvent    read FOnBeforeClose    write FOnBeforeClose;
      property OnAfterCreated   : TNotifyEvent    read FOnAfterCreated   write FOnAfterCreated;
  end;

{$IFDEF FPC}
procedure Register;
{$ENDIF}

// *********************************************************
// ********************** ATTENTION ! **********************
// *********************************************************
// **                                                     **
// **  MANY OF THE EVENTS IN CEF4DELPHI COMPONENTS LIKE   **
// **  TCHROMIUM, TFMXCHROMIUM OR TCEFAPPLICATION ARE     **
// **  EXECUTED IN A CEF THREAD BY DEFAULT.               **
// **                                                     **
// **  WINDOWS CONTROLS MUST BE CREATED AND DESTROYED IN  **
// **  THE SAME THREAD TO AVOID ERRORS.                   **
// **  SOME OF THEM RECREATE THE HANDLERS IF THEY ARE     **
// **  MODIFIED AND CAN CAUSE THE SAME ERRORS.            **
// **                                                     **
// **  DON'T CREATE, MODIFY OR DESTROY WINDOWS CONTROLS   **
// **  INSIDE THE CEF4DELPHI EVENTS AND USE               **
// **  SYNCHRONIZATION OBJECTS TO PROTECT VARIABLES AND   **
// **  FIELDS IF THEY ARE ALSO USED IN THE MAIN THREAD.   **
// **                                                     **
// **  READ THIS FOR MORE INFORMATION :                   **
// **  https://www.briskbard.com/index.php?pageid=cef     **
// **                                                     **
// **  USE OUR FORUMS FOR MORE QUESTIONS :                **
// **  https://www.briskbard.com/forum/                   **
// **                                                     **
// *********************************************************
// *********************************************************

// This component should *ONLY* be used in emtremely simple applications with simple browsers.
// In other cases it's recomended using a TChromium with a TCEFWindowParent as shown in the
// SimpleBrowser2 demo.

implementation

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils;
  {$ELSE}
  SysUtils;
  {$ENDIF}

constructor TChromiumWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FChromium       := nil;
  FOnClose        := nil;
  FOnBeforeClose  := nil;
  FOnAfterCreated := nil;
  FUseSetFocus    := True;
end;

procedure TChromiumWindow.AfterConstruction;
begin
  inherited AfterConstruction;

  if not(csDesigning in ComponentState) then
    begin
      FChromium                := TChromium.Create(self);
      FChromium.OnClose        := {$IFDEF FPC}@{$ENDIF}WebBrowser_OnClose;
      FChromium.OnBeforeClose  := {$IFDEF FPC}@{$ENDIF}WebBrowser_OnBeforeClose;
      FChromium.OnAfterCreated := {$IFDEF FPC}@{$ENDIF}WebBrowser_OnAfterCreated;
      {$IFDEF LINUX}
      // This is a workaround for the CEF issue #2026. Read below for more info.
      FChromium.OnGotFocus     := {$IFDEF FPC}@{$ENDIF}WebBrowser_OnGotFocus;
      TabStop                  := True;
      {$ENDIF}
    end;
end;

function TChromiumWindow.GetBrowserInitialized : boolean;
begin
  Result := (FChromium <> nil) and FChromium.Initialized;
end;

procedure TChromiumWindow.WebBrowser_OnClose(Sender: TObject; const browser: ICefBrowser; var aAction : TCefCloseBrowserAction);
begin
  aAction := cbaClose;
  if assigned(FOnClose) then
    begin
      {$IFDEF MSWINDOWS}
        PostMessage(Handle, CEF_DOONCLOSE, 0, 0);
        aAction := cbaDelay;
      {$ELSE}
        {$IFDEF FPC}
        Application.QueueAsyncCall(@BrowserOnClose, 0);
        {$ENDIF}
      {$ENDIF}
    end;
end;

procedure TChromiumWindow.WebBrowser_OnBeforeClose(Sender: TObject; const browser: ICefBrowser);
begin
  if assigned(FOnBeforeClose) then FOnBeforeClose(self);
end;

procedure TChromiumWindow.WebBrowser_OnAfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  {$IFDEF MSWINDOWS}
    PostMessage(Handle, CEF_AFTERCREATED, 0, 0);
  {$ELSE}
    {$IFDEF FPC}
    Application.QueueAsyncCall(@BrowserAfterCreated, 0);
    {$ENDIF}
  {$ENDIF}
end;

{$IFDEF MSWINDOWS}
procedure TChromiumWindow.OnCloseMsg(var aMessage : TMessage);
begin
  if assigned(FOnClose) then FOnClose(self);
end;

procedure TChromiumWindow.OnAfterCreatedMsg(var aMessage : TMessage);
begin
  UpdateSize;
  if assigned(FOnAfterCreated) then FOnAfterCreated(self);
end;
{$ENDIF}

{$IFDEF FPC}
procedure TChromiumWindow.WebBrowser_OnGotFocus(Sender: TObject; const browser: ICefBrowser);
begin
  Application.QueueAsyncCall(@BrowserSetFocusMsg, 0);
end;

procedure TChromiumWindow.BrowserSetFocusMsg(Data: PtrInt);
begin
  SetFocus;
end;

procedure TChromiumWindow.BrowserAfterCreated(Data: PtrInt);
begin
  UpdateSize;
  if assigned(FOnAfterCreated) then FOnAfterCreated(self);
end;

procedure TChromiumWindow.BrowserOnCLose(Data: PtrInt);
begin
  if assigned(FOnClose) then FOnClose(self);
end;
{$ENDIF}

function TChromiumWindow.CreateBrowser : boolean;
begin
  Result := not(csDesigning in ComponentState) and
            (FChromium <> nil) and
            FChromium.CreateBrowser(self, '');
end;

procedure TChromiumWindow.CloseBrowser(aForceClose : boolean);
begin
  if (FChromium <> nil) then FChromium.CloseBrowser(aForceClose);
end;

procedure TChromiumWindow.LoadURL(const aURL : ustring);
begin
  if not(csDesigning in ComponentState) and (FChromium <> nil) then
    FChromium.LoadURL(aURL);
end;

procedure TChromiumWindow.NotifyMoveOrResizeStarted;
begin
  if (FChromium <> nil) then FChromium.NotifyMoveOrResizeStarted;
end;

function TChromiumWindow.GetChromium: TChromium;
begin
  result := FChromium;
end;

function TChromiumWindow.GetUseSetFocus: Boolean;
begin
  Result := FUseSetFocus;
end;

// This is a workaround for the CEF issue #2026
// https://bitbucket.org/chromiumembedded/cef/issues/2026/multiple-major-keyboard-focus-issues-on
// We use ChromiumWindow1.OnEnter, ChromiumWindow1.OnExit and
// TChromium.OnGotFocus to avoid most of the focus issues.
// ChromiumWindow1.TabStop must be TRUE.
procedure TChromiumWindow.DoEnter;
begin
  inherited DoEnter;

  {$IFDEF LINUX}
  if not(csDesigning in ComponentState) and
     FChromium.Initialized and
     not(FChromium.FrameIsFocused) then
    FChromium.SetFocus(True);
  {$ENDIF}
end;

procedure TChromiumWindow.DoExit;
begin
  inherited DoExit;

  {$IFDEF LINUX}
  if not(csDesigning in ComponentState) then
    FChromium.SendCaptureLostEvent;
  {$ENDIF}
end;

{$IFDEF FPC}
procedure Register;
begin
  {$I res/tchromiumwindow.lrs}
  RegisterComponents('Chromium', [TChromiumWindow]);
end;
{$ENDIF}

end.
