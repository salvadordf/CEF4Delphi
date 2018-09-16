// ************************************************************************
// ***************************** CEF4Delphi *******************************
// ************************************************************************
//
// CEF4Delphi is based on DCEF3 which uses CEF3 to embed a chromium-based
// browser in Delphi applications.
//
// The original license of DCEF3 still applies to CEF4Delphi.
//
// For more information about CEF4Delphi visit :
//         https://www.briskbard.com/index.php?lang=en&pageid=cef
//
//        Copyright © 2018 Salvador Diaz Fau. All rights reserved.
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

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

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
  uCEFWindowParent, uCEFChromium, uCEFInterfaces, uCEFConstants, uCEFTypes, uCEFWinControl;

type
  TChromiumWindow = class(TCEFWinControl)
    protected
      FChromium       : TChromium;
      FOnClose        : TNotifyEvent;
      FOnBeforeClose  : TNotifyEvent;
      FOnAfterCreated : TNotifyEvent;

      function    GetBrowserInitialized : boolean;
      function    GetChildWindowHandle : THandle; override;

      procedure   WndProc(var aMessage: TMessage); override;

      procedure   OnCloseMsg(var aMessage : TMessage); message CEF_DOONCLOSE;
      procedure   OnBeforeCloseMsg(var aMessage : TMessage); message CEF_DOONBEFORECLOSE;
      procedure   OnAfterCreatedMsg(var aMessage : TMessage); message CEF_AFTERCREATED;

      procedure   WebBrowser_OnClose(Sender: TObject; const browser: ICefBrowser; out Result: Boolean);
      procedure   WebBrowser_OnBeforeClose(Sender: TObject; const browser: ICefBrowser);
      procedure   WebBrowser_OnAfterCreated(Sender: TObject; const browser: ICefBrowser);

   public
      constructor Create(AOwner: TComponent); override;
      procedure   AfterConstruction; override;
      function    CreateBrowser : boolean;
      procedure   CloseBrowser(aForceClose : boolean);
      procedure   LoadURL(const aURL : ustring);
      procedure   NotifyMoveOrResizeStarted;

      property ChromiumBrowser    : TChromium       read FChromium;
      property Initialized        : boolean         read GetBrowserInitialized;

    published
      property OnClose          : TNotifyEvent    read FOnClose          write FOnClose;
      property OnBeforeClose    : TNotifyEvent    read FOnBeforeClose    write FOnBeforeClose;
      property OnAfterCreated   : TNotifyEvent    read FOnAfterCreated   write FOnAfterCreated;
  end;

{$IFDEF FPC}
procedure Register;
{$ENDIF}

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
    end;
end;

function TChromiumWindow.GetChildWindowHandle : THandle;
begin
  Result := 0;

  if (FChromium <> nil) then Result := FChromium.WindowHandle;

  if (Result = 0) then Result := inherited GetChildWindowHandle;
end;

procedure TChromiumWindow.WndProc(var aMessage: TMessage);
var
  TempHandle : THandle;
begin
  case aMessage.Msg of
    WM_SETFOCUS:
      begin
        if (FChromium <> nil) then
          FChromium.SetFocus(True)
         else
          begin
            TempHandle := ChildWindowHandle;
            if (TempHandle <> 0) then PostMessage(TempHandle, WM_SETFOCUS, aMessage.WParam, 0);
          end;

        inherited WndProc(aMessage);
      end;

    WM_ERASEBKGND:
      if (ChildWindowHandle = 0) then inherited WndProc(aMessage);

    CM_WANTSPECIALKEY:
      if not(TWMKey(aMessage).CharCode in [VK_LEFT .. VK_DOWN, VK_RETURN, VK_ESCAPE]) then
        aMessage.Result := 1
       else
        inherited WndProc(aMessage);

    WM_GETDLGCODE : aMessage.Result := DLGC_WANTARROWS or DLGC_WANTCHARS;

    else inherited WndProc(aMessage);
  end;
end;

function TChromiumWindow.GetBrowserInitialized : boolean;
begin
  Result := (FChromium <> nil) and FChromium.Initialized;
end;

procedure TChromiumWindow.WebBrowser_OnClose(Sender: TObject; const browser: ICefBrowser; out Result: Boolean);
begin
  if assigned(FOnClose) then
    begin
      PostMessage(Handle, CEF_DOONCLOSE, 0, 0);
      Result := True;
    end
   else
    Result := False;
end;

procedure TChromiumWindow.WebBrowser_OnBeforeClose(Sender: TObject; const browser: ICefBrowser);
begin
  if assigned(FOnBeforeClose) then PostMessage(Handle, CEF_DOONBEFORECLOSE, 0, 0);
end;

procedure TChromiumWindow.WebBrowser_OnAfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  PostMessage(Handle, CEF_AFTERCREATED, 0, 0);
end;

procedure TChromiumWindow.OnCloseMsg(var aMessage : TMessage);
begin
  if assigned(FOnClose) then FOnClose(self);
end;

procedure TChromiumWindow.OnBeforeCloseMsg(var aMessage : TMessage);
begin
  if assigned(FOnBeforeClose) then FOnBeforeClose(self);
end;

procedure TChromiumWindow.OnAfterCreatedMsg(var aMessage : TMessage);
begin
  UpdateSize;
  if assigned(FOnAfterCreated) then FOnAfterCreated(self);
end;

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

{$IFDEF FPC}
procedure Register;
begin
  {$I res/tchromiumwindow.lrs}
  RegisterComponents('Chromium', [TChromiumWindow]);
end;
{$ENDIF}

end.
