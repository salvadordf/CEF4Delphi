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
//        Copyright © 2017 Salvador Díaz Fau. All rights reserved.
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

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  WinApi.Windows, System.Classes, WinApi.Messages,
  {$ELSE}
  Windows, Classes, Messages,
  {$ENDIF}
  uCEFWindowParent, uCEFChromium, uCEFInterfaces, uCEFConstants;

type
  TChromiumWindow = class(TCEFWindowParent)
    protected
      FChromium       : TChromium;
      FOnClose        : TNotifyEvent;
      FOnBeforeClose  : TNotifyEvent;
      FOnAfterCreated : TNotifyEvent;

      function    GetChildWindowHandle : THandle; override;
      function    GetBrowserInitialized : boolean;

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
      procedure   LoadURL(const aURL : string);
      procedure   NotifyMoveOrResizeStarted;

      property ChromiumBrowser    : TChromium       read FChromium;
      property Initialized        : boolean         read GetBrowserInitialized;

    published
      property OnClose          : TNotifyEvent    read FOnClose          write FOnClose;
      property OnBeforeClose    : TNotifyEvent    read FOnBeforeClose    write FOnBeforeClose;
      property OnAfterCreated   : TNotifyEvent    read FOnAfterCreated   write FOnAfterCreated;
  end;

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
      FChromium.OnClose        := WebBrowser_OnClose;
      FChromium.OnBeforeClose  := WebBrowser_OnBeforeClose;
      FChromium.OnAfterCreated := WebBrowser_OnAfterCreated;
    end;
end;

function TChromiumWindow.GetChildWindowHandle : THandle;
begin
  if (FChromium <> nil) then
    Result := FChromium.WindowHandle
   else
    Result := 0;
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

procedure TChromiumWindow.LoadURL(const aURL : string);
begin
  if not(csDesigning in ComponentState) and (FChromium <> nil) then
    FChromium.LoadURL(aURL);
end;

procedure TChromiumWindow.NotifyMoveOrResizeStarted;
begin
  if (FChromium <> nil) then FChromium.NotifyMoveOrResizeStarted;
end;

end.
