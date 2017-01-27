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

interface

uses
  WinApi.Windows, System.Classes, WinApi.Messages,
  uCEFWindowParent, uCEFChromium, uCEFInterfaces, uCEFConstants;

type
  TChromiumWindow = class(TCEFWindowParent)
    protected
      FChromium : TChromium;
      FOnClose  : TNotifyEvent;

      function    GetChildWindowHandle : THandle; override;

      procedure   OnCloseMsg(var aMessage : TMessage); message CEF_DOONCLOSE;

      procedure   WebBrowser_OnClose(Sender: TObject; const browser: ICefBrowser; out Result: Boolean);

   public
      constructor Create(AOwner: TComponent); override;
      procedure   AfterConstruction; override;
      procedure   CreateBrowser;
      procedure   CloseBrowser(aForceClose : boolean);
      procedure   LoadURL(const aURL : string);

      property ChromiumBrowser  : TChromium       read FChromium;

    published
      property OnClose          : TNotifyEvent    read FOnClose     write FOnClose;
  end;

implementation

uses
  System.SysUtils;

constructor TChromiumWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FChromium := nil;
  FOnClose  := nil;
end;

procedure TChromiumWindow.AfterConstruction;
begin
  inherited AfterConstruction;

  if not(csDesigning in ComponentState) then
    begin
      FChromium         := TChromium.Create(self);
      FChromium.OnClose := WebBrowser_OnClose;
    end;
end;

function TChromiumWindow.GetChildWindowHandle : THandle;
begin
  if (FChromium <> nil) then
    Result := FChromium.WindowHandle
   else
    Result := 0;
end;

procedure TChromiumWindow.WebBrowser_OnClose(Sender: TObject; const browser: ICefBrowser; out Result: Boolean);
begin
  PostMessage(self.Handle, CEF_DOONCLOSE, 0, 0);
  Result := True;
end;

procedure TChromiumWindow.OnCloseMsg(var aMessage : TMessage);
begin
  if assigned(FOnClose) then FOnClose(self);
end;

procedure TChromiumWindow.CreateBrowser;
begin
  if not(csDesigning in ComponentState) and (FChromium <> nil) then
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

end.
