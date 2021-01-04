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

unit uCEFLinkedWindowParent;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
    {$IFDEF MSWINDOWS}WinApi.Windows, WinApi.Messages,{$ENDIF} System.Classes, Vcl.Controls, Vcl.Graphics,
  {$ELSE}
    {$IFDEF MSWINDOWS}Windows,{$ENDIF} Classes, Forms, Controls, Graphics,
    {$IFDEF FPC}
      LCLProc, LCLType, LCLIntf, LResources, LMessages, InterfaceBase,
      {$IFDEF LINUX}xlib, x,{$ENDIF}
    {$ELSE}
      Messages,
    {$ENDIF}
  {$ENDIF}
  uCEFWinControl, uCEFTypes, uCEFInterfaces, uCEFChromium;

type
  {$IFNDEF FPC}{$IFDEF DELPHI16_UP}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}{$ENDIF}
  TCEFLinkedWindowParent = class(TCEFWinControl)
    protected
      FChromium               : TChromium;
                                                   
      {$IFDEF FPC}{$IFDEF LINUX}
      procedure SetVisible(Value: Boolean); override;  
      {$ENDIF}{$ENDIF}
      procedure SetChromium(aValue : TChromium);

      function  GetChildWindowHandle : THandle; override;
      {$IFDEF MSWINDOWS}
      procedure WndProc(var aMessage: TMessage); override;
      {$ENDIF}
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    public
      constructor Create(AOwner : TComponent); override;
      {$IFDEF FPC}{$IFDEF LINUX}
      procedure UpdateSize; override;
      {$ENDIF}{$ENDIF}

    published
      property Chromium   : TChromium    read FChromium   write SetChromium;
  end;


{$IFDEF FPC}
procedure Register;
{$ENDIF}

implementation

uses
  uCEFMiscFunctions, uCEFClient, uCEFConstants, uCEFLibFunctions,
  uCEFApplication;

constructor TCEFLinkedWindowParent.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  FChromium := nil;
end;

function TCEFLinkedWindowParent.GetChildWindowHandle : THandle;
begin
  Result := 0;

  if (FChromium <> nil) then Result := FChromium.WindowHandle;

  if (Result = 0) then Result := inherited GetChildWindowHandle;
end;

{$IFDEF MSWINDOWS}
procedure TCEFLinkedWindowParent.WndProc(var aMessage: TMessage);
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
{$ENDIF}

procedure TCEFLinkedWindowParent.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (AComponent = FChromium) then FChromium := nil;
end;

{$IFDEF FPC}{$IFDEF LINUX}   
procedure TCEFLinkedWindowParent.SetVisible(Value: Boolean);
var
  TempChanged : boolean;
begin
  TempChanged := (Visible <> Value);

  inherited SetVisible(Value);

  if not(csDesigning in ComponentState) and
     TempChanged and
     (FChromium <> nil) and
     FChromium.Initialized then
    FChromium.UpdateXWindowVisibility(Visible);
end;

procedure TCEFLinkedWindowParent.UpdateSize;
begin
  if not(csDesigning in ComponentState) and
     (FChromium <> nil) and
     FChromium.Initialized then
    FChromium.UpdateBrowserSize(Left, Top, Width, Height);
end;
{$ENDIF}{$ENDIF}

procedure TCEFLinkedWindowParent.SetChromium(aValue : TChromium);
begin
  FChromium := aValue;
  if (aValue <> nil) then aValue.FreeNotification(Self);
end;

{$IFDEF FPC}
procedure Register;
begin
  {$I res/tceflinkedwindowparent.lrs}
  RegisterComponents('Chromium', [TCEFLinkedWindowParent]);
end;
{$ENDIF}

end.
