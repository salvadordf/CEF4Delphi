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

unit uCEFLinkedWinControlBase;

{$I cef.inc}

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
  {$IFDEF MACOSX}
    {$ModeSwitch objectivec1}
  {$ENDIF}
{$ENDIF}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
    {$IFDEF MSWINDOWS}WinApi.Windows, WinApi.Messages,{$ENDIF} System.Classes, Vcl.Controls, Vcl.Graphics,
  {$ELSE}
    {$IFDEF MSWINDOWS}Windows, Messages,{$ENDIF} Classes, Controls,
    {$IFDEF FPC}
    LCLProc, LCLType, LCLIntf,
    {$ENDIF}
  {$ENDIF}
  uCEFTypes, uCEFInterfaces, uCEFWinControl, uCEFChromium;

type

  { TCEFLinkedWinControlBase }

  TCEFLinkedWinControlBase = class(TCEFWinControl)
    protected
      function  GetChromium: TChromium; virtual; abstract;
      function  GetUseSetFocus: Boolean; virtual;

      {$IFDEF FPC}
      procedure SetVisible(Value: Boolean); override;
      {$ENDIF}
      function  GetChildWindowHandle : {$IFNDEF MSWINDOWS}{$IFDEF FPC}LclType.{$ENDIF}{$ENDIF}THandle; override;
      {$IFDEF MSWINDOWS}
      procedure WndProc(var aMessage: TMessage); override;
      {$ENDIF}

      property  Chromium   : TChromium    read GetChromium;
    public
      procedure UpdateSize; override;
  end;

implementation

{ TCEFLinkedWinControlBase }

function TCEFLinkedWinControlBase.GetUseSetFocus: Boolean;
begin
  Result := True;
end;

{$IFDEF FPC}
procedure TCEFLinkedWinControlBase.SetVisible(Value: Boolean);
{$IFDEF LINUX}
var
  TempChanged : boolean;
{$ENDIF}
begin
  {$IFDEF LINUX}
  TempChanged := (Visible <> Value);
  {$ENDIF}

  inherited SetVisible(Value);

  {$IFDEF LINUX}
  if not(csDesigning in ComponentState) and
     TempChanged and
     (Chromium <> nil) and
     Chromium.Initialized then
    Chromium.UpdateXWindowVisibility(Visible);
  {$ENDIF}
end;
{$ENDIF}

function TCEFLinkedWinControlBase.GetChildWindowHandle: THandle;
begin
  Result := 0;

  if (Chromium <> nil) then Result := Chromium.WindowHandle;

  if (Result = 0) then Result := inherited GetChildWindowHandle;
end;

{$IFDEF MSWINDOWS}
procedure TCEFLinkedWinControlBase.WndProc(var aMessage: TMessage);
var
  TempHandle : THandle;
begin
  case aMessage.Msg of
    WM_SETFOCUS:
      begin
        if GetUseSetFocus and (Chromium <> nil) then
          Chromium.SetFocus(True)
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

procedure TCEFLinkedWinControlBase.UpdateSize;
begin
  {$IFDEF LINUX}
  if not(csDesigning in ComponentState) and
     (Chromium <> nil) and
     Chromium.Initialized then
    Chromium.UpdateBrowserSize(Left, Top, Width, Height);
  {$ELSE}
  inherited UpdateSize;
  {$ENDIF}
end;


end.

