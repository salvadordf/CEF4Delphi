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

unit uCEFLinkedWindowParent;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
    {$IFDEF MSWINDOWS}WinApi.Windows,{$ENDIF} System.Classes, Vcl.Controls,
  {$ELSE}
    {$IFDEF MSWINDOWS}Windows,{$ENDIF} Classes, Forms, Controls, Graphics,
    {$IFDEF FPC}
      LCLProc, LCLType, LCLIntf, LResources, LMessages, InterfaceBase,
      {$IFDEF LINUX}xlib, x,{$ENDIF}
    {$ELSE}
      Messages,
    {$ENDIF}
  {$ENDIF}
  uCEFWinControl, uCEFTypes, uCEFInterfaces, uCEFChromium,
  uCEFLinkedWinControlBase;

type
  {$IFNDEF FPC}{$IFDEF DELPHI16_UP}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}{$ENDIF}

  { TCEFLinkedWindowParent }

  TCEFLinkedWindowParent = class(TCEFLinkedWinControlBase)
    protected
      FChromium               : TChromium;

      function  GetChromium: TChromium; override;
      procedure SetChromium(aValue : TChromium);

      procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    public
      constructor Create(AOwner : TComponent); override;

    published
      property  Chromium   : TChromium    read FChromium   write SetChromium;
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

procedure TCEFLinkedWindowParent.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (AComponent = FChromium) then FChromium := nil;
end;

function TCEFLinkedWindowParent.GetChromium: TChromium;
begin
  Result := FChromium;
end;

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
