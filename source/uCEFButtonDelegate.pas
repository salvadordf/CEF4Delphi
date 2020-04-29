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
//        Copyright © 2020 Salvador Diaz Fau. All rights reserved.
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

unit uCEFButtonDelegate;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes, System.SysUtils,
  {$ELSE}
  Classes, SysUtils,
  {$ENDIF}
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes, uCEFViewDelegate;

type
  TCefButtonDelegateRef = class(TCefViewDelegateRef, ICefButtonDelegate)
    protected
      procedure OnButtonPressed(const button: ICefButton);
      procedure OnButtonStateChanged(const button: ICefButton);

    public
      class function UnWrap(data: Pointer): ICefButtonDelegate;
  end;

implementation

uses
  uCEFLibFunctions, uCEFMiscFunctions;

procedure TCefButtonDelegateRef.OnButtonPressed(const button: ICefButton);
begin
  PCefButtonDelegate(FData)^.on_button_pressed(PCefButtonDelegate(FData),
                                               CefGetData(button));
end;

procedure TCefButtonDelegateRef.OnButtonStateChanged(const button: ICefButton);
begin
  PCefButtonDelegate(FData)^.on_button_state_changed(PCefButtonDelegate(FData),
                                                     CefGetData(button));
end;

class function TCefButtonDelegateRef.UnWrap(data: Pointer): ICefButtonDelegate;
begin
  if (data <> nil) then
    Result := Create(data) as ICefButtonDelegate
   else
    Result := nil;
end;

end.

