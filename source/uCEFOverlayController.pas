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

unit uCEFOverlayController;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes, System.SysUtils,
  {$ELSE}
  Classes, SysUtils,
  {$ENDIF}
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefOverlayControllerRef = class(TCefBaseRefCountedRef, ICefOverlayController)
    public
      function  IsValid: boolean;
      function  IsSame(const that: ICefOverlayController): boolean;
      function  GetContentsView: ICefView;
      function  GetWindow: ICefWindow;
      function  GetDockingMode: TCefDockingMode;
      procedure DestroyOverlay;
      procedure SetBounds(const bounds: TCefRect);
      function  GetBounds: TCefRect;
      function  GetBoundsInScreen: TCefRect;
      procedure SetSize(const size: TCefSize);
      function  GetSize: TCefSize;
      procedure SetPosition(const position: TCefPoint);
      function  GetPosition: TCefPoint;
      procedure SetInsets(const insets: TCefInsets);
      function  GetInsets: TCefInsets;
      procedure SizeToPreferredSize;
      procedure SetVisible(visible: boolean);
      function  IsVisible: boolean;
      function  IsDrawn: boolean;

      class function UnWrap(data: Pointer): ICefOverlayController;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFView, uCEFWindow;

function TCefOverlayControllerRef.IsValid: boolean;
begin
  Result := PCefOverlayController(FData)^.is_valid(PCefOverlayController(FData)) <> 0;
end;

function TCefOverlayControllerRef.IsSame(const that: ICefOverlayController): boolean;
begin
  Result := PCefOverlayController(FData)^.is_same(PCefOverlayController(FData), CefGetData(that)) <> 0;
end;

function TCefOverlayControllerRef.GetContentsView: ICefView;
begin
  Result := TCefViewRef.UnWrap(PCefOverlayController(FData)^.get_contents_view(PCefOverlayController(FData)));
end;

function TCefOverlayControllerRef.GetWindow: ICefWindow;
begin
  Result := TCefWindowRef.UnWrap(PCefOverlayController(FData)^.get_window(PCefOverlayController(FData)));
end;

function TCefOverlayControllerRef.GetDockingMode: TCefDockingMode;
begin
  Result := PCefOverlayController(FData)^.get_docking_mode(PCefOverlayController(FData));
end;

procedure TCefOverlayControllerRef.DestroyOverlay;
begin
  PCefOverlayController(FData)^.destroy(PCefOverlayController(FData));
end;

procedure TCefOverlayControllerRef.SetBounds(const bounds: TCefRect);
begin
  PCefOverlayController(FData)^.set_bounds(PCefOverlayController(FData), @bounds);
end;

function TCefOverlayControllerRef.GetBounds: TCefRect;
begin
  Result := PCefOverlayController(FData)^.get_bounds(PCefOverlayController(FData));
end;

function TCefOverlayControllerRef.GetBoundsInScreen: TCefRect;
begin
  Result := PCefOverlayController(FData)^.get_bounds_in_screen(PCefOverlayController(FData));
end;

procedure TCefOverlayControllerRef.SetSize(const size: TCefSize);
begin
  PCefOverlayController(FData)^.set_size(PCefOverlayController(FData), @size);
end;

function TCefOverlayControllerRef.GetSize: TCefSize;
begin
  Result := PCefOverlayController(FData)^.get_size(PCefOverlayController(FData));
end;

procedure TCefOverlayControllerRef.SetPosition(const position: TCefPoint);
begin
  PCefOverlayController(FData)^.set_position(PCefOverlayController(FData), @position);
end;

function TCefOverlayControllerRef.GetPosition: TCefPoint;
begin
  Result := PCefOverlayController(FData)^.get_position(PCefOverlayController(FData));
end;

procedure TCefOverlayControllerRef.SetInsets(const insets: TCefInsets);
begin
  PCefOverlayController(FData)^.set_insets(PCefOverlayController(FData), @insets);
end;

function TCefOverlayControllerRef.GetInsets: TCefInsets;
begin
  Result := PCefOverlayController(FData)^.get_insets(PCefOverlayController(FData));
end;

procedure TCefOverlayControllerRef.SizeToPreferredSize;
begin
  PCefOverlayController(FData)^.size_to_preferred_size(PCefOverlayController(FData));
end;

procedure TCefOverlayControllerRef.SetVisible(visible: boolean);
begin
  PCefOverlayController(FData)^.set_visible(PCefOverlayController(FData), ord(visible));
end;

function TCefOverlayControllerRef.IsVisible: boolean;
begin
  Result := PCefOverlayController(FData)^.is_visible(PCefOverlayController(FData)) <> 0;
end;

function TCefOverlayControllerRef.IsDrawn: boolean;
begin
  Result := PCefOverlayController(FData)^.is_drawn(PCefOverlayController(FData)) <> 0;
end;

class function TCefOverlayControllerRef.UnWrap(data: Pointer): ICefOverlayController;
begin
  if (data <> nil) then
    Result := Create(data) as ICefOverlayController
   else
    Result := nil;
end;

end.
