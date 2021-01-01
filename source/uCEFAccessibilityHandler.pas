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

unit uCEFAccessibilityHandler;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes, uCEFChromiumEvents;

type
  TOnAccessibilityEvent = procedure(Sender: TObject; const value: ICefValue) of object;

  TCEFAccessibilityHandlerOwn = class(TCefBaseRefCountedOwn, ICefAccessibilityHandler)
    protected
      procedure OnAccessibilityTreeChange(const value: ICefValue); virtual;
      procedure OnAccessibilityLocationChange(const value: ICefValue); virtual;

    public
      constructor Create; virtual;
  end;

  TCustomAccessibilityHandler = class(TCEFAccessibilityHandlerOwn)
    protected
      FOnTreeChange     : TOnAccessibilityEvent;
      FOnLocationChange : TOnAccessibilityEvent;

      procedure OnAccessibilityTreeChange(const value: ICefValue); override;
      procedure OnAccessibilityLocationChange(const value: ICefValue); override;

    public
      constructor Create; override;

      property OnTreeChange       : TOnAccessibilityEvent   read FOnTreeChange       write FOnTreeChange;
      property OnLocationChange   : TOnAccessibilityEvent   read FOnLocationChange   write FOnLocationChange;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFValue;

procedure cef_accessibility_handler_on_accessibility_tree_change(self: PCefAccessibilityHandler; value: PCefValue); stdcall;
var
  TempObject  : TObject;
begin
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCEFAccessibilityHandlerOwn) then
    TCEFAccessibilityHandlerOwn(TempObject).OnAccessibilityTreeChange(TCefValueRef.UnWrap(value));
end;

procedure cef_accessibility_handler_on_accessibility_location_change(self: PCefAccessibilityHandler; value: PCefValue); stdcall;
var
  TempObject  : TObject;
begin
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCEFAccessibilityHandlerOwn) then
    TCEFAccessibilityHandlerOwn(TempObject).OnAccessibilityLocationChange(TCefValueRef.UnWrap(value));
end;

constructor TCEFAccessibilityHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCEFAccessibilityHandler));

  with PCEFAccessibilityHandler(FData)^ do
    begin
      on_accessibility_tree_change     := {$IFDEF FPC}@{$ENDIF}cef_accessibility_handler_on_accessibility_tree_change;
      on_accessibility_location_change := {$IFDEF FPC}@{$ENDIF}cef_accessibility_handler_on_accessibility_location_change;
    end;
end;

procedure TCEFAccessibilityHandlerOwn.OnAccessibilityTreeChange(const value: ICefValue);
begin
  //
end;

procedure TCEFAccessibilityHandlerOwn.OnAccessibilityLocationChange(const value: ICefValue);
begin
  //
end;


// *****************************************
// ****** TCustomAccessibilityHandler ******
// *****************************************

constructor TCustomAccessibilityHandler.Create;
begin
  inherited Create;

  FOnTreeChange     := nil;
  FOnLocationChange := nil;
end;

procedure TCustomAccessibilityHandler.OnAccessibilityTreeChange(const value: ICefValue);
begin
  if assigned(FOnTreeChange) then FOnTreeChange(self, value);
end;

procedure TCustomAccessibilityHandler.OnAccessibilityLocationChange(const value: ICefValue);
begin
  if assigned(FOnLocationChange) then FOnLocationChange(self, value);
end;

end.
