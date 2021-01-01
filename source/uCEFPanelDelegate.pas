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

unit uCEFPanelDelegate;

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
  TCefPanelDelegateRef = class(TCefViewDelegateRef, ICefPanelDelegate)
    public
      class function UnWrap(data: Pointer): ICefPanelDelegate;
  end;

  TCefPanelDelegateOwn = class(TCefViewDelegateOwn, ICefPanelDelegate)
    public
      constructor Create; override;
  end;

  TCustomPanelDelegate = class(TCefPanelDelegateOwn)
    protected
      FEvents : Pointer;

      // ICefViewDelegate
      procedure OnGetPreferredSize(const view: ICefView; var aResult : TCefSize); override;
      procedure OnGetMinimumSize(const view: ICefView; var aResult : TCefSize); override;
      procedure OnGetMaximumSize(const view: ICefView; var aResult : TCefSize); override;
      procedure OnGetHeightForWidth(const view: ICefView; width: Integer; var aResult: Integer); override;
      procedure OnParentViewChanged(const view: ICefView; added: boolean; const parent: ICefView); override;
      procedure OnChildViewChanged(const view: ICefView; added: boolean; const child: ICefView); override;
      procedure OnFocus(const view: ICefView); override;
      procedure OnBlur(const view: ICefView); override;

    public
      constructor Create(const events: ICefPanelDelegateEvents); reintroduce;
  end;

implementation

uses
  uCEFLibFunctions, uCEFMiscFunctions;

// **************************************************************
// ******************** TCefPanelDelegateRef ********************
// **************************************************************

class function TCefPanelDelegateRef.UnWrap(data: Pointer): ICefPanelDelegate;
begin
  if (data <> nil) then
    Result := Create(data) as ICefPanelDelegate
   else
    Result := nil;
end;

// **************************************************************
// ******************** TCefPanelDelegateOwn ********************
// **************************************************************

constructor TCefPanelDelegateOwn.Create;
begin
  inherited CreateData(SizeOf(TCefPanelDelegate));

  InitializeCEFMethods;
end;

// **************************************************************
// ******************** TCustomPanelDelegate ********************
// **************************************************************

constructor TCustomPanelDelegate.Create(const events: ICefPanelDelegateEvents);
begin
  inherited Create;

  FEvents := Pointer(events);
end;

procedure TCustomPanelDelegate.OnGetPreferredSize(const view: ICefView; var aResult : TCefSize);
begin
  try
    if (FEvents <> nil) then
      ICefPanelDelegateEvents(FEvents).doOnGetPreferredSize(view, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomPanelDelegate.OnGetPreferredSize', e) then raise;
  end;
end;

procedure TCustomPanelDelegate.OnGetMinimumSize(const view: ICefView; var aResult : TCefSize);
begin
  try
    if (FEvents <> nil) then
      ICefPanelDelegateEvents(FEvents).doOnGetMinimumSize(view, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomPanelDelegate.OnGetMinimumSize', e) then raise;
  end;
end;

procedure TCustomPanelDelegate.OnGetMaximumSize(const view: ICefView; var aResult : TCefSize);
begin
  try
    if (FEvents <> nil) then
      ICefPanelDelegateEvents(FEvents).doOnGetMaximumSize(view, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomPanelDelegate.OnGetMaximumSize', e) then raise;
  end;
end;

procedure TCustomPanelDelegate.OnGetHeightForWidth(const view: ICefView; width: Integer; var aResult: Integer);
begin
  try
    if (FEvents <> nil) then
      ICefPanelDelegateEvents(FEvents).doOnGetHeightForWidth(view, width, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomPanelDelegate.OnGetHeightForWidth', e) then raise;
  end;
end;

procedure TCustomPanelDelegate.OnParentViewChanged(const view: ICefView; added: boolean; const parent: ICefView);
begin
  try
    if (FEvents <> nil) then
      ICefPanelDelegateEvents(FEvents).doOnParentViewChanged(view, added, parent);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomPanelDelegate.OnParentViewChanged', e) then raise;
  end;
end;

procedure TCustomPanelDelegate.OnChildViewChanged(const view: ICefView; added: boolean; const child: ICefView);
begin
  try
    if (FEvents <> nil) then
      ICefPanelDelegateEvents(FEvents).doOnChildViewChanged(view, added, child);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomPanelDelegate.OnChildViewChanged', e) then raise;
  end;
end;

procedure TCustomPanelDelegate.OnFocus(const view: ICefView);
begin
  try
    if (FEvents <> nil) then
      ICefPanelDelegateEvents(FEvents).doOnFocus(view);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomPanelDelegate.OnFocus', e) then raise;
  end;
end;

procedure TCustomPanelDelegate.OnBlur(const view: ICefView);
begin
  try
    if (FEvents <> nil) then
      ICefPanelDelegateEvents(FEvents).doOnBlur(view);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomPanelDelegate.OnBlur', e) then raise;
  end;
end;

end.

