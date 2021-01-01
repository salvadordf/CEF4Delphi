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

  TCefButtonDelegateOwn = class(TCefViewDelegateOwn, ICefButtonDelegate)
    protected
      procedure OnButtonPressed(const button: ICefButton); virtual;
      procedure OnButtonStateChanged(const button: ICefButton); virtual;

      procedure InitializeCEFMethods; override;
    public
      constructor Create; override;
  end;

  TCustomButtonDelegate = class(TCefButtonDelegateOwn)
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

      // ICefButtonDelegate
      procedure OnButtonPressed(const button: ICefButton); override;
      procedure OnButtonStateChanged(const button: ICefButton); override;

    public
      constructor Create(const events: ICefButtonDelegateEvents); reintroduce;
  end;

implementation

uses
  uCEFLibFunctions, uCEFMiscFunctions, uCEFButton;


// **************************************************************
// ****************** TCefButtonDelegateRef *********************
// **************************************************************

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

// **************************************************************
// ****************** TCefButtonDelegateOwn *********************
// **************************************************************

procedure cef_button_delegate_on_button_pressed(self: PCefButtonDelegate; button: PCefButton); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefButtonDelegateOwn) then
    TCefButtonDelegateOwn(TempObject).OnButtonPressed(TCefButtonRef.UnWrap(button));
end;

procedure cef_button_delegate_on_button_state_changed(self: PCefButtonDelegate; button: PCefButton); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefButtonDelegateOwn) then
    TCefButtonDelegateOwn(TempObject).OnButtonStateChanged(TCefButtonRef.UnWrap(button));
end;


constructor TCefButtonDelegateOwn.Create;
begin
  inherited CreateData(SizeOf(TCefButtonDelegate));

  InitializeCEFMethods;
end;

procedure TCefButtonDelegateOwn.InitializeCEFMethods;
begin
  inherited InitializeCEFMethods;

  with PCefButtonDelegate(FData)^ do
    begin
      on_button_pressed       := {$IFDEF FPC}@{$ENDIF}cef_button_delegate_on_button_pressed;
      on_button_state_changed := {$IFDEF FPC}@{$ENDIF}cef_button_delegate_on_button_state_changed;
    end;
end;

procedure TCefButtonDelegateOwn.OnButtonPressed(const button: ICefButton);
begin
  //
end;

procedure TCefButtonDelegateOwn.OnButtonStateChanged(const button: ICefButton);
begin
  //
end;

// **************************************************************
// ****************** TCustomButtonDelegate *********************
// **************************************************************

constructor TCustomButtonDelegate.Create(const events: ICefButtonDelegateEvents);
begin
  inherited Create;

  FEvents := Pointer(events);
end;

procedure TCustomButtonDelegate.OnGetPreferredSize(const view: ICefView; var aResult : TCefSize);
begin
  try
    if (FEvents <> nil) then
      ICefButtonDelegateEvents(FEvents).doOnGetPreferredSize(view, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomButtonDelegate.OnGetPreferredSize', e) then raise;
  end;
end;

procedure TCustomButtonDelegate.OnGetMinimumSize(const view: ICefView; var aResult : TCefSize);
begin
  try
    if (FEvents <> nil) then
      ICefButtonDelegateEvents(FEvents).doOnGetMinimumSize(view, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomButtonDelegate.OnGetMinimumSize', e) then raise;
  end;
end;

procedure TCustomButtonDelegate.OnGetMaximumSize(const view: ICefView; var aResult : TCefSize);
begin
  try
    if (FEvents <> nil) then
      ICefButtonDelegateEvents(FEvents).doOnGetMaximumSize(view, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomButtonDelegate.OnGetMaximumSize', e) then raise;
  end;
end;

procedure TCustomButtonDelegate.OnGetHeightForWidth(const view: ICefView; width: Integer; var aResult: Integer);
begin
  try
    if (FEvents <> nil) then
      ICefButtonDelegateEvents(FEvents).doOnGetHeightForWidth(view, width, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomButtonDelegate.OnGetHeightForWidth', e) then raise;
  end;
end;

procedure TCustomButtonDelegate.OnParentViewChanged(const view: ICefView; added: boolean; const parent: ICefView);
begin
  try
    if (FEvents <> nil) then
      ICefButtonDelegateEvents(FEvents).doOnParentViewChanged(view, added, parent);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomButtonDelegate.OnParentViewChanged', e) then raise;
  end;
end;

procedure TCustomButtonDelegate.OnChildViewChanged(const view: ICefView; added: boolean; const child: ICefView);
begin
  try
    if (FEvents <> nil) then
      ICefButtonDelegateEvents(FEvents).doOnChildViewChanged(view, added, child);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomButtonDelegate.OnChildViewChanged', e) then raise;
  end;
end;

procedure TCustomButtonDelegate.OnFocus(const view: ICefView);
begin
  try
    if (FEvents <> nil) then
      ICefButtonDelegateEvents(FEvents).doOnFocus(view);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomButtonDelegate.OnFocus', e) then raise;
  end;
end;

procedure TCustomButtonDelegate.OnBlur(const view: ICefView);
begin
  try
    if (FEvents <> nil) then
      ICefButtonDelegateEvents(FEvents).doOnBlur(view);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomButtonDelegate.OnBlur', e) then raise;
  end;
end;

procedure TCustomButtonDelegate.OnButtonPressed(const button: ICefButton);
begin
  try
    if (FEvents <> nil) then
      ICefButtonDelegateEvents(FEvents).doOnButtonPressed(button);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomButtonDelegate.OnButtonPressed', e) then raise;
  end;
end;

procedure TCustomButtonDelegate.OnButtonStateChanged(const button: ICefButton);
begin
  try
    if (FEvents <> nil) then
      ICefButtonDelegateEvents(FEvents).doOnButtonStateChanged(button);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomButtonDelegate.OnButtonStateChanged', e) then raise;
  end;
end;

end.

