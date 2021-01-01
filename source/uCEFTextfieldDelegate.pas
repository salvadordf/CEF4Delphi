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

unit uCEFTextfieldDelegate;

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
  TCefTextfieldDelegateRef = class(TCefViewDelegateRef, ICefTextfieldDelegate)
    protected
      procedure OnKeyEvent(const textfield: ICefTextfield; const event: TCefKeyEvent; var aResult : boolean);
      procedure OnAfterUserAction(const textfield: ICefTextfield);

    public
      class function UnWrap(data: Pointer): ICefTextfieldDelegate;
  end;

  TCefTextfieldDelegateOwn = class(TCefViewDelegateOwn, ICefTextfieldDelegate)
    protected
      procedure OnKeyEvent(const textfield: ICefTextfield; const event: TCefKeyEvent; var aResult : boolean); virtual;
      procedure OnAfterUserAction(const textfield: ICefTextfield); virtual;

      procedure InitializeCEFMethods; override;

    public
      constructor Create; override;
  end;

  TCustomTextfieldDelegate = class(TCefTextfieldDelegateOwn)
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

      // ICefTextfieldDelegate
      procedure OnKeyEvent(const textfield: ICefTextfield; const event: TCefKeyEvent; var aResult : boolean); override;
      procedure OnAfterUserAction(const textfield: ICefTextfield); override;

    public
      constructor Create(const events: ICefTextfieldDelegateEvents); reintroduce;
  end;

implementation

uses
  uCEFLibFunctions, uCEFMiscFunctions, uCEFTextfield;


// **************************************************************
// ***************** TCefTextfieldDelegateRef *******************
// **************************************************************

procedure TCefTextfieldDelegateRef.OnKeyEvent(const textfield : ICefTextfield;
                                              const event     : TCefKeyEvent;
                                              var   aResult   : boolean);
begin
  aResult := (PCefTextfieldDelegate(FData)^.on_key_event(PCefTextfieldDelegate(FData),
                                                         CefGetData(textfield),
                                                         @event) <> 0);
end;

procedure TCefTextfieldDelegateRef.OnAfterUserAction(const textfield: ICefTextfield);
begin
  PCefTextfieldDelegate(FData)^.on_after_user_action(PCefTextfieldDelegate(FData),
                                                     CefGetData(textfield));
end;

class function TCefTextfieldDelegateRef.UnWrap(data: Pointer): ICefTextfieldDelegate;
begin
  if (data <> nil) then
    Result := Create(data) as ICefTextfieldDelegate
   else
    Result := nil;
end;


// **************************************************************
// ***************** TCefTextfieldDelegateOwn *******************
// **************************************************************

function cef_textfield_delegate_on_key_event(self: PCefTextfieldDelegate; textfield: PCefTextfield; const event: PCefKeyEvent): Integer; stdcall;
var
  TempObject : TObject;
  TempResult : boolean;
begin
  TempObject := CefGetObject(self);
  TempResult := False;

  if (TempObject <> nil) and (TempObject is TCefTextfieldDelegateOwn) then
    TCefTextfieldDelegateOwn(TempObject).OnKeyEvent(TCefTextfieldRef.UnWrap(textfield), event^, TempResult);

  Result := ord(TempResult);
end;

procedure cef_textfield_delegate_on_accelerator(self: PCefTextfieldDelegate; textfield: PCefTextfield); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefTextfieldDelegateOwn) then
    TCefTextfieldDelegateOwn(TempObject).OnAfterUserAction(TCefTextfieldRef.UnWrap(textfield));
end;

constructor TCefTextfieldDelegateOwn.Create;
begin
  inherited CreateData(SizeOf(TCefTextfieldDelegate));

  InitializeCEFMethods;
end;

procedure TCefTextfieldDelegateOwn.InitializeCEFMethods;
begin
  inherited InitializeCEFMethods;

  with PCefTextfieldDelegate(FData)^ do
    begin
      on_after_user_action := {$IFDEF FPC}@{$ENDIF}cef_textfield_delegate_on_accelerator;
      on_key_event         := {$IFDEF FPC}@{$ENDIF}cef_textfield_delegate_on_key_event;
    end;
end;

procedure TCefTextfieldDelegateOwn.OnKeyEvent(const textfield: ICefTextfield; const event: TCefKeyEvent; var aResult : boolean);
begin
  //
end;

procedure TCefTextfieldDelegateOwn.OnAfterUserAction(const textfield: ICefTextfield);
begin
  //
end;


// **************************************************************
// ***************** TCustomTextfieldDelegate *******************
// **************************************************************

constructor TCustomTextfieldDelegate.Create(const events: ICefTextfieldDelegateEvents);
begin
  inherited Create;

  FEvents := Pointer(events);
end;

procedure TCustomTextfieldDelegate.OnGetPreferredSize(const view: ICefView; var aResult : TCefSize);
begin
  try
    if (FEvents <> nil) then
      ICefTextfieldDelegateEvents(FEvents).doOnGetPreferredSize(view, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomTextfieldDelegate.OnGetPreferredSize', e) then raise;
  end;
end;

procedure TCustomTextfieldDelegate.OnGetMinimumSize(const view: ICefView; var aResult : TCefSize);
begin
  try
    if (FEvents <> nil) then
      ICefTextfieldDelegateEvents(FEvents).doOnGetMinimumSize(view, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomTextfieldDelegate.OnGetMinimumSize', e) then raise;
  end;
end;

procedure TCustomTextfieldDelegate.OnGetMaximumSize(const view: ICefView; var aResult : TCefSize);
begin
  try
    if (FEvents <> nil) then
      ICefTextfieldDelegateEvents(FEvents).doOnGetMaximumSize(view, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomTextfieldDelegate.OnGetMaximumSize', e) then raise;
  end;
end;

procedure TCustomTextfieldDelegate.OnGetHeightForWidth(const view: ICefView; width: Integer; var aResult: Integer);
begin
  try
    if (FEvents <> nil) then
      ICefTextfieldDelegateEvents(FEvents).doOnGetHeightForWidth(view, width, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomTextfieldDelegate.OnGetHeightForWidth', e) then raise;
  end;
end;

procedure TCustomTextfieldDelegate.OnParentViewChanged(const view: ICefView; added: boolean; const parent: ICefView);
begin
  try
    if (FEvents <> nil) then
      ICefTextfieldDelegateEvents(FEvents).doOnParentViewChanged(view, added, parent);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomTextfieldDelegate.OnParentViewChanged', e) then raise;
  end;
end;

procedure TCustomTextfieldDelegate.OnChildViewChanged(const view: ICefView; added: boolean; const child: ICefView);
begin
  try
    if (FEvents <> nil) then
      ICefTextfieldDelegateEvents(FEvents).doOnChildViewChanged(view, added, child);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomTextfieldDelegate.OnChildViewChanged', e) then raise;
  end;
end;

procedure TCustomTextfieldDelegate.OnFocus(const view: ICefView);
begin
  try
    if (FEvents <> nil) then
      ICefTextfieldDelegateEvents(FEvents).doOnFocus(view);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomTextfieldDelegate.OnFocus', e) then raise;
  end;
end;

procedure TCustomTextfieldDelegate.OnBlur(const view: ICefView);
begin
  try
    if (FEvents <> nil) then
      ICefTextfieldDelegateEvents(FEvents).doOnBlur(view);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomTextfieldDelegate.OnBlur', e) then raise;
  end;
end;

procedure TCustomTextfieldDelegate.OnKeyEvent(const textfield: ICefTextfield; const event: TCefKeyEvent; var aResult : boolean);
begin
  try
    if (FEvents <> nil) then
      ICefTextfieldDelegateEvents(FEvents).doOnKeyEvent(textfield, event, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomTextfieldDelegate.OnKeyEvent', e) then raise;
  end;
end;

procedure TCustomTextfieldDelegate.OnAfterUserAction(const textfield: ICefTextfield);
begin
  try
    if (FEvents <> nil) then
      ICefTextfieldDelegateEvents(FEvents).doOnAfterUserAction(textfield);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomTextfieldDelegate.OnAfterUserAction', e) then raise;
  end;
end;

end.
