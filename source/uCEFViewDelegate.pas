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

unit uCEFViewDelegate;

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
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefViewDelegateRef = class(TCefBaseRefCountedRef, ICefViewDelegate)
    protected
      procedure OnGetPreferredSize(const view: ICefView; var aResult : TCefSize);
      procedure OnGetMinimumSize(const view: ICefView; var aResult : TCefSize);
      procedure OnGetMaximumSize(const view: ICefView; var aResult : TCefSize);
      procedure OnGetHeightForWidth(const view: ICefView; width: Integer; var aResult: Integer);
      procedure OnParentViewChanged(const view: ICefView; added: boolean; const parent: ICefView);
      procedure OnChildViewChanged(const view: ICefView; added: boolean; const child: ICefView);
      procedure OnFocus(const view: ICefView);
      procedure OnBlur(const view: ICefView);

    public
      class function UnWrap(data: Pointer): ICefViewDelegate;
  end;

  TCefViewDelegateOwn = class(TCefBaseRefCountedOwn, ICefViewDelegate)
    protected
      procedure OnGetPreferredSize(const view: ICefView; var aResult : TCefSize); virtual;
      procedure OnGetMinimumSize(const view: ICefView; var aResult : TCefSize); virtual;
      procedure OnGetMaximumSize(const view: ICefView; var aResult : TCefSize); virtual;
      procedure OnGetHeightForWidth(const view: ICefView; width: Integer; var aResult: Integer); virtual;
      procedure OnParentViewChanged(const view: ICefView; added: boolean; const parent: ICefView); virtual;
      procedure OnChildViewChanged(const view: ICefView; added: boolean; const child: ICefView); virtual;
      procedure OnFocus(const view: ICefView); virtual;
      procedure OnBlur(const view: ICefView); virtual;

      procedure InitializeCEFMethods; virtual;
    public
      constructor Create; virtual;
  end;

  TCustomViewDelegate = class(TCefViewDelegateOwn)
    protected
      FEvents : Pointer;

      procedure OnGetPreferredSize(const view: ICefView; var aResult : TCefSize); override;
      procedure OnGetMinimumSize(const view: ICefView; var aResult : TCefSize); override;
      procedure OnGetMaximumSize(const view: ICefView; var aResult : TCefSize); override;
      procedure OnGetHeightForWidth(const view: ICefView; width: Integer; var aResult: Integer); override;
      procedure OnParentViewChanged(const view: ICefView; added: boolean; const parent: ICefView); override;
      procedure OnChildViewChanged(const view: ICefView; added: boolean; const child: ICefView); override;
      procedure OnFocus(const view: ICefView); override;
      procedure OnBlur(const view: ICefView); override;

    public
      constructor Create(const events: ICefViewDelegateEvents); reintroduce;
      destructor  Destroy; override;
  end;

implementation

uses
  uCEFLibFunctions, uCEFMiscFunctions, uCEFView;


// **************************************************************
// ******************** TCefViewDelegateRef *********************
// **************************************************************

procedure TCefViewDelegateRef.OnGetPreferredSize(const view: ICefView; var aResult : TCefSize);
begin
  aResult := PCefViewDelegate(FData)^.get_preferred_size(PCefViewDelegate(FData),
                                                           CefGetData(view));
end;

procedure TCefViewDelegateRef.OnGetMinimumSize(const view: ICefView; var aResult : TCefSize);
begin
  aResult := PCefViewDelegate(FData)^.get_minimum_size(PCefViewDelegate(FData),
                                                       CefGetData(view));
end;

procedure TCefViewDelegateRef.OnGetMaximumSize(const view: ICefView; var aResult : TCefSize);
begin
  aResult := PCefViewDelegate(FData)^.get_maximum_size(PCefViewDelegate(FData),
                                                       CefGetData(view));
end;

procedure TCefViewDelegateRef.OnGetHeightForWidth(const view: ICefView; width: Integer; var aResult: Integer);
begin
  aResult := PCefViewDelegate(FData)^.get_height_for_width(PCefViewDelegate(FData),
                                                           CefGetData(view),
                                                           width);
end;

procedure TCefViewDelegateRef.OnParentViewChanged(const view: ICefView; added: boolean; const parent: ICefView);
begin
  PCefViewDelegate(FData)^.on_parent_view_changed(PCefViewDelegate(FData),
                                                  CefGetData(view),
                                                  ord(added),
                                                  CefGetData(parent));
end;

procedure TCefViewDelegateRef.OnChildViewChanged(const view: ICefView; added: boolean; const child: ICefView);
begin
  PCefViewDelegate(FData)^.on_child_view_changed(PCefViewDelegate(FData),
                                                 CefGetData(view),
                                                 ord(added),
                                                 CefGetData(child));
end;

procedure TCefViewDelegateRef.OnFocus(const view: ICefView);
begin
  PCefViewDelegate(FData)^.on_focus(PCefViewDelegate(FData),
                                    CefGetData(view));
end;

procedure TCefViewDelegateRef.OnBlur(const view: ICefView);
begin
  PCefViewDelegate(FData)^.on_blur(PCefViewDelegate(FData),
                                   CefGetData(view));
end;

class function TCefViewDelegateRef.UnWrap(data: Pointer): ICefViewDelegate;
begin
  if (data <> nil) then
    Result := Create(data) as ICefViewDelegate
   else
    Result := nil;
end;



// **************************************************************
// ******************** TCefViewDelegateOwn *********************
// **************************************************************

function cef_view_delegate_get_preferred_size(self: PCefViewDelegate; view: PCefView): TCefSize; stdcall;
var
  TempObject : TObject;
  TempSize   : TCefSize;
begin
  TempObject      := CefGetObject(self);
  TempSize.width  := 100;
  TempSize.height := 100;

  if (TempObject <> nil) and (TempObject is TCefViewDelegateOwn) then
    TCefViewDelegateOwn(TempObject).OnGetPreferredSize(TCefViewRef.UnWrap(view),
                                                       TempSize);

  Result := TempSize;
end;

function cef_view_delegate_get_minimum_size(self: PCefViewDelegate; view: PCefView): TCefSize; stdcall;
var
  TempObject : TObject;
  TempSize   : TCefSize;
begin
  TempObject      := CefGetObject(self);
  TempSize.width  := 0;
  TempSize.height := 0;

  if (TempObject <> nil) and (TempObject is TCefViewDelegateOwn) then
    TCefViewDelegateOwn(TempObject).OnGetMinimumSize(TCefViewRef.UnWrap(view),
                                                     TempSize);

  Result := TempSize;
end;

function cef_view_delegate_get_maximum_size(self: PCefViewDelegate; view: PCefView): TCefSize; stdcall;
var
  TempObject : TObject;
  TempSize   : TCefSize;
begin
  TempObject      := CefGetObject(self);
  TempSize.width  := 0;
  TempSize.height := 0;

  if (TempObject <> nil) and (TempObject is TCefViewDelegateOwn) then
    TCefViewDelegateOwn(TempObject).OnGetMaximumSize(TCefViewRef.UnWrap(view),
                                                     TempSize);

  Result := TempSize;
end;

function cef_view_delegate_get_height_for_width(self: PCefViewDelegate; view: PCefView; width: Integer): Integer; stdcall;
var
  TempObject : TObject;
  TempHeight : integer;
begin
  TempObject := CefGetObject(self);
  TempHeight := 0;

  if (TempObject <> nil) and (TempObject is TCefViewDelegateOwn) then
    TCefViewDelegateOwn(TempObject).OnGetHeightForWidth(TCefViewRef.UnWrap(view),
                                                        width,
                                                        TempHeight);

  Result := TempHeight;
end;

procedure cef_view_delegate_on_parent_view_changed(self: PCefViewDelegate; view: PCefView; added: Integer; parent: PCefView); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefViewDelegateOwn) then
    TCefViewDelegateOwn(TempObject).OnParentViewChanged(TCefViewRef.UnWrap(view),
                                                        added <> 0,
                                                        TCefViewRef.UnWrap(parent));
end;

procedure cef_view_delegate_on_child_view_changed(self: PCefViewDelegate; view: PCefView; added: Integer; child: PCefView); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefViewDelegateOwn) then
    TCefViewDelegateOwn(TempObject).OnChildViewChanged(TCefViewRef.UnWrap(view),
                                                       added <> 0,
                                                       TCefViewRef.UnWrap(child));
end;

procedure cef_view_delegate_on_focus(self: PCefViewDelegate; view: PCefView); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefViewDelegateOwn) then
    TCefViewDelegateOwn(TempObject).OnFocus(TCefViewRef.UnWrap(view));
end;

procedure cef_view_delegate_on_blur(self: PCefViewDelegate; view: PCefView); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefViewDelegateOwn) then
    TCefViewDelegateOwn(TempObject).OnBlur(TCefViewRef.UnWrap(view));
end;

constructor TCefViewDelegateOwn.Create;
begin
  inherited CreateData(SizeOf(TCefViewDelegate));

  InitializeCEFMethods;
end;

procedure TCefViewDelegateOwn.InitializeCEFMethods;
begin
  with PCefViewDelegate(FData)^ do
    begin
      get_preferred_size      := {$IFDEF FPC}@{$ENDIF}cef_view_delegate_get_preferred_size;
      get_minimum_size        := {$IFDEF FPC}@{$ENDIF}cef_view_delegate_get_minimum_size;
      get_maximum_size        := {$IFDEF FPC}@{$ENDIF}cef_view_delegate_get_maximum_size;
      get_height_for_width    := {$IFDEF FPC}@{$ENDIF}cef_view_delegate_get_height_for_width;
      on_parent_view_changed  := {$IFDEF FPC}@{$ENDIF}cef_view_delegate_on_parent_view_changed;
      on_child_view_changed   := {$IFDEF FPC}@{$ENDIF}cef_view_delegate_on_child_view_changed;
      on_focus                := {$IFDEF FPC}@{$ENDIF}cef_view_delegate_on_focus;
      on_blur                 := {$IFDEF FPC}@{$ENDIF}cef_view_delegate_on_blur;
    end;
end;

procedure TCefViewDelegateOwn.OnGetPreferredSize(const view: ICefView; var aResult : TCefSize);
begin
  //
end;

procedure TCefViewDelegateOwn.OnGetMinimumSize(const view: ICefView; var aResult : TCefSize);
begin
  //
end;

procedure TCefViewDelegateOwn.OnGetMaximumSize(const view: ICefView; var aResult : TCefSize);
begin
  //
end;

procedure TCefViewDelegateOwn.OnGetHeightForWidth(const view: ICefView; width: Integer; var aResult: Integer);
begin
  //
end;

procedure TCefViewDelegateOwn.OnParentViewChanged(const view: ICefView; added: boolean; const parent: ICefView);
begin
  //
end;

procedure TCefViewDelegateOwn.OnChildViewChanged(const view: ICefView; added: boolean; const child: ICefView);
begin
  //
end;

procedure TCefViewDelegateOwn.OnFocus(const view: ICefView);
begin
  //
end;

procedure TCefViewDelegateOwn.OnBlur(const view: ICefView);
begin
  //
end;


// **************************************************************
// ******************** TCustomViewDelegate *********************
// **************************************************************

constructor TCustomViewDelegate.Create(const events: ICefViewDelegateEvents);
begin
  inherited Create;

  FEvents := Pointer(events);
end;

destructor TCustomViewDelegate.Destroy;
begin
  FEvents := nil;

  inherited Destroy;
end;

procedure TCustomViewDelegate.OnGetPreferredSize(const view: ICefView; var aResult : TCefSize);
begin
  try
    if (FEvents <> nil) then
      ICefViewDelegateEvents(FEvents).doOnGetPreferredSize(view, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomViewDelegate.OnGetPreferredSize', e) then raise;
  end;
end;

procedure TCustomViewDelegate.OnGetMinimumSize(const view: ICefView; var aResult : TCefSize);
begin
  try
    if (FEvents <> nil) then
      ICefViewDelegateEvents(FEvents).doOnGetMinimumSize(view, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomViewDelegate.OnGetMinimumSize', e) then raise;
  end;
end;

procedure TCustomViewDelegate.OnGetMaximumSize(const view: ICefView; var aResult : TCefSize);
begin
  try
    if (FEvents <> nil) then
      ICefViewDelegateEvents(FEvents).doOnGetMaximumSize(view, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomViewDelegate.OnGetMaximumSize', e) then raise;
  end;
end;

procedure TCustomViewDelegate.OnGetHeightForWidth(const view: ICefView; width: Integer; var aResult: Integer);
begin
  try
    if (FEvents <> nil) then
      ICefViewDelegateEvents(FEvents).doOnGetHeightForWidth(view, width, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomViewDelegate.OnGetHeightForWidth', e) then raise;
  end;
end;

procedure TCustomViewDelegate.OnParentViewChanged(const view: ICefView; added: boolean; const parent: ICefView);
begin
  try
    if (FEvents <> nil) then
      ICefViewDelegateEvents(FEvents).doOnParentViewChanged(view, added, parent);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomViewDelegate.OnParentViewChanged', e) then raise;
  end;
end;

procedure TCustomViewDelegate.OnChildViewChanged(const view: ICefView; added: boolean; const child: ICefView);
begin
  try
    if (FEvents <> nil) then
      ICefViewDelegateEvents(FEvents).doOnChildViewChanged(view, added, child);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomViewDelegate.OnChildViewChanged', e) then raise;
  end;
end;

procedure TCustomViewDelegate.OnFocus(const view: ICefView);
begin
  try
    if (FEvents <> nil) then
      ICefViewDelegateEvents(FEvents).doOnFocus(view);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomViewDelegate.OnFocus', e) then raise;
  end;
end;

procedure TCustomViewDelegate.OnBlur(const view: ICefView);
begin
  try
    if (FEvents <> nil) then
      ICefViewDelegateEvents(FEvents).doOnBlur(view);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomViewDelegate.OnBlur', e) then raise;
  end;
end;

end.

