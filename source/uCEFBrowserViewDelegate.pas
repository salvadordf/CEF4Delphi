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

unit uCEFBrowserViewDelegate;

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
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes, uCEFViewDelegate;

type
  TCefBrowserViewDelegateRef = class(TCefViewDelegateRef, ICefBrowserViewDelegate)
    protected
      procedure OnBrowserCreated(const browser_view: ICefBrowserView; const browser: ICefBrowser);
      procedure OnBrowserDestroyed(const browser_view: ICefBrowserView; const browser: ICefBrowser);
      procedure OnGetDelegateForPopupBrowserView(const browser_view: ICefBrowserView; const settings: TCefBrowserSettings; const client: ICefClient; is_devtools: boolean; var aResult : ICefBrowserViewDelegate);
      procedure OnPopupBrowserViewCreated(const browser_view, popup_browser_view: ICefBrowserView; is_devtools: boolean; var aResult : boolean);
      function  GetChromeToolbarType: TCefChromeToolbarType;

    public
      class function UnWrap(data: Pointer): ICefBrowserViewDelegate;
  end;

  TCefBrowserViewDelegateOwn = class(TCefViewDelegateOwn, ICefBrowserViewDelegate)
    protected
      procedure OnBrowserCreated(const browser_view: ICefBrowserView; const browser: ICefBrowser); virtual;
      procedure OnBrowserDestroyed(const browser_view: ICefBrowserView; const browser: ICefBrowser); virtual;
      procedure OnGetDelegateForPopupBrowserView(const browser_view: ICefBrowserView; const settings: TCefBrowserSettings; const client: ICefClient; is_devtools: boolean; var aResult : ICefBrowserViewDelegate); virtual;
      procedure OnPopupBrowserViewCreated(const browser_view, popup_browser_view: ICefBrowserView; is_devtools: boolean; var aResult : boolean); virtual;
      function  GetChromeToolbarType: TCefChromeToolbarType; virtual;

      procedure InitializeCEFMethods; override;

    public
      constructor Create; override;
  end;

  TCustomBrowserViewDelegate = class(TCefBrowserViewDelegateOwn)
    protected
      FEvents : Pointer;

      // ICefViewDelegate
      procedure OnGetPreferredSize(const view: ICefView; var aResult : TCefSize); override;
      procedure OnGetMinimumSize(const view: ICefView; var aResult : TCefSize); override;
      procedure OnGetMaximumSize(const view: ICefView; var aResult : TCefSize); override;
      procedure OnGetHeightForWidth(const view: ICefView; width: Integer; var aResult: Integer); override;
      procedure OnParentViewChanged(const view: ICefView; added: boolean; const parent: ICefView); override;
      procedure OnChildViewChanged(const view: ICefView; added: boolean; const child: ICefView); override;
      procedure OnWindowChanged(const view: ICefView; added: boolean); override;
      procedure OnLayoutChanged(const view: ICefView; new_bounds: TCefRect); override;
      procedure OnFocus(const view: ICefView); override;
      procedure OnBlur(const view: ICefView); override;

      // ICefBrowserViewDelegate
      procedure OnBrowserCreated(const browser_view: ICefBrowserView; const browser: ICefBrowser); override;
      procedure OnBrowserDestroyed(const browser_view: ICefBrowserView; const browser: ICefBrowser); override;
      procedure OnGetDelegateForPopupBrowserView(const browser_view: ICefBrowserView; const settings: TCefBrowserSettings; const client: ICefClient; is_devtools: boolean; var aResult : ICefBrowserViewDelegate); override;
      procedure OnPopupBrowserViewCreated(const browser_view, popup_browser_view: ICefBrowserView; is_devtools: boolean; var aResult : boolean); override;
      function  GetChromeToolbarType: TCefChromeToolbarType; override;

    public
      constructor Create(const events: ICefBrowserViewDelegateEvents); reintroduce;
  end;

implementation

uses
  uCEFLibFunctions, uCEFMiscFunctions, uCEFBrowserView, uCEFBrowser, uCEFClient, uCEFConstants;


// **************************************************************
// **************** TCefBrowserViewDelegateRef ******************
// **************************************************************

procedure TCefBrowserViewDelegateRef.OnBrowserCreated(const browser_view : ICefBrowserView;
                                                      const browser      : ICefBrowser);
begin
  PCefBrowserViewDelegate(FData)^.on_browser_created(PCefBrowserViewDelegate(FData),
                                                     CefGetData(browser_view),
                                                     CefGetData(browser));
end;

procedure TCefBrowserViewDelegateRef.OnBrowserDestroyed(const browser_view : ICefBrowserView;
                                                        const browser      : ICefBrowser);
begin
  PCefBrowserViewDelegate(FData)^.on_browser_destroyed(PCefBrowserViewDelegate(FData),
                                                       CefGetData(browser_view),
                                                       CefGetData(browser));
end;

procedure TCefBrowserViewDelegateRef.OnGetDelegateForPopupBrowserView(const browser_view : ICefBrowserView;
                                                                      const settings     : TCefBrowserSettings;
                                                                      const client       : ICefClient;
                                                                            is_devtools  : boolean;
                                                                      var   aResult      : ICefBrowserViewDelegate);
begin
  aResult := UnWrap((PCefBrowserViewDelegate(FData)^.get_delegate_for_popup_browser_view(PCefBrowserViewDelegate(FData),
                                                                                         CefGetData(browser_view),
                                                                                         @settings,
                                                                                         CefGetData(client),
                                                                                         ord(is_devtools))));
end;

procedure TCefBrowserViewDelegateRef.OnPopupBrowserViewCreated(const browser_view       : ICefBrowserView;
                                                               const popup_browser_view : ICefBrowserView;
                                                                     is_devtools        : boolean;
                                                               var   aResult            : boolean);
begin
  aResult := (PCefBrowserViewDelegate(FData)^.on_popup_browser_view_created(PCefBrowserViewDelegate(FData),
                                                                            CefGetData(browser_view),
                                                                            CefGetData(popup_browser_view),
                                                                            ord(is_devtools)) <> 0);
end;

function TCefBrowserViewDelegateRef.GetChromeToolbarType: TCefChromeToolbarType;
begin
  Result := PCefBrowserViewDelegate(FData)^.get_chrome_toolbar_type(PCefBrowserViewDelegate(FData));
end;

class function TCefBrowserViewDelegateRef.UnWrap(data: Pointer): ICefBrowserViewDelegate;
begin
  if (data <> nil) then
    Result := Create(data) as ICefBrowserViewDelegate
   else
    Result := nil;
end;


// **************************************************************
// **************** TCefBrowserViewDelegateOwn ******************
// **************************************************************

procedure cef_browserview_delegate_on_browser_created(self         : PCefBrowserViewDelegate;
                                                      browser_view : PCefBrowserView;
                                                      browser      : PCefBrowser); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefBrowserViewDelegateOwn) then
    TCefBrowserViewDelegateOwn(TempObject).OnBrowserCreated(TCefBrowserViewRef.UnWrap(browser_view),
                                                            TCefBrowserRef.UnWrap(browser));
end;

procedure cef_browserview_delegate_on_browser_destroyed(self         : PCefBrowserViewDelegate;
                                                        browser_view : PCefBrowserView;
                                                        browser      : PCefBrowser); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefBrowserViewDelegateOwn) then
    TCefBrowserViewDelegateOwn(TempObject).OnBrowserDestroyed(TCefBrowserViewRef.UnWrap(browser_view),
                                                              TCefBrowserRef.UnWrap(browser));
end;

function cef_browserview_delegate_get_delegate_for_popup_browser_view(      self         : PCefBrowserViewDelegate;
                                                                            browser_view : PCefBrowserView;
                                                                      const settings     : PCefBrowserSettings;
                                                                            client       : PCefClient;
                                                                            is_devtools  : Integer): PCefBrowserViewDelegate; stdcall;
var
  TempObject   : TObject;
  TempDelegate : ICefBrowserViewDelegate;
begin
  TempObject   := CefGetObject(self);
  TempDelegate := nil;

  if (TempObject <> nil) and (TempObject is TCefBrowserViewDelegateOwn) then
    TCefBrowserViewDelegateOwn(TempObject).OnGetDelegateForPopupBrowserView(TCefBrowserViewRef.UnWrap(browser_view),
                                                                            settings^,
                                                                            TCefClientRef.UnWrap(client),
                                                                            is_devtools <> 0,
                                                                            TempDelegate);

  Result := CefGetData(TempDelegate);
end;

function cef_browserview_delegate_on_popup_browser_view_created(self               : PCefBrowserViewDelegate;
                                                                browser_view       : PCefBrowserView;
                                                                popup_browser_view : PCefBrowserView;
                                                                is_devtools        : Integer): Integer; stdcall;
var
  TempObject : TObject;
  TempResult : boolean;
begin
  TempObject := CefGetObject(self);
  TempResult := False;

  if (TempObject <> nil) and (TempObject is TCefBrowserViewDelegateOwn) then
    TCefBrowserViewDelegateOwn(TempObject).OnPopupBrowserViewCreated(TCefBrowserViewRef.UnWrap(browser_view),
                                                                     TCefBrowserViewRef.UnWrap(popup_browser_view),
                                                                     is_devtools <> 0,
                                                                     TempResult);

  Result := ord(TempResult);
end;

function cef_browserview_delegate_get_chrome_toolbar_type(self : PCefBrowserViewDelegate): TCefChromeToolbarType; stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);
  Result     := CEF_CTT_NONE;

  if (TempObject <> nil) and (TempObject is TCefBrowserViewDelegateOwn) then
    Result := TCefBrowserViewDelegateOwn(TempObject).GetChromeToolbarType();
end;

constructor TCefBrowserViewDelegateOwn.Create;
begin
  inherited CreateData(SizeOf(TCefBrowserViewDelegate));

  InitializeCEFMethods;
end;

procedure TCefBrowserViewDelegateOwn.InitializeCEFMethods;
begin
  inherited InitializeCEFMethods;

  with PCefBrowserViewDelegate(FData)^ do
    begin
      on_browser_created                  := {$IFDEF FPC}@{$ENDIF}cef_browserview_delegate_on_browser_created;
      on_browser_destroyed                := {$IFDEF FPC}@{$ENDIF}cef_browserview_delegate_on_browser_destroyed;
      get_delegate_for_popup_browser_view := {$IFDEF FPC}@{$ENDIF}cef_browserview_delegate_get_delegate_for_popup_browser_view;
      on_popup_browser_view_created       := {$IFDEF FPC}@{$ENDIF}cef_browserview_delegate_on_popup_browser_view_created;
      get_chrome_toolbar_type             := {$IFDEF FPC}@{$ENDIF}cef_browserview_delegate_get_chrome_toolbar_type;
    end;
end;

procedure TCefBrowserViewDelegateOwn.OnBrowserCreated(const browser_view: ICefBrowserView; const browser: ICefBrowser);
begin
  //
end;

procedure TCefBrowserViewDelegateOwn.OnBrowserDestroyed(const browser_view: ICefBrowserView; const browser: ICefBrowser);
begin
  //
end;

procedure TCefBrowserViewDelegateOwn.OnGetDelegateForPopupBrowserView(const browser_view: ICefBrowserView; const settings: TCefBrowserSettings; const client: ICefClient; is_devtools: boolean; var aResult : ICefBrowserViewDelegate);
begin
  //
end;

procedure TCefBrowserViewDelegateOwn.OnPopupBrowserViewCreated(const browser_view, popup_browser_view: ICefBrowserView; is_devtools: boolean; var aResult : boolean);
begin
  //
end;

function TCefBrowserViewDelegateOwn.GetChromeToolbarType: TCefChromeToolbarType;
begin
  Result := CEF_CTT_NONE;
end;


// **************************************************************
// **************** TCustomBrowserViewDelegate ******************
// **************************************************************

constructor TCustomBrowserViewDelegate.Create(const events: ICefBrowserViewDelegateEvents);
begin
  inherited Create;

  FEvents := Pointer(events);
end;

procedure TCustomBrowserViewDelegate.OnGetPreferredSize(const view: ICefView; var aResult : TCefSize);
begin
  try
    if (FEvents <> nil) then
      ICefBrowserViewDelegateEvents(FEvents).doOnGetPreferredSize(view, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomBrowserViewDelegate.OnGetPreferredSize', e) then raise;
  end;
end;

procedure TCustomBrowserViewDelegate.OnGetMinimumSize(const view: ICefView; var aResult : TCefSize);
begin
  try
    if (FEvents <> nil) then
      ICefBrowserViewDelegateEvents(FEvents).doOnGetMinimumSize(view, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomBrowserViewDelegate.OnGetMinimumSize', e) then raise;
  end;
end;

procedure TCustomBrowserViewDelegate.OnGetMaximumSize(const view: ICefView; var aResult : TCefSize);
begin
  try
    if (FEvents <> nil) then
      ICefBrowserViewDelegateEvents(FEvents).doOnGetMaximumSize(view, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomBrowserViewDelegate.OnGetMaximumSize', e) then raise;
  end;
end;

procedure TCustomBrowserViewDelegate.OnGetHeightForWidth(const view: ICefView; width: Integer; var aResult: Integer);
begin
  try
    if (FEvents <> nil) then
      ICefBrowserViewDelegateEvents(FEvents).doOnGetHeightForWidth(view, width, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomBrowserViewDelegate.OnGetHeightForWidth', e) then raise;
  end;
end;

procedure TCustomBrowserViewDelegate.OnParentViewChanged(const view: ICefView; added: boolean; const parent: ICefView);
begin
  try
    if (FEvents <> nil) then
      ICefBrowserViewDelegateEvents(FEvents).doOnParentViewChanged(view, added, parent);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomBrowserViewDelegate.OnParentViewChanged', e) then raise;
  end;
end;

procedure TCustomBrowserViewDelegate.OnChildViewChanged(const view: ICefView; added: boolean; const child: ICefView);
begin
  try
    if (FEvents <> nil) then
      ICefBrowserViewDelegateEvents(FEvents).doOnChildViewChanged(view, added, child);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomBrowserViewDelegate.OnChildViewChanged', e) then raise;
  end;
end;

procedure TCustomBrowserViewDelegate.OnWindowChanged(const view: ICefView; added: boolean);
begin
  try
    if (FEvents <> nil) then
      ICefBrowserViewDelegateEvents(FEvents).doOnWindowChanged(view, added);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomBrowserViewDelegate.OnWindowChanged', e) then raise;
  end;
end;

procedure TCustomBrowserViewDelegate.OnLayoutChanged(const view: ICefView; new_bounds: TCefRect);
begin
  try
    if (FEvents <> nil) then
      ICefBrowserViewDelegateEvents(FEvents).doOnLayoutChanged(view, new_bounds);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomBrowserViewDelegate.OnLayoutChanged', e) then raise;
  end;
end;

procedure TCustomBrowserViewDelegate.OnFocus(const view: ICefView);
begin
  try
    if (FEvents <> nil) then
      ICefBrowserViewDelegateEvents(FEvents).doOnFocus(view);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomBrowserViewDelegate.OnFocus', e) then raise;
  end;
end;

procedure TCustomBrowserViewDelegate.OnBlur(const view: ICefView);
begin
  try
    if (FEvents <> nil) then
      ICefBrowserViewDelegateEvents(FEvents).doOnBlur(view);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomBrowserViewDelegate.OnBlur', e) then raise;
  end;
end;

procedure TCustomBrowserViewDelegate.OnBrowserCreated(const browser_view: ICefBrowserView; const browser: ICefBrowser);
begin
  try
    if (FEvents <> nil) then
      ICefBrowserViewDelegateEvents(FEvents).doOnBrowserCreated(browser_view, browser);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomBrowserViewDelegate.OnBrowserCreated', e) then raise;
  end;
end;

procedure TCustomBrowserViewDelegate.OnBrowserDestroyed(const browser_view: ICefBrowserView; const browser: ICefBrowser);
begin
  try
    if (FEvents <> nil) then
      ICefBrowserViewDelegateEvents(FEvents).doOnBrowserDestroyed(browser_view, browser);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomBrowserViewDelegate.OnBrowserDestroyed', e) then raise;
  end;
end;

procedure TCustomBrowserViewDelegate.OnGetDelegateForPopupBrowserView(const browser_view: ICefBrowserView; const settings: TCefBrowserSettings; const client: ICefClient; is_devtools: boolean; var aResult : ICefBrowserViewDelegate);
begin
  try
    if (FEvents <> nil) then
      ICefBrowserViewDelegateEvents(FEvents).doOnGetDelegateForPopupBrowserView(browser_view, settings, client, is_devtools, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomBrowserViewDelegate.OnGetDelegateForPopupBrowserView', e) then raise;
  end;
end;

procedure TCustomBrowserViewDelegate.OnPopupBrowserViewCreated(const browser_view, popup_browser_view: ICefBrowserView; is_devtools: boolean; var aResult : boolean);
begin
  try
    if (FEvents <> nil) then
      ICefBrowserViewDelegateEvents(FEvents).doOnPopupBrowserViewCreated(browser_view, popup_browser_view, is_devtools, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomBrowserViewDelegate.OnPopupBrowserViewCreated', e) then raise;
  end;
end;

function TCustomBrowserViewDelegate.GetChromeToolbarType: TCefChromeToolbarType;
begin
  Result := inherited GetChromeToolbarType();

  try
    if (FEvents <> nil) then
      ICefBrowserViewDelegateEvents(FEvents).doOnGetChromeToolbarType(Result);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomBrowserViewDelegate.GetChromeToolbarType', e) then raise;
  end;
end;

end.

