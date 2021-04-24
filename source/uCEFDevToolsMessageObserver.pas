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

unit uCEFDevToolsMessageObserver;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCEFDevToolsMessageObserverOwn = class(TCefBaseRefCountedOwn, ICefDevToolsMessageObserver)
    protected
      procedure OnDevToolsMessage(const browser: ICefBrowser; const message_: Pointer; message_size: NativeUInt; var aHandled: boolean); virtual;
      procedure OnDevToolsMethodResult(const browser: ICefBrowser; message_id: integer; success: boolean; const result: Pointer; result_size: NativeUInt); virtual;
      procedure OnDevToolsEvent(const browser: ICefBrowser; const method: ustring; const params: Pointer; params_size: NativeUInt); virtual;
      procedure OnDevToolsAgentAttached(const browser: ICefBrowser); virtual;
      procedure OnDevToolsAgentDetached(const browser: ICefBrowser); virtual;

    public
      constructor Create; virtual;
  end;

  TCustomDevToolsMessageObserver = class(TCEFDevToolsMessageObserverOwn)
    protected
      FEvents : Pointer;

      procedure OnDevToolsMessage(const browser: ICefBrowser; const message_: Pointer; message_size: NativeUInt; var aHandled: boolean); override;
      procedure OnDevToolsMethodResult(const browser: ICefBrowser; message_id: integer; success: boolean; const result: Pointer; result_size: NativeUInt); override;
      procedure OnDevToolsEvent(const browser: ICefBrowser; const method: ustring; const params: Pointer; params_size: NativeUInt); override;
      procedure OnDevToolsAgentAttached(const browser: ICefBrowser); override;
      procedure OnDevToolsAgentDetached(const browser: ICefBrowser); override;

    public
      constructor Create(const events: IChromiumEvents); reintroduce;
      destructor  Destroy; override;
  end;

implementation

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBrowser, uCEFJson;


// ************************************************************
// ************** TCEFDevToolsMessageObserverOwn **************
// ************************************************************

function cef_on_dev_tools_message(      self         : PCefDevToolsMessageObserver;
                                        browser      : PCefBrowser;
                                  const message_     : Pointer;
                                        message_size : NativeUInt): Integer; stdcall;
var
  TempObject  : TObject;
  TempHandled : boolean;
begin
  TempObject  := CefGetObject(self);
  TempHandled := False;

  if (TempObject <> nil) and (TempObject is TCEFDevToolsMessageObserverOwn) then
    TCEFDevToolsMessageObserverOwn(TempObject).OnDevToolsMessage(TCefBrowserRef.UnWrap(browser),
                                                                 message_,
                                                                 message_size,
                                                                 TempHandled);

  Result := ord(TempHandled);
end;

procedure cef_on_dev_tools_method_result(      self        : PCefDevToolsMessageObserver;
                                               browser     : PCefBrowser;
                                               message_id  : Integer;
                                               success     : Integer;
                                         const result      : Pointer;
                                               result_size : NativeUInt); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCEFDevToolsMessageObserverOwn) then
    TCEFDevToolsMessageObserverOwn(TempObject).OnDevToolsMethodResult(TCefBrowserRef.UnWrap(browser),
                                                                      message_id,
                                                                      success <> 0,
                                                                      result,
                                                                      result_size);
end;

procedure cef_on_dev_tools_event(      self        : PCefDevToolsMessageObserver;
                                       browser     : PCefBrowser;
                                 const method      : PCefString;
                                 const params      : Pointer;
                                       params_size : NativeUInt); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCEFDevToolsMessageObserverOwn) then
    TCEFDevToolsMessageObserverOwn(TempObject).OnDevToolsEvent(TCefBrowserRef.UnWrap(browser),
                                                               CefString(method),
                                                               params,
                                                               params_size);
end;

procedure cef_on_dev_tools_agent_attached(self    : PCefDevToolsMessageObserver;
                                          browser : PCefBrowser); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCEFDevToolsMessageObserverOwn) then
    TCEFDevToolsMessageObserverOwn(TempObject).OnDevToolsAgentAttached(TCefBrowserRef.UnWrap(browser));
end;

procedure cef_on_dev_tools_agent_detached(self    : PCefDevToolsMessageObserver;
                                          browser : PCefBrowser); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCEFDevToolsMessageObserverOwn) then
    TCEFDevToolsMessageObserverOwn(TempObject).OnDevToolsAgentDetached(TCefBrowserRef.UnWrap(browser));
end;

constructor TCEFDevToolsMessageObserverOwn.Create;
begin
  inherited CreateData(SizeOf(TCefDevToolsMessageObserver));

  with PCefDevToolsMessageObserver(FData)^ do
    begin
      on_dev_tools_message        := {$IFDEF FPC}@{$ENDIF}cef_on_dev_tools_message;
      on_dev_tools_method_result  := {$IFDEF FPC}@{$ENDIF}cef_on_dev_tools_method_result;
      on_dev_tools_event          := {$IFDEF FPC}@{$ENDIF}cef_on_dev_tools_event;
      on_dev_tools_agent_attached := {$IFDEF FPC}@{$ENDIF}cef_on_dev_tools_agent_attached;
      on_dev_tools_agent_detached := {$IFDEF FPC}@{$ENDIF}cef_on_dev_tools_agent_detached;
    end;
end;

procedure TCEFDevToolsMessageObserverOwn.OnDevToolsMessage(const browser      : ICefBrowser;
                                                           const message_     : Pointer;
                                                                 message_size : NativeUInt;
                                                           var   aHandled     : boolean);
begin
  //
end;

procedure TCEFDevToolsMessageObserverOwn.OnDevToolsMethodResult(const browser     : ICefBrowser;
                                                                      message_id  : integer;
                                                                      success     : boolean;
                                                                const result      : Pointer;
                                                                      result_size : NativeUInt);
begin
  //
end;

procedure TCEFDevToolsMessageObserverOwn.OnDevToolsEvent(const browser     : ICefBrowser;
                                                         const method      : ustring;
                                                         const params      : Pointer;
                                                               params_size : NativeUInt);
begin
  //
end;

procedure TCEFDevToolsMessageObserverOwn.OnDevToolsAgentAttached(const browser: ICefBrowser);
begin
  //
end;

procedure TCEFDevToolsMessageObserverOwn.OnDevToolsAgentDetached(const browser: ICefBrowser);
begin
  //
end;


// ************************************************************
// ************** TCustomDevToolsMessageObserver **************
// ************************************************************

constructor TCustomDevToolsMessageObserver.Create(const events: IChromiumEvents);
begin
  inherited Create;

  FEvents := Pointer(events);
end;

destructor TCustomDevToolsMessageObserver.Destroy;
begin
  FEvents := nil;

  inherited Destroy;
end;

procedure TCustomDevToolsMessageObserver.OnDevToolsMessage(const browser      : ICefBrowser;
                                                           const message_     : Pointer;
                                                                 message_size : NativeUInt;
                                                           var   aHandled     : boolean);
begin
  try
    if (FEvents <> nil) then
      IChromiumEvents(FEvents).doOnDevToolsMessage(browser, message_, message_size, aHandled);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomDevToolsMessageObserver.OnDevToolsMessage', e) then raise;
  end;
end;

procedure TCustomDevToolsMessageObserver.OnDevToolsMethodResult(const browser     : ICefBrowser;
                                                                      message_id  : integer;
                                                                      success     : boolean;
                                                                const result      : Pointer;
                                                                      result_size : NativeUInt);
begin
  try
    if (FEvents <> nil) then
      IChromiumEvents(FEvents).doOnDevToolsMethodResult(browser, message_id, success, result, result_size);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomDevToolsMessageObserver.OnDevToolsMethodResult', e) then raise;
  end;
end;

procedure TCustomDevToolsMessageObserver.OnDevToolsEvent(const browser     : ICefBrowser;
                                                         const method      : ustring;
                                                         const params      : Pointer;
                                                               params_size : NativeUInt);
begin
  try
    if (FEvents <> nil) then
      IChromiumEvents(FEvents).doOnDevToolsEvent(browser, method, params, params_size);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomDevToolsMessageObserver.OnDevToolsEvent', e) then raise;
  end;
end;

procedure TCustomDevToolsMessageObserver.OnDevToolsAgentAttached(const browser: ICefBrowser);
begin
  try
    if (FEvents <> nil) then
      IChromiumEvents(FEvents).doOnDevToolsAgentAttached(browser);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomDevToolsMessageObserver.OnDevToolsAgentAttached', e) then raise;
  end;
end;

procedure TCustomDevToolsMessageObserver.OnDevToolsAgentDetached(const browser: ICefBrowser);
begin
  try
    if (FEvents <> nil) then
      IChromiumEvents(FEvents).doOnDevToolsAgentDetached(browser);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomDevToolsMessageObserver.OnDevToolsAgentDetached', e) then raise;
  end;
end;

end.
