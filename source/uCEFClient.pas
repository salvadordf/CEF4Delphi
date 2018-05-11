// ************************************************************************
// ***************************** CEF4Delphi *******************************
// ************************************************************************
//
// CEF4Delphi is based on DCEF3 which uses CEF3 to embed a chromium-based
// browser in Delphi applications.
//
// The original license of DCEF3 still applies to CEF4Delphi.
//
// For more information about CEF4Delphi visit :
//         https://www.briskbard.com/index.php?lang=en&pageid=cef
//
//        Copyright � 2018 Salvador D�az Fau. All rights reserved.
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

unit uCEFClient;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefClientRef = class(TCefBaseRefCountedRef, ICefClient)
    protected
      procedure GetContextMenuHandler(var aHandler : ICefContextMenuHandler); virtual;
      procedure GetDialogHandler(var aHandler : ICefDialogHandler); virtual;
      procedure GetDisplayHandler(var aHandler : ICefDisplayHandler); virtual;
      procedure GetDownloadHandler(var aHandler : ICefDownloadHandler); virtual;
      procedure GetDragHandler(var aHandler : ICefDragHandler); virtual;
      procedure GetFindHandler(var aHandler : ICefFindHandler); virtual;
      procedure GetFocusHandler(var aHandler : ICefFocusHandler); virtual;
      procedure GetJsdialogHandler(var aHandler : ICefJsdialogHandler); virtual;
      procedure GetKeyboardHandler(var aHandler : ICefKeyboardHandler); virtual;
      procedure GetLifeSpanHandler(var aHandler : ICefLifeSpanHandler); virtual;
      procedure GetLoadHandler(var aHandler : ICefLoadHandler); virtual;
      procedure GetRenderHandler(var aHandler : ICefRenderHandler); virtual;
      procedure GetRequestHandler(var aHandler : ICefRequestHandler); virtual;
      function  OnProcessMessageReceived(const browser: ICefBrowser; sourceProcess: TCefProcessId; const message_ : ICefProcessMessage): Boolean; virtual;

      procedure RemoveReferences; virtual;

    public
      class function UnWrap(data: Pointer): ICefClient;
  end;

  TCefClientOwn = class(TCefBaseRefCountedOwn, ICefClient)
    protected
      procedure GetContextMenuHandler(var aHandler : ICefContextMenuHandler); virtual;
      procedure GetDialogHandler(var aHandler : ICefDialogHandler); virtual;
      procedure GetDisplayHandler(var aHandler : ICefDisplayHandler); virtual;
      procedure GetDownloadHandler(var aHandler : ICefDownloadHandler); virtual;
      procedure GetDragHandler(var aHandler : ICefDragHandler); virtual;
      procedure GetFindHandler(var aHandler : ICefFindHandler); virtual;
      procedure GetFocusHandler(var aHandler : ICefFocusHandler); virtual;
      procedure GetJsdialogHandler(var aHandler : ICefJsdialogHandler); virtual;
      procedure GetKeyboardHandler(var aHandler : ICefKeyboardHandler); virtual;
      procedure GetLifeSpanHandler(var aHandler : ICefLifeSpanHandler); virtual;
      procedure GetLoadHandler(var aHandler : ICefLoadHandler); virtual;
      procedure GetRenderHandler(var aHandler : ICefRenderHandler); virtual;
      procedure GetRequestHandler(var aHandler : ICefRequestHandler); virtual;
      function  OnProcessMessageReceived(const browser: ICefBrowser; sourceProcess: TCefProcessId; const message_ : ICefProcessMessage): Boolean; virtual;

      procedure RemoveReferences; virtual;

    public
      constructor Create; virtual;
  end;

  TCustomClientHandler = class(TCefClientOwn)
    protected
      FEvents             : Pointer;
      FLoadHandler        : ICefLoadHandler;
      FFocusHandler       : ICefFocusHandler;
      FContextMenuHandler : ICefContextMenuHandler;
      FDialogHandler      : ICefDialogHandler;
      FKeyboardHandler    : ICefKeyboardHandler;
      FDisplayHandler     : ICefDisplayHandler;
      FDownloadHandler    : ICefDownloadHandler;
      FJsDialogHandler    : ICefJsDialogHandler;
      FLifeSpanHandler    : ICefLifeSpanHandler;
      FRenderHandler      : ICefRenderHandler;
      FRequestHandler     : ICefRequestHandler;
      FDragHandler        : ICefDragHandler;
      FFindHandler        : ICefFindHandler;

      procedure GetContextMenuHandler(var aHandler : ICefContextMenuHandler); override;
      procedure GetDialogHandler(var aHandler : ICefDialogHandler); override;
      procedure GetDisplayHandler(var aHandler : ICefDisplayHandler); override;
      procedure GetDownloadHandler(var aHandler : ICefDownloadHandler); override;
      procedure GetDragHandler(var aHandler : ICefDragHandler); override;
      procedure GetFindHandler(var aHandler : ICefFindHandler); override;
      procedure GetFocusHandler(var aHandler : ICefFocusHandler); override;
      procedure GetJsdialogHandler(var aHandler : ICefJsdialogHandler); override;
      procedure GetKeyboardHandler(var aHandler : ICefKeyboardHandler); override;
      procedure GetLifeSpanHandler(var aHandler : ICefLifeSpanHandler); override;
      procedure GetLoadHandler(var aHandler : ICefLoadHandler); override;
      procedure GetRenderHandler(var aHandler : ICefRenderHandler); override;
      procedure GetRequestHandler(var aHandler : ICefRequestHandler); override;
      function  OnProcessMessageReceived(const browser: ICefBrowser; sourceProcess: TCefProcessId; const message_ : ICefProcessMessage): Boolean; override;

      procedure InitializeVars;

    public
      constructor Create(const events: IChromiumEvents;
                         aCreateLoadHandler, aCreateFocusHandler, aCreateContextMenuHandler, aCreateDialogHandler,
                         aCreateKeyboardHandler, aCreateDisplayHandler, aCreateDownloadHandler, aCreateJsDialogHandler,
                         aCreateLifeSpanHandler, aCreateRenderHandler, aCreateRequestHandler, aCreateDragHandler,
                         aCreateFindHandler : boolean); reintroduce; virtual;
      procedure   BeforeDestruction; override;
      procedure   RemoveReferences; override;
  end;

implementation

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  uCEFMiscFunctions, uCEFLibFunctions, uCEFProcessMessage, uCEFBrowser, uCEFLoadHandler,
  uCEFFocusHandler, uCEFContextMenuHandler, uCEFDialogHandler, uCEFKeyboardHandler,
  uCEFDisplayHandler, uCEFDownloadHandler, uCEFJsDialogHandler,
  uCEFLifeSpanHandler, uCEFRequestHandler, uCEFRenderHandler, uCEFDragHandler,
  uCEFFindHandler, uCEFConstants, uCEFApplication;


// ******************************************************
// ****************** TCefClientRef *********************
// ******************************************************

class function TCefClientRef.UnWrap(data: Pointer): ICefClient;
begin
  if (data <> nil) then
    Result := Create(data) as ICefClient
   else
    Result := nil;
end;

procedure TCefClientRef.GetContextMenuHandler(var aHandler : ICefContextMenuHandler);
begin
  aHandler := nil;
end;

procedure TCefClientRef.GetDialogHandler(var aHandler : ICefDialogHandler);
begin
  aHandler := nil;
end;

procedure TCefClientRef.GetDisplayHandler(var aHandler : ICefDisplayHandler);
begin
  aHandler := nil;
end;

procedure TCefClientRef.GetDownloadHandler(var aHandler : ICefDownloadHandler);
begin
  aHandler := nil;
end;

procedure TCefClientRef.GetDragHandler(var aHandler : ICefDragHandler);
begin
  aHandler := nil;
end;

procedure TCefClientRef.GetFindHandler(var aHandler : ICefFindHandler);
begin
  aHandler := nil;
end;

procedure TCefClientRef.GetFocusHandler(var aHandler : ICefFocusHandler);
begin
  aHandler := nil;
end;

procedure TCefClientRef.GetJsdialogHandler(var aHandler : ICefJsDialogHandler);
begin
  aHandler := nil;
end;

procedure TCefClientRef.GetKeyboardHandler(var aHandler : ICefKeyboardHandler);
begin
  aHandler := nil;
end;

procedure TCefClientRef.GetLifeSpanHandler(var aHandler : ICefLifeSpanHandler);
begin
  aHandler := nil;
end;

procedure TCefClientRef.GetLoadHandler(var aHandler : ICefLoadHandler);
begin
  aHandler := nil;
end;

procedure TCefClientRef.GetRenderHandler(var aHandler : ICefRenderHandler);
begin
  aHandler := nil;
end;

procedure TCefClientRef.GetRequestHandler(var aHandler : ICefRequestHandler);
begin
  aHandler := nil;
end;

function TCefClientRef.OnProcessMessageReceived(const browser: ICefBrowser; sourceProcess: TCefProcessId; const message_ : ICefProcessMessage): Boolean;
begin
  Result := False;
end;

procedure TCefClientRef.RemoveReferences;
begin
  //
end;


// ******************************************************
// ****************** TCefClientOwn *********************
// ******************************************************


function cef_client_own_get_context_menu_handler(self: PCefClient): PCefContextMenuHandler; stdcall;
var
  TempObject  : TObject;
  TempHandler : ICefContextMenuHandler;
begin
  Result      := nil;
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefClientOwn) then
    try
      TCefClientOwn(TempObject).GetContextMenuHandler(TempHandler);
      if (TempHandler <> nil) then Result := TempHandler.Wrap;
    finally
      TempHandler := nil;
    end;
end;

function cef_client_own_get_dialog_handler(self: PCefClient): PCefDialogHandler; stdcall;
var
  TempObject  : TObject;
  TempHandler : ICefDialogHandler;
begin
  Result      := nil;
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefClientOwn) then
    try
      TCefClientOwn(TempObject).GetDialogHandler(TempHandler);
      if (TempHandler <> nil) then Result := TempHandler.Wrap;
    finally
      TempHandler := nil;
    end;
end;

function cef_client_own_get_display_handler(self: PCefClient): PCefDisplayHandler; stdcall;
var
  TempObject  : TObject;
  TempHandler : ICefDisplayHandler;
begin
  Result      := nil;
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefClientOwn) then
    try
      TCefClientOwn(TempObject).GetDisplayHandler(TempHandler);
      if (TempHandler <> nil) then Result := TempHandler.Wrap;
    finally
      TempHandler := nil;
    end;
end;

function cef_client_own_get_download_handler(self: PCefClient): PCefDownloadHandler; stdcall;
var
  TempObject  : TObject;
  TempHandler : ICefDownloadHandler;
begin
  Result      := nil;
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefClientOwn) then
    try
      TCefClientOwn(TempObject).GetDownloadHandler(TempHandler);
      if (TempHandler <> nil) then Result := TempHandler.Wrap;
    finally
      TempHandler := nil;
    end;
end;

function cef_client_own_get_drag_handler(self: PCefClient): PCefDragHandler; stdcall;
var
  TempObject  : TObject;
  TempHandler : ICefDragHandler;
begin
  Result      := nil;
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefClientOwn) then
    try
      TCefClientOwn(TempObject).GetDragHandler(TempHandler);
      if (TempHandler <> nil) then Result := TempHandler.Wrap;
    finally
      TempHandler := nil;
    end;
end;

function cef_client_own_get_find_handler(self: PCefClient): PCefFindHandler; stdcall;
var
  TempObject  : TObject;
  TempHandler : ICefFindHandler;
begin
  Result      := nil;
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefClientOwn) then
    try
      TCefClientOwn(TempObject).GetFindHandler(TempHandler);
      if (TempHandler <> nil) then Result := TempHandler.Wrap;
    finally
      TempHandler := nil;
    end;
end;

function cef_client_own_get_focus_handler(self: PCefClient): PCefFocusHandler; stdcall;
var
  TempObject  : TObject;
  TempHandler : ICefFocusHandler;
begin
  Result      := nil;
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefClientOwn) then
    try
      TCefClientOwn(TempObject).GetFocusHandler(TempHandler);
      if (TempHandler <> nil) then Result := TempHandler.Wrap;
    finally
      TempHandler := nil;
    end;
end;

function cef_client_own_get_jsdialog_handler(self: PCefClient): PCefJsDialogHandler; stdcall;
var
  TempObject  : TObject;
  TempHandler : ICefJsDialogHandler;
begin
  Result      := nil;
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefClientOwn) then
    try
      TCefClientOwn(TempObject).GetJsdialogHandler(TempHandler);
      if (TempHandler <> nil) then Result := TempHandler.Wrap;
    finally
      TempHandler := nil;
    end;
end;

function cef_client_own_get_keyboard_handler(self: PCefClient): PCefKeyboardHandler; stdcall;
var
  TempObject  : TObject;
  TempHandler : ICefKeyboardHandler;
begin
  Result      := nil;
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefClientOwn) then
    try
      TCefClientOwn(TempObject).GetKeyboardHandler(TempHandler);
      if (TempHandler <> nil) then Result := TempHandler.Wrap;
    finally
      TempHandler := nil;
    end;
end;

function cef_client_own_get_life_span_handler(self: PCefClient): PCefLifeSpanHandler; stdcall;
var
  TempObject  : TObject;
  TempHandler : ICefLifeSpanHandler;
begin
  Result      := nil;
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefClientOwn) then
    try
      TCefClientOwn(TempObject).GetLifeSpanHandler(TempHandler);
      if (TempHandler <> nil) then Result := TempHandler.Wrap;
    finally
      TempHandler := nil;
    end;
end;

function cef_client_own_get_load_handler(self: PCefClient): PCefLoadHandler; stdcall;
var
  TempObject  : TObject;
  TempHandler : ICefLoadHandler;
begin
  Result      := nil;
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefClientOwn) then
    try
      TCefClientOwn(TempObject).GetLoadHandler(TempHandler);
      if (TempHandler <> nil) then Result := TempHandler.Wrap;
    finally
      TempHandler := nil;
    end;
end;

function cef_client_own_get_get_render_handler(self: PCefClient): PCefRenderHandler; stdcall;
var
  TempObject  : TObject;
  TempHandler : ICefRenderHandler;
begin
  Result      := nil;
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefClientOwn) then
    try
      TCefClientOwn(TempObject).GetRenderHandler(TempHandler);
      if (TempHandler <> nil) then Result := TempHandler.Wrap;
    finally
      TempHandler := nil;
    end;
end;

function cef_client_own_get_request_handler(self: PCefClient): PCefRequestHandler; stdcall;
var
  TempObject  : TObject;
  TempHandler : ICefRequestHandler;
begin
  Result      := nil;
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefClientOwn) then
    try
      TCefClientOwn(TempObject).GetRequestHandler(TempHandler);
      if (TempHandler <> nil) then Result := TempHandler.Wrap;
    finally
      TempHandler := nil;
    end;
end;

function cef_client_own_on_process_message_received(self           : PCefClient;
                                                    browser        : PCefBrowser;
                                                    source_process : TCefProcessId;
                                                    message_       : PCefProcessMessage): Integer; stdcall;
var
  TempObject  : TObject;
begin
  Result      := Ord(False);
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefClientOwn) then
    Result := Ord(TCefClientOwn(TempObject).OnProcessMessageReceived(TCefBrowserRef.UnWrap(browser),
                                                                     source_process,
                                                                     TCefProcessMessageRef.UnWrap(message_)));
end;

constructor TCefClientOwn.Create;
begin
  inherited CreateData(SizeOf(TCefClient));

  with PCefClient(FData)^ do
    begin
      get_context_menu_handler    := cef_client_own_get_context_menu_handler;
      get_dialog_handler          := cef_client_own_get_dialog_handler;
      get_display_handler         := cef_client_own_get_display_handler;
      get_download_handler        := cef_client_own_get_download_handler;
      get_drag_handler            := cef_client_own_get_drag_handler;
      get_find_handler            := cef_client_own_get_find_handler;
      get_focus_handler           := cef_client_own_get_focus_handler;
      get_jsdialog_handler        := cef_client_own_get_jsdialog_handler;
      get_keyboard_handler        := cef_client_own_get_keyboard_handler;
      get_life_span_handler       := cef_client_own_get_life_span_handler;
      get_load_handler            := cef_client_own_get_load_handler;
      get_render_handler          := cef_client_own_get_get_render_handler;
      get_request_handler         := cef_client_own_get_request_handler;
      on_process_message_received := cef_client_own_on_process_message_received;
    end;
end;

procedure TCefClientOwn.GetContextMenuHandler(var aHandler : ICefContextMenuHandler);
begin
  aHandler := nil;
end;

procedure TCefClientOwn.GetDialogHandler(var aHandler : ICefDialogHandler);
begin
  aHandler := nil;
end;

procedure TCefClientOwn.GetDisplayHandler(var aHandler : ICefDisplayHandler);
begin
  aHandler := nil;
end;

procedure TCefClientOwn.GetDownloadHandler(var aHandler : ICefDownloadHandler);
begin
  aHandler := nil;
end;

procedure TCefClientOwn.GetDragHandler(var aHandler : ICefDragHandler);
begin
  aHandler := nil;
end;

procedure TCefClientOwn.GetFindHandler(var aHandler : ICefFindHandler);
begin
  aHandler := nil;
end;

procedure TCefClientOwn.GetFocusHandler(var aHandler : ICefFocusHandler);
begin
  aHandler := nil;
end;

procedure TCefClientOwn.GetJsdialogHandler(var aHandler : ICefJsDialogHandler);
begin
  aHandler := nil;
end;

procedure TCefClientOwn.GetKeyboardHandler(var aHandler : ICefKeyboardHandler);
begin
  aHandler := nil;
end;

procedure TCefClientOwn.GetLifeSpanHandler(var aHandler : ICefLifeSpanHandler);
begin
  aHandler := nil;
end;

procedure TCefClientOwn.GetLoadHandler(var aHandler : ICefLoadHandler);
begin
  aHandler := nil;
end;

procedure TCefClientOwn.GetRenderHandler(var aHandler : ICefRenderHandler);
begin
  aHandler := nil;
end;

procedure TCefClientOwn.GetRequestHandler(var aHandler : ICefRequestHandler);
begin
  aHandler := nil;
end;

function TCefClientOwn.OnProcessMessageReceived(const browser       : ICefBrowser;
                                                      sourceProcess : TCefProcessId;
                                                const message_      : ICefProcessMessage): Boolean;
begin
  Result := False;
end;

procedure TCefClientOwn.RemoveReferences;
begin
  //
end;


// ******************************************************
// *************** TCustomClientHandler *****************
// ******************************************************


constructor TCustomClientHandler.Create(const events                     : IChromiumEvents;
                                              aCreateLoadHandler         : boolean;
                                              aCreateFocusHandler        : boolean;
                                              aCreateContextMenuHandler  : boolean;
                                              aCreateDialogHandler       : boolean;
                                              aCreateKeyboardHandler     : boolean;
                                              aCreateDisplayHandler      : boolean;
                                              aCreateDownloadHandler     : boolean;
                                              aCreateJsDialogHandler     : boolean;
                                              aCreateLifeSpanHandler     : boolean;
                                              aCreateRenderHandler       : boolean;
                                              aCreateRequestHandler      : boolean;
                                              aCreateDragHandler         : boolean;
                                              aCreateFindHandler         : boolean);
begin
  inherited Create;

  InitializeVars;

  FEvents := Pointer(events);

  if (FEvents <> nil) then
    begin
      if aCreateLoadHandler        then FLoadHandler        := TCustomLoadHandler.Create(FEvents);
      if aCreateFocusHandler       then FFocusHandler       := TCustomFocusHandler.Create(FEvents);
      if aCreateContextMenuHandler then FContextMenuHandler := TCustomContextMenuHandler.Create(FEvents);
      if aCreateDialogHandler      then FDialogHandler      := TCustomDialogHandler.Create(FEvents);
      if aCreateKeyboardHandler    then FKeyboardHandler    := TCustomKeyboardHandler.Create(FEvents);
      if aCreateDisplayHandler     then FDisplayHandler     := TCustomDisplayHandler.Create(FEvents);
      if aCreateDownloadHandler    then FDownloadHandler    := TCustomDownloadHandler.Create(FEvents);
      if aCreateJsDialogHandler    then FJsDialogHandler    := TCustomJsDialogHandler.Create(FEvents);
      if aCreateLifeSpanHandler    then FLifeSpanHandler    := TCustomLifeSpanHandler.Create(FEvents);
      if aCreateRenderHandler      then FRenderHandler      := TCustomRenderHandler.Create(FEvents);
      if aCreateRequestHandler     then FRequestHandler     := TCustomRequestHandler.Create(FEvents);
      if aCreateDragHandler        then FDragHandler        := TCustomDragHandler.Create(FEvents);
      if aCreateFindHandler        then FFindHandler        := TCustomFindHandler.Create(FEvents);
    end;
end;

procedure TCustomClientHandler.BeforeDestruction;
begin
  InitializeVars;

  inherited BeforeDestruction;
end;

procedure TCustomClientHandler.RemoveReferences;
begin
  FEvents := nil;

  if (FLoadHandler        <> nil) then FLoadHandler.RemoveReferences;
  if (FFocusHandler       <> nil) then FFocusHandler.RemoveReferences;
  if (FContextMenuHandler <> nil) then FContextMenuHandler.RemoveReferences;
  if (FDialogHandler      <> nil) then FDialogHandler.RemoveReferences;
  if (FKeyboardHandler    <> nil) then FKeyboardHandler.RemoveReferences;
  if (FDisplayHandler     <> nil) then FDisplayHandler.RemoveReferences;
  if (FDownloadHandler    <> nil) then FDownloadHandler.RemoveReferences;
  if (FJsDialogHandler    <> nil) then FJsDialogHandler.RemoveReferences;
  if (FLifeSpanHandler    <> nil) then FLifeSpanHandler.RemoveReferences;
  if (FRequestHandler     <> nil) then FRequestHandler.RemoveReferences;
  if (FRenderHandler      <> nil) then FRenderHandler.RemoveReferences;
  if (FDragHandler        <> nil) then FDragHandler.RemoveReferences;
  if (FFindHandler        <> nil) then FFindHandler.RemoveReferences;
end;

procedure TCustomClientHandler.InitializeVars;
begin
  FLoadHandler        := nil;
  FFocusHandler       := nil;
  FContextMenuHandler := nil;
  FDialogHandler      := nil;
  FKeyboardHandler    := nil;
  FDisplayHandler     := nil;
  FDownloadHandler    := nil;
  FJsDialogHandler    := nil;
  FLifeSpanHandler    := nil;
  FRequestHandler     := nil;
  FRenderHandler      := nil;
  FDragHandler        := nil;
  FFindHandler        := nil;
  FEvents             := nil;
end;

procedure TCustomClientHandler.GetContextMenuHandler(var aHandler : ICefContextMenuHandler);
begin
  if (FContextMenuHandler <> nil) then
    aHandler := FContextMenuHandler
   else
    aHandler := nil;
end;

procedure TCustomClientHandler.GetDialogHandler(var aHandler : ICefDialogHandler);
begin
  if (FDialogHandler <> nil) then
    aHandler := FDialogHandler
   else
    aHandler := nil;
end;

procedure TCustomClientHandler.GetDisplayHandler(var aHandler : ICefDisplayHandler);
begin
  if (FDisplayHandler <> nil) then
    aHandler := FDisplayHandler
   else
    aHandler := nil;
end;

procedure TCustomClientHandler.GetDownloadHandler(var aHandler : ICefDownloadHandler);
begin
  if (FDownloadHandler <> nil) then
    aHandler := FDownloadHandler
   else
    aHandler := nil;
end;

procedure TCustomClientHandler.GetDragHandler(var aHandler : ICefDragHandler);
begin
  if (FDragHandler <> nil) then
    aHandler := FDragHandler
   else
    aHandler := nil;
end;

procedure TCustomClientHandler.GetFindHandler(var aHandler : ICefFindHandler);
begin
  if (FFindHandler <> nil) then
    aHandler := FFindHandler
   else
    aHandler := nil;
end;

procedure TCustomClientHandler.GetFocusHandler(var aHandler : ICefFocusHandler);
begin
  if (FFocusHandler <> nil) then
    aHandler := FFocusHandler
   else
    aHandler := nil;
end;

procedure TCustomClientHandler.GetJsdialogHandler(var aHandler : ICefJsDialogHandler);
begin
  if (FJsDialogHandler <> nil) then
    aHandler := FJsDialogHandler
   else
    aHandler := nil;
end;

procedure TCustomClientHandler.GetKeyboardHandler(var aHandler : ICefKeyboardHandler);
begin
  if (FKeyboardHandler <> nil) then
    aHandler := FKeyboardHandler
   else
    aHandler := nil;
end;

procedure TCustomClientHandler.GetLifeSpanHandler(var aHandler : ICefLifeSpanHandler);
begin
  if (FLifeSpanHandler <> nil) then
    aHandler := FLifeSpanHandler
   else
    aHandler := nil;
end;

procedure TCustomClientHandler.GetLoadHandler(var aHandler : ICefLoadHandler);
begin
  if (FLoadHandler <> nil) then
    aHandler := FLoadHandler
   else
    aHandler := nil;
end;

procedure TCustomClientHandler.GetRenderHandler(var aHandler : ICefRenderHandler);
begin
  if (FRenderHandler <> nil) then
    aHandler := FRenderHandler
   else
    aHandler := nil;
end;

procedure TCustomClientHandler.GetRequestHandler(var aHandler : ICefRequestHandler);
begin
  if (FRequestHandler <> nil) then
    aHandler := FRequestHandler
   else
    aHandler := nil;
end;

function TCustomClientHandler.OnProcessMessageReceived(const browser       : ICefBrowser;
                                                             sourceProcess : TCefProcessId;
                                                       const message_      : ICefProcessMessage): Boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnProcessMessageReceived(browser, sourceProcess, message_)
   else
    Result := inherited OnProcessMessageReceived(browser, sourceProcess, message_);
end;

end.
