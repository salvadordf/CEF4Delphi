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

unit uCEFRenderHandler;

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
  TCefRenderHandlerOwn = class(TCefBaseRefCountedOwn, ICefRenderHandler)
    protected
      procedure GetAccessibilityHandler(var aAccessibilityHandler : ICefAccessibilityHandler); virtual;
      function  GetRootScreenRect(const browser: ICefBrowser; var rect: TCefRect): Boolean; virtual;
      procedure GetViewRect(const browser: ICefBrowser; var rect: TCefRect); virtual;
      function  GetScreenPoint(const browser: ICefBrowser; viewX, viewY: Integer; var screenX, screenY: Integer): Boolean; virtual;
      function  GetScreenInfo(const browser: ICefBrowser; var screenInfo: TCefScreenInfo): Boolean; virtual;
      procedure OnPopupShow(const browser: ICefBrowser; show: Boolean); virtual;
      procedure OnPopupSize(const browser: ICefBrowser; const rect: PCefRect); virtual;
      procedure OnPaint(const browser: ICefBrowser; kind: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; const buffer: Pointer; width, height: Integer); virtual;
      procedure OnAcceleratedPaint(const browser: ICefBrowser; kind: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; shared_handle: Pointer); virtual;
      function  OnStartDragging(const browser: ICefBrowser; const dragData: ICefDragData; allowedOps: TCefDragOperations; x, y: Integer): Boolean; virtual;
      procedure OnUpdateDragCursor(const browser: ICefBrowser; operation: TCefDragOperation); virtual;
      procedure OnScrollOffsetChanged(const browser: ICefBrowser; x, y: Double); virtual;
      procedure OnIMECompositionRangeChanged(const browser: ICefBrowser; const selected_range: PCefRange; character_boundsCount: NativeUInt; const character_bounds: PCefRect); virtual;
      procedure OnTextSelectionChanged(const browser: ICefBrowser; const selected_text: ustring; const selected_range: PCefRange); virtual;
      procedure OnVirtualKeyboardRequested(const browser: ICefBrowser; input_mode: TCefTextInpuMode); virtual;

      procedure RemoveReferences; virtual;

    public
      constructor Create; virtual;
  end;

  TCustomRenderHandler = class(TCefRenderHandlerOwn)
    protected
      FEvents : Pointer;

      procedure GetAccessibilityHandler(var aAccessibilityHandler : ICefAccessibilityHandler); override;
      function  GetRootScreenRect(const browser: ICefBrowser; var rect: TCefRect): Boolean; override;
      procedure GetViewRect(const browser: ICefBrowser; var rect: TCefRect); override;
      function  GetScreenPoint(const browser: ICefBrowser; viewX, viewY: Integer; var screenX, screenY: Integer): Boolean; override;
      procedure OnPopupShow(const browser: ICefBrowser; show: Boolean); override;
      procedure OnPopupSize(const browser: ICefBrowser; const rect: PCefRect); override;
      procedure OnPaint(const browser: ICefBrowser; kind: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; const buffer: Pointer; width, height: Integer); override;
      procedure OnAcceleratedPaint(const browser: ICefBrowser; kind: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; shared_handle: Pointer); override;
      function  GetScreenInfo(const browser: ICefBrowser; var screenInfo: TCefScreenInfo): Boolean; override;
      function  OnStartDragging(const browser: ICefBrowser; const dragData: ICefDragData; allowedOps: TCefDragOperations; x, y: Integer): Boolean; override;
      procedure OnUpdateDragCursor(const browser: ICefBrowser; operation: TCefDragOperation); override;
      procedure OnScrollOffsetChanged(const browser: ICefBrowser; x, y: Double); override;
      procedure OnIMECompositionRangeChanged(const browser: ICefBrowser; const selected_range: PCefRange; character_boundsCount: NativeUInt; const character_bounds: PCefRect); override;
      procedure OnTextSelectionChanged(const browser: ICefBrowser; const selected_text: ustring; const selected_range: PCefRange); override;
      procedure OnVirtualKeyboardRequested(const browser: ICefBrowser; input_mode: TCefTextInpuMode); override;

      procedure RemoveReferences; override;

    public
      constructor Create(const events : IChromiumEvents); reintroduce; virtual;
      destructor  Destroy; override;
  end;

implementation

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBrowser, uCEFDragData;


function cef_render_handler_get_accessibility_handler(self: PCefRenderHandler): PCefAccessibilityHandler; stdcall;
var
  TempHandler : ICefAccessibilityHandler;
  TempObject  : TObject;
begin
  Result      := nil;
  TempHandler := nil;
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefRenderHandlerOwn) then
    begin
      TCefRenderHandlerOwn(TempObject).GetAccessibilityHandler(TempHandler);

      if (TempHandler <> nil) then Result := TempHandler.Wrap;
    end;
end;

function cef_render_handler_get_root_screen_rect(self    : PCefRenderHandler;
                                                 browser : PCefBrowser;
                                                 rect    : PCefRect): Integer; stdcall;
var
  TempObject : TObject;
begin
  Result     := Ord(False);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefRenderHandlerOwn) then
    Result := Ord(TCefRenderHandlerOwn(TempObject).GetRootScreenRect(TCefBrowserRef.UnWrap(browser),
                                                                     rect^));
end;

procedure cef_render_handler_get_view_rect(self    : PCefRenderHandler;
                                           browser : PCefBrowser;
                                           rect    : PCefRect); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefRenderHandlerOwn) then
    TCefRenderHandlerOwn(TempObject).GetViewRect(TCefBrowserRef.UnWrap(browser),
                                                 rect^);

  // CEF needs a rect with valid height and width
  if (rect^.width  <= 0) then rect^.width  := 800;
  if (rect^.height <= 0) then rect^.height := 600;
end;

function cef_render_handler_get_screen_point(self             : PCefRenderHandler;
                                             browser          : PCefBrowser;
                                             viewX, viewY     : Integer;
                                             screenX, screenY : PInteger): Integer; stdcall;
var
  TempObject : TObject;
begin
  Result     := Ord(False);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefRenderHandlerOwn) then
    Result := Ord(TCefRenderHandlerOwn(TempObject).GetScreenPoint(TCefBrowserRef.UnWrap(browser),
                                                                  viewX,
                                                                  viewY,
                                                                  screenX^,
                                                                  screenY^));
end;

function cef_render_handler_get_screen_info(self        : PCefRenderHandler;
                                            browser     : PCefBrowser;
                                            screen_info : PCefScreenInfo): Integer; stdcall;
var
  TempObject : TObject;
begin
  Result     := Ord(False);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefRenderHandlerOwn) then
    Result := Ord(TCefRenderHandlerOwn(TempObject).GetScreenInfo(TCefBrowserRef.UnWrap(browser),
                                                                 screen_info^));
end;

procedure cef_render_handler_on_popup_show(self    : PCefRenderHandler;
                                           browser : PCefBrowser;
                                           show    : Integer); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefRenderHandlerOwn) then
    TCefRenderHandlerOwn(TempObject).OnPopupShow(TCefBrowserRef.UnWrap(browser),
                                                 show <> 0);
end;

procedure cef_render_handler_on_popup_size(      self    : PCefRenderHandler;
                                                 browser : PCefBrowser;
                                           const rect    : PCefRect); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefRenderHandlerOwn) then
    TCefRenderHandlerOwn(TempObject).OnPopupSize(TCefBrowserRef.UnWrap(browser),
                                                 rect);
end;

procedure cef_render_handler_on_paint(      self             : PCefRenderHandler;
                                            browser          : PCefBrowser;
                                            kind             : TCefPaintElementType;
                                            dirtyRectsCount  : NativeUInt;
                                      const dirtyRects       : PCefRectArray;
                                      const buffer           : Pointer;
                                            width            : Integer;
                                            height           : Integer); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefRenderHandlerOwn) then
    TCefRenderHandlerOwn(TempObject).OnPaint(TCefBrowserRef.UnWrap(browser),
                                             kind,
                                             dirtyRectsCount,
                                             dirtyRects,
                                             buffer,
                                             width,
                                             height);
end;

procedure cef_render_handler_on_accelerated_paint(      self             : PCefRenderHandler;
                                                        browser          : PCefBrowser;
                                                        kind             : TCefPaintElementType;
                                                        dirtyRectsCount  : NativeUInt;
                                                  const dirtyRects       : PCefRectArray;
                                                        shared_handle    : Pointer); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefRenderHandlerOwn) then
    TCefRenderHandlerOwn(TempObject).OnAcceleratedPaint(TCefBrowserRef.UnWrap(browser),
                                                        kind,
                                                        dirtyRectsCount,
                                                        dirtyRects,
                                                        shared_handle);
end;

function cef_render_handler_start_dragging(self        : PCefRenderHandler;
                                           browser     : PCefBrowser;
                                           drag_data   : PCefDragData;
                                           allowed_ops : TCefDragOperations;
                                           x           : Integer;
                                           y           : Integer): Integer; stdcall;
var
  TempObject : TObject;
begin
  Result     := Ord(False);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefRenderHandlerOwn) then
    Result := Ord(TCefRenderHandlerOwn(TempObject).OnStartDragging(TCefBrowserRef.UnWrap(browser),
                                                                   TCefDragDataRef.UnWrap(drag_data),
                                                                   allowed_ops,
                                                                   x,
                                                                   y));
end;

procedure cef_render_handler_update_drag_cursor(self      : PCefRenderHandler;
                                                browser   : PCefBrowser;
                                                operation : TCefDragOperation); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefRenderHandlerOwn) then
    TCefRenderHandlerOwn(TempObject).OnUpdateDragCursor(TCefBrowserRef.UnWrap(browser), operation);
end;

procedure cef_render_handler_on_scroll_offset_changed(self    : PCefRenderHandler;
                                                      browser : PCefBrowser;
                                                      x       : Double;
                                                      y       : Double); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefRenderHandlerOwn) then
    TCefRenderHandlerOwn(TempObject).OnScrollOffsetChanged(TCefBrowserRef.UnWrap(browser),
                                                           x,
                                                           y);
end;

procedure cef_render_handler_on_ime_composition_range_changed(      self                  : PCefRenderHandler;
                                                                    browser               : PCefBrowser;
                                                              const selected_range        : PCefRange;
                                                                    character_boundsCount : NativeUInt;
                                                              const character_bounds      : PCefRect); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefRenderHandlerOwn) then
    TCefRenderHandlerOwn(TempObject).OnIMECompositionRangeChanged(TCefBrowserRef.UnWrap(browser),
                                                                  selected_range,
                                                                  character_boundsCount,
                                                                  character_bounds);
end;

procedure cef_render_handler_on_text_selection_changed(      self           : PCefRenderHandler;
                                                             browser        : PCefBrowser;
                                                       const selected_text  : PCefString;
                                                       const selected_range : PCefRange); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefRenderHandlerOwn) then
    TCefRenderHandlerOwn(TempObject).OnTextSelectionChanged(TCefBrowserRef.UnWrap(browser),
                                                            CefString(selected_text),
                                                            selected_range);
end;

procedure cef_render_handler_on_virtual_keyboard_requested(self       : PCefRenderHandler;
                                                           browser    : PCefBrowser;
                                                           input_mode : TCefTextInpuMode); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefRenderHandlerOwn) then
    TCefRenderHandlerOwn(TempObject).OnVirtualKeyboardRequested(TCefBrowserRef.UnWrap(browser),
                                                                input_mode);
end;

constructor TCefRenderHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefRenderHandler));

  with PCefRenderHandler(FData)^ do
    begin
      get_accessibility_handler        := {$IFDEF FPC}@{$ENDIF}cef_render_handler_get_accessibility_handler;
      get_root_screen_rect             := {$IFDEF FPC}@{$ENDIF}cef_render_handler_get_root_screen_rect;
      get_view_rect                    := {$IFDEF FPC}@{$ENDIF}cef_render_handler_get_view_rect;
      get_screen_point                 := {$IFDEF FPC}@{$ENDIF}cef_render_handler_get_screen_point;
      get_screen_info                  := {$IFDEF FPC}@{$ENDIF}cef_render_handler_get_screen_info;
      on_popup_show                    := {$IFDEF FPC}@{$ENDIF}cef_render_handler_on_popup_show;
      on_popup_size                    := {$IFDEF FPC}@{$ENDIF}cef_render_handler_on_popup_size;
      on_paint                         := {$IFDEF FPC}@{$ENDIF}cef_render_handler_on_paint;
      on_accelerated_paint             := {$IFDEF FPC}@{$ENDIF}cef_render_handler_on_accelerated_paint;
      start_dragging                   := {$IFDEF FPC}@{$ENDIF}cef_render_handler_start_dragging;
      update_drag_cursor               := {$IFDEF FPC}@{$ENDIF}cef_render_handler_update_drag_cursor;
      on_scroll_offset_changed         := {$IFDEF FPC}@{$ENDIF}cef_render_handler_on_scroll_offset_changed;
      on_ime_composition_range_changed := {$IFDEF FPC}@{$ENDIF}cef_render_handler_on_ime_composition_range_changed;
      on_text_selection_changed        := {$IFDEF FPC}@{$ENDIF}cef_render_handler_on_text_selection_changed;
      on_virtual_keyboard_requested    := {$IFDEF FPC}@{$ENDIF}cef_render_handler_on_virtual_keyboard_requested;
    end;
end;

procedure TCefRenderHandlerOwn.GetAccessibilityHandler(var aAccessibilityHandler : ICefAccessibilityHandler);
begin
  aAccessibilityHandler := nil;
end;

function TCefRenderHandlerOwn.GetRootScreenRect(const browser: ICefBrowser; var rect: TCefRect): Boolean;
begin
  Result := False;
end;

function TCefRenderHandlerOwn.GetScreenInfo(const browser: ICefBrowser; var screenInfo: TCefScreenInfo): Boolean;
begin
  Result := False;
end;

function TCefRenderHandlerOwn.GetScreenPoint(const browser: ICefBrowser; viewX, viewY: Integer; var screenX, screenY: Integer): Boolean;
begin
  Result := False;
end;

procedure TCefRenderHandlerOwn.GetViewRect(const browser: ICefBrowser; var rect: TCefRect);
begin
  //
end;

procedure TCefRenderHandlerOwn.OnPaint(const browser: ICefBrowser; kind: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; const buffer: Pointer; width, height: Integer);
begin
  //
end;

procedure TCefRenderHandlerOwn.OnAcceleratedPaint(const browser: ICefBrowser; kind: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; shared_handle: Pointer);
begin
  //
end;

procedure TCefRenderHandlerOwn.OnPopupShow(const browser: ICefBrowser; show: Boolean);
begin
  //
end;

procedure TCefRenderHandlerOwn.OnPopupSize(const browser: ICefBrowser; const rect: PCefRect);
begin
  //
end;

procedure TCefRenderHandlerOwn.OnScrollOffsetChanged(const browser: ICefBrowser; x, y: Double);
begin
  //
end;

procedure TCefRenderHandlerOwn.OnIMECompositionRangeChanged(const browser               : ICefBrowser;
                                                            const selected_range        : PCefRange;
                                                                  character_boundsCount : NativeUInt;
                                                            const character_bounds      : PCefRect);
begin
  //
end;

procedure TCefRenderHandlerOwn.OnTextSelectionChanged(const browser        : ICefBrowser;
                                                      const selected_text  : ustring;
                                                      const selected_range : PCefRange);
begin
  //
end;

procedure TCefRenderHandlerOwn.OnVirtualKeyboardRequested(const browser    : ICefBrowser;
                                                                input_mode : TCefTextInpuMode);
begin
  //
end;

function TCefRenderHandlerOwn.OnStartDragging(const browser: ICefBrowser; const dragData: ICefDragData; allowedOps: TCefDragOperations; x, y: Integer): Boolean;
begin
  Result := False;
end;

procedure TCefRenderHandlerOwn.OnUpdateDragCursor(const browser: ICefBrowser; operation: TCefDragOperation);
begin
  //
end;

procedure TCefRenderHandlerOwn.RemoveReferences;
begin
  //
end;

// TCustomRenderHandler

constructor TCustomRenderHandler.Create(const events : IChromiumEvents);
begin
  inherited Create;

  FEvents := Pointer(events);
end;

destructor TCustomRenderHandler.Destroy;
begin
  RemoveReferences;

  inherited Destroy;
end;

procedure TCustomRenderHandler.RemoveReferences;
begin
  FEvents := nil;
end;

procedure TCustomRenderHandler.GetAccessibilityHandler(var aAccessibilityHandler : ICefAccessibilityHandler);
begin
  if (FEvents <> nil) then IChromiumEvents(FEvents).doOnGetAccessibilityHandler(aAccessibilityHandler);
end;

function TCustomRenderHandler.GetRootScreenRect(const browser: ICefBrowser; var rect: TCefRect): Boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnGetRootScreenRect(browser, rect)
   else
    Result := inherited GetRootScreenRect(browser, rect);
end;

function TCustomRenderHandler.GetScreenInfo(const browser: ICefBrowser; var screenInfo: TCefScreenInfo): Boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnGetScreenInfo(browser, screenInfo)
   else
    Result := inherited GetScreenInfo(browser, screenInfo);
end;

function TCustomRenderHandler.GetScreenPoint(const browser: ICefBrowser; viewX, viewY: Integer; var screenX, screenY: Integer): Boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnGetScreenPoint(browser, viewX, viewY, screenX, screenY)
   else
    Result := inherited GetScreenPoint(browser, viewX, viewY, screenX, screenY);
end;

procedure TCustomRenderHandler.GetViewRect(const browser: ICefBrowser; var rect: TCefRect);
begin
  if (FEvents <> nil) then
    IChromiumEvents(FEvents).doOnGetViewRect(browser, rect)
   else
    inherited GetViewRect(browser, rect);
end;

procedure TCustomRenderHandler.OnPaint(const browser         : ICefBrowser;
                                             kind            : TCefPaintElementType;
                                             dirtyRectsCount : NativeUInt;
                                       const dirtyRects      : PCefRectArray;
                                       const buffer          : Pointer;
                                             width           : Integer;
                                             height          : Integer);
begin
  if (FEvents <> nil) then IChromiumEvents(FEvents).doOnPaint(browser, kind, dirtyRectsCount, dirtyRects, buffer, width, height);
end;

procedure TCustomRenderHandler.OnAcceleratedPaint(const browser         : ICefBrowser;
                                                        kind            : TCefPaintElementType;
                                                        dirtyRectsCount : NativeUInt;
                                                  const dirtyRects      : PCefRectArray;
                                                        shared_handle   : Pointer);
begin
  if (FEvents <> nil) then IChromiumEvents(FEvents).doOnAcceleratedPaint(browser, kind, dirtyRectsCount, dirtyRects, shared_handle);
end;

procedure TCustomRenderHandler.OnPopupShow(const browser: ICefBrowser; show: Boolean);
begin
  if (FEvents <> nil) then IChromiumEvents(FEvents).doOnPopupShow(browser, show);
end;

procedure TCustomRenderHandler.OnPopupSize(const browser: ICefBrowser; const rect: PCefRect);
begin
  if (FEvents <> nil) then IChromiumEvents(FEvents).doOnPopupSize(browser, rect);
end;

procedure TCustomRenderHandler.OnScrollOffsetChanged(const browser: ICefBrowser; x, y: Double);
begin
  if (FEvents <> nil) then IChromiumEvents(FEvents).doOnScrollOffsetChanged(browser, x, y);
end;

procedure TCustomRenderHandler.OnIMECompositionRangeChanged(const browser               : ICefBrowser;
                                                            const selected_range        : PCefRange;
                                                                  character_boundsCount : NativeUInt;
                                                            const character_bounds      : PCefRect);
begin
  if (FEvents <> nil) then IChromiumEvents(FEvents).doOnIMECompositionRangeChanged(browser, selected_range, character_boundsCount, character_bounds);
end;

procedure TCustomRenderHandler.OnTextSelectionChanged(const browser        : ICefBrowser;
                                                      const selected_text  : ustring;
                                                      const selected_range : PCefRange);
begin
  if (FEvents <> nil) then IChromiumEvents(FEvents).doOnTextSelectionChanged(browser, selected_text, selected_range);
end;

procedure TCustomRenderHandler.OnVirtualKeyboardRequested(const browser    : ICefBrowser;
                                                                input_mode : TCefTextInpuMode);
begin
  if (FEvents <> nil) then IChromiumEvents(FEvents).doOnVirtualKeyboardRequested(browser, input_mode);
end;

function TCustomRenderHandler.OnStartDragging(const browser    : ICefBrowser;
                                              const dragData   : ICefDragData;
                                                    allowedOps : TCefDragOperations;
                                                    x          : Integer;
                                                    y          : Integer): Boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnStartDragging(browser, dragData, allowedOps, x, y)
   else
    Result := inherited OnStartDragging(browser, dragData, allowedOps, x, y);
end;

procedure TCustomRenderHandler.OnUpdateDragCursor(const browser: ICefBrowser; operation: TCefDragOperation);
begin
  if (FEvents <> nil) then IChromiumEvents(FEvents).doOnUpdateDragCursor(browser, operation);
end;

end.
