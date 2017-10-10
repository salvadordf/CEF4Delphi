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
//        Copyright © 2017 Salvador Díaz Fau. All rights reserved.
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

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefRenderHandlerOwn = class(TCefBaseRefCountedOwn, ICefRenderHandler)
    protected
      procedure GetAccessibilityHandler(var aAccessibilityHandler : ICefAccessibilityHandler); virtual;
      function  GetRootScreenRect(const browser: ICefBrowser; var rect: TCefRect): Boolean; virtual;
      function  GetViewRect(const browser: ICefBrowser; var rect: TCefRect): Boolean; virtual;
      function  GetScreenPoint(const browser: ICefBrowser; viewX, viewY: Integer; var screenX, screenY: Integer): Boolean; virtual;
      function  GetScreenInfo(const browser: ICefBrowser; var screenInfo: TCefScreenInfo): Boolean; virtual;
      procedure OnPopupShow(const browser: ICefBrowser; show: Boolean); virtual;
      procedure OnPopupSize(const browser: ICefBrowser; const rect: PCefRect); virtual;
      procedure OnPaint(const browser: ICefBrowser; kind: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; const buffer: Pointer; width, height: Integer); virtual;
      procedure OnCursorChange(const browser: ICefBrowser; cursor: TCefCursorHandle; CursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo); virtual;
      function  OnStartDragging(const browser: ICefBrowser; const dragData: ICefDragData; allowedOps: TCefDragOperations; x, y: Integer): Boolean; virtual;
      procedure OnUpdateDragCursor(const browser: ICefBrowser; operation: TCefDragOperation); virtual;
      procedure OnScrollOffsetChanged(const browser: ICefBrowser; x, y: Double); virtual;
      procedure OnIMECompositionRangeChanged(const browser: ICefBrowser; const selected_range: PCefRange; character_boundsCount: NativeUInt; const character_bounds: PCefRect); virtual;

    public
      constructor Create; virtual;
  end;

  TCustomRenderHandler = class(TCefRenderHandlerOwn)
    protected
      FEvent: IChromiumEvents;

      procedure GetAccessibilityHandler(var aAccessibilityHandler : ICefAccessibilityHandler); override;
      function  GetRootScreenRect(const browser: ICefBrowser; var rect: TCefRect): Boolean; override;
      function  GetViewRect(const browser: ICefBrowser; var rect: TCefRect): Boolean; override;
      function  GetScreenPoint(const browser: ICefBrowser; viewX, viewY: Integer; var screenX, screenY: Integer): Boolean; override;
      procedure OnPopupShow(const browser: ICefBrowser; show: Boolean); override;
      procedure OnPopupSize(const browser: ICefBrowser; const rect: PCefRect); override;
      procedure OnPaint(const browser: ICefBrowser; kind: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; const buffer: Pointer; width, height: Integer); override;
      procedure OnCursorChange(const browser: ICefBrowser; cursor: TCefCursorHandle; cursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo); override;
      function  GetScreenInfo(const browser: ICefBrowser; var screenInfo: TCefScreenInfo): Boolean; override;
      function  OnStartDragging(const browser: ICefBrowser; const dragData: ICefDragData; allowedOps: TCefDragOperations; x, y: Integer): Boolean; override;
      procedure OnUpdateDragCursor(const browser: ICefBrowser; operation: TCefDragOperation); override;
      procedure OnScrollOffsetChanged(const browser: ICefBrowser; x, y: Double); override;
      procedure OnIMECompositionRangeChanged(const browser: ICefBrowser; const selected_range: PCefRange; character_boundsCount: NativeUInt; const character_bounds: PCefRect); override;

    public
      constructor Create(const events: IChromiumEvents); reintroduce; virtual;
      destructor  Destroy; override;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBrowser, uCEFDragData;


function cef_render_handler_get_accessibility_handler(self: PCefRenderHandler): PCefAccessibilityHandler; stdcall;
var
  TempHandler : ICefAccessibilityHandler;
begin
  with TCefRenderHandlerOwn(CefGetObject(self)) do
    begin
      TempHandler := nil;

      GetAccessibilityHandler(TempHandler);

      if (TempHandler <> nil) then
        Result := TempHandler.Wrap
       else
        Result := nil;
    end;
end;

function cef_render_handler_get_root_screen_rect(self: PCefRenderHandler;
  browser: PCefBrowser; rect: PCefRect): Integer; stdcall;
begin
  with TCefRenderHandlerOwn(CefGetObject(self)) do
    Result := Ord(GetRootScreenRect(TCefBrowserRef.UnWrap(browser), rect^));
end;

function cef_render_handler_get_view_rect(self: PCefRenderHandler;
  browser: PCefBrowser; rect: PCefRect): Integer; stdcall;
begin
  with TCefRenderHandlerOwn(CefGetObject(self)) do
    Result := Ord(GetViewRect(TCefBrowserRef.UnWrap(browser), rect^));
end;

function cef_render_handler_get_screen_point(self: PCefRenderHandler;
  browser: PCefBrowser; viewX, viewY: Integer; screenX, screenY: PInteger): Integer; stdcall;
begin
  with TCefRenderHandlerOwn(CefGetObject(self)) do
    Result := Ord(GetScreenPoint(TCefBrowserRef.UnWrap(browser), viewX, viewY, screenX^, screenY^));
end;

function cef_render_handler_get_screen_info(self: PCefRenderHandler;
  browser: PCefBrowser; screen_info: PCefScreenInfo): Integer; stdcall;
begin
  with TCefRenderHandlerOwn(CefGetObject(self)) do
    Result := Ord(GetScreenInfo(TCefBrowserRef.UnWrap(browser), screen_info^));
end;

procedure cef_render_handler_on_popup_show(self: PCefRenderProcessHandler;
  browser: PCefBrowser; show: Integer); stdcall;
begin
  with TCefRenderHandlerOwn(CefGetObject(self)) do
    OnPopupShow(TCefBrowserRef.UnWrap(browser), show <> 0);
end;

procedure cef_render_handler_on_popup_size(self: PCefRenderProcessHandler;
  browser: PCefBrowser; const rect: PCefRect); stdcall;
begin
  with TCefRenderHandlerOwn(CefGetObject(self)) do
    OnPopupSize(TCefBrowserRef.UnWrap(browser), rect);
end;

procedure cef_render_handler_on_paint(self: PCefRenderProcessHandler;
  browser: PCefBrowser; kind: TCefPaintElementType; dirtyRectsCount: NativeUInt;
  const dirtyRects: PCefRectArray; const buffer: Pointer; width, height: Integer); stdcall;
begin
  with TCefRenderHandlerOwn(CefGetObject(self)) do
    OnPaint(TCefBrowserRef.UnWrap(browser), kind, dirtyRectsCount, dirtyRects,
      buffer, width, height);
end;

procedure cef_render_handler_on_cursor_change(self: PCefRenderProcessHandler;
  browser: PCefBrowser; cursor: TCefCursorHandle; type_: TCefCursorType;
  const custom_cursor_info: PCefCursorInfo); stdcall;
begin
  with TCefRenderHandlerOwn(CefGetObject(self)) do
    OnCursorChange(TCefBrowserRef.UnWrap(browser), cursor, type_, custom_cursor_info);
end;

function cef_render_handler_start_dragging(self: PCefRenderProcessHandler; browser: PCefBrowser;
  drag_data: PCefDragData; allowed_ops: TCefDragOperations; x, y: Integer): Integer; stdcall;
begin
  with TCefRenderHandlerOwn(CefGetObject(self)) do
    Result := Ord(OnStartDragging(TCefBrowserRef.UnWrap(browser),
      TCefDragDataRef.UnWrap(drag_data), allowed_ops, x, y));
end;

procedure cef_render_handler_update_drag_cursor(self: PCefRenderProcessHandler;
  browser: PCefBrowser; operation: TCefDragOperation); stdcall;
begin
  with TCefRenderHandlerOwn(CefGetObject(self)) do
    OnUpdateDragCursor(TCefBrowserRef.UnWrap(browser), operation);
end;

procedure cef_render_handler_on_scroll_offset_changed(self: PCefRenderProcessHandler;
  browser: PCefBrowser; x, y: Double); stdcall;
begin
  with TCefRenderHandlerOwn(CefGetObject(self)) do
    OnScrollOffsetChanged(TCefBrowserRef.UnWrap(browser), x, y);
end;

procedure cef_render_handler_on_ime_composition_range_changed(self: PCefRenderProcessHandler;
                                                                    browser: PCefBrowser;
                                                              const selected_range: PCefRange;
                                                                    character_boundsCount: NativeUInt;
                                                              const character_bounds: PCefRect); stdcall;
begin
  with TCefRenderHandlerOwn(CefGetObject(self)) do
    OnIMECompositionRangeChanged(TCefBrowserRef.UnWrap(browser), selected_range, character_boundsCount, character_bounds);
end;

constructor TCefRenderHandlerOwn.Create;
begin
  CreateData(SizeOf(TCefRenderHandler), False);

  with PCefRenderHandler(FData)^ do
    begin
      get_accessibility_handler        := cef_render_handler_get_accessibility_handler;
      get_root_screen_rect             := cef_render_handler_get_root_screen_rect;
      get_view_rect                    := cef_render_handler_get_view_rect;
      get_screen_point                 := cef_render_handler_get_screen_point;
      get_screen_info                  := cef_render_handler_get_screen_info;
      on_popup_show                    := cef_render_handler_on_popup_show;
      on_popup_size                    := cef_render_handler_on_popup_size;
      on_paint                         := cef_render_handler_on_paint;
      on_cursor_change                 := cef_render_handler_on_cursor_change;
      start_dragging                   := cef_render_handler_start_dragging;
      update_drag_cursor               := cef_render_handler_update_drag_cursor;
      on_scroll_offset_changed         := cef_render_handler_on_scroll_offset_changed;
      on_ime_composition_range_changed := cef_render_handler_on_ime_composition_range_changed;
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

function TCefRenderHandlerOwn.GetScreenPoint(const browser: ICefBrowser; viewX,
  viewY: Integer; var screenX, screenY: Integer): Boolean;
begin
  Result := False;
end;

function TCefRenderHandlerOwn.GetViewRect(const browser: ICefBrowser; var rect: TCefRect): Boolean;
begin
  Result := False;
end;

procedure TCefRenderHandlerOwn.OnCursorChange(const browser: ICefBrowser;
  cursor: TCefCursorHandle; CursorType: TCefCursorType;
  const customCursorInfo: PCefCursorInfo);
begin

end;

procedure TCefRenderHandlerOwn.OnPaint(const browser: ICefBrowser;
  kind: TCefPaintElementType; dirtyRectsCount: NativeUInt;
  const dirtyRects: PCefRectArray; const buffer: Pointer; width, height: Integer);
begin

end;

procedure TCefRenderHandlerOwn.OnPopupShow(const browser: ICefBrowser;
  show: Boolean);
begin

end;

procedure TCefRenderHandlerOwn.OnPopupSize(const browser: ICefBrowser;
  const rect: PCefRect);
begin

end;

procedure TCefRenderHandlerOwn.OnScrollOffsetChanged(
  const browser: ICefBrowser; x, y: Double);
begin

end;

procedure TCefRenderHandlerOwn.OnIMECompositionRangeChanged(const browser               : ICefBrowser;
                                                            const selected_range        : PCefRange;
                                                                  character_boundsCount : NativeUInt;
                                                            const character_bounds      : PCefRect);
begin

end;

function TCefRenderHandlerOwn.OnStartDragging(const browser: ICefBrowser;
  const dragData: ICefDragData; allowedOps: TCefDragOperations; x,
  y: Integer): Boolean;
begin
  Result := False;
end;

procedure TCefRenderHandlerOwn.OnUpdateDragCursor(const browser: ICefBrowser;
  operation: TCefDragOperation);
begin

end;

// TCustomRenderHandler

constructor TCustomRenderHandler.Create(const events: IChromiumEvents);
begin
  inherited Create;

  FEvent := events;
end;

destructor TCustomRenderHandler.Destroy;
begin
  FEvent := nil;

  inherited Destroy;
end;

procedure TCustomRenderHandler.GetAccessibilityHandler(var aAccessibilityHandler : ICefAccessibilityHandler);
begin
  if (FEvent <> nil) then FEvent.doOnGetAccessibilityHandler(aAccessibilityHandler);
end;

function TCustomRenderHandler.GetRootScreenRect(const browser: ICefBrowser; var rect: TCefRect): Boolean;
begin
  if (FEvent <> nil) then
    Result := FEvent.doOnGetRootScreenRect(browser, rect)
   else
    Result := inherited GetRootScreenRect(browser, rect);
end;

function TCustomRenderHandler.GetScreenInfo(const browser: ICefBrowser; var screenInfo: TCefScreenInfo): Boolean;
begin
  if (FEvent <> nil) then
    Result := FEvent.doOnGetScreenInfo(browser, screenInfo)
   else
    Result := inherited GetScreenInfo(browser, screenInfo);
end;

function TCustomRenderHandler.GetScreenPoint(const browser: ICefBrowser; viewX, viewY: Integer; var screenX, screenY: Integer): Boolean;
begin
  if (FEvent <> nil) then
    Result := FEvent.doOnGetScreenPoint(browser, viewX, viewY, screenX, screenY)
   else
    Result := inherited GetScreenPoint(browser, viewX, viewY, screenX, screenY);
end;

function TCustomRenderHandler.GetViewRect(const browser: ICefBrowser; var rect: TCefRect): Boolean;
begin
  if (FEvent <> nil) then
    Result := FEvent.doOnGetViewRect(browser, rect)
   else
    Result := inherited GetViewRect(browser, rect);
end;

procedure TCustomRenderHandler.OnCursorChange(const browser          : ICefBrowser;
                                                    cursor           : TCefCursorHandle;
                                                    cursorType       : TCefCursorType;
                                              const customCursorInfo : PCefCursorInfo);
begin
  if (FEvent <> nil) then
    FEvent.doOnCursorChange(browser, cursor, cursorType, customCursorInfo);
end;

procedure TCustomRenderHandler.OnPaint(const browser         : ICefBrowser;
                                             kind            : TCefPaintElementType;
                                             dirtyRectsCount : NativeUInt;
                                       const dirtyRects      : PCefRectArray;
                                       const buffer          : Pointer;
                                             width           : Integer;
                                             height          : Integer);
begin
  if (FEvent <> nil) then
    FEvent.doOnPaint(browser, kind, dirtyRectsCount, dirtyRects, buffer, width, height);
end;

procedure TCustomRenderHandler.OnPopupShow(const browser: ICefBrowser; show: Boolean);
begin
  if (FEvent <> nil) then FEvent.doOnPopupShow(browser, show);
end;

procedure TCustomRenderHandler.OnPopupSize(const browser: ICefBrowser; const rect: PCefRect);
begin
  if (FEvent <> nil) then FEvent.doOnPopupSize(browser, rect);
end;

procedure TCustomRenderHandler.OnScrollOffsetChanged(const browser: ICefBrowser; x, y: Double);
begin
  if (FEvent <> nil) then FEvent.doOnScrollOffsetChanged(browser, x, y);
end;

procedure TCustomRenderHandler.OnIMECompositionRangeChanged(const browser               : ICefBrowser;
                                                            const selected_range        : PCefRange;
                                                                  character_boundsCount : NativeUInt;
                                                            const character_bounds      : PCefRect);
begin
  if (FEvent <> nil) then
    FEvent.doOnIMECompositionRangeChanged(browser, selected_range, character_boundsCount, character_bounds);
end;

function TCustomRenderHandler.OnStartDragging(const browser    : ICefBrowser;
                                              const dragData   : ICefDragData;
                                                    allowedOps : TCefDragOperations;
                                                    x          : Integer;
                                                    y          : Integer): Boolean;
begin
  if (FEvent <> nil) then
    Result := FEvent.doOnStartDragging(browser, dragData, allowedOps, x, y)
   else
    Result := inherited OnStartDragging(browser, dragData, allowedOps, x, y);
end;

procedure TCustomRenderHandler.OnUpdateDragCursor(const browser: ICefBrowser; operation: TCefDragOperation);
begin
  if (FEvent <> nil) then FEvent.doOnUpdateDragCursor(browser, operation);
end;

end.
