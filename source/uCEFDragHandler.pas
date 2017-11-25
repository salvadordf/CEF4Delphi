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

unit uCEFDragHandler;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefDragHandlerOwn = class(TCefBaseRefCountedOwn, ICefDragHandler)
    protected
      function OnDragEnter(const browser: ICefBrowser; const dragData: ICefDragData; mask: TCefDragOperations): Boolean; virtual;
      procedure OnDraggableRegionsChanged(const browser: ICefBrowser; regionsCount: NativeUInt; regions: PCefDraggableRegionArray); virtual;

    public
      constructor Create; virtual;
  end;

  TCustomDragHandler = class(TCefDragHandlerOwn)
    protected
      FEvent: IChromiumEvents;

      function  OnDragEnter(const browser: ICefBrowser;  const dragData: ICefDragData; mask: TCefDragOperations): Boolean; override;
      procedure OnDraggableRegionsChanged(const browser: ICefBrowser; regionsCount: NativeUInt; regions: PCefDraggableRegionArray); override;

    public
      constructor Create(const events: IChromiumEvents); reintroduce; virtual;
      destructor  Destroy; override;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBrowser, uCEFDragData;

function cef_drag_handler_on_drag_enter(self: PCefDragHandler; browser: PCefBrowser;
  dragData: PCefDragData; mask: TCefDragOperations): Integer; stdcall;
begin
  with TCefDragHandlerOwn(CefGetObject(self)) do
    Result := Ord(OnDragEnter(TCefBrowserRef.UnWrap(browser), TCefDragDataRef.UnWrap(dragData), mask));
end;

procedure cef_drag_handler_on_draggable_regions_changed(self: PCefDragHandler;
  browser: PCefBrowser; regionsCount: NativeUInt; regions: PCefDraggableRegionArray); stdcall;
begin
  with TCefDragHandlerOwn(CefGetObject(self)) do
    OnDraggableRegionsChanged(TCefBrowserRef.UnWrap(browser), regionsCount, regions);
end;

constructor TCefDragHandlerOwn.Create;
begin
  CreateData(SizeOf(TCefDragHandler));
  with PCefDragHandler(FData)^ do
  begin
    on_drag_enter := cef_drag_handler_on_drag_enter;
    on_draggable_regions_changed := cef_drag_handler_on_draggable_regions_changed;
  end;
end;

function TCefDragHandlerOwn.OnDragEnter(const browser: ICefBrowser;
  const dragData: ICefDragData; mask: TCefDragOperations): Boolean;
begin
  Result := False;
end;

procedure TCefDragHandlerOwn.OnDraggableRegionsChanged(
  const browser: ICefBrowser; regionsCount: NativeUInt;
  regions: PCefDraggableRegionArray);
begin

end;

// TCustomDragHandler

constructor TCustomDragHandler.Create(const events: IChromiumEvents);
begin
  inherited Create;

  FEvent := events;
end;

destructor TCustomDragHandler.Destroy;
begin
  FEvent := nil;

  inherited Destroy;
end;

function TCustomDragHandler.OnDragEnter(const browser: ICefBrowser; const dragData: ICefDragData; mask: TCefDragOperations): Boolean;
begin
  if (FEvent <> nil) then
    Result := FEvent.doOnDragEnter(browser, dragData, mask)
   else
    Result := inherited OnDragEnter(browser, dragData, mask);
end;

procedure TCustomDragHandler.OnDraggableRegionsChanged(const browser: ICefBrowser; regionsCount: NativeUInt; regions: PCefDraggableRegionArray);
begin
  if (FEvent <> nil) then
    FEvent.doOnDraggableRegionsChanged(browser, regionsCount, regions);
end;

end.
