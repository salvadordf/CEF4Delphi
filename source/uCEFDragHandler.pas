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

unit uCEFDragHandler;

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
  TCefDragHandlerOwn = class(TCefBaseRefCountedOwn, ICefDragHandler)
    protected
      function  OnDragEnter(const browser: ICefBrowser; const dragData: ICefDragData; mask: TCefDragOperations): Boolean; virtual;
      procedure OnDraggableRegionsChanged(const browser: ICefBrowser; const frame: ICefFrame; regionsCount: NativeUInt; const regions: PCefDraggableRegionArray); virtual;

      procedure RemoveReferences; virtual;

    public
      constructor Create; virtual;
  end;

  TCustomDragHandler = class(TCefDragHandlerOwn)
    protected
      FEvents : Pointer;

      function  OnDragEnter(const browser: ICefBrowser;  const dragData: ICefDragData; mask: TCefDragOperations): Boolean; override;
      procedure OnDraggableRegionsChanged(const browser: ICefBrowser; const frame: ICefFrame; regionsCount: NativeUInt; const regions: PCefDraggableRegionArray); override;

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
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBrowser, uCEFDragData, uCEFFrame;

function cef_drag_handler_on_drag_enter(self     : PCefDragHandler;
                                        browser  : PCefBrowser;
                                        dragData : PCefDragData;
                                        mask     : TCefDragOperations): Integer; stdcall;
var
  TempObject : TObject;
begin
  Result     := Ord(False);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefDragHandlerOwn) then
    Result := Ord(TCefDragHandlerOwn(TempObject).OnDragEnter(TCefBrowserRef.UnWrap(browser),
                                                             TCefDragDataRef.UnWrap(dragData),
                                                             mask));
end;

procedure cef_drag_handler_on_draggable_regions_changed(      self         : PCefDragHandler;
                                                              browser      : PCefBrowser;
                                                              frame        : PCefFrame;
                                                              regionsCount : NativeUInt;
                                                        const regions      : PCefDraggableRegionArray); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefDragHandlerOwn) then
    TCefDragHandlerOwn(TempObject).OnDraggableRegionsChanged(TCefBrowserRef.UnWrap(browser),
                                                             TCefFrameRef.UnWrap(frame),
                                                             regionsCount,
                                                             regions);
end;

constructor TCefDragHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefDragHandler));

  with PCefDragHandler(FData)^ do
    begin
      on_drag_enter                := {$IFDEF FPC}@{$ENDIF}cef_drag_handler_on_drag_enter;
      on_draggable_regions_changed := {$IFDEF FPC}@{$ENDIF}cef_drag_handler_on_draggable_regions_changed;
    end;
end;

function TCefDragHandlerOwn.OnDragEnter(const browser  : ICefBrowser;
                                        const dragData : ICefDragData;
                                              mask     : TCefDragOperations): Boolean;
begin
  Result := False;
end;

procedure TCefDragHandlerOwn.OnDraggableRegionsChanged(const browser      : ICefBrowser;
                                                       const frame        : ICefFrame;
                                                             regionsCount : NativeUInt;
                                                       const regions      : PCefDraggableRegionArray);
begin
  //
end;

procedure TCefDragHandlerOwn.RemoveReferences;
begin
  //
end;

// TCustomDragHandler

constructor TCustomDragHandler.Create(const events : IChromiumEvents);
begin
  inherited Create;

  FEvents := Pointer(events);
end;

destructor TCustomDragHandler.Destroy;
begin
  RemoveReferences;

  inherited Destroy;
end;

procedure TCustomDragHandler.RemoveReferences;
begin
  FEvents := nil;
end;

function TCustomDragHandler.OnDragEnter(const browser  : ICefBrowser;
                                        const dragData : ICefDragData;
                                              mask     : TCefDragOperations): Boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnDragEnter(browser, dragData, mask)
   else
    Result := inherited OnDragEnter(browser, dragData, mask);
end;

procedure TCustomDragHandler.OnDraggableRegionsChanged(const browser      : ICefBrowser;
                                                       const frame        : ICefFrame;
                                                             regionsCount : NativeUInt;
                                                       const regions      : PCefDraggableRegionArray);
begin
  if (FEvents <> nil) then IChromiumEvents(FEvents).doOnDraggableRegionsChanged(browser, frame, regionsCount, regions);
end;

end.
