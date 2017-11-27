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

unit uBufferPanel;

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  Winapi.Windows, Winapi.Messages, System.Classes, Vcl.ExtCtrls, Vcl.Controls,
  Vcl.Graphics, System.SyncObjs, System.SysUtils;
  {$ELSE}
  Windows, Messages, Classes, Controls,
  ExtCtrls, Graphics, SyncObjs, SysUtils;
  {$ENDIF}

type
  TBufferPanel = class(TCustomPanel)
    protected
      FMutex          : THandle;
      FBuffer         : TBitmap;
      FScanlineSize   : integer;

      function  GetBufferBits : pointer;
      function  GetBufferWidth : integer;
      function  GetBufferHeight : integer;

      function  CopyBuffer(aDC : HDC; const aRect : TRect) : boolean;
      function  SaveBufferToFile(const aFilename : string) : boolean;
      procedure DestroyBuffer;

      procedure WMPaint(var aMessage: TWMPaint); message WM_PAINT;
      procedure WMEraseBkgnd(var aMessage : TWMEraseBkgnd); message WM_ERASEBKGND;

    public
      constructor Create(AOwner: TComponent); override;
      destructor  Destroy; override;
      procedure   AfterConstruction; override;
      function    SaveToFile(const aFilename : string) : boolean;
      procedure   InvalidatePanel;
      function    BeginBufferDraw : boolean;
      procedure   EndBufferDraw;
      procedure   BufferDraw(x, y : integer; const aBitmap : TBitmap);
      function    UpdateBufferDimensions(aWidth, aHeight : integer) : boolean;
      function    BufferIsResized(aUseMutex : boolean = True) : boolean;

      property Buffer         : TBitmap            read FBuffer;
      property ScanlineSize   : integer            read FScanlineSize;
      property BufferWidth    : integer            read GetBufferWidth;
      property BufferHeight   : integer            read GetBufferHeight;
      property BufferBits     : pointer            read GetBufferBits;

      property DockManager;

    published
      property Align;
      property Alignment;
      property Anchors;
      property AutoSize;
      property BevelEdges;
      property BevelInner;
      property BevelKind;
      property BevelOuter;
      property BevelWidth;
      property BiDiMode;
      property BorderWidth;
      property BorderStyle;
      property Caption;
      property Color;
      property Constraints;
      property Ctl3D;
      property UseDockManager default True;
      property DockSite;
      property DoubleBuffered;
      property DragCursor;
      property DragKind;
      property DragMode;
      property Enabled;
      property FullRepaint;
      property Font;
      property Locked;
      property ParentBiDiMode;
      property ParentBackground;
      property ParentColor;
      property ParentCtl3D;
      property ParentFont;
      property ParentShowHint;
      property PopupMenu;
      property ShowHint;
      property TabOrder;
      property TabStop;
      property Visible;
      property OnCanResize;
      property OnClick;
      property OnConstrainedResize;
      property OnContextPopup;
      property OnDockDrop;
      property OnDockOver;
      property OnDblClick;
      property OnDragDrop;
      property OnDragOver;
      property OnEndDock;
      property OnEndDrag;
      property OnEnter;
      property OnExit;
      property OnGetSiteInfo;
      property OnMouseDown;
      property OnMouseMove;
      property OnMouseUp;
      property OnResize;
      property OnStartDock;
      property OnStartDrag;
      property OnUnDock;
      {$IFDEF DELPHI9_UP}
      property VerticalAlignment;
      property OnAlignInsertBefore;
      property OnAlignPosition;
      {$ENDIF}
      {$IFDEF DELPHI10_UP}
      property Padding;
      property OnMouseActivate;
      property OnMouseEnter;
      property OnMouseLeave;
      {$ENDIF}
      {$IFDEF DELPHI12_UP}
      property ShowCaption;
      property ParentDoubleBuffered;
      {$ENDIF}
      {$IFDEF DELPHI14_UP}
      property Touch;
      property OnGesture;
      {$ENDIF}
      {$IFDEF DELPHI17_UP}
      property StyleElements;
      {$ENDIF}
  end;

implementation

uses
  uCEFMiscFunctions, uCEFApplication;

constructor TBufferPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FMutex  := 0;
  FBuffer := nil;
end;

destructor TBufferPanel.Destroy;
begin
  DestroyBuffer;

  if (FMutex <> 0) then
    begin
      CloseHandle(FMutex);
      FMutex := 0;
    end;

  inherited Destroy;
end;

procedure TBufferPanel.AfterConstruction;
begin
  inherited AfterConstruction;

  FMutex := CreateMutex(nil, False, nil);
end;

procedure TBufferPanel.DestroyBuffer;
begin
  if BeginBufferDraw then
    begin
      if (FBuffer <> nil) then FreeAndNil(FBuffer);
      EndBufferDraw;
    end;
end;

function TBufferPanel.SaveBufferToFile(const aFilename : string) : boolean;
begin
  Result := False;

  try
    if (FBuffer <> nil) then
      begin
        FBuffer.SaveToFile(aFilename);
        Result := True;
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TBufferPanel.SaveBufferToFile', e) then raise;
  end;
end;

function TBufferPanel.SaveToFile(const aFilename : string) : boolean;
begin
  if BeginBufferDraw then
    begin
      Result := SaveBufferToFile(aFilename);
      EndBufferDraw;
    end
   else
    Result := False;
end;

procedure TBufferPanel.InvalidatePanel;
begin
  PostMessage(Handle, CM_INVALIDATE, 0, 0);
end;

function TBufferPanel.BeginBufferDraw : boolean;
begin
  Result := (FMutex <> 0) and (WaitForSingleObject(FMutex, 5000) = WAIT_OBJECT_0);
end;

procedure TBufferPanel.EndBufferDraw;
begin
  if (FMutex <> 0) then ReleaseMutex(FMutex);
end;

function TBufferPanel.CopyBuffer(aDC : HDC; const aRect : TRect) : boolean;
begin
  Result := False;

  if BeginBufferDraw then
    begin
      Result := (FBuffer <> nil) and
                (aDC     <> 0)   and
                BitBlt(aDC, aRect.Left, aRect.Top, aRect.Right - aRect.Left, aRect.Bottom - aRect.Top,
                       FBuffer.Canvas.Handle, aRect.Left, aRect.Top,
                       SrcCopy);

      EndBufferDraw;
    end;
end;

procedure TBufferPanel.WMPaint(var aMessage: TWMPaint);
var
  TempPaintStruct : TPaintStruct;
  TempDC          : HDC;
begin
  try
    TempDC := BeginPaint(Handle, TempPaintStruct);

    if csDesigning in ComponentState then
      begin
        Canvas.Font.Assign(Font);
        Canvas.Brush.Color := Color;
        Canvas.Brush.Style := bsSolid;
        Canvas.Pen.Style   := psDash;

        Canvas.Rectangle(0, 0, Width, Height);
      end
     else
      if not(CopyBuffer(TempDC, TempPaintStruct.rcPaint)) then
        begin
          Canvas.Brush.Color := Color;
          Canvas.Brush.Style := bsSolid;
          Canvas.FillRect(rect(0, 0, Width, Height));
        end;
  finally
    EndPaint(Handle, TempPaintStruct);
    aMessage.Result := 1;
  end;
end;

procedure TBufferPanel.WMEraseBkgnd(var aMessage : TWMEraseBkgnd);
begin
  aMessage.Result := 1;
end;

function TBufferPanel.GetBufferBits : pointer;
begin
  if (FBuffer <> nil) then
    Result := FBuffer.Scanline[pred(FBuffer.Height)]
   else
    Result := nil;
end;

function TBufferPanel.GetBufferWidth : integer;
begin
  if (FBuffer <> nil) then
    Result := FBuffer.Width
   else
    Result := 0;
end;

function TBufferPanel.GetBufferHeight : integer;
begin
  if (FBuffer <> nil) then
    Result := FBuffer.Height
   else
    Result := 0;
end;

procedure TBufferPanel.BufferDraw(x, y : integer; const aBitmap : TBitmap);
begin
  if (FBuffer <> nil) then FBuffer.Canvas.Draw(x, y, aBitmap);
end;

function TBufferPanel.UpdateBufferDimensions(aWidth, aHeight : integer) : boolean;
begin
  if ((FBuffer        =  nil)      or
      (FBuffer.Width  <> aWidth)   or
      (FBuffer.Height <> aHeight)) then
    begin
      if (FBuffer <> nil) then FreeAndNil(FBuffer);

      FBuffer             := TBitmap.Create;
      FBuffer.PixelFormat := pf32bit;
      FBuffer.HandleType  := bmDIB;
      FBuffer.Width       := aWidth;
      FBuffer.Height      := aHeight;
      FScanlineSize       := FBuffer.Width * SizeOf(TRGBQuad);
      Result              := True;
    end
   else
    Result := False;
end;

function TBufferPanel.BufferIsResized(aUseMutex : boolean) : boolean;
begin
  Result := False;

  if not(aUseMutex) or BeginBufferDraw then
    begin
      // CEF and Chromium use 'floor' to round the float values in Device <-> Logical unit conversions
      // and Delphi uses MulDiv, which uses the bankers rounding, to resize the components in high DPI mode.
      // This is the cause of slight differences in size between the buffer and the panel in some occasions.

      Result := (FBuffer <> nil) and
                (FBuffer.Width  = LogicalToDevice(DeviceToLogical(Width,  GlobalCEFApp.DeviceScaleFactor), GlobalCEFApp.DeviceScaleFactor)) and
                (FBuffer.Height = LogicalToDevice(DeviceToLogical(Height, GlobalCEFApp.DeviceScaleFactor), GlobalCEFApp.DeviceScaleFactor));

      if aUseMutex then EndBufferDraw;
    end;
end;

end.
