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
//        Copyright © 2018 Salvador Diaz Fau. All rights reserved.
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

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  {$IFDEF MSWINDOWS}Winapi.Windows, Winapi.Messages, Vcl.ExtCtrls, Vcl.Controls, Vcl.Graphics,{$ENDIF}
  System.Classes, System.SyncObjs, System.SysUtils,
  {$ELSE}
    {$IFDEF MSWINDOWS}Windows,{$ENDIF} Classes, Forms, Controls, Graphics,
    {$IFDEF FPC}
    LCLProc, LCLType, LCLIntf, LResources, LMessages, InterfaceBase,
    {$ELSE}
    Messages,
    {$ENDIF}
    ExtCtrls, SyncObjs, SysUtils,
  {$ENDIF}
  uCEFConstants;

type
  {$IFNDEF FPC}{$IFDEF DELPHI16_UP}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}{$ENDIF}
  TBufferPanel = class(TCustomPanel)
    protected
      FMutex          : THandle;
      FBuffer         : TBitmap;
      FScanlineSize   : integer;

      procedure CreateSyncObj;

      procedure DestroySyncObj;
      procedure DestroyBuffer;

      function  GetBufferBits : pointer;
      function  GetBufferWidth : integer;
      function  GetBufferHeight : integer;

      function  CopyBuffer : boolean;
      function  SaveBufferToFile(const aFilename : string) : boolean;

      procedure Paint; override;

      procedure WMEraseBkgnd(var aMessage : TWMEraseBkgnd); message WM_ERASEBKGND;

    public
      constructor Create(AOwner: TComponent); override;
      destructor  Destroy; override;
      procedure   AfterConstruction; override;
      function    SaveToFile(const aFilename : string) : boolean;
      function    InvalidatePanel : boolean;
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
      {$IFDEF FPC}
      property OnUTF8KeyPress;
      {$ELSE}
      property BevelEdges;
      property BevelKind;
      property Ctl3D;
      property Locked;
      property ParentBackground;
      property ParentCtl3D;
      property OnCanResize;
      {$ENDIF}
      property BevelInner;
      property BevelOuter;
      property BevelWidth;
      property BiDiMode;
      property BorderWidth;
      property BorderStyle;
      property Caption;
      property Color;
      property Constraints;
      property UseDockManager default True;
      property DockSite;
      property DoubleBuffered;
      property DragCursor;
      property DragKind;
      property DragMode;
      property Enabled;
      property FullRepaint;
      property Font;
      property ParentBiDiMode;
      property ParentColor;
      property ParentFont;
      property ParentShowHint;
      property PopupMenu;
      property ShowHint;
      property TabOrder;
      property TabStop;
      property Visible;
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
      property OnMouseWheel;
      property OnKeyDown;
      property OnKeyPress;
      property OnKeyUp;
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

{$IFDEF FPC}
procedure Register;
{$ENDIF}

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
  DestroySyncObj;

  inherited Destroy;
end;

procedure TBufferPanel.AfterConstruction;
begin
  inherited AfterConstruction;

  CreateSyncObj;
end;

procedure TBufferPanel.CreateSyncObj;
begin
  FMutex := CreateMutex(nil, False, nil);
end;

procedure TBufferPanel.DestroySyncObj;
begin
  if (FMutex <> 0) then
    begin
      CloseHandle(FMutex);
      FMutex := 0;
    end;
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
  Result := False;

  if BeginBufferDraw then
    begin
      Result := SaveBufferToFile(aFilename);
      EndBufferDraw;
    end;
end;

function TBufferPanel.InvalidatePanel : boolean;
begin
  Result := HandleAllocated and PostMessage(Handle, CM_INVALIDATE, 0, 0);
end;

function TBufferPanel.BeginBufferDraw : boolean;
begin
  Result := (FMutex <> 0) and (WaitForSingleObject(FMutex, 5000) = WAIT_OBJECT_0);
end;

procedure TBufferPanel.EndBufferDraw;
begin
  if (FMutex <> 0) then ReleaseMutex(FMutex);
end;

function TBufferPanel.CopyBuffer : boolean;
begin
  Result := False;

  if BeginBufferDraw then
    begin
      Result := (FBuffer <> nil) and
                BitBlt(Canvas.Handle, 0, 0, Width, Height,
                       FBuffer.Canvas.Handle, 0, 0,
                       SrcCopy);

      EndBufferDraw;
    end;
end;

procedure TBufferPanel.Paint;
begin
  if csDesigning in ComponentState then
    begin
      Canvas.Font.Assign(Font);
      Canvas.Brush.Color := Color;
      Canvas.Brush.Style := bsSolid;
      Canvas.Pen.Style   := psDash;

      Canvas.Rectangle(0, 0, Width, Height);
    end
   else
    if not(CopyBuffer) then
      begin
        Canvas.Brush.Color := Color;
        Canvas.Brush.Style := bsSolid;
        Canvas.FillRect(rect(0, 0, Width, Height));
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
  Result := False;

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
    end;
end;

function TBufferPanel.BufferIsResized(aUseMutex : boolean) : boolean;
var
  TempDevWidth, TempLogWidth, TempDevHeight, TempLogHeight : integer;
begin
  Result := False;
  if (GlobalCEFApp = nil) then exit;

  if not(aUseMutex) or BeginBufferDraw then
    begin
      if (GlobalCEFApp.DeviceScaleFactor = 1) then
        begin
          Result := (FBuffer <> nil) and
                    (FBuffer.Width  = Width) and
                    (FBuffer.Height = Height);
        end
       else
        begin
          // CEF and Chromium use 'floor' to round the float values in Device <-> Logical unit conversions
          // and Delphi uses MulDiv, which uses the bankers rounding, to resize the components in high DPI mode.
          // This is the cause of slight differences in size between the buffer and the panel in some occasions.

          TempLogWidth  := DeviceToLogical(Width,  GlobalCEFApp.DeviceScaleFactor);
          TempLogHeight := DeviceToLogical(Height, GlobalCEFApp.DeviceScaleFactor);

          TempDevWidth  := LogicalToDevice(TempLogWidth,  GlobalCEFApp.DeviceScaleFactor);
          TempDevHeight := LogicalToDevice(TempLogHeight, GlobalCEFApp.DeviceScaleFactor);

          Result := (FBuffer <> nil) and
                    (FBuffer.Width  = TempDevWidth) and
                    (FBuffer.Height = TempDevHeight);
        end;

      if aUseMutex then EndBufferDraw;
    end;
end;

{$IFDEF FPC}
procedure Register;
begin
  {$I res/tbufferpanel.lrs}
  RegisterComponents('Chromium', [TBufferPanel]);
end;
{$ENDIF}

end.
