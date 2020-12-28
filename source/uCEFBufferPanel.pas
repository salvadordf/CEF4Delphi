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
//        Copyright © 2020 Salvador Diaz Fau. All rights reserved.
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

unit uCEFBufferPanel;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  {$IFDEF MSWINDOWS}Winapi.Windows, Winapi.Messages, Vcl.ExtCtrls, Vcl.Controls, Vcl.Graphics, WinApi.Imm, {$ENDIF}
  System.Classes, System.SyncObjs, System.SysUtils, Vcl.Forms,
  {$ELSE}
    {$IFDEF MSWINDOWS}Windows, imm, {$ENDIF} Classes, Forms, Controls, Graphics,
    {$IFDEF FPC}
    LCLProc, LCLType, LCLIntf, LResources, LMessages, InterfaceBase, {$IFDEF MSWINDOWS}Win32Extra,{$ENDIF}
    {$ELSE}
    Messages,
    {$ENDIF}
    ExtCtrls, SyncObjs, SysUtils,
  {$ENDIF}
  {$IFDEF MSWINDOWS}uCEFOSRIMEHandler,{$ENDIF} uCEFConstants, uCEFTypes, uCEFBitmapBitBuffer;

type
  TOnIMECommitTextEvent     = procedure(Sender: TObject; const aText : ustring; const replacement_range : PCefRange; relative_cursor_pos : integer) of object;
  TOnIMESetCompositionEvent = procedure(Sender: TObject; const aText : ustring; const underlines : TCefCompositionUnderlineDynArray; const replacement_range, selection_range : TCefRange) of object;
  {$IFDEF MSWINDOWS}
  TOnHandledMessageEvent    = procedure(Sender: TObject; var aMessage: TMessage; var aHandled : boolean) of object;
  {$ENDIF}

  {$IFNDEF FPC}{$IFDEF DELPHI16_UP}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}{$ENDIF}
  TBufferPanel = class(TCustomPanel)
    protected
      FScanlineSize            : integer;
      FTransparent             : boolean;
      FOnPaintParentBkg        : TNotifyEvent;
      FForcedDeviceScaleFactor : single;
      {$IFDEF MSWINDOWS}
      FBuffer                  : TBitmap;
      FSyncObj                 : THandle;
      FIMEHandler              : TCEFOSRIMEHandler;
      FOnIMECancelComposition  : TNotifyEvent;
      FOnIMECommitText         : TOnIMECommitTextEvent;
      FOnIMESetComposition     : TOnIMESetCompositionEvent;
      FOnCustomTouch           : TOnHandledMessageEvent;
      FOnPointerDown           : TOnHandledMessageEvent;
      FOnPointerUp             : TOnHandledMessageEvent;
      FOnPointerUpdate         : TOnHandledMessageEvent;
      {$ELSE}
      FBuffer                  : TCEFBitmapBitBuffer;   
      FPopupBuffer             : TCEFBitmapBitBuffer;
      FBitmap                  : TBitmap;
      FSyncObj                 : TCriticalSection;
      FPopupScanlineSize       : integer;
      {$ENDIF}

      procedure CreateSyncObj;

      procedure DestroySyncObj;
      procedure DestroyBuffer;

      function  GetBufferBits : pointer;
      function  GetBufferWidth : integer;
      function  GetBufferHeight : integer;
      function  GetScreenScale : single; virtual;
      function  GetRealScreenScale(var aResultScale : single) : boolean; virtual;
      {$IFDEF MSWINDOWS}
      function  GetParentFormHandle : TCefWindowHandle;
      function  GetParentForm : TCustomForm;
      {$ELSE}
      function  GetPopupBufferBits : pointer;
      function  GetPopupBufferWidth : integer;
      function  GetPopupBufferHeight : integer;
      {$ENDIF}

      procedure SetTransparent(aValue : boolean);

      function  CopyBuffer : boolean;
      function  SaveBufferToFile(const aFilename : string) : boolean;

      procedure Paint; override;
      {$IFDEF MSWINDOWS}
      procedure CreateParams(var Params: TCreateParams); override;
      procedure WndProc(var aMessage: TMessage); override;
      procedure WMEraseBkgnd(var aMessage : TWMEraseBkgnd); message WM_ERASEBKGND;
      procedure WMTouch(var aMessage: TMessage); message WM_TOUCH;
      procedure WMPointerDown(var aMessage: TMessage); message WM_POINTERDOWN;
      procedure WMPointerUpdate(var aMessage: TMessage); message WM_POINTERUPDATE;
      procedure WMPointerUp(var aMessage: TMessage); message WM_POINTERUP;
      procedure WMIMEStartComp(var aMessage: TMessage);
      procedure WMIMEEndComp(var aMessage: TMessage);
      procedure WMIMESetContext(var aMessage: TMessage);
      procedure WMIMEComposition(var aMessage: TMessage);
      {$ENDIF}

    public
      constructor Create(AOwner: TComponent); override;
      destructor  Destroy; override;
      procedure   AfterConstruction; override;
      function    SaveToFile(const aFilename : string) : boolean;
      function    InvalidatePanel : boolean;
      function    BeginBufferDraw : boolean;
      procedure   EndBufferDraw;
      procedure   BufferDraw(x, y : integer; const aBitmap : TBitmap); overload;
      procedure   BufferDraw(const aBitmap : TBitmap; const aSrcRect, aDstRect : TRect); overload;
      function    UpdateBufferDimensions(aWidth, aHeight : integer) : boolean;
      function    BufferIsResized(aUseMutex : boolean = True) : boolean;
      procedure   CreateIMEHandler;
      procedure   ChangeCompositionRange(const selection_range : TCefRange; const character_bounds : TCefRectDynArray);
      {$IFNDEF MSWINDOWS}
      procedure   DrawPopupBuffer(const aSrcRect, aDstRect : TRect);
      function    UpdatePopupBufferDimensions(aWidth, aHeight : integer) : boolean;
      {$ENDIF}

      property ScanlineSize              : integer                   read FScanlineSize;
      property BufferWidth               : integer                   read GetBufferWidth;
      property BufferHeight              : integer                   read GetBufferHeight;
      property BufferBits                : pointer                   read GetBufferBits;
      property ScreenScale               : single                    read GetScreenScale;
      property ForcedDeviceScaleFactor   : single                    read FForcedDeviceScaleFactor   write FForcedDeviceScaleFactor;
      {$IFDEF MSWINDOWS}                                                                
      property Buffer                    : TBitmap                   read FBuffer;
      property ParentFormHandle          : TCefWindowHandle          read GetParentFormHandle;
      property ParentForm                : TCustomForm               read GetParentForm;
      {$ELSE}
      property Buffer                    : TCEFBitmapBitBuffer       read FBuffer;
      property PopupBuffer               : TCEFBitmapBitBuffer       read FPopupBuffer;     
      property PopupBufferWidth          : integer                   read GetPopupBufferWidth;
      property PopupBufferHeight         : integer                   read GetPopupBufferHeight;
      property PopupBufferBits           : pointer                   read GetPopupBufferBits;
      property PopupScanlineSize         : integer                   read FPopupScanlineSize;
      {$ENDIF}

      property DockManager;
      property Canvas;

    published
      {$IFDEF MSWINDOWS}
      property OnIMECancelComposition    : TNotifyEvent              read FOnIMECancelComposition    write FOnIMECancelComposition;
      property OnIMECommitText           : TOnIMECommitTextEvent     read FOnIMECommitText           write FOnIMECommitText;
      property OnIMESetComposition       : TOnIMESetCompositionEvent read FOnIMESetComposition       write FOnIMESetComposition;
      property OnCustomTouch             : TOnHandledMessageEvent    read FOnCustomTouch             write FOnCustomTouch;
      property OnPointerDown             : TOnHandledMessageEvent    read FOnPointerDown             write FOnPointerDown;
      property OnPointerUp               : TOnHandledMessageEvent    read FOnPointerUp               write FOnPointerUp;
      property OnPointerUpdate           : TOnHandledMessageEvent    read FOnPointerUpdate           write FOnPointerUpdate;
      {$ENDIF}
      property OnPaintParentBkg          : TNotifyEvent              read FOnPaintParentBkg          write FOnPaintParentBkg;

      property Transparent               : boolean                   read FTransparent               write SetTransparent     default False;

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
  {$IFDEF DELPHI16_UP}
  System.Math,
  {$ELSE}
  Math,
  {$ENDIF}
  uCEFMiscFunctions, uCEFApplicationCore;

constructor TBufferPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FBuffer           := nil;
  FTransparent      := False;
  FOnPaintParentBkg := nil;
  FScanlineSize     := 0;

  if (GlobalCEFApp <> nil) and (GlobalCEFApp.ForcedDeviceScaleFactor <> 0) then
    FForcedDeviceScaleFactor := GlobalCEFApp.ForcedDeviceScaleFactor
   else
    FForcedDeviceScaleFactor := 0;

  {$IFDEF MSWINDOWS}           
  FSyncObj                := 0;
  FIMEHandler             := nil;
  FOnIMECancelComposition := nil;
  FOnIMECommitText        := nil;
  FOnIMESetComposition    := nil;
  FOnCustomTouch          := nil;
  FOnPointerDown          := nil;
  FOnPointerUp            := nil;
  FOnPointerUpdate        := nil;
  {$ELSE}
  FSyncObj                := nil;
  FBitmap                 := nil;
  FPopupBuffer            := nil;
  FPopupScanlineSize      := 0;
  {$ENDIF}
end;

destructor TBufferPanel.Destroy;
begin
  DestroyBuffer;
  DestroySyncObj;

  {$IFDEF MSWINDOWS}
  if (FIMEHandler <> nil) then FreeAndNil(FIMEHandler);
  {$ELSE}
  if (FBitmap <> nil) then FreeAndNil(FBitmap);
  {$ENDIF}

  inherited Destroy;
end;

procedure TBufferPanel.AfterConstruction;
begin
  inherited AfterConstruction;

  CreateSyncObj;

  {$IFDEF MSWINDOWS}
    {$IFNDEF FPC}
    ImeMode := imDontCare;
    ImeName := '';
    {$ENDIF}
  {$ENDIF}
end;

procedure TBufferPanel.CreateIMEHandler;
begin
  {$IFDEF MSWINDOWS}
  if (FIMEHandler = nil) and HandleAllocated then
    FIMEHandler := TCEFOSRIMEHandler.Create(Handle);
  {$ENDIF}
end;

procedure TBufferPanel.ChangeCompositionRange(const selection_range  : TCefRange;
                                              const character_bounds : TCefRectDynArray);
begin
  {$IFDEF MSWINDOWS}
  if (FIMEHandler <> nil) then
    FIMEHandler.ChangeCompositionRange(selection_range, character_bounds);
  {$ENDIF}
end;

{$IFNDEF MSWINDOWS}
procedure TBufferPanel.DrawPopupBuffer(const aSrcRect, aDstRect : TRect);
var
  src_y, dst_y, TempWidth : integer;
  src, dst : pointer;
begin
  if (FBuffer = nil) or (FPopupBuffer = nil) then exit;

  src_y := aSrcRect.Top;
  dst_y := aDstRect.Top;

  TempWidth := min(aSrcRect.Right - aSrcRect.Left + 1,
                   aDstRect.Right - aDstRect.Left + 1);

  if (aSrcRect.Left + TempWidth >= FPopupBuffer.Width) then
    TempWidth := FPopupBuffer.Width - aSrcRect.Left;

  if (aDstRect.Left + TempWidth >= FBuffer.Width) then
    TempWidth := FBuffer.Width - aDstRect.Left;

  while (src_y <= aSrcRect.Bottom) and (src_y < FPopupBuffer.Height) and
        (dst_y <= aDstRect.Bottom) and (dst_y < FBuffer.Height) do
    begin
      src := FPopupBuffer.ScanLine[src_y];
      dst := FBuffer.ScanLine[dst_y];

      if (aSrcRect.Left > 0) then
        inc(src, aSrcRect.Left * SizeOf(TRGBQuad));

      if (aDstRect.Left > 0) then
        inc(dst, aDstRect.Left * SizeOf(TRGBQuad));

      move(src^, dst^, TempWidth * SizeOf(TRGBQuad));

      inc(src_y);
      inc(dst_y);
    end;
end;
{$ENDIF}

procedure TBufferPanel.CreateSyncObj;
begin
  {$IFDEF MSWINDOWS}
  FSyncObj := CreateMutex(nil, False, nil);
  {$ELSE}
  FSyncObj := TCriticalSection.Create;
  {$ENDIF}
end;

procedure TBufferPanel.DestroySyncObj;
begin
  {$IFDEF MSWINDOWS}
  if (FSyncObj <> 0) then
    begin
      CloseHandle(FSyncObj);
      FSyncObj := 0;
    end;
  {$ELSE}
  if (FSyncObj <> nil) then FreeAndNil(FSyncObj);
  {$ENDIF}
end;

procedure TBufferPanel.DestroyBuffer;
begin
  if BeginBufferDraw then
    begin
      if (FBuffer      <> nil) then FreeAndNil(FBuffer);
      {$IFNDEF MSWINDOWS}
      if (FPopupBuffer <> nil) then FreeAndNil(FPopupBuffer);
      {$ENDIF}
      EndBufferDraw;
    end;
end;

function TBufferPanel.SaveBufferToFile(const aFilename : string) : boolean;
begin
  Result := False;
  try
    {$IFDEF MSWINDOWS}
    if (FBuffer <> nil) then
      begin
        FBuffer.SaveToFile(aFilename);
        Result := True;
      end;
    {$ELSE}
    if (FBitmap <> nil) then
      begin
        FBitmap.SaveToFile(aFilename);
        Result := True;
      end;
    {$ENDIF}
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
  {$IFDEF MSWINDOWS}
  Result := HandleAllocated and PostMessage(Handle, CM_INVALIDATE, 0, 0);
  {$ELSE}
  Result := True;
  TThread.Queue(nil, @Invalidate);
  {$ENDIF}
end;

function TBufferPanel.BeginBufferDraw : boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := (FSyncObj <> 0) and (WaitForSingleObject(FSyncObj, 5000) = WAIT_OBJECT_0);
  {$ELSE}
  if (FSyncObj <> nil) then
    begin
      FSyncObj.Acquire;
      Result := True;
    end
   else
    Result := False;
  {$ENDIF}
end;

procedure TBufferPanel.EndBufferDraw;
begin
  {$IFDEF MSWINDOWS}
  if (FSyncObj <> 0) then ReleaseMutex(FSyncObj);
  {$ELSE}
  if (FSyncObj <> nil) then FSyncObj.Release;
  {$ENDIF}
end;

function TBufferPanel.CopyBuffer : boolean;
var
  {$IFDEF MSWINDOWS}
  TempFunction  : TBlendFunction;
  {$ELSE}
  y : integer;
  src, dst : pointer;
  {$ENDIF}
begin
  Result := False;

  if BeginBufferDraw then
    try
      if (FBuffer <> nil) and (FBuffer.Width <> 0) and (FBuffer.Height <> 0) then
        begin
          {$IFDEF MSWINDOWS}
            if FTransparent then
              begin
                // TODO : To avoid flickering we should be using another bitmap
                // for the background image. We should blend "FBuffer" with the
                // "background bitmap" and then blit the result to the canvas.

                if assigned(FOnPaintParentBkg) then FOnPaintParentBkg(self);

                TempFunction.BlendOp             := AC_SRC_OVER;
                TempFunction.BlendFlags          := 0;
                TempFunction.SourceConstantAlpha := 255;
                TempFunction.AlphaFormat         := AC_SRC_ALPHA;

                Result := AlphaBlend(Canvas.Handle, 0, 0, Width, Height,
                                     FBuffer.Canvas.Handle, 0, 0, FBuffer.Width, FBuffer.Height,
                                     TempFunction);
              end
             else
              Result := BitBlt(Canvas.Handle, 0, 0, Width, Height,
                               FBuffer.Canvas.Handle, 0, 0,
                               SrcCopy);
          {$ELSE}
            try
              Canvas.Lock;

              if (FBitmap = nil) then
                begin
                  FBitmap             := TBitmap.Create;
                  FBitmap.PixelFormat := pf32bit;
                  FBitmap.HandleType  := bmDIB;
                  FBitmap.Width       := 1;
                  FBitmap.Height      := 1;
                end;

              if (FBitmap.Width  <> FBuffer.Width)  or
                 (FBitmap.Height <> FBuffer.Height) then
                begin
                  FBitmap.Width              := FBuffer.Width;
                  FBitmap.Height             := FBuffer.Height;
                  FBitmap.Canvas.Brush.Color := clWhite;
                  FBitmap.Canvas.FillRect(0, 0, FBitmap.Width, FBitmap.Height);
                end;

              FBitmap.BeginUpdate;
              y := 0;
              while (y < FBitmap.Height) do
                begin
                  src := FBuffer.ScanLine[y];
                  dst := FBitmap.ScanLine[y];
                  move(src^, dst^, FBuffer.ScanLineSize);
                  inc(y);
                end;
              FBitmap.EndUpdate;

              Canvas.Draw(0, 0, FBitmap);
              Result := True;
            finally
              Canvas.Unlock;
            end;
          {$ENDIF}
        end;
    finally
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
    if not(CopyBuffer) and not(FTransparent) then
      begin
        Canvas.Brush.Color := Color;
        Canvas.Brush.Style := bsSolid;
        Canvas.FillRect(rect(0, 0, Width, Height));
      end;
end;

{$IFDEF MSWINDOWS}
procedure TBufferPanel.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  if FTransparent then
    Params.ExStyle := Params.ExStyle and not WS_EX_TRANSPARENT;
end;

procedure TBufferPanel.WndProc(var aMessage: TMessage);
begin
  case aMessage.Msg of
    WM_IME_STARTCOMPOSITION : WMIMEStartComp(aMessage);
    WM_IME_COMPOSITION      : WMIMEComposition(aMessage);

    WM_IME_ENDCOMPOSITION :
      begin
        WMIMEEndComp(aMessage);
        inherited WndProc(aMessage);
      end;

    WM_IME_SETCONTEXT :
      begin
        aMessage.LParam := aMessage.LParam and not(ISC_SHOWUICOMPOSITIONWINDOW);
        inherited WndProc(aMessage);
        WMIMESetContext(aMessage);
      end;

    else inherited WndProc(aMessage);
  end;
end;

procedure TBufferPanel.WMEraseBkgnd(var aMessage : TWMEraseBkgnd);
begin
  aMessage.Result := 1;
end;

procedure TBufferPanel.WMTouch(var aMessage: TMessage);
var
  TempHandled : boolean;
begin
  TempHandled := False;
  {$IFDEF MSWINDOWS}
  if assigned(FOnCustomTouch) then FOnCustomTouch(self, aMessage, TempHandled);
  {$ENDIF}
  if not(TempHandled) then inherited;
end;

procedure TBufferPanel.WMPointerDown(var aMessage: TMessage);
var
  TempHandled : boolean;
begin
  TempHandled := False;
  {$IFDEF MSWINDOWS}
  if assigned(FOnPointerDown) then FOnPointerDown(self, aMessage, TempHandled);
  {$ENDIF}
  if not(TempHandled) then inherited;
end;

procedure TBufferPanel.WMPointerUpdate(var aMessage: TMessage);
var
  TempHandled : boolean;
begin
  TempHandled := False;
  {$IFDEF MSWINDOWS}
  if assigned(FOnPointerUpdate) then FOnPointerUpdate(self, aMessage, TempHandled);
  {$ENDIF}
  if not(TempHandled) then inherited;
end;

procedure TBufferPanel.WMPointerUp(var aMessage: TMessage);
var
  TempHandled : boolean;
begin
  TempHandled := False;
  {$IFDEF MSWINDOWS}
  if assigned(FOnPointerUp) then FOnPointerUp(self, aMessage, TempHandled);
  {$ENDIF}
  if not(TempHandled) then inherited;
end;

procedure TBufferPanel.WMIMEStartComp(var aMessage: TMessage);
begin
  if (FIMEHandler <> nil) then
    begin
      {$IFNDEF FPC}
      FInImeComposition := False;
      {$ENDIF}

      FIMEHandler.CreateImeWindow;
      FIMEHandler.MoveImeWindow;
      FIMEHandler.ResetComposition;
    end;
end;

procedure TBufferPanel.WMIMEEndComp(var aMessage: TMessage);
begin
  if assigned(FOnIMECancelComposition) then FOnIMECancelComposition(self);

  if (FIMEHandler <> nil) then
    begin
      FIMEHandler.ResetComposition;
      FIMEHandler.DestroyImeWindow;
    end;
end;

procedure TBufferPanel.WMIMESetContext(var aMessage: TMessage);
begin
  if (FIMEHandler <> nil) then
    begin
      FIMEHandler.CreateImeWindow;
      FIMEHandler.MoveImeWindow;
    end;
end;

procedure TBufferPanel.WMIMEComposition(var aMessage: TMessage);
var
  TempText        : ustring;
  TempRange       : TCefRange;
  TempCompStart   : integer;
  TempUnderlines  : TCefCompositionUnderlineDynArray;
  TempSelection   : TCefRange;
begin
  TempText       := '';
  TempCompStart  := 0;
  TempUnderlines := nil;

  try
    if (FIMEHandler <> nil) then
      begin
        if FIMEHandler.GetResult(aMessage.LParam, TempText) then
          begin
            if assigned(FOnIMECommitText) then
              begin
                TempRange.from := high(Integer);
                TempRange.to_  := high(Integer);

                FOnIMECommitText(self, TempText, @TempRange, 0);
              end;

            FIMEHandler.ResetComposition;
          end;

        if FIMEHandler.GetComposition(aMessage.LParam, TempText, TempUnderlines, TempCompStart) then
          begin
            if assigned(FOnIMESetComposition) then
              begin
                TempRange.from := high(Integer);
                TempRange.to_  := high(Integer);

                TempSelection.from := TempCompStart;
                TempSelection.to_  := TempCompStart + length(TempText);

                FOnIMESetComposition(self, TempText, TempUnderlines, TempRange, TempSelection);
              end;

            FIMEHandler.UpdateCaretPosition(pred(TempCompStart));
          end
         else
          begin
            if assigned(FOnIMECancelComposition) then FOnIMECancelComposition(self);

            FIMEHandler.ResetComposition;
            FIMEHandler.DestroyImeWindow;
          end;
      end;
  finally
    if (TempUnderlines <> nil) then
      begin
        Finalize(TempUnderlines);
        TempUnderlines := nil;
      end;
  end;
end;
{$ENDIF}

function TBufferPanel.GetBufferBits : pointer;
begin
  if (FBuffer <> nil) then
    begin
      {$IFDEF MSWINDOWS}
      Result := FBuffer.Scanline[pred(FBuffer.Height)];
      {$ELSE}
      Result := FBuffer.BufferBits;
      {$ENDIF}
    end
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

{$IFNDEF MSWINDOWS}
function TBufferPanel.GetPopupBufferBits : pointer;
begin
  if (FPopupBuffer <> nil) then
    Result := FPopupBuffer.BufferBits
   else
    Result := nil;
end;

function TBufferPanel.GetPopupBufferWidth : integer;
begin
  if (FPopupBuffer <> nil) then
    Result := FPopupBuffer.Width
   else
    Result := 0;
end;

function TBufferPanel.GetPopupBufferHeight : integer;    
begin
  if (FPopupBuffer <> nil) then
    Result := FPopupBuffer.Height
   else
    Result := 0;
end;
{$ENDIF}

function TBufferPanel.GetRealScreenScale(var aResultScale : single) : boolean;
var
  {$IFDEF MSWINDOWS}
  TempHandle : TCefWindowHandle;
  TempDC     : HDC;
  TempDPI    : UINT;
  {$ELSE}
    {$IFDEF LINUX}
      {$IFDEF FPC}
      TempForm    : TCustomForm;
      TempMonitor : TMonitor;
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
begin
  Result       := False;
  aResultScale := 1;

  {$IFDEF MSWINDOWS}
  TempHandle := ParentFormHandle;

  if (TempHandle <> 0) then
    begin
      Result := True;

      if RunningWindows10OrNewer and GetDPIForHandle(TempHandle, TempDPI) then
        aResultScale := TempDPI / USER_DEFAULT_SCREEN_DPI
       else
        begin
          TempDC       := GetWindowDC(TempHandle);
          aResultScale := GetDeviceCaps(TempDC, LOGPIXELSX) / USER_DEFAULT_SCREEN_DPI;
          ReleaseDC(TempHandle, TempDC);
        end;
    end;
  {$ELSE}
    {$IFDEF LINUX}
      {$IFDEF FPC}
      if (MainThreadID = GetCurrentThreadId()) then
        begin
          TempForm := GetParentForm(self, True);

          if (TempForm <> nil) then
            begin
              TempMonitor := TempForm.Monitor;

              if (TempMonitor <> nil) then
                begin
                  aResultScale := TempMonitor.PixelsPerInch / USER_DEFAULT_SCREEN_DPI;
                  Result       := True;
                end;
            end;
        end;
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
end;

function TBufferPanel.GetScreenScale : single;
var
  TempScale : single;
begin
  if (FForcedDeviceScaleFactor <> 0) then
    Result := FForcedDeviceScaleFactor
   else
    if GetRealScreenScale(TempScale) then
      Result := TempScale
     else
      if (GlobalCEFApp <> nil) then
        Result := GlobalCEFApp.DeviceScaleFactor
       else
        Result := 1;
end;

{$IFDEF MSWINDOWS}
function TBufferPanel.GetParentForm : TCustomForm;
var
  TempComp : TComponent;
begin
  Result   := nil;
  TempComp := Owner;

  while (TempComp <> nil) do
    if (TempComp is TCustomForm) then
      begin
        Result := TCustomForm(TempComp);
        exit;
      end
     else
      TempComp := TempComp.owner;
end;

function TBufferPanel.GetParentFormHandle : TCefWindowHandle;
var
  TempForm : TCustomForm;
begin
  Result   := 0;
  TempForm := GetParentForm;

  if (TempForm <> nil)  then
    Result := TempForm.Handle
   else
    if (Application          <> nil) and
       (Application.MainForm <> nil) then
      Result := Application.MainForm.Handle;
end;
{$ENDIF}

procedure TBufferPanel.SetTransparent(aValue : boolean);
begin
  if (FTransparent <> aValue) then
    begin
      FTransparent := aValue;

      {$IFDEF MSWINDOWS}
      RecreateWnd{$IFDEF FPC}(self){$ENDIF};
      {$ENDIF}
    end;
end;

procedure TBufferPanel.BufferDraw(x, y : integer; const aBitmap : TBitmap);
begin
  {$IFDEF MSWINDOWS}
  if (FBuffer <> nil) then FBuffer.Canvas.Draw(x, y, aBitmap);
  {$ENDIF}
end;

procedure TBufferPanel.BufferDraw(const aBitmap : TBitmap; const aSrcRect, aDstRect : TRect);
begin
  {$IFDEF MSWINDOWS}
  if (FBuffer <> nil) then FBuffer.Canvas.CopyRect(aDstRect, aBitmap.Canvas, aSrcRect);
  {$ENDIF}
end;

function TBufferPanel.UpdateBufferDimensions(aWidth, aHeight : integer) : boolean;
begin
  Result := False;

  if ((FBuffer        =  nil)      or
      (FBuffer.Width  <> aWidth)   or
      (FBuffer.Height <> aHeight)) then
    begin
      if (FBuffer <> nil) then FreeAndNil(FBuffer);

      {$IFDEF MSWINDOWS}
      FBuffer             := TBitmap.Create;
      FBuffer.PixelFormat := pf32bit;
      FBuffer.HandleType  := bmDIB;     
      FBuffer.Width       := aWidth;
      FBuffer.Height      := aHeight;
      FScanlineSize       := aWidth * SizeOf(TRGBQuad);
      {$ELSE}
      FBuffer             := TCEFBitmapBitBuffer.Create(aWidth, aHeight);
      FScanlineSize       := FBuffer.ScanlineSize;
      {$ENDIF}

      Result := True;
    end;
end;   

{$IFNDEF MSWINDOWS}
function TBufferPanel.UpdatePopupBufferDimensions(aWidth, aHeight : integer) : boolean;
begin
  Result := False;

  if ((FPopupBuffer        =  nil)      or
      (FPopupBuffer.Width  <> aWidth)   or
      (FPopupBuffer.Height <> aHeight)) then
    begin
      if (FPopupBuffer <> nil) then FreeAndNil(FPopupBuffer);

      FPopupBuffer       := TCEFBitmapBitBuffer.Create(aWidth, aHeight);
      FPopupScanlineSize := FPopupBuffer.ScanlineSize;
      Result             := True;
    end;
end;
{$ENDIF}

function TBufferPanel.BufferIsResized(aUseMutex : boolean) : boolean;
var
  TempDevWidth, TempLogWidth, TempDevHeight, TempLogHeight : integer;
  TempScale : single;
begin
  Result := False;
  if (GlobalCEFApp = nil) then exit;

  if not(aUseMutex) or BeginBufferDraw then
    begin
      TempScale := ScreenScale;

      if (TempScale = 1) then
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

          TempLogWidth  := DeviceToLogical(Width,  TempScale);
          TempLogHeight := DeviceToLogical(Height, TempScale);

          TempDevWidth  := LogicalToDevice(TempLogWidth,  TempScale);
          TempDevHeight := LogicalToDevice(TempLogHeight, TempScale);

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
