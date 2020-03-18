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
  {$IFDEF MSWINDOWS}Winapi.Windows, Winapi.Messages, Vcl.ExtCtrls, Vcl.Controls, Vcl.Graphics, WinApi.Imm,{$ENDIF}
  System.Classes, System.SyncObjs, System.SysUtils,
  {$ELSE}
    {$IFDEF MSWINDOWS}Windows, imm, {$ENDIF} Classes, Forms, Controls, Graphics,
    {$IFDEF FPC}
    LCLProc, LCLType, LCLIntf, LResources, LMessages, InterfaceBase, {$IFDEF MSWINDOWS}Win32Extra,{$ENDIF}
    {$ELSE}
    Messages,
    {$ENDIF}
    ExtCtrls, SyncObjs, SysUtils,
  {$ENDIF}
  {$IFDEF MSWINDOWS}uCEFOSRIMEHandler,{$ENDIF} uCEFConstants, uCEFTypes;

type
  TOnIMECommitTextEvent     = procedure(Sender: TObject; const aText : ustring; const replacement_range : PCefRange; relative_cursor_pos : integer) of object;
  TOnIMESetCompositionEvent = procedure(Sender: TObject; const aText : ustring; const underlines : TCefCompositionUnderlineDynArray; const replacement_range, selection_range : TCefRange) of object;
  {$IFDEF MSWINDOWS}
  TOnHandledMessageEvent    = procedure(Sender: TObject; var aMessage: TMessage; var aHandled : boolean) of object;
  {$ENDIF}

  {$IFNDEF FPC}{$IFDEF DELPHI16_UP}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}{$ENDIF}
  TBufferPanel = class(TCustomPanel)
    protected
      FMutex                  : THandle;
      FBuffer                 : TBitmap;
      FScanlineSize           : integer;
      FTransparent            : boolean;
      FOnPaintParentBkg       : TNotifyEvent;
      FOnWrongSize            : TNotifyEvent;
      {$IFDEF MSWINDOWS}
      FIMEHandler             : TCEFOSRIMEHandler;
      FOnIMECancelComposition : TNotifyEvent;
      FOnIMECommitText        : TOnIMECommitTextEvent;
      FOnIMESetComposition    : TOnIMESetCompositionEvent;
      FOnCustomTouch          : TOnHandledMessageEvent;
      FOnPointerDown          : TOnHandledMessageEvent;
      FOnPointerUp            : TOnHandledMessageEvent;
      FOnPointerUpdate        : TOnHandledMessageEvent;
      {$ENDIF}

      procedure CreateSyncObj;

      procedure DestroySyncObj;
      procedure DestroyBuffer;

      function  GetBufferBits : pointer;
      function  GetBufferWidth : integer;
      function  GetBufferHeight : integer;

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

      property Buffer         : TBitmap            read FBuffer;
      property ScanlineSize   : integer            read FScanlineSize;
      property BufferWidth    : integer            read GetBufferWidth;
      property BufferHeight   : integer            read GetBufferHeight;
      property BufferBits     : pointer            read GetBufferBits;

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
      property OnWrongSize               : TNotifyEvent              read FOnWrongSize               write FOnWrongSize;

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
  uCEFMiscFunctions, uCEFApplicationCore;

constructor TBufferPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FMutex            := 0;
  FBuffer           := nil;
  FTransparent      := False;
  FOnPaintParentBkg := nil;
  FOnWrongSize      := nil;

  {$IFDEF MSWINDOWS}
  FIMEHandler             := nil;
  FOnIMECancelComposition := nil;
  FOnIMECommitText        := nil;
  FOnIMESetComposition    := nil;
  FOnCustomTouch          := nil;
  FOnPointerDown          := nil;
  FOnPointerUp            := nil;
  FOnPointerUpdate        := nil;
  {$ENDIF}
end;

destructor TBufferPanel.Destroy;
begin
  DestroyBuffer;
  DestroySyncObj;

  {$IFDEF MSWINDOWS}
  if (FIMEHandler <> nil) then FreeAndNil(FIMEHandler);
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

procedure TBufferPanel.CreateSyncObj;
begin
  {$IFDEF MSWINDOWS}
  FMutex := CreateMutex(nil, False, nil);
  {$ENDIF}
end;

procedure TBufferPanel.DestroySyncObj;
begin
  {$IFDEF MSWINDOWS}
  if (FMutex <> 0) then
    begin
      CloseHandle(FMutex);
      FMutex := 0;
    end;
  {$ENDIF}
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
  {$IFDEF MSWINDOWS}
  Result := HandleAllocated and PostMessage(Handle, CM_INVALIDATE, 0, 0);
  {$ENDIF}
end;

function TBufferPanel.BeginBufferDraw : boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := (FMutex <> 0) and (WaitForSingleObject(FMutex, 5000) = WAIT_OBJECT_0);
  {$ENDIF}
end;

procedure TBufferPanel.EndBufferDraw;
begin
  {$IFDEF MSWINDOWS}
  if (FMutex <> 0) then ReleaseMutex(FMutex);
  {$ENDIF}
end;

function TBufferPanel.CopyBuffer : boolean;
var
  {$IFDEF MSWINDOWS}
  TempFunction  : TBlendFunction;
  {$ENDIF}
  TempWrongSize : boolean;
begin
  Result        := False;
  TempWrongSize := False;

  {$IFDEF MSWINDOWS}
  if BeginBufferDraw then
    try
      if (FBuffer <> nil) then
        begin
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

          TempWrongSize := (Width <> FBuffer.Width) or (Height <> FBuffer.Height);
        end;
    finally
      EndBufferDraw;
    end;
  {$ENDIF}

  if TempWrongSize and assigned(FOnWrongSize) then FOnWrongSize(self);
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
  if (FBuffer <> nil) then FBuffer.Canvas.Draw(x, y, aBitmap);
end;

procedure TBufferPanel.BufferDraw(const aBitmap : TBitmap; const aSrcRect, aDstRect : TRect);
begin
  if (FBuffer <> nil) then FBuffer.Canvas.CopyRect(aDstRect, aBitmap.Canvas, aSrcRect);
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

      FScanlineSize := FBuffer.Width * SizeOf(TRGBQuad);
      Result        := True;
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
