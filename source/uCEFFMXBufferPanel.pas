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
//        Copyright © 2019 Salvador Diaz Fau. All rights reserved.
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

unit uCEFFMXBufferPanel;

{$I cef.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ELSE}
  System.SyncObjs,
  {$ENDIF}
  System.Classes, System.UIConsts, System.Types, System.UITypes,
  {$IFDEF DELPHI17_UP}
  FMX.Graphics,
  {$ENDIF}
  FMX.Types, FMX.Controls;

type
  TDialogKeyEvent = procedure(Sender: TObject; var Key: Word; Shift: TShiftState) of object;

  {$IFNDEF FPC}{$IFDEF DELPHI16_UP}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}{$ENDIF}
  TFMXBufferPanel = class(TControl)
    protected
      {$IFDEF MSWINDOWS}
      FMutex            : THandle;
      {$ELSE}
      FBufferCS         : TCriticalSection;
      {$ENDIF}
      FBuffer           : TBitmap;
      FScanlineSize     : integer;
      FColor            : TAlphaColor;
      FHighSpeedDrawing : boolean;
      FOnDialogKey      : TDialogKeyEvent;

      procedure CreateSyncObj;

      procedure DestroySyncObj;
      procedure DestroyBuffer;

      function  GetScreenScale : Single;
      function  GetBufferWidth : integer;
      function  GetBufferHeight : integer;

      function  CopyBuffer : boolean;
      function  SaveBufferToFile(const aFilename : string) : boolean;

      procedure Paint; override;
      procedure DialogKey(var Key: Word; Shift: TShiftState); override;

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
      function    ScreenToClient(aPoint : TPoint) : TPoint;
      function    ClientToScreen(aPoint : TPoint) : TPoint;

      property Buffer         : TBitmap            read FBuffer;
      property ScanlineSize   : integer            read FScanlineSize;
      property BufferWidth    : integer            read GetBufferWidth;
      property BufferHeight   : integer            read GetBufferHeight;
      property ScreenScale    : single             read GetScreenScale;

    published
      property Align;
      property Anchors;
      property Visible;
      property Enabled;
      property TabOrder;
      property Color            : TAlphaColor        read FColor            write FColor            default claWhite;
      property HighSpeedDrawing : boolean            read FHighSpeedDrawing write FHighSpeedDrawing default True;

      {$IFDEF DELPHI17_UP}
      property TabStop;
      property CanFocus;
      property CanParentFocus;
      property Height;
      property Width;
      property Padding;
      property Opacity;
      property Margins;
      property Position;
      property RotationAngle;
      property RotationCenter;
      property Scale;
      property Size;
      {$ENDIF}
      {$IFNDEF DELPHI23_UP}
      property Hint;
      property ShowHint;
      {$ENDIF}

      property OnEnter;
      property OnExit;
      property OnResize;
      property OnClick;
      property OnMouseDown;
      property OnMouseMove;
      property OnMouseUp;
      property OnMouseEnter;
      property OnMouseLeave;
      property OnMouseWheel;
      property OnKeyUp;
      property OnKeyDown;
      property OnDialogKey    : TDialogKeyEvent    read FOnDialogKey      write FOnDialogKey;
  end;

implementation

uses
  System.SysUtils, System.Math,
  FMX.Platform, uCEFMiscFunctions, uCEFApplication;

constructor TFMXBufferPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  {$IFDEF MSWINDOWS}
  FMutex            := 0;
  {$ELSE}
  FBufferCS         := nil;
  {$ENDIF}
  FBuffer           := nil;
  FScanlineSize     := 0;
  FColor            := claWhite;
  FOnDialogKey      := nil;
  FHighSpeedDrawing := True;
end;

destructor TFMXBufferPanel.Destroy;
begin
  DestroyBuffer;
  DestroySyncObj;

  inherited Destroy;
end;

procedure TFMXBufferPanel.AfterConstruction;
begin
  inherited AfterConstruction;

  CreateSyncObj;
end;

procedure TFMXBufferPanel.CreateSyncObj;
begin
  {$IFDEF MSWINDOWS}
  FMutex := CreateMutex(nil, False, nil);
  {$ELSE}
  FBufferCS := TCriticalSection.Create;
  {$ENDIF}
end;

procedure TFMXBufferPanel.DestroySyncObj;
begin
  {$IFDEF MSWINDOWS}
  if (FMutex <> 0) then
    begin
      CloseHandle(FMutex);
      FMutex := 0;
    end;
  {$ELSE}
  if (FBufferCS <> nil) then FreeAndNil(FBufferCS);
  {$ENDIF}
end;

procedure TFMXBufferPanel.DestroyBuffer;
begin
  if BeginBufferDraw then
    begin
      if (FBuffer <> nil) then FreeAndNil(FBuffer);
      EndBufferDraw;
    end;
end;

function TFMXBufferPanel.SaveBufferToFile(const aFilename : string) : boolean;
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
      if CustomExceptionHandler('TFMXBufferPanel.SaveBufferToFile', e) then raise;
  end;
end;

function TFMXBufferPanel.SaveToFile(const aFilename : string) : boolean;
begin
  Result := False;

  if BeginBufferDraw then
    begin
      Result := SaveBufferToFile(aFilename);
      EndBufferDraw;
    end;
end;

procedure TFMXBufferPanel.InvalidatePanel;
begin
  InvalidateRect(TRectF.Create(0, 0, Width, Height));
end;

function TFMXBufferPanel.BeginBufferDraw : boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := (FMutex <> 0) and (WaitForSingleObject(FMutex, 5000) = WAIT_OBJECT_0);
  {$ELSE}
  if (FBufferCS <> nil) then
    begin
      FBufferCS.Acquire;
      Result := True;
    end
   else
    Result := False;
  {$ENDIF}
end;

procedure TFMXBufferPanel.EndBufferDraw;
begin
  {$IFDEF MSWINDOWS}
  if (FMutex <> 0) then ReleaseMutex(FMutex);
  {$ELSE}
  if (FBufferCS <> nil) then FBufferCS.Release;
  {$ENDIF}
end;

function TFMXBufferPanel.CopyBuffer : boolean;
var
  TempSrc, TempDst : TRectF;
begin
  Result := False;

  if Canvas.BeginScene then
    try
      if BeginBufferDraw then
        begin
          if (FBuffer <> nil) then
            begin
              TempSrc := TRectF.Create(0, 0, FBuffer.Width, FBuffer.Height);
              TempDst := TRectF.Create(0, 0, FBuffer.Width / ScreenScale, FBuffer.Height / ScreenScale);

              Canvas.DrawBitmap(FBuffer, TempSrc, TempDst, 1, FHighSpeedDrawing);

              Result := True;
            end;

          EndBufferDraw;
        end;
    finally
      Canvas.EndScene;
    end;
end;

procedure TFMXBufferPanel.DialogKey(var Key: Word; Shift: TShiftState);
begin
  if assigned(FOnDialogKey) then FOnDialogKey(self, Key, Shift);

  inherited DialogKey(Key, Shift);
end;

procedure TFMXBufferPanel.Paint;
var
  TempRect : TRectF;
begin
  if (csDesigning in ComponentState) or not(CopyBuffer) then
    begin
      TempRect := TRectF.Create(0, 0, Width, Height);

      if Canvas.BeginScene then
        try
          Canvas.ClearRect(TempRect, FColor);
        finally
          Canvas.EndScene;
        end;
    end;
end;

function TFMXBufferPanel.GetScreenScale : Single;
begin
  if (GlobalCEFApp <> nil) then
    Result := GlobalCEFApp.DeviceScaleFactor
   else
    Result := 1;
end;

function TFMXBufferPanel.GetBufferWidth : integer;
begin
  if (FBuffer <> nil) then
    Result := FBuffer.Width
   else
    Result := 0;
end;

function TFMXBufferPanel.GetBufferHeight : integer;
begin
  if (FBuffer <> nil) then
    Result := FBuffer.Height
   else
    Result := 0;
end;

procedure TFMXBufferPanel.BufferDraw(x, y : integer; const aBitmap : TBitmap);
var
  TempSrc, TempDst : TRectF;
begin
  if (FBuffer <> nil) then
    begin
      TempSrc := TRectF.Create(0, 0, aBitmap.Width, aBitmap.Height);
      TempDst := TRectF.Create(x, y, x + (aBitmap.Width / ScreenScale), y + (aBitmap.Height / ScreenScale));

      if FBuffer.Canvas.BeginScene then
        try
          FBuffer.Canvas.DrawBitmap(aBitmap, TempSrc, TempDst, 1, FHighSpeedDrawing);
        finally
          FBuffer.Canvas.EndScene;
        end;
    end;
end;

function TFMXBufferPanel.UpdateBufferDimensions(aWidth, aHeight : integer) : boolean;
begin
  Result := False;

  if ((FBuffer        =  nil)      or
      (FBuffer.Width  <> aWidth)   or
      (FBuffer.Height <> aHeight)) then
    begin
      if (FBuffer <> nil) then FreeAndNil(FBuffer);

      FBuffer             := TBitmap.Create(aWidth, aHeight);
      {$IFDEF DELPHI17_UP}
      FBuffer.BitmapScale := ScreenScale;
      FScanlineSize       := FBuffer.BytesPerLine;
      {$ELSE}
      FScanlineSize       := aWidth * SizeOf(TRGBQuad);
      {$ENDIF}
      Result              := True;
    end;
end;

function TFMXBufferPanel.BufferIsResized(aUseMutex : boolean) : boolean;
var
  TempWidth, TempHeight : integer;
begin
  Result := False;
  if (GlobalCEFApp = nil) then exit;

  if not(aUseMutex) or BeginBufferDraw then
    begin
      TempWidth  := round(Width  * GlobalCEFApp.DeviceScaleFactor);
      TempHeight := round(Height * GlobalCEFApp.DeviceScaleFactor);

      Result := (FBuffer <> nil) and
                (FBuffer.Width  = TempWidth) and
                (FBuffer.Height = TempHeight);

      if aUseMutex then EndBufferDraw;
    end;
end;

function TFMXBufferPanel.ScreenToClient(aPoint : TPoint) : TPoint;
var
  TempPoint : TPointF;
begin
  TempPoint.x := aPoint.x;
  TempPoint.y := aPoint.y;
  TempPoint   := ScreenToLocal(TempPoint);
  Result.x    := round(TempPoint.x);
  Result.y    := round(TempPoint.y);
end;

function TFMXBufferPanel.ClientToScreen(aPoint : TPoint) : TPoint;
var
  TempPoint : TPointF;
begin
  TempPoint.x := aPoint.x;
  TempPoint.y := aPoint.y;
  TempPoint   := LocalToScreen(TempPoint);
  Result.x    := round(TempPoint.x);
  Result.y    := round(TempPoint.y);
end;

end.
