unit uCEFFMXBufferPanel;

{$I cef.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows, FMX.Platform.Win,
  {$ELSE}
  System.SyncObjs,
  {$ENDIF}
  System.Classes, System.UIConsts, System.Types, System.UITypes,
  {$IFDEF DELPHI19_UP}
  FMX.Graphics,
  {$ENDIF}
  FMX.Types, FMX.Controls, FMX.Forms,
  uCEFTypes, uCEFConstants;

type
  TDialogKeyEvent = procedure(Sender: TObject; var Key: Word; Shift: TShiftState) of object;

  {$IFNDEF FPC}{$IFDEF DELPHI16_UP}[ComponentPlatformsAttribute(pfidWindows or pfidOSX or pfidLinux)]{$ENDIF}{$ENDIF}
  /// <summary>
  /// TBufferPanel is used by FMX applications with browsers in OSR mode
  /// to draw the browser contents. See the FMXExternalPumpBrowser demo for more details.
  /// </summary>
  TFMXBufferPanel = class(TControl)
    protected
      {$IFDEF MSWINDOWS}
      FMutex                   : THandle;
      {$ELSE}
      FBufferCS                : TCriticalSection;
      {$ENDIF}
      FBuffer                  : TBitmap;
      FScanlineSize            : integer;
      FColor                   : TAlphaColor;
      FHighSpeedDrawing        : boolean;
      FOnDialogKey             : TDialogKeyEvent;
      FForcedDeviceScaleFactor : single;

      procedure CreateSyncObj;

      procedure DestroySyncObj;
      procedure DestroyBuffer;

      function  GetScreenScale : single; virtual;
      function  GetBufferWidth : integer;
      function  GetBufferHeight : integer;
      function  GetParentForm : TCustomForm;
      function  GetParentFormHandle : TCefWindowHandle;
      function  GetRealScreenScale(var aResultScale : single) : boolean; virtual;

      function  CopyBuffer : boolean;
      function  SaveBufferToFile(const aFilename : string) : boolean;

      procedure Paint; override;
      procedure DialogKey(var Key: Word; Shift: TShiftState); override;

    public
      constructor Create(AOwner: TComponent); override;
      destructor  Destroy; override;
      procedure   AfterConstruction; override;
      /// <summary>
      /// Save the visible web contents as a bitmap file.
      /// </summary>
      function    SaveToFile(const aFilename : string) : boolean;
      /// <summary>
      /// Invalidate this panel.
      /// </summary>
      procedure   InvalidatePanel;
      /// <summary>
      /// Acquires the synchronization object before drawing into the background bitmap.
      /// </summary>
      function    BeginBufferDraw : boolean;
      /// <summary>
      /// Releases the synchronization object after drawing into the background bitmap.
      /// </summary>
      procedure   EndBufferDraw;
      /// <summary>
      /// Draws a part of aBitmap into the background bitmap buffer at the specified rectangle.
      /// </summary>
      /// <param name="aBitmap">Bitmap that will be drawn into the background bitmap.</param>
      /// <param name="aSrcRect">Rectangle that defines the area of aBitmap that will be drawn into the background bitmap.</param>
      /// <param name="aDstRect">Rectangle that defines the area of the background bitmap where aBitmap will be drawn.</param>
      procedure   BufferDraw(const aBitmap : TBitmap; const aSrcRect, aDstRect : TRectF);
      /// <summary>
      /// Update the background bitmap size.
      /// </summary>
      function    UpdateBufferDimensions(aWidth, aHeight : integer) : boolean;
      /// <summary>
      /// Check if the background image buffers have the same dimensions as this panel. Returns true if they have the same size.
      /// </summary>
      function    BufferIsResized(aUseMutex : boolean = True) : boolean;
      /// <summary>
      /// Convert a point from the screen coordinate system to the client coordinate system.
      /// </summary>
      function    ScreenToClient(aPoint : TPoint) : TPoint; overload;
      /// <summary>
      /// Convert a point from the screen coordinate system to the client coordinate system.
      /// </summary>
      function    ScreenToClient(aPoint : TPointF) : TPointF; overload;
      /// <summary>
      /// Convert a point from the client coordinate system to the screen coordinate system.
      /// </summary>
      function    ClientToScreen(aPoint : TPoint) : TPoint; overload;
      /// <summary>
      /// Convert a point from the client coordinate system to the screen coordinate system.
      /// </summary>
      function    ClientToScreen(aPoint : TPointF) : TPointF; overload;
      /// <summary>
      /// Background bitmap.
      /// </summary>
      property Buffer                    : TBitmap                   read FBuffer;
      /// <summary>
      /// Returns the scanline size.
      /// </summary>
      property ScanlineSize              : integer                   read FScanlineSize;
      /// <summary>
      /// Image width.
      /// </summary>
      property BufferWidth               : integer                   read GetBufferWidth;
      /// <summary>
      /// Image height.
      /// </summary>
      property BufferHeight              : integer                   read GetBufferHeight;
      /// <summary>
      /// Returns the screen scale.
      /// </summary>
      property ScreenScale               : single                    read GetScreenScale;
      /// <summary>
      /// Screen scale value used instead of the real one.
      /// </summary>
      property ForcedDeviceScaleFactor   : single                    read FForcedDeviceScaleFactor   write FForcedDeviceScaleFactor;

    published
      property Align;
      property Anchors;
      property Visible;
      property Enabled;
      property TabOrder;
      /// <summary>
      /// Color used to clear the panel canvas in the Paint method.
      /// </summary>
      property Color            : TAlphaColor        read FColor            write FColor            default claWhite;
      /// <summary>
      /// Set HighSpeedDrawing to True to draw the buffer to the canvas using a high speed interpolation mode.
      /// </summary>
      property HighSpeedDrawing : boolean            read FHighSpeedDrawing write FHighSpeedDrawing default True;

      {$IFDEF DELPHI17_UP}
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
      {$ENDIF}
      {$IFDEF DELPHI18_UP}
      property TabStop;
      {$ENDIF}
      {$IFDEF DELPHI21_UP}
      property Size;
      {$ENDIF}
      {$IFDEF DELPHI25_UP}
      property OnResized;
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
      /// <summary>
      /// Event triggered before the DialogKey.
      /// </summary>
      property OnDialogKey    : TDialogKeyEvent    read FOnDialogKey      write FOnDialogKey;
  end;

implementation

uses
  System.SysUtils, System.Math,
  {$IFDEF MSWINDOWS}{$IFDEF DELPHI24_UP}FMX.Helpers.Win,{$ENDIF}{$ENDIF}
  FMX.Platform, {$IFDEF MACOS}FMX.Platform.Mac,{$ENDIF}
  uCEFMiscFunctions, uCEFApplicationCore;

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

  if (GlobalCEFApp <> nil) and (GlobalCEFApp.ForcedDeviceScaleFactor <> 0) then
    FForcedDeviceScaleFactor := GlobalCEFApp.ForcedDeviceScaleFactor
   else
    FForcedDeviceScaleFactor := 0;
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
  TempSrc, TempDst, TempClip : TRectF;
  TempState : TCanvasSaveState;
  TempScale : single;
begin
  Result := False;

  if Canvas.BeginScene then
    try
      if BeginBufferDraw then
        try
          if (FBuffer <> nil) then
            begin
              TempScale := ScreenScale;
              TempSrc   := TRectF.Create(0, 0, FBuffer.Width, FBuffer.Height);
              TempDst   := TRectF.Create(0, 0, FBuffer.Width / TempScale, FBuffer.Height / TempScale);
              TempClip  := TRectF.Create(0, 0, Width, Height);

              TempState := Canvas.SaveState;
              try
                Canvas.IntersectClipRect(TempClip);
                Canvas.DrawBitmap(FBuffer, TempSrc, TempDst, 1, FHighSpeedDrawing);
                Result := True;
              finally
                Canvas.RestoreState(TempState);
              end;
            end;
        finally
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

function TFMXBufferPanel.GetParentForm : TCustomForm;
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

function TFMXBufferPanel.GetParentFormHandle : TCefWindowHandle;
{$IFDEF MSWINDOWS}
var
  TempForm : TCustomForm;
{$ENDIF}
begin
  InitializeWindowHandle(Result);

  {$IFDEF MSWINDOWS}
  TempForm := GetParentForm;

  if (TempForm <> nil)  then
    Result := FmxHandleToHWND(TempForm.Handle)
   else
    if (Application          <> nil) and
       (Application.MainForm <> nil) then
      Result := FmxHandleToHWND(Application.MainForm.Handle);
  {$ENDIF}
end;

function TFMXBufferPanel.GetRealScreenScale(var aResultScale : single) : boolean;
{$IFDEF DELPHI24_UP}{$IFDEF MSWINDOWS}
var
  TempHandle : TCefWindowHandle;
{$ENDIF}{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  {$IFDEF DELPHI24_UP}
  TempHandle := GetParentFormHandle;

  if (TempHandle <> 0) then
    begin
      Result       := True;
      aResultScale := GetWndScale(TempHandle);
    end
   else
    begin
      Result       := False;
      aResultScale := 1;
    end;
  {$ELSE}
  Result       := False;
  aResultScale := 1;
  {$ENDIF}
  {$ENDIF}

  {$IFDEF LINUX}
  if (Screen.DisplayCount = 1) then
    aResultScale := Screen.Displays[0].Scale
   else
    aResultScale := Screen.DisplayFromForm(GetParentForm).Scale;

  Result := True;
  {$ENDIF}

  {$IFDEF MACOS}
  Result       := True;
  aResultScale := TMacWindowHandle(GetParentForm.Handle).Wnd.backingScaleFactor;
  {$ENDIF}
end;

function TFMXBufferPanel.GetScreenScale : single;
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

procedure TFMXBufferPanel.BufferDraw(const aBitmap : TBitmap; const aSrcRect, aDstRect : TRectF);
begin
  if (FBuffer <> nil) then
    if FBuffer.Canvas.BeginScene then
      try
        FBuffer.Canvas.DrawBitmap(aBitmap, aSrcRect, aDstRect, 1, FHighSpeedDrawing);
      finally
        FBuffer.Canvas.EndScene;
      end;
end;

function TFMXBufferPanel.UpdateBufferDimensions(aWidth, aHeight : integer) : boolean;
{$IFDEF DELPHI18_UP}
var
  TempScale : single;
{$ENDIF}
begin
  Result    := False;
  {$IFDEF DELPHI18_UP}
  TempScale := ScreenScale;
  {$ENDIF}

  if ((FBuffer             =  nil)       or
      {$IFDEF DELPHI18_UP}
      (FBuffer.BitmapScale <> TempScale) or
      {$ENDIF}
      (FBuffer.Width       <> aWidth)    or
      (FBuffer.Height      <> aHeight))  then
    begin
      if (FBuffer <> nil) then FreeAndNil(FBuffer);

      FBuffer             := TBitmap.Create(aWidth, aHeight);
      {$IFDEF DELPHI18_UP}
      FBuffer.BitmapScale := TempScale;
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
  TempScale : single;
begin
  Result := False;

  if not(aUseMutex) or BeginBufferDraw then
    begin
      TempScale  := ScreenScale;
      TempWidth  := round(Width  * TempScale);
      TempHeight := round(Height * TempScale);

      Result := (FBuffer <> nil) and
                {$IFDEF DELPHI18_UP}
                (FBuffer.BitmapScale = TempScale) and
                {$ENDIF}
                (FBuffer.Width       = TempWidth) and
                (FBuffer.Height      = TempHeight);

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

function TFMXBufferPanel.ScreenToClient(aPoint : TPointF) : TPointF;
begin
  Result := ScreenToLocal(aPoint);
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

function TFMXBufferPanel.ClientToScreen(aPoint : TPointF) : TPointF;
begin
  Result := LocalToScreen(aPoint);
end;

end.
