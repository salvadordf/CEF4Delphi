unit uCEFBrowserBitmap;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
    {$IFDEF MSWINDOWS}Winapi.Windows,{$ELSE}System.SyncObjs,{$ENDIF} System.Classes, System.SysUtils, Vcl.Graphics;
  {$ELSE}
    {$IFDEF MSWINDOWS}Windows,{$ENDIF} Classes, SysUtils, Graphics
    {$IFDEF FPC}, LCLProc, LCLType, LCLIntf, LResources, InterfaceBase{$ENDIF}
    {$IFNDEF MSWINDOWS}, SyncObjs{$ENDIF};
  {$ENDIF}

type
  TCEFBrowserBitmap = class(TBitmap)
    protected
      FScanlineSize            : integer;
      FDeviceScaleFactor       : single;
      {$IFDEF MSWINDOWS}
      FSyncObj                 : THandle;
      {$ELSE}
      FSyncObj                 : TCriticalSection;
      {$ENDIF}

      function  GetBufferBits : pointer;

      procedure CreateSyncObj;
      procedure DestroySyncObj;

    public
      constructor Create; override;
      destructor  Destroy; override;
      function    BeginBufferDraw : boolean;
      procedure   EndBufferDraw;
      function    UpdateBufferDimensions(aWidth, aHeight : integer) : boolean;
      function    BufferIsResized(aUseMutex : boolean = True) : boolean;
      procedure   BufferDraw(const aBitmap : TBitmap; const aSrcRect, aDstRect : TRect);

      property ScanlineSize              : integer                   read FScanlineSize;
      property BufferBits                : pointer                   read GetBufferBits;
      property DeviceScaleFactor         : single                    read FDeviceScaleFactor         write FDeviceScaleFactor;
  end;

implementation

uses
  uCEFMiscFunctions;

constructor TCEFBrowserBitmap.Create;
begin
  inherited Create;

  FScanlineSize      := 0;
  FDeviceScaleFactor := 1;
  CreateSyncObj;
end;

destructor TCEFBrowserBitmap.Destroy;
begin
  DestroySyncObj;

  inherited Destroy;
end;

procedure TCEFBrowserBitmap.CreateSyncObj;
begin
  {$IFDEF MSWINDOWS}
  FSyncObj := CreateMutex(nil, False, nil);
  {$ELSE}
  FSyncObj := TCriticalSection.Create;
  {$ENDIF}
end;

procedure TCEFBrowserBitmap.DestroySyncObj;
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

function TCEFBrowserBitmap.GetBufferBits : pointer;
begin
  if (Height <> 0) then
    Result := Scanline[pred(Height)]
   else
    Result := nil;
end;

function TCEFBrowserBitmap.BeginBufferDraw : boolean;
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

procedure TCEFBrowserBitmap.EndBufferDraw;
begin
  {$IFDEF MSWINDOWS}
  if (FSyncObj <> 0) then ReleaseMutex(FSyncObj);
  {$ELSE}
  if (FSyncObj <> nil) then FSyncObj.Release;
  {$ENDIF}
end;

function TCEFBrowserBitmap.UpdateBufferDimensions(aWidth, aHeight : integer) : boolean;
begin
  Result        := False;
  FScanlineSize := aWidth * SizeOf(TRGBQuad);

  if (Width  <> aWidth)  or
     (Height <> aHeight) then
    begin
      {$IFDEF DELPHI16_UP}
      SetSize(aWidth, aHeight);
      {$ELSE}
      Width  := aWidth;
      Height := aHeight;
      {$ENDIF}
      Result := True;
    end;
end;

function TCEFBrowserBitmap.BufferIsResized(aUseMutex : boolean) : boolean;
var
  TempDevWidth, TempLogWidth, TempDevHeight, TempLogHeight : integer;
begin
  Result := False;

  if not(aUseMutex) or BeginBufferDraw then
    begin
      if (FDeviceScaleFactor = 1) then
        Result := (Width  = Width) and
                  (Height = Height)
       else
        begin
          // CEF and Chromium use 'floor' to round the float values in Device <-> Logical unit conversions
          // and Delphi uses MulDiv, which uses the bankers rounding, to resize the components in high DPI mode.
          // This is the cause of slight differences in size between the buffer and the panel in some occasions.

          TempLogWidth  := DeviceToLogical(Width,  FDeviceScaleFactor);
          TempLogHeight := DeviceToLogical(Height, FDeviceScaleFactor);

          TempDevWidth  := LogicalToDevice(TempLogWidth,  FDeviceScaleFactor);
          TempDevHeight := LogicalToDevice(TempLogHeight, FDeviceScaleFactor);

          Result := (Width  = TempDevWidth) and
                    (Height = TempDevHeight);
        end;

      if aUseMutex then EndBufferDraw;
    end;
end;

procedure TCEFBrowserBitmap.BufferDraw(const aBitmap : TBitmap; const aSrcRect, aDstRect : TRect);
begin
  if (aBitmap <> nil) then
    begin
      Canvas.Lock;
      aBitmap.Canvas.Lock;
      Canvas.CopyRect(aDstRect, aBitmap.Canvas, aSrcRect);
      aBitmap.Canvas.UnLock;
      Canvas.UnLock;
    end;
end;

end.
