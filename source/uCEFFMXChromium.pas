unit uCEFFMXChromium;

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  System.Classes, System.Types,
  {$IFDEF MSWINDOWS}
  WinApi.Windows, WinApi.Messages, FMX.Platform.Win,
  {$ENDIF}
  FMX.Types, FMX.Platform, FMX.Forms, FMX.Controls,
  {$IFDEF DELPHI19_UP}
  FMX.Graphics,
  {$ENDIF}
  uCEFTypes, uCEFInterfaces, uCEFConstants, uCEFChromiumCore;

type
  {$IFNDEF FPC}{$IFDEF DELPHI16_UP}[ComponentPlatformsAttribute(pfidWindows or pfidOSX or pfidLinux)]{$ENDIF}{$ENDIF}
  /// <summary>
  ///  FMX version of TChromiumCore that puts together all browser procedures, functions, properties and events in one place.
  ///  It has all you need to create, modify and destroy a web browser.
  /// </summary>
  TFMXChromium = class(TChromiumCore, IChromiumEvents)
    protected
      function  GetParentFormHandle : TCefWindowHandle; override;
      function  GetParentForm : TCustomForm;
      function  GetScreenScale : Single;
      procedure InitializeDevToolsWindowInfo; virtual;

    public
      /// <summary>
      /// Open developer tools (DevTools) in its own browser. If inspectElementAt has a valid point
      /// with coordinates different than low(integer) then the element at the specified location
      /// will be inspected. If the DevTools browser is already open then it will be focused.
      /// </summary>
      procedure ShowDevTools(inspectElementAt: TPoint);
      /// <summary>
      /// close the developer tools.
      /// </summary>
      procedure CloseDevTools;
      /// <summary>
      /// Move the parent form to the x and y coordinates.
      /// </summary>
      procedure MoveFormTo(const x, y: Integer);
      /// <summary>
      /// Move the parent form adding x and y to the coordinates.
      /// </summary>
      procedure MoveFormBy(const x, y: Integer);
      /// <summary>
      /// Add x to the parent form width.
      /// </summary>
      procedure ResizeFormWidthTo(const x : Integer);
      /// <summary>
      /// Add y to the parent form height.
      /// </summary>
      procedure ResizeFormHeightTo(const y : Integer);
      /// <summary>
      /// Set the parent form left property to x.
      /// </summary>
      procedure SetFormLeftTo(const x : Integer);
      /// <summary>
      /// Set the parent form top property to y.
      /// </summary>
      procedure SetFormTopTo(const y : Integer);
      /// <summary>
      /// Used to create the browser after the global request context has been
      /// initialized. You need to set all properties and events before calling
      /// this function because it will only create the internal handlers needed
      /// for those events and the property values will be used in the browser
      /// initialization.
      /// The browser will be fully initialized when the TChromiumCore.OnAfterCreated
      /// event is triggered.
      /// </summary>
      function  CreateBrowser(const aWindowName : ustring = ''; const aContext : ICefRequestContext = nil; const aExtraInfo : ICefDictionaryValue = nil) : boolean; overload; virtual;
      /// <summary>
      /// Copy the DC to a bitmap stream. Only works on Windows with browsers without GPU acceleration.
      /// It's recommended to use the "Page.captureScreenshot" DevTools method instead.
      /// </summary>
      function  SaveAsBitmapStream(const aStream : TStream; const aRect : System.Types.TRect) : boolean;
      /// <summary>
      /// Takes a snapshot into a TBitmap using the SaveAsBitmapStream function.
      /// </summary>
      function  TakeSnapshot(var aBitmap : TBitmap; const aRect : System.Types.TRect) : boolean;
      /// <summary>
      /// Returns the screen scale of the monitor where the parent form is located.
      /// </summary>
      property  ScreenScale    : single             read GetScreenScale;
  end;

// *********************************************************
// ********************** ATTENTION ! **********************
// *********************************************************
// **                                                     **
// **  MANY OF THE EVENTS IN CEF4DELPHI COMPONENTS LIKE   **
// **  TCHROMIUM, TFMXCHROMIUM OR TCEFAPPLICATION ARE     **
// **  EXECUTED IN A CEF THREAD BY DEFAULT.               **
// **                                                     **
// **  WINDOWS CONTROLS MUST BE CREATED AND DESTROYED IN  **
// **  THE SAME THREAD TO AVOID ERRORS.                   **
// **  SOME OF THEM RECREATE THE HANDLERS IF THEY ARE     **
// **  MODIFIED AND CAN CAUSE THE SAME ERRORS.            **
// **                                                     **
// **  DON'T CREATE, MODIFY OR DESTROY WINDOWS CONTROLS   **
// **  INSIDE THE CEF4DELPHI EVENTS AND USE               **
// **  SYNCHRONIZATION OBJECTS TO PROTECT VARIABLES AND   **
// **  FIELDS IF THEY ARE ALSO USED IN THE MAIN THREAD.   **
// **                                                     **
// **  READ THIS FOR MORE INFORMATION :                   **
// **  https://www.briskbard.com/index.php?pageid=cef     **
// **                                                     **
// **  USE OUR FORUMS FOR MORE QUESTIONS :                **
// **  https://www.briskbard.com/forum/                   **
// **                                                     **
// *********************************************************
// *********************************************************

implementation

uses
  {$IFDEF MSWINDOWS}{$IFDEF DELPHI24_UP}FMX.Helpers.Win,{$ENDIF}{$ENDIF}
  System.SysUtils, System.Math,
  uCEFApplicationCore;

function TFMXChromium.CreateBrowser(const aWindowName  : ustring;
                                    const aContext     : ICefRequestContext;
                                    const aExtraInfo   : ICefDictionaryValue) : boolean;
var
  TempHandle : TCefWindowHandle;
begin
  {$IFDEF MACOS}
  TempHandle := nil;
  {$ELSE}
  TempHandle := 0;
  {$ENDIF}
  Result := inherited CreateBrowser(TempHandle, Rect(0, 0, 0, 0), aWindowName, aContext, aExtraInfo);
end;

procedure TFMXChromium.InitializeDevToolsWindowInfo;
var
  TempHandle : TCefWindowHandle;
begin
  {$IFDEF MACOS}
  TempHandle := nil;
  {$ELSE}
  TempHandle := 0;
  {$ENDIF}
  DefaultInitializeDevToolsWindowInfo(TempHandle, Rect(0, 0, 0, 0), '');
end;

procedure TFMXChromium.ShowDevTools(inspectElementAt: TPoint);
begin
  if Initialized then
    begin
      InitializeDevToolsWindowInfo;
      inherited ShowDevTools(inspectElementAt, @FDevWindowInfo.WindowInfoRecord);
    end;
end;

procedure TFMXChromium.CloseDevTools;
begin
  inherited CloseDevTools;
end;

function TFMXChromium.GetParentForm : TCustomForm;
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

function TFMXChromium.GetScreenScale : Single;
{$IFDEF DELPHI24_UP}{$IFDEF MSWINDOWS}
var
  TempHandle : TCefWindowHandle;
{$ENDIF}{$ENDIF}
begin
  {$IFDEF DELPHI24_UP}{$IFDEF MSWINDOWS}
  TempHandle := GetParentFormHandle;

  if (TempHandle <> 0) then
    Result := GetWndScale(TempHandle)
   else
  {$ENDIF}{$ENDIF}
    if (GlobalCEFApp <> nil) then
      Result := GlobalCEFApp.DeviceScaleFactor
     else
      Result := 1;
end;

function TFMXChromium.GetParentFormHandle : TCefWindowHandle;
{$IFDEF MSWINDOWS}
var
  TempForm : TCustomForm;
{$ENDIF}
begin
  Result := inherited GetParentFormHandle;

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

procedure TFMXChromium.MoveFormTo(const x, y: Integer);
var
  TempForm : TCustomForm;
  {$IFDEF DELPHI21_UP}
  TempRect : TRect;
  {$ENDIF}
begin
  TempForm := GetParentForm;
  {$IFDEF DELPHI21_UP}
  if (TempForm <> nil) then
    begin
      TempRect.Left   := min(max(x, max(round(screen.DesktopLeft), 0)), round(screen.DesktopWidth)  - TempForm.Width);
      TempRect.Top    := min(max(y, max(round(screen.DesktopTop),  0)), round(screen.DesktopHeight) - TempForm.Height);
      TempRect.Right  := TempRect.Left + TempForm.Width  - 1;
      TempRect.Bottom := TempRect.Top  + TempForm.Height - 1;

      TempForm.SetBounds(TempRect.Left, TempRect.Top, TempRect.Right - TempRect.Left + 1, TempRect.Bottom - TempRect.Top + 1);
    end;
  {$ELSE}
  TempForm.SetBounds(x, y, TempForm.Width, TempForm.Height);
  {$ENDIF}
end;

procedure TFMXChromium.MoveFormBy(const x, y: Integer);
var
  TempForm : TCustomForm;
  {$IFDEF DELPHI21_UP}
  TempRect : TRect;
  {$ENDIF}
begin
  TempForm := GetParentForm;
  {$IFDEF DELPHI21_UP}
  if (TempForm <> nil) then
    begin
      TempRect.Left   := min(max(TempForm.Left + x, max(round(screen.DesktopLeft), 0)), round(screen.DesktopWidth)  - TempForm.Width);
      TempRect.Top    := min(max(TempForm.Top  + y, max(round(screen.DesktopTop),  0)), round(screen.DesktopHeight) - TempForm.Height);
      TempRect.Right  := TempRect.Left + TempForm.Width  - 1;
      TempRect.Bottom := TempRect.Top  + TempForm.Height - 1;

      TempForm.SetBounds(TempRect.Left, TempRect.Top, TempRect.Right - TempRect.Left + 1, TempRect.Bottom - TempRect.Top + 1);
    end;
  {$ELSE}
  TempForm.SetBounds(TempForm.Left + x, TempForm.Top + y, TempForm.Width, TempForm.Height);
  {$ENDIF}
end;

procedure TFMXChromium.ResizeFormWidthTo(const x : Integer);
var
  TempForm : TCustomForm;
  TempX, TempDeltaX : integer;
begin
  TempForm := GetParentForm;

  if (TempForm <> nil) then
    begin
      TempX          := max(x, 100);
      TempDeltaX     := TempForm.Width  - TempForm.ClientWidth;
      TempForm.Width := TempX + TempDeltaX;
    end;
end;

procedure TFMXChromium.ResizeFormHeightTo(const y : Integer);
var
  TempForm : TCustomForm;
  TempY, TempDeltaY : integer;
begin
  TempForm := GetParentForm;

  if (TempForm <> nil) then
    begin
      TempY           := max(y, 100);
      TempDeltaY      := TempForm.Height - TempForm.ClientHeight;
      TempForm.Height := TempY + TempDeltaY;
    end;
end;

procedure TFMXChromium.SetFormLeftTo(const x : Integer);
var
  TempForm : TCustomForm;
begin
  TempForm := GetParentForm;

  if (TempForm <> nil) then
    {$IFDEF DELPHI21_UP}
    TempForm.Left := min(max(x, max(round(screen.DesktopLeft), 0)), round(screen.DesktopWidth) - TempForm.Width);
    {$ELSE}
    TempForm.Left := x;
    {$ENDIF}
end;

procedure TFMXChromium.SetFormTopTo(const y : Integer);
var
  TempForm : TCustomForm;
begin
  TempForm := GetParentForm;

  if (TempForm <> nil) then
    {$IFDEF DELPHI21_UP}
    TempForm.Top := min(max(y, max(round(screen.DesktopTop), 0)), round(screen.DesktopHeight) - TempForm.Height);
    {$ELSE}
    TempForm.Top := y;
    {$ENDIF}
end;

function TFMXChromium.SaveAsBitmapStream(const aStream : TStream; const aRect : System.Types.TRect) : boolean;
{$IFDEF MSWINDOWS}
var
  TempDC   : HDC;
  TempRect : System.Types.TRect;
{$ENDIF}
begin
  Result := False;

  {$IFDEF MSWINDOWS}
  if not(FIsOSR) and (FRenderCompHWND <> 0) and (aStream <> nil) then
    begin
      TempDC := GetDC(FRenderCompHWND);

      if (TempDC <> 0) then
        try
          TempRect := aRect;
          Result   := OffsetRect(TempRect, - TempRect.Left, - TempRect.Top) and
                      CopyDCToBitmapStream(TempDC, TempRect, aStream);
        finally
          ReleaseDC(FRenderCompHWND, TempDC);
        end;
    end;
  {$ENDIF}
end;

function TFMXChromium.TakeSnapshot(var aBitmap : TBitmap; const aRect : System.Types.TRect) : boolean;
var
  TempStream : TMemoryStream;
begin
  Result     := False;
  TempStream := nil;

  if FIsOSR or (aBitmap = nil) then exit;

  try
    TempStream := TMemoryStream.Create;

    if SaveAsBitmapStream(TempStream, aRect) then
      begin
        aBitmap.LoadFromStream(TempStream);
        Result := True;
      end;
  finally
    FreeAndNil(TempStream);
  end;
end;

end.
