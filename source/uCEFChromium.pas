unit uCEFChromium;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
  {$IFDEF MSWINDOWS}WinApi.Windows,{$ENDIF} System.Classes, Vcl.Controls, Vcl.Graphics, Vcl.Forms,
  {$ELSE}
    {$IFDEF MSWINDOWS}Windows,{$ENDIF} Classes, Controls, Graphics, Forms,
    {$IFDEF FPC}
    LCLProc, LCLType, LCLIntf, LResources, LMessages, InterfaceBase,
    {$ELSE}
    Messages,
    {$ENDIF}
  {$ENDIF}
  uCEFConstants, uCEFTypes, uCEFInterfaces, uCEFChromiumCore;

type
  {$IFNDEF FPC}{$IFDEF DELPHI16_UP}[ComponentPlatformsAttribute(pfidWindows)]{$ENDIF}{$ENDIF}
  /// <summary>
  ///  VCL and LCL version of TChromiumCore that puts together all browser procedures, functions, properties and events in one place.
  ///  It has all you need to create, modify and destroy a web browser.
  /// </summary>
  TChromium = class(TChromiumCore)
    protected
      function  GetParentFormHandle : TCefWindowHandle; override;
      function  GetParentForm : TCustomForm;
      procedure InitializeDevToolsWindowInfo(aDevTools : TWinControl); virtual;
    public
      {$IFDEF MSWINDOWS}
      /// <summary>
      /// Used with browsers in OSR mode to initialize drag and drop in Windows.
      /// </summary>
      procedure InitializeDragAndDrop(const aDropTargetCtrl : TWinControl);
      {$ENDIF MSWINDOWS}
      /// <summary>
      /// Open developer tools (DevTools) in its own browser. If inspectElementAt has a valid point
      /// with coordinates different than low(integer) then the element at the specified location
      /// will be inspected. If the DevTools browser is already open then it will be focused.
      /// </summary>
      procedure ShowDevTools(inspectElementAt: TPoint; const aDevTools : TWinControl = nil);
      /// <summary>
      /// Close the developer tools.
      /// </summary>
      procedure CloseDevTools(const aDevTools : TWinControl = nil);
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
      function  CreateBrowser(const aBrowserParent : TWinControl = nil; const aWindowName : ustring = ''; const aContext : ICefRequestContext = nil; const aExtraInfo : ICefDictionaryValue = nil) : boolean; overload; virtual;
      /// Copy the DC to a bitmap stream. Only works on Windows with browsers without GPU acceleration.
      /// It's recommended to use the "Page.captureScreenshot" DevTools method instead.
      /// </summary>
      function  SaveAsBitmapStream(const aStream : TStream) : boolean;
      /// <summary>
      /// Copy the DC to a TBitmap. Only works on Windows with browsers without GPU acceleration.
      /// It's recommended to use the "Page.captureScreenshot" DevTools method instead.
      /// </summary>
      function  TakeSnapshot(var aBitmap : TBitmap) : boolean;
  end;

{$IFDEF FPC}
procedure Register;
{$ENDIF}

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
  {$IFDEF DELPHI16_UP}
  System.SysUtils, System.Math,
  {$ELSE}
  SysUtils, Math,
  {$ENDIF}
  uCEFMiscFunctions;

{ TChromium }

{$IFDEF MSWINDOWS}
procedure TChromium.InitializeDragAndDrop(const aDropTargetCtrl: TWinControl);
begin
  if (aDropTargetCtrl <> nil) then
    inherited InitializeDragAndDrop(aDropTargetCtrl.Handle);
end;
{$ENDIF MSWINDOWS}

procedure TChromium.InitializeDevToolsWindowInfo(aDevTools: TWinControl);
var
  TempHandle : TCefWindowHandle;
begin
  if (aDevTools <> nil) then
    DefaultInitializeDevToolsWindowInfo(aDevTools.Handle, aDevTools.ClientRect, aDevTools.Name)
   else
    begin
      InitializeWindowHandle(TempHandle);
      DefaultInitializeDevToolsWindowInfo(TempHandle, Rect(0, 0, 0, 0), '');
    end;
end;

procedure TChromium.ShowDevTools(inspectElementAt: TPoint; const aDevTools : TWinControl);
begin
  if Initialized then
    begin
      InitializeDevToolsWindowInfo(aDevTools);
      inherited ShowDevTools(inspectElementAt, @FDevWindowInfo.WindowInfoRecord);
    end;
end;

procedure TChromium.CloseDevTools(const aDevTools : TWinControl);
begin
  if Initialized then
    begin
      if (aDevTools <> nil) then
        inherited CloseDevTools(aDevTools.Handle)
       else
        inherited CloseDevTools(0);
    end;
end;

function TChromium.GetParentForm : TCustomForm;
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

function TChromium.GetParentFormHandle : TCefWindowHandle;
var
  TempForm : TCustomForm;
begin
  Result   := inherited GetParentFormHandle;
  TempForm := GetParentForm;

  if (TempForm <> nil) and TempForm.HandleAllocated then
    Result := TempForm.Handle
   else
    if (Application          <> nil) and
       (Application.MainForm <> nil) and
       Application.MainForm.HandleAllocated then
      Result := Application.MainForm.Handle;
end;

procedure TChromium.MoveFormTo(const x, y: Integer);
var
  TempForm : TCustomForm;
  TempRect : TRect;
begin
  TempForm := GetParentForm;

  if (TempForm <> nil) then
    begin
      TempRect.Left   := min(max(x, max(screen.DesktopLeft, 0)), screen.DesktopWidth  - TempForm.Width);
      TempRect.Top    := min(max(y, max(screen.DesktopTop,  0)), screen.DesktopHeight - TempForm.Height);
      TempRect.Right  := TempRect.Left + TempForm.Width  - 1;
      TempRect.Bottom := TempRect.Top  + TempForm.Height - 1;

      TempForm.SetBounds(TempRect.Left, TempRect.Top, TempRect.Right - TempRect.Left + 1, TempRect.Bottom - TempRect.Top + 1);
    end;
end;

procedure TChromium.MoveFormBy(const x, y: Integer);
var
  TempForm : TCustomForm;
  TempRect : TRect;
begin
  TempForm := GetParentForm;

  if (TempForm <> nil) then
    begin
      TempRect.Left   := min(max(TempForm.Left + x, max(screen.DesktopLeft, 0)), screen.DesktopWidth  - TempForm.Width);
      TempRect.Top    := min(max(TempForm.Top  + y, max(screen.DesktopTop,  0)), screen.DesktopHeight - TempForm.Height);
      TempRect.Right  := TempRect.Left + TempForm.Width  - 1;
      TempRect.Bottom := TempRect.Top  + TempForm.Height - 1;

      TempForm.SetBounds(TempRect.Left, TempRect.Top, TempRect.Right - TempRect.Left + 1, TempRect.Bottom - TempRect.Top + 1);
    end;
end;

procedure TChromium.ResizeFormWidthTo(const x : Integer);
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

procedure TChromium.ResizeFormHeightTo(const y : Integer);
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

procedure TChromium.SetFormLeftTo(const x : Integer);
var
  TempForm : TCustomForm;
begin
  TempForm := GetParentForm;

  if (TempForm <> nil) then
    TempForm.Left := min(max(x, max(screen.DesktopLeft, 0)), screen.DesktopWidth  - TempForm.Width);
end;

procedure TChromium.SetFormTopTo(const y : Integer);
var
  TempForm : TCustomForm;
begin
  TempForm := GetParentForm;

  if (TempForm <> nil) then
    TempForm.Top := min(max(y, max(screen.DesktopTop, 0)), screen.DesktopHeight - TempForm.Height);
end;

function TChromium.CreateBrowser(const aBrowserParent : TWinControl;
                                 const aWindowName    : ustring;
                                 const aContext       : ICefRequestContext;
                                 const aExtraInfo     : ICefDictionaryValue) : boolean;
var
  TempHandle : TCefWindowHandle;
  TempRect   : TRect;
begin
  if (aBrowserParent <> nil) then
    begin
      TempHandle := aBrowserParent.Handle;
      TempRect   := aBrowserParent.ClientRect;
    end
   else
    begin
      InitializeWindowHandle(TempHandle);
      TempRect := rect(0, 0, 0, 0);
    end;

  Result := inherited CreateBrowser(TempHandle, TempRect, aWindowName, aContext, aExtraInfo);
end;

function TChromium.SaveAsBitmapStream(const aStream : TStream) : boolean;
{$IFDEF MSWINDOWS}
var
  TempDC   : HDC;
  TempRect : TRect;
{$ENDIF}
begin
  Result := False;

  {$IFDEF MSWINDOWS}
  if not(FIsOSR) and (FRenderCompHWND <> 0) and (aStream <> nil) then
    begin
      TempDC := GetDC(FRenderCompHWND);

      if (TempDC <> 0) then
        try
          GetClientRect(FRenderCompHWND, TempRect);
          Result := CopyDCToBitmapStream(TempDC, TempRect, aStream);
        finally
          ReleaseDC(FRenderCompHWND, TempDC);
        end;
    end;
  {$ENDIF}
end;

function TChromium.TakeSnapshot(var aBitmap : TBitmap) : boolean;
{$IFDEF MSWINDOWS}
var
  TempDC     : HDC;
  TempRect   : TRect;
  TempWidth  : Integer;
  TempHeight : Integer;
{$ENDIF}
begin
  Result := False;

  {$IFDEF MSWINDOWS}
  if not(FIsOSR) and (FRenderCompHWND <> 0) then
    begin
      GetClientRect(FRenderCompHWND, TempRect);

      TempWidth  := TempRect.Right  - TempRect.Left;
      TempHeight := TempRect.Bottom - TempRect.Top;

      if (TempWidth <= 0) or (TempHeight <= 0) then exit;

      if (aBitmap <> nil) then FreeAndNil(aBitmap);

      aBitmap        := TBitmap.Create;
      aBitmap.Height := TempHeight;
      aBitmap.Width  := TempWidth;

      TempDC := GetDC(FRenderCompHWND);

      if (TempDC <> 0) then
        try
          Result := BitBlt(aBitmap.Canvas.Handle, 0, 0, TempWidth, TempHeight,
                           TempDC, 0, 0, SRCCOPY);
        finally
          ReleaseDC(FRenderCompHWND, TempDC);
        end;
    end;
  {$ENDIF}
end;

{$IFDEF FPC}
procedure Register;
begin
  {$I res/tchromium.lrs}
  RegisterComponents('Chromium', [TChromium]);
end;
{$ENDIF}

end.
