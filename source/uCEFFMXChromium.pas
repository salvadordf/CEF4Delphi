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

unit uCEFFMXChromium;

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  System.Classes, System.Types,
  {$IFDEF MSWINDOWS}
  WinApi.Windows, WinApi.Messages, FMX.Platform.Win,
  {$ENDIF}
  FMX.Types, FMX.Platform, FMX.Forms, FMX.Controls, FMX.Graphics,
  uCEFTypes, uCEFInterfaces, uCEFChromiumCore;

type
  {$IFNDEF FPC}{$IFDEF DELPHI16_UP}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}{$ENDIF}
  TFMXChromium = class(TChromiumCore, IChromiumEvents)
    protected
      function  GetParentFormHandle : TCefWindowHandle; override;
      function  GetParentForm : TCustomForm;
      function  GetScreenScale : Single;
      procedure InitializeDevToolsWindowInfo; virtual;

    public
      procedure ShowDevTools(inspectElementAt: TPoint);
      procedure CloseDevTools;

      procedure MoveFormTo(const x, y: Integer);
      procedure MoveFormBy(const x, y: Integer);
      procedure ResizeFormWidthTo(const x : Integer);
      procedure ResizeFormHeightTo(const y : Integer);
      procedure SetFormLeftTo(const x : Integer);
      procedure SetFormTopTo(const y : Integer);

      function  CreateBrowser(const aWindowName : ustring = ''; const aContext : ICefRequestContext = nil; const aExtraInfo : ICefDictionaryValue = nil) : boolean; overload; virtual;
      function  SaveAsBitmapStream(var aStream : TStream; const aRect : System.Types.TRect) : boolean;
      function  TakeSnapshot(var aBitmap : TBitmap; const aRect : System.Types.TRect) : boolean;

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
  {$IFDEF MSWINDOWS}FMX.Helpers.Win,{$ENDIF}
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
begin
  DefaultInitializeDevToolsWindowInfo(0, Rect(0, 0, 0, 0), '');
end;

procedure TFMXChromium.ShowDevTools(inspectElementAt: TPoint);
begin
  if Initialized then
    begin
      InitializeDevToolsWindowInfo;
      inherited ShowDevTools(inspectElementAt, @FDevWindowInfo);
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
  {$IFDEF DELPHI17_UP}
  TempRect : TRect;
  {$ENDIF}
begin
  TempForm := GetParentForm;
  {$IFDEF DELPHI17_UP}
  if (TempForm <> nil) then
    begin
      TempRect.Left   := min(max(x, max(screen.DesktopLeft, 0)), screen.DesktopWidth  - TempForm.Width);
      TempRect.Top    := min(max(y, max(screen.DesktopTop,  0)), screen.DesktopHeight - TempForm.Height);
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
  {$IFDEF DELPHI17_UP}
  TempRect : TRect;
  {$ENDIF}
begin
  TempForm := GetParentForm;
  {$IFDEF DELPHI17_UP}
  if (TempForm <> nil) then
    begin
      TempRect.Left   := min(max(TempForm.Left + x, max(screen.DesktopLeft, 0)), screen.DesktopWidth  - TempForm.Width);
      TempRect.Top    := min(max(TempForm.Top  + y, max(screen.DesktopTop,  0)), screen.DesktopHeight - TempForm.Height);
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
    {$IFDEF DELPHI17_UP}
    TempForm.Left := min(max(x, max(screen.DesktopLeft, 0)), screen.DesktopWidth  - TempForm.Width);
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
    {$IFDEF DELPHI17_UP}
    TempForm.Top := min(max(y, max(screen.DesktopTop, 0)), screen.DesktopHeight - TempForm.Height);
    {$ELSE}
    TempForm.Top := y;
    {$ENDIF}
end;

function TFMXChromium.SaveAsBitmapStream(var aStream : TStream; const aRect : System.Types.TRect) : boolean;
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

    if SaveAsBitmapStream(TStream(TempStream), aRect) then
      begin
        aBitmap.LoadFromStream(TempStream);
        Result := True;
      end;
  finally
    FreeAndNil(TempStream);
  end;
end;

end.
