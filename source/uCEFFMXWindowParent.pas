unit uCEFFMXWindowParent;

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  System.Classes, System.Types, System.UITypes,
  {$IFDEF MSWINDOWS}
  WinApi.Windows,
  {$ENDIF}
  FMX.Controls, FMX.Types, FMX.Forms,
  uCEFConstants, uCEFFMXChromium;

type
  {$IFNDEF FPC}{$IFDEF DELPHI16_UP}[ComponentPlatformsAttribute(pfidWindows or pfidOSX or pfidLinux)]{$ENDIF}{$ENDIF}
  TFMXWindowParent = class(TCommonCustomForm)
    protected
      {$IFDEF MSWINDOWS}
      FChromium : TFMXChromium;
      function  GetChildWindowHandle : HWND;
      {$ENDIF}
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;
      {$IFDEF DELPHI17_UP}
      procedure Resize; override;
      {$ENDIF}
    public
      {$IFDEF MSWINDOWS}
      /// <summary>
      /// Set a new parent for this control.
      /// </summary>
      procedure Reparent(const aNewParentHandle : {$IFDEF DELPHI18_UP}TWindowHandle{$ELSE}TFmxHandle{$ENDIF});
      /// <summary>
      /// Updates the size of the child windows created by the browser.
      /// </summary>
      procedure UpdateSize;
      /// <summary>
      /// Handle of the first child window created by the browser.
      /// </summary>
      property  ChildWindowHandle : HWND          read GetChildWindowHandle;
      /// <summary>
      /// TChromium instance used by this component.
      /// </summary>
      property  Chromium          : TFMXChromium  read FChromium              write FChromium;
      {$ENDIF}
      {$IFNDEF DELPHI17_UP}
      procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
      {$ENDIF}


    published
      property Visible;
      property Height;
      property Width;
      {$IFDEF DELPHI17_UP}
      property Touch;
      property OnGesture;
      {$ENDIF}
  end;

implementation

// This class inherits from TCommonCustomForm because CEF needs a Windows handle
// to create the browser in normal mode.

// TFMXWindowParent has to be created and resized at runtime.
// It's also necessary to call "Reparent" to add this component as a child component to your form.

uses
  System.SysUtils, FMX.Platform, {$IFDEF MSWINDOWS}FMX.Platform.Win,{$ENDIF}
  uCEFApplicationCore;

{$IFDEF DELPHI17_UP}
procedure TFMXWindowParent.Resize;
begin
  inherited Resize;

  {$IFDEF MSWINDOWS}
  UpdateSize;
  {$ENDIF}
end;
{$ELSE}
procedure TFMXWindowParent.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);

  {$IFDEF MSWINDOWS}
  UpdateSize;
  {$ENDIF}
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
function TFMXWindowParent.GetChildWindowHandle : HWND;
var
  TempHWND : HWND;
begin
  TempHWND := FmxHandleToHWND(Handle);
  Result   := WinApi.Windows.GetWindow(TempHWND, GW_CHILD);
end;

procedure TFMXWindowParent.UpdateSize;
var
  TempHWND, TempChildHWND : HWND;
  TempRect : System.Types.TRect;
  TempClientRect : TRectF;
  TempScale : single;
begin
  TempChildHWND := ChildWindowHandle;
  if (TempChildHWND = 0) then exit;

  TempHWND := BeginDeferWindowPos(1);

  try
    if assigned(FChromium) then
      TempScale := FChromium.ScreenScale
     else
      TempScale := GlobalCEFApp.DeviceScaleFactor;

    TempClientRect  := ClientRect;
    TempRect.Left   := round(TempClientRect.Left);
    TempRect.Top    := round(TempClientRect.Top);
    TempRect.Right  := round(TempClientRect.Right  * TempScale);
    TempRect.Bottom := round(TempClientRect.Bottom * TempScale);

    TempHWND := DeferWindowPos(TempHWND, TempChildHWND, HWND_TOP,
                               TempRect.left, TempRect.top, TempRect.right - TempRect.left, TempRect.bottom - TempRect.top,
                               SWP_NOZORDER);
  finally
    EndDeferWindowPos(TempHWND);
  end;
end;

procedure TFMXWindowParent.Reparent(const aNewParentHandle : {$IFDEF DELPHI18_UP}TWindowHandle{$ELSE}TFmxHandle{$ENDIF});
var
  TempChildHandle, TempParentHandle : HWND;
begin
  {$IFDEF DELPHI18_UP}
  if (aNewParentHandle <> nil) then
  {$ELSE}
  if (aNewParentHandle <> 0) then
  {$ENDIF}
    begin
      TempChildHandle  := FmxHandleToHWND(Handle);
      TempParentHandle := FmxHandleToHWND(aNewParentHandle);

      if (TempChildHandle <> 0) and (TempParentHandle <> 0) then
        begin
          SetWindowLong(TempChildHandle, GWL_STYLE, WS_CHILDWINDOW);
          WinApi.Windows.SetParent(TempChildHandle, TempParentHandle);
        end;
    end;
end;
{$ENDIF}

procedure TFMXWindowParent.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  {$IFDEF MSWINDOWS}
  if (Operation = opRemove) and (AComponent = FChromium) then FChromium := nil;
  {$ENDIF}
end;


end.
