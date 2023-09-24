unit uCEFWinControl;

{$I cef.inc}

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
  {$IFDEF MACOSX}
    {$ModeSwitch objectivec1}
  {$ENDIF}
{$ENDIF}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
    {$IFDEF MSWINDOWS}WinApi.Windows, {$ENDIF} System.Classes, Vcl.Controls, Vcl.Graphics,
  {$ELSE}
    {$IFDEF MSWINDOWS}Windows,{$ENDIF} Classes, Forms, Controls, Graphics,
    {$IFDEF FPC}
    LCLProc, LCLType, LCLIntf, LResources, InterfaceBase,
    {$ENDIF}
  {$ENDIF}
  {$IFDEF FPC}{$IFDEF MACOSX}
  CocoaAll,
  {$ENDIF}{$ENDIF}
  uCEFTypes, uCEFInterfaces;

type
  /// <summary>
  /// Custom TWinControl used by CEF browsers.
  /// </summary>
  TCEFWinControl = class(TWinControl)
    protected
      function  GetChildWindowHandle : {$IFNDEF MSWINDOWS}{$IFDEF FPC}LclType.{$ENDIF}{$ENDIF}THandle; virtual;
      procedure Resize; override;

    public
      /// <summary>
      /// Take a snapshot of the browser contents into aBitmap. This function only works in Windows without hardware acceleration.
      /// </summary>
      function    TakeSnapshot(var aBitmap : TBitmap) : boolean;
      /// <summary>
      /// Destroy the child windows created by the browser.
      /// </summary>
      function    DestroyChildWindow : boolean;
      /// <summary>
      /// Exposes the CreateHandle procedure to create the Handle at any time.
      /// </summary>
      procedure   CreateHandle; override;
      /// <summary>
      /// Invalidate the child windows created by the browser.
      /// </summary>
      procedure   InvalidateChildren;
      /// <summary>
      /// Updates the size of the child windows created by the browser.
      /// </summary>
      procedure   UpdateSize; virtual;
      /// <summary>
      /// Handle of the first child window created by the browser.
      /// </summary>
      property  ChildWindowHandle : THandle  read GetChildWindowHandle;

    published
      property  Align;
      property  Anchors;
      property  Color;
      property  Constraints;
      property  TabStop;
      property  TabOrder;
      property  Visible;
      property  Enabled;
      property  ShowHint;
      property  Hint;
      property  DragKind;
      property  DragCursor;
      property  DragMode;
      property  OnResize;        
      property  OnEnter;
      property  OnExit;
      property  OnDragDrop;
      property  OnDragOver;
      property  OnStartDrag;
      property  OnEndDrag;
      {$IFNDEF FPC}
      property  OnCanResize;
      {$ENDIF}
      {$IFDEF DELPHI14_UP}
      property  Touch;
      property  OnGesture;
      {$ENDIF}
      property  DoubleBuffered;
      {$IFDEF DELPHI12_UP}
      property  ParentDoubleBuffered;
      {$ENDIF}
  end;

implementation

uses
  uCEFMiscFunctions, uCEFClient, uCEFConstants;

function TCEFWinControl.GetChildWindowHandle : {$IFNDEF MSWINDOWS}{$IFDEF FPC}LclType.{$ENDIF}{$ENDIF}THandle;
begin
  {$IFDEF MSWINDOWS}
  if not(csDesigning in ComponentState) and HandleAllocated then
    Result := GetWindow(Handle, GW_CHILD)
   else
  {$ENDIF}
    Result := 0;
end;

procedure TCEFWinControl.CreateHandle;
begin
  inherited CreateHandle;
end;

procedure TCEFWinControl.InvalidateChildren;
begin
  if HandleAllocated then
    RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_ALLCHILDREN);
end;

procedure TCEFWinControl.UpdateSize;
{$IFDEF MSWINDOWS}
var
  TempRect : TRect;
  TempHWND : THandle;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  TempHWND := ChildWindowHandle;
  if (TempHWND = 0) then exit;

  TempRect := GetClientRect;

  SetWindowPos(TempHWND, 0,
               0, 0, TempRect.right, TempRect.bottom,
               SWP_NOZORDER);
  {$ENDIF}
end;

function TCEFWinControl.TakeSnapshot(var aBitmap : TBitmap) : boolean;
{$IFDEF MSWINDOWS}
var
  TempHWND   : HWND;
  TempDC     : HDC;
  TempRect   : TRect;
  TempWidth  : Integer;
  TempHeight : Integer;
{$ENDIF}
begin
  Result := False;
  {$IFDEF MSWINDOWS}

  TempHWND := ChildWindowHandle;
  if (TempHWND = 0) then exit;
  {$IFDEF DELPHI16_UP}Winapi.{$ENDIF}Windows.GetClientRect(TempHWND, TempRect);
  TempDC     := GetDC(TempHWND);
  TempWidth  := TempRect.Right  - TempRect.Left;
  TempHeight := TempRect.Bottom - TempRect.Top;

  if (aBitmap = nil) then
    aBitmap := TBitmap.Create;

  aBitmap.Height := TempHeight;
  aBitmap.Width  := TempWidth;

  Result := BitBlt(aBitmap.Canvas.Handle, 0, 0, TempWidth, TempHeight,
                   TempDC, 0, 0, SRCCOPY);

  ReleaseDC(TempHWND, TempDC);
  {$ENDIF}
end;

function TCEFWinControl.DestroyChildWindow : boolean;
{$IFDEF MSWINDOWS}
var
  TempHWND : HWND;
{$ENDIF}
{$IFDEF FPC}{$IFDEF MACOSX}
var
  ViewObj: NSObject;
{$ENDIF}{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  TempHWND := ChildWindowHandle;
  Result   := (TempHWND <> 0) and DestroyWindow(TempHWND);
  {$ELSE}
  Result := False;
  {$IFDEF FPC}{$IFDEF MACOSX}
  ViewObj := NSObject(ChildWindowHandle);
  if ViewObj <> nil then begin
    if ViewObj.isKindOfClass_(nsview) then begin
      NSView(ViewObj).removeFromSuperview;
      Result := True;
    end;
  end;
  {$ENDIF}{$ENDIF}
  {$ENDIF}
end;

procedure TCEFWinControl.Resize;
begin
  inherited Resize;

  UpdateSize;
end;

end.
