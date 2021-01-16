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

unit uCEFWinControl;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

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
  uCEFTypes, uCEFInterfaces;

type
  TCEFWinControl = class(TWinControl)
    protected
      function  GetChildWindowHandle : THandle; virtual;
      procedure Resize; override;

    public
      function    TakeSnapshot(var aBitmap : TBitmap) : boolean;
      function    DestroyChildWindow : boolean;
      procedure   CreateHandle; override;
      procedure   InvalidateChildren;
      procedure   UpdateSize; virtual;

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

function TCEFWinControl.GetChildWindowHandle : THandle;
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
  if HandleAllocated then RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_ALLCHILDREN);
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
  if (aBitmap = nil) then exit;

  TempHWND := ChildWindowHandle;
  if (TempHWND = 0) then exit;
  {$IFDEF DELPHI16_UP}Winapi.{$ENDIF}Windows.GetClientRect(TempHWND, TempRect);
  TempDC     := GetDC(TempHWND);
  TempWidth  := TempRect.Right  - TempRect.Left;
  TempHeight := TempRect.Bottom - TempRect.Top;

  aBitmap        := TBitmap.Create;
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
begin
  {$IFDEF MSWINDOWS}
  TempHWND := ChildWindowHandle;
  Result   := (TempHWND <> 0) and DestroyWindow(TempHWND);
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

procedure TCEFWinControl.Resize;
begin
  inherited Resize;

  UpdateSize;
end;

end.
