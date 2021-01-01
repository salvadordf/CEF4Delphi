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

unit uCEFFMXWindowParent;

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  System.Classes, System.Types, System.UITypes,
  {$IFDEF MSWINDOWS}
  WinApi.Windows,
  {$ENDIF}
  FMX.Controls, FMX.Types, FMX.Forms;

type
  {$IFNDEF FPC}{$IFDEF DELPHI16_UP}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}{$ENDIF}
  TFMXWindowParent = class(TCommonCustomForm)
    protected
      {$IFDEF MSWINDOWS}
      function  GetChildWindowHandle : HWND;
      procedure UpdateSize;
      {$ENDIF}
      procedure Resize; override;

    public
      {$IFDEF MSWINDOWS}
      procedure Reparent(const aNewParentHandle : TWindowHandle);
      property  ChildWindowHandle : HWND   read GetChildWindowHandle;
      {$ENDIF}

    published
      property Visible;
      property Height;
      property Width;
      property Touch;
      property OnGesture;
  end;

implementation

// This class inherits from TCommonCustomForm because CEF needs a Windows handle
// to create the browser in normal mode.

// TFMXWindowParent has to be created and resized at runtime.
// It's also necessary to call "Reparent" to add this component as a child component to your form.

uses
  System.SysUtils, FMX.Platform, FMX.Platform.Win;

procedure TFMXWindowParent.Resize;
begin
  inherited Resize;

  {$IFDEF MSWINDOWS}
  UpdateSize;
  {$ENDIF}
end;

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
begin
  TempChildHWND := ChildWindowHandle;
  if (TempChildHWND = 0) then exit;

  TempHWND := BeginDeferWindowPos(1);

  try
    TempClientRect  := ClientRect;
    TempRect.Left   := round(TempClientRect.Left);
    TempRect.Top    := round(TempClientRect.Top);
    TempRect.Right  := round(TempClientRect.Right);
    TempRect.Bottom := round(TempClientRect.Bottom);

    TempHWND := DeferWindowPos(TempHWND, TempChildHWND, HWND_TOP,
                               TempRect.left, TempRect.top, TempRect.right - TempRect.left, TempRect.bottom - TempRect.top,
                               SWP_NOZORDER);
  finally
    EndDeferWindowPos(TempHWND);
  end;
end;

procedure TFMXWindowParent.Reparent(const aNewParentHandle : TWindowHandle);
var
  TempChildHandle, TempParentHandle : HWND;
begin
  if (aNewParentHandle <> nil) then
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


end.
