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
//        Copyright © 2020 Salvador Diaz Fau. All rights reserved.
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

unit uCEFViewDelegate;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes, System.SysUtils,
  {$ELSE}
  Classes, SysUtils,
  {$ENDIF}
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefViewDelegateRef = class(TCefBaseRefCountedRef, ICefViewDelegate)
    protected
      function  GetPreferredSize(const view: ICefView): TCefSize;
      function  GetMinimumSize(const view: ICefView): TCefSize;
      function  GetMaximumSize(const view: ICefView): TCefSize;
      function  GetHeightForWidth(const view: ICefView; width: Integer): Integer;
      procedure OnParentViewChanged(const view: ICefView; added: boolean; const parent: ICefView);
      procedure OnChildViewChanged(const view: ICefView; added: boolean; const child: ICefView);
      procedure OnFocus(const view: ICefView);
      procedure OnBlur(const view: ICefView);

    public
      class function UnWrap(data: Pointer): ICefViewDelegate;
  end;

implementation

uses
  uCEFLibFunctions, uCEFMiscFunctions;

function TCefViewDelegateRef.GetPreferredSize(const view: ICefView): TCefSize;
begin
  Result := PCefViewDelegate(FData)^.get_preferred_size(PCefViewDelegate(FData),
                                                        CefGetData(view));
end;

function TCefViewDelegateRef.GetMinimumSize(const view: ICefView): TCefSize;
begin
  Result := PCefViewDelegate(FData)^.get_minimum_size(PCefViewDelegate(FData),
                                                      CefGetData(view));
end;

function TCefViewDelegateRef.GetMaximumSize(const view: ICefView): TCefSize;
begin
  Result := PCefViewDelegate(FData)^.get_maximum_size(PCefViewDelegate(FData),
                                                      CefGetData(view));
end;

function TCefViewDelegateRef.GetHeightForWidth(const view: ICefView; width: Integer): Integer;
begin
  Result := PCefViewDelegate(FData)^.get_height_for_width(PCefViewDelegate(FData),
                                                          CefGetData(view),
                                                          width);
end;

procedure TCefViewDelegateRef.OnParentViewChanged(const view: ICefView; added: boolean; const parent: ICefView);
begin
  PCefViewDelegate(FData)^.on_parent_view_changed(PCefViewDelegate(FData),
                                                  CefGetData(view),
                                                  ord(added),
                                                  CefGetData(parent));
end;

procedure TCefViewDelegateRef.OnChildViewChanged(const view: ICefView; added: boolean; const child: ICefView);
begin
  PCefViewDelegate(FData)^.on_child_view_changed(PCefViewDelegate(FData),
                                                 CefGetData(view),
                                                 ord(added),
                                                 CefGetData(child));
end;

procedure TCefViewDelegateRef.OnFocus(const view: ICefView);
begin
  PCefViewDelegate(FData)^.on_focus(PCefViewDelegate(FData),
                                    CefGetData(view));
end;

procedure TCefViewDelegateRef.OnBlur(const view: ICefView);
begin
  PCefViewDelegate(FData)^.on_blur(PCefViewDelegate(FData),
                                   CefGetData(view));
end;

class function TCefViewDelegateRef.UnWrap(data: Pointer): ICefViewDelegate;
begin
  if (data <> nil) then
    Result := Create(data) as ICefViewDelegate
   else
    Result := nil;
end;

end.

