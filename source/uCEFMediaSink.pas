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

unit uCEFMediaSink;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefMediaSinkRef = class(TCefBaseRefCountedRef, ICefMediaSink)
  protected
    function  GetId: ustring;
    function  GetName: ustring;
    function  GetDescription: ustring;
    function  GetIconType: TCefMediaSinkIconType;
    procedure GetDeviceInfo(const callback: ICefMediaSinkDeviceInfoCallback);
    function  IsCastSink: boolean;
    function  IsDialSink: boolean;
    function  IsCompatibleWith(const source: ICefMediaSource): boolean;
  public
    class function UnWrap(data: Pointer): ICefMediaSink;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;

function TCefMediaSinkRef.GetId: ustring;
begin
  Result := CefStringFreeAndGet(PCefMediaSink(FData)^.get_id(PCefMediaSink(FData)));
end;

function TCefMediaSinkRef.GetName: ustring;
begin
  Result := CefStringFreeAndGet(PCefMediaSink(FData)^.get_name(PCefMediaSink(FData)));
end;

function TCefMediaSinkRef.GetDescription: ustring;
begin
  Result := CefStringFreeAndGet(PCefMediaSink(FData)^.get_description(PCefMediaSink(FData)));
end;

function TCefMediaSinkRef.GetIconType: TCefMediaSinkIconType;
begin
  Result := PCefMediaSink(FData)^.get_icon_type(PCefMediaSink(FData));
end;

procedure TCefMediaSinkRef.GetDeviceInfo(const callback: ICefMediaSinkDeviceInfoCallback);
begin
  PCefMediaSink(FData)^.get_device_info(PCefMediaSink(FData), CefGetData(callback));
end;

function TCefMediaSinkRef.IsCastSink: Boolean;
begin
  Result := PCefMediaSink(FData)^.is_cast_sink(PCefMediaSink(FData)) <> 0;
end;

function TCefMediaSinkRef.IsDialSink: Boolean;
begin
  Result := PCefMediaSink(FData)^.is_dial_sink(PCefMediaSink(FData)) <> 0;
end;

function TCefMediaSinkRef.IsCompatibleWith(const source: ICefMediaSource): boolean;
begin
  Result := PCefMediaSink(FData)^.is_compatible_with(PCefMediaSink(FData), CefGetData(source)) <> 0;
end;

class function TCefMediaSinkRef.UnWrap(data: Pointer): ICefMediaSink;
begin
  if (data <> nil) then
    Result := Create(data) as ICefMediaSink
   else
    Result := nil;
end;

end.
