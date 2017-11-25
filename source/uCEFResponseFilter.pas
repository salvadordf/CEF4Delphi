// ************************************************************************
// ***************************** CEF4Delphi *******************************
// ************************************************************************
//
// CEF4Delphi is based on DCEF3 which uses CEF3 to embed a chromium-based
// browser in Delphi applications.
//
// The original license of DCEF3 still applies to CEF4Delphi.
//
// For more information about CEF4Delphi visit :
//         https://www.briskbard.com/index.php?lang=en&pageid=cef
//
//        Copyright © 2017 Salvador Díaz Fau. All rights reserved.
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

unit uCEFResponseFilter;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefResponseFilterOwn = class(TCefBaseRefCountedOwn, ICefResponseFilter)
  protected
    function InitFilter: Boolean; virtual; abstract;
    function Filter(dataIn: Pointer; dataInSize : NativeUInt; dataInRead: PNativeUInt; dataOut: Pointer; dataOutSize : NativeUInt; dataOutWritten: PNativeUInt): TCefResponseFilterStatus; virtual; abstract;
  public
    constructor Create; virtual;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;

function cef_response_filter_init_filter(self: PCefResponseFilter): Integer; stdcall;
begin
  with TCefResponseFilterOwn(CefGetObject(self)) do
    Result := Ord(InitFilter());
end;

function cef_response_filter_filter(self: PCefResponseFilter; data_in: Pointer; data_in_size : NativeUInt; var data_in_read: NativeUInt;
  data_out: Pointer; data_out_size: NativeUInt; var data_out_written: NativeUInt): TCefResponseFilterStatus; stdcall;
begin
  with TCefResponseFilterOwn(CefGetObject(self)) do
    Result := Filter(data_in, data_in_size, @data_in_read, data_out, data_out_size, @data_out_written);
end;

constructor TCefResponseFilterOwn.Create;
begin
  CreateData(SizeOf(TCefResponseFilter));
  with PCefResponseFilter(FData)^ do
  begin
    init_filter := cef_response_filter_init_filter;
    filter := cef_response_filter_filter;
  end;
end;

end.
