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

unit uCEFGetGeolocationCallback;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefGetGeolocationCallbackOwn = class(TCefBaseRefCountedOwn, ICefGetGeolocationCallback)
    protected
      procedure OnLocationUpdate(const position: PCefGeoposition); virtual;

    public
      constructor Create; virtual;
  end;

  TOnLocationUpdate = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const position: PCefGeoposition);

  TCefFastGetGeolocationCallback = class(TCefGetGeolocationCallbackOwn)
    protected
      FCallback: TOnLocationUpdate;

      procedure OnLocationUpdate(const position: PCefGeoposition); override;

    public
      constructor Create(const callback: TOnLocationUpdate); reintroduce;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;

procedure cef_get_geolocation_callback_on_location_update(
  self: PCefGetGeolocationCallback; const position: PCefGeoposition); stdcall;
begin
  with TCefGetGeolocationCallbackOwn(CefGetObject(self)) do
    OnLocationUpdate(position);
end;

// TCefGetGeolocationCallbackOwn

constructor TCefGetGeolocationCallbackOwn.Create;
begin
  inherited CreateData(SizeOf(TCefGetGeolocationCallback));
  with PCefGetGeolocationCallback(FData)^ do
    on_location_update := cef_get_geolocation_callback_on_location_update;
end;

procedure TCefGetGeolocationCallbackOwn.OnLocationUpdate(
  const position: PCefGeoposition);
begin

end;

// TCefFastGetGeolocationCallback

constructor TCefFastGetGeolocationCallback.Create(
  const callback: TOnLocationUpdate);
begin
  inherited Create;
  FCallback := callback;
end;

procedure TCefFastGetGeolocationCallback.OnLocationUpdate(
  const position: PCefGeoposition);
begin
  FCallback(position);
end;

end.
