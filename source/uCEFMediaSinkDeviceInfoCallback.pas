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

unit uCEFMediaSinkDeviceInfoCallback;

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
  TCefMediaSinkDeviceInfoCallbackOwn = class(TCefBaseRefCountedOwn, ICefMediaSinkDeviceInfoCallback)
    protected
      procedure OnMediaSinkDeviceInfo(const ip_address: ustring; port: integer; const model_name: ustring); virtual; abstract;
    public
      constructor Create; virtual;
  end;

  TCefFastMediaSinkDeviceInfoCallback = class(TCefMediaSinkDeviceInfoCallbackOwn)
    protected
      FCallback: TCefMediaSinkDeviceInfoCallbackProc;

      procedure OnMediaSinkDeviceInfo(const ip_address: ustring; port: integer; const model_name: ustring); override;
    public
      constructor Create(const callback: TCefMediaSinkDeviceInfoCallbackProc); reintroduce;
      destructor  Destroy; override;
  end;

  TCefCustomMediaSinkDeviceInfoCallback = class(TCefMediaSinkDeviceInfoCallbackOwn)
    protected
      FEvents : Pointer;

      procedure OnMediaSinkDeviceInfo(const ip_address: ustring; port: integer; const model_name: ustring); override;
    public
      constructor Create(const aEvents : IChromiumEvents); reintroduce;
      destructor  Destroy; override;
  end;

implementation

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  uCEFMiscFunctions, uCEFLibFunctions, uCEFMediaSink;

procedure cef_media_sink_device_info_callback_on_media_sink_device_info(self        : PCefMediaSinkDeviceInfoCallback;
                                                                        device_info : PCefMediaSinkDeviceInfo); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefMediaSinkDeviceInfoCallbackOwn) then
    TCefMediaSinkDeviceInfoCallbackOwn(TempObject).OnMediaSinkDeviceInfo(CefString(@device_info^.ip_address),
                                                                         device_info^.port,
                                                                         CefString(@device_info^.model_name));
end;


// ****************************************************************
// ************** TCefMediaSinkDeviceInfoCallbackOwn **************
// ****************************************************************

constructor TCefMediaSinkDeviceInfoCallbackOwn.Create;
begin
  inherited CreateData(SizeOf(TCefMediaSinkDeviceInfoCallback));

  PCefMediaSinkDeviceInfoCallback(FData)^.on_media_sink_device_info :=
    {$IFDEF FPC}@{$ENDIF}cef_media_sink_device_info_callback_on_media_sink_device_info;
end;


// *****************************************************************
// ************** TCefFastMediaSinkDeviceInfoCallback **************
// *****************************************************************

constructor TCefFastMediaSinkDeviceInfoCallback.Create(const callback: TCefMediaSinkDeviceInfoCallbackProc);
begin
  inherited Create;

  FCallback := callback;
end;

procedure TCefFastMediaSinkDeviceInfoCallback.OnMediaSinkDeviceInfo(const ip_address: ustring; port: integer; const model_name: ustring);
begin
  if assigned(FCallback) then
    FCallback(ip_address, port, model_name);
end;

destructor TCefFastMediaSinkDeviceInfoCallback.Destroy;
begin
  FCallback := nil;

  inherited Destroy;
end;


// *******************************************************************
// ************** TCefCustomMediaSinkDeviceInfoCallback **************
// *******************************************************************

constructor TCefCustomMediaSinkDeviceInfoCallback.Create(const aEvents : IChromiumEvents);
begin
  inherited Create;

  FEvents := Pointer(aEvents);
end;

destructor TCefCustomMediaSinkDeviceInfoCallback.Destroy;
begin
  FEvents := nil;

  inherited Destroy;
end;

procedure TCefCustomMediaSinkDeviceInfoCallback.OnMediaSinkDeviceInfo(const ip_address: ustring; port: integer; const model_name: ustring);
begin
  try
    try
      if (FEvents <> nil) then
        IChromiumEvents(FEvents).doOnMediaSinkDeviceInfo(ip_address, port, model_name);
    except
      on e : exception do
        if CustomExceptionHandler('TCefCustomMediaSinkDeviceInfoCallback.OnMediaSinkDeviceInfo', e) then raise;
    end;
  finally
    FEvents := nil;
  end;
end;

end.
