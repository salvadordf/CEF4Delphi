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

unit uCEFv8ArrayBufferReleaseCallback;

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
  TCefv8ArrayBufferReleaseCallbackOwn = class(TCefBaseRefCountedOwn, ICefv8ArrayBufferReleaseCallback)
    protected
      procedure ReleaseBuffer(buffer : Pointer); virtual;

    public
      constructor Create; virtual;
  end;

  TCefFastv8ArrayBufferReleaseCallback = class(TCefv8ArrayBufferReleaseCallbackOwn)
    protected
      FCallback: TCefv8ArrayBufferReleaseCallbackProc;

      procedure ReleaseBuffer(buffer : Pointer); override;

    public
      constructor Create(const callback: TCefv8ArrayBufferReleaseCallbackProc); reintroduce;
  end;

  TCefv8ArrayBufferReleaseCallbackRef = class(TCefBaseRefCountedRef, ICefv8ArrayBufferReleaseCallback)
    protected
      procedure ReleaseBuffer(buffer : Pointer); virtual;

    public
      class function UnWrap(data: Pointer): ICefv8ArrayBufferReleaseCallback;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;

procedure cef_v8array_buffer_release_callback_release_buffer(self   : PCefv8ArrayBufferReleaseCallback;
                                                             buffer : Pointer); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefv8ArrayBufferReleaseCallbackOwn) then
    TCefv8ArrayBufferReleaseCallbackOwn(TempObject).ReleaseBuffer(buffer);
end;


// TCefv8ArrayBufferReleaseCallbackOwn

constructor TCefv8ArrayBufferReleaseCallbackOwn.Create;
begin
  inherited CreateData(SizeOf(TCefv8ArrayBufferReleaseCallback));

  with PCefv8ArrayBufferReleaseCallback(FData)^ do
    release_buffer := {$IFDEF FPC}@{$ENDIF}cef_v8array_buffer_release_callback_release_buffer;
end;

procedure TCefv8ArrayBufferReleaseCallbackOwn.ReleaseBuffer(buffer: Pointer);
begin
  //
end;


// TCefFastv8ArrayBufferReleaseCallback

constructor TCefFastv8ArrayBufferReleaseCallback.Create(const callback: TCefv8ArrayBufferReleaseCallbackProc);
begin
  inherited Create;

  FCallback := callback;
end;

procedure TCefFastv8ArrayBufferReleaseCallback.ReleaseBuffer(buffer: Pointer);
begin
  FCallback(buffer);
end;


// TCefv8ArrayBufferReleaseCallbackRef

procedure TCefv8ArrayBufferReleaseCallbackRef.ReleaseBuffer(buffer : Pointer);
begin
  PCefv8ArrayBufferReleaseCallback(FData)^.release_buffer(PCefv8ArrayBufferReleaseCallback(FData), buffer);
end;

class function TCefv8ArrayBufferReleaseCallbackRef.UnWrap(data: Pointer): ICefv8ArrayBufferReleaseCallback;
begin
  if (data <> nil) then
    Result := Create(data) as ICefv8ArrayBufferReleaseCallback
   else
    Result := nil;
end;

end.
