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

unit uCEFEndTracingCallback;

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
  TCefEndTracingCallbackOwn = class(TCefBaseRefCountedOwn, ICefEndTracingCallback)
    protected
      procedure OnEndTracingComplete(const tracingFile: ustring); virtual;
    public
      constructor Create; virtual;
  end;

  TCefFastEndTracingCallback = class(TCefEndTracingCallbackOwn)
    protected
      FCallback: TCefEndTracingCallbackProc;
      procedure OnEndTracingComplete(const tracingFile: ustring); override;
    public
      constructor Create(const callback: TCefEndTracingCallbackProc); reintroduce;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;

// TCefEndTracingCallbackOwn

procedure cef_end_tracing_callback_on_end_tracing_complete(      self         : PCefEndTracingCallback;
                                                           const tracing_file : PCefString); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefEndTracingCallbackOwn) then
    TCefEndTracingCallbackOwn(TempObject).OnEndTracingComplete(CefString(tracing_file));
end;

constructor TCefEndTracingCallbackOwn.Create;
begin
  inherited CreateData(SizeOf(TCefEndTracingCallback));

  with PCefEndTracingCallback(FData)^ do
    on_end_tracing_complete := {$IFDEF FPC}@{$ENDIF}cef_end_tracing_callback_on_end_tracing_complete;
end;

procedure TCefEndTracingCallbackOwn.OnEndTracingComplete(const tracingFile: ustring);
begin
  //
end;

// TCefFastEndTracingCallback

constructor TCefFastEndTracingCallback.Create(const callback: TCefEndTracingCallbackProc);
begin
  inherited Create;

  FCallback := callback;
end;

procedure TCefFastEndTracingCallback.OnEndTracingComplete(const tracingFile: ustring);
begin
  FCallback(tracingFile);
end;

end.
