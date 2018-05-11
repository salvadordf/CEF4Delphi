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
//        Copyright � 2018 Salvador D�az Fau. All rights reserved.
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

unit uCEFCompletionCallback;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces;

type
  TCefCompletionCallbackOwn = class(TCefBaseRefCountedOwn, ICefCompletionCallback)
    protected
      procedure OnComplete; virtual;

    public
      constructor Create; virtual;
  end;

  TCefFastCompletionCallback = class(TCefCompletionCallbackOwn)
    protected
      FProc: TCefCompletionCallbackProc;

      procedure OnComplete; override;

    public
      constructor Create(const proc: TCefCompletionCallbackProc); reintroduce;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFTypes;

procedure cef_completion_callback_on_complete(self: PCefCompletionCallback); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefCompletionCallbackOwn) then
    TCefCompletionCallbackOwn(TempObject).OnComplete;
end;

// TCefCompletionHandlerOwn

constructor TCefCompletionCallbackOwn.Create;
begin
  inherited CreateData(SizeOf(TCefCompletionCallback));

  PCefCompletionCallback(FData).on_complete := cef_completion_callback_on_complete;
end;

procedure TCefCompletionCallbackOwn.OnComplete;
begin
  //
end;

// TCefFastCompletionHandler

constructor TCefFastCompletionCallback.Create(const proc: TCefCompletionCallbackProc);
begin
  inherited Create;

  FProc := proc;
end;

procedure TCefFastCompletionCallback.OnComplete;
begin
  FProc();
end;

end.
