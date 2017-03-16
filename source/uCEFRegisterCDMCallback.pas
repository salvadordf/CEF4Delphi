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

unit uCEFRegisterCDMCallback;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefRegisterCDMProc = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(result: TCefCDMRegistrationError; const error_message: ustring);

  TCefRegisterCDMCallbackOwn = class(TCefBaseRefCountedOwn, ICefRegisterCDMCallback)
    protected
      procedure OnCDMRegistrationComplete(result: TCefCDMRegistrationError; const error_message: ustring); virtual;

    public
      constructor Create; virtual;
  end;

  TCefFastRegisterCDMCallback = class(TCefRegisterCDMCallbackOwn)
    protected
      FCallback: TCefRegisterCDMProc;

      procedure OnCDMRegistrationComplete(result: TCefCDMRegistrationError; const error_message: ustring); override;

    public
      constructor Create(const callback: TCefRegisterCDMProc); reintroduce;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;

procedure cef_register_cdm_callback_on_cdm_registration_complete(self:PCefRegisterCDMCallback;
                                                                 result: TCefCDMRegistrationError;
                                                                 const error_message: PCefString); stdcall;
begin
  with TCefRegisterCDMCallbackOwn(CefGetObject(self)) do
    OnCDMRegistrationComplete(result, CefString(error_message));
end;

// TCefRegisterCDMCallbackOwn

constructor TCefRegisterCDMCallbackOwn.Create;
begin
  inherited CreateData(SizeOf(TCefRegisterCDMCallback));

  PCefRegisterCDMCallback(FData).on_cdm_registration_complete := cef_register_cdm_callback_on_cdm_registration_complete;
end;

procedure TCefRegisterCDMCallbackOwn.OnCDMRegistrationComplete(result: TCefCDMRegistrationError;
                                                               const error_message: ustring);
begin
  //
end;

// TCefFastRegisterCDMCallback

constructor TCefFastRegisterCDMCallback.Create(const callback: TCefRegisterCDMProc);
begin
  FCallback := callback;
end;

procedure TCefFastRegisterCDMCallback.OnCDMRegistrationComplete(result: TCefCDMRegistrationError;
                                                                const error_message: ustring);
begin
  FCallback(result, error_message);
end;


end.
