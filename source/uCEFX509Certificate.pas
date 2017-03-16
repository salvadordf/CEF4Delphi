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

unit uCEFX509Certificate;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes,
  {$ELSE}
  Classes,
  {$ENDIF}
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCEFX509CertificateRef = class(TCefBaseRefCountedRef, ICefX509Certificate)
    protected
      function GetSubject: ICefX509CertPrincipal;
      function GetIssuer: ICefX509CertPrincipal;
      function GetSerialNumber: ICefBinaryValue;
      function GetValidStart: TCefTime;
      function GetValidExpiry: TCefTime;
      function GetDerEncoded: ICefBinaryValue;
      function GetPemEncoded: ICefBinaryValue;
      function GetIssuerChainSize: NativeUInt;
      function GetDEREncodedIssuerChain(chainCount: NativeUInt): IInterfaceList;
      function GetPEMEncodedIssuerChain(chainCount: NativeUInt): IInterfaceList;

    public
      class function UnWrap(data: Pointer): ICefX509Certificate;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBinaryValue, uCEFX509CertPrincipal;

function TCEFX509CertificateRef.GetSubject: ICefX509CertPrincipal;
begin
  Result := TCefX509CertPrincipalRef.UnWrap(PCefX509Certificate(FData).get_subject(FData));
end;

function TCEFX509CertificateRef.GetIssuer: ICefX509CertPrincipal;
begin
  Result := TCefX509CertPrincipalRef.UnWrap(PCefX509Certificate(FData).get_issuer(FData));
end;

function TCEFX509CertificateRef.GetSerialNumber: ICefBinaryValue;
begin
  Result := TCefBinaryValueRef.UnWrap(PCefX509Certificate(FData).get_serial_number(FData));
end;

function TCEFX509CertificateRef.GetValidStart: TCefTime;
begin
  Result := PCefX509Certificate(FData).get_valid_start(FData);
end;

function TCEFX509CertificateRef.GetValidExpiry: TCefTime;
begin
  Result := PCefX509Certificate(FData).get_valid_expiry(FData);
end;

function TCEFX509CertificateRef.GetDerEncoded: ICefBinaryValue;
begin
  Result := TCefBinaryValueRef.UnWrap(PCefX509Certificate(FData).get_derencoded(FData));
end;

function TCEFX509CertificateRef.GetPemEncoded: ICefBinaryValue;
begin
  Result := TCefBinaryValueRef.UnWrap(PCefX509Certificate(FData).get_pemencoded(FData));
end;

function TCEFX509CertificateRef.GetIssuerChainSize: NativeUInt;
begin
  Result := PCefX509Certificate(FData).get_issuer_chain_size(FData);
end;

function TCEFX509CertificateRef.GetDEREncodedIssuerChain(chainCount: NativeUInt): IInterfaceList;
var
  arr: PPCefBinaryValue;
  i: Integer;
begin
  Result := TInterfaceList.Create;
  GetMem(arr, chainCount * SizeOf(Pointer));
  try
    PCefX509Certificate(FData).get_derencoded_issuer_chain(FData, chainCount, arr);
    for i := 0 to chainCount - 1 do
       Result.Add(TCefBinaryValueRef.UnWrap(PPointerArray(arr)[i]));
  finally
    FreeMem(arr);
  end;
end;

function TCEFX509CertificateRef.GetPEMEncodedIssuerChain(chainCount: NativeUInt): IInterfaceList;
var
  arr: PPCefBinaryValue;
  i: Integer;
begin
  Result := TInterfaceList.Create;
  GetMem(arr, chainCount * SizeOf(Pointer));
  try
    PCefX509Certificate(FData).get_pemencoded_issuer_chain(FData, chainCount, arr);
    for i := 0 to chainCount - 1 do
       Result.Add(TCefBinaryValueRef.UnWrap(PPointerArray(arr)[i]));
  finally
    FreeMem(arr);
  end;
end;

class function TCEFX509CertificateRef.UnWrap(data: Pointer): ICefX509Certificate;
begin
  if (data <> nil) then
    Result := Create(data) as ICefX509Certificate
   else
    Result := nil;
end;

end.
