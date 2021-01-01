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

unit uCEFX509Certificate;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
    {$IFDEF MSWINDOWS}WinApi.Windows,{$ENDIF} System.Classes, System.SysUtils,
  {$ELSE}
    {$IFDEF MSWINDOWS}Windows,{$ENDIF} Classes, SysUtils,
  {$ENDIF}
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCEFX509CertificateRef = class(TCefBaseRefCountedRef, ICefX509Certificate)
    protected
      function  GetSubject: ICefX509CertPrincipal;
      function  GetIssuer: ICefX509CertPrincipal;
      function  GetSerialNumber: ICefBinaryValue;
      function  GetValidStart: TCefTime;
      function  GetValidExpiry: TCefTime;
      function  GetDerEncoded: ICefBinaryValue;
      function  GetPemEncoded: ICefBinaryValue;
      function  GetIssuerChainSize: NativeUInt;
      procedure GetDEREncodedIssuerChain(chainCount: NativeUInt; var chain : TCefBinaryValueArray);
      procedure GetPEMEncodedIssuerChain(chainCount: NativeUInt; var chain : TCefBinaryValueArray);

    public
      class function UnWrap(data: Pointer): ICefX509Certificate;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBinaryValue, uCEFX509CertPrincipal;

function TCEFX509CertificateRef.GetSubject: ICefX509CertPrincipal;
begin
  Result := TCefX509CertPrincipalRef.UnWrap(PCefX509Certificate(FData)^.get_subject(PCefX509Certificate(FData)));
end;

function TCEFX509CertificateRef.GetIssuer: ICefX509CertPrincipal;
begin
  Result := TCefX509CertPrincipalRef.UnWrap(PCefX509Certificate(FData)^.get_issuer(PCefX509Certificate(FData)));
end;

function TCEFX509CertificateRef.GetSerialNumber: ICefBinaryValue;
begin
  Result := TCefBinaryValueRef.UnWrap(PCefX509Certificate(FData)^.get_serial_number(PCefX509Certificate(FData)));
end;

function TCEFX509CertificateRef.GetValidStart: TCefTime;
begin
  Result := PCefX509Certificate(FData)^.get_valid_start(PCefX509Certificate(FData));
end;

function TCEFX509CertificateRef.GetValidExpiry: TCefTime;
begin
  Result := PCefX509Certificate(FData)^.get_valid_expiry(PCefX509Certificate(FData));
end;

function TCEFX509CertificateRef.GetDerEncoded: ICefBinaryValue;
begin
  Result := TCefBinaryValueRef.UnWrap(PCefX509Certificate(FData)^.get_derencoded(PCefX509Certificate(FData)));
end;

function TCEFX509CertificateRef.GetPemEncoded: ICefBinaryValue;
begin
  Result := TCefBinaryValueRef.UnWrap(PCefX509Certificate(FData)^.get_pemencoded(PCefX509Certificate(FData)));
end;

function TCEFX509CertificateRef.GetIssuerChainSize: NativeUInt;
begin
  Result := PCefX509Certificate(FData)^.get_issuer_chain_size(PCefX509Certificate(FData));
end;

procedure TCEFX509CertificateRef.GetDEREncodedIssuerChain(chainCount: NativeUInt; var chain : TCefBinaryValueArray);
var
  TempArray : array of PCefBinaryValue;
  i : NativeUInt;
begin
  TempArray := nil;

  try
    try
      if (chainCount > 0) then
        begin
          SetLength(TempArray, chainCount);

          i := 0;
          while (i < chainCount) do
            begin
              TempArray[i] := nil;
              inc(i);
            end;

          PCefX509Certificate(FData)^.get_derencoded_issuer_chain(PCefX509Certificate(FData), chainCount, TempArray[0]);

          if (chainCount > 0) then
            begin
              SetLength(chain, chainCount);

              i := 0;
              while (i < chainCount) do
                begin
                  chain[i] := TCefBinaryValueRef.UnWrap(TempArray[i]);
                  inc(i);
                end;
            end;
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TCEFX509CertificateRef.GetDEREncodedIssuerChain', e) then raise;
    end;
  finally
    if (TempArray <> nil) then
      begin
        Finalize(TempArray);
        TempArray := nil;
      end;
  end;
end;

procedure TCEFX509CertificateRef.GetPEMEncodedIssuerChain(chainCount: NativeUInt; var chain : TCefBinaryValueArray);
var
  TempArray : array of PCefBinaryValue;
  i : NativeUInt;
begin
  TempArray := nil;

  try
    try
      if (chainCount > 0) then
        begin
          SetLength(TempArray, chainCount);

          i := 0;
          while (i < chainCount) do
            begin
              TempArray[i] := nil;
              inc(i);
            end;

          PCefX509Certificate(FData)^.get_pemencoded_issuer_chain(PCefX509Certificate(FData), chainCount, TempArray[0]);

          if (chainCount > 0) then
            begin
              SetLength(chain, chainCount);

              i := 0;
              while (i < chainCount) do
                begin
                  chain[i] := TCefBinaryValueRef.UnWrap(TempArray[i]);
                  inc(i);
                end;
            end;
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TCEFX509CertificateRef.GetPEMEncodedIssuerChain', e) then raise;
    end;
  finally
    if (TempArray <> nil) then
      begin
        Finalize(TempArray);
        TempArray := nil;
      end;
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
