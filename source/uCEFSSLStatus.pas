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

unit uCEFSSLStatus;

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
  TCefSSLStatusRef = class(TCefBaseRefCountedRef, ICefSSLStatus)
    protected
      function IsSecureConnection: boolean;
      function GetCertStatus: TCefCertStatus;
      function GetSSLVersion: TCefSSLVersion;
      function GetContentStatus: TCefSSLContentStatus;
      function GetX509Certificate: ICefX509Certificate;

    public
      class function UnWrap(data: Pointer): ICefSSLStatus;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBinaryValue, uCEFX509Certificate;

function TCefSSLStatusRef.IsSecureConnection: boolean;
begin
  Result := (PCefSSLStatus(FData).is_secure_connection(FData) <> 0);
end;

function TCefSSLStatusRef.GetCertStatus: TCefCertStatus;
begin
  Result := PCefSSLStatus(FData).get_cert_status(FData);
end;

function TCefSSLStatusRef.GetSSLVersion: TCefSSLVersion;
begin
  Result := PCefSSLStatus(FData).get_sslversion(FData);
end;

function TCefSSLStatusRef.GetContentStatus: TCefSSLContentStatus;
begin
  Result := PCefSSLStatus(FData).get_content_status(FData);
end;

function TCefSSLStatusRef.GetX509Certificate: ICefX509Certificate;
begin
  Result := TCEFX509CertificateRef.UnWrap(PCefSSLStatus(FData).get_x509certificate(FData));
end;

class function TCefSSLStatusRef.UnWrap(data: Pointer): ICefSSLStatus;
begin
  if (data <> nil) then
    Result := Create(data) as ICefSSLStatus
   else
    Result := nil;
end;

end.
