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

unit uCEFX509CertPrincipal;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

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
  TCefX509CertPrincipalRef = class(TCefBaseRefCountedRef, ICefX509CertPrincipal)
    protected
      function  GetDisplayName: ustring;
      function  GetCommonName: ustring;
      function  GetLocalityName: ustring;
      function  GetStateOrProvinceName: ustring;
      function  GetCountryName: ustring;
      procedure GetStreetAddresses(const addresses: TStrings);
      procedure GetOrganizationNames(const names: TStrings);
      procedure GetOrganizationUnitNames(const names: TStrings);
      procedure GetDomainComponents(const components: TStrings);

    public
      class function UnWrap(data: Pointer): ICefX509CertPrincipal;
  end;

implementation

uses
  {$IFDEF DELPHI16_UP}
    {$IFDEF MSWINDOWS}WinApi.Windows,{$ENDIF} System.SysUtils,
  {$ELSE}
    {$IFDEF MSWINDOWS}Windows,{$ENDIF} SysUtils,
  {$ENDIF}
  uCEFMiscFunctions, uCEFStringList;

function TCefX509CertPrincipalRef.GetDisplayName: ustring;
begin
  Result := CefStringFreeAndGet(PCefX509CertPrincipal(FData)^.get_display_name(PCefX509CertPrincipal(FData)));
end;

function TCefX509CertPrincipalRef.GetCommonName: ustring;
begin
  Result := CefStringFreeAndGet(PCefX509CertPrincipal(FData)^.get_common_name(PCefX509CertPrincipal(FData)));
end;

function TCefX509CertPrincipalRef.GetLocalityName: ustring;
begin
  Result := CefStringFreeAndGet(PCefX509CertPrincipal(FData)^.get_locality_name(PCefX509CertPrincipal(FData)));
end;

function TCefX509CertPrincipalRef.GetStateOrProvinceName: ustring;
begin
  Result := CefStringFreeAndGet(PCefX509CertPrincipal(FData)^.get_state_or_province_name(PCefX509CertPrincipal(FData)));
end;

function TCefX509CertPrincipalRef.GetCountryName: ustring;
begin
  Result := CefStringFreeAndGet(PCefX509CertPrincipal(FData)^.get_country_name(PCefX509CertPrincipal(FData)));
end;

procedure TCefX509CertPrincipalRef.GetStreetAddresses(const addresses: TStrings);
var
  TempSL : ICefStringList;
begin
  if (addresses <> nil) then
    begin
      TempSL := TCefStringListOwn.Create;
      PCefX509CertPrincipal(FData)^.get_street_addresses(PCefX509CertPrincipal(FData), TempSL.Handle);
      TempSL.CopyToStrings(addresses);
    end;
end;

procedure TCefX509CertPrincipalRef.GetOrganizationNames(const names: TStrings);
var
  TempSL : ICefStringList;
begin
  if (names <> nil) then
    begin
      TempSL := TCefStringListOwn.Create;
      PCefX509CertPrincipal(FData)^.get_organization_names(PCefX509CertPrincipal(FData), TempSL.Handle);
      TempSL.CopyToStrings(names);
    end;
end;

procedure TCefX509CertPrincipalRef.GetOrganizationUnitNames(const names: TStrings);
var
  TempSL : ICefStringList;
begin
  if (names <> nil) then
    begin
      TempSL := TCefStringListOwn.Create;
      PCefX509CertPrincipal(FData)^.get_organization_unit_names(PCefX509CertPrincipal(FData), TempSL.Handle);
      TempSL.CopyToStrings(names);
    end;
end;

procedure TCefX509CertPrincipalRef.GetDomainComponents(const components: TStrings);
var
  TempSL : ICefStringList;
begin
  if (components <> nil) then
    begin
      TempSL := TCefStringListOwn.Create;
      PCefX509CertPrincipal(FData)^.get_domain_components(PCefX509CertPrincipal(FData), TempSL.Handle);
      TempSL.CopyToStrings(components);
    end;
end;

class function TCefX509CertPrincipalRef.UnWrap(data: Pointer): ICefX509CertPrincipal;
begin
  if (data <> nil) then
    Result := Create(data) as ICefX509CertPrincipal
   else
    Result := nil;
end;

end.
