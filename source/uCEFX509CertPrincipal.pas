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

unit uCEFX509CertPrincipal;

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
  TCefX509CertPrincipalRef = class(TCefBaseRefCountedRef, ICefX509CertPrincipal)
    protected
      function  GetDisplayName: ustring;
      function  GetCommonName: ustring;
      function  GetLocalityName: ustring;
      function  GetStateOrProvinceName: ustring;
      function  GetCountryName: ustring;
      procedure GetStreetAddresses(addresses: TStrings);
      procedure GetOrganizationNames(names: TStrings);
      procedure GetOrganizationUnitNames(names: TStrings);
      procedure GetDomainComponents(components: TStrings);

    public
      class function UnWrap(data: Pointer): ICefX509CertPrincipal;
  end;

implementation

uses
  {$IFDEF DELPHI16_UP}
  WinApi.Windows, System.SysUtils,
  {$ELSE}
  Windows, SysUtils,
  {$ENDIF}
  uCEFMiscFunctions, uCEFLibFunctions, uCEFApplication;

function TCefX509CertPrincipalRef.GetDisplayName: ustring;
begin
  Result := CefStringFreeAndGet(PCefX509CertPrincipal(FData).get_display_name(FData));
end;

function TCefX509CertPrincipalRef.GetCommonName: ustring;
begin
  Result := CefStringFreeAndGet(PCefX509CertPrincipal(FData).get_common_name(FData));
end;

function TCefX509CertPrincipalRef.GetLocalityName: ustring;
begin
  Result := CefStringFreeAndGet(PCefX509CertPrincipal(FData).get_locality_name(FData));
end;

function TCefX509CertPrincipalRef.GetStateOrProvinceName: ustring;
begin
  Result := CefStringFreeAndGet(PCefX509CertPrincipal(FData).get_state_or_province_name(FData));
end;

function TCefX509CertPrincipalRef.GetCountryName: ustring;
begin
  Result := CefStringFreeAndGet(PCefX509CertPrincipal(FData).get_country_name(FData));
end;

procedure TCefX509CertPrincipalRef.GetStreetAddresses(addresses: TStrings);
var
  TempList : TCefStringList;
begin
  TempList := nil;

  try
    try
      if (addresses <> nil) then
        begin
          TempList := cef_string_list_alloc;
          PCefX509CertPrincipal(FData).get_street_addresses(FData, TempList);
          CefStringListToStringList(TempList, addresses);
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TCefX509CertPrincipalRef.GetStreetAddresses', e) then raise;
    end;
  finally
    if (TempList <> nil) then cef_string_list_free(TempList);
  end;
end;

procedure TCefX509CertPrincipalRef.GetOrganizationNames(names: TStrings);
var
  TempList : TCefStringList;
begin
  TempList := nil;

  try
    try
      if (names <> nil) then
        begin
          TempList := cef_string_list_alloc;
          PCefX509CertPrincipal(FData).get_organization_names(FData, TempList);
          CefStringListToStringList(TempList, names);
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TCefX509CertPrincipalRef.GetOrganizationNames', e) then raise;
    end;
  finally
    if (TempList <> nil) then cef_string_list_free(TempList);
  end;
end;

procedure TCefX509CertPrincipalRef.GetOrganizationUnitNames(names: TStrings);
var
  TempList : TCefStringList;
begin
  TempList := nil;

  try
    try
      if (names <> nil) then
        begin
          TempList := cef_string_list_alloc;
          PCefX509CertPrincipal(FData).get_organization_unit_names(FData, TempList);
          CefStringListToStringList(TempList, names);
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TCefX509CertPrincipalRef.GetOrganizationUnitNames', e) then raise;
    end;
  finally
    if (TempList <> nil) then cef_string_list_free(TempList);
  end;
end;

procedure TCefX509CertPrincipalRef.GetDomainComponents(components: TStrings);
var
  TempList : TCefStringList;
begin
  TempList := nil;

  try
    try
      if (components <> nil) then
        begin
          TempList := cef_string_list_alloc;
          PCefX509CertPrincipal(FData).get_domain_components(FData, TempList);
          CefStringListToStringList(TempList, components);
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TCefX509CertPrincipalRef.GetDomainComponents', e) then raise;
    end;
  finally
    if (TempList <> nil) then cef_string_list_free(TempList);
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
