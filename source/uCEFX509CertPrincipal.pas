unit uCEFX509CertPrincipal;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

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
      procedure GetOrganizationNames(const names: TStrings);
      procedure GetOrganizationUnitNames(const names: TStrings);

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

class function TCefX509CertPrincipalRef.UnWrap(data: Pointer): ICefX509CertPrincipal;
begin
  if (data <> nil) then
    Result := Create(data) as ICefX509CertPrincipal
   else
    Result := nil;
end;

end.
