unit uCEFSchemeRegistrar;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  uCEFBaseScopedWrapper, uCEFTypes;

type
  TCefSchemeRegistrarRef = class(TCEFBaseScopedWrapperRef)
    public
      function AddCustomScheme(const schemeName: ustring; options : TCefSchemeOptions): Boolean;
  end;

implementation

uses
  uCEFMiscFunctions;

function TCefSchemeRegistrarRef.AddCustomScheme(const schemeName : ustring;
                                                      options    : TCefSchemeOptions): Boolean;
var
  TempName : TCefString;
begin
  TempName := CefString(schemeName);
  Result   := PCefSchemeRegistrar(FData)^.add_custom_scheme(PCefSchemeRegistrar(FData),
                                                            @TempName,
                                                            options) <> 0;
end;

end.
