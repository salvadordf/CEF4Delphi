unit uCEFPreferenceRegistrar;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  uCEFBaseScopedWrapper, uCEFTypes, uCEFInterfaces;

type
  TCefPreferenceRegistrarRef = class(TCEFBaseScopedWrapperRef)
    public
      function AddPreference(const name: ustring; const default_value: ICefValue): Boolean;
  end;

implementation

uses
  uCEFMiscFunctions;

function TCefPreferenceRegistrarRef.AddPreference(const name          : ustring;
                                                  const default_value : ICefValue): Boolean;
var
  TempName : TCefString;
begin
  TempName := CefString(name);
  Result   := PCefPreferenceRegistrar(FData)^.add_preference(PCefPreferenceRegistrar(FData),
                                                             @TempName,
                                                             CefGetData(default_value)) <> 0;
end;

end.
