unit uMyV8Accessor;

{$MODE Delphi}

{$I ..\..\..\..\source\cef.inc}

interface

uses
  uCEFv8Value, uCEFv8Accessor, uCEFInterfaces, uCEFTypes;

type
  TMyV8Accessor = class(TCefV8AccessorOwn)
    protected
      FMyVal : ustring;

      function Get(const name: ustring; const object_: ICefv8Value; var retval : ICefv8Value; var exception: ustring): Boolean; override;
      function Set_(const name: ustring; const object_, value: ICefv8Value; var exception: ustring): Boolean; override;
  end;

implementation

function TMyV8Accessor.Get(const name      : ustring;
                           const object_   : ICefv8Value;
                           var   retval    : ICefv8Value;
                           var   exception : ustring): Boolean;
begin
  if (name = 'myval') then
    begin
      retval := TCefv8ValueRef.NewString(FMyVal);
      Result := True;
    end
   else
    Result := False;
end;

function TMyV8Accessor.Set_(const name      : ustring;
                            const object_   : ICefv8Value;
                            const value     : ICefv8Value;
                            var   exception : ustring): Boolean;
begin
  if (name = 'myval') then
    begin
      if value.IsString then
        FMyVal := value.GetStringValue
       else
        exception := 'Invalid value type';

      Result := True;
    end
   else
    Result := False;
end;

end.
