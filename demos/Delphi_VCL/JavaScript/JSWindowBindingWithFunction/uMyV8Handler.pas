unit uMyV8Handler;

interface

uses
  uCEFTypes, uCEFInterfaces, uCEFv8Value, uCEFv8Handler;

type
  TMyV8Handler = class(TCefv8HandlerOwn)
    protected
      function Execute(const name: ustring; const obj: ICefv8Value; const arguments: TCefv8ValueArray; var retval: ICefv8Value; var exception: ustring): Boolean; override;
  end;

implementation

function TMyV8Handler.Execute(const name      : ustring;
                              const obj       : ICefv8Value;
                              const arguments : TCefv8ValueArray;
                              var   retval    : ICefv8Value;
                              var   exception : ustring): Boolean;
begin
  if (name = 'myfunc') then
    begin
      retval := TCefv8ValueRef.NewString('My Value!');
      Result := True;
    end
   else
    Result := False;
end;


end.
