unit uMyV8Handler;

{$MODE Delphi}

{$I ..\..\..\..\source\cef.inc}

interface

uses
  uCEFTypes, uCEFInterfaces, uCEFv8Value, uCEFv8Handler;

type
  TMyV8Handler = class(TCefv8HandlerOwn)
    protected
      FMyParam : string;

      function Execute(const name: ustring; const obj: ICefv8Value; const arguments: TCefv8ValueArray; var retval: ICefv8Value; var exception: ustring): Boolean; override;
  end;

implementation

function TMyV8Handler.Execute(const name      : ustring;
                              const obj       : ICefv8Value;
                              const arguments : TCefv8ValueArray;
                              var   retval    : ICefv8Value;
                              var   exception : ustring): Boolean;
begin
  if (name = 'GetMyParam') then
    begin
      retval := TCefv8ValueRef.NewString(FMyParam);
      Result := True;
    end
   else
    if (name = 'SetMyParam') then
      begin
        if (length(arguments) > 0) and arguments[0].IsString then
          FMyParam := arguments[0].GetStringValue;

        Result := True;
      end
     else
      Result := False;
end;


end.
