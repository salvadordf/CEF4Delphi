unit uMyV8Handler;

{$MODE Delphi}

{$I ..\..\..\..\source\cef.inc}

interface

uses
  uCEFv8Context, uCEFTypes, uCEFInterfaces, uCEFv8Value, uCEFv8Handler;

type
  TMyV8Handler = class(TCefv8HandlerOwn)
    protected
      function Execute(const name: ustring; const obj: ICefv8Value; const arguments: TCefv8ValueArray; var retval: ICefv8Value; var exception: ustring): Boolean; override;
  end;

implementation

uses
  uJSExecutingFunctions;

function TMyV8Handler.Execute(const name      : ustring;
                              const obj       : ICefv8Value;
                              const arguments : TCefv8ValueArray;
                              var   retval    : ICefv8Value;
                              var   exception : ustring): Boolean;
begin
  Result := False;

  if (name = 'register') and
     (length(arguments) = 1) and
     arguments[0].IsFunction then
    begin
      GlobalCallbackFunc    := arguments[0];
      GlobalCallbackContext := TCefv8ContextRef.Current;
      Result                := True;
    end;
end;


end.
