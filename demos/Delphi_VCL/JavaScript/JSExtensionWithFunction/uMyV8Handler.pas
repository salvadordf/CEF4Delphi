unit uMyV8Handler;

interface

uses
  uCEFTypes, uCEFInterfaces, uCEFv8Value, uCEFProcessMessage, uCEFv8Handler, uCEFv8Context;

const
  TEST_MESSAGE_NAME = 'test_message';

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
var
  TempMessage : ICefProcessMessage;
  TempFrame   : ICefFrame;
begin
  Result := False;

  try
    if (name = 'myfunc') then
      begin
        TempMessage := TCefProcessMessageRef.New(TEST_MESSAGE_NAME);
        TempMessage.ArgumentList.SetString(0, 'Message received!');

        TempFrame := TCefv8ContextRef.Current.Browser.MainFrame;

        if (TempFrame <> nil) and TempFrame.IsValid then
          TempFrame.SendProcessMessage(PID_BROWSER, TempMessage);

        retval := TCefv8ValueRef.NewString('My Value!');
        Result := True;
      end;
  finally
    TempMessage := nil;
  end;
end;


end.
