unit uTestExtensionHandler;

{$I ..\..\..\..\source\cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  Winapi.Windows,
  {$ELSE}
  Windows,
  {$ENDIF}
  uCEFRenderProcessHandler, uCEFBrowserProcessHandler, uCEFInterfaces, uCEFProcessMessage,
  uCEFv8Context, uCEFTypes, uCEFv8Handler;

type
  TTestExtensionHandler = class(TCefv8HandlerOwn)
    protected
      function Execute(const name: ustring; const object_: ICefv8Value; const arguments: TCefv8ValueArray; var retval: ICefv8Value; var exception: ustring): Boolean; override;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFConstants, uJSExtension;

function TTestExtensionHandler.Execute(const name      : ustring;
                                       const object_   : ICefv8Value;
                                       const arguments : TCefv8ValueArray;
                                       var   retval    : ICefv8Value;
                                       var   exception : ustring): Boolean;
var
  TempMessage : ICefProcessMessage;
  TempFrame   : ICefFrame;
begin
  Result := False;

  try
    if (name = 'mouseover') then
      begin
        if (length(arguments) > 0) and arguments[0].IsString then
          begin
            TempMessage := TCefProcessMessageRef.New(MOUSEOVER_MESSAGE_NAME);
            TempMessage.ArgumentList.SetString(0, arguments[0].GetStringValue);

            TempFrame := TCefv8ContextRef.Current.Browser.MainFrame;

            if (TempFrame <> nil) and TempFrame.IsValid then
              TempFrame.SendProcessMessage(PID_BROWSER, TempMessage);
          end;

        Result := True;
      end
     else
      if (name = 'sendresulttobrowser') then
        begin
          if (length(arguments) > 1) and arguments[0].IsString and arguments[1].IsString then
            begin
              TempMessage := TCefProcessMessageRef.New(arguments[1].GetStringValue);
              TempMessage.ArgumentList.SetString(0, arguments[0].GetStringValue);

              TempFrame := TCefv8ContextRef.Current.Browser.MainFrame;

              if (TempFrame <> nil) and TempFrame.IsValid then
                TempFrame.SendProcessMessage(PID_BROWSER, TempMessage);
            end;

          Result := True;
        end;
  finally
    TempMessage := nil;
  end;
end;

end.
