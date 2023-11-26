unit uTestExtension;

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
  TTestExtension = class
    class procedure mouseover(const data: string);
    class procedure sendresulttobrowser(const msgtext, msgname : string);
  end;

implementation

uses
  uCEFMiscFunctions, uCEFConstants, uJSRTTIExtension;

class procedure TTestExtension.mouseover(const data: string);
var
  TempMessage : ICefProcessMessage;
  TempFrame   : ICefFrame;
begin
  try
    TempMessage := TCefProcessMessageRef.New(MOUSEOVER_MESSAGE_NAME);
    TempMessage.ArgumentList.SetString(0, data);

    // Sending a message back to the browser. It'll be received in the TChromium.OnProcessMessageReceived event.
    // TCefv8ContextRef.Current returns the v8 context for the frame that is currently executing Javascript.

    TempFrame := TCefv8ContextRef.Current.Browser.MainFrame;

    if (TempFrame <> nil) and TempFrame.IsValid then
      TempFrame.SendProcessMessage(PID_BROWSER, TempMessage);
  finally
    TempMessage := nil;
  end;
end;

class procedure TTestExtension.sendresulttobrowser(const msgtext, msgname : string);
var
  TempMessage : ICefProcessMessage;
  TempFrame   : ICefFrame;
begin
  try
    TempMessage := TCefProcessMessageRef.New(msgname);
    TempMessage.ArgumentList.SetString(0, msgtext);

    TempFrame := TCefv8ContextRef.Current.Browser.MainFrame;

    if (TempFrame <> nil) and TempFrame.IsValid then
      TempFrame.SendProcessMessage(PID_BROWSER, TempMessage);
  finally
    TempMessage := nil;
  end;
end;

end.
