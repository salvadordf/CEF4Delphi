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
    class procedure mousestate(const data: string);
    class procedure minimize();
    class procedure maximize();
    class procedure close();
  end;

implementation

uses
  uCEFMiscFunctions, uCEFConstants, uCustomTitleBarExtension;

class procedure TTestExtension.mousestate(const data: string);
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

class procedure TTestExtension.minimize();
var
  TempMessage : ICefProcessMessage;
  TempFrame   : ICefFrame;
begin
    try
    TempMessage := TCefProcessMessageRef.New(WINDOW_MINIMIZE_MESSAGE);

    // Sending a message back to the browser. It'll be received in the TChromium.OnProcessMessageReceived event.
    // TCefv8ContextRef.Current returns the v8 context for the frame that is currently executing Javascript.

    TempFrame := TCefv8ContextRef.Current.Browser.MainFrame;

    if (TempFrame <> nil) and TempFrame.IsValid then
      TempFrame.SendProcessMessage(PID_BROWSER, TempMessage);
  finally
    TempMessage := nil;
  end;
end;

class procedure TTestExtension.maximize();
var
  TempMessage : ICefProcessMessage;
  TempFrame   : ICefFrame;
begin
    try
    TempMessage := TCefProcessMessageRef.New(WINDOW_MAXIMIZE_MESSAGE);

    // Sending a message back to the browser. It'll be received in the TChromium.OnProcessMessageReceived event.
    // TCefv8ContextRef.Current returns the v8 context for the frame that is currently executing Javascript.

    TempFrame := TCefv8ContextRef.Current.Browser.MainFrame;

    if (TempFrame <> nil) and TempFrame.IsValid then
      TempFrame.SendProcessMessage(PID_BROWSER, TempMessage);
  finally
    TempMessage := nil;
  end;
end;

class procedure TTestExtension.close();
var
  TempMessage : ICefProcessMessage;
  TempFrame   : ICefFrame;
begin
    try
    TempMessage := TCefProcessMessageRef.New(WINDOW_CLOSE_MESSAGE);

    // Sending a message back to the browser. It'll be received in the TChromium.OnProcessMessageReceived event.
    // TCefv8ContextRef.Current returns the v8 context for the frame that is currently executing Javascript.

    TempFrame := TCefv8ContextRef.Current.Browser.MainFrame;

    if (TempFrame <> nil) and TempFrame.IsValid then
      TempFrame.SendProcessMessage(PID_BROWSER, TempMessage);
  finally
    TempMessage := nil;
  end;
end;


end.
