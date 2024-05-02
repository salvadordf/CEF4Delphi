program SubProcess;

{$MODE Delphi}

{$I ..\..\..\..\source\cef.inc}

uses
  {$IFDEF DELPHI16_UP}
  WinApi.Windows,
  {$ELSE}
  LCLIntf, LCLType, LMessages, Forms, Interfaces,
  {$ENDIF}
  uCEFApplicationCore,
  uCEFConstants,
  uCEFv8Value,
  uCEFInterfaces;

// CEF3 needs to set the LARGEADDRESSAWARE flag which allows 32-bit processes to use up to 3GB of RAM.
{$SetPEFlags $20}

procedure GlobalCEFApp_OnContextCreated(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context);
var
  TempValue : ICEFv8Value;
begin
  // This is the first JS Window Binding example in the "JavaScript Integration" wiki page at
  // https://bitbucket.org/chromiumembedded/cef/wiki/JavaScriptIntegration.md

  TempValue := TCefv8ValueRef.NewString('My Value!');

  context.Global.SetValueByKey('myval', TempValue, V8_PROPERTY_ATTRIBUTE_NONE);
end;

{$R *.res}

begin
  GlobalCEFApp                  := TCefApplicationCore.Create;
  GlobalCEFApp.OnContextCreated := GlobalCEFApp_OnContextCreated;      
  GlobalCEFApp.SetCurrentDir    := True;

  // The main process and the subprocess *MUST* have the same FrameworkDirPath, ResourcesDirPath,
  // LocalesDirPath, cache and UserDataPath paths

  // The demos are compiled into the BIN directory. Make sure SubProcess.exe and JSSimpleWindowBinding.exe are in that
  // directory or this demo won't work.

  // In case you want to use custom directories for the CEF3 binaries, cache and user data.
{
  GlobalCEFApp.FrameworkDirPath     := 'cef';
  GlobalCEFApp.ResourcesDirPath     := 'cef';
  GlobalCEFApp.LocalesDirPath       := 'cef\locales';
  GlobalCEFApp.cache                := 'cef\cache';
  GlobalCEFApp.UserDataPath         := 'cef\User Data';
}

  GlobalCEFApp.StartSubProcess;
  GlobalCEFApp.Free;
  GlobalCEFApp := nil;
end.

