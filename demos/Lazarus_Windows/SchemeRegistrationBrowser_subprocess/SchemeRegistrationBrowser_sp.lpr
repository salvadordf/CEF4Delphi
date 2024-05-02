program SchemeRegistrationBrowser_sp;

{$MODE Delphi}

{$I ..\..\..\source\cef.inc}

uses
  LCLIntf, LCLType, LMessages, Forms, Interfaces,
  uCEFApplicationCore, uCEFConstants, uCEFSchemeRegistrar;

// CEF3 needs to set the LARGEADDRESSAWARE flag which allows 32-bit processes
// to use up to 3GB of RAM.
{$SetPEFlags $20}

// It's necessary to register the custom scheme in all CEF subprocesses
procedure GlobalCEFApp_OnRegCustomSchemes(const registrar: TCefSchemeRegistrarRef);
begin
  registrar.AddCustomScheme('hello', CEF_SCHEME_OPTION_STANDARD or CEF_SCHEME_OPTION_LOCAL);
end;   

{$R *.res}

begin
  GlobalCEFApp                     := TCefApplicationCore.Create;   
  GlobalCEFApp.OnRegCustomSchemes  := GlobalCEFApp_OnRegCustomSchemes;       
  GlobalCEFApp.LogFile             := 'debug.log';
  GlobalCEFApp.LogSeverity         := LOGSEVERITY_INFO;  
  GlobalCEFApp.SetCurrentDir       := True;
  GlobalCEFApp.StartSubProcess;
  GlobalCEFApp.Free;
  GlobalCEFApp := nil;
end.

