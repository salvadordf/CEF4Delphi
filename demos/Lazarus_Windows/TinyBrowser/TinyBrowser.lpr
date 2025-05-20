program TinyBrowser;

{$MODE Delphi}

uses
  LCLIntf, LCLType, LMessages, Interfaces,
  uTinyBrowser in 'uTinyBrowser.pas',
  uCEFApplicationCore;

{$R *.res}

// CEF needs to set the LARGEADDRESSAWARE ($20) flag which allows 32-bit processes to use up to 3GB of RAM.
{$IFDEF WIN32}{$SetPEFlags $20}{$ENDIF}

begin
  CreateGlobalCEFApp;

  if GlobalCEFApp.StartMainProcess then
    begin
      GlobalCEFApp.RunMessageLoop;
      DestroyTinyBrowser;
    end;

  DestroyGlobalCEFApp;
end.
