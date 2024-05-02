program TinyBrowser;

{$MODE Delphi}

uses
  LCLIntf, LCLType, LMessages, Interfaces,
  uTinyBrowser in 'uTinyBrowser.pas',
  uCEFApplicationCore;

{.$R *.res}

{$R *.res}

begin
  CreateGlobalCEFApp;

  if GlobalCEFApp.StartMainProcess then
    begin
      GlobalCEFApp.RunMessageLoop;
      DestroyTinyBrowser;
    end;

  DestroyGlobalCEFApp;
end.
