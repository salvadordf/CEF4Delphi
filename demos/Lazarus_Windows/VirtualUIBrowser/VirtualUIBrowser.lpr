program VirtualUIBrowser;

{$MODE OBJFPC}{$H+}

uses
  Forms,
  LCLIntf, LCLType, LMessages, Interfaces,
  uCEFApplication,
  uVirtualUIBrowser in 'uVirtualUIBrowser.pas' {Form1};

// CEF needs to set the LARGEADDRESSAWARE ($20) flag which allows 32-bit processes to use up to 3GB of RAM.
{$IFDEF WIN32}{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}{$ENDIF}

{$R *.res}

begin
  CreateGlobalCEFApp;

  if GlobalCEFApp.StartMainProcess then
    begin
      Application.Initialize;
      Application.CreateForm(TForm1, Form1);
      Application.Run;
    end;

  DestroyGlobalCEFApp;
end.
