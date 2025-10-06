program KioskBrowser;

{$MODE Delphi}

uses
  Forms, Interfaces,
  uCEFApplication,
  uKioskBrowser in 'uKioskBrowser.pas' {Form1},
  uVirtualTouchKeyboard in 'uVirtualTouchKeyboard.pas';

{$R *.res}

{$IFDEF WIN32}
const
  IMAGE_FILE_LARGE_ADDRESS_AWARE = $0020;
  // CEF needs to set the LARGEADDRESSAWARE ($20) flag which allows 32-bit processes to use up to 3GB of RAM.
  {$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}
{$ENDIF}

begin
  CreateGlobalCEFApp;

  // You *MUST* call GlobalCEFApp.StartMainProcess in a if..then clause
  // with the Application initialization inside the begin..end.
  // Read this https://www.briskbard.com/index.php?lang=en&pageid=cef
  if GlobalCEFApp.StartMainProcess then
    begin
      Application.Initialize;
      Application.MainFormOnTaskbar := True;
      Application.CreateForm(TForm1, Form1);
      Application.Run;
    end;

  DestroyGlobalCEFApp;
end.
