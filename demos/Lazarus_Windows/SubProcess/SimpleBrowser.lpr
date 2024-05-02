program SimpleBrowser;

{$I ..\..\..\source\cef.inc}

uses
  Forms, Interfaces,
  Windows,
  uCEFApplication,
  uSimpleBrowser in 'uSimpleBrowser.pas' {Form1},
  uCEFLoader in 'uCEFLoader.pas';

//{$R *.res}

// CEF3 needs to set the LARGEADDRESSAWARE flag which allows 32-bit processes to use up to 3GB of RAM.
// If you don't add this flag the rederer process will crash when you try to load large images.
// The IMAGE_FILE_LARGE_ADDRESS_AWARE constant is declared in WinApi.Windows. If you don't want to add
// WinApi.Windows to the 'uses' section in this file just replace the following line by
// {$SetPEFlags $20}
{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}

{$R *.res}

begin
  // This demo has the GlobalCEFApp creation, initialization and destruction in uCEFLoader.pas
  // Read the code comments in uCEFLoader.pas for more details.

  Application.Initialize;
  {$IFDEF DELPHI11_UP}
  Application.MainFormOnTaskbar := True;
  {$ENDIF}
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
