program SimpleBrowser;

{$I ..\..\..\source\cef.inc}

uses
  Forms, Interfaces,
  Windows,
  uCEFApplication,
  uSimpleBrowser in 'uSimpleBrowser.pas' {Form1},
  uCEFLoader in 'uCEFLoader.pas';

// CEF needs to set the LARGEADDRESSAWARE ($20) flag which allows 32-bit processes to use up to 3GB of RAM.
{$IFDEF WIN32}{$SetPEFlags $20}{$ENDIF}

{$R *.res}

begin
  // This demo has the GlobalCEFApp creation, initialization and destruction in uCEFLoader.pas
  // Read the code comments in uCEFLoader.pas for more details.

  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
