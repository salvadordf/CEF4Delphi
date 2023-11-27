program VirtualUIBrowser;

{$I ..\..\..\source\cef.inc}

uses
  {$IFDEF DELPHI16_UP}
  Vcl.Forms,
  {$ELSE}
  Forms,
  {$ENDIF }
  uCEFLoader in 'uCEFLoader.pas',
  uVirtualUIBrowser in 'uVirtualUIBrowser.pas' {Form1},
  uVirtualUIBrowserConstants in 'uVirtualUIBrowserConstants.pas';

{$R *.res}

{$IFDEF WIN32}
  // CEF needs to set the LARGEADDRESSAWARE flag which allows 32-bit processes to use up to 3GB of RAM.
  // If you don't add this flag the rederer process will crash when you try to load large images.
  {$SetPEFlags $20}
{$ENDIF}

begin
  Application.Initialize;
  {$IFDEF DELPHI11_UP}
  Application.MainFormOnTaskbar := True;
  {$ENDIF}
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
