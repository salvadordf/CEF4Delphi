program SimpleFMXBrowser;

uses
  System.StartUpCopy,
  FMX.Forms,
  {$IFDEF MSWINDOWS}
  WinApi.Windows,
  {$ENDIF }
  uCEFApplication,
  uSimpleFMXBrowser in 'uSimpleFMXBrowser.pas' {SimpleFMXBrowserFrm};

{$R *.res}

{$IFDEF MSWINDOWS}
// CEF3 needs to set the LARGEADDRESSAWARE flag which allows 32-bit processes to use up to 3GB of RAM.
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
      Application.CreateForm(TSimpleFMXBrowserFrm, SimpleFMXBrowserFrm);
      Application.Run;

      SimpleFMXBrowserFrm.Free;
    end;

  DestroyGlobalCEFApp;
end.
