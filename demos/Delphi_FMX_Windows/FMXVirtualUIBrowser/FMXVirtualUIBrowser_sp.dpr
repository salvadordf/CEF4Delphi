program FMXVirtualUIBrowser_sp;

uses
  uFMXVirtualUIBrowser_sp in 'uFMXVirtualUIBrowser_sp.pas';

{$R *.res}

const
  IMAGE_FILE_LARGE_ADDRESS_AWARE = $0020;

// CEF needs to set the LARGEADDRESSAWARE ($20) flag which allows 32-bit processes to use up to 3GB of RAM.
{$IFDEF WIN32}{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}{$ENDIF}

begin
  CreateGlobalCEFApp;
end.
