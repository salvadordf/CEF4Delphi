program JSExtension_sp;

uses
  uCEFApplicationCore,
  uTestExtensionHandler in 'uTestExtensionHandler.pas',
  uJSExtension_sp in 'uJSExtension_sp.pas';

const
  IMAGE_FILE_LARGE_ADDRESS_AWARE = $0020;

// CEF needs to set the LARGEADDRESSAWARE ($20) flag which allows 32-bit processes to use up to 3GB of RAM.
{$IFDEF WIN32}{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}{$ENDIF}

begin
  CreateGlobalCEFApp;
  DestroyGlobalCEFApp;
end.

