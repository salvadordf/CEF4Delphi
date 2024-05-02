program JSExtension_sp;

{$MODE Delphi}

uses
  LCLIntf, LCLType, LMessages, Forms, Interfaces,
  uCEFApplicationCore, uJSExtension_sp, uTestExtensionHandler;

{$IFDEF WIN32}
// CEF needs to set the LARGEADDRESSAWARE ($20) flag which allows 32-bit processes to use up to 3GB of RAM.
const
  IMAGE_FILE_LARGE_ADDRESS_AWARE = $0020;
  {$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}
{$ENDIF}

{$R *.res}

begin
  CreateGlobalCEFApp;
  DestroyGlobalCEFApp;
end.

