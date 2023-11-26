program VirtualUIBrowser_sp;

{$I ..\..\..\source\cef.inc}

uses
  uCEFLoader_sp in 'uCEFLoader_sp.pas',
  uVirtualUIBrowserConstants in 'uVirtualUIBrowserConstants.pas';

const
  IMAGE_FILE_LARGE_ADDRESS_AWARE = $0020;

// CEF needs to set the LARGEADDRESSAWARE ($20) flag which allows 32-bit processes to use up to 3GB of RAM.
{$IFDEF WIN32}{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}{$ENDIF}

begin
  // This SubProcess project is only used for the CEF subprocesses.
end.
