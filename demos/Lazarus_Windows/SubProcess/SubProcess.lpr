program SubProcess;

{$MODE Delphi}

{$I ..\..\..\source\cef.inc}

uses
  LCLIntf, LCLType, LMessages, Forms, Interfaces,
  uCEFApplicationCore;

// CEF needs to set the LARGEADDRESSAWARE ($20) flag which allows 32-bit processes to use up to 3GB of RAM.
{$IFDEF WIN32}{$SetPEFlags $20}{$ENDIF}

{$R *.res}

begin
  GlobalCEFApp                  := TCefApplicationCore.Create;  
  GlobalCEFApp.SetCurrentDir    := True;

  // The main process and the subprocess *MUST* have the same GlobalCEFApp
  // properties and events, specially FrameworkDirPath, ResourcesDirPath,
  // LocalesDirPath, cache and UserDataPath paths.

  // The demos are compiled into the BIN directory. Make sure SubProcess.exe
  // and SimpleBrowser.exe are in that directory or this demo won't work.

  // In case you want to use custom directories for the CEF3 binaries, cache
  // and user data.
{
  GlobalCEFApp.FrameworkDirPath     := 'cef';
  GlobalCEFApp.ResourcesDirPath     := 'cef';
  GlobalCEFApp.LocalesDirPath       := 'cef\locales';
  GlobalCEFApp.cache                := 'cef\cache';
  GlobalCEFApp.UserDataPath         := 'cef\User Data';
}

  GlobalCEFApp.StartSubProcess;
  GlobalCEFApp.Free;
  GlobalCEFApp := nil;
end.

