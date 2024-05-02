program JSSimpleWindowBinding;

{$MODE Delphi}

{$I ..\..\..\..\source\cef.inc}

uses
  Forms,
  LCLIntf, LCLType, LMessages, Interfaces,
  uCEFApplication,
  uJSSimpleWindowBinding in 'uJSSimpleWindowBinding.pas' {JSSimpleWindowBindingFrm};

{.$R *.res}

// CEF3 needs to set the LARGEADDRESSAWARE flag which allows 32-bit processes to use up to 3GB of RAM.
{$SetPEFlags $20}

{$R *.res}

begin
  GlobalCEFApp := TCefApplication.Create; 
  GlobalCEFApp.SetCurrentDir       := True;

  // This is the same demo than the JSSimpleWindowBinding but using a different executable for the subprocesses.
  // Notice that GlobalCEFApp.OnContextCreated is now defined in the SubProcess.

  // Follow these steps to test this demo :
  // 1. Build the SubProcess project in this directory.
  // 2. Copy the CEF binaries to the BIN directory in CEF4Delphi.
  // 3. Build this project : JSSimpleWindowBinding
  // 4. Run this demo : JSSimpleWindowBinding

  GlobalCEFApp.BrowserSubprocessPath := 'SubProcess.exe';

  if GlobalCEFApp.StartMainProcess then
    begin
      Application.Initialize;
      {$IFDEF DELPHI11_UP}
      Application.MainFormOnTaskbar := True;
      {$ENDIF}
      Application.CreateForm(TJSSimpleWindowBindingFrm, JSSimpleWindowBindingFrm);
      Application.Run;
    end;

  GlobalCEFApp.Free;
  GlobalCEFApp := nil;
end.
