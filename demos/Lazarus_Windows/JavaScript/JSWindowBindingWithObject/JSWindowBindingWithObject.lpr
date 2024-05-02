program JSWindowBindingWithObject;

{$MODE Delphi}

{$I ..\..\..\..\source\cef.inc}

uses
  {$IFDEF DELPHI16_UP}
  Vcl.Forms,
  WinApi.Windows,
  {$ELSE}
  Forms,
  LCLIntf, LCLType, LMessages, Interfaces,
  {$ENDIF }
  uCEFApplication,
  uJSWindowBindingWithObject in 'uJSWindowBindingWithObject.pas' {JSWindowBindingWithObjectFrm},
  uMyV8Accessor in 'uMyV8Accessor.pas';

{.$R *.res}

// CEF3 needs to set the LARGEADDRESSAWARE flag which allows 32-bit processes to use up to 3GB of RAM.
{$SetPEFlags $20}

{$R *.res}

begin
  // GlobalCEFApp creation and initialization moved to a different unit to fix the memory leak described in the bug #89
  // https://github.com/salvadordf/CEF4Delphi/issues/89
  CreateGlobalCEFApp;

  if GlobalCEFApp.StartMainProcess then
    begin
      Application.Initialize;
      {$IFDEF DELPHI11_UP}
      Application.MainFormOnTaskbar := True;
      {$ENDIF}
      Application.CreateForm(TJSWindowBindingWithObjectFrm, JSWindowBindingWithObjectFrm);
      Application.Run;
    end;

  DestroyGlobalCEFApp;
end.
