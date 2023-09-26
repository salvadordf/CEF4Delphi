program FMXExternalPumpBrowser2;

uses
  // FMUX.Config.pas belongs to the FMXLinux project but we need to override it.
  // Read the comments in that unit for more details.
  FMUX.Config in 'FMUX.Config.pas',
  // FMX initializes GTK in the initialization section of some of its units and
  // that means that GTK is already initialized when the code in the DPR is
  // executed.
  // Chromium has to be initialized in a process with only one thread but GTK
  // creates several threads during its initialization. To avoid this problem
  // we have to initialize CEF before GTK.
  // uCEFLoader *MUST* be the first unit in the DPR file to make sure Chromium
  // is initialized before GTK.
  // uCEFLoader *MUST NOT* make any reference to any FMX unit to keep the right
  // initalization order.
  // Read the answer to this question for more more information :
  // https://stackoverflow.com/questions/52103407/changing-the-initialization-order-of-the-unit-in-delphi
  uCEFLoader in 'uCEFLoader.pas',
  System.StartUpCopy,
  FMX.Forms,
  uFMXExternalPumpBrowser2 in 'uFMXExternalPumpBrowser2.pas' {FMXExternalPumpBrowserFrm};

{$R *.res}

begin
  // At this point it's safe to initialize GTK
  InitializeGTK;
  Application.Initialize;
  Application.CreateForm(TFMXExternalPumpBrowserFrm, FMXExternalPumpBrowserFrm);
  Application.Run;

  // The form needs to be destroyed *BEFORE* stopping the work scheduler.
  FMXExternalPumpBrowserFrm.Free;
end.
