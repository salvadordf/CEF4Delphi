 unit uWindowsServiceBrowser;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.SvcMgr, Vcl.Dialogs,
  uEncapsulatedBrowser;

type
  TService1 = class(TService)
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure ServiceCreate(Sender: TObject);
  private
    FEncapsulatedBrowser : TEncapsulatedBrowser;
  public
    function GetServiceController: TServiceController; override;
    procedure CreateBrowser;
    procedure DestroyBrowser;
  end;

var
  Service1: TService1;

implementation

{$R *.dfm}

// This demo creates a browser in a Windows Service to take a snapshot of
// google.com and save it as c:\windows\temp\snapshot.bmp

// The browser uses the off-screen rendering mode (OSR) to avoid using any
// Windows control.

// CEF is configured to use a different EXE for the subprocesses and the
// browser is handled in a thread (TCEFBrowserThread)

// This demo uses some OutputDebugString calls for debugging purposses.
// Download DebugView++ from https://github.com/CobaltFusion/DebugViewPP
// Run DebugView++ as administrator and select the
// "Log -> Capture Global Win32" menu option to see the OutputDebugString
// messages.

// To install WindowsServiceBrowser execute the following command as administrator :
//      WindowsServiceBrowser.exe /install

// To uninstall WindowsServiceBrowser execute the following command as administrator :
//      WindowsServiceBrowser.exe /uninstall

// Use the "Windows Services Manager" to start, stop, pause and continue WindowsServiceBrowser
// Execute "services.msc" from the Run command option in the Start menu.

// Read this for more information about Windows Service Applications :
// https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Service_Applications
// https://learn.microsoft.com/en-us/dotnet/framework/windows-services/introduction-to-windows-service-applications

uses
  uCEFApplication;

procedure GlobalCEFApp_OnContextInitialized;
begin
  Service1.CreateBrowser;
end;

procedure CreateGlobalCEFApp;
begin
  if (GlobalCEFApp = nil) then
    begin
      GlobalCEFApp                            := TCefApplication.Create;
      GlobalCEFApp.WindowlessRenderingEnabled := True;
      GlobalCEFApp.ShowMessageDlg             := False;                          // This demo shouldn't show any window.
      GlobalCEFApp.BrowserSubprocessPath      := 'WindowsServiceBrowser_sp.exe'; // This is the other EXE for the CEF subprocesses.
      GlobalCEFApp.BlinkSettings              := 'hideScrollbars';               // This setting removes all scrollbars to capture a cleaner snapshot
      GlobalCEFApp.EnableGPU                  := False;
      GlobalCEFApp.DisableComponentUpdate     := True;
      GlobalCEFApp.SetCurrentDir              := True;    
      GlobalCEFApp.OnContextInitialized       := GlobalCEFApp_OnContextInitialized;
      GlobalCEFApp.StartMainProcess;
    end;
end;

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  Service1.Controller(CtrlCode);
end;

function TService1.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TService1.CreateBrowser;
begin
  if (FEncapsulatedBrowser = nil) then
    begin
      FEncapsulatedBrowser := TEncapsulatedBrowser.Create;
      FEncapsulatedBrowser.CreateBrowser;
    end;
end;

procedure TService1.DestroyBrowser;
begin
  if (FEncapsulatedBrowser <> nil) then
    FreeAndNil(FEncapsulatedBrowser);
end;

procedure TService1.ServiceCreate(Sender: TObject);
begin
  FEncapsulatedBrowser := nil;
end;

procedure TService1.ServiceStart(Sender: TService; var Started: Boolean);
begin
  OutputDebugString('WindowsServiceBrowser starting...');
  CreateGlobalCEFApp;
end;

procedure TService1.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  OutputDebugString('WindowsServiceBrowser stopping...');
  DestroyBrowser;
  DestroyGlobalCEFApp;
end;

end.
