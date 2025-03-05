unit uTinyBrowser2;

{$MODE Delphi}          

interface

uses
  Types, SysUtils,
  uCEFInterfaces, uCEFTypes, uCEFChromiumCore;

type
  TTinyBrowser2 = class
    private
      FChromium : TChromiumCore;    

      function GetClient : ICefClient;

      procedure Chromium_OnBeforeClose(Sender: TObject; const browser: ICefBrowser);

    public
      constructor Create;
      destructor  Destroy; override;
      procedure   AfterConstruction; override;   

      property Client      : ICefClient read GetClient;
  end;

procedure CreateGlobalCEFApp;
procedure DestroyTinyBrowser;

implementation

// This demo is similar to the cefsimple demo in the official CEF project.

// It doesn't use LCL to create forms from Lazarus code.
// It just uses CEF to create a browser window as if it was a popup browser window.

// This browser doesn't use multiple threads to handle the browser and it doesn't use an external message pump.
// For this reason we have to call GlobalCEFApp.RunMessageLoop to let CEF handle the message loop and
// GlobalCEFApp.QuitMessageLoop when the browser has been destroyed.

// The destruction steps are much simpler for that reason.
// In this demo it's only necessary to implement the TChromium.OnClose and TChromium.OnBeforeClose events.
// The TChromium.OnClose event only sets aAction to cbaClose to continue closing the browser.
// The TChromium.OnBeforeClose event calls GlobalCEFApp.QuitMessageLoop because the browser has been destroyed
// and it's necessary to close the message loop.

uses
  uCEFApplication, uCEFConstants, uCEFMiscFunctions;

var
  TinyBrowser : TTinyBrowser2 = nil;

procedure GlobalCEFApp_OnContextInitialized;
begin
  TinyBrowser := TTinyBrowser2.Create;
end;                 

procedure GlobalCEFApp_OnGetDefaultClient(var aClient : ICefClient);
begin
  aClient := TinyBrowser.Client;
end;

procedure CreateGlobalCEFApp;
begin
  GlobalCEFApp                            := TCefApplication.Create;
  GlobalCEFApp.MultiThreadedMessageLoop   := False;
  GlobalCEFApp.ExternalMessagePump        := False;                              
  GlobalCEFApp.DisablePopupBlocking       := True;
  GlobalCEFApp.cache                      := 'cache'; 
  GlobalCEFApp.SetCurrentDir              := True;
  GlobalCEFApp.OnContextInitialized       := GlobalCEFApp_OnContextInitialized;
  GlobalCEFApp.OnGetDefaultClient         := GlobalCEFApp_OnGetDefaultClient;
end;

procedure DestroyTinyBrowser;
begin
  if (TinyBrowser <> nil) then
    begin
      TinyBrowser.Free;
      TinyBrowser := nil;
    end;
end;

constructor TTinyBrowser2.Create;
begin
  inherited Create;

  FChromium := nil;
end;

destructor TTinyBrowser2.Destroy;
begin
  if (FChromium <> nil) then
    begin
      FChromium.Free;
      FChromium := nil;
    end;

  inherited Destroy;
end;

procedure TTinyBrowser2.AfterConstruction;     
var
  TempHandle : TCefWindowHandle;
  TempRect : TRect;
begin
  inherited AfterConstruction;

  FChromium                  := TChromiumCore.Create(nil);
  FChromium.DefaultURL       := 'https://www.google.com';
  FChromium.OnBeforeClose    := Chromium_OnBeforeClose;

  // The MultiBrowserMode store all the browser references in TChromium.
  // The first browser reference is the browser in the main form.
  // When MiniBrowser allows CEF to create child popup browsers it will also
  // store their reference inside TChromium and you can use all the TChromium's
  // methods and properties to manipulate those browsers.
  // To do that call TChromium.SelectBrowser with the browser ID that will be
  // used when you call any method or property in TChromium.
  FChromium.MultiBrowserMode := True;

  InitializeWindowHandle(TempHandle);
  FChromium.CreateBrowser(TempHandle, TempRect, 'Tiny Browser 2', nil, nil, True);
end;     

function TTinyBrowser2.GetClient : ICefClient;
begin
  if (FChromium <> nil) then
    Result := FChromium.CefClient
   else
    Result := nil;
end;

procedure TTinyBrowser2.Chromium_OnBeforeClose(Sender: TObject; const browser: ICefBrowser);
begin
  // The main browser is being destroyed
  if (FChromium.BrowserId = 0) then
    GlobalCEFApp.QuitMessageLoop;
end;

end.
