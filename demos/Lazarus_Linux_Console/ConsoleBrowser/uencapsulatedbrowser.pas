unit uencapsulatedbrowser;

{$mode objfpc}{$H+}

interface

uses
  SyncObjs, SysUtils,
  uCEFTypes, ucefbrowserthread;

type
  TEncapsulatedBrowser = class
    protected
      FThread       : TCEFBrowserThread;
      FWidth        : integer;
      FHeight       : integer;
      FDelayMs      : integer;
      FScale        : single;
      FSnapshotPath : string;
      FErrorText    : string;

      procedure Thread_OnError(Sender: TObject);
      procedure Thread_OnSnapshotAvailable(Sender: TObject);

    public
      constructor Create;
      destructor  Destroy; override;
      procedure   LoadURL(const aURL : string);

      property Width           : integer   read FWidth          write FWidth;
      property Height          : integer   read FHeight         write FHeight;
      property SnapshotPath    : string    read FSnapshotPath   write FSnapshotPath;
      property ErrorText       : string    read FErrorText;
  end;

procedure CreateGlobalCEFApp;
function  WaitForMainAppEvent : boolean;
procedure WriteResult;

implementation

uses
  uCEFApplication;

var
  MainAppEvent        : TSimpleEvent;
  EncapsulatedBrowser : TEncapsulatedBrowser = nil;

procedure GlobalCEFApp_OnContextInitialized;
var
  TempParam, TempURL : ustring;
begin
  TempURL := '';

  // This demo reads the "/url" parameter to load it as the default URL in the browser.
  // For example : ConsoleBrowser2.exe /url=https://www.briskbard.com
  if (ParamCount > 0) then
    begin
      TempParam := paramstr(1);

      if (Copy(TempParam, 1, 5) = '/url=') then
        begin
          TempURL := trim(Copy(TempParam, 6, length(TempParam)));
          if (length(TempURL) > 0) then WriteLn('Loading ' + TempURL);
        end;
    end;

  if (length(TempURL) = 0) then
    begin
      TempURL := 'https://www.google.com';
      WriteLn('No URL has been specified. Using the default...');
    end;

  EncapsulatedBrowser := TEncapsulatedBrowser.Create;
  EncapsulatedBrowser.LoadURL(TempURL);
end;

function WaitForMainAppEvent : boolean;
begin
  Result := True;

  // Wait for 1 minute max.
  if (MainAppEvent.WaitFor(60000) = wrTimeout) then
    begin
      WriteLn('Timeout expired!');
      Result := False;
    end;
end;

procedure WriteResult;
begin
  if (EncapsulatedBrowser = nil) then
    WriteLn('There was a problem in the browser initialization')
   else
    if (length(EncapsulatedBrowser.ErrorText) > 0) then
      WriteLn(EncapsulatedBrowser.ErrorText)
     else
      WriteLn('Snapshot saved successfully as ' + EncapsulatedBrowser.SnapshotPath);
end;

procedure CreateGlobalCEFApp;
begin
  GlobalCEFApp                            := TCefApplication.Create;
  GlobalCEFApp.WindowlessRenderingEnabled := True;
  GlobalCEFApp.ShowMessageDlg             := False;               // This demo shouldn't show any window, just console messages.
  GlobalCEFApp.BrowserSubprocessPath      := 'consolebrowser_sp'; // This is the other EXE for the CEF subprocesses. It's on the same directory as this app.
  GlobalCEFApp.BlinkSettings              := 'hideScrollbars';    // This setting removes all scrollbars to capture a cleaner snapshot
  GlobalCEFApp.OnContextInitialized       := @GlobalCEFApp_OnContextInitialized;     
  GlobalCEFApp.SetCurrentDir              := True;
  GlobalCEFApp.DisableZygote              := True;
  GlobalCEFApp.StartMainProcess;
end;

constructor TEncapsulatedBrowser.Create;
begin
  inherited Create;

  FThread        := nil;
  FWidth         := 1024;
  FHeight        := 768;
  FSnapshotPath  := 'snapshot.png';
  FErrorText     := '';
end;

destructor TEncapsulatedBrowser.Destroy;
begin
  if (FThread <> nil) then
    begin
      FThread.TerminateBrowserThread;
      FThread.WaitFor;
      FreeAndNil(FThread);
    end;

  inherited Destroy;
end;

procedure TEncapsulatedBrowser.LoadURL(const aURL : string);
begin
  if (FThread = nil) then
    begin
      FThread                     := TCEFBrowserThread.Create(FWidth, FHeight, aURL, FSnapshotPath);
      FThread.OnError             := @Thread_OnError;
      FThread.OnSnapshotAvailable := @Thread_OnSnapshotAvailable;
      FThread.Start;
    end
   else
    FThread.LoadUrl(aURL);
end;

procedure TEncapsulatedBrowser.Thread_OnError(Sender: TObject);
begin
  // This code is executed in the TCEFBrowserThread thread context while the main application thread is waiting for MainAppEvent.

  FErrorText := 'Error';

  if (FThread.ErrorCode <> 0) then
    FErrorText := FErrorText + ' ' + inttostr(FThread.ErrorCode);

  FErrorText := FErrorText + ' : ' + FThread.ErrorText;

  if (length(FThread.FailedUrl) > 0) then
    FErrorText := FErrorText + ' - ' + FThread.FailedUrl;

  if assigned(MainAppEvent) then
    MainAppEvent.SetEvent;
end;

procedure TEncapsulatedBrowser.Thread_OnSnapshotAvailable(Sender: TObject);
begin
  // This code is executed in the TCEFBrowserThread thread context while the main application thread is waiting for MainAppEvent.
  if assigned(MainAppEvent) then
    MainAppEvent.SetEvent;
end;

initialization
  MainAppEvent := TSimpleEvent.Create;

finalization
  MainAppEvent.Free;
  if (EncapsulatedBrowser <> nil) then FreeAndNil(EncapsulatedBrowser);

end.
