unit uencapsulatedbrowser;

{$mode objfpc}{$H+}

interface

uses
  SyncObjs, SysUtils,
  ucefbrowserthread;

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
      FEvent        : TSimpleEvent;

      procedure Thread_OnError(Sender: TObject);
      procedure Thread_OnSnapshotAvailable(Sender: TObject);

    public
      constructor Create;
      destructor  Destroy; override;
      procedure   AfterConstruction;
      procedure   LoadURL(const aURL : string);   
      function    WaitForEvent : boolean;
      procedure   WriteResult;

      property Width           : integer   read FWidth          write FWidth;
      property Height          : integer   read FHeight         write FHeight;
      property SnapshotPath    : string    read FSnapshotPath   write FSnapshotPath;
      property ErrorText       : string    read FErrorText;
  end;

procedure InitializeEncapsulatedBrowser;
procedure FinalizeEncapsulatedBrowser;
procedure CaptureScreenshot(const aURL: string);

implementation

var
  EncapsulatedBrowser : TEncapsulatedBrowser = nil;

procedure InitializeEncapsulatedBrowser;
begin
  TCEFBrowserThread.CreateGlobalCEFApp;
end;

procedure FinalizeEncapsulatedBrowser;
begin
  if (EncapsulatedBrowser <> nil) then
    FreeAndNil(EncapsulatedBrowser);

  TCEFBrowserThread.DestroyGlobalCEFApp;
end;

procedure CaptureScreenshot(const aURL: string);
begin
  EncapsulatedBrowser := TEncapsulatedBrowser.Create;
  EncapsulatedBrowser.LoadURL(aURL);

  if EncapsulatedBrowser.WaitForEvent then
    EncapsulatedBrowser.WriteResult;
end;

constructor TEncapsulatedBrowser.Create;
begin
  inherited Create;

  FEvent         := nil;
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

  if (FEvent <> nil) then
     FreeAndNil(FEvent);

  inherited Destroy;
end;

procedure TEncapsulatedBrowser.AfterConstruction;
begin
  inherited AfterConstruction;

  FEvent := TSimpleEvent.Create;
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
  // This code is executed in the TCEFBrowserThread thread context while the main application thread is waiting for FEvent.

  FErrorText := 'Error';

  if (FThread.ErrorCode <> 0) then
    FErrorText := FErrorText + ' ' + inttostr(FThread.ErrorCode);

  FErrorText := FErrorText + ' : ' + FThread.ErrorText;

  if (length(FThread.FailedUrl) > 0) then
    FErrorText := FErrorText + ' - ' + FThread.FailedUrl;

  if assigned(FEvent) then
    FEvent.SetEvent;
end;

procedure TEncapsulatedBrowser.Thread_OnSnapshotAvailable(Sender: TObject);
begin
  // This code is executed in the TCEFBrowserThread thread context while the main application thread is waiting for FEvent.
  if assigned(FEvent) then
    FEvent.SetEvent;
end;

function TEncapsulatedBrowser.WaitForEvent : boolean;
begin
  Result := True;

  // Wait for 1 minute max.
  if assigned(FEvent) and (FEvent.WaitFor(60000) = wrTimeout) then
    begin
      WriteLn('Timeout expired!');
      Result := False;
    end;
end;

procedure TEncapsulatedBrowser.WriteResult;
begin
  if (length(FErrorText) > 0) then
    WriteLn(FErrorText)
   else
    WriteLn('Snapshot saved successfully as ' + FSnapshotPath);
end;

end.
