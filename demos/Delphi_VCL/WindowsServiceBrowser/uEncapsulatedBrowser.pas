unit uEncapsulatedBrowser;

{$I ..\..\..\source\cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  WinApi.Windows, System.SyncObjs, System.SysUtils,
  {$ELSE}
  Windows, SyncObjs, SysUtils,
  {$ENDIF}
  uCEFTypes, uCEFBrowserThread;

type
  TEncapsulatedBrowser = class
    protected
      FThread       : TCEFBrowserThread;
      FWidth        : integer;
      FHeight       : integer;
      FDelayMs      : integer;
      FScale        : single;
      FSnapshotPath : ustring;
      FErrorText    : ustring;
      FDefaultURL   : ustring;

      procedure Thread_OnError(Sender: TObject);
      procedure Thread_OnSnapshotAvailable(Sender: TObject);

      procedure DestroyBrowser;

    public
      constructor Create;
      destructor  Destroy; override;
      procedure   CreateBrowser;

      property DefaultURL      : ustring    read FDefaultURL     write FDefaultURL;
      property Width           : integer    read FWidth          write FWidth;
      property Height          : integer    read FHeight         write FHeight;
      property DelayMs         : integer    read FDelayMs        write FDelayMs;
      property Scale           : single     read FScale          write FScale;
      property SnapshotPath    : ustring    read FSnapshotPath   write FSnapshotPath;
      property ErrorText       : ustring    read FErrorText;
  end;

implementation

constructor TEncapsulatedBrowser.Create;
begin
  inherited Create;

  FDefaultURL    := 'https://www.google.com';
  FThread        := nil;
  FWidth         := 1024;
  FHeight        := 768;
  FDelayMs       := 500;
  FScale         := 1;    // This is the relative scale to a 96 DPI screen. It's calculated with the formula : scale = custom_DPI / 96
  FSnapshotPath  := 'c:\windows\temp\snapshot.bmp';  // You need a directory where a Windows Service has rights to write a file
  FErrorText     := '';
end;

destructor TEncapsulatedBrowser.Destroy;
begin
  DestroyBrowser;

  inherited Destroy;
end;

procedure TEncapsulatedBrowser.CreateBrowser;
begin
  if (FThread = nil) then
    begin
      FThread                     := TCEFBrowserThread.Create(FDefaultURL, FWidth, FHeight, FDelayMs, FScale);
      FThread.SyncEvents          := False; // Needed for the Windows Service
      FThread.OnError             := Thread_OnError;
      FThread.OnSnapshotAvailable := Thread_OnSnapshotAvailable;
      FThread.Start;
    end;
end;

procedure TEncapsulatedBrowser.DestroyBrowser;
begin
  if (FThread <> nil) then
    begin
      if FThread.TerminateBrowserThread then
        FThread.WaitFor;

      FreeAndNil(FThread);
    end;
end;

procedure TEncapsulatedBrowser.Thread_OnError(Sender: TObject);
begin
  FErrorText := 'Error';

  if (FThread.ErrorCode <> 0) then
    FErrorText := FErrorText + ' ' + inttostr(FThread.ErrorCode);

  FErrorText := FErrorText + ' : ' + FThread.ErrorText;

  if (length(FThread.FailedUrl) > 0) then
    FErrorText := FErrorText + ' - ' + FThread.FailedUrl;

  OutputDebugString(PWideChar('WindowsServiceBrowser error : ' + FErrorText));
end;

procedure TEncapsulatedBrowser.Thread_OnSnapshotAvailable(Sender: TObject);
begin
  if FThread.SaveSnapshotToFile(FSnapshotPath) then
    OutputDebugString('WindowsServiceBrowser : snapshot saved successfully.')
   else
    begin
      FErrorText := 'There was an error copying the snapshot';
      OutputDebugString(PWideChar('WindowsServiceBrowser error : ' + FErrorText));
    end;
end;

end.
