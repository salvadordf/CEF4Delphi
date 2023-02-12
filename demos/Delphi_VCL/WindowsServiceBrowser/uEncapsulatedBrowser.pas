// ************************************************************************
// ***************************** CEF4Delphi *******************************
// ************************************************************************
//
// CEF4Delphi is based on DCEF3 which uses CEF to embed a chromium-based
// browser in Delphi applications.
//
// The original license of DCEF3 still applies to CEF4Delphi.
//
// For more information about CEF4Delphi visit :
//         https://www.briskbard.com/index.php?lang=en&pageid=cef
//
//        Copyright © 2023 Salvador Diaz Fau. All rights reserved.
//
// ************************************************************************
// ************ vvvv Original license and comments below vvvv *************
// ************************************************************************
(*
 *                       Delphi Chromium Embedded 3
 *
 * Usage allowed under the restrictions of the Lesser GNU General Public License
 * or alternatively the restrictions of the Mozilla Public License 1.1
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * Unit owner : Henri Gourvest <hgourvest@gmail.com>
 * Web site   : http://www.progdigy.com
 * Repository : http://code.google.com/p/delphichromiumembedded/
 * Group      : http://groups.google.com/group/delphichromiumembedded
 *
 * Embarcadero Technologies, Inc is not permitted to use or redistribute
 * this source code without explicit permission.
 *
 *)

unit uEncapsulatedBrowser;

{$I cef.inc}

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
