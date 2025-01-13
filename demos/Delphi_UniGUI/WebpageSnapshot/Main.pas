unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, uniGUITypes, uniGUIAbstractClasses,
  uniGUIClasses, uniGUIRegClasses, uniGUIForm, uniImage, uniButton,
  uniMultiItem, uniComboBox, uniGUIBaseClasses, uniPanel,
  uCEFBrowserThread, uniStatusBar, uniMemo, uniTimer, uniCanvas;

type
  TMainForm = class(TUniForm)
    UniPanel1: TUniPanel;
    AddressCb: TUniComboBox;
    GoBtn: TUniButton;
    UniStatusBar1: TUniStatusBar;
    UniMemo1: TUniMemo;
    UniCanvas1: TUniCanvas;
    UniTimer1: TUniTimer;
    procedure UniFormCreate(Sender: TObject);
    procedure UniFormClose(Sender: TObject; var Action: TCloseAction);
    procedure GoBtnClick(Sender: TObject);
    procedure UniFormShow(Sender: TObject);
    procedure UniTimer1Timer(Sender: TObject);
  private
    FThread : TCEFBrowserThread;
    FMustRefresh : boolean;

    procedure Thread_OnError(Sender: TObject);
    procedure Thread_OnSnapshotAvailable(Sender: TObject);
    procedure Thread_OnHTMLAvailable(Sender: TObject);
  public
    { Public declarations }
  end;

function MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  uniGUIVars, MainModule, uniGUIApplication,
  uCEFApplication;

function MainForm: TMainForm;
begin
  Result := TMainForm(UniMainModule.GetFormInstance(TMainForm));
end;

procedure TMainForm.GoBtnClick(Sender: TObject);
begin
  UniStatusBar1.Panels[0].Text := 'Loading...';
  screen.cursor := crAppStart;
  FMustRefresh := False;

  if (FThread = nil) then
    begin
      FThread                     := TCEFBrowserThread.Create(AddressCb.Text, UniCanvas1.Width, UniCanvas1.Height);
      FThread.OnError             := Thread_OnError;
      FThread.OnSnapshotAvailable := Thread_OnSnapshotAvailable;
      FThread.OnHTMLAvailable     := Thread_OnHTMLAvailable;
      FThread.SyncEvents          := True;
      FThread.Start;
    end
   else
    FThread.LoadUrl(AddressCb.Text);
end;

procedure TMainForm.UniFormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (FThread <> nil) then
    begin
      if FThread.TerminateBrowserThread then
        FThread.WaitFor;

      FreeAndNil(FThread);
    end;
end;

procedure TMainForm.UniFormCreate(Sender: TObject);
begin
  FThread := nil;
end;

procedure TMainForm.UniFormShow(Sender: TObject);
begin
  if GlobalCEFApp.GlobalContextInitialized then
    UniMemo1.Lines.Add('GlobalCEFApp Initialized')
   else
    UniMemo1.Lines.Add('Error: ' + GlobalCEFApp.LastErrorMessage);

  UniMemo1.Refresh;
end;

procedure TMainForm.UniTimer1Timer(Sender: TObject);
begin
  if FMustRefresh then
    begin
      FMustRefresh := False;
      Repaint;
    end;
end;

procedure TMainForm.Thread_OnError(Sender: TObject);
begin
  UniStatusBar1.Panels[0].Text := 'Error ' + inttostr(FThread.ErrorCode) + ' : ' + FThread.ErrorText + ' - ' + FThread.FailedUrl;
  screen.cursor := crDefault;
end;

procedure TMainForm.Thread_OnSnapshotAvailable(Sender: TObject);
var
  TempBitmap : TBitmap;
begin
  TempBitmap    := nil;
  screen.cursor := crDefault;

  if FThread.CopySnapshot(TempBitmap) then
    begin
      UniCanvas1.Bitmap.Assign(TempBitmap);
      UniStatusBar1.Panels[0].Text := 'Snapshot copied successfully';
      TempBitmap.Free;
    end
   else
    UniStatusBar1.Panels[0].Text := 'There was an error copying the snapshot';
end;

procedure TMainForm.Thread_OnHTMLAvailable(Sender: TObject);
begin
  UniMemo1.Lines.Add(FThread.HTMLcopy);
  FMustRefresh := True;
end;

initialization
  RegisterAppFormClass(TMainForm);

end.
