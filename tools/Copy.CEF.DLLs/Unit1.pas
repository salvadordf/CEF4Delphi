unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls;

type
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Panel1: TPanel;
    Label1: TLabel;
    edtRootFolder: TEdit;
    Panel2: TPanel;
    MemoPanel: TPanel;
    Label2: TLabel;
    Memo1: TMemo;
    btnCopy: TButton;
    Panel3: TPanel;
    btnBack: TButton;
    Panel4: TPanel;
    Memo2: TMemo;
    procedure btnBackClick(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure AddLog(const Msg: string);
    procedure CopyFilesToFolder(const SrcSubFolder, DestFolder, DestSubFolder:
        string; var CopyCount, ErrorCount: Integer);
    procedure CopyToFolder(DestDir: string);
    function GetRootDir: string;
    function GetSettingsFileName: string;
    procedure LoadSettings;
    procedure SaveSettings;
    function ValidateRootDir: Boolean;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

// This tool was created by Rael Bauer (raelb)
// CopyCEFDlls is used to copy cef dll files to multiple destination folders.

{
  Copies DLLs according to default distribution, i.e.:
  \Release              -> \DestDir
  \Release\swiftshader  -> \DestDir\swiftshader
  \Resources            -> \DestDir
  \Resources\locales    -> \DestDir\locales
}

implementation

uses
  Utils;

{$R *.dfm}

procedure TForm1.AddLog(const Msg: string);
begin
  Memo2.Lines.Add(Msg);
end;

procedure TForm1.btnBackClick(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 0;
end;

procedure TForm1.btnCopyClick(Sender: TObject);
var
  I: Integer;
begin
  PageControl1.ActivePageIndex := 1;
  Application.ProcessMessages;
  Screen.Cursor := crHourGlass;
  try
    for I := 0 to Memo1.Lines.Count - 1 do
      CopyToFolder(RemoveTrailingBackSlash(Memo1.Lines[I]));
    AddLog('Done.');
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.CopyFilesToFolder(const SrcSubFolder, DestFolder,
    DestSubFolder: string; var CopyCount, ErrorCount: Integer);
var
  DestFile: string;
  I: Integer;
  SourceFiles: TStringList;
  SrcFile: string;
begin
  SourceFiles := TStringList.Create;
  try
    GetFolderContents(GetRootDir + SrcSubFolder, SourceFiles, False, False,
      True, False);

    if not DirectoryExists(DestFolder + DestSubFolder) then
      CreateDir(DestFolder + DestSubFolder);

    for I := 0 to SourceFiles.Count - 1 do
    begin
      SrcFile  := GetRootDir + SrcSubFolder + SourceFiles[I];
      DestFile := DestFolder + DestSubFolder + SourceFiles[I];

      try
        CopyFile(PChar(SrcFile), PChar(DestFile), False);
        Inc(CopyCount);
      except
        On E: Exception do
        begin
          AddLog('Error copying file: ' + SourceFiles[I]);
          AddLog('To: ' + DestFolder + SrcSubFolder);
          AddLog(E.ClassName +', ' + E.Message);
          AddLog('');
          Inc(ErrorCount);
        end;
      end;
    end;
  finally
    SourceFiles.Free;
  end;
end;

procedure TForm1.CopyToFolder(DestDir: string);
var
  CopyCount: Integer;
  ErrorCount: Integer;
begin
  if not ValidateRootDir then
    exit;

  AddLog('Processing "' + DestDir + '"');
  if not DirectoryExists(DestDir) then
  begin
    AddLog('Folder not found.');
    AddLog('');
    exit;
  end;

  CopyCount := 0;
  ErrorCount := 0;
  CopyFilesToFolder('\Release', DestDir, '', CopyCount, ErrorCount);
  CopyFilesToFolder('\Release\swiftshader', DestDir, '\swiftshader', CopyCount, ErrorCount);
  CopyFilesToFolder('\Resources', DestDir, '', CopyCount, ErrorCount);
  CopyFilesToFolder('\Resources\locales', DestDir, '\locales', CopyCount, ErrorCount);

  AddLog(CopyCount.ToString +' files copied. ' + ErrorCount.ToString +' errors.');
  AddLog('');
end;

function TForm1.ValidateRootDir: Boolean;
begin
  Result := False;

  if not DirectoryExists(GetRootDir) then
  begin
    AddLog('Root directory does not exist');
    exit;
  end;

  if not DirectoryExists(GetRootDir + '\Release') then
  begin
    AddLog('\Release directory does not exist');
    exit;
  end;

  if not DirectoryExists(GetRootDir + '\Release\swiftshader') then
  begin
    AddLog('\Release\swiftshader directory does not exist');
    exit;
  end;

  if not DirectoryExists(GetRootDir + '\Resources') then
  begin
    AddLog('\Resources directory does not exist');
    exit;
  end;

  if not DirectoryExists(GetRootDir + '\Resources\locales') then
  begin
    AddLog('\Resources\locales directory does not exist');
    exit;
  end;

  Result := True;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  SaveSettings;
end;

function TForm1.GetSettingsFileName: string;
begin
  Result := ExtractFilePath(Application.ExeName) + 'settings.txt';
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  LoadSettings;
  PageControl1.ActivePageIndex := 0;
end;

function TForm1.GetRootDir: string;
begin
  Result := RemoveTrailingBackSlash(edtRootFolder.Text);
end;

procedure TForm1.LoadSettings;
var
  StrList: TStringList;
begin
  if FileExists(GetSettingsFileName) then
  begin
    StrList := TStringList.Create;
    try
      StrList.LoadFromFile(GetSettingsFileName);
      edtRootFolder.Text := StrList.Values['root_dir'];
      StringToList(StrList.Values['paths'], '|', Memo1.Lines);
    finally
      StrList.Free;
    end;
  end;
end;

procedure TForm1.SaveSettings;
var
  S: string;
  StrList: TStringList;
  I: Integer;
begin
  StrList := TStringList.Create;
  try
    StrList.Values['root_dir'] := edtRootFolder.Text;
    S := '';
    for I := 0 to Memo1.Lines.Count - 1 do
      S := S + Memo1.Lines[I] + '|';
    StrList.Values['paths'] := S;
    StrList.SaveToFile(GetSettingsFileName);
  finally
    StrList.Free;
  end;
end;

end.
