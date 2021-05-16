unit uAppHelperRenamer;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.IOUtils,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm1 = class(TForm)
    OpenDialog1: TOpenDialog;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  Posix.Stdio;

procedure TForm1.Button1Click(Sender: TObject);
const
  HELPER_SUBFIX   = '_helper';
  GPU_SUBFIX      = '_helper_gpu';
  PLUGIN_SUBFIX   = '_helper_plugin';
  RENDERER_SUBFIX = '_helper_renderer';
var
  appBundleName, appBundlePath, appNewBundlePath, appExecutable, appExecPath,
  appNewName, appOldSubfix, appNewSubfix: string;
begin
  if OpenDialog1.Execute then
    begin
      appBundleName := TPath.GetFileNameWithoutExtension(OpenDialog1.FileName);

      if appBundleName.EndsWith(HELPER_SUBFIX) then
        begin
          appOldSubfix := HELPER_SUBFIX;
          appNewSubfix := 'Helper';
        end
       else
        if appBundleName.EndsWith(GPU_SUBFIX) then
          begin
            appOldSubfix := GPU_SUBFIX;
            appNewSubfix := 'Helper (GPU)';
          end
         else
          if appBundleName.EndsWith(PLUGIN_SUBFIX) then
            begin
              appOldSubfix := PLUGIN_SUBFIX;
              appNewSubfix := 'Helper (Plugin)';
            end
           else
            if appBundleName.EndsWith(RENDERER_SUBFIX) then
              begin
                appOldSubfix := RENDERER_SUBFIX;
                appNewSubfix := 'Helper (Renderer)';
              end
             else
              exit;

      appBundlePath := TPath.GetDirectoryName(OpenDialog1.FileName);
      appExecPath   := OpenDialog1.FileName + TPath.DirectorySeparatorChar +
                       'Contents' + TPath.DirectorySeparatorChar +
                       'MacOS' + TPath.DirectorySeparatorChar;
      appNewName    := appBundleName.Remove(appBundleName.LastIndexOf(appOldSubfix)) + ' ' +
                       appNewSubfix;
      appExecutable := TPath.Combine(appExecPath, appBundleName);

      if TFile.Exists(appExecutable) then
        begin
          RenameFile(appExecutable, TPath.Combine(appExecPath, appNewName));
          appNewBundlePath := TPath.Combine(appBundlePath, appNewName + '.app');
          RenameFile(OpenDialog1.FileName, appNewBundlePath);
        end;
    end;
end;

end.
