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
//        Copyright © 2021 Salvador Diaz Fau. All rights reserved.
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

unit uFMXMiscFunctions;

interface

procedure CopyCEFFramework;
procedure CopyCEFHelpers(const aProjectName : string);

implementation

uses
  System.SysUtils, System.Types, System.IOUtils, Posix.Stdio,
  uCEFMiscFunctions;

const
  PRJ_HELPER_SUBFIX   = '_helper';
  PRJ_GPU_SUBFIX      = '_helper_gpu';
  PRJ_PLUGIN_SUBFIX   = '_helper_plugin';
  PRJ_RENDERER_SUBFIX = '_helper_renderer';
  HELPER_SUBFIX   = ' Helper';
  GPU_SUBFIX      = ' Helper (GPU)';
  PLUGIN_SUBFIX   = ' Helper (Plugin)';
  RENDERER_SUBFIX = ' Helper (Renderer)';

procedure CopyAllFiles(const aSrcPath, aDstPath: string);
var
  TempDirectories, TempFiles : TStringDynArray;
  i : integer;
  TempNewDstPath, TempSrcFile, TempDstFile : string;
begin
  try
    TempDirectories := TDirectory.GetDirectories(aSrcPath);

    for i := 0 to pred(Length(TempDirectories)) do
      begin
        TempNewDstPath := aDstPath + TempDirectories[i].Substring(TDirectory.GetParent(TempDirectories[i]).Length);

        if not(TDirectory.Exists(TempNewDstPath)) then
          TDirectory.CreateDirectory(TempNewDstPath);

        CopyAllFiles(TempDirectories[i], TempNewDstPath);
      end;

    TempFiles := TDirectory.GetFiles(aSrcPath);

    for i := 0 to pred(Length(TempFiles)) do
      begin
        TempSrcFile := TempFiles[i];
        TempDstFile := aDstPath + TPath.DirectorySeparatorChar + TPath.GetFileName(TempFiles[i]);
        TFile.Copy(TempSrcFile, TempDstFile);
        TFile.SetAttributes(TempDstFile, TFile.GetAttributes(TempSrcFile));
      end;
  except
    on e : exception do
      WriteLn('CopyAllFiles error : ' + e.Message);
  end;
end;

procedure CopyCEFFramework;
const
  CEF_FRAMEWORK_DIR = 'Chromium Embedded Framework.framework';
var
  appFrameworksPath, dstCEFPath, srcCEFPath : string;
begin
  try
    appFrameworksPath := TDirectory.GetParent(ExtractFileDir(ParamStr(0))) + TPath.DirectorySeparatorChar + 'Frameworks';
    dstCEFPath        := appFrameworksPath + TPath.DirectorySeparatorChar + CEF_FRAMEWORK_DIR;
    srcCEFPath        := TDirectory.GetParent(GetModulePath) + TPath.DirectorySeparatorChar + CEF_FRAMEWORK_DIR;

    if not(TDirectory.Exists(appFrameworksPath)) then
      TDirectory.CreateDirectory(appFrameworksPath);

    if TDirectory.Exists(srcCEFPath) and
       not(TDirectory.Exists(dstCEFPath)) then
      begin
        TDirectory.CreateDirectory(dstCEFPath);
        CopyAllFiles(srcCEFPath, dstCEFPath);
      end;
  except
    on e : exception do
      WriteLn('CopyCEFFramework error : ' + e.Message);
  end;
end;

procedure RenameCEFHelper(const aHelperPrjPath : string);
var
  appBundleName, appBundlePath, appNewBundlePath, appExecutable, appExecPath,
  appNewName, appOldSubfix, appNewSubfix : string;
begin
  try
    appBundleName := TPath.GetFileNameWithoutExtension(aHelperPrjPath);

    if appBundleName.EndsWith(PRJ_HELPER_SUBFIX) then
      begin
        appOldSubfix := PRJ_HELPER_SUBFIX;
        appNewSubfix := HELPER_SUBFIX;
      end
     else
      if appBundleName.EndsWith(PRJ_GPU_SUBFIX) then
        begin
          appOldSubfix := PRJ_GPU_SUBFIX;
          appNewSubfix := GPU_SUBFIX;
        end
       else
        if appBundleName.EndsWith(PRJ_PLUGIN_SUBFIX) then
          begin
            appOldSubfix := PRJ_PLUGIN_SUBFIX;
            appNewSubfix := PLUGIN_SUBFIX;
          end
         else
          if appBundleName.EndsWith(PRJ_RENDERER_SUBFIX) then
            begin
              appOldSubfix := PRJ_RENDERER_SUBFIX;
              appNewSubfix := RENDERER_SUBFIX;
            end
           else
            exit;

    appBundlePath := TPath.GetDirectoryName(aHelperPrjPath);
    appExecPath   := aHelperPrjPath + TPath.DirectorySeparatorChar +
                     'Contents' + TPath.DirectorySeparatorChar +
                     'MacOS' + TPath.DirectorySeparatorChar;
    appNewName    := appBundleName.Remove(appBundleName.LastIndexOf(appOldSubfix)) +
                     appNewSubfix;
    appExecutable := appExecPath + TPath.DirectorySeparatorChar + appBundleName;

    if TFile.Exists(appExecutable) then
      begin
        RenameFile(appExecutable, appExecPath + TPath.DirectorySeparatorChar + appNewName);
        appNewBundlePath := appBundlePath + TPath.DirectorySeparatorChar + appNewName + '.app';

        if TDirectory.Exists(appNewBundlePath) then
          TDirectory.Delete(appNewBundlePath, True);

        RenameFile(aHelperPrjPath, appNewBundlePath);
      end;
  except
    on e: exception do
      WriteLn('RenameCEFHelper error : ' + e.Message);
  end;
end;

procedure CopyCEFHelpers(const aProjectName : string);
const
  projectSubfixes : array [0..3] of string = (PRJ_HELPER_SUBFIX, PRJ_GPU_SUBFIX, PRJ_PLUGIN_SUBFIX, PRJ_RENDERER_SUBFIX);
  helperSubfixes  : array [0..3] of string = (HELPER_SUBFIX, GPU_SUBFIX, PLUGIN_SUBFIX, RENDERER_SUBFIX);
var
  appParentPath, appFrameworksPath : string;
  srcBundlePath, dstBundlePath : string;
  helperBundlePath, prjBundleName, helperBundleName : string;
  i : integer;
begin
  appParentPath     := TDirectory.GetParent(GetModulePath);
  appFrameworksPath := TDirectory.GetParent(ExtractFileDir(ParamStr(0))) + TPath.DirectorySeparatorChar + 'Frameworks';

  for i := 0 to 3 do
    begin
      prjBundleName    := aProjectName + projectSubfixes[i] + '.app';
      helperBundleName := aProjectName + helperSubfixes[i]  + '.app';

      srcBundlePath    := appParentPath     + TPath.DirectorySeparatorChar + prjBundleName;
      dstBundlePath    := appFrameworksPath + TPath.DirectorySeparatorChar + prjBundleName;
      helperBundlePath := appFrameworksPath + TPath.DirectorySeparatorChar + helperBundleName;

      if TDirectory.Exists(srcBundlePath) then
        begin
          if TDirectory.Exists(dstBundlePath) then
            TDirectory.Delete(dstBundlePath, True);

          if not(TDirectory.Exists(helperBundlePath)) or
             (TDirectory.GetCreationTimeUtc(srcBundlePath) > TDirectory.GetCreationTimeUtc(helperBundlePath)) then
            begin
              CopyAllFiles(srcBundlePath, dstBundlePath);
              RenameCEFHelper(dstBundlePath);
            end;
        end;
    end;
end;



end.
