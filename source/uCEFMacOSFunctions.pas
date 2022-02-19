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
//        Copyright © 2022 Salvador Diaz Fau. All rights reserved.
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

unit uCEFMacOSFunctions;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  System.UITypes,
  {$IFDEF MACOS}
  FMX.Helpers.Mac, System.Messaging, Macapi.CoreFoundation, Macapi.Foundation,
  {$ENDIF}
  uCEFMacOSConstants;

{$IFDEF MACOSX}
function  KeyToMacOSKeyCode(aKey : Word): integer;
{$ENDIF}
{$IFDEF MACOS}
procedure CopyCEFFramework;
procedure CopyCEFHelpers(const aProjectName : string);
procedure ShowMessageCF(const aHeading, aMessage : string; const aTimeoutInSecs : double = 0);
function  NSEventTrackingRunLoopMode: NSString;
{$ENDIF}

implementation

{$IFDEF MACOS}
uses
  System.SysUtils, System.Types, System.IOUtils, Posix.Stdio, FMX.Types,
  uCEFMiscFunctions;

const
  PRJ_HELPER_SUBFIX   = '_helper';
  PRJ_GPU_SUBFIX      = '_helper_gpu';
  PRJ_PLUGIN_SUBFIX   = '_helper_plugin';
  PRJ_RENDERER_SUBFIX = '_helper_renderer';
  HELPER_SUBFIX       = ' Helper';
  GPU_SUBFIX          = ' Helper (GPU)';
  PLUGIN_SUBFIX       = ' Helper (Plugin)';
  RENDERER_SUBFIX     = ' Helper (Renderer)';
{$ENDIF}

{$IFDEF MACOSX}
// Key Code translation following the information found in these documents :
// https://developer.apple.com/library/archive/documentation/mac/pdf/MacintoshToolboxEssentials.pdf
// https://eastmanreference.com/complete-list-of-applescript-key-codes
function KeyToMacOSKeyCode(aKey : Word): integer;
begin
  case aKey of
    vkBack             : Result := kVK_Delete;
    vkTab              : Result := kVK_Tab;
    vkClear            : Result := kVK_ANSI_KeypadClear;
    vkReturn           : Result := kVK_Return;
    vkShift            : Result := kVK_Shift;
    vkControl          : Result := kVK_Control;
    vkMenu             : Result := kVK_Option;
    vkPause            : Result := kVK_F15;
    vkCapital          : Result := kVK_CapsLock;
    vkEscape           : Result := kVK_Escape;
    vkSpace            : Result := kVK_Space;
    vkPrior            : Result := kVK_PageUp;
    vkNext             : Result := kVK_PageDown;
    vkEnd              : Result := kVK_End;
    vkHome             : Result := kVK_Home;
    vkLeft             : Result := kVK_LeftArrow;
    vkUp               : Result := kVK_UpArrow;
    vkRight            : Result := kVK_RightArrow;
    vkDown             : Result := kVK_DownArrow;
    vkSnapshot         : Result := kVK_F13;
    vkHelp,
    vkInsert           : Result := kVK_Help;
    vkDelete           : Result := kVK_ForwardDelete;
    vk0                : Result := kVK_ANSI_0;
    vk1                : Result := kVK_ANSI_1;
    vk2                : Result := kVK_ANSI_2;
    vk3                : Result := kVK_ANSI_3;
    vk4                : Result := kVK_ANSI_4;
    vk5                : Result := kVK_ANSI_5;
    vk6                : Result := kVK_ANSI_6;
    vk7                : Result := kVK_ANSI_7;
    vk8                : Result := kVK_ANSI_8;
    vk9                : Result := kVK_ANSI_9;
    vkA                : Result := kVK_ANSI_A;
    vkB                : Result := kVK_ANSI_B;
    vkC                : Result := kVK_ANSI_C;
    vkD                : Result := kVK_ANSI_D;
    vkE                : Result := kVK_ANSI_E;
    vkF                : Result := kVK_ANSI_F;
    vkG                : Result := kVK_ANSI_G;
    vkH                : Result := kVK_ANSI_H;
    vkI                : Result := kVK_ANSI_I;
    vkJ                : Result := kVK_ANSI_J;
    vkK                : Result := kVK_ANSI_K;
    vkL                : Result := kVK_ANSI_L;
    vkM                : Result := kVK_ANSI_M;
    vkN                : Result := kVK_ANSI_N;
    vkO                : Result := kVK_ANSI_O;
    vkP                : Result := kVK_ANSI_P;
    vkQ                : Result := kVK_ANSI_Q;
    vkR                : Result := kVK_ANSI_R;
    vkS                : Result := kVK_ANSI_S;
    vkT                : Result := kVK_ANSI_T;
    vkU                : Result := kVK_ANSI_U;
    vkV                : Result := kVK_ANSI_V;
    vkW                : Result := kVK_ANSI_W;
    vkX                : Result := kVK_ANSI_X;
    vkY                : Result := kVK_ANSI_Y;
    vkZ                : Result := kVK_ANSI_Z;
    vkLWin             : Result := kVK_Option;
    vkRWin             : Result := kVK_RightOption;
    vkNumpad0          : Result := kVK_ANSI_Keypad0;
    vkNumpad1          : Result := kVK_ANSI_Keypad1;
    vkNumpad2          : Result := kVK_ANSI_Keypad2;
    vkNumpad3          : Result := kVK_ANSI_Keypad3;
    vkNumpad4          : Result := kVK_ANSI_Keypad4;
    vkNumpad5          : Result := kVK_ANSI_Keypad5;
    vkNumpad6          : Result := kVK_ANSI_Keypad6;
    vkNumpad7          : Result := kVK_ANSI_Keypad7;
    vkNumpad8          : Result := kVK_ANSI_Keypad8;
    vkNumpad9          : Result := kVK_ANSI_Keypad9;
    vkMultiply         : Result := kVK_ANSI_KeypadMultiply;
    vkAdd              : Result := kVK_ANSI_KeypadPlus;
    vkSubtract         : Result := kVK_ANSI_KeypadMinus;
    vkDecimal          : Result := kVK_ANSI_KeypadDecimal;
    vkDivide           : Result := kVK_ANSI_KeypadDivide;
    vkF1               : Result := kVK_F1;
    vkF2               : Result := kVK_F2;
    vkF3               : Result := kVK_F3;
    vkF4               : Result := kVK_F4;
    vkF5               : Result := kVK_F5;
    vkF6               : Result := kVK_F6;
    vkF7               : Result := kVK_F7;
    vkF8               : Result := kVK_F8;
    vkF9               : Result := kVK_F9;
    vkF10              : Result := kVK_F10;
    vkF11              : Result := kVK_F11;
    vkF12              : Result := kVK_F12;
    vkF13              : Result := kVK_F13;
    vkF14              : Result := kVK_F14;
    vkF15              : Result := kVK_F15;
    vkF16              : Result := kVK_F16;
    vkF17              : Result := kVK_F17;
    vkF18              : Result := kVK_F18;
    vkF19              : Result := kVK_F19;
    vkF20              : Result := kVK_F20;
    vkNumLock          : Result := kVK_ANSI_KeypadClear;
    vkScroll           : Result := kVK_F14;
    vkLShift           : Result := kVK_Shift;
    vkRShift           : Result := kVK_RightShift;
    vkLControl         : Result := kVK_Control;
    vkRControl         : Result := kVK_RightControl;
    vkLMenu,
    vkRMenu            : Result := kVK_Command;
    vkVolumeMute       : Result := kVK_Mute;
    vkVolumeDown       : Result := kVK_VolumeDown;
    vkVolumeUp         : Result := kVK_VolumeUp;
    vkSemicolon        : Result := kVK_ANSI_Semicolon;
    vkEqual            : Result := kVK_ANSI_Equal;
    vkComma            : Result := kVK_ANSI_Comma;
    vkMinus            : Result := kVK_ANSI_Minus;
    vkPeriod           : Result := kVK_ANSI_Period;
    vkSlash            : Result := kVK_ANSI_Slash;
    vkTilde            : Result := kVK_ANSI_Grave;
    vkLeftBracket      : Result := kVK_ANSI_LeftBracket;
    vkBackslash        : Result := kVK_ANSI_Backslash;
    vkRightBracket     : Result := kVK_ANSI_RightBracket;
    vkQuote            : Result := kVK_ANSI_Quote;
    vkOem102           : Result := kVK_ANSI_Backslash;   // kVK_JIS_Yen in Japanese keyboards ?
    vkOemClear         : Result := kVK_ANSI_KeypadClear;
    else                 Result := 0;
  end;
end;
{$ENDIF}

{$IFDEF MACOS}
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
      FMX.Types.Log.d('CopyAllFiles error : ' + e.Message);
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
      FMX.Types.Log.d('CopyCEFFramework error : ' + e.Message);
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
    on e : exception do
      FMX.Types.Log.d('RenameCEFHelper error : ' + e.Message);
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

procedure ShowMessageCF(const aHeading, aMessage : string; const aTimeoutInSecs : double = 0);
var
  TempHeading, TempMessage : CFStringRef;
  TempResponse : CFOptionFlags;
begin
  TempHeading := CFStringCreateWithCharactersNoCopy(nil, PChar(aHeading), Length(AHeading), kCFAllocatorNull);
  TempMessage := CFStringCreateWithCharactersNoCopy(nil, PChar(aMessage), Length(AMessage), kCFAllocatorNull);

  try
    CFUserNotificationDisplayAlert(aTimeoutInSecs, kCFUserNotificationNoteAlertLevel, nil, nil, nil, TempHeading, TempMessage, nil, nil, nil, TempResponse);
  finally
    CFRelease(TempHeading);
    CFRelease(TempMessage);
  end;
end;

function NSEventTrackingRunLoopMode: NSString;
begin
  result := CocoaNSStringConst(libFoundation, 'NSEventTrackingRunLoopMode');
end;
{$ENDIF}

end.
