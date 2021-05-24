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

unit uCEFLoader;

interface

{$IFDEF LINUX}
uses
  FMUX.Config;

procedure InitializeGTK;
{$ENDIF}

implementation

{$IFDEF LINUX}
uses
  System.SysUtils, System.IOUtils;

function GetLibDirName: string;
begin
  {$IFNDEF FMXLINUX_EXTERNAL_RUNTIME}
  Result := TPath.Combine(TPath.GetHomePath, '.fmxlinux');
  {$ELSE}
  Result := ExtractFilePath(ParamStr(0));
  {$ENDIF}
end;

function GetLibFileName: string;
const
  LibName = 'libfmux';
  LibVer = '1.60';
begin
  {$IFDEF FMXLINUX_EXTERNAL_RUNTIME}
  Result := TPath.Combine(GetLibDirName, LibName + '.so');
  {$ELSE}
    {$IFDEF TRIAL}
    Result := TPath.Combine(GetLibDirName, LibName + '-Trial-' + LibVer + '.so');
    {$ELSEIF GETIT}
    Result := TPath.Combine(GetLibDirName, LibName + '-Getit' + LibVer + '.so');
    {$ELSE}
    Result := TPath.Combine(GetLibDirName, LibName + '-' + LibVer + '.so');
    {$ENDIF}
  {$ENDIF}
end;

procedure InitializeGTK;
var
  FmuxInit: procedure (Flags: Integer); cdecl;
  TempHandle : NativeInt;
begin
  TempHandle := LoadLibrary(PChar(GetLibFileName));

  if (TempHandle <> 0) then
    begin
      FmuxInit := GetProcAddress(TempHandle, 'FmuxInit');
      FmuxInit(0);
    end;
end;

initialization
  DoNotCallFmuxInit := True;
{$ENDIF}

end.
