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

unit uCEFArgCopy;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}


interface

uses
  {$IFDEF DELPHI16_UP}
    System.Classes, System.SysUtils, System.AnsiStrings;
  {$ELSE}
    Classes, SysUtils;
  {$ENDIF}

type
  TCEFArgCopy = class
    protected
      FArgCCopy : longint;
      FArgVCopy : PPAnsiChar;

      procedure InitializeFields;
      procedure DestroyFields;

    public
      constructor Create;
      destructor  Destroy; override;
      procedure   CopyFromArgs(aArgc : longint; aArgv : PPAnsiChar);

      property argc : longint     read FArgCCopy;
      property argv : PPAnsiChar  read FArgVCopy;
  end;

implementation

{$POINTERMATH ON}

constructor TCEFArgCopy.Create;
begin
  inherited Create;

  InitializeFields;
end;

destructor TCEFArgCopy.Destroy;
begin
  DestroyFields;

  inherited Destroy;
end;

procedure TCEFArgCopy.InitializeFields;
begin
  FArgCCopy := 0;
  FArgVCopy := nil;
end;

procedure TCEFArgCopy.DestroyFields;
var
  i : integer;
begin
  if (FArgVCopy <> nil) then
    begin
      i := pred(FArgCCopy);

      while (i >= 0) do
        begin
          if (FArgVCopy[i] <> nil) then
            {$IFDEF DELPHI18_UP}System.AnsiStrings.{$ENDIF}StrDispose(FArgVCopy[i]);

          dec(i);
        end;

      FreeMem(FArgVCopy);
    end;

  InitializeFields;
end;

procedure TCEFArgCopy.CopyFromArgs(aArgc : longint; aArgv : PPAnsiChar);
var
  i : integer;
begin
  DestroyFields;

  if (aArgc > 0) and (aArgv <> nil) then
    begin
      i         := 0;
      FArgCCopy := aArgc;

      GetMem(FArgVCopy, (FArgCCopy + 1) * SizeOf(Pointer));

      while (i < aArgc) do
        begin
          {$IFDEF FPC}
            FArgVCopy[i] := StrAlloc(length(aArgv[i]) + 1);
            StrCopy(FArgVCopy[i], aArgv[i]);
          {$ELSE}
            {$IFDEF DELPHI18_UP}
              FArgVCopy[i] := System.AnsiStrings.AnsiStrAlloc(length(aArgv[i]) + 1);
              System.AnsiStrings.StrCopy(FArgVCopy[i], aArgv[i]);
            {$ELSE}
              FArgVCopy[i] := System.SysUtils.AnsiStrAlloc(length(aArgv[i]) + 1);
              System.SysUtils.StrCopy(FArgVCopy[i], aArgv[i]);
            {$ENDIF}
          {$ENDIF}

          inc(i);
        end;

      FArgVCopy[i] := nil;
    end;
end;

end.
