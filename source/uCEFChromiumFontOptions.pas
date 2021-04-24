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

unit uCEFChromiumFontOptions;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes,
  {$ELSE}
  Classes,
  {$ENDIF}
  uCEFTypes;

type
  TChromiumFontOptions = class(TPersistent)
    protected
      FStandardFontFamily     : ustring;
      FCursiveFontFamily      : ustring;
      FSansSerifFontFamily    : ustring;
      FMinimumLogicalFontSize : Integer;
      FFantasyFontFamily      : ustring;
      FSerifFontFamily        : ustring;
      FDefaultFixedFontSize   : Integer;
      FDefaultFontSize        : Integer;
      FRemoteFontsDisabled    : TCefState;
      FFixedFontFamily        : ustring;
      FMinimumFontSize        : Integer;

    public
      constructor Create; virtual;

    published
      property StandardFontFamily     : ustring   read FStandardFontFamily     write FStandardFontFamily;
      property FixedFontFamily        : ustring   read FFixedFontFamily        write FFixedFontFamily;
      property SerifFontFamily        : ustring   read FSerifFontFamily        write FSerifFontFamily;
      property SansSerifFontFamily    : ustring   read FSansSerifFontFamily    write FSansSerifFontFamily;
      property CursiveFontFamily      : ustring   read FCursiveFontFamily      write FCursiveFontFamily;
      property FantasyFontFamily      : ustring   read FFantasyFontFamily      write FFantasyFontFamily;
      property DefaultFontSize        : Integer   read FDefaultFontSize        write FDefaultFontSize        default 0;
      property DefaultFixedFontSize   : Integer   read FDefaultFixedFontSize   write FDefaultFixedFontSize   default 0;
      property MinimumFontSize        : Integer   read FMinimumFontSize        write FMinimumFontSize        default 0;
      property MinimumLogicalFontSize : Integer   read FMinimumLogicalFontSize write FMinimumLogicalFontSize default 0;
      property RemoteFonts            : TCefState read FRemoteFontsDisabled    write FRemoteFontsDisabled    default STATE_DEFAULT;
  end;

implementation

constructor TChromiumFontOptions.Create;
begin
  FStandardFontFamily     := '';
  FCursiveFontFamily      := '';
  FSansSerifFontFamily    := '';
  FMinimumLogicalFontSize := 0;
  FFantasyFontFamily      := '';
  FSerifFontFamily        := '';
  FDefaultFixedFontSize   := 0;
  FDefaultFontSize        := 0;
  FRemoteFontsDisabled    := STATE_DEFAULT;
  FFixedFontFamily        := '';
  FMinimumFontSize        := 0;
end;

end.
