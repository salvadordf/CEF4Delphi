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

unit uCEFPDFPrintOptions;

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
  TPDFPrintOptions = class(TPersistent)
    protected
      Fpage_width            : integer;
      Fpage_height           : Integer;
      Fscale_factor          : integer;
      Fmargin_top            : integer;
      Fmargin_right          : integer;
      Fmargin_bottom         : integer;
      Fmargin_left           : integer;
      Fmargin_type           : TCefPdfPrintMarginType;
      Fheader_footer_enabled : boolean;
      Fselection_only        : boolean;
      Flandscape             : boolean;
      Fbackgrounds_enabled   : boolean;

    public
      constructor Create; virtual;

    published
      property page_width            : integer                 read Fpage_width               write Fpage_width            default 0;
      property page_height           : integer                 read Fpage_height              write Fpage_height           default 0;
      property scale_factor          : integer                 read Fscale_factor             write Fscale_factor          default 0;
      property margin_top            : integer                 read Fmargin_top               write Fmargin_top;
      property margin_right          : integer                 read Fmargin_right             write Fmargin_right;
      property margin_bottom         : integer                 read Fmargin_bottom            write Fmargin_bottom;
      property margin_left           : integer                 read Fmargin_left              write Fmargin_left;
      property margin_type           : TCefPdfPrintMarginType  read Fmargin_type              write Fmargin_type           default PDF_PRINT_MARGIN_DEFAULT;
      property header_footer_enabled : boolean                 read Fheader_footer_enabled    write Fheader_footer_enabled default False;
      property selection_only        : boolean                 read Fselection_only           write Fselection_only        default False;
      property landscape             : boolean                 read Flandscape                write Flandscape             default False;
      property backgrounds_enabled   : boolean                 read Fbackgrounds_enabled      write Fbackgrounds_enabled   default False;
  end;

implementation

constructor TPDFPrintOptions.Create;
begin
  Fpage_width            := 0;
  Fpage_height           := 0;
  Fscale_factor          := 100;
  Fmargin_top            := 0;
  Fmargin_right          := 0;
  Fmargin_bottom         := 0;
  Fmargin_left           := 0;
  Fmargin_type           := PDF_PRINT_MARGIN_DEFAULT;
  Fheader_footer_enabled := False;
  Fselection_only        := False;
  Flandscape             := False;
  Fbackgrounds_enabled   := False;
end;

end.
