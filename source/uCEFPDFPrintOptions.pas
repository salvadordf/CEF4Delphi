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

unit uCEFPDFPrintOptions;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes,
  {$ELSE}
  Classes,
  {$ENDIF}
  uCEFTypes;

type
  TPDFPrintOptions = class
    protected
      FLandscape            : boolean;
      FPrintBackground      : boolean;
      FScale                : double;
      FPaperWidth           : double;
      FPaperHeight          : double;
      FPreferCSSPageSize    : boolean;
      FMarginType           : TCefPdfPrintMarginType;
      FMarginTop            : double;
      FMarginRight          : double;
      FMarginBottom         : double;
      FMarginLeft           : double;
      FPageRanges           : ustring;
      FDisplayHeaderFooter  : boolean;
      FHeaderTemplate       : ustring;
      FFooterTemplate       : ustring;

      function  GetScalePct: double;
      function  GetPaperWidthMM: double;
      function  GetPaperHeightMM: double;
      function  GetMarginTopMM: double;
      function  GetMarginRightMM: double;
      function  GetMarginBottomMM: double;
      function  GetMarginLeftMM: double;

      procedure SetScalePct(const aValue: double);
      procedure SetPaperWidthMM(const aValue: double);
      procedure SetPaperHeightMM(const aValue: double);
      procedure SetMarginTopMM(const aValue: double);
      procedure SetMarginRightMM(const aValue: double);
      procedure SetMarginBottomMM(const aValue: double);
      procedure SetMarginLeftMM(const aValue: double);

      function  InchesToMM(const aInches: double): double;
      function  MMToInches(const aMM: double): double;

    public
      constructor Create; virtual;
      procedure   CopyToSettings(var aSettings : TCefPdfPrintSettings);

      property Landscape             : boolean                 read FLandscape                write FLandscape;
      property PrintBackground       : boolean                 read FPrintBackground          write FPrintBackground;
      property PreferCSSPageSize     : boolean                 read FPreferCSSPageSize        write FPreferCSSPageSize;
      property PageRanges            : ustring                 read FPageRanges               write FPageRanges;
      property DisplayHeaderFooter   : boolean                 read FDisplayHeaderFooter      write FDisplayHeaderFooter;
      property HeaderTemplate        : ustring                 read FHeaderTemplate           write FHeaderTemplate;
      property FooterTemplate        : ustring                 read FFooterTemplate           write FFooterTemplate;

      property Scale                 : double                  read FScale                    write FScale;
      property ScalePct              : double                  read GetScalePct               write SetScalePct;

      property PaperWidthInch        : double                  read FPaperWidth               write FPaperWidth;
      property PaperHeightInch       : double                  read FPaperHeight              write FPaperHeight;
      property PaperWidthMM          : double                  read GetPaperWidthMM           write SetPaperWidthMM;
      property PaperHeightMM         : double                  read GetPaperHeightMM          write SetPaperHeightMM;

      property MarginType            : TCefPdfPrintMarginType  read FMarginType               write FMarginType;
      property MarginTopInch         : double                  read FMarginTop                write FMarginTop;
      property MarginRightInch       : double                  read FMarginRight              write FMarginRight;
      property MarginBottomInch      : double                  read FMarginBottom             write FMarginBottom;
      property MarginLeftInch        : double                  read FMarginLeft               write FMarginLeft;
      property MarginTopMM           : double                  read GetMarginTopMM            write SetMarginTopMM;
      property MarginRightMM         : double                  read GetMarginRightMM          write SetMarginRightMM;
      property MarginBottomMM        : double                  read GetMarginBottomMM         write SetMarginBottomMM;
      property MarginLeftMM          : double                  read GetMarginLeftMM           write SetMarginLeftMM;
  end;

implementation

uses
  uCEFMiscFunctions;

const
  MM_IN_ONE_INCH = 25.4;

constructor TPDFPrintOptions.Create;
begin
  FLandscape            := False;
  FPrintBackground      := False;
  FScale                := 0;
  FPaperWidth           := 0;
  FPaperHeight          := 0;
  FPreferCSSPageSize    := False;
  FMarginType           := PDF_PRINT_MARGIN_DEFAULT;
  FMarginTop            := 0;
  FMarginRight          := 0;
  FMarginBottom         := 0;
  FMarginLeft           := 0;
  FPageRanges           := '';
  FDisplayHeaderFooter  := False;
  FHeaderTemplate       := '';
  FFooterTemplate       := '';
end;

function TPDFPrintOptions.InchesToMM(const aInches: double): double;
begin
  Result := aInches * MM_IN_ONE_INCH;
end;

function TPDFPrintOptions.MMToInches(const aMM: double): double;
begin
  Result := aMM / MM_IN_ONE_INCH;
end;

function TPDFPrintOptions.GetScalePct: double;
begin
  if (FScale <= 0) then
    Result := 100
   else
    Result := FScale * 100;
end;

function TPDFPrintOptions.GetPaperWidthMM: double;
begin
  Result := InchesToMM(FPaperWidth);
end;

function TPDFPrintOptions.GetPaperHeightMM: double;
begin
  Result := InchesToMM(FPaperHeight);
end;

function TPDFPrintOptions.GetMarginTopMM: double;
begin
  Result := InchesToMM(FMarginTop);
end;

function TPDFPrintOptions.GetMarginRightMM: double;
begin
  Result := InchesToMM(FMarginRight);
end;

function TPDFPrintOptions.GetMarginBottomMM: double;
begin
  Result := InchesToMM(FMarginBottom);
end;

function TPDFPrintOptions.GetMarginLeftMM: double;
begin
  Result := InchesToMM(FMarginLeft);
end;

procedure TPDFPrintOptions.SetScalePct(const aValue: double);
begin
  if (aValue <= 0) then
    FScale := 0
   else
    FScale := aValue / 100;
end;

procedure TPDFPrintOptions.SetPaperWidthMM(const aValue: double);
begin
  if (aValue <= 0) then
    FPaperWidth := 0
   else
    FPaperWidth := MMToInches(aValue);
end;

procedure TPDFPrintOptions.SetPaperHeightMM(const aValue: double);
begin
  if (aValue <= 0) then
    FPaperHeight := 0
   else
    FPaperHeight := MMToInches(aValue);
end;

procedure TPDFPrintOptions.SetMarginTopMM(const aValue: double);
begin
  if (aValue <= 0) then
    FMarginTop := 0
   else
    FMarginTop := MMToInches(aValue);
end;

procedure TPDFPrintOptions.SetMarginRightMM(const aValue: double);
begin
  if (aValue <= 0) then
    FMarginRight := 0
   else
    FMarginRight := MMToInches(aValue);
end;

procedure TPDFPrintOptions.SetMarginBottomMM(const aValue: double);
begin
  if (aValue <= 0) then
    FMarginBottom := 0
   else
    FMarginBottom := MMToInches(aValue);
end;

procedure TPDFPrintOptions.SetMarginLeftMM(const aValue: double);
begin
  if (aValue <= 0) then
    FMarginLeft := 0
   else
    FMarginLeft := MMToInches(aValue);
end;

procedure TPDFPrintOptions.CopyToSettings(var aSettings : TCefPdfPrintSettings);
begin
  aSettings.landscape             := Ord(FLandscape);
  aSettings.print_background      := Ord(FPrintBackground);
  aSettings.scale                 := FScale;
  aSettings.paper_width           := FPaperWidth;
  aSettings.paper_height          := FPaperHeight;
  aSettings.prefer_css_page_size  := Ord(FPreferCSSPageSize);
  aSettings.margin_type           := FMarginType;
  aSettings.margin_top            := FMarginTop;
  aSettings.margin_right          := FMarginRight;
  aSettings.margin_bottom         := FMarginBottom;
  aSettings.margin_left           := FMarginLeft;
  aSettings.page_ranges           := CefString(FPageRanges);
  aSettings.display_header_footer := Ord(FDisplayHeaderFooter);
  aSettings.header_template       := CefString(FHeaderTemplate);
  aSettings.footer_template       := CefString(FFooterTemplate);
end;

end.
