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
  /// <summary>
  /// The TPDFPrintOptions properties are used to fill the TCefPdfPrintSettings record which is used in the TChromiumCore.PrintToPDF call.
  /// </summary>
  TPDFPrintOptions = class
    protected
      FLandscape                : boolean;
      FPrintBackground          : boolean;
      FScale                    : double;
      FPaperWidth               : double;
      FPaperHeight              : double;
      FPreferCSSPageSize        : boolean;
      FMarginType               : TCefPdfPrintMarginType;
      FMarginTop                : double;
      FMarginRight              : double;
      FMarginBottom             : double;
      FMarginLeft               : double;
      FPageRanges               : ustring;
      FDisplayHeaderFooter      : boolean;
      FHeaderTemplate           : ustring;
      FFooterTemplate           : ustring;
      FGenerateTaggedPDF        : boolean;
      FGenerateDocumentOutline  : boolean;

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
      /// <summary>
      /// Constructor of TPDFPrintOptions
      /// </summary>
      constructor Create; virtual;
      /// <summary>
      /// Copy the fields of this class to the TCefPdfPrintSettings parameter.
      /// </summary>
      procedure   CopyToSettings(var aSettings : TCefPdfPrintSettings);
      /// <summary>
      /// Set to true for landscape mode or false for portrait mode.
      /// </summary>
      property Landscape                 : boolean                 read FLandscape                write FLandscape;
      /// <summary>
      /// Set to true to print background graphics.
      /// </summary>
      property PrintBackground           : boolean                 read FPrintBackground          write FPrintBackground;
      /// <summary>
      /// Set to true to prefer page size as defined by css. Defaults to false,
      /// in which case the content will be scaled to fit the paper size.
      /// </summary>
      property PreferCSSPageSize         : boolean                 read FPreferCSSPageSize        write FPreferCSSPageSize;
      /// <summary>
      /// <para>Paper ranges to print, one based, e.g., '1-5, 8, 11-13'. Pages are printed
      /// in the document order, not in the order specified, and no more than once.
      /// Defaults to empty string, which implies the entire document is printed.</para>
      /// <para>The page numbers are quietly capped to actual page count of the document,
      /// and ranges beyond the end of the document are ignored. If this results in
      /// no pages to print, an error is reported. It is an error to specify a range
      /// with start greater than end.</para>
      /// </summary>
      property PageRanges                : ustring                 read FPageRanges               write FPageRanges;
      /// <summary>
      /// Set to true to display the header and/or footer. Modify
      /// HeaderTemplate and/or FooterTemplate to customize the display.
      /// </summary>
      property DisplayHeaderFooter       : boolean                 read FDisplayHeaderFooter      write FDisplayHeaderFooter;
      /// <summary>
      /// <para>HTML template for the print header. Only displayed if
      /// DisplayHeaderFooter is true. Should be valid HTML markup with
      /// the following classes used to inject printing values into them:</para>
      /// <code>
      /// - date: formatted print date
      /// - title: document title
      /// - url: document location
      /// - pageNumber: current page number
      /// - totalPages: total pages in the document
      /// </code>
      /// <para>For example, "<span class=title></span>" would generate a span containing
      /// the title.</para>
      /// </summary>
      property HeaderTemplate            : ustring                 read FHeaderTemplate           write FHeaderTemplate;
      /// <summary>
      /// HTML template for the print footer. Only displayed if
      /// DisplayHeaderFooter is true. Uses the same format as
      /// HeaderTemplate.
      /// </summary>
      property FooterTemplate            : ustring                 read FFooterTemplate           write FFooterTemplate;
      /// <summary>
      /// Set to true to generate tagged (accessible) PDF.
      /// </summary>
      property GenerateTaggedPDF         : boolean                 read FGenerateTaggedPDF        write FGenerateTaggedPDF;
      /// <summary>
      /// Set to true to generate a document outline.
      /// </summary>
      property GenerateDocumentOutline   : boolean                 read FGenerateDocumentOutline  write FGenerateDocumentOutline;
      /// <summary>
      /// The percentage to scale the PDF by before printing (e.g. .5 is 50%).
      /// If this value is less than or equal to zero the default value of 1.0
      /// will be used.
      /// </summary>
      property Scale                     : double                  read FScale                    write FScale;
      /// <summary>
      /// The percentage value to scale the PDF by before printing (e.g. 50 is 50%).
      /// </summary>
      property ScalePct                  : double                  read GetScalePct               write SetScalePct;
      /// <summary>
      /// Output paper width in inches. If either of these values is less than or
      /// equal to zero then the default paper size (letter, 8.5 x 11 inches) will
      /// be used.
      /// </summary>
      property PaperWidthInch            : double                  read FPaperWidth               write FPaperWidth;
      /// <summary>
      /// Output paper height in inches. If either of these values is less than or
      /// equal to zero then the default paper size (letter, 8.5 x 11 inches) will
      /// be used.
      /// </summary>
      property PaperHeightInch           : double                  read FPaperHeight              write FPaperHeight;
      /// <summary>
      /// Output paper width in mm.
      /// </summary>
      property PaperWidthMM              : double                  read GetPaperWidthMM           write SetPaperWidthMM;
      /// <summary>
      /// Output paper height in mm.
      /// </summary>
      property PaperHeightMM             : double                  read GetPaperHeightMM          write SetPaperHeightMM;
      /// <summary>
      /// Margin type.
      /// </summary>
      property MarginType                : TCefPdfPrintMarginType  read FMarginType               write FMarginType;
      /// <summary>
      /// Top margin in inches. Only used if MarginType is set to
      /// PDF_PRINT_MARGIN_CUSTOM.
      /// </summary>
      property MarginTopInch             : double                  read FMarginTop                write FMarginTop;
      /// <summary>
      /// Right margin in inches. Only used if MarginType is set to
      /// PDF_PRINT_MARGIN_CUSTOM.
      /// </summary>
      property MarginRightInch           : double                  read FMarginRight              write FMarginRight;
      /// <summary>
      /// Bottom margin in inches. Only used if MarginType is set to
      /// PDF_PRINT_MARGIN_CUSTOM.
      /// </summary>
      property MarginBottomInch          : double                  read FMarginBottom             write FMarginBottom;
      /// <summary>
      /// Left margin in inches. Only used if MarginType is set to
      /// PDF_PRINT_MARGIN_CUSTOM.
      /// </summary>
      property MarginLeftInch            : double                  read FMarginLeft               write FMarginLeft;
      /// <summary>
      /// Top margin in mm.
      /// </summary>
      property MarginTopMM               : double                  read GetMarginTopMM            write SetMarginTopMM;
      /// <summary>
      /// Right margin in mm.
      /// </summary>
      property MarginRightMM             : double                  read GetMarginRightMM          write SetMarginRightMM;
      /// <summary>
      /// Bottom margin in mm.
      /// </summary>
      property MarginBottomMM            : double                  read GetMarginBottomMM         write SetMarginBottomMM;
      /// <summary>
      /// Left margin in mm.
      /// </summary>
      property MarginLeftMM              : double                  read GetMarginLeftMM           write SetMarginLeftMM;
  end;

implementation

uses
  uCEFMiscFunctions;

const
  MM_IN_ONE_INCH = 25.4;

constructor TPDFPrintOptions.Create;
begin
  FLandscape               := False;
  FPrintBackground         := False;
  FScale                   := 0;
  FPaperWidth              := 0;
  FPaperHeight             := 0;
  FPreferCSSPageSize       := False;
  FMarginType              := PDF_PRINT_MARGIN_DEFAULT;
  FMarginTop               := 0;
  FMarginRight             := 0;
  FMarginBottom            := 0;
  FMarginLeft              := 0;
  FPageRanges              := '';
  FDisplayHeaderFooter     := False;
  FHeaderTemplate          := '';
  FFooterTemplate          := '';
  FGenerateTaggedPDF       := False;
  FGenerateDocumentOutline := False;
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
  aSettings.landscape                 := Ord(FLandscape);
  aSettings.print_background          := Ord(FPrintBackground);
  aSettings.scale                     := FScale;
  aSettings.paper_width               := FPaperWidth;
  aSettings.paper_height              := FPaperHeight;
  aSettings.prefer_css_page_size      := Ord(FPreferCSSPageSize);
  aSettings.margin_type               := FMarginType;
  aSettings.margin_top                := FMarginTop;
  aSettings.margin_right              := FMarginRight;
  aSettings.margin_bottom             := FMarginBottom;
  aSettings.margin_left               := FMarginLeft;
  aSettings.page_ranges               := CefString(FPageRanges);
  aSettings.display_header_footer     := Ord(FDisplayHeaderFooter);
  aSettings.header_template           := CefString(FHeaderTemplate);
  aSettings.footer_template           := CefString(FFooterTemplate);
  aSettings.generate_tagged_pdf       := Ord(FGenerateTaggedPDF);
  aSettings.generate_document_outline := Ord(FGenerateDocumentOutline);
end;

end.
