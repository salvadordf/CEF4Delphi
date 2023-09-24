unit uCEFChromiumFontOptions;

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
  /// The TChromiumFontOptions properties are used to fill the TCefBrowserSettings record which is used during the browser creation.
  /// </summary>
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
      /// <summary>
      /// Constructor of TChromiumFontOptions
      /// </summary>
      constructor Create; virtual;

    published
      /// <summary>
      /// Standard font family name.
      /// </summary>
      property StandardFontFamily     : ustring   read FStandardFontFamily     write FStandardFontFamily;
      /// <summary>
      /// Fixed font family name.
      /// </summary>
      property FixedFontFamily        : ustring   read FFixedFontFamily        write FFixedFontFamily;
      /// <summary>
      /// Serif font family name.
      /// </summary>
      property SerifFontFamily        : ustring   read FSerifFontFamily        write FSerifFontFamily;
      /// <summary>
      /// SansSerif font family name.
      /// </summary>
      property SansSerifFontFamily    : ustring   read FSansSerifFontFamily    write FSansSerifFontFamily;
      /// <summary>
      /// Cursive font family name.
      /// </summary>
      property CursiveFontFamily      : ustring   read FCursiveFontFamily      write FCursiveFontFamily;
      /// <summary>
      /// Fantasy font family name.
      /// </summary>
      property FantasyFontFamily      : ustring   read FFantasyFontFamily      write FFantasyFontFamily;
      /// <summary>
      /// Default font size.
      /// </summary>
      property DefaultFontSize        : Integer   read FDefaultFontSize        write FDefaultFontSize        default 0;
      /// <summary>
      /// Default fixed font size.
      /// </summary>
      property DefaultFixedFontSize   : Integer   read FDefaultFixedFontSize   write FDefaultFixedFontSize   default 0;
      /// <summary>
      /// Minimum font size.
      /// </summary>
      property MinimumFontSize        : Integer   read FMinimumFontSize        write FMinimumFontSize        default 0;
      /// <summary>
      /// Minimum logical font size.
      /// </summary>
      property MinimumLogicalFontSize : Integer   read FMinimumLogicalFontSize write FMinimumLogicalFontSize default 0;
      /// <summary>
      /// Controls the loading of fonts from remote sources. Also configurable using
      /// the "disable-remote-fonts" command-line switch.
      /// </summary>
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
