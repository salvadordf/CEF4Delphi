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
