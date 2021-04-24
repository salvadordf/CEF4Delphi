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

unit uCEFPrintSettings;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefPrintSettingsRef = class(TCefBaseRefCountedRef, ICefPrintSettings)
  protected
    function  IsValid: Boolean;
    function  IsReadOnly: Boolean;
    procedure SetOrientation(landscape: Boolean);
    function  IsLandscape: Boolean;
    procedure SetPrinterPrintableArea(const physicalSizeDeviceUnits: PCefSize; const printableAreaDeviceUnits: PCefRect; landscapeNeedsFlip: Boolean);
    procedure SetDeviceName(const name: ustring);
    function  GetDeviceName: ustring;
    procedure SetDpi(dpi: Integer);
    function  GetDpi: Integer;
    procedure SetPageRanges(const ranges: TCefRangeArray);
    function  GetPageRangesCount: NativeUInt;
    procedure GetPageRanges(out ranges: TCefRangeArray);
    procedure SetSelectionOnly(selectionOnly: Boolean);
    function  IsSelectionOnly: Boolean;
    procedure SetCollate(collate: Boolean);
    function  WillCollate: Boolean;
    procedure SetColorModel(model: TCefColorModel);
    function  GetColorModel: TCefColorModel;
    procedure SetCopies(copies: Integer);
    function  GetCopies: Integer;
    procedure SetDuplexMode(mode: TCefDuplexMode);
    function  GetDuplexMode: TCefDuplexMode;
  public
    class function New: ICefPrintSettings;
    class function UnWrap(data: Pointer): ICefPrintSettings;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;


function TCefPrintSettingsRef.GetColorModel: TCefColorModel;
begin
  Result := PCefPrintSettings(FData)^.get_color_model(FData);
end;

function TCefPrintSettingsRef.GetCopies: Integer;
begin
  Result := PCefPrintSettings(FData)^.get_copies(FData);
end;

function TCefPrintSettingsRef.GetDeviceName: ustring;
begin
  Result := CefStringFreeAndGet(PCefPrintSettings(FData)^.get_device_name(FData));
end;

function TCefPrintSettingsRef.GetDpi: Integer;
begin
  Result := PCefPrintSettings(FData)^.get_dpi(FData);
end;

function TCefPrintSettingsRef.GetDuplexMode: TCefDuplexMode;
begin
  Result := PCefPrintSettings(FData)^.get_duplex_mode(FData);
end;

procedure TCefPrintSettingsRef.GetPageRanges(out ranges: TCefRangeArray);
var
  TempLen : NativeUInt;
begin
  TempLen := GetPageRangesCount;
  SetLength(ranges, TempLen);

  if (TempLen > 0) then PCefPrintSettings(FData)^.get_page_ranges(FData, @TempLen, @ranges[0]);
end;

function TCefPrintSettingsRef.GetPageRangesCount: NativeUInt;
begin
  Result := PCefPrintSettings(FData)^.get_page_ranges_count(FData);
end;

function TCefPrintSettingsRef.IsLandscape: Boolean;
begin
  Result := PCefPrintSettings(FData)^.is_landscape(FData) <> 0;
end;

function TCefPrintSettingsRef.IsReadOnly: Boolean;
begin
  Result := PCefPrintSettings(FData)^.is_read_only(FData) <> 0;
end;

function TCefPrintSettingsRef.IsSelectionOnly: Boolean;
begin
  Result := PCefPrintSettings(FData)^.is_selection_only(FData) <> 0;
end;

function TCefPrintSettingsRef.IsValid: Boolean;
begin
  Result := PCefPrintSettings(FData)^.is_valid(FData) <> 0;
end;

class function TCefPrintSettingsRef.New: ICefPrintSettings;
begin
  Result := UnWrap(cef_print_settings_create());
end;

procedure TCefPrintSettingsRef.SetCollate(collate: Boolean);
begin
  PCefPrintSettings(FData)^.set_collate(FData, Ord(collate));
end;

procedure TCefPrintSettingsRef.SetColorModel(model: TCefColorModel);
begin
  PCefPrintSettings(FData)^.set_color_model(FData, model);
end;

procedure TCefPrintSettingsRef.SetCopies(copies: Integer);
begin
  PCefPrintSettings(FData)^.set_copies(FData, copies);
end;

procedure TCefPrintSettingsRef.SetDeviceName(const name: ustring);
var
  TempName : TCefString;
begin
  TempName := CefString(name);
  PCefPrintSettings(FData)^.set_device_name(FData, @TempName);
end;

procedure TCefPrintSettingsRef.SetDpi(dpi: Integer);
begin
  PCefPrintSettings(FData)^.set_dpi(FData, dpi);
end;

procedure TCefPrintSettingsRef.SetDuplexMode(mode: TCefDuplexMode);
begin
  PCefPrintSettings(FData)^.set_duplex_mode(FData, mode);
end;

procedure TCefPrintSettingsRef.SetOrientation(landscape: Boolean);
begin
  PCefPrintSettings(FData)^.set_orientation(FData, Ord(landscape));
end;

procedure TCefPrintSettingsRef.SetPageRanges(const ranges: TCefRangeArray);
var
  TempLen : NativeUInt;
begin
  TempLen := Length(ranges);

  if (TempLen > 0) then
    PCefPrintSettings(FData)^.set_page_ranges(FData, TempLen, @ranges[0])
   else
    PCefPrintSettings(FData)^.set_page_ranges(FData, 0, nil);
end;

procedure TCefPrintSettingsRef.SetPrinterPrintableArea(const physicalSizeDeviceUnits  : PCefSize;
                                                       const printableAreaDeviceUnits : PCefRect;
                                                             landscapeNeedsFlip       : Boolean);
begin
  PCefPrintSettings(FData)^.set_printer_printable_area(FData,
                                                       physicalSizeDeviceUnits,
                                                       printableAreaDeviceUnits,
                                                       Ord(landscapeNeedsFlip));
end;

procedure TCefPrintSettingsRef.SetSelectionOnly(selectionOnly: Boolean);
begin
  PCefPrintSettings(FData)^.set_selection_only(FData, Ord(selectionOnly));
end;

class function TCefPrintSettingsRef.UnWrap(data: Pointer): ICefPrintSettings;
begin
  if (data <> nil) then
    Result := Create(data) as ICefPrintSettings
   else
    Result := nil;
end;

function TCefPrintSettingsRef.WillCollate: Boolean;
begin
  Result := PCefPrintSettings(FData)^.will_collate(FData) <> 0;
end;

end.
