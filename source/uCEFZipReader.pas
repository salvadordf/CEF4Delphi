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

unit uCEFZipReader;

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
  TCefZipReaderRef = class(TCefBaseRefCountedRef, ICefZipReader)
  protected
    function MoveToFirstFile: Boolean;
    function MoveToNextFile: Boolean;
    function MoveToFile(const fileName: ustring; caseSensitive: Boolean): Boolean;
    function Close: Boolean;
    function GetFileName: ustring;
    function GetFileSize: Int64;
    function GetFileLastModified: TCefTime;
    function OpenFile(const password: ustring): Boolean;
    function CloseFile: Boolean;
    function ReadFile(buffer: Pointer; bufferSize: NativeUInt): Integer;
    function Tell: Int64;
    function Eof: Boolean;
  public
    class function UnWrap(data: Pointer): ICefZipReader;
    class function New(const stream: ICefStreamReader): ICefZipReader;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;

function TCefZipReaderRef.Close: Boolean;
begin
  Result := PCefZipReader(FData)^.close(PCefZipReader(FData)) <> 0;
end;

function TCefZipReaderRef.CloseFile: Boolean;
begin
  Result := PCefZipReader(FData)^.close_file(PCefZipReader(FData)) <> 0;
end;

class function TCefZipReaderRef.New(const stream: ICefStreamReader): ICefZipReader;
begin
  Result := UnWrap(cef_zip_reader_create(CefGetData(stream)));
end;

function TCefZipReaderRef.Eof: Boolean;
begin
  Result := PCefZipReader(FData)^.eof(PCefZipReader(FData)) <> 0;
end;

function TCefZipReaderRef.GetFileLastModified: TCefTime;
begin
  Result := PCefZipReader(FData)^.get_file_last_modified(PCefZipReader(FData));
end;

function TCefZipReaderRef.GetFileName: ustring;
begin
  Result := CefStringFreeAndGet(PCefZipReader(FData)^.get_file_name(PCefZipReader(FData)));
end;

function TCefZipReaderRef.GetFileSize: Int64;
begin
  Result := PCefZipReader(FData)^.get_file_size(PCefZipReader(FData));
end;

function TCefZipReaderRef.MoveToFile(const fileName: ustring; caseSensitive: Boolean): Boolean;
var
  TempFilename : TCefString;
begin
  TempFilename := CefString(fileName);
  Result       := PCefZipReader(FData)^.move_to_file(PCefZipReader(FData), @TempFilename, Ord(caseSensitive)) <> 0;
end;

function TCefZipReaderRef.MoveToFirstFile: Boolean;
begin
  Result := PCefZipReader(FData)^.move_to_first_file(PCefZipReader(FData)) <> 0;
end;

function TCefZipReaderRef.MoveToNextFile: Boolean;
begin
  Result := PCefZipReader(FData)^.move_to_next_file(PCefZipReader(FData)) <> 0;
end;

function TCefZipReaderRef.OpenFile(const password: ustring): Boolean;
var
  TempPassword : TCefString;
begin
  TempPassword := CefString(password);
  Result       := PCefZipReader(FData)^.open_file(PCefZipReader(FData), @TempPassword) <> 0;
end;

function TCefZipReaderRef.ReadFile(buffer: Pointer; bufferSize: NativeUInt): Integer;
begin
  Result := PCefZipReader(FData)^.read_file(PCefZipReader(FData), buffer, buffersize);
end;

function TCefZipReaderRef.Tell: Int64;
begin
  Result := PCefZipReader(FData)^.tell(PCefZipReader(FData));
end;

class function TCefZipReaderRef.UnWrap(data: Pointer): ICefZipReader;
begin
  if (data <> nil) then
    Result := Create(data) as ICefZipReader
   else
    Result := nil;
end;

end.
