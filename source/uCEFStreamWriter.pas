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

unit uCEFStreamWriter;

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
  TCefStreamWriterRef = class(TCefBaseRefCountedRef, ICefStreamWriter)
    protected
      function write(const ptr: Pointer; size, n: NativeUInt): NativeUInt;
      function Seek(offset: Int64; whence: Integer): Integer;
      function Tell: Int64;
      function Flush: Integer;
      function MayBlock: Boolean;

    public
      class function UnWrap(data: Pointer): ICefStreamWriter;
      class function CreateForFile(const fileName: ustring): ICefStreamWriter;
      class function CreateForHandler(const handler: ICefWriteHandler): ICefStreamWriter;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;

class function TCefStreamWriterRef.CreateForFile(const fileName: ustring): ICefStreamWriter;
var
  TempFileName : TCefString;
begin
  TempFileName := CefString(fileName);
  Result       := UnWrap(cef_stream_writer_create_for_file(@TempFileName));
end;

class function TCefStreamWriterRef.CreateForHandler(const handler: ICefWriteHandler): ICefStreamWriter;
begin
  Result := UnWrap(cef_stream_writer_create_for_handler(CefGetData(handler)));
end;

function TCefStreamWriterRef.Flush: Integer;
begin
  Result := PCefStreamWriter(FData)^.flush(PCefStreamWriter(FData));
end;

function TCefStreamWriterRef.MayBlock: Boolean;
begin
  Result := PCefStreamWriter(FData)^.may_block(PCefStreamWriter(FData)) <> 0;
end;

function TCefStreamWriterRef.Seek(offset: Int64; whence: Integer): Integer;
begin
  Result := PCefStreamWriter(FData)^.seek(PCefStreamWriter(FData), offset, whence);
end;

function TCefStreamWriterRef.Tell: Int64;
begin
  Result := PCefStreamWriter(FData)^.tell(PCefStreamWriter(FData));
end;

class function TCefStreamWriterRef.UnWrap(data: Pointer): ICefStreamWriter;
begin
  if data <> nil then
    Result := Create(data) as ICefStreamWriter
   else
    Result := nil;
end;

function TCefStreamWriterRef.write(const ptr: Pointer; size, n: NativeUInt): NativeUInt;
begin
  Result := PCefStreamWriter(FData)^.write(PCefStreamWriter(FData), ptr, size, n);
end;

end.
