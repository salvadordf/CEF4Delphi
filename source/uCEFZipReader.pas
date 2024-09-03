unit uCEFZipReader;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  /// <summary>
  /// Class that supports the reading of zip archives via the zlib unzip API.
  /// The functions of this interface should only be called on the thread that
  /// creates the object.
  /// </summary>
  /// <remarks>
  /// <para><see cref="uCEFTypes|TCefZipReader">Implements TCefZipReader</see></para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_zip_reader_capi.h">CEF source file: /include/capi/cef_zip_reader_capi.h (cef_zip_reader_t)</see></para>
  /// </remarks>
  TCefZipReaderRef = class(TCefBaseRefCountedRef, ICefZipReader)
  protected
    /// <summary>
    /// Moves the cursor to the first file in the archive. Returns true (1) if the
    /// cursor position was set successfully.
    /// </summary>
    function MoveToFirstFile: Boolean;
    /// <summary>
    /// Moves the cursor to the next file in the archive. Returns true (1) if the
    /// cursor position was set successfully.
    /// </summary>
    function MoveToNextFile: Boolean;
    /// <summary>
    /// Moves the cursor to the specified file in the archive. If |caseSensitive|
    /// is true (1) then the search will be case sensitive. Returns true (1) if
    /// the cursor position was set successfully.
    /// </summary>
    function MoveToFile(const fileName: ustring; caseSensitive: Boolean): Boolean;
    /// <summary>
    /// Closes the archive. This should be called directly to ensure that cleanup
    /// occurs on the correct thread.
    /// </summary>
    function Close: Boolean;
    /// <summary>
    /// Returns the name of the file.
    /// </summary>
    function GetFileName: ustring;
    /// <summary>
    /// Returns the uncompressed size of the file.
    /// </summary>
    function GetFileSize: Int64;
    /// <summary>
    /// Returns the last modified timestamp for the file.
    /// </summary>
    function GetFileLastModified: TCefBaseTime;
    /// <summary>
    /// Opens the file for reading of uncompressed data. A read password may
    /// optionally be specified.
    /// </summary>
    function OpenFile(const password: ustring): Boolean;
    /// <summary>
    /// Closes the file.
    /// </summary>
    function CloseFile: Boolean;
    /// <summary>
    /// Read uncompressed file contents into the specified buffer. Returns < 0 if
    /// an error occurred, 0 if at the end of file, or the number of bytes read.
    /// </summary>
    function ReadFile(buffer: Pointer; bufferSize: NativeUInt): Integer;
    /// <summary>
    /// Returns the current offset in the uncompressed file contents.
    /// </summary>
    function Tell: Int64;
    /// <summary>
    /// Returns true (1) if at end of the file contents.
    /// </summary>
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

function TCefZipReaderRef.GetFileLastModified: TCefBaseTime;
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
