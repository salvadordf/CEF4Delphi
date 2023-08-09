unit uCEFStreamReader;

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
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefStreamReaderRef = class(TCefBaseRefCountedRef, ICefStreamReader)
  protected
    function Read(ptr: Pointer; size, n: NativeUInt): NativeUInt;
    function Seek(offset: Int64; whence: Integer): Integer;
    function Tell: Int64;
    function Eof: Boolean;
    function MayBlock: Boolean;
  public
    class function UnWrap(data: Pointer): ICefStreamReader;
    class function CreateForFile(const filename: ustring): ICefStreamReader;
    class function CreateForCustomStream(const stream: ICefCustomStreamReader): ICefStreamReader;
    class function CreateForStream(const stream: TSTream; owned: Boolean): ICefStreamReader;
    class function CreateForData(data: Pointer; size: NativeUInt): ICefStreamReader;
    class function CreateForHandler(const handler: ICefReadHandler): ICefStreamReader;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFCustomStreamReader;

class function TCefStreamReaderRef.CreateForCustomStream(const stream: ICefCustomStreamReader): ICefStreamReader;
begin
  Result := UnWrap(cef_stream_reader_create_for_handler(CefGetData(stream)))
end;

class function TCefStreamReaderRef.CreateForData(data: Pointer; size: NativeUInt): ICefStreamReader;
begin
  Result := UnWrap(cef_stream_reader_create_for_data(data, size))
end;

class function TCefStreamReaderRef.CreateForHandler(const handler: ICefReadHandler): ICefStreamReader;
begin
  Result := UnWrap(cef_stream_reader_create_for_handler(CefGetData(handler)));
end;

class function TCefStreamReaderRef.CreateForFile(const filename: ustring): ICefStreamReader;
var
  TempFileName : TCefString;
begin
  TempFileName := CefString(filename);
  Result       := UnWrap(cef_stream_reader_create_for_file(@TempFileName))
end;

class function TCefStreamReaderRef.CreateForStream(const stream: TSTream; owned: Boolean): ICefStreamReader;
begin
  Result := CreateForCustomStream(TCefCustomStreamReader.Create(stream, owned) as ICefCustomStreamReader);
end;

function TCefStreamReaderRef.Eof: Boolean;
begin
  Result := PCefStreamReader(FData)^.eof(PCefStreamReader(FData)) <> 0;
end;

function TCefStreamReaderRef.MayBlock: Boolean;
begin
  Result := PCefStreamReader(FData)^.may_block(PCefStreamReader(FData)) <> 0;
end;

function TCefStreamReaderRef.Read(ptr: Pointer; size, n: NativeUInt): NativeUInt;
begin
  Result := PCefStreamReader(FData)^.read(PCefStreamReader(FData), ptr, size, n);
end;

function TCefStreamReaderRef.Seek(offset: Int64; whence: Integer): Integer;
begin
  Result := PCefStreamReader(FData)^.seek(PCefStreamReader(FData), offset, whence);
end;

function TCefStreamReaderRef.Tell: Int64;
begin
  Result := PCefStreamReader(FData)^.tell(PCefStreamReader(FData));
end;

class function TCefStreamReaderRef.UnWrap(data: Pointer): ICefStreamReader;
begin
  if (data <> nil) then
    Result := Create(data) as ICefStreamReader
   else
    Result := nil;
end;

end.
