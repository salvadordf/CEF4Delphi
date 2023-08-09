unit uCEFStreamWriter;

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
