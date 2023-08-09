unit uCEFCustomStreamReader;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes, System.SysUtils,
  {$ELSE}
  Classes, SysUtils,
  {$ENDIF}
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefCustomStreamReader = class(TCefBaseRefCountedOwn, ICefCustomStreamReader)
    protected
      FStream : TStream;
      FOwned  : Boolean;

      function Read(ptr: Pointer; size, n: NativeUInt): NativeUInt; virtual;
      function Seek(offset: Int64; whence: Integer): Integer; virtual;
      function Tell: Int64; virtual;
      function Eof: Boolean; virtual;
      function MayBlock: Boolean; virtual;

    public
      constructor Create(Stream: TStream; Owned: Boolean); overload; virtual;
      constructor Create(const filename: string); overload; virtual;
      destructor  Destroy; override;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;

function cef_stream_reader_read(self: PCefReadHandler; ptr: Pointer; size, n: NativeUInt): NativeUInt; stdcall;
var
  TempObject  : TObject;
begin
  Result      := 0;
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefCustomStreamReader) then
    Result := TCefCustomStreamReader(TempObject).Read(ptr, size, n);
end;

function cef_stream_reader_seek(self: PCefReadHandler; offset: Int64; whence: Integer): Integer; stdcall;
var
  TempObject  : TObject;
begin
  Result      := 0;
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefCustomStreamReader) then
    Result := TCefCustomStreamReader(TempObject).Seek(offset, whence);
end;

function cef_stream_reader_tell(self: PCefReadHandler): Int64; stdcall;
var
  TempObject  : TObject;
begin
  Result      := 0;
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefCustomStreamReader) then
    Result := TCefCustomStreamReader(TempObject).Tell;
end;

function cef_stream_reader_eof(self: PCefReadHandler): Integer; stdcall;
var
  TempObject  : TObject;
begin
  Result      := Ord(True);
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefCustomStreamReader) then
    Result := Ord(TCefCustomStreamReader(TempObject).Eof);
end;

function cef_stream_reader_may_block(self: PCefReadHandler): Integer; stdcall;
var
  TempObject  : TObject;
begin
  Result      := Ord(False);
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefCustomStreamReader) then
    Result := Ord(TCefCustomStreamReader(TempObject).MayBlock);
end;


constructor TCefCustomStreamReader.Create(Stream: TStream; Owned: Boolean);
begin
  inherited CreateData(SizeOf(TCefReadHandler));

  FStream := stream;
  FOwned  := Owned;

  with PCefReadHandler(FData)^ do
    begin
      read      := {$IFDEF FPC}@{$ENDIF}cef_stream_reader_read;
      seek      := {$IFDEF FPC}@{$ENDIF}cef_stream_reader_seek;
      tell      := {$IFDEF FPC}@{$ENDIF}cef_stream_reader_tell;
      eof       := {$IFDEF FPC}@{$ENDIF}cef_stream_reader_eof;
      may_block := {$IFDEF FPC}@{$ENDIF}cef_stream_reader_may_block;
    end;
end;

constructor TCefCustomStreamReader.Create(const filename: string);
begin
  Create(TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite), True);
end;

destructor TCefCustomStreamReader.Destroy;
begin
  if FOwned then FStream.Free;

  inherited Destroy;
end;

function TCefCustomStreamReader.Eof: Boolean;
begin
  Result := FStream.Position = FStream.size;
end;

function TCefCustomStreamReader.MayBlock: Boolean;
begin
  Result := False;
end;

function TCefCustomStreamReader.Read(ptr: Pointer; size, n: NativeUInt): NativeUInt;
begin
  Result := NativeUInt(FStream.Read(ptr^, n * size)) div size;
end;

function TCefCustomStreamReader.Seek(offset: Int64; whence: Integer): Integer;
begin
  Result := FStream.Seek(offset, TSeekOrigin(whence));
end;

function TCefCustomStreamReader.Tell: Int64;
begin
  Result := FStream.Position;
end;

end.
