unit uCustomResourceHandler;

{$I ..\..\..\source\cef.inc}

interface
uses
  {$IFDEF DELPHI16_UP}
  System.Classes, WinApi.Windows, System.SysUtils,
  {$ELSE}
  Classes, Windows, SysUtils,
  {$ENDIF}
  uCEFInterfaces, uCEFTypes, uCEFResourceHandler;

type
  TCustomResourceHandler = class(TCefResourceHandlerOwn)
    private
      FStream      : TMemoryStream;
      FMimeType    : string;
      FStatusText  : string;
      FStatus      : Integer;

    protected
      function  ProcessRequest(const request: ICefRequest; const callback: ICefCallback): Boolean; override;
      function  ReadResponse(const dataOut: Pointer; bytesToRead: Integer; var bytesRead: Integer; const callback: ICefCallback): Boolean; override;
      procedure GetResponseHeaders(const response: ICefResponse; out responseLength: Int64; out redirectUrl: ustring); override;
      //function  open(const request: ICefRequest; var handle_request: boolean; const callback: ICefCallback): boolean; override;
      //function  read(const data_out: Pointer; bytes_to_read: Integer; var bytes_read: Integer; const callback: ICefResourceReadCallback): boolean; override;

    public
      constructor Create(const browser: ICefBrowser; const frame: ICefFrame; const schemeName: ustring; const request: ICefRequest; const aStream : TStream; const aMimeType : ustring); reintroduce;
      destructor  Destroy; override;
  end;

implementation

constructor TCustomResourceHandler.Create(const browser    : ICefBrowser;
                                          const frame      : ICefFrame;
                                          const schemeName : ustring;
                                          const request    : ICefRequest;
                                          const aStream    : TStream;
                                          const aMimeType  : ustring);
begin
  inherited Create(browser, frame, schemeName, request);

  FStream     := TMemoryStream.Create;
  FStatus     := 0;
  FMimeType   := aMimeType;
  FStatusText := '';

  if (FStream <> nil) and (aStream <> nil) then
    begin
      FStream.LoadFromStream(aStream);
      FStream.Seek(0, soFromBeginning);
    end;
end;

destructor TCustomResourceHandler.Destroy;
begin
  if (FStream <> nil) then FreeAndNil(FStream);

  inherited Destroy;
end;

procedure TCustomResourceHandler.GetResponseHeaders(const response       : ICefResponse;
                                                    out   responseLength : Int64;
                                                    out   redirectUrl    : ustring);
begin
  if (response <> nil) then
    begin
      response.Status     := FStatus;
      response.StatusText := FStatusText;
      response.MimeType   := FMimeType;
    end;

  if (FStream <> nil) then
    responseLength := FStream.Size
   else
    responseLength := 0;
end;
        {
function TCustomResourceHandler.open(const request        : ICefRequest;
                                     var   handle_request : boolean;
                                     const callback       : ICefCallback): boolean;
begin
  Result         := True;
  handle_request := True;
  FStatus        := 200;
  FStatusText    := 'OK';

  if (FStream  <> nil) then FStream.Seek(0, soFromBeginning);
end;

function TCustomResourceHandler.read(const data_out      : Pointer;
                                           bytes_to_read : Integer;
                                     var   bytes_read    : Integer;
                                     const callback      : ICefResourceReadCallback): boolean;
begin
  if (FStream <> nil) and (data_out <> nil) then
    begin
      bytes_read := FStream.Read(data_out^, bytes_to_read);
      Result     := True;
    end
   else
    Result := False;
end;   }

function TCustomResourceHandler.ProcessRequest(const request : ICefRequest; const callback : ICefCallback): Boolean;
begin
  Result      := True;
  FStatus     := 200;
  FStatusText := 'OK';

  if (FStream  <> nil) then FStream.Seek(0, soFromBeginning);
  if (callback <> nil) then callback.Cont;
end;

function TCustomResourceHandler.ReadResponse(const dataOut     : Pointer;
                                                   bytesToRead : Integer;
                                             var   bytesRead   : Integer;
                                             const callback    : ICefCallback): Boolean;
begin
  if (FStream <> nil) and (DataOut <> nil) then
    begin
      BytesRead := FStream.Read(DataOut^, BytesToRead);
      Result    := (BytesRead > 0);
    end
   else
    Result := False;
end;

end.
