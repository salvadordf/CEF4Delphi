unit uHelloScheme;

{$I ..\..\..\source\cef.inc}

interface
uses
  {$IFDEF DELPHI16_UP}
  System.Classes, WinApi.Windows, System.SysUtils,
  {$ELSE}
  Classes, Windows, SysUtils,
  {$ENDIF}
  uCEFInterfaces, uCEFTypes, uCEFResourceHandler, uCEFMiscFunctions;

type
  THelloScheme = class(TCefResourceHandlerOwn)
    private
      FStream      : TMemoryStream;
      FMimeType    : string;
      FStatusText  : string;
      FStatus      : Integer;

    protected
      function  ProcessRequest(const request: ICefRequest; const callback: ICefCallback): Boolean; override;
      procedure GetResponseHeaders(const response: ICefResponse; out responseLength: Int64; out redirectUrl: ustring); override;
      function  ReadResponse(const dataOut: Pointer; bytesToRead: Integer; var bytesRead: Integer; const callback: ICefCallback): Boolean; override;

    public
      constructor Create(const browser: ICefBrowser; const frame: ICefFrame; const schemeName: ustring; const request: ICefRequest); override;
      destructor  Destroy; override;
      procedure   AfterConstruction; override;
  end;

implementation

uses
  uCEFConstants;

constructor THelloScheme.Create(const browser    : ICefBrowser;
                                const frame      : ICefFrame;
                                const schemeName : ustring;
                                const request    : ICefRequest);
begin
  inherited Create(browser, frame, schemeName, request);

  FStream     := nil;
  FStatus     := 0;
  FMimeType   := '';
  FStatusText := '';
end;

destructor THelloScheme.Destroy;
begin
  if (FStream <> nil) then FreeAndNil(FStream);

  inherited Destroy;
end;

procedure THelloScheme.AfterConstruction;
begin
  inherited AfterConstruction;

  FStream := TMemoryStream.Create;
end;

procedure THelloScheme.GetResponseHeaders(const response       : ICefResponse;
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

function THelloScheme.ProcessRequest(const request : ICefRequest; const callback : ICefCallback): Boolean;
var
  TempFilename, TempExt, TempMessageTxt : string;
  TempParts : TUrlParts;
  TempFile : TFileStream;
  TempResp : TStringStream;
begin
  Result      := False;
  FStatus     := 404;
  FStatusText := 'ERROR';
  FMimeType   := '';
  TempFile    := nil;
  TempResp    := nil;

  try
    try
      if (FStream <> nil) and (request <> nil) then
        begin
          TempFilename := '';
          FStream.Clear;

          if CefParseUrl(Request.URL, TempParts) and
             (length(TempParts.path) > 0) and
             (TempParts.path <> '/') then
            begin
              TempFilename := TempParts.path;

              if (length(TempFilename) > 0) and (TempFilename[1] = '/') then
                TempFilename := copy(TempFilename, 2, length(TempFilename));

              if (length(TempFilename) > 0) and (TempFilename[length(TempFilename)] = '/') then
                TempFilename := copy(TempFilename, 1, length(TempFilename) - 1);
            end;

          if (length(TempFilename) > 0) then
            begin
              if (CompareText(TempFilename, 'customrequest') = 0) then
                begin
                  Result      := True;
                  FStatus     := 200;
                  FStatusText := 'OK';

                  // This could be any information that your application needs to send to JS.
                  TempMessageTxt := 'This is the response from Delphi!' + CRLF + CRLF +
                                    'Request query : ' + TempParts.query;

                  TempResp := TStringStream.Create(TempMessageTxt);
                  TempResp.Seek(0, soFromBeginning);
                  FStream.LoadFromStream(TStream(TempResp));
                end
               else
                if FileExists(TempFilename) then
                  begin
                    TempExt := ExtractFileExt(TempFilename);

                    if (length(TempExt) > 0) and (TempExt[1] = '.') then
                      TempExt := copy(TempExt, 2, length(TempExt));

                    FMimeType   := CefGetMimeType(TempExt);
                    Result      := True;
                    FStatus     := 200;
                    FStatusText := 'OK';
                    TempFile    := TFileStream.Create(TempFilename, fmOpenRead);
                    TempFile.Seek(0, soFromBeginning);
                    FStream.LoadFromStream(TStream(TempFile));
                  end;
            end;

          FStream.Seek(0, soFromBeginning);
        end;
    except
      on e : exception do
        if CustomExceptionHandler('THelloScheme.ProcessRequest', e) then raise;
    end;
  finally
    if (callback <> nil) then callback.Cont;
    if (TempFile <> nil) then FreeAndNil(TempFile);
    if (TempResp <> nil) then FreeAndNil(TempResp);
  end;
end;

function THelloScheme.ReadResponse(const dataOut     : Pointer;
                                         bytesToRead : Integer;
                                   var   bytesRead   : Integer;
                                   const callback    : ICefCallback): Boolean;
begin
  if (FStream <> nil) and (DataOut <> nil) then
    begin
      // This function will be called several times because the stream is bigger
      // than bytesToRead. Each time we will copy a chunk of the stream to
      // DataOut.
      BytesRead := FStream.Read(DataOut^, BytesToRead);
      Result    := (BytesRead > 0);
    end
   else
    Result := False;
end;

end.
