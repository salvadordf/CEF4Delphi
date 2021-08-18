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

unit uHelloScheme;

{$I cef.inc}

interface
uses
  Classes, Windows, SysUtils,
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
  TempFilename, TempExt : string;
  TempParts : TUrlParts;
  TempFile : TFileStream;
begin
  Result      := False;
  FStatus     := 404;
  FStatusText := 'ERROR';
  FMimeType   := '';
  TempFile    := nil;

  try
    try
      if (FStream <> nil) and (request <> nil) then
        begin
          TempFilename := '';
          FStream.Clear;

          if CefParseUrl(Request.URL, TempParts) then
            begin
              if (length(TempParts.path) > 0) and
                 (TempParts.path <> '/') then
                begin
                  TempFilename := TempParts.path;

                  if (length(TempFilename) > 0) and (TempFilename[1] = '/') then
                    TempFilename := copy(TempFilename, 2, length(TempFilename));

                  if (length(TempFilename) > 0) and (TempFilename[length(TempFilename)] = '/') then
                    TempFilename := copy(TempFilename, 1, length(TempFilename) - 1);

                  if (length(TempFilename) > 0) and not(FileExists(TempFilename)) then
                    TempFilename := '';
                end;

              if (length(TempFilename) = 0) and
                 (length(TempParts.host) > 0) and
                 (TempParts.host <> '/') then
                begin
                  TempFilename := TempParts.host;

                  if (length(TempFilename) > 0) and (TempFilename[1] = '/') then
                    TempFilename := copy(TempFilename, 2, length(TempFilename));

                  if (length(TempFilename) > 0) and (TempFilename[length(TempFilename)] = '/') then
                    TempFilename := copy(TempFilename, 1, length(TempFilename) - 1);

                  if (length(TempFilename) > 0) and not(FileExists(TempFilename)) then
                    TempFilename := '';
                end;
            end;

          if (length(TempFilename) > 0) then
            begin
              TempExt := ExtractFileExt(TempFilename);

              if (length(TempExt) > 0) and (TempExt[1] = '.') then
                TempExt := copy(TempExt, 2, length(TempExt));

              Result      := True;
              FStatus     := 200;
              FStatusText := 'OK';
              FMimeType   := CefGetMimeType(TempExt);
              TempFile    := TFileStream.Create(TempFilename, fmOpenRead);
              TempFile.Seek(0, soFromBeginning);
              FStream.LoadFromStream(TStream(TempFile));
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
