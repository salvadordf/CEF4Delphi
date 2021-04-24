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

unit uCustomResourceHandler;

{$I cef.inc}

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
      procedure GetResponseHeaders(const response: ICefResponse; out responseLength: Int64; out redirectUrl: ustring); override;
      function  ReadResponse(const dataOut: Pointer; bytesToRead: Integer; var bytesRead: Integer; const callback: ICefCallback): Boolean; override;

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
