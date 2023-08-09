unit uCEFUrlRequest;

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
  TCefUrlRequestRef = class(TCefBaseRefCountedRef, ICefUrlRequest)
  protected
    function  GetRequest: ICefRequest;
    function  GetClient: ICefUrlrequestClient;
    function  GetRequestStatus: TCefUrlRequestStatus;
    function  GetRequestError: Integer;
    function  GetResponse: ICefResponse;
    function  GetResponseWasCached: boolean;
    procedure Cancel;

  public
    class function UnWrap(data: Pointer): ICefUrlRequest;
    class function New(const request: ICefRequest; const client: ICefUrlRequestClient; const requestContext: ICefRequestContext): ICefUrlRequest;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFRequest, uCEFResponse, uCEFUrlrequestClient;

procedure TCefUrlRequestRef.Cancel;
begin
  PCefUrlRequest(FData)^.cancel(PCefUrlRequest(FData));
end;

class function TCefUrlRequestRef.New(const request        : ICefRequest;
                                     const client         : ICefUrlRequestClient;
                                     const requestContext : ICefRequestContext): ICefUrlRequest;
begin
  Result := UnWrap(cef_urlrequest_create(CefGetData(request), CefGetData(client), CefGetData(requestContext)));
end;

function TCefUrlRequestRef.GetRequest: ICefRequest;
begin
  Result := TCefRequestRef.UnWrap(PCefUrlRequest(FData)^.get_request(PCefUrlRequest(FData)));
end;

function TCefUrlRequestRef.GetClient: ICefUrlrequestClient;
begin
  Result := TCefUrlrequestClientRef.UnWrap(PCefUrlRequest(FData)^.get_client(PCefUrlRequest(FData)));
end;

function TCefUrlRequestRef.GetRequestError: Integer;
begin
  Result := PCefUrlRequest(FData)^.get_request_error(PCefUrlRequest(FData));
end;

function TCefUrlRequestRef.GetRequestStatus: TCefUrlRequestStatus;
begin
  Result := PCefUrlRequest(FData)^.get_request_status(PCefUrlRequest(FData));
end;

function TCefUrlRequestRef.GetResponseWasCached: boolean;
begin
  Result := PCefUrlRequest(FData)^.response_was_cached(PCefUrlRequest(FData)) <> 0;
end;

function TCefUrlRequestRef.GetResponse: ICefResponse;
begin
  Result := TCefResponseRef.UnWrap(PCefUrlRequest(FData)^.get_response(PCefUrlRequest(FData)));
end;

class function TCefUrlRequestRef.UnWrap(data: Pointer): ICefUrlRequest;
begin
  if (data <> nil) then
    Result := Create(data) as ICefUrlRequest
   else
    Result := nil;
end;

end.
