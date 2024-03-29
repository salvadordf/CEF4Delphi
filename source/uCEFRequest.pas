unit uCEFRequest;

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
  TCefRequestRef = class(TCefBaseRefCountedRef, ICefRequest)
  protected
    function  IsReadOnly: Boolean;
    function  GetUrl: ustring;
    function  GetMethod: ustring;
    function  GetPostData: ICefPostData;
    procedure GetHeaderMap(const HeaderMap: ICefStringMultimap);
    procedure SetUrl(const value: ustring);
    procedure SetMethod(const value: ustring);
    procedure SetReferrer(const referrerUrl: ustring; policy: TCefReferrerPolicy);
    function  GetReferrerUrl: ustring;
    function  GetReferrerPolicy: TCefReferrerPolicy;
    procedure SetPostData(const value: ICefPostData);
    procedure SetHeaderMap(const HeaderMap: ICefStringMultimap);
    function  GetHeaderByName(const name: ustring): ustring;
    procedure SetHeaderByName(const name, value: ustring; overwrite: boolean);
    function  GetFlags: TCefUrlRequestFlags;
    procedure SetFlags(flags: TCefUrlRequestFlags);
    function  GetFirstPartyForCookies: ustring;
    procedure SetFirstPartyForCookies(const url: ustring);
    procedure Assign(const url, method: ustring; const postData: ICefPostData; const headerMap: ICefStringMultimap);
    function  GetResourceType: TCefResourceType;
    function  GetTransitionType: TCefTransitionType;
    function  GetIdentifier: UInt64;
  public
    class function UnWrap(data: Pointer): ICefRequest;
    class function New: ICefRequest;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFPostData;

function TCefRequestRef.IsReadOnly: Boolean;
begin
  Result := PCefRequest(FData)^.is_read_only(PCefRequest(FData)) <> 0;
end;

procedure TCefRequestRef.Assign(const url, method: ustring; const postData: ICefPostData; const headerMap: ICefStringMultimap);
var
  TempURL, TempMethod : TCefString;
begin
  TempURL    := cefstring(url);
  TempMethod := cefstring(method);
  PCefRequest(FData)^.set_(PCefRequest(FData), @TempURL, @TempMethod, CefGetData(postData), headerMap.Handle);
end;

function TCefRequestRef.GetFirstPartyForCookies: ustring;
begin
  Result := CefStringFreeAndGet(PCefRequest(FData)^.get_first_party_for_cookies(PCefRequest(FData)));
end;

function TCefRequestRef.GetFlags: TCefUrlRequestFlags;
begin
  Result := PCefRequest(FData)^.get_flags(PCefRequest(FData));
end;

procedure TCefRequestRef.GetHeaderMap(const HeaderMap: ICefStringMultimap);
begin
  PCefRequest(FData)^.get_header_map(PCefRequest(FData), HeaderMap.Handle);
end;

function TCefRequestRef.GetIdentifier: UInt64;
begin
  Result := PCefRequest(FData)^.get_identifier(PCefRequest(FData));
end;

function TCefRequestRef.GetMethod: ustring;
begin
  Result := CefStringFreeAndGet(PCefRequest(FData)^.get_method(PCefRequest(FData)))
end;

function TCefRequestRef.GetPostData: ICefPostData;
begin
  Result := TCefPostDataRef.UnWrap(PCefRequest(FData)^.get_post_data(PCefRequest(FData)));
end;

function TCefRequestRef.GetResourceType: TCefResourceType;
begin
  Result := PCefRequest(FData)^.get_resource_type(FData);
end;

function TCefRequestRef.GetTransitionType: TCefTransitionType;
begin
  Result := PCefRequest(FData)^.get_transition_type(FData);
end;

function TCefRequestRef.GetUrl: ustring;
begin
  Result := CefStringFreeAndGet(PCefRequest(FData)^.get_url(PCefRequest(FData)))
end;

class function TCefRequestRef.New: ICefRequest;
begin
  Result := UnWrap(cef_request_create());
end;

procedure TCefRequestRef.SetFirstPartyForCookies(const url: ustring);
var
  TempURL : TCefString;
begin
  TempURL := CefString(url);
  PCefRequest(FData)^.set_first_party_for_cookies(PCefRequest(FData), @TempURL);
end;

procedure TCefRequestRef.SetFlags(flags: TCefUrlRequestFlags);
begin
  PCefRequest(FData)^.set_flags(PCefRequest(FData), PByte(@flags)^);
end;

procedure TCefRequestRef.SetHeaderMap(const HeaderMap: ICefStringMultimap);
begin
  PCefRequest(FData)^.set_header_map(PCefRequest(FData), HeaderMap.Handle);
end;

function TCefRequestRef.GetHeaderByName(const name: ustring): ustring;
var
  TempName : TCefString;
begin
  TempName := CefString(name);
  Result   := CefStringFreeAndGet(PCefRequest(FData)^.get_header_by_name(PCefRequest(FData), @TempName));
end;

procedure TCefRequestRef.SetHeaderByName(const name, value: ustring; overwrite: boolean);
var
  TempName, TempValue : TCefString;
begin
  TempName  := CefString(name);
  TempValue := CefString(value);
  PCefRequest(FData)^.set_header_by_name(PCefRequest(FData), @TempName, @TempValue, ord(overwrite));
end;

procedure TCefRequestRef.SetMethod(const value: ustring);
var
  TempValue : TCefString;
begin
  TempValue := CefString(value);
  PCefRequest(FData)^.set_method(PCefRequest(FData), @TempValue);
end;

procedure TCefRequestRef.SetReferrer(const referrerUrl: ustring; policy: TCefReferrerPolicy);
var
  TempURL : TCefString;
begin
  TempURL := CefString(referrerUrl);
  PCefRequest(FData)^.set_referrer(PCefRequest(FData), @TempURL, policy);
end;

function TCefRequestRef.GetReferrerUrl: ustring;
begin
  Result := CefStringFreeAndGet(PCefRequest(FData)^.get_referrer_url(PCefRequest(FData)));
end;

function TCefRequestRef.GetReferrerPolicy: TCefReferrerPolicy;
begin
  Result := PCefRequest(FData)^.get_referrer_policy(PCefRequest(FData));
end;

procedure TCefRequestRef.SetPostData(const value: ICefPostData);
begin
  if (value <> nil) then
    PCefRequest(FData)^.set_post_data(PCefRequest(FData), CefGetData(value));
end;

procedure TCefRequestRef.SetUrl(const value: ustring);
var
  TempURL : TCefString;
begin
  TempURL := CefString(value);
  PCefRequest(FData)^.set_url(PCefRequest(FData), @TempURL);
end;

class function TCefRequestRef.UnWrap(data: Pointer): ICefRequest;
begin
  if (data <> nil) then
    Result := Create(data) as ICefRequest
   else
    Result := nil;
end;


end.
