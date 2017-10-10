// ************************************************************************
// ***************************** CEF4Delphi *******************************
// ************************************************************************
//
// CEF4Delphi is based on DCEF3 which uses CEF3 to embed a chromium-based
// browser in Delphi applications.
//
// The original license of DCEF3 still applies to CEF4Delphi.
//
// For more information about CEF4Delphi visit :
//         https://www.briskbard.com/index.php?lang=en&pageid=cef
//
//        Copyright © 2017 Salvador Díaz Fau. All rights reserved.
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

unit uCEFRequest;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefRequestRef = class(TCefBaseRefCountedRef, ICefRequest)
  protected
    function IsReadOnly: Boolean;
    function GetUrl: ustring;
    function GetMethod: ustring;
    function GetPostData: ICefPostData;
    procedure GetHeaderMap(const HeaderMap: ICefStringMultimap);
    procedure SetUrl(const value: ustring);
    procedure SetMethod(const value: ustring);
    procedure SetReferrer(const referrerUrl: ustring; policy: TCefReferrerPolicy);
    function GetReferrerUrl: ustring;
    function GetReferrerPolicy: TCefReferrerPolicy;
    procedure SetPostData(const value: ICefPostData);
    procedure SetHeaderMap(const HeaderMap: ICefStringMultimap);
    function GetFlags: TCefUrlRequestFlags;
    procedure SetFlags(flags: TCefUrlRequestFlags);
    function GetFirstPartyForCookies: ustring;
    procedure SetFirstPartyForCookies(const url: ustring);
    procedure Assign(const url, method: ustring; const postData: ICefPostData; const headerMap: ICefStringMultimap);
    function GetResourceType: TCefResourceType;
    function GetTransitionType: TCefTransitionType;
    function GetIdentifier: UInt64;
  public
    class function UnWrap(data: Pointer): ICefRequest;
    class function New: ICefRequest;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFPostData;

function TCefRequestRef.IsReadOnly: Boolean;
begin
  Result := PCefRequest(FData).is_read_only(PCefRequest(FData)) <> 0;
end;

procedure TCefRequestRef.Assign(const url, method: ustring; const postData: ICefPostData; const headerMap: ICefStringMultimap);
var
  u, m: TCefString;
begin
  u := cefstring(url);
  m := cefstring(method);
  PCefRequest(FData).set_(PCefRequest(FData), @u, @m, CefGetData(postData), headerMap.Handle);
end;

function TCefRequestRef.GetFirstPartyForCookies: ustring;
begin
  Result := CefStringFreeAndGet(PCefRequest(FData).get_first_party_for_cookies(PCefRequest(FData)));
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
  Result := PCefRequest(FData).get_resource_type(FData);
end;

function TCefRequestRef.GetTransitionType: TCefTransitionType;
begin
    Result := PCefRequest(FData).get_transition_type(FData);
end;

function TCefRequestRef.GetUrl: ustring;
begin
  Result := CefStringFreeAndGet(PCefRequest(FData)^.get_url(PCefRequest(FData)))
end;

class function TCefRequestRef.New: ICefRequest;
begin
  Result := UnWrap(cef_request_create);
end;

procedure TCefRequestRef.SetFirstPartyForCookies(const url: ustring);
var
  str: TCefString;
begin
  str := CefString(url);
  PCefRequest(FData).set_first_party_for_cookies(PCefRequest(FData), @str);
end;

procedure TCefRequestRef.SetFlags(flags: TCefUrlRequestFlags);
begin
  PCefRequest(FData)^.set_flags(PCefRequest(FData), PByte(@flags)^);
end;

procedure TCefRequestRef.SetHeaderMap(const HeaderMap: ICefStringMultimap);
begin
  PCefRequest(FData)^.set_header_map(PCefRequest(FData), HeaderMap.Handle);
end;

procedure TCefRequestRef.SetMethod(const value: ustring);
var
  v: TCefString;
begin
  v := CefString(value);
  PCefRequest(FData)^.set_method(PCefRequest(FData), @v);
end;

procedure TCefRequestRef.SetReferrer(const referrerUrl: ustring; policy: TCefReferrerPolicy);
var
  u: TCefString;
begin
  u := CefString(referrerUrl);
  PCefRequest(FData)^.set_referrer(PCefRequest(FData), @u, policy);
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
  if value <> nil then
    PCefRequest(FData)^.set_post_data(PCefRequest(FData), CefGetData(value));
end;

procedure TCefRequestRef.SetUrl(const value: ustring);
var
  v: TCefString;
begin
  v := CefString(value);
  PCefRequest(FData)^.set_url(PCefRequest(FData), @v);
end;

class function TCefRequestRef.UnWrap(data: Pointer): ICefRequest;
begin
  if data <> nil then
    Result := Create(data) as ICefRequest else
    Result := nil;
end;


end.
