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

unit uCEFResponse;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefResponseRef = class(TCefBaseRefCountedRef, ICefResponse)
    protected
      function  IsReadOnly: Boolean;
      function  GetError: TCefErrorCode;
      procedure SetError(error: TCefErrorCode);
      function  GetStatus: Integer;
      procedure SetStatus(status: Integer);
      function  GetStatusText: ustring;
      procedure SetStatusText(const StatusText: ustring);
      function  GetMimeType: ustring;
      procedure SetMimeType(const mimetype: ustring);
      function  GetCharset: ustring;
      procedure SetCharset(const charset: ustring);
      function  GetHeaderByName(const name: ustring): ustring;
      procedure SetHeaderByName(const name, value: ustring; overwrite: boolean);
      procedure GetHeaderMap(const headerMap: ICefStringMultimap);
      procedure SetHeaderMap(const headerMap: ICefStringMultimap);
      function  GetURL: ustring;
      procedure SetURL(const url: ustring);

    public
      class function UnWrap(data: Pointer): ICefResponse;
      class function New: ICefResponse;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;


class function TCefResponseRef.New: ICefResponse;
begin
  Result := UnWrap(cef_response_create());
end;

function TCefResponseRef.GetError: TCefErrorCode;
begin
  Result := PCefResponse(FData)^.get_error(FData);
end;

function TCefResponseRef.GetHeaderByName(const name: ustring): ustring;
var
  TempName : TCefString;
begin
  TempName := CefString(name);
  Result   := CefStringFreeAndGet(PCefResponse(FData)^.get_header_by_name(PCefResponse(FData), @TempName));
end;

procedure TCefResponseRef.SetHeaderByName(const name, value: ustring; overwrite: boolean);
var
  TempName, TempValue : TCefString;
begin
  TempName  := CefString(name);
  TempValue := CefString(value);
  PCefResponse(FData)^.set_header_by_name(PCefResponse(FData), @TempName, @TempValue, ord(overwrite));
end;

procedure TCefResponseRef.GetHeaderMap(const headerMap: ICefStringMultimap);
begin
  PCefResponse(FData)^.get_header_map(PCefResponse(FData), headermap.Handle);
end;

function TCefResponseRef.GetMimeType: ustring;
begin
  Result := CefStringFreeAndGet(PCefResponse(FData)^.get_mime_type(PCefResponse(FData)));
end;

function TCefResponseRef.GetStatus: Integer;
begin
  Result := PCefResponse(FData)^.get_status(PCefResponse(FData));
end;

function TCefResponseRef.GetStatusText: ustring;
begin
  Result := CefStringFreeAndGet(PCefResponse(FData)^.get_status_text(PCefResponse(FData)));
end;

function TCefResponseRef.IsReadOnly: Boolean;
begin
  Result := PCefResponse(FData)^.is_read_only(PCefResponse(FData)) <> 0;
end;

procedure TCefResponseRef.SetError(error: TCefErrorCode);
begin
  PCefResponse(FData)^.set_error(FData, error);
end;

procedure TCefResponseRef.SetHeaderMap(const headerMap: ICefStringMultimap);
begin
  PCefResponse(FData)^.set_header_map(PCefResponse(FData), headerMap.Handle);
end;

procedure TCefResponseRef.SetMimeType(const mimetype: ustring);
var
  TempType : TCefString;
begin
  TempType := CefString(mimetype);
  PCefResponse(FData)^.set_mime_type(PCefResponse(FData), @TempType);
end;

function TCefResponseRef.GetCharset: ustring;
begin
  Result := CefStringFreeAndGet(PCefResponse(FData)^.get_charset(PCefResponse(FData)));
end;

procedure TCefResponseRef.SetCharset(const charset: ustring);
var
  TempCharset : TCefString;
begin
  TempCharset := CefString(charset);
  PCefResponse(FData)^.set_charset(PCefResponse(FData), @TempCharset);
end;

procedure TCefResponseRef.SetStatus(status: Integer);
begin
  PCefResponse(FData)^.set_status(PCefResponse(FData), status);
end;

procedure TCefResponseRef.SetStatusText(const StatusText: ustring);
var
  TempStatus : TCefString;
begin
  TempStatus := CefString(StatusText);
  PCefResponse(FData)^.set_status_text(PCefResponse(FData), @TempStatus);
end;

function TCefResponseRef.GetURL : ustring;
begin
  Result := CefStringFreeAndGet(PCefResponse(FData)^.get_url(PCefResponse(FData)));
end;

procedure TCefResponseRef.SetURL(const url : ustring);
var
  TempURL : TCefString;
begin
  TempURL := CefString(url);
  PCefResponse(FData)^.set_url(PCefResponse(FData), @TempURL);
end;

class function TCefResponseRef.UnWrap(data: Pointer): ICefResponse;
begin
  if (data <> nil) then
    Result := Create(data) as ICefResponse
   else
    Result := nil;
end;

end.
