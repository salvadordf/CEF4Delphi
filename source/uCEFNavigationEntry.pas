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

unit uCEFNavigationEntry;

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
  TCefNavigationEntryRef = class(TCefBaseRefCountedRef, ICefNavigationEntry)
    protected
      function IsValid: Boolean;
      function GetUrl: ustring;
      function GetDisplayUrl: ustring;
      function GetOriginalUrl: ustring;
      function GetTitle: ustring;
      function GetTransitionType: TCefTransitionType;
      function HasPostData: Boolean;
      function GetCompletionTime: TDateTime;
      function GetHttpStatusCode: Integer;
      function GetSSLStatus: ICefSSLStatus;

    public
      class function UnWrap(data: Pointer): ICefNavigationEntry;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFSSLStatus;

function TCefNavigationEntryRef.IsValid: Boolean;
begin
  Result := PCefNavigationEntry(FData)^.is_valid(FData) <> 0;
end;

function TCefNavigationEntryRef.GetUrl: ustring;
begin
  Result := CefStringFreeAndGet(PCefNavigationEntry(FData)^.get_url(FData));
end;

function TCefNavigationEntryRef.GetDisplayUrl: ustring;
begin
  Result := CefStringFreeAndGet(PCefNavigationEntry(FData)^.get_display_url(FData));
end;

function TCefNavigationEntryRef.GetOriginalUrl: ustring;
begin
  Result := CefStringFreeAndGet(PCefNavigationEntry(FData)^.get_original_url(FData));
end;

function TCefNavigationEntryRef.GetTitle: ustring;
begin
  Result := CefStringFreeAndGet(PCefNavigationEntry(FData)^.get_title(FData));
end;

function TCefNavigationEntryRef.GetTransitionType: TCefTransitionType;
begin
  Result := PCefNavigationEntry(FData)^.get_transition_type(FData);
end;

function TCefNavigationEntryRef.HasPostData: Boolean;
begin
  Result := PCefNavigationEntry(FData)^.has_post_data(FData) <> 0;
end;

function TCefNavigationEntryRef.GetCompletionTime: TDateTime;
begin
  Result := CefTimeToDateTime(PCefNavigationEntry(FData)^.get_completion_time(FData));
end;

function TCefNavigationEntryRef.GetHttpStatusCode: Integer;
begin
  Result := PCefNavigationEntry(FData)^.get_http_status_code(FData);
end;

function TCefNavigationEntryRef.GetSSLStatus: ICefSSLStatus;
begin
  Result := TCefSSLStatusRef.UnWrap(PCefNavigationEntry(FData)^.get_sslstatus(FData));
end;

class function TCefNavigationEntryRef.UnWrap(data: Pointer): ICefNavigationEntry;
begin
  if (data <> nil) then
    Result := Create(data) as ICefNavigationEntry
   else
    Result := nil;
end;

end.
