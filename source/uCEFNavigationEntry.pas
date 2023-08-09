unit uCEFNavigationEntry;

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
  Result := CefBaseTimeToDateTime(PCefNavigationEntry(FData)^.get_completion_time(FData));
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
