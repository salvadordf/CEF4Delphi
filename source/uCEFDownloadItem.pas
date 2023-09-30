unit uCEFDownloadItem;

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
  TCefDownloadItemRef = class(TCefBaseRefCountedRef, ICefDownLoadItem)
  protected
    function IsValid: Boolean;
    function IsInProgress: Boolean;
    function IsComplete: Boolean;
    function IsCanceled: Boolean;
    function IsInterrupted: Boolean;
    function GetInterruptReason: TCefDownloadInterruptReason;
    function GetCurrentSpeed: Int64;
    function GetPercentComplete: Integer;
    function GetTotalBytes: Int64;
    function GetReceivedBytes: Int64;
    function GetStartTime: TDateTime;
    function GetEndTime: TDateTime;
    function GetFullPath: ustring;
    function GetId: Cardinal;
    function GetUrl: ustring;
    function GetOriginalUrl: ustring;
    function GetSuggestedFileName: ustring;
    function GetContentDisposition: ustring;
    function GetMimeType: ustring;
  public
    class function UnWrap(data: Pointer): ICefDownLoadItem;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;

function TCefDownloadItemRef.GetContentDisposition: ustring;
begin
  Result := CefStringFreeAndGet(PCefDownloadItem(FData)^.get_content_disposition(PCefDownloadItem(FData)));
end;

function TCefDownloadItemRef.GetCurrentSpeed: Int64;
begin
  Result := PCefDownloadItem(FData)^.get_current_speed(PCefDownloadItem(FData));
end;

function TCefDownloadItemRef.GetEndTime: TDateTime;
begin
  Result := CefBaseTimeToDateTime(PCefDownloadItem(FData)^.get_end_time(PCefDownloadItem(FData)));
end;

function TCefDownloadItemRef.GetFullPath: ustring;
begin
  Result := CefStringFreeAndGet(PCefDownloadItem(FData)^.get_full_path(PCefDownloadItem(FData)));
end;

function TCefDownloadItemRef.GetId: Cardinal;
begin
  Result := PCefDownloadItem(FData)^.get_id(PCefDownloadItem(FData));
end;

function TCefDownloadItemRef.GetMimeType: ustring;
begin
  Result := CefStringFreeAndGet(PCefDownloadItem(FData)^.get_mime_type(PCefDownloadItem(FData)));
end;

function TCefDownloadItemRef.GetOriginalUrl: ustring;
begin
  Result := CefStringFreeAndGet(PCefDownloadItem(FData)^.get_original_url(PCefDownloadItem(FData)));
end;

function TCefDownloadItemRef.GetPercentComplete: Integer;
begin
  Result := PCefDownloadItem(FData)^.get_percent_complete(PCefDownloadItem(FData));
end;

function TCefDownloadItemRef.GetReceivedBytes: Int64;
begin
  Result := PCefDownloadItem(FData)^.get_received_bytes(PCefDownloadItem(FData));
end;

function TCefDownloadItemRef.GetStartTime: TDateTime;
begin
  Result := CefBaseTimeToDateTime(PCefDownloadItem(FData)^.get_start_time(PCefDownloadItem(FData)));
end;

function TCefDownloadItemRef.GetSuggestedFileName: ustring;
begin
  Result := CefStringFreeAndGet(PCefDownloadItem(FData)^.get_suggested_file_name(PCefDownloadItem(FData)));
end;

function TCefDownloadItemRef.GetTotalBytes: Int64;
begin
  Result := PCefDownloadItem(FData)^.get_total_bytes(PCefDownloadItem(FData));
end;

function TCefDownloadItemRef.GetUrl: ustring;
begin
  Result := CefStringFreeAndGet(PCefDownloadItem(FData)^.get_url(PCefDownloadItem(FData)));
end;

function TCefDownloadItemRef.IsCanceled: Boolean;
begin
  Result := PCefDownloadItem(FData)^.is_canceled(PCefDownloadItem(FData)) <> 0;
end;

function TCefDownloadItemRef.IsInterrupted: Boolean;
begin
  Result := PCefDownloadItem(FData)^.is_interrupted(PCefDownloadItem(FData)) <> 0;
end;

function TCefDownloadItemRef.GetInterruptReason: TCefDownloadInterruptReason;
begin
  Result := PCefDownloadItem(FData)^.get_interrupt_reason(PCefDownloadItem(FData));
end;

function TCefDownloadItemRef.IsComplete: Boolean;
begin
  Result := PCefDownloadItem(FData)^.is_complete(PCefDownloadItem(FData)) <> 0;
end;

function TCefDownloadItemRef.IsInProgress: Boolean;
begin
  Result := PCefDownloadItem(FData)^.is_in_progress(PCefDownloadItem(FData)) <> 0;
end;

function TCefDownloadItemRef.IsValid: Boolean;
begin
  Result := PCefDownloadItem(FData)^.is_valid(PCefDownloadItem(FData)) <> 0;
end;

class function TCefDownloadItemRef.UnWrap(data: Pointer): ICefDownLoadItem;
begin
  if (data <> nil) then
    Result := Create(data) as ICefDownLoadItem
   else
    Result := nil;
end;

end.
