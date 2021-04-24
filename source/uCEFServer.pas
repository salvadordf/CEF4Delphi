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

unit uCEFServer;

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
  TCEFServerRef = class(TCefBaseRefCountedRef, ICefServer)
    protected
      function  GetTaskRunner : ICefTaskRunner; virtual;
      procedure Shutdown; virtual;
      function  IsRunning : boolean; virtual;
      function  GetAddress : ustring; virtual;
      function  HasConnection : boolean; virtual;
      function  IsValidConnection(connection_id: Integer) : boolean; virtual;
      procedure SendHttp200response(connection_id: Integer; const content_type: ustring; const data: Pointer; data_size: NativeUInt); virtual;
      procedure SendHttp404response(connection_id: Integer); virtual;
      procedure SendHttp500response(connection_id: Integer; const error_message: ustring); virtual;
      procedure SendHttpResponse(connection_id, response_code: Integer; const content_type: ustring; content_length: int64; const extra_headers: ICefStringMultimap); virtual;
      procedure SendRawData(connection_id: Integer; const data: Pointer; data_size: NativeUInt); virtual;
      procedure CloseConnection(connection_id: Integer); virtual;
      procedure SendWebSocketMessage(connection_id: Integer; const data: Pointer; data_size: NativeUInt); virtual;

    public
      class function UnWrap(data: Pointer): ICefServer;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFTaskRunner;

// ******************************************************
// ****************** TCEFServerRef *********************
// ******************************************************

class function TCEFServerRef.UnWrap(data: Pointer): ICefServer;
begin
  if (data <> nil) then
    Result := Create(data) as ICefServer
   else
    Result := nil;
end;

function TCEFServerRef.GetTaskRunner : ICefTaskRunner;
begin
  Result := TCefTaskRunnerRef.UnWrap(PCefServer(FData)^.get_task_runner(PCefServer(FData)));
end;

procedure TCEFServerRef.Shutdown;
begin
  PCefServer(FData)^.shutdown(PCefServer(FData));
end;

function TCEFServerRef.IsRunning : boolean;
begin
  Result := PCefServer(FData)^.is_running(PCefServer(FData)) <> 0;
end;

function TCEFServerRef.GetAddress : ustring;
begin
  Result := CefStringFreeAndGet(PCefServer(FData)^.get_address(PCefServer(FData)));
end;

function TCEFServerRef.HasConnection : boolean;
begin
  Result := PCefServer(FData)^.has_connection(PCefServer(FData)) <> 0;
end;

function TCEFServerRef.IsValidConnection(connection_id: Integer) : boolean;
begin
  Result := PCefServer(FData)^.is_valid_connection(PCefServer(FData), connection_id) <> 0;
end;

procedure TCEFServerRef.SendHttp200response(connection_id: Integer; const content_type: ustring; const data: Pointer; data_size: NativeUInt);
var
  TempContentType : TCefString;
begin
  TempContentType := CefString(content_type);
  PCefServer(FData)^.send_http200response(PCefServer(FData), connection_id, @TempContentType, data, data_size);
end;

procedure TCEFServerRef.SendHttp404response(connection_id: Integer);
begin
  PCefServer(FData)^.send_http404response(PCefServer(FData), connection_id);
end;

procedure TCEFServerRef.SendHttp500response(connection_id: Integer; const error_message: ustring);
var
  TempError : TCefString;
begin
  TempError := CefString(error_message);
  PCefServer(FData)^.send_http500response(PCefServer(FData), connection_id, @TempError);
end;

procedure TCEFServerRef.SendHttpResponse(connection_id, response_code: Integer; const content_type: ustring; content_length: int64; const extra_headers: ICefStringMultimap);
var
  TempContentType : TCefString;
begin
  TempContentType := CefString(content_type);
  PCefServer(FData)^.send_http_response(PCefServer(FData), connection_id, response_code, @TempContentType, content_length, extra_headers.Handle);
end;

procedure TCEFServerRef.SendRawData(connection_id: Integer; const data: Pointer; data_size: NativeUInt);
begin
  PCefServer(FData)^.send_raw_data(PCefServer(FData), connection_id, data, data_size);
end;

procedure TCEFServerRef.CloseConnection(connection_id: Integer);
begin
  PCefServer(FData)^.close_connection(PCefServer(FData), connection_id);
end;

procedure TCEFServerRef.SendWebSocketMessage(connection_id: Integer; const data: Pointer; data_size: NativeUInt);
begin
  PCefServer(FData)^.send_web_socket_message(PCefServer(FData), connection_id, data, data_size);
end;

end.
