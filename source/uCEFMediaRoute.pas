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

unit uCEFMediaRoute;

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
  TCefMediaRouteRef = class(TCefBaseRefCountedRef, ICefMediaRoute)
  protected
    function  GetId: ustring;
    function  GetSource: ICefMediaSource;
    function  GetSink: ICefMediaSink;
    procedure SendRouteMessage(const message_: ustring);
    procedure Terminate;
  public
    class function UnWrap(data: Pointer): ICefMediaRoute;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFMediaSource, uCEFMediaSink;

function TCefMediaRouteRef.GetId: ustring;
begin
  Result := CefStringFreeAndGet(PCefMediaRoute(FData)^.get_id(PCefMediaRoute(FData)));
end;

function TCefMediaRouteRef.GetSource: ICefMediaSource;
begin
  Result := TCefMediaSourceRef.UnWrap(PCefMediaRoute(FData)^.get_source(PCefMediaRoute(FData)));
end;

function TCefMediaRouteRef.GetSink: ICefMediaSink;
begin
  Result := TCefMediaSinkRef.UnWrap(PCefMediaRoute(FData)^.get_sink(PCefMediaRoute(FData)));
end;

procedure TCefMediaRouteRef.SendRouteMessage(const message_: ustring);
var
  TempMsgPtr : pointer;
  TempMsg    : AnsiString;
  TempSize   : NativeUInt;
begin
  TempSize := length(message_);

  if (TempSize > 0) then
    begin
      TempMsg    := Utf8Encode(message_);
      TempMsgPtr := @TempMsg[1];
    end
   else
    TempMsgPtr := nil;

  PCefMediaRoute(FData)^.send_route_message(PCefMediaRoute(FData),
                                            TempMsgPtr,
                                            TempSize);
end;

procedure TCefMediaRouteRef.Terminate;
begin
  PCefMediaRoute(FData)^.terminate(PCefMediaRoute(FData));
end;

class function TCefMediaRouteRef.UnWrap(data: Pointer): ICefMediaRoute;
begin
  if (data <> nil) then
    Result := Create(data) as ICefMediaRoute
   else
    Result := nil;
end;

end.
