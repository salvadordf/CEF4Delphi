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

unit uCEFResponseFilter;

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
  TOnFilterEvent     = procedure(Sender: TObject; data_in: Pointer; data_in_size: NativeUInt; var data_in_read: NativeUInt; data_out: Pointer; data_out_size : NativeUInt; var data_out_written: NativeUInt; var aResult : TCefResponseFilterStatus) of object;
  TOnInitFilterEvent = procedure(Sender: TObject; var aResult : boolean) of object;

  TCefResponseFilterRef = class(TCefBaseRefCountedRef, ICefResponseFilter)
    protected
      function InitFilter: Boolean; virtual;
      function Filter(data_in: Pointer; data_in_size: NativeUInt; var data_in_read: NativeUInt; data_out: Pointer; data_out_size : NativeUInt; var data_out_written: NativeUInt): TCefResponseFilterStatus; virtual;

    public
      class function UnWrap(data: Pointer): ICefResponseFilter;
  end;

  TCefResponseFilterOwn = class(TCefBaseRefCountedOwn, ICefResponseFilter)
    protected
      function InitFilter: Boolean; virtual; abstract;
      function Filter(data_in: Pointer; data_in_size: NativeUInt; var data_in_read: NativeUInt; data_out: Pointer; data_out_size : NativeUInt; var data_out_written: NativeUInt): TCefResponseFilterStatus; virtual; abstract;

    public
      constructor Create; virtual;
  end;

  TCustomResponseFilter = class(TCefResponseFilterOwn)
    protected
      FOnFilter     : TOnFilterEvent;
      FOnInitFilter : TOnInitFilterEvent;

      function InitFilter: Boolean; override;
      function Filter(data_in: Pointer; data_in_size: NativeUInt; var data_in_read: NativeUInt; data_out: Pointer; data_out_size : NativeUInt; var data_out_written: NativeUInt): TCefResponseFilterStatus; override;

    public
      constructor Create; override;

      property OnFilter      : TOnFilterEvent      read FOnFilter      write FOnFilter;
      property OnInitFilter  : TOnInitFilterEvent  read FOnInitFilter  write FOnInitFilter;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;

// TCefResponseFilterOwn

function cef_response_filter_init_filter(self: PCefResponseFilter): Integer; stdcall;
var
  TempObject : TObject;
begin
  Result     := Ord(True);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefResponseFilterOwn) then
    Result := Ord(TCefResponseFilterOwn(TempObject).InitFilter());
end;

function cef_response_filter_filter(    self             : PCefResponseFilter;
                                        data_in          : Pointer;
                                        data_in_size     : NativeUInt;
                                    var data_in_read     : NativeUInt;
                                        data_out         : Pointer;
                                        data_out_size    : NativeUInt;
                                    var data_out_written : NativeUInt): TCefResponseFilterStatus; stdcall;
var
  TempObject : TObject;
begin
  Result     := RESPONSE_FILTER_DONE;
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefResponseFilterOwn) then
    Result := TCefResponseFilterOwn(TempObject).Filter(data_in,  data_in_size,  data_in_read,
                                                       data_out, data_out_size, data_out_written);
end;

constructor TCefResponseFilterOwn.Create;
begin
  inherited CreateData(SizeOf(TCefResponseFilter));

  with PCefResponseFilter(FData)^ do
    begin
      init_filter := {$IFDEF FPC}@{$ENDIF}cef_response_filter_init_filter;
      filter      := {$IFDEF FPC}@{$ENDIF}cef_response_filter_filter;
    end;
end;


// TCustomResponseFilter


constructor TCustomResponseFilter.Create;
begin
  inherited Create;

  FOnFilter     := nil;
  FOnInitFilter := nil;
end;

function TCustomResponseFilter.InitFilter: Boolean;
begin
  Result := True;
  if assigned(FOnInitFilter) then FOnInitFilter(self, Result);
end;

function TCustomResponseFilter.Filter(    data_in          : Pointer;
                                          data_in_size     : NativeUInt;
                                      var data_in_read     : NativeUInt;
                                          data_out         : Pointer;
                                          data_out_size    : NativeUInt;
                                      var data_out_written : NativeUInt) : TCefResponseFilterStatus;
begin
  Result := RESPONSE_FILTER_DONE;

  if assigned(FOnFilter) then
    FOnFilter(self,
              data_in,  data_in_size,  data_in_read,
              data_out, data_out_size, data_out_written,
              Result);
end;


// TCefResponseFilterRef

class function TCefResponseFilterRef.UnWrap(data: Pointer): ICefResponseFilter;
begin
  if (data <> nil) then
    Result := Create(data) as ICefResponseFilter
   else
    Result := nil;
end;

function TCefResponseFilterRef.InitFilter: Boolean;
begin
  Result := PCefResponseFilter(FData)^.init_filter(PCefResponseFilter(FData)) <> 0;
end;

function TCefResponseFilterRef.Filter(    data_in          : Pointer;
                                          data_in_size     : NativeUInt;
                                      var data_in_read     : NativeUInt;
                                          data_out         : Pointer;
                                          data_out_size    : NativeUInt;
                                      var data_out_written : NativeUInt) : TCefResponseFilterStatus;
begin
  Result := PCefResponseFilter(FData)^.filter(PCefResponseFilter(FData),
                                              data_in,
                                              data_in_size,
                                              data_in_read,
                                              data_out,
                                              data_out_size,
                                              data_out_written);
end;

end.
