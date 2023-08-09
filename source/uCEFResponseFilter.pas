unit uCEFResponseFilter;

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
