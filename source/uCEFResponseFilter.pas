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

  /// <summary>
  /// Reference class implementing a resource filter.
  /// </summary>
  /// <remarks>
  /// <para>The functions of this interface will be called on the browser process IO thread.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_response_filter_capi.h">CEF source file: /include/capi/cef_response_filter_capi.h (cef_response_filter_t)</see></para>
  /// </remarks>
  TCefResponseFilterRef = class(TCefBaseRefCountedRef, ICefResponseFilter)
    protected
      /// <summary>
      /// Initialize the response filter. Will only be called a single time. The
      /// filter will not be installed if this function returns false (0).
      /// </summary>
      function InitFilter: Boolean; virtual;
      /// <summary>
      /// <para>Called to filter a chunk of data. Expected usage is as follows:</para>
      /// <code>
      ///  1. Read input data from |data_in| and set |data_in_read| to the number of
      ///     bytes that were read up to a maximum of |data_in_size|. |data_in| will
      ///     be NULL if |data_in_size| is zero.
      ///  2. Write filtered output data to |data_out| and set |data_out_written| to
      ///     the number of bytes that were written up to a maximum of
      ///     |data_out_size|. If no output data was written then all data must be
      ///     read from |data_in| (user must set |data_in_read| = |data_in_size|).
      ///  3. Return RESPONSE_FILTER_DONE if all output data was written or
      ///     RESPONSE_FILTER_NEED_MORE_DATA if output data is still pending.
      /// </code>
      /// <para>This function will be called repeatedly until the input buffer has been
      /// fully read (user sets |data_in_read| = |data_in_size|) and there is no
      /// more input data to filter (the resource response is complete). This
      /// function may then be called an additional time with an NULL input buffer
      /// if the user filled the output buffer (set |data_out_written| =
      /// |data_out_size|) and returned RESPONSE_FILTER_NEED_MORE_DATA to indicate
      /// that output data is still pending.</para>
      /// <para>Calls to this function will stop when one of the following conditions is
      /// met:</para>
      /// <code>
      ///  1. There is no more input data to filter (the resource response is
      ///     complete) and the user sets |data_out_written| = 0 or returns
      ///     RESPONSE_FILTER_DONE to indicate that all data has been written, or;
      ///  2. The user returns RESPONSE_FILTER_ERROR to indicate an error.
      /// </code>
      /// <para>Do not keep a reference to the buffers passed to this function.</para>
      /// </summary>
      function Filter(data_in: Pointer; data_in_size: NativeUInt; var data_in_read: NativeUInt; data_out: Pointer; data_out_size : NativeUInt; var data_out_written: NativeUInt): TCefResponseFilterStatus; virtual;

    public
      class function UnWrap(data: Pointer): ICefResponseFilter;
  end;

  /// <summary>
  /// Owned class implementing a resource filter.
  /// </summary>
  /// <remarks>
  /// <para>The functions of this interface will be called on the browser process IO thread.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_response_filter_capi.h">CEF source file: /include/capi/cef_response_filter_capi.h (cef_response_filter_t)</see></para>
  /// </remarks>
  TCefResponseFilterOwn = class(TCefBaseRefCountedOwn, ICefResponseFilter)
    protected
      /// <summary>
      /// Initialize the response filter. Will only be called a single time. The
      /// filter will not be installed if this function returns false (0).
      /// </summary>
      function InitFilter: Boolean; virtual; abstract;
      /// <summary>
      /// <para>Called to filter a chunk of data. Expected usage is as follows:</para>
      /// <code>
      ///  1. Read input data from |data_in| and set |data_in_read| to the number of
      ///     bytes that were read up to a maximum of |data_in_size|. |data_in| will
      ///     be NULL if |data_in_size| is zero.
      ///  2. Write filtered output data to |data_out| and set |data_out_written| to
      ///     the number of bytes that were written up to a maximum of
      ///     |data_out_size|. If no output data was written then all data must be
      ///     read from |data_in| (user must set |data_in_read| = |data_in_size|).
      ///  3. Return RESPONSE_FILTER_DONE if all output data was written or
      ///     RESPONSE_FILTER_NEED_MORE_DATA if output data is still pending.
      /// </code>
      /// <para>This function will be called repeatedly until the input buffer has been
      /// fully read (user sets |data_in_read| = |data_in_size|) and there is no
      /// more input data to filter (the resource response is complete). This
      /// function may then be called an additional time with an NULL input buffer
      /// if the user filled the output buffer (set |data_out_written| =
      /// |data_out_size|) and returned RESPONSE_FILTER_NEED_MORE_DATA to indicate
      /// that output data is still pending.</para>
      /// <para>Calls to this function will stop when one of the following conditions is
      /// met:</para>
      /// <code>
      ///  1. There is no more input data to filter (the resource response is
      ///     complete) and the user sets |data_out_written| = 0 or returns
      ///     RESPONSE_FILTER_DONE to indicate that all data has been written, or;
      ///  2. The user returns RESPONSE_FILTER_ERROR to indicate an error.
      /// </code>
      /// <para>Do not keep a reference to the buffers passed to this function.</para>
      /// </summary>
      function Filter(data_in: Pointer; data_in_size: NativeUInt; var data_in_read: NativeUInt; data_out: Pointer; data_out_size : NativeUInt; var data_out_written: NativeUInt): TCefResponseFilterStatus; virtual; abstract;

    public
      constructor Create; virtual;
  end;

  /// <summary>
  /// Custom class implementing a resource filter with events.
  /// </summary>
  /// <remarks>
  /// <para>The functions and events of this interface will be called on the browser process IO thread.</para>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_response_filter_capi.h">CEF source file: /include/capi/cef_response_filter_capi.h (cef_response_filter_t)</see></para>
  /// </remarks>
  TCustomResponseFilter = class(TCefResponseFilterOwn)
    protected
      FOnFilter     : TOnFilterEvent;
      FOnInitFilter : TOnInitFilterEvent;

      function InitFilter: Boolean; override;
      function Filter(data_in: Pointer; data_in_size: NativeUInt; var data_in_read: NativeUInt; data_out: Pointer; data_out_size : NativeUInt; var data_out_written: NativeUInt): TCefResponseFilterStatus; override;

    public
      constructor Create; override;

      /// <summary>
      /// <para>OnFilter is triggered when ICefResponseFilter.Filter is executed to filter a chunk of data.</para>
      /// <para>Expected usage is as follows:</para>
      /// <code>
      ///  1. Read input data from |data_in| and set |data_in_read| to the number of
      ///     bytes that were read up to a maximum of |data_in_size|. |data_in| will
      ///     be NULL if |data_in_size| is zero.
      ///  2. Write filtered output data to |data_out| and set |data_out_written| to
      ///     the number of bytes that were written up to a maximum of
      ///     |data_out_size|. If no output data was written then all data must be
      ///     read from |data_in| (user must set |data_in_read| = |data_in_size|).
      ///  3. Return RESPONSE_FILTER_DONE if all output data was written or
      ///     RESPONSE_FILTER_NEED_MORE_DATA if output data is still pending.
      /// </code>
      /// <para>This function will be called repeatedly until the input buffer has been
      /// fully read (user sets |data_in_read| = |data_in_size|) and there is no
      /// more input data to filter (the resource response is complete). This
      /// function may then be called an additional time with an NULL input buffer
      /// if the user filled the output buffer (set |data_out_written| =
      /// |data_out_size|) and returned RESPONSE_FILTER_NEED_MORE_DATA to indicate
      /// that output data is still pending.</para>
      /// <para>Calls to this function will stop when one of the following conditions is
      /// met:</para>
      /// <code>
      ///  1. There is no more input data to filter (the resource response is
      ///     complete) and the user sets |data_out_written| = 0 or returns
      ///     RESPONSE_FILTER_DONE to indicate that all data has been written, or;
      ///  2. The user returns RESPONSE_FILTER_ERROR to indicate an error.
      /// </code>
      /// <para>Do not keep a reference to the buffers passed to this function.</para>
      /// </summary>
      property OnFilter      : TOnFilterEvent      read FOnFilter      write FOnFilter;
      /// <summary>
      /// OnInitFilter is triggered when ICefResponseFilter.InitFilter is executed.
      /// Set the aResult parameter to install the filter.
      /// </summary>
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
