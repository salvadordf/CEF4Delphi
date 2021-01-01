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

unit uCEFv8Value;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes, System.SysUtils,
  {$ELSE}
  Classes, SysUtils,
  {$ENDIF}
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefv8ValueRef = class(TCefBaseRefCountedRef, ICefv8Value)
    protected
      function IsValid: Boolean;
      function IsUndefined: Boolean;
      function IsNull: Boolean;
      function IsBool: Boolean;
      function IsInt: Boolean;
      function IsUInt: Boolean;
      function IsDouble: Boolean;
      function IsDate: Boolean;
      function IsString: Boolean;
      function IsObject: Boolean;
      function IsArray: Boolean;
      function IsArrayBuffer: Boolean;
      function IsFunction: Boolean;
      function IsSame(const that: ICefv8Value): Boolean;
      function GetBoolValue: Boolean;
      function GetIntValue: Integer;
      function GetUIntValue: Cardinal;
      function GetDoubleValue: Double;
      function GetDateValue: TDateTime;
      function GetStringValue: ustring;
      function IsUserCreated: Boolean;
      function HasException: Boolean;
      function GetException: ICefV8Exception;
      function ClearException: Boolean;
      function WillRethrowExceptions: Boolean;
      function SetRethrowExceptions(rethrow: Boolean): Boolean;
      function HasValueByKey(const key: ustring): Boolean;
      function HasValueByIndex(index: Integer): Boolean;
      function DeleteValueByKey(const key: ustring): Boolean;
      function DeleteValueByIndex(index: Integer): Boolean;
      function GetValueByKey(const key: ustring): ICefv8Value;
      function GetValueByIndex(index: Integer): ICefv8Value;
      function SetValueByKey(const key: ustring; const value: ICefv8Value; attribute: TCefV8PropertyAttributes): Boolean;
      function SetValueByIndex(index: Integer; const value: ICefv8Value): Boolean;
      function SetValueByAccessor(const key: ustring; settings: TCefV8AccessControls; attribute: TCefV8PropertyAttributes): Boolean;
      function GetKeys(const keys: TStrings): Integer;
      function SetUserData(const data: ICefv8Value): Boolean;
      function GetUserData: ICefv8Value;
      function GetExternallyAllocatedMemory: Integer;
      function AdjustExternallyAllocatedMemory(changeInBytes: Integer): Integer;
      function GetArrayLength: Integer;
      function GetArrayBufferReleaseCallback : ICefv8ArrayBufferReleaseCallback;
      function NeuterArrayBuffer : boolean;
      function GetFunctionName: ustring;
      function GetFunctionHandler: ICefv8Handler;
      function ExecuteFunction(const obj: ICefv8Value; const arguments: TCefv8ValueArray): ICefv8Value;
      function ExecuteFunctionWithContext(const context: ICefv8Context; const obj: ICefv8Value; const arguments: TCefv8ValueArray): ICefv8Value;

    public
      class function UnWrap(data: Pointer): ICefv8Value;
      class function NewUndefined: ICefv8Value;
      class function NewNull: ICefv8Value;
      class function NewBool(value: Boolean): ICefv8Value;
      class function NewInt(value: Integer): ICefv8Value;
      class function NewUInt(value: Cardinal): ICefv8Value;
      class function NewDouble(value: Double): ICefv8Value;
      class function NewDate(value: TDateTime): ICefv8Value;
      class function NewString(const str: ustring): ICefv8Value;
      class function NewObject(const Accessor: ICefV8Accessor; const Interceptor: ICefV8Interceptor): ICefv8Value;
      class function NewObjectProc(const getter        : TCefV8AccessorGetterProc;
                                   const setter        : TCefV8AccessorSetterProc;
                                   const getterbyname  : TCefV8InterceptorGetterByNameProc;
                                   const setterbyname  : TCefV8InterceptorSetterByNameProc;
                                   const getterbyindex : TCefV8InterceptorGetterByIndexProc;
                                   const setterbyindex : TCefV8InterceptorSetterByIndexProc): ICefv8Value;
      class function NewArray(len: Integer): ICefv8Value;
      class function NewArrayBuffer(buffer: Pointer; length: NativeUInt; const callback : ICefv8ArrayBufferReleaseCallback): ICefv8Value;
      class function NewFunction(const name: ustring; const handler: ICefv8Handler): ICefv8Value;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFv8Accessor, uCEFv8Handler, uCEFv8Exception,
  uCEFv8Interceptor, uCEFStringList, uCEFv8ArrayBufferReleaseCallback;

function TCefv8ValueRef.AdjustExternallyAllocatedMemory(changeInBytes: Integer): Integer;
begin
  Result := PCefV8Value(FData)^.adjust_externally_allocated_memory(PCefV8Value(FData), changeInBytes);
end;

class function TCefv8ValueRef.NewArray(len: Integer): ICefv8Value;
begin
  Result := UnWrap(cef_v8value_create_array(len));
end;

class function TCefv8ValueRef.NewArrayBuffer(      buffer   : Pointer;
                                                   length   : NativeUInt;
                                             const callback : ICefv8ArrayBufferReleaseCallback): ICefv8Value;
begin
  Result := UnWrap(cef_v8value_create_array_buffer(buffer, length, CefGetData(callback)));
end;

class function TCefv8ValueRef.NewBool(value: Boolean): ICefv8Value;
begin
  Result := UnWrap(cef_v8value_create_bool(Ord(value)));
end;

class function TCefv8ValueRef.NewDate(value: TDateTime): ICefv8Value;
var
  TempValue : TCefTime;
begin
  TempValue := DateTimeToCefTime(value);
  Result    := UnWrap(cef_v8value_create_date(@TempValue));
end;

class function TCefv8ValueRef.NewDouble(value: Double): ICefv8Value;
begin
  Result := UnWrap(cef_v8value_create_double(value));
end;

class function TCefv8ValueRef.NewFunction(const name: ustring; const handler: ICefv8Handler): ICefv8Value;
var
  TempName : TCefString;
begin
  TempName := CefString(name);
  Result   := UnWrap(cef_v8value_create_function(@TempName, CefGetData(handler)));
end;

class function TCefv8ValueRef.NewInt(value: Integer): ICefv8Value;
begin
  Result := UnWrap(cef_v8value_create_int(value));
end;

class function TCefv8ValueRef.NewUInt(value: Cardinal): ICefv8Value;
begin
  Result := UnWrap(cef_v8value_create_uint(value));
end;

class function TCefv8ValueRef.NewNull: ICefv8Value;
begin
  Result := UnWrap(cef_v8value_create_null());
end;

class function TCefv8ValueRef.NewObject(const Accessor: ICefV8Accessor; const Interceptor: ICefV8Interceptor): ICefv8Value;
begin
  Result := UnWrap(cef_v8value_create_object(CefGetData(Accessor), CefGetData(Interceptor)));
end;

class function TCefv8ValueRef.NewObjectProc(const getter        : TCefV8AccessorGetterProc;
                                            const setter        : TCefV8AccessorSetterProc;
                                            const getterbyname  : TCefV8InterceptorGetterByNameProc;
                                            const setterbyname  : TCefV8InterceptorSetterByNameProc;
                                            const getterbyindex : TCefV8InterceptorGetterByIndexProc;
                                            const setterbyindex : TCefV8InterceptorSetterByIndexProc): ICefv8Value;
begin
  Result := NewObject(TCefFastV8Accessor.Create(getter, setter) as ICefV8Accessor,
                      TCefFastV8Interceptor.Create(getterbyname, setterbyname, getterbyindex, setterbyindex) as ICefV8Interceptor);
end;

class function TCefv8ValueRef.NewString(const str: ustring): ICefv8Value;
var
  TempString : TCefString;
begin
  TempString := CefString(str);
  Result     := UnWrap(cef_v8value_create_string(@TempString));
end;

class function TCefv8ValueRef.NewUndefined: ICefv8Value;
begin
  Result := UnWrap(cef_v8value_create_undefined());
end;

function TCefv8ValueRef.DeleteValueByIndex(index: Integer): Boolean;
begin
  Result := PCefV8Value(FData)^.delete_value_byindex(PCefV8Value(FData), index) <> 0;
end;

function TCefv8ValueRef.DeleteValueByKey(const key: ustring): Boolean;
var
  TempKey : TCefString;
begin
  TempKey := CefString(key);
  Result  := PCefV8Value(FData)^.delete_value_bykey(PCefV8Value(FData), @TempKey) <> 0;
end;

function TCefv8ValueRef.ExecuteFunction(const obj: ICefv8Value; const arguments: TCefv8ValueArray): ICefv8Value;
var
  args : PPCefV8Value;
  i, j : NativeUInt;
begin
  Result := nil;
  args   := nil;

  try
    try
      if (arguments <> nil) then
        begin
          i := 0;
          j := Length(arguments);

          GetMem(args, SizeOf(PCefV8Value) * j);

          while (i < j) do
            begin
              args^[i] := CefGetData(arguments[i]);
              inc(i);
            end;
        end
       else
        j := 0;

      Result := TCefv8ValueRef.UnWrap(PCefV8Value(FData)^.execute_function(PCefV8Value(FData),
                                                                           CefGetData(obj),
                                                                           j,
                                                                           args));
    except
      on e : exception do
        if CustomExceptionHandler('TCefv8ValueRef.ExecuteFunction', e) then raise;
    end;
  finally
    if (args <> nil) then FreeMem(args);
  end;
end;

function TCefv8ValueRef.ExecuteFunctionWithContext(const context   : ICefv8Context;
                                                   const obj       : ICefv8Value;
                                                   const arguments : TCefv8ValueArray): ICefv8Value;
var
  args : PPCefV8Value;
  i, j : NativeUInt;
begin
  Result := nil;
  args   := nil;

  try
    try
      if (arguments <> nil) then
        begin
          i := 0;
          j := Length(arguments);

          GetMem(args, SizeOf(PCefV8Value) * j);

          while (i < j) do
            begin
              args^[i] := CefGetData(arguments[i]);
              inc(i);
            end;
        end
       else
        j := 0;

      Result := TCefv8ValueRef.UnWrap(PCefV8Value(FData)^.execute_function_with_context(PCefV8Value(FData),
                                                                                        CefGetData(context),
                                                                                        CefGetData(obj),
                                                                                        j,
                                                                                        args));
    except
      on e : exception do
        if CustomExceptionHandler('TCefv8ValueRef.ExecuteFunctionWithContext', e) then raise;
    end;
  finally
    if (args <> nil) then FreeMem(args);
  end;
end;

function TCefv8ValueRef.GetArrayLength: Integer;
begin
  Result := PCefV8Value(FData)^.get_array_length(PCefV8Value(FData));
end;

function TCefv8ValueRef.GetArrayBufferReleaseCallback : ICefv8ArrayBufferReleaseCallback;
begin
  Result := TCefv8ArrayBufferReleaseCallbackRef.UnWrap(PCefV8Value(FData)^.get_array_buffer_release_callback(PCefV8Value(FData)));
end;

function TCefv8ValueRef.NeuterArrayBuffer : boolean;
begin
  Result := PCefV8Value(FData)^.neuter_array_buffer(PCefV8Value(FData)) <> 0;
end;

function TCefv8ValueRef.GetBoolValue: Boolean;
begin
  Result := PCefV8Value(FData)^.get_bool_value(PCefV8Value(FData)) <> 0;
end;

function TCefv8ValueRef.GetDateValue: TDateTime;
begin
  Result := CefTimeToDateTime(PCefV8Value(FData)^.get_date_value(PCefV8Value(FData)));
end;

function TCefv8ValueRef.GetDoubleValue: Double;
begin
  Result := PCefV8Value(FData)^.get_double_value(PCefV8Value(FData));
end;

function TCefv8ValueRef.GetExternallyAllocatedMemory: Integer;
begin
  Result := PCefV8Value(FData)^.get_externally_allocated_memory(PCefV8Value(FData));
end;

function TCefv8ValueRef.GetFunctionHandler: ICefv8Handler;
begin
  Result := TCefv8HandlerRef.UnWrap(PCefV8Value(FData)^.get_function_handler(PCefV8Value(FData)));
end;

function TCefv8ValueRef.GetFunctionName: ustring;
begin
  Result := CefStringFreeAndGet(PCefV8Value(FData)^.get_function_name(PCefV8Value(FData)))
end;

function TCefv8ValueRef.GetIntValue: Integer;
begin
  Result := PCefV8Value(FData)^.get_int_value(PCefV8Value(FData))
end;

function TCefv8ValueRef.GetUIntValue: Cardinal;
begin
  Result := PCefV8Value(FData)^.get_uint_value(PCefV8Value(FData))
end;

function TCefv8ValueRef.GetKeys(const keys: TStrings): Integer;
var
  TempSL : ICefStringList;
begin
  Result := 0;
  TempSL := TCefStringListOwn.Create;

  if (PCefV8Value(FData)^.get_keys(PCefV8Value(FData), TempSL.Handle) <> 0) then
    begin
      TempSL.CopyToStrings(keys);
      Result := keys.Count;
    end;
end;

function TCefv8ValueRef.SetUserData(const data: ICefv8Value): Boolean;
begin
  Result := PCefV8Value(FData)^.set_user_data(PCefV8Value(FData), CefGetData(data)) <> 0;
end;

function TCefv8ValueRef.GetStringValue: ustring;
begin
  Result := CefStringFreeAndGet(PCefV8Value(FData)^.get_string_value(PCefV8Value(FData)));
end;

function TCefv8ValueRef.IsUserCreated: Boolean;
begin
  Result := PCefV8Value(FData)^.is_user_created(PCefV8Value(FData)) <> 0;
end;

function TCefv8ValueRef.IsValid: Boolean;
begin
  Result := PCefV8Value(FData)^.is_valid(PCefV8Value(FData)) <> 0;
end;

function TCefv8ValueRef.HasException: Boolean;
begin
  Result := PCefV8Value(FData)^.has_exception(PCefV8Value(FData)) <> 0;
end;

function TCefv8ValueRef.GetException: ICefV8Exception;
begin
   Result := TCefV8ExceptionRef.UnWrap(PCefV8Value(FData)^.get_exception(PCefV8Value(FData)));
end;

function TCefv8ValueRef.ClearException: Boolean;
begin
  Result := PCefV8Value(FData)^.clear_exception(PCefV8Value(FData)) <> 0;
end;

function TCefv8ValueRef.WillRethrowExceptions: Boolean;
begin
  Result := PCefV8Value(FData)^.will_rethrow_exceptions(PCefV8Value(FData)) <> 0;
end;

function TCefv8ValueRef.SetRethrowExceptions(rethrow: Boolean): Boolean;
begin
  Result := PCefV8Value(FData)^.set_rethrow_exceptions(PCefV8Value(FData), Ord(rethrow)) <> 0;
end;

function TCefv8ValueRef.GetUserData: ICefv8Value;
begin
  Result := TCefv8ValueRef.UnWrap(PCefV8Value(FData)^.get_user_data(PCefV8Value(FData)));
end;

function TCefv8ValueRef.GetValueByIndex(index: Integer): ICefv8Value;
begin
  Result := TCefv8ValueRef.UnWrap(PCefV8Value(FData)^.get_value_byindex(PCefV8Value(FData), index));
end;

function TCefv8ValueRef.GetValueByKey(const key: ustring): ICefv8Value;
var
  TempKey : TCefString;
begin
  TempKey := CefString(key);
  Result  := TCefv8ValueRef.UnWrap(PCefV8Value(FData)^.get_value_bykey(PCefV8Value(FData), @TempKey));
end;

function TCefv8ValueRef.HasValueByIndex(index: Integer): Boolean;
begin
  Result := PCefV8Value(FData)^.has_value_byindex(PCefV8Value(FData), index) <> 0;
end;

function TCefv8ValueRef.HasValueByKey(const key: ustring): Boolean;
var
  TempKey : TCefString;
begin
  TempKey := CefString(key);
  Result  := PCefV8Value(FData)^.has_value_bykey(PCefV8Value(FData), @TempKey) <> 0;
end;

function TCefv8ValueRef.IsArray: Boolean;
begin
  Result := PCefV8Value(FData)^.is_array(PCefV8Value(FData)) <> 0;
end;

function TCefv8ValueRef.IsArrayBuffer: Boolean;
begin
  Result := PCefV8Value(FData)^.is_array_buffer(PCefV8Value(FData)) <> 0;
end;

function TCefv8ValueRef.IsBool: Boolean;
begin
  Result := PCefV8Value(FData)^.is_bool(PCefV8Value(FData)) <> 0;
end;

function TCefv8ValueRef.IsDate: Boolean;
begin
  Result := PCefV8Value(FData)^.is_date(PCefV8Value(FData)) <> 0;
end;

function TCefv8ValueRef.IsDouble: Boolean;
begin
  Result := PCefV8Value(FData)^.is_double(PCefV8Value(FData)) <> 0;
end;

function TCefv8ValueRef.IsFunction: Boolean;
begin
  Result := PCefV8Value(FData)^.is_function(PCefV8Value(FData)) <> 0;
end;

function TCefv8ValueRef.IsInt: Boolean;
begin
  Result := PCefV8Value(FData)^.is_int(PCefV8Value(FData)) <> 0;
end;

function TCefv8ValueRef.IsUInt: Boolean;
begin
  Result := PCefV8Value(FData)^.is_uint(PCefV8Value(FData)) <> 0;
end;

function TCefv8ValueRef.IsNull: Boolean;
begin
  Result := PCefV8Value(FData)^.is_null(PCefV8Value(FData)) <> 0;
end;

function TCefv8ValueRef.IsObject: Boolean;
begin
  Result := PCefV8Value(FData)^.is_object(PCefV8Value(FData)) <> 0;
end;

function TCefv8ValueRef.IsSame(const that: ICefv8Value): Boolean;
begin
  Result := PCefV8Value(FData)^.is_same(PCefV8Value(FData), CefGetData(that)) <> 0;
end;

function TCefv8ValueRef.IsString: Boolean;
begin
  Result := PCefV8Value(FData)^.is_string(PCefV8Value(FData)) <> 0;
end;

function TCefv8ValueRef.IsUndefined: Boolean;
begin
  Result := PCefV8Value(FData)^.is_undefined(PCefV8Value(FData)) <> 0;
end;

function TCefv8ValueRef.SetValueByAccessor(const key: ustring; settings: TCefV8AccessControls; attribute: TCefV8PropertyAttributes): Boolean;
var
  TempKey : TCefString;
begin
  TempKey := CefString(key);
  Result  := PCefV8Value(FData)^.set_value_byaccessor(PCefV8Value(FData), @TempKey, PByte(@settings)^, PByte(@attribute)^) <> 0;
end;

function TCefv8ValueRef.SetValueByIndex(index: Integer; const value: ICefv8Value): Boolean;
begin
  Result:= PCefV8Value(FData)^.set_value_byindex(PCefV8Value(FData), index, CefGetData(value)) <> 0;
end;

function TCefv8ValueRef.SetValueByKey(const key: ustring; const value: ICefv8Value; attribute: TCefV8PropertyAttributes): Boolean;
var
  TempKey : TCefString;
begin
  TempKey := CefString(key);
  Result  := PCefV8Value(FData)^.set_value_bykey(PCefV8Value(FData), @TempKey, CefGetData(value), PByte(@attribute)^) <> 0;
end;

class function TCefv8ValueRef.UnWrap(data: Pointer): ICefv8Value;
begin
  if (data <> nil) then
    Result := Create(data) as ICefv8Value
   else
    Result := nil;
end;

end.
