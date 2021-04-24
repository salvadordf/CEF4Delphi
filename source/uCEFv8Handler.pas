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

unit uCEFv8Handler;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  {$IFDEF MSWINDOWS}WinApi.Windows,{$ENDIF} System.Rtti, System.TypInfo, System.Variants,
  System.SysUtils, System.Classes, System.Math, System.SyncObjs,
  {$ELSE}
  {$IFDEF DELPHI14_UP}Rtti,{$ENDIF} TypInfo, Variants, SysUtils, Classes, Math, SyncObjs, {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  {$ENDIF}
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefv8HandlerRef = class(TCefBaseRefCountedRef, ICefv8Handler)
    protected
      function Execute(const name: ustring; const object_: ICefv8Value; const arguments: TCefv8ValueArray; var retval: ICefv8Value; var exception: ustring): Boolean;

    public
      class function UnWrap(data: Pointer): ICefv8Handler;
  end;

  TCefv8HandlerOwn = class(TCefBaseRefCountedOwn, ICefv8Handler)
    protected
      function Execute(const name: ustring; const object_: ICefv8Value; const arguments: TCefv8ValueArray; var retval: ICefv8Value; var exception: ustring): Boolean; virtual;

    public
      constructor Create; virtual;
  end;

{$IFDEF DELPHI14_UP}
  TCefRTTIExtension = class(TCefv8HandlerOwn)
    protected
      FValue: TValue;
      FCtx: TRttiContext;
      FSyncMainThread: Boolean;

      function GetValue(pi: PTypeInfo; const v: ICefv8Value; var ret: TValue): Boolean;
      function SetValue(const v: TValue; var ret: ICefv8Value): Boolean;
  {$IFDEF CPUX64}
      class function StrToPtr(const str: ustring): Pointer;
      class function PtrToStr(p: Pointer): ustring;
  {$ENDIF}
      function Execute(const name: ustring; const object_: ICefv8Value; const arguments: TCefv8ValueArray; var retval: ICefv8Value; var exception: ustring): Boolean; override;

    public
      constructor Create(const value: TValue; SyncMainThread: Boolean = False); reintroduce;
      destructor Destroy; override;
      class function Register(const name: ustring; const value: TValue; SyncMainThread: Boolean = False) : boolean;
  end;
{$ENDIF}

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFv8Value, uCEFConstants;

function cef_v8_handler_execute(      self           : PCefv8Handler;
                                const name           : PCefString;
                                      object_        : PCefv8Value;
                                      argumentsCount : NativeUInt;
                                const arguments      : PPCefV8Value;
                                var   retval         : PCefV8Value;
                                      exception      : PCefString): Integer; stdcall;
var
  TempArgs        : TCefv8ValueArray;
  i               : NativeUInt;
  TempReturnValue : ICefv8Value;
  TempException   : ustring;
  TempObject      : TObject;
  TempRecObject   : ICefv8Value;
begin
  Result     := Ord(False);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefv8HandlerOwn) then
    try
      TempRecObject   := TCefv8ValueRef.UnWrap(object_);
      TempReturnValue := nil;
      TempArgs        := nil;
      TempException   := '';

      if (arguments <> nil) and (argumentsCount > 0) then
        begin
          SetLength(TempArgs, argumentsCount);

          i := 0;
          while (i < argumentsCount) do
            begin
              TempArgs[i] := TCefv8ValueRef.UnWrap(arguments^[i]);
              inc(i);
            end;
        end;

      Result := Ord(TCefv8HandlerOwn(TempObject).Execute(CefString(name),
                                                         TempRecObject,
                                                         TempArgs,
                                                         TempReturnValue,
                                                         TempException));

      retval := CefGetData(TempReturnValue);

      if (exception <> nil) then
        begin
          CefStringFree(exception);
          exception^ := CefStringAlloc(TempException);
        end;
    finally
      i := 0;
      while (i < argumentsCount) do
        begin
          TempArgs[i] := nil;
          inc(i);
        end;

      TempRecObject   := nil;
      TempReturnValue := nil;
    end;
end;

function TCefv8HandlerRef.Execute(const name      : ustring;
                                  const object_   : ICefv8Value;
                                  const arguments : TCefv8ValueArray;
                                  var   retval    : ICefv8Value;
                                  var   exception : ustring): Boolean;
var
  TempArgs        : array of PCefV8Value;
  TempLen, i      : integer;
  TempReturnValue : PCefV8Value;
  TempException   : TCefString;
  TempName        : TCefString;
begin
  i       := 0;
  TempLen := Length(arguments);

  SetLength(TempArgs, TempLen);

  while (i < TempLen) do
    begin
      TempArgs[i] := CefGetData(arguments[i]);
      inc(i);
    end;

  CefStringInitialize(@TempException);

  TempReturnValue := nil;
  TempName        := CefString(name);
  Result          := PCefv8Handler(FData)^.execute(PCefv8Handler(FData), @TempName, CefGetData(object_), TempLen, @TempArgs, TempReturnValue, @TempException) <> 0;
  retval          := TCefv8ValueRef.UnWrap(TempReturnValue);
  exception       := CefStringClearAndGet(@TempException);
end;

class function TCefv8HandlerRef.UnWrap(data: Pointer): ICefv8Handler;
begin
  if (data <> nil) then
    Result := Create(data) as ICefv8Handler
   else
    Result := nil;
end;

// TCefv8HandlerOwn

constructor TCefv8HandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefv8Handler));

  PCefv8Handler(FData)^.execute := {$IFDEF FPC}@{$ENDIF}cef_v8_handler_execute;
end;

function TCefv8HandlerOwn.Execute(const name: ustring; const object_: ICefv8Value; const arguments: TCefv8ValueArray; var retval: ICefv8Value; var exception: ustring): Boolean;
begin
  Result := False;
end;


{$IFDEF DELPHI14_UP}
// TCefRTTIExtension

constructor TCefRTTIExtension.Create(const value: TValue; SyncMainThread: Boolean);
begin
  inherited Create;

  FCtx            := TRttiContext.Create;
  FSyncMainThread := SyncMainThread;
  FValue          := value;
end;

destructor TCefRTTIExtension.Destroy;
begin
  FCtx.Free;

  inherited Destroy;
end;

function TCefRTTIExtension.GetValue(pi: PTypeInfo; const v: ICefv8Value; var ret: TValue): Boolean;
  function ProcessInt: Boolean;
  var
    sv: record
      case byte of
      0:  (ub: Byte);
      1:  (sb: ShortInt);
      2:  (uw: Word);
      3:  (sw: SmallInt);
      4:  (si: Integer);
      5:  (ui: Cardinal);
    end;
    pd: PTypeData;
  begin
    pd := GetTypeData(pi);
    if (v.IsInt or v.IsBool) and (v.GetIntValue >= pd.MinValue) and (v.GetIntValue <= pd.MaxValue) then
    begin
      case pd.OrdType of
        otSByte: sv.sb := v.GetIntValue;
        otUByte: sv.ub := v.GetIntValue;
        otSWord: sv.sw := v.GetIntValue;
        otUWord: sv.uw := v.GetIntValue;
        otSLong: sv.si := v.GetIntValue;
        otULong: sv.ui := v.GetIntValue;
      end;
      TValue.Make(@sv, pi, ret);
    end else
      Exit(False);
    Result := True;
  end;

  function ProcessInt64: Boolean;
  var
    i: Int64;
  begin
    i := StrToInt64(v.GetStringValue); // hack
    TValue.Make(@i, pi, ret);
    Result := True;
  end;

  function ProcessUString: Boolean;
  var
    vus: string;
  begin
    if v.IsString then
    begin
      vus := v.GetStringValue;
      TValue.Make(@vus, pi, ret);
    end else
      Exit(False);
    Result := True;
  end;

  function ProcessLString: Boolean;
  var
    vas: AnsiString;
  begin
    if v.IsString then
    begin
      vas := AnsiString(v.GetStringValue);
      TValue.Make(@vas, pi, ret);
    end else
      Exit(False);
    Result := True;
  end;

  function ProcessWString: Boolean;
  var
    vws: WideString;
  begin
    if v.IsString then
    begin
      vws := v.GetStringValue;
      TValue.Make(@vws, pi, ret);
    end else
      Exit(False);
    Result := True;
  end;

  function ProcessFloat: Boolean;
  var
    sv: record
      case byte of
      0: (fs: Single);
      1: (fd: Double);
      2: (fe: Extended);
      3: (fc: Comp);
      4: (fcu: Currency);
    end;
  begin
    if v.IsDouble or v.IsInt then
    begin
      case GetTypeData(pi).FloatType of
        ftSingle: sv.fs := v.GetDoubleValue;
        ftDouble: sv.fd := v.GetDoubleValue;
        ftExtended: sv.fe := v.GetDoubleValue;
        ftComp: sv.fc := v.GetDoubleValue;
        ftCurr: sv.fcu := v.GetDoubleValue;
      end;
      TValue.Make(@sv, pi, ret);
    end else
    if v.IsDate then
    begin
      sv.fd := v.GetDateValue;
      TValue.Make(@sv, pi, ret);
    end else
      Exit(False);
    Result := True;
  end;

  function ProcessSet: Boolean;
  var
    sv: record
      case byte of
      0:  (ub: Byte);
      1:  (sb: ShortInt);
      2:  (uw: Word);
      3:  (sw: SmallInt);
      4:  (si: Integer);
      5:  (ui: Cardinal);
    end;
  begin
    if v.IsInt then
    begin
      case GetTypeData(pi).OrdType of
        otSByte: sv.sb := v.GetIntValue;
        otUByte: sv.ub := v.GetIntValue;
        otSWord: sv.sw := v.GetIntValue;
        otUWord: sv.uw := v.GetIntValue;
        otSLong: sv.si := v.GetIntValue;
        otULong: sv.ui := v.GetIntValue;
      end;
      TValue.Make(@sv, pi, ret);
    end else
      Exit(False);
    Result := True;
  end;

  function ProcessVariant: Boolean;
  var
    vr   : Variant;
    i, j : Integer;
    vl   : TValue;
  begin
    VarClear(vr);
    if v.IsString then vr := v.GetStringValue else
    if v.IsBool then vr := v.GetBoolValue else
    if v.IsInt then vr := v.GetIntValue else
    if v.IsDouble then vr := v.GetDoubleValue else
    if v.IsUndefined then TVarData(vr).VType := varEmpty else
    if v.IsNull then TVarData(vr).VType := varNull else
    if v.IsArray then
      begin
        i  := 0;
        j  := v.GetArrayLength;
        vr := VarArrayCreate([0, j], varVariant);

        while (i < j) do
          begin
            if not GetValue(pi, v.GetValueByIndex(i), vl) then Exit(False);
            VarArrayPut(vr, vl.AsVariant, i);
            inc(i);
          end;

      end else
      Exit(False);

    TValue.Make(@vr, pi, ret);
    Result := True;
  end;

  function ProcessObject: Boolean;
  var
    ud: ICefv8Value;
    i: Pointer;
    td: PTypeData;
    rt: TRttiType;
  begin
    if v.IsObject then
    begin
      ud := v.GetUserData;
      if (ud = nil) then Exit(False);
{$IFDEF CPUX64}
      rt := StrToPtr(ud.GetValueByIndex(0).GetStringValue);
{$ELSE}
      rt := TRttiType(ud.GetValueByIndex(0).GetIntValue);
{$ENDIF}
      td := GetTypeData(rt.Handle);

      if (rt.TypeKind = tkClass) and td.ClassType.InheritsFrom(GetTypeData(pi).ClassType) then
      begin
{$IFDEF CPUX64}
        i := StrToPtr(ud.GetValueByIndex(1).GetStringValue);
{$ELSE}
        i := Pointer(ud.GetValueByIndex(1).GetIntValue);
{$ENDIF}

        TValue.Make(@i, pi, ret);
      end else
        Exit(False);
    end else
      Exit(False);
    Result := True;
  end;

  function ProcessClass: Boolean;
  var
    ud: ICefv8Value;
    i: Pointer;
    rt: TRttiType;
  begin
    if v.IsObject then
    begin
      ud := v.GetUserData;
      if (ud = nil) then Exit(False);
{$IFDEF CPUX64}
      rt := StrToPtr(ud.GetValueByIndex(0).GetStringValue);
{$ELSE}
      rt := TRttiType(ud.GetValueByIndex(0).GetIntValue);
{$ENDIF}

      if (rt.TypeKind = tkClassRef) then
      begin
{$IFDEF CPUX64}
        i := StrToPtr(ud.GetValueByIndex(1).GetStringValue);
{$ELSE}
        i := Pointer(ud.GetValueByIndex(1).GetIntValue);
{$ENDIF}
        TValue.Make(@i, pi, ret);
      end else
        Exit(False);
    end else
      Exit(False);
    Result := True;
  end;

  function ProcessRecord: Boolean;
  var
    r: TRttiField;
    f: TValue;
    rec: Pointer;
  begin
    if v.IsObject then
    begin
      TValue.Make(nil, pi, ret);
      {$IFDEF DELPHI15_UP}
      rec := TValueData(ret).FValueData.GetReferenceToRawData;
      {$ELSE}
      rec := IValueData(TValueData(ret).FHeapData).GetReferenceToRawData;
      {$ENDIF}
      for r in FCtx.GetType(pi).GetFields do
      begin
        if not GetValue(r.FieldType.Handle, v.GetValueByKey(r.Name), f) then
          Exit(False);
        r.SetValue(rec, f);
      end;
      Result := True;
    end else
      Result := False;
  end;

  function ProcessInterface: Boolean;
  begin
    if pi = TypeInfo(ICefV8Value) then
    begin
      TValue.Make(@v, pi, ret);
      Result := True;
    end else
      Result := False; // todo
  end;
begin
  case pi.Kind of
    tkInteger, tkEnumeration: Result := ProcessInt;
    tkInt64: Result := ProcessInt64;
    tkUString: Result := ProcessUString;
    tkLString: Result := ProcessLString;
    tkWString: Result := ProcessWString;
    tkFloat: Result := ProcessFloat;
    tkSet: Result := ProcessSet;
    tkVariant: Result := ProcessVariant;
    tkClass: Result := ProcessObject;
    tkClassRef: Result := ProcessClass;
    tkRecord: Result := ProcessRecord;
    tkInterface: Result := ProcessInterface;
  else
    Result := False;
  end;
end;

function TCefRTTIExtension.SetValue(const v: TValue; var ret: ICefv8Value): Boolean;

  function ProcessRecord: Boolean;
  var
    rf: TRttiField;
    vl: TValue;
    ud, v8: ICefv8Value;
    rec: Pointer;
    rt: TRttiType;
  begin
    ud := TCefv8ValueRef.NewArray(1);
    rt := FCtx.GetType(v.TypeInfo);
{$IFDEF CPUX64}
    ud.SetValueByIndex(0, TCefv8ValueRef.NewString(PtrToStr(rt)));
{$ELSE}
    ud.SetValueByIndex(0, TCefv8ValueRef.NewInt(Integer(rt)));
{$ENDIF}
    ret := TCefv8ValueRef.NewObject(nil, nil);
    ret.SetUserData(ud);

{$IFDEF DELPHI15_UP}
    rec := TValueData(v).FValueData.GetReferenceToRawData;
{$ELSE}
    rec := IValueData(TValueData(v).FHeapData).GetReferenceToRawData;
{$ENDIF}

    if FSyncMainThread then
    begin
      v8 := ret;
      TThread.Synchronize(nil, procedure
      var
        rf: TRttiField;
        o: ICefv8Value;
      begin
        for rf in rt.GetFields do
        begin
          vl := rf.GetValue(rec);
          SetValue(vl, o);
          v8.SetValueByKey(rf.Name, o, V8_PROPERTY_ATTRIBUTE_NONE);
        end;
      end)
    end else
      for rf in FCtx.GetType(v.TypeInfo).GetFields do
      begin
        vl := rf.GetValue(rec);
        if not SetValue(vl, v8) then
          Exit(False);
        ret.SetValueByKey(rf.Name, v8,  V8_PROPERTY_ATTRIBUTE_NONE);
      end;
    Result := True;
  end;

  function ProcessObject: Boolean;
  var
    m: TRttiMethod;
    p: TRttiProperty;
    fl: TRttiField;
    f: ICefv8Value;
    _r, _g, _s, ud: ICefv8Value;
    _a: TCefv8ValueArray;
    rt: TRttiType;
  begin
    rt := FCtx.GetType(v.TypeInfo);

    ud := TCefv8ValueRef.NewArray(2);
{$IFDEF CPUX64}
    ud.SetValueByIndex(0, TCefv8ValueRef.NewString(PtrToStr(rt)));
    ud.SetValueByIndex(1, TCefv8ValueRef.NewString(PtrToStr(v.AsObject)));
{$ELSE}
    ud.SetValueByIndex(0, TCefv8ValueRef.NewInt(Integer(rt)));
    ud.SetValueByIndex(1, TCefv8ValueRef.NewInt(Integer(v.AsObject)));
{$ENDIF}
    ret := TCefv8ValueRef.NewObject(nil, nil); // todo
    ret.SetUserData(ud);

    for m in rt.GetMethods do
      if m.Visibility > mvProtected then
      begin
        f := TCefv8ValueRef.NewFunction(m.Name, Self);
        ret.SetValueByKey(m.Name, f, V8_PROPERTY_ATTRIBUTE_NONE);
      end;

    for p in rt.GetProperties do
      if (p.Visibility > mvProtected) then
      begin
        if _g = nil then _g := ret.GetValueByKey('__defineGetter__');
        if _s = nil then _s := ret.GetValueByKey('__defineSetter__');
        SetLength(_a, 2);
        _a[0] := TCefv8ValueRef.NewString(p.Name);
        if p.IsReadable then
        begin
          _a[1] := TCefv8ValueRef.NewFunction('$pg' + p.Name, Self);
          _r := _g.ExecuteFunction(ret, _a);
        end;
        if p.IsWritable then
        begin
          _a[1] := TCefv8ValueRef.NewFunction('$ps' + p.Name, Self);
          _r := _s.ExecuteFunction(ret, _a);
        end;
      end;

    for fl in rt.GetFields do
      if (fl.Visibility > mvProtected) then
      begin
        if _g = nil then _g := ret.GetValueByKey('__defineGetter__');
        if _s = nil then _s := ret.GetValueByKey('__defineSetter__');

        SetLength(_a, 2);
        _a[0] := TCefv8ValueRef.NewString(fl.Name);
        _a[1] := TCefv8ValueRef.NewFunction('$vg' + fl.Name, Self);
        _r := _g.ExecuteFunction(ret, _a);
        _a[1] := TCefv8ValueRef.NewFunction('$vs' + fl.Name, Self);
        _r := _s.ExecuteFunction(ret, _a);
      end;

    Result := True;
  end;

  function ProcessClass: Boolean;
  var
    m: TRttiMethod;
    f, ud: ICefv8Value;
    c: TClass;
    rt: TRttiType;
  begin
    c := v.AsClass;
    rt := FCtx.GetType(c);

    ud := TCefv8ValueRef.NewArray(2);
{$IFDEF CPUX64}
    ud.SetValueByIndex(0, TCefv8ValueRef.NewString(PtrToStr(rt)));
    ud.SetValueByIndex(1, TCefv8ValueRef.NewString(PtrToStr(c)));
{$ELSE}
    ud.SetValueByIndex(0, TCefv8ValueRef.NewInt(Integer(rt)));
    ud.SetValueByIndex(1, TCefv8ValueRef.NewInt(Integer(c)));
{$ENDIF}
    ret := TCefv8ValueRef.NewObject(nil, nil); // todo
    ret.SetUserData(ud);

    if c <> nil then
    begin
      for m in rt.GetMethods do
        if (m.Visibility > mvProtected) and (m.MethodKind in [mkClassProcedure, mkClassFunction]) then
        begin
          f := TCefv8ValueRef.NewFunction(m.Name, Self);
          ret.SetValueByKey(m.Name, f, V8_PROPERTY_ATTRIBUTE_NONE);
        end;
    end;

    Result := True;
  end;

  function ProcessVariant: Boolean;
  var
    vr: Variant;
  begin
    vr := v.AsVariant;
    case TVarData(vr).VType of
      varSmallint, varInteger, varShortInt:
        ret := TCefv8ValueRef.NewInt(vr);
      varByte, varWord, varLongWord:
        ret := TCefv8ValueRef.NewUInt(vr);
      varUString, varOleStr, varString:
        ret := TCefv8ValueRef.NewString(vr);
      varSingle, varDouble, varCurrency, varUInt64, varInt64:
        ret := TCefv8ValueRef.NewDouble(vr);
      varBoolean:
        ret := TCefv8ValueRef.NewBool(vr);
      varNull:
        ret := TCefv8ValueRef.NewNull;
      varEmpty:
        ret := TCefv8ValueRef.NewUndefined;
    else
      ret := nil;
      Exit(False)
    end;
    Result := True;
  end;

  function ProcessInterface: Boolean;
  var
    m: TRttiMethod;
    f: ICefv8Value;
    ud: ICefv8Value;
    rt: TRttiType;
  begin

    if TypeInfo(ICefV8Value) = v.TypeInfo then
    begin
      ret := ICefV8Value(v.AsInterface);
      Result := True;
    end else
    begin
      rt := FCtx.GetType(v.TypeInfo);


      ud := TCefv8ValueRef.NewArray(2);
  {$IFDEF CPUX64}
      ud.SetValueByIndex(0, TCefv8ValueRef.NewString(PtrToStr(rt)));
      ud.SetValueByIndex(1, TCefv8ValueRef.NewString(PtrToStr(Pointer(v.AsInterface))));
  {$ELSE}
      ud.SetValueByIndex(0, TCefv8ValueRef.NewInt(Integer(rt)));
      ud.SetValueByIndex(1, TCefv8ValueRef.NewInt(Integer(v.AsInterface)));
  {$ENDIF}
      ret := TCefv8ValueRef.NewObject(nil, nil);
      ret.SetUserData(ud);

      for m in rt.GetMethods do
        if m.Visibility > mvProtected then
        begin
          f := TCefv8ValueRef.NewFunction(m.Name, Self);
          ret.SetValueByKey(m.Name, f, V8_PROPERTY_ATTRIBUTE_NONE);
        end;

      Result := True;
    end;
  end;

  function ProcessFloat: Boolean;
  begin
    if v.TypeInfo = TypeInfo(TDateTime) then
      ret := TCefv8ValueRef.NewDate(TValueData(v).FAsDouble) else
      ret := TCefv8ValueRef.NewDouble(v.AsExtended);
    Result := True;
  end;

begin
  case v.TypeInfo.Kind of
    tkUString, tkLString, tkWString, tkChar, tkWChar:
      ret := TCefv8ValueRef.NewString(v.AsString);
    tkInteger: ret := TCefv8ValueRef.NewInt(v.AsInteger);
    tkEnumeration:
      if v.TypeInfo = TypeInfo(Boolean) then
        ret := TCefv8ValueRef.NewBool(v.AsBoolean) else
        ret := TCefv8ValueRef.NewInt(TValueData(v).FAsSLong);
    tkFloat: if not ProcessFloat then Exit(False);
    tkInt64: ret := TCefv8ValueRef.NewDouble(v.AsInt64);
    tkClass: if not ProcessObject then Exit(False);
    tkClassRef: if not ProcessClass then Exit(False);
    tkRecord: if not ProcessRecord then Exit(False);
    tkVariant: if not ProcessVariant then Exit(False);
    tkInterface: if not ProcessInterface then Exit(False);
  else
    Exit(False)
  end;
  Result := True;
end;

class function TCefRTTIExtension.Register(const name: ustring; const value: TValue; SyncMainThread: Boolean) : boolean;
var
  TempCode    : ustring;
  TempHandler : ICefv8Handler;
begin
  try
    TempHandler := TCefRTTIExtension.Create(value, SyncMainThread);
    TempCode    := format('this.__defineSetter__(''%s'', function(v){native function $s();$s(v)});' +
                          'this.__defineGetter__(''%0:s'', function(){native function $g();return $g()});',
                          [name]);

    Result := CefRegisterExtension(name, TempCode, TempHandler);
  finally
    TempHandler := nil;
  end;
end;

{$IFDEF CPUX64}
class function TCefRTTIExtension.StrToPtr(const str: ustring): Pointer;
begin
  HexToBin(PWideChar(str), @Result, SizeOf(Result));
end;

class function TCefRTTIExtension.PtrToStr(p: Pointer): ustring;
begin
  SetLength(Result, SizeOf(p)*2);
  BinToHex(@p, PWideChar(Result), SizeOf(p));
end;
{$ENDIF}

function TCefRTTIExtension.Execute(const name      : ustring;
                                   const object_   : ICefv8Value;
                                   const arguments : TCefv8ValueArray;
                                   var   retval    : ICefv8Value;
                                   var   exception : ustring): Boolean;
var
  p: PChar;
  ud: ICefv8Value;
  rt: TRttiType;
  val: TObject;
  cls: TClass;
  m: TRttiMethod;
  pr: TRttiProperty;
  vl: TRttiField;
  args: array of TValue;
  prm: TArray<TRttiParameter>;
  i: Integer;
  ret: TValue;
begin
  Result := True;
  p := PChar(name);
  m := nil;
  if object_ <> nil then
  begin
    ud := object_.GetUserData;
    if ud <> nil then
    begin
{$IFDEF CPUX64}
      rt := StrToPtr(ud.GetValueByIndex(0).GetStringValue);
{$ELSE}
      rt := TRttiType(ud.GetValueByIndex(0).GetIntValue);
{$ENDIF}
      case rt.TypeKind of
        tkClass:
          begin
{$IFDEF CPUX64}
            val := StrToPtr(ud.GetValueByIndex(1).GetStringValue);
{$ELSE}
            val := TObject(ud.GetValueByIndex(1).GetIntValue);
{$ENDIF}
            cls := GetTypeData(rt.Handle).ClassType;

            if p^ = '$' then
            begin
              inc(p);
              case p^ of
                'p':
                  begin
                    inc(p);
                    case p^ of
                    'g':
                      begin
                        inc(p);
                        pr := rt.GetProperty(p);
                        if FSyncMainThread then
                        begin
                          TThread.Synchronize(nil, procedure begin
                            ret := pr.GetValue(val);
                          end);
                          Exit(SetValue(ret, retval));
                        end else
                          Exit(SetValue(pr.GetValue(val), retval));
                      end;
                    's':
                      begin
                        inc(p);
                        pr := rt.GetProperty(p);
                        if GetValue(pr.PropertyType.Handle, arguments[0], ret) then
                        begin
                          if FSyncMainThread then
                            TThread.Synchronize(nil, procedure begin
                              pr.SetValue(val, ret) end) else
                            pr.SetValue(val, ret);
                          Exit(True);
                        end else
                          Exit(False);
                      end;
                    end;
                  end;
                'v':
                  begin
                    inc(p);
                    case p^ of
                    'g':
                      begin
                        inc(p);
                        vl := rt.GetField(p);
                        if FSyncMainThread then
                        begin
                          TThread.Synchronize(nil, procedure begin
                            ret := vl.GetValue(val);
                          end);
                          Exit(SetValue(ret, retval));
                        end else
                          Exit(SetValue(vl.GetValue(val), retval));
                      end;
                    's':
                      begin
                        inc(p);
                        vl := rt.GetField(p);
                        if GetValue(vl.FieldType.Handle, arguments[0], ret) then
                        begin
                          if FSyncMainThread then
                            TThread.Synchronize(nil, procedure begin
                              vl.SetValue(val, ret) end) else
                            vl.SetValue(val, ret);
                          Exit(True);
                        end else
                          Exit(False);
                      end;
                    end;
                  end;
              end;
            end else
              m := rt.GetMethod(name);
          end;
        tkClassRef:
          begin
            val := nil;
{$IFDEF CPUX64}
            cls := StrToPtr(ud.GetValueByIndex(1).GetStringValue);
{$ELSE}
            cls := TClass(ud.GetValueByIndex(1).GetIntValue);
{$ENDIF}
            m := FCtx.GetType(cls).GetMethod(name);
          end;
      else
        m := nil;
        cls := nil;
        val := nil;
      end;

      prm := m.GetParameters;
      i := Length(prm);
      if i = Length(arguments) then
      begin
        SetLength(args, i);
        for i := 0 to i - 1 do
          if not GetValue(prm[i].ParamType.Handle, arguments[i], args[i]) then
            Exit(False);

        case m.MethodKind of
          mkClassProcedure, mkClassFunction:
            if FSyncMainThread then
              TThread.Synchronize(nil, procedure begin
                ret := m.Invoke(cls, args) end) else
              ret := m.Invoke(cls, args);
          mkProcedure, mkFunction:
            if (val <> nil) then
            begin
              if FSyncMainThread then
                TThread.Synchronize(nil, procedure begin
                  ret := m.Invoke(val, args) end) else
                ret := m.Invoke(val, args);
            end else
              Exit(False)
        else
          Exit(False);
        end;

        if m.MethodKind in [mkClassFunction, mkFunction] then
          if not SetValue(ret, retval) then
            Exit(False);
      end else
        Exit(False);
    end else
    if p^ = '$' then
    begin
      inc(p);
      case p^ of
        'g': SetValue(FValue, retval);
        's': GetValue(FValue.TypeInfo, arguments[0], FValue);
      else
        Exit(False);
      end;
    end else
      Exit(False);
  end else
    Exit(False);
end;
{$ENDIF}

end.
