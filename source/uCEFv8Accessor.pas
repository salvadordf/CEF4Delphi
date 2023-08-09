unit uCEFv8Accessor;

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
  TCefV8AccessorOwn = class(TCefBaseRefCountedOwn, ICefV8Accessor)
    protected
      function Get(const name: ustring; const object_: ICefv8Value; var retval: ICefv8Value; var exception: ustring): Boolean; virtual;
      function Set_(const name: ustring; const object_, value: ICefv8Value; var exception: ustring): Boolean; virtual;

    public
      constructor Create; virtual;
  end;

  TCefFastV8Accessor = class(TCefV8AccessorOwn)
    protected
      FGetter: TCefV8AccessorGetterProc;
      FSetter: TCefV8AccessorSetterProc;

      function Get(const name: ustring; const object_: ICefv8Value; var retval: ICefv8Value; var exception: ustring): Boolean; override;
      function Set_(const name: ustring; const object_, value: ICefv8Value; var exception: ustring): Boolean; override;

    public
      constructor Create(const getter: TCefV8AccessorGetterProc; const setter: TCefV8AccessorSetterProc); reintroduce;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFv8Value;

function cef_v8_accessor_get(      self      : PCefV8Accessor;
                             const name      : PCefString;
                                   object_   : PCefv8Value;
                             out   retval    : PCefv8Value;
                                   exception : PCefString): Integer; stdcall;
var
  TempObject      : TObject;
  TempException   : ustring;
  TempReturnValue : ICefv8Value;
  TempRecObject   : ICefv8Value;
begin
  Result     := Ord(False);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefV8AccessorOwn) then
    try
      TempRecObject   := TCefv8ValueRef.UnWrap(object_);
      TempException   := '';
      TempReturnValue := nil;

      Result := Ord(TCefV8AccessorOwn(TempObject).Get(CefString(name),
                                                      TempRecObject,
                                                      TempReturnValue,
                                                      TempException));

      retval := CefGetData(TempReturnValue);

      if (exception <> nil) then
        begin
          CefStringFree(exception);
          exception^ := CefStringAlloc(TempException);
        end;
    finally
      TempRecObject   := nil;
      TempReturnValue := nil;
    end;
end;

function cef_v8_accessor_set(      self      : PCefV8Accessor;
                             const name      : PCefString;
                                   object_   : PCefv8Value;
                                   value     : PCefv8Value;
                                   exception : PCefString): Integer; stdcall;
var
  TempObject    : TObject;
  TempException : ustring;
  TempValue     : ICefv8Value;
  TempRecObject : ICefv8Value;
begin
  Result     := Ord(False);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefV8AccessorOwn) then
    try
      TempRecObject := TCefv8ValueRef.UnWrap(object_);
      TempValue     := TCefv8ValueRef.UnWrap(value);
      TempException := '';

      Result := Ord(TCefV8AccessorOwn(TempObject).Set_(CefString(name),
                                                       TempRecObject,
                                                       TempValue,
                                                       TempException));

      if (exception <> nil) then
        begin
          CefStringFree(exception);
          exception^ := CefStringAlloc(TempException);
        end;
    finally
      TempRecObject := nil;
      TempValue     := nil;
    end;
end;

// TCefV8AccessorOwn

constructor TCefV8AccessorOwn.Create;
begin
  inherited CreateData(SizeOf(TCefV8Accessor));

  with PCefV8Accessor(FData)^ do
    begin
      get  := {$IFDEF FPC}@{$ENDIF}cef_v8_accessor_get;
      set_ := {$IFDEF FPC}@{$ENDIF}cef_v8_accessor_set;
    end;
end;

function TCefV8AccessorOwn.Get(const name: ustring; const object_: ICefv8Value; var retval: ICefv8Value; var exception: ustring): Boolean;
begin
  Result := False;
end;

function TCefV8AccessorOwn.Set_(const name: ustring; const object_, value: ICefv8Value; var exception: ustring): Boolean;
begin
  Result := False;
end;

// TCefFastV8Accessor

constructor TCefFastV8Accessor.Create(const getter: TCefV8AccessorGetterProc; const setter: TCefV8AccessorSetterProc);
begin
  FGetter := getter;
  FSetter := setter;
end;

function TCefFastV8Accessor.Get(const name: ustring; const object_: ICefv8Value; var retval: ICefv8Value; var exception: ustring): Boolean;
begin
  if Assigned(FGetter) then
    Result := FGetter(name, object_, retval, exception)
   else
    Result := False;
end;

function TCefFastV8Accessor.Set_(const name: ustring; const object_, value: ICefv8Value; var exception: ustring): Boolean;
begin
  if Assigned(FSetter) then
    Result := FSetter(name, object_, value, exception)
   else
    Result := False;
end;

end.
