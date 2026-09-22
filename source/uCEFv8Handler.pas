unit uCEFv8Handler;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Rtti, System.TypInfo, System.Variants, System.SysUtils, System.Classes,
  {$ELSE}
  {$IFDEF DELPHI14_UP}Rtti,{$ENDIF} TypInfo, Variants, SysUtils, Classes,
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

  TCefCustomUserData = class(TCefBaseRefCountedOwn, ICefCustomUserData)
    protected
      FUserDataType : Pointer;
      FUserData     : Pointer;

      function GetUserDataType : Pointer;
      function GetUserData : Pointer;

    public
      constructor Create(aUserDataType, aUserData : Pointer);
      destructor Destroy; override;
      class function UnWrap(data: Pointer): ICefCustomUserData;
  end;

implementation

uses
  {$IFDEF DELPHI14_UP}uCEFConstants,{$ENDIF} uCEFMiscFunctions, uCEFv8Value;

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
  TempResult      : boolean;
begin
  TempResult := False;
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

      TempResult := TCefv8HandlerOwn(TempObject).Execute(CefString(name),
                                                         TempRecObject,
                                                         TempArgs,
                                                         TempReturnValue,
                                                         TempException);

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

  Result := Ord(TempResult);
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


// TCefCustomUserData

constructor TCefCustomUserData.Create(aUserDataType, aUserData : Pointer);
begin
  inherited CreateData(SizeOf(TCefBaseRefCounted));

  FUserDataType := aUserDataType;
  FUserData     := aUserData;

  {$IFDEF INTFLOG}
  CefDebugLog(ClassName + '.Create');
  {$ENDIF}
end;

destructor TCefCustomUserData.Destroy;
begin
  {$IFDEF INTFLOG}
  CefDebugLog(ClassName + '.Destroy');
  {$ENDIF}
  inherited Destroy;
end;

class function TCefCustomUserData.UnWrap(data: Pointer): ICefCustomUserData;
var
  TempUserData : TCefCustomUserData;
begin
  if (data <> nil) then
    begin
      // Get the original class instance from the data pointer.
      TempUserData := TCefCustomUserData(CefGetObject(data));

      // TempUserData already has an increased reference count.
      // We need to decrease it before querying it with the "as" operator,
      // which increases the count.
      if not(TempUserData.HasOneRef) and TempUserData.HasAtLeastOneRef then
        TempUserData._Release;

      Result := TempUserData as ICefCustomUserData;
    end
   else
    Result := nil;
end;

function TCefCustomUserData.GetUserDataType : Pointer;
begin
  Result := FUserDataType;
end;

function TCefCustomUserData.GetUserData : Pointer;
begin
  Result := FUserData;
end;

end.
