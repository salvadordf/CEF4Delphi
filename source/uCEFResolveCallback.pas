unit uCEFResolveCallback;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes, System.SysUtils,
  {$ELSE}
  Classes, SysUtils,
  {$ENDIF}
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefResolveCallbackOwn = class(TCefBaseRefCountedOwn, ICefResolveCallback)
    protected
      procedure OnResolveCompleted(result: TCefErrorCode; const resolvedIps: TStrings); virtual; abstract;

    public
      constructor Create; virtual;
  end;

  TCefCustomResolveCallback = class(TCefResolveCallbackOwn)
    protected
      FEvents : Pointer;

      procedure OnResolveCompleted(result: TCefErrorCode; const resolvedIps: TStrings); override;

    public
      constructor Create(const aEvents : IChromiumEvents); reintroduce;
      destructor  Destroy; override;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFStringList;

procedure cef_resolve_callback_on_resolve_completed(self         : PCefResolveCallback;
                                                    result       : TCefErrorCode;
                                                    resolved_ips : TCefStringList); stdcall;
var
  TempSL     : TStringList;
  TempCefSL  : ICefStringList;
  TempObject : TObject;
begin
  TempSL := nil;

  try
    try
      TempObject := CefGetObject(self);

      if (TempObject <> nil) and (TempObject is TCefResolveCallbackOwn) then
        begin
          TempSL    := TStringList.Create;
          TempCefSL := TCefStringListRef.Create(resolved_ips);
          TempCefSL.CopyToStrings(TempSL);

          TCefResolveCallbackOwn(TempObject).OnResolveCompleted(result, TempSL);
        end;
    except
      on e : exception do
        if CustomExceptionHandler('cef_resolve_callback_on_resolve_completed', e) then raise;
    end;
  finally
    if (TempSL <> nil) then FreeAndNil(TempSL);
  end;
end;

// TCefResolveCallbackOwn

constructor TCefResolveCallbackOwn.Create;
begin
  inherited CreateData(SizeOf(TCefResolveCallback));

  with PCefResolveCallback(FData)^ do
    on_resolve_completed := {$IFDEF FPC}@{$ENDIF}cef_resolve_callback_on_resolve_completed;
end;

// TCefCustomResolveCallback

constructor TCefCustomResolveCallback.Create(const aEvents : IChromiumEvents);
begin
  inherited Create;

  FEvents := Pointer(aEvents);
end;

destructor TCefCustomResolveCallback.Destroy;
begin
  FEvents := nil;

  inherited Destroy;
end;

procedure TCefCustomResolveCallback.OnResolveCompleted(result: TCefErrorCode; const resolvedIps: TStrings);
begin
  try
    try
      if (FEvents <> nil) then IChromiumEvents(FEvents).doResolvedHostAvailable(result, resolvedIps);
    except
      on e : exception do
        if CustomExceptionHandler('TCefCustomResolveCallback.OnResolveCompleted', e) then raise;
    end;
  finally
    FEvents := nil;
  end;
end;

end.
