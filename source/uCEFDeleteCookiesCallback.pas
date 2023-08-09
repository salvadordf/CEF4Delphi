unit uCEFDeleteCookiesCallback;

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
  TCefDeleteCookiesCallbackOwn = class(TCefBaseRefCountedOwn, ICefDeleteCookiesCallback)
    protected
      procedure OnComplete(numDeleted: Integer); virtual; abstract;

    public
      constructor Create; virtual;
  end;

  TCefFastDeleteCookiesCallback = class(TCefDeleteCookiesCallbackOwn)
    protected
      FCallback: TCefDeleteCookiesCallbackProc;

      procedure OnComplete(numDeleted: Integer); override;

    public
      constructor Create(const callback: TCefDeleteCookiesCallbackProc); reintroduce;
      destructor  Destroy; override;
  end;

  TCefCustomDeleteCookiesCallback = class(TCefDeleteCookiesCallbackOwn)
    protected
      FEvents : Pointer;

      procedure OnComplete(numDeleted: Integer); override;

    public
      constructor Create(const aEvents : IChromiumEvents); reintroduce;
      destructor  Destroy; override;
  end;

implementation

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  uCEFMiscFunctions, uCEFLibFunctions;

procedure cef_delete_cookie_callback_on_complete(self: PCefDeleteCookiesCallback; num_deleted: Integer); stdcall;
var
  TempObject  : TObject;
begin
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefDeleteCookiesCallbackOwn) then
    TCefDeleteCookiesCallbackOwn(TempObject).OnComplete(num_deleted);
end;

// TCefDeleteCookiesCallbackOwn

constructor TCefDeleteCookiesCallbackOwn.Create;
begin
  inherited CreateData(SizeOf(TCefDeleteCookiesCallback));

  PCefDeleteCookiesCallback(FData)^.on_complete := {$IFDEF FPC}@{$ENDIF}cef_delete_cookie_callback_on_complete;
end;

// TCefFastDeleteCookiesCallback

constructor TCefFastDeleteCookiesCallback.Create(const callback: TCefDeleteCookiesCallbackProc);
begin
  inherited Create;

  FCallback := callback;
end;

procedure TCefFastDeleteCookiesCallback.OnComplete(numDeleted: Integer);
begin
  if assigned(FCallback) then FCallback(numDeleted)
end;

destructor TCefFastDeleteCookiesCallback.Destroy;
begin
  FCallback := nil;

  inherited Destroy;
end;

// TCefCustomDeleteCookiesCallback

constructor TCefCustomDeleteCookiesCallback.Create(const aEvents : IChromiumEvents);
begin
  inherited Create;

  FEvents := Pointer(aEvents);
end;

destructor TCefCustomDeleteCookiesCallback.Destroy;
begin
  FEvents := nil;

  inherited Destroy;
end;

procedure TCefCustomDeleteCookiesCallback.OnComplete(numDeleted: Integer);
begin
  try
    try
      if (FEvents <> nil) then IChromiumEvents(FEvents).doCookiesDeleted(numDeleted);
    except
      on e : exception do
        if CustomExceptionHandler('TCefCustomDeleteCookiesCallback.OnComplete', e) then raise;
    end;
  finally
    FEvents := nil;
  end;
end;

end.
