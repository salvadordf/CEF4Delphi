unit uCEFFocusHandler;

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
  TCefFocusHandlerOwn = class(TCefBaseRefCountedOwn, ICefFocusHandler)
    protected
      procedure OnTakeFocus(const browser: ICefBrowser; next: Boolean); virtual;
      function  OnSetFocus(const browser: ICefBrowser; source: TCefFocusSource): Boolean; virtual;
      procedure OnGotFocus(const browser: ICefBrowser); virtual;

      procedure RemoveReferences; virtual;

    public
      constructor Create; virtual;
  end;

  TCustomFocusHandler = class(TCefFocusHandlerOwn)
    protected
      FEvents : Pointer;

      procedure OnTakeFocus(const browser: ICefBrowser; next: Boolean); override;
      function  OnSetFocus(const browser: ICefBrowser; source: TCefFocusSource): Boolean; override;
      procedure OnGotFocus(const browser: ICefBrowser); override;

      procedure RemoveReferences; override;

    public
      constructor Create(const events : IChromiumEvents); reintroduce; virtual;
      destructor  Destroy; override;
  end;

implementation

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBrowser;

procedure cef_focus_handler_on_take_focus(self    : PCefFocusHandler;
                                          browser : PCefBrowser;
                                          next    : Integer); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefFocusHandlerOwn) then
    TCefFocusHandlerOwn(TempObject).OnTakeFocus(TCefBrowserRef.UnWrap(browser),
                                                next <> 0);
end;

function cef_focus_handler_on_set_focus(self    : PCefFocusHandler;
                                        browser : PCefBrowser;
                                        source  : TCefFocusSource): Integer; stdcall;
var
  TempObject : TObject;
begin
  Result     := Ord(False);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefFocusHandlerOwn) then
    Result := Ord(TCefFocusHandlerOwn(TempObject).OnSetFocus(TCefBrowserRef.UnWrap(browser),
                                                             source))
end;

procedure cef_focus_handler_on_got_focus(self    : PCefFocusHandler;
                                         browser : PCefBrowser); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefFocusHandlerOwn) then
    TCefFocusHandlerOwn(TempObject).OnGotFocus(TCefBrowserRef.UnWrap(browser));
end;

constructor TCefFocusHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefFocusHandler));

  with PCefFocusHandler(FData)^ do
    begin
      on_take_focus := {$IFDEF FPC}@{$ENDIF}cef_focus_handler_on_take_focus;
      on_set_focus  := {$IFDEF FPC}@{$ENDIF}cef_focus_handler_on_set_focus;
      on_got_focus  := {$IFDEF FPC}@{$ENDIF}cef_focus_handler_on_got_focus;
    end;
end;

function TCefFocusHandlerOwn.OnSetFocus(const browser: ICefBrowser; source: TCefFocusSource): Boolean;
begin
  Result := False;
end;

procedure TCefFocusHandlerOwn.OnGotFocus(const browser: ICefBrowser);
begin
  //
end;

procedure TCefFocusHandlerOwn.OnTakeFocus(const browser: ICefBrowser; next: Boolean);
begin
  //
end;

procedure TCefFocusHandlerOwn.RemoveReferences;
begin
  //
end;

// TCustomFocusHandler

constructor TCustomFocusHandler.Create(const events : IChromiumEvents);
begin
  inherited Create;

  FEvents := Pointer(events);
end;

destructor TCustomFocusHandler.Destroy;
begin
  RemoveReferences;

  inherited Destroy;
end;

procedure TCustomFocusHandler.RemoveReferences;
begin
  FEvents := nil;
end;

procedure TCustomFocusHandler.OnGotFocus(const browser: ICefBrowser);
begin
  if (FEvents <> nil) then IChromiumEvents(FEvents).doOnGotFocus(browser);
end;

function TCustomFocusHandler.OnSetFocus(const browser: ICefBrowser; source: TCefFocusSource): Boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnSetFocus(browser, source)
   else
    Result := inherited OnSetFocus(browser, source);
end;

procedure TCustomFocusHandler.OnTakeFocus(const browser: ICefBrowser; next: Boolean);
begin
  if (FEvents <> nil) then IChromiumEvents(FEvents).doOnTakeFocus(browser, next);
end;

end.

