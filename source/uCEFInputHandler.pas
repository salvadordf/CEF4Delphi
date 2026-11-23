unit uCEFInputHandler;

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
  TCefInputHandlerOwn = class(TCefBaseRefCountedOwn, ICefInputHandler)
    protected
      function OnPreKeyboardEvent(const browser: ICefBrowser; const event: PCefKeyboardInputEvent; const context: PCefInputEventContext; out isKeyboardShortcut: Boolean): Boolean; virtual;
      function OnKeyboardEvent(const browser: ICefBrowser; const event: PCefKeyboardInputEvent; const context: PCefInputEventContext): Boolean; virtual;
      function OnPreMouseEvent(const browser: ICefBrowser; const event: PCefMouseInputEvent): Boolean; virtual;
      function OnPreGestureEvent(const browser: ICefBrowser; const event: PCefGestureInputEvent): Boolean; virtual;

      procedure RemoveReferences; virtual;

    public
      constructor Create; virtual;
  end;

  TCustomInputHandler = class(TCefInputHandlerOwn)
    protected
      FEvents : Pointer;

      function OnPreKeyboardEvent(const browser: ICefBrowser; const event: PCefKeyboardInputEvent; const context: PCefInputEventContext; out isKeyboardShortcut: Boolean): Boolean; override;
      function OnKeyboardEvent(const browser: ICefBrowser; const event: PCefKeyboardInputEvent; const context: PCefInputEventContext): Boolean; override;
      function OnPreMouseEvent(const browser: ICefBrowser; const event: PCefMouseInputEvent): Boolean; override;
      function OnPreGestureEvent(const browser: ICefBrowser; const event: PCefGestureInputEvent): Boolean; override;

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
  uCEFMiscFunctions, uCEFBrowser;

function cef_input_handler_on_pre_keyboard_event(      self                 : PCefInputHandler;
                                                       browser              : PCefBrowser;
                                                 const event                : PCefKeyboardInputEvent;
                                                 const context              : PCefInputEventContext;
                                                       is_keyboard_shortcut : PInteger): integer; stdcall;
var
  TempShortcut : boolean;
  TempObject   : TObject;
  TempResult   : boolean;
begin
  TempResult   := False;
  TempShortcut := is_keyboard_shortcut^ <> 0;
  TempObject   := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefInputHandlerOwn) then
    TempResult := TCefInputHandlerOwn(TempObject).OnPreKeyboardEvent(TCefBrowserRef.UnWrap(browser),
                                                                     event,
                                                                     context,
                                                                     TempShortcut);

  is_keyboard_shortcut^ := Ord(TempShortcut);
  Result                := Ord(TempResult);
end;

function cef_input_handler_on_keyboard_event(      self    : PCefInputHandler;
                                                   browser : PCefBrowser;
                                             const event   : PCefKeyboardInputEvent;
                                             const context : PCefInputEventContext): integer; stdcall;
var
  TempObject : TObject;
  TempResult : boolean;
begin
  TempResult := False;
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefInputHandlerOwn) then
    TempResult := TCefInputHandlerOwn(TempObject).OnKeyboardEvent(TCefBrowserRef.UnWrap(browser),
                                                                  event,
                                                                  context);

  Result := Ord(TempResult);
end;

function cef_input_handler_on_pre_mouse_event(self: PCefInputHandler; browser: PCefBrowser; const event: PCefMouseInputEvent): integer; stdcall;
var
  TempObject   : TObject;
  TempResult   : boolean;
begin
  TempResult   := False;
  TempObject   := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefInputHandlerOwn) then
    TempResult := TCefInputHandlerOwn(TempObject).OnPreMouseEvent(TCefBrowserRef.UnWrap(browser),
                                                                  event);

  Result := Ord(TempResult);
end;

function cef_input_handler_on_pre_gesture_event(self: PCefInputHandler; browser: PCefBrowser; const event: PCefGestureInputEvent): integer; stdcall;
var
  TempObject   : TObject;
  TempResult   : boolean;
begin
  TempResult   := False;
  TempObject   := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefInputHandlerOwn) then
    TempResult := TCefInputHandlerOwn(TempObject).OnPreGestureEvent(TCefBrowserRef.UnWrap(browser),
                                                                    event);

  Result := Ord(TempResult);
end;

{ TCefInputHandlerOwn }

constructor TCefInputHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefInputHandler));

  with PCefInputHandler(FData)^ do
    begin
      on_pre_keyboard_event := {$IFDEF FPC}@{$ENDIF}cef_input_handler_on_pre_keyboard_event;
      on_keyboard_event     := {$IFDEF FPC}@{$ENDIF}cef_input_handler_on_keyboard_event;
      on_pre_mouse_event    := {$IFDEF FPC}@{$ENDIF}cef_input_handler_on_pre_mouse_event;
      on_pre_gesture_event  := {$IFDEF FPC}@{$ENDIF}cef_input_handler_on_pre_gesture_event;
    end;
end;

function TCefInputHandlerOwn.OnKeyboardEvent(const browser: ICefBrowser;
  const event: PCefKeyboardInputEvent;
  const context: PCefInputEventContext): Boolean;
begin
  Result := False;
end;

function TCefInputHandlerOwn.OnPreGestureEvent(const browser: ICefBrowser;
  const event: PCefGestureInputEvent): Boolean;
begin
  Result := False;
end;

function TCefInputHandlerOwn.OnPreKeyboardEvent(const browser: ICefBrowser;
  const event: PCefKeyboardInputEvent; const context: PCefInputEventContext;
  out isKeyboardShortcut: Boolean): Boolean;
begin
  Result := False;
end;

function TCefInputHandlerOwn.OnPreMouseEvent(const browser: ICefBrowser;
  const event: PCefMouseInputEvent): Boolean;
begin
  Result := False;
end;

procedure TCefInputHandlerOwn.RemoveReferences;
begin
  //
end;

{ TCustomInputHandler }

constructor TCustomInputHandler.Create(const events: IChromiumEvents);
begin
  inherited Create;

  FEvents := Pointer(events);
end;

destructor TCustomInputHandler.Destroy;
begin
  RemoveReferences;

  inherited Destroy;
end;

procedure TCustomInputHandler.RemoveReferences;
begin
  FEvents := nil;
end;

function TCustomInputHandler.OnKeyboardEvent(const browser : ICefBrowser;
                                             const event   : PCefKeyboardInputEvent;
                                             const context : PCefInputEventContext): Boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnKeyboardEvent(browser, event, context)
   else
    Result := inherited OnKeyboardEvent(browser, event, context);
end;

function TCustomInputHandler.OnPreGestureEvent(const browser : ICefBrowser;
                                               const event   : PCefGestureInputEvent): Boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnPreGestureEvent(browser, event)
   else
    Result := inherited OnPreGestureEvent(browser, event);
end;

function TCustomInputHandler.OnPreKeyboardEvent(const browser            : ICefBrowser;
                                                const event              : PCefKeyboardInputEvent;
                                                const context            : PCefInputEventContext;
                                                out   isKeyboardShortcut : Boolean): Boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnPreKeyboardEvent(browser, event, context, isKeyboardShortcut)
   else
    Result := inherited OnPreKeyboardEvent(browser, event, context, isKeyboardShortcut);
end;

function TCustomInputHandler.OnPreMouseEvent(const browser : ICefBrowser;
                                             const event   : PCefMouseInputEvent): Boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnPreMouseEvent(browser, event)
   else
    Result := inherited OnPreMouseEvent(browser, event);
end;

end.
