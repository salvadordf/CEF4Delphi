unit uCEFCommandHandler;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefCommandHandlerOwn = class(TCefBaseRefCountedOwn, ICefCommandHandler)
    protected
      function  OnChromeCommand(const browser: ICefBrowser; command_id: integer; disposition: TCefWindowOpenDisposition): boolean; virtual;
      function  OnIsChromeAppMenuItemVisible(const browser: ICefBrowser; command_id: integer): boolean; virtual;
      function  OnIsChromeAppMenuItemEnabled(const browser: ICefBrowser; command_id: integer): boolean; virtual;
      function  OnIsChromePageActionIconVisible(icon_type: TCefChromePageActionIconType): boolean; virtual;
      function  OnIsChromeToolbarButtonVisible(button_type: TCefChromeToolbarButtonType): boolean; virtual;

      procedure RemoveReferences; virtual;

    public
      constructor Create; virtual;
  end;

  TCustomCommandHandler = class(TCefCommandHandlerOwn)
    protected
      FEvents : Pointer;

      function  OnChromeCommand(const browser: ICefBrowser; command_id: integer; disposition: TCefWindowOpenDisposition): boolean; override;
      function  OnIsChromeAppMenuItemVisible(const browser: ICefBrowser; command_id: integer): boolean; override;
      function  OnIsChromeAppMenuItemEnabled(const browser: ICefBrowser; command_id: integer): boolean; override;
      function  OnIsChromePageActionIconVisible(icon_type: TCefChromePageActionIconType): boolean; override;
      function  OnIsChromeToolbarButtonVisible(button_type: TCefChromeToolbarButtonType): boolean; override;

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

function cef_command_handler_on_chrome_command(self        : PCefCommandHandler;
                                               browser     : PCefBrowser;
                                               command_id  : integer;
                                               disposition : TCefWindowOpenDisposition): Integer; stdcall;
var
  TempObject : TObject;
begin
  Result     := Ord(False);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefCommandHandlerOwn) then
    Result := Ord(TCefCommandHandlerOwn(TempObject).OnChromeCommand(TCefBrowserRef.UnWrap(browser),
                                                                    command_id,
                                                                    disposition));
end;

function cef_command_handler_is_chrome_app_menu_item_visible(self       : PCefCommandHandler;
                                                             browser    : PCefBrowser;
                                                             command_id : integer): integer; stdcall;
var
  TempObject : TObject;
begin
  Result     := Ord(True);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefCommandHandlerOwn) then
    Result := Ord(TCefCommandHandlerOwn(TempObject).OnIsChromeAppMenuItemVisible(TCefBrowserRef.UnWrap(browser),
                                                                                 command_id));
end;

function cef_command_handler_is_chrome_app_menu_item_enabled(self       : PCefCommandHandler;
                                                             browser    : PCefBrowser;
                                                             command_id : integer): integer; stdcall;
var
  TempObject : TObject;
begin
  Result     := Ord(True);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefCommandHandlerOwn) then
    Result := Ord(TCefCommandHandlerOwn(TempObject).OnIsChromeAppMenuItemEnabled(TCefBrowserRef.UnWrap(browser),
                                                                                 command_id));
end;

function cef_command_handler_is_chrome_page_action_icon_visible(self      : PCefCommandHandler;
                                                                icon_type : TCefChromePageActionIconType): integer; stdcall;
var
  TempObject : TObject;
begin
  Result     := Ord(True);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefCommandHandlerOwn) then
    Result := Ord(TCefCommandHandlerOwn(TempObject).OnIsChromePageActionIconVisible(icon_type));
end;

function cef_command_handler_is_chrome_toolbar_button_visible(self        : PCefCommandHandler;
                                                              button_type : TCefChromeToolbarButtonType): integer; stdcall;
var
  TempObject : TObject;
begin
  Result     := Ord(True);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefCommandHandlerOwn) then
    Result := Ord(TCefCommandHandlerOwn(TempObject).OnIsChromeToolbarButtonVisible(button_type));
end;

constructor TCefCommandHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefCommandHandler));

  with PCefCommandHandler(FData)^ do
    begin
      on_chrome_command                  := {$IFDEF FPC}@{$ENDIF}cef_command_handler_on_chrome_command;
      is_chrome_app_menu_item_visible    := {$IFDEF FPC}@{$ENDIF}cef_command_handler_is_chrome_app_menu_item_visible;
      is_chrome_app_menu_item_enabled    := {$IFDEF FPC}@{$ENDIF}cef_command_handler_is_chrome_app_menu_item_enabled;
      is_chrome_page_action_icon_visible := {$IFDEF FPC}@{$ENDIF}cef_command_handler_is_chrome_page_action_icon_visible;
      is_chrome_toolbar_button_visible   := {$IFDEF FPC}@{$ENDIF}cef_command_handler_is_chrome_toolbar_button_visible;
    end;
end;

function TCefCommandHandlerOwn.OnChromeCommand(const browser: ICefBrowser; command_id: integer; disposition: TCefWindowOpenDisposition): boolean;
begin
  Result := False;
end;

function TCefCommandHandlerOwn.OnIsChromeAppMenuItemVisible(const browser: ICefBrowser; command_id: integer): boolean;
begin
  Result := True;
end;

function TCefCommandHandlerOwn.OnIsChromeAppMenuItemEnabled(const browser: ICefBrowser; command_id: integer): boolean;
begin
  Result := True;
end;

function TCefCommandHandlerOwn.OnIsChromePageActionIconVisible(icon_type: TCefChromePageActionIconType): boolean;
begin
  Result := True;
end;

function TCefCommandHandlerOwn.OnIsChromeToolbarButtonVisible(button_type: TCefChromeToolbarButtonType): boolean;
begin
  Result := True;
end;

procedure TCefCommandHandlerOwn.RemoveReferences;
begin
  //
end;


// TCustomCommandHandler

constructor TCustomCommandHandler.Create(const events : IChromiumEvents);
begin
  inherited Create;

  FEvents := Pointer(events);
end;

destructor TCustomCommandHandler.Destroy;
begin
  RemoveReferences;

  inherited Destroy;
end;

procedure TCustomCommandHandler.RemoveReferences;
begin
  FEvents := nil;
end;

function TCustomCommandHandler.OnChromeCommand(const browser: ICefBrowser; command_id: integer; disposition: TCefWindowOpenDisposition): boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnChromeCommand(browser, command_id, disposition)
   else
    Result := inherited OnChromeCommand(browser, command_id, disposition);
end;

function TCustomCommandHandler.OnIsChromeAppMenuItemVisible(const browser: ICefBrowser; command_id: integer): boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnIsChromeAppMenuItemVisible(browser, command_id)
   else
    Result := inherited OnIsChromeAppMenuItemVisible(browser, command_id);
end;

function TCustomCommandHandler.OnIsChromeAppMenuItemEnabled(const browser: ICefBrowser; command_id: integer): boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnIsChromeAppMenuItemEnabled(browser, command_id)
   else
    Result := inherited OnIsChromeAppMenuItemEnabled(browser, command_id);
end;

function TCustomCommandHandler.OnIsChromePageActionIconVisible(icon_type: TCefChromePageActionIconType): boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnIsChromePageActionIconVisible(icon_type)
   else
    Result := inherited OnIsChromePageActionIconVisible(icon_type);
end;

function TCustomCommandHandler.OnIsChromeToolbarButtonVisible(button_type: TCefChromeToolbarButtonType): boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnIsChromeToolbarButtonVisible(button_type)
   else
    Result := inherited OnIsChromeToolbarButtonVisible(button_type);
end;

end.
