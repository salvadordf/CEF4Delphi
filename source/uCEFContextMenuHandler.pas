unit uCEFContextMenuHandler;

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
  TCefContextMenuHandlerOwn = class(TCefBaseRefCountedOwn, ICefContextMenuHandler)
    protected
      procedure OnBeforeContextMenu(const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; const model: ICefMenuModel); virtual;
      function  RunContextMenu(const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; const model: ICefMenuModel; const callback: ICefRunContextMenuCallback): Boolean; virtual;
      function  OnContextMenuCommand(const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; commandId: Integer; eventFlags: TCefEventFlags): Boolean; virtual;
      procedure OnContextMenuDismissed(const browser: ICefBrowser; const frame: ICefFrame); virtual;
      function  RunQuickMenu(const browser: ICefBrowser; const frame: ICefFrame; location: PCefPoint; size: PCefSize; edit_state_flags: TCefQuickMenuEditStateFlags; const callback: ICefRunQuickMenuCallback): boolean; virtual;
      function  OnQuickMenuCommand(const browser: ICefBrowser; const frame: ICefFrame; command_id: integer; event_flags: TCefEventFlags): boolean; virtual;
      procedure OnQuickMenuDismissed(const browser: ICefBrowser; const frame: ICefFrame); virtual;

      procedure RemoveReferences; virtual;

    public
      constructor Create; virtual;
  end;

  TCustomContextMenuHandler = class(TCefContextMenuHandlerOwn)
    protected
      FEvents : Pointer;

      procedure OnBeforeContextMenu(const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; const model: ICefMenuModel); override;
      function  RunContextMenu(const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; const model: ICefMenuModel; const callback: ICefRunContextMenuCallback): Boolean; override;
      function  OnContextMenuCommand(const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; commandId: Integer; eventFlags: TCefEventFlags): Boolean; override;
      procedure OnContextMenuDismissed(const browser: ICefBrowser; const frame: ICefFrame); override;
      function  RunQuickMenu(const browser: ICefBrowser; const frame: ICefFrame; location: PCefPoint; size: PCefSize; edit_state_flags: TCefQuickMenuEditStateFlags; const callback: ICefRunQuickMenuCallback): boolean; override;
      function  OnQuickMenuCommand(const browser: ICefBrowser; const frame: ICefFrame; command_id: integer; event_flags: TCefEventFlags): boolean; override;
      procedure OnQuickMenuDismissed(const browser: ICefBrowser; const frame: ICefFrame); override;

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
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBrowser, uCEFFrame, uCEFContextMenuParams,
  uCEFMenuModel, uCEFRunContextMenuCallback, uCEFRunQuickMenuCallback;

procedure cef_context_menu_handler_on_before_context_menu(self    : PCefContextMenuHandler;
                                                          browser : PCefBrowser;
                                                          frame   : PCefFrame;
                                                          params  : PCefContextMenuParams;
                                                          model   : PCefMenuModel); stdcall;
var
  TempObject  : TObject;
begin
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefContextMenuHandlerOwn) then
    TCefContextMenuHandlerOwn(TempObject).OnBeforeContextMenu(TCefBrowserRef.UnWrap(browser),
                                                              TCefFrameRef.UnWrap(frame),
                                                              TCefContextMenuParamsRef.UnWrap(params),
                                                              TCefMenuModelRef.UnWrap(model));
end;

function cef_context_menu_handler_run_context_menu(self     : PCefContextMenuHandler;
                                                   browser  : PCefBrowser;
                                                   frame    : PCefFrame;
                                                   params   : PCefContextMenuParams;
                                                   model    : PCefMenuModel;
                                                   callback : PCefRunContextMenuCallback): Integer; stdcall;
var
  TempObject  : TObject;
begin
  Result      := Ord(False);
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefContextMenuHandlerOwn) then
    Result := Ord(TCefContextMenuHandlerOwn(TempObject).RunContextMenu(TCefBrowserRef.UnWrap(browser),
                                                                       TCefFrameRef.UnWrap(frame),
                                                                       TCefContextMenuParamsRef.UnWrap(params),
                                                                       TCefMenuModelRef.UnWrap(model),
                                                                       TCefRunContextMenuCallbackRef.UnWrap(callback)));
end;

function cef_context_menu_handler_on_context_menu_command(self        : PCefContextMenuHandler;
                                                          browser     : PCefBrowser;
                                                          frame       : PCefFrame;
                                                          params      : PCefContextMenuParams;
                                                          command_id  : Integer;
                                                          event_flags : TCefEventFlags): Integer; stdcall;
var
  TempObject  : TObject;
begin
  Result      := Ord(False);
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefContextMenuHandlerOwn) then
    Result := Ord(TCefContextMenuHandlerOwn(TempObject).OnContextMenuCommand(TCefBrowserRef.UnWrap(browser),
                                                                             TCefFrameRef.UnWrap(frame),
                                                                             TCefContextMenuParamsRef.UnWrap(params),
                                                                             command_id,
                                                                             event_flags));
end;

procedure cef_context_menu_handler_on_context_menu_dismissed(self    : PCefContextMenuHandler;
                                                             browser : PCefBrowser;
                                                             frame   : PCefFrame); stdcall;
var
  TempObject  : TObject;
begin
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefContextMenuHandlerOwn) then
    TCefContextMenuHandlerOwn(TempObject).OnContextMenuDismissed(TCefBrowserRef.UnWrap(browser),
                                                                 TCefFrameRef.UnWrap(frame));
end;

function cef_context_menu_handler_run_quick_menu(self                : PCefContextMenuHandler;
                                                 browser             : PCefBrowser;
                                                 frame               : PCefFrame;
                                                 location            : PCefPoint;
                                                 size                : PCefSize;
                                                 edit_state_flags    : TCefQuickMenuEditStateFlags;
                                                 callback            : PCefRunQuickMenuCallback): Integer; stdcall;
var
  TempObject  : TObject;
begin
  Result      := Ord(False);
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefContextMenuHandlerOwn) then
    Result := Ord(TCefContextMenuHandlerOwn(TempObject).RunQuickMenu(TCefBrowserRef.UnWrap(browser),
                                                                     TCefFrameRef.UnWrap(frame),
                                                                     location,
                                                                     size,
                                                                     edit_state_flags,
                                                                     TCefRunQuickMenuCallbackRef.UnWrap(callback)));
end;

function cef_context_menu_handler_on_quick_menu_command(self        : PCefContextMenuHandler;
                                                        browser     : PCefBrowser;
                                                        frame       : PCefFrame;
                                                        command_id  : Integer;
                                                        event_flags : TCefEventFlags): Integer; stdcall;
var
  TempObject  : TObject;
begin
  Result      := Ord(False);
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefContextMenuHandlerOwn) then
    Result := Ord(TCefContextMenuHandlerOwn(TempObject).OnQuickMenuCommand(TCefBrowserRef.UnWrap(browser),
                                                                           TCefFrameRef.UnWrap(frame),
                                                                           command_id,
                                                                           event_flags));
end;

procedure cef_context_menu_handler_on_quick_menu_dismissed(self    : PCefContextMenuHandler;
                                                           browser : PCefBrowser;
                                                           frame   : PCefFrame); stdcall;
var
  TempObject  : TObject;
begin
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefContextMenuHandlerOwn) then
    TCefContextMenuHandlerOwn(TempObject).OnQuickMenuDismissed(TCefBrowserRef.UnWrap(browser),
                                                               TCefFrameRef.UnWrap(frame));
end;

constructor TCefContextMenuHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefContextMenuHandler));

  with PCefContextMenuHandler(FData)^ do
    begin
      on_before_context_menu    := {$IFDEF FPC}@{$ENDIF}cef_context_menu_handler_on_before_context_menu;
      run_context_menu          := {$IFDEF FPC}@{$ENDIF}cef_context_menu_handler_run_context_menu;
      on_context_menu_command   := {$IFDEF FPC}@{$ENDIF}cef_context_menu_handler_on_context_menu_command;
      on_context_menu_dismissed := {$IFDEF FPC}@{$ENDIF}cef_context_menu_handler_on_context_menu_dismissed;
      run_quick_menu            := {$IFDEF FPC}@{$ENDIF}cef_context_menu_handler_run_quick_menu;
      on_quick_menu_command     := {$IFDEF FPC}@{$ENDIF}cef_context_menu_handler_on_quick_menu_command;
      on_quick_menu_dismissed   := {$IFDEF FPC}@{$ENDIF}cef_context_menu_handler_on_quick_menu_dismissed;
    end;
end;

procedure TCefContextMenuHandlerOwn.OnBeforeContextMenu(const browser : ICefBrowser;
                                                        const frame   : ICefFrame;
                                                        const params  : ICefContextMenuParams;
                                                        const model   : ICefMenuModel);
begin
  //
end;

function TCefContextMenuHandlerOwn.OnContextMenuCommand(const browser    : ICefBrowser;
                                                        const frame      : ICefFrame;
                                                        const params     : ICefContextMenuParams;
                                                              commandId  : Integer;
                                                              eventFlags : TCefEventFlags): Boolean;
begin
  Result := False;
end;

procedure TCefContextMenuHandlerOwn.OnContextMenuDismissed(const browser : ICefBrowser;
                                                           const frame   : ICefFrame);
begin
  //
end;

function TCefContextMenuHandlerOwn.RunQuickMenu(const browser          : ICefBrowser;
                                                const frame            : ICefFrame;
                                                      location         : PCefPoint;
                                                      size             : PCefSize;
                                                      edit_state_flags : TCefQuickMenuEditStateFlags;
                                                const callback         : ICefRunQuickMenuCallback): boolean;
begin
  Result := False;
end;

function TCefContextMenuHandlerOwn.OnQuickMenuCommand(const browser     : ICefBrowser;
                                                      const frame       : ICefFrame;
                                                            command_id  : integer;
                                                            event_flags : TCefEventFlags): boolean;
begin
  Result := False;
end;

procedure TCefContextMenuHandlerOwn.OnQuickMenuDismissed(const browser : ICefBrowser;
                                                         const frame   : ICefFrame);
begin
  //
end;

function TCefContextMenuHandlerOwn.RunContextMenu(const browser  : ICefBrowser;
                                                  const frame    : ICefFrame;
                                                  const params   : ICefContextMenuParams;
                                                  const model    : ICefMenuModel;
                                                  const callback : ICefRunContextMenuCallback): Boolean;
begin
  Result := False;
end;

procedure TCefContextMenuHandlerOwn.RemoveReferences;
begin
  //
end;

// TCustomContextMenuHandler

constructor TCustomContextMenuHandler.Create(const events : IChromiumEvents);
begin
  inherited Create;

  FEvents := Pointer(events);
end;

destructor TCustomContextMenuHandler.Destroy;
begin
  RemoveReferences;

  inherited Destroy;
end;

procedure TCustomContextMenuHandler.RemoveReferences;
begin
  FEvents := nil;
end;

procedure TCustomContextMenuHandler.OnBeforeContextMenu(const browser : ICefBrowser;
                                                        const frame   : ICefFrame;
                                                        const params  : ICefContextMenuParams;
                                                        const model   : ICefMenuModel);
begin
  if (FEvents <> nil) then IChromiumEvents(FEvents).doOnBeforeContextMenu(browser, frame, params, model);
end;

function TCustomContextMenuHandler.RunContextMenu(const browser  : ICefBrowser;
                                                  const frame    : ICefFrame;
                                                  const params   : ICefContextMenuParams;
                                                  const model    : ICefMenuModel;
                                                  const callback : ICefRunContextMenuCallback): Boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doRunContextMenu(browser, frame, params, model, callback)
   else
    Result := inherited RunContextMenu(browser, frame, params, model, callback);
end;

function TCustomContextMenuHandler.OnContextMenuCommand(const browser    : ICefBrowser;
                                                        const frame      : ICefFrame;
                                                        const params     : ICefContextMenuParams;
                                                              commandId  : Integer;
                                                              eventFlags : TCefEventFlags): Boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnContextMenuCommand(browser, frame, params, commandId, eventFlags)
   else
    Result := inherited OnContextMenuCommand(browser, frame, params, commandId, eventFlags);
end;

procedure TCustomContextMenuHandler.OnContextMenuDismissed(const browser : ICefBrowser;
                                                           const frame   : ICefFrame);
begin
  if (FEvents <> nil) then IChromiumEvents(FEvents).doOnContextMenuDismissed(browser, frame);
end;

function TCustomContextMenuHandler.RunQuickMenu(const browser          : ICefBrowser;
                                                const frame            : ICefFrame;
                                                      location         : PCefPoint;
                                                      size             : PCefSize;
                                                      edit_state_flags : TCefQuickMenuEditStateFlags;
                                                const callback         : ICefRunQuickMenuCallback): boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doRunQuickMenu(browser, frame, location, size, edit_state_flags, callback)
   else
    Result := inherited RunQuickMenu(browser, frame, location, size, edit_state_flags, callback);
end;

function TCustomContextMenuHandler.OnQuickMenuCommand(const browser     : ICefBrowser;
                                                      const frame       : ICefFrame;
                                                            command_id  : integer;
                                                            event_flags : TCefEventFlags): boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnQuickMenuCommand(browser, frame, command_id, event_flags)
   else
    Result := inherited OnQuickMenuCommand(browser, frame, command_id, event_flags);
end;

procedure TCustomContextMenuHandler.OnQuickMenuDismissed(const browser : ICefBrowser;
                                                         const frame   : ICefFrame);
begin
  if (FEvents <> nil) then IChromiumEvents(FEvents).doOnQuickMenuDismissed(browser, frame);
end;

end.
