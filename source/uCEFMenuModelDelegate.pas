unit uCEFMenuModelDelegate;

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
  TCefMenuModelDelegateOwn = class(TCefBaseRefCountedOwn, ICefMenuModelDelegate)
  protected
    procedure ExecuteCommand(const menuModel: ICefMenuModel; commandId: Integer; eventFlags: TCefEventFlags); virtual;
    procedure MouseOutsideMenu(const menuModel: ICefMenuModel; const screenPoint: PCefPoint); virtual;
    procedure UnhandledOpenSubmenu(const menuModel: ICefMenuModel; isRTL: boolean); virtual;
    procedure UnhandledCloseSubmenu(const menuModel: ICefMenuModel; isRTL: boolean); virtual;
    procedure MenuWillShow(const menuModel: ICefMenuModel); virtual;
    procedure MenuClosed(const menuModel: ICefMenuModel); virtual;
    function  FormatLabel(const menuModel: ICefMenuModel; var label_ : ustring) : boolean; virtual;
  public
    constructor Create; virtual;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFMenuModel;

procedure cef_menu_model_delegate_execute_command(self        : PCefMenuModelDelegate;
                                                  menu_model  : PCefMenuModel;
                                                  command_id  : Integer;
                                                  event_flags : TCefEventFlags); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefMenuModelDelegateOwn) then
    TCefMenuModelDelegateOwn(TempObject).ExecuteCommand(TCefMenuModelRef.UnWrap(menu_model),
                                                        command_id,
                                                        event_flags);
end;

procedure cef_menu_model_delegate_mouse_outside_menu(      self         : PCefMenuModelDelegate;
                                                           menu_model   : PCefMenuModel;
                                                     const screen_point : PCefPoint); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefMenuModelDelegateOwn) then
    TCefMenuModelDelegateOwn(TempObject).MouseOutsideMenu(TCefMenuModelRef.UnWrap(menu_model),
                                                          screen_point);
end;

procedure cef_menu_model_delegate_unhandled_open_submenu(self       : PCefMenuModelDelegate;
                                                         menu_model : PCefMenuModel;
                                                         is_rtl     : integer); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefMenuModelDelegateOwn) then
    TCefMenuModelDelegateOwn(TempObject).UnhandledOpenSubmenu(TCefMenuModelRef.UnWrap(menu_model),
                                                              is_rtl <> 0);
end;

procedure cef_menu_model_delegate_unhandled_close_submenu(self       : PCefMenuModelDelegate;
                                                          menu_model : PCefMenuModel;
                                                          is_rtl     : integer); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefMenuModelDelegateOwn) then
    TCefMenuModelDelegateOwn(TempObject).UnhandledCloseSubmenu(TCefMenuModelRef.UnWrap(menu_model),
                                                               is_rtl <> 0);
end;

procedure cef_menu_model_delegate_menu_will_show(self       : PCefMenuModelDelegate;
                                                 menu_model : PCefMenuModel); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefMenuModelDelegateOwn) then
    TCefMenuModelDelegateOwn(TempObject).MenuWillShow(TCefMenuModelRef.UnWrap(menu_model));
end;

procedure cef_menu_model_delegate_menu_closed(self: PCefMenuModelDelegate; menu_model: PCefMenuModel); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefMenuModelDelegateOwn) then
    TCefMenuModelDelegateOwn(TempObject).MenuClosed(TCefMenuModelRef.UnWrap(menu_model));
end;

function cef_menu_model_delegate_format_label(self       : PCefMenuModelDelegate;
                                              menu_model : PCefMenuModel;
                                              label_     : PCefString) : integer; stdcall;
var
  TempObject : TObject;
  TempLabel  : ustring;
begin
  Result     := Ord(False);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefMenuModelDelegateOwn) then
    begin
      TempLabel := CefStringClearAndGet(label_);
      Result    := Ord(TCefMenuModelDelegateOwn(TempObject).FormatLabel(TCefMenuModelRef.UnWrap(menu_model), TempLabel));
      if (label_ <> nil) then label_^ := CefStringAlloc(TempLabel);
    end;
end;

constructor TCefMenuModelDelegateOwn.Create;
begin
  inherited CreateData(SizeOf(TCefMenuModelDelegate));

  with PCefMenuModelDelegate(FData)^ do
    begin
      execute_command         := {$IFDEF FPC}@{$ENDIF}cef_menu_model_delegate_execute_command;
      mouse_outside_menu      := {$IFDEF FPC}@{$ENDIF}cef_menu_model_delegate_mouse_outside_menu;
      unhandled_open_submenu  := {$IFDEF FPC}@{$ENDIF}cef_menu_model_delegate_unhandled_open_submenu;
      unhandled_close_submenu := {$IFDEF FPC}@{$ENDIF}cef_menu_model_delegate_unhandled_close_submenu;
      menu_will_show          := {$IFDEF FPC}@{$ENDIF}cef_menu_model_delegate_menu_will_show;
      menu_closed             := {$IFDEF FPC}@{$ENDIF}cef_menu_model_delegate_menu_closed;
      format_label            := {$IFDEF FPC}@{$ENDIF}cef_menu_model_delegate_format_label;
    end;
end;

procedure TCefMenuModelDelegateOwn.ExecuteCommand(const menuModel  : ICefMenuModel;
                                                        commandId  : Integer;
                                                        eventFlags : TCefEventFlags);
begin
  //
end;

procedure TCefMenuModelDelegateOwn.MouseOutsideMenu(const menuModel: ICefMenuModel; const screenPoint: PCefPoint);
begin
  //
end;

procedure TCefMenuModelDelegateOwn.UnhandledOpenSubmenu(const menuModel: ICefMenuModel; isRTL: boolean);
begin
  //
end;

procedure TCefMenuModelDelegateOwn.UnhandledCloseSubmenu(const menuModel: ICefMenuModel; isRTL: boolean);
begin
  //
end;

procedure TCefMenuModelDelegateOwn.MenuWillShow(const menuModel: ICefMenuModel);
begin
  //
end;

procedure TCefMenuModelDelegateOwn.MenuClosed(const menuModel: ICefMenuModel);
begin
  //
end;

function TCefMenuModelDelegateOwn.FormatLabel(const menuModel: ICefMenuModel; var label_ : ustring) : boolean;
begin
  Result := False;
end;

end.
