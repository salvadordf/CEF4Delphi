unit uCEFPermissionHandler;

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
  TCefPermissionHandlerOwn = class(TCefBaseRefCountedOwn, ICefPermissionHandler)
    protected
      function  OnRequestMediaAccessPermission(const browser: ICefBrowser; const frame: ICefFrame; const requesting_origin: ustring; requested_permissions: cardinal; const callback: ICefMediaAccessCallback): boolean; virtual;
      function  OnShowPermissionPrompt(const browser: ICefBrowser; prompt_id: uint64; const requesting_origin: ustring; requested_permissions: cardinal; const callback: ICefPermissionPromptCallback): boolean; virtual;
      procedure OnDismissPermissionPrompt(const browser: ICefBrowser; prompt_id: uint64; result: TCefPermissionRequestResult); virtual;

      procedure RemoveReferences; virtual;

    public
      constructor Create; virtual;
  end;

  TCustomPermissionHandler = class(TCefPermissionHandlerOwn)
    protected
      FEvents : Pointer;

      function  OnRequestMediaAccessPermission(const browser: ICefBrowser; const frame: ICefFrame; const requesting_origin: ustring; requested_permissions: cardinal; const callback: ICefMediaAccessCallback): boolean; override;
      function  OnShowPermissionPrompt(const browser: ICefBrowser; prompt_id: uint64; const requesting_origin: ustring; requested_permissions: cardinal; const callback: ICefPermissionPromptCallback): boolean; override;
      procedure OnDismissPermissionPrompt(const browser: ICefBrowser; prompt_id: uint64; result: TCefPermissionRequestResult); override;

      procedure RemoveReferences; override;

    public
      constructor Create(const events : IChromiumEvents); reintroduce;
      destructor  Destroy; override;
  end;

implementation

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBrowser, uCEFFrame, uCEFPermissionPromptCallback, uCEFMediaAccessCallback;

function cef_permission_handler_on_request_media_access_permission(      self                  : PCefPermissionHandler;
                                                                         browser               : PCefBrowser;
                                                                         frame                 : PCefFrame;
                                                                   const requesting_origin     : PCefString;
                                                                         requested_permissions : cardinal;
                                                                         callback              : PCefMediaAccessCallback): integer; stdcall;
var
  TempObject : TObject;
begin
  Result     := Ord(False);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefPermissionHandlerOwn) then
    Result := Ord(TCefPermissionHandlerOwn(TempObject).OnRequestMediaAccessPermission(TCefBrowserRef.UnWrap(browser),
                                                                                      TCefFrameRef.UnWrap(frame),
                                                                                      CefString(requesting_origin),
                                                                                      requested_permissions,
                                                                                      TCefMediaAccessCallbackRef.UnWrap(callback)));
end;

function cef_permission_handler_on_show_permission_prompt(      self                  : PCefPermissionHandler;
                                                                browser               : PCefBrowser;
                                                                prompt_id             : uint64;
                                                          const requesting_origin     : PCefString;
                                                                requested_permissions : cardinal;
                                                                callback              : PCefPermissionPromptCallback): integer; stdcall;
var
  TempObject : TObject;
begin
  Result     := Ord(False);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefPermissionHandlerOwn) then
    Result := Ord(TCefPermissionHandlerOwn(TempObject).OnShowPermissionPrompt(TCefBrowserRef.UnWrap(browser),
                                                                              prompt_id,
                                                                              CefString(requesting_origin),
                                                                              requested_permissions,
                                                                              TCefPermissionPromptCallbackRef.UnWrap(callback)));
end;

procedure cef_permission_handler_on_dismiss_permission_prompt(self      : PCefPermissionHandler;
                                                              browser   : PCefBrowser;
                                                              prompt_id : uint64;
                                                              result    : TCefPermissionRequestResult); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefPermissionHandlerOwn) then
    TCefPermissionHandlerOwn(TempObject).OnDismissPermissionPrompt(TCefBrowserRef.UnWrap(browser),
                                                                   prompt_id,
                                                                   result);
end;


// TCefPermissionHandlerOwn

constructor TCefPermissionHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefPermissionHandler));

  with PCefPermissionHandler(FData)^ do
    begin
      on_request_media_access_permission := {$IFDEF FPC}@{$ENDIF}cef_permission_handler_on_request_media_access_permission;
      on_show_permission_prompt          := {$IFDEF FPC}@{$ENDIF}cef_permission_handler_on_show_permission_prompt;
      on_dismiss_permission_prompt       := {$IFDEF FPC}@{$ENDIF}cef_permission_handler_on_dismiss_permission_prompt;
    end;
end;

function TCefPermissionHandlerOwn.OnRequestMediaAccessPermission(const browser               : ICefBrowser;
                                                                 const frame                 : ICefFrame;
                                                                 const requesting_origin     : ustring;
                                                                       requested_permissions : cardinal;
                                                                 const callback              : ICefMediaAccessCallback): boolean;
begin
  Result := False;
end;

function TCefPermissionHandlerOwn.OnShowPermissionPrompt(const browser               : ICefBrowser;
                                                               prompt_id             : uint64;
                                                         const requesting_origin     : ustring;
                                                               requested_permissions : cardinal;
                                                         const callback              : ICefPermissionPromptCallback): boolean;
begin
  Result := False;
end;

procedure TCefPermissionHandlerOwn.OnDismissPermissionPrompt(const browser: ICefBrowser; prompt_id: uint64; result: TCefPermissionRequestResult);
begin
  //
end;

procedure TCefPermissionHandlerOwn.RemoveReferences;
begin
  //
end;


// TCustomPermissionHandler

constructor TCustomPermissionHandler.Create(const events : IChromiumEvents);
begin
  inherited Create;

  FEvents := Pointer(events);
end;

destructor TCustomPermissionHandler.Destroy;
begin
  RemoveReferences;

  inherited Destroy;
end;

function TCustomPermissionHandler.OnRequestMediaAccessPermission(const browser               : ICefBrowser;
                                                                 const frame                 : ICefFrame;
                                                                 const requesting_origin     : ustring;
                                                                       requested_permissions : cardinal;
                                                                 const callback              : ICefMediaAccessCallback): boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnRequestMediaAccessPermission(browser, frame, requesting_origin, requested_permissions, callback)
   else
    Result := inherited OnRequestMediaAccessPermission(browser, frame, requesting_origin, requested_permissions, callback);
end;

function TCustomPermissionHandler.OnShowPermissionPrompt(const browser               : ICefBrowser;
                                                               prompt_id             : uint64;
                                                         const requesting_origin     : ustring;
                                                               requested_permissions : cardinal;
                                                         const callback              : ICefPermissionPromptCallback): boolean;
begin
  if (FEvents <> nil) then
    Result := IChromiumEvents(FEvents).doOnShowPermissionPrompt(browser, prompt_id, requesting_origin, requested_permissions, callback)
   else
    Result := inherited OnShowPermissionPrompt(browser, prompt_id, requesting_origin, requested_permissions, callback);
end;

procedure TCustomPermissionHandler.OnDismissPermissionPrompt(const browser   : ICefBrowser;
                                                                   prompt_id : uint64;
                                                                   result    : TCefPermissionRequestResult);
begin
  if (FEvents <> nil) then
    IChromiumEvents(FEvents).doOnDismissPermissionPrompt(browser, prompt_id, result);
end;

procedure TCustomPermissionHandler.RemoveReferences;
begin
  FEvents := nil;
end;

end.

