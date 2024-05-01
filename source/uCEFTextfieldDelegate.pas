unit uCEFTextfieldDelegate;

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
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes, uCEFViewDelegate;

type
  TCefTextfieldDelegateRef = class(TCefViewDelegateRef, ICefTextfieldDelegate)
    protected
      procedure OnKeyEvent(const textfield: ICefTextfield; const event: TCefKeyEvent; var aResult : boolean);
      procedure OnAfterUserAction(const textfield: ICefTextfield);

    public
      /// <summary>
      /// Returns a ICefTextfieldDelegate instance using a PCefTextfieldDelegate data pointer.
      /// </summary>
      class function UnWrap(data: Pointer): ICefTextfieldDelegate;
  end;

  /// <summary>
  /// Implement this interface to handle Textfield events. The functions of this
  /// interface will be called on the browser process UI thread unless otherwise
  /// indicated.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_textfield_delegate_capi.h">CEF source file: /include/capi/views/cef_textfield_delegate_capi.h (cef_textfield_delegate_t)</see></para>
  /// </remarks>
  TCefTextfieldDelegateOwn = class(TCefViewDelegateOwn, ICefTextfieldDelegate)
    protected
      /// <summary>
      /// Called when |textfield| receives a keyboard event. |event| contains
      /// information about the keyboard event. Return true (1) if the keyboard
      /// event was handled or false (0) otherwise for default handling.
      /// </summary>
      procedure OnKeyEvent(const textfield: ICefTextfield; const event: TCefKeyEvent; var aResult : boolean); virtual;
      /// <summary>
      /// Called after performing a user action that may change |textfield|.
      /// </summary>
      procedure OnAfterUserAction(const textfield: ICefTextfield); virtual;
      /// <summary>
      /// Links the methods in the internal CEF record data pointer with the methods in this class.
      /// </summary>
      procedure InitializeCEFMethods; override;

    public
      constructor Create; override;
  end;

  /// <summary>
  /// This class handles all the ICefTextfieldDelegate and ICefViewDelegate methods which call the ICefTextfieldDelegateEvents methods.
  /// ICefTextfieldDelegateEvents will be implemented by the control receiving the ICefTextfieldDelegate events.
  /// </summary>
  TCustomTextfieldDelegate = class(TCefTextfieldDelegateOwn)
    protected
      FEvents : Pointer;

      // ICefViewDelegate
      procedure OnGetPreferredSize(const view: ICefView; var aResult : TCefSize); override;
      procedure OnGetMinimumSize(const view: ICefView; var aResult : TCefSize); override;
      procedure OnGetMaximumSize(const view: ICefView; var aResult : TCefSize); override;
      procedure OnGetHeightForWidth(const view: ICefView; width: Integer; var aResult: Integer); override;
      procedure OnParentViewChanged(const view: ICefView; added: boolean; const parent: ICefView); override;
      procedure OnChildViewChanged(const view: ICefView; added: boolean; const child: ICefView); override;
      procedure OnWindowChanged(const view: ICefView; added: boolean); override;
      procedure OnLayoutChanged(const view: ICefView; new_bounds: TCefRect); override;
      procedure OnFocus(const view: ICefView); override;
      procedure OnBlur(const view: ICefView); override;
      procedure OnThemeChanged(const view: ICefView); override;

      // ICefTextfieldDelegate
      procedure OnKeyEvent(const textfield: ICefTextfield; const event: TCefKeyEvent; var aResult : boolean); override;
      procedure OnAfterUserAction(const textfield: ICefTextfield); override;

    public
      /// <summary>
      /// Creates an instance of this class liked to an interface that's implemented by a control receiving the events.
      /// </summary>
      constructor Create(const events: ICefTextfieldDelegateEvents); reintroduce;
  end;

implementation

uses
  uCEFLibFunctions, uCEFMiscFunctions, uCEFTextfield;


// **************************************************************
// ***************** TCefTextfieldDelegateRef *******************
// **************************************************************

procedure TCefTextfieldDelegateRef.OnKeyEvent(const textfield : ICefTextfield;
                                              const event     : TCefKeyEvent;
                                              var   aResult   : boolean);
begin
  aResult := (PCefTextfieldDelegate(FData)^.on_key_event(PCefTextfieldDelegate(FData),
                                                         CefGetData(textfield),
                                                         @event) <> 0);
end;

procedure TCefTextfieldDelegateRef.OnAfterUserAction(const textfield: ICefTextfield);
begin
  PCefTextfieldDelegate(FData)^.on_after_user_action(PCefTextfieldDelegate(FData),
                                                     CefGetData(textfield));
end;

class function TCefTextfieldDelegateRef.UnWrap(data: Pointer): ICefTextfieldDelegate;
begin
  if (data <> nil) then
    Result := Create(data) as ICefTextfieldDelegate
   else
    Result := nil;
end;


// **************************************************************
// ***************** TCefTextfieldDelegateOwn *******************
// **************************************************************

function cef_textfield_delegate_on_key_event(self: PCefTextfieldDelegate; textfield: PCefTextfield; const event: PCefKeyEvent): Integer; stdcall;
var
  TempObject : TObject;
  TempResult : boolean;
begin
  TempObject := CefGetObject(self);
  TempResult := False;

  if (TempObject <> nil) and (TempObject is TCefTextfieldDelegateOwn) then
    TCefTextfieldDelegateOwn(TempObject).OnKeyEvent(TCefTextfieldRef.UnWrap(textfield), event^, TempResult);

  Result := ord(TempResult);
end;

procedure cef_textfield_delegate_on_accelerator(self: PCefTextfieldDelegate; textfield: PCefTextfield); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefTextfieldDelegateOwn) then
    TCefTextfieldDelegateOwn(TempObject).OnAfterUserAction(TCefTextfieldRef.UnWrap(textfield));
end;

constructor TCefTextfieldDelegateOwn.Create;
begin
  inherited CreateData(SizeOf(TCefTextfieldDelegate));

  InitializeCEFMethods;
end;

procedure TCefTextfieldDelegateOwn.InitializeCEFMethods;
begin
  inherited InitializeCEFMethods;

  with PCefTextfieldDelegate(FData)^ do
    begin
      on_after_user_action := {$IFDEF FPC}@{$ENDIF}cef_textfield_delegate_on_accelerator;
      on_key_event         := {$IFDEF FPC}@{$ENDIF}cef_textfield_delegate_on_key_event;
    end;
end;

procedure TCefTextfieldDelegateOwn.OnKeyEvent(const textfield: ICefTextfield; const event: TCefKeyEvent; var aResult : boolean);
begin
  //
end;

procedure TCefTextfieldDelegateOwn.OnAfterUserAction(const textfield: ICefTextfield);
begin
  //
end;


// **************************************************************
// ***************** TCustomTextfieldDelegate *******************
// **************************************************************

constructor TCustomTextfieldDelegate.Create(const events: ICefTextfieldDelegateEvents);
begin
  inherited Create;

  FEvents := Pointer(events);
end;

procedure TCustomTextfieldDelegate.OnGetPreferredSize(const view: ICefView; var aResult : TCefSize);
begin
  inherited OnGetPreferredSize(view, aResult);

  try
    if (FEvents <> nil) then
      ICefTextfieldDelegateEvents(FEvents).doOnGetPreferredSize(view, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomTextfieldDelegate.OnGetPreferredSize', e) then raise;
  end;
end;

procedure TCustomTextfieldDelegate.OnGetMinimumSize(const view: ICefView; var aResult : TCefSize);
begin
  inherited OnGetMinimumSize(view, aResult);

  try
    if (FEvents <> nil) then
      ICefTextfieldDelegateEvents(FEvents).doOnGetMinimumSize(view, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomTextfieldDelegate.OnGetMinimumSize', e) then raise;
  end;
end;

procedure TCustomTextfieldDelegate.OnGetMaximumSize(const view: ICefView; var aResult : TCefSize);
begin
  inherited OnGetMaximumSize(view, aResult);

  try
    if (FEvents <> nil) then
      ICefTextfieldDelegateEvents(FEvents).doOnGetMaximumSize(view, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomTextfieldDelegate.OnGetMaximumSize', e) then raise;
  end;
end;

procedure TCustomTextfieldDelegate.OnGetHeightForWidth(const view: ICefView; width: Integer; var aResult: Integer);
begin
  inherited OnGetHeightForWidth(view, width, aResult);

  try
    if (FEvents <> nil) then
      ICefTextfieldDelegateEvents(FEvents).doOnGetHeightForWidth(view, width, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomTextfieldDelegate.OnGetHeightForWidth', e) then raise;
  end;
end;

procedure TCustomTextfieldDelegate.OnParentViewChanged(const view: ICefView; added: boolean; const parent: ICefView);
begin
  try
    if (FEvents <> nil) then
      ICefTextfieldDelegateEvents(FEvents).doOnParentViewChanged(view, added, parent);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomTextfieldDelegate.OnParentViewChanged', e) then raise;
  end;
end;

procedure TCustomTextfieldDelegate.OnChildViewChanged(const view: ICefView; added: boolean; const child: ICefView);
begin
  try
    if (FEvents <> nil) then
      ICefTextfieldDelegateEvents(FEvents).doOnChildViewChanged(view, added, child);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomTextfieldDelegate.OnChildViewChanged', e) then raise;
  end;
end;

procedure TCustomTextfieldDelegate.OnWindowChanged(const view: ICefView; added: boolean);
begin
  try
    if (FEvents <> nil) then
      ICefTextfieldDelegateEvents(FEvents).doOnWindowChanged(view, added);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomTextfieldDelegate.OnWindowChanged', e) then raise;
  end;
end;

procedure TCustomTextfieldDelegate.OnLayoutChanged(const view: ICefView; new_bounds: TCefRect);
begin
  try
    if (FEvents <> nil) then
      ICefTextfieldDelegateEvents(FEvents).doOnLayoutChanged(view, new_bounds);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomTextfieldDelegate.OnLayoutChanged', e) then raise;
  end;
end;

procedure TCustomTextfieldDelegate.OnFocus(const view: ICefView);
begin
  try
    if (FEvents <> nil) then
      ICefTextfieldDelegateEvents(FEvents).doOnFocus(view);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomTextfieldDelegate.OnFocus', e) then raise;
  end;
end;

procedure TCustomTextfieldDelegate.OnBlur(const view: ICefView);
begin
  try
    if (FEvents <> nil) then
      ICefTextfieldDelegateEvents(FEvents).doOnBlur(view);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomTextfieldDelegate.OnBlur', e) then raise;
  end;
end;

procedure TCustomTextfieldDelegate.OnThemeChanged(const view: ICefView);
begin
  try
    if (FEvents <> nil) then
      ICefTextfieldDelegateEvents(FEvents).doOnThemeChanged(view);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomTextfieldDelegate.OnThemeChanged', e) then raise;
  end;
end;

procedure TCustomTextfieldDelegate.OnKeyEvent(const textfield: ICefTextfield; const event: TCefKeyEvent; var aResult : boolean);
begin
  try
    if (FEvents <> nil) then
      ICefTextfieldDelegateEvents(FEvents).doOnKeyEvent(textfield, event, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomTextfieldDelegate.OnKeyEvent', e) then raise;
  end;
end;

procedure TCustomTextfieldDelegate.OnAfterUserAction(const textfield: ICefTextfield);
begin
  try
    if (FEvents <> nil) then
      ICefTextfieldDelegateEvents(FEvents).doOnAfterUserAction(textfield);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomTextfieldDelegate.OnAfterUserAction', e) then raise;
  end;
end;

end.
