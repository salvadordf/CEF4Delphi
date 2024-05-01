unit uCEFButtonDelegate;

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
  TCefButtonDelegateRef = class(TCefViewDelegateRef, ICefButtonDelegate)
    protected
      procedure OnButtonPressed(const button: ICefButton);
      procedure OnButtonStateChanged(const button: ICefButton);

    public
      /// <summary>
      /// Returns a ICefButtonDelegate instance using a PCefButtonDelegate data pointer.
      /// </summary>
      class function UnWrap(data: Pointer): ICefButtonDelegate;
  end;

  /// <summary>
  /// Implement this interface to handle Button events. The functions of this
  /// interface will be called on the browser process UI thread unless otherwise
  /// indicated.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_button_delegate_capi.h">CEF source file: /include/capi/views/cef_button_delegate_capi.h (cef_button_delegate_t)</see></para>
  /// </remarks>
  TCefButtonDelegateOwn = class(TCefViewDelegateOwn, ICefButtonDelegate)
    protected
      /// <summary>
      /// Called when |button| is pressed.
      /// </summary>
      procedure OnButtonPressed(const button: ICefButton); virtual;
      /// <summary>
      /// Called when the state of |button| changes.
      /// </summary>
      procedure OnButtonStateChanged(const button: ICefButton); virtual;
      /// <summary>
      /// Links the methods in the internal CEF record data pointer with the methods in this class.
      /// </summary>
      procedure InitializeCEFMethods; override;
    public
      constructor Create; override;
  end;

  /// <summary>
  /// This class handles all the ICefButtonDelegate methods which call the ICefButtonDelegateEvents methods.
  /// ICefButtonDelegateEvents will be implemented by the control receiving the ICefButtonDelegate events.
  /// </summary>
  TCustomButtonDelegate = class(TCefButtonDelegateOwn)
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

      // ICefButtonDelegate
      procedure OnButtonPressed(const button: ICefButton); override;
      procedure OnButtonStateChanged(const button: ICefButton); override;

    public
      /// <summary>
      /// Creates an instance of this class liked to an interface that's implemented by a control receiving the events.
      /// </summary>
      constructor Create(const events: ICefButtonDelegateEvents); reintroduce;
  end;

implementation

uses
  uCEFLibFunctions, uCEFMiscFunctions, uCEFButton;


// **************************************************************
// ****************** TCefButtonDelegateRef *********************
// **************************************************************

procedure TCefButtonDelegateRef.OnButtonPressed(const button: ICefButton);
begin
  PCefButtonDelegate(FData)^.on_button_pressed(PCefButtonDelegate(FData),
                                               CefGetData(button));
end;

procedure TCefButtonDelegateRef.OnButtonStateChanged(const button: ICefButton);
begin
  PCefButtonDelegate(FData)^.on_button_state_changed(PCefButtonDelegate(FData),
                                                     CefGetData(button));
end;

class function TCefButtonDelegateRef.UnWrap(data: Pointer): ICefButtonDelegate;
begin
  if (data <> nil) then
    Result := Create(data) as ICefButtonDelegate
   else
    Result := nil;
end;

// **************************************************************
// ****************** TCefButtonDelegateOwn *********************
// **************************************************************

procedure cef_button_delegate_on_button_pressed(self: PCefButtonDelegate; button: PCefButton); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefButtonDelegateOwn) then
    TCefButtonDelegateOwn(TempObject).OnButtonPressed(TCefButtonRef.UnWrap(button));
end;

procedure cef_button_delegate_on_button_state_changed(self: PCefButtonDelegate; button: PCefButton); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefButtonDelegateOwn) then
    TCefButtonDelegateOwn(TempObject).OnButtonStateChanged(TCefButtonRef.UnWrap(button));
end;


constructor TCefButtonDelegateOwn.Create;
begin
  inherited CreateData(SizeOf(TCefButtonDelegate));

  InitializeCEFMethods;
end;

procedure TCefButtonDelegateOwn.InitializeCEFMethods;
begin
  inherited InitializeCEFMethods;

  with PCefButtonDelegate(FData)^ do
    begin
      on_button_pressed       := {$IFDEF FPC}@{$ENDIF}cef_button_delegate_on_button_pressed;
      on_button_state_changed := {$IFDEF FPC}@{$ENDIF}cef_button_delegate_on_button_state_changed;
    end;
end;

procedure TCefButtonDelegateOwn.OnButtonPressed(const button: ICefButton);
begin
  //
end;

procedure TCefButtonDelegateOwn.OnButtonStateChanged(const button: ICefButton);
begin
  //
end;

// **************************************************************
// ****************** TCustomButtonDelegate *********************
// **************************************************************

constructor TCustomButtonDelegate.Create(const events: ICefButtonDelegateEvents);
begin
  inherited Create;

  FEvents := Pointer(events);
end;

procedure TCustomButtonDelegate.OnGetPreferredSize(const view: ICefView; var aResult : TCefSize);
begin
  inherited OnGetPreferredSize(view, aResult);

  try
    if (FEvents <> nil) then
      ICefButtonDelegateEvents(FEvents).doOnGetPreferredSize(view, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomButtonDelegate.OnGetPreferredSize', e) then raise;
  end;
end;

procedure TCustomButtonDelegate.OnGetMinimumSize(const view: ICefView; var aResult : TCefSize);
begin
  inherited OnGetMinimumSize(view, aResult);

  try
    if (FEvents <> nil) then
      ICefButtonDelegateEvents(FEvents).doOnGetMinimumSize(view, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomButtonDelegate.OnGetMinimumSize', e) then raise;
  end;
end;

procedure TCustomButtonDelegate.OnGetMaximumSize(const view: ICefView; var aResult : TCefSize);
begin
  inherited OnGetMaximumSize(view, aResult);

  try
    if (FEvents <> nil) then
      ICefButtonDelegateEvents(FEvents).doOnGetMaximumSize(view, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomButtonDelegate.OnGetMaximumSize', e) then raise;
  end;
end;

procedure TCustomButtonDelegate.OnGetHeightForWidth(const view: ICefView; width: Integer; var aResult: Integer);
begin
  inherited OnGetHeightForWidth(view, width, aResult);

  try
    if (FEvents <> nil) then
      ICefButtonDelegateEvents(FEvents).doOnGetHeightForWidth(view, width, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomButtonDelegate.OnGetHeightForWidth', e) then raise;
  end;
end;

procedure TCustomButtonDelegate.OnParentViewChanged(const view: ICefView; added: boolean; const parent: ICefView);
begin
  try
    if (FEvents <> nil) then
      ICefButtonDelegateEvents(FEvents).doOnParentViewChanged(view, added, parent);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomButtonDelegate.OnParentViewChanged', e) then raise;
  end;
end;

procedure TCustomButtonDelegate.OnChildViewChanged(const view: ICefView; added: boolean; const child: ICefView);
begin
  try
    if (FEvents <> nil) then
      ICefButtonDelegateEvents(FEvents).doOnChildViewChanged(view, added, child);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomButtonDelegate.OnChildViewChanged', e) then raise;
  end;
end;

procedure TCustomButtonDelegate.OnWindowChanged(const view: ICefView; added: boolean);
begin
  try
    if (FEvents <> nil) then
      ICefButtonDelegateEvents(FEvents).doOnWindowChanged(view, added);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomButtonDelegate.OnWindowChanged', e) then raise;
  end;
end;

procedure TCustomButtonDelegate.OnLayoutChanged(const view: ICefView; new_bounds: TCefRect);
begin
  try
    if (FEvents <> nil) then
      ICefButtonDelegateEvents(FEvents).doOnLayoutChanged(view, new_bounds);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomButtonDelegate.OnLayoutChanged', e) then raise;
  end;
end;

procedure TCustomButtonDelegate.OnFocus(const view: ICefView);
begin
  try
    if (FEvents <> nil) then
      ICefButtonDelegateEvents(FEvents).doOnFocus(view);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomButtonDelegate.OnFocus', e) then raise;
  end;
end;

procedure TCustomButtonDelegate.OnBlur(const view: ICefView);
begin
  try
    if (FEvents <> nil) then
      ICefButtonDelegateEvents(FEvents).doOnBlur(view);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomButtonDelegate.OnBlur', e) then raise;
  end;
end;

procedure TCustomButtonDelegate.OnThemeChanged(const view: ICefView);
begin
  try
    if (FEvents <> nil) then
      ICefButtonDelegateEvents(FEvents).doOnThemeChanged(view);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomButtonDelegate.OnThemeChanged', e) then raise;
  end;
end;

procedure TCustomButtonDelegate.OnButtonPressed(const button: ICefButton);
begin
  try
    if (FEvents <> nil) then
      ICefButtonDelegateEvents(FEvents).doOnButtonPressed(button);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomButtonDelegate.OnButtonPressed', e) then raise;
  end;
end;

procedure TCustomButtonDelegate.OnButtonStateChanged(const button: ICefButton);
begin
  try
    if (FEvents <> nil) then
      ICefButtonDelegateEvents(FEvents).doOnButtonStateChanged(button);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomButtonDelegate.OnButtonStateChanged', e) then raise;
  end;
end;

end.

