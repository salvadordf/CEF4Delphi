unit uCEFPanelDelegate;

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
  TCefPanelDelegateRef = class(TCefViewDelegateRef, ICefPanelDelegate)
    public
      /// <summary>
      /// Returns a ICefPanelDelegate instance using a PCefPanelDelegate data pointer.
      /// </summary>
      class function UnWrap(data: Pointer): ICefPanelDelegate;
  end;

  /// <summary>
  /// Implement this interface to handle Panel events. The functions of this
  /// interface will be called on the browser process UI thread unless otherwise
  /// indicated.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_panel_delegate_capi.h">CEF source file: /include/capi/views/cef_panel_delegate_capi.h (cef_panel_delegate_t)</see></para>
  /// </remarks>
  TCefPanelDelegateOwn = class(TCefViewDelegateOwn, ICefPanelDelegate)
    public
      constructor Create; override;
  end;

  /// <summary>
  /// This class handles all the ICefPanelDelegate methods which call the ICefPanelDelegateEvents methods.
  /// ICefPanelDelegateEvents will be implemented by the control receiving the ICefPanelDelegate events.
  /// </summary>
  TCustomPanelDelegate = class(TCefPanelDelegateOwn)
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

    public
      /// <summary>
      /// Creates an instance of this class liked to an interface that's implemented by a control receiving the events.
      /// </summary>
      constructor Create(const events: ICefPanelDelegateEvents); reintroduce;
  end;

implementation

uses
  uCEFLibFunctions, uCEFMiscFunctions;

// **************************************************************
// ******************** TCefPanelDelegateRef ********************
// **************************************************************

class function TCefPanelDelegateRef.UnWrap(data: Pointer): ICefPanelDelegate;
begin
  if (data <> nil) then
    Result := Create(data) as ICefPanelDelegate
   else
    Result := nil;
end;

// **************************************************************
// ******************** TCefPanelDelegateOwn ********************
// **************************************************************

constructor TCefPanelDelegateOwn.Create;
begin
  inherited CreateData(SizeOf(TCefPanelDelegate));

  InitializeCEFMethods;
end;

// **************************************************************
// ******************** TCustomPanelDelegate ********************
// **************************************************************

constructor TCustomPanelDelegate.Create(const events: ICefPanelDelegateEvents);
begin
  inherited Create;

  FEvents := Pointer(events);
end;

procedure TCustomPanelDelegate.OnGetPreferredSize(const view: ICefView; var aResult : TCefSize);
begin
  inherited OnGetPreferredSize(view, aResult);

  try
    if (FEvents <> nil) then
      ICefPanelDelegateEvents(FEvents).doOnGetPreferredSize(view, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomPanelDelegate.OnGetPreferredSize', e) then raise;
  end;
end;

procedure TCustomPanelDelegate.OnGetMinimumSize(const view: ICefView; var aResult : TCefSize);
begin
  inherited OnGetMinimumSize(view, aResult);

  try
    if (FEvents <> nil) then
      ICefPanelDelegateEvents(FEvents).doOnGetMinimumSize(view, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomPanelDelegate.OnGetMinimumSize', e) then raise;
  end;
end;

procedure TCustomPanelDelegate.OnGetMaximumSize(const view: ICefView; var aResult : TCefSize);
begin
  inherited OnGetMaximumSize(view, aResult);

  try
    if (FEvents <> nil) then
      ICefPanelDelegateEvents(FEvents).doOnGetMaximumSize(view, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomPanelDelegate.OnGetMaximumSize', e) then raise;
  end;
end;

procedure TCustomPanelDelegate.OnGetHeightForWidth(const view: ICefView; width: Integer; var aResult: Integer);
begin
  inherited OnGetHeightForWidth(view, width, aResult);

  try
    if (FEvents <> nil) then
      ICefPanelDelegateEvents(FEvents).doOnGetHeightForWidth(view, width, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomPanelDelegate.OnGetHeightForWidth', e) then raise;
  end;
end;

procedure TCustomPanelDelegate.OnParentViewChanged(const view: ICefView; added: boolean; const parent: ICefView);
begin
  try
    if (FEvents <> nil) then
      ICefPanelDelegateEvents(FEvents).doOnParentViewChanged(view, added, parent);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomPanelDelegate.OnParentViewChanged', e) then raise;
  end;
end;

procedure TCustomPanelDelegate.OnChildViewChanged(const view: ICefView; added: boolean; const child: ICefView);
begin
  try
    if (FEvents <> nil) then
      ICefPanelDelegateEvents(FEvents).doOnChildViewChanged(view, added, child);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomPanelDelegate.OnChildViewChanged', e) then raise;
  end;
end;

procedure TCustomPanelDelegate.OnWindowChanged(const view: ICefView; added: boolean);
begin
  try
    if (FEvents <> nil) then
      ICefPanelDelegateEvents(FEvents).doOnWindowChanged(view, added);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomPanelDelegate.OnWindowChanged', e) then raise;
  end;
end;

procedure TCustomPanelDelegate.OnLayoutChanged(const view: ICefView; new_bounds: TCefRect);
begin
  try
    if (FEvents <> nil) then
      ICefPanelDelegateEvents(FEvents).doOnLayoutChanged(view, new_bounds);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomPanelDelegate.OnLayoutChanged', e) then raise;
  end;
end;

procedure TCustomPanelDelegate.OnFocus(const view: ICefView);
begin
  try
    if (FEvents <> nil) then
      ICefPanelDelegateEvents(FEvents).doOnFocus(view);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomPanelDelegate.OnFocus', e) then raise;
  end;
end;

procedure TCustomPanelDelegate.OnBlur(const view: ICefView);
begin
  try
    if (FEvents <> nil) then
      ICefPanelDelegateEvents(FEvents).doOnBlur(view);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomPanelDelegate.OnBlur', e) then raise;
  end;
end;

procedure TCustomPanelDelegate.OnThemeChanged(const view: ICefView);
begin
  try
    if (FEvents <> nil) then
      ICefPanelDelegateEvents(FEvents).doOnThemeChanged(view);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomPanelDelegate.OnThemeChanged', e) then raise;
  end;
end;

end.

