unit uCEFViewDelegate;

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
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefViewDelegateRef = class(TCefBaseRefCountedRef, ICefViewDelegate)
    protected
      procedure OnGetPreferredSize(const view: ICefView; var aResult : TCefSize);
      procedure OnGetMinimumSize(const view: ICefView; var aResult : TCefSize);
      procedure OnGetMaximumSize(const view: ICefView; var aResult : TCefSize);
      procedure OnGetHeightForWidth(const view: ICefView; width: Integer; var aResult: Integer);
      procedure OnParentViewChanged(const view: ICefView; added: boolean; const parent: ICefView);
      procedure OnChildViewChanged(const view: ICefView; added: boolean; const child: ICefView);
      procedure OnWindowChanged(const view: ICefView; added: boolean);
      procedure OnLayoutChanged(const view: ICefView; new_bounds: TCefRect);
      procedure OnFocus(const view: ICefView);
      procedure OnBlur(const view: ICefView);
      procedure OnThemeChanged(const view: ICefView);

    public
      /// <summary>
      /// Returns a ICefViewDelegate instance using a PCefViewDelegate data pointer.
      /// </summary>
      class function UnWrap(data: Pointer): ICefViewDelegate;
  end;

  /// <summary>
  /// Implement this interface to handle view events. All size and position values
  /// are in density independent pixels (DIP) unless otherwise indicated. The
  /// functions of this interface will be called on the browser process UI thread
  /// unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_view_delegate_capi.h">CEF source file: /include/capi/views/cef_view_delegate_capi.h (cef_view_delegate_t)</see></para>
  /// </remarks>
  TCefViewDelegateOwn = class(TCefBaseRefCountedOwn, ICefViewDelegate)
    protected
      /// <summary>
      /// Return the preferred size for |view|. The Layout will use this information
      /// to determine the display size.
      /// </summary>
      procedure OnGetPreferredSize(const view: ICefView; var aResult : TCefSize); virtual;
      /// <summary>
      /// Return the minimum size for |view|.
      /// </summary>
      procedure OnGetMinimumSize(const view: ICefView; var aResult : TCefSize); virtual;
      /// <summary>
      /// Return the maximum size for |view|.
      /// </summary>
      procedure OnGetMaximumSize(const view: ICefView; var aResult : TCefSize); virtual;
      /// <summary>
      /// Return the height necessary to display |view| with the provided |width|.
      /// If not specified the result of get_preferred_size().height will be used by
      /// default. Override if |view|'s preferred height depends upon the width (for
      /// example, with Labels).
      /// </summary>
      procedure OnGetHeightForWidth(const view: ICefView; width: Integer; var aResult: Integer); virtual;
      /// <summary>
      /// Called when the parent of |view| has changed. If |view| is being added to
      /// |parent| then |added| will be true (1). If |view| is being removed from
      /// |parent| then |added| will be false (0). If |view| is being reparented the
      /// remove notification will be sent before the add notification. Do not
      /// modify the view hierarchy in this callback.
      /// </summary>
      procedure OnParentViewChanged(const view: ICefView; added: boolean; const parent: ICefView); virtual;
      /// <summary>
      /// Called when a child of |view| has changed. If |child| is being added to
      /// |view| then |added| will be true (1). If |child| is being removed from
      /// |view| then |added| will be false (0). If |child| is being reparented the
      /// remove notification will be sent to the old parent before the add
      /// notification is sent to the new parent. Do not modify the view hierarchy
      /// in this callback.
      /// </summary>
      procedure OnChildViewChanged(const view: ICefView; added: boolean; const child: ICefView); virtual;
      /// <summary>
      /// Called when |view| is added or removed from the ICefWindow.
      /// </summary>
      procedure OnWindowChanged(const view: ICefView; added: boolean); virtual;
      /// <summary>
      /// Called when the layout of |view| has changed.
      /// </summary>
      procedure OnLayoutChanged(const view: ICefView; new_bounds: TCefRect); virtual;
      /// <summary>
      /// Called when |view| gains focus.
      /// </summary>
      procedure OnFocus(const view: ICefView); virtual;
      /// <summary>
      /// Called when |view| loses focus.
      /// </summary>
      procedure OnBlur(const view: ICefView); virtual;
      /// <summary>
      /// <para>Called when the theme for |view| has changed, after the new theme colors
      /// have already been applied. Views are notified via the component hierarchy
      /// in depth-first reverse order (children before parents).</para>
      /// <para>This will be called in the following cases:</para>
      /// <code>
      /// 1. When |view|, or a parent of |view|, is added to a Window.
      /// 2. When the native/OS or Chrome theme changes for the Window that contains
      ///    |view|. See ICefWindowDelegate.OnThemeColorsChanged documentation.
      /// 3. When the client explicitly calls ICefWindow.ThemeChanged on the
      ///    Window that contains |view|.
      /// </code>
      /// <para>Optionally use this callback to override the new per-View theme colors by
      /// calling ICefView.SetBackgroundColor or the appropriate component-
      /// specific function. See ICefWindow.SetThemeColor documentation for how
      /// to customize additional Window theme colors.</para>
      /// <summary>
      procedure OnThemeChanged(const view: ICefView); virtual;
      /// <summary>
      /// Links the methods in the internal CEF record data pointer with the methods in this class.
      /// </summary>
      procedure InitializeCEFMethods; virtual;
    public
      constructor Create; virtual;
  end;

  /// <summary>
  /// This class handles all the ICefViewDelegate methods which call the ICefViewDelegateEvents methods.
  /// ICefViewDelegateEvents will be implemented by the control receiving the ICefViewDelegate events.
  /// </summary>
  TCustomViewDelegate = class(TCefViewDelegateOwn)
    protected
      FEvents : Pointer;

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
      constructor Create(const events: ICefViewDelegateEvents); reintroduce;
      destructor  Destroy; override;
  end;

implementation

uses
  uCEFLibFunctions, uCEFMiscFunctions, uCEFView;


// **************************************************************
// ******************** TCefViewDelegateRef *********************
// **************************************************************

procedure TCefViewDelegateRef.OnGetPreferredSize(const view: ICefView; var aResult : TCefSize);
begin
  aResult := PCefViewDelegate(FData)^.get_preferred_size(PCefViewDelegate(FData), CefGetData(view));
end;

procedure TCefViewDelegateRef.OnGetMinimumSize(const view: ICefView; var aResult : TCefSize);
begin
  aResult := PCefViewDelegate(FData)^.get_minimum_size(PCefViewDelegate(FData), CefGetData(view));
end;

procedure TCefViewDelegateRef.OnGetMaximumSize(const view: ICefView; var aResult : TCefSize);
begin
  aResult := PCefViewDelegate(FData)^.get_maximum_size(PCefViewDelegate(FData), CefGetData(view));
end;

procedure TCefViewDelegateRef.OnGetHeightForWidth(const view: ICefView; width: Integer; var aResult: Integer);
begin
  aResult := PCefViewDelegate(FData)^.get_height_for_width(PCefViewDelegate(FData),
                                                           CefGetData(view),
                                                           width);
end;

procedure TCefViewDelegateRef.OnParentViewChanged(const view: ICefView; added: boolean; const parent: ICefView);
begin
  PCefViewDelegate(FData)^.on_parent_view_changed(PCefViewDelegate(FData),
                                                  CefGetData(view),
                                                  ord(added),
                                                  CefGetData(parent));
end;

procedure TCefViewDelegateRef.OnChildViewChanged(const view: ICefView; added: boolean; const child: ICefView);
begin
  PCefViewDelegate(FData)^.on_child_view_changed(PCefViewDelegate(FData),
                                                 CefGetData(view),
                                                 ord(added),
                                                 CefGetData(child));
end;

procedure TCefViewDelegateRef.OnWindowChanged(const view: ICefView; added: boolean);
begin
  PCefViewDelegate(FData)^.on_window_changed(PCefViewDelegate(FData),
                                             CefGetData(view),
                                             ord(added));
end;

procedure TCefViewDelegateRef.OnLayoutChanged(const view: ICefView; new_bounds: TCefRect);
begin
  PCefViewDelegate(FData)^.on_layout_changed(PCefViewDelegate(FData),
                                             CefGetData(view),
                                             @new_bounds);
end;

procedure TCefViewDelegateRef.OnFocus(const view: ICefView);
begin
  PCefViewDelegate(FData)^.on_focus(PCefViewDelegate(FData),
                                    CefGetData(view));
end;

procedure TCefViewDelegateRef.OnBlur(const view: ICefView);
begin
  PCefViewDelegate(FData)^.on_blur(PCefViewDelegate(FData),
                                   CefGetData(view));
end;

procedure TCefViewDelegateRef.OnThemeChanged(const view: ICefView);
begin
  PCefViewDelegate(FData)^.on_theme_changed(PCefViewDelegate(FData),
                                            CefGetData(view));
end;

class function TCefViewDelegateRef.UnWrap(data: Pointer): ICefViewDelegate;
begin
  if (data <> nil) then
    Result := Create(data) as ICefViewDelegate
   else
    Result := nil;
end;



// **************************************************************
// ******************** TCefViewDelegateOwn *********************
// **************************************************************

function cef_view_delegate_get_preferred_size(self: PCefViewDelegate; view: PCefView): TCefSize; stdcall;
var
  TempObject : TObject;
  TempSize   : TCefSize;
begin
  TempObject      := CefGetObject(self);
  TempSize.width  := 0;
  TempSize.height := 0;

  if (TempObject <> nil) and (TempObject is TCefViewDelegateOwn) then
    TCefViewDelegateOwn(TempObject).OnGetPreferredSize(TCefViewRef.UnWrap(view),
                                                       TempSize);

  Result.width  := TempSize.width;
  Result.height := TempSize.height;
end;

function cef_view_delegate_get_minimum_size(self: PCefViewDelegate; view: PCefView): TCefSize; stdcall;
var
  TempObject : TObject;
  TempSize   : TCefSize;
begin
  TempObject      := CefGetObject(self);
  TempSize.width  := 0;
  TempSize.height := 0;

  if (TempObject <> nil) and (TempObject is TCefViewDelegateOwn) then
    TCefViewDelegateOwn(TempObject).OnGetMinimumSize(TCefViewRef.UnWrap(view),
                                                     TempSize);

  Result.width  := TempSize.width;
  Result.height := TempSize.height;
end;

function cef_view_delegate_get_maximum_size(self: PCefViewDelegate; view: PCefView): TCefSize; stdcall;
var
  TempObject : TObject;
  TempSize   : TCefSize;
begin
  TempObject      := CefGetObject(self);
  TempSize.width  := 0;
  TempSize.height := 0;

  if (TempObject <> nil) and (TempObject is TCefViewDelegateOwn) then
    TCefViewDelegateOwn(TempObject).OnGetMaximumSize(TCefViewRef.UnWrap(view),
                                                     TempSize);

  Result.width  := TempSize.width;
  Result.height := TempSize.height;
end;

function cef_view_delegate_get_height_for_width(self: PCefViewDelegate; view: PCefView; width: Integer): Integer; stdcall;
var
  TempObject : TObject;
  TempHeight : integer;
begin
  TempObject := CefGetObject(self);
  TempHeight := 0;

  if (TempObject <> nil) and (TempObject is TCefViewDelegateOwn) then
    TCefViewDelegateOwn(TempObject).OnGetHeightForWidth(TCefViewRef.UnWrap(view),
                                                        width,
                                                        TempHeight);

  Result := TempHeight;
end;

procedure cef_view_delegate_on_parent_view_changed(self: PCefViewDelegate; view: PCefView; added: Integer; parent: PCefView); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefViewDelegateOwn) then
    TCefViewDelegateOwn(TempObject).OnParentViewChanged(TCefViewRef.UnWrap(view),
                                                        added <> 0,
                                                        TCefViewRef.UnWrap(parent));
end;

procedure cef_view_delegate_on_child_view_changed(self: PCefViewDelegate; view: PCefView; added: Integer; child: PCefView); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefViewDelegateOwn) then
    TCefViewDelegateOwn(TempObject).OnChildViewChanged(TCefViewRef.UnWrap(view),
                                                       added <> 0,
                                                       TCefViewRef.UnWrap(child));
end;

procedure cef_view_delegate_on_window_changed(self: PCefViewDelegate; view: PCefView; added: Integer); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefViewDelegateOwn) then
    TCefViewDelegateOwn(TempObject).OnWindowChanged(TCefViewRef.UnWrap(view),
                                                    added <> 0);
end;

procedure cef_view_delegate_on_layout_changed(self: PCefViewDelegate; view: PCefView; const new_bounds: PCefRect); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefViewDelegateOwn) then
    TCefViewDelegateOwn(TempObject).OnLayoutChanged(TCefViewRef.UnWrap(view),
                                                    new_bounds^);
end;

procedure cef_view_delegate_on_focus(self: PCefViewDelegate; view: PCefView); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefViewDelegateOwn) then
    TCefViewDelegateOwn(TempObject).OnFocus(TCefViewRef.UnWrap(view));
end;

procedure cef_view_delegate_on_blur(self: PCefViewDelegate; view: PCefView); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefViewDelegateOwn) then
    TCefViewDelegateOwn(TempObject).OnBlur(TCefViewRef.UnWrap(view));
end;

procedure cef_view_delegate_on_theme_changed(self: PCefViewDelegate; view: PCefView); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefViewDelegateOwn) then
    TCefViewDelegateOwn(TempObject).OnThemeChanged(TCefViewRef.UnWrap(view));
end;

constructor TCefViewDelegateOwn.Create;
begin
  inherited CreateData(SizeOf(TCefViewDelegate));

  InitializeCEFMethods;
end;

procedure TCefViewDelegateOwn.InitializeCEFMethods;
begin
  with PCefViewDelegate(FData)^ do
    begin
      // Disable these 3 callbacks in 32 bits as a bad workaround for issue #278
      // https://github.com/salvadordf/CEF4Delphi/issues/278
      // The TCefRect return type seems to be messing the stack and the other parameters
      // are assigned wrong addresses.
      {$IFDEF TARGET_64BITS}
      get_preferred_size      := {$IFDEF FPC}@{$ENDIF}cef_view_delegate_get_preferred_size;
      get_minimum_size        := {$IFDEF FPC}@{$ENDIF}cef_view_delegate_get_minimum_size;
      get_maximum_size        := {$IFDEF FPC}@{$ENDIF}cef_view_delegate_get_maximum_size;
      {$ELSE}
      get_preferred_size      := nil;
      get_minimum_size        := nil;
      get_maximum_size        := nil;
      {$ENDIF}
      get_height_for_width    := {$IFDEF FPC}@{$ENDIF}cef_view_delegate_get_height_for_width;
      on_parent_view_changed  := {$IFDEF FPC}@{$ENDIF}cef_view_delegate_on_parent_view_changed;
      on_child_view_changed   := {$IFDEF FPC}@{$ENDIF}cef_view_delegate_on_child_view_changed;
      on_window_changed       := {$IFDEF FPC}@{$ENDIF}cef_view_delegate_on_window_changed;
      on_layout_changed       := {$IFDEF FPC}@{$ENDIF}cef_view_delegate_on_layout_changed;
      on_focus                := {$IFDEF FPC}@{$ENDIF}cef_view_delegate_on_focus;
      on_blur                 := {$IFDEF FPC}@{$ENDIF}cef_view_delegate_on_blur;
      on_theme_changed        := {$IFDEF FPC}@{$ENDIF}cef_view_delegate_on_theme_changed;
    end;
end;

procedure TCefViewDelegateOwn.OnGetPreferredSize(const view: ICefView; var aResult : TCefSize);
begin
  aResult.width  := 0;
  aResult.height := 0;
end;

procedure TCefViewDelegateOwn.OnGetMinimumSize(const view: ICefView; var aResult : TCefSize);
begin
  aResult.width  := 0;
  aResult.height := 0;
end;

procedure TCefViewDelegateOwn.OnGetMaximumSize(const view: ICefView; var aResult : TCefSize);
begin
  aResult.width  := 0;
  aResult.height := 0;
end;

procedure TCefViewDelegateOwn.OnGetHeightForWidth(const view: ICefView; width: Integer; var aResult: Integer);
begin
  aResult := 0;
end;

procedure TCefViewDelegateOwn.OnParentViewChanged(const view: ICefView; added: boolean; const parent: ICefView);
begin
  //
end;

procedure TCefViewDelegateOwn.OnChildViewChanged(const view: ICefView; added: boolean; const child: ICefView);
begin
  //
end;

procedure TCefViewDelegateOwn.OnWindowChanged(const view: ICefView; added: boolean);
begin
  //
end;

procedure TCefViewDelegateOwn.OnLayoutChanged(const view: ICefView; new_bounds: TCefRect);
begin
  //
end;

procedure TCefViewDelegateOwn.OnFocus(const view: ICefView);
begin
  //
end;

procedure TCefViewDelegateOwn.OnBlur(const view: ICefView);
begin
  //
end;

procedure TCefViewDelegateOwn.OnThemeChanged(const view: ICefView);
begin
  //
end;


// **************************************************************
// ******************** TCustomViewDelegate *********************
// **************************************************************

constructor TCustomViewDelegate.Create(const events: ICefViewDelegateEvents);
begin
  inherited Create;

  FEvents := Pointer(events);
end;

destructor TCustomViewDelegate.Destroy;
begin
  FEvents := nil;

  inherited Destroy;
end;

procedure TCustomViewDelegate.OnGetPreferredSize(const view: ICefView; var aResult : TCefSize);
begin
  inherited OnGetPreferredSize(view, aResult);

  try
    if (FEvents <> nil) then
      ICefViewDelegateEvents(FEvents).doOnGetPreferredSize(view, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomViewDelegate.OnGetPreferredSize', e) then raise;
  end;
end;

procedure TCustomViewDelegate.OnGetMinimumSize(const view: ICefView; var aResult : TCefSize);
begin
  inherited OnGetMinimumSize(view, aResult);

  try
    if (FEvents <> nil) then
      ICefViewDelegateEvents(FEvents).doOnGetMinimumSize(view, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomViewDelegate.OnGetMinimumSize', e) then raise;
  end;
end;

procedure TCustomViewDelegate.OnGetMaximumSize(const view: ICefView; var aResult : TCefSize);
begin
  inherited OnGetMaximumSize(view, aResult);

  try
    if (FEvents <> nil) then
      ICefViewDelegateEvents(FEvents).doOnGetMaximumSize(view, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomViewDelegate.OnGetMaximumSize', e) then raise;
  end;
end;

procedure TCustomViewDelegate.OnGetHeightForWidth(const view: ICefView; width: Integer; var aResult: Integer);
begin
  inherited OnGetHeightForWidth(view, width, aResult);

  try
    if (FEvents <> nil) then
      ICefViewDelegateEvents(FEvents).doOnGetHeightForWidth(view, width, aResult);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomViewDelegate.OnGetHeightForWidth', e) then raise;
  end;
end;

procedure TCustomViewDelegate.OnParentViewChanged(const view: ICefView; added: boolean; const parent: ICefView);
begin
  try
    if (FEvents <> nil) then
      ICefViewDelegateEvents(FEvents).doOnParentViewChanged(view, added, parent);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomViewDelegate.OnParentViewChanged', e) then raise;
  end;
end;

procedure TCustomViewDelegate.OnChildViewChanged(const view: ICefView; added: boolean; const child: ICefView);
begin
  try
    if (FEvents <> nil) then
      ICefViewDelegateEvents(FEvents).doOnChildViewChanged(view, added, child);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomViewDelegate.OnChildViewChanged', e) then raise;
  end;
end;

procedure TCustomViewDelegate.OnWindowChanged(const view: ICefView; added: boolean);
begin
  try
    if (FEvents <> nil) then
      ICefViewDelegateEvents(FEvents).doOnWindowChanged(view, added);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomViewDelegate.OnWindowChanged', e) then raise;
  end;
end;

procedure TCustomViewDelegate.OnLayoutChanged(const view: ICefView; new_bounds: TCefRect);
begin
  try
    if (FEvents <> nil) then
      ICefViewDelegateEvents(FEvents).doOnLayoutChanged(view, new_bounds);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomViewDelegate.OnLayoutChanged', e) then raise;
  end;
end;

procedure TCustomViewDelegate.OnFocus(const view: ICefView);
begin
  try
    if (FEvents <> nil) then
      ICefViewDelegateEvents(FEvents).doOnFocus(view);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomViewDelegate.OnFocus', e) then raise;
  end;
end;

procedure TCustomViewDelegate.OnBlur(const view: ICefView);
begin
  try
    if (FEvents <> nil) then
      ICefViewDelegateEvents(FEvents).doOnBlur(view);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomViewDelegate.OnBlur', e) then raise;
  end;
end;

procedure TCustomViewDelegate.OnThemeChanged(const view: ICefView);
begin
  try
    if (FEvents <> nil) then
      ICefViewDelegateEvents(FEvents).doOnThemeChanged(view);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomViewDelegate.OnThemeChanged', e) then raise;
  end;
end;

end.

