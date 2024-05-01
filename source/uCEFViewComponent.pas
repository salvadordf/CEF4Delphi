unit uCEFViewComponent;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
    {$IFDEF MSWINDOWS}WinApi.Windows,{$ENDIF} System.Classes,
  {$ELSE}
    {$IFDEF MSWINDOWS}Windows,{$ENDIF} Classes,
    {$IFDEF FPC}
    LCLProc, LCLType, LCLIntf, LResources, InterfaceBase,
    {$ENDIF}
  {$ENDIF}
  uCEFTypes, uCEFInterfaces, uCEFViewsFrameworkEvents;

type
  TCEFViewComponent = class(TComponent, ICefViewDelegateEvents)
    protected
      // ICefViewDelegateEvents
      FOnGetPreferredSize        : TOnGetPreferredSizeEvent;
      FOnGetMinimumSize          : TOnGetMinimumSizeEvent;
      FOnGetMaximumSize          : TOnGetMaximumSizeEvent;
      FOnGetHeightForWidth       : TOnGetHeightForWidthEvent;
      FOnParentViewChanged       : TOnParentViewChangedEvent;
      FOnChildViewChanged        : TOnChildViewChangedEvent;
      FOnWindowChanged           : TOnWindowChangedEvent;
      FOnLayoutChanged           : TOnLayoutChangedEvent;
      FOnFocus                   : TOnFocusEvent;
      FOnBlur                    : TOnBlurEvent;
      FOnThemeChanged            : TOnThemeChangedEvent;

      FComponentID               : integer;

      procedure CreateView; virtual;
      procedure DestroyView; virtual;
      procedure Initialize; virtual;

      function  GetInitialized : boolean; virtual;
      function  GetAsView : ICefView; virtual;
      function  GetTypeString : ustring;
      function  GetAsBrowserView : ICefBrowserView; virtual;
      function  GetAsButton : ICefButton; virtual;
      function  GetAsPanel : ICefPanel; virtual;
      function  GetAsScrollView : ICefScrollView; virtual;
      function  GetAsTextfield : ICefTextfield; virtual;
      function  GetIsValid : boolean;
      function  GetIsAttached : boolean;
      function  GetDelegate : ICefViewDelegate;
      function  GetWindow : ICefWindow;
      function  GetID : Integer;
      function  GetGroupID : Integer;
      function  GetParentView : ICefView;
      function  GetBounds : TCefRect;
      function  GetBoundsInScreen : TCefRect;
      function  GetSize : TCefSize;
      function  GetPosition : TCefPoint;
      function  GetPreferredSize : TCefSize;
      function  GetMinimumSize : TCefSize;
      function  GetMaximumSize : TCefSize;
      function  GetIsVisible : boolean;
      function  GetIsDrawn : boolean;
      function  GetIsEnabled : boolean;
      function  GetIsFocusable : boolean;
      function  GetIsAccessibilityFocusable : boolean;
      function  GetBackgroundColor : TCefColor;
      function  GetViewForID(id_: Integer): ICefView;
      function  GetHeightForWidth(width: Integer): Integer;
      function  GetInsets: TCefInsets;
      function  GetComponentID : integer;

      procedure SetID(id_: Integer);
      procedure SetGroupID(group_id: Integer);
      procedure SetBounds(const bounds_: TCefRect);
      procedure SetSize(const size_: TCefSize);
      procedure SetPosition(const position_: TCefPoint);
      procedure SetVisible(visible_: boolean);
      procedure SetEnabled(enabled_: boolean);
      procedure SetFocusable(focusable_: boolean);
      procedure SetBackgroundColor(color: TCefColor);
      procedure SetInsets(const insets: TCefInsets);

      // ICefViewDelegateEvents
      procedure doOnGetPreferredSize(const view: ICefView; var aResult : TCefSize); virtual;
      procedure doOnGetMinimumSize(const view: ICefView; var aResult : TCefSize); virtual;
      procedure doOnGetMaximumSize(const view: ICefView; var aResult : TCefSize); virtual;
      procedure doOnGetHeightForWidth(const view: ICefView; width: Integer; var aResult: Integer); virtual;
      procedure doOnParentViewChanged(const view: ICefView; added: boolean; const parent: ICefView); virtual;
      procedure doOnChildViewChanged(const view: ICefView; added: boolean; const child: ICefView); virtual;
      procedure doOnWindowChanged(const view: ICefView; added: boolean); virtual;
      procedure doOnLayoutChanged(const view: ICefView; new_bounds: TCefRect); virtual;
      procedure doOnFocus(const view: ICefView); virtual;
      procedure doOnBlur(const view: ICefView); virtual;
      procedure doOnThemeChanged(const view: ICefView); virtual;
      procedure doCreateCustomView; virtual;

    public
      constructor Create(AOwner: TComponent); override;
      procedure   AfterConstruction; override;
      procedure   BeforeDestruction; override;
      /// <summary>
      /// Returns a string representation of this View which includes the type and
      /// various type-specific identifying attributes. If |include_children| is
      /// true (1) any child Views will also be included. Used primarily for testing
      /// purposes.
      /// </summary>
      function    ToStringEx(include_children: boolean): ustring;
      /// <summary>
      /// Returns true (1) if this View is the same as |that| View.
      /// </summary>
      function    IsSame(const that: ICefView): boolean;
      /// <summary>
      /// Size this View to its preferred size. Size is in parent coordinates, or
      /// DIP screen coordinates if there is no parent.
      /// </summary>
      procedure   SizeToPreferredSize;
      /// <summary>
      /// Indicate that this View and all parent Views require a re-layout. This
      /// ensures the next call to layout() will propagate to this View even if the
      /// bounds of parent Views do not change.
      /// </summary>
      procedure   InvalidateLayout;
      /// <summary>
      /// Request keyboard focus. If this View is focusable it will become the
      /// focused View.
      /// </summary>
      procedure   RequestFocus;
      /// <summary>
      /// Returns the current theme color associated with |color_id|, or the
      /// placeholder color (red) if unset. See cef_color_ids.h for standard ID
      /// values. Standard colors can be overridden and custom colors can be added
      /// using ICefWindow.SetThemeColor.
      /// </summary>
      function    GetThemeColor(color_id: integer): TCefColor;
      /// <summary>
      /// Convert |point| from this View's coordinate system to DIP screen
      /// coordinates. This View must belong to a Window when calling this function.
      /// Returns true (1) if the conversion is successful or false (0) otherwise.
      /// Use ICefDisplay.ConvertPointToPixels() after calling this function
      /// if further conversion to display-specific pixel coordinates is desired.
      /// </summary>
      function    ConvertPointToScreen(var point: TCefPoint): boolean;
      /// <summary>
      /// Convert |point| to this View's coordinate system from DIP screen
      /// coordinates. This View must belong to a Window when calling this function.
      /// Returns true (1) if the conversion is successful or false (0) otherwise.
      /// Use ICefDisplay.ConvertPointFromPixels() before calling this
      /// function if conversion from display-specific pixel coordinates is
      /// necessary.
      /// </summary>
      function    ConvertPointFromScreen(var point: TCefPoint): boolean;
      /// <summary>
      /// Convert |point| from this View's coordinate system to that of the Window.
      /// This View must belong to a Window when calling this function. Returns true
      /// (1) if the conversion is successful or false (0) otherwise.
      /// </summary>
      function    ConvertPointToWindow(var point: TCefPoint): boolean;
      /// <summary>
      /// Convert |point| to this View's coordinate system from that of the Window.
      /// This View must belong to a Window when calling this function. Returns true
      /// (1) if the conversion is successful or false (0) otherwise.
      /// </summary>
      function    ConvertPointFromWindow(var point: TCefPoint): boolean;
      /// <summary>
      /// Convert |point| from this View's coordinate system to that of |view|.
      /// |view| needs to be in the same Window but not necessarily the same view
      /// hierarchy. Returns true (1) if the conversion is successful or false (0)
      /// otherwise.
      /// </summary>
      function    ConvertPointToView(const view : ICefView; var point: TCefPoint): boolean;
      /// <summary>
      /// Convert |point| to this View's coordinate system from that |view|. |view|
      /// needs to be in the same Window but not necessarily the same view
      /// hierarchy. Returns true (1) if the conversion is successful or false (0)
      /// otherwise.
      /// </summary>
      function    ConvertPointFromView(const view : ICefView; var point: TCefPoint): boolean;
      /// <summary>
      /// Returns true when the control is fully initialized.
      /// </summary>
      property Initialized                    : boolean                    read GetInitialized;
      /// <summary>
      /// Returns this control as a View.
      /// </summary>
      property AsView                         : ICefView                   read GetAsView;
      /// <summary>
      /// Returns this View as a BrowserView or NULL if this is not a BrowserView.
      /// </summary>
      property AsBrowserView                  : ICefBrowserView            read GetAsBrowserView;
      /// <summary>
      /// Returns this View as a Button or NULL if this is not a Button.
      /// </summary>
      property AsButton                       : ICefButton                 read GetAsButton;
      /// <summary>
      /// Returns this View as a Panel or NULL if this is not a Panel.
      /// </summary>
      property AsPanel                        : ICefPanel                  read GetAsPanel;
      /// <summary>
      /// Returns this View as a ScrollView or NULL if this is not a ScrollView.
      /// </summary>
      property AsScrollView                   : ICefScrollView             read GetAsScrollView;
      /// <summary>
      /// Returns this View as a Textfield or NULL if this is not a Textfield.
      /// </summary>
      property AsTextfield                    : ICefTextfield              read GetAsTextfield;
      /// <summary>
      /// Recursively descends the view tree starting at this View, and returns the
      /// first child that it encounters with the given ID. Returns NULL if no
      /// matching child view is found.
      /// </summary>
      property ViewForID[id_: Integer]        : ICefView                   read GetViewForID;
      /// <summary>
      /// Returns true (1) if this View is valid.
      /// </summary>
      property Valid                          : boolean                    read GetIsValid;
      /// <summary>
      /// Returns true (1) if this View is currently attached to another View. A
      /// View can only be attached to one View at a time.
      /// </summary>
      property Attached                       : boolean                    read GetIsAttached;
      /// <summary>
      /// Returns the delegate associated with this View, if any.
      /// </summary>
      property Delegate                       : ICefViewDelegate           read GetDelegate;
      /// <summary>
      /// Returns the top-level Window hosting this View, if any.
      /// </summary>
      property Window                         : ICefWindow                 read GetWindow;
      /// <summary>
      /// Returns the View that contains this View, if any.
      /// </summary>
      property ParentView                     : ICefView                   read GetParentView;
      /// <summary>
      /// Returns the bounds (size and position) of this View in DIP screen
      /// coordinates.
      /// </summary>
      property BoundsInScreen                 : TCefRect                   read GetBoundsInScreen;
      /// <summary>
      /// Returns the size this View would like to be if enough space is available.
      /// Size is in parent coordinates, or DIP screen coordinates if there is no
      /// parent.
      /// </summary>
      property PreferredSize                  : TCefSize                   read GetPreferredSize;
      /// <summary>
      /// Returns the minimum size for this View. Size is in parent coordinates, or
      /// DIP screen coordinates if there is no parent.
      /// </summary>
      property MinimumSize                    : TCefSize                   read GetMinimumSize;
      /// <summary>
      /// Returns the maximum size for this View. Size is in parent coordinates, or
      /// DIP screen coordinates if there is no parent.
      /// </summary>
      property MaximumSize                    : TCefSize                   read GetMaximumSize;
      /// <summary>
      /// Returns whether this View is visible. A view may be visible but still not
      /// drawn in a Window if any parent views are hidden. If this View is a Window
      /// then a return value of true (1) indicates that this Window is currently
      /// visible to the user on-screen. If this View is not a Window then call
      /// is_drawn() to determine whether this View and all parent views are visible
      /// and will be drawn.
      /// </summary>
      property Visible                        : boolean                    read GetIsVisible                   write SetVisible;
      /// <summary>
      /// Returns whether this View is visible and drawn in a Window. A view is
      /// drawn if it and all parent views are visible. If this View is a Window
      /// then calling this function is equivalent to calling is_visible().
      /// Otherwise, to determine if the containing Window is visible to the user
      /// on-screen call is_visible() on the Window.
      /// </summary>
      property Drawn                          : boolean                    read GetIsDrawn;
      /// <summary>
      /// Get or set whether this View is enabled. A disabled View does not receive
      /// keyboard or mouse inputs. If |enabled| differs from the current value the
      /// View will be repainted. Also, clears focus if the focused View is
      /// disabled.
      /// </summary>
      property Enabled                        : boolean                    read GetIsEnabled                   write SetEnabled;
      /// <summary>
      /// Gets and sets whether this View is capable of taking focus. It will clear focus if
      /// the focused View is set to be non-focusable. This is false (0) by default
      /// so that a View used as a container does not get the focus.
      /// </summary>
      property Focusable                      : boolean                    read GetIsFocusable                 write SetFocusable;
      /// <summary>
      /// Return whether this View is focusable when the user requires full keyboard
      /// access, even though it may not be normally focusable.
      /// </summary>
      property AccessibilityFocusable         : boolean                    read GetIsAccessibilityFocusable;
      /// <summary>
      /// Returns the background color for this View. If the background color is
      /// unset then the current `GetThemeColor(CEF_ColorPrimaryBackground)` value
      /// will be returned. If this View belongs to an overlay (created with
      /// ICefWindow.AddOverlayView), and the background color is unset, then a
      /// value of transparent (0) will be returned.
      /// </summary>
      property BackgroundColor                : TCefColor                  read GetBackgroundColor             write SetBackgroundColor;
      /// <summary>
      /// Gets or sets the ID for this View. ID should be unique within the subtree that you
      /// intend to search for it. 0 is the default ID for views.
      /// </summary>
      property ID                             : integer                    read GetID                          write SetID;
      /// <summary>
      /// Returns the group id of this View, or -1 if not set.
      /// </summary>
      property GroupID                        : integer                    read GetGroupID                     write SetGroupID;
      /// <summary>
      /// Returns the bounds (size and position) of this View in parent coordinates,
      /// or DIP screen coordinates if there is no parent.
      /// </summary>
      property Bounds                         : TCefRect                   read GetBounds                      write SetBounds;
      /// <summary>
      /// Returns the size of this View in parent coordinates, or DIP screen
      /// coordinates if there is no parent.
      /// </summary>
      property Size                           : TCefSize                   read GetSize                        write SetSize;
      /// <summary>
      /// Returns the position of this View. Position is in parent coordinates, or
      /// DIP screen coordinates if there is no parent.
      /// </summary>
      property Position                       : TCefPoint                  read GetPosition                    write SetPosition;
      /// <summary>
      /// Returns the insets for this View in parent coordinates, or DIP screen
      /// coordinates if there is no parent.
      /// </summary>
      property Insets                         : TCefInsets                 read GetInsets                      write SetInsets;
      /// <summary>
      /// Returns the type of this View as a string. Used primarily for testing
      /// purposes.
      /// </summary>
      property TypeString                     : ustring                    read GetTypeString;
      /// <summary>
      /// Returns the height necessary to display this View with the provided width.
      /// </summary>
      property HeightForWidth[width: Integer] : Integer                    read GetHeightForWidth;

    published
      /// <summary>
      /// Return the preferred size for |view|. The Layout will use this information
      /// to determine the display size.
      /// </summary>
      property OnGetPreferredSize             : TOnGetPreferredSizeEvent   read FOnGetPreferredSize            write FOnGetPreferredSize;
      /// <summary>
      /// Return the minimum size for |view|.
      /// </summary>
      property OnGetMinimumSize               : TOnGetMinimumSizeEvent     read FOnGetMinimumSize              write FOnGetMinimumSize;
      /// <summary>
      /// Return the maximum size for |view|.
      /// </summary>
      property OnGetMaximumSize               : TOnGetMaximumSizeEvent     read FOnGetMaximumSize              write FOnGetMaximumSize;
      /// <summary>
      /// Return the height necessary to display |view| with the provided |width|.
      /// If not specified the result of get_preferred_size().height will be used by
      /// default. Override if |view|'s preferred height depends upon the width (for
      /// example, with Labels).
      /// </summary>
      property OnGetHeightForWidth            : TOnGetHeightForWidthEvent  read FOnGetHeightForWidth           write FOnGetHeightForWidth;
      /// <summary>
      /// Called when the parent of |view| has changed. If |view| is being added to
      /// |parent| then |added| will be true (1). If |view| is being removed from
      /// |parent| then |added| will be false (0). If |view| is being reparented the
      /// remove notification will be sent before the add notification. Do not
      /// modify the view hierarchy in this callback.
      /// </summary>
      property OnParentViewChanged            : TOnParentViewChangedEvent  read FOnParentViewChanged           write FOnParentViewChanged;
      /// <summary>
      /// Called when a child of |view| has changed. If |child| is being added to
      /// |view| then |added| will be true (1). If |child| is being removed from
      /// |view| then |added| will be false (0). If |child| is being reparented the
      /// remove notification will be sent to the old parent before the add
      /// notification is sent to the new parent. Do not modify the view hierarchy
      /// in this callback.
      /// </summary>
      property OnChildViewChanged             : TOnChildViewChangedEvent   read FOnChildViewChanged            write FOnChildViewChanged;
      /// <summary>
      /// Called when |view| is added or removed from the ICefWindow.
      /// </summary>
      property OnWindowChanged                : TOnWindowChangedEvent      read FOnWindowChanged               write FOnWindowChanged;
      /// <summary>
      /// Called when the layout of |view| has changed.
      /// </summary>
      property OnLayoutChanged                : TOnLayoutChangedEvent      read FOnLayoutChanged               write FOnLayoutChanged;
      /// <summary>
      /// Called when |view| gains focus.
      /// </summary>
      property OnFocus                        : TOnFocusEvent              read FOnFocus                       write FOnFocus;
      /// <summary>
      /// Called when |view| loses focus.
      /// </summary>
      property OnBlur                         : TOnBlurEvent               read FOnBlur                        write FOnBlur;
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
      property OnThemeChanged                 : TOnThemeChangedEvent       read FOnThemeChanged                write FOnThemeChanged;
  end;

// *********************************************************
// ********************** ATTENTION ! **********************
// *********************************************************
// **                                                     **
// **  MANY OF THE EVENTS IN CEF4DELPHI COMPONENTS LIKE   **
// **  TCHROMIUM, TFMXCHROMIUM OR TCEFAPPLICATION ARE     **
// **  EXECUTED IN A CEF THREAD BY DEFAULT.               **
// **                                                     **
// **  WINDOWS CONTROLS MUST BE CREATED AND DESTROYED IN  **
// **  THE SAME THREAD TO AVOID ERRORS.                   **
// **  SOME OF THEM RECREATE THE HANDLERS IF THEY ARE     **
// **  MODIFIED AND CAN CAUSE THE SAME ERRORS.            **
// **                                                     **
// **  DON'T CREATE, MODIFY OR DESTROY WINDOWS CONTROLS   **
// **  INSIDE THE CEF4DELPHI EVENTS AND USE               **
// **  SYNCHRONIZATION OBJECTS TO PROTECT VARIABLES AND   **
// **  FIELDS IF THEY ARE ALSO USED IN THE MAIN THREAD.   **
// **                                                     **
// **  READ THIS FOR MORE INFORMATION :                   **
// **  https://www.briskbard.com/index.php?pageid=cef     **
// **                                                     **
// **  USE OUR FORUMS FOR MORE QUESTIONS :                **
// **  https://www.briskbard.com/forum/                   **
// **                                                     **
// *********************************************************
// *********************************************************

implementation

uses
  uCEFViewDelegate, uCEFMiscFunctions, uCEFTask, uCEFApplicationCore;

constructor TCEFViewComponent.Create(AOwner: TComponent);
begin
  inherited Create(aOwner);

  Initialize;
  FComponentID := 0;
end;

procedure TCEFViewComponent.AfterConstruction;
begin
  inherited AfterConstruction;

  if assigned(GlobalCEFApp) then
    FComponentID := GlobalCEFApp.NextComponentID;
end;

procedure TCEFViewComponent.BeforeDestruction;
begin
  if assigned(GlobalCEFApp) then
    GlobalCEFApp.RemoveComponentID(FComponentID);

  DestroyView;

  inherited BeforeDestruction;
end;

procedure TCEFViewComponent.Initialize;
begin
  FOnGetPreferredSize  := nil;
  FOnGetMinimumSize    := nil;
  FOnGetMaximumSize    := nil;
  FOnGetHeightForWidth := nil;
  FOnParentViewChanged := nil;
  FOnChildViewChanged  := nil;
  FOnWindowChanged     := nil;
  FOnLayoutChanged     := nil;
  FOnFocus             := nil;
  FOnBlur              := nil;
  FOnThemeChanged      := nil;
end;

procedure TCEFViewComponent.CreateView;
var
  TempTask : ICefTask;
begin
  if CefCurrentlyOn(TID_UI) then
    doCreateCustomView
   else
    try
      TempTask := TCefCreateCustomViewTask.Create(self);
      CefPostTask(TID_UI, TempTask);
    finally
      TempTask := nil;
    end;
end;

procedure TCEFViewComponent.DestroyView;
begin
  //
end;

function TCEFViewComponent.GetInitialized : boolean;
begin
  Result := False;
end;

function TCEFViewComponent.GetComponentID : integer;
begin
  Result := FComponentID;
end;

function TCEFViewComponent.GetAsView : ICefView;
begin
  Result := nil;
end;

function TCEFViewComponent.GetAsBrowserView : ICefBrowserView;
begin
  if Initialized then
    Result := AsView.AsBrowserView
   else
    Result := nil;
end;

function TCEFViewComponent.GetAsButton : ICefButton;
begin
  if Initialized then
    Result := AsView.AsButton
   else
    Result := nil;
end;

function TCEFViewComponent.GetAsPanel : ICefPanel;
begin
  if Initialized then
    Result := AsView.AsPanel
   else
    Result := nil;
end;

function TCEFViewComponent.GetAsScrollView : ICefScrollView;
begin
  if Initialized then
    Result := AsView.AsScrollView
   else
    Result := nil;
end;

function TCEFViewComponent.GetAsTextfield : ICefTextfield;
begin
  if Initialized then
    Result := AsView.AsTextfield
   else
    Result := nil;
end;

function TCEFViewComponent.GetTypeString : ustring;
begin
  if Initialized then
    Result := AsView.GetTypeString
   else
    Result := '';
end;

function TCEFViewComponent.ToStringEx(include_children: boolean): ustring;
begin
  if Initialized then
    Result := AsView.ToStringEx(include_children)
   else
    Result := '';
end;

function TCEFViewComponent.GetIsValid : boolean;
begin
  Result := Initialized and AsView.IsValid;
end;

function TCEFViewComponent.GetIsAttached : boolean;
begin
  Result := Initialized and AsView.IsAttached;
end;

function TCEFViewComponent.IsSame(const that: ICefView): boolean;
begin
  if Initialized then
    Result := AsView.IsSame(that)
   else
    Result := False;
end;

function TCEFViewComponent.GetDelegate : ICefViewDelegate;
begin
  if Initialized then
    Result := AsView.GetDelegate
   else
    Result := nil;
end;

function TCEFViewComponent.GetWindow : ICefWindow;
begin
  if Initialized then
    Result := AsView.GetWindow
   else
    Result := nil;
end;

function TCEFViewComponent.GetID : Integer;
begin
  if Initialized then
    Result := AsView.GetID
   else
    Result := 0;
end;

procedure TCEFViewComponent.SetID(id_: Integer);
begin
  if Initialized then
    AsView.SetID(id_);
end;

function TCEFViewComponent.GetGroupID : Integer;
begin
  if Initialized then
    Result := AsView.GetGroupID
   else
    Result := -1;
end;

procedure TCEFViewComponent.SetGroupID(group_id: Integer);
begin
  if Initialized then
    AsView.SetGroupID(group_id);
end;

function TCEFViewComponent.GetParentView : ICefView;
begin
  if Initialized then
    Result := AsView.GetParentView
   else
    Result := nil;
end;

function TCEFViewComponent.GetViewForID(id_: Integer): ICefView;
begin
  if Initialized then
    Result := AsView.GetViewForID(id_)
   else
    Result := nil;
end;

procedure TCEFViewComponent.SetBounds(const bounds_: TCefRect);
begin
  if Initialized then
    AsView.SetBounds(bounds_);
end;

function TCEFViewComponent.GetBounds : TCefRect;
var
  TempRect : TCefRect;
begin
  if Initialized then
    TempRect := AsView.GetBounds
   else
    begin
      TempRect.x      := 0;
      TempRect.y      := 0;
      TempRect.width  := 0;
      TempRect.height := 0;
    end;

  Result := TempRect;
end;

function TCEFViewComponent.GetBoundsInScreen : TCefRect;
var
  TempRect : TCefRect;
begin
  if Initialized then
    TempRect := AsView.GetBoundsInScreen
   else
    begin
      TempRect.x      := 0;
      TempRect.y      := 0;
      TempRect.width  := 0;
      TempRect.height := 0;
    end;

  Result := TempRect;
end;

procedure TCEFViewComponent.SetSize(const size_: TCefSize);
begin
  if Initialized then
    AsView.SetSize(size_);
end;

function TCEFViewComponent.GetSize : TCefSize;
var
  TempSize : TCefSize;
begin
  if Initialized then
    TempSize := AsView.GetSize
   else
    begin
      TempSize.width  := 0;
      TempSize.height := 0;
    end;

  Result := TempSize;
end;

procedure TCEFViewComponent.SetPosition(const position_: TCefPoint);
begin
  if Initialized then
    AsView.SetPosition(position_);
end;

function TCEFViewComponent.GetPosition : TCefPoint;
var
  TempPoint : TCefPoint;
begin
  if Initialized then
    TempPoint := AsView.GetPosition
   else
    begin
      TempPoint.x := 0;
      TempPoint.y := 0;
    end;

  Result := TempPoint;
end;

function TCEFViewComponent.GetPreferredSize : TCefSize;
var
  TempSize : TCefSize;
begin
  if Initialized then
    TempSize := AsView.GetPreferredSize
   else
    begin
      TempSize.width  := 0;
      TempSize.height := 0;
    end;

  Result := TempSize;
end;

procedure TCEFViewComponent.SizeToPreferredSize;
begin
  if Initialized then
    AsView.SizeToPreferredSize;
end;

function TCEFViewComponent.GetMinimumSize : TCefSize;
var
  TempSize : TCefSize;
begin
  if Initialized then
    TempSize := AsView.GetMinimumSize
   else
    begin
      TempSize.width  := 0;
      TempSize.height := 0;
    end;

  Result := TempSize;
end;

function TCEFViewComponent.GetMaximumSize : TCefSize;
var
  TempSize : TCefSize;
begin
  if Initialized then
    TempSize := AsView.GetMaximumSize
   else
    begin
      TempSize.width  := 0;
      TempSize.height := 0;
    end;

  Result := TempSize;
end;

function TCEFViewComponent.GetHeightForWidth(width: Integer): Integer;
begin
  if Initialized then
    Result := AsView.GetHeightForWidth(width)
   else
    Result := 0;
end;

function TCEFViewComponent.GetInsets: TCefInsets;
begin
  if Initialized then
    Result := AsView.GetInsets
   else
    begin
      Result.top    := 0;
      Result.left   := 0;
      Result.bottom := 0;
      Result.right  := 0;
    end;
end;

procedure TCEFViewComponent.InvalidateLayout;
begin
  if Initialized then
    AsView.InvalidateLayout;
end;

procedure TCEFViewComponent.SetVisible(visible_: boolean);
begin
  if Initialized then
    AsView.SetVisible(visible_);
end;

function TCEFViewComponent.GetIsVisible : boolean;
begin
  Result := Initialized and AsView.IsVisible;
end;

function TCEFViewComponent.GetIsDrawn : boolean;
begin
  Result := Initialized and AsView.IsDrawn;
end;

procedure TCEFViewComponent.SetEnabled(enabled_: boolean);
begin
  if Initialized then
    AsView.SetEnabled(enabled_);
end;

function TCEFViewComponent.GetIsEnabled : boolean;
begin
  Result := Initialized and AsView.IsEnabled;
end;

procedure TCEFViewComponent.SetFocusable(focusable_: boolean);
begin
  if Initialized then
    AsView.SetFocusable(focusable_);
end;

function TCEFViewComponent.GetIsFocusable : boolean;
begin
  Result := Initialized and AsView.IsFocusable;
end;

function TCEFViewComponent.GetIsAccessibilityFocusable : boolean;
begin
  Result := Initialized and AsView.IsAccessibilityFocusable;
end;

procedure TCEFViewComponent.RequestFocus;
begin
  if Initialized then
    AsView.RequestFocus;
end;

procedure TCEFViewComponent.SetBackgroundColor(color: TCefColor);
begin
  if Initialized then
    AsView.SetBackgroundColor(color);
end;

procedure TCEFViewComponent.SetInsets(const insets: TCefInsets);
begin
  if Initialized then
    AsView.SetInsets(insets);
end;

function TCEFViewComponent.GetBackgroundColor : TCefColor;
begin
  if Initialized then
    Result := AsView.GetBackgroundColor
   else
    Result := 0;
end;

function TCEFViewComponent.GetThemeColor(color_id: integer): TCefColor;
begin
  if Initialized then
    Result := AsView.GetThemeColor(color_id)
   else
    Result := 0;
end;

function TCEFViewComponent.ConvertPointToScreen(var point: TCefPoint): boolean;
begin
  Result := Initialized and AsView.ConvertPointToScreen(point);
end;

function TCEFViewComponent.ConvertPointFromScreen(var point: TCefPoint): boolean;
begin
  Result := Initialized and AsView.ConvertPointFromScreen(point);
end;

function TCEFViewComponent.ConvertPointToWindow(var point: TCefPoint): boolean;
begin
  Result := Initialized and AsView.ConvertPointToWindow(point);
end;

function TCEFViewComponent.ConvertPointFromWindow(var point: TCefPoint): boolean;
begin
  Result := Initialized and AsView.ConvertPointFromWindow(point);
end;

function TCEFViewComponent.ConvertPointToView(const view : ICefView; var point: TCefPoint): boolean;
begin
  Result := Initialized and AsView.ConvertPointToView(view, point);
end;

function TCEFViewComponent.ConvertPointFromView(const view : ICefView; var point: TCefPoint): boolean;
begin
  Result := Initialized and AsView.ConvertPointFromView(view, point);
end;

procedure TCEFViewComponent.doOnGetPreferredSize(const view    : ICefView;
                                                 var   aResult : TCefSize);
begin
  if assigned(FOnGetPreferredSize) then
    FOnGetPreferredSize(self, view, aResult);
end;

procedure TCEFViewComponent.doOnGetMinimumSize(const view    : ICefView;
                                               var   aResult : TCefSize);
begin
  if assigned(FOnGetMinimumSize) then
    FOnGetMinimumSize(self, view, aResult);
end;

procedure TCEFViewComponent.doOnGetMaximumSize(const view    : ICefView;
                                               var   aResult : TCefSize);
begin
  if assigned(FOnGetMaximumSize) then
    FOnGetMaximumSize(self, view, aResult);
end;

procedure TCEFViewComponent.doOnGetHeightForWidth(const view    : ICefView;
                                                        width   : Integer;
                                                  var   aResult : Integer);
begin
  if assigned(FOnGetHeightForWidth) then
    FOnGetHeightForWidth(self, view, width, aResult);
end;

procedure TCEFViewComponent.doOnParentViewChanged(const view   : ICefView;
                                                        added  : boolean;
                                                  const parent : ICefView);
begin
  if assigned(FOnParentViewChanged) then
    FOnParentViewChanged(self, view, added, Parent);
end;

procedure TCEFViewComponent.doOnChildViewChanged(const view  : ICefView;
                                                       added : boolean;
                                                 const child : ICefView);
begin
  if assigned(FOnChildViewChanged) then
    FOnChildViewChanged(self, view, added, child);
end;

procedure TCEFViewComponent.doOnWindowChanged(const view  : ICefView;
                                                    added : boolean);
begin
  if assigned(FOnWindowChanged) then
    FOnWindowChanged(self, view, added);
end;

procedure TCEFViewComponent.doOnLayoutChanged(const view: ICefView; new_bounds: TCefRect);
begin
  if assigned(FOnLayoutChanged) then
    FOnLayoutChanged(self, view, new_bounds);
end;

procedure TCEFViewComponent.doOnFocus(const view: ICefView);
begin
  if assigned(FOnFocus) then
    FOnFocus(self, view);
end;

procedure TCEFViewComponent.doOnBlur(const view: ICefView);
begin
  if assigned(FOnBlur) then
    FOnBlur(self, view);
end;

procedure TCEFViewComponent.doOnThemeChanged(const view: ICefView);
begin
  if assigned(FOnThemeChanged) then
    FOnThemeChanged(self, view);
end;

procedure TCEFViewComponent.doCreateCustomView;
begin
  //
end;

end.
