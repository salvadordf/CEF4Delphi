unit uCEFScrollViewComponent;

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
  uCEFTypes, uCEFInterfaces, uCEFConstants, uCEFViewsFrameworkEvents, uCEFViewComponent;

type
  {$IFNDEF FPC}{$IFDEF DELPHI16_UP}[ComponentPlatformsAttribute(pfidWindows or pfidOSX or pfidLinux)]{$ENDIF}{$ENDIF}
  TCEFScrollViewComponent = class(TCEFViewComponent)
    protected
      FScrollView                  : ICefScrollView;

      procedure DestroyView; override;
      procedure Initialize; override;

      function  GetInitialized : boolean; override;
      function  GetAsView : ICefView; override;
      function  GetAsScrollView : ICefScrollView; override;
      function  GetContentView : ICefView;
      function  GetVisibleContentRect : TCefRect;
      function  GetHasHorizontalScrollbar : boolean;
      function  GetHorizontalScrollbarHeight : Integer;
      function  GetHasVerticalScrollbar : boolean;
      function  GetVerticalScrollbarWidth : Integer;

      procedure SetContentView(const view: ICefView);

      // ICefViewDelegateEvents
      procedure doCreateCustomView; override;

    public
      /// <summary>
      /// Create a new ScrollView.
      /// </summary>
      procedure CreateScrollView;
      /// <summary>
      /// Get and set the content View. The content View must have a specified size (e.g.
      /// via ICefView.SetBounds or ICefViewDelegate.GetPreferredSize).
      /// </summary>
      property  ContentView               : ICefView      read GetContentView                 write SetContentView;
      /// <summary>
      /// Returns the visible region of the content View.
      /// </summary>
      property  VisibleContentRect        : TCefRect      read GetVisibleContentRect;
      /// <summary>
      /// Returns the height of the horizontal scrollbar.
      /// </summary>
      property  HorizontalScrollbarHeight : Integer       read GetHorizontalScrollbarHeight;
      /// <summary>
      /// Returns the width of the vertical scrollbar.
      /// </summary>
      property  VerticalScrollbarWidth    : Integer       read GetVerticalScrollbarWidth;
      /// <summary>
      /// Returns true (1) if the horizontal scrollbar is currently showing.
      /// </summary>
      property  HasHorizontalScrollbar    : boolean       read GetHasHorizontalScrollbar;
      /// <summary>
      /// Returns true (1) if the vertical scrollbar is currently showing.
      /// </summary>
      property  HasVerticalScrollbar      : boolean       read GetHasVerticalScrollbar;
  end;

{$IFDEF FPC}
procedure Register;
{$ENDIF}

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
  uCEFScrollView, uCEFMiscFunctions, uCEFViewDelegate;

procedure TCEFScrollViewComponent.CreateScrollView;
begin
  CreateView;
end;

procedure TCEFScrollViewComponent.doCreateCustomView;
var
  TempDelegate : ICefViewDelegate;
begin
  if (FScrollView = nil) then
    try
      TempDelegate := TCustomViewDelegate.Create(self);
      FScrollView  := TCefScrollViewRef.CreateScrollView(TempDelegate);
    finally
      TempDelegate := nil;
    end;
end;

procedure TCEFScrollViewComponent.Initialize;
begin
  inherited Initialize;

  FScrollView := nil;
end;

procedure TCEFScrollViewComponent.DestroyView;
begin
  FScrollView := nil;
end;

function TCEFScrollViewComponent.GetInitialized : boolean;
begin
  Result := (FScrollView <> nil);
end;

function TCEFScrollViewComponent.GetAsView : ICefView;
begin
  Result := FScrollView as ICefView;
end;

function TCEFScrollViewComponent.GetAsScrollView : ICefScrollView;
begin
  Result := FScrollView;
end;

procedure TCEFScrollViewComponent.SetContentView(const view: ICefView);
begin
  if Initialized then FScrollView.SetContentView(view);
end;

function TCEFScrollViewComponent.GetContentView : ICefView;
begin
  if Initialized then
    Result := FScrollView.GetContentView
   else
    Result := nil;
end;

function TCEFScrollViewComponent.GetVisibleContentRect : TCefRect;
var
  TempRect : TCefRect;
begin
  if Initialized then
    TempRect := FScrollView.GetVisibleContentRect
   else
    begin
      TempRect.x      := 0;
      TempRect.y      := 0;
      TempRect.width  := 0;
      TempRect.height := 0;
    end;

  Result := TempRect;
end;

function TCEFScrollViewComponent.GetHasHorizontalScrollbar : boolean;
begin
  Result := Initialized and FScrollView.HasHorizontalScrollbar;
end;

function TCEFScrollViewComponent.GetHorizontalScrollbarHeight : Integer;
begin
  if Initialized then
    Result := FScrollView.GetHorizontalScrollbarHeight
   else
    Result := 0;
end;

function TCEFScrollViewComponent.GetHasVerticalScrollbar : boolean;
begin
  Result := Initialized and FScrollView.HasVerticalScrollbar;
end;

function TCEFScrollViewComponent.GetVerticalScrollbarWidth : Integer;
begin
  if Initialized then
    Result := FScrollView.GetVerticalScrollbarWidth
   else
    Result := 0;
end;

{$IFDEF FPC}
procedure Register;
begin
  {$I res/tcefscrollviewcomponent.lrs}
  RegisterComponents('Chromium Views Framework', [TCEFScrollViewComponent]);
end;
{$ENDIF}

end.
