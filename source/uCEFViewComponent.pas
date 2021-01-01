// ************************************************************************
// ***************************** CEF4Delphi *******************************
// ************************************************************************
//
// CEF4Delphi is based on DCEF3 which uses CEF to embed a chromium-based
// browser in Delphi applications.
//
// The original license of DCEF3 still applies to CEF4Delphi.
//
// For more information about CEF4Delphi visit :
//         https://www.briskbard.com/index.php?lang=en&pageid=cef
//
//        Copyright © 2021 Salvador Diaz Fau. All rights reserved.
//
// ************************************************************************
// ************ vvvv Original license and comments below vvvv *************
// ************************************************************************
(*
 *                       Delphi Chromium Embedded 3
 *
 * Usage allowed under the restrictions of the Lesser GNU General Public License
 * or alternatively the restrictions of the Mozilla Public License 1.1
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * Unit owner : Henri Gourvest <hgourvest@gmail.com>
 * Web site   : http://www.progdigy.com
 * Repository : http://code.google.com/p/delphichromiumembedded/
 * Group      : http://groups.google.com/group/delphichromiumembedded
 *
 * Embarcadero Technologies, Inc is not permitted to use or redistribute
 * this source code without explicit permission.
 *
 *)

unit uCEFViewComponent;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

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
      FOnFocus                   : TOnFocusEvent;
      FOnBlur                    : TOnBlurEvent;

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

      procedure SetID(id_: Integer);
      procedure SetGroupID(group_id: Integer);
      procedure SetBounds(const bounds_: TCefRect);
      procedure SetSize(const size_: TCefSize);
      procedure SetPosition(const position_: TCefPoint);
      procedure SetVisible(visible_: boolean);
      procedure SetEnabled(enabled_: boolean);
      procedure SetFocusable(focusable_: boolean);
      procedure SetBackgroundColor(color: TCefColor);

      // ICefViewDelegateEvents
      procedure doOnGetPreferredSize(const view: ICefView; var aResult : TCefSize); virtual;
      procedure doOnGetMinimumSize(const view: ICefView; var aResult : TCefSize); virtual;
      procedure doOnGetMaximumSize(const view: ICefView; var aResult : TCefSize); virtual;
      procedure doOnGetHeightForWidth(const view: ICefView; width: Integer; var aResult: Integer); virtual;
      procedure doOnParentViewChanged(const view: ICefView; added: boolean; const parent: ICefView); virtual;
      procedure doOnChildViewChanged(const view: ICefView; added: boolean; const child: ICefView); virtual;
      procedure doOnFocus(const view: ICefView); virtual;
      procedure doOnBlur(const view: ICefView); virtual;
      procedure doCreateCustomView; virtual;

    public
      constructor Create(AOwner: TComponent); override;
      procedure   BeforeDestruction; override;

      function    ToStringEx(include_children: boolean): ustring;
      function    IsSame(const that: ICefView): boolean;
      procedure   SizeToPreferredSize;
      procedure   InvalidateLayout;
      procedure   RequestFocus;

      function    ConvertPointToScreen(var point: TCefPoint): boolean;
      function    ConvertPointFromScreen(var point: TCefPoint): boolean;
      function    ConvertPointToWindow(var point: TCefPoint): boolean;
      function    ConvertPointFromWindow(var point: TCefPoint): boolean;
      function    ConvertPointToView(const view : ICefView; var point: TCefPoint): boolean;
      function    ConvertPointFromView(const view : ICefView; var point: TCefPoint): boolean;

      property Initialized                    : boolean                    read GetInitialized;
      property AsView                         : ICefView                   read GetAsView;
      property AsBrowserView                  : ICefBrowserView            read GetAsBrowserView;
      property AsButton                       : ICefButton                 read GetAsButton;
      property AsPanel                        : ICefPanel                  read GetAsPanel;
      property AsScrollView                   : ICefScrollView             read GetAsScrollView;
      property AsTextfield                    : ICefTextfield              read GetAsTextfield;
      property ViewForID[id_: Integer]        : ICefView                   read GetViewForID;
      property Valid                          : boolean                    read GetIsValid;
      property Attached                       : boolean                    read GetIsAttached;
      property Delegate                       : ICefViewDelegate           read GetDelegate;
      property Window                         : ICefWindow                 read GetWindow;
      property ParentView                     : ICefView                   read GetParentView;
      property BoundsInScreen                 : TCefRect                   read GetBoundsInScreen;
      property PreferredSize                  : TCefSize                   read GetPreferredSize;
      property MinimumSize                    : TCefSize                   read GetMinimumSize;
      property MaximumSize                    : TCefSize                   read GetMaximumSize;
      property Visible                        : boolean                    read GetIsVisible                   write SetVisible;
      property Drawn                          : boolean                    read GetIsDrawn;
      property Enabled                        : boolean                    read GetIsEnabled                   write SetEnabled;
      property Focusable                      : boolean                    read GetIsFocusable                 write SetFocusable;
      property AccessibilityFocusable         : boolean                    read GetIsAccessibilityFocusable;
      property BackgroundColor                : TCefColor                  read GetBackgroundColor             write SetBackgroundColor;
      property ID                             : integer                    read GetID                          write SetID;
      property GroupID                        : integer                    read GetGroupID                     write SetGroupID;
      property Bounds                         : TCefRect                   read GetBounds                      write SetBounds;
      property Size                           : TCefSize                   read GetSize                        write SetSize;
      property Position                       : TCefPoint                  read GetPosition                    write SetPosition;
      property TypeString                     : ustring                    read GetTypeString;
      property HeightForWidth[width: Integer] : Integer                    read GetHeightForWidth;

    published
      property OnGetPreferredSize             : TOnGetPreferredSizeEvent   read FOnGetPreferredSize            write FOnGetPreferredSize;
      property OnGetMinimumSize               : TOnGetMinimumSizeEvent     read FOnGetMinimumSize              write FOnGetMinimumSize;
      property OnGetMaximumSize               : TOnGetMaximumSizeEvent     read FOnGetMaximumSize              write FOnGetMaximumSize;
      property OnGetHeightForWidth            : TOnGetHeightForWidthEvent  read FOnGetHeightForWidth           write FOnGetHeightForWidth;
      property OnParentViewChanged            : TOnParentViewChangedEvent  read FOnParentViewChanged           write FOnParentViewChanged;
      property OnChildViewChanged             : TOnChildViewChangedEvent   read FOnChildViewChanged            write FOnChildViewChanged;
      property OnFocus                        : TOnFocusEvent              read FOnFocus                       write FOnFocus;
      property OnBlur                         : TOnBlurEvent               read FOnBlur                        write FOnBlur;
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
  uCEFViewDelegate, uCEFMiscFunctions, uCEFTask;

constructor TCEFViewComponent.Create(AOwner: TComponent);
begin
  inherited Create(aOwner);

  Initialize;
end;

procedure TCEFViewComponent.BeforeDestruction;
begin
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
  FOnFocus             := nil;
  FOnBlur              := nil;
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

function TCEFViewComponent.GetBackgroundColor : TCefColor;
begin
  if Initialized then
    Result := AsView.GetBackgroundColor
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

procedure TCEFViewComponent.doCreateCustomView;
begin
  //
end;

end.
