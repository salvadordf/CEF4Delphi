unit uCEFScrollView;

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
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes, uCEFView;

type
  /// <summary>
  /// A ScrollView will show horizontal and/or vertical scrollbars when necessary
  /// based on the size of the attached content view. Methods must be called on
  /// the browser process UI thread unless otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_scroll_view_capi.h">CEF source file: /include/capi/views/cef_scroll_view_capi.h (cef_scroll_view_t)</see></para>
  /// </remarks>
  TCefScrollViewRef = class(TCefViewRef, ICefScrollView)
    protected
      /// <summary>
      /// Set the content View. The content View must have a specified size (e.g.
      /// via ICefView.SetBounds or ICefViewDelegate.GetPreferredSize).
      /// </summary>
      procedure SetContentView(const view: ICefView);
      /// <summary>
      /// Returns the content View.
      /// </summary>
      function  GetContentView : ICefView;
      /// <summary>
      /// Returns the visible region of the content View.
      /// </summary>
      function  GetVisibleContentRect : TCefRect;
      /// <summary>
      /// Returns true (1) if the horizontal scrollbar is currently showing.
      /// </summary>
      function  HasHorizontalScrollbar : boolean;
      /// <summary>
      /// Returns the height of the horizontal scrollbar.
      /// </summary>
      function  GetHorizontalScrollbarHeight : Integer;
      /// <summary>
      /// Returns true (1) if the vertical scrollbar is currently showing.
      /// </summary>
      function  HasVerticalScrollbar : boolean;
      /// <summary>
      /// Returns the width of the vertical scrollbar.
      /// </summary>
      function  GetVerticalScrollbarWidth : Integer;

    public
      /// <summary>
      /// Returns a ICefScrollView instance using a PCefScrollView data pointer.
      /// </summary>
      class function UnWrap(data: Pointer): ICefScrollView;
      /// <summary>
      /// Create a new ScrollView.
      /// </summary>
      class function CreateScrollView(const delegate: ICefViewDelegate): ICefScrollView;
  end;

implementation

uses
  uCEFLibFunctions, uCEFMiscFunctions;

procedure TCefScrollViewRef.SetContentView(const view: ICefView);
begin
  PCefScrollView(FData)^.set_content_view(PCefScrollView(FData),
                                          CefGetData(view));
end;

function TCefScrollViewRef.GetContentView : ICefView;
begin
  Result := TCefViewRef.UnWrap(PCefScrollView(FData)^.get_content_view(PCefScrollView(FData)));
end;

function TCefScrollViewRef.GetVisibleContentRect : TCefRect;
begin
  Result := PCefScrollView(FData)^.get_visible_content_rect(PCefScrollView(FData));
end;

function TCefScrollViewRef.HasHorizontalScrollbar : boolean;
begin
  Result := (PCefScrollView(FData)^.has_horizontal_scrollbar(PCefScrollView(FData)) <> 0);
end;

function TCefScrollViewRef.GetHorizontalScrollbarHeight : Integer;
begin
  Result := PCefScrollView(FData)^.get_horizontal_scrollbar_height(PCefScrollView(FData));
end;

function TCefScrollViewRef.HasVerticalScrollbar : boolean;
begin
  Result := (PCefScrollView(FData)^.has_vertical_scrollbar(PCefScrollView(FData)) <> 0);
end;

function TCefScrollViewRef.GetVerticalScrollbarWidth : Integer;
begin
  Result := PCefScrollView(FData)^.get_vertical_scrollbar_width(PCefScrollView(FData));
end;

class function TCefScrollViewRef.UnWrap(data: Pointer): ICefScrollView;
begin
  if (data <> nil) then
    Result := Create(data) as ICefScrollView
   else
    Result := nil;
end;

class function TCefScrollViewRef.CreateScrollView(const delegate: ICefViewDelegate): ICefScrollView;
var
  TempScrollView : PCefScrollView;
begin
  Result := nil;

  if (delegate <> nil) then
    begin
      TempScrollView := cef_scroll_view_create(CefGetData(delegate));

      if (TempScrollView <> nil) then
        Result := Create(TempScrollView) as ICefScrollView;
    end;
end;

end.

