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
  TCefScrollViewRef = class(TCefViewRef, ICefScrollView)
    protected
      procedure SetContentView(const view: ICefView);
      function  GetContentView : ICefView;
      function  GetVisibleContentRect : TCefRect;
      function  HasHorizontalScrollbar : boolean;
      function  GetHorizontalScrollbarHeight : Integer;
      function  HasVerticalScrollbar : boolean;
      function  GetVerticalScrollbarWidth : Integer;

    public
      class function UnWrap(data: Pointer): ICefScrollView;
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

