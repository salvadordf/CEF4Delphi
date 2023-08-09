unit uCEFPanel;

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
  TCefPanelRef = class(TCefViewRef, ICefPanel)
    protected
      function  GetAsWindow : ICefWindow;
      function  SetToFillLayout : ICefFillLayout;
      function  SetToBoxLayout(const settings: TCefBoxLayoutSettings): ICefBoxLayout;
      function  GetLayout : ICefLayout;
      procedure Layout;
      procedure AddChildView(const view: ICefView);
      procedure AddChildViewAt(const view: ICefView; index: Integer);
      procedure ReorderChildView(const view: ICefView; index: Integer);
      procedure RemoveChildView(const view: ICefView);
      procedure RemoveAllChildViews;
      function  GetChildViewCount : NativeUInt;
      function  GetChildViewAt(index: Integer): ICefView;

    public
      class function UnWrap(data: Pointer): ICefPanel;
      class function CreatePanel(const delegate: ICefPanelDelegate): ICefPanel;
  end;

implementation

uses
  uCEFLibFunctions, uCEFMiscFunctions, uCEFWindow, uCEFLayout, uCEFFillLayout,
  uCEFBoxLayout;

function TCefPanelRef.GetAsWindow : ICefWindow;
begin
  Result := TCefWindowRef.UnWrap(PCefPanel(FData)^.as_window(PCefPanel(FData)));
end;

function TCefPanelRef.SetToFillLayout : ICefFillLayout;
begin
  Result := TCefFillLayoutRef.UnWrap(PCefPanel(FData)^.set_to_fill_layout(PCefPanel(FData)));
end;

function TCefPanelRef.SetToBoxLayout(const settings: TCefBoxLayoutSettings): ICefBoxLayout;
begin
  Result := TCefBoxLayoutRef.UnWrap(PCefPanel(FData)^.set_to_box_layout(PCefPanel(FData), @settings));
end;

function TCefPanelRef.GetLayout : ICefLayout;
begin
  Result := TCefLayoutRef.UnWrap(PCefPanel(FData)^.get_layout(PCefPanel(FData)));
end;

procedure TCefPanelRef.Layout;
begin
  PCefPanel(FData)^.layout(PCefPanel(FData));
end;

procedure TCefPanelRef.AddChildView(const view: ICefView);
begin
  PCefPanel(FData)^.add_child_view(PCefPanel(FData), CefGetData(view));
end;

procedure TCefPanelRef.AddChildViewAt(const view: ICefView; index: Integer);
begin
  PCefPanel(FData)^.add_child_view_at(PCefPanel(FData), CefGetData(view), index);
end;

procedure TCefPanelRef.ReorderChildView(const view: ICefView; index: Integer);
begin
  PCefPanel(FData)^.reorder_child_view(PCefPanel(FData), CefGetData(view), index);
end;

procedure TCefPanelRef.RemoveChildView(const view: ICefView);
begin
  PCefPanel(FData)^.remove_child_view(PCefPanel(FData), CefGetData(view));
end;

procedure TCefPanelRef.RemoveAllChildViews;
begin
  PCefPanel(FData)^.remove_all_child_views(PCefPanel(FData));
end;

function TCefPanelRef.GetChildViewCount : NativeUInt;
begin
  Result := PCefPanel(FData)^.get_child_view_count(PCefPanel(FData));
end;

function TCefPanelRef.GetChildViewAt(index: Integer): ICefView;
begin
  Result := TCefViewRef.UnWrap(PCefPanel(FData)^.get_child_view_at(PCefPanel(FData), index));
end;

class function TCefPanelRef.UnWrap(data: Pointer): ICefPanel;
begin
  if (data <> nil) then
    Result := Create(data) as ICefPanel
   else
    Result := nil;
end;

class function TCefPanelRef.CreatePanel(const delegate: ICefPanelDelegate): ICefPanel;
var
  TempPanel : PCefPanel;
begin
  Result := nil;

  if (delegate <> nil) then
    begin
      TempPanel := cef_panel_create(CefGetData(delegate));

      if (TempPanel <> nil) then
        Result := Create(TempPanel) as ICefPanel;
    end;
end;

end.

