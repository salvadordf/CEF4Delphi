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
  /// <summary>
  /// A Panel is a container in the views hierarchy that can contain other Views
  /// as children. Methods must be called on the browser process UI thread unless
  /// otherwise indicated.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/views/cef_panel_capi.h">CEF source file: /include/capi/views/cef_panel_capi.h (cef_panel_t)</see></para>
  /// </remarks>
  TCefPanelRef = class(TCefViewRef, ICefPanel)
    protected
      /// <summary>
      /// Returns this Panel as a Window or NULL if this is not a Window.
      /// </summary>
      function  GetAsWindow : ICefWindow;
      /// <summary>
      /// Set this Panel's Layout to FillLayout and return the FillLayout object.
      /// </summary>
      function  SetToFillLayout : ICefFillLayout;
      /// <summary>
      /// Set this Panel's Layout to BoxLayout and return the BoxLayout object.
      /// </summary>
      function  SetToBoxLayout(const settings: TCefBoxLayoutSettings): ICefBoxLayout;
      /// <summary>
      /// Get the Layout.
      /// </summary>
      function  GetLayout : ICefLayout;
      /// <summary>
      /// Lay out the child Views (set their bounds based on sizing heuristics
      /// specific to the current Layout).
      /// </summary>
      procedure Layout;
      /// <summary>
      /// Add a child View.
      /// </summary>
      procedure AddChildView(const view: ICefView);
      /// <summary>
      /// Add a child View at the specified |index|. If |index| matches the result
      /// of GetChildCount() then the View will be added at the end.
      /// </summary>
      procedure AddChildViewAt(const view: ICefView; index: Integer);
      /// <summary>
      /// Move the child View to the specified |index|. A negative value for |index|
      /// will move the View to the end.
      /// </summary>
      procedure ReorderChildView(const view: ICefView; index: Integer);
      /// <summary>
      /// Remove a child View. The View can then be added to another Panel.
      /// </summary>
      procedure RemoveChildView(const view: ICefView);
      /// <summary>
      /// Remove all child Views. The removed Views will be deleted if the client
      /// holds no references to them.
      /// </summary>
      procedure RemoveAllChildViews;
      /// <summary>
      /// Returns the number of child Views.
      /// </summary>
      function  GetChildViewCount : NativeUInt;
      /// <summary>
      /// Returns the child View at the specified |index|.
      /// </summary>
      function  GetChildViewAt(index: Integer): ICefView;

    public
      /// <summary>
      /// Returns a ICefPanel instance using a PCefPanel data pointer.
      /// </summary>
      class function UnWrap(data: Pointer): ICefPanel;
      /// <summary>
      /// Create a new Panel.
      /// </summary>
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

