unit uCEFLinuxOSRIMEHandler;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF LCLGTK2}gtk2, glib2, gdk2,{$ENDIF}
  Classes, ExtCtrls, Forms;

type
  TCEFLinuxOSRIMEHandler = class
    protected
      FPanel           : TCustomPanel;
      FForm            : TCustomForm;
      FHasFocus        : boolean;
      {$IFDEF LCLGTK2}
      FIMContext       : PGtkIMContext;
      {$ENDIF}

      function    GetInitialized : boolean;
      procedure   SetPanel(aValue : TCustomPanel);

    public
      constructor Create(aPanel : TCustomPanel);
      destructor  Destroy; override;
      procedure   CreateContext;
      procedure   DestroyContext;
      procedure   SetClientWindow;
      procedure   ResetClientWindow;
      procedure   ConnectSignals;
      procedure   Focus;
      procedure   Blur;
      procedure   Reset;
      procedure   SetCursorLocation(X, Y: integer);
      {$IFDEF LCLGTK2}
      function    FilterKeyPress(aEvent : PGdkEventKey) : boolean;
      {$ENDIF}

      property Initialized : boolean      read GetInitialized;
      property HasFocus    : boolean      read FHasFocus;
      property Panel       : TCustomPanel read FPanel             write SetPanel;
  end;

implementation

// https://chromium.googlesource.com/chromium/src/+/4079d37114e1dd416e99d5edc535f4214b787fc7/chrome/browser/ui/gtk/input_method_context_impl_gtk.cc  
// https://chromium.googlesource.com/chromium/src/+/refs/heads/main/ui/gtk/input_method_context_impl_gtk.cc

uses
  {$IFDEF LCLGTK2}pango,{$ENDIF}
  {$IFDEF FPC}LCLType, LCLIntf, LMessages,{$ENDIF}
  SysUtils;

{$IFDEF LCLGTK2}
procedure gtk_commit_cb({%H-}context: PGtkIMContext; const Str: Pgchar; {%H-}Data: Pointer); cdecl;
begin
  SendMessage(HWND(Data), LM_IM_COMPOSITION, GTK_IM_FLAG_COMMIT, LPARAM(Str));
end;

procedure gtk_preedit_start_cb({%H-}context: PGtkIMContext; {%H-}Data: Pointer); cdecl;
begin
  SendMessage(HWND(Data), LM_IM_COMPOSITION, GTK_IM_FLAG_START, LPARAM(context));
end;

procedure gtk_preedit_end_cb({%H-}context: PGtkIMContext; {%H-}Data: Pointer); cdecl;
begin
  SendMessage(HWND(Data), LM_IM_COMPOSITION, GTK_IM_FLAG_END, LPARAM(context));
end;

procedure gtk_preedit_changed_cb({%H-}context:PGtkIMContext; {%H-}Data:Pointer); cdecl;
var
  TempStr       : Pgchar;
  TempPangoAttr : PPangoAttrList;
  TempCurpos    : gint;
begin
  gtk_im_context_get_preedit_string(context, @TempStr, TempPangoAttr, @TempCurpos);
  SendMessage(HWND(Data), LM_IM_COMPOSITION, GTK_IM_FLAG_PREEDIT, LPARAM(pchar(TempStr)));
  g_free(TempStr);
  pango_attr_list_unref(TempPangoAttr);
end;
{$ENDIF}

constructor TCEFLinuxOSRIMEHandler.Create(aPanel : TCustomPanel);
begin
  inherited Create;

  FPanel     := aPanel;
  FHasFocus  := False;
  {$IFDEF LCLGTK2}
  FIMContext := nil;
  {$ENDIF}

  if assigned(FPanel) then
    FForm := GetParentForm(FPanel)
   else
    FForm := nil;
end;

destructor TCEFLinuxOSRIMEHandler.Destroy;
begin
  ResetClientWindow;
  DestroyContext;

  inherited Destroy;
end;

function TCEFLinuxOSRIMEHandler.GetInitialized : boolean;
begin
  {$IFDEF LCLGTK2}
  Result := assigned(FPanel) and assigned(FIMContext);
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

procedure TCEFLinuxOSRIMEHandler.SetPanel(aValue : TCustomPanel);

begin
  FPanel := aValue;

  if assigned(FPanel) then
    FForm := GetParentForm(FPanel)
   else
    FForm := nil;
end;

procedure TCEFLinuxOSRIMEHandler.CreateContext;
begin
  {$IFDEF LCLGTK2}
  if not(assigned(FIMContext)) then
    begin
      FIMContext := gtk_im_multicontext_new();
      SetClientWindow;
      ConnectSignals;
    end;
  {$ENDIF}
end;

procedure TCEFLinuxOSRIMEHandler.DestroyContext;
begin
  {$IFDEF LCLGTK2}
  if assigned(FIMContext) then
    begin
      g_object_unref(FIMContext);
      FIMContext := nil;
    end;       
  {$ENDIF}
end;

procedure TCEFLinuxOSRIMEHandler.SetClientWindow;
{$IFDEF LCLGTK2}
var
  TempWidget : PGtkWidget;
{$ENDIF}
begin
  if Initialized then
    begin
      {$IFDEF LCLGTK2}
      TempWidget := PGtkWidget(FForm.Handle);
      gtk_im_context_set_client_window(FIMContext, TempWidget^.window);
      {$ENDIF}
    end;
end;

procedure TCEFLinuxOSRIMEHandler.ResetClientWindow;
begin
  if Initialized then
    begin
      {$IFDEF LCLGTK2}
      gtk_im_context_reset(FIMContext);
      gtk_im_context_set_client_window(FIMContext, nil);
      {$ENDIF}
    end;
end;

procedure TCEFLinuxOSRIMEHandler.ConnectSignals;
begin
  if Initialized then
    begin
      {$IFDEF LCLGTK2}
      g_signal_connect(G_OBJECT(FIMContext), 'commit',          G_CALLBACK(@gtk_commit_cb),          GPointer(FPanel.Handle));
      g_signal_connect(G_OBJECT(FIMContext), 'preedit-start',   G_CALLBACK(@gtk_preedit_start_cb),   GPointer(FPanel.Handle));
      g_signal_connect(G_OBJECT(FIMContext), 'preedit-end',     G_CALLBACK(@gtk_preedit_end_cb),     GPointer(FPanel.Handle));
      g_signal_connect(G_OBJECT(FIMContext), 'preedit-changed', G_CALLBACK(@gtk_preedit_changed_cb), GPointer(FPanel.Handle));
      {$ENDIF}
    end;
end;

procedure TCEFLinuxOSRIMEHandler.Focus;
begin
  if Initialized then
    begin
      {$IFDEF LCLGTK2}
      gtk_im_context_focus_in(FIMContext);
      {$ENDIF}
      FHasFocus := True;
    end;
end;

procedure TCEFLinuxOSRIMEHandler.Blur;
begin
  if Initialized then
    begin
      {$IFDEF LCLGTK2}
      gtk_im_context_focus_out(FIMContext);
      {$ENDIF}
      FHasFocus := False;
    end;
end;

procedure TCEFLinuxOSRIMEHandler.Reset;
begin
  if Initialized then
    begin
      {$IFDEF LCLGTK2}
      gtk_im_context_reset(FIMContext);

      // Some input methods may not honour the reset call.
      // Focusing out/in the to make sure it gets reset correctly.
      if FHasFocus then
        begin
          gtk_im_context_focus_out(FIMContext);
          gtk_im_context_focus_in(FIMContext);
        end;
      {$ENDIF}
    end;
end;

procedure TCEFLinuxOSRIMEHandler.SetCursorLocation(X, Y: integer);
{$IFDEF LCLGTK2}
var
  TempCurPos: TGdkRectangle;
{$ENDIF}
begin
  if Initialized then
    begin
      {$IFDEF LCLGTK2}
      TempCurPos.x      := x;
      TempCurPos.y      := y;
      TempCurPos.width  := 0;
      TempCurPos.height := 0;

      gtk_im_context_set_cursor_location(FIMContext, @TempCurPos);
      {$ENDIF}
    end;
end;

{$IFDEF LCLGTK2}
function TCEFLinuxOSRIMEHandler.FilterKeyPress(aEvent : PGdkEventKey) : boolean;
begin
  if Initialized then
    Result := gtk_im_context_filter_keypress(FIMContext, aEvent);
end;
{$ENDIF}

end.

