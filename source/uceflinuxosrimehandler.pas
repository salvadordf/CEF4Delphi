unit uCEFLinuxOSRIMEHandler;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF LCLGTK2}gtk2, glib2, gdk2,{$ENDIF}
  {$IFDEF LCLGTK3}LazGdk3, LazGtk3, LazGObject2, LazGLib2, gtk3procs, gtk3widgets,{$ENDIF}
  Classes, ExtCtrls, Forms;

type
  TCEFLinuxOSRIMEHandler = class
    protected
      FPanel           : TCustomPanel;
      FForm            : TCustomForm;
      FHasFocus        : boolean;
      {$IF DEFINED(LCLGTK2) or DEFINED(LCLGTK3)}
      FIMContext       : PGtkIMContext;
      {$IFEND}

      function    GetInitialized : boolean;
      procedure   SetPanel(aValue : TCustomPanel);

    public
      constructor Create(aPanel : TCustomPanel);
      destructor  Destroy; override;
      function    CreateContext : boolean;
      procedure   DestroyContext;
      procedure   SetClientWindow;
      procedure   ResetClientWindow;
      function    ConnectSignals : boolean;
      procedure   Focus;
      procedure   Blur;
      procedure   Reset;
      procedure   SetCursorLocation(X, Y: integer);
      {$IF DEFINED(LCLGTK2) or DEFINED(LCLGTK3)}
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
  {$IFDEF FPC}
    {$IF DEFINED(LCLGTK2) or DEFINED(LCLGTK3)}pango, LMessages,{$ENDIF}
    LCLType, LCLIntf,
    {$IFDEF LCLGTK3}uCEFBufferPanel,{$ENDIF}
  {$ENDIF}
  SysUtils, uCEFMiscFunctions;

{$IFDEF LCLGTK3}
// This is just a workaround for the missing implementation of SendMessage in GTK3.
function SendMessage(HandleWnd: HWND; Msg: Cardinal; wParam: WParam; lParam: LParam): LResult;
var
  LMessage : TLMessage;
  LPanel   : TBufferPanel;
begin
  LMessage.Msg    := Msg;
  LMessage.WParam := WParam;
  LMessage.LParam := LParam;
  LMessage.Result := 0;

  LPanel := TBufferPanel(HandleWnd);

  if (LPanel <> nil) then
    LPanel.SendMessage(LMessage);

  Result := LMessage.Result;
end;
{$ENDIF}

{$IF DEFINED(LCLGTK2) or DEFINED(LCLGTK3)}
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
  gtk_im_context_get_preedit_string(context, @TempStr, {$IFDEF LCLGTK3}@{$ENDIF}TempPangoAttr, @TempCurpos);
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
  {$IF DEFINED(LCLGTK2) or DEFINED(LCLGTK3)}
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
  {$IF DEFINED(LCLGTK2) or DEFINED(LCLGTK3)}
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

function TCEFLinuxOSRIMEHandler.CreateContext : boolean;
begin
  Result := False;
  {$IF DEFINED(LCLGTK2) or DEFINED(LCLGTK3)}
  if not(assigned(FIMContext)) then
    begin
      FIMContext := gtk_im_multicontext_new();

      if assigned(FIMContext) then
        begin
          SetClientWindow;
          Result := ConnectSignals;
        end;
    end;
  {$ENDIF}
end;

procedure TCEFLinuxOSRIMEHandler.DestroyContext;
begin
  {$IF DEFINED(LCLGTK2) or DEFINED(LCLGTK3)}
  if assigned(FIMContext) then
    begin
      g_object_unref(FIMContext);
      FIMContext := nil;
    end;       
  {$ENDIF}
end;

procedure TCEFLinuxOSRIMEHandler.SetClientWindow;
{$IF DEFINED(LCLGTK2) or DEFINED(LCLGTK3)}
var
  TempWindow : PGdkWindow;
{$ENDIF}
begin
  if Initialized then
    begin
      {$IFDEF LCLGTK2}
      TempWindow := PGtkWidget(FForm.Handle)^.window;
      gtk_im_context_set_client_window(FIMContext, TempWindow);
      {$ENDIF}
      {$IFDEF LCLGTK3}
      TempWindow := TGtk3Widget(FForm.Handle).Widget^.window;
      gtk_im_context_set_client_window(FIMContext, TempWindow);
      {$ENDIF}
    end;
end;

procedure TCEFLinuxOSRIMEHandler.ResetClientWindow;
begin
  if Initialized then
    begin
      {$IF DEFINED(LCLGTK2) or DEFINED(LCLGTK3)}
      gtk_im_context_reset(FIMContext);
      gtk_im_context_set_client_window(FIMContext, nil);
      {$ENDIF}
    end;
end;

function TCEFLinuxOSRIMEHandler.ConnectSignals: boolean;
var                                        
  TempHandlerID1, TempHandlerID2, TempHandlerID3, TempHandlerID4 : {$IF DEFINED(LCLGTK2) or DEFINED(LCLGTK3)}gulong{$ELSE}cardinal{$ENDIF};
  {$IF DEFINED(LCLGTK2) or DEFINED(LCLGTK3)}
  TempData : GPointer;
  {$ENDIF}
begin             
  TempHandlerID1 := 0;
  TempHandlerID2 := 0;
  TempHandlerID3 := 0;
  TempHandlerID4 := 0;

  try
    try
      if Initialized then
        begin
          {$IFDEF LCLGTK3}
          // This should be the data passed to the callback. We'll enable this line as soon as Lazarus implements SendMessage in GTK3.
          //TempData := GPointer(TGtk3Widget(FPanel.Handle).Widget);
          TempData := GPointer(FPanel);

          TempHandlerID1 := g_signal_connect_data(PGObject(FIMContext), 'commit',          TGCallback(@gtk_commit_cb),          TempData, nil, G_CONNECT_DEFAULT);
          TempHandlerID2 := g_signal_connect_data(PGObject(FIMContext), 'preedit-start',   TGCallback(@gtk_preedit_start_cb),   TempData, nil, G_CONNECT_DEFAULT);
          TempHandlerID3 := g_signal_connect_data(PGObject(FIMContext), 'preedit-end',     TGCallback(@gtk_preedit_end_cb),     TempData, nil, G_CONNECT_DEFAULT);
          TempHandlerID4 := g_signal_connect_data(PGObject(FIMContext), 'preedit-changed', TGCallback(@gtk_preedit_changed_cb), TempData, nil, G_CONNECT_DEFAULT);
          {$ENDIF}
          {$IFDEF LCLGTK2}
          TempData := GPointer(FPanel.Handle);

          TempHandlerID1 := g_signal_connect(G_OBJECT(FIMContext), 'commit',          G_CALLBACK(@gtk_commit_cb),          TempData);
          TempHandlerID2 := g_signal_connect(G_OBJECT(FIMContext), 'preedit-start',   G_CALLBACK(@gtk_preedit_start_cb),   TempData);
          TempHandlerID3 := g_signal_connect(G_OBJECT(FIMContext), 'preedit-end',     G_CALLBACK(@gtk_preedit_end_cb),     TempData);
          TempHandlerID4 := g_signal_connect(G_OBJECT(FIMContext), 'preedit-changed', G_CALLBACK(@gtk_preedit_changed_cb), TempData);
          {$ENDIF}
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TCEFLinuxOSRIMEHandler.ConnectSignals', e) then raise;
    end;
  finally
    Result := (TempHandlerID1 > 0) and (TempHandlerID2 > 0) and (TempHandlerID3 > 0) and (TempHandlerID4 > 0);
  end;
end;

procedure TCEFLinuxOSRIMEHandler.Focus;
begin
  if Initialized then
    begin
      {$IF DEFINED(LCLGTK2) or DEFINED(LCLGTK3)}
      gtk_im_context_focus_in(FIMContext);
      {$ENDIF}
      FHasFocus := True;
    end;
end;

procedure TCEFLinuxOSRIMEHandler.Blur;
begin
  if Initialized then
    begin
      {$IF DEFINED(LCLGTK2) or DEFINED(LCLGTK3)}
      gtk_im_context_focus_out(FIMContext);
      {$ENDIF}
      FHasFocus := False;
    end;
end;

procedure TCEFLinuxOSRIMEHandler.Reset;
begin
  if Initialized then
    begin
      {$IF DEFINED(LCLGTK2) or DEFINED(LCLGTK3)}
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
{$IF DEFINED(LCLGTK2) or DEFINED(LCLGTK3)}
var
  TempCurPos: TGdkRectangle;
{$ENDIF}
begin
  if Initialized then
    begin
      {$IF DEFINED(LCLGTK2) or DEFINED(LCLGTK3)}
      TempCurPos.x      := x;
      TempCurPos.y      := y;
      TempCurPos.width  := 0;
      TempCurPos.height := 0;

      gtk_im_context_set_cursor_location(FIMContext, @TempCurPos);
      {$ENDIF}
    end;
end;

{$IF DEFINED(LCLGTK2) or DEFINED(LCLGTK3)}
function TCEFLinuxOSRIMEHandler.FilterKeyPress(aEvent : PGdkEventKey) : boolean;
begin
  if Initialized then
    Result := gtk_im_context_filter_keypress(FIMContext, aEvent);
end;
{$ENDIF}

end.

