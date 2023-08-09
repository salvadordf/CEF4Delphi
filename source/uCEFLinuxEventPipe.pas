unit uCEFLinuxEventPipe;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

{$IFDEF LINUX}
uses
  Classes, SysUtils, Forms, Unix;

type
  gint          = integer;
  pgint         = ^gint;
  guint         = longword;
  gushort       = word;
  gboolean      = boolean32;
  gpointer      = Pointer;
  Pgpointer     = ^gpointer;
  PGMainContext = Pointer;
  PGSource      = ^TGSource;
  PGSourceFunc  = ^TGSourceFunc;
  PGSourceFuncs = ^TGSourceFuncs;
  PGSList       = ^TGSList;

  TGSList = record
    data : gpointer;
    next : PGSList;
  end;

  TGSourceFunc = function (data:gpointer):gboolean;cdecl;

  TGSourceDummyMarshal = procedure;cdecl;

  TGSourceFuncs = record
    prepare          : function  (source:PGSource; timeout:pgint):gboolean; cdecl;
    check            : function  (source:PGSource):gboolean; cdecl;
    dispatch         : function  (source:PGSource; callback:TGSourceFunc; user_data:gpointer):gboolean; cdecl;
    finalize         : procedure (source:PGSource); cdecl;
    closure_callback : TGSourceFunc;
    closure_marshal  : TGSourceDummyMarshal;
  end;

  PGSourceCallbackFuncs = ^TGSourceCallbackFuncs;
  TGSourceCallbackFuncs = record
    ref   : procedure (cb_data:gpointer); cdecl;
    unref : procedure (cb_data:gpointer); cdecl;
    get   : procedure (cb_data:gpointer; source:PGSource; func:PGSourceFunc; data:Pgpointer); cdecl;
  end;

  TGSource = record
    callback_data  : gpointer;
    callback_funcs : PGSourceCallbackFuncs;
    source_funcs   : PGSourceFuncs;
    ref_count      : guint;
    context        : PGMainContext;
    priority       : gint;
    flags          : guint;
    source_id      : guint;
    poll_fds       : PGSList;
    prev           : PGSource;
    next           : PGSource;
    reserved1      : gpointer;
    reserved2      : gpointer;
  end;

  TCEFLinuxEventPipe = class;

  PCustomGSource = ^TCustomGSource;
  TCustomGSource = record
    base   : TGSource;
    parent : TCEFLinuxEventPipe;
  end;

  PGPollFD = ^TGPollFD;
  TGPollFD = record
    fd      : gint;
    events  : gushort;
    revents : gushort;
  end;

  TOnPrepareEvent = procedure(Sender: TObject; var aTimeout: integer) of object;
  TOnCheckEvent   = procedure(Sender: TObject; var aMustDispatch: boolean) of object;

  TCEFLinuxEventPipe = class
    protected
      FLibHandle          : {$IFDEF FPC}TLibHandle{$ELSE}THandle{$ENDIF};
      FWakeupGPollFD      : TGPollFD;
      FWorkSource         : PCustomGSource;
      FSourceFuncs        : TGSourceFuncs;
      FWakeupPipeRead     : integer;
      FWakeupPipeWrite    : integer;
      FPendingRead        : integer;
      FLibName            : string;
      FOnPrepare          : TOnPrepareEvent;
      FOnCheck            : TOnCheckEvent;
      FOnDispatch         : TNotifyEvent;

      function  GetInitialized : boolean;
      function  GetHasData : boolean;
      function  GetHasPendingData : boolean;

      function  DoOnPrepare : integer;
      function  DoOnCheck : boolean;
      procedure DoOnDispatch;

    public
      constructor Create;
      destructor  Destroy; override;
      procedure   InitializePipe;
      function    ReadAll(var aValue : integer) : boolean;
      function    Read(var aValue : integer) : boolean;
      function    Write(aValue : integer) : boolean;

      property Initialized    : boolean          read GetInitialized;
      property HasData        : boolean          read GetHasData;
      property HasPendingData : boolean          read GetHasPendingData;
      property LibName        : string           read FLibName         write FLibName;
      property OnPrepare      : TOnPrepareEvent  read FOnPrepare       write FOnPrepare;
      property OnCheck        : TOnCheckEvent    read FOnCheck         write FOnCheck;
      property OnDispatch     : TNotifyEvent     read FOnDispatch      write FOnDispatch;
  end;
{$ENDIF}

implementation

{$IFDEF LINUX}
const
  G_IO_IN                 = 1;
  G_PRIORITY_DEFAULT_IDLE = 200;

var
  g_main_context_default   : function:PGMainContext;cdecl;
  g_source_new             : function(source_funcs:PGSourceFuncs; struct_size:guint):PGSource;cdecl;
  g_source_add_poll        : procedure(source:PGSource; fd:PGPollFD);cdecl;
  g_source_set_priority    : procedure(source:PGSource; priority:gint);cdecl;
  g_source_set_can_recurse : procedure(source:PGSource; can_recurse:gboolean);cdecl;
  g_source_attach          : function(source:PGSource; context:PGMainContext):guint;cdecl;
  g_source_destroy         : procedure(source:PGSource);cdecl;
  g_source_unref           : procedure(source:PGSource);cdecl;

function WorkSourcePrepare(source:PGSource; timeout:pgint):gboolean; cdecl;
begin
  timeout^ := PCustomGSource(source)^.parent.DoOnPrepare();

  // We always return FALSE, so that our timeout is honored.  If we were
  // to return TRUE, the timeout would be considered to be 0 and the poll
  // would never block.  Once the poll is finished, Check will be called.
  Result := False;
end;

function WorkSourceCheck(source:PGSource):gboolean; cdecl;
begin
  // Only return TRUE if Dispatch should be called.
  Result := PCustomGSource(source)^.parent.DoOnCheck();
end;

function WorkSourceDispatch(source:PGSource; callback:TGSourceFunc; user_data:gpointer):gboolean; cdecl;
begin
  PCustomGSource(source)^.parent.DoOnDispatch();

  // Always return TRUE so our source stays registered.
  Result := True;
end;


constructor TCEFLinuxEventPipe.Create;
begin
  inherited Create;

  FLibName         := 'libglib-2.0.so';
  FLibHandle       := 0;
  FPendingRead     := 0;
  FWorkSource      := nil;
  FWakeupPipeRead  := 0;
  FWakeupPipeWrite := 0;
  FOnPrepare       := nil;
  FOnCheck         := nil;
  FOnDispatch      := nil;

  FWakeupGPollFD.fd      := 0;
  FWakeupGPollFD.events  := 0;
  FWakeupGPollFD.revents := 0;

  FSourceFuncs.prepare          := nil;
  FSourceFuncs.check            := nil;
  FSourceFuncs.dispatch         := nil;
  FSourceFuncs.finalize         := nil;
  FSourceFuncs.closure_callback := nil;       
  FSourceFuncs.closure_marshal  := nil;
end;

destructor TCEFLinuxEventPipe.Destroy;
begin
  try
    if assigned(FWorkSource) then
      begin
        g_source_destroy(PGSource(FWorkSource));
        g_source_unref(PGSource(FWorkSource));
        FWorkSource := nil;
      end;

    if (FWakeupPipeRead <> 0) then
      begin
        FileClose(FWakeupPipeRead);
        FWakeupPipeRead := 0;
      end;

    if (FWakeupPipeWrite <> 0) then
      begin
        FileClose(FWakeupPipeWrite);
        FWakeupPipeWrite := 0;
      end;

    if (FLibHandle <> 0) then
      begin
        FreeLibrary(FLibHandle);
        FLibHandle := 0;
      end;
  finally
    inherited Destroy;
  end;
end;

procedure TCEFLinuxEventPipe.InitializePipe;
var
  TempContext : PGMainContext;
begin
  {$IFDEF FPC}
  FLibHandle := LoadLibrary(FLibName);
  {$ELSE}
  FLibHandle := LoadLibrary(PChar(FLibName));
  {$ENDIF}

  if FLibHandle = 0 then
    exit;

  {$IFDEF FPC}Pointer({$ENDIF}g_main_context_default{$IFDEF FPC}){$ENDIF}   := GetProcAddress(FLibHandle, 'g_main_context_default');
  {$IFDEF FPC}Pointer({$ENDIF}g_source_new{$IFDEF FPC}){$ENDIF}             := GetProcAddress(FLibHandle, 'g_source_new');
  {$IFDEF FPC}Pointer({$ENDIF}g_source_add_poll{$IFDEF FPC}){$ENDIF}        := GetProcAddress(FLibHandle, 'g_source_add_poll');
  {$IFDEF FPC}Pointer({$ENDIF}g_source_set_priority{$IFDEF FPC}){$ENDIF}    := GetProcAddress(FLibHandle, 'g_source_set_priority');
  {$IFDEF FPC}Pointer({$ENDIF}g_source_set_can_recurse{$IFDEF FPC}){$ENDIF} := GetProcAddress(FLibHandle, 'g_source_set_can_recurse');
  {$IFDEF FPC}Pointer({$ENDIF}g_source_attach{$IFDEF FPC}){$ENDIF}          := GetProcAddress(FLibHandle, 'g_source_attach');
  {$IFDEF FPC}Pointer({$ENDIF}g_source_destroy{$IFDEF FPC}){$ENDIF}         := GetProcAddress(FLibHandle, 'g_source_destroy');
  {$IFDEF FPC}Pointer({$ENDIF}g_source_unref{$IFDEF FPC}){$ENDIF}           := GetProcAddress(FLibHandle, 'g_source_unref');

  if assigned(g_main_context_default) and
     assigned(g_source_new) and
     assigned(g_source_add_poll) and
     assigned(g_source_set_priority) and
     assigned(g_source_set_can_recurse) and
     assigned(g_source_attach) and
     assigned(g_source_destroy) and
     assigned(g_source_unref) and
     (AssignPipe(FWakeupPipeRead, FWakeupPipeWrite) <> -1) then
    begin
      TempContext            := g_main_context_default();

      FWakeupGPollFD.fd      := FWakeupPipeRead;
      FWakeupGPollFD.events  := G_IO_IN;

      FSourceFuncs.check     := {$IFDEF FPC}@{$ENDIF}WorkSourceCheck;
      FSourceFuncs.dispatch  := {$IFDEF FPC}@{$ENDIF}WorkSourceDispatch;
      FSourceFuncs.prepare   := {$IFDEF FPC}@{$ENDIF}WorkSourcePrepare;

      FWorkSource            := PCustomGSource(g_source_new(@FSourceFuncs, SizeOf(TCustomGSource)));
      FWorkSource^.parent    := self;

      g_source_add_poll(PGSource(FWorkSource), @FWakeupGPollFD);
      g_source_set_priority(PGSource(FWorkSource), G_PRIORITY_DEFAULT_IDLE);
      g_source_set_can_recurse(PGSource(FWorkSource), True);

      if (g_source_attach(PGSource(FWorkSource), TempContext) <= 0) then
        FWorkSource := nil;
    end;
end;

function TCEFLinuxEventPipe.GetInitialized : boolean;
begin
  Result := (FLibHandle       <> 0) and
            (FWakeupPipeRead  <> 0) and
            (FWakeupPipeWrite <> 0) and
            assigned(FWorkSource);
end;

function TCEFLinuxEventPipe.GetHasData : boolean;
begin
  Result := ((FWakeupGPollFD.revents and G_IO_IN) <> 0);
end;

function TCEFLinuxEventPipe.GetHasPendingData : boolean;
begin
  Result := (FPendingRead > 0);
end;

function TCEFLinuxEventPipe.DoOnPrepare : integer;
begin
  Result := 0;

  if assigned(FOnPrepare) then
    FOnPrepare(self, Result);
end;

function TCEFLinuxEventPipe.DoOnCheck : boolean;
begin
  Result := False;

  if assigned(FOnCheck) then
    FOnCheck(self, Result);
end;

procedure TCEFLinuxEventPipe.DoOnDispatch;
begin
  if assigned(FOnDispatch) then
    FOnDispatch(self);
end;

function TCEFLinuxEventPipe.ReadAll(var aValue : integer) : boolean;
var
  TempValues : array of integer;
  TempRead   : Longint;
begin
  Result := False;
  aValue := 0;

  if Initialized and (FPendingRead > 0) then
    try
      SetLength(TempValues, FPendingRead);

      TempRead := FileRead(FWakeupPipeRead, TempValues[0], SizeOf(integer) * FPendingRead);
      TempRead := pred(TempRead div SizeOf(integer));

      if (TempRead >= 0) then
        begin
          aValue       := TempValues[TempRead];
          FPendingRead := 0;
          Result       := True;
        end;
    finally
      Finalize(TempValues);
    end;
end;

function TCEFLinuxEventPipe.Read(var aValue : integer) : boolean;
var
  TempValue : integer;
  TempRead  : Longint;
begin
  Result := False;
  aValue := 0;

  if Initialized and HasPendingData then
    begin
      TempRead := FileRead(FWakeupPipeRead, TempValue, SizeOf(integer));

      if (TempRead >= 0) then
        begin
          dec(FPendingRead);

          aValue := TempValue;
          Result := True;
        end;
    end;
end;

function TCEFLinuxEventPipe.Write(aValue : integer) : boolean;
begin
  Result := False;

  if Initialized and
     (FileWrite(FWakeupPipeWrite, aValue, SizeOf(integer)) > 0) then
    begin
      inc(FPendingRead);
      Result := True;
    end;
end;
{$ENDIF}

end.

