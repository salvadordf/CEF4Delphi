unit uCEFSettingObserver;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefSettingObserverOwn = class(TCefBaseRefCountedOwn, ICefSettingObserver)
    protected
      procedure OnSettingChanged(const requesting_url, top_level_url : ustring; content_type: TCefContentSettingTypes); virtual;

      procedure RemoveReferences; virtual;

    public
      constructor Create; virtual;
  end;

  TCustomSettingObserver = class(TCefSettingObserverOwn)
    protected
      FEvents : Pointer;

      procedure OnSettingChanged(const requesting_url, top_level_url : ustring; content_type: TCefContentSettingTypes); override;

      procedure RemoveReferences; override;

    public
      constructor Create(const events : IChromiumEvents); reintroduce; virtual;
      destructor  Destroy; override;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBrowser;


// TCefSettingObserverOwn

procedure cef_setting_observer_on_setting_changed(self: PCefSettingObserver; const requesting_url, top_level_url: PCefString; content_type: TCefContentSettingTypes); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefSettingObserverOwn) then
    TCefSettingObserverOwn(TempObject).OnSettingChanged(CefString(requesting_url),
                                                        CefString(top_level_url),
                                                        content_type);
end;

constructor TCefSettingObserverOwn.Create;
begin
  inherited CreateData(SizeOf(TCefSettingObserver));

  PCefSettingObserver(FData)^.on_setting_changed := {$IFDEF FPC}@{$ENDIF}cef_setting_observer_on_setting_changed;
end;

procedure TCefSettingObserverOwn.OnSettingChanged(const requesting_url, top_level_url : ustring; content_type: TCefContentSettingTypes);
begin
  //
end;

procedure TCefSettingObserverOwn.RemoveReferences;
begin
  //
end;


// TCustomSettingObserver

constructor TCustomSettingObserver.Create(const events : IChromiumEvents);
begin
  inherited Create;

  FEvents := Pointer(events);
end;

destructor TCustomSettingObserver.Destroy;
begin
  RemoveReferences;

  inherited Destroy;
end;

procedure TCustomSettingObserver.RemoveReferences;
begin
  FEvents := nil;
end;

procedure TCustomSettingObserver.OnSettingChanged(const requesting_url, top_level_url : ustring; content_type: TCefContentSettingTypes);
begin
  if (FEvents <> nil) then
    IChromiumEvents(FEvents).doOnSettingChanged(requesting_url, top_level_url, content_type);
end;

end.
