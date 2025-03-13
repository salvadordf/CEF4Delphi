unit uCEFPreferenceObserver;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
    System.Classes,
  {$ELSE}
    Classes,
  {$ENDIF}
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefPreferenceObserverOwn = class(TCefBaseRefCountedOwn, ICefPreferenceObserver)
    protected
      procedure OnPreferenceChanged(const name : ustring); virtual;

      procedure RemoveReferences; virtual;

    public
      constructor Create; virtual;
  end;

  TCustomPreferenceObserver = class(TCefPreferenceObserverOwn)
    protected
      FEvents : Pointer;

      procedure OnPreferenceChanged(const name : ustring); override;

      procedure RemoveReferences; override;

    public
      constructor Create(const events : IChromiumEvents); reintroduce; virtual;
      destructor  Destroy; override;
  end;

  TPreferenceInfo = class
    protected
      FObserver     : ICefPreferenceObserver;
      FRegistration : ICefRegistration;
      FName         : ustring;

    public
      constructor Create(const aName : ustring; const events : IChromiumEvents);
      destructor  Destroy; override;

      property  PrefName     : ustring                read FName;
      property  Observer     : ICefPreferenceObserver read FObserver;
      property  Registration : ICefRegistration       read FRegistration  write FRegistration;
  end;

  TPreferenceInfoList = class(TList)
    protected
      function SearchPreference(const aName : ustring): integer;

    public
      destructor Destroy; override;

      function  HasPreference(const aName : ustring): boolean;
      function  AddPreference(const aName : ustring; const events : IChromiumEvents): integer;
      procedure RemovePreference(const aName : ustring);
  end;

implementation

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBrowser;


// TCefPreferenceObserverOwn

procedure cef_preference_observer_on_preference_changed(self: PCefPreferenceObserver; const name: PCefString); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefPreferenceObserverOwn) then
    TCefPreferenceObserverOwn(TempObject).OnPreferenceChanged(CefString(name));
end;

constructor TCefPreferenceObserverOwn.Create;
begin
  inherited CreateData(SizeOf(TCefPreferenceObserver));

  PCefPreferenceObserver(FData)^.on_preference_changed := {$IFDEF FPC}@{$ENDIF}cef_preference_observer_on_preference_changed;
end;

procedure TCefPreferenceObserverOwn.OnPreferenceChanged(const name : ustring);
begin
  //
end;

procedure TCefPreferenceObserverOwn.RemoveReferences;
begin
  //
end;


// TCustomPreferenceObserver

constructor TCustomPreferenceObserver.Create(const events : IChromiumEvents);
begin
  inherited Create;

  FEvents := Pointer(events);
end;

destructor TCustomPreferenceObserver.Destroy;
begin
  RemoveReferences;

  inherited Destroy;
end;

procedure TCustomPreferenceObserver.RemoveReferences;
begin
  FEvents := nil;
end;

procedure TCustomPreferenceObserver.OnPreferenceChanged(const name : ustring);
begin
  if (FEvents <> nil) then
    IChromiumEvents(FEvents).doOnPreferenceChanged(name);
end;


// TPreferenceInfo

constructor TPreferenceInfo.Create(const aName : ustring; const events : IChromiumEvents);
begin
  inherited Create;

  FName         := aName;
  FObserver     := TCustomPreferenceObserver.Create(events);
  FRegistration := nil;
end;

destructor TPreferenceInfo.Destroy;
begin
  FRegistration := nil;
  FObserver     := nil;

  inherited Destroy;
end;


// TPreferenceInfoList



destructor TPreferenceInfoList.Destroy;
var
  i : integer;
begin
  i := pred(Count);
  while (i >= 0) do
    begin
      TPreferenceInfo(Items[i]).Free;
      dec(i);
    end;

  inherited Destroy;
end;

function TPreferenceInfoList.SearchPreference(const aName : ustring): integer;
var
  i : integer;
begin
  Result := -1;

  i := pred(Count);
  while (i >= 0) do
    if (TPreferenceInfo(Items[i]).PrefName = aName) then
      begin
        Result := i;
        exit;
      end
     else
      dec(i);
end;

function TPreferenceInfoList.HasPreference(const aName : ustring): boolean;
begin
  Result := SearchPreference(aName) >= 0;
end;

function TPreferenceInfoList.AddPreference(const aName : ustring; const events : IChromiumEvents): integer;
begin
  Result := Add(TPreferenceInfo.Create(aName, events));
end;

procedure TPreferenceInfoList.RemovePreference(const aName : ustring);
var
  i : integer;
begin
  i := SearchPreference(aName);
  if (i >= 0) then
    begin
      TPreferenceInfo(Items[i]).Free;
      Delete(i);
    end;
end;

end.
