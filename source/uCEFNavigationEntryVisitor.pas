unit uCEFNavigationEntryVisitor;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces;

type
  TCefNavigationEntryVisitorOwn = class(TCefBaseRefCountedOwn, ICefNavigationEntryVisitor)
    protected
      function Visit(const entry: ICefNavigationEntry; current: Boolean; index, total: Integer): Boolean; virtual;

    public
      constructor Create;
  end;

  TCefFastNavigationEntryVisitor = class(TCefNavigationEntryVisitorOwn)
    protected
      FVisitor: TCefNavigationEntryVisitorProc;

      function Visit(const entry: ICefNavigationEntry; current: Boolean; index, total: Integer): Boolean; override;

    public
      constructor Create(const proc: TCefNavigationEntryVisitorProc); reintroduce;
  end;

  TCustomCefNavigationEntryVisitor = class(TCefNavigationEntryVisitorOwn)
    protected
      FEvents : Pointer;

      function Visit(const entry: ICefNavigationEntry; current: Boolean; index, total: Integer): Boolean; override;

    public
      constructor Create(const aEvents : IChromiumEvents); reintroduce;
      destructor  Destroy; override;
  end;

implementation

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  uCEFTypes, uCEFMiscFunctions, uCEFNavigationEntry;

function cef_navigation_entry_visitor_visit(self    : PCefNavigationEntryVisitor;
                                            entry   : PCefNavigationEntry;
                                            current : Integer;
                                            index   : Integer;
                                            total   : Integer): Integer; stdcall;
var
  TempObject : TObject;
begin
  Result     := Ord(False);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefNavigationEntryVisitorOwn) then
    Result := Ord(TCefNavigationEntryVisitorOwn(TempObject).Visit(TCefNavigationEntryRef.UnWrap(entry),
                                                                  current <> 0,
                                                                  index,
                                                                  total));
end;

// TCefNavigationEntryVisitorOwn

constructor TCefNavigationEntryVisitorOwn.Create;
begin
  inherited CreateData(SizeOf(TCefNavigationEntryVisitor));

  PCefNavigationEntryVisitor(FData)^.visit := {$IFDEF FPC}@{$ENDIF}cef_navigation_entry_visitor_visit;
end;

function TCefNavigationEntryVisitorOwn.Visit(const entry   : ICefNavigationEntry;
                                                   current : Boolean;
                                                   index   : Integer;
                                                   total   : Integer): Boolean;
begin
  Result:= False;
end;

// TCefFastNavigationEntryVisitor

constructor TCefFastNavigationEntryVisitor.Create(const proc: TCefNavigationEntryVisitorProc);
begin
  FVisitor := proc;

  inherited Create;
end;

function TCefFastNavigationEntryVisitor.Visit(const entry   : ICefNavigationEntry;
                                                    current : Boolean;
                                                    index   : Integer;
                                                    total   : Integer): Boolean;
begin
  Result := FVisitor(entry, current, index, total);
end;

// TCustomCefNavigationEntryVisitor

constructor TCustomCefNavigationEntryVisitor.Create(const aEvents : IChromiumEvents);
begin
  inherited Create;

  FEvents := Pointer(aEvents);
end;

destructor TCustomCefNavigationEntryVisitor.Destroy;
begin
  FEvents := nil;

  inherited Destroy;
end;

function TCustomCefNavigationEntryVisitor.Visit(const entry   : ICefNavigationEntry;
                                                      current : Boolean;
                                                      index   : Integer;
                                                      total   : Integer): Boolean;
begin
  Result := False;

  try
    if (FEvents <> nil) then
      Result := IChromiumEvents(FEvents).doNavigationVisitorResultAvailable(entry, current, index, total);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomCefNavigationEntryVisitor.Visit', e) then raise;
  end;
end;

end.
