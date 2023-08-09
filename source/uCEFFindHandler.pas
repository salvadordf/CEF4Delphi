unit uCEFFindHandler;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  uCEFBaseRefCounted, uCEFTypes, uCEFInterfaces;

type
  TCefFindHandlerOwn = class(TCefBaseRefCountedOwn, ICefFindHandler)
    protected
      procedure OnFindResult(const browser: ICefBrowser; identifier, count: Integer; const selectionRect: PCefRect; activeMatchOrdinal: Integer; finalUpdate: Boolean); virtual; abstract;

      procedure RemoveReferences; virtual;

    public
      constructor Create; virtual;
  end;

  TCustomFindHandler = class(TCefFindHandlerOwn)
    protected
      FEvents : Pointer;

      procedure OnFindResult(const browser: ICefBrowser; identifier, count: Integer; const selectionRect: PCefRect; activeMatchOrdinal: Integer; finalUpdate: Boolean); override;

      procedure RemoveReferences; override;

    public
      constructor Create(const events : IChromiumEvents); reintroduce; virtual;
      destructor  Destroy; override;
  end;

implementation

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBrowser;

procedure cef_find_handler_on_find_result(      self                 : PCefFindHandler;
                                                browser              : PCefBrowser;
                                                identifier           : Integer;
                                                count                : Integer;
                                          const selection_rect       : PCefRect;
                                                active_match_ordinal : integer;
                                                final_update         : Integer); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefFindHandlerOwn) then
    TCefFindHandlerOwn(TempObject).OnFindResult(TCefBrowserRef.UnWrap(browser),
                                                identifier,
                                                count,
                                                selection_rect,
                                                active_match_ordinal,
                                                final_update <> 0);
end;

constructor TCefFindHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefFindHandler));

  PCefFindHandler(FData)^.on_find_result := {$IFDEF FPC}@{$ENDIF}cef_find_handler_on_find_result;
end;

procedure TCefFindHandlerOwn.RemoveReferences;
begin
  //
end;

// TCustomFindHandler

constructor TCustomFindHandler.Create(const events : IChromiumEvents);
begin
  inherited Create;

  FEvents := Pointer(events);
end;

destructor TCustomFindHandler.Destroy;
begin
  RemoveReferences;

  inherited Destroy;
end;

procedure TCustomFindHandler.RemoveReferences;
begin
  FEvents := nil;
end;

procedure TCustomFindHandler.OnFindResult(const browser            : ICefBrowser;
                                                identifier         : Integer;
                                                count              : Integer;
                                          const selectionRect      : PCefRect;
                                                activeMatchOrdinal : Integer;
                                                finalUpdate        : Boolean);
begin
  if (FEvents <> nil) then
    IChromiumEvents(FEvents).doOnFindResult(browser, identifier, count, selectionRect, activeMatchOrdinal, finalUpdate);
end;

end.
