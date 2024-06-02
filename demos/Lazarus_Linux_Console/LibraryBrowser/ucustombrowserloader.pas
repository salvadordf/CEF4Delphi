unit ucustombrowserloader;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

const
  LIBNAME = 'libcustombrowser.so';

type
  TInitializeCEF4DelphiFunc = procedure; cdecl;
  TFinalizeCEF4DelphiFunc   = procedure; cdecl;
  TTakeSnapshotFunc         = procedure; cdecl;

  TCustomBrowserLoader = class
    private
      FInitializeCEF4Delphi : TInitializeCEF4DelphiFunc;
      FFinalizeCEF4Delphi   : TFinalizeCEF4DelphiFunc;
      FTakeSnapshot         : TTakeSnapshotFunc;
      FLibHandle            : TLibHandle;
      FLibLoaded            : boolean;

      function  GetLibPath : string;
      function  LoadCEFLibrary: boolean;
      procedure UnloadCEFLibrary;
      procedure InitializeCEF4Delphi;
      procedure FinalizeCEF4Delphi;

    public
      constructor Create;
      procedure AfterConstruction; override;
      procedure BeforeDestruction; override;
      procedure TakeSnapshot;
  end;

var
  GlobalCustomBrowseLoader : TCustomBrowserLoader = nil;

implementation

constructor TCustomBrowserLoader.Create;
begin
  inherited Create;

  FInitializeCEF4Delphi := nil;
  FFinalizeCEF4Delphi   := nil;
  FTakeSnapshot         := nil;
  FLibHandle            := 0;
  FLibLoaded            := False;
end;

procedure TCustomBrowserLoader.AfterConstruction;
begin
  inherited AfterConstruction;

  if LoadCEFLibrary then
    InitializeCEF4Delphi;
end;

procedure TCustomBrowserLoader.BeforeDestruction;
begin
  FinalizeCEF4Delphi;
  UnloadCEFLibrary;

  inherited BeforeDestruction;
end;

function TCustomBrowserLoader.LoadCEFLibrary: boolean;
begin
  Result     := False;
  FLibHandle := LoadLibrary(GetLibPath());

  if (FLibHandle <> 0) then
    begin
      Pointer(FInitializeCEF4Delphi) := GetProcAddress(FLibHandle, 'InitializeCEF4Delphi');
      Pointer(FFinalizeCEF4Delphi)   := GetProcAddress(FLibHandle, 'FinalizeCEF4Delphi');
      Pointer(FTakeSnapshot)         := GetProcAddress(FLibHandle, 'TakeSnapshot');

      FLibLoaded := assigned(FInitializeCEF4Delphi) and
                    assigned(FFinalizeCEF4Delphi) and
                    assigned(FTakeSnapshot);

      Result := FLibLoaded;
    end;
end;

function TCustomBrowserLoader.GetLibPath : string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0))) + LIBNAME;
end;

procedure TCustomBrowserLoader.UnloadCEFLibrary;
begin
  if FLibLoaded then
    FreeLibrary(FLibHandle);
end;

procedure TCustomBrowserLoader.InitializeCEF4Delphi;
begin
  if FLibLoaded then
    FInitializeCEF4Delphi();
end;

procedure TCustomBrowserLoader.FinalizeCEF4Delphi;
begin
  if FLibLoaded then
    FFinalizeCEF4Delphi();
end;

procedure TCustomBrowserLoader.TakeSnapshot;
begin
  if FLibLoaded then
    FTakeSnapshot();
end;

end.

