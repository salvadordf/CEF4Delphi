unit uCEFComponentIdList;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes, System.SyncObjs;
  {$ELSE}
  Classes, SyncObjs;
  {$ENDIF}

type
  /// <summary>
  ///  Class used to keep a list of valid custom component IDs for any component that handles a CEF Task event.
  /// </summary>
  TCEFComponentIdList = class
    protected
      { Using a TList for backwards compatibility reasons. }
      FList    : TList;
      FIdGen   : integer;
      FSyncObj : TCriticalSection;

      function  GetInitialized : boolean;

      function  Lock: boolean;
      procedure Unlock;

    public
      constructor Create;
      destructor  Destroy; override;
      procedure   AfterConstruction; override;
      /// <summary>
      /// Returns true if a custom component ID is valid before executing a CEF task.
      /// </summary>
      function    ValidID(aID : integer) : boolean;
      /// <summary>
      /// Returns the next component ID and adds this value to the valid ID list.
      /// </summary>
      function    NextID : integer;
      /// <summary>
      /// Removes a component ID from the valid ID list when a component is destroyed.
      /// </summary>
      procedure   RemoveID(aID : integer);
      /// <summary>
      /// Returns true when this class is fully initialized and ready to be used.
      /// </summary>
      property    Initialized : boolean     read GetInitialized;
  end;

implementation

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils, System.Types;
  {$ELSE}
  SysUtils, Types;
  {$ENDIF}

constructor TCEFComponentIdList.Create;
begin
  inherited Create;

  FList    := nil;
  FIdGen   := 0;
  FSyncObj := nil;
end;

destructor  TCEFComponentIdList.Destroy;
begin
  if assigned(FList)    then FreeAndNil(FList);
  if assigned(FSyncObj) then FreeAndNil(FSyncObj);

  inherited Destroy;
end;

procedure TCEFComponentIdList.AfterConstruction;
begin
  inherited AfterConstruction;

  FList    := TList.Create;
  FSyncObj := TCriticalSection.Create;
end;

function TCEFComponentIdList.GetInitialized : boolean;
begin
  Result := assigned(FSyncObj) and assigned(FList);
end;

function TCEFComponentIdList.Lock: boolean;
begin
  Result := False;

  if Initialized then
    begin
      FSyncObj.Acquire;
      Result := True;
    end;
end;

procedure TCEFComponentIdList.Unlock;
begin
  FSyncObj.Release;
end;

function TCEFComponentIdList.ValidID(aID : integer) : boolean;
begin
  if (aID <= 0) then
    Result := True
   else
    begin
      Result := False;

      if Lock then
        try
          Result := FList.IndexOf(Pointer(aID)) >= 0;
        finally
          Unlock;
        end;
    end;
end;

function TCEFComponentIdList.NextID : integer;
begin
  Result := 0;

  if Lock then
    try
      repeat
        if (FIdGen < pred(high(integer))) then
          inc(FIdGen)
         else
          FIdGen := 1;
      until (FList.IndexOf(Pointer(FIdGen)) < 0);

      FList.Add(Pointer(FIdGen));
      Result := FIdGen;
    finally
      Unlock;
    end;
end;

procedure TCEFComponentIdList.RemoveID(aID : integer);
begin
  if (aID > 0) and Lock then
    try
      FList.Remove(Pointer(aID));
    finally
      Unlock;
    end;
end;

end.
