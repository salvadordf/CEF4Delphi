unit uCEFAccessibilityHandler;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes, uCEFChromiumEvents;

type
  TOnAccessibilityEvent = procedure(Sender: TObject; const value: ICefValue) of object;

  TCEFAccessibilityHandlerOwn = class(TCefBaseRefCountedOwn, ICefAccessibilityHandler)
    protected
      procedure OnAccessibilityTreeChange(const value: ICefValue); virtual;
      procedure OnAccessibilityLocationChange(const value: ICefValue); virtual;

    public
      constructor Create; virtual;
  end;

  TCustomAccessibilityHandler = class(TCEFAccessibilityHandlerOwn)
    protected
      FOnTreeChange     : TOnAccessibilityEvent;
      FOnLocationChange : TOnAccessibilityEvent;

      procedure OnAccessibilityTreeChange(const value: ICefValue); override;
      procedure OnAccessibilityLocationChange(const value: ICefValue); override;

    public
      constructor Create; override;

      property OnTreeChange       : TOnAccessibilityEvent   read FOnTreeChange       write FOnTreeChange;
      property OnLocationChange   : TOnAccessibilityEvent   read FOnLocationChange   write FOnLocationChange;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFValue;

procedure cef_accessibility_handler_on_accessibility_tree_change(self: PCefAccessibilityHandler; value: PCefValue); stdcall;
var
  TempObject  : TObject;
begin
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCEFAccessibilityHandlerOwn) then
    TCEFAccessibilityHandlerOwn(TempObject).OnAccessibilityTreeChange(TCefValueRef.UnWrap(value));
end;

procedure cef_accessibility_handler_on_accessibility_location_change(self: PCefAccessibilityHandler; value: PCefValue); stdcall;
var
  TempObject  : TObject;
begin
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCEFAccessibilityHandlerOwn) then
    TCEFAccessibilityHandlerOwn(TempObject).OnAccessibilityLocationChange(TCefValueRef.UnWrap(value));
end;

constructor TCEFAccessibilityHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCEFAccessibilityHandler));

  with PCEFAccessibilityHandler(FData)^ do
    begin
      on_accessibility_tree_change     := {$IFDEF FPC}@{$ENDIF}cef_accessibility_handler_on_accessibility_tree_change;
      on_accessibility_location_change := {$IFDEF FPC}@{$ENDIF}cef_accessibility_handler_on_accessibility_location_change;
    end;
end;

procedure TCEFAccessibilityHandlerOwn.OnAccessibilityTreeChange(const value: ICefValue);
begin
  //
end;

procedure TCEFAccessibilityHandlerOwn.OnAccessibilityLocationChange(const value: ICefValue);
begin
  //
end;


// *****************************************
// ****** TCustomAccessibilityHandler ******
// *****************************************

constructor TCustomAccessibilityHandler.Create;
begin
  inherited Create;

  FOnTreeChange     := nil;
  FOnLocationChange := nil;
end;

procedure TCustomAccessibilityHandler.OnAccessibilityTreeChange(const value: ICefValue);
begin
  if assigned(FOnTreeChange) then FOnTreeChange(self, value);
end;

procedure TCustomAccessibilityHandler.OnAccessibilityLocationChange(const value: ICefValue);
begin
  if assigned(FOnLocationChange) then FOnLocationChange(self, value);
end;

end.
