unit Interfaces;

{$mode objfpc}{$H+}

interface

{$IFDEF UNIX}{$IFNDEF DisableCWString}uses cwstring;{$ENDIF}{$ENDIF}

procedure CustomWidgetSetInitialization;
procedure CustomWidgetSetFinalization;

implementation

uses
 {$IFNDEF EnableLibOverlay}
 gtk2DisableLibOverlay,
 {$ENDIF}
 Gtk2Int, Forms;

procedure CustomWidgetSetInitialization;
begin
  CreateWidgetset(TGtk2WidgetSet);
end;

procedure CustomWidgetSetFinalization;
begin
  FreeWidgetSet;
end;

end.
