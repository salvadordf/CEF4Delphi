{
 /***************************************************************************
                         Interfaces.pp  -  determines what interface to use
                             -------------------

                   Initial Revision  : Thu July 1st CST 1999


 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

unit interfaces;

{$mode objfpc}
{$H+}

interface

uses
  {$IFDEF UNIX}{$IFNDEF DisableCWString}cwstring,{$ENDIF}{$ENDIF}
  InterfaceBase;

procedure CustomWidgetSetInitialization;
procedure CustomWidgetSetFinalization;

implementation

uses
  gtk3int, Forms, xlib,
  uCEFLinuxFunctions;

function CustomX11ErrorHandler(Display:PDisplay; ErrorEv:PXErrorEvent):longint;cdecl;
begin
  {$IFDEF DEBUG}
  XError := ErrorEv^.error_code;
  WriteLn('Error: ' + IntToStr(XError));
  {$ENDIF}
  Result := 0;
end;

function CustomXIOErrorHandler(Display:PDisplay):longint;cdecl;
begin
  Result := 0;
end;

procedure CustomWidgetSetInitialization;
begin
  //gdk_set_allowed_backends('X11');
  CreateWidgetset(TGtk3WidgetSet);
  // Install xlib error handlers so that the application won't be terminated
  // on non-fatal errors. Must be done after initializing GTK.
  XSetErrorHandler(@CustomX11ErrorHandler);
  XSetIOErrorHandler(@CustomXIOErrorHandler);
end;

procedure CustomWidgetSetFinalization;
begin
  FreeWidgetSet;
end;

end.
