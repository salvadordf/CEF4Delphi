unit uCEFLinkedWindowParent;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
    {$IFDEF MSWINDOWS}WinApi.Windows,{$ENDIF} System.Classes, Vcl.Controls,
  {$ELSE}
    {$IFDEF MSWINDOWS}Windows,{$ENDIF} Classes, Forms, Controls, Graphics,
    {$IFDEF FPC}
      LCLProc, LCLType, LCLIntf, LResources, LMessages, InterfaceBase,
      {$IFDEF LINUX}xlib, x,{$ENDIF}
    {$ELSE}
      Messages,
    {$ENDIF}
  {$ENDIF}
  uCEFWinControl, uCEFTypes, uCEFInterfaces, uCEFChromium,
  uCEFConstants, uCEFLinkedWinControlBase;

type
  {$IFNDEF FPC}{$IFDEF DELPHI16_UP}[ComponentPlatformsAttribute(pfidWindows)]{$ENDIF}{$ENDIF}
  /// <summary>
  /// This component can be used by VCL and LCL applications. It has the
  /// same purpose as TCEFWindowParent but it has a Chromium property to
  /// link it directly to a TChromium component.
  /// TCEFLinkedWindowParent resizes the child controls created by CEF
  /// for browsers in normal mode and sets the browser focus using the
  /// linked TChromium component. TCEFWindowParent and TCEFLinkedWindowParent
  /// work fine in Windows and you can used any of them but you can't use
  /// TCEFWindowParent in Linux or MacOS.
  /// </summary>
  TCEFLinkedWindowParent = class(TCEFLinkedWinControlBase)
    protected
      FChromium : TChromium;

      function  GetChromium: TChromium; override;
      procedure SetChromium(aValue : TChromium);

      procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    public
      constructor Create(AOwner : TComponent); override;

    published
      /// <summary>
      /// TChromium instance used by this component.
      /// </summary>
      property  Chromium   : TChromium    read FChromium   write SetChromium;
  end;


{$IFDEF FPC}
procedure Register;
{$ENDIF}

implementation

uses
  uCEFMiscFunctions, uCEFClient, uCEFLibFunctions, uCEFApplication;

constructor TCEFLinkedWindowParent.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  FChromium := nil;
end;

procedure TCEFLinkedWindowParent.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (AComponent = FChromium) then FChromium := nil;
end;

function TCEFLinkedWindowParent.GetChromium: TChromium;
begin
  Result := FChromium;
end;

procedure TCEFLinkedWindowParent.SetChromium(aValue : TChromium);
begin
  FChromium := aValue;
  if (aValue <> nil) then aValue.FreeNotification(Self);
end;

{$IFDEF FPC}
procedure Register;
begin
  {$I res/tceflinkedwindowparent.lrs}
  RegisterComponents('Chromium', [TCEFLinkedWindowParent]);
end;
{$ENDIF}

end.
