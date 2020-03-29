// ************************************************************************
// ***************************** CEF4Delphi *******************************
// ************************************************************************
//
// CEF4Delphi is based on DCEF3 which uses CEF to embed a chromium-based
// browser in Delphi applications.
//
// The original license of DCEF3 still applies to CEF4Delphi.
//
// For more information about CEF4Delphi visit :
//         https://www.briskbard.com/index.php?lang=en&pageid=cef
//
//        Copyright © 2020 Salvador Diaz Fau. All rights reserved.
//
// ************************************************************************
// ************ vvvv Original license and comments below vvvv *************
// ************************************************************************
(*
 *                       Delphi Chromium Embedded 3
 *
 * Usage allowed under the restrictions of the Lesser GNU General Public License
 * or alternatively the restrictions of the Mozilla Public License 1.1
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * Unit owner : Henri Gourvest <hgourvest@gmail.com>
 * Web site   : http://www.progdigy.com
 * Repository : http://code.google.com/p/delphichromiumembedded/
 * Group      : http://groups.google.com/group/delphichromiumembedded
 *
 * Embarcadero Technologies, Inc is not permitted to use or redistribute
 * this source code without explicit permission.
 *
 *)

unit uCEFMediaObserverComponent;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
    {$IFDEF MSWINDOWS}WinApi.Windows, WinApi.Messages, WinApi.ActiveX,{$ENDIF}
    System.Classes, System.Math,
  {$ELSE}
    {$IFDEF MSWINDOWS}Windows, ActiveX,{$ENDIF} Classes, Math,
    {$IFDEF FPC}
    LCLProc, LCLType, LCLIntf, LResources, LMessages, InterfaceBase,
    {$ELSE}
    Messages,
    {$ENDIF}
  {$ENDIF}
  uCEFTypes, uCEFInterfaces, uCEFMediaObserver, uCEFMediaObserverEvents;

type
  {$IFNDEF FPC}{$IFDEF DELPHI16_UP}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}{$ENDIF}
  TCEFMediaObserverComponent = class(TComponent, ICefMediaObserverEvents)
    protected
      FMediaObserver          : ICefMediaObserver;
      FOnSinks                : TOnSinksEvent;
      FOnRoutes               : TOnRoutesEvent;
      FOnRouteStateChanged    : TOnRouteStateChangedEvent;
      FOnRouteMessageReceived : TOnRouteMessageReceivedEvent;

      // ICefMediaObserverEvents
      procedure doOnSinks(const sinks: TCefMediaSinkArray); virtual;
      procedure doOnRoutes(const routes: TCefMediaRouteArray); virtual;
      procedure doOnRouteStateChanged(const route: ICefMediaRoute; state: TCefMediaRouteConnectionState); virtual;
      procedure doOnRouteMessageReceived(const route: ICefMediaRoute; const message_: Pointer; message_size: NativeUInt); virtual;

    public
      constructor Create(AOwner: TComponent); override;
      destructor  Destroy; override;
      procedure   AfterConstruction; override;

      property MediaObserver : ICefMediaObserver  read FMediaObserver;

    published
      property OnSinks                : TOnSinksEvent                 read FOnSinks                  write FOnSinks;
      property OnRoutes               : TOnRoutesEvent                read FOnRoutes                 write FOnRoutes;
      property OnRouteStateChanged    : TOnRouteStateChangedEvent     read FOnRouteStateChanged      write FOnRouteStateChanged;
      property OnRouteMessageReceived : TOnRouteMessageReceivedEvent  read FOnRouteMessageReceived   write FOnRouteMessageReceived;
  end;

{$IFDEF FPC}
procedure Register;
{$ENDIF}

implementation

// *********************************************************
// ********************** ATTENTION ! **********************
// *********************************************************
// **                                                     **
// **  MANY OF THE EVENTS IN CEF4DELPHI COMPONENTS LIKE   **
// **  TCHROMIUM, TFMXCHROMIUM OR TCEFAPPLICATION ARE     **
// **  EXECUTED IN A CEF THREAD BY DEFAULT.               **
// **                                                     **
// **  WINDOWS CONTROLS MUST BE CREATED AND DESTROYED IN  **
// **  THE SAME THREAD TO AVOID ERRORS.                   **
// **  SOME OF THEM RECREATE THE HANDLERS IF THEY ARE     **
// **  MODIFIED AND CAN CAUSE THE SAME ERRORS.            **
// **                                                     **
// **  DON'T CREATE, MODIFY OR DESTROY WINDOWS CONTROLS   **
// **  INSIDE THE CEF4DELPHI EVENTS AND USE               **
// **  SYNCHRONIZATION OBJECTS TO PROTECT VARIABLES AND   **
// **  FIELDS IF THEY ARE ALSO USED IN THE MAIN THREAD.   **
// **                                                     **
// **  READ THIS FOR MORE INFORMATION :                   **
// **  https://www.briskbard.com/index.php?pageid=cef     **
// **                                                     **
// **  USE OUR FORUMS FOR MORE QUESTIONS :                **
// **  https://www.briskbard.com/forum/                   **
// **                                                     **
// *********************************************************
// *********************************************************

constructor TCEFMediaObserverComponent.Create(AOwner: TComponent);
begin
  inherited Create(aOwner);

  FMediaObserver          := nil;
  FOnSinks                := nil;
  FOnRoutes               := nil;
  FOnRouteStateChanged    := nil;
  FOnRouteMessageReceived := nil;
end;

destructor TCEFMediaObserverComponent.Destroy;
begin
  FMediaObserver := nil;

  inherited Destroy;
end;

procedure TCEFMediaObserverComponent.AfterConstruction;
begin
  inherited AfterConstruction;

  FMediaObserver := TCustomMediaObserver.Create(self);
end;

procedure TCEFMediaObserverComponent.doOnSinks(const sinks: TCefMediaSinkArray);
begin
  if assigned(FOnSinks) then
    FOnSinks(self, sinks);
end;

procedure TCEFMediaObserverComponent.doOnRoutes(const routes: TCefMediaRouteArray);
begin
  if assigned(FOnRoutes) then
    FOnRoutes(self, routes);
end;

procedure TCEFMediaObserverComponent.doOnRouteStateChanged(const route : ICefMediaRoute;
                                                                 state : TCefMediaRouteConnectionState);
begin
  if assigned(FOnRouteStateChanged) then
    FOnRouteStateChanged(self, route, state);
end;

procedure TCEFMediaObserverComponent.doOnRouteMessageReceived(const route        : ICefMediaRoute;
                                                              const message_     : Pointer;
                                                                    message_size : NativeUInt);
begin
  if assigned(FOnRouteMessageReceived) then
    FOnRouteMessageReceived(self, route, message_, message_size);
end;

{$IFDEF FPC}
procedure Register;
begin
  {$I res/tcefmediaobservercomponent.lrs}
  RegisterComponents('Chromium', [TCEFMediaObserverComponent]);
end;
{$ENDIF}

end.
