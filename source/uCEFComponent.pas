unit uCEFComponent;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  /// <summary>
  /// Structure representing a snapshot of a component's state at the time of
  /// retrieval. To get updated information, retrieve a new cef_component_t object
  /// via ICefComponentUpdater.GetComponentById or GetComponents. The
  /// functions of this structure may be called on any thread.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_component_updater_capi.h">CEF source file: /include/capi/cef_component_updater_capi.h (cef_component_t)</see></para>
  /// </remarks>
  TCefComponentRef = class(TCefBaseRefCountedRef, ICefComponent)
    protected
      /// <summary>
      /// Returns the unique identifier for this component.
      /// </summary>
      function GetId: ustring;
      /// <summary>
      /// Returns the human-readable name of this component. Returns an NULL string
      /// if the component is not installed.
      /// </summary>
      function GetName: ustring;
      /// <summary>
      /// Returns the version of this component as a string (e.g., "1.2.3.4").
      /// Returns an NULL string if the component is not installed.
      /// </summary>
      function GetVersion: ustring;
      /// <summary>
      /// Returns the state of this component at the time this object was created. A
      /// component is considered installed when its state is one of:
      /// CEF_COMPONENT_STATE_UPDATED, CEF_COMPONENT_STATE_UP_TO_DATE, or
      /// CEF_COMPONENT_STATE_RUN.
      /// </summary>
      function GetState: TCefComponentState;

    public
      class function UnWrap(data: Pointer): ICefComponent;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;

class function TCefComponentRef.UnWrap(data: Pointer): ICefComponent;
begin
  if (data <> nil) then
    Result := Create(data) as ICefComponent
   else
    Result := nil;
end;

function TCefComponentRef.GetId: ustring;
begin
  Result := CefStringFreeAndGet(PCefComponent(FData)^.get_id(PCefComponent(FData)));
end;

function TCefComponentRef.GetName: ustring;
begin
  Result := CefStringFreeAndGet(PCefComponent(FData)^.get_name(PCefComponent(FData)));
end;

function TCefComponentRef.GetVersion: ustring;
begin
  Result := CefStringFreeAndGet(PCefComponent(FData)^.get_version(PCefComponent(FData)));
end;

function TCefComponentRef.GetState: TCefComponentState;
begin
  Result := PCefComponent(FData)^.get_state(PCefComponent(FData));
end;

end.
