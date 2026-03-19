unit uCEFComponentUpdater;

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
  /// This structure provides access to Chromium's component updater service,
  /// allowing clients to discover registered components and trigger on-demand
  /// updates. The functions of this structure may only be called on the browser
  /// process UI thread. If the CEF context is not initialized or the component
  /// updater service is not available, functions will return safe defaults (0,
  /// nullptr, or NULL).
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_component_updater_capi.h">CEF source file: /include/capi/cef_component_updater_capi.h (cef_component_updater_t)</see></para>
  /// </remarks>
  TCefComponentUpdaterRef = class(TCefBaseRefCountedRef, ICefComponentUpdater)
    protected
      /// <summary>
      /// Returns the number of registered components, or 0 if the service is not
      /// available.
      /// </summary>
      function  GetComponentCount: NativeUInt;
      /// <summary>
      /// Populates |components_| with all registered components. Any existing
      /// contents will be cleared first.
      /// </summary>
      function  GetComponents(var components_: TCefComponentArray): boolean;
      /// <summary>
      /// Returns the component with the specified |component_id|, or nullptr if not
      /// found or the service is not available.
      /// </summary>
      function  GetComponentById(const component_id: ustring; var aResult: ICefComponent): boolean;
      /// <summary>
      /// <para>Triggers an on-demand update for the component with the specified
      /// |component_id|. |priority| specifies whether the update should be
      /// processed in the background or foreground. Use
      /// CEF_COMPONENT_UPDATE_PRIORITY_FOREGROUND for user-initiated updates.</para>
      ///
      /// <para>|callback| will be called asynchronously on the UI thread when the update
      /// operation completes. The callback is always executed, including when the
      /// component is already up-to-date (returns CEF_COMPONENT_UPDATE_ERROR_NONE),
      /// when the requested component doesn't exist, or when the service is
      /// unavailable (returns CEF_COMPONENT_UPDATE_ERROR_SERVICE_ERROR). The
      /// callback may be nullptr if no notification is needed.</para>
      /// </summary>
      procedure Update(const component_id: ustring; priority: TCefComponentUpdatePriority; const callback: ICefComponentUpdateCallback);

    public
      class function UnWrap(data: Pointer): ICefComponentUpdater;
      class function New: ICefComponentUpdater;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFComponent;

class function TCefComponentUpdaterRef.UnWrap(data: Pointer): ICefComponentUpdater;
begin
  if (data <> nil) then
    Result := Create(data) as ICefComponentUpdater
   else
    Result := nil;
end;

class function TCefComponentUpdaterRef.New: ICefComponentUpdater;
begin
  Result := UnWrap(cef_component_updater_get());
end;

function TCefComponentUpdaterRef.GetComponentCount: NativeUInt;
begin
  Result := PCefComponentUpdater(FData)^.get_component_count(PCefComponentUpdater(FData));
end;

function TCefComponentUpdaterRef.GetComponents(var components_: TCefComponentArray): boolean;
var
  i, TempCount  : NativeUInt;
  TempComponent : PCefComponent;
begin
  Result := False;
  PCefComponentUpdater(FData)^.get_components(PCefComponentUpdater(FData), TempCount, TempComponent);

  if (TempCount > 0) then
    begin
      SetLength(components_, TempCount);
      i := 0;

      while (i < TempCount) do
        begin
          components_[i] := TCefComponentRef.Unwrap(TempComponent);
          inc(TempComponent);
          inc(i);
        end;

      Result := True;
    end;
end;

function TCefComponentUpdaterRef.GetComponentById(const component_id: ustring; var aResult: ICefComponent): boolean;
var
  TempComponentId : TCefString;
  TempComponent   : PCefComponent;
begin
  Result          := False;
  aResult         := nil;
  TempComponentId := CefString(component_id);
  TempComponent   := PCefComponentUpdater(FData)^.get_component_by_id(PCefComponentUpdater(FData), @TempComponentId);

  if assigned(TempComponent) then
    begin
      aResult := TCefComponentRef.Unwrap(TempComponent);
      Result  := True;
    end;
end;

procedure TCefComponentUpdaterRef.Update(const component_id: ustring; priority: TCefComponentUpdatePriority; const callback: ICefComponentUpdateCallback);
var
  TempComponentId : TCefString;
begin
  TempComponentId := CefString(component_id);
  PCefComponentUpdater(FData)^.Update(PCefComponentUpdater(FData), @TempComponentId, priority, CefGetData(callback));
end;

end.
