unit uCEFv8BackingStore;

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
  /// <para>Structure representing a V8 ArrayBuffer backing store. The backing store
  /// holds the memory that backs an ArrayBuffer. It must be created on a thread
  /// with a valid V8 isolate (renderer main thread or WebWorker thread). Once
  /// created, the data() pointer can be safely read/written from any thread. This
  /// allows expensive operations like memcpy to be performed on a background
  /// thread before creating the ArrayBuffer on the V8 thread.</para>
  ///
  /// <para>The backing store is consumed when passed to
  /// TCefv8ValueRef.NewArrayBufferFromBackingStore(), after
  /// which IsValid() returns false (0).</para>
  /// </summary>
  /// <remarks>
  /// <para><see href="https://bitbucket.org/chromiumembedded/cef/src/master/include/capi/cef_v8_capi.h">CEF source file: /include/capi/cef_v8_capi.h (cef_v8_backing_store_t)</see></para>
  /// </remarks>
  TCefv8BackingStoreRef = class(TCefBaseRefCountedRef, ICefv8BackingStore)
    protected
      /// <summary>
      /// Returns a pointer to the allocated memory, or nullptr if the backing store
      /// has been consumed or is otherwise invalid. The pointer is safe to
      /// read/write from any thread. The caller must ensure all writes are complete
      /// before passing this object to
      /// TCefv8ValueRef.NewArrayBufferFromBackingStore(). Pointers obtained
      /// from this function should not be retained after calling
      /// TCefv8ValueRef.NewArrayBufferFromBackingStore(), as the memory will
      /// then be owned by the ArrayBuffer and subject to V8 garbage collection.
      /// </summary>
      function data: Pointer;
      /// <summary>
      /// Returns the size of the allocated memory in bytes, or 0 if the backing
      /// store has been consumed.
      /// </summary>
      function ByteLength: NativeUInt;
      /// <summary>
      /// Returns true (1) if this backing store has not yet been consumed by
      /// TCefv8ValueRef.NewArrayBufferFromBackingStore().
      /// </summary>
      function IsValid: boolean;

    public
      class function UnWrap(data_: Pointer): ICefv8BackingStore;
      class function New(byte_length: NativeUInt): ICefv8BackingStore;
  end;

implementation

uses
  uCEFLibFunctions;

class function TCefv8BackingStoreRef.UnWrap(data_: Pointer): ICefv8BackingStore;
begin
  if (data_ <> nil) then
    Result := Create(data_) as ICefv8BackingStore
   else
    Result := nil;
end;

class function TCefv8BackingStoreRef.New(byte_length: NativeUInt): ICefv8BackingStore;
begin
  Result := UnWrap(cef_v8_backing_store_create(byte_length));
end;

function TCefv8BackingStoreRef.data: Pointer;
begin
  Result := PCefv8BackingStore(FData)^.data(PCefv8BackingStore(FData));
end;

function TCefv8BackingStoreRef.ByteLength: NativeUInt;
begin
  Result := PCefv8BackingStore(FData)^.byte_length(PCefv8BackingStore(FData));
end;

function TCefv8BackingStoreRef.IsValid: Boolean;
begin
  Result := PCefv8BackingStore(FData)^.is_valid(PCefv8BackingStore(FData)) <> 0;
end;

end.
