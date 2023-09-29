unit uCEFLabelButtonComponent;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
    {$IFDEF MSWINDOWS}WinApi.Windows,{$ENDIF} System.Classes,
  {$ELSE}
    {$IFDEF MSWINDOWS}Windows,{$ENDIF} Classes,
    {$IFDEF FPC}
    LCLProc, LCLType, LCLIntf, LResources, InterfaceBase,
    {$ENDIF}
  {$ENDIF}
  uCEFTypes, uCEFInterfaces, uCEFConstants, uCEFViewsFrameworkEvents, uCEFButtonComponent;

type
  {$IFNDEF FPC}{$IFDEF DELPHI16_UP}[ComponentPlatformsAttribute(pfidWindows or pfidOSX or pfidLinux)]{$ENDIF}{$ENDIF}
  TCEFLabelButtonComponent = class(TCEFButtonComponent)
    protected
      FLabelButton     : ICefLabelButton;
      FText            : ustring;

      procedure DestroyView; override;
      procedure Initialize; override;

      function  GetInitialized : boolean; override;
      function  GetAsView : ICefView; override;
      function  GetAsButton : ICefButton; override;
      function  GetAsLabelButton : ICefLabelButton; override;
      function  GetAsMenuButton : ICefMenuButton; virtual;
      function  GetText : ustring;
      function  GetImage(button_state: TCefButtonState): ICefImage;

      procedure SetText(const text_: ustring);
      procedure SetImage(button_state: TCefButtonState; const image: ICefImage);

      // ICefViewDelegateEvents
      procedure doCreateCustomView; override;

    public
      /// <summary>
      /// Create a new LabelButton. |aText| will be shown on the LabelButton and used as the default
      /// accessible name.
      /// </summary>
      procedure CreateLabelButton(const aText : ustring);
      /// <summary>
      /// Sets the text color shown for the specified button |for_state| to |color|.
      /// </summary>
      procedure SetTextColor(for_state: TCefButtonState; color: TCefColor);
      /// <summary>
      /// Sets the text colors shown for the non-disabled states to |color|.
      /// </summary>
      procedure SetEnabledTextColors(color: TCefColor);
      /// <summary>
      /// <para>Sets the font list. The format is "<FONT_FAMILY_LIST>,[STYLES] <SIZE>",
      /// where:</para>
      /// <code>
      /// - FONT_FAMILY_LIST is a comma-separated list of font family names,
      /// - STYLES is an optional space-separated list of style names (case-sensitive
      ///   "Bold" and "Italic" are supported), and
      /// - SIZE is an integer font size in pixels with the suffix "px".
      /// </code>
      /// <para>Here are examples of valid font description strings:</para>
      /// <code>
      /// - "Arial, Helvetica, Bold Italic 14px"
      /// - "Arial, 14px"
      /// </code>
      /// </summary>
      procedure SetFontList(const font_list: ustring);
      /// <summary>
      /// Sets the horizontal alignment; reversed in RTL. Default is
      /// CEF_HORIZONTAL_ALIGNMENT_CENTER.
      /// </summary>
      procedure SetHorizontalAlignment(alignment: TCefHorizontalAlignment);
      /// <summary>
      /// Reset the minimum size of this LabelButton to |size|.
      /// </summary>
      procedure SetMinimumSize(const size_: TCefSize);
      /// <summary>
      /// Reset the maximum size of this LabelButton to |size|.
      /// </summary>
      procedure SetMaximumSize(const size_: TCefSize);
      /// <summary>
      /// Gets and sets the text shown on the LabelButton. By default |text| will also be
      /// used as the accessible name.
      /// </summary>
      property  Text                                  : ustring         read GetText          write SetText;
      /// <summary>
      /// Returns the image shown for |button_state|. If no image exists for that
      /// state then the image for CEF_BUTTON_STATE_NORMAL will be returned.
      /// </summary>
      property  Image[button_state : TCefButtonState] : ICefImage       read GetImage         write SetImage;
      /// <summary>
      /// Returns this LabelButton as a MenuButton or NULL if this is not a
      /// MenuButton.
      /// </summary>
      property  AsMenuButton                          : ICefMenuButton  read GetAsMenuButton;
  end;

{$IFDEF FPC}
procedure Register;
{$ENDIF}

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

implementation

uses
  uCEFLabelButton, uCEFMiscFunctions, uCEFButtonDelegate;

procedure TCEFLabelButtonComponent.CreateLabelButton(const aText : ustring);
begin
  FText := aText;
  CreateView;
end;

procedure TCEFLabelButtonComponent.doCreateCustomView;
var
  TempDelegate : ICefButtonDelegate;
begin
  if (FLabelButton = nil) then
    try
      TempDelegate := TCustomButtonDelegate.Create(self);
      FLabelButton := TCefLabelButtonRef.CreateLabelButton(TempDelegate, FText);
    finally
      TempDelegate := nil;
    end;
end;

procedure TCEFLabelButtonComponent.DestroyView;
begin
  inherited DestroyView;

  FLabelButton := nil;
end;

procedure TCEFLabelButtonComponent.Initialize;
begin
  inherited Initialize;

  FLabelButton := nil;
end;

function TCEFLabelButtonComponent.GetInitialized : boolean;
begin
  Result := (FLabelButton <> nil);
end;

function TCEFLabelButtonComponent.GetAsView : ICefView;
begin
  if Initialized then
    Result := FLabelButton as ICefView
   else
    Result := nil;
end;

function TCEFLabelButtonComponent.GetAsButton : ICefButton;
begin
  if Initialized then
    Result := FLabelButton as ICefButton
   else
    Result := nil;
end;

function TCEFLabelButtonComponent.GetAsLabelButton : ICefLabelButton;
begin
  Result := FLabelButton;
end;

function TCEFLabelButtonComponent.GetAsMenuButton : ICefMenuButton;
begin
  Result := nil;
end;

procedure TCEFLabelButtonComponent.SetText(const text_: ustring);
begin
  FText := text_;

  if Initialized then
    AsLabelButton.SetText(FText);
end;

function TCEFLabelButtonComponent.GetText : ustring;
begin
  if Initialized then
    FText := AsLabelButton.GetText;

  Result := FText;
end;

procedure TCEFLabelButtonComponent.SetImage(button_state: TCefButtonState; const image: ICefImage);
begin
  if Initialized then AsLabelButton.SetImage(button_state, image);
end;

function TCEFLabelButtonComponent.GetImage(button_state: TCefButtonState): ICefImage;
begin
  if Initialized then
    Result := AsLabelButton.GetImage(button_state)
   else
    Result := nil;
end;

procedure TCEFLabelButtonComponent.SetTextColor(for_state: TCefButtonState; color: TCefColor);
begin
  if Initialized then AsLabelButton.SetTextColor(for_state, color);
end;

procedure TCEFLabelButtonComponent.SetEnabledTextColors(color: TCefColor);
begin
  if Initialized then AsLabelButton.SetEnabledTextColors(color);
end;

procedure TCEFLabelButtonComponent.SetFontList(const font_list: ustring);
begin
  if Initialized then AsLabelButton.SetFontList(font_list);
end;

procedure TCEFLabelButtonComponent.SetHorizontalAlignment(alignment: TCefHorizontalAlignment);
begin
  if Initialized then AsLabelButton.SetHorizontalAlignment(alignment);
end;

procedure TCEFLabelButtonComponent.SetMinimumSize(const size_: TCefSize);
begin
  if Initialized then AsLabelButton.SetMinimumSize(size_);
end;

procedure TCEFLabelButtonComponent.SetMaximumSize(const size_: TCefSize);
begin
  if Initialized then AsLabelButton.SetMaximumSize(size_);
end;

{$IFDEF FPC}
procedure Register;
begin
  {$I res/tceflabelbuttoncomponent.lrs}
  RegisterComponents('Chromium Views Framework', [TCEFLabelButtonComponent]);
end;
{$ENDIF}

end.
