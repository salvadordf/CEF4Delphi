unit uCEFDomVisitor;

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
  TCefDomVisitorOwn = class(TCefBaseRefCountedOwn, ICefDomVisitor)
    protected
      procedure visit(const document: ICefDomDocument); virtual;

    public
      constructor Create; virtual;
  end;

  TCefFastDomVisitor = class(TCefDomVisitorOwn)
    protected
      FProc    : TCefDomVisitorProc;

      procedure visit(const document: ICefDomDocument); override;

    public
      constructor Create(const proc: TCefDomVisitorProc); reintroduce; virtual;
  end;

  TCefFastDomVisitor2 = class(TCefDomVisitorOwn)
    protected
      FProc    : TCefDomVisitorProc2;
      FBrowser : ICefBrowser;
      FFrame   : ICefFrame;

      procedure visit(const document: ICefDomDocument); override;

    public
      constructor Create(const browser: ICefBrowser; const frame: ICefFrame; const proc: TCefDomVisitorProc2); reintroduce; virtual;
      destructor  Destroy; override;
  end;

  TCefFastDomVisitor3 = class(TCefDomVisitorOwn)
    protected
      FProc    : TCefDomVisitorProc3;
      FBrowser : ICefBrowser;
      FFrame   : ICefFrame;
      FValue   : ustring;

      procedure visit(const document: ICefDomDocument); override;

    public
      constructor Create(const browser: ICefBrowser; const frame: ICefFrame; const proc: TCefDomVisitorProc3; const aValue : ustring); reintroduce; virtual;
      destructor  Destroy; override;
  end;


implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFDomDocument;

procedure cef_dom_visitor_visite(self: PCefDomVisitor; document: PCefDomDocument); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefDomVisitorOwn) then
    TCefDomVisitorOwn(TempObject).visit(TCefDomDocumentRef.UnWrap(document));
end;

// TCefDomVisitorOwn

constructor TCefDomVisitorOwn.Create;
begin
  inherited CreateData(SizeOf(TCefDomVisitor));

  PCefDomVisitor(FData)^.visit := {$IFDEF FPC}@{$ENDIF}cef_dom_visitor_visite;
end;

procedure TCefDomVisitorOwn.visit(const document: ICefDomDocument);
begin

end;

// TCefFastDomVisitor

constructor TCefFastDomVisitor.Create(const proc: TCefDomVisitorProc);
begin
  inherited Create;

  FProc := proc;
end;

procedure TCefFastDomVisitor.visit(const document: ICefDomDocument);
begin
  FProc(document);
end;


// TCefFastDomVisitor2

constructor TCefFastDomVisitor2.Create(const browser: ICefBrowser; const frame: ICefFrame; const proc: TCefDomVisitorProc2);
begin
  inherited Create;

  FBrowser := browser;
  FFrame   := frame;
  FProc    := proc;
end;

destructor TCefFastDomVisitor2.Destroy;
begin
  FBrowser := nil;
  FFrame   := nil;

  inherited Destroy;
end;

procedure TCefFastDomVisitor2.visit(const document: ICefDomDocument);
begin
  FProc(FBrowser, FFrame, document);
end;


// TCefFastDomVisitor3

constructor TCefFastDomVisitor3.Create(const browser: ICefBrowser; const frame: ICefFrame; const proc: TCefDomVisitorProc3; const aValue : ustring);
begin
  inherited Create;

  FBrowser := browser;
  FFrame   := frame;
  FProc    := proc;
  FValue   := aValue;
end;

destructor TCefFastDomVisitor3.Destroy;
begin
  FBrowser := nil;
  FFrame   := nil;

  inherited Destroy;
end;

procedure TCefFastDomVisitor3.visit(const document: ICefDomDocument);
begin
  FProc(FBrowser, FFrame, document, FValue);
end;

end.
