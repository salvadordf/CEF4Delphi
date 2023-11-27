unit uHelperProcessDom;

(* The code in this unit is executed in the renderer sub-process
*)

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uCEFApplication, uCEFInterfaces, uCEFDomVisitor, uCEFTypes,
  uCEFProcessMessage;

const
  MSG_REQUEST_DOM = 'MSG_REQUEST_DOM';
  MSG_RESPONSE_DOM = 'MSG_RESPONSE_DOM';

procedure InitProcessMessagesHandler;

implementation

type

  { TDomVisitorFindXY }

  TDomVisitorFindXY = class(TCefDomVisitorOwn)
    protected
      FFrame   : ICefFrame;
      FX, FY: Integer;
      procedure visit(const document: ICefDomDocument); override;
    public
      constructor Create(AFrame: ICefFrame; X,Y: Integer); reintroduce; virtual;
  end;


{ TDomVisitorFindXY }

procedure TDomVisitorFindXY.visit(const document: ICefDomDocument);
var
  node, foundNode: ICefDomNode;
  nb: TCefRect;
  TempMsg : ICefProcessMessage;
begin
  node := document.GetBody;
  foundNode := nil;
  if node <> nil then
    begin
      nb := node.GetElementBounds;
      if (FX >= nb.x) and (FX < nb.x + nb.width) and
         (FY >= nb.y) and (FY < nb.y + nb.height)
      then
        begin
          while node <> nil do
            begin
              foundNode := node;
              if node.HasChildren then
                begin
                  node := node.FirstChild;
                  while node <> nil do
                    begin
                      nb := node.GetElementBounds;
                      if (FX >= nb.x) and (FX < nb.x + nb.width) and
                         (FY >= nb.y) and (FY < nb.y + nb.height)
                      then
                        break  // go outer loop
                      else
                        node := node.NextSibling;
                    end;
                end
                else
                  node := nil;
            end;
        end;
    end;

  TempMsg := TCefProcessMessageRef.New(MSG_RESPONSE_DOM); // Same name than TCefCustomRenderProcessHandler.MessageName
  if foundNode <> nil then
    TempMsg.ArgumentList.SetString(0, foundNode.AsMarkup)
  else
    TempMsg.ArgumentList.SetString(0, 'Not Found');
  if (FFrame <> nil) and FFrame.IsValid then
    FFrame.SendProcessMessage(PID_BROWSER, TempMsg);
end;

constructor TDomVisitorFindXY.Create(AFrame: ICefFrame; X, Y: Integer);
begin
  FFrame := AFrame;
  FX := X;
  FY := Y;
  inherited Create;
end;


procedure DoProcessMessageReceived(const browser: ICefBrowser;
  const frame: ICefFrame; sourceProcess: TCefProcessId;
  const message: ICefProcessMessage; var aHandled: boolean);
var
  TempVisitor : TDomVisitorFindXY;
begin
  aHandled := False;
  case message.Name of
    MSG_REQUEST_DOM: begin
      if (frame <> nil) and frame.IsValid then
        begin
          TempVisitor := TDomVisitorFindXY.Create(
            frame,
            message.ArgumentList.GetInt(0),
            message.ArgumentList.GetInt(1)
          );
          frame.VisitDom(TempVisitor);
        end;
      aHandled := True;

    end;
  end;
end;

procedure InitProcessMessagesHandler;
begin
  GlobalCEFApp.OnProcessMessageReceived := @DoProcessMessageReceived;
end;

end.

