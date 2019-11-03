program fuckfpws;
{$mode objfpc}{$H+}
uses Classes, fphttpserver;

Type TTestHTTPServer = Class(TFPHTTPServer)
  public procedure HandleRequest(Var req:TFPHTTPConnectionRequest; Var res:TFPHTTPConnectionResponse); override;
  end;

Var ws:TTestHTTPServer;

procedure TTestHTTPServer.HandleRequest(var req:TFPHTTPConnectionRequest; var res:TFPHTTPConnectionResponse);
Var F:TStringStream;
begin
  F:=TStringStream.Create('Goodbye,World!');
  res.ContentLength:=F.Size;
  res.ContentStream:=F;
  res.SendContent;
  res.ContentStream:=Nil;
  F.Free;
end;

begin
  ws:=TTestHTTPServer.Create(Nil);
  ws.Threaded:=False;
  ws.Port:=18856;
  ws.AcceptIdleTimeout:=1000;
  ws.Active:=True;
  ws.Free;
end.
