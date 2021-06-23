unit unit_ws_container;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}  Classes, SysUtils,log_core,
  wsutils,
  wsmessages,
  wsstream,
  ssockets,
  WebsocketsClient;


type
  { TRecieverThread }

  TRecieverThread = class(TThread)
  private
    FCommunicator: TWebsocketCommunincator;
  protected
    procedure Execute; override;
  public
    constructor Create(ACommunicator: TWebsocketCommunincator);
  end;


type
  { TSimpleChat }

  TSimpleChat = class
  private
    FReciever: TRecieverThread;          //Thread for Reciving Msg
    FCommunicator: TWebsocketCommunincator;  //Handels connection
    l:tlog;
    procedure RecieveMessage(Sender: TObject);
    procedure StreamClosed(Sender: TObject);
  public
    procedure send(msg:string);
    constructor Create(ACommunicator: TWebsocketCommunincator);
    destructor Destroy; override;
  end;


implementation

uses unitmain;

{ TSimpleChat }

procedure TSimpleChat.StreamClosed(Sender: TObject);
begin
  //WriteLn('Connection to ', FCommunicator.SocketStream.RemoteAddress.Address, ' closed');
//  l.print('Connection to '+FCommunicator.SocketStream.RemoteAddress.Address+' closed');
  l.print('Connection (STREAM) closed');
end;

procedure TSimpleChat.RecieveMessage(Sender: TObject);
var
  MsgList: TWebsocketMessageOwnerList;
  m: TWebsocketMessage;
begin
  MsgList := TWebsocketMessageOwnerList.Create(True);
  try
    FCommunicator.GetUnprocessedMessages(MsgList);  //Nachrichten abrufen
    for m in MsgList do
      if m is TWebsocketStringMessage then begin
        //WriteLn('Message from ', FCommunicator.SocketStream.RemoteAddress.Address, ': ', TWebsocketStringMessage(m).Data)
        l.inp('Message from '+ FCommunicator.SocketStream.RemoteAddress.Address+ ': '+ TWebsocketStringMessage(m).Data);
      end
      else if m is TWebsocketPongMessage then begin
        //WriteLn('Pong from ', FCommunicator.SocketStream.RemoteAddress.Address, ': ', TWebsocketPongMessage(m).Data);
        l.inp('Pong from '+ FCommunicator.SocketStream.RemoteAddress.Address+ ': '+ TWebsocketPongMessage(m).Data);
      end;
  finally
    MsgList.Free; //Destroys MsgList (better: freeandnil() )
  end;
end;

procedure TSimpleChat.send(msg:string);     //sends a msg
var str:string;
begin
  str:=msg;
 if FCommunicator.Open then begin
    //ReadLn(str);
    if not FCommunicator.Open then Exit;
    {with FCommunicator.WriteMessage do
    try
       WriteRaw(str);
    finally
       Free;
    end;}
    try
      fcommunicator.WriteMessage.WriteRaw(msg)
    finally
      fcommunicator.WriteMessage.Free;
    end;
  end;

 {if FCommunicator.Open then begin
  try
    //fcommunicator.WriteMessage.WriteRaw(msg);
    fcommunicator.WriteMessage.WriteAnsiString(msg);
    fcommunicator.WriteMessage.DispatchStr(msg);
    //fcommunicator.Free;
    l.print('SENDING: '+msg);
  except
    //freeandnil(fcommunicator);
    //fcommunicator.Free;
    l.print('ERROR while sending this msg: '+msg);
  end;
 end;}
end;

constructor TSimpleChat.Create(ACommunicator: TWebsocketCommunincator);
begin
  //l:=tlog.create(1,formmain.e_log);
  //l.enable_line_nr;
  FCommunicator := ACommunicator;                       //Communicator zuweisen
  FCommunicator.OnClose:=@StreamClosed;                 //Event zuweisung
  FCommunicator.OnRecieveMessage:=@RecieveMessage;
  FReciever := TRecieverThread.Create(ACommunicator);   //Reciver erstellen
end;

destructor TSimpleChat.Destroy;
begin
  while not FReciever.Finished do begin
    Sleep(10);
  end;
  FReciever.Free;
  FCommunicator.Free;
  inherited Destroy;
end;

{ TRecieverThread }

procedure TRecieverThread.Execute;
begin
  while not Terminated and FCommunicator.Open do
  begin
    FCommunicator.RecieveMessage;
    Sleep(100);                      //polling of messages every 100ms 1/10s
  end;
end;

constructor TRecieverThread.Create(ACommunicator: TWebsocketCommunincator);
begin
  FCommunicator := ACommunicator;    //Communicator  (Verbindungs Handler ) wird durchgereicht
  inherited Create(False);           //?
end;

end.

