unit ws_handler_unit;
//Version 1.0 working
{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  wsutils,
  wsmessages,
  wsstream,
  ssockets,
  WebsocketsClient;
type

{ treceiverthread }

 treceiverthread = class(TThread)
   private
     ws_com: TWebsocketCommunincator;
   protected
     procedure Execute; override;
   public
     constructor create(_comm: TWebsocketCommunincator);
   end;

type

{ twsmessage }

 twsmessage=class
  private
     data:ansistring;
     sender:ansistring;
  public
    readed:boolean;
    constructor create(_data,_sender:ansistring);
    function get_data:ansistring;
    function get_sender:ansistring;
end;

type TprocNoArgs = procedure of object;

type

{ twshandler }

 twshandler=class
  private
    new_msgl:tlist;
    comm:TWebsocketCommunincator;
    client: TWebsocketClient;
    receiver: TReceiverThread;
    on_receive: TprocNoArgs; //procedural pointer to an external procedure
    on_close: tprocnoargs; //procedural pointer to an external procedure
    procedure internal_onclose(Sender: TObject); //Triggered from twebsocketclient
    procedure internal_onreceive(Sender: TObject);   //triggered from twebsocketclient
  public
    constructor create();
    procedure set_onreceive(p:TprocNoArgs);
    procedure set_onclose(p:Tprocnoargs);
    function connect(url:string;port:integer):integer;
    function disconnect():integer;
    function send_str(str:string):integer;
    function receive():integer;
    function get_new_msg:tlist;
end;
implementation

{ twshandler }

procedure twshandler.internal_onclose(Sender: TObject);
begin
  if assigned(on_close) then on_close;; //on_close Handler aufrufen
end;

procedure twshandler.internal_onreceive(Sender: TObject);
begin
  self.receive; //Nachrichten abrufen
  if assigned(on_receive) then on_receive;   //on_receive Handler aufrufen
end;

constructor twshandler.create();
begin
  //pass
end;

procedure twshandler.set_onreceive(p: TprocNoArgs);
begin
  on_receive:=p;
end;

procedure twshandler.set_onclose(p: Tprocnoargs);
begin
  on_close:=p
end;

function twshandler.connect(url: string; port: integer): integer;
begin
  try
    client:=twebsocketclient.Create(url,port,'');
    comm:=client.Connect(TSocketHandler.Create);
    receiver:= TReceiverThread.Create(comm);
    comm.OnClose:=@internal_onclose;
    comm.OnRecieveMessage:=@internal_onreceive;
    result:=0;
  except
    if assigned(receiver) then receiver.destroy;
    if assigned(comm) then comm.Destroy;
    if assigned(client) then client.Destroy;
    result:=1; //Error
  end;
end;

function twshandler.disconnect(): integer;
begin
  if assigned(comm) then begin
    if comm.Open then comm.Close();
    while comm.Open do sleep(100);
  end;
  if comm.Open then result:=1
  else result:=0;
end;

function twshandler.send_str(str: string): integer;
begin
  if comm.Open then begin
    with comm.WriteMessage do
      try
        WriteRaw(str);
      finally
        Free;
      end;
  end;
  result:=0;
end;

function twshandler.receive(): integer;
var msgl: TWebsocketMessageOwnerList;
    mi:integer;
    msg:twsmessage;
begin
    try
      msgl:=TWebsocketMessageOwnerList.Create(True);
      comm.GetUnprocessedMessages(msgl);  //Nachrichten abrufen
      if not assigned(new_msgl) then new_msgl:=tlist.create();   // creates new_msgl if it doesn't exists
      for mi:=0 to msgl.count-1 do begin
        if msgl.Items[mi] is TWebsocketStringMessage then begin
           msg:=twsmessage.create(TWebsocketStringMessage(msgl.items[mi]).Data,comm.SocketStream.RemoteAddress.Address);
           new_msgl.Add(msg);
        end;
      end;
    finally
      //msgl.Free; //Destroys MsgList (better: freeandnil() )
      freeandnil(msgl);
    end;
    result:=0;
end;

function twshandler.get_new_msg: tlist;
begin
  if not assigned(new_msgl) then new_msgl:=tlist.create();
  result:=new_msgl;
  new_msgl:=tlist.create();
end;


{ twsmessage }

constructor twsmessage.create(_data, _sender: ansistring);
begin
  data:=_data;
  sender:=_sender;
  readed:=false;
end;

function twsmessage.get_data: ansistring;
begin
  result:=data;
  readed:=true;
end;

function twsmessage.get_sender: ansistring;
begin
  result:=sender;
end;

{ treceiverthread }

procedure treceiverthread.Execute;
begin
  while not Terminated and ws_com.Open do begin
    ws_com.RecieveMessage;
    Sleep(10);      //polling of messages every 10ms //maybe change to make it faster
  end;
end;

constructor treceiverthread.create(_comm: TWebsocketCommunincator);
begin
  ws_com:= _comm;    //Communicator  (Verbindungs Handler ) wird durchgereicht
  inherited Create(False);  //create wird von tthread geerbt
end;

end.

