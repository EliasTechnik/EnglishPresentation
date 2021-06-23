unit obsws;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}  Classes,  SysUtils,log_core,wsutils,wsmessages,wsstream,ssockets,WebsocketsClient,fpjson, jsonparser, StdCtrls;

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

type

{ twsclient }

 twsclient=class
  private
    errorlist:tstringlist;
    receiver: TReceiverThread;          //Thread for Reciving Msg
    ws_com: TWebsocketCommunincator;  //Handels connection
    ws_client:twebsocketclient;
    new_msgl:tlist;
    l:tlog;
    procedure recieve_msg(Sender: TObject);
    procedure stream_closed(Sender: TObject);
    function submit_error(_error_txt:ansistring):integer;
  public
    constructor create(_host:ansistring;_port:integer;_path:ansistring);
    procedure assign_log(_log:tlog);
    function connect():integer;
    function get_error(_error_id:integer):ansistring;    //remove that useless feature
    function disconnect():integer;
    function send(_msg:string):integer;
    function get_new_messages():tlist;
end;

implementation

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
    Sleep(100);      //polling of messages every 100ms 1/10s  //maybe change to make it faster
  end;
end;

constructor treceiverthread.create(_comm: TWebsocketCommunincator);
begin
  ws_com:= _comm;    //Communicator  (Verbindungs Handler ) wird durchgereicht
  inherited Create(False);  //create wird von tthread geerbt
end;

{ twsclient }

procedure twsclient.recieve_msg(Sender: TObject);
var msgl: TWebsocketMessageOwnerList;
    m: TWebsocketMessage;
    msg:twsmessage;
begin
  msgl:=TWebsocketMessageOwnerList.Create(True);
  try
    ws_com.GetUnprocessedMessages(msgl);  //Nachrichten abrufen
    if not assigned(new_msgl) then new_msgl:=tlist.create();
    for m in msgl do
      if m is TWebsocketStringMessage then begin
        msg:=twsmessage.create(TWebsocketStringMessage(m).Data,ws_com.SocketStream.RemoteAddress.Address);
        new_msgl.Add(msg);
        //l.inp('Message from '+ FCommunicator.SocketStream.RemoteAddress.Address+ ': '+ TWebsocketStringMessage(m).Data);
      end;
  finally
    //msgl.Free; //Destroys MsgList (better: freeandnil() )
    freeandnil(msgl);
  end;
end;

procedure twsclient.stream_closed(Sender: TObject);
begin
  //todo
end;

function twsclient.submit_error(_error_txt: ansistring): integer;
begin
  result:=errorlist.Add(_error_txt);
end;

constructor twsclient.create(_host: ansistring; _port: integer;
  _path: ansistring);
begin
  errorlist:=tstringlist.Create;
  ws_client:=twebsocketclient.Create(_host,_port,_path);
end;

procedure twsclient.assign_log(_log: tlog);
begin
  l:=_log;
end;

function twsclient.connect(): integer;
begin
  try
     ws_com:=ws_client.Connect(TSocketHandler.Create);
     ws_com.OnClose:=@stream_closed;
     ws_com.OnRecieveMessage:=@recieve_msg;
     receiver:=treceiverthread.create(ws_com);
     result:=0; //0= Connection established
  except
    result:=1; //1= Connection error
  end;
end;

function twsclient.get_error(_error_id: integer): ansistring;
begin
  if _error_id>=errorlist.Count then result:=''
  else result:=errorlist.Strings[_error_id];
end;

function twsclient.disconnect(): integer;
begin
  if assigned(ws_com) then begin
    if ws_com.Open then ws_com.Close();
    while ws_com.Open do sleep(100);
  end;
  if ws_com.Open then result:=1
  else result:=0;
end;

function twsclient.send(_msg: string): integer;
begin
 result:=0;
 if ws_com.Open then begin
    //if not ws_com.Open then Exit;
    try
      //test
      _msg:='hi server';
      l.outp('SEND STR: '+_msg);
      ws_com.WriteMessage(wmtString,Word.maxvalue).Write(_msg,length(_msg));
      l.outp('SENDED!');
      //ws_com.WriteMessage.WriteRaw('{"request-type":"GetSceneList","message-id":"1"}');
      result:=1;
    finally
      ws_com.WriteMessage.Free;
    end;
  end;
end;

function twsclient.get_new_messages(): tlist;
begin
  if not assigned(new_msgl) then new_msgl:=tlist.create();
  result:=new_msgl;
  new_msgl:=tlist.create();
end;

end.

