unit unit_tally;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazSerial, ws_handler_unit, graphics, jsontools, log_core, StdCtrls, lazsynaser;

type

{ trgb }

 trgb=record
     r,g,b:integer;
end;

type tsState=(sOff,sOn,sSelect);

type tSwState=(sTrBegin,sTrVidEnd,sTrEnd,sSwScenes,sSwNone);

type tcolorset=record
    front_on,front_off,front_sel,front_ident,back_on,back_off,back_sel,back_ident:trgb;
    dimmer_front,dimmer_back:byte;
end;

type

{ tip }

 tip=record
    b1,b2,b3,b4:byte;
end;

type

{ ttally }

 ttally=class
  private
    id:integer;
    dip_nr:integer; //nr from dip-sw positions
    device_health:byte; //connection state of Tally Slave
    afront_color:trgb;  //(actual) real color
    aback_color:trgb;
    efront_color:trgb;  //(edited) new color
    eback_color:trgb;
    tally_ip:tip; //tally ip in the internal tally network
    emode:byte;
    eanimation:byte;
    amode:byte;
    aanimation:byte;
    push_update:boolean;
    online:boolean; //if tally is online
  public
    constructor create(_id: integer);
    constructor create_dummy;
    procedure decode_tally_info(msg: array of byte);
    procedure set_front_color(_front_color:trgb);
    procedure set_back_color(_back_color:trgb);
    procedure set_online(_online:boolean);
    procedure init_tally(_msg: array of byte);
    function is_online:boolean;
    function get_id:integer;
    function get_dip_nr:integer;
    function get_device_health:byte;  //device health
    function get_front_color:trgb;   //returns actual color
    function get_back_color:trgb;    //returns actual color
    function get_efront_color:trgb; //returns expected cplor
    function get_eback_color:trgb; //returns expected color
    function get_tally_ip:string;
    function get_tally_ip_bin:tip;
    function new_data:boolean;
    function get_instr_str(msg_type:byte):string;   //complete string to send //msg_type=1 for instruction
end;

type

{ tscene }

 tscene=class
  private
    id:integer;      //remove
    name:ansistring;
    assigned_tallys:tlist;
    state:tsState; //0: inactive; 1: active; 2:preview
    preview:boolean;
    cs:tcolorset;
    tallys_update:tprocnoargs;
    procedure update_tallys;
    function dimm(color:byte;dimmer:byte):byte;
  public
    constructor create(_name:ansistring;_id:integer;_cs:tcolorset);   //remove id
    procedure add_tally(_tally:ttally);
    procedure remove_tally(_tally: ttally);
    procedure rename(_name:ansistring);
    procedure set_state(_state:tsState);
    procedure set_preview(_preview:boolean);
    procedure set_tallys_update_trigger(p:tprocnoargs);
    procedure set_cs(_cs:tcolorset);
    function get_cs:tcolorset;
    function get_preview:boolean;
    function get_state:tsState;
    function get_tallys:tlist;
    function get_name:ansistring;
    function get_id:integer;     //remove
    destructor destroy;
end;

type

{ ttally_controler }

 ttally_controler=class
  private
    com:tlazserial;
    ws:twshandler;
    tallys:tlist;
    scenes:tlist;
    tasklist:tstringlist;
    serialbuffer:ansistring;//buffer for serial communication
    last_msg_id:integer;
    l:tlog;
    cli:tcommandline;
    memo:tmemo;
    wanted_msg:ansistring;
    msg_received:boolean; //gets true if an msg with the suitable msg_id was received and saved in "whanted_msg"
    gui_update:tprocnoargs; //updates gui
    wait_for_msg:boolean; // gets true if an request was send and not yet fullfilled
    current_task:string;
    active_scene:tscene;  //holds the active scene
    preview_scene:tscene; //holds the preview scene or is nil
    next_scene:tscene; //holds the new scene
    studio_mode:boolean;
    switch_state:tswstate;
    next_tally_id:integer; //next id to assign
    notification_text:ansistring; //stores Text to notify user at nect gui_update()
    global_colorset:tcolorset;
    procedure init;
    procedure obs_update; //get's called when a new obs message arrived
    procedure master_update(sender:tobject); //get's called when a new master message has arrived
    procedure com_event(sender: tobject; Reason: THookSerialReason; const Value: string);     //get's called when an com event occurs
    procedure request(_type,_payload:ansistring);
    procedure check_tasklist; //checks for queried Tasks and trys to finish them
    procedure change_scene_state(sname:ansistring;sstate:tsstate);
    procedure switch_studiomode(switch_to:integer);
    procedure decode_master_str(s:string); //decodes string send from master
    procedure init_master;
    procedure send_to_master(s:ansistring); //sends string to master
    function msg_start(s:ansistring):integer; //returns start position of the string
    function msg_end(s: ansistring): integer; //returns the last byte position (checksumm) of the string
    function get_tally_by_ip(ip:string):ttally;
    function as_int(s:ansistring):ansistring; //converts char to byte values
    function exists_same_tally(dip_nr:byte):integer; //count of tallys with the same dip_nr
  public
    init_end:boolean;
    constructor create(_log:tlog;_cli:tcommandline);
    procedure set_com(_com:tlazserial);
    procedure set_gui_update_trigger(p:tprocnoargs);
    procedure assign_log(_memo:tmemo);
    procedure set_global_colorset(cs:tcolorset); //sets the colorset for all Tallys without
    procedure reset_global_colorset; //sets the default colorset
    procedure prime_tallys; //updates real tallys
    procedure reload_tallys; //reload tally list
    procedure display_tally_ecolors; //prints all current ecolors of all tallys to the log
    function get_global_colorset:tcolorset;
    function start_com_comm:boolean; //starts COM connection to Master
    function stop_com_comm:boolean;
    function start_obs_comm(_url:ansistring;_port:integer):integer;
    function stop_obs_comm():integer;
    function get_scene(index:integer):tscene; //changed to represent the list index (not the scene id)
    function get_tally(id:integer):ttally;
    function get_tally_by_dipnr(dip_nr:byte):ttally;
    function get_scene_count:integer;
    function get_scene_by_name(sname:ansistring):tscene;
    function get_tallys_count:integer;
    function get_tallys_online:integer;
    function get_online_tallys:tlist;//returns list with all current online tallys
    function is_studiomode:boolean;
    function is_obs_ready:boolean; //returns true if OBS Connection is initialized
    function get_notification_text:ansistring; //empty if no notification event present
end;

implementation

{ tscene }

procedure tscene.update_tallys;
var i:integer;
    t:ttally;
    fc,bc:trgb;
begin
  case state of
       sOff:begin
         fc.r:=dimm(cs.front_off.r,cs.dimmer_front);
         fc.g:=dimm(cs.front_off.g,cs.dimmer_front);
         fc.b:=dimm(cs.front_off.b,cs.dimmer_front);
         bc.r:=dimm(cs.back_off.r,cs.dimmer_back);
         bc.g:=dimm(cs.back_off.g,cs.dimmer_back);
         bc.b:=dimm(cs.back_off.b,cs.dimmer_back);
         end;
       sOn:begin
         fc.r:=dimm(cs.front_on.r,cs.dimmer_front);
         fc.g:=dimm(cs.front_on.g,cs.dimmer_front);
         fc.b:=dimm(cs.front_on.b,cs.dimmer_front);
         bc.r:=dimm(cs.back_on.r,cs.dimmer_back);
         bc.g:=dimm(cs.back_on.g,cs.dimmer_back);
         bc.b:=dimm(cs.back_on.b,cs.dimmer_back);
         end;
  end;
  if (preview=true) and (state=sOff) then begin
   bc.r:=dimm(cs.back_sel.r,cs.dimmer_back);
   bc.g:=dimm(cs.back_sel.g,cs.dimmer_back);
   bc.b:=dimm(cs.back_sel.b,cs.dimmer_back);
   fc.r:=dimm(cs.front_sel.r,cs.dimmer_front);
   fc.g:=dimm(cs.front_sel.g,cs.dimmer_front);
   fc.b:=dimm(cs.front_sel.b,cs.dimmer_front);
  end;
  if assigned_tallys.Count>0 then begin
   for i:=0 to assigned_tallys.count-1  do begin
     t:=ttally(assigned_tallys.items[i]);
     t.set_front_color(fc);
     t.set_back_color(bc);
   end;
   tallys_update;
  end;
end;

function tscene.dimm(color: byte; dimmer: byte): byte;
var s:real;
begin
  if dimmer=0 then begin
   result:=0;
  end
  else begin
       s:=dimmer/255;
       result:=round(color*s)
  end;
end;

constructor tscene.create(_name: ansistring; _id: integer;_cs:tcolorset);
begin
  name:=_name;
  id:=_id;
  assigned_tallys:=tlist.create();
  state:=sOff;
  preview:=false;
  cs:=_cs;
end;

procedure tscene.add_tally(_tally: ttally);     //adds tally if not previosly added
var i:integer;
    existent:boolean;
begin
  if assigned_tallys.Count>0 then begin
   for i:=0 to assigned_tallys.Count-1 do begin
     if _tally.get_id=ttally(assigned_tallys.Items[i]).get_id then existent:=true;
   end;
   if existent=false then assigned_tallys.Add(_tally);
  end
  else assigned_tallys.Add(_tally);
end;

procedure tscene.remove_tally(_tally: ttally);
var p:pointer;
begin
  assigned_tallys.Remove(_tally);
end;

procedure tscene.rename(_name: ansistring);
begin
  name:=_name
end;

procedure tscene.set_state(_state: tsState);
begin
  state:=_state;
  self.update_tallys;
end;

procedure tscene.set_preview(_preview: boolean);
begin
  preview:=_preview;
  self.update_tallys;
end;

procedure tscene.set_tallys_update_trigger(p: tprocnoargs);
begin
  tallys_update:=p;
end;

procedure tscene.set_cs(_cs: tcolorset);
begin
  cs:=_cs;
  update_tallys;
end;

function tscene.get_cs: tcolorset;
begin
  result:=cs;
end;

function tscene.get_preview: boolean;
begin
  result:=preview;
end;

function tscene.get_state: tsState;
begin
  result:=state;
end;

function tscene.get_tallys: tlist;   //maybe not so good because the org. list is returned
begin
  result:=assigned_tallys;
end;

function tscene.get_name: ansistring;
begin
  result:=name;
end;

function tscene.get_id: integer;
begin
  result:=id;
end;

destructor tscene.destroy;
begin
    assigned_tallys.Destroy;
end;

{ ttally }

constructor ttally.create(_id: integer);
begin
  push_update:=false;
  id:=_id;
  dip_nr:=id;
  with efront_color do begin
    r:=128;
    g:=0;
    b:=255;
  end;
  with eback_color do begin
    r:=128;
    g:=0;
    b:=255;
  end;
end;

constructor ttally.create_dummy;
begin
  push_update:=false;
  id:=-1;
  dip_nr:=0;
  with efront_color do begin
    r:=128;
    g:=0;
    b:=255;
  end;
  with eback_color do begin
    r:=128;
    g:=0;
    b:=255;
  end;
end;

procedure ttally.decode_tally_info(msg: array of byte); //does not change the dip_nr
var r,g,b:integer;
begin
  with tally_ip do begin
    b1:=msg[2];
    b2:=msg[3];
    b3:=msg[4];
    b4:=msg[5];
  end;
  with afront_color do begin
    r:=msg[6];
    g:=msg[7];
    b:=msg[8];
  end;
  with aback_color do begin
    r:=msg[10];
    g:=msg[11];
    b:=msg[12];
  end;
  device_health:=msg[13];
  amode:=msg[14];
  aanimation:=msg[15];
end;

procedure ttally.set_front_color(_front_color: trgb);
begin
  efront_color:=_front_color;
  push_update:=true;
end;

procedure ttally.set_back_color(_back_color: trgb);
begin
  eback_color:=_back_color;
  push_update:=true;
end;

procedure ttally.set_online(_online: boolean);
begin
  online:=_online;
end;

procedure ttally.init_tally(_msg: array of byte);
begin
  push_update:=true;
  dip_nr:=_msg[1];
  with efront_color do begin
    r:=0;
    g:=0;
    b:=0;
  end;
  with eback_color do begin
    r:=0;
    g:=0;
    b:=0;
  end;
  self.decode_tally_info(_msg);
end;

function ttally.is_online: boolean;
begin
  result:=online;
end;

function ttally.get_id: integer;
begin
  result:=id;
end;

function ttally.get_dip_nr: integer;
begin
  result:=dip_nr;
end;

function ttally.get_device_health: byte;
begin
  result:=device_health;
end;

function ttally.get_front_color: trgb;
begin
   result:=afront_color;
end;

function ttally.get_back_color: trgb;
begin
   result:=aback_color;
end;

function ttally.get_efront_color: trgb;
begin
  result:=efront_color;
end;

function ttally.get_eback_color: trgb;
begin
  result:=eback_color;
end;

function ttally.get_tally_ip: string;
begin
   result:=inttostr(tally_ip.b1)+'.'+inttostr(tally_ip.b2)+'.'+inttostr(tally_ip.b3)+'.'+inttostr(tally_ip.b4);
end;

function ttally.get_tally_ip_bin: tip;
begin
 result:=tally_ip;
end;

function ttally.new_data: boolean;
begin
  result:=push_update;
end;

function ttally.get_instr_str(msg_type: byte): string;
var s:string;
    chk_sum,i:integer;
begin
  s:=char(255);                                                                     //start_sequence
  s:=s+char(255)+char(255)+char(255);
  s:=s+char(msg_type);                                                                     //msg_type
  s:=s+char(dip_nr);                                                                //dip_nr
  s:=s+char(tally_ip.b1)+char(tally_ip.b2)+char(tally_ip.b3)+char(tally_ip.b4);     //target_ip
  s:=s+char(efront_color.r)+char(efront_color.g)+char(efront_color.b);              //front_color
  s:=s+char(128);                                                                   //Seperator Byte
  s:=s+char(eback_color.r)+char(eback_color.g)+char(eback_color.b);                 //back_color
  s:=s+char(1);                                                                     //device_health
  s:=s+char(emode);                                                                 //emode
  s:=s+char(eanimation);                                                            //enimation
  chk_sum:=0;
  for i:=5 to 21 do begin
      chk_sum:=chk_sum+ord(s[i]);
      if chk_sum>255 then chk_sum:=0;
  end;
  s:=s+char(chk_sum);                                                               //CheckSum
  s:=s+char(254)+char(254)+char(254)+char(254);                                     //end_sequence
  push_update:=false;
  result:=s;
end;

{ ttally_controler }

procedure ttally_controler.init;  //Init of the controller
var j,jarr:tjsonnode;
    s:tscene;
    i:integer;
    req_result:ansistring;
    current_scene:string;
begin
  case current_task of
  'none': begin
          l.print('INR: Request StudioModeStatus');
          init_end:=false;
          request('GetStudioModeStatus','');
          //current_task:='init_mode';
          tasklist.Add('init_mode');
          end;
  'init_mode':begin
    l.print('INR: Got StudioModeStatus');
    j:=tjsonnode.create;
    if j.TryParse(wanted_msg) then begin
       studio_mode:=j.Find('studio-mode').AsBoolean;
       //tasklist.Delete(tasklist.IndexOf('init_mode'));
    end;
    j.Destroy;
    //current_task:='init_scenes';
    tasklist.add('init_scenes');
    l.print('INR: Request SceneList');
    request('GetSceneList','');
    end;
  'init_scenes': begin
            l.print('INR: Got SceneList');
            j:=tjsonnode.create;
            if j.TryParse(wanted_msg) then begin
               current_scene:=j.Find('current-scene').asstring;
               jarr:=j.Find('scenes').AsArray;
               for i:=0 to jarr.Count-1 do begin
                  s:=tscene.create(jarr.Child(i).Find('name').AsString,i,global_colorset);
                  s.set_tallys_update_trigger(@self.prime_tallys);
                  if s.get_name=current_scene then begin
                   s.set_state(sOn); //Maybe change
                   active_scene:=s;
                  end;
                  scenes.add(s) //todo
                end;
            end;
            //tasklist.Delete(tasklist.IndexOf('init_scenes'));
            j.Destroy;
            if studio_mode then begin
              //current_task:='init_preview';
              tasklist.add('init_preview');
              l.print('INR: Request PreviewScene');
              request('GetPreviewScene','');
            end
            else begin
              init_end:=true;
              l.print('INR: Init End');
              //current_task:='none'
            end;
           end;
  'init_preview':begin
    l.print('INR: Got PreviewScene');
    j:=tjsonnode.create;
    if j.TryParse(wanted_msg) then begin
       preview_scene:=get_scene_by_name(j.Find('name').AsString);
       preview_scene.set_preview(true);
       //tasklist.Delete(tasklist.IndexOf('init_preview'));
    end;
    j.Destroy;
    //current_task:='none';
    l.print('INR: Init End');
    init_end:=true;
    end;
  end;
  gui_update; //updates gui
end;

procedure ttally_controler.obs_update;
var ms:tlist;
    m:twsmessage;
    i,k,del:integer;
    msg_id:integer;
    jobj,j:tjsonnode;
    update_type,sname:string;
    s:tscene;
begin
  jobj:=tjsonnode.create;
  if not wait_for_msg then begin
    ms:=ws.get_new_msg();
    if ms.Count>0 then begin
       for i:=0 to ms.Count-1 do begin
           m:=twsmessage(ms.items[i]);
           //l.print(m.get_data);
           if pos('"status": "error"',m.get_data)<>0 then begin
             //error
             //todo
             l.print('WSU: err: '+m.get_data);
           end
           else if pos('"message-id": "'+inttostr(last_msg_id)+'"',m.get_data)<>0 then begin
              //msg erwartet
              //l.print(twsmessage(ms.Items[i]).get_data);
              if jobj.TryParse(m.get_data) then begin
                 //valid JSON
                 msg_id:=strtoint(jobj.Find('message-id').AsString);
                 if msg_id=last_msg_id then begin
                    wanted_msg:=twsmessage(ms.Items[i]).get_data;
                    msg_received:=true;
                    l.print('WSU: Got new message (answer to an request)');
                    check_tasklist();
                 end;
              end;
           end
           else begin
               //other Update
                if jobj.TryParse(m.get_data) then begin
                   //valid JSON
                   update_type:=jobj.Find('update-type').asString;
                   case update_type of
                   'PreviewSceneChanged':begin
                       l.print('WSU: PreviewSceneChanged');
                       if assigned(preview_scene) then begin
                          preview_scene.set_preview(false);
                       end;
                       preview_scene:=get_scene_by_name(jobj.Find('scene-name').AsString);
                       preview_scene.set_preview(true);
                       gui_update;
                       end;
                   'SwitchScenes':begin
                        //?
                       l.print('WSU: SwitchScenes');
                       switch_state:=sSwScenes;
                       end;
                   'TransitionBegin':begin
                       l.print('WSU: TransitionBegin');
                       switch_state:=sTrBegin;
                       next_scene:=get_scene_by_name(jobj.find('to-scene').AsString);
                       next_scene.set_state(sOn);
                       gui_update;
                       end;
                   'TransitionVideoEnd':begin
                       //nothing
                       l.print('WSU: TransitionVideoEnd');
                       switch_state:=sTrVidEnd;
                       end;
                   'TransitionEnd':begin
                       l.print('WSU: TransitionEnd');
                       switch_state:=sTrEnd;
                       if assigned(active_scene) then begin
                        if active_scene.get_name<>next_scene.get_name then begin
                           active_scene.set_state(sOff);
                           end;
                        end;
                       active_scene:=next_scene; //maybe change
                       switch_state:=sSwNone;
                       gui_update;
                       end;
                   'StudioModeSwitched':begin
                        l.print('WSU: StudioModeSwitched');
                        if jobj.Find('new-state').asboolean then switch_studiomode(1)   //on
                        else switch_studiomode(0);         //off
                       end;
                   'SourceCreated':begin
                        l.print('WSU: SourceCreated');
                        if (jobj.Find('sourceType').asstring)=('scene') then begin
                         l.print('WSU: SceneCreated');
                         s:=tscene.create(jobj.find('sourceName').asstring,scenes.Count,global_colorset);
                         s.set_tallys_update_trigger(@self.prime_tallys);
                         scenes.Add(s);
                         end;
                        end;
                   'SourceDestroyed':begin
                        l.print('WSU: SourceDestroyed');
                        if (jobj.Find('sourceType').asstring)=('scene') then begin
                          sname:=jobj.Find('sourceName').asstring;
                          for k:=0 to scenes.Count-1 do begin
                               if (tscene(scenes.items[k]).get_name)=sname then begin
                                del:=k;
                               end;
                          end;
                          s:=tscene(scenes.items[del]);
                          scenes.Remove(s);
                          s.destroy;
                          l.print('WSU: SceneDestroyed');
                          gui_update;
                        end;
                     end;
                   'Exiting':begin
                        l.print('WSU: Exiting');
                        notification_text:='OBS wurde vom Benutzer geschlossen. Bitte trennen sie die Websocket Verbindung.';
                        //self.stop_obs_comm();
                        gui_update;
                     end;
                   'SourceRenamed':begin
                       l.print('WSU: SourceRenamed');
                       if (jobj.Find('sourceType').asstring)=('scene') then begin
                          sname:=jobj.Find('previousName').asstring;
                          s:=self.get_scene_by_name(sname);
                          s.rename(jobj.Find('newName').asstring);
                          l.print('WSU: Renamed "'+sname+'" to "'+s.get_name+'"');
                          gui_update;
                          end;
                       end;
                   end;
                end;
           end;
        end;
      end;
   end;
end;

procedure ttally_controler.master_update(sender:tobject);   //SerialConnetionUpdate
var s:ansistring;
    i:integer;
    m_start,m_end:integer;
begin
  if com.DataAvailable then begin
     l.print('SCU: new data available!');
     serialbuffer:=serialbuffer+com.readdata;
     //l.print('SCU: BUFFER: '+as_int(serialbuffer));
     if msg_start(serialbuffer)<>0 then begin
        //valid half msg
        l.print('SCU: valid half message received');
        m_start:=msg_start(serialbuffer);
        while m_start>5 do begin
          delete(serialbuffer,1,m_start-5); //moves start sequence to the begining of the string
          m_start:=msg_start(serialbuffer);
          l.print('SCU: removed garbage from buffer.');
        end;
        m_end:=msg_end(serialbuffer);
        if m_end<>0 then begin
           m_start:=msg_start(serialbuffer);
           s:=copy(serialbuffer,m_start,msg_end(serialbuffer)-m_start+1);
           //s:=copy(serialbuffer,msg_start(serialbuffer),17);
           delete(serialbuffer,1,msg_end(serialbuffer)+4); //removes msg from buffer
           l.print('SCU: extracted: '+as_int(s));
           l.print('SCU: message length: '+inttostr(length(s))+' B');
           if length(s)=17 then begin
              //valid TP string
              l.print('SCU: valid message extracted');
              decode_master_str(s);
           end
           else begin
              l.print('SCU: invalid message extracted');
           end;
        end;
     end;
  end;
end;

procedure ttally_controler.com_event(sender: tobject; Reason: THookSerialReason;
  const Value: string);
begin
  case Reason of
    HR_SerialClose : l.print('SCE: Port '+Value+' closed.');
    HR_Connect :   l.print('SCE: Port '+Value+' connected');
//    HR_CanRead :   StatusBar1.SimpleText := 'CanRead : ' + Value ;
//    HR_CanWrite :  StatusBar1.SimpleText := 'CanWrite : ' + Value ;
//    HR_ReadCount : StatusBar1.SimpleText := 'ReadCount : ' + Value ;
//    HR_WriteCount : StatusBar1.SimpleText := 'WriteCount : ' + Value ;
    HR_Wait :  l.print('SCE: Wait: '+Value);
  end ;
end;

procedure ttally_controler.request(_type, _payload: ansistring); //_payload should be formated //format: "key":"value"
var msg:string;
    msg_id:integer;
begin
  inc(last_msg_id);
  msg_id:=last_msg_id;
  if _payload='' then msg:='{"request-type":"'+_type+'","message-id":"'+inttostr(msg_id)+'"}'
  else msg:='{"request-type":"'+_type+'",'+_payload+',"message-id":"'+inttostr(msg_id)+'"}';
  wanted_msg:='';
  msg_received:=false;
  ws.send_str(msg);
end;

procedure ttally_controler.check_tasklist;
var i:integer;
    task:string;
    remove_from_tasklist:tstringlist;
begin
  remove_from_tasklist:=tstringlist.Create;
  l.print('TL: Checking Tasklist');
  if tasklist.count=0 then begin
    current_task:='none';
    l.print('TL: No Task');
  end
  else begin
    l.print('TL: Task Count: '+inttostr(tasklist.Count));
    for i:=1 to tasklist.Count do begin
      task:=tasklist.strings[i-1];
      l.print('TL: Task: '+task);
      case task of
      'init_scenes':begin
        current_task:=task;
        self.init;
        end;
      'init_mode':begin
        current_task:=task;
        self.init;
        end;
      'init_preview':begin
        current_task:=task;
        self.init;
        end;
      'ssm_get_preview':begin
        current_task:=task;
        self.switch_studiomode(2);
        end;
      else begin
           current_task:='none';
           l.print('TL: err: UNKNOWN -->going into Idle');
         end;
      end;
      remove_from_tasklist.Add(task);
    end;
    for i:=0 to remove_from_tasklist.count-1 do begin
      tasklist.Delete(tasklist.IndexOf(remove_from_tasklist.Strings[i]));
    end;
    if tasklist.Count=0 then begin
     current_task:='none';
     l.print('TL: going into Idle');
    end;
    remove_from_tasklist.Destroy;
    l.print('TL: Current Task: '+current_task);
  end;
end;

procedure ttally_controler.change_scene_state(sname: ansistring; sstate: tsstate);
var s:tscene;
    i:integer;
    //temp
    cs:tcolorset;
begin
  s:=self.get_scene_by_name(sname);
  s.set_state(sstate);
  {
  cs:=s.get_cs;
  l.print('Css: Color of scene #'+s.get_name+':');
  l.print(' | front_on:    | R:'+inttostr(cs.front_on.r)+' | G:'+inttostr(cs.front_on.g)+' | B:'+inttostr(cs.front_on.b));
  l.print(' | front_off:   | R:'+inttostr(cs.front_off.r)+' | G:'+inttostr(cs.front_off.g)+' | B:'+inttostr(cs.front_off.b));
  l.print(' | front_sel:   | R:'+inttostr(cs.front_sel.r)+' | G:'+inttostr(cs.front_sel.g)+' | B:'+inttostr(cs.front_sel.b));
  l.print(' | front_ident: | R:'+inttostr(cs.front_ident.r)+' | G:'+inttostr(cs.front_ident.g)+' | B:'+inttostr(cs.front_ident.b));
  l.print(' | back_on:     | R:'+inttostr(cs.back_on.r)+' | G:'+inttostr(cs.back_on.g)+' | B:'+inttostr(cs.back_on.b));
  l.print(' | back_off:    | R:'+inttostr(cs.back_off.r)+' | G:'+inttostr(cs.back_off.g)+' | B:'+inttostr(cs.back_off.b));
  l.print(' | back_sel:    | R:'+inttostr(cs.back_sel.r)+' | G:'+inttostr(cs.back_sel.g)+' | B:'+inttostr(cs.back_sel.b));
  l.print(' | back_ident:  | R:'+inttostr(cs.back_ident.r)+' | G:'+inttostr(cs.back_ident.g)+' | B:'+inttostr(cs.back_ident.b));
  l.print(' |-----------------------------------------------------------------------------------------|');
  l.print(' | dimmer_front: '+inttostr(cs.dimmer_front));
  l.print(' | dimmer_back: '+inttostr(cs.dimmer_back));
  }
end;

procedure ttally_controler.switch_studiomode(switch_to: integer);
var j:tjsonnode;
begin
  case switch_to of
  0:begin
    //off (called by update)
    studio_mode:=false;
    preview_scene.set_preview(false);
    preview_scene:=nil;
    l.print('SSM: Switched StudioMode to OFF');
    gui_update;
    end;
  1:begin
    //on (called by update)
    l.print('SSM: Switched StudioMode to ON');
    l.print('SSM: Loading Preview');
    tasklist.Add('ssm_get_preview');
    request('GetPreviewScene','');
    end;
  2:begin
    //called by tasklist
    j:=tjsonnode.Create;
    if j.TryParse(wanted_msg) then begin
       l.print('SSM: Got preview');
       preview_scene:=get_scene_by_name(j.Find('name').AsString);
       preview_scene.set_preview(true);
       studio_mode:=true;
       //tasklist.Delete(tasklist.IndexOf('ssm_get_preview'));
       //j.Destroy;
       gui_update;
       end;
    if assigned(j) then begin
     l.print('SSM: destroy OBJECT(j)');
     j.Destroy;
     end;
    end;
  end;
end;

procedure ttally_controler.decode_master_str(s: string);    //decodes string send from master
var i,chk_sum:integer;
    msg:array[0..16] of byte;
    t:ttally;
    ip:string;
begin
  for i:=1 to length(s) do msg[i-1]:=ord(s[i]);
  chk_sum:=0;
  for i:=0 to 15 do begin
    chk_sum:=chk_sum+msg[i];
    if chk_sum>255 then chk_sum:=0;
  end;
  if (chk_sum=msg[16]) and (msg[9]=128) then begin
    l.print('DMS: valid msg! CHK_SUM: '+inttostr(chk_sum)+' Seperator Byte: '+inttostr(msg[9]));
    case msg[0] of
    0:begin
      //test
      l.print('DMS: rec: TEST');
      end;
    2:begin
      //update
      l.print('DMS: rec: UPDATE');
      //ip:=inttostr(msg[2])+'.'+inttostr(msg[3])+'.'+inttostr(msg[4])+'.'+inttostr(msg[5]);
      t:=get_tally_by_dipnr(msg[1]);
      t.decode_tally_info(msg);
      l.print('DMS: Tally '+inttostr(t.get_dip_nr)+' updated.');
      gui_update;
      end;
    3:begin
      //init
      l.print('DMS: rec: TALLY_INIT');
      t:=get_tally_by_dipnr(msg[1]);
      t.decode_tally_info(msg);
      t.set_online(true);
      l.print('DMS: New Tally #'+inttostr(t.get_dip_nr)+' initalized.');
      gui_update;
      end;
    6:begin
      //tally removed
      l.print('DMS: rec: TALLY_REMOVED');
      t:=get_tally_by_dipnr(msg[1]);
      t.set_online(false);
      l.print('DMS: Tally #'+inttostr(t.get_dip_nr)+ ' removed.');
      send_to_master(t.get_instr_str(10));
      gui_update;
      end;
    8:begin
      //tally_added
      l.print('DMS: rec: TALLY_ADDED');
      t:=get_tally_by_dipnr(msg[1]);
      if t.is_online then begin
       t.decode_tally_info(msg);
       l.print('DMS: known Tally. Merged Data into #'+inttostr(t.get_dip_nr));
      end
      else begin
        t.init_tally(msg);
        l.print('DMS: New Tally #'+inttostr(t.get_dip_nr)+' initalized.');
      end;
      t.set_online(true);
      send_to_master(t.get_instr_str(9));    //tally_added_ack
      l.print('DMS: sended: TALLY_ADD_ACK');
      gui_update;
      end;
    11:begin
      //ready
      l.print('DMS: rec: READY');
      init_master;
      end;
    end;
  end
  else begin
    l.print('DMS: ERROR: invalid msg! CHK_SUM: '+inttostr(chk_sum)+' Seperator Byte: '+inttostr(msg[9]));
  end;
end;

procedure ttally_controler.init_master;
var msg:array [0..24] of byte;
    s_msg:string[25];
    i,chk_sum:integer;
begin
   for i:=0 to 24 do msg[i]:=0;
   for i:=0 to 3 do msg[i]:=255;
   for i:=21 to 24 do msg[i]:=254;
   msg[4]:=3;
   chk_sum:=0;
   for i:=0 to 19 do begin
     chk_sum:=chk_sum+msg[i];
     if chk_sum>255 then chk_sum:=0;
   end;
   msg[20]:=chk_sum;
   for i:=0 to 24 do s_msg[i+1]:=char(msg[i]);
   send_to_master(s_msg);
end;

procedure ttally_controler.send_to_master(s: ansistring);
begin
  if assigned(com) then begin
    if com.Active then begin
      //com.WriteBuffer(s,length(s));  //does not work!
      com.WriteData(s);
      l.print('STM: wrote message with length of '+inttostr(length(s))+ ' bytes.');
    end;
  end;
end;

procedure ttally_controler.prime_tallys;
var i:integer;
    t:ttally;
begin
  if tallys.count>0 then begin
    for i:=0 to tallys.count-1 do begin
      t:=ttally(tallys.items[i]);
      if t.is_online then begin;
        if t.new_data then begin
          send_to_master(t.get_instr_str(1));
          l.print('PT: primed tally #'+inttostr(t.get_dip_nr));
        end;
      end;
    end;
  end;
end;

procedure ttally_controler.reload_tallys;
begin
  init_master;
end;

procedure ttally_controler.display_tally_ecolors;
var i:integer;
    t:ttally;
    c_front,c_back:trgb;
begin
  for i:=0 to tallys.count-1 do begin
    t:=ttally(tallys.items[i]);
    if t.is_online then begin;
      c_front:=t.get_front_color;
      c_back:=t.get_back_color;
      l.print('Tally #'+inttostr(t.get_dip_nr)+' c_front: R: '+inttostr(c_front.r)+' G: '+inttostr(c_front.g)+' B: '+inttostr(c_front.b));
      l.print('Tally #'+inttostr(t.get_dip_nr)+' c_back: R: '+inttostr(c_back.r)+' G: '+inttostr(c_back.g)+' B: '+inttostr(c_back.b));
    end;
  end;
end;

function ttally_controler.msg_start(s: ansistring): integer;
var i:integer;
begin
  result:=0;
  if length(s)>4 then begin
    for i:=4 to length(s) do begin
      if(s[i]=chr(255)) then begin
        if(s[i-1]=chr(255)) then begin
          if(s[i-2]=chr(255)) then begin
           if(s[i-3]=chr(255)) then begin
            //start sequence
            if result=0 then begin
               result:=i+1;
            end;
           end;
          end;
        end;
      end;
    end;
  end;
  //l.print('msg_start returned '+inttostr(result));
end;

function ttally_controler.msg_end(s: ansistring): integer;
var i:integer;
begin
  result:=0;
  if length(s)>4 then begin
    for i:=4 to length(s) do begin
      if(s[i]=chr(254)) then begin
        if(s[i-1]=chr(254)) then begin
          if(s[i-2]=chr(254)) then begin
           if(s[i-3]=chr(254)) then begin
            //end sequence
            if result=0 then begin
              result:=i-4;
            end;
           end;
          end;
        end;
      end;
    end;
  end;
  l.print('msg_end returned '+inttostr(result));
end;

function ttally_controler.get_tally_by_ip(ip: string): ttally; //debricated
var i:integer;
begin
  result:=nil;
  for i:=0 to tallys.count-1 do begin
    if ttally(tallys.Items[i]).get_tally_ip=ip then result:=ttally(tallys.Items[i]);
  end;
end;

function ttally_controler.get_tally_by_dipnr(dip_nr: byte): ttally;
var i:integer;
begin
  result:=nil;
  for i:=0 to tallys.count-1 do begin
    if ttally(tallys.Items[i]).get_dip_nr=dip_nr then result:=ttally(tallys.items[i]);
  end;
end;

function ttally_controler.as_int(s: ansistring): ansistring;
var i:integer;
    rs:ansistring;
begin
  rs:='';
  for i:=1 to length(s) do rs:=rs+inttostr(ord(s[i]))+' ';
  result:=rs;
end;

function ttally_controler.exists_same_tally(dip_nr: byte): integer;
var t_count:integer;
    i:integer;
begin
  t_count:=0;
  for i:=0 to tallys.count-1 do begin
    if ttally(tallys.Items[i]).get_dip_nr=dip_nr then inc(t_count);
  end;
  result:=t_count;
end;

constructor ttally_controler.create(_log: tlog; _cli: tcommandline);
var t:ttally;
    i:integer;
begin
 l:=_log;
 cli:=_cli;
 cli.add_simple_command('DisplayAllTallyEColors',@display_tally_ecolors,'Displays the expected collor values for every online tally.');
 init_end:=false;
 tallys:=tlist.create;
 //fill tally list
 for i:=0 to 31 do begin
    t:=ttally.create(i);
    tallys.Add(t);
 end;
 scenes:=tlist.create;
 tasklist:=tstringlist.create;
 serialbuffer:='';
 wait_for_msg:=false;
 current_task:='none';
 wanted_msg:='';
 msg_received:=true;
 studio_mode:=false;
 switch_state:=sSwNone;
 next_tally_id:=0;
 notification_text:='';
 self.reset_global_colorset;
end;

procedure ttally_controler.set_com(_com: tlazserial);
begin
 com:=_com;
 com.OnRxData:=@master_update;
 com.OnStatus:=@com_event;
end;

procedure ttally_controler.set_gui_update_trigger(p: tprocnoargs);
begin
  gui_update:=p;
end;

procedure ttally_controler.assign_log(_memo: tmemo);     //outdated
begin
  memo:=_memo;
  l.assign_logobj(memo);
end;

procedure ttally_controler.set_global_colorset(cs: tcolorset);
var i:integer;
begin
  global_colorset:=cs;
  if scenes.count>0 then begin
     for i:=0 to scenes.Count-1 do begin
        tscene(scenes.items[i]).set_cs(global_colorset);
     end;
  end;
end;

procedure ttally_controler.reset_global_colorset;
var i:integer;
begin
  with global_colorset do begin
    front_on.r:=255;
    front_on.g:=0;
    front_on.b:=0;
    front_off.r:=0;
    front_off.g:=0;
    front_off.b:=0;
    front_sel.r:=0;
    front_sel.g:=0;
    front_sel.b:=0;
    front_ident.r:=255;
    front_ident.g:=0;
    front_ident.b:=255;
    back_on.r:=255;
    back_on.g:=0;
    back_on.b:=0;
    back_off.r:=0;
    back_off.g:=255;
    back_off.b:=0;
    back_sel.r:=255;
    back_sel.g:=255;
    back_sel.b:=0;
    back_ident.r:=255;
    back_ident.g:=0;
    back_ident.b:=255;
    dimmer_front:=255;
    dimmer_back:=255;
  end;
  if scenes.count>0 then begin
     for i:=0 to scenes.Count-1 do begin
        tscene(scenes.items[i]).set_cs(global_colorset);
     end;
  end;
  l.print('TC: Global colorset reseted. Colors are now:');
  l.print(' | front_on:    | R:'+inttostr(global_colorset.front_on.r)+' | G:'+inttostr(global_colorset.front_on.g)+' | B:'+inttostr(global_colorset.front_on.b));
  l.print(' | front_off:   | R:'+inttostr(global_colorset.front_off.r)+' | G:'+inttostr(global_colorset.front_off.g)+' | B:'+inttostr(global_colorset.front_off.b));
  l.print(' | front_sel:   | R:'+inttostr(global_colorset.front_sel.r)+' | G:'+inttostr(global_colorset.front_sel.g)+' | B:'+inttostr(global_colorset.front_sel.b));
  l.print(' | front_ident: | R:'+inttostr(global_colorset.front_ident.r)+' | G:'+inttostr(global_colorset.front_ident.g)+' | B:'+inttostr(global_colorset.front_ident.b));
  l.print(' | back_on:     | R:'+inttostr(global_colorset.back_on.r)+' | G:'+inttostr(global_colorset.back_on.g)+' | B:'+inttostr(global_colorset.back_on.b));
  l.print(' | back_off:    | R:'+inttostr(global_colorset.back_off.r)+' | G:'+inttostr(global_colorset.back_off.g)+' | B:'+inttostr(global_colorset.back_off.b));
  l.print(' | back_sel:    | R:'+inttostr(global_colorset.back_sel.r)+' | G:'+inttostr(global_colorset.back_sel.g)+' | B:'+inttostr(global_colorset.back_sel.b));
  l.print(' | back_ident:  | R:'+inttostr(global_colorset.back_ident.r)+' | G:'+inttostr(global_colorset.back_ident.g)+' | B:'+inttostr(global_colorset.back_ident.b));
  l.print(' |-----------------------------------------------------------------------------------------|');
  l.print(' | dimmer_front: '+inttostr(global_colorset.dimmer_front));
  l.print(' | dimmer_back: '+inttostr(global_colorset.dimmer_back));
end;

function ttally_controler.get_global_colorset: tcolorset;
begin
  result:=global_colorset;
end;

function ttally_controler.start_com_comm: boolean;
begin
  com.BaudRate:=br115200;
  com.DataBits:=tdatabits.db8bits;
  com.FlowControl:=fcnone;
  com.StopBits:=tstopbits.sbOne;
  try
    com.active:=true;
    result:=true;
    //init_master; //wait for master to be ready
  except
    result:=false;
  end;
end;

function ttally_controler.stop_com_comm: boolean;   //TODO: message master about closing the connection
begin
  try
    com.active:=false;
    result:=true;
  except
    result:=false;
  end;
end;

function ttally_controler.start_obs_comm(_url: ansistring; _port: integer
  ): integer;
var err:integer;
begin
  l.print('OWS: open websocket');
  ws:=twshandler.create();
  ws.set_onreceive(@obs_update);
  err:=ws.connect(_url,_port);
  if err=0 then begin // wenn verbindung erfolgreich aufgebaut
     self.init;
  end
  else begin
    l.print('SWS: err: '+inttostr(err));
    ws.Destroy;
  end;
  result:=err;
end;

function ttally_controler.stop_obs_comm(): integer;
var i:integer;
    s:tscene;
begin
  l.print('SWS: close websocket');
  if assigned(ws) then begin
     result:=ws.disconnect();
     ws.Destroy;
     if scenes.count>0 then begin
      active_scene:=nil;
      preview_scene:=nil;
      next_scene:=nil;
      for i:=scenes.count-1 to 0 do begin
        s:=tscene(scenes.items[i]);
        //scenes.Remove(s); //doesnt remove it?
        s.Destroy;
      end;
      scenes.Destroy;
      scenes:=tlist.create;
   end;
  end
  else result:=1;
  //no gui update!
end;

function ttally_controler.get_scene(index: integer): tscene;
var i:integer;
begin
  if index<scenes.count then result:=tscene(scenes.items[index])
  else result:=tscene.create('empty',0,global_colorset);
end;

function ttally_controler.get_tally(id: integer): ttally;
var i:integer;
begin
 if tallys.count=0 then result:=ttally.create_dummy      //obsolete
 else begin
  for i:=0 to tallys.count-1 do begin
    if (ttally(tallys.items[i]).get_dip_nr=id) then result:=ttally(tallys.items[i]);
  end;
 end;
end;

function ttally_controler.get_scene_count: integer;
begin
  result:=scenes.Count;
end;

function ttally_controler.get_scene_by_name(sname: ansistring): tscene;
var i:integer;
    s:tscene;
begin
  for i:=0 to scenes.count-1 do begin
    s:=tscene(scenes.Items[i]);
    if s.get_name=sname then result:=s;
  end;
  if not assigned(result) then result:=tscene.create('empty',-1,global_colorset);
end;

function ttally_controler.get_tallys_count: integer;     //returns online tallys
var i,a:integer;
begin
  result:=tallys.count;
end;

function ttally_controler.get_tallys_online: integer;
var i,a:integer;
begin
  a:=0;
  for i:=0 to tallys.count-1 do begin
    if ttally(tallys.Items[i]).is_online then inc(a);
  end;
  result:=a;
end;

function ttally_controler.get_online_tallys: tlist;
var online:tlist;
    i:integer;
    t:ttally;
begin
  online:=tlist.create();
  for i:=0 to tallys.Count-1 do begin
    t:=ttally(tallys.Items[i]);
    if t.is_online then online.Add(t);
  end;
  result:=online;
end;

function ttally_controler.is_studiomode: boolean;
begin
  result:=studio_mode;
end;

function ttally_controler.is_obs_ready: boolean;
begin
  result:=init_end;
end;

function ttally_controler.get_notification_text: ansistring;
var text:ansistring;
begin
  text:=notification_text;
  notification_text:='';
  result:=text;
end;

end.

