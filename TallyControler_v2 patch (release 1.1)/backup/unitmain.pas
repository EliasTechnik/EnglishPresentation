unit unitmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  ExtCtrls, ColorBox, Grids, ComCtrls, Buttons, ValEdit, CheckLst, wsutils,
  wsmessages, wsstream, ssockets, WebsocketsClient, log_core, unit_ws_container,
  obsws, LazSerial, unit_ws_setup, unit_tally, unit_form_log, BLCKSOCK, synaser, unit_form_benchmark,StrUtils;
//BLCKSOCK enthält synaser  //synaser muss extra includiert werden

type

  { Tformmain }

  Tformmain = class(TForm)
    b_reload_tallys: TButton;
    b_colorreset: TButton;
    b_ws_setup: TButton;
    cb_com_port: TComboBox;
    cfront_on: TColorButton;
    cfront_off: TColorButton;
    cfront_sel: TColorButton;
    cfront_ident: TColorButton;
    cback_on: TColorButton;
    cback_sel: TColorButton;
    cback_ident: TColorButton;
    cback_off: TColorButton;
    cd: TColorDialog;
    cb_scene: TComboBox;
    cb_flip_sides: TCheckBox;
    clb_tallys: TCheckListBox;
    cb_single_tally: TComboBox;
    gb_obs_conn: TGroupBox;
    gb_master_conn: TGroupBox;
    gb_tally_colors: TGroupBox;
    gb_tally_options: TGroupBox;
    gb_tally_status: TGroupBox;
    gb_scenes: TGroupBox;
    gb_as_tally: TGroupBox;
    gb_single_tally: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    l_com_status: TLabel;
    mi_darkmode: TMenuItem;
    mi_benchmark: TMenuItem;
    mm: TMainMenu;
    mi_config: TMenuItem;
    mi_tools: TMenuItem;
    mi_over: TMenuItem;
    mi_log: TMenuItem;
    mi_save: TMenuItem;
    mi_save_under: TMenuItem;
    mi_load: TMenuItem;
    rtr: TPanel;
    rtop: TPanel;
    serial: TLazSerial;
    l_ws_status: TLabel;
    pleft: TPanel;
    right: TPanel;
    sh_ws_status: TShape;
    sg_scenes: TStringGrid;
    sb: TStatusBar;
    sh_com_status: TShape;
    sg_tallys: TStringGrid;
    spliter_1: TSplitter;
    tb_ws_conn: TToggleBox;
    tb_com_conn: TToggleBox;
    tb_front_dimmer: TTrackBar;
    tb_back_dimmer: TTrackBar;
    tb_ident_tally: TToggleBox;
    procedure b_colorresetClick(Sender: TObject);
    procedure b_reload_tallysClick(Sender: TObject);
    procedure b_ws_setupClick(Sender: TObject);
    procedure cback_identColorChanged(Sender: TObject);
    procedure cback_offColorChanged(Sender: TObject);
    procedure cback_onColorChanged(Sender: TObject);
    procedure cback_selColorChanged(Sender: TObject);
    procedure cb_com_portChange(Sender: TObject);
    procedure cb_sceneEditingDone(Sender: TObject);
    procedure cb_single_tallyEditingDone(Sender: TObject);
    procedure cfront_identColorChanged(Sender: TObject);
    procedure cfront_offColorChanged(Sender: TObject);
    procedure cfront_onColorChanged(Sender: TObject);
    procedure cfront_selColorChanged(Sender: TObject);
    procedure clb_tallysClickCheck(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mi_benchmarkClick(Sender: TObject);
    procedure mi_darkmodeClick(Sender: TObject);
    procedure mi_logClick(Sender: TObject);
    procedure mi_saveClick(Sender: TObject);
    procedure tb_back_dimmerChange(Sender: TObject);
    procedure tb_com_connClick(Sender: TObject);
    procedure tb_front_dimmerChange(Sender: TObject);
    procedure tb_ws_connClick(Sender: TObject);
  private
    comm_cb_index:integer;
    log:tlog;
    cli:tcommandline;
    com_allowed:boolean;
  public
    tc:ttally_controler;
    procedure update_scenes;
    procedure update_gui;
    procedure update_com;
    procedure fill_color_form;
    procedure update_as_tallys;
    procedure update_tallys;
    procedure update_colorset;
    procedure update_single_tally;
    procedure set_form_color;
    function rgbtohexstr(c:trgb):string;
    function colortotrgb(s:tcolor):trgb;
    function extract_dipnr(s:string):integer;


  end;

var
  formmain: Tformmain;

implementation

{$R *.lfm}

{ Tformmain }

procedure Tformmain.FormCreate(Sender: TObject);
var h,w:integer;
begin
  log:=tlog.create(1);
  cli:=tcommandline.create(log);
  tc:=ttally_controler.create(log,cli);
  tc.set_gui_update_trigger(@update_gui);
  tc.set_com(serial);
  tc.reset_global_colorset;
  fill_color_form;
  com_allowed:=true;//false;   //switches Com OFF and ON (Hardcoded)
  comm_cb_index:=0;
  update_com;

  //set window position
  h:=screen.Height;
  w:=screen.Width;
  formmain.left:=0;
  formmain.top:=0;

  set_form_color;
end;

procedure Tformmain.mi_benchmarkClick(Sender: TObject);
begin
  form_benchmark.Show;
end;

procedure Tformmain.mi_darkmodeClick(Sender: TObject);     //darkmode
begin
  if mi_darkmode.Checked then mi_darkmode.Checked:=false
  else mi_darkmode.checked:=true;
  set_form_color;
end;

procedure Tformmain.mi_logClick(Sender: TObject);
begin
  log.assign_logobj(form_log.m_log);
  log.enable_output;
  cli.assign_input(form_log.e_command);
  form_log.Show;
end;

procedure Tformmain.mi_saveClick(Sender: TObject);
begin

end;

procedure Tformmain.tb_back_dimmerChange(Sender: TObject);
begin
  update_colorset;
end;

procedure Tformmain.tb_com_connClick(Sender: TObject);
var status:boolean;
begin
  if com_allowed then begin
    if tb_com_conn.Checked=true then begin
      //open connection
      l_com_status.Caption:='Versuche zu verbinden...';
      serial.Device:=cb_com_port.text;
      status:=tc.start_com_comm;
      if status then begin
       //ok
       sh_com_status.brush.Color:=rgbtocolor(0,255,0);
       l_com_status.Caption:='Verbunden';
       tb_com_conn.Caption:='Verbunden';
      end
      else begin
       //error
       sh_com_status.brush.Color:=rgbtocolor(255,0,0);
       l_com_status.Caption:='Verbindungsversuch fehlgeschlagen! Nicht verbunden.';
       tb_com_conn.Checked:=false;
      end;
    end
    else begin
      //close connection
      l_com_status.Caption:='Trenne Verbindung...';
      status:=tc.stop_com_comm();
      if status then begin
        //closed
        l_com_status.Caption:='Verbindung getrennt.';
        sh_com_status.brush.Color:=rgbtocolor(255,0,0);
        tb_com_conn.Caption:='Verbinden';

      end
      else begin
        //closing failed
        l_com_status.Caption:='Fehler: Konnte Verbindung nicht trennen!';
        sh_com_status.brush.Color:=rgbtocolor(255,0,0);
        tb_com_conn.Caption:='Verbunden';
        tb_com_conn.checked:=true;
      end;
    end;
  end
  else begin
      if tb_com_conn.Checked then tb_com_conn.State:=cbUnchecked
      else tb_com_conn.State:=cbChecked
  end;
  update_gui;
end;

procedure Tformmain.tb_front_dimmerChange(Sender: TObject);
begin
  update_colorset;
end;

procedure Tformmain.tb_ws_connClick(Sender: TObject);
var status:integer;
begin
  sh_ws_status.brush.Color:=rgbtocolor(255,128,0);

  if tb_ws_conn.Checked=true then begin
    //try to open connection
    l_ws_status.Caption:='Versuche zu verbinden...';
    //sleep(1000);
    status:=tc.start_obs_comm(form_ws_setup.ip,form_ws_setup.ws_port);
    if status=0 then begin
      //open
      sh_ws_status.brush.Color:=rgbtocolor(0,255,0);
      l_ws_status.Caption:='Verbunden';
      tb_ws_conn.Caption:='Verbunden';
      //update_scenes;
    end
    else begin
      //failed
      sh_ws_status.brush.Color:=rgbtocolor(255,0,0);
      l_ws_status.Caption:='Verbindungsversuch fehlgeschlagen! Nicht verbunden.';
      tb_ws_conn.Checked:=false;
    end;
  end
  else begin
    //close connection
    l_ws_status.Caption:='Trenne Verbindung...';
    status:=tc.stop_obs_comm();
    if status=0 then begin
      //closed
      l_ws_status.Caption:='Verbindung getrennt.';
      sh_ws_status.brush.Color:=rgbtocolor(255,0,0);
      tb_ws_conn.Caption:='Verbinden';
      update_gui;
    end
    else begin
      //closing failed
      l_ws_status.Caption:='Fehler: Konnte Verbindung nicht trennen!';
      sh_ws_status.brush.Color:=rgbtocolor(255,0,0);
      tb_ws_conn.Caption:='Verbunden';
      tb_ws_conn.checked:=true;
    end;
  end;
end;

procedure Tformmain.update_scenes;
var i:integer;
    s:tscene;
    t:ttally;
    tallys:tlist;
begin
  //scenes
  if tc.get_scene_count>0 then begin
    //load
    sg_scenes.RowCount:=tc.get_scene_count+1;
    for i:=1 to tc.get_scene_count do begin
        s:=tc.get_scene(i-1);
        sg_scenes.Cells[0,i]:=inttostr(i);
        sg_scenes.Cells[1,i]:=s.get_name;
        case s.get_state of
          soff:sg_scenes.Cells[2,i]:='Inaktiv';
          son:sg_scenes.Cells[2,i]:='Aktiv';
        end;
        if s.get_preview then sg_scenes.cells[3,i]:='Ausgewählt'
        else sg_scenes.cells[3,i]:='';
        tallys:=s.get_tallys;
        sg_scenes.cells[4,i]:=inttostr(tallys.Count);
    end;
  end
  else begin
      sg_scenes.RowCount:=1;
  end;
  //obs mode
  if tc.is_studiomode then sb.Panels[0].Text:='OBS Modus: Studio'
  else sb.Panels[0].Text:='OBS Modus: Einfach';

end;

procedure Tformmain.update_gui;
var msg:ansistring;
begin
  //todo
  msg:=tc.get_notification_text;
  if msg<>'' then showmessage(msg);
  //showmessage('Update Scenes');
  update_scenes;
  //showmessage('Update Tallys');
  update_tallys;
  //showmessage('Update Tallys AS');
  update_as_tallys;
end;

procedure Tformmain.update_com;
  var l:tstringlist;
begin
  l:=tstringlist.Create;
  try
     l.CommaText:=GetSerialPortNames; //aus synaser
     if l.Count<>0 then begin;
       cb_com_port.Clear;
       cb_com_port.items:=l;
       cb_com_port.itemindex:=comm_cb_index;
       //tb_com_conn.state:=cbGrayed;
       tb_com_conn.Enabled:=true;
     end
     else tb_com_conn.Enabled:=false;
  finally
     l.Destroy;
  end;
end;

procedure Tformmain.fill_color_form;
  var gc:tcolorset;
begin
  gc:=tc.get_global_colorset;
  cfront_on.buttoncolor:=rgbtocolor(gc.front_on.r,gc.front_on.g,gc.front_on.b);
  cfront_off.buttoncolor:=rgbtocolor(gc.front_off.r,gc.front_off.g,gc.front_off.b);
  cfront_sel.buttoncolor:=rgbtocolor(gc.front_sel.r,gc.front_sel.g,gc.front_sel.b);
  cfront_ident.buttoncolor:=rgbtocolor(gc.front_ident.r,gc.front_ident.g,gc.front_ident.b);
  cback_on.buttoncolor:=rgbtocolor(gc.back_on.r,gc.back_on.g,gc.back_on.b);
  cback_off.buttoncolor:=rgbtocolor(gc.back_off.r,gc.back_off.g,gc.back_off.b);
  cback_sel.buttoncolor:=rgbtocolor(gc.back_sel.r,gc.back_sel.g,gc.back_sel.b);
  cback_ident.buttoncolor:=rgbtocolor(gc.back_ident.r,gc.back_ident.g,gc.back_ident.b);
  tb_front_dimmer.Position:=gc.dimmer_front;
  tb_back_dimmer.Position:=gc.dimmer_back;
end;

procedure Tformmain.update_as_tallys;
var sel_scene:tscene;
    tally:ttally;
    tls:tlist;
    i,j,selection,last_added:integer;
begin
  //Scenes
    //showmessage('Loading Scenes');
  selection:=cb_scene.itemindex;
  if tc.get_scene_count>0 then begin
     cb_scene.Clear;
     cb_scene.Enabled:=true;
     for i:=0 to tc.get_scene_count-1 do begin;
         cb_scene.Items.Add(tc.get_scene(i).get_name);
     end;
  end
  else begin
      cb_scene.Clear;
      cb_scene.Enabled:=false;
  end;

  if selection>tc.get_scene_count-1 then selection:=cb_scene.Items.Count-1
  else cb_scene.itemindex:=selection;
  if cb_scene.itemindex<0 then cb_scene.itemindex:=0;
  //showmessage('Loading Tallys');
  if tc.get_scene_count>0 then begin
     clb_tallys.Enabled:=true;
     if tc.get_tallys_count>0 then begin
       sel_scene:=tc.get_scene_by_name(cb_scene.items[cb_scene.ItemIndex]);
       clb_tallys.ClearSelection;
       clb_tallys.Clear;
       tls:=sel_scene.get_tallys;
       last_added:=0;
       for i:=0 to tc.get_tallys_count-1 do begin
         tally:=tc.get_tally(i);
         if tally.is_online then begin
           clb_tallys.items.add('Tally Nr. '+inttostr(tally.get_dip_nr));
           if tls.count>0 then begin
             for j:=0 to tls.count-1 do begin
                 if ttally(tls.Items[j]).get_dip_nr=tally.get_dip_nr then clb_tallys.Checked[last_added]:=true
                 else clb_tallys.Checked[last_added]:=false;
             end;
           end;
           inc(last_added);
         end;
       end;
     end
     else begin
       clb_tallys.Clear;
     end;
  end
  else begin
      clb_tallys.Enabled:=false;
  end;
end;

procedure Tformmain.update_tallys;    //updates gui for tally content
var tally_count,i,row,tallys_online:integer;
    t:ttally;
begin
  //tallys
  //lb_free.Clear;
  tally_count:=tc.get_tallys_count;
  tallys_online:=tc.get_tallys_online;
  //statusbar update
  sb.Panels[1].Text:='Verbundene Tallys: '+inttostr(tallys_online);
  //gb_tally_status_update
  if tallys_online>0 then begin
    sg_tallys.RowCount:=tallys_online+1;
    row:=1;
    for i:=1 to tally_count do begin
       t:=tc.get_tally(i-1);
       if t.is_online then begin
         sg_tallys.Cells[0,row]:=inttostr(t.get_dip_nr);
         sg_tallys.cells[1,row]:=rgbtohexstr(t.get_front_color);
         sg_tallys.cells[2,row]:=rgbtohexstr(t.get_back_color);
         sg_tallys.cells[3,row]:=inttostr(t.get_device_health); //TODO: decode device_health
         inc(row);
       end;
    end;
  end
  else begin
      //clear
    sg_tallys.RowCount:=1;
  end;
end;

procedure Tformmain.update_colorset;
var gc:tcolorset;
begin
  gc.front_on:=colortotrgb(cfront_on.ButtonColor);
  gc.front_off:=colortotrgb(cfront_off.ButtonColor);
  gc.front_sel:=colortotrgb(cfront_sel.ButtonColor);
  gc.front_ident:=colortotrgb(cfront_ident.ButtonColor);
  gc.back_on:=colortotrgb(cback_on.ButtonColor);
  gc.back_off:=colortotrgb(cback_off.ButtonColor);
  gc.back_sel:=colortotrgb(cback_sel.ButtonColor);
  gc.back_ident:=colortotrgb(cback_ident.ButtonColor);
  gc.dimmer_front:=tb_front_dimmer.Position;
  gc.dimmer_back:=tb_back_dimmer.Position;
  tc.set_global_colorset(gc);
end;

procedure Tformmain.update_single_tally;
var i,item_index:integer;
    l:tlist;
    t:ttally;
begin
  l:=tc.get_online_tallys;
  item_index:=cb_single_tally.ItemIndex;
  if l.Count=0 then begin
    cb_single_tally.Clear;
    cb_single_tally.Items.Add('Kein Tally verbunden.');
    cb_single_tally.ItemIndex:=0;
  end
  else begin
     for i:=0 to l.count-1 do begin
        t:=ttally(l.Items[i]);
        cb_single_tally.Items.Add('Tally # '+inttostr(t.get_dip_nr));
     end;
     if item_index<0 then item_index:=1;
     if item_index>cb_single_tally.Items.Count then item_index:=cb_single_tally.Items.Count;
     cb_single_tally.ItemIndex:=item_index;
  end;

end;

procedure Tformmain.set_form_color;
var dark,light,tdark,tlight,fixed_dark,fixed_light:tcolor;
begin
 light:=clDefault;
 dark:=rgbtocolor(20,20,20);
 tdark:=rgbtocolor(200,200,200);
 tlight:=clblack;
 fixed_light:=clBtnFace;
 fixed_dark:=rgbtocolor(40,40,40);

 if mi_darkmode.Checked then begin
   formmain.Color:=dark;
   formmain.Font.Color:=tdark;
   sg_scenes.Color:=dark;
   sg_scenes.Font.Color:=tdark;
   sg_scenes.FixedColor:=fixed_dark;
   sg_scenes.BorderColor:=fixed_dark;
   sg_tallys.Color:=dark;
   sg_tallys.Font.Color:=tdark;
   sg_tallys.fixedcolor:=fixed_dark;
   sg_tallys.bordercolor:=fixed_dark;
   clb_tallys.Color:=dark;
   clb_tallys.Font.Color:=tdark;

   gb_scenes.font.color:=tdark;
   gb_tally_status.Font.color:=tdark;
   gb_as_tally.Font.color:=tdark;
   gb_tally_options.font.color:=tdark;
   gb_tally_colors.font.color:=tdark;
   gb_single_tally.font.color:=tdark;
   gb_obs_conn.font.color:=tdark;
   gb_master_conn.font.color:=tdark;
 end
 else begin
   formmain.Color:=light;
   formmain.Font.Color:=tlight;
   sg_scenes.color:=light;
   sg_scenes.Font.Color:=tlight;
   sg_scenes.FixedColor:=fixed_light;
   sg_scenes.BorderColor:=fixed_light;
   sg_tallys.Color:=light;
   sg_tallys.Font.Color:=tlight;
   sg_tallys.fixedcolor:=fixed_light;
   sg_tallys.bordercolor:=fixed_light;
   clb_tallys.Color:=light;
   clb_tallys.Font.Color:=tlight;
 end;;

end;

function Tformmain.rgbtohexstr(c: trgb): string;
begin
  result:='#'+inttohex(c.r,2)+inttohex(c.g,2)+inttohex(c.b,2);
end;

function Tformmain.colortotrgb(s: tcolor): trgb;
begin
  result.r:=Red(s);
  result.g:=Green(s);
  result.b:=Blue(s);
end;

function Tformmain.extract_dipnr(s: string): integer;
begin
  delete(s,1,pos('.',s)+1);
  //showmessage('#'+s);
  result:=strtoint(s);
end;

procedure Tformmain.b_ws_setupClick(Sender: TObject);
begin
  form_ws_setup.showmodal;
end;

procedure Tformmain.cback_identColorChanged(Sender: TObject);
begin
  update_colorset;
end;

procedure Tformmain.cback_offColorChanged(Sender: TObject);
begin
  update_colorset;
end;

procedure Tformmain.cback_onColorChanged(Sender: TObject);
begin
  update_colorset;
end;

procedure Tformmain.cback_selColorChanged(Sender: TObject);
begin
  update_colorset;
end;

procedure Tformmain.b_colorresetClick(Sender: TObject);
begin
  tc.reset_global_colorset;
  fill_color_form;
end;

procedure Tformmain.b_reload_tallysClick(Sender: TObject);
begin
  tc.reload_tallys;
end;

procedure Tformmain.cb_com_portChange(Sender: TObject);
begin
  serial.Device:=cb_com_port.Text;
end;

procedure Tformmain.cb_sceneEditingDone(Sender: TObject);
begin
  update_as_tallys;
end;

procedure Tformmain.cb_single_tallyEditingDone(Sender: TObject);
begin
  update_single_tally;
end;

procedure Tformmain.cfront_identColorChanged(Sender: TObject);
begin
  update_colorset;
end;

procedure Tformmain.cfront_offColorChanged(Sender: TObject);
begin
  update_colorset;
end;

procedure Tformmain.cfront_onColorChanged(Sender: TObject);
begin
  update_colorset;
end;

procedure Tformmain.cfront_selColorChanged(Sender: TObject);
begin
  update_colorset;
end;

procedure Tformmain.clb_tallysClickCheck(Sender: TObject);
var t:ttally;
    i:integer;
    s:tscene;
begin
  s:=tc.get_scene_by_name(cb_scene.Items[cb_scene.ItemIndex]);
  for i:=0 to clb_tallys.Items.count-1 do begin
      //showmessage(' @i: '+inttostr(i));
      t:=tc.get_tally_by_dipnr(extract_dipnr(clb_tallys.Items[i]));
      //showmessage('DipNr: '+inttostr(t.get_dip_nr)+' @i: '+inttostr(i));
      if clb_tallys.Checked[i] then begin
        s.add_tally(t);
        //showmessage('DipNr: '+inttostr(t.get_dip_nr)+' was added.');
      end
      else begin
        s.remove_tally(t);
        //showmessage('DipNr: '+inttostr(t.get_dip_nr)+' was removed.');
      end;
  end;
  //showmessage('done. going to update gui ...');
  update_gui;
end;


end.

