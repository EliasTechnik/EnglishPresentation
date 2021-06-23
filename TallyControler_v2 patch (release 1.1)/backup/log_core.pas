unit log_core;
// Version 1.3 //with commands
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, LCLType;

type TprocNoArgs = procedure of object;

type

 { tcommand }

 tcommand=class
  private
   identifier:string;
   action:tprocnoargs;
   helptext:string;
  public
   constructor create(_identifier:string;_action:tprocnoargs;_helptext:string);
   procedure trigger;
   function get_identifier:string;
   function get_helptext:string;
 end;

{ log }

type

 { tlog }

 tlog=class
    private
     logobj:tmemo;
     id:integer;
     lnr:integer;
     line_nr_enable:boolean;
     vlog:tstringlist;
     output:boolean;   //toggels the output to logobj  //false at init
     procedure write_log(s:string);
     procedure sync_logobj;
    public
     constructor create(_id:integer);
     procedure assign_logobj(_object:tmemo);
     procedure print(s:string);
     procedure inp(s:string);
     procedure outp(s:string);
     procedure enable_line_nr;
     procedure disable_line_nr;
     procedure enable_output; //turns the output to logobj on and sync the logbuffer to the object
     procedure disable_output; //turns the output in logobj off. Messages will be buffered.
     procedure clear;
end;

type

 { tcommandline }

 tcommandline=class
  private
   input:tedit;
   output:tlog;
   commands:tlist;
   history:tstringlist;
   history_pos:integer;
   current_input:string;
   procedure key_down_handler(Sender: TObject; var Key: Word;Shift: TShiftState);
   function get_command(_identifier:string):tcommand;
   procedure decode_command(txt:string);
   procedure display_help;
  public
   constructor create(_output:tlog);
   procedure assign_input(_input:tedit);
   function add_simple_command(_identifier:string;_action:tprocnoargs;_helptext:string):boolean;
 end;

implementation

{ tcommandline }

procedure tcommandline.key_down_handler(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case key of
    VK_UP:begin
      if history_pos=history.count then begin
        current_input:=input.Text;
      end;
      if history_pos>0 then begin
        history_pos:=history_pos-1;
        input.Text:=history.Strings[history_pos];
      end;
    end;
    VK_DOWN:begin
      if history_pos=history.count-1 then begin
        history_pos:=history_pos+1;
        input.text:=current_input;
      end;
      if history_pos<history.Count-1 then begin
        history_pos:=history_pos+1;
        input.Text:=history.Strings[history_pos];
      end;
    end;
    VK_RETURN:begin
      history.Add(input.Text);
      history_pos:=history.Count;
      current_input:='';
      decode_command(input.text);
      input.text:='';
    end;
  end;
end;

function tcommandline.get_command(_identifier: string): tcommand;
var i:integer;
begin
  result:=nil;
  for i:=0 to commands.count-1 do begin
    if tcommand(commands.Items[i]).get_identifier=_identifier then result:= tcommand(commands.items[i]);
  end;
end;

procedure tcommandline.decode_command(txt: string);
var c:tcommand;
    identifier,arg:string;
    arguments:tstringlist;
    sr:tstringarray;
    i:integer;
begin
  arguments:=tstringlist.Create;
  sr:=txt.Split(' ');
  identifier:=sr[0];
  if length(sr)>1 then begin
    for i:=1 to length(sr)-1 do begin
      arg:=sr[i];
      if arg<>'' then begin
        if arg<>' ' then begin
          output.inp('found arg: '+arg);
          arguments.add(arg);
        end;
      end;
    end;
  end;

  {
  if pos(' ',txt)=0 then begin
    identifier:=txt;
  end
  else begin
    identifier:=copy(txt,1,pos(' ',txt)-1);
    delete(txt,1,pos(' ',txt));
    repeat
      if pos(' ',txt)=0 then begin
        if length(txt)>0 then arg:=txt;
        txt:='';
      end
      else begin
        arg:=copy(txt,1,pos(' ',txt)-1);
        output.inp('found arg: '+arg);
        if arg<>'' then begin
          if arg<>' ' then arguments.Add(arg);
        end;
        delete(txt,1,pos(' ',txt));
      end;
    until length(txt)=0;
  end;
  }
  c:=get_command(identifier);
  //output.inp('understood: '+identifier);
  //output.inp('found: '+c.get_identifier);
  if c<>nil then begin
    if arguments.count>0 then begin
      output.inp('understood argument: '+arguments.strings[0]);
      if arguments.strings[0]='help' then output.inp('Help for '''+identifier+''': '+c.get_helptext)
    end
    else begin
       //output.inp('triggered: '+c.get_identifier);
       c.trigger;
    end;
  end
  else begin
    output.inp('ERROR: command '''+identifier+''' not found. ');
  end;
end;

procedure tcommandline.display_help;
var i:integer;
begin
  output.inp('There are the following commands available: ');
  for i:=0 to commands.count-1 do begin
    output.inp('   - '+tcommand(commands.items[i]).get_identifier+': //'+tcommand(commands.items[i]).get_helptext);
  end;

end;

constructor tcommandline.create(_output: tlog);
begin
  commands:=tlist.create;
  history:=tstringlist.create;
  history.Add('');
  history_pos:=0;
  current_input:='';
  output:=_output;
  add_simple_command('help',@display_help,'Displays all commands that can be issued from user input');
end;

procedure tcommandline.assign_input(_input: tedit);
begin
  input:=_input;
  input.OnKeyDown:=@key_down_handler;
end;

function tcommandline.add_simple_command(_identifier: string;
  _action: tprocnoargs;_helptext:string): boolean;
var c:tcommand;
begin
  c:=tcommand.create(_identifier,_action,_helptext);
  if get_command(_identifier)=nil then begin
     commands.Add(c);
     result:=true;
  end
  else begin
    result:=false;
  end;
end;

{ tcommand }

constructor tcommand.create(_identifier: string; _action: tprocnoargs;
  _helptext: string);
begin
  identifier:=_identifier;
  action:=_action;
  helptext:=_helptext;
end;

procedure tcommand.trigger;
begin
  action;
end;

function tcommand.get_identifier: string;
begin
  result:=identifier;
end;

function tcommand.get_helptext: string;
begin
  result:=helptext;
end;

{ log }

procedure tlog.write_log(s: string);
begin
  //writes to stringlist and sync logobj
  vlog.Add(s);
  if output then begin
    try
       logobj.Lines.Add(s);
    except
      self.disable_output;
    end;
  end;
end;

procedure tlog.sync_logobj;
var i:integer;
begin
  try
   logobj.Clear;
   if vlog.Count>0 then begin;
     for i:=0 to vlog.count-1 do begin
       logobj.Lines.add(vlog.Strings[i]);
     end;
   end;
  except
    self.disable_output;
  end;
end;

constructor tlog.create(_id: integer);
begin
  id:=_id;
  line_nr_enable:=true;
  lnr:=0;
  vlog:=tstringlist.Create;
  output:=false;
end;

procedure tlog.assign_logobj(_object: tmemo);
begin
  logobj:=_object;
end;

procedure tlog.print(s: string);
var o:string;
begin
  lnr:=lnr+1;
  o:='';
  if line_nr_enable then begin
    o:=inttostr(lnr)+': ';
  end;
  o:=o+s;
  self.write_log(o);
end;

procedure tlog.inp(s: string);
var o:string;
begin
  lnr:=lnr+1;
  o:='';
  if line_nr_enable then begin
    o:=inttostr(lnr)+': ';
  end;
  o:=o+'>> '+s;
  self.write_log(o);
end;

procedure tlog.outp(s: string);
var o:string;
begin
  lnr:=lnr+1;
  o:='';
  if line_nr_enable then begin
    o:=inttostr(lnr)+': ';
  end;
  o:=o+'<< '+s;
  self.write_log(o);
end;

procedure tlog.enable_line_nr;
begin
  line_nr_enable:=true;
end;

procedure tlog.disable_line_nr;
begin
  line_nr_enable:=false;
end;

procedure tlog.enable_output;
begin
  output:=true;
  self.sync_logobj;
end;

procedure tlog.disable_output;
begin
  output:=false;
end;

procedure tlog.clear;
begin
  vlog.Clear;
  if assigned(logobj) then logobj.Clear;
  lnr:=0;
end;

end.

