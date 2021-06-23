unit myjson;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

{ tmyjsonnode }

 tmyjsonnode=class   //
  private
    jname:ansistring;
    nodetype:string;   //string,integer,float,array,object
    jnodes:tlist;      //contains other nodes
    jvalue:ansistring;
    function extract_nodestr(str:ansistring):ansistring;
    function substract_str(astr,bstr:ansistring):ansistring;
  public
    constructor create();
    procedure parse(_jsonstr:ansistring);
    procedure set_name(_name:ansistring);
    procedure set_value(_value:ansistring);
    procedure set_type(_type:string);
    procedure parse_array_str(str:ansistring);
    function get_nodetype:string;
    function get_count:integer;
    function get_item(_index:integer):tmyjsonnode; //for arrays or objects
    function get_value:ansistring;
    function get_name:ansistring;

end;

type

{ tmyjsonobj }

 tmyjsonobj=class    //container for an json document
  private
    nodes:tlist;
  public
    constructor create(); //create an empty json document
    function parse(_str:ansistring):integer; //parse a ansistring to an myjsonobject  //returns 0 if succesfull
    function get(_name:string):tmyjsonnode; //get the jsonnode with the _name
end;

implementation

{ tmyjsonnode }

function tmyjsonnode.extract_nodestr(str: ansistring): ansistring;
var i,open,close:integer;
    s:ansistring;
    c:char;
begin
  open:=0;
  close:=0;
  s:='';
  for i:=1 to length(str) do begin
    c:=str[i];
    if c='{' then inc(open);
    else if c='}' then inc(close);
    if open > close then s:=s+c;
  end;
  result:=str;
end;

function tmyjsonnode.substract_str(astr, bstr: ansistring): ansistring;  // bstr has to be shorter or equal length to astr
begin
  try
    delete(astr,1,length(bstr));
    result:=astr;
  except
    result:='';
  end;
end;

constructor tmyjsonnode.create();
begin
  jnodes:tlist.create;;
end;

procedure tmyjsonnode.parse(_jsonstr: ansistring);
var lvl,fieldstart,dpcount:integer;
    c:char;
    nodestr:ansistring;
    action:string;
    field:tmyjsonnode;
    setfull:integer; //indicates if a name:value set is complete (0=not startet, 1=name, 2=value, 3=complete)
begin
  jnodes:tlist.create();
  fieldstart:=0;
  dpcount:=0;
  action:='none';
  setfull:=0;
  //lvl:=0;                   //{ = inc lvl
                              //} = dec lvl
                              //" first = field start
                              //" second = field end
                              // : = next Field is a Value
                              //[ = array start
                              //] = array end
  p:=1;                            // , = next field
  while length(_jsonstr)>0 do begin
     c:=_jsonstr[p]
     case c of
     '{':begin
           inc(lvl);
           nodestr:=extract_nodestr(copy(_jsonstr,p+1,length(_jsonstr)-(p+1)));
           jnodes.add(tmyjsonnode.create(nodestr));
           _jsonstr:=substract_str(_jsonstr,nodestr);
           p:=1;
         end;
     '"':begin
           case setfull of
           0:begin
             setfull:=1;
             field:=tmyjsonnode.create();
             end;
           1:begin
             //name
             delete(_jsonstr,1,1);
             field.set_name(copy(_jsonstr,1,pos(_jsonstr,'"')-1));
             end;
           2:begin
             //value
             delete(_jsonstr,1,1);
             field.set_value(copy(_jsonstr,1,pos(_jsonstr,'"')-1));
             end;
           3:begin
             jnodes.Add(field);
             setfull:=0;
             end;
           end;
         end;
     ':':begin
           setfull:=2;
         end;
     '[':begin
           //Array open
           field.set_type('array');
           case setfull of
           0:begin
             setfull:=1;
             field:=tmyjsonnode.create();
             end;
           1:begin
             //name
             delete(_jsonstr,1,1);
             field.set_name(copy(_jsonstr,1,pos(_jsonstr,'"')-1));
             end;
           2:begin
             //value
             delete(_jsonstr,1,1);
             field.set_value(copy(_jsonstr,1,pos(_jsonstr,'"')-1));
             end;
           3:begin
             jnodes.Add(field);
             setfull:=0;
             end;
           end;
         end;
     ']':begin

     end;
     ',':begin

     end;
    end;
   end;
  end;
end;

procedure tmyjsonnode.set_name(_name: ansistring);
begin
  jname:=_name;
end;

procedure tmyjsonnode.set_value(_value: ansistring);
begin
  nodetype:='string';
  jvalue:=_value;
end;

function tmyjsonnode.get_nodetype: string;
begin
  result:=nodetype;
end;

function tmyjsonnode.get_count: integer;
begin
  if (nodetype='array') or (nodetype='object') then result:=tlist.count
  else:=0;
end;

function tmyjsonnode.get_item(_index: integer): tmyjsonnode;
begin
   if _index<=jarray.count-1 then result:=tmyjsonnode(jarray.Items[_index];
   else result:=self;
end;

function tmyjsonnode.get_value: ansistring;
begin
   if (nodetype='array') or (nodetype='object') then result:=''
   else:=jvalue;
end;

function tmyjsonnode.get_name: ansistring;
begin
    result:=name;
end;

{ tmyjsonobj }

constructor tmyjsonobj.create();
begin
  nodes:=tlist.create;
end;

function tmyjsonobj.parse(_str: ansistring): integer;
begin

end;

function tmyjsonobj.get(_name: string): tmyjsonnode;
var i:integer;
begin
  if nodes.Count=0 then result:=tmyjsonnode.create('{ "NULL":"NULL"}')
  else begin
    for i:=0 to nodes.count-1 do begin
      if tmyjsonnode(nodes.items[i]).get_name=_name then result:=tmyjsonnode(nodes.items[i]);
    end;
  end;
end;

end.

