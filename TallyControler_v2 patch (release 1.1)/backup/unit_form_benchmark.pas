unit unit_form_benchmark;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Spin, unit_tally;

type

  { Tform_benchmark }

  Tform_benchmark = class(TForm)
    bt_set_intervall: TButton;
    cb_enable_benchmark: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    rb_blink: TRadioButton;
    rb_stop: TRadioButton;
    bt: TTimer;
    se_intervall: TSpinEdit;
    procedure btTimer(Sender: TObject);
    procedure bt_set_intervallClick(Sender: TObject);
    procedure cb_enable_benchmarkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public
    blink_state:boolean;
  end;

var
  form_benchmark: Tform_benchmark;

implementation

{$R *.lfm}

uses unitmain;

{ Tform_benchmark }

procedure Tform_benchmark.FormCreate(Sender: TObject);
begin
  blink_state:=false;
end;

procedure Tform_benchmark.cb_enable_benchmarkClick(Sender: TObject);
begin
  if cb_enable_benchmark.Checked then begin
    bt.Enabled:=true;
  end
  else begin
    bt.enabled:=false
  end;
end;

procedure Tform_benchmark.btTimer(Sender: TObject);
var i:integer;
    t:ttally;
    c:trgb;
begin
  if rb_blink.Checked then begin
    if formmain.tc.get_tallys_count>0 then begin
      if blink_state=true then begin
         blink_state:=false;
         c.r:=255;
         c.g:=255;
         c.b:=255;
      end
      else begin
         blink_state:=true;
         c.r:=0;
         c.g:=0;
         c.b:=0;
      end;
       for i:=0 to formmain.tc.get_tallys_count-1 do begin
          t:=formmain.tc.get_tally(i);
          t.set_front_color(c);
          t.set_back_color(c);
       end;
       formmain.tc.prime_tallys;
    end;
  end;
  if rb_stop.Checked then begin
     c.r:=0;
     c.g:=0;
     c.b:=0;
     for i:=0 to formmain.tc.get_tallys_count-1 do begin
          t:=formmain.tc.get_tally(i);
          t.set_front_color(c);
          t.set_back_color(c);
     end;
     formmain.tc.prime_tallys;
  end;
end;

procedure Tform_benchmark.bt_set_intervallClick(Sender: TObject);
begin
  if bt.Enabled then begin
    bt.Enabled:=false;
    bt.Interval:=se_intervall.Value;
    bt.Enabled:=true;
  end
  else bt.Interval:=se_intervall.Value;
end;

end.

