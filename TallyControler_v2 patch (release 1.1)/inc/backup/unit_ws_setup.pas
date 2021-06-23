unit unit_ws_setup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Spin,
  ButtonPanel, StdCtrls;

type

  { Tform_ws_setup }

  Tform_ws_setup = class(TForm)
    bp: TButtonPanel;
    cb_local: TCheckBox;
    cb_auth: TCheckBox;
    Port: TLabel;
    e_url: TLabeledEdit;
    e_pw: TLabeledEdit;
    se_port: TSpinEdit;
    procedure cb_authChange(Sender: TObject);
    procedure cb_localChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private

  public
    f_auth:boolean;
    ip:ansistring;
    ws_port:integer;
    pw:ansistring;
    e_url_buffer:ansistring;
  end;

var
  form_ws_setup: Tform_ws_setup;

implementation

{$R *.lfm}

{ Tform_ws_setup }

procedure Tform_ws_setup.cb_authChange(Sender: TObject);
begin
  if cb_auth.Checked then begin
    e_pw.Enabled:=true;
    f_auth:=true;
  end
  else begin
    e_pw.Enabled:=false;
    f_auth:=false;
  end;
end;

procedure Tform_ws_setup.cb_localChange(Sender: TObject);
begin
  if cb_local.Checked then begin
    e_url.Enabled:=false;
    e_url_buffer:=e_url.text;
    e_url.Text:='127.0.0.1';
  end
  else begin
    e_url.Enabled:=true;
    e_url.text:=e_url_buffer;
  end;
end;

procedure Tform_ws_setup.FormCreate(Sender: TObject);
begin
  if cb_auth.Checked then begin
    e_pw.Enabled:=true;
    f_auth:=true;
  end
  else begin
    e_pw.Enabled:=false;
    f_auth:=false;
  end;
  e_url_buffer:=e_url.text;
  if cb_local.Checked then begin
    e_url.Enabled:=false;
    e_url_buffer:=e_url.text;
    e_url.Text:='127.0.0.1';
  end
  else begin
    e_url.Enabled:=true;
    e_url.text:=e_url_buffer;
  end;
  ip:=e_url.text;
  ws_port:=se_port.value;
  pw:=e_pw.text;
end;

procedure Tform_ws_setup.OKButtonClick(Sender: TObject);
begin
  if cb_auth.Checked then begin
    e_pw.Enabled:=true;
    f_auth:=true;
  end
  else begin
    e_pw.Enabled:=false;
    f_auth:=false;
  end;
  ip:=e_url.text;
  ws_port:=se_port.value;
  pw:=e_pw.text;
end;

end.

