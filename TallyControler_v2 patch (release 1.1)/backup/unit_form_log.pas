unit unit_form_log;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { Tform_log }

  Tform_log = class(TForm)
    e_command: TEdit;
    m_log: TMemo;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  form_log: Tform_log;

implementation

{$R *.lfm}

{ Tform_log }


procedure Tform_log.FormCreate(Sender: TObject);
begin
  form_log.left:=0;
  form_log.top:=0;
end;

end.

