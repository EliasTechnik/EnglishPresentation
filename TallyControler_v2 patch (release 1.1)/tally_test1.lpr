program tally_test1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, unitmain, log_core, LazSerialPort, unit_ws_setup, ws_handler_unit,
  JsonTools, unit_tally, laz_synapse, unit_form_log, unit_form_benchmark;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(Tformmain, formmain);
  Application.CreateForm(Tform_ws_setup, form_ws_setup);
  Application.CreateForm(Tform_log, form_log);
  Application.CreateForm(Tform_benchmark, form_benchmark);
  Application.Run;
end.

