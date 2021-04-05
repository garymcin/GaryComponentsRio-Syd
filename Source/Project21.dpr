program Project21;

uses
  Vcl.Forms,
  Unit22 in 'Unit22.pas' {Form22},
  GEMTrafficLight in 'GEMTrafficLight.pas',
  Unit24 in 'Unit24.pas' {Form24};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm22, Form22);
  Application.CreateForm(TForm24, Form24);
  Application.Run;
end.
