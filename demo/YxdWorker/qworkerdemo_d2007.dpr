program qworkerdemo_d2007;

uses
  Forms,
  YxdMapFile,
  Unit2 in 'Unit2.pas' {Form2},
  main in 'main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
