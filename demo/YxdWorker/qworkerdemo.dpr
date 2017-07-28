program qworkerdemo;

uses
  YxdMapFile,
  Forms,
  main in 'main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  {$IFDEF USEINLINE}
  Application.MainFormOnTaskbar := True;
  {$ENDIF}
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
