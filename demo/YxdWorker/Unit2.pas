unit Unit2;

interface

uses
  YxdWorker,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TForm2 = class(TForm)
    BitBtn1: TBitBtn;
    procedure BitBtn1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure DoTest(AJob: PJob);
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.BitBtn1Click(Sender: TObject);
begin
  Workers.Post(dotest, NewExData('fdafdsafd'));
end;

procedure TForm2.DoTest(AJob: PJob);
begin
  ShowMessage(ajob.ExtData.AsString);
end;

end.
