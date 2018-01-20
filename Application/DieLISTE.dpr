program DieLISTE;

uses
  System.StartUpCopy,
  FMX.Forms,
  Supermarkt in 'Supermarkt.pas' {Form2},
  Login in 'Login.pas' {Form5},
  UMain in 'UMain.pas' {Form6},
  PWaendern in 'PWaendern.pas' {Form7},
  Liste in 'Liste.pas' {Form8},
  Hinzufuegen in 'Hinzufuegen.pas' {Form9},
  PWvergessen in 'PWvergessen.pas' {Form10},
  ServerAPI in 'ServerAPI.pas';

{$R *.res}

begin
  Application.Initialize;

  Application.CreateForm(TForm5, Form5);
  Application.Run;

end.
