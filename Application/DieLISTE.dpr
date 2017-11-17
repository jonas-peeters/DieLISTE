program DieLISTE;

uses
  System.StartUpCopy,
  FMX.Forms,
  Home in 'Home.pas' {Form1},
  SupermarktListe in 'SupermarktListe.pas' {Form2},
  Supermaerkte in 'Supermaerkte.pas' {Form3},
  Profil in 'Profil.pas' {Form4};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TForm3, Form3);
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
