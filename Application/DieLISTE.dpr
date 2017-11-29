program DieLISTE;

uses
  System.StartUpCopy,
  FMX.Forms,
  Home in 'Home.pas' {Form1},
  SupermarktListe in 'SupermarktListe.pas' {Form2},
  Supermaerkte in 'Supermaerkte.pas' {Form3},
  Profil in 'Profil.pas' {Form4},
  Login in 'Login.pas' {Form5},
  UMain in 'UMain.pas' {Form6},
  ServerAPI in 'ServerAPI.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm6, Form6);
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TForm3, Form3);
  Application.CreateForm(TForm4, Form4);
  Application.CreateForm(TForm5, Form5);
  Application.Run;
end.
