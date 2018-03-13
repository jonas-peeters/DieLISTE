{*
  Programm zum Erstellen und Bearbeiten von Listen
}
program DieLISTE;

uses
  System.StartUpCopy,
  FMX.Forms,
  Login in 'Login.pas' {LoginForm},
  UMain in 'UMain.pas' {MainForm},
  PWaendern in 'PWaendern.pas' {FormPWaendern},
  Liste in 'Liste.pas' {FormListe},
  Hinzufuegen in 'Hinzufuegen.pas' {FormAddItem},
  PWvergessen in 'PWvergessen.pas' {FormPWVergessen},
  ServerAPI in 'ServerAPI.pas',
  ListeBearbeiten in 'ListeBearbeiten.pas' {FormListeBearbeiten},
  AddUser in 'AddUser.pas' {FormAddUser},
  ItemBearbeiten in 'ItemBearbeiten.pas' {FormItemBearbeiten},
  Helper in 'Helper.pas',
  CCR.PrefsIniFile in 'lib\CCR.PrefsIniFile.pas',
  CCR.PrefsIniFile.Android in 'lib\CCR.PrefsIniFile.Android.pas',
  CCR.PrefsIniFile.Apple in 'lib\CCR.PrefsIniFile.Apple.pas',
  Registrieren in 'Registrieren.pas' {FormRegistrieren};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormLogin, LoginForm);
  Application.Run;

end.
