program DieLISTE;

uses
  System.StartUpCopy,
  FMX.Forms,
  Login in 'Login.pas' {LoginForm},
  UMain in 'UMain.pas' {Form6},
  PWaendern in 'PWaendern.pas' {Form7},
  Liste in 'Liste.pas' {Form8},
  Hinzufuegen in 'Hinzufuegen.pas' {Form9},
  PWvergessen in 'PWvergessen.pas' {Form10},
  ServerAPI in 'ServerAPI.pas',
  ListeBearbeiten in 'ListeBearbeiten.pas' {FormListeBearbeiten},
  AddUser in 'AddUser.pas' {Form1},
  ItemBearbeiten in 'ItemBearbeiten.pas' {FormItemBearbeiten},
  Helper in 'Helper.pas',
  CCR.PrefsIniFile in 'lib\CCR.PrefsIniFile.pas',
  CCR.PrefsIniFile.Android in 'lib\CCR.PrefsIniFile.Android.pas',
  CCR.PrefsIniFile.Apple in 'lib\CCR.PrefsIniFile.Apple.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormLogin, LoginForm);
  Application.Run;

end.
