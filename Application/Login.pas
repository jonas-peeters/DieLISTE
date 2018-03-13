{*
  Login-Form

  Dies ist die erste Seite, die jeder User sehen wird.
  Hier kann der User sich anmelden, registrieren und die Passwort-Vergessen-Seite
  öffnen.

  Die Anmeldedaten des Users werden automatische gespeichert, sodass er/sie beim
  nächsten mal automatisch angemeldet wird.
}
unit Login;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Controls.Presentation, FMX.Layouts, UMain, ServerAPI, PWVergessen,
  Helper, FMX.Ani, Registrieren;

type
  TFormLogin = class(TForm)
    GridPanelLayout1: TGridPanelLayout;
    LblAnmelden: TLabel;
    EdtEMailLogin: TEdit;
    EdtPWLogin: TEdit;
    LabelPWVergessen: TLabel;
    PanelLogin: TPanel;
    LabelLogin: TLabel;
    PanelRegistrieren: TPanel;
    Label1: TLabel;
    procedure BtnRegistrierenClick(Sender: TObject);
    procedure BtnPWVergessenClick(Sender: TObject);
    procedure login(email: String; password: String);
    procedure BtnLosClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure subFormClosed(Sender: TObject; var Action: TCloseAction);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  //* Die Login-Form
  LoginForm: TFormLogin;
  //* Die von hier aufgerufene Haupt-Form
  MainForm: TFormMain;

implementation

{$R *.fmx}

{*
  Anmelden des Users

  Meldet den User beim Server an und, wenn keine Serververbindung besteht,
  benutzt die Offline gespechicherten Daten, um zu überprüfen, ob der User
  bereits angemeldet gewesen war. In diesem Fall wird der 'Offline-Modus'
  gestartet.

  @param email EMail des Users
  @param password Passwort des Users
}
procedure TFormLogin.login(email: String; password: String);
var offlineData: TOfflineData;
begin
  if UMain.serverAPI.isValidOnline then
  begin
    if interpretServerResponse(UMain.serverAPI.login(email, password)) then
    begin
      offlineData.email := email;
      offlineData.password := password;
      offlineData.worked := true;
      offlineData.lists := UMain.serverAPI.getListString;
      saveOfflineData(offlineData);
      MainForm.Show;
      Hide;
      MainForm.OnClose := subFormClosed;
      MainForm.UpdateLists();
      MainForm.UpdateUserData();
      MainForm.Timer.Enabled := true;
    end;
  end
  else if getOfflineData.worked then
  begin
    MainForm.Show;
    Hide;
    MainForm.OnClose := subFormClosed;
    MainForm.UpdateLists();
    MainForm.UpdateUserData();
    ShowMessage('Du wurdest offline angemeldet. Bis wieder eine Internetverbindung besteht, können keine Änderungen vorgenommen werden!')
  end
  else
    ShowMessage('Du brauchst eine aktive Internetverbindung für diese Aktion!');
end;

{*
  Startet die Anmeldung

  Der Button, um sich mit den angegebenen Daten anzumelden, wurde gedrückt.

  @param Sender Button zum anmelden
}
procedure TFormLogin.BtnLosClick(Sender: TObject);
begin
  if checkForInvalidCharacters(EdtEMailLogin) AND checkForInvalidCharacters(EdtPWLogin) then
    login(EdtEMailLogin.Text, EdtPWLogin.Text);
end;

{*
  Passwort vergessen

  Die Passwort-Vergessen-Form wird geöffnet

  @param Sender Button für die Passwort vergesen Funktion
}
procedure TFormLogin.BtnPWVergessenClick(Sender: TObject);
var PwVergessenForm: TForm;
begin
  PwVergessenForm:= TFormPWVergessen.Create(nil, UMain.serverAPI);
  PwVergessenForm.Show;
end;

{*
  Registrieren

  Das Registrierungsformular wird geöffnet

  @param Sender Button fürs Registrieren
}
procedure TFormLogin.BtnRegistrierenClick(Sender: TObject);
var registerForm: TForm;
begin
  registerForm := TFormRegistrieren.Create(Application);
  registerForm.Show;
end;


{*
  Startpunkt des Programms

  Die Mainform wird bereits im Hintergrund erstellt, um die Server API zu
  initialisieren.

  Wenn die offline gespeicherten Daten beim letzten mal funktioniert haben,
  wird außerdem automatisch ein Anmeldeversuch durchgeführt.

  @param Sender Die Login Form
}
procedure TFormLogin.FormShow(Sender: TObject);
var
  offlineData: TOfflineData;
begin
  MainForm := TFormMain.Create(nil);
  offlineData := getOfflineData;
  EdtEMailLogin.Text := offlineData.email;
  EdtPWLogin.Text := offlineData.password;
  if offlineData.worked then
  begin
    login(offlineData.email, offlineData.password);
  end;
end;

{*
  Unterform schließt sich

  Wird eine von hier aufgerufene Form geschlossen, so wird davon ausgegangen,
  dass der User sich abgemeldet hat. Daher werden die Anmeldedaten entfernt.

  @param Sender Die Unterform
  @param Action Schließaktion der Unterform
}
procedure TFormLogin.subFormClosed(Sender: TObject; var Action: TCloseAction);
begin
  EdtEMailLogin.Text := '';
  EdtPWLogin.Text := '';
  {$IF defined(MSWINDOWS)}
    Close;
    Release;
  {$ELSE}
    Show;
  {$ENDIF}
end;

end.
