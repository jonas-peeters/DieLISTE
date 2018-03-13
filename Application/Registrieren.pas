{*
  Registrieren Form

  Hier können sich die User registrieren.
}
unit Registrieren;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Controls.Presentation, FMX.Layouts, UMain, Helper;

type
  TFormRegistrieren = class(TForm)
    GridPanelLayout1: TGridPanelLayout;
    LblRegistrieren: TLabel;
    EdtEMailRegister: TEdit;
    EdtBenutzernameRegister: TEdit;
    EdtPWRegister: TEdit;
    PanelRegistrieren: TPanel;
    LblRegister: TLabel;
    PanelAbbrechen: TPanel;
    LblCancel: TLabel;
    procedure LblRegisterClick(Sender: TObject);
    procedure LblCancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  //* Die Registrierungs-Form
  FormRegistrieren: TFormRegistrieren;

implementation

{$R *.fmx}

{*
  Registrieren

  Mit den angegebenen Daten wird versucht einen user zu erstellen.

  @param Sender Button um die Registrierung durchzuführen
}
procedure TFormRegistrieren.LblRegisterClick(Sender: TObject);
var email, name, password: String;
begin
  if checkForInvalidCharacters(EdtEMailRegister)
    AND checkForInvalidCharacters(EdtBenutzernameRegister)
    AND checkForInvalidCharacters(EdtPWRegister) then
  begin
    email:= EdtEmailRegister.Text;
    name:= EdtBenutzernameRegister.Text;
    password:= EdtPWRegister.Text;
    if email = '' then
      ShowMessage('Du musst eine valide E-Mail angeben!')
    else if name = '' then
      ShowMessage('Du musst einen Benutzernamen angeben!')
    else if password.Length < 6 then
      ShowMessage('Dein Passwort muss mindestens 6 Stellen haben')
    else
    begin
      if UMain.serverAPI.isOnline then
      begin
        if interpretServerResponse(UMain.serverAPI.createUser(email, name, password)) then
        begin
          ShowMessage('Vielen Dank für Ihre Regestrierung. Bitte verifizieren Sie ihre E-Mail, um die Regestrierung zu vollenden!');
          Close;
          Release;
        end;
      end
      else
        ShowMessage('Du brauchst eine aktive Internetverbindung für diese Aktion!');
    end;
  end;
end;

{*
  Abbruch

  Die Registrierung wird abgebrochen und der User zur Login-Seite
  weitergeleitet.

  @param Sender Button zum abbrechen
}
procedure TFormRegistrieren.LblCancelClick(Sender: TObject);
begin
  Close;
  Release;
end;

end.
