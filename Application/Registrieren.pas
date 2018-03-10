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
    Label1: TLabel;
    PanelAbbrechen: TPanel;
    Label2: TLabel;
    procedure Label1Click(Sender: TObject);
    procedure Label2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormRegistrieren: TFormRegistrieren;

implementation

{$R *.fmx}

procedure TFormRegistrieren.Label1Click(Sender: TObject);
var email, name, password: String;
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
      UMain.serverAPI.createUser(email,name, password);
      ShowMessage('Vielen Dank für Ihre Regestrierung. Bitte verifizieren Sie ihre E-Mail, um die Regestrierung zu vollenden!');
      Close;
      Release;
    end
    else
      ShowMessage('Du brauchst eine aktive Internetverbindung für diese Aktion!');
  end;
end;

procedure TFormRegistrieren.Label2Click(Sender: TObject);
begin
  Close;
  Release;
end;

end.
