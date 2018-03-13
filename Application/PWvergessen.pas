{*
  Passwort-Vergessen-Form

  Hier kann der User ein neues Passwort anfordern.
}
unit PWvergessen;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Controls.Presentation, FMX.Layouts, ServerAPI, Helper,
  FMX.Objects;

type
  TFormPWvergessen = class(TForm)
    GridPanelLayout1: TGridPanelLayout;
    LblText: TLabel;
    EdtEMail: TEdit;
    BtnSenden: TButton;
    LblPWvergessen: TLabel;
    BtnCancel: TButton;
    ImgBack: TImage;
    procedure BtnSendenClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure ImgBackClick(Sender: TObject);
    constructor Create(AOwner: TComponent; var serverAPI: TServerAPI);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  //* Die Passwort-Vergessen-Form
  FormPWvergessen: TFormPWvergessen;
  //* Private Instanz der Server API
  privateServerAPI: TServerAPI;

implementation

{$R *.fmx}

{*
  Neuer Konstruktor

  Neuer Konstruktor, um die Server API zu initialisieren.

  @param AOwner Der Parent dieses Objekts
  @param serverAPI Instanz der Server API, um mit dem Server zu kommunizieren
}
constructor TFormPWvergessen.Create(AOwner: TComponent; var serverAPI: TServerAPI);
begin
  inherited Create(AOwner);
  privateServerAPI := serverAPI;
end;

{*
  Abbrechen

  Der Button zum Abbrechen wurde gedrückt. Der User wird auf die Login-Form
  weitergeleitet.

  @param Sender Der gedrückte Button
}
procedure TFormPWvergessen.ImgBackClick(Sender: TObject);
begin
  Close;
  Release;
end;

{*
  Abbrechen

  Der Button zum Abbrechen wurde gedrückt. Der User wird auf die Login-Form
  weitergeleitet.

  @param Sender Der gedrückte Button
}
procedure TFormPWvergessen.BtnCancelClick(Sender: TObject);
begin
  Close;
  Release;
end;

{*
  EMail senden

  Es werden Instruktionen für ein neues Passwort angefordert. Wenn die
  angegebene EMail registriert ist, so wird an diese eine Anleitung zum
  zurücksetzen gesendet.

  @param Sender Button zum senden
}
procedure TFormPWvergessen.BtnSendenClick(Sender: TObject);
begin
  if checkForInvalidCharacters(EdtEMail) then
  begin
    if privateServerAPI.isOnline then
    begin
      if interpretServerResponse(privateServerAPI.forgotPassword(EdtEMail.Text)) then
      begin
        ShowMessage('Die E-Mail wurde versendet. Sie können dort ihr Passwort neu erstellen.');
        Close;
        Release;
      end;
    end
    else
      ShowMessage('Du brauchst eine aktive Internetverbindung für diese Aktion!');
  end;
end;

end.
