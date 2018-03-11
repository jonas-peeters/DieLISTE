{*
  Passwort-Ändern-Form

  Hier kann der User sein Passwort ändern.
}
unit PWaendern;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Controls.Presentation, FMX.Layouts, ServerAPI, Helper,
  FMX.Objects;

type
  TFormPWaendern = class(TForm)
    GridPanelLayout1: TGridPanelLayout;
    EdtPWneu1: TEdit;
    EdtPWneu2: TEdit;
    BtnPWaendern: TButton;
    LblPWaendern: TLabel;
    BtnCancel: TButton;
    ImgBack: TImage;
    procedure BtnPWaendernClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure ImgBackClick(Sender: TObject);
    public constructor Create(AOwner: TComponent; var serverAPI: TServerAPI);
  private
  public
    { Public-Deklarationen }
  end;

var
  //* Die Passwort-Ändern-Form
  FormPWaendern: TFormPWaendern;
  //* Private Instanz der Server API in der der User bereits authetifiziert ist.
  privateServerAPI: TServerAPI;

implementation

{$R *.fmx}


{*
  Neuer Konstruktor

  Neuer Konstruktor, dem eine Kopie der Server API aus der Main-Form übergeben
  wird, in der der User bereits angemeldet ist.

  @param AOwner Der Parent dieser Form
  @param serverAPI Instanz der server API in der der User bereits angemeldet ist.
}
constructor TFormPWaendern.Create(AOwner: TComponent; var serverAPI: TServerAPI);
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
procedure TFormPWaendern.ImgBackClick(Sender: TObject);
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
procedure TFormPWaendern.BtnCancelClick(Sender: TObject);
begin
  Close;
  Release;
end;

{*
  Passwort ändern

  Der Button zum Passwort ändern wurde gedrückt. Stimmen die angegebenen
  Passwörter überein, so wird das Passwort geändert.

  @param Sender Buttom zum Passwrt ändern
}
procedure TFormPWaendern.BtnPWaendernClick(Sender: TObject);
begin
  if EdtPWneu1.text=EdtPWneu2.text then
  begin
    if privateServerAPI.isOnline then
    begin
      if interpretServerResponse(privateServerAPI.changePassword(EdtPWneu1.Text)) then
      begin
        ShowMessage('Dein Passwort wurde geändert.');
        Close;
        Release;
      end;
    end
    else
      ShowMessage('Du brauchst eine aktive Internetverbindung für diese Aktion!');
  end
  else
    ShowMessage('Die Passwörter stimen nicht überein!');
end;

end.
