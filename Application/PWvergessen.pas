{*
  The user can request a password reset
}
unit PWvergessen;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Controls.Presentation, FMX.Layouts, ServerAPI, Helper;

type
  TFormPWvergessen = class(TForm)
    GridPanelLayout1: TGridPanelLayout;
    LblText: TLabel;
    EdtEMail: TEdit;
    BtnSenden: TButton;
    LblPWvergessen: TLabel;
    BtnCancel: TButton;
    BtnBack: TButton;
    procedure BtnSendenClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnBackClick(Sender: TObject);
    public constructor Create(AOwner: TComponent; var serverAPI: TServerAPI);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FormPWvergessen: TFormPWvergessen;
  privateServerAPI: TServerAPI;

implementation

{$R *.fmx}

constructor TFormPWvergessen.Create(AOwner: TComponent; var serverAPI: TServerAPI);
begin
  inherited Create(AOwner);
  privateServerAPI := serverAPI;
end;

procedure TFormPWvergessen.BtnBackClick(Sender: TObject);
begin
  Release;
end;

procedure TFormPWvergessen.BtnCancelClick(Sender: TObject);
begin
  Release;
end;

procedure TFormPWvergessen.BtnSendenClick(Sender: TObject);
begin
  if interpretServerResponse(privateServerAPI.forgotPassword(EdtEMail.Text)) then
  begin
    ShowMessage('Die E-Mail wurde versendet. Sie können dort ihr Passwort neu erstellen.');
    Release;
  end;
end;

end.
