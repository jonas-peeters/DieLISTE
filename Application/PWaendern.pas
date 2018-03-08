{*
  The user can change their password
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
    EdtPWalt: TEdit;
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
  FormPWaendern: TFormPWaendern;
  privateServerAPI: TServerAPI;

implementation

{$R *.fmx}

constructor TFormPWaendern.Create(AOwner: TComponent; var serverAPI: TServerAPI);
begin
  inherited Create(AOwner);
  privateServerAPI := serverAPI;
end;

procedure TFormPWaendern.ImgBackClick(Sender: TObject);
begin
  Release;
end;

procedure TFormPWaendern.BtnCancelClick(Sender: TObject);
begin
  Release;
end;

procedure TFormPWaendern.BtnPWaendernClick(Sender: TObject);
begin
if EdtPWneu1.text=EdtPWneu2.text then
  begin

  if interpretServerResponse(privateServerAPI.changePassword(EdtPWneu1.Text)) then
    begin
      ShowMessage('Ihr Passwort wurde geändert.');
      Release;
    end;
  end;
end;

end.
