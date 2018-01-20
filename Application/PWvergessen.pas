unit PWvergessen;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Controls.Presentation, FMX.Layouts, ServerAPI;

type
  TForm10 = class(TForm)
    GridPanelLayout1: TGridPanelLayout;
    LblText: TLabel;
    EdtEMail: TEdit;
    BtnSenden: TButton;
    LblPWvergessen: TLabel;
    Panel1: TPanel;
    BtnCancel: TButton;
    procedure BtnSendenClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    public constructor Create(AOwner: TComponent; var serverAPI: TServerAPI);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form10: TForm10;
  privateServerAPI: TServerAPI;

implementation

{$R *.fmx}

constructor TForm10.Create(AOwner: TComponent; var serverAPI: TServerAPI);
begin
  inherited Create(AOwner);
  privateServerAPI := serverAPI;
end;

procedure TForm10.BtnCancelClick(Sender: TObject);
begin
  Release;
end;

procedure TForm10.BtnSendenClick(Sender: TObject);
begin
  if privateServerAPI.forgotPassword(EdtEMail.Text)= '"EMail sent"' then
  begin
    ShowMessage('Die EMail wurde versendet. Sie können dort ihr Passwort neu erstellen.');
    Form10.Free;
  end;
end;

end.
