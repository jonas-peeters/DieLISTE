unit Login;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Controls.Presentation, FMX.Layouts, UMain, ServerAPI, PWVergessen;

type
  TForm5 = class(TForm)
    GridPanelLayout1: TGridPanelLayout;
    LblAnmelden: TLabel;
    LblRegistrieren: TLabel;
    EdtBenutzername1: TEdit;
    EdtPW1: TEdit;
    EdtEMail: TEdit;
    EdtBenutzername2: TEdit;
    EdtPW2: TEdit;
    BtnLos: TButton;
    BtnPWVergessen: TButton;
    BtnRegistrieren: TButton;
    TitleLabel: TLabel;
    procedure BtnLosClick(Sender: TObject);
    procedure BtnRegistrierenClick(Sender: TObject);
    procedure BtnPWVergessenClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form5: TForm5;
  serverAPI: TServerAPI;
implementation

{$R *.fmx}

procedure TForm5.BtnPWVergessenClick(Sender: TObject);
var PwVergessenForm: TForm;
begin
  PwVergessenForm:= Tform10.Create(Application,serverAPI);
  PWvergessenform.Show;
end;

procedure TForm5.BtnRegistrierenClick(Sender: TObject);
var email, name, password: string;
begin
   email:= EdtEmail.Text;
   name:= EdtBenutzername2.Text;
   password:= EdtPW2.Text;
   UMain.serverAPI.createUser(email,name, password);
end;

procedure TForm5.BtnLosClick(Sender: TObject);
var
  i: Integer;
begin
  if UMain.serverAPI.login(EdtBenutzername1.Text, EdtPW1.Text)='"OK: Authenticated"' then // Check if the user gets autheticated
  begin
    Form6.ShowModal;
  end;
end;

end.
