{*
  Login form

  This is the first from any user will see.
  The user can log in, register a new account and open the forgot password page.

  The login data of the users are saved, so that they will automatically be
  logged in the next time.
}
unit Login;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Controls.Presentation, FMX.Layouts, UMain, ServerAPI, PWVergessen,
  Helper;

type
  TFormLogin = class(TForm)
    GridPanelLayout1: TGridPanelLayout;
    LblAnmelden: TLabel;
    LblRegistrieren: TLabel;
    EdtBenutzername1: TEdit;
    EdtPW1: TEdit;
    EdtEMail: TEdit;
    EdtBenutzername2: TEdit;
    EdtPW2: TEdit;
    BtnLos: TButton;
    BtnRegistrieren: TButton;
    procedure BtnRegistrierenClick(Sender: TObject);
    procedure BtnPWVergessenClick(Sender: TObject);
    procedure login(email: String; password: String);
    procedure BtnLosClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  LoginForm: TFormLogin;
  MainForm: TFormMain;

implementation

{$R *.fmx}

procedure TFormLogin.login(email: String; password: String);
var loginData: TLoginData;
begin
  if interpretServerResponse(UMain.serverAPI.login(email, password)) then
  begin
    loginData.email := email;
    loginData.password := password;
    loginData.worked := true;
    saveLoginData(loginData);
    MainForm.Show;
    MainForm.UpdateLists();
  end;
end;

procedure TFormLogin.BtnLosClick(Sender: TObject);
begin
  login(EdtBenutzername1.Text, EdtPW1.Text);
end;

procedure TFormLogin.BtnPWVergessenClick(Sender: TObject);
var PwVergessenForm: TForm;
begin
  PwVergessenForm:= TFormPWVergessen.Create(nil, UMain.serverAPI);
  PwVergessenForm.Show;
end;

procedure TFormLogin.BtnRegistrierenClick(Sender: TObject);
var email, name, password: string;
begin
   email:= EdtEmail.Text;
   name:= EdtBenutzername2.Text;
   password:= EdtPW2.Text;
   UMain.serverAPI.createUser(email,name, password);
end;

procedure TFormLogin.FormShow(Sender: TObject);
var
  loginData: TLoginData;
begin
  MainForm := TFormMain.Create(nil);
  MainForm.Hide;
  loginData := getLoginData;
  EdtBenutzername1.Text := loginData.email;
  EdtPW1.Text := loginData.password;
  if loginData.worked then
    login(loginData.email, loginData.password);
end;

end.
