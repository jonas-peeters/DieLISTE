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
  LoginForm: TFormLogin;
  MainForm: TFormMain;

implementation

{$R *.fmx}

procedure TFormLogin.login(email: String; password: String);
var loginData: TLoginData;
begin
  if UMain.serverAPI.isOnline then
  begin
    if interpretServerResponse(UMain.serverAPI.login(email, password)) then
    begin
      loginData.email := email;
      loginData.password := password;
      loginData.worked := true;
      saveLoginData(loginData);
      MainForm.Show;
      MainForm.OnClose := subFormClosed;
      MainForm.UpdateLists();
      MainForm.UpdateUserData();
    end;
  end
  else
    ShowMessage('Du brauchst eine aktive Internetverbindung für diese Aktion!');
end;

procedure TFormLogin.BtnLosClick(Sender: TObject);
begin
  login(EdtEMailLogin.Text, EdtPWLogin.Text);
end;

procedure TFormLogin.BtnPWVergessenClick(Sender: TObject);
var PwVergessenForm: TForm;
begin
  PwVergessenForm:= TFormPWVergessen.Create(nil, UMain.serverAPI);
  PwVergessenForm.Show;
end;

procedure TFormLogin.BtnRegistrierenClick(Sender: TObject);
var registerForm: TForm;
begin
  registerForm := TFormRegistrieren.Create(Application);
  registerForm.Show;
end;

procedure TFormLogin.FormShow(Sender: TObject);
var
  loginData: TLoginData;
  online: Boolean;
begin
  MainForm := TFormMain.Create(nil);
  MainForm.Hide;
  loginData := getLoginData;
  EdtEMailLogin.Text := loginData.email;
  EdtPWLogin.Text := loginData.password;
  if loginData.worked then
  begin
    login(loginData.email, loginData.password);
  end;
end;

procedure TFormLogin.subFormClosed(Sender: TObject; var Action: TCloseAction);
begin
  EdtEMailLogin.Text := '';
  EdtPWLogin.Text := '';
end;

end.
