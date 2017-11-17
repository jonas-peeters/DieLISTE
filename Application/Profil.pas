unit Profil;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.ListBox, FMX.StdCtrls, FMX.Controls.Presentation;

type
  TForm4 = class(TForm)
    GridPanelLayout1: TGridPanelLayout;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Button4: TButton;
    ListBox1: TListBox;
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form4: TForm4;

implementation

{$R *.fmx}

uses Home, SupermarktListe;

procedure TForm4.Button2Click(Sender: TObject);
begin
  Profil.Form4.Visible := false;
  Home.Form1.Visible := true;
end;


procedure TForm4.Button3Click(Sender: TObject);
begin
  Profil.Form4.Visible := false;
  SupermarktListe.Form2.Visible := true;
end;

end.
