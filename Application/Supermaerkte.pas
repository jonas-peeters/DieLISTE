unit Supermaerkte;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Layouts, FMX.ListBox, FMX.StdCtrls, FMX.Controls.Presentation;

type
  TForm3 = class(TForm)
    GridPanelLayout1: TGridPanelLayout;
    PlusBtn2: TButton;
    ProfilBtn2: TButton;
    HomeBtn2: TButton;
    SupermaerkteBtn2: TButton;
    Label1: TLabel;
    ListBox1: TListBox;
    procedure ProfilBtn2Click(Sender: TObject);
    procedure HomeBtn2Click(Sender: TObject);
    procedure SupermaerkteBtn2Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form3: TForm3;

implementation

{$R *.fmx}

uses Home, Profil, SupermarktListe;

procedure TForm3.HomeBtn2Click(Sender: TObject);
begin
  Supermaerkte.Form3.Visible := false;
  Home.Form1.Visible := true;
end;

procedure TForm3.ProfilBtn2Click(Sender: TObject);
begin
  Supermaerkte.Form3.Visible := false;
  Profil.Form4.Visible := true;
end;

procedure TForm3.SupermaerkteBtn2Click(Sender: TObject);
begin
  Supermaerkte.Form3.Visible := false;
  SupermarktListe.Form2.Visible := true;
end;

end.
