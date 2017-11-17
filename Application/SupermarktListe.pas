unit SupermarktListe;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.ListBox;

type
  TForm2 = class(TForm)
    GridPanelLayout1: TGridPanelLayout;
    PlusBtn2: TButton;
    ProfilBtn2: TButton;
    HomeBtn2: TButton;
    SupermaerkteBtn2: TButton;
    Label1: TLabel;
    EditBtn2: TButton;
    ListBox1: TListBox;
    procedure ProfilBtn2Click(Sender: TObject);
    procedure HomeBtn2Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

uses Home, Profil;

procedure TForm2.HomeBtn2Click(Sender: TObject);
begin
  SupermarktListe.Form2.Visible := false;
  Home.Form1.Visible := true;
end;

procedure TForm2.ProfilBtn2Click(Sender: TObject);
begin
  SupermarktListe.Form2.Visible := false;
  Profil.Form4.Visible := true;
end;

end.
