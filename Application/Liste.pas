unit Liste;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.ListBox, FMX.StdCtrls, FMX.Controls.Presentation, Hinzufuegen;

type
  TForm8 = class(TForm)
    GridPanelLayout1: TGridPanelLayout;
    LblListe: TLabel;
    BtnEdit: TButton;
    BtnHinzufuegen: TButton;
    ListBox1: TListBox;
    Panel1: TPanel;
    procedure BtnHinzufuegenClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form8: TForm8;

implementation

{$R *.fmx}

procedure TForm8.BtnHinzufuegenClick(Sender: TObject);
begin
  Hinzufuegen.Form9.Visible:=true;
end;

end.
