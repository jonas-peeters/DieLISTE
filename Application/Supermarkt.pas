unit Supermarkt;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.ListBox;

type
  TForm2 = class(TForm)
    GridPanelLayout1: TGridPanelLayout;
    PlusBtn2: TButton;
    Label1: TLabel;
    ListBox1: TListBox;
    BackButton: TButton;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

end.
