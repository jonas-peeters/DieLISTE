unit Hinzufuegen;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Controls.Presentation, FMX.Layouts;

type
  TForm9 = class(TForm)
    GridPanelLayout1: TGridPanelLayout;
    LblHinzufuegen: TLabel;
    EdtName: TEdit;
    EdtEinheit: TEdit;
    EdtMenge: TEdit;
    EdtKategorie: TEdit;
    BtnOK: TButton;
    BtnSchliessen: TButton;
    BtnHinzufuegen: TButton;
    Panel1: TPanel;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form9: TForm9;

implementation

{$R *.fmx}

end.
