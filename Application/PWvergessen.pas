unit PWvergessen;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Controls.Presentation, FMX.Layouts;

type
  TForm10 = class(TForm)
    GridPanelLayout1: TGridPanelLayout;
    LblText: TLabel;
    EdtEMail: TEdit;
    EdtBenutzername: TEdit;
    BtnSenden: TButton;
    LblPWvergessen: TLabel;
    Panel1: TPanel;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form10: TForm10;

implementation

{$R *.fmx}



end.
