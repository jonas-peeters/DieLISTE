unit AddUser;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.ListBox, FMX.Controls.Presentation;

type
  TForm1 = class(TForm)
    GridPanelLayout1: TGridPanelLayout;
    BtnBack: TButton;
    ListBox1: TListBox;
    Label1: TLabel;
    procedure BtnBackClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.BtnBackClick(Sender: TObject);
begin
  Release;
end;

end.
