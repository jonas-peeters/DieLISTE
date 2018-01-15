unit Liste;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.ListBox, FMX.StdCtrls, FMX.Controls.Presentation, Hinzufuegen, serverAPI;

type
  TForm8 = class(TForm)
    GridPanelLayout1: TGridPanelLayout;
    LblListe: TLabel;
    BtnEdit: TButton;
    BtnHinzufuegen: TButton;
    ListBox1: TListBox;
    Panel1: TPanel;
    procedure BtnHinzufuegenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form8: TForm8;
  serverAPI: TServerAPI;

implementation

{$R *.fmx}

procedure TForm8.BtnHinzufuegenClick(Sender: TObject);
begin
  Hinzufuegen.Form9.Visible:=true;
end;

procedure TForm8.FormCreate(Sender: TObject);
var a: Array of string;
    I:integer;
begin
  serverAPI := TServerAPI.create();
  A:=TServerAPI.jsonArrayToArray(TServerAPI.GetLists);
  for I := Low(A) to High(A) do
  begin
    ListBox1.items.Add(A[i]);
  end;
end;

end.
