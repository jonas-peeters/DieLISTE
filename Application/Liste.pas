unit Liste;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.ListBox, FMX.StdCtrls, FMX.Controls.Presentation, Hinzufuegen, serverAPI,
  FMX.Edit;

type
  TForm8 = class(TForm)
    GridPanelLayout1: TGridPanelLayout;
    LblListe: TLabel;
    BtnEdit: TButton;
    BtnHinzufuegen: TButton;
    ListBox1: TListBox;
    Panel1: TPanel;
    EdtNeuerName: TEdit;
    BtnAendern: TButton;
    procedure BtnHinzufuegenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BtnEditClick(Sender: TObject);
    procedure BtnAendernClick(Sender: TObject);
    constructor Create(AOwner: TComponent; var serverAPI: TServerAPI);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form8: TForm8;
  privateServerAPI: TServerAPI;

implementation

{$R *.fmx}

procedure TForm8.BtnAendernClick(Sender: TObject);
begin
  privateServerAPI.ChangeListName(EdtNeuerName.Text);
end;

procedure TForm8.BtnEditClick(Sender: TObject);
begin
  EdtNeuerName.Visible:=true;
  BtnAendern.Visible:=true;
  EdtNeuerName.Text:= LblListe.Text;
end;

procedure TForm8.BtnHinzufuegenClick(Sender: TObject);
begin
  Hinzufuegen.Form9.Visible:=true;
end;

procedure TForm8.FormCreate(Sender: TObject);
var a: TArray;
    I:integer;
begin
  A:=privateServerAPI.jsonArrayToArray(privateServerAPI.GetLists());
  for I := Low(A) to High(A) do
  begin
      ListBox1.items.Add(A[i]);
  end;
  EdtNeuerName.Visible:=False;
  BtnAendern.Visible:=false;
end;

constructor TForm8.Create(AOwner: TComponent; var serverAPI: TServerAPI);
begin
  inherited Create(AOwner);
  privateServerAPI := serverAPI;
end;

end.
