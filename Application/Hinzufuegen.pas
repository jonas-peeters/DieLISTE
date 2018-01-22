unit Hinzufuegen;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Controls.Presentation, FMX.Layouts, serverAPI;

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
    procedure BtnSchliessenClick(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    constructor Create(AOwner: TComponent; var serverAPI: TServerAPI);
    procedure BtnHinzufuegenClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form9: TForm9;
  privateServerAPI: TServerAPI;

implementation

{$R *.fmx}
constructor TForm9.Create(AOwner: TComponent; var serverAPI: TServerAPI);
begin
  inherited Create(AOwner);
  privateServerAPI := serverAPI;
end;

procedure TForm9.BtnHinzufuegenClick(Sender: TObject);
var name, einheit, kategorie: string;
    menge: real;
begin
    name:= EdtName.Text;
    einheit:= EdtEinheit.Text;
    kategorie:=EdtKategorie.Text;
    menge:= StrToFloat(EdtMenge.Text);
    privateServerAPI.AddToList(name, menge, einheit, kategorie);
    EDTname.Text:='Name';
    Edteinheit.Text:= 'Einheit';
    Edtkategorie.Text:='Kategorie';
    Edtmenge.Text:='Menge';
end;

procedure TForm9.BtnOKClick(Sender: TObject);
var name, einheit, kategorie: string;
    menge: real;
begin
    name:= EdtName.Text;
    einheit:= EdtEinheit.Text;
    kategorie:=EdtKategorie.Text;
    menge:= StrToFloat(EdtMenge.Text);
    privateServerAPI.AddToList(name, menge, einheit, kategorie);
    Release;
end;

procedure TForm9.BtnSchliessenClick(Sender: TObject);
begin
 Release;
end;

end.
