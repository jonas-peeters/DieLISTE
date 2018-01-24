unit Hinzufuegen;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Controls.Presentation, FMX.Layouts, serverAPI, FMX.ListBox;

type
  TFormHinzufuegen = class(TForm)
    GridPanelLayout1: TGridPanelLayout;
    LblHinzufuegen: TLabel;
    EdtName: TEdit;
    EdtEinheit: TEdit;
    EdtMenge: TEdit;
    BtnOK: TButton;
    BtnSchliessen: TButton;
    BtnHinzufuegen: TButton;
    Panel1: TPanel;
    CBCategory: TComboBox;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    ListBoxItem4: TListBoxItem;
    ListBoxItem5: TListBoxItem;
    ListBoxItem6: TListBoxItem;
    ListBoxItem7: TListBoxItem;
    ListBoxItem8: TListBoxItem;
    ListBoxItem9: TListBoxItem;
    procedure BtnSchliessenClick(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    constructor Create(AOwner: TComponent; var serverAPI: TServerAPI; selectedListId: Integer);
    procedure BtnHinzufuegenClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  HinzufuegenForm: TFormHinzufuegen;
  privateServerAPI: TServerAPI;
  listId: Integer;

implementation

{$R *.fmx}
constructor TFormHinzufuegen.Create(AOwner: TComponent; var serverAPI: TServerAPI; selectedListId: Integer);
begin
  inherited Create(AOwner);
  privateServerAPI := serverAPI;
  listId := selectedListId;
end;

procedure TFormHinzufuegen.BtnHinzufuegenClick(Sender: TObject);
var
  name, einheit, menge: string;
  kategorie: Integer;
begin
    name:= EdtName.Text;
    einheit:= EdtEinheit.Text;
    menge:= EdtMenge.Text;
    kategorie := CBCategory.ItemIndex;
    privateServerAPI.AddToList(name, menge + einheit, false, kategorie, listId);
    EdtName.Text := '';
    EdtEinheit.Text := '';
    EdtMenge.Text := '';
    CBCategory.ItemIndex := 0;
end;

procedure TFormHinzufuegen.BtnOKClick(Sender: TObject);
var
  name, einheit, menge: string;
  kategorie: Integer;
begin
    name:= EdtName.Text;
    einheit:= EdtEinheit.Text;
    menge:= EdtMenge.Text;
    kategorie := CBCategory.ItemIndex;
    privateServerAPI.AddToList(name, menge + einheit, false, kategorie, listId);
    Release;
end;

procedure TFormHinzufuegen.BtnSchliessenClick(Sender: TObject);
begin
 Release;
end;

end.
