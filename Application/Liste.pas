{*
  View of an individual list

  Here the user sees the item that are in the list, can enter the menu for
  editing the list, can open the page for adding new items and can open the page
  to edit an item.
}
unit Liste;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.ListBox, FMX.StdCtrls, FMX.Controls.Presentation, Hinzufuegen, ListeBearbeiten, serverAPI,
  FMX.Edit, ItemBearbeiten, Helper;

type
  TFormListe = class(TForm)
    GridPanelLayout1: TGridPanelLayout;
    LblListe: TLabel;
    BtnEdit: TButton;
    BtnHinzufuegen: TButton;
    ListBox1: TListBox;
    BtnBack: TButton;
    procedure BtnHinzufuegenClick(Sender: TObject);
    procedure BtnEditClick(Sender: TObject);
    constructor Create(AOwner: TComponent; var serverAPI: TServerAPI; clickedList: TListe);
    procedure Update();
    procedure FormActivate(Sender: TObject);
    procedure BtnBackClick(Sender: TObject);
    procedure ClickOnItem(Sender: TObject);
    procedure subFormClosed(Sender: TObject; var Action: TCloseAction);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  ListForm: TFormListe;
  privateServerAPI: TServerAPI;
  list: TListe;
  listId: Integer;
  lists: Tlistarray;

implementation

{$R *.fmx}

procedure TFormListe.subFormClosed(Sender: TObject; var Action: TCloseAction);
begin
  Update();
end;

procedure TFormListe.BtnBackClick(Sender: TObject);
begin
  Close;
  Release;
end;

procedure TFormListe.BtnEditClick(Sender: TObject);
var editlistForm:TFormListeBearbeiten;
begin
  editlistForm := TFormListeBearbeiten.Create(Application, privateServerAPI, listId);
  editlistForm.Show;
  editlistForm.OnClose := subFormClosed;
end;

procedure TFormListe.BtnHinzufuegenClick(Sender: TObject);
var
  additemForm: TFormHinzufuegen;
begin
  additemForm := TFormHinzufuegen.Create(Application, privateServerAPI, listId);
  additemForm.Show;
  additemForm.OnClose := subFormClosed;
end;

constructor TFormListe.Create(AOwner: TComponent; var serverAPI: TServerAPI; clickedList: TListe);
begin
  inherited Create(AOwner);
  privateServerAPI := serverAPI;
  listId := clickedList.id;
  Update();
end;

procedure TFormListe.FormActivate(Sender: TObject);
begin
  Update();
end;

procedure TFormListe.Update();
var
  i: Integer;
  item: TListBoxItem;
  lists: TListArray;
begin
  lists := privateServerAPI.getLists();
  for i := 0 to High(lists) do
    if lists[i].id = listId then
      list := lists[i];
  LblListe.Text := list.name;
  ListBox1.Clear;
  for i := 0 to High(list.items) do
  begin
    item := TListBoxItem.Create(ListBox1);
    item.Text := list.items[i].name + Tabulator + list.items[i].quantity;
    if list.items[i].done then
      item.ItemData.Accessory := TListBoxItemData.TAccessory(1)
    else
      item.ItemData.Accessory := TListBoxItemData.TAccessory(0);
    item.ItemData.Detail := IntToStr(i);
    item.OnClick := ClickOnItem;
    ListBox1.AddObject(item);
  end;
end;

procedure TFormListe.ClickOnItem(Sender: TObject);
var
  itemAendernForm: TFormItemBearbeiten;
begin
  itemAendernForm := TFormItemBearbeiten.Create(nil, privateServerAPI, list.items[StrToInt(ListBox1.Selected.ItemData.Detail)]);
  itemAendernForm.Show();
end;


end.
