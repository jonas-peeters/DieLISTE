unit Liste;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.ListBox, FMX.StdCtrls, FMX.Controls.Presentation, Hinzufuegen, serverAPI,
  FMX.Edit;

type
  TFormListe = class(TForm)
    GridPanelLayout1: TGridPanelLayout;
    LblListe: TLabel;
    BtnEdit: TButton;
    BtnHinzufuegen: TButton;
    ListBox1: TListBox;
    Panel1: TPanel;
    procedure BtnHinzufuegenClick(Sender: TObject);
    procedure BtnEditClick(Sender: TObject);
    constructor Create(AOwner: TComponent; var serverAPI: TServerAPI; clickedList: TListe);
    procedure Update();
    procedure FormActivate(Sender: TObject);
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

implementation

{$R *.fmx}

procedure TFormListe.BtnEditClick(Sender: TObject);
var
  neuerName: String;
begin
  repeat
    if not InputQuery('Namen ändern', 'Neuer Name:', neuerName) then
      neuerName := list.name
  until neuerName <> '';
  if neuerName <> list.name then
  begin
    privateServerAPI.ChangeListName(neuerName, listId);
    Update();
  end;
end;

procedure TFormListe.BtnHinzufuegenClick(Sender: TObject);
var
  additemForm: TFormHinzufuegen;
begin
  additemForm := TFormHinzufuegen.Create(Application, privateServerAPI, listId);
  additemForm.Show;
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
  begin
    if lists[i].id = listId then
    begin
      list := lists[i];
    end;
  end;
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
    ListBox1.AddObject(item);
  end;
end;

end.
