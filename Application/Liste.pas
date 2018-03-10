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
  FMX.Edit, ItemBearbeiten, Helper, FMX.Gestures, FMX.Objects;

type
  TFormListe = class(TForm)
    GridPanelLayout1: TGridPanelLayout;
    LblListe: TLabel;
    ListBox1: TListBox;
    ImgBack: TImage;
    ImgAdd: TImage;
    ImgEdit: TImage;
    procedure ImgEditClick(Sender: TObject);
    GestureManager1: TGestureManager;
    Line1: TLine;
    constructor Create(AOwner: TComponent; var serverAPI: TServerAPI; clickedList: TListe);
    procedure Update();
    procedure FormActivate(Sender: TObject);
    procedure ClickOnItem(Sender: TObject);
    procedure subFormClosed(Sender: TObject; var Action: TCloseAction);
    procedure ListBox1ChangeCheck(Sender: TObject);
    procedure ImgBackClick(Sender: TObject);
    procedure ImgAddClick(Sender: TObject);
    procedure FormGesture(Sender: TObject; const EventInfo: TGestureEventInfo;
      var Handled: Boolean);

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
  closed: Boolean;

implementation

{$R *.fmx}

procedure TFormListe.subFormClosed(Sender: TObject; var Action: TCloseAction);
begin
  if (Sender.InheritsFrom(TFormListeBearbeiten)) then
  begin
    if ((Sender as TFormListeBearbeiten).hasDeletedList) then
    begin
      Close;
      Release;
    end;
  end;
  Update();
end;


procedure TFormListe.ImgBackClick(Sender: TObject);
begin
  Close;
  Release;
end;

procedure TFormListe.ImgEditClick(Sender: TObject);
var editlistForm:TFormListeBearbeiten;
begin
  editlistForm := TFormListeBearbeiten.Create(Application, privateServerAPI, listId);
  editlistForm.Show;
  editlistForm.OnClose := subFormClosed;
end;

procedure TFormListe.ImgAddClick(Sender: TObject);
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
  closed := false;
  Update();
end;

procedure TFormListe.FormActivate(Sender: TObject);
begin
  Update();
end;

procedure TFormListe.FormGesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  Handled := true;
  if not closed then
  begin
    closed := true;
    Close;
    Release;
  end;
end;

procedure TFormListe.ListBox1ChangeCheck(Sender: TObject);
var
  item: TItem;
  i: Integer;
  child: TListBoxItem;
  changed: Boolean;
begin
  changed := false;
  for i := 0 to ListBox1.Items.Count - 1 do
  begin
    child := ListBox1.ItemByIndex(i);
    item := list.items[StrToInt(child.ItemData.Detail)];
    if child.IsChecked <> item.done then
    begin
      privateServerAPI.DeleteItem(item.itemId);
      privateServerAPI.AddToList(item.name, item.quantity, child.IsChecked, item.categoryId, item.listId);
      changed := true;
    end;
  end;
  if changed then
    Update;
end;

procedure TFormListe.Update;
var
  i: Integer;
  item: TListBoxItem;
  lists: TListArray;
begin
  if privateServerAPI.isOnline then
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
      item.Text := Tabulator + list.items[i].name + Tabulator + list.items[i].quantity;
      if list.items[i].done then
        item.IsChecked := true
      else
        item.IsChecked := false;
      item.ItemData.Detail := IntToStr(i);
      item.OnClick := ClickOnItem;
      ListBox1.AddObject(item);
    end;
  end
  else
  begin
    ShowMessage('Du brauchst eine aktive Internetverbindung!');
    Close;
    Release;
  end;
end;

procedure TFormListe.ClickOnItem(Sender: TObject);
var
  itemAendernForm: TFormItemBearbeiten;
begin
  itemAendernForm := TFormItemBearbeiten.Create(Application, privateServerAPI, list.items[StrToInt((Sender as TListBoxItem).ItemData.Detail)]);
  itemAendernForm.OnClose := subFormClosed;
  itemAendernForm.Show();
end;


end.
