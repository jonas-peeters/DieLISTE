{*
  Here the user can change the properties of a list.

  Possible actions are:
  - Changing the name of the list
  - Remove users from the list
  - See who has access to the list
  - Open the page for adding users to this list
  - Deleting this list
}
unit ListeBearbeiten;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.StdCtrls, FMX.ListBox, FMX.Controls.Presentation, serverAPI, AddUser,
  Helper;

type
  TFormListeBearbeiten = class(TForm)
    GridPanelLayout1: TGridPanelLayout;
    BtnBack: TButton;
    ListBox1: TListBox;
    Label1: TLabel;
    LBIEditListName: TListBoxItem;
    LBIDeleteList: TListBoxItem;
    LBIAddUser: TListBoxItem;
    SettingsGroupHeader: TListBoxGroupHeader;
    GroupHeaderUser: TListBoxGroupHeader;
    procedure BtnBackClick(Sender: TObject);
    procedure EditListNameClick(Sender: TObject);
    procedure DeleteListClick(Sender: TObject);
    constructor Create(AOwner: TComponent; var serverAPI: TServerAPI; selectedListId: Integer);
    procedure AddUserClick(Sender: TObject);
    procedure subFormClosed(Sender: TObject; var Action: TCloseAction);
    procedure Update();
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FormListeBearbeiten: TFormListeBearbeiten;
  list: TListe;
  listId: Integer;
  privateServerAPI: TServerAPI;

implementation

{$R *.fmx}

constructor TFormListeBearbeiten.Create(AOwner: TComponent; var serverAPI: TServerAPI; selectedListId: Integer);
begin
  inherited Create(AOwner);
  privateServerAPI := serverAPI;
  listId := selectedListId;
  Update;
end;

procedure TFormListeBearbeiten.subFormClosed(Sender: TObject; var Action: TCloseAction);
begin
  Update();
end;

procedure TFormListeBearbeiten.Update();
var
  lists: TListArray;
  i: Integer;
  item: TListBoxItem;
  header: TListBoxGroupHeader;
begin
  lists := privateServerAPI.getLists;
  for i := 0 to High(lists) do
    if lists[i].id = listId then
      list := lists[i];
  ListBox1.Clear;

  // Add Listname
  header := TListBoxGroupHeader.Create(ListBox1);
  header.Text := list.name;
  ListBox1.AddObject(header);

  // Add change name
  item := TListBoxItem.Create(ListBox1);
  item.Text := 'Name ändern';
  item.OnClick := EditListNameClick;
  ListBox1.AddObject(item);

  // Add delete list
  item := TListBoxItem.Create(ListBox1);
  item.Text := 'Liste löschen';
  item.OnClick := DeleteListClick;
  ListBox1.AddObject(item);

  // Add 'Benutzer' header
  header := TListBoxGroupHeader.Create(ListBox1);
  header.Text := 'Benutzer';
  ListBox1.AddObject(header);

  // Add add user
  item := TListBoxItem.Create(ListBox1);
  item.Text := 'User hinzufügen';
  item.OnClick := AddUserClick;
  ListBox1.AddObject(item);

  // Add users with access
  for i := 0 to High(list.user) do
  begin
    item := TListBoxItem.Create(ListBox1);
    item.Text := list.user[i];
    item.ItemData.Detail := IntToStr(i);
    ListBox1.AddObject(item);
  end;
end;

procedure TFormListeBearbeiten.BtnBackClick(Sender: TObject);
begin
  Close;
  Release;
end;

procedure TFormListeBearbeiten.AddUserClick(Sender: TObject);
var
  addUserForm: TFormAddUser;
begin
  addUserForm := TFormAddUser.Create(nil, privateServerAPI, listId);
  addUserForm.Show();
  addUserForm.OnClose := subFormClosed;
end;

procedure TFormListeBearbeiten.DeleteListClick(Sender: TObject);
begin
MessageDlg('Wollen Sie die Liste wirklich löschen?', System.UITypes.TMsgDlgType.mtCustom,
[ System.UITypes.TMsgDlgBtn.mbYes,
  System.UITypes.TMsgDlgBtn.mbNo,
  System.UITypes.TMsgDlgBtn.mbCancel
],0,
procedure (const AResult:System.UITypes.TModalResult)
begin
  case AResult of
    mrYES:
      begin
      privateServerAPI.removeList(list.id);
      ShowMessage('Die Liste wurde gelöscht!');
      Close;
      Release;
      end;
  end;
end);
end;

procedure TFormListeBearbeiten.EditListNameClick(Sender: TObject);
var neuerName:String;
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

end.
