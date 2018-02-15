unit AddUser;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.ListBox, FMX.Controls.Presentation, FMX.Edit, serverAPI;

type
  TFormAddUser = class(TForm)
    GridPanelLayout1: TGridPanelLayout;
    BtnBack: TButton;
    ListBox1: TListBox;
    Label1: TLabel;
    Edit1: TEdit;
    procedure BtnBackClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure ClickOnName(Sender: TObject);
    constructor Create(AOwner: TComponent; var serverAPI: TServerAPI; selectedListId: Integer);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TFormAddUser;
  privateServerAPI: TServerAPI;
  listId: Integer;

implementation

{$R *.fmx}

constructor TFormAddUser.Create(AOwner: TComponent; var serverAPI: TServerAPI; selectedListId: Integer);
var
  vorschlaege: TArray;
  item: TListBoxItem;
  i: Integer;
begin
  inherited Create(AOwner);
  privateServerAPI := serverAPI;
  listId := selectedListId;
  vorschlaege := privateServerAPI.userSuggestions(listID, '');
  for i := 0 to High(vorschlaege) do
  begin
    item := TListBoxItem.Create(Listbox1);
    item.Text := vorschlaege[i];
    item.ItemData.Accessory := TListBoxItemData.TAccessory(1);
    item.OnClick := ClickOnName;
    ListBox1.AddObject(item);
  end;
end;

procedure TFormAddUser.BtnBackClick(Sender: TObject);
begin
  Release;
end;

procedure TFormAddUser.Edit1Change(Sender: TObject);
var
  i: Integer;
  item: TListBoxItem;
  vorschlaege: TArray;
begin
  Listbox1.Items.Clear;
  vorschlaege := privateServerAPI.userSuggestions(listID, Edit1.Text);
  for i := 0 to High(vorschlaege) do
  begin
    item := TListBoxItem.Create(Listbox1);
    item.Text := vorschlaege[i];
    item.ItemData.Accessory := TListBoxItemData.TAccessory(1);
    item.OnClick := ClickOnName;
    ListBox1.AddObject(item);
  end;
end;

procedure TFormAddUser.ClickOnName(Sender: TObject);
begin
MessageDlg('Wollen Sie diese Person zu ihrer Liste hinzufügen?', System.UITypes.TMsgDlgType.mtCustom,
[ System.UITypes.TMsgDlgBtn.mbYes,
  System.UITypes.TMsgDlgBtn.mbNo,
  System.UITypes.TMsgDlgBtn.mbCancel
],0,
procedure (const AResult:System.UITypes.TModalResult)
begin
  case AResult of
    mrYES:
    if  privateServerAPI.inviteUser(listId, ListBox1.Selected.Text)='"Success"' then
      begin
        ShowMessage('Der User wurde eingeladen!');
        Release;
      end;
  end;
end);
end;


end.
