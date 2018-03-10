{*
  The user can find and add other users that then have access to the list

  When this form is opened the user will see a list of users that he is also
  working with on other lists.
  Only when the users starts typing he will see other users in the system, but
  if his search string complies with users he is working with on other projects,
  those will be placed at the top of the list.

  When clicking on a users name (after an additional confirmation) this user
  will recieve an email, where they can accept the invitation or report it as
  spam.
}
unit AddUser;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.ListBox, FMX.Controls.Presentation, FMX.Edit, serverAPI,
  Helper, FMX.Objects, FMX.Gestures;

type
  TFormAddUser = class(TForm)
    GridPanelLayout1: TGridPanelLayout;
    ListBox1: TListBox;
    Edit1: TEdit;
    Label1: TLabel;
    ClearEditButton1: TClearEditButton;
    Image1: TImage;
    GestureManager1: TGestureManager;
    procedure BtnBackClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure ClickOnName(Sender: TObject);
    constructor Create(AOwner: TComponent; var serverAPI: TServerAPI; selectedListId: Integer);
    procedure ClearEditButton1Click(Sender: TObject);
    procedure FormGesture(Sender: TObject; const EventInfo: TGestureEventInfo;
      var Handled: Boolean);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TFormAddUser;
  privateServerAPI: TServerAPI;
  listId: Integer;
  closed: Boolean;

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
  closed := false;
  if privateServerAPI.isOnline then
  begin
    vorschlaege := privateServerAPI.userSuggestions(listID, '');
    for i := 0 to High(vorschlaege) do
    begin
      item := TListBoxItem.Create(Listbox1);
      item.Text := vorschlaege[i];
      item.ItemData.Accessory := TListBoxItemData.TAccessory(1);
      item.OnClick := ClickOnName;
      ListBox1.AddObject(item);
    end;
  end
  else
  begin
    ShowMessage('Du brauchst eine aktive Internetverbindung für diese Aktion!');
    Close;
    Release;
  end;
end;

procedure TFormAddUser.BtnBackClick(Sender: TObject);
begin
  Close;
  Release;
end;

procedure TFormAddUser.Edit1Change(Sender: TObject);
var
  i: Integer;
  item: TListBoxItem;
  vorschlaege: TArray;
begin
  Listbox1.Items.Clear;
  if privateServerAPI.isOnline then
  begin
    vorschlaege := privateServerAPI.userSuggestions(listID, Edit1.Text);
    for i := 0 to High(vorschlaege) do
    begin
      item := TListBoxItem.Create(Listbox1);
      item.Text := vorschlaege[i];
      item.ItemData.Accessory := TListBoxItemData.TAccessory(1);
      item.OnClick := ClickOnName;
      ListBox1.AddObject(item);
    end;
  end
  else
    ShowMessage('Du brauchst eine aktive Internetverbindung für diese Aktion!');
end;

procedure TFormAddUser.FormGesture(Sender: TObject;
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

procedure TFormAddUser.ClearEditButton1Click(Sender: TObject);
begin
  Edit1.Text := '';
end;

procedure TFormAddUser.ClickOnName(Sender: TObject);
begin
  if privateServerAPI.isOnline then
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
        if interpretServerResponse(privateServerAPI.inviteUser(listId, ListBox1.Selected.Text)) then
          begin
            ShowMessage('Der User wurde eingeladen!');
            Close;
            Release;
          end;
      end;
    end);
  end
  else
    ShowMessage('Du brauchst eine aktive Internetverbindung für diese Aktion!');
end;


end.
