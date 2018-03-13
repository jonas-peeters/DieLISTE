{*
  User-Hinzufügen-Form

  Hier kann der User andere User finden und zu der ausgewählten Liste einladen.

  Wenn die Form geöffnet wird, sieht der User zunächst eine Liste anderer User
  mit denen er bereits an anderen Listen zusammenarbeitet, um diese schnell
  hinzufügen zu können.
  Beginnt der User in dem Suchfeld zu tippen werden live vom Server die Namen
  anderer User herutergeladen. User mit denen der User an anderen Liste
  zusammenarbeitet werden noch immer ganz oben angezeigt bis sie nicht mehr dem
  Suchbegriff entsprechen.
}
unit AddUser;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.ListBox, FMX.Controls.Presentation, FMX.Edit, serverAPI,
  Helper, FMX.Objects, FMX.Gestures, REST.Client, REST.Types, IPPeerCommon,
  IPPeerClient;

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
  //* Die User-Hinzufügen-Form
  FormAddUser: TFormAddUser;
  //* Private Instanz der Server API in der der User angemeldet ist.
  privateServerAPI: TServerAPI;
  //* Die Id der Liste zu der User andere User hinzufügen möchte
  listId: Integer;
  //* Ob die Geste um zur vorherigen Seite zurückzukehren bereits bearbeitet wurde
  closed: Boolean;

implementation

{$R *.fmx}

{*
  Neuer Konstruktor

  Neuer Konstruktor, um die Server API und die Id der Liste, zu der ein neuer
  User hinzugefügt werden soll, zu übergeben.

  @param AOwner Der Parent der Form
  @param serverAPI Private Instanz der Server API in der der User angemeldet ist
  @param selectedListId Id der Liste, zu der ein neuer User hinzugefügt werden soll
}
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

{*
  Zurück

  Der User wird auf die Liste-Bearbeiten-Form weitergeleitet.

  @param Sender Button um zurückzukehren
}
procedure TFormAddUser.BtnBackClick(Sender: TObject);
begin
  Close;
  Release;
end;

{*
  Änderung im Suchfeld

  Wenn der User tippt wird automatisch vom eine List ein Usern geladen und
  angezeigt, die mit dem Suchbegriff übereinstimmt.

  @param Sender Das Suchfeld
}
procedure TFormAddUser.Edit1Change(Sender: TObject);
var
  // Eigene Instanz, um asyncron an der Form zu arbeiten.
  request: TRESTRequest;
begin
  if checkForInvalidCharacters(Edit1) AND privateServerAPI.isOnline then
  begin
    request := TRESTRequest.Create(nil);
    request.Method := REST.Types.rmGET;
    request.Resource := 'user/lists/' + IntToStr(listId) + '/suggestions/' + Edit1.Text;
    request.Client := privateServerAPI.client;
    request.Timeout := 3000;
    try
      request.ExecuteAsync(procedure
      var
        i: Integer;
        item: TListBoxItem;
        vorschlaege: TArray;
      begin
        vorschlaege := jsonArrayToStringArray(request.Response.Content);
        Listbox1.Items.Clear;
        for i := 0 to High(vorschlaege) do
        begin
          item := TListBoxItem.Create(Listbox1);
          item.Text := vorschlaege[i];
          item.ItemData.Accessory := TListBoxItemData.TAccessory(1);
          item.OnClick := ClickOnName;
          ListBox1.AddObject(item);
        end;
      end, true, true, procedure (Sender: TObject)
      begin
        ShowMessage('Du brauchst eine aktive Internetverbindung für diese Aktion!');
      end);
    except
      ShowMessage('Du brauchst eine aktive Internetverbindung für diese Aktion!');
    end;
  end
  else
    ShowMessage('Du brauchst eine aktive Internetverbindung für diese Aktion!');
end;

{*
  Zurück

  Wird bei einer Streichgeste vom linken Rand nach rechts ausgeführt.

  Der User wird dann auf die Liste-Bearbeiten-Form weitergeleitet.

  @param Sender Der GestureManager
  @param EventInfo Informationen über die Geste
  @param Handled Ob die Geste schon bearbeitet wurde
}
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

{*
  Suchfeld leeren

  @param Sender Clear-Button auf dem Edit feld
}
procedure TFormAddUser.ClearEditButton1Click(Sender: TObject);
begin
  Edit1.Text := '';
end;

{*
  User hinzufügen

  Der User klickt auf einen Benuzternamen und nach einer weiteren Abfrage wird
  dieser per EMail zu der Liste eingeladen.

  @param Sender ListBoxItem auf das geklickt wurde
}
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
