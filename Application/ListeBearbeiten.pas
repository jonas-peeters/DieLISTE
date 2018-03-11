{*
  Liste-Bearbeiten-Form

  Der User kann die Eigenschaften einer Liste bearbeiten.

  Mögliche Aktionen sind:
  - Listennamen ändern
  - User von der Liste entfernen
  - Sehen wer Zugriff auf die Liste hat
  - Form, um User hinzufügen, öffnen
  - Die Liste löschen
}
unit ListeBearbeiten;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.StdCtrls, FMX.ListBox, FMX.Controls.Presentation, serverAPI, AddUser,
  Helper, FMX.Objects, FMX.Platform, FMX.Gestures;

type
  TFormListeBearbeiten = class(TForm)
    GridPanelLayout1: TGridPanelLayout;
    ListBox1: TListBox;
    Label1: TLabel;
    LBIEditListName: TListBoxItem;
    LBIDeleteList: TListBoxItem;
    LBIAddUser: TListBoxItem;
    SettingsGroupHeader: TListBoxGroupHeader;
    GroupHeaderUser: TListBoxGroupHeader;
    ImgBack: TImage;
    GestureManager1: TGestureManager;
    procedure ImgBackClick(Sender: TObject);
    procedure EditListNameClick(Sender: TObject);
    procedure DeleteListClick(Sender: TObject);
    constructor Create(AOwner: TComponent; var serverAPI: TServerAPI; selectedListId: Integer);
    procedure AddUserClick(Sender: TObject);
    procedure subFormClosed(Sender: TObject; var Action: TCloseAction);
    procedure Update();
    procedure FormGesture(Sender: TObject; const EventInfo: TGestureEventInfo;
      var Handled: Boolean);
  private
    { Private-Deklarationen }
  public
    {*
      Ob die Liste gelöscht wurde.

      Dies wird benutzt, damit die Liste-Anzeigen-Form sich automatisch
      schließen kann, sollte die Liste gelöscht worden sein.
    }
    hasDeletedList: Boolean;
  end;

var
  //* Die Liste-Bearbeiten-From
  FormListeBearbeiten: TFormListeBearbeiten;
  //* Die ausgewählte Liste
  list: TListe;
  //* Die Id der ausgewählten Liste
  listId: Integer;
  //* Private Instanz der Server API in der der User angemeldet ist.
  privateServerAPI: TServerAPI;
  //* Um zu überprüfen, ob die Gestenaktion bereits ausgeführt wurde.
  closed: Boolean;

implementation

{$R *.fmx}

{*
  Neuer Konstruktor

  Neuer Kontruktor, um eine private Instanz der Server API und die Id der
  ausgewählten Liste zu übergeben.

  @param AOwner Der Parent der Form
  @param serverAPI Instanz der Server API in der der User angemeldet ist
  @param selectedListId Id der ausgewählten Liste
}
constructor TFormListeBearbeiten.Create(AOwner: TComponent; var serverAPI: TServerAPI; selectedListId: Integer);
begin
  inherited Create(AOwner);
  privateServerAPI := serverAPI;
  listId := selectedListId;
  hasDeletedList := false;
  closed := false;
  Update;
end;

{*
  Unterform geschlossen

  Wird eine Unterform geschlossen, so wird die Seite geupdated

  @param Sender Die Unterform
  @param Action Die Schließaktion der Unterform
}
procedure TFormListeBearbeiten.subFormClosed(Sender: TObject; var Action: TCloseAction);
begin
  Update();
end;

{*
  Update

  Die Form geupdated, um die aktuellen Informationen über die Liste anzuzeigen.
}
procedure TFormListeBearbeiten.Update();
var
  lists: TListArray;
  i: Integer;
  item: TListBoxItem;
  mainHeader: TListBoxHeader;
  mainLabel: TLabel;
  groupHeader: TListBoxGroupHeader;
begin
  if privateServerAPI.isOnline then
  begin
    lists := privateServerAPI.getLists;
    for i := 0 to High(lists) do
      if lists[i].id = listId then
        list := lists[i];
    ListBox1.Clear;

    Label1.Text := 'Liste "' + list.name + '" bearbeiten';

    // Add 'Allgemein' header
    groupHeader := TListBoxGroupHeader.Create(ListBox1);
    groupHeader.Text := 'Allgemein';
    ListBox1.AddObject(groupHeader);
    {$IF defined(MSWINDOWS)}
      groupHeader.Height:=23;
    {$ENDIF}

    // Add change name
    item := TListBoxItem.Create(ListBox1);
    item.Text := 'Name ändern';
    item.OnClick := EditListNameClick;
    ListBox1.AddObject(item);
    {$IF defined(MSWINDOWS)}
      item.Height:=23;
    {$ENDIF}

    // Add delete list
    item := TListBoxItem.Create(ListBox1);
    item.Text := 'Liste löschen';
    item.OnClick := DeleteListClick;
    ListBox1.AddObject(item);
    {$IF defined(MSWINDOWS)}
      item.Height:=23;
    {$ENDIF}

    // Add add user
    item := TListBoxItem.Create(ListBox1);
    item.Text := 'User hinzufügen';
    item.OnClick := AddUserClick;
    ListBox1.AddObject(item);
    {$IF defined(MSWINDOWS)}
      item.Height:=23;
    {$ENDIF}

     // Add 'Benutzer' header
    groupHeader := TListBoxGroupHeader.Create(ListBox1);
    groupHeader.Text := 'Benutzer';
    ListBox1.AddObject(groupHeader);
    {$IF defined(MSWINDOWS)}
      groupHeader.Height:=23;
    {$ENDIF}

    // Add users with access
    for i := 0 to High(list.user) do
    begin
      item := TListBoxItem.Create(ListBox1);
      item.Text := list.user[i];
      item.ItemData.Detail := IntToStr(i);
      ListBox1.AddObject(item);
      {$IF defined(MSWINDOWS)}
        item.Height:=20;
      {$ENDIF}
    end;
  end
  else
  begin
    Close;
    Release;
  end;
end;

{*
  Zurück

  Die Form wird geschlossen und der User landet wieder auf der
  Liste-Anzeigen-Seite.

  @param Sender Button um zurückzugelangen.
}
procedure TFormListeBearbeiten.ImgBackClick(Sender: TObject);
begin
  Close;
  Release;
end;

{*
  User hinzufügen

  Die Seite, um einen User hinzuzufügen, wird geöffnet.

  @param Sender Button/Label um User hinzuzufügen
}
procedure TFormListeBearbeiten.AddUserClick(Sender: TObject);
var
  addUserForm: TFormAddUser;
begin
  addUserForm := TFormAddUser.Create(nil, privateServerAPI, listId);
  addUserForm.Show();
  addUserForm.OnClose := subFormClosed;
end;


{*
  Liste löschen

  Nach einer weiteren Abfrage wird die Liste gelöscht. Dann wird der User auf
  die Home-Seite weitergeleitet.

  @param Sender Button/Label um die Liste zu löschen
}
procedure TFormListeBearbeiten.DeleteListClick(Sender: TObject);
begin
  if privateServerAPI.isOnline then
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
          privateServerAPI.removeList(listId);
          ShowMessage('Die Liste wurde gelöscht!');
          hasDeletedList := true;
          Close;
          Release;
          end;
      end;
    end);
  end
  else
    ShowMessage('Du brauchst eine aktive Internetverbindung für diese Aktion!');
end;

{*
  Namen bearbeiten

  In einer Input Box kann der User der Liste einen neuen Namen geben.

  @param Sender Button/Label um der Liste inene neuen Name zu geben.
}
procedure TFormListeBearbeiten.EditListNameClick(Sender: TObject);
var
  dialogService: IFMXDialogServiceAsync;
begin
  if privateServerAPI.isOnline then
  begin
    if TPlatformServices.Current.SupportsPlatformService (IFMXDialogServiceAsync, IInterface (dialogService)) then
    begin
      // Bei dieser InputQuery funktioniert unter Windows und MacOS der Cancel Button nicht.
      // Diese Software ist nur zur Benutzung unter iOS und Android gedacht.
      // Dort funktioniert die Funktion einwandfrei.
      // Soll diese Funktion dennoch unter
      // Windows ausgeführt werden kann entweder nur der OK-Button gedrückt werden
      // oder der im Embarcadero Bug System vorgeschlagene Fix angewandt werden.
      // Dieser ist hier zu finden: https://quality.embarcadero.com/browse/RSP-16670
      dialogService.InputQueryAsync('Namen ändern', ['Neuer Name'], [list.name],
        procedure (const AResult : TModalResult; const AValues : array of string)
        begin
            case AResult of
              mrOk:
                begin
                  if AValues[0] <> list.name then
                  begin
                    privateServerAPI.ChangeListName(AValues[0], listId);
                    Update();
                  end;
                end;
              else
                begin
                end;
            end;
        end
      );
    end;
  end
  else
    ShowMessage('Du brauchst eine aktive Internetverbindung für diese Aktion!');
end;

{*
  Streichaktion von links nach rechts

  Der user wird auf die List-Anzeigen-Seite weitergeleitet.

  @param Sender GestureManager
  @param EventInfo Informationen über die Geste
  @param Handled Ob die Geste bearbeitet wurde.
}
procedure TFormListeBearbeiten.FormGesture(Sender: TObject;
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

end.
