{*
  Haupt-Form (Home & Profil)

  Hier kann der User seine Listen sehen und seine persönlichen Daten bearbeiten.
}
unit UMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.ScrollBox, FMX.Memo, serverAPI,
  FMX.TabControl, FMX.Layouts, FMX.ListBox, Liste, JSON, FMX.Edit, FMX.SearchBox,
  PWvergessen, PWaendern, Helper, FMX.Platform, FMX.Objects;

type
  TFormMain = class(TForm)
    TabControl1: TTabControl;
    ProfilTab: TTabItem;
    HomeTab: TTabItem;
    GridPanelLayout1: TGridPanelLayout;
    LblHome: TLabel;
    LBLists: TListBox;
    GridPanelLayout2: TGridPanelLayout;
    LblUsername: TLabel;
    LblAllergien: TLabel;
    ListBox2: TListBox;
    SearchBox1: TSearchBox;
    LblAbmelden: TListBoxItem;
    ImgAdd: TImage;
    ImgEdit: TImage;
    Line1: TLine;
    Timer: TTimer;
    // Sonstiges
    procedure FormCreate(Sender: TObject);
    procedure listFormClose(Sender: TObject; var Action: TCloseAction);
    procedure TimerTimer(Sender: TObject);
    procedure tryLogin();

    // Home Seite
    procedure LBListItemClick(Sender: TObject);
    procedure ImgAddClick(Sender: TObject);
    procedure UpdateTitle(online: Boolean);

    // Profil Seite
    procedure LBIUserLoeschenClick(Sender: TObject);
    procedure LBIPasswortaendernClick(Sender: TObject);
    procedure LblAbmeldenClick(Sender: TObject);
    procedure ImgEditClick(Sender: TObject);

  private
    { Private declarations }
  public
    //* Ob der User sich abgemeldet hat
    loggedOut: Boolean;
    procedure UpdateLists();
    procedure UpdateUserData();
  end;

var
  //* Die Hauptform
  MainForm: TFormMain;
  //* Die ServerAPI, durch die die Kommunikation mit dem Server durchgeführt wird.
  serverAPI: TServerAPI;
  //* Die Listen des Users
  lists: TListArray;
  //* Die Daten über den User
  user: TUserData;
  //* Ein Zähler, damit  für die verschiedenen Aktionen nur ein Timer gebraucht wird.
  timerCounter: Integer;

implementation

{$R *.fmx}

//////////////////////////////
// Home Seite Buttons, etc. //
//////////////////////////////

{*
  Aktualisiert das Label auf der Home Seite

  Wenn der übergebene Bool true ist, dann wird der Text des Labels auf Home
  gesetzt.

  Wenn der übergebene Bool false ist, dann wird der Text des Labels auf Offline
  gesetzt.

  @param online Ist die Anwendung aktuell mit dem Server verbunden
}
procedure TFormMain.UpdateTitle(online: Boolean);
begin
  if not online then
    begin
      LblHome.Text := 'Offline';
      LblHome.TextSettings.FontColor := TAlphaColors.Crimson;
    end
    else
    begin
      LblHome.Text := 'Home';
      LblHome.TextSettings.FontColor := TAlphaColors.Black;
    end;
end;

{*
  Liste hinzufügen

  Es wird eine Liste mit dem Namen "Neue Liste" hinzugefügt und die Listen auf
  der Hauptseite werden aktualisiert.

  @param Sender Der Button um eine Liste hinzuzufügen
}
procedure TFormMain.ImgAddClick(Sender: TObject);
begin
  if serverAPI.isOnline then
  begin
    serverAPI.AddList('Neue Liste');
    UpdateLists();
  end
  else
    ShowMessage('Du brauchst eine aktive Internetverbindung für diese Aktion!');
end;

{*
  Die Listen auf der Hauptseite werden aktualisiert

  Wenn sich etwas an den Listen geändert hat, so wird die Liste auf der
  Hauptseite aktualisiert. Ist die Anwendung nicht mit dem Server verbunden, so
  werden offline Daten verwendet.
}
procedure TFormMain.UpdateLists();
var
  i: Integer;
  item: TListBoxItem;
begin
  if lists <> serverAPI.getCachedLists then
  begin
    LBLists.Items.Clear;
    lists := serverAPI.getCachedLists;
    for i := 0 to High(lists) do
    begin
      item := TListBoxItem.Create(LBLists);
      item.Text := lists[i].name;
      item.ItemData.Accessory := TListBoxItemData.TAccessory(1);
      item.ItemData.Detail := IntToStr(i);
      item.OnClick := LBListItemClick;
      {$IF defined(MSWINDOWS)}
        item.Height:=25;
      {$ENDIF}
      LBLists.AddObject(item);
    end;
  end;
end;

{*
  Ein ListBoxItem wird geklickt

  Wenn der User auf eine Liste auf der Hauptseite klickt, so wird diese geöffnet

  @param Sender Das ListBoxItem auf das geklickt wurde
}
procedure TFormMain.LBListItemClick(Sender: TObject);
var
  listForm: TFormListe;
begin
  listForm := TFormListe.Create(Application, serverAPI, lists[StrToInt((sender as TListBoxItem).ItemData.Detail)]);
  listForm.Show;
  listForm.OnClose := listFormClose;
end;



///////////////////////////////
// Profil Seite Button, etc. //
///////////////////////////////

{*
  Allergien ändern

  Nachdem der User diesen Button geklickt hat, sieht er eine Input Box in die er
  neue Allergien eintragen kann. In dem Input Feld stehen die bereits
  eingetragenen allergien.

  @param Sender Der gelickte Button
}
procedure TFormMain.ImgEditClick(Sender: TObject);
var
  dialogService: IFMXDialogServiceAsync;
begin
  if serverAPI.isOnline then
    if TPlatformServices.Current.SupportsPlatformService (IFMXDialogServiceAsync, IInterface (dialogService)) then
    begin
      // Bei dieser InputQuery funktioniert unter Windows und MacOS der Cancel Button nicht.
      // Diese Software ist nur zur Benutzung unter iOS und Android gedacht.
      // Dort funktioniert die Funktion einwandfrei. Soll diese Funktion dennoch unter
      // Windows ausgeführt werden kann entweder nur der OK-Button gedrückt werden
      // oder der im Embarcadero Bug System vorgeschlagene Fix angewandt werden.
      // Dieser ist hier zu finden: https://quality.embarcadero.com/browse/RSP-16670
      dialogService.InputQueryAsync('Allergien', ['Neuer Eintrag'], [user.allergies],
        procedure (const AResult : TModalResult; const AValues : array of string)
        begin
            case AResult of
              mrOk:
                begin
                  serverAPI.editInfo(AValues[0]);
                  UpdateUserData();
                end;
            end;
        end
      );
    end
  else
    ShowMessage('Du brauchst eine aktive Internetverbindung für diese Aktion!');
end;

{*
  Passwort ändern

  Die Passwort-Ändern-Form wird geöffnet.

  @param Sender Der Button um sein Passwort zu ändern
}
procedure TFormMain.LBIPasswortaendernClick(Sender: TObject);
var
  PWAendernForm: TForm;
begin
  PWAendernForm := TFormPWaendern.Create(Application, serverAPI);
  PWAendernForm.Show;
end;

{*
  User löschen

  Nach einer erneuten Nachfrage wird der Account des Users gelöscht.

  @param Sender Der Button um den user zu löschen.
}
procedure TFormMain.LBIUserLoeschenClick(Sender: TObject);
begin
  if serverAPI.isOnline then
  begin
    MessageDlg('Wollen Sie den Account wirklich löschen?', System.UITypes.TMsgDlgType.mtCustom,
    [ System.UITypes.TMsgDlgBtn.mbYes,
      System.UITypes.TMsgDlgBtn.mbNo,
      System.UITypes.TMsgDlgBtn.mbCancel
    ],0,
    procedure (const AResult:System.UITypes.TModalResult)
    begin
      case AResult of
        mrYES:
        if UMain.serverAPI.deleteUser()='"Deleted user"' then
          begin
            ShowMessage('Der User wurde gelöscht!');
            loggedOut := true;
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
  User abmelden

  Alle offline gespeicherten Daten werden zurückgesetzt und der User wird auf
  die Startseite weitergeleitet.

  @param Sender Button zum abmelden
}
procedure TFormMain.LblAbmeldenClick(Sender: TObject);
var
  offlineData: TOfflineData;
begin
  offlineData.worked := false;
  offlineData.email := '';
  offlineData.password := '';
  offlineData.lists := '[]';
  saveOfflineData(offlineData);
  ShowMessage('Du wurdest erfolgreich abgemeldet.');
  loggedOut := true;
  Close;
  Release;
end;

{*
  Die Userdaten werden aktualisiert

  Der Name und die eingetragenen Allergien des Users werden auf die Profilseite
  geschrieben.

  Ist die Anwendung offline, so wird der Name durch 'Offline' ersetzt. Bei der
  nächsten Verbindung wird dies dann aktualisiert.
}
procedure TFormMain.UpdateUserData();
begin
  if serverAPI.isOnline then
  begin
    user := serverAPI.me();
    if user.allergies <> '' then
      LblAllergien.Text := user.allergies;
    LblUsername.Text := user.name;
  end
  else
  begin
    user.name := 'Offline';
    LblUsername.Text := 'Offline';
    LblAllergien.Text := '';
  end;
end;



///////////////
// Sonstiges //
///////////////

{*
  Neuer Initializer für die Main Form

  Hier wird die server api und der timer counter für Hintergrundupdates
  initialisiert.

  @param Sender Ersteller der Form
}
procedure TFormMain.FormCreate(Sender: TObject);
begin
  serverAPI := TServerAPI.create();
  timerCounter := 19;
end;

{*
  Eine TFormListe wird geschlossen

  Diese Funktion wird aufgerufen, wenn eine, hier erstellte, ListForm
  geschlossen wird, um die Liste an Listen zu aktualisieren, für den Fall, dass
  sich ein Name geändert hat.

  @param Sender Die List Form die geschlossen wurde
  @param Action Die CloseAction die ausgeführt wird
}
procedure TFormMain.listFormClose(Sender: TObject; var Action: TCloseAction);
begin
  UpdateLists();
end;

{*
  Der Timer feuert

  Hier wird überprüft, ob noch eine Verbindung zum Server besteht und, sollte
  dies der Fall sein, die Listen aktualisiert, um auf Änderungen von anderen
  Usern zu reagieren.

  Außerdem werden die Userdaten aktuelisiert, sollte dies noch nicht geschehen
  sein.

  @param Sender Das Timer Objekt
}
procedure TFormMain.TimerTimer(Sender: TObject);
var
  offlineData: TOfflineData;
begin
  timerCounter := timerCounter + 1;
  if timerCounter = 20 then
  begin
    timerCounter := 0;
    UpdateTitle(serverAPI.isOnline);
    serverAPI.getLists;
    if user.name = 'Offline' then
    begin
      tryLogin;
    end;
  end
  else if timerCounter = 10 then
  begin
    UpdateTitle(serverAPI.isOnline);
    if user.name = 'Offline' then
    begin
      tryLogin;
    end;
  end;
  UpdateLists;
end;

{*
  Versucht den User anzumelden

  Lädt die Logindaten aus dem Offlinestorage und versucht den User anzumelden.
  Schlägt dies Fehl, wird der User auf die Hauptseite weitergeleitet.
}
procedure TFormMain.tryLogin();
var
  offlineData: TOfflineData;
begin
  if serverAPI.isOnline then
  begin
    offlineData := getOfflineData;
    if interpretServerResponse(serverAPI.login(offlineData.email, offlineData.password)) then
    begin
      UpdateUserData;
      serverAPI.getLists;
    end
    else
    begin
      loggedOut := true;
      Close;
      Release;
    end;
  end;
end;

end.

