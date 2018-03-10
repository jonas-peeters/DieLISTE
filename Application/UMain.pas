{*
  The main window of the application

  Here the user can see all their lists and edit their personal data.
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
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure ImgAddClick(Sender: TObject);
    procedure LBListItemClick(Sender: TObject);
    procedure LBIUserLoeschenClick(Sender: TObject);
    procedure LBIPasswortaendernClick(Sender: TObject);
    procedure listFormClose(Sender: TObject; var Action: TCloseAction);
    procedure LblAbmeldenClick(Sender: TObject);
    procedure ImgEditClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure UpdateTitle(online: Boolean);
  private
    { Private declarations }
  public
    procedure UpdateLists();
    procedure UpdateUserData();
  end;

var
  MainForm: TFormMain;
  serverAPI: TServerAPI;
  lists: TListArray;
  user: TUserData;
  timerCounter: Integer;

implementation

{$R *.fmx}

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
              else
                begin
                end;
            end;
        end
      );
    end
  else
    ShowMessage('Du brauchst eine aktive Internetverbindung für diese Aktion!');
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  serverAPI := TServerAPI.create();
  timerCounter := 9;
end;

procedure TFormMain.listFormClose(Sender: TObject; var Action: TCloseAction);
begin
  UpdateLists();
end;

procedure TFormMain.Timer1Timer(Sender: TObject);
var
  offlineData: TOfflineData;
begin
  offlineData := getOfflineData;
  timerCounter := timerCounter + 1;
  if timerCounter = 20 then
  begin
    timerCounter := 0;
    UpdateTitle(serverAPI.isOnline);
    serverAPI.getLists;
  end
  else if timerCounter = 10 then
  begin
    UpdateTitle(serverAPI.isOnline);
  end;
  UpdateLists;
end;

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

procedure TFormMain.LBListItemClick(Sender: TObject);
var
  listForm: TFormListe;
begin
  listForm := TFormListe.Create(Application, serverAPI, lists[StrToInt((sender as TListBoxItem).ItemData.Detail)]);
  listForm.Show;
  listForm.OnClose := listFormClose;
end;

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
    LblUsername.Text := 'Offline';
    LblAllergien.Text := '';
  end;
end;

procedure TFormMain.LBIPasswortaendernClick(Sender: TObject);
var
  PWAendernForm: TForm;
begin
  PWAendernForm := TFormPWaendern.Create(Application, serverAPI);
  PWAendernForm.Show;
end;

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
            Release;
          end;
      end;
    end);
  end
  else
    ShowMessage('Du brauchst eine aktive Internetverbindung für diese Aktion!');
end;

procedure TFormMain.LblAbmeldenClick(Sender: TObject);
var
  offlineData: TOfflineData;
begin
  offlineData.worked := false;
  offlineData.email := '';
  offlineData.password := '';
  offlineData.lists := '[]';
  saveOfflineData(offlineData);
  Close;
  ShowMessage('Du wurdest erfolgreich abgemeldet.');
  Release;
end;

end.

