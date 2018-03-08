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
  PWvergessen, PWaendern, Helper, FMX.Platform;

type
  TFormMain = class(TForm)
    TabControl1: TTabControl;
    ProfilTab: TTabItem;
    HomeTab: TTabItem;
    GridPanelLayout1: TGridPanelLayout;
    PlusBtn2: TButton;
    Label1: TLabel;
    LBLists: TListBox;
    GridPanelLayout2: TGridPanelLayout;
    LblUsername: TLabel;
    LblAllergien: TLabel;
    EditButton: TButton;
    ListBox2: TListBox;
    SearchBox1: TSearchBox;
    LblAbmelden: TListBoxItem;
    procedure FormCreate(Sender: TObject);
    procedure PlusBtn2Click(Sender: TObject);
    procedure LBListItemClick(Sender: TObject);
    procedure LBIUserLoeschenClick(Sender: TObject);
    procedure LBIPasswortaendernClick(Sender: TObject);
    procedure listFormClose(Sender: TObject; var Action: TCloseAction);
    procedure LblAbmeldenClick(Sender: TObject);
    procedure EditButtonClick(Sender: TObject);
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

implementation

{$R *.fmx}

procedure TFormMain.EditButtonClick(Sender: TObject);
var
  dialogService: IFMXDialogServiceAsync;
begin
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
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  serverAPI := TServerAPI.create();
end;

procedure TFormMain.listFormClose(Sender: TObject; var Action: TCloseAction);
begin
  UpdateLists();
end;

procedure TFormMain.LBListItemClick(Sender: TObject);
var
  listForm: TFormListe;
begin
  listForm := TFormListe.Create(Application, serverAPI, lists[StrToInt((sender as TListBoxItem).ItemData.Detail)]);
  listForm.Show;
  listForm.OnClose := listFormClose;
end;

procedure TFormMain.PlusBtn2Click(Sender: TObject);
begin
  serverAPI.AddList('Neue Liste');
  UpdateLists();
end;

procedure TFormMain.UpdateLists();
var
  i: Integer;
  item: TListBoxItem;
begin
  LBLists.Items.Clear;
  lists := serverAPI.getLists();
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

procedure TFormMain.UpdateUserData();
begin
  user := serverAPI.me();
  if user.allergies <> '' then
    LblAllergien.Text := user.allergies;
  LblUsername.Text := user.name;
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
end;

procedure TFormMain.LblAbmeldenClick(Sender: TObject);
var
  loginData: TLoginData;
begin
  loginData.worked := false;
  saveLoginData(loginData);
  Close;
  Release;
end;

end.

