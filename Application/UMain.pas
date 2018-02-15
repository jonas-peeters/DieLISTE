unit UMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.ScrollBox, FMX.Memo, serverAPI,
  FMX.TabControl, FMX.Layouts, FMX.ListBox, Liste, JSON, FMX.Edit, FMX.SearchBox,
  PWvergessen, PWaendern;

type
  TFormMain = class(TForm)
    TabControl1: TTabControl;
    ProfilTab: TTabItem;
    HomeTab: TTabItem;
    GridPanelLayout1: TGridPanelLayout;
    PlusBtn2: TButton;
    Label1: TLabel;
    EditBtn2: TButton;
    LBLists: TListBox;
    GridPanelLayout2: TGridPanelLayout;
    Label2: TLabel;
    Label3: TLabel;
    EditButton: TButton;
    ListBox2: TListBox;
    SearchBox1: TSearchBox;
    procedure FormCreate(Sender: TObject);
    procedure PlusBtn2Click(Sender: TObject);
    procedure LBListsClick(Sender: TObject);
    procedure LBIUserLoeschenClick(Sender: TObject);
    procedure LBIPasswortaendernClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure UpdateLists();
  end;

var
  MainForm: TFormMain;
  serverAPI: TServerAPI;
  lists: TListArray;

implementation

{$R *.fmx}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  serverAPI := TServerAPI.create();
end;

procedure TFormMain.LBListsClick(Sender: TObject);
var
  listForm: TFormListe;
begin
  if LBLists.Selected.Index <> -1 then
  begin
    listForm := TFormListe.Create(Application, serverAPI, lists[LBLists.ItemIndex]);
    listForm.Show;
    UpdateLists();
  end;
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
    LBLists.AddObject(item);
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


end.

