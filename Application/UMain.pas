unit UMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.ScrollBox, FMX.Memo, serverAPI,
  FMX.TabControl, FMX.Layouts, FMX.ListBox, Liste, JSON, FMX.Edit, FMX.SearchBox,
  Windows;

type
  TFormMain = class(TForm)
    TabControl1: TTabControl;
    ProfilTab: TTabItem;
    HomeTab: TTabItem;
    SupermaerkteTab: TTabItem;
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
    GridPanelLayout3: TGridPanelLayout;
    Button1: TButton;
    Label4: TLabel;
    ListBox3: TListBox;
    Button2: TButton;
    SearchBox1: TSearchBox;
    procedure FormCreate(Sender: TObject);
    procedure PlusBtn2Click(Sender: TObject);
    procedure LBListsClick(Sender: TObject);
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

end.
