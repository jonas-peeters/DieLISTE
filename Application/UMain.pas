unit UMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.ScrollBox, FMX.Memo, serverAPI,
  FMX.TabControl, FMX.Layouts, FMX.ListBox, Liste;

type
  TForm6 = class(TForm)
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
    procedure FormCreate(Sender: TObject);
    procedure PlusBtn2Click(Sender: TObject);
  private
    { Private declarations }
  public
    procedure UpdateLists();
  end;

var
  Form6: TForm6;
  serverAPI: TServerAPI;

implementation

{$R *.fmx}

procedure TForm6.FormCreate(Sender: TObject);
begin
  serverAPI := TServerAPI.create();
end;

procedure TForm6.PlusBtn2Click(Sender: TObject);
begin
  serverAPI.AddList('Neue Liste');
  UpdateLists();
end;

Procedure TForm6.UpdateLists();
var
  response: String;
  responseAsArray: TArray;
  i: Integer;
  listItem: TListBoxItem;
begin
  response := serverAPI.getLists();
  responseAsArray := serverAPI.jsonArrayToArray(response);
  for i := 0 to High(responseAsArray)-1 do
  begin
    LBLists.Items.Append(responseAsArray[i]);
  end;
end;

end.
