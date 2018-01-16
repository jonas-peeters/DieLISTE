unit UMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.ScrollBox, FMX.Memo, serverAPI,
  FMX.TabControl, FMX.Layouts, FMX.ListBox;

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
    ListBox1: TListBox;
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
    LBIUserLöschen: TListBoxItem;
    LBIPasswortvergessen: TListBoxItem;
    LBIPasswortändern: TListBoxItem;
    Label5: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure LBIUserLöschenClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
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

procedure TForm6.LBIUserLöschenClick(Sender: TObject);
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
        Form6.CloseModal;
      end;
  end;
end);
end;

end.

