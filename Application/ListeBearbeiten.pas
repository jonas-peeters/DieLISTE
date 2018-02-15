unit ListeBearbeiten;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.StdCtrls, FMX.ListBox, FMX.Controls.Presentation, serverAPI;

type
  TFormListeBearbeiten = class(TForm)
    GridPanelLayout1: TGridPanelLayout;
    BtnBack: TButton;
    ListBox1: TListBox;
    Label1: TLabel;
    LBIEditListName: TListBoxItem;
    LBIDeleteList: TListBoxItem;
    LBIAddUser: TListBoxItem;
    SettingsGroupHeader: TListBoxGroupHeader;
    GroupHeaderUser: TListBoxGroupHeader;
    procedure BtnBackClick(Sender: TObject);
    procedure LBIEditListNameClick(Sender: TObject);
    procedure LBIDeleteListClick(Sender: TObject);
    constructor Create(AOwner: TComponent; var serverAPI: TServerAPI; selectedListId: Integer;selectedListName: String);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FormListeBearbeiten: TFormListeBearbeiten;
  list: TListe;
  listId: Integer;
  privateServerAPI: TServerAPI;

implementation

{$R *.fmx}

constructor TFormListeBearbeiten.Create(AOwner: TComponent; var serverAPI: TServerAPI; selectedListId: Integer; selectedListName:String);
begin
  inherited Create(AOwner);
  privateServerAPI := serverAPI;
  listId := selectedListId;
  list.name:=selectedListName;
  LBIEditListName.Text:=list.name+'(Zum ändern klicken)';
end;

procedure TFormListeBearbeiten.BtnBackClick(Sender: TObject);
begin
  Release;
end;

procedure TFormListeBearbeiten.LBIDeleteListClick(Sender: TObject);
begin
MessageDlg('Wollen Sie die Liste wirklich löschen?', System.UITypes.TMsgDlgType.mtCustom,
[ System.UITypes.TMsgDlgBtn.mbYes,
  System.UITypes.TMsgDlgBtn.mbNo,
  System.UITypes.TMsgDlgBtn.mbCancel
],0,
procedure (const AResult:System.UITypes.TModalResult)
 var
 item: TListBoxItem;
begin
  case AResult of
    mrYES:
      begin
      privateServerAPI.removeList(listId);
      ShowMessage('Die Liste wurde gelöscht!');
      Release;
      end;
  end;
end);
end;

procedure TFormListeBearbeiten.LBIEditListNameClick(Sender: TObject);
var neuerName:String;
begin
  repeat
    if not InputQuery('Namen ändern', 'Neuer Name:', neuerName) then
      neuerName := list.name
  until neuerName <> '';
  if neuerName <> list.name then
  begin
    privateServerAPI.ChangeListName(neuerName, listId);
  end;
end;

end.
