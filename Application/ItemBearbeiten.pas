{*
  The user can edit an item on this form.

  Possible actions include:
  - Changing the name
  - Changing the quantity
  - Changing the category
  - Changing the done/not done status
  - Deleting the item
}
unit ItemBearbeiten;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Controls.Presentation, FMX.Layouts, serverAPI, FMX.ListBox,
  Helper;

type
  TFormItemBearbeiten = class(TForm)
    GridPanelLayout1: TGridPanelLayout;
    LblBearbeiten: TLabel;
    EdtName: TEdit;
    EdtEinheit: TEdit;
    EdtMenge: TEdit;
    BtnOK: TButton;
    BtnSchliessen: TButton;
    CBCategory: TComboBox;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    ListBoxItem4: TListBoxItem;
    ListBoxItem5: TListBoxItem;
    ListBoxItem6: TListBoxItem;
    ListBoxItem7: TListBoxItem;
    ListBoxItem8: TListBoxItem;
    ListBoxItem9: TListBoxItem;
    BtnBack: TButton;
    BtnErledigt: TButton;
    BtnLoeschen: TButton;
    procedure BtnSchliessenClick(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    constructor Create(AOwner: TComponent; var serverAPI: TServerAPI; item: TItem);
    procedure BtnBackClick(Sender: TObject);
    procedure BtnLoeschenClick(Sender: TObject);
    procedure BtnErledigtClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FormItemBearbeiten: TFormItemBearbeiten;
  privateServerAPI: TServerAPI;
  itemToChange: TItem;
  list:Tliste;

implementation

{$R *.fmx}
constructor TFormItemBearbeiten.Create(AOwner: TComponent; var serverAPI: TServerAPI; item: TItem);
var
  i: Integer;
begin
  inherited Create(AOwner);
  privateServerAPI := serverAPI;
  itemToChange := item;
  EdtName.Text := item.name;
  EdtMenge.Text := item.quantity.Split([' '])[0];
  for i := 1 to High(item.quantity.Split([' '])) do
    EdtEinheit.Text := EdtEinheit.Text + ' ' + item.quantity.Split([' '])[i];
  CBCategory.Index := item.categoryId;
end;

procedure TFormItemBearbeiten.BtnBackClick(Sender: TObject);
begin
  Close;
  Release;
end;

procedure TFormItemBearbeiten.BtnErledigtClick(Sender: TObject);
var
  name, einheit, menge, erledigt: string;
  kategorie: Integer;
begin
    interpretServerResponse(privateServerAPI.DeleteItem(itemToChange.itemId));
    name := EdtName.Text;
    einheit := EdtEinheit.Text;
    menge := EdtMenge.Text;
    erledigt:= btnerledigt.Text;
    kategorie := CBCategory.Index;
    interpretServerResponse(privateServerAPI.AddToList(name, menge + ' ' + einheit + '      '+  erledigt, false, kategorie, itemToChange.listId));
    Close;
    Release;
end;

procedure TFormItemBearbeiten.BtnLoeschenClick(Sender: TObject);
begin
   begin
MessageDlg('Wollen Sie das Item löschen?', System.UITypes.TMsgDlgType.mtCustom,
[ System.UITypes.TMsgDlgBtn.mbYes,
  System.UITypes.TMsgDlgBtn.mbNo,
  System.UITypes.TMsgDlgBtn.mbCancel
],0,
procedure (const AResult:System.UITypes.TModalResult)
begin
  case AResult of
    mrYES:
      begin
      privateServerAPI.deleteitem(itemToChange.itemId);
      ShowMessage('Das Item wurde gelöscht!');
      Close;
      Release;
      end;
  end;;
end);
end;
end;

procedure TFormItemBearbeiten.BtnOKClick(Sender: TObject);
var
  name, einheit, menge: string;
  kategorie: Integer;
begin
    interpretServerResponse(privateServerAPI.DeleteItem(itemToChange.itemId));
    name := EdtName.Text;
    einheit := EdtEinheit.Text;
    menge := EdtMenge.Text;
    kategorie := CBCategory.Index;
    interpretServerResponse(privateServerAPI.AddToList(name, menge + ' ' + einheit, false, kategorie, itemToChange.listId));
    Close;
    Release;
end;


procedure TFormItemBearbeiten.BtnSchliessenClick(Sender: TObject);
begin
  Close;
  Release;
end;

end.
