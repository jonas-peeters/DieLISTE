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
  Helper, StrUtils, FMX.Colors, FMX.Objects;

type
  TFormItemBearbeiten = class(TForm)
    GridPanelLayout1: TGridPanelLayout;
    LblBearbeiten: TLabel;
    EdtName: TEdit;
    EdtEinheit: TEdit;
    EdtMenge: TEdit;
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
    PanelErledigt: TPanel;
    LabelErledigt: TLabel;
    PanelLoeschen: TPanel;
    LabelLoeschen: TLabel;
    PanelAbbrechen: TPanel;
    LabelAbbrechen: TLabel;
    PanelOK: TPanel;
    LabelOK: TLabel;
    ImgBack: TImage;
    procedure BtnSchliessenClick(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    constructor Create(AOwner: TComponent; var serverAPI: TServerAPI; item: TItem);
    procedure ImgBackClick(Sender: TObject);
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
  if High(item.quantity.Split([' '])) > 0 then
    EdtEinheit.Text := item.quantity.Split([' '])[1];
  if High(item.quantity.Split([' '])) > 1 then
    for i := 2 to High(item.quantity.Split([' '])) do
      EdtEinheit.Text := EdtEinheit.Text + ' ' + item.quantity.Split([' '])[i];
  CBCategory.ItemIndex := item.categoryId - 1;

  if item.done then
    LabelErledigt.Text:= 'Nicht erledigt'
  else
    LabelErledigt.Text:= 'Erledigt'

end;

procedure TFormItemBearbeiten.ImgBackClick(Sender: TObject);
begin
  Close;
  Release;
end;

procedure TFormItemBearbeiten.BtnErledigtClick(Sender: TObject);
var
  name, einheit, menge: string;
  kategorie: Integer;
  erledigt: Boolean;
begin
  if privateServerAPI.isOnline then
  begin
    interpretServerResponse(privateServerAPI.DeleteItem(itemToChange.itemId));
    name := EdtName.Text;
    einheit := EdtEinheit.Text;
    menge := EdtMenge.Text;
    kategorie := CBCategory.ItemIndex + 1;
    if itemToChange.done then
      erledigt := false
    else
      erledigt := true;
    interpretServerResponse(privateServerAPI.AddToList(name, menge + ' ' + einheit, erledigt, kategorie, itemToChange.listId));
    Close;
    Release;
  end
  else
    ShowMessage('Du brauchst eine aktive Internetverbindung für diese Aktion!');
end;

procedure TFormItemBearbeiten.BtnLoeschenClick(Sender: TObject);
begin
  if privateServerAPI.isOnline then
  begin
    MessageDlg('Willst du das Item löschen?', System.UITypes.TMsgDlgType.mtCustom,
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
          Close;
          Release;
          end;
      end;;
    end);
  end
  else
    ShowMessage('Du brauchst eine aktive Internetverbindung für diese Aktion!');
end;

procedure TFormItemBearbeiten.BtnOKClick(Sender: TObject);
var
  name, einheit, menge: string;
  kategorie: Integer;
begin
  if privateServerAPI.isOnline then
  begin
    interpretServerResponse(privateServerAPI.DeleteItem(itemToChange.itemId));
    name := EdtName.Text;
    einheit := EdtEinheit.Text;
    menge := EdtMenge.Text;
    kategorie := CBCategory.ItemIndex + 1;
    interpretServerResponse(privateServerAPI.AddToList(name, menge + ' ' + einheit, itemToChange.done, kategorie, itemToChange.listId));
    Close;
    Release;
  end
  else
    ShowMessage('Du brauchst eine aktive Internetverbindung für diese Aktion!');
end;


procedure TFormItemBearbeiten.BtnSchliessenClick(Sender: TObject);
begin
  Close;
  Release;
end;

end.
