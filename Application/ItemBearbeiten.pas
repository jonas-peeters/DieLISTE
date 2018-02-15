unit ItemBearbeiten;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Controls.Presentation, FMX.Layouts, serverAPI, FMX.ListBox;

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
    procedure BtnHinzufuegenClick(Sender: TObject);
    procedure BtnBackClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FormItemBearbeiten: TFormItemBearbeiten;
  privateServerAPI: TServerAPI;
  itemToChange: TItem;

implementation

{$R *.fmx}
constructor TFormItemBearbeiten.Create(AOwner: TComponent; var serverAPI: TServerAPI; item: TItem);
begin
  inherited Create(AOwner);
  privateServerAPI := serverAPI;
  itemToChange := item;
  EdtName.Text:= item.name;
  EdtMenge.Text:= item.quantity.Split([' '])[0];
  EdtEinheit.Text:=item.quantity.Split([' '])[1];
  CBCategory.itemindex:=item.categoryId;
end;

procedure TFormItemBearbeiten.BtnBackClick(Sender: TObject);
begin
  Release;
end;

procedure TFormItemBearbeiten.BtnOKClick(Sender: TObject);
var
  name, einheit, menge: string;
  kategorie: Integer;
begin
    privateServerAPI.DeleteItem();
    name:= EdtName.Text;
    einheit:= EdtEinheit.Text;
    menge:= EdtMenge.Text;
    kategorie := CBCategory.ItemIndex;
    privateServerAPI.AddToList(name, menge + einheit, false, kategorie, itemToChange.listId);
    Release;
end;

procedure TFormItemBearbeiten.BtnSchliessenClick(Sender: TObject);
begin
 Release;
end;

end.
