{*
  Item-Bearbeiten-Form

  Der User kann ein Item bearbeiten.

  Mögliche Aktionen sind z.B.:
  - Namen ändern
  - Menge ändern
  - Einheit ändern
  - Kategory ändern
  - Erledigt status ändern
  - Löschen
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
  //* Die Item-Bearbeiten-Form
  FormItemBearbeiten: TFormItemBearbeiten;
  //* Private Instanz der Server API in der der USer angemeldet ist
  privateServerAPI: TServerAPI;
  //* Das Item, welches bearbeitet wird
  itemToChange: TItem;
  //* Die Liste in der das Item ist
  list: Tliste;

implementation

{$R *.fmx}

{*
  Neuer Konstruktor

  Neuer Konstruktor, um eine Instanz der Server API und das zu ändernde Item zu
  übergeben. Außerdem werden alle Eigenschaften des Items wieder in die
  ensprechenden Felder eingetragen.

  @param AOwner Der Parent der Form
  @param serverAPI Instanz der Server API in der der USer angemeldet ist
  @param item Das Item, welches bearbeitet wird
}
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

{*
  Zurück

  Der User wird auf die Liste-Anzeigen-Form weitergeleitet.

  @param Sender Button um zurückzukehren
}
procedure TFormItemBearbeiten.ImgBackClick(Sender: TObject);
begin
  Close;
  Release;
end;

{*
  Erledigt

  Das Item wird als erledigt makiert und der User auf die Liste-Anzeigen-Form
  weitergeleitet.
  Andere Änderungen werden auch gepeichert.

  @param Sender Button um das Item als erledigt zu markieren.
}
procedure TFormItemBearbeiten.BtnErledigtClick(Sender: TObject);
var
  name, einheit, menge: string;
  kategorie: Integer;
  erledigt: Boolean;
begin
  if checkForInvalidCharacters(EdtName)
    AND checkForInvalidCharacters(EdtEinheit)
    AND checkForInvalidCharacters(EdtMenge) then
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
end;

{*
  Löschen

  Das Item wird gelöscht und der User auf die Liste-Anzeigen-Form
  weitergeleitet.

  @param Sender Button um das Item zu löschen.
}
procedure TFormItemBearbeiten.BtnLoeschenClick(Sender: TObject);
begin
  if checkForInvalidCharacters(EdtName)
    AND checkForInvalidCharacters(EdtEinheit)
    AND checkForInvalidCharacters(EdtMenge) then
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
end;

{*
  OK

  Alle Änderungen an dem Item werden gespeichert und der User auf die
  Liste-Anzeigen-Form weitergeleitet.

  @param Sender OK-Button
}
procedure TFormItemBearbeiten.BtnOKClick(Sender: TObject);
var
  name, einheit, menge: string;
  kategorie: Integer;
begin
  if checkForInvalidCharacters(EdtName)
    AND checkForInvalidCharacters(EdtEinheit)
    AND checkForInvalidCharacters(EdtMenge) then
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
end;

{*
  Zurück

  Der User wird auf die Liste-Anzeigen-Form weitergeleitet.

  @param Sender Button um zurückzukehren
}
procedure TFormItemBearbeiten.BtnSchliessenClick(Sender: TObject);
begin
  Close;
  Release;
end;

end.
