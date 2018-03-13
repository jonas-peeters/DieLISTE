{*
  Item-Hinzufügen-Form

  Der User kann auf dieser Seite Item zu einer Liste hinzufügen.
}
unit Hinzufuegen;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Controls.Presentation, FMX.Layouts, serverAPI, FMX.ListBox,
  FMX.Objects, Helper;

type
  TFormHinzufuegen = class(TForm)
    GridPanelLayout1: TGridPanelLayout;
    LblHinzufuegen: TLabel;
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
    PanelOK: TPanel;
    LabelOK: TLabel;
    PanelAbbrechen: TPanel;
    LabelAbbrechen: TLabel;
    PanelWeitere: TPanel;
    LabelWeitere: TLabel;
    ImgBack: TImage;
    procedure BtnSchliessenClick(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    constructor Create(AOwner: TComponent; var serverAPI: TServerAPI; selectedListId: Integer);
    procedure BtnHinzufuegenClick(Sender: TObject);
    procedure ImgBackClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  //* Die Item-Hinzufügen-Form
  HinzufuegenForm: TFormHinzufuegen;
  //* Private Instanz der Server API in der der User angemeldet ist
  privateServerAPI: TServerAPI;
  //* Id der Liste zu der das Item hinzugefügt werde soll
  listId: Integer;

implementation

{$R *.fmx}

{*
  Neuer Konstruktor

  Neuer Konstruktor, um der Form die Server API und die Id der Liste, zu der das
  Item hinzufügt werden soll, zu übergeben.

  @param AOwner Der Parent der Form
  @param serverAPI Private Instanz der Server API in der der User angemeldet ist
  @param selectedListId Id der Liste zu der das Item hinzugefügt werden soll
}
constructor TFormHinzufuegen.Create(AOwner: TComponent; var serverAPI: TServerAPI; selectedListId: Integer);
begin
  inherited Create(AOwner);
  privateServerAPI := serverAPI;
  listId := selectedListId;
end;

{*
  Zurück

  Der User wird auf die Liste-Anzeigen-Form weitergeleitet.

  @param Sender Button um zurückzukehren
}
procedure TFormHinzufuegen.ImgBackClick(Sender: TObject);
begin
  Close;
  Release;
end;

{*
  Weiteres Item hinzufügen

  Das Item wird der Liste hinzugefügt und die Eingabefelder werden
  zurückgesetzt, damit der User direkt noch ein weiteres Item hinzufügen kann.

  @param Sender Button um ein weitere Item hinzuzufügen
}
procedure TFormHinzufuegen.BtnHinzufuegenClick(Sender: TObject);
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
      name:= EdtName.Text;
      einheit:= EdtEinheit.Text;
      menge:= EdtMenge.Text;
      kategorie := CBCategory.ItemIndex + 1;
      privateServerAPI.AddToList(name, menge + ' ' + einheit, false, kategorie, listId);
      EdtName.Text := '';
      EdtEinheit.Text := '';
      EdtMenge.Text := '';
      CBCategory.ItemIndex := 0;
    end
    else
      ShowMessage('Du brauchst eine aktive Internetverbindung für diese Aktion!');
  end;
end;

{*
  OK

  Das Item wird hinzugefügt und der User auf die List-Anzeigen Seite
  weitergeleitet.

  @param Sender OK-Button
}
procedure TFormHinzufuegen.BtnOKClick(Sender: TObject);
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
      name:= EdtName.Text;
      einheit:= EdtEinheit.Text;
      menge:= EdtMenge.Text;
      kategorie := CBCategory.ItemIndex + 1;
      privateServerAPI.AddToList(name, menge + ' ' + einheit, false, kategorie, listId);
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
procedure TFormHinzufuegen.BtnSchliessenClick(Sender: TObject);
begin
  Close;
  Release;
end;

end.
