{*
  Liste-Anzeigen-Form

  Hier sieht der User eine einzelne Liste mit allen darin enthaltenen Items,
  kann neue hinzufügen und zu den Seiten zum Bearbeiten der Liste und einzelner
  Gegenstände gelangen.
}
unit Liste;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.ListBox, FMX.StdCtrls, FMX.Controls.Presentation, Hinzufuegen, ListeBearbeiten, serverAPI,
  FMX.Edit, ItemBearbeiten, Helper, FMX.Gestures, FMX.Objects, Rest.Client,
  Rest.Types;

type
  TFormListe = class(TForm)
    GridPanelLayout1: TGridPanelLayout;
    LblListe: TLabel;
    ListBox1: TListBox;
    ImgBack: TImage;
    ImgAdd: TImage;
    ImgEdit: TImage;
    GestureManager1: TGestureManager;
    Line1: TLine;
    procedure ImgEditClick(Sender: TObject);
    constructor Create(AOwner: TComponent; var serverAPI: TServerAPI; clickedList: TListe);
    procedure Update();
    procedure ClickOnItem(Sender: TObject);
    procedure subFormClosed(Sender: TObject; var Action: TCloseAction);
    procedure ListBox1ChangeCheck(Sender: TObject);
    procedure ImgBackClick(Sender: TObject);
    procedure ImgAddClick(Sender: TObject);
    procedure FormGesture(Sender: TObject; const EventInfo: TGestureEventInfo;
      var Handled: Boolean);

  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  //* Die Liste-Anzeigen-Form
  ListForm: TFormListe;
  //* Private Instanz der Server API in der der User angemeldet ist
  privateServerAPI: TServerAPI;
  //* Die Liste die gerade angezeigt wird
  list: TListe;
  //* Die Id der Liste die gerade angezeigt wird
  listId: Integer;
  //* Die Listen auf die der User Zugriff hat
  lists: Tlistarray;
  //* Um zu überprüfen, od die Gestenaktion bereits ausgeführt wurde
  closed: Boolean;

implementation

{$R *.fmx}

{*
  Unterfrom wird geschlossen

  Wenn eine Unterform geschlossen wird (z.B. Item bearbeiten/hinzufügen oder
  Liste bearbeiten), dann wird die Liste aktualisiert.

  Wenn die Liste gelöscht wurde, dann wird die Form direkt geschlossen und der
  User auf die Hauptseite weitergeleitet.

  @param Sender Unterform die geschlossen wird
  @param Action Schließaktion der Unterform
}
procedure TFormListe.subFormClosed(Sender: TObject; var Action: TCloseAction);
var
  request: TRestRequest;
begin
  if (Sender.InheritsFrom(TFormListeBearbeiten)) then
  begin
    if ((Sender as TFormListeBearbeiten).hasDeletedList) then
    begin
      Close;
      Release;
    end;
  end;
  request := TRESTRequest.Create(nil);
  request.Method := REST.Types.rmGET;  //GET
  request.Resource := 'user/lists';
  request.Client := privateServerAPI.client;
  request.Timeout := 3000;
  if privateServerAPI.isOnline then
  begin
    request.ExecuteAsync(procedure
    begin
      privateServerAPI.cache := responseToListArray(request.Response.Content);
      Update;
    end);
  end;
end;

{*
  Zurück

  Das Fenster wird geschlossen und der User landet wieder auf der Hauptseite.

  @param Sender Button um zurück zu gelangen
}
procedure TFormListe.ImgBackClick(Sender: TObject);
begin
  Close;
  Release;
end;

{*
  Liste bearbeiten

  Der User wird auf die Liste-Bearbeiten-Form für die aktuelle Liste
  weitergeleitet.

  @param Sender Button um die Liste zu bearbeiten
}
procedure TFormListe.ImgEditClick(Sender: TObject);
var editlistForm:TFormListeBearbeiten;
begin
  if privateServerAPI.isOnline then
  begin
    editlistForm := TFormListeBearbeiten.Create(Application, privateServerAPI, listId);
    editlistForm.Show;
    editlistForm.OnClose := subFormClosed;
  end
  else
    ShowMessage('Du brauchst eine aktive Internetverbindung für diese Aktion!');
end;

{*
  Item hinzufügen

  Der User wird auf die Item-Hinzufügen-Form weitergeleitet.

  @param Sender Button zum hinzufügen eines Items
}
procedure TFormListe.ImgAddClick(Sender: TObject);
var
  additemForm: TFormHinzufuegen;
begin
  additemForm := TFormHinzufuegen.Create(Application, privateServerAPI, listId);
  additemForm.Show;
  additemForm.OnClose := subFormClosed;
end;

{*
  Neuer Konstruktor

  Neuer Konstruktor, um der Form eine private Instanz der Server API zu
  übergeben in der der User angemeldet ist und um die aktuell ausgewählte Liste
  zu übergeben.

  @param AOwner Der Parent der Form
  @param serverAPI Instanz der serverAPI in der der User angemeldet ist.
  @param clickedList Die ausgewählte Liste
}
constructor TFormListe.Create(AOwner: TComponent; var serverAPI: TServerAPI; clickedList: TListe);
begin
  inherited Create(AOwner);
  privateServerAPI := serverAPI;
  listId := clickedList.id;
  closed := false;
  lists := nil;
  Update();
end;

{*
  Zurück

  Wird bei einer Streichgeste vom linken Rand nach rechts ausgeführt.

  Der User wird dann auf die Hauptseite weitergeleitet.

  @param Sender Der GestureManager
  @param EventInfo Informationen über die Geste
  @param Handled Ob die Geste schon bearbeitet wurde
}
procedure TFormListe.FormGesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  Handled := true;
  if not closed then
  begin
    closed := true;
    Close;
    Release;
  end;
end;

{*
  Item abhaken

  Wenn der User ein Item der Listbox anklickt, so wird diese Änderung an den
  Server übertragen.

  @param Sender Die ListBox
}
procedure TFormListe.ListBox1ChangeCheck(Sender: TObject);
var
  item: TItem;
  i: Integer;
  child: TListBoxItem;
  changed: Boolean;
begin
  if privateServerAPI.isOnline then
  begin
    changed := false;
    for i := 0 to ListBox1.Items.Count - 1 do
    begin
      child := ListBox1.ItemByIndex(i);
      item := list.items[StrToInt(child.ItemData.Detail)];
      if child.IsChecked <> item.done then
      begin
        privateServerAPI.DeleteItem(item.itemId);
        privateServerAPI.AddToList(item.name, item.quantity, child.IsChecked, item.categoryId, item.listId);
        changed := true;
      end;
    end;
    if changed then
      Update;
  end
  else
  begin
    Update;
    ShowMessage('Du brauchst eine aktive Internetverbindung für diese Aktion!');
  end;
end;

{*
  Update der Liste

  Die Items werden geupdated, um der Serverversion zu entsprechen.
}
procedure TFormListe.Update;
var
  i: Integer;
  item: TListBoxItem;
begin
  if lists <> privateServerAPI.getCachedLists then
  begin
    lists := privateServerAPI.getCachedLists;
    for i := 0 to High(lists) do
      if lists[i].id = listId then
        list := lists[i];
    LblListe.Text := list.name;
    ListBox1.Clear;
    for i := 0 to High(list.items) do
    begin
      item := TListBoxItem.Create(ListBox1);
      item.Text := Tabulator + list.items[i].name + Tabulator + list.items[i].quantity;
      if list.items[i].done then
        item.IsChecked := true
      else
        item.IsChecked := false;
      item.ItemData.Detail := IntToStr(i);
      item.OnClick := ClickOnItem;
      ListBox1.AddObject(item);
    end;
  end;
end;

{*
  Item bearbeiten

  Wird ausgeführt wenn ein Item in der Liste angeklickt wird. Daraufhin wird
  der User auf die Item-Bearbeiten-Form weitergeleitet.

  @param Sender ListBoxItem das angeklickt wurde
}
procedure TFormListe.ClickOnItem(Sender: TObject);
var
  itemAendernForm: TFormItemBearbeiten;
begin
  itemAendernForm := TFormItemBearbeiten.Create(Application, privateServerAPI, list.items[StrToInt((Sender as TListBoxItem).ItemData.Detail)]);
  itemAendernForm.OnClose := subFormClosed;
  itemAendernForm.Show();
end;


end.
