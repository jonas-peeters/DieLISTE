{*
  The 'ServerAPI' is an interface to communicate with the server.

  Because the user is authenticated by a session cookie, it is nessecary to
  route all private server request beginning at the login-request through the
  same client object.

  All the functions to do a request can be found here.
}
unit ServerAPI;

interface

uses
  SysUtils,
  Classes,
  REST.Client,
  REST.Types,
  IPPeerCommon,
  IPPeerClient,
  JSON,
  Helper, FMX.Dialogs;

type
  TServerAPI=class(TObject)
  private
    online: String;
  public
    cache: TListArray;
    client: TRESTClient;
    constructor create();
    function createUser(email: String; name: String; password: String): String;
    function login(email : String; password: String): String;
    function deleteUser(): String;
    function changePassword(password: string): String;
    function forgotPassword(email: string): String;
    function me(): TUserData;
    function addList(name: string): String;
    function getLists(): TListArray;
    function AddToList(name: string; menge: String; fertig: boolean; kategorie: Integer; liste: Integer): string;
    function ChangeListName(name: string; id: Integer): string;
    function removeList(id: integer):String;
    function userSuggestions(ListID:integer; Name: string): TArray;
    function inviteUser(ListID:integer; Name: string): string;
    function removeUser(listId: Integer; name:string):String;
    function DeleteItem(id:integer):string;
    function editInfo(text: string):string;
    function isOnline(): Boolean;
    function isValidOnline(): Boolean;
    function getCachedLists(): TListArray;
    function getListString(): String;
    function getValidLists(): TListArray;
end;

implementation

{*
  Kontruktor

  Der Rest-Client wird mit der Server-URL initialisiert.
}
constructor TServerAPI.create();
begin
  inherited Create;
  online := '';
  client := TRestClient.Create('https://die-liste.herokuapp.com');
end;

{*
  Neuer User

  Hier wird der neue User mit Email-Adresse, Benutzername und Passwort erstellt.
  Seine Daten werden online in einer Datenbank gespeichert.

  @param email Email des gerade erstellten Users
  @param password Passwort des gerade erstellten Users
  @param name Benutzername des gerade erstellten Users
  @return String (Antwort des Servers)
}
function TServerAPI.createUser(email: String; name: String; password: String): String;
var
  jsonString: String;
  request: TRESTRequest;
begin
  jsonString := '{"email": "' + email + '", "username": "' + name + '", "password": "' + password + '"}';
  request := TRESTRequest.Create(nil);
  request.Method := REST.Types.rmPOST;
  request.Body.JSONWriter.WriteRaw(jsonString);
  request.Resource := 'user/create';
  request.Client := self.client;
  request.Timeout := 3000;
  try
    request.Execute;
    result := request.Response.Content;
  except
    ShowMessage('Du brauchst eine aktive Internetverbindung für diese Aktion!');
    result := '{"code":"0"}';
  end;

end;

{*
  Serververbindung überprüfen

  Die Verbindung wird getestet und der Status dieses Tests zurückgegeben.
  Weil dieser immer etwas dauert und dieser Test für Hintergrundupdates
  verwendet soll, läuft er asyncron ab.
  Daher wird immer das Ergebnis des zuletzt abgeschlossen Test zurückgegeben.

  Ist eine 100-prozentige Richtigkeit erforderlich, bitte die isValidOnline()
  Funktion verwenden.

  @return Ob eine Verbindung hergestellt werden konnte.
}
function TServerAPI.isOnline(): Boolean;
var
  request: TRESTRequest;
  r: Boolean;
begin
  request := TRESTRequest.Create(nil);
  request.Method := Rest.Types.rmGET;
  request.Resource := 'info/online';
  request.Client := self.client;
  request.Timeout := 3000;
  try
    request.ExecuteAsync(procedure begin
      if request.Response.Content = 'true' then
      begin
        online := 'true';
      end
      else
      begin
        online := 'false';
      end;
    end, true, true, procedure(Sender: TObject)
    begin
      online := 'false';
    end);
  except
    online := 'false';
  end;

  if online = 'true' then
  begin
    result := true;
  end
  else
  begin
    result := false;
  end;
end;

{*
  Serververbindung überprüfen

  Die Verbindung wird getestet und der Status dieses Tests zurückgegeben.
  Weil dieser immer etwas dauert, ist er nicht für Hintergundtests geeignet.
  (Dafür die isOnline() Funktion verwenden.)

  @return Ob eine Verbindung hergestellt werden konnte.
}
function TServerAPI.isValidOnline(): Boolean;
var
  request: TRESTRequest;
  r: Boolean;
begin
  request := TRESTRequest.Create(nil);
  request.Method := Rest.Types.rmGET;
  request.Resource := 'info/online';
  request.Client := self.client;
  request.Timeout := 3000;
  try
    request.Execute;
    if request.Response.Content = 'true' then
    begin
      online := 'true';
      result := true;
    end
    else
    begin
      online := 'false';
      result := false;
    end;
  except
    online := 'false';
    result := false;
  end;
end;

{*
  Uservorschlag bei der Usersuche

  Hier kann ein User einen anderen User suchen, um zu sehen, ob dieser
  registriert ist und er ihn in einem weiteren Schritt zu seiner Liste
  hinzufügen kann

  @param ListID ID der Liste, zu welcher ein User hinzugefügt werden soll
  @param Name Name der jeweiligen Liste
}
function TServerAPI.userSuggestions(ListID: integer; Name: string): TArray;
var
  request: TRESTRequest;
  r: TArray;
begin
  request := TRESTRequest.Create(nil);
  request.Method := REST.Types.rmGET;
  request.Resource := 'user/lists/' + IntToStr(listId) + '/suggestions/' + name;
  request.Client := self.client;
  request.Timeout := 3000;
  try
    request.Execute;
    result := jsonArrayToStringArray(request.Response.Content);
  except
    ShowMessage('Du brauchst eine aktive Internetverbindung für diese Aktion!');
    result := [];
  end;

end;


{*
  User hinzufügen

  Hier kann ein User einen anderen User zu einer seiner Listen hinzufügen

  @param ListID ID der Liste, zu welcher ein User hinzugefügt werden soll
  @param Name Name des Users der hinzugefügt werden soll
}
function TServerAPI.inviteUser(ListID:integer; Name: string): string;
var
  request: TRESTRequest;
begin
  request := TRESTRequest.Create(nil);
  request.Method := REST.Types.rmGET;
  request.Resource := 'user/lists/' + IntToStr(listId) + '/invite/' + name;
  request.Client := self.client;
  request.Timeout := 3000;
  try
    request.Execute;
    result := request.Response.Content;
  except
    ShowMessage('Du brauchst eine aktive Internetverbindung für diese Aktion!');
    result := '{"code":"0"}';
  end;
end;


{*
  Anmeldung

  Hier kann ein User sich anmelden. Dabei werden die angegeben Daten mit den
  Daten im Server verglichen und wenn sie übereinstimmen, wird der User
  angemeldet.
  Damit dies nicht bei jeder Anfrage geschehen muss, wird ein Session-Cookie
  gespeichert. Aus diesem Grund wird auf im gesamten Projekt die gleiche Instanz
  der Server API verwendet in der der User angemeldet ist.

  @param email Email des Users
  @param password Passwort des Users
}
function TServerAPI.login(email: String; password: String): String;
var
  jsonString: String;
  request: TRESTRequest;
begin
  jsonString := '{"email": "' + email + '", "password": "' + password + '"}';
  request := TRESTRequest.Create(nil);
  request.Method := REST.Types.rmPOST; //POST
  request.Body.JSONWriter.WriteRaw(jsonString);
  request.Resource := 'login';
  request.Client := self.client;
  request.Timeout := 3000;
  try
    request.Execute;
    result := request.Response.Content;
  except
    ShowMessage('Du brauchst eine aktive Internetverbindung für diese Aktion!');
    result := '{"code":"0"}';
  end;
  getLists;
end;

{*
  User löschen

  Hier kann ein User sein Account löschen. Damit werden dann alle seine Daten
  vom Server gelöscht
}
function TServerAPI.deleteUser(): String;
var
  request: TRESTRequest;
begin
  request := TRESTRequest.Create(nil);
  request.Method := REST.Types.rmDELETE;
  request.Resource := 'user';
  request.Client := self.client;
  request.Timeout := 3000;
  try
    request.Execute;
    result := request.Response.Content;
  except
    ShowMessage('Du brauchst eine aktive Internetverbindung für diese Aktion!');
    result := '{"code":"0"}';
  end;
end;


{*
  Passwort ändern

  Hier kann ein User sein Passwort beliebig in ein anderes umändern.

  @param password Passwort des Users
}
function TServerAPI.changePassword(password:string): String;
var
  jsonString: string;
  request: TRESTRequest;
begin
  jsonString := '{"password": "' + password + '"}';
  request := TRESTRequest.Create(nil);
  request.Method := REST.Types.rmPOST;
  request.Body.JSONWriter.WriteRaw(jsonString);
  request.Resource := 'user/password/change';
  request.Client := self.client;
  request.Timeout := 3000;
  try
    request.Execute;
    result := request.Response.Content;
  except
    ShowMessage('Du brauchst eine aktive Internetverbindung für diese Aktion!');
    result := '{"code":"0"}';
  end;
end;

{*
  Passwort zurücksetzten

  An die angegebene E-Mail-Adresse wird eine E-Mail gesendet. In dieser ist ein
  Link zu einer Website auf der der User ein neues Passwort wählen kann.

  @param email Email des Users
}
function TServerAPI.forgotPassword(email: string): String;
var
  jsonString: string;
  request: TRESTRequest;
begin
  jsonString := '{"email": "' + email + '"}';
  request := TRESTRequest.Create(nil);
  request.Method := REST.Types.rmPOST;
  request.Body.JSONWriter.WriteRaw(jsonString);
  request.Resource := '/user/password/forgot';
  request.Client := self.client;
  request.Timeout := 3000;
  try
    request.Execute;
    result := request.Response.Content;
  except
    ShowMessage('Du brauchst eine aktive Internetverbindung für diese Aktion!');
    result := '{"code":"0"}';
  end;
end;

{*
  Daten laden

  Hier werden die Daten des Users, welcher sich angemeldet hat, abgerufen.

  @return TUserData (Die Daten des Users)
}
function TServerAPI.me(): TUserData;
var
  request: TRESTRequest;
begin
  request := TRESTRequest.Create(nil);
  request.Method := REST.Types.rmGET;  //GET
  request.Resource := 'user';
  request.Client := self.client;
  request.Timeout := 3000;
  try
    request.Execute;
    result := responseToUser(request.Response.Content);
  except
    ShowMessage('Du brauchst eine aktive Internetverbindung für diese Aktion!');
    result.name := 'Offline';
  end;
end;

{*
  Liste erstellen

  Hier kann ein User eine neue Liste erstellen

  @param name Name der neu erstellten Liste
  @return String (Die Antwort des Servers)
}
function TServerAPI.addList(name: string): String;
var
  request: TRESTRequest;
  jsonString: String;
begin
  request := TRESTRequest.Create(nil);
  jsonString := '{"name": "' + name + '"}';
  request.Body.JSONWriter.WriteRaw(jsonString);
  request.Method := REST.Types.rmPOST;  //POST
  request.Resource := 'user/lists';
  request.Client := self.client;
  request.Timeout := 3000;
  try
    request.Execute;
    result := request.Response.Content;
  except
    ShowMessage('Du brauchst eine aktive Internetverbindung für diese Aktion!');
    result := '{"code":"0"}';
  end;
  getLists;
end;

{*
  Liste löschen

  Hier kann ein User eine seiner Liste löschen. Dabei werden dann auch alle die
  Daten, die die Liste enthielt, aus der Datenbank gelöscht.

  @param id ID der jeweiligen Liste
  @return String (Die Antwort des Servers)
}
function TserverAPI.removeList(id: Integer):String;
var
  jsonString: string;
  request: TRESTRequest;
begin
  request := TRESTRequest.Create(nil);
  JsonString := '{"id": "' + IntToStr(id) + '"}';
  request.Method := REST.Types.rmPOST;
  request.Body.JSONWriter.WriteRaw(jsonString);
  request.Resource := '/user/lists/delete';
  request.Client := self.client;
  request.Timeout := 3000;
  try
    request.Execute;
    result := request.Response.Content;
  except
    ShowMessage('Du brauchst eine aktive Internetverbindung für diese Aktion!');
    result := '{"code":"0"}';
  end;
  getLists;
end;

{*
  Allergien eintragen

  Hier kann ein User seine Allergien notieren.

  @param text Allergien des Users
  @return String (Die Antwort des Servers)
}
function TserverAPI.editInfo(text: string):string;
var
  request: TRESTRequest;
  jsonString: String;
begin
  request := TRESTRequest.Create(nil);
  jsonString := '{"allergies": "' + text + '"}';
  request.Body.JSONWriter.WriteRaw(jsonString);
  request.Method := REST.Types.rmPOST;  //POST
  request.Resource := 'user/allergies';
  request.Client := self.client;
  request.Timeout := 3000;
  try
    request.Execute;
    result := request.Response.Content;
  except
    ShowMessage('Du brauchst eine aktive Internetverbindung für diese Aktion!');
    result := '{"code":"0"}';
  end;
end;

{*
  Listen erhalten

  Die Listen werden auf den Stand des Servers gebracht. Da dies fast immer etwas
  dauert arbeitet diese Funktion asyncron. Daher wird immer die gecachte Version
  zurückgegeben, die beim letzten Aufruf geladen wurde.

  @return Listen aus dem cache
}
function TServerAPI.getLists(): TListArray;
var
  request: TRESTRequest;
begin
  request := TRESTRequest.Create(nil);
  request.Method := REST.Types.rmGET;  //GET
  request.Resource := 'user/lists';
  request.Client := self.client;
  request.Timeout := 3000;
  if isOnline then
  begin
    request.ExecuteAsync(procedure
    begin
      cache := responseToListArray(request.Response.Content);
    end);
  end
  else
  begin
    cache := responseToListArray(getOfflineData.lists);
  end;
  result := cache;
end;

{*
  Listen erhalten (immer Server Version)

  Wenn Internet besteht werden die Listen immer vom Server geladen. (Auch wenn
  dies lange dauert.)

  @return Listen vom Server
}
function TServerAPI.getValidLists(): TListArray;
var
  request: TRESTRequest;
begin
  request := TRESTRequest.Create(nil);
  request.Method := REST.Types.rmGET;  //GET
  request.Resource := 'user/lists';
  request.Client := self.client;
  request.Timeout := 3000;
  if isOnline then
  begin
    request.Execute;
    cache := responseToListArray(request.Response.Content);
  end
  else
  begin
    cache := responseToListArray(getOfflineData.lists);
  end;
  result := cache;
end;

function TServerAPI.getCachedLists(): TListArray;
begin
  result := cache;
end;

{*
  Listen erhalten (als String)

  Die Listen des Users werden geladen. Dies wird Hauptsächlich dazu genutzt, um
  diese Daten Offline zu speichern, da dort nur Strings gespeichert werden
  können.

  @return Listen des Users (als String)
}
function TServerAPI.getListString(): String;
var
  request: TRESTRequest;
  r: String;
  offlineData: TOfflineData;
begin
  request := TRESTRequest.Create(nil);
  request.Method := REST.Types.rmGET;  //GET
  request.Resource := 'user/lists';
  request.Client := self.client;
  request.Timeout := 3000;
  request.Execute;
  if request.Response.Content <> '' then
  begin
    result := request.Response.Content;
    cache := responseToListArray(request.Response.Content);
  end
  else
  begin
    result := '[]';
  end;
end;

{*
  Item hinzufügen

  Ein Item wird zu einer Liste hinzugefügt.

  @param name Name des Items
  @param menge Menge des Items
  @param fertig Ob das Item erledigt ist
  @param kategorie Die Id der Kategorie des Items
  @param liste Id der Liste zu der das Item hinzugefügt werden soll
}
function TServerAPI.AddToList(name: string; menge: String; fertig: boolean; kategorie: Integer; liste: Integer):string;
var
  request: TRESTRequest;
  jsonString: String;
  done: String;
begin
  if fertig then
    done := 'true'
  else
    done := 'false';
  request := TRESTRequest.Create(nil);
  jsonString := '{"name": "' + name + '", "quantity": "' + menge + '", "done": "' + done + '", "categoryId": "' + IntToStr(kategorie) + '", "listId": "' + IntToStr(liste) + '"}';
  request.Body.JSONWriter.WriteRaw(jsonString);
  request.Method := REST.Types.rmPOST;  //POST
  request.Resource := 'user/lists/items';
  request.Client := self.client;
  request.Timeout := 3000;
  try
    request.Execute;
    result := request.Response.Content;
  except
    ShowMessage('Du brauchst eine aktive Internetverbindung für diese Aktion!');
    result := '{"code":"0"}';
  end;
  getLists;
end;

{*
  Listname ändern

  Hier kann ein User den Namen einer seiner Listen ändern.

  @param name Neuer name der Liste
  @param id ID der Liste
  @return String (Die Antwort des Servers)
}
function TServerAPI.ChangeListName(name: string; id: Integer):string;
var
  request: TRESTRequest;
  jsonString: String;
begin
  request := TRESTRequest.Create(nil);
  jsonString := '{"id": "' + IntToStr(id) + '", "name": "' + name + '"}';
  request.Body.JSONWriter.WriteRaw(jsonString);
  request.Method := REST.Types.rmPOST;  //POST
  request.Resource := 'user/lists/name';
  request.Client := self.client;
  request.Timeout := 3000;
  try
    request.Execute;
    result := request.Response.Content;
  except
    ShowMessage('Du brauchst eine aktive Internetverbindung für diese Aktion!');
    result := '{"code":"0"}';
  end;
  getLists;
end;

{*
  User entfernen

  Hier kann ein User von einer Liste entfernt werden, welchen von beiden
  Beteiligten genutzt wird.

  @param name Name der Liste
  @param ListId ID der Liste
  @return String (Die Antwort des Servers)
}
function TserverAPI.removeUser(listId: Integer; name:string):String;
  var
  jsonString: string;
  request: TRESTRequest;
begin
  request := TRESTRequest.Create(nil);
  jsonString := '{"list_id": "' + IntToStr(listId) + '", "username": "' + name + '"}';
  request.Method := REST.Types.rmPOST;
  request.Body.JSONWriter.WriteRaw(jsonString);
  request.Resource := '/user/lists/removeuser';
  request.Client := self.client;
  request.Timeout := 3000;
  try
    request.Execute;
    result := request.Response.Content;
  except
    ShowMessage('Du brauchst eine aktive Internetverbindung für diese Aktion!');
    result := '{"code":"0"}';
  end;
end;

{*
  Item löschen

  Hier kann ein Item von der Liste eines Users gelöscht werden.

  @param id ID des Items
  @return String (Die Antwort des Servers)
}
function TServerAPI.DeleteItem(id:integer):string;
var
  request: TRESTRequest;
  jsonString: string;
begin
  request := TRESTRequest.Create(nil);
  request.Method := REST.Types.rmPOST;
  jsonString := '{"id": "' + IntToStr(id) +'"}';
  request.Body.JSONWriter.WriteRaw(jsonString);
  request.Resource := 'user/lists/items/delete';
  request.Client := self.client;
  request.Timeout := 3000;
  try
    request.Execute;
    result := request.Response.Content;
  except
    ShowMessage('Du brauchst eine aktive Internetverbindung für diese Aktion!');
    result := '{"code":"0"}';
  end;
end;

end.
