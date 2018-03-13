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
    cache: TListArray;
    online: String;
  public
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
constructor TServerAPI.create();
begin
  inherited Create;
  online := '';
  client := TRestClient.Create('die-liste.herokuapp.com');
end;

{*
  Neuer User

  Hier wird der neue User mit Email-Adresse, Benutzername und Passowrt erstellt
  und seine Daten werden in einem Server(Array) gespeichert.

  @param email Email des gerade erstellten Users
  @param password Passwort des gerade erstellten Users
  @param name Benutzername des gerade erstellten Users
  @param jsonString Der Parameter, auf dem die Daten des Users gespeichert werden
  @param request Der Parameter, der verschiedene Daten anfragt
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
  @param request Der Parameter fragt verschiedene Daten beim Server an
}

function TServerAPI.userSuggestions(ListID:integer; Name: string): TArray;
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
  @param Name Name der jeweiligen Liste
  @param request Der Parameter fragt verschiedene Daten beim Server an
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
  Anmedlung

  Hier kann ein User sich anmelden. DAbei werden die angegeben Daten mit den
  Daten im Server verglichen und wenn sie übereinstimmen, wird der User angemeldet

  @param email Email des Users
  @param password Passwort des Users
  @pram jsonString Parameter, auf dem die Daten des Users gespeichert werden,
                   um sie mit den Daten im Server zu vergleichen
  @param request Der Parameter fragt verschiedene Daten beim Server an
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

  @param request Der Parameter fragt verschiedene Daten beim Server an
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
  @param jsonString Parameter, auf dem das Passwort gespeichert wird
  @param request Der Parameter fragt verschiedene Daten beim Server an
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

  Hier kann ein User sein Passwort zurücksetzten, falls er dieses vergessen hat

  @param email Email des Users
  @param jsonString Parameter, auf dem die Email gespeichert wird
  @param request Der Parameter fragt verschiedene Daten beim Server an
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

  Hier werden die Daten des Users, welcher sich angemeldet hat, abgerufen

  @param request Der Parameter fragt verschiedene Daten beim Server an
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
  @param jsonString Parameter, auf dem der Name gespeichert wird
  @param request Der Parameter fragt verschiedene Daten beim Server an
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
  Daten, die die Liste enthielt, vom Server unwiderruflich gelöscht

  @param id ID der jeweiligen Liste
  @param jsonString Parameter, auf dem der Name gespeichert wird
  @param request Der Parameter fragt verschiedene Daten beim Server an
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

  Hier kann ein User seine Allergien notieren

  @param text Allergien des Users
  @param jsonString Parameter, auf dem der Name gespeichert wird
  @param request Der Parameter fragt verschiedene Daten beim Server an
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
  Listen anzeigen

  Hier werden alle Listen, die ein User erstellt hat oder zu denen er eingeladen
  wurde, aufgelistet.

  @param request Der Parameter fragt verschiedene Daten beim Server an
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

  Hier kann ein User den Namen einer seiner Listen ändern

  @param name Name der Liste
  @param id ID der Liste
  @param jsonString Parameter, auf dem der Name gespeichert wird
  @param request Der Parameter fragt verschiedene Daten beim Server an
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
  Beteiligten genutzt wird

  @param name Name der Liste
  @param ListId ID der Liste
  @param jsonString Parameter, auf dem der Name gespeichert wird
  @param request Der Parameter fragt verschiedene Daten beim Server an
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

  Hier kann ein Item von der Liste eines Users gelöscht werden

  @param id ID des Items
  @param jsonString Parameter, auf dem der Name gespeichert wird
  @param request Der Parameter fragt verschiedene Daten beim Server an
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
