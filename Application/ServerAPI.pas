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
  Helper;

type
  TServerAPI=class(TObject)
  private
    client: TRESTClient;
  public
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
end;

implementation
constructor TServerAPI.create();
begin
  inherited Create;
  client := TRestClient.Create('die-liste.herokuapp.com');
end;

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
  request.Execute;
  result := request.Response.Content;
end;

function TServerAPI.userSuggestions(ListID:integer; Name: string): TArray;
var
  request: TRESTRequest;
begin
  request := TRESTRequest.Create(nil);
  request.Method := REST.Types.rmGET;
  request.Resource := 'user/lists/' + IntToStr(listId) + '/suggestions/' + name;
  request.Client := self.client;
  request.Execute;
  result := jsonArrayToStringArray(request.Response.Content);
end;

function TServerAPI.inviteUser(ListID:integer; Name: string): string;
var
  request: TRESTRequest;
begin
  request := TRESTRequest.Create(nil);
  request.Method := REST.Types.rmGET;
  request.Resource := 'user/lists/' + IntToStr(listId) + '/invite/' + name;
  request.Client := self.client;
  request.Execute;
  result := request.Response.Content;
end;


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
  request.Execute;
  result := request.Response.Content;
end;

function TServerAPI.deleteUser(): String;
var
  request: TRESTRequest;
begin
  request := TRESTRequest.Create(nil);
  request.Method := REST.Types.rmDELETE;
  request.Resource := 'user';
  request.Client := self.client;
  request.Execute;
  result := request.Response.Content;
end;

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
  request.Execute;
  result := request.Response.Content;
end;

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
  request.Execute;
  result := request.Response.Content;
end;

function TServerAPI.me(): TUserData;
var
  request: TRESTRequest;
begin
  request := TRESTRequest.Create(nil);
  request.Method := REST.Types.rmGET;  //GET
  request.Resource := 'user';
  request.Client := self.client;
  request.Execute;
  result := responseToUser(request.Response.Content);
end;

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
  request.Execute;
  result := request.Response.Content;
end;

function TserverAPI.removeList(id: Integer):String;
var
  jsonString: string;
  request: TRESTRequest;
begin
  request := TRESTRequest.Create(nil);
  jsonString := '{"id": "' + IntToStr(id) + '"}';
  request.Method := REST.Types.rmPOST;
  request.Body.JSONWriter.WriteRaw(jsonString);
  request.Resource := '/user/lists/delete';
  request.Client := self.client;
  request.Execute;
  result := request.Response.Content;
end;


function TServerAPI.getLists(): TListArray;
var
  request: TRESTRequest;
begin
  request := TRESTRequest.Create(nil);
  request.Method := REST.Types.rmGET;  //GET
  request.Resource := 'user/lists';
  request.Client := self.client;
  request.Execute;
  result := responseToListArray(request.Response.Content);
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
  request.Execute;
  result := request.Response.Content;
end;

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
  request.Execute;
  result := request.Response.Content;
end;

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
  request.Execute;
  result := request.Response.Content;
end;

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
  request.Execute;
  result := request.Response.Content;
end;

end.
