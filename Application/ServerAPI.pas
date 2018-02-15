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
    function me(): String;
    function jsonToRecord(jsonString: string): TUserData;
    function addList(name: string): String;
    function getLists(): TListArray;
    function jsonArrayToArray(const s: string): TListArray;
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

function TServerAPI.me(): String;
var
  request: TRESTRequest;
begin
  request := TRESTRequest.Create(nil);
  request.Method := REST.Types.rmGET;  //GET
  request.Resource := 'user';
  request.Client := self.client;
  request.Execute;
  result := request.Response.Content;
end;

function TServerAPI.jsonToRecord(jsonString: string): TUserData;
var
  jsonObject: TJSONObject;
  userData: TUserData;
begin
  jsonObject := JSON.TJSONObject.ParseJSONValue(jsonString) as TJSONObject;
  userData.name := jsonObject.GetValue('username').Value;
  userData.password:= jsonObject.GetValue('password').Value;
  userData.allergies := jsonObject.GetValue('allergies').Value;
  userData.verifiedemail := jsonObject.GetValue('verified').Value.ToBoolean;
  userData.email:= jsonObject.GetValue('email').Value;
  result := userData;
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
  result := jsonArrayToArray(request.Response.Content);
end;

function TServerAPI.jsonArrayToArray(const s:string): TListArray;
var jsonListArray: TJSONArray;
    jsonItemArray: TJSONArray;
    jsonUserArray: TJSONArray;
    memberOfListArray: TJSONValue;
    memberOfItemArray: TJSONValue;
    i, j:integer;
begin
  result := nil;
  jsonListArray:=TjsonObject.ParseJSONValue(s) as TjsonArray;
  SetLength(Result, jsonListArray.Count);
  for i := 0 to (jsonListArray.Count-1) do
  begin
    memberOfListArray := jsonListArray.Items[i];
    Result[i].name := memberOfListArray.GetValue('name', 'Kein Name gefunden');
    Result[i].id := memberOfListArray.GetValue('id', -1);
    jsonUserArray := memberOfListArray.GetValue('user', TJSONArray.Create());
    SetLength(Result[i].user, jsonUserArray.Count);
    for j := 0 to (jsonUserArray.Count - 1) do
      Result[i].user[j] := jsonUserArray.Items[j].Value;
    jsonItemArray := memberOfListArray.GetValue('items', TJSONArray.Create());
    SetLength(Result[i].items, jsonItemArray.Count);
    for j := 0 to (jsonItemArray.Count - 1) do
    begin
      memberOfItemArray := jsonItemArray.Items[j];
      Result[i].items[j].name := memberOfItemArray.GetValue('name', 'Keine Name gefunden');
      Result[i].items[j].quantity := memberOfItemArray.GetValue('quantity', 'Keine Menge gefunden');
      Result[i].items[j].done := memberOfItemArray.GetValue('done', false);
      Result[i].items[j].categoryId := memberOfItemArray.GetValue('categoryId', 0);
      Result[i].items[j].itemId := memberOfItemArray.GetValue('itemId', 0);
      Result[i].items[j].listId := memberOfItemArray.GetValue('listId', 0);
    end;
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
