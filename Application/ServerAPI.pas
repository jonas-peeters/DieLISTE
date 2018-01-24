unit ServerAPI;

interface

uses
  SysUtils,
  Classes,
  REST.Client,
  REST.Types,
  IPPeerCommon,
  IPPeerClient,
  JSON;

type
  TUserData= record
  email:string;
  name: string;
  password: string;
  allergies: string;
  verifiedemail: boolean;
end;

type
  TItem=record
    name: String;
    quantity: String;
    done: boolean;
    categoryId: Integer;
  end;

type
  TListe=record
    name: String;
    id: Integer;
    items: Array of TItem;
  end;

type
  TListArray = Array of TListe;

type
 TArray= Array of string;

type
  TServerAPI=class(TObject)
  private
    client: TRESTClient;
  public
    constructor create();
    function createUser(email: String; name: String; password: String): String;
    function login(email : String; password: String): String;
    function deleteUser(): String;
    function changePassword(password:string): String;
    function forgotPassword(email: string): String;
    function me(): String;
    function jsonToRecord(jsonString: string): TUserData;
    function addList(name:string): String;
    function getLists(): TListArray;
    function jsonArrayToArray(const s:string): TListArray;
    function AddToList(name:string; menge: String; fertig: boolean; kategorie: Integer; liste: Integer):string;
    function ChangeListName(name:string):string;
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
    jsonItemArray := memberOfListArray.GetValue('items', TJSONArray.Create());
    SetLength(Result[i].items, jsonItemArray.Count);
    for j := 0 to (jsonItemArray.Count - 1) do
    begin
      memberOfItemArray := jsonItemArray.Items[j];
      Result[i].items[j].name := memberOfItemArray.GetValue('name', 'Keine Name gefunden');
      Result[i].items[j].quantity := memberOfItemArray.GetValue('quantity', 'Keine Menge gefunden');
      Result[i].items[j].done := memberOfItemArray.GetValue('done', false);
      Result[i].items[j].categoryId := memberOfItemArray.GetValue('categoryId', 0);
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

// Not working currently (Back-End)
function TServerAPI.ChangeListName(name:string):string;
var  request: TRESTRequest;
begin
  request := TRESTRequest.Create(nil);
  request.Method := REST.Types.rmPOST;  //POST
  request.Resource := 'user/lists';
  request.Client := self.client;
  request.Execute;
  result := request.Response.Content;
end;


end.
