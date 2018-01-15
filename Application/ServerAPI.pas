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
    function me(): String;
    function jsonToRecord(jsonString: string): TUserData;
    function addList();
    function getLists(request:TRESTRequest);
    function jsonArrayToArray(const s:string):TArray;
    procedure AddToArray(var a:TArray; const value: string);
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

function TServerAPI.addList();
var  request: TRESTRequest;
begin
  request := TRESTRequest.Create(nil);
  request.Method := REST.Types.rmPOST;  //POST
  request.Resource := 'user/lists';
  request.Client := self.client;
  request.Execute;
  result := request.Response.Content;
end;

function TServerAPI.getLists(request:TRESTRequest);
begin
  request := TRESTRequest.Create(nil);
  request.Method := REST.Types.rmGET;  //GET
  request.Resource := 'user/lists';
  request.Client := self.client;
  request.Execute;
  result := request.Response.Content;
end;

procedure TServerAPI.AddToArray(var a:TArray; const value: string);           //Zeile 139 bis 161
overload;                                                                     //ist das konvertieren der
var laenge: integer;                                                          //Arrays zu für uns
begin                                                                         //brauchbaren Arrays
  laenge:=length(a);
  Setlength(a,laenge+1);
  a[laenge]:=value;
  end;

function TServerAPI.jsonArrayToArray(const s:string):TArray;
var jar: TjsonArray;
    item: Tjsonstring;
    i:integer;
begin
  result:=nil;
  jar:=TjsonObject.ParseJSONValue(s)as TjsonArray;
  for I := 0 to (jar.Count-1) do
    begin
      item:= jar.Items as Tjsonstring;
      AddToArray(result,item.Value);
    end;
  jar.Free;
end;

end.
