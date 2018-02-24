{*
  Helper unit to define records and do fill them from JSON responses
}
unit Helper;

interface

uses
  FMX.Dialogs, JSON, IOUtils, StrUtils, CCR.PrefsIniFile, System.IniFiles;

type
  TUserData= record
  email:string;
  name: string;
  password: string;
  allergies: string;
  verifiedemail: boolean;
  id: Integer;
end;

type
  TItem=record
    name: String;
    quantity: String;
    done: boolean;
    categoryId: Integer;
    listId: Integer;
    itemId: Integer;
  end;

type
  TListe=record
    name: String;
    id: Integer;
    items: Array of TItem;
    user: Array of String;
  end;

type
  TListArray = Array of TListe;

type
 TArray= Array of string;

type
  TLoginData=record
    email: String;
    password: String;
    worked: Boolean;
  end;

function interpretServerResponse(response: String): Boolean;
function jsonArrayToStringArray(const s:string): TArray;
function responseToListArray(const s:string): TListArray;
function responseToUser(jsonString: string): TUserData;
procedure saveLoginData(loginData: TLoginData);
function getLoginData(): TLoginData;

implementation

procedure saveLoginData(loginData: TLoginData);
var
  jsonObject: TJSONObject;
  path: String;
  settings: TCustomIniFile;
begin
  jsonObject := TJSONObject.Create;
  jsonObject.AddPair('email', loginData.email);
  jsonObject.AddPair('password', loginData.password);
  jsonObject.AddPair('worked', IfThen(loginData.worked, 'True', 'False'));

  settings := CreateUserPreferencesIniFile;
  settings.WriteString('prefs', 'login_data', jsonObject.ToString);
end;

function getLoginData(): TLoginData;
var
  jsonObject: TJSONObject;
  saved: String;
  settings: TCustomIniFile;
begin
  jsonObject := TJSONObject.Create;
  try
    settings := CreateUserPreferencesIniFile;
    saved := settings.ReadString('prefs', 'login_data', '{"email":"","password":"","worked":"false"}');

    jsonObject := TJSONObject.ParseJSONValue(saved) as TJSONObject;
    Result.email := jsonObject.GetValue('email', '');
    Result.password := jsonObject.GetValue('password', '');
    Result.worked := jsonObject.GetValue('worked', False);
  except
    Result.worked := false;
  end;
end;

function responseToUser(jsonString: string): TUserData;
var
  jsonObject: TJSONObject;
  userData: TUserData;
begin
  jsonObject := JSON.TJSONObject.ParseJSONValue(jsonString) as TJSONObject;
  userData.name := jsonObject.GetValue('username', '');
  userData.password:= jsonObject.GetValue('password', '');
  userData.allergies := jsonObject.GetValue('allergies', '');
  userData.verifiedemail := jsonObject.GetValue('verified', false);
  userData.email := jsonObject.GetValue('email', '');
  userData.id := jsonObject.GetValue('id', -1);
  result := userData;
end;

function responseToListArray(const s:string): TListArray;
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
    Result[i].name := memberOfListArray.GetValue<String>('name', 'Kein Name gefunden');
    Result[i].id := memberOfListArray.GetValue<Integer>('id', -1);
    jsonUserArray := memberOfListArray.GetValue<TJSONArray>('user', TJSONArray.Create());
    SetLength(Result[i].user, jsonUserArray.Count);
    for j := 0 to (jsonUserArray.Count - 1) do
      Result[i].user[j] := jsonUserArray.Items[j].Value;
    jsonItemArray := memberOfListArray.GetValue<TJSONArray>('items', TJSONArray.Create());
    SetLength(Result[i].items, jsonItemArray.Count);
    for j := 0 to (jsonItemArray.Count - 1) do
    begin
      memberOfItemArray := jsonItemArray.Items[j];
      Result[i].items[j].name := memberOfItemArray.GetValue<String>('name', 'Keine Name gefunden');
      Result[i].items[j].quantity := memberOfItemArray.GetValue<String>('quantity', 'Keine Menge gefunden');
      Result[i].items[j].done := memberOfItemArray.GetValue<Boolean>('done', false);
      Result[i].items[j].categoryId := memberOfItemArray.GetValue<Integer>('categoryId', 0);
      Result[i].items[j].itemId := memberOfItemArray.GetValue<Integer>('id', -1);
      Result[i].items[j].listId := memberOfListArray.GetValue<Integer>('id', -1);
    end;
  end;
end;

function jsonArrayToStringArray(const s:string): TArray;
var jsonArray: TJSONArray;
    i:integer;
begin
  result := nil;
  jsonArray:=TjsonObject.ParseJSONValue(s) as TjsonArray;
  SetLength(Result, jsonArray.Count);
  for i := 0 to (jsonArray.Count-1) do
  begin
    result[i] := jsonArray.Items[i].Value;
  end;
end;

{*
  Tool for interpreting the response by the server. This function will return
  True when the response suggests that the request has worked and false when
  not.
  In addition to that this function will also throw error messages dedicated to
  the individual error codes as described in the server documentation.

  This will only work for all requests that return a status message and not for
  the request that return data!

  @param response The response string from the server
  @return Boolean, if the request was a success
}
function interpretServerResponse(response: String): Boolean;
var
  jsonObject: TJSONObject;
  errorCode: Integer;
  errorMessage: String;
begin
  try
    jsonObject := TJSONObject.ParseJSONValue(response) as TJSONObject;
    errorCode := jsonObject.GetValue('code', 0);
    case errorCode of
      10: begin result := True; end;
      11: begin result := True; end;
      12: begin result := True; end;
      20: begin result := false; ShowMessage('JSON nicht gefunden'); end;
      21: begin result := false; ShowMessage('Parameter nicht gefunden'); end;
      22: begin result := false; ShowMessage('User nicht gefunden'); end;
      23: begin result := false; ShowMessage('Liste nicht gefunden'); end;
      24: begin result := false; ShowMessage('Item nicht gefunden'); end;
      25: begin result := false; ShowMessage('Unerwarteter Fehler. Sind alle eingaben korrekt?'); end;
      30: begin result := false; ShowMessage('Server Error: Lesen nicht möglich'); end;
      31: begin result := false; ShowMessage('Server Error: Schreiben nicht möglich'); end;
      32: begin result := false; ShowMessage('Server Fehler: Die E-Mail ist möglicherweise nicht angekommen.'); end;
      40: begin result := false; ShowMessage('Anmeldung Fehlgeschlagen: Versuche es erneut.'); end;
      41: begin result := false; ShowMessage('Fehler: Der User konnte nicht gefunden werden.'); end;
      42: begin result := false; ShowMessage('Fehler: Die E-Mail konnte nicht gefunden werden.'); end;
      43: begin result := false; ShowMessage('Anmeldung Fehlgeschlagen: Passwort oder E-Mail falsch.'); end;
      44: begin result := false; ShowMessage('Benutzername oder E-Mail nicht verfügbar.'); end;
      45: begin result := false; ShowMessage('Diese Aktion ist nicht möglich. Deine E-Mail ist noch nicht verifiziert.'); end;
    end;
  except
    result := false;
  end;
end;



end.
