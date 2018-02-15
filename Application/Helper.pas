unit Helper;

interface

uses
  FMX.Dialogs, JSON;

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

function interpretServerResponse(response: String): Boolean;
function jsonArrayToStringArray(const s:string): TArray;

implementation

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
      20: begin result := false; end;
      21: begin result := false; end;
      22: begin result := false; end;
      23: begin result := false; end;
      24: begin result := false; end;
      25: begin result := false; ShowMessage('Unerwarteter Fehler. Sind alle eingaben korrekt?'); end;
      30: begin result := false; end;
      31: begin result := false; end;
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
