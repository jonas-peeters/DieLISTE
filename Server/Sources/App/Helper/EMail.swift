import Vapor

/// Send an email to the user where he can verify his/her email address
///
/// - Parameters:
///   - email: The users email address
///   - username: The users username
///   - link: The link where the user can verify his/her email
///   - drop: Droplet in order to get the mail API key and send the request
/// - Returns: true on success, false when something goes wrong
func sendEMailVerificationEMail(email: String, username: String, link: String, drop: Droplet) -> Bool {
    let content = "<html lang=en><meta charset=UTF-8><link href='https://fonts.googleapis.com/css?family=Source+Code+Pro'rel=stylesheet><style>*{margin:0;padding:0;outline:0;box-sizing:border-box}body,html{width:100%;height:100%;background-color:#fafafa;color:#333;-webkit-font-smoothing:antialiased}.container{width:90%;padding:32px;margin:0 auto;position:relative;border-radius:10px;top:5%;text-align:left;background-color:#EEE}.title{font-size:40px;opacity:.9;font-weight:100;font-family:'Source Code Pro',monospace;text-shadow:2px 2px rgba(0,0,0,.1);line-height:100px}.message{font-size:20px;opacity:.8;font-family:'Source Code Pro',monospace;text-shadow:1px 1px rgba(0,0,0,.2);line-height:25px}a.button{font-size:20px;opacity:.8;font-family:'Source Code Pro',monospace;text-shadow:1px 1px rgba(0,0,0,.2);line-height:25px;border-radius:10px;border-width:2px;border-style:solid;border-color:#FFF;background-color:#54C5EC;color:#FFF;padding:5px;margin:40px;text-decoration:none}</style><div class=container><h1 class=title>Willkommen </h1><p class=message>Hallo \(username),</p><br><p class=message>vielen Dank für Deine Anmeldung bei DieLISTE!</p><br><p class=message>Klicke auf den folgenden Button, um deine E-Mail zu verifizieren und alle Funktionen nutzen zu können:</p><br><a class=button href=\(link)>E-Mail Verfizieren</a><br><br><p class=message>Viel Spaß beim Erstellen deiner ersten Liste!</p><br><p class=message>Dein DieLISTE Team!</div>"
    return sendEMail(to: email, content: content, drop: drop, subject: "Registrierung DieLISTE")
}

func sendForgotPasswordEMail(email: String, username: String, link: String, drop: Droplet) -> Bool {
    let content = "<html lang='en'> <head> <meta charset='UTF-8'> <link href='https://fonts.googleapis.com/css?family=Source+Code+Pro' rel='stylesheet'> <style>*{margin: 0; padding: 0; outline: 0; box-sizing: border-box;}html, body{width: 100%; height: 100%; background-color: #fafafa; color: #333333; -webkit-font-smoothing: antialiased;}.container{width: 90%; padding: 32px; margin: 0 auto; position: relative; border-radius: 10px; top: 5%; text-align: left; background-color: #EEE;}.title{font-size: 40px; opacity: 0.9; font-weight: 100; font-family: 'Source Code Pro', monospace; text-shadow: 2px 2px rgba(0,0,0,0.1); line-height: 100px;}.message{font-size: 20px; opacity: 0.8; font-family: 'Source Code Pro', monospace; text-shadow: 1px 1px rgba(0,0,0,0.2); line-height: 25px;}a.button{font-size: 20px; opacity: 0.8; font-family: 'Source Code Pro', monospace; text-shadow: 1px 1px rgba(0,0,0,0.2); line-height: 25px; border-radius: 10px; border-width: 2px; border-style: solid; border-color: #FFFFFF; background-color: #54C5EC; color: #FFFFFF; padding: 5px; margin: 40px; text-decoration: none;}</style> </head> <body> <div class='container'> <h1 class='title'>Passwort zurücksetzen </h1> <p class='message'>Hallo \(username),</p><br><p class='message'> du erhälst diese E-Mail, weil du das Zurücksetzten deines Passworts beantragt hast. Wenn du diese Änderung sich angefordert hast, kannst du diese E-Mail ignorieren. </p><br><p class='message'> Klicke auf den folgenden Button, um ein neues Passwort festzulegen: </p><br><a href='\(link)' class='button'>Passwort zurücksetzten</a> <br><br><p class='message'> Viel Spaß beim Erstellen weiterer Listen! </p><br><p class='message'> Dein DieLISTE Team! </p></div></body></html>"
    return sendEMail(to: email, content: content, drop: drop, subject: "Passwort zurücksetzen")
}

/// Send an email via the Sendgrid email api
///
/// - Parameters:
///   - email: Target email address
///   - content: HTML site as a string. This is what the user will see
///   - drop: Droplet in order to get the mail API key and send the request
/// - Returns: true on success, false when something goes wrong
func sendEMail(to email: String, content: String, drop: Droplet, subject: String) -> Bool {
    if let apiKey = drop.config["email","api_key"]?.string! {
        let uri = "https://api.sendgrid.com/v3/mail/send"
        let body = "{\"personalizations\": [{\"to\": [{\"email\": \"\(email)\"}],\"subject\": \"\(subject)\"}],\"from\": {\"email\": \"no_reply@die-liste.herokuapp.com\", \"name\":\"DieLISTE\"},\"content\": [{\"type\": \"text/html\",\"value\": \"\(content)\"}]}";
        do {
            let _ = try drop.client.post(uri, query: [:], ["Content-Type" : "application/json", "Authorization" : "Bearer \(apiKey)"], body)
        } catch {
            print("Could not send email")
            return false
        }
        return true
    }
    print("Could not get sendgrid api key. Make sure the API key is present in the email.json file with the identifier 'api_key'.")
    return false
}
