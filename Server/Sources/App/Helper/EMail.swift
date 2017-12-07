import Foundation
import Vapor

/// Send an email to the user where he can verify his/her email address
///
/// - Parameters:
///   - email: The users email address
///   - username: The users username
///   - link: The link where the user can verify his/her email
/// - Returns: true on success, false when something goes wrong
func sendEMailVerificationEMail(email: String, username: String, link: String, config: Config) -> Bool {
    let content = "<html lang='en'> <head> <meta charset='UTF-8'> <link href='https://fonts.googleapis.com/css?family=Source+Code+Pro' rel='stylesheet'> <style>*{margin: 0; padding: 0; outline: 0; box-sizing: border-box;}html, body{width: 100%; height: 100%; background-color: #92a8d1; background-image: -webkit-linear-gradient(top, #81BEF7 40%, #819FF7 100%); color: white; -webkit-font-smoothing: antialiased;}.container{width: 100%; padding: 32px; margin: 0 auto; position: relative; top: 0%; text-align: left;}.title{font-size: 40px; opacity: 0.9; font-weight: 100; font-family: 'Source Code Pro', monospace; text-shadow: 0px 4px rgba(0,0,0,0.1); line-height: 100px;}.message{font-size: 20px; opacity: 0.8; font-family: 'Source Code Pro', monospace; text-shadow: 0px 2px rgba(0,0,0,0.2); line-height: 25px;}</style> </head> <body> <div class='container'> <h1 class='title'>Willkommen</h1> <p class='message'>Hallo \(username),</p><br><p class='message'>mit dem folgenden Link kannst du deine E-Mail Adresse verifizieren: </p><br><p class='message'>\(link)</p></div></body></html>"
    
    return sendEMail(to: email, content: content, config: config)
}


/// Send an email via the Sendgrid email api
///
/// - Parameters:
///   - email: Target email address
///   - content: HTML site as a string. This is what the user will see
/// - Returns: true on success, false when something goes wrong
func sendEMail(to email: String, content: String, config: Config) -> Bool {
    var success: Bool? = nil
    let url = URL(string: "https://api.sendgrid.com/v3/mail/send")!
    var request = URLRequest(url: url)
    request.setValue("application/json", forHTTPHeaderField: "Content-Type")
    let apiKey = config["email","api_key"]!.string!
    request.setValue("Bearer \(apiKey)", forHTTPHeaderField: "Authorization")
    request.httpMethod = "POST"
    
    let body = "{\"personalizations\": [{\"to\": [{\"email\": \"\(email)\"}],\"subject\": \"E-Mail Verification\"}],\"from\": {\"email\": \"no_reply@die-liste.herokuapp.com\", \"name\":\"DieLISTE\"},\"content\": [{\"type\": \"text/html\",\"value\": \"\(content)\"}]}";
    
    request.httpBody = body.data(using: .utf8)
    let task = URLSession.shared.dataTask(with: request) { data, response, error in
        guard let _ = data, error == nil else {
            print("While sending an EMail the folowing error occured: \(error.debugDescription)")
            success = false
            return
        }
        
        //let responseString = String(data: data, encoding: .utf8)  //Only for debugging
        success = true
    }
    task.resume()
    
    // Waiting for the task to complete
    while success == nil {}
    return success!
}
