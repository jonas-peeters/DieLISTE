import Vapor
import HTTP
import FluentProvider
import AuthProvider

/// Controlling all routes concerning anything related user infos
///
/// This excludes routes about password changes, etc. These can be found in `PasswordController`
final class UserController {
    /// Used to create webviews that can be shown in any webbrowser
    let view: ViewRenderer
    
    /// Initializer
    ///
    /// - Parameter view: Used for creating webviews
    init(_ view: ViewRenderer) {
        self.view = view
    }
    
    /// Adds all routes relevant to user information
    ///
    /// - parameters:
    ///   - drop: The droplet to append the routes
    ///   - loginRoute: The login route builder
    ///   - authedRoute: A route that is protected by some sort of middleware
    func addRoutes(drop: Droplet, loginRoute: RouteBuilder, authedRoute: RouteBuilder) {
        let userRoute = drop.grouped("user")
        let authedUserRoute = authedRoute.grouped("user")
        let listController = ListController()
        
        // Logging in
        loginRoute.post("login", handler: login)
        
        // Create a new user
        userRoute.post("create") { req in
            return try self.create(req, drop: drop)
        }
        
        // Verifiy email
        userRoute.get("verify", String.parameter) { req in
            return try self.verifyEMail(req, drop: drop)
        }
        
        // Get profil information
        authedUserRoute.get(handler: me)
        
        // Delete user account
        authedUserRoute.delete(handler: delete)
        
        // Change the allergies
        authedUserRoute.post("allergies", handler: writeAllergies)
        
        // Add password specific routes
        let passwordController = PasswordController(self.view)
        passwordController.addRoutes(drop: drop, userRoute: userRoute, authedRoute: authedUserRoute)
        
        // Add lists specified in the list controller
        listController.addRoutes(drop: drop, listRoute: authedUserRoute.grouped("lists"))
    }
    
    /// Creates a new user
    ///
    /// Route for request: POST to `/user/create`
    ///
    /// JSON encoding for request:
    ///
    ///     {
    ///       "email": $EMAIL_String,
    ///       "username": $USERNAME_String,
    ///       "password": $PASSWORD_String
    ///     }
    ///
    /// - parameters:
    ///   - request: A HTTP request
    /// - returns: The user (JSON encoded)
    func create(_ request: Request, drop: Droplet) throws -> ResponseRepresentable {
        do {
            guard let json = request.json else {
                return generateJSONError(from: "Bad request\n\nReason: No JSON content found\n\nPossible solutions:\n - Try to format the body of the request as application/json")
            }
            let newUser: User = try User(json: json)
            
            if !(try User.all().filter({ user in user.username == newUser.username || user.email == newUser.email}).isEmpty) {
                return generateJSONError(from: "Username of email is unavailable.")
            }
            
            do {
                try newUser.save()
                let uniqueId = UUID().uuidString
                try drop.cache.set(uniqueId, newUser.id!.string!, expiration: Date(timeIntervalSinceNow: 86400)) // Expires in 24 hours
                //if !sendEMailVerificationEMail(email: newUser.email, username: newUser.username, link: "http://localhost:4343/user/verify/\(uniqueId)", config: drop.config) { // For local tests only
                if !sendEMailVerificationEMail(email: newUser.email, username: newUser.username, link: "https://die-liste.herokuapp.com/user/verify/\(uniqueId)", drop: drop) {
                    return generateJSONError(from: "User created but email not sent")
                }
            } catch {
                print(error)
                return generateJSONError(from: "Internal Server Error: Unable to save user data.\nPlease contact the server administrator or the software support team.")
            }
            
            return newUser
        } catch {
            return generateJSONError(from: "Bad request\n\nReason: Couldn't parse JSON as user\n\nPossible solutions:\n - Check JSON keys for typos\n - Check JSON values for the correct type")
        }
    }
    
    /// Request to verify an email address using a previously generated unique id
    ///
    /// Route: GET to '/user/verify/$UUID'
    ///
    /// - Parameters:
    ///   - request: A HTTP request
    ///   - drop: Vapor droplet for access to the cache
    /// - Returns: "Success" when everything worked
    func verifyEMail(_ request: Request, drop: Droplet) throws -> ResponseRepresentable {
        do {
            let verifyId = try request.parameters.next(String.self)
            guard let cacheNode = try drop.cache.get(verifyId) else {
                return try self.view.make("error", ["message": "Token abgelaufen"], for: request)
            }
            let id = cacheNode.string
            guard let user = try User.find(id) else {
                return try self.view.make("error", ["message": "User nicht gefunden"], for: request)
            }
            user.verifiedEmail = true
            try user.save()
            return try self.view.make("success", ["message": "E-Mail erfolgreich verifiziert"], for: request)
        } catch {
            return try self.view.make("error", ["message": "Parameter nicht gefunden"], for: request)
        }
    }
    
    /// Login a user
    ///
    /// Route for request: POST to `/login`
    ///
    /// JSON encoding for request:
    ///
    ///     {
    ///       "email": $EMAIL_String,
    ///       "password": $PASSWORD_String
    ///     }
    ///
    /// By sending this request the client will get a session cookie that will allow the continuing communication to require no password sending.
    /// This strenghtens the security.
    ///
    /// - parameters:
    ///   - request: A HTTP request
    /// - returns: "OK: Authenticated" when the credentials are valid
    func login(_ request: Request) throws -> ResponseRepresentable {
        let email: String
        let password: String
        do {
            let json = request.json
            if json == nil {
                return generateJSONError(from: "No JSON content found\n\nTry to format the body as application/json.")
            }
            email = try json!.get(User.Keys.email) as String
            password = try json!.get(User.Keys.password) as String
        } catch {
            return generateJSONError(from: "Bad credentials:\n\nPossible solutions: - email and password have to provided\n - email and password have to be provided as strings\n - content type has to be application/json")
        }
        let credentials = Password(username: email, password: password)
        
        let user = try User.authenticate(credentials)
        request.auth.authenticate(user)
        
        return try makeJSON(from: "OK: Authenticated")
    }
    
    /// Deleting a user
    ///
    /// Route for request: DELETE to `/user`
    ///
    /// The user has to be authenticated
    ///
    /// - parameters:
    ///   - request: A HTTP request
    /// - returns: "Deleted user" when the credentials are valid
    func delete(_ request: Request) throws -> ResponseRepresentable {
        let user = request.auth.authenticated(User.self)
        
        try user?.delete()
        
        return try makeJSON(from: "Deleted user")
    }
    
    /// Only works when authenticated
    ///
    /// Route for request: GET to `/user`
    ///
    /// - parameters:
    ///   - request: A HTTP request
    /// - returns: The requesting user
    func me(_ request: Request) throws -> ResponseRepresentable {
        return try makeJSON(from: request.auth.authenticated(User.self)!)
    }
    
    /// Write into the allergies string of a user
    ///
    /// Route for request: POST to `/user/allergies`
    ///
    /// JSON encoding for request
    ///
    ///     {
    ///       "allergies": $ALLERGIES_String
    ///     }
    ///
    /// The user has to be autheticated
    ///
    /// - Parameter request: The HTTP request
    /// - Returns: "Updated allergies" on success
    func writeAllergies(_ request: Request) throws -> ResponseRepresentable {
        guard let user = request.auth.authenticated(User.self) else {
            return generateJSONError(from: "User not found")
        }
        guard let json = request.json else {
            return generateJSONError(from: "Could not retrieve JSON")
        }
        do {
            let allergies = try json.get("allergies") as String
            user.allergies = allergies
            try user.save()
            return try makeJSON(from: "Updated allergies")
        } catch {
            return generateJSONError(from: "Could not read allergies")
        }
    }
}
