import Vapor
import HTTP
import FluentProvider
import AuthProvider

final class UserController {
    
    /// Adds all routes relevant to user information
    ///
    /// - parameters:
    ///   - drop: The droplet to append the routes
    ///   - loginRoute: The login route builder
    ///   - authedRoute: A route that is protected by some sort of middleware
    func addRoutes(drop: Droplet, loginRoute: RouteBuilder, authedRoute: RouteBuilder) {
        loginRoute.post("login", handler: login)
        let userRoute = drop.grouped("user")
        userRoute.post("create", handler: create)
        let authedUserRoute = authedRoute.grouped("user")
        authedUserRoute.get("me", handler: me)
        let listController = ListController()
        listController.addRoutes(drop: drop, listRoute: authedUserRoute.grouped("lists"))
    }
    
    /// Creates a new user
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
    func create(request: Request) throws -> ResponseRepresentable {
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
            } catch {
                print(error)
                return generateJSONError(from: "Internal Server Error: Unable to save user data.\nPlease contact the server administrator or the software support team.")
            }
            return newUser
        } catch {
            return generateJSONError(from: "Bad request\n\nReason: Couldn't parse JSON as user\n\nPossible solutions:\n - Check JSON keys for typos\n - Check JSON values for the correct type")
        }
    }
    
    /// Login a user
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
    
    /// Only works when authenticated
    ///
    /// - parameters:
    ///   - request: A HTTP request
    /// - returns: The requesting user
    func me(_ request: Request) throws -> ResponseRepresentable {
        return try makeJSON(from: request.auth.authenticated(User.self)!)
    }
    
}
