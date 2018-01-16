import Vapor
import HTTP
import FluentProvider
import AuthProvider

final class PasswordController {
    let view: ViewRenderer
    
    init(_ view: ViewRenderer) {
        self.view = view
    }
    
    func addRoutes(drop: Droplet, userRoute: RouteBuilder, authedRoute: RouteBuilder) {
        // Change password
        authedRoute.post("password", "change", handler: changePassword)
        
        // Forgot password
        userRoute.post("password","forgot") { req in
            return try self.forgotPassword(req, drop: drop)
        }
        
        // Set new password webpage
        userRoute.get("password","reset", String.parameter, handler: { req in
            return try self.resetPassword(req, drop: drop)
        })
        
        // Set new password bts
        userRoute.get("password","submit", String.parameter, String.parameter, handler: { req in
            return try self.passwordResetSuccess(req, drop: drop)
        })
    }
    
    /// Changes the password of a user (only when authenticated)
    ///
    /// Route for request: POST to '/user/password/change'
    ///
    /// JSON encoding for request:
    ///
    ///     {
    ///       "password": $NEW_PASSWORD_String
    ///     }
    ///
    /// - Parameter request: A HTTP request
    /// - Returns: "Changed Password" when the action was successful
    func changePassword(_ request: Request) throws -> ResponseRepresentable {
        let user = request.auth.authenticated(User.self)
        
        do {
            let json = request.json
            let newPassword = try json!.get(User.Keys.password) as String
            user?.password = newPassword
            
            do {
                try user?.save()
            } catch {
                return generateJSONError(from: "Could not save new password! Try again later.")
            }
        } catch {
            return generateJSONError(from: "Could not read password from JSON.")
        }
        
        return try makeJSON(from: "Changed Password")
    }
    
    /// Sends an email to the user so that he/she can reset their password
    ///
    /// Route for request: POST to '/user/password/forgot'
    ///
    /// JSON encoding for request:
    ///
    ///     {
    ///       "email": $EMAIL_String
    ///     }
    ///
    /// - Parameter request: A HTTP request
    /// - Returns: "EMail sent" on success
    func forgotPassword(_ request: Request, drop: Droplet) throws -> ResponseRepresentable {
        do {
            let json = request.json
            let email = try json!.get(User.Keys.email) as String
            var user: User? = nil
            user = try User.all().first(where: { $0.email.lowercased() == email.lowercased() })
            if user != nil {
                if user!.verifiedEmail {
                    let uniqueId = UUID().uuidString
                    try drop.cache.set(uniqueId, user!.id!.string!, expiration: Date(timeIntervalSinceNow: 86400))
                    if sendForgotPasswordEMail(email: email, username: user!.username, link: "https://die-liste.herokuapp.com/user/password/reset/\(uniqueId)", drop: drop) {
                        return try makeJSON(from: "EMail sent")
                    } else {
                        return generateJSONError(from: "EMail not sent")
                    }
                } else {
                    return generateJSONError(from: "E-Mail not verified")
                }
            } else {
                return generateJSONError(from: "User with that email not found")
            }
        } catch {
            return generateJSONError(from: "Could not read e-mail from json")
        }
    }
    
    /// Webpage where the forgot-password-email is pointing towards
    ///
    /// Route: GET to '/user/password/reset/$UUID'
    ///
    /// - Parameters:
    ///   - request: The HTTP request
    ///   - drop: The droplet to access the cache
    /// - Returns: A webpage to reset the password or an error webpage
    func resetPassword(_ request: Request, drop: Droplet) throws -> ResponseRepresentable {
        do {
            let verifyId = try request.parameters.next(String.self)
            guard let cacheNode = try drop.cache.get(verifyId) else {
                return try self.view.make("error", ["message": "Token abgelaufen"], for: request)
            }
            let id = cacheNode.string
            guard let user = try User.find(id) else {
                return try self.view.make("error", ["message": "User nicht gefunden"], for: request)
            }
            return try self.view.make("resetPassword", ["username": user.username, "uuid": verifyId], for: request)
        } catch {
            return try self.view.make("error", ["message": "Parameter nicht gefunden"], for: request)
        }
    }
    
    /// Webpage where the resez-password-webpage is pointing towards
    ///
    /// Route: GET to '/user/password/reset/$UUID/$NEW_PASSWORD'
    ///
    /// - Parameters:
    ///   - request: The HTTP request
    ///   - drop: The droplet to access the cache
    /// - Returns: A webpage where the user is told that his password has changed
    func passwordResetSuccess(_ request: Request, drop: Droplet) throws -> ResponseRepresentable {
        do {
            let verifyId = try request.parameters.next(String.self)
            guard let cacheNode = try drop.cache.get(verifyId) else {
                return try self.view.make("error", ["message": "Token abgelaufen"], for: request)
            }
            let id = cacheNode.string
            guard let user = try User.find(id) else {
                return try self.view.make("error", ["message": "User nicht gefunden"], for: request)
            }
            let newPassword = try request.parameters.next(String.self)
            if newPassword != "" {
                user.password = newPassword
                try user.save()
                return try self.view.make("success", ["message": "Passwort erfolgreich geändert"], for: request)
            } else {
                return try self.view.make("error", ["message": "Das Passwort muss min. die Länge 1 haben"], for: request)
            }
        } catch {
            return try self.view.make("error", ["message": "Parameter nicht gefunden"], for: request)
        }
    }
}







