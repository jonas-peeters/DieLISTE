import Vapor
import HTTP
import FluentProvider
import AuthProvider

/// Controlling all routes concerning anything with the users password
final class PasswordController {
    /// Used to create webviews that can be shown in any webbrowser
    let view: ViewRenderer
    
    /// Initializer
    ///
    /// - Parameter view: Used for creating webviews
    init(_ view: ViewRenderer) {
        self.view = view
    }
    
    /// Add routes that have something to do with the users password
    ///
    /// - Parameters:
    ///   - drop: The droplet in order to access the cache
    ///   - userRoute: Route to '/user'
    ///   - authedRoute: Authenticated route to '/user'
    func addRoutes(drop: Droplet, userRoute: RouteBuilder, authedRoute: RouteBuilder) {
        // Change password
        authedRoute.post("password", "change", handler: { req in
            return try self.changePassword(req, drop: drop)
        })
        
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
    /// - Returns: A status (see docs)
    func changePassword(_ request: Request, drop: Droplet) throws -> ResponseRepresentable {
        guard let user = request.auth.authenticated(User.self) else {
            return status(40)
        }
        do {
            guard let json = request.json else {
                return status(20)
            }
            let unhashedPassword = try json.get("password") as String
            let newPassword: String = (try drop.hash.make(unhashedPassword)).makeString()
            user.password = newPassword
            
            do {
                try user.save()
            } catch {
                return generateJSONError(from: "Could not save new password! Try again later.")
            }
        } catch {
            return status(25)
        }
        
        return status(10)
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
    /// - Returns: A status (see docs)
    func forgotPassword(_ request: Request, drop: Droplet) throws -> ResponseRepresentable {
        do {
            guard let json = request.json else {
                return status(20)
            }
            let email = try json.get(User.Keys.email) as String
            var user: User? = nil
            user = try User.all().first(where: { $0.email.lowercased() == email.lowercased() })
            if user != nil {
                if user!.verifiedEmail {
                    let uniqueId = UUID().uuidString
                    try drop.cache.set(uniqueId, user!.id!.string!, expiration: Date(timeIntervalSinceNow: 86400))
                    if sendForgotPasswordEMail(email: email, username: user!.username, link: "https://die-liste.herokuapp.com/user/password/reset/\(uniqueId)", drop: drop) {
                        return status(10)
                    } else {
                        return status(32)
                    }
                } else {
                    return status(45)
                }
            } else {
                return status(42)
            }
        } catch {
            return status(25)
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
            let newPassword: String = try request.parameters.next(String.self)
            if newPassword.count > 5 {
                let newHashedPassword = (try drop.hash.make(newPassword)).makeString()
                user.password = newHashedPassword
                try user.save()
                return try self.view.make("success", ["message": "Passwort erfolgreich geändert"], for: request)
            } else {
                return try self.view.make("error", ["message": "Das Passwort muss min. die Länge 6 haben"], for: request)
            }
        } catch {
            return try self.view.make("error", ["message": "Parameter nicht gefunden"], for: request)
        }
    }
}







