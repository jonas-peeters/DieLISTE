import Foundation
import Vapor
import HTTP
import FluentProvider
import AuthProvider

final class UserController {
    
    func addRoutes(drop: Droplet, loginRoute: RouteBuilder, authedRoute: RouteBuilder) {
        loginRoute.post("login", handler: login)
        let userRoute = drop.grouped("user")
        userRoute.post("create", handler: create)
        let authedUserRoute = authedRoute.grouped("user")
        authedUserRoute.get("lists", handler: getLists)
        authedUserRoute.get("me", handler: me)
        authedUserRoute.post("lists", handler: addList)
    }
    
    func index(request: Request) throws -> ResponseRepresentable {
        return try makeJSON(from: User.all())
    }
    
    func create(request: Request) throws -> ResponseRepresentable {
        do {
            let user = try request.user()
            do {
                try user.save()
            } catch {
                print("Error: 1")
                return generateJSONError(from: "Internal Server Error: Unable to save user data.\nPlease contact the server administrator or the software support team.")
            }
            return user
        } catch {
            print("Error: 2")
            return generateJSONError(from: "Bad request\n\nReason: No JSON content found or JSON is malformed.\n\nPossible solutions:\n - Try to format the body of the request as application/json\n - Check server log")
        }
    }
    
    func show(_ request: Request) throws -> ResponseRepresentable {
        print(request)
        return try request.parameters.next(User.self)
    }
    
    func login(_ request: Request) throws -> ResponseRepresentable {
        let email: String
        let password: String
        do {
            let json = request.json
            if json == nil {
                throw Abort.badRequest
            }
            email = try json!.get(User.Keys.email) as String
            password = try json!.get(User.Keys.password) as String
        } catch {
            return generateJSONError(from: "Bad credentials:\n\nPossible solutions: - email and password have to provided\n - email and password have to be provided as strings\n - content type has to be application/json")
        }
        let credentials = Password(username: email, password: password)
        
        let user = try User.authenticate(credentials)
        request.auth.authenticate(user)
        
        return "OK: Authenticated"
    }
    
    func me(_ request: Request) throws -> ResponseRepresentable {
        return try makeJSON(from: request.auth.authenticated(User.self)!)
    }
    
    func addList(_ request: Request) throws -> ResponseRepresentable {
        let list = try request.list()
        try list.save()
        let connection = try Pivot<User, List>.init(request.auth.authenticated(User.self)!, list)
        try connection.save()
        return try getLists(request)
    }
    
    func getLists(_ request: Request) throws -> ResponseRepresentable {
        return try makeJSON(from: request.auth.authenticated(User.self)!.lists.all())
    }
    
}

extension Request {
    func user() throws -> User {
        guard let json = json else { throw Abort.badRequest }
        do {
            return try User(json: json)
        } catch {
            print(generateJSONError(from: "Malformed JSON: The provided JSON data couldn't be parsed.\n\nPossible solutions:\n - Check if all keys are spelled correctly\n - Check if all types are correct").wrapped)
            throw Abort.badRequest
        }
    }
    
    func list() throws -> List {
        guard let json = json else {
            throw Abort.badRequest
        }
        do {
            return try List(json: json)
        } catch {
            print(generateJSONError(from: "Malformed JSON: The provided JSON data couldn't be parsed.\n\nPossible solutions:\n - Check if all keys are spelled correctly\n - Check if all types are correct").wrapped)
            throw Abort.badRequest
        }
    }
}
