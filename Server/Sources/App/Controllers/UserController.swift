import Foundation
import Vapor
import HTTP
import FluentProvider

final class UserController {
    
    func addRoutes(drop: Droplet) {
        let basic = drop.grouped("user")
        basic.get(User.parameter, "info", handler: show)
        basic.post(handler: create)
    }
    
    func index(request: Request) throws -> ResponseRepresentable {
        return try makeJSON(from: User.all())
    }
    
    func create(request: Request) throws -> ResponseRepresentable {
        do {
            let user = try request.user()
            try user.save()
            return user
        } catch {
            return generateJSONError(from: "Bad request\n\nReason: No JSON content found.\n\nPossible Solutions:\nTry to format the body of the request as application/json")
        }
    }
    
    func show(_ request: Request) throws -> ResponseRepresentable {
        return try request.parameters.next(User.self)
    }
    
}

extension Request {
    func user() throws -> User {
        print(self)
        guard let json = json else { throw Abort.badRequest }
        return try User(json: json)
    }
}
