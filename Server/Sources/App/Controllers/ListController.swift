import Vapor
import FluentProvider

final class ListController {
    
    func addRoutes(drop: Droplet, listRoute: RouteBuilder) {
        listRoute.get(handler: getLists)
        listRoute.post(handler: addList)
        listRoute.delete(handler: removeList)
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
    
    func removeList(_ request: Request) throws -> ResponseRepresentable {
        guard let json = request.json else {
            throw Abort.badRequest
        }
        let user: User = request.auth.authenticated(User.self)!
        let idToDelete: Int = try json.get("id")
        let lists = try user.lists.all()
        var list: List?
        for listToCheck in lists {
            if listToCheck.id!.wrapped.int! == idToDelete {
                list = listToCheck
                break
            }
        }
        if list == nil {
            return generateJSONError(from: "List does not exist")
        }
        try request.auth.authenticated(User.self)!.lists.remove(list!)
        try list!.delete()
        return try getLists(request)
    }
}

extension Request {
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
