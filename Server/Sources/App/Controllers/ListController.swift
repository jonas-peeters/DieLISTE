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
        let lists = try request.auth.authenticated(User.self)!.lists.all()
        var json: JSON = try makeJSON(from: lists)
        
        for (listCount, list) in lists.makeIterator().enumerated() {
            try json[listCount]?.set("items", list.items.all())
            for (itemCount, item) in try list.items.all().makeIterator().enumerated() {
                try json[listCount]!["items"]![itemCount]?.set("category", item.categoryId?.wrapped.int)
            }
        }
        
        return json
    }
    
    func removeList(_ request: Request) throws -> ResponseRepresentable {
        guard let json = request.json else {
            throw Abort.badRequest
        }
        let user: User = request.auth.authenticated(User.self)!
        let idToDelete: Int = try json.get("id")
        let lists = try user.lists.all()
        
        let list = lists.filter({ list in list.id!.wrapped.int! == idToDelete })
        
        if list.isEmpty {
            return generateJSONError(from: "List does not exist")
        }
        
        try request.auth.authenticated(User.self)!.lists.remove(list[0])
        try list[0].delete()
        
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
