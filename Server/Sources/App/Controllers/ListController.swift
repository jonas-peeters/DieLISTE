import Vapor
import FluentProvider

final class ListController {
    
    /// ## Adds all routes relevant to lists
    ///
    /// Get: Returns all lists for the given user
    ///
    /// Post: Creates a new list
    ///
    /// Delete: Deletes a list
    ///
    /// Put: Puts a new item into a list
    ///
    /// - parameters:
    ///   - drop: The droplet the routes should be added to
    ///   - listRoute: The route used for anything list related
    func addRoutes(drop: Droplet, listRoute: RouteBuilder) {
        listRoute.get(handler: getLists)
        listRoute.post(handler: addList)
        listRoute.delete(handler: removeList)
        listRoute.put(handler: addToList)
    }
    
    /// Creates a new list
    ///
    /// JSON encoding for request
    ///
    ///     {
    ///         "name": $LISTNAME
    ///     }
    ///
    /// User must be authenticated/logged in
    ///
    /// - parameters:
    ///   - request: A HTTP request
    /// - returns: The lists of the user
    func addList(_ request: Request) throws -> ResponseRepresentable {
        let list = try request.list()
        try list.save()
        let connection = try Pivot<User, List>.init(request.auth.authenticated(User.self)!, list)
        try connection.save()
        return try getLists(request)
    }
    
    /// Returns a JSON array with all the lists and their items
    ///
    /// User must be authenticated/logged in
    ///
    /// - parameters:
    ///   - request: A HTTP request
    /// - returns: The lists of the user
    func getLists(_ request: Request) throws -> ResponseRepresentable {
        let lists = try request.auth.authenticated(User.self)!.lists.all()
        var json: JSON = try makeJSON(from: lists)
        
        for (listCount, list) in lists.makeIterator().enumerated() {
            try json[listCount]!.set("items", list.children(type: Item.self, foreignIdKey: Item.Keys.id).all())
        }
        
        return json
    }
    
    /// Adds an item to a list
    ///
    /// JSON encoding for request
    ///
    ///     {
    ///         "name": $ITEMNAME_String,
    ///         "quantity": $QUANTITY_String,
    ///         "done": $DONE_Boolean,
    ///         "listId": $LISTID_Int,
    ///         "categoryId": $CATEGORYID_Int
    ///     }
    ///
    /// User must be authenticated/logged in
    ///
    /// - parameters:
    ///   - request: A HTTP request
    /// - returns: The lists of the user
    func addToList(_ request: Request) throws -> ResponseRepresentable {
        guard let json = request.json else {
            throw Abort.badRequest
        }
        
        let item: Item?
        do {
            item = try Item(json: json)
        } catch {
            return generateJSONError(from: "Malformed JSON: Could not interfer item from json")
        }
        
        try item?.save()
        
        return try getLists(request)
    }
    
    /// Deletes a list
    ///
    /// JSON encoding for request
    ///
    ///     {
    ///         "id": $LISTID
    ///     }
    ///
    /// User must be authenticated/logged in
    ///
    /// - parameters:
    ///   - request: A HTTP request
    /// - returns: The lists of the user
    func removeList(_ request: Request) throws -> ResponseRepresentable {
        let list = try request.getListFromUser(idKey: "id")
        
        try request.auth.authenticated(User.self)!.lists.remove(list)
        try list.delete()
        
        return try getLists(request)
    }
}

extension Request {
    /// Creates a list from the JSON provided in the body of the request
    ///
    /// - returns: A list if one could be created and badRequest if no JSON could be retrieved or the JSON is malformed
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
    
    /// Finds the list from a request if the user has access.
    /// If the user has no access to this list "notFound" will be returned.
    ///
    /// - parameters:
    ///   - idKey: The key in the json from the request that contains the list id
    /// - returns: A list if a list is found, notFound if no list is found and badRequest if no JSON could be retrieved
    func getListFromUser(idKey: String) throws -> List {
        guard let json = json else {
            throw Abort.badRequest
        }
        let user: User = auth.authenticated(User.self)!
        let idToFind: Int = try json.get(idKey)
        let lists = try user.lists.all()
        
        let list = lists.filter({ list in list.id!.wrapped.int! == idToFind })
        
        if list.isEmpty {
            throw Abort.notFound
        }
        
        return list[0]
    }
}
