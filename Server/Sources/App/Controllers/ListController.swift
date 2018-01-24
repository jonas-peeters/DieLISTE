import Vapor
import FluentProvider

/// Controlling all routes concerning lists
final class ListController {
    
    /// ## Adds all routes relevant to lists
    ///
    /// Get `/user/lists`: Returns all lists for the given user
    ///
    /// Post `/user/lists`: Creates a new list
    ///
    /// Delete `/user/lists`: Deletes a list
    ///
    /// Post `/user/lists/name`: Changes the name of a list
    ///
    /// Post `/user/lists/items`: Adds an item to a list
    ///
    /// Delete to `/user/lists/items`: Deleted an item from a list
    ///
    /// - parameters:
    ///   - drop: The droplet the routes should be added to
    ///   - listRoute: The route used for anything list related
    func addRoutes(drop: Droplet, listRoute: RouteBuilder) {
        listRoute.get(handler: getLists)
        listRoute.post(handler: addList)
        listRoute.delete(handler: removeList)
        listRoute.post("name", handler: changeName)
        let itemRoute = listRoute.grouped("items")
        itemRoute.post(handler: addToList)
        itemRoute.delete(handler: deleteFromList)
    }
    
    /// Creates a new list
    ///
    /// Route for request: POST to `/user/lists`
    ///
    /// JSON encoding for request
    ///
    ///     {
    ///         "name": $LISTNAME_String
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
    /// Route for request: GET to `/user/lists`
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
            try json[listCount]!.set("items", list.items)
        }
        
        return json
    }
    
    /// Changes the name of a list
    ///
    /// Route for request: POST to `/user/lists/name`
    ///
    /// JSON encoding for request:
    ///
    ///     {
    ///         "name": $NEWLISTNAME_String,
    ///         "id": $LISTID_Int
    ///     }
    ///
    /// - Parameter request: A HTTP request
    /// - Returns: "Changed name" on success
    func changeName(_ request: Request) throws -> ResponseRepresentable {
        guard let user = request.auth.authenticated(User.self) else {
            return generateJSONError(from: "User not autheticated")
        }
        guard let json = request.json else {
            return generateJSONError(from: "Could not get JSON")
        }
        do {
            let listId = try json.get("id") as Int
            let newName = try json.get("name") as String
            guard let list = try user.lists.find(listId) else {
                return generateJSONError(from: "Could not find list")
            }
            list.name = newName
            try list.save()
            return try makeJSON(from: "Changed name")
        } catch {
            return generateJSONError(from: "Could not read data from json")
        }
    }
    
    /// Adds an item to a list
    ///
    /// Route for request: POST to `/user/lists/items`
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
        let user = request.auth.authenticated(User.self)!
        guard let json = request.json else {
            return generateJSONError(from: "Could not retrieve JSON")
        }
        
        let item: Item?
        do {
            item = try Item(json: json)
            do {
                if try !user.lists.all().contains(where: { $0.id!.int! == item!.listId!.int! }) {
                    return generateJSONError(from: "\(user.username) as no access to list \(String(describing: item!.listId!.int!))")
                }
            } catch {
                return generateJSONError(from: "Could not get user's lists")
            }
        } catch {
            return generateJSONError(from: "Malformed JSON: Could not interfer item from json")
        }
        
        try item?.save()
        
        return try getLists(request)
    }
    
    /// Deletes an item from a list
    ///
    /// Route for request: DELETE to `/user/lists/items`
    ///
    /// JSON encoding for request
    ///
    ///     {
    ///         "id": $ID_Int
    ///     }
    ///
    /// User must be authenticated/logged in
    ///
    /// - Parameter request: The HTTP request
    /// - Returns: "Item deleted" on success
    func deleteFromList(_ request: Request) throws -> ResponseRepresentable {
        guard let user = request.auth.authenticated(User.self) else {
            return generateJSONError(from: "User not found")
        }
        guard let json = request.json else {
            return generateJSONError(from: "Could not retrieve JSON")
        }
        do {
            let id = try json.get("id") as Int
            do {
                guard let item = user.items.first(where: { $0.id!.int! == id}) else {
                    return generateJSONError(from: "Could not find item")
                }
                try user.lists.find(item.listId!)?.children(type: Item.self, foreignIdKey: List.foreignIdKey).delete(item)
                return try makeJSON(from: "Item deleted")
            }
        } catch {
            return generateJSONError(from: "Could not interfere item")
        }
    }
    
    /// Deletes a list
    ///
    /// Route for request: DELETE to `/user/lists`
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
        
        return try makeJSON(from: "Deleted list")
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
        guard let list = try user.lists.find(idToFind) else {
            throw Abort(.notFound, reason: "List not found")
        }
        
        return list
    }
}
