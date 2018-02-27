import Vapor
import FluentProvider

/// Controlling all routes concerning lists
final class ListController {
    /// Used to create webviews that can be shown in any webbrowser
    let view: ViewRenderer
    
    /// Initializer
    ///
    /// - Parameter view: Used for creating webviews
    init(_ view: ViewRenderer) {
        self.view = view
    }
    
    /// ## Adds all routes relevant to lists
    ///
    /// Get `/user/lists`: Returns all lists for the given user
    ///
    /// Post `/user/lists`: Creates a new list
    ///
    /// Post `/user/lists/delete`: Deletes a list
    ///
    /// Post `/user/lists/name`: Changes the name of a list
    ///
    /// Post `/user/lists/items`: Adds an item to a list
    ///
    /// POST to `/user/lists/items/delete`: Deleted an item from a list
    ///
    /// - parameters:
    ///   - drop: The droplet the routes should be added to
    ///   - listRoute: The route used for anything list related
    func addRoutes(drop: Droplet, listRoute: RouteBuilder) {
        listRoute.get(handler: getLists)
        listRoute.post(handler: addList)
        listRoute.post("delete", handler: removeList)
        listRoute.post("name", handler: changeName)
        listRoute.get(Int.parameter, "suggestions", handler: userSuggestions) // With typed suggestion
        listRoute.get(Int.parameter, "suggestions", String.parameter, handler: userSuggestions) // Without typed suggestion
        listRoute.get(Int.parameter, "invite", String.parameter, handler: { request in
            try self.inviteUser(request, drop: drop)
        })
        drop.get("user", "lists", "acceptinvitation", String.parameter, handler: { request in
            try self.acceptInvitation(request, drop: drop)
        })
        listRoute.post("removeuser", handler: removeUser)
        let itemRoute = listRoute.grouped("items")
        itemRoute.post(handler: addToList)
        itemRoute.post("delete", handler: deleteFromList)
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
    /// - returns: A status (see docs)
    func addList(_ request: Request) throws -> ResponseRepresentable {
        guard let user = request.auth.authenticated(User.self) else {
            return status(40)
        }
        guard let json = request.json else {
            return status(20)
        }
        do {
            let list = try List(json: json)
            do {
                try list.save()
                let connection = try Pivot<User, List>(user, list)
                try connection.save()
                return status(10)
            } catch {
                return status(31)
            }
        } catch {
            return status(25)
        }
    }
    
    /// Returns a JSON array with all the lists and their items
    ///
    /// Route for request: GET to `/user/lists`
    ///
    /// User must be authenticated/logged in
    ///
    /// - parameters:
    ///   - request: A HTTP request
    /// - returns: A status (see docs)
    func getLists(_ request: Request) throws -> ResponseRepresentable {
        guard let user = request.auth.authenticated(User.self) else {
            return status(40)
        }
        do {
            let lists = try user.lists.all()
            var json: JSON = try makeJSON(from: lists)
            
            for (listCount, list) in lists.makeIterator().enumerated() {
                try json[listCount]!.set("items", list.items)
                try json[listCount]!.set("user", list.users)
            }
            
            return json
        } catch {
            return status(30)
        }
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
            return status(40)
        }
        guard let json = request.json else {
            return status(20)
        }
        do {
            let listId = try json.get("id") as Int
            let newName = try json.get("name") as String
            guard let list = try user.lists.find(listId) else {
                return status(23)
            }
            list.name = newName
            try list.save()
            return status(10)
        } catch {
            return status(25)
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
    /// - returns: A status (see docs)
    func addToList(_ request: Request) throws -> ResponseRepresentable {
        guard let user = request.auth.authenticated(User.self) else {
            return status(40)
        }
        guard let json = request.json else {
            return status(20)
        }
        
        let item: Item?
        do {
            item = try Item(json: json)
            do {
                if try !user.lists.all().contains(where: { $0.id!.int! == item!.listId!.int! }) {
                    return status(23)
                }
                do {
                    try item?.save()
                    return status(10)
                } catch {
                    return status(31)
                }
            } catch {
                return status(30)
            }
        } catch {
            return status(25)
        }
    }
    
    /// Deletes an item from a list
    ///
    /// Route for request: POST to `/user/lists/items/delete`
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
    /// - Returns: A status (see docs)
    func deleteFromList(_ request: Request) throws -> ResponseRepresentable {
        guard let user = request.auth.authenticated(User.self) else {
            return status(40)
        }
        guard let json = request.json else {
            return status(20)
        }
        do {
            let id = try json.get("id") as Int
            do {
                guard let item = user.items.first(where: { $0.id!.int! == id}) else {
                    return status(24)
                }
                try user.lists.find(item.listId!)?.children(type: Item.self, foreignIdKey: List.foreignIdKey).delete(item)
                return status(10)
            }
        } catch {
            return status(25)
        }
    }
    
    /// Deletes a list
    ///
    /// Route for request: POST to `/user/lists/delete`
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
    /// - returns: A status (see docs)
    func removeList(_ request: Request) throws -> ResponseRepresentable {
        guard let user = request.auth.authenticated(User.self) else {
            return status(40)
        }
        guard let json = request.json else {
            return status(20)
        }
        do {
            let id: Int = try json.get("id")
            guard let list = try user.lists.find(id) else {
                return status(23)
            }
            do {
                try user.lists.remove(list)
                if list.users.isEmpty {
                    for item in list.items {
                        try item.delete()
                    }
                    try list.delete()
                }
                try user.save()
                return status(10)
            } catch {
                return status(31)
            }
        } catch {
            return status(25)
        }
    }
    
    /// Evaluates a search term by the user to show some suggestions on which users to invite to a list
    ///
    /// Path for request: GET to /user/lists/$LISTID_Int/suggestions/$SEARCH_String
    ///
    /// - Parameter request: A HTTP request
    /// - Returns: A JSON array with strings
    func userSuggestions(_ request: Request) throws -> ResponseRepresentable {
        guard let user = request.auth.authenticated(User.self) else {
            return status(40)
        }
        do {
            
            guard let list = try user.lists.find(request.parameters.next() as Int) else {
                return status(23)
            }
            do {
                let typedString = try request.parameters.next() as String
                var users = user.connectedUsers.filter({ $0.key.contains(typedString) })
                for otherUser in try User.all().filter({ $0.username.contains(typedString) && users[$0.username] == nil && $0.verifiedEmail }) {
                    users[otherUser.username] = 0
                }
                for otherUser in list.users {
                    users.removeValue(forKey: otherUser)
                }
                
                let sortedUsers = users.sorted {
                    if $0.value == $1.value {
                        return max($0.key, $1.key).equals(caseInsensitive: $1.key)
                    } else {
                        return $0.value > $1.value
                    }
                }.map({ $0.key })
                var fifteenSortedUsers: [String] = []
                if sortedUsers.count > 0 {
                    for i in 0...(min(15, sortedUsers.count-1)) {
                        fifteenSortedUsers.append(sortedUsers[i])
                    }
                }
                return try makeJSON(from: sortedUsers)
            } catch { // When there is no search term, all users this user is currently working with on other lists are shown
                var users = user.connectedUsers
                for otherUser in list.users {
                    users.removeValue(forKey: otherUser)
                }
                return try makeJSON(from: users.sorted { $0.value > $1.value }.map({ $0.key }))
            }
        } catch {
            return status(21)
        }
    }
    
    /// Invite a user to a list
    ///
    /// Path for request: GET to /user/list/LISTID_Int/invite/USERNAME_String
    ///
    /// - Parameters:
    ///   - request: A HTTP request
    ///   - drop: The Droplet. Needed for sending the email and accessing the cache
    /// - Returns: A status (see docs)
    func inviteUser(_ request: Request, drop: Droplet) throws -> ResponseRepresentable {
        guard let user = request.auth.authenticated(User.self) else {
            return status(40)
        }
        do {
            guard let list = try user.lists.find(request.parameters.next() as Int) else {
                return status(23)
            }
            do {
                let username = try request.parameters.next() as String
                guard let invitedUser = try User.all().first(where: { $0.username.equals(caseInsensitive: username) }) else {
                    return status(22)
                }
                let uuid = UUID().uuidString
                let spamuuid = UUID().uuidString
                do {
                    var node = JSON()
                    try node.set("user_id", invitedUser.id?.int!)
                    try node.set("list_id", list.id?.int!)
                    try drop.cache.set(uuid, node, expiration: Date(timeIntervalSinceNow: 86400*2)) // Expires in two days
                    try drop.cache.set(spamuuid, user.id?.int!, expiration: Date(timeIntervalSinceNow: 86400*7)) // Expires in seven days
                    if sendInvitedToListEMail(email: invitedUser.email, targetUsername: username, sourceUsername: user.username, listName: list.name, link: "https://die-liste.herokuapp.com/user/lists/acceptinvitation/" + uuid, spamLink: "https://die-liste.herokuapp.com/spam/" + spamuuid, drop: drop) {
                        return status(10)
                    } else {
                        return status(32)
                    }
                } catch {
                    return status(31)
                }
            } catch {
                return status(21)
            }
        } catch {
            return status(21)
        }
    }
    
    /// Accept an invitation to a list
    ///
    /// - Parameters:
    ///   - request: A HTTP request
    ///   - drop: The droplet in order to access the cache
    /// - Returns: A webview with a message
    func acceptInvitation(_ request: Request, drop: Droplet) throws -> ResponseRepresentable {
        do {
            let uuid = try request.parameters.next() as String
            guard let cacheNode = try drop.cache.get(uuid) else {
                return try self.view.make("error", ["message": "Token abgelaufen. Bitte lasse dir einen neuen zusenden."], for: request)
            }
            let userId = try cacheNode.get("user_id") as Int
            let listId = try cacheNode.get("list_id") as Int
            guard let user = try User.find(userId) else {
                return try self.view.make("error", ["message": "User nicht gefunden"], for: request)
            }
            guard let list = try List.find(listId) else {
                return try self.view.make("error", ["message": "Liste nicht gefunden"], for: request)
            }
            try user.lists.add(list)
            try user.save()
            return try self.view.make("success", ["message": "Du bist erfolgreich der Liste \(list.name) beigetreten."], for: request)
        } catch {
            return try self.view.make("error", ["message": "Parameter nicht gefunden"], for: request)
        }
    }
    
    /// Remove a user from a list
    ///
    /// Route for request: GET to /user/lists
    ///
    /// JSON encoding for request
    ///
    ///     {
    ///         "list_id": $LISTID,
    ///         "username": $USERNAME_String
    ///     }
    ///
    /// - Parameter request: A HTTP request
    /// - Returns: A status (see docs)
    func removeUser(_ request: Request) -> ResponseRepresentable {
        guard let user = request.auth.authenticated(User.self) else {
            return status(40)
        }
        guard let json = request.json else {
            return status(20)
        }
        do {
            let listId = try json.get("list_id") as Int
            let username = try json.get("username") as String
            
            do {
                guard let list = try user.lists.find(listId) else {
                    return status(23)
                }
                guard let userToRemove = try list.connectedUsers.all().first(where: { $0.username.equals(caseInsensitive: username) }) else {
                    return status(22)
                }
                do {
                    try userToRemove.lists.remove(list)
                    return status(10)
                } catch {
                    return status(31)
                }
                
            } catch {
                return status(23)
            }
        } catch {
            return status(25)
        }
    }
}
